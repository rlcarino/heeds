!======================================================================
!
!    HEEDS (Higher Education Enrollment Decision Support) - A program
!      to create enrollment scenarios for 'next term' in a university
!    Copyright (C) 2012-2014 Ricolindo L. Carino
!
!    This file is part of the HEEDS program.
!
!    HEEDS is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    HEEDS is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program (see the file COPYING.TXT). If not, see
!    <http://www.gnu.org/licenses/>.
!
!    Send inquiries about HEEDS to:
!      Ricolindo L Carino
!      E-mail: Ricolindo.Carino@AcademicForecasts.com
!      Address: 600 Cruise St., Starkville, MS 39759, U.S.A.
!
!======================================================================


program SERVER

    use XMLIO
    use INITIALIZE

    use EditUNIVERSITY
    use EditTEACHERS
    use EditSECTIONS
    use EditBLOCKS
    use EditCHECKLIST
    use EditENLISTMENT

    implicit none

    ! interface to FastCGI routines in C
    interface

        ! The function to accept a request from the webserver
        function FCGI_Accept () bind(C, NAME='FCGI_Accept')
            use ISO_C_BINDING
            implicit none
            integer(C_INT) :: FCGI_Accept
        end function FCGI_Accept

        ! The function to retrieve POSTed data
        function FCGI_getchar () bind(C, NAME='FCGI_getchar')
            use ISO_C_BINDING
            implicit none
            character(C_CHAR) :: FCGI_getchar
        end function FCGI_getchar

        ! The function to write back to the webserver
        function FCGI_puts (s) bind(C, NAME='FCGI_puts')
            use ISO_C_BINDING
            implicit none
            integer(C_INT) :: FCGI_puts
            character(C_CHAR), dimension(*) :: s
        end function FCGI_puts

    end interface

    ! static initializations
    call initializations()

    ! run as webserver
    call run_webserver()


contains


    subroutine run_webserver()

        integer :: checkSumQuery, iTmp, kStart
        character(len=MAX_LEN_USERNAME) :: tTeacher
        character (len=MAX_LEN_PASSWD_VAR) :: Password
        character(len=MAX_LEN_FILE_PATH) :: fileUSERlog
        integer :: checkSum, jTmp

        call log_comment(trim(fileEXE)//' server started ...')

        ! read basic data for university
        call xml_read_basic_data(pathToYear)

        sorryMessageOfficial = trim(UniversityCodeNoMirror)//' officials and their friends '// &
            ' have a "read-only" permission at this time. '// &
            ' Please see the '//trim(titleTheRegistrar)//' if you wish to change some data.'

        sorryMessageStudent = 'Students are not allowed to make changes here. '// &
            ' Please see your Adviser, or your Dean, or the '//trim(titleTheRegistrar)// &
            ' if you wish to change some data.'

        do iTmp=1,NumTeachers
            if (Teacher(iTmp)%Role/=SYSAD) cycle
            call get_teacher_password(iTmp, Password)
            call log_comment('Password for '//trim(Teacher(iTmp)%TeacherId)//' : '//Password)
        end do

        ! log directory for users in the college
        do jTmp=1,NumColleges
            call make_directory( trim(dirLOG)//trim(College(jTmp)%Code)//DIRSEP )
        end do

        ! create student directories
        call make_student_directories()

        ! read additional data if ACTION was specified
        call switch_to_period(trim(ACTION))

        ! CGI scriptname
#if defined GLNX
        CGI_PATH = PROTOCOL//trim(IP_ADDR)//FSLASH//UniversityCode
#else
        CGI_PATH = FSLASH//UniversityCode
#endif
        USERNAME = PROGNAME

        ! loop until killed/fnSTOP
        do while (FCGI_Accept() >= 0)

            ! timestamp of request
            CALL SYSTEM_CLOCK(tick, count_rate, count_max)
            call date_and_time (date=currentDate, time=currentTime)

            ! last refresh time for mirror
            secsLastRefresh = (tick-tickLastRefresh)/count_rate

            ! next refresh time for mirror
            secsNextRefresh = max(maxRefreshTime - secsLastRefresh, 0)

            ! date changed ?
            if (currentDate/=previousDate) then
                backspace (unitHTML)
                call html_comment('File closed due to date change')
                call flush(unitHTML)
                close(unitHTML)

                call initialize_date_change()
                ! remember new date
                previousDate = currentdate
            end if

            ! tell the webserver to expect text/html
            iTmp = FCGI_puts ('Content-type: text/html'//CRLF//NUL)

            ! rewind the response file
            rewind (unitHTML)

            ! Retrieve DOCUMENT_URI and QUERY_STRING/CONTENT
            call FCGI_getquery(unitHTML)

            ! decrypt query
            call cgi_get_named_integer(QUERY_STRING, 's', checkSumQuery, kStart)
            call cgi_get_named_string(QUERY_STRING, 'q', cipher, iTmp)
            if (kStart==0 .and. iTmp==0) then
                iTmp = len_trim(cipher)
                kStart = atoi( cipher(iTmp-3:iTmp) )
                if (kStart>MAX_LEN_QUERY_STRING/2 .and. kStart<MAX_LEN_QUERY_STRING) then
                    cipher(iTmp-3:iTmp) = SPACE
                    call decrypt(queryEncryptionKey(:kStart), cipher)
                    checkSum = calculate_check_sum(cipher)
                    if (checkSumQuery==checkSum) then
                        QUERY_STRING = trim(cipher)//'&'//QUERY_STRING
                    else
                        QUERY_STRING = '(invalid - incorrect checksum)'
                    end if
                else
                    QUERY_STRING = '(invalid - wrong start)'
                end if
            else ! not encrypted
                QUERY_STRING = '(invalid - not encrypted)'
            end if

            ! make copy of QUERY_STRING
            cipher = QUERY_STRING
            call html_comment('revised CONTENT='//trim(QUERY_STRING))

            ! Establish USERNAME/ROLE and REQUEST
            loginCheckMessage = SPACE
            call get_user_request()

            ! write request to user's log file
            call blank_to_underscore(USERNAME, tTeacher)
            if (requestingStudent>0) then
                Student(requestingStudent)%OnlineStatus = tick
                iTmp = year_prefix(Student(requestingStudent))
                fileUSERlog = trim(dirLOG)// &
                    trim(Student(requestingStudent)%StdNo(1:iTmp))//DIRSEP// &
                    trim(Student(requestingStudent)%StdNo)//'.log'
            else if (requestingTeacher>0) then
                Teacher(requestingTeacher)%OnlineStatus = tick
                iTmp = Department(Teacher(requestingTeacher)%DeptIdx)%CollegeIdx
                fileUSERlog = trim(dirLOG)// &
                    trim(College(iTmp)%Code)//DIRSEP// &
                    trim(tTeacher)//DASH//currentDate//'.log'
            else
                fileUSERlog = trim(dirLOG)//'UNKNOWNS.log'
            end if
            !call html_comment('fileUSERlog='//fileUSERlog, 'fileREQ='//fileREQ)

            if (REQUEST>fnLogout) then ! no passwords
                call log_request(unitUSER, trim(fileUSERlog), &
                    SPACE, &
                    trim(REMOTE_ADDR)//' : '//currentDate//DASH//currentTime//' : '// &
                    trim(fnDescription(REQUEST) ), &
                    trim(cipher) )

                call log_request(unitREQ, trim(fileREQ), &
                    '# '// &
                    trim(USERNAME)//' : '//REMOTE_ADDR//' : '//currentDate//DASH//currentTime//' : '// &
                    trim(fnDescription(REQUEST) ), &
                    trim(cipher) )

            else
                call log_request(unitUSER, trim(fileUSERlog), &
                    SPACE, &
                    trim(REMOTE_ADDR)//' : '//currentDate//DASH//currentTime//' : '// &
                    trim(fnDescription(REQUEST) ) )

                call log_request(unitREQ, trim(fileREQ), &
                    '# '// &
                    trim(USERNAME)//' : '//REMOTE_ADDR//' : '//currentDate//DASH//currentTime//' : '// &
                    trim(fnDescription(REQUEST) ) )
            end if

            ! compose response
            call server_respond(unitHTML)

            ! send response to server
            call FCGI_putfile(unitHTML)

            if (REQUEST==fnStop) exit

            ! automatically logout users idle who have been idle for 30 minutes (1800 seconds)
            do iTmp=1,NumStudents+NumAdditionalStudents
                if (Student(iTmp)%OnlineStatus==0) cycle
                if ( (tick-Student(iTmp)%OnlineStatus)/count_rate>=maxIdleTime) Student(iTmp)%OnlineStatus = 0
            end do
            do iTmp=1,NumTeachers+NumAdditionalTeachers
                if (Teacher(iTmp)%OnlineStatus==0) cycle
                if ( (tick-Teacher(iTmp)%OnlineStatus)/count_rate>=maxIdleTime ) Teacher(iTmp)%OnlineStatus = 0
            end do

            ! auto-backup?
            if (.not. isReadOnly .and. (tick-tickLastBackup)/count_rate>=maxBackupTime) then
                call xml_backup(trim(pathToYear)//'BACKUP.XML'//DASH//currentDate//DASH//currentTime(:6))
            end if

            ! reload data?
            if (isReadOnly .and. secsNextRefresh==0 ) then

                ! re-load  basic data for university
                call initialize_basic_data ()
                call xml_read_basic_data(pathToYear)
                ! tickLastRefresh is set in xml_read_basic_data()

                ! read additional data if ACTION was specified
                call switch_to_period(trim(ACTION))

            end if

        end do ! while (FCGI_Accept() >= 0)

        ! backspace the response file for additional "call html_comment()" before stop
        backspace (unitHTML)

#if defined no_password_check
#else
        ! write data
        if (isRectify) then ! enlistment only
            call qualify_term(currentTerm, iTmp, jTmp)
            pathToTerm = trim(dirDATA)//trim(itoa(iTmp))//DIRSEP//trim(txtSemester(jTmp))//DIRSEP
            call xml_write_pre_enlistment(pathToTerm, 'ENLISTMENT', Enlistment(jTmp,0:), Section(jTmp,0:) )
        else ! everything
            call xml_write_data()
            if (NumWaiverRecords>0) then
                call qualify_term(nextTerm, iTmp, jTmp)
                pathToTerm = trim(dirDATA)//trim(itoa(iTmp))//DIRSEP//trim(txtSemester(jTmp))//DIRSEP
                call xml_write_waivers(pathToTerm, Section(jTmp,0:), jTmp)
            end if
        end if
#endif

        ! terminate
        call html_comment('Stopping '//fileEXE)
        call flush(unitHTML)
        close(unitHTML)

        call terminate(trim(fileEXE)//SPACE//ACTION//' stopped normally.')

    end subroutine run_webserver


    subroutine FCGI_getquery( unitNo )
        ! Retrieve FastCGI environment variables DOCUMENT_URI and QUERY_STRING
        ! Invoked after FCGI_Accept() has completed
        ! Write debugging information to file unit number 'unitNo', which must already be open
        ! Debugging information should be <!-- HTML remark -->

        integer, intent(in)               :: unitNo

        integer                           :: i, j
        integer                           :: iLen, nRet, jLen, kLen
        character(len=7)                  :: cLen

        character(len=MAX_LEN_FILE_PATH)  :: tName
        logical :: boundaryReached, isText

        ! write to the beginning of file unitNo
        rewind (unitNo)

        call html_comment('FCGI_getquery()')

        !call get_environment_variable('HTTP_USER_AGENT', value=httpVariable, length=iLen, status=i)
        !call html_comment('HTTP_USER_AGENT='//trim(httpVariable)//SPACE//itoa(iLen)//itoa(i))

        !call get_environment_variable('REMOTE_PORT', value=httpVariable, length=iLen, status=i)
        !call html_comment('REMOTE_PORT='//trim(httpVariable)//SPACE//itoa(iLen)//itoa(i))

        !call get_environment_variable('REMOTE_IDENT', value=httpVariable, length=iLen, status=i)
        !call html_comment('REMOTE_IDENT='//trim(httpVariable)//SPACE//itoa(iLen)//itoa(i))

        !call get_environment_variable('REMOTE_HOST', value=httpVariable, length=iLen, status=i)
        !call html_comment('REMOTE_HOST='//trim(httpVariable)//SPACE//itoa(iLen)//itoa(i))

        !call get_environment_variable('REMOTE_USER', value=httpVariable, length=iLen, status=i)
        !call html_comment('REMOTE_USER='//trim(httpVariable)//SPACE//itoa(iLen)//itoa(i))

        ! the remote IP
        call get_environment_variable('REMOTE_ADDR', value=REMOTE_ADDR, length=iLen, status=i)
        call html_comment('REMOTE_ADDR='//REMOTE_ADDR//SPACE//itoa(iLen)//itoa(i))

        ! the requested script ('/' if none)
        call get_environment_variable('DOCUMENT_URI', value=DOCUMENT_URI)
        iLen = len_trim(DOCUMENT_URI)
        if ( iLen == 0 ) then
            ! default is /
            DOCUMENT_URI = '/'
        endif

        call html_comment('DOCUMENT_URI='//DOCUMENT_URI(:iLen))

        ! for other environment variables, see <nginx directory>/conf/fastcgi_params
        !call html_comment('KEY='//queryEncryptionKey)

        ! QUERY_STRING (request method was GET) ?
        call get_environment_variable( "QUERY_STRING", value=QUERY_STRING, length=iLen )
        if ( iLen > 0 ) then
            if (iLen>MAX_LEN_QUERY_STRING) QUERY_STRING = '(QUERY_STRING too long.)'
            return
        end if

        ! get CONTENT_LENGTH
        call get_environment_variable( "CONTENT_LENGTH", value=cLen, length=iLen )
        if ( iLen==0 ) then ! nothing sent
            QUERY_STRING = '(QUERY_STRING is empty.)'
            return
        end if

        ! assume request was POST
        call html_comment('CONTENT_LENGTH='//trim(cLen))
        read( cLen, * ) iLen
        QUERY_STRING = SPACE

        ! get characters up to first CR (or last character) into work area
        wrkCipher = SPACE ! initialize work area
        nRet = 0 ! characters retrieved
        call FCGI_getline (nRet, iLen, wrkCipher, jLen, MAX_LEN_QUERY_STRING)

        ! multipart/form-data ?
        boundaryReached = index(wrkCipher(:jLen), '------WebKitFormBoundary') == 1
        if (.not. boundaryReached) then ! not multipart
            QUERY_STRING = wrkCipher(:jLen)
            return
        end if

        ! build QUERY_STRING and upload file from multipart/form-data
        do while (nRet<iLen)

            ! get characters up to next CR
            call FCGI_getline (nRet, iLen, wrkCipher, jLen, MAX_LEN_QUERY_STRING)
            boundaryReached = index(wrkCipher(:jLen), '------WebKitFormBoundary') == 1
            if (boundaryReached) then ! boundary reached
                !call html_comment('PARTIAL_QUERY_STRING='//trim(QUERY_STRING))
                cycle
            end if

           ! check for filename="..."
            j = index(wrkCipher(:jLen), '; filename="')
            if (j>0) then ! get filename= and name=
                tName = wrkCipher(j+12:jLen-3)
                wrkCipher(j:) = SPACE ! erase
                jLen = j-1
                j = index(wrkCipher(:jLen), '; name="')
                QUERY_STRING = trim(QUERY_STRING)//'&'//wrkCipher(j+8:jLen-1)//'='//tName
                !call html_comment(wrkCipher(j+8:jLen-1)//'='//tName)

                ! get content-type
                call FCGI_getline (nRet, iLen, wrkCipher, jLen, MAX_LEN_QUERY_STRING)
                !QUERY_STRING = trim(QUERY_STRING)//'&content='//wrkCipher(15:jLen)
                !call html_comment('&content='//wrkCipher(15:jLen))
                isText = index(wrkCipher(:jLen), 'text')>0

                ! get empty line
                call FCGI_getline (nRet, iLen, wrkCipher, jLen, MAX_LEN_QUERY_STRING)

                ! get file contents
                open(unit=unitETC, file=trim(dirDATA)//'uploads'//DIRSEP//currentDate//DASH// &
                    currentTime(1:7)//DASH//tName, status='new')
                !call html_comment(trim(dirDATA)//'uploads'//DIRSEP//currentDate//DASH// &
                !    currentTime//DASH//tName)
                boundaryReached = .false.
                do while (.not. boundaryReached)

                    ! fill buffer from POSTed data
                    call FCGI_getline (nRet, iLen, wrkCipher, jLen, MAX_LEN_QUERY_STRING)
                    boundaryReached = wrkCipher(:24)== '------WebKitFormBoundary'
                    if (boundaryReached) exit

                    ! look-ahead for boundary
                    call FCGI_getline (nRet, iLen, cipher, kLen, MAX_LEN_QUERY_STRING)
                    boundaryReached = cipher(:24)== '------WebKitFormBoundary' .or. kLen==0

                    if (.not. boundaryReached) then

                        write(unitETC, AFORMAT, advance='no') wrkCipher(:jLen)
                        !call html_comment('PARTIAL_CONTENT='//wrkCipher(:jLen-2))

                        write(unitETC, AFORMAT, advance='no') cipher(:kLen)
                        !call html_comment('PARTIAL_CONTENT='//cipher(:kLen-2))

                    elseif (jLen>2) then

                        write(unitETC, AFORMAT, advance='no') wrkCipher(:jLen)
                        !call html_comment('PARTIAL_CONTENT='//wrkCipher(:jLen-2))

                    end if

                end do
                close(unitETC)

            else
                ! get name=
                j = index(wrkCipher(:jLen), '; name="')
                tName = wrkCipher(j+8:jLen-3)

                ! get empty line
                call FCGI_getline (nRet, iLen, wrkCipher, jLen, MAX_LEN_QUERY_STRING)

                ! get value
                call FCGI_getline (nRet, iLen, wrkCipher, jLen, MAX_LEN_QUERY_STRING)
                QUERY_STRING = trim(QUERY_STRING)//'&'//trim(tName)//'='//wrkCipher(:jLen-2)

            end if

        end do
        call html_comment('QUERY_STRING='//trim(QUERY_STRING))

    end subroutine FCGI_getquery


    subroutine FCGI_getline (current, maxChars, str, strLen, maxStrLen)

        integer, intent (in out) :: current
        integer, intent (in) :: maxChars, maxStrLen
        character(len=*), intent (out) :: str
        integer, intent (out) :: strLen

        character :: ch

        strLen = 0
        do while (current<maxChars)
            ch = FCGI_getchar()
            current = current + 1
            strLen = strLen+1
            str(strLen:strLen) = ch
            if (ch==LF) exit
            if (strLen==maxStrLen) exit
        end do
        call html_comment('FCGI_getline(): '//trim(itoa(strlen))//' of '// &
            trim(itoa(maxStrLen))//', "'//str(:strLen)//'"')

    end subroutine FCGI_getline



    subroutine FCGI_putfile ( unitNo )
        ! Copy file 'unitNo' line by line to the webserver via FCGI_puts()
        ! File must already exist, expected to contain the response to some query

        integer, intent(in) :: unitNo
        integer :: iStat

        call html_comment('FCGI_putfile()')

        ! flush any pending writes
        flush(unitNo)

        ! copy line by line to webserver
        rewind(unitNo)
        do while (.true.)
            read(unitNo, AFORMAT, iostat=iStat) cipher
            if (iStat < 0) exit ! no more lines
!#if defined no_password_check
!            if (cipher(1:5)=='DBG::') then
!                if (isRoleSysAd) then
!                    cipher = '<pre>'//trim(cipher)//'</pre>'
!                else
!                    cycle
!                end if
!            end if
!#else
            if (cipher(1:5)=='DBG::') cycle
!#endif
            iStat = FCGI_puts (trim(cipher)//NUL) ! FCGI_puts expects NULL terminated strings
        end do

    end subroutine FCGI_putfile


    subroutine server_respond (device)
        integer, intent (in) :: device

        integer :: i, tYear, tTerm, targetTerm

        call html_comment('server_respond()')

        ! Establish TERM if required
        if (REQUEST>=fnScheduleOfClasses) then
            call cgi_get_named_integer(QUERY_STRING, 'A9', targetTerm, i)
            if (i/=0 .or. targetTerm<=0) targetTerm = currentTerm ! default
        else
            targetTerm = currentTerm ! default
        end if

        ! target directory if files are to be modified
        if (targetTerm>0) then
            call qualify_term (targetTerm, tYear, tTerm, termDescription)
            pathToTerm = trim(dirDATA)//trim(itoa(tYear))//DIRSEP//trim(txtSemester(tTerm))//DIRSEP
        else
            termDescription = SPACE
            pathToTerm = pathToYear
        end if

        if (isPeriodFour .and. .not. (isRoleSysAd .or. isRoleOfficial .or. isRoleStaff) ) then
            REQUEST = fnLogout
            loginCheckMessage = &
                beginitalic//'Only'//enditalic//' the Registrar and Staff, or University officials are allowed '// &
                'to use '//PROGNAME//' at this time.'
        end if

        call html_comment('FN='//fnDescription(REQUEST), 'PATH='//pathToTerm, 'DESC='//termDescription)

        ! trap official role attempting to change data
        if (isRoleOfficial) call check_permission()

        select case (REQUEST)

            case (fnStop)
                call html_landing_page(device, '<h3>The program will stop</h3>')

!            case (fnSwitchPeriod)
!                ! write data
!                call xml_write_data()
!                if (NumWaiverRecords>0) then
!                    call qualify_term(nextTerm, tYear, tTerm)
!                    pathToTerm = trim(dirDATA)//trim(itoa(tYear))//DIRSEP//trim(txtSemester(tTerm))//DIRSEP
!                    call xml_write_waivers(pathToTerm, Section(tTerm,0:), tTerm)
!                end if
!
!                call switch_to_period()
!                call html_college_links(device, CollegeIdxUser, mesg='Period in term changed.')
!
!            case (fnSwitchTerm)
!                ! write data
!                call xml_write_data()
!                if (NumWaiverRecords>0) then
!                    call qualify_term(nextTerm, tYear, tTerm)
!                    pathToTerm = trim(dirDATA)//trim(itoa(tYear))//DIRSEP//trim(txtSemester(tTerm))//DIRSEP
!                    call xml_write_waivers(pathToTerm, Section(tTerm,0:), tTerm)
!                end if
!
!                call switch_to_term()
!                call html_college_links(device, CollegeIdxUser, mesg='Academic term advanced; period is Classlists')

            case (fnReload)

                ! write data
                call xml_write_data()
                if (NumWaiverRecords>0) then
                    call qualify_term(nextTerm, tYear, tTerm)
                    pathToTerm = trim(dirDATA)//trim(itoa(tYear))//DIRSEP//trim(txtSemester(tTerm))//DIRSEP
                    call xml_write_waivers(pathToTerm, Section(tTerm,0:), tTerm)
                end if

                ! re-load  basic data for university
                call initialize_basic_data ()
                call xml_read_basic_data(pathToYear)
                ! tickLastRefresh is set in xml_read_basic_data()

                ! read additional data
                call switch_to_period(trim(ACTION))
                call html_college_links(device, CollegeIdxUser, mesg='Reloaded data.')

            case (fnLogin)

                if (isRoleBenefactor) then
                    REQUEST = fnListBeneficiaries
                    call links_to_students(device, REQUEST, currentTerm, NumSections(currentTerm), Section(currentTerm,0:), &
                        Enlistment(currentTerm,0:), USERNAME)
!                elseif (isRoleStudent) then
!                    if (isPeriodOne) call advise_PE(requestingStudent)
!                    call html_college_links(device, CollegeIdxUser, mesg=loginCheckMessage)
                else
                    call html_college_links(device, CollegeIdxUser, mesg=loginCheckMessage)
                end if

            case (fnLoginByNonEditors)
                if (isRoleOfficial) then
                    call html_college_links(device, CollegeIdxUser, &
                        mesg='"Toggle login permission for non-editors roles" failed. '//sorryMessageOfficial)
                else
                    isAllowedNonEditors = .not. isAllowedNonEditors
                    call html_college_links(device, CollegeIdxUser, &
                        mesg='Toggled login permission for non-editor roles.')
                end if

            case (fnChangeTeacherPassword)
                call change_current_teacher_password(device)

            case (fnChangeStudentPassword)
                call change_current_student_password(device)

            case (fnResetPasswords)

                if (isRoleOfficial) then

                    call html_college_links(device, CollegeIdxUser, mesg='"Reset ALL passwords" failed. '//sorryMessageOfficial)

                else
                    call xml_backup(trim(pathToYear)//'BACKUP.XML'//DASH//currentDate//DASH//currentTime(:6))

                    ! the students
                    do i=1,NumStudents+NumAdditionalStudents
                        call set_password(Student(i)%Password)
                    end do
                    ! the teachers
                    do i=1,NumTeachers+NumAdditionalTeachers
                        if (Teacher(i)%Role/=SYSAD) then
                            call set_password(Teacher(i)%Password)
                        end if
                    end do
                    isDirtySTUDENTS = .true.
                    call html_college_links(device, CollegeIdxUser, mesg='Passwords were reset for non-ADMIN roles')
                end if

            case (fnLogout)
                if (requestingTeacher>0) then
                    Teacher(requestingTeacher)%OnlineStatus = 0
                else if (requestingStudent>0) then
                    Student(requestingStudent)%OnlineStatus = 0
                end if
                call html_landing_page(device, SPACE)

            case (fnFileDownload)
                call download_file(device, targetTerm)

            case (fnEditSignatories)
                call edit_signatories(device)

            case (fnTimers)
                call edit_timers(device)

            case (fnRecentStudentActivity)
                call recent_student_activity(device)

            case (fnRecentTeacherActivity)
                call recent_teacher_activity(device)

            case (fnTranscript)
                call student_transcript(device)

            case (fnGradeCertification)
                call grade_certification(device)

            case (fnEditMOTD)
                call message_of_the_day(device)

            case (fnEditEMERGENCY)
                call emergency_message(device)

            case (fnEditFees)
                call school_fees(device)

            case (fnEditScholarships)
                call scholarships_list(device)

            ! college info
            case (fnCollegeLinks)
                call html_college_links(device)

            ! subject info
            case (fnSubjectList)
                call subject_list_all (device)

            ! curriculum info
            case (fnCurriculumList, fnActivateCurriculum, fnDeactivateCurriculum)
                call curriculum_list_all(device, REQUEST)

            case (fnCurriculum)
                call curriculum_display(device)

            ! room info
            case (fnRoomList)
                call room_list_all (device)

            ! teacher info
            case (fnTeachersByDept, fnTeachersByName, fnOnlineTeachers, fnFindTeacher)
                call teacher_list_all (device, REQUEST)

            ! subjects with open sections
            case (fnScheduleOfClasses)
                call section_list_all (device)

            ! schedule of classes
            case (fnScheduleByArea, fnTBARooms, fnTBATeachers, fnTeacherClasses, fnUnpostedGrades, fnGradesheetsToReceive)
                call section_list_classes (device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:), Enlistment(targetTerm,0:), REQUEST)

            ! schedule conflicts
            case (fnRoomConflicts)
                call room_conflicts (device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:))

            case (fnRoomSchedule)
                call room_schedule(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:) )

            case (fnTeacherConflicts)
                call teacher_conflicts (device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:))

            case (fnPrintableWorkload)
                call teacher_schedule_printable(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:) )

            ! blocks
            case (fnBlockList)
                call block_list_all(device, targetTerm, NumBlocks(targetTerm), Block(targetTerm,0:), &
                    NumSections(targetTerm), Section(targetTerm,0:))

            case (fnBlockSchedule)
                call block_schedule(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:))

            case (fnBlockConflicts)
                call block_conflicts(device, targetTerm, NumBlocks(targetTerm), Block(targetTerm,0:), &
                    NumSections(targetTerm), Section(targetTerm,0:))

            case (fnEditSubject)
                call subject_edit (device)

            case (fnEditCurriculum)
                call curriculum_edit (device)

            case (fnEditEquivalencies)
                call equivalencies_edit (device)

            case (fnEditRoom)
                call room_edit (device)

            case (fnEditTeacher, fnGenerateTeacherPassword)
                call teacher_edit (device)

            case (fnDeleteTeacher)
                call teacher_delete (device)

            case (fnTeacherEditSchedule)
                call teacher_schedule (device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:) )

            case (fnScheduleOfferSubject)
                call section_offer_subject (device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:), Enlistment(targetTerm,0:) )

            case (fnScheduleDelete)
                call section_delete(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:), Enlistment(targetTerm,0:) )

            case (fnScheduleAddLab)
                call section_add_laboratory(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:), Enlistment(targetTerm,0:) )

            case (fnScheduleEdit, fnScheduleValidate)
                call section_validate_inputs (device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:), Enlistment(targetTerm,0:) )

            case (fnBlockDeleteNotClasses, fnBlockEditName, fnBlockCopy, fnBlockDeleteAlsoClasses, &
                    fnBlockEditSection, fnBlockEditSubject)
                call edit_block(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:), REQUEST)

            case (fnBlockNewSelect)
                call block_select_curriculum_year(device, targetTerm )

            case (fnBlockNewAdd)
                call block_add(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), NumBlocks(targetTerm), Block(targetTerm,0:) )

            ! students
            case (fnStudentEdit, fnGenerateStudentPassword)
                call student_edit_info(device)

            case (fnStudentAdd)
                call student_add(device)

            case (fnStudentsDistribution)
                call student_distribution(device)

            case (fnStudentsByCurriculum, fnOnlineStudents, fnFindStudent, fnListBeneficiaries)
                call links_to_students(device, REQUEST)

            case (fnAdvisersByTeacher )
                call student_advisers_by_teacher(device)

            case (fnAdvisersByCurriculum )
                call student_advisers_by_curriculum(device)

            case (fnStudentGrades)
                if (isPeriodOne) then
                    targetTerm = currentTerm
                    call student_edit_grades(device, targetTerm, .true., Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:) )
                else
                    targetTerm = nextTerm
                    call student_edit_grades(device, targetTerm, .false., Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:) )
                end if

            case (fnEditCheckList)
                if (isPeriodOne) then
                    targetTerm = currentTerm
                    call checklist_edit(device, targetTerm, .true., &
                        Section(targetTerm,0:), Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), Enlistment(targetTerm,0:), &
                        trim(pathToYear)//trim(txtSemester(targetTerm))//DIRSEP)
                elseif (isPeriodTwo .or. isPeriodThree .or. isPeriodFour) then
                    targetTerm = nextTerm
                    call checklist_edit(device, targetTerm, isPeriodThree .or. isPeriodFour, &
                        Section(targetTerm,0:), Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), Enlistment(targetTerm,0:), &
                        trim(pathToNextYear)//trim(txtSemester(targetTerm))//DIRSEP)
                else
                    call html_write_header(device, SPACE, 'Checklists not available')
                    write(device,AFORMAT) &
                        linebreak//'Period in the term has not been specified'//linebreak//linebreak//horizontal
                    REQUEST = 0 ! do not suppress footer
                end if

            case (fnStudentsWithConflicts, fnStudentPriority, fnListUnlocked)
                call links_to_students(device, REQUEST, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    Enlistment(targetTerm,0:))

!                ! cases for next semester's predicted demand for subjects
!                case (fnDemandFreshmen,fnUpdateDemandFreshmen)
!                    call demand_by_new_freshmen(device, Offering(nextTerm,MAX_ALL_DUMMY_SUBJECTS:))

            case (fnDemandForSubjects)
                call demand_for_subjects(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), Enlistment(targetTerm,0:))

            case (fnConfirmResetDemandForSubjects)
                call confirm_reset_demand_for_subjects(device, targetTerm)

            case (fnResetDemandForSubjects)
                call reset_demand_for_subjects(Section(targetTerm,0:), Enlistment(targetTerm,0:), &
                    trim(pathToNextYear)//trim(txtSemester(targetTerm))//DIRSEP, loginCheckMessage)
                call html_college_links(device, targetCollege, loginCheckMessage)

            case (fnConfirmUpdateDemandForSubjects)
                call confirm_needs_analysis(device, targetTerm)

            case (fnUpdateDemandForSubjects)
                if (.not. isPeriodOne) then

                    targetTerm = nextTerm
                    call needs_analysis(targetTerm, isPeriodThree .or. isPeriodFour, &
                        Section(targetTerm,0:), Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), Enlistment(targetTerm,0:), &
                        trim(pathToNextYear)//trim(txtSemester(targetTerm))//DIRSEP, loginCheckMessage)
                    if (len_trim(loginCheckMessage)==0) then
                        call html_write_header(device, SPACE, 'Needs Analysis updated.')
                        write(device,AFORMAT) trim(make_href(fnDemandForSubjects, College(targetCollege)%Code, &
                            A1=College(targetCollege)%Code, A9=nextTerm, pre=linebreak// &
                                'Needs Analysis for ', post=linebreak//linebreak//horizontal) )
                    else
                        call html_college_links(device, targetCollege, loginCheckMessage)
                    end if

                else

                    call html_write_header(device, SPACE, 'Needs Analysis not available')
                    write(device,AFORMAT) &
                        linebreak//'Must be Period 2 - Advising, or Period 3 - Gradesheets to run Needs Analysis'// &
                            linebreak//linebreak//horizontal
                end if
                REQUEST = 0 ! do not suppress footer

            case (fnPotentialStudents)
                call list_potential_students(device, targetTerm, Enlistment(targetTerm,0:))

            ! summary of enlistment
            case (fnEnlistmentSummary, fnBottleneck, fnExtraSlots)
                call enlistment_summarize(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), Enlistment(targetTerm,0:), REQUEST)

            case (fnUnderloadSummary)
                call underload_summarize(device, targetTerm, Enlistment(targetTerm,0:))

            case (fnUnderloadedStudents)
                call underloaded_students(device, Enlistment(targetTerm,0:))

            case (fnStudentsNotEnrolled)
                call enlistment_no_classes(device, Enlistment(targetTerm,0:))

            case (fnNotAccommodated)
                call enlistment_not_accommodated(device, targetTerm, Enlistment(targetTerm,0:))

            case (fnGradesheet)
                call enlistment_grades(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    Enlistment(targetTerm,0:) )

            case (fnPrintableGradesheet)
                call printable_gradesheet(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    Enlistment(targetTerm,0:))

            case (fnClassList)
                call class_list(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), Enlistment(targetTerm,0:))

            case (fnPrintableClassList)
                call printable_class_list(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    Enlistment(targetTerm,0:))

            case (fnChangeMatriculation, fnStudentClasses)
                call enlistment_edit(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:), Enlistment(targetTerm,0:) )

            case (fnToggleSelfEnlistALL)
                call toggle_allow_students_to_enlistALL(device, targetTerm )

            case (fnToggleSelfEnlistPE)
                call toggle_allow_students_to_enlistPE(device, targetTerm )

            case (fnStudentForceEnlist)
                call enlistment_forced(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    Enlistment(targetTerm,0:) )

            case (fnFindBlock)
                call enlistment_find_block(device, targetTerm, Section(targetTerm,0:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:), Enlistment(targetTerm,0:) )

            case (fnPrintableSchedule)
                call enlistment_printable(device, NumSections(targetTerm), Section(targetTerm,0:), Enlistment(targetTerm,0:))

!            case (fnGenerateTimetables)
!                call generate_timetables(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
!                    wrkOffering, Enlistment(targetTerm,0:), tArray )
!                call html_college_info(device, targetCollege)
!
            case (fnConfirmTimetabling)
                call confirm_preenlist_or_delist(device, targetTerm)

            case (fnTimetabling)

                ! auto change enlistment
                call preenlist_or_delist(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    !Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:), Enlistment(targetTerm,0:), tArray, loginCheckMessage)
                if (len_trim(loginCheckMessage)==0) then
                else
                    ! headers written by preenlist_or_delist()
                    call html_college_info(device, targetCollege)
                end if

            case (fnSummaryWAG)
                call average_grade_summary_list(device )

            case (fnSwitchUser)
                call switch_user(device )


            case default
                targetCollege = CollegeIdxUser
                targetDepartment = DeptIdxUser
                termDescription = SPACE
                call html_write_header(device, SPACE, 'Function not available')
                write(device,AFORMAT) &
                    linebreak//'The functionality "'//trim(fnDescription(REQUEST))// &
                    '" is not enabled in this version of '//PROGNAME//DOT//linebreak//linebreak//horizontal
                REQUEST = 0 ! do not suppress footer

        end select

        call html_write_footer(device)

    end subroutine server_respond


    subroutine get_user_request()

        character (len=MAX_LEN_PASSWD_VAR) :: tPassword
        integer :: errNo

        call html_comment('get_user_request()')

        isRoleOfficial = .false.
        isRoleSysAd = .false.
        isRoleStaff = .false.
        isRoleDean = .false.
        isRoleChair = .false.
        isRoleFaculty = .false.
        isRoleStudent = .false.
        isRoleGuest = .false.
        isRoleBenefactor = .false.

        DeptIdxUser = 0
        CollegeIdxUser = 0
        CurriculumIdxUser = 0
        requestingTeacher = 0
        requestingStudent = 0

        ! initialize index to target object of REQUEST
        targetSubject = 0
        targetSection = 0
        targetDepartment = 0
        targetCurriculum = 0
        targetCollege = 0
        targetRoom = 0
        targetTeacher = 0
        targetBlock = 0
        targetStudent = 0

        ! Establish REQUESTed function if any, else return the landing page
        call cgi_get_named_integer(QUERY_STRING, 'F', REQUEST, errNo)
        if (errNo==-1) then
            call html_comment('REQUESTed fn = '//itoa(REQUEST)//', errNo = '//itoa(errNo))
            REQUEST = fnLogout
            return
        end if

        ! Get USERNAME
        if (REQUEST/=fnLogin) then
            call cgi_get_named_string(QUERY_STRING, 'N', USERNAME, errNo)
        else
            call cgi_get_named_string(QUERY_STRING, 'U', USERNAME, errNo)
        end if
        if (errNo==0) then
            requestingTeacher = index_to_teacher(USERNAME)
            requestingStudent = index_to_student(USERNAME)
        end if

        if (requestingTeacher>0) then ! in Teacher()

            DeptIdxUser = Teacher(requestingTeacher)%DeptIdx
            CollegeIdxUser = Department(DeptIdxUser)%CollegeIdx
            ROLE = Teacher(requestingTeacher)%Role

            select case (ROLE)

                case (GUEST)
                    isRoleGuest = .true.
                    ROLE = SPACE

                case (FACULTY)
                    isRoleFaculty = .true.
                    targetDepartment = DeptIdxUser

                case (CHAIR)
                    isRoleChair = .true.
                    targetDepartment = DeptIdxUser

                case (DEAN)
                    isRoleDean = .true.
                    DeptIdxUser = 0

                case (STAFF)
                    isRoleStaff = .true.
                    !ROLE = SPACE

                case (OFFICIAL)
                    isRoleOfficial = .true.
                    DeptIdxUser = NumDepartments
                    CollegeIdxUser = NumColleges

                case (BENEFACTOR)
                    isRoleBenefactor = .true.
                    DeptIdxUser = NumDepartments
                    CollegeIdxUser = NumColleges
                    ROLE = SPACE

                case (SYSAD)
                    isRoleSysAd = .true.
                    DeptIdxUser = NumDepartments
                    CollegeIdxUser = NumColleges
                    ROLE = SPACE

            end select

        elseif (requestingStudent>0) then ! in Student()

            ROLE = SPACE
            isRoleStudent = .true.
            targetStudent = requestingStudent
            targetCurriculum = Student(requestingStudent)%CurriculumIdx
            CurriculumIdxUser = targetCurriculum
            CollegeIdxUser = Curriculum(targetCurriculum)%CollegeIdx
            DeptIdxUser = 0

        else ! assume Guest
            USERNAME = GUEST
            ROLE = SPACE
            isRoleGuest = .true.
            requestingTeacher = index_to_teacher(USERNAME)
            DeptIdxUser = Teacher(requestingTeacher)%DeptIdx
            CollegeIdxUser = Department(DeptIdxUser)%CollegeIdx
        end if

        ! stop by Registrar Role?
        if (REQUEST==fnStop) return

        ! disallow non-editors?
        if (.not. isAllowedNonEditors .and. &
            (isRoleGuest .or. isRoleOfficial .or. &
             (isRoleStudent .and. .not. College(CollegeIdxUser)%AllowEnlistmentInALL(currentTerm)) ) ) then
            REQUEST = fnLogout
            loginCheckMessage = &
                ' Please go back to '//PROGNAME//' Index and select the ''mirror'' for '// &
                trim(UniversityCodeNoMirror)//DOT
            return
        end if

        ! disallow non-editors during grade rectification
        if (isRectify .and. (isRoleGuest .or. isRoleOfficial .or. isRoleStudent) ) then
            REQUEST = fnLogout
            loginCheckMessage = ' Officials, guests and students are not allowed here at this time.'
            return
        end if

        if (REQUEST/=fnLogin) then ! not logging in
            ! previously logged out?  force user to login
            if (requestingTeacher>0) then ! teacher
                if (Teacher(requestingTeacher)%OnlineStatus==0 .and. .not. isRoleGuest) REQUEST = fnLogout
            else ! student
                if (Student(requestingStudent)%OnlineStatus==0) REQUEST = fnLogout
            end if
            return
        end if
        ! request is login; validate POSTed data

        ! Always allow Guest account
        if (trim(USERNAME)==GUEST) then ! Guest
            loginCheckMessage = &
                ' You are logged in as Guest. Visit the '//trim(UniversityCodeNoMirror)//SPACE// &
                ' Registrar for your own Username and Password.'
            return
        end if

#if defined no_password_check
        loginCheckMessage = 'Successful login for '//USERNAME//' without a password.'
        return
#endif

        ! password provided ?
        call cgi_get_named_string(QUERY_STRING, 'P', tPassword, errNo)
        if (errNo==-1) then ! no password
            REQUEST = fnLogout
            loginCheckMessage = 'Username/Password combination not valid.'
        else ! password provided
            if (requestingTeacher>0) then
                if (is_teacher_password(requestingTeacher,tPassword) ) then ! password matched
                    loginCheckMessage = 'Successful login for '//USERNAME
                else ! return login page
                    REQUEST = fnLogout
                    loginCheckMessage = 'Username/Password combination not valid.'
                end if
            elseif (requestingStudent>0) then
                if (is_student_password(requestingStudent,tPassword) ) then ! password matched
                    loginCheckMessage = 'Successful login for '//USERNAME
                else ! return login page
                    REQUEST = fnLogout
                    loginCheckMessage = 'Username/Password combination not valid.'
                end if
            else ! return login page
                REQUEST = fnLogout
                loginCheckMessage = 'Username/Password combination not valid.'
            end if
        end if

    end subroutine get_user_request


    subroutine download_file(device, Term)
        integer, intent(in) :: device, Term

        character (len=MAX_LEN_FILE_PATH) :: fileName
#if defined GLNX
        character (len=MAX_LEN_XML_LINE) :: line
#endif        
        integer :: errNo, i, j
        character (len=MAX_LEN_PASSWD_VAR) :: XMLfile, Password

        call cgi_get_named_string(QUERY_STRING, 'A1', XMLfile, errNo)

        call html_comment('download_file('//trim(XMLfile)//')')

        select case(trim(XMLfile))

            case ('PASSWORDS-TEACHERS.CSV')

                write(device,AFORMAT) &
                    '#', &
                    '#  !!!!!!!!! FOR THE REGISTRAR''S EYES ONLY !!!!!!!!! ', &
                    '#  !!!!!!!!!     PASSWORDS-TEACHERS.CSV     !!!!!!!!! ', &
                    '#', &
                    '# Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                                FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
                    '# NAME - name of teacher', &
                    '# UNIT - academic unit', &
                    '# USERNAME - HEEDS login name', &
                    '# PASSWORD - HEEDS password', &
                    '# ROLE - HEEDS role', &
                    '# COMMENT - Date received/Initials', &
                    '#', &
                    '"NAME","UNIT","USERNAME","PASSWORD","ROLE","COMMENT"'

                do i=1,NumTeachers+NumAdditionalTeachers
                    call get_teacher_password(i, Password)
                    if (Teacher(i)%Role/=SYSAD ) then
                        write(device,AFORMAT) &
                            '"'//Teacher(i)%Name//'","'// &
                                 Department(Teacher(i)%DeptIdx)%Code//'","'// &
                                 Teacher(i)%TeacherId//'","'// &
                                 Password(:MAX_LEN_PASSWORD)//'","'// &
                                 Teacher(i)%Role//'"'
                    end if
                end do

            case ('PASSWORDS-STUDENTS.CSV')

                write(device,AFORMAT) &
                    '#', &
                    '#  !!!!!!!!! FOR THE REGISTRAR''S EYES ONLY !!!!!!!!! ', &
                    '#  !!!!!!!!!     PASSWORDS-STUDENTS.CSV     !!!!!!!!! ', &
                    '#', &
                    '# Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                                FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
                    '# NAME - name of student', &
                    '# CURRICULUM - curriculum', &
                    '# USERNAME - HEEDS login name', &
                    '# PASSWORD - HEEDS password', &
                    '# COMMENT - Date received/Initials', &
                    '#', &
                    '"NAME","CURRICULUM","USERNAME","PASSWORD","COMMENT"'

                do j=1,NumStudents+NumAdditionalStudents
                    i = StdRank(j)
                    call get_student_password(i, Password)
                    write(device,AFORMAT) &
                        '"'//Student(i)%Name//'","'// &
                             Curriculum(Student(i)%CurriculumIdx)%Code//'","'// &
                             Student(i)%StdNo//'","'// &
                             Password(:MAX_LEN_PASSWORD)//'"'
                end do


            case ('BACKUP.XML')

                fileName = trim(XMLfile)//DASH//trim(UniversityCodeNoMirror)//DASH//currentDate//DASH//currentTime(:6)
                call xml_backup(trim(WEBROOT)//fileName)

                call html_write_header(device, trim(fileName)// ' created', SPACE)
#if defined GLNX
                fileName = trim(fileName)//'.gz'
#endif

                write(device,AFORMAT)'Right-click on <a href="/'//trim(fileName)// &
                    '">this</a> link, then "Save Link As..."'//horizontal
                REQUEST = fnCollegeLinks


            case ('CLASSES-BLOCKS.CSV')

                call write_classes_blocks_csv (device, Term, NumSections(Term), Section(Term,0:), &
                    NumBlocks(Term), Block(Term,0:) )


            case ('ENLISTMENT.CSV')
                call write_enlistment_csv (device, Term, Enlistment(Term,0:), Section(Term,0:) )


            case ('STUDENT-INFO.CSV')

                fileName = 'STUDENT-INFO-'//trim(UniversityCodeNoMirror)//'.CSV'
                open(unit=unitETC, file=trim(WEBROOT)//fileName)
                call write_student_info_csv (unitETC)
                close(unitETC)

                call html_write_header(device, trim(fileName)// ' created', SPACE)
#if defined GLNX
                line = 'gzip '//trim(WEBROOT)//fileName
                call system(trim(line), errNo)
                call log_comment(itoa(errNo)//' returned by: '//line)
                if (errNo==0) fileName = trim(fileName)//'.gz'
!#else
!                line = '"C:\Program Files\7-Zip\7z.exe" a -t7z '//trim(trim(WEBROOT)//fileName)//'.7z '// &
!                    trim(WEBROOT)//fileName
!                call system(trim(line), errNo)
!                call log_comment(itoa(errNo)//' returned by: '//line)
!                if (errNo==0) fileName = trim(fileName)//'.7z'
#endif
                write(device,AFORMAT)'Right-click on <a href="/'//trim(fileName)// &
                    '">this</a> link, then "Save Link As..."'//horizontal
                REQUEST = fnCollegeLinks

            case default

                targetCollege = CollegeIdxUser
                targetDepartment = DeptIdxUser
                termDescription = SPACE
                call html_write_header(device, SPACE, horizontal//'File not recognized "'//trim(XMLfile))
                REQUEST = 0


        end select

    end subroutine download_file


    subroutine switch_to_period(arg)
        character(len=*), intent (in), optional :: arg
        character(len=MAX_LEN_CLASS_ID) :: tAction
        integer :: iTmp, jTmp, kTmp, errNo

        isPeriodOne = .false.
        isPeriodTwo = .false.
        isPeriodThree = .false.
        isPeriodFour = .false.

        if (present(arg)) then
            tAction = arg
        else
            call cgi_get_named_string(QUERY_STRING, 'A1', tAction, iTmp)
        end if

        termBegin = cTm1
        termEnd = termBegin+2
        select case (trim(tAction))

            case ('Classlists')
                isPeriodOne = .true.
                isProbabilistic = .false.

            case ('Advising')
                isPeriodTwo = .true.
                isProbabilistic = .true.

            case ('Gradesheets')
                isPeriodThree = .true.
                isProbabilistic = .false.
                if (isRectify) then
                    termBegin = currentTerm
                    termEnd = termBegin
                end if

            case ('Rectify')
                isPeriodThree = .true.
                isProbabilistic = .false.
                isRectify = .true.
                termBegin = currentTerm
                termEnd = termBegin

            case ('Preregistration')
                isPeriodFour = .true.
                isProbabilistic = .false.

            case default

        end select

        call log_comment('switch_to_period('//tAction//')')

        ! read schedules, blocks, enlistment
        do kTmp=termBegin,termEnd

            call qualify_term(kTmp, iTmp, jTmp)
            pathToTerm = trim(dirDATA)//trim(itoa(iTmp))//DIRSEP//trim(txtSemester(jTmp))//DIRSEP

            ! read the classes
            call read_classes(pathToTerm, jTmp, NumSections(jTmp), Section(jTmp,0:), &
                Offering(jTmp,MAX_ALL_DUMMY_SUBJECTS:), errNo)

            ! read the blocks
            call read_blocks(pathToTerm, jTmp, NumBlocks(jTmp), Block(jTmp,0:), NumSections(jTmp), Section(jTmp,0:), errNo)

            ! count no. of sections by dept
            call count_sections_by_dept(jTmp, NumSections(jTmp), Section(jTmp,0:))

            ! read enlistment files, if any
            call read_enlistment(pathToTerm, jTmp, 'ENLISTMENT', 0, 6, &
                NumSections(jTmp), Section(jTmp,0:), Enlistment(jTmp,0:), NumEnlistment(jTmp), errNo)

            call recalculate_available_seats(Section(jTmp,0:), Enlistment(jTmp,0:))

        end do

        if (.not. isPeriodOne .and. .not. isRectify) then

            call qualify_term(nextTerm, iTmp, jTmp)
            pathToTerm = trim(dirDATA)//trim(itoa(iTmp))//DIRSEP//trim(txtSemester(jTmp))//DIRSEP

            ! read waivers for next term
            call xml_read_waivers(pathToTerm, nextTerm, NumSections(jTmp), Section(jTmp,0:), &
                NumWaiverRecords, errNo)

            ! freshmen quota for next term
            call xml_read_intake(pathToTerm, errNo) ! try the XML file
            if (errNo/=0) then ! something wrong with XML file
                NFintake(1:NumCurricula-1) = 1
            end if
#if defined REGIST
            call log_comment('# incoming freshmen next term ='//trim(itoa(sum(NFintake)))//'?')
#endif
        end if

        ! initialize QUERY_STRING encryption key to invalidate old links
        do iTmp=1,MAX_LEN_QUERY_STRING
            call random_number(harvest)
            jTmp = 1 + int(255*harvest)
            queryEncryptionKey(iTmp:iTmp) = achar(jTmp)
        end do
        !call html_comment('KEY='//queryEncryptionKey)

    end subroutine switch_to_period


!    subroutine switch_to_term()
!        integer :: iTmp, jTmp, kTmp, prevCurrentTerm, prevCurrentYear
!        character(len=5) :: nextYearDir
!
!        prevCurrentYear = currentYear
!        prevCurrentTerm = currentTerm
!        call cgi_get_named_integer(QUERY_STRING, 'A1', currentTerm, iTmp)
!
!        ! proceed to next academic year?
!        if (prevCurrentTerm==3 .and. currentTerm==1) then
!            currentYear = currentYear+1
!            ! create directories for year after
!            nextYearDir = trim(itoa(currentYear+1))//DIRSEP
!            call make_directory( trim(dirDATA)//nextYearDir)
!            call make_directory( trim(dirBACKUP)//nextYearDir)
!            do iTmp=1,3
!                call make_directory( trim(dirDATA)//nextYearDir//trim(txtSemester(iTmp))//DIRSEP )
!                call make_directory( trim(dirBACKUP)//nextYearDir//trim(txtSemester(iTmp))//DIRSEP )
!            end do
!
!        end if
!
!        call log_comment('switch_to_term('//text_term_school_year(currentTerm,currentYear)//')')
!        call html_comment('switch_to_term('//text_term_school_year(currentTerm,currentYear)//')')
!
!        ! compute relative years and terms (before and after current)
!        call initialize_past_future_years_terms ()
!
!        ! re-initialize term-specific data
!        call initialize_term_data()
!
!        ! re-initialize Student() if school year changed
!        if (currentYear/=prevCurrentYear) then
!
!            call initialize_student(Student(0))
!            Student = Student(0)
!            NumStudents = 0
!            NumAdditionalStudents = 0
!            do iTmp=1,MAX_ALL_STUDENTS
!                StdRank(iTmp) = iTmp
!            end do
!
!            ! retrieve older list of students, update with newer list
!            iTmp = min(currentYear, prevCurrentYear)
!            jTmp = max(currentYear, prevCurrentYear)
!
!            call read_students(trim(dirDATA)//trim(itoa(iTmp))//DIRSEP, kTmp)
!            call read_students(trim(dirDATA)//trim(itoa(jTmp))//DIRSEP, kTmp)
!
!            ! create student directories
!            call make_student_directories()
!
!        end if
!
!        ! reset period
!        call switch_to_period('Classlists')
!
!    end subroutine switch_to_term


    subroutine toggle_allow_students_to_enlistPE(device, thisTerm)

        integer, intent(in) :: device, thisTerm

        integer :: cdx
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege

        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, cdx)
        targetCollege = index_to_college(tCollege)
        call html_comment('toggle_allow_students_to_enlistPE('//tCollege//itoa(thisTerm)//')')

        if (isRoleOfficial) then
            call html_write_header(device, College(targetCollege)%Code//'- '//College(targetCollege)%Name, &
                sorryMessageOfficial)
        else
            College(targetCollege)%AllowEnlistmentInPE(thisTerm) = &
                .not. College(targetCollege)%AllowEnlistmentInPE(thisTerm)
            call html_write_header(device, College(targetCollege)%Code//'- '//College(targetCollege)%Name, &
                'Toggled allow/disallow self-enlistment by students in PE (only).')

            if (College(targetCollege)%AllowEnlistmentInPE(thisTerm)) then
                College(targetCollege)%AllowEnlistmentInALL(thisTerm) = .false.
            end if
        end if
        call html_college_info(device, targetCollege)

    end subroutine toggle_allow_students_to_enlistPE


    subroutine toggle_allow_students_to_enlistALL(device, thisTerm)

        integer, intent(in) :: device, thisTerm

        integer :: cdx
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege

        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, cdx)
        targetCollege = index_to_college(tCollege)
        call html_comment('toggle_allow_students_to_enlistALL('//tCollege//itoa(thisTerm)//')')

        if (isRoleOfficial) then
            call html_write_header(device, College(targetCollege)%Code//'- '//College(targetCollege)%Name, &
                sorryMessageOfficial)
        else
            College(targetCollege)%AllowEnlistmentInALL(thisTerm) = &
                .not. College(targetCollege)%AllowEnlistmentInALL(thisTerm)
            call html_write_header(device, College(targetCollege)%Code//'- '//College(targetCollege)%Name, &
                'Toggled allow/disallow self-enlistment by students in all subjects.')

            if (College(targetCollege)%AllowEnlistmentInALL(thisTerm)) then
                College(targetCollege)%AllowEnlistmentInPE(thisTerm) = .false.
            end if
        end if
        call html_college_info(device, targetCollege)

    end subroutine toggle_allow_students_to_enlistALL



    subroutine write_student_info_csv(device)

        integer, intent (in) :: device

        integer :: ierr, std, idx
        character (len=MAX_LEN_XML_LINE) :: line

        write(device,AFORMAT) &
            '#', &
            '#  Student info for '//UniversityCodeNoMirror, &
            '#', &
            '# Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                        FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
            '#', '#', &
            '"StdNo","Name","Gender","Curriculum","Adviser","Country","Classification","BirthDate",'// &
            '"BirthPlace","HomeAddress","EntryDate","GraduationDate","LastAttended","AdmissionData",'// &
            '"Scholarship","TranscriptRemark"'

        do idx=1,NumStudents+NumAdditionalStudents

            std = StdRank(idx)

            call xml_read_student_info(std, ierr)

            line = COMMA//'"'//trim(StudentInfo%TranscriptRemark)//'"'
            line = COMMA//'"'//trim(StudentInfo%Scholarship)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%AdmissionData)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%LastAttended)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%GraduationDate)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%EntryDate)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%HomeAddress)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%BirthPlace)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%BirthDate)//'"'//line
            line = COMMA//'"'//trim(itoa(Student(std)%Classification))//'"'//line
            line = COMMA//'"'//trim(itoa(StudentInfo%CountryIdx))//'"'//line
            line = COMMA//'"'//trim(Student(std)%Adviser)//'"'//line
            line = COMMA//'"'//trim(Curriculum(Student(std)%CurriculumIdx)%Code)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%Gender)//'"'//line
            line = COMMA//'"'//trim(Student(std)%Name)//'"'//line
            line = '"'//trim(Student(std)%StdNo)//'"'//line

            write(device,AFORMAT) trim(line)
        end do
        close(device)

    end subroutine write_student_info_csv


    subroutine check_permission()
        logical :: permitted

        permitted = .true.
        select case (REQUEST)

#if defined no_password_check
#else
            case (fnStop)
                permitted = .false.
#endif

            case (fnSwitchPeriod)
                permitted = .false.

            case (fnSwitchTerm)
                permitted = .false.

            case (fnFileDownload, fnReload, fnEditMOTD, fnEditEquivalencies, fnEditFees)
                permitted = .false.

            case (fnStudentAdd, fnEditScholarships, fnEditSignatories)
                permitted = .false.

            case (fnGenerateTimetables)
                permitted = .false.

            case (fnClearTimetables)
                permitted = .false.

        end select

        if (.not. permitted) then
            loginCheckMessage = 'Operation "'//trim(fnDescription(REQUEST) )//'" failed. '//sorryMessageOfficial
            REQUEST = fnLogin
        end if

    end subroutine check_permission


    subroutine average_grade_summary_list(device) !, NumSections, Section, eList )
        integer, intent (in) :: device
        !integer, intent (in), optional :: NumSections
        !type (TYPE_SECTION), intent(in), optional :: Section(0:)
        !type (TYPE_PRE_ENLISTMENT), intent(in), optional :: eList(0:)

        integer :: ierr, idx, ldx, tdx, std, nClasses(3), nAdvised(3), term, tYear, tTerm, n_count
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo, tAction
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum

        real :: SumEnrolled, SumEarned, SumDown, SumUp, tUnits
        real :: GSumEnrolled, GSumEarned, GSumDown, GSumUp

        character(len=10) :: str_cumulative = 'CUMULATIVE'
        character(len= 9) :: str_last_term  = 'LAST TERM'

        integer :: prevtaken, grd, crse
        real :: up, down

        ! which Curriculum ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
        call cgi_get_named_string(QUERY_STRING, 'A2', tAction, ierr)
        call html_comment('average_grade_summary_list('//trim(tCurriculum)//' by '//tAction//')')

        call html_write_header(device, 'Weighted average grade of '//trim(tCurriculum)//' students')

        write(device,AFORMAT) beginitalic//str_cumulative//' and '//str_last_term// &
            ' numbers are : Weighted Average Grade, Units Enrolled, Units Earned.'//enditalic

        write(device,AFORMAT) linebreak, linebreak, &
            '<table border="0" width="100%">', &
            begintr//thalignleft//'#'//endth//thalignleft//'STDNO'//endth, &
            thalignleft//'NAME OF STUDENT'//endth//thalignleft//'CURRIC'//endth

        if (trim(tAction)==str_cumulative) then
            write(device,AFORMAT) thalignleft//trim(str_cumulative)//endth
        else
            write(device,AFORMAT) &
                trim(make_href(fnSummaryWAG, str_cumulative, A1=tCurriculum, A2=str_cumulative, pre=thalignleft, post=endth))
        end if

        if (trim(tAction)==str_last_term) then
            write(device,AFORMAT) thalignleft//trim(str_last_term)//endth
        else
            write(device,AFORMAT) &
                trim(make_href(fnSummaryWAG, str_last_term, A1=tCurriculum, A2=str_last_term, pre=thalignleft, post=endth))
        end if

        write(device,AFORMAT) thalignleft//'LINKS'//endth//endtr

        n_count = 0
        tArray = 0
        do idx=1,NumStudents+NumAdditionalStudents
            std = StdRank(idx)
            ldx = Student(std)%CurriculumIdx
            if (CurrProgCode(ldx) /= tCurriculum) cycle

            ! remember index to student
            tArray(n_count+1) = std

            ! read checklist
            call read_student_records (std)

            SumUp = 0.0
            SumDown = 0.0
            GSumDown = 0.0
            GSumUp = 0.0
            SumEnrolled = 0.0
            SumEarned = 0.0
            GSumEnrolled = 0.0
            GSumEarned = 0.0
            prevtaken = -10

            do tdx=1,lenTCG

                ! FINALGRADES only (with completions if applicable)
                if (TCG(tdx)%Code/=3 .or. TCG(tdx)%Used .or. TCG(tdx)%ErrorCode>1) cycle

                if (prevtaken /= TCG(tdx)%Taken) then

                    ! write summary for current term
                    if (SumUp*SumDown>0.0) then

                        tArray(n_count+2) = int(1.0E5*SumUp/SumDown)
                        tArray(n_count+3) = int(SumEnrolled)
                        tArray(n_count+4) = int(SumEarned)

                        GSumEnrolled = GSumEnrolled + SumEnrolled
                        GSumEarned = GSumEarned + SumEarned

                        GSumUp = GSumUp + SumUp
                        GSumDown = GSumDown + SumDown

                    end if

                    ! re-initialize accumulators
                    prevtaken = TCG(tdx)%Taken
                    SumUp = 0.0
                    SumDown = 0.0
                    SumEnrolled = 0.0
                    SumEarned = 0.0

                end if

                crse = TCG(tdx)%Subject
                grd = TCG(tdx)%Grade
                tUnits = Subject(crse)%Units

                up = 0.0
                down = 0.0

                if (TCG(tdx)%ReExam/=0) then
                    grd = TCG(tdx)%ReExam
                end if

                if (tUnits == 0.0 .or. Subject(crse)%Name(1:5)=='NSTP ') then
                    ! exclude

                else if (grd==gdxDRP .or. grd==gdxPASS) then
                    ! exclude

                else if (grd==gdxINC .or. grd==gdxNFE .or. grd==gdxREGD) then
                    SumEnrolled = SumEnrolled + tUnits

                else
                    SumEnrolled = SumEnrolled + tUnits
                    if (is_grade_numeric_pass(grd)) SumEarned = SumEarned + tUnits
                    up = tUnits*fGrade(grd)
                    if (up/=0.0) then
                        down = tUnits
                    else
                        down = 0.0
                    end if
                end if

                SumUp = SumUp + Up
                SumDown = SumDown + Down
            end do

            ! write summary for last term
            if (SumUp*SumDown>0.0) then

                tArray(n_count+2) = int(1.0E5*SumUp/SumDown)
                tArray(n_count+3) = int(SumEnrolled)
                tArray(n_count+4) = int(SumEarned)

                GSumEnrolled = GSumEnrolled + SumEnrolled
                GSumEarned = GSumEarned + SumEarned

                GSumUp = GSumUp + SumUp
                GSumDown = GSumDown + SumDown

            end if

            ! cumulative average
            if (GSumUp*GSumDown>0.0) then

                tArray(n_count+5) = int(1.0E5*GSumUp/GSumDown)
                tArray(n_count+6) = int(GSumEnrolled)
                tArray(n_count+7) = int(GSumEarned)
            end if

            n_count = n_count + 7

        end do

        if (trim(tAction)==str_cumulative) then ! sort by +5
            tdx = 5
        else ! if (trim(tAction)==str_last_term) then ! sort by +2
            tdx = 2
        end if
        do idx=0,n_count-8,7
            do ldx=idx+7,n_count,7
                if (tArray(idx+tdx)<tArray(ldx+tdx)) then ! swap
                    tArray(n_count+1:n_count+7) = tArray(idx+1:idx+7)
                    tArray(idx+1:idx+7) = tArray(ldx+1:ldx+7)
                    tArray(ldx+1:ldx+7) = tArray(n_count+1:n_count+7)
                end if
            end do
        end do

        do idx=0,n_count-1,7
            tdx = idx/7 + 1
            std = tArray(idx+1)
            tStdNo = Student(std)%StdNo
            ldx = Student(std)%CurriculumIdx
            call count_preenlistment(std, 0, nClasses, nAdvised)

            write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(tdx,2))//'">'// &
                begintd//trim(itoa(tdx))//DOT//endtd//begintd//tStdNo//endtd, &
                begintd//trim(Student(std)%Name)//endtd, &
                begintd//trim(Curriculum(ldx)%Code)//endtd

            ! cumulative average
            if (tArray(idx+5)>0) then
                write(device,'(a,f8.2,a,i4,a,i4, a)') &
                    begintd, tArray(idx+5)/1.0E5, COMMA, tArray(idx+6), COMMA, tArray(idx+7), endtd
            end if

            ! write summary for last term
            if (tArray(idx+2)>0) then
                write(device,'(a,f8.2,a,i4,a,i4, a)') &
                    begintd, tArray(idx+2)/1.0E5, COMMA, tArray(idx+3), COMMA, tArray(idx+4), endtd
            end if

            ! links
            write(device,AFORMAT) begintd//beginsmall, &
                trim(make_href(fnStudentGrades, 'Grades', A1=tStdNo, pre=nbsp)), &
                trim(make_href(fnEditCheckList, 'Checklist', A1=tStdNo, pre=nbsp))
            do tTerm=termBegin,termEnd
                call qualify_term (tTerm, tYear, term)
                if (nClasses(term)+nAdvised(term)>0) write(device,AFORMAT) &
                    trim(make_href(fnStudentClasses, txtSemester(term+6), A1=tStdNo, A9=term, pre=nbsp))
            end do
            write(device,AFORMAT) endsmall//endtd, endtr

        end do

        write(device,AFORMAT) endtable, horizontal

    end subroutine average_grade_summary_list


    subroutine switch_user(device)

        integer, intent (in) :: device

        character (len=MAX_LEN_USERNAME) :: tUserName
        integer :: ierr

        isRoleOfficial = .false.
        isRoleSysAd = .false.
        isRoleStaff = .false.
        isRoleDean = .false.
        isRoleChair = .false.
        isRoleFaculty = .false.
        isRoleStudent = .false.
        isRoleGuest = .false.
        isRoleBenefactor = .false.

        ! which user?
        call cgi_get_named_string(QUERY_STRING, 'A1', tUserName, ierr)
        requestingTeacher = index_to_teacher(tUserName)
        requestingStudent = index_to_student(tUserName)

        if (requestingTeacher>0) then ! in Teacher()

            DeptIdxUser = Teacher(requestingTeacher)%DeptIdx
            CollegeIdxUser = Department(DeptIdxUser)%CollegeIdx
            ROLE = Teacher(requestingTeacher)%Role
            USERNAME = tUserName
            Teacher(requestingTeacher)%OnlineStatus = tick

            select case (ROLE)

                case (GUEST)
                    isRoleGuest = .true.
                    ROLE = SPACE

                case (FACULTY)
                    isRoleFaculty = .true.
                    targetDepartment = DeptIdxUser

                case (CHAIR)
                    isRoleChair = .true.
                    targetDepartment = DeptIdxUser

                case (DEAN)
                    isRoleDean = .true.
                    DeptIdxUser = 0

                case (STAFF)
                    isRoleStaff = .true.
                    !ROLE = SPACE

                case (OFFICIAL)
                    isRoleOfficial = .true.
                    DeptIdxUser = NumDepartments
                    CollegeIdxUser = NumColleges

                case (BENEFACTOR)
                    isRoleBenefactor = .true.
                    DeptIdxUser = NumDepartments
                    CollegeIdxUser = NumColleges
                    ROLE = SPACE

                case (SYSAD)
                    isRoleSysAd = .true.
                    DeptIdxUser = NumDepartments
                    CollegeIdxUser = NumColleges
                    ROLE = SPACE

            end select

        elseif (requestingStudent>0) then ! in Student()

            ROLE = SPACE
            isRoleStudent = .true.
            targetStudent = requestingStudent
            targetCurriculum = Student(requestingStudent)%CurriculumIdx
            CurriculumIdxUser = targetCurriculum
            CollegeIdxUser = Curriculum(targetCurriculum)%CollegeIdx
            DeptIdxUser = 0
            USERNAME = tUserName
            Student(requestingStudent)%OnlineStatus = tick

        else ! assume Guest
            USERNAME = GUEST
            ROLE = SPACE
            isRoleGuest = .true.
            requestingTeacher = index_to_teacher(USERNAME)
            DeptIdxUser = Teacher(requestingTeacher)%DeptIdx
            CollegeIdxUser = Department(DeptIdxUser)%CollegeIdx
            Teacher(requestingTeacher)%OnlineStatus = tick
        end if

        call html_college_links(device, CollegeIdxUser, mesg='Logged in by ADMIN')

    end subroutine switch_user


end program SERVER
