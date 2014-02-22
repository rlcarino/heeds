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
    call run_webserver()

contains


    subroutine run_webserver()

        integer :: checkSumQuery, iTmp, kStart
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        character (len=MAX_LEN_PASSWD_VAR) :: Password
        character(len=MAX_LEN_FILE_PATH) :: fileUSERlog
        integer :: checkSum, jTmp

        call log_comment(trim(fileEXE)//' server started ...')

        ! read basic data for university
        call xml_read_basic_data(pathToYear)

        sorryMessage = trim(UniversityCode)//' officials and their friends '// &
            ' have a "read-only" permission at this time. '// &
            ' Please see the '//trim(titleTheRegistrar)//' if you wish to change some data.'

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

            ! date changed ?
            if (currentDate/=previousDate) then
                backspace (unitHTML)
                call html_comment('Closing this file due to date change')
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
                    trim(tTeacher)//'.log'
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

        end do ! while (FCGI_Accept() >= 0)

        ! backspace the response file for additional "call html_comment()" before stop
        backspace (unitHTML)

#if defined no_password_check
#else
        ! write data
        call xml_write_data()
        if (NumWaiverRecords>0) then
            call qualify_term(nextTerm, iTmp, jTmp)
            pathToTerm = trim(dirDATA)//trim(itoa(iTmp))//DIRSEP//trim(txtSemester(jTmp))//DIRSEP
            call xml_write_waivers(pathToTerm, Section(jTmp,0:), jTmp)
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

        integer                           :: i
        integer                           :: iLen
        character(len=1)                  :: ch
        character(len=7)                  :: cLen
        !character(len=MAX_LEN_FILE_PATH)  :: httpVariable

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

        ! QUERY_STRING (request method was GET) ?
        call get_environment_variable( "QUERY_STRING", value=QUERY_STRING, length=iLen )

        if ( iLen > 0 ) then

                if (iLen>MAX_LEN_QUERY_STRING) QUERY_STRING = '(QUERY_STRING too long.)'
                call html_comment('QUERY_STRING='//QUERY_STRING(:iLen))

        else

            ! anything in CONTENT_LENGTH (request method was POST) ?
            call get_environment_variable( "CONTENT_LENGTH", value=cLen, length=iLen )
            if ( iLen > 0 ) then
                call html_comment('CONTENT_LENGTH='//trim(cLen))
                read( cLen, * ) iLen
                if (iLen<=MAX_LEN_QUERY_STRING) then
                    do i=1,iLen
                        ch = FCGI_getchar()
                        QUERY_STRING( i:i ) = ch
                    end do
                    QUERY_STRING( iLen+1: ) = SPACE
                else
                    QUERY_STRING =  '(CONTENT_LENGTH too big.)'
                end if
                call html_comment('CONTENT='//trim(QUERY_STRING))
            end if

        endif

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

    end subroutine FCGI_getquery



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
            if (cipher(1:5)=='DBG::') cycle
            !if (cipher(1:5)=='DBG::') then
            !    if (isRoleSysAd) then
            !        cipher = '<pre>'//trim(cipher)//'</pre>'
            !    else
            !        cycle
            !    end if
            !end if
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
            loginCheckMessage = PROGNAME//' is for use by the Registrar''s Office '// &
                beginitalic//'only'//enditalic//' at this time.'
        end if

        call html_comment('FN='//fnDescription(REQUEST), 'PATH='//pathToTerm, 'DESC='//termDescription)

        ! trap official role attempting to change data
        if (isRoleOfficial) call check_permission()

        select case (REQUEST)

            case (fnStop)
                call html_landing_page(device, '<h3>The program will stop</h3>')

            case (fnSwitchPeriod)
                ! write data
                call xml_write_data()
                if (NumWaiverRecords>0) then
                    call qualify_term(nextTerm, tYear, tTerm)
                    pathToTerm = trim(dirDATA)//trim(itoa(tYear))//DIRSEP//trim(txtSemester(tTerm))//DIRSEP
                    call xml_write_waivers(pathToTerm, Section(tTerm,0:), tTerm)
                end if

                call switch_to_period()
                call html_college_links(device, CollegeIdxUser, mesg='Period in term changed.')

            case (fnSwitchTerm)
                ! write data
                call xml_write_data()
                if (NumWaiverRecords>0) then
                    call qualify_term(nextTerm, tYear, tTerm)
                    pathToTerm = trim(dirDATA)//trim(itoa(tYear))//DIRSEP//trim(txtSemester(tTerm))//DIRSEP
                    call xml_write_waivers(pathToTerm, Section(tTerm,0:), tTerm)
                end if

                call switch_to_term()
                call html_college_links(device, CollegeIdxUser, mesg='Academic term advanced; period is Classlists')

            case (fnLogin)
                call html_college_links(device, CollegeIdxUser, mesg=loginCheckMessage)

            case (fnChangeTeacherPassword)
                call change_current_teacher_password(device)

            case (fnChangeStudentPassword)
                call change_current_student_password(device)

            case (fnResetPasswords)

                if (isRoleOfficial) then

                    call html_college_links(device, CollegeIdxUser, mesg='"Reset ALL passwords" failed. '//sorryMessage)

                else
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

            case (fnToggleEditGrade)
                if (isRoleSysAd) then
                    isEnabledEditGrade = .not. isEnabledEditGrade
                    call html_college_links(device, CollegeIdxUser, mesg='Toggled "Enable/Disable edit grade"')
                else
                    REQUEST = fnLogout
                    Teacher(requestingTeacher)%OnlineStatus = 0
                    call html_landing_page(device, SPACE)
                end if

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

            case (fnStudentsByCurriculum, fnOnlineStudents, fnFindStudent)
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

            case (fnStudentsWithConflicts, fnStudentPriority)
                call links_to_students(device, REQUEST, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    Enlistment(targetTerm,0:))

!                ! cases for next semester's predicted demand for subjects
!                case (fnDemandFreshmen,fnUpdateDemandFreshmen)
!                    call demand_by_new_freshmen(device, Offering(nextTerm,MAX_ALL_DUMMY_SUBJECTS:))

            case (fnDemandForSubjects)
                call demand_for_subjects(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), Enlistment(targetTerm,0:))

            case (fnUpdateDemandForSubjects)
                if (.not. isPeriodOne) then
                    targetTerm = nextTerm
                    call needs_analysis(targetTerm, isPeriodThree .or. isPeriodFour, &
                        Section(targetTerm,0:), Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), Enlistment(targetTerm,0:), &
                        trim(pathToNextYear)//trim(txtSemester(targetTerm))//DIRSEP, loginCheckMessage)

                    call html_write_header(device, SPACE, loginCheckMessage)
                    write(device,AFORMAT) trim(make_href(fnDemandForSubjects, College(targetCollege)%Code, &
                        A1=College(targetCollege)%Code, A9=nextTerm, pre=linebreak// &
                            'Needs Analysis for ', post=linebreak//linebreak//horizontal) )
                else

                    call html_write_header(device, SPACE, 'Needs Analysis not available')
                    write(device,AFORMAT) &
                        linebreak//'Must be Period 2 - Advising or Period 3 - Grading to run Needs Analysis'// &
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

            case (fnAllowStudentsToEnlist)
                call toggle_allow_students_to_enlist(device, targetTerm )

            case (fnStudentForceEnlist)
                call enlistment_forced(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    Enlistment(targetTerm,0:) )

            case (fnFindBlock)
                call enlistment_find_block(device, targetTerm, Section(targetTerm,0:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:), Enlistment(targetTerm,0:) )

            case (fnPrintableSchedule)
                call enlistment_printable(device, NumSections(targetTerm), Section(targetTerm,0:), Enlistment(targetTerm,0:))


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
                    ROLE = SPACE

                case (OFFICIAL)
                    isRoleOfficial = .true.
                    DeptIdxUser = NumDepartments
                    CollegeIdxUser = NumColleges

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
                ' You are logged in as Guest. Visit the '//trim(UniversityCode)//SPACE// &
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
        integer :: errNo, i, j, k
        character (len=MAX_LEN_PASSWD_VAR) :: XMLfile!, Password

        call cgi_get_named_string(QUERY_STRING, 'A1', XMLfile, errNo)

        call html_comment('download_file('//trim(XMLfile)//')')

        select case(trim(XMLfile))

!            case ('PASSWORDS-TEACHERS.CSV')
!
!                write(device,AFORMAT) &
!                    '#', &
!                    '#  !!!!!!!!! FOR THE REGISTRAR''S EYES ONLY !!!!!!!!! ', &
!                    '#  !!!!!!!!!     PASSWORDS-TEACHERS.CSV     !!!!!!!!! ', &
!                    '#', &
!                    '# Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
!                                FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
!                    '# NAME - name of teacher', &
!                    '# UNIT - academic unit', &
!                    '# USERNAME - HEEDS login name', &
!                    '# PASSWORD - HEEDS password', &
!                    '# ROLE - HEEDS role', &
!                    '# COMMENT - Date received/Initials', &
!                    '#', &
!                    '"NAME","UNIT","USERNAME","PASSWORD","ROLE","COMMENT"'
!
!                do i=1,NumTeachers+NumAdditionalTeachers
!                    call get_teacher_password(i, Password)
!                    if (Teacher(i)%Role/=SYSAD ) then
!                        write(device,AFORMAT) &
!                            '"'//Teacher(i)%Name//'","'// &
!                                 Department(Teacher(i)%DeptIdx)%Code//'","'// &
!                                 Teacher(i)%TeacherId//'","'// &
!                                 Password(:MAX_LEN_PASSWORD)//'","'// &
!                                 Teacher(i)%Role//'"'
!                    end if
!                end do
!
!            case ('PASSWORDS-STUDENTS.CSV')
!
!                write(device,AFORMAT) &
!                    '#', &
!                    '#  !!!!!!!!! FOR THE REGISTRAR''S EYES ONLY !!!!!!!!! ', &
!                    '#  !!!!!!!!!     PASSWORDS-STUDENTS.CSV     !!!!!!!!! ', &
!                    '#', &
!                    '# Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
!                                FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
!                    '# NAME - name of student', &
!                    '# CURRICULUM - curriculum', &
!                    '# USERNAME - HEEDS login name', &
!                    '# PASSWORD - HEEDS password', &
!                    '# COMMENT - Date received/Initials', &
!                    '#', &
!                    '"NAME","CURRICULUM","USERNAME","PASSWORD","COMMENT"'
!
!                do j=1,NumStudents+NumAdditionalStudents
!                    i = StdRank(j)
!                    call get_student_password(i, Password)
!                    write(device,AFORMAT) &
!                        '"'//Student(i)%Name//'","'// &
!                             Curriculum(Student(i)%CurriculumIdx)%Code//'","'// &
!                             Student(i)%StdNo//'","'// &
!                             Password(:MAX_LEN_PASSWORD)//'"'
!                end do


            case ('BACKUP.XML')

                fileName = trim(XMLfile)//DASH//trim(UniversityCode)//DASH//currentDate//DASH//currentTime(:6)
                open(unit=unitXML, file=trim(WEBROOT)//fileName)
                write(unitXML,AFORMAT) XML_DOC

                call xml_university(unitXML)
                call xml_colleges(unitXML)
                call xml_departments(unitXML)
                call xml_subjects(unitXML)
                call xml_curricula(unitXML)
                call xml_equivalencies(unitXML)
                call xml_rooms(unitXML)
                call xml_teachers(unitXML)
                call xml_students(unitXML, 0)

                do k=termBegin,termEnd
                    call qualify_term (k, i, j)

                    call xml_classes(unitXML, NumSections(j), Section(j,0:), 0, j)
                    call xml_blocks(unitXML, NumBlocks(j), Block(j,0:), Section(j,0:), 0, j)
                    call xml_pre_enlistment(unitXML, Enlistment(j,0:), Section(j,0:), 0, j)

                end do
                if (NumWaiverRecords>0) then
                    call qualify_term (nextTerm, i, j)
                    call xml_waivers(unitXML, Section(j,0:), j)
                end if

                close(unitXML)

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


            case ('CLASSES-BLOCKS.CSV')

                call write_classes_blocks_csv (device, Term, NumSections(Term), Section(Term,0:), &
                    NumBlocks(Term), Block(Term,0:) )


            case ('ENLISTMENT.CSV')
                call write_enlistment_csv (device, Term, Enlistment(Term,0:), Section(Term,0:) )


            case ('STUDENT-INFO.CSV')

                fileName = 'STUDENT-INFO-'//trim(UniversityCode)//'.CSV'
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

            case ('Preregistration')
                isPeriodFour = .true.
                isProbabilistic = .false.

            case default

        end select
        termBegin = cTm1
        termEnd = termBegin+2

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

        if (.not. isPeriodOne) then

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


    subroutine switch_to_term()
        integer :: iTmp, jTmp, kTmp, prevCurrentTerm, prevCurrentYear
        character(len=5) :: nextYearDir

        prevCurrentYear = currentYear
        prevCurrentTerm = currentTerm
        call cgi_get_named_integer(QUERY_STRING, 'A1', currentTerm, iTmp)

!        ! return to previous academic year?
!        if (prevCurrentTerm==1 .and. currentTerm==3) then
!            currentYear = currentYear-1
!            ! create directories for year before
!            nextYearDir = trim(itoa(currentYear-1))//DIRSEP
!            call make_directory( trim(dirDATA)//nextYearDir)
!            call make_directory( trim(dirBACKUP)//nextYearDir)
!            do iTmp=1,3
!                call make_directory( trim(dirDATA)//nextYearDir//trim(txtSemester(iTmp))//DIRSEP )
!                call make_directory( trim(dirBACKUP)//nextYearDir//trim(txtSemester(iTmp))//DIRSEP )
!            end do
!
        ! proceed to next academic year?
        if (prevCurrentTerm==3 .and. currentTerm==1) then
            currentYear = currentYear+1
            ! create directories for year after
            nextYearDir = trim(itoa(currentYear+1))//DIRSEP
            call make_directory( trim(dirDATA)//nextYearDir)
            call make_directory( trim(dirBACKUP)//nextYearDir)
            do iTmp=1,3
                call make_directory( trim(dirDATA)//nextYearDir//trim(txtSemester(iTmp))//DIRSEP )
                call make_directory( trim(dirBACKUP)//nextYearDir//trim(txtSemester(iTmp))//DIRSEP )
            end do

        end if

        call log_comment('switch_to_term('//text_term_school_year(currentTerm,currentYear)//')')
        call html_comment('switch_to_term('//text_term_school_year(currentTerm,currentYear)//')')

        ! compute relative years and terms (before and after current)
        call initialize_past_future_years_terms ()

        ! re-initialize term-specific data
        call initialize_term_data()

        ! re-initialize Student() if school year changed
        if (currentYear/=prevCurrentYear) then

            call initialize_student(Student(0))
            Student = Student(0)
            NumStudents = 0
            NumAdditionalStudents = 0
            do iTmp=1,MAX_ALL_STUDENTS
                StdRank(iTmp) = iTmp
            end do

            ! retrieve older list of students, update with newer list
            iTmp = min(currentYear, prevCurrentYear)
            jTmp = max(currentYear, prevCurrentYear)

            call read_students(trim(dirDATA)//trim(itoa(iTmp))//DIRSEP, kTmp)
            call read_students(trim(dirDATA)//trim(itoa(jTmp))//DIRSEP, kTmp)

            ! create student directories
            call make_student_directories()

        end if

        ! reset period
        call switch_to_period('Classlists')

    end subroutine switch_to_term


    subroutine toggle_allow_students_to_enlist(device, thisTerm)

        integer, intent(in) :: device, thisTerm

        integer :: cdx
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege

        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, cdx)
        targetCollege = index_to_college(tCollege)
        call html_comment('toggle_allow_students_to_enlist('//tCollege//itoa(thisTerm)//')')

        if (isRoleOfficial) then
            call html_write_header(device, College(targetCollege)%Code//'- '//College(targetCollege)%Name, &
                sorryMessage)
        else
            College(targetCollege)%AllowEnlistmentByStudents(thisTerm) = &
                .not. College(targetCollege)%AllowEnlistmentByStudents(thisTerm)
            call html_write_header(device, College(targetCollege)%Code//'- '//College(targetCollege)%Name, &
                'Toggled allow/disallow self-enlistment by students.')
        end if
        call html_college_info(device, targetCollege)

    end subroutine toggle_allow_students_to_enlist



    subroutine write_student_info_csv(device)

        integer, intent (in) :: device

        integer :: ierr, std, idx
        character (len=MAX_LEN_XML_LINE) :: line

        write(device,AFORMAT) &
            '#', &
            '#  Student info for '//UniversityCode, &
            '#', &
            '# Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                        FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
            '#', '#', &
            '"StdNo","Name","Gender","Curriculum","Adviser","Country","Classification","BirthDate",'// &
            '"BirthPlace","HomeAddress","EntryDate","GraduationDate","LastAttended","AdmissionData","Scholarship"'

        do idx=1,NumStudents+NumAdditionalStudents

            std = StdRank(idx)

            call xml_read_student_info(std, ierr)

            line = COMMA//'"'//trim(StudentInfo%Scholarship)//'"'
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
            line = COMMA//'"'//trim(Student(std)%Gender)//'"'//line
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

            case (fnFileDownload)
                permitted = .false.

            case (fnToggleEditGrade)
                permitted = .false.

            case (fnStudentAdd)
                permitted = .false.

            case (fnClearTimetables)
                permitted = .false.

        end select

        if (.not. permitted) then
            loginCheckMessage = 'Operation "'//trim(fnDescription(REQUEST) )//'" failed. '//sorryMessage
            REQUEST = fnLogin
        end if

    end subroutine check_permission

end program SERVER
