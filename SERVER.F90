!======================================================================
!
!    HEEDS (Higher Education Enrollment Decision Support) - A program
!      to create enrollment scenarios for 'next term' in a university
!    Copyright (C) 2012, 2013 Ricolindo L. Carino
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


program MAIN

    use XMLIO
    use INITIALIZE
    use DisplayROOMS
    use DisplayTEACHERS
    use DisplaySUBJECTS
    use DisplayCURRICULA
    use DisplaySECTIONS
    use DisplayBLOCKS
    use EditUNIVERSITY
    use EditROOMS
    use EditTEACHERS
    use EditSUBJECTS
    use EditCURRICULA
    use EditSECTIONS
    use EditBLOCKS

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


    integer :: iTmp, jTmp, kTmp, kStart, errNo
    character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
    character (len=MAX_LEN_PASSWD_VAR) :: Password
    character(len=MAX_LEN_FILE_PATH) :: fileUSERlog
    real :: harvest    ! random number
    integer :: checkSum, checkSumQuery

    ! static initializations
    call initializations()

    ! open a 'scratch' HTML response file
    open(unit=unitHTML, file=trim(dirLOG)//'scratch.html', form='formatted', status='unknown')

    call log_comment(trim(fileEXE)//' started ...')

    ! initialize log for requests
    fileREQ = trim(dirLOG)//'requests-'//currentDate//'.log'
    call log_request(unitREQ, trim(fileREQ), &
        '#-------', '#Begins '//currentDate//DASH//currentTime, '#-------', &
        '#Executable is : '//trim(fileEXE)//SPACE//DASH//SPACE//'( '//PROGNAME//VERSION//')', &
        '#Arguments are : '//trim(UniversityCode)//SPACE//itoa(currentYear)//trim(txtSemester(currentTerm)) )

    ! directory for start of School Year
    pathToYear = trim(dirDATA)//trim(itoa(currentYear))//DIRSEP

    ! directory for year of next term
    pathToNextYear = trim(dirDATA)//trim(itoa(nextYear))//DIRSEP

    ! schedule classes for the whole year
    termBegin = currentTerm
    termEnd = termBegin+2 ! nextTerm

    ! read data based on ACTION
    select case (trim(ACTION))

        case ('Import')

            ! assume custom data
            call custom_read_basic_data()

        case default

            ! assume XML data
            call xml_read_basic_data(pathToYear)

            ! read schedules
            do kTmp=termBegin,termEnd
                call qualify_term (kTmp, iTmp, jTmp, fileUSERlog)
                pathToTerm = trim(dirDATA)//trim(itoa(iTmp))//DIRSEP//trim(txtSemester(jTmp))//DIRSEP
                ! read the classes
                call read_classes(pathToTerm, NumSections(jTmp), Section(jTmp,0:), &
                    Offering(jTmp,MAX_ALL_DUMMY_SUBJECTS:), errNo)
                ! read the blocks
                call read_blocks(pathToTerm, NumBlocks(jTmp), Block(jTmp,0:), NumSections(jTmp), Section(jTmp,0:), errNo)
                ! get no. of sections by dept
                call count_sections_by_dept(jTmp, NumSections(jTmp), Section(jTmp,0:))
            end do

    end select

    do iTmp=1,NumTeachers
        if (Teacher(iTmp)%Role/=ADMINISTRATION) cycle
        call get_teacher_password(iTmp, Password)
        call log_comment('Password for '//trim(Teacher(iTmp)%TeacherId)//' : '//Password)
    end do

    ! log directory for users in the college
    do iTmp=1,NumColleges
        call make_directory( trim(dirLOG)//trim(College(iTmp)%Code)//DIRSEP )
    end do

    ! initialize QUERY_STRING encryption key
    do iTmp=1,MAX_LEN_QUERY_STRING
        call random_number(harvest)
        kStart = 1 + int(255*harvest)
        queryEncryptionKey(iTmp:iTmp) = achar(kStart)
    end do

    ! make "Stop!" page
#if defined GLNX
    QUERY_put = IP_ADDR
#else
    QUERY_put = 'localhost'
#endif
    CGI_PATH = 'http://'//trim(QUERY_put)//FSLASH//UniversityCode
    USERNAME = PROGNAME
    call html_login('Stop-'//PROGNAME//'.html', trim(make_href(fnStop, 'Stop '//PROGNAME) ) )

    ! reset CGI_PATH
    CGI_PATH = FSLASH//UniversityCode

    ! loop until killed/fnSTOP
    do while (FCGI_Accept() >= 0)

        ! timestamp of request
        CALL SYSTEM_CLOCK(tick, count_rate, count_max)
        call date_and_time (date=currentDate, time=currentTime)

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
                    QUERY_STRING = '(invalid)'
                end if
            else
                QUERY_STRING = '(invalid)'
            end if
        else ! not encrypted
            QUERY_STRING = '(invalid)'
        end if

        ! make copy of QUERY_STRING
        cipher = QUERY_STRING
        call html_comment('revised CONTENT='//trim(QUERY_STRING))

        ! initialize index to target object of REQUEST
        targetSubject = 0
        targetSection = 0
        targetDepartment = 0
        targetCurriculum = 0
        targetCollege = 0
        targetRoom = 0
        targetTeacher = 0
        targetBlock = 0
        targetTerm = 0
        targetStudent = 0

        ! Establish USERNAME/ROLE and REQUEST
        loginCheckMessage = SPACE
        call get_user_request()

        ! open user's log file, create if necessary
        call blank_to_underscore(USERNAME, tTeacher)
        Teacher(requestingTeacher)%Status = tick
        iTmp = Department(Teacher(requestingTeacher)%DeptIdx)%CollegeIdx
        fileUSERlog = trim(dirLOG)// &
            trim(College(iTmp)%Code)//DIRSEP// &
            trim(tTeacher)//'.log'

        if (REQUEST>=fnLogout) then ! no passwords
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

        call html_comment(fnDescription(REQUEST))

        ! compose response
        call server_respond(unitHTML)

        ! send response to server
        call FCGI_putfile(unitHTML)

        ! stop?
        if (REQUEST==fnStop) exit

        ! automatically logout users idle who have been idle for 30 minutes (1800 seconds)
        do iTmp=1,NumTeachers+NumAdditionalTeachers
            if (Teacher(iTmp)%Status==0) cycle
            if ( (tick-Teacher(iTmp)%Status)/count_rate>=maxIdleTime ) Teacher(iTmp)%Status = 0
        end do


    end do ! while (FCGI_Accept() >= 0)

    ! rewind the response file for additional "call html_comment()" before stop
    rewind (unitHTML)

    ! remove "Stop" link
    call unlink('Stop-'//PROGNAME//'.html')

    ! save files
    call xml_write_university(trim(pathToYear)//'UNIVERSITY.XML')
    call xml_write_colleges(trim(pathToYear)//'COLLEGES.XML')
    call xml_write_departments(trim(pathToYear)//'DEPARTMENTS.XML')
    call xml_write_rooms(trim(pathToYear)//'ROOMS.XML')
    call xml_write_teachers(trim(pathToYear)//'TEACHERS.XML')
    call xml_write_subjects(trim(pathToYear)//'SUBJECTS.XML')
    call xml_write_curricula(trim(pathToYear)//'CURRICULA.XML')
    do kTmp=termBegin,termEnd
        call qualify_term (kTmp, iTmp, jTmp, fileUSERlog)
        pathToTerm = trim(dirDATA)//trim(itoa(iTmp))//DIRSEP//trim(txtSemester(jTmp))//DIRSEP
        call xml_write_classes(pathToTerm, NumSections(jTmp), Section(jTmp,0:), 0)
        call xml_write_blocks(pathToTerm, NumBlocks(jTmp), Block(jTmp,0:), Section(jTmp,0:), 0)
    end do

    ! terminate
    close(unitHTML)
    call terminate(trim(fileEXE)//' stopped')


contains


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

        ! write to the beginning of file unitNo
        rewind (unitNo)

        call html_comment('FCGI_getquery()')

        ! the remote IP
        call get_environment_variable('REMOTE_ADDR', value=REMOTE_ADDR, length=iLen, status=i)

        call html_comment('REMOTE_ADDR='//REMOTE_ADDR//SPACE//itoa(iLen)//itoa(i))

        ! QUERY_STRING (request method was GET) ?
        call get_environment_variable( "QUERY_STRING", value=QUERY_STRING, length=iLen )

        if ( iLen > 0 ) then

                call html_comment('QUERY_STRING='//QUERY_STRING(:iLen))

        else

            ! anything in CONTENT_LENGTH (request method was POST) ?
            call get_environment_variable( "CONTENT_LENGTH", value=cLen, length=iLen )
            call html_comment('CONTENT_LENGTH='//trim(cLen))

            if ( iLen > 0 ) then
                read( cLen, * ) iLen
                do i=1,iLen
                    ch = FCGI_getchar()
                    QUERY_STRING( i:i ) = ch
                end do
                QUERY_STRING( iLen+1: ) = ' '
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
            read(unitNo, AFORMAT, iostat=iStat) QUERY_STRING
            if (iStat < 0) exit ! no more lines
            if (QUERY_STRING(1:5)=='DBG::') cycle
            iStat = FCGI_puts (trim(QUERY_STRING)//NUL) ! FCGI_puts expects NULL terminated strings
        end do

    end subroutine FCGI_putfile


    subroutine server_respond (device)
        integer, intent (in) :: device

        integer :: i, tYear, tTerm

        call html_comment('server_respond()')

        ! target directory if files are to be modified
        if (targetTerm>0) then
            if (targetTerm<currentTerm) then ! next school year
                call qualify_term (3+targetTerm, tYear, tTerm, termDescription)
            else
                call qualify_term (targetTerm, tYear, tTerm, termDescription)
            end if
            termDescription = ' for '//termDescription
            pathToTerm = trim(dirDATA)//trim(itoa(tYear))//DIRSEP//trim(txtSemester(tTerm))//DIRSEP
        else
            termDescription = SPACE
            pathToTerm = pathToYear
        end if

        ! suspended ?
        if (isSuspendMode .and. .not. isRoleAdmin ) REQUEST = 0

        select case (REQUEST)

            case (0)
                Teacher(requestingTeacher)%Status = 0
                call html_landing_page(device, SPACE)
                write(device,AFORMAT) &
                    '<br>'//PROGNAME//FSLASH//trim(Action)//' is in suspend-mode. '// &
                    'Only the Administrator is allowed at this time.<hr>'

            case (fnStop)
                call html_landing_page(device, 'The program will stop.')

            case (fnLogin)
                call html_college_links(device, CollegeIdxUser, mesg=loginCheckMessage)

            case (fnChangeTeacherPassword)
                call change_current_teacher_password(device)

            case (fnResetPasswords)
                ! the teachers
                do i=1,NumTeachers+NumAdditionalTeachers
                    if (Teacher(i)%Role/=ADMINISTRATION) then
                        call set_password(Teacher(i)%Password)
                    end if
                end do
                call html_college_links(device, CollegeIdxUser, mesg='Passwords were reset for non-ADMIN roles')

            case (fnLogout)
                Teacher(requestingTeacher)%Status = 0
                call html_landing_page(device, SPACE)

            case (fnSuspendProgram)
                if (isRoleAdmin) then
                    isSuspendMode = .not. isSuspendMode
                    call html_college_links(device, CollegeIdxUser, mesg='Toggled "Suspend" mode')
                else
                    REQUEST = fnLogout
                    Teacher(requestingTeacher)%Status = 0
                    call html_landing_page(device, SPACE)
                end if

            case (fnDownloadXML)
                call xml_download(device)

            case (fnEditSignatories)
                call edit_signatories(device)

            case (fnRecentTeacherActivity)
                call recent_teacher_activity(device)

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

            ! schedule of classes
            case (fnScheduleOfClasses, fnScheduleByArea, fnTBARooms, fnTBATeachers, fnTeacherClasses)
                call section_list_all (device, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:), REQUEST)

            ! schedule conflicts
            case (fnRoomConflicts)
                call room_conflicts (device, NumSections(targetTerm), Section(targetTerm,0:))

            case (fnRoomSchedule)
                call room_schedule(device, NumSections(targetTerm), Section(targetTerm,0:) )

            case (fnTeacherConflicts)
                call teacher_conflicts (device, NumSections(targetTerm), Section(targetTerm,0:))

            case (fnPrintableWorkload)
                call teacher_schedule_printable(device, NumSections(targetTerm), Section(targetTerm,0:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:) )

            ! blocks
            case (fnBlockList)
                call block_list_all(device, NumBlocks(targetTerm), Block(targetTerm,0:), &
                    NumSections(targetTerm), Section(targetTerm,0:))

            case (fnBlockSchedule)
                call block_schedule(device, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:))

            case (fnBlockConflicts)
                call block_conflicts(device, NumBlocks(targetTerm), Block(targetTerm,0:), &
                    NumSections(targetTerm), Section(targetTerm,0:))

            case (fnEditSubject)
                call subject_edit (device)

            case (fnEditCurriculum)
                call curriculum_edit (device)

            case (fnEditRoom)
                call room_edit (device)

            case (fnEditTeacher, fnGenerateTeacherPassword)
                call teacher_edit (device)

            case (fnDeleteTeacher)
                call teacher_delete (device)

            case (fnTeacherEditSchedule)
                call teacher_schedule (device, NumSections(targetTerm), Section(targetTerm,0:) )

            case (fnScheduleOfferSubject)
                call section_offer_subject (device, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:) )

            case (fnScheduleDelete)
                call section_delete(device, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:) )

            case (fnScheduleAddLab)
                call section_add_laboratory(device, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:) )

            case (fnScheduleEdit, fnScheduleValidate)
                call section_validate_inputs (device, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:) )

            case (fnBlockDeleteNotClasses, fnBlockEditName, fnBlockCopy, fnBlockDeleteAlsoClasses, &
                    fnBlockEditSection, fnBlockEditSubject)
                call edit_block(device, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:), REQUEST)

            case (fnBlockNewSelect)
                call block_select_curriculum_year(device, NumSections(targetTerm), Section(targetTerm,0:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:) )

            case (fnBlockNewAdd)
                call block_add(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), NumBlocks(targetTerm), Block(targetTerm,0:) )

            case default
                targetCollege = CollegeIdxUser
                targetDepartment = DeptIdxUser
                termDescription = SPACE
                call html_write_header(device, SPACE, 'Feature not available')
                write(device,AFORMAT) &
                    '<br>The feature  "'//trim(fnDescription(REQUEST))// &
                    '" is not activated in this version of '//PROGNAME//'<hr>'

        end select

        call html_write_footer(device)

    end subroutine server_respond


    subroutine get_user_request()

        character (len=MAX_LEN_PASSWD_VAR) :: tPassword
        integer :: ierr

        call html_comment('get_user_request()')

        isRoleAdmin = .false.
        isRoleDean = .false.
        isRoleChair = .false.
        isRoleGuest = .false.

        DeptIdxUser = 0
        CollegeIdxUser = 0
        CurriculumIdxUser = 0
        requestingTeacher = 0
        requestingStudent = 0

        ! Establish REQUESTed function if any, else return the landing page
        call cgi_get_named_integer(QUERY_STRING, 'F', REQUEST, ierr)
        if (ierr==-1) then
            REQUEST = fnLogout
            !call html_comment('REQUESTed fn = '//itoa(REQUEST)//', ierr = '//itoa(ierr))
            return
        end if
        ! stop by Registrar Role?
        if (REQUEST==fnStop) return

        ! Get USERNAME
        if (REQUEST/=fnLogin) then
            call cgi_get_named_string(QUERY_STRING, 'N', USERNAME, ierr)
        else
            call cgi_get_named_string(QUERY_STRING, 'U', USERNAME, ierr)
        end if
        if (ierr==0) then
            requestingTeacher = index_to_teacher(USERNAME)
        end if

        if (requestingTeacher>0) then ! in Teacher()

            DeptIdxUser = Teacher(requestingTeacher)%DeptIdx
            CollegeIdxUser = Department(DeptIdxUser)%CollegeIdx
            ROLE = Teacher(requestingTeacher)%Role

            select case (ROLE)

                case (GUEST)
                    isRoleGuest = .true.

                case (CHAIR)
                    isRoleChair = .true.
                    targetDepartment = DeptIdxUser

                case (DEAN)
                    isRoleDean = .true.
                    DeptIdxUser = 0

                case (ADMINISTRATION)
                    isRoleAdmin = .true.
                    DeptIdxUser = NumDepartments
                    CollegeIdxUser = NumColleges

            end select

        else ! assume Guest
            USERNAME = GUEST
            ROLE = GUEST
            isRoleGuest = .true.
            requestingTeacher = index_to_teacher(USERNAME)
            DeptIdxUser = Teacher(requestingTeacher)%DeptIdx
            CollegeIdxUser = Department(DeptIdxUser)%CollegeIdx
        end if

        ! Establish TERM if required
        if (REQUEST>=fnScheduleOfClasses) then
            call cgi_get_named_integer(QUERY_STRING, 'A9', targetTerm, ierr)
            if (ierr/=0) targetTerm = currentTerm ! default
        end if

        if (REQUEST/=fnLogin) then ! not logging in
            ! previously logged out?  force user to login
            if (Teacher(requestingTeacher)%Status==0 .and. .not. isRoleGuest) REQUEST = fnLogout
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
        call cgi_get_named_string(QUERY_STRING, 'P', tPassword, ierr)
        if (ierr==-1) then ! no password
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
            else ! return login page
                REQUEST = fnLogout
                loginCheckMessage = 'Username/Password combination not valid.'
            end if
        end if

    end subroutine get_user_request


    subroutine xml_download(device)
        integer, intent(in) :: device

        character (len=MAX_LEN_FILE_PATH) :: fileName
        !character (len=MAX_LEN_XML_LINE) :: line
        integer :: ierr, i !, j
        character (len=MAX_LEN_PASSWD_VAR) :: XMLfile, Password

        call html_comment('xml_download()')

        call cgi_get_named_string(QUERY_STRING, 'A1', XMLfile, ierr)

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
                    if (Teacher(i)%Role/=ADMINISTRATION ) then
                        write(device,AFORMAT) &
                            '"'//Teacher(i)%Name//'","'// &
                                 Department(Teacher(i)%DeptIdx)%Code//'","'// &
                                 Teacher(i)%TeacherId//'","'// &
                                 Password(:MAX_LEN_PASSWORD)//'","'// &
                                 Teacher(i)%Role//'"'
                    end if
                end do


            case ('BACKUP.XML')

                fileName = trim(XMLfile)//DASH//currentDate//DASH//currentTime(:6)
                open(unit=unitXML, file=trim(dirWEB)//fileName)
                write(unitXML,AFORMAT) XML_DOC

                call xml_university(unitXML)
                call xml_colleges(unitXML)
                call xml_departments(unitXML)
                call xml_subjects(unitXML)
                call xml_curricula(unitXML)
                call xml_equivalencies(unitXML)
                call xml_rooms(unitXML)
                call xml_teachers(unitXML)

                call xml_classes(unitXML, NumSections(currentTerm), Section(currentTerm,0:), 0)
                call xml_blocks(unitXML, NumBlocks(currentTerm), Block(currentTerm,0:), Section(currentTerm,0:), 0)

                call xml_classes(unitXML, NumSections(nextTerm), Section(nextTerm,0:), 0)
                call xml_blocks(unitXML, NumBlocks(nextTerm), Block(nextTerm,0:), Section(nextTerm,0:), 0)

                close(unitXML)

                call html_write_header(device, trim(XMLfile)// ' created.', SPACE)
                write(device,AFORMAT)'Right-click on <a href="/'//trim(fileName)// &
                    '">this</a> link, then "Save Link As..."<hr>'
                REQUEST = fnCollegeLinks

            case default

                targetCollege = CollegeIdxUser
                targetDepartment = DeptIdxUser
                termDescription = SPACE
                call html_write_header(device, SPACE, '<hr>File not recognized "'//trim(XMLfile))
                REQUEST = 0
        end select

    end subroutine xml_download


end program main
