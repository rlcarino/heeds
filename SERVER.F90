!======================================================================
!
!    HEEDS (Higher Education Enrollment Decision Support) - A program
!      to create enrollment scenarios for 'next term' in a university
!    Copyright (C) 2012-2015 Ricolindo L. Carino
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
    use EditEVALUATION

    implicit none


    integer :: checkSum, checkSumQuery, iTmp, jTmp, kStart
    character(len=MAX_LEN_USERNAME) :: tTeacher
    logical :: pathExists

    ! collect command-line arguments
    call get_arguments()

    ! compute relative years and terms (before and after current)
    call initialize_past_future_years_terms (currentYear, currentTerm)

    call year_data_initialize () ! initialize basic data

    ! unser set server flag temporarily
    isServer = .false.

    call year_data_read(pathToYear) ! read data for current year

    ! disable Guest?
    if (.not. isEnabledGuest) then
        USERNAME = GUEST
        iTmp = index_to_teacher(USERNAME)
        call initialize_teacher(Teacher(iTmp))
        Teacher(iTmp)%DeptIdx = 0
    end if

    ! check DOCUMENT_ROOT - 'root' for nginx
    inquire(file=trim(DOCUMENT_ROOT), exist=pathExists)
    if (.not. pathExists) &
        call terminate(1, 'Error: DOCUMENT_ROOT '//trim(DOCUMENT_ROOT)//' does not exist')
    call system(cpCmd//trim(DOCUMENT_ROOT)//DIRSEP//'50x-under-maintenance.html '// &
        trim(DOCUMENT_ROOT)//DIRSEP//'50x.html')

    ! create a download directory in DOCUMENT_ROOT
    dirDOWNLOADS = trim(DOCUMENT_ROOT)//DIRSEP//'downloads'//DIRSEP
    urlDOWNLOADS = trim(SERVER_PROTOCOL)//trim(SERVER_NAME)//FSLASH//'downloads'//FSLASH
    inquire(file=dirDOWNLOADS, exist=pathExists)
    if (.not. pathExists) then
        call system (mkdirCmd//trim(dirDOWNLOADS), iTmp)
        call terminate(iTmp, 'Error: cannot create directory '//dirDOWNLOADS)
    end if

    ! write a file to the directory
    open(unit=unitETC, file=trim(dirDOWNLOADS)//'test', form='formatted', status='unknown', iostat=iTmp)
    call terminate(iTmp, 'Error: cannot write to directory '//dirDOWNLOADS)
    write(unitETC,AFORMAT) 'This file was created by '//trim(fileEXE)//' on '//startDateTime
    close(unitETC)

    ! create a picure directory in DOCUMENT_ROOT
    dirPICTURES = trim(DOCUMENT_ROOT)//DIRSEP//'pictures'//DIRSEP
    urlPICTURES = trim(SERVER_PROTOCOL)//trim(SERVER_NAME)//FSLASH//'pictures'//FSLASH
    inquire(file=dirPICTURES, exist=pathExists)
    if (.not. pathExists) then
        call system (mkdirCmd//trim(dirPICTURES), iTmp)
        call terminate(iTmp, 'Error: cannot create directory '//dirPICTURES)
    end if

    ! write a file to the directory
    open(unit=unitETC, file=trim(dirPICTURES)//'test', form='formatted', status='unknown', iostat=iTmp)
    call terminate(iTmp, 'Error: cannot write to directory '//dirPICTURES)
    write(unitETC,AFORMAT) 'This file was created by '//trim(fileEXE)//' on '//startDateTime
    close(unitETC)

    ! generate key to obfuscate queries
    call regenerate_obfuscator()

    sorryMessageOfficial = red//trim(UniversityCode)//' officials and their friends '// &
        ' have a "read-only" permission at this time. '// &
        ' See the '//trim(titleTheRegistrar)//' if you wish to change some data.'//e_color//linebreak

    sorryMessageStudent = red//'Students are not allowed to make changes at this time. '// &
        ' See your Adviser, or the Dean, or the '//trim(titleTheRegistrar)// &
        ' if you wish to change some data.'//e_color//linebreak

    sorryMessageSchedules = red//'The Schedule of Classes is not available for viewing at this time. '// &
        ' Try again later, or see the Dean or the '//trim(titleTheRegistrar)// &
        ' if you really need to see this schedule.'//e_color//linebreak

    ! log directory for users in the college
    do jTmp=1,NumColleges
        call make_directory( trim(dirLOG)//trim(College(jTmp)%Code)//DIRSEP )
    end do

    ! evaluation form directories
    do jTmp=1,NumTeachers
        call make_directory( trim(dirEVALUATIONS(currentTerm))//trim(Teacher(jTmp)%TeacherId)//DIRSEP )
    end do
    call log_comment('Evaluations dir is '//dirEVALUATIONS(currentTerm))

    ! retrieve evaluation assignments
    iTmp = 0
    tArray = 0
    call evaluation_duty_read(trim(dirEVALUATIONS(currentTerm))//'EVALUATION_DUTIES', iTmp, tArray)
    call log_comment(itoa(iTmp)//' in '//trim(dirEVALUATIONS(currentTerm))//'EVALUATION_DUTIES')

    ! create student directories
    call make_student_directories()

    ! CGI scriptname
    if (isMirror) then
#if defined GLNX
        CGI_PATH = trim(SERVER_PROTOCOL)//trim(SERVER_NAME)//FSLASH//'mirror'
#else
        CGI_PATH = FSLASH//'mirror'
#endif
    else
#if defined GLNX
        CGI_PATH = trim(SERVER_PROTOCOL)//trim(SERVER_NAME)//FSLASH//itoa(currentYear)
#else
        CGI_PATH = FSLASH//itoa(currentYear)
#endif
    end if

    ! user sending the initial login page
    USERNAME = PROGNAME

    ! loop until killed/fnSTOP
    isServer = .true.
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
            call html_comment('File closed due to date change from '//previousDate//' to '//currentDate)

            call html_comment('New log directory is '//trim(dirLOG)//currentDate)
            call html_comment('New request log is '//trim(dirLOG)//currentDate//DIRSEP//startDateTime//'-request.log')
            call html_comment('New scratch file is '//trim(dirLOG)//currentDate//DIRSEP//startDateTime//'-scratch.html')

            call flush(unitHTML)
            close(unitHTML)

            call initialize_date_change()

            ! remember new date
            previousDate = currentDate

            ! release locked out users
            do iTmp=1,NumStudents+NumAdditionalStudents
                if (Student(iTmp)%OnlineStatus<0) Student(iTmp)%OnlineStatus = 0
            end do
            do iTmp=1,NumTeachers+NumAdditionalTeachers
                if (Teacher(iTmp)%OnlineStatus(1)<0) Teacher(iTmp)%OnlineStatus(:) = 0
            end do

        end if

        ! tell the webserver to expect text/html
        iTmp = FCGI_puts ('Content-type: text/html'//CRLF//NUL)

        ! rewind the response file
        rewind (unitHTML)

        ! Retrieve DOCUMENT_URI and QUERY_STRING/CONTENT
        call FCGI_getquery(unitHTML)
        fileIP = trim(dirLOG)//currentDate//DIRSEP//REMOTE_ADDR

        ! deobfuscate query
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
                QUERY_STRING = '(invalid - incorrect start)'
            end if
        else ! not obfuscated
            QUERY_STRING = '(invalid - not from here)'
        end if
#if defined DBcgi
        call html_comment('CONTENT='//trim(QUERY_STRING))
#endif

        ! make copy of QUERY_STRING; remove passwords from copy, if any
        cipher = QUERY_STRING
        call cgi_get_named_string(cipher, 'C', tTeacher, iTmp)
        call cgi_get_named_string(cipher, 'P', tTeacher, iTmp)
        call cgi_get_named_string(cipher, 'R', tTeacher, iTmp)

        ! Establish USERNAME/ROLE and REQUEST
        UserRequestCheckMessage = SPACE
        fileUSER = SPACE
        call get_user_request()

        ! write request to log files
        if (REQUEST==fnLogin) cipher = trim(cipher)//' : '//HTTP_USER_AGENT
        if (REQUEST==fnHomePage) cipher = trim(cipher)//' : '//UserRequestCheckMessage
        cipher = currentDate//DASH//currentTime//' : '//trim(USERNAME)//'@'//trim(REMOTE_ADDR)//' : '// &
            trim(fnDescription(REQUEST))//' : '//cipher
        call log_request(unitREQ, trim(fileREQ), SPACE, trim(cipher) )
        call log_request(unitIP, trim(fileIP), SPACE, trim(cipher) )
        if (len_trim(fileUSER)>0) &
            call log_request(unitUSER, trim(fileUSER), SPACE, trim(cipher) )

        !call html_comment('Logs: IP='//fileIP, ', User='//fileUSER, ', Requests='//fileREQ)

        ! compose response
        call server_respond(unitHTML) ! might have exited via terminate_on_IOerr()

        ! send response to server
        call FCGI_putfile(unitHTML)

        if (REQUEST==fnStop) exit

        ! automatically log out users idle who have been idle for maxIdleTime seconds
        do iTmp=1,NumStudents+NumAdditionalStudents
            if (Student(iTmp)%OnlineStatus<=0) cycle
            if ( (tick-Student(iTmp)%OnlineStatus)/count_rate>=maxIdleTime) then
                Student(iTmp)%OnlineStatus = 0
                !jTmp = year_prefix(Student(iTmp))
                fileUSER = trim(dirLOG)//trim(basefile_student(iTmp))//'.log'
                cipher = currentDate//DASH//currentTime//' : '//trim(Student(iTmp)%StdNo)// &
                    '@nowhere : Logged out automatically'
                call log_request(unitUSER, trim(fileUSER), SPACE, cipher )
                call log_request(unitREQ, trim(fileREQ), SPACE, cipher )
            end if
        end do
        do iTmp=1,NumTeachers+NumAdditionalTeachers
            if (Teacher(iTmp)%OnlineStatus(1)<=0) cycle
            if ( (tick-Teacher(iTmp)%OnlineStatus(1))/count_rate>=maxIdleTime ) then
                Teacher(iTmp)%OnlineStatus(:) = 0
                jTmp = Department(Teacher(iTmp)%DeptIdx)%CollegeIdx
                fileUSER = trim(dirLOG)//trim(College(jTmp)%Code)//DIRSEP// &
                    trim(Teacher(iTmp)%TeacherId)//DASH//currentDate//'.log'
                cipher = currentDate//DASH//currentTime//' : '//trim(Teacher(iTmp)%TeacherId)// &
                    '@nowhere : Logged out automatically'
                call log_request(unitUSER, trim(fileUSER), SPACE, cipher )
                call log_request(unitREQ, trim(fileREQ), SPACE, cipher )
            end if
        end do

        ! disable writing of diagnostic in case of write-to-disk error termination
        fileUSER = SPACE
        fileIP = SPACE

        ! auto-backup?
        if (.not. isReadOnly .and. (tick-tickLastBackup)/count_rate>=maxBackupTime) then
            call backup_write()
        end if

        ! reload data for mirror?
        if (isMirror .and. secsNextRefresh==0 ) then
            call year_data_initialize () ! re-load  basic data
            call year_data_read(pathToYear) ! tickLastRefresh is set
            call regenerate_obfuscator()
        end if

    end do ! while (FCGI_Accept() >= 0)

    call backup_write()

    ! terminate
    call terminate(-1, trim(fileEXE)//' stopped normally.')


contains

#include "SERVER-OTHER.F90"

    subroutine get_user_request()

        character (len=MAX_LEN_PASSWD_VAR) :: tPassword
        integer :: errNo
        character (len=MAX_LEN_USERNAME) :: originalUSERNAME
        integer :: originalREQUEST
        integer*8 :: ip_address
        real :: elapsedTime

        call html_comment('get_user_request()')

        originalREQUEST = 0
        originalUSERNAME = '(nobody)'
        REQUEST = 0
        USERNAME = '(nobody)'
        requestingTeacher = 0
        requestingStudent = 0

        isRegistrar = .false.
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

        ! IP
        ip_address = iptoint8(REMOTE_ADDR)

        ! Establish REQUESTed function if any, else return the login page
        call cgi_get_named_integer(QUERY_STRING, 'F', originalREQUEST, errNo)
        if (errNo==-1) then
            call html_comment('REQUEST = '//itoa(originalREQUEST)//', errNo = '//itoa(errNo))
            REQUEST = fnLoginPage
            return
        end if

        ! Get USERNAME
        if (originalREQUEST==fnSwitchUser) then
            call cgi_get_named_string(QUERY_STRING, 'N', originalUSERNAME, errNo)
            call cgi_get_named_string(QUERY_STRING, 'A1', USERNAME, errNo)
            REQUEST = fnLogin

        elseif (originalREQUEST/=fnLogin) then
            call cgi_get_named_string(QUERY_STRING, 'N', originalUSERNAME, errNo)
            USERNAME = originalUSERNAME
            REQUEST = originalREQUEST

        else
            call cgi_get_named_string(QUERY_STRING, 'U', originalUSERNAME, errNo)
            USERNAME = originalUSERNAME
            REQUEST = originalREQUEST

        end if

        ! Get index to user
        if (errNo==0) then
            requestingTeacher = index_to_teacher(USERNAME)
            if (requestingTeacher==0) requestingStudent = index_to_student(USERNAME)
        end if

        if (requestingTeacher>0) then ! in Teacher()

            DeptIdxUser = Teacher(requestingTeacher)%DeptIdx
            CollegeIdxUser = Department(DeptIdxUser)%CollegeIdx
            ROLE = Teacher(requestingTeacher)%Role
            fileUSER = trim(dirLOG)//trim(College(CollegeIdxUser)%Code)//DIRSEP// &
                trim(USERNAME)//DASH//currentDate//'.log'

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
                    isRegistrar = trim(USERNAME)==PROGNAME .or. USERNAME(1:9)=='Registrar'

            end select

            ! disallow non-editors?
            if (.not. isAllowedNonEditors .and. (isRoleGuest .or. isRoleOfficial) ) then
                REQUEST = fnHomePage
                UserRequestCheckMessage = ' Officials and guests are not allowed here at this time.'
                return
            end if

            select case (REQUEST)

                case (fnLogin)

#if defined no_password_check
                    UserRequestCheckMessage = 'Successful login of user '//trim(USERNAME)//' without a password.'
                    Teacher(requestingTeacher)%OnlineStatus(1) = tick
                    Teacher(requestingTeacher)%OnlineStatus(2) = ip_address
                    return
#endif
                    ! allow Guest account to login without checking password
                    if (trim(USERNAME)==GUEST) then ! Guest
                        UserRequestCheckMessage = &
                            ' You are logged in as Guest. Visit the '//trim(UniversityCode)//SPACE// &
                            ' Registrar for your own Username and Password.'
                        return
                    end if

                    ! duplicate login ?
                    if (Teacher(requestingTeacher)%OnlineStatus(1)>0) then
                        REQUEST = fnHomePage
                        elapsedTime = 1.0*(tick-Teacher(requestingTeacher)%OnlineStatus(1))/count_rate
                        UserRequestCheckMessage = ' Failed login - user has an ongoing session. Last activity was '// &
                            trim(ftoa(elapsedTime/60.0,2))//' minutes ago.'
                        return
                    end if

                    ! password provided ?
                    call cgi_get_named_string(QUERY_STRING, 'P', tPassword, errNo)
                    if (errNo==-1 .and. originalREQUEST==fnSwitchUser) then  ! no need for a password
                        Teacher(requestingTeacher)%OnlineStatus(1) = tick
                        Teacher(requestingTeacher)%OnlineStatus(2) = ip_address
                        UserRequestCheckMessage = 'Administrative login of '//trim(USERNAME)//' without password by '// &
                            originalUSERNAME
                        call log_teacher_record_change(requestingTeacher, UserRequestCheckMessage)
                        return
                    end if

                    ! password OK?
                    if (isPassword_of_teacher(requestingTeacher,tPassword)) then
                        Teacher(requestingTeacher)%OnlineStatus(1) = tick
                        Teacher(requestingTeacher)%OnlineStatus(2) = ip_address
                        UserRequestCheckMessage = 'Successful login from IP address '//REMOTE_ADDR

                        ! force password to be different from USERNAME
                        if (trim(Teacher(requestingTeacher)%TeacherID)==trim(tPassword)) then
                            REQUEST = fnChangeTeacherPassword
                        end if

                    else
                        REQUEST = fnHomePage
                        Teacher(requestingTeacher)%OnlineStatus(1) = Teacher(requestingTeacher)%OnlineStatus(1) - 1
                        if (Teacher(requestingTeacher)%OnlineStatus(1)<-MAX_LOGIN_ATTEMPTS) then
                            UserRequestCheckMessage = ' Failed login - too many attempts. Try again tomorrow.'
                        else
                            UserRequestCheckMessage = ' Failed login - Username/Password combination not found.'
                        end if
                    end if


                case (fnLogout)
                    Teacher(requestingTeacher)%OnlineStatus(:) = 0
                    REQUEST = fnHomePage
                    UserRequestCheckMessage = ' Successfully logged out.'

                case (fnStop)
                    UserRequestCheckMessage = ' The server program has stopped.'

                case default
                    if (trim(USERNAME)==GUEST) return ! allow Guest everytime

                    if (Teacher(requestingTeacher)%OnlineStatus(1)<=0) then
                        REQUEST = fnHomePage
                        UserRequestCheckMessage = ' Failed request - user is not logged in.'

                    elseif (Teacher(requestingTeacher)%OnlineStatus(2)/=ip_address) then
                        REQUEST = fnHomePage
                        UserRequestCheckMessage = ' Failed request - user is logged in at another IP address.'

                    else
                        Teacher(requestingTeacher)%OnlineStatus(1) = tick

                        ! force password to be different from USERNAME
                        tPassword = Teacher(requestingTeacher)%TeacherID
                        if (isPassword_of_teacher(requestingTeacher,tPassword)) then
                            REQUEST = fnChangeTeacherPassword
                        end if
                    end if

            end select


        elseif (requestingStudent>0) then ! in Student()

            ROLE = SPACE
            isRoleStudent = .true.
            targetStudent = requestingStudent
            targetCurriculum = Student(requestingStudent)%CurriculumIdx
            CurriculumIdxUser = targetCurriculum
            CollegeIdxUser = Curriculum(targetCurriculum)%CollegeIdx
            DeptIdxUser = 0
            fileUSER = trim(dirLOG)//trim(basefile_student(requestingStudent))//'.log'

            ! disallow non-editors?
            if (.not. isAllowedNonEditors ) then
                REQUEST = fnHomePage
                UserRequestCheckMessage = ' Students are not allowed here at this time.'
                return
            end if

            select case (REQUEST)

                case (fnLogin)
#if defined no_password_check
                    UserRequestCheckMessage = 'Successful login without a password.'
                    Student(requestingStudent)%OnlineStatus = tick
                    return
#endif
                    if (Student(requestingStudent)%OnlineStatus>0) then
                        elapsedTime = 1.0*(tick-Student(requestingStudent)%OnlineStatus)/count_rate
                        REQUEST = fnHomePage
                        UserRequestCheckMessage = ' Failed login - user has an ongoing session. Last activity was '// &
                            trim(ftoa(elapsedTime/60.0,2))//' minutes ago.'
                        return
                    end if

                    ! password provided ?
                    call cgi_get_named_string(QUERY_STRING, 'P', tPassword, errNo)
                    if (errNo==-1 .and. originalREQUEST==fnSwitchUser) then  ! no need for a password
                        Student(requestingStudent)%OnlineStatus = tick
                        UserRequestCheckMessage = 'Administrative login of '//trim(USERNAME)//' without password by '// &
                            originalUSERNAME
                        call log_student_record_change(requestingStudent, UserRequestCheckMessage)
                        return
                    end if

                    ! password OK?
                    if (isPassword_of_student(requestingStudent,tPassword)) then
                        Student(requestingStudent)%OnlineStatus = tick
                        UserRequestCheckMessage = 'Successful login from IP address '//REMOTE_ADDR

                    else
                        REQUEST = fnHomePage
                        Student(requestingStudent)%OnlineStatus = Student(requestingStudent)%OnlineStatus - 1
                        if (Student(requestingStudent)%OnlineStatus<-MAX_LOGIN_ATTEMPTS) then
                            UserRequestCheckMessage = ' Failed login - too many attempts. Try again tomorrow.'
                        else
                            UserRequestCheckMessage = ' Failed login - Username/Password combination not found.'
                        end if
                    end if

                case (fnLogout)
                    Student(requestingStudent)%OnlineStatus = 0
                    REQUEST = fnHomePage
                    UserRequestCheckMessage = ' Successfully logged out.'

                case default
                    if (Student(requestingStudent)%OnlineStatus<=0) then
                        REQUEST = fnHomePage
                        UserRequestCheckMessage = ' User is not logged in.'
                    else ! force students to supply info
                        Student(requestingStudent)%OnlineStatus = tick
                        tPassword = Student(requestingStudent)%StdNo
                        if (isPassword_of_student(requestingStudent,tPassword)) then
                            if (REQUEST/=fnChangeStudentPassword) then
                                REQUEST = fnChangeStudentPassword
                                QUERY_STRING = 'T='//currentTime//'&F='//trim(itoa(REQUEST))//'&'
                            ! else request is from change password page; allow request
                            end if
                        else if (REQUEST/=fnStudentEdit .and. &
                                 REQUEST/=fnSelectTongue .and. &
                                 REQUEST/=fnSelectPSGC .and. &
                                 index(Student(requestingStudent)%Name,'(*)')>0 )  then
!                                    (index(Student(requestingStudent)%Name,'(*)')>0 .or. &
!                                     Curriculum(Student(requestingStudent)%CurriculumIdx)%Code(1:6)=='CBEAGC') )  then
                            REQUEST = fnStudentEdit
                            QUERY_STRING = 'T='//currentTime//'&F='//trim(itoa(REQUEST))//'&A1='//trim(USERNAME)//'&'
                        end if
                    end if
            end select


        else ! unknown user?

            REQUEST = fnHomePage
            UserRequestCheckMessage = ' Unknown user.'

        end if

    end subroutine get_user_request


    subroutine server_respond (device)
        integer, intent (in) :: device

        integer :: i

        call html_comment('server_respond()')

        ! Establish TERM, target directory if files are to be modified
        call cgi_get_named_integer(QUERY_STRING, 'A9', targetTerm, i)
        if (i/=0 .or. targetTerm<=0) targetTerm = currentTerm ! default
        pathToTerm = trim(dirDATA)//trim(itoa(currentYear))//DIRSEP//trim(txtSemester(targetTerm))//DIRSEP

        if (REQUEST==fnSwitchTerm) then
            call cgi_get_named_integer(QUERY_STRING, 'A1', targetTerm, i)
            ! assume login
            REQUEST = fnLogin
            pathToTerm = trim(dirDATA)//trim(itoa(currentYear))//DIRSEP//trim(txtSemester(targetTerm))//DIRSEP
#if defined DBcgi
            call html_comment('Switched to term: FN='//fnDescription(REQUEST), 'PATH='//pathToTerm)
        else
            call html_comment('FN='//fnDescription(REQUEST), 'PATH='//pathToTerm)
#endif
        end if

        ! trap official role attempting to change data
        if (isRoleOfficial) call check_permission()

        select case (REQUEST)

            case (fnHomePage, fnLogout, fnStop)
                call html_home_page(device, UserRequestCheckMessage)

            case (fnLoginPage)
                call html_login_page(device)

            case (fnReload)
                ! re-load basic data
                call year_data_initialize ()
                call year_data_read(pathToYear) ! tickLastRefresh is set
                call regenerate_obfuscator()
                call html_college_links(device, CollegeIdxUser, mesg='Reloaded data.')

            case (fnLogin)
                if (isRoleBenefactor) then
                    REQUEST = fnListBeneficiaries
                    call links_to_students(device, REQUEST, currentTerm, USERNAME)

                else if (isRoleStudent) then

                    if (isPassword_of_student(requestingStudent,USERNAME) ) then
                        REQUEST = fnChangeStudentPassword
                        QUERY_STRING = 'T='//currentTime//'&F='//trim(itoa(REQUEST))//'&'
                        call change_current_student_password(device)
                    else
                        REQUEST = fnStudentEdit
                        QUERY_STRING = 'T='//currentTime//'&F='//trim(itoa(REQUEST))//'&A1='//trim(USERNAME)//'&'
                        call student_edit_info(device)
                    end if

                else
                    call html_college_links(device, CollegeIdxUser, mesg=UserRequestCheckMessage)
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
                call reset_all_passwords(device)

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
                call room_list_all (device, REQUEST)

            ! teacher info
            case (fnTeachersByDept, fnOnlineTeachers, fnFindTeacher)
                call teacher_list_all (device, REQUEST)

            ! subjects with open sections
            case (fnScheduleOfClasses)
                call section_list_all (device)

            ! schedule of classes
            case (fnScheduleByArea, fnTBARooms, fnTBATeachers, fnTeacherClasses, fnUnpostedGrades, fnGradesheetsToReceive)
                call section_list_classes (device, targetTerm, REQUEST)

            case (fnScheduleCopyLastYear)
                call section_copy_classes_last_year (device, targetTerm)

            ! schedule conflicts
            case (fnRoomConflicts)
                call room_conflicts (device, targetTerm)

            case (fnRoomSchedule)
                call room_schedule(device, targetTerm)

            case (fnTeacherConflicts)
                call teacher_conflicts (device, targetTerm)

            case (fnPrintableWorkload)
                call teacher_schedule_printable(device, targetTerm)

            ! blocks
            case (fnBlockList)
                call block_list_all(device, targetTerm)

            case (fnBlockSchedule)
                call block_schedule(device, targetTerm)

            case (fnBlockConflicts)
                call block_conflicts(device, targetTerm)

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
                call teacher_schedule (device, targetTerm)

            case (fnEvaluationForm)
                call evaluation_form (device, targetTerm)

            case (fnEvaluationRatings)
                call evaluation_ratings (device, targetTerm)

            case (fnEvaluationDuties)
                call evaluation_duties (device, targetTerm)

            case (fnScheduleOfferSubject)
                call section_offer_subject (device, targetTerm)

            case (fnScheduleDelete)
                call section_delete(device, targetTerm)

            case (fnScheduleAddLab)
                call section_add_laboratory(device, targetTerm)

            case (fnScheduleEdit, fnScheduleValidate)
                call section_validate_inputs (device, targetTerm)

            case (fnBlockDeleteNotClasses, fnBlockEditName, fnBlockCopy, fnBlockDeleteAlsoClasses, &
                    fnBlockEditSection, fnBlockEditSubject)
                call edit_block(device, targetTerm, REQUEST)

            case (fnBlockNewSelect)
                call block_select_curriculum_year(device, targetTerm )

            case (fnBlockNewAdd)
                call block_add(device, targetTerm)

            ! students
            case (fnStudentEdit, fnGenerateStudentPassword, fnSelectPSGC, fnSelectTongue)
                call student_edit_info(device)

            case (fnStudentAdd)
                call student_add(device)

            case (fnStudentsByCurriculum, fnOnlineStudents, fnFindStudent, fnListBeneficiaries)
                call links_to_students(device, REQUEST)

            case (fnAdvisersByTeacher )
                call student_advisers_by_teacher(device)

            case (fnAdvisersByCurriculum )
                call student_advisers_by_curriculum(device)

            case (fnStudentsWithConflicts, fnAdvisingCode, fnAdvisingStatusStudents, fnEnlistmentStatusStudents)
                call links_to_students(device, REQUEST, targetTerm)

            case (fnStudentGrades)
                call student_edit_grades(device, targetTerm)

            case (fnFees)
                call student_fees(device)

            case (fnEditCheckList)
                call checklist_edit(device, targetTerm)

            case (fnAdvisingEnlistmentStatus, &
                    fnConfirmResetDemandForSubjects, fnConfirmUpdateDemandForSubjects, fnConfirmTimetabling)
                call confirm_advising_enlistment_action(device, targetTerm, REQUEST)

            case (fnResetDemandForSubjects, fnUpdateDemandForSubjects, fnTimetabling)
                call execute_advising_enlistment_action(device, targetTerm, REQUEST, UserRequestCheckMessage)

            ! summary of enlistment
            case (fnEnlistmentSummary, fnBottleneck, fnExtraSlots)
                call enlistment_summarize(device, targetTerm, REQUEST)

            case (fnUnderloadSummary)
                call underload_summarize(device, targetTerm )

            case (fnUnderloadedStudents)
                call underloaded_students(device, targetTerm )

            case (fnStudentsNotEnrolled)
                call enlistment_no_classes(device, i)
                if (i/=0) then ! remove studentsc
                    call student_purge_removed(device)
                end if

            case (fnNotAccommodated)
                call enlistment_not_accommodated(device, targetTerm )

            case (fnGradesheet)
                call enlistment_grades(device, targetTerm)

            case (fnPrintableGradesheet)
                call printable_gradesheet(device, targetTerm)

            case (fnClassList)
                call class_list(device, targetTerm)

            case (fnPrintableClassList)
                call printable_class_list(device, targetTerm)

            case (fnChangeMatriculation, fnStudentClasses)
                call enlistment_edit(device, targetTerm)

            case (fnStudentForceEnlist)
                call enlistment_forced(device, targetTerm)

            case (fnFindBlock)
                call enlistment_find_block(device, targetTerm)

            case (fnPrintableSchedule)
                call enlistment_printable(device, targetTerm)

            case (fnSummaryWAG)
                call average_grade_summary_list(device )

            case (fnTogglePermission)
                call toggle_permission(device, targetTerm )

            case (fnSearchCategory)
                call search_category(device, targetTerm)

            case (fnUploadPicture)
                call file_uploadIDpicture(device)

            case (fnUploadCSV)
                call file_uploadCSV(device)

            case (fnDistributionPSGC)
                call distribution_by_PSGC(device)

            case (fnGenderDistribution)
                call distribution_by_gender(device, targetTerm)

            case default
                targetCollege = CollegeIdxUser
                targetDepartment = DeptIdxUser
                call html_write_header(device, SPACE, 'Function not available')
                write(device,AFORMAT) &
                    linebreak//'The functionality "'//trim(fnDescription(REQUEST))// &
                    '" is not enabled in this version of '//PROGNAME//DOT//linebreak//linebreak//horizontal
                REQUEST = 0 ! do not suppress footer

        end select

        call html_write_footer(device)

    end subroutine server_respond


end program SERVER
