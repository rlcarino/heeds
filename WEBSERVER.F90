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


module WEBSERVER

    use EditUNIVERSITY
    use EditSUBJECTS
    use EditROOMS
    use EditTEACHERS
    use EditSECTIONS
    use EditBLOCKS
    use EditCURRICULA
    use EditPREDICTIONS
    use EditENLISTMENT
    use DEMAND
    use REPORTS

    implicit none

contains


    subroutine server_start()

        integer :: iTmp, kStart
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        character(len=MAX_LEN_FILE_PATH) :: logTeacher
        logical :: logExists
        real :: harvest    ! random number

        ! initialize QUERY_STRING encryption key
        do iTmp=1,MAX_LEN_QUERY_STRING
            call random_number(harvest)
            kStart = 1 + int(255*harvest)
            queryEncryptionKey(iTmp:iTmp) = achar(kStart)
        end do

        ! make "Stop!" page
        call hostnm(QUERY_put, iTmp)
        if (iTmp/=0) QUERY_put = 'localhost'
#if defined GLNX
#else
        QUERY_put = trim(QUERY_put)//':82'
#endif
        CGI_PATH = 'http://'//trim(QUERY_put)//FSLASH//ACTION
        call html_login('Stop-'//trim(UniversityCode)//DASH//trim(ACTION)//'.html', &
            trim(make_href(fnStop, 'Stop', post=nbsp//ACTION) ) )

        ! reset CGI_PATH
        CGI_PATH = FSLASH//ACTION

        ! notes
        call file_log_message(trim(fileExecutable)//' started '//ACTION)
        if (noWrites) then ! training mode
            call file_log_message(trim(fileExecutable)//' is in training mode. '// &
                'Any made changes will be lost after the program exits.')
        end if

        ! loop until killed/fnSTOP
        do while (FCGI_Accept() >= 0)
        
            ! timestamp of request
            call date_and_time (date=currentDate, time=currentTime)

            ! tell the webserver to expect text/html
            iTmp = FCGI_puts ('Content-type: text/html'//CRLF//NUL)

            ! rewind the response file
            rewind (unitHTML)

            ! Retrieve DOCUMENT_URI and QUERY_STRING/CONTENT
            call FCGI_getquery(unitHTML)

            ! encrypted query ?
            call cgi_get_named_string(QUERY_STRING, 'q', cipher, iTmp)
            if (iTmp==0) then
                iTmp = len_trim(cipher)
                kStart = atoi( cipher(iTmp-3:iTmp) )
                if (kStart>MAX_LEN_QUERY_STRING/2 .and. kStart<MAX_LEN_QUERY_STRING) then
                    cipher(iTmp-3:iTmp) = SPACE
                    call decrypt(queryEncryptionKey(:kStart), cipher)
                    QUERY_STRING = trim(cipher)//'&'//QUERY_STRING
                else
                    QUERY_STRING = '(invalid)'
                end if
            end if

            ! make copy of QUERY_STRING
            cipher = QUERY_STRING

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
            targetLogin = 0
            targetStudent = 0

            ! Establish USERNAME/ROLE and REQUEST
            loginCheckMessage = SPACE
            call get_user_request()

            ! override targetTerm if  is ENLISTMENT
            if (currentTerm==nextTerm) targetTerm = currentTerm

            ! open user's log file, create if necessary
            call blank_to_underscore(USERNAME, tTeacher)
            logTeacher = trim(dirBACKUP)//trim(tTeacher)//'.log'
            inquire(file=trim(logTeacher), exist=logExists)
            if (.not. logExists) then
                open(unit=unitUSER, file=trim(logTeacher), status='new')
            else
                open(unit=unitUSER, file=trim(logTeacher), status='old', position='append')
            end if

            ! append query to user log file
            write(unitUSER,AFORMAT) SPACE, &
                REMOTE_ADDR//' : '//currentDate//DASH//currentTime//' : '// &
                fnDescription(REQUEST)
            if (REQUEST>4) then ! no passwords
                write(unitUSER,AFORMAT) trim(cipher)
                write(unitREQ, AFORMAT) trim(cipher)
            end if

            call html_comment(fnDescription(REQUEST))

            ! compose response
            call server_respond(unitHTML)

            ! send response to server
            call FCGI_putfile(unitHTML)

            ! close user log file
            close(unitUSER)

            ! stop?
            if (REQUEST==fnStop) exit

        end do ! while (FCGI_Accept() >= 0)

        ! remove "Stop" link
        call unlink('Stop-'//trim(UniversityCode)//DASH//trim(ACTION)//'.html')

        ! terminate
        call terminate(trim(fileExecutable)//' completed '//ACTION)

    end subroutine server_start


    subroutine server_respond (device)
        integer, intent (in) :: device

        integer :: tYear, tTerm

        call html_comment('server_respond()')

        ! target directory if files are to be modified
        if (targetTerm>0) then
            if (targetTerm<currentTerm) then ! next school year
                call qualify_term (3+targetTerm, tYear, tTerm, termDescription)
            else
                call qualify_term (targetTerm, tYear, tTerm, termDescription)
            end if
            termDescription = ' for '//termDescription
            pathToTerm = trim(itoa(tYear))//DIRSEP//trim(txtSemester(tTerm))//DIRSEP
        else
            termDescription = SPACE
            pathToTerm = trim(itoa(currentYear))//DIRSEP
        end if

        ! suspended ?
        if (isSuspended .and. .not. isRoleAdmin) REQUEST = 0

        select case (REQUEST)

            case (0)
                Teacher(targetLogin)%Status = 0
                call html_landing_page(device, &
                    '<hr>The '//trim(UniversityCode)//SPACE//trim(REGISTRAR)//' has temporarily suspended '// &
                    PROGNAME//FSLASH//trim(Action)//'. Please try again later.<hr>')

            case (fnStop)
                if (isDirtySTUDENTS) then
                    call xml_write_students(pathToYear, 0)
                end if
                if (isDirtyPreenlisted) then
                    pathToTerm = trim(pathToYear)//trim(txtSemester(currentTerm))//DIRSEP
                    call xml_write_pre_enlistment(pathToTerm, 'ENLISTMENT', Preenlisted, Section(currentTerm,0:))
                endif

                call html_landing_page(device, 'The program will stop.')

            case (fnLogin)
                Teacher(targetLogin)%Status = 1
                call html_college_links(device, CollegeIdxUser, mesg=loginCheckMessage)

            case (fnChangePassword)
                Teacher(targetLogin)%Status = 1
                call change_current_password(device)

            case (fnGeneratePassword)
                call generate_password(device)

            case (fnLogout)
                Teacher(targetLogin)%Status = 0
                call html_landing_page(device, SPACE)

            case (fnSuspendProgram)
                if (isRoleAdmin) then
                    if (isDirtySTUDENTS) then
                        call xml_write_students(pathToYear, 0)
                    end if
                    isSuspended = .not. isSuspended
                    call html_college_links(device, CollegeIdxUser, mesg='Toggled "Suspend" mode')
                else
                    REQUEST = fnLogout
                    Teacher(targetLogin)%Status = 0
                    call html_landing_page(device, SPACE)
                end if

            case (fnToggleTrainingMode)
                noWrites = .not. noWrites
                call html_college_links(device, CollegeIdxUser, mesg='Toggled "Training" mode')

            case (fnEditSignatories)
                call edit_signatories(device)

            case (fnDownloadXML)
                call download_xml(device)

            ! college info
            case (fnCollegeLinks)
                call html_college_links(device)


            ! subject info
            case (fnSubjectList)
                call subject_list_all (device)

            case (fnEditSubject)
                call subject_edit (device)


            ! curriculum info
            case (fnCurriculumList, fnActivateCurriculum, fnDeactivateCurriculum)
                call curriculum_list_all(device, REQUEST)

            case (fnCurriculum)
                call curriculum_display(device)

            case (fnEditCurriculum)
                call curriculum_edit (device)


            ! room info
            case (fnRoomList)
                call room_list_all (device)

            case (fnEditRoom)
                call room_edit (device)


            ! teacher info
            case (fnTeachersByDept, fnTeachersByName)
                call teacher_list_all (device, REQUEST)

            case (fnEditTeacher)
                call teacher_edit (device)


            ! blocks
            case (fnBlockList)
                call block_list_all(device, NumBlocks(targetTerm), Block(targetTerm,0:))

            case (fnBlockSchedule, fnBlockDeleteName, fnBlockEditName, fnBlockCopy, fnBlockDeleteAll, &
                fnBlockEditSection, fnBlockEditSubject)
                call block_show_schedule(device, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:), REQUEST)

            case (fnBlockNewSelect)
                call block_select_curriculum_year(device, NumSections(targetTerm), Section(targetTerm,0:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:) )

            case (fnBlockNewAdd)
                call block_add(device, targetTerm, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), NumBlocks(targetTerm), Block(targetTerm,0:) )


            ! schedule of classes
            case (fnScheduleOfClasses, fnScheduleByArea, fnTBARooms, fnTBATeachers, fnTeacherClasses)
                call section_list_all (device, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:), REQUEST)

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

            case (fnScheduleEdit)
                call section_edit(device, NumSections(targetTerm), Section(targetTerm,0:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:) )

            case (fnScheduleValidate)
                call section_validate_inputs (device, NumSections(targetTerm), Section(targetTerm,0:), &
                    Offering(targetTerm,MAX_ALL_DUMMY_SUBJECTS:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:) )

            ! schedule conflicts
            case (fnRoomConflicts)
                call room_conflicts (device, NumSections(targetTerm), Section(targetTerm,0:))

            case (fnRoomSchedule)
                call room_schedule(device, NumSections(targetTerm), Section(targetTerm,0:) )

            case (fnTeacherConflicts)
                call teacher_conflicts (device, NumSections(targetTerm), Section(targetTerm,0:))

            case (fnTeacherEditSchedule)
                call teacher_schedule(device, NumSections(targetTerm), Section(targetTerm,0:) )

            case (fnPrintableWorkload)
                call teacher_schedule_printable(device, NumSections(targetTerm), Section(targetTerm,0:), &
                    NumBlocks(targetTerm), Block(targetTerm,0:) )

!            ! students
!            case (fnStudentAddPrompt)
!                call student_prompt_add(device)
!
!            case (fnStudentAdd)
!                call student_add(device)

            case (fnStudentsDistribution)
                call student_distribution(device)

            case (fnStudentsByCurriculum,fnStudentsByname,fnStudentsByYear,fnStudentsByProgram)
                call links_to_students(device, REQUEST)

            case (fnStudentPerformance)
                call student_performance(device)

            case (fnEditCheckList)
                 call checklist_edit(device, .false., &
                    Section(nextTerm,0:), Offering(nextTerm,MAX_ALL_DUMMY_SUBJECTS:), &
                    trim(pathToNextYear)//trim(txtSemester(nextTerm))//DIRSEP)

            ! cases for next semester's predicted demand for subjects
            case (fnDemandFreshmen,fnUpdateDemandFreshmen)
                call demand_by_new_freshmen(device, Offering(nextTerm,MAX_ALL_DUMMY_SUBJECTS:))

            case (fnDemandForSubjects)
                call demand_for_subjects(device, NumSections(nextTerm), Section(nextTerm,0:), &
                    Offering(nextTerm,MAX_ALL_DUMMY_SUBJECTS:))

            case (fnPotentialStudents)
                call list_potential_students(device)


            ! cases for current semester enlistment
            case (fnEnlistmentSummary, fnBottleneck, fnExtraSlots, fnUnderloadSummary, fnUnderloadedStudents)
                call enlistment_summarize(device, NumSections(currentTerm), Section(currentTerm,0:), &
                    Offering(currentTerm,MAX_ALL_DUMMY_SUBJECTS:), Preenlisted, REQUEST)

            case (fnNotAccommodated)
                call enlistment_not_accommodated(device, Preenlisted)

            case (fnGradeSheet)
                call enlistment_grades(device, NumSections(currentTerm), Section(currentTerm,0:))

            case (fnClassList)
                call class_list(device, NumSections(currentTerm), Section(currentTerm,0:))

            case (fnChangeMatriculation)
                call enlistment_edit(device, NumSections(currentTerm), Section(currentTerm,0:), &
                    NumBlocks(currentTerm), Block(currentTerm,0:) )

            case (fnFindBlock)
                call enlistment_find_block(device, NumSections(currentTerm), Section(currentTerm,0:), &
                    NumBlocks(currentTerm), Block(currentTerm,0:) )

            case (fnPrintableSchedule)
                call enlistment_printable(device, NumSections(currentTerm), Section(currentTerm,0:))

            case default
                targetCollege = CollegeIdxUser
                targetDepartment = DeptIdxUser
                termDescription = SPACE
                call html_write_header(device, SPACE, &
                    '<hr>The functionality "'//trim(fnDescription(REQUEST))// &
                    '" is not activated in '//PROGNAME//VERSION)

        end select

        call html_write_footer(device)

    end subroutine server_respond


    subroutine terminate(msg)

        character(len=*), intent (in) :: msg

        call file_log_message(msg, 'Ends '//currentDate//DASH//currentTime )

        close(unitREQ)
        close(unitHTML)
        close(unitLOG)

        stop
    end subroutine terminate


    subroutine student_prompt_add(device)
        integer, intent (in) :: device
        integer :: ldx

        call html_write_header(device, 'Add a student')
        call make_form_start(device, fnStudentAdd)
        write(device,AFORMAT) &
            '<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
            begintr//begintd//'Student Number:'//endtd//begintd// &  ! student number
            '<input name="StdNo" value="StdNo">'//endtd//endtr, & ! name
            begintr//begintd//'Lastname Firstname MI:'//endtd//begintd// &
            '<input name="Name" size="40" value="Lastname Firstname MI">'//endtd//endtr, & ! gender
            begintr//begintd//'Gender:'//endtd//begintd//'<select name="Gender">', &
            '<option value=""> (select) <option value="F"> Female <option value="M"> Male', &
            '</select>'//endtd//endtr, & ! country index
            begintr//begintd//'Country:'//endtd//begintd//'<select name="CountryIdx">', &
            '<option value="1"> Philippines <option value="0"> Other ', &
            '</select>'//endtd//endtr, & ! curriculum
            begintr//begintd//'Curriculum:'//endtd//begintd//'<select name="CurriculumIdx">', &
            '<option value=""> (select curriculum)'
        do ldx=1,NumCurricula
            if (.not. Curriculum(ldx)%Active) cycle
            write(device,AFORMAT) '<option value="'//trim(Curriculum(ldx)%Code)//'"> '// &
                trim(Curriculum(ldx)%Code)//' - '//trim(Curriculum(ldx)%Title)
        end do
        write(device,AFORMAT) '</select>'//endtd//endtr
        ! classification
        write(device,AFORMAT) &
            begintr//begintd//'Year level:'//endtd//begintd//'<select name="Classification">'
        do ldx=1,8
            write(device,AFORMAT) '<option value="'//trim(itoa(ldx))//'"> '// &
                trim(txtYear(ldx+9))
        end do
        write(device,AFORMAT) '</select>'//endtd//endtr
        ! Add button
        write(device,AFORMAT) &
            '</table><br><input type="submit" name="action" value="Add student">', &
            '</form><hr>'

    end subroutine student_prompt_add


    subroutine student_add(device)
        integer, intent (in) :: device

        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character (len=MAX_LEN_TEXT_YEAR) :: tYear
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        type (TYPE_STUDENT) :: wrk
        integer :: ierr

        ! student exists?
        call cgi_get_named_string(QUERY_STRING, 'StdNo', tStdNo, ierr)
        targetStudent = index_to_student(tStdNo)

        if (ierr/=0 .or. tStdNo==SPACE) then
            call html_college_links(device, CollegeIdxUser, 'Add student: student number not specified?')
            return
        else if (targetStudent/=0) then
            call html_college_links(device, CollegeIdxUser, 'Add student: student "'//tStdNo//'" already on record.')
            return
        end if
        wrk%StdNo = tStdNo

        call cgi_get_named_string(QUERY_STRING, 'Name', wrk%Name, ierr)
        call cgi_get_named_string(QUERY_STRING, 'Gender', wrk%Gender, ierr)

        call cgi_get_named_string(QUERY_STRING, 'CurriculumIdx', tCurriculum, ierr)
        wrk%CurriculumIdx = index_to_curriculum(tCurriculum)
        call cgi_get_named_string(QUERY_STRING, 'Classification', tYear, ierr)
        wrk%Classification = index_to_year(tYear)
        call cgi_get_named_integer(QUERY_STRING, 'CountryIdx', wrk%CountryIdx, ierr)

        targetStudent = NumStudents
        do while (tStdNo<Student(targetStudent)%StdNo)
            Student(targetStudent+1) = Student(targetStudent)
            targetStudent = targetStudent-1
        end do
        Student(targetStudent+1) = wrk
        NumStudents = NumStudents + 1
        isDirtySTUDENTS = .false.

        call sort_alphabetical_students()

        targetStudent = index_to_student(tStdNo)
        call html_college_links(device, CollegeIdxUser, 'Added '//trim(text_student_info(targetStudent)))

    end subroutine student_add


    subroutine get_user_request()

        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character (len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character (len=MAX_LEN_PASSWD_VAR) :: tPassword
        integer :: ierr

        isRoleAdmin = .false.
        isRoleSRE = .false.
        isRoleChair = .false.
        isRoleStudent = .false.
        isRoleGuest = .false.

        DeptIdxUser = 0
        CollegeIdxUser = 0
        CurriculumIdxUser = 0

        ! Get USERNAME
        call cgi_get_named_string(QUERY_STRING, 'N', USERNAME, ierr)
        targetLogin = index_to_teacher(USERNAME)

        if (targetLogin>0) then
            ROLE = Teacher(targetLogin)%Role
        else ! not in Teachers(); assume Guest
            USERNAME = GUEST
            ROLE = GUEST
        end if

        if (trim(ROLE)==GUEST) then ! Guest
            isRoleGuest = .true.
            if (trim(USERNAME)/=GUEST) then
                DeptIdxUser = Teacher(targetLogin)%DeptIdx
                CollegeIdxUser = Department(DeptIdxUser)%CollegeIdx
            end if
        elseif (trim(ROLE)==REGISTRAR) then ! Administrator
            isRoleAdmin = .true.
            DeptIdxUser = NumDepartments
            CollegeIdxUser = Department(DeptIdxUser)%CollegeIdx
        else ! Chair or Adviser ?
            tDepartment = ROLE
            targetDepartment = index_to_dept(tDepartment)
            if (targetDepartment/=0) then ! Chair
                isRoleChair = .true.
                DeptIdxUser = targetDepartment
                CollegeIdxUser = Department(targetDepartment)%CollegeIdx
            else
                tCurriculum = ROLE
                targetCurriculum = abs(index_to_curriculum(tCurriculum))
                if (targetCurriculum/=0) then ! adviser
                    isRoleSRE = .true.
                    ROLE = CurrProgCode(targetCurriculum)
                    CurriculumIdxUser = targetCurriculum
                    CollegeIdxUser = Curriculum(targetCurriculum)%CollegeIdx
                    DeptIdxUser = 0
                else
                    isRoleGuest = .true.
                end if
            end if
        end if

        ! Establish REQUESTed function if any, else return the landing page
        call cgi_get_named_integer(QUERY_STRING, 'F', REQUEST, ierr)
        if (ierr==-1) then
            REQUEST = fnLogout
            return
        end if

        ! Establish TERM if required
        if (REQUEST>=fnScheduleOfClasses) then
            call cgi_get_named_integer(QUERY_STRING, 'A9', targetTerm, ierr)
        end if

        if (REQUEST/=fnLogin) then ! not logging in
            ! previously logged out?  force user to login
            if (Teacher(targetLogin)%Status==0 .and. .not. isRoleGuest) REQUEST = fnLogout
            return
        end if
        ! request is login; validate POSTed data

        ! Always allow Guest account
        if (trim(USERNAME)==GUEST) then ! Guest
            loginCheckMessage = &
                ' You are logged in as Guest. Contact the '//REGISTRAR//' to obtain an account.'
            return
        end if

        ! password provided ?
        call cgi_get_named_string(QUERY_STRING, 'P', tPassword, ierr)
        if (ierr==-1) then ! no password
            REQUEST = fnLogout
            loginCheckMessage = 'Username and/or Password not valid.'
        else ! password provided
            if (is_password(targetLogin,tPassword) ) then ! password matched
                loginCheckMessage = 'Successful login for '//USERNAME
            else ! return login page
                REQUEST = fnLogout
                loginCheckMessage = 'Username and/or Password not valid.'
            end if
        end if

    end subroutine get_user_request


    subroutine download_xml(device)
        integer, intent(in) :: device

        character (len=MAX_LEN_FILE_PATH) :: XMLfile, fileName
        character (len=MAX_LEN_XML_LINE) :: line
        integer :: ierr, eof
        logical :: fileExists

        call cgi_get_named_string(QUERY_STRING, 'A1', XMLfile, ierr)

        fileName = trim(dirDATA)//trim(pathToYear)//XMLfile
        inquire(file=fileName, exist=fileExists)
        if (.not. fileExists) then
            fileName = trim(dirDATA)//trim(pathToTerm)//XMLfile
            inquire(file=fileName, exist=fileExists)
        end if
        if (fileExists) then
            write(device,AFORMAT) '<comment>', &
                'Save as '//trim(fileName), '</comment>'
            open(unit=unitXML, file=fileName, form='formatted', status='old')
            do
                read(unitXML, AFORMAT, iostat=eof) line
                if (eof<0) exit
                write(device,AFORMAT) trim(line)
            end do
            close(unitXML)
        else
            targetCollege = CollegeIdxUser
            targetDepartment = DeptIdxUser
            termDescription = SPACE
            call html_write_header(device, SPACE, '<hr>File not found "'//trim(fileName))
            REQUEST = 0
        end if

    end subroutine download_xml


    subroutine execute_log()

        integer :: unitREPLAY=990, errNo, eof
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        character(len=MAX_LEN_FILE_PATH) :: logTeacher, fileName
        logical :: logExists

        ! disallow xml_write_*() for now
        !noWrites = .true.

        REMOTE_ADDR = SPACE
        DOCUMENT_URI = SPACE

        ! reset CGI_PATH
        CGI_PATH = FSLASH//trim(ACTION)//FSLASH//UniversityCode

        ! notes
        call file_log_message(trim(fileExecutable)//' started '//ACTION//' execute_log()')
        if (noWrites) then ! training mode
            call file_log_message(trim(fileExecutable)//' is in training mode. '// &
                'Any made changes will be lost after the program exits.')
        end if

        fileName = 'requests.log'
        open(unit=unitREPLAY, file=fileName, form='formatted', status='old', iostat=errNo)
        if (errNo/=0) return

        ! loop until EOF/fnSTOP
        do

            read(unitREPLAY, AFORMAT, iostat=eof) QUERY_STRING
            if (eof<0) exit
            if (QUERY_STRING(1:1)=='#') cycle

            ! timestamp of request
            call date_and_time (date=currentDate, time=currentTime)

            ! rewind the response file
            rewind (unitHTML)

            ! make copy of QUERY_STRING
            cipher = QUERY_STRING

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
            targetLogin = 0
            targetStudent = 0

            ! force everyone to be logged in
            do eof=1,NumTeachers
                Teacher(eof)%Status = 1
            end do

            ! Establish USERNAME/ROLE and REQUEST
            loginCheckMessage = SPACE
            call get_user_request()

            ! override targetTerm if  is ENLISTMENT
            if (currentTerm==nextTerm) targetTerm = currentTerm

            ! open user's log file, create if necessary
            call blank_to_underscore(USERNAME, tTeacher)
            logTeacher = trim(dirBACKUP)//trim(tTeacher)//'.log'
            inquire(file=trim(logTeacher), exist=logExists)
            if (.not. logExists) then
                open(unit=unitUSER, file=trim(logTeacher), status='new')
            else
                open(unit=unitUSER, file=trim(logTeacher), status='old', position='append')
            end if

            ! append query to user log file
            write(unitUSER,AFORMAT) SPACE, &
                REMOTE_ADDR//' : '//currentDate//DASH//currentTime//' : '// &
                fnDescription(REQUEST)
            if (REQUEST>4) then ! no passwords
                write(unitUSER,AFORMAT) trim(cipher)
                write(unitREQ, AFORMAT) trim(cipher)
            end if

            call html_comment(fnDescription(REQUEST))

            ! compose response
            call server_respond(unitHTML)

!            ! send response to server
!            call FCGI_putfile(unitHTML)

            ! close user log file
            close(unitUSER)

            ! stop?
            if (REQUEST==fnStop) exit

        end do

        close(unitREPLAY)

        ! terminate
        call terminate(trim(fileExecutable)//' completed '//ACTION)

    end subroutine execute_log


    subroutine rename_university()

        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character (len=MAX_LEN_FILE_PATH) :: dataSource
        !character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        real :: harvest
        integer :: iTmp, jTmp, kTmp, lTmp, errNo

        ! read schedules
        do kTmp=termBegin,termEnd
            call qualify_term (kTmp, iTmp, jTmp, dataSource)
            pathToTerm = trim(itoa(iTmp))//DIRSEP//trim(txtSemester(jTmp))//DIRSEP
            ! read the classes
            call read_classes(pathToTerm, NumSections(jTmp), Section(jTmp,0:), &
                Offering(jTmp,MAX_ALL_DUMMY_SUBJECTS:), errNo)
            ! read the blocks
            call read_blocks(pathToTerm, NumBlocks(jTmp), Block(jTmp,0:), NumSections(jTmp), Section(jTmp,0:), errNo)
            ! get no. of sections by dept
            call count_sections_by_dept(jTmp, NumSections(jTmp), Section(jTmp,0:))
        end do

        ! student records
        do iTmp = 1,NumStudents
            if (mod(iTmp,1000) == 0) then
                write(*,*) trim(itoa(iTmp))//' / '//itoa(NumStudents)//' done reading ...'
            end if
            TCG = TYPE_STUDENT_RECORD (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, .false., SPACE, SPACE)
            lenTCG = 0
            call custom_read_substitutions (iTmp)
            call custom_read_student_grades (iTmp)
        end do

        ! read waivers for next term
        pathToTerm = trim(pathToNextYear)//trim(txtSemester(nextTerm))//DIRSEP
        call read_waivers(pathToTerm, NumSections(nextTerm), Section(nextTerm,0:), &
            Offering(nextTerm,MAX_ALL_DUMMY_SUBJECTS:), NumWaiverRecords, errNo)

        ! read predictions for next term
        call read_predictions(pathToTerm, NumSections(nextTerm), Section(nextTerm,0:), &
            Advised, NumPredictionRecords, errNo)

        ! read enlistment files (if any)
        call read_pre_enlistment(pathToTerm, 'ENLISTMENT', 0, 6, &
            NumSections(nextTerm), Section(nextTerm,0:), Preenlisted, NumEnlistmentRecords, errNo)

        ! rename University
        UniversityName = 'DEMO University'
        UniversityAddress = '(University Address)'
        UniversityPresident = 'Firstname MI. Lastname, Ph.D.'
        DeanOfInstruction = 'Firstname MI. Lastname, Ph.D.'
        VPAcademicAffairs = 'Firstname MI. Lastname, Ph.D.'
        DeanOfCampus = 'Firstname MI. Lastname, Ph.D.'
        REGISTRAR = 'Registrar'
        call xml_write_university(pathToYear)

!        ! rename colleges
!        do jTmp=1,NumColleges-1
!            College(jTmp)%Code = 'COLL'//itoa2bz(jTmp)
!            College(jTmp)%Name = 'College '//itoa2bz(jTmp)
!            College(jTmp)%Dean =  'Dean of College '//itoa2bz(jTmp)
!        end do
        jTmp = NumColleges
        College(jTmp)%Code = ADMINISTRATION
        College(jTmp)%Name = trim(UniversityName)//SPACE//ADMINISTRATION
        College(jTmp)%Dean = VPAcademicAffairs
        call xml_write_colleges(pathToYear)

!        ! rename departments
!        do iTmp=2,NumDepartments-1
!            Department(iTmp)%Code = 'DEPT'//itoa2bz(iTmp)
!            Department(iTmp)%Name = 'Department '//itoa2bz(iTmp)//' in '//College(Department(iTmp)%CollegeIdx)%Name
!        end do
        iTmp = NumDepartments
        REGISTRAR = 'Registrar'
        Department(iTmp)%Code = 'Registrar'
        Department(iTmp)%Name = trim(UniversityName)//SPACE//REGISTRAR
        Department(iTmp)%SectionPrefix = SPACE
        call xml_write_departments(pathToYear)

        ! rename rooms
        do jTmp=1,NumDepartments
            kTmp = 0
            do iTmp=1,NumRooms+NumAdditionalRooms
                if (jTmp/=Room(iTmp)%DeptIdx) cycle
                kTmp = kTmp + 1
                Room(iTmp)%Code = trim(Department(jTmp)%Code)//' Rm'//itoa2bz(kTmp)
            end do
        end do
        call xml_write_rooms(pathToYear)

!        ! rename teachers
!        do jTmp=1,NumDepartments
!            kTmp = 0
!            do iTmp=1,NumTeachers+NumAdditionalTeachers
!                if (jTmp/=Teacher(iTmp)%DeptIdx) cycle
!                kTmp = kTmp + 1
!                Teacher(iTmp)%TeacherID = 'T'//itoa2bz(kTmp)//'D'//itoa2bz(jTmp)
!                Teacher(iTmp)%Name = itoa2bz(kTmp)//'TeacherID in '//Department(jTmp)%Code
!            end do
!        end do

        ! rename subjects
        tSubject(1:) = 'X'
        iTmp = 0 ! area number
        kTmp = 0 ! topic number in area
        do jTmp=1,NumSubjects
            errNo = index(Subject(jTmp)%Name, SPACE)
            if (tSubject(:errNo)==Subject(jTmp)%Name(:errNo)) then ! same as previous area
                kTmp = kTmp + 1
            else ! new area
                iTmp = iTmp+1
                kTmp = 1
                tSubject = Subject(jTmp)%Name(:errNo)
            end if
            Subject(jTmp)%Name = Subject(jTmp)%Name(:errNo)//itoa3bz(kTmp)
            !Subject(jTmp)%Name = 'A'//itoa3bz(iTmp)//SPACE//itoa3bz(kTmp)
            !Subject(jTmp)%Title = 'Subject area '//itoa3bz(iTmp)//', Topic '//itoa3bz(kTmp)
        end do

        iTmp = 0 ! area number for (Must be in Plan Of Study)
        kTmp = 0 ! topic number in area
        do jTmp=NumDummySubjects,0
            if (trim(Subject(jTmp)%Title)/='(Must be in Plan Of Study)') cycle
            kTmp = kTmp + 1
            Subject(jTmp)%Name = 'ELEC '//itoa3bz(kTmp)
            Subject(jTmp)%Title = 'Elective type '//itoa3bz(kTmp)//' - Must be in Plan Of Study '
        end do
        call xml_write_subjects(pathToYear)

        ! rename curricular programs
        done = .false.
        lTmp = 0
        do jTmp=1,NumCurricula
            if (done(jTmp)) cycle
            lTmp = lTmp+1
            kTmp = 0
            tCurriculum = CurrProgCode(jTmp)
            do iTmp=jTmp,NumCurricula
                if (CurrProgCode(iTmp)/=tCurriculum) cycle
                kTmp = kTmp+1
!                Curriculum(iTmp)%Code = 'P'//itoa3bz(lTmp)//DASH//'V'//itoa2bz(kTmp)
!                Curriculum(iTmp)%Title = 'Program '//itoa3bz(lTmp)
!                Curriculum(iTmp)%Specialization = 'Variant '//itoa2bz(kTmp)
!                Curriculum(iTmp)%Remark = SPACE
!                CurrProgCode(iTmp) = 'P'//itoa3bz(lTmp)
                Curriculum(iTmp)%Code = trim(CurrProgCode(iTmp))//'-V'//itoa2bz(kTmp)
                done(iTmp) = .true.
            end do
        end do
        call xml_write_curricula(pathToYear)
        call xml_write_equivalencies(pathToYear)

        ! rewrite classes
        do kTmp=termBegin,termEnd
            call qualify_term (kTmp, iTmp, jTmp, dataSource)
            pathToTerm = trim(itoa(iTmp))//DIRSEP//trim(txtSemester(jTmp))//DIRSEP
            call xml_write_sections(pathToTerm, NumSections(jTmp), Section(jTmp,0:), 0)
            call xml_write_blocks(pathToTerm, NumBlocks(jTmp), Block(jTmp,0:), Section(jTmp,0:), 0)
        end do

        pathToTerm = trim(pathToNextYear)//trim(txtSemester(nextTerm))//DIRSEP
        ! rewrite waivers for next term
        call xml_write_waivers(pathToTerm, Section(nextTerm,0:) )
        ! rewrite predictions for next term
        call xml_write_pre_enlistment(pathToTerm, 'PREDICTIONS', Advised, Section(nextTerm,0:) )
        ! rewrite enlistment for next term
        call xml_write_pre_enlistment(pathToTerm, 'ENLISTMENT', Preenlisted, Section(nextTerm,0:) )

    !        ! rename student
    !        tArray = 0 ! count for each curriculum
    !        do lTmp = 1,NumStudents
    !            kTmp = Student(lTmp)%CurriculumIdx
    !            iTmp = tArray(kTmp)+1
    !            tArray(kTmp) = iTmp
    !            Student(lTmp)%StdNo = Student(lTmp)%StdNo(:5)//itoa3bz(kTmp)//DASH//itoa3bz(iTmp)//'0'
    !            Student(lTmp)%Name = itoa3bz(iTmp)//'Student of Program '//trim(Curriculum(kTmp)%Code)
    !        end do
        call xml_write_students(pathToYear, 0)
        do lTmp = 1,NumStudents
            if (mod(lTmp,1000) == 0) then
                write(*,*) trim(itoa(lTmp))//' / '//itoa(NumStudents)//' done writing...'
            end if

            ! generate random passing grades
            do iTmp=1,Student(lTmp)%Record(1,0)
                if (Student(lTmp)%Record(4,iTmp)<=0) cycle
                jTmp = Student(lTmp)%Record(5,iTmp)
                kTmp = jTmp
                if (jTmp>0 .and. jTmp<10) then ! numeric pass
                    do while (kTmp==jTmp .or. kTmp==0)
                        call random_number(harvest)
                        kTmp = int(harvest*10.0)
                    end do
                end if
                Student(lTmp)%Record(5,iTmp) = kTmp
            end do
            call xml_write_student_grades(lTmp)

            call xml_write_substitutions(lTmp)
        end do

        ! teachers
        call regenerate_all_passwords()
        call xml_write_teachers(pathToYear)
        call write_password_file(pathToYear)

    end subroutine rename_university


    subroutine add_data_from_enlistment(path, basename, NumSections, Section)
        character(len=*), intent(in) :: path, basename
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)

        character (len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_CLASS_ID) :: tSection
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        integer :: cdx, k, sdx, std, ier, nSubj, nSect, nStd
        type (TYPE_STUDENT) :: wrk

        character (len=MAX_LEN_FILE_PATH) :: fileName
        character (len=MAX_LEN_XML_LINE) :: line
        integer :: eof, ndels, pos(60)

        fileName = trim(dirDATA)//trim(path)//basename//'.CSV'
        open(unit=unitRAW, file=fileName, form='formatted', status='old', iostat=ier)
        if (ier/=0) return

        call file_log_message ('Retrieving additional info from '//fileName)

        nSubj = 0
        nSect =0
        nStd = 0

        loop_ENLISTMENT  : &
        do
            read (unitRAW, AFORMAT, iostat = eof) line
            if (eof<0) exit loop_ENLISTMENT
            if (line(1:1)=='#' .or. line(1:3)=='   ') cycle loop_ENLISTMENT

        !#STUDNO,SUBJECT CODE,CLASS CODE,SutdName,Course,TERM,COLLEGE,TEACHER,SECTION
        !1      2            3          4        5      6    7       8       9       10       11      12     13        14          15
        !08-09517,EM 218,G007,"Abbariao, Cristopher Adolfo",MAED-SS,13-S,GS,To Be Assigned,GS
        !08-09517,MA 204,G022,"Abbariao, Cristopher Adolfo",MAED-SS,13-S,GS,To Be Assigned,GS
        !08-09517,SOC SCI 218A,G052,"Abbariao, Cristopher Adolfo",MAED-SS,13-S,GS,To Be Assigned,GS
        !08-09517,SS 217,G053,"Abbariao, Cristopher Adolfo",MAED-SS,13-S,GS,To Be Assigned,GS
        !06-00593,BA 72,B120,"Accad, Leonard Andrew Bramaje",BSENT,13-S,CBEA,"Gonzaga, Jeremiah",BSENT-3C
        !06-00593,BA 66,B128,"Accad, Leonard Andrew Bramaje",BSENT,13-S,CBEA,"Singson, Marcial",BSENT-3C
        !08-02956,ENG 13,E028,"Adviento, Baby Jane Concepcion",BSED-FIL,13-S,CTE,"Clemente, Beatriz",BSED 1K
        !
            call index_to_delimiters(COMMA, line, ndels, pos)

            ! subject
            tSubject = line(pos(2)+1:pos(3)-1)
            cdx = index_to_subject(tSubject)
            if (cdx<=0) then ! add it
                NumAdditionalSubjects = NumAdditionalSubjects+1
                cdx = NumSubjects + NumAdditionalSubjects

                Subject(cdx)%Name = tSubject
                Subject(cdx)%Title = tSubject
                Subject(cdx)%DeptIdx = NumDepartments
                Subject(cdx)%Units = 3.0

                Subject(cdx)%TermOffered = 7
                Subject(cdx)%LectHours = 3.0
                Subject(cdx)%MinLectSize = 50
                Subject(cdx)%MaxLectSize = 50
                Subject(cdx)%LectLoad = 0.0
                Subject(cdx)%LabHours = 0.0
                Subject(cdx)%MinLabSize = 50
                Subject(cdx)%MaxLabSize = 50
                Subject(cdx)%LabLoad = 0.0

                k = 1
                Subject(cdx)%lenPreq = k
                Subject(cdx)%Prerequisite(k) = INDEX_TO_NONE
                Subject(cdx)%lenCoreq = k
                Subject(cdx)%Corequisite = INDEX_TO_NONE
                Subject(cdx)%lenConc = k
                Subject(cdx)%Concurrent = INDEX_TO_NONE
                Subject(cdx)%lenConcPreq = k
                Subject(cdx)%ConcPrerequisite= INDEX_TO_NONE

                Subject(cdx)%LabFee = 0.0
                Subject(cdx)%Tuition = 0.0

                nSubj = nSubj + 1
                call file_log_message ('Added subject '//tSubject)

            end if

            ! section
            tSection = trim(tSubject)//SPACE//line(pos(3)+1:pos(4)-1)
            sdx = index_to_section(tSection, NumSections, Section)
            if (sdx==0 .and. (pos(3)+1/=pos(4)) ) then
                do k=1,NumDepartments
                    if (Department(k)%SectionPrefix==line(poS(3)+1:pos(3)+1) ) exit
                end do
                NumSections = NumSections+1
                Section(NumSections) = TYPE_SECTION (tSection, line(pos(3)+1:pos(4)-1), SPACE, &
                    k, cdx, Subject(cdx)%MaxLectSize, Subject(cdx)%MaxLectSize, 1, 0, 0, 0, 0, 0)
                nSect = nSect + 1
                call file_log_message ('Added section '//tSection)
            end if

            ! student
            tStdNo = line(1:pos(2)-1)
            std = index_to_student(tStdNo)
            if (std==0) then
        !#STUDNO,SUBJECT CODE,CLASS CODE,SutdName,Course,TERM,COLLEGE,TEACHER,SECTION
        !1      2            3          4        5      6    7       8       9       10       11      12     13        14          15
        !08-09517,EM 218,G007,"Abbariao, Cristopher Adolfo",MAED-SS,13-S,GS,To Be Assigned,GS
        !08-09517,MA 204,G022,"Abbariao, Cristopher Adolfo",MAED-SS,13-S,GS,To Be Assigned,GS
                k = index(line, '",')
                call initialize_student(wrk)
                wrk%StdNo = tStdNo
                wrk%Name = line(pos(4)+2:k-1)
                tCurriculum = line(k+2:)
                k = index(tCurriculum, COMMA)
                tCurriculum(k:) = SPACE
                k = index_to_curriculum(tCurriculum)
                if (k<0) then
                    k = -k
                elseif (k==0) then
                    k = NumCurricula
                end if
                wrk%CurriculumIdx = k
                call update_student_info(wrk, k)
                if (k<0) call file_log_message ('Added '//tStdNo//trim(tCurriculum)//' - '// &
                    trim(text_student_curriculum(-k)) )
                nStd = nStd + 1

            end if

        end do loop_ENLISTMENT
        close(unitRAW)

        if (nSubj>0) call xml_write_subjects(pathToYear)
        if (nSect>0) call xml_write_sections(path, NumSections, Section, 0)
        if (nStd>0) call xml_write_students(pathToYear, 0)

    end subroutine add_data_from_enlistment


end module WEBSERVER
