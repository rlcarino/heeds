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
        CGI_PATH = 'http://'//trim(QUERY_put)//FSLASH//trim(UniversityCode)//FSLASH//ACTION
        call html_login('Stop-'//trim(UniversityCode)//DASH//trim(ACTION)//'.html', &
            trim(make_href(fnStop, 'Stop', post=nbsp//trim(UniversityCode)//FSLASH//ACTION)))

        ! reset CGI_PATH
        CGI_PATH = FSLASH//trim(UniversityCode)//FSLASH//ACTION

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
            logTeacher = trim(dirLOG)//trim(tTeacher)//'.log'
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
#if defined PRODUCTION
#else
            write(unitHTML,AFORMAT) '<!-- '//trim(fnDescription(REQUEST))//' -->'
#endif

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

        return

    end subroutine server_start


    subroutine server_respond (device)
        integer, intent (in) :: device

        integer :: tYear, tTerm

#if defined PRODUCTION
#else
        write(unitHTML,AFORMAT) '<!-- server_respond() -->'
#endif
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
            case (fnScheduleOfClasses, fnScheduleByArea, fnTBARooms, fnTBATeachers)
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

            case (fnTeacherSchedule)
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
                    Section(nextTerm,0:), Offering(nextTerm,MAX_ALL_DUMMY_SUBJECTS:))

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

!            case (fnGradeSheet)
!                call enlistment_grades(device, NumSections(currentTerm), Section(currentTerm,0:))

            case (fnClassList)
                call class_list(device, NumSections(currentTerm), Section(currentTerm,0:), Preenlisted)

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

        return
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
        do ldx=1,NumCurricula-1
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

        return
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
        return
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

        return
    end subroutine get_user_request


    subroutine download_xml(device)
        integer, intent(in) :: device

        character (len=MAX_LEN_FILE_PATH) :: XMLfile, fileName
        character (len=MAX_LEN_XML_LINE) :: line
        integer :: ierr, eof
        logical :: fileExists

        call cgi_get_named_string(QUERY_STRING, 'A1', XMLfile, ierr)

        fileName = trim(dirXML)//trim(pathToYear)//XMLfile
        inquire(file=fileName, exist=fileExists)
        if (.not. fileExists) then
            fileName = trim(dirXML)//trim(pathToTerm)//XMLfile
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

        return
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
        CGI_PATH = FSLASH//trim(UniversityCode)//FSLASH//ACTION

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
            logTeacher = trim(dirLOG)//trim(tTeacher)//'.log'
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
#if defined PRODUCTION
#else
            write(unitHTML,AFORMAT) '<!-- '//trim(fnDescription(REQUEST))//' -->'
#endif

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

        return

    end subroutine execute_log


end module WEBSERVER
