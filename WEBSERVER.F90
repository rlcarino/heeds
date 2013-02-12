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

        integer :: device, iTmp, kStart
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        character(len=MAX_LEN_FILE_PATH) :: logTeacher
        logical :: logExists


        ! delete existing pages
        call system(delCmd//trim(dirWWW)//'index.*', iTmp)

        ! make landing page
        call html_login(trim(dirWWW)//DIRSEP//'index.html', SPACE)

        ! make error page
        call html_login(trim(dirWWW)//DIRSEP//'50x.html', &
            'The '//PROGNAME//' program is not running. Contact the '//REGISTRAR)

        ! notes
        call file_log_message(trim(fileExecutable)//' is ready.')
        if (noWrites) then ! training mode
            call file_log_message(trim(fileExecutable)//' is in training mode. '// &
                'Any made changes will be lost after the program exits.')
        end if
        ! open a 'scratch' response file
        device = stderr - 1
        open(unit=device, form='formatted', status='scratch')
        !open(unit=device, file='WRK.HTML', form='formatted', status='unknown')

        do while (FCGI_Accept() >= 0)
        
            ! timestamp of request
            call date_and_time (date=currentDate, time=currentTime)

            ! tell the webserver to expect text/html
            iTmp = FCGI_puts ('Content-type: text/html'//CRLF//NUL)

            ! rewind the response file
            rewind (device)

            ! Retrieve DOCUMENT_URI and QUERY_STRING/CONTENT
            call FCGI_getquery(device)

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

            ! Establish USERNAME/ROLE and REQUEST
            loginCheckMessage = SPACE
            call get_user_request(targetLogin)

            ! open user's log file, create if necessary
            call blank_to_underscore(USERNAME, tTeacher)
            logTeacher = trim(dirLOG)//trim(tTeacher)//'.log'
            inquire(file=trim(logTeacher), exist=logExists)
            if (.not. logExists) then
                open(unit=stderr+1, file=trim(logTeacher), status='new')
            else
                open(unit=stderr+1, file=trim(logTeacher), status='old', position='append')
            end if

            ! append query to user log file
            write(stderr+1,AFORMAT) SPACE, trim(REMOTE_ADDR)//' : '//currentDate//DASH//currentTime//' : '// &
                trim(fnDescription(REQUEST))
            if (REQUEST>4) then ! no passwords
                write(stderr+1,AFORMAT) trim(cipher)
            end if

            ! compose response
            call server_respond(device)

            ! send response to server
            call FCGI_putfile(device)

            ! close user log file
            close(stderr+1)

            if (REQUEST==fnStopProgram) exit

            ! reset available features (in case objects were added or deleted)
            call set_feature_availability()

        end do ! while (FCGI_Accept() >= 0)
        close(device)

        ! make 'Sorry!' landing page
        call html_login(trim(dirWWW)//DIRSEP//'index.html', &
            'The '//PROGNAME//' program is not running. Contact the '//REGISTRAR)

        ! wait for next query before NGINX raises error
        iTmp = FCGI_Accept()

        return

    end subroutine server_start


    subroutine server_respond (device)
        integer, intent (in) :: device

        ! set TermQualifier to next semester if REQUEST uses data for next semester
        if (REQUEST>=fnNextScheduleOfClasses) then
            fnOFFSET = fnNextOffset
            TermQualifier = green//trim(txtSemester(targetTerm+3))//' Term, SY '// &
                trim(itoa(targetYear))//DASH//itoa(targetYear+1)//black
            pathToSOURCE = pathToTarget
            pathToUPDATES = UPDATES//pathToTarget
        else
            fnOFFSET = 0
            TermQualifier = SPACE
            pathToSOURCE = pathToCurrent
            pathToUPDATES = UPDATES//pathToCurrent
        end if

        ! target object of REQUEST
        targetStudent = 0
        targetSubject = 0
        targetSection = 0
        targetDepartment = 0
        targetCurriculum =0
        targetCollege = 0
        targetRoom = 0
        targetTeacher = 0
        targetBlock = 0

        select case (REQUEST)

            case (fnLogin)
                Teacher(targetLogin)%Status = 1
                call html_college_links(device, CollegeIdxUser, mesg=loginCheckMessage)

            case (fnChangeInitialPassword, fnChangePassword)
                Teacher(targetLogin)%Status = 1
                call change_current_password(device)

            case (fnToggleTrainingMode)
                noWrites = .not. noWrites
                call html_college_links(device, CollegeIdxUser, mesg='Toggled training mode')

            case (fnSearch)
                call object_search (device)

            case (fnCollegeLinks)
                call html_college_links(device)

            case (fnSubjectList)
                call subject_list_all (device)

            case (fnEditSubject)
                call subject_edit (device)

            case (fnCurriculumList, fnActivateCurriculum, fnDeactivateCurriculum)
                call curriculum_list_all(device, REQUEST)

            case (fnCurriculum)
                call curriculum_display(device)

            case (fnEditCurriculum)
                call curriculum_edit (device)

            case (fnEditRoom)
                call room_edit (device)

            case (fnEditTeacher)
                call teacher_edit (device)

            ! cases for this semester's schedule of classes
            case (fnTeachersByDept, fnTeachersByName, fnTeacherConflicts)
                call teacher_list_all (device, NumCurrentSections, CurrentSection, REQUEST)

            case (fnTeacherSchedule)
                call teacher_schedule(device, NumCurrentSections, CurrentSection)

            case (fnPrintableWorkload)
                call teacher_schedule_printable(device, NumCurrentSections, CurrentSection, &
                    NumCurrentBlocks, CurrentBlock)

            case (fnRoomList, fnRoomConflicts)
                call room_list_all (device, NumCurrentSections, CurrentSection, REQUEST)

            case (fnRoomSchedule)
                call room_schedule(device, NumCurrentSections, CurrentSection)

            case (fnScheduleOfClasses, fnScheduleByArea, fnTBARooms, fnTBATeachers)
                call section_list_all (device, NumCurrentSections, CurrentSection, CurrentOffering, &
                    NumCurrentBlocks, CurrentBlock, REQUEST)

            case (fnScheduleOfferSubject)
                call section_offer_subject (device, NumCurrentSections, CurrentSection, CurrentOffering, &
                    NumCurrentBlocks, CurrentBlock)

            case (fnScheduleDelete)
                call section_delete(device, NumCurrentSections, CurrentSection, CurrentOffering, &
                    NumCurrentBlocks, CurrentBlock)

            case (fnScheduleAddLab)
                call section_add_laboratory(device, NumCurrentSections, CurrentSection, CurrentOffering, &
                    NumCurrentBlocks, CurrentBlock)

            case (fnScheduleEdit)
                call section_edit(device, NumCurrentSections, CurrentSection, &
                    NumCurrentBlocks, CurrentBlock)

            case (fnScheduleValidate)
                call section_validate_inputs (device, NumCurrentSections, CurrentSection, CurrentOffering, &
                    NumCurrentBlocks, CurrentBlock)

            case (fnBlockSchedule, fnBlockDeleteName, fnBlockEditName, fnBlockCopy, fnBlockDeleteAll, &
                fnBlockEditSection, fnBlockEditSubject)
                call block_show_schedule(device, NumCurrentSections, CurrentSection, CurrentOffering, &
                    NumCurrentBlocks, CurrentBlock, REQUEST)

            case (fnBlockNewSelect)
                call block_select_curriculum_year(device, NumCurrentSections, CurrentSection, NumCurrentBlocks, CurrentBlock)

            case (fnBlockNewAdd)
                call block_add(device, currentTerm, NumCurrentSections, CurrentSection, CurrentOffering, &
                    NumCurrentBlocks, CurrentBlock)

            case (fnEnlistmentSummary, fnBottleneck, fnExtraSlots, fnUnderloadSummary, fnUnderloadedStudents)
                call enlistment_summarize(device, NumCurrentSections, CurrentSection, CurrentOffering, Preenlisted, REQUEST)

            case (fnNotAccommodated)
                call enlistment_not_accommodated(device, Preenlisted)

            case (fnGradeSheet)
                call enlistment_grades(device, NumCurrentSections, CurrentSection)

            ! cases for student info
            case (fnStudentsByCurriculum,fnStudentsByname,fnStudentsByYear,fnStudentsByProgram, fnClassList)
                call links_to_students(device, REQUEST, NumCurrentSections, CurrentSection)

            case (fnStudentsDistribution)
                call student_distribution(device)

            case (fnStudentAddPrompt)
                call student_prompt_add(device)

            case (fnStudentAdd)
                call student_add(device)

            case (fnEditCheckList)
                if (Period==1) then
                    call checklist_edit(device, UseCurrentClasses, CurrentSection, CurrentOffering)
                else
                    call checklist_edit(device, UseNextClasses, NextSection, NextOffering)
                end if

            case (fnChangeMatriculation)
                call enlistment_edit(device, NumCurrentSections, CurrentSection, &
                    NumCurrentBlocks, CurrentBlock)

            case (fnFindBlock)
                call enlistment_find_block(device, NumCurrentSections, CurrentSection, &
                    NumCurrentBlocks, CurrentBlock)

            case (fnPrintableSchedule)
                call enlistment_printable(device, NumCurrentSections, CurrentSection)

            case (fnStudentPerformance)
                call student_performance(device)

            ! cases for next semester's predicted demand for subjects
            case (fnDemandFreshmen,fnUpdateDemandFreshmen)
                call demand_by_new_freshmen(device, NextOffering)

            case (fnDemandForSubjects)
                call demand_for_subjects(device)

            case (fnPotentialStudents)
                call list_potential_students(device)

            ! cases for next semester's schedule of classes
            case (fnNextTeachersByDept, fnNextTeachersByName,fnNextTeacherConflicts)
                call teacher_list_all (device, NumNextSections, NextSection, REQUEST)

            case (fnNextTeacherSchedule)
                call teacher_schedule(device, NumNextSections, NextSection)

            case (fnNextPrintableWorkload)
                call teacher_schedule_printable(device, NumNextSections, NextSection, &
                    NumNextBlocks, NextBlock)

            case (fnNextRoomList, fnNextRoomConflicts)
                call room_list_all (device, NumNextSections, NextSection, REQUEST)

            case (fnNextRoomSchedule)
                call room_schedule(device, NumNextSections, NextSection)

            case (fnNextScheduleOfClasses, fnNextScheduleByArea, fnNextTBARooms, fnNextTBATeachers)
                call section_list_all (device, NumNextSections, NextSection, NextOffering, &
                    NumNextBlocks, NextBlock, REQUEST)

            case (fnNextScheduleOfferSubject)
                call section_offer_subject (device, NumNextSections, NextSection, NextOffering, &
                    NumNextBlocks, NextBlock)

            case (fnNextScheduleDelete)
                call section_delete(device, NumNextSections, NextSection, NextOffering, &
                    NumNextBlocks, NextBlock)

            case (fnNextScheduleAddLab)
                call section_add_laboratory(device, NumNextSections, NextSection, NextOffering, &
                    NumNextBlocks, NextBlock)

            case (fnNextScheduleEdit)
                call section_edit(device, NumNextSections, NextSection, &
                    NumNextBlocks, NextBlock)

            case (fnNextScheduleValidate)
                call section_validate_inputs (device, NumNextSections, NextSection, NextOffering, &
                    NumNextBlocks, NextBlock)

            case (fnNextBlockSchedule, fnNextBlockDeleteName, fnNextBlockEditName, fnNextBlockCopy, &
                fnNextBlockDeleteAll, fnNextBlockEditSection, fnNextBlockEditSubject)
                call block_show_schedule(device, NumNextSections, NextSection, NextOffering, &
                    NumNextBlocks, NextBlock, REQUEST)

            case (fnNextBlockNewSelect)
                call block_select_curriculum_year(device, NumNextSections, NextSection, NumNextBlocks, NextBlock)

            case (fnNextBlockNewAdd)
                call block_add(device, nextTerm, NumNextSections, NextSection, NextOffering, &
                    NumNextBlocks, NextBlock)

            case (fnStopUser)
                Teacher(targetLogin)%Status = 0
                call html_landing_page(device, SPACE)

            case (fnStopProgram)
                if (isRoleAdmin) then
                    if (isDirtySTUDENTS) then
                        call xml_write_students(pathToCurrent, 0)
                    end if
                    loginCheckMessage = 'The '//PROGNAME//' program will stop.'
                else
                    REQUEST = fnStopUser
                    Teacher(targetLogin)%Status = 0
                    loginCheckMessage = SPACE
                end if
                call html_landing_page(device, loginCheckMessage)

            case default
                targetCollege = CollegeIdxUser
                targetDepartment = DeptIdxUser
                TermQualifier = SPACE
                call html_write_header(device, SPACE, &
                    '<hr>The functionality you requested is not available in '//PROGNAME//VERSION)

        end select

        call html_write_footer(device)

        return
    end subroutine server_respond


    subroutine server_end(msg)

        character(len=*), intent (in) :: msg

        call file_log_message(msg, 'Ends '//currentDate//DASH//currentTime )
        close(stderr)
        write(*,*) 'Check .log files '//trim(dirLog)//' for other messages.'
        stop
    end subroutine server_end


    subroutine object_search (device)
        integer, intent(in) :: device
        integer :: A1, ierr, dept

        ! get arguments
        call cgi_get_named_integer(QUERY_STRING, 'A1', A1, ierr)
        targetCollege = CollegeIdxUser
        targetDepartment = DeptIdxUser
        dept = targetDepartment
        if (A1<=0) then
            call html_write_header(device, 'Search', '<br><hr>Search item not specified?')
        else
            select case (A1)

                case (1) ! Student
                    call links_to_students(device, REQUEST, NumCurrentSections, CurrentSection)

                case (2) ! Section
                    call section_list_all (device, NumCurrentSections, CurrentSection, CurrentOffering, &
                        NumCurrentBlocks, CurrentBlock, REQUEST, dept)

                case (3) ! Teacher
                    call teacher_list_all (device, NumCurrentSections, CurrentSection, REQUEST)

                case (4) ! Room
                    call room_list_all (device, NumCurrentSections, CurrentSection, REQUEST)

                case (5) ! Section
                    call section_list_all (device, NumNextSections, NextSection, NextOffering, &
                        NumNextBlocks, NextBlock, REQUEST, dept)

                case (6) ! Teacher
                    call teacher_list_all (device, NumNextSections, NextSection, REQUEST)

                case (7) ! Room
                    call room_list_all (device, NumNextSections, NextSection, REQUEST)

                case default
                    call html_write_header(device, SPACE, &
                        '<hr>The functionality you requested is not available in this version of '//PROGNAME//'.')

            end select
        end if
        return
    end subroutine object_search


    subroutine set_term_offered_accg_to_curricula()
        ! reset Subject()%TermOffered based on appearance of subject in the curricular programs
        integer :: kdx, reqd

        NextOffering = TYPE_OFFERED_SUBJECTS (0, 0, 0, 0, 0, 0, 0, 0)
        do targetCurriculum=1,NumCurricula
            do kdx=1,Curriculum(targetCurriculum)%NSubjects
                targetSubject = Curriculum(targetCurriculum)%SubjectIdx(kdx)
                select case(mod(Curriculum(targetCurriculum)%SubjectTerm(kdx),3))
                    case (0) ! required during summer
                        NextOffering(targetSubject)%Demand = NextOffering(targetSubject)%Demand + 1
                    case (1) ! required during first sem
                        NextOffering(targetSubject)%NSections = NextOffering(targetSubject)%NSections + 1
                    case (2) ! required during second sem
                        NextOffering(targetSubject)%TotalSlots = NextOffering(targetSubject)%TotalSlots + 1
                end select
            end do
        end do
        do targetSubject=1,NumSubjects
            reqd = 0
            if (NextOffering(targetSubject)%Demand>0) reqd = reqd + 4 ! summer
            if (NextOffering(targetSubject)%NSections>0) reqd = reqd + 1 ! first sem
            if (NextOffering(targetSubject)%TotalSlots >0) reqd = reqd + 2 ! second sem

            if (reqd==0) then ! not required in any curriculum
                !write(*,*) trim(Subject(targetSubject)%Name)//' is not required in any curriculum?'
                Subject(targetSubject)%TermOffered = 3 ! 1,2
            else if (reqd/=Subject(targetSubject)%TermOffered) then ! not consistent
                !write(*,*) trim(Subject(targetSubject)%Name)//' is required '// &
                !  text_term_offered(reqd)//' but set to be offered '// &
                !  text_term_offered(Subject(targetSubject)%TermOffered)
                Subject(targetSubject)%TermOffered = reqd
            end if
        end do
        do targetSubject=NumDummySubjects,-2
            Subject(targetSubject)%TermOffered = 7
        end do

        return
    end subroutine set_term_offered_accg_to_curricula


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


    subroutine reset_passwords()
        character (len=MAX_LEN_PASSWORD) :: Password
        integer :: i, k, lenP

        ! the teachers
        do i=1,NumTeachers
            Teacher(i)%Password = SPACE
            Password = Teacher(i)%TeacherID
            ! use first 16 characters only
            Password(17:) = SPACE
            lenP = len_trim(Password)
            call encrypt(passwordEncryptionKey, Password)
            Teacher(i)%Password = Password
            if (trim(Teacher(i)%TeacherID)/=trim(Department(Teacher(i)%DeptIdx)%Code)) then
                Teacher(i)%Role = GUEST
            else
                Teacher(i)%Role = Department(Teacher(i)%DeptIdx)%Code
            end if
        end do
        ! create default scheduler roles
        do k=2,NumDepartments
            i = NumTeachers+k-1
            Teacher(i)%TeacherID = Department(k)%Code
            Teacher(i)%DeptIdx = NumDepartments
            Teacher(i)%Role = Teacher(i)%TeacherID
            Teacher(i)%Name = trim(Teacher(i)%TeacherID)//' Teaching Load Scheduler'
            Teacher(i)%MaxLoad = 0
            Teacher(i)%Specialization = 'Load scheduling'
            Password = Teacher(i)%TeacherID
            ! use first 16 characters only
            Password(17:) = SPACE
            lenP = len_trim(Password)
            call encrypt(passwordEncryptionKey, Password)
            Teacher(i)%Password = Password
        end do
        NumTeachers = NumDepartments + NumTeachers -1
        ! create default adviser roles
        done = .false.
        do k=1,NumCurricula
            if (done(k)) cycle
            i = NumTeachers + 1
            NumTeachers = i

            Teacher(i)%TeacherID = CurrProgCode(k)
            Teacher(i)%DeptIdx = NumDepartments
            Teacher(i)%Role = Teacher(i)%TeacherID
            Teacher(i)%Name = trim(Teacher(i)%TeacherID)//' Adviser'
            Teacher(i)%MaxLoad = 0
            Teacher(i)%Specialization = 'Student advising'
            Password = Teacher(i)%TeacherID
            ! use first 16 characters only
            Password(17:) = SPACE
            lenP = len_trim(Password)
            call encrypt(passwordEncryptionKey, Password)
            Teacher(i)%Password = Password

            do i = k+1,NumCurricula
                if (CurrProgCode(k)==CurrProgCode(i)) done(i) = .true.
            end do
        end do
        ! the Guest account
        i = NumTeachers + 1
        NumTeachers = i
        Teacher(i)%TeacherID = GUEST
        Teacher(i)%Name = 'Guest Account'
        Password = Teacher(i)%TeacherID
        ! use first 16 characters only
        Password(17:) = SPACE
        lenP = len_trim(Password)
        call encrypt(passwordEncryptionKey, Password)
        Teacher(i)%Password = Password
        Teacher(i)%Role = GUEST

        ! update  TEACHERS.XML
        call xml_write_teachers(pathToCurrent)

        return
    end subroutine reset_passwords


    subroutine change_current_password(device)
        integer, intent(in) :: device

        character (len=MAX_LEN_PASSWORD) :: t0Password, t1Password, t2Password
        integer :: ierr
        logical :: flagIsUp

        flagIsUp = .false.
        if (REQUEST==fnChangePassword) then
            call cgi_get_named_string(QUERY_STRING, 'C', t0Password, ierr)
            if ( len_trim(t0Password)>0 ) then
                t0Password(17:) = SPACE
                call encrypt(passwordEncryptionKey, t0Password)
                if (Teacher(targetLogin)%Password/=t0Password) then
                    loginCheckMessage = 'Current password is incorrect.'
                    flagIsUp = .true.
                else
                    loginCheckMessage = 'Change current password.'
                end if
            else
                loginCheckMessage = ''
                flagIsUp = .true.
            end if
        end if
        if (.not. flagIsUp) then
            call cgi_get_named_string(QUERY_STRING, 'P', t1Password, ierr)
            call cgi_get_named_string(QUERY_STRING, 'R', t2Password, ierr)
            if ( len_trim(t1Password)>0 .and. len_trim(t2Password)>0 ) then
                if ( t1Password==t2Password ) then
                    t1Password(17:) = SPACE
                    call encrypt(passwordEncryptionKey, t1Password)
                    if (Teacher(targetLogin)%Password/=t1Password .and. &
                            Teacher(targetLogin)%TeacherID/=t2Password) then
                        Teacher(targetLogin)%Password = t1Password
                        call xml_write_teachers(pathToCurrent)
                        loginCheckMessage = 'Successfully changed password for '//USERNAME
                        call html_college_links(device, CollegeIdxUser, mesg=loginCheckMessage)
                        return
                    else
                        loginCheckMessage = 'New password is the same as the old password or username'
                    end if
                else
                    loginCheckMessage = 'New password and repeat do not match'
                end if
            else
                loginCheckMessage = ''
            end if
        end if

        write(device,AFORMAT) &
            '<html><head><title>'//PROGNAME//VERSION//'</title></head><body>', &
            '<h1>'//trim(UniversityCode)//nbsp//PROGNAME//' Password Service</h1>'
        if (len_trim(loginCheckMessage)>0) write(device,AFORMAT) &
            red//trim(loginCheckMessage)//black

        write(device,AFORMAT) '<br>'//&
            '<form name="input" method="post" action="'//CGI_PATH//'">', &
            '<input type="hidden" name="F" value="'//trim(itoa(REQUEST))//'">', &
            '<input type="hidden" name="N" value="'//trim(USERNAME)//'">', &
            '<h2>Change password for '//trim(USERNAME)//'</h2><br>'
        if (REQUEST==fnChangePassword) write(device,AFORMAT) &
            '<b>Old password:</b><br>', &
            '<input size="20" type="password" name="C" value="">', &
            '<br><br>'
        write(device,AFORMAT) &
            '<b>New password:</b><br>', &
            '<input size="20" type="password" name="P" value="">', &
            '<br><br>', &
            '<b>Repeat new password:</b><br>', &
            '<input size="20" type="password" name="R" value="">', &
            '<br><br>', &
            '<input type="submit" value="Update"></form><hr>'

        return
    end subroutine change_current_password


    subroutine get_user_request(idxUser)

        integer, intent(out) :: idxUser ! index to USERNAME in Teachers()

        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character (len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character (len=MAX_LEN_PASSWORD) :: tPassword, cPassword
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
        idxUser = index_to_teacher(USERNAME)

        if (idxUser>0) then
            ROLE = Teacher(idxUser)%Role
        else ! not in Teachers(); assume Guest
            USERNAME = GUEST
            ROLE = GUEST
        end if

        if (trim(ROLE)==GUEST) then ! Guest
            isRoleGuest = .true.
            if (trim(USERNAME)/=GUEST) then
                DeptIdxUser = Teacher(idxUser)%DeptIdx
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
            REQUEST = fnStopUser
            return
        end if
        if (REQUEST/=fnLogin) then ! not logging in
            ! previously logged out?  force user to login
            if (Teacher(idxUser)%Status==0 .and. .not. isRoleGuest) REQUEST = fnStopUser
            return
        end if
        ! request is login; validate POSTed data

        ! Always allow Guest account
        if (trim(USERNAME)==GUEST) then ! Guest
            loginCheckMessage = &
                ' You are logged in as Guest. Contact the '//REGISTRAR//' to obtain an account.'
            return
        end if

        ! Return login page if password not provided
        call cgi_get_named_string(QUERY_STRING, 'P', tPassword, ierr)
        if (ierr==-1) then
            REQUEST = fnStopUser
            loginCheckMessage = 'Username and/or Password not valid.'
            return
        end if

        ! Return login page if password not correct for user
        cPassword = tPassword
        call encrypt(passwordEncryptionKey, cPassword)
        if ( trim(Teacher(idxUser)%Password)/=trim(cPassword) ) then
            REQUEST = fnStopUser
            loginCheckMessage = 'Username and/or Password not valid.'
            return
        end if
        ! password matched

        ! password same as username?
        if ( trim(USERNAME)/=trim(tPassword) ) then
            loginCheckMessage = 'Successful login for '//USERNAME
        else ! first time to login
            REQUEST = fnChangeInitialPassword ! initial login; force change password
            loginCheckMessage = 'Password not set.'
        end if

        return
    end subroutine get_user_request


end module WEBSERVER
