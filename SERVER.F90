!======================================================================
!
!    HEEDS (Higher Education Enrollment Decision Support) - A program
!      to create enrollment scenarios for 'next term' in a university
!    Copyright (C) 2012 Ricolindo L Carino
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


module SERVER

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

    use ISO_C_BINDING

    implicit none


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

contains


    subroutine server_loop()

        ! for POSTed data
        character(len=1) :: ch
        character(len=10) :: contentLength

        integer :: nChars, ierr, device, nHits

        ! Counter for requests
        nHits = 0

        ! open a 'scratch' response file
        device = max(10, getpid()) ! unit number for response file
        open(unit=device, form='formatted', file='/tmp/'//itoa(device)) !status='scratch')

        do while (FCGI_Accept() >= 0)

            ! rewind the response file
            rewind (device)

            ! Tell the webserver to expect text/html
            nChars = FCGI_puts ('Content-type: text/html'//CRLF//NUL)

            ! Request method was GET ?
            call get_environment_variable('QUERY_STRING', QUERY_STRING)
            nChars = len_trim(QUERY_STRING)
            if (nChars==0) then
                ! Request method was POST ?
                call get_environment_variable('CONTENT_LENGTH', contentLength)
                nChars = len_trim(contentLength)
                QUERY_STRING = SPACE
                if (nChars>0) then
                    nChars = atoi(contentLength)
                    do ierr=1,nChars
                        ch = FCGI_getchar()
                        QUERY_STRING(ierr:ierr) = ch
                    end do
                !else  ! not GET nor POST
                end if
            end if

            ! append query to log file
            nHits = nHits + 1
            call file_log_message ('Request '//trim(itoa(nHits))//': '//trim(QUERY_STRING))

            call server_respond(device)

            ! Flush all writes to the response file
            flush(device)

            ! copy to FCGI's stdout
            rewind(device)
            do while (.true.)

                read(device ,AFORMAT,iostat=ierr) QUERY_STRING
                if (ierr < 0) exit ! no more lines

                ! FCGI expects NULL terminated strings
                nChars = FCGI_puts (trim(QUERY_STRING)//NUL)

            end do

            if (REQUEST==fnStopProgram) exit

            ! reset available features (in case objects were added or deleted)
            call set_feature_availability()

        end do ! while (FCGI_Accept() >= 0)
        close(device)

        return

    end subroutine server_loop


    subroutine server_respond (device)
        integer, intent (in) :: device

        integer :: ierr

        ! Establish REQUESTed function; initialize arguments
        call cgi_get_named_integer(QUERY_STRING, 'F', REQUEST, ierr)

        ! if no REQUESTED funtion, return the landing page
        if (ierr==-1) REQUEST = fnLogin

        ! set TermQualifier to next semester if REQUEST uses data for next semester
        if (REQUEST>=fnNextScheduleOfClasses) then
            fnOFFSET = fnNextOffset
            TermQualifier = green//trim(txtSemester(targetTerm+3))//' Semester, SY '// &
                trim(itoa(targetYear))//dash//itoa(targetYear+1)//black
            pathToSOURCE = pathToTarget
            pathToUPDATES = 'UPDATES'//DIRSEP//pathToTarget
        else
            fnOFFSET = 0
            TermQualifier = SPACE
            pathToSOURCE = pathToCurrent
            pathToUPDATES = 'UPDATES'//DIRSEP//pathToCurrent
        end if

        ! target object of REQUESTed function
        targetStudent = 0
        targetSubject = 0
        targetSection = 0
        targetDepartment = 0
        targetCurriculum =0
        targetCollege = 0
        targetRoom = 0
        targetBlock = 0

        ! Establish ROLE
        !call cgi_get_named_integer(QUERY_STRING, 'U', USER, ierr)
        ! hardcode USER to Registrar for now
        USER = REGISTRAR
        call get_user_info()

        ! Write response to REQUEST
        select case (REQUEST)

            case (fnLogin)
                call html_college_links(device, CollegeIdxUser)

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

            case (fnStopProgram)
                if (isDirtySTUDENTS) then
                    call xml_write_students(pathToCurrent, 0)
                end if
                targetCollege = CollegeIdxUser
                targetDepartment = DeptIdxUser
                TermQualifier = SPACE

                call html_write_header(device, SPACE, &
                    '<hr>The stop signal was sent to the '//PROGNAME//' program.')

            case default
                targetCollege = CollegeIdxUser
                targetDepartment = DeptIdxUser
                TermQualifier = SPACE
                call html_write_header(device, SPACE, &
                    '<hr>The functionality you requested is not available in this version of '//PROGNAME//'.')

        end select

        call html_write_footer(device)

        return
    end subroutine server_respond


    subroutine server_end(msg)

        character(len=*), intent (in) :: msg

        call date_and_time (date=currentDate,time=currentTime)
        call file_log_message(msg, 'Ends '//currentDate//dash//currentTime(1:4) )
        close(stderr)
        write(*,*) 'Check the latest .log file in '//trim(dirLog)//' for other messages.'
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

        return
    end subroutine set_term_offered_accg_to_curricula


    subroutine student_prompt_add(device)
        integer, intent (in) :: device
        integer :: ldx

        call html_write_header(device, 'Add a student')
        write(device,AFORMAT) &
            '<form name="input" method="post" action="'//CGI_SCRIPT//'">', &
            '<input type="hidden" name="F" value="'//trim(itoa(fnStudentAdd))//'">', &
            '<table border="0" width="100%" cellpadding="0" cellspacing="0">'
        ! student number
        write(device,AFORMAT) &
            begintr//begintd//'Student Number:'//endtd//begintd// &
            '<input name="StdNo" value="StdNo">'//endtd//endtr
        ! name
        write(device,AFORMAT) &
            begintr//begintd//'Lastname Firstname MI:'//endtd//begintd// &
            '<input name="Name" size="40" value="Lastname Firstname MI">'//endtd//endtr
        ! gender
        write(device,AFORMAT) &
            begintr//begintd//'Gender:'//endtd//begintd//'<select name="Gender">', &
            '<option value=""> (select) <option value="F"> Female <option value="M"> Male', &
            '</select>'//endtd//endtr
        ! country index
        write(device,AFORMAT) &
            begintr//begintd//'Country:'//endtd//begintd//'<select name="CountryIdx">', &
            '<option value="1"> Philippines <option value="0"> Other ', &
            '</select>'//endtd//endtr
        ! curriculum
        write(device,AFORMAT) &
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


end module SERVER
