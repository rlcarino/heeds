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

    implicit none

    integer, parameter :: sleep_time = 2 ! if there are no active users


contains


    subroutine server_start()

        integer :: i, active_users, sleep_time = 2 ! if there are no active users
        
        ! delete existing pages
        call system(delCmd//trim(dirWWW)//'index.*', i)

        ! the temporary directory for responses
        QUERY_STRING = dirTmp
        ! write the CGI script, change "\" to "/"
        do i=1,len_trim(dirTmp)
            if (QUERY_STRING(i:i)==BSLASH) QUERY_STRING(i:i) = FSLASH
        end do

        ! make the landing page and the page that communicates with the back-end
#if defined GROUP
        call cgi_write_index_php(trim(dirWWW)//DIRSEP//'index.php')
        call cgi_write_heeds_php(trim(dirCGI)//CGI_SCRIPT, trim(QUERY_STRING), '30') ! ('NN' seconds to timeout)
#else
        call html_login(trim(dirWWW)//DIRSEP//'index.html')
        call cgi_write_script_php(trim(dirCGI)//CGI_SCRIPT, trim(QUERY_STRING), '30') ! ('NN' seconds to timeout)
#endif

        call file_log_message('# cgi script ='//trim(dirCGI)//CGI_SCRIPT)
        call file_log_message('# scratch directory ='//trim(dirTmp))
        
        ! invalidate previous requests
        call server_check_mailbox(1,active_users)
        call file_log_message('# stale requests ='//itoa(active_users))

        call file_log_message(trim(fileExecutable)//' is ready.')
        if (noWrites) then ! training mode
            call file_log_message(trim(fileExecutable)//' is in training mode. '// &
                'Any made changes will be lost after the program exits.')
        end if

        ! loop until fnStopProgram is requested
        REQUEST = 0
        do while (.true.)

            call server_check_mailbox(2, active_users)
            if (active_users == 0) then ! no mailboxes
                call sleep (sleep_time)
            end if

            if (REQUEST==fnStopProgram) exit

        end do

#if defined PERSONAL
        ! re-make the landing page
        call html_login(trim(dirWWW)//DIRSEP//'index.html', trim(fileExecutable)//' is not running.')
        ! delete the CGI script
        call system(delCmd//trim(dirCGI)//CGI_SCRIPT, i)
#endif

        return
    end subroutine server_start
  

    subroutine server_check_mailbox(actionIndex, activeUsers)

        integer, intent(in) :: actionIndex
        integer, intent(out) :: activeusers

        integer :: ierr, pos, t_unit, sarray(13)
        character (len=MAX_LEN_FILE_PATH) :: tLine, mailBox, responseFile

        ! make a list of mailboxes
        activeUsers = 0
        call system(dirCmd//trim(dirTmp)//' > mailboxes', ierr)
        !write(*,*) 'dir *.mbox: ', ierr
        ierr = stat('mailboxes', sarray)
        !write(*,*) 'stat mailboxes: ', ierr

        ! any mailboxes ?
        if (ierr .eq. 0 .and. sarray(8) .gt. 0) then

            open(unit=8, file='mailboxes', status='unknown', form='formatted', iostat=ierr)
            !write(*,*) 'open mailBox: ', ierr

            ! loop through mailboxes
            do while (ierr==0)

                read(8,AFORMAT,iostat=ierr) tLine
                if (ierr .lt. 0) then ! no more mailboxes
                    close(8)
                    exit
                end if

                ! a mailBox?
                if (index(tLine, '.mbox') == 0) cycle
                !write(*,*) trim(tLine)

                ! initialize
                QUERY_STRING = SPACE

                ! form path to response
                mailBox = trim(dirTmp)//tLine

                ! extract name of response file by stripping '.mbox'
                pos = len_trim(tLine)
                responseFile = trim(dirTmp)//tLine(:pos-5)
                !write(*,*)  trim(responseFile)

                ! open mailBox
                t_unit = 100+activeUsers
                open(unit=t_unit, file=mailBox, form='formatted', iostat=ierr)
                !write(*,*) 'open : ', ierr
                if (ierr .lt. 0) then
                    ierr = 0
                    cycle
                end if

                ! read QUERY
                read(t_unit,AFORMAT, iostat=ierr) QUERY_STRING
                !write(*,*) 'read : ', ierr
                if (ierr .lt. 0) then
                    ierr = 0
                    cycle
                end if
                close(t_unit)

                ! remove quotes
                pos = len_trim(QUERY_STRING)
                QUERY_STRING = QUERY_STRING(2:pos-1)

                ! append query to log file
                call file_log_message ('Request: '//trim(tLine)//'?'//trim(QUERY_STRING))

                ! respond
                call open_for_write(t_unit, responseFile)
                select case (actionIndex)

                    case (1) ! response to stale requests
                        call html_login(responseFile, mesg=PROGNAME//' is undergoing maintenance. Please try again later.')

                    case (2) ! response to current requests
                        call server_respond(t_unit)
                        ! reset available features (in case objects were added or deleted)
                        call set_feature_availability()

                end select
                close(t_unit)

                ! write lock
                call write_lock_file(responseFile)

                ! delete mailBox
                call unlink(mailBox, ierr)

                ! increment counter for active users
                activeUsers = activeUsers + 1

            end do

        end if

        return
    end subroutine server_check_mailbox


    subroutine server_respond (device)
        integer, intent (in) :: device

        integer :: ierr

        ! Establish REQUESTed function; initialize arguments
        call cgi_get_named_integer(QUERY_STRING, 'F', REQUEST, ierr)
        if (ierr==-1) REQUEST = fnLogin

        ! set TermQualifier to next semester if REQUEST uses data for next semester
        if (REQUEST>=fnNextScheduleOfClasses) then
            fnOFFSET = fnNextOffset
            TermQualifier = green//trim(txtSemester(targetTerm+3))//' Semester, SY '// &
                trim(itoa(targetYear))//DASH//itoa(targetYear+1)//black
            pathToSOURCE = pathToTarget
            pathToUPDATES = UPDATES//pathToTarget
        else
            fnOFFSET = 0
            TermQualifier = SPACE
            pathToSOURCE = pathToCurrent
            pathToUPDATES = UPDATES//pathToCurrent
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
        call cgi_get_named_string(QUERY_STRING, 'U', USER, ierr)
#if defined PERSONAL
        if (REQUEST==fnLogin) then ! set defaultUSER
            if (ierr==0) defaultUSER = USER
        else
            USER = defaultUSER
        end if
#endif
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

            case (fnStopUser)
                targetCollege = CollegeIdxUser
                targetDepartment = DeptIdxUser
                TermQualifier = SPACE
                call html_landing_page(device)


            case (fnStopProgram)
                if (isDirtySTUDENTS) then
                    call xml_write_students(pathToCurrent, 0)
                end if
                targetCollege = CollegeIdxUser
                targetDepartment = DeptIdxUser
                TermQualifier = SPACE
                call html_landing_page(device, 'You stopped '//trim(fileExecutable))


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
        call file_log_message(msg, 'Ends '//currentDate//DASH//currentTime(1:4) )
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
            '<form name="input" method="post" action="'//CGI_PATH//'">', &
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


    subroutine cgi_write_index_php(fileName)

        character (len=*), intent (in) :: fileName
        integer :: device=2

        call open_for_write (device, fileName)
        write(device,AFORMAT) &
            '<?php', &
            '    // index.php', &
            '    // By '//EMAIL, &
            '    // For '//PROGNAME, &
            '    // See '//WEB, &
            '', &
            '    $username = ""; // Specified by user during registration', &
            '    $role = ""; // Specified by administrator when approving user''s registration', &
            '    $remaining = 0; // timeout - time(), where timeout is set during login', &
            '', &
            '    session_start(); // Start a session, or resume existing session', &
            '    if( isset($_SESSION["user"]) ) // Already set ?', &
            '    {', &
            '        $username = $_SESSION["user"]["username"];', &
            '        $role = $_SESSION["user"]["role"];', &
            '        $remaining = $_SESSION["user"]["timeout"] - time();', &
            '    }', &
            '', &
            '    if( empty($username) or empty($role) or ($remaining <= 0 ) ) // User data is valid ?', &
            '        $redirect = "/cgi-bin/login.php"; // Redirect to login page', &
            '    else', &
            '        $redirect = "/cgi-bin/'//CGI_SCRIPT//'"; // Allow to proceed', &
            '', &
            '    header("Location: " . $redirect);', &
            '    die("Redirecting to:  " . $redirect);', &
            '?>'

        close(device)

        return
    end subroutine cgi_write_index_php


    subroutine cgi_write_heeds_php(fileName, wrkDir, waitTime)

        character (len=*), intent (in) :: fileName, wrkDir, waitTime

        integer :: device=2

        call open_for_write (device, fileName)
        write(device,AFORMAT) &
            '<?php', &
            '    // '//CGI_SCRIPT, &
            '    // By '//EMAIL, &
            '    // For '//PROGNAME, &
            '    // See '//WEB, &
            '', &
            '    $max_seconds_to_wait = '//waitTime//'; // max wait time for response from back-end (seconds)', &
            '    $sleeptime = 100000; // usecs (microseconds; 1 sec = 1 000 000 usec)', &
            '    $username = ""; // Specified by user during registration', &
            '    $role = ""; // Set by administrator when approving user''s registration', &
            '    $remaining = 0; // timeout - time(), where timeout is set during login', &
            '', &
            '    session_start(); // Start a session, or resume an existing session', &
            '    if( isset($_SESSION["user"]) ) // Already set ?', &
            '    {', &
            '        $username = $_SESSION["user"]["username"];', &
            '        $role = $_SESSION["user"]["role"];', &
            '        $remaining = $_SESSION["user"]["timeout"] - time();', &
            '    }', &
            '    if( empty($username) or empty($role) or ($remaining <= 0 ) ) // User data is valid ?', &
            '    {', &
            '        echo "<h1><font color=''#ff0000''>Please return to the login page.</font></h1>" . ', &
            '            "There is a problem with one of the following :<br><i>" . ', &
            '            "&nbsp; Username is ''" . $username . "''<br>" .', &
            '            "&nbsp; Role is ''" . $role . "''<br>" .', &
            '            "&nbsp; Remaining time is " . $remaining . " seconds</i>" .', &
            '            "<br><br>Your IP address is " . $_SERVER["REMOTE_ADDR"];', &
            '        die ("<hr>Go to <a href=''/cgi-bin/login.php''>HEEDS Login</a>");', &
            '    }', &
            '', &
            '    // create mailbox name from timestamp and IP address of user', &
            '    $timestamp = gettimeofday();', &
            '    $fname = "'//trim(dirTmp)//'" . $timestamp["sec"] . $timestamp["usec"] . "-" . $_SERVER["REMOTE_ADDR"];', &
            '', &
            '    $handle = fopen($fname . ".mbox", "wt"); // open mailbox for writing', &
            '', &
            '    // write  "N=username&U=role&[GET/POST data]"\n to mailbox', &
            '    if ($_SERVER["REQUEST_METHOD"]=="POST")', &
            '        fwrite($handle, "\"U=" . $role . "&N=" . $username . "&" . file_get_contents("php://input") . "\"\n");', &
            '    else', &
            '        fwrite($handle, "\"U=" . $role . "&N=" . $username . "&" . $_SERVER["QUERY_STRING"] . "\"\n");', &
            '', &
            '    fclose($handle); // close mailbox', &
            '', &
            '    // wait for backend to read mailbox and to respond', &
            '    $loopcount = $max_seconds_to_wait * 1000000 / $sleeptime;', &
            '    for ($i=1; $i<=$loopcount; $i++)', &
            '    {', &
            '        usleep($sleeptime);', &
            '        if (is_file($fname . ".lock")) // lock indicates the back-end has responded', &
            '        {', &
            '            copy ($fname, "php://output"); // display response', &
            '            unlink ($fname . ".lock"); // delete lock', &
            '            unlink ($fname); // delete response', &
            '            return; // quit', &
            '        }', &
            '    }', &
            '', &
            '    // backend did not respond; remove mailbox', &
            '    if( is_file($fname . ".mbox") ) unlink ($fname . ".mbox");', &
            '', &
            '    unset($_SESSION["user"]); // clear the session', &
            '', &
            '    // display ''sorry'' message', &
            '    die("<html><head><title>'//PROGNAME//VERSION//'</title></head><body>" . ', &
            '        "<h1>'//trim(UniversityCode)//nbsp//PROGNAME//'</h1>" . ', &
            '        "<font color=''#ff0000''>'//trim(fileExecutable)//' is not running.</font><br><br>" . ', &
            '        "<i>Contact the '//PROGNAME//' Administrator.</i>" .', &
            '        "</body></html>");', &
            '?>'

        close(device)
        return
    end subroutine cgi_write_heeds_php


    subroutine cgi_write_script_php(fileName, wrkDir, waitTime)

        character (len=*), intent (in) :: fileName, wrkDir, waitTime

        integer :: device=2

        call open_for_write (device, fileName)
        write(device,AFORMAT) &
            '<?php', &
            '    // '//CGI_SCRIPT, &
            '    // By '//EMAIL, &
            '    // For '//PROGNAME, &
            '    // See '//WEB, &
            '', &
            '    $max_seconds_to_wait = '//waitTime//'; // max wait time for response from back-end (seconds)', &
            '    $sleeptime = 100000; // usecs (microseconds; 1 sec = 1 000 000 usec)', &
            '', &
            '    // create mailbox name from timestamp and IP address of user', &
            '    $timestamp = gettimeofday();', &
            '    $fname = "'//trim(dirTmp)//'" . $timestamp["sec"] . $timestamp["usec"] . "-" . $_SERVER["REMOTE_ADDR"];', &
            '    $handle = fopen($fname . ".mbox", "wt"); // open mailbox for writing', &
            '', &
            '    // write GET/POST data to mailbox', &
            '    if ($_SERVER["REQUEST_METHOD"]=="POST")', &
            '        fwrite($handle, "\"" . file_get_contents("php://input") . "\"\n");', &
            '    else', &
            '        fwrite($handle, "\"" . $_SERVER["QUERY_STRING"] . "\"\n");', &
            '    fclose($handle); // close mailbox', &
            '', &
            '    // wait for backend to read mailbox and to respond', &
            '    $loopcount = $max_seconds_to_wait * 1000000 / $sleeptime;', &
            '    for ($i=1; $i<=$loopcount; $i++)', &
            '    {', &
            '        usleep($sleeptime);', &
            '        if (is_file($fname . ".lock")) // lock indicates the back-end has responded', &
            '        {', &
            '            copy ($fname, "php://output"); // display response', &
            '            unlink ($fname . ".lock"); // delete lock', &
            '            unlink ($fname); // delete response', &
            '            return; // quit', &
            '        }', &
            '    }', &
            '', &
            '    // backend did not respond; remove mailbox', &
            '    if( is_file($fname . ".mbox") ) unlink ($fname . ".mbox");', &
            '', &
            '    // display ''sorry'' message', &
            '    die("<html><head><title>'//PROGNAME//VERSION//'</title></head><body>" . ', &
            '        "<h1>'//trim(UniversityCode)//nbsp//PROGNAME//'</h1>" . ', &
            '        "<font color=''#ff0000''>'//trim(fileExecutable)//' is not running.</font><br><br>" . ', &
            '        "</body></html>");', &
            '?>'

        close(device)
        return
    end subroutine cgi_write_script_php


end module SERVER
