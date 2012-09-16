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

        integer :: active_users
  
        ! loop until the file "stopped" exists
        ! this file is created when 'Stop PROGNAME' is selected by REGISTRAR
        do while (access('stopped', 'rw') .ne. 0) ! stopped exists?

            call server_check_mailbox(2, active_users)
            if (active_users == 0) then ! no mailboxes
                call sleep (sleep_time)
            end if

        end do
        call unlink('stopped', active_users)
        return

    end subroutine server_start
  
  
    subroutine server_end(msg)

        character(len=*), intent (in) :: msg

        call date_and_time (date=currentDate,time=currentTime)
        call file_log_message(msg, 'Ends '//currentDate//dash//currentTime(1:4) )
        close(stderr)
        write(*,*) 'Check the latest .log file in '//trim(dirLog)//' for other messages.'
        stop
    end subroutine server_end


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
                REMOTE_ADDR = SPACE

                ! form path to response
                mailBox = trim(dirTmp)//tLine

                ! extract name of response file by stripping '.mbox'
                pos = len_trim(tLine)
                responseFile = trim(dirTmp)//tLine(:pos-5)
                !write(*,*)  trim(responseFile)

                ! make a copy of timestamp & IP address
                UserList(0)%lastQUERY = tLine(:pos-5)

                ! extract REMOTE_ADDRESS
                pos = len_trim(responseFile)-1
                do while (responseFile(pos:pos)/=dash)
                    pos = pos-1
                end do
                REMOTE_ADDR = responseFile(pos+1:)

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

                ! de-obfuscate?
                if (QUERY_STRING(1:4)=='GET:') then ! METHOD=GET
                    QUERY_STRING = QUERY_STRING(5:) ! remove initial "GET:"
                    call cgi_obfuscate(QUERY_STRING)
                end if

                ! append query to log file
                call file_log_message ('Request: '//trim(tLine)//'?'//trim(QUERY_STRING))

                ! make a copy QUERY
                UserList(0)%lastQUERY = trim(UserList(0)%lastQUERY)//'?'//QUERY_STRING

                ! respond
                select case (actionIndex)

                    case (1) ! response to stale requests
                        call html_login(responseFile, mesg=PROGNAME//' is undergoing maintenance. Please try again later.')

                    case (2) ! response to current requests
                        call server_respond(responseFile)
                        ! reset available features (in case objects were added or deleted)
                        call set_feature_availability()

                end select

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


    subroutine server_respond (fname)
        character(len=*), intent (in) :: fname
        integer :: ierr, device, logoutUSERidx
        character(len=10) :: submitButton
        character (len=MAX_LEN_USER_CODE) :: logoutUSER

        !write(*,*) trim(fname), '?', trim(QUERY_STRING)

        ! Establish REQUESTed function; initialize arguments
        call cgi_get_named_integer(QUERY_STRING, 'F', REQUEST, ierr)
        if (REQUEST==fnStopProgram) then
            call cgi_get_named_string(QUERY_STRING, 'A', submitButton, ierr)
            if (trim(submitButton)=='Logout') then
                REQUEST = fnStopUser
            end if
        end if

        ! set TermQualifier to next semester if REQUEST uses data for next semester
        if (REQUEST>=fnNextScheduleOfClasses) then
            fnOFFSET = fnNextOffset
            TermQualifier = green//trim(txtSemester(targetTerm+3))//' Semester, SY '// &
                trim(itoa(targetYear))//dash//itoa(targetYear+1)//black
            pathToSections = pathToTarget
            pathToSectionUpdates = 'updates-to-classes'//DIRSEP//pathToTarget
        else
            fnOFFSET = 0
            TermQualifier = SPACE
            pathToSections = pathToCurrent
            pathToSectionUpdates = 'updates-to-classes'//DIRSEP//pathToCurrent
        end if

        targetStudent = 0
        targetSubject = 0
        targetSection = 0
        targetDepartment = 0
        targetCurriculum =0
        targetCollege = 0
        targetRoom = 0
        targetBlock = 0

        ! Establish ROLE
        call cgi_get_named_integer(QUERY_STRING, 'U', targetUser, ierr)
        if (targetUser<1 .or. targetUser>NumUsers) then
            call html_login(fname, mesg='Please select a user role.')
            return
        end if
        USER = UserList(targetUser)%Code
        isRoleAdmin = .false.
        isRoleSRE = .false.
        isRoleChair = .false.
        DeptIdxUser = UserList(targetUser)%DeptIdx
        CollegeIdxUser = UserList(targetUser)%CollegeIdx
        CurriculumIdxUser = UserList(targetUser)%CurriculumIdx
        if (trim(USER)==REGISTRAR) then
            isRoleAdmin = .true.
        elseif (UserList(targetUser)%Role==2) then
            isRoleSRE = .true.
        elseif (UserList(targetUser)%Role==1) then
            isRoleChair = .true.
        end if

        ! copy from UserList(0), set in server_check_mailbox()
        UserList(targetUser)%lastQUERY = UserList(0)%lastQUERY

        ! STOP the program by admin?
        if (REQUEST==fnStopProgram) then ! reset session
            UserList(targetUser)%Session = -1
            if (isDirtySTUDENTS) then
                call xml_write_students(pathToCurrent, 0)
                isDirtySTUDENTS = .false.
            end if
            ! write 'Sorry!' page for incoming requests
            call html_sorry(trim(dirWWW)//DIRSEP//'index.html', 'The '//PROGNAME//' back-end program is not running?')
            call html_sorry(fname, 'The stop signal was sent to '//PROGNAME)

            if (isRoleAdmin) call system('echo stop > stopped')
            return

        ! logging out ?
        else if (REQUEST==fnStopUser) then ! reset session
            UserList(targetUser)%Session = -1
            if (isDirtySTUDENTS) then
                call xml_write_students(pathToCurrent, 0)
                isDirtySTUDENTS = .false.
            end if
            call html_login(fname)
            return

        ! logging in, but already has an ongoing session? (multiple logins)
        else if (REQUEST==fnLogin) then
            if (UserList(targetUser)%Session/=-1) then ! true=logged in
                call html_login(fname, mesg=trim(USER)//' has an ongoing session at IP address '// &
                    trim(REMOTE_ADDR)//'. Please logout that session first.')
                return
            else
                ! check password if REGISTRAR
                if (isRoleAdmin .and. checkPassword) then
                    call cgi_get_named_integer(QUERY_STRING, 'P', device, ierr)
                    if (device/=adminPassword) then
                        call html_login(fname, mesg='Password is incorrect.')
                        return
                    end if
                end if
                UserList(targetUser)%Session = 0
            end if

        ! already logged out but request is not fnLogin? (old page submitted)
        else if (REQUEST/=fnLogin) then
            if (UserList(targetUser)%Session==-1) then ! true=logged out
                call html_login(fname, mesg='Already logged out. Please login again.')
                return
            else
                UserList(targetUser)%Session = 0
            end if
        end if

        ! Open and write HTML headers to the response file (fname)
        device = 100+REQUEST
        call open_for_write(device, fname)

        ! Write response to REQUEST
        select case (REQUEST)

            case (fnLogin)
                call html_college_links(device, CollegeIdxUser)

            case (fnStopUserByAdmin)
                call cgi_get_named_string(QUERY_STRING, 'A1', logoutUSER, ierr)
                logoutUSERidx = index_to_user(logoutUSER)
                UserList(logoutUSERidx)%Session = -1
                call html_college_links(device, CollegeIdxUser, 'Forcibly logged out '//logoutUSER)

            case (fnSearch)
                call object_search (device)

!            case (fnCollegeSelect)
!                call list_colleges(device)

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

!            case (fnRebuildChecklists)
!                    call RebuildChecklists(device, fname)

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

            case default
                targetCollege = CollegeIdxUser
                targetDepartment = DeptIdxUser
                TermQualifier = SPACE
                call html_write_header(device, 'Functionality not available', &
                    '<br><hr>The functionality you requested is not available in this version of '//PROGNAME//' .')

        end select

        call html_write_footer(device)

        return
    end subroutine server_respond


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
                    call html_write_header(device, 'Functionality not available', &
                    '<br><hr>The functionality you requested is not available in this version of '//PROGNAME//' .')

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
            '<input type="hidden" name="U" value="'//trim(itoa(targetUser))//'">', &
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
