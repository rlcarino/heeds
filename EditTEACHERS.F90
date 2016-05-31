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


module EditTEACHERS

    use HTML

    implicit none

contains


    subroutine teacher_edit(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_USERNAME) :: tTeacher, tAction
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer :: ierr, i, j, iTeach
        character (len=255) :: header, remark, tmpRHS
        type (TYPE_TEACHER) :: wrk
        logical :: isDirtyTEACHERS, criticalErr
        !character (len=MAX_LEN_PASSWD_VAR) :: Password
        !integer :: lenP

        call html_comment('teacher_edit()')

        isDirtyTEACHERS = .false.
        remark = SPACE

        ! which teacher ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, iTeach)
        if (iTeach/=0 .or. tTeacher==SPACE) tTeacher = 'Guest'
        iTeach = index_to_teacher(tTeacher)

        targetTeacher = iTeach
        targetDepartment = Teacher(targetTeacher)%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx

        if (REQUEST==fnGenerateTeacherPassword) then
            call set_password(Teacher(iTeach)%Password)
            call teacher_details_write(unitXML, dirTEACHERS, iTeach)
            call log_teacher_record_change(iTeach, 'Changed password')
            call teacher_info(device, Teacher(iTeach), 'Edit info for teacher '//tTeacher, &
                SPACE, 'Update', iTeach)
            return
        end if

        wrk = Teacher(iTeach)

        ! check for requested action
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)

        if (ierr/=0 .or. tAction==SPACE) then ! no action; display existing info

            if (trim(tTeacher)=='Guest') then
                header = 'Add new teacher'
                tAction = 'Add'
            else
                header = 'Edit info for teacher '//tTeacher
                tAction = 'Update'
            end if

            call teacher_info(device, wrk, header, remark, tAction, iTeach)

        else if (isRoleOfficial) then

            header = 'Edit info for teacher '//tTeacher
            remark = 'Edit teacher "'//trim(tTeacher)//'" failed. '//sorryMessageOfficial
            call teacher_info(device, wrk, header, remark, tAction, iTeach)

        else ! action is Add or Update; collect changes

            call cgi_get_named_string(QUERY_STRING, 'Login', wrk%TeacherId, ierr)
            !write(*,*) 'ierr=', ierr, ', Login=', wrk%TeacherId
            if (ierr/=0) wrk%TeacherId = Teacher(iTeach)%TeacherId
            if (wrk%TeacherId /= Teacher(iTeach)%TeacherId) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Username='//wrk%TeacherId
                call log_teacher_record_change(iTeach, 'TeacherId changed to '//wrk%TeacherId)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Name', tmpRHS, ierr)
            wrk%Name = tmpRHS
            !write(*,*) 'ierr=', ierr, ', Name=', wrk%Name
            if (ierr/=0) wrk%Name = Teacher(iTeach)%Name
            if (wrk%Name /= Teacher(iTeach)%Name) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Name='//wrk%Name
                call log_teacher_record_change(iTeach, 'Name changed to '//wrk%Name)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Bachelor', tmpRHS, ierr)
            wrk%Bachelor = tmpRHS
            !write(*,*) 'ierr=', ierr, ', Bachelor=', wrk%Bachelor
            if (ierr/=0) wrk%Bachelor = Teacher(iTeach)%Bachelor
            if (wrk%Bachelor /= Teacher(iTeach)%Bachelor) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Bachelor='//wrk%Bachelor
                call log_teacher_record_change(iTeach, 'Bachelor changed to '//wrk%Bachelor)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Master', tmpRHS, ierr)
            wrk%Master = tmpRHS
            !write(*,*) 'ierr=', ierr, ', Master=', wrk%Master
            if (ierr/=0) wrk%Master = Teacher(iTeach)%Master
            if (wrk%Master /= Teacher(iTeach)%Master) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Master='//wrk%Master
                call log_teacher_record_change(iTeach, 'Master changed to '//wrk%Master)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Doctorate', tmpRHS, ierr)
            wrk%Doctorate = tmpRHS
            !write(*,*) 'ierr=', ierr, ', Doctorate=', wrk%Doctorate
            if (ierr/=0) wrk%Doctorate = Teacher(iTeach)%Doctorate
            if (wrk%Doctorate /= Teacher(iTeach)%Doctorate) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Doctorate='//wrk%Doctorate
                call log_teacher_record_change(iTeach, 'Doctorate changed to '//wrk%Doctorate)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Specialization', tmpRHS, ierr)
            wrk%Specialization = tmpRHS
            !write(*,*) 'ierr=', ierr, ', Specialization=', wrk%Specialization
            if (ierr/=0) wrk%Specialization = Teacher(iTeach)%Specialization
            if (wrk%Specialization /= Teacher(iTeach)%Specialization) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Specialization='//wrk%Specialization
                call log_teacher_record_change(iTeach, 'Specialization changed to '//wrk%Specialization)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Role', wrk%Role, ierr)
            !write(*,*) 'ierr=', ierr, ', Role=', wrk%Role
            if (ierr/=0) wrk%Role = Teacher(iTeach)%Role
            if (wrk%Role /= Teacher(iTeach)%Role) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Role='//wrk%Role
                call log_teacher_record_change(iTeach, 'Role changed to '//wrk%Role)
            end if

            call cgi_get_named_integer(QUERY_STRING, 'Load', wrk%MaxLoad, ierr)
            !write(*,*) 'ierr=', ierr, ', MaxLoad=', wrk%MaxLoad
            if (ierr/=0 .or. wrk%MaxLoad<=0) wrk%MaxLoad = Teacher(iTeach)%MaxLoad
            if (wrk%MaxLoad /= Teacher(iTeach)%MaxLoad) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Max load='//itoa(wrk%MaxLoad)
                call log_teacher_record_change(iTeach, 'MaxLoad changed to '//itoa(wrk%MaxLoad))
            end if

            call cgi_get_named_integer(QUERY_STRING, 'Rank', wrk%Rank, ierr)
            !write(*,*) 'ierr=', ierr, ', Rank=', wrk%Rank
            if (ierr/=0 .or. wrk%Rank<=0) wrk%Rank = Teacher(iTeach)%Rank
            if (wrk%Rank /= Teacher(iTeach)%Rank) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Rank='//AcademicRank(wrk%Rank)
                call log_teacher_record_change(iTeach, 'Rank changed to '//AcademicRank(wrk%Rank) )
            end if

            call cgi_get_named_integer(QUERY_STRING, 'Step', wrk%Step, ierr)
            !write(*,*) 'ierr=', ierr, ', Step=', wrk%Step
            if (ierr/=0 .or. wrk%Step<=0) wrk%Step = Teacher(iTeach)%Step
            if (wrk%Step /= Teacher(iTeach)%Step) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Step='//RankStep(wrk%Step)
                call log_teacher_record_change(iTeach, 'Step changed to '//RankStep(wrk%Step))
            end if

            call cgi_get_named_string(QUERY_STRING, 'Department', tDepartment, ierr)
            wrk%DeptIdx = index_to_dept(tDepartment)
            !write(*,*) 'ierr=', ierr, ', DeptIdx=', wrk%DeptIdx
            if (ierr/=0 .or. wrk%DeptIdx<=0) wrk%DeptIdx = Teacher(iTeach)%DeptIdx
            if (wrk%DeptIdx /= Teacher(iTeach)%DeptIdx) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Unit='//Department(wrk%DeptIdx)%Code
                call log_teacher_record_change(iTeach, 'Unit changed to '//Department(wrk%DeptIdx)%Code)
            end if


            if (isDirtyTEACHERS) then  ! some changes

                if (wrk%TeacherId /= Teacher(iTeach)%TeacherId) then  ! new username
                    j = index_to_teacher(wrk%TeacherId)
                    if (j==0) then ! not used

                        if (trim(tAction)=='Add') then
                            call check_array_bound (NumTeachers+1, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS', criticalErr)
                            if (criticalErr) then
                                targetDepartment = DeptIdxUser
                                targetCollege = CollegeIdxUser
                                call html_college_links(device, targetCollege, &
                                    'No more space for additional teacher')
                                return
                            else
                                NumTeachers = NumTeachers+1
                                iTeach = NumTeachers
                                tTeacher = wrk%TeacherId
                                call set_password(wrk%Password, trim(wrk%TeacherId))
                                remark = ': Added new teacher'//remark

                            end if
                        else
                            remark = ': Updated teacher'//remark
                            ! copy new name to Student()%Adviser
                            do i=1,NumStudents+NumAdditionalStudents
                                if (Student(i)%Adviser==Teacher(iTeach)%TeacherId) Student(i)%Adviser = wrk%TeacherId
                            end do
                        end if

                        Teacher(iTeach) = wrk
                        call sort_alphabetical_teachers()
                        call teacher_details_write(unitXML, trim(dirTEACHERS)//'index', 1, NumTeachers)

                    else
                        remark = ': Invalid username: "'//trim(wrk%TeacherId)//'" is already taken by '//Teacher(j)%Name
                        isDirtyTEACHERS = .false.
                    end if

                else ! same username
                    remark = ': Updated teacher'//remark
                    Teacher(iTeach) = wrk
                end if

                if (trim(tAction)=='Add') then
                    header = 'Add new teacher'
                else
                    header = 'Edit info for teacher '//tTeacher
                end if
                call teacher_info(device, wrk, header, remark(3:), tAction, iTeach)

                if (isDirtyTEACHERS) then
                    call teacher_details_write(unitXML, dirTEACHERS, iTeach)
                end if

            else ! Add or Update clicked, but no changes made
                remark = ': No changes made?'
                call html_college_links(device, Department(wrk%DeptIdx)%CollegeIdx, &
                    trim(tTeacher)//remark)
            end if

        end if

    end subroutine teacher_edit


    subroutine teacher_delete (device)
        integer, intent (in) :: device
        integer :: n_count, nsect(3), iSect, iTeach, iTerm, iStd
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        character(len=MAX_LEN_USERNAME) :: tTeacher
        character (len=127) :: mesg
        integer, dimension(60,7) :: TimeTable

        call html_comment('teacher_delete()')

        ! which teacher ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, iTeach)
        iTeach = index_to_teacher(tTeacher)
        if (iTeach==0) then
            call html_college_links(device, CollegeIdxUser, mesg='Teacher "'//trim(tTeacher)//'" not found.')
            return
        end if

        if (isRoleOfficial) then
            call html_college_links(device, CollegeIdxUser, mesg='Teacher "'//trim(tTeacher)// &
                '" not deleted. '//sorryMessageOfficial)
            return
        end if

        ! count classes handled by teacher
        nsect = 0
        mesg = SPACE
        do iTerm=firstSemester,summerTerm
            call timetable_clear(TimeTable)
            do iSect=1,NumSections(iTerm)
                call meetings_of_section_by_teacher(iTerm, iSect, iTeach, n_meetings, meetings)
                if (n_meetings==0) cycle ! teacher not assigned to this section
                nsect(iTerm) = nsect(iTerm)+1
            end do
            if (nsect(iTerm)>0) then
                mesg = trim(mesg)//COMMA//SPACE//txtSemester(iTerm+6)
            end if
        end do
        if (sum(nsect)>0) then
            mesg = 'Delete "'//trim(tTeacher)//'" failed; has classes during '//trim(mesg(2:))//' term.'
            call html_college_links(device, CollegeIdxUser, mesg)
            return
        end if
        ! check if Adviser
        n_count = 0
        do iStd=1,NumStudents+NumAdditionalStudents
            if (Student(iStd)%Adviser==tTeacher) then
                n_count = n_count+1
                call html_comment(itoa(n_count)//Student(iStd)%StdNo//trim(Student(iStd)%Name)//Student(iStd)%Adviser)
            end if
        end do
        if (n_count>0) then
            mesg = 'Delete "'//trim(tTeacher)//'" failed; has '//itoa(n_count)//' advisees.'
            call html_college_links(device, CollegeIdxUser, mesg)
            return
        end if

        call log_teacher_record_change(iTeach, trim(tTeacher)//' deleted.')

        call initialize_teacher(Teacher(iTeach))
        Teacher(iTeach)%DeptIdx = 0
        call teacher_details_write(unitXML, trim(dirTEACHERS)//'index', 1, NumTeachers)
        call html_college_links(device, CollegeIdxUser, mesg='Teacher "'//trim(tTeacher)//'" deleted.')

    end subroutine teacher_delete


    subroutine teacher_list_all (device, fn)
        integer, intent (in) :: device, fn
        integer :: iTeach, nfacs, sdx, tdx,  ierr
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        !character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=127) :: mesg, stats
        !character(len=1) :: ch

        call html_comment('teacher_list_all()')

        ! collect teachers
        tArray = 0
        nfacs = 0
        select case (fn)

            case (fnFindTeacher)
                call cgi_get_named_string(QUERY_STRING, 'A1', stats, ierr)
                sdx = len_trim(stats)
                if (sdx>0) then
                    do tdx=1,NumTeachers+NumAdditionalTeachers
                        iTeach = TeacherRank(tdx)
                        if (index(Teacher(iTeach)%Name,stats(:sdx))==0) then
                            call upper_case(stats)
                            if (index(Teacher(iTeach)%Name,stats(:sdx))==0) cycle
                        end if
                        if (Teacher(iTeach)%Role==SYSAD .and. .not. isRegistrar) cycle
                        nfacs = nfacs+1
                        tArray(nfacs) = iTeach
                    end do
                    mesg = 'Teachers with "'//stats(:sdx)//'" in name.'
                else
                    mesg = 'Search teacher failed: search string not specified.'
                end if

            case (fnOnlineTeachers)
                do tdx=1,NumTeachers+NumAdditionalTeachers
                    iTeach = TeacherRank(tdx)
                    if (Teacher(iTeach)%OnlineStatus(1)<=0) cycle
                    if (Teacher(iTeach)%Role==SYSAD .and. .not. isRegistrar) cycle
                    nfacs = nfacs+1
                    tArray(nfacs) = iTeach
                end do
                mesg = 'Online teachers'

            case (fnTeachersByDept)
                ! which unit ?
                call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)
                targetDepartment = index_to_dept(tDepartment)
                targetCollege = Department(targetDepartment)%CollegeIdx
                do tdx=1,NumTeachers+NumAdditionalTeachers
                    iTeach = TeacherRank(tdx)
                    if (targetDepartment/=Teacher(iTeach)%DeptIdx) cycle
                    if (Teacher(iTeach)%Role==SYSAD .and. .not. isRegistrar) cycle
                    nfacs = nfacs+1
                    tArray(nfacs) = iTeach
                end do
                mesg = 'Teachers in '//tDepartment

        end select

        call html_teacher_list (device, fn, nfacs, tArray(0:nfacs), mesg, SPACE)

    end subroutine teacher_list_all


    subroutine teacher_conflicts (device, thisTerm)
        integer, intent (in) :: device, thisTerm
        integer :: iTeach, nfacs, nsect, iMeet, sdx, tdx, ierr, iSect
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        !character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=127) :: mesg
        integer, dimension(60,7) :: TimeTable
        !character(len=1) :: ch
        real :: totalLect, lectHours, totalLab, labHours, totalUnits, meetingUnits

        call html_comment('teacher_conflicts()')

        ! collect teachers
        tArray = 0
        nfacs = 0

        ! which college ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        targetCollege = index_to_college(tCollege)
        ! collect teachers
        do tdx=1,NumTeachers+NumAdditionalTeachers
            iTeach = TeacherRank(tdx)
            if (Department(Teacher(iTeach)%DeptIdx)%CollegeIdx/=targetCollege) cycle ! not in college
            ! collect classes of teacher
            call timetable_clear(TimeTable)
            nsect = 0 ! how many conflicts
            do iSect=1,NumSections(thisTerm)
                call meetings_of_section_by_teacher(thisTerm, iSect, iTeach, n_meetings, meetings)
                if (n_meetings>0) then ! teacher assigned to this section
                    ierr = -10
                    call timetable_add_meetings_of_section( thisTerm, iSect, n_meetings, meetings, TimeTable, ierr)
                    if (ierr /= -10) then ! conflict
                        nsect = nsect+1
                        exit
                    end if
                end if
            end do
            if (nsect>0) then ! conflict
                nfacs = nfacs + 1
                tArray(nfacs) = iTeach
            end if
        end do
        mesg = 'Teachers with schedule conflicts in '//tCollege

        call html_write_header(device, mesg)

        if (nfacs == 0) then
            write(device,AFORMAT) BRNONEHR
        else
            ! sort teachers
            do tdx=1,nfacs-1
                do sdx=tdx+1,nfacs
                    if (Teacher(tArray(sdx))%Name<Teacher(tArray(tdx))%Name) then
                        iTeach =tArray(sdx)
                        tArray(sdx) = tArray(tdx)
                        tArray(tdx) = iTeach
                    end if
                end do
            end do

            write(device,AFORMAT) '<table border="0" width="90%">'//&
                b_tr//b_thal//'Name (Specialization)'//e_th// &
                b_thac//PROGNAME//' Role'//e_th// &
                b_thac//'No. of'//linebreak//'classes'//e_th// &
                b_thac//'Lect'//linebreak//'hours'//e_th// &
                b_thac//'Lab'//linebreak//'hours'//e_th// &
                b_thac//'Teaching'//linebreak//'load'//e_th// &
                b_thac//'Remark'//e_th//e_tr

            do tdx=1,nfacs
                iTeach = tArray(tdx)
                ! check conflicts
                mesg = SPACE
                call timetable_clear(TimeTable)
                ! collect classes of teacher
                nsect = 0
                totalUnits = 0.0
                totalLect = 0.0
                totalLab = 0.0
                do iSect=1,NumSections(thisTerm)
                    call meetings_of_section_by_teacher(thisTerm, iSect, iTeach, n_meetings, meetings)
                    if (n_meetings>0) then ! teacher assigned to this section
                        nsect = nsect+1
                        tArray(nfacs+nsect) = iSect
                        do iMeet=1,n_meetings
                            call class_hours_and_load(iMeet, iSect, thisTerm, meetingUnits, lectHours, labHours)
                            totalLect = totalLect + lectHours
                            totalLab = totalLab + labHours
                            totalUnits = totalUnits + meetingUnits
                        end do
                        ierr = -10
                        call timetable_add_meetings_of_section(thisTerm, iSect, n_meetings, meetings, TimeTable, ierr)
                        if (ierr /= -10) then
                            mesg = red//'Conflict!'//e_color
                        end if
                    end if
                end do
                QUERY_put = Teacher(iTeach)%TeacherId

                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(tdx,2))//'">'// &
                    b_td//trim(itoa(tdx))//'. '//trim(Teacher(iTeach)%Name)// &
                    ' ('//trim(Teacher(iTeach)%Specialization)//')'
                if ( isRole_dean_of_college(Department(Teacher(iTeach)%DeptIdx)%CollegeIdx, orHigherUp) ) then
                    write(device,AFORMAT) trim(make_href(fnEditTeacher, 'Edit', &
                        A1=QUERY_put, pre=nbsp//b_small, post=e_small))
                end if
                write(device,AFORMAT) e_td//b_tdac//trim(Teacher(iTeach)%Role)//e_td, &
                    b_tdac//itoa(nsect)//trim(make_href(fnTeacherEditSchedule, 'Edit', &
                    A1=QUERY_put, A9=thisTerm, pre=b_small, post=e_small))
                write(device,'(2(a,f5.1), a,f5.2,a)') e_td//b_tdac, totalLect, &
                    e_td//b_tdac, totalLab, &
                    e_td//b_tdac, totalUnits, &
                    '/'//trim(itoa(Teacher(iTeach)%MaxLoad))// &
                    trim(make_href(fnPrintableWorkload, 'Printable', &
                    A1=QUERY_put, A9=thisTerm, pre=nbsp//b_small, post=e_small, newtab='"_blank"'))//e_td// &
                    b_tdac//trim(mesg)//e_td//e_tr
            end do
            write(device,AFORMAT) e_table//horizontal

        end if

    end subroutine teacher_conflicts


    subroutine teacher_schedule(device, thisTerm, LoadSource)
        integer, intent(in), optional :: LoadSource
        integer, intent (in) :: device, thisTerm
        integer :: iMeet, tLen1, tLen2, ierr, LoadFromDept, iTeach, iSect, iDept
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_USERNAME) :: tTeacher
        character(len=MAX_LEN_CLASS_ID) :: tAction, tClassId
        integer, dimension(60,7) :: TimeTable
        logical :: conflicted, assigned, allowed_to_edit, allowed_to_show
        character(len=255) :: mesg

        call html_comment('teacher_schedule()')

        ! which teacher?
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, ierr)
        iTeach = index_to_teacher(tTeacher)
        if (iTeach==0) then
            call html_college_links(device, CollegeIdxUser, mesg='Teacher "'//trim(tTeacher)//'" not found.')
            return
        end if
        targetTeacher = iTeach
        targetDepartment = Teacher(targetTeacher)%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx
        mesg = SPACE
        allowed_to_edit = isRole_admin_of_college(targetCollege) .or. &
            ( isRole_chair_of_department(targetDepartment,orHigherUp) .and. &
              College(targetCollege)%isAllowed(ToEditCLASSES,thisTerm) )
        allowed_to_show = College(targetCollege)%isAllowed(ToShowTEACHERS,thisTerm) .or. &
                   isRole_chair_of_department(targetDepartment, orHigherUp)

        ! check if there are other arguments
        call cgi_get_named_string(QUERY_STRING, 'A2', tAction, ierr)
        if (ierr==0) then ! action is Add or Del
            call cgi_get_named_string(QUERY_STRING, 'A3', tClassId, ierr)
            iSect = index_to_section(tClassId, thisTerm)
            if (iSect>0 .and. .not. isRoleofficial) then ! target of action is indexed by iSect
                LoadFromDept = Section(thisTerm,iSect)%DeptIdx
                if (tAction=='Add') then
                    do iMeet=1,Section(thisTerm,iSect)%NMeets
                        Section(thisTerm,iSect)%TeacherIdx(iMeet) = targetTeacher
                    end do
                    mesg = 'Added '//tClassId
                end if
                if (tAction=='Del') then
                    do iMeet=1,Section(thisTerm,iSect)%NMeets
                        Section(thisTerm,iSect)%TeacherIdx(iMeet) = 0
                    end do
                    mesg = 'Deleted '//tClassId
                end if
                call class_details_write(unitXML, thisTerm, dirCLASSES(thisTerm), iSect)
            end if

            if (isRoleOfficial) then
                mesg = '"'//trim(tAction)//SPACE//trim(tClassId)//'" failed. '//sorryMessageOfficial
            end if
        end if

        call html_write_header(device, 'Teaching schedule of '//trim(Teacher(targetTeacher)%Name), mesg)

        if (.not. allowed_to_show) then
            write(device,AFORMAT) b_para, trim(sorryMessageSchedules), e_para, horizontal
            return
        end if

        ! collect meetings of teacher targetTeacher
        call timetable_meetings_of_teacher(thisTerm, targetTeacher, 0, tLen1, tArray, TimeTable, conflicted)
        !write(*, '(3i6)') (tArray(iMeet), iMeet=1,tLen1)
        !if (conflicted) write(*,*) 'Conflict in schedule '//trim(Teacher(targetTeacher)%Name)
        call list_sections_to_edit(device, thisTerm, tLen1, tArray, fnTeacherEditSchedule, &
            tTeacher, 'Del', allowed_to_edit, allowed_to_show)
        !write(*, '(3i6)') (tArray(iMeet), iMeet=1,tLen1)
        if (tLen1>0) call timetable_display(device, thisTerm, TimeTable)

        ! make list of TBA sections LoadSource that fit the schedule of teacher
        if (present(LoadSource)) then
            LoadFromDept = LoadSource
        else
            call cgi_get_named_string(QUERY_STRING, 'A4', tDepartment, ierr)
            LoadFromDept = index_to_dept(tDepartment)
            if (ierr/=0 .or. LoadFromDept<=0) then
                LoadFromDept = targetDepartment
            else
                mesg = 'Searched for feasible classes in '//tDepartment
            end if
        end if

        tLen2 = 0
        do iSect=1,NumSections(thisTerm)
            if (LoadFromDept/=Section(thisTerm,iSect)%DeptIdx) cycle ! not in this unit
            if (Section(thisTerm,iSect)%NMeets==1 .and. Section(thisTerm,iSect)%DayIdx(1)==0) cycle ! meeting days/time not specified
            ! teacher(s) already assigned to this section?
            assigned = .false.
            do iMeet=1,Section(thisTerm,iSect)%NMeets
                if (Section(thisTerm,iSect)%TeacherIdx(iMeet)/=0) assigned = .true.
            end do
            if (assigned) cycle ! section has a teacher
            if (.not. is_conflict_timetable_with_section(thisTerm, iSect, TimeTable)) then ! add to list
                do iMeet=1,Section(thisTerm,iSect)%NMeets
                    tArray(tLen1+tLen2+1) = iSect
                    tArray(tLen1+tLen2+2) = iMeet
                    tArray(tLen1+tLen2+3) = 0
                    tLen2 = tLen2+3
                end do
            end if
        end do
        tArray(tLen1+tLen2+1) = 0
        tArray(tLen1+tLen2+2) = 0
        tArray(tLen1+tLen2+3) = 0
        if (tLen2>0) then
            write(device,AFORMAT) horizontal
            call list_sections_to_edit(device, thisTerm, tLen2, tArray(tLen1+1), &
                fnTeacherEditSchedule, tTeacher, 'Add', allowed_to_edit, allowed_to_show, &
                b_bold//'Classes with TBA teachers in '//trim(Department(LoadFromDept)%Code)// &
                ' that fit the schedule of '//trim(Teacher(targetTeacher)%Name)//e_bold)
        end if

        ! search for feasible classes in another unit?
        if ( .not. (isRoleStudent .or. isRoleGuest) ) then
            call make_form_start(device, fnTeacherEditSchedule, tTeacher, A9=thisTerm)
            write(device,AFORMAT) linebreak//'Search for feasible classes in : <select name="A4">'
            do iDept=2,NumDepartments
                if (iDept/=LoadFromDept) then
                    ierr = 0
                else
                    ierr = 1
                end if
                write(device,AFORMAT) '<option value="'//trim(Department(iDept)%Code)//'"'//trim(selected(ierr))//'> '// &
                    trim(Department(iDept)%Code)//DASH//trim(Department(iDept)%Name)
            end do
            write(device,AFORMAT) '</select>'//nbsp//'<input type="submit" value="Find classes">'//e_form
        end if
        write(device,AFORMAT) horizontal

    end subroutine teacher_schedule


    subroutine teacher_info(device, wrk, header, remark, tAction, iTeach)
        integer, intent(in) :: device, iTeach
        type (TYPE_TEACHER), intent(in) :: wrk
        character (len=*), intent(in)  :: header, remark, tAction
        character(len=MAX_LEN_USERNAME) :: tTeacher
        character (len=MAX_LEN_PASSWD_VAR) :: Password
        integer :: i, j, k, nClasses(3)

        call html_comment('teacher_info()')

        tTeacher = wrk%TeacherId
        targetDepartment = wrk%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx
        call getPassword_of_teacher(iTeach, Password)

        call html_write_header(device, header, remark)

        if (isRole_dean_of_college(targetCollege,orHigherUp) ) then
            call count_teacher_load(iTeach, 0, nClasses)
            if (nClasses(currentTerm)>0) then
                write(device,AFORMAT) &
                    trim(make_href(fnEvaluationDuties, 'evaluation duties', &
                        A1=tTeacher, A9=currentTerm, pre=b_italic//'Assign'//nbsp, post=e_italic//DOT//nbsp//nbsp))
            end if
        end if

        write(device,AFORMAT) &
            trim(make_href(fnTeachersByDept, 'other', &
                A1=Department(targetDepartment)%Code, pre=b_italic//'Edit'//nbsp, post=' teacher.'//e_italic//linebreak))

        call make_form_start(device, fnEditTeacher, tTeacher)

        write(device,AFORMAT) '<table border="0" width="100%">', &
            b_tr//b_td//'Username '//e_td//b_td//'<input name="Login" size="'//trim(itoa(MAX_LEN_USERNAME))// &
            '" value="'//trim(tTeacher)//'">'//e_td//e_tr, &
            b_tr//b_td//'Teacher name '//e_td//b_td//'<input name="Name" size="'//trim(itoa(MAX_LEN_PERSON_NAME))// &
            '" value="'//trim(wrk%Name)//'">'//e_td//e_tr
        ! dept
        write(device,AFORMAT) &
            b_tr//b_td//'Unit '//e_td//b_td//'<select name="Department">'
        do i=2,NumDepartments
            if (i/=targetDepartment) then
                j=0
            else
                j=1
            end if
            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(Department(i)%Code)//'">'// &
                trim(Department(i)%Name)
        end do
        write(device,AFORMAT) '</select>'//e_td//e_tr
        ! rank
        write(device,AFORMAT) &
            b_tr//b_td//'Rank '//e_td//b_td//'<select name="Rank">'
        do i=0,4
            if (i/=wrk%Rank) then
                j=0
            else
                j=1
            end if
            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(itoa(i))//'">'// &
                trim(AcademicRank(i))
        end do
        write(device,AFORMAT) '</select>'//e_td//e_tr
        ! step
        write(device,AFORMAT) &
            b_tr//b_td//'Step '//e_td//b_td//'<select name="Step">'
        do i=0,12
            if (i/=wrk%Step) then
                j=0
            else
                j=1
            end if
            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(itoa(i))//'">'// &
                trim(RankStep(i))
        end do
        write(device,AFORMAT) '</select>'//e_td//e_tr
        ! bachelor
        write(device,AFORMAT) &
            b_tr//b_td//'Bachelor '//e_td//b_td//'<input name="Bachelor" size="'// &
            trim(itoa(MAX_LEN_TEACHER_DEGREE))//'" value="'//trim(wrk%Bachelor)//'">'//e_td//e_tr
        ! master
        write(device,AFORMAT) &
            b_tr//b_td//'Master '//e_td//b_td//'<input name="Master" size="'// &
            trim(itoa(MAX_LEN_TEACHER_DEGREE))//'" value="'//trim(wrk%Master)//'">'//e_td//e_tr
        ! doctorate
        write(device,AFORMAT) &
            b_tr//b_td//'Doctorate '//e_td//b_td//'<input name="Doctorate" size="'// &
            trim(itoa(MAX_LEN_TEACHER_DEGREE))//'" value="'//trim(wrk%Doctorate)//'">'//e_td//e_tr
        ! specialization
        write(device,AFORMAT) &
            b_tr//b_td//'Specialization '//e_td//b_td//'<input name="Specialization" size="'// &
            trim(itoa(MAX_LEN_TEACHER_DEGREE))//'" value="'//trim(wrk%Specialization)//'">'//e_td//e_tr
        ! max load
        write(device,AFORMAT) &
            b_tr//b_td//'Max load '//e_td//b_td//'<input name="Load" size="3" value="'// &
            trim(itoa(wrk%MaxLoad))//'">'//e_td//e_tr

        ! Role
        if ( isRole_dean_of_college(Department(Teacher(iTeach)%DeptIdx)%CollegeIdx, orHigherUp) ) then

!        GUEST      = 'Guest     ', & ! 0-Cannot change anything
!        FACULTY    = 'Faculty   ', & ! 1-Faculty - can edit own classlists, enter own grades, advise students assigned by Dean
!        CHAIR      = 'Chair     ', & ! 2-Chair of Department(Teacher()%DeptIdx); also a Faculty
!        DEAN       = 'Dean      ', & ! 3-Dean of College(Department(Teacher()%DeptIdx)%CollegeIdx); also a Chair
!        STAFF      = 'Staff     ', & ! 4-Registrar's staff for a college
!        OFFICIAL   = 'Official  ', & ! 5-High-ranking official of the University
!        SYSAD      = 'ADMIN     ', & ! 6-Registrar; also a Dean
!        BENEFACTOR = 'Benefactor'    ! 7-Scholarship

            k = -1
            if (isRoleDean)  k = 3
            if (isRoleSysAd) k = 4
            if (isRegistrar .or. isRoleOfficial) k = MAX_ALL_ROLES
            write(device,AFORMAT) &
                b_tr//b_td//'Role '//e_td//b_td//'<select name="Role">'
            do i=0,k
                if (txtRole(i)/=wrk%Role) then
                    j=0
                else
                    j=1
                end if
                write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(txtRole(i))//'">'//txtRole(i)
            end do
            write(device,AFORMAT) '</select>'//e_td//e_tr

            if ( (isRoleSysAd .and. wrk%Role/=SYSAD) .or. isRegistrar) then
                write(device,AFORMAT) b_tr//b_td//'Password'//e_td//b_td//trim(Password)//  &
                    trim(make_href(fnGenerateTeacherPassword, 'Reset password', &
                        A1=tTeacher, pre=nbsp//nbsp//b_small, post=e_small)), &
                    e_td//e_tr
            end if
        else
            write(device,AFORMAT) &
                b_tr//b_td//'Role '//e_td//b_td//trim(wrk%Role),  &
                e_td//e_tr

        end if

        write(device,AFORMAT) &
            e_table//linebreak//nbsp//'<input name="action" type="submit" value="'//trim(tAction)//'"> ', &
            b_italic//'(or, select another action below)'//e_italic//e_form//horizontal

        if ( isRole_admin_of_college(targetCollege) ) then
            call make_form_start(device, fnDeleteTeacher, tTeacher)
            write(device,AFORMAT) '<table border="0" cellpadding="0" cellspacing="0">', &
                b_tr//b_td//b_bold//'Delete teacher '//e_bold//tTeacher// &
                '<input type="submit" name="action" value="Delete">'//e_form// &
                e_td//e_tr//e_table//horizontal
        end if

        call make_form_start(device, fnFindTeacher)
        write(device,AFORMAT) '<table border="0" cellpadding="0" cellspacing="0">', &
            b_tr//b_td// &
            b_bold//'Search for teacher'//e_bold//' with <input name="A1" value=""> in name. ', &
            '<input type="submit" name="action" value="Search">'//e_form// &
            e_td//e_tr//e_table//horizontal

    end subroutine teacher_info


    subroutine teacher_schedule_printable(device, thisTerm)
        integer, intent (in) :: device, thisTerm
        integer :: tLen1, ierr
        character(len=MAX_LEN_USERNAME) :: tTeacher
        character(len=MAX_LEN_PERSON_NAME) :: tName
        integer, dimension(60,7) :: TimeTable
        logical :: conflicted

        call html_comment('teacher_schedule_printable()')

        ! which teacher?
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, ierr)
        targetTeacher = index_to_teacher(tTeacher)
        if (targetTeacher==0) then
            call html_college_links(device, CollegeIdxUser, mesg='Teacher "'//trim(tTeacher)//'" not found.')
            return
        end if

        ! collect meetings of teacher targetTeacher
        call timetable_meetings_of_teacher(thisTerm, targetTeacher, 0, tLen1, tArray, TimeTable, conflicted)
        if (conflicted) then
            targetCollege = CollegeIdxUser
            targetDepartment = DeptIdxUser
            call html_write_header(device, 'Teacher schedule', linebreak//horizontal// &
                'Teacher "'//tTeacher//'": conflict in schedule')
            return
        end if

        targetDepartment = Teacher(targetTeacher)%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx

        write(device,AFORMAT) &
            '<html><head><title>'//trim(UniversityCode)//SPACE//PROGNAME//VERSION// &
            '</title></head><body>', '<table border="0" width="100%">', &
            b_tr//b_tdac//'Republic of the Philippines'//e_td//e_tr, &
            b_tr//b_tdac//b_bold//trim(UniversityName)//e_bold//e_td//e_tr, &
            b_tr//b_tdac//trim(UniversityAddress)//e_td//e_tr, &
            b_tr//b_td//linebreak//e_td//e_tr, &
            b_tr//b_tdac//trim(College(targetCollege)%Name)//e_td//e_tr, &
            b_tr//b_tdac//b_bold//'INDIVIDUAL FACULTY TEACHING LOAD'//e_bold//e_td//e_tr, &
            b_tr//b_tdac//trim(text_term_school_year(thisTerm+3,currentYear))//e_td//e_tr, &
            b_tr//b_td//linebreak//e_td//e_tr, &
            e_table

        tName = Teacher(targetTeacher)%Name
        ierr = index(tName, COMMA//SPACE)
        if (ierr>0) then ! switch first & last names
            tName = trim(tName(ierr+2:))//SPACE//tName(:ierr-1)
        end if

        write(device,AFORMAT) '<table border="0" width="100%">', &
            b_tr//'<td width="50%">Name:  '//b_bold//trim(tName)//e_bold//e_td, &
            '<td width="50%">Area of Retraining: '//trim(Teacher(targetTeacher)%Specialization)//e_td//e_tr
        ! bachelor
        write(device,AFORMAT) &
            b_tr//'<td width="50%">Bachelor: '//trim(Teacher(targetTeacher)%Bachelor)//e_bold//e_td, &
            '<td width="50%">Faculty Rank: '//trim(AcademicRank(Teacher(targetTeacher)%Rank))//nbsp// &
            trim(RankStep(Teacher(targetTeacher)%Step))//e_td//e_tr
        ! master
        write(device,AFORMAT) &
            b_tr//'<td width="50%">Master: '//trim(Teacher(targetTeacher)%Master)//e_bold//e_td, &
            '<td width="50%">Length of Service in University: '//nbsp//e_td//e_tr
        ! doctorate
        write(device,AFORMAT) &
            b_tr//'<td width="50%">Doctorate: '//trim(Teacher(targetTeacher)%Doctorate)//e_bold//e_td, &
            b_td_nbsp_e_td//e_tr
        write(device,AFORMAT) e_table//horizontal

        if (.not. (College(targetCollege)%isAllowed(ToShowTEACHERS,thisTerm) .or. &
                   isRole_chair_of_department(targetDepartment, orHigherUp)) ) then
            write(device,AFORMAT) trim(sorryMessageSchedules), horizontal
        else
            call teacher_workload(device, thisTerm, tLen1, tArray)

        end if


        write(device,AFORMAT) linebreak//linebreak//linebreak//linebreak//'<table border="0" width="100%">', &
            b_tr//'<td width="50%">Prepared by:'//linebreak//linebreak//linebreak//linebreak//linebreak//linebreak// &
            trim(College(targetCollege)%Dean)// &
            linebreak//'College Dean, '//trim(College(targetCollege)%Code)//linebreak//linebreak//linebreak//e_td, &
            '<td width="50%">Received by:'//linebreak//linebreak//linebreak//linebreak//linebreak//linebreak// &
            trim(tName)// &
            linebreak//'Faculty'//linebreak//linebreak//linebreak//e_td//e_tr
        if (len_trim(DeanOfCampus)>0) then
            write(device,AFORMAT) &
                b_tr//'<td width="50%">Recommending Approval:'//linebreak//linebreak//linebreak//linebreak//linebreak// &
                    linebreak//trim(DeanOfCampus)// &
                linebreak//trim(titleDeanOfCampus)//linebreak//linebreak//linebreak//e_td, &
                '<td width="50%">Recommending Approval:'//linebreak//linebreak//linebreak//linebreak//linebreak//linebreak// &
                trim(DeanOfInstruction)// &
                linebreak//trim(titleDeanOfInstruction)//linebreak//linebreak//linebreak//e_td//e_tr
        else
            write(device,AFORMAT) &
                b_tr//'<td width="50%">Recommending Approval:'//linebreak//linebreak//linebreak//linebreak//linebreak// &
                    linebreak// &
                trim(DeanOfInstruction)// &
                linebreak//trim(titleDeanOfInstruction)//linebreak//linebreak//linebreak//e_td//b_td_nbsp_e_td//e_tr
        end if
        write(device,AFORMAT) &
            b_tr//'<td width="50%">Recommending Approval:'//linebreak//linebreak//linebreak//linebreak//linebreak//linebreak// &
            trim(VPAcademicAffairs)// &
            linebreak//trim(titleVPAcademicAffairs)//e_td, &
            '<td width="50%">Approved by:'//linebreak//linebreak//linebreak//linebreak//linebreak//linebreak// &
            trim(UniversityPresident)// &
            linebreak//trim(titleUniversityPresident)//e_td//e_tr
        write(device,AFORMAT) e_table

    end subroutine teacher_schedule_printable


    subroutine teacher_workload(device, thisTerm, lenSL, SectionList)
        integer, intent(in) :: device, thisTerm, lenSL, SectionList(3*lenSL+3)
        integer :: iSubj, idx, iMeet, rdx, previous, iSect
        real :: totalUnits, meetingUnits
        real :: lectHours, labHours, totalLect, totalLab
        logical :: sectionDone

        call html_comment('teacher_workload()')

        if (lenSL < 3) then
            write(device,AFORMAT) linebreak//'No teaching load?'//linebreak
            return
        end if

        totalUnits = 0.0
        totalLect = 0.0
        totalLab = 0.0

        write(device,AFORMAT) '<table border="0" width="100%">'//b_tr, &
            b_thal//'Subject'//linebreak//'Code'//e_th// &
            b_thal//'Descriptive Title (Units)'//e_th// &
            b_thal//'Program'//linebreak//'Year/Sec'//e_th// &
            b_thal//'Class'//linebreak//'Code'//e_th// &
            b_thal//'Class'//linebreak//'Size'//e_th // &
            b_thal//'Day'//e_th// &
            b_thal//'Time'//e_th// &
            b_thal//'Room'//e_th// &
            b_thal//'Hrs.'//linebreak//'Lect'//e_th// &
            b_thal//'Hrs.'//linebreak//'Lab'//e_th// &
            b_thal//'Work'//linebreak//'Load'//e_th// &
            e_tr

        previous = 0
        sectionDone = .false.
        do idx=1,lenSL,3
            iSect=SectionList(idx)
            iMeet=SectionList(idx+1)
            iSubj = Section(thisTerm,iSect)%SubjectIdx

            !new section ?
            if (iSect/=previous) then

                previous = iSect
                sectionDone = .false.

                !  subject
                write(device,AFORMAT) b_tr//b_td//trim(Subject(iSubj)%Name)//e_td
                !  subject title, credit
                write(device,'(a,f5.1,a)') b_td//trim(Subject(iSubj)%Title)//' (', Subject(iSubj)%Units, ')'//e_td
                ! block
                write(device,AFORMAT) b_td
                call blocks_in_section(device, iSect, 0, thisTerm)
                write(device,AFORMAT) e_td
                ! section code
                write(device,AFORMAT) b_td//trim(Section(thisTerm,iSect)%Code)//e_td
                ! class size
                write(device,AFORMAT) b_td//trim(itoa(Section(thisTerm,iSect)%Slots))//e_td

            else

                if (sectionDone) cycle

                write(device,AFORMAT) b_tr// &
                    b_td_nbsp_e_td// & ! subject code
                    b_td_nbsp_e_td// & ! title, credit
                    b_td_nbsp_e_td// & ! block
                    b_td_nbsp_e_td// & ! section code
                    b_td_nbsp_e_td     ! class size

            end if

            ! day(s)
            if (is_regular_schedule(iSect, thisTerm)) then
                write(device,AFORMAT) b_td//text_days_of_section(Section(thisTerm,iSect))//e_td
                call class_hours_and_load(-1, iSect, thisTerm, meetingUnits, lectHours, labHours)
                sectionDone = .true.
            else
                write(device,AFORMAT) b_td//txtDay(Section(thisTerm,iSect)%DayIdx(iMeet))//e_td
                call class_hours_and_load(iMeet, iSect, thisTerm, meetingUnits, lectHours, labHours)
            end if

            ! time
            write(device,AFORMAT) b_td//trim(text_time_period(Section(thisTerm,iSect)%bTimeIdx(iMeet), &
                Section(thisTerm,iSect)%eTimeIdx(iMeet)))//e_td
            ! room
            rdx = Section(thisTerm,iSect)%RoomIdx(iMeet)
            if (rdx > 0) then
                write(device,AFORMAT) b_td//trim(Room(rdx)%Code)//e_td
            else
                write(device,AFORMAT) b_td//'TBA'//e_td
            end if

            totalLect = totalLect + lectHours
            totalLab = totalLab + labHours
            totalUnits = totalUnits + meetingUnits

            if (lectHours>0.0) then
                write(device,'(a,f5.2,a)') b_td, lectHours, e_td ! lect hours
            else
                write(device,AFORMAT) b_td_nbsp_e_td
            end if

            if (labHours>0.0) then
                write(device,'(a,f5.2,a)') b_td, labHours, e_td ! lab hours
            else
                write(device,AFORMAT) b_td_nbsp_e_td
            end if

            write(device,'(a,f5.2,a)') b_td, meetingUnits, e_td//e_tr

        end do
        write(device,AFORMAT) b_tr//'<td colspan="11">'//horizontal//e_td//e_tr, &
            b_tr//b_td_nbsp_e_td// & ! subject code
            b_td_nbsp_e_td// & ! title, credit
            b_td_nbsp_e_td// & ! block
            b_td_nbsp_e_td// & ! section code
            b_td_nbsp_e_td//  & ! class size
            b_td_nbsp_e_td//  & ! day
            b_td_nbsp_e_td//  & ! time
            b_td//b_bold//'Totals'//e_bold//' : '//e_td ! room
        write(device,'(3(a,f5.2,a))') b_td, totalLect, e_td, & ! hours lect
            b_td, totalLab, e_td, & ! hours lab
            b_td, totalUnits, e_td//e_tr ! load
        !write(device,AFORMAT) b_tr//'<td colspan="11">'//horizontal//e_td//e_tr

        write(device,AFORMAT) e_table

    end subroutine teacher_workload


    subroutine class_hours_and_load(iMeet, iSect, thisTerm, meetingUnits, lectHours, labHours)
        integer, intent (in) :: iMeet, iSect, thisTerm
        real, intent(out) :: meetingUnits, lectHours, labHours
        integer :: iSubj
        real :: meetingHours

        !call html_comment('class_hours_and_load()')

        iSubj = Section(thisTerm,iSect)%SubjectIdx
        if (iMeet/=-1) then
            meetingHours = (Section(thisTerm,iSect)%eTimeIdx(iMeet) - Section(thisTerm,iSect)%bTimeIdx(iMeet))/4.0
            if (isSubject_lecture_lab(iSubj)) then
                if (is_lecture_class(iSect, thisTerm)) then ! lecture of lecture-lab
                    meetingUnits = meetingHours*Subject(iSubj)%LectLoad/Subject(iSubj)%LectHours
                    lectHours = meetingHours
                    labHours = 0.0
                else ! lab of lecture-lab
                    meetingUnits = meetingHours*Subject(iSubj)%LabLoad/Subject(iSubj)%LabHours
                    lectHours = 0.0
                    labHours = meetingHours
                end if
            else if (Subject(iSubj)%LectHours>0.0) then ! lecture-only
                meetingUnits = meetingHours*Subject(iSubj)%LectLoad/Subject(iSubj)%LectHours
                lectHours = meetingHours
                labHours = 0.0
            else if (Subject(iSubj)%LabHours>0.0) then ! lab-only
                meetingUnits = meetingHours*Subject(iSubj)%LabLoad/Subject(iSubj)%LabHours
                lectHours = 0.0
                labHours = meetingHours
            end if
        else ! assume schedule is correct
            if (isSubject_lecture_lab(iSubj)) then
                if (is_lecture_class(iSect, thisTerm)) then ! lecture of lecture-lab
                    meetingUnits = Subject(iSubj)%LectLoad
                    lectHours = Subject(iSubj)%LectHours
                    labHours = 0.0
                    meetingHours = lectHours
                else ! lab of lecture-lab
                    meetingUnits = Subject(iSubj)%LabLoad
                    lectHours = 0.0
                    labHours = Subject(iSubj)%LabHours
                    meetingHours = labHours
                end if
            else if (Subject(iSubj)%LectHours>0.0) then ! lecture-only
                meetingUnits = Subject(iSubj)%LectLoad
                lectHours = Subject(iSubj)%LectHours
                labHours = 0.0
                meetingHours = lectHours
            else if (Subject(iSubj)%LabHours>0.0) then ! lab-only
                meetingUnits = Subject(iSubj)%LabLoad
                lectHours = 0.0
                labHours = Subject(iSubj)%LabHours
                meetingHours = labHours
            end if
        end if

    end subroutine class_hours_and_load


    subroutine change_current_teacher_password(device)
        integer, intent(in) :: device

        character (len=MAX_LEN_PASSWD_VAR) :: t0Password, t1Password, t2Password
        integer :: ierr
        logical :: flagIsUp

        call html_comment('change_current_teacher_password()')

        flagIsUp = .false.
        if (REQUEST==fnChangeTeacherPassword) then
            call cgi_get_named_string(QUERY_STRING, 'C', t0Password, ierr)
            if ( len_trim(t0Password)>0 ) then
                t0Password(17:) = SPACE
                if (isPassword_of_teacher(requestingTeacher,t0Password) ) then
                    UserRequestCheckMessage = 'Change current password.'
                else
                    UserRequestCheckMessage = 'Current password is incorrect.'
                    flagIsUp = .true.
                end if
            else
                UserRequestCheckMessage = ''
                flagIsUp = .true.
            end if
        end if
        if (.not. flagIsUp) then
            call cgi_get_named_string(QUERY_STRING, 'P', t1Password, ierr)
            call cgi_get_named_string(QUERY_STRING, 'R', t2Password, ierr)
            if ( len_trim(t1Password)>0 .and. len_trim(t2Password)>0 ) then
                if ( t1Password==t2Password ) then
                    t1Password(17:) = SPACE
                    if (isPassword_of_teacher(requestingTeacher, t1Password) .or. &
                            Teacher(requestingTeacher)%TeacherId==t2Password) then
                        UserRequestCheckMessage = 'New password is not valid.'
                    else
                        call set_password(Teacher(requestingTeacher)%Password, t1Password)
                        call teacher_details_write(unitXML, dirTEACHERS, requestingTeacher)
                        UserRequestCheckMessage = 'Successfully changed password for '//USERNAME
                        call html_college_links(device, CollegeIdxUser, mesg=UserRequestCheckMessage)
                        REQUEST = fnCollegeLinks
                        return
                    end if
                else
                    UserRequestCheckMessage = 'New password and repeat do not match.'
                end if
            else
                UserRequestCheckMessage = 'New password and/or repeat not specified.'
            end if
        end if

        call html_write_header(device, 'Change password for '//trim(USERNAME), UserRequestCheckMessage )

        call make_form_start(device, REQUEST)

        if (REQUEST==fnChangeTeacherPassword) write(device,AFORMAT) &
            b_bold//'Old password:'//e_bold//linebreak, &
            '<input size="20" type="password" name="C" value="">', &
            linebreak//linebreak
        write(device,AFORMAT) &
            b_bold//'New password:'//e_bold//linebreak, &
            '<input size="20" type="password" name="P" value="">', &
            linebreak//linebreak, &
            b_bold//'Repeat new password:'//e_bold//linebreak, &
            '<input size="20" type="password" name="R" value="">', &
            linebreak//linebreak, &
            '<input type="submit" value="Update">'//e_form//horizontal

    end subroutine change_current_teacher_password


    subroutine recent_teacher_activity(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_USERNAME) :: tTeacher, tRole
        integer :: ldx, iTmp, ierr, sarray(13)
        character (len=MAX_LEN_FILE_PATH) :: wrkDir, logFile

        ! which teacher ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, iTmp)
        targetTeacher = index_to_teacher(tTeacher)
        tTeacher = fn_blank_to_underscore(tTeacher)
        iTmp = Teacher(targetTeacher)%DeptIdx
        ldx = Department(iTmp)%CollegeIdx
        tRole = Teacher(targetTeacher)%Role

        call cgi_get_named_string(QUERY_STRING, 'A2', tRole, iTmp)
        call cgi_get_named_string(QUERY_STRING, 'A3', logFile, iTmp)

        call html_comment('recent_teacher_activity('//tTeacher//')')

        call html_write_header(device, 'Activity log of '//trim(Teacher(targetTeacher)%Name), SPACE)

        if (len_trim(logFile)>0) then
            write(device,AFORMAT) trim(tRole)//' activity'
            write(device,AFORMAT) '<pre>'
            call copy_to_unit(logFile, device)
            write(device,AFORMAT) '</pre>'//horizontal
        end if

        call getcwd(wrkDir)
        !call html_comment('cwd='//wrkDir)

        ! make a list of logs for teacher
        logFile = trim(dirLOG)//trim(College(ldx)%Code)//DIRSEP//trim(tTeacher)//'-*.log'
        call html_comment(dirCmd//trim(logFile)//' > '//trim(wrkDir)//DIRSEP//'logfiles')

        call system(dirCmd//trim(logFile)//' > '//trim(wrkDir)//DIRSEP//'logfiles', ierr)
        ierr = stat(trim(wrkDir)//DIRSEP//'logfiles', sarray)

        ! any mailboxes ?
        if (ierr .eq. 0 .and. sarray(8) .gt. 0) then

            write(device,AFORMAT) 'Select a date of activity: '

            open(unit=unitETC, file=trim(wrkDir)//DIRSEP//'logfiles', &
                status='unknown', form='formatted', iostat=ierr)
            !write(*,*) 'open mailBox: ', ierr

            ! loop through log files
            do while (ierr==0)

                read(unitETC,AFORMAT,iostat=ierr) logFile
                if (ierr .lt. 0) then ! no more mailboxes
                    close(unitETC)
                    exit
                end if

                ldx = len_trim(logFile)
                ! *-YYYYMMDD.log
                !  2109876543210
                tRole = logFile(ldx-11:ldx-4)
                write(device,AFORMAT) trim(make_href(fnRecentTeacherActivity, tRole, &
                    A1=tTeacher, A2=tRole, A3=trim(logFile), pre=nbsp))

            end do

        end if
        write(device,AFORMAT) linebreak//horizontal

    end subroutine recent_teacher_activity


    subroutine teacher_search_info(tIdx, searchString, location)
        integer, intent(in) :: tIdx
        character(len=*), intent(in) :: searchString
        character(len=80), intent(out) :: location
        integer :: iSect, iTerm, n_meetings, meetings(MAX_SECTION_MEETINGS)

        location = SPACE
        do iTerm=summerTerm,firstSemester,-1
            do iSect=1,NumSections(iTerm)
                if (index(Section(iTerm,iSect)%ClassId,searchString)==0) cycle ! search string not in section ID
                call meetings_of_section_by_teacher(iTerm, iSect, tIdx, n_meetings, meetings)
                if (n_meetings==0) cycle ! teacher not assigned to this section
                location = trim(txtSemester(iTerm+6))//trim(termQualifier(iTerm+6))//' : '//location
                exit
            end do
        end do
        if (index(Teacher(tIdx)%Specialization,searchString)>0) location = 'Specialization : '//location
        if (index(Teacher(tIdx)%Doctorate,searchString)>0) location = 'Doctorate : '//location
        if (index(Teacher(tIdx)%Master,searchString)>0) location = 'Master : '//location
        if (index(Teacher(tIdx)%Bachelor,searchString)>0) location = 'Bachelor : '//location
        if (index(Teacher(tIdx)%Name,searchString)>0) location = 'Name : '//location
        if (index(Teacher(tIdx)%TeacherId,searchString)>0) location = 'Login : '//location

    end subroutine teacher_search_info


    subroutine html_teacher_list (device, fn, nfacs, tArray, header, searchString)
        integer, intent (in) :: device, fn, nfacs
        integer, intent (in out) :: tArray(0:nfacs)
        character (len=*), intent (in) :: header
        character (len=*), intent (in) :: searchString ! for fn==fnSearchCategory

        integer :: iTeach, nsect(3), iMeet, sdx, tdx, iTerm, ierr, tLen, iSect
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer, dimension(60,7) :: TimeTable
        character (len=127) :: mesg, stats, tString
        real :: totalLect(3), lectHours, totalLab(3), labHours, totalUnits(3), meetingUnits
        character(len=80) :: location

        tString = searchString
        tLen = max(len_trim(tString),1)
        call html_write_header(device, header)

        if (nfacs == 0) then
            write(device,AFORMAT) '<table border="0">', &
                b_tr//b_td//JUSTNONE//e_td//b_tdar
            if ( (isRoleSysAd .or. isRoleOfficial .or. isRoleStaff) .and. &
                 fn/=fnSearchCategory .and. fn/=fnOnlineTeachers) then
                write(device,AFORMAT) trim(make_href(fnEditTeacher, 'Add teacher', &
                    A1='Guest', pre=b_small//'('//nbsp, post=' )'//e_small))
            end if
            write(device,AFORMAT) e_td//e_tr//e_table
        else
            ! sort teachers
            do tdx=1,nfacs-1
                do sdx=tdx+1,nfacs
                    if (Teacher(tArray(sdx))%Name<Teacher(tArray(tdx))%Name) then
                        iTeach =tArray(sdx)
                        tArray(sdx) = tArray(tdx)
                        tArray(tdx) = iTeach
                    end if
                end do
            end do

            write(device,AFORMAT) '<table border="0">', b_tr//b_td// &
                b_italic//'(Numbers per term are: No. of classes / Lect hrs / Lab hrs)'//e_italic, &
                e_td//b_tdar
            if ( (isRoleSysAd .or. isRoleOfficial .or. isRoleStaff) .and. &
                 fn/=fnSearchCategory .and. fn/=fnOnlineTeachers) then
                write(device,AFORMAT) trim(make_href(fnEditTeacher, 'Add teacher', &
                    A1='Guest', pre=b_small//'('//nbsp, post=' )'//e_small))
            end if
            write(device,AFORMAT) e_td//e_tr//e_table

            write(device,AFORMAT) '<table border="0">'//&
                b_tr//b_thal//'Name (Specialization)'//e_th// &
                b_thal//'Login/Role'//e_th
            do iTerm=firstSemester,summerTerm
                write(device,AFORMAT) &
                    b_thac//txtSemester(iTerm+6)//termQualifier(iTerm+6)//linebreak// &
                    text_school_year(currentYear)//e_th
            end do
            write(device,AFORMAT) b_thac//'Remark'//e_th//b_thac//'Links'//e_th
            if (fn/=fnSearchCategory) then
                write(device,AFORMAT) e_tr
            else
                write(device,AFORMAT) b_thal//'"'//tString(:tLen)//'" found in ...'//e_th//e_tr
            end if

            do tdx=1,nfacs
                iTeach = tArray(tdx)

                mesg = SPACE
                nsect = 0
                totalUnits = 0.0
                totalLect = 0.0
                totalLab = 0.0

                do iTerm=firstSemester,summerTerm

                    ! collect classes of teacher
                    call timetable_clear(TimeTable)
                    do iSect=1,NumSections(iTerm)
                        call meetings_of_section_by_teacher(iTerm, iSect, iTeach, n_meetings, meetings)
                        if (n_meetings==0) cycle ! teacher not assigned to this section
                        nsect(iTerm) = nsect(iTerm)+1
                        do iMeet=1,n_meetings
                            call class_hours_and_load(iMeet, iSect, iTerm, meetingUnits, lectHours, labHours)
                            totalLect(iTerm) = totalLect(iTerm) + lectHours
                            totalLab(iTerm) = totalLab(iTerm) + labHours
                            totalUnits(iTerm) = totalUnits(iTerm) + meetingUnits
                        end do
                        ierr = -10
                        call timetable_add_meetings_of_section( iTerm, iSect, n_meetings, meetings, TimeTable, ierr)
                        if (ierr /= -10) then
                            mesg = trim(mesg)//SPACE//txtSemester(iTerm+6)
                        end if
                    end do

                end do
                if (len_trim(mesg)>0) mesg = red//'Conflict: '//trim(mesg)//e_color

                QUERY_put = Teacher(iTeach)%TeacherId

                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(tdx,2))//'">'// &
                    b_td//trim(itoa(tdx))//'. '//trim(Teacher(iTeach)%Name)//e_td, &
                    b_td//trim(Teacher(iTeach)%TeacherId)//FSLASH//trim(Teacher(iTeach)%Role)//e_td

                do iTerm=firstSemester,summerTerm

                    if ( nsect(iTerm)>0) then
                        write(stats,'(i2,2(a,f5.1))') &
                            nsect(iTerm), SPACE//FSLASH//SPACE, totalLect(iTerm), SPACE//FSLASH//SPACE, &
                            totalLab(iTerm) ! , SPACE//FSLASH//SPACE, totalUnits(iTerm)

                        write(device,AFORMAT) trim(make_href(fnTeacherClasses, stats, &
                            A1=QUERY_put, A9=iTerm, pre=b_tdac//nbsp, post=b_small//b_italic))

                        if ( isRole_dean_of_college(Department(Teacher(iTeach)%DeptIdx)%CollegeIdx,orHigherUp) ) then
                            if (iTerm==currentTerm .and. &
                                College(Department(Teacher(iTeach)%DeptIdx)%CollegeIdx)%isAllowed(ToEvaluateTeachers,iTerm) ) then
                                write(device,AFORMAT) trim(make_href(fnEvaluationForm, 'Eval', &
                                    A1=QUERY_put, A2=EvalTypeDescription(3), A9=iTerm, pre=nbsp))
                                write(device,AFORMAT) &
                                    trim(make_href(fnEvaluationDuties, 'Duties', A1=QUERY_put, A9=iTerm, pre=nbsp, &
                                    post=nbsp//LPAR//trim(itoa(Teacher(iTeach)%EvalsToGive))// &
                                         ', '//trim(itoa(Teacher(iTeach)%EvalsToReceive))//RPAR))
                            end if
                            write(device,AFORMAT) trim(make_href(fnEvaluationRatings, 'Report', &
                                A1=QUERY_put, A2='Teacher', A9=iTerm, pre=nbsp ))
                        end if
                        write(device,AFORMAT) e_italic//e_small//nbsp//e_td
                    else
                        write(device,AFORMAT) b_td_nbsp_e_td
                    end if

                end do

                write(device,AFORMAT) b_tdac//trim(mesg)//e_td//b_td//b_small
                if ( isRole_dean_of_college(Department(Teacher(iTeach)%DeptIdx)%CollegeIdx, orHigherUp) ) then
                    write(device,AFORMAT) &
                        trim(make_href(fnEditTeacher, 'Info', A1=QUERY_put, pre=nbsp)), &
                        trim(make_href(fnRecentTeacherActivity, 'Log', A1=QUERY_put, pre=nbsp))
                    if (Teacher(iTeach)%NumAdvisees>0) write(device,AFORMAT) &
                        trim(make_href(fnAdvisersByTeacher, 'Advisees', A1=QUERY_put, pre=nbsp))
                    if (Teacher(iTeach)%Role==BENEFACTOR) write(device,AFORMAT) &
                        trim(make_href(fnListBeneficiaries, 'Beneficiaries', A1=QUERY_put, pre=nbsp))
                    if (isRoleSysAd) then
                        write(device,AFORMAT) &
                            trim(make_href(fnSwitchUser, 'Login', A1=QUERY_put, pre=nbsp, newtab='"_blank"'))
                    end if
                end if
		        if (fn/=fnSearchCategory) then
                    write(device,AFORMAT) e_small//e_td//e_tr
		        else
		            call teacher_search_info(iTeach, tString(:tLen), location)
		            !call html_comment(Teacher(iTeach)%TeacherId//tString(:tLen)//' - '//trim(location))
		            iMeet = max(3, len_trim(location) )
                    write(device,AFORMAT) e_small//e_td//b_td//b_small//location(:iMeet-2)//e_small//e_td//e_tr
		        end if
            end do
            write(device,AFORMAT) e_table

        end if
        write(device,AFORMAT) horizontal

    end subroutine html_teacher_list


end module EditTEACHERS
