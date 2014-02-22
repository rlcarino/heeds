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


module EditTEACHERS

    use HTML

    implicit none

contains


    subroutine teacher_edit(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher, tAction
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer :: ierr, tdx, i, j
        character (len=255) :: header, remark, tmpRHS
        type (TYPE_TEACHER) :: wrk
        logical :: isDirtyTEACHERS, criticalErr
        !character (len=MAX_LEN_PASSWD_VAR) :: Password
        !integer :: lenP

        call html_comment('teacher_edit()')

        isDirtyTEACHERS = .false.
        remark = SPACE

        ! which teacher ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, tdx)
        if (tdx/=0 .or. tTeacher==SPACE) tTeacher = 'Guest'
        tdx = index_to_teacher(tTeacher)

        targetTeacher = tdx
        targetDepartment = Teacher(targetTeacher)%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx

        if (REQUEST==fnGenerateTeacherPassword) then
            call set_password(Teacher(tdx)%Password)
            call xml_write_teachers(trim(pathToYear)//'TEACHERS.XML')
            call log_teacher_record_change(tdx, 'Changed password')
            call teacher_info(device, Teacher(tdx), 'Edit info for teacher '//tTeacher, &
                SPACE, 'Update', tdx)
            return
        end if

        wrk = Teacher(tdx)

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

            call teacher_info(device, wrk, header, remark, tAction, tdx)

        else if (isRoleOfficial) then

            header = 'Edit info for teacher '//tTeacher
            remark = 'Edit teacher "'//trim(tTeacher)//'" failed. '//sorryMessage
            call teacher_info(device, wrk, header, remark, tAction, tdx)

        else ! action is Add or Update; collect changes

            call cgi_get_named_string(QUERY_STRING, 'Login', wrk%TeacherID, ierr)
            !write(*,*) 'ierr=', ierr, ', Login=', wrk%TeacherID
            if (ierr/=0) wrk%TeacherID = Teacher(tdx)%TeacherID
            if (wrk%TeacherID /= Teacher(tdx)%TeacherID) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Username='//wrk%TeacherID
                call log_teacher_record_change(tdx, 'TeacherID changed to '//wrk%TeacherID)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Name', tmpRHS, ierr)
            wrk%Name = tmpRHS
            !write(*,*) 'ierr=', ierr, ', Name=', wrk%Name
            if (ierr/=0) wrk%Name = Teacher(tdx)%Name
            if (wrk%Name /= Teacher(tdx)%Name) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Name='//wrk%Name
                call log_teacher_record_change(tdx, 'Name changed to '//wrk%Name)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Bachelor', tmpRHS, ierr)
            wrk%Bachelor = tmpRHS
            !write(*,*) 'ierr=', ierr, ', Bachelor=', wrk%Bachelor
            if (ierr/=0) wrk%Bachelor = Teacher(tdx)%Bachelor
            if (wrk%Bachelor /= Teacher(tdx)%Bachelor) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Bachelor='//wrk%Bachelor
                call log_teacher_record_change(tdx, 'Bachelor changed to '//wrk%Bachelor)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Master', tmpRHS, ierr)
            wrk%Master = tmpRHS
            !write(*,*) 'ierr=', ierr, ', Master=', wrk%Master
            if (ierr/=0) wrk%Master = Teacher(tdx)%Master
            if (wrk%Master /= Teacher(tdx)%Master) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Master='//wrk%Master
                call log_teacher_record_change(tdx, 'Master changed to '//wrk%Master)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Doctorate', tmpRHS, ierr)
            wrk%Doctorate = tmpRHS
            !write(*,*) 'ierr=', ierr, ', Doctorate=', wrk%Doctorate
            if (ierr/=0) wrk%Doctorate = Teacher(tdx)%Doctorate
            if (wrk%Doctorate /= Teacher(tdx)%Doctorate) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Doctorate='//wrk%Doctorate
                call log_teacher_record_change(tdx, 'Doctorate changed to '//wrk%Doctorate)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Specialization', tmpRHS, ierr)
            wrk%Specialization = tmpRHS
            !write(*,*) 'ierr=', ierr, ', Specialization=', wrk%Specialization
            if (ierr/=0) wrk%Specialization = Teacher(tdx)%Specialization
            if (wrk%Specialization /= Teacher(tdx)%Specialization) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Specialization='//wrk%Specialization
                call log_teacher_record_change(tdx, 'Specialization changed to '//wrk%Specialization)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Role', wrk%Role, ierr)
            !write(*,*) 'ierr=', ierr, ', Role=', wrk%Role
            if (ierr/=0) wrk%Role = Teacher(tdx)%Role
            if (wrk%Role /= Teacher(tdx)%Role) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Role='//wrk%Role
                call log_teacher_record_change(tdx, 'Role changed to '//wrk%Role)
            end if

            call cgi_get_named_integer(QUERY_STRING, 'Load', wrk%MaxLoad, ierr)
            !write(*,*) 'ierr=', ierr, ', MaxLoad=', wrk%MaxLoad
            if (ierr/=0 .or. wrk%MaxLoad<=0) wrk%MaxLoad = Teacher(tdx)%MaxLoad
            if (wrk%MaxLoad /= Teacher(tdx)%MaxLoad) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Max load='//itoa(wrk%MaxLoad)
                call log_teacher_record_change(tdx, 'MaxLoad changed to '//itoa(wrk%MaxLoad))
            end if

            call cgi_get_named_integer(QUERY_STRING, 'Rank', wrk%Rank, ierr)
            !write(*,*) 'ierr=', ierr, ', Rank=', wrk%Rank
            if (ierr/=0 .or. wrk%Rank<=0) wrk%Rank = Teacher(tdx)%Rank
            if (wrk%Rank /= Teacher(tdx)%Rank) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Rank='//AcademicRank(wrk%Rank)
                call log_teacher_record_change(tdx, 'Rank changed to '//AcademicRank(wrk%Rank) )
            end if

            call cgi_get_named_integer(QUERY_STRING, 'Step', wrk%Step, ierr)
            !write(*,*) 'ierr=', ierr, ', Step=', wrk%Step
            if (ierr/=0 .or. wrk%Step<=0) wrk%Step = Teacher(tdx)%Step
            if (wrk%Step /= Teacher(tdx)%Step) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Step='//RankStep(wrk%Step)
                call log_teacher_record_change(tdx, 'Step changed to '//RankStep(wrk%Step))
            end if

            call cgi_get_named_string(QUERY_STRING, 'Department', tDepartment, ierr)
            wrk%DeptIdx = index_to_dept(tDepartment)
            !write(*,*) 'ierr=', ierr, ', DeptIdx=', wrk%DeptIdx
            if (ierr/=0 .or. wrk%DeptIdx<=0) wrk%DeptIdx = Teacher(tdx)%DeptIdx
            if (wrk%DeptIdx /= Teacher(tdx)%DeptIdx) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Unit='//Department(wrk%DeptIdx)%Code
                call log_teacher_record_change(tdx, 'Unit changed to '//Department(wrk%DeptIdx)%Code)
            end if


            if (isDirtyTEACHERS) then  ! some changes

                if (wrk%TeacherID /= Teacher(tdx)%TeacherID) then  ! new username
                    j = index_to_teacher(wrk%TeacherID)
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
                                tdx = NumTeachers
                                tTeacher = wrk%TeacherID
                                call set_password(wrk%Password)
                                remark = ': Added new teacher'//remark
                            end if
                        else
                            remark = ': Updated teacher'//remark
                        end if
                        ! copy new name to Student()%Adviser
                        do i=1,NumStudents+NumAdditionalStudents
                            if (Student(i)%Adviser==Teacher(tdx)%TeacherId) Student(i)%Adviser = wrk%TeacherId
                        end do

                        Teacher(tdx) = wrk
                        call sort_alphabetical_teachers()

                    else
                        remark = ': Invalid username: "'//trim(wrk%TeacherID)//'" is already taken by '//Teacher(j)%Name
                        isDirtyTEACHERS = .false.
                    end if

                else ! same username
                    remark = ': Updated teacher'//remark
                    Teacher(tdx) = wrk
                end if

                if (trim(tAction)=='Add') then
                    header = 'Add new teacher'
                else
                    header = 'Edit info for teacher '//tTeacher
                end if
                call teacher_info(device, wrk, header, remark(3:), tAction, tdx)

                if (isDirtyTEACHERS) call xml_write_teachers(trim(pathToYear)//'TEACHERS.XML')

            else ! Add or Update clicked, but no changes made
                remark = ': No changes made?'
                call html_college_links(device, Department(wrk%DeptIdx)%CollegeIdx, &
                    trim(tTeacher)//remark)
            end if

        end if

    end subroutine teacher_edit


    subroutine teacher_delete (device)
        integer, intent (in) :: device
        integer :: fac, nsect(3), sdx, tdx, term
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        character (len=127) :: mesg
        integer, dimension(60,6) :: TimeTable
        integer :: tYear, tTerm

        call html_comment('teacher_delete()')

        ! which teacher ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, tdx)
        fac = index_to_teacher(tTeacher)
        if (fac==0) then
            call html_college_links(device, CollegeIdxUser, mesg='Teacher "'//trim(tTeacher)//'" not found.')
            return
        end if

        if (isRoleOfficial) then
            call html_college_links(device, CollegeIdxUser, mesg='Teacher "'//trim(tTeacher)// &
                '" not deleted. '//sorryMessage)
            return
        end if

        ! count classes handled by teacher
        nsect = 0
        mesg = SPACE
        do tTerm=termBegin,termEnd
            call qualify_term (tTerm, tYear, term)
            call timetable_clear(TimeTable)
            do sdx=1,NumSections(term)
                call meetings_of_section_by_teacher(Section(term,0:), sdx, fac, n_meetings, meetings)
                if (n_meetings==0) cycle ! teacher not assigned to this section
                nsect(term) = nsect(term)+1
            end do
            if (nsect(term)>0) then
                mesg = trim(mesg)//COMMA//SPACE//txtSemester(term+6)
            end if
        end do
        if (sum(nsect)>0) then
            mesg = 'Delete "'//trim(tTeacher)//'" failed; has classes during '//trim(mesg(2:))//' term.'
            call html_college_links(device, CollegeIdxUser, mesg)
            return
        end if
        ! check if Adviser
        tdx = 0
        do sdx=1,NumStudents+NumAdditionalStudents
            if (Student(sdx)%Adviser==tTeacher) tdx = tdx+1
        end do
        if (tdx>0) then
            mesg = 'Delete "'//trim(tTeacher)//'" failed; has advisees.'
            call html_college_links(device, CollegeIdxUser, mesg)
            return
        end if

        call log_teacher_record_change(fac, trim(tTeacher)//' deleted.')

        call initialize_teacher(Teacher(fac))
        Teacher(fac)%DeptIdx = 0
        call xml_write_teachers(trim(pathToYear)//'TEACHERS.XML')
        call html_college_links(device, CollegeIdxUser, mesg='Teacher "'//trim(tTeacher)//'" deleted.')

    end subroutine teacher_delete


    subroutine teacher_list_all (device, fn)
        integer, intent (in) :: device, fn
        integer :: fac, nfacs, nsect(3), mdx, sdx, tdx, term, ierr
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=127) :: mesg, stats
        integer, dimension(60,6) :: TimeTable
        character(len=1) :: ch
        real :: totalLect(3), lectHours, totalLab(3), labHours, totalUnits(3), meetingUnits
        integer :: tYear, tTerm

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
                        fac = TeacherRank(tdx)
                        if (index(Teacher(fac)%Name,stats(:sdx))==0) then
                            call upper_case(stats)
                            if (index(Teacher(fac)%Name,stats(:sdx))==0) cycle
                        end if
                        if (Teacher(fac)%Role==SYSAD .and. .not. isRoleSysAd) cycle
                        nfacs = nfacs+1
                        tArray(nfacs) = fac
                    end do
                    mesg = 'Teachers with "'//stats(:sdx)//'" in name.'
                else
                    mesg = 'Search teacher failed: search string not specified.'
                end if

            case (fnOnlineTeachers)
                do tdx=1,NumTeachers+NumAdditionalTeachers
                    fac = TeacherRank(tdx)
                    if (Teacher(fac)%OnlineStatus==0) cycle
                    if (Teacher(fac)%Role==SYSAD .and. .not. isRoleSysAd) cycle
                    nfacs = nfacs+1
                    tArray(nfacs) = fac
                end do
                mesg = 'Online teachers'

            case (fnTeachersByDept)
                ! which department ?
                call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)
                targetDepartment = index_to_dept(tDepartment)
                targetCollege = Department(targetDepartment)%CollegeIdx
                do tdx=1,NumTeachers+NumAdditionalTeachers
                    fac = TeacherRank(tdx)
                    if (targetDepartment/=Teacher(fac)%DeptIdx) cycle
                    if (Teacher(fac)%Role==SYSAD .and. .not. isRoleSysAd) cycle
                    nfacs = nfacs+1
                    tArray(nfacs) = fac
                end do
                mesg = 'Teachers in '//tDepartment

            case (fnTeachersByname)
                ! which college ?
                call cgi_get_named_string(QUERY_STRING, 'A2', ch, ierr)
                call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
                targetCollege = index_to_college(tCollege)
                do tdx=1,NumTeachers+NumAdditionalTeachers
                    fac = TeacherRank(tdx)
                    if (Department(Teacher(fac)%DeptIdx)%CollegeIdx /= targetCollege) cycle
                    if (Teacher(fac)%Role==SYSAD .and. .not. isRoleSysAd) cycle
                    if (Teacher(fac)%Name(1:1) /= ch) cycle
                    nfacs = nfacs+1
                    tArray(nfacs) = fac
                end do
                if (nfacs>0) then
                    targetDepartment = Teacher(tArray(nfacs))%DeptIdx
                    targetCollege = Department(targetDepartment)%CollegeIdx
                else
                    targetDepartment = DeptIdxUser
                    targetCollege = CollegeIdxUser
                end if
                mesg = '"'//ch//'" teachers in '//tCollege

        end select

        call html_write_header(device, mesg)

        if (nfacs == 0) then
            write(device,AFORMAT) '<table border="0">', &
                begintr//begintd//JUSTNONE//endtd//tdalignright
            if ( (isRoleSysAd .or. isRoleOfficial .or. isRoleStaff) .and. fn/=fnOnlineTeachers) then
                write(device,AFORMAT) trim(make_href(fnEditTeacher, 'Add teacher', &
                    A1='Guest', pre=beginsmall//'('//nbsp, post=' )'//endsmall))
            end if
            write(device,AFORMAT) endtd//endtr//endtable
        else
            ! sort teachers
            do tdx=1,nfacs-1
                do sdx=tdx+1,nfacs
                    if (Teacher(tArray(sdx))%Name<Teacher(tArray(tdx))%Name) then
                        fac =tArray(sdx)
                        tArray(sdx) = tArray(tdx)
                        tArray(tdx) = fac
                    end if
                end do
            end do

            write(device,AFORMAT) '<table border="0">', begintr//begintd// &
                beginitalic//'(Numbers per term are: No. of classes / Lect hrs / Lab hrs)'//enditalic, &
                endtd//tdalignright
            if ((isRoleSysAd .or. isRoleOfficial .or. isRoleStaff) .and. fn/=fnOnlineTeachers) then
                write(device,AFORMAT) trim(make_href(fnEditTeacher, 'Add teacher', &
                    A1='Guest', pre=beginsmall//'('//nbsp, post=' )'//endsmall))
            end if
            write(device,AFORMAT) endtd//endtr//endtable

            write(device,AFORMAT) '<table border="0">'//&
                begintr//thalignleft//'Name (Specialization)'//endth// &
                thalignleft//'Login/Role'//endth
            do tTerm=termBegin,termEnd
                call qualify_term (tTerm, tYear, term)
                write(device,AFORMAT) &
                    thaligncenter//txtSemester(term+6)//termQualifier(term+6)//linebreak// &
                    text_school_year(tYear)//endth
            end do
            write(device,AFORMAT) &
                thaligncenter//'Remark'//endth//tdnbspendtd//endtr

            do tdx=1,nfacs
                fac = tArray(tdx)

                mesg = SPACE
                nsect = 0
                totalUnits = 0.0
                totalLect = 0.0
                totalLab = 0.0

                do tTerm=termBegin,termEnd
                    call qualify_term (tTerm, tYear, term)

                    ! collect classes of teacher
                    call timetable_clear(TimeTable)
                    do sdx=1,NumSections(term)
                        call meetings_of_section_by_teacher(Section(term,0:), sdx, fac, n_meetings, meetings)
                        if (n_meetings==0) cycle ! teacher not assigned to this section
                        nsect(term) = nsect(term)+1
                        do mdx=1,n_meetings
                            call class_hours_and_load(mdx, sdx, Section(term,0:), meetingUnits, lectHours, labHours)
                            totalLect(term) = totalLect(term) + lectHours
                            totalLab(term) = totalLab(term) + labHours
                            totalUnits(term) = totalUnits(term) + meetingUnits
                        end do
                        ierr = -10
                        call timetable_add_meetings_of_section( Section(term,0:), &
                            sdx, n_meetings, meetings, TimeTable, ierr)
                        if (ierr /= -10) then
                            mesg = trim(mesg)//SPACE//txtSemester(term+6)
                        end if
                    end do

                end do
                if (len_trim(mesg)>0) mesg = red//'Conflict: '//trim(mesg)//black

                QUERY_put = Teacher(fac)%TeacherID

                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(tdx,2))//'">'// &
                    begintd//trim(itoa(tdx))//'. '//trim(Teacher(fac)%Name)//endtd, &
                    begintd//trim(Teacher(fac)%TeacherID)//FSLASH//trim(Teacher(fac)%Role)//endtd

                do tTerm=termBegin,termEnd
                    call qualify_term (tTerm, tYear, term)
                    write(stats,'(i2,2(a,f5.1))') &
                        nsect(term), SPACE//FSLASH//SPACE, totalLect(term), SPACE//FSLASH//SPACE, &
                        totalLab(term) ! , SPACE//FSLASH//SPACE, totalUnits(term)
                    write(device,AFORMAT) trim(make_href(fnTeacherClasses, stats, &
                        A1=QUERY_put, A9=term, pre=tdaligncenter//nbsp, post=nbsp//endtd))
                end do

                write(device,AFORMAT) tdaligncenter//trim(mesg)//endtd//begintd//beginsmall
                if ( is_dean_of_college(Department(Teacher(fac)%DeptIdx)%CollegeIdx, orHigherUp) ) then
                    write(device,AFORMAT) &
                        trim(make_href(fnEditTeacher, 'Info', A1=QUERY_put, pre=nbsp)), &
                        trim(make_href(fnRecentTeacherActivity, 'Log', A1=QUERY_put, pre=nbsp))
                    if (Teacher(fac)%NumAdvisees>0) write(device,AFORMAT) &
                        trim(make_href(fnAdvisersByTeacher, 'Advisees', A1=QUERY_put, pre=nbsp))
                end if
                write(device,AFORMAT) endsmall//endtd//endtr
            end do
            write(device,AFORMAT) endtable

        end if
        write(device,AFORMAT) horizontal

    end subroutine teacher_list_all


    subroutine teacher_conflicts (device, thisTerm, NumSections, Section)
        integer, intent (in) :: device, thisTerm
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer :: fac, nfacs, nsect, mdx, sdx, tdx, ierr
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        !character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=127) :: mesg
        integer, dimension(60,6) :: TimeTable
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
            fac = TeacherRank(tdx)
            if (Department(Teacher(fac)%DeptIdx)%CollegeIdx/=targetCollege) cycle ! not in college
            ! collect classes of teacher
            call timetable_clear(TimeTable)
            nsect = 0 ! how many conflicts
            do sdx=1,NumSections
                call meetings_of_section_by_teacher(Section, sdx, fac, n_meetings, meetings)
                if (n_meetings>0) then ! teacher assigned to this section
                    ierr = -10
                    call timetable_add_meetings_of_section( Section, sdx, n_meetings, meetings, TimeTable, ierr)
                    if (ierr /= -10) then ! conflict
                        nsect = nsect+1
                        exit
                    end if
                end if
            end do
            if (nsect>0) then ! conflict
                nfacs = nfacs + 1
                tArray(nfacs) = fac
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
                        fac =tArray(sdx)
                        tArray(sdx) = tArray(tdx)
                        tArray(tdx) = fac
                    end if
                end do
            end do

            write(device,AFORMAT) '<table border="0" width="90%">'//&
                begintr//thalignleft//'Name (Specialization)'//endth// &
                thaligncenter//PROGNAME//' Role'//endth// &
                thaligncenter//'No. of'//linebreak//'classes'//endth// &
                thaligncenter//'Lect'//linebreak//'hours'//endth// &
                thaligncenter//'Lab'//linebreak//'hours'//endth// &
                thaligncenter//'Teaching'//linebreak//'load'//endth// &
                thaligncenter//'Remark'//endth//endtr

            do tdx=1,nfacs
                fac = tArray(tdx)
                ! check conflicts
                mesg = SPACE
                call timetable_clear(TimeTable)
                ! collect classes of teacher
                nsect = 0
                totalUnits = 0.0
                totalLect = 0.0
                totalLab = 0.0
                do sdx=1,NumSections
                    call meetings_of_section_by_teacher(Section, sdx, fac, n_meetings, meetings)
                    if (n_meetings>0) then ! teacher assigned to this section
                        nsect = nsect+1
                        tArray(nfacs+nsect) = sdx
                        do mdx=1,n_meetings
                            call class_hours_and_load(mdx, sdx, Section, meetingUnits, lectHours, labHours)
                            totalLect = totalLect + lectHours
                            totalLab = totalLab + labHours
                            totalUnits = totalUnits + meetingUnits
                        end do
                        ierr = -10
                        call timetable_add_meetings_of_section(Section, sdx, n_meetings, meetings, TimeTable, ierr)
                        if (ierr /= -10) then
                            mesg = red//'Conflict!'//black
                        end if
                    end if
                end do
                QUERY_put = Teacher(fac)%TeacherID

                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(tdx,2))//'">'// &
                    begintd//trim(itoa(tdx))//'. '//trim(Teacher(fac)%Name)// &
                    ' ('//trim(Teacher(fac)%Specialization)//')'
                if ( is_dean_of_college(Department(Teacher(fac)%DeptIdx)%CollegeIdx, orHigherUp) ) then
                    write(device,AFORMAT) trim(make_href(fnEditTeacher, 'Edit', &
                        A1=QUERY_put, pre=nbsp//beginsmall, post=endsmall))
                end if
                write(device,AFORMAT) endtd//tdaligncenter//trim(Teacher(fac)%Role)//endtd, &
                    tdaligncenter//itoa(nsect)//trim(make_href(fnTeacherEditSchedule, 'Edit', &
                    A1=QUERY_put, A9=thisTerm, pre=beginsmall, post=endsmall))
                write(device,'(2(a,f5.1), a,f5.2,a)') endtd//tdaligncenter, totalLect, &
                    endtd//tdaligncenter, totalLab, &
                    endtd//tdaligncenter, totalUnits, &
                    '/'//trim(itoa(Teacher(fac)%MaxLoad))// &
                    trim(make_href(fnPrintableWorkload, 'Printable', &
                    A1=QUERY_put, A9=thisTerm, pre=nbsp//beginsmall, post=endsmall))//endtd// &
                    tdaligncenter//trim(mesg)//endtd//endtr
            end do
            write(device,AFORMAT) endtable//horizontal

        end if

    end subroutine teacher_conflicts


    subroutine teacher_schedule(device, thisTerm, NumSections, Section, LoadSource)
        integer, intent(in), optional :: LoadSource
        integer, intent (in) :: device, thisTerm
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        integer :: mdx, sdx, tLen1, tLen2, ierr, sect, LoadFromDept
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        character(len=MAX_LEN_CLASS_ID) :: tAction, tClassId
        integer, dimension(60,6) :: TimeTable
        logical :: conflicted, assigned, allowed_to_edit
        character(len=255) :: mesg

        call html_comment('teacher_schedule()')

        ! which teacher?
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, ierr)
        mdx = index_to_teacher(tTeacher)
        if (mdx==0) then
            call html_college_links(device, CollegeIdxUser, mesg='Teacher "'//trim(tTeacher)//'" not found.')
            return
        end if
        targetTeacher = mdx
        targetDepartment = Teacher(targetTeacher)%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx
        mesg = SPACE
        allowed_to_edit = is_admin_of_college(targetCollege) .or. &
            ( is_chair_of_department(targetDepartment,orHigherUp) .and. &
              ( (thisTerm==currentTerm .and. isPeriodOne) .or. &
                (thisTerm==nextTerm .and. (.not. isPeriodOne)) ) )

        ! check if there are other arguments
        call cgi_get_named_string(QUERY_STRING, 'A2', tAction, ierr)
        if (ierr==0) then ! action is Add or Del
            call cgi_get_named_string(QUERY_STRING, 'A3', tClassId, ierr)
            sect = index_to_section(tClassId, NumSections, Section)
            if (sect>0 .and. .not. isRoleofficial) then ! target of action is indexed by sect
                LoadFromDept = Section(sect)%DeptIdx
                if (tAction=='Add') then
                    do mdx=1,Section(sect)%NMeets
                        Section(sect)%TeacherIdx(mdx) = targetTeacher
                    end do
                    mesg = 'Added '//tClassId
                end if
                if (tAction=='Del') then
                    do mdx=1,Section(sect)%NMeets
                        Section(sect)%TeacherIdx(mdx) = 0
                    end do
                    mesg = 'Deleted '//tClassId
                end if
                call xml_write_classes(pathToTerm, NumSections, Section, 0)
            end if

            if (isRoleOfficial) then
                mesg = '"'//trim(tAction)//SPACE//trim(tClassId)//'" failed. '//sorryMessage
            end if
        end if

        call html_write_header(device, 'Teaching schedule of '//trim(Teacher(targetTeacher)%Name), mesg)

        ! collect meetings of teacher targetTeacher
        call timetable_meetings_of_teacher(NumSections, Section, targetTeacher, 0, tLen1, tArray, TimeTable, conflicted)
        !write(*, '(3i6)') (tArray(mdx), mdx=1,tLen1)
        !if (conflicted) write(*,*) 'Conflict in schedule '//trim(Teacher(targetTeacher)%Name)
        call list_sections_to_edit(device, thisTerm, Section, tLen1, tArray, fnTeacherEditSchedule, &
            tTeacher, 'Del', allowed_to_edit)
        !write(*, '(3i6)') (tArray(mdx), mdx=1,tLen1)
        if (tLen1>0) call timetable_display(device, Section, TimeTable)

        write(device,AFORMAT) horizontal

        if (isRoleStudent) return

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
        do sdx=1,NumSections
            if (LoadFromDept/=Section(sdx)%DeptIdx) cycle ! not in this department
            if (Section(sdx)%NMeets==1 .and. Section(sdx)%DayIdx(1)==0) cycle ! meeting days/time not specified
            ! teacher(s) already assigned to this section?
            assigned = .false.
            do mdx=1,Section(sdx)%NMeets
                if (Section(sdx)%TeacherIdx(mdx)/=0) assigned = .true.
            end do
            if (assigned) cycle ! section has a teacher
            if (.not. is_conflict_timetable_with_section(Section, sdx, TimeTable)) then ! add to list
                do mdx=1,Section(sdx)%NMeets
                    tArray(tLen1+tLen2+1) = sdx
                    tArray(tLen1+tLen2+2) = mdx
                    tArray(tLen1+tLen2+3) = 0
                    tLen2 = tLen2+3
                end do
            end if
        end do
        tArray(tLen1+tLen2+1) = 0
        tArray(tLen1+tLen2+2) = 0
        tArray(tLen1+tLen2+3) = 0
        if (tLen2>0) then
            call list_sections_to_edit(device, thisTerm, Section, tLen2, tArray(tLen1+1), fnTeacherEditSchedule, tTeacher, 'Add', &
            allowed_to_edit, &
            beginbold//'Classes with TBA teachers in '//trim(Department(LoadFromDept)%Code)// &
            ' that fit the schedule of '//trim(Teacher(targetTeacher)%Name)//endbold)
        end if

        ! search for feasible classes in another department?
        if ( .not. (isRoleStudent .or. isRoleGuest) ) then
            call make_form_start(device, fnTeacherEditSchedule, tTeacher, A9=thisTerm)
            write(device,AFORMAT) linebreak//'Search for feasible classes in : <select name="A4">'
            do mdx=2,NumDepartments
                if (mdx/=LoadFromDept) then
                    ierr = 0
                else
                    ierr = 1
                end if
                write(device,AFORMAT) '<option value="'//trim(Department(mdx)%Code)//'"'//trim(selected(ierr))//'> '// &
                    trim(Department(mdx)%Code)//DASH//trim(Department(mdx)%Name)
            end do
            write(device,AFORMAT) '</select>'//nbsp//'<input type="submit" value="Find classes">'//endform
        end if
        write(device,AFORMAT) horizontal

    end subroutine teacher_schedule


    subroutine teacher_info(device, wrk, header, remark, tAction, tdx)
        integer, intent(in) :: device, tdx
        type (TYPE_TEACHER), intent(in) :: wrk
        character (len=*), intent(in)  :: header, remark, tAction
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        character (len=MAX_LEN_PASSWD_VAR) :: Password
        integer :: i, j, k

        call html_comment('teacher_info()')

        tTeacher = wrk%TeacherID
        targetDepartment = wrk%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx
        call get_teacher_password(tdx, Password)

        call html_write_header(device, header, remark)

        call make_form_start(device, fnEditTeacher, tTeacher)

        write(device,AFORMAT) '<table border="0" width="100%">', &
            begintr//begintd//'Username '//endtd//begintd//'<input name="Login" size="'//trim(itoa(MAX_LEN_TEACHER_CODE))// &
            '" value="'//trim(tTeacher)//'">'//endtd//endtr, &
            begintr//begintd//'Teacher name '//endtd//begintd//'<input name="Name" size="'//trim(itoa(MAX_LEN_PERSON_NAME))// &
            '" value="'//trim(wrk%Name)//'">'//endtd//endtr
        ! dept
        write(device,AFORMAT) &
            begintr//begintd//'Unit '//endtd//begintd//'<select name="Department">'
        do i=2,NumDepartments
            if (i/=targetDepartment) then
                j=0
            else
                j=1
            end if
            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(Department(i)%Code)//'">'// &
                trim(Department(i)%Name)
        end do
        write(device,AFORMAT) '</select>'//endtd//endtr
        ! rank
        write(device,AFORMAT) &
            begintr//begintd//'Rank '//endtd//begintd//'<select name="Rank">'
        do i=0,4
            if (i/=wrk%Rank) then
                j=0
            else
                j=1
            end if
            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(itoa(i))//'">'// &
                trim(AcademicRank(i))
        end do
        write(device,AFORMAT) '</select>'//endtd//endtr
        ! step
        write(device,AFORMAT) &
            begintr//begintd//'Step '//endtd//begintd//'<select name="Step">'
        do i=0,12
            if (i/=wrk%Step) then
                j=0
            else
                j=1
            end if
            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(itoa(i))//'">'// &
                trim(RankStep(i))
        end do
        write(device,AFORMAT) '</select>'//endtd//endtr
        ! bachelor
        write(device,AFORMAT) &
            begintr//begintd//'Bachelor '//endtd//begintd//'<input name="Bachelor" size="'// &
            trim(itoa(MAX_LEN_TEACHER_DEGREE))//'" value="'//trim(wrk%Bachelor)//'">'//endtd//endtr
        ! master
        write(device,AFORMAT) &
            begintr//begintd//'Master '//endtd//begintd//'<input name="Master" size="'// &
            trim(itoa(MAX_LEN_TEACHER_DEGREE))//'" value="'//trim(wrk%Master)//'">'//endtd//endtr
        ! doctorate
        write(device,AFORMAT) &
            begintr//begintd//'Doctorate '//endtd//begintd//'<input name="Doctorate" size="'// &
            trim(itoa(MAX_LEN_TEACHER_DEGREE))//'" value="'//trim(wrk%Doctorate)//'">'//endtd//endtr
        ! specialization
        write(device,AFORMAT) &
            begintr//begintd//'Specialization '//endtd//begintd//'<input name="Specialization" size="'// &
            trim(itoa(MAX_LEN_TEACHER_DEGREE))//'" value="'//trim(wrk%Specialization)//'">'//endtd//endtr
        ! max load
        write(device,AFORMAT) &
            begintr//begintd//'Max load '//endtd//begintd//'<input name="Load" size="3" value="'// &
            trim(itoa(wrk%MaxLoad))//'">'//endtd//endtr

        ! Role
        if ( is_dean_of_college(Department(Teacher(tdx)%DeptIdx)%CollegeIdx, orHigherUp) ) then

            k = -1
            if (isRoleSysAd .or. isRoleOfficial) k = MAX_ALL_ROLES
            if (isRoleDean)  k = 3
            write(device,AFORMAT) &
                begintr//begintd//'Role '//endtd//begintd//'<select name="Role">'
            do i=0,k
                if (txtRole(i)/=wrk%Role) then
                    j=0
                else
                    j=1
                end if
                write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(txtRole(i))//'">'//txtRole(i)
            end do
            write(device,AFORMAT) '</select>'//endtd//endtr

            if (.not. isRoleOfficial) then
                write(device,AFORMAT) begintr//begintd//'Password'//endtd//begintd//trim(Password)//  &
                    trim(make_href(fnGenerateTeacherPassword, 'Reset password', &
                        A1=tTeacher, pre=beginsmall, post=endsmall)), &
                    endtd//endtr
            end if
        else
            write(device,AFORMAT) &
                begintr//begintd//'Role '//endtd//begintd//trim(wrk%Role),  &
                endtd//endtr

        end if

        write(device,AFORMAT) &
            endtable//linebreak//nbsp//'<input name="action" type="submit" value="'//trim(tAction)//'"> ', &
            beginitalic//'(or, select another action below)'//enditalic//endform//horizontal

        if ( is_admin_of_college(targetCollege) ) then
            call make_form_start(device, fnDeleteTeacher, tTeacher)
            write(device,AFORMAT) '<table border="0" cellpadding="0" cellspacing="0">', &
                begintr//begintd//beginbold//'Delete teacher'//endbold//tTeacher// &
                '<input type="submit" name="action" value="Delete">'//endform// &
                endtd//endtr//endtable//horizontal
        end if

        call make_form_start(device, fnFindTeacher)
        write(device,AFORMAT) '<table border="0" cellpadding="0" cellspacing="0">', &
            begintr//begintd// &
            beginbold//'Search for teacher'//endbold//' with <input name="A1" value=""> in name. ', &
            '<input type="submit" name="action" value="Search">'//endform// &
            endtd//endtr//endtable//horizontal

    end subroutine teacher_info


    subroutine teacher_schedule_printable(device, thisTerm, NumSections, Section, NumBlocks, Block)
        integer, intent (in) :: device, thisTerm, NumSections, NumBlocks
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        integer :: tLen1, ierr
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        character(len=MAX_LEN_PERSON_NAME) :: tName
        integer, dimension(60,6) :: TimeTable
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
        call timetable_meetings_of_teacher(NumSections, Section, targetTeacher, 0, tLen1, tArray, TimeTable, conflicted)
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
            begintr//tdaligncenter//'Republic of the Philippines'//endtd//endtr, &
            begintr//tdaligncenter//beginbold//trim(UniversityName)//endbold//endtd//endtr, &
            begintr//tdaligncenter//trim(UniversityAddress)//endtd//endtr, &
            begintr//begintd//linebreak//endtd//endtr, &
            begintr//tdaligncenter//trim(College(targetCollege)%Name)//endtd//endtr, &
            begintr//tdaligncenter//beginbold//'INDIVIDUAL FACULTY TEACHING LOAD'//endbold//endtd//endtr, &
            begintr//tdaligncenter//trim(text_term_school_year(thisTerm+3,currentYear))//endtd//endtr, &
            begintr//begintd//linebreak//endtd//endtr, &
            endtable

        tName = Teacher(targetTeacher)%Name
        ierr = index(tName, COMMA//SPACE)
        if (ierr>0) then ! switch first & last names
            tName = trim(tName(ierr+2:))//SPACE//tName(:ierr-1)
        end if

        write(device,AFORMAT) '<table border="0" width="100%">', &
            begintr//'<td width="50%">Name:  '//beginbold//trim(tName)//endbold//endtd, &
            '<td width="50%">Area of Retraining: '//trim(Teacher(targetTeacher)%Specialization)//endtd//endtr
        ! bachelor
        write(device,AFORMAT) &
            begintr//'<td width="50%">Bachelor: '//trim(Teacher(targetTeacher)%Bachelor)//endbold//endtd, &
            '<td width="50%">Faculty Rank: '//trim(AcademicRank(Teacher(targetTeacher)%Rank))//nbsp// &
            trim(RankStep(Teacher(targetTeacher)%Step))//endtd//endtr
        ! master
        write(device,AFORMAT) &
            begintr//'<td width="50%">Master: '//trim(Teacher(targetTeacher)%Master)//endbold//endtd, &
            '<td width="50%">Length of Service in University: '//nbsp//endtd//endtr
        ! doctorate
        write(device,AFORMAT) &
            begintr//'<td width="50%">Doctorate: '//trim(Teacher(targetTeacher)%Doctorate)//endbold//endtd, &
            begintd//nbsp//endtd//endtr
        write(device,AFORMAT) endtable//horizontal

        call teacher_workload(device, thisTerm, Section, tLen1, tArray, NumBlocks, Block)

        write(device,AFORMAT) linebreak//linebreak//linebreak//linebreak//'<table border="0" width="100%">', &
            begintr//'<td width="50%">Prepared by:'//linebreak//linebreak//linebreak//linebreak//linebreak//linebreak// &
            trim(College(targetCollege)%Dean)// &
            linebreak//'College Dean, '//trim(College(targetCollege)%Code)//linebreak//linebreak//linebreak//endtd, &
            '<td width="50%">Received by:'//linebreak//linebreak//linebreak//linebreak//linebreak//linebreak// &
            trim(tName)// &
            linebreak//'Faculty'//linebreak//linebreak//linebreak//endtd//endtr
        if (len_trim(DeanOfCampus)>0) then
            write(device,AFORMAT) &
                begintr//'<td width="50%">Recommending Approval:'//linebreak//linebreak//linebreak//linebreak//linebreak// &
                    linebreak//trim(DeanOfCampus)// &
                linebreak//trim(titleDeanOfCampus)//linebreak//linebreak//linebreak//endtd, &
                '<td width="50%">Recommending Approval:'//linebreak//linebreak//linebreak//linebreak//linebreak//linebreak// &
                trim(DeanOfInstruction)// &
                linebreak//trim(titleDeanOfInstruction)//linebreak//linebreak//linebreak//endtd//endtr
        else
            write(device,AFORMAT) &
                begintr//'<td width="50%">Recommending Approval:'//linebreak//linebreak//linebreak//linebreak//linebreak// &
                    linebreak// &
                trim(DeanOfInstruction)// &
                linebreak//trim(titleDeanOfInstruction)//linebreak//linebreak//linebreak//endtd//tdnbspendtd//endtr
        end if
        write(device,AFORMAT) &
            begintr//'<td width="50%">Recommending Approval:'//linebreak//linebreak//linebreak//linebreak//linebreak//linebreak// &
            trim(VPAcademicAffairs)// &
            linebreak//trim(titleVPAcademicAffairs)//endtd, &
            '<td width="50%">Approved by:'//linebreak//linebreak//linebreak//linebreak//linebreak//linebreak// &
            trim(UniversityPresident)// &
            linebreak//trim(titleUniversityPresident)//endtd//endtr
        write(device,AFORMAT) endtable

    end subroutine teacher_schedule_printable


    subroutine teacher_workload(device, thisTerm, Section, lenSL, SectionList, NumBlocks, Block)
        integer, intent(in) :: device, thisTerm, lenSL, SectionList(3*lenSL+3)
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumBlocks
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        integer :: crse, idx, mdx, rdx, sdx, previous
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

        write(device,AFORMAT) '<table border="0" width="100%">'//begintr, &
            thalignleft//'Subject'//linebreak//'Code'//endth// &
            thalignleft//'Descriptive Title (Units)'//endth// &
            thalignleft//'Program'//linebreak//'Year/Sec'//endth// &
            thalignleft//'Class'//linebreak//'Code'//endth// &
            thalignleft//'Class'//linebreak//'Size'//endth // &
            thalignleft//'Day'//endth// &
            thalignleft//'Time'//endth// &
            thalignleft//'Room'//endth// &
            thalignleft//'Hrs.'//linebreak//'Lect'//endth// &
            thalignleft//'Hrs.'//linebreak//'Lab'//endth// &
            thalignleft//'Work'//linebreak//'Load'//endth// &
            endtr

        previous = 0
        sectionDone = .false.
        do idx=1,lenSL,3
            sdx=SectionList(idx)
            mdx=SectionList(idx+1)
            crse = Section(sdx)%SubjectIdx

            !new section ?
            if (sdx/=previous) then

                previous = sdx
                sectionDone = .false.

                !  subject
                write(device,AFORMAT) begintr//begintd//trim(Subject(crse)%Name)//endtd
                !  subject title, credit
                write(device,'(a,f5.1,a)') begintd//trim(Subject(crse)%Title)//' (', Subject(crse)%Units, ')'//endtd
                ! block
                write(device,AFORMAT) begintd
                call blocks_in_section(device, sdx, 0, thisTerm, NumBlocks, Block)
                write(device,AFORMAT) endtd
                ! section code
                write(device,AFORMAT) begintd//trim(Section(sdx)%Code)//endtd
                ! class size
                write(device,AFORMAT) begintd//trim(itoa(Section(sdx)%Slots))//endtd

            else

                if (sectionDone) cycle

                write(device,AFORMAT) begintr// &
                    tdnbspendtd// & ! subject code
                    tdnbspendtd// & ! title, credit
                    tdnbspendtd// & ! block
                    tdnbspendtd// & ! section code
                    tdnbspendtd     ! class size

            end if

            ! day(s)
            if (is_regular_schedule(sdx, Section)) then
                write(device,AFORMAT) begintd//text_days_of_section(Section(sdx))//endtd
                call class_hours_and_load(-1, sdx, Section, meetingUnits, lectHours, labHours)
                sectionDone = .true.
            else
                write(device,AFORMAT) begintd//txtDay(Section(sdx)%DayIdx(mdx))//endtd
                call class_hours_and_load(mdx, sdx, Section, meetingUnits, lectHours, labHours)
            end if

            ! time
            write(device,AFORMAT) begintd//trim(text_time_period(Section(sdx)%bTimeIdx(mdx), &
                Section(sdx)%eTimeIdx(mdx)))//endtd
            ! room
            rdx = Section(sdx)%RoomIdx(mdx)
            if (rdx > 0) then
                write(device,AFORMAT) begintd//trim(Room(rdx)%Code)//endtd
            else
                write(device,AFORMAT) begintd//'TBA'//endtd
            end if

            totalLect = totalLect + lectHours
            totalLab = totalLab + labHours
            totalUnits = totalUnits + meetingUnits

            if (lectHours>0.0) then
                write(device,'(a,f5.2,a)') begintd, lectHours, endtd ! lect hours
            else
                write(device,AFORMAT) tdnbspendtd
            end if

            if (labHours>0.0) then
                write(device,'(a,f5.2,a)') begintd, labHours, endtd ! lab hours
            else
                write(device,AFORMAT) tdnbspendtd
            end if

            write(device,'(a,f5.2,a)') begintd, meetingUnits, endtd//endtr

        end do
        write(device,AFORMAT) begintr//'<td colspan="11">'//horizontal//endtd//endtr, &
            begintr//tdnbspendtd// & ! subject code
            tdnbspendtd// & ! title, credit
            tdnbspendtd// & ! block
            tdnbspendtd// & ! section code
            tdnbspendtd//  & ! class size
            tdnbspendtd//  & ! day
            tdnbspendtd//  & ! time
            begintd//beginbold//'Totals'//endbold//' : '//endtd ! room
        write(device,'(3(a,f5.2,a))') begintd, totalLect, endtd, & ! hours lect
            begintd, totalLab, endtd, & ! hours lab
            begintd, totalUnits, endtd//endtr ! load
        !write(device,AFORMAT) begintr//'<td colspan="11">'//horizontal//endtd//endtr

        write(device,AFORMAT) endtable

    end subroutine teacher_workload


    subroutine class_hours_and_load(mdx, sdx, Section, meetingUnits, lectHours, labHours)
        integer, intent (in) :: mdx, sdx
        type (TYPE_SECTION), intent(in) :: Section(0:)
        real, intent(out) :: meetingUnits, lectHours, labHours
        integer :: crse
        real :: meetingHours

        call html_comment('class_hours_and_load()')

        crse = Section(sdx)%SubjectIdx
        if (mdx/=-1) then
            meetingHours = (Section(sdx)%eTimeIdx(mdx) - Section(sdx)%bTimeIdx(mdx))/4.0
            if (is_lecture_lab_subject(crse)) then
                if (is_lecture_class(sdx, Section)) then ! lecture of lecture-lab
                    meetingUnits = meetingHours*Subject(crse)%LectLoad/Subject(crse)%LectHours
                    lectHours = meetingHours
                    labHours = 0.0
                else ! lab of lecture-lab
                    meetingUnits = meetingHours*Subject(crse)%LabLoad/Subject(crse)%LabHours
                    lectHours = 0.0
                    labHours = meetingHours
                end if
            else if (Subject(crse)%LectHours>0.0) then ! lecture-only
                meetingUnits = meetingHours*Subject(crse)%LectLoad/Subject(crse)%LectHours
                lectHours = meetingHours
                labHours = 0.0
            else if (Subject(crse)%LabHours>0.0) then ! lab-only
                meetingUnits = meetingHours*Subject(crse)%LabLoad/Subject(crse)%LabHours
                lectHours = 0.0
                labHours = meetingHours
            end if
        else ! assume schedule is correct
            if (is_lecture_lab_subject(crse)) then
                if (is_lecture_class(sdx, Section)) then ! lecture of lecture-lab
                    meetingUnits = Subject(crse)%LectLoad
                    lectHours = Subject(crse)%LectHours
                    labHours = 0.0
                    meetingHours = lectHours
                else ! lab of lecture-lab
                    meetingUnits = Subject(crse)%LabLoad
                    lectHours = 0.0
                    labHours = Subject(crse)%LabHours
                    meetingHours = labHours
                end if
            else if (Subject(crse)%LectHours>0.0) then ! lecture-only
                meetingUnits = Subject(crse)%LectLoad
                lectHours = Subject(crse)%LectHours
                labHours = 0.0
                meetingHours = lectHours
            else if (Subject(crse)%LabHours>0.0) then ! lab-only
                meetingUnits = Subject(crse)%LabLoad
                lectHours = 0.0
                labHours = Subject(crse)%LabHours
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
                if (is_teacher_password(requestingTeacher,t0Password) ) then
                    loginCheckMessage = 'Change current password.'
                else
                    loginCheckMessage = 'Current password is incorrect.'
                    flagIsUp = .true.
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
                    if (is_teacher_password(requestingTeacher, t1Password) .or. &
                            Teacher(requestingTeacher)%TeacherID==t2Password) then
                        loginCheckMessage = 'New password is not valid.'
                    else
                        call set_password(Teacher(requestingTeacher)%Password, t1Password)
                        call xml_write_teachers(trim(pathToYear)//'TEACHERS.XML')
                        loginCheckMessage = 'Successfully changed password for '//USERNAME
                        call html_college_links(device, CollegeIdxUser, mesg=loginCheckMessage)
                        REQUEST = fnCollegeLinks
                        return
                    end if
                else
                    loginCheckMessage = 'New password and repeat do not match.'
                end if
            else
                loginCheckMessage = 'New password and/or repeat not specified.'
            end if
        end if

        call html_write_header(device, 'Change password for '//trim(USERNAME), loginCheckMessage )

        call make_form_start(device, REQUEST)

        if (REQUEST==fnChangeTeacherPassword) write(device,AFORMAT) &
            beginbold//'Old password:'//endbold//linebreak, &
            '<input size="20" type="password" name="C" value="">', &
            linebreak//linebreak
        write(device,AFORMAT) &
            beginbold//'New password:'//endbold//linebreak, &
            '<input size="20" type="password" name="P" value="">', &
            linebreak//linebreak, &
            beginbold//'Repeat new password:'//endbold//linebreak, &
            '<input size="20" type="password" name="R" value="">', &
            linebreak//linebreak, &
            '<input type="submit" value="Update">'//endform//horizontal

    end subroutine change_current_teacher_password


    subroutine recent_teacher_activity(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher, tRole
        integer :: ldx, iTmp !, nDays, sYear, sMonth, sDay, eYear, eMonth, eDay
        character (len=MAX_LEN_FILE_PATH) :: logFile
        logical :: logExists

        ! which teacher ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, iTmp)
        targetTeacher = index_to_teacher(tTeacher)
        call blank_to_underscore(tTeacher, tRole)
        tTeacher = tRole
        iTmp = Teacher(targetTeacher)%DeptIdx
        ldx = Department(iTmp)%CollegeIdx
        tRole = Teacher(targetTeacher)%Role

        call html_comment('recent_teacher_activity()')

        call html_write_header(device, 'Recent activity of '//trim(Teacher(targetTeacher)%Name), SPACE)

        logFile = trim(dirLOG)// &
            trim(College(ldx)%Code)//DIRSEP// &
            trim(tTeacher)//'.log'
        inquire(file=logFile, exist=logExists)
        if (logExists) then
            call copy_to_unit(logFile, device)
        else
            write(device,AFORMAT) BRNONEHR
        end if

    end subroutine recent_teacher_activity


end module EditTEACHERS
