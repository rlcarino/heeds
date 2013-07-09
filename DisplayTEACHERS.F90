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


module DisplayTEACHERS

    use HTML

    implicit none

contains


    subroutine teacher_list_all (device, fn)
        integer, intent (in) :: device, fn
        integer :: fac, nfacs, nsect(3), mdx, sdx, tdx, term, termStore, ierr
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=127) :: mesg, stats
        integer, dimension(60,6) :: TimeTable
        character(len=1) :: ch
        real :: totalLect(3), lectHours, totalLab(3), labHours, totalUnits(3), meetingUnits
        character (len=80) :: tDesc
        integer :: tYear, tTerm

        ! collect teachers
        tArray = 0
        nfacs = 0
        select case (fn)

            case (fnTeachersByDept)
                ! which department ?
                call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)
                targetDepartment = index_to_dept(tDepartment)
                do tdx=1,NumTeachers+NumAdditionalTeachers
                    fac = TeacherRank(tdx)
                    if (targetDepartment/=Teacher(fac)%DeptIdx) cycle
                    if (trim(Teacher(fac)%Role)==trim(REGISTRAR)) cycle
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
                    if (trim(Teacher(fac)%Role)==trim(REGISTRAR)) cycle
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
                begintr//begintd//'(None?)'//endtd//tdalignright
            if (isRoleAdmin) then
                write(device,AFORMAT) trim(make_href(fnEditTeacher, 'Add teacher', &
                    A1='Guest', pre='<small>('//nbsp, post=' )</small>', alt=SPACE))
            end if
            write(device,AFORMAT) endtd//endtr//'</table>'
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
                '<i>(Numbers per term are: # classes / Lect hrs / Lab hrs)</i>', &
                endtd//tdalignright
            if (isRoleAdmin) then
                write(device,AFORMAT) trim(make_href(fnEditTeacher, 'Add teacher', &
                    A1='Guest', pre='<small>('//nbsp, post=' )</small>', alt=SPACE))
            end if
            write(device,AFORMAT) endtd//endtr//'</table>'

            write(device,AFORMAT) '<table border="0">'//&
                begintr//thalignleft//'Name (Specialization) / Username / Role'//endth
            do term=termBegin,termEnd
                call qualify_term (term, tYear, tTerm, tDesc)
                write(device,AFORMAT) &
                    thaligncenter//txtSemester(tTerm+6)//' Term<br>'// &
                    text_school_year(tYear)//endth
            end do
            write(device,AFORMAT) &
                thaligncenter//'Remark'//endth//endtr

            do tdx=1,nfacs
                fac = tArray(tdx)

                mesg = SPACE
                nsect = 0
                totalUnits = 0.0
                totalLect = 0.0
                totalLab = 0.0

                do tTerm=termBegin,termEnd
                    call qualify_term (tTerm, tYear, term, tDesc)

                    ! collect classes of teacher
                    call timetable_clear(TimeTable)
                    do sdx=1,NumSections(term)
                        call meetings_of_section_by_teacher(NumSections(term), Section(term,0:), &
                            sdx, fac, n_meetings, meetings)
                        if (n_meetings==0) cycle ! teacher not assigned to this section
                        nsect(term) = nsect(term)+1
                        do mdx=1,n_meetings
                            call class_hours_and_load(mdx, sdx, Section(term,0:), meetingUnits, lectHours, labHours)
                            totalLect(term) = totalLect(term) + lectHours
                            totalLab(term) = totalLab(term) + labHours
                            totalUnits(term) = totalUnits(term) + meetingUnits
                        end do
                        ierr = -10
                        call timetable_add_meetings_of_section(NumSections(term), Section(term,0:), &
                            sdx, n_meetings, meetings, TimeTable, ierr)
                        if (ierr /= -10) then
                            mesg = trim(mesg)//SPACE//txtSemester(term+6)
                        end if
                    end do

                end do
                if (len_trim(mesg)>0) mesg = red//'Conflict: '//trim(mesg)//black

                QUERY_put = Teacher(fac)%TeacherID

                write(device,AFORMAT) begintr//begintd//trim(itoa(tdx))//'. '//trim(Teacher(fac)%Name)// &
                    ' ('//trim(Teacher(fac)%Specialization)//') / '// &
                    trim(Teacher(fac)%TeacherID)//' / '//Teacher(fac)%Role

                if (isRoleAdmin .or. (isRoleChair .and. DeptIdxUser==Teacher(fac)%DeptIdx) ) then
                    write(device,AFORMAT) trim(make_href(fnEditTeacher, 'Edit', &
                        A1=QUERY_put, pre=nbsp//'<small>( ', post=' )</small>', alt=SPACE))
                end if

                write(device,AFORMAT) endtd

                termStore = targetTerm
                do term=termBegin,termEnd
                    call qualify_term (term, tYear, targetTerm, tDesc)

                    write(stats,'(i2,2(a,f5.1))') &
                        nsect(targetTerm), SPACE//FSLASH//SPACE, totalLect(targetTerm), SPACE//FSLASH//SPACE, &
                        totalLab(targetTerm) ! , SPACE//FSLASH//SPACE, totalUnits(targetTerm)
                    write(device,AFORMAT) trim(make_href(fnTeacherClasses, stats, &
                        A1=QUERY_put, pre=tdaligncenter//nbsp, post=nbsp//endtd))

                end do
                targetTerm = termStore

                write(device,AFORMAT) tdaligncenter//trim(mesg)//endtd//endtr
            end do
            write(device,AFORMAT) '</table>'

        end if
        write(device,AFORMAT) '<hr>'

    end subroutine teacher_list_all


    subroutine teacher_conflicts (device, NumSections, Section)
        integer, intent (in) :: device
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
                call meetings_of_section_by_teacher(NumSections, Section, sdx, fac, n_meetings, meetings)
                if (n_meetings>0) then ! teacher assigned to this section
                    ierr = -10
                    call timetable_add_meetings_of_section(NumSections, Section, sdx, &
                        n_meetings, meetings, TimeTable, ierr)
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
            write(device,AFORMAT) '(None?)'
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
                thaligncenter//'No. of<br>classes'//endth// &
                thaligncenter//'Lect<br>hours'//endth// &
                thaligncenter//'Lab<br>hours'//endth// &
                thaligncenter//'Teaching<br>load'//endth// &
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
                    call meetings_of_section_by_teacher(NumSections, Section, sdx, fac, n_meetings, meetings)
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
                        call timetable_add_meetings_of_section(NumSections, Section, sdx, n_meetings, meetings, TimeTable, ierr)
                        if (ierr /= -10) then
                            mesg = red//'Conflict!'//black
                        end if
                    end if
                end do
                QUERY_put = Teacher(fac)%TeacherID

                write(device,AFORMAT) begintr//begintd//trim(itoa(tdx))//'. '//trim(Teacher(fac)%Name)// &
                    ' ('//trim(Teacher(fac)%Specialization)//')'
                if (isRoleAdmin .or. (isRoleChair .and. DeptIdxUser==Teacher(fac)%DeptIdx) ) then
                    write(device,AFORMAT) trim(make_href(fnEditTeacher, 'Edit', &
                        A1=QUERY_put, pre=nbsp//'<small>', post='</small>', alt=SPACE))
                end if
                write(device,AFORMAT) endtd//tdaligncenter//trim(Teacher(fac)%Role)//endtd, &
                    tdaligncenter//itoa(nsect)//trim(make_href(fnTeacherEditSchedule, 'Edit', &
                    A1=QUERY_put, pre=' <small>', post='</small>', alt=SPACE))
                write(device,'(2(a,f5.1), a,f5.2,a)') endtd//tdaligncenter, totalLect, &
                    endtd//tdaligncenter, totalLab, &
                    endtd//tdaligncenter, totalUnits, &
                    '/'//trim(itoa(Teacher(fac)%MaxLoad))// &
                    trim(make_href(fnPrintableWorkload, 'Printable', &
                    A1=QUERY_put, pre=nbsp//'<small>', post='</small>', alt=SPACE))//endtd// &
                    tdaligncenter//trim(mesg)//endtd//endtr
            end do
            write(device,AFORMAT) '</table>'

        end if
        write(device,AFORMAT) '<hr>'

    end subroutine teacher_conflicts


    subroutine teacher_schedule(device, NumSections, Section, LoadSource)
        integer, intent(in), optional :: LoadSource
        integer, intent (in) :: device
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        integer :: mdx, sdx, tLen1, tLen2, ierr, sect, LoadFromDept
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        character(len=MAX_LEN_CLASS_ID) :: tAction, tClassId
        integer, dimension(60,6) :: TimeTable
        logical :: conflicted, assigned, allowed_to_edit
        character(len=127) :: mesg

        ! which teacher?
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, ierr)
        targetTeacher = index_to_teacher(tTeacher)
        targetDepartment = Teacher(targetTeacher)%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx
        allowed_to_edit = isRoleAdmin .or. (isRoleChair .and. targetDepartment==DeptIdxUser)
        mesg = SPACE

        ! check if there are other arguments
        call cgi_get_named_string(QUERY_STRING, 'A2', tAction, ierr)
        if (ierr==0) then ! action is Add or Del
            call cgi_get_named_string(QUERY_STRING, 'A3', tClassId, ierr)
            sect = index_to_section(tClassId, NumSections, Section)
            if (sect>0) then ! target of action is indexed by sect
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
                call xml_write_sections(pathToTerm, NumSections, Section, 0)
            end if
        end if

        call html_write_header(device, 'Teaching schedule of '//trim(Teacher(targetTeacher)%Name), mesg)

        ! collect meetings of teacher targetTeacher
        call timetable_meetings_of_teacher(NumSections, Section, targetTeacher, 0, tLen1, tArray, TimeTable, conflicted)
        !write(*, '(3i6)') (tArray(mdx), mdx=1,tLen1)
        !if (conflicted) write(*,*) 'Conflict in schedule '//trim(Teacher(targetTeacher)%Name)
        call list_sections_to_edit(device, Section, tLen1, tArray, fnTeacherEditSchedule, tTeacher, 'Del', allowed_to_edit)
        !write(*, '(3i6)') (tArray(mdx), mdx=1,tLen1)
        if (tLen1>0) call timetable_display(device, Section, TimeTable)

        write(device,AFORMAT) '<hr>'
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
            if (.not. is_conflict_timetable_with_section(NumSections, Section, sdx, TimeTable)) then ! add to list
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
            call list_sections_to_edit(device, Section, tLen2, tArray(tLen1+1), fnTeacherEditSchedule, tTeacher, 'Add', &
            allowed_to_edit, &
            '<b>Classes with TBA teachers in '//trim(Department(LoadFromDept)%Code)// &
            ' that fit the schedule of '//trim(Teacher(targetTeacher)%Name)//'</b>')
        end if

        ! search for feasible classes in another department?
        call make_form_start(device, fnTeacherEditSchedule, tTeacher)

        write(device,AFORMAT) '<br>Search for feasible classes in : <select name="A4">'
        do mdx=2,NumDepartments
            if (mdx/=LoadFromDept) then
                ierr = 0
            else
                ierr = 1
            end if
            write(device,AFORMAT) '<option value="'//trim(Department(mdx)%Code)//'"'//trim(selected(ierr))//'> '// &
            trim(Department(mdx)%Code)//DASH//trim(Department(mdx)%Name)
        end do
        write(device,AFORMAT) '</select>'//nbsp//'<input type="submit" value="Find classes"></form><hr>'

    end subroutine teacher_schedule


    subroutine teacher_info(device, wrk, header, remark, tAction, tdx)
        integer, intent(in) :: device, tdx
        type (TYPE_TEACHER), intent(in) :: wrk
        character (len=*), intent(in)  :: header, remark, tAction
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        character (len=MAX_LEN_PASSWD_VAR) :: Password
        integer :: i, j

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
        write(device,AFORMAT) &
            begintr//begintd//'Role '//endtd//begintd//'<select name="Role">',  &
            '<option value="Guest">Guest'
        do i=2,NumDepartments-1
            if (trim(Department(i)%Code)/=trim(wrk%Role)) then
                j=0
            else
                j=1
            end if
            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(Department(i)%Code)//'">'// &
                trim(Department(i)%Code)//' Teaching Load Scheduler'
        end do
        done = .false.
        do i=1,NumCurricula-1
            if (done(i)) cycle
            if (trim(CurrProgCode(i))/=trim(wrk%Role)) then
                j=0
            else
                j=1
            end if
            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(CurrProgCode(i))//'">'// &
                trim(CurrProgCode(i))//' Curriculum Adviser'
            do j = i+1,NumCurricula-1
                if (CurrProgCode(i)==CurrProgCode(j)) done(j) = .true.
            end do
        end do
        write(device,AFORMAT) '</select>'//endtd//endtr
        if (isRoleAdmin) then
            write(device,AFORMAT) begintr//begintd//'Password'//endtd//begintd//trim(Password)//  &
                trim(make_href(fnGenerateTeacherPassword, 'Reset password', &
                    A1=tTeacher, pre=' <small>', post='</small>')), &
                endtd//endtr
        end if
        write(device,AFORMAT) &
            '</table><br>'//nbsp//'<input name="action" type="submit" value="'//trim(tAction)//'"></form><hr>'

    end subroutine teacher_info


    subroutine teacher_schedule_printable(device, NumSections, Section, NumBlocks, Block)
        integer, intent (in) :: device
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        integer, intent (in) :: NumBlocks
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        integer :: tLen1, ierr
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        integer, dimension(60,6) :: TimeTable
        logical :: conflicted

        ! which teacher?
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, ierr)
        targetTeacher = index_to_teacher(tTeacher)

        ! collect meetings of teacher targetTeacher
        call timetable_meetings_of_teacher(NumSections, Section, targetTeacher, 0, tLen1, tArray, TimeTable, conflicted)
        if (conflicted) then
            targetCollege = CollegeIdxUser
            targetDepartment = DeptIdxUser
            call html_write_header(device, 'Teacher schedule', '<br><hr>Teacher "'//tTeacher//'": conflict in schedule')
            return
        end if

        targetDepartment = Teacher(targetTeacher)%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx

        write(device,AFORMAT) '<table border="0" width="100%">', &
            begintr//tdaligncenter//'Republic of the Philippines'//endtd//endtr, &
            begintr//tdaligncenter//'<b>'//trim(UniversityName)//'</b>'//endtd//endtr, &
            begintr//tdaligncenter//trim(UniversityAddress)//endtd//endtr, &
            begintr//begintd//'<br>'//endtd//endtr, &
            begintr//tdaligncenter//trim(College(targetCollege)%Name)//endtd//endtr, &
            begintr//tdaligncenter//'<b>INDIVIDUAL FACULTY TEACHING LOAD</b>'//endtd//endtr, &
            begintr//tdaligncenter//trim(txtSemester(targetTerm+3))//' Term, SY '// &
            trim(itoa(currentYear))//DASH//trim(itoa(currentYear+1))//endtd//endtr, &
            begintr//begintd//'<br>'//endtd//endtr, &
            '</table>'

        write(device,AFORMAT) '<table border="0" width="100%">', &
            begintr//'<td width="50%">Name: <b>'//trim(Teacher(targetTeacher)%Name)//'</b>'//endtd, &
            '<td width="50%">Area of Retraining: '//trim(Teacher(targetTeacher)%Specialization)//endtd//endtr
        ! bachelor
        write(device,AFORMAT) &
            begintr//'<td width="50%">Bachelor: '//trim(Teacher(targetTeacher)%Bachelor)//'</b>'//endtd, &
            '<td width="50%">Faculty Rank: '//trim(AcademicRank(Teacher(targetTeacher)%Rank))//nbsp// &
            trim(RankStep(Teacher(targetTeacher)%Step))//endtd//endtr
        ! master
        write(device,AFORMAT) &
            begintr//'<td width="50%">Master: '//trim(Teacher(targetTeacher)%Master)//'</b>'//endtd, &
            '<td width="50%">Length of Service in University: '//nbsp//endtd//endtr
        ! doctorate
        write(device,AFORMAT) &
            begintr//'<td width="50%">Doctorate: '//trim(Teacher(targetTeacher)%Doctorate)//'</b>'//endtd, &
            begintd//nbsp//endtd//endtr
        write(device,AFORMAT) '</table><hr>'

        call teacher_workload(device, Section, tLen1, tArray, NumBlocks, Block)

        write(device,AFORMAT) '<br><br><br><br><table border="0" width="100%">', &
            begintr//'<td width="50%">Prepared by:<br><br><br><br><br><br>'// &
            trim(College(targetCollege)%Dean)// &
            '<br>College Dean, '//trim(College(targetCollege)%Code)//'<br><br><br>'//endtd, &
            '<td width="50%">Received by:<br><br><br><br><br><br>'// &
            trim(Teacher(targetTeacher)%Name)// &
            '<br>Faculty<br><br><br>'//endtd//endtr
        if (len_trim(DeanOfCampus)>0) then
            write(device,AFORMAT) &
                begintr//'<td width="50%">Recommending Approval:<br><br><br><br><br><br>'// &
                trim(DeanOfCampus)// &
                '<br>'//trim(titleDeanOfCampus)//'<br><br><br>'//endtd, &
                '<td width="50%">Recommending Approval:<br><br><br><br><br><br>'// &
                trim(DeanOfInstruction)// &
                '<br>'//trim(titleDeanOfInstruction)//'<br><br><br>'//endtd//endtr
        else
            write(device,AFORMAT) &
                begintr//'<td width="50%">Recommending Approval:<br><br><br><br><br><br>'// &
                trim(DeanOfInstruction)// &
                '<br>'//trim(titleDeanOfInstruction)//'<br><br><br>'//endtd//tdnbspendtd//endtr
        end if
        write(device,AFORMAT) &
            begintr//'<td width="50%">Recommending Approval:<br><br><br><br><br><br>'// &
            trim(VPAcademicAffairs)// &
            '<br>'//trim(titleVPAcademicAffairs)//endtd, &
            '<td width="50%">Approved by:<br><br><br><br><br><br>'// &
            trim(UniversityPresident)// &
            '<br>'//trim(titleUniversityPresident)//endtd//endtr
        write(device,AFORMAT) '</table>'

    end subroutine teacher_schedule_printable


    subroutine teacher_workload(device, Section, lenSL, SectionList, NumBlocks, Block)
        integer, intent(in) :: device, lenSL, SectionList(3*lenSL+3)
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumBlocks
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        integer :: crse, idx, mdx, rdx, sdx, previous
        real :: totalUnits, meetingUnits
        real :: lectHours, labHours, totalLect, totalLab
        logical :: sectionDone

        if (lenSL < 3) then
            write(device,AFORMAT) '<br>No teaching load?<br>'
            return
        end if

        totalUnits = 0.0
        totalLect = 0.0
        totalLab = 0.0

        write(device,AFORMAT) '<table border="0" width="100%">'//begintr, &
            thalignleft//'Subject<br>Code'//endth// &
            thalignleft//'Descriptive Title (Units)'//endth// &
            thalignleft//'Program<br>Year/Sec'//endth// &
            thalignleft//'Class<br>Code'//endth// &
            thalignleft//'Class<br>Size'//endth // &
            thalignleft//'Day'//endth// &
            thalignleft//'Time'//endth// &
            thalignleft//'Room'//endth// &
            thalignleft//'Hrs.<br>Lect'//endth// &
            thalignleft//'Hrs.<br>Lab'//endth// &
            thalignleft//'Work<br>Load'//endth// &
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
                call blocks_in_section(device, sdx, 0, NumBlocks, Block)
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
                write(device,AFORMAT) begintd//text_days_of_section(sdx, 0, Section)//endtd
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
        write(device,AFORMAT) begintr//'<td colspan="11"><hr>'//endtd//endtr, &
            begintr//tdnbspendtd// & ! subject code
            tdnbspendtd// & ! title, credit
            tdnbspendtd// & ! block
            tdnbspendtd// & ! section code
            tdnbspendtd//  & ! class size
            tdnbspendtd//  & ! day
            tdnbspendtd//  & ! time
            begintd//'<b>Totals</b> : '//endtd ! room
        write(device,'(3(a,f5.2,a))') begintd, totalLect, endtd, & ! hours lect
            begintd, totalLab, endtd, & ! hours lab
            begintd, totalUnits, endtd//endtr ! load
        !write(device,AFORMAT) begintr//'<td colspan="11"><hr>'//endtd//endtr

        write(device,AFORMAT) '</table>'

    end subroutine teacher_workload


    subroutine class_hours_and_load(mdx, sdx, Section, meetingUnits, lectHours, labHours)
        integer, intent (in) :: mdx, sdx
        type (TYPE_SECTION), intent(in) :: Section(0:)
        real, intent(out) :: meetingUnits, lectHours, labHours
        integer :: crse
        real :: meetingHours

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
                    loginCheckMessage = 'New password and repeat do not match'
                end if
            else
                loginCheckMessage = ''
            end if
        end if

        call html_write_header(device, 'Change password for '//trim(USERNAME), loginCheckMessage )

!        write(device,AFORMAT) &
!            '<html><head><title>'//PROGNAME//VERSION//'</title></head><body>', &
!            '<h2>Change password for '//trim(USERNAME)//'</h2>'
!        if (len_trim(loginCheckMessage)>0) write(device,AFORMAT) &
!            red//trim(loginCheckMessage)//black
        write(device,AFORMAT) &
            '<form name="input" method="post" action="'//trim(CGI_PATH)//'">', &
            '<input type="hidden" name="F" value="'//trim(itoa(REQUEST))//'">', &
            '<input type="hidden" name="N" value="'//trim(USERNAME)//'">'

        if (REQUEST==fnChangeTeacherPassword) write(device,AFORMAT) &
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

    end subroutine change_current_teacher_password


    subroutine generate_teacher_password(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        integer :: tdx

        ! which teacher ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, tdx)
        tdx = index_to_teacher(tTeacher)
        call set_password(Teacher(tdx)%Password)
        call xml_write_teachers(trim(pathToYear)//'TEACHERS.XML')
        call teacher_info(device, Teacher(tdx), 'Edit info for teacher '//tTeacher, &
            SPACE, 'Update', tdx)

    end subroutine generate_teacher_password


end module DisplayTEACHERS
