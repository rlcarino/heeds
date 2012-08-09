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


    subroutine demand_for_subjects (device)
        integer, intent (in) :: device

        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer :: ierr, i, j, l, std, owner_dept, owner_coll
        integer :: crse, nlines
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=4) :: tNote
        real, dimension(0:MAX_ALL_SUBJECTS) :: tCount

        ! which department ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)
        targetDepartment = index_to_dept(tDepartment)
        if (ierr/=0 .or. targetDepartment==0) then
            targetCollege = CollegeIdxUser
            targetDepartment = DeptIdxUser
            call html_write_header(device, 'Demand for subjects', '<br><hr>Department "'//tDepartment//'" not found.')
            return
        end if

        targetCollege = Department(targetDepartment)%CollegeIdx
        owner_dept = targetDepartment
        owner_coll = targetCollege

        !write(device,AFORMAT) '<b>'//trim(Department(targetDepartment)%Name)//', '// &
        !  trim(College(targetCollege)%Code)//'</b><hr><br>'

        call offerings_summarize(NumNextSections, NextSection, NextOffering, owner_dept)

        ! calculate priority demand, priority accomodated/not accommodated
        NextSection(:)%RemSlots = NextSection(:)%Slots

        ! use Advised(:)%Contrib for Offering(:)%Demand
        tCount = 0.0
        do std=1,NumStudents
            if (owner_coll/=Curriculum(Student(std)%CurriculumIdx)%CollegeIdx) cycle ! student not in target college
            ! count all contributions
            do i = 1,Advised(std)%NPriority+Advised(std)%NAlternates+Advised(std)%NCurrent
                j = Advised(std)%Subject(i)
                tCount(j) = tCount(j) + Advised(std)%Contrib(i)
            end do
        end do

        do j=1,NumSubjects+NumAdditionalSubjects
            NextOffering(j)%Demand = tCount(j)
            NextOffering(j)%PriorityNotAccommodated = tCount(j)
        end do

        ! calculate remaining slots, open sections
        do i=1,NumNextSections
            if (index(NextSection(i)%Code,'+')>0) cycle ! an additional schedule
            j = NextSection(i)%SubjectIdx
            if (j==0) cycle ! deleted
            l = NextSection(i)%RemSlots
            if (l==0) cycle

            owner_dept = NextSection(i)%DeptIdx
            owner_coll = Department(owner_dept)%CollegeIdx
            if (owner_dept/=targetDepartment) cycle

            ! lecture-lab ?
            if (.not. is_lecture_lab_subject(j)) then ! lecture only or lab only
                NextOffering(j)%OpenSlots = NextOffering(j)%OpenSlots + l
                NextOffering(j)%OpenSections = NextOffering(j)%OpenSections + 1
            else ! a lecture-lab subject
                if (is_lecture_class(i, NextSection)) then ! this is the lecture section
                else ! this is the lab section
                    NextOffering(j)%OpenSlots = NextOffering(j)%OpenSlots + l
                    NextOffering(j)%OpenSections = NextOffering(j)%OpenSections + 1
                end if
            end if
        end do

        nlines = 0
        call html_write_header(device, 'Demand for subjects and available seats')
        write(device,AFORMAT) '<table border="1" width="50%">'
        do crse=1,NumSubjects+NumAdditionalSubjects

            if (.not. is_used_in_college_subject(targetCollege, crse) ) cycle
            if (NextOffering(crse)%OpenSections==0 .and. NextOffering(crse)%Demand==0) cycle

            if (mod(nlines,20)==0) &
            write(device,AFORMAT) begintr//begintd//'<i><p>Subject<br></p></i>'//endtd, & ! subject
                tdalignright//'<i><p>No. of<br>sects</p></i>'//endtd, & ! no. of sections
                tdalignright//'<i><p>Total<br>seats</p></i>'//endtd, & ! total seats
                tdalignright//'<i><p>Priority<br>demand</p></i>'//endtd ! priority demand

            if (NextOffering(crse)%NSections>0) then
                tNote = ' '
            else
                tNote = ' (*)'
            end if
            tSubject = Subject(crse)%Name
            write(device,AFORMAT)  begintr// &
                begintd//trim(tSubject)//tNote//endtd// & ! subject
                tdalignright//trim(itoa(NextOffering(crse)%NSections))//endtd// & ! no. of sections, total seats
                tdalignright//trim(itoa(NextOffering(crse)%TotalSlots))//endtd// & ! total seats
                !tdalignright//trim(itoa(NextOffering(crse)%Demand))//endtd//endtr ! priority demand
                trim(cgi_make_href(fnPotentialStudents, targetUser, itoa(NextOffering(crse)%Demand), &
                A1=tSubject, A2=Department(targetDepartment)%Code, pre=tdalignright, post=endtd//endtr ))
            nlines = nlines+1
        end do
        write(device,AFORMAT) '</table>', &
            '<br>Legends:', &
            '<br><i>No. of sects</i> = number of sections/labs offered', &
            '<br><i>Total seats</i> = total seats available', &
            '<br><i>Priority demand</i> = number of students who need the subject next semester', &
            '<br>(*) = no sections open; subject may be needed by graduating students'
        write(device,AFORMAT) '<hr>'

        ! reset NextOffering()
        call offerings_summarize(NumNextSections, NextSection, NextOffering)

        return

    end subroutine demand_for_subjects


    subroutine list_potential_students(device)
        integer, intent (in) :: device
        integer :: n_count, tdx, std, ierr, ncol
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        ! which subject ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tSubject, ierr)
        targetSubject = index_to_subject(tSubject)
        if (ierr/=0 .or. targetSubject==0) then
            targetCollege = CollegeIdxUser
            targetDepartment = DeptIdxUser
            call html_write_header(device, 'Demand for subjects', '<br><hr>Subject "'//tSubject//'" not found.')
            return
        end if

        call cgi_get_named_string(QUERY_STRING, 'A2', tDepartment, ierr)
        targetDepartment = index_to_dept(tDepartment)
        targetCollege = Department(targetDepartment)%CollegeIdx
        !write(device,AFORMAT) &
        !    '<b>'//Department(targetDepartment)%Name//trim(Subject(targetSubject)%Name)//' - '// &
        !    trim(Subject(targetSubject)%Title)//'</b><br><hr>'
        ! collect students
        n_count = 0
        !tArray = 0
        do tdx=1,NumStudents
            std = StdRank(tdx)
            if (targetCollege/=Curriculum(Student(std)%CurriculumIdx)%CollegeIdx) cycle ! student not in target college
            do ncol=1,Advised(std)%lenSubject
                if (targetSubject/=Advised(std)%Subject(ncol)) cycle  ! not this subject
                if (Advised(std)%Contrib(ncol)==0.0) cycle  ! alternate
                if (Advised(std)%Section(ncol)>0) cycle     ! accommodated
                n_count = n_count+1
                tArray(n_count) = std
            end do
        end do
        call html_write_header(device, 'Demand for '//trim(Subject(targetSubject)%Name))
        call list_students(device, n_count, tArray, targetSubject, Advised)
        write(device,AFORMAT) '<hr>'

        return
    end subroutine list_potential_students

