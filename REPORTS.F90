!======================================================================
!
!    HEEDS (Higher Education Enrollment Decision Support) - A program
!      to create enrollment scenarios for 'next term' in a university
!    Copyright (C) 2012, 2013 Ricolindo L Carino
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


module REPORTS

    use HTML

    implicit none


contains
  
#include "custom_reports.F90"

    subroutine student_distribution(device)
        integer, intent (in), optional :: device
        integer :: CollegeCount(0:MAX_ALL_COLLEGES), ProgramCount(0:MAX_ALL_CURRICULA), CurriculumCount(0:MAX_ALL_CURRICULA)
        integer :: cdx, gdx, ierr, ldx, sdx, n, maxcount, idxCurr
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        !character (len=255) :: longline

        ! which college ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        targetCollege = index_to_college(tCollege)
        if (ierr/=0 .or. targetCollege==0) then
            targetDepartment = DeptIdxUser
            targetCollege = CollegeIdxUser
            call html_write_header(device, 'Add block', '<hr><br>College"'//trim(tCollege)//'" not found')
            return
        end if
        ! no. of students
        ProgramCount= 0 ! in a degree program
        CurriculumCount= 0 ! in a specific curriculum
        CollegeCount = 0 ! per college
        do sdx=1,NumStudents
            ldx = Student(sdx)%CurriculumIdx
            CurriculumCount(ldx) = CurriculumCount(ldx)+1
            cdx = CurrProgNum(ldx)
            n = Curriculum(ldx)%CollegeIdx
            ProgramCount(cdx) = ProgramCount(cdx)+1
            CollegeCount(n) = CollegeCount(n) + 1
        end do

        call html_write_header(device, 'Distribution of students')

        if (tCollege==ADMINISTRATION) then
            write(device,AFORMAT) '<table width="100%" cellspacing="0" cellpadding="0">', &
                begintr//'<td width="10%">.'//endtd// &
                '<td width="60%"><b>DISTRIBUTION OF STUDENTS</b>'//endtd// &
                '<td width="10%" align="right">count'//endtd// &
                '<td width="10%" align="right">% coll'//endtd// &
                '<td width="10%" align="right">% univ'//endtd//endtr
            write(device,AFORMAT) &
                begintr//'<td width="10%">.'//endtd// &
                '<td width="60%" align="right"><b>Total no. of students in '//UniversityCode//'</b>'//endtd// &
                '<td width="10%" align="right"><b>'//trim(itoa(NumStudents))//'</b>'//endtd// &
                '<td width="10%" align="right">.'//endtd//'<td width="10%" align="right"><b>100</b>'//endtd// &
                endtr
            write(device,AFORMAT) &
                begintr//'<td width="10%">College'//endtd// &
                '<td width="60%">Percent, relative to total in university'//endtd// &
                '<td width="10%" align="right">.'//endtd//'<td width="10%" align="right">.'//endtd// &
                '<td width="10%" align="right">.'//endtd// &
                endtr
            maxcount = Numstudents
            do gdx=1,NumColleges
                if (CollegeCount(gdx) <= 0) cycle

                ! code
                !write(device,AFORMAT) &
                !   begintr//'<td width="10%"><small><a href="#'//trim(College(gdx)%Code)//'">'//trim(College(gdx)%Code)//'</a></small>'//endtd
                if (available(fnStudentsDistribution)) then
                    write(device,AFORMAT) trim(cgi_make_href(fnStudentsDistribution, College(gdx)%Code, &
                        A1=College(gdx)%Code, &
                        pre=begintr//'<td width="10%"><small>', post='</small>'//endtd))
                else
                    write(device,AFORMAT)  begintr//'<td width="10%"><small>'//trim(College(gdx)%Code)//'</a></small>'//endtd
                end if
            
                ! bar chart
                write(device,AFORMAT) &
                    '<td width="60%"><table width="100%" border="0" cellpadding="0">'//begintr
                n = (100*CollegeCount(gdx))/maxcount! relative width
                if (CollegeCount(gdx)>0) then
                    write(device,AFORMAT) '<td width="'//trim(itoa(n))//'%" bgcolor="blue">.'//endtd
                else
                    write(device,AFORMAT) begintd//'.'//endtd
                end if
                ! empty filler
                n = max( 100*(maxcount-CollegeCount(gdx))/maxcount, 1)
                write(device,AFORMAT) '<td width="'//trim(itoa(n))//'%">.'//endtd//endtr//'</table>'
                ! count
                write(device,AFORMAT) tdalignright//trim(itoa(CollegeCount(gdx)))//endtd
                ! % in college and % in university
                write(device,'(a,f6.2,a)') &
                    '<td width="10%" align="right">100'//endtd// &
                    '<td width="10%" align="right">', 100.0*CollegeCount(gdx)/maxcount, endtd//endtr
            end do
            write(device,AFORMAT) '</table>'

        else

            if (CollegeCount(targetCollege) <= 0) then
                write(device,AFORMAT) '<br>No students in this college.<br><hr>'
                return
            end if
            ! program-level statistics
            maxcount = CollegeCount(targetCollege)
            !maxcount = 0
            !do ldx=1,NumCurricula
            !  if (Curriculum(ldx)%CollegeIdx /= targetCollege) cycle
            !  cdx = CurrProgNum(ldx)
            !  if (ProgramCount(cdx) > maxcount) maxcount = ProgramCount(cdx)
            !end do
            write(device,AFORMAT) '<table width="100%" cellspacing="0" cellpadding="0">', &
                begintr//'<td width="10%">.'//endtd// &
                '<td width="60%"><b>DISTRIBUTION OF STUDENTS IN '//trim(College(targetCollege)%Code)//'</b>'//endtd// &
                '<td width="10%" align="right">count'//endtd// &
                '<td width="10%" align="right">% coll'//endtd// &
                '<td width="10%" align="right">% univ'//endtd//endtr
            write(device,AFORMAT) &
                begintr//'<td width="10%">.'//endtd// &
                '<td width="60%" align="right"><b>Total no. of students in '//UniversityCode//'</b>'//endtd// &
                '<td width="10%" align="right"><b>'//trim(itoa(NumStudents))//'</b>'//endtd// &
                '<td width="10%" align="right">.'//endtd//'<td width="10%" align="right">.'//endtd// &
                endtr
            write(device,'(a,f6.2,a)') &
                begintr//'<td width="10%">.'//endtd// &
                '<td width="60%" align="right"><b>Total no. of students in '//trim(College(targetCollege)%Code)//'</b>'//endtd// &
                '<td width="10%" align="right"><b>'//trim(itoa(CollegeCount(targetCollege)))//'</b>'//endtd// &
                '<td width="10%" align="right"><b>100</b>'//endtd// &
                '<td width="10%" align="right"><b>', 100.0*CollegeCount(targetCollege)/NumStudents, '</b>'//endtd// &
                begintr
            write(device,AFORMAT) &
                begintr//'<td width="10%">Program'//endtd// &
                '<td width="60%">Percent of students in program, relative to total in college'//endtd// &
                '<td width="10%" align="right">.'//endtd//'<td width="10%" align="right">.'//endtd// &
                '<td width="10%" align="right">.'//endtd// &
                endtr
  
            done = .false.
            do ldx=1,NumCurricula
                if (Curriculum(ldx)%CollegeIdx /= targetCollege) cycle
                cdx = CurrProgNum(ldx)
                if (done(cdx)) cycle ! done
                ! code
                write(device,AFORMAT) &
                    begintr//'<td width="10%"><small><a href="#'//trim(CurrProgCode(ldx))//'">'// &
                    CurrProgCode(ldx)//'</a></small>'//endtd
                ! bar chart
                write(device,AFORMAT) &
                    '<td width="60%"><table width="100%" border="0" cellpadding="0">'//begintr
                n = (100*ProgramCount(cdx))/maxcount ! relative width
                if (ProgramCount(cdx)>0) then
                    write(device,AFORMAT) '<td width="'//trim(itoa(n))//'%" bgcolor="blue">.'//endtd
                else
                    write(device,AFORMAT) begintd//'.'//endtd
                end if
                ! empty filler
                n = max( 100*(maxcount-ProgramCount(cdx))/maxcount, 1)
                write(device,AFORMAT) '<td width="'//trim(itoa(n))//'%">.'//endtd//endtr//'</table>'
                ! count
                write(device,AFORMAT) tdalignright//trim(itoa(ProgramCount(cdx)))//endtd
                ! % in college and % in university
                write(device,'(a,f6.2,a)') &
                    '<td width="10%" align="right">', 100.0*ProgramCount(cdx)/CollegeCount(targetCollege), endtd, &
                    '<td width="10%" align="right">', 100.0*ProgramCount(cdx)/NumStudents, endtd//endtr
                done(cdx) = .true. ! signal done
            end do
            write(device,AFORMAT) '</table><br>'
  
            ! curriculum-level statistics
            done = .false.
            do idxCurr=1,NumCurricula
                if (Curriculum(idxCurr)%CollegeIdx /= targetCollege) cycle
                if (done(idxCurr)) cycle ! done
  
                cdx = CurrProgNum(idxCurr)  ! generic program
                maxcount = max(ProgramCount(cdx),1)
        
                write(device,AFORMAT) '<p><a name="'//trim(CurrProgCode(idxCurr))//'"></a><br><hr>'
                write(device,AFORMAT) '<table width="100%" cellspacing="0" cellpadding="0">', &
                    begintr//'<td width="10%">.'//endtd// &
                    '<td width="60%"><b>Detailed distribution of students in program '//CurrProgCode(idxCurr)//'</b>'//endtd// &
                    '<td width="10%" align="right">count'//endtd// &
                    '<td width="10%" align="right">% coll'//endtd// &
                    '<td width="10%" align="right">% univ'//endtd//endtr
                write(device,'(a,f6.2,a)') &
                    begintr//'<td width="10%">.'//endtd// &
                    '<td width="60%" align="right"><b>Total no. of students in '// &
                    trim(College(targetCollege)%Code)//'</b>'//endtd// &
                    '<td width="10%" align="right"><b>'//trim(itoa(CollegeCount(targetCollege)))//'</b>'//endtd// &
                    '<td width="10%" align="right"><b>100</b>'//endtd// &
                    '<td width="10%" align="right"><b>', 100.0*CollegeCount(targetCollege)/NumStudents, '</b>'//endtd// &
                    begintr
                write(device,'(a,f6.2,a)') &
                    begintr//'<td width="10%">.'//endtd// &
                    '<td width="60%" align="right"><b>Total no. of students in '//trim(CurrProgCode(idxCurr))//'</b>'//endtd// &
                    '<td width="10%" align="right">'//trim(itoa(ProgramCount(cdx)))//endtd// &
                    '<td width="10%" align="right">', 100.0*ProgramCount(cdx)/CollegeCount(targetCollege), endtd, &
                    '<td width="10%" align="right">', 100.0*ProgramCount(cdx)/NumStudents, endtd//endtr
  
                write(device,AFORMAT) &
                    begintr//'<td width="10%">Curriculum'//endtd// &
                    '<td width="60%">Percent of students in curriculum, relative to total in program'//endtd// &
                    '<td width="10%" align="right">.'//endtd//'<td width="10%" align="right">.'//endtd// &
                    '<td width="10%" align="right">.'//endtd// &
                    endtr

                do ldx=1,NumCurricula ! specific curriculum
                    if (CurrProgNum(ldx) /= cdx) cycle
                    ! code
                    write(device,AFORMAT) &
                        trim(cgi_make_href(fnCurriculum, Curriculum(ldx)%Code, &
                        A1=Curriculum(ldx)%Code, &
                        pre=begintr//'<td width="10%"><small>', post='</small>'//endtd))
                    ! bar chart
                    write(device,AFORMAT) &
                        '<td width="60%"><table width="100%" border="0" cellpadding="0">'//begintr
                    n = (100*CurriculumCount(ldx))/maxcount ! relative width
                    if (CurriculumCount(ldx)>0) then
                        write(device,AFORMAT) '<td width="'//trim(itoa(n))//'%" bgcolor="blue">.'//endtd
                    else
                        write(device,AFORMAT) begintd//'.'//endtd
                    end if
                    ! empty filler
                    n = max( 100*(maxcount-CurriculumCount(ldx))/maxcount, 1)
                    write(device,AFORMAT) '<td width="'//trim(itoa(n))//'%">.'//endtd//endtr//'</table>'
                    ! count
                    !write(device,AFORMAT) tdalignright//trim(itoa(CurriculumCount(ldx)))//endtd
                    if (CurriculumCount(ldx) .gt. 0) then
                        write(device,AFORMAT) trim(cgi_make_href(fnStudentsByCurriculum, itoa(CurriculumCount(ldx)), &
                            A1=Curriculum(ldx)%Code, &
                            pre=tdalignright, post=endtd))
                    else
                        write(device,AFORMAT) tdalignright//'0'//endtd
                    end if
                    ! % in college and % in university
                    write(device,'(a,f6.2,a)') &
                        '<td width="10%" align="right">', 100.0*CurriculumCount(ldx)/CollegeCount(targetCollege), endtd, &
                        '<td width="10%" align="right">', 100.0*CurriculumCount(ldx)/NumStudents, endtd//endtr
  
                    done(ldx) = .true. ! signal done
                end do
                write(device,AFORMAT) '</table>'
  
            end do

        end if
        write(device,AFORMAT) '<hr>'

        return
    end subroutine student_distribution


    subroutine enlistment_not_accommodated(device, Preenlisted)
        integer, intent (in) :: device
        type (TYPE_PRE_ENLISTMENT), intent(in) :: Preenlisted(0:)
        integer :: n_count, tdx, std, ierr, ncol
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject

        ! which subject ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tSubject, ierr)
        targetSubject = index_to_subject(tSubject)
        if (ierr/=0 .or. targetSubject==0) then
            write(device,AFORMAT) '<br>'//red//'Subject "'//tSubject//'" not found.'//black//'<br><hr>'
            targetCollege = NumColleges ! trigger 'All colleges' link for Admin
            return
        end if
        targetDepartment = Subject(targetSubject)%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx
        call html_write_header (device, 'Students not accommodated in priority subject '// &
            trim(Subject(targetSubject)%Name)//SPACE//DASH//SPACE//trim(Subject(targetSubject)%Title))
            !'<b>Students not accommodated in priority subject '//trim(Subject(targetSubject)%Name)//' - '// &
            !  trim(Subject(targetSubject)%Title)//'</b><br>', Department(targetDepartment)%Name// &
            !trim(txtSemester(currentTerm+3))//' Term, SY '//trim(itoa(currentYear))//DASH//itoa(currentYear+1)// &
            !'<hr>'
        ! collect students
        n_count = 0
        !tArray = 0
        do tdx=1,NumStudents
            std = StdRank(tdx)
            do ncol=1,Preenlisted(std)%NPriority ! +Preenlisted(std)%NAlternates+Preenlisted(std)%NCurrent
                if (targetSubject/=Preenlisted(std)%Subject(ncol)) cycle  ! not this subject
                if (Preenlisted(std)%Section(ncol)>0) cycle     ! accommodated
                !if (Preenlisted(std)%Contrib(ncol)==0.0) cycle  ! alternate
                n_count = n_count+1
                tArray(n_count) = std
            end do
        end do
  
        call list_students(device, n_count, tArray, targetSubject, Preenlisted)
        write(device,AFORMAT) '<hr>'

        return
    end subroutine enlistment_not_accommodated


    subroutine enlistment_summarize (device, NumSections, Section, Offering, Preenlisted, fn)
        integer, intent (in) :: device, fn
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in out), dimension (0:) :: Section
        type (TYPE_OFFERED_SUBJECTS), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS), intent (in out) :: Offering
        type (TYPE_PRE_ENLISTMENT), intent(in) :: Preenlisted(0:)
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer :: ierr, i, j, k, l, n, std, uload, remain, pos(0:90)
        integer :: allowed_units, underloaded_by
        !real, dimension(0:MAX_ALL_SUBJECTS) :: tCount

        call offerings_summarize(NumSections, Section, Offering)
    
        MaxLoad = 0 ! maximum allowed load
        do std = 1,NumStudents
            if (MaxLoad<Preenlisted(std)%AllowedLoad) MaxLoad = Preenlisted(std)%AllowedLoad
        end do

        ! calculate priority demand, priority accomodated/not accommodated
        Section(:)%RemSlots = Section(:)%Slots

        do std = 1,NumStudents
            do i=1,Preenlisted(std)%NPriority+Preenlisted(std)%NAlternates+Preenlisted(std)%NCurrent
                j = Preenlisted(std)%Subject(i)
                if (i<=Preenlisted(std)%NPriority) then ! priority demand
                    Offering(j)%Demand = Offering(j)%Demand + 1
                end if
                k = Preenlisted(std)%Section(i)
                if (k > 0) then ! accommodated or force enlisted
                    Offering(j)%Accommodated = Offering(j)%Accommodated + 1
                    Section(k)%RemSlots = Section(k)%RemSlots - 1
                else
                    if (i<=Preenlisted(std)%NPriority) then ! priority not accommodated
                        Offering(j)%PriorityNotAccommodated = Offering(j)%PriorityNotAccommodated + 1
                    end if
                end if
            end do
        end do

        ! calculate remaining seats, open sections
        do i=1,NumSections
            if (index(Section(i)%Code,'+')>0) cycle ! an additional schedule
            j = Section(i)%SubjectIdx
            l = Section(i)%RemSlots
            if (l==0) cycle
            ! lecture-lab ?
            if (.not. is_lecture_lab_subject(j)) then ! lecture only or lab only
                Offering(j)%OpenSlots = Offering(j)%OpenSlots + l
                Offering(j)%OpenSections = Offering(j)%OpenSections + 1
            else ! a lecture-lab subject
                if (is_lecture_class(i, Section)) then ! this is the lecture section
                else ! this is the lab section
                    Offering(j)%OpenSlots = Offering(j)%OpenSlots + l
                    Offering(j)%OpenSections = Offering(j)%OpenSections + 1
                end if
            end if
        end do

        select case (fn)

            case (fnEnlistmentSummary)
                ! which department ?
                call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)
                targetDepartment = index_to_dept(tDepartment)
                if (ierr/=0 .or. targetDepartment==0) then
                    targetDepartment = DeptIdxUser
                    targetCollege = CollegeIdxUser
                    call html_write_header(device, 'Enlistment summary', '<hr><br>Department "'//tDepartment//'" not found')
                    return
                end if

                targetCollege = Department(targetDepartment)%CollegeIdx

                call html_write_header(device, 'Enlistment summary for subjects in '// &
                trim(Department(targetDepartment)%Name)//', '//trim(College(targetCollege)%Code))

                !write(device,AFORMAT) '<b>Enlistment summary</b> for subjects in '// &
                !  trim(Department(targetDepartment)%Name)//', '//trim(College(targetCollege)%Code)// &
                !  trim(txtSemester(currentTerm+3))//' Term, SY '//trim(itoa(currentYear))//DASH//itoa(currentYear+1)//'<br>'// &
                !  '<hr><br>'
                do i=1,NumSubjects+NumAdditionalSubjects
                    SubjectRank(i) = i
                end do
                call enlistment_write_summary(device, Offering, targetDepartment, NumSubjects+NumAdditionalSubjects)

            case (fnBottleneck)
                do i=1,NumSubjects+NumAdditionalSubjects
                    SubjectRank(i) = i
                    Offering(i)%SortKey = Offering(i)%PriorityNotAccommodated
                end do
                do i=1,NumSubjects+NumAdditionalSubjects-1
                    do j=i+1,NumSubjects+NumAdditionalSubjects
                        if (Offering(SubjectRank(i))%SortKey <= Offering(SubjectRank(j))%SortKey) then
                            k = SubjectRank(i)
                            SubjectRank(i) = SubjectRank(j)
                            SubjectRank(j) = k
                        end if
                    end do
                end do
                call html_write_header(device, 'Top 100 subjects for which demand > available seats')
                !write(device,AFORMAT) '<b>Top 100 subjects for which demand > available seats</b>, ', &
                !   trim(txtSemester(currentTerm+3))//' Term, SY '//trim(itoa(currentYear))//DASH//itoa(currentYear+1)//'<hr><br>'
                call enlistment_write_summary(device, Offering, 0, 100)
                targetCollege = NumColleges

            case (fnExtraSlots)
                do i=1,NumSubjects+NumAdditionalSubjects
                    SubjectRank(i) = i
                    Offering(i)%SortKey = Offering(i)%OpenSlots
                end do
                do i=1,NumSubjects+NumAdditionalSubjects-1
                    do j=i+1,NumSubjects+NumAdditionalSubjects
                        if (Offering(SubjectRank(i))%SortKey <= Offering(SubjectRank(j))%SortKey) then
                            k = SubjectRank(i)
                            SubjectRank(i) = SubjectRank(j)
                            SubjectRank(j) = k
                        end if
                    end do
                end do
                call html_write_header(device, 'Top 100 subjects for which available seats > demand')
                !write(device,AFORMAT) '<b>Top 100 subjects for which available seats > demand</b>, ', &
                !   trim(txtSemester(currentTerm+3))//' Term, SY '//trim(itoa(currentYear))//DASH//itoa(currentYear+1)//'<hr><br>'
                call enlistment_write_summary(device, Offering, 0, 100)
                targetCollege = NumColleges


            case (fnUnderloadSummary)

                call html_write_header(device, 'Summary of underloading/overloading')
                !write(device,AFORMAT) '<b>Summary of underloading/overloading</b>, ', &
                !   trim(txtSemester(currentTerm+3))//' Term, SY '//trim(itoa(currentYear))//DASH//itoa(currentYear+1)//'<hr><br>'

                write(device,AFORMAT) 'Note: Entry at position (<i>row</i>, <i>column</i>) indicates the number '// &
                    ' of students who were allowed <i>column</i> units but were underloaded(-)/overloaded(+) by '// &
                    ' <i>row</i> units.'
                write(device,AFORMAT) '<table border="1" width="100%">'//begintr//tdnbspendtd

                do j=MaxLoad,0,-1
                    write(device,'(a,i5,a)') tdalignright, j, endtd
                end do
                write(device,AFORMAT) begintd//'Total'//endtd//begintd//'%'//endtd//endtr
                tArray(1:NumStudents) = 0
                n = 0
                do std=1,NumStudents
                    if (Preenlisted(std)%NPriority+Preenlisted(std)%NAlternates == 0) cycle
                    remain = 0
                    do j = 1,Preenlisted(std)%NPriority+Preenlisted(std)%NAlternates
                        remain = remain + Subject(Preenlisted(std)%Subject(j))%Units
                    end do
                    uload = min(Preenlisted(std)%AllowedLoad, remain)
                    do j = 1,Preenlisted(std)%NPriority+Preenlisted(std)%NAlternates
                        if (Preenlisted(std)%Section(j) > 0) then
                            uload = uload - Subject(Preenlisted(std)%Subject(j))%Units
                        end if
                    end do
                    tArray(std) = uload
                    n = n+1
                end do
                pos = 0
                do l=MaxLoad,-6,-1

                    k = 0
                    pos(0:MaxLoad) = 0
                    do std = 1,NumStudents
                        if (Preenlisted(std)%NPriority+Preenlisted(std)%NAlternates == 0 .or. tArray(std) /= l) cycle
                        uload = Preenlisted(std)%AllowedLoad
                        pos(uload) = pos(uload) + 1
                        pos(MaxLoad+1+uload) = pos(MaxLoad+1+uload) + 1
                        k = k+1
                    end do
                    write(device,AFORMAT) begintr//'<td align="center">'//trim(itoa(-l))//endtd
                    do j=MaxLoad,0,-1
                        if (pos(j)>0) then
                            write(device,AFORMAT) trim(cgi_make_href(fnUnderloadedStudents, itoa(pos(j)), &
                                A1=trim(itoa(j)), A2=trim(itoa(l)), pre=tdalignright, post=endtd))
                        else
                            write(device,AFORMAT) tdalignright//'.'//endtd
                        end if
                    end do
                    write(device,AFORMAT) tdalignright//trim(itoa(k))//endtd//tdalignright// &
                        trim(itoa(int((100.0*k)/n+0.5)))//endtd//endtr

                end do
                write(device,'(30a)') begintr//begintd//'Total'//endtd, &
                    (tdalignright//trim(itoabz(pos(MaxLoad+1+j)))//endtd, j=MaxLoad,0,-1),  &
                    tdalignright//trim(itoa(n))//endtd//tdnbspendtd//endtr

                write(device,AFORMAT) begintr//begintd//'Load'//endtd
                do j=MaxLoad,0,-1
                    write(device,'(a,i5,a)') tdalignright, j, endtd
                end do
                write(device,AFORMAT) tdnbspendtd//begintd//'%'//endtd//endtr//'</table>'
                write(device,AFORMAT) 'Note: Entry at position (<i>row</i>, <i>column</i>) indicates the number '// &
                    ' of students who were allowed <i>column</i> units but were underloaded(-)/overloaded(+) by '// &
                    ' <i>row</i> units.<hr>'
                targetCollege = NumColleges


            case (fnUnderloadedStudents)

                call cgi_get_named_integer(QUERY_STRING, 'A1', allowed_units, ierr)
                call cgi_get_named_integer(QUERY_STRING, 'A2', underloaded_by, ierr)

                call html_write_header(device, 'Students allowed '//itoa(allowed_units)//' units, but underloaded by '// &
                    itoa(underloaded_by))
                !write(device,AFORMAT) '<b>Students allowed '//itoa(allowed_units)//' units, but underloaded by '// &
                !   itoa(underloaded_by)//'</b>, ', &
                !   trim(txtSemester(currentTerm+3))//' Term, SY '//trim(itoa(currentYear))//DASH//itoa(currentYear+1)//')<hr><br>'

                write(device,AFORMAT) '<table border="0" width="100%">', &
                    begintr//thalignleft//'Count'//endth// &
                    thalignleft//'STD NO'//endth// &
                    thalignleft//'NAME'//endth// &
                    thalignleft//'CURRICULUM'//endth// &
                    thalignleft//'LINKS'//endth// &
                    endtr

                n = 0
                do std=1,NumStudents
                    if (Preenlisted(std)%NPriority+Preenlisted(std)%NAlternates == 0) cycle
                    if (Preenlisted(std)%AllowedLoad/=allowed_units) cycle
                    remain = 0
                    do j = 1,Preenlisted(std)%NPriority+Preenlisted(std)%NAlternates
                        remain = remain + Subject(Preenlisted(std)%Subject(j))%Units
                    end do
                    uload = min(Preenlisted(std)%AllowedLoad, remain)
                    do j = 1,Preenlisted(std)%NPriority+Preenlisted(std)%NAlternates
                        if (Preenlisted(std)%Section(j) > 0) then
                            uload = uload - Subject(Preenlisted(std)%Subject(j))%Units
                        end if
                    end do
                    if (uload/=underloaded_by) cycle

                    n = n + 1
                    write(device,AFORMAT) begintr// &
                        tdaligncenter//trim(itoa(n))//'.'//endtd// &
                        begintd//Student(std)%StdNo//endtd// &
                        begintd//trim(Student(std)%Name)//endtd// &
                        begintd//trim(Curriculum(Student(std)%CurriculumIdx)%Code)//endtd//begintd

                    if (available(fnChangeMatriculation) ) then
                        write(device,AFORMAT) trim(cgi_make_href(fnChangeMatriculation, 'schedule', &
                            A1=Student(std)%StdNo, &
                            pre=' [ ', post=' ]'))
                    end if
                    if (available(fnEditCheckList) ) then
                        write(device,AFORMAT) trim(cgi_make_href(fnEditCheckList, 'checklist', &
                            A1=Student(std)%StdNo, &
                            pre=' [ ', post=' ]'))
                    end if
                    if (available(fnStudentPerformance) ) then
                        write(device,AFORMAT) trim(cgi_make_href(fnStudentPerformance, 'performance', &
                            A1=Student(std)%StdNo, &
                            pre=' [ ', post=' ]'))
                    end if

                    !end if

                    write(device,AFORMAT) endtd//endtr
                end do
                write(device,AFORMAT) '</table><hr>'
                targetCollege = NumColleges

        end select

        return
    end subroutine enlistment_summarize


end module REPORTS
