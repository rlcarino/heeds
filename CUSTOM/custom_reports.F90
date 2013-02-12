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


subroutine student_performance (device, mesg)
    implicit none
    integer, intent (in) :: device
    character(len=*), intent (in), optional  :: mesg

    character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
    character(len=MAX_LEN_SUBJECT_CODE) :: tSubject, tArea
    character(len=255) :: AreaNumbers
    real :: SumEnrolled, SumEarned, SumDown, SumUp, tUnits
    real :: GSumEnrolled, GSumEarned, GSumDown, GSumUp

    integer :: crse, dept, gdx, idx, jdx, ierr, l

    character (len=300) :: line
    integer :: ndels, pos(30)
    integer :: tdx, grd
    integer :: prevtaken
    real :: up, down
    character (len=10) :: token1, token2

    ! which student?
    call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
    targetStudent = index_to_student(tStdNo)
    if (ierr/=0 .or. targetStudent==0) then
        targetDepartment = DeptIdxUser
        targetCollege = CollegeIdxUser
        call html_write_header(device, 'Student performance', '<hr><br>Student "'//trim(tStdNo)//'" - not listed?')
        return
    else
        targetCurriculum = Student(targetStudent)%CurriculumIdx
        targetCollege = Curriculum(targetCurriculum)%CollegeIdx
    end if
    !write(*,*) 'Performance of '//trim(text_student_info(targetStudent))

    call html_write_header(device, text_student_curriculum(targetStudent), mesg)

    ! read checklist
    call read_student_records (targetStudent)

    write(device,AFORMAT) '<br><b>UNOFFICIAL Copy of Grades and Weighted Average by Term</b><table border="0" width="100%">'
    write(device,AFORMAT) begintr//'<td colspan="7"><hr>'//endtd//endtr, &
    begintr//begintd//'SUBJECT'//endtd, &
    '<td colspan="4" align="left">COLLEGIATE RECORDS'//endtd, &
    tdaligncenter//'FINAL'//endtd, &
    tdnbspendtd//tdnbspendtd//tdnbspendtd//tdnbspendtd//endtr, &
    begintr//begintd//'NUMBER'//endtd, &
    '<td colspan="4" align="left">DESCRIPTIVE TITLE OF THE SUBJECT'//endtd, &
    tdaligncenter//'GRADE'//endtd, &
    tdaligncenter//'CREDIT'//endtd, &
    '<td colspan="3" align="right">WTD AVERAGE'//endtd//endtr
    SumUp = 0.0
    SumDown = 0.0
    GSumDown = 0.0
    GSumUp = 0.0
    prevtaken = -1
    do tdx=1,lenTCG
        if (TCG(tdx)%Code /= 2) cycle
        ! ignore removal/completion records; already in TCG()%ReExam
        if (index(TCG(tdx)%txtLine, ',REMOVAL') + &
        index(TCG(tdx)%txtLine, ',COMPLETION') > 0) cycle
        !
        if (prevtaken /= TCG(tdx)%Taken) then
            ! write summary for current term
            write(device,AFORMAT) begintr//'<td colspan="7"><hr>'//endtd
            if (SumUp*SumDown>0.0) then
                write(device,'(a,f8.2,a,f5.1,a,f8.2,a)') &
                tdalignright, SumUp, endtd// &
                tdalignright, SumDown, endtd// &
                tdalignright, SumUp/SumDown, endtd//endtr
                GSumUp = GSumUp + SumUp
                GSumDown = GSumDown + SumDown
            else
                write(device,AFORMAT) endtr
            end if
            ! header for next term
            if (TCG(tdx)%Term == 0) then
                write(device,AFORMAT) &
                begintr//tdnbspendtd//'<td colspan="4" align="left">SUMMER, '// &
                trim(itoa(TCG(tdx)%Year))//endtd//'<td colspan="6">'//nbsp//endtd//endtr
            else
                write(device,AFORMAT) &
                    begintr//tdnbspendtd, &
                    '<td colspan="4" align="left">'//trim(txtSemester(TCG(tdx)%Term))// &
                    ' SEMESTER, '//trim(itoa(TCG(tdx)%Year))//DASH// &
                    trim(itoa(TCG(tdx)%Year+1))//endtd// &
                    '<td colspan="6">'//nbsp//endtd//endtr
            end if
            ! re-initialize accumulators
            prevtaken = TCG(tdx)%Taken
            SumUp = 0.0
            SumDown = 0.0
        end if
        !
        if (TCG(tdx)%ErrorCode > 1) then
            call index_to_delimiters(COMMA, TCG(tdx)%txtLine, ndels, pos)
            line = begintr
            line = trim(line)//begintd//TCG(tdx)%txtLine(pos(4)+1:pos(5)-1)//endtd
            line = trim(line)//'<td colspan="4">'//nbsp//endtd
            line = trim(line)//tdaligncenter//TCG(tdx)%txtLine(pos(7)+1:pos(8)-1)//endtd
            line = trim(line)//'<td colspan="2">DATA ERROR'//endtd//endtr
        !
        else
            crse = TCG(tdx)%Subject
            grd = TCG(tdx)%Grade
            up = 0.0
            down = 0.0
            token1 = SPACE
            token2 = SPACE
            tSubject = Subject(crse)%Name

            line = begintr
            line = trim(line)//begintd//trim(tSubject)//endtd
            line = trim(line)//'<td colspan="4">'//trim(Subject(crse)%Title)//endtd
            if (grd == gdxDRP) then
                line = trim(line)//tdaligncenter//'Drp.'//endtd
            else if (grd == gdxINC) then
                line = trim(line)//tdaligncenter//'Inc.'//endtd
            else if (grd == gdxREGD) then
                line = trim(line)//tdaligncenter//'(NG)'//endtd
            else
                line = trim(line)//tdaligncenter//txtGrade(pGrade(grd))//endtd
            end if

            if (Subject(crse)%Units == 0) then
                line = trim(line)//tdnbspendtd
            else if (is_grade_numeric_pass(grd)) then
                ! numeric pass
                down = Subject(crse)%Units
                up = down*fGrade(grd)
                line = trim(line)//tdaligncenter//trim(ftoa(Subject(crse)%Units,1))//endtd
            else if (grd == gdx5) then
                ! 5.0
                down = Subject(crse)%Units
                up = down*5.0
                line = trim(line)//tdnbspendtd
            !
            else
                ! non numeric grade
                if ( is_grade_passing(grd) ) then
                    line = trim(line)//tdaligncenter//trim(ftoa(Subject(crse)%Units,1))//endtd
                else
                    line = trim(line)//tdnbspendtd
                end if
            end if

        end if
        if (down>0.0) then
            write (token1,'(f8.2)') up
            write (token2,'(f5.1)') down
        end if
        write (device,AFORMAT) trim(line)// &
            tdalignright//trim(token1)//endtd// &
            tdalignright//trim(token2)//endtd// &
        tdnbspendtd
        SumUp = SumUp + Up
        SumDown = SumDown + Down
    end do
    ! write summary for last term
    write(device,AFORMAT) begintr//'<td colspan="7"><hr>'//endtd
    if (SumUp*SumDown>0.0) then
        write(device,'(a,f8.2,a,f5.1,a,f8.2,a)') &
            tdalignright, SumUp, endtd// &
            tdalignright, SumDown, endtd// &
            tdalignright, SumUp/SumDown, endtd//endtr
        GSumUp = GSumUp + SumUp
        GSumDown = GSumDown + SumDown
    else
        write(device,AFORMAT) endtr
    end if
    if (GSumUp*GSumDown>0.0) then
        write(device,'(a,f8.2,a,f5.1,a,f8.2,a)') &
            begintr//'<td colspan="7" align="right"><b>General Weighted Average : </b>'//endtd// &
            tdalignright, GSumUp, endtd// &
            tdalignright, GSumDown, endtd// &
            tdalignright, GSumUp/GSumDown, endtd//endtr
    end if
    write(device,AFORMAT) '</table><hr>'

    write(device,AFORMAT) '<br><b>WEIGHTED AVERAGE BY SUBJECT AREA</b><br><hr>', &
        '<table border="0" width="100%">'// &
        begintr//begintd//'SUBJECT AREA'//endtd// &
        tdalignright//'UNITS'//endtd// &
        tdalignright//'UNITS'//endtd// &
        tdalignright//'WEIGHTED'//endtd// &
        begintd//nbsp//'SUBJECTS'//endtd//endtr, &
        begintr//begintd//nbsp//endtd// &
        tdalignright//'ENROLLED'//endtd// &
        tdalignright//'EARNED'//endtd// &
        tdalignright//'AVERAGE'//endtd// &
        begintd//nbsp//endtd//endtr// &
        begintr//'<td colspan="5"><hr>'//endtd//endtr

    GSumDown = 0.0
    GSumUp = 0.0
    GSumEnrolled = 0.0
    GSumEarned = 0.0
    do idx=1,NumSubjectAreas
        SumDown = 0.0
        SumUp = 0.0
        SumEnrolled = 0.0
        SumEarned = 0.0
        AreaNumbers = SPACE
        do l=lenTCG,1,-1
            if (TCG(l)%Used) cycle ! already counted
            if (TCG(l)%Code/=2) cycle ! not a grade
            crse = TCG(l)%Subject
            tSubject = Subject(crse)%Name
            jdx = index(tSubject,SPACE)
            tArea = tSubject(:jdx)
            if (SubjectArea(idx)%Code/=tArea) cycle
            dept = Subject(crse)%DeptIdx
            gdx = TCG(l)%Grade
            tUnits = max(1.0,1.0*Subject(crse)%Units)
            if (gdx/=gdxLOA .and. gdx/=gdxDRP) then
                if (index(TCG(l)%txtLine, ',REMOVAL') + index(TCG(l)%txtLine, ',COMPLETION') == 0) then
                    SumEnrolled = SumEnrolled + tUnits
                    AreaNumbers = COMMA//trim(tSubject(jdx:))//AreaNumbers
                end if
                if (is_grade_passing(gdx)) SumEarned = SumEarned + tUnits
                if (fGrade(gdx)>0) then
                    SumDown = SumDown + tUnits
                    SumUp = SumUp + tUnits*fGrade(gdx)
                end if
            end if
            TCG(l)%Used = .true.
        end do
        if (SumEnrolled==0.0) cycle ! no units enrolled in this area
        !write(*,*) SubjectArea(idx), SumEnrolled, SumEarned, SumUp/max(1.0,SumDown), trim(AreaNumbers)
        write(device,'(2(a,f5.1),a,f8.2,a)') &
        begintr//begintd//trim(SubjectArea(idx)%Code)// & !' @ '//trim(Department(dept)%Code)// &
        endtd//tdalignright, SumEnrolled, &
        endtd//tdalignright, SumEarned, &
        endtd//tdalignright, SumUp/max(1.0,SumDown), &
        endtd//begintd//nbsp//trim(SubjectArea(idx)%Code)//trim(AreaNumbers(2:))//endtd//endtr
        ! exclude 0-unit subjects
        if (Department(dept)%Code/='DMST' .and. Department(dept)%Code/='DHK' .and. Department(dept)%Code/='BNP') then
            GSumEnrolled = GSumEnrolled + SumEnrolled
            GSumEarned = GSumEarned + SumEarned
            GSumDown = GSumDown + SumDown
            GSumUp = GSumUp + SumUp
        end if
    end do
    write(device,AFORMAT) begintr//'<td colspan="5"><hr>'//endtd//endtr
    write(device,'(2(a,f5.1),a,f8.2,a)') &
    begintr//tdalignright//'TOTALS'//nbsp// &
    endtd//tdalignright, GSumEnrolled, &
    endtd//tdalignright, GSumEarned, &
    endtd//tdnbspendtd//begintd//'(Excludes 0-credit subjects; includes extra subjects)'//endtd//endtr
    write(device,AFORMAT) '</table><hr>'

    return
end subroutine student_performance



subroutine enlistment_write_summary(device, Offering, idxDEPT, maxSubjects)
    integer, intent (in) :: device, idxDEPT, maxSubjects
    type (TYPE_OFFERED_SUBJECTS), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS), intent (in) :: Offering
    integer :: crse, cdx, nlines, nSubjects
    integer :: idxCOLL
    character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
    character (len=4) :: tNote

    nlines = 0
    nSubjects = 0
    write(device,AFORMAT) '<table border="1" width="87%">'
    do cdx=1,NumSubjects+NumAdditionalSubjects
        crse = SubjectRank(cdx)
        !write(*,*) cdx, Subject(crse)%Name, Offering(crse)%Demand
        if (Offering(crse)%Demand == 0) cycle

        if (idxDEPT/=0) then
            idxCOLL = Department(idxDEPT)%CollegeIdx
            if (.not. is_used_in_college_subject(idxCOLL, crse) ) cycle
        end if
        if (mod(nlines,20)==0) &
        write(device,AFORMAT) begintr//'<td width="15%"><i><p>Subject<br></p></i>'//endtd, & ! subject
            '<td width="8%" align="right"><i><p>No. of<br>sections</p></i>'//endtd, & ! no. of sections
            '<td width="8%" align="right"><i><p>Total<br>seats</p></i>'//endtd, & ! total seats
            '<td width="8%" align="right"><i><p>Total<br>accom</p></i>'//endtd, & ! total accom
            '<td width="8%" align="right"><i><p>Open<br>seats</p></i>'//endtd, & !  open seats
            '<td width="4%">'//nbsp//endtd// &
            '<td width="8%" align="right"><i><p>Priority<br>demand</p></i>'//endtd, & ! priority demand
            '<td width="8%" align="right"><i><p>Priority<br>not acc</p></i>'//endtd//endtr ! priority not accom

        tSubject = Subject(crse)%Name

        ! subject
        if (Offering(crse)%NSections>0) then
            tNote = ' '
        else
            tNote = ' (*)'
        end if
        write(device,AFORMAT)  begintr//'<td width="15%">'//trim(tSubject)//tNote//endtd
        ! no. of sections, total seats
        write(device,AFORMAT) tdalignright//trim(itoa(Offering(crse)%NSections))//endtd, &
        tdalignright//trim(itoa(Offering(crse)%TotalSlots))//endtd
        ! total accom
        write(device,AFORMAT) tdalignright//itoa(Offering(crse)%Accommodated)//endtd

        ! open seats
        write(device,AFORMAT) tdalignright//trim(itoa(Offering(crse)%OpenSlots))//endtd//tdnbspendtd
        ! priority demand
        write(device,AFORMAT) tdalignright//trim(itoa(Offering(crse)%Demand))//endtd
        ! priority not accom
        if (Offering(crse)%PriorityNotAccommodated>0) then
            write(device,AFORMAT) trim(make_href(fnNotAccommodated, &
                itoa(Offering(crse)%PriorityNotAccommodated), &
                A1=tSubject, pre=tdalignright, post=endtd//endtr ))
        else
            write(device,AFORMAT) tdalignright//'0'//endtd//endtr
        end if

        nlines = nlines+1

        nSubjects = nSubjects+1
        if (nSubjects>=maxSubjects) exit
    end do
    write(device,AFORMAT) '</table>', &
        '<br>Legends:', &
        '<br><i>No. of sections</i> = sections/labs open', &
        '<br><i>Total seats</i> = Total number of seats, all sections', &
        '<br><i>Total accom</i> = No. of students accommodated in the subject', &
        '<br><i>Priority demand</i> = No. of students who need the subject as specified in their curriculum, '// &
        ' or as a back subject', &
        '<br><i>Priority not accom</i> = priority demand not satisfied', &
        '<br>(*) = no sections open, or subject is needed by graduating students'
    write(device,AFORMAT) '<hr>'
    return
end subroutine enlistment_write_summary
