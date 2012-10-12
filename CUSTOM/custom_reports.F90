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


subroutine student_performance (device, mesg)
    implicit none
    integer, intent (in) :: device
    character(len=*), intent (in), optional  :: mesg

    character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
    character(len=MAX_LEN_SUBJECT_CODE) :: tSubject, tArea
    character(len=255) :: AreaNumbers
    integer, dimension(0:20) :: Frequency
    real :: SumEnrolled, SumEarned, SumDown, SumUp, tUnits
    real :: GSumEnrolled, GSumEarned, GSumDown, GSumUp

    integer :: crse, dept, gdx, idx, jdx, ierr, l

    character (len=300) :: line
    integer :: ndels, pos(30)
    integer :: tdx, grd
    integer :: prevtaken
    real :: up, down
    character (len=6) :: token1, token2

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
    write(device,AFORMAT) begintr//'<td colspan="8"><hr>'//endtd//endtr, &
    begintr//begintd//'SUBJECT'//endtd, &
    '<td colspan="4" align="left">COLLEGIATE RECORDS'//endtd, &
    tdaligncenter//'FINAL'//endtd, &
    tdaligncenter//'REMOVAL/'//endtd, &
    tdnbspendtd//tdnbspendtd//tdnbspendtd//tdnbspendtd//endtr, &
    begintr//begintd//'NUMBER'//endtd, &
    '<td colspan="4" align="left">DESCRIPTIVE TITLE OF THE SUBJECT'//endtd, &
    tdaligncenter//'GRADE'//endtd, &
    tdaligncenter//'COMPLETION'//endtd, &
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
            write(device,AFORMAT) begintr//'<td colspan="8"><hr>'//endtd
            if (SumUp*SumDown>0.0) then
                write(device,'(a,f7.2,a,f6.2,a,f8.6,a)') &
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
                ' SEMESTER, '//trim(itoa(TCG(tdx)%Year))//dash// &
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
            !                l = index(tSubject,dash)
            !                if (l > 0) tSubject(l:) = ' '
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
            if (TCG(tdx)%ReExam == 0) then
                line = trim(line)//tdaligncenter//dash//endtd
            else
                line = trim(line)//tdaligncenter//txtGrade(pGrade(TCG(tdx)%ReExam))//endtd
            end if
            !
            if (Subject(crse)%Units == 0) then
                !                    if (is_grade_passing(grd)) then
                !                        if (Subject(crse)%Name(1:3) == 'PE ') then
                !                            line = trim(line)//tdaligncenter//'(2)'//endtd
                !                        else if (Subject(crse)%Name(1:3) == 'MS ') then
                !                            line = trim(line)//tdaligncenter//'(1.5)'//endtd
                !                        else if (Subject(crse)%Name(1:4) == 'LTS ' .or. &
                !                        Subject(crse)%Name(1:5) == 'CWTS ' .or. &
                !                        Subject(crse)%Name(1:5) == 'ROTC ' .or. &
                !                        Subject(crse)%Name(1:5) == 'NSTP ') then
                !                            line = trim(line)//tdaligncenter//'(1.5)'//endtd
                !                        end if
                !                    else
                line = trim(line)//tdnbspendtd
            !                    end if
            else if (is_grade_numeric_pass(grd)) then
                ! numeric pass
                down = Subject(crse)%Units
                up = down*fGrade(grd)
                line = trim(line)//tdaligncenter//trim(ftoa(Subject(crse)%Units,1))//endtd
            !                else if (grd == gdx4) then
            !                    ! 4.0
            !                    if (TCG(tdx)%ReExam == 0) then
            !                        down = Subject(crse)%Units
            !                        up = down*fGrade(grd)
            !                        line = trim(line)//tdnbspendtd
            !                    else if (TCG(tdx)%ReExam == gdx5) then
            !                        down = Subject(crse)%Units
            !                        up = down*4.5
            !                        line = trim(line)//tdnbspendtd
            !                    else  ! 3.0
            !                        down = Subject(crse)%Units
            !                        up = down*3.5
            !                        line = trim(line)//tdaligncenter//trim(ftoa(Subject(crse)%Units,1))//endtd
            !                    end if
            !                else if (grd == gdxINC) then
            !                    ! INC
            !                    if (TCG(tdx)%ReExam == 0) then
            !                        line = trim(line)//tdnbspendtd
            !                    else if (TCG(tdx)%ReExam > 0 .and. TCG(tdx)%ReExam < 10) then
            !                        down = Subject(crse)%Units
            !                        up = down*fGrade(TCG(tdx)%ReExam)
            !                        line = trim(line)//tdaligncenter//trim(ftoa(Subject(crse)%Units,1))//endtd
            !                    else if (TCG(tdx)%ReExam == gdx5) then
            !                        down = Subject(crse)%Units
            !                        up = down*5.0
            !                        line = trim(line)//tdnbspendtd
            !                    else if (TCG(tdx)%ReExam == gdx4) then
            !                        down = Subject(crse)%Units
            !                        up = down*4.0
            !                        line = trim(line)//tdnbspendtd
            !                    end if
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
            write (token1,'(f6.2)') up
            write (token2,'(f6.2)') down
        end if
        write (device,AFORMAT) trim(line)// &
        tdalignright//trim(token1)//endtd// &
        tdalignright//trim(token2)//endtd// &
        tdnbspendtd
        SumUp = SumUp + Up
        SumDown = SumDown + Down
    end do
    ! write summary for last term
    write(device,AFORMAT) begintr//'<td colspan="8"><hr>'//endtd
    if (SumUp*SumDown>0.0) then
        write(device,'(a,f7.2,a,f6.2,a,f8.6,a)') &
        tdalignright, SumUp, endtd// &
        tdalignright, SumDown, endtd// &
        tdalignright, SumUp/SumDown, endtd//endtr
        GSumUp = GSumUp + SumUp
        GSumDown = GSumDown + SumDown
    else
        write(device,AFORMAT) endtr
    end if
    if (GSumUp*GSumDown>0.0) then
        write(device,'(a,f7.2,a,f6.2,a,f8.6,a)') &
        begintr//'<td colspan="8" align="right"><b>General Weighted Average : </b>'//endtd// &
        tdalignright, GSumUp, endtd// &
        tdalignright, GSumDown, endtd// &
        tdalignright, GSumUp/GSumDown, endtd//endtr
    end if

    write(device,AFORMAT) '</table><hr>' !begintr//'<td colspan="8">Grading System: '// &
      !'1 - Excellent; 1.5 - Very Good; 2 - Good; 2.5 - Satisfactory; 3 - Pass; '// &
      !'4 - Conditional failure; 5 - Failure; Inc. - Incomplete; Drp. - Dropped'//endtd//endtr, &
      !'</table><hr>'

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
        Frequency = 0
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
            Frequency(gdx) = Frequency(gdx) + 1
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
        write(device,'(2(a,f5.1),a,f6.4,a)') &
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
    write(device,'(2(a,f5.1),a,f6.4,a)') &
    begintr//tdalignright//'TOTALS'//nbsp// &
    endtd//tdalignright, GSumEnrolled, &
    endtd//tdalignright, GSumEarned, &
    endtd//tdnbspendtd//begintd//'(Excludes 0-credit subjects; includes extra subjects)'//endtd//endtr
    write(device,AFORMAT) '</table><hr>'

    return
end subroutine student_performance

