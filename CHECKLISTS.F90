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


module CHECKLISTS

    use PRE_ENLISTMENT

    implicit none

    ! worksheet to evaluate the record of a student
    character (len=MAX_LEN_FILE_PATH) :: fileTCG
    integer, parameter :: MAX_LEN_STUDENT_RECORD = 300
    type :: TYPE_STUDENT_RECORD
        integer :: &
        ErrorCode, & ! if record is garbled
        Code, &      ! -1=error, 0=remark, 1=Substitution, 2=Grade
        Year, &      ! actual year grade was given/subject was registered
        Term, &      ! actual term (SUMMER, FIRST, SECOND) grade was given
        Subject, &    ! index to subject
        Grade, &     ! index to grade
        ReExam, &    ! index to re-exam/completion/removal grade
        Taken, &     ! number of terms since grade was given
        UnitsUsed, & ! units used for substitutions
        Reqd(0:5), & ! Plan of study: indices to required subjects
        Subst(0:5)   ! Plan of study: indices to replacement subjects
        logical :: Used  ! record has been used
        character(len=127) :: txtLine ! text of record
        character(len=80) :: errLine  ! error message
    end type TYPE_STUDENT_RECORD
    type (TYPE_STUDENT_RECORD), dimension (MAX_LEN_STUDENT_RECORD) :: TCG
    integer :: lenTCG

    type (TYPE_CURRICULUM) :: CheckList

    type :: TYPE_CHECKLIST_EXTENSION
        integer :: PriorityRank ! index to rank as recommended subject
        integer :: Grade       ! actual or "substitute" grade
        !integer :: SubstIdxPtr ! pointer to substitution rule used for checking satisfaction of prerequisite
        !integer :: Group     ! -2=Credit already earned for subject
                             ! -1=Not a recommended subject
                             ! 0=Subject is not passed and prerequisite not satisfied
                             ! 1=subject is currently registered (may be failed with probalility Prob
                             ! 2=NSTP, PE subject
                             ! 3=subject is scheduled in the student's term in his/her curriculum
                             ! 4=subject is scheduled in previous terms, but not yet passed
                             ! 5=subject is scheduled in succeeding terms, but prerequisite (is already|may be) satisfied
        integer :: EarlyTime ! When is the earliest time the subject can be taken (in semeters, from current semester)?
        integer :: LateTime  ! EarlyTime+Slack
                             ! Slack = No. of sems the student can delay taking the subject without extending degree completion
        real    :: Contrib   ! Contribution to forecast for subject
        logical :: OKPreq, OKCoreq, OKConcPreq
        character (len=MAX_LEN_SUBJECT_CODE) :: &
            Disp_Subject, &  ! named subject, or blank
            Disp_Comment    ! comment
        character (len=MAX_LEN_TEXT_GRADE) :: Disp_Grade ! grade
        character (len=4) :: Disp_Units ! units
        character (len=5) :: Disp_Remarks ! remarks (PriN, AltK)
                        ! *=specify, ?=check prereq, #=repeat/remove/complete,
                        ! %=lapsed 4/INC, @=offered, $=prerequisite satisfied
        character (len=255) :: &
            Disp_Input_Elective, & ! elective input
            Disp_Input_Grade    ! grade input
    end type TYPE_CHECKLIST_EXTENSION

    type (TYPE_CHECKLIST_EXTENSION) :: CLExt(0:MAX_SUBJECTS_IN_CURRICULUM)

    ! private tokens
    character (len=MAX_LEN_FILE_PATH), private :: fileName
    character (len=MAX_LEN_XML_LINE), private :: line
    integer, private :: unitNo=2, eof, ndels, pos(60)


contains


#include "custom_checklists.F90"


    subroutine xml_write_student_grades(std)

        integer, intent (in) :: std

        integer :: idx

        ! training only?
        if (noWrites) return

        ! generate file name
        idx = index(Student(std)%StdNo,DASH)-1
        if (idx<2) idx = 2
        fileName = trim(dirTRANSCRIPTS)//trim(Student(std)%StdNo(1:idx))//DIRSEP//trim(Student(std)%StdNo)//'.XML'

        ! make backup & open new file
        call move_to_backup(fileName)
        call xml_open_file(unitNo, XML_STUDENT_RECORD, fileName, idx)
        write(unitNo,AFORMAT) &
            '    <comment>', &
            '        Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
            FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
            '        StdNo - Student number', &
            '        Name - Student name', &
            '        Gender - M=male, F=female', &
            '        Country - Country index; 1=Philippines', &
            '        Curriculum - Curriculum code', &
            '        Scholarship - Scholarship code', &
            '        Classification - 0=undetermined; 1=NF, 2=SO, 3=JR, 4=SR', &
            '        Grade - YEAR,TERM,SUBJECT,GRADE', &
            '    </comment>'

        call xml_write_character(unitNo, indent0, 'StdNo', Student(std)%StdNo)
        if (trim(Student(std)%Name)/='(not in directory)') &
            call xml_write_character(unitNo, indent0, 'Name', Student(std)%Name)
        if (Student(std)%Gender/=SPACE .and. Student(std)%Gender/='X') &
            call xml_write_character(unitNo, indent0, 'Gender', Student(std)%Gender)
        if (Student(std)%CountryIdx/=1) &
            call xml_write_integer(unitNo,   indent0, 'Country', Student(std)%CountryIdx)
        if (Student(std)%CurriculumIdx/=0) &
            call xml_write_character(unitNo, indent0, 'Curriculum', Curriculum(Student(std)%CurriculumIdx)%Code)
        if (Student(std)%Classification/=-1) &
            call xml_write_integer(unitNo,   indent0, 'Classification', Student(std)%Classification)
        do idx=1,Student(std)%Record(1,0)
            if (Student(std)%Record(4,idx)<=0) cycle
            call xml_write_character(unitNo, indent0, trim(txtGradeType(Student(std)%Record(1,idx))), &
                trim(itoa(Student(std)%Record(2,idx)))//COMMA// &
                trim(txtSemester(Student(std)%Record(3,idx)))//COMMA// &
                trim(Subject(Student(std)%Record(4,idx))%Name)//COMMA// &
                txtGrade(pGrade(Student(std)%Record(5,idx))) )
        end do
        call xml_close_file(unitNo, XML_STUDENT_RECORD)
        return
    end subroutine xml_write_student_grades


    subroutine xml_write_substitutions(std)
        implicit none
        integer, intent (in) :: std

        integer :: crse_required, idx, k, l
        integer :: rank, Year, Term, idxCURR

        ! training only?
        if (noWrites) return

        ! generate file name
        idx = index(Student(std)%StdNo,DASH)-1
        if (idx<2) idx = 2
        fileName = trim(dirSUBSTITUTIONS)//trim(Student(std)%StdNo(1:idx))//DIRSEP//trim(Student(std)%StdNo)//'.XML'

        ! make backup & open new file
        call move_to_backup(fileName)
        call xml_open_file(unitNo, XML_SUBSTITUTIONS, fileName, idx)
        write(unitNo,AFORMAT) &
            '    <comment>', &
            '        Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                        FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
            '        StdNo - Student number', &
            '        Name - Student name', &
            '        Curriculum - Curriculum code', &
            '        Substitution - REQUIRED SUBJECT(S),REPLACEMENT SUBJECT(S)', &
            '    </comment>'

        idxCURR = Student(std)%CurriculumIdx
        call xml_write_character(unitNo, indent0, 'StdNo', Student(std)%StdNo)
        if (trim(Student(std)%Name)/='(not in directory)') &
            call xml_write_character(unitNo, indent0, 'Name', Student(std)%Name)
        if (Student(std)%CurriculumIdx/=0) &
            call xml_write_character(unitNo, indent0, 'Curriculum', Curriculum(idxCURR)%Code)

        l = 0 ! how many unspecified subjects?
        do k=1,Curriculum(idxCURR)%NSubjects
            crse_required = Curriculum(idxCURR)%SubjectIdx(k)
            if (crse_required < 0) l = l + 1
        end do
        if (l>0) then
            write(unitNo,AFORMAT) '    <comment>', &
                '        Substitutions for '//trim(Curriculum(idxCURR)%Code)
            do k=1,Curriculum(idxCURR)%NSubjects
                crse_required = Curriculum(idxCURR)%SubjectIdx(k)
                if (crse_required < 0) then
                    rank = Curriculum(idxCURR)%SubjectTerm(k)
                    call rank_to_year_term(rank, Year, Term)
                    write(unitNo, aformat) indentation(:indent1)// &
                        trim(txtYear(Year))//' Year, '// &
                        trim(txtSemester(Term))//' Term: '// &
                        Subject(crse_required)%Name
                end if
            end do
            write(unitNo,AFORMAT) '    </comment>'
        end if
        l = 0 ! how many entries?
        do idx=1,lenTCG
            if (TCG(idx)%Code==1) l = l+1
        end do
        if (l>0) then
            do idx=1,lenTCG
                if (TCG(idx)%Code/=1) cycle

                !write(*,*) idx, trim(TCG(idx)%txtLine)

                call xml_write_character(unitNo, indent0, 'Substitution')
                if (TCG(idx)%Year>0 .and. TCG(idx)%Year<18) &
                    call xml_write_character(unitNo, indent1, 'Year', txtYear(TCG(idx)%Year) )
                if (TCG(idx)%Term>=0 .and. TCG(idx)%Term<=8) &
                    call xml_write_character(unitNo, indent1, 'Term', txtSemester(TCG(idx)%Term) )
                line = SPACE
                !write(*,*) 'xml_write_substitution : Required=', TCG(idx)%Reqd(0)
                do k=1,TCG(idx)%Reqd(0)

                    !write(*,*) k, Subject(TCG(idx)%Reqd(k))%Name

                    line = COMMA//trim(Subject(TCG(idx)%Reqd(k))%Name)//line
                end do
                call xml_write_character(unitNo, indent1, 'Required', line(2:))
                line = SPACE
                !write(*,*) 'xml_write_substitution : Replacement=', TCG(idx)%Subst(0)
                do k=1,TCG(idx)%Subst(0)

                    !write(*,*) k, Subject(TCG(idx)%Subst(k))%Name

                    line = COMMA//trim(Subject(TCG(idx)%Subst(k))%Name)//line
                end do
                call xml_write_character(unitNo, indent1, 'Replacement', line(2:))
                call xml_write_character(unitNo, indent0, '/Substitution')
            end do
        end if

        call xml_close_file(unitNo, XML_SUBSTITUTIONS)
        return
    end subroutine xml_write_substitutions


    subroutine xml_read_student_grades(std, errNo, openQuietly)

        integer, intent (in) :: std
        integer, intent (out) :: errNo
        logical, optional, intent(in) :: openQuietly

        character(len=MAX_LEN_XML_LINE) :: value
        character(len=MAX_LEN_XML_TAG) :: tag
        type (TYPE_STUDENT) :: wrkStudent
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_TEXT_GRADE) :: tGrade
        character (len=MAX_LEN_TEXT_SEMESTER) :: tTerm
        integer :: cdx, idx, grdType, idxCurr, gdx
        logical :: flag

        if (present(openQuietly)) then
            flag = openQuietly
        else
            flag = .false.
        end if

        ! generate file name
        idx = index(Student(std)%StdNo,DASH)-1
        if (idx<2) idx = 2
        fileName = trim(dirTRANSCRIPTS)//trim(Student(std)%StdNo(1:idx))//DIRSEP//trim(Student(std)%StdNo)//'.XML'

        ! open file, return on any error
        call xml_open_file(unitNo, XML_STUDENT_RECORD, fileName, errNo, forReading, flag)
        if (errNo/=0) return

        ! initialize using existing info
        wrkStudent = Student(std)
        wrkStudent%Record = 0

        ! examine the file line by line
        do
            read(unitNo, AFORMAT, iostat=eof) line
            if (eof<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, eof)
            if (eof/=0) exit

            select case (trim(tag))

                case ('StdNo')
                    ! do nothing; should not change

                case ('Name')
                    call upper_case(value)
                    wrkStudent%Name = adjustl(value)

                case ('Gender')
                    wrkStudent%Gender = adjustl(value)

                case ('Curriculum')
                    tCurriculum = adjustl(value)
                    idxCurr = index_to_curriculum(tCurriculum)
                    if (idxCurr<0) then
                        idxCurr = -idxCurr
                    else if (idxCurr==0) then
                        idxCurr =NumCurricula
                    end if
                    wrkStudent%CurriculumIdx = idxCurr

                case ('Country')
                    wrkStudent%CountryIdx = atoi(value)

                case ('Classification') ! ignore
                    wrkStudent%Classification = atoi(value)

                case default
                    call upper_case(tag)
                    do grdType = 0,3
                        if (trim(tag)==trim(txtGradeType(grdType))) then
                            call index_to_delimiters(COMMA, value, ndels, pos)
                            idx = wrkStudent%Record(1,0)+1
                            wrkStudent%Record(1,0) = idx

                            wrkStudent%Record(1,idx) = grdType ! type
                            wrkStudent%Record(2,idx) = atoi(value(1:pos(2)-1)) ! year
                            tTerm = value(pos(2)+1:pos(3)-1)
                            call upper_case(tTerm)
                            wrkStudent%Record(3,idx) = index_to_term(tTerm) ! term
                            tSubject = value(pos(3)+1:pos(4)-1)
                            wrkStudent%Record(4,idx) = index_to_subject(tSubject) ! subject
                            tGrade = value(pos(4)+1:pos(5)-1)
                            gdx = index_to_grade(tGrade) ! grade
                            if (gdx<=0 .and. &
                                wrkStudent%Record(2,idx)==currentYear .and. &
                                wrkStudent%Record(3,idx)==currentTerm .and. &
                                Period>1) then
                                gdx = gdxREGD
                                !write(*,*) wrkStudent%Record(2,idx), tTerm, tSubject, tGrade, gdx
                            end if
                            wrkStudent%Record(5,idx) = gdx
                            exit
                        end if
                    end do

            end select

        end do
        call xml_close_file(unitNo)

        ! add enlisted subjects
        if (Period>1) then
            do idx=1,Preenlisted(std)%lenSubject
                if (Preenlisted(std)%Section(idx)>0) then
                    flag = .false. ! duplicate not found
                    do cdx=1,wrkStudent%Record(1,0)
                        if (wrkStudent%Record(1,cdx)/=1) cycle ! same grade type?
                        if (wrkStudent%Record(2,cdx)/=currentYear) cycle ! same year?
                        if (wrkStudent%Record(3,cdx)/=currentTerm) cycle ! same term?
                        if (wrkStudent%Record(4,cdx)/=Preenlisted(std)%Subject(idx)) cycle ! same subject?
                        flag = .true.
                        if (wrkStudent%Record(5,cdx)==gdxREGD .and. &
                            Preenlisted(std)%Grade(idx)/=gdxREGD) then ! update grade
                            wrkStudent%Record(5,cdx) = Preenlisted(std)%Grade(idx)
                        end if
                        exit
                    end do
                    if (flag) cycle ! duplicate
                    cdx = wrkStudent%Record(1,0)+1
                    wrkStudent%Record(1,0) = cdx
                    wrkStudent%Record(1,cdx) = 1 ! type=FINALGRADE
                    wrkStudent%Record(2,cdx) = currentYear ! year
                    wrkStudent%Record(3,cdx) = currentTerm ! term
                    wrkStudent%Record(4,cdx) = Preenlisted(std)%Subject(idx) ! subject
                    wrkStudent%Record(5,cdx) = Preenlisted(std)%Grade(idx) ! grade
                end if
            end do
        end if

        ! update student record
        Student(std) = wrkStudent

        ! add to TCG
        do idx=1,Student(std)%Record(1,0) ! Record(i,:) 1=type,2=year,3=term,4=subject,5=grade
            lenTCG = lenTCG + 1
            !Grade,Year,Term,Subject,Section,Units,Grade
            TCG(lenTCG)%txtLine = 'Grade,'// &
                trim(itoa(Student(std)%Record(2,idx)))//COMMA// &
                trim(txtSemester(Student(std)%Record(3,idx)))//COMMA// &
                trim(Subject(Student(std)%Record(4,idx))%Name)//COMMA// &
                trim(txtGradeType(Student(std)%Record(1,idx)))//COMMA// &
                trim(ftoa(Subject(Student(std)%Record(4,idx))%Units,1))//COMMA// &
                txtGrade(pGrade(Student(std)%Record(5,idx)))
            TCG(lenTCG)%Code = 2  ! grade
            TCG(lenTCG)%Year    = Student(std)%Record(2,idx)
            TCG(lenTCG)%Term    = Student(std)%Record(3,idx)
            TCG(lenTCG)%Subject = Student(std)%Record(4,idx)
            TCG(lenTCG)%Grade   = Student(std)%Record(5,idx)
        end do

        return
    end subroutine xml_read_student_grades


    subroutine xml_read_substitutions(std, errNo, openQuietly)

        integer, intent (in) :: std
        integer, intent (out) :: errNo
        logical, optional, intent(in) :: openQuietly

        character(len=MAX_LEN_XML_LINE) :: value
        character(len=MAX_LEN_XML_TAG) :: tag
        character (len=MAX_LEN_TEXT_SEMESTER) :: tTerm
        character (len=MAX_LEN_TEXT_YEAR) :: tYear

        integer :: idx
        logical :: flag

        if (present(openQuietly)) then
            flag = openQuietly
        else
            flag = .false.
        end if

        ! generate file name
        idx = index(Student(std)%StdNo,DASH)-1
        if (idx<2) idx = 2
        fileName = trim(dirSUBSTITUTIONS)//trim(Student(std)%StdNo(1:idx))//DIRSEP//trim(Student(std)%StdNo)//'.XML'

        ! open file, return on any error
        call xml_open_file(unitNo, XML_SUBSTITUTIONS, fileName, errNo, forReading, flag)
        if (errNo/=0) return

        ! examine the file line by line
        do
            read(unitNo, AFORMAT, iostat=eof) line
            if (eof<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, eof)
            if (eof/=0) exit

            !write(*,*) 'xml_read_substitution: '//trim(tag)//' -> '//trim(value)

            select case (trim(tag))

                case ('Substitution')
                    call check_array_bound (lenTCG+1, MAX_LEN_STUDENT_RECORD, 'MAX_LEN_STUDENT_RECORD')

                case ('Year') ! value is one of FIRST, SECOND, THIRD, FOURTH, ...
                    tYear = adjustl(value)
                    TCG(lenTCG+1)%Year = index_to_year(tYear)

                case ('Term') ! value is one of FIRST, SECOND, SUMMER
                    tTerm = adjustl(value)
                    TCG(lenTCG+1)%Term = index_to_term(tTerm)

                case ('Required') ! value is comma-separated list of subjects
                    call tokenize_subjects(value, ',', 5, TCG(lenTCG+1)%Reqd(0), TCG(lenTCG+1)%Reqd(1:), eof)
                    !write(*,*) 'Required=', TCG(lenTCG+1)%Reqd(0)

                case ('Replacement') ! value is comma-separated list of subjects
                    call tokenize_subjects(value, ',', 5, TCG(lenTCG+1)%Subst(0), TCG(lenTCG+1)%Subst(1:), eof)
                    !write(*,*) 'Replacement=', TCG(lenTCG+1)%Subst(0)

                case ('/Substitution')
                    ! PlanOfCoursework,Year,Term,Reqd(1),Reqd(2),...,Reqd(m),Subst(1),Subst(2),...,Subst(n)
                    lenTCG = lenTCG+1
                    line = SPACE
                    do idx=TCG(lenTCG)%Subst(0),1,-1
                        line = COMMA//trim(Subject(TCG(lenTCG)%Subst(idx))%Name)//line
                    end do
                    !write(*,*) 'Replacement='//trim(line(2:))
                    do idx=TCG(lenTCG)%Reqd(0),1,-1
                        line = COMMA//trim(Subject(TCG(lenTCG)%Reqd(idx))%Name)//line
                    end do
                    !write(*,*) 'Required='//trim(line(2:))
                    if (TCG(lenTCG)%Term>=0 .and. TCG(lenTCG)%Term<=8) then
                        tTerm = txtSemester(TCG(lenTCG)%Term)
                    else
                        tTerm = space
                    end if
                    line = COMMA//trim(tTerm)//line

                    if (TCG(lenTCG)%Year>0 .and. TCG(lenTCG)%Year<18) then
                        tYear = txtYear(TCG(lenTCG)%Year)
                    else
                        tYear = space
                    end if
                    TCG(lenTCG)%txtLine = 'PlanOfCoursework,'//trim(tYear)//line
                    TCG(lenTCG)%Code = 1
                    TCG(lenTCG)%Used = .false.

                case default

            end select

        end do
        call xml_close_file(unitNo)

        return
    end subroutine xml_read_substitutions


    subroutine read_student_records (std, DoNotRename)
        integer, intent (in) :: std
        logical, optional, intent (in) :: DoNotRename

        integer :: i, j, k, tdx, ierr

        TCG = TYPE_STUDENT_RECORD (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, .false., SPACE, SPACE)
        lenTCG = 0

        ! retrieve record of substitutions
        call xml_read_substitutions(std, ierr)
        if (ierr/=0) then
            call custom_read_substitutions (std, DoNotRename)
            call xml_write_substitutions(std)
        end if

        ! retrieve record of grades
        call xml_read_student_grades(std, ierr)
        if (ierr/=0) then
            call custom_read_student_grades (std, DoNotRename)
            call xml_write_student_grades(std)
        end if

        ! compute when grades were received relative to baseYear
        do tdx=1,lenTCG
            if (TCG(tdx)%Code/=2) cycle
            if (TCG(tdx)%Year==0 .or. TCG(tdx)%Term==-1) then
                TCG(tdx)%Year = baseYear
                TCG(tdx)%Term = 0
            end if
            TCG(tdx)%Taken = 3*(TCG(tdx)%Year - BaseYear + 1) + TCG(tdx)%Term
        end do

        ! sort TCG; place 0-credit subjects last in each semester
        k = lenTCG + 1
        do i=1,lenTCG-1
            if (TCG(i)%Code/=2) cycle
            do j=i+1,lenTCG
                if (TCG(j)%Code/=2) cycle
                if (TCG(j)%Taken<TCG(i)%Taken) then
                    TCG(k) = TCG(i)
                    TCG(i) = TCG(j)
                    TCG(j) = TCG(k)
                else if (TCG(j)%Taken==TCG(i)%Taken) then
                    if (Subject(TCG(j)%Subject)%Units>Subject(TCG(i)%Subject)%Units) then
                        TCG(k) = TCG(i)
                        TCG(i) = TCG(j)
                        TCG(j) = TCG(k)
                    end if
                end if
            end do
        end do
        TCG(k) = TCG(k+1)

        return
    end subroutine read_student_records


    subroutine custom_read_student_grades (std, DoNotRename)
        integer, intent (in) :: std
        logical, optional, intent (in) :: DoNotRename
        character (len=MAX_LEN_TEXT_YEAR) :: tYear
        character (len=MAX_LEN_TEXT_SEMESTER) :: tTerm
        !type (TYPE_CURRICULUM) :: tCheckList
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject, token
        character (len=MAX_LEN_CURRICULUM_CODE) :: stdCurriculum
        character (len=10) :: tSection
        character (len=4) :: tGrade!, tUnits
        integer :: idxCURR
        integer :: i, j, k, cdx, eof, gdx, tdx
        logical :: fileOK

        idxCURR = Student(std)%CurriculumIdx
        StdNoYearLen = index(Student(std)%StdNo,DASH)-1
        if (StdNoYearLen<=0) StdNoYearLen = StdNoChars
        fileTCG = trim(dirRAW)//'checklists'//DIRSEP//Student(std)%StdNo(1:StdNoYearLen)//DIRSEP//Student(std)%StdNo

        inquire (file=fileTCG, exist=fileOK)
        if (fileOK) then
            open(200, file=fileTCG, form='formatted', status='old')

            ! first line will indicate updated student info
            read (200, AFORMAT, iostat = eof) line
            call index_to_delimiters(COMMA, line, ndels, pos)
            stdCurriculum = line(pos(5)+1:pos(6)-1)
            idxCURR = index_to_curriculum(stdCurriculum)
            if (stdCurriculum==SPACE .or. idxCURR==0) then ! first line does not contain student info
                if (line(1:5)=='Grade') then
                    lenTCG = lenTCG + 1
                    call check_array_bound (lenTCG, MAX_LEN_STUDENT_RECORD, 'MAX_LEN_STUDENT_RECORD')
                    TCG(lenTCG)%txtLine = line
                    TCG(lenTCG)%Code = 0
                end if
            else if (idxCURR<=0) then
                idxCURR = -idxCURR
            end if
            if (idxCURR>0 .and. idxCURR/=Student(std)%CurriculumIdx) then
                Student(std)%CurriculumIdx = idxCURR
                isDirtySTUDENTS = .true.
                write (stderr,AFORMAT) 'UPDATE: '//trim(line)
            else ! revert
                idxCURR = Student(std)%CurriculumIdx
            end if
            do
                read (200, AFORMAT, iostat = eof) line
                if (eof<0) exit
                if (line(1:5)=='Grade') then
                    lenTCG = lenTCG + 1
                    call check_array_bound (lenTCG, MAX_LEN_STUDENT_RECORD, 'MAX_LEN_STUDENT_RECORD')
                    TCG(lenTCG)%txtLine = line
                end if
            end do
            close(200)

        end if

        ! add enlisted subjects
        !Grade,Year,Term,Subject,Section,Units,Grade
        ! 1     2    3      4       5     6     7
        if (Period>1) then
            do i=1,Preenlisted(std)%lenSubject
                if (Preenlisted(std)%Section(i)>0) then
                    lenTCG = lenTCG + 1
                    call check_array_bound (lenTCG, MAX_LEN_STUDENT_RECORD, 'MAX_LEN_STUDENT_RECORD')
                        TCG(lenTCG)%txtLine = 'Grade,'// &
                        trim(itoa(currentYear))//COMMA// &
                        trim(txtSemester(currentTerm))//COMMA// &
                        trim(Subject(Preenlisted(std)%Subject(i))%Name)//COMMA// &
                        trim(CurrentSection(Preenlisted(std)%Section(i))%Code)//COMMA// &
                        trim(ftoa(Subject(Preenlisted(std)%Subject(i))%Units,1))//COMMA// &
                        trim(txtGrade(pGrade(Preenlisted(std)%Grade(i))))
                    !write(*,*) trim(TCG(lenTCG)%txtLine)
                end if
            end do
        end if



        ! parse TCG; assume initial good academic standing, copy TCG to Record()
        Student(std)%Record(1,0) = 0
        !tCheckList = Curriculum(idxCURR)
        loop_tcg: &
        do tdx=1,lenTCG
            line = TCG(tdx)%txtLine
            TCG(tdx)%Used = .false.

            i = index(line, '0A')
            if (i>0) then
                line(i+1:i+1) = 'a'
                TCG(tdx)%txtLine(i+1:i+1) = 'a'
            end if

            !write(*,*) trim(line)

            if (line(1:5)=='Grade') then
                TCG(tdx)%Code = 2
                call index_to_delimiters(COMMA, line, ndels, pos)
                !Grade,Year,Term,Subject,Section,Units,Grade
                ! 1   2    3    4      5       6     7
                if (ndels<7) then
                    TCG(tdx)%ErrorCode = 5
                    TCG(tdx)%errLine = ' ERROR : '
                    TCG(tdx)%errLine(pos(ndels+1):) = ' ^ missing token(s)'
                    !write(*,AFORMAT) trim(TCG(tdx)%txtLine), trim(TCG(tdx)%errLine)
                    cycle loop_tcg
                end if
                tSection = adjustl(line(pos(5)+1:pos(6)-1))
                !tUnits = adjustl(line(pos(6)+1:pos(7)-1))
                tGrade  = adjustl(line(pos(7)+1:pos(8)-1))
                tYear = adjustl(line(pos(2)+1:pos(3)-1))
                tTerm = adjustl(line(pos(3)+1:pos(4)-1))
                tSubject = adjustl(line(pos(4)+1:pos(5)-1))

                !write(*,*) tYear, tTerm, tSubject, tSection, tUnits, tGrade
                gdx = index_to_grade(tGrade)
                !! exclude REGD subjects (i.e. "current")?
                !if (gdx<0 .or. (gdx==gdxREGD .and. excludeREGD)) then
                !  TCG(tdx)%ErrorCode = 16
                !  TCG(tdx)%errLine = ' ERROR : '
                !  TCG(tdx)%errLine(pos(7):) = ' ^ grade not valid, or REGD subjects excluded on purpose'
                !  !write(*,AFORMAT) trim(TCG(tdx)%txtLine), trim(TCG(tdx)%errLine)
                !  cycle loop_tcg
                !end if
                ! exclude invalid grades
                if (gdx<=0) then
                    TCG(tdx)%ErrorCode = 16
                    TCG(tdx)%errLine = ' ERROR : '
                    TCG(tdx)%errLine(pos(7):) = ' ^ grade not valid'
                    !write(*,AFORMAT) trim(TCG(tdx)%txtLine), trim(TCG(tdx)%errLine)
                    cycle loop_tcg
                end if
                ! force current and "future" records to be error; current records are retrieved from ENLISTMENT
                i = atoi(tYear)
                j = index_to_term(tTerm)
                if ( (i .gt. targetYear) .or. &
                (i .eq. targetYear) .and. (j .ge. targetTerm) ) then
                    TCG(tdx)%Code = 0
                    TCG(tdx)%ErrorCode = 14
                    TCG(tdx)%errLine = ' ERROR : '
                    TCG(tdx)%errLine(pos(2):) = ' ^ year/term is in the future?'
                    !write(*,AFORMAT) trim(TCG(tdx)%txtLine), trim(TCG(tdx)%errLine)
                    cycle loop_tcg
                end if

                ! check subject
                cdx = index_to_subject(tSubject)
                if (cdx==0) then
                    TCG(tdx)%ErrorCode = 18
                    TCG(tdx)%errLine = ' ERROR : '
                    TCG(tdx)%errLine(pos(4):) = ' ^ not in catalog'
                    !write(*,AFORMAT) trim(TCG(tdx)%txtLine), trim(TCG(tdx)%errLine)
                    cycle loop_tcg
                end if
                ! Gender-specific PE? Add M or F

                i = index(tSection, DASH)
                if ( (tSubject(1:4)=='PE 2' .or. tSubject(1:4)=='PE 3') .and. &
                     i>0) then
                  tSubject = tSubject(1:4)//DASH//tSection(:i-1)
                  if (tSubject=='PE 2-BB' .or. tSubject=='PE 2-BS' .or. &
                      tSubject=='PE 2-JD' .or. tSubject=='PE 2-SF' .or. &
                      tSubject=='PE 2-SO' .or. tSubject=='PE 2-SW' .or. &
                      tSubject=='PE 2-FUT' .or. &
                      tSubject=='PE 2-VB' .or. tSubject=='PE 2-BD') then
                    tSubject = 'PE 2-'//Student(std)%Gender//tSubject(6:)
                  end if
                  k = index_to_subject(tSubject)
                  cdx = max(cdx, k)
                end if

                ! Called from HTMLTranscripts? Do not renamed subject!
                if (.not. present(DoNotRename)) then
                    k = index_to_new_subject(cdx)
                    if (k/=cdx) then
                        !write(*,*) trim(line)//' - subject taken as '//tSubject// &
                        !  ' renamed '//Subject(k)%Name
                        cdx = k
                    end if
                end if
                j = atoi(tYear)
                k = index_to_term(tTerm)
                TCG(tdx)%Subject = cdx
                TCG(tdx)%Grade = gdx
                TCG(tdx)%Year  = j
                TCG(tdx)%Term = k
                !write(*,*) trim(line)//' : ', j, k, cdx, gdx

                ! copy to RECORD()
                i = Student(std)%Record(1,0) + 1
                Student(std)%Record(1,0) = i
                Student(std)%Record(2,i) = j
                Student(std)%Record(3,i) = k
                Student(std)%Record(4,i) = cdx
                Student(std)%Record(5,i) = gdx
                ! record type
                if (index(TCG(tdx)%txtLine, 'APE')>0) then
                    Student(std)%Record(1,i) = 0
                elseif (index(TCG(tdx)%txtLine, 'REMOVAL')>0) then
                    Student(std)%Record(1,i) = 2
                elseif (index(TCG(tdx)%txtLine, 'COMPLETION')>0) then
                    Student(std)%Record(1,i) = 3
                else
                    Student(std)%Record(1,i) = 1
                end if

                ! check for duplicates
                do i=tdx-1,1,-1
                    if (TCG(i)%Code/=2) cycle
                    if (TCG(i)%Subject/=cdx) cycle
                    if (TCG(i)%Grade/=gdx) cycle
                    if (TCG(i)%Year/=j) cycle
                    if (TCG(i)%Term/=k) cycle
                    ! duplicate: same subject, same grade, same semester
                    TCG(tdx)%Code = 0
                    TCG(tdx)%ErrorCode = 15
                    TCG(tdx)%errLine = ' ERROR : '
                    TCG(tdx)%errLine(pos(2):) = ' ^ duplicate grade?'
                    !write(*,AFORMAT) trim(TCG(tdx)%txtLine), trim(TCG(tdx)%errLine)
                    ! erase from Record()
                    Student(std)%Record(1,0) = Student(std)%Record(1,0) - 1
                    Student(std)%Record(:,Student(std)%Record(1,0)) = 0
                    exit
                end do

            end if
        end do loop_tcg
        ! match COMPLETIONS & REMOVALS with original grades
        do tdx=1,lenTCG
            if (TCG(tdx)%Code/=2) cycle
            cdx = TCG(tdx)%Subject
            if (index(TCG(tdx)%txtLine, 'REMOVAL')>0) then
                k = 0
                if (Subject(cdx)%Name(1:4)/='PE 2' .and. &
                Subject(cdx)%Name(1:4)/='PE 3') then
                    do j=lenTCG,1,-1
                        if (TCG(j)%Code/=2) cycle
                        if (TCG(j)%Subject/=TCG(tdx)%Subject) cycle
                        if (TCG(j)%Grade/=gdx4) cycle
                        k = j
                        exit
                    end do
                else
                    do j=lenTCG,1,-1
                        if (TCG(j)%Code/=2) cycle
                        if (Subject(TCG(j)%Subject)%Name(1:4)/=Subject(cdx)%Name(1:4)) cycle
                        if (TCG(j)%Grade/=gdx4) cycle
                        k = j
                        exit
                    end do
                end if
                if (k>0) then
                    TCG(k)%ReExam = TCG(tdx)%Grade
                ! WARNING: Uncommenting next line will cause errors in forecasting demand
                !           TCG(tdx)%Code = -1
                else
                    write(stderr,AFORMAT) 'Original grade of 4 not found: '// &
                        Student(std)%StdNo//SPACE//trim(TCG(tdx)%txtLine)
                end if
            else if (index(TCG(tdx)%txtLine, 'COMPLETION')>0) then
                k = 0
                if (Subject(cdx)%Name(1:4)/='PE 2' .and. &
                Subject(cdx)%Name(1:4)/='PE 3') then
                    do j=lenTCG,1,-1
                        if (TCG(j)%Code/=2) cycle
                        if (TCG(j)%Subject/=TCG(tdx)%Subject) cycle
                        if (TCG(j)%Grade/=gdxINC) cycle
                        k = j
                        exit
                    end do
                else
                    do j=lenTCG,1,-1
                        if (TCG(j)%Code/=2) cycle
                        if (Subject(TCG(j)%Subject)%Name(1:4)/=Subject(cdx)%Name(1:4)) cycle
                        if (TCG(j)%Grade/=gdxINC) cycle
                        k = j
                        exit
                    end do
                end if
                if (k>0) then
                    TCG(k)%ReExam = TCG(tdx)%Grade
                    ! WARNING: Uncommenting next line will cause errors in forecasting demand
                    !           TCG(tdx)%Code = -1
                else
                    write(stderr,AFORMAT) 'Original grade of INC not found: '// &
                        Student(std)%StdNo//SPACE//trim(TCG(tdx)%txtLine)
                end if
            end if
        end do

        ! remove grade for "CHEM XX" subjects if grade for "CHEM XX.0" and/or "CHEM XX.1" are available
        do tdx=1,lenTCG
            tSubject = Subject(TCG(tdx)%Subject)%Name
            if (tSubject == 'CHEM 40' .or. &
                tSubject == 'CHEM 32' .or. &
                tSubject == 'CHEM 17' .or. &
                tSubject == 'CHEM 15' .or. &
                tSubject == 'CHEM 16' .or. &
                tSubject == 'CHEM 43' .or. &
                tSubject == 'CHEM 44') then

                ! find "CHEM XX.0 and/or "CHEM XX.1"
                token = trim(tSubject)//'.0'
                cdx = index_to_subject(token)
                token = trim(tSubject)//'.1'
                gdx = index_to_subject(token)
                j = 0
                do i=1,lenTCG
                    if (TCG(i)%Code/=2) cycle ! not a grade
                    if (TCG(i)%Subject/=cdx .and. TCG(i)%Subject/=gdx) cycle ! not CHEM XX.0 nor CHEM XX.1
                    if (TCG(i)%Taken/=TCG(tdx)%Taken) cycle ! grade not in the same semester
                    j = i
                    exit
                end do
                if (j>0) then ! found
                    TCG(tdx)%Code = -1
                    !write(*,*) 'Mistake : '//Student(std)%StdNo//DASH//trim(TCG(tdx)%txtLine)
                end if
            end if
        end do

        return
    end subroutine custom_read_student_grades


    subroutine  custom_read_substitutions (std, DoNotRename)
        integer, intent (in) :: std
        logical, optional, intent (in) :: DoNotRename
        character (len=MAX_LEN_TEXT_YEAR) :: tYear
        character (len=MAX_LEN_TEXT_SEMESTER) :: tTerm
        type (TYPE_CURRICULUM) :: tCheckList
        character (len=MAX_LEN_SUBJECT_CODE) :: token
        integer :: q(MAX_LEN_STUDENT_RECORD)
        integer :: idxCURR, pocw
        integer :: i, j, k, cdx, eof, tdx

        idxCURR = Student(std)%CurriculumIdx
        StdNoYearLen = index(Student(std)%StdNo,DASH)-1
        if (StdNoYearLen<=0) StdNoYearLen = StdNoChars
        fileTCG = trim(dirRAW)//'checklists'//DIRSEP//Student(std)%StdNo(1:StdNoYearLen)//DIRSEP//Student(std)%StdNo
        pocw = 0 ! no. of PlanOfStudy entries

        open(200, file=fileTCG, form='formatted', status='old', iostat=eof)
        if (eof==0) then
            do
                read (200, AFORMAT, iostat=eof) line
                if (eof<0) exit
                if (line(1:16)=='PlanOfCoursework') then
                    lenTCG = lenTCG + 1
                    pocw = pocw + 1
                    call check_array_bound (lenTCG, MAX_LEN_STUDENT_RECORD, 'MAX_LEN_STUDENT_RECORD')
                    if (line(17:22)==',PE 2,') line = line(1:16)//',,'//line(17:)
                    TCG(lenTCG)%txtLine = line
                end if
            end do
            close(200)
        end if

        if (pocw==0) then ! get default GE
            fileTCG = trim(dirRAW)//trim(pathToCurrent)//'defaultGE'
            !write(*,*) 'Looking into '//trim(fileTCG)
            open(200, file=fileTCG, form='formatted', status='old', iostat=eof)
            if (eof==0) then
                do
                    read (200, AFORMAT, iostat = eof) line
                    if (eof<0) exit
                    if (line(1:1)=='#' .or. line(1:1)==SPACE) cycle
                    call index_to_delimiters(COMMA, line, ndels, pos)
                    if (line(1:pos(2)-1)/=trim(CurrProgCode(idxCURR))) cycle
                    lenTCG = lenTCG + 1
                    call check_array_bound (lenTCG, MAX_LEN_STUDENT_RECORD, 'MAX_LEN_STUDENT_RECORD')
                    TCG(lenTCG)%txtLine = line(pos(2)+1:)
                  !write(*,*) trim(TCG(lenTCG)%txtLine)
                end do
                close(200)
            end if
        end if

        ! parse TCG; assume initial good academic standing
        tCheckList = Curriculum(idxCURR)
        loop_tcg: &
        do tdx=1,lenTCG
            line = TCG(tdx)%txtLine
            TCG(tdx)%Used = .false.

            i = index(line, '0A')
            if (i>0) then
                line(i+1:i+1) = 'a'
                TCG(tdx)%txtLine(i+1:i+1) = 'a'
            end if

            if (line(1:16)=='PlanOfCoursework') then
                !write(*,*) tdx, trim(line)
                ! PlanOfCoursework,Year,Term,Reqd(1),Reqd(2),...,Reqd(m),Subst(1),Subst(2),...,Subst(n)
                !1                2    3    4                           5
                call index_to_delimiters(COMMA, line, ndels, pos)
                if (ndels<5) then
                    TCG(tdx)%ErrorCode = 25
                    TCG(tdx)%errLine = ' ERROR : '
                    TCG(tdx)%errLine(pos(ndels+1):) = ' ^ missing token(s)'
                    !write(*,AFORMAT) trim(TCG(tdx)%txtLine), trim(TCG(tdx)%errLine)
                    cycle loop_tcg
                end if
                tYear = line(pos(2)+1:pos(3)-1)
                tTerm = line(pos(3)+1:pos(4)-1)
                i = index_to_term(tTerm)
                j = index_to_year(tYear)
                if (i<0 .or. j==0) then
                    TCG(tdx)%ErrorCode = 1
                    TCG(tdx)%errLine = ' WARNING : '
                    TCG(tdx)%errLine(pos(3):) = ' ^ error in YEAR or TERM'
                    TCG(tdx)%Term = tCheckList%NumTerms   ! last semester
                else
                    if (i==0) i = 3
                    TCG(tdx)%Term = (j-1)*3 + i
                end if
                ! check validity of tokens
                q = 0
                do k=4,ndels
                    token = line(pos(k)+1:pos(k+1)-1)

                    ! add gender to certain PE 2 activities
                    if (token=='PE 2-BB' .or. token=='PE 2-BS' .or. &
                        token=='PE 2-JD' .or. token=='PE 2-SF' .or. &
                        token=='PE 2-SO' .or. token=='PE 2-SW' .or. &
                        token=='PE 2-FUT' .or. &
                        token=='PE 2-VB' .or. token=='PE 2-BD') then
                        token = 'PE 2-'//Student(std)%Gender//token(6:)
                    end if

                    i = index_to_subject(token)
                    if (i/=0) then
                        if (.not. present(DoNotRename)) then
                            j = index_to_new_subject(i)
                            if (j/=i) then
                                ! write(*,*) Student(std)%StdNo//' - in plan of study: '//token// &
                                !   ' renamed '//Subject(j)%Name
                                i = j
                            end if
                            q(k) = i
                        end if
                    else
                        TCG(tdx)%ErrorCode = 30
                        TCG(tdx)%errLine = ' ERROR : '
                        TCG(tdx)%errLine(pos(k):) = ' ^ not in catalog'
                        !write(*,AFORMAT) trim(TCG(tdx)%txtLine), trim(TCG(tdx)%errLine)
                        cycle loop_tcg
                    end if
                end do

                ! determine which are required, which are not
                do j=4,ndels
                    token = Subject(q(j))%Name
                    if (token=='ADDITIONAL') then
                        i = TCG(tdx)%Reqd(0) + 1
                        TCG(tdx)%Reqd(0) = i
                        TCG(tdx)%Reqd(i) = q(j)
                    else
                        k = index_of_subject_in_curriculum(tCheckList, q(j))
                        if (k==0) then ! not in curriculum
                            if (q(j)<0) then ! dummy subject that is not in curriculum
                                TCG(tdx)%ErrorCode = 40
                                TCG(tdx)%errLine = ' ERROR : '
                                TCG(tdx)%errLine(pos(j):) = ' ^ not in curriculum; extra dummy?'

                                !write(*,AFORMAT) trim(TCG(tdx)%txtLine), trim(TCG(tdx)%errLine)
                                cycle loop_tcg
                            !else if (token(1:4)=='PE 2') then ! extra PE 2 choice
                            !    i = TCG(tdx)%Reqd(0) + 1
                            !    TCG(tdx)%Reqd(0) = i
                            !    TCG(tdx)%Reqd(i) = q(j)
                            else ! other named subject that is not in curriculum
                                i = TCG(tdx)%Subst(0) + 1
                                TCG(tdx)%Subst(0) = i
                                TCG(tdx)%Subst(i) = q(j)

                              !write(*,*) token//' - other named subject that is not in curriculum'
                            end if
                        else ! required in curriculum
                            ! check if prior token is dummy
                            if (j>4 .and. q(j-1)<0) then ! PlanOfStudy,Year,Term,dummy,Reqd (added previously)
                                i = TCG(tdx)%Subst(0) + 1
                                TCG(tdx)%Subst(0) = i
                                TCG(tdx)%Subst(i) = q(j)

                              !write(*,*) token//' - named subject for a dummy in curriculum'
                            else ! j=4 .or. previous is not a dummy (therefore, substitution?)
                                i = TCG(tdx)%Reqd(0) + 1
                                TCG(tdx)%Reqd(0) = i
                                TCG(tdx)%Reqd(i) = q(j)

                              !write(*,*) token//' - named subject/dummy that is in curriculum'
                            end if

                        end if
                    end if
                end do

                ! if (m==0 or n==0)  then
                !   ERROR
                if (TCG(tdx)%Reqd(0)==0) then
                    TCG(tdx)%ErrorCode = 50
                    TCG(tdx)%errLine = ' ERROR : '
                    TCG(tdx)%errLine(pos(4):) = ' ^ required subject is missing or is extra?'

                    !write(*,AFORMAT) trim(TCG(tdx)%txtLine), trim(TCG(tdx)%errLine)
                    cycle loop_tcg
                else if (TCG(tdx)%Reqd(0)*TCG(tdx)%Subst(0)==0) then
                    TCG(tdx)%ErrorCode = 50
                    TCG(tdx)%errLine = ' ERROR : '
                    TCG(tdx)%errLine(pos(5):) = ' ^ subject is missing or is a duplicate?'

                    !write(*,AFORMAT) trim(TCG(tdx)%txtLine), trim(TCG(tdx)%errLine)
                    cycle loop_tcg
                end if
                ! else if (m==1) then
                !   if (Reqd(1)==ADDITIONAL) then
                !     ADD Subst(1) to Year,Term in curriculum
                if (TCG(tdx)%Reqd(0)==1) then
                    token = Subject(TCG(tdx)%Reqd(1))%Name
                    if (token=='ADDITIONAL') then
                        i = tCheckList%NSubjects
                        do j = 1,TCG(tdx)%Subst(0)
                            tCheckList%NSubjects = i+j
                            tCheckList%SubjectTerm(i+j) = TCG(tdx)%Term
                            tCheckList%SubjectIdx(i+j) = TCG(tdx)%Subst(j)
                        end do
                    !   else if (Reqd(1) is a dummy subject) then
                    !     REPLACE Reqd(1) in Year,Term of curriculum with Subst(1)
                    else if (TCG(tdx)%Reqd(1)<0) then
                        k = 0
                        do i=1,tCheckList%NSubjects
                            if (tCheckList%SubjectTerm(i)==TCG(tdx)%Term .and. &
                            tCheckList%SubjectIdx(i)==TCG(tdx)%Reqd(1)) then
                                k = i
                                exit
                            end if
                        end do
                        if (k>0) then ! found exact position in curriculum
                            tCheckList%SubjectIdx(k) = TCG(tdx)%Subst(1)
                        else ! find a similar dummy not in specified term
                            do i=1,tCheckList%NSubjects
                                if (tCheckList%SubjectIdx(i)==TCG(tdx)%Reqd(1)) then
                                    k = i
                                    exit
                                end if
                            end do
                            if (k>0) then ! found one similar dummy
                                tCheckList%SubjectIdx(k) = TCG(tdx)%Subst(1)
                                TCG(tdx)%ErrorCode = 1
                                TCG(tdx)%errLine = ' WARNING : '
                                TCG(tdx)%errLine(pos(3):) = ' ^ error in YEAR or TERM'
                            else ! none found
                                TCG(tdx)%ErrorCode = 1
                                TCG(tdx)%errLine = ' WARNING : '
                                TCG(tdx)%errLine(pos(4):) = ' ^ not used; extra dummy?'
                            end if
                        end if
                    !   else if (Reqd(1) is a named subject) then
                    !     REPLACE Reqd(1) with Subst(1..n)
                    !   end if
                    else ! if (TCG(tdx)%Reqd(1)>0) then
                        k = 0
                        do i=1,tCheckList%NSubjects
                            if (tCheckList%SubjectIdx(i)==TCG(tdx)%Reqd(1)) then
                                k = i
                                exit
                            end if
                        end do
                        if (k>0) then ! found exact position in curriculum
                            tCheckList%SubjectIdx(k) = TCG(tdx)%Subst(1)
                            i = tCheckList%NSubjects
                            do j = 2,TCG(tdx)%Subst(0)
                                tCheckList%NSubjects = i+j-1
                                tCheckList%SubjectTerm(i+j-1) = tCheckList%SubjectTerm(k)
                                tCheckList%SubjectIdx(i+j-1) = TCG(tdx)%Subst(j)
                            end do
                        else ! not found
                            TCG(tdx)%ErrorCode = 1
                            TCG(tdx)%errLine = ' WARNING : '
                            TCG(tdx)%errLine(pos(4):) = ' ^ not used; duplicate entry?'
                        end if
                    end if
                ! else {group substitution}
                else
                    !   do i=1,min(m,n)
                    !     REPLACE Reqd(i) with Susbt(i)
                    !   end do
                    do cdx=1,min( TCG(tdx)%Reqd(0), TCG(tdx)%Subst(0) )
                        k = 0
                        do i=1,tCheckList%NSubjects
                            if (tCheckList%SubjectIdx(i)==TCG(tdx)%Reqd(cdx)) then
                                k = i
                                exit
                            end if
                        end do
                        if (k>0) then ! found exact position in curriculum
                            tCheckList%SubjectIdx(k) = TCG(tdx)%Subst(cdx)
                        else ! not found
                            TCG(tdx)%ErrorCode = 1
                            TCG(tdx)%errLine = ' WARNING : '
                            TCG(tdx)%errLine(pos(4):) = ' ^ not used; duplicate entry?'
                        end if
                    end do
                    !   do i=min(m,n)+1,m
                    !     REMOVE Reqd(i)
                    !   end do
                    do cdx=min( TCG(tdx)%Reqd(0), TCG(tdx)%Subst(0) )+1, TCG(tdx)%Reqd(0)
                        k = 0
                        do i=1,tCheckList%NSubjects
                            if (tCheckList%SubjectIdx(i)==TCG(tdx)%Reqd(cdx)) then
                                k = i
                                exit
                            end if
                        end do
                        if (k>0) then ! found exact position in curriculum
                            tCheckList%SubjectIdx(k) = 0
                        end if
                    end do
                    !   do i=min(m,n)+1,n
                    !     ADD Subst(i) to Year,Term in curriculum
                    !   end do
                    do cdx=min( TCG(tdx)%Reqd(0), TCG(tdx)%Subst(0) )+1, TCG(tdx)%Subst(0)
                        i = tCheckList%NSubjects+1
                        tCheckList%NSubjects = i
                        tCheckList%SubjectTerm(i) = TCG(tdx)%Term
                        tCheckList%SubjectIdx(i) = TCG(tdx)%Subst(cdx)
                    end do
                ! end if

                end if

                TCG(tdx)%Code = 1

                !write(*,*) 'custom_read_substitution: Required=', TCG(tdx)%Reqd(0)
                !do cdx=1,TCG(tdx)%Reqd(0)
                !    write(*,*) Subject(TCG(tdx)%Reqd(cdx))%Name
                !end do
                !write(*,*) 'custom_read_substitution: Replacement=', TCG(tdx)%Subst(0)
                !do cdx=1,TCG(tdx)%Subst(0)
                !    write(*,*) Subject(TCG(tdx)%Subst(cdx))%Name
                !end do
            end if
        end do loop_tcg

        return
    end subroutine custom_read_substitutions

end module CHECKLISTS
