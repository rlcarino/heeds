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
    integer, private :: eof, ndels, pos(60)


contains


#include "custom_checklists.F90"


    subroutine xml_write_student_grades(std)

        integer, intent (in) :: std

        integer :: idx

        ! training only?
        if (noWrites) return

        ! generate file name
        idx = year_prefix(Student(std))
        fileName = trim(dirTRANSCRIPTS)//trim(Student(std)%StdNo(1:idx))//DIRSEP//trim(Student(std)%StdNo)//'.XML'

        ! write file
        call xml_open_file(unitXML, XML_STUDENT_RECORD, fileName, idx)
        write(unitXML,AFORMAT) &
            '    <comment>', &
            '        Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
            FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
            '        StdNo - Student number', &
            '        Name - Student name', &
            '        Grade - YEAR,TERM,SUBJECT,GRADE', &
            '    </comment>'

        call xml_write_character(unitXML, indent0, 'StdNo', Student(std)%StdNo)
        if (trim(Student(std)%Name)/='(not in directory)') &
            call xml_write_character(unitXML, indent0, 'Name', Student(std)%Name)
        do idx=1,Student(std)%Record(1,0)
            if (Student(std)%Record(4,idx)<=0) cycle
            call xml_write_character(unitXML, indent0, trim(txtGradeType(Student(std)%Record(1,idx))), &
                trim(itoa(Student(std)%Record(2,idx)))//COMMA// &
                trim(txtSemester(Student(std)%Record(3,idx)))//COMMA// &
                trim(Subject(Student(std)%Record(4,idx))%Name)//COMMA// &
                txtGrade(pGrade(Student(std)%Record(5,idx))) )
        end do
        call xml_close_file(unitXML, XML_STUDENT_RECORD)

    end subroutine xml_write_student_grades


    subroutine xml_write_substitutions(std)
        implicit none
        integer, intent (in) :: std

        integer :: crse_required, idx, k, l
        integer :: rank, Year, Term, idxCURR

        ! training only?
        if (noWrites) return

        ! generate file name
        idx = year_prefix(Student(std))
        fileName = trim(dirSUBSTITUTIONS)//trim(Student(std)%StdNo(1:idx))//DIRSEP//trim(Student(std)%StdNo)//'.XML'

        call html_comment('xml_write_substitutions('//trim(filename)//')')

        ! write file
        call xml_open_file(unitXML, XML_SUBSTITUTIONS, fileName, idx)
        write(unitXML,AFORMAT) &
            '    <comment>', &
            '        Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                        FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
            '        StdNo - Student number', &
            '        Name - Student name', &
            '        Curriculum - Curriculum code', &
            '        Substitution - REQUIRED SUBJECT(S),REPLACEMENT SUBJECT(S)', &
            '    </comment>'

        idxCURR = Student(std)%CurriculumIdx
        call xml_write_character(unitXML, indent0, 'StdNo', Student(std)%StdNo)

        if (trim(Student(std)%Name)/='(not in directory)') &
            call xml_write_character(unitXML, indent0, 'Name', Student(std)%Name)

        if (Student(std)%CurriculumIdx/=0) &
            call xml_write_character(unitXML, indent0, 'Curriculum', Curriculum(idxCURR)%Code)

        l = 0 ! how many unspecified subjects?
        do k=1,Curriculum(idxCURR)%NSubjects
            crse_required = Curriculum(idxCURR)%SubjectIdx(k)
            if (crse_required < 0) l = l + 1
        end do

        if (l>0) then
            write(unitXML,AFORMAT) '    <comment>', &
                '        Substitutions for '//trim(Curriculum(idxCURR)%Code)
            do k=1,Curriculum(idxCURR)%NSubjects
                crse_required = Curriculum(idxCURR)%SubjectIdx(k)
                if (crse_required < 0) then
                    rank = Curriculum(idxCURR)%SubjectTerm(k)
                    call rank_to_year_term(rank, Year, Term)
                    write(unitXML, aformat) indentation(:indent1)// &
                        trim(txtYear(Year))//' Year, '// &
                        trim(txtSemester(Term))//' Term: '// &
                        Subject(crse_required)%Name
                end if
            end do
            write(unitXML,AFORMAT) '    </comment>'
        end if

        do idx=1,Student(std)%Reqd(0,0)

            if (Student(std)%Reqd(0,idx)==0) cycle

            call xml_write_character(unitXML, indent0, 'Substitution')
            if (Student(std)%Reqd(-1,idx)>0) then
                call rank_to_year_term(Student(std)%Reqd(-1,idx), Year, Term)
                call xml_write_character(unitXML, indent1, 'Year', txtYear(Year) )
                call xml_write_character(unitXML, indent1, 'Term', txtSemester(Term) )
            end if

            line = SPACE
            do k=1,Student(std)%Reqd(0,idx)
                line = COMMA//trim(Subject(Student(std)%Reqd(k,idx))%Name)//line
            end do
            call xml_write_character(unitXML, indent1, 'Required', line(2:))

            line = SPACE
            do k=1,Student(std)%Subst(0,idx)
                line = COMMA//trim(Subject(Student(std)%Subst(k,idx))%Name)//line
            end do
            call xml_write_character(unitXML, indent1, 'Replacement', line(2:))

            call xml_write_character(unitXML, indent0, '/Substitution')

        end do

        call xml_close_file(unitXML, XML_SUBSTITUTIONS)

    end subroutine xml_write_substitutions



    subroutine xml_read_student_grades(std, errNo)

        integer, intent (in) :: std
        integer, intent (out) :: errNo

        character(len=MAX_LEN_XML_LINE) :: value
        character(len=MAX_LEN_XML_TAG) :: tag
        type (TYPE_STUDENT) :: wrkStudent
        !character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_TEXT_GRADE) :: tGrade
        character (len=MAX_LEN_TEXT_SEMESTER) :: tTerm
        integer :: idx, grdType, gdx !cdx, idxCurr,
        !logical :: flag

        ! generate file name
        idx = year_prefix(Student(std))
        fileName = trim(dirTRANSCRIPTS)//trim(Student(std)%StdNo(1:idx))//DIRSEP//trim(Student(std)%StdNo)//'.XML'

        ! open file, return on any error
        call xml_open_file(unitXML, XML_STUDENT_RECORD, fileName, errNo, forReading)
        if (errNo/=0) return

        call html_comment('xml_read_student_grades('//trim(filename)//')')


        ! initialize using existing info
        wrkStudent%Record = 0

        ! examine the file line by line
        do
            read(unitXML, AFORMAT, iostat=eof) line
            if (eof<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, eof)
            if (eof/=0) exit

            select case (trim(tag))

                case ('StdNo')
                    ! do nothing; should not change

                case ('Name')
                    ! do nothing
                    !call upper_case(value)
                    !wrkStudent%Name = adjustl(value)

                case ('Gender')
                    ! do nothing
                    !wrkStudent%Gender = adjustl(value)

                case ('Curriculum')
                    ! do nothing
                    !tCurriculum = adjustl(value)
                    !idxCurr = index_to_curriculum(tCurriculum)
                    !if (idxCurr<0) then
                    !    idxCurr = -idxCurr
                    !else if (idxCurr==0) then
                    !    idxCurr =NumCurricula
                    !end if
                    !wrkStudent%CurriculumIdx = idxCurr

                case ('Country')
                    ! do nothing
                    !wrkStudent%CountryIdx = atoi(value)

                case ('Classification') ! ignore
                    ! do nothing
                    !wrkStudent%Classification = atoi(value)

                case default

                    call upper_case(tag) ! APE, FINALGRADE, REMOVAL, COMPLETION

                    do grdType = 0,3

                        if (trim(tag)/=trim(txtGradeType(grdType))) cycle

                        call index_to_delimiters(COMMA, value, ndels, pos)
                        idx = wrkStudent%Record(1,0)+1
                        wrkStudent%Record(1,idx) = grdType ! type

                        wrkStudent%Record(2,idx) = atoi(value(1:pos(2)-1)) ! year

                        tTerm = value(pos(2)+1:pos(3)-1)
                        call upper_case(tTerm)
                        wrkStudent%Record(3,idx) = index_to_term(tTerm) ! term

                        tSubject = value(pos(3)+1:pos(4)-1)
                        wrkStudent%Record(4,idx) = index_to_subject(tSubject) ! subject

                        tGrade = value(pos(4)+1:pos(5)-1)
                        gdx = index_to_grade(tGrade) ! grade
!                        if (gdx==gdxREGD) then ! registered
!                            if (.not. advisingPeriod) cycle ! exclude
!                        end if
                        wrkStudent%Record(5,idx) = gdx

                        wrkStudent%Record(1,0) = idx ! number of records

                        exit

                    end do

            end select

        end do
        call xml_close_file(unitXML)

        ! update student record
        Student(std)%Record = wrkStudent%Record

    end subroutine xml_read_student_grades


    subroutine xml_read_substitutions(std, errNo)

        integer, intent (in) :: std
        integer, intent (out) :: errNo

        character(len=MAX_LEN_XML_LINE) :: value
        character(len=MAX_LEN_XML_TAG) :: tag
        character (len=MAX_LEN_TEXT_SEMESTER) :: tTerm
        character (len=MAX_LEN_TEXT_YEAR) :: tYear
        integer :: idx, lenSubst

        ! generate file name
        idx = year_prefix(Student(std))
        fileName = trim(dirSUBSTITUTIONS)//trim(Student(std)%StdNo(1:idx))//DIRSEP//trim(Student(std)%StdNo)//'.XML'

        ! open file, return on any error
        call xml_open_file(unitXML, XML_SUBSTITUTIONS, fileName, errNo, forReading)
        if (errNo/=0) return

        call html_comment('xml_read_substitutions('//trim(filename)//')')

        lenSubst = Student(std)%Reqd(0,0) ! how many records so far
        ! examine the file line by line
        do
            read(unitXML, AFORMAT, iostat=eof) line
            if (eof<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, eof)
            if (eof/=0) exit

            !write(*,*) 'xml_read_substitution: '//trim(tag)//' -> '//trim(value)

            select case (trim(tag))

                case ('Substitution')
                    call check_array_bound (lenSubst+1, MAX_SUBJECTS_IN_CURRICULUM/3, &
                        'MAX_SUBJECTS_IN_CURRICULUM/3 @ '//fileName)

                case ('Year') ! value is one of FIRST, SECOND, THIRD, FOURTH, ...
                    tYear = adjustl(value)
                    Student(std)%Reqd(-1,lenSubst+1) = index_to_year(tYear)

                case ('Term') ! value is one of FIRST, SECOND, SUMMER
                    tTerm = adjustl(value)
                    idx = index_to_term(tTerm)
                    if (Student(std)%Reqd(-1,lenSubst+1)>0 .and. idx>0) then
                        Student(std)%Reqd(-1,lenSubst+1) = 3*(Student(std)%Reqd(-1,lenSubst+1)-1)+idx
                    end if

                case ('Required') ! value is comma-separated list of subjects
                    call tokenize_subjects(value, ',', 5, Student(std)%Reqd(0,lenSubst+1), Student(std)%Reqd(1:,lenSubst+1), eof)

                case ('Replacement') ! value is comma-separated list of subjects
                    call tokenize_subjects(value, ',', 5, Student(std)%Subst(0,lenSubst+1), Student(std)%Subst(1:,lenSubst+1), eof)

                case ('/Substitution')
                    lenSubst = lenSubst+1
                    Student(std)%Reqd(0,0) = lenSubst

                case default

            end select

        end do
        call xml_close_file(unitXML)

    end subroutine xml_read_substitutions


    subroutine read_student_records (std)
        integer, intent (in) :: std

#if defined UPLB
        logical :: DoNotRename = .true.
#endif
        integer :: ierr

        TCG = TYPE_STUDENT_RECORD (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, .false., SPACE, SPACE)
        lenTCG = 0

        ! retrieve record of substitutions
        if (Student(std)%Reqd(0,0)==0) then
            call xml_read_substitutions(std, ierr)
#if defined UPLB
            if (ierr/=0) then
                call custom_read_substitutions (std, DoNotRename)
                call xml_write_substitutions(std)
            end if
#endif
        end if

        ! retrieve record of grades
        if (Student(std)%Record(1,0)==0) then ! not yet retrieved
            call xml_read_student_grades(std, ierr)
#if defined UPLB
            if (ierr/=0) then
                call custom_read_student_grades (std, DoNotRename)
                call xml_write_student_grades(std)
            end if
#endif
        end if

    end subroutine read_student_records



    subroutine parse_student_records (std)
        integer, intent (in) :: std

        integer :: i, j, k, idx, tdx

        ! add substitutions to TCG
        do tdx=1,Student(std)%Reqd(0,0)
            ! PlanOfCoursework,Year,Term,Reqd(1),Reqd(2),...,Reqd(m),Subst(1),Subst(2),...,Subst(n)
            lenTCG = lenTCG+1
            line = SPACE
            do idx=Student(std)%Subst(0,tdx),1,-1
                line = COMMA//trim(Subject(Student(std)%Subst(idx,tdx))%Name)//line
            end do
            !write(*,*) 'Replacement='//trim(line(2:))
            do idx=Student(std)%Reqd(0,tdx),1,-1
                line = COMMA//trim(Subject(Student(std)%Reqd(idx,tdx))%Name)//line
            end do
            if (Student(std)%Reqd(-1,tdx)>0) then
                call rank_to_year_term(Student(std)%Reqd(-1,tdx), i, j)
                TCG(lenTCG)%Year = i
                TCG(lenTCG)%Term = j
                TCG(lenTCG)%txtLine = 'PlanOfCoursework,'//trim(txtYear(i))//COMMA//trim(txtSemester(j))//line
            else
                TCG(lenTCG)%txtLine = 'PlanOfCoursework,,'//line
            end if
            TCG(lenTCG)%Reqd(0:5) = Student(std)%Reqd(0:5,tdx)
            TCG(lenTCG)%Subst(0:5) = Student(std)%Subst(0:5,tdx)
            TCG(lenTCG)%Code = 1
            TCG(lenTCG)%Used = .false.
        end do

        ! add grades to TCG
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
            TCG(lenTCG)%Used = .false.
        end do

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

    end subroutine parse_student_records


    subroutine remake_student_records (std, DoNotRename)
        integer, intent (in) :: std
        logical, optional, intent (in) :: DoNotRename

        integer :: i, j, k, ierr
        real :: harvest

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
        end if

        ! generate random passing grades
        do i=1,Student(std)%Record(1,0)
            if (Student(std)%Record(4,i)<=0) cycle
            j = Student(std)%Record(5,i)
            k = j
            if (UniversityCode(1:3)=='CSU') then ! percentage grades
                if (j>=75) then ! passing
                    do while (k==j)
                        call random_number(harvest)
                        k = 76+int(harvest*24.0)
                    end do
                end if
            else ! assume 1.0, 1.25, 1.5, etc
                if (j>0 .and. j<10) then ! numeric pass
                    do while (k==j .or. k==0)
                        call random_number(harvest)
                        k = int(harvest*10.0)
                    end do
                end if
            end if
            Student(std)%Record(5,i) = k
        end do

        ! rewrite
        call xml_write_student_grades(std)

    end subroutine remake_student_records


end module CHECKLISTS
