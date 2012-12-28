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


subroutine custom_read_subjects(path, errNo)
    character(len=*), intent(in) :: path
    integer, intent(out) :: errNo

    integer :: ier

    call SIAS_read_subjects(path, errNo) ! try SIAS format
    if (errNo==0) then
        ! fees
        call SIAS_read_assessment(path, ier)
        ! PREREQUISITES.CSV
        call custom_read_prerequisites(path, ier)
        ! SUBJECTS-PREREQUISITES
        if (ier/=0) call custom_read_subjects_prerequisites(path, ier)
    end if
    return
end subroutine custom_read_subjects


subroutine custom_read_subjects_other(path, errNo)

    character(len=*), intent(in) :: path
    integer, intent (out) :: errNo

    ! fees
    call SIAS_read_assessment(path, errNo)
    ! prerequisites
    call custom_read_subjects_prerequisites(path, errNo)
    errNo = 0

    return
end subroutine custom_read_subjects_other


subroutine SIAS_read_subjects(path, errNo)

    character(len=*), intent(in) :: path
    integer, intent(out) :: errNo

    integer :: jdx, kdx, ldx
    character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
    character (len=MAX_LEN_XML_LINE) :: subline

    ! predefine special subjects (symbols in prerequisites)
    Subject      = TYPE_SUBJECT (SPACE,     SPACE                             , &
    1,0.0,0.0,0,0,0,0,0,0,0,0,0,0,0,  0,0,  0,0,  0,0,  0)
    Subject(-1)  = TYPE_SUBJECT ('(dummy)', '(dummy)'                         , &
    1,0.0,0.0,0,0,0,0,0,0,0,0,0,0,0,  0,0,  0,0,  0,0,  0)
    Subject(-2)  = TYPE_SUBJECT ('AND',     '(appears in prerequisites)'      , &
    1,0.0,7,0.0,0,0,0,0,0,0,0,0,0,1,-13,1,-13,1,-13,1,-13)
    Subject(-3)  = TYPE_SUBJECT ('APPROVAL','Approval by competent authority' , &
    1,0.0,7,0.0,0,0,0,0,0,0,0,0,0,1,-13,1,-13,1,-13,1,-13)
    Subject(-4)  = TYPE_SUBJECT ('FIFTH',   '(appears in prerequisites)'      , &
    1,0.0,7,0.0,0,0,0,0,0,0,0,0,0,1,-13,1,-13,1,-13,1,-13)
    Subject(-5)  = TYPE_SUBJECT ('FINAL',   '(appears in prerequisites)'      , &
    1,0.0,7,0.0,0,0,0,0,0,0,0,0,0,1,-13,1,-13,1,-13,1,-13)
    Subject(-6)  = TYPE_SUBJECT ('FOURTH',  '(appears in prerequisites)'      , &
    1,0.0,7,0.0,0,0,0,0,0,0,0,0,0,1,-13,1,-13,1,-13,1,-13)
    Subject(-7)  = TYPE_SUBJECT ('FRESHMAN','(appears in prerequisites)'      , &
    1,0.0,7,0.0,0,0,0,0,0,0,0,0,0,1,-13,1,-13,1,-13,1,-13)
    Subject(-8)  = TYPE_SUBJECT ('GRADUATE','(appears in prerequisites)'      , &
    1,0.0,7,0.0,0,0,0,0,0,0,0,0,0,1,-13,1,-13,1,-13,1,-13)
    Subject(-9)  = TYPE_SUBJECT ('JUNIOR',  '(appears in prerequisites)'      , &
    1,0.0,7,0.0,0,0,0,0,0,0,0,0,0,1,-13,1,-13,1,-13,1,-13)
    Subject(-10) = TYPE_SUBJECT ('LANGUAGE','(Language Elective)'             , &
    1,3.0,7,0.0,0,0,0,0,0,0,0,0,0,1,-13,1,-13,1,-13,1,-13)
    Subject(-11) = TYPE_SUBJECT ('MAJOR',   '(Must be in Plan Of Study)'      , &
    1,3.0,7,0.0,0,0,0,0,0,0,0,0,0,1,-13,1,-13,1,-13,1,-13)
    Subject(-12) = TYPE_SUBJECT ('MINOR',   '(Must be in Plan Of Study)'      , &
    1,3.0,7,0.0,0,0,0,0,0,0,0,0,0,1,-13,1,-13,1,-13,1,-13)
    Subject(-13) = TYPE_SUBJECT ('NONE',    '(appears in prerequisites)'      , &
    1,0.0,7,0.0,0,0,0,0,0,0,0,0,0,1,-13,1,-13,1,-13,1,-13)
    Subject(-14) = TYPE_SUBJECT ('OR',      '(appears in prerequisites)'      , &
    1,0.0,7,0.0,0,0,0,0,0,0,0,0,0,1,-13,1,-13,1,-13,1,-13)
    Subject(-15) = TYPE_SUBJECT ('SECOND',  '(appears in prerequisites)'      , &
    1,0.0,7,0.0,0,0,0,0,0,0,0,0,0,1,-13,1,-13,1,-13,1,-13)
    Subject(-16) = TYPE_SUBJECT ('SENIOR',  '(appears in prerequisites)'      , &
    1,0.0,7,0.0,0,0,0,0,0,0,0,0,0,1,-13,1,-13,1,-13,1,-13)
    Subject(-17) = TYPE_SUBJECT ('SIXTH',   '(appears in prerequisites)'      , &
    1,0.0,7,0.0,0,0,0,0,0,0,0,0,0,1,-13,1,-13,1,-13,1,-13)
    Subject(-18) = TYPE_SUBJECT ('SOPHOMORE','(appears in prerequisites)'     , &
    1,0.0,7,0.0,0,0,0,0,0,0,0,0,0,1,-13,1,-13,1,-13,1,-13)
    Subject(-19) = TYPE_SUBJECT ('THIRD',   '(appears in prerequisites)'      , &
    1,0.0,7,0.0,0,0,0,0,0,0,0,0,0,1,-13,1,-13,1,-13,1,-13)

    NumDummySubjects = -19
    INDEX_TO_NONE = -13 ! index to NONE (the prerequisite of "No prerequisite" subjects)

    fileName = trim(dirRAW)//trim(path)//'SUBJECTS.CSV'
    open (unit=unitNo, file=fileName, status='old', iostat=errNo)
    if (errNo/=0) return

    call file_log_message('Retrieving subjects from '//fileName)
    ! skip first line
    read(unitNo, AFORMAT) line
    ! read subject codes
    do
        read(unitNo, AFORMAT, iostat=eof) line
        if (eof<0) exit
        if (line==SPACE .or. line(1:1)=='#') cycle

        !----------------------------------------------------------------------------
        !!id,code,subjectno,name,units,loadunits,tfunits,lecunits,labunits,hours,orderno
        !!978,"ELECT 2 (BSAB)","ELECT 2","(ABM 81) International Trade",3.0000,3.0000,3.0000,3.0000,0.0000,3.0000,0.0000
        !! 1  2              3 4       5 6                            7
        call index_to_delimiters('"', line, ndels, pos)
        call check_array_bound (NumSubjects+NumAdditionalSubjects+1, MAX_ALL_SUBJECTS, 'MAX_ALL_SUBJECTS')

        ! already in Subject()?
        tSubject = line(pos(2)+1:pos(3)-1)
        call upper_case(tSubject)
        kdx = index_to_subject(tSubject)
        if (kdx/=0) cycle

        !write(*,*) 'Adding '//trim(line)
        NumSubjects = NumSubjects + 1
        Subject(NumSubjects)%Name = tSubject
        Subject(NumSubjects)%DeptIdx = NumDepartments ! refers to REGISTRAR
        !id,  code,            subjectno, name,                       units,  loadu, tfunits,lecunits,labunits,hours,orderno
        !978,"ELECT 2 (BSAB)","ELECT 2","(ABM 81) International Trade",3.0000,3.0000,3.0000,3.0000,0.0000,3.0000,0.0000
        ! 1  2              3 4       5 6                            7
        !                                                             1      2      3      4      5      6      7
        !id,code,subjectno,name,units,loadunits,tfunits,lecunits,labunits,hours,orderno
        !1275,"PE 12A","PE 12A"," To be arrange",2.0000,2.0000,2.0000,2.0000,0.0000,2.0000,0.0000
        !892,"PSYCH 59","PSYCH 59","Abnormal Psychology",3.0000,3.0000,3.0000,3.0000,0.0000,3.0000,0.0000
        !127,"MATH 68","MATH 68","Abstract Algebra",3.0000,3.0000,3.0000,3.0000,0.0000,3.0000,0.0000
        !1172,"MATH 213","MATH 213","Abstract Algebra",3.0000,3.0000,3.0000,3.0000,0.0000,3.0000,0.0000
        !1552,"MATH 79A","MATH 79A","Abstract Algebra 1",3.0000,3.0000,3.0000,3.0000,0.0000,3.0000,0.0000
        !846,"MATH 79B","MATH 79B","Abstract Algebra II",3.0000,3.0000,3.0000,3.0000,0.0000,3.0000,0.0000
        !1557,"MATH 79C","MATH 79C","Abstract Algebra iii",3.0000,3.0000,3.0000,3.0000,0.0000,3.0000,0.0000
        !1803,"ET 62A","ET 62A","AC Machine",4.0000,4.0000,4.0000,3.0000,1.0000,6.0000,0.0000
        !571,"IT 67A","IT 67A","Accounting Information Systems",3.0000,3.0000,3.0000,2.0000,1.0000,5.0000,0.0000
        !                                                      1      2      3      4      5      6      7

        Subject(NumSubjects)%Title = line(pos(6)+1:pos(7)-1)

        subline = line(pos(7)+2:)
        call index_to_delimiters(COMMA, subline, ndels, pos)

        !strUnits = subline(pos(1)+1:pos(2)-1)
        !read(strUnits, '(f4.1)') Subject(NumSubjects)%Units
        Subject(NumSubjects)%Units = atof(subline(pos(1)+1:pos(1)+3)) ! 2)-1))
        Subject(NumSubjects)%TermOffered = 7
        Subject(NumSubjects)%LectHours = atof(subline(pos(4)+1:pos(4)+3)) ! 5)-1))
        Subject(NumSubjects)%MinLectSize = 50
        Subject(NumSubjects)%MaxLectSize = 50
        Subject(NumSubjects)%LabHours = atof(subline(pos(6)+1:pos(6)+3)) - & !7)-1))
        Subject(NumSubjects)%LectHours
        Subject(NumSubjects)%MinLabSize = 50
        Subject(NumSubjects)%MaxLabSize = 50
        ! default workload
        if (Subject(NumSubjects)%LectHours>0.0 .and. Subject(NumSubjects)%LabHours>0.0) then
            Subject(NumSubjects)%LectLoad = 2.0*Subject(NumSubjects)%Units/3.0
            Subject(NumSubjects)%LabLoad = Subject(NumSubjects)%Units - Subject(NumSubjects)%LectLoad
        elseif (Subject(NumSubjects)%LectHours>0.0) then
            Subject(NumSubjects)%LectLoad = Subject(NumSubjects)%Units
        else
            Subject(NumSubjects)%LabLoad = Subject(NumSubjects)%Units
        end if

        Department(NumDepartments)%hasInfo = .true.


        !write(*,*) 'Found '//trim(tSubject), Subject(NumSubjects)%Units, Subject(NumSubjects)%LectHours, Subject(NumSubjects)%LabHours

        jdx = 1
        Subject(NumSubjects)%lenPreq = jdx
        Subject(NumSubjects)%Prerequisite(jdx) = INDEX_TO_NONE
        Subject(NumSubjects)%lenCoreq = jdx
        Subject(NumSubjects)%Corequisite = INDEX_TO_NONE
        Subject(NumSubjects)%lenConc = jdx
        Subject(NumSubjects)%Concurrent = INDEX_TO_NONE
        Subject(NumSubjects)%lenConcPreq = jdx
        Subject(NumSubjects)%ConcPrerequisite= INDEX_TO_NONE
    !----------------------------------------------------------------------------

    end do
    close(unitNo)

    ! sort
    do ldx=1,NumSubjects-1
        do kdx=ldx+1,NumSubjects
            if (Subject(ldx)%Name>Subject(kdx)%Name) then
                Subject(0) = Subject(ldx)
                Subject(ldx) = Subject(kdx)
                Subject(kdx) = Subject(0)
            endif
        end do
    end do
    Subject(0) = Subject(NumSubjects+1)

    return
end subroutine SIAS_read_subjects


subroutine SIAS_read_assessment(path, errNo)

    character(len=*), intent(in) :: path
    integer, intent(out) :: errNo

    character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
    integer :: cdx
    character :: tType
    real :: tFee

    fileName = trim(dirRAW)//trim(path)//'ASSESSMENT.CSV'
    open (unit=unitNo, file=fileName, status='old', iostat=errNo)
    if (errNo/=0) return

    call file_log_message('Retrieving lab fees from '//trim(fileName))
    ! skip first line
    read(unitNo, AFORMAT, iostat=eof) line
    do
        read(unitNo, AFORMAT, iostat=eof) line
        if (eof<0) exit
        if (len_trim(line)==0 .or. line(1:1)=='#') cycle

        call index_to_delimiters(COMMA, line, ndels, pos)

        tSubject = line(pos(18)+2:pos(19)-2)
        if (tSubject==SPACE) cycle
        cdx = index_to_subject(tSubject)
        if (cdx==0) then
            write(*,*) trim(line)//' : Not found - '//tSubject
            cycle
        end if
        tType =  line(pos(21)+2:pos(21)+2)
        tFee =  atof(line(pos(27)+1:pos(28)-1))
        !read(tSubject,'(f7.3)') tFee
        !write(*,*) Subject(cdx)%Name, tType, tFee
        select case (tType)
            case ('1', '2')
                Subject(cdx)%Tuition = tFee
            case ('3')
                Subject(cdx)%LabFee = tFee
            case ('4')
                Subject(cdx)%Tuition = Subject(cdx)%Units*tFee
        end select
    end do
    close(unitNo)

    return
end subroutine SIAS_read_assessment


subroutine custom_read_subjects_prerequisites(path, errNo)

    character(len=*), intent(in) :: path
    integer, intent(out) :: errNo

    character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
    integer :: cdx

    fileName = trim(dirRAW)//trim(path)//'SUBJECTS-PREREQUISITES'
    open (unit=unitNo, file=fileName, status='old', iostat=errNo)
    if (errNo/=0) return

    call file_log_message('Retrieving prerequisites from '//trim(fileName))
    ! read subject codes
    do

        read(unitNo, AFORMAT, iostat=eof) line
        if (eof<0) exit
        if (len_trim(line)==0 .or. line(1:1)=='#') cycle

        call index_to_delimiters(COMMA, line, ndels, pos)

        tSubject = line(:pos(2)-1)
        cdx = index_to_subject(tSubject)
        if (cdx==0) then
            write(*,*) trim(line)//' : Not found - '//tSubject
            cycle
        end if

        ! tokenize prerequisite
        line = line(pos(2)+1:) ! get prerequisite expression
        call tokenize_subjects(line, '+', MAX_ALL_SUBJECT_PREREQ, &
        Subject(cdx)%lenPreq, Subject(cdx)%Prerequisite, eof)
        if (eof>0) write(*,*) trim(line)

    end do
    close(unitNo)

    return
end subroutine custom_read_subjects_prerequisites


subroutine custom_read_prerequisites(path, errNo)

    character(len=*), intent(in) :: path
    integer, intent(out) :: errNo

    character (len=MAX_LEN_SUBJECT_CODE) :: tSubject, preq1, preq2, preq3, coreq1, coreq2
    integer :: cdx, pdx1, pdx2, pdx3, rdx1, rdx2

    fileName = trim(dirRAW)//trim(path)//'PREREQUISITES.CSV'
    open (unit=unitNo, file=fileName, status='old', iostat=errNo)
    if (errNo/=0) return

    call file_log_message('Retrieving prerequisites from '//trim(fileName))
    ! Subject Code,Prereq1,Prereq2,Prereq3,Coreq1,Coreq2
    do

        read(unitNo, AFORMAT, iostat=eof) line
        if (eof<0) exit
        if (len_trim(line)==0 .or. line(1:1)=='#') cycle
        !            Subject Code,Prereq1,Prereq2,Prereq3,Coreq1,Coreq2
        !            AA 11 ,DRAFT 55              ,,,,
        !            AA 12 ,,,,,
        !            AB MGT 11 ,,,,,
        !            ACCREV                ,,,,,
        !            ACCTG 55 ,,,,,
        !            ACCTG 55A             ,,,,,
        !            ACCTG 55B ,MATH 13               ,,,,
        !            ACCTG 56 ,ACCTG 55 ,,,,
        !            ACCTG 57 ,ACCTG 56 ,,,,
        !            ACCTG 58 ,ACCTG 57 ,,,,
        !            ACCTG 59 ,ACCTG 55 ,ACCTG 56 ,,,

        call index_to_delimiters(COMMA, line, ndels, pos)

        tSubject = line(:pos(2)-1)
        cdx = index_to_subject(tSubject)
        if (cdx==0) then
            call file_log_message(trim(line)//' : Not found - '//tSubject)
            cycle
        end if

        ! prerequisite
        pdx1 = 0
        pdx2 = 0
        pdx3 = 0
        preq1 = line(pos(2)+1:pos(3)-1)
        preq2 = line(pos(3)+1:pos(4)-1)
        preq3 = line(pos(4)+1:pos(5)-1)
        if (preq1/=SPACE) pdx1 = index_to_subject(preq1)
        if (preq2/=SPACE) pdx2 = index_to_subject(preq2)
        if (preq3/=SPACE) pdx3 = index_to_subject(preq3)
        line = SPACE
        if (pdx1>0) line = preq1
        if (pdx2>0) then
            if (len_trim(line)>0) then
                line = 'AND+'//trim(preq2)//'+'//line
            else
                line = preq2
            end if
        end if
        if (pdx3>0) then
            if (len_trim(line)>0) then
                line = 'AND+'//trim(preq3)//'+'//line
            else
                line = preq3
            end if
        end if
        if (len_trim(line)>0) then ! tokenize prerequisite
            call tokenize_subjects(line, '+', MAX_ALL_SUBJECT_PREREQ, &
            Subject(cdx)%lenPreq, Subject(cdx)%Prerequisite, eof)
            !call file_log_message(trim(tSubject)//' has prereq : '//trim(line))
        end if

        ! coprequisite
        rdx1 = 0
        rdx2 = 0
        coreq1 = line(pos(5)+1:pos(6)-1)
        coreq2 = line(pos(6)+1:pos(7)-1)
        if (coreq1/=SPACE) rdx1 = index_to_subject(coreq1)
        if (coreq2/=SPACE) rdx2 = index_to_subject(coreq2)
        line = SPACE
        if (rdx1>0) line = coreq1
        if (rdx2>0) then
            if (len_trim(line)>0) then
                line = 'AND+'//trim(coreq2)//'+'//line
            else
                line = coreq2
            end if
        end if
        if (len_trim(line)>0) then ! tokenize corequisite
            call tokenize_subjects(line, '+', MAX_ALL_SUBJECT_COREQ, &
            Subject(cdx)%lenCoreq, Subject(cdx)%Corequisite, eof)
            !call file_log_message(trim(tSubject)//' has coreq : '//trim(line))
        end if

    end do
    close(unitNo)

    return
end subroutine custom_read_prerequisites

