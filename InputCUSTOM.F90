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


module InputCUSTOM

    use UNIVERSITY

    implicit none

    ! private tokens
    character (len=MAX_LEN_FILE_PATH), private :: fileName, studentCopy
    character (len=MAX_LEN_XML_LINE), private :: line, value
    character (len=MAX_LEN_XML_TAG), private :: tag
    integer, private :: eof, ndels, pos(30)
    logical, private :: pathExists


contains


    subroutine custom_read_colleges(path, errNo)
        !id,code,name,sched,number,orperiod,orexam
        !6,"CA","College of Agriculture","C","","11-1"," "
        !5,"CAS","College of Arts and Sciences","A","","11-1"," "
        !1 2   3 4                            5

        character(len=*), intent(in) :: path
        integer, intent(out) :: errNo


        fileName = trim(path)//'COLLEGES.CSV'
        open (unit=unitRAW, file=fileName, status='old', iostat=errNo)
        if (errNo/=0) return

        call log_comment('Retrieving college codes from '//fileName)
        ! skip first line
        read(unitRAW, AFORMAT) line

        do
            read(unitRAW, AFORMAT, iostat=eof) line

            if (eof<0) exit
            if (line==SPACE .or. line(1:1)=='#') cycle

            call index_to_delimiters('"', line, ndels, pos)

            if (index(line,ADMINISTRATION)>0) cycle ! add later

            NumColleges = NumColleges + 1
            call check_array_bound (NumColleges, MAX_ALL_COLLEGES, 'MAX_ALL_COLLEGES')
            call blank_to_underscore(line(pos(2)+1:pos(3)-1), College(NumColleges)%Code)
            College(NumColleges)%Name = line(pos(4)+1:pos(5)-1)

        end do

        close(unitRAW)
        call log_comment (itoa(NumColleges)//' colleges after reading '//fileName)

        ! add 'administrative' college for data that does not fit in the 'academic' colleges
        NumColleges = NumColleges + 1
        call check_array_bound (NumColleges, MAX_ALL_COLLEGES, 'MAX_ALL_COLLEGES')
        call initialize_college (College(NumColleges), &
            ADMINISTRATION, UniversityCode//' Administration', ADMINISTRATION)

    end subroutine custom_read_colleges



    subroutine custom_read_departments (path, errNo)

        character(len=*), intent(in) :: path
        integer, intent (out) :: errNo

        integer :: ldx
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        fileName = trim(path)//'DEPARTMENTS.CSV'
        open (unit=unitRAW, file=fileName, status='old', iostat=errNo)
        if (errNo/=0) return

        call log_comment('Retrieving department info from '//fileName)

        ! skip first line
        read(unitRAW, AFORMAT) line

        do

            read(unitRAW, AFORMAT, iostat=eof) line
            if (eof<0) exit
            if (line==SPACE .or. line(1:1)=='#') cycle

            !id,code,name,sched,number,orperiod,orexam
            !6,"CA","College of Agriculture","C","","11-1"," "
            !5,"CAS","College of Arts and Sciences","A","","11-1"," "
            !1 2   3 4                            5
            call index_to_delimiters('"', line, ndels, pos)

            call blank_to_underscore(line(pos(2)+1:pos(3)-1), tDepartment)

            if (index(tDepartment,trim(ADMINISTRATION))>0) cycle ! add at the end

            tCollege = tDepartment
            ldx = index_to_college(tCollege)
            NumDepartments = NumDepartments + 1
            call check_array_bound (NumDepartments, MAX_ALL_DEPARTMENTS, 'MAX_ALL_DEPARTMENTS')
            Department(NumDepartments)%Code = tDepartment
            Department(NumDepartments)%Name = line(pos(4)+1:pos(5)-1)
            Department(NumDepartments)%SectionPrefix = line(pos(6)+1:pos(7)-1)
            if (Department(NumDepartments)%SectionPrefix==SPACE) Department(NumDepartments)%SectionPrefix = '#'
            Department(NumDepartments)%CollegeIdx = ldx

        end do

        close(unitRAW)
        call log_comment (itoa(NumDepartments)//' departments after reading '//fileName)

        ! add REGISTAR as 'administrative' department for data that does not fit in the 'academic' departments
        NumDepartments = NumDepartments + 1
        call check_array_bound (NumDepartments, MAX_ALL_DEPARTMENTS, 'MAX_ALL_DEPARTMENTS')
        call initialize_department (Department(NumDepartments), &
            ADMINISTRATION, UniversityCode//' Registrar', 'Z', NumColleges)

    end subroutine custom_read_departments




    subroutine custom_read_rooms(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent(out) :: errNo

        character (len=MAX_LEN_ROOM_CODE) :: tCode
        integer :: i, j, k, locx, seats
        character (len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        fileName = trim(path)//'ROOMS.CSV'
        open (unit=unitRAW, file=fileName, status='old', iostat=errNo)
        if (errNo/=0) return
        call log_comment('Retrieving room codes from '//fileName)

        ! skip first line
        read(unitRAW, AFORMAT) line

        do
            read(unitRAW, AFORMAT, iostat=eof) line
            if (eof<0) exit
            if (line==SPACE .or. line(1:1)=='#') cycle

            call index_to_delimiters('"', line, ndels, pos)
            !id,code,name,type
            !8,"A101","A101","[CED]"
            !9,"A102","A102","[CED]"
            !10,"A103","A103","[CED]"
            !1  2    3 4    5 6     7

            tCode = line(pos(2)+1:pos(3)-1)
            call upper_case(tCode)
            j = index_to_room(tCode)
            if (j>0) cycle ! already encountered

            call blank_to_underscore(line(pos(6)+2:pos(7)-2), tDepartment)
            !tDepartment = line(pos(6)+2:pos(7)-2)
            k = index_to_dept(tDepartment)
            if (k==0) k = NumDepartments ! refers to ADMINISTRATION
            Department(k)%hasInfo = .true.

            locx = 0 ! all rooms are close to each other?

            seats = 50 ! all rooms have the same capacity?

            call check_array_bound (NumRooms+NumAdditionalRooms+1, MAX_ALL_ROOMS, 'MAX_ALL_ROOMS')
            do i=NumRooms,1,-1
                if (tCode<Room(i)%Code) then
                    Room(i+1) = Room(i)
                else
                    j = i
                    exit
                end if
            end do
            call initialize_room (Room(j+1), tCode, k, locx, seats)
            NumRooms = NumRooms + 1

        end do
        call log_comment (itoa(NumRooms)//' rooms after reading '//fileName)

    end subroutine custom_read_rooms



    subroutine custom_read_teachers(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent(out) :: errNo

        character (len=MAX_LEN_PERSON_NAME) :: tTeacher
        character (len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character (len=MAX_LEN_TEACHER_CODE)   :: teacherId
        integer :: i, j, k
        character (len=1) :: ch
        type(TYPE_TEACHER) :: wrkTeacher

        fileName = trim(path)//'TEACHERS.CSV'
        open (unit=unitRAW, file=fileName, status='old', iostat=errNo)
        if (errNo/=0) return

        call log_comment('Retrieving teachers from '//fileName)
        ! skip first line
        read(unitRAW, AFORMAT) line

        do

            read(unitRAW, AFORMAT, iostat=eof) line
            if (eof<0) exit
            if (line==SPACE .or. line(1:1)=='#') cycle

            call index_to_delimiters('"', line, ndels, pos)
            !id,code,name,deptcd,speccd
            !429,"ABAG","Abaleta, Gladys","",""
            !145,"ABAJ","Abaleta, Jay A.","",""
            !295,"ABED","Abella, Dante","CAHS",""
            !198,"ABEO","Abella, Orlando","",""
            !331,"ABEW","Abena, Winston","CAHS","SPECIALIZATION"
            !1   2    3 4              5 6    7

            call initialize_teacher(wrkTeacher)

            ! teacher name
            tTeacher = SPACE
            j = 0
            do i=pos(4)+1,pos(5)-1
                ch = line(i:i)
                if (index(SPECIAL,ch)>0) cycle
                j = j+1
                tTeacher(j:j) = ch
            end do
            call upper_case(tTeacher)

            ! teacher ID
            teacherId = SPACE
            j = 0
            do i=pos(2)+1,pos(3)-1
                ch = line(i:i)
                if (index(SPECIAL,ch)>0 .or. ch==COMMA .or. ch==SPACE .or. ch==DASH) cycle
                j = j+1
                teacherId(j:j) = ch
            end do
            call upper_case(teacherId)

            ! department
            call blank_to_underscore(line(pos(6)+1:pos(7)-1), tDepartment)
            k = index_to_dept(tDepartment)
            if (k==0) k = NumDepartments ! refers to ADMINISTRATION

            j = index_to_teacher(teacherId)
            if (j>0) then ! already encountered; check if the same name
                if (tTeacher/=Teacher(j)%Name) then ! update only
                    Teacher(j)%Name =  tTeacher
                    wrkTeacher%DeptIdx = k
                end if
                cycle
            end if

            wrkTeacher%TeacherId = teacherId
            wrkTeacher%Name = tTeacher
            wrkTeacher%DeptIdx = k
            wrkTeacher%MaxLoad = 21
            Department(k)%hasInfo = .true.

            call check_array_bound (NumTeachers+NumAdditionalTeachers+1, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')

            do i=NumTeachers,1,-1
                if (tTeacher<Teacher(i)%TeacherId) then
                    Teacher(i+1) = Teacher(i)
                else
                    j = i
                    exit
                end if
            end do

            Teacher(j+1) = wrkTeacher
            NumTeachers = NumTeachers + 1

        end do

        close(unitRAW)
        call log_comment (itoa(NumTeachers)//' teachers after reading '//fileName)

    end subroutine custom_read_teachers

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

    end subroutine custom_read_subjects


    subroutine custom_read_subjects_other(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent (out) :: errNo

        ! fees
        call SIAS_read_assessment(path, errNo)
        ! prerequisites
        call custom_read_subjects_prerequisites(path, errNo)
        errNo = 0

    end subroutine custom_read_subjects_other


    subroutine SIAS_read_subjects(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent(out) :: errNo

        integer :: jdx, kdx, ldx
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_XML_LINE) :: subline

        NumDummySubjects = -19
        INDEX_TO_NONE = -13
        Subject(NumDummySubjects:-1) = TYPE_SUBJECT ( &
            SPACE, & ! character (len=MAX_LEN_SUBJECT_CODE) :: Name
            SPACE, & ! character (len=MAX_LEN_SUBJECT_TITLE) :: Title
            1, & ! integer :: DeptIdx
            0.0, & ! real :: Units
            0.0, & ! real :: Failrate(1:3)
            0, & ! integer :: GrandTotal(2,1:3)
            7, & ! integer :: TermOffered
            0.0, 0.0, & ! real :: Tuition, LabFee
            0.0, 0.0, & ! real :: LectHours, LectLoad
            0, 0, & ! integer :: MinLectSize, MaxLectSize
            0.0, 0.0, & ! real :: LabHours, LabLoad
            0, 0, & ! integer :: MaxLabSize, MinLabSize, &
            1, INDEX_TO_NONE, & !     lenPreq, Prerequisite(MAX_ALL_SUBJECT_PREREQ), &
            1, INDEX_TO_NONE, & !     lenCoreq, Corequisite(MAX_ALL_SUBJECT_COREQ), &
            1, INDEX_TO_NONE, & !     lenConc, Concurrent(MAX_ALL_SUBJECT_CONCURRENT), &
            1, INDEX_TO_NONE) !     lenConcPreq, ConcPrerequisite(MAX_ALL_SUBJECT_CONCPREQ)

        Subject(-1)%Name = '(dummy)'
        Subject(-1)%Title = '(dummy)'

        Subject(-2)%Name = 'AND'
        Subject(-2)%Title = '(appears in prerequisites)'

        Subject(-3)%Name = 'APPROVAL'
        Subject(-3)%Title = 'Approval by competent authority'

        Subject(-4)%Name = 'FIFTH'
        Subject(-4)%Title = '(appears in prerequisites)'

        Subject(-5)%Name = 'FINAL'
        Subject(-5)%Title = '(appears in prerequisites)'

        Subject(-6)%Name = 'FOURTH'
        Subject(-6)%Title = '(appears in prerequisites)'

        Subject(-7)%Name = 'FRESHMAN'
        Subject(-7)%Title = '(appears in prerequisites)'

        Subject(-8)%Name = 'GRADUATE'
        Subject(-8)%Title = '(appears in prerequisites)'

        Subject(-9)%Name = 'JUNIOR'
        Subject(-9)%Title = '(appears in prerequisites)'

        Subject(-10)%Name = 'LANGUAGE'
        Subject(-10)%Title = '(Language Elective)'

        Subject(-11)%Name = 'MAJOR'
        Subject(-11)%Title = '(Must be in Plan Of Study)'

        Subject(-12)%Name = 'MINOR'
        Subject(-12)%Title = '(Must be in Plan Of Study)'

        Subject(-13)%Name = 'NONE'
        Subject(-13)%Title = '(appears in prerequisites)'

        Subject(-14)%Name = 'OR'
        Subject(-14)%Title = '(appears in prerequisites)'

        Subject(-15)%Name = 'SECOND'
        Subject(-15)%Title = '(appears in prerequisites)'

        Subject(-16)%Name = 'SENIOR'
        Subject(-16)%Title = '(appears in prerequisites)'

        Subject(-17)%Name = 'SIXTH'
        Subject(-17)%Title = '(appears in prerequisites)'

        Subject(-18)%Name = 'SOPHOMORE'
        Subject(-18)%Title = '(appears in prerequisites)'

        Subject(-19)%Name = 'THIRD'
        Subject(-19)%Title = '(appears in prerequisites)'

        ! the rest of the subjects
        call initialize_subject (Subject(0))
        Subject(2:) = Subject(0)

        fileName = trim(path)//'SUBJECTS.CSV'
        open (unit=unitRAW, file=fileName, status='old', iostat=errNo)
        if (errNo/=0) return

        call log_comment('Retrieving subjects from '//fileName)
        ! skip first line
        read(unitRAW, AFORMAT) line
        ! read subject codes
        do
            read(unitRAW, AFORMAT, iostat=eof) line
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
            Subject(NumSubjects)%DeptIdx = NumDepartments ! refers to ADMINISTRATION
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
        close(unitRAW)

        call log_comment (itoa(NumSubjects)//' subjects after reading '//fileName)

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

    end subroutine SIAS_read_subjects


    subroutine SIAS_read_assessment(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent(out) :: errNo

        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        integer :: cdx
        character :: tType
        real :: tFee

        fileName = trim(path)//'ASSESSMENT.CSV'
        open (unit=unitRAW, file=fileName, status='old', iostat=errNo)
        if (errNo/=0) return

        call log_comment('Retrieving lab fees from '//trim(fileName))
        ! skip first line
        read(unitRAW, AFORMAT, iostat=eof) line
        do
            read(unitRAW, AFORMAT, iostat=eof) line
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
        close(unitRAW)

        return
    end subroutine SIAS_read_assessment


    subroutine custom_read_subjects_prerequisites(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent(out) :: errNo

        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        integer :: cdx

        fileName = trim(path)//'SUBJECTS-PREREQUISITES'
        open (unit=unitRAW, file=fileName, status='old', iostat=errNo)
        if (errNo/=0) return

        call log_comment('Retrieving prerequisites from '//trim(fileName))
        ! read subject codes
        do

            read(unitRAW, AFORMAT, iostat=eof) line
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
        close(unitRAW)

        return
    end subroutine custom_read_subjects_prerequisites


    subroutine custom_read_prerequisites(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent(out) :: errNo

        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject, preq1, preq2, preq3, coreq1, coreq2
        integer :: cdx, pdx1, pdx2, pdx3, rdx1, rdx2

        fileName = trim(path)//'PREREQUISITES.CSV'
        open (unit=unitRAW, file=fileName, status='old', iostat=errNo)
        if (errNo/=0) return

        call log_comment('Retrieving prerequisites from '//trim(fileName))
        ! Subject Code,Prereq1,Prereq2,Prereq3,Coreq1,Coreq2
        do

            read(unitRAW, AFORMAT, iostat=eof) line
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
                call log_comment(trim(line)//' : Not found - '//tSubject)
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
                !call log_comment(trim(tSubject)//' has prereq : '//trim(line))
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
                !call log_comment(trim(tSubject)//' has coreq : '//trim(line))
            end if

        end do
        close(unitRAW)

        return
    end subroutine custom_read_prerequisites


    subroutine custom_read_curricula(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent(out) :: errNo

        logical :: FlagIsUp
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=MAX_LEN_TEXT_SEMESTER) :: strTerm
        character (len=MAX_LEN_TEXT_YEAR) :: strYear
        integer :: idxCURR, idxterm, idxCOLL
        integer :: i, j, year, term
        character (len=MAX_LEN_SUBJECT_CODE) :: token
        character (len=MAX_LEN_FILE_PATH) :: currFile
        character (len=1) :: ch

        fileName = trim(path)//'CURRICULA.CSV'
        open (unit=unitRAW, file=fileName, status='old', iostat=errNo)
        if (errNo/=0) return

        call log_comment('Retrieving curricular programs from '//fileName)
        ! skip first line
        read(unitRAW, AFORMAT) line
        do
            read(unitRAW, AFORMAT, iostat=eof) line
            if (eof<0) exit
            if (line(1:1)=='#' .or. line=='   ') cycle

            !----------------------------------------------------------------------------
            !id,code,name,deptcode,fileName
            !43,"BAPA","Bachelor of Arts in Public Administration","CBAPA","BAPA.CSV"
            !1  2    3 4                                         5 6     7 8        9
            call index_to_delimiters('"', line, ndels, pos)

            ! remove non-alphabetic, non-DASH
            tCurriculum = SPACE
            j = 0
            do i=pos(2)+1,pos(3)-1
                ch = line(i:i)
                if ((ch>='A' .and. ch<='Z') .or. index(DECDIGITS,ch)>0 .or. ch==DASH) then
                    j = j+1
                    tCurriculum(j:j) = ch
                end if
            end do
            idxCURR = index_to_curriculum(tCurriculum)
            if (idxCURR>0) then
                call log_comment (line, trim(tCurriculum)//' - curriculum already listed')
                cycle
            end if

            currFile = trim(path)//line(pos(2)+1:pos(3)-1)//'.CSV'
            inquire(file=currFile, exist=FlagIsUp)
            if (.not. FlagIsUp) then
                !write(*,*) 'File not found: '//trim(currFile)
                if (ndels>=8 .and. pos(9)-pos(8)>1) then
                    currFile =trim(path)//line(pos(8)+1:pos(9)-1)
                    inquire(file=currFile, exist=FlagIsUp)
                    !if (.not. FlagIsUp) then
                    !    write(*,*) 'File not found: '//trim(currFile)
                    !    cycle
                    !end if
                end if
            end if

            call blank_to_underscore(line(pos(6)+1:pos(7)-1), tCollege)
            !tCollege = line(pos(6)+1:pos(7)-1)
            idxCOLL = 0
            do j=0,NumColleges
                if (tcollege==College(j)%Code) idxCOLL = j
            end do
            if (idxCOLL==0) then
                call log_comment (line, trim(tcollege)//' - college not known; using ADMIN')
                idxCOLL = NumColleges
            end if
            College(idxCOLL)%hasInfo = .true.
            call check_array_bound (NumCurricula+1, MAX_ALL_CURRICULA, 'MAX_ALL_CURRICULA')
            NumCurricula = NumCurricula + 1
            Curriculum(NumCurricula)%Code = tCurriculum
            Curriculum(NumCurricula)%CollegeIdx = idxCOLL
            Curriculum(NumCurricula)%Title = line(pos(4)+1:pos(5)-1)
            Curriculum(NumCurricula)%Specialization = SPACE
            Curriculum(NumCurricula)%Remark = SPACE
            Curriculum(NumCurricula)%NSubjects = 0

            if (.not. FlagIsUp) then
                call log_comment (line, 'Curriculum file not found: '//trim(currFile))
                token = 'NONE'
                Curriculum(NumCurricula)%NumTerms = 1
                Curriculum(NumCurricula)%NSubjects = 1
                Curriculum(NumCurricula)%SubjectIdx(1) = index_to_subject (token)
                Curriculum(NumCurricula)%SubjectTerm(1) = 1
                cycle
            end if

            ! read subjects
            call log_comment ('Retrieving '//trim(currFile)//' for '//tCurriculum)
            open (unit=2, file=currFile, status='old')
            ! skip first line
            read(2, AFORMAT) line
            do
                read(2, AFORMAT, iostat=eof) line
                if (eof<0) exit
                if (line==SPACE .or. line(1:1)=='#') cycle
                call index_to_delimiters('"', line, ndels, pos)
                !no,code,name,units,year,term,optional,subject,curr,id
                !0,"FIN 30B","Basic Finance",3.0000,1,"1",0,1226,19,1120
                !0,"PE 11","Physical Fitness",2.0000,1,"1",0,639,19,1123
                !0,"ACCTG 30D","Fundamentals of Accounting, Part 1",3.0000,1,"1",0,1199,19,1121
                !  2         3 4                                  5          6 7
                strTerm = line(pos(6)+1:pos(7)-1)
                term = atoi(strTerm)
                if (term==6) then
                    term = 3
                    strTerm = 'S'
                end if

                strYear = line(pos(6)-2:pos(6)-2)
                year = atoi(strYear)
                if (year>0 .and. term>0) then ! ok

                    ! collect subjects
                    idxTerm = (year-1)*3 + term
                    Curriculum(NumCurricula)%NumTerms = max(idxTerm, Curriculum(NumCurricula)%NumTerms)

                    token = line(pos(2)+1:pos(3)-1)
                    ! convert to uppercase
                    call upper_case(token)
                    i = index_to_subject (token)
                    if (i/=0) then
                        j = Curriculum(NumCurricula)%NSubjects+1
                        Curriculum(NumCurricula)%NSubjects = j
                        Curriculum(NumCurricula)%SubjectIdx(j) = i
                        Curriculum(NumCurricula)%SubjectTerm(j) = idxTerm
                        !if (is_offered(i,mod(term,3)) ) then
                        if ( is_offered(i,term) ) then
                            if (.not. is_prerequisite_satisfiable_in_curriculum(i,NumCurricula)) then
                                ! errNo = 126 ! subject prerequisite cannot be satisfied in this curriculum
                                call log_comment (trim(Curriculum(NumCurricula)%Code)//', '// &
                                trim(strYear)//' year, '//trim(strTerm)//' term, '//trim(token)// &
                                ': preq '//trim(text_prerequisite_in_curriculum(i))//' not specified earlier!')
                            end if
                        else
                            ! subject not offered in semester taken
                            call log_comment (token//line, 'Subject not offered during '//strTerm//' Term')
                        end if
                    else
                        ! subject not in catalog
                        call log_comment (line, token//' Subject not in catalog')
                    end if
                end if

            end do
            close(2)
        !----------------------------------------------------------------------------

        end do
        close(unitRAW)
        call log_comment (itoa(NumCurricula)//' curricula after reading '//fileName)


    end subroutine custom_read_curricula


    subroutine custom_read_basic_data()
        integer :: i, j, errNo

        ! read the colleges
        call custom_read_colleges(pathToYear, errNo)
        if (errNo/=0) call terminate('Error in reading the list of colleges')

        ! read the departments
        call custom_read_departments(pathToYear, errNo)
        if (errNo/=0) call terminate('Error in reading the list of departments')

        ! read the subjects
        call custom_read_subjects(pathToYear, errNo)
        if (errNo/=0) call terminate('Error in reading the list of subjects')

        call get_subject_areas()

        ! read the curricular programs
        call custom_read_curricula(pathToYear, errNo)
        if (errNo/=0) call terminate('Error in reading the list of curricular programs')

        ! Synchronize pre-requisites of co-requisite subjects
        ! For example, CHEM 17.0 has MATH 11 or MATH 17, CHEM 17.1 has NONE. Set
        ! pre-requisite of CHEM 17.1 to that of CHEM 17
        call log_comment('Synchronizing pre-requisites of co-requisite subjects...')
        do j=1,NumSubjects
            if (Subject(Subject(j)%Prerequisite(1))%Name/='NONE') cycle ! pre-requisite is NONE
            if (Subject(j)%lenCoreq/=1) cycle ! should be one token only
            i = Subject(j)%Corequisite(1)
            if (i<=0) cycle ! token should be a named subject
            call log_comment(Subject(j)%Name//'has co-requisite '//Subject(i)%Name)
            ! pre-requisite is NONE, co-requisite is a named subject
            Subject(j)%lenPreq = Subject(i)%lenPreq
            Subject(j)%Prerequisite = Subject(i)%Prerequisite
        end do

        ! read the rooms
        call custom_read_rooms(pathToYear, errNo)
        if (NumRooms==0) then
            do i=2,NumDepartments-1 ! create rooms for each department
                NumRooms = NumRooms+1
                call initialize_room(Room(NumRooms), trim(Department(i)%Code)//'-Room', &
                    i, 0, 0)
            end do
            errNo = 0
        end if

            ! read the teachers
        call custom_read_teachers(pathToYear, errNo)
        if (errNo/=0 .or. NumTeachers==1) then ! 1=Guest only

            do i=2,NumDepartments-1 ! create teacher for each department
                NumTeachers = NumTeachers+1
                call check_array_bound (NumTeachers, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')
                Teacher(NumTeachers)%TeacherID = trim(Department(i)%Code)//'-Teacher'
                Teacher(NumTeachers)%DeptIdx = i
                Teacher(NumTeachers)%Role = GUEST
                Teacher(NumTeachers)%Name = trim(Department(i)%Code)//' Teacher'
                Teacher(NumTeachers)%MaxLoad = 0
                Teacher(NumTeachers)%Specialization = 'Teaching'
                call set_password(Teacher(NumTeachers)%Password)

            end do
        end if

        ! the Developer account
        NumTeachers = NumTeachers+1
        call check_array_bound (NumTeachers, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')
        Teacher(NumTeachers)%TeacherID = PROGNAME
        Teacher(NumTeachers)%DeptIdx = NumDepartments
        Teacher(NumTeachers)%Role = ADMINISTRATION
        Teacher(NumTeachers)%Name = PROGNAME//' Developer'
        Teacher(NumTeachers)%MaxLoad = 0
        Teacher(NumTeachers)%Specialization = 'HEEDS Development'
        call set_password(Teacher(NumTeachers)%Password)

        ! the Administrator
        NumTeachers = NumTeachers+1
        call check_array_bound (NumTeachers, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')
        Teacher(NumTeachers)%TeacherID = ADMINISTRATION
        Teacher(NumTeachers)%DeptIdx = NumDepartments
        Teacher(NumTeachers)%Role = ADMINISTRATION
        Teacher(NumTeachers)%Name = PROGNAME//' Administrator'
        Teacher(NumTeachers)%MaxLoad = 0
        Teacher(NumTeachers)%Specialization = 'HEEDS Administration'
        call set_password(Teacher(NumTeachers)%Password)

    end subroutine custom_read_basic_data


end module InputCUSTOM
