
    subroutine import_custom_csv()

        integer :: jTmp, iTmp, errNo

        termBegin = currentTerm
        termEnd = termBegin

        ! read the colleges
        call custom_read_colleges(pathToYear, errNo)
        if (errNo/=0) call terminate('Error in reading the list of colleges')

        ! log directory for users in the college
        do iTmp=1,NumColleges
            call make_directory( trim(dirLOG)//trim(College(iTmp)%Code)//DIRSEP )
        end do

        ! read the departments
        call custom_read_departments(pathToYear, errNo)
        if (errNo/=0) call terminate('Error in reading the list of departments')

        ! read the rooms
        call custom_read_rooms(pathToYear, errNo)
        if (NumRooms==0) then
            do iTmp=2,NumDepartments-1 ! create rooms for each department
                NumRooms = NumRooms+1
                call initialize_room(Room(NumRooms), trim(Department(iTmp)%Code)//' Room', &
                    iTmp, 0, 0)
            end do
            errNo = 0
        end if

        ! read the teachers
        call custom_read_teachers(pathToYear, errNo)
        if (errNo/=0 .or. NumTeachers==1) then ! 1=Guest only

            do iTmp=2,NumDepartments-1 ! create teacher for each department
                NumTeachers = NumTeachers+1
                call check_array_bound (NumTeachers, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')
                Teacher(NumTeachers)%TeacherID = trim(Department(iTmp)%Code)//'-Teacher'
                Teacher(NumTeachers)%DeptIdx = iTmp
                Teacher(NumTeachers)%Role = GUEST
                Teacher(NumTeachers)%Name = trim(Department(iTmp)%Code)//' Teacher'
                Teacher(NumTeachers)%MaxLoad = 0
                Teacher(NumTeachers)%Specialization = 'Teaching'
                call set_password(Teacher(NumTeachers)%Password)

            end do
            errNo = 0
        end if

        ! the Developer account
        NumTeachers = NumTeachers+1
        call check_array_bound (NumTeachers, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')
        Teacher(NumTeachers)%TeacherID = PROGNAME
        Teacher(NumTeachers)%DeptIdx = NumDepartments
        Teacher(NumTeachers)%Role = SYSAD
        Teacher(NumTeachers)%Name = PROGNAME//' Developer'
        Teacher(NumTeachers)%MaxLoad = 0
        Teacher(NumTeachers)%Specialization = 'HEEDS Development'
        call set_password(Teacher(NumTeachers)%Password)

        ! the Administrator
        NumTeachers = NumTeachers+1
        call check_array_bound (NumTeachers, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')
        Teacher(NumTeachers)%TeacherID = SYSAD
        Teacher(NumTeachers)%DeptIdx = NumDepartments
        Teacher(NumTeachers)%Role = SYSAD
        Teacher(NumTeachers)%Name = PROGNAME//' Administrator'
        Teacher(NumTeachers)%MaxLoad = 0
        Teacher(NumTeachers)%Specialization = 'HEEDS Administration'
        call set_password(Teacher(NumTeachers)%Password)

        call sort_teachers()
        call sort_alphabetical_teachers()

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
        do jTmp=1,NumSubjects
            if (Subject(Subject(jTmp)%Prerequisite(1))%Name/='NONE') cycle ! pre-requisite is NONE
            if (Subject(jTmp)%lenCoreq/=1) cycle ! should be one token only
            iTmp = Subject(jTmp)%Corequisite(1)
            if (iTmp<=0) cycle ! token should be a named subject
            call log_comment(Subject(jTmp)%Name//'has co-requisite '//Subject(iTmp)%Name)
            ! pre-requisite is NONE, co-requisite is a named subject
            Subject(jTmp)%lenPreq = Subject(iTmp)%lenPreq
            Subject(jTmp)%Prerequisite = Subject(iTmp)%Prerequisite
        end do


        ! read the students
        call custom_read_students_from_enlistment(trim(pathToTerm)//'FINALGRADE', iTmp, errNo)
        if (iTmp>0) call sort_alphabetical_students()

        ! write XM data
        call xml_write_data()
        call terminate(trim(fileEXE)//SPACE//ACTION//' completed normally.')

    end subroutine import_custom_csv


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

            if (index(line,SYSAD)>0) cycle ! add later

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
            SYSAD, UniversityCode//' Administration', SYSAD, SPACE, SPACE)

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

            if (index(tDepartment,trim(SYSAD))>0) cycle ! add at the end

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
            SYSAD, UniversityCode//' Registrar', TheRegistrar, 'Z', NumColleges)

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
            if (k==0) k = NumDepartments ! refers to SYSAD
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
            if (k==0) k = NumDepartments ! refers to SYSAD

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
            Subject(NumSubjects)%DeptIdx = NumDepartments ! refers to SYSAD
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

        ! fees
        call custom_read_assessment(path, errNo)

        ! PREREQUISITES.CSV
        call custom_read_prerequisites(path, errNo)
        ! SUBJECTS-PREREQUISITES
        if (errNo/=0) call custom_read_subjects_prerequisites(path, errNo)

    end subroutine custom_read_subjects


    subroutine custom_read_assessment(path, errNo)

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
    end subroutine custom_read_assessment


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




    subroutine custom_read_students_from_enlistment (filePath, numEntries, ier)
        character (len=*), intent (in) :: filePath
        integer, intent (out) :: numEntries, ier

        type (TYPE_STUDENT) :: wrkStudent
        character (len=MAX_LEN_CURRICULUM_CODE) :: StdCurriculum
        character :: ch
        integer :: idxCURR, i, j, indexLoc

        numEntries = 0
        fileName = trim(filePath)//'.CSV'
        open(unit=unitRAW, file=fileName, status='old', iostat=ier)
        if (ier/=0) return

        call log_comment ('Retrieving list of students from '//fileName)
        ! skip first line
        read (unitRAW, AFORMAT, iostat = eof) line
        do
            read (unitRAW, AFORMAT, iostat = eof) line
            if (eof < 0) exit
            if (line(1:1) == '#' .or. line(1:3) == '   ') cycle
            call index_to_delimiters('"', line, ndels, pos)
            !no,code,name,crscd,year,f1,g1,u1,f2,g2,u2,f3,g3,u3,f4,g4,u4,units
            !1,"266441","Abad, Aiza  L.","BSBA-HRDM",3,"BL 30","2.25","3","HRDM 50","2.5","3","HRDM 51","1.5","3","ELECT 1 (BSBA)","2.5","3",15.00
            !1 2      3 4              5 6         7   8     9 1    1 1 1 1       1 1   1 1 1 2       2 2   2 2 2 2              2 2   2 3 3
            !                                                  0    1 2 3 4       5 6   7 8 9 0       1 2   3 4 5 6              7 8   9 0 1
            !0,"","","",0,"HRDM 52","2.0","3","","","","","","","","","",0.00
            !1 23 45 67   8       9 1   1 1 1 11 11 11 22 22 22 22 22 33
            !                       0   1 2 3 45 67 89 01 23 45 67 89 01
            if (pos(3)-pos(2)>1) then ! new student

                call initialize_student(wrkStudent)
                wrkStudent%StdNo = adjustl(line(pos(2)+1:pos(3)-1))
                j = year_prefix(wrkStudent)

                StdCurriculum = line(pos(6)+1:pos(7)-1)//DASH//wrkStudent%StdNo(1:j)
                if (StdCurriculum(1:3)=='AB-') StdCurriculum = 'AB'//StdCurriculum(4:)
                idxCURR = index_to_curriculum(StdCurriculum)
                if (idxCURR==0) then
                    StdCurriculum = 'OTHER'
                    idxCURR = index_to_curriculum(StdCurriculum)
                elseif (idxCURR<=0) then
                    idxCURR = -idxCURR
                  !write (*,*) line(:pos(8))//'... using curriculum '// Curriculum(idxCURR)%Code
                end if
                wrkStudent%Name = SPACE
                ! remove punctuation
                j = 0
                do i=pos(4)+1,pos(5)-1
                    ch = line(i:i)
                    if (index(SPECIAL,ch)>0) cycle
                    j = j+1
                    if (j>MAX_LEN_PERSON_NAME) cycle
                    wrkStudent%Name(j:j) = ch
                end do
                call upper_case(wrkStudent%Name)
                wrkStudent%CurriculumIdx = idxCURR
                wrkStudent%Classification = atoi(line(pos(7)+2:pos(8)-2))

                !call update_student_info(wrkStudent, indexLoc)
                if (indexLoc<0) numEntries = numEntries + 1

            end if
        end do
        close(unitRAW)
        call log_comment (itoa(numEntries)//' students added from '//fileName)

    end subroutine custom_read_students_from_enlistment


    subroutine generate_checklists()

        !character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_TEXT_GRADE) :: tGrade
        character(len=MAX_LEN_STUDENT_CODE) :: prevStdNo, tStdNo
        character (len=4) :: dirYear
        character (len=1) :: ch
        integer :: idxYear, idxTerm
        integer :: cdx, fdx, gdx, idx, sdx, ierr, i, j, k

        type(TYPE_PRE_ENLISTMENT) :: wrk
        character (len=MAX_LEN_CLASS_ID) :: tClassID, tCode

        ! read basic data for university
        call xml_read_basic_data(pathToYear)

        ! zero out counters
        do cdx=MAX_ALL_DUMMY_SUBJECTS,MAX_ALL_SUBJECTS
            Subject(cdx)%Failrate = 0.0
            Subject(cdx)%GrandTotal = 0
        end do

        do idxYear = baseYear,currentYear
            dirYear = itoa(idxYear)

            do idxTerm = 1,3

                ! try 'FINALGRADE.CSV'
                fileName = trim(dirDATA)//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP// &
                    'FINALGRADE.CSV'
                open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
                if (ierr==0) then
                    write(*,*) 'Retrieving grades from '//trim(fileName)

                    ! initialize student data
                    call initialize_student(Student(0))
                    Student = Student(0)
                    NumStudents = 0

                    ! initialize dummy classes
                    NumSections(idxTerm) = 0
                    call initialize_section (Section(idxTerm,0))
                    Section(idxTerm,:) = Section(idxTerm,0)

                    ! initialize pre-enlisted subjects of students
                    call initialize_pre_enlistment(Enlistment(idxTerm,0))
                    Enlistment(:,:) = Enlistment(idxTerm,0)

                    prevStdNo = '#-#-#-#-#-#-#-#'
                    ! skip first line
                    read (unitRAW, AFORMAT, iostat = eof) line
                    do
                        read (unitRAW, AFORMAT, iostat = eof) line
                        if (eof < 0) exit
                        if (line(1:1) == '#' .or. line(1:3) == '   ') cycle

                        call index_to_delimiters('"', line, ndels, pos)
                        !no,code,name,crscd,year,f1,g1,u1,f2,g2,u2,f3,g3,u3,f4,g4,u4,units
                        !1,"266441","Abad, Aiza  L.","BSBA-HRDM",3,"BL 30","2.25","3","HRDM 50","2.5","3","HRDM 51","1.5","3","ELECT 1 (BSBA)","2.5","3",15.00
                        !1 2      3 4              5 6         7   8     9 1    1 1 1 1       1 1   1 1 1 2       2 2   2 2 2 2              2 2   2 3 3
                        !                                                  0    1 2 3 4       5 6   7 8 9 0       1 2   3 4 5 6              7 8   9 0 1
                        !0,"","","",0,"HRDM 52","2.0","3","","","","","","","","","",0.00
                        !1 23 45 67   8       9 1   1 1 1 11 11 11 22 22 22 22 22 33
                        !                       0   1 2 3 45 67 89 01 23 45 67 89 01

                        if (pos(3)-pos(2)>1) then ! new student

                            NumStudents = NumStudents + 1

                            Student(NumStudents)%StdNo = adjustl(line(pos(2)+1:pos(3)-1))
                            Student(NumStudents)%Name = SPACE
                            ! remove punctuation
                            j = 0
                            do i=pos(4)+1,pos(5)-1
                                ch = line(i:i)
                                if (index(SPECIAL,ch)>0) cycle
                                j = j+1
                                if (j>MAX_LEN_PERSON_NAME) cycle
                                Student(NumStudents)%Name(j:j) = ch
                            end do
                            call upper_case(Student(NumStudents)%Name)
                        end if

                        do idx=8,31,6
                            if (pos(idx+1)-pos(idx)==1) cycle ! empty subject
                            ! subject, grade
                            tSubject = adjustl(line(pos(idx)+1:pos(idx+1)-1))
                            call upper_case(tSubject)
                            cdx = index_to_subject(tSubject)
                            if (cdx<=0) then
                                !call log_comment (trim(Student(NumStudents)%Name)//' - "'//trim(tSubject)// &
                                !    '" not in catalog')
                                !cycle
                                write(*,*) trim(Student(NumStudents)%Name)//' - "'//trim(tSubject)//'" not in catalog'
                                NumAdditionalSubjects = NumAdditionalSubjects+1
                                cdx = NumSubjects + NumAdditionalSubjects

                                Subject(cdx)%Name = tSubject
                                Subject(cdx)%Title = tSubject
                                Subject(cdx)%DeptIdx = NumDepartments
                                Subject(cdx)%Units = 3.0

                                Subject(cdx)%TermOffered = 7
                                Subject(cdx)%LectHours = 3.0
                                Subject(cdx)%MinLectSize = 50
                                Subject(cdx)%MaxLectSize = 50
                                Subject(cdx)%LectLoad = 0.0
                                Subject(cdx)%LabHours = 0.0
                                Subject(cdx)%MinLabSize = 50
                                Subject(cdx)%MaxLabSize = 50
                                Subject(cdx)%LabLoad = 0.0

                                k = 1
                                Subject(cdx)%lenPreq = k
                                Subject(cdx)%Prerequisite(k) = INDEX_TO_NONE
                                Subject(cdx)%lenCoreq = k
                                Subject(cdx)%Corequisite = INDEX_TO_NONE
                                Subject(cdx)%lenConc = k
                                Subject(cdx)%Concurrent = INDEX_TO_NONE
                                Subject(cdx)%lenConcPreq = k
                                Subject(cdx)%ConcPrerequisite= INDEX_TO_NONE

                                Subject(cdx)%LabFee = 0.0
                                Subject(cdx)%Tuition = 0.0

                            end if
                            tGrade  = adjustl(line(pos(idx+2)+1:pos(idx+3)-1))
                            if (tGrade==SPACE) then
                                gdx = gdxDRP
                            else
                                gdx = index_to_grade(tGrade)
                                if (gdx==0) gdx = gdxDRP
                            end if

                            fdx = Enlistment(idxTerm,NumStudents)%lenSubject + 1
                            call check_array_bound (fdx, MAX_SUBJECTS_PER_TERM, &
                                'MAX_SUBJECTS_PER_TERM : '//trim(Student(NumStudents)%StdNo)//' - too many subjects taken?')
                            Enlistment(idxTerm,NumStudents)%Subject(fdx) = cdx
                            Enlistment(idxTerm,NumStudents)%Grade(fdx) = gdx
                            Enlistment(idxTerm,NumStudents)%lenSubject = fdx

                            if (is_grade_numeric_pass(gdx)) then  ! grade counter
                                Subject(cdx)%GrandTotal(1,idxTerm) = Subject(cdx)%GrandTotal(1,idxTerm) + 1
                            else
                                Subject(cdx)%GrandTotal(2,idxTerm) = Subject(cdx)%GrandTotal(2,idxTerm) + 1
                            end if

                            ! create a class?
                            if (cdx>99) then
                                tCode = 'Z'//itoa(cdx)
                            else if (cdx>9) then
                                tCode = 'Z0'//itoa(cdx)
                            else
                                tCode = 'Z00'//itoa(cdx)
                            end if

                            if (Subject(cdx)%LabHours>0) then ! subject has lab/recitation
                                tClassID = trim(Subject(cdx)%Name)//SPACE//trim(tCode)//DASH//'1L'
                            else
                                tClassID = trim(Subject(cdx)%Name)//SPACE//tCode
                            end if
                            k = index_to_section(tClassID, NumSections(idxTerm), Section(idxTerm,0:) )

                            if (k==0) then ! add a class

                                if (Subject(cdx)%LectHours>0) then ! subject has lecture
                                    tClassID = trim(Subject(cdx)%Name)//SPACE//tCode
                                    k = NumSections(idxTerm) + 1
                                    NumSections(idxTerm) = k
                                    Section(idxTerm,k) = TYPE_SECTION (tClassID, tCode, SPACE, &
                                        NumDepartments, cdx, Subject(cdx)%MaxLectSize, Subject(cdx)%MaxLectSize, &
                                        1, 0, 0, 0, 0, 0)
                                    !write(*,*) 'Added lecture ', k, tClassID
                                end if
                                if (Subject(cdx)%LabHours>0) then ! subject has lab/recitation
                                    tCode = trim(tCode)//DASH//'1L'
                                    tClassID = trim(Subject(cdx)%Name)//SPACE//tCode
                                    k = NumSections(idxTerm) + 1
                                    NumSections(idxTerm) = k
                                    Section(idxTerm,k) = TYPE_SECTION (tClassID, tCode, SPACE, &
                                        NumDepartments, cdx, Subject(cdx)%MaxLabSize, Subject(cdx)%MaxLabSize, &
                                        1, 0, 0, 0, 0, 0)
                                    !write(*,*) 'Added lab ', k, tClassID
                                end if
                            end if
                            Enlistment(idxTerm,NumStudents)%Section(fdx) = k

                        end do

                    end do
                    close(unitRAW)
                    call write_transcripts(idxYear, idxTerm)

                    ! create "equivalent" ENLISTMENT.XML
                    fileName = trim(dirDATA)//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP
                    call xml_write_classes(fileName, NumSections(idxTerm), Section(idxTerm,0:), 0)
                    call xml_write_pre_enlistment(fileName, 'EQUIVALENT-ENLISTMENT', Enlistment(idxTerm,0:), Section(idxTerm,0:))

                end if

                ! try 'STUDENT-SUBJECT-GRADE.CSV'
                fileName = trim(dirDATA)//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP// &
                    'STUDENT-SUBJECT-GRADE.CSV'
                open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
                if (ierr==0) then
                    write(*,*) 'Retrieving grades from '//trim(fileName)

                    ! initialize student data
                    call initialize_student(Student(0))
                    Student = Student(0)
                    NumStudents = 0

                    ! initialize pre-enlisted subjects of students
                    call initialize_pre_enlistment(Enlistment(idxTerm,0))
                    Enlistment(:,:) = Enlistment(idxTerm,0)

                    prevStdNo = '#-#-#-#-#-#-#-#'
                    ! skip first line
                    read (unitRAW, AFORMAT, iostat = eof) line
                    do
                        read (unitRAW, AFORMAT, iostat = eof) line
                        if (eof < 0) exit
                        if (line(1:1) == '#' .or. line(1:3) == '   ') cycle

                        call index_to_delimiters(COMMA, line, ndels, pos)
                        !1       2          3 4
                        !06-00593,SOC SCI 12,0,Politics and Governance with Philippine Constituti,,Eva Malupeng,I,,138467,183,1,"Accad, Leonard Andrew Bramaje"
                        !06-00593,ENG 102,5,Speech and Oral Communication,,Eva Malupeng,Failed,,138467,1892,1,"Accad, Leonard Andrew Bramaje"
                        !06-00593,PE F,0,Badminton/Table Tennis,,Eva Malupeng,N,,138467,939,1,"Accad, Leonard Andrew Bramaje"
                        tStdNo = adjustl(line(:pos(2)-1))
                        if (tStdNo/=prevStdNo) then ! new student
                            NumStudents = NumStudents + 1

                            Student(NumStudents)%StdNo = tStdNo
                            prevStdNo = tStdNo
                            Student(NumStudents)%Name = SPACE
                            idx = len_trim(line)-1
                            cdx = 0
                            do i=idx,1,-1
                                if (line(i:i)=='"') then
                                    cdx = i
                                    exit
                                end if
                            end do
                            if (cdx>0) Student(NumStudents)%Name = line(cdx+1:idx)
                        end if

                        ! subject, grade
                        tSubject = adjustl(line(pos(2)+1:pos(3)-1))
                        call upper_case(tSubject)
                        cdx = index_to_subject(tSubject)
                        if (cdx<=0) then
                            !call log_comment (trim(Student(NumStudents)%Name)//' - "'//trim(tSubject)// &
                            !    '" not in catalog')
                            !cycle
                            write(*,*) trim(Student(NumStudents)%Name)//' - "'//trim(tSubject)//'" not in catalog'
                            NumAdditionalSubjects = NumAdditionalSubjects+1
                            cdx = NumSubjects + NumAdditionalSubjects

                            Subject(cdx)%Name = tSubject
                            Subject(cdx)%Title = tSubject
                            Subject(cdx)%DeptIdx = NumDepartments
                            Subject(cdx)%Units = 3.0

                            Subject(cdx)%TermOffered = 7
                            Subject(cdx)%LectHours = 3.0
                            Subject(cdx)%MinLectSize = 50
                            Subject(cdx)%MaxLectSize = 50
                            Subject(cdx)%LectLoad = 0.0
                            Subject(cdx)%LabHours = 0.0
                            Subject(cdx)%MinLabSize = 50
                            Subject(cdx)%MaxLabSize = 50
                            Subject(cdx)%LabLoad = 0.0

                            k = 1
                            Subject(cdx)%lenPreq = k
                            Subject(cdx)%Prerequisite(k) = INDEX_TO_NONE
                            Subject(cdx)%lenCoreq = k
                            Subject(cdx)%Corequisite = INDEX_TO_NONE
                            Subject(cdx)%lenConc = k
                            Subject(cdx)%Concurrent = INDEX_TO_NONE
                            Subject(cdx)%lenConcPreq = k
                            Subject(cdx)%ConcPrerequisite= INDEX_TO_NONE

                            Subject(cdx)%LabFee = 0.0
                            Subject(cdx)%Tuition = 0.0

                        end if

                        tGrade  = adjustl(line(pos(3)+1:pos(4)-1))
                        if (tGrade==SPACE) then
                            gdx = gdxDRP
                        elseif (tGrade=='0') then
                            gdx = gdxDRP
                        else
                            gdx = index_to_grade(tGrade)
                            if (gdx==0) gdx = gdxDRP
                        end if

                        fdx = Enlistment(idxTerm,NumStudents)%lenSubject + 1
                        call check_array_bound (fdx, MAX_SUBJECTS_PER_TERM, &
                            'MAX_SUBJECTS_PER_TERM : '//trim(Student(NumStudents)%StdNo)//' - too many subjects taken?')
                        Enlistment(idxTerm,NumStudents)%Subject(fdx) = cdx
                        Enlistment(idxTerm,NumStudents)%Grade(fdx) = gdx
                        Enlistment(idxTerm,NumStudents)%lenSubject = fdx

!                        ! create a class?
!                        if (cdx>99) then
!                            tCode = 'Z'//itoa(cdx)
!                        else if (cdx>9) then
!                            tCode = 'Z0'//itoa(cdx)
!                        else
!                            tCode = 'Z00'//itoa(cdx)
!                        end if
!
!                        if (Subject(cdx)%LabHours>0) then ! subject has lab/recitation
!                            tClassID = trim(Subject(cdx)%Name)//SPACE//trim(tCode)//DASH//'1L'
!                        else
!                            tClassID = trim(Subject(cdx)%Name)//SPACE//tCode
!                        end if
!                        k = index_to_section(tClassID, NumSections(idxTerm), Section(idxTerm,0:) )
!
!                        if (k==0) then ! add a class
!
!                            if (Subject(cdx)%LectHours>0) then ! subject has lecture
!                                tClassID = trim(Subject(cdx)%Name)//SPACE//tCode
!                                k = NumSections(idxTerm) + 1
!                                NumSections(idxTerm) = k
!                                Section(idxTerm,k) = TYPE_SECTION (tClassID, tCode, SPACE, &
!                                    NumDepartments, cdx, Subject(cdx)%MaxLectSize, Subject(cdx)%MaxLectSize, &
!                                    1, 0, 0, 0, 0, 0)
!                                !write(*,*) 'Added lecture ', k, tClassID
!                            end if
!                            if (Subject(cdx)%LabHours>0) then ! subject has lab/recitation
!                                tCode = trim(tCode)//DASH//'1L'
!                                tClassID = trim(Subject(cdx)%Name)//SPACE//tCode
!                                k = NumSections(idxTerm) + 1
!                                NumSections(idxTerm) = k
!                                Section(idxTerm,k) = TYPE_SECTION (tClassID, tCode, SPACE, &
!                                    NumDepartments, cdx, Subject(cdx)%MaxLabSize, Subject(cdx)%MaxLabSize, &
!                                    1, 0, 0, 0, 0, 0)
!                                !write(*,*) 'Added lab ', k, tClassID
!                            end if
!                        end if
!                        Enlistment(idxTerm,NumStudents)%Section(fdx) = k

                        if (is_grade_numeric_pass(gdx)) then  ! grade counter
                            Subject(cdx)%GrandTotal(1,idxTerm) = Subject(cdx)%GrandTotal(1,idxTerm) + 1
                        else
                            Subject(cdx)%GrandTotal(2,idxTerm) = Subject(cdx)%GrandTotal(2,idxTerm) + 1
                        end if

                    end do
                    close(unitRAW)
                    call write_transcripts(idxYear, idxTerm)

                end if

                ! try 'STDNO-NAME-SUBJ-TITLE-GRADE.CSV'
                fileName = trim(dirDATA)//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP// &
                'STDNO-NAME-SUBJ-TITLE-GRADE.CSV'
                open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
                if (ierr==0) then
                    write(*,*) 'Retrieving grades from '//trim(fileName)

                    ! initialize student data
                    call initialize_student(Student(0))
                    Student = Student(0)
                    NumStudents = 0

                    ! initialize pre-enlisted subjects of students
                    call initialize_pre_enlistment(Enlistment(idxTerm,0))
                    Enlistment(:,:) = Enlistment(idxTerm,0)

                    prevStdNo = '#-#-#-#-#-#-#-#'
                    ! skip first line
                    read (unitRAW, AFORMAT, iostat = eof) line
                    do
                        read (unitRAW, AFORMAT, iostat = eof) line
                        if (eof < 0) exit
                        if (line(1:1) == '#' .or. line(1:3) == '   ') cycle

                        call index_to_delimiters('"', line, ndels, pos)
!code,name,subjcd,subjnm,grade,usernm,remark,prerem,controlno,subject,no
!"06-00593","Accad, Leonard Andrew Bramaje      ","PE F","Badminton/Table Tennis",0,"Eva Malupeng                            ","N"," ",138467,939,1
!"06-00593","Accad, Leonard Andrew Bramaje      ","BN 11","Basic Nutrition",1500,"Eva Malupeng                            ","F"," ",138467,1614,1
!2        3,4                                   5,6     7,8               9,    ,10
!"06-00593","Accad, Leonard Andrew Bramaje      ","SOC SCI 12","Politics and Governance with Philippine Constituti",0,"Eva Malupeng                            ","I"," ",138467,183,1
!"06-00593","Accad, Leonard Andrew Bramaje      ","ENG 102","Speech and Oral Communication",1500,"Eva Malupeng                            ","F"," ",138467,1892,1
!"07-07105","Allauigan, Crisostomo Isabelito B. ","PUBOFFRS","Public Officers & Local Government Code",85,"Eva Malupeng                            ","P"," ",100643,54,1
!"07-07105","Allauigan, Crisostomo Isabelito B. ","CRIMPRO","Criminal Procedure",84,"Eva Malupeng                            ","P"," ",100643,65,1
!2        3,4                                   5,6       7,8                  9,  ,10
                        tStdNo = adjustl(line(pos(2)+1:pos(3)-1))
                        if (tStdNo/=prevStdNo) then ! new student
                            NumStudents = NumStudents + 1

                            Student(NumStudents)%StdNo = tStdNo
                            prevStdNo = tStdNo
                            Student(NumStudents)%Name = SPACE
                            ! remove punctuation
                            j = 0
                            do i=pos(4)+1,pos(5)-1
                                ch = line(i:i)
                                if (index(SPECIAL,ch)>0) cycle
                                j = j+1
                                if (j>MAX_LEN_PERSON_NAME) cycle
                                Student(NumStudents)%Name(j:j) = ch
                            end do
                            call upper_case(Student(NumStudents)%Name)
                        end if

                        tSubject = adjustl(line(pos(6)+1:pos(7)-1))
                        call upper_case(tSubject)
                        cdx = index_to_subject(tSubject)
                        if (cdx<=0) then
                            !call log_comment (trim(Student(NumStudents)%Name)//' - "'//trim(tSubject)// &
                            !    '" not in catalog')
                            !cycle
                            write(*,*) trim(Student(NumStudents)%Name)//' - "'//trim(tSubject)//'" not in catalog'
                            NumAdditionalSubjects = NumAdditionalSubjects+1
                            cdx = NumSubjects + NumAdditionalSubjects

                            Subject(cdx)%Name = tSubject
                            Subject(cdx)%Title = tSubject
                            Subject(cdx)%DeptIdx = NumDepartments
                            Subject(cdx)%Units = 3.0

                            Subject(cdx)%TermOffered = 7
                            Subject(cdx)%LectHours = 3.0
                            Subject(cdx)%MinLectSize = 50
                            Subject(cdx)%MaxLectSize = 50
                            Subject(cdx)%LectLoad = 0.0
                            Subject(cdx)%LabHours = 0.0
                            Subject(cdx)%MinLabSize = 50
                            Subject(cdx)%MaxLabSize = 50
                            Subject(cdx)%LabLoad = 0.0

                            k = 1
                            Subject(cdx)%lenPreq = k
                            Subject(cdx)%Prerequisite(k) = INDEX_TO_NONE
                            Subject(cdx)%lenCoreq = k
                            Subject(cdx)%Corequisite = INDEX_TO_NONE
                            Subject(cdx)%lenConc = k
                            Subject(cdx)%Concurrent = INDEX_TO_NONE
                            Subject(cdx)%lenConcPreq = k
                            Subject(cdx)%ConcPrerequisite= INDEX_TO_NONE

                            Subject(cdx)%LabFee = 0.0
                            Subject(cdx)%Tuition = 0.0

                        end if

                        tGrade  = adjustl(line(pos(9)+2:pos(10)-2))
                        if (len_trim(tGrade)==4 .and. tGrade(1:1)=='1') then ! convert to 1.00 - 5.00
                            tGrade(1:1) = tGrade(2:2)
                            tGrade(2:2) = DOT
                        end if
                        if (tGrade==SPACE) then
                            gdx = gdxDRP
                        else
                            gdx = index_to_grade(tGrade)
                            if (gdx==0) gdx = gdxDRP
                        end if

                        fdx = Enlistment(idxTerm,NumStudents)%lenSubject + 1
                        call check_array_bound (fdx, MAX_SUBJECTS_PER_TERM, &
                            'MAX_SUBJECTS_PER_TERM : '//trim(Student(NumStudents)%StdNo)//' - too many subjects taken?')
                        Enlistment(idxTerm,NumStudents)%Subject(fdx) = cdx
                        Enlistment(idxTerm,NumStudents)%Grade(fdx) = gdx
                        Enlistment(idxTerm,NumStudents)%lenSubject = fdx

                        if (is_grade_numeric_pass(gdx)) then  ! grade counter
                            Subject(cdx)%GrandTotal(1,idxTerm) = Subject(cdx)%GrandTotal(1,idxTerm) + 1
                        else
                            Subject(cdx)%GrandTotal(2,idxTerm) = Subject(cdx)%GrandTotal(2,idxTerm) + 1
                        end if


!                        ! create a class?
!                        if (cdx>99) then
!                            tCode = 'Z'//itoa(cdx)
!                        else if (cdx>9) then
!                            tCode = 'Z0'//itoa(cdx)
!                        else
!                            tCode = 'Z00'//itoa(cdx)
!                        end if
!
!                        if (Subject(cdx)%LabHours>0) then ! subject has lab/recitation
!                            tClassID = trim(Subject(cdx)%Name)//SPACE//trim(tCode)//DASH//'1L'
!                        else
!                            tClassID = trim(Subject(cdx)%Name)//SPACE//tCode
!                        end if
!                        k = index_to_section(tClassID, NumSections(idxTerm), Section(idxTerm,0:) )
!
!                        if (k==0) then ! add a class
!
!                            if (Subject(cdx)%LectHours>0) then ! subject has lecture
!                                tClassID = trim(Subject(cdx)%Name)//SPACE//tCode
!                                k = NumSections(idxTerm) + 1
!                                NumSections(idxTerm) = k
!                                Section(idxTerm,k) = TYPE_SECTION (tClassID, tCode, SPACE, &
!                                    NumDepartments, cdx, Subject(cdx)%MaxLectSize, Subject(cdx)%MaxLectSize, &
!                                    1, 0, 0, 0, 0, 0)
!                                !write(*,*) 'Added lecture ', k, tClassID
!                            end if
!                            if (Subject(cdx)%LabHours>0) then ! subject has lab/recitation
!                                tCode = trim(tCode)//DASH//'1L'
!                                tClassID = trim(Subject(cdx)%Name)//SPACE//tCode
!                                k = NumSections(idxTerm) + 1
!                                NumSections(idxTerm) = k
!                                Section(idxTerm,k) = TYPE_SECTION (tClassID, tCode, SPACE, &
!                                    NumDepartments, cdx, Subject(cdx)%MaxLabSize, Subject(cdx)%MaxLabSize, &
!                                    1, 0, 0, 0, 0, 0)
!                                !write(*,*) 'Added lab ', k, tClassID
!                            end if
!                        end if
!                        Enlistment(idxTerm,NumStudents)%Section(fdx) = k

                    end do
                    close(unitRAW)
                    call write_transcripts(idxYear, idxTerm)

                end if

                ! try 'ENLISTMENT.XML'
                fileName = trim(dirDATA)//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP// &
                    'ENLISTMENT.XML'
                open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
                if (ierr==0) then
                    write(*,*) 'Retrieving grades from '//trim(fileName)

                    ! initialize student data
                    call initialize_student(Student(0))
                    Student = Student(0)
                    NumStudents = 0

                    ! initialize pre-enlisted subjects of students
                    call initialize_pre_enlistment(Enlistment(idxTerm,0))
                    Enlistment(:,:) = Enlistment(idxTerm,0)

                    prevStdNo = '#-#-#-#-#-#-#-#'
                    ! examine the file line by line
                    do
                        read(unitRAW, AFORMAT, iostat=eof) line
                        if (eof<0) exit

                        ! get tag and value if any; exit on any error
                        call xml_parse_line(line, tag, value, eof)
                        if (eof/=0) exit

                        select case (trim(tag))

                            case ('Student') ! initialize temporary college data
                                call initialize_pre_enlistment (wrk)
                                NumStudents = NumStudents + 1

                            case ('StdNo')
                                tStdNo = adjustl(value)
                                Student(NumStudents)%StdNo = tStdNo

                            case ('Name')
                                Student(NumStudents)%Name = adjustl(value)

                            case ('Graded')

                                idx = index(value, COMMA)

                                ! extract grade
                                tGrade = adjustl(value(idx+1:))
                                gdx = index_to_grade(tGrade)

                                ! extract subject
                                tClassID = adjustl(value(:idx-1))
                                sdx = len_trim(tClassID)
                                do while (sdx>1 .and. tClassID(sdx:sdx)/=SPACE)
                                    sdx = sdx-1
                                end do
                                tSubject = tClassID(:sdx)
                                cdx = index_to_subject(tSubject)
                                if (cdx<=0) then ! subject code not found
                                    call log_comment ('No such class; ignored - '//tClassID)
                                    cycle
                                end if

                                call check_array_bound (wrk%lenSubject+1, MAX_SUBJECTS_PER_TERM, 'MAX_SUBJECTS_PER_TERM')
                                wrk%lenSubject = wrk%lenSubject + 1
                                wrk%Subject(wrk%lenSubject) = cdx
                                wrk%Grade(wrk%lenSubject) = gdx

                                if (is_grade_numeric_pass(gdx)) then  ! grade counter
                                    Subject(cdx)%GrandTotal(1,idxTerm) = Subject(cdx)%GrandTotal(1,idxTerm) + 1
                                else
                                    Subject(cdx)%GrandTotal(2,idxTerm) = Subject(cdx)%GrandTotal(2,idxTerm) + 1
                                end if

                            case ('/Student')
                                wrk%NPriority = wrk%lenSubject
                                Enlistment(idxTerm,NumStudents) = wrk

                            case default

                        end select

                    end do

                    close(unitRAW)

                    call write_transcripts(idxYear, idxTerm)

                end if

                if (NumStudents==0) then
                    write(*,*) 'Grade file not found in '// &
                        trim(dirDATA)//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP
                    cycle
                end if


            end do ! idxTerm = 1,3

        end do ! idxYear = baseYear,currentYear

        ! Subject() was modified; rewrite
        call xml_write_subjects(trim(pathToYear)//'SUBJECTS.XML')
        call terminate(trim(fileEXE)//SPACE//ACTION//' completed normally.')

    end subroutine generate_checklists


    subroutine write_transcripts(idxYear, idxTerm)

        integer, intent (in) :: idxYear, idxTerm

        integer :: idx, std

        ! create student directories
        call make_student_directories()
        do std=1,NumStudents

            ! generate file name
            idx = year_prefix(Student(std))
            fileName = trim(dirTRANSCRIPTS)//trim(Student(std)%StdNo(1:idx))//DIRSEP// &
                trim(Student(std)%StdNo)//'.XML'

            inquire(file=trim(fileName), exist=pathExists)
            if (.not. pathExists) then
                open(unit=unitXML, file=trim(fileName), status='new')

                write(unitXML,AFORMAT) XML_DOC, &
                    '<'//XML_ROOT_STUDENT_RECORD//'>', &
                    '    <comment>', &
                    '        Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                    FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
                    '        StdNo - Student number', &
                    '        Name - Student name', &
                    '        Grade - YEAR,TERM,SUBJECT,GRADE', &
                    '    </comment>'

                call xml_write_character(unitXML, indent0, 'StdNo', Student(std)%StdNo)
                call xml_write_character(unitXML, indent0, 'Name', Student(std)%Name)

            else
                open(unit=unitXML, file=trim(fileName), status='old', position='append')
                backspace(unitXML)
            end if

            do idx=1,Enlistment(idxTerm,std)%lenSubject
                call xml_write_character(unitXML, indent0, txtGradeType(1), &
                    trim(itoa(idxYear))//COMMA// &
                    trim(txtSemester(idxTerm))//COMMA// &
                    trim(Subject(Enlistment(idxTerm,std)%Subject(idx))%Name)//COMMA// &
                    txtGrade(pGrade(Enlistment(idxTerm,std)%Grade(idx))) )
            end do

            write(unitXML,AFORMAT) '</'//XML_ROOT_STUDENT_RECORD//'>'
            close(unitXML)

            if (mod(std,1000)==0) write(*,*) std,  '/', NumStudents, ' done...'
        end do

    end subroutine write_transcripts


    subroutine read_students_from_enlistment(idxYear, idxTerm)

        integer, intent(in) :: idxYear, idxTerm

        type (TYPE_STUDENT) :: wrkStudent

        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character (len=4) :: dirYear
        integer :: idx, ierr

        dirYear = itoa(idxYear)
        fileName = trim(dirDATA)//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP//'ENLISTMENT.XML'
        open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
        if (ierr/=0) return

        ! examine the file line by line
        do
            read(unitRAW, AFORMAT, iostat=eof) line
            if (eof<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, eof)
            if (eof/=0) exit

            select case (trim(tag))

                case ('Student')
                    call initialize_student(wrkStudent)
                    wrkStudent%CurriculumIdx = NumCurricula

                case ('StdNo')
                    tStdNo = adjustl(value)
                    wrkStudent%StdNo = tStdNo

                case ('Name')
                    wrkStudent%Name = adjustl(value)

                case ('/Student')
                    call update_student_info(wrkStudent, idx)

                case default

            end select

        end do
        close(unitRAW)

    end subroutine read_students_from_enlistment

