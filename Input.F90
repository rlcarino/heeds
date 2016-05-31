
    subroutine import_custom_csv()

        integer :: jTmp, iTmp, errNo

        ! read the colleges
        call custom_read_colleges(pathToYear, errNo)
        call terminate(errNo,'Error in reading the list of colleges')

        ! log directory for users in the college
        do iTmp=1,NumColleges
            call make_directory( trim(dirLOG)//trim(College(iTmp)%Code)//DIRSEP )
        end do

        ! read the departments
        call custom_read_departments(pathToYear, errNo)
        call terminate(errNo,'Error in reading the list of departments')

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
                Teacher(NumTeachers)%TeacherId = trim(Department(iTmp)%Code)//'-Teacher'
                Teacher(NumTeachers)%DeptIdx = iTmp
                Teacher(NumTeachers)%Role = GUEST
                Teacher(NumTeachers)%Name = trim(Department(iTmp)%Code)//' Teacher'
                Teacher(NumTeachers)%MaxLoad = 0
                Teacher(NumTeachers)%Specialization = 'Teaching'
                call set_password(Teacher(NumTeachers)%Password, trim(Teacher(NumTeachers)%TeacherId))

            end do
            errNo = 0
        end if

        ! the Developer account
        NumTeachers = NumTeachers+1
        call check_array_bound (NumTeachers, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')
        Teacher(NumTeachers)%TeacherId = PROGNAME
        Teacher(NumTeachers)%DeptIdx = NumDepartments
        Teacher(NumTeachers)%Role = SYSAD
        Teacher(NumTeachers)%Name = PROGNAME//' Developer'
        Teacher(NumTeachers)%MaxLoad = 0
        Teacher(NumTeachers)%Specialization = 'HEEDS Development'
        call set_password(Teacher(NumTeachers)%Password)

        ! the Administrator
        NumTeachers = NumTeachers+1
        call check_array_bound (NumTeachers, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')
        Teacher(NumTeachers)%TeacherId = SYSAD
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
        call terminate(errNo,'Error in reading the list of subjects')

        call get_subject_areas()

        ! read the curricular programs
        call custom_read_curricula(pathToYear, errNo)
        call terminate(errNo,'Error in reading the list of curricular programs')

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

            if (index(line,UniversityCode)>0) cycle ! add later

            NumColleges = NumColleges + 1
            call check_array_bound (NumColleges, MAX_ALL_COLLEGES, 'MAX_ALL_COLLEGES')
            College(NumColleges)%Code = fn_blank_to_underscore(line(pos(2)+1:pos(3)-1))
            College(NumColleges)%Name = line(pos(4)+1:pos(5)-1)

        end do

        close(unitRAW)
        call log_comment (itoa(NumColleges)//' colleges after reading '//fileName)

        ! add 'administrative' college for data that does not fit in the 'academic' colleges
        NumColleges = NumColleges + 1
        call check_array_bound (NumColleges, MAX_ALL_COLLEGES, 'MAX_ALL_COLLEGES')
        call initialize_college (College(NumColleges), &
            UniversityCode, trim(UniversityCode)//' Administration', SYSAD, SPACE, SPACE)

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

            tDepartment = fn_blank_to_underscore(line(pos(2)+1:pos(3)-1))

            if (index(tDepartment,trim(UniversityCode))>0) cycle ! add at the end

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
            UniversityCode, trim(UniversityCode)//' Registrar', TheRegistrar, 'Z', NumColleges)

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

            tDepartment = fn_blank_to_underscore(line(pos(6)+2:pos(7)-2))
            !tDepartment = line(pos(6)+2:pos(7)-2)
            k = index_to_dept(tDepartment)
            if (k==0) k = NumDepartments ! refers to UniversityCode
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
            call initialize_room (Room(j+1), tCode, k, locx, seats, 0)
            NumRooms = NumRooms + 1

        end do
        call log_comment (itoa(NumRooms)//' rooms after reading '//fileName)

    end subroutine custom_read_rooms



    subroutine custom_read_teachers(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent(out) :: errNo

        character (len=MAX_LEN_PERSON_NAME) :: tTeacher
        character (len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character (len=MAX_LEN_USERNAME)   :: teacherId
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
            tDepartment = fn_blank_to_underscore(line(pos(6)+1:pos(7)-1))
            k = index_to_dept(tDepartment)
            if (k==0) k = NumDepartments ! refers to UniversityCode

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
            Subject(NumSubjects)%DeptIdx = NumDepartments ! refers to UniversityCode
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
            call upper_case(tSubject)
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
            call upper_case(tSubject)
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
            call upper_case(tSubject)
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
            call upper_case(preq1)
            call upper_case(preq2)
            call upper_case(preq3)
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
            call upper_case(coreq1)
            call upper_case(coreq2)
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

            tCollege = fn_blank_to_underscore(line(pos(6)+1:pos(7)-1))
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
                        !if (isSubject_offered(i,mod(term,3)) ) then
                        if ( isSubject_offered(i,term) ) then
                            if (.not. is_prerequisite_satisfiable_in_curriculum(i,Curriculum(NumCurricula))) then
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
                !wrkStudent%statusScholastic = max(0,atoi(line(pos(7)+2:pos(8)-2)))

                !call update_student_info(wrkStudent, indexLoc)
                if (indexLoc<0) numEntries = numEntries + 1

            end if
        end do
        close(unitRAW)
        call log_comment (itoa(numEntries)//' students added from '//fileName)

    end subroutine custom_read_students_from_enlistment


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
                    call upper_case(tStdNo)
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

!        subroutine read_stud_profile()
!
!        ! read CSU-Gonzaga students from XML mysql-export
!
!            type (TYPE_STUDENT) :: wrkStudent
!            character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
!            integer :: idxCURR, indexLoc, numEntries, ier
!
!            pathToTerm = trim(pathToYear)//trim(txtSemester(currentTerm))//DIRSEP
!
!            fileName = trim(pathToYear)//'stud_profile.xml'
!            open(unit=unitETC, file=fileName, status='old', iostat=ier)
!            if (ier/=0) return
!            numEntries = 0
!
!            ! examine the file line by line
!            do
!                read(unitETC, AFORMAT, iostat=eof) line
!                if (eof<0) exit
!
!                ! get tag and value if any
!                call xml_parse_line(line, tag, value, eof)
!
!                !if (eof/=0) exit
!                !write(*,*) eof, trim(line), trim(tag), trim(value)
!
!    !    <database name="csudata">
!    !        <table name="studprofile">
!    !            <column name="idnum">09-0695</column>
!    !            <column name="sname">ABAD</column>
!    !            <column name="fname">KATHRINA</column>
!    !            <column name="mname">A</column>
!    !            <column name="course">BSHIM</column>
!    !            <column name="college">CAS</column>
!    !        </table>
!                select case (trim(tag))
!
!                    case ('table name="studprofile"')
!                        call initialize_student(wrkStudent)
!
!                    case ('column name="idnum"')
!                        wrkStudent%StdNo = adjustl(value)
!                        call upper_case(wrkStudent%StdNo)
!
!                    case ('column name="sname"')
!                        wrkStudent%Name = value
!
!                    case ('column name="fname"')
!                        wrkStudent%Name = trim(wrkStudent%Name)//COMMA//SPACE//value
!
!                    case ('column name="mname"')
!                        wrkStudent%Name = trim(wrkStudent%Name)//SPACE//value
!
!                    case ('column name="course"')
!                        tCurriculum = trim(value)//DASH//wrkStudent%StdNo(:2)
!                        idxCurr = index_to_curriculum(tCurriculum)
!                        if (idxCurr<0) then
!                            idxCurr = -idxCurr
!                        else if (idxCurr==0) then
!                            idxCurr = NumCurricula
!                        end if
!                        wrkStudent%CurriculumIdx = idxCurr
!
!                    case ('/table')
!                        if (len_trim(wrkStudent%StdNo)==0) cycle
!                        if (len_trim(wrkStudent%Password)==0) then
!                            call set_password(wrkStudent%Password, trim(wrkStudent%StdNo))
!                            isDirtySTUDENTS = .true.
!                        end if
!                        call update_student_info(wrkStudent, indexLoc)
!                        if (indexLoc<0) then
!                            numEntries = numEntries + 1
!                            !write(*,*) trim(text_student_curriculum(-indexLoc))
!                        end if
!
!                    case default
!                        ! do nothing
!                end select
!            end do
!            close(unitETC)
!            write(*,*) numEntries, 'students from '//trim(fileName)
!
!
!            ! update students
!            if (NumStudents>0) call xml_write_students(trim(pathToYear), 0)
!
!        end subroutine read_stud_profile
!
!
!        subroutine read_stud_grade()
!
!            !character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
!            character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
!            character (len=MAX_LEN_TEXT_GRADE) :: tGrade
!            character(len=MAX_LEN_STUDENT_CODE) :: prevStdNo, tStdNo
!            character (len=4) :: dirYear
!            integer :: idxYear, idxTerm
!            integer :: cdx, fdx, gdx, idx, sdx, ierr, k, std
!
!            type(TYPE_PRE_ENLISTMENT) :: wrk
!            character (len=MAX_LEN_CLASS_ID) :: tClassId, tCode
!
!
!            ! zero out counters
!            do cdx=MAX_ALL_DUMMY_SUBJECTS,MAX_ALL_SUBJECTS
!                Subject(cdx)%Failrate = 0.0
!                Subject(cdx)%GrandTotal = 0
!            end do
!
!            do idxYear = baseYear,currentYear
!                dirYear = itoa(idxYear)
!
!                do idxTerm = 1,3
!
!                    ! try 'stud_grade.xml'
!                    fileName = trim(dirDATA)//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP// &
!                        'stud_grade.xml'
!                    open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
!                    if (ierr==0) then
!                        write(*,*) 'Retrieving grades from '//trim(fileName)
!
!                        ! initialize dummy classes
!                        NumSections(idxTerm) = 0
!                        call initialize_section (Section(idxTerm,0))
!                        Section(idxTerm,:) = Section(idxTerm,0)
!
!                        ! initialize pre-enlisted subjects of students
!                        call initialize_pre_enlistment(Enlistment(idxTerm,0))
!                        Enlistment(:,:) = Enlistment(idxTerm,0)
!
!                        prevStdNo = SPACE
!                        ! examine the file line by line
!                        do
!                            read(unitRAW, AFORMAT, iostat=eof) line
!                            if (eof<0) exit
!
!                            ! get tag and value if any; exit on any error
!                            call xml_parse_line(line, tag, value, eof)
!                            !if (eof/=0) exit
!
!    !        <table name="stud_rec">
!    !            <column name="idnum">00-0023</column>
!    !            <column name="syear">2013-2014</column>
!    !            <column name="sem">1st</column>
!    !            <column name="school">CAGAYAN STATE UNIVERSITY - GONZAGA                                                                                                                    </column>
!    !            <column name="code">IT 120</column>
!    !            <column name="subject">IT Elective 4                                                                                                                                         </column>
!    !            <column name="grade">76.00</column>
!    !            <column name="units">3.00</column>
!    !            <column name="rem"></column>
!    !            <column name="pcode">G11314S115</column>
!    !        </table>
!                            select case (trim(tag))
!
!                                case ('column name="idnum"')
!                                    tStdNo = adjustl(value)
!                                    std = index_to_student(tStdNo)
!                                    if (std==0 .and. prevStdNo/=SPACE) then
!                                        write(*,*) 'ADD student '//tStdNo
!                                        NumAdditionalStudents = NumAdditionalStudents + 1
!                                        std = NumStudents + NumAdditionalStudents
!                                        Student(std)%StdNo = tStdNo
!                                        prevStdNo = tStdNo
!                                    end if
!
!                                case ('column name="code"')
!                                    tSubject = value
!                                    call upper_case(tSubject)
!                                    cdx = index_to_subject(tSubject)
!                                    if (cdx<=0) then
!
!                                        write(*,*) trim(Student(std)%Name)//' - "'//trim(tSubject)//'" not in catalog'
!                                        NumAdditionalSubjects = NumAdditionalSubjects+1
!                                        cdx = NumSubjects + NumAdditionalSubjects
!
!                                        Subject(cdx)%Name = tSubject
!                                        Subject(cdx)%Title = tSubject
!                                        Subject(cdx)%DeptIdx = NumDepartments
!                                        Subject(cdx)%Units = 3.0
!
!                                        Subject(cdx)%TermOffered = 7
!                                        Subject(cdx)%LectHours = 3.0
!                                        Subject(cdx)%MinLectSize = 50
!                                        Subject(cdx)%MaxLectSize = 50
!                                        Subject(cdx)%LectLoad = 0.0
!                                        Subject(cdx)%LabHours = 0.0
!                                        Subject(cdx)%MinLabSize = 50
!                                        Subject(cdx)%MaxLabSize = 50
!                                        Subject(cdx)%LabLoad = 0.0
!
!                                        k = 1
!                                        Subject(cdx)%lenPreq = k
!                                        Subject(cdx)%Prerequisite(k) = INDEX_TO_NONE
!                                        Subject(cdx)%lenCoreq = k
!                                        Subject(cdx)%Corequisite = INDEX_TO_NONE
!                                        Subject(cdx)%lenConc = k
!                                        Subject(cdx)%Concurrent = INDEX_TO_NONE
!                                        Subject(cdx)%lenConcPreq = k
!                                        Subject(cdx)%ConcPrerequisite= INDEX_TO_NONE
!
!                                        Subject(cdx)%LabFee = 0.0
!                                        Subject(cdx)%Tuition = 0.0
!
!                                    end if
!
!                                case ('column name="grade"')
!
!                                    tGrade = value
!                                    if (tGrade==SPACE) then
!                                        gdx = gdxDRP
!                                    else
!                                        gdx = index(tGrade,DOT)
!                                        if (gdx>0) tGrade(gdx:) = SPACE
!                                        gdx = index_to_grade(tGrade)
!                                        if (gdx==0) then
!                                            write(*,*) trim(Student(std)%Name)//' - "'//trim(tSubject)//'" grade is '//trim(value)// &
!                                                ' using DRP.'
!                                            gdx = gdxDRP
!                                        end if
!
!                                    end if
!
!                                case ('column name="pcode"')
!
!                                    fdx = Enlistment(idxTerm,std)%lenSubject + 1
!                                    call check_array_bound (fdx, MAX_SUBJECTS_PER_TERM, &
!                                        'MAX_SUBJECTS_PER_TERM : '//trim(Student(std)%StdNo)//' - too many subjects taken?')
!                                    Enlistment(idxTerm,std)%Subject(fdx) = cdx
!                                    Enlistment(idxTerm,std)%Grade(fdx) = gdx
!                                    Enlistment(idxTerm,std)%lenSubject = fdx
!
!                                    if (isGrade_numeric_pass(gdx)) then  ! grade counter
!                                        Subject(cdx)%GrandTotal(1,idxTerm) = Subject(cdx)%GrandTotal(1,idxTerm) + 1
!                                    else
!                                        Subject(cdx)%GrandTotal(2,idxTerm) = Subject(cdx)%GrandTotal(2,idxTerm) + 1
!                                    end if
!
!                                    ! create a class?
!                                    tCode = 'Z'//value(8:)
!                                    if (Subject(cdx)%LabHours>0) then ! subject has lab/recitation
!                                        tClassId = trim(Subject(cdx)%Name)//SPACE//trim(tCode)//DASH//'1L'
!                                    else
!                                        tClassId = trim(Subject(cdx)%Name)//SPACE//tCode
!                                    end if
!                                    k = index_to_section(tClassId, NumSections(idxTerm), Section(idxTerm,0:) )
!
!                                    if (k==0) then ! add a class
!
!                                        if (Subject(cdx)%LectHours>0) then ! subject has lecture
!                                            tClassId = trim(Subject(cdx)%Name)//SPACE//tCode
!                                            k = NumSections(idxTerm) + 1
!                                            NumSections(idxTerm) = k
!                                            Section(idxTerm,k) = TYPE_SECTION (tClassId, tCode, SPACE, &
!                                                NumDepartments, cdx, Subject(cdx)%MaxLectSize, Subject(cdx)%MaxLectSize, &
!                                                1, 0, 0, 0, 0, 0)
!                                            !write(*,*) 'Added lecture ', k, tClassId
!                                        end if
!                                        if (Subject(cdx)%LabHours>0) then ! subject has lab/recitation
!                                            tCode = trim(tCode)//DASH//'1L'
!                                            tClassId = trim(Subject(cdx)%Name)//SPACE//tCode
!                                            k = NumSections(idxTerm) + 1
!                                            NumSections(idxTerm) = k
!                                            Section(idxTerm,k) = TYPE_SECTION (tClassId, tCode, SPACE, &
!                                                NumDepartments, cdx, Subject(cdx)%MaxLabSize, Subject(cdx)%MaxLabSize, &
!                                                1, 0, 0, 0, 0, 0)
!                                            !write(*,*) 'Added lab ', k, tClassId
!                                        end if
!                                    end if
!                                    Enlistment(idxTerm,std)%Section(fdx) = k
!
!                                case default
!                                    ! do nothing
!                            end select
!
!                        end do
!                        close(unitRAW)
!                        call xml_write_transcripts(idxYear, idxTerm)
!
!                        ! create "equivalent" ENLISTMENT.XML
!                        fileName = trim(dirDATA)//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP
!                        call xml_write_classes(fileName, NumSections(idxTerm), Section(idxTerm,0:), 0)
!                        call xml_write_pre_enlistment(fileName, 'EQUIVALENT-ENLISTMENT', Enlistment(idxTerm,0:), Section(idxTerm,0:))
!
!                    end if
!
!
!                    ! try 'ENLISTMENT.XML'
!                    fileName = trim(dirDATA)//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP// &
!                        'ENLISTMENT.XML'
!                    open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
!                    if (ierr==0) then
!                        write(*,*) 'Retrieving grades from '//trim(fileName)
!
!                        ! initialize student data
!                        call initialize_student(Student(0))
!                        Student = Student(0)
!                        NumStudents = 0
!
!                        ! initialize pre-enlisted subjects of students
!                        call initialize_pre_enlistment(Enlistment(idxTerm,0))
!                        Enlistment(:,:) = Enlistment(idxTerm,0)
!
!                        prevStdNo = '#-#-#-#-#-#-#-#'
!                        ! examine the file line by line
!                        do
!                            read(unitRAW, AFORMAT, iostat=eof) line
!                            if (eof<0) exit
!
!                            ! get tag and value if any; exit on any error
!                            call xml_parse_line(line, tag, value, eof)
!                            if (eof/=0) exit
!
!                            select case (trim(tag))
!
!                                case ('Student') ! initialize temporary college data
!                                    call initialize_pre_enlistment (wrk)
!                                    NumStudents = NumStudents + 1
!
!                                case ('StdNo')
!                                    tStdNo = adjustl(value)
!                                    Student(NumStudents)%StdNo = tStdNo
!
!                                case ('Name')
!                                    Student(NumStudents)%Name = adjustl(value)
!
!                                case ('Graded')
!
!                                    idx = index(value, COMMA)
!
!                                    ! extract grade
!                                    tGrade = adjustl(value(idx+1:))
!                                    gdx = index_to_grade(tGrade)
!
!                                    ! extract subject
!                                    tClassId = adjustl(value(:idx-1))
!                                    sdx = len_trim(tClassId)
!                                    do while (sdx>1 .and. tClassId(sdx:sdx)/=SPACE)
!                                        sdx = sdx-1
!                                    end do
!                                    tSubject = tClassId(:sdx)
!                                    cdx = index_to_subject(tSubject)
!                                    if (cdx<=0) then ! subject code not found
!                                        call log_comment ('No such class; ignored - '//tClassId)
!                                        cycle
!                                    end if
!
!                                    call check_array_bound (wrk%lenSubject+1, MAX_SUBJECTS_PER_TERM, 'MAX_SUBJECTS_PER_TERM')
!                                    wrk%lenSubject = wrk%lenSubject + 1
!                                    wrk%Subject(wrk%lenSubject) = cdx
!                                    wrk%Grade(wrk%lenSubject) = gdx
!
!                                    if (isGrade_numeric_pass(gdx)) then  ! grade counter
!                                        Subject(cdx)%GrandTotal(1,idxTerm) = Subject(cdx)%GrandTotal(1,idxTerm) + 1
!                                    else
!                                        Subject(cdx)%GrandTotal(2,idxTerm) = Subject(cdx)%GrandTotal(2,idxTerm) + 1
!                                    end if
!
!                                case ('/Student')
!                                    wrk%NPriority = wrk%lenSubject
!                                    Enlistment(idxTerm,NumStudents) = wrk
!
!                                case default
!
!                            end select
!
!                        end do
!
!                        close(unitRAW)
!
!                        call xml_write_transcripts(idxYear, idxTerm)
!
!                    end if
!
!                    if (NumStudents==0) then
!                        write(*,*) 'Grade file not found in '// &
!                            trim(dirDATA)//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP
!                        cycle
!                    end if
!
!
!                end do ! idxTerm = 1,3
!
!            end do ! idxYear = baseYear,currentYear
!
!            !! Subject() was modified; rewrite
!            !call xml_write_subjects(trim(pathToYear)//'SUBJECTS.XML')
!
!        end subroutine read_stud_grade
!
!
!        subroutine import_custom_students_csv()
!
!            type (TYPE_STUDENT) :: wrkStudent
!            character (len=MAX_LEN_CURRICULUM_CODE) :: StdCurriculum
!            character :: ch
!            integer :: idxCURR, i, j, indexLoc, numEntries, ier
!
!            numEntries = 0
!            fileName = trim(pathToYear)//'STUDENTS.CSV'
!            open(unit=unitRAW, file=fileName, status='old', iostat=ier)
!            if (ier/=0) return
!
!            call log_comment ('Retrieving list of students from '//fileName)
!            ! skip first line
!            read (unitRAW, AFORMAT, iostat = eof) line
!            do
!                read (unitRAW, AFORMAT, iostat = eof) line
!                if (eof < 0) exit
!                if (line(1:1) == '#' .or. line(1:3) == '   ') cycle
!                call index_to_delimiters('"', line, ndels, pos)
!                !no,code,name,crscd,year,f1,g1,u1,f2,g2,u2,f3,g3,u3,f4,g4,u4,units
!                !1,"266441","Abad, Aiza  L.","BSBA-HRDM",3,"BL 30","2.25","3","HRDM 50","2.5","3","HRDM 51","1.5","3","ELECT 1 (BSBA)","2.5","3",15.00
!                !1 2      3 4              5 6         7   8     9 1    1 1 1 1       1 1   1 1 1 2       2 2   2 2 2 2              2 2   2 3 3
!                !                                                  0    1 2 3 4       5 6   7 8 9 0       1 2   3 4 5 6              7 8   9 0 1
!                !0,"","","",0,"HRDM 52","2.0","3","","","","","","","","","",0.00
!                !1 23 45 67   8       9 1   1 1 1 11 11 11 22 22 22 22 22 33
!                !                       0   1 2 3 45 67 89 01 23 45 67 89 01
!                if (pos(3)-pos(2)>1) then ! new student
!
!                    call initialize_student(wrkStudent)
!                    wrkStudent%StdNo = adjustl(line(pos(2)+1:pos(3)-1))
!                    j = year_prefix(wrkStudent)
!
!                    StdCurriculum = line(pos(6)+1:pos(7)-1)//DASH//wrkStudent%StdNo(1:j)
!                    if (StdCurriculum(1:3)=='AB-') StdCurriculum = 'AB'//StdCurriculum(4:)
!                    idxCURR = index_to_curriculum(StdCurriculum)
!                    if (idxCURR==0) then
!                        StdCurriculum = 'OTHER'
!                        idxCURR = index_to_curriculum(StdCurriculum)
!                    elseif (idxCURR<=0) then
!                        idxCURR = -idxCURR
!                      !write (*,*) line(:pos(8))//'... using curriculum '// Curriculum(idxCURR)%Code
!                    end if
!                    wrkStudent%Name = SPACE
!                    ! remove punctuation
!                    j = 0
!                    do i=pos(4)+1,pos(5)-1
!                        ch = line(i:i)
!                        if (index(SPECIAL,ch)>0) cycle
!                        j = j+1
!                        if (j>MAX_LEN_PERSON_NAME) cycle
!                        wrkStudent%Name(j:j) = ch
!                    end do
!                    call upper_case(wrkStudent%Name)
!                    wrkStudent%CurriculumIdx = idxCURR
!                    wrkStudent%statusScholastic = max(0, atoi(line(pos(7)+2:pos(8)-2)))
!
!                    call update_student_info(wrkStudent, indexLoc)
!                    if (indexLoc<0) numEntries = numEntries + 1
!
!                end if
!            end do
!            close(unitRAW)
!            call log_comment (itoa(numEntries)//' students added from '//fileName)
!
!            ! update students
!            if (NumStudents>0) call xml_write_students(trim(pathToYear), 0)
!
!
!        end subroutine import_custom_students_csv
!
!
!        subroutine merge_enlistment()
!
!            integer :: errNo
!
!            ! read schedules, blocks, enlistment
!            pathToTerm = trim(dirDATA)//trim(itoa(currentYear))//DIRSEP//trim(txtSemester(curRentTerm))//DIRSEP
!
!            ! read the classes
!            call read_classes(pathToTerm, curRentTerm, NumSections(curRentTerm), Section(curRentTerm,0:), &
!                Offering(curRentTerm,MAX_ALL_DUMMY_SUBJECTS:), errNo)
!
!            ! read the blocks
!            call read_blocks(pathToTerm, curRentTerm, NumBlocks(curRentTerm), Block(curRentTerm,0:), NumSections(curRentTerm), &
!                Section(curRentTerm,0:), errNo)
!
!            ! read enlistment files, if any
!            call read_enlistment(pathToTerm, curRentTerm, 'ENLISTMENT', 0, 6, &
!                NumSections(curRentTerm), Section(curRentTerm,0:), Enlistment(curRentTerm,0:), NumEnlistment(curRentTerm), errNo)
!
!            call read_special_enlistment(pathToTerm, currentTerm, 'ENLISTMENT-MERGE', &
!                NumSections(currentTerm), Section(currentTerm,0:), Enlistment(currentTerm,0:), NumEnlistment(currentTerm), errNo)
!
!            call xml_write_pre_enlistment(pathToTerm, 'ENLISTMENT', Enlistment(currentTerm,0:), Section(currentTerm,0:), 0)
!
!        end subroutine merge_enlistment
!
!
!        subroutine read_special_enlistment(path, iTerm, basename, NumSections, Section, eList, numEntries, errNo)
!
!            character(len=*), intent(in) :: path, basename
!            integer, intent (in) :: iTerm, NumSections
!            type (TYPE_SECTION), intent(in) :: Section(0:)
!            type (TYPE_PRE_ENLISTMENT), intent(in out) :: eList(0:)
!            integer, intent (out) :: numEntries, errNo
!
!
!            type(TYPE_PRE_ENLISTMENT) :: wrk
!            character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
!            character (len=MAX_LEN_CLASS_ID) :: tClass
!            character (len=MAX_LEN_STUDENT_CODE) :: tStdNo
!            character (len = MAX_LEN_TEXT_GRADE) :: tGrade
!            integer :: cdx, idx, sdx, std, gdx, pos, i, j, nGraded
!
!            numEntries = 0
!            ! open file, return on any error
!            fileName = trim(path)//trim(basename)//dotXML
!            call xml_read_file(unitETC, ROOT_ENLISTMENT//trim(txtSemester(iTerm)), fileName, errNo)
!            if (errNo/=0) return
!
!            ! examine the file line by line
!            do
!                read(unitETC, AFORMAT, iostat=eof) line
!                if (eof<0) exit
!
!                ! get tag and value if any; exit on any error
!                call xml_parse_line(line, tag, value, eof)
!                if (eof/=0) exit
!
!                select case (trim(tag))
!
!                    case ('Student') ! initialize temporary college data
!                        call initialize_pre_enlistment (wrk)
!                        nGraded = 0
!
!                    case ('StdNo')
!                        tStdNo = adjustl(value)
!                        std = index_to_student(tStdNo)
!
!                    case ('UnitsEarned')
!                        wrk%UnitsEarned = atof(value)
!
!                    case ('StdYear')
!                        wrk%levelYear = atoi(value)
!
!                    case ('StdClassification')
!                        wrk%levelClassification = atoi(value)
!
!                    case ('AllowedLoad')
!                        wrk%AllowedLoad = atof(value)
!
!                    case ('StdPriority')
!                        wrk%levelPriority = atoi(value)
!
!                    case ('NPriority')
!                        wrk%NPriority = atoi(value)
!
!                    case ('NAlternates')
!                        wrk%NAlternates = atoi(value)
!
!                    case ('NCurrent')
!                        wrk%NCurrent = atoi(value)
!
!                    case ('Status')
!                        wrk%statusEnlistment = atoi(value)
!
!                    case ('codeNSTP')
!                        wrk%codeNSTP = atoi(value)
!
!
!                    case ('Predicted')
!                        idx = index(value, COMMA)
!                        tSubject = adjustl(value(:idx-1))
!                        cdx = index_to_subject(tSubject)
!                        if (cdx<=0) then ! subject code not found
!                            call log_comment ('No such subject; ignored - '//tSubject)
!                            cycle
!                        end if
!                        ! check if already among previous entries
!                        pos = 0
!                        do i=1,wrk%lenSubject
!                            if (wrk%Subject(i)==cdx) pos = i
!                        end do
!                        if (pos==0) then
!                            pos = wrk%lenSubject + 1
!                            call check_array_bound (pos, MAX_SUBJECTS_PER_TERM, 'MAX_SUBJECTS_PER_TERM')
!                            wrk%lenSubject = pos
!                        else
!                            call log_comment('Using duplicate predicted entry:  '//trim(tStdNo)//DASH//trim(line))
!                        end if
!                        ! add as predicted subject
!                        wrk%Subject(pos) = cdx
!                        wrk%Contrib(pos) = atof( trim( value(idx+1:) ) )
!
!                    case ('Allowed')
!                        tSubject = adjustl(value)
!                        cdx = index_to_subject(tSubject)
!                        if (cdx<=0) then ! subject code not found
!                            call log_comment ('No such subject; ignored - '//tSubject)
!                            cycle
!                        end if
!                        ! check if already among previous entries
!                        pos = 0
!                        do i=1,wrk%lenSubject
!                            if (wrk%Subject(i)==cdx) pos = i
!                        end do
!                        if (pos==0) then
!                            pos = wrk%lenSubject + 1
!                            call check_array_bound (pos, MAX_SUBJECTS_PER_TERM, 'MAX_SUBJECTS_PER_TERM')
!                            wrk%lenSubject = pos
!                            ! add as allowed subject
!                            wrk%Subject(pos) = cdx
!                            wrk%Contrib(pos) = 0.0
!                        else
!                            call log_comment('Discarding duplicate allowed entry:  '//trim(tStdNo)//DASH//trim(line))
!                        end if
!
!                    case ('Graded')
!                        idx = index(value, COMMA)
!                        tClass = adjustl(value(:idx-1))
!                        tGrade = adjustl(value(idx+1:))
!                        sdx = index_to_section(tClass, NumSections, Section)
!                        gdx = index_to_grade(tGrade)
!                        ! check if already among previous entries
!                        pos = 0
!                        do i=1,wrk%lenSubject
!                            if (wrk%Subject(i)==Section(sdx)%SubjectIdx) pos = i
!                        end do
!                        if (pos==0) then
!                            pos = wrk%lenSubject + 1
!                            call check_array_bound (pos, MAX_SUBJECTS_PER_TERM, 'MAX_SUBJECTS_PER_TERM')
!                            wrk%lenSubject = pos
!                        else
!                            call log_comment('Using duplicate graded entry:  '//trim(tStdNo)//DASH//trim(line))
!                        end if
!                        wrk%Section(pos) = sdx
!                        wrk%Subject(pos) = Section(sdx)%SubjectIdx
!                        wrk%Grade(pos) = gdx
!                        wrk%Contrib(pos) = 1.0
!                        nGraded = nGraded+1
!
!                    case ('/Student')
!                        if (wrk%NPriority+wrk%NAlternates+wrk%NCurrent /= wrk%lenSubject) then
!                            wrk%NPriority = wrk%lenSubject
!                            wrk%NAlternates = 0
!                            wrk%NCurrent = 0
!                        end if
!                        if (std/=0) then
!
!                            !eList(std) = wrk
!                            !numEntries = numEntries+1
!
!                            ! check if already in eList(std)
!                            do i=1,wrk%lenSubject
!                                if (wrk%Grade(i)==0) cycle ! not graded
!                                pos = 0
!                                do j=1,eList(std)%lenSubject
!                                    if (wrk%Subject(i)/=eList(std)%Subject(j)) cycle ! not the same subject
!                                    pos = j
!                                    exit
!                                end do
!                                if (pos==0) then ! add
!                                    do j=eList(std)%lenSubject,1,-1
!                                        eList(std)%Section(j+1) = eList(std)%Section(j)
!                                        eList(std)%Subject(j+1) = eList(std)%Subject(j)
!                                        eList(std)%Grade(j+1) = eList(std)%Grade(j)
!                                        eList(std)%Contrib(j+1) = eList(std)%Contrib(j)
!                                    end do
!                                    eList(std)%Section(1) = wrk%Section(i)
!                                    eList(std)%Subject(1) = wrk%Subject(i)
!                                    eList(std)%Grade(1) = wrk%Grade(i)
!                                    eList(std)%Contrib(1) = wrk%Contrib(i)
!                                    eList(std)%NPriority = eList(std)%NPriority+1
!                                    eList(std)%lenSubject = eList(std)%lenSubject+1
!                                    numEntries = numEntries+1
!                                    call log_comment(trim(Student(std)%StdNo)//SPACE//trim(Student(std)%Name)// &
!                                        ' - added enlistment record & grade in '//Section(wrk%Section(i))%ClassId)
!                                elseif (eList(std)%Grade(pos)/=wrk%Grade(i)) then ! not the same grade
!                                    ! use whichever is the valid grade
!                                    if (wrk%Grade(i)/=0) then
!                                        eList(std)%Section(pos) = wrk%Section(i)
!                                        eList(std)%Grade(pos) = wrk%Grade(i)
!                                        call log_comment(trim(Student(std)%StdNo)//SPACE//trim(Student(std)%Name)// &
!                                            ' - added grade in '//Section(wrk%Section(i))%ClassId)
!                                    end if
!                                !else
!                                !    call log_comment(trim(Student(std)%StdNo)//SPACE//trim(Student(std)%Name))// &
!                                !        ' - matched '//Section(wrk%Section(i))%ClassId)
!                                end if
!                            end do
!
!                        else
!
!                            call log_comment ('No such student; ignored - '//tStdNo)
!
!                        end if
!
!                    case default
!                        if ( trim(tag)==FSLASH//ROOT_ENLISTMENT//trim(txtSemester(iTerm)) ) exit
!
!                end select
!
!            end do
!
!            close(unitETC)
!            call log_comment (itoa(numEntries)//' pre-enlistment entries in '//fileName)
!
!        end subroutine read_special_enlistment
!
!



    subroutine check_add_subject(tSubject, cdx)

        character (len=MAX_LEN_SUBJECT_CODE), intent(in) :: tSubject
        integer, intent(out) :: cdx

        cdx = index_to_subject(tSubject)
        if (cdx>0) return

        call log_comment ('Adding subject "'//trim(tSubject)//'" to catalog', out6=.true.)

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

        Subject(cdx)%lenPreq = 1
        Subject(cdx)%Prerequisite(1) = INDEX_TO_NONE
        Subject(cdx)%lenCoreq = 1
        Subject(cdx)%Corequisite = INDEX_TO_NONE
        Subject(cdx)%lenConc = 1
        Subject(cdx)%Concurrent = INDEX_TO_NONE
        Subject(cdx)%lenConcPreq = 1
        Subject(cdx)%ConcPrerequisite= INDEX_TO_NONE

        Subject(cdx)%LabFee = 0.0
        Subject(cdx)%Tuition = 0.0

    end subroutine check_add_subject


    subroutine check_add_class(cdx, idxTerm, sdx)

        integer, intent(in) :: cdx, idxTerm
        integer, intent(out) :: sdx

        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_CLASS_ID) :: tClassId, tCode

        tSubject = Subject(cdx)%Name

        ! class code
        if (cdx>99) then
            tCode = 'Z'//itoa(cdx)
        else if (cdx>9) then
            tCode = 'Z0'//itoa(cdx)
        else
            tCode = 'Z00'//itoa(cdx)
        end if

        if (Subject(cdx)%LabHours>0) then ! subject has lab/recitation
            tClassId = trim(tSubject)//SPACE//trim(tCode)//DASH//'1L'
        else
            tClassId = trim(tSubject)//SPACE//tCode
        end if
        sdx = index_to_section(tClassId, idxTerm )
        if (sdx/=0) return ! already exists

        if (Subject(cdx)%LectHours>0) then ! subject has lecture
            tClassId = trim(tSubject)//SPACE//tCode
            sdx = NumSections(idxTerm) + 1
            NumSections(idxTerm) = sdx
            Section(idxTerm,sdx) = TYPE_SECTION (tClassId, tCode, SPACE, &
                NumDepartments, cdx, Subject(cdx)%MaxLectSize, Subject(cdx)%MaxLectSize, &
                1, 0, 0, 0, 0, 0)
        end if
        if (Subject(cdx)%LabHours>0) then ! subject has lab/recitation
            tCode = trim(tCode)//DASH//'1L'
            tClassId = trim(tSubject)//SPACE//tCode
            sdx = NumSections(idxTerm) + 1
            NumSections(idxTerm) = sdx
            Section(idxTerm,sdx) = TYPE_SECTION (tClassId, tCode, SPACE, &
                NumDepartments, cdx, Subject(cdx)%MaxLabSize, Subject(cdx)%MaxLabSize, &
                1, 0, 0, 0, 0, 0)
        end if

    end subroutine check_add_class


    subroutine generate_checklists()

        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_TEXT_GRADE) :: tGrade
        character(len=MAX_LEN_STUDENT_CODE) :: prevStdNo, tStdNo
        character (len=4) :: dirYear
        !character (len=1) :: ch
        integer :: idxYear, idxTerm, idxStudent, iTmp, jTmp
        integer :: cdx, fdx, gdx, idx, sdx, ierr, nGrades

        !type(TYPE_PRE_ENLISTMENT) :: wrk
        !character (len=MAX_LEN_CLASS_ID) :: tClassId
        character (len=MAX_LEN_FILE_PATH) :: fileName, masterList
        logical :: pathExists

        ! read basic data for university
        call year_data_read(pathToYear)

        ! zero out counters
        do cdx=MAX_ALL_DUMMY_SUBJECTS,MAX_ALL_SUBJECTS
            Subject(cdx)%Failrate = 0.0
            Subject(cdx)%GrandTotal = 0
        end do

        ! masterlist of students
        masterList = trim(dirDATA)//'info'//DIRSEP//'students-from-gradefiles'
        open(unit=unitHTML+1, file=trim(masterList))

        do idxYear = baseYear,currentYear-1
            dirYear = itoa(idxYear)

            call read_students_from_exports(idxYear)
            if (NumStudents==0) cycle

            ! create/clean directories
            call collect_prefix_years()
            iTmp = 1
            do jTmp=2,len_trim(StdNoPrefix)
                if (StdNoPrefix(jTmp:jTmp)/=':') cycle
                if (StdNoPrefix(iTmp+1:jTmp-1)==SPACE) cycle
                call make_directory( trim(dirDATA)//dirYear//DIRSEP//'GRADES'//DIRSEP//StdNoPrefix(iTmp+1:jTmp-1), .true. )
                iTmp = jTmp
            end do

            ! reset classes
            NumSections = 0
            call initialize_section (Section(1,0))
            Section(:,:) = Section(1,0)

            ! reset blocks
            NumBlocks = 0
            call initialize_block(Block(1,0))
            Block(:,:) = Block(1,0)

            ! reset enlistment
            NumEnlistment = 0

            do idxTerm = 1,3

                call initialize_past_future_years_terms (idxYear, idxTerm)
                pathToTerm = trim(pathToYear)//trim(txtSemester(idxTerm))//DIRSEP

                nGrades = 0

                ! try 'FINALGRADE.CSV'
                fileName = trim(pathToTerm)//'FINALGRADE.CSV'
                open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
                if (ierr==0) then

                    call log_comment('Retrieving grades from '//fileName, out6=.true.)

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
                            tStdNo = adjustl(line(pos(2)+1:pos(3)-1))
                            idxStudent = index_to_student(tStdNo)
                            Student(idxStudent)%Enlistment(idxTerm)%statusAdvising = ADVISING_IRREGULAR
                            Student(idxStudent)%Enlistment(idxTerm)%statusEnlistment = ENLISTMENT_PAID
                        end if

                        do idx=8,31,6
                            if (pos(idx+1)-pos(idx)==1) cycle ! empty subject

                            ! add subject if it doesn't exist
                            tSubject = adjustl(line(pos(idx)+1:pos(idx+1)-1))
                            call upper_case(tSubject)
                            call check_add_subject(tSubject, cdx)

                            tGrade  = adjustl(line(pos(idx+2)+1:pos(idx+3)-1))
                            if (tGrade==SPACE) then
                                gdx = gdxDRP
                            else
                                gdx = index_to_grade(tGrade)
                                if (gdx==0) gdx = gdxDRP
                            end if

                            fdx = Student(idxStudent)%Enlistment(idxTerm)%lenSubject + 1
                            call check_array_bound (fdx, MAX_SUBJECTS_PER_TERM, &
                                'MAX_SUBJECTS_PER_TERM : '//trim(Student(idxStudent)%StdNo)//' - too many subjects taken?')
                            Student(idxStudent)%Enlistment(idxTerm)%Subject(fdx) = cdx
                            Student(idxStudent)%Enlistment(idxTerm)%Grade(fdx) = gdx
                            Student(idxStudent)%Enlistment(idxTerm)%lenSubject = fdx
                            Student(idxStudent)%Enlistment(idxTerm)%NPriority = fdx
                            nGrades = nGrades + 1

                            ! add class if it doesn't exist
                            call check_add_class(cdx, idxTerm, sdx)
                            Student(idxStudent)%Enlistment(idxTerm)%Section(fdx) = sdx

                            if (isGrade_numeric_pass(gdx)) then  ! grade counter
                                Subject(cdx)%GrandTotal(1,idxTerm) = Subject(cdx)%GrandTotal(1,idxTerm) + 1
                            else
                                Subject(cdx)%GrandTotal(2,idxTerm) = Subject(cdx)%GrandTotal(2,idxTerm) + 1
                            end if

                        end do

                    end do
                    close(unitRAW)

                    call log_comment(itoa(nGrades)//' grade entries in '//fileName, out6=.true.)

                    ! create "equivalent" CLASSES
                    if (NumSections(idxTerm)>0) then
                        do sdx=1,NumSections(idxTerm)
                            call class_details_write(unitXML, idxTerm, &
                                trim(dirDATA)//dirYEAR//DIRSEP//'CLASSES'//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP, sdx)
                        end do
                        call class_details_write(unitXML, idxTerm, trim(dirDATA)//dirYEAR//DIRSEP// &
                            'CLASSES'//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP//'index', 1, NumSections(idxTerm))
                    end if

                end if

                ! try 'STUDENT-SUBJECT-GRADE.CSV'
                fileName = trim(pathToTerm)//'STUDENT-SUBJECT-GRADE.CSV'
                open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
                if (ierr==0) then
                    call log_comment('Retrieving grades from '//fileName, out6=.true.)

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
                        call upper_case(tStdNo)
                        if (tStdNo/=prevStdNo) then ! new student
                            idxStudent = index_to_student(tStdNo)
                            prevStdNo = tStdNo
                            Student(idxStudent)%Enlistment(idxTerm)%statusAdvising = ADVISING_IRREGULAR
                            Student(idxStudent)%Enlistment(idxTerm)%statusEnlistment = ENLISTMENT_PAID

                        end if

                        ! add subject if doesn't exist
                        tSubject = adjustl(line(pos(2)+1:pos(3)-1))
                        call upper_case(tSubject)
                        call check_add_subject(tSubject, cdx)

                        ! grade
                        tGrade  = adjustl(line(pos(3)+1:pos(4)-1))
                        if (tGrade==SPACE) then
                            gdx = gdxDRP
                        elseif (tGrade=='0') then
                            gdx = gdxDRP
                        else
                            gdx = index_to_grade(tGrade)
                            if (gdx==0) gdx = gdxDRP
                        end if

                        fdx = Student(idxStudent)%Enlistment(idxTerm)%lenSubject + 1
                        call check_array_bound (fdx, MAX_SUBJECTS_PER_TERM, &
                            'MAX_SUBJECTS_PER_TERM : '//trim(Student(idxStudent)%StdNo)//' - too many subjects taken?')
                        Student(idxStudent)%Enlistment(idxTerm)%Subject(fdx) = cdx
                        Student(idxStudent)%Enlistment(idxTerm)%Grade(fdx) = gdx
                        Student(idxStudent)%Enlistment(idxTerm)%lenSubject = fdx
                        Student(idxStudent)%Enlistment(idxTerm)%NPriority = fdx
                        nGrades = nGrades + 1

                        ! add class if it doesn't exist
                        call check_add_class(cdx, idxTerm, sdx)
                        Student(idxStudent)%Enlistment(idxTerm)%Section(fdx) = sdx

                        if (isGrade_numeric_pass(gdx)) then  ! grade counter
                            Subject(cdx)%GrandTotal(1,idxTerm) = Subject(cdx)%GrandTotal(1,idxTerm) + 1
                        else
                            Subject(cdx)%GrandTotal(2,idxTerm) = Subject(cdx)%GrandTotal(2,idxTerm) + 1
                        end if

                    end do
                    close(unitRAW)
                    call log_comment(itoa(nGrades)//' grade entries in '//fileName, out6=.true.)

                end if

                ! try 'STDNO-NAME-SUBJ-TITLE-GRADE.CSV'
                fileName = trim(pathToTerm)//'STDNO-NAME-SUBJ-TITLE-GRADE.CSV'
                open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
                if (ierr==0) then
                    call log_comment('Retrieving grades from '//fileName, out6=.true.)

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
                        call upper_case(tStdNo)
                        if (tStdNo/=prevStdNo) then ! new student
                            idxStudent = index_to_student(tStdNo)
                            Student(idxStudent)%Enlistment(idxTerm)%statusAdvising = ADVISING_IRREGULAR
                            Student(idxStudent)%Enlistment(idxTerm)%statusEnlistment = ENLISTMENT_PAID
                            prevStdNo = tStdNo
                        end if

                        ! add subject if doesn't exist
                        tSubject = adjustl(line(pos(6)+1:pos(7)-1))
                        call upper_case(tSubject)
                        call check_add_subject(tSubject, cdx)

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

                        fdx = Student(idxStudent)%Enlistment(idxTerm)%lenSubject + 1
                        call check_array_bound (fdx, MAX_SUBJECTS_PER_TERM, &
                            'MAX_SUBJECTS_PER_TERM : '//trim(Student(idxStudent)%StdNo)//' - too many subjects taken?')
                        Student(idxStudent)%Enlistment(idxTerm)%Subject(fdx) = cdx
                        Student(idxStudent)%Enlistment(idxTerm)%Grade(fdx) = gdx
                        Student(idxStudent)%Enlistment(idxTerm)%lenSubject = fdx
                        Student(idxStudent)%Enlistment(idxTerm)%NPriority = fdx
                        nGrades = nGrades + 1

                        ! add class if it doesn't exist
                        call check_add_class(cdx, idxTerm, sdx)
                        Student(idxStudent)%Enlistment(idxTerm)%Section(fdx) = sdx

                        if (isGrade_numeric_pass(gdx)) then  ! grade counter
                            Subject(cdx)%GrandTotal(1,idxTerm) = Subject(cdx)%GrandTotal(1,idxTerm) + 1
                        else
                            Subject(cdx)%GrandTotal(2,idxTerm) = Subject(cdx)%GrandTotal(2,idxTerm) + 1
                        end if


                    end do
                    close(unitRAW)
                    call log_comment(itoa(nGrades)//' grade entries in '//fileName, out6=.true.)

                end if

!                ! try 'ENLISTMENT.XML', BACKUP.XML
!                fileName = trim(pathToTerm)//'ENLISTMENT.XML'
!                open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
!                if (ierr /= 0) then ! try BACKUP.XML
!                    fileName = trim(dirDATA)//dirYEAR//DIRSEP//'BACKUP.XML'
!                    open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
!                    if (ierr==0) then ! advance to <ENLISTMENT_term>
!                        do
!                            read(unitRAW, AFORMAT, iostat=eof) line
!                            if (eof<0) then
!                                ierr = 1
!                                exit
!                            end if
!                            ! get tag and value if any; exit on any error
!                            call xml_parse_line(line, tag, value, eof)
!                            if (eof/=0) cycle
!                            if (trim(tag)==ROOT_ENLISTMENT//trim(txtSemester(idxTerm)) ) exit
!
!                        end do
!                    end if
!                end if
!                if (ierr==0) then
!                    call log_comment('Retrieving '//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))// &
!                        ' grades from '//fileName, out6=.true.)
!
!                    ! examine the file line by line
!                    do
!                        read(unitRAW, AFORMAT, iostat=eof) line
!                        if (eof<0) exit
!
!                        ! get tag and value if any; exit on any error
!                        call xml_parse_line(line, tag, value, eof)
!                        if (eof/=0) exit
!
!                        select case (trim(tag))
!
!                            case ('Student') ! initialize temporary college data
!                                call initialize_pre_enlistment (wrk)
!
!                            case ('StdNo')
!                                tStdNo = adjustl(value)
!                                call upper_case(tStdNo)
!                                idxStudent = index_to_student(tStdNo)
!
!                            case ('Graded')
!                                wrk%statusAdvising = ADVISING_IRREGULAR
!                                wrk%statusEnlistment = ENLISTMENT_PAID
!
!                                idx = index(value, COMMA)
!
!                                ! extract grade
!                                tGrade = adjustl(value(idx+1:))
!                                gdx = index_to_grade(tGrade)
!
!                                ! extract subject
!                                tClassId = adjustl(value(:idx-1))
!                                sdx = len_trim(tClassId)
!                                do while (sdx>1 .and. tClassId(sdx:sdx)/=SPACE)
!                                    sdx = sdx-1
!                                end do
!                                tSubject = tClassId(:sdx)
!                                cdx = index_to_subject(tSubject)
!                                if (cdx<=0) then ! subject code not found
!                                    call log_comment ('No such class; ignored - '//tClassId)
!                                    cycle
!                                end if
!
!                                call check_array_bound (wrk%lenSubject+1, MAX_SUBJECTS_PER_TERM, 'MAX_SUBJECTS_PER_TERM')
!                                wrk%lenSubject = wrk%lenSubject + 1
!                                wrk%Subject(wrk%lenSubject) = cdx
!                                wrk%Grade(wrk%lenSubject) = gdx
!                                nGrades = nGrades + 1
!
!                                if (isGrade_numeric_pass(gdx)) then  ! grade counter
!                                    Subject(cdx)%GrandTotal(1,idxTerm) = Subject(cdx)%GrandTotal(1,idxTerm) + 1
!                                else
!                                    Subject(cdx)%GrandTotal(2,idxTerm) = Subject(cdx)%GrandTotal(2,idxTerm) + 1
!                                end if
!
!                            case ('/Student')
!                                wrk%NPriority = wrk%lenSubject
!                                Student(idxStudent)%Enlistment(idxTerm) = wrk
!
!                            case default
!                                if (trim(tag)==FSLASH//ROOT_ENLISTMENT//trim(txtSemester(idxTerm)) ) exit
!
!                        end select
!
!                    end do
!
!                    close(unitRAW)
!                    call log_comment(itoa(nGrades)//' grade entries in '//fileName, out6=.true.)
!
!                end if

                if (nGrades==0) then
                    call log_comment('Grade file not found in '//pathToTerm, out6=.true.)
                end if

            end do ! idxTerm = 1,3

            call finalgrades_write(idxYear)

            ! masterlist of students
            do idxStudent=1,numStudents
                !if (len_trim(Student(idxStudent)%StdNo)==0 .or. len_trim(Student(idxStudent)%Name)==0) cycle
                !write(unitHTML+1,AFORMAT) trim(Student(idxStudent)%StdNo)//COMMA//trim(Student(idxStudent)%Name)
                iTmp = len_trim(Student(idxStudent)%Name)
                if (len_trim(Student(idxStudent)%StdNo)==0 .or. iTmp==0) cycle
                jTmp = index(Student(idxStudent)%Name, COMMA)
                cdx = jTmp
                do idx=jTmp+1,iTmp
                    if (index(SPECIAL//COMMA//PRIME, Student(idxStudent)%Name(idx:idx))>0) cycle
                    cdx = cdx + 1
                    Student(idxStudent)%Name(cdx:cdx) = Student(idxStudent)%Name(idx:idx)
                end do
                Student(idxStudent)%Name(cdx+1:) = SPACE
                write(unitHTML+1,AFORMAT) trim(Student(idxStudent)%StdNo)//COMMA//trim(Student(idxStudent)%Name)
            end do

        end do ! idxYear = baseYear,currentYear-1

        do idxYear = baseYear,currentYear

            ! index-STUDENTS exists?
            fileName = trim(dirDATA)//trim(itoa(idxYear))//DIRSEP//'STUDENTS'//DIRSEP//'index'
            inquire(file=trim(fileName), exist=pathExists)
            if (.not. pathExists) cycle

            currentYear = idxYear
            dirYear = itoa(idxYear)
            call log_comment(SPACE, '==== School year starts '//dirYear, 'Reading data for current year', out6=.true.)

            ! compute relative years and terms (before and after current)
            idxTerm = 2
            call initialize_past_future_years_terms (idxYear, idxTerm)

            call year_data_initialize () ! initialize basic data

            ! unser set server flag temporarily
            isServer = .false.

            call year_data_read(pathToYear) ! read data for current year

            ! create directories
            call collect_prefix_years()
            iTmp = 1
            do jTmp=2,len_trim(StdNoPrefix)
                if (StdNoPrefix(jTmp:jTmp)/=':') cycle
                if (StdNoPrefix(iTmp+1:jTmp-1)==SPACE) cycle
                call make_directory( trim(dirDATA)//dirYear//DIRSEP//'GRADES'//DIRSEP//StdNoPrefix(iTmp+1:jTmp-1) )
                iTmp = jTmp
            end do

            call finalgrades_write(idxYear)

            ! masterlist of students
            do idxStudent=1,numStudents
                iTmp = len_trim(Student(idxStudent)%Name)
                if (len_trim(Student(idxStudent)%StdNo)==0 .or. iTmp==0) cycle
                jTmp = index(Student(idxStudent)%Name, COMMA)
                cdx = jTmp
                do idx=jTmp+1,iTmp
                    if (index(SPECIAL//COMMA//PRIME, Student(idxStudent)%Name(idx:idx))>0) cycle
                    cdx = cdx + 1
                    Student(idxStudent)%Name(cdx:cdx) = Student(idxStudent)%Name(idx:idx)
                end do
                Student(idxStudent)%Name(cdx+1:) = SPACE
                write(unitHTML+1,AFORMAT) trim(Student(idxStudent)%StdNo)//COMMA//trim(Student(idxStudent)%Name)
            end do

        end do

        ! masterlist of students
        close(unitHTML+1)
        call system('sort '//trim(masterList)//' | uniq | sort -t, -k 2 > '//trim(dirDATA)//'info'//DIRSEP//'masterlist-STUDENTS')

        call terminate(-1, trim(fileEXE)//SPACE//ACTION//' completed normally.')

    end subroutine generate_checklists


    subroutine read_students_from_exports(idxYear)

        integer, intent(in) :: idxYear

        character(len=MAX_LEN_STUDENT_CODE) :: prevStdNo, tStdNo
        character (len=4) :: dirYear
        character (len=1) :: ch
        integer :: idxTerm, cdx, idx, ierr, i, j, numEntries, numEntriesThisTerm, indexLoc
        !logical :: fileExists

        character (len=MAX_LEN_FILE_PATH) :: fileName
        type (TYPE_STUDENT) :: wrkStudent

        ! initialize student data
        call initialize_student(wrkStudent)
        Student = wrkStudent
        NumStudents = 0

        dirYear = itoa(idxYear)
        call log_comment(SPACE, '==== School year starts '//dirYear, out6=.true.)

        do idxTerm = 1,3

            numEntriesThisTerm = 0
            pathToTerm = trim(dirDATA)//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP

            ! try 'FINALGRADE.CSV'
            fileName = trim(pathToTerm)//'FINALGRADE.CSV'
            open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
            if (ierr==0) then

                !call log_comment('Retrieving students from '//fileName, out6=.true.)
                numEntries = 0

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

                        call initialize_student(wrkStudent)
                        wrkStudent%StdNo = adjustl(line(pos(2)+1:pos(3)-1))
                        call upper_case(wrkStudent%StdNo)
                        wrkStudent%CurriculumIdx = NumCurricula
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

                        if (.not. isAlphaNumeric(wrkStudent%StdNo) ) cycle
                        call update_student_info(wrkStudent, indexLoc)
                        if (indexLoc<0) numEntries = numEntries + 1
                    end if

                end do
                close(unitRAW)
                call log_comment(itoa(numEntries)//FSLASH//itoa(NumStudents)//' students from '//fileName, out6=.true.)
                numEntriesThisTerm = numEntriesThisTerm + numEntries
            end if

            ! try 'STUDENT-SUBJECT-GRADE.CSV'
            fileName = trim(pathToTerm)//'STUDENT-SUBJECT-GRADE.CSV'
            open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
            if (ierr==0) then

                !call log_comment('Retrieving students from '//fileName, out6=.true.)
                numEntries = 0

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
                    call upper_case(tStdNo)
                    if (tStdNo/=prevStdNo) then ! new student
                        call initialize_student(wrkStudent)
                        wrkStudent%StdNo = tStdNo
                        wrkStudent%CurriculumIdx = NumCurricula
                        prevStdNo = tStdNo
                        wrkStudent%Name = SPACE
                        idx = len_trim(line)-1
                        cdx = 0
                        do i=idx,1,-1
                            if (line(i:i)=='"') then
                                cdx = i
                                exit
                            end if
                        end do
                        if (cdx>0) wrkStudent%Name = line(cdx+1:idx)
                        call upper_case(wrkStudent%Name)

                        if (.not. isAlphaNumeric(wrkStudent%StdNo) ) cycle
                        call update_student_info(wrkStudent, indexLoc)
                        if (indexLoc<0) numEntries = numEntries + 1
                    end if

                end do
                close(unitRAW)
                call log_comment(itoa(numEntries)//FSLASH//itoa(NumStudents)//' students from '//fileName, out6=.true.)
                numEntriesThisTerm = numEntriesThisTerm + numEntries

            end if

            ! try 'STDNO-NAME-SUBJ-TITLE-GRADE.CSV'
            fileName = trim(pathToTerm)//'STDNO-NAME-SUBJ-TITLE-GRADE.CSV'
            open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
            if (ierr==0) then
                !call log_comment('Retrieving students from '//fileName, out6=.true.)
                numEntries = 0

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
                    call upper_case(tStdNo)
                    if (tStdNo/=prevStdNo) then ! new student
                        call initialize_student(wrkStudent)
                        wrkStudent%StdNo = tStdNo
                        prevStdNo = tStdNo
                        wrkStudent%Name = SPACE
                        wrkStudent%CurriculumIdx = NumCurricula
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

                        if (.not. isAlphaNumeric(wrkStudent%StdNo) ) cycle
                        call update_student_info(wrkStudent, indexLoc)
                        if (indexLoc<0) numEntries = numEntries + 1
                    end if

                end do
                close(unitRAW)
                call log_comment(itoa(numEntries)//FSLASH//itoa(NumStudents)//' students from '//fileName, out6=.true.)
                numEntriesThisTerm = numEntriesThisTerm + numEntries

            end if

            ! try 'ENLISTMENT.XML'
            fileName = trim(pathToTerm)//'ENLISTMENT.XML'
            open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
            if (ierr /= 0) then ! try BACKUP.XML
                fileName = trim(dirDATA)//dirYEAR//DIRSEP//'BACKUP.XML'
                open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
                if (ierr==0) then ! advance to <ENLISTMENT_term>
                    do
                        read(unitRAW, AFORMAT, iostat=eof) line
                        if (eof<0) then
                            ierr = 1
                            exit
                        end if
                        ! get tag and value if any; exit on any error
                        call xml_parse_line(line, tag, value, eof)
                        if (eof/=0) then
                            cycle
                        end if
                        if (trim(tag) == ROOT_ENLISTMENT//trim(txtSemester(idxTerm)) ) then
                            exit
                        end if

                    end do
                end if
            end if
            if (ierr==0) then
                !call log_comment('Retrieving students from '//fileName, out6=.true.)
                numEntries = 0

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
                            call initialize_student(wrkStudent)

                        case ('StdNo')
                            tStdNo = adjustl(value)
                            call upper_case(tStdNo)
                            wrkStudent%StdNo = tStdNo
                            wrkStudent%CurriculumIdx = NumCurricula

                        case ('Name')
                            wrkStudent%Name = adjustl(value)
                            call upper_case(wrkStudent%Name)

                        case ('/Student')
                            if (.not. isAlphaNumeric(wrkStudent%StdNo) ) cycle
                            call update_student_info(wrkStudent, indexLoc)
                            if (indexLoc<0) numEntries = numEntries + 1

                        case default
                            if (trim(tag) == FSLASH//ROOT_ENLISTMENT//trim(txtSemester(idxTerm)) ) then
                                exit
                            end if

                    end select

                end do

                close(unitRAW)
                call log_comment(itoa(numEntries)//FSLASH//itoa(NumStudents)//' students from '//fileName, out6=.true.)
                numEntriesThisTerm = numEntriesThisTerm + numEntries

            end if

        end do ! idxTerm = 1,3

    end subroutine read_students_from_exports


    subroutine generate_payments()

        integer, parameter :: MAX_ALL_TRANSACTIONS = 100
        character (len=MAX_LEN_ACCOUNT_CODE) :: tCode(0:MAX_ALL_TRANSACTIONS)
        real :: tAmount(0:MAX_ALL_TRANSACTIONS)
        integer :: nTransactions
        character(len=7) :: prevReceipt, tReceipt

        character(len=MAX_LEN_STUDENT_CODE) :: prevStdNo, tStdNo, prevDate, tDate
        character (len=4) :: dirYear
        integer :: idxYear, idxTerm
        integer :: ierr, nReceipts

        ! read basic data for university
        call year_data_read(pathToYear)
        call make_student_directories()

        do idxYear = baseYear,currentYear
            dirYear = itoa(idxYear)

            do idxTerm = 1,3

                nReceipts = 0

                ! try 'ORTRANRECyy-t.CSV'
                fileName = trim(dirDATA)//'accounting'//DIRSEP//'ORTRANREC'//dirYear(3:4)//DASH//trim(itoa(idxTerm))//'.CSV'
                open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
                if (ierr/=0) then
                    write(*,*) 'OR Transactions file not found for '//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))
                    call log_comment('OR Transactions file not found for '//dirYEAR//DIRSEP//trim(txtSemester(idxTerm)) )
                    cycle
                end if

                write(*,*) 'Retrieving payments from '//trim(fileName)
                call log_comment('Retrieving payments from '//fileName)

                prevReceipt = '#-#-#-#'
                prevStdNo = '#-#-#-#-#-#-#-#'
                prevDate = SPACE
                nTransactions = 0
                ! skip first line
                read (unitRAW, AFORMAT, iostat = eof) line
                do
                    read (unitRAW, AFORMAT, iostat = eof) line
                    if (eof < 0) exit
                    if (line(1:3) == '   ') cycle

                    call index_to_delimiters(',', line, ndels, pos)
!no,date,receiptno,code,name,deptcd,coursecd,checkno,feecoa,amount
!1,"01/05",1001716,"09-01157","Rique, Paul John Rumusud.","CHIM","","","CERT",30.00
!2,"",1001717,"06-05749","Agron, Michael B.","CICS","","","CERT",30.00
!3,"",1001718,"11-01777","Claveria, Claudelyn De Guzman","CBEA","","","SOC",100.00
!4,"",1001718,"11-01777","Claveria, Claudelyn De Guzman","CBEA","","","ATH",80.00
!5,"",1001718,"11-01777","Claveria, Claudelyn De Guzman","CBEA","","","SCUAA",120.00
!6,"",1001718,"11-01777","Claveria, Claudelyn De Guzman","CBEA","","","DEV",200.00
!7,"",1001721,"11-02319","Sicat, Lester Jade Udasco","CHIM","","","ID",110.00

                    tDate = adjustl(line(pos(2)+2:pos(3)-2) )
                    tReceipt = adjustl(line(pos(3)+1:pos(4)-1) )
                    tStdNo = adjustl(line(pos(4)+2:pos(5)-2) )
                    tCode(0) = adjustl(line(pos(ndels-1)+2:pos(ndels)-2) )
                    read(line(pos(ndels)+1:pos(ndels+1)-1),'(f8.2)') tAmount(0)

                    if (len_trim(tDate)/=0) then
                        prevDate = tDate
                        write(*,*) 'Date = '//prevDate
                    end if

                    if (len_trim(tReceipt)==0 .or. len_trim(tStdNo)==0 .or. &
                        len_trim(tCode(0))==0 .or. tAmount(0)==0.0) then
                        write(*,*) 'Transaction is garbled : '//trim(line)
                        cycle
                    end if

                    if (tReceipt/=prevReceipt) then
                        ! write previous transaction
                        if (nTransactions>0) then
                            call xml_write_payments(idxYear, idxTerm, &
                                prevStdNo, prevDate, prevReceipt, nTransactions, tCode, tAmount)
                            !write(*,*) nReceipts, prevReceipt, ', ', prevStdNo, ', # transactions = ', nTransactions
                            !write(*,'(20(a,1x,f9.2))') (SPACE//trim(tCode(i))//'=', tAmount(i), i=1,nTransactions)
                        end if
                        ! remember current transaction
                        prevReceipt = tReceipt
                        prevStdNo = tStdNo
                        nTransactions = 0
                        nReceipts = nReceipts+1
                    end if

                    nTransactions = nTransactions + 1
                    tCode(nTransactions) = tCode(0)
                    tAmount(nTransactions) = tAmount(0)

                end do
                if (nTransactions>0) then
                    !write(*,*) nReceipts, tReceipt, ', ', tStdNo, ', # transactions = ', nTransactions
                    call xml_write_payments(idxYear, idxTerm, &
                        prevStdNo, prevDate, prevReceipt, nTransactions, tCode, tAmount)
                end if
                close(unitRAW)
                write(*,*) nReceipts, ' receipts in '//trim(fileName)

            end do ! idxTerm = 1,3

        end do ! idxYear = baseYear,currentYear

        call terminate(-1,trim(fileEXE)//SPACE//ACTION//' completed normally.')

    end subroutine generate_payments


    subroutine xml_write_payments(idxYear, idxTerm, tStdNo, tDate, tReceipt, nTransactions, tCode, tAmount)

        integer, parameter :: MAX_ALL_TRANSACTIONS = 100
        integer, intent (in) :: idxYear, idxTerm, nTransactions
        character (len=MAX_LEN_ACCOUNT_CODE), intent (in)  :: tCode(0:MAX_ALL_TRANSACTIONS)
        real, intent (in)  :: tAmount(0:MAX_ALL_TRANSACTIONS)
        character(len=7), intent (in)  :: tReceipt, tDate
        character(len=MAX_LEN_STUDENT_CODE), intent (in)  :: tStdNo

        logical :: pathExists
        character (len=MAX_LEN_FILE_PATH) :: filePayments
        integer :: idx

        ! generate file name
        idx = index(tStdNo,DASH)-1
        if (idx<=0) idx = StdNoChars

        filePayments = trim(dirPAYMENTS)//trim(tStdNo(1:idx))
        inquire(file=trim(filePayments), exist=pathExists)
        if (.not. pathExists) then
            call system (mkdirCmd//trim(filePayments), idx)
        end if
        filePayments = trim(filePayments)//DIRSEP//trim(tStdNo)//dotXML
        !write(*,*) trim(filePayments)

        inquire(file=trim(filePayments), exist=pathExists)
        if (.not. pathExists) then
            open(unit=unitXML, file=trim(filePayments), status='new')
            call xml_write_character(unitXML, 0, XML_DOC)
            call xml_write_character(unitXML, 0, 'PAYMENTS')
            call xml_write_character(unitXML, indent0, 'StdNo', tStdNo)

        else
            open(unit=unitXML, file=trim(filePayments), status='old', position='append')
            backspace(unitXML)
        end if

        call xml_write_character(unitXML, indent0, 'Payment')
        call xml_write_integer(unitXML, indent1, 'Year', idxYear)
        call xml_write_integer(unitXML, indent1, 'Term', idxTerm)
        call xml_write_character(unitXML, indent1, 'Date', tDate)
        call xml_write_character(unitXML, indent1, 'Receipt', tReceipt)
        do idx=1,nTransactions
            call xml_write_character(unitXML, indent1, 'Account', &
                trim(tCode(idx))//COMMA//trim(ftoa(tAmount(idx),2)) )
        end do
        call xml_write_character(unitXML, indent0, '/Payment')

        call xml_write_character(unitXML, 0, '/PAYMENTS')
        close(unitXML)

    end subroutine xml_write_payments


    subroutine generate_assessments()

        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo, tDate
        character(len=MAX_LEN_PERSON_NAME) :: tName
        character (len=4) :: dirYear
        integer :: idxYear, idxTerm
        integer :: ierr, nRecords, idx, std, offset
        type (TYPE_STUDENT) :: wrkStudent

        logical :: pathExists
        character (len=MAX_LEN_FILE_PATH) :: fileAssessments


        ! read basic data for university
        call year_data_read(pathToYear)
        call initialize_student(wrkStudent)

        ! retrieve students from assessment files
        do idxYear = baseYear,currentYear
            dirYear = itoa(idxYear)
            do idxTerm = 1,3
                ! try 'ENROLLASSESSyy-t.CSV'
                fileName = trim(dirDATA)//'accounting'//DIRSEP//'ASSESSDETAUTO'//dirYear(3:4)//DASH//trim(itoa(idxTerm))//'.CSV'
                open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
                if (ierr/=0) then
                    write(*,*) 'Assessment file not found for '//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))
                    cycle
                end if
                ! skip first line
                read (unitRAW, AFORMAT, iostat = eof) line
                do
                    read (unitRAW, AFORMAT, iostat = eof) line
                    if (eof < 0) exit
                    if (line(1:3) == '   ') cycle

                    call index_to_delimiters(',', line, ndels, pos)

                    offset = 0
                    if (line(pos(6)-1:pos(6)+1) /= '","') offset = 1

                    tDate = adjustl(line(pos(2)+1:pos(3)-1) )
                    tStdNo = adjustl(line(pos(3)+2:pos(4)-2) )
                    tName = adjustl(line(pos(4)+2:pos(offset+6)-2) )

                    std = index_to_student(tStdNo)
                    if (std>0) cycle
                    ! add
                    wrkStudent%StdNo =tStdNo
                    call upper_case(wrkStudent%StdNo)
                    call upper_case(tName)
                    wrkStudent%Name = tName
                    call update_student_info(wrkStudent, std)
                    !write(*,*) 'Added', -std, NumStudents, tStdNo, trim(tName)

                end do
                close(unitRAW)
            end do ! idxTerm = 1,3
        end do ! idxYear = baseYear,currentYear

        call sort_alphabetical_students()
        call make_student_directories()

        ! read the assessment files
        do idxYear = baseYear,currentYear
            dirYear = itoa(idxYear)

            do idxTerm = 1,3

                nRecords = 0

                ! try 'ASSESSDETAUTOyy-t.CSV'
                fileName = trim(dirDATA)//'accounting'//DIRSEP//'ASSESSDETAUTO'//dirYear(3:4)//DASH//trim(itoa(idxTerm))//'.CSV'
                open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
                if (ierr/=0) then
                    write(*,*) 'Assessment file not found for '//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))
                    call log_comment('Assessment file not found for '//dirYEAR//DIRSEP//trim(txtSemester(idxTerm)) )
                    cycle
                end if

                write(*,*) 'Retrieving assessments from '//trim(fileName)
                call log_comment('Retrieving assessments from '//fileName)

                ! skip first line
                read (unitRAW, AFORMAT, iostat = eof) line
                do
                    read (unitRAW, AFORMAT, iostat = eof) line
                    if (eof < 0) exit
                    if (line(1:3) == '   ') cycle
                    !write(*,*) trim(line)

                    call index_to_delimiters(',', line, ndels, pos)

!no,date,code,name,crscd,totmisc,totlab,tottuition,totothers,totassess,totdisc,totpayment,totadjust,totprevious,accrec,sub,deptcd,year
!  2    3"  "4"5 "6"   "7       8      9          10        11        12      13         14        15          16      17 18"   "19
!1,11/02/2013,"13-03503","Abedes, Krizzia Gianna Begonia          ","CBEA",2095.00,540.00,0.00,460.00,3095.00,0.00,3550.00,0.00,0.00,-455.00,77489,"CBEA",1
                    offset = 0
                    if (line(pos(6)-1:pos(6)+1) /= '","') offset = 1

                    tDate = adjustl(line(pos(2)+1:pos(3)-1) )
                    tStdNo = adjustl(line(pos(3)+2:pos(4)-2) )
                    tName = adjustl(line(pos(4)+2:pos(offset+6)-2) )

!                    write(*,*) trim(tDate)
!                    write(*,*) trim(tStdNo)
!                    write(*,*) trim(tName)

                    ! generate file name
                    idx = index(tStdNo,DASH)-1
                    if (idx<=0) idx = StdNoChars

                    fileAssessments = trim(dirASSESSMENTS)//trim(tStdNo(1:idx))//DIRSEP//trim(tStdNo)//dotXML
                    inquire(file=trim(fileAssessments), exist=pathExists)
                    if (.not. pathExists) then
                        open(unit=unitXML, file=trim(fileAssessments), status='new')

                        call xml_write_character(unitXML, 0, XML_DOC)
                        call xml_write_character(unitXML, 0, 'ASSESSMENTS')
                        call xml_write_character(unitXML, indent0, 'StdNo', tStdNo)
                        call xml_write_character(unitXML, indent0, 'Name', tName)

                    else
                        open(unit=unitXML, file=trim(fileAssessments), status='old', position='append')
                        backspace(unitXML)
                    end if

                    call xml_write_character(unitXML, indent0, 'Assessment')

                    call xml_write_integer(unitXML, indent1, 'Year', idxYear)
                    call xml_write_integer(unitXML, indent1, 'Term', idxTerm)
                    call xml_write_character(unitXML, indent1, 'Date', tDate )

!no,date,code,name,crscd,totmisc,totlab,tottuition,totothers,totassess,totdisc,totpayment,totadjust,totprevious,accrec,sub,deptcd,year
!  2    3"  "4"5 "6"   "7       8      9          10        11        12      13         14        15          16      17 18"   "19
!1,11/02/2013,"13-03503","Abedes, Krizzia Gianna Begonia          ","CBEA",2095.00,540.00,0.00,460.00,3095.00,0.00,3550.00,0.00,0.00,-455.00,77489,"CBEA",1
                    call xml_write_character(unitXML, indent1, 'Misc',       line(pos(offset+ 7)+1:pos(offset+ 8)-1) )
                    call xml_write_character(unitXML, indent1, 'Lab',        line(pos(offset+ 8)+1:pos(offset+ 9)-1) )
                    call xml_write_character(unitXML, indent1, 'Tuition',    line(pos(offset+ 9)+1:pos(offset+10)-1) )
                    call xml_write_character(unitXML, indent1, 'Other',      line(pos(offset+10)+1:pos(offset+11)-1) )
                    call xml_write_character(unitXML, indent1, 'Total',      line(pos(offset+11)+1:pos(offset+12)-1) )

                    call xml_write_character(unitXML, indent1, 'Discount',   line(pos(offset+12)+1:pos(offset+13)-1) )
                    call xml_write_character(unitXML, indent1, 'Payment',    line(pos(offset+13)+1:pos(offset+14)-1) )
                    call xml_write_character(unitXML, indent1, 'Adjust',     line(pos(offset+14)+1:pos(offset+15)-1) )
                    call xml_write_character(unitXML, indent1, 'Previous',   line(pos(offset+15)+1:pos(offset+16)-1) )
                    call xml_write_character(unitXML, indent1, 'Receivable', line(pos(offset+16)+1:pos(offset+17)-1) )

                    call xml_write_character(unitXML, indent0, '/Assessment')

                    call xml_write_character(unitXML, 0, '/ASSESSMENTS')
                    close(unitXML)

                    nRecords = nRecords+1
                end do
                close(unitRAW)
                write(*,*) nRecords, ' records in '//trim(fileName)

            end do ! idxTerm = 1,3

        end do ! idxYear = baseYear,currentYear

        call terminate(-1,trim(fileEXE)//SPACE//ACTION//' completed normally.')

    end subroutine generate_assessments

