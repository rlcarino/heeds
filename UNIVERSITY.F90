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

module UNIVERSITY

    use UTILITIES

    implicit none

!===========================================================
! declarations for University-level info
!===========================================================

    ! year that University records usable by HEEDS are available
    integer :: baseYear = 2000

    character (len=20) :: UniversityCode = 'UNIVERSITY CODE'

    character (len=80) :: &
        UniversityName = '(Specify NAME in UNIVERSITY.XML)', &
        UniversityAddress = '(Specify ADDRESS in UNIVERSITY.XML)'

    integer, parameter :: MAX_LEN_PERSON_NAME = 40 ! Max length of a person's name
    character (len=MAX_LEN_PERSON_NAME) :: &
        UniversityPresident = 'Firstname MI LastName, PhD', &
        titleUniversityPresident = 'University President', &
        !
        VPAcademicAffairs = 'Firstname MI LastName, PhD', &
        titleVPAcademicAffairs = 'Vice-President for Academic Affairs', &
        !
        DeanOfCampus = 'Firstname MI LastName, PhD', &
        titleDeanOfCampus = 'Dean of Campus', &
        !
        DeanOfInstruction = 'Firstname MI LastName, PhD', &
        titleDeanOfInstruction = 'Dean Of Instruction'


!===========================================================
! declarations for colleges
!===========================================================

    integer, parameter :: &
        MAX_ALL_COLLEGES = 30, & ! max no. of colleges
        MAX_LEN_COLLEGE_CODE=10, & ! length of college codes
        MAX_LEN_COLLEGE_NAME=60 ! length of college names

    type :: TYPE_COLLEGE
        character (len=MAX_LEN_COLLEGE_CODE) :: Code
        character (len=MAX_LEN_COLLEGE_NAME) :: Name, Dean
        logical :: hasInfo
    end type TYPE_COLLEGE

    type (TYPE_COLLEGE), dimension (0:MAX_ALL_COLLEGES) :: College
    integer :: NumColleges

    ! 'Administrative' college, for data not under the academic colleges
    character (len=MAX_LEN_COLLEGE_CODE) :: ADMINISTRATION = 'ADMIN'

!===========================================================
! declarations for departments
!===========================================================

    integer, parameter :: &
        MAX_ALL_DEPARTMENTS = 50, & ! max no. of departments
        MAX_LEN_DEPARTMENT_CODE = 10, & ! length of dept codes
        MAX_DEPARTMENT_NAME_LEN = 60 ! length of dept names

    type :: typeDEPARTMENT
        character (len=MAX_LEN_DEPARTMENT_CODE) :: Code
        character (len=MAX_DEPARTMENT_NAME_LEN) :: Name
        character (len=1) :: SectionPrefix ! prefix for section codes of classes in dept
        integer :: CollegeIdx
        logical :: hasInfo
    end type typeDEPARTMENT

    type (typeDEPARTMENT), dimension (0:MAX_ALL_DEPARTMENTS) :: Department
    integer :: NumDepartments

    integer, dimension(1:3,MAX_ALL_DEPARTMENTS) :: ScheduleCount ! last section in department

    ! 'Administrative' department, for data not under the academic departments
    character (len=MAX_LEN_DEPARTMENT_CODE) :: REGISTRAR = 'Registrar'

!===========================================================
! declarations for rooms
!===========================================================

    integer, parameter :: &
        MAX_ALL_ROOMS = 300, & ! room names
        MAX_LEN_ROOM_CODE=16    ! length of room code

    type :: TYPE_ROOM
        character (len=MAX_LEN_ROOM_CODE) :: Code
        integer :: DeptIdx, Cluster, MaxCapacity
    end type TYPE_ROOM

    type (TYPE_ROOM), dimension(0:MAX_ALL_ROOMS) :: Room
    integer :: NumRooms, NumAdditionalRooms

!===========================================================
! declarations for teachers, users
!===========================================================

    integer, parameter :: &
        MAX_ALL_TEACHERS = 900, & ! maximum number of teachers
        MAX_LEN_TEACHER_CODE=20, & ! length of login name
        MAX_LEN_TEACHER_DEGREE=80, & ! length of string for Teacher degrees
        MAX_LEN_ACADEMIC_RANK=19, & ! length of string for academic rank
        MAX_LEN_STEP_IN_RANK=6, & ! length of string for step in academic rank
        MAX_LEN_ROLE = 20

    type :: TYPE_TEACHER
        character (len=MAX_LEN_TEACHER_CODE)   :: TeacherId
        character (len=MAX_LEN_PERSON_NAME)    :: Name
        integer                                :: DeptIdx, MaxLoad, Rank, Step
        character (len=MAX_LEN_TEACHER_DEGREE) :: Bachelor, Master, Doctorate, Specialization
        character (len=MAX_LEN_PASSWD_VAR)     :: Password ! Encrypted password
        character (len=MAX_LEN_ROLE):: Role     ! Guest; set by Admin
        integer                                :: Status   ! 0=logged out, 1=logged in
    end type TYPE_TEACHER

    type (TYPE_TEACHER), dimension(0:MAX_ALL_TEACHERS) :: Teacher
    integer, dimension(0:MAX_ALL_TEACHERS) :: TeacherRank
    integer :: NumTeachers, NumAdditionalTeachers

    character (len=MAX_LEN_ACADEMIC_RANK), dimension(0:4) :: AcademicRank = (/ &
        '(Specify Rank)     ', &
        'Instructor         ', &
        'Assistant Professor', &
        'Associate Professor', &
        'Professor          ' /)

    character (len=MAX_LEN_STEP_IN_RANK), dimension(0:12) :: RankStep = (/ &
        '(Step)', &
        'I     ', &
        'II    ', &
        'III   ', &
        'IV    ', &
        'V     ', &
        'VI    ', &
        'VII   ', &
        'VIII  ', &
        'IX    ', &
        'X     ', &
        'XI    ', &
        'XII   ' /)

    ! Users
    character (len=MAX_LEN_TEACHER_CODE) :: USERNAME, GUEST = 'Guest'
    character (len=MAX_LEN_PERSON_NAME) :: ROLE
    integer :: DeptIdxUser, CollegeIdxUser, CurriculumIdxUser

!===========================================================
! declarations for time notions
!===========================================================

    integer, parameter :: MAX_LEN_TEXT_YEAR = 7
    character (len=MAX_LEN_TEXT_YEAR), dimension(0:17) :: txtYear = (/ &
        'ERROR  ', 'FIRST  ', 'SECOND ', 'THIRD  ', 'FOURTH ', 'FIFTH  ', 'SIXTH  ', &
        'SEVENTH', 'EIGHTH ', &
        '-------', 'First  ', 'Second ', 'Third  ', 'Fourth ', 'Fifth  ', 'Sixth  ', &
        'Seventh', 'Eighth ' /)

     ! academic terms
    integer, parameter :: MAX_LEN_TEXT_SEMESTER = 6
    character (len=MAX_LEN_TEXT_SEMESTER), dimension(0:9) :: txtSemester = (/ &
        'ERROR ','FIRST ', 'SECOND', 'SUMMER', &
                 'First ', 'Second', 'Summer',&
                 '1st   ', '2nd   ', 'Summer' /)

    character (len=10) :: termQualifier(0:9) = (/ &
        ' ERROR    ', ' SEMESTER ', ' SEMESTER ', ' TERM     ', &
                      ' Semester ', ' Semester ', ' Term     ', &
                      ' Sem.     ', ' Sem.     ', ' Term     ' /)

    ! days
    character (len = 3), dimension (0:6) :: txtDay = (/       &
        '   ','Mon','Tue','Wed','Thu','Fri','Sat'/)
    !character (len = 2), dimension (0:6) :: txtDay2 = (/       &
    !    '  ','M ','T ','W ','Th','F ','S '/)

    ! times
    integer, parameter :: MAX_LEN_TEXT_TIME = 5
    character (len=MAX_LEN_TEXT_TIME), dimension (0:57) :: txtTime = (/ &
        '     ', '7    ', &    !       1
        '7:15 ', '7:30 ', '7:45 ', '8    ', &    !  2 -  5
        '8:15 ', '8:30 ', '8:45 ', '9    ', &    !  6 -  9
        '9:15 ', '9:30 ', '9:45 ', '10   ', &    ! 10 - 13
        '10:15', '10:30', '10:45', '11   ', &    ! 14 - 17
        '11:15', '11:30', '11:45', '12   ', &    ! 18 - 21
        '12:15', '12:30', '12:45', '1    ', &    ! 22 - 25
        '1:15 ', '1:30 ', '1:45 ', '2    ', &    ! 26 - 29
        '2:15 ', '2:30 ', '2:45 ', '3    ', &    ! 30 - 33
        '3:15 ', '3:30 ', '3:45 ', '4    ', &    ! 34 - 37
        '4:15 ', '4:30 ', '4:45 ', '5    ', &    ! 38 - 41
        '5:15 ', '5:30 ', '5:45 ', '6    ', &    ! 42 - 45
        '6:15 ', '6:30 ', '6:45 ', '7p   ', &    ! 46 - 49
        '7:15p', '7:30p', '7:45p', '8p   ', &    ! 50 - 53
        '8:15p', '8:30p', '8:45p', '9p   ' /)    ! 54 - 57
    character (len=3) :: ampm(0:2) = (/ ' nn', ' am', ' pm' /)

    ! special times
    integer, parameter :: &
        TIME_INDEX_EARLY_DAY=5,  &            ! index of 8:00 am; time before is 'early'
        TIME_INDEX_BEGIN_LUNCH=13, &          ! begin lunchtime (10:00 am)
        TIME_INDEX_END_LUNCH=29, &            ! end lunchtime (2:00 pm)
        TIME_INDEX_LATE_DAY=45                ! index of 6:00 pm

!===========================================================
! declarations for subjects
!===========================================================

    integer, parameter :: &
        MAX_ALL_SUBJECTS = 3000, &           ! max no. of subjects
        MAX_LEN_SUBJECT_CODE = 20, &         ! length of subject names
        MAX_LEN_SUBJECT_TITLE=100, &         ! length of subject titles
        MAX_ALL_SUBJECT_PREREQ = 50, &       ! max number of tokens in prereq of a subject
        MAX_ALL_SUBJECT_COREQ = 5, &         ! max number of tokens in corequisite of a subject
        MAX_ALL_SUBJECT_CONCURRENT = 5, &    ! max number of tokens in concurrencies
        MAX_ALL_SUBJECT_CONCPREQ = 5, &      ! max number of tokens in concurrent prereq of a subject
        MAX_ALL_DUMMY_SUBJECTS = -50         ! max no. of dummy subjects

    type :: TYPE_SUBJECT
        character (len=MAX_LEN_SUBJECT_CODE) :: Name
        character (len=MAX_LEN_SUBJECT_TITLE) :: Title
        integer :: DeptIdx
        real :: Units
        real :: Failrate(1:3)
        integer :: GrandTotal(2,1:3)
        integer :: TermOffered
        real :: Tuition, LabFee, LectHours, LectLoad
        integer :: MinLectSize, MaxLectSize
        real :: LabHours, LabLoad
        integer :: MaxLabSize, MinLabSize, &
            lenPreq, Prerequisite(MAX_ALL_SUBJECT_PREREQ), &
            lenCoreq, Corequisite(MAX_ALL_SUBJECT_COREQ), &
            lenConc, Concurrent(MAX_ALL_SUBJECT_CONCURRENT), &
            lenConcPreq, ConcPrerequisite(MAX_ALL_SUBJECT_CONCPREQ)
    end type TYPE_SUBJECT

    type (TYPE_SUBJECT), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Subject
    integer :: NumSubjects, NumDummySubjects, NumAdditionalSubjects
    integer :: INDEX_TO_NONE

    integer, dimension (0:MAX_ALL_SUBJECTS) :: SubjectRank

    ! subject areas
    type :: TYPE_SUBJECT_AREA
        character (len=MAX_LEN_SUBJECT_CODE) :: Code
        integer :: CollegeIdx, Count
    end type TYPE_SUBJECT_AREA
    type (TYPE_SUBJECT_AREA), dimension (0:MAX_ALL_SUBJECTS/3) :: SubjectArea
    integer :: NumSubjectAreas

    ! substitutions/equivalencies
    integer, parameter :: &
        MaxAllSubstitutions = 700, &                ! max no. of substitutions
        MaxSubsArraySize = MaxAllSubstitutions*10   ! max size of substitutions array
    integer :: NumSubst
    integer, dimension(MaxAllSubstitutions) :: SubstIdx
    integer, dimension(MaxSubsArraySize) :: Substitution

    ! renamed subjects
    integer, parameter :: MAX_ALL_RENAMES = 500
    integer :: NumRenames
    integer, dimension(MAX_ALL_RENAMES,2) :: Renamed

    ! subject offering
    type :: TYPE_OFFERED_SUBJECTS
        integer :: Demand, NSections, TotalSlots, &
        Accommodated, PriorityNotAccommodated, &
        OpenSections, OpenSlots, SortKey
    end type TYPE_OFFERED_SUBJECTS
    type (TYPE_OFFERED_SUBJECTS), dimension (3,MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering


!===========================================================
! declarations for curricula
!===========================================================

    integer, parameter :: &
        MAX_ALL_CURRICULA = 275, & ! max no. of active curricula
        MAX_LEN_CURRICULUM_CODE = 20, & ! max length of curriculum codes
        MAX_LEN_CURRICULUM_NAME = 127, & ! max length of curriculum names
        MAX_SUBJECTS_IN_CURRICULUM = 200 ! max no. of subjects in a curriculum

    integer, parameter :: &
        MAX_SUBJECTS_PER_TERM = 2*MAX_SUBJECTS_IN_CURRICULUM/3 ! max no. of subjects per term

    type :: TYPE_CURRICULUM
        logical :: Active
        character (len=MAX_LEN_CURRICULUM_CODE) :: Code
        character (len=MAX_LEN_CURRICULUM_NAME) :: Title
        character (len=MAX_LEN_CURRICULUM_NAME) :: Specialization
        character (len=MAX_LEN_CURRICULUM_NAME) :: Remark
        integer :: CollegeIdx, NumTerms, NSubjects
        integer, dimension(MAX_SUBJECTS_IN_CURRICULUM) :: SubjectIdx, SubjectTerm
    end type TYPE_CURRICULUM

    type (TYPE_CURRICULUM), dimension (0:MAX_ALL_CURRICULA) :: Curriculum
    integer :: NumCurricula

!    ! freshman intake by curriculum
!    integer, dimension(0:MAX_ALL_CURRICULA) :: NFintake

    ! flags to indicate processing is done for a curriculum group
    logical :: done(0:MAX_ALL_CURRICULA)

    ! file unit numbers based on generic curriculum
    integer, dimension(0:MAX_ALL_CURRICULA) :: CurrProgNum
    character (len=MAX_LEN_CURRICULUM_CODE), dimension(0:MAX_ALL_CURRICULA) :: CurrProgCode

    real, dimension(0:20) :: TermUnits, SpecifiedUnits


!===========================================================
! declarations for sections
!===========================================================

    integer, parameter :: &
        MAX_ALL_SECTIONS = 6000, &      ! all sections offered in a given term
        MAX_LEN_SECTION_CODE = 20, &    ! length of section codes
        MAX_SECTION_MEETINGS = 12       ! maximum no. of meetings of a section in a week

    integer, parameter :: &
        MAX_LEN_CLASS_ID = MAX_LEN_SUBJECT_CODE+MAX_LEN_SECTION_CODE, &      ! length of section codes
        MAX_LEN_BLOCK_CODE = MAX_LEN_CLASS_ID

    type :: TYPE_SECTION
        character (len=MAX_LEN_CLASS_ID) :: ClassId
        character (len=MAX_LEN_SECTION_CODE) :: Code
        integer :: DeptIdx, SubjectIdx, Slots, RemSlots, NMeets
        integer, dimension(0:MAX_SECTION_MEETINGS) :: DayIdx, bTimeIdx, eTimeIdx, RoomIdx, TeacherIdx
    end type TYPE_SECTION
    type (TYPE_SECTION), dimension (3,0:MAX_ALL_SECTIONS) :: Section
    integer :: NumSections(3)

    logical :: UseCLASSES = .false.


!===========================================================
! declarations for blocks
!===========================================================

    type :: TYPE_BLOCK
        character(len=MAX_LEN_BLOCK_CODE) :: BlockID  ! block code
        character(len=MAX_LEN_CURRICULUM_NAME) :: Name ! block description
        integer :: Size ! size of block
        integer :: DeptIdx ! index to department that created the block
        integer :: CurriculumIdx ! index to curriculum
        integer :: Year ! year level in curriculum
        integer :: Term ! term
        integer :: NumClasses ! no of subjects for this block
        integer, dimension(MAX_SUBJECTS_PER_TERM) :: Subject, Section ! subjects and corresponding sections
    end type TYPE_BLOCK

    integer, parameter :: MAX_ALL_BLOCKS = 6*MAX_ALL_CURRICULA ! max all blocks
    type (TYPE_BLOCK), dimension(3,0:MAX_ALL_BLOCKS) :: Block
    integer :: NumBlocks(3)

!===========================================================
! declarations for timetables
!===========================================================

    integer, parameter :: &
        MAX_ALL_SECTIONS_OF_A_SUBJECT = 150, &   ! max sections in a subject
        MAX_ALL_STUDENTS_OF_A_SUBJECT = 5000  ! max students taking a subject

    ! undesirability weights
    integer, parameter :: &
        PENALTY_LONG_DAY=500, & ! penalty for a "long span of classes" day
        PENALTY_HUNGRY_DAY=300, &  ! penalty for "no lunch" days
        PENALTY_EARLY_DAY=0, &    ! penalty for early days
        PENALTY_LATE_DAY=20, &     ! penalty for late days
        PENALTY_TRAVEL_IN_A_HURRY=400 ! penalty for travels between consecutive classses

    integer, dimension (0:MAX_ALL_SECTIONS_OF_A_SUBJECT) :: &
        LongDays, HungerDays, EarlyDays, LateDays, Travels

    integer, dimension (0:MAX_ALL_SECTIONS_OF_A_SUBJECT) :: &
        Undesirability, UndesirabilityRank



contains

!===========================================================
! routines for colleges
!===========================================================

    subroutine initialize_college (wrkCollege, tCode, tName, tDean)

        type(TYPE_COLLEGE), intent(out) :: wrkCollege
        character(len=*), intent(in), optional :: tCode, tName, tDean

        if (present(tCode)) then
            wrkCollege = TYPE_COLLEGE(tCode, tName, tDean, .false.)
        else
            wrkCollege = TYPE_COLLEGE(SPACE, SPACE, 'Firstname MI Lastname, Ph.D.', .false.)
        end if

    end subroutine initialize_college


    function index_to_college (tCode)
        !returns index of tCode in the list of colleges

        integer :: index_to_college
        character (len=MAX_LEN_COLLEGE_CODE), intent (in) :: tCode

        integer :: i

        index_to_college = 0
        do i=1,NumColleges
            if (tCode==College(i)%Code) then
                index_to_college = i
                exit
            end if
        end do

    end function index_to_college


!===========================================================
! routines for departments
!===========================================================

    subroutine initialize_department (wrkDepartment, tCode, tName, tPrefix, iCollege)

        type(typeDEPARTMENT), intent (out) :: wrkDepartment
        character(len=*), intent (in), optional :: tCode, tName, tPrefix
        integer, intent (in), optional :: iCollege

        if (present(tCode)) then
            wrkDepartment = typeDEPARTMENT(tCode, tName, tPrefix, iCollege, .false.)
        else
            wrkDepartment = typeDEPARTMENT(SPACE, SPACE, SPACE, 0, .false.)
        end if

    end subroutine initialize_department


    function index_to_dept(token)

        integer :: index_to_dept
        character (len=MAX_LEN_DEPARTMENT_CODE), intent(in) :: token

        integer :: i

        index_to_dept = 0
        do i=1,NumDepartments
            if (token==Department(i)%Code) then
                index_to_dept = i
                exit
            end if
        end do

    end function index_to_dept


!===========================================================
! routines for rooms
!===========================================================

    subroutine initialize_room (wrkRoom, tCode, iDept, iCluster, iCapacity)

        type(TYPE_ROOM), intent (out) :: wrkRoom
        character(len=*), intent (in), optional :: tCode
        integer, intent (in), optional :: iDept, iCluster, iCapacity

        if (present(tCode)) then
            wrkRoom = TYPE_ROOM(tCode, iDept, iCluster, iCapacity)

        else
            wrkRoom = TYPE_ROOM(SPACE, NumDepartments, 0, 0)
        end if


    end subroutine initialize_room


    function index_to_room (tRoom)

        integer :: index_to_room
        character (len=MAX_LEN_ROOM_CODE), intent (in) :: tRoom

        integer :: rdx

        index_to_room = 0

        ! use sequential search because room codes may be edited dynamically
        do rdx=1,NumRooms+NumAdditionalRooms
            if (tRoom==Room(rdx)%Code) then
                index_to_room = rdx
                return
            end if
        end do

    end function index_to_room


!===========================================================
! routines for teachers
!===========================================================

    subroutine initialize_teacher (wrkTeacher)

        type(TYPE_TEACHER), intent (out) :: wrkTeacher

        wrkTeacher = TYPE_TEACHER(SPACE, SPACE, NumDepartments, 0, 0, 0, SPACE, SPACE, SPACE, SPACE, &
                SPACE, GUEST, 0)

    end subroutine initialize_teacher


    function index_to_teacher (token)

        integer :: index_to_teacher
        character (len=MAX_LEN_TEACHER_CODE), intent (in) :: token

        integer :: i, j!, tdx

        index_to_teacher = 0
        if (len_trim(token)==0) return

        ! sequential search, because teachers may be added dynamically or teacher codes edited
        j = 0
        do i=0,NumTeachers
            if (token==Teacher(i)%TeacherId) then
                j = i
                exit
            end if
        end do
        index_to_teacher = j

    end function index_to_teacher


    subroutine sort_teachers

        integer :: i, j
        type(TYPE_TEACHER) :: wrkTeacher

        do i=1,NumTeachers-1
            do j=i+1,NumTeachers
                if (Teacher(i)%TeacherId>Teacher(j)%TeacherId) then
                    wrkTeacher = Teacher(i)
                    Teacher(i) = Teacher(j)
                    Teacher(j) = wrkTeacher
                end if
            end do
        end do

    end subroutine sort_teachers


    subroutine sort_alphabetical_teachers()

        integer :: i, j, k
        !write (*,*) 'Sorting teachers alphabetically... please wait...'

        TeacherRank = 0
        do i=1,NumTeachers
            TeacherRank(i) = i
        end do

        do i=1,NumTeachers-1

            do j=i+1,NumTeachers
                if (Teacher(TeacherRank(i))%Name > Teacher(TeacherRank(j))%Name) then
                    k = TeacherRank(i)
                    TeacherRank(i) = TeacherRank(j)
                    TeacherRank(j) = k
                end if
            end do

        end do

    end subroutine sort_alphabetical_teachers



    subroutine get_teacher_password(tdx, Password)
        integer, intent (in) :: tdx
        character (len=MAX_LEN_PASSWD_VAR), intent (out) :: Password

        Password = Teacher(tdx)%Password
        call decrypt(passwordEncryptionKey, Password)
        Password = Password(lenPasswordEncryptionKey-atoi(Password(3:4))+1:)

    end subroutine get_teacher_password



    function is_teacher_password(tdx, Password)
        logical :: is_teacher_password
        integer, intent (in) :: tdx
        character (len=MAX_LEN_PASSWD_VAR), intent (in) :: Password
        character (len=MAX_LEN_PASSWD_VAR) :: tPassword

        call get_teacher_password(tdx, tPassword)
        is_teacher_password = tPassword(:MAX_LEN_PASSWORD)==Password(:MAX_LEN_PASSWORD)

    end function is_teacher_password


!===========================================================
! routines for time, time periods
!===========================================================


    function index_to_year (tYear)
        ! returns index of tYear in the list of Years
        integer :: index_to_year
        character (len=MAX_LEN_TEXT_YEAR), intent (in) :: tYear
        integer :: i, idx

        idx = 0
        do i=1,17
            if (tYear==txtYear(i)) then
                idx = i
                exit
            end if
        end do
        if (idx>9) idx = idx-9
        index_to_year = idx

    end function index_to_year


    function text_time_period(startime, endtime)
        character (len=2*MAX_LEN_TEXT_TIME+1) :: text_time_period
        integer, intent (in) :: startime, endtime
        if (startime>=1 .and. endtime<=57 .and. startime<endtime) then
            text_time_period = trim(txtTime(startime))//"-"//trim(txtTime(endtime))
        else
            text_time_period = 'TBA'
        end if

    end function text_time_period


    function index_to_time(tTime)
        integer :: index_to_time
        character (len=MAX_LEN_TEXT_TIME), intent (in) :: tTime
        integer :: hdx, i
        hdx = 0
        do i=1,57
            if (txtTime(i)==tTime) then
                hdx = i
                exit
            end if
        end do
        index_to_time = hdx

    end function index_to_time


    function index_to_term (tTerm)

        ! returns index of tTerm in the list of Terms
        integer :: index_to_term
        character (len=MAX_LEN_TEXT_SEMESTER), intent (in) :: tTerm
        integer :: i
        index_to_term = 0
        do i=1,9
            if (tTerm==txtSemester(i)) then
                index_to_term = i
                exit
            end if
        end do
        if (index_to_term>6) index_to_term = index_to_term - 3
        if (index_to_term>3) index_to_term = index_to_term - 3

    end function index_to_term


    subroutine rank_to_year_term (rank, Year, Term)
        ! rank: 1 2 3 4 5 6 7 8 9 10 11 12
        ! year: 1 1 1 2 2 2 3 3 3  4  4  4
        ! term: 1 2 3 1 2 3 1 2 3  1  2  3
        integer, intent(in) :: rank
        integer, intent(out) :: Year, Term
        Year = (rank+2)/3
        Term = rank-3*(Year-1)

    end subroutine rank_to_year_term


    subroutine qualify_term (plusCurrent, Year, Term, description)
        integer, intent(in) :: plusCurrent
        integer, intent(out) :: Year, Term
        character(len=*), intent (out) :: description

        if (plusCurrent<=3) then
            Year = currentYear
            Term = plusCurrent
        else
            Year = currentYear+1
            Term = plusCurrent-3
        end if
        if (Term<3) then
            description = trim(txtSemester(Term+6))//' Semester, '//text_school_year(Year)
        else
            description = 'Summer Term, '//text_school_year(Year)
        end if

    end subroutine qualify_term


    function text_term_offered (num)
        ! returns the text representation of TermOffered of subj

        character (len=3) :: text_term_offered
        integer, intent (in) :: num

        character (len=3) :: term
        integer :: i

        if (num==0) then
            text_term_offered = '0'
            return
        end if
        term = SPACE
        i = num
        if (i>=4) then
            term = 'S'//term
            i = i-4
        end if
        if (i>=2) then
            term = '2'//term
            i = i-2
        end if
        if (i==1) term = '1'//term
        text_term_offered = term

    end function text_term_offered


    function text_term_offered_separated (num)
        ! returns the text representation of TermOffered of subj

        character (len=5) :: text_term_offered_separated
        integer, intent (in) :: num

        character (len=5) :: term
        integer :: i

        if (num==0) then
            text_term_offered_separated = '0'
            return
        end if

        term = SPACE
        i = num
        if (i>=4) then
            term = 'S' ! 'Summer'
            i = i-4
            if (i>0) term = ','//term ! ', '//term
        end if

        if (i>=2) then
            term = '2'//term ! 'Second Semester'//term
            i = i-2
            if (i>0) term = ','//term ! ', '//term
        end if

        if (i==1) term = '1'//term ! 'First Semester'//term

        text_term_offered_separated = term

    end function text_term_offered_separated


    function text_school_year (year)
        ! returns the text representation of SY year-(year+1)

        character (len=10) :: text_school_year
        integer, intent (in) :: year

        text_school_year = 'SY '//trim(itoa(year))//dash//itoa2bz(mod(year+1,1000))

    end function text_school_year


!===========================================================
! routines for subjects
!===========================================================

    subroutine initialize_subject (wrkSubject)

        type(TYPE_SUBJECT), intent (out) :: wrkSubject

        wrkSubject = TYPE_SUBJECT ( &
            SPACE, & ! character (len=MAX_LEN_SUBJECT_CODE) :: Name
            SPACE, & ! character (len=MAX_LEN_SUBJECT_TITLE) :: Title
            NumDepartments, & ! integer :: DeptIdx
            0.0, & ! real :: Units
            0.0, & ! real :: Failrate(1:3)
            0, & ! integer :: GrandTotal(2,1:3)
            0, & ! integer :: TermOffered
            0.0, 0.0, & ! real :: Tuition, LabFee
            0.0, 0.0, & ! real :: LectHours, LectLoad
            0, 0, & ! integer :: MinLectSize, MaxLectSize
            0.0, 0.0, & ! real :: LabHours, LabLoad
            0, 0, & ! integer :: MaxLabSize, MinLabSize, &
            1, INDEX_TO_NONE, & !     lenPreq, Prerequisite(MAX_ALL_SUBJECT_PREREQ), &
            1, INDEX_TO_NONE, & !     lenCoreq, Corequisite(MAX_ALL_SUBJECT_COREQ), &
            1, INDEX_TO_NONE, & !     lenConc, Concurrent(MAX_ALL_SUBJECT_CONCURRENT), &
            1, INDEX_TO_NONE) !     lenConcPreq, ConcPrerequisite(MAX_ALL_SUBJECT_CONCPREQ)

    end subroutine initialize_subject


    function index_to_subject(token)
        integer :: index_to_subject
        character (len=MAX_LEN_SUBJECT_CODE), intent(in) :: token
        integer :: i, j, cdx

        ! try the dummy subjects
        index_to_subject = 0
        do cdx=NumDummySubjects,-1
            if (token==Subject(cdx)%Name) then
                index_to_subject = cdx
                return
            end if
        end do

        ! try the newly added subjects
        do cdx=NumSubjects+1,NumSubjects+NumAdditionalSubjects
            if (token==Subject(cdx)%Name) then
                index_to_subject = cdx
                return
            end if
        end do

        ! the named subjects
        i = 1
        j = NumSubjects
        do
            if (i>j) then ! not found
                cdx = 0
                exit
            else
                cdx = (i + j)/2
                if (token==Subject(cdx)%Name) then
                    !           write(*,*)  '... found '//Subject(cdx)%Name
                    exit
                else if (token<Subject(cdx)%Name) then
                    j = cdx-1
                !           write(*,*)  '...<'//Subject(cdx)%Name
                else
                    i = cdx+1
                !           write(*,*)  '...>'//Subject(cdx)%Name
                end if
            end if
        end do
        index_to_subject = cdx

    end function index_to_subject


    function index_to_new_subject(subj)
        integer :: index_to_new_subject
        integer, intent(in) :: subj
        integer :: i, j, cdx
        i = 1
        j = NumRenames
        do
            if (i>j) then ! not found
                cdx = subj
                exit
            else
                cdx = (i + j)/2
                if (subj==Renamed(cdx,1)) then
                    cdx = Renamed(cdx,2)
                    exit
                else if (subj<Renamed(cdx,1)) then
                    j = cdx-1
                else
                    i = cdx+1
                end if
            end if
        end do
        index_to_new_subject = cdx

    end function index_to_new_subject


    subroutine tokenize_subjects(subline, symbol, maxTokens, nTokens, tokenArray, ier, mesg)
        integer, intent (in) :: maxTokens
        integer, intent (out) :: nTokens, tokenArray(maxTokens), ier
        character, intent (in) :: symbol ! delimiter
        character(len=*), intent (in) :: subline
        character(len=*), intent (out), optional :: mesg
        integer :: i, j, k, ndelsub, posub(30)
        character (len=MAX_LEN_SUBJECT_CODE) :: token
        character (len=MAX_LEN_FILE_PATH) :: errMsg

        !write(*,*) 'String is : '//subline
        errMsg = SPACE
        ier = 0
        k = 0
        call index_to_delimiters(symbol, subline, ndelsub, posub)
        do j=1,ndelsub
            token = adjustl(subline(posub(j)+1:posub(j+1)-1))
            i = index_to_subject(token)
            if (i==0) then
                    errMsg = ', '//token
                    cycle
            end if
            k = k+1
            tokenArray(k) = i
        end do
        nTokens = k
        if (present(mesg)) then
            if (len_trim(errMsg)>0) then
                mesg = 'Not found -'//errMsg(2:)
            else
                mesg = SPACE
            end if
        end if

    end subroutine tokenize_subjects


    function is_graduate_level_subject(subj)
        logical :: is_graduate_level_subject
        integer, intent (in) :: subj
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        integer :: i,f,l,num
        tSubject = Subject(subj)%Name
        i = index(tSubject,DASH)
        if (i>0) then
            l = i-1
        else
            l = len_trim(tSubject)
        end if
        i = index(tSubject,'.')
        if (i>0) l = i-1
        f = index(tSubject, SPACE)+1
        tSubject = tSubject(f:l)
        num = atoi(tSubject)
        is_graduate_level_subject = num>200

    end function is_graduate_level_subject


    function is_offered(subj, term)
        logical :: is_offered
        integer, intent (in) :: subj, term
        integer :: j

        ! offered?
        j = Subject(subj)%TermOffered
        is_offered = &
        (j==7) .or. & ! offered 12S
        (term==1 .and. mod(j,2)==1) .or. & ! offered 1,12,1S
        (term==2 .and. (j==2 .or. j==3 .or. j==6) ) .or. & ! offered 2,12,2S
        (term==3 .and. (j>3) ) ! offered S,1S,2S

    end function is_offered


    function is_lecture_lab_subject(subj)
        integer, intent (in) :: subj
        logical :: is_lecture_lab_subject
        ! returns true if subj is a lecture-lab/recit subject
        is_lecture_lab_subject = Subject(subj)%LectHours*Subject(subj)%LabHours/=0.0

    end function is_lecture_lab_subject


    function text_prerequisite_of_subject(subj,href)
        ! returns the text representation of the prerequisite of subj
        character (len=255) :: text_prerequisite_of_subject, displayStr
        integer, intent (in) :: subj, href
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=255) :: str127(2*MAX_ALL_SUBJECT_PREREQ)
        integer :: j, prereq!, ddx1, ddx2

        !ddx1 = Subject(subj)%DeptIdx

        ! corequisite
        displayStr = SPACE
        str127 = SPACE
        do j=Subject(subj)%lenCoreq,1,-1
            tSubject = Subject(Subject(subj)%Corequisite(j))%Name
            if (tSubject=='AND' .or. tSubject=='OR') then
                if (j/=1) then
                    str127(j) = '('//trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))//')'
                    str127(j+1) = str127(j+3)
                    str127(j+2) = SPACE
                else
                    str127(j) = trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))
                end if
            else
                str127(j) = tSubject
            end if
        end do
        if (str127(1) /= 'NONE' .and. str127(1)/=SPACE) displayStr = 'Co-req: '//trim(str127(1))
        ! concurrencies
        str127 = SPACE
        do j=Subject(subj)%lenConc,1,-1
            tSubject = Subject(Subject(subj)%Concurrent(j))%Name
            if (tSubject=='AND' .or. tSubject=='OR') then
                if (j/=1) then
                    str127(j) = '('//trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))//')'
                    str127(j+1) = str127(j+3)
                    str127(j+2) = SPACE
                else
                    str127(j) = trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))
                end if
            else
                str127(j) = tSubject
            end if
        end do
        if (str127(1) /= 'NONE' .and. str127(1)/=SPACE) then
            if (displayStr/=SPACE) then
                displayStr = 'Conc. with: '//trim(str127(1))//'. '//displayStr
            else
                displayStr = 'Conc. with: '//str127(1)
            end if
        end if
        str127 = SPACE
        do j=Subject(subj)%lenConcPreq,1,-1
            tSubject = Subject(Subject(subj)%ConcPrerequisite(j))%Name
            if (tSubject=='AND' .or. tSubject=='OR') then
                if (j/=1) then
                    str127(j) = '('//trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))//')'
                    str127(j+1) = str127(j+3)
                    str127(j+2) = SPACE
                else
                    str127(j) = trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))
                end if
            else
                str127(j) = tSubject
            end if
        end do
        if (str127(1) /= 'NONE' .and. str127(1)/=SPACE) then
            if (displayStr/=SPACE) then
                displayStr = 'Conc. preq: '//trim(str127(1))//'. '//displayStr
            else
                displayStr = 'Conc. preq: '//str127(1)
            end if
        end if

        str127 = SPACE
        do j=Subject(subj)%lenPreq,1,-1
            prereq = Subject(subj)%Prerequisite(j)
            tSubject = Subject(prereq)%Name
            if (tSubject == 'OR') then
                if (j /= 1) then
                    str127(j) = trim(str127(j+2))//' or '//trim(str127(j+1))//SPACE
                    str127(j+1) = str127(j+3)
                    str127(j+2) = SPACE
                else
                    str127(j) = trim(str127(j+2))//' or '//trim(str127(j+1))
                end if
            else if (tSubject == 'AND') then
                if (j /= 1) then
                    str127(j) = trim(str127(j+2))//' and '//trim(str127(j+1))//SPACE
                    str127(j+1) = str127(j+3)
                    str127(j+2) = SPACE
                else
                    str127(j) = trim(str127(j+2))//' and '//trim(str127(j+1))
                end if
            else
                !ddx2 = Subject(prereq)%DeptIdx
                !if (Department(ddx2)%Code=='(dummy)' .or. ddx1==ddx2 .or. href==0) then
                !  str127(j) = tSubject
                !else
                !  str127(j) = make_href(not_cgi, fnSubjectList, Subject(prereq)%Name, &
                !    A1=Department(Subject(prereq)%DeptIdx)%Code, anchor=Subject(prereq)%Name)
                !end if
                str127(j) = tSubject
            end if
        end do

        if (str127(1)/=SPACE) then
            if (displayStr/=SPACE) then
                displayStr = trim(str127(1))//'. '//displayStr
            else
                displayStr = str127(1)
            end if
        end if
        text_prerequisite_of_subject = displayStr

    end function text_prerequisite_of_subject


!===========================================================
! routines for curricula
!===========================================================


    subroutine make_curriculum_groups
        ! associate each curriculum with a file unit number
        integer :: i, j, k, l, idx
        CurrProgNum = 0
        CurrProgCode = SPACE
        k = 0
        do idx=1,NumCurricula
            if (CurrProgNum(idx)==0) then
                k = k+1
                CurrProgNum(idx) = k
                l = len_trim(Curriculum(idx)%Code)
                j = index(Curriculum(idx)%Code, '-')
                if (j==0) then
                    j = l
                else
                    j = j-1
                end if
                CurrProgCode(idx) = Curriculum(idx)%Code(1:j)
                do i = idx+1,NumCurricula
                    if (Curriculum(idx)%CollegeIdx==Curriculum(i)%CollegeIdx .and. &
                    Curriculum(idx)%Code(1:j)==Curriculum(i)%Code(1:j) .and. &
                    (Curriculum(i)%Code(j+1:j+1)==SPACE .or. &
                    Curriculum(i)%Code(j+1:j+1)=='-')) then
                        CurrProgNum(i) = k
                        CurrProgCode(i) = CurrProgCode(idx)
                    end if
                end do
            end if
        end do
        CurrProgNum(0) = k

    end subroutine make_curriculum_groups


    function index_to_curriculum (token)
        integer :: index_to_curriculum
        character (len=MAX_LEN_CURRICULUM_CODE), intent (in) :: token
        integer :: i, j, k
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum

        ! find an exact match
        tCurriculum = token
        index_to_curriculum = 0
        do i=1,NumCurricula
            if (tCurriculum==Curriculum(i)%Code) then
                index_to_curriculum = i
                return
            end if
        end do

        ! try removing the qualifiers
        do
            j = 0
            do k=len_trim(tCurriculum), 2,-1
                if (tCurriculum(k:k)=='-') then
                    j = k
                    exit
                end if
            end do
            if (j>0) then
                tCurriculum(j:) = SPACE
                do i=1,NumCurricula-1
                    if (tCurriculum==Curriculum(i)%Code) then
                        index_to_curriculum = -i
                        return
                    end if
                end do
            else
                exit
            end if
        end do

        ! find last code whose first few characters (up to the '-') match
        tCurriculum = trim(token)//DASH
        do i=1,NumCurricula-1
            if (index(Curriculum(i)%Code,trim(tCurriculum))==1) then
                index_to_curriculum = -i
            end if
        end do

    end function index_to_curriculum


    function index_of_subject_in_curriculum (tCurriculum, crse)
        ! returns the first occurence of crse in tCurriculum
        integer :: index_of_subject_in_curriculum
        type (TYPE_CURRICULUM), intent (in) :: tCurriculum
        integer, intent (in) :: crse
        integer :: i
        index_of_subject_in_curriculum = 0
        do i=1,tCurriculum%NSubjects
            if (tCurriculum%SubjectIdx(i)==crse .and. &
            tCurriculum%SubjectTerm(i)>0 ) then
                index_of_subject_in_curriculum = i
                exit
            end if
        end do

    end function index_of_subject_in_curriculum


    function is_used_in_college_subject (college_idx, subject_idx)
        ! returns true if subject area is used in a curriculum in college
        logical :: is_used_in_college_subject, found
        integer, intent (in) :: college_idx, subject_idx
        integer :: i
        found = .false.
        do i=1,NumCurricula
            if (Curriculum(i)%CollegeIdx/=college_idx) cycle
            if (index_of_subject_in_curriculum(Curriculum(i), subject_idx)>0) then
                found = .true.
                exit
            end if
        end do
        is_used_in_college_subject = found

    end function is_used_in_college_subject


    function is_used_in_curriculum_subject_area (tCurriculum, area)
        ! returns true if subject area is in curriculum
        logical :: is_used_in_curriculum_subject_area, found
        type (TYPE_CURRICULUM), intent (in) :: tCurriculum
        character(len=*) :: area
        integer :: i
        found = .false.
        do i=1,tCurriculum%NSubjects
            if (index(Subject(tCurriculum%SubjectIdx(i))%Name,area)==1) then
                found = .true.
                exit
            end if
        end do
        is_used_in_curriculum_subject_area = found

    end function is_used_in_curriculum_subject_area


    function text_prerequisite_in_curriculum(crse,tCurriculum)
        ! returns the text representation of the prerequisite of crse
        character (len=255) :: text_prerequisite_in_curriculum, displayStr
        integer, intent (in) :: crse
        type (TYPE_CURRICULUM), intent (in), optional :: tCurriculum
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=255) :: str127(MAX_ALL_SUBJECT_PREREQ)
        integer :: i, j, k, idxCURR
        logical :: satisfiable

        ! corequisite
        displayStr = SPACE
        str127 = SPACE
        do j=Subject(crse)%lenCoreq,1,-1
            tSubject = Subject(Subject(crse)%Corequisite(j))%Name
            if (tSubject=='AND' .or. tSubject=='OR') then
                if (j/=1) then
                    str127(j) = '('//trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))//')'
                    str127(j+1) = str127(j+3)
                    str127(j+2) = SPACE
                else
                    str127(j) = trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))
                end if
            else
                str127(j) = tSubject
            end if
        end do
        if (str127(1)/='NONE' .and. str127(1)/=SPACE) displayStr = 'Co-req: '//trim(str127(1))
        ! concurrencies
        str127 = SPACE
        do j=Subject(crse)%lenConc,1,-1
            tSubject = Subject(Subject(crse)%Concurrent(j))%Name
            if (tSubject=='AND' .or. tSubject=='OR') then
                if (j/=1) then
                    str127(j) = '('//trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))//')'
                    str127(j+1) = str127(j+3)
                    str127(j+2) = SPACE
                else
                    str127(j) = trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))
                end if
            else
                str127(j) = tSubject
            end if
        end do
        if (str127(1)/='NONE' .and. str127(1)/=SPACE) then
            if (displayStr/=SPACE) then
                displayStr = 'Conc. with: '//trim(str127(1))//'. '//displayStr
            else
                displayStr = 'Conc. with: '//str127(1)
            end if
        end if
        str127 = SPACE
        do j=Subject(crse)%lenConcPreq,1,-1
            tSubject = Subject(Subject(crse)%ConcPrerequisite(j))%Name
            if (tSubject=='AND' .or. tSubject=='OR') then
                if (j/=1) then
                    str127(j) = '('//trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))//')'
                    str127(j+1) = str127(j+3)
                    str127(j+2) = SPACE
                else
                    str127(j) = trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))
                end if
            else
                str127(j) = tSubject
            end if
        end do
        if (str127(1)/='NONE' .and. str127(1)/=SPACE) then
            if (displayStr/=SPACE) then
                displayStr = 'Conc. preq: '//trim(str127(1))//'. '//displayStr
            else
                displayStr = 'Conc. preq: '//str127(1)
            end if
        end if
        str127 = SPACE
        satisfiable = .true.
        if (present(tCurriculum)) then
            idxCURR = index_to_curriculum(tCurriculum%Code)
            satisfiable = is_prerequisite_satisfiable_in_curriculum(crse, idxCURR)
            do j=Subject(crse)%lenPreq,1,-1
                k = Subject(crse)%Prerequisite(j)
                tSubject = Subject(k)%Name
                if (tSubject=='AND') then
                    if (j/=1) then
                        str127(j) = trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))
                        str127(j+1) = str127(j+3)
                        str127(j+2) = SPACE
                    else
                        str127(j) = trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))
                    end if
                else if (tSubject=='OR') then
                    if (index(str127(j+2),'*')>0) then
                        str127(j) = str127(j+1)
                    else if (index(str127(j+1),'*')>0) then
                        str127(j) = str127(j+2)
                    else
                        str127(j) = trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))
                    end if
                    if (j/=1) then
                        str127(j+1) = str127(j+3)
                        str127(j+2) = SPACE
                    end if
                else
                    if (k>0) then
                        i = index_of_subject_in_curriculum(tCurriculum, k)
                        if (i>0) then
                            if (satisfiable) then
                                str127(j) = tSubject
                            else
                                str127(j) = red//trim(tSubject)//black
                            end if
                        else
                            str127(j) = red//trim(tSubject)//black//'*'
                        end if
                    else
                        str127(j) = tSubject
                    end if
                end if
            end do
        else
            do j=Subject(crse)%lenPreq,1,-1
                tSubject = Subject(Subject(crse)%Prerequisite(j))%Name
                if (tSubject=='AND' .or. tSubject=='OR') then
                    if (j/=1) then
                        str127(j) = '('//trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))//')'
                        str127(j+1) = str127(j+3)
                        str127(j+2) = SPACE
                    else
                        str127(j) = trim(str127(j+2))//SPACE//trim(tSubject)//SPACE//trim(str127(j+1))
                    end if
                else
                    str127(j) = tSubject
                end if
            end do
        end if
        if (str127(1)/=SPACE) then
            if (displayStr/=' ') then
                displayStr = trim(str127(1))//'. '//displayStr
            else
                displayStr = str127(1)
            end if
        end if
        text_prerequisite_in_curriculum = displayStr

    end function text_prerequisite_in_curriculum



    function is_prerequisite_satisfiable_in_curriculum(crse, curr)
        logical :: is_prerequisite_satisfiable_in_curriculum
        integer, intent (in) :: crse, curr
        integer, dimension(0:MAX_ALL_SUBJECT_PREREQ) :: tmpPreq
        integer :: ntokens, k, j, jdx, l
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject

        ! evaluate prerequisite
        tmpPreq = 0
        ntokens = Subject(crse)%lenPreq
        tmpPreq(1:ntokens) = Subject(crse)%Prerequisite(1:ntokens)
        if (crse<0) then
            tmpPreq(1) = 1
            ntokens = 0
        end if
        l = index_of_subject_in_curriculum(Curriculum(curr),crse)
        do j = ntokens,1,-1
            jdx = tmpPreq(j)
            tSubject = Subject(jdx)%Name
            if (jdx>0) then ! a named subject
                k = index_of_subject_in_curriculum(Curriculum(curr), jdx)
                if (k>0) then ! found in curriculum
                    ! check if taken earlier
                    if (Curriculum(curr)%SubjectTerm(k)<Curriculum(curr)%SubjectTerm(l)) then
                        tmpPreq(j) = 1
                    else
                        tmpPreq(j) = 0
                    end if
                else
                    tmpPreq(j) = 0
                end if
            else
                if (tSubject=='OR') then
                    tmpPreq(j) = tmpPreq(j+1) + tmpPreq(j+2)
                    tmpPreq(j+1) = 0
                    tmpPreq(j+2) = 0
                    do k=j+3,ntokens
                        tmpPreq(k-2) = tmpPreq(k)
                    end do
                else if (tSubject=='AND') then
                    tmpPreq(j) = tmpPreq(j+1) * tmpPreq(j+2)
                    tmpPreq(j+1) = 0
                    tmpPreq(j+2) = 0
                    do k=j+3,ntokens
                        tmpPreq(k-2) = tmpPreq(k)
                    end do
                else ! any other dummy subject
                    tmpPreq(j) = 1
                end if
            end if
        end do
        is_prerequisite_satisfiable_in_curriculum = tmpPreq(1)>0

    end function is_prerequisite_satisfiable_in_curriculum


    function text_curriculum_info(idxCURR)
        integer, intent (in) :: idxCURR
        character (len=MAX_LEN_CURRICULUM_CODE+MAX_LEN_CURRICULUM_NAME+MAX_LEN_CURRICULUM_NAME+MAX_LEN_CURRICULUM_NAME) :: tmp, &
            text_curriculum_info
        tmp = SPACE
        if (Curriculum(idxCURR)%Remark/=SPACE) &
            tmp = COMMA//SPACE//trim(Curriculum(idxCURR)%Remark)//tmp
        if (Curriculum(idxCURR)%Specialization/=SPACE)  &
            tmp = COMMA//SPACE//trim(Curriculum(idxCURR)%Specialization)//tmp
        tmp = trim(Curriculum(idxCURR)%Code)//SPACE//DASH//SPACE// &
            trim(Curriculum(idxCURR)%Title)//tmp
        text_curriculum_info = tmp

    end function text_curriculum_info

    subroutine set_term_offered_accg_to_curricula(Offering)
        ! reset Subject()%TermOffered based on appearance of subject in the curricular programs
        type (TYPE_OFFERED_SUBJECTS), intent(out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        integer :: kdx, reqd, curr, subj

        Offering = TYPE_OFFERED_SUBJECTS (0, 0, 0, 0, 0, 0, 0, 0)
        do curr=1,NumCurricula
            do kdx=1,Curriculum(curr)%NSubjects
                subj = Curriculum(curr)%SubjectIdx(kdx)
                select case(mod(Curriculum(curr)%SubjectTerm(kdx),3))
                    case (0) ! required during summer
                        Offering(subj)%Demand = Offering(subj)%Demand + 1
                    case (1) ! required during first sem
                        Offering(subj)%NSections = Offering(subj)%NSections + 1
                    case (2) ! required during second sem
                        Offering(subj)%TotalSlots = Offering(subj)%TotalSlots + 1
                end select
            end do
        end do
        do subj=1,NumSubjects
            reqd = 0
            if (Offering(subj)%Demand>0) reqd = reqd + 4 ! summer
            if (Offering(subj)%NSections>0) reqd = reqd + 1 ! first sem
            if (Offering(subj)%TotalSlots >0) reqd = reqd + 2 ! second sem

            if (reqd==0) then ! not required in any curriculum
                !write(*,*) trim(Subject(subj)%Name)//' is not required in any curriculum?'
                Subject(subj)%TermOffered = 3 ! 1,2
            else if (reqd/=Subject(subj)%TermOffered) then ! not consistent
                !write(*,*) trim(Subject(subj)%Name)//' is required '// &
                !  text_term_offered(reqd)//' but set to be offered '// &
                !  text_term_offered(Subject(subj)%TermOffered)
                Subject(subj)%TermOffered = reqd
            end if
        end do
        do subj=NumDummySubjects,-2
            Subject(subj)%TermOffered = 7
        end do

    end subroutine set_term_offered_accg_to_curricula


!===========================================================
! routines for sections
!===========================================================


    subroutine initialize_section(S)
        type (TYPE_SECTION) :: S
        S = TYPE_SECTION (SPACE, SPACE, & ! SPACE,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    end subroutine initialize_section


    function index_to_section(tSection, NumSections, Section)
        integer :: index_to_section
        character (len=MAX_LEN_CLASS_ID), intent (in) :: tSection
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumSections
        integer :: i, sdx
        sdx = 0
        do i=1,NumSections
            if (tSection/=Section(i)%ClassId) cycle
            sdx = i
            exit
        end do
        index_to_section = sdx

    end function index_to_section


    function is_lecture_class(sect, Section)
        integer, intent (in) :: sect
        type (TYPE_SECTION), intent(in) :: Section(0:)
        logical :: is_lecture_class
        ! returns true if sect is a lecture class (no DASH in section code)
        is_lecture_class = index(Section(sect)%Code,DASH)==0 .or. &
           Subject(Section(sect)%SubjectIdx)%Name(1:3)=='PE '

    end function is_lecture_class


    function text_days_of_section(sect, NumSections, Section)
        integer, intent(in) :: sect
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumSections
        character (len=7) :: line, text_days_of_section
        integer :: j
        if (Section(sect)%NMeets>0) then
            line = SPACE
            do j=Section(sect)%NMeets,1,-1
                select case (Section(sect)%DayIdx(j))
                    case (0)
                        line = 'TBA'
                    case (1)
                        line = 'M'//line
                    case (2)
                        line = 'T'//line
                    case (3)
                        line = 'W'//line
                    case (4)
                        line = 'Th'//line
                    case (5)
                        line = 'F'//line
                    case (6)
                        line = 'S'//line
                end select
            end do
        else
            line = 'TBA'
        end if
        text_days_of_section = line

    end function text_days_of_section


    subroutine offerings_sort(NumSections, Section)
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        integer, intent (in out) :: NumSections
        integer :: i, j
        do i=1,NumSections-1
            do j=i+1,NumSections
                if (Section(i)%ClassId>Section(j)%ClassId) then
                    Section(0) = Section(i)
                    Section(i) = Section(j)
                    Section(j) = Section(0)
                end if
            end do
        end do
        call initialize_section(Section(0))

    end subroutine offerings_sort


    subroutine offerings_summarize(NumSections, Section, Offering, DeptIdx)
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumSections
        type (TYPE_OFFERED_SUBJECTS), intent(out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        integer, intent (in), optional :: DeptIdx
        integer :: k, l, filter_dept

        if (present(DeptIdx)) then
            filter_dept = DeptIdx
        else
            filter_dept = 0
        end if
        Offering = TYPE_OFFERED_SUBJECTS (0, 0, 0, 0, 0, 0, 0, 0)
        do k=1,NumSections
            if (index(Section(k)%Code,'+')>0) cycle ! an additional schedule
            l = Section(k)%SubjectIdx
            if (l==0) cycle ! section was deleted
            if (filter_dept>0) then
                if (filter_dept/=Section(k)%DeptIdx) cycle
            end if
            ! lecture-lab ?
            if (.not. is_lecture_lab_subject(l)) then ! lecture only or lab only
                Offering(l)%TotalSlots = Offering(l)%TotalSlots + Section(k)%Slots
                Offering(l)%NSections = Offering(l)%NSections + 1
            else ! a lecture-lab subject
                if (is_lecture_class(k, Section)) then ! this is the lecture section
                else ! this is the lab section
                    Offering(l)%TotalSlots = Offering(l)%TotalSlots + Section(k)%Slots
                    Offering(l)%NSections = Offering(l)%NSections + 1
                end if
            end if
        end do

    end subroutine offerings_summarize


    subroutine count_sections_by_dept(Term, NumSections, Section)
        integer, intent (in) :: Term, NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer :: sect, dept
        ScheduleCount(Term,:) = 0
#if defined UPLB
        ! Subject administered by departments
        do sect=1,NumSections
            if (Section(sect)%SubjectIdx==0) cycle
            dept = Section(sect)%DeptIdx
            ScheduleCount(Term,dept) = ScheduleCount(Term,dept) + 1
        end do
#else
        ! Subjects administered by program
        do sect=1,NumSections
            if (Section(sect)%SubjectIdx==0) cycle
            dept = Section(sect)%DeptIdx
            ScheduleCount(Term,dept) = max(atoi(Section(sect)%Code(2:)), ScheduleCount(Term,dept))
        end do
#endif

    end subroutine count_sections_by_dept


    subroutine delete_sections_of_dept(NumSections, Section, DeptIdx)
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        integer, intent (in) :: DeptIdx
        integer :: sect

        do sect=1,NumSections
            if (Section(sect)%SubjectIdx==0) cycle
            if (DeptIdx==Section(sect)%DeptIdx) then
                call initialize_section(Section(sect))
            end if
        end do

    end subroutine delete_sections_of_dept


    function is_regular_schedule(sect, Section)
        ! returns true if section meetings have same (time, room, teacher), different days
        integer, intent (in) :: sect
        type (TYPE_SECTION), intent(in) :: Section(0:)
        logical :: is_regular_schedule
        logical :: sameTeacher, sameRoom, sameTime
        integer :: mdx

        sameTime = .true.
        sameRoom = .true.
        sameTeacher = .true.
        do mdx=2,Section(sect)%NMeets
          if (Section(sect)%bTimeIdx(1)/=Section(sect)%bTimeIdx(mdx) .or. &
              Section(sect)%eTimeIdx(1)/=Section(sect)%eTimeIdx(mdx)) sameTime = .false.
          if (Section(sect)%RoomIdx(1)/=Section(sect)%RoomIdx(mdx)) sameRoom = .false.
          if (Section(sect)%TeacherIdx(1)/=Section(sect)%TeacherIdx(mdx)) sameTeacher = .false.
        end do
        is_regular_schedule = sameTime .and. sameRoom .and. sameTeacher


    end function is_regular_schedule


    function is_TBA_day_or_hours(tSection)
        ! returns true if section days or hours are not specified
        type (TYPE_SECTION), intent(in) :: tSection
        logical :: is_TBA_day_or_hours

        is_TBA_day_or_hours = tSection%DayIdx(1)==0 .or. &
            tSection%bTimeIdx(1)==0 .or. tSection%eTimeIdx(1)==0

    end function is_TBA_day_or_hours


    function get_next_lab_section(sect, NumSections, Section)
        ! return code for next lab section of sect
        integer, intent (in) :: sect, NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
        character(len=MAX_LEN_SECTION_CODE) :: get_next_lab_section
        character(len=MAX_LEN_CLASS_ID) :: tClassId

        integer :: i
        logical :: found
        i = 0
        found = .true.
        do while (found)
            i = i + 1
            tClassId = trim(Section(sect)%ClassID)//DASH//trim(itoa(i))//'L'
            found = index_to_section(tClassId, NumSections, Section)>0
        end do
        get_next_lab_section = trim(Section(sect)%Code)//DASH//trim(itoa(i))//'L'

    end function get_next_lab_section


!===========================================================
! routines for blocks
!===========================================================


    subroutine initialize_block(B)
        type (TYPE_BLOCK) :: B
        B = TYPE_BLOCK ('Block Code', 'Block Name and Description', 0, 0, 0, 0, 0, 0, 0, 0)

    end subroutine initialize_block


    function index_to_block(tBlock, NumBlocks, Block)
        integer :: index_to_block
        character (len=MAX_LEN_BLOCK_CODE), intent (in) :: tBlock
        integer, intent(in) :: NumBlocks
        type (TYPE_BLOCK), dimension(0:), intent(in) :: Block
        integer :: i, sdx

        sdx = 0
        do i=1,NumBlocks
            if (tBlock==Block(i)%BlockID) then
                sdx = i
                exit
            end if
        end do
        index_to_block = sdx

    end function index_to_block


    subroutine sort_alphabetical_blocks(NumBlocks, Block)
        integer, intent(in) :: NumBlocks
        type (TYPE_BLOCK), dimension(0:), intent(in out) :: Block
        integer :: idx, jdx, kdx
        ! sort blocks
        kdx = 0
        do jdx=1,NumBlocks-1
            do idx=jdx+1,NumBlocks
                if (Block(jdx)%BlockID>Block(idx)%BlockID) then
                    Block(kdx) = Block(idx)
                    Block(idx) = Block(jdx)
                    Block(jdx) = Block(kdx)
                end if
            end do
        end do
        call initialize_block(Block(kdx))

    end subroutine sort_alphabetical_blocks


    subroutine delete_blocks_from_dept(NumBlocks, Block,  DeptIdx)
        integer, intent(in out) :: NumBlocks
        type (TYPE_BLOCK), dimension(0:), intent(in out) :: Block
        integer, intent (in) :: DeptIdx
        integer :: blk, idx

        !write(*,*) 'Removing blocks in '//Department(DeptIdx)%Code ! , ', total=',  NumBlocks
        do blk=1,NumBlocks
            if (DeptIdx==Block(blk)%DeptIdx) then
                !write(*,*) 'Removing block '//Block(blk)%BlockID
                call initialize_block(Block(blk))
            end if
        end do
        blk = 0
        do idx=1,NumBlocks
            if (Block(idx)%CurriculumIdx/=0) then
                blk = blk+1
                Block(blk) = Block(idx)
            end if
        end do
        NumBlocks = blk
        !write(*,*) NumBlocks, ' left'

    end subroutine delete_blocks_from_dept


    subroutine delete_section_from_blocks(sect, NumSections, Section, NumBlocks, Block)
        integer, intent(in) :: sect, NumBlocks
        type (TYPE_BLOCK), dimension(0:), intent(in out) :: Block
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        integer, intent (in out) :: NumSections
        integer :: i, j
        do i=1,NumBlocks
            do j=1,Block(i)%NumClasses
                if (Block(i)%Section(j)/=sect) cycle
                Block(i)%Section(j) = 0
                exit
            end do
        end do
        call initialize_section(Section(sect))
        !integer :: pos
        !do pos=sect,NumSections-1
        !  Section(pos) = Section(pos+1)
        !end do
        !call initialize_section(Section(NumSections))
        !NumSections = NumSections - 1

    end subroutine delete_section_from_blocks


!===========================================================
! routines for timetables
!===========================================================


    subroutine timetable_clear(TimeTable)
        integer, dimension (60,6), intent(out) :: TimeTable
        TimeTable = 0
        TimeTable(59,1:6) = 60 ! earliest time
        TimeTable(60,1:6) = 1  ! latest time

    end subroutine timetable_clear


    function is_conflict_timetable_with_section(NumSections, Section, sect, TimeTable)
        logical :: is_conflict_timetable_with_section
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumSections
        integer, intent (in) :: sect
        integer, dimension(60,6), intent (in) :: TimeTable
        integer :: idx, jdx, mdx
        !
        is_conflict_timetable_with_section = .false.
        if (sect>0) then ! a valid sect pointer
            loop_meets : &
            do mdx=1,Section(sect)%NMeets
                jdx = Section(sect)%DayIdx(mdx)
                if (jdx==0) cycle
                do idx = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
                    if (TimeTable(idx, jdx)/=0) then
                        is_conflict_timetable_with_section = .true.
                        exit loop_meets
                    end if
                end do
            end do loop_meets
        end if

    end function is_conflict_timetable_with_section


    subroutine timetable_add_section(NumSections, Section, sect, TimeTable, loc)
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumSections
        integer, intent (in) :: sect
        integer, dimension (60,6), intent (in out) :: TimeTable
        integer, intent (in out) :: loc
        integer :: idx, jdx, mdx
        integer, dimension (60,6) :: tTable
        !
        if (sect>0) then ! a valid sect pointer
            tTable = TimeTable
            do mdx=1,Section(sect)%NMeets
                jdx = Section(sect)%DayIdx(mdx)
                if (jdx==0) cycle
                do idx = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
                    if (tTable(idx, jdx)/=0) then
                        !           write(*,*)  'Conflict detected in timetable_add_section(); called from', loc
                        loc = tTable(idx,jdx)
                        return
                    else
                        tTable(idx,jdx) =  sect
                    end if
                end do
            end do
            ! nothing unusual
            TimeTable = tTable
            do mdx=1,Section(sect)%NMeets
                jdx = Section(sect)%DayIdx(mdx)
                if (jdx==0) cycle
                !do idx = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
                !  TimeTable(idx, jdx) = sect
                !end do
                if (TimeTable(59,jdx)>Section(sect)%bTimeIdx(mdx)) then
                    TimeTable(59,jdx) = Section(sect)%bTimeIdx(mdx)
                end if
                if (TimeTable(60,jdx)<Section(sect)%eTimeIdx(mdx)) then
                    TimeTable(60,jdx) = Section(sect)%eTimeIdx(mdx)
                end if
            end do
        else if (sect<0) then
            call log_comment('Invalid section '//itoa(sect)//' in timetable_add_section(); called from '//itoa(loc))
        end if

    end subroutine timetable_add_section


    function is_conflict_timetable_with_section_meetings(NumSections, Section, sect, n_meetings, meetings, TimeTable)
        logical :: is_conflict_timetable_with_section_meetings
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumSections
        integer, intent (in) :: sect, n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer, dimension(60,6), intent (in) :: TimeTable
        integer :: idx, jdx, kdx, mdx
        !
        is_conflict_timetable_with_section_meetings = .false.
        if (sect>0) then ! a valid sect pointer
            loop_meets : &
            do kdx=1,n_meetings
                mdx=meetings(kdx)
                jdx = Section(sect)%DayIdx(mdx)
                if (jdx==0) cycle
                do idx = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
                    if (TimeTable(idx, jdx)/=0) then
                        is_conflict_timetable_with_section_meetings = .true.
                        exit loop_meets
                    end if
                end do
            end do loop_meets
        end if

    end function is_conflict_timetable_with_section_meetings


    subroutine timetable_add_meetings_of_section(NumSections, Section, sect, n_meetings, meetings, TimeTable, loc)
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumSections
        integer, intent (in) :: sect, n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer, intent (in out) :: TimeTable(60,6), loc
        integer :: idx, jdx, kdx, mdx
        integer :: tTable(60,6)
        !
        tTable = TimeTable
        do kdx=1,n_meetings
            mdx = meetings(kdx)
            jdx = Section(sect)%DayIdx(mdx)
            if (jdx==0) cycle
            do idx = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
                if (tTable(idx, jdx)==0) then
                    tTable(idx,jdx) = sect
                else
                    loc = tTable(idx,jdx)
                    return ! do not add to TimeTable()
                end if
            end do
        end do
        ! nothing unusual
        TimeTable = tTable
        do kdx=1,n_meetings
            mdx = meetings(kdx)
            jdx = Section(sect)%DayIdx(mdx)
            if (jdx==0) cycle
            !do idx = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
            !  TimeTable(idx, jdx) = sect
            !end do
            if (TimeTable(59,jdx)>Section(sect)%bTimeIdx(mdx)) then
                TimeTable(59,jdx) = Section(sect)%bTimeIdx(mdx)
            end if
            if (TimeTable(60,jdx)<Section(sect)%eTimeIdx(mdx)) then
                TimeTable(60,jdx) = Section(sect)%eTimeIdx(mdx)
            end if
        end do

    end subroutine timetable_add_meetings_of_section


    function is_conflict_timetable_with_struct_section(Section, m_first, m_last, TimeTable)
        logical :: is_conflict_timetable_with_struct_section
        type (TYPE_SECTION), intent (in) :: Section
        integer, intent (in) :: m_first, m_last
        integer, dimension(60,6), intent (in) :: TimeTable
        integer :: idx, jdx, mdx
        !
        is_conflict_timetable_with_struct_section = .false.
        loop_meets : &
        do mdx=m_first,m_last
            jdx = Section%DayIdx(mdx)
            if (jdx==0) cycle
            do idx = Section%bTimeIdx(mdx), Section%eTimeIdx(mdx)-1
                if (TimeTable(idx, jdx)/=0) then
                    is_conflict_timetable_with_struct_section = .true.
                    exit loop_meets
                end if
            end do
        end do loop_meets

    end function is_conflict_timetable_with_struct_section


    subroutine timetable_add_struct_section(Section, TimeTable, loc)
        type (TYPE_SECTION), intent (in) :: Section
        integer, dimension (60,6), intent (in out) :: TimeTable
        integer, intent (in out) :: loc
        integer :: idx, jdx, mdx
        integer, dimension (60,6) :: tTable
        !
        tTable = TimeTable
        do mdx=1,Section%NMeets
            jdx = Section%DayIdx(mdx)
            if (jdx==0) cycle
            do idx = Section%bTimeIdx(mdx), Section%eTimeIdx(mdx)-1
                if (tTable(idx, jdx)==0) then
                    tTable(idx, jdx) = mdx
                else! conflict detected
                    loc = tTable(idx,jdx)
                    return
                end if
            end do
        end do
        TimeTable = tTable
        ! nothing unusual
        do mdx=1,Section%NMeets
            jdx = Section%DayIdx(mdx)
            if (jdx==0) cycle
            if (TimeTable(59,jdx)>Section%bTimeIdx(mdx)) then
                TimeTable(59,jdx) = Section%bTimeIdx(mdx)
            end if
            if (TimeTable(60,jdx)<Section%eTimeIdx(mdx)) then
                TimeTable(60,jdx) = Section%eTimeIdx(mdx)
            end if
        end do

    end subroutine timetable_add_struct_section


    subroutine timetable_remove_section(NumSections, Section, sect, TimeTable, loc)
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumSections
        integer, intent (in) :: sect
        integer, dimension (60,6), intent (in out) :: TimeTable
        integer, intent (in out) :: loc
        integer :: idx, jdx, mdx
        !
        if (sect>0) then ! a valid sect pointer
            do mdx=1,Section(sect)%NMeets
                jdx = Section(sect)%DayIdx(mdx)
                if (jdx==0) cycle
                do idx = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
                    if (TimeTable(idx, jdx)/=sect) then
                        call log_comment('ERROR detected in timetable_remove_section(); called from '//itoa(loc))
                        loc = TimeTable(idx,jdx)
                        return
                    end if
                end do
            end do
            ! nothing unusual
            do mdx=1,Section(sect)%NMeets
                jdx = Section(sect)%DayIdx(mdx)
                if (jdx==0) cycle
                do idx = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
                    TimeTable(idx, jdx) = 0
                end do
                ! reset earliest time
                idx = 1
                do while (TimeTable(idx,jdx)==0 .and. idx<60)
                    idx = idx+1
                end do
                TimeTable(59,jdx) = idx
                ! reset latest time
                idx = 57
                do while (TimeTable(idx,jdx)==0 .and. idx>1)
                    idx = idx-1
                end do
                TimeTable(60,jdx) = idx
            end do
        end if

    end subroutine timetable_remove_section


    function is_consistent_section_hours_with_subject_defn(tSection)
        type (TYPE_SECTION), intent (in) :: tSection
        logical :: is_consistent_section_hours_with_subject_defn
        integer :: idx
        real :: n15, SectionHours

        is_consistent_section_hours_with_subject_defn = .true.
        n15 = 0.0 ! no. of 15-minute intervals
        do idx=1,tSection%NMeets
            n15 = n15 + tSection%eTimeIdx(idx) - tSection%bTimeIdx(idx)
        end do
        if (n15==0.0) return ! assume TBA is OK
        ! disable check for summer schedules
        if (targetTerm==3) return

        ! figure out how many hours based on subject type and section code
        idx = tSection%SubjectIdx
        if (is_lecture_lab_subject(idx)) then
            if (index(tSection%ClassId,DASH)==0) then
                SectionHours = Subject(idx)%LectHours
            else
                SectionHours = Subject(idx)%LabHours
            end if
        else if (Subject(idx)%LectHours > 0) then
            SectionHours = Subject(idx)%LectHours
        else ! if (Subject(idx)%LabHours > 0) then
            SectionHours = Subject(idx)%LabHours
        end if
        is_consistent_section_hours_with_subject_defn = 4.0*SectionHours==n15

    end function is_consistent_section_hours_with_subject_defn


    function is_conflict_free_section_hours(tSection, NumSections, Section)
        logical :: is_conflict_free_section_hours, tDetermination
        type (TYPE_SECTION), intent (in) :: tSection
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumSections
        integer, dimension(60,6) :: TimeTable
        character(len=MAX_LEN_CLASS_ID) :: tClassId
        integer :: i, j

        tDetermination = .true.
        ! meeting conflicts?
        call timetable_clear(TimeTable)
        i = -10
        call timetable_add_struct_section(tSection, TimeTable, i)
        if ( i/=-10) then
            tDetermination = .false.
        else ! lab section conflicts with lecture section?
            if (is_lecture_lab_subject(tSection%SubjectIdx)) then ! subject is lecture-lab
                ! a lab section ?
                i = index(tSection%ClassId,DASH)
                if (i>0) then
                    ! find lecture section
                    tClassId = tSection%ClassId(:i-1)
                    j = index_to_section(tClassId, NumSections, Section)
                    if (j>0) then
                        if (is_conflict_timetable_with_section(NumSections, Section, j, TimeTable)) tDetermination = .false.
                    end if
                end if
            end if
        end if
        is_conflict_free_section_hours = tDetermination

    end function is_conflict_free_section_hours


    subroutine sections_compound(NumSections, Section, nconflicts, ignoreMismatch)
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        integer, intent (in out) :: NumSections
        integer, intent (out) :: nconflicts
        logical, intent (in), optional :: ignoreMismatch
#ifdef DEBUG
        character (len=255) :: line
#endif
        !character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_CLASS_ID) :: tSection
        integer :: crse, sect, sdx, i, j, idx, n15, tHours
        integer :: TimeTable(60,6)
        logical :: ignore

        if (present(ignoreMismatch)) then
            ignore = ignoreMismatch
        else
            ignore = .false.
        end if

        nconflicts = 0
        !
        ! add lecture times to laboratory section
        do sect=1,NumSections
            crse = Section(sect)%SubjectIdx
            if (crse<0) cycle ! done previously
            !if (Subject(crse)%LectHours * Subject(crse)%LabHours==0) cycle
            if (.not. is_lecture_lab_subject(crse)) cycle
            ! subject is lecture-lab
            if (is_lecture_class(sect, Section)) cycle ! a lecture section

            !write(*,*) 'Laboratory class '//Section(sect)%ClassId
            call timetable_clear(TimeTable)
            idx = -10
            call timetable_add_section(NumSections, Section, sect, TimeTable, idx)
            if ( idx/=-10) then
                nconflicts = nconflicts + 1
                call log_comment(Section(sect)%ClassId//' conflicts with '//Section(idx)%ClassId)
            end if
            !
            !tSubject = Subject(crse)%Name
            !tSection = trim(tSubject)//SPACE//Section(sect)%Code(:j-1)
            tSection = Section(sect)%ClassId
            j = len_trim(tSection)
            do while (tSection(j:j)/=DASH)
                j = j-1
            end do
            tSection(j:) = SPACE
            sdx = index_to_section(tSection, NumSections, Section)
            !write(*,*) 'lecture class '//tSection, sdx
            if (sdx>0) then ! lecture found
                idx = -10
                call timetable_add_section(NumSections, Section, sdx, TimeTable, idx)
                if ( idx/=-10) then
                    nconflicts = nconflicts + 1
                    call log_comment(Section(sdx)%ClassId//' conflicts with '//Section(idx)%ClassId)
                end if
                j = Section(sect)%NMeets
                Section(sect)%NMeets = j + Section(sdx)%NMeets
                do i=1,Section(sdx)%NMeets
                    Section(sect)%DayIdx(j+i) = Section(sdx)%DayIdx(i)
                    Section(sect)%bTimeIdx(j+i) = Section(sdx)%bTimeIdx(i)
                    Section(sect)%eTimeIdx(j+i) = Section(sdx)%eTimeIdx(i)
                    Section(sect)%RoomIdx(j+i) = Section(sdx)%RoomIdx(i)
                end do
            else
                nconflicts = nconflicts + 1
                call log_comment('Lecture class '//tSection//'not found!')
            end if
        end do
        !
        ! "erase" lecture times of lecture-laboratory subjects
        do sect=1,NumSections
            crse = Section(sect)%SubjectIdx
            if (crse<0) cycle ! done previously
            !if (Subject(crse)%LectHours * Subject(crse)%LabHours==0) cycle
            if (.not. is_lecture_lab_subject(crse)) cycle
            ! subject is lecture-lab
            j = index(Section(sect)%Code,DASH)
            if (j==0) then
                Section(sect)%SubjectIdx = -(crse-NumDummySubjects)
            ! a lecture section
            end if
        end do
        !
        sect = 0
        call initialize_section(Section(sect))
        do sdx=1,NumSections
            crse = Section(sdx)%SubjectIdx
            if (crse<0) then
#ifdef DEBUG
                crse = -(crse-NumDummySubjects)
                line = '#'
#endif
            else
#ifdef DEBUG
                line = SPACE
#endif
                sect = sect+1
                Section(sect) = Section(sdx)
            end if
#ifdef DEBUG
            line = trim(line)//trim(Section(sdx)%ClassId)//COMMA//itoa(Section(sdx)%Slots)
            do i=1,Section(sdx)%NMeets
                line = trim(line)//COMMA// &
                    trim(text_time_period(Section(sdx)%bTimeIdx(i), Section(sdx)%eTimeIdx(i)))//COMMA// &
                    trim(txtDay(Section(sdx)%DayIdx(i)))//COMMA// &
                    Room(Section(sdx)%RoomIdx(i))%Code
            end do
            write(*,*) trim(line)
#endif
        end do
        NumSections = sect
        call initialize_section(Section(sect+1))
        ! check consistency of hours
        do sdx=1,NumSections
            crse = Section(sdx)%SubjectIdx
            if (crse<=0) cycle
            tHours = Subject(crse)%LectHours+Subject(crse)%LabHours
            n15 = 0
            do i=1,Section(sdx)%NMeets
                n15 = n15 + Section(sdx)%eTimeIdx(i) - Section(sdx)%bTimeIdx(i)
            end do
            if (n15 .gt. 0 .and. 4*tHours .ne. n15) then
                call log_comment(Section(sdx)%ClassId//': scheduled hours ('// &
                    trim(itoa(n15/4))//') is inconsistent with subject parameter hours ('//trim(itoa(tHours))//')')
                if (.not. ignore) nconflicts = nconflicts + 1
            end if
        end do

    end subroutine sections_compound


    subroutine meetings_of_section_by_teacher(NumSections, Section, section_index, teacher_index, n_meetings, meetings)
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumSections
        integer, intent(in) :: section_index, teacher_index
        integer, intent(out) :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer :: i
        n_meetings = 0
        meetings = 0
        do i=1,Section(section_index)%NMeets
            if (Section(section_index)%TeacherIdx(i)==teacher_index) then
                n_meetings = 1+n_meetings
                meetings(n_meetings) = i
            end if
        end do

    end subroutine meetings_of_section_by_teacher


    subroutine meetings_of_section_in_room(NumSections, Section, section_index, room_index, n_meetings, meetings)
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumSections
        integer, intent(in) :: section_index, room_index
        integer, intent(out) :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer :: i
        n_meetings = 0
        meetings = 0
        do i=1,Section(section_index)%NMeets
            if (Section(section_index)%RoomIdx(i)==room_index) then
                n_meetings = 1+n_meetings
                meetings(n_meetings) = i
            end if
        end do

    end subroutine meetings_of_section_in_room


    subroutine timetable_meetings_of_teacher(NumSections, Section, teacher_index, to_skip, len_list, list, TimeTable, conflicted)
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumSections
        integer, intent(in) :: teacher_index, to_skip
        integer, intent (out) :: len_list, list(:)
        integer, dimension(60,6), intent (out) :: TimeTable
        logical, intent(out) :: conflicted
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer :: idx, conflict_loc, sdx

        len_list = 0 ! initialize list
        call timetable_clear(TimeTable) ! initialize weekly schedule
        conflicted = .false.
        do sdx=1,NumSections ! loop over all sections
            if (sdx==to_skip) cycle ! skip specified section (if, for example, the section is being edited)
            if (Section(sdx)%SubjectIdx==0) cycle ! section ws deleted
            call meetings_of_section_by_teacher(NumSections, Section, sdx, teacher_index, n_meetings, meetings) ! collect meetings assigned to teacher
            if (n_meetings==0) cycle ! teacher not assigned to this section
            do idx=1,n_meetings ! loop over meetings for this section
                list(len_list+1) = sdx ! add index to Section() to list of meetings for teacher
                list(len_list+2) = meetings(idx) ! index to Section(sdx)%DayIdx(), etc
                conflict_loc = -10 ! assume no conflicting section
                call timetable_add_meetings_of_section(NumSections, Section, sdx, 1, meetings(idx), TimeTable, conflict_loc) ! add meeting to weekly schedule
                if (conflict_loc/=-10) then
                    conflicted = .true.
                else
                    conflict_loc = 0
                end if
                list(len_list+3) = conflict_loc ! index to conflicting section, if any
                len_list = len_list+3
            end do
        end do
        ! end markers
        list(len_list+1) = 0
        list(len_list+2) = 0
        list(len_list+3) = 0

    end subroutine timetable_meetings_of_teacher


    subroutine timetable_meetings_in_room(NumSections, Section, room_index, to_skip, len_list, list, TimeTable, conflicted)
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumSections
        integer, intent(in) :: room_index, to_skip
        integer, intent (out) :: len_list, list(:)
        integer, dimension(60,6), intent (out) :: TimeTable
        logical, intent(out) :: conflicted
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer :: idx, conflict_loc, sdx
        len_list = 0 ! initialize list
        call timetable_clear(TimeTable) ! initialize weekly schedule
        conflicted = .false.
        do sdx=1,NumSections ! loop over all sections
            if (sdx==to_skip) cycle ! skip specified section (if, for example, the section is being edited)
            if (Section(sdx)%SubjectIdx==0) cycle ! section ws deleted
            call meetings_of_section_in_room(NumSections, Section, sdx, room_index, n_meetings, meetings) ! collect meetings assigned to room
            if (n_meetings==0) cycle ! room not assigned to this section
            do idx=1,n_meetings ! loop over meetings for this section
                list(len_list+1) = sdx ! add index to Section() to list of meetings in room
                list(len_list+2) = meetings(idx) ! index to Section(sdx)%DayIdx(), etc
                conflict_loc = -10 ! assume no conflicting section
                call timetable_add_meetings_of_section(NumSections, Section, sdx, 1, meetings(idx), TimeTable, conflict_loc) ! add meeting to weekly schedule
                if (conflict_loc/=-10) then
                    conflicted = .true.
                else
                    conflict_loc = 0
                end if
                list(len_list+3) = conflict_loc ! index to conflicting section, if any
                len_list = len_list+3
            end do
        end do
        ! end markers
        list(len_list+1) = 0
        list(len_list+2) = 0
        list(len_list+3) = 0

    end subroutine timetable_meetings_in_room


    subroutine timetable_undesirability(ns, NumSections, Section, TimeTable)
        integer, intent(in) :: ns
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumSections
        integer, dimension(60,6), intent(in out)  :: TimeTable

        integer :: day_idx, mdx, sect, tSect, tmdx
        integer :: i, j, k
        logical :: TimeBlockFree

        ! initialize earliest times, latest times, undesirability indices
        LongDays = 0
        HungerDays = 0
        EarlyDays = 0
        LateDays = 0
        Travels = 0
        Undesirability = 0

        do sect=NumSections+1,NumSections+ns
            i = sect-NumSections

            !write(*,*) i, sect, Section(sect)%ClassId
            do mdx=1,Section(sect)%NMeets
                day_idx = Section(sect)%DayIdx(mdx)
                if (day_idx==0) cycle
                ! need to travel?
                ! previous class is
                tsect = 0
                if (Section(sect)%bTimeIdx(mdx) > 1) then
                    tsect = TimeTable(Section(sect)%bTimeIdx(mdx)-1, day_idx)
                end if
                if (tsect > 0) then
                    do tmdx=1,Section(tsect)%NMeets ! find the room
                        if (day_idx == Section(tsect)%DayIdx(tmdx) .and. & ! same day
                        Section(sect)%bTimeIdx(mdx) == Section(tsect)%eTimeIdx(tmdx) ) then ! previous class
                            if (Room(Section(tsect)%RoomIdx(tmdx))%Cluster /=  &
                            Room(Section(sect)%RoomIdx(mdx))%Cluster ) &
                            Travels(i) = Travels(i) + 1
                            !write(*,*) 'Travel from '//Room(Section(tsect)%RoomIdx(tmdx))%Code, &
                            !  Room(Section(sect)%RoomIdx(mdx))%Code
                        end if
                    end do
                end if
                ! next class is
                tsect = 0
                if (Section(sect)%eTimeIdx(mdx) < 58) then
                    tsect = TimeTable(Section(sect)%eTimeIdx(mdx)+1, day_idx)
                end if
                if (tsect > 0) then
                    do tmdx=1,Section(tsect)%NMeets ! find the room
                        if (day_idx == Section(tsect)%DayIdx(tmdx) .and. & ! same day
                        Section(sect)%eTimeIdx(mdx) == Section(tsect)%bTimeIdx(tmdx) ) then ! next class
                            if (Room(Section(tsect)%RoomIdx(tmdx))%Cluster /=  &
                            Room(Section(sect)%RoomIdx(mdx))%Cluster ) &
                            Travels(i) = Travels(i) + 1
                            !write(*,*) 'Travel from '//Room(Section(tsect)%RoomIdx(tmdx))%Code, &
                            !  Room(Section(sect)%RoomIdx(mdx))%Code
                        end if
                    end do
                end if
                ! how many early days
                if (Section(sect)%bTimeIdx(mdx) < TIME_INDEX_EARLY_DAY) then
                    EarlyDays(i) = EarlyDays(i) + 1
                    !write(*,*) 'Early day '//Section(sect)%ClassId, txtDay(Section(sect)%DayIdx(mdx)), &
                    !  txtTime(Section(sect)%bTimeIdx(mdx))
                    ! long day?
                    if (TimeTable(60,day_idx) >= TIME_INDEX_LATE_DAY) LongDays(i) = LongDays(i) + 1
                end if
                ! how many late days
                if (Section(sect)%eTimeIdx(mdx) > TIME_INDEX_LATE_DAY) then
                    LateDays(i) = LateDays(i) + 1
                    !write(*,*) 'Late day '//Section(sect)%ClassId, txtDay(Section(sect)%DayIdx(mdx)), &
                    !  txtTime(Section(sect)%eTimeIdx(mdx))
                    ! long day?
                    if (TimeTable(59,day_idx) <= TIME_INDEX_EARLY_DAY) LongDays(i) = LongDays(i) + 1
                end if
                ! how many days will it not allow lunch time?
                if (.not. (Section(sect)%eTimeIdx(mdx) <= TIME_INDEX_BEGIN_LUNCH .or. &
                Section(sect)%bTimeIdx(mdx) >= TIME_INDEX_END_LUNCH) ) then
                    ! temporarily add to timetable
                    do k = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
                        TimeTable(k, day_idx) = sect
                    end do
                    ! check if schedule does not allow lunch time
                    TimeBlockFree = .false.
                    do k = TIME_INDEX_BEGIN_LUNCH,TIME_INDEX_END_LUNCH-1
                        if (TimeTable(k, day_idx) == 0) then
                            TimeBlockFree = .true.
                        end if
                    end do
                    if (.not. TimeBlockFree) then
                        HungerDays(i) = HungerDays(i) + 1
                      !write(*,*) 'Hungry day '//Section(sect)%ClassId, txtDay(Section(sect)%DayIdx(mdx))
                    end if
                    ! remove from timetable
                    do k = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
                        TimeTable(k, day_idx) = 0
                    end do
                end if
            end do !  mdx=1,Section(sect)%NMeets

        end do !  sect=NumSections+1,NumSections+ns

        ! compute a single undesirablility index for each option

        do j=1,ns
            UndesirabilityRank(j) = j
            !
            if (LongDays(j) > LongDays(0)) LongDays(0) = LongDays(j)
            if (HungerDays(j) > HungerDays(0)) HungerDays(0) = HungerDays(j)
            if (EarlyDays(j) > EarlyDays(0)) EarlyDays(0) = EarlyDays(j)
            if (LateDays(j) > LateDays(0)) LateDays(0) = LateDays(j)
            if (Travels(j) > Travels(0)) Travels(0) = Travels(j)
        end do
        if (LongDays(0) > 0) then
            do j=1,ns
                Undesirability(j) = Undesirability(j) + (PENALTY_LONG_DAY*LongDays(j))/LongDays(0)
            end do
        end if
        if (HungerDays(0) > 0) then
            do j=1,ns
                Undesirability(j) = Undesirability(j) + (PENALTY_HUNGRY_DAY*HungerDays(j))/HungerDays(0)
            end do
        end if
        if (EarlyDays(0) > 0) then
            do j=1,ns
                Undesirability(j) = Undesirability(j) + (PENALTY_EARLY_DAY*EarlyDays(j))/EarlyDays(0)
            end do
        end if
        if (LateDays(0) > 0) then
            do j=1,ns
                Undesirability(j) = Undesirability(j) + (PENALTY_LATE_DAY*LateDays(j))/LateDays(0)
            end do
        end if
        if (Travels(0) > 0) then
            do j=1,ns
                Undesirability(j) = Undesirability(j) + (PENALTY_TRAVEL_IN_A_HURRY*Travels(j))/Travels(0)
            end do
        end if

        ! sort options according to increasing undesirability index
        do i=1,ns-1
            do j=i,ns
                if ( Undesirability(UndesirabilityRank(i)) > Undesirability(UndesirabilityRank(j)) ) then
                    k = UndesirabilityRank(j)
                    UndesirabilityRank(j) = UndesirabilityRank(i)
                    UndesirabilityRank(i) = k
                end if
            end do
        end do

    end subroutine timetable_undesirability


    subroutine recalculate_available_seats(Section)
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        ! calculate priority demand, priority accomodated/not accommodated
        Section(:)%RemSlots = Section(:)%Slots
    end subroutine recalculate_available_seats


end module UNIVERSITY
