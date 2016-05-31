!======================================================================
!
!    HEEDS (Higher Education Enrollment Decision Support) - A program
!      to create enrollment scenarios for 'next term' in a university
!    Copyright (C) 2012-2015 Ricolindo L. Carino
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

    ! max no. of colleges and schools in University
    integer, parameter :: MAX_ALL_COLLEGES = 25

    ! year that University records usable by HEEDS are available
    integer :: baseYear = 2000

    character (len=80) :: &
        UniversityName = 'Long Name of University', &
        UniversityAddress = 'University Address', &
        UniversityWeb = 'University URL', &
        UniversityPhone = 'University Phone'

    integer, parameter :: MAX_LEN_PERSON_NAME = 50 ! Max length of a person's name
    character (len=MAX_LEN_PERSON_NAME) :: &
        UniversityPresident = 'Fname MI LName', &
        titleUniversityPresident = 'University President', &
        !
        VPAcademicAffairs = 'Fname MI LName', &
        titleVPAcademicAffairs = 'Vice-President for Academic Affairs', &
        !
        DeanOfCampus = 'Fname MI LName', &
        titleDeanOfCampus = 'Dean of Campus', &
        !
        DeanOfInstruction = 'Fname MI LName', &
        titleDeanOfInstruction = 'Dean Of Instruction', &
        !
        TheRegistrar = 'Fname MI LName', &
        titleTheRegistrar = 'University Registrar'

!===========================================================
! declarations for school fees
!===========================================================

    integer, parameter :: MAX_ALL_FEES = 30 ! Max no. of fees
    character (len=MAX_LEN_PERSON_NAME) :: FeeDescription(MAX_ALL_FEES) = SPACE
    real :: FeeAmount(MAX_ALL_FEES) = 0.0

    integer, parameter :: &
        MAX_ALL_ACCOUNTS = 100, & ! Max no. of accounting codes
        MAX_LEN_ACCOUNT_CODE = 12 ! Max length of accounting codes
    character (len=MAX_LEN_ACCOUNT_CODE) :: AccountCode(0:MAX_ALL_ACCOUNTS)
    character (len=MAX_LEN_PERSON_NAME) :: AccountDescription(0:MAX_ALL_ACCOUNTS)
    integer :: NumAccounts

!===========================================================
! declarations for PHILIPPINE STANDARD GEOGRAPHIC CODE (PSGC)
!===========================================================

    integer, parameter :: MAX_ALL_PSGC      = 44032 ! Max no. of PSGC codes
    integer, parameter :: MAX_LEN_PSGC      =     9 ! Length of PSGC
    integer, parameter :: MAX_LEN_PSGC_NAME =    45 ! Length of PSGC name
    integer, parameter :: MAX_ALL_MUNICIPALITIES =    1650 ! how many numicipalities
    integer, parameter :: kindRegion = 3, kindProvince = 5, kindMunicipality = 7 ! where 0s start
    character (len=MAX_LEN_PSGC), parameter :: nullPSGC = '000000000'
    type :: TYPE_PSGC
        character (len=MAX_LEN_PSGC) :: Code  ! RR0000000 - Region, RRPP00000 - Province/District
        character (len=MAX_LEN_PSGC_NAME) :: Name  ! RRPPMM000 - Municipality, RRPPMMBBB - Barangay
    end type TYPE_PSGC
    type (TYPE_PSGC) :: PSGC(0:MAX_ALL_PSGC)
    integer :: NumPSGC = 0, NumRegion = 0, NumProvince = 0, NumMunicipality = 0
    integer :: Region(20), Province(100), Municipality(MAX_ALL_MUNICIPALITIES) ! indices to PSGC()

    integer, dimension (0:MAX_ALL_MUNICIPALITIES, 0:MAX_ALL_COLLEGES) :: PSGCcount


!===========================================================
! declarations for mother tongues
!===========================================================

    integer, parameter :: &
        MAX_ALL_TONGUES = 200, &
        MAX_LEN_CODE_TONGUE = 3, &
        MAX_LEN_TONGUE = 40
    type :: TYPE_TONGUE
        integer :: Rank
        character (len=MAX_LEN_CODE_TONGUE) :: Code  ! xxx
        character (len=MAX_LEN_TONGUE) :: Name
    end type TYPE_TONGUE
    type (TYPE_TONGUE), dimension (0:MAX_ALL_TONGUES) :: Tongue
    integer :: NumTongue


!===========================================================
! declarations for religions
!===========================================================

!    integer, parameter :: &
!        MAX_ALL_RELIGIONS = 40, &
!        MAX_LEN_RELIGION = 65
!    character (len=MAX_LEN_RELIGION), dimension (0:MAX_ALL_RELIGIONS) :: Religion
!    integer :: NumReligions


!===========================================================
! declarations for colleges
!===========================================================

    integer, parameter :: &
        MAX_LEN_COLLEGE_CODE=10, & ! length of college codes
        MAX_LEN_COLLEGE_NAME=60 ! length of college names
    integer, parameter :: AdviseOpenSubjects = 0, &
        ToEditCLASSES  = 1, ToShowTEACHERS  = 2, ToEditCHECKLISTS = 3, ToEnlistStudents   = 4, &
        ToSelfEnlistPE = 5, ToSelfEnlistALL = 6, ToEditCLASSLISTS = 7, ToEvaluateTeachers = 8, &
        ToEditGRADES = 9
    character(len=51) :: AllowedAction(AdviseOpenSubjects:ToEditGRADES) = (/ &
        '<b>Advise subjects with open sections ONLY</b>     ', &
        '<b>Editing of the Schedule of Classes</b> by chairs', &
        '<b>Names of teachers in the Schedule of Classes</b>', &
        '<b>Editing of student checklists</b> by advisers   ', &
        '<b>Enlistment of students</b> by advisers          ', &
        '<b>Self-enlistment in PE</b> (only) by students    ', &
        '<b>Self-enlistment in ALL subjects</b> by students ', &
        '<b>Editing of classlists</b> by teachers           ', &
        '<b>Evaluation of teaching effectiveness</b>        ', &
        '<b>Editing of grades</b> by teachers               ' /)
    type :: TYPE_COLLEGE
        character (len=MAX_LEN_COLLEGE_CODE) :: Code
        character (len=MAX_LEN_COLLEGE_NAME) :: Name, Dean, TranscriptPreparer, TranscriptChecker
        logical :: hasInfo, isAllowed(AdviseOpenSubjects:ToEditGRADES,3)
    end type TYPE_COLLEGE

    type (TYPE_COLLEGE), dimension (0:MAX_ALL_COLLEGES) :: College
    integer :: NumColleges

!===========================================================
! declarations for departments
!===========================================================

    integer, parameter :: &
        MAX_ALL_DEPARTMENTS = 100, & ! max no. of departments
        MAX_LEN_DEPARTMENT_CODE = 10, & ! length of dept codes
        MAX_DEPARTMENT_NAME_LEN = 60 ! length of dept names

    type :: type_DEPARTMENT
        character (len=MAX_LEN_DEPARTMENT_CODE) :: Code
        character (len=MAX_DEPARTMENT_NAME_LEN) :: Name, Chair
        character (len=1) :: SectionPrefix ! prefix for section codes of classes in dept
        integer :: CollegeIdx
        logical :: hasInfo
    end type type_DEPARTMENT

    type (type_DEPARTMENT), dimension (0:MAX_ALL_DEPARTMENTS) :: Department
    integer :: NumDepartments

    integer, dimension(1:3,MAX_ALL_DEPARTMENTS) :: ScheduleCount ! last section in department

!===========================================================
! declarations for rooms
!===========================================================

    integer, parameter :: &
        MAX_ALL_ROOMS = 600, & ! room names
        MAX_LEN_ROOM_CODE=16    ! length of room code

    type :: TYPE_ROOM
        character (len=MAX_LEN_ROOM_CODE) :: Code
        integer :: DeptIdx, Cluster, MaxCapacity, EnergyFee
    end type TYPE_ROOM

    type (TYPE_ROOM), dimension(0:MAX_ALL_ROOMS) :: Room
    integer :: NumRooms, NumAdditionalRooms

!===========================================================
! declarations for teachers, users
!===========================================================

    integer, parameter :: &
        MAX_ALL_TEACHERS = 1500, & ! maximum number of teachers
        MAX_LEN_TEACHER_DEGREE=80, & ! length of string for Teacher degrees
        MAX_LEN_ACADEMIC_RANK=19, & ! length of string for academic rank
        MAX_LEN_STEP_IN_RANK=6, & ! length of string for step in academic rank
        MAX_LEN_ROLE = 10

    type :: TYPE_TEACHER
        character (len=MAX_LEN_USERNAME)   :: TeacherId
        character (len=MAX_LEN_PERSON_NAME)    :: Name
        integer                                :: DeptIdx, MaxLoad, Rank, Step, NumAdvisees
        character (len=MAX_LEN_TEACHER_DEGREE) :: Bachelor, Master, Doctorate, Specialization
        character (len=MAX_LEN_PASSWD_VAR)     :: Password ! Encrypted password
        character (len=MAX_LEN_ROLE)           :: Role     ! Guest; set by Admin
        integer(8)                             :: OnlineStatus(2)   ! (1): 0=logged out, 1+=clock tick of last activity; (2):IP addr
        integer                                :: EvalsToGive, EvalsToReceive
    end type TYPE_TEACHER

    type (TYPE_TEACHER), dimension(0:MAX_ALL_TEACHERS) :: Teacher
    integer, dimension(0:MAX_ALL_TEACHERS) :: TeacherRank
    integer :: NumTeachers, NumAdditionalTeachers, requestingTeacher

    integer, parameter :: MAX_ALL_ROLES = 7
    character (len=MAX_LEN_ROLE), parameter ::  &
        GUEST      = 'Guest     ', & ! 0-Cannot change anything
        FACULTY    = 'Faculty   ', & ! 1-Faculty - can edit own classlists, enter own grades, advise students assigned by Dean
        CHAIR      = 'Chair     ', & ! 2-Chair of Department(Teacher()%DeptIdx); also a Faculty
        DEAN       = 'Dean      ', & ! 3-Dean of College(Department(Teacher()%DeptIdx)%CollegeIdx); also a Chair
        STAFF      = 'Staff     ', & ! 4-Registrar's staff for a college
        OFFICIAL   = 'Official  ', & ! 5-High-ranking official of the University
        SYSAD      = 'ADMIN     ', & ! 6-Registrar; also a Dean
        BENEFACTOR = 'Benefactor'    ! 7-Scholarship

    logical, parameter :: orHigherUp = .true.

    character (len=MAX_LEN_ROLE), dimension(0:MAX_ALL_ROLES) :: txtRole = (/ &
        GUEST, FACULTY, CHAIR, DEAN, STAFF, SYSAD, OFFICIAL, BENEFACTOR /)

    ! Users
    character (len=MAX_LEN_ROLE) :: ROLE
    integer :: DeptIdxUser, CollegeIdxUser, CurriculumIdxUser

    logical :: &
        isRegistrar = .false., &
        isRoleBenefactor = .false., &
        isRoleOfficial = .false., &
        isRoleSysAd = .false., &
        isRoleStaff = .false., &
        isRoleDean = .false., &
        isRoleChair = .false., &
        isRoleFaculty = .false., &
        isRoleStudent = .false., &
        isRoleGuest = .false.

    character (len=MAX_LEN_ACADEMIC_RANK), dimension(0:4) :: AcademicRank = (/ &
        '(Specify Rank)     ', &
        'Instructor         ', 'Assistant Professor', &
        'Associate Professor', 'Professor          ' /)

    character (len=MAX_LEN_STEP_IN_RANK), dimension(0:12) :: RankStep = (/ &
        '(Step)', &
        'I     ', 'II    ', 'III   ', 'IV    ', &
        'V     ', 'VI    ', 'VII   ', 'VIII  ', &
        'IX    ', 'X     ', 'XI    ', 'XII   ' /)

!===========================================================
! declarations for time notions
!===========================================================

    integer, parameter :: MAX_LEN_TEXT_YEAR = 7
    character (len=MAX_LEN_TEXT_YEAR), dimension(-1:18) :: txtYear = (/  &
        'ERROR  ', &
        'YNS    ', 'FIRST  ', 'SECOND ', 'THIRD  ', 'FOURTH ', 'FIFTH  ', 'SIXTH  ', 'SEVENTH', 'EIGHTH ', &
        'Error  ', &
        'YNS    ', '1st    ', '2nd    ', '3rd    ', '4th    ', '5th    ', '6th    ', '7th    ', '8th    ' /)
    integer, parameter :: YEAR_NOT_SET = 0, MAX_YEAR_LEVEL = 8

     ! academic terms
    integer, parameter :: MAX_LEN_TEXT_SEMESTER = 6
    character (len=MAX_LEN_TEXT_SEMESTER), dimension(0:9) :: txtSemester = (/ &
        'ERROR ',     'FIRST ',     'SECOND',     'SUMMER', &
                      'First ',     'Second',     'Summer',&
                      '1st   ',     '2nd   ',     'Summer' /)

    character (len=10) :: termQualifier(0:9) = (/ &
        ' ERROR    ', ' SEMESTER ', ' SEMESTER ', ' TERM     ', &
                      ' Semester ', ' Semester ', ' Term     ', &
                      ' Sem.     ', ' Sem.     ', '          ' /)

    integer, parameter :: firstSemester = 1, secondSemester = 2, summerTerm = 3

    ! months
    character (len=10) :: txtMonth(0:12) = (/ &
        'ERROR     ', 'January   ', 'February  ', 'March     ', &
                      'April     ', 'May       ', 'June      ', &
                      'July      ', 'August    ', 'September ', &
                      'October   ', 'November  ', 'December  ' /)

    ! days
    character (len = 3), dimension (0:7) :: txtDay = (/       &
        '   ','Mon','Tue','Wed','Thu','Fri','Sat', 'Sun'/)
    !character (len = 2), dimension (0:7) :: txtDay2 = (/       &
    !    '  ','M ','T ','W ','Th','F ','S ','Su'/)

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
        MAX_ALL_SUBJECTS = 4000, &  ! max no. of subjects
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
        MAX_ALL_SUBSTITUTIONS = 1000, &                ! max no. of substitutions
        MAX_LEN_SUBSTITUTION_ARRAY = MAX_ALL_SUBSTITUTIONS*10   ! max size of substitutions array
    integer :: NumSubst
    integer, dimension(MAX_ALL_SUBSTITUTIONS) :: SubstIdx
    integer, dimension(MAX_LEN_SUBSTITUTION_ARRAY) :: Substitution

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
    type (TYPE_OFFERED_SUBJECTS) :: Offering(3,MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS)


!===========================================================
! declarations for curricula
!===========================================================

    integer, parameter :: &
        MAX_ALL_CURRICULA = 400, & ! max no. of active curricula
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

    ! flags to indicate processing is done for a curriculum group
    logical :: done(0:MAX_ALL_CURRICULA)

    ! file unit numbers based on generic curriculum
    integer, dimension(0:MAX_ALL_CURRICULA) :: CurrProgNum
    character (len=MAX_LEN_CURRICULUM_CODE), dimension(0:MAX_ALL_CURRICULA) :: CurrProgCode

    real, dimension(0:20) :: TermUnits, SpecifiedUnits

!===========================================================
! declarations for students
!===========================================================
    type :: TYPE_PRE_ENLISTMENT
        integer :: levelClassification, levelYear, levelPriority, &
            lenSubject, NPriority, NAlternates, NCurrent, NRemaining, MissingPOCW, &
            statusAdvising, statusEnlistment, statusScholastic, codeConditional, codeNSTP
        real :: UnitsEarned, AllowedLoad, UnderLoad
        integer, dimension(MAX_SUBJECTS_PER_TERM) :: Subject, Section, Grade
        real, dimension(MAX_SUBJECTS_PER_TERM) :: Contrib
    end type TYPE_PRE_ENLISTMENT
    integer :: NumEnlistment(3)=0

    integer, parameter :: MAX_ALL_STUDENTS = 30000 ! max no. of students
    integer, parameter :: & ! year-curriculum-number
        MAX_LEN_EMAIL_ADDRESS = 40, & ! length of email address
        MAX_LEN_STUDENT_CODE = 13 ! length of student numbers: YYYY-PPP-SSSS

    type :: TYPE_STUDENT
        character(len=1) :: Gender
        character(len=MAX_LEN_STUDENT_CODE) :: StdNo
        character(len=MAX_LEN_PERSON_NAME)  :: Name
        character(len=MAX_LEN_PASSWD_VAR)   :: Password ! Encrypted password
        character(len=MAX_LEN_USERNAME)     :: Adviser
        character(len=MAX_LEN_PERSON_NAME)  :: Scholarship
        character(len=MAX_LEN_PSGC)         :: HomePSGC
        character(len=MAX_LEN_CODE_TONGUE)  :: MotherTongue
        integer(8) :: OnlineStatus   ! 0=logged out, 1+=clock tick of last activity
        integer :: CurriculumIdx
        integer :: ResidenceStatus
        type (TYPE_PRE_ENLISTMENT) :: Enlistment(3)
    end type TYPE_STUDENT

    type (TYPE_STUDENT), dimension (0:MAX_ALL_STUDENTS) :: Student
    integer, dimension (0:MAX_ALL_STUDENTS):: StdRank

    integer :: NumStudents, NumAdditionalStudents, requestingStudent
    logical :: isDirtySTUDENTS = .false.

    real :: MaxLoad
    character (len=MAX_LEN_FILE_PATH) :: StdNoPrefix

    integer :: StdNoYearLen ! no. of characters in StdNo to use for directory name
    integer, parameter :: StdNoChars = 2 ! no. of characters in StdNo to use for directory name

    type :: TYPE_STUDENT_INFO
        character(len=20) :: BirthDate, EntryDate, GraduationDate ! September xx, yyyy
        character(len=MAX_LEN_PERSON_NAME)  :: Name, AdmissionData, Mother, Father, Guardian
        character(len=2*MAX_LEN_PERSON_NAME)  :: HomeStreetAddress, BirthPlace
        character(len=MAX_LEN_PSGC)  :: BirthPlacePSGC, HomePSGC
        character(len=MAX_LEN_CODE_TONGUE)  :: MotherTongue
        character(len=3*MAX_LEN_PERSON_NAME)  :: LastAttended, TranscriptRemark, TranscriptAdditionalRemark
        character(len=1) :: Gender
        character(len=MAX_LEN_USERNAME) :: Adviser
        character(len=MAX_LEN_PERSON_NAME)  :: Scholarship
        character(len=MAX_LEN_EMAIL_ADDRESS)  :: Email
        integer :: CurriculumIdx
        integer :: ResidenceStatus
    end type TYPE_STUDENT_INFO

    type (TYPE_STUDENT_INFO) :: StudentInfo

!===========================================================
! declarations for scholarships
!===========================================================

    integer, parameter :: MAX_ALL_SCHOLARSHIPS = 100 ! Max no. of scholarships
    character (len=MAX_LEN_PERSON_NAME) :: ScholarshipCode(0:MAX_ALL_SCHOLARSHIPS) = SPACE
    character (len=MAX_LEN_PERSON_NAME) :: ScholarshipDescription(0:MAX_ALL_SCHOLARSHIPS) = SPACE

!===========================================================
! declarations for grades
!===========================================================

    ! grade types
    character (len=10), dimension(0:3) :: txtGradeType = (/ &
        'ADVANCE   ', 'FINALGRADE', 'REMOVAL   ', 'COMPLETION' /)
        ! 0             1             2             3

    ! grades
    integer, parameter :: MAX_LEN_TEXT_GRADE = 4
    integer, parameter :: ZERO_PERCENT_GRADE = 49
    character (len = MAX_LEN_TEXT_GRADE), dimension(0:ZERO_PERCENT_GRADE+100) :: txtGrade = (/  &
        '____',                             & !  0, 0
        '1.00',  '1.0 ',  '1   ',           & !  1, 1-3
        '1.25',                             & !  2, 4
        '1.50',  '1.5 ',                    & !  3, 5-6
        '1.75',                             & !  4, 7
        '2.00',  '2.0 ',  '2   ',           & !  5, 8-10
        '2.25',                             & !  6, 11
        '2.50',  '2.5 ',                    & !  7, 12-13
        '2.75',                             & !  8, 14
        '3.00',  '3.0 ',  '3   ',           & !  9, 15-17
        '4.00',  '4.0 ',  '4   ',           & ! 10, 18-20
        'INC.',  'INC ',  'Inc ',  'Inc.',  & ! 11, 21-24
        '5.00',  '5.0 ',  '5   ',           & ! 12, 25-27
        'DRP.',  'DRP ',  'Drp.',  'Drp ',  & ! 13, 28-31
        'S   ',  'S.  ',  's   ',  's.  ',  & ! 14, 32-35
        'U   ',  'U.  ',  'u   ',  'u.  ',  & ! 15, 36-39
        'PASS',  'Pass',                    & ! 16, 40-41
        'LOA ',  'LOA.',  'Loa ',  'Loa.',  & ! 17, 42-45
        'REGD',                             & ! 18, 46
        'FAIL',  'Fail',                    & ! 19, 47-48
        'NFE ',                             & ! 20, 49
        ('    ',  NumStudents=1,100) /)

    ! pointer to grade
    integer, dimension(0:ZERO_PERCENT_GRADE+100) :: pGrade = (/ &
        0,  1,  4,  5,  7,  8, 11, 12, 14, 15, &
        18, 21, 25, 28, 32, 36, 40, 42, 46, 47, 49, 50, &
        (0, NumStudents=22,ZERO_PERCENT_GRADE+100) /)

    ! shorcuts to certain grades
    integer, parameter ::  &
        gdx4    = 10, gdxINC  = 11, gdx5     = 12, gdxDRP = 13, &
        gdxS    = 14, gdxU    = 15, gdxPASS  = 16, gdxLOA = 17, &
        gdxREGD = 18, gdxFAIL = 19, gdxNFE   = 20

#if defined UPLB
    real, dimension(0:ZERO_PERCENT_GRADE+100) :: fGrade = (/  & ! float value for grades
        0.00,                   & ! error
        1.00, 1.25, 1.50, 1.75, & ! 1, 1.25, 1.5, 1.75
        2.00, 2.25, 2.50, 2.75, & ! 2, 2.25, 2.5, 2.75
        3.00, 4.00, 0.00, 5.00, & ! 3, 4, INC, 5
        0.00, 0.00, 0.00, 0.00, & ! DRP, S, U, PASS
        0.00, 0.00, 0.00, 0.00, & ! LOA, REGD, FAIL, ****
        (0.0, NumStudents=21,ZERO_PERCENT_GRADE+100) /)
#else
    ! CSU equivalents of 1.0-5.0 grading scale
    real, dimension(0:ZERO_PERCENT_GRADE+100) :: fGrade = (/  & ! float value for grades
        0.00,                   & ! error
        100.0, 96.0, 93.0, 90.0, & ! 1, 1.25, 1.5, 1.75
         87.0, 84.0, 81.0, 78.0, & ! 2, 2.25, 2.5, 2.75
         75.0, 0.00, 0.00, 0.00, & ! 3, 4, INC, 5
         0.00, 0.00, 0.00, 0.00, & ! DRP, S, U, PASS
         0.00, 0.00, 0.00, 0.00, & ! LOA, REGD, FAIL, ****
        (0.0, NumStudents=21,ZERO_PERCENT_GRADE+100) /)
#endif

    ! residency status
    integer, parameter :: &
        REMOVE_FROM_ROSTER       = -1, &
        RESIDENCY_STATUS_NOT_SET = 0, &
        CURRENTLY_ENROLLED       = 1, &
        LEAVE_OF_ABSENCE         = 2, &
        ABSENT_WITHOUT_LEAVE     = 3, &
        MAX_LEN_RESIDENCE_STATUS = 4

    character (len=32), dimension(REMOVE_FROM_ROSTER:MAX_LEN_RESIDENCE_STATUS) :: descResidenceStatus = (/ &
        ' (to be removed from roster)    ', & ! -1
        ' (residence status not set)     ', & ! 0
        ' Currently enrolled             ', & ! 1
        ' On leave of absence            ', & ! 2
        ' Absent without leave           ', & ! 3
        ' (residence status not selected)' /)
        !1234567890123456789012345678901234567890123456789012345678901234567890

    ! scholastic standing
    character (len=12), dimension(0:8) :: txtScholastic = (/ &
        'ERROR       ', 'Good        ', 'Satisfactory', 'Warning     ', 'Probation   ', &
                        'Dismissed   ', 'Perm Disq   ', 'LOA         ', 'Not enrolled' /)

    ! standing
    character (len=9), dimension(0:10) :: txtStanding = (/ &
        'ERROR    ', 'FRESHMAN ', 'SOPHOMORE', 'JUNIOR   ', 'SENIOR   ', 'FIFTH    ', &
        'SIXTH    ', 'SEVENTH  ', 'EIGHTH   ', 'NINTH    ', 'GRADUATE '/)

    ! year levels
    integer, parameter :: &
        YEAR_LEVEL_NOT_SET = 0, &
        YEAR_LEVEL_FIRST   = 1, &
        YEAR_LEVEL_SECOND  = 2, &
        YEAR_LEVEL_THIRD   = 3, &
        YEAR_LEVEL_FOURTH  = 4, &
        YEAR_LEVEL_FIFTH   = 5, &
        YEAR_LEVEL_SIXTH   = 6, &
        YEAR_LEVEL_SEVENTH = 7, &
        YEAR_LEVEL_EIGHTH  = 8

!===========================================================
! declarations for sections
!===========================================================

    integer, parameter :: &
        MAX_ALL_SECTIONS = 6000, &      ! all sections offered in a given term
        MAX_LEN_SECTION_CODE = 20, &    ! length of section codes
        MAX_SECTION_MEETINGS = 21       ! maximum no. of meetings of a section in a week

    integer, parameter :: &
        MAX_LEN_CLASS_ID = MAX_LEN_SUBJECT_CODE+MAX_LEN_SECTION_CODE, &      ! length of section codes
        MAX_LEN_BLOCK_CODE = MAX_LEN_CLASS_ID

    type :: TYPE_SECTION
        character (len=MAX_LEN_CLASS_ID) :: ClassId
        character (len=MAX_LEN_SECTION_CODE) :: Code
        character (len=8) :: GradeSubmissionDate
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
        logical :: isDirty
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


!===========================================================
! worksheet to evaluate the record of a student
!===========================================================

    character (len=MAX_LEN_FILE_PATH) :: fileTCG
    integer, parameter :: MAX_LEN_STUDENT_RECORD = 300
    type :: TYPE_STUDENT_RECORD
        integer :: &
            ErrorCode, & ! if record is garbled
            Code, &      ! -1=error, 0=remark, 1=Substitution, 2=APE, 3=FINALGRADE, 4=REMOVAL, 5=COMPLETION
            Year, &      ! actual year grade was given/subject was registered
            Term, &      ! actual term (SUMMER, FIRST, SECOND) grade was given
            Subject, &   ! index to subject
            Grade, &     ! index to grade
            ReExam, &    ! index to re-exam/completion/removal grade
            Taken, &     ! number of terms since grade was given
            Reqd(0:5), & ! Plan of study: indices to required subjects
            Subst(0:5)   ! Plan of study: indices to replacement subjects
        real :: UnitsUsed ! units used for substitutions
        logical :: Used  ! record has been used
        character(len=127) :: txtLine
        character(len=255) :: errLine
    end type TYPE_STUDENT_RECORD
    type (TYPE_STUDENT_RECORD), dimension (MAX_LEN_STUDENT_RECORD) :: TCG
    integer :: lenTCG

    type (TYPE_CURRICULUM) :: CheckList

    type :: TYPE_CHECKLIST_EXTENSION
        integer :: PriorityRank ! index to rank as recommended subject
        integer :: Grade       ! actual or "substitute" grade
        integer, dimension(0:MAX_ALL_SUBJECT_PREREQ) :: CLPreq, CPPreq
        integer :: EarlyTime ! When is the earliest time the subject can be taken (in semeters, from current semester)?
        integer :: LateTime  ! EarlyTime+Slack
                             ! Slack = No. of sems the student can delay taking the subject without extending degree completion
        real    :: Contrib   ! Contribution to forecast for subject
        logical :: OKPreq, OKCoreq, OKConcPreq, isOffered
        character (len=MAX_LEN_SUBJECT_CODE+34) :: Disp_Subject, &
            Disp_Comment    ! comment
        character (len=MAX_LEN_TEXT_GRADE+27) :: Disp_Grade ! <color>grade<e_color>
        character (len=4) :: Disp_Units ! units
        character (len=5) :: Disp_Remarks ! remarks (PriN, AltK)
                        ! *=specify, ?=check prereq, #=repeat/remove/complete,
                        ! %=lapsed 4/INC, @=offered, $=prerequisite satisfied
        character (len=255) :: &
            Disp_Input_Elective, & ! elective input
            Disp_Input_Grade    ! grade input
    end type TYPE_CHECKLIST_EXTENSION

    type (TYPE_CHECKLIST_EXTENSION) :: CLExt(0:MAX_SUBJECTS_IN_CURRICULUM)


!===========================================================
! student statistics
!===========================================================

    ! Enlistment(.,.)%<field> codes
    integer, parameter :: &
        SCHOLASTIC_STATUS_NOT_SET        = 0, & ! ScholasticStatus
        SCHOLASTIC_FAILED_NONE           = 1, & ! ScholasticStatus
        SCHOLASTIC_FAILED_1SUBJ_PLUS     = 2, & ! ScholasticStatus
        SCHOLASTIC_FAILED_25PCT_PLUS     = 3, & ! ScholasticStatus
        SCHOLASTIC_FAILED_50PCT_PLUS     = 4, & ! ScholasticStatus
        SCHOLASTIC_FAILED_75PCT_PLUS     = 5, & ! ScholasticStatus
        SCHOLASTIC_ALL_FAILED_LAST_SEM   = 6, & ! ScholasticStatus
        SCHOLASTIC_ALL_DRP_LAST_SEM      = 7, & ! ScholasticStatus
        SCHOLASTIC_NOT_ENROLLED_LAST_SEM = 8, & ! ScholasticStatus
        MAX_ALL_SCHOLASTIC_STATUS        = 8, &
        ANALYSIS_UNRESOLVED_INC_NFE         =  9, & ! codeConditional
        ANALYSIS_NSTP_MISMATCH              = 10, & ! codeNSTP
        ANALYSIS_NO_REMAINING_SUBJECTS      = 11, & ! statusAdvising==ADVISING_FINISHED
        ANALYSIS_REGULAR                    = 12, & ! statusAdvising==ADVISING_REGULAR
        !ANALYSIS_NOT_FINISHED_NONE_FEASIBLE = 13, & ! NRemaining
        !ANALYSIS_GRADUATING                 = 14, & !
        MAX_ALL_ANALYSIS_CODES              = 5

    character (len=50), dimension(SCHOLASTIC_STATUS_NOT_SET:MAX_ALL_SCHOLASTIC_STATUS+MAX_ALL_ANALYSIS_CODES) :: &
            descScholastic = (/ &
        ' (scholastic status not set)                      ', & ! 0
        ' who did not fail any subject last sem            ', & ! 1
        ' who failed (0%,25%] of units last sem            ', & ! 2
        ' who failed (25%,50%] of units last sem           ', & ! 3
        ' who failed (50%,75%] of units last sem           ', & ! 4
        ' who failed (75%,100%) of units last sem          ', & ! 5
        ' who failed 100% of units last sem                ', & ! 6
        ' who dropped all subjects last sem                ', & ! 7
        ' who did not enroll last sem                      ', & ! 8
        ' with unresolved INC or NFE                       ', & ! 9
        ' with NSTP 11/12 track mismatch                   ', & ! 10
        ' with no remaining subjects                       ', & ! 11
        ' with advised subjects as scheduled in curriculum ', & ! 12
        !' with no feasible subjects     ', & ! 13
        !' with 27 units or less remaining                  ', & ! 14
        ' (specify criterion)                              '/)  ! 15
        !1234567890123456789012345678901234567890123456789012345678901234567890

    ! Enlistment(.,.)%statusAdvising, %statusEnlistment, %levelPriority codes
    integer, parameter :: &
        ADVISING_EXCLUDED    = 0, & ! Excluded
        ADVISING_NEEDED      = 1, & ! Not yet advised
        ADVISING_FINISHED    = 2, & ! Curricular program completed
        ADVISING_NO_SUBJECTS = 3, & ! Subjects not offered, or prerequisites not satisfied
        ADVISING_REGULAR     = 4, & ! Auto-advised as regular student
        ADVISING_IRREGULAR   = 5, & ! Auto-advised as irregular student
        ENLISTMENT_NEEDED    = 6, & ! Not yet enlisted
        ENLISTMENT_AUTOMATIC = 7, & ! Auto-enlisted by Registrar via 'Preenlist'
        ENLISTMENT_MANUAL    = 8, & ! Enlisted by adviser, or self-enlisted
        ENLISTMENT_CONFIRMED = 9, & ! Schedule confirmed by student or Registrar via 'CONFIRM Schedule'
        ENLISTMENT_LOCKED    =10, & ! Schedule printed and locked by Registrar via 'LOCK'
        ENLISTMENT_PAID      =11, & ! Partial or full payment made
        PRIORITY_NEW_STUDENT =12, & ! New student (no record of numeric grades)
        PRIORITY_GRADUATING  =13, & ! Graduating (27 units or less left, no remaining subjects w/ unsatisfied prereqs)
        PRIORITY_GOOD        =14, & ! No failed subject or not on LOA last semester
        PRIORITY_WARNING     =15, & ! Warning: failed (0-50%] of enrolled units last semester
        PRIORITY_PROBATION   =16, & ! Probation: failed (50%,75%] of enrolled units last semester
        PRIORITY_DISMISSED   =17    ! Dismissed/Permanently disqualified: failed (75%-100%] of enrolled units last semester

    character (len=4), dimension(ADVISING_EXCLUDED:PRIORITY_DISMISSED) :: txtStatusCode = (/ &
        'Excl', 'NYAd', 'NRem', 'NSub', 'AReg', 'AIrr', &
        'NYEn', 'EnlA', 'EnlM', 'Cfrm', 'Lckd', 'Paid', &
        'NewS', 'Grad', 'Good', 'Warn', 'Prob', 'Dism' /)

    character (len=26), dimension(ADVISING_EXCLUDED:PRIORITY_DISMISSED) :: descStatusCode = (/ &
        'Excluded                  ', 'Not yet advised           ', 'No remaining subjects     ', &
        'No advised subjects       ', 'Advised regular subjects  ', 'Advised irregular subjects', &
        'Advised, not yet enlisted ', 'Auto-preenlisted          ', 'Manually-enlisted         ', &
        'Schedule confirmed        ', 'Schedule locked           ', 'Payment made              ', &
        'Priority: New student     ', 'Priority: Graduating      ', 'Priority: Good standing   ', &
        'Priority: Warning         ', 'Priority: Probation       ', 'Priority: Dismissed       '/)
        !1234567890123456789012345678901234567890123456789012345678901234567890

    integer :: CollegeCount(0:MAX_ALL_COLLEGES), ProgramCount(0:MAX_ALL_CURRICULA), CurriculumCount(0:MAX_ALL_CURRICULA)
    integer :: CurriculumYearStatus(0:MAX_ALL_CURRICULA,YEAR_NOT_SET:MAX_YEAR_LEVEL,ADVISING_EXCLUDED:PRIORITY_DISMISSED)
    integer :: GenderCount(0:MAX_ALL_CURRICULA,YEAR_NOT_SET:MAX_YEAR_LEVEL,3)
    integer, parameter :: MALE = 1, UNKNOWN = 2, FEMALE = 3


!===========================================================
! teacher evaluation
!===========================================================
    integer, parameter :: MAX_EVALUATION_CRITERIA = 30 ! number of evaluation criteria
    type :: TYPE_EVALUATION_CRITERION
        integer :: Category, Item
        character (len=255) :: Criterion
    end type TYPE_EVALUATION_CRITERION

    type (TYPE_EVALUATION_CRITERION) :: Evaluation(MAX_EVALUATION_CRITERIA)
    integer :: NumEvalCriteria

    integer, parameter :: MAX_EVALUATION_CATEGORIES = 6 ! max number of evaluation criterion groups
    type :: TYPE_EVALUATION_CATEGORY
        character :: Code
        integer :: Weight
        character (len=255) :: Description
    end type TYPE_EVALUATION_CATEGORY

    type (TYPE_EVALUATION_CATEGORY) :: EvalCategory(MAX_EVALUATION_CATEGORIES)
    integer :: NumEvalCategories

    integer, parameter :: MAX_EVALUATION_TYPES = 4, & ! max number of evaluation types
        MAX_LEN_EVAL_TYPE_DESCRIPTION = 10
    character (len=MAX_LEN_EVAL_TYPE_DESCRIPTION), dimension(MAX_EVALUATION_TYPES) :: EvalTypeDescription = (/ &
        'SELF      ', 'PEER      ', 'SUPERVISOR', 'STUDENT   ' /)

    integer, parameter :: MAX_EVALUATION_RATINGS = 5, & ! max number of evaluation ratings
        MAX_LEN_RATING = 19
    character (len=MAX_LEN_RATING), dimension(0:MAX_EVALUATION_RATINGS) :: EvalRating = (/ &
        '(select)           ', &
        '1=Poor             ', &
        '2=Fair             ', &
        '3=Satisfactory     ', &
        '4=Very satisfactory', &
        '5=Outstanding      ' /)

    type :: TYPE_EVALUATION_FORM
        integer                           :: EvalType ! 1=Self, 2=Peer, 3=Supervisor, 4=Student
        integer                           :: Year, Term ! YYYY; 1=FIRST, 2=SECOND, 3=SUMMER
        character (len=14)                :: LastModified ! YYYYMMDD-HH:MM
        character (len=MAX_LEN_USERNAME)  :: EvaluatedID, EvaluatorID
        character (len=MAX_LEN_CLASS_ID)  :: ClassId ! class, if EvalType==4, EvaluatorID is a student number
        character (len=2048)              :: Comments ! evaluator's comments
        integer                           :: Rating(MAX_EVALUATION_CRITERIA) ! rating for evaluation criterion
    end type TYPE_EVALUATION_FORM

    type (TYPE_EVALUATION_FORM) :: EvaluationForm
    character (len=MAX_LEN_FILE_PATH) :: dirEVALUATIONS(3) ! directory for teacher evaluations


contains

    subroutine student_counts(thisTerm)
        integer, intent(in) :: thisTerm

        integer :: gdx, sdx, ldx, pdx, kdx, levelYear, statusEnlistment, statusAdvising
!#if defined UPLB
        integer :: levelPriority
!#endif

        CollegeCount = 0   ! # students in college
        ProgramCount= 0    ! # students in a degree program
        CurriculumCount= 0 ! # students in a specific curriculum
        GenderCount = 0
        CurriculumYearStatus = 0
        do sdx=1,NumStudents+NumAdditionalStudents
            if (Student(sdx)%Gender=='M') then
                gdx = MALE
            else if (Student(sdx)%Gender=='F') then
                gdx = FEMALE
            else
                gdx = UNKNOWN
            end if
            ldx = Student(sdx)%CurriculumIdx
            pdx = CurrProgNum(ldx)
            kdx = Curriculum(ldx)%CollegeIdx

            levelYear        = Student(sdx)%Enlistment(thisTerm)%levelYear
            statusEnlistment = Student(sdx)%Enlistment(thisTerm)%statusEnlistment
            statusAdvising   = Student(sdx)%Enlistment(thisTerm)%statusAdvising
            levelPriority    = Student(sdx)%Enlistment(thisTerm)%levelPriority

            CurriculumCount(ldx) = CurriculumCount(ldx)+1
            ProgramCount(pdx)    = ProgramCount(pdx)+1
            CollegeCount(kdx)    = CollegeCount(kdx) + 1

            GenderCount(ldx,levelYear,gdx)   = GenderCount(ldx,levelYear,gdx) + 1

            CurriculumYearStatus(ldx,levelYear,statusAdvising)   = CurriculumYearStatus(ldx,levelYear,statusAdvising) + 1
            CurriculumYearStatus(ldx,levelYear,statusEnlistment) = CurriculumYearStatus(ldx,levelYear,statusEnlistment) + 1

#if defined UPLB
            if (levelPriority/=0) &
                CurriculumYearStatus(ldx,levelYear,levelPriority) = CurriculumYearStatus(ldx,levelYear,levelPriority) + 1
#endif
        end do

    end subroutine student_counts


!===========================================================
! routines for PSGC
!===========================================================

    function index_to_PSGC(code)
        integer :: index_to_PSGC
        character (len=MAX_LEN_PSGC), intent (in) :: code
        integer :: i, j, sdx

        i = 1
        j = NumPSGC
        sdx = 0
        do
            if (i>j) then
                sdx = 0
                exit
            else
                sdx = (i + j)/2
                if (code==PSGC(sdx)%Code) then
                    exit
                else if (code <PSGC(sdx)%Code) then
                    j = sdx-1
                else
                    i = sdx+1
                end if
            end if
        end do
        index_to_PSGC = sdx

    end function index_to_PSGC


    function index_to_PSGCsubset(code, nChars, lenArray, locArray)
        integer :: index_to_PSGCsubset
        character (len=MAX_LEN_PSGC), intent (in) :: code
        integer, intent(in) :: nChars, lenArray, locArray(lenArray)
        integer :: i, j, sdx

        i = 1
        j = lenArray
        sdx = 0
        do
            if (i>j) then
                sdx = 0
                exit
            else
                sdx = (i + j)/2
                if (code(1:nChars)==PSGC(locArray(sdx))%Code(1:nChars)) then
                    exit
                else if (code(1:nChars) <PSGC(locArray(sdx))%Code(1:nChars)) then
                    j = sdx-1
                else
                    i = sdx+1
                end if
            end if
        end do
        index_to_PSGCsubset = sdx

    end function index_to_PSGCsubset


    function isPSGC_kind(tCode, tKind)
        character (len=MAX_LEN_PSGC), intent(in) :: tCode
        integer, intent(in) :: tKind ! 3-region, 5-province, 7-municipality
        logical :: isPSGC_kind

        isPSGC_kind = tCode(tKind:MAX_LEN_PSGC)==nullPSGC(tKind:MAX_LEN_PSGC)

    end function isPSGC_kind


    function makePSGC_kind(tCode, tKind)
        character (len=MAX_LEN_PSGC), intent(in) :: tCode
        integer, intent(in) :: tKind ! 3-region, 5-province, 7-municipality
        character (len=MAX_LEN_PSGC) :: makePSGC_kind

        makePSGC_kind = tCode(1:tKind-1)//nullPSGC(tKind:MAX_LEN_PSGC)

    end function makePSGC_kind


    function text_PSGC(code)
        character (len=3*MAX_LEN_PSGC_NAME) :: text_PSGC, tName
        character (len=MAX_LEN_PSGC), intent (in) :: code

        integer :: idxBarangay, idxMunicipality, idxProvince, idxRegion
        character (len=MAX_LEN_PSGC) :: tCode

        tName = SPACE
        if (isPSGC_kind(code, kindRegion)) then ! region
            idxRegion = index_to_PSGC(code)
            tName = PSGC(idxRegion)%Name
        elseif (isPSGC_kind(code, kindProvince)) then ! province/district
            tCode = makePSGC_kind(code, kindRegion)
            idxRegion = index_to_PSGC(tCode)
            idxProvince = index_to_PSGC(code)
            tName = trim(PSGC(idxProvince)%Name)//', '//PSGC(idxRegion)%Name
        elseif (isPSGC_kind(code, kindMunicipality)) then ! municipality/city
            tCode = makePSGC_kind(code, kindProvince)
            idxProvince = index_to_PSGC(tCode)
            idxMunicipality = index_to_PSGC(code)
            tName = trim(PSGC(idxMunicipality)%Name)//', '//PSGC(idxProvince)%Name
        else
            tCode = makePSGC_kind(code, kindProvince)
            idxProvince = index_to_PSGC(tCode)
            tCode = makePSGC_kind(code, kindMunicipality)
            idxMunicipality = index_to_PSGC(tCode)
            idxBarangay = index_to_PSGC(code)
            tName = trim(PSGC(idxBarangay)%Name)//', '//trim(PSGC(idxMunicipality)%Name)//', '//PSGC(idxProvince)%Name
        end if
        text_PSGC = tName

    end function text_PSGC


!===========================================================
! routines for PSGC
!===========================================================

    function index_to_Tongue(code)
        integer :: index_to_Tongue
        character (len=MAX_LEN_CODE_TONGUE), intent (in) :: code
        integer :: i, j, sdx

        i = 1
        j = NumTongue
        sdx = 0
        do
            if (i>j) then
                sdx = 0
                exit
            else
                sdx = (i + j)/2
                if (code==Tongue(sdx)%Code) then
                    exit
                else if (code <Tongue(sdx)%Code) then
                    j = sdx-1
                else
                    i = sdx+1
                end if
            end if
        end do
        index_to_Tongue = sdx

    end function index_to_Tongue


!===========================================================
! routines for accounts
!===========================================================

    function index_to_account_code (tCode)
        !returns index of tCode in the list of accounts

        integer :: index_to_account_code
        character (len=MAX_LEN_ACCOUNT_CODE), intent (in) :: tCode

        integer :: i

        index_to_account_code = 0
        do i=1,NumAccounts
            if (tCode==AccountCode(i)) then
                index_to_account_code = i
                exit
            end if
        end do

    end function index_to_account_code


    function index_to_account_description (tDesc)
        !returns index of tDesc in the list of accounts

        integer :: index_to_account_description
        character (len=MAX_LEN_PERSON_NAME), intent (in) :: tDesc

        integer :: i

        index_to_account_description = 0
        do i=1,NumAccounts
            if (tDesc==AccountDescription(i)) then
                index_to_account_description = i
                exit
            end if
        end do

    end function index_to_account_description


!===========================================================
! routines for colleges
!===========================================================

    subroutine initialize_college (wrkCollege, tCode, tName, tDean, tPreparer, tChecker)

        type(TYPE_COLLEGE), intent(out) :: wrkCollege
        character(len=*), intent(in), optional :: tCode, tName, tDean, tPreparer, tChecker

        if (present(tCode)) then
            wrkCollege = TYPE_COLLEGE(tCode, tName, tDean, tPreparer, tChecker, &
                .false., .false.)
        else
            wrkCollege = TYPE_COLLEGE(SPACE, SPACE, 'Fname MI LName', 'Fname MI LName', &
                'Fname MI LName', .false., .false.)
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

    subroutine initialize_department (wrkDepartment, tCode, tName, tChair, tPrefix, iCollege)

        type(type_DEPARTMENT), intent (out) :: wrkDepartment
        character(len=*), intent (in), optional :: tCode, tName, tChair, tPrefix
        integer, intent (in), optional :: iCollege

        if (present(tCode)) then
            wrkDepartment = type_DEPARTMENT(tCode, tName, tChair, tPrefix, iCollege, .false.)
        else
            wrkDepartment = type_DEPARTMENT(SPACE, SPACE, SPACE, SPACE, 0, .false.)
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

    subroutine initialize_room (wrkRoom, tCode, iDept, iCluster, iCapacity, iEnergyFee)

        type(TYPE_ROOM), intent (out) :: wrkRoom
        character(len=*), intent (in), optional :: tCode
        integer, intent (in), optional :: iDept, iCluster, iCapacity, iEnergyFee

        if (present(tCode)) then
            wrkRoom = TYPE_ROOM(tCode, iDept, iCluster, iCapacity, iEnergyFee)

        else
            wrkRoom = TYPE_ROOM(SPACE, NumDepartments, 0, 0, 0)
        end if


    end subroutine initialize_room


    function index_to_room (tRoom)

        integer :: index_to_room
        character (len=MAX_LEN_ROOM_CODE), intent (in) :: tRoom

        integer :: rdx ! i, j,

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

        wrkTeacher = TYPE_TEACHER(SPACE, SPACE, NumDepartments, 0, 0, 0, 0, SPACE, SPACE, SPACE, SPACE, &
                SPACE, GUEST, 0, 0, 0)

    end subroutine initialize_teacher


    function index_to_teacher (token)

        integer :: index_to_teacher
        character (len=MAX_LEN_USERNAME), intent (in) :: token

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


    subroutine count_teacher_load(fac, term, nLoad)
        integer, intent (in) :: fac, term
        integer, intent (out) :: nLoad(3)

        integer :: mdx, sdx, iStart, iEnd, iTerm

        nLoad = 0 ! how many classes taught
        if (term/=0) then
            iStart = term
            iEnd = term
        else
            iStart = 1
            iEnd = 3
        end if
        do iTerm=iStart,iEnd
            do sdx=1,NumSections(iTerm)
                do mdx=1,Section(iTerm,sdx)%NMeets
                    if (Section(iTerm,sdx)%TeacherIdx(mdx)/=fac) cycle
                    nLoad(iTerm) = nLoad(iTerm) + 1
                    exit
                end do
            end do
        end do

    end subroutine count_teacher_load


    function isRole_admin_of_college(cdx)
        logical :: isRole_admin_of_college, flagIsUp
        integer, intent (in) :: cdx

        if (isRoleStaff) then
            flagIsUp = index(Teacher(requestingTeacher)%Specialization, trim(College(cdx)%Code)//SPACE ) > 0
        elseif (isRoleSysAd .or. isRoleOfficial) then
            flagIsUp = .true.
        else
            flagIsUp = .false.
        end if
        isRole_admin_of_college = flagIsUp

    end function isRole_admin_of_college


    function isRole_dean_of_college(cdx, higherUp)
        logical :: isRole_dean_of_college, flagIsUp
        integer, intent (in) :: cdx
        logical, optional, intent (in) :: higherUp

        flagIsUp = .false.
        !if (isRoleDean) then
        if (isRoleDean .or. isRoleChair) then
            flagIsUp = CollegeIdxUser == cdx
        elseif (present(higherUp)) then
            flagIsUp = isRole_admin_of_college(cdx)
        end if
        isRole_dean_of_college = flagIsUp

    end function isRole_dean_of_college


    function isRole_chair_of_department(ddx, higherUp)
        logical :: isRole_chair_of_department, flagIsUp
        integer, intent (in) :: ddx
        logical, optional, intent (in) :: higherUp

        flagIsUp = .false.
        if (isRoleChair) then
            flagIsUp = DeptIdxUser == ddx
        elseif (present(higherUp)) then
            flagIsUp = isRole_dean_of_college(Department(ddx)%CollegeIdx, higherUp)
        end if
        isRole_chair_of_department = flagIsUp

    end function isRole_chair_of_department


    function isRole_teacher_of_class(tSection, higherUp)
        logical :: isRole_teacher_of_class, flagIsUp
        type (TYPE_SECTION), intent (in) :: tSection
        logical, optional, intent (in) :: higherUp
        integer :: i

        ! does USER teach the section?
        flagIsUp = .false.
        if (requestingTeacher/=0) then
            do i=1,tSection%NMeets
                if (tSection%TeacherIdx(i)==requestingTeacher) flagIsUp = .true.
            end do
        end if
        if (.not. flagIsUp .and. present(higherUp)) then
#if defined UPLB
            flagIsUp = isRole_chair_of_department(tSection%DeptIdx, higherUp)
#else
            flagIsUp = isRole_dean_of_college(Department(tSection%DeptIdx)%CollegeIdx, higherUp)
#endif
        end if
        isRole_teacher_of_class = flagIsUp

    end function isRole_teacher_of_class


    function isRole_adviser_of_student(std, higherUp)
        logical :: isRole_adviser_of_student, flagIsUp
        integer, intent (in) :: std
        logical, optional, intent (in) :: higherUp
        integer :: idx

        flagIsUp = .false.
        if (USERNAME==Student(std)%Adviser) then
            flagIsUp = .true.
        elseif (present(higherUp)) then
#if defined UPLB
            idx = index_to_teacher(Student(std)%Adviser)
            if (idx/=0) then
                flagIsUp = isRole_chair_of_department(Teacher(idx)%DeptIdx, higherUp)
            else
                idx = Student(std)%CurriculumIdx
                flagIsUp = isRole_dean_of_college(Curriculum(idx)%CollegeIdx, higherUp)
            end if
#else
            idx = Student(std)%CurriculumIdx
            flagIsUp = isRole_dean_of_college(Curriculum(idx)%CollegeIdx, higherUp) .or. &
                ( (isRoleDean .or. isRoleChair) .and. (idx==NumCurricula .or. &
                                                       Curriculum(idx)%CollegeIdx==CollegeIdxUser) )
#endif
        end if
        isRole_adviser_of_student = flagIsUp

    end function isRole_adviser_of_student


    function isRole_benefactor_of_student(std)
        logical :: isRole_benefactor_of_student
        integer, intent (in) :: std

        isRole_benefactor_of_student = USERNAME==Student(std)%Scholarship

    end function isRole_benefactor_of_student


    subroutine getPassword_of_teacher(tdx, Password)
        integer, intent (in) :: tdx
        character (len=MAX_LEN_PASSWD_VAR), intent (out) :: Password

        Password = Teacher(tdx)%Password
        call decrypt(passwordEncryptionKey, Password)
        Password = Password(lenPasswordEncryptionKey-atoi(Password(3:4))+1:)
        !write(*,*) Teacher(tdx)%Password, Password

    end subroutine getPassword_of_teacher


    function isPassword_of_teacher(tdx, Password)
        logical :: isPassword_of_teacher
        integer, intent (in) :: tdx
        character (len=*), intent (in) :: Password
        character (len=MAX_LEN_PASSWD_VAR) :: sPassword, tPassword

        sPassword = Password
        call getPassword_of_teacher(tdx, tPassword)
        isPassword_of_teacher = tPassword(:MAX_LEN_PASSWORD)==sPassword(:MAX_LEN_PASSWORD)

    end function isPassword_of_teacher


    subroutine getPassword_of_student(std, Password)
        integer, intent (in) :: std
        character (len=MAX_LEN_PASSWD_VAR), intent (out) :: Password

        Password = Student(std)%Password
        call decrypt(passwordEncryptionKey, Password)
        Password = Password(lenPasswordEncryptionKey-atoi(Password(3:4))+1:)

    end subroutine getPassword_of_student



    function isPassword_of_student(std, Password)
        logical :: isPassword_of_student
        integer, intent (in) :: std
        character (len=*), intent (in) :: Password
        character (len=MAX_LEN_PASSWD_VAR) :: sPassword, tPassword

        sPassword = Password
        call getPassword_of_student(std, tPassword)
        isPassword_of_student = tPassword(:MAX_LEN_PASSWORD)==sPassword(:MAX_LEN_PASSWORD)

    end function isPassword_of_student


!===========================================================
! routines for time, time periods
!===========================================================


    function index_to_year (tYear)
        ! returns index of tYear in the list of Years
        integer :: index_to_year
        character (len=MAX_LEN_TEXT_YEAR), intent (in) :: tYear
        integer :: i, idx

        idx = 0
        do i=1,19
            if (tYear==txtYear(i)) then
                idx = i
                exit
            end if
        end do
        if (idx>10) idx = idx-10
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


    function index_to_term (strTerm)
        ! returns index of strTerm in the list of Terms
        integer :: index_to_term
        character (len=MAX_LEN_TEXT_SEMESTER), intent (in) :: strTerm
        integer :: i
        index_to_term = 0
        do i=1,9
            if (strTerm==txtSemester(i)) then
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


    subroutine qualify_term (target, Year, Term, description)
        integer, intent(in) :: target
        integer, intent(out) :: Year, Term
        character(len=*), intent (out), optional :: description
        character(len=11) :: comment
        character(len=20) :: color

        Term = target
        Year = currentYear
        comment = SPACE
        color = e_color

        if (present(description)) then
            description = color//trim(text_term_school_year(Term+3, Year, comment))//e_color
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

        text_school_year = 'SY '//trim(itoa(year))//dash//itoa2bz(mod(year+1,100))

    end function text_school_year


    function text_term_school_year (thisTerm, thisYear, comment)

        character (len=MAX_LEN_TEXT_SEMESTER+32) :: text_term_school_year
        integer, intent (in) :: thisTerm, thisYear
        character (len=*), intent(in), optional :: comment

        if (present(comment)) then
            text_term_school_year = trim(txtSemester(thisTerm))//trim(comment)//trim(termQualifier(thisTerm))//SPACE// &
                text_school_year(thisYear)
        else
            text_term_school_year = trim(txtSemester(thisTerm))//trim(termQualifier(thisTerm))//SPACE// &
                text_school_year(thisYear)
        end if

    end function text_term_school_year



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

    end subroutine initialize_subject


    function index_to_subject(token)
        integer :: index_to_subject
        character (len=MAX_LEN_SUBJECT_CODE), intent(in) :: token
        integer :: i, j, cdx

        index_to_subject = 0
        if (len_trim(token)==0) return

        ! try the dummy subjects
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
            call upper_case(token)
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


    function isSubject_graduate_level(subj)
        logical :: isSubject_graduate_level
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
        i = index(tSubject,DOT)
        if (i>0) l = i-1
        f = index(tSubject, SPACE)+1
        tSubject = tSubject(f:l)
        num = atoi(tSubject)
        isSubject_graduate_level = num>200

    end function isSubject_graduate_level


    function isSubject_offered(subj, term)
        logical :: isSubject_offered
        integer, intent (in) :: subj, term
        integer :: j

        ! offered?
        j = Subject(subj)%TermOffered
        isSubject_offered = &
        (j==7) .or. & ! offered 12S
        (term==1 .and. mod(j,2)==1) .or. & ! offered 1,12,1S
        (term==2 .and. (j==2 .or. j==3 .or. j==6) ) .or. & ! offered 2,12,2S
        (term==3 .and. (j>3) ) ! offered S,1S,2S

    end function isSubject_offered


    function isSubject_lecture_lab(subj)
        integer, intent (in) :: subj
        logical :: isSubject_lecture_lab
        ! returns true if subj is a lecture-lab/recit subject
        isSubject_lecture_lab = Subject(subj)%LectHours*Subject(subj)%LabHours/=0.0

    end function isSubject_lecture_lab


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
        j = href
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


    function get_area (token)
        character (len=MAX_LEN_SUBJECT_CODE), intent(in) :: token
        character (len=MAX_LEN_SUBJECT_CODE) :: get_area, tArea
        integer :: i, lenToken

        lenToken = len_trim(token)
        tArea = token
        do i=lenToken-1,1,-1
            if (token(i:i)/=SPACE) cycle
            tArea(i+1:) = SPACE
            exit
        end do
        get_area = tArea

    end function get_area

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
            if (CurrProgNum(idx)/=0) cycle

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
            !call log_comment( itoa(idx)//Curriculum(idx)%Code//itoa(k)//CurrProgCode(idx) )
            do i = idx+1,NumCurricula
                if (CurrProgNum(i)/=0) cycle

                if (Curriculum(idx)%CollegeIdx==Curriculum(i)%CollegeIdx .and. &
                        Curriculum(idx)%Code(1:j)==Curriculum(i)%Code(1:j) .and. &
                        (Curriculum(i)%Code(j+1:j+1)==SPACE .or. Curriculum(i)%Code(j+1:j+1)=='-') ) then
                    CurrProgNum(i) = k
                    CurrProgCode(i) = CurrProgCode(idx)
                    !call log_comment( itoa(i)//Curriculum(i)%Code//itoa(k)//CurrProgCode(idx) )
                end if
            end do

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
        if (len_trim(token)==0) return
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


    function isSubject_used_in_college (college_idx, subject_idx)
        ! returns true if subject area is used in a curriculum in college
        logical :: isSubject_used_in_college, found
        integer, intent (in) :: college_idx, subject_idx
        integer :: i
        found = .false.
        if (college_idx==NumColleges) then
            found = .true.
        else
            do i=1,NumCurricula
                if (Curriculum(i)%CollegeIdx/=college_idx) cycle
                if (index_of_subject_in_curriculum(Curriculum(i), subject_idx)>0) then
                    found = .true.
                    exit
                end if
            end do
        end if
        isSubject_used_in_college = found

    end function isSubject_used_in_college


    function isSubject_area_used_in_curriculum (tCurriculum, area)
        ! returns true if subject area is in curriculum
        logical :: isSubject_area_used_in_curriculum, found
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
        isSubject_area_used_in_curriculum = found

    end function isSubject_area_used_in_curriculum


    function text_prerequisite_in_curriculum(crse,tCurriculum)
        ! returns the text representation of the prerequisite of crse
        character (len=255) :: text_prerequisite_in_curriculum, displayStr
        integer, intent (in) :: crse
        type (TYPE_CURRICULUM), intent (in), optional :: tCurriculum
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=255) :: str127(MAX_ALL_SUBJECT_PREREQ)
        integer :: i, j, k
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
            satisfiable = is_prerequisite_satisfiable_in_curriculum(crse, tCurriculum)
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
                                str127(j) = red//trim(tSubject)//e_color
                            end if
                        else
                            str127(j) = red//trim(tSubject)//e_color//'*'
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
            if (displayStr/=SPACE) then
                displayStr = trim(str127(1))//'. '//displayStr
            else
                displayStr = str127(1)
            end if
        end if
        text_prerequisite_in_curriculum = displayStr

    end function text_prerequisite_in_curriculum



    function is_prerequisite_satisfiable_in_curriculum(crse, tCurriculum)
        logical :: is_prerequisite_satisfiable_in_curriculum
        integer, intent (in) :: crse
        type (TYPE_CURRICULUM), intent (in) :: tCurriculum
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
        l = index_of_subject_in_curriculum(tCurriculum,crse)
        do j = ntokens,1,-1
            jdx = tmpPreq(j)
            tSubject = Subject(jdx)%Name
            if (jdx>0) then ! a named subject
                k = index_of_subject_in_curriculum(tCurriculum, jdx)
                if (k>0) then ! found in curriculum
                    ! check if taken earlier
                    if (tCurriculum%SubjectTerm(k)<tCurriculum%SubjectTerm(l)) then
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


    subroutine set_term_offered_accg_to_curricula(thisTerm)
        ! reset Subject()%TermOffered based on appearance of subject in the curricular programs
        integer, intent(in) :: thisTerm
        integer :: kdx, reqd, curr, subj

        Offering(thisTerm,:) = TYPE_OFFERED_SUBJECTS (0, 0, 0, 0, 0, 0, 0, 0)
        do curr=1,NumCurricula
            do kdx=1,Curriculum(curr)%NSubjects
                subj = Curriculum(curr)%SubjectIdx(kdx)
                select case(mod(Curriculum(curr)%SubjectTerm(kdx),3))
                    case (0) ! required during summer
                        Offering(thisTerm,subj)%Demand = Offering(thisTerm,subj)%Demand + 1
                    case (1) ! required during first sem
                        Offering(thisTerm,subj)%NSections = Offering(thisTerm,subj)%NSections + 1
                    case (2) ! required during second sem
                        Offering(thisTerm,subj)%TotalSlots = Offering(thisTerm,subj)%TotalSlots + 1
                end select
            end do
        end do
        do subj=1,NumSubjects
            reqd = 0
            if (Offering(thisTerm,subj)%Demand>0) reqd = reqd + 4 ! summer
            if (Offering(thisTerm,subj)%NSections>0) reqd = reqd + 1 ! first sem
            if (Offering(thisTerm,subj)%TotalSlots >0) reqd = reqd + 2 ! second sem

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
! routines for students
!===========================================================

    function basefile_student(std)
        integer, intent(in) :: std
        character(len=MAX_LEN_STUDENT_CODE+StdNoChars+1) :: basefile_student

        integer :: StdNoYearLen

        StdNoYearLen = index(Student(std)%StdNo,DASH)-1
        if (StdNoYearLen<=0) StdNoYearLen = StdNoChars
        !basefile_student = Student(std)%StdNo(1:StdNoYearLen)//DIRSEP//filename_from(Student(std)%StdNo)
        basefile_student = Student(std)%StdNo(1:StdNoYearLen)//DIRSEP//Student(std)%StdNo

    end function basefile_student


    function year_prefix(S)
        type (TYPE_STUDENT) :: S
        integer :: year_prefix

        integer :: StdNoYearLen

        StdNoYearLen = index(S%StdNo,DASH)-1
        if (StdNoYearLen<=0) StdNoYearLen = StdNoChars
        year_prefix = StdNoYearLen

    end function year_prefix


    subroutine collect_prefix_years()
        ! collect student number prefix
        integer :: idx, jdx, std

        StdNoPrefix = ':'
        do std=1,NumStudents+NumAdditionalStudents
        if (len_trim(Student(std)%StdNo)==0) cycle
            idx = year_prefix(Student(std))
            jdx = index(StdNoPrefix, ':'//Student(std)%StdNo(:idx)//DASH)
            if (jdx==0) StdNoPrefix = trim(StdNoPrefix)//Student(std)%StdNo(:idx)//DASH//':'
        end do
        ! remove blanks & dashes
        idx = 0
        do jdx=1,len_trim(StdNoPrefix)
            if (StdNoPrefix(jdx:jdx)==DASH .or. StdNoPrefix(jdx:jdx)==SPACE) cycle
            idx = idx+1
            StdNoPrefix(idx:idx) = StdNoPrefix(jdx:jdx)
        end do
        StdNoPrefix(idx+1:) = SPACE
        call log_comment('StdNo years - '//trim(StdNoPrefix))

    end subroutine collect_prefix_years


    subroutine initialize_student(S)
        type (TYPE_STUDENT) :: S
        type (TYPE_PRE_ENLISTMENT) :: eList(3)

        call initialize_pre_enlistment(eList(1))
        eList(2:3) = eList(1)
        S = TYPE_STUDENT ( &
            SPACE, & ! gender
            '####-#####', &  ! std no
            'LASTNAME, FIRSTNAME MI', & ! name
            SPACE, &  ! Password
            SPACE, &  ! Adviser
            SPACE, &  ! Scholarship
            SPACE, &  ! HomePSGC
            SPACE, &  ! MotherTongue
            0, & ! Online status
            0, & ! CurriculumIdx
            RESIDENCY_STATUS_NOT_SET, & ! ResidenceStatus
            eList )

    end subroutine initialize_student


    subroutine initialize_student_info()

        StudentInfo%Name = SPACE
        StudentInfo%Email = SPACE
        StudentInfo%Father = SPACE
        StudentInfo%Mother = SPACE
        StudentInfo%Guardian = SPACE
        StudentInfo%BirthDate = SPACE
        StudentInfo%EntryDate = SPACE
        StudentInfo%GraduationDate = SPACE
        StudentInfo%BirthPlace = SPACE
        StudentInfo%BirthPlacePSGC = SPACE
        StudentInfo%HomeStreetAddress = SPACE
        StudentInfo%HomePSGC = SPACE
        StudentInfo%MotherTongue = SPACE
        StudentInfo%LastAttended = SPACE
        StudentInfo%TranscriptRemark = SPACE
        StudentInfo%TranscriptAdditionalRemark = SPACE
        StudentInfo%AdmissionData = SPACE
        StudentInfo%Scholarship = SPACE
        StudentInfo%Gender = SPACE
        StudentInfo%Adviser = SPACE
        StudentInfo%CurriculumIdx = 0
        StudentInfo%ResidenceStatus = RESIDENCY_STATUS_NOT_SET

    end subroutine initialize_student_info


    subroutine insert_student(S, loc)
        type (TYPE_STUDENT) :: S
        integer, intent(out) :: loc

        NumStudents = NumStudents+1
        call check_array_bound (NumStudents, MAX_ALL_STUDENTS, 'MAX_ALL_STUDENTS')
        loc = NumStudents
        do while (loc>1 .and. Student(loc-1)%StdNo>S%StdNo)
            Student(loc) = Student(loc-1)
            loc = loc - 1
        end do
        Student(loc) = S
        !write(*,*) 'INSerted '//S%StdNo, ' to ', loc, '/', NumStudents

    end subroutine insert_student


    subroutine update_student_info(S, idx)
        type (TYPE_STUDENT) :: S
        integer, intent(out) :: idx
        integer :: loc

        idx = index_to_student(S%StdNo)
        if (idx==0) then ! insert so that Students() is sorted
            call insert_student(S, loc)
            idx = -loc
        else
            Student(idx) = S
            !write(*,*) 'UPDated  '//S%StdNo, ' at ', idx, '/', NumStudents
        end if

    end subroutine update_student_info


    subroutine sort_alphabetical_students()
        integer :: i, j, k

        StdRank = 0
        do i=1,NumStudents+NumAdditionalStudents
            StdRank(i) = i
        end do
        do i=1,NumStudents+NumAdditionalStudents-1
            do j=i+1,NumStudents+NumAdditionalStudents
                if (Student(StdRank(i))%Name>Student(StdRank(j))%Name) then
                    k = StdRank(i)
                    StdRank(i) = StdRank(j)
                    StdRank(j) = k
                end if
            end do
        end do

    end subroutine sort_alphabetical_students


    function index_to_student(StdNum)
        integer :: index_to_student
        character (len=MAX_LEN_STUDENT_CODE), intent (in) :: StdNum
        integer :: i, j, sdx

        i = 1
        j = NumStudents
        sdx = 0
        do
            if (i>j) then
                sdx = 0
                !write(*,*) 'Not found : '//StdNum
                exit
            else
                sdx = (i + j)/2
                if (StdNum ==Student(sdx)%StdNo) then
                    !write(*,*) 'Found '//StdNum//' at ', sdx
                    exit
                else if (StdNum <Student(sdx)%StdNo) then
                    !write(*,*) StdNum//' before '//Student(sdx)%StdNo, sdx
                    j = sdx-1
                else
                    i = sdx+1
                    !write(*,*) StdNum//' after '//Student(sdx)%StdNo, sdx
                end if
            end if
        end do
        if (sdx==0) then
            do i=NumStudents+1,NumStudents+NumAdditionalStudents
                if (StdNum==Student(i)%StdNo) then
                    sdx = i
                    exit
                end if
            end do
        end if
        index_to_student = sdx

    end function index_to_student


    function text_student_curriculum(std, long)

        integer, intent (in) :: std
        logical, intent (in), optional :: long

        character (len=MAX_LEN_STUDENT_CODE+MAX_LEN_PERSON_NAME+ &
            MAX_LEN_CURRICULUM_CODE+MAX_LEN_CURRICULUM_NAME+MAX_LEN_CURRICULUM_NAME+&
            MAX_LEN_CURRICULUM_NAME) :: text_student_curriculum
        integer :: idxCURR

        idxCURR = Student(std)%CurriculumIdx
        if (present(long)) then
            text_student_curriculum = trim(Student(std)%StdNo)//SPACE//trim(Student(std)%Name)//COMMA//SPACE// &
                    text_curriculum_info(idxCURR)
        else
            text_student_curriculum = trim(Student(std)%StdNo)//SPACE//trim(Student(std)%Name)//COMMA//SPACE// &
                    Curriculum(idxCURR)%Code
        end if

    end function text_student_curriculum


    subroutine student_copy_to_info(wrkStudent)

        type (TYPE_STUDENT) :: wrkStudent

        call initialize_student_info()
        StudentInfo%Gender = wrkStudent%Gender
        StudentInfo%Name = wrkStudent%Name
        StudentInfo%CurriculumIdx = wrkStudent%CurriculumIdx
        StudentInfo%Adviser = wrkStudent%Adviser
        StudentInfo%Scholarship = wrkStudent%Scholarship
        StudentInfo%HomePSGC = wrkStudent%HomePSGC
        StudentInfo%MotherTongue = wrkStudent%MotherTongue
        StudentInfo%ResidenceStatus = wrkStudent%ResidenceStatus

    end subroutine student_copy_to_info


    subroutine student_copy_from_info(wrkStudent)

        type (TYPE_STUDENT), intent(in out) :: wrkStudent

        if (len_trim(StudentInfo%Gender)>0) then
            wrkStudent%Gender = StudentInfo%Gender
        else
            StudentInfo%Gender = wrkStudent%Gender
        end if
        if (len_trim(StudentInfo%Name)>0) then
            wrkStudent%Name = StudentInfo%Name
        else
            StudentInfo%Name = wrkStudent%Name
        end if
        if (StudentInfo%CurriculumIdx/=NumCurricula) then
            wrkStudent%CurriculumIdx = StudentInfo%CurriculumIdx
        else
            StudentInfo%CurriculumIdx = wrkStudent%CurriculumIdx
        end if
        if (len_trim(StudentInfo%Adviser)>0) then
            wrkStudent%Adviser = StudentInfo%Adviser
        else
            StudentInfo%Adviser = wrkStudent%Adviser
        end if
        if (len_trim(StudentInfo%Scholarship)>0) then
            wrkStudent%Scholarship = StudentInfo%Scholarship
        else
            StudentInfo%Scholarship = wrkStudent%Scholarship
        end if
        if (len_trim(StudentInfo%HomePSGC)>0) then
            wrkStudent%HomePSGC = StudentInfo%HomePSGC
        else
            StudentInfo%HomePSGC = wrkStudent%HomePSGC
        end if
        if (len_trim(StudentInfo%MotherTongue)>0) then
            wrkStudent%MotherTongue = StudentInfo%MotherTongue
        else
            StudentInfo%MotherTongue = wrkStudent%MotherTongue
        end if
        if (StudentInfo%ResidenceStatus>0) then
            wrkStudent%ResidenceStatus = StudentInfo%ResidenceStatus
        else
            StudentInfo%ResidenceStatus = wrkStudent%ResidenceStatus
        end if
!        if (StudentInfo%>0) then
!            wrkStudent% = StudentInfo%
!        else
!            StudentInfo% = wrkStudent%
!        end if

    end subroutine student_copy_from_info

!===========================================================
! routines for grades
!===========================================================

    function isGrade_numeric_pass(GradeIdx, checkREGD)
        logical :: isGrade_numeric_pass
        integer, intent (in) :: GradeIdx
        logical, intent (in), optional :: checkREGD
        logical :: includeREGD
        if (present(checkREGD)) then
            includeREGD = checkREGD
        else
            includeREGD = .true.
        end if
        isGrade_numeric_pass = GradeIdx>=(ZERO_PERCENT_GRADE+75) .or. &
            (GradeIdx>0 .and. GradeIdx<10) .or. &
            GradeIdx==gdxPASS .or. (GradeIdx==gdxREGD .and. includeREGD)

    end function isGrade_numeric_pass


    function isGrade_passing(GradeIdx, checkREGD)
        logical :: isGrade_passing
        integer, intent (in) :: GradeIdx
        logical, intent (in), optional :: checkREGD
        logical :: includeREGD
        if (present(checkREGD)) then
            includeREGD = checkREGD
        else
            includeREGD = .true.
        end if
        isGrade_passing = GradeIdx>=(ZERO_PERCENT_GRADE+75) .or. &
            GradeIdx==gdxS .or. &
            (GradeIdx>0 .and. GradeIdx<10) .or. &
            GradeIdx==gdxPASS .or. (GradeIdx==gdxREGD .and. includeREGD)

    end function isGrade_passing


    function isGrade_failing(GradeIdx)
        logical :: isGrade_failing
        integer, intent (in) :: GradeIdx
        isGrade_failing = GradeIdx==gdx5 .or. GradeIdx==gdxU .or. &
            GradeIdx==gdxDRP .or. GradeIdx==gdxLOA .or. GradeIdx==gdxFAIL .or. &
            (GradeIdx>ZERO_PERCENT_GRADE .and. GradeIdx<(ZERO_PERCENT_GRADE+75))

    end function isGrade_failing


    function isGrade_conditional(GradeIdx)
        logical :: isGrade_conditional
        integer, intent (in) :: GradeIdx
        isGrade_conditional = GradeIdx==gdx4 .or. GradeIdx==gdxINC .or. GradeIdx==gdxNFE

    end function isGrade_conditional


    function index_to_grade (Token)
        integer :: index_to_grade
        character (len=MAX_LEN_TEXT_GRADE), intent (in) :: Token
        integer :: i, j, Idx
        Idx = atoi(Token)
        if (Idx>=50 .and. Idx<=100) then
            Idx = Idx+ZERO_PERCENT_GRADE
        else
            Idx = -99
            do i = 0, 20
                do j=pGrade(i), pGrade(i+1)-1
                    if (txtGrade(j)==Token) then
                        Idx = i
                        exit
                    end if
                end do
                if (Idx>=0) exit
            end do
            if (Idx<0) Idx = 0
            if (UniversityCode(1:3)=='CSU') then ! convert to percentage
                select case (Idx)
                    case (1) ! 1.0
                        Idx = ZERO_PERCENT_GRADE+99 !99:100
                    case (2) ! 1.25
                        Idx = ZERO_PERCENT_GRADE+97 ! 96:98
                    case (3) ! 1.5
                        Idx = ZERO_PERCENT_GRADE+94 ! 93:95
                    case (4) ! 1.75
                        Idx = ZERO_PERCENT_GRADE+91 ! 90:92
                    case (5) ! 2.0
                        Idx = ZERO_PERCENT_GRADE+88 ! 87:89
                    case (6) ! 2.25
                        Idx = ZERO_PERCENT_GRADE+85 ! 84:86
                    case (7) ! 2.5
                        Idx = ZERO_PERCENT_GRADE+82 ! 81:83
                    case (8) ! 2.75
                        Idx = ZERO_PERCENT_GRADE+79 ! 78:80
                    case (9) ! 3.0
                        Idx = ZERO_PERCENT_GRADE+76 ! 75:77
                    case (12) ! 5.0
                        Idx = ZERO_PERCENT_GRADE+70
                end select
            end if
        end if
        index_to_grade = Idx

    end function index_to_grade


!===========================================================
! routines for sections
!===========================================================


    subroutine initialize_section(S)
        type (TYPE_SECTION) :: S
        S = TYPE_SECTION (SPACE, SPACE, SPACE, &
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    end subroutine initialize_section


    function index_to_section(tSection, thisTerm)
        integer :: index_to_section, thisTerm
        character (len=MAX_LEN_CLASS_ID), intent (in) :: tSection
        integer :: i, sdx
        sdx = 0
        if (len_trim(tSection)==0) return
        do i=1,NumSections(thisTerm)
            if (tSection/=Section(thisTerm,i)%ClassId) cycle
            sdx = i
            exit
        end do
        index_to_section = sdx

    end function index_to_section


    function is_lecture_class(sect, thisTerm)
        integer, intent (in) :: sect, thisTerm
        logical :: is_lecture_class
        ! returns true if sect is a lecture class (no DASH in section code)
        is_lecture_class = index(Section(thisTerm,sect)%Code,DASH)==0 .or. &
           Subject(Section(thisTerm,sect)%SubjectIdx)%Name(1:3)=='PE '

    end function is_lecture_class


    function text_days_of_section(tSection)
        type (TYPE_SECTION), intent(in) :: tSection
        character (len=7) :: line, text_days_of_section
        integer :: j
        if (tSection%NMeets>0) then
            line = SPACE
            do j=tSection%NMeets,1,-1
                select case (tSection%DayIdx(j))
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
                        line = 'Sa'//line
                    case (7)
                        line = 'Su'//line
                end select
            end do
        else
            line = 'TBA'
        end if
        text_days_of_section = line

    end function text_days_of_section


    subroutine offerings_sort(thisTerm)
        integer, intent (in) :: thisTerm
        integer :: i, j
        do i=1,NumSections(thisTerm)-1
            do j=i+1,NumSections(thisTerm)
                if (Section(thisTerm,i)%ClassId>Section(thisTerm,j)%ClassId) then
                    Section(thisTerm,0) = Section(thisTerm,i)
                    Section(thisTerm,i) = Section(thisTerm,j)
                    Section(thisTerm,j) = Section(thisTerm,0)
                end if
            end do
        end do
        call initialize_section(Section(thisTerm,0))

    end subroutine offerings_sort


    subroutine offerings_summarize(thisTerm, DeptIdx)
        integer, intent (in) :: thisTerm
        integer, intent (in), optional :: DeptIdx
        integer :: k, l, filter_dept

        if (present(DeptIdx)) then
            filter_dept = DeptIdx
        else
            filter_dept = 0
        end if
        Offering(thisTerm,:) = TYPE_OFFERED_SUBJECTS (0, 0, 0, 0, 0, 0, 0, 0)
        do k=1,NumSections(thisTerm)

            l = Section(thisTerm,k)%SubjectIdx
            if (l==0) cycle ! section was deleted
            if (filter_dept>0) then
                if (filter_dept/=Section(thisTerm,k)%DeptIdx) cycle
            end if
            ! lecture-lab ?
            if (.not. isSubject_lecture_lab(l)) then ! lecture only or lab only
                Offering(thisTerm,l)%TotalSlots = Offering(thisTerm,l)%TotalSlots + Section(thisTerm,k)%Slots
                Offering(thisTerm,l)%NSections = Offering(thisTerm,l)%NSections + 1
            else ! a lecture-lab subject
                if (is_lecture_class(k, thisTerm)) then ! this is the lecture section
                else ! this is the lab section
                    Offering(thisTerm,l)%TotalSlots = Offering(thisTerm,l)%TotalSlots + Section(thisTerm,k)%Slots
                    Offering(thisTerm,l)%NSections = Offering(thisTerm,l)%NSections + 1
                end if
            end if
        end do

    end subroutine offerings_summarize


    subroutine count_sections_by_dept(thisTerm)
        integer, intent (in) :: thisTerm
        integer :: sect, dept
        ScheduleCount(thisTerm,:) = 0
#if defined UPLB
        ! Subject administered by departments
        do sect=1,NumSections(thisTerm)
            if (Section(thisTerm,sect)%SubjectIdx==0) cycle
            dept = Section(thisTerm,sect)%DeptIdx
            ScheduleCount(thisTerm,dept) = ScheduleCount(thisTerm,dept) + 1
        end do
#else
        ! Subjects administered by program
        do sect=1,NumSections(thisTerm)
            if (Section(thisTerm,sect)%SubjectIdx==0) cycle
            dept = Section(thisTerm,sect)%DeptIdx
            ScheduleCount(thisTerm,dept) = max(atoi(Section(thisTerm,sect)%Code(2:)), ScheduleCount(thisTerm,dept))
        end do
#endif

    end subroutine count_sections_by_dept


    subroutine delete_sections_of_dept(thisTerm, DeptIdx)
        integer, intent (in) :: thisTerm
        integer, intent (in) :: DeptIdx
        integer :: sect

        do sect=1,NumSections(thisTerm)
            if (Section(thisTerm,sect)%SubjectIdx==0) cycle
            if (DeptIdx==Section(thisTerm,sect)%DeptIdx) then
                call initialize_section(Section(thisTerm,sect))
            end if
        end do

    end subroutine delete_sections_of_dept


    function is_regular_schedule(sect, thisTerm)
        ! returns true if section meetings have same (time, room, teacher), different days
        integer, intent (in) :: sect, thisTerm
        logical :: is_regular_schedule
        logical :: sameTeacher, sameRoom, sameTime
        integer :: mdx

        sameTime = .true.
        sameRoom = .true.
        sameTeacher = .true.
        do mdx=2,Section(thisTerm,sect)%NMeets
          if (Section(thisTerm,sect)%bTimeIdx(1)/=Section(thisTerm,sect)%bTimeIdx(mdx) .or. &
              Section(thisTerm,sect)%eTimeIdx(1)/=Section(thisTerm,sect)%eTimeIdx(mdx)) sameTime = .false.
          if (Section(thisTerm,sect)%RoomIdx(1)/=Section(thisTerm,sect)%RoomIdx(mdx)) sameRoom = .false.
          if (Section(thisTerm,sect)%TeacherIdx(1)/=Section(thisTerm,sect)%TeacherIdx(mdx)) sameTeacher = .false.
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


    function get_next_lab_section(sect, thisTerm)
        ! return code for next lab section of sect
        integer, intent (in) :: sect, thisTerm
        character(len=MAX_LEN_SECTION_CODE) :: get_next_lab_section
        character(len=MAX_LEN_CLASS_ID) :: tClassId

        integer :: i
        logical :: found
        i = 0
        found = .true.
        do while (found)
            i = i + 1
            tClassId = trim(Section(thisTerm,sect)%ClassId)//DASH//trim(itoa(i))//'L'
            found = index_to_section(tClassId, thisTerm)>0
        end do
        get_next_lab_section = trim(Section(thisTerm,sect)%Code)//DASH//trim(itoa(i))//'L'

    end function get_next_lab_section


!===========================================================
! routines for blocks
!===========================================================


    subroutine initialize_block(B)
        type (TYPE_BLOCK) :: B
        B = TYPE_BLOCK ('Block Code', 'Block Name and Description', 0, 0, 0, 0, 0, 0, 0, 0, .false.)

    end subroutine initialize_block


    function index_to_block(tBlock, thisTerm)
        integer :: index_to_block, thisTerm
        character (len=MAX_LEN_BLOCK_CODE), intent (in) :: tBlock
        integer :: i, sdx

        sdx = 0
        if (len_trim(tBlock)==0) return
        do i=1,NumBlocks(thisTerm)
            if (tBlock==Block(thisTerm,i)%BlockID) then
                sdx = i
                exit
            end if
        end do
        index_to_block = sdx

    end function index_to_block


    subroutine sort_alphabetical_blocks(thisTerm)
        integer, intent(in) :: thisTerm
        integer :: idx, jdx, kdx
        ! sort blocks
        kdx = 0
        do jdx=1,NumBlocks(thisTerm)-1
            do idx=jdx+1,NumBlocks(thisTerm)
                if (Block(thisTerm,jdx)%BlockID>Block(thisTerm,idx)%BlockID) then
                    Block(thisTerm,kdx) = Block(thisTerm,idx)
                    Block(thisTerm,idx) = Block(thisTerm,jdx)
                    Block(thisTerm,jdx) = Block(thisTerm,kdx)
                end if
            end do
        end do
        call initialize_block(Block(thisTerm,kdx))

    end subroutine sort_alphabetical_blocks


!    subroutine delete_blocks_from_dept(NumBlocks, Block,  DeptIdx)
!        integer, intent(in out) :: NumBlocks
!        type (TYPE_BLOCK), dimension(0:), intent(in out) :: Block
!        integer, intent (in) :: DeptIdx
!        integer :: blk, idx
!
!        !write(*,*) 'Removing blocks in '//Department(DeptIdx)%Code ! , ', total=',  NumBlocks
!        do blk=1,NumBlocks
!            if (DeptIdx==Block(blk)%DeptIdx) then
!                !write(*,*) 'Removing block '//Block(blk)%BlockID
!                call initialize_block(Block(blk))
!            end if
!        end do
!        blk = 0
!        do idx=1,NumBlocks
!            if (Block(idx)%CurriculumIdx/=0) then
!                blk = blk+1
!                Block(blk) = Block(idx)
!            end if
!        end do
!        NumBlocks = blk
!        !write(*,*) NumBlocks, ' left'
!
!    end subroutine delete_blocks_from_dept


    subroutine delete_section_from_blocks(sect, thisTerm)
        integer, intent(in) :: sect, thisTerm
        integer :: i, j
        do i=1,NumBlocks(thisTerm)
            do j=1,Block(thisTerm,i)%NumClasses
                if (Block(thisTerm,i)%Section(j)/=sect) cycle
                Block(thisTerm,i)%Section(j) = 0
                Block(thisTerm,i)%isDirty = .true.
                exit
            end do
        end do
        call initialize_section(Section(thisTerm,sect))

    end subroutine delete_section_from_blocks


    function find_section_in_blocks(tClassId, thisTerm)
        integer :: find_section_in_blocks
        character(len=MAX_LEN_CLASS_ID) :: tClassId
        integer, intent(in) :: thisTerm
        integer :: sect, i, j

        find_section_in_blocks = 0
        sect = index_to_section(tClassId, thisTerm)
        if (sect==0) return
        do i=1,NumBlocks(thisTerm)
            do j=1,Block(thisTerm,i)%NumClasses
                if (Block(thisTerm,i)%Section(j)/=sect) cycle
                find_section_in_blocks = i
                return
            end do
        end do

    end function find_section_in_blocks


!===========================================================
! routines for timetables
!===========================================================


    subroutine timetable_clear(TimeTable)
        integer, dimension (60,7), intent(out) :: TimeTable
        TimeTable = 0
        TimeTable(59,1:7) = 60 ! earliest time
        TimeTable(60,1:7) = 1  ! latest time

    end subroutine timetable_clear


    function is_conflict_timetable_with_section(thisTerm, sect, TimeTable)
        logical :: is_conflict_timetable_with_section
        integer, intent (in) :: sect, thisTerm
        integer, dimension(60,7), intent (in) :: TimeTable
        integer :: idx, jdx, mdx
        !
        is_conflict_timetable_with_section = .false.
        if (sect>0) then ! a valid sect pointer
            loop_meets : &
            do mdx=1,Section(thisTerm,sect)%NMeets
                jdx = Section(thisTerm,sect)%DayIdx(mdx)
                if (jdx==0) cycle
                do idx = Section(thisTerm,sect)%bTimeIdx(mdx), Section(thisTerm,sect)%eTimeIdx(mdx)-1
                    if (TimeTable(idx, jdx)/=0) then
                        is_conflict_timetable_with_section = .true.
                        exit loop_meets
                    end if
                end do
            end do loop_meets
        end if

    end function is_conflict_timetable_with_section


    subroutine timetable_add_section(thisterm, sect, TimeTable, loc)
        integer, intent (in) :: sect, thisTerm
        integer, dimension (60,7), intent (in out) :: TimeTable
        integer, intent (in out) :: loc
        integer :: idx, jdx, mdx
        integer, dimension (60,7) :: tTable
        !
        if (sect>0) then ! a valid sect pointer
            tTable = TimeTable
            do mdx=1,Section(thisTerm,sect)%NMeets
                jdx = Section(thisTerm,sect)%DayIdx(mdx)
                if (jdx==0) cycle
                do idx = Section(thisTerm,sect)%bTimeIdx(mdx), Section(thisTerm,sect)%eTimeIdx(mdx)-1
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
            do mdx=1,Section(thisTerm,sect)%NMeets
                jdx = Section(thisTerm,sect)%DayIdx(mdx)
                if (jdx==0) cycle
                !do idx = Section(thisTerm,sect)%bTimeIdx(mdx), Section(thisTerm,sect)%eTimeIdx(mdx)-1
                !  TimeTable(idx, jdx) = sect
                !end do
                if (TimeTable(59,jdx)>Section(thisTerm,sect)%bTimeIdx(mdx)) then
                    TimeTable(59,jdx) = Section(thisTerm,sect)%bTimeIdx(mdx)
                end if
                if (TimeTable(60,jdx)<Section(thisTerm,sect)%eTimeIdx(mdx)) then
                    TimeTable(60,jdx) = Section(thisTerm,sect)%eTimeIdx(mdx)
                end if
            end do
        else if (sect<0) then
            call log_comment('Invalid section '//itoa(sect)//' in timetable_add_section(); called from '//itoa(loc))
        end if

    end subroutine timetable_add_section


    function is_conflict_timetable_with_section_meetings(thisTerm, sect, n_meetings, meetings, TimeTable)
        logical :: is_conflict_timetable_with_section_meetings
        integer, intent (in) :: thisTerm, sect, n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer, dimension(60,7), intent (in) :: TimeTable
        integer :: idx, jdx, kdx, mdx
        !
        is_conflict_timetable_with_section_meetings = .false.
        if (sect>0) then ! a valid sect pointer
            loop_meets : &
            do kdx=1,n_meetings
                mdx=meetings(kdx)
                jdx = Section(thisTerm,sect)%DayIdx(mdx)
                if (jdx==0) cycle
                do idx = Section(thisTerm,sect)%bTimeIdx(mdx), Section(thisTerm,sect)%eTimeIdx(mdx)-1
                    if (TimeTable(idx, jdx)/=0) then
                        is_conflict_timetable_with_section_meetings = .true.
                        exit loop_meets
                    end if
                end do
            end do loop_meets
        end if

    end function is_conflict_timetable_with_section_meetings


    subroutine timetable_add_meetings_of_section(thisTerm, sect, n_meetings, meetings, TimeTable, loc)
        integer, intent (in) :: thisTerm, sect, n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer, intent (in out) :: TimeTable(60,7), loc
        integer :: idx, jdx, kdx, mdx
        integer :: tTable(60,7)

        tTable = TimeTable
        do kdx=1,n_meetings
            mdx = meetings(kdx)
            jdx = Section(thisTerm,sect)%DayIdx(mdx)
            if (jdx==0) cycle
            do idx = Section(thisTerm,sect)%bTimeIdx(mdx), Section(thisTerm,sect)%eTimeIdx(mdx)-1
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
            jdx = Section(thisTerm,sect)%DayIdx(mdx)
            if (jdx==0) cycle
            !do idx = Section(thisTerm,sect)%bTimeIdx(mdx), Section(thisTerm,sect)%eTimeIdx(mdx)-1
            !  TimeTable(idx, jdx) = sect
            !end do
            if (TimeTable(59,jdx)>Section(thisTerm,sect)%bTimeIdx(mdx)) then
                TimeTable(59,jdx) = Section(thisTerm,sect)%bTimeIdx(mdx)
            end if
            if (TimeTable(60,jdx)<Section(thisTerm,sect)%eTimeIdx(mdx)) then
                TimeTable(60,jdx) = Section(thisTerm,sect)%eTimeIdx(mdx)
            end if
        end do

    end subroutine timetable_add_meetings_of_section


    function is_conflict_timetable_with_struct_section(wrkSection, m_first, m_last, TimeTable)
        logical :: is_conflict_timetable_with_struct_section
        type (TYPE_SECTION), intent (in) :: wrkSection
        integer, intent (in) :: m_first, m_last
        integer, dimension(60,7), intent (in) :: TimeTable
        integer :: idx, jdx, mdx
        !
        is_conflict_timetable_with_struct_section = .false.
        loop_meets : &
        do mdx=m_first,m_last
            jdx = wrkSection%DayIdx(mdx)
            if (jdx==0) cycle
            do idx = wrkSection%bTimeIdx(mdx), wrkSection%eTimeIdx(mdx)-1
                if (TimeTable(idx, jdx)/=0) then
                    is_conflict_timetable_with_struct_section = .true.
                    exit loop_meets
                end if
            end do
        end do loop_meets

    end function is_conflict_timetable_with_struct_section


    subroutine timetable_add_struct_section(Section, TimeTable, loc)
        type (TYPE_SECTION), intent (in) :: Section
        integer, dimension (60,7), intent (in out) :: TimeTable
        integer, intent (in out) :: loc
        integer :: idx, jdx, mdx
        integer, dimension (60,7) :: tTable
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


    subroutine timetable_remove_section(thisTerm, sect, TimeTable, loc)
        integer, intent (in) :: sect, thisTerm
        integer, dimension (60,7), intent (in out) :: TimeTable
        integer, intent (in out) :: loc
        integer :: idx, jdx, mdx
        !
        if (sect>0) then ! a valid sect pointer
            do mdx=1,Section(thisTerm,sect)%NMeets
                jdx = Section(thisTerm,sect)%DayIdx(mdx)
                if (jdx==0) cycle
                do idx = Section(thisTerm,sect)%bTimeIdx(mdx), Section(thisTerm,sect)%eTimeIdx(mdx)-1
                    if (TimeTable(idx, jdx)/=sect) then
                        call log_comment('ERROR detected in timetable_remove_section(); called from '//itoa(loc))
                        loc = TimeTable(idx,jdx)
                        return
                    end if
                end do
            end do
            ! nothing unusual
            do mdx=1,Section(thisTerm,sect)%NMeets
                jdx = Section(thisTerm,sect)%DayIdx(mdx)
                if (jdx==0) cycle
                do idx = Section(thisTerm,sect)%bTimeIdx(mdx), Section(thisTerm,sect)%eTimeIdx(mdx)-1
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


    function is_consistent_section_hours_with_subject_defn(tSection, term, ignore)
        type (TYPE_SECTION), intent (in) :: tSection
        integer, intent (in) :: term
        logical, intent (in), optional :: ignore
        logical :: is_consistent_section_hours_with_subject_defn, display
        integer :: idx
        real :: n15, SectionHours

        is_consistent_section_hours_with_subject_defn = .true.

        ! disable check for summer schedules
        if (term==3) return

        n15 = 0.0 ! no. of 15-minute intervals
        do idx=1,tSection%NMeets
            n15 = n15 + tSection%eTimeIdx(idx) - tSection%bTimeIdx(idx)
        end do
        if (n15==0.0) return ! assume TBA is OK

        if (present(ignore)) then
            display = .not. ignore
        else
            display = .false.
        end if
        ! figure out how many hours based on subject type and section code
        idx = tSection%SubjectIdx
        if (isSubject_lecture_lab(idx)) then
            if (index(tSection%ClassId,DASH)==0) then
                SectionHours = Subject(idx)%LectHours
            else
                SectionHours = Subject(idx)%LabHours
            end if
        else if (Subject(idx)%LectHours > 0.0) then
            SectionHours = Subject(idx)%LectHours
        else ! if (Subject(idx)%LabHours > 0) then
            SectionHours = Subject(idx)%LabHours
        end if
        if (abs(4.0*SectionHours-n15)>0.25) then
            is_consistent_section_hours_with_subject_defn = .false.
            call html_comment(trim(tSection%ClassId)//': scheduled hours ('// &
                trim(ftoa(n15/4.0,5))//') is inconsistent with subject parameter hours ('//trim(ftoa(SectionHours,5))//')')
            if (display) then
                write(unitHTML,AFORMAT) trim(tSection%ClassId)//': scheduled hours ('// &
                    trim(ftoa(n15/4.0,5))//') is inconsistent with subject parameter hours ('//trim(ftoa(SectionHours,5))//')'
            end if
        end if

    end function is_consistent_section_hours_with_subject_defn


    function is_conflict_free_section_hours(tSection, thisTerm)
        logical :: is_conflict_free_section_hours, tDetermination
        type (TYPE_SECTION), intent (in) :: tSection
        integer, intent (in) :: thisTerm
        integer, dimension(60,7) :: TimeTable
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
            if (isSubject_lecture_lab(tSection%SubjectIdx)) then ! subject is lecture-lab
                ! a lab section ?
                i = index(tSection%ClassId,DASH)
                if (i>0) then
                    ! find lecture section
                    tClassId = tSection%ClassId(:i-1)
                    j = index_to_section(tClassId, thisTerm)
                    if (j>0) then
                        if (is_conflict_timetable_with_section(thisTerm, j, TimeTable)) tDetermination = .false.
                    end if
                end if
            end if
        end if
        is_conflict_free_section_hours = tDetermination

    end function is_conflict_free_section_hours


    subroutine sections_compound(thisTerm, nconflicts, ignoreMismatch)
        integer, intent (in) :: thisTerm
        integer, intent (out) :: nconflicts
        logical, intent (in), optional :: ignoreMismatch
#ifdef DEBUG
        character (len=255) :: line
#endif
        !character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_CLASS_ID) :: tSection
        integer :: crse, sect, sdx, i, j, idx!, n15, tHours
        integer :: TimeTable(60,7)
        logical :: ignore

        if (present(ignoreMismatch)) then
            ignore = ignoreMismatch
        else
            ignore = .false.
        end if

        nconflicts = 0
        !
        ! add lecture times to laboratory section
        do sect=1,NumSections(thisTerm)
            crse = Section(thisTerm,sect)%SubjectIdx
            if (crse<0) cycle ! done previously

            if (.not. is_consistent_section_hours_with_subject_defn(Section(thisTerm,sect), thisTerm, ignore)) then
                if (.not. ignore) nconflicts = nconflicts + 1
            end if

            !if (Subject(crse)%LectHours * Subject(crse)%LabHours==0) cycle
            if (.not. isSubject_lecture_lab(crse)) cycle
            ! subject is lecture-lab
            if (is_lecture_class(sect, thisTerm)) cycle ! a lecture section

            !write(*,*) 'Laboratory class '//Section(thisTerm,sect)%ClassId
            call timetable_clear(TimeTable)
            idx = -10
            call timetable_add_section(thisTerm, sect, TimeTable, idx)
            if ( idx/=-10) then
                if (.not. ignore) nconflicts = nconflicts + 1
                write(unitHTML,AFORMAT) Section(thisTerm,sect)%ClassId//' conflicts with '//Section(thisTerm,idx)%ClassId
            end if
            !
            !tSubject = Subject(crse)%Name
            !tSection = trim(tSubject)//SPACE//Section(thisTerm,sect)%Code(:j-1)
            tSection = Section(thisTerm,sect)%ClassId
            j = len_trim(tSection)
            do while (tSection(j:j)/=DASH)
                j = j-1
            end do
            tSection(j:) = SPACE
            sdx = index_to_section(tSection, thisTerm)
            !write(*,*) 'lecture class '//tSection, sdx
            if (sdx>0) then ! lecture found
                idx = -10
                call timetable_add_section(thisTerm, sdx, TimeTable, idx)
                if ( idx/=-10) then
                    if (.not. ignore) nconflicts = nconflicts + 1
                    write(unitHTML,AFORMAT) Section(thisTerm,sdx)%ClassId//' conflicts with '//Section(thisTerm,idx)%ClassId
                end if
                j = Section(thisTerm,sect)%NMeets
                Section(thisTerm,sect)%NMeets = j + Section(thisTerm,sdx)%NMeets
                do i=1,Section(thisTerm,sdx)%NMeets
                    Section(thisTerm,sect)%DayIdx(j+i) = Section(thisTerm,sdx)%DayIdx(i)
                    Section(thisTerm,sect)%bTimeIdx(j+i) = Section(thisTerm,sdx)%bTimeIdx(i)
                    Section(thisTerm,sect)%eTimeIdx(j+i) = Section(thisTerm,sdx)%eTimeIdx(i)
                    Section(thisTerm,sect)%RoomIdx(j+i) = Section(thisTerm,sdx)%RoomIdx(i)
                end do
            else
                if (.not. ignore) nconflicts = nconflicts + 1
                write(unitHTML,AFORMAT) 'Lecture class '//tSection//'not found!'
            end if
        end do
        !
        ! "erase" lecture times of lecture-laboratory subjects
        do sect=1,NumSections(thisTerm)
            crse = Section(thisTerm,sect)%SubjectIdx
            if (crse<0) cycle ! done previously
            !if (Subject(crse)%LectHours * Subject(crse)%LabHours==0) cycle
            if (.not. isSubject_lecture_lab(crse)) cycle
            ! subject is lecture-lab
            j = index(Section(thisTerm,sect)%Code,DASH)
            if (j==0) then
                Section(thisTerm,sect)%SubjectIdx = -(crse-NumDummySubjects)
            ! a lecture section
            end if
        end do
        !
        sect = 0
        call initialize_section(Section(thisTerm,sect))
        do sdx=1,NumSections(thisTerm)
            crse = Section(thisTerm,sdx)%SubjectIdx
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
                Section(thisTerm,sect) = Section(thisTerm,sdx)
            end if
#ifdef DEBUG
            line = trim(line)//trim(Section(thisTerm,sdx)%ClassId)//COMMA//itoa(Section(thisTerm,sdx)%Slots)
            do i=1,Section(thisTerm,sdx)%NMeets
                line = trim(line)//COMMA// &
                    trim(text_time_period(Section(thisTerm,sdx)%bTimeIdx(i), Section(thisTerm,sdx)%eTimeIdx(i)))//COMMA// &
                    trim(txtDay(Section(thisTerm,sdx)%DayIdx(i)))//COMMA// &
                    Room(Section(thisTerm,sdx)%RoomIdx(i))%Code
            end do
            write(*,*) trim(line)
#endif
        end do
        NumSections(thisTerm) = sect
        call initialize_section(Section(thisTerm,sect+1))

    end subroutine sections_compound


    subroutine meetings_of_section_by_teacher(thisTerm, section_index, teacher_index, n_meetings, meetings)
        integer, intent(in) :: thisTerm, section_index, teacher_index
        integer, intent(out) :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer :: i
        n_meetings = 0
        meetings = 0
        do i=1,Section(thisTerm,section_index)%NMeets
            if (Section(thisTerm,section_index)%TeacherIdx(i)==teacher_index) then
                n_meetings = 1+n_meetings
                meetings(n_meetings) = i
            end if
        end do

    end subroutine meetings_of_section_by_teacher


    subroutine meetings_of_section_in_room(thisTerm, section_index, room_index, n_meetings, meetings)
        integer, intent(in) :: thisTerm, section_index, room_index
        integer, intent(out) :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer :: i
        n_meetings = 0
        meetings = 0
        do i=1,Section(thisTerm,section_index)%NMeets
            if (Section(thisTerm,section_index)%RoomIdx(i)==room_index) then
                n_meetings = 1+n_meetings
                meetings(n_meetings) = i
            end if
        end do

    end subroutine meetings_of_section_in_room


    subroutine timetable_undesirability(ns, thisTerm, TimeTable)
        integer, intent(in) :: ns
        integer, intent (in) :: thisTerm
        integer, dimension(60,7), intent(in out)  :: TimeTable

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

        do sect=NumSections(thisTerm)+1,NumSections(thisTerm)+ns
            i = sect-NumSections(thisTerm)

            !write(*,*) i, sect, Section(thisTerm,sect)%ClassId
            do mdx=1,Section(thisTerm,sect)%NMeets
                day_idx = Section(thisTerm,sect)%DayIdx(mdx)
                if (day_idx==0) cycle
                ! need to travel?
                ! previous class is
                tsect = 0
                if (Section(thisTerm,sect)%bTimeIdx(mdx) > 1) then
                    tsect = TimeTable(Section(thisTerm,sect)%bTimeIdx(mdx)-1, day_idx)
                end if
                if (tsect > 0) then
                    do tmdx=1,Section(thisTerm,tsect)%NMeets ! find the room
                        if (day_idx == Section(thisTerm,tsect)%DayIdx(tmdx) .and. & ! same day
                        Section(thisTerm,sect)%bTimeIdx(mdx) == Section(thisTerm,tsect)%eTimeIdx(tmdx) ) then ! previous class
                            if (Room(Section(thisTerm,tsect)%RoomIdx(tmdx))%Cluster /=  &
                            Room(Section(thisTerm,sect)%RoomIdx(mdx))%Cluster ) &
                            Travels(i) = Travels(i) + 1
                            !write(*,*) 'Travel from '//Room(Section(thisTerm,tsect)%RoomIdx(tmdx))%Code, &
                            !  Room(Section(thisTerm,sect)%RoomIdx(mdx))%Code
                        end if
                    end do
                end if
                ! next class is
                tsect = 0
                if (Section(thisTerm,sect)%eTimeIdx(mdx) < 58) then
                    tsect = TimeTable(Section(thisTerm,sect)%eTimeIdx(mdx)+1, day_idx)
                end if
                if (tsect > 0) then
                    do tmdx=1,Section(thisTerm,tsect)%NMeets ! find the room
                        if (day_idx == Section(thisTerm,tsect)%DayIdx(tmdx) .and. & ! same day
                        Section(thisTerm,sect)%eTimeIdx(mdx) == Section(thisTerm,tsect)%bTimeIdx(tmdx) ) then ! next class
                            if (Room(Section(thisTerm,tsect)%RoomIdx(tmdx))%Cluster /=  &
                            Room(Section(thisTerm,sect)%RoomIdx(mdx))%Cluster ) &
                            Travels(i) = Travels(i) + 1
                            !write(*,*) 'Travel from '//Room(Section(thisTerm,tsect)%RoomIdx(tmdx))%Code, &
                            !  Room(Section(thisTerm,sect)%RoomIdx(mdx))%Code
                        end if
                    end do
                end if
                ! how many early days
                if (Section(thisTerm,sect)%bTimeIdx(mdx) < TIME_INDEX_EARLY_DAY) then
                    EarlyDays(i) = EarlyDays(i) + 1
                    !write(*,*) 'Early day '//Section(thisTerm,sect)%ClassId, txtDay(Section(thisTerm,sect)%DayIdx(mdx)), &
                    !  txtTime(Section(thisTerm,sect)%bTimeIdx(mdx))
                    ! long day?
                    if (TimeTable(60,day_idx) >= TIME_INDEX_LATE_DAY) LongDays(i) = LongDays(i) + 1
                end if
                ! how many late days
                if (Section(thisTerm,sect)%eTimeIdx(mdx) > TIME_INDEX_LATE_DAY) then
                    LateDays(i) = LateDays(i) + 1
                    !write(*,*) 'Late day '//Section(thisTerm,sect)%ClassId, txtDay(Section(thisTerm,sect)%DayIdx(mdx)), &
                    !  txtTime(Section(thisTerm,sect)%eTimeIdx(mdx))
                    ! long day?
                    if (TimeTable(59,day_idx) <= TIME_INDEX_EARLY_DAY) LongDays(i) = LongDays(i) + 1
                end if
                ! how many days will it not allow lunch time?
                if (.not. (Section(thisTerm,sect)%eTimeIdx(mdx) <= TIME_INDEX_BEGIN_LUNCH .or. &
                Section(thisTerm,sect)%bTimeIdx(mdx) >= TIME_INDEX_END_LUNCH) ) then
                    ! temporarily add to timetable
                    do k = Section(thisTerm,sect)%bTimeIdx(mdx), Section(thisTerm,sect)%eTimeIdx(mdx)-1
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
                      !write(*,*) 'Hungry day '//Section(thisTerm,sect)%ClassId, txtDay(Section(thisTerm,sect)%DayIdx(mdx))
                    end if
                    ! remove from timetable
                    do k = Section(thisTerm,sect)%bTimeIdx(mdx), Section(thisTerm,sect)%eTimeIdx(mdx)-1
                        TimeTable(k, day_idx) = 0
                    end do
                end if
            end do !  mdx=1,Section(thisTerm,sect)%NMeets

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


    subroutine timetable_meetings_of_block(thisTerm, block_index, to_skip, len_list, list, TimeTable, conflicted)
        integer, intent(in) :: thisTerm, block_index, to_skip
        integer, intent (out) :: len_list, list(:)
        integer, dimension(60,7), intent (out) :: TimeTable
        logical, intent(out) :: conflicted
        integer :: i, j, conflict_loc, sdx, sect, crse, lect
        integer :: meetings(MAX_SECTION_MEETINGS)
        character(len=MAX_LEN_CLASS_ID) :: tClassId

        len_list = 0 ! initialize list
        call timetable_clear(TimeTable) ! initialize weekly schedule
        conflicted = .false.
        do sdx=1,Block(thisTerm,block_index)%NumClasses ! loop over enries in Block(thisTerm,)
            sect = Block(thisTerm,block_index)%Section(sdx)
            if (sect==0) cycle ! not accommodated
            if (sect==to_skip) cycle ! skip specified section (if, for example, the section is being edited)
            crse = Section(thisTerm,sect)%SubjectIdx

            if (isSubject_lecture_lab(crse)) then ! subject is lecture-lab
                ! add lecture
                j = index(Section(thisTerm,sect)%Code,DASH)
                tClassId = trim(Subject(crse)%Name)//SPACE//Section(thisTerm,sect)%Code(:j-1)
                lect = index_to_section(tClassId, thisTerm)
                do i=1,Section(thisTerm,lect)%NMeets
                    meetings(1) = i
                    list(len_list+1) = lect
                    list(len_list+2) = i
                    conflict_loc = -10 ! assume no conflicting section
                    call timetable_add_meetings_of_section(thisTerm, lect, 1, meetings, TimeTable, conflict_loc) ! add meeting to weekly schedule
                    if (conflict_loc/=-10) then
                        conflicted = .true.
                    else
                        conflict_loc = 0
                    end if
                    list(len_list+3) = conflict_loc ! index to conflicting section, if any
                    len_list = len_list+3
                end do
            end if
            ! add section
            do i=1,Section(thisTerm,sect)%NMeets
                meetings(1) = i
                list(len_list+1) = sect
                list(len_list+2) = i
                conflict_loc = -10 ! assume no conflicting section
                call timetable_add_meetings_of_section(thisTerm, sect, 1, meetings, TimeTable, conflict_loc) ! add meeting to weekly schedule
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

    end subroutine timetable_meetings_of_block


    subroutine timetable_meetings_of_student(thisTerm, iStd, to_skip, len_list, list, TimeTable, conflicted)

        integer, intent (in) :: thisTerm
        integer, intent(in) :: iStd, to_skip
        integer, intent (out) :: len_list, list(:)
        integer, dimension(60,7), intent (out) :: TimeTable
        logical, intent(out) :: conflicted
        integer :: i, j, conflict_loc, sdx, sect, crse, lect
        integer :: meetings(MAX_SECTION_MEETINGS)
        character(len=MAX_LEN_CLASS_ID) :: tClassId

        len_list = 0 ! initialize list
        call timetable_clear(TimeTable) ! initialize weekly schedule
        conflicted = .false.
        do sdx=1,Student(iStd)%Enlistment(thisTerm)%NPriority+Student(iStd)%Enlistment(thisTerm)%NAlternates + &
                 Student(iStd)%Enlistment(thisTerm)%NCurrent ! loop over enries in eList()
            sect = Student(iStd)%Enlistment(thisTerm)%Section(sdx)
            if (sect==0) cycle ! not accommodated
            if (sect==to_skip) cycle ! skip specified section (if, for example, the section is being edited)
            crse = Section(thisTerm,sect)%SubjectIdx

            if (isSubject_lecture_lab(crse)) then ! subject is lecture-lab
                ! add lecture
                j = index(Section(thisTerm,sect)%Code,DASH)
                tClassId = trim(Subject(crse)%Name)//SPACE//Section(thisTerm,sect)%Code(:j-1)
                lect = index_to_section(tClassId, thisTerm)
                do i=1,Section(thisTerm,lect)%NMeets
                    meetings(1) = i
                    list(len_list+1) = lect
                    list(len_list+2) = i
                    conflict_loc = -10 ! assume no conflicting section
                    call timetable_add_meetings_of_section(thisTerm, lect, 1, meetings, TimeTable, conflict_loc) ! add meeting to weekly schedule
                    if (conflict_loc/=-10) then
                        conflicted = .true.
                    else
                        conflict_loc = 0
                    end if
                    list(len_list+3) = conflict_loc ! index to conflicting section, if any
                    len_list = len_list+3
                end do
            end if
            ! add section
            do i=1,Section(thisTerm,sect)%NMeets
                meetings(1) = i
                list(len_list+1) = sect
                list(len_list+2) = i
                conflict_loc = -10 ! assume no conflicting section
                call timetable_add_meetings_of_section(thisTerm, sect, 1, meetings, TimeTable, conflict_loc) ! add meeting to weekly schedule
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

    end subroutine timetable_meetings_of_student


    subroutine timetable_meetings_of_teacher(thisTerm, teacher_index, to_skip, len_list, list, TimeTable, conflicted)
        integer, intent (in) :: thisTerm
        integer, intent(in) :: teacher_index, to_skip
        integer, intent (out) :: len_list, list(:)
        integer, dimension(60,7), intent (out) :: TimeTable
        logical, intent(out) :: conflicted
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer :: idx, conflict_loc, sdx

        len_list = 0 ! initialize list
        call timetable_clear(TimeTable) ! initialize weekly schedule
        conflicted = .false.
        do sdx=1,NumSections(thisTerm) ! loop over all sections
            if (sdx==to_skip) cycle ! skip specified section (if, for example, the section is being edited)
            if (Section(thisTerm,sdx)%SubjectIdx==0) cycle ! section ws deleted
            call meetings_of_section_by_teacher(thisTerm, sdx, teacher_index, n_meetings, meetings) ! collect meetings assigned to teacher
            if (n_meetings==0) cycle ! teacher not assigned to this section
            do idx=1,n_meetings ! loop over meetings for this section
                list(len_list+1) = sdx ! add index to Section(thisTerm,) to list of meetings for teacher
                list(len_list+2) = meetings(idx) ! index to Section(thisTerm,sdx)%DayIdx(), etc
                conflict_loc = -10 ! assume no conflicting section
                call timetable_add_meetings_of_section(thisTerm, sdx, 1, meetings(idx), TimeTable, conflict_loc) ! add meeting to weekly schedule
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


    subroutine timetable_meetings_in_room(thisTerm, room_index, to_skip, len_list, list, TimeTable, conflicted)
        integer, intent (in) :: thisTerm
        integer, intent(in) :: room_index, to_skip
        integer, intent (out) :: len_list, list(:)
        integer, dimension(60,7), intent (out) :: TimeTable
        logical, intent(out) :: conflicted
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer :: idx, conflict_loc, sdx
        len_list = 0 ! initialize list
        call timetable_clear(TimeTable) ! initialize weekly schedule
        conflicted = .false.
        do sdx=1,NumSections(thisTerm) ! loop over all sections
            if (sdx==to_skip) cycle ! skip specified section (if, for example, the section is being edited)
            if (Section(thisTerm,sdx)%SubjectIdx==0) cycle ! section ws deleted
            call meetings_of_section_in_room(thisTerm, sdx, room_index, n_meetings, meetings) ! collect meetings assigned to room
            if (n_meetings==0) cycle ! room not assigned to this section
            do idx=1,n_meetings ! loop over meetings for this section
                list(len_list+1) = sdx ! add index to Section(thisTerm,) to list of meetings in room
                list(len_list+2) = meetings(idx) ! index to Section(thisTerm,sdx)%DayIdx(), etc
                conflict_loc = -10 ! assume no conflicting section
                call timetable_add_meetings_of_section(thisTerm, sdx, 1, meetings(idx), TimeTable, conflict_loc) ! add meeting to weekly schedule
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


!===========================================================
! routines for predictions/pre-enlistment
!===========================================================


    subroutine initialize_pre_enlistment(eList)
        type (TYPE_PRE_ENLISTMENT) :: eList
        eList = TYPE_PRE_ENLISTMENT ( &
            0, YEAR_NOT_SET, 0, & ! levelClassification, levelYear, levelPriority, &
            0, 0, 0, 0, 0, 0, & ! lenSubject, NPriority, NAlternates, NCurrent, NRemaining, MissingPOCW, &
            ADVISING_NEEDED, ENLISTMENT_NEEDED, SCHOLASTIC_STATUS_NOT_SET, 0, 0, & ! statusAdvising, statusEnlistment, statusScholastic, codeConditional, codeNSTP
            0.0, 0.0, 0.0, & ! UnitsEarned, AllowedLoad, UnderLoad
            0, 0, 0, & ! dimension(MAX_SUBJECTS_PER_TERM) :: Subject, Section, Grade
            0.0) ! dimension(MAX_SUBJECTS_PER_TERM) :: Contrib

    end subroutine initialize_pre_enlistment


    subroutine recalculate_available_seats(thisTerm)
        integer, intent(in) :: thisTerm
        integer :: i, j, std
        ! calculate priority demand, priority accomodated/not accommodated
        Section(thisTerm,:)%RemSlots = Section(thisTerm,:)%Slots
        NumEnlistment(thisTerm) = 0
        do std = 1,NumStudents+NumAdditionalStudents
            do i=1,Student(std)%Enlistment(thisTerm)%NPriority+Student(std)%Enlistment(thisTerm)%NAlternates + &
                Student(std)%Enlistment(thisTerm)%NCurrent
                j = Student(std)%Enlistment(thisTerm)%Section(i)
                if (j > 0) then ! accommodated or force enlisted
                    Section(thisTerm,j)%RemSlots = Section(thisTerm,j)%RemSlots - 1
                    NumEnlistment(thisTerm) = NumEnlistment(thisTerm) + 1
                end if
            end do
        end do

    end subroutine recalculate_available_seats


    subroutine count_preenlistment(std, term, nEnlisted, nAdvised)
        integer, intent (in) :: std, term
        integer, intent (out) :: nAdvised(3), nEnlisted(3)

        integer :: fdx, iStart, iEnd, iTerm

        nAdvised = 0 ! how many advised subjects
        nEnlisted = 0 ! how many enlisted subjects
        if (term/=0) then
            iStart = term
            iEnd = term
        else
            iStart = 1
            iEnd = 3
        end if
        do iTerm=iStart,iEnd
            do fdx=1,Student(std)%Enlistment(iTerm)%lenSubject
                if (Student(std)%Enlistment(iTerm)%Contrib(fdx)>0.5) nAdvised(iTerm) = nAdvised(iTerm) + 1
                if (Student(std)%Enlistment(iTerm)%Section(fdx)>0) nEnlisted(iTerm) = nEnlisted(iTerm) + 1
            end do
        end do

    end subroutine count_preenlistment

#if defined UPLB

    subroutine check_for_regular_advised_subjects(curr, thisTerm, Advice)
        type (TYPE_PRE_ENLISTMENT), intent (inout) :: Advice
        integer, intent(in) :: curr, thisTerm
        integer :: idx, jdx, kdx, tdx, i, crse, StdTerm

        StdTerm = (Advice%levelYear-1)*3 + thisTerm ! advised term based on units earned by student
        kdx = 0 ! how many subjects are programmed for StdTerm ?
        tdx = 0 ! how many priority subjects are programmed ?
        jdx = 0 ! how many subjects remaining after StdTerm (future subjects)
        do idx=1,Curriculum(curr)%NSubjects
            if (Curriculum(curr)%SubjectTerm(idx)/=StdTerm) then
                if (Curriculum(curr)%SubjectTerm(idx)>StdTerm) jdx = jdx + 1 ! future subject
                cycle ! not taken during advised term
            end if
            crse = Curriculum(curr)%SubjectIdx(idx) ! index to Subject()
            if (crse<0) cycle ! ignore special
            if (Subject(crse)%Name(1:3)=='GE(') cycle ! ignore GE
            if (Subject(crse)%Name(1:3)=='PE ') cycle ! ignore PE
            if (Subject(crse)%Name(1:5)=='NSTP ') cycle ! ignore NSTP
            kdx = kdx + 1 ! taken during StdTerm
            do i=1,Advice%NPriority
                if ( crse==Advice%Subject(i) ) then
                    tdx = tdx + 1 ! a priority subject
                    !call html_comment('Priority '//itoa(tdx)//' - '//Subject(crse)%Name)
                    exit
                end if
            end do
        end do
        ! any programmed subjects as well as future subjects and there are priority unprogrammed subjects
        if (kdx>0 .and. jdx>0 .and. kdx==tdx) then
            Advice%statusAdvising = ADVISING_REGULAR
        end if

    end subroutine check_for_regular_advised_subjects

#else

    subroutine check_for_regular_advised_subjects(curr, thisTerm, Advice)
        type (TYPE_PRE_ENLISTMENT), intent (inout) :: Advice
        integer, intent(in) :: curr, thisTerm
        integer :: idx, jdx, kdx, tdx, i, crse, StdTerm

        StdTerm = (Advice%levelYear-1)*3 + thisTerm ! advised term based on units earned by student
        kdx = 0 ! how many subjects are programmed for StdTerm ?
        tdx = 0 ! how many priority subjects are programmed ?
        jdx = 0 ! how many subjects remaining after StdTerm (future subjects)
        do idx=1,Curriculum(curr)%NSubjects
            if (Curriculum(curr)%SubjectTerm(idx)/=StdTerm) then
                if (Curriculum(curr)%SubjectTerm(idx)>StdTerm) jdx = jdx + 1 ! future subject
                cycle ! not taken during advised term
            end if
            crse = Curriculum(curr)%SubjectIdx(idx) ! index to Subject()
            if (Subject(crse)%Name(1:3)=='PE ') cycle ! ignore PE
            if (Subject(crse)%Name(1:5)=='NSTP ') cycle ! ignore NSTP
            kdx = kdx + 1 ! taken during StdTerm
            do i=1,Advice%NPriority
                if ( crse==Advice%Subject(i) ) then
                    tdx = tdx + 1 ! a priority subject
                    !call html_comment('Priority '//itoa(tdx)//' - '//Subject(crse)%Name)
                    exit
                end if
            end do
        end do
        ! any programmed subjects as well as future subjects and there are priority unprogrammed subjects
        if (kdx>0 .and. jdx>0 .and. kdx==tdx) then
            Advice%statusAdvising = ADVISING_REGULAR
        end if

    end subroutine check_for_regular_advised_subjects

#endif

    subroutine get_picture_file(std, path)
        integer, intent (in) :: std
        character(len=*), intent(out) :: path
        character(len=3), dimension(6) :: imageExt = (/ 'jpg', 'JPG', 'png', 'PNG', 'bmp', 'BMP' /)

        integer :: k, l
        logical :: photoExists
        character(len=MAX_LEN_FILE_PATH) :: line

        path = SPACE
        l = year_prefix(Student(std))
        do k=1,6
            line = trim(basefile_student(std))//DOT//imageExt(k)
            inquire(file=trim(dirPICTURES)//line, exist=photoExists)
            if (photoExists) then
                path = trim(Student(std)%StdNo(1:l))//FSLASH//trim(Student(std)%StdNo)//DOT//imageExt(k)
                exit
            end if
        end do

    end subroutine get_picture_file


end module UNIVERSITY
