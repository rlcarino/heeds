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


module USERFUNCTIONS

    use BLOCKS
    use TIMETABLES
    use CHECKLISTS
    use CGI

    implicit none

    ! index to server functions
    integer, parameter ::  &
    fnLogin                   =  1, & ! login user
    fnStopUser                =  2, & ! logout user
    fnStopUserByAdmin         =  3, & ! forced logout of user by Admin
    fnSearch                  =  4, & ! search for an object
    fnCollegeSelect           =  5, & ! select target college
    fnCollegeLinks            =  6, & ! index to college info
    fnSubjectList             =  7, & ! view list of subjects administered by a department
    fnEditSubject             =  8, & ! edit subject
    fnCurriculumList          =  9, & ! view list of curricular programs administered by a college
    fnCurriculum              = 10, & ! view a curricular program
    fnEditCurriculum          = 11, & ! edit curriculum
    fnActivateCurriculum      = 12, & ! activate curriculum
    fnDeactivateCurriculum    = 13, & ! deactivate curriculum
    fnEditRoom                = 14, & ! edit room parameters
    fnEditTeacher             = 15, & ! edit teacher record
    !
    fnStudentsByProgram       = 16, & ! view list students in a program
    fnStudentsByCurriculum    = 17, & ! view list students in a curriculum
    fnStudentsByName          = 18, & ! view alphabetical list of students in a college
    fnStudentsByYear          = 19, & ! view list of students in college by year
    fnStudentsDistribution    = 20, & ! view distribution of students, by curriculum and by college
    !
    fnStudentAdd              = 21, & ! add a student
    fnStudentAddPrompt        = 22, & ! entry form for 'add a student'
    fnStudentPerformance      = 23, & ! view student performance
    fnEditCheckList           = 24, & ! display checklist for editing
    fnChangeMatriculation     = 25, & ! change matriculation
    fnFindBlock               = 26, & ! find a block for student
    ! = 26
    !
    fnScheduleOfClasses       = 31, & ! display schedule of classes for editing
    fnScheduleOfferSubject     = 32, & ! offer a subject
    fnScheduleAddLab          = 33, & ! add a lab section
    fnScheduleDelete          = 34, & ! delete a section
    fnScheduleEdit            = 35, & ! edit a section
    fnScheduleValidate        = 36, & ! check correctness of edit inputs
    fnTeachersByDept          = 37, & ! view list teachers belonging to a college
    fnTeachersByName          = 38, & ! view alphabetical list of teachers in a college
    fnTeacherSchedule         = 39, & ! view weekly schedule of a teacher
    fnRoomList                = 40, & ! view list rooms administered by a college
    fnRoomSchedule            = 41, & ! view weekly schedule of a room
    fnRoomConflicts           = 42, & ! view list of rooms with conflicts
    fnTeacherConflicts        = 43, & ! view list of teachers with conflicts
    fnTBARooms                = 44, & ! view list of sections with TBA rooms
    fnTBATeachers             = 45, & ! view list of sections with TBA teachers
    fnBlockSchedule           = 46, & ! display schedule of block section
    fnBlockEditName           = 47, & ! edit name of block section
    fnBlockDeleteName         = 48, & ! delete block, but keep its sections
    fnBlockDeleteAll          = 49, & ! delete block and its sections
    fnBlockNewSelect          = 50, & ! select parameters for a new block
    fnBlockNewAdd             = 51, & ! add a new block
    fnBlockCopy               = 52, & ! copy block
    fnBlockEditSection        = 53, & ! edit section in block
    fnBlockEditSubject        = 54, & ! update subjects in block
    fnScheduleByArea          = 55, & ! display schedule of classes for editing, by area
    fnPrintableWorkload       = 56, & ! printable teaching load
    ! = 56
    !
    fnEnlistmentSummary       = 61, & ! students not accommodated in a priority subject
    fnNotAccommodated         = 62, & ! students not accommodated in a priority subject
    fnBottleneck              = 63, & ! "bottleneck" subjects
    fnExtraSlots              = 64, & ! subjects with excess slots
    fnUnderloadSummary        = 65, & ! summary of underloading
    fnUnderloadedStudents     = 66, & ! underloaded students
    fnClassList               = 67, & ! view list of students in a class
    fnGradeSheet              = 68, & ! enter grades
    fnRebuildChecklists       = 69, & ! rebuild checklists from grades each semester
    ! = 60
    !
    fnNextScheduleOfClasses   = 81, & ! display next semester's schedule of classes for editing
    fnNextScheduleOfferSubject = 82, & ! offer a subject
    fnNextScheduleAddLab      = 83, & ! add a lab section
    fnNextScheduleDelete      = 84, & ! delete a section
    fnNextScheduleEdit        = 85, & ! edit a section
    fnNextScheduleValidate    = 86, & ! check correctness of edit inputs
    fnNextTeachersByDept      = 87, & ! view list teachers belonging to a college
    fnNextTeachersByName      = 88, & ! view alphabetical list of teachers in a college
    fnNextTeacherSchedule     = 89, & ! view weekly schedule of a teacher
    fnNextRoomList            = 90, & ! view list rooms administered by a college
    fnNextRoomSchedule        = 91, & ! view weekly schedule of a room
    fnNextRoomConflicts       = 92, & ! view list of rooms with conflicts
    fnNextTeacherConflicts    = 93, & ! view list of teachers with conflicts
    fnNextTBARooms            = 94, & ! view list of sections with TBA rooms
    fnNextTBATeachers         = 95, & ! view list of sections with TBA teachers
    fnNextBlockSchedule       = 96, & ! display schedule of block section
    fnNextBlockEditName       = 97, & ! edit name of block section
    fnNextBlockDeleteName     = 98, & ! delete block, but keep sections
    fnNextBlockDeleteAll      = 99, & ! delete block and its sections
    fnNextBlockNewSelect      =100, & ! select parameters for a new block
    fnNextBlockNewAdd         =101, & ! add a new block
    fnNextBlockCopy           =102, & ! copy block
    fnNextBlockEditSection    =103, & ! edit section in block
    fnNextBlockEditSubject    =104, & ! update subjects in block
    fnNextScheduleByArea      =105, & ! display schedule of classes for editing, by area
    fnNextPrintableWorkload   =106, & ! printable teaching load
    ! = 106
    !
    fnDemandFreshmen          = 111, & ! view demand for subjects by incoming students
    fnUpdateDemandFreshmen    = 112, & ! update no. of incoming students
    fnPrintableSchedule       = 113, & ! printable weekly timetable
    fnAdviseStudent           = 114, & ! generate Prediction()
    fnDemandForSubjects       = 115, & ! view demand for subjects
    fnPotentialStudents       = 116, & ! list of potential students of a subject
    !
    fnStopProgram              = 120    ! should be the last (used as array extent)

    integer, parameter :: fnNextOffset = fnNextScheduleOfClasses - fnScheduleOfClasses

    logical, dimension(0:fnStopProgram) :: available ! modified in set_feature_availability()
    ! assume all functions are available throughout the term


    ! users
    integer, parameter :: &
    MAX_LEN_USER_CODE = MAX_LEN_CURRICULUM_CODE, &
    MAX_LEN_USER_NAME = MAX_LEN_CURRICULUM_NAME, &
    MAX_ALL_USERFUNCTIONS=4*MAX_ALL_CURRICULA+MAX_ALL_COLLEGES
    type :: typeUSER
        character (len=MAX_LEN_USER_CODE) :: Code
        character (len=MAX_LEN_USER_NAME) :: Name
        character (len=MAX_QUERY_STRING_LEN) :: lastQUERY
        integer :: Role, Session, CollegeIdx, DeptIdx, CurriculumIdx, Year
    end type typeUSER
    type (typeUSER), dimension(0:MAX_ALL_USERFUNCTIONS) :: UserList
    integer :: NumUsers

    ! information about a  user who made a request
    integer :: DeptIdxUser, CollegeIdxUser, CurriculumIdxUser
    character (len=MAX_LEN_USER_CODE) :: USER

    character(len=20) :: REMOTE_ADDR ! The IP address of the user
    character(len=MAX_QUERY_STRING_LEN) :: QUERY_STRING ! The QUERY

    ! the requested server function
    integer :: REQUEST

    ! the target of the fucntion
    integer :: targetCollege, targetDepartment, targetSubject, targetRoom, targetTeacher, &
    targetCurriculum, targetStudent, targetBlock, targetSection, targetUser

    ! work arrays
    integer :: tArray(max(MAX_ALL_STUDENTS,2*MAX_ALL_SUBJECTS))
    character (len=80) :: QUERY_put, TermQualifier


contains


    subroutine set_feature_availability ()
        !
        !  Modify availability of functions based on data that is loaded
        !

        !  Relevant dates for REGIST during (currentYear, currentTerm)
        !  Note: currentYear is the start year of the current Academic Year,
        !        currentTerm is First Semester or Second Semester
        !
        !    Date 1: First day of registration
        !    Date 2: Last day for late registration
        !    Date 3: Last day for submission of grades
        !
        !  Define:
        !    Period I=[Date 1, Date 2] (enlistment Period)
        !       - change matriculation
        !    Period II=(Date 2, Date 3] (mid-term)
        !       - REGD subjects are in TCG
        !       - probabilistic forecasts
        !    Period III=(Date 3,Date 1 of next term) (term break)
        !       - latest grades are available, in TCG
        !       - deterministic forecasts
        !
        !  REGIST Function availability:
        !

        available = .true. ! all functions are initially available; reset below
        available(0) = .false.

        if (NumStudents==0) then
            available(fnStudentsByProgram) = .false.
            available(fnStudentsByCurriculum) = .false.
            available(fnStudentsByName) = .false.
            available(fnStudentsByYear) = .false.
            available(fnStudentsDistribution) = .false.
            available(fnEditCheckList) = .false.
            available(fnStudentPerformance) = .false.
        endif
        if (NumStudents==0 .or. NumEnlistmentRecords==0) then
            available(fnChangeMatriculation) = .false.
            available(fnFindBlock) = .false.
            available(fnBottleneck) = .false.
            available(fnExtraSlots) = .false.
            available(fnUnderLoadSummary) = .false.
            available(fnUnderLoadedStudents) = .false.
            available(fnClassList) = .false.
            available(fnGradeSheet) = .false.
            available(fnEnlistmentSummary) = .false.
        endif
        if (NumStudents==0 .or. NumPredictionRecords==0) then
            available(fnDemandForSubjects) = .false.
            available(fnPotentialStudents) = .false.
        endif

        if (NumStudents==0 .or. NumPredictionRecords>0) then
            available(fnAdviseStudent) = .false.
        endif

        if (NumTeachers<=1) then
            available(fnTeachersByDept) = .false.
            available(fnTeachersByName) = .false.
            available(fnTeacherSchedule) = .false.
        endif

        if (NumRooms<=1) then
            available(fnRoomList) = .false.
            available(fnRoomSchedule) = .false.
        endif

        if (NumCurrentSections==0) then
            available(fnScheduleOfClasses) = .false.
            available(fnTeacherSchedule) = .false.
            available(fnBlockSchedule) = .false.
            available(fnRoomSchedule) = .false.
            available(fnRoomConflicts) = .false.
            available(fnTeacherConflicts) = .false.
            available(fnTBARooms) = .false.
            available(fnTBATeachers) = .false.
            available(fnFindBlock) = .false.
        endif

        if (NumCurricula==0) then
            available(fnCurriculumList) = .false.
            available(fnCurriculum) = .false.
        endif

        if (Period==1) available(fnAdviseStudent) = .false.

        if (Period==1 .or. NumNextSections==0) then
            available(fnNextScheduleOfClasses) = .false.
            available(fnNextScheduleOfferSubject) = .false.
            available(fnNextScheduleAddLab) = .false.
            available(fnNextScheduleDelete) = .false.
            available(fnNextScheduleEdit) = .false.
            available(fnNextScheduleValidate) = .false.
            available(fnNextTeachersByDept) = .false.
            available(fnNextTeachersByName) = .false.
            available(fnNextTeacherSchedule) = .false.
            available(fnNextRoomList) = .false.
            available(fnNextRoomSchedule) = .false.
            available(fnNextBlockSchedule) = .false.
            available(fnNextRoomConflicts) = .false.
            available(fnNextTeacherConflicts) = .false.
            available(fnNextTBARooms) = .false.
            available(fnNextTBATeachers) = .false.
        end if

        if (Period/=1) then
            available(fnChangeMatriculation) = .false.
            available(fnFindBlock) = .false.
        end if

        ! deactivate "Advise all students"
        available(fnAdviseStudent) = .false.
        ! deactivate "Rebuild all individual COGs"
        available(fnRebuildChecklists) = .false.

        ! any freshman INTAKE data?
        available(fnDemandFreshmen) = sum(NFintake)>0

        ! university-specific customizations
#if defined CUSTOM
        available(fnStudentsByYear) = .false.
#endif

        return
    end subroutine set_feature_availability


    subroutine create_user_names()
        ! Make list of users
        integer :: idx, rank, year!, jdx

        UserList = typeUSER (SPACE,SPACE,SPACE,0,-1,0,0,0,0)
        NumUsers = 0
        done = .false.
        do targetDepartment=2,NumDepartments
            if (.not. Department(targetDepartment)%hasInfo) cycle
            NumUsers = NumUsers + 1
            UserList(NumUsers)%Code = Department(targetDepartment)%Code
            if (REGISTRAR/=Department(targetDepartment)%Code) then
                UserList(NumUsers)%Name = trim(Department(targetDepartment)%Code)//' Teaching Load Scheduler'
            else
                UserList(NumUsers)%Name = trim(Department(targetDepartment)%Code)//'@'//UniversityCode
            end if
            UserList(NumUsers)%Role = 1
            UserList(NumUsers)%DeptIdx = targetDepartment
            UserList(NumUsers)%CollegeIdx = Department(targetDepartment)%CollegeIdx
            UserList(NumUsers)%CurriculumIdx = 0
        end do
        ! sort
        do targetDepartment=1,NumUsers-1
            do targetCollege=targetDepartment+1,NumUsers
                if (UserList(targetCollege)%Code<UserList(targetDepartment)%Code) then
                    UserList(NUmUsers+1) = UserList(targetDepartment)
                    UserList(targetDepartment) = UserList(targetCollege)
                    UserList(targetCollege) = UserList(NUmUsers+1)
                end if
            end do
        end do
        idx = NumUsers ! remember departments
        do targetCollege=1,NumColleges-1
            do targetCurriculum=1,NumCurricula
                if (.not. Curriculum(targetCurriculum)%Active) cycle
                if (Curriculum(targetCurriculum)%CollegeIdx /= targetCollege) cycle
                if (done(targetCurriculum)) cycle
                do rank=1,10
                    NumUsers = NumUsers + 1
                    UserList(NumUsers)%Code = trim(CurrProgCode(targetCurriculum))//SPACE//itoa3bz(rank)
                    UserList(NumUsers)%Name = trim(CurrProgCode(targetCurriculum))//' Registration Adviser '//itoa3bz(rank)
                    UserList(NumUsers)%Role = 2
                    UserList(NumUsers)%CollegeIdx = targetCollege
                    UserList(NumUsers)%CurriculumIdx = targetCurriculum
                    UserList(NumUsers)%Year = rank
                end do
                do year=targetCurriculum+1,NumCurricula
                    if (CurrProgCode(year) == CurrProgCode(targetCurriculum)) done(year) = .true.
                end do
            end do
        end do
        do targetDepartment=idx+1,NumUsers-1
            do targetCollege=targetDepartment+1,NumUsers
                if (UserList(targetCollege)%Code<UserList(targetDepartment)%Code) then
                    UserList(NUmUsers+1) = UserList(targetDepartment)
                    UserList(targetDepartment) = UserList(targetCollege)
                    UserList(targetCollege) = UserList(NUmUsers+1)
                end if
            end do
        end do
        return
    end subroutine create_user_names


    function index_to_user(token)
        integer :: index_to_user
        character (len=MAX_LEN_USER_CODE), intent(in) :: token
        integer :: i
        index_to_user = 0
        do i=1,NumUsers
            if (token==UserList(i)%Code) then
                index_to_user = i
                exit
            end if
        end do
        return
    end function index_to_user


end module USERFUNCTIONS
