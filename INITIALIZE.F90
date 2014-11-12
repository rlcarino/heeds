!======================================================================
!
!    HEEDS (Higher Education Enrollment Decision Support) - A program
!      to create enrollment scenarios for 'next term' in a university
!    Copyright (C) 2012-2014 Ricolindo L. Carino
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

module INITIALIZE

    use UNIVERSITY

    implicit none


contains

!===========================================================
! initializations
!===========================================================

    subroutine initializations()

        integer :: iTmp, numArgs
        character (len=MAX_LEN_FILE_PATH) :: dataSource
        logical :: pathExists

        ! program starts         YYYYMMDD         HHMMSS.LLL
        call date_and_time (date=currentDate,time=currentTime)
        startDateTime = currentDate//DASH//currentTime(:6)
        previousDate = currentDate
        currentYear = atoi(currentDate(1:4))

        ! initialize random seed
        call initialize_random_seed()

        ! the executable
        call getarg(0, fileEXE)
        iTmp = len_trim(fileEXE)
        do while (iTmp>0)
            if (fileEXE(iTmp:iTmp)/=DIRSEP) then
                iTmp = iTmp-1
            else
                exit
            end if
        end do
        fileEXE = fileEXE(iTmp+1:)

        ! the version
        iTmp = index(fileEXE, '-')
        VERSION = SPACE//fileEXE(iTmp+1:)

        ! arguments are: UNIV YEAR TERM ACTION PATH
        numArgs = iargc()
        if (numArgs<1) call usage('Error: command arguments are UNIV YEAR TERM ACTION PATH')

        ! WEBROOT - 'root' for nginx
        inquire(file=WEBROOT, exist=pathExists)
        if (.not. pathExists) call usage('Error: directory '//WEBROOT//' does not exist')

        ! data directory
        if (numArgs>4) then
            call getarg(5, dirUNIV)
        else
            ! user's HOME directory
#if defined GLNX
            call get_environment_variable("HOME", dirUNIV)
#else
            dirUNIV = 'C:'
#endif
            dirUNIV = trim(dirUNIV)//DIRSEP//PROGNAME
        end if

        ! University code
        call getarg(1, UniversityCode)

        ! dirUNIV - location of files for target University
        dirUNIV = trim(dirUNIV)//DIRSEP//trim(UniversityCode)//DIRSEP
        inquire(file=trim(dirUNIV), exist=pathExists)
        if (.not. pathExists) call usage('Error: directory '//trim(dirUNIV)//' does not exist')

        ! log directory
        dirLOG = trim(dirUNIV)//'logs'//DIRSEP
        call make_directory( dirLOG )
        call log_comment( '-------', 'Begins '//currentDate//DASH//currentTime, '-------', &
            'Executable is : '//trim(fileEXE)//SPACE//DASH//SPACE//'( '//PROGNAME//VERSION//' )' )

        ! data directory
        dirDATA = trim(dirUNIV)//'data'//DIRSEP
        lenDirDATA = len_trim(dirDATA)
        call make_directory( dirDATA )

        ! create year/term directories in dirDATA
        call make_year_term_directory( dirDATA, currentYear )

        ! instance is running as mirror?
        iTmp = index(UniversityCode, '-mirror')
        if (iTmp > 0) then
            isReadOnly = .true.
            UniversityCodeNoMirror = UniversityCode(:iTmp-1)
        else
            isReadOnly = .false.
            iTmp = index(UniversityCode, '-rectify')
            if (iTmp > 0) then
                isRectify = .true.
                UniversityCodeNoMirror = UniversityCode(:iTmp-1)
            else
                UniversityCodeNoMirror = UniversityCode
            end if
        end if
        isAllowedNonEditors = .true.

        ! ACTION
        if (numArgs>3) then
            call getarg(4, ACTION)
        else
            ACTION = 'Select'
        end if

        ! YEAR when the current School Year started
        if (numArgs>1) then
            call getarg(2, dataSource)
            currentYear = atoi(dataSource)
        else ! get YYYYMM from currentDate
            currentYear = atoi(currentDate(1:4))
            iTmp = atoi(currentDate(5:6)) ! month
            ! assume 1st sem=June-Oct, 2nd sem=Nov-March, summer=Apr-May
            if (iTmp<=5) then ! it is before 1st sem; School Year started in previous year
                currentYear = currentYear - 1
            end if
        end if

        ! current TERM
        if (numArgs>2) then
            call getarg(3, dataSource)
        else ! get YYYYMM from currentDate
            iTmp = atoi(currentDate(5:6)) ! month
            ! assume 1st sem=June-Oct, 2nd sem=Nov-March, summer=Apr-May
            select case (iTmp)
                case (1:3)
                    dataSource = '2'
                case (4:5)
                    dataSource = 'S'
                case (6:10)
                    dataSource = '1'
                case (11:12)
                    dataSource = '2'
            end select
        end if

        select case (trim(dataSource))
            case ('1')
                currentTerm = 1
            case ('2')
                currentTerm = 2
            case ('S', 's', '3')
                currentTerm = 3
            case default
                currentTerm = 0
        end select

        ! valid currentYear and currentTerm?
        if (currentYear*currentTerm<=0) then
            call usage('Error: Invalid YEAR and/or TERM')
        end if

        ! date-related initializations
        call initialize_date_change()

        ! compute relative years and terms (before and after current)
        call initialize_past_future_years_terms ()

        ! reset basic data
        call initialize_basic_data ()

    end subroutine initializations


    subroutine initialize_basic_data ()

        integer :: iTmp
        character (len=MAX_LEN_FILE_PATH) :: dataSource
        logical :: pathExists

        ! the colleges
        NumColleges = 0
        call initialize_college (College(0))
        College(1:) = College(0)

        ! the departments
        call initialize_department (Department(0))
        Department(2:) = Department(0)

        Department(0)%Code = 'ERROR'
        Department(0)%Name = '(ERROR)'
        Department(1)%Code = '(dummy)'
        Department(1)%Name = '(dummy)'
        NumDepartments = 1
        ScheduleCount = 0

        ! the subjects
        NumDummySubjects = -1
        NumSubjects = 0
        NumAdditionalSubjects = 0
        NumRenames = 0
        Renamed = 0

        call initialize_subject (Subject(0))
        Subject = Subject(0)
        Subject(-1)%Name = '(dummy)'
        Subject(-1)%Title = '(dummy)'
        INDEX_TO_NONE = -1 ! fix after reading in all subject codes

        ! string representation of percentage grade, float value, reference
        do iTmp=1,100
            txtGrade(ZERO_PERCENT_GRADE+iTmp) = itoa(iTmp)
            fGrade(ZERO_PERCENT_GRADE+iTmp) = 1.0*iTmp
            pGrade(ZERO_PERCENT_GRADE+iTmp) = ZERO_PERCENT_GRADE+iTmp
        end do

        ! the curricular programs
        NumCurricula = 0
        Curriculum = TYPE_CURRICULUM (.true., SPACE, SPACE, SPACE, SPACE, 0, 0, 0, 0, 0)
        NumSubst = 0
        SubstIdx = 0
        Substitution = 0

        ! the rooms
        NumRooms = 0
        NumAdditionalRooms = 0
        call initialize_room (Room(0))
        Room = Room(0)
        Room(0)%Code = 'TBA' ! for 'Room not found!'

        ! the teachers
        call initialize_teacher(Teacher(0))
        Teacher = Teacher(0)
        Teacher(0)%TeacherId = 'TBA'
        Teacher(0)%Name = '(Teacher to be assigned)'

        NumTeachers = 1
        NumAdditionalTeachers = 0
        ! the Guest account
        Teacher(NumTeachers)%TeacherID = GUEST
        Teacher(NumTeachers)%Name = 'Guest Account'
        Teacher(NumTeachers)%DeptIdx = NumDepartments
        Teacher(NumTeachers)%Role = GUEST
        call set_password(Teacher(NumTeachers)%Password, GUEST)

        ! student data
        call initialize_student(Student(0))
        Student = Student(0)
        NumStudents = 0
        NumAdditionalStudents = 0
        do iTmp=1,MAX_ALL_STUDENTS
            StdRank(iTmp) = iTmp
        end do

        ! term-specific data
        call initialize_term_data()
        isPeriodTwo = .false.
        isPeriodOne = .false.
        isPeriodThree = .false.
        isPeriodFour = .false.
        isProbabilistic = .true.
        termBegin = currentTerm
        termEnd = termBegin-1

        ! message of the day
        MOTD = SPACE
        dataSource = trim(pathToYear)//'MOTD.TXT'
        inquire(file=dataSource, exist=pathExists)

        if (pathExists) then
            open (unit=unitETC, file=dataSource, status='old')
            do
                read(unitETC, AFORMAT, iostat=iTmp) QUERY_STRING
                if (iTmp<0) exit
                if (len_trim(QUERY_STRING)>0) MOTD = trim(MOTD)//SPACE//QUERY_STRING
            end do
            close(unitETC)
        end if

        ! emergency message 
        EMERGENCY = SPACE

        ! disable Guest?
        if (.not. isEnabledGuest) then
            USERNAME = GUEST
            iTmp = index_to_teacher(USERNAME)
            call initialize_teacher(Teacher(iTmp))
            Teacher(iTmp)%DeptIdx = 0
        end if

    end subroutine initialize_basic_data


    subroutine initialize_date_change()

        ! backup directory
        dirBACKUP = trim(dirUNIV)//currentDate//DASH//trim(fileEXE)//'-backup'//DIRSEP

        ! create year/term directories in dirBACKUP
        call make_year_term_directory( dirBACKUP, currentYear )

        ! initialize log for requests
        fileREQ = trim(dirLOG)//currentDate//DASH//trim(fileEXE)//'-request.log'
        call log_request(unitREQ, trim(fileREQ), &
            '#-------', '#Begins '//currentDate//DASH//currentTime, '#-------', &
            '#Executable is : '//trim(fileEXE)//SPACE//DASH//SPACE//'( '//PROGNAME//VERSION//')', &
            '#Arguments are : '//trim(UniversityCode)//SPACE//itoa(currentYear)//trim(txtSemester(currentTerm)) )

        ! open a 'scratch' HTML response file
        open(unit=unitHTML, file=trim(dirLOG)//currentDate//DASH//trim(fileEXE)//'-scratch.html', &
            form='formatted', status='unknown')

    end subroutine initialize_date_change


    subroutine initialize_past_future_years_terms ()

        select case (currentTerm)

            case (1)
                cTm3Year = currentYear-1 ! one school year ago (year)
                cTm3 = 1                 ! 3 terms ago (1st)
                cTm2Year = currentYear-1
                cTm2 = 2                 ! 2 terms ago (2nd)
                cTm1Year = currentYear-1 ! year of previous term
                cTm1 = 3                 ! 1 term ago (summer)
                nextYear = currentYear   ! year of next term (the same year)
                nextTerm = 2             ! next term (2nd)

            case (2)
                cTm3Year = currentYear-1
                cTm3 = 2
                cTm2Year = currentYear-1
                cTm2 = 3
                cTm1Year = currentYear
                cTm1 = 1
                nextYear = currentYear
                nextTerm = 3

            case (3)
                cTm3Year = currentYear-1
                cTm3 = 3
                cTm2Year = currentYear
                cTm2 = 1
                cTm1Year = currentYear
                cTm1 = 2
                nextYear = currentYear+1
                nextTerm = 1

        end select

        ! directory for start of School Year
        pathToYear = trim(dirDATA)//trim(itoa(currentYear))//DIRSEP

        ! directory for year of next term
        pathToNextYear = trim(dirDATA)//trim(itoa(nextYear))//DIRSEP

    end subroutine initialize_past_future_years_terms


    subroutine initialize_term_data()

        ! classes
        NumSections = 0
        call initialize_section (Section(1,0))
        Section(:,:) = Section(1,0)

        ! blocks
        NumBlocks = 0
        call initialize_block(Block(1,0))
        Block(:,:) = Block(1,0)

        ! initialize freshman quotas
        NFintake = 0

        ! pre-enlisted, advised subjects of students
        NumEnlistment = 0
        call initialize_pre_enlistment(Enlistment(1,0))
        Enlistment(:,:) = Enlistment(1,0)

        ! waivers
        NumWaiverRecords = 0
        call initialize_waiver(WaiverCOI(0))
        WaiverCOI = WaiverCOI(0)

    end subroutine initialize_term_data


end module INITIALIZE
