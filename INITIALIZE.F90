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

        ! program starts
        call date_and_time (date=currentDate,time=currentTime)
        startDateTime = currentDate//DASH//currentTime(:6)

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

        ! arguments are: UNIV YEAR TERM
        numArgs = iargc() ! UNIV YEAR TERM
        if (numArgs<4) call usage('Error: Not enough command arguments')

        ! user's HOME directory
#if defined GLNX
        call get_environment_variable("HOME", dirUNIV)
#else
        dirUNIV = SPACE
#endif

        ! ACTION
        call getarg(4, ACTION)

        ! University code
        call getarg(1, UniversityCode)

        ! top-level HEEDS directories
        dirWEB  = trim(dirUNIV)//DIRSEP//'HEEDS'//DIRSEP//'web'//DIRSEP
        inquire(file=trim(dirWEB), exist=pathExists)
        if (.not. pathExists) call usage('Error: directory '//trim(dirWEB)//' does not exist')

        dirUNIV = trim(dirUNIV)//DIRSEP//'HEEDS'//DIRSEP//trim(UniversityCode)//DIRSEP
        inquire(file=trim(dirUNIV), exist=pathExists)
        if (.not. pathExists) call usage('Error: directory '//trim(dirUNIV)//' does not exist')

        ! current YEAR
        call getarg(2, dataSource)
        currentYear = atoi(dataSource)

        ! current TERM
        call getarg(3, dataSource)
        select case (trim(dataSource))

            case ('1')
                currentTerm = 1

            case ('2')
                currentTerm = 2

            case ('S', 's')
                currentTerm = 3

            case default
                currentTerm = 0

        end select

        ! valid currentYear and currentTerm?
        if (currentYear*currentTerm<=0) then
            call usage('Error: Invalid YEAR and/or TERM')
        end if

        ! compute relative years and terms (before and  after current)
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
        pathToYear = trim(itoa(currentYear))//DIRSEP
        ! directory for year of next term
        pathToNextYear = trim(itoa(nextYear))//DIRSEP

        ! absolute next year
        dataSource = trim(itoa(currentYear+1))//DIRSEP

        ! data directory
        dirDATA = trim(dirUNIV)//'data'//DIRSEP
        call make_directory( dirDATA )
        lenDirDAT = len_trim(dirDATA)

        call make_directory( trim(dirDATA)//pathToYear )
        call make_directory( trim(dirDATA)//dataSource )
        do iTmp=1,3
            call make_directory( trim(dirDATA)//trim(pathToYear)//trim(txtSemester(iTmp))//DIRSEP )
            call make_directory( trim(dirDATA)//trim(dataSource)//trim(txtSemester(iTmp))//DIRSEP )
        end do

        ! log directory
        dirLOG = trim(dirUNIV)//'logs'//DIRSEP
        call make_directory( dirLOG )
        call log_comment( '-------', 'Begins '//currentDate//DASH//currentTime, '-------', &
            'Executable is : '//trim(fileEXE)//SPACE//DASH//SPACE//'( '//PROGNAME//VERSION//' )' )

        ! backup directory
#if defined no_password_check
        dirBACKUP = dirLOG
#else
        dirBACKUP = trim(dirUNIV)//'backup-'//trim(startDateTime)//DASH//trim(fileEXE)//DIRSEP
#endif
        call make_directory( dirBACKUP )
        call make_directory( trim(dirBACKUP)//pathToYear )
        call make_directory( trim(dirBACKUP)//dataSource )
        do iTmp=1,3
            call make_directory( trim(dirBACKUP)//trim(pathToYear)//trim(txtSemester(iTmp))//DIRSEP )
            call make_directory( trim(dirBACKUP)//trim(dataSource)//trim(txtSemester(iTmp))//DIRSEP )
        end do

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

        ! classes
        NumSections = 0
        call initialize_section (Section(1,0))
        Section(:,:) = Section(1,0)

        ! blocks
        NumBlocks = 0
        call initialize_block(Block(1,0))
        Block(:,:) = Block(1,0)

        ! show schedules for which terms ?
        termBegin = currentTerm
        termEnd = termBegin

        ! action flags
        isActionAdvising = .false. ! REGD grade is PASSing ?
        isActionClasslists = .false.

    end subroutine initializations


end module INITIALIZE
