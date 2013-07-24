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

    use IO

    implicit none


contains

!===========================================================
! initializations
!===========================================================

    subroutine initializations()

        integer :: jTmp, iTmp, errNo, numArgs
        character (len=MAX_LEN_FILE_PATH) :: dataSource
        logical :: pathExists
        !integer :: idxGrp=0, MaxAlternates=1

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
        if (numArgs<3) call usage('Error: Not enough command arguments')

        ! user's HOME directory
#if defined GLNX
        call get_environment_variable("HOME", dirUNIV)
#else
        dirUNIV = SPACE
#endif

        ! University code
        call getarg(1, UniversityCode)

        ! append University code to HOME directory
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
        dirBACKUP = trim(dirUNIV)//'backup-'//trim(startDateTime)//DASH//trim(fileEXE)//DIRSEP
        call make_directory( dirBACKUP, .true. )
        call make_directory( trim(dirBACKUP)//pathToYear, .true.  )
        call make_directory( trim(dirBACKUP)//dataSource, .true.  )
        do iTmp=1,3
            call make_directory( trim(dirBACKUP)//trim(pathToYear)//trim(txtSemester(iTmp))//DIRSEP, .true.)
            call make_directory( trim(dirBACKUP)//trim(dataSource)//trim(txtSemester(iTmp))//DIRSEP, .true.)
        end do

        ! directory for start of School Year
        pathToYear = trim(dirDATA)//trim(itoa(currentYear))//DIRSEP

        ! directory for year of next term
        pathToNextYear = trim(dirDATA)//trim(itoa(nextYear))//DIRSEP

        ! consolidated academic data file exists?
        inquire(file=trim(pathToYear)//'CATALOG.XML', exist=hasCATALOG)

        ! read the university-level data
        call read_university(pathToYear, errNo)
        if (errNo/=0) call terminate('Error in reading university info')

        ! read the colleges
        call read_colleges(pathToYear, errNo)
        if (errNo/=0) call terminate('Error in reading the list of colleges')

        ! log directory for users in the college
        do iTmp=1,NumColleges
            call make_directory( trim(dirLOG)//trim(College(iTmp)%Code)//DIRSEP )
        end do

        ! read the departments
        call read_departments(pathToYear, errNo)
        if (errNo/=0) call terminate('Error in reading the list of departments')

        ! read the subjects
        call read_subjects(pathToYear, errNo)
        if (errNo/=0) call terminate('Error in reading the list of subjects')

        ! read the curricular programs
        call read_curricula(pathToYear, errNo)
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

        ! read the teachers
        call read_teachers(pathToYear, errNo)

        ! read the rooms
        call read_rooms(pathToYear, errNo)

        ! mark colleges with subject or curriculum information
        do jTmp=1,NumDepartments
            iTmp = Department(jTmp)%CollegeIdx
            College(iTmp)%hasInfo = College(iTmp)%hasInfo .or. Department(jTmp)%hasInfo
        end do

        ! initialize update files and create backup CATALOG.XML
        if (hasCATALOG) then
            call xml_write_university(trim(pathToYear)//'UNIVERSITY.XML')
            call xml_write_colleges(trim(pathToYear)//'COLLEGES.XML')
            call xml_write_departments(trim(pathToYear)//'DEPARTMENTS.XML')
            call xml_write_rooms(trim(pathToYear)//'ROOMS.XML')
            call xml_write_teachers(trim(pathToYear)//'TEACHERS.XML')
            call xml_write_subjects(trim(pathToYear)//'SUBJECTS.XML')
            call xml_write_curricula(trim(pathToYear)//'CURRICULA.XML')
            call xml_write_equivalencies(trim(pathToYear)//'EQUIVALENCIES.XML')
            call move_to_backup(trim(pathToYear)//'CATALOG.XML')

        else
            call xml_write_catalog(trim(dirBACKUP)//trim(itoa(currentYear))//DIRSEP//'CATALOG.XML')

        end if

        ! initialize data on classes
        NumSections = 0
        call initialize_section (Section(1,0))
        Section(:,:) = Section(1,0)

        ! initialize data on blocks
        NumBlocks = 0
        call initialize_block(Block(1,0))
        Block(:,:) = Block(1,0)

        ! show schedules for which terms ?
        termBegin = currentTerm
        termEnd = termBegin

        ! action flags
        isActionAdvising = .false.
        isActionClasslists = .false.

        ! allow files to be rewritten by default
        noWrites = .false.

    end subroutine initializations


end module INITIALIZE
