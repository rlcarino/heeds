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

module INITIALIZE

    use XMLIO

    implicit none


contains


!===========================================================
! get_arguments
!===========================================================

    subroutine get_arguments()

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

        ! arguments are: PATH UNIV YEAR TERM ACTION
        numArgs = iargc()
        if (numArgs<3) call usage('Error: command arguments are UNIV YEAR TERM ACTION')

        ! ACTION
        call getarg(4, ACTION)
        if (len_trim(ACTION)==0) ACTION = 'Server'

        ! instance will be running as mirror?
        isMirror = trim(ACTION)=='Mirror'
        isReadOnly = isMirror
#if defined no_password_check
        isReadOnly = .true.
#endif
        isAllowedNonEditors = .true.

#if defined UPLB
        isProbabilistic = .false.
#endif
        ! University code
        call getarg(1, UniversityCode)

        ! start YEAR for the current School Year
        call getarg(2, dataSource)
        currentYear = atoi(dataSource)

        ! current TERM
        call getarg(3, dataSource)
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

        ! configure HEEDS_DIRECTORY, DOCUMENT_ROOT, SERVER_NAME, SERVER_PROTOCOL
        call read_configuration(trim(UniversityCode)//dotXML, iTmp)

        ! dirUNIV - location of files for target University
        dirUNIV = trim(HEEDS_DIRECTORY)//DIRSEP//trim(UniversityCode)//DIRSEP
        inquire(file=trim(dirUNIV), exist=pathExists)
        if (.not. pathExists) call usage('Error: data directory '//trim(dirUNIV)//' does not exist')

        ! data directory
        dirDATA = trim(dirUNIV)//'dat'//DIRSEP
        lenDirDATA = len_trim(dirDATA)
        call make_directory( trim(dirData)//itoa(currentYear) )

        ! backup directory
        dirBACKUP = trim(dirUNIV)//'bak'//DIRSEP
        call make_directory( trim(dirBACKUP)//itoa(currentYear) )

        ! date-related initializations
        call initialize_date_change()

        ! PHILIPPINE STANDARD GEOGRAPHIC CODE (PSGC)
        call read_PSGC()
        call log_comment (itoa(NumPSGC)//' in PSGC.XML')

        ! Mother tongues
        call read_TONGUES()
        call log_comment (itoa(NumTongue)//' in TONGUES.XML')

        ! Teacher evaluation form
        call read_EVALUATION_FORM()
        call log_comment(itoa(NumEvalCriteria)//FSLASH//itoa(NumEvalCategories)// &
            ' evaluation criteria/categories in EVALUATION.XML')

    end subroutine get_arguments


    subroutine year_data_initialize ()

        integer :: iTmp
        character (len=MAX_LEN_FILE_PATH) :: dataSource
        logical :: pathExists

        ! accounting
        NumAccounts = 0
        AccountCode = SPACE
        AccountDescription = SPACE
        AccountCode(0) = 'NOT FOUND'
        AccountDescription(0) = '(Account code not found)'

        ! fees
        FeeAmount = 0.0
        FeeDescription = SPACE

        ! scholarships
        ScholarshipCode = SPACE
        ScholarshipDescription = SPACE

        ! the colleges
        NumColleges = 0
        call initialize_college (College(0))
        College(1:) = College(0)

        ! the units
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
        Teacher(NumTeachers)%TeacherId = GUEST
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

        ! classes
        NumSections = 0
        call initialize_section (Section(1,0))
        Section(:,:) = Section(1,0)

        ! blocks
        NumBlocks = 0
        call initialize_block(Block(1,0))
        Block(:,:) = Block(1,0)

        ! pre-enlisted, advised subjects of students
        NumEnlistment = 0

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

    end subroutine year_data_initialize


    subroutine initialize_past_future_years_terms (currentYear, currentTerm)

        integer, intent(in) :: currentYear, currentTerm
        integer :: iTmp

        select case (currentTerm)

            case (1)
                cTm3Year = currentYear-1 ! one school year ago (year)
                cTm3 = 1                 ! 3 terms ago (1st)
                cTm2Year = currentYear-1
                cTm2 = 2                 ! 2 terms ago (2nd)
                cTm1Year = currentYear-1 ! year of previous term
                cTm1 = 3                 ! 1 term ago (summer)

            case (2)
                cTm3Year = currentYear-1
                cTm3 = 2
                cTm2Year = currentYear-1
                cTm2 = 3
                cTm1Year = currentYear
                cTm1 = 1

            case (3)
                cTm3Year = currentYear-1
                cTm3 = 3
                cTm2Year = currentYear
                cTm2 = 1
                cTm1Year = currentYear
                cTm1 = 2

        end select

        call set_directories_and_indexes(currentYear)
        call make_directory( dirCOLLEGES )
        call make_directory( dirDEPARTMENTS )
        call make_directory( dirSUBJECTS )
        call make_directory( dirCURRICULA )
        call make_directory( dirROOMS )
        call make_directory( dirTEACHERS )
        call make_directory( dirSTUDENTS )
        do iTmp=firstSemester, summerTerm
            call make_directory( dirCLASSES(iTmp) )
            call make_directory( dirBLOCKS(iTmp) )
            call make_directory( dirEVALUATIONS(iTmp) )
        end do

    end subroutine initialize_past_future_years_terms


    subroutine set_directories_and_indexes(year)
        integer, intent(in) :: year
        integer :: iTmp

        ! directory for start of School Year
        pathToYear = trim(dirDATA)//trim(itoa(year))//DIRSEP

        ! data directories
        dirCOLLEGES      = trim(pathToYear)//'COLLEGES'//DIRSEP
        dirDEPARTMENTS   = trim(pathToYear)//'DEPARTMENTS'//DIRSEP
        dirSUBJECTS      = trim(pathToYear)//'SUBJECTS'//DIRSEP
        dirCURRICULA     = trim(pathToYear)//'CURRICULA'//DIRSEP
        dirROOMS         = trim(pathToYear)//'ROOMS'//DIRSEP
        dirTEACHERS      = trim(pathToYear)//'TEACHERS'//DIRSEP
        dirSTUDENTS      = trim(pathToYear)//'STUDENTS'//DIRSEP

        do iTmp=firstSemester, summerTerm
            dirCLASSES(iTmp)       = trim(pathToYear)//'CLASSES'//DIRSEP//trim(txtSemester(iTmp))//DIRSEP
            indexCLASSES(iTmp)     = trim(dirCLASSES(iTmp))//'index'

            dirBLOCKS(iTmp)        = trim(pathToYear)//'BLOCKS'//DIRSEP//trim(txtSemester(iTmp))//DIRSEP
            indexBLOCKS(iTmp)      = trim(dirBLOCKS(iTmp))//'index'

            ! directory for teacher evaluation "forms"
            dirEVALUATIONS(iTmp)   = trim(dirDATA)//'evaluations'//DIRSEP// &
                trim(itoa(year))//DIRSEP//trim(txtSemester(iTmp))//DIRSEP

        end do

    end subroutine set_directories_and_indexes


    ! date-related initializations
    subroutine initialize_date_change()
        ! log directory

        dirLOG = trim(dirUNIV)//'log-'//trim(itoa(currentYear))//DIRSEP
        call make_directory( trim(dirLOG)//currentDate )

        call log_comment( '-------', 'Begins '//currentDate//DASH//currentTime, '-------', &
            'Executable is '//fileEXE )

        ! initialize log for requests
        !fileREQ = trim(dirLOG)//currentDate//DIRSEP//startDateTime//'-request.log'
        fileREQ = trim(dirLOG)//currentDate//DIRSEP//currentTime(:6)//DASH//trim(ACTION)//'-request.log'
        call log_request(unitREQ, trim(fileREQ), &
            '#-------', '#Begins '//currentDate//DASH//currentTime, '#-------', &
            '#Executable is : '//trim(fileEXE), &
            '#Arguments are : '//trim(UniversityCode)//SPACE//itoa(currentYear)//trim(txtSemester(currentTerm)) )

        ! open a 'scratch' HTML response file
        !open(unit=unitHTML, file=trim(dirLOG)//currentDate//DIRSEP//startDateTime//'-scratch.html', &
        open(unit=unitHTML, file=trim(dirLOG)//currentDate//DIRSEP//currentTime(:6)//DASH//trim(ACTION)//'-scratch.html', &
            form='formatted', status='unknown')

    end subroutine initialize_date_change

end module INITIALIZE
