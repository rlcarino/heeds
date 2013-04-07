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


program MAIN

    use WEBSERVER
!    use SCHEDULING

    implicit none

    integer :: kTmp, jTmp, iTmp, errNo, numArgs
    character (len=MAX_LEN_FILE_PATH) :: dataSource
    logical :: pathExists
    integer :: idxGrp=0, MaxAlternates=1

    ! program starts
    call date_and_time (date=currentDate,time=currentTime)
    startDateTime = currentDate//DASH//currentTime(:6)

    ! initialize random seed
    call initialize_random_seed()

    ! the executable
    call getarg(0, fileExecutable)
    iTmp = len_trim(fileExecutable)
    do while (iTmp>0)
        if (fileExecutable(iTmp:iTmp)/=DIRSEP) then
            iTmp = iTmp-1
        else
            exit
        end if
    end do
    fileExecutable = fileExecutable(iTmp+1:)

    ! arguments are: UNIV YEAR TERM ACTION
    numArgs = iargc()
    if (numArgs<4) call usage('Error: Not enough command arguments')

    ! get UNIV YEAR TERM ACTION
    call getarg(1, UniversityCode)

    ! HEEDS root directory
#if defined GLNX
    call get_environment_variable("HOME", dirHEEDS)
#else
    call get_environment_variable("HOMEDRIVE", dirHEEDS)
#endif
    dirHEEDS = trim(dirHEEDS)//DIRSEP//'HEEDS'//DIRSEP//'dat'//DIRSEP//trim(UniversityCode)//DIRSEP
    inquire(file=trim(dirHEEDS), exist=pathExists)
    if (.not. pathExists) call usage('Error: directory '//trim(dirHEEDS)//' does not exist')

    call getarg(2, dataSource)
    currentYear = atoi(dataSource)

    call getarg(3, dataSource)
    select case (trim(dataSource))

        case ('1')
            currentTerm = 1
            prevYearYear = currentYear-1 ! one school year ago (year)
            prevYearTerm = 1             ! one school year ago (1st)
            prevTermYear = currentYear-1 ! year of previous term
            prevTermTerm = 3             ! term of previous term (summer)
            nextYear = currentYear       ! year of next term (the same year)
            nextTerm = 2                 ! next term (2nd)

        case ('2')
            currentTerm = 2
            prevYearYear = currentYear-1
            prevYearTerm = 2
            prevTermYear = currentYear
            prevTermTerm = 1
            nextYear = currentYear
            nextTerm = 3

        case ('S')
            currentTerm = 3
            prevYearYear = currentYear-1
            prevYearTerm = 3
            prevTermYear = currentYear
            prevTermTerm = 2
            nextYear = currentYear+1
            nextTerm = 1

        case default
            currentTerm = 0

    end select

    ! valid currentYear and currentTerm?
    if (currentYear*currentTerm<=0) then
        call usage('Error: Invalid YEAR and/or TERM')
    end if

    ! show schedules for which terms ?
    termBegin = currentTerm
    termEnd = termBegin+2

    ! allow files to be rewritten by default
    noWrites = .false.
    call getarg(4, ACTION)
    call lower_case(ACTION)
    ! ACTION to 1st capitalized
    call upper_case(ACTION(1:1))

    select case (trim(ACTION))

!        case ('Randomize')

        case ('Rename')
            termBegin = currentTerm
            termEnd = termBegin+2

        case ('Resetpasswords')

        case ('Checklists')

        case ('Predict')
            if (numArgs>4) then
                call getarg(5, dataSource)
                call lower_case(dataSource)
                UseCLASSES = trim(dataSource)=='use_classes'
            else
                UseCLASSES = .false.
            end if

        case ('Pre-enlist') ! get priority group
            idxGrp = -1
            if (numArgs>4) then
                call getarg(5, dataSource)
                idxGrp = atoi(dataSource)
            end if
            if (idxGrp<0 .or. idxGrp>7) then
                call usage('Error: "'//trim(ACTION)//'" requires a priority group from 0 to 6.')
            end if

            ! the last argument is the no. of alternate subjects
            maxAlternates = 0
            if (numArgs>5) then
                call getarg(6, dataSource)
                maxAlternates = atoi(dataSource)
                if (maxAlternates<0) maxAlternates = 0
            end if

        case ('Classes')
            termBegin = currentTerm
            termEnd = termBegin+2
            if (numArgs>4) then
                call getarg(5, dataSource)
                call lower_case(dataSource)
                noWrites = trim(dataSource)=='training'
            end if

        case ('Advising')
            termBegin = currentTerm+1
            termEnd = termBegin
            if (numArgs>4) then
                call getarg(5, dataSource)
                call lower_case(dataSource)
                noWrites = trim(dataSource)=='training'
            end if

        case ('Enlistment')
            nextYear = currentYear
            nextTerm = currentTerm
            termBegin = currentTerm
            termEnd = termBegin
            if (numArgs>4) then
                call getarg(5, dataSource)
                call lower_case(dataSource)
                noWrites = trim(dataSource)=='training'
            end if

        case default
            call usage('Error: ACTION "'//trim(ACTION)//'" is not recognized.')

    end select

    write(*,AFORMAT) &
        'Executable is : '//trim(fileExecutable)//SPACE//'('//VERSION//')', &
        'Arguments are : '//trim(UniversityCode)//SPACE//itoa(currentYear)//SPACE// &
        txtSemester(currentTerm)//SPACE//trim(ACTION)

    ! set up directories
    call make_heeds_directories()

    ! open the I/O log file
    open(unit=unitLOG, file=trim(dirLOG)//trim(fileExecutable)//'.log', status='unknown')
    call file_log_message( '-------', 'Begins '//currentDate//DASH//currentTime, '-------', &
        'Executable is : '//trim(fileExecutable)//SPACE//DASH//SPACE//'( '//PROGNAME//VERSION//')', &
        'Arguments are : '//trim(UniversityCode)//SPACE//itoa(currentYear)//SPACE// &
        txtSemester(currentTerm)//SPACE//trim(ACTION) )

    ! open log for requests only
    open(unit=unitREQ, file=trim(dirLOG)//'requests.log', status='unknown')

    ! open a 'scratch' HTML response file
#if defined PRODUCTION
    open(unit=unitHTML, form='formatted', status='scratch')
#else
    open(unit=unitHTML, file=trim(dirLOG)//'scratch.html', form='formatted', status='unknown')
#endif

    ! initialize data on classes
    NumSections = 0
    call initialize_section (Section(1,0))
    Section(:,:) = Section(1,0)

    ! initialize data on blocks
    NumBlocks = 0
    call initialize_block(Block(1,0))
    Block(:,:) = Block(1,0)

    ! initialize freshman quotas
    NFintake = 0

    ! initialize student data
    call initialize_student(Student(0))
    Student = Student(0)
    NumStudents = 0
    do iTmp=1,MAX_ALL_STUDENTS
        StdRank(iTmp) = iTmp
    end do

    ! initialize pre-enlisted subjects of students
    NumEnlistmentRecords = 0
    call initialize_pre_enlistment(Preenlisted(0))
    Preenlisted = Preenlisted(0)

    ! initialize predictions
    NumPredictionRecords = 0
    call initialize_pre_enlistment(Advised(0))
    Advised = Advised(0)

    ! initialize waivers
    NumWaiverRecords = 0
    call initialize_waiver(WaiverCOI(0))
    WaiverCOI = WaiverCOI(0)

    ! read the university-level data
    call read_university(pathToYear, errNo)
    if (errNo/=0) call terminate('Error in reading university info')

    ! read the colleges
    call read_colleges(pathToYear, errNo)
    if (errNo/=0) call terminate('Error in reading the list of colleges')

    ! read the departments
    call read_departments(pathToYear, errNo)
    if (errNo/=0) call terminate('Error in reading the list of departments')

    ! read the subjects
    call read_subjects(pathToYear, errNo)
    if (errNo/=0) call terminate('Error in reading the list of subjects')

    ! string representation of percentage grade, float value, reference
    do iTmp=1,100
        txtGrade(ZERO_PERCENT_GRADE+iTmp) = itoa(iTmp)
        fGrade(ZERO_PERCENT_GRADE+iTmp) = 1.0*iTmp
        pGrade(ZERO_PERCENT_GRADE+iTmp) = ZERO_PERCENT_GRADE+iTmp
    end do

    ! generate checklists only?
    if (trim(ACTION)=='Checklists') then
        ! extract grades from FINALGRADE(.CSV) in dirRAW
        call extract_student_grades()
        ! create student directories
        call make_student_directories()
        do iTmp=1,NumStudents
            call xml_write_student_grades(iTmp)
            if (mod(iTmp,1000)==0) write(*,*) iTmp,  ' done...'
        end do
        call terminate(trim(fileExecutable)//' completed '//ACTION)
    end if

    ! read the curricular programs
    call read_curricula(pathToYear, errNo)
    if (errNo/=0) call terminate('Error in reading the list of curricular programs')

#if defined UPLB
    ! no need to reset term offered of a subject
#else
    ! set term offered of a subject to when it is taken in curricular programs
    call file_log_message('Resetting subject term of offering to when taken in curricular programs...')
    call set_term_offered_accg_to_curricula(Offering(1,MAX_ALL_DUMMY_SUBJECTS:))
#endif

    ! Synchronize pre-requisites of co-requisite subjects
    ! For example, CHEM 17.0 has MATH 11 or MATH 17, CHEM 17.1 has NONE. Set
    ! pre-requisite of CHEM 17.1 to that of CHEM 17
    call file_log_message('Synchronizing pre-requisites of co-requisite subjects...')
    do targetSubject=1,NumSubjects
        if (Subject(Subject(targetSubject)%Prerequisite(1))%Name/='NONE') cycle ! pre-requisite is NONE
        if (Subject(targetSubject)%lenCoreq/=1) cycle ! should be one token only
        itmp = Subject(targetSubject)%Corequisite(1)
        if (itmp<=0) cycle ! token should be a named subject
        call file_log_message(Subject(targetSubject)%Name//'has co-requisite '//Subject(itmp)%Name)
        ! pre-requisite is NONE, co-requisite is a named subject
        Subject(targetSubject)%lenPreq = Subject(itmp)%lenPreq
        Subject(targetSubject)%Prerequisite = Subject(itmp)%Prerequisite
    end do

    ! read the teachers
    call read_teachers(pathToYear, errNo)
    if (errNo/=0) call terminate('Error in reading the list of teachers')

    ! reset passwords only?
    if (trim(ACTION)=='Resetpasswords') then
        call reset_passwords()
        call terminate(trim(fileExecutable)//' completed '//ACTION)
    end if

    ! read the rooms
    call read_rooms(pathToYear, errNo)
    if (errNo/=0) call terminate('Error in reading the list of rooms')

    ! mark colleges with subject or curriculum information
    do targetDepartment=1,NumDepartments
        iTmp = Department(targetDepartment)%CollegeIdx
        College(iTmp)%hasInfo = College(iTmp)%hasInfo .or. Department(targetDepartment)%hasInfo
    end do

    ! preparing the schedule of classes ?
    if (trim(ACTION)=='Classes') then

        ! read schedules
        do kTmp=termBegin,termEnd
            call qualify_term (kTmp, iTmp, jTmp, dataSource)
            pathToTerm = trim(itoa(iTmp))//DIRSEP//trim(txtSemester(jTmp))//DIRSEP
            ! read the classes
            call read_classes(pathToTerm, NumSections(kTmp), Section(kTmp,0:), &
                Offering(kTmp,MAX_ALL_DUMMY_SUBJECTS:), errNo)
            ! read the blocks
            call read_blocks(pathToTerm, NumBlocks(kTmp), Block(kTmp,0:), NumSections(kTmp), Section(kTmp,0:), errNo)
            ! get no. of sections by dept
            call count_sections_by_dept(kTmp, NumSections(kTmp), Section(kTmp,0:))
        end do

        ! start server-mode
        call server_start()

    end if

    ! read the list of students
    call read_students(pathToYear, errNo)
    if (errNo/=0) call terminate('Error in reading the list of students')

    ! create student directories
    call make_student_directories()

!    ! randomize grades?
!    if (trim(ACTION)=='Randomize') then
!        write(*,*)  'Randomizing grades... please wait...'
!        do iTmp = 1,NumStudents
!            if (mod(iTmp,1000) == 0) then
!                write(*,*) trim(itoa(iTmp))//' / '//itoa(NumStudents)//' done...'
!            end if
!            jTmp = StdRank(iTmp)
!            call remake_student_records (jTmp)
!        end do
!        call terminate(trim(fileExecutable)//' completed '//ACTION)
!    end if

    ! predict for nextTerm?
    if (trim(ACTION)=='Predict') then

        ! read classes for next term
        pathToTerm = trim(pathToNextYear)//trim(txtSemester(nextTerm))//DIRSEP
        call read_classes(pathToTerm, NumSections(nextTerm), Section(nextTerm,0:), &
            Offering(nextTerm,MAX_ALL_DUMMY_SUBJECTS:), errNo)

        ! read waivers for next term
        call read_waivers(pathToTerm, NumSections(nextTerm), Section(nextTerm,0:), &
            Offering(nextTerm,MAX_ALL_DUMMY_SUBJECTS:), NumWaiverRecords, errNo)

        ! predict for each student
        call advise_all_students(UseCLASSES, Offering(nextTerm,MAX_ALL_DUMMY_SUBJECTS:))

        ! write predictions
        call xml_write_pre_enlistment(pathToTerm, 'PREDICTIONS', Advised, Section(nextTerm,0:))

        ! invalidate pre-enlistment by moving ENLISTMENT files to backup
        dataSource = trim(dirXML)//trim(pathToTerm)//'ENLISTMENT'

        call move_to_backup(trim(dataSource)//'.XML') ! the monolithic enlistment file

        do iTmp=2,7 ! the enlistment files for each group
            call move_to_backup(trim(dataSource)//DASH//trim(itoa(iTmp))//'.XML')
        end do
        call terminate(trim(fileExecutable)//' completed '//ACTION)

    end if

    ! advising students ?
    if (trim(ACTION)=='Advising') then

        ! read classes for next term
        pathToTerm = trim(pathToNextYear)//trim(txtSemester(nextTerm))//DIRSEP
        call read_classes(pathToTerm, NumSections(nextTerm), Section(nextTerm,0:), &
            Offering(nextTerm,MAX_ALL_DUMMY_SUBJECTS:), errNo)

        ! read waivers for next term
        call read_waivers(pathToTerm, NumSections(nextTerm), Section(nextTerm,0:), &
            Offering(nextTerm,MAX_ALL_DUMMY_SUBJECTS:), NumWaiverRecords, errNo)

        ! read predictions for next term
        call read_predictions(pathToTerm, NumSections(nextTerm), Section(nextTerm,0:), &
            Advised, NumPredictionRecords, errNo)

        ! freshmen quota for next term
        call xml_read_intake(pathToTerm, errNo) ! try the XML file
        if (errNo/=0) then ! something wrong with XML file
            NFintake(1:NumCurricula-1) = 1
            call xml_write_intake(pathToTerm)
        end if
        call file_log_message('# incoming freshmen next term ='//trim(itoa(sum(NFintake)))//'?')

        ! start server-mode
        call server_start()

    end if

!    ! pre-enlist students?
!    if (trim(ACTION)=='Pre-enlist') then
!        call generate_initial_schedules(idxGrp, maxAlternates)
!        call terminate(trim(fileExecutable)//' completed '//ACTION)
!    end if

    if (trim(ACTION)=='Enlistment') then

        ! read classes for current term
        pathToTerm = trim(pathToYear)//trim(txtSemester(currentTerm))//DIRSEP
        call read_classes(pathToTerm, NumSections(currentTerm), Section(currentTerm,0:), &
            Offering(currentTerm,MAX_ALL_DUMMY_SUBJECTS:), errNo)

        ! read enlistment files (if any) for currentTerm
        call read_pre_enlistment(pathToTerm, 'ENLISTMENT', 0, 6, &
            NumSections(currentTerm), Section(currentTerm,0:), Preenlisted, NumEnlistmentRecords, errNo)

        call recalculate_available_seats(Section(currentTerm,0:))

        ! start server-mode
        call server_start()

    end if

    ! obfuscate the University?
    if (trim(ACTION)=='Rename') then
        call rename_university()
        call terminate(trim(fileExecutable)//' completed '//ACTION)
    end if


contains


    subroutine usage(mesg)

        character(len=*), intent(in), optional :: mesg

        write(*,AFORMAT) trim(fileExecutable)//' is version'//VERSION//' of '//PROGNAME
        if (present(mesg)) write(*,AFORMAT) trim(mesg)
        write(*,AFORMAT) SPACE, &
            'Non-interactive usage: '//trim(fileExecutable)//' UNIV YEAR TERM ACTION', &
            '  UNIV - code for university', &
            '  YEAR - chronological year when the current School Year started', &
            '  TERM - current term: 1=1st Sem, 2=2nd Sem, S=summer', &
            '  ACTION - ', &
            '      resetpasswords - reset passwords to usernames, based on TEACHERS.XML for current term', &
            '      checklists - create enrollment records of students from raw grades', &
            '      predict [use_classes] - predict subject demand for next term [use CLASSES.XML for subjects offered]', &
            '      pre-enlist group maxalt - pre-enlist group, use max no. of alternate subjects, for next term', &
            SPACE
#if defined GLNX
        write(*,AFORMAT) &
            'Webserver usage: spawn-fcgi -a IP_ADDRESS -p PORT_NUM -- '//trim(fileExecutable)//' UNIV YEAR TERM ACTION'
#else
        write(*,AFORMAT) &
            'Webserver usage: spawn-fcgi -a IP_ADDRESS -p PORT_NUM -f "'//trim(fileExecutable)//' UNIV YEAR TERM ACTION"'
#endif
        write(*,AFORMAT) &
            '  IP_ADDRESS, PORT_NUM - as specified by fastcgi_pass in nginx configuration', &
            '  UNIV, YEAR, TERM - same as above', &
            '  ACTION - ', &
            '      classes - edit the schedule of classes, for the current and next two terms', &
            '      advising - advise students, on their subjects next term', &
            '      enlistment - finalize enlistment of students into classes, for the current term', SPACE
         stop
    end subroutine usage


    subroutine make_heeds_directories()

        ! directory for raw input data
        dirRAW = trim(dirHEEDS)//'raw'//DIRSEP

        ! directory for the School Year
        pathToYear = trim(itoa(currentYear))//DIRSEP
        ! temporarily set pathToNextYear to absolute next year; reset before exit
        pathToNextYear = trim(itoa(currentYear+1))//DIRSEP

        ! directory for log files
#if defined PRODUCTION
        dirLOG = trim(dirHEEDS)//'log'//DIRSEP//trim(startDateTime)//DIRSEP
#else
        dirLOG = trim(dirHEEDS)//'log'//DIRSEP//'debug'//DIRSEP
#endif
        call make_clean_directory( dirLOG, .true. )

        ! directory for XML data
        dirXML = trim(dirHEEDS)//'xml'//DIRSEP
        call make_clean_directory( dirXML )
        lenDirXML = len_trim(dirXML)

        call make_clean_directory( trim(dirXML)//pathToYear )
        call make_clean_directory( trim(dirXML)//pathToNextYear )
        do iTmp=1,3
            call make_clean_directory( trim(dirXML)//trim(pathToYear)//trim(txtSemester(iTmp))//DIRSEP )
            call make_clean_directory( trim(dirXML)//trim(pathToNextYear)//trim(txtSemester(iTmp))//DIRSEP )
            call make_clean_directory( trim(dirXML)//UPDATES//trim(pathToYear)//trim(txtSemester(iTmp))//DIRSEP )
            call make_clean_directory( trim(dirXML)//UPDATES//trim(pathToNextYear)//trim(txtSemester(iTmp))//DIRSEP )
        end do

        ! directory for backups
#if defined PRODUCTION
        dirBAK = trim(dirHEEDS)//'bak'//DIRSEP//trim(startDateTime)//DIRSEP
#else
        dirBAK = trim(dirHEEDS)//'bak'//DIRSEP//'debug'//DIRSEP
#endif
        call make_clean_directory( dirBAK )
        call make_clean_directory( trim(dirBAK)//pathToYear )
        call make_clean_directory( trim(dirBAK)//pathToNextYear )
        do iTmp=1,3
            call make_clean_directory( trim(dirBAK)//trim(pathToYear)//trim(txtSemester(iTmp))//DIRSEP, .true.)
            call make_clean_directory( trim(dirBAK)//trim(pathToNextYear)//trim(txtSemester(iTmp))//DIRSEP, .true.)
            call make_clean_directory( trim(dirBAK)//UPDATES//trim(pathToYear)//trim(txtSemester(iTmp))//DIRSEP, .true.)
            call make_clean_directory( trim(dirBAK)//UPDATES//trim(pathToNextYear)//trim(txtSemester(iTmp))//DIRSEP, .true.)
        end do

        ! reset pathToNextYear
        pathToNextYear = trim(itoa(nextYear))//DIRSEP

        return
    end subroutine make_heeds_directories


    subroutine make_student_directories()

        ! create student directories
        dirSUBSTITUTIONS    = trim(dirXML)//'substitutions'//DIRSEP ! directory for input/UNEDITED checklists from Registrar
        dirTRANSCRIPTS      = trim(dirXML)//'transcripts'//DIRSEP ! directory for raw transcripts
        dirEditedCHECKLISTS = trim(dirXML)//'edited-checklists'//DIRSEP ! for output/EDITED checklists from College Secretaries

        call make_clean_directory( dirTRANSCRIPTS )
        call make_clean_directory( dirSUBSTITUTIONS )
        call make_clean_directory( dirEditedCHECKLISTS )

        call make_clean_directory( trim(dirBAK)//'transcripts' )
        call make_clean_directory( trim(dirBAK)//'substitutions' )
        call make_clean_directory( trim(dirBAK)//'edited-checklists' )

        call collect_prefix_years()
        itmp = 1
        do jtmp=2,len_trim(StdNoPrefix)
            if (StdNoPrefix(jtmp:jtmp)/=':') cycle

            call make_clean_directory( trim(dirXML)//'transcripts'//DIRSEP//StdNoPrefix(itmp+1:jtmp-1) )
            call make_clean_directory( trim(dirXML)//'substitutions'//DIRSEP//StdNoPrefix(itmp+1:jtmp-1) )
            call make_clean_directory( trim(dirXML)//'edited-checklists'//DIRSEP//StdNoPrefix(itmp+1:jtmp-1) )

            call make_clean_directory( trim(dirBAK)//'transcripts'//DIRSEP//StdNoPrefix(itmp+1:jtmp-1) )
            call make_clean_directory( trim(dirBAK)//'substitutions'//DIRSEP//StdNoPrefix(itmp+1:jtmp-1) )
            call make_clean_directory( trim(dirBAK)//'edited-checklists'//DIRSEP//StdNoPrefix(itmp+1:jtmp-1) )

            itmp = jtmp
        end do

        return
    end subroutine make_student_directories


    subroutine make_clean_directory(dirName, clean)
        character(len=*), intent (in) :: dirName
        logical, intent (in), optional :: clean
        logical :: pathExists
        integer :: ierr

        inquire(file=trim(dirName), exist=pathExists)
        if (.not. pathExists) then
            call system (mkdirCmd//trim(dirName), ierr)
        else if (present(clean)) then
            call system(delCmd//trim(dirName)//'*', ierr)
        end if

        return
    end subroutine make_clean_directory


    subroutine rename_university()

        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        !character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        real :: harvest
        integer :: lTmp

        ! read schedules
        do kTmp=termBegin,termEnd
            call qualify_term (kTmp, iTmp, jTmp, dataSource)
            pathToTerm = trim(itoa(iTmp))//DIRSEP//trim(txtSemester(jTmp))//DIRSEP
            ! read the classes
            call read_classes(pathToTerm, NumSections(kTmp), Section(kTmp,0:), &
                Offering(kTmp,MAX_ALL_DUMMY_SUBJECTS:), errNo)
            ! read the blocks
            call read_blocks(pathToTerm, NumBlocks(kTmp), Block(kTmp,0:), NumSections(kTmp), Section(kTmp,0:), errNo)
            ! get no. of sections by dept
            call count_sections_by_dept(kTmp, NumSections(kTmp), Section(kTmp,0:))
        end do

        ! student records
        do iTmp = 1,NumStudents
            if (mod(iTmp,1000) == 0) then
                write(*,*) trim(itoa(iTmp))//' / '//itoa(NumStudents)//' done reading ...'
            end if
            TCG = TYPE_STUDENT_RECORD (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, .false., SPACE, SPACE)
            lenTCG = 0
            call custom_read_substitutions (iTmp)
            call custom_read_student_grades (iTmp)
        end do

        ! read waivers for next term
        pathToTerm = trim(pathToNextYear)//trim(txtSemester(nextTerm))//DIRSEP
        call read_waivers(pathToTerm, NumSections(nextTerm), Section(nextTerm,0:), &
            Offering(nextTerm,MAX_ALL_DUMMY_SUBJECTS:), NumWaiverRecords, errNo)

        ! read predictions for next term
        call read_predictions(pathToTerm, NumSections(nextTerm), Section(nextTerm,0:), &
            Advised, NumPredictionRecords, errNo)

        ! read enlistment files (if any)
        call read_pre_enlistment(pathToTerm, 'ENLISTMENT', 0, 6, &
            NumSections(nextTerm), Section(nextTerm,0:), Preenlisted, NumEnlistmentRecords, errNo)

        ! rename University
        UniversityName = 'DEMO University'
        UniversityAddress = '(University Address)'
        UniversityPresident = 'Firstname MI. Lastname, Ph.D.'
        DeanOfInstruction = 'Firstname MI. Lastname, Ph.D.'
        VPAcademicAffairs = 'Firstname MI. Lastname, Ph.D.'
        DeanOfCampus = 'Firstname MI. Lastname, Ph.D.'
        REGISTRAR = 'Registrar'
        call xml_write_university(pathToYear)

!        ! rename colleges
!        do jTmp=1,NumColleges-1
!            College(jTmp)%Code = 'COLL'//itoa2bz(jTmp)
!            College(jTmp)%Name = 'College '//itoa2bz(jTmp)
!            College(jTmp)%Dean =  'Dean of College '//itoa2bz(jTmp)
!        end do
        jTmp = NumColleges
        College(jTmp)%Code = ADMINISTRATION
        College(jTmp)%Name = trim(UniversityName)//SPACE//ADMINISTRATION
        College(jTmp)%Dean = VPAcademicAffairs
        call xml_write_colleges(pathToYear)

!        ! rename departments
!        do iTmp=2,NumDepartments-1
!            Department(iTmp)%Code = 'DEPT'//itoa2bz(iTmp)
!            Department(iTmp)%Name = 'Department '//itoa2bz(iTmp)//' in '//College(Department(iTmp)%CollegeIdx)%Name
!        end do
        iTmp = NumDepartments
        REGISTRAR = 'Registrar'
        Department(iTmp)%Code = 'Registrar'
        Department(iTmp)%Name = trim(UniversityName)//SPACE//REGISTRAR
        Department(iTmp)%SectionPrefix = SPACE
        call xml_write_departments(pathToYear)

        ! rename rooms
        do jTmp=1,NumDepartments
            kTmp = 0
            do iTmp=1,NumRooms+NumAdditionalRooms
                if (jTmp/=Room(iTmp)%DeptIdx) cycle
                kTmp = kTmp + 1
                Room(iTmp)%Code = trim(Department(jTmp)%Code)//' Rm'//itoa2bz(kTmp)
            end do
        end do
        call xml_write_rooms(pathToYear)

!        ! rename teachers
!        do jTmp=1,NumDepartments
!            kTmp = 0
!            do iTmp=1,NumTeachers+NumAdditionalTeachers
!                if (jTmp/=Teacher(iTmp)%DeptIdx) cycle
!                kTmp = kTmp + 1
!                Teacher(iTmp)%TeacherID = 'T'//itoa2bz(kTmp)//'D'//itoa2bz(jTmp)
!                Teacher(iTmp)%Name = itoa2bz(kTmp)//'TeacherID in '//Department(jTmp)%Code
!            end do
!        end do

        ! rename subjects
        tSubject(1:) = 'X'
        iTmp = 0 ! area number
        kTmp = 0 ! topic number in area
        do jTmp=1,NumSubjects
            errNo = index(Subject(jTmp)%Name, SPACE)
            if (tSubject(:errNo)==Subject(jTmp)%Name(:errNo)) then ! same as previous area
                kTmp = kTmp + 1
            else ! new area
                iTmp = iTmp+1
                kTmp = 1
                tSubject = Subject(jTmp)%Name(:errNo)
            end if
            Subject(jTmp)%Name = Subject(jTmp)%Name(:errNo)//itoa3bz(kTmp)
            !Subject(jTmp)%Name = 'A'//itoa3bz(iTmp)//SPACE//itoa3bz(kTmp)
            !Subject(jTmp)%Title = 'Subject area '//itoa3bz(iTmp)//', Topic '//itoa3bz(kTmp)
        end do

        iTmp = 0 ! area number for (Must be in Plan Of Study)
        kTmp = 0 ! topic number in area
        do jTmp=NumDummySubjects,0
            if (trim(Subject(jTmp)%Title)/='(Must be in Plan Of Study)') cycle
            kTmp = kTmp + 1
            Subject(jTmp)%Name = 'ELEC '//itoa3bz(kTmp)
            Subject(jTmp)%Title = 'Elective type '//itoa3bz(kTmp)//' - Must be in Plan Of Study '
        end do
        call xml_write_subjects(pathToYear)

        ! rename curricular programs
        done = .false.
        lTmp = 0
        do jTmp=1,NumCurricula
            if (done(jTmp)) cycle
            lTmp = lTmp+1
            kTmp = 0
            tCurriculum = CurrProgCode(jTmp)
            do iTmp=jTmp,NumCurricula
                if (CurrProgCode(iTmp)/=tCurriculum) cycle
                kTmp = kTmp+1
!                Curriculum(iTmp)%Code = 'P'//itoa3bz(lTmp)//DASH//'V'//itoa2bz(kTmp)
!                Curriculum(iTmp)%Title = 'Program '//itoa3bz(lTmp)
!                Curriculum(iTmp)%Specialization = 'Variant '//itoa2bz(kTmp)
!                Curriculum(iTmp)%Remark = SPACE
!                CurrProgCode(iTmp) = 'P'//itoa3bz(lTmp)
                Curriculum(iTmp)%Code = trim(CurrProgCode(iTmp))//'-V'//itoa2bz(kTmp)
                done(iTmp) = .true.
            end do
        end do
        call xml_write_curricula(pathToYear)
        call xml_write_equivalencies(pathToYear)

        ! rewrite classes
        do kTmp=termBegin,termEnd
            call qualify_term (kTmp, iTmp, jTmp, dataSource)
            pathToTerm = trim(itoa(iTmp))//DIRSEP//trim(txtSemester(jTmp))//DIRSEP
            call xml_write_sections(pathToTerm, NumSections(kTmp), Section(kTmp,0:), 0)
            call xml_write_blocks(pathToTerm, NumBlocks(kTmp), Block(kTmp,0:), Section(kTmp,0:), 0)
        end do

        pathToTerm = trim(pathToNextYear)//trim(txtSemester(nextTerm))//DIRSEP
        ! rewrite waivers for next term
        call xml_write_waivers(pathToTerm, Section(nextTerm,0:) )
        ! rewrite predictions for next term
        call xml_write_pre_enlistment(pathToTerm, 'PREDICTIONS', Advised, Section(nextTerm,0:) )
        ! rewrite enlistment for next term
        call xml_write_pre_enlistment(pathToTerm, 'ENLISTMENT', Preenlisted, Section(nextTerm,0:) )

    !        ! rename student
    !        tArray = 0 ! count for each curriculum
    !        do lTmp = 1,NumStudents
    !            kTmp = Student(lTmp)%CurriculumIdx
    !            iTmp = tArray(kTmp)+1
    !            tArray(kTmp) = iTmp
    !            Student(lTmp)%StdNo = Student(lTmp)%StdNo(:5)//itoa3bz(kTmp)//DASH//itoa3bz(iTmp)//'0'
    !            Student(lTmp)%Name = itoa3bz(iTmp)//'Student of Program '//trim(Curriculum(kTmp)%Code)
    !        end do
        call xml_write_students(pathToYear, 0)
        do lTmp = 1,NumStudents
            if (mod(lTmp,1000) == 0) then
                write(*,*) trim(itoa(lTmp))//' / '//itoa(NumStudents)//' done writing...'
            end if

            ! generate random passing grades
            do iTmp=1,Student(lTmp)%Record(1,0)
                if (Student(lTmp)%Record(4,iTmp)<=0) cycle
                jTmp = Student(lTmp)%Record(5,iTmp)
                kTmp = jTmp
                if (jTmp>0 .and. jTmp<10) then ! numeric pass
                    do while (kTmp==jTmp .or. kTmp==0)
                        call random_number(harvest)
                        kTmp = int(harvest*10.0)
                    end do
                end if
                Student(lTmp)%Record(5,iTmp) = kTmp
            end do
            call xml_write_student_grades(lTmp)

            call xml_write_substitutions(lTmp)
        end do

        ! teachers
        call reset_passwords()

        return

    end subroutine rename_university


end program MAIN
