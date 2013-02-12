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
    use SCHEDULING

    implicit none

    integer :: itmp, jtmp, errNo
    integer :: idxGrp=-1, maxAlternates=-1
    character (len=MAX_LEN_FILE_PATH) :: dataSource
    character (len=40) :: argString

    ! program starts
    call date_and_time (date=currentDate,time=currentTime)
    startDateTime = currentDate//DASH//currentTime(:6)

    ! initialize random seed
    call initialize_random_seed()

    ! initialize QUERY_STRING encryption key
    do iTmp=1,MAX_LEN_QUERY_STRING
        call random_number(harvest)
        errNo = 1 + int(255*harvest)
        queryEncryptionKey(iTmp:iTmp) = achar(errNo)
    end do

    ! string representation of percentage grade, float value, reference
    do iTmp=1,100
        txtGrade(ZERO_PERCENT_GRADE+iTmp) = itoa(iTmp)
        fGrade(ZERO_PERCENT_GRADE+iTmp) = 1.0*iTmp
        pGrade(ZERO_PERCENT_GRADE+iTmp) = ZERO_PERCENT_GRADE+iTmp
    end do

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

    ! 4+ arguments ?
    iTmp = iargc()
    if (iTmp<4) then
        write(*,AFORMAT) 'Usage: '//trim(fileExecutable)//' univ year term period action group', &
            '  where', &
            'univ      - university code', &
            'year      - the year when current Academic Year started', &
            'term      - 0=summer, 1=first sem, 2=second sem', &
            'period    - 1=enrollment period, 2=mid-term, 3=end-of-term (grades are available)', &
            'action    - checklists, advise, schedule, server, training', &
            'group     - priority group to schedule, if action=schedule'
        write(*,AFORMAT) ' ', 'You are running '//PROGNAME//VERSION
        stop
    end if

    ! get arguments
    call getarg(1, UniversityCode)
    call getarg(2, argString)
    currentYear = atoi(argString)
    call getarg(3, argString)
    currentTerm = atoi(argString)
    call getarg(4, argString)
    Period = atoi(argString)

    ! default mode
    argString = 'TRAINING'
    noWrites = .true.

    if (iTmp>4) then
        call getarg(5, argString)
        call upper_case(argString)

        select case (trim(argString))

            case ('CHECKLISTS')
                noWrites = .false. ! allow files to be rewritten

            case ('ADVISE')
                noWrites = .false. ! allow files to be rewritten

            case ('SCHEDULE') ! get priority group
                idxGrp = -1
                if (iTmp>5) then
                    call getarg(6, dataSource)
                    idxGrp = atoi(dataSource)
                end if
                if (idxGrp<0 .or. idxGrp>7) then
                    write(*,*) 'Action "'//trim(argString)//'" requires a priority group from 0 to 6.'
                    stop
                end if

                ! the last argument is the no. of alternate subjects
                maxAlternates = 0
                if (iTmp>6) then
                    call getarg(7, dataSource)
                    maxAlternates = atoi(dataSource)
                    if (maxAlternates<0) MaxAlternates = 0
                end if

                noWrites = .false. ! allow files to be rewritten

            case ('SERVER')
                noWrites = .false.

            case ('TRAINING')

            case ('RESETPASSWORDS')
                noWrites = .false.

            case default
                write(*,AFORMAT) &
                    'Action "'//trim(argString)//'" not recognized', &
                    'Valid actions are: checklists, advise, schedule, server, training', &
                    'STOPPED'
                stop

        end select

    end if

    write(*,*) trim(fileExecutable)//VERSION//' started '//currentDate//DASH//currentTime
    write(*,*) trim(UniversityCode), currentYear, currentTerm, Period, trim(argString), idxGrp, maxAlternates

    if (currentTerm==1) then
        prevYearYear = currentYear-1
        prevYearTerm = 1
        prevTermYear = currentYear
        prevTermTerm = 0
        nextYear = currentYear
        nextTerm = 2
    else if (currentTerm==2) then
        prevYearYear = currentYear-1
        prevYearTerm = 2
        prevTermYear = currentYear
        prevTermTerm = 1
        nextYear = currentYear
        nextTerm = 0
    else
        prevYearYear = currentYear-1
        prevYearTerm = 0
        prevTermYear = currentYear
        prevTermTerm = 2
        nextYear = currentYear+1
        nextTerm = 1
    end if

    ! predict for which term?
    if (Period==1) then ! enlistment period, forecasts are for current term
        targetYear = currentYear
        targetTerm = currentTerm
    else ! forecasts are for next term
        Period = 2 ! even if Period was set to 3 or 4
        targetYear = nextYear
        targetTerm = nextTerm
    end if
    pathToCurrent = trim(itoa(currentYear))//DIRSEP//trim(txtSemester(currentTerm))//DIRSEP
    pathToTarget = trim(itoa(targetYear))//DIRSEP//trim(txtSemester(targetTerm))//DIRSEP

    if (trim(argString)=='SCHEDULE') then ! use data from pathToTarget
        pathToCurrent = pathToTarget
        write(*,*) 'Creating schedules for SY '//trim(itoa(targetYear))//DASH//trim(itoa(targetYear+1))// &
             COMMA//space//trim(txtSemester(targetTerm))//' Term, group ', idxGrp
    end if

    ! fixed directories
    dirWWW                 = dirHEEDS//'web'//DIRSEP ! DocumentRoot in for webserver
    dirLog                 = dirHEEDS//'log'//DIRSEP//trim(startDateTime)//DIRSEP ! for log files
    dirUploadCHECKLISTS    = dirHEEDS//'web'//DIRSEP//'static'//DIRSEP//'upload-checklists'//DIRSEP ! individual checklists for upload
    dirUploadENLISTMENT    = dirHEEDS//'web'//DIRSEP//'static'//DIRSEP//'upload-enlistment'//DIRSEP ! enlisted classes by student

    call system (mkdirCmd//trim(dirLog))
    call system (mkdirCmd//trim(dirUploadCHECKLISTS))
    call system (mkdirCmd//trim(dirUploadENLISTMENT))

    ! directories for raw input data
    dirRAW    = dirHEEDS//'raw'//DIRSEP//trim(UniversityCode)//DIRSEP

    ! directories for XML input/ouput data
    dirXML                 = dirHEEDS//'xml'//DIRSEP//trim(UniversityCode)//DIRSEP !//currentDate//DASH//currentTime(:6)//DIRSEP
    dirSUBSTITUTIONS       = trim(dirXML)//'substitutions'//DIRSEP ! directory for input/UNEDITED checklists from Registrar
    dirTRANSCRIPTS         = trim(dirXML)//'transcripts'//DIRSEP ! directory for raw transcripts
    dirEditedCHECKLISTS    = trim(dirXML)//'edited-checklists'//DIRSEP ! for output/EDITED checklists from College Secretaries

    call system (mkdirCmd//trim(dirXML)//trim(pathToCurrent))
    call system (mkdirCmd//trim(dirXML)//UPDATES//trim(pathToCurrent))
    if (currentTerm/=targetTerm) then
        call system (mkdirCmd//trim(dirXML)//trim(pathToTarget))
        call system (mkdirCmd//trim(dirXML)//UPDATES//trim(pathToTarget))
    end if
    call system (mkdirCmd//trim(dirXML)//'substitutions')
    call system (mkdirCmd//trim(dirXML)//'transcripts')
    call system (mkdirCmd//trim(dirXML)//'edited-checklists')

    ! backup directories
    dirBak                 = dirHEEDS//'bak'//DIRSEP//trim(UniversityCode)//DIRSEP
    call system (mkdirCmd//trim(dirBak)//trim(pathToCurrent) )
    call system (mkdirCmd//trim(dirBak)//UPDATES//trim(pathToCurrent) )
    if (currentTerm/=targetTerm) then
        call system (mkdirCmd//trim(dirBak)//trim(pathToTarget) )
        call system (mkdirCmd//trim(dirBak)//UPDATES//trim(pathToTarget) )
    end if
    call system (mkdirCmd//trim(dirBak)//'substitutions')
    call system (mkdirCmd//trim(dirBak)//'transcripts')
    call system (mkdirCmd//trim(dirBak)//'edited-checklists')

    ! log file
    open(unit=stderr, file=trim(dirLog)//trim(fileExecutable)//'.log', status='unknown')
    write(stderr,AFORMAT) '-------', 'Begins '//currentDate//DASH//currentTime, '-------', &
        'Executable is : '//trim(fileExecutable)//SPACE//DASH//SPACE//PROGNAME//VERSION

    ! read the university name
    call read_university(pathToCurrent, errNo)
    if (errNo/=0) then ! something wrong
        call server_end('Error in reading university info')
    end if
    call file_log_message(UniversityName)

    ! read the colleges
    call read_colleges(pathToCurrent, errNo)
    if (errNo/=0) then ! something wrong
        call server_end('Error in reading the list of colleges')
    end if
    call file_log_message('# colleges ='//itoa(NumColleges))

    ! read the departments
    call read_departments(pathToCurrent, errNo)
    if (errNo/=0) then ! something wrong
        call server_end('Error in reading the list of departments')
    end if
    call file_log_message('# departments ='//itoa(NumDepartments))

    ! read the subjects
    call read_subjects(pathToCurrent, errNo)
    if (errNo/=0) then ! something wrong
        call server_end('Error in reading the list of subjects')
    end if
    call file_log_message('# subjects ='//itoa(NumSubjects))

    ! read the curricular programs
    call read_curricula(pathToCurrent, errNo)
    if (errNo/=0) then ! something wrong
        call server_end('Error in reading the list of curricular programs')
    end if
    call file_log_message('# curricular programs ='//itoa(NumCurricula))

    ! initialize student structure
    call initialize_student(Student(0))
    Student = Student(0)
    NumStudents = 0
    do iTmp=1,MAX_ALL_STUDENTS
        StdRank(iTmp) = iTmp
    end do

    if (trim(argString)=='CHECKLISTS') then
        call extract_student_grades()
        write(*,*) 'Writing student records...'
        do iTmp=1,NumStudents
            call xml_write_student_grades(iTmp)
            if (mod(iTmp,1000)==0) write(*,*) iTmp,  ' done...'
        end do
        call server_end(trim(fileExecutable)//space//trim(argString)//'-mode is complete.')
    end if

#if defined UPLB
    ! no need to reset term offered of a subject
#else
    ! set term offered of a subject to when it is taken in curricular programs
    call set_term_offered_accg_to_curricula()
    call file_log_message('Resetting subject term of offering to when taken in curricular programs...')
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

    ! read the rooms
    call read_rooms(pathToCurrent, errNo)
    if (errNo/=0) then ! something wrong
        call server_end('Error in reading the list of rooms')
    end if
    call file_log_message('# rooms ='//itoa(NumRooms))

    ! read the teachers
    call read_teachers(pathToCurrent, errNo)
    if (errNo/=0) then ! something wrong
        call server_end('Error in reading the list of teachers')
    end if
    call file_log_message('# teachers ='//itoa(NumTeachers))

    ! mark colleges with subject or curriculum information
    do targetDepartment=1,NumDepartments
        iTmp = Department(targetDepartment)%CollegeIdx
        College(iTmp)%hasInfo = College(iTmp)%hasInfo .or. Department(targetDepartment)%hasInfo
    end do

    ! read the classes
    call read_classes(pathToCurrent, NumCurrentSections, CurrentSection, CurrentOffering, errNo)
    UseCurrentCLASSES = NumCurrentSections>0
    call file_log_message('# sections for current term ='//itoa(NumCurrentSections))

    ! read the list of students
    call read_students(pathToCurrent, errNo)
    call file_log_message('# students ='//itoa(NumStudents))

    ! initialize enlisted subjects of students
    call initialize_pre_enlistment(Preenlisted(0))
    Preenlisted = Preenlisted(0)

    ! scheduling-mode?
    if (trim(argString)=='SCHEDULE') then
        call generate_initial_schedules(idxGrp, maxAlternates)
        call server_end(trim(fileExecutable)//space//trim(argString)//'-mode is complete.')
    end if

    ! read the blocks
    call read_blocks(pathToCurrent, NumCurrentBlocks, CurrentBlock, NumCurrentSections, CurrentSection, errNo)
    call file_log_message('# blocks for current term ='//itoa(NumCurrentBlocks))

    ! get no. of sections by dept
    call count_sections_by_dept(currentTerm, NumCurrentSections, CurrentSection)

    ! read ENLISTMENT files
    NumEnlistmentRecords = 0
    call read_pre_enlistment(pathToCurrent, 'ENLISTMENT', 0, 6, &
        NumCurrentSections, CurrentSection, Preenlisted, NumEnlistmentRecords, errNo)
    call file_log_message('# enlistment records ='//itoa(NumEnlistmentRecords))

    call recalculate_available_seats(CurrentSection)

    ! for next term
    call initialize_section(NextSection(0))
    NextSection = NextSection(0)
    NumNextSections = 0
    UseNextCLASSES = .false.

    NextOffering = TYPE_OFFERED_SUBJECTS (0, 0, 0, 0, 0, 0, 0, 0)

    call initialize_pre_enlistment(Advised(0))
    Advised = Advised(0)
    NumPredictionRecords = 0

    call initialize_waiver(WaiverCOI(0))
    WaiverCOI = WaiverCOI(0)
    NumWaiverRecords = 0

    if (Period>1) then

        ! read the intake file
        NFintake = 0
        call xml_read_intake(pathToTarget, errNo) ! try the XML file
        if (errNo/=0) then ! something wrong with XML file
            NFintake(1:NumCurricula-1) = 1
            call xml_write_intake(pathToTarget)
        end if
        call file_log_message('# incoming freshmen next term ='//trim(itoa(sum(NFintake)))//'?')

        ! read the classes
        call read_classes(pathToTarget, NumNextSections, NextSection, NextOffering, errNo)
        UseNextCLASSES = NumNextSections>0
        call file_log_message('# sections next term ='//itoa(NumNextSections))

        ! read the blocks
        call read_blocks(pathToTarget, NumNextBlocks, NextBlock, NumNextSections, NextSection, errNo)
        call file_log_message('# blocks next semester ='//itoa(NumNextBlocks))

        ! get no. of sections by deptCurrent
        call count_sections_by_dept(nextTerm, NumNextSections, NextSection)

        ! read the list of new students
        call read_students(pathToTarget, errNo)
        call file_log_message('# students, including new ones ='//itoa(NumStudents))

        ! retrieve predictions
        call read_predictions(pathToTarget, NumNextSections, NextSection, Advised, &
            NumPredictionRecords, errNo)
        call file_log_message('# prediction records for next term ='//itoa(NumPredictionRecords))

        call recalculate_available_seats(NextSection)

        call read_waivers(pathToTarget, NumNextSections, NextSection, NextOffering, NumWaiverRecords, errNo)
        call file_log_message('# waiver records for next term ='//itoa(NumWaiverRecords))

    end if

    ! create student directories
    itmp = 1
    do jtmp=2,len_trim(StdNoPrefix)
        if (StdNoPrefix(jtmp:jtmp)/=':') cycle
        call system (mkdirCmd//trim(dirUploadCHECKLISTS)//StdNoPrefix(itmp+1:jtmp-1) )
        call system (mkdirCmd//trim(dirUploadENLISTMENT)//StdNoPrefix(itmp+1:jtmp-1) )
        call system (mkdirCmd//trim(dirXML)//'substitutions'//DIRSEP//StdNoPrefix(itmp+1:jtmp-1) )
        call system (mkdirCmd//trim(dirXML)//'transcripts'//DIRSEP//StdNoPrefix(itmp+1:jtmp-1) )
        call system (mkdirCmd//trim(dirXML)//'edited-checklists'//DIRSEP//StdNoPrefix(itmp+1:jtmp-1) )
        call system (mkdirCmd//trim(dirBak)//'substitutions'//DIRSEP//StdNoPrefix(itmp+1:jtmp-1) )
        call system (mkdirCmd//trim(dirBak)//'transcripts'//DIRSEP//StdNoPrefix(itmp+1:jtmp-1) )
        call system (mkdirCmd//trim(dirBak)//'edited-checklists'//DIRSEP//StdNoPrefix(itmp+1:jtmp-1) )
        itmp = jtmp
    end do


    
    select case (trim(argString))

        case ('ADVISE')

            if (Period==1) then
                write(*,*)  'NEEDS ANALYSIS is not available during the enrollment period'

            else
                call advise_all_students()
                call xml_write_pre_enlistment(pathToTarget, 'PREDICTIONS', Advised, NextSection)

                ! invalidate pre-enlistment by moving ENLISTMENT files to backup
                dataSource = trim(dirXML)//trim(pathToTarget)//'ENLISTMENT'
                call move_to_backup(trim(dataSource)//'.XML')
                do iTmp=2,7
                    call move_to_backup(trim(dataSource)//DASH//trim(itoa(iTmp))//'.XML')
                end do

            end if

        case ('SERVER', 'TRAINING')

            ! everything is clean for now
            isDirtySTUDENTS = .false.

            ! set available functions
            call set_feature_availability()

            ! start of server loop
            call server_start()

            ! server loop has exited

        case ('RESETPASSWORDS')

            call reset_passwords()

        case default

    end select

    call server_end(trim(fileExecutable)//space//trim(argString)//'-mode is complete.')

end program MAIN
