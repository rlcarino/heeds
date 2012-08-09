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


program MAIN

    use SERVER

    implicit none

    integer :: active_users, itmp, errNo
    character (len=MAX_LEN_FILE_PATH) :: dataSource
    character (len=40) :: argString
    real :: harvest

    ! 4+ arguments ?
    iTmp = iargc()
    if (iTmp<4) then
        write(*,AFORMAT) 'Usage: '//PROGNAME//' univ year term period', &
            '  where', &
            'univ      - university code', &
            'year      - the year when current Academic Year started', &
            'term      - 1=first sem, 2=second sem', &
            'period    - 1=enrollment period, 2=mid-term, 3=end-of-term (grades are available)', &
            'action    - convert, checklists, advise, schedule, server'
        stop
    end if

    ! program starts
    call date_and_time (date=currentDate,time=currentTime)

    ! initialize random seed
    call initialize_random_seed()

    ! generate password for REGISTRAR
    call RANDOM_NUMBER(harvest)
    adminPassword = int(1.0E8*harvest)

    ! get arguments
    call getarg(1, UniversityCode)
    call getarg(2, argString)
    currentYear = atoi(argString)
    call getarg(3, argString)
    currentTerm = atoi(argString)
    call getarg(4, argString)
    Period = atoi(argString)

    if (iTmp>4) then
        call getarg(5, argString)
        call upper_case(argString)

        select case (trim(argString))
            case ('CHECKLISTS')
            case ('ADVISE')
            case ('SCHEDULE')
            case ('SERVER')
            case default
                argString = 'CONVERT'
        end select
    else
        argString = 'CONVERT'
    end if

    write(*,*) PROGNAME//VERSION//' started '//currentDate//dash//currentTime
    write(*,*) 'University, Current year, term, period, action : ', &
        trim(UniversityCode), currentYear, currentTerm, Period, trim(argString)

    if (currentTerm==1) then
        prevYearYear = currentYear-1
        prevYearTerm = 1
        prevTermYear = currentYear-1
        prevTermTerm = 2
        nextYear = currentYear
        nextTerm = 2
    else if (currentTerm==2) then
        prevYearYear = currentYear-1
        prevYearTerm = 2
        prevTermYear = currentYear
        prevTermTerm = 1
        nextYear = currentYear+1
        nextTerm = 1
    else
        write(*,*) 'Current term must be 1 (1st sem) or 2 (2nd sem)'
        stop
    end if

    ! predict for which term?
    if (Period==1) then ! enlistment period, forecasts are for current term
        targetYear = currentYear
        targetTerm = currentTerm
    else ! forecasts are for next term
        targetYear = nextYear
        targetTerm = nextTerm
    end if
    pathToCurrent = trim(itoa(currentYear))//DIRSEP//trim(txtSemester(currentTerm))//DIRSEP
    pathToTarget = trim(itoa(targetYear))//DIRSEP//trim(txtSemester(targetTerm))//DIRSEP

    ! fixed directories
    dirWWW                 = dirHEEDS//'web'//DIRSEP ! DocumentRoot in Apache
    dirCGI                 = dirHEEDS//'cgi'//DIRSEP ! Alias to /cgi-bin in Apache
    dirTmp                 = dirHEEDS//'tmp'//DIRSEP ! where the CGI script communicates with HEEDS
    dirBak                 = dirHEEDS//'bak'//DIRSEP ! for backup files
    dirLog                 = dirHEEDS//'log'//DIRSEP ! for log files
    dirUploadCHECKLISTS    = dirHEEDS//'web'//DIRSEP//'static'//DIRSEP//'upload-checklists'//DIRSEP ! individual checklists for upload
    dirUploadENLISTMENT    = dirHEEDS//'web'//DIRSEP//'static'//DIRSEP//'upload-enlistment'//DIRSEP ! enlisted classes by student

    call system (mkdirCmd//trim(dirLog))
    call system (mkdirCmd//trim(dirUploadCHECKLISTS))
    call system (mkdirCmd//trim(dirUploadENLISTMENT))

    ! directories for raw input data
    dirRAW    = dirHEEDS//'raw'//DIRSEP//trim(UniversityCode)//DIRSEP

    ! directories for XML input/ouput data
    dirXML                 = dirHEEDS//'xml'//DIRSEP//trim(UniversityCode)//DIRSEP !//currentDate//dash//currentTime(:6)//DIRSEP
    dirSUBSTITUTIONS          = trim(dirXML)//'substitutions'//DIRSEP ! directory for input/UNEDITED checklists from Registrar
    dirTRANSCRIPTS         = trim(dirXML)//'transcripts'//DIRSEP ! directory for raw transcripts
    dirEditedCHECKLISTS    = trim(dirXML)//'edited-checklists'//DIRSEP ! for output/EDITED checklists from College Secretaries

    call system (mkdirCmd//trim(dirXML)//trim(pathToCurrent))
    if (currentTerm/=targetTerm) call system (mkdirCmd//trim(dirXML)//trim(pathToTarget))
    call system (mkdirCmd//trim(dirSUBSTITUTIONS))
    call system (mkdirCmd//trim(dirTRANSCRIPTS))
    call system (mkdirCmd//trim(dirEditedCHECKLISTS))

    ! backup directories
    dataSource = trim(dirBak)//trim(UniversityCode)//DIRSEP ! //currentDate//dash//currentTime(:6)//DIRSEP
    call system (mkdirCmd//trim(dataSource)//trim(pathToCurrent) )
    if (currentTerm/=targetTerm) call system (mkdirCmd//trim(dataSource)//trim(pathToTarget) )

    ! delete lock files from requests received while application was not running
    call system(delCmd//trim(dirTmp)//'*.lock', errNo)

    if (MANY_LOG_FILES) then
        open(unit=stderr, file=trim(dirLOG)//currentDate//dash//currentTime(:7)//'log', status='replace')
    else
        open(unit=stderr, file=trim(dirLOG)//currentDate//'.log', status='replace')
    end if
    write(stderr,AFORMAT) '-------', 'Begins '//currentDate//dash//currentTime, '-------'

    ! read the university name
    call read_university(pathToCurrent, errNo)
    if (errNo/=0) then ! something wrong
        call server_end('Error in reading university info')
    end if
    write(*,*) UniversityName

    ! read the colleges
    call read_colleges(pathToCurrent, errNo)
    if (errNo/=0) then ! something wrong
        call server_end('Error in reading the list of colleges')
    end if
    write(*,*) NumColleges, ' colleges'

    ! read the departments
    call read_departments(pathToCurrent, errNo)
    if (errNo/=0) then ! something wrong
        call server_end('Error in reading the list of departments')
    end if
    write(*,*)  NumDepartments,  ' departments'

    ! read the subjects
    call read_subjects(pathToCurrent, errNo)
    if (errNo/=0) then ! something wrong
        call server_end('Error in reading the list of subjects')
    end if
    write(*,*)  NumSubjects,  ' subjects'

    ! read the curricular programs
    call read_curricula(pathToCurrent, errNo)
    if (errNo/=0) then ! something wrong
        call server_end('Error in reading the list of curricular programs')
    end if
    write(*,*)  NumCurricula, ' curricular programs'

    ! initialize NumStudents, student ranks
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
        write(*,*) 'All done!'
        !argString = 'SERVER'
        call server_end(' Stopped')
    end if

    select case (trim(UniversityCode))
        case ('CSU-Andrews', 'ISU') ! Subjects administered by program
              ! set term offered of a subject to when it is taken in curricula programs
            call set_term_offered_accg_to_curricula()
        case default ! Subject administered by departments
    end select

    ! Synchronize pre-requisites of co-requisite subjects
    ! For example, CHEM 17.0 has MATH 11 or MATH 17, CHEM 17.1 has NONE. Set
    ! pre-requisite of CHEM 17.1 to that of CHEM 17
    write(*,*) 'Synchronizing pre-requisites of co-requisite subjects...'
    do targetSubject=1,NumSubjects
        if (Subject(Subject(targetSubject)%Prerequisite(1))%Name/='NONE') cycle ! pre-requisite is NONE
        if (Subject(targetSubject)%lenCoreq/=1) cycle ! should be one token only
        itmp = Subject(targetSubject)%Corequisite(1)
        if (itmp<=0) cycle ! token should be a named subject
        write(*,*) Subject(targetSubject)%Name//'has co-requisite '//Subject(itmp)%Name
        ! pre-requisite is NONE, co-requisite is a named subject
        Subject(targetSubject)%lenPreq = Subject(itmp)%lenPreq
        Subject(targetSubject)%Prerequisite = Subject(itmp)%Prerequisite
    end do

    ! read the rooms
    call read_rooms(pathToCurrent, errNo)
    if (errNo/=0) then ! something wrong
        call server_end('Error in reading the list of rooms')
    end if
    write(*,*)  NumRooms, ' rooms'

    ! read the teachers
    call read_teachers(pathToCurrent, errNo)
    if (errNo/=0) then ! something wrong
        call server_end('Error in reading the list of teachers')
    end if
    write(*,*)  NumTeachers, ' teachers'

    ! mark colleges with some information
    do targetDepartment=1,NumDepartments
        iTmp = Department(targetDepartment)%CollegeIdx
        College(iTmp)%hasInfo = College(iTmp)%hasInfo .or. Department(targetDepartment)%hasInfo
    end do

    ! read the classes
    call read_classes(pathToCurrent, NumCurrentSections, CurrentSection, CurrentOffering, errNo)
    UseCurrentCLASSES = NumCurrentSections>0
    write(*,*)  NumCurrentSections, ' sections for current term'

    ! read the blocks
    call read_blocks(pathToCurrent, NumCurrentBlocks, CurrentBlock, NumCurrentSections, CurrentSection, errNo)
    write(*,*)  NumCurrentBlocks, ' blocks for current term'

    ! get no. of sections by dept
    call count_sections_by_dept(currentTerm, NumCurrentSections, CurrentSection)

    ! read the list of students
    call read_students(pathToCurrent, errNo)
    write(*,*)  NumStudents, ' students for current term'

    ! read currently enlisted subjects of students
    call initialize_pre_enlistment(Preenlisted(0))
    Preenlisted = Preenlisted(0)
    NumEnlistmentRecords = 0
    call read_pre_enlistment(trim(pathToCurrent)//'ENLISTMENT', NumCurrentSections, CurrentSection, Preenlisted, &
        NumEnlistmentRecords, errNo)
    write(*,*)  NumEnlistmentRecords,    ' enlistment records'

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

    if (Period>1) then

        ! read the intake file
        NFintake = 0
        call xml_read_intake(pathToTarget, errNo) ! try the XML file
        if (errNo/=0) then ! something wrong with XML file
            NFintake(1:NumCurricula-1) = 1
            call xml_write_intake(pathToTarget)
        end if
        write(*,*)  sum(NFintake),    ' incoming freshmen next term?'

        ! read the classes
        call read_classes(pathToTarget, NumNextSections, NextSection, NextOffering, errNo)
        UseNextCLASSES = NumNextSections>0
        write(*,*)  NumNextSections,    ' sections next term'

        ! read the blocks
        call read_blocks(pathToTarget, NumNextBlocks, NextBlock, NumNextSections, NextSection, errNo)
        write(*,*)  NumNextBlocks,    ' blocks next semester'

        ! get no. of sections by deptCurrent
        call count_sections_by_dept(nextTerm, NumNextSections, NextSection)

    end if

    select case (trim(argString))

        case ('CONVERT')

        case ('ADVISE')

        case ('SCHEDULE')

        case ('SERVER')

            call create_user_names()
            write(*,*)  NumUsers,  ' users'

            ! everything is clean for now
            isDirtySTUDENTS = .false.

            ! set available functions
            call set_feature_availability()

            ! make the login page
            call html_login(trim(dirWWW)//DIRSEP//'index.html')

            ! write the CGI script, change "\" to "/"
            QUERY_STRING = dirTmp
            do iTmp=1,len_trim(dirTmp)
                if (QUERY_STRING(iTmp:iTmp)==bslash) QUERY_STRING(iTmp:iTmp) = fslash
            end do
            call cgi_write_script(trim(dirCGI)//CGI_SCRIPT, trim(QUERY_STRING), '30') ! ('NN' seconds to timeout)

            ! invalidate previous requests
            call server_check_mailbox(1,active_users)
            write(*,*) active_users, 'stale requests...'

            write(*,AFORMAT) 'The '//PROGNAME//' back-end program is ready.', &
                'The CGI script is '//CGI_PATH, &
                'Temporary HTML files will be in '//trim(dirTmp), &
                'No. of predefined users is '//itoa(NumUsers), &
                'The administrative password is '//itoa(adminPassword)

            ! start of server loop
            call server_start()

            ! server loop has exited; remove CGI script
            call unlink(trim(dirCGI)//CGI_SCRIPT)

        case default

    end select

    call server_end(' Stopped')

end program MAIN
