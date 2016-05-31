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


program STATIC

    use INITIALIZE
    use XMLIO

    implicit none

    ! private tokens
    character (len=MAX_LEN_FILE_PATH) :: fileName
    character (len=MAX_LEN_XML_LINE) :: line, value
    character(len=MAX_LEN_XML_TAG) :: tag
    integer :: eof, ndels, pos(60)
    character (len=MAX_LEN_PASSWD_VAR) :: Password
    logical :: pathExists

    ! static initializations
    call get_arguments()

    ! compute relative years and terms (before and after current)
    call initialize_past_future_years_terms (currentYear, currentTerm)

    ! reset basic data
    call year_data_initialize ()

    isReadOnly = .false.
    isServer = .false.

    select case (trim(ACTION))

        case ('Convert') ! convert old format BACKUP.XML to new format
            fileName = trim(pathToYear)//'BACKUP.XML'
            call year_data_read_from_old_backup(fileName, eof)
            isDirtyData = .true.
            !call backup_write()
            call year_data_write()
            call move_to_backup(fileName)

        case ('Restore') ! restore data from BACKUP
            call year_data_read(trim(pathToYear)//trim(UniversityCode)//'-BACKUP.XML')
            call year_data_write()

        case ('Passwords') ! show passwords for SYSADs
            inquire(file=trim(dirTEACHERS)//'index', exist=pathExists)
            if ( pathExists ) then
                call university_data_read(trim(pathToYear)//'UNIVERSITY.XML')
                call departments_index_read(unitIDX, dirDEPARTMENTS)
                call teachers_index_read(unitIDX, dirTEACHERS)
            else
                fileName = trim(pathToYear)//'BACKUP.XML'
                inquire(file=fileName, exist=pathExists)
                if (pathExists) then ! last run terminated normally; read from backup
                    call university_data_read(fileName)
                    call department_details_read(fileName)
                    call teacher_details_read(fileName)
                end if
            end if
            write(*,*) 'Non-student users = ', NumTeachers
            do eof=1,NumTeachers
                if (Teacher(eof)%Role/=SYSAD) cycle
                call getPassword_of_teacher(eof, Password)
                write(*,*) 'Password for '//trim(Teacher(eof)%TeacherId)//' : '//Password
            end do

        case ('Checklists')
!                fileName = trim(dirDATA)//'transcripts'
!                inquire(file=trim(fileName), exist=pathExists)
!                if (pathExists) then ! rename
!    !#if defined GLNX
!    !                line = 'tar -czf '//trim(fileName)//DASH//currentDate//DASH//currentTime//'.tar.gz '//fileName
!    !                call system(trim(line), eof)
!    !                if (eof/=0) call log_comment(itoa(eof)//' returned by: '//trim(line))
!    !#else
!                    line = trim(fileName)//DASH//currentDate//DASH//currentTime
!                    call rename (trim(fileName), trim(line), eof)
!                    if (eof/=0) call log_comment('Status='//trim(itoa(eof))//' in moving to '//trim(line) )
!    !#endif
!                end if
            call generate_checklists()


        case ('Payments')
            fileName = trim(dirDATA)//'payments'
            inquire(file=trim(fileName), exist=pathExists)
            if (pathExists) then ! rename
                line = trim(fileName)//DASH//currentDate//DASH//currentTime
#if defined GLNX
                call rename (trim(fileName), trim(line), eof)
!#else
!                call system(mvCmd//trim(fileName)//SPACE//trim(line), eof)
#endif
                if (eof/=0) call log_comment('Status='//trim(itoa(eof))//' in moving to '//trim(line) )
            end if
            call generate_payments()


        case ('Assessments')
            fileName = trim(dirDATA)//'assessments'
            inquire(file=trim(fileName), exist=pathExists)
            if (pathExists) then ! rename
                line = trim(fileName)//DASH//currentDate//DASH//currentTime
!#if defined GLNX
                call rename (trim(fileName), trim(line), eof)
!#else
!                call system(mvCmd//trim(fileName)//SPACE//trim(line), eof)
!#endif
                if (eof/=0) call log_comment('Status='//trim(itoa(eof))//' in moving to '//trim(line) )
            end if
            call generate_assessments()


        case ('Import')
            call import_custom_csv()
            call year_data_write()


        case ('Special')

            ! read data for university
            call year_data_read(pathToYear)

            dirSTUDENTINFO      = trim(dirDATA)//'info'//DIRSEP ! directory for individual student info
            call custom_read_gender(pathToYear)

!            call generate_student_yearly_record()

            ! read CSU-Gonzaga students from XML mysql-export
            !call read_stud_profile()
            !call read_stud_grade()

            !call import_custom_students_csv()

            ! read enlistment files, if any
            !call merge_enlistment()

            !call import_ASC()

        case default

    end select
    call terminate(-1,trim(fileEXE)//SPACE//trim(ACTION)//' completed normally.')


contains

#include "Input.F90"


    subroutine custom_read_gender(path)

        character(len=*), intent(in) :: path
        integer :: errNo, loc, iStd
        character (len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character :: cGender


        fileName = trim(path)//'GENDER.CSV'
        open (unit=unitRAW, file=fileName, status='old', iostat=errNo)
        if (errNo/=0) return

        call log_comment('Retrieving gender data from '//fileName)
        ! skip first line
        read(unitRAW, AFORMAT) line

        do
            read(unitRAW, AFORMAT, iostat=eof) line

            if (eof<0) exit
            if (line==SPACE .or. line(1:1)=='#') cycle
            loc = index(line, COMMA)
            if (loc==0) cycle
            cGender = line(loc+1:)
            if (cGender==SPACE) cycle
            tStdno = line(:loc-1)
            iStd = index_to_student(tStdNo)
            if (iStd==0) cycle
            Student(iStd)%Gender = cGender
            call student_details_write(unitXML, dirSTUDENTS, iStd)

            call xml_read_student_info(iStd, errNo)
            if (cGender==StudentInfo%Gender) cycle

            StudentInfo%Gender = cGender
            call xml_student_info(iStd)

        end do

        close(unitRAW)

    end subroutine custom_read_gender


    subroutine year_data_read_from_old_backup(fileName, errNo)

        character (len=*), intent(in) :: fileName
        integer, intent (out) :: errNo

        integer :: jTmp, iTmp

        errNo = 0

        ! university-level data
        call university_data_read(fileName)
        call log_comment(trim(UniversityName)//' @ '//UniversityAddress)

        ! account codes
        if (NumAccounts==0) call xml_read_accounts(fileName)
        call log_comment(itoa(NumAccounts)//' account codes')

        ! the colleges
        call college_details_read(fileName)
        call log_comment(itoa(NumColleges)//' colleges')
        ! add 'administrative' college for data that does not fit in the 'academic' colleges
        NumColleges = NumColleges + 1
        call check_array_bound (NumColleges, MAX_ALL_COLLEGES, 'MAX_ALL_COLLEGES')
        call initialize_college (College(NumColleges), &
            SYSAD, trim(UniversityCode)//' Administration', SYSAD, SPACE, SPACE)

        ! the departments
        call department_details_read(fileName)
        call log_comment(itoa(NumDepartments-1)//' departments')
        ! add REGISTAR as 'administrative' department for data that does not fit in the 'academic' departments
        NumDepartments = NumDepartments + 1
        call check_array_bound (NumDepartments, MAX_ALL_DEPARTMENTS, 'MAX_ALL_DEPARTMENTS')
        call initialize_department (Department(NumDepartments), &
            SYSAD, trim(UniversityCode)//' Registrar', TheRegistrar, 'Z', NumColleges)

        ! the subjects
        call subject_codes_read(fileName)
        call subject_details_read(fileName)
        call log_comment (itoa(NumSubjects)//FSLASH//itoa(-NumDummySubjects)//' "regular"/"dummy" subjects')
        call get_subject_areas()

        ! the curricular programs
        call curriculum_details_read(fileName)
        call log_comment (itoa(NumCurricula)//' curricula')
        ! add OTHER
        NumCurricula = NumCurricula + 1
        call check_array_bound (NumCurricula, MAX_ALL_CURRICULA, 'MAX_ALL_CURRICULA')
        Curriculum(NumCurricula) = Curriculum(0)
        Curriculum(NumCurricula)%Code = 'OTHER'
        Curriculum(NumCurricula)%CollegeIdx = NumColleges
        Curriculum(NumCurricula)%Title = 'Change curriculum'
        Curriculum(NumCurricula)%Specialization = SPACE
        Curriculum(NumCurricula)%Remark = SPACE
        Curriculum(NumCurricula)%NSubjects = 0
        Curriculum(NumCurricula)%Active = .true.

        ! the substitution rules
        iTmp = 0
        call equivalencies_by_curriculum(fileName, iTmp)
        call equivalencies_for_all_curricula(fileName, iTmp)
        SubstIdx(NumSubst+1) = iTmp+1

        call log_comment('EQUIVALENCIES')
        do iTmp=1,NumSubst
            if (Substitution(SubstIdx(iTmp))>0 .and. Substitution(SubstIdx(iTmp))<=NumCurricula) then
                line = trim(Curriculum(Substitution(SubstIdx(iTmp)))%Code)//' : '
            elseif (Substitution(SubstIdx(iTmp))==-1) then
                line = 'All curricula : '
            end if
            do jTmp=SubstIdx(iTmp)+1, SubstIdx(iTmp+1)-1
                line = trim(line)//SPACE//trim(Subject(Substitution(jTmp))%Name)//SPACE
            end do
            call log_comment(itoa(iTmp)//line)
        end do

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

        ! the rooms
        call room_details_read(fileName)
        call log_comment (itoa(NumRooms)//' rooms')
        if (NumRooms==0) then
            do iTmp=2,NumDepartments-1 ! create rooms for each department
                NumRooms = NumRooms+1
                call initialize_room(Room(NumRooms), trim(Department(iTmp)%Code)//' Room', &
                    iTmp, 0, 0)
            end do
        end if

        ! the teachers
        call teacher_details_read(fileName)
        call log_comment (itoa(NumTeachers)//' teachers')
        Teacher(1)%DeptIdx = NumDepartments ! Guest's unit is not previously set
        if (NumTeachers==1) then ! 1=Guest only

            do iTmp=2,NumDepartments-1 ! create teacher for each department
                NumTeachers = NumTeachers+1
                call check_array_bound (NumTeachers, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')
                Teacher(NumTeachers)%TeacherID = trim(Department(iTmp)%Code)//'-Teacher'
                Teacher(NumTeachers)%DeptIdx = iTmp
                Teacher(NumTeachers)%Role = GUEST
                Teacher(NumTeachers)%Name = trim(Department(iTmp)%Code)//' Teacher'
                Teacher(NumTeachers)%MaxLoad = 0
                Teacher(NumTeachers)%Specialization = 'Teaching'
                call set_password(Teacher(NumTeachers)%Password, trim(Teacher(NumTeachers)%TeacherID))

            end do
            errNo = 0

            ! the Developer account
            NumTeachers = NumTeachers+1
            call check_array_bound (NumTeachers, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')
            Teacher(NumTeachers)%TeacherID = PROGNAME
            Teacher(NumTeachers)%DeptIdx = NumDepartments
            Teacher(NumTeachers)%Role = SYSAD
            Teacher(NumTeachers)%Name = PROGNAME//' Developer'
            Teacher(NumTeachers)%MaxLoad = 0
            Teacher(NumTeachers)%Specialization = 'HEEDS Development'
            call set_password(Teacher(NumTeachers)%Password)

            ! the Administrator
            NumTeachers = NumTeachers+1
            call check_array_bound (NumTeachers, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')
            Teacher(NumTeachers)%TeacherID = SYSAD
            Teacher(NumTeachers)%DeptIdx = NumDepartments
            Teacher(NumTeachers)%Role = SYSAD
            Teacher(NumTeachers)%Name = PROGNAME//' Administrator'
            Teacher(NumTeachers)%MaxLoad = 0
            Teacher(NumTeachers)%Specialization = 'HEEDS Administration'
            call set_password(Teacher(NumTeachers)%Password)

        end if

        ! add BENEFACTORS not in TEACHERS.XML
        do iTmp=1,MAX_ALL_SCHOLARSHIPS

            if (len_trim(ScholarshipCode(iTmp))==0) cycle
            jTmp = index_to_teacher(ScholarshipCode(iTmp))
            if (jTmp>0) cycle ! already in file

            NumTeachers = NumTeachers+1
            call check_array_bound (NumTeachers, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')
            Teacher(NumTeachers)%TeacherID = ScholarshipCode(iTmp)
            Teacher(NumTeachers)%DeptIdx = NumDepartments
            Teacher(NumTeachers)%Role = BENEFACTOR
            Teacher(NumTeachers)%Name = ScholarshipDescription(iTmp)
            Teacher(NumTeachers)%MaxLoad = 0
            Teacher(NumTeachers)%Specialization = 'Benefactor'
            call set_password(Teacher(NumTeachers)%Password, Teacher(NumTeachers)%TeacherID)

        end do

        call sort_teachers()
        call sort_alphabetical_teachers()

        ! mark colleges with subject or curriculum information
        do jTmp=1,NumDepartments
            iTmp = Department(jTmp)%CollegeIdx
            College(iTmp)%hasInfo = College(iTmp)%hasInfo .or. Department(jTmp)%hasInfo
        end do

        ! the students
        call student_details_read(fileName, iTmp)
        call sort_alphabetical_students()

        ! schedules, blocks, enlistment
        do jTmp=firstSemester,summerTerm

            ! the classes
            call class_details_read(fileName, jTmp)
            call log_comment (itoa(NumSections(jTmp))//' sections for '//trim(text_term_school_year(jTmp, currentYear)) )
            ! sort & summarize
            call offerings_sort(jTmp)
            call offerings_summarize(jTmp, 0)

            ! the blocks
            call block_details_read(fileName, jTmp)
            ! compress block array
            iTmp = 0
            do eof=1,NumBlocks(jTmp)
                !write(*,*) eof, Block(eof)%BlockID, Block(eof)%CurriculumIdx
                if (Block(jTmp,eof)%CurriculumIdx/=0) then
                    iTmp = iTmp+1
                    Block(jTmp,iTmp) = Block(jTmp,eof)
                end if
            end do
            NumBlocks(jTmp) = iTmp
            call log_comment (itoa(NumBlocks(jTmp))//' blocks for '//trim(text_term_school_year(jTmp, currentYear)) )

            ! count no. of sections by dept
            call count_sections_by_dept(jTmp)

            ! enlistment
            call xml_read_pre_enlistment(fileName, jTmp, iTmp)

            call recalculate_available_seats(jTmp)
        end do

        ! remember clock tick when data is retrieved
        CALL SYSTEM_CLOCK(tickLastRefresh, count_rate, count_max)
        tickLastBackup = tickLastRefresh

    end subroutine year_data_read_from_old_backup


    subroutine xml_read_accounts(pathToFile)

        character(len=*), intent(in) :: pathToFile

        integer :: loc, stat

        NumAccounts = 0

        ! open file, return on any error
        call xml_read_file(unitXML, ROOT_ACCOUNTS, pathToFile, stat)
        call terminate(stat, 'Error in reading accounting codes from '//pathToFile)

        ! examine the file line by line
        do
            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit

            select case (trim(tag))

                case (FSLASH//ROOT_ACCOUNTS)
                    exit

                case ('Account', 'ACCOUNT')
                     loc = index(value, COMMA)
                     NumAccounts = NumAccounts + 1
                     AccountCode(NumAccounts) = value(1:loc-1)
                     AccountDescription(NumAccounts) = value(loc+1:)

                case default ! do nothing

            end select

        end do

        close(unitXML)

    end subroutine xml_read_accounts



    subroutine xml_read_pre_enlistment(fileName, thisTerm, errNo)

        character(len=*), intent(in) :: fileName
        integer, intent (in) :: thisTerm
        integer, intent (out) :: errNo

        type(TYPE_PRE_ENLISTMENT) :: wrk
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_CLASS_ID) :: tClass
        character (len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character (len = MAX_LEN_TEXT_GRADE) :: tGrade
        integer :: iSubj, idx, iSect, iStd,gdx, pos, i, nGraded, stat
        logical :: checkRegular

        ! open file, return on any error
        tag = ROOT_ENLISTMENT//trim(txtSemester(thisTerm))
        call xml_read_file(unitXML, trim(tag), fileName, errNo)
        if (errNo/=0) then
            call log_comment ('Warning: "'//trim(tag)//'" not found in '//fileName)
            return
        end if

        ! examine the file line by line
        NumEnlistment(thisTerm) = 0
        do
            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit

            select case (trim(tag))

                case ('Student') ! initialize temporary college data
                    call initialize_pre_enlistment (wrk)
                    nGraded = 0
                    checkRegular = .false.

                case ('StdNo')
                    tStdNo = adjustl(value)
                    iStd = index_to_student(tStdNo)

                case ('UnitsEarned')
                    wrk%UnitsEarned = atof(value)

                case ('levelYear','StdYear')
                    wrk%levelYear = atoi(value)

                case ('levelClassification', 'StdClassification')
                    wrk%levelClassification = atoi(value)

                case ('AllowedLoad')
                    wrk%AllowedLoad = atof(value)

                case ('levelPriority', 'StdPriority')
                    wrk%levelPriority = atoi(value)

                case ('NPriority')
                    wrk%NPriority = atoi(value)

                case ('NAlternates')
                    wrk%NAlternates = atoi(value)

                case ('NCurrent')
                    wrk%NCurrent = atoi(value)

                case ('MissingPOCW')
                    wrk%MissingPOCW = atoi(value)

                case ('codeConditional')
                    wrk%codeConditional = atoi(value)

                case ('codeNSTP')
                    wrk%codeNSTP = atoi(value)

                case ('statusAdvising')
                    wrk%statusAdvising = atoi(value)

                case ('Status')
                    i = atoi(value)
                    select case (i)
                        !        Status=-3          : NAdv
                        case (-3)
                            wrk%statusAdvising = ADVISING_NEEDED
                        !        Status=-2          : Done
                        case (-2)
                            wrk%statusAdvising = ADVISING_FINISHED
                        !        Status=-1          : Excl
                        case (-1)
                            wrk%statusAdvising = ADVISING_EXCLUDED
                        !        Status=0          : NSub
                        case (0)
                            wrk%statusAdvising = ADVISING_NO_SUBJECTS
                        !        Status=1          : AAdv
                        case (1)
                            wrk%statusAdvising = ADVISING_IRREGULAR ! check later
                        !        Status=2          : MAdv
                        case (2)
                            wrk%statusAdvising = ADVISING_IRREGULAR
                        !        Status=3          : AEnl
                        case (3)
                            wrk%statusEnlistment = ENLISTMENT_AUTOMATIC
                        !        Status=4          : MEnl
                        case (4)
                            wrk%statusEnlistment = ENLISTMENT_MANUAL
                        !        Status=5          : Cfrm
                        case (5)
                            wrk%statusEnlistment = ENLISTMENT_CONFIRMED
                        !        Status=6          : Lckd
                        case (6)
                            wrk%statusEnlistment = ENLISTMENT_LOCKED
                        !        Status=7          : Paid
                        case (7)
                            wrk%statusEnlistment = ENLISTMENT_PAID

                    end select
                    checkRegular = .true.

                case ('statusEnlistment')
                    wrk%statusEnlistment = atoi(value)

                case ('statusScholastic', 'ScholasticStatus')
                    wrk%statusScholastic = atoi(value)

                case ('Predicted')
                    idx = index(value, COMMA)
                    tSubject = adjustl(value(:idx-1))
                    call upper_case(tSubject)
                    iSubj = index_to_subject(tSubject)
                    if (iSubj<=0) then ! subject code not found
                        call log_comment (tStdNo//': no such subject; ignored - '//line)
                        cycle
                    end if
                    ! check if already among previous entries
                    pos = 0
                    do i=1,wrk%lenSubject
                        if (wrk%Subject(i)==iSubj) pos = i
                    end do
                    if (pos==0) then
                        pos = wrk%lenSubject + 1
                        call check_array_bound (pos, MAX_SUBJECTS_PER_TERM, 'MAX_SUBJECTS_PER_TERM')
                        wrk%lenSubject = pos
                    else
                        call log_comment('Using duplicate predicted entry:  '//trim(tStdNo)//DASH//trim(line))
                    end if
                    ! add as predicted subject
                    wrk%Subject(pos) = iSubj
                    wrk%Contrib(pos) = atof( trim( value(idx+1:) ) )

                case ('Allowed')
                    tSubject = adjustl(value)
                    call upper_case(tSubject)
                    iSubj = index_to_subject(tSubject)
                    if (iSubj<=0) then ! subject code not found
                        call log_comment (tStdNo//': no such subject; ignored - '//line)
                        cycle
                    end if
                    ! check if already among previous entries
                    pos = 0
                    do i=1,wrk%lenSubject
                        if (wrk%Subject(i)==iSubj) pos = i
                    end do
                    if (pos==0) then
                        pos = wrk%lenSubject + 1
                        call check_array_bound (pos, MAX_SUBJECTS_PER_TERM, 'MAX_SUBJECTS_PER_TERM')
                        wrk%lenSubject = pos
                        ! add as allowed subject
                        wrk%Subject(pos) = iSubj
                        wrk%Contrib(pos) = 0.0
                    else
                        call log_comment('Discarding duplicate allowed entry:  '//trim(tStdNo)//DASH//trim(line))
                    end if

                case ('Graded')
                    idx = index(value, COMMA)
                    tClass = adjustl(value(:idx-1))
                    tGrade = adjustl(value(idx+1:))
                    iSect = index_to_section(tClass, thisTerm)
                    gdx = index_to_grade(tGrade)
                    ! check if already among previous entries
                    pos = 0
                    do i=1,wrk%lenSubject
                        if (wrk%Subject(i)==Section(thisTerm,iSect)%SubjectIdx) pos = i
                    end do
                    if (pos==0) then
                        pos = wrk%lenSubject + 1
                        call check_array_bound (pos, MAX_SUBJECTS_PER_TERM, 'MAX_SUBJECTS_PER_TERM')
                        wrk%lenSubject = pos
                    else
                        call log_comment('Using duplicate graded entry:  '//trim(tStdNo)//DASH//trim(line))
                    end if
                    wrk%Section(pos) = iSect
                    wrk%Subject(pos) = Section(thisTerm,iSect)%SubjectIdx
                    wrk%Grade(pos) = gdx
                    wrk%Contrib(pos) = 1.0
                    nGraded = nGraded+1

                case ('/Student')

                    if (wrk%NPriority+wrk%NAlternates+wrk%NCurrent /= wrk%lenSubject) then
                        wrk%NPriority = wrk%lenSubject
                        wrk%NAlternates = 0
                        wrk%NCurrent = 0
                    end if
                    if (iStd/=0) then
                        ! check if manually enlisted without being advised
                        if (wrk%statusAdvising<ADVISING_REGULAR .and. sum(wrk%Section)>0) then
                            wrk%statusAdvising = ADVISING_IRREGULAR
                        end if
                        ! check is advised subjects are regular
                        if (checkRegular .and. wrk%statusAdvising==ADVISING_IRREGULAR) then
                            call check_for_regular_advised_subjects(Student(iStd)%CurriculumIdx, thisTerm, wrk)
                        end if
                        Student(iStd)%Enlistment(thisTerm) = wrk
                        NumEnlistment(thisTerm) = NumEnlistment(thisTerm)+1
                    else
                        call log_comment ('No such student; ignored - '//tStdNo)
                    end if

                case default
                    if ( trim(tag)==FSLASH//ROOT_ENLISTMENT//trim(txtSemester(thisTerm)) ) exit

            end select

        end do

        close(unitXML)
        !call log_comment (itoa(numEntries)//' pre-enlistment entries for '//trim(text_term_school_year(thisTerm, currentYear))// &
        !    ' in '//fileName)

    end subroutine xml_read_pre_enlistment


    subroutine year_data_write()

        integer :: jTmp, iTmp, idx

        if (isReadOnly) return

        ! the university-level data
        call university_data_write(unitXML, trim(pathToYear)//'UNIVERSITY.XML')

        ! the colleges
        do idx=1,NumColleges-1
            call college_details_write(unitXML, dirCOLLEGES, idx)
        end do
        call college_details_write(unitXML, trim(dirCOLLEGES)//'index', 1, NumColleges-1)

        ! the departments
        do idx=2,NumDepartments-1
            call department_details_write(unitXML, dirDEPARTMENTS, idx)
        end do
        call department_details_write(unitXML, trim(dirDEPARTMENTS)//'index', 2,NumDepartments-1)

        ! the subjects
        do idx=NumDummySubjects,NumSubjects+NumAdditionalSubjects
            call subject_details_write(unitXML, dirSUBJECTS, idx)
        end do
        call subject_details_write(unitXML, trim(dirSUBJECTS)//'index', NumDummySubjects, NumSubjects+NumAdditionalSubjects)

        ! the curricular programs
        do idx=1,NumCurricula-1
            call curriculum_details_write(unitXML, dirCURRICULA, idx)
        end do
        call curriculum_details_write(unitXML, trim(dirCURRICULA)//'index', 1, NumCurricula-1)

        ! the substitution rules
        call equivalence_data_write(unitXML, trim(pathToYear)//'EQUIVALENCIES.XML')

        ! the rooms
        do idx=1,NumRooms+NumAdditionalRooms
            call room_details_write(unitXML, dirROOMS, idx)
        end do
        call room_details_write(unitXML, trim(dirROOMS)//'index', 1, NumRooms+NumAdditionalRooms)

        ! the teachers
        do idx=1,NumTeachers+NumAdditionalTeachers
            call teacher_details_write(unitXML, dirTEACHERS, idx)
        end do
        call teacher_details_write(unitXML, trim(dirTEACHERS)//'index', 1, NumTeachers+NumAdditionalTeachers)

        ! the classes, blocks
        do jTmp=firstSemester,summerTerm

            ! the classes
            if (NumSections(jTmp)>0) then
                do idx=1,NumSections(jTmp)
                    call class_details_write(unitXML, jTmp, dirCLASSES(jTmp), idx)
                end do
                call class_details_write(unitXML, jTmp, indexCLASSES(jTmp), 1, NumSections(jTmp))
            end if

            ! the blocks
            if (NumBlocks(jTmp)>0) then
                do idx=1,NumBlocks(jTmp)
                    call block_details_write(unitXML, jTmp, dirBLOCKS(jTmp), idx)
                end do
                call block_details_write(unitXML, jTmp, indexBLOCKS(jTmp), 1, NumBlocks(jTmp))
            end if

        end do

        ! the students
        call collect_prefix_years()
        iTmp = 1
        do jTmp=2,len_trim(StdNoPrefix)
            if (StdNoPrefix(jTmp:jTmp)/=':') cycle
            if (StdNoPrefix(iTmp+1:jTmp-1)==SPACE) cycle
            call make_directory( trim(dirSTUDENTS)//StdNoPrefix(iTmp+1:jTmp-1) )
            call make_directory( trim(dirDATA)//trim(itoa(currentYear))//DIRSEP//'GRADES'//DIRSEP//StdNoPrefix(iTmp+1:jTmp-1) )
            iTmp = jTmp
        end do

        call student_details_write(unitXML, trim(dirSTUDENTS)//'index', 1, NumStudents+NumAdditionalStudents)
        do idx=1,NumStudents+NumAdditionalStudents
            call student_details_write(unitXML, dirSTUDENTS, idx)
            call finalgrades_write(currentYear, idx)
        end do

    end subroutine year_data_write


end program STATIC
