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


program STATIC

    use INITIALIZE
    use XMLIO

    implicit none

    ! private tokens
    character (len=MAX_LEN_FILE_PATH) :: fileName
    character (len=MAX_LEN_XML_LINE) :: line, value
    character(len=MAX_LEN_XML_TAG) :: tag
    integer :: eof, ndels, pos(60)
    logical :: pathExists

    ! static initializations
    call initializations()

    select case (trim(ACTION))

        case ('Restore')
            call restore_from_backup()


        case ('Checklists')
            fileName = trim(dirDATA)//'transcripts'
            inquire(file=trim(fileName), exist=pathExists)
            if (pathExists) then ! rename
!#if defined GLNX
!                line = 'tar -czf '//trim(fileName)//DASH//currentDate//DASH//currentTime//'.tar.gz '//fileName
!                call system(trim(line), eof)
!                if (eof/=0) call log_comment(itoa(eof)//' returned by: '//trim(line))
!#else
                line = trim(fileName)//DASH//currentDate//DASH//currentTime
                call rename (fileName, trim(line), eof)
                if (eof/=0) call log_comment('Status='//trim(itoa(eof))//' in moving to '//trim(line) )
!#endif
            end if
            call generate_checklists()


        case ('Import')
            call import_custom_csv()


        case ('Special')
            termBegin = currentTerm
            termEnd = termBegin

            ! read basic data for university
            call xml_read_basic_data(pathToYear)

            call read_stud_profile()
            call read_stud_grade()


        case default

    end select


contains

#if defined REGIST

#include "InputREGIST.F90"

#else

#include "InputSIAS.F90"

#endif

    subroutine restore_from_backup()

        integer :: iTmp, jTmp, kTmp, errNo

        ! read basic data for university
        call xml_read_basic_data(pathToYear, 'BACKUP.XML')

        termBegin = currentTerm
        termEnd = termBegin

        ! read schedules, blocks, enlistment
        do kTmp=termBegin,termEnd

            call qualify_term(kTmp, iTmp, jTmp)

            pathToYear = trim(dirDATA)//trim(itoa(iTmp))//DIRSEP

            ! read the classes
            call read_classes(pathToYear, jTmp, NumSections(jTmp), Section(jTmp,0:), &
                Offering(jTmp,MAX_ALL_DUMMY_SUBJECTS:), errNo, 'BACKUP.XML')

            ! read the blocks
            call read_blocks(pathToYear, jTmp, NumBlocks(jTmp), Block(jTmp,0:), NumSections(jTmp), Section(jTmp,0:), &
                errNo, 'BACKUP.XML')

            ! read enlistment files, if any
            call read_enlistment(pathToYear, jTmp, 'BACKUP', 0, 6, &
                NumSections(jTmp), Section(jTmp,0:), Enlistment(jTmp,0:), NumEnlistment(jTmp), errNo)

        end do

        call xml_write_data()

        ! read waivers, if any
        do kTmp=termBegin,termEnd
            call qualify_term(kTmp, iTmp, jTmp)
            pathToYear = trim(dirDATA)//trim(itoa(iTmp))//DIRSEP
            call xml_read_waivers(pathToYear, jTmp, NumSections(jTmp), Section(jTmp,0:), &
                NumWaiverRecords, errNo, 'BACKUP.XML')
            if (NumWaiverRecords>0) then
                pathToTerm = trim(dirDATA)//trim(itoa(iTmp))//DIRSEP//trim(txtSemester(jTmp))//DIRSEP
                call xml_write_waivers(pathToTerm, Section(jTmp,0:), jTmp)
            end if
        end do

        call terminate(trim(fileEXE)//SPACE//ACTION//' completed.')

    end subroutine restore_from_backup


    subroutine update_CTE_enlistment()

        integer :: iTmp, jTmp, errNo, cdx
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege

        termBegin = currentTerm
        termEnd = termBegin

        ! read basic data for university
        call xml_read_basic_data(pathToYear)

        pathToTerm = trim(pathToYear)//trim(txtSemester(currentTerm))//DIRSEP

        ! read the classes
        call read_classes(pathToTerm, currentTerm, NumSections(currentTerm), Section(currentTerm,0:), &
            Offering(currentTerm,MAX_ALL_DUMMY_SUBJECTS:), errNo)

        ! read the blocks
        call read_blocks(pathToTerm, currentTerm, NumBlocks(currentTerm), Block(currentTerm,0:), NumSections(currentTerm), &
            Section(currentTerm,0:), errNo)

        ! read enlistment files, if any
        call read_enlistment(pathToTerm, currentTerm, 'ENLISTMENT', 0, 6, &
            NumSections(currentTerm), Section(currentTerm,0:), Enlistment(currentTerm,0:), NumEnlistment(currentTerm), errNo)

        ! any enlisted files by CTE students
        tCollege = 'CTE'
        cdx = index_to_college(tCollege)

        ! update from CTE enlistment file
        pathToTerm = trim(pathToYear)//trim(txtSemester(currentTerm))//DIRSEP
        call read_enlistment_special(pathToTerm, 'ENLISTMENT-CTE.CSV', &
            NumSections(currentTerm), Section(currentTerm,0:), Enlistment(currentTerm,0:), iTmp, errNo)

        ! update students
        call xml_write_students(trim(pathToYear), 0)

        ! write CTE enlistment
        done = .false.
        do iTmp=1,NumCurricula
            if (done(iTmp)) cycle
            if (cdx/=Curriculum(iTmp)%CollegeIdx) cycle ! not in CTE
            call xml_write_pre_enlistment(pathToTerm, 'ENLISTMENT', &
                Enlistment(currentTerm,0:), Section(currentTerm,0:), iTmp)
            do jTmp=iTmp+1,NumCurricula
                if (CurrProgCode(iTmp) == CurrProgCode(jTmp)) done(jTmp) = .true.
            end do
        end do

        call terminate(trim(fileEXE)//SPACE//ACTION//' completed.')

    end subroutine update_CTE_enlistment


    subroutine read_enlistment_special(path, fname, NumSections, Section, eList, numEntries, errNo)

        character(len=*), intent(in) :: path, fname
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in out) :: eList(0:)
        integer, intent (out) :: numEntries, errNo

        type(TYPE_PRE_ENLISTMENT) :: wrk
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_CLASS_ID) :: tClassId
        character (len=MAX_LEN_STUDENT_CODE) :: tStdNo, prevStdNo
        character (len=MAX_LEN_SECTION_CODE) :: tCode
        character (len=MAX_LEN_PERSON_NAME) :: tName
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        integer :: cdx, sdx, std, idx, idxCurr, i, j

        numEntries = 0
        ! open file, return on any error
        fileName = trim(path)//fname
        open(unit=unitRAW,file=fileName, status='old', iostat=errNo)
        if (errNo/=0) then
            write(*,*) 'Not found: '//trim(fileName)
            return
        end if
        write(*,*) 'Retrieving enlistment from '//trim(fileName)

        ! skip first line
        prevStdNo = SPACE
        read (unitRAW, AFORMAT, iostat = eof) line
        do
            read (unitRAW, AFORMAT, iostat = eof) line
            if (eof < 0) exit
!StudNumber,SubjectCode,ClassCode,,,StudName
!12-01847,TLE 58,E0335,13-2,BSED 2J,"Abad, Adelfa Cardenas"
!12-01847,PE,,13-2,BSED 2J,"Abad, Adelfa Cardenas"
!12-01847,TLE 57,E0334,13-2,BSED 2J,"Abad, Adelfa Cardenas"
!13-01760,LIT 11,E0233,13-2,BSED 1H,"Abad, Janette Manuel"
!13-01760,ENG 12,E0232,13-2,BSED 1H,"Abad, Janette Manuel"
!13-01760,SOC SCI 12,E0231,13-2,BSED 1H,"Abad, Janette Manuel"
!11-04497,ELECTIVE 1,E0456,13-2,BSED 3K,"Abad, Jarice Mamba"
!11-04497,TLE 64,E0453,13-2,BSED 3K,"Abad, Jarice Mamba"
!11-04497,EDU 45,E0452,13-2,BSED 3K,"Abad, Jarice Mamba"
!1       2      3     4    5       6
            if (line(1:3) == '   ') cycle
            call index_to_delimiters(COMMA, line, ndels, pos)

            tStdNo = adjustl(line(:pos(2)-1))
            if (tStdNo/=prevStdNo) then
                std = index_to_student(prevStdNo)
                if (std==0 .and. prevStdNo/=SPACE) then
                    write(*,*) 'ADD student '//tStdNo//tName//Curriculum(abs(idxCurr))%Code
                    NumAdditionalStudents = NumAdditionalStudents + 1
                    std = NumStudents + NumAdditionalStudents
                    Student(std)%StdNo = prevStdNo
                    Student(std)%Name = tName
                    Student(std)%CurriculumIdx =  abs(idxCurr)
                end if

                ! merge those previously enlisted in HEEDS
                do i=1,eList(std)%lenSubject
                    sdx = eList(std)%Section(i)
                    if (sdx==0) cycle ! not enlisted
                    ! find location of previously enlisted section
                    cdx = 0
                    do j=1,wrk%lenSubject
                        if (wrk%Subject(j)==eList(std)%Subject(i)) cdx = j
                    end do
                    if (cdx==0) then ! not found; add section previously enlisted in HEEDS
                        idx = wrk%lenSubject + 1
                        wrk%lenSubject = idx
                        wrk%Subject(idx) = eList(std)%Subject(i)
                        wrk%Section(idx) = sdx
                        wrk%Grade(idx) = gdxREGD
                        wrk%Contrib(idx) = 1.0
                        write (*,*) indentation(:indent1)//'Added previously enlisted '//Section(sdx)%ClassId
                    else ! found; replace with section that was enlisted in HEEDS
                        if (wrk%Section(cdx) /= sdx) then
                            write (*,*) indentation(:indent1)//'Using previously enlisted '//trim(Section(sdx)%ClassId)// &
                                ' instead of '//Section(wrk%Section(cdx))%ClassId
                            wrk%Section(cdx) = sdx
                        else
                            write (*,*) indentation(:indent1)//'Retained enlisted '//Section(sdx)%ClassId
                            wrk%Section(cdx) = sdx
                        end if
                    end if

                end do

                eList(std) = wrk
                numEntries = numEntries+1

                call initialize_pre_enlistment (wrk)
                prevStdNo = tStdNo
                tName = line(pos(6)+2:)
                idx = len_trim(tName)
                tName(idx:) = SPACE
                call upper_case(tName)
                tCurriculum = line(pos(5)+1:pos(6)-1)
                idxCurr = index(tCurriculum, SPACE)
                if (idxCurr>0) tCurriculum(idxCurr+1:) = SPACE
                idxCurr = index_to_curriculum(tCurriculum)

                write(*,AFORMAT) SPACE, tStdNo//tName
            end if

            tSubject = adjustl(line(pos(2)+1:pos(3)-1))
            cdx = index_to_subject(tSubject)
            if (cdx==0) then
                write(*,*) indentation(:indent0)//trim(tSubject)//' - Subject not found: '//trim(line)
                cycle
            end if

            if (pos(4)-pos(3)==1) then
                write(*,*) indentation(:indent0)//'Section code not specified: '//trim(line)
                cycle
            end if
            tCode = line(pos(3)+1:pos(3)+1)//line(pos(3)+3:pos(4)-1)
            if (Subject(cdx)%LabHours/=0.0) tCode =  trim(tCode)//'-1L'

            tClassId = trim(tSubject)//SPACE//tCode
            sdx = index_to_section(tClassId, NumSections, Section)
            if (sdx==0) then
                write(*,*) indentation(:indent0)//trim(tClassId)//' - class not found: '//trim(line)
                cycle
            end if

            idx = wrk%lenSubject + 1
            wrk%lenSubject = idx
            wrk%Section(idx) = sdx
            wrk%Subject(idx) = cdx
            wrk%Grade(idx) = gdxREGD
            wrk%Contrib(idx) = 1.0
            write(*,*) indentation(:indent0)//'Found '//tClassId

        end do
        close(unitRAW)

        std = index_to_student(prevStdNo)
        if (std==0 .and. prevStdNo/=SPACE) then
            write(*,*) 'ADD student '//tStdNo//tName//tCurriculum
            NumAdditionalStudents = NumAdditionalStudents + 1
            std = NumStudents + NumAdditionalStudents
            Student(std)%StdNo = prevStdNo
            Student(std)%Name = tName
            Student(std)%CurriculumIdx =  abs(idxCurr)
        end if

        ! merge those previously enlisted in HEEDS
        do i=1,eList(std)%lenSubject
            sdx = eList(std)%Section(i)
            if (sdx==0) cycle ! not enlisted
            ! find location of previously enlisted section
            cdx = 0
            do j=1,wrk%lenSubject
                if (wrk%Subject(j)==eList(std)%Subject(i)) cdx = j
            end do
            if (cdx==0) then ! not found; add section previously enlisted in HEEDS
                idx = wrk%lenSubject + 1
                wrk%lenSubject = idx
                wrk%Subject(idx) = eList(std)%Subject(i)
                wrk%Section(idx) = sdx
                wrk%Grade(idx) = gdxREGD
                wrk%Contrib(idx) = 1.0
                write (*,*) indentation(:indent2)//'Added previously enlisted '//Section(sdx)%ClassId
            else ! found; replace with section that was enlisted in HEEDS
                if (wrk%Section(cdx) /= sdx) then
                    write (*,*) indentation(:indent2)//'Using previously enlisted '// &
                        trim(Section(sdx)%ClassId)//' instead of '//Section(wrk%Section(cdx))%ClassId
                    wrk%Section(cdx) = sdx
                else
                    write (*,*) indentation(:indent1)//'Retained enlisted '//Section(sdx)%ClassId
                    wrk%Section(cdx) = sdx
                end if
            end if

        end do

        eList(std) = wrk
        numEntries = numEntries+1

    end subroutine read_enlistment_special


    subroutine read_stud_profile()

    ! read CSU-Gonzaga students from XML mysql-export

        type (TYPE_STUDENT) :: wrkStudent
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        integer :: idxCURR, indexLoc, numEntries, ier

        pathToTerm = trim(pathToYear)//trim(txtSemester(currentTerm))//DIRSEP

        fileName = trim(pathToYear)//'stud_profile.xml'
        open(unit=unitXML, file=fileName, status='old', iostat=ier)
        if (ier/=0) return
        numEntries = 0

        ! examine the file line by line
        do
            read(unitXML, AFORMAT, iostat=eof) line
            if (eof<0) exit

            ! get tag and value if any
            call xml_parse_line(line, tag, value, eof)

            !if (eof/=0) exit
            !write(*,*) eof, trim(line), trim(tag), trim(value)

!    <database name="csudata">
!        <table name="studprofile">
!            <column name="idnum">09-0695</column>
!            <column name="sname">ABAD</column>
!            <column name="fname">KATHRINA</column>
!            <column name="mname">A</column>
!            <column name="course">BSHIM</column>
!            <column name="college">CAS</column>
!        </table>
            select case (trim(tag))

                case ('table name="studprofile"')
                    call initialize_student(wrkStudent)

                case ('column name="idnum"')
                    wrkStudent%StdNo = adjustl(value)
                    call upper_case(wrkStudent%StdNo)

                case ('column name="sname"')
                    wrkStudent%Name = value

                case ('column name="fname"')
                    wrkStudent%Name = trim(wrkStudent%Name)//COMMA//SPACE//value

                case ('column name="mname"')
                    wrkStudent%Name = trim(wrkStudent%Name)//SPACE//value

                case ('column name="course"')
                    tCurriculum = trim(value)//DASH//wrkStudent%StdNo(:2)
                    idxCurr = index_to_curriculum(tCurriculum)
                    if (idxCurr<0) then
                        idxCurr = -idxCurr
                    else if (idxCurr==0) then
                        idxCurr = NumCurricula
                    end if
                    wrkStudent%CurriculumIdx = idxCurr

                case ('/table')
                    if (len_trim(wrkStudent%StdNo)==0) cycle
                    if (len_trim(wrkStudent%Password)==0) then
                        call set_password(wrkStudent%Password)
                        isDirtySTUDENTS = .true.
                    end if
                    call update_student_info(wrkStudent, indexLoc)
                    if (indexLoc<0) then
                        numEntries = numEntries + 1
                        !write(*,*) trim(text_student_curriculum(-indexLoc))
                    end if

                case default
                    ! do nothing
            end select
        end do
        close(unitXML)
        write(*,*) numEntries, 'students from '//trim(fileName)


        ! update students
        if (NumStudents>0) call xml_write_students(trim(pathToYear), 0)

    end subroutine read_stud_profile


    subroutine read_stud_grade()

        !character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_TEXT_GRADE) :: tGrade
        character(len=MAX_LEN_STUDENT_CODE) :: prevStdNo, tStdNo
        character (len=4) :: dirYear
        integer :: idxYear, idxTerm
        integer :: cdx, fdx, gdx, idx, sdx, ierr, k, std

        type(TYPE_PRE_ENLISTMENT) :: wrk
        character (len=MAX_LEN_CLASS_ID) :: tClassID, tCode


        ! zero out counters
        do cdx=MAX_ALL_DUMMY_SUBJECTS,MAX_ALL_SUBJECTS
            Subject(cdx)%Failrate = 0.0
            Subject(cdx)%GrandTotal = 0
        end do

        do idxYear = baseYear,currentYear
            dirYear = itoa(idxYear)

            do idxTerm = 1,3

                ! try 'stud_grade.xml'
                fileName = trim(dirDATA)//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP// &
                    'stud_grade.xml'
                open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
                if (ierr==0) then
                    write(*,*) 'Retrieving grades from '//trim(fileName)

                    ! initialize dummy classes
                    NumSections(idxTerm) = 0
                    call initialize_section (Section(idxTerm,0))
                    Section(idxTerm,:) = Section(idxTerm,0)

                    ! initialize pre-enlisted subjects of students
                    call initialize_pre_enlistment(Enlistment(idxTerm,0))
                    Enlistment(:,:) = Enlistment(idxTerm,0)

                    prevStdNo = SPACE
                    ! examine the file line by line
                    do
                        read(unitRAW, AFORMAT, iostat=eof) line
                        if (eof<0) exit

                        ! get tag and value if any; exit on any error
                        call xml_parse_line(line, tag, value, eof)
                        !if (eof/=0) exit

!        <table name="stud_rec">
!            <column name="idnum">00-0023</column>
!            <column name="syear">2013-2014</column>
!            <column name="sem">1st</column>
!            <column name="school">CAGAYAN STATE UNIVERSITY - GONZAGA                                                                                                                    </column>
!            <column name="code">IT 120</column>
!            <column name="subject">IT Elective 4                                                                                                                                         </column>
!            <column name="grade">76.00</column>
!            <column name="units">3.00</column>
!            <column name="rem"></column>
!            <column name="pcode">G11314S115</column>
!        </table>
                        select case (trim(tag))

                            case ('column name="idnum"')
                                tStdNo = adjustl(value)
                                std = index_to_student(tStdNo)
                                if (std==0 .and. prevStdNo/=SPACE) then
                                    write(*,*) 'ADD student '//tStdNo
                                    NumAdditionalStudents = NumAdditionalStudents + 1
                                    std = NumStudents + NumAdditionalStudents
                                    Student(std)%StdNo = tStdNo
                                    prevStdNo = tStdNo
                                end if

                            case ('column name="code"')
                                tSubject = value
                                call upper_case(tSubject)
                                cdx = index_to_subject(tSubject)
                                if (cdx<=0) then

                                    write(*,*) trim(Student(std)%Name)//' - "'//trim(tSubject)//'" not in catalog'
                                    NumAdditionalSubjects = NumAdditionalSubjects+1
                                    cdx = NumSubjects + NumAdditionalSubjects

                                    Subject(cdx)%Name = tSubject
                                    Subject(cdx)%Title = tSubject
                                    Subject(cdx)%DeptIdx = NumDepartments
                                    Subject(cdx)%Units = 3.0

                                    Subject(cdx)%TermOffered = 7
                                    Subject(cdx)%LectHours = 3.0
                                    Subject(cdx)%MinLectSize = 50
                                    Subject(cdx)%MaxLectSize = 50
                                    Subject(cdx)%LectLoad = 0.0
                                    Subject(cdx)%LabHours = 0.0
                                    Subject(cdx)%MinLabSize = 50
                                    Subject(cdx)%MaxLabSize = 50
                                    Subject(cdx)%LabLoad = 0.0

                                    k = 1
                                    Subject(cdx)%lenPreq = k
                                    Subject(cdx)%Prerequisite(k) = INDEX_TO_NONE
                                    Subject(cdx)%lenCoreq = k
                                    Subject(cdx)%Corequisite = INDEX_TO_NONE
                                    Subject(cdx)%lenConc = k
                                    Subject(cdx)%Concurrent = INDEX_TO_NONE
                                    Subject(cdx)%lenConcPreq = k
                                    Subject(cdx)%ConcPrerequisite= INDEX_TO_NONE

                                    Subject(cdx)%LabFee = 0.0
                                    Subject(cdx)%Tuition = 0.0

                                end if

                            case ('column name="grade"')

                                tGrade = value
                                if (tGrade==SPACE) then
                                    gdx = gdxDRP
                                else
                                    gdx = index(tGrade,DOT)
                                    if (gdx>0) tGrade(gdx:) = SPACE
                                    gdx = index_to_grade(tGrade)
                                    if (gdx==0) then
                                        write(*,*) trim(Student(std)%Name)//' - "'//trim(tSubject)//'" grade is '//trim(value)// &
                                            ' using DRP.'
                                        gdx = gdxDRP
                                    end if

                                end if

                            case ('column name="pcode"')

                                fdx = Enlistment(idxTerm,std)%lenSubject + 1
                                call check_array_bound (fdx, MAX_SUBJECTS_PER_TERM, &
                                    'MAX_SUBJECTS_PER_TERM : '//trim(Student(std)%StdNo)//' - too many subjects taken?')
                                Enlistment(idxTerm,std)%Subject(fdx) = cdx
                                Enlistment(idxTerm,std)%Grade(fdx) = gdx
                                Enlistment(idxTerm,std)%lenSubject = fdx

                                if (is_grade_numeric_pass(gdx)) then  ! grade counter
                                    Subject(cdx)%GrandTotal(1,idxTerm) = Subject(cdx)%GrandTotal(1,idxTerm) + 1
                                else
                                    Subject(cdx)%GrandTotal(2,idxTerm) = Subject(cdx)%GrandTotal(2,idxTerm) + 1
                                end if

                                ! create a class?
                                tCode = 'Z'//value(8:)
                                if (Subject(cdx)%LabHours>0) then ! subject has lab/recitation
                                    tClassID = trim(Subject(cdx)%Name)//SPACE//trim(tCode)//DASH//'1L'
                                else
                                    tClassID = trim(Subject(cdx)%Name)//SPACE//tCode
                                end if
                                k = index_to_section(tClassID, NumSections(idxTerm), Section(idxTerm,0:) )

                                if (k==0) then ! add a class

                                    if (Subject(cdx)%LectHours>0) then ! subject has lecture
                                        tClassID = trim(Subject(cdx)%Name)//SPACE//tCode
                                        k = NumSections(idxTerm) + 1
                                        NumSections(idxTerm) = k
                                        Section(idxTerm,k) = TYPE_SECTION (tClassID, tCode, SPACE, &
                                            NumDepartments, cdx, Subject(cdx)%MaxLectSize, Subject(cdx)%MaxLectSize, &
                                            1, 0, 0, 0, 0, 0)
                                        !write(*,*) 'Added lecture ', k, tClassID
                                    end if
                                    if (Subject(cdx)%LabHours>0) then ! subject has lab/recitation
                                        tCode = trim(tCode)//DASH//'1L'
                                        tClassID = trim(Subject(cdx)%Name)//SPACE//tCode
                                        k = NumSections(idxTerm) + 1
                                        NumSections(idxTerm) = k
                                        Section(idxTerm,k) = TYPE_SECTION (tClassID, tCode, SPACE, &
                                            NumDepartments, cdx, Subject(cdx)%MaxLabSize, Subject(cdx)%MaxLabSize, &
                                            1, 0, 0, 0, 0, 0)
                                        !write(*,*) 'Added lab ', k, tClassID
                                    end if
                                end if
                                Enlistment(idxTerm,std)%Section(fdx) = k

                            case default
                                ! do nothing
                        end select

                    end do
                    close(unitRAW)
!                    call write_transcripts(idxYear, idxTerm)

                    ! create "equivalent" ENLISTMENT.XML
                    fileName = trim(dirDATA)//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP
                    call xml_write_classes(fileName, NumSections(idxTerm), Section(idxTerm,0:), 0)
                    call xml_write_pre_enlistment(fileName, 'EQUIVALENT-ENLISTMENT', Enlistment(idxTerm,0:), Section(idxTerm,0:))

                end if


                ! try 'ENLISTMENT.XML'
                fileName = trim(dirDATA)//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP// &
                    'ENLISTMENT.XML'
                open(unit=unitRAW,file=fileName,status='old', iostat=ierr)
                if (ierr==0) then
                    write(*,*) 'Retrieving grades from '//trim(fileName)

                    ! initialize student data
                    call initialize_student(Student(0))
                    Student = Student(0)
                    NumStudents = 0

                    ! initialize pre-enlisted subjects of students
                    call initialize_pre_enlistment(Enlistment(idxTerm,0))
                    Enlistment(:,:) = Enlistment(idxTerm,0)

                    prevStdNo = '#-#-#-#-#-#-#-#'
                    ! examine the file line by line
                    do
                        read(unitRAW, AFORMAT, iostat=eof) line
                        if (eof<0) exit

                        ! get tag and value if any; exit on any error
                        call xml_parse_line(line, tag, value, eof)
                        if (eof/=0) exit

                        select case (trim(tag))

                            case ('Student') ! initialize temporary college data
                                call initialize_pre_enlistment (wrk)
                                NumStudents = NumStudents + 1

                            case ('StdNo')
                                tStdNo = adjustl(value)
                                Student(NumStudents)%StdNo = tStdNo

                            case ('Name')
                                Student(NumStudents)%Name = adjustl(value)

                            case ('Graded')

                                idx = index(value, COMMA)

                                ! extract grade
                                tGrade = adjustl(value(idx+1:))
                                gdx = index_to_grade(tGrade)

                                ! extract subject
                                tClassID = adjustl(value(:idx-1))
                                sdx = len_trim(tClassID)
                                do while (sdx>1 .and. tClassID(sdx:sdx)/=SPACE)
                                    sdx = sdx-1
                                end do
                                tSubject = tClassID(:sdx)
                                cdx = index_to_subject(tSubject)
                                if (cdx<=0) then ! subject code not found
                                    call log_comment ('No such class; ignored - '//tClassID)
                                    cycle
                                end if

                                call check_array_bound (wrk%lenSubject+1, MAX_SUBJECTS_PER_TERM, 'MAX_SUBJECTS_PER_TERM')
                                wrk%lenSubject = wrk%lenSubject + 1
                                wrk%Subject(wrk%lenSubject) = cdx
                                wrk%Grade(wrk%lenSubject) = gdx

                                if (is_grade_numeric_pass(gdx)) then  ! grade counter
                                    Subject(cdx)%GrandTotal(1,idxTerm) = Subject(cdx)%GrandTotal(1,idxTerm) + 1
                                else
                                    Subject(cdx)%GrandTotal(2,idxTerm) = Subject(cdx)%GrandTotal(2,idxTerm) + 1
                                end if

                            case ('/Student')
                                wrk%NPriority = wrk%lenSubject
                                Enlistment(idxTerm,NumStudents) = wrk

                            case default

                        end select

                    end do

                    close(unitRAW)

                    call write_transcripts(idxYear, idxTerm)

                end if

                if (NumStudents==0) then
                    write(*,*) 'Grade file not found in '// &
                        trim(dirDATA)//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP
                    cycle
                end if


            end do ! idxTerm = 1,3

        end do ! idxYear = baseYear,currentYear

        !! Subject() was modified; rewrite
        !call xml_write_subjects(trim(pathToYear)//'SUBJECTS.XML')

    end subroutine read_stud_grade

end program STATIC
