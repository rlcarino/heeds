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
            call generate_checklists()


        case ('Import')
            call import_custom_csv()


        case ('Special')
            call special_action()


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


    subroutine special_action()

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

    end subroutine special_action


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


end program STATIC
