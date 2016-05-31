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


module XMLIO

    use UNIVERSITY

    implicit none

    character (len=MAX_LEN_FILE_PATH) :: &
        dirCOLLEGES, &
        dirDEPARTMENTS, &
        dirSUBJECTS, &
        dirCURRICULA, &
        dirROOMS, &
        dirTEACHERS, &
        dirCLASSES(3), &
        dirBLOCKS(3),  &
        dirSTUDENTS, &
        indexCLASSES(3), &
        indexBLOCKS(3)

    ! private tokens
    character (len=MAX_LEN_FILE_PATH), private :: fileName
    character (len=MAX_LEN_XML_LINE), private :: line, value
    character (len=MAX_LEN_XML_TAG), private :: tag
    integer, private :: ndels, pos(30)

contains

#include "XMLIO-OTHER.F90"

    subroutine read_configuration(pathToFile, errNo)

        character(len=*), intent(in) :: pathToFile
        integer, intent (out) :: errNo
        integer :: stat

        ! defaults
#if defined GLNX
        call get_environment_variable("HOME", DOCUMENT_ROOT)
        HEEDS_DIRECTORY = trim(DOCUMENT_ROOT)//DIRSEP//UniversityCode  ! /home/user/UNIVERSITY
        DOCUMENT_ROOT = trim(DOCUMENT_ROOT)//DIRSEP//'web'  ! /home/user/web
#else
        call get_environment_variable("HOMEDRIVE", DOCUMENT_ROOT)
        HEEDS_DIRECTORY = trim(DOCUMENT_ROOT)//DIRSEP//PROGNAME ! C:\HEEDS\UNIVERSITY
        DOCUMENT_ROOT = trim(DOCUMENT_ROOT)//DIRSEP//PROGNAME//DIRSEP//'web' ! C:\HEEDS\web
#endif
        SERVER_PROTOCOL = 'http://'
        SERVER_NAME = 'localhost'

        ! open file, return on any error
        call xml_read_file(unitXML, 'CONFIGURATION', pathToFile, errNo)
        if (errNo/=0) return

        ! examine the file line by line
        do
            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit

            select case (trim(tag))

                case ('/CONFIGURATION')
                    exit

                case ('DOCUMENT_ROOT')
                     DOCUMENT_ROOT = adjustl(value)

                case ('HEEDS_DIRECTORY')
                     HEEDS_DIRECTORY = adjustl(value)

                case ('SERVER_PROTOCOL')
                     SERVER_PROTOCOL = adjustl(value)

                case ('SERVER_NAME')
                     SERVER_NAME = adjustl(value)

                case default ! do nothing

            end select

        end do

        close(unitXML)
        !call log_comment('From '//pathTofile, 'HEEDS_DIRECTORY='//trim(HEEDS_DIRECTORY), 'DOCUMENT_ROOT='//trim(DOCUMENT_ROOT), &
        !    'SERVER_PROTOCOL='//trim(SERVER_PROTOCOL), 'SERVER_NAME='//trim(SERVER_NAME) )

    end subroutine read_configuration


!===========================================================
! routines for country-level data
!===========================================================

    subroutine read_PSGC()

        integer :: indexLoc, idx, stat
        character(len=MAX_LEN_PSGC) :: tCode

        ! initialize
        NumPSGC = 0
        PSGC = TYPE_PSGC(SPACE,SPACE)

        ! open file, return on any error
        call xml_read_file(unitXML, ROOT_PSGC, 'PSGC.XML', stat)
        if (stat/=0) return

        do

            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit
            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit
            if (trim(tag)/='PSGC') cycle
            !write(*,*) trim(tag)//'='//trim(value)
            tCode = value(1:MAX_LEN_PSGC)
            NumPSGC = NumPSGC + 1
            indexLoc = NumPSGC
            do while (indexLoc>1 .and. PSGC(indexLoc-1)%Code>tCode)
                PSGC(indexLoc) = PSGC(indexLoc-1)
                indexLoc = indexLoc - 1
            end do
            PSGC(indexLoc)%Code = tCode
            PSGC(indexLoc)%Name = value(MAX_LEN_PSGC+2:)

        end do

        close(unitXML)

        ! collect indices to Regions, Provinces/"Not a Province", Municipality\ies
        Region = 0
        Province = 0
        Municipality = 0
        NumRegion = 0
        NumProvince = 0
        NumMunicipality = 0
        do indexLoc=1,NumPSGC
            if ( isPSGC_kind(PSGC(indexLoc)%Code, kindRegion) ) then
                NumRegion = NumRegion+1
                Region(NumRegion) = indexLoc
                !write(*,*) NumRegion, PSGC(indexLoc)%Code//' - '//PSGC(indexLoc)%Name
            elseif ( isPSGC_kind(PSGC(indexLoc)%Code, kindProvince) ) then
                NumProvince = NumProvince+1
                Province(NumProvince) = indexLoc
                !write(*,*) NumProvince, PSGC(indexLoc)%Code//' -   '//PSGC(indexLoc)%Name
            elseif ( isPSGC_kind(PSGC(indexLoc)%Code, kindMunicipality) ) then
                NumMunicipality = NumMunicipality+1
                Municipality(NumMunicipality) = indexLoc
                !write(*,*) NumMunicipality, PSGC(indexLoc)%Code//' -     '//PSGC(indexLoc)%Name
            end if
        end do
        Region(NumRegion+1) = NumPSGC+1
        Province(NumProvince+1) = NumPSGC+1
        Municipality(NumMunicipality+1) = NumPSGC+1

        ! convert Town, Province names to proper case
        do indexLoc=1,NumProvince
            idx = Province(indexLoc)
            call proper_case(PSGC(idx)%Name)
            if (PSGC(idx)%Name(1:3)=='Ncr') PSGC(idx)%Name(1:3) = 'NCR'
        end do
        do indexLoc=1,NumMunicipality
            idx = Municipality(indexLoc)
            call proper_case(PSGC(idx)%Name)
            if (PSGC(idx)%Name(1:3)=='Ncr') PSGC(idx)%Name(1:3) = 'NCR'
        end do

    end subroutine read_PSGC


    subroutine read_TONGUES()

        integer :: indexLoc, j, k, stat
        character(len=MAX_LEN_CODE_TONGUE) :: tCode

        ! initialize
        NumTongue = 0
        Tongue = TYPE_TONGUE(0,SPACE,SPACE)

        ! open file, return on any error
        call xml_read_file(unitXML, ROOT_TONGUES, 'TONGUES.XML', indexLoc)
        if (indexLoc/=0) return

        do

            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit
            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit
            if (trim(tag)/='TONGUE') cycle
            !write(*,*) trim(tag)//'='//trim(value)
            tCode = value(1:MAX_LEN_CODE_TONGUE)
            NumTongue = NumTongue + 1
            indexLoc = NumTongue
            do while (indexLoc>1 .and. Tongue(indexLoc-1)%Code>tCode)
                Tongue(indexLoc) = Tongue(indexLoc-1)
                indexLoc = indexLoc - 1
            end do
            Tongue(indexLoc)%Code = tCode
            Tongue(indexLoc)%Name = value(MAX_LEN_CODE_TONGUE+2:)

        end do

        close(unitXML)
        ! sort
        do indexLoc=1,NumTongue
            Tongue(indexLoc)%Rank = indexLoc
        end do
        do indexLoc=1,NumTongue-1
            do j=indexLoc+1,NumTongue
                if (Tongue(Tongue(indexLoc)%Rank)%Name>Tongue(Tongue(j)%Rank)%Name) then
                    k = Tongue(indexLoc)%Rank
                    Tongue(indexLoc)%Rank = Tongue(j)%Rank
                    Tongue(j)%Rank = k
                end if
            end do
        end do

    end subroutine read_TONGUES


!===========================================================
! routines for University-level data
!===========================================================

    subroutine university_data_read(pathToFile)

        character(len=*), intent(in) :: pathToFile

        integer :: nFees, loc, nSchols, stat

        ! find ROOT_UNIVERSITY
        call xml_read_file(unitXML, ROOT_UNIVERSITY, pathToFile, stat)
        call terminate(stat, 'Error: "'//ROOT_UNIVERSITY//'" not found in '//pathToFile)

        ! examine the file line by line
        nFees = 0
        nSchols = 0
        NumAccounts = 0
        do
            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit

            select case (trim(tag))

                case (FSLASH//ROOT_UNIVERSITY)
                    exit

                case ('NAME')
                    UniversityName = adjustl(value)

                case ('ADDRESS')
                    UniversityAddress = adjustl(value)

                case ('WEB')
                     UniversityWeb = adjustl(value)

                case ('PHONE')
                     UniversityPhone = adjustl(value)

                case ('BASEYEAR')
                    baseYear = atoi(value)

                case ('PRESIDENT')
                    UniversityPresident = adjustl(value)

                case ('DEANOFINSTRUCTION')
                    DeanOfInstruction = adjustl(value)

                case ('VPACADEMICAFFAIRS')
                    VPAcademicAffairs = adjustl(value)

                case ('DEANOFCAMPUS')
                    DeanOfCampus = adjustl(value)

                case ('THEREGISTRAR')
                    TheRegistrar = adjustl(value)

                case ('PASSKEY')
                     passwordEncryptionKey = adjustl(value)

                case ('FEE')
                     loc = index(value, COMMA)
                     nFees = nFees + 1
                     FeeAmount(nFees) = atof(value(1:loc-1))
                     FeeDescription(nFees) = trim(value(loc+1:))

                case ('SCHOLARSHIP')
                     loc = index(value, COMMA)
                     nSchols = nSchols + 1
                     ScholarshipCode(nSchols) = value(1:loc-1)
                     ScholarshipDescription(nSchols) = value(loc+1:)

                case ('Account', 'ACCOUNT')
                     loc = index(value, COMMA)
                     NumAccounts = NumAccounts + 1
                     AccountCode(NumAccounts) = value(1:loc-1)
                     AccountDescription(NumAccounts) = value(loc+1:)

                case default ! do nothing

            end select

        end do

        close(unitXML)

    end subroutine university_data_read


    subroutine university_data_write(device, universityFile)
        integer, intent (in) :: device
        character (len=*), intent (in) :: universityFile

        integer :: idx, stat
        character (len=MAX_LEN_FILE_PATH) :: fileName, filePart

        stat = 0 ! error status

        if (len_trim(universityFile)>0) then
            fileName = universityFile
#if defined GLNX
            filePart = trim(fileName)//dotPART
#else
            filePart = fileName
#endif
            open(unit=device, file=filePart, iostat=stat, err=999)
            call xml_write_character(device, 0, XML_DOC)
        else
            fileName = ' university-level data to backup file'
        end if

        call xml_write_character(unitXML, indent0, ROOT_UNIVERSITY)
        call xml_write_character(unitXML, indent1, 'NAME', UniversityName)
        call xml_write_character(unitXML, indent1, 'ADDRESS', UniversityAddress)
        call xml_write_character(unitXML, indent1, 'WEB', UniversityWeb)
        call xml_write_character(unitXML, indent1, 'PHONE', UniversityPhone)
        call xml_write_character(unitXML, indent1, 'PRESIDENT', UniversityPresident)
        call xml_write_character(unitXML, indent1, 'DEANOFINSTRUCTION', DeanOfInstruction)
        call xml_write_character(unitXML, indent1, 'VPACADEMICAFFAIRS', VPAcademicAffairs)
        call xml_write_character(unitXML, indent1, 'DEANOFCAMPUS', DeanOfCampus)
        call xml_write_character(unitXML, indent1, 'THEREGISTRAR', TheRegistrar)
        call xml_write_integer  (unitXML, indent1, 'BASEYEAR', baseYear)
        call xml_write_character(unitXML, indent1, 'PASSKEY', passwordEncryptionKey)

        do idx=1,MAX_ALL_FEES
            if (FeeAmount(idx)==0.0) cycle
            call xml_write_character(unitXML, indent1, 'FEE', trim(ftoa(FeeAmount(idx),1))//COMMA// &
                trim(FeeDescription(idx)))
        end do

        do idx=1,MAX_ALL_SCHOLARSHIPS
            if (len_trim(ScholarshipCode(idx))==0) cycle
            call xml_write_character(unitXML, indent1, 'SCHOLARSHIP', trim(ScholarshipCode(idx))//COMMA// &
                trim(ScholarshipDescription(idx)))
        end do

        do idx=1,NumAccounts
            call xml_write_character(unitXML, indent1, 'ACCOUNT', trim(AccountCode(idx))//COMMA// &
                trim(AccountDescription(idx)))
        end do

        call xml_write_character(unitXML, indent0, FSLASH//ROOT_UNIVERSITY)

        if (len_trim(universityFile)>0) then
            close(device, iostat=stat, err=999)
#if defined GLNX
            call rename(filePart, fileName, stat)
#endif
        end if

        999 call terminate(stat, 'Error in writing '//fileName)
        call log_comment('Successfully wrote '//fileName)
        isDirtyData = .true. ! allow writing to backup

    end subroutine university_data_write



!===========================================================
! routines for College()
!===========================================================


    subroutine colleges_index_read(device, path)
        integer, intent (in) :: device
        character (len=*), intent (in) :: path

        integer :: stat

        open(unit=device, file=trim(path)//'index', iostat=stat)
        call terminate(stat, 'Error '//trim(itoa(stat))//' in opening index for '//path)
        do
            read(device, AFORMAT, iostat=stat) line
            if (stat<0) exit
            call college_details_read(trim(path)//trim(line)//dotXML)
        end do
        close(device)

    end subroutine colleges_index_read


    subroutine college_details_write(device, path, first, last)
        character (len=*), intent (in) :: path
        integer, intent (in) :: device, first
        integer, intent (in), optional :: last

        integer :: idx, ldx, tdx, lastIdx, tab0, tab1, tab2, stat
        logical :: monolithic!, fileExists
        character (len=MAX_LEN_FILE_PATH) :: fileName, filePart

        if (isReadOnly) return

        if (present(last)) then
            lastIdx = last
            tab0 = INDENT_INCR
        else
            lastIdx = first
            tab0 = 0
        end if
        monolithic = lastIdx>first

        tab1 = tab0+INDENT_INCR
        tab2 = tab0+INDENT_INCR*2

        stat = 0 ! error status
        if (monolithic) then ! single file

            if (len_trim(path)>0) then
                fileName = path
#if defined GLNX
                filePart = trim(fileName)//dotPART
#else
                filePart = fileName
#endif
                open(unit=device, file=filePart, iostat=stat, err=999)

                ! write the index only
                if (index(path, DIRSEP//'index')>0) then
                    write(device,AFORMAT, iostat=stat, err=999) &
                        ( trim(filename_from(College(ldx)%Code)), ldx=first,lastIdx )
                    close(device, iostat=stat, err=999)
#if defined GLNX
                    call rename(filePart, fileName, stat)
#endif
                    goto 999
                end if

                call xml_write_character(device, 0, XML_DOC)
            else
                fileName = 'colleges to backup file'
            end if

            call xml_write_character(device, 0, ROOT_COLLEGES)

        end if


        do ldx=first,lastIdx

            if (.not. monolithic) then

                fileName = trim(path)//trim(filename_from(College(ldx)%Code))//dotXML
#if defined GLNX
                filePart = trim(fileName)//dotPART
#else
                filePart = fileName
#endif
                open(unit=device, file=filePart, iostat=stat, err=999)
                call xml_write_character(device, 0, XML_DOC)
                call xml_write_character(device, 0, ROOT_COLLEGES)

            end if

            call xml_write_character(device, tab0, 'College')
            call xml_write_character(device, tab1, 'Code', College(ldx)%Code)
            call xml_write_character(device, tab1, 'Name', College(ldx)%Name)
            call xml_write_character(device, tab1, 'Dean', College(ldx)%Dean)
            call xml_write_character(device, tab1, 'TranscriptPreparer', College(ldx)%TranscriptPreparer)
            call xml_write_character(device, tab1, 'TranscriptChecker', College(ldx)%TranscriptChecker)

            !logical :: isAllowed(AdviseOpenSubjects:ToEditGRADES,3)
            do tdx=firstSemester,summerTerm
                do idx=AdviseOpenSubjects,ToEditGRADES
                    if (College(ldx)%isAllowed(idx,tdx)) then
                        call xml_write_character(device, tab1, 'isAllowed', &
                            trim(itoa(idx))//SPACE//trim(itoa(tdx))//' 1')
                    else
                        call xml_write_character(device, tab1, 'isAllowed', &
                            trim(itoa(idx))//SPACE//trim(itoa(tdx))//' 0')
                    end if
                end do
            end do

            call xml_write_character(device, tab0, '/College')

            if (.not. monolithic) then
                call xml_write_character(device, 0, FSLASH//ROOT_COLLEGES)
                close(device, iostat=stat, err=999)
#if defined GLNX
                call rename(filePart, fileName, stat)
#endif
                if (stat/=0) goto 999
            end if

        end do

        if (monolithic) then
            call xml_write_character(device, 0, FSLASH//ROOT_COLLEGES)
            if (len_trim(path)>0) then
                close(device, iostat=stat, err=999)
#if defined GLNX
                call rename(filePart, fileName, stat)
#endif
            end if
        end if

        999 call terminate(stat, 'Error in writing '//fileName)
        if (monolithic) call log_comment('Successfully wrote '//fileName)

        isDirtyData = .true. ! allow writing to backup

    end subroutine college_details_write


   subroutine college_details_read(pathToFile)

        character(len=*), intent(in) :: pathToFile

        type(TYPE_COLLEGE) :: wrkCollege
        integer :: idx, tdx, lval, stat

        ! find ROOT_COLLEGES
        call xml_read_file(unitXML, ROOT_COLLEGES, pathToFile, stat)
        call terminate(stat, 'Error: "'//ROOT_COLLEGES//'" not found in '//pathToFile)

        ! examine the file line by line
        do
            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit

            select case (trim(tag))

                case (FSLASH//ROOT_COLLEGES)
                    exit

                case ('College') ! initialize temporary college data
                    call initialize_college (wrkCollege)

                case ('Code')
                    wrkCollege%Code = adjustl(value)

                case ('Name')
                    wrkCollege%Name = adjustl(value)

                case ('Dean')
                    wrkCollege%Dean = adjustl(value)

                case ('TranscriptPreparer')
                    wrkCollege%TranscriptPreparer = adjustl(value)

                case ('TranscriptChecker')
                    wrkCollege%TranscriptChecker = adjustl(value)

                case ('isAllowed')
                    read(value,*) idx, tdx, lval
                    wrkCollege%isAllowed(idx,tdx) = lval/= 0

                case ('/College') ! add temporary college data to College()
                    if (index(wrkCollege%Code,trim(UniversityCode))>0) cycle ! add later
                    idx = index_to_college(wrkCollege%Code)
                    if (idx==0) then ! add
                        NumColleges = NumColleges + 1
                        call check_array_bound (NumColleges, MAX_ALL_COLLEGES, 'MAX_ALL_COLLEGES')
                        idx = NumColleges
                    end if
                    College(idx) = wrkCollege

                case default ! do nothing

            end select

        end do
        close(unitXML)

    end subroutine college_details_read



!===========================================================
! routines for Department()
!===========================================================



    subroutine departments_index_read(device, path)
        integer, intent (in) :: device
        character (len=*), intent (in) :: path

        integer :: stat

        open(unit=device, file=trim(path)//'index', iostat=stat)
        call terminate(stat, 'Error '//trim(itoa(stat))//' in opening index for '//path)
        do
            read(device, AFORMAT, iostat=stat) line
            if (stat<0) exit
            call department_details_read(trim(path)//trim(line)//dotXML)
        end do
        close(device)

    end subroutine departments_index_read


    subroutine department_details_write(device, path, first, last)
        character (len=*), intent (in) :: path
        integer, intent (in) :: device, first
        integer, intent (in), optional :: last

        integer :: ldx, lastIdx, tab0, tab1, tab2, stat
        logical :: monolithic!, fileExists
        character (len=MAX_LEN_FILE_PATH) :: fileName, filePart

        if (isReadOnly) return

        if (present(last)) then
            lastIdx = last
            tab0 = INDENT_INCR
        else
            lastIdx = first
            tab0 = 0
        end if
        monolithic = lastIdx>first

        tab1 = tab0+INDENT_INCR
        tab2 = tab0+INDENT_INCR*2

        stat = 0 ! error status
        if (monolithic) then ! single file

            if (len_trim(path)>0) then
                fileName = path
#if defined GLNX
                filePart = trim(fileName)//dotPART
#else
                filePart = fileName
#endif
                open(unit=device, file=filePart, iostat=stat, err=999)

                ! write the index only
                if (index(path, DIRSEP//'index')>0) then
                    write(device,AFORMAT, iostat=stat, err=999) &
                        ( trim(filename_from(Department(ldx)%Code)), ldx=first,lastIdx )
                    close(device, iostat=stat, err=999)
#if defined GLNX
                    call rename(filePart, fileName, stat)
#endif
                    goto 999
                end if

                call xml_write_character(device, 0, XML_DOC)
            else
                fileName = ' departments to backup file'
            end if

            call xml_write_character(device, 0, ROOT_DEPARTMENTS)

        end if

        do ldx=first,lastIdx

            if (.not. monolithic) then

                fileName = trim(path)//trim(filename_from(Department(ldx)%Code))//dotXML
#if defined GLNX
                filePart = trim(fileName)//dotPART
#else
                filePart = fileName
#endif
                open(unit=device, file=filePart, iostat=stat, err=999)
                call xml_write_character(device, 0, XML_DOC)
                call xml_write_character(device, 0, ROOT_DEPARTMENTS)

            end if

            call xml_write_character(device, tab0, 'Department')
            call xml_write_character(device, tab1, 'Code', Department(ldx)%Code)
            call xml_write_character(device, tab1, 'Name', Department(ldx)%Name)
            call xml_write_character(device, tab1, 'College', College(Department(ldx)%CollegeIdx)%Code)
            if (Department(ldx)%SectionPrefix/=SPACE) &
                call xml_write_character(device, tab1, 'SectionPrefix', Department(ldx)%SectionPrefix)
            call xml_write_character(device, tab0, '/Department')

            if (.not. monolithic) then
                call xml_write_character(device, 0, FSLASH//ROOT_DEPARTMENTS)
                close(device, iostat=stat, err=999)
#if defined GLNX
                call rename(filePart, fileName, stat)
#endif
                if (stat/=0) goto 999
            end if

        end do

        if (monolithic) then
            call xml_write_character(device, 0, FSLASH//ROOT_DEPARTMENTS)
            if (len_trim(path)>0) then
                close(device, iostat=stat, err=999)
#if defined GLNX
                call rename(filePart, fileName, stat)
#endif
            end if
        end if

        999 call terminate(stat, 'Error in writing '//fileName)
        if (monolithic) call log_comment('Successfully wrote '//fileName)

        isDirtyData = .true. ! allow writing to backup

    end subroutine department_details_write



    subroutine department_details_read(pathToFile)

        character(len=*), intent(in) :: pathToFile

        integer :: idx, stat
        type(type_DEPARTMENT) :: wrkDepartment
        character (len=MAX_LEN_COLLEGE_CODE) :: tColl

        ! find ROOT_DEPARTMENTS
        call xml_read_file(unitXML, ROOT_DEPARTMENTS, pathToFile, stat)
        call terminate(stat, 'Error: "'//ROOT_DEPARTMENTS//'" not found in '//pathToFile)

        ! examine the file line by line
        do

            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit

            select case (trim(tag))

                case (FSLASH//ROOT_DEPARTMENTS)
                    exit

                case ('Department') ! initialize temporary department data
                    call initialize_department (wrkDepartment)

                case ('Code')
                    wrkDepartment%Code = adjustl(value)

                case ('Name')
                    wrkDepartment%Name = adjustl(value)

                case ('College')
                    tColl = adjustl(value)
                    idx = index_to_college(tColl)
                    if (idx==0) idx = NumColleges ! use SYSAD for invalid college code
                    wrkDepartment%CollegeIdx = idx

                case ('SectionPrefix')
                    wrkDepartment%SectionPrefix = adjustl(value)

                case ('/Department') ! add temporary department data to Department()
                    if (index(wrkDepartment%Code,trim(SYSAD))>0) cycle ! add at the end
                    idx = index_to_dept(wrkDepartment%Code)
                    if (idx==0) then ! add
                        NumDepartments = NumDepartments + 1
                        call check_array_bound (NumDepartments, MAX_ALL_DEPARTMENTS, 'MAX_ALL_DEPARTMENTS')
                        idx = NumDepartments
                    end if
                    Department(idx) = wrkDepartment

                case default
                   ! do nothing

            end select

        end do

        close(unitXML)

    end subroutine department_details_read


!===========================================================
! routines for rooms
!===========================================================



    subroutine rooms_index_read(device, path)
        integer, intent (in) :: device
        character (len=*), intent (in) :: path

        integer :: stat

        open(unit=device, file=trim(path)//'index', iostat=stat)
        call terminate(stat, 'Error '//trim(itoa(stat))//' in opening index for '//path)
        do
            read(device, AFORMAT, iostat=stat) line
            if (stat<0) exit
            call room_details_read(trim(path)//trim(line)//dotXML)
        end do
        close(device)

    end subroutine rooms_index_read


    subroutine room_details_write(device, path, first, last)
        character (len=*), intent (in) :: path
        integer, intent (in) :: device, first
        integer, intent (in), optional :: last

        integer :: ldx, lastIdx, tab0, tab1, tab2, stat
        logical :: monolithic!, fileExists
        character (len=MAX_LEN_FILE_PATH) :: fileName, filePart

        if (isReadOnly) return

        if (present(last)) then
            lastIdx = last
            tab0 = INDENT_INCR
        else
            lastIdx = first
            tab0 = 0
        end if
        monolithic = lastIdx>first

        tab1 = tab0+INDENT_INCR
        tab2 = tab0+INDENT_INCR*2

        stat = 0 ! error status
        if (monolithic) then ! single file

            if (len_trim(path)>0) then
                fileName = path
#if defined GLNX
                filePart = trim(fileName)//dotPART
#else
                filePart = fileName
#endif
                open(unit=device, file=filePart, iostat=stat, err=999)

                ! write the index only
                if (index(path, DIRSEP//'index')>0) then
                    write(device,AFORMAT, iostat=stat, err=999) &
                        ( trim(filename_from(Room(ldx)%Code)), ldx=first,lastIdx )
                    close(device, iostat=stat, err=999)
#if defined GLNX
                    call rename(filePart, fileName, stat)
#endif
                    goto 999
                end if

                call xml_write_character(device, 0, XML_DOC)
            else
                fileName = 'rooms to backup file'
            end if

            call xml_write_character(device, 0, ROOT_ROOMS)

        end if

        do ldx=first,lastIdx

            if (trim(Room(ldx)%Code)==SPACE) cycle

            if (.not. monolithic) then

                fileName = trim(path)//trim(filename_from(Room(ldx)%Code))//dotXML
#if defined GLNX
                filePart = trim(fileName)//dotPART
#else
                filePart = fileName
#endif
                open(unit=device, file=filePart, iostat=stat, err=999)
                call xml_write_character(device, 0, XML_DOC)
                call xml_write_character(device, 0, ROOT_ROOMS)

            end if

            call xml_write_character(device, tab0, 'Room')
            call xml_write_character(device, tab1, 'Code', Room(ldx)%Code)
            call xml_write_character(device, tab1, 'Department', Department(Room(ldx)%DeptIdx)%Code)
            call xml_write_integer(device, tab1, 'Cluster', Room(ldx)%Cluster)
            call xml_write_integer(device, tab1, 'MaxCapacity', Room(ldx)%MaxCapacity)
            call xml_write_integer(device, tab1, 'EnergyFee', Room(ldx)%EnergyFee)
            call xml_write_character(device, tab0, '/Room')

            if (.not. monolithic) then
                call xml_write_character(device, 0, FSLASH//ROOT_ROOMS)
                close(device, iostat=stat, err=999)
#if defined GLNX
                call rename(filePart, fileName, stat)
#endif
                if (stat/=0) goto 999
            end if

        end do

        if (monolithic) then
            call xml_write_character(device, 0, FSLASH//ROOT_ROOMS)
            if (len_trim(path)>0) then
                close(device, iostat=stat, err=999)
#if defined GLNX
                call rename(filePart, fileName, stat)
#endif
            end if
        end if

        999 call terminate(stat, 'Error in writing '//fileName)
        if (monolithic) call log_comment('Successfully wrote '//fileName)

        isDirtyData = .true. ! allow writing to backup

    end subroutine room_details_write


    subroutine room_details_read(pathToFile)

        character(len=*), intent(in) :: pathToFile

        integer :: idx, stat
        type(TYPE_ROOM) :: wrkRoom
        character (len=MAX_LEN_DEPARTMENT_CODE) :: tDept

        ! open file, return on any error
        call xml_read_file(unitXML, ROOT_ROOMS, pathToFile, stat)
        if (stat/=0) return

        ! examine the file line by line
        do

            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit

            select case (trim(tag))

                case (FSLASH//ROOT_ROOMS)
                    exit

                case ('Room') ! initialize temporary room data
                    call initialize_room (wrkRoom)

                case ('Code')
                    wrkRoom%Code = adjustl(value)

                case ('Department')
                    tDept = adjustl(value)
                    idx = index_to_dept(tDept)
                    if (idx==0) idx = NumDepartments ! use SYSAD if tDept not in list of departments
                    wrkRoom%DeptIdx = idx

                case ('Cluster')
                    wrkRoom%Cluster = atoi(value)

                case ('MaxCapacity')
                    wrkRoom%MaxCapacity = atoi(value)

                case ('EnergyFee')
                    wrkRoom%EnergyFee = atoi(value)

                case ('/Room') ! add temporary room data to Room()
                    if (len_trim(wrkRoom%Code)==0) cycle
                    idx = index_to_room(wrkRoom%Code)
                    if (idx==0) then ! add
                        NumRooms = NumRooms + 1
                        call check_array_bound (NumRooms, MAX_ALL_ROOMS, 'MAX_ALL_ROOMS')
                        idx = NumRooms
                    end if
                    Room(idx) = wrkRoom
                    Department(wrkRoom%DeptIdx)%hasInfo = .true.

                case default
                ! do nothing

            end select

        end do
        close(unitXML)

    end subroutine room_details_read


!===========================================================
! routines for teachers
!===========================================================


    subroutine teachers_index_read(device, path)
        integer, intent (in) :: device
        character (len=*), intent (in) :: path

        integer :: stat

        open(unit=device, file=trim(path)//'index', iostat=stat)
        call terminate(stat, 'Error '//trim(itoa(stat))//' in opening index for '//path)
        do
            read(device, AFORMAT, iostat=stat) line
            if (stat<0) exit
            call teacher_details_read(trim(path)//trim(line)//dotXML)
        end do
        close(device)

    end subroutine teachers_index_read


    subroutine teacher_details_write(device, path, first, last)
        character (len=*), intent (in) :: path
        integer, intent (in) :: device, first
        integer, intent (in), optional :: last

        integer :: ldx, lastIdx, tab0, tab1, tab2, stat
        logical :: monolithic!, fileExists
        character (len=MAX_LEN_FILE_PATH) :: fileName, filePart

        stat = 0 ! error status
        if (isReadOnly) return

        if (present(last)) then
            lastIdx = last
            tab0 = INDENT_INCR
        else
            lastIdx = first
            tab0 = 0
        end if
        monolithic = lastIdx>first

        tab1 = tab0+INDENT_INCR
        tab2 = tab0+INDENT_INCR*2

        stat = 0 ! error status
        if (monolithic) then ! single file

            if (len_trim(path)>0) then
                fileName = path
#if defined GLNX
                filePart = trim(fileName)//dotPART
#else
                filePart = fileName
#endif
                open(unit=device, file=filePart, iostat=stat, err=999)

                ! write the index only
                if (index(path, DIRSEP//'index')>0) then
                    do ldx=first,lastIdx
                        if (trim(Teacher(ldx)%TeacherId)==SPACE .or. trim(Teacher(ldx)%TeacherId)==trim(GUEST) .or. &
                            Teacher(ldx)%DeptIdx==0) cycle
                        write(device,AFORMAT, iostat=stat, err=999) trim(filename_from(Teacher(ldx)%TeacherId) )
                    end do
                    close(device, iostat=stat, err=999)
#if defined GLNX
                    call rename(filePart, fileName, stat)
#endif
                    goto 999
                end if

                call xml_write_character(device, 0, XML_DOC)

            else
                fileName = 'teachers to backup file'
            end if

            call xml_write_character(device, 0, ROOT_TEACHERS)

        end if

        do ldx=first,lastIdx

            if (trim(Teacher(ldx)%TeacherId)==SPACE .or. trim(Teacher(ldx)%TeacherId)==trim(GUEST) .or. &
                Teacher(ldx)%DeptIdx==0) cycle

            if (.not. monolithic) then

                fileName = trim(path)//trim(filename_from(Teacher(ldx)%TeacherId))//dotXML
#if defined GLNX
                filePart = trim(fileName)//dotPART
#else
                filePart = fileName
#endif
                open(unit=device, file=filePart, iostat=stat, err=999)
                call xml_write_character(device, 0, XML_DOC)
                call xml_write_character(device, 0, ROOT_TEACHERS)

            end if

            call xml_write_character(device, tab0, 'Teacher')
            call xml_write_character(device, tab1, 'TeacherId', Teacher(ldx)%TeacherId)
            call xml_write_character(device, tab1, 'Name', Teacher(ldx)%Name)
            call xml_write_character(device, tab1, 'Department', Department(Teacher(ldx)%DeptIdx)%Code)
            if (Teacher(ldx)%MaxLoad>0) &
                call xml_write_integer(device, tab1, 'MaxLoad', Teacher(ldx)%MaxLoad)
            if (Teacher(ldx)%Rank>0) &
                call xml_write_character(device, tab1, 'Rank', AcademicRank(Teacher(ldx)%Rank))
            if (Teacher(ldx)%Step>0) &
                call xml_write_character(device, tab1, 'Step', RankStep(Teacher(ldx)%Step))
            if (Teacher(ldx)%Bachelor/=SPACE) &
                call xml_write_character(device, tab1, 'Bachelor', Teacher(ldx)%Bachelor)
            if (Teacher(ldx)%Master/=SPACE) &
                call xml_write_character(device, tab1, 'Master', Teacher(ldx)%Master)
            if (Teacher(ldx)%Doctorate/=SPACE)  &
                call xml_write_character(device, tab1, 'Doctorate', Teacher(ldx)%Doctorate)
            if (Teacher(ldx)%Specialization/=SPACE)  &
                call xml_write_character(device, tab1, 'Specialization', Teacher(ldx)%Specialization)
            call xml_write_character(device, tab1, 'Password', Teacher(ldx)%Password)
            if (.not. monolithic) call xml_write_character(device, tab1, 'Key', passwordEncryptionKey)
            if (Teacher(ldx)%Role/=SPACE)  &
                call xml_write_character(device, tab1, 'Role', Teacher(ldx)%Role)
            call xml_write_character(device, tab0, '/Teacher')

            if (.not. monolithic) then
                call xml_write_character(device, 0, FSLASH//ROOT_TEACHERS)
                close(device, iostat=stat, err=999)
#if defined GLNX
                call rename(filePart, fileName, stat)
#endif
                if (stat/=0) return
            end if

        end do

        if (monolithic) then
            call xml_write_character(device, 0, FSLASH//ROOT_TEACHERS)
            if (len_trim(path)>0) then
                close(device, iostat=stat, err=999)
#if defined GLNX
                call rename(filePart, fileName, stat)
#endif
            end if
        end if

        999 call terminate(stat, 'Error in writing '//fileName)
        if (monolithic) call log_comment('Successfully wrote '//fileName)

        isDirtyData = .true. ! allow writing to backup

    end subroutine teacher_details_write


    subroutine teacher_details_read(pathToFile)

        character(len=*), intent(in) :: pathToFile

        integer :: i, j, stat
        type(TYPE_TEACHER) :: wrkTeacher
        character (len=MAX_LEN_DEPARTMENT_CODE) :: tDept
        character (len=MAX_LEN_STEP_IN_RANK) :: tStep
        character (len=MAX_LEN_ACADEMIC_RANK) :: tRank
        character(len=lenPasswordEncryptionKey) :: tKey
        character (len=MAX_LEN_PASSWD_VAR) :: Password

        ! open file, return on any error
        call xml_read_file(unitXML, ROOT_TEACHERS, pathToFile, stat)
        if (stat/=0) return

        ! examine the file line by line
        do

            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit

            select case (trim(tag))

                case (FSLASH//ROOT_TEACHERS)
                    exit

                case ('Teacher') ! initialize temporary teacher data
                    call initialize_teacher(wrkTeacher)
                    tKey = passwordEncryptionKey

                case ('TeacherId')
                    wrkTeacher%TeacherId = adjustl(value)

                case ('Name')
                    wrkTeacher%Name = adjustl(value)
                    call upper_case(wrkTeacher%Name)

                case ('Department')
                    tDept = adjustl(value)
                    j = index_to_dept(tDept)
                    if (j==0) j = NumDepartments ! use SYSAD for invalid department code
                    wrkTeacher%DeptIdx = j

                ! read additional info here in case file has the old format
                case ('MaxLoad')
                    wrkTeacher%MaxLoad = atoi(value)

                case ('Rank')
                    tRank = adjustl(value)
                    j = 0
                    do i=1,4
                        if (tRank/=AcademicRank(i)) cycle
                        j = i
                        exit
                    end do
                    wrkTeacher%Rank = j

                case ('Step')
                    tStep = adjustl(value)
                    j = 0
                    do i=1,12
                        if (tStep/=RankStep(i)) cycle
                        j = i
                        exit
                    end do
                    wrkTeacher%Step = j

                case ('Bachelor')
                    wrkTeacher%Bachelor = adjustl(value)

                case ('Master')
                    wrkTeacher%Master = adjustl(value)

                case ('Doctorate')
                    wrkTeacher%Doctorate = adjustl(value)

                case ('Specialization')
                    wrkTeacher%Specialization = adjustl(value)

                case ('Password')
                    wrkTeacher%Password = adjustl(value)

                case ('Key')
                    tKey = adjustl(value)

                case ('Role')
                    wrkTeacher%Role = adjustl(value)
                    i = -1
                    do j=0,MAX_ALL_ROLES
                        if (wrkTeacher%Role/=txtRole(j)) cycle
                        i = j
                        exit
                    end do
                    if (i/=-1) then
                        wrkTeacher%Role = txtRole(i)
                    else
                        wrkTeacher%Role = GUEST
                    end if

                case ('/Teacher') ! add/merge temporary teacher data to Teacher()
                    if (len_trim(wrkTeacher%TeacherId)==0) cycle
                    if (trim(wrkTeacher%TeacherId)==trim(GUEST) ) cycle ! Guest already in
                    if (len_trim(wrkTeacher%Name)==0) cycle ! no name
                    ! teacher encountered previously?
                    i = 0
                    do j=1,NumTeachers
                        if (wrkTeacher%TeacherId/=Teacher(j)%TeacherId) cycle
                        i = j
                        exit
                    end do
                    if (i==0) then
                        NumTeachers = NumTeachers + 1
                        call check_array_bound (NumTeachers, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')
                        i = NumTeachers
                    end if
                    Teacher(i) = wrkTeacher
                    if (isPassword_of_teacher(i, GUEST) .or. len_trim(Teacher(i)%Password)==0) then
                        call set_password(Teacher(i)%Password, trim(Teacher(i)%TeacherId))
                    else if (tKey/=passwordEncryptionKey) then
                        Password = Teacher(i)%Password
                        call decrypt(tKey, Password)
                        Password = Password(lenPasswordEncryptionKey-atoi(Password(3:4))+1:)
                        call set_password(Teacher(i)%Password, Password)
                    end if

                    Department(Teacher(i)%DeptIdx)%hasInfo = .true.
                case default
                    ! do nothing
            end select

        end do

        close(unitXML)

    end subroutine teacher_details_read



!===========================================================
! routines for subjects
!===========================================================


    subroutine subject_codes_read(pathToFile)

        character(len=*), intent(in) :: pathToFile

        integer :: i, j, stat
        type(TYPE_SUBJECT) :: wrkSubject
        character (len=MAX_LEN_DEPARTMENT_CODE) :: tDept

        ! find ROOT_SUBJECTS
        call xml_read_file(unitXML, ROOT_SUBJECTS, pathToFile, stat)
        call terminate(stat, 'Error: "'//ROOT_SUBJECTS//'" not found in '//pathToFile)

        ! read subject code & responsible department
        do
            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit

            select case (trim(tag))

                case (FSLASH//ROOT_SUBJECTS)
                    exit

                case ('Subject') ! initialize temporary subject data
                    call initialize_subject (wrkSubject)

                case ('Code')
                    wrkSubject%Name = adjustl(value)
                    call upper_case(wrkSubject%Name)

                case ('Department')
                    tDept = adjustl(value)
                    j = index_to_dept(tDept)
                    if (j==0) j = NumDepartments ! use SYSAD for invalid department code
                    wrkSubject%DeptIdx = j

                case ('/Subject') ! add temporary subject data to Subject()
                    ! no subject code ?
                    if (trim(wrkSubject%Name)==SPACE) cycle
                    ! duplicate subject ?
                    i = index_to_subject(wrkSubject%Name)
                    if (i/=0) cycle ! duplicate

                    ! not a dummy subject
                    Department(wrkSubject%DeptIdx)%hasInfo = .true.

                    if (wrkSubject%DeptIdx>1) then
                        NumSubjects = NumSubjects + 1
                        call check_array_bound (NumSubjects, MAX_ALL_SUBJECTS, 'MAX_ALL_SUBJECTS')
                        i = NumSubjects
                        do while (wrkSubject%Name<Subject(i-1)%Name)
                            Subject(i)%Name = Subject(i-1)%Name
                            i = i-1
                        end do
                        Subject(i)%Name = wrkSubject%Name

                    elseif (wrkSubject%DeptIdx==1) then ! dummy subject
                        NumDummySubjects = NumDummySubjects-1
                        Subject(NumDummySubjects)%Name = wrkSubject%Name

                    end if

                case default
                ! do nothing

            end select

        end do
        close(unitXML)

        ! fix INDEX_TO_NONE
        wrkSubject%Name = 'NONE'
        INDEX_TO_NONE = index_to_subject(wrkSubject%Name)
        Subject(:)%Prerequisite(1) = INDEX_TO_NONE
        Subject(:)%Corequisite(1) = INDEX_TO_NONE
        Subject(:)%Concurrent(1) = INDEX_TO_NONE
        Subject(:)%ConcPrerequisite(1) = INDEX_TO_NONE

    end subroutine subject_codes_read


    subroutine subject_details_read(pathToFile)

        character(len=*), intent(in) :: pathToFile

        integer :: i, j, k, l, stat
        type(TYPE_SUBJECT) :: wrkSubject
        character (len=MAX_LEN_DEPARTMENT_CODE) :: tDept

        ! read the rest of subject basic info
        call xml_read_file(unitXML, ROOT_SUBJECTS, pathToFile, stat)
        call terminate(stat, 'Error: "'//ROOT_SUBJECTS//'" not found in '//pathToFile)
        do
            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit

            select case (trim(tag))

                case (FSLASH//ROOT_SUBJECTS)
                    exit

                case ('Subject') ! initialize temporary subject data
                    call initialize_subject (wrkSubject)

                case ('Code')
                    wrkSubject%Name = adjustl(value)
                    call upper_case(wrkSubject%Name)

                case ('Title')
                    wrkSubject%Title = adjustl(value)

                case ('Department')
                    tDept = adjustl(value)
                    j = index_to_dept(tDept)
                    if (j==0) j = NumDepartments ! use SYSAD for invalid department code
                    wrkSubject%DeptIdx = j

                case ('Units')
                    wrkSubject%Units = floor(100.0*atof(value))/100.0

                case ('Enrollment')
                    call index_to_delimiters(COMMA, value, ndels, pos)
                    do k=1,6,2 !  1st term, 2nd term, summer
                        l = (k+1)/2
                        i = atoi(value(pos(k)+1:pos(k+1)-1)) ! pass
                        j = atoi(value(pos(k+1)+1:pos(k+2)-1)) ! fail
                        wrkSubject%GrandTotal(1,l) = i
                        wrkSubject%GrandTotal(2,l) = j
                        wrkSubject%Failrate(l) = (1.0*j)/max(1,i+j)
                    end do

                case ('TermOffered')
                    wrkSubject%TermOffered = 0
                    if (index(value, '1')>0) wrkSubject%TermOffered = wrkSubject%TermOffered+1
                    if (index(value, '2')>0) wrkSubject%TermOffered = wrkSubject%TermOffered+2
                    if (index(value, 'S')>0) wrkSubject%TermOffered = wrkSubject%TermOffered+4

                case ('LectHours')
                    wrkSubject%LectHours = atof(value)

                case ('MinLectSize')
                    wrkSubject%MinLectSize = atoi(value)

                case ('MaxLectSize')
                    wrkSubject%MaxLectSize = atoi(value)

                case ('LabHours')
                    wrkSubject%LabHours = atof(value)

                case ('MinLabSize')
                    wrkSubject%MinLabSize = atoi(value)

                case ('MaxLabSize')
                    wrkSubject%MaxLabSize = atoi(value)

                ! read the additional info also, in case the file has the old format
                case ('LabFee')
                    wrkSubject%LabFee = atof(value)

                case ('Tuition')
                    wrkSubject%Tuition = atof(value)

                case ('LectLoad')
                    wrkSubject%LectLoad = atof(value)

                case ('LabLoad')
                    wrkSubject%LabLoad = atof(value)

                case ('Prerequisite')
                    call tokenize_subjects(value, '+', MAX_ALL_SUBJECT_PREREQ, &
                        wrkSubject%lenPreq, wrkSubject%Prerequisite, stat)
                    if (stat>0) exit

                case ('Corequisite')
                    call tokenize_subjects(value, '+', MAX_ALL_SUBJECT_COREQ, &
                        wrkSubject%lenCoreq, wrkSubject%Corequisite, stat)
                    if (stat>0) exit
                    if (wrkSubject%lenCoreq>0 .and. Subject(wrkSubject%Corequisite(1))%Name/='NONE') then
                        call log_comment(trim(wrkSubject%Name)//' has co-requisite '// &
                            Subject(wrkSubject%Corequisite(1))%Name ) ! , j=1,wrkSubject%lenCoreq)
                    end if

                case ('ConcurrentWith')
                    call tokenize_subjects(value, '+', MAX_ALL_SUBJECT_CONCURRENT, wrkSubject%lenConc, wrkSubject%Concurrent, stat)
                    if (stat>0) exit
                    if (wrkSubject%lenConc>0 .and. Subject(wrkSubject%Concurrent(1))%Name/='NONE') then
                        call log_comment(trim(wrkSubject%Name)//' is concurrent with '// &
                            Subject(wrkSubject%Concurrent(1))%Name ) !, j=1,wrkSubject%lenConc)
                    end if

                case ('ConcurrentPrerequisite')
                    call tokenize_subjects(value, '+', MAX_ALL_SUBJECT_CONCPREQ, wrkSubject%lenConcPreq, &
                        wrkSubject%ConcPrerequisite, stat)
                    if (stat>0) exit
                    if (wrkSubject%lenConcPreq>0 .and. Subject(wrkSubject%ConcPrerequisite(1))%Name/='NONE') then
                        call log_comment(trim(wrkSubject%Name)//' has pre-req that can be concurrent : '// &
                           Subject(wrkSubject%ConcPrerequisite(1))%Name) ! , j=1,wrkSubject%lenConcPreq)
                    end if

                case ('/Subject') ! add temporary subject data to Subject()
                    i = index_to_subject(wrkSubject%Name)
                    Subject(i) = wrkSubject

                case default
                    ! do nothing

            end select

        end do

        close(unitXML)

    end subroutine subject_details_read



    subroutine subjects_index_read(device, path)
        integer, intent (in) :: device
        character (len=*), intent (in) :: path

        integer :: stat

        open(unit=device, file=trim(path)//'index', iostat=stat, err=999)
        ! read subject codes first
        do
            read(device, AFORMAT, iostat=stat) line
            if (stat<0) exit
            call subject_codes_read(trim(path)//trim(line)//dotXML)
        end do

        ! read subject details
        rewind(device, iostat=stat, err=999)
        do
            read(device, AFORMAT, iostat=stat) line
            if (stat<0) exit
            call subject_details_read(trim(path)//trim(line)//dotXML)
        end do

        close(device)
        stat = 0

        999 call terminate(stat, 'Error '//trim(itoa(stat))//' in reading index of  '//path)

    end subroutine subjects_index_read



    subroutine subject_details_write(device, path, first, last)
        character (len=*), intent (in) :: path
        integer, intent (in) :: device, first
        integer, intent (in), optional :: last

        integer :: lastIdx, tab0, tab1, tab2, stat
        logical :: monolithic!, fileExists
        character (len=MAX_LEN_FILE_PATH) :: fileName, filePart
        integer :: subj, i, j
        character(len=255) :: mesg1, mesg2, mesg3, mesg4, errMesg

        if (isReadOnly) return

        if (present(last)) then
            lastIdx = last
            tab0 = INDENT_INCR
        else
            lastIdx = first
            tab0 = 0
        end if

        monolithic = lastIdx>first

        tab1 = tab0+INDENT_INCR
        tab2 = tab0+INDENT_INCR*2

        stat = 0 ! error status
        errMesg = SPACE
        if (monolithic) then ! single file

            if (len_trim(path)>0) then
                fileName = path
#if defined GLNX
                filePart = trim(fileName)//dotPART
#else
                filePart = fileName
#endif
                errMesg = 'open '//trim(filePart)//'; '//errMesg
                open(unit=device, file=filePart, iostat=stat, err=999)

                ! write the index only
                if (index(path, DIRSEP//'index')>0) then
                    errMesg = 'write subject codes; '//errMesg
                    write(device,AFORMAT, iostat=stat, err=999) &
                        ( trim(filename_from(Subject(subj)%Name)), subj=first,-2 ), &
                        ( trim(filename_from(Subject(subj)%Name)), subj=1,lastIdx )
                    errMesg = 'close; '//errMesg
                    close(device, iostat=stat, err=999)
                    errMesg = 'rename; '//errmEsg
#if defined GLNX
                    call rename(filePart, fileName, stat)
#endif
                    goto 999
                end if

                call xml_write_character(device, 0, XML_DOC)

            else
                fileName = 'subjects to backup file'
            end if

            call xml_write_character(device, 0, ROOT_SUBJECTS)

        end if

        do subj=first,lastIdx

            if (subj==0 .or. subj==-1) cycle

            if (.not. monolithic) then

                fileName = trim(path)//trim(filename_from(Subject(subj)%Name))//dotXML
#if defined GLNX
                filePart = trim(fileName)//dotPART
#else
                filePart = fileName
#endif
                errMesg = 'open '//trim(filePart)//'; '//errMesg
                open(unit=device, file=filePart, iostat=stat, err=999)
                call xml_write_character(device, 0, XML_DOC)
                call xml_write_character(device, 0, ROOT_SUBJECTS)

            end if

            ! prerequisite
            i = Subject(subj)%Prerequisite(1)
            mesg1 = Subject(i)%Name
            do j=2,Subject(subj)%lenPreq
                i = Subject(subj)%Prerequisite(j)
                mesg1 = trim(mesg1)//'+'//Subject(i)%Name
            end do
            ! corequisite
            i = Subject(subj)%Corequisite(1)
            mesg2 = Subject(i)%Name
            do j=2,Subject(subj)%lenCoreq
                i = Subject(subj)%Corequisite(j)
                mesg2 = trim(mesg2)//'+'//Subject(i)%Name
            end do
            ! concurrent
            i = Subject(subj)%Concurrent(1)
            mesg3 = Subject(i)%Name
            do j=2,Subject(subj)%lenConc
                i = Subject(subj)%Concurrent(j)
                mesg3 = trim(mesg3)//'+'//Subject(i)%Name
            end do
            ! prerequisite that can be taken concurrently
            i = Subject(subj)%ConcPrerequisite(1)
            mesg4 = Subject(i)%Name
            do j=2,Subject(subj)%lenConcPreq
                i = Subject(subj)%ConcPrerequisite(j)
                mesg4 = trim(mesg4)//'+'//Subject(i)%Name
            end do

            call xml_write_character(device, tab0, 'Subject')
            call xml_write_character(device, tab1, 'Code', Subject(subj)%Name)
            call xml_write_character(device, tab1, 'Title', Subject(subj)%Title)
            call xml_write_character(device, tab1, 'Department', Department(Subject(subj)%DeptIdx)%Code)
            call xml_write_character(device, tab1, 'TermOffered', text_term_offered(Subject(subj)%TermOffered))
            if (Subject(subj)%Units/=0.0) call xml_write_float(device, tab1, 'Units', Subject(subj)%Units,2)
            if (Subject(subj)%Units>0.0) call xml_write_character(device, tab1, 'DisplayUnits', ftoa(Subject(subj)%Units,2))
            if (sum(Subject(subj)%GrandTotal(:,:))>0) &
                call xml_write_character(device, tab1, 'Enrollment', &
                    trim(itoa(Subject(subj)%GrandTotal(1,1)))//COMMA// &
                    trim(itoa(Subject(subj)%GrandTotal(2,1)))//COMMA// &
                    trim(itoa(Subject(subj)%GrandTotal(1,2)))//COMMA// &
                    trim(itoa(Subject(subj)%GrandTotal(2,2)))//COMMA// &
                    trim(itoa(Subject(subj)%GrandTotal(1,3)))//COMMA// &
                    trim(itoa(Subject(subj)%GrandTotal(2,3))) )
            if (Subject(subj)%Tuition/=0.0) call xml_write_float(device, tab1, 'Tuition', Subject(subj)%Tuition,2)
            if (Subject(subj)%LabFee/=0.0) call xml_write_float(device, tab1, 'LabFee', Subject(subj)%LabFee,2)
            if (Subject(subj)%LectHours/=0.0) call xml_write_float(device, tab1, 'LectHours', Subject(subj)%LectHours,2)
            if (Subject(subj)%MinLectSize/=0) call xml_write_integer(device, tab1, 'MinLectSize', Subject(subj)%MinLectSize)
            if (Subject(subj)%MaxLectSize/=0) call xml_write_integer(device, tab1, 'MaxLectSize', Subject(subj)%MaxLectSize)
            if (Subject(subj)%LectLoad/=0.0) call xml_write_float(device, tab1, 'LectLoad', Subject(subj)%LectLoad,2)
            if (Subject(subj)%LabHours/=0.0) call xml_write_float(device, tab1, 'LabHours', Subject(subj)%LabHours,2)
            if (Subject(subj)%MinLabSize/=0) call xml_write_integer(device, tab1, 'MinLabSize', Subject(subj)%MinLabSize)
            if (Subject(subj)%MaxLabSize/=0) call xml_write_integer(device, tab1, 'MaxLabSize', Subject(subj)%MaxLabSize)
            if (Subject(subj)%LabLoad/=0.0) call xml_write_float(device, tab1, 'LabLoad', Subject(subj)%LabLoad,2)
            if (trim(mesg1)/='NONE') call xml_write_character(device, tab1, 'Prerequisite', mesg1)
            if (trim(mesg2)/='NONE') call xml_write_character(device, tab1, 'Corequisite', mesg2)
            if (trim(mesg3)/='NONE') call xml_write_character(device, tab1, 'ConcurrentWith', mesg3)
            if (trim(mesg4)/='NONE') call xml_write_character(device, tab1, 'ConcurrentPrerequisite', mesg4)

            call xml_write_character(device, tab0, '/Subject')


            if (.not. monolithic) then
                call xml_write_character(device, 0, FSLASH//ROOT_SUBJECTS)
                errMesg = 'close; '//errmesg
                close(device, iostat=stat, err=999)
                errMesg = 'rename; '//errMesg
#if defined GLNX
                call rename(filePart, fileName, stat)
#endif
                if (stat/=0) goto 999
            end if

        end do

        if (monolithic) then
            call xml_write_character(device, 0, FSLASH//ROOT_SUBJECTS)
            if (len_trim(path)>0) then
                errMesg = 'close monolithic; '//errMesg
                close(device, iostat=stat, err=999)
                errMesg = 'rename monolithic; '//errMesg
#if defined GLNX
                call rename(filePart, fileName, stat)
#endif
            end if
        end if

        999 call terminate(stat, 'Error back trace: '//errMesg)
        if (monolithic) call log_comment('Successfully wrote '//fileName)

        isDirtyData = .true. ! allow writing to backup

    end subroutine subject_details_write



!===========================================================
! routines for curricula
!===========================================================


    subroutine curricula_index_read(device, path)
        integer, intent (in) :: device
        character (len=*), intent (in) :: path

        integer :: stat

        open(unit=device, file=trim(path)//'index', iostat=stat)
        call terminate(stat, 'Error '//trim(itoa(stat))//' in reading index of  '//path)
        do
            read(device, AFORMAT, iostat=stat) line
            if (stat<0) exit
            call curriculum_details_read(trim(path)//trim(line)//dotXML)
        end do
        close(device)

    end subroutine curricula_index_read


    subroutine curriculum_details_write(device, path, first, last)
        character (len=*), intent (in) :: path
        integer, intent (in) :: device, first
        integer, intent (in), optional :: last

        integer :: lastIdx, tab0, tab1, tab2, stat
        logical :: monolithic!, fileExists
        character (len=MAX_LEN_FILE_PATH) :: fileName, filePart

        integer :: idxCOLL, idxCURR, idx, tdx, iYear, iTerm
        character(len=255) :: mesg
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum

        if (isReadOnly) return

        if (present(last)) then
            lastIdx = last
            tab0 = INDENT_INCR
        else
            lastIdx = first
            tab0 = 0
        end if
        monolithic = lastIdx>first

        tab1 = tab0+INDENT_INCR
        tab2 = tab0+INDENT_INCR*2

        stat = 0 ! error status
        if (monolithic) then ! single file

            if (len_trim(path)>0) then
                fileName = path
#if defined GLNX
                filePart = trim(fileName)//dotPART
#else
                filePart = fileName
#endif
                open(unit=device, file=filePart, iostat=stat, err=999)

                ! write the index only
                if (index(path, DIRSEP//'index')>0) then
                    write(device,AFORMAT, iostat=stat, err=999) &
                        ( trim(filename_from(Curriculum(idxCURR)%Code)), idxCURR=first,lastIdx )
                    close(device, iostat=stat, err=999)
#if defined GLNX
                    call rename(filePart, fileName, stat)
#endif
                    goto 999
                end if

                call xml_write_character(device, 0, XML_DOC)

            else
                fileName = 'curricula to backup file'
            end if

            call xml_write_character(device, 0, ROOT_CURRICULA)

        end if

        do idxCURR=first,lastIdx

            tCurriculum = Curriculum(idxCURR)%Code
            if (trim(tCurriculum)==SPACE) cycle
            idxCOLL = Curriculum(idxCURR)%CollegeIdx

            if (.not. monolithic) then

                fileName = trim(path)//trim(filename_from(tCurriculum))//dotXML
#if defined GLNX
                filePart = trim(fileName)//dotPART
#else
                filePart = fileName
#endif
                open(unit=device, file=filePart, iostat=stat, err=999)
                call xml_write_character(device, 0, XML_DOC)
                call xml_write_character(device, 0, ROOT_CURRICULA)

            end if

            call xml_write_character(device, tab0, 'Curriculum')
            call xml_write_character(device, tab1, 'Code', tCurriculum)
            call xml_write_character(device, tab1, 'Title', Curriculum(idxCURR)%Title)

            if (Curriculum(idxCURR)%Specialization/=SPACE) then
                call xml_write_character(device, tab1, 'Specialization', Curriculum(idxCURR)%Specialization)
            end if

            if (Curriculum(idxCURR)%Remark/=SPACE) then
                call xml_write_character(device, tab1, 'Remark', Curriculum(idxCURR)%Remark)
            end if

            if (Curriculum(idxCURR)%Active) then
                mesg = 'ACTIVE'
            else
                mesg = 'INACTIVE'
            end if
            call xml_write_character(device, tab1, 'Status', mesg)

            call xml_write_character(device, tab1, 'College', College(idxCOLL)%Code)

            do tdx=1,Curriculum(idxCURR)%NumTerms
                call rank_to_year_term(tdx, iYear, iTerm)
                mesg = SPACE
                do idx=1,Curriculum(idxCURR)%NSubjects
                    if (INDEX_TO_NONE==Curriculum(idxCURR)%SubjectIdx(idx)) cycle
                    if (Curriculum(idxCURR)%SubjectTerm(idx) == tdx) then
                        mesg = trim(mesg)//COMMA//Subject(Curriculum(idxCURR)%SubjectIdx(idx))%Name
                    end if
                end do
                if (mesg==SPACE) cycle
                call xml_write_character(device, tab1, 'Load')
                call xml_write_character(device, tab2, 'Year', txtYear(iYear))
                call xml_write_character(device, tab2, 'Term', txtSemester(iTerm))
                call xml_write_character(device, tab2, 'Subjects', mesg(2:))
                call xml_write_character(device, tab1, '/Load')
            end do
            do tdx=1,NumSubst
                if (Substitution(SubstIdx(tdx))==idxCURR) then
                    mesg = SPACE
                    do idx=SubstIdx(tdx)+1, SubstIdx(tdx+1)-1
                        mesg = trim(mesg)//COMMA//Subject(Substitution(idx))%Name
                    end do
                    call xml_write_character(device, tab1, 'Substitution', mesg(2:))
                end if
            end do

            call xml_write_character(device, tab0, '/Curriculum')

            if (.not. monolithic) then
                call xml_write_character(device, 0, FSLASH//ROOT_CURRICULA)
                close(device, iostat=stat, err=999)
#if defined GLNX
                call rename(filePart, fileName, stat)
#endif
                if (stat/=0) goto 999
            end if

        end do

        if (monolithic) then
            call xml_write_character(device, 0, FSLASH//ROOT_CURRICULA)
            if (len_trim(path)>0) then
                close(device, iostat=stat, err=999)
#if defined GLNX
                call rename(filePart, fileName, stat)
#endif
            end if
        end if

        999 call terminate(stat, 'Error in writing '//fileName)
        if (monolithic) call log_comment('Successfully wrote '//fileName)

        isDirtyData = .true. ! allow writing to backup

    end subroutine curriculum_details_write


    subroutine curriculum_details_read(pathToFile)

        character(len=*), intent(in) :: pathToFile

        type(TYPE_CURRICULUM) :: tmpCurriculum

        integer :: i, j, k, tdx, iYear, iTerm, stat
        character (len=MAX_LEN_SUBJECT_CODE) :: token
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=MAX_LEN_TEXT_SEMESTER) :: strTerm
        character (len=MAX_LEN_TEXT_YEAR) :: strYear
        integer :: nLoad, loadArray(MAX_SUBJECTS_PER_TERM)

        ! find ROOT_CURRICULA
        call xml_read_file(unitXML, ROOT_CURRICULA, pathToFile, stat)
        call terminate(stat, 'Error: "'//ROOT_CURRICULA//'" not found in '//pathToFile)

        ! examine the file line by line
        do
            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit

            ! get tag and value if any
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit

            select case (trim(tag))

                case (FSLASH//ROOT_CURRICULA)
                    exit

                case ('Curriculum') ! initialize temporary curriculum data
                    tmpCurriculum = Curriculum(0)

                case ('Code')
                    tmpCurriculum%Code = adjustl(value)

                case ('Title')
                    tmpCurriculum%Title = adjustl(value)

                case ('Specialization')
                    tmpCurriculum%Specialization = adjustl(value)

                case ('Remark')
                    tmpCurriculum%Remark = adjustl(value)

                case ('Status') ! value is ACTIVE or INACTIVE
                    tmpCurriculum%Active = trim(value)=='ACTIVE'

                case ('College') ! value is a college code
                    tCollege = value
                    tmpCurriculum%CollegeIdx = index_to_college(tCollege)

                case ('Load') ! value is empty
                    nLoad = 0
                    loadArray = 0
                    iYear = -1
                    iTerm = -1

                case ('Year') ! value is one of FIRST, SECOND, THIRD, FOURTH, ...
                    strYear = adjustl(value)
                    call upper_case(strYear)
                    iYear = index_to_year(strYear)

                case ('Term') ! value is one of FIRST, SECOND, SUMMER
                    strTerm = adjustl(value)
                    call upper_case(strTerm)
                    iTerm = index_to_term(strTerm)

                case ('Subjects') ! value is comma-separated list of subjects
                    call upper_case(value)
                    call tokenize_subjects(value, ',', MAX_SUBJECTS_PER_TERM, nLoad, loadArray, stat)

                case ('/Load') ! value is empty
                    if (iYear>0 .and. iTerm>0) then ! ok
                        ! collect subjects
                        tdx = (iYear-1)*3 + iTerm
                        tmpCurriculum%NumTerms = tdx
                        do k = 1,nLoad
                            if (INDEX_TO_NONE==loadArray(k)) cycle
                            token = Subject(loadArray(k))%Name
                            i = index_to_new_subject(loadArray(k))
                            !if (i/=loadArray(k)) &
                            !    call log_comment (tCurriculum//token//' renamed '//Subject(i)%Name)
                            if ( isSubject_offered(i,iTerm) ) then
                                j = tmpCurriculum%NSubjects+1
                                tmpCurriculum%NSubjects = j
                                tmpCurriculum%SubjectIdx(j) = i
                                tmpCurriculum%SubjectTerm(j) = tdx
                            else
                                call log_comment (token//': subject not offered during '//strTerm//' Term')
                            !return
                            end if
                        end do
                    end if
                    nLoad = 0
                    loadArray = 0
                    iYear = -1
                    iTerm = -1

                case ('/Curriculum') ! add temporary curriculum data to Curriculum()
                    if (trim(tmpCurriculum%Code)=='OTHER') cycle ! ignore OTHER
                    if (len_trim(tmpCurriculum%Code)==0) cycle ! ignore
                    if (tmpCurriculum%CollegeIdx==0) tmpCurriculum%CollegeIdx = NumColleges

                    ! already in list?
                    k = 0
                    do i=1,NumCurricula
                        if (tmpCurriculum%Code/=Curriculum(i)%Code) cycle
                        k = i
                        exit
                    end do
                    if (k==0) then ! add
                        NumCurricula = NumCurricula + 1
                        call check_array_bound (NumCurricula, MAX_ALL_CURRICULA, 'MAX_ALL_CURRICULA')
                        k = NumCurricula
                    end if
                    Curriculum(k) = tmpCurriculum
                    College(tmpCurriculum%CollegeIdx)%hasInfo = .true.

                    ! if active curriculum, check that prerequisites are taken before successor subjects
                    if (tmpCurriculum%Active) then
                        nLoad = tmpCurriculum%NSubjects
                    else
                        nLoad = 0
                        !write(*,*) 'Not active: '//tmpCurriculum%Code
                    end if
                    do k = 1, nLoad
                        i = tmpCurriculum%SubjectIdx(k)
                        if (.not. is_prerequisite_satisfiable_in_curriculum(i,tmpCurriculum)) then
                            token = Subject(i)%Name
                            call rank_to_year_term (tmpCurriculum%SubjectTerm(k), iYear, iTerm)
                            strYear = txtYear(iYear)
                            strTerm = txtSemester(iTerm)
                            call log_comment (trim(tmpCurriculum%Code)//', '// &
                                trim(strYear)//' year, '//trim(strTerm)//' term, '//trim(token)// &
                                ': preq '//trim(text_prerequisite_in_curriculum(i))//' not specified earlier!')
                        end if
                    end do
                case default
                    ! do nothing
            end select

        end do

        close(unitXML)


    end subroutine curriculum_details_read



    subroutine equivalence_data_read()
        !device, path, indexFile)
        !integer, intent (in) :: device
        !character (len=*), intent (in) :: path

        integer :: ptrS, stat

        ptrS = 0 ! substitutions

        ! from curriculum files
        open(unit=unitIDX, file=trim(dirCURRICULA)//'index', iostat=stat)
        call terminate(stat, 'Error '//trim(itoa(stat))//' in opening index for '//dirCURRICULA)
        do
            read(unitIDX, AFORMAT, iostat=stat) line
            if (stat<0) exit
            call equivalencies_by_curriculum(trim(dirCURRICULA)//trim(line)//dotXML, ptrS)
        end do
        close(unitIDX)

        ! from EQUIVALENCIES.XML
        call equivalencies_for_all_curricula(trim(pathToYear)//'EQUIVALENCIES.XML', ptrS)
        SubstIdx(NumSubst+1) = ptrS+1

    end subroutine equivalence_data_read


    subroutine equivalencies_by_curriculum(fileName, ptrS)
        character(len=*), intent(in) :: fileName
        integer, intent (in out) :: ptrS

        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        integer :: idxCURR, i, stat
        integer :: nLoad, loadArray(MAX_SUBJECTS_PER_TERM)

        call xml_read_file(unitXML, ROOT_CURRICULA, fileName, stat)
        call terminate(stat, 'Error: "'//ROOT_CURRICULA//'" not found in '//fileName)
        do
            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit

            ! get tag and value if any
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit

            select case (trim(tag))

                case (FSLASH//ROOT_CURRICULA)
                    exit

                case ('Code')
                    tCurriculum = adjustl(value)
                    idxCURR = index_to_curriculum(tCurriculum)

                case ('Substitution') ! value is comma-separated list of subjects
                    call upper_case(value)
                    call tokenize_subjects(value, ',', MAX_SUBJECTS_PER_TERM, nLoad, loadArray, stat)
                    call check_array_bound (NumSubst+1, MAX_ALL_SUBSTITUTIONS, 'MAX_ALL_SUBSTITUTIONS')
                    call check_array_bound (ptrS+nLoad, MAX_LEN_SUBSTITUTION_ARRAY, 'MAX_LEN_SUBSTITUTION_ARRAY')
                    NumSubst = NumSubst + 1
                    ptrS = ptrS+1
                    SubstIdx(NumSubst) = ptrS
                    Substitution(ptrS) = idxCURR
                    do i=1,nLoad
                        ptrS = ptrS+1
                        Substitution(ptrS) = loadArray(i)
                    end do
                case default
                    ! do nothing
            end select
        end do
        close(unitXML)

    end subroutine equivalencies_by_curriculum


    subroutine equivalencies_for_all_curricula(fileName, ptrS)

        character(len=*), intent(in) :: fileName
        integer, intent (in out) :: ptrS

        integer :: i, stat
        integer :: nLoad, loadArray(MAX_SUBJECTS_PER_TERM)

        ! find ROOT_EQUIVALENCIES
        call xml_read_file(unitXML, ROOT_EQUIVALENCIES, fileName, stat)
        if (stat/=0) return

        ! examine the file line by line
        do
            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit

            ! get tag and value if any
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit

            select case (trim(tag))

                case (FSLASH//ROOT_EQUIVALENCIES)
                    exit

                case ('Equivalence') ! value is comma-separated list of subjects
                    call upper_case(value)
                    call tokenize_subjects(value, ',', MAX_SUBJECTS_PER_TERM, nLoad, loadArray, stat)
                    call check_array_bound (NumSubst+1, MAX_ALL_SUBSTITUTIONS, 'MAX_ALL_SUBSTITUTIONS')
                    call check_array_bound (ptrS+nLoad, MAX_LEN_SUBSTITUTION_ARRAY, 'MAX_LEN_SUBSTITUTION_ARRAY')
                    NumSubst = NumSubst + 1
                    ptrS = ptrS+1
                    SubstIdx(NumSubst) = ptrS
                    Substitution(ptrS) = -1
                    do i=1,nLoad
                        ptrS = ptrS+1
                        Substitution(ptrS) = loadArray(i)
                    end do
                case default
                    ! do nothing
            end select
        end do
        close(unitXML)

    end subroutine equivalencies_for_all_curricula



    subroutine equivalence_data_write(device, equivalenceFile)
        integer, intent(in) :: device
        character (len=*), intent (in) :: equivalenceFile

        integer :: idx, stat, tdx
        character (len=MAX_LEN_FILE_PATH) :: fileName, filePart, mesg

        stat = 0
        if (len_trim(equivalenceFile)>0) then
            fileName = equivalenceFile
#if defined GLNX
            filePart = trim(fileName)//dotPART
#else
            filePart = fileName
#endif
            open(unit=device, file=filePart, iostat=stat, err=999)
            call xml_write_character(device, 0, XML_DOC)
        else
            fileName = ' equivalence rules to backup file'
        end if

        call xml_write_character(device, indent0, ROOT_EQUIVALENCIES)

        do tdx=1,NumSubst
            if (Substitution(SubstIdx(tdx))==-1) then
                mesg = SPACE
                do idx=SubstIdx(tdx)+1, SubstIdx(tdx+1)-1
                    mesg = trim(mesg)//COMMA//Subject(Substitution(idx))%Name
                end do
                call xml_write_character(device, indent1, 'Equivalence', mesg(2:))
            end if
        end do

        call xml_write_character(device, indent0, FSLASH//ROOT_EQUIVALENCIES)

        if (len_trim(equivalenceFile)>0) then
            close(device, iostat=stat, err=999)
#if defined GLNX
            call rename(filePart, fileName, stat)
#endif
        end if

        999 call terminate(stat, 'Error in writing '//filename)
        call log_comment('Successfully wrote '//fileName)

        isDirtyData = .true. ! allow writing to backup

    end subroutine equivalence_data_write


!===========================================================
! routines for sections
!===========================================================

    subroutine classes_index_read(device, thisTerm, path, keepDuplicates)

        integer, intent (in) :: device, thisTerm
        character (len=*), intent (in) :: path
        logical, intent(in), optional :: keepDuplicates

        integer :: stat

        open(unit=device, file=trim(path)//'index', iostat=stat)
        if (stat/=0) then
            call log_comment('Error '//trim(itoa(stat))//' in opening index for '//path)
            return
        end if
        if (present(keepDuplicates)) then
            do
                read(device, AFORMAT, iostat=stat) line
                if (stat<0) exit
                call class_details_read(trim(path)//trim(line)//dotXML, thisTerm, .true.)
            end do
        else
            do
                read(device, AFORMAT, iostat=stat) line
                if (stat<0) exit
                call class_details_read(trim(path)//trim(line)//dotXML, thisTerm)
            end do
        end if
        close(device)

    end subroutine classes_index_read



    subroutine class_details_write(device, thisTerm, path, first, last)
        character (len=*), intent (in) :: path
        integer, intent (in) :: device, thisTerm, first
        integer, intent (in), optional :: last

        integer :: iSect, mdx, subj, lastIdx, tab0, tab1, tab2, stat
        logical :: monolithic!, fileExists
        character (len=MAX_LEN_FILE_PATH) :: fileName, filePart

        if (isReadOnly) return

        if (present(last)) then
            lastIdx = last
            tab0 = INDENT_INCR
        else
            lastIdx = first
            tab0 = 0
        end if
        tab1 = tab0+INDENT_INCR
        tab2 = tab0+INDENT_INCR*2
        monolithic = lastIdx>first

        stat = 0 ! error status
        if (monolithic) then
            if (len_trim(path)>0) then
                fileName = path
#if defined GLNX
                filePart = trim(fileName)//dotPART
#else
                filePart = fileName
#endif
                open(unit=device, file=filePart, iostat=stat, err=999)
                if (index(path, DIRSEP//'index')>0) then
                    do iSect=first,lastIdx
                        if (Section(thisTerm,iSect)%SubjectIdx==0) cycle
                        write(device,AFORMAT, iostat=stat, err=999) trim(filename_from(Section(thisTerm,iSect)%ClassId))
                    end do
                    close(device, iostat=stat, err=999)
#if defined GLNX
                    call rename(filePart, fileName, stat)
#endif
                    goto 999
                else
                    call xml_write_character(device, 0, XML_DOC)
                end if

            else
                fileName = trim(txtSemester(thisTerm))//' term classes to backup file'

            end if
            call xml_write_character(device, 0, ROOT_SECTIONS//trim(txtSemester(thisTerm)) )

        end if

        do iSect=first,lastIdx

            subj = Section(thisTerm,iSect)%SubjectIdx
            if (subj==0) cycle

            if (.not. monolithic) then
                fileName = trim(path)//trim(filename_from(Section(thisTerm,iSect)%ClassId))//dotXML
#if defined GLNX
                filePart = trim(fileName)//dotPART
#else
                filePart = fileName
#endif
                open(unit=device, file=filePart, iostat=stat, err=999)
                call xml_write_character(device, 0, XML_DOC)
                call xml_write_character(device, 0, ROOT_SECTIONS//trim(txtSemester(thisTerm)) )
            end if

            call xml_write_character(device, tab0, 'Section')
            if (is_regular_schedule(iSect, thisTerm)) then

                ! class is "regular": single entry for all meetings
                call xml_write_character(device, tab1, 'Subject', Subject(subj)%Name)
                call xml_write_character(device, tab1, 'Class', Section(thisTerm,iSect)%Code)
                if (len_trim(Section(thisTerm,iSect)%GradeSubmissionDate)>0) &
                    call xml_write_character(device, tab1, 'GradesIn', Section(thisTerm,iSect)%GradeSubmissionDate)
                call xml_write_character(device, tab1, 'Owner', Department(Section(thisTerm,iSect)%DeptIdx)%Code)
                call xml_write_integer(device,   tab1, 'Seats', Section(thisTerm,iSect)%Slots)
                call xml_write_character(device, tab1, 'Meeting')
                call xml_write_character(device, tab2, 'Time', text_time_period(Section(thisTerm,iSect)%bTimeIdx(1), &
                    Section(thisTerm,iSect)%eTimeIdx(1)))
                call xml_write_character(device, tab2, 'Day', text_days_of_section(Section(thisTerm,iSect)))
                call xml_write_character(device, tab2, 'Room', Room(Section(thisTerm,iSect)%RoomIdx(1))%Code)
                call xml_write_character(device, tab2, 'Teacher', Teacher(Section(thisTerm,iSect)%TeacherIdx(1))%TeacherId)
                call xml_write_character(device, tab1, '/Meeting')

            else ! class is "irregular": one entry for each meeting

                call xml_write_character(device, tab1, 'Subject', Subject(subj)%Name)
                call xml_write_character(device, tab1, 'Class', Section(thisTerm,iSect)%Code)
                if (len_trim(Section(thisTerm,iSect)%GradeSubmissionDate)>0) &
                    call xml_write_character(device, tab1, 'GradesIn', Section(thisTerm,iSect)%GradeSubmissionDate)
                call xml_write_character(device, tab1, 'Owner', Department(Section(thisTerm,iSect)%DeptIdx)%Code)
                call xml_write_integer(device,   tab1, 'Seats', Section(thisTerm,iSect)%Slots)
                do mdx=1,Section(thisTerm,iSect)%NMeets
                    call xml_write_character(device, tab1, 'Meeting')
                    call xml_write_character(device, tab2, 'Time', text_time_period(Section(thisTerm,iSect)%bTimeIdx(mdx), &
                        Section(thisTerm,iSect)%eTimeIdx(mdx)))
                    call xml_write_character(device, tab2, 'Day', txtDay(Section(thisTerm,iSect)%DayIdx(mdx)))
                    call xml_write_character(device, tab2, 'Room', Room(Section(thisTerm,iSect)%RoomIdx(mdx))%Code)
                    call xml_write_character(device, tab2, 'Teacher', Teacher(Section(thisTerm,iSect)%TeacherIdx(mdx))%TeacherId)
                    call xml_write_character(device, tab1, '/Meeting')
                end do
            end if
            call xml_write_character(device, tab0, '/Section')

            ! close file
            if (.not. monolithic) then
                call xml_write_character(device, 0, FSLASH//ROOT_SECTIONS//trim(txtSemester(thisTerm)) )
                close(device, iostat=stat, err=999)
#if defined GLNX
                call rename(filePart, fileName, stat)
#endif
                if (stat/=0) goto 999
            end if

        end do

        if (monolithic) then
            call xml_write_character(device, 0, FSLASH//ROOT_SECTIONS//trim(txtSemester(thisTerm)) )
            if (len_trim(path)>0) then
                close(device, iostat=stat, err=999)
#if defined GLNX
                call rename(filePart, fileName, stat)
#endif
            end if
        end if

        999 call terminate(stat, 'Error in writing '//fileName)
        if (monolithic) call log_comment('Successfully wrote '//fileName)

        isDirtyData = .true. ! allow writing to backup

    end subroutine class_details_write



    subroutine class_details_read(fileName, thisTerm, keepDuplicates)

        character(len=*), intent(in) :: fileName
        integer, intent (in) :: thisTerm
        logical, intent(in), optional :: keepDuplicates

        integer :: i, j, k, stat
        character(len=MAX_LEN_XML_LINE) :: value
        character(len=MAX_LEN_XML_TAG) :: tag
        type(TYPE_SECTION) :: wrkSection
        integer :: btime, dayidx(7), etime, ndays, iidx, pDASH
        integer :: subj, rmidx, tidx
        character (len = 1) :: ch
        character (len=5) :: strBTime, strETime
        character (len=MAX_LEN_DEPARTMENT_CODE) :: tDept
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_CLASS_ID) :: token
        character (len=MAX_LEN_ROOM_CODE) :: tRoom
        character (len=MAX_LEN_USERNAME) :: tTeacher

        ! open file, return on any error
        tag = ROOT_SECTIONS//trim(txtSemester(thisTerm))
        call xml_read_file(unitXML, trim(tag), fileName, stat)
        if (stat/=0) then
            call log_comment ('Warning: "'//trim(tag)//'" not found in '//fileName)
            return
        end if

        ! examine the file line by line
        do
            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit

            select case (trim(tag))

                case ('Section') ! initialize temporary section data
                    call initialize_section(wrkSection)

                case ('Subject') ! subject code
                    tSubject = adjustl(value)
                    call upper_case(tSubject)
                    subj = index_to_subject(tSubject)
                    wrkSection%SubjectIdx = subj

                case ('Class') ! section code
                    wrkSection%Code = adjustl(value)

                case ('GradesIn') ! grade submission date
                    wrkSection%GradeSubmissionDate = adjustl(value)

                case ('Owner') ! available seats in class
                    tDept = adjustl(value)
                    iidx = index_to_dept (tDept)
                    if (iidx==0) iidx = NumDepartments ! Registrar
                    wrkSection%DeptIdx = iidx

                case ('Seats') ! available seats in class
                    wrkSection%Slots = atoi(value)

                case ('Meeting') ! a meeting
                   ! do nothing for now

                case ('Time') ! begin, end times
                    j = index(value, '-')
                    if (j==0) then ! assume TBA
                        btime = 0
                        etime = 0
                    else
                        strBTime = value(1:j-1)
                        strETime = value(j+1:)
                        btime = index_to_time(strBTime)
                        etime = index_to_time(strETime)
                        if (etime<btime) then ! begin time is later than end time; assume evening class
                            etime = etime+48
                            call log_comment (trim(value)//' - assuming '//strETime//' is evening...')
                        end if
                    end if

                case ('Day') !   days
                    ndays = 0
                    dayidx = 0
                    k = len_trim(value)
                    if (value(:k)/='TBA') then
                        pDASH = -1
                        do i=1,k
                            ch = value(i:i)
                            iidx = 0
                            if (ch=='M') then
                                iidx = 1
                            else if (ch=='-') then
                                pDASH = i
                            else if (ch=='T') then
                                if (value(i+1:i+1)=='h' .or. value(i+1:i+1)=='H') then
                                    iidx = 4
                                else
                                    iidx = 2
                                end if
                            else if (ch=='W') then
                                iidx = 3
                            else if (ch=='F') then
                                iidx = 5
                            else if (ch=='S') then
                                if (value(i+1:i+1)=='a' .or. value(i+1:i+1)=='A') then
                                    iidx = 6
                                else
                                    iidx = 7
                                end if
                            end if
                            if (iidx>0) then
                                ndays = ndays+1
                                if (ndays>7) then
                                    call log_comment('Too many days: '//trim(value))
                                    ndays = 1 ! force to be TBA
                                    dayidx = 0
                                    btime = 0
                                    etime = 0
                                    exit
                                end if
                                dayidx(ndays) = iidx
                                if (pDASH==i-1) then
                                    do j=dayidx(ndays-1)+1,iidx
                                        dayidx(ndays) = j
                                        ndays = ndays+1
                                    end do
                                    ndays = ndays-1
                                end if
                            end if
                        end do
                    else
                        ndays = 1 ! count TBA day-time as 1 meeting
                    end if

                case ('Room') ! room
                    tRoom = adjustl(value)
                    if (tRoom=='TBA') then
                        rmidx = 0
                    else
                        rmidx = index_to_room (tRoom)
                        if (rmidx==0) then
                            call log_comment (trim(value)//' - '//trim(tRoom)//' room is not valid; using TBA')
                        end if
                    end if

                case ('Teacher') ! teacher
                    tTeacher = adjustl(value)
                    if (tTeacher=='TBA') then
                        tidx = 0
                    else
                        tidx = index_to_teacher (tTeacher)
                        if (tidx==0) then
                            call log_comment (trim(value)//' - '//trim(tTeacher)//' teacher is not valid; using TBA')
                        end if
                    end if

                case ('/Meeting') ! transfer to list of meetings
                    k = wrkSection%NMeets
                    wrkSection%DayIdx(k+1:k+ndays) = dayidx(1:ndays)
                    wrkSection%bTimeIdx(k+1:k+ndays) = btime
                    wrkSection%eTimeIdx(k+1:k+ndays) = etime
                    wrkSection%RoomIdx(k+1:k+ndays) = rmidx
                    wrkSection%TeacherIdx(k+1:k+ndays) = tidx
                    wrkSection%NMeets = wrkSection%NMeets + ndays

                case ('/Section') ! make ClassId, then add to list of sections
                    if (wrkSection%SubjectIdx<=0) then
                        call log_comment ('In '//trim(fileName)//' : '//trim(tSubject)//' : subject not in catalog')
                    else
                        token = trim(Subject(wrkSection%SubjectIdx)%Name)//SPACE//wrkSection%Code
                        wrkSection%ClassId = token
                        k = 0 ! already in list ?
                        do i=1,NumSections(thisTerm)
                            if (Section(thisTerm,i)%ClassId .ne. wrkSection%ClassId) cycle
                            k = i
                            if (present(keepDuplicates)) then
                                k = 0
                            end if
                            exit
                        end do
                        if (k==0) then ! add
                            NumSections(thisTerm) = NumSections(thisTerm) + 1
                            call check_array_bound (NumSections(thisTerm), MAX_ALL_SECTIONS, 'MAX_ALL_SECTIONS')
                            k = NumSections(thisTerm)
                        end if
                        Section(thisTerm,k) = wrkSection
                    end if

                case default
                    if ( trim(tag)==FSLASH//ROOT_SECTIONS//trim(txtSemester(thisTerm)) ) exit

            end select
        end do

        close(unitXML)

    end subroutine class_details_read




!===========================================================
! routines for blocks
!===========================================================



    subroutine blocks_index_read(device, thisTerm, path)

        integer, intent (in) :: device, thisTerm
        character (len=*), intent (in) :: path

        integer :: numEntries, blk, stat

        open(unit=device, file=trim(path)//'index', iostat=stat)
        if (stat/=0) then
            call log_comment('Error '//trim(itoa(stat))//' in opening index for '//path)
            return
        end if
        do
            read(device, AFORMAT, iostat=stat) line
            if (stat<0) exit
            call block_details_read(trim(path)//trim(line)//dotXML, thisTerm)
        end do
        close(device)

        ! compress block array
        numEntries = 0
        do blk=1,NumBlocks(thisTerm)
            !write(*,*) blk, Block(thisTerm,blk)%BlockID, Block(thisTerm,blk)%CurriculumIdx
            if (Block(thisTerm,blk)%CurriculumIdx/=0) then
                numEntries = numEntries+1
                Block(thisTerm,numEntries) = Block(thisTerm,blk)
                !write(*,*) 'Adding ', mainEntries, Block(thisTerm,blk)%BlockID
            end if
        end do
        NumBlocks(thisTerm) = numEntries

        call sort_alphabetical_blocks(thisTerm)

    end subroutine blocks_index_read


    subroutine block_details_write(device, thisTerm, path, first, last)
        character (len=*), intent (in) :: path
        integer, intent (in) :: device, thisTerm, first
        integer, intent (in), optional :: last

        integer :: iSect, blk, lastIdx, i, tab0, tab1, stat
        logical :: monolithic!, fileExists
        character (len=MAX_LEN_FILE_PATH) :: fileName, filePart

        if (isReadOnly) return

        if (present(last)) then
            lastIdx = last
            tab0 = INDENT_INCR
        else
            lastIdx = first
            tab0 = 0
        end if
        tab1 = tab0+INDENT_INCR
        monolithic = lastIdx>first

        stat = 0 ! error status
        if (monolithic) then
            if (len_trim(path)>0) then
                fileName = path
#if defined GLNX
                filePart = trim(fileName)//dotPART
#else
                filePart = fileName
#endif
                open(unit=device, file=filePart, iostat=stat, err=999)
                if (index(path, DIRSEP//'index')>0) then
                    do blk=first,lastIdx
                        if (Block(thisTerm,blk)%NumClasses==0) cycle
                        write(device,AFORMAT, iostat=stat, err=999) trim(filename_from(Block(thisTerm,blk)%BlockID))
                    end do
                    close(device, iostat=stat, err=999)
#if defined GLNX
                    call rename(filePart, fileName, stat)
#endif
                    goto 999
                else
                    call xml_write_character(device, 0, XML_DOC)
                end if

            else
                fileName = trim(txtSemester(thisTerm))//' term blocks to backup file'

            end if
            call xml_write_character(device, 0, ROOT_BLOCKS//trim(txtSemester(thisTerm)) )
        end if

        do blk=first,lastIdx

            if (Block(thisTerm,blk)%NumClasses==0) cycle

            if (.not. monolithic) then
                fileName = trim(path)//trim(filename_from(Block(thisTerm,blk)%BlockID))//dotXML
#if defined GLNX
                filePart = trim(fileName)//dotPART
#else
                filePart = fileName
#endif
                open(unit=device, file=filePart, iostat=stat, err=999)
                call xml_write_character(device, 0, XML_DOC)
                call xml_write_character(device, 0, ROOT_BLOCKS//trim(txtSemester(thisTerm)) )
            end if

            call xml_write_character(device, tab0, 'Block')
            call xml_write_character(device, tab1, 'Code', Block(thisTerm,blk)%BlockID)
            call xml_write_character(device, tab1, 'Description', Block(thisTerm,blk)%Name)
            call xml_write_character(device, tab1, 'Curriculum', Curriculum(Block(thisTerm,blk)%CurriculumIdx)%Code)
            call xml_write_integer(device,   tab1, 'Year', Block(thisTerm,blk)%Year)
            call xml_write_integer(device,   tab1, 'Term', Block(thisTerm,blk)%Term)
            call xml_write_character(device, tab1, 'Owner', Department(Block(thisTerm,blk)%DeptIdx)%Code)

            ! write classes assigned to block
            do i=1,Block(thisTerm,blk)%NumClasses
                iSect = Block(thisTerm,blk)%Section(i)
                if (iSect==0) then
                    call xml_write_character(device, tab1, 'Subject', Subject(Block(thisTerm,blk)%Subject(i))%Name)
                else
                    call xml_write_character(device, tab1, 'Section', Section(thisTerm,iSect)%ClassId)
                end if
            end do

            call xml_write_character(device, tab0, '/Block')

            if (.not. monolithic) then
                call xml_write_character(device, 0, FSLASH//ROOT_BLOCKS//trim(txtSemester(thisTerm)) )
                close(device, iostat=stat, err=999)
#if defined GLNX
                call rename(filePart, fileName, stat)
#endif
                if (stat/=0) goto 999
            end if

        end do
        if (monolithic) then
            call xml_write_character(device, 0, FSLASH//ROOT_BLOCKS//trim(txtSemester(thisTerm)) )
            if (len_trim(path)>0) then
                close(device, iostat=stat, err=999)
#if defined GLNX
                call rename(filePart, fileName, stat)
#endif
            end if
        end if

        999 call terminate(stat, 'Error in writing '//fileName)
        if (monolithic) call log_comment('Successfully wrote '//fileName)

        isDirtyData = .true. ! allow writing to backup

    end subroutine block_details_write


    subroutine block_details_read(fileName, thisTerm, keepDuplicates)

        character(len=*), intent(in) :: fileName
        integer, intent (in) :: thisTerm
        logical, intent(in), optional :: keepDuplicates

        type (TYPE_BLOCK) :: wrkBlock
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character (len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_CLASS_ID) :: tSection
        character (len=MAX_LEN_XML_LINE) :: value
        character (len=MAX_LEN_XML_TAG) :: tag
        integer :: i, idxCurr, idxSubj, idxSect, loc, stat

        ! open file, return on any error
        tag = ROOT_BLOCKS//trim(txtSemester(thisTerm))
        call xml_read_file(unitXML, trim(tag), fileName, stat)
        if (stat/=0) then
            call log_comment ('Warning: "'//trim(tag)//'" not found in '//fileName)
            return
        end if

        ! examine the file line by line
        do
            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit

            ! get tag and value if any
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit

            select case (trim(tag))

                case ('Block')
                    call initialize_block(wrkBlock)

                case ('Code')
                    wrkBlock%BlockID = adjustl(value)

                case ('Description')
                    wrkBlock%Name = adjustl(value)

                case ('Curriculum')
                    tCurriculum = adjustl(value)
                    idxCurr = index_to_curriculum(tCurriculum)
                    if (idxCurr<0) then
                        idxCurr = -idxCurr
                    else if (idxCurr==0) then
                        idxCurr =NumCurricula
                    end if
                    wrkBlock%CurriculumIdx = idxCurr

                case ('Year')
                    wrkBlock%Year = atoi(value)

                case ('Term')
                    wrkBlock%Term = atoi(value)
                    if (wrkBlock%Term==0) wrkBlock%Term = 3

                case ('Owner')
                    tDepartment = adjustl(value)
                    wrkBlock%DeptIdx = index_to_dept(tDepartment)
                    if (wrkBlock%DeptIdx==0) wrkBlock%DeptIdx = NumDepartments

                case ('Subject')
                    tSubject = adjustl(value)
                    call upper_case(tSubject)
                    idxSubj = index_to_subject(tSubject)
                    if (idxSubj==0) cycle

                    ! check if duplicate
                    loc = 0 ! not found
                    do i=1,wrkBlock%NumClasses
                        if (wrkBlock%Subject(i)==idxSubj) then
                            loc = i
                            exit
                        end if
                    end do
                    if (loc==0) then ! not a duplicate
                        wrkBlock%NumClasses = wrkBlock%NumClasses + 1
                        wrkBlock%Subject(wrkBlock%NumClasses) = idxSubj
                    else
                        call log_comment ('In '//trim(fileName)//' : '//trim(WrkBlock%BlockID)// &
                            ' duplicate subject '//trim(tSubject)//'; ignored.')
                    end if

                case ('Section')
                    tSection = adjustl(value)
                    if (len_trim(tSection)==0) cycle ! no section code
                    idxSect = index_to_section(tSection, thisTerm)
                    if (idxSect==0) cycle ! section does not exist

                    idxSubj = Section(thisTerm,idxSect)%SubjectIdx
                    ! check if duplicate
                    loc = 0 ! not found
                    do i=1,wrkBlock%NumClasses
                        if (wrkBlock%Subject(i)==idxSubj) then
                            loc = i
                            exit
                        end if
                    end do
                    if (loc==0) then ! not a duplicate
                        loc = wrkBlock%NumClasses + 1
                        wrkBlock%NumClasses = loc
                        wrkBlock%Subject(loc) = idxSubj
                        wrkBlock%Section(loc) = idxSect
                    elseif (wrkBlock%Section(loc)==0) then ! duplicate subject and prior entry had no section
                        wrkBlock%Section(loc) = idxSect
                    else ! duplicate subject and prior entry had a section
                        call log_comment ('In '//trim(fileName)//' : '//trim(WrkBlock%BlockID)// &
                            ' using section '//trim(tSection)//' instead of earlier entry '// &
                            Section(thisTerm,wrkBlock%Section(loc))%ClassId)
                        wrkBlock%Section(loc) = idxSect
                    end if

                case ('/Block')
                    loc = 0 ! not found
                    do idxCurr=1,NumBlocks(thisTerm)
                        if (wrkBlock%BlockID/=Block(thisTerm,idxCurr)%BlockID) cycle
                        loc = idxCurr
                        if (present(keepDuplicates)) then
                            loc = 0
                        end if
                        exit
                    end do
                    if (loc==0) then
                        NumBlocks(thisTerm) = NumBlocks(thisTerm) + 1
                        call check_array_bound (NumBlocks(thisTerm), MAX_ALL_BLOCKS, 'MAX_ALL_BLOCKS')
                        loc = NumBlocks(thisTerm)
                    end if
                    Block(thisTerm,loc) = wrkBlock

                case default
                    if (trim(tag)==FSLASH//ROOT_BLOCKS//trim(txtSemester(thisTerm)) ) exit

                    ! do nothing
            end select
        end do
        close(unitXML)

    end subroutine block_details_read


!===========================================================
! routines for students
!===========================================================



    subroutine students_index_read(device, path)
        integer, intent (in) :: device
        character (len=*), intent (in) :: path

        integer :: stat

        open(unit=device, file=trim(path)//'index', iostat=stat)
        call terminate(stat, 'Error '//trim(itoa(stat))//' in reading index of  '//path)
        do
            read(device, AFORMAT, iostat=stat) line
            if (stat<0) exit
            call student_details_read(trim(path)//trim(line)//dotXML, stat)
        end do
        close(device)

    end subroutine students_index_read


    subroutine student_details_write(device, path, first, last)
        character (len=*), intent (in) :: path
        integer, intent (in) :: device, first
        integer, intent (in), optional :: last

        integer :: std, lastIdx, tab0, tab1, tab2, stat
        integer :: iTerm, iSect, i, gdx, lenSubject
        logical :: monolithic!, fileExists
        character (len=MAX_LEN_FILE_PATH) :: fileName, filePart

        if (isReadOnly) return

        if (present(last)) then
            lastIdx = last
            tab0 = INDENT_INCR
        else
            lastIdx = first
            tab0 = 0
        end if

        monolithic = lastIdx>first

        tab1 = tab0+INDENT_INCR
        tab2 = tab0+INDENT_INCR*2

        stat = 0 ! error status
        if (monolithic) then ! single file for all students

            if (len_trim(path)>0) then
                fileName = path
#if defined GLNX
                filePart = trim(fileName)//dotPART
#else
                filePart = fileName
#endif
                open(unit=device, file=filePart, iostat=stat, err=999)

                ! write the index only
                if (index(path, DIRSEP//'index')>0) then
                    do std=first,lastIdx
                        if (len_trim(Student(std)%StdNo)==0) cycle
                        if (Student(std)%ResidenceStatus==REMOVE_FROM_ROSTER) cycle
                        write(device,AFORMAT, iostat=stat, err=999) trim(basefile_student(std))
                    end do
                    close(device, iostat=stat, err=999)
#if defined GLNX
                    call rename(filePart, fileName, stat)
#endif
                    goto 999
                end if

                call xml_write_character(device, 0, XML_DOC)

            else
                fileName = 'students to backup file'

            end if

            call xml_write_character(device, 0, ROOT_STUDENTS)

        end if


        do std=first,lastIdx

            if (len_trim(Student(std)%StdNo)==0) cycle
            if (Student(std)%ResidenceStatus==REMOVE_FROM_ROSTER) cycle

            if (.not. monolithic) then

                fileName = trim(path)//trim(basefile_student(std))//dotXML
#if defined GLNX
                filePart = trim(fileName)//dotPART
#else
                filePart = fileName
#endif
                open(unit=device, file=filePart, iostat=stat, err=999)
                call xml_write_character(device, 0, XML_DOC)
                call xml_write_character(device, 0, ROOT_STUDENTS)

            end if

            call xml_write_character(device, tab0, 'Student')

            ! Student() entries
            if (Student(std)%Gender=='M' .or. Student(std)%Gender=='F') &
                call xml_write_character(device, tab1, 'Gender', Student(std)%Gender)

            call xml_write_character(device, tab1, 'StdNo', Student(std)%StdNo)

            call xml_write_character(device, tab1, 'Name', Student(std)%Name)

            if (len_trim(Student(std)%Password)/=0) then
                call xml_write_character(device, tab1, 'Password', Student(std)%Password)
                if (.not. monolithic) call xml_write_character(device, tab1, 'Key', passwordEncryptionKey)
            end if

            if (Student(std)%CurriculumIdx/=0) &
                call xml_write_character(device, tab1, 'Curriculum', Curriculum(Student(std)%CurriculumIdx)%Code)

            if (Student(std)%ResidenceStatus/=RESIDENCY_STATUS_NOT_SET) &
                call xml_write_integer(device,   tab1, 'ResidenceStatus', Student(std)%ResidenceStatus)

            if (len_trim(Student(std)%Adviser)/=0) &
                call xml_write_character(device, tab1, 'Adviser', Student(std)%Adviser)

            if (len_trim(Student(std)%Scholarship)/=0) &
                call xml_write_character(device, tab1, 'Scholarship', Student(std)%Scholarship)

            if (len_trim(Student(std)%HomePSGC)/=0) &
                call xml_write_character(device, tab1, 'HomePSGC', Student(std)%HomePSGC)

            if (len_trim(Student(std)%MotherTongue)/=0) &
                call xml_write_character(device, tab1, 'MotherTongue', Student(std)%MotherTongue)

!            ! StudentInfo entries
!            if (len_trim(StudentInfo%Email)/=0) &
!                call xml_write_character(device, tab1, 'Email', StudentInfo%Email)
!
!            if (len_trim(StudentInfo%Father)/=0) &
!                call xml_write_character(device, tab1, 'Father', StudentInfo%Father)
!
!            if (len_trim(StudentInfo%Mother)/=0) &
!                call xml_write_character(device, tab1, 'Mother', StudentInfo%Mother)
!
!            if (len_trim(StudentInfo%Guardian)/=0) &
!                call xml_write_character(device, tab1, 'Guardian', StudentInfo%Guardian)
!
!            if (len_trim(StudentInfo%Gender)/=0) &
!                call xml_write_character(device, tab1, 'Gender', StudentInfo%Gender)
!
!            if (len_trim(StudentInfo%BirthDate)/=0) &
!                call xml_write_character(device, tab1, 'BirthDate', StudentInfo%BirthDate)
!
!            if (len_trim(StudentInfo%EntryDate)/=0) &
!                call xml_write_character(device, tab1, 'EntryDate', StudentInfo%EntryDate)
!
!            if (len_trim(StudentInfo%GraduationDate)/=0) &
!                call xml_write_character(device, tab1, 'GraduationDate', StudentInfo%GraduationDate)
!
!            if (len_trim(StudentInfo%BirthPlace)/=0) &
!                call xml_write_character(device, tab1, 'BirthPlace', StudentInfo%BirthPlace)
!
!            if (len_trim(StudentInfo%BirthPlacePSGC)/=0) &
!                call xml_write_character(device, tab1, 'BirthPlacePSGC', StudentInfo%BirthPlacePSGC)
!
!            if (len_trim(StudentInfo%HomeStreetAddress)/=0) &
!                call xml_write_character(device, tab1, 'HomeStreetAddress', StudentInfo%HomeStreetAddress)
!
!            if (len_trim(StudentInfo%LastAttended)/=0) &
!                call xml_write_character(device, tab1, 'LastAttended', StudentInfo%LastAttended)
!
!            if (len_trim(StudentInfo%TranscriptRemark)/=0) &
!                call xml_write_character(device, tab1, 'TranscriptRemark', StudentInfo%TranscriptRemark)
!
!            if (len_trim(StudentInfo%TranscriptAdditionalRemark)/=0) &
!                call xml_write_character(device, tab1, 'TranscriptAdditionalRemark', StudentInfo%TranscriptAdditionalRemark)
!
!            if (len_trim(StudentInfo%AdmissionData)/=0) &
!                call xml_write_character(device, tab1, 'AdmissionData', StudentInfo%AdmissionData)

            ! enlistment
            do iTerm=firstSemester, summerTerm

                if (Student(std)%Enlistment(iTerm)%statusAdvising==ADVISING_NEEDED .and. &
                    Student(std)%Enlistment(iTerm)%statusEnlistment==ENLISTMENT_NEEDED) cycle

                lenSubject = max(Student(std)%Enlistment(iTerm)%lenSubject, &
                    Student(std)%Enlistment(iTerm)%NPriority+Student(std)%Enlistment(iTerm)%NAlternates+ &
                    Student(std)%Enlistment(iTerm)%NCurrent)
                if (lenSubject==0) cycle

                call xml_write_character(device, tab1, ROOT_ENLISTMENT )
                call xml_write_integer  (device, tab2, 'Term', iTerm)

                if (Student(std)%Enlistment(iTerm)%levelClassification/=0) &
                    call xml_write_integer  (device, tab2, 'levelClassification', &
                        Student(std)%Enlistment(iTerm)%levelClassification )
                if (Student(std)%Enlistment(iTerm)%levelYear/=YEAR_NOT_SET) &
                    call xml_write_integer  (device, tab2, 'levelYear', Student(std)%Enlistment(iTerm)%levelYear)
                call xml_write_integer  (device, tab2, 'levelPriority', Student(std)%Enlistment(iTerm)%levelPriority)

                if (Student(std)%Enlistment(iTerm)%NPriority/=0) &
                    call xml_write_integer  (device, tab2, 'NPriority', Student(std)%Enlistment(iTerm)%NPriority)
                if (Student(std)%Enlistment(iTerm)%NAlternates/=0) &
                    call xml_write_integer  (device, tab2, 'NAlternates', Student(std)%Enlistment(iTerm)%NAlternates)
                if (Student(std)%Enlistment(iTerm)%NCurrent/=0) &
                    call xml_write_integer  (device, tab2, 'NCurrent', Student(std)%Enlistment(iTerm)%NCurrent)
                if (Student(std)%Enlistment(iTerm)%NRemaining/=0) &
                    call xml_write_integer  (device, tab2, 'NRemaining', Student(std)%Enlistment(iTerm)%NRemaining)
                if (Student(std)%Enlistment(iTerm)%MissingPOCW/=0) &
                    call xml_write_integer  (device, tab2, 'MissingPOCW', Student(std)%Enlistment(iTerm)%MissingPOCW)

                if (Student(std)%Enlistment(iTerm)%statusAdvising/=ADVISING_NEEDED) &
                    call xml_write_integer  (device, tab2, 'statusAdvising', Student(std)%Enlistment(iTerm)%statusAdvising)
                if (Student(std)%Enlistment(iTerm)%statusEnlistment/=ENLISTMENT_NEEDED) &
                    call xml_write_integer  (device, tab2, 'statusEnlistment', Student(std)%Enlistment(iTerm)%statusEnlistment)
                if (Student(std)%Enlistment(iTerm)%statusScholastic/=SCHOLASTIC_STATUS_NOT_SET) &
                    call xml_write_integer  (device, tab2, 'statusScholastic', Student(std)%Enlistment(iTerm)%statusScholastic)
                if (Student(std)%Enlistment(iTerm)%codeConditional/=0) &
                    call xml_write_integer  (device, tab2, 'codeConditional', Student(std)%Enlistment(iTerm)%codeConditional)
                if (Student(std)%Enlistment(iTerm)%codeNSTP/=0) &
                    call xml_write_integer  (device, tab2, 'codeNSTP', Student(std)%Enlistment(iTerm)%codeNSTP)

                if (Student(std)%Enlistment(iTerm)%UnitsEarned/=0.0) &
                    call xml_write_character(device, tab2, 'UnitsEarned', ftoa(Student(std)%Enlistment(iTerm)%UnitsEarned,1))
                if (Student(std)%Enlistment(iTerm)%AllowedLoad/=0.0) &
                    call xml_write_character(device, tab2, 'AllowedLoad', ftoa(Student(std)%Enlistment(iTerm)%AllowedLoad,1))
                if (Student(std)%Enlistment(iTerm)%UnderLoad/=0.0) &
                    call xml_write_character(device, tab2, 'UnderLoad', ftoa(Student(std)%Enlistment(iTerm)%UnderLoad,1))

                do i=1,lenSubject
                    iSect = Student(std)%Enlistment(iTerm)%Section(i)
                    if (iSect>0) then
                        gdx = Student(std)%Enlistment(iTerm)%Grade(i)
                        call xml_write_character(device, tab2, 'Graded', &
                            trim(Subject(Section(iTerm,iSect)%SubjectIdx)%Name)//SPACE//trim(Section(iTerm,iSect)%Code)//COMMA// &
                            txtGrade(pGrade(gdx)) )
                    elseif (Student(std)%Enlistment(iTerm)%Subject(i)>0) then
                        if (Student(std)%Enlistment(iTerm)%Contrib(i)>0.0) then
                            call xml_write_character(device, tab2, 'Predicted', &
                                trim(Subject(Student(std)%Enlistment(iTerm)%Subject(i))%Name)//COMMA// &
                                ftoa(Student(std)%Enlistment(iTerm)%Contrib(i),5) )
                        else
                            call xml_write_character(device, tab2, 'Allowed', &
                                Subject(Student(std)%Enlistment(iTerm)%Subject(i))%Name)
                        end if
                    end if
                end do

                call xml_write_character(device, tab1, FSLASH//ROOT_ENLISTMENT )

            end do

            call xml_write_character(device, tab0, '/Student')

            if (.not. monolithic) then
                call xml_write_character(device, 0, FSLASH//ROOT_STUDENTS)
                close(device, iostat=stat, err=999)
#if defined GLNX
                call rename(filePart, fileName, stat)
#endif
                if (stat/=0) goto 999
            end if

        end do

        if (monolithic) then
            call xml_write_character(device, 0, FSLASH//ROOT_STUDENTS)
            if (len_trim(path)>0) then
                close(device, iostat=stat, err=999)
#if defined GLNX
                call rename(filePart, fileName, stat)
#endif
            end if
        end if

        999 call terminate(stat, 'Error in writing '//fileName)
        if (monolithic) call log_comment('Successfully wrote '//fileName)
        isDirtyData = .true. ! allow writing to backup

    end subroutine student_details_write


    subroutine read_new_students(fileName, numEntries)

        character(len=*), intent(in) :: fileName
        integer, intent (out) :: numEntries

        type (TYPE_STUDENT) :: wrkStudent
        integer :: indexLoc, i, stat, errNo
        logical :: criticalErr

        ! open file, return on any error
        numEntries = 0
        open(unit=unitRAW, file=fileName, status='old', iostat=errNo)
        if (errNo/=0) then
            call log_comment('Not found: '//trim(fileName) )
            return
        end if

        call log_comment('read_new_students('//trim(fileName)//')')
        ! examine the file line by line
        do
            read(unitRAW, AFORMAT, iostat=stat) line
            if (stat<0) exit

            if (line(1:1) == '#' .or. line(1:3) == '   ') cycle

            call index_to_delimiters(COMMA, line, ndels, pos)
            if (ndels<3) cycle

            call initialize_student(wrkStudent)
            wrkStudent%StdNo = adjustl(line(:pos(2)-1))
            call upper_case(wrkStudent%StdNo)
            indexLoc = index_to_student(wrkStudent%StdNo)
            if (indexLoc/=0) then
                call log_comment (trim(line)//' - student already exists')
                cycle ! exists
            end if

            call check_array_bound (NumStudents+NumAdditionalStudents+1, MAX_ALL_STUDENTS, 'MAX_ALL_STUDENTS', criticalErr)
            if (criticalErr) then
                call log_comment (itoa(MAX_ALL_STUDENTS)//' limit on the number of students reached.')
                close(unitRAW)
                return
            end if

            wrkStudent%Name = line(pos(2)+1:)
            i = index(wrkStudent%Name, '"')
            do while (i>0)
                wrkStudent%Name(i:) = wrkStudent%Name(i+1:)
                i = index(wrkStudent%Name, '"')
            end do
            wrkStudent%Name = adjustl(wrkStudent%Name)
            call upper_case(wrkStudent%Name)
            wrkStudent%CurriculumIdx = NumCurricula

            ! use student number as initial password
            call set_password(wrkStudent%Password, trim(wrkStudent%StdNo))

            ! add a record
            if (NumStudents>0) then
                NumAdditionalStudents = NumAdditionalStudents + 1
            else ! this is the first student
                NumStudents = 1
            end if

            indexLoc = NumStudents+NumAdditionalStudents
            StdRank(indexLoc) = indexLoc
            Student(indexLoc) = wrkStudent
            numEntries = numEntries + 1

        end do
        close(unitRAW)
        call log_comment (itoa(numEntries)//' students added from '//fileName)

    end subroutine read_new_students


    subroutine student_details_read(fileName, numEntries)

        character(len=*), intent(in) :: fileName
        integer, intent (out) :: numEntries

        type (TYPE_STUDENT) :: wrkStudent
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        integer :: indexLoc, idxCurr, iTerm, stat
        character(len=lenPasswordEncryptionKey) :: tKey
        character (len=MAX_LEN_PASSWD_VAR) :: Password

        type(TYPE_PRE_ENLISTMENT) :: wrkEnlistment(3)
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_CLASS_ID) :: tClass
        character (len = MAX_LEN_TEXT_GRADE) :: tGrade
        integer :: cdx, idx, iSect, gdx, loc, i
        logical :: checkRegular

        ! open file, return on any error
        numEntries = 0
        call xml_read_file(unitXML, ROOT_STUDENTS, fileName, stat)
        if (stat/=0) return

        ! examine the file line by line
        do
            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit

            ! get tag and value if any
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit

            select case (trim(tag))

                case (FSLASH//ROOT_STUDENTS)
                    exit

                case ('Student')
                    call initialize_student(wrkStudent)
                    tKey = passwordEncryptionKey

                    call initialize_pre_enlistment (wrkEnlistment(1))
                    wrkEnlistment(2:3) = wrkEnlistment(1)

                case ('Gender')
                    wrkStudent%Gender = adjustl(value)
                    call upper_case(wrkStudent%Gender)

                case ('StdNo')
                    wrkStudent%StdNo = adjustl(value)
                    call upper_case(wrkStudent%StdNo)

                case ('Name')
                    wrkStudent%Name = adjustl(value)
                    call upper_case(wrkStudent%Name)
!                    ! add comma before last token
!                    call index_to_delimiters(COMMA, wrkStudent%Name, ndels, pos)
!                    if (ndels<3) then ! not "LAST, FIRST, MIDDLE"
!                        loc = 0
!                        do i=len_trim(wrkStudent%Name),1,-1
!                            if (wrkStudent%Name(i:i)==SPACE) then
!                                loc = i
!                                exit
!                            end if
!                        end do
!                        if (loc>0) then
!                            wrkStudent%Name = wrkStudent%Name(:loc-1)//COMMA//wrkStudent%Name(loc:pos(ndels+1)-1)
!                        end if
!                    end if

                case ('Password')
                    wrkStudent%Password = adjustl(value)

                case ('Key')
                    tKey = adjustl(value)

                case ('Adviser')
                    wrkStudent%Adviser = adjustl(value)

                case ('Scholarship')
                    wrkStudent%Scholarship = adjustl(value)

                case ('HomePSGC')
                    wrkStudent%HomePSGC = adjustl(value)

                case ('MotherTongue')
                    wrkStudent%MotherTongue = adjustl(value)

                case ('Curriculum')
                    tCurriculum = adjustl(value)
                    idxCurr = index_to_curriculum(tCurriculum)
                    if (idxCurr<0) then
                        idxCurr = -idxCurr
                    else if (idxCurr==0) then
                        idxCurr = NumCurricula
                    end if
                    wrkStudent%CurriculumIdx = idxCurr

                case ('ResidenceStatus')
                    wrkStudent%ResidenceStatus = max(0,atoi(value))

                case (ROOT_ENLISTMENT)
                    checkRegular = .false.

                case ('Term')
                    iTerm = atoi(value)

                case ('levelYear','StdYear')
                    wrkEnlistment(iTerm)%levelYear = atoi(value)

                case ('levelClassification', 'StdClassification')
                    wrkEnlistment(iTerm)%levelClassification = atoi(value)

                case ('levelPriority', 'StdPriority')
                    wrkEnlistment(iTerm)%levelPriority = atoi(value)


                case ('NPriority')
                    wrkEnlistment(iTerm)%NPriority = atoi(value)

                case ('NAlternates')
                    wrkEnlistment(iTerm)%NAlternates = atoi(value)

                case ('NCurrent')
                    wrkEnlistment(iTerm)%NCurrent = atoi(value)

                case ('NRemaining')
                    wrkEnlistment(iTerm)%NRemaining = atoi(value)

                case ('MissingPOCW')
                    wrkEnlistment(iTerm)%MissingPOCW = atoi(value)

                case ('statusAdvising')
                    wrkEnlistment(iTerm)%statusAdvising = atoi(value)

                case ('statusEnlistment')
                    wrkEnlistment(iTerm)%statusEnlistment = atoi(value)

                case ('statusScholastic', 'ScholasticStatus')
                    wrkEnlistment(iTerm)%statusScholastic = atoi(value)

                case ('codeConditional')
                    wrkEnlistment(iTerm)%codeConditional = atoi(value)

                case ('codeNSTP')
                    wrkEnlistment(iTerm)%codeNSTP = atoi(value)

                case ('UnitsEarned')
                    wrkEnlistment(iTerm)%UnitsEarned = atof(value)

                case ('AllowedLoad')
                    wrkEnlistment(iTerm)%AllowedLoad = atof(value)

                case ('UnderLoad')
                    wrkEnlistment(iTerm)%UnderLoad = atof(value)

                case ('Status')
                    i = atoi(value)
                    select case (i)
                        !        Status=-3          : NAdv
                        case (-3)
                            wrkEnlistment(iTerm)%statusAdvising = ADVISING_NEEDED
                        !        Status=-2          : Done
                        case (-2)
                            wrkEnlistment(iTerm)%statusAdvising = ADVISING_FINISHED
                        !        Status=-1          : Excl
                        case (-1)
                            wrkEnlistment(iTerm)%statusAdvising = ADVISING_EXCLUDED
                        !        Status=0          : NSub
                        case (0)
                            wrkEnlistment(iTerm)%statusAdvising = ADVISING_NO_SUBJECTS
                        !        Status=1          : AAdv
                        case (1)
                            wrkEnlistment(iTerm)%statusAdvising = ADVISING_IRREGULAR ! check later
                        !        Status=2          : MAdv
                        case (2)
                            wrkEnlistment(iTerm)%statusAdvising = ADVISING_IRREGULAR
                        !        Status=3          : AEnl
                        case (3)
                            wrkEnlistment(iTerm)%statusEnlistment = ENLISTMENT_AUTOMATIC
                        !        Status=4          : MEnl
                        case (4)
                            wrkEnlistment(iTerm)%statusEnlistment = ENLISTMENT_MANUAL
                        !        Status=5          : Cfrm
                        case (5)
                            wrkEnlistment(iTerm)%statusEnlistment = ENLISTMENT_CONFIRMED
                        !        Status=6          : Lckd
                        case (6)
                            wrkEnlistment(iTerm)%statusEnlistment = ENLISTMENT_LOCKED
                        !        Status=7          : Paid
                        case (7)
                            wrkEnlistment(iTerm)%statusEnlistment = ENLISTMENT_PAID

                    end select
                    checkRegular = .true.

                case ('Predicted')
                    idx = index(value, COMMA)
                    tSubject = adjustl(value(:idx-1))
                    call upper_case(tSubject)
                    cdx = index_to_subject(tSubject)
                    if (cdx<=0) then ! subject code not found
                        call log_comment (wrkStudent%StdNo//': no such subject; ignored - '//line)
                        cycle
                    end if
                    ! check if already among previous entries
                    loc = 0
                    do i=1,wrkEnlistment(iTerm)%lenSubject
                        if (wrkEnlistment(iTerm)%Subject(i)==cdx) loc = i
                    end do
                    if (loc==0) then
                        loc = wrkEnlistment(iTerm)%lenSubject + 1
                        call check_array_bound (loc, MAX_SUBJECTS_PER_TERM, 'MAX_SUBJECTS_PER_TERM in '//trim(line))
                        wrkEnlistment(iTerm)%lenSubject = loc
                    else
                        call log_comment('Using duplicate predicted entry:  '//trim(wrkStudent%StdNo)//DASH//trim(line))
                    end if
                    ! add as predicted subject
                    wrkEnlistment(iTerm)%Subject(loc) = cdx
                    wrkEnlistment(iTerm)%Contrib(loc) = atof( trim( value(idx+1:) ) )

                case ('Allowed')
                    tSubject = adjustl(value)
                    call upper_case(tSubject)
                    cdx = index_to_subject(tSubject)
                    if (cdx<=0) then ! subject code not found
                        call log_comment (wrkStudent%StdNo//': no such subject; ignored - '//line)
                        cycle
                    end if
                    ! check if already among previous entries
                    loc = 0
                    do i=1,wrkEnlistment(iTerm)%lenSubject
                        if (wrkEnlistment(iTerm)%Subject(i)==cdx) loc = i
                    end do
                    if (loc==0) then
                        loc = wrkEnlistment(iTerm)%lenSubject + 1
                        call check_array_bound (loc, MAX_SUBJECTS_PER_TERM, 'MAX_SUBJECTS_PER_TERM in '//trim(line))
                        wrkEnlistment(iTerm)%lenSubject = loc
                        ! add as allowed subject
                        wrkEnlistment(iTerm)%Subject(loc) = cdx
                        wrkEnlistment(iTerm)%Contrib(loc) = 0.0
                    else
                        call log_comment('Discarding duplicate allowed entry:  '//trim(wrkStudent%StdNo)//DASH//trim(line))
                    end if

                case ('Graded')
                    idx = index(value, COMMA)
                    tClass = adjustl(value(:idx-1))
                    tGrade = adjustl(value(idx+1:))
                    iSect = index_to_section(tClass, iTerm)
                    gdx = index_to_grade(tGrade)
                    ! check if already among previous entries
                    loc = 0
                    do i=1,wrkEnlistment(iTerm)%lenSubject
                        if (wrkEnlistment(iTerm)%Subject(i)==Section(iTerm,iSect)%SubjectIdx) loc = i
                    end do
                    if (loc==0) then
                        loc = wrkEnlistment(iTerm)%lenSubject + 1
                        call check_array_bound (loc, MAX_SUBJECTS_PER_TERM, 'MAX_SUBJECTS_PER_TERM in '//trim(line))
                        wrkEnlistment(iTerm)%lenSubject = loc
                    else
                        call log_comment('Using duplicate graded entry:  '//trim(wrkStudent%StdNo)//DASH//trim(line))
                    end if
                    wrkEnlistment(iTerm)%Section(loc) = iSect
                    wrkEnlistment(iTerm)%Subject(loc) = Section(iTerm,iSect)%SubjectIdx
                    wrkEnlistment(iTerm)%Grade(loc) = gdx
                    wrkEnlistment(iTerm)%Contrib(loc) = 1.0

                case (FSLASH//ROOT_ENLISTMENT)
                    if (wrkEnlistment(iTerm)%NPriority + wrkEnlistment(iTerm)%NAlternates + wrkEnlistment(iTerm)%NCurrent /= &
                            wrkEnlistment(iTerm)%lenSubject) then
                        wrkEnlistment(iTerm)%NPriority = wrkEnlistment(iTerm)%lenSubject
                        wrkEnlistment(iTerm)%NAlternates = 0
                        wrkEnlistment(iTerm)%NCurrent = 0
                    end if
                    ! check if manually enlisted without being advised
                    if (wrkEnlistment(iTerm)%statusAdvising<ADVISING_REGULAR .and. sum(wrkEnlistment(iTerm)%Section)>0) then
                        wrkEnlistment(iTerm)%statusAdvising = ADVISING_IRREGULAR
                    end if
                    ! check is advised subjects are regular
                    if (checkRegular .and. wrkEnlistment(iTerm)%statusAdvising==ADVISING_IRREGULAR) then
                        call check_for_regular_advised_subjects(idxCurr, iTerm, wrkEnlistment(iTerm))
                    end if

                case ('/Student')
                    if (.not. isAlphaNumeric(wrkStudent%StdNo) ) cycle
                    if (len_trim(wrkStudent%Password)==0) then
                        call set_password(wrkStudent%Password, trim(wrkStudent%StdNo))
                        isDirtySTUDENTS = .true.
                    else if (tKey/=passwordEncryptionKey) then
                        Password = wrkStudent%Password
                        call decrypt(tKey, Password)
                        Password = Password(lenPasswordEncryptionKey-atoi(Password(3:4))+1:)
                        call set_password(wrkStudent%Password, Password)
                    end if
                    if (len_trim(wrkStudent%HomePSGC)==0) wrkStudent%HomePSGC = PSGC(NumPSGC)%Code
                    wrkStudent%Enlistment = wrkEnlistment
                    call update_student_info(wrkStudent, indexLoc)
                    if (indexLoc<0) numEntries = numEntries + 1
                    !do iTerm=1,3
                    !    Enlistment(iTerm,abs(indexLoc)) = wrkEnlistment(iTerm)
                    !end do

                case default
                    ! do nothing

            end select
        end do
        close(unitXML)

    end subroutine student_details_read



    subroutine backup_write(backupFile)

        character (len=MAX_LEN_FILE_PATH), intent(in), optional :: backupFile

        integer :: iTerm, stat
        character (len=MAX_LEN_FILE_PATH) :: fileName, filePart
        logical :: existsBackup

        tickLastBackup = tick
        if (isReadOnly) return
        if (.not. isDirtyData) return

        if (present(backupFile) ) then
            fileName = backupFile
        else
            fileName = trim(pathToYear)//trim(UniversityCode)//'-BACKUP.XML'
        end if
        filePart = trim(fileName)//dotPart
        open(unit=unitXML, file=filePart, form='formatted', status='unknown', iostat=stat, err=999)
        call xml_write_character(unitXML, 0, XML_DOC)

        call university_data_write(unitXML, '')

        call college_details_write(unitXML, '', 1, NumColleges-1)

        call department_details_write(unitXML, '', 2, NumDepartments-1)

        call subject_details_write(unitXML, '', NumDummySubjects, NumSubjects+NumAdditionalSubjects)

        call curriculum_details_write(unitXML, '', 1,NumCurricula-1)

        call equivalence_data_write(unitXML, '')

        call room_details_write(unitXML, '', 1, NumRooms+NumAdditionalRooms)

        call teacher_details_write(unitXML, '', 1, NumTeachers+NumAdditionalTeachers)

        call student_details_write(unitXML, '', 1, NumStudents+NumAdditionalStudents)

        do iTerm=firstSemester,summerTerm
            call class_details_write(unitXML, iTerm, '', 1, NumSections(iTerm) )

            call block_details_write(unitXML, iTerm, '', 1, NumBlocks(iTerm) )

        end do

        close(unitXML, iostat=stat, err=999)
        ! temporary backup created

        ! move existing backup to archive
        inquire(file=fileName, exist=existsBackup)
        if (existsBackup) call move_to_backup(fileName)

        ! rename temporary backup file
!#if defined GLNX
        call rename(filePart, fileName, stat)
!#else
!        call system(mvCmd//trim(filePart)//SPACE//trim(fileName), stat)
!#endif

        ! reset 'dirty' flag
        isDirtyData = .false.

        999 call terminate(stat, 'Error in writing backup to '//fileName)
        call log_comment('Backup on '//currentDate//DASH//currentTime//' to '//trim(fileName))

    end subroutine backup_write


    subroutine year_data_read(path)

        character (len=*), intent(in) :: path

        integer :: jTmp, iTmp, errNo
        logical :: readFromBackup

        readFromBackup = index(path, trim(UniversityCode)//'-BACKUP.XML')>0

        ! the university-level data
        if (readFromBackup) then
            call university_data_read(path)
        else
            call university_data_read(trim(path)//'UNIVERSITY.XML')
        end if
        call log_comment(trim(UniversityName)//' @ '//UniversityAddress)

        ! the colleges
        if (readFromBackup) then
            call college_details_read(path)
        else
            call colleges_index_read(unitIDX, dirCOLLEGES)
        end if
        call log_comment(itoa(NumColleges)//' colleges from '//path)

        ! add 'administrative' college for data that does not fit in the 'academic' colleges
        NumColleges = NumColleges + 1
        call check_array_bound (NumColleges, MAX_ALL_COLLEGES, 'MAX_ALL_COLLEGES')
        call initialize_college (College(NumColleges), &
            SYSAD, trim(UniversityCode)//' Administration', SYSAD, SPACE, SPACE)

        ! log directory for users in the college
        do iTmp=1,NumColleges
            call make_directory( trim(dirLOG)//trim(College(iTmp)%Code)//DIRSEP )
        end do

        ! the departments
        if (readFromBackup) then
            call department_details_read(path)
        else
            call departments_index_read(unitIDX, dirDEPARTMENTS)
        end if
        call log_comment(itoa(NumDepartments-1)//' departments from '//path)

        ! add REGISTAR as 'administrative' department for data that does not fit in the 'academic' departments
        NumDepartments = NumDepartments + 1
        call check_array_bound (NumDepartments, MAX_ALL_DEPARTMENTS, 'MAX_ALL_DEPARTMENTS')
        call initialize_department (Department(NumDepartments), &
            SYSAD, trim(UniversityCode)//' Registrar', TheRegistrar, 'Z', NumColleges)


        ! the subjects
        if (readFromBackup) then
            call subject_codes_read(path)
            call subject_details_read(path)
        else
            call subjects_index_read(unitIDX, dirSUBJECTS)
        end if
        call log_comment (itoa(NumSubjects)//FSLASH//itoa(-NumDummySubjects)// &
            ' "regular"/"dummy" subjects from '//path)

        ! subject areas
        call get_subject_areas()

        ! the curricular programs
        if (readFromBackup) then
            call curriculum_details_read(path)
        else
            call curricula_index_read(unitIDX, dirCURRICULA)
        end if
        call log_comment (itoa(NumCurricula)//' curricula from '//dirCURRICULA)

        ! sort
        do iTmp=1,NumCurricula-1
            do jTmp=iTmp+1,NumCurricula
                if (Curriculum(iTmp)%Code>Curriculum(jTmp)%Code) then
                    Curriculum(NumCurricula+1) = Curriculum(iTmp)
                    Curriculum(iTmp) = Curriculum(jTmp)
                    Curriculum(jTmp) = Curriculum(NumCurricula+1)
                end if
            end do
        end do

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

        call make_curriculum_groups()

        ! the substitution rules
        if (readFromBackup) then
            iTmp = 0 ! substitutions
            call equivalencies_by_curriculum(path, iTmp)
            call equivalencies_for_all_curricula(path, iTmp)
            SubstIdx(NumSubst+1) = iTmp+1
        else
            call equivalence_data_read()
        end if

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
        if (readFromBackup) then
            call room_details_read(path)
        else
            call rooms_index_read(unitIDX, dirROOMS)
        end if
        call log_comment (itoa(NumRooms)//' rooms from '//path)

        if (NumRooms==0) then
            do iTmp=2,NumDepartments-1 ! create rooms for each department
                NumRooms = NumRooms+1
                call initialize_room(Room(NumRooms), trim(Department(iTmp)%Code)//' Room', iTmp, 0, 0)
            end do
            errNo = 0
        end if

        ! the teachers
        if (readFromBackup) then
            call teacher_details_read(path)
        else
            call teachers_index_read(unitIDX, dirTEACHERS)
        end if
        call log_comment (itoa(NumTeachers)//' teachers from '//path)

        Teacher(1)%DeptIdx = NumDepartments ! Guest's unit is not previously set
        if (NumTeachers==1) then ! 1=Guest only

            do iTmp=2,NumDepartments-1 ! create teacher for each department
                NumTeachers = NumTeachers+1
                call check_array_bound (NumTeachers, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')
                Teacher(NumTeachers)%TeacherId = trim(Department(iTmp)%Code)//'-Teacher'
                Teacher(NumTeachers)%DeptIdx = iTmp
                Teacher(NumTeachers)%Role = GUEST
                Teacher(NumTeachers)%Name = trim(Department(iTmp)%Code)//' Teacher'
                Teacher(NumTeachers)%MaxLoad = 0
                Teacher(NumTeachers)%Specialization = 'Teaching'
                call set_password(Teacher(NumTeachers)%Password, trim(Teacher(NumTeachers)%TeacherId))

            end do
            errNo = 0

            ! the Developer account
            NumTeachers = NumTeachers+1
            call check_array_bound (NumTeachers, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')
            Teacher(NumTeachers)%TeacherId = PROGNAME
            Teacher(NumTeachers)%DeptIdx = NumDepartments
            Teacher(NumTeachers)%Role = SYSAD
            Teacher(NumTeachers)%Name = PROGNAME//' Developer'
            Teacher(NumTeachers)%MaxLoad = 0
            Teacher(NumTeachers)%Specialization = 'HEEDS Development'
            call set_password(Teacher(NumTeachers)%Password)

            ! the Administrator
            NumTeachers = NumTeachers+1
            call check_array_bound (NumTeachers, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')
            Teacher(NumTeachers)%TeacherId = 'Registrar'
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
            Teacher(NumTeachers)%TeacherId = ScholarshipCode(iTmp)
            Teacher(NumTeachers)%DeptIdx = NumDepartments
            Teacher(NumTeachers)%Role = BENEFACTOR
            Teacher(NumTeachers)%Name = ScholarshipDescription(iTmp)
            Teacher(NumTeachers)%MaxLoad = 0
            Teacher(NumTeachers)%Specialization = 'Benefactor'
            call set_password(Teacher(NumTeachers)%Password, Teacher(NumTeachers)%TeacherId)

        end do

        call sort_teachers()
        call sort_alphabetical_teachers()

        ! mark colleges with subject or curriculum information
        do jTmp=1,NumDepartments
            iTmp = Department(jTmp)%CollegeIdx
            College(iTmp)%hasInfo = College(iTmp)%hasInfo .or. Department(jTmp)%hasInfo
        end do


        ! classes and blocks
        do jTmp=firstSemester,summerTerm

            ! the classes
            if (readFromBackup) then
                call class_details_read(path, jTmp)
            else
                call classes_index_read(unitIDX, jTmp, dirCLASSES(jTmp))
            end if
            call log_comment (itoa(NumSections(jTmp))//' sections for '//trim(text_term_school_year(jTmp, currentYear))// &
                ' in '//path)

            ! sort & summarize
            call offerings_sort(jTmp)
            call offerings_summarize(jTmp, 0)

            ! read the blocks
            if (readFromBackup) then
                call block_details_read(path, jTmp )
            else
                call blocks_index_read(unitIDX, jTmp, dirBLOCKS(jTmp) )
            end if
            call log_comment (itoa(NumBlocks(jTmp))//' blocks for '//trim(text_term_school_year(jTmp, currentYear))// &
                ' in '//path)

            ! compress block array
            errNo = 0
            do iTmp=1,NumBlocks(jTmp)
                if (Block(jTmp,iTmp)%CurriculumIdx/=0) then
                    errNo = errNo+1
                    Block(jTmp,errNo) = Block(jTmp,iTmp)
                end if
            end do
            NumBlocks(jTmp) = errNo
            errNo = 0

            call sort_alphabetical_blocks(jTmp)

            ! count no. of sections by dept
            call count_sections_by_dept(jTmp)

        end do

        ! read the students
        if (readFromBackup) then
            call student_details_read(path, iTmp)
        else
            call students_index_read(unitIDX, dirSTUDENTS)
        end if
        ! update advisee counts
        do jTmp=1,NumTeachers+NumAdditionalTeachers
            Teacher(jTmp)%NumAdvisees = 0
        end do
        do iTmp=1,NumStudents+NumAdditionalStudents
            jTmp = index_to_teacher(Student(iTmp)%Adviser)
            Teacher(jTmp)%NumAdvisees = Teacher(jTmp)%NumAdvisees + 1
        end do

        call log_comment (itoa(NumStudents)//' students from '//path)

        call sort_alphabetical_students()

        do jTmp=firstSemester,summerTerm
            call recalculate_available_seats(jTmp)
        end do

        ! remember clock tick when data is retrieved
        CALL SYSTEM_CLOCK(tickLastRefresh, count_rate, count_max)
        tickLastBackup = tickLastRefresh

    end subroutine year_data_read


   subroutine read_EVALUATION_FORM()

        integer :: idx, stat

        ! initialize
        NumEvalCriteria = 0
        Evaluation = TYPE_EVALUATION_CRITERION(0, 0, SPACE)
        NumEvalCategories = 0
        EvalCategory = TYPE_EVALUATION_CATEGORY(SPACE, 0, SPACE)

        ! open file, return on any error
        call xml_read_file(unitXML, ROOT_EVALUATION, 'EVALUATION.XML', stat)
        if (stat/=0) return

        ! examine the file line by line
        do
            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit

            select case (trim(tag))

                case (FSLASH//ROOT_EVALUATION)
                    exit

                case ('Category')
                    NumEvalCategories = NumEvalCategories + 1
                    idx = 0

                case ('Code')
                    EvalCategory(NumEvalCategories)%Code = value

                case ('Weight')
                    EvalCategory(NumEvalCategories)%Weight = atoi(value)

                case ('Description')
                    EvalCategory(NumEvalCategories)%Description = value

                case ('Item')
                    idx = idx + 1
                    NumEvalCriteria = NumEvalCriteria + 1
                    Evaluation(NumEvalCriteria)%Category = NumEvalCategories
                    Evaluation(NumEvalCriteria)%Item = idx
                    Evaluation(NumEvalCriteria)%Criterion = value

                case ('/Category')

                case default ! do nothing

            end select

        end do
        close(unitXML)

    end subroutine read_EVALUATION_FORM


    subroutine evaluation_details_read(pathToFile)

        character(len=*), intent(in) :: pathToFile

        integer :: idx, stat

        EvaluationForm = TYPE_EVALUATION_FORM(0, 0, 0, SPACE, SPACE, SPACE, SPACE, SPACE, 0)

        ! find ROOT_EVALUATION
        call xml_read_file(unitXML, ROOT_EVALUATION, pathToFile, stat)
        if (stat/=0) return ! nothing found

        ! examine the file line by line
        do
            read(unitXML, AFORMAT, iostat=stat) line
            if (stat<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, stat)
            if (stat/=0) exit

            select case (trim(tag))

                case (FSLASH//ROOT_EVALUATION)
                    exit

                case ('EvalType')
                    EvaluationForm%EvalType = atoi(value)

                case ('Year')
                    EvaluationForm%Year = atoi(value)

                case ('Term')
                    EvaluationForm%Term = atoi(value)

                case ('LastModified')
                    EvaluationForm%LastModified = value

                case ('EvaluatedID')
                    EvaluationForm%EvaluatedID = value

                case ('EvaluatorID')
                    EvaluationForm%EvaluatorID = value

                case ('ClassId')
                    EvaluationForm%ClassId = value

                case ('Comments')
                    EvaluationForm%Comments = value

                case ('Rating')
                    read(value,*) (EvaluationForm%Rating(idx), idx=1,NumEvalCriteria)

                case default ! do nothing

            end select

        end do
        close(unitXML)

    end subroutine evaluation_details_read



    subroutine evaluation_details_write(pathToFile)

        character(len=*), intent(in) :: pathToFile

        integer :: stat, idx
        character (len=MAX_LEN_FILE_PATH) :: fileName, filePart

        fileName = pathToFile
        filePart = trim(fileName)//dotPart
        call open_for_write(unitXML, filePart, stat)

        call xml_write_character(unitXML, 0, XML_DOC)
        call xml_write_character(unitXML, indent0, ROOT_EVALUATION)

        call xml_write_integer  (unitXML, indent1, 'EvalType', EvaluationForm%EvalType)
        call xml_write_integer  (unitXML, indent1, 'Year', EvaluationForm%Year)
        call xml_write_integer  (unitXML, indent1, 'Term', EvaluationForm%Term)
        call xml_write_character(unitXML, indent1, 'LastModified', EvaluationForm%LastModified)
        call xml_write_character(unitXML, indent1, 'EvaluatedID', EvaluationForm%EvaluatedID)
        call xml_write_character(unitXML, indent1, 'EvaluatorID', EvaluationForm%EvaluatorID)

        if (len_trim(EvaluationForm%ClassId)>0) &
            call xml_write_character(unitXML, indent1, 'ClassId', EvaluationForm%ClassId)

        line = SPACE
        write(line,'(50i2)') (EvaluationForm%Rating(idx), idx=1,NumEvalCriteria)
        call xml_write_character(unitXML, indent1, 'Rating', line)

        if (len_trim(EvaluationForm%Comments)>0) &
            call xml_write_character(unitXML, indent1, 'Comments', EvaluationForm%Comments)

        call xml_write_character(unitXML, indent0, FSLASH//ROOT_EVALUATION)

        close(unitXML, iostat=stat, err=999)
        ! temporary backup created

        ! rename temporary backup file
        call rename(filePart, fileName, stat)

        999 call terminate(stat, 'Error in writing to '//fileName)

    end subroutine evaluation_details_write


    subroutine evaluation_duty_read(evalDutyFile, tLen, tArray)

        character (len=*), intent(in) :: evalDutyFile
        integer, intent(in out) :: tLen, tArray(:)
        integer :: ierr, pos, idx1, idx2, start
        character (len=MAX_LEN_FILE_PATH) :: line
        character(len=MAX_LEN_USERNAME) :: evaluatorID, evaluatedID

        ! remember 1st position
        start = tLen
        ! open file for reading; exit (go to 999) on error
        open(unit=unitETC, file=evalDutyFile, status='old', iostat=ierr, err=999)
        !call html_comment('evaluation_duty_read : '//itoa(ierr)//' in opening '//evalDutyFile)
        ierr = 1 ! file is open
        do
            ! read one line; stop reading (goto 998) of end or error
            read(unitETC, AFORMAT, end=998, err=998) line
            !call html_comment(line)
            pos = index(line, COMMA)
            if (pos<2) cycle

            evaluatorID = line(1:pos-1)
            idx1 = index_to_teacher( evaluatorID )
            evaluatedID = line(pos+1:)
            idx2 = index_to_teacher( evaluatedID )
            !call html_comment(itoa(idx1)//evaluatorID, itoa(idx2)//evaluatedID)

            if (idx1*idx2>0) then
                tLen = tLen+1
                tArray(2*tLen-1) = idx1
                tArray(2*tLen  ) = idx2
            end if

        end do
        998 if (ierr/=0) close(unitETC)
        999 continue

        ! re-count Teacher()%EvalsToGive, Teacher()%EvalsToReceive
         if (start<tLen) then
            do pos=1,NumTeachers
                Teacher(pos)%EvalsToGive = 0
                Teacher(pos)%EvalsToReceive = 0
            end do
            do  pos=1,tLen
                idx1 = tArray(2*pos-1)
                idx2 = tArray(2*pos  )
                Teacher(idx1)%EvalsToGive = Teacher(idx1)%EvalsToGive +1
                Teacher(idx2)%EvalsToReceive = Teacher(idx2)%EvalsToReceive + 1
            end do
        end if
        !call html_comment('evaluation_duty_read : '//itoa(tLen)//' in '//evalDutyFile)

    end subroutine evaluation_duty_read


    subroutine evaluation_duty_write(evalDutyFile, tLen, tArray)

        character (len=*), intent(in) :: evalDutyFile
        integer, intent(in) :: tLen, tArray(:)
        integer :: ierr, pos, idx1, idx2

        call open_for_write(unitETC, evalDutyFile, ierr)
        ! re-count Teacher()%EvalsToGive, Teacher()%EvalsToReceive
        do pos=1,NumTeachers
            Teacher(pos)%EvalsToGive = 0
            Teacher(pos)%EvalsToReceive = 0
        end do
        ierr = 0
        do  pos=1,tLen
            idx1 = tArray(2*pos-1)
            idx2 = tArray(2*pos  )
            if (idx1*idx2==0) cycle

            Teacher(idx1)%EvalsToGive = Teacher(idx1)%EvalsToGive +1
            Teacher(idx2)%EvalsToReceive = Teacher(idx2)%EvalsToReceive + 1
            write(unitETC,AFORMAT) trim(Teacher(idx1)%TeacherID)//COMMA//trim(Teacher(idx2)%TeacherID)
            ierr = ierr + 1
        end do
        close(unitETC)
        !call html_comment('evaluation_duty_write() : '//itoa(ierr)//' in '//evalDutyFile)

    end subroutine evaluation_duty_write

    subroutine  effectiveness_ratings_retrieve (evalType, evalFilePattern, fileCount, sumRating, remarksFile)
        integer, intent(in) :: evalType
        character (len=*), intent(in) :: evalFilePattern, remarksFile
        integer, dimension(MAX_EVALUATION_TYPES), intent(in out) :: fileCount
        real, dimension(MAX_EVALUATION_TYPES,MAX_EVALUATION_CRITERIA), intent(in out) :: sumRating

        character (len=MAX_LEN_FILE_PATH) :: evaluationFile, fileList
        integer :: ierr, sarray(13)

        ! where to write list of files that match pattern
        fileList = evalFilePattern
        do ierr = len_trim(evalFilePattern),1,-1
            if (fileList(ierr:ierr)==DIRSEP) then
                 if (fileList(ierr-1:ierr-1)=='*') then
                     fileList(ierr-1:) = SPACE
                 else
                     fileList(ierr+1:) = SPACE
                 end if
                 exit
             end if
        end do
        fileList = trim(fileList)//EvalTypeDescription(evalType)

        !call html_comment('effectiveness_ratings_retrieve()', evalFilePattern, fileList)
        call system(dirCmd//trim(evalFilePattern)//' > '//trim(fileList), ierr)

        ierr = stat(fileList, sarray)

        if (ierr .eq. 0 .and. sarray(8) .gt. 0) then

            open(unit=unitETC, file=fileList, status='unknown', form='formatted', iostat=ierr)
            ! loop through files
            do while (ierr==0)

                read(unitETC,AFORMAT,iostat=ierr) evaluationFile
                if (ierr .lt. 0) then ! no more mailboxes
                    close(unitETC)
                    exit
                end if

                !call html_comment(evaluationFile)
                call evaluation_details_read(evaluationFile)
                fileCount(evalType) = fileCount(evalType) + 1
                sumRating(evalType,1:NumEvalCriteria) = sumRating(evalType,1:NumEvalCriteria) + &
                    EvaluationForm%Rating(1:NumEvalCriteria)

                ! write Remarks ?
                if (len_trim(remarksFile)>0) then
                    if (len_trim(EvaluationForm%Comments)>0) &
                        write(unitREM,AFORMAT) b_para, &
                            trim(EvalTypeDescription(evalType))//': '//trim(EvaluationForm%Comments), &
                            e_para
                end if

            end do

        end if

    end subroutine effectiveness_ratings_retrieve


end module XMLIO
