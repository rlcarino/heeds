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

module UNIVERSITY

    use XML

    implicit none

    ! University
    character (len=20) :: &
        UniversityCode = 'UNIVERSITY CODE'
    character (len=60) :: &
        UniversityName = '(Specify NAME in UNIVERSITY.XML)', &
        UniversityAddress = '(Specify ADDRESS in UNIVERSITY.XML)'
    character (len=40) :: &
        UniversityPresident = 'Firstname MI LastName, PhD', &
        VPAcademicAffairs = 'Firstname MI LastName, PhD', &
        DeanOfCampus = 'Firstname MI LastName, PhD', &
        DeanOfInstruction = 'Firstname MI LastName, PhD'

    integer, parameter :: &
        MAX_LEN_COLLEGE_CODE=10, & ! length of college codes
        MAX_LEN_DEPARTMENT_CODE=10 ! length of dept codes

    ! 'Administrative' college, for data not under the academic colleges
    character (len=MAX_LEN_COLLEGE_CODE) :: ADMINISTRATION = 'ADMIN'
    ! 'Administrative' department, for data not under the academic departments
    character (len=MAX_LEN_DEPARTMENT_CODE) :: REGISTRAR = 'Registrar'

    integer :: baseYear = 2000 ! year that records usable by HEEDS are available in the database
    integer :: StdNoYearLen ! no. of characters in StdNo to use for directory name
    integer, parameter :: StdNoChars = 2 ! no. of characters in StdNo to use for directory name


    ! private tokens
    character (len=MAX_LEN_FILE_PATH), private :: fileName
    character (len=MAX_LEN_XML_LINE), private :: line, value
    character (len=MAX_LEN_XML_TAG), private :: tag
    integer, private :: eof, ndels, pos(30)
    logical, private :: noXML


contains


#include "custom_read_university.F90"


    subroutine read_university(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent (out) :: errNo

        noXML = .false.
        call xml_read_university(path, errNo) ! try the XML file
        if (errNo/=0) then ! something wrong; try the SIAS routine
            noXML = .true.
            call custom_read_university(path, errNo)
            if (errNo/=0) return ! something still wrong
        end if

        ! write the UNIVERSITY file in XML?
        if (noXML) call xml_write_university(path)

        return
    end subroutine read_university


    subroutine xml_read_university(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent (out) :: errNo

        ! open file, return on any error
        fileName = trim(dirXML)//trim(path)//'UNIVERSITY.XML'
        call xml_open_file(unitXML, XML_ROOT_UNIVERSITY, fileName, errNo, forReading)
        if (errNo/=0) return

        ! examine the file line by line
        do
            read(unitXML, AFORMAT, iostat=eof) line
            if (eof<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, eof)
            if (eof/=0) exit

            select case (trim(tag))

                case ('NAME')
                    UniversityName = adjustl(value)

                case ('ADDRESS')
                    UniversityAddress = adjustl(value)

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

                case ('ADMINISTRATION')
                    ADMINISTRATION = adjustl(value)

                case ('REGISTRAR')
                    REGISTRAR = adjustl(value)

                case default ! do nothing

            end select

        end do

        call xml_close_file(unitXML)
        call file_log_message(trim(UniversityName)//' @ '//UniversityAddress)

        return
    end subroutine xml_read_university


    subroutine xml_write_university(path)

        character(len=*), intent(in) :: path

        ! training only?
        if (noWrites) return

        fileName = trim(dirXML)//trim(path)//'UNIVERSITY.XML'
        call xml_open_file(unitXML, XML_ROOT_UNIVERSITY, fileName, eof)

        write(unitXML,AFORMAT) &
        '    <comment>', &
        '        Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                    FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
        '    </comment>'

        call xml_write_character(unitXML, indent0, 'NAME', UniversityName)
        call xml_write_character(unitXML, indent0, 'ADDRESS', UniversityAddress)
        call xml_write_character(unitXML, indent0, 'PRESIDENT', UniversityPresident)
        call xml_write_character(unitXML, indent0, 'DEANOFINSTRUCTION', DeanOfInstruction)
        call xml_write_character(unitXML, indent0, 'VPACADEMICAFFAIRS', VPAcademicAffairs)
        call xml_write_character(unitXML, indent0, 'DEANOFCAMPUS', DeanOfCampus)
        call xml_write_integer  (unitXML, indent0, 'BASEYEAR', baseYear)
        call xml_write_character(unitXML, indent0, 'ADMINISTRATION', ADMINISTRATION)
        call xml_write_character(unitXML, indent0, 'REGISTRAR', REGISTRAR)

        call xml_close_file(unitXML, XML_ROOT_UNIVERSITY)

        return
    end subroutine xml_write_university



end module UNIVERSITY
