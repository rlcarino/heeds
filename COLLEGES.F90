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


module COLLEGES

    use UNIVERSITY

    implicit none

    ! public tokens
    integer, parameter :: &
        MAX_ALL_COLLEGES = 20, & ! max no. of colleges
        MAX_LEN_COLLEGE_NAME=60, & ! length of college names
        MAX_ALL_DEPARTMENTS = 250, & ! max no. of departments
        MAX_DEPARTMENT_NAME_LEN=60 ! length of dept names

    type :: TYPE_COLLEGE
        character (len=MAX_LEN_COLLEGE_CODE) :: Code
        character (len=MAX_LEN_COLLEGE_NAME) :: Name, Dean
        logical :: hasInfo
    end type TYPE_COLLEGE

    type (TYPE_COLLEGE), dimension (0:MAX_ALL_COLLEGES) :: College
    integer :: NumColleges

    ! private tokens
    character (len=MAX_LEN_FILE_PATH), private :: fileName
    character (len=MAX_LEN_XML_LINE), private :: line, value
    character (len=MAX_LEN_XML_TAG), private :: tag
    integer, private :: eof, ndels, pos(30)
    logical, private :: noXML

contains

#include "custom_read_colleges.F90"


    subroutine initialize_college (wrkCollege, tCode, tName, tDean)

        type(TYPE_COLLEGE), intent(out) :: wrkCollege
        character(len=*), intent(in), optional :: tCode, tName, tDean

        if (present(tCode)) then
            wrkCollege = TYPE_COLLEGE(tCode, tName, tDean, .false.)
        else
            wrkCollege = TYPE_COLLEGE(SPACE, SPACE, 'Firstname MI Lastname, Ph.D.', .false.)
        end if

    end subroutine initialize_college


    function index_to_college (tCode)
        !returns index of tCode in the list of colleges

        integer :: index_to_college
        character (len=MAX_LEN_COLLEGE_CODE), intent (in) :: tCode

        integer :: i

        index_to_college = 0
        do i=1,NumColleges
            if (tCode==College(i)%Code) then
                index_to_college = i
                exit
            end if
        end do

    end function index_to_college


    subroutine read_colleges(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent (out) :: errNo

        logical :: noXML

        NumColleges = 0
        call initialize_college (College(0))
        College(1:) = College(0)

        noXML = .false.
        call xml_read_colleges(path, errNo) ! try the XML file
        if (errNo/=0) then ! something wrong with XML file
            noXML = .true.
            call  custom_read_colleges(path, errNo) ! try custom format
            if (errNo/=0) return ! something still wrong
        end if

        ! add 'administrative' college for data that does not fit in the 'academic' colleges
        NumColleges = NumColleges + 1
        call initialize_college (College(NumColleges), &
            ADMINISTRATION, UniversityCode//' Administration', UniversityPresident)

        ! write the COLLEGES file in XML?
        if (noXML ) call xml_write_colleges(path)

    end subroutine read_colleges


    subroutine xml_read_colleges(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent (out) :: errNo

        type(TYPE_COLLEGE) :: wrkCollege

        ! open file, return on any error
        fileName = trim(dirDATA)//trim(path)//'COLLEGES.XML'
        call xml_open_file(unitXML, XML_ROOT_COLLEGES, fileName, errNo, forReading)
        if (errNo/=0) return

        ! examine the file line by line
        do
            read(unitXML, AFORMAT, iostat=eof) line
            if (eof<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, eof)
            if (eof/=0) exit

            select case (trim(tag))

                case ('College') ! initialize temporary college data
                    call initialize_college (wrkCollege)

                case ('Code')
                    wrkCollege%Code = adjustl(value)

                case ('Name')
                    wrkCollege%Name = adjustl(value)

                case ('Dean')
                    wrkCollege%Dean = adjustl(value)

                case ('/College') ! add temporary college data to College()
                    if (index(wrkCollege%Code,trim(ADMINISTRATION))>0) cycle ! add later
                    NumColleges = NumColleges + 1
                    call check_array_bound (NumColleges, MAX_ALL_COLLEGES, 'MAX_ALL_COLLEGES')
                    College(NumColleges) = wrkCollege

                case default ! do nothing

            end select

        end do

        call xml_close_file(unitXML)
        call file_log_message (itoa(NumColleges)//' entries in '//fileName)

    end subroutine xml_read_colleges


    subroutine xml_write_colleges(path, dirOPT)

        character(len=*), intent(in) :: path
        character(len=*), intent(in), optional :: dirOPT
        integer :: ldx

        ! training only?
        if (noWrites) return

        if (present(dirOPT)) then
            fileName = trim(dirOPT)//trim(path)//'COLLEGES.XML'
        else
            fileName = trim(dirDATA)//trim(path)//'COLLEGES.XML'
        endif

        call xml_open_file(unitXML, XML_ROOT_COLLEGES, fileName, ldx)

        write(unitXML,AFORMAT) &
            '    <comment>', &
            '        Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                        FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
            '        Code - college code', &
            '        Name - long name of college', &
            '        Dean - signatory for college (Firstname MI Lastname, PhD)', &
            '    </comment>'

        do ldx = 1,NumColleges-1 ! exclude ADMINISTRATION
            call xml_write_character(unitXML, indent0, 'College')
            call xml_write_character(unitXML, indent1, 'Code', College(ldx)%Code)
            call xml_write_character(unitXML, indent1, 'Name', College(ldx)%Name)
            call xml_write_character(unitXML, indent1, 'Dean', College(ldx)%Dean)
            call xml_write_character(unitXML, indent0, '/College')
        end do

        call xml_close_file(unitXML, XML_ROOT_COLLEGES)

    end subroutine xml_write_colleges


end module COLLEGES
