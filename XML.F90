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


module XML

    use BASE

    implicit none

    ! maximum line size in an XML file
    integer, parameter :: MAX_LEN_XML_LINE = 1000

    ! maximum characters in a tag
    integer, parameter :: MAX_LEN_XML_TAG = 40

    ! root names                                              12345678901234567890
    character(len=10), parameter :: XML_ROOT_UNIVERSITY    = 'UNIVERSITY'
    character(len=16), parameter :: XML_ROOT_COLLEGES      = 'LIST_OF_COLLEGES'
    character(len=19), parameter :: XML_ROOT_DEPARTMENTS   = 'LIST_OF_DEPARTMENTS'
    character(len=16), parameter :: XML_ROOT_SUBJECTS      = 'LIST_OF_SUBJECTS'
    character(len=13), parameter :: XML_ROOT_ROOMS         = 'LIST_OF_ROOMS'
    character(len=16), parameter :: XML_ROOT_TEACHERS      = 'LIST_OF_TEACHERS'
    character(len=16), parameter :: XML_ROOT_SECTIONS      = 'LIST_OF_SECTIONS'
    character(len=17), parameter :: XML_ROOT_CURRICULA     = 'LIST_OF_CURRICULA'
    character(len=21), parameter :: XML_ROOT_EQUIVALENCIES = 'LIST_OF_EQUIVALENCIES'
    character(len=14), parameter :: XML_ROOT_BLOCKS        = 'LIST_OF_BLOCKS'
    character(len=16), parameter :: XML_ROOT_STUDENTS      = 'LIST_OF_STUDENTS'
    character(len=11), parameter :: XML_ROOT_PREDICTIONS   = 'PREDICTIONS'
    character(len=11), parameter :: XML_ROOT_WAIVERS       = 'WAIVERS_COI'
    character(len=10), parameter :: XML_ROOT_ENLISTMENT    = 'ENLISTMENT'
    character(len=11), parameter :: XML_ROOT_GRADESHEETS   = 'GRADESHEETS'
    character(len=14), parameter :: XML_STUDENT_RECORD     = 'STUDENT_RECORD'
    character(len= 9), parameter :: XML_FAILRATES          = 'FAILRATES'
    character(len=15), parameter :: XML_INTAKE             = 'FRESHMAN_INTAKE'
    character(len=13), parameter :: XML_SUBSTITUTIONS      = 'SUBSTITUTIONS'
    ! root names                                              12345678901234567890

    ! indentation
    integer, parameter :: INDENT_INCR = 4 ! no. SPACEs for next indent
    integer             :: indent0 = INDENT_INCR, & ! indentation levels
        indent1 = INDENT_INCR*2, &
        indent2 = INDENT_INCR*3, &
        indent3 = INDENT_INCR*4, &
        indent4 = INDENT_INCR*5, &
        indent5 = INDENT_INCR*6
    character(len=80)  :: indentation = ' '

    logical            :: forReading = .true., QUIETLY = .true., noWrites = .false.

contains


    subroutine xml_open_file(device, rootName, fileName, errNo, readOnly, openQuietly)
        integer, intent (in) :: device
        character (len=*), intent (in) :: fileName, rootName
        integer, intent (in out) :: errNo
        logical, intent (in), optional :: readOnly, openQuietly
        character(len=MAX_LEN_XML_LINE) :: xmlLine
        logical :: asInput, beQuiet, rootFound
        integer :: eof

        errNo = 0
        if (present(readOnly)) then
            asInput = readOnly
        else
            asInput = .false.
        end if
        if (present(openQuietly)) then
            beQuiet = openQuietly
        else
            beQuiet = .false.
        end if

        if (.not. asInput) then ! for writing
            call file_io_log('Writing '//trim(fileName), beQuiet)
            call open_for_write(device, fileName)
            write(device,AFORMAT) '<?xml version="1.0" encoding="ISO-8859-1" ?>', '<'//rootName//'>'

        else ! for reading
            ! file exists ?
            inquire(file=fileName, exist=asInput)
            if (.not. asInput) then ! not there
                errNo = -1
                call file_io_log('File not found: '//fileName, beQuiet)
            else ! open & look for rootName in file
                open (unit=device, file=fileName, status='old', iostat=eof)
                call file_io_log('Status='//trim(itoa(eof))//' in reading '//fileName)
                rootFound = .false.
                do
                    read(device, AFORMAT, iostat=eof) xmlLine
                    if (eof<0) exit
                    if (index(xmlLine, '<'//rootName//'>') > 0) then
                        rootFound = .true.
                        exit
                    end if
                end do
                if (.not. rootFound) then
                    errNo = 1
                    call file_io_log('Not in file: <'//rootName//'>')
                end if
            end if
        end if
        return
    end subroutine xml_open_file


    subroutine xml_close_file(device, rootName)
        integer, intent (in) :: device
        character (len=*), intent (in), optional :: rootName
        if (present(rootName)) write(device,AFORMAT) '</'//rootName//'>'
        close(device)
        return
    end subroutine xml_close_file


    subroutine xml_write_character(device, indent, tag, value)
        integer, intent (in) :: device, indent
        character (len=*), intent (in) :: tag
        character (len=*), intent (in), optional :: value
        character(len=MAX_LEN_XML_LINE) :: xmlLine
        integer :: idx ! position of ' & ' in value

        if (present(value)) then ! convert ' & ' to ' and '
            xmlLine = value
            idx = index(xmlLine, ' & ')
            do while (idx>0)
                xmlLine = xmlLine(:idx)//'and'//xmlLine(idx+2:)
                idx = index(xmlLine, ' & ')
            end do
            write(device, AFORMAT) indentation(:indent)// &
                '<'//trim(tag)//'>'//trim(xmlLine)//'</'//trim(tag)//'>'
        else
            write(device, AFORMAT) indentation(:indent)//'<'//trim(tag)//'>'
        end if
        return
    end subroutine xml_write_character


    subroutine xml_write_integer(device, indent, tag, value)
        integer, intent (in) :: device, indent
        character (len=*), intent (in) :: tag
        integer, intent (in) :: value
        call xml_write_character(device, indent, tag, itoa(value))
        return
    end subroutine xml_write_integer


    subroutine xml_write_float(device, indent, tag, value, dadp)
        integer, intent (in) :: device, indent
        character (len=*), intent (in) :: tag
        real, intent (in) :: value
        integer, intent (in), optional :: dadp

        call xml_write_character(device, indent, tag, ftoa(value,dadp))
        return
    end subroutine xml_write_float


    subroutine xml_parse_line(line, tag, value, errNo)
        character(len=MAX_LEN_XML_LINE), intent (in out) :: line
        character(len=MAX_LEN_XML_TAG), intent (out) :: tag
        integer, intent (in out) :: errNo
        character(len=MAX_LEN_XML_LINE), intent (out) :: value
        ! locals
        integer :: lenLine ! last non-blank character in line
        integer :: nL, nR, pos(10,2) ! number & positions of < and >
        integer :: i

        ! initialize return values
        line = adjustl(line)
        tag = SPACE
        value = SPACE

        ! get positions of < and >
        lenLine = len_trim(line)
        nL = 0
        nR = 0
        pos = 0
        do i = 1,lenLine
            if (line(i:i)=='<') then
                nL = nL+1
                pos(nL,1) = i
            elseif (line(i:i)=='>') then
                nR = nR+1
                pos(nR,2) = i
            end if
        end do

        ! any < or > ?
        if (nL==0 .and. nR==0) return

        ! same number of < and > ?
        if (nL/=nR) then
            call file_log_message('Unmatched < or > in line '//trim(line))
            errNo = 701
            return
        end if

        ! assume line has <tag> only, or </tag> only, or <tag>value</tag>
        tag = line(pos(1,1)+1:pos(1,2)-1)
        if (nL>1) then ! <tag>value</tag>
            value = line(pos(1,2)+1:pos(2,1)-1)
        end if

        return
    end subroutine xml_parse_line


end module XML
