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


subroutine custom_read_university(path, errNo)

    character(len=*), intent(in) :: path
    integer, intent(out) :: errNo

    fileName = trim(dirRAW)//trim(path)//'UNIVERSITY'
    open (unit=unitNum, file=fileName, status='old', iostat=errNo)
    if (errNo/=0) return

    call file_log_message('Retrieving university info from '//fileName)

    do
        read(unitNum, AFORMAT, iostat=eof) line
        if (eof<0) exit
        if (line==SPACE .or. line(1:1)=='#') cycle

        call index_to_delimiters(COMMA, line, ndels, pos)
        select case (line(:pos(2)-1))
            case ('NAME')
                UniversityName = line(pos(2)+1:)
            case ('ADDRESS')
                UniversityAddress = line(pos(2)+1:)
            case ('ADMINISTRATION')
                ADMINISTRATION = line(pos(2)+1:)
            case ('REGISTRAR')
                REGISTRAR = line(pos(2)+1:)
            case ('BASEYEAR')
                baseYear = atoi(trim(line(pos(2)+1:)))
        end select

    end do
    close(unitNum)

    return
end subroutine custom_read_university


subroutine custom_read_colleges(path, errNo)
    !id,code,name,sched,number,orperiod,orexam
    !6,"CA","College of Agriculture","C","","11-1"," "
    !5,"CAS","College of Arts and Sciences","A","","11-1"," "
    !1 2   3 4                            5

    character(len=*), intent(in) :: path
    integer, intent(out) :: errNo

    fileName = trim(dirRAW)//trim(path)//'COLLEGES.CSV'
    open (unit=unitNum, file=fileName, status='old', iostat=errNo)
    if (errNo/=0) return

    call file_log_message('Retrieving college codes from '//fileName)
    ! skip first line
    read(unitNum, AFORMAT) line

    do
        read(unitNum, AFORMAT, iostat=eof) line

        if (eof<0) exit
        if (line==SPACE .or. line(1:1)=='#') cycle

        call index_to_delimiters('"', line, ndels, pos)

        if (index(line,ADMINISTRATION)>0) cycle ! add later

        NumColleges = NumColleges + 1
        call check_array_bound (NumColleges, MAX_ALL_COLLEGES, 'MAX_ALL_COLLEGES')
        College(NumColleges)%Code = line(pos(2)+1:pos(3)-1)
        College(NumColleges)%Name = line(pos(4)+1:pos(5)-1)

    end do

    close(unitNum)

    return
end subroutine custom_read_colleges

