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



subroutine custom_read_departments (path, errNo)

    character(len=*), intent(in) :: path
    integer, intent (out) :: errNo

    integer :: ldx
    character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
    character (len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

    fileName = trim(dirDATA)//trim(path)//'DEPARTMENTS.CSV'
    open (unit=unitRAW, file=fileName, status='old', iostat=errNo)
    if (errNo/=0) return

    call file_log_message('Retrieving department info from '//fileName)

    ! skip first line
    read(unitRAW, AFORMAT) line

    do

        read(unitRAW, AFORMAT, iostat=eof) line
        if (eof<0) exit
        if (line==SPACE .or. line(1:1)=='#') cycle

        !id,code,name,sched,number,orperiod,orexam
        !6,"CA","College of Agriculture","C","","11-1"," "
        !5,"CAS","College of Arts and Sciences","A","","11-1"," "
        !1 2   3 4                            5
        call index_to_delimiters('"', line, ndels, pos)

        tDepartment = line(pos(2)+1:pos(3)-1)
        if (index(tDepartment,trim(REGISTRAR))>0) cycle ! add at the end

        tCollege = tDepartment
        ldx = index_to_college(tCollege)
        NumDepartments = NumDepartments + 1
        call check_array_bound (NumDepartments, MAX_ALL_DEPARTMENTS, 'MAX_ALL_DEPARTMENTS')
        Department(NumDepartments)%Code = tDepartment
        Department(NumDepartments)%Name = line(pos(4)+1:pos(5)-1)
        Department(NumDepartments)%SectionPrefix = line(pos(6)+1:pos(7)-1)
        if (Department(NumDepartments)%SectionPrefix==SPACE) Department(NumDepartments)%SectionPrefix = '#'
        Department(NumDepartments)%CollegeIdx = ldx

    end do

    close(unitRAW)

    return
end subroutine custom_read_departments

