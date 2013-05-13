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


subroutine custom_read_teachers(path, errNo)

    character(len=*), intent(in) :: path
    integer, intent(out) :: errNo

    character (len=MAX_LEN_PERSON_NAME) :: tTeacher
    character (len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
    integer :: i, j, k
    character (len=1) :: ch
    type(TYPE_TEACHER) :: wrkTeacher

    fileName = trim(dirDATA)//trim(path)//'TEACHERS.CSV'
    open (unit=unitRAW, file=fileName, status='old', iostat=errNo)
    if (errNo/=0) return

    call file_log_message('Retrieving teachers from '//fileName)
    ! skip first line
    read(unitRAW, AFORMAT) line

    do

        read(unitRAW, AFORMAT, iostat=eof) line
        if (eof<0) exit
        if (line==SPACE .or. line(1:1)=='#') cycle

        call index_to_delimiters('"', line, ndels, pos)
        !id,code,name,deptcd,speccd
        !429,"ABAG","Abaleta, Gladys","",""
        !145,"ABAJ","Abaleta, Jay A.","",""
        !295,"ABED","Abella, Dante","CAHS",""
        !198,"ABEO","Abella, Orlando","",""
        !331,"ABEW","Abena, Winston","CAHS","SPECIALIZATION"
        !1   2    3 4              5 6    7

        call initialize_teacher(wrkTeacher)

        ! remove punctuation
        tTeacher = SPACE
        j = 0
        do i=pos(4)+1,pos(5)-1
            ch = line(i:i)
            if (index(SPECIAL,ch)>0) cycle
            !if ((ch>='A' .and. ch<='Z') .or. (ch>='a' .and. ch<='z') .or. ch==SPACE .or. ch==DASH) then
            j = j+1
            tTeacher(j:j) = ch
        !end if
        end do

        call upper_case(tTeacher)
        j = index_to_teacher(tTeacher)
        if (j>0) cycle ! already encountered
        tDepartment = line(pos(6)+1:pos(7)-1)
        k = index_to_dept(tDepartment)
        if (k==0) k = NumDepartments ! refers to REGISTRAR

        wrkTeacher%TeacherId = tTeacher
        wrkTeacher%Name = tTeacher
        wrkTeacher%DeptIdx = k
        wrkTeacher%MaxLoad = 21
        Department(k)%hasInfo = .true.

        call check_array_bound (NumTeachers+NumAdditionalTeachers+1, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')

        do i=NumTeachers,1,-1
            if (tTeacher<Teacher(i)%TeacherId) then
                Teacher(i+1) = Teacher(i)
            else
                j = i
                exit
            end if
        end do

        Teacher(j+1) = wrkTeacher
        NumTeachers = NumTeachers + 1

    end do

    close(unitRAW)

    return
end subroutine custom_read_teachers

