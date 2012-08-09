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


module TEACHERS

    use DEPARTMENTS

    implicit none

    ! faculty variables
    integer, parameter :: &
        MAX_ALL_TEACHERS = 2000, & ! maximum number of eachers
        MAX_LEN_TEACHER_CODE=20, & ! length of login name
        MAX_LEN_TEACHER_NAME=40 ! length of Teacher name

    type :: TYPE_TEACHER
        character (len=MAX_LEN_TEACHER_CODE) :: TeacherId
        character (len=MAX_LEN_TEACHER_NAME) :: Name
        integer :: DeptIdx, MaxLoad
    end type TYPE_TEACHER

    integer :: NumTeachers, NumAdditionalTeachers

    type (TYPE_TEACHER), dimension(0:MAX_ALL_TEACHERS) :: Teacher
    integer, dimension(0:MAX_ALL_TEACHERS) :: TeacherRank

    ! private tokens
    character (len=MAX_LEN_FILE_PATH), private :: fileName
    character (len=MAX_LEN_XML_LINE), private :: line
    integer, private :: unitNum=2, eof, ndels, pos(30)

contains

#include "custom_read_teachers.F90"

    subroutine read_teachers(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent (out) :: errNo

        logical :: noXML

        NumTeachers = 0
        NumAdditionalTeachers = 0

        call initialize_teacher(Teacher(0))
        Teacher = Teacher(0)
        Teacher(0)%TeacherId = 'TBA'
        Teacher(0)%Name = '(Teacher to be assigned)'

        noXML = .false.
        call xml_read_teachers(path, errNo) ! try the XML file
        if (errNo/=0) then ! something wrong with XML file
            noXML = .true.
            call custom_read_teachers(path, errNo) ! try custom format
            if (errNo/=0) return ! something still wrong
        end if

        call sort_alphabetical_teachers()

        ! write the XML TEACHERS file?
        if (noXML) call xml_write_teachers(path)

        return
    end subroutine read_teachers



    subroutine initialize_teacher (wrkTeacher, tCode, tName, iDept, iLoad)

        type(TYPE_TEACHER), intent (out) :: wrkTeacher
        character(len=*), intent (in), optional :: tCode, tName
        integer, intent (in), optional :: iDept, iLoad

        if (present(tCode)) then
            wrkTeacher = TYPE_TEACHER(tCode, tName, iDept, iLoad)

        else ! place teacher under REGISTRAR
            wrkTeacher = TYPE_TEACHER(SPACE, SPACE, NumDepartments, 12)

        end if

        return
    end subroutine initialize_teacher


    function index_to_teacher (token)

        integer :: index_to_teacher
        character (len=MAX_LEN_TEACHER_CODE), intent (in) :: token

        integer :: i, j, tdx

        tdx = 0
        if (token==Teacher(tdx)%TeacherId) then
            index_to_teacher = tdx
            return
        end if

        ! try the newly added Teachers
        do tdx=NumTeachers+1,NumTeachers+NumAdditionalTeachers
            if (token==Teacher(tdx)%TeacherId) then
                index_to_teacher = tdx
                return
            end if
        end do

        ! try the original teachers
        i = 1
        j = NumTeachers
        do
            if (i>j) then
                tdx = 0
                exit
            else
                tdx = (i + j)/2
                if (token==Teacher(tdx)%TeacherId) then
                    exit
                else if (token<Teacher(tdx)%TeacherId) then
                    j = tdx-1
                else
                    i = tdx+1
                end if
            end if
        end do
        index_to_teacher = tdx

        return
    end function index_to_teacher


    subroutine sort_teachers

        integer :: i, j
        type(TYPE_TEACHER) :: wrkTeacher

        do i=1,NumTeachers-1
            do j=i+1,NumTeachers
                if (Teacher(i)%TeacherId>Teacher(j)%TeacherId) then
                    wrkTeacher = Teacher(i)
                    Teacher(i) = Teacher(j)
                    Teacher(j) = wrkTeacher
                end if
            end do
        end do

        return
    end subroutine sort_teachers


    subroutine sort_alphabetical_teachers()

        integer :: i, j, k
        write (*,*) 'Sorting teachers alphabetically... please wait...'

        TeacherRank = 0
        do i=1,NumTeachers
            TeacherRank(i) = i
        end do

        do i=1,NumTeachers-1

            do j=i+1,NumTeachers
                if (Teacher(TeacherRank(i))%Name > Teacher(TeacherRank(j))%Name) then
                    k = TeacherRank(i)
                    TeacherRank(i) = TeacherRank(j)
                    TeacherRank(j) = k
                end if
            end do

        end do

        return
    end subroutine sort_alphabetical_teachers


    subroutine xml_write_teachers(path)

        character(len=*), intent(in) :: path
        integer :: ldx

        fileName = trim(dirXML)//trim(path)//'TEACHERS.XML'
        call xml_open_file(unitNum, XML_ROOT_TEACHERS, fileName, ldx)

        write(unitNum,AFORMAT) &
        '    <comment>', &
        '        Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                    FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
        '        TeacherId - teacher code', &
        '        Name - name of teacher', &
        '        Department - responsible department', &
        '        MaxLoad - maximum teaching load', &
        '    </comment>'

        do ldx = 1,NumTeachers+NumAdditionalTeachers

            call xml_write_character(unitNum, indent0, 'Teacher')
            call xml_write_character(unitNum, indent1, 'TeacherId', Teacher(ldx)%TeacherId)
            call xml_write_character(unitNum, indent1, 'Name', Teacher(ldx)%Name)
            call xml_write_character(unitNum, indent1, 'Department', Department(Teacher(ldx)%DeptIdx)%Code)
            call xml_write_integer(unitNum, indent1, 'MaxLoad', Teacher(ldx)%MaxLoad)
            call xml_write_character(unitNum, indent0, '/Teacher')

        end do

        call xml_close_file(unitNum, XML_ROOT_TEACHERS)

        return
    end subroutine xml_write_teachers


    subroutine xml_read_teachers(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent (out) :: errNo

        integer :: j
        character(len=MAX_LEN_XML_LINE) :: value
        character(len=MAX_LEN_XML_TAG) :: tag
        type(TYPE_TEACHER) :: wrkTeacher
        character (len=MAX_LEN_DEPARTMENT_CODE) :: tDept

        ! open file, return on any error
        fileName = trim(dirXML)//trim(path)//'TEACHERS.XML'
        call xml_open_file(unitNum, XML_ROOT_TEACHERS, fileName, errNo, forReading)
        if (errNo/=0) return

        ! examine the file line by line
        do

            read(unitNum, AFORMAT, iostat=eof) line
            if (eof<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, eof)
            if (eof/=0) exit

            select case (trim(tag))

                case ('Teacher') ! initialize temporary teacher data
                    call initialize_teacher(wrkTeacher)

                case ('TeacherId')
                    wrkTeacher%TeacherId = adjustl(value)

                case ('Name')
                    wrkTeacher%Name = adjustl(value)

                case ('Department')
                    tDept = adjustl(value)
                    j = index_to_dept(tDept)
                    if (j==0) j = NumDepartments ! use REGISTRAR for invalid department code
                    wrkTeacher%DeptIdx = j

                case ('MaxLoad')
                    wrkTeacher%MaxLoad = atoi(value)

                case ('/Teacher') ! add temporary teacher data to Teacher()
                    NumTeachers = NumTeachers + 1
                    call check_array_bound (NumTeachers, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')
                    Teacher(NumTeachers) = wrkTeacher
                    Department(wrkTeacher%DeptIdx)%hasInfo = .true.


                case default
                    ! do nothing
            end select

        end do

        call xml_close_file(unitNum)

        call sort_teachers()

        return
    end subroutine xml_read_teachers


end module TEACHERS
