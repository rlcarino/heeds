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


module TEACHERS

    use DEPARTMENTS

    implicit none

    ! faculty variables
    integer, parameter :: &
        MAX_ALL_TEACHERS = 2000, & ! maximum number of teachers
        MAX_LEN_TEACHER_CODE=20, & ! length of login name
        MAX_LEN_TEACHER_DEGREE=80, & ! length of string for Teacher degrees
        MAX_LEN_ACADEMIC_RANK=19, & ! length of string for academic rank
        MAX_LEN_STEP_IN_RANK=6, & ! length of string for step in academic rank
        MAX_LEN_PASSWD_VAR=32, &  ! length of password variables
        MIN_LEN_PASSWORD=8, &
        MAX_LEN_PASSWORD=12, &
        MAX_LEN_ROLE = 20


    type :: TYPE_TEACHER
        character (len=MAX_LEN_TEACHER_CODE)   :: TeacherId
        character (len=MAX_LEN_PERSON_NAME)    :: Name
        integer                                :: DeptIdx, MaxLoad, Rank, Step
        character (len=MAX_LEN_TEACHER_DEGREE) :: Bachelor, Master, Doctorate, Specialization
        character (len=MAX_LEN_PASSWD_VAR)     :: Password ! Encrypted password
        character (len=MAX_LEN_ROLE):: Role     ! Guest; set by Admin
        integer                                :: Status   ! 0=logged out, 1=logged in
    end type TYPE_TEACHER

    type (TYPE_TEACHER), dimension(0:MAX_ALL_TEACHERS) :: Teacher
    integer, dimension(0:MAX_ALL_TEACHERS) :: TeacherRank
    integer :: NumTeachers, NumAdditionalTeachers

    character (len=MAX_LEN_ACADEMIC_RANK), dimension(0:4) :: AcademicRank = (/ &
        '(Specify Rank)     ', &
        'Instructor         ', &
        'Assistant Professor', &
        'Associate Professor', &
        'Professor          ' /)

    character (len=MAX_LEN_STEP_IN_RANK), dimension(0:12) :: RankStep = (/ &
        '(Step)', &
        'I     ', &
        'II    ', &
        'III   ', &
        'IV    ', &
        'V     ', &
        'VI    ', &
        'VII   ', &
        'VIII  ', &
        'IX    ', &
        'X     ', &
        'XI    ', &
        'XII   ' /)

    ! Users
    character (len=MAX_LEN_TEACHER_CODE) :: USERNAME, GUEST = 'Guest'
    character (len=MAX_LEN_PERSON_NAME) :: ROLE
    integer :: DeptIdxUser, CollegeIdxUser, CurriculumIdxUser

    ! private tokens
    character (len=MAX_LEN_FILE_PATH), private :: fileName
    character (len=MAX_LEN_XML_LINE), private :: line
    integer, private :: eof, ndels, pos(30)

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

        ! additional teacher info
        call xml_read_teachers_other(path, errNo)

        call sort_alphabetical_teachers()

        ! write the XML TEACHERS file?
        if (noXML .and. NumTeachers>0) call xml_write_teachers(path)


    end subroutine read_teachers


    subroutine initialize_teacher (wrkTeacher)

        type(TYPE_TEACHER), intent (out) :: wrkTeacher

        wrkTeacher = TYPE_TEACHER(SPACE, SPACE, NumDepartments, 0, 0, 0, SPACE, SPACE, SPACE, SPACE, &
                SPACE, GUEST, 0)


    end subroutine initialize_teacher


    function index_to_teacher (token)

        integer :: index_to_teacher
        character (len=MAX_LEN_TEACHER_CODE), intent (in) :: token

        integer :: i, j!, tdx

        index_to_teacher = 0
        if (len_trim(token)==0) return

        ! sequential search
        j = 0
        do i=0,NumTeachers
            if (token==Teacher(i)%TeacherId) then
                j = i
                exit
            end if
        end do
        index_to_teacher = j

!        ! try the newly added Teachers
!        do tdx=NumTeachers+1,NumTeachers+NumAdditionalTeachers
!            if (token==Teacher(tdx)%TeacherId) then
!                index_to_teacher = tdx
!                return
!            end if
!        end do
!
!        ! try the original teachers
!        i = 1
!        j = NumTeachers
!        do
!            if (i>j) then
!                tdx = 0
!                exit
!            else
!                tdx = (i + j)/2
!                if (token==Teacher(tdx)%TeacherId) then
!                    exit
!                else if (token<Teacher(tdx)%TeacherId) then
!                    j = tdx-1
!                else
!                    i = tdx+1
!                end if
!            end if
!        end do
!        index_to_teacher = tdx


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


    end subroutine sort_teachers


    subroutine sort_alphabetical_teachers()

        integer :: i, j, k
        !write (*,*) 'Sorting teachers alphabetically... please wait...'

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


    end subroutine sort_alphabetical_teachers


    subroutine xml_write_teachers(path, dirOPT)

        character(len=*), intent(in) :: path
        character(len=*), intent(in), optional :: dirOPT
        integer :: ldx

        ! training only?
        if (noWrites) return

        ! basic info
        if (present(dirOPT)) then
            fileName = trim(dirOPT)//trim(path)//'TEACHERS.XML'
        else
            fileName = trim(dirDATA)//trim(path)//'TEACHERS.XML'
        endif

        call xml_open_file(unitXML, XML_ROOT_TEACHERS, fileName, ldx)

        write(unitXML,AFORMAT) &
        '    <comment>', &
        '        Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                    FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
        '        TeacherId - teacher code', &
        '        Name - name of teacher', &
        '        Department - responsible department', &
        '    </comment>'

        do ldx = 1,NumTeachers+NumAdditionalTeachers

            call xml_write_character(unitXML, indent0, 'Teacher')
            call xml_write_character(unitXML, indent1, 'TeacherId', Teacher(ldx)%TeacherId)
            call xml_write_character(unitXML, indent1, 'Name', Teacher(ldx)%Name)
            call xml_write_character(unitXML, indent1, 'Department', Department(Teacher(ldx)%DeptIdx)%Code)
            if (Teacher(ldx)%MaxLoad>0) &
                call xml_write_integer(unitXML, indent1, 'MaxLoad', Teacher(ldx)%MaxLoad)
            if (Teacher(ldx)%Rank>0) &
                call xml_write_character(unitXML, indent1, 'Rank', AcademicRank(Teacher(ldx)%Rank))
            if (Teacher(ldx)%Step>0) &
                call xml_write_character(unitXML, indent1, 'Step', RankStep(Teacher(ldx)%Step))
            if (Teacher(ldx)%Bachelor/=SPACE) &
                call xml_write_character(unitXML, indent1, 'Bachelor', Teacher(ldx)%Bachelor)
            if (Teacher(ldx)%Master/=SPACE) &
                call xml_write_character(unitXML, indent1, 'Master', Teacher(ldx)%Master)
            if (Teacher(ldx)%Doctorate/=SPACE)  &
                call xml_write_character(unitXML, indent1, 'Doctorate', Teacher(ldx)%Doctorate)
            if (Teacher(ldx)%Specialization/=SPACE)  &
                call xml_write_character(unitXML, indent1, 'Specialization', Teacher(ldx)%Specialization)
            if (Teacher(ldx)%Password/=SPACE)  &
                call xml_write_character(unitXML, indent1, 'Password', Teacher(ldx)%Password)
            if (Teacher(ldx)%Role/=SPACE)  &
                call xml_write_character(unitXML, indent1, 'Role', Teacher(ldx)%Role)
            call xml_write_character(unitXML, indent0, '/Teacher')

        end do

        call xml_close_file(unitXML, XML_ROOT_TEACHERS)


    end subroutine xml_write_teachers


    subroutine xml_read_teachers(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent (out) :: errNo

        integer :: i, j
        character(len=MAX_LEN_XML_LINE) :: value
        character(len=MAX_LEN_XML_TAG) :: tag
        type(TYPE_TEACHER) :: wrkTeacher
        character (len=MAX_LEN_DEPARTMENT_CODE) :: tDept
        character (len=MAX_LEN_STEP_IN_RANK) :: tStep
        character (len=MAX_LEN_ACADEMIC_RANK) :: tRank

        ! open file, return on any error
        fileName = trim(dirDATA)//trim(path)//'TEACHERS.XML'
        call xml_open_file(unitXML, XML_ROOT_TEACHERS, fileName, errNo, forReading)
        if (errNo/=0) return

        ! examine the file line by line
        do

            read(unitXML, AFORMAT, iostat=eof) line
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
                    call upper_case(value)
                    wrkTeacher%Name = adjustl(value)

                case ('Department')
                    tDept = adjustl(value)
                    j = index_to_dept(tDept)
                    if (j==0) j = NumDepartments ! use REGISTRAR for invalid department code
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

                case ('Role')
                    wrkTeacher%Role = adjustl(value)

                case ('/Teacher') ! add/merge temporary teacher data to Teacher()
                    ! teacher encountered previously?
                    i = 0
                    do j=1,NumTeachers
                        if (wrkTeacher%TeacherId/=Teacher(j)%TeacherId) cycle
                        i = j
                        exit
                    end do
                    if (i/=0) then ! overwrite existing record
                        call file_log_message ('Duplicate record for '//trim(wrkTeacher%TeacherId)//'; updated.')
                        Teacher(i) = wrkTeacher
                    else
                        NumTeachers = NumTeachers + 1
                        call check_array_bound (NumTeachers, MAX_ALL_TEACHERS, 'MAX_ALL_TEACHERS')
                        Teacher(NumTeachers) = wrkTeacher
                        if (wrkTeacher%TeacherID/=wrkTeacher%Role) Department(wrkTeacher%DeptIdx)%hasInfo = .true.
                    end if

                case default
                    ! do nothing
            end select

        end do

        call xml_close_file(unitXML)
        call file_log_message (itoa(NumTeachers)//' entries in '//fileName)

        call sort_teachers()


    end subroutine xml_read_teachers


    subroutine xml_read_teachers_other(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent (out) :: errNo

        integer :: i, j
        character(len=MAX_LEN_XML_LINE) :: value
        character(len=MAX_LEN_XML_TAG) :: tag
        type(TYPE_TEACHER) :: wrkTeacher
        character (len=MAX_LEN_STEP_IN_RANK) :: tStep
        character (len=MAX_LEN_ACADEMIC_RANK) :: tRank

        ! open file, return on any error
        errNo = 0
        fileName = trim(dirDATA)//trim(path)//'TEACHERS-OTHER.XML'
        call xml_open_file(unitETC, XML_ROOT_TEACHERS, fileName, eof, forReading)
        if (eof/=0) return

        ! examine the file line by line
        do

            read(unitETC, AFORMAT, iostat=eof) line
            if (eof<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, eof)
            if (eof/=0) exit

            select case (trim(tag))

                case ('Teacher')
                    ! ignore

                case ('TeacherId') ! copy teacher data
                    wrkTeacher%TeacherId = adjustl(value)
                    j = index_to_teacher(wrkTeacher%TeacherId)
                    wrkTeacher = Teacher(j)

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

                case ('Role')
                    wrkTeacher%Role = adjustl(value)

                case ('/Teacher') ! update  Teacher() with temporary teacher data
                    j = index_to_teacher(wrkTeacher%TeacherId)
                    Teacher(j) = wrkTeacher

                case default
                    ! do nothing
            end select

        end do

        call xml_close_file(unitETC)


    end subroutine xml_read_teachers_other


    subroutine set_password(Password, forcedPassword)
        character (len=MAX_LEN_PASSWD_VAR), intent (in out) :: Password
        character (len=*), intent (in), optional :: forcedPassword
        integer :: i, j, lenP, lenS
        real :: harvest ! random number
        character (len=34) :: choice = 'A2B3D45F6G7H8J9bcdfghjkmnprstvwxyz'

        if (present(forcedPassword)) then
            Password = forcedPassword
            lenP = len_trim(Password)
            if (lenP>MAX_LEN_PASSWORD) then ! force to be at most MAX_LEN_PASSWORD characters
                lenP = MAX_LEN_PASSWORD
                Password(lenP+1:) = SPACE
            end if
        else ! generate password with MIN_LEN_PASSWORD to MAX_LEN_PASSWORD characters
            call random_number(harvest)
            lenP = MIN_LEN_PASSWORD + int(harvest*(MAX_LEN_PASSWORD-MIN_LEN_PASSWORD+1))
            do i=1,lenP
                call random_number(harvest)
                j = 1 + int(harvest*34)
                Password(i:i) = choice(j:j)
            end do
        end if
        ! add salt
        lenS = lenPasswordEncryptionKey-lenP ! length of salt
        Password(lenS+1:) = Password(:lenP) ! right-justify Password() to make space for salt
        do i=1,lenS ! generate random salt
            call random_number(harvest)
            j = 1 + int(harvest*35)
            Password(i:i) = choice(j:j)
        end do
        ! add length to beginning+1
        Password(3:4) = itoa2bz(lenP)
        !write(*,*) lenS, Password(:lenS), lenP, Password(lenS+1:)!, lenS+lenP, Password
        ! encrypt salted password
        call encrypt(passwordEncryptionKey, Password)

    end subroutine set_password


    subroutine get_password(tdx, Password)
        integer, intent (in) :: tdx
        character (len=MAX_LEN_PASSWD_VAR), intent (out) :: Password

        Password = Teacher(tdx)%Password
        call decrypt(passwordEncryptionKey, Password)
        Password = Password(lenPasswordEncryptionKey-atoi(Password(3:4))+1:)


    end subroutine get_password


    function is_password(tdx, Password)
        logical :: is_password
        integer, intent (in) :: tdx
        character (len=MAX_LEN_PASSWD_VAR), intent (in) :: Password
        character (len=MAX_LEN_PASSWD_VAR) :: tPassword

        call get_password(tdx, tPassword)
        is_password = tPassword(:MAX_LEN_PASSWORD)==Password(:MAX_LEN_PASSWORD)


    end function is_password


    subroutine write_password_file(path)
        character (len=*), intent (in) :: path
        integer :: i!, k, kStart
        character (len=MAX_LEN_PASSWD_VAR) :: Password

        ! write CSV password file
        open(unit=unitETC, file=trim(dirDATA)//trim(path)//'PASSWORDS.CSV', form='formatted', status='unknown')
        write(unitETC,AFORMAT) &
            '#', &
            '#  !!!!!!!!! FOR THE REGISTRAR''S EYES ONLY !!!!!!!!! ', &
            '#', &
            '# Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                        FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
            '# NAME - name of teacher', &
            '# UNIT - academic unit', &
            '# USERNAME - HEEDS login name', &
            '# PASSWORD - HEEDS password', &
            '# ROLE - HEEDS role', &
            '# COMMENT - Date received/Initials', &
            '#', &
            '"NAME","UNIT","USERNAME","PASSWORD","ROLE","COMMENT"'

        do i=1,NumTeachers+NumAdditionalTeachers
            call get_password(i, Password)
!            if (trim(Teacher(i)%Role)==trim(REGISTRAR) ) then
!            else
                write(unitETC,AFORMAT) &
                    '"'//Teacher(i)%Name//'","'// &
                         Department(Teacher(i)%DeptIdx)%Code//'","'// &
                         Teacher(i)%TeacherId//'","'// &
                         '(Ask the Registrar)","'// & !Password//'","'// &
                         Teacher(i)%Role//'"'
!            end if
        end do
        close (unitETC)


    end subroutine write_password_file


end module TEACHERS
