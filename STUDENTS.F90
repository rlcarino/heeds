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


module STUDENTS

    use CURRICULA

    implicit none

    integer, parameter :: &
        MAX_ALL_STUDENTS = 40000, & ! max no. of students
        MAX_LEN_STUDENT_CODE = 10, & ! length of student numbers
        MAX_LEN_STUDENT_NAME = 40 ! length of student names

    type :: TYPE_STUDENT
        character(len=MAX_LEN_STUDENT_CODE) :: StdNo
        character(len=MAX_LEN_STUDENT_NAME) :: Name
        character(len=1) :: Gender
        integer :: CountryIdx
        integer :: CurriculumIdx
        integer :: Classification
        integer :: Record(5,0:MAX_SUBJECTS_IN_CURRICULUM) ! 1=type,2=year,3=term,4=subject,5=grade
    end type TYPE_STUDENT

    type (TYPE_STUDENT), dimension (0:MAX_ALL_STUDENTS) :: Student
    integer, dimension (0:MAX_ALL_STUDENTS):: StdRank

    integer :: NumStudents
    logical :: isDirtySTUDENTS = .false.

    integer :: MaxLoad


    ! private tokens
    character (len=MAX_LEN_FILE_PATH), private :: fileName
    character (len=MAX_LEN_XML_LINE), private :: line
    integer, private :: unitNo=2, eof, ndels, pos(60)

contains


#include "custom_read_students.F90"


    subroutine read_students(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent (out) :: errNo

        integer :: iCurr, ierr, i, numEntries, partialEntries, numUpdates, mainEntries, previous
        logical :: noXML

        errNo = 0 ! errors or 'not found' are OK; there might be no students entered yet
        previous = NumStudents

        call xml_read_students (path, 0, mainEntries, ierr)
        noXML = mainEntries==0
        ! check for students added by program advisers
        numUpdates = 0
        done = .false.
        do iCurr=1,NumCurricula
            if (done(iCurr)) cycle
            call xml_read_students (path, iCurr, partialEntries, ierr, QUIETLY)
            numUpdates = numUpdates + partialEntries
            do i = iCurr+1,NumCurricula
                if (CurrProgCode(iCurr)==CurrProgCode(i)) done(i) = .true.
            end do
        end do
        numEntries = mainEntries+numUpdates

        if (numEntries==0) then ! no XML student files; try the custom format
            call custom_read_students(path, numEntries, ierr)
        end if
        if (NumStudents>previous) call sort_alphabetical_students()

        if ((noXML .and. numEntries>0) .or. numUpdates>0) then ! students were added; write the XML students file
            call xml_write_students(path, 0)
        end if

        return
    end subroutine read_students


    subroutine initialize_student(S)
        type (TYPE_STUDENT) :: S
        S = TYPE_STUDENT ('####-#####', '(not in directory)', SPACE, 1, 0, -1, 0)
        return
    end subroutine initialize_student


    subroutine insert_student(S, loc)
        type (TYPE_STUDENT) :: S
        integer, intent(out) :: loc

        NumStudents = NumStudents+1
        call check_array_bound (NumStudents, MAX_ALL_STUDENTS, 'MAX_ALL_STUDENTS')
        loc = NumStudents
        do while (loc>1 .and. Student(loc-1)%StdNo>S%StdNo)
            Student(loc) = Student(loc-1)
            loc = loc - 1
        end do
        Student(loc) = S
        !write(*,*) 'INSerted '//S%StdNo, ' to ', loc, '/', NumStudents
        return
    end subroutine insert_student


    subroutine update_student_info(S, idx)
        type (TYPE_STUDENT) :: S
        integer, intent(out) :: idx
        integer :: loc
        idx = index_to_student(S%StdNo)
        if (idx==0) then ! insert so that Students() is sorted
            call insert_student(S, loc)
            idx = -loc
        else
            Student(idx) = S
            !write(*,*) 'UPDated  '//S%StdNo, ' at ', idx, '/', NumStudents
        end if
        return
    end subroutine update_student_info


    subroutine sort_alphabetical_students()
        integer :: i, j, k

        write (*,*) 'Sorting students alphabetically... please wait...'
        StdRank = 0
        do i=1,NumStudents
            StdRank(i) = i
        end do
        do i=1,NumStudents-1
            if (mod(i,1000)==0) write(*,*) i, ' students...'
            do j=i+1,NumStudents
                if (Student(StdRank(i))%Name>Student(StdRank(j))%Name) then
                    k = StdRank(i)
                    StdRank(i) = StdRank(j)
                    StdRank(j) = k
                end if
            end do
        end do
        return
    end subroutine sort_alphabetical_students


    function index_to_student(StdNum)
        integer :: index_to_student
        character (len=MAX_LEN_STUDENT_CODE), intent (in) :: StdNum
        integer :: i, j, sdx
        i = 1
        j = NumStudents
        do
            if (i>j) then
                sdx = 0
                !write(*,*) 'Not found : '//StdNum
                exit
            else
                sdx = (i + j)/2
                if (StdNum ==Student(sdx)%StdNo) then
                    !write(*,*) 'Found '//StdNum//' at ', sdx
                    exit
                else if (StdNum <Student(sdx)%StdNo) then
                    !write(*,*) StdNum//' before '//Student(sdx)%StdNo, sdx
                    j = sdx-1
                else
                    i = sdx+1
                    !write(*,*) StdNum//' after '//Student(sdx)%StdNo, sdx
                end if
            end if
        end do
        index_to_student = sdx
        return
    end function index_to_student


    function text_student_info (std)
        character (len=127) :: text_student_info
        integer, intent (in) :: std
        integer :: idxCURR
        idxCURR = Student(std)%CurriculumIdx
        text_student_info = trim(Student(std)%StdNo)//COMMA//trim(Student(std)%Name)// &
        COMMA//trim(Student(std)%Gender)//COMMA// &
        trim(itoa(Student(std)%CountryIdx))//COMMA// &
        trim(Curriculum(idxCURR)%Code)//COMMA// &
        College(Curriculum(idxCURR)%CollegeIdx)%Code
        return
    end function text_student_info


    function text_student_curriculum(std)

        integer, intent (in) :: std

        character (len=MAX_LEN_STUDENT_CODE+MAX_LEN_STUDENT_NAME+ &
            MAX_LEN_CURRICULUM_CODE+MAX_LEN_CURRICULUM_NAME+MAX_LEN_CURRICULUM_NAME+&
            MAX_LEN_CURRICULUM_NAME) :: tmp, text_student_curriculum
        integer :: idxCURR

        idxCURR = Student(std)%CurriculumIdx
        tmp = SPACE
        if (Curriculum(idxCURR)%Remark/=SPACE)  &
        tmp = COMMA//SPACE//trim(Curriculum(idxCURR)%Remark)//tmp
        if (Curriculum(idxCURR)%Specialization/=SPACE)  &
        tmp = COMMA//SPACE//trim(Curriculum(idxCURR)%Specialization)//tmp
        tmp = trim(Student(std)%StdNo)//SPACE//trim(Student(std)%Name)//SPACE//dash//SPACE// &
            trim(Curriculum(idxCURR)%Title)//tmp
        text_student_curriculum = tmp

        return
    end function text_student_curriculum


    subroutine xml_write_students(path, iCurr)

        integer, intent (in) :: iCurr
        character(len=*), intent(in) :: path ! YEAR/TERM/

        integer :: std, idx

        ! training only?
        if (noWrites) return

        ! generate file name
        if (iCurr>0) then
            fileName = trim(dirXML)//trim(path)//'STUDENTS-'//trim(CurrProgCode(iCurr))//'.XML'
        else
            fileName = trim(dirXML)//trim(path)//'STUDENTS.XML'
        end if

        ! make backup & open new file
        call move_to_backup(fileName)
        call xml_open_file(unitNo, XML_ROOT_STUDENTS, fileName, std)
        write(unitNo,AFORMAT) &
        '    <comment>', &
        '        Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                    FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
        '        StdNo - Student number', &
        '        Name - Student name', &
        '        Gender - M=male, F=female', &
        '        Country - Country index; 1=Philippines', &
        '        Curriculum - Curriculum code', &
        '        Scholarship - Scholarship code', &
        '        Classification - 0=undetermined; 1=NF, 2=SO, 3=JR, 4=SR', &
        '    </comment>'

        ! loop over students
        do std=1,NumStudents
            idx = Student(std)%CurriculumIdx
            if (iCurr>0) then
                if (CurrProgCode(idx)/=CurrProgCode(iCurr)) cycle
            end if

            call xml_write_character(unitNo, indent0, 'Student')
            call xml_write_character(unitNo, indent1, 'StdNo', Student(std)%StdNo)
            call xml_write_character(unitNo, indent1, 'Name', Student(std)%Name)
            call xml_write_character(unitNo, indent1, 'Gender', Student(std)%Gender)
            if (Student(std)%CountryIdx/=1) &
                call xml_write_integer(unitNo,   indent1, 'Country', Student(std)%CountryIdx)
            call xml_write_character(unitNo, indent1, 'Curriculum', Curriculum(idx)%Code)
            if (Student(std)%Classification/=0) &
                call xml_write_integer(unitNo,   indent1, 'Classification', Student(std)%Classification)
            call xml_write_character(unitNo, indent0, '/Student')
        end do
        call xml_close_file(unitNo, XML_ROOT_STUDENTS)
        return
    end subroutine xml_write_students


    subroutine xml_read_students(path, iCurr, numEntries, errNo, openQuietly)

        character(len=*), intent(in) :: path ! YEAR/TERM/
        integer, intent (in) :: iCurr
        integer, intent (out) :: numEntries, errNo
        logical, intent (in), optional :: openQuietly

        type (TYPE_STUDENT) :: wrkStudent
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character (len=MAX_LEN_XML_LINE) :: value
        character (len=MAX_LEN_XML_TAG) :: tag
        logical :: quiet
        integer :: indexLoc, idxCurr

        ! generate file name
        if (iCurr>0) then
            fileName = trim(dirXML)//trim(path)//'STUDENTS-'//trim(CurrProgCode(iCurr))//'.XML'
        else
            fileName = trim(dirXML)//trim(path)//'STUDENTS.XML'
        end if

        if (present(openQuietly)) then
            quiet = openQuietly
        else
            quiet = .false.
        end if

        ! open file, return on any error
        numEntries = 0
        call xml_open_file(unitNo, XML_ROOT_STUDENTS, fileName, errNo, forReading, quiet)
        if (errNo/=0) return

        ! examine the file line by line
        do
            read(unitNo, AFORMAT, iostat=eof) line
            if (eof<0) exit
            ! get tag and value if any
            call xml_parse_line(line, tag, value, eof)
            if (eof/=0) exit
            select case (trim(tag))

                case ('Student')
                    call initialize_student(wrkStudent)

                case ('StdNo')
                    call upper_case(value)
                    wrkStudent%StdNo = adjustl(value)

                case ('Name')
                    call upper_case(value)
                    wrkStudent%Name = adjustl(value)

                case ('Gender')
                    wrkStudent%Gender = adjustl(value)

                case ('Curriculum')
                    tCurriculum = adjustl(value)
                    idxCurr = index_to_curriculum(tCurriculum)
                    if (idxCurr<0) then
                        idxCurr = -idxCurr
                    else if (idxCurr==0) then
                        idxCurr = NumCurricula
                    end if
                    wrkStudent%CurriculumIdx = idxCurr

                case ('Country')
                    wrkStudent%CountryIdx = atoi(value)

                case ('Classification')
                    wrkStudent%Classification = atoi(value)

                case ('/Student')
                    call update_student_info(wrkStudent, indexLoc)
                    if (indexLoc<0) numEntries = numEntries + 1

                case default
                    ! do nothing
            end select
        end do
        call xml_close_file(unitNo)
        call file_log_message (itoa(numEntries)//' students in '//fileName)

        return
    end subroutine xml_read_students


end module STUDENTS
