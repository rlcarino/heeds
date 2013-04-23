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


module SECTIONS

    use SUBJECTS
    use TEACHERS
    use ROOMS

    implicit none

    integer, parameter :: &
        MAX_ALL_SECTIONS = 6000, &     ! all sections offered in a given term
        MAX_LEN_SECTION_CODE = 20, &      ! length of section codes
        MAX_ALL_SECTION_CODES = 1000, &      ! section names
        MAX_SECTION_MEETINGS = 12       ! maximum no. of meetings of a section in a week

    integer, parameter :: &
        MAX_LEN_CLASS_ID = MAX_LEN_SUBJECT_CODE+MAX_LEN_SECTION_CODE, &      ! length of section codes
        MAX_LEN_BLOCK_CODE = MAX_LEN_CLASS_ID

    type :: TYPE_SECTION
        character (len=MAX_LEN_CLASS_ID) :: ClassId
        character (len=MAX_LEN_SECTION_CODE) :: Code
        character (len=MAX_LEN_BLOCK_CODE) :: BlockID
        integer :: DeptIdx, SubjectIdx, Slots, RemSlots, NMeets
        integer, dimension(0:MAX_SECTION_MEETINGS) :: DayIdx, bTimeIdx, eTimeIdx, RoomIdx, TeacherIdx
    end type TYPE_SECTION
    type (TYPE_SECTION), dimension (3,0:MAX_ALL_SECTIONS) :: Section
    integer :: NumSections(3)

    logical :: UseCLASSES = .false.

    ! private tokens
    character (len=MAX_LEN_FILE_PATH), private :: fileName
    character (len=MAX_LEN_XML_LINE), private :: line
    integer, private :: eof, ndels, pos(30)

contains

#include "custom_read_classes.F90"


    subroutine initialize_section(S)
        type (TYPE_SECTION) :: S
        S = TYPE_SECTION (SPACE, SPACE, SPACE, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        return
    end subroutine initialize_section


    function index_to_section(tSection, NumSections, Section)
        integer :: index_to_section
        character (len=MAX_LEN_CLASS_ID), intent (in) :: tSection
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumSections
        integer :: i, sdx
        sdx = 0
        do i=1,NumSections
            if (tSection/=Section(i)%ClassId) cycle
            sdx = i
            exit
        end do
        index_to_section = sdx
        return
    end function index_to_section


    function is_lecture_class(sect, Section)
        integer, intent (in) :: sect
        type (TYPE_SECTION), intent(in) :: Section(0:)
        logical :: is_lecture_class
        ! returns true if sect is a lecture class (no DASH in section code)
        is_lecture_class = index(Section(sect)%Code,DASH)==0 .or. &
           Subject(Section(sect)%SubjectIdx)%Name(1:3)=='PE '
        return
    end function is_lecture_class


    function text_days_of_section(sect, NumSections, Section)
        integer, intent(in) :: sect
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumSections
        character (len=7) :: line, text_days_of_section
        integer :: j
        if (Section(sect)%NMeets>0) then
            line = SPACE
            do j=Section(sect)%NMeets,1,-1
                select case (Section(sect)%DayIdx(j))
                    case (0)
                        line = 'TBA'
                    case (1)
                        line = 'M'//line
                    case (2)
                        line = 'T'//line
                    case (3)
                        line = 'W'//line
                    case (4)
                        line = 'Th'//line
                    case (5)
                        line = 'F'//line
                    case (6)
                        line = 'S'//line
                end select
            end do
        else
            line = 'TBA'
        end if
        text_days_of_section = line
        return
    end function text_days_of_section


    subroutine offerings_sort(NumSections, Section)
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        integer, intent (in out) :: NumSections
        integer :: i, j
        do i=1,NumSections-1
            do j=i+1,NumSections
                if (Section(i)%ClassId>Section(j)%ClassId) then
                    Section(0) = Section(i)
                    Section(i) = Section(j)
                    Section(j) = Section(0)
                end if
            end do
        end do
        call initialize_section(Section(0))
        return
    end subroutine offerings_sort


    subroutine offerings_summarize(NumSections, Section, Offering, DeptIdx)
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumSections
        type (TYPE_OFFERED_SUBJECTS), intent(out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        integer, intent (in), optional :: DeptIdx
        integer :: k, l, filter_dept

        if (present(DeptIdx)) then
            filter_dept = DeptIdx
        else
            filter_dept = 0
        end if
        Offering = TYPE_OFFERED_SUBJECTS (0, 0, 0, 0, 0, 0, 0, 0)
        do k=1,NumSections
            if (index(Section(k)%Code,'+')>0) cycle ! an additional schedule
            l = Section(k)%SubjectIdx
            if (l==0) cycle ! section was deleted
            if (filter_dept>0) then
                if (filter_dept/=Section(k)%DeptIdx) cycle
            end if
            ! lecture-lab ?
            if (.not. is_lecture_lab_subject(l)) then ! lecture only or lab only
                Offering(l)%TotalSlots = Offering(l)%TotalSlots + Section(k)%Slots
                Offering(l)%NSections = Offering(l)%NSections + 1
            else ! a lecture-lab subject
                if (is_lecture_class(k, Section)) then ! this is the lecture section
                else ! this is the lab section
                    Offering(l)%TotalSlots = Offering(l)%TotalSlots + Section(k)%Slots
                    Offering(l)%NSections = Offering(l)%NSections + 1
                end if
            end if
        end do
        return
    end subroutine offerings_summarize


    subroutine count_sections_by_dept(Term, NumSections, Section)
        integer, intent (in) :: Term, NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer :: sect, dept
        ScheduleCount(Term,:) = 0
#if defined UPLB
        ! Subject administered by departments
        do sect=1,NumSections
            if (Section(sect)%SubjectIdx==0) cycle
            dept = Section(sect)%DeptIdx
            ScheduleCount(Term,dept) = ScheduleCount(Term,dept) + 1
        end do
#else
        ! Subjects administered by program
        do sect=1,NumSections
            if (Section(sect)%SubjectIdx==0) cycle
            dept = Section(sect)%DeptIdx
            ScheduleCount(Term,dept) = max(atoi(Section(sect)%Code(2:)), ScheduleCount(Term,dept))
        end do
#endif
        return
    end subroutine count_sections_by_dept


    subroutine delete_sections_of_dept(NumSections, Section, DeptIdx)
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        integer, intent (in) :: DeptIdx
        integer :: sect

        write(*,*) 'Removing classes in '//Department(DeptIdx)%Code
        do sect=1,NumSections
            if (Section(sect)%SubjectIdx==0) cycle
            if (DeptIdx==Section(sect)%DeptIdx) then
                call initialize_section(Section(sect))
            end if
        end do
        return
    end subroutine delete_sections_of_dept


    subroutine xml_write_sections(path, NumSections, Section, iDept, dirOPT)

        character(len=*), intent(in) :: path
        character(len=*), intent(in), optional :: dirOPT
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumSections
        integer, intent (in) :: iDept

        integer :: sdx, mdx, subj

        ! training only?
        if (noWrites) return

        ! all sections, or only the sections for given department?
        if (iDept>0) then
            if (present(dirOPT)) then
                fileName = trim(dirOPT)//trim(path)//'CLASSES-'//trim(Department(iDept)%Code)//'.XML'
            else
                fileName = trim(dirXML)//trim(path)//'CLASSES-'//trim(Department(iDept)%Code)//'.XML'
            endif
        else
            if (present(dirOPT)) then
                fileName = trim(dirOPT)//trim(path)//'CLASSES.XML'
            else
                fileName = trim(dirXML)//trim(path)//'CLASSES.XML'
            endif
        end if
        call xml_open_file(unitXML, XML_ROOT_SECTIONS, fileName, sdx)
        write(unitXML,AFORMAT) &
        '    <comment>', &
        '        Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                    FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
        '        Subject - subject code', &
        '        Class - class code, to differentiate classes for the same subject', &
        '        Owner - responsible department', &
        '        Slots - class capacity', &
        '        BlockID - block code, if class is assigned to a blocked section', &
        '        Meeting - a class meeting', &
        '        Time - begin time - end time of meeting', &
        '        Days - days of meetings', &
        '        Room - code for meeting room', &
        '        Teacher - code for teacher of meeting', &
        '    </comment>'

        do sdx=1,NumSections
            subj = Section(sdx)%SubjectIdx
            if (subj==0) cycle
            !write(*,*) Section(sdx)%ClassId, Section(sdx)%SubjectIdx

            ! subject belongs to given department?
            if (iDept>0 .and. Section(sdx)%DeptIdx/=iDept) cycle

            if (is_regular_schedule(sdx, Section)) then

                ! class is "regular": single entry for all meetings
                call xml_write_character(unitXML, indent0, 'Section')
                call xml_write_character(unitXML, indent1, 'Subject', Subject(subj)%Name)
                call xml_write_character(unitXML, indent1, 'Class', Section(sdx)%Code)
                call xml_write_character(unitXML, indent1, 'Owner', Department(Section(sdx)%DeptIdx)%Code)
                call xml_write_integer(unitXML,   indent1, 'Seats', Section(sdx)%Slots)
                if (len_trim(Section(sdx)%BlockID)>0) &
                    call xml_write_character(unitXML, indent1, 'BlockID', Section(sdx)%BlockID)
                call xml_write_character(unitXML, indent1, 'Meeting')
                call xml_write_character(unitXML, indent2, 'Time', text_time_period(Section(sdx)%bTimeIdx(1), &
                    Section(sdx)%eTimeIdx(1)))
                call xml_write_character(unitXML, indent2, 'Day', text_days_of_section(sdx, NumSections, Section))
                call xml_write_character(unitXML, indent2, 'Room', Room(Section(sdx)%RoomIdx(1))%Code)
                call xml_write_character(unitXML, indent2, 'Teacher', Teacher(Section(sdx)%TeacherIdx(1))%TeacherId)
                call xml_write_character(unitXML, indent1, '/Meeting')
                call xml_write_character(unitXML, indent0, '/Section')

            else ! class is "irregular": one entry for each meeting
                call xml_write_character(unitXML, indent0, 'Section')
                call xml_write_character(unitXML, indent1, 'Subject', Subject(subj)%Name)
                call xml_write_character(unitXML, indent1, 'Class', Section(sdx)%Code)
                call xml_write_character(unitXML, indent1, 'Owner', Department(Section(sdx)%DeptIdx)%Code)
                call xml_write_integer(unitXML,   indent1, 'Seats', Section(sdx)%Slots)
                if (len_trim(Section(sdx)%BlockID)>0) &
                    call xml_write_character(unitXML, indent1, 'BlockID', Section(sdx)%BlockID)
                do mdx=1,Section(sdx)%NMeets
                    call xml_write_character(unitXML, indent1, 'Meeting')
                    call xml_write_character(unitXML, indent2, 'Time', text_time_period(Section(sdx)%bTimeIdx(mdx), &
                        Section(sdx)%eTimeIdx(mdx)))
                    call xml_write_character(unitXML, indent2, 'Day', txtDay(Section(sdx)%DayIdx(mdx)))
                    call xml_write_character(unitXML, indent2, 'Room', Room(Section(sdx)%RoomIdx(mdx))%Code)
                    call xml_write_character(unitXML, indent2, 'Teacher', Teacher(Section(sdx)%TeacherIdx(mdx))%TeacherId)
                    call xml_write_character(unitXML, indent1, '/Meeting')
                end do
                call xml_write_character(unitXML, indent0, '/Section')
            end if

        end do

        call xml_close_file(unitXML, XML_ROOT_SECTIONS)

        return
    end subroutine xml_write_sections


    subroutine read_classes(path, NumSections, Section, Offering, errNo)

        character(len=*), intent(in) :: path
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        integer, intent (in out) :: NumSections
        type (TYPE_OFFERED_SUBJECTS), intent (in out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        integer, intent (out) :: errNo

        integer :: ddx, ierr, mainEntries, numUpdates, partialEntries, numEntries
        logical :: noXML

        errNo = 0 ! 0 is OK; there might be no classes entered yet

        fileName = trim(dirXML)//trim(path)//'CLASSES.XML'
        call xml_read_classes(fileName, NumSections, Section, ierr)
        noXML = NumSections==0
        mainEntries = NumSections
        numEntries = NumSections
!        ! check for classes edited by departments
!        do ddx=2,NumDepartments-1
!            fileName = trim(dirXML)//UPDATES//trim(path)//'CLASSES-'//trim(Department(ddx)%Code)//'.XML'
!            call xml_read_classes(fileName, NumSections, Section, ierr)
!            partialEntries = NumSections - numEntries
!            numEntries = NumSections
!            if (partialEntries>0) then ! remove classes of dept from monolithic file
!                call delete_sections_of_dept(mainEntries, Section, ddx)
!            end if
!            if (ierr==0) call move_to_backup(filename) ! delete departmental block
!        end do
        numUpdates = NumSections - mainEntries
        if (NumSections==0) then ! try the custom format
            fileName = trim(dirRAW)//trim(path)//'CLASSES'
            call custom_read_classes(fileName, NumSections, Section, ierr)
            numUpdates = 0
        end if

        ! sort & summarize
        call offerings_sort(NumSections, Section)
        call offerings_summarize(NumSections, Section, Offering, 0)

        ! write the XML classes file?
        if ( (noXML .and. NumSections>0) .or. numUpdates>0 ) &
            call xml_write_sections(path, NumSections, Section, 0)

        return
    end subroutine read_classes


    subroutine xml_read_classes(fName, NumSections, Section, errNo)

        character(len=*), intent(in) :: fName
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        integer, intent (in out) :: NumSections
        integer, intent (out) :: errNo

        integer :: i, j, k
        character(len=MAX_LEN_XML_LINE) :: value
        character(len=MAX_LEN_XML_TAG) :: tag
        type(TYPE_SECTION) :: wrkSection
        integer :: btime, dayidx(6), etime, ndays, iidx, pDASH
        integer :: subj, rmidx, tidx
        character (len = 1) :: ch
        character (len=5) :: strBTime, strETime
        character (len=MAX_LEN_DEPARTMENT_CODE) :: tDept
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_CLASS_ID) :: token
        character (len=MAX_LEN_ROOM_CODE) :: tRoom
        character (len=MAX_LEN_TEACHER_CODE) :: tTeacher
        logical :: flag

        ! open file, return on any error
        call xml_open_file(unitXML, XML_ROOT_SECTIONS, fName, errNo, forReading)
        if (errNo/=0) return

        ! examine the file line by line
        do
            read(unitXML, AFORMAT, iostat=eof) line
            if (eof<0) exit

            ! get tag and value if any; exit on any error
            call xml_parse_line(line, tag, value, eof)
            if (eof/=0) exit

            select case (trim(tag))

                case ('Section') ! initialize temporary section data
                    call initialize_section(wrkSection)

                case ('Subject') ! subject code
                    tSubject = adjustl(value)
                    subj = index_to_subject(tSubject)
                    if (subj<=0) then
                        errNo = 141
                        call file_log_message (trim(value)//' : subject not in catalog')
                        return
                    end if
                    wrkSection%SubjectIdx = subj

                case ('Class') ! section code
                    wrkSection%Code = adjustl(value)

                case ('Owner') ! available seats in class
                    tDept = adjustl(value)
                    iidx = index_to_dept (tDept)
                    if (iidx==0) iidx = NumDepartments ! Registrar
                    wrkSection%DeptIdx = iidx

                case ('Seats') ! available seats in class
                    wrkSection%Slots = atoi(value)

                case ('BlockID') ! section is assigned to block
                    wrkSection%BlockID = adjustl(value)

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
                            call file_log_message (trim(value)//' - assuming '//strETime//' is evening...')
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
                                iidx = 6
                            end if
                            if (iidx>0) then
                                ndays = ndays+1
                                if (ndays>6) then
                                    write(*,*) 'Too many days: '//trim(value)
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
                            call file_log_message (trim(value)//' - '//trim(tRoom)//' room is not valid; using TBA')
                        end if
                    end if

                case ('Teacher') ! teacher
                    tTeacher = adjustl(value)
                    if (tTeacher=='TBA') then
                        tidx = 0
                    else
                        !do i=1,len_trim(tTeacher)
                        !  ch = tTeacher(i:i)
                        !  if ( ('a'<=ch .and. ch<='z') .or. ('A'<=ch .and. ch<='Z') .or. ('0'<=ch .and. ch<='9')) cycle
                        !  tTeacher(i:i) = DASH
                        !end do
                        tidx = index_to_teacher (tTeacher)
                        if (tidx==0) then
                            call file_log_message (trim(value)//' - '//trim(tTeacher)//' teacher is not valid; using TBA')
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
                    token = trim(Subject(wrkSection%SubjectIdx)%Name)//SPACE//wrkSection%Code
                    wrkSection%ClassId = token
                    flag = .true.
                    do i=1,NumSections
                        if (Section(i)%ClassId .ne. wrkSection%ClassId) cycle
                        flag = .false.
                        exit
                    end do
                    if (flag) then
                        NumSections = NumSections + 1
                        call check_array_bound (NumSections, MAX_ALL_SECTIONS, 'MAX_ALL_SECTIONS')
                        Section(NumSections) = wrkSection
                    else
                        call file_log_message ('In '//trim(fName)//' : '//trim(wrkSection%ClassId)// &
                            ' owned by '//trim(tDept)//' - duplicate class; ignored.')
                    end if

                case default
                    ! do nothing

            end select
        end do

        call xml_close_file(unitXML)
        call file_log_message (itoa(NumSections)//' sections after reading '//fName)

        return
    end subroutine xml_read_classes


    function is_regular_schedule(sect, Section)
        ! returns true if section meetings have same (time, room, teacher), different days
        integer, intent (in) :: sect
        type (TYPE_SECTION), intent(in) :: Section(0:)
        logical :: is_regular_schedule
        logical :: sameTeacher, sameRoom, sameTime
        integer :: mdx

        sameTime = .true.
        sameRoom = .true.
        sameTeacher = .true.
        do mdx=2,Section(sect)%Nmeets
          if (Section(sect)%bTimeIdx(1)/=Section(sect)%bTimeIdx(mdx) .or. &
              Section(sect)%eTimeIdx(1)/=Section(sect)%eTimeIdx(mdx)) sameTime = .false.
          if (Section(sect)%RoomIdx(1)/=Section(sect)%RoomIdx(mdx)) sameRoom = .false.
          if (Section(sect)%TeacherIdx(1)/=Section(sect)%TeacherIdx(mdx)) sameTeacher = .false.
        end do
        is_regular_schedule = sameTime .and. sameRoom .and. sameTeacher

        return
    end function is_regular_schedule


end module SECTIONS
