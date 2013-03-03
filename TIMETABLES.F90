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


module TIMETABLES

    use SECTIONS

    implicit none

    ! constants for building/assessing initial schedules
    ! array extents
    integer, parameter :: &
    MAX_ALL_SECTIONS_OF_A_SUBJECT=150, &   ! max sections in a subject
    MAX_ALL_STUDENTS_OF_A_SUBJECT=5000, & ! max students taking a subject
    ! undesirability weights
    PENALTY_LONG_DAY=500, &
    PENALTY_HUNGRY_DAY=300, &  ! penalty for "no lunch" days
    PENALTY_EARLY_DAY=0, &    ! penalty for early days
    PENALTY_LATE_DAY=20, &     ! penalty for late days
    PENALTY_TRAVEL_IN_A_HURRY=400 ! penalty for travels between consecutive classses

    integer, dimension (0:MAX_ALL_SECTIONS_OF_A_SUBJECT) :: LongDays, HungerDays, EarlyDays, LateDays, Travels
    integer, dimension (0:MAX_ALL_SECTIONS_OF_A_SUBJECT) :: Undesirability, UndesirabilityRank

contains


    subroutine timetable_clear(TimeTable)
        integer, dimension (60,6), intent(out) :: TimeTable
        TimeTable = 0
        TimeTable(59,1:6) = 60 ! earliest time
        TimeTable(60,1:6) = 1  ! latest time
        return
    end subroutine timetable_clear


    function is_conflict_timetable_with_section(NumSections, Section, sect, TimeTable)
        logical :: is_conflict_timetable_with_section
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section
        integer, intent (in) :: NumSections
        integer, intent (in) :: sect
        integer, dimension(60,6), intent (in) :: TimeTable
        integer :: idx, jdx, mdx
        !
        is_conflict_timetable_with_section = .false.
        if (sect>0) then ! a valid sect pointer
            loop_meets : &
            do mdx=1,Section(sect)%NMeets
                jdx = Section(sect)%DayIdx(mdx)
                if (jdx==0) cycle
                do idx = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
                    if (TimeTable(idx, jdx)/=0) then
                        is_conflict_timetable_with_section = .true.
                        exit loop_meets
                    end if
                end do
            end do loop_meets
        end if
        return
    end function is_conflict_timetable_with_section



    subroutine timetable_add_section(NumSections, Section, sect, TimeTable, loc)
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section
        integer, intent (in) :: NumSections
        integer, intent (in) :: sect
        integer, dimension (60,6), intent (in out) :: TimeTable
        integer, intent (in out) :: loc
        integer :: idx, jdx, mdx
        integer, dimension (60,6) :: tTable
        !
        if (sect>0) then ! a valid sect pointer
            tTable = TimeTable
            do mdx=1,Section(sect)%NMeets
                jdx = Section(sect)%DayIdx(mdx)
                if (jdx==0) cycle
                do idx = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
                    if (tTable(idx, jdx)/=0) then
                        !           write(*,*)  'Conflict detected in timetable_add_section(); called from', loc
                        loc = tTable(idx,jdx)
                        return
                    else
                        tTable(idx,jdx) =  sect
                    end if
                end do
            end do
            ! nothing unusual
            TimeTable = tTable
            do mdx=1,Section(sect)%NMeets
                jdx = Section(sect)%DayIdx(mdx)
                if (jdx==0) cycle
                !do idx = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
                !  TimeTable(idx, jdx) = sect
                !end do
                if (TimeTable(59,jdx)>Section(sect)%bTimeIdx(mdx)) then
                    TimeTable(59,jdx) = Section(sect)%bTimeIdx(mdx)
                end if
                if (TimeTable(60,jdx)<Section(sect)%eTimeIdx(mdx)) then
                    TimeTable(60,jdx) = Section(sect)%eTimeIdx(mdx)
                end if
            end do
        else if (sect<0) then
            write(unitLOG,*) 'Invalid section', sect, ' in timetable_add_section(); called from', loc
        end if
        return
    end subroutine timetable_add_section


    function is_conflict_timetable_with_section_meetings(NumSections, Section, sect, n_meetings, meetings, TimeTable)
        logical :: is_conflict_timetable_with_section_meetings
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section
        integer, intent (in) :: NumSections
        integer, intent (in) :: sect, n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer, dimension(60,6), intent (in) :: TimeTable
        integer :: idx, jdx, kdx, mdx
        !
        is_conflict_timetable_with_section_meetings = .false.
        if (sect>0) then ! a valid sect pointer
            loop_meets : &
            do kdx=1,n_meetings
                mdx=meetings(kdx)
                jdx = Section(sect)%DayIdx(mdx)
                if (jdx==0) cycle
                do idx = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
                    if (TimeTable(idx, jdx)/=0) then
                        is_conflict_timetable_with_section_meetings = .true.
                        exit loop_meets
                    end if
                end do
            end do loop_meets
        end if
        return
    end function is_conflict_timetable_with_section_meetings


    subroutine timetable_add_meetings_of_section(NumSections, Section, sect, n_meetings, meetings, TimeTable, loc)
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section
        integer, intent (in) :: NumSections
        integer, intent (in) :: sect, n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer, intent (in out) :: TimeTable(60,6), loc
        integer :: idx, jdx, kdx, mdx
        integer :: tTable(60,6)
        !
        tTable = TimeTable
        do kdx=1,n_meetings
            mdx = meetings(kdx)
            jdx = Section(sect)%DayIdx(mdx)
            if (jdx==0) cycle
            do idx = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
                if (tTable(idx, jdx)==0) then
                    tTable(idx,jdx) = sect
                else
                    loc = tTable(idx,jdx)
                    return ! do not add to TimeTable()
                end if
            end do
        end do
        ! nothing unusual
        TimeTable = tTable
        do kdx=1,n_meetings
            mdx = meetings(kdx)
            jdx = Section(sect)%DayIdx(mdx)
            if (jdx==0) cycle
            !do idx = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
            !  TimeTable(idx, jdx) = sect
            !end do
            if (TimeTable(59,jdx)>Section(sect)%bTimeIdx(mdx)) then
                TimeTable(59,jdx) = Section(sect)%bTimeIdx(mdx)
            end if
            if (TimeTable(60,jdx)<Section(sect)%eTimeIdx(mdx)) then
                TimeTable(60,jdx) = Section(sect)%eTimeIdx(mdx)
            end if
        end do
        return
    end subroutine timetable_add_meetings_of_section


    function is_conflict_timetable_with_struct_section(Section, m_first, m_last, TimeTable)
        logical :: is_conflict_timetable_with_struct_section
        type (TYPE_SECTION), intent (in) :: Section
        integer, intent (in) :: m_first, m_last
        integer, dimension(60,6), intent (in) :: TimeTable
        integer :: idx, jdx, mdx
        !
        is_conflict_timetable_with_struct_section = .false.
        loop_meets : &
        do mdx=m_first,m_last
            jdx = Section%DayIdx(mdx)
            if (jdx==0) cycle
            do idx = Section%bTimeIdx(mdx), Section%eTimeIdx(mdx)-1
                if (TimeTable(idx, jdx)/=0) then
                    is_conflict_timetable_with_struct_section = .true.
                    exit loop_meets
                end if
            end do
        end do loop_meets
        return
    end function is_conflict_timetable_with_struct_section


    subroutine timetable_add_struct_section(Section, TimeTable, loc)
        type (TYPE_SECTION), intent (in) :: Section
        integer, dimension (60,6), intent (in out) :: TimeTable
        integer, intent (in out) :: loc
        integer :: idx, jdx, mdx
        integer, dimension (60,6) :: tTable
        !
        tTable = TimeTable
        do mdx=1,Section%NMeets
            jdx = Section%DayIdx(mdx)
            if (jdx==0) cycle
            do idx = Section%bTimeIdx(mdx), Section%eTimeIdx(mdx)-1
                if (tTable(idx, jdx)==0) then
                    tTable(idx, jdx) = mdx
                else! conflict detected
                    loc = tTable(idx,jdx)
                    return
                end if
            end do
        end do
        TimeTable = tTable
        ! nothing unusual
        do mdx=1,Section%NMeets
            jdx = Section%DayIdx(mdx)
            if (jdx==0) cycle
            if (TimeTable(59,jdx)>Section%bTimeIdx(mdx)) then
                TimeTable(59,jdx) = Section%bTimeIdx(mdx)
            end if
            if (TimeTable(60,jdx)<Section%eTimeIdx(mdx)) then
                TimeTable(60,jdx) = Section%eTimeIdx(mdx)
            end if
        end do
        return
    end subroutine timetable_add_struct_section


    subroutine timetable_remove_section(NumSections, Section, sect, TimeTable, loc)
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section
        integer, intent (in) :: NumSections
        integer, intent (in) :: sect
        integer, dimension (60,6), intent (in out) :: TimeTable
        integer, intent (in out) :: loc
        integer :: idx, jdx, mdx
        !
        if (sect>0) then ! a valid sect pointer
            do mdx=1,Section(sect)%NMeets
                jdx = Section(sect)%DayIdx(mdx)
                if (jdx==0) cycle
                do idx = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
                    if (TimeTable(idx, jdx)/=sect) then
                        write(unitLOG,*)  'ERROR detected in timetable_remove_section(); called from', loc
                        loc = TimeTable(idx,jdx)
                        return
                    end if
                end do
            end do
            ! nothing unusual
            do mdx=1,Section(sect)%NMeets
                jdx = Section(sect)%DayIdx(mdx)
                if (jdx==0) cycle
                do idx = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
                    TimeTable(idx, jdx) = 0
                end do
                ! reset earliest time
                idx = 1
                do while (TimeTable(idx,jdx)==0 .and. idx<60)
                    idx = idx+1
                end do
                TimeTable(59,jdx) = idx
                ! reset latest time
                idx = 57
                do while (TimeTable(idx,jdx)==0 .and. idx>1)
                    idx = idx-1
                end do
                TimeTable(60,jdx) = idx
            end do
        end if
        return
    end subroutine timetable_remove_section


    function is_consistent_section_hours_with_subject_defn(tSection)
        type (TYPE_SECTION), intent (in) :: tSection
        logical :: is_consistent_section_hours_with_subject_defn
        integer :: idx
        real :: n15, SectionHours

        is_consistent_section_hours_with_subject_defn = .true.
        n15 = 0.0 ! no. of 15-minute intervals
        do idx=1,tSection%NMeets
            n15 = n15 + tSection%eTimeIdx(idx) - tSection%bTimeIdx(idx)
        end do
        if (n15==0.0) return ! assume TBA is OK
        ! disable check for summer schedules
        if ( (fnOFFSET==0 .and. currentTerm==0) .or. &
             (fnOFFSET>0 .and. currentTerm==2) ) return

        ! figure out how many hours based on subject type and section code
        idx = tSection%SubjectIdx
        if (is_lecture_lab_subject(idx)) then
            if (index(tSection%ClassId,DASH)==0) then
                SectionHours = Subject(idx)%LectHours
            else
                SectionHours = Subject(idx)%LabHours
            end if
        else if (Subject(idx)%LectHours > 0) then
            SectionHours = Subject(idx)%LectHours
        else ! if (Subject(idx)%LabHours > 0) then
            SectionHours = Subject(idx)%LabHours
        end if
        is_consistent_section_hours_with_subject_defn = 4.0*SectionHours==n15
        return
    end function is_consistent_section_hours_with_subject_defn


    function is_conflict_free_section_hours(tSection, NumSections, Section)
        logical :: is_conflict_free_section_hours, tDetermination
        type (TYPE_SECTION), intent (in) :: tSection
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section
        integer, intent (in) :: NumSections
        integer, dimension(60,6) :: TimeTable
        character(len=MAX_LEN_CLASS_ID) :: tClassId
        integer :: i, j

        tDetermination = .true.
        ! meeting conflicts?
        call timetable_clear(TimeTable)
        i = -10
        call timetable_add_struct_section(tSection, TimeTable, i)
        if ( i/=-10) then
            tDetermination = .false.
        else ! lab section conflicts with lecture section?
            if (is_lecture_lab_subject(tSection%SubjectIdx)) then ! subject is lecture-lab
                ! a lab section ?
                i = index(tSection%ClassId,DASH)
                if (i>0) then
                    ! find lecture section
                    tClassId = tSection%ClassId(:i-1)
                    j = index_to_section(tClassId, NumSections, Section)
                    if (j>0) then
                        if (is_conflict_timetable_with_section(NumSections, Section, j, TimeTable)) tDetermination = .false.
                    end if
                end if
            end if
        end if
        is_conflict_free_section_hours = tDetermination
        return
    end function is_conflict_free_section_hours


    subroutine sections_compound(NumSections, Section, nconflicts, ignoreMismatch)
        type (TYPE_SECTION), intent(in out), dimension (0:) :: Section
        integer, intent (in out) :: NumSections
        integer, intent (out) :: nconflicts
        logical, intent (in), optional :: ignoreMismatch
#ifdef DEBUG
        character (len=255) :: line
#endif
        !character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_CLASS_ID) :: tSection
        integer :: crse, sect, sdx, i, j, idx, n15, tHours
        integer :: TimeTable(60,6)
        logical :: ignore

        if (present(ignoreMismatch)) then
            ignore = ignoreMismatch
        else
            ignore = .false.
        end if

        nconflicts = 0
        !
        ! add lecture times to laboratory section
        do sect=1,NumSections
            crse = Section(sect)%SubjectIdx
            if (crse<0) cycle ! done previously
            !if (Subject(crse)%LectHours * Subject(crse)%LabHours==0) cycle
            if (.not. is_lecture_lab_subject(crse)) cycle
            ! subject is lecture-lab
            if (is_lecture_class(sect, Section)) cycle ! a lecture section

            !write(*,*) 'Laboratory class '//Section(sect)%ClassId
            call timetable_clear(TimeTable)
            idx = -10
            call timetable_add_section(NumSections, Section, sect, TimeTable, idx)
            if ( idx/=-10) then
                nconflicts = nconflicts + 1
                call file_log_message(Section(sect)%ClassId//' conflicts with '//Section(idx)%ClassId)
            end if
            !
            !tSubject = Subject(crse)%Name
            !tSection = trim(tSubject)//SPACE//Section(sect)%Code(:j-1)
            tSection = Section(sect)%ClassId
            j = len_trim(tSection)
            do while (tSection(j:j)/=DASH)
                j = j-1
            end do
            tSection(j:) = SPACE
            sdx = index_to_section(tSection, NumSections, Section)
            !write(*,*) 'lecture class '//tSection, sdx
            if (sdx>0) then ! lecture found
                idx = -10
                call timetable_add_section(NumSections, Section, sdx, TimeTable, idx)
                if ( idx/=-10) then
                    nconflicts = nconflicts + 1
                    call file_log_message(Section(sdx)%ClassId//' conflicts with '//Section(idx)%ClassId)
                end if
                j = Section(sect)%NMeets
                Section(sect)%NMeets = j + Section(sdx)%NMeets
                do i=1,Section(sdx)%NMeets
                    Section(sect)%DayIdx(j+i) = Section(sdx)%DayIdx(i)
                    Section(sect)%bTimeIdx(j+i) = Section(sdx)%bTimeIdx(i)
                    Section(sect)%eTimeIdx(j+i) = Section(sdx)%eTimeIdx(i)
                    Section(sect)%RoomIdx(j+i) = Section(sdx)%RoomIdx(i)
                end do
            else
                nconflicts = nconflicts + 1
                call file_log_message('Lecture class '//tSection//'not found!')
            end if
        end do
        !
        ! "erase" lecture times of lecture-laboratory subjects
        do sect=1,NumSections
            crse = Section(sect)%SubjectIdx
            if (crse<0) cycle ! done previously
            !if (Subject(crse)%LectHours * Subject(crse)%LabHours==0) cycle
            if (.not. is_lecture_lab_subject(crse)) cycle
            ! subject is lecture-lab
            j = index(Section(sect)%Code,DASH)
            if (j==0) then
                Section(sect)%SubjectIdx = -(crse-NumDummySubjects)
            ! a lecture section
            end if
        end do
        !
        sect = 0
        call initialize_section(Section(sect))
        do sdx=1,NumSections
            crse = Section(sdx)%SubjectIdx
            if (crse<0) then
#ifdef DEBUG
                crse = -(crse-NumDummySubjects)
                line = '#'
#endif
            else
#ifdef DEBUG
                line = SPACE
#endif
                sect = sect+1
                Section(sect) = Section(sdx)
            end if
#ifdef DEBUG
            line = trim(line)//trim(Section(sdx)%ClassId)//COMMA//itoa(Section(sdx)%Slots)
            do i=1,Section(sdx)%NMeets
                line = trim(line)//COMMA// &
                    trim(text_time_period(Section(sdx)%bTimeIdx(i), Section(sdx)%eTimeIdx(i)))//COMMA// &
                    trim(txtDay(Section(sdx)%DayIdx(i)))//COMMA// &
                    Room(Section(sdx)%RoomIdx(i))%Code
            end do
            write(*,*) trim(line)
#endif
        end do
        NumSections = sect
        call initialize_section(Section(sect+1))
        ! check consistency of hours
        do sdx=1,NumSections
            crse = Section(sdx)%SubjectIdx
            if (crse<=0) cycle
            tHours = Subject(crse)%LectHours+Subject(crse)%LabHours
            n15 = 0
            do i=1,Section(sdx)%NMeets
                n15 = n15 + Section(sdx)%eTimeIdx(i) - Section(sdx)%bTimeIdx(i)
            end do
            if (n15 .gt. 0 .and. 4*tHours .ne. n15) then
                write(unitLOG,*) Section(sdx)%ClassId//': scheduled hours ('// &
                    trim(itoa(n15/4))//') is inconsistent with subject parameter hours ('//trim(itoa(tHours))//')'
                if (.not. ignore) nconflicts = nconflicts + 1
            end if
        end do
        return
    end subroutine sections_compound


    subroutine meetings_of_section_by_teacher(NumSections, Section, section_index, teacher_index, n_meetings, meetings)
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section
        integer, intent (in) :: NumSections
        integer, intent(in) :: section_index, teacher_index
        integer, intent(out) :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer :: i
        n_meetings = 0
        meetings = 0
        do i=1,Section(section_index)%NMeets
            if (Section(section_index)%TeacherIdx(i)==teacher_index) then
                n_meetings = 1+n_meetings
                meetings(n_meetings) = i
            end if
        end do
        return
    end subroutine meetings_of_section_by_teacher


    subroutine meetings_of_section_in_room(NumSections, Section, section_index, room_index, n_meetings, meetings)
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section
        integer, intent (in) :: NumSections
        integer, intent(in) :: section_index, room_index
        integer, intent(out) :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer :: i
        n_meetings = 0
        meetings = 0
        do i=1,Section(section_index)%NMeets
            if (Section(section_index)%RoomIdx(i)==room_index) then
                n_meetings = 1+n_meetings
                meetings(n_meetings) = i
            end if
        end do
        return
    end subroutine meetings_of_section_in_room


    subroutine timetable_meetings_of_teacher(NumSections, Section, teacher_index, to_skip, len_list, list, TimeTable, conflicted)
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section
        integer, intent (in) :: NumSections
        integer, intent(in) :: teacher_index, to_skip
        integer, intent (out) :: len_list, list(:)
        integer, dimension(60,6), intent (out) :: TimeTable
        logical, intent(out) :: conflicted
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer :: idx, conflict_loc, sdx
        len_list = 0 ! initialize list
        call timetable_clear(TimeTable) ! initialize weekly schedule
        conflicted = .false.
        do sdx=1,NumSections ! loop over all sections
            if (sdx==to_skip) cycle ! skip specified section (if, for example, the section is being edited)
            if (Section(sdx)%SubjectIdx==0) cycle ! section ws deleted
            call meetings_of_section_by_teacher(NumSections, Section, sdx, teacher_index, n_meetings, meetings) ! collect meetings assigned to teacher
            if (n_meetings==0) cycle ! teacher not assigned to this section
            do idx=1,n_meetings ! loop over meetings for this section
                list(len_list+1) = sdx ! add index to Section() to list of meetings for teacher
                list(len_list+2) = meetings(idx) ! index to Section(sdx)%DayIdx(), etc
                conflict_loc = -10 ! assume no conflicting section
                call timetable_add_meetings_of_section(NumSections, Section, sdx, 1, meetings(idx), TimeTable, conflict_loc) ! add meeting to weekly schedule
                if (conflict_loc/=-10) then
                    conflicted = .true.
                else
                    conflict_loc = 0
                end if
                list(len_list+3) = conflict_loc ! index to conflicting section, if any
                len_list = len_list+3
            end do
        end do
        ! end markers
        list(len_list+1) = 0
        list(len_list+2) = 0
        list(len_list+3) = 0
        return
    end subroutine timetable_meetings_of_teacher


    subroutine timetable_meetings_in_room(NumSections, Section, room_index, to_skip, len_list, list, TimeTable, conflicted)
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section
        integer, intent (in) :: NumSections
        integer, intent(in) :: room_index, to_skip
        integer, intent (out) :: len_list, list(:)
        integer, dimension(60,6), intent (out) :: TimeTable
        logical, intent(out) :: conflicted
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer :: idx, conflict_loc, sdx
        len_list = 0 ! initialize list
        call timetable_clear(TimeTable) ! initialize weekly schedule
        conflicted = .false.
        do sdx=1,NumSections ! loop over all sections
            if (sdx==to_skip) cycle ! skip specified section (if, for example, the section is being edited)
            if (Section(sdx)%SubjectIdx==0) cycle ! section ws deleted
            call meetings_of_section_in_room(NumSections, Section, sdx, room_index, n_meetings, meetings) ! collect meetings assigned to room
            if (n_meetings==0) cycle ! room not assigned to this section
            do idx=1,n_meetings ! loop over meetings for this section
                list(len_list+1) = sdx ! add index to Section() to list of meetings in room
                list(len_list+2) = meetings(idx) ! index to Section(sdx)%DayIdx(), etc
                conflict_loc = -10 ! assume no conflicting section
                call timetable_add_meetings_of_section(NumSections, Section, sdx, 1, meetings(idx), TimeTable, conflict_loc) ! add meeting to weekly schedule
                if (conflict_loc/=-10) then
                    conflicted = .true.
                else
                    conflict_loc = 0
                end if
                list(len_list+3) = conflict_loc ! index to conflicting section, if any
                len_list = len_list+3
            end do
        end do
        ! end markers
        list(len_list+1) = 0
        list(len_list+2) = 0
        list(len_list+3) = 0
        return
    end subroutine timetable_meetings_in_room


    subroutine timetable_undesirability(ns, NumSections, Section, TimeTable)
        integer, intent(in) :: ns
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section
        integer, intent (in) :: NumSections
        integer, dimension(60,6), intent(in out)  :: TimeTable

        integer :: day_idx, mdx, sect, tSect, tmdx
        integer :: i, j, k
        logical :: TimeBlockFree

        ! initialize earliest times, latest times, undesirability indices
        LongDays = 0
        HungerDays = 0
        EarlyDays = 0
        LateDays = 0
        Travels = 0
        Undesirability = 0

        do sect=NumSections+1,NumSections+ns
            i = sect-NumSections

            !write(*,*) i, sect, Section(sect)%ClassId
            do mdx=1,Section(sect)%NMeets
                day_idx = Section(sect)%DayIdx(mdx)
                if (day_idx==0) cycle
                ! need to travel?
                ! previous class is
                tsect = 0
                if (Section(sect)%bTimeIdx(mdx) > 1) then
                    tsect = TimeTable(Section(sect)%bTimeIdx(mdx)-1, day_idx)
                end if
                if (tsect > 0) then
                    do tmdx=1,Section(tsect)%NMeets ! find the room
                        if (day_idx == Section(tsect)%DayIdx(tmdx) .and. & ! same day
                        Section(sect)%bTimeIdx(mdx) == Section(tsect)%eTimeIdx(tmdx) ) then ! previous class
                            if (Room(Section(tsect)%RoomIdx(tmdx))%Cluster /=  &
                            Room(Section(sect)%RoomIdx(mdx))%Cluster ) &
                            Travels(i) = Travels(i) + 1
                            !write(*,*) 'Travel from '//Room(Section(tsect)%RoomIdx(tmdx))%Code, &
                            !  Room(Section(sect)%RoomIdx(mdx))%Code
                        end if
                    end do
                end if
                ! next class is
                tsect = 0
                if (Section(sect)%eTimeIdx(mdx) < 58) then
                    tsect = TimeTable(Section(sect)%eTimeIdx(mdx)+1, day_idx)
                end if
                if (tsect > 0) then
                    do tmdx=1,Section(tsect)%NMeets ! find the room
                        if (day_idx == Section(tsect)%DayIdx(tmdx) .and. & ! same day
                        Section(sect)%eTimeIdx(mdx) == Section(tsect)%bTimeIdx(tmdx) ) then ! next class
                            if (Room(Section(tsect)%RoomIdx(tmdx))%Cluster /=  &
                            Room(Section(sect)%RoomIdx(mdx))%Cluster ) &
                            Travels(i) = Travels(i) + 1
                            !write(*,*) 'Travel from '//Room(Section(tsect)%RoomIdx(tmdx))%Code, &
                            !  Room(Section(sect)%RoomIdx(mdx))%Code
                        end if
                    end do
                end if
                ! how many early days
                if (Section(sect)%bTimeIdx(mdx) < TIME_INDEX_EARLY_DAY) then
                    EarlyDays(i) = EarlyDays(i) + 1
                    !write(*,*) 'Early day '//Section(sect)%ClassId, txtDay(Section(sect)%DayIdx(mdx)), &
                    !  txtTime(Section(sect)%bTimeIdx(mdx))
                    ! long day?
                    if (TimeTable(60,day_idx) >= TIME_INDEX_LATE_DAY) LongDays(i) = LongDays(i) + 1
                end if
                ! how many late days
                if (Section(sect)%eTimeIdx(mdx) > TIME_INDEX_LATE_DAY) then
                    LateDays(i) = LateDays(i) + 1
                    !write(*,*) 'Late day '//Section(sect)%ClassId, txtDay(Section(sect)%DayIdx(mdx)), &
                    !  txtTime(Section(sect)%eTimeIdx(mdx))
                    ! long day?
                    if (TimeTable(59,day_idx) <= TIME_INDEX_EARLY_DAY) LongDays(i) = LongDays(i) + 1
                end if
                ! how many days will it not allow lunch time?
                if (.not. (Section(sect)%eTimeIdx(mdx) <= TIME_INDEX_BEGIN_LUNCH .or. &
                Section(sect)%bTimeIdx(mdx) >= TIME_INDEX_END_LUNCH) ) then
                    ! temporarily add to timetable
                    do k = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
                        TimeTable(k, day_idx) = sect
                    end do
                    ! check if schedule does not allow lunch time
                    TimeBlockFree = .false.
                    do k = TIME_INDEX_BEGIN_LUNCH,TIME_INDEX_END_LUNCH-1
                        if (TimeTable(k, day_idx) == 0) then
                            TimeBlockFree = .true.
                        end if
                    end do
                    if (.not. TimeBlockFree) then
                        HungerDays(i) = HungerDays(i) + 1
                      !write(*,*) 'Hungry day '//Section(sect)%ClassId, txtDay(Section(sect)%DayIdx(mdx))
                    end if
                    ! remove from timetable
                    do k = Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)-1
                        TimeTable(k, day_idx) = 0
                    end do
                end if
            end do !  mdx=1,Section(sect)%NMeets

        end do !  sect=NumSections+1,NumSections+ns

        ! compute a single undesirablility index for each option

        do j=1,ns
            UndesirabilityRank(j) = j
            !
            if (LongDays(j) > LongDays(0)) LongDays(0) = LongDays(j)
            if (HungerDays(j) > HungerDays(0)) HungerDays(0) = HungerDays(j)
            if (EarlyDays(j) > EarlyDays(0)) EarlyDays(0) = EarlyDays(j)
            if (LateDays(j) > LateDays(0)) LateDays(0) = LateDays(j)
            if (Travels(j) > Travels(0)) Travels(0) = Travels(j)
        end do
        if (LongDays(0) > 0) then
            do j=1,ns
                Undesirability(j) = Undesirability(j) + (PENALTY_LONG_DAY*LongDays(j))/LongDays(0)
            end do
        end if
        if (HungerDays(0) > 0) then
            do j=1,ns
                Undesirability(j) = Undesirability(j) + (PENALTY_HUNGRY_DAY*HungerDays(j))/HungerDays(0)
            end do
        end if
        if (EarlyDays(0) > 0) then
            do j=1,ns
                Undesirability(j) = Undesirability(j) + (PENALTY_EARLY_DAY*EarlyDays(j))/EarlyDays(0)
            end do
        end if
        if (LateDays(0) > 0) then
            do j=1,ns
                Undesirability(j) = Undesirability(j) + (PENALTY_LATE_DAY*LateDays(j))/LateDays(0)
            end do
        end if
        if (Travels(0) > 0) then
            do j=1,ns
                Undesirability(j) = Undesirability(j) + (PENALTY_TRAVEL_IN_A_HURRY*Travels(j))/Travels(0)
            end do
        end if

        ! sort options according to increasing undesirability index
        do i=1,ns-1
            do j=i,ns
                if ( Undesirability(UndesirabilityRank(i)) > Undesirability(UndesirabilityRank(j)) ) then
                    k = UndesirabilityRank(j)
                    UndesirabilityRank(j) = UndesirabilityRank(i)
                    UndesirabilityRank(i) = k
                end if
            end do
        end do
        return
    end subroutine timetable_undesirability

end module TIMETABLES
