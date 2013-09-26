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


module DisplayROOMS

    use HTML

    implicit none

contains

    subroutine room_list_all (device)
        integer, intent (in) :: device
        integer :: rdx, n_count, nsect(3), sdx, tdx, term, termStore, ierr
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer, dimension(60,6) :: TimeTable
        character (len=127) :: mesg
        character (len=80) :: tDesc
        integer :: tYear, tTerm

        ! collect rooms
        tArray = 0
        n_count = 0

        ! which dept ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)

        call html_comment('room_list_all('//trim(tDepartment)//')')

        targetDepartment = index_to_dept(tDepartment)
        do rdx=1,NumRooms+NumAdditionalRooms
            if (targetDepartment==Room(rdx)%DeptIdx) then
                n_count = n_count+1
                tArray(n_count) = rdx
            end if
        end do
        mesg = 'Rooms in '//tDepartment

        call html_write_header(device, mesg)

        if (n_count == 0) then
            write(device,AFORMAT) '<table border="0">', &
                begintr//begintd//'(None?)'//endtd//tdalignright
            if (isRoleAdmin) then
                write(device,AFORMAT) trim(make_href(fnEditRoom, 'Add room', &
                    A1='TBA', pre='<small>('//nbsp, post=' )</small>'))
            end if
            write(device,AFORMAT) endtd//endtr//'</table>'
        else
            ! sort rooms
            do tdx=1,n_count-1
                do sdx=tdx+1,n_count
                    if (Room(tArray(sdx))%Code<Room(tArray(tdx))%Code) then
                        rdx =tArray(sdx)
                        tArray(sdx) = tArray(tdx)
                        tArray(tdx) = rdx
                    end if
                end do
            end do

            if (isRoleAdmin) then
                write(device,AFORMAT) trim(make_href(fnEditRoom, 'Add room', &
                    A1='TBA', pre='<small>('//nbsp, post=' )</small>'))//'<br>'
            end if

            write(device,AFORMAT) '<i>Note: Number under Term links to classes in the room.</i><br>', &
                '<table border="0" width="75%">'//&
                begintr//thalignleft//'Code'//endth// &
                thaligncenter//'Cluster'//endth, &
                thaligncenter//'Capacity'//endth
            do term=termBegin,termEnd
                call qualify_term (term, tYear, tTerm, tDesc)
                write(device,AFORMAT) &
                    thaligncenter//txtSemester(tTerm+6)//' Term<br>'// &
                    text_school_year(tYear)//endth
            end do
            write(device,AFORMAT) &
                thaligncenter//'Remark'//endth//endtr

            do tdx=1,n_count
                rdx = tArray(tdx)

                ! check conflicts for each term
                mesg = SPACE
                nsect = 0
                do tTerm=termBegin,termEnd
                    call qualify_term (tTerm, tYear, term, tDesc)
                    call timetable_clear(TimeTable)
                    ! collect classes in room
                    do sdx=1,NumSections(term)
                        call meetings_of_section_in_room(NumSections(term), Section(term,0:), sdx, rdx, &
                            n_meetings, meetings)
                        if (n_meetings==0) cycle ! room not assigned to this section
                        nsect(term) = nsect(term)+1
                        ierr = -10
                        call timetable_add_meetings_of_section(NumSections(term), Section(term,0:), sdx, &
                            n_meetings, meetings, TimeTable, ierr)
                        if (ierr /= -10) then
                            mesg = trim(mesg)//SPACE//txtSemester(term+6)
                        end if
                    end do
                end do
                if (len_trim(mesg)>0) mesg = red//'Conflict: '//trim(mesg)//black

                QUERY_put = Room(rdx)%Code
                write(device,AFORMAT) begintr//begintd//trim(Room(rdx)%Code)
                if (isRoleAdmin .or. &
                    !(isRoleChair .and. Room(rdx)%DeptIdx==DeptIdxUser) .or. &
                    (isRoleDean .and. Department(Room(rdx)%DeptIdx)%CollegeIdx==CollegeIdxUser) &
                        ) then
                    write(device,AFORMAT) trim(make_href(fnEditRoom, 'Edit', &
                        A1=QUERY_put, pre=nbsp//'<small>', post='</small>'))
                end if
                write(device,AFORMAT) &
                    endtd//tdaligncenter//trim(itoa(Room(rdx)%Cluster))//endtd// &
                    tdaligncenter//trim(itoa(Room(rdx)%MaxCapacity))//endtd

                termStore = targetTerm
                do term=termBegin,termEnd
                    call qualify_term (term, tYear, targetTerm, tDesc)
                    write(device,AFORMAT) trim(make_href(fnRoomSchedule, itoa(nsect(targetTerm)), &
                        A1=QUERY_put, pre=tdaligncenter, post=endtd))
                end do
                targetTerm = termStore
                write(device,AFORMAT) tdaligncenter//trim(mesg)//endtd//endtr
            end do
            write(device,AFORMAT) '</table>'
        end if
        write(device,AFORMAT) '<hr>'

    end subroutine room_list_all



    subroutine room_conflicts (device, NumSections, Section)
        integer, intent (in) :: device
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer :: rdx, n_count, nsect, sdx, tdx, ierr
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        integer, dimension(60,6) :: TimeTable
        character (len=127) :: mesg

        ! collect rooms
        tArray = 0
        n_count = 0
        ! which college
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)

        call html_comment('room_conflicts('//trim(tCollege)//')')

        targetCollege = index_to_college(tCollege)
        do rdx=1,NumRooms+NumAdditionalRooms
            if (Department(Room(rdx)%DeptIdx)%CollegeIdx/=targetCollege) cycle ! not in college
            call timetable_clear(TimeTable)
            ! collect classes in room
            nsect = 0
            do sdx=1,NumSections
                call meetings_of_section_in_room(NumSections, Section, sdx, rdx, n_meetings, meetings)
                if (n_meetings>0) then ! room assigned to this section
                    ierr = -10
                    call timetable_add_meetings_of_section(NumSections, Section, sdx, &
                    n_meetings, meetings, TimeTable, ierr)
                    if (ierr /= -10) then
                        nsect = nsect+1
                        exit
                    end if
                end if
            end do
            if (nsect>0) then ! conflict
                n_count = n_count + 1
                tArray(n_count) = rdx
            end if
        end do
        mesg = 'Rooms with schedule conflicts in '//tCollege

        call html_write_header(device, mesg)

        if (n_count == 0) then
            write(device,AFORMAT) '(None?)'
        else
            ! sort rooms
            do tdx=1,n_count-1
                do sdx=tdx+1,n_count
                    if (Room(tArray(sdx))%Code<Room(tArray(tdx))%Code) then
                        rdx =tArray(sdx)
                        tArray(sdx) = tArray(tdx)
                        tArray(tdx) = rdx
                    end if
                end do
            end do

            write(device,AFORMAT) '<table border="0" width="50%">'//&
                begintr//thalignleft//'Code'//endth// &
                thaligncenter//'Cluster'//endth, &
                thaligncenter//'Capacity'//endth// &
                thaligncenter//'Classes'//endth// &
                thaligncenter//'Remark'//endth//endtr

            do tdx=1,n_count
                rdx = tArray(tdx)
                ! check conflicts
                mesg = SPACE
                call timetable_clear(TimeTable)
                ! collect classes in room
                nsect = 0
                do sdx=1,NumSections
                    call meetings_of_section_in_room(NumSections, Section, sdx, rdx, n_meetings, meetings)
                    if (n_meetings>0) then ! room assigned to this section
                        nsect = nsect+1
                        tArray(n_count+nsect) = sdx
                        ierr = -10
                        call timetable_add_meetings_of_section(NumSections, Section, sdx, n_meetings, meetings, TimeTable, ierr)
                        if (ierr /= -10) then
                            mesg = red//'Conflict!'//black
                        end if
                    end if
                end do
                QUERY_put = Room(rdx)%Code
                write(device,AFORMAT) begintr//begintd//trim(Room(rdx)%Code)
                if (isRoleAdmin .or. &
                    !(isRoleChair .and. Room(rdx)%DeptIdx==DeptIdxUser) .or. &
                    (isRoleDean .and. Department(Room(rdx)%DeptIdx)%CollegeIdx==CollegeIdxUser) &
                        ) then
                    write(device,AFORMAT) trim(make_href(fnEditRoom, 'Edit', &
                        A1=QUERY_put, pre=nbsp//'<small>', post='</small>'))
                end if
                write(device,AFORMAT) &
                    endtd//tdaligncenter//trim(itoa(Room(rdx)%Cluster))//endtd// &
                    tdaligncenter//trim(itoa(Room(rdx)%MaxCapacity))//endtd
                !if (nsect>0) then
                write(device,AFORMAT) trim(make_href(fnRoomSchedule, itoa(nsect), &
                    A1=QUERY_put, pre=tdaligncenter, post=endtd))
                !else
                !  write(device,AFORMAT) tdaligncenter//trim(itoa(nsect))//endtd
                !end if
                write(device,AFORMAT) tdaligncenter//trim(mesg)//endtd//endtr
            end do
            write(device,AFORMAT) '</table>'
        end if
        write(device,AFORMAT) '<hr>'

    end subroutine room_conflicts


    subroutine room_schedule(device, NumSections, Section, LoadSource)
        integer, intent(in), optional :: LoadSource
        integer, intent (in) :: device
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        integer :: mdx, sdx, tLen1, tLen2, ierr, sect, LoadFromDept
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_ROOM_CODE) :: tRoom
        character(len=MAX_LEN_CLASS_ID) :: tAction, tClassId
        integer, dimension(60,6) :: TimeTable
        logical :: conflicted, assigned, allowed_to_edit
        character(len=127) :: mesg

        call cgi_get_named_string(QUERY_STRING, 'A1', tRoom, ierr)

        call html_comment('room_schedule('//trim(tRoom)//')')

        targetRoom = index_to_room(tRoom)
        targetDepartment = Room(targetRoom)%DeptIdx
        allowed_to_edit = isRoleAdmin .or. &
            (isRoleChair .and. targetDepartment==DeptIdxUser) .or. &
            (isRoleDean .and. Department(targetDepartment)%CollegeIdx==CollegeIdxUser)
        mesg = SPACE

        ! check if there are other arguments
        call cgi_get_named_string(QUERY_STRING, 'A2', tAction, ierr)

        if (ierr==0) then ! action is Add or Del
            call cgi_get_named_string(QUERY_STRING, 'A3', tClassId, ierr)
            sect = index_to_section(tClassId, NumSections, Section)
            if (sect>0) then ! target of action is indexed by sect
                LoadFromDept = Section(sect)%DeptIdx
                if (tAction=='Add') then
                    do mdx=1,Section(sect)%NMeets
                        Section(sect)%RoomIdx(mdx) = targetRoom
                    end do
                    mesg = 'Added '//tClassId
                end if
                if (tAction=='Del') then
                    do mdx=1,Section(sect)%NMeets
                        Section(sect)%RoomIdx(mdx) = 0
                    end do
                    mesg = 'Deleted '//tClassId
                end if
                call xml_write_classes(pathToTerm, NumSections, Section, 0)
            end if
        end if

        call html_write_header(device, 'Classes in room '//tRoom, mesg)

        ! collect classes in room rdx
        call timetable_meetings_in_room(NumSections, Section, targetRoom, 0, tLen1, tArray, TimeTable, conflicted)

        call list_sections_to_edit(device, Section, tLen1, tArray, fnRoomSchedule, tRoom, 'Del', allowed_to_edit)

        if (tLen1>0) call timetable_display(device, Section, TimeTable)
        write(device,AFORMAT) '<hr>'

        ! make list of TBA sections LoadSource that fit the schedule of room
        if (present(LoadSource)) then
            LoadFromDept = LoadSource
        else
            call cgi_get_named_string(QUERY_STRING, 'A4', tDepartment, ierr)
            LoadFromDept = index_to_dept(tDepartment)
            if (ierr/=0 .or. LoadFromDept<=0) then
                LoadFromDept = targetDepartment
            else
                mesg = 'Searched for feasible classes in '//tDepartment
            end if
        end if

        tLen2 = 0
        do sdx=1,NumSections
            if (LoadFromDept/=Section(sdx)%DeptIdx) cycle ! not in this department
            if (Section(sdx)%NMeets==1 .and. Section(sdx)%DayIdx(1)==0) cycle ! meeting days/time not specified
            ! room(s) already assigned to this section?
            assigned = .false.
            do mdx=1,Section(sdx)%NMeets
                if (Section(sdx)%RoomIdx(mdx)/=0) assigned = .true.
            end do
            if (assigned) cycle ! section has a teacher
            if (.not. is_conflict_timetable_with_section(NumSections, Section, sdx, TimeTable)) then ! add to list
                do mdx=1,Section(sdx)%NMeets
                    tArray(tLen1+tLen2+1) = sdx
                    tArray(tLen1+tLen2+2) = mdx
                    tArray(tLen1+tLen2+3) = 0
                    tLen2 = tLen2+3
                end do
            end if
        end do
        tArray(tLen1+tLen2+1) = 0
        tArray(tLen1+tLen2+2) = 0
        tArray(tLen1+tLen2+3) = 0
        if (tLen2>0) then
            call list_sections_to_edit(device, Section, tLen2, tArray(tLen1+1), fnRoomSchedule, tRoom, 'Add', &
            allowed_to_edit, '<b>Classes with TBA rooms in '//trim(Department(LoadFromDept)%Code)// &
            ' that fit available times in '//trim(tRoom)//'</b>')
        end if

        ! search for feasible classes in another department?
        call make_form_start(device, fnRoomSchedule, tRoom)
        write(device,AFORMAT) '<br>Search for feasible classes in : <select name="A4">'
        do mdx=2,NumDepartments
            if (mdx/=LoadFromDept) then
                ierr = 0
            else
                ierr = 1
            end if
            write(device,AFORMAT) '<option value="'//trim(Department(mdx)%Code)//'"'//trim(selected(ierr))//'> '// &
            trim(Department(mdx)%Code)//DASH//trim(Department(mdx)%Name)
        end do
        write(device,AFORMAT) '</select>'//nbsp//'<input type="submit" value="Find classes"></form><hr>'

    end subroutine room_schedule


    subroutine room_info(device, wrk, header, remark, tAction)
        integer, intent(in) :: device
        type (TYPE_ROOM), intent(in) :: wrk
        character (len=*), intent(in)  :: header, remark, tAction
        character(len=MAX_LEN_ROOM_CODE) :: tRoom
        integer :: i, j

        tRoom = wrk%Code
        targetDepartment = wrk%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx

        call html_write_header(device, header, remark)

        call make_form_start(device, fnEditRoom, tRoom)

        write(device,AFORMAT) '<table border="0" width="80%">', &
            begintr//begintd//'Room code'//endtd//begintd//'<input name="Code" size="'//trim(itoa(MAX_LEN_ROOM_CODE))// &
            '" value="'//trim(tRoom)//'">'//endtd//endtr

        write(device,AFORMAT) &
            begintr//begintd//'Responsible department'//endtd//begintd//'<select name="Department">'
        do i=2,NumDepartments
            if (i/=targetDepartment) then
                j=0
            else
                j=1
            end if
            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(Department(i)%Code)//'">'// &
            trim(Department(i)%Name)
        end do
        write(device,AFORMAT) '</select>'//endtd//endtr, &
            begintr//begintd//'Maximum seating capacity'//endtd//begintd//'<input name="MaxCapacity" size="3" value="'// &
            trim(itoa(wrk%MaxCapacity))//'">'//endtd//endtr, &
            begintr//begintd//'Cluster'//endtd//begintd//'<input name="Cluster" size="3" value="'// &
            trim(itoa(wrk%Cluster))//'">'//endtd//endtr

        write(device,AFORMAT) '</table><br>'//nbsp// &
            '<input name="action" type="submit" value="'//trim(tAction)//'"></form><pre>', &
            'NOTE: Rooms that are within walking distance of each other must belong to the same cluster.', &
            '</pre><hr>'

    end subroutine room_info


end module DisplayROOMS
