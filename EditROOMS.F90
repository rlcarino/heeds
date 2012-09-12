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


module EditROOMS

  use HTML

  implicit none

contains

  subroutine room_list_all (device, NumSections, Section, fn)
    integer, intent (in) :: device, fn
    integer, intent (in) :: NumSections
    type (TYPE_SECTION), intent(in), dimension (0:MAX_ALL_SECTIONS) :: Section
    integer :: rdx, n_count, nsect, sdx, tdx, ierr
    integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
    character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
    character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
    integer, dimension(60,6) :: TimeTable
    character (len=127) :: mesg

    ! collect rooms
    tArray = 0
    n_count = 0
    select case (fn)

           case (fnSearch)
                   targetDepartment = DeptIdxUser
                   targetCollege  = Department(targetDepartment)%CollegeIdx
                   ! search string ?
                   call cgi_get_named_string(QUERY_STRING, 'A2', mesg, ierr)
                   if (mesg==SPACE) then
                           targetCollege = CollegeIdxUser
                           targetDepartment = DeptIdxUser
                           call html_write_header(device, 'Search', '<br><hr>Search string not specified.')
                           return
                   else
                           do rdx=1,NumRooms+NumAdditionalRooms
                              if (isRoleChair .and. Room(rdx)%DeptIdx/=DeptIdxUser) cycle
                              if (index(Room(rdx)%Code,trim(mesg)//SPACE)>0) then
                                n_count = n_count+1
                                tArray(n_count) = rdx
                              end if
                           end do
                   end if
                   mesg = 'Search results for "'//trim(mesg)//'" rooms'

           case (fnRoomList, fnNextRoomList)
                   ! which dept ?
                   call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)
                   targetDepartment = index_to_dept(tDepartment)
                   if (ierr/=0 .or. targetDepartment<=0) then
                           targetCollege = CollegeIdxUser
                           targetDepartment = DeptIdxUser
                           call html_write_header(device, 'Search', '<br><hr>Dept "'//tDepartment//'" not found.')
                           return
                   else
                           do rdx=1,NumRooms+NumAdditionalRooms
                              if (targetDepartment==Room(rdx)%DeptIdx) then 
                                n_count = n_count+1
                                tArray(n_count) = rdx
                              end if
                           end do
                   end if
                   mesg = 'Rooms in '//tDepartment

           case (fnRoomConflicts, fnNextRoomConflicts)
                   ! which college
                   call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
                   targetCollege = index_to_college(tCollege)
                   if (ierr/=0 .or. targetCollege<=0) then
                           targetCollege = CollegeIdxUser
                           targetDepartment = DeptIdxUser
                           call html_write_header(device, 'Search', '<br><hr>College "'//trim(tCollege)//'" not found.')
                           return
                   else
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
                   end if
                   mesg = 'Rooms with schedule conflicts in '//tCollege

    end select

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
              begintr//thalignleft//'Code'//endth//thaligncenter//'Area'//endth, &
                       thaligncenter//'Capacity'//endth//&
                       thaligncenter//'Classes'//endth//thaligncenter//'Remark'//endth//endtr
   
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
#if defined DO_NOT_ENCODE
              QUERY_put = Room(rdx)%Code
#else
              call cgi_url_encode(Room(rdx)%Code, QUERY_put)
#endif
              write(device,AFORMAT) begintr//begintd//trim(Room(rdx)%Code)
              if (isRoleAdmin .or. (isRoleChair .and. Room(rdx)%DeptIdx==DeptIdxUser)) then
                write(device,AFORMAT) trim(cgi_make_href(fnEditRoom, targetUser, 'Edit', &
                  A1=QUERY_put, pre='&nbsp;<small>', post='</small>'))
              end if
              write(device,AFORMAT) &
                endtd//tdaligncenter//trim(itoa(Room(rdx)%Cluster))//endtd// &
                tdaligncenter//trim(itoa(Room(rdx)%MaxCapacity))//endtd
              !if (nsect>0) then
                write(device,AFORMAT) trim(cgi_make_href(fnOFFSET+fnRoomSchedule, targetUser, itoa(nsect), &
                  A1=QUERY_put, pre=tdaligncenter, post=endtd))
              !else
              !  write(device,AFORMAT) tdaligncenter//trim(itoa(nsect))//endtd
              !end if
              write(device,AFORMAT) tdaligncenter//trim(mesg)//endtd//endtr
            end do
            write(device,AFORMAT) '</table>'
    end if
    write(device,AFORMAT) '<hr>'

    return
  end subroutine room_list_all 


  subroutine room_schedule(device, NumSections, Section, LoadSource)
    integer, intent(in), optional :: LoadSource
    integer, intent (in) :: device
    integer, intent (in) :: NumSections
    type (TYPE_SECTION), intent(in out), dimension (0:MAX_ALL_SECTIONS) :: Section
    integer :: mdx, sdx, tLen1, tLen2, ierr, sect, LoadFromDept
    character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
    character(len=MAX_LEN_ROOM_CODE) :: tRoom
    character(len=MAX_LEN_CLASS_ID) :: tAction, tClassId
    integer, dimension(60,6) :: TimeTable
    logical :: conflicted, assigned, allowed_to_edit
    character(len=127) :: mesg 

    call cgi_get_named_string(QUERY_STRING, 'A1', tRoom, ierr)
    targetRoom = index_to_room(tRoom)
    if (ierr/=0 .or. targetRoom==0) then
            targetCollege = CollegeIdxUser
            targetDepartment = DeptIdxUser
            call html_write_header(device, 'Search', '<br><hr>Room "'//tRoom//'" not found.')
            return
    end if
    targetDepartment = Room(targetRoom)%DeptIdx
    allowed_to_edit = isRoleAdmin .or. (isRoleChair .and. targetDepartment==DeptIdxUser)
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
                    call xml_write_sections(pathToSections, NumSections, Section, 0)
                    call xml_write_sections(pathToSectionUpdates, NumSections, Section, LoadFromDept)
            end if
    end if

    call html_write_header(device, 'Classes in room '//tRoom, mesg)

    ! collect classes in room rdx
    call timetable_meetings_in_room(NumSections, Section, targetRoom, 0, tLen1, tArray, TimeTable, conflicted)
    call timetable_display(device, Section, TimeTable)
    call list_sections_to_edit(device, Section, tLen1, tArray, fnOFFSET+fnRoomSchedule, tRoom, 'Del', allowed_to_edit)
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
            call list_sections_to_edit(device, Section, tLen2, tArray(tLen1+1), fnOFFSET+fnRoomSchedule, tRoom, 'Add', &
              allowed_to_edit, '<b>Classes with TBA rooms in '//trim(Department(LoadFromDept)%Code)// &
              ' that fit available times in '//trim(tRoom)//'</b>')
    end if

    ! search for feasible classes in another department?
    write(device,AFORMAT) &
        '<br><form name="input" method="post" action="'//CGI_PATH//'">', &
        '<input type="hidden" name="F" value="'//trim(itoa(fnOFFSET+fnRoomSchedule))//'">', &
        '<input type="hidden" name="U" value="'//trim(itoa(targetUser))//'">', &
        '<input type="hidden" name="A1" value="'//trim(tRoom)//'">'


    write(device,AFORMAT) '<br>Search for feasible classes in : <select name="A4">'
    do mdx=2,NumDepartments
      if (mdx/=LoadFromDept) then
                ierr = 0
      else
                ierr = 1
      end if
      write(device,AFORMAT) '<option value="'//trim(Department(mdx)%Code)//'"'//trim(selected(ierr))//'> '// &
        trim(Department(mdx)%Code)//dash//trim(Department(mdx)%Name)
    end do
    write(device,AFORMAT) '</select>&nbsp;<input type="submit" value="Find classes"><hr>'

    return
  end subroutine room_schedule


  subroutine room_edit(device)
    integer, intent(in) :: device
    character(len=MAX_LEN_ROOM_CODE) :: tRoom, tAction
    character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
    integer :: ierr, rdx, i, j
    character (len=255) :: mesg, remark
    type (TYPE_ROOM) :: wrk
    logical :: isDirtyROOMS

    ! which subject ?
    call cgi_get_named_string(QUERY_STRING, 'A1', tRoom, rdx)
    if (rdx/=0 .or. tRoom==SPACE) then
            mesg = 'Room to edit not specified?'
    else
            rdx = index_to_room(tRoom)
            mesg = 'Room code '//tRoom//' is invalid?'
    end if
    if (rdx<=0) then ! subject code is invalid
            targetCollege = CollegeIdxUser
            targetDepartment = DeptIdxUser
            call html_write_header(device, 'Search', '<br><hr>'//trim(mesg))
            return
    end if

    wrk = Room(rdx) ! make a working copy

    ! check for other arguments
    call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
    !write(*,*) 'ierr=', ierr, ', action=', tAction
    isDirtyROOMS = .false.
    remark = SPACE

    select case (trim(tAction))

        case ('Update')
            
            call cgi_get_named_integer(QUERY_STRING, 'MaxCapacity', wrk%MaxCapacity, ierr)
            !write(*,*) 'ierr=', ierr, ', MaxCapacity=', wrk%MaxCapacity
            if (ierr/=0) wrk%MaxCapacity = Room(rdx)%MaxCapacity

            call cgi_get_named_integer(QUERY_STRING, 'Cluster', wrk%Cluster, ierr)
            !write(*,*) 'ierr=', ierr, ', Cluster=', wrk%Cluster
            if (ierr/=0) wrk%Cluster = Room(rdx)%Cluster

            call cgi_get_named_string(QUERY_STRING, 'Code', wrk%Code, ierr)
            !write(*,*) 'ierr=', ierr, ', Code=', wrk%Code
            if (ierr/=0) wrk%Code = Room(rdx)%Code

            call cgi_get_named_string(QUERY_STRING, 'Department', tDepartment, ierr)
            wrk%DeptIdx = index_to_dept(tDepartment)
            !write(*,*) 'ierr=', ierr, ', DeptIdx=', wrk%DeptIdx
            if (ierr/=0 .or. wrk%DeptIdx<=0) wrk%DeptIdx = Room(rdx)%DeptIdx

            if (wrk%Code /= Room(rdx)%Code) then
                isDirtyROOMS = .true.
                remark = trim(remark)//': Code changed to '//wrk%Code
            end if

            if (wrk%DeptIdx /= Room(rdx)%DeptIdx) then
                isDirtyROOMS = .true.
                remark = trim(remark)//': Department changed to '//Department(wrk%DeptIdx)%Code
            end if

            if ( wrk%MaxCapacity /= Room(rdx)%MaxCapacity) then
                isDirtyROOMS = .true.
                remark = trim(remark)//': Max seating capacity changed to '//itoa(wrk%MaxCapacity)
            end if

            if ( wrk%Cluster /= Room(rdx)%Cluster) then
                isDirtyROOMS = .true.
                remark = trim(remark)//': Cluster changed to '//itoa(wrk%Cluster)
            end if

            if (isDirtyROOMS) then
                    if ( wrk%Code /= Room(rdx)%Code) then
                            ! add new subject?
                            j = index_to_room(wrk%Code)
                            if (j==0) then
                                    NumAdditionalRooms = NumAdditionalRooms+1
                                    Room(NumRooms+NumAdditionalRooms) = wrk
                                    rdx = NumRooms+NumAdditionalRooms
                                    tRoom = wrk%Code
                                    remark = ': Added new room '//wrk%Code
                            else
                                    remark = ': Add new room failed; "'//trim(wrk%Code)//'" already exists.'
                                    isDirtyROOMS = .false.
                            end if
                    else
                            ! update existing
                            Room(rdx) = wrk
                    end if
            end if

        case default
                !write(*,*) 'Unknown action: '//tAction


    end select
    
    if (isDirtyROOMS) then
            call xml_write_rooms(pathToCurrent)
            call html_college_links(device, Department(wrk%DeptIdx)%CollegeIdx, trim(tRoom)//remark)
            return
    end if

    targetDepartment = Room(rdx)%DeptIdx

    call html_write_header(device, 'Edit room '//tRoom, remark(3:))

    write(device,AFORMAT) &
      '<form name="input" method="post" action="'//CGI_PATH//'">', &
      '<input type="hidden" name="U" value="'//trim(itoa(targetUser))//'">', &
      '<input type="hidden" name="F" value="'//trim(itoa(fnEditRoom))//'">'// &
      '<input type="hidden" name="A1" value="'//trim(tRoom)//'">', &
      '<table border="0" width="100%">'

    write(device,AFORMAT) &
      begintr//begintd//'Room code'//endtd//begintd//'<input name="Code" size="'//trim(itoa(MAX_LEN_ROOM_CODE))// &
        '" value="'//trim(tRoom)//'"> (A new room will be created if this is changed)'//endtd//endtr
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
        trim(itoa(Room(rdx)%MaxCapacity))//'">'//endtd//endtr, &
      begintr//begintd//'Cluster'//endtd//begintd//'<input name="Cluster" size="3" value="'// &
        trim(itoa(Room(rdx)%Cluster))//'">'//endtd//endtr

    write(device,AFORMAT) '</table><br>&nbsp;<input name="action" type="submit" value="Update"></form><pre>', &
      'NOTE: Rooms that are located in buildings within walking distance of each other belong to the same cluster.', &
      '</pre><hr>'

    return
  end subroutine room_edit


end module EditROOMS
