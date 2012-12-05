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


module EditSECTIONS

  use HTML

  implicit none

contains



  subroutine teacher_search_given_time(device, NumSections, Section, wrk, to_skip, teacher_count, given_teacher_dept)
    integer, intent (in) :: to_skip, device
    integer, intent (in) :: NumSections
    type (TYPE_SECTION), intent(in), dimension (0:) :: Section
    integer, intent (out) :: teacher_count
    type (TYPE_SECTION), intent (in) :: wrk
    integer, intent (in), optional :: given_teacher_dept
    integer :: i, j, ierr, idx, tdx, teacher_dept
    integer, dimension(60,6) :: TimeTable
    integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
    character :: ch
    integer :: ncol
    logical :: skip

    !write(*,*) 'Owner of section=', wrk%DeptIdx
    if (present(given_teacher_dept)) then
            teacher_dept = given_teacher_dept
            !write(*,*) 'Given teacher dept=', teacher_dept
    else
            teacher_dept = Subject(wrk%SubjectIdx)%DeptIdx
            !write(*,*) 'Dept of subject=', teacher_dept
    end if
    ierr = -10
    call timetable_clear(TimeTable)
    call timetable_add_struct_section(wrk, TimeTable, ierr)

    ! collect teachers into tArray()
    write(device,AFORMAT) '<b>Teachers in '//trim(Department(teacher_dept)%Code)//' available to teach</b>'
    do i=1,wrk%NMeets
      if (i<wrk%NMeets) then
              ch = COMMA
      else
              ch = SPACE
      end if
      write(device,AFORMAT) nbsp//txtDay(wrk%DayIdx(i))//nbsp// &
        txtTime(wrk%bTimeIdx(i))//dash//trim(txtTime(wrk%eTimeIdx(i)))//ch
    end do
    write(device,AFORMAT) '<table border="0" width="100%">', &
      begintr//'<th align="left">Teacher</th><th align="left">Assigned classes</th>'//endtr
    teacher_count = 0
    do idx=1,NumTeachers+NumAdditionalTeachers
      tdx = TeacherRank(idx)
      if (Teacher(tdx)%DeptIdx /= teacher_dept) cycle
      skip = .false.
      !tArray = 0 ! assigned classes
      ncol = 0
      do i=1,NumSections
        if (Section(i)%SubjectIdx==0) cycle ! section was deleted
        if (i==to_skip) cycle
        call meetings_of_section_by_teacher(NumSections, Section, i, tdx, n_meetings, meetings)
        if (n_meetings==0) cycle ! teacher not assigned to this section
        ncol = ncol+1
        tArray(ncol) = i
        if (is_conflict_timetable_with_section_meetings(NumSections, Section, i, n_meetings, meetings, TimeTable)) then
          skip = .true. ! teacher not available
          exit ! do not consider the remaining sections for this teacher
        end if
      end do
      if (skip) cycle ! done with this teacher
      teacher_count = teacher_count+1
      write(device,AFORMAT) trim(cgi_make_href(fnOFFSET+fnTeacherSchedule, Teacher(tdx)%Name, &
          A1=Teacher(tdx)%TeacherID, &
          pre=begintr//begintd, &
          post=endtd//begintd//'['//trim(itoa(ncol))//']'))
      do j=1,ncol
        write(device,AFORMAT) ' : '//Section(tArray(j))%ClassId
      end do
      write(device,AFORMAT) endtd//endtr
    end do
    if (teacher_count == 0) write(device,AFORMAT) &
      begintr//'<td colspan="2"><i>(No teachers available during specified times, or time not specified)</i>'//endtd//endtr
    write(device,AFORMAT) '</table>'

    return
  end subroutine teacher_search_given_time


  subroutine room_search_given_time(device, NumSections, Section, wrk, to_skip, room_count, given_room_dept)
    integer, intent (in) :: to_skip, device
    integer, intent (in) :: NumSections
    type (TYPE_SECTION), intent(in), dimension (0:) :: Section
    integer, intent (out) :: room_count
    type (TYPE_SECTION), intent (in) :: wrk
    integer, intent (in), optional :: given_room_dept
    integer :: i, j, ierr, rdx, room_dept
    integer, dimension(60,6) :: TimeTable
    integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
    character :: ch
    integer :: ncol
    logical :: skip

    if (present(given_room_dept)) then
            room_dept = given_room_dept
    else
            room_dept = Subject(wrk%SubjectIdx)%DeptIdx
    end if
    ierr = -10
    call timetable_clear(TimeTable)
    call timetable_add_struct_section(wrk, TimeTable, ierr)

    ! collect rooms into tArray()
    write(device,AFORMAT) '<b>Rooms in '//trim(Department(room_dept)%Code)// &
      ' available during</b>'
    do i=1,wrk%NMeets
      if (i<wrk%NMeets) then
              ch = COMMA
      else
              ch = SPACE
      end if
      write(device,AFORMAT) nbsp//txtDay(wrk%DayIdx(i))//nbsp// &
        txtTime(wrk%bTimeIdx(i))//dash//trim(txtTime(wrk%eTimeIdx(i)))//ch
    end do
    write(device,AFORMAT) '<table border="0" width="100%">', &
      begintr//'<th align="left">Room</th><th align="left">[Count]: Scheduled classes</th>'//endtr
    room_count = 0
    do rdx=1,NumRooms+NumAdditionalRooms
      if (Room(rdx)%DeptIdx /= room_dept) cycle
      !write(*,*) 'Room is ', Room(rdx)%Code
      skip = .false.
      !tArray = 0 ! current classes
      ncol = 0
      do i=1,NumSections
        if (Section(i)%SubjectIdx==0) cycle ! section was deleted
        if (i==to_skip) cycle
        call meetings_of_section_in_room(NumSections, Section, i, rdx, n_meetings, meetings)
        if (n_meetings==0) cycle ! room not assigned to this section
        !write(*,*) 'Section is ', Section(i)%ClassId
        ncol = ncol+1
        tArray(ncol) = i
        if (is_conflict_timetable_with_section_meetings(NumSections, Section, i, n_meetings, meetings, TimeTable)) then
          !write(*,*) '        '//Section(i)%ClassId//' is in conflict!' 
          skip = .true. ! room not available
          exit ! do not consider the remaining sections for this room
        end if
      end do
      if (skip) cycle ! done with this room
      !write(*,*) '        '//Room(rdx)%Code//' is OK!' 
      room_count = room_count+1
#if defined DO_NOT_ENCODE
      QUERY_put = Room(rdx)%Code
#else
      call cgi_url_encode(Room(rdx)%Code, QUERY_put)
#endif
      write(device,AFORMAT) trim(cgi_make_href(fnOFFSET+fnRoomSchedule, Room(rdx)%Code, &
          A1=QUERY_put, &
          pre=begintr//begintd, &
          post=endtd//begintd//'['//trim(itoa(ncol))//']'))
      do j=1,ncol
        write(device,AFORMAT) ' : '//Section(tArray(j))%ClassId
      end do
      write(device,AFORMAT) endtd//endtr
    end do
    if (room_count == 0) write(device,AFORMAT) &
      begintr//'<td colspan="2"><i>(No rooms available during specified times, or time not specified)</i>'//endtd//endtr
    write(device,AFORMAT) '</table>'

    return
  end subroutine room_search_given_time


  subroutine section_list_all (device, NumSections, Section, Offering, NumBlocks, Block,  fn, givenDept, mesg)
    ! write list of sections in department 'dept' to unit 'device'
    integer, intent (in) :: device, fn
    integer, intent (in out) :: NumSections
    type (TYPE_SECTION), intent(in out), dimension (0:) :: Section
    integer, intent (in) :: NumBlocks
    type (TYPE_BLOCK), dimension(0:), intent(in) :: Block
    type (TYPE_OFFERED_SUBJECTS), intent(in out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
    integer, intent(in), optional :: givenDept
    character(len=*), intent(in), optional :: mesg

    integer :: cdx, idx, jdx, i, rdx, sdx, tdx, ncol, nclosed, maxcol=7, nopen, nsections
    integer :: owner_dept, owner_coll, previous
    character(len=127) :: header
    character (len=MAX_LEN_SUBJECT_CODE) :: tSubject, tSeats, searchString
    character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
    character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
    logical :: isLecture, okToAdd

    targetDepartment = DeptIdxUser
    targetCollege = CollegeIdxUser
    call recalculate_available_seats(Section)
    select case(fn)

        case (fnScheduleOfClasses, fnNextScheduleOfClasses)
          if (present(givenDept)) then
            targetDepartment = givenDept
            i = 0
          else
            call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, i)
            targetDepartment = index_to_dept(tDepartment)
          end if
          if (i/=0 .or. targetDepartment==0) then
            call html_write_header(device, 'Classes in department', '<br><hr>Department "'//tDepartment//'" not found')
            return
          end if
          targetCollege = Department(targetDepartment)%CollegeIdx
          header = 'Classes in '//Department(targetDepartment)%Code

        case (fnSearch) ! search sections matched by specified substring
          header = 'Search for classes containing text'
          call cgi_get_named_string(QUERY_STRING, 'A2', searchString, i)
          if (i/=0 .or. searchString==SPACE) then
            call html_write_header(device, header, '<br><hr>Search string not specified')
            return
          end if
          targetDepartment = givenDept
          targetCollege = Department(targetDepartment)%CollegeIdx
          header = trim(header)//' "'//trim(searchString)//'"'

        case (fnScheduleByArea, fnNextScheduleByArea) ! sections by subject area
          header = 'Classes in subject area'
          call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, i)
          if (i/=0 .or. tCollege==SPACE) then
            call html_write_header(device, header, '<br><hr>College not specified')
            return
          end if
          targetCollege = index_to_college(tCollege)
          call cgi_get_named_string(QUERY_STRING, 'A2', searchString, i)
          if (i/=0 .or. searchString==SPACE) then
            call html_write_header(device, header, '<br><hr>Subject area not specified')
            return
          end if
          header = trim(header)//' "'//trim(searchString)//'"'
#if defined UPLB
          ! department already set above
#else
          ! Subjects administered by program
          tDepartment = tCollege
          targetDepartment = index_to_dept(tDepartment)
#endif

        case (fnTBARooms,fnNextTBARooms,fnTBATeachers,fnNextTBATeachers) ! TBA rooms, teachers
          header = 'Classes with TBA room/teacher'
          call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, i)
          if (i/=0 .or. tCollege==SPACE) then
            call html_write_header(device, header, '<br><hr>College not specified')
            return
          end if
          targetCollege = index_to_college(tCollege)
          header = tCollege//header
#if defined UPLB
          ! department already set above
#else
          ! Subjects administered by program
          tDepartment = tCollege
          targetDepartment = index_to_dept(tDepartment)
#endif
    end select

    if (present(mesg)) then
            call html_write_header(device, header, mesg)
    else
            call html_write_header(device, header)
    end if

    ! make list of sections
    nsections = 0
    do sdx=1,NumSections
        cdx = Section(sdx)%SubjectIdx
        if (cdx==0) cycle ! section was deleted
        owner_dept = Section(sdx)%DeptIdx
        owner_coll = Department(owner_dept)%CollegeIdx
        okToAdd = .false.

        select case(fn)

          case (fnScheduleOfClasses, fnNextScheduleOfClasses)
            okToAdd = owner_dept==targetDepartment

          case (fnSearch) ! search sections matched by specified substring
            okToAdd = index(Section(sdx)%ClassId, trim(searchString))>0

          case (fnScheduleByArea, fnNextScheduleByArea) ! sections by subject area
            okToAdd = index(Section(sdx)%ClassId, trim(searchString)//SPACE)==1

          case (fnTBARooms,fnNextTBARooms) ! sections with TBA rooms
            if (owner_coll==targetCollege) then
              i = 0
              do ncol=1,Section(sdx)%NMeets
                if (Section(sdx)%RoomIdx(ncol)==0) i = i+1
              end do
              okToAdd = i>0
            end if

          case (fnTBATeachers,fnNextTBATeachers) ! sections with TBA teachers
            if (owner_coll==targetCollege) then
              i = 0
              do ncol=1,Section(sdx)%NMeets
                if (Section(sdx)%TeacherIdx(ncol)==0) i = i+1
              end do
              okToAdd = i>0
            end if

        end select

        if (okToAdd) then
                nsections = nsections + 1
                tArray(nsections) = sdx
        end if

    end do ! sdx=1,NumSections
    if (nsections==0) then
        write(device,AFORMAT) '<br>(None)<hr>'
        return
    end if

    ! sort sections by class id
    do idx=1,nsections-1
      do jdx=idx+1,nsections
        if (Section(tArray(jdx))%ClassId<Section(tArray(idx))%ClassId) then
                ncol = tArray(jdx)
                tArray(jdx) = tArray(idx)
                tArray(idx) = ncol
        end if
      end do
    end do
    !write(*,*) nsections, ' sections'

    ! make list subjects
    nopen = 0
    previous = 0
    do idx=1,nsections
      cdx = Section(tArray(idx))%SubjectIdx
      if (previous/=cdx) then
        nopen = nopen+1
        tArray(nsections+nopen) = cdx
        previous = cdx
      end if
    end do
    !write(*,*) nopen, ' subjects'

    ! write shortcut to subjects
    write(device,AFORMAT) '<table border="0" width="100%">'
    ncol = 0
    do idx=1,nopen
        tSubject = Subject(tArray(nsections+idx))%Name
        i = index(tSubject, dash)
        if (tSubject(1:3) /= 'PE ' .and. i > 0) cycle
        ncol = ncol + 1
        if (ncol == 1) then
            write(device,AFORMAT) begintr//begintd//'<a href="#'//trim(tSubject)//'">'//trim(tSubject)//'</a>'//endtd
        else if (ncol == maxcol) then
            write(device,AFORMAT) begintd//'<a href="#'//trim(tSubject)//'">'//trim(tSubject)//'</a>'//endtd//endtr
            ncol = 0
        else
            write(device,AFORMAT) begintd//'<a href="#'//trim(tSubject)//'">'//trim(tSubject)//'</a>'//endtd
        end if
    end do
    if (ncol /= 0)  then
        do i=ncol+1,maxcol
            write(device,AFORMAT) tdnbspendtd
        end do
        write(device,AFORMAT) endtr
    end if
    write(device,AFORMAT) '</table>'

    nclosed = 0
    ! make list of closed subjects here, starting at tArray(nsections+nopen+1)
#if defined UPLB
    do cdx=1,NumSubjects+NumAdditionalSubjects
        if (Offering(cdx)%NSections>0) cycle
        if (Subject(cdx)%DeptIdx/=targetDepartment) cycle
        nclosed = nclosed+1
        tArray(nsections+nopen+nclosed) = cdx
    end do
#else
    ! Subjects administered by program
    do cdx=1,NumSubjects+NumAdditionalSubjects
        if (Offering(cdx)%NSections>0) cycle
        if (.not. is_used_in_college_subject(targetCollege, cdx)) cycle
        nclosed = nclosed+1
        tArray(nsections+nopen+nclosed) = cdx
    end do
#endif

    ! offer to open sections
    if (nclosed>0 .and. (isRoleAdmin .or. (isRoleChair .and. DeptIdxUser==targetDepartment))) then
            write(device,AFORMAT) &
              '<form name="input" method="post" action="'//CGI_SCRIPT//'">', &
              '<input type="hidden" name="F" value="'//trim(itoa(fnOFFSET+fnScheduleOfferSubject))//'">', &
              '<input type="hidden" name="A2" value="'//trim(Department(targetDepartment)%Code)//'">', &
              '<table border="0" width="100%">'//begintr//'<td colspan="'//trim(itoa(maxcol))//'" align="right">', &
              'Open a section in <select name="A1"> <option value=""> (select subject)'
            do idx=1,nclosed
                tSubject = Subject(tArray(nsections+nopen+idx))%Name
                write(device,AFORMAT) '<option value="'//trim(tSubject)//'"> '//tSubject
            end do
            write(device,AFORMAT) '</select> '//nbsp//nbsp//' <input type="submit" value="Submit">'// &
              endtd//endtr//'</table></form>'
    end if

    write(device,AFORMAT) '<hr><br>', &
        '<i>Note: If the hyperlinks are active, the section code links to the gradesheet,', &
        ' the block name links to the block schedule, and the no. of seats links to the classlist. ', &
        ' Deleting a lecture section automatically deletes the associated laboratory or recitation sections. ', &
        ' Laboratory or recitation section codes MUST have the format "LECT-nL" or "LECT-nR" where "LECT" is ', &
        ' the code for the lecture section, and "n" is an integer.</i><br><br>', &
        '<table border="0" width="100%">'//begintr, &
        thalignleft//'Subject'//endth//&
        thalignleft//'Section'//endth//&
        thalignleft//'Block'//endth//&
        thalignleft//'Seats/Open'//endth//&
        thalignleft//'Day'//endth//&
        thalignleft//'Time'//endth//&
        thalignleft//'Room'//endth//&
        thalignleft//'Teacher'//endth//&
        thalignleft//'<small>Action</small>'//endth//endtr

    do idx=1,nopen
      cdx = tArray(nsections+idx)

      if (is_lecture_lab_subject(cdx)) then ! subject is lecture-lab
              tDepartment = 'lect'
      else
              tDepartment = 'sect'
      end if
#if defined DO_NOT_ENCODE
      QUERY_put = Subject(cdx)%Name
#else
      call cgi_url_encode(Subject(cdx)%Name, QUERY_put)
#endif
      write(device,AFORMAT) &
          begintr//'<td colspan="8">'//nbsp//endtd, & 
          begintd//'<a name="'//trim(Subject(cdx)%Name)//'"></a><small>[<a href="#TOP">Top</a>]</small>'//endtd//endtr
      write(device,AFORMAT) &
          begintr//begintd//trim(Subject(cdx)%Name)//endtd//'<td colspan="8">'// &
          trim(Subject(cdx)%Title)//'. '//trim(ftoa(Subject(cdx)%Units,1))//' units. '

      if (is_lecture_lab_subject(cdx)) then
        write(device,AFORMAT) &
          trim(ftoa(Subject(cdx)%LectHours+Subject(cdx)%LabHours,2))//' hrs ('// &
          trim(ftoa(Subject(cdx)%LectHours,2))//' lect + '// &
          trim(ftoa(Subject(cdx)%LabHours,2))//' lab/recit).'
      else if (Subject(cdx)%LectHours > 0.0) then
        write(device,AFORMAT) trim(ftoa(Subject(cdx)%LectHours,2))//' hrs lect.'
      else if (Subject(cdx)%LabHours > 0.0) then
        write(device,AFORMAT) trim(ftoa(Subject(cdx)%LabHours,2))//' hrs lab/recit.'
      end if

      write(device,AFORMAT) '('//trim(text_term_offered_separated(Subject(cdx)%TermOffered))//')'
      if (isRoleAdmin) then
           write(device,AFORMAT) trim(cgi_make_href(fnEditSubject, 'Edit', A1=Subject(cdx)%Name, &
                  pre=nbsp//'<small>[ ', post=' ]</small>'))
      end if
      write(device,AFORMAT) endtd//endtr, &
          begintr//tdnbspendtd//'<td colspan="7">'//nbsp//'Pr. '//trim(text_prerequisite_of_subject(cdx,0))//endtd

#if defined UPLB
      okToAdd = isRoleAdmin .or. (isRoleChair .and. DeptIdxUser==Subject(cdx)%DeptIdx)
#else
      ! Subjects administered by program
      okToAdd = isRoleAdmin .or. (isRoleChair .and. DeptIdxUser==targetDepartment)
#endif

      if (okToAdd) then
          write(device,AFORMAT) begintd//'<small>', &
              trim(cgi_make_href(fnOFFSET+fnScheduleOfferSubject, 'Add '//tDepartment, &
                A1=QUERY_put, A2=Department(targetDepartment)%Code, &
                post='</small>'//endtd//endtr)) 
      else
              write(device,AFORMAT) tdnbspendtd//endtr
      end if

      ! sections
      do jdx=1,nsections
        sdx = tArray(jdx)
        if (Section(sdx)%SubjectIdx/=cdx) cycle

        owner_dept = Section(sdx)%DeptIdx
        owner_coll = Department(owner_dept)%CollegeIdx

        isLecture = is_lecture_lab_subject(cdx) .and. is_lecture_class(sdx, Section) ! empty SPACE
        if (isLecture) then
                write(device,AFORMAT) begintr//'<td colspan="9">'//nbsp//endtd//endtr
                tSeats = itoa(Section(sdx)%Slots)
        else
                tSeats = trim(itoa(Section(sdx)%Slots))//fslash//trim(itoa(Section(sdx)%RemSlots))
        end if
#if defined DO_NOT_ENCODE
        QUERY_put = Section(sdx)%ClassId
#else
        call cgi_url_encode(Section(sdx)%ClassId, QUERY_put)
#endif
        ! subject 
        write(device,AFORMAT) begintr//tdnbspendtd

        ! section code, link to gradesheet entry form
        if (fnOFFSET==0 .and. available(fnGradeSheet) .and. &
            (isRoleAdmin .or. (isRoleChair .and. DeptIdxUser==owner_dept))) then
          write(device,AFORMAT) trim(cgi_make_href(fnGradeSheet, trim(Section(sdx)%Code), &
          A1=QUERY_PUT, pre=begintd, post=endtd))
        else
          write(device,AFORMAT) begintd//trim(Section(sdx)%Code)//endtd
        end if

        ! blocks
        write(device,AFORMAT) begintd
        call blocks_in_section(device, sdx, fnOFFSET+fnBlockSchedule, NumBlocks, Block)
        write(device,AFORMAT) endtd

        ! seats, link to classlist
        if (fnOFFSET==0 .and. available(fnClassList)) then
          write(device,AFORMAT) trim(cgi_make_href(fnClassList, tSeats, &
          A1=QUERY_PUT, pre=begintd, post=endtd))
        else
          write(device,AFORMAT) begintd//trim(tSeats)//endtd
        end if

        ! time, day, room, teacher
        if (is_regular_schedule(sdx, Section)) then
          tdx = Section(sdx)%TeacherIdx(1) 
          rdx = Section(sdx)%RoomIdx(1) 
          write(device,AFORMAT) &
              begintd//trim(text_days_of_section(sdx, NumSections, Section))//endtd// &
              begintd//trim(text_time_period(Section(sdx)%bTimeIdx(1), Section(sdx)%eTimeIdx(1)))//endtd
          if (rdx/=0) then
              write(device,AFORMAT) begintd//trim(Room(rdx)%Code)//endtd
          else
              write(device,AFORMAT) begintd//red//trim(Room(rdx)%Code)//black//endtd
          end if
          if (tdx/=0) then
                  write(device,AFORMAT) begintd//trim(Teacher(tdx)%Name)//endtd
          else
              write(device,AFORMAT) begintd//red//trim(Teacher(tdx)%Name)//black//endtd
          end if

        else

          do ncol=1,Section(sdx)%NMeets
            tdx = Section(sdx)%TeacherIdx(ncol) 
            rdx = Section(sdx)%RoomIdx(ncol) 
            write(device,AFORMAT) &
              begintd//txtDay(Section(sdx)%DayIdx(ncol))//endtd// &
              begintd//trim(text_time_period(Section(sdx)%bTimeIdx(ncol), Section(sdx)%eTimeIdx(ncol)))//endtd
            if (rdx/=0) then
                    write(device,AFORMAT) begintd//trim(Room(rdx)%Code)//endtd
            else
                    write(device,AFORMAT) begintd//red//trim(Room(rdx)%Code)//black//endtd
            end if
            if (tdx/=0) then
                    write(device,AFORMAT) begintd//trim(Teacher(tdx)%Name)
            else
                    write(device,AFORMAT) begintd//red//trim(Teacher(tdx)%Name)//black
            end if
            if (ncol<Section(sdx)%NMeets) then
                    write(device,AFORMAT) endtd//begintd//endtd//endtr// &
                      begintr//begintd//endtd//begintd//endtd//begintd//endtd//begintd//endtd
            else
                  write(device,AFORMAT) endtd
            end if
          end do

        end if
        write(device,AFORMAT) begintd//'<small>'
        if (isRoleAdmin .or. (isRoleChair .and. DeptIdxUser==owner_dept)) then
            write(device,AFORMAT) trim(cgi_make_href(fnOFFSET+fnScheduleEdit, ' Edit', &
                A1=QUERY_put))
            write(device,AFORMAT) trim(cgi_make_href(fnOFFSET+fnScheduleDelete, ' Del', &
                A1=QUERY_put))
            if (isLecture) then
              write(device,AFORMAT) trim(cgi_make_href(fnOFFSET+fnScheduleAddLab, 'Add lab', &
                A1=QUERY_put))
            end if
        else
            write(device,AFORMAT) nbsp
        end if
        write(device,AFORMAT) '</small>'//endtd//endtr

        ! correct no. of hours?
        if (.not. is_consistent_section_hours_with_subject_defn(Section(sdx))) then
                write(device,AFORMAT) begintr//'<td align="center" colspan="9">'// &
                  red//'Total meeting hours is inconsistent with the subject specifications?'//black//endtd//endtr
        end if
        ! meeting conflicts?
        if (.not. is_conflict_free_section_hours(Section(sdx), NumSections, Section)) then
                write(device,AFORMAT) begintr//'<td align="center" colspan="9">'// &
                  red//'Conflict in meeting times, or conflict with lecture section?'//black//endtd//endtr
        end if

      end do

    end do
    write(device,AFORMAT) '</table><hr>'

    return
  end subroutine section_list_all
 

  subroutine section_offer_subject(device, NumSections, Section, Offering, NumBlocks, Block)
    integer, intent (in) :: device, NumBlocks
    type (TYPE_BLOCK), dimension(0:), intent(in) :: Block
    integer, intent (in out) :: NumSections
    type (TYPE_SECTION), intent(in out), dimension (0:) :: Section
    type (TYPE_OFFERED_SUBJECTS), intent(in out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
    character(len=MAX_LEN_SUBJECT_CODE) :: tSubject
    character(len=MAX_LEN_SECTION_CODE) :: tSection
    character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
    integer :: crse, Term, kdx, dept
    character (len=127) :: mesg

    ! what subject to offer ?
    call cgi_get_named_string(QUERY_STRING, 'A1', tSubject, crse)
    if (crse/=0 .or. tSubject==SPACE) then
            mesg = 'Subject to offer not specified?'
    else
            crse = index_to_subject(tSubject)
            mesg = 'In section_offer_subject: Subject code '//tSubject//' is invalid?'
    end if
    if (crse<=0) then ! subject code is invalid
          targetCollege = CollegeIdxUser
          targetDepartment = DeptIdxUser
          call html_write_header(device, 'Open a section', '<br><hr>'//mesg)
          return
    end if

#if defined UPLB
    ! Subject administered by departments
    targetDepartment = Subject(crse)%DeptIdx
    targetCollege = Department(targetDepartment)%CollegeIdx
    tDepartment = Department(targetDepartment)%Code
#else
    ! Subjects administered by program
    call cgi_get_named_string(QUERY_STRING, 'A2', tDepartment, kdx)
    targetDepartment = index_to_dept(tDepartment)
    targetCollege = Department(targetDepartment)%CollegeIdx
#endif

    dept = targetDepartment
    if (fnOFFSET==0) then
            Term = currentTerm
    else
            Term = nextTerm
    end if
    kdx = ScheduleCount(Term,targetDepartment) + 1 ! new section in department
    ScheduleCount(Term,targetDepartment) = kdx
    if (kdx>99) then
            tSection = Department(targetDepartment)%SectionPrefix//itoa(kdx)
    else if (kdx>9) then
            tSection = Department(targetDepartment)%SectionPrefix//'0'//itoa(kdx)
    else 
            tSection = Department(targetDepartment)%SectionPrefix//'00'//itoa(kdx)
    end if
    
    if (Subject(crse)%LectHours>0) then ! subject has lecture
            NumSections = NumSections+1
            Section(NumSections) = TYPE_SECTION (trim(Subject(crse)%Name)//' '//tSection, tSection, SPACE, &
              targetDepartment, crse, Subject(crse)%MaxLectSize, Subject(crse)%MaxLectSize, 1, 0, 0, 0, 0, 0)
    end if
    if (Subject(crse)%LabHours>0) then ! subject has lab/recitation
            NumSections = NumSections+1
            tSection = trim(tSection)//dash//'0L'
            Section(NumSections) = TYPE_SECTION (trim(Subject(crse)%Name)//' '//tSection, tSection, SPACE, &
              targetDepartment, crse, Subject(crse)%MaxLabSize, Subject(crse)%MaxLabSize, 1, 0, 0, 0, 0, 0)
    end if

    call offerings_summarize(NumSections, Section, Offering)
    call section_list_all (device, NumSections, Section, Offering, NumBlocks, Block,  &
      fnOFFSET+fnScheduleOfClasses, dept, 'Opened a section in '//tSubject)

    return
  end subroutine section_offer_subject

 
  subroutine section_add_laboratory(device, NumSections, Section, Offering, NumBlocks, Block)
    integer, intent (in) :: device, NumBlocks
    type (TYPE_BLOCK), dimension(0:), intent(in) :: Block
    integer, intent (in out) :: NumSections
    type (TYPE_SECTION), intent(in out), dimension (0:) :: Section
    type (TYPE_OFFERED_SUBJECTS), intent(in out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
    character(len=MAX_LEN_CLASS_ID) :: tClassId
    character(len=MAX_LEN_SECTION_CODE) :: tSection
    character (len=127) :: mesg
    integer :: crse, sect, l, dept

    call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, sect)
    if (sect/=0 .or. tClassId==SPACE) then
            mesg = 'Lecture section not specified?'
    else
            sect = index_to_section(tClassId, NumSections, Section)
            mesg = 'Section "'//tClassId//'" not found?'
    end if
    if (sect<=0) then ! section is invalid
          targetCollege = CollegeIdxUser
          targetDepartment = DeptIdxUser
          call html_write_header(device, 'Add lab', '<br><hr>'//mesg)
          return
    end if

    dept = Section(sect)%DeptIdx
    targetDepartment = dept
    targetCollege = Department(dept)%CollegeIdx
    crse = Section(sect)%SubjectIdx

    l = len_trim(Section(sect+1)%Code) ! last letter is L(ab) or R(ecit)
    tSection = trim(Section(sect)%Code)//dash//'0'//Section(sect+1)%Code(l:l)
    NumSections = NumSections+1
    Section(NumSections) = TYPE_SECTION (trim(Subject(crse)%Name)//' '//tSection, tSection, SPACE, &
              targetDepartment, crse, Subject(crse)%MaxLabSize, Subject(crse)%MaxLabSize, 1, 0, 0, 0, 0, 0)
    !write(*,*) 'Adding '//trim(Subject(crse)%Name)//' '//tSection

    call offerings_summarize(NumSections, Section, Offering)
    call section_list_all (device, NumSections, Section, Offering, NumBlocks, Block,  &
      fnOFFSET+fnScheduleOfClasses, dept, 'Opened new section '//trim(Subject(crse)%Name)//' '//tSection)

    return
  end subroutine section_add_laboratory

 
  subroutine section_delete(device, NumSections, Section, Offering, NumBlocks, Block)
    integer, intent (in) :: device, NumBlocks
    type (TYPE_BLOCK), dimension(0:), intent(in out) :: Block
    integer, intent (in out) :: NumSections
    type (TYPE_SECTION), intent(in out), dimension (0:) :: Section
    type (TYPE_OFFERED_SUBJECTS), intent(in out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
    character(len=MAX_LEN_CLASS_ID) :: tClassId
    integer :: sect, crse, pos, i, dept
    character (len=127) :: mesg

    call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, sect)
    if (sect/=0 .or. tClassId==SPACE) then
            mesg = 'Section to delete not specified?'
    else
            sect = index_to_section(tClassId, NumSections, Section)
            mesg = 'Section "'//tClassId//'" not found?'
    end if
    if (sect<=0) then ! section is invalid
          targetCollege = CollegeIdxUser
          targetDepartment = DeptIdxUser
          call html_write_header(device, 'Delete section', '<br><hr>'//mesg)
          return
    end if

    dept = Section(sect)%DeptIdx
    targetDepartment = dept
    targetCollege = Department(dept)%CollegeIdx
    crse = Section(sect)%SubjectIdx
    mesg = 'Deleted section '//tClassId

    if (.not. is_lecture_lab_subject(crse)) then ! lecture only, or lab only
            !write(*,*) 'Removing '//tClassId
            Call delete_section_from_blocks(sect, NumSections, Section, NumBlocks, Block)
    else ! lecture-lab subject
            if (is_lecture_class(sect, Section)) then ! remove lecture and lab sections
                    write(*,*) 'Deleted '//tClassId
                    Call delete_section_from_blocks(sect, NumSections, Section, NumBlocks, Block)
                    tClassId = trim(tClassId)//dash
                    i = len_trim(tClassId)
                    !write(*,*) 'Sections to remove: '//tClassId
                    do pos=1,NumSections
                      if (Section(pos)%ClassId(:i)/=tClassId(:i)) cycle
                      write(*,*) 'Deleted '//Section(pos)%ClassId
                      Call delete_section_from_blocks(pos, NumSections, Section, NumBlocks, Block)
                    end do
            else ! remove this section only
                    !write(*,*) 'Removing '//tClassId
                    Call delete_section_from_blocks(sect, NumSections, Section, NumBlocks, Block)
            end if
    end if
    call xml_write_blocks(pathToSOURCE, NumBlocks, Block,  Section, 0)
    call xml_write_blocks(pathToUPDATES, NumBlocks, Block,  Section, dept)
    call xml_write_sections(pathToSOURCE, NumSections, Section, 0)
    call xml_write_sections(pathToUPDATES, NumSections, Section, dept)
    
    call offerings_summarize(NumSections, Section, Offering)
    call section_list_all (device, NumSections, Section, Offering, NumBlocks, Block,  &
      fnOFFSET+fnScheduleOfClasses, dept, trim(mesg))

    return
  end subroutine section_delete

 
  subroutine section_edit(device, NumSections, Section, NumBlocks, Block)
    integer, intent (in) :: device
    integer, intent (in) :: NumBlocks
    type (TYPE_BLOCK), dimension(0:), intent(in) :: Block
    integer, intent (in out) :: NumSections
    type (TYPE_SECTION), intent(in out), dimension (0:) :: Section
    character(len=MAX_LEN_CLASS_ID) :: tClassId
    integer :: sect
    character (len=127) :: mesg

    ! get index to section
    call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, sect)
    if (sect/=0 .or. tClassId==SPACE) then
          mesg = 'Section to edit not specified?'
    else
          sect = index_to_section(tClassId, NumSections, Section)
          mesg = 'Section "'//tClassId//'" not found?'
    end if
    if (sect<=0) then ! section is invalid
          targetCollege = CollegeIdxUser
          targetDepartment = DeptIdxUser
          call html_write_header(device, 'Edit section', '<br><hr>'//mesg)
          return
    end if

    targetDepartment = Section(sect)%DeptIdx
    targetCollege = Department(targetDepartment)%CollegeIdx
    call html_write_header(device, 'Edit section '//tClassId)

    ! go to validation
    call section_validation_form(device, NumSections, Section, NumBlocks, Block,  &
        1, sect, Section(sect), targetDepartment, targetDepartment)

    return
  end subroutine section_edit
 

  subroutine section_write_edit_form(device, NumSections, Section, NumBlocks, Block, &
      sect, tSection, teacher_dept, room_dept)
    integer, intent (in) :: device, sect, teacher_dept, room_dept
    integer, intent (in) :: NumSections
    type (TYPE_SECTION), intent(in), dimension (0:) :: Section
    integer, intent (in) :: NumBlocks
    type (TYPE_BLOCK), dimension(0:), intent(in) :: Block
    type (TYPE_SECTION), intent (in) :: tSection
    integer :: rdx, tdx, idx, tLen! ddx, sdx
    integer :: idx_meet, idx_select, idx_ampm
    integer :: DayIdx, bTimeIdx, eTimeIdx, RoomIdx, TeacherIdx
    !integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
    character(len=32) :: tHours

    tLen = len_trim(Section(sect)%Code)+1
    if (is_lecture_class(sect,Section)) then
            tHours = trim(ftoa(Subject(Section(sect)%SubjectIdx)%LectHours,2))//' lecture hours'
    else
            tHours = trim(ftoa(Subject(Section(sect)%SubjectIdx)%LabHours,2))//' laboratory hours'
    end if
    ! write input form to capture edits to Section(sect); previous inputs are in tSection
    write(device,AFORMAT) &
        '<form name="input" method="post" action="'//CGI_SCRIPT//'">', &
        '<input type="hidden" name="F" value="'//trim(itoa(fnOFFSET+fnScheduleValidate))//'">'// &
        '<input type="hidden" name="A1" value="'//trim(Section(sect)%ClassId)//'">'

    !write(*,*) 'section_write_edit_form()'
    !write(*,*) 'ORIGINAL: Code=', Section(sect)%Code, ', slots=', Section(sect)%Slots, &
    !  ', NMeets=', Section(sect)%NMeets
    !write(*,*) (txtDay(Section(sect)%DayIdx(idx)), text_time_period(Section(sect)%bTimeIdx(idx), Section(sect)%eTimeIdx(idx)), &
    !  Room(Section(sect)%RoomIdx(idx))%Code, Teacher(Section(sect)%TeacherIdx(idx))%TeacherID, idx=1,Section(sect)%NMeets)
    !write(*,*) 'PROPOSED: Code=', tSection%Code, ', slots=', tSection%Slots, ', NMeets=', tSection%NMeets
    !write(*,*) (txtDay(tSection%DayIdx(idx)), text_time_period(tSection%bTimeIdx(idx), tSection%eTimeIdx(idx)), &
    !  Room(tSection%RoomIdx(idx))%Code, Teacher(tSection%TeacherIdx(idx))%TeacherID, idx=1,tSection%NMeets)
    write(device,AFORMAT) &
        '<b>SECTION CODE</b> '//trim(Section(sect)%Code)// &
        nbsp//nbsp//'<i>change to </i>&nbsp<input size="'//trim(itoa(tLen))//'" name="code" value="'// &
        trim(tSection%Code)//'">', &
        nbsp//nbsp//nbsp//nbsp//nbsp//nbsp//'<b>NO. OF STUDENTS</b> '//trim(itoa(Section(sect)%Slots))// &
        nbsp//nbsp//'<i>change to </i>&nbsp<input size="3" name="slots" value="'//trim(itoa(tSection%Slots))//'">', &
        '<br><i>(Note: Class meetings must total <b>'//trim(tHours)//'</b>) :</i>'

    write(device,AFORMAT) '<table border="0" width="100%">'//begintr, &
        '<td align="left"><b>Meeting</b>'//endtd//&
        '<td align="left"><b>Day</b>'//endtd// &
        '<td align="left"><b>Begin</b>'//endtd//&
        '<td align="left"><b>End</b>'//endtd// &
        '<td align="left"><b>Room</b>'//endtd//&
        '<td align="left"><b>Teacher</b>'//endtd//endtr
    do idx_meet=1,tSection%NMeets+3
      DayIdx = tSection%DayIdx(idx_meet)
      bTimeIdx = tSection%bTimeIdx(idx_meet)
      eTimeIdx = tSection%eTimeIdx(idx_meet)
      RoomIdx = tSection%RoomIdx(idx_meet)
      TeacherIdx = tSection%TeacherIdx(idx_meet)
      if (idx_meet<=tSection%NMeets) then
              write(device,AFORMAT) begintr//'<td align="center">'//trim(itoa(idx_meet))//endtd
      else
              write(device,AFORMAT) begintr//'<td align="center">(Add)'//endtd
      end if
      write(device,AFORMAT) begintd//'<select name="day'//trim(itoa(idx_meet))//'">'
      do idx=0,6
        if (idx/=DayIdx) then
                  idx_select = 0
        else
                  idx_select = 1
        end if
        write(device,AFORMAT) '<option value="'//trim(itoa(idx))//'"'// &
          trim(selected(idx_select))//'> '//txtDay(idx)
      end do
      write(device,AFORMAT) '</select>'//endtd//begintd//'<select name="btime'//trim(itoa(idx_meet))//'"><option value="0"> '
      do idx=1,53
        if (idx<21) then
                idx_ampm = 1
        elseif (idx>21) then
                idx_ampm = 2
        else
                idx_ampm = 0
        end if
        if (idx/=bTimeIdx) then
                  idx_select = 0
        else
                  idx_select = 1
        end if
        write(device,AFORMAT) '<option value="'//trim(itoa(idx))//'"'//trim(selected(idx_select))// &
          '> '//trim(txtTime(idx))//ampm(idx_ampm)
      end do
      write(device,AFORMAT) '</select>'//endtd//begintd//'<select name="etime'//trim(itoa(idx_meet))//'"><option value="0"> '
      do idx=5,57
        if (idx<21) then
                idx_ampm = 1
        elseif (idx>21) then
                idx_ampm = 2
        else
                idx_ampm = 0
        end if
        if (idx/=eTimeIdx) then
                  idx_select = 0
        else
                  idx_select = 1
        end if
        write(device,AFORMAT) '<option value="'//trim(itoa(idx))//'"'//trim(selected(idx_select))// &
          '> '//trim(txtTime(idx))//ampm(idx_ampm)
      end do
      write(device,AFORMAT) '</select>'//endtd//begintd//'<select name="room'//trim(itoa(idx_meet))//'"><option value="0"> '
      do rdx=1,NumRooms+NumAdditionalRooms
        if (rdx/=RoomIdx) then
                if ( (isRoleChair .and. Room(rdx)%DeptIdx==DeptIdxUser) .or. &
                     (isRoleAdmin .and. Room(rdx)%DeptIdx==room_dept) ) then
                        write(device,AFORMAT) '<option value="'//trim(Room(rdx)%Code)//'"> '//trim(Room(rdx)%Code)
                end if
        else
                write(device,AFORMAT) '<option value="'//trim(Room(rdx)%Code)//'" selected="selected"> '// &
                  trim(Room(rdx)%Code)
        end if
      end do
      write(device,AFORMAT) '</select>'//endtd//begintd//'<select name="teacher'//trim(itoa(idx_meet))//'"><option value="0"> '
      do tdx=1,NumTeachers+NumAdditionalTeachers
        idx = TeacherRank(tdx)
        if (idx/=TeacherIdx) then
                if ( (isRoleChair .and. Teacher(idx)%DeptIdx==DeptIdxUser) .or. &
                     (isRoleAdmin .and. Teacher(idx)%DeptIdx==teacher_dept) ) then
                        write(device,AFORMAT) '<option value="'//trim(Teacher(idx)%TeacherID)//'"> '//trim(Teacher(idx)%Name)
                end if
        else
                write(device,AFORMAT) '<option value="'//trim(Teacher(idx)%TeacherID)//'" selected="selected"> '// &
                  trim(Teacher(idx)%Name)
        end if
      end do
      write(device,AFORMAT) '</select>'//endtd//endtr

    end do
    write(device,AFORMAT) '</table>', &
      '<br><input type="submit" name="action" value="Validate"> above choices.', &
      '</form><hr>'
    return
  end subroutine section_write_edit_form


  subroutine section_build_from_query (NumSections, Section, section_index, tSection)
    integer, intent (in) :: NumSections
    type (TYPE_SECTION), intent(in), dimension (0:) :: Section
    integer, intent(in) :: section_index
    type (TYPE_SECTION), intent(out) :: tSection
    integer :: cgi_err, crse, idx, idx_meet, jdx
    character (len=3*MAX_LEN_ROOM_CODE) :: tRoom
    character(len=3*MAX_LEN_TEACHER_CODE) :: tLogin
    character(len=3*MAX_LEN_SECTION_CODE) :: tCode

    call initialize_section(tSection)
    crse = Section(section_index)%SubjectIdx
    tSection%DeptIdx = Section(section_index)%DeptIdx
    tSection%SubjectIdx = crse

    ! section code & class ID
    call cgi_get_named_string(QUERY_STRING, 'code', tCode, cgi_err)
    tSection%Code = tCode
    tSection%ClassId = trim(Subject(crse)%Name)//SPACE//tSection%Code

    ! no. of students
    call cgi_get_named_integer(QUERY_STRING, 'slots', tSection%Slots, cgi_err)

    ! class meetings
    idx_meet = 0
    do idx=1,MAX_SECTION_MEETINGS
      call cgi_get_named_integer(QUERY_STRING, 'day'//trim(itoa(idx)), jdx, cgi_err)
      tSection%DayIdx(idx_meet+1) = max(jdx,0)
      call cgi_get_named_integer(QUERY_STRING, 'btime'//trim(itoa(idx)), jdx, cgi_err)
      tSection%bTimeIdx(idx_meet+1) = max(jdx,0)
      call cgi_get_named_integer(QUERY_STRING, 'etime'//trim(itoa(idx)), jdx, cgi_err)
      tSection%eTimeIdx(idx_meet+1) = max(jdx,0)
      call cgi_get_named_string(QUERY_STRING, 'room'//trim(itoa(idx)), tRoom, cgi_err)
      if (cgi_err==0) tSection%RoomIdx(idx_meet+1) = index_to_room(tRoom)
      call cgi_get_named_string(QUERY_STRING, 'teacher'//trim(itoa(idx)), tLogin, cgi_err)
      if (cgi_err==0) tSection%TeacherIdx(idx_meet+1) = index_to_teacher(tLogin)
      idx_meet = idx_meet+1
    end do
    ! ignore trailing TBA days (DayIdx is 0) after the first one
    do while (idx_meet>1 .and. tSection%DayIdx(idx_meet)==0)
      tSection%bTimeIdx(idx_meet) = 0
      tSection%eTimeIdx(idx_meet) = 0
      tSection%RoomIdx(idx_meet) = 0
      tSection%TeacherIdx(idx_meet) = 0
      idx_meet = idx_meet-1
    end do
    tSection%NMeets = idx_meet
    !write(*,*) 'BuildSectionFromQuery(', section_index, ')'
    !write(*,*) '  subject=', crse, ', '//Subject(crse)%Name
    !write(*,*) '  code=', tSection%Code, ', classID=', tSection%ClassId
    !write(*,*) '  slots=', tSection%Slots
    !write(*,*) '  NMeets=', idx_meet
    !write(*,*) ('  '//txtDay(tSection%DayIdx(idx))//SPACE, &
    !  trim(text_time_period(tSection%bTimeIdx(idx), tSection%eTimeIdx(idx)))//SPACE, &
    !  trim(Room(tSection%RoomIdx(idx))%Code)//SPACE, &
    !  trim(Teacher(tSection%TeacherIdx(idx))%TeacherID), idx=1,idx_meet)

    return
  end subroutine section_build_from_query


  subroutine section_validate_inputs(device, NumSections, Section, Offering, NumBlocks, Block)
    integer, intent (in) :: device
    integer, intent (in out) :: NumSections
    type (TYPE_SECTION), intent(in out), dimension (0:) :: Section
    integer, intent (in) :: NumBlocks
    type (TYPE_BLOCK), dimension(0:), intent(in) :: Block
    type (TYPE_OFFERED_SUBJECTS), intent(in out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
    integer :: action_index, ierr, sect
    integer :: teacher_dept, room_dept, dept
    type (TYPE_SECTION) :: wrk
    character(len=MAX_LEN_CLASS_ID) :: tClassId, tAction
    character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
    character(len=127), dimension(MAX_SECTION_MEETINGS) :: mesg

    call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, sect)
    if (sect/=0 .or. tClassId==SPACE) then
            mesg(1) = 'Section to edit not specified?'
    else
            sect = index_to_section(tClassId, NumSections, Section)
            mesg(1) = 'Section "'//tClassId//'" not found?'
    end if
    if (sect<=0) then ! section is invalid
            targetCollege = CollegeIdxUser
            targetDepartment = DeptIdxUser
            call html_write_header(device, 'Edit section', '<br><hr>'//mesg(1))
            return
    end if

    dept = Section(sect)%DeptIdx
    targetDepartment = dept
    targetCollege = Department(dept)%CollegeIdx
    room_dept = 0
    teacher_dept = 0

    ! extract section info from QUERY
    call section_build_from_query (NumSections, Section, sect, wrk)

    ! action is ?
    call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
    select case (trim(tAction))
            case ('Confirm') ! Confirm previously validated edits
                    action_index = 2
                    Section(sect) = wrk
                    call xml_write_sections(pathToSOURCE, NumSections, Section, 0)
                    call xml_write_sections(pathToUPDATES, NumSections, Section, dept)
                    
                    call offerings_summarize(NumSections, Section, Offering)
                    call section_list_all (device, NumSections, Section, Offering, NumBlocks, Block,  &
                      fnOFFSET+fnScheduleOfClasses, dept, 'Finished editing '//trim(wrk%ClassId))
                    return
            case ('Find rooms')
                    action_index = 3
                    call cgi_get_named_string(QUERY_STRING, 'room_dept', tDepartment, room_dept)
                    room_dept = index_to_dept(tDepartment)
            case ('Find teachers')
                    action_index = 4
                    call cgi_get_named_string(QUERY_STRING, 'teacher_dept', tDepartment, teacher_dept)
                    teacher_dept = index_to_dept(tDepartment)
            case default ! assume Validate
                    action_index = 1
    end select
    if (room_dept==0) room_dept = targetDepartment
    if (teacher_dept==0) teacher_dept = targetDepartment

    !write(*,*) 'section_offer_subject(', Section(sect)%ClassId, ')'
    !write(*,*) 'section_validate_inputs:  Action is ', tAction, ' room_dept=', room_dept, ' teacher_dept=', teacher_dept

    ! page heading
    call html_write_header(device, 'Proposed changes to section '//tClassId)
    call section_validation_form(device, NumSections, Section, NumBlocks, Block,  &
        action_index, sect, wrk, teacher_dept, room_dept)
    return
  end subroutine section_validate_inputs


  subroutine section_validation_form(device, NumSections, Section, NumBlocks, Block,  &
      action_index, sect, wrk, teacher_dept, room_dept)
    integer, intent (in out) :: NumSections
    type (TYPE_SECTION), intent(in out), dimension (0:) :: Section
    integer, intent (in) :: device, action_index, sect, teacher_dept, room_dept
    integer, intent (in) :: NumBlocks
    type (TYPE_BLOCK), dimension(0:), intent(in) :: Block
    type (TYPE_SECTION), intent(in) :: wrk
    integer :: ierr, crse, ddx, idx, jdx, mdx, rdx, sdx, tdx, idx_meet, idx_select, tLen
    integer, dimension(60,6) :: TimeTable
    logical :: conflict_teacher, conflict_room, flagIsUp

    !write(*,*) 'section_validation_form:  Action is ', action_index, ' room_dept=', room_dept, ' teacher_dept=', teacher_dept

    call section_write_edit_form(device, NumSections, Section, NumBlocks, Block, &
        sect, wrk, teacher_dept, room_dept)
    crse = Section(sect)%SubjectIdx
    ierr = 0

    ! section code & class ID
    if (wrk%Code==SPACE) then ! code NOT specified
            ierr = ierr+1
            write(device,AFORMAT) red//'  Section code not specified?'//black//'<br>'
    else ! code specified
            idx = index_to_section(wrk%ClassId, NumSections, Section)
            if (idx>0 .and. idx/=sect) then
                    ierr = ierr+1
                    write(device,AFORMAT) red//'  Class ID '//wrk%ClassId//' already in use?'//black//'<br>'
            end if
    end if
    ! no. of students
    if (wrk%Slots<=0) then
            ierr = ierr+1
            write(device,AFORMAT) red//'  Number of students must be more than 0?'//black//'<br>'
    end if

    ! correct no. of hours?
    if (.not. is_consistent_section_hours_with_subject_defn(wrk)) then
            ierr = ierr+1
            write(device,AFORMAT) red//'Total meeting hours is inconsistent with subject definition hours?'//black//'<br>'
    end if

    ! meeting conflicts?
    if (.not. is_conflict_free_section_hours(wrk, NumSections, Section)) then
            ierr = ierr+1
            write(device,AFORMAT) red//'Conflict in meeting times; or, if a lecture-lab subject, '// &
              'conflict with lecture section?'//black//'<br>'
    end if

    ! check room conflict for each meeting
    do idx_meet=1,Section(sect)%NMeets
      rdx = wrk%RoomIdx(idx_meet)
      if (rdx==0 .or. Room(rdx)%Code=='TBA') cycle ! none assigned yet
      call timetable_meetings_in_room(NumSections, Section, rdx, sect, tLen, tArray, TimeTable, conflict_room)
      if (conflict_room) then
        ierr = ierr+1
        write(device,AFORMAT) red//'  Prior conflict of classes in room '//trim(Room(rdx)%Code)//black//'<br>'
      else
        if (is_conflict_timetable_with_struct_section(wrk, idx_meet, idx_meet, TimeTable)) then
          conflict_room = .true.
          ierr = ierr+1
          write(device,AFORMAT) red//'  Meeting '//itoa(idx_meet)//' conflicts with classes in room '// &
            trim(Room(rdx)%Code)//black//'<br>'
        end if
      end if
      !if (isRoleChair .and. Room(rdx)%DeptIdx/=DeptIdxUser) then
      !    write(device,AFORMAT) red//'  '//trim(Department(DeptIdxUser)%Code)//' cannot assign classes to room '// &
      !      trim(Room(rdx)%Code)//'@'//trim(Department(Room(rdx)%DeptIdx)%Code)//black//'<br>'
      !    ierr = ierr+1
      !end if
    end do

    ! check if room capacity exceeded
    do idx_meet=1,Section(sect)%NMeets
      rdx = wrk%RoomIdx(idx_meet)
      if (rdx==0 .or. Room(rdx)%Code=='TBA') cycle ! none assigned yet
      if (Room(rdx)%MaxCapacity<wrk%Slots) then
        ierr = ierr+1
        write(device,AFORMAT) red//'  Class size exceeds capacity of room '//trim(Room(rdx)%Code)//black//'<br>'
      end if
    end do

    ! check teacher conflict for each meeting
    do idx_meet=1,wrk%NMeets
      tdx = wrk%TeacherIdx(idx_meet)
      if (tdx==0 .or. Teacher(tdx)%TeacherId=='TBA)') cycle
      call timetable_meetings_of_teacher(NumSections, Section, tdx, sect, tLen, tArray, TimeTable, conflict_teacher)
      if (conflict_teacher) then ! teacher has conflicted schedule
        ierr = ierr+1
        write(device,AFORMAT) red//'  Prior conflict in teaching schedule of '//trim(Teacher(tdx)%Name)//black//'<br>'
      else
        if (is_conflict_timetable_with_struct_section(wrk,idx_meet,idx_meet,TimeTable)) then
          conflict_teacher = .true.
          ierr = ierr+1
          write(device,AFORMAT) red//'  Meeting '//itoa(idx_meet)//' conflicts with teaching schedule of '// &
            trim(Teacher(tdx)%Name)//black//'<br>'
        end if
      end if
      !if (isRoleChair .and. Teacher(tdx)%DeptIdx/=DeptIdxUser) then
      !    write(device,AFORMAT) red//'  '//trim(Department(DeptIdxUser)%Code)//' cannot assign classes to '// &
      !      trim(Teacher(tdx)%Name)//' of '//trim(Department(Teacher(tdx)%DeptIdx)%Code)//black//'<br>'
      !    ierr = ierr+1
      !end if
    end do

    ! common form inputs
    write(device,AFORMAT) &
        '<form name="input" method="post" action="'//CGI_SCRIPT//'">', &
        '<input type="hidden" name="F" value="'//trim(itoa(fnOFFSET+fnScheduleValidate))//'">', &
        '<input type="hidden" name="A1" value="'//trim(Section(sect)%ClassId)//'">', &
        '<input type="hidden" name="code" value="'//trim(wrk%Code)//'">'// &
        '<input type="hidden" name="slots" value="'//trim(itoa(wrk%Slots))//'">'
    do idx=1,wrk%NMeets
      write(device,AFORMAT) &
        '<input type="hidden" name="day'//trim(itoa(idx))//'" value="'//trim(itoa(wrk%DayIdx(idx)))//'">', &
        '<input type="hidden" name="btime'//trim(itoa(idx))//'" value="'//trim(itoa(wrk%bTimeIdx(idx)))//'">', &
        '<input type="hidden" name="etime'//trim(itoa(idx))//'" value="'//trim(itoa(wrk%eTimeIdx(idx)))//'">', &
        '<input type="hidden" name="room'//trim(itoa(idx))//'" value="'//trim(Room(wrk%RoomIdx(idx))%Code)//'">', &
        '<input type="hidden" name="teacher'//trim(itoa(idx))//'" value="'//trim(Teacher(wrk%teacherIdx(idx))%TeacherID)//'">'
    end do

    if (ierr==0) then ! nothing wrong?
        write(device,AFORMAT) &
          '<input type="submit" name="action" value="Confirm"> changes; else, specify additional changes, then Validate.'
    end if
    write(device,AFORMAT) '<hr>'

    if (conflict_room .or. action_index==3) then ! tAction=='Find rooms') then
            call room_search_given_time(device, NumSections, Section, wrk, sect, jdx, room_dept)
    end if

    write(device,AFORMAT) 'Search for rooms in: <select name="room_dept"><option value="0"> '
    do ddx=2,NumDepartments
      if (ddx/=room_dept) then
                idx_select = 0
      else
                idx_select = 1
      end if
      write(device,AFORMAT) '<option value="'//trim(Department(ddx)%Code)//'"'//trim(selected(idx_select))//'> '// &
        trim(Department(ddx)%Code)//dash//trim(Department(ddx)%Name)
    end do
    write(device,AFORMAT) '</select>'//nbsp//'<input type="submit" name="action" value="Find rooms"><hr>'

    if (conflict_teacher .or. action_index==4) then ! tAction=='Find teachers') then
            call teacher_search_given_time(device, NumSections, Section, wrk, sect, idx, teacher_dept)
    end if

    write(device,AFORMAT) 'Search for teachers in: <select name="teacher_dept"><option value="0"> '
    do ddx=2,NumDepartments
      if (ddx/=teacher_dept) then
                idx_select = 0
      else
                idx_select = 1
      end if
      write(device,AFORMAT) '<option value="'//trim(Department(ddx)%Code)//'"'//trim(selected(idx_select))//'> '// &
        trim(Department(ddx)%Code)//dash//trim(Department(ddx)%Name)
    end do
    write(device,AFORMAT) '</select>'//nbsp//'<input type="submit" name="action" value="Find teachers"><hr>'

    ! show other sections, if any
    write(device,AFORMAT) &
      '<a name="sections"></a><table border="0" width="100%">'//begintr, &
      tdnbspendtd//'<td align="right">', &
      !'[ Other '//trim(Subject(crse)%Name)//' <a href="#sections">sections</a> ] ', &
      '[ Usage of <a href="#rooms">room</a>(s) ] ', &
      '[ Classes of <a href="#teachers">teacher</a>(s) ] ', &
      '[ Other sections in <a href="#block">block</a> ] ', &
      '[ <a href="#TOP">TOP</a> ] ', &
      endtd//endtr//'</table>'
    tLen = 0
    do sdx=1,NumSections
      if (crse/=Section(sdx)%SubjectIdx) cycle ! not the same subject
      if (sdx==sect) cycle ! exclude section being edited
      do idx=1,Section(sdx)%NMeets
        tArray(tLen+1) = sdx
        tArray(tLen+2) = idx
        tArray(tLen+3) = 0
        tLen = tLen+3
      end do
    end do
    tArray(tLen+1) = 0
    tArray(tLen+2) = 0
    tArray(tLen+3) = 0
    !write(*,*) tLen/3, ' other sections...', &
    !  (Section(tArray(3*(sdx-1)+1))%ClassId, sdx=1,tLen/3)
    call list_sections_to_edit(device, Section, tLen, tArray, 0, SPACE, SPACE, .false., '<b>Other sections</b>')
    write(device,AFORMAT) '<hr>'

    ! show classes in the same rooms, if any
    flagIsUp = .false. ! none
    write(device,AFORMAT) &
      '<a name="rooms"></a><table border="0" width="100%">'//begintr, &
      tdnbspendtd//tdalignright, &
      '[ Other '//trim(Subject(crse)%Name)//' <a href="#sections">sections</a> ] ', &
      !'[ Usage of <a href="#rooms">room</a>(s) ] ', &
      '[ Classes of <a href="#teachers">teacher</a>(s) ] ', &
      '[ Other sections in <a href="#block">block</a> ] ', &
      '[ <a href="#TOP">TOP</a> ] ', &
      endtd//endtr//'</table>'
    do idx_meet=1,Section(sect)%NMeets
      rdx = wrk%RoomIdx(idx_meet)
      if (rdx/=0) then
        tLen = 0
        do idx=1,idx_meet-1 ! check if encountered previously
          if (rdx==wrk%RoomIdx(idx)) tLen = tLen + 1
        end do
        if (tLen==0) then ! not yet encountered
            flagIsUp = .true.
            ! collect classes in room rdx
            call timetable_meetings_in_room(NumSections, Section, rdx, sect, tLen, tArray, TimeTable, conflict_room)
            call list_sections_to_edit(device, Section, tLen, tArray, 0, SPACE, SPACE, .false., &
              '<b>Meetings in '//trim(Room(rdx)%Code)//'</b>')

            ! add section meetings
            do mdx=1,wrk%NMeets
              ddx = wrk%DayIdx(mdx)
              if (ddx==0) cycle
              if (wrk%RoomIdx(mdx)==rdx) then
                      do jdx = wrk%bTimeIdx(mdx), wrk%eTimeIdx(mdx)-1
                        TimeTable(jdx,ddx) = -1
                      end do
              end if
            end do
            call timetable_display(device, Section, TimeTable)
            write(device,AFORMAT) '<hr>'
        end if
      end if
    end do
    if (.not. flagIsUp) then
      write(device,AFORMAT) '<b>Other class meetings in room(s)</b><br>(None)<hr>'
    end if

    ! show classes of the teachers, if any
    flagIsUp = .false. ! none
    write(device,AFORMAT) &
      '<a name="teachers"></a><table border="0" width="100%">'//begintr, &
      tdnbspendtd//tdalignright, &
      '[ Other '//trim(Subject(crse)%Name)//' <a href="#sections">sections</a> ] ', &
      '[ Usage of <a href="#rooms">room</a>(s) ] ', &
      !'[ Classes of <a href="#teachers">teacher</a>(s) ] ', &
      '[ Other sections in <a href="#block">block</a> ] ', &
      '[ <a href="#TOP">TOP</a> ] ', &
      endtd//endtr//'</table>'
    do idx_meet=1,Section(sect)%NMeets
      tdx = wrk%TeacherIdx(idx_meet)
      if (tdx/=0) then
        tLen = 0
        do idx=1,idx_meet-1 ! check if encountered previously
          if (tdx==wrk%TeacherIdx(idx)) tLen = tLen + 1
        end do
        if (tLen==0) then ! not yet encountered
            flagIsUp = .true.
            ! collect classes of teacher
            call timetable_meetings_of_teacher(NumSections, Section, tdx, sect, tLen, tArray, TimeTable, conflict_teacher)
            !write(*,*) tLen/3, ' other class meetings of teacher '//Teacher(tdx)%Name
            call list_sections_to_edit(device, Section, tLen, tArray, 0, SPACE, SPACE, .false., &
              '<b>Class meetings of '//trim(Teacher(tdx)%Name)//'</b>')
            ! add section meetings
            do mdx=1,wrk%NMeets
              ddx = wrk%DayIdx(mdx)
              if (ddx==0) cycle
              if (wrk%TeacherIdx(mdx)==tdx) then
                      do jdx = wrk%bTimeIdx(mdx), wrk%eTimeIdx(mdx)-1
                        TimeTable(jdx,ddx) = -1
                      end do
              end if
            end do
            call timetable_display(device, Section, TimeTable)
            write(device,AFORMAT) '<hr>'
        end if
      end if
    end do
    if (.not. flagIsUp) then
      write(device,AFORMAT) '<b>Other classes of teacher(s)</b><br>(None)<hr>'
    end if

    ! show blocks where section is used
    write(device,AFORMAT) &
      '<a name="block"></a><table border="0" width="100%">'//begintr, &
      tdnbspendtd//tdalignright, &
      '[ Other '//trim(Subject(crse)%Name)//' <a href="#sections">sections</a> ] ', &
      '[ Usage of <a href="#rooms">room</a>(s) ] ', &
      '[ Classes of <a href="#teachers">teacher</a>(s) ] ', &
      !'[ Other sections in <a href="#block">block</a> ] ', &
      '[ <a href="#TOP">TOP</a> ] ', &
      endtd//endtr//'</table>'

!    do targetBlock=1,NumBlocks
!
!      flagIsUp = .false.
!      do idx=1,Block(targetBlock)%NumClasses
!        if (Block(targetBlock)%Section(idx)/=sect) cycle
!        flagIsUp = .true.
!        exit
!      end do
!      if (flagIsUp) then ! section is used in Block(targetBlock)
!            ! collect meetings of block targetBlock
!            tLen = 0
!            call timetable_meetings_of_block(NumSections, Section, targetBlock, Block, 0, tLen, tArray, TimeTable, flagIsUp)
!            call list_sections_to_edit(device, Section, tLen, tArray, 0, SPACE, SPACE, .false., &
!              '<br><b>Blocked schedule '//trim(Block(targetBlock)%BlockID)//'</b>')
!            ! add section meetings
!            do mdx=1,wrk%NMeets
!              ddx = wrk%DayIdx(mdx)
!              if (ddx==0) cycle
!              do jdx = wrk%bTimeIdx(mdx), wrk%eTimeIdx(mdx)-1
!                TimeTable(jdx,ddx) = -1
!              end do
!            end do
!            call timetable_display(device, Section, TimeTable)
!            write(device,AFORMAT) '<hr>'
!      end if
!    end do

    write(device,AFORMAT) '</form>'

    return
  end subroutine section_validation_form


end module EditSECTIONS
