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


module DisplaySECTIONS

    use HTML

    implicit none


contains


    subroutine teacher_search_given_time(device, NumSections, Section, wrk, to_skip, teacher_count, given_teacher_dept)
        integer, intent (in) :: to_skip, device
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
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
            txtTime(wrk%bTimeIdx(i))//DASH//trim(txtTime(wrk%eTimeIdx(i)))//ch
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
            write(device,AFORMAT) trim(make_href(fnTeacherEditSchedule, Teacher(tdx)%Name, &
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

    end subroutine teacher_search_given_time


    subroutine room_search_given_time(device, NumSections, Section, wrk, to_skip, room_count, given_room_dept)
        integer, intent (in) :: to_skip, device
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
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
                txtTime(wrk%bTimeIdx(i))//DASH//trim(txtTime(wrk%eTimeIdx(i)))//ch
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
            QUERY_put = Room(rdx)%Code
            write(device,AFORMAT) trim(make_href(fnRoomSchedule, Room(rdx)%Code, &
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

    end subroutine room_search_given_time


    subroutine section_list_all (device, NumSections, Section, Offering, NumBlocks, Block,  fn, givenDept, mesg)
        ! write list of sections in department 'dept' to unit 'device'
        integer, intent (in) :: device, fn
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        integer, intent (in) :: NumBlocks
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        type (TYPE_OFFERED_SUBJECTS), intent(in out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        integer, intent(in), optional :: givenDept
        character(len=*), intent(in), optional :: mesg

        integer :: cdx, idx, jdx, i, rdx, sdx, tdx, ncol, nclosed, maxcol=7, nopen, nsections
        integer :: owner_dept, owner_coll, previous
        character(len=127) :: header
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject, tSeats, searchString
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        logical :: isLecture, okToAdd, conflicted, isUserTeacher
        integer, dimension(60,6) :: TimeTable

        targetDepartment = DeptIdxUser
        targetCollege = CollegeIdxUser
        call recalculate_available_seats(Section)
        select case(fn)

            case (fnScheduleOfClasses)
                if (present(givenDept)) then
                    targetDepartment = givenDept
                    i = 0
                else
                    call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, i)
                    targetDepartment = index_to_dept(tDepartment)
                end if
                targetCollege = Department(targetDepartment)%CollegeIdx
                header = 'Classes in '//Department(targetDepartment)%Code

            case (fnScheduleByArea) ! sections by subject area
                header = 'Classes in subject area'
                call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, i)
                targetCollege = index_to_college(tCollege)
                call cgi_get_named_string(QUERY_STRING, 'A2', searchString, i)
                header = trim(header)//' "'//trim(searchString)//'"'
#if defined UPLB
                ! department already set above
#else
                ! Subjects administered by program
                tDepartment = tCollege
                targetDepartment = index_to_dept(tDepartment)
#endif

            case (fnTBARooms, fnTBATeachers) ! TBA rooms, teachers
                header = 'Classes with TBA room/teacher'
                call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, i)
                targetCollege = index_to_college(tCollege)
                header = tCollege//header
#if defined UPLB
                ! department already set above
#else
                ! Subjects administered by program
                tDepartment = tCollege
                targetDepartment = index_to_dept(tDepartment)
#endif

            case (fnTeacherClasses)
                call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, i)
                targetTeacher = index_to_teacher(tTeacher)
                targetDepartment = Teacher(targetTeacher)%DeptIdx
                targetCollege = Department(targetDepartment)%CollegeIdx
                header = 'Classes of '//Teacher(targetTeacher)%Name

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

                case (fnScheduleOfClasses)
                    okToAdd = owner_dept==targetDepartment

                case (fnScheduleByArea) ! sections by subject area
                    okToAdd = index(Section(sdx)%ClassId, trim(searchString)//SPACE)==1

                case (fnTBARooms) ! sections with TBA rooms
                    if (owner_coll==targetCollege) then
                        i = 0
                        do ncol=1,Section(sdx)%NMeets
                            if (Section(sdx)%RoomIdx(ncol)==0) i = i+1
                        end do
                        okToAdd = i>0
                    end if

                case (fnTBATeachers) ! sections with TBA teachers
                    if (owner_coll==targetCollege) then
                        i = 0
                        do ncol=1,Section(sdx)%NMeets
                            if (Section(sdx)%TeacherIdx(ncol)==0) i = i+1
                        end do
                        okToAdd = i>0
                    end if

                case (fnTeacherClasses)
                        i = 0
                        do ncol=1,Section(sdx)%NMeets
                            if (Section(sdx)%TeacherIdx(ncol)==targetTeacher) i = i+1
                        end do
                        okToAdd = i>0

            end select

            if (okToAdd) then
                nsections = nsections + 1
                tArray(nsections) = sdx
            end if

        end do ! sdx=1,NumSections
        if (nsections==0) then
            if (fn==fnTeacherClasses) then
                write(device,AFORMAT) trim(make_href(fnTeacherEditSchedule, 'Add', &
                    A1=tTeacher, pre='<br>( ', post=' )<hr>'))
            else
                write(device,AFORMAT) '<br>( None )<hr>'
            end if
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

        if (fn/=fnTeacherClasses) then
            ! write shortcut to subjects
            write(device,AFORMAT) '<table border="0" width="100%">'
            ncol = 0
            do idx=1,nopen
                tSubject = Subject(tArray(nsections+idx))%Name
                i = index(tSubject, DASH)
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
        end if


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

        if (fn/=fnTeacherClasses) then
            ! offer to open sections
            if (fnAvailable(fnScheduleOfferSubject) .and. &
                    nclosed>0 .and. (isRoleAdmin .or. (isRoleChair .and. DeptIdxUser==targetDepartment))) then
                call make_form_start(device, fnScheduleOfferSubject, A2=Department(targetDepartment)%Code)
                write(device,AFORMAT) &
                    '<table border="0" width="100%">'//begintr//'<td colspan="'//trim(itoa(maxcol))//'" align="right">', &
                    'Open a section in <select name="A1"> <option value=""> (select subject)'
                do idx=1,nclosed
                    tSubject = Subject(tArray(nsections+nopen+idx))%Name
                    write(device,AFORMAT) '<option value="'//trim(tSubject)//'"> '//tSubject
                end do
                write(device,AFORMAT) '</select> '//nbsp//nbsp//' <input type="submit" value="Submit">'// &
                endtd//endtr//'</table></form>'
            end if
            write(device,AFORMAT) '<hr><br>'
        end if

        write(device,AFORMAT) &
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
            QUERY_put = Subject(cdx)%Name
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
            if (fn/=fnTeacherClasses .and. isRoleAdmin) then
                write(device,AFORMAT) trim(make_href(fnEditSubject, 'Edit', A1=Subject(cdx)%Name, &
                    pre=nbsp//'<small>[ ', post=' ]</small>', alt=SPACE))
            end if
            write(device,AFORMAT) endtd//endtr, &
                begintr//tdnbspendtd//'<td colspan="7">'//nbsp//'Pr. '//trim(text_prerequisite_of_subject(cdx,0))//endtd

#if defined UPLB
            okToAdd = isRoleAdmin .or. (isRoleChair .and. DeptIdxUser==Subject(cdx)%DeptIdx)
#else
            ! Subjects administered by program
            okToAdd = isRoleAdmin .or. (isRoleChair .and. DeptIdxUser==targetDepartment)
#endif

            if (fn/=fnTeacherClasses .and. okToAdd) then
                write(device,AFORMAT) begintd//'<small>', &
                    trim(make_href(fnScheduleOfferSubject, 'Add '//tDepartment, &
                    A1=QUERY_put, A2=Department(targetDepartment)%Code, &
                    post='</small>'//endtd//endtr))
            else
                write(device,AFORMAT) tdnbspendtd//endtr
            end if

            ! sections
            do jdx=1,nsections
                sdx = tArray(jdx)
                if (Section(sdx)%SubjectIdx/=cdx) cycle

                ! does USER teach the section?
                isUserTeacher = .false.
                do i=1,Section(sdx)%NMeets
                    if (Section(sdx)%TeacherIdx(i)==requestingTeacher) isUserTeacher = .true.
                end do

                owner_dept = Section(sdx)%DeptIdx
                owner_coll = Department(owner_dept)%CollegeIdx

                isLecture = is_lecture_lab_subject(cdx) .and. is_lecture_class(sdx, Section) ! empty SPACE
                if (isLecture) then
                    write(device,AFORMAT) begintr//'<td colspan="9">'//nbsp//endtd//endtr
                    tSeats = itoa(Section(sdx)%Slots)
                else
                    tSeats = trim(itoa(Section(sdx)%Slots))//FSLASH//trim(itoa(Section(sdx)%RemSlots))
                end if
                QUERY_put = Section(sdx)%ClassId
                ! subject
                write(device,AFORMAT) begintr//tdnbspendtd

                ! section code, link to gradesheet entry form
                if ( (isRoleAdmin .or. (isRoleChair .and. DeptIdxUser==owner_dept) .or. &
                     isUserTeacher) ) then
                    write(device,AFORMAT) trim(make_href(fnGradeSheet, trim(Section(sdx)%Code), &
                        A1=QUERY_PUT, pre=begintd, post=endtd))
                else
                    write(device,AFORMAT) begintd//trim(Section(sdx)%Code)//endtd
                end if

                ! blocks
                write(device,AFORMAT) begintd
                call blocks_in_section(device, sdx, fnBlockSchedule, NumBlocks, Block)
                write(device,AFORMAT) endtd

                ! seats, link to classlist
                write(device,AFORMAT) trim(make_href(fnClassList, tSeats, &
                    A1=QUERY_PUT, pre=begintd, post=endtd))

                ! time, day, room, teacher
                if (is_regular_schedule(sdx, Section)) then
                    tdx = Section(sdx)%TeacherIdx(1)
                    rdx = Section(sdx)%RoomIdx(1)
                    write(device,AFORMAT) &
                        begintd//trim(text_days_of_section(sdx, NumSections, Section))//endtd// &
                        begintd//trim(text_time_period(Section(sdx)%bTimeIdx(1), Section(sdx)%eTimeIdx(1)))//endtd
                    if (rdx/=0) then
                        if (fn==fnRoomSchedule) then
                            write(device,AFORMAT) begintd//trim(Room(rdx)%Code)//endtd
                        else
                            write(device,AFORMAT) trim(make_href(fnRoomSchedule, Room(rdx)%Code, &
                                A1=Room(rdx)%Code, pre=begintd, post=endtd))
                        end if
                    else
                        write(device,AFORMAT) begintd//red//trim(Room(rdx)%Code)//black//endtd
                    end if
                    if (tdx/=0) then
                        if (fn==fnTeacherClasses) then
                            write(device,AFORMAT) begintd//trim(Teacher(tdx)%Name)//endtd
                        else
                            write(device,AFORMAT) trim(make_href(fnTeacherClasses, Teacher(tdx)%Name, &
                                A1=Teacher(tdx)%TeacherID, pre=begintd, post=endtd))
                        end if
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
                            if (fn==fnRoomSchedule) then
                                write(device,AFORMAT) begintd//trim(Room(rdx)%Code)//endtd
                            else
                                write(device,AFORMAT) trim(make_href(fnRoomSchedule, Room(rdx)%Code, &
                                    A1=Room(rdx)%Code, pre=begintd))
                            end if
                        else
                            write(device,AFORMAT) begintd//red//trim(Room(rdx)%Code)//black//endtd
                        end if
                        if (tdx/=0) then
                            if (fn==fnTeacherClasses) then
                                write(device,AFORMAT) begintd//trim(Teacher(tdx)%Name)
                            else
                                write(device,AFORMAT) trim(make_href(fnTeacherClasses, Teacher(tdx)%Name, &
                                    A1=Teacher(tdx)%TeacherID, pre=begintd))
                            end if
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
                    write(device,AFORMAT) trim(make_href(fnScheduleEdit, ' Edit', &
                        A1=QUERY_put))
                    if (fn/=fnTeacherClasses) then
                        write(device,AFORMAT) trim(make_href(fnScheduleDelete, ' Del', &
                            A1=QUERY_put))
                        if (isLecture) &
                            write(device,AFORMAT) trim(make_href(fnScheduleAddLab, 'Add lab', &
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
        write(device,AFORMAT) '</table><br>'
        if (fn==fnTeacherClasses) then
            call timetable_meetings_of_teacher(NumSections, Section, targetTeacher, 0, nsections, tArray, TimeTable, conflicted)
            if (nsections>0) call timetable_display(device, Section, TimeTable)
        end if
        write(device,AFORMAT) '<hr>'

    end subroutine section_list_all


end module DisplaySECTIONS
