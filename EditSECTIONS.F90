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


module EditSECTIONS

    use DisplaySECTIONS

    implicit none


contains


    subroutine section_offer_subject(device, NumSections, Section, Offering, NumBlocks, Block)
        integer, intent (in) :: device, NumBlocks
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_OFFERED_SUBJECTS), intent(in out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=MAX_LEN_SECTION_CODE) :: tSection
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer :: crse, kdx, dept, Term
        character (len=127) :: mesg

        call html_comment('section_offer_subject()')

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
        Term = targetTerm
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
            Section(NumSections) = TYPE_SECTION (trim(Subject(crse)%Name)//' '//tSection, tSection, &
                targetDepartment, crse, Subject(crse)%MaxLectSize, Subject(crse)%MaxLectSize, 1, 0, 0, 0, 0, 0)
        end if
        if (Subject(crse)%LabHours>0) then ! subject has lab/recitation
            NumSections = NumSections+1
            tSection = trim(tSection)//DASH//'1L'
            Section(NumSections) = TYPE_SECTION (trim(Subject(crse)%Name)//' '//tSection, tSection, &
            targetDepartment, crse, Subject(crse)%MaxLabSize, Subject(crse)%MaxLabSize, 1, 0, 0, 0, 0, 0)
        end if

        call xml_write_classes(pathToTerm, NumSections, Section, 0)
        call offerings_summarize(NumSections, Section, Offering)
        call section_list_all (device, NumSections, Section, Offering, NumBlocks, Block,  &
            fnScheduleOfClasses, dept, 'Opened a section in '//tSubject)

    end subroutine section_offer_subject


    subroutine section_add_laboratory(device, NumSections, Section, Offering, NumBlocks, Block)
        integer, intent (in) :: device, NumBlocks
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_OFFERED_SUBJECTS), intent(in out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        character(len=MAX_LEN_CLASS_ID) :: tClassId
        character(len=MAX_LEN_SECTION_CODE) :: tSection
        character (len=127) :: mesg
        integer :: crse, sect, dept

        call html_comment('section_add_laboratory()')

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

        tSection = get_next_lab_section(sect, NumSections, Section)
        NumSections = NumSections+1
        Section(NumSections) = TYPE_SECTION (trim(Subject(crse)%Name)//' '//tSection, tSection, & ! SPACE, &
            targetDepartment, crse, Subject(crse)%MaxLabSize, Subject(crse)%MaxLabSize, 1, 0, 0, 0, 0, 0)
        !write(*,*) 'Adding '//trim(Subject(crse)%Name)//' '//tSection

        call xml_write_classes(pathToTerm, NumSections, Section, 0)
        call offerings_summarize(NumSections, Section, Offering)
        call section_list_all (device, NumSections, Section, Offering, NumBlocks, Block,  &
            fnScheduleOfClasses, dept, 'Opened new section '//trim(Subject(crse)%Name)//' '//tSection)

    end subroutine section_add_laboratory


    subroutine section_delete(device, NumSections, Section, Offering, NumBlocks, Block)
        integer, intent (in) :: device, NumBlocks
        type (TYPE_BLOCK), intent(in out) :: Block(0:)
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_OFFERED_SUBJECTS), intent(in out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        character(len=MAX_LEN_CLASS_ID) :: tClassId
        integer :: sect, crse, pos, i, dept, Term
        character (len=127) :: mesg

        call html_comment('section_delete()')

        Term = targetTerm

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
                !write(*,*) 'Deleted '//tClassId
                Call delete_section_from_blocks(sect, NumSections, Section, NumBlocks, Block)
                tClassId = trim(tClassId)//DASH
                i = len_trim(tClassId)
                !write(*,*) 'Sections to remove: '//tClassId
                do pos=1,NumSections
                    if (Section(pos)%ClassId(:i)/=tClassId(:i)) cycle
                    !write(*,*) 'Deleted '//Section(pos)%ClassId
                    Call delete_section_from_blocks(pos, NumSections, Section, NumBlocks, Block)
                end do
            else ! remove this section only
                !write(*,*) 'Removing '//tClassId
                Call delete_section_from_blocks(sect, NumSections, Section, NumBlocks, Block)
            end if
        end if
        call xml_write_blocks(pathToTerm, NumBlocks, Block,  Section, 0)
        call xml_write_classes(pathToTerm, NumSections, Section, 0)
        call offerings_summarize(NumSections, Section, Offering)
        call count_sections_by_dept(Term, NumSections, Section)
        call section_list_all (device, NumSections, Section, Offering, NumBlocks, Block,  &
            fnScheduleOfClasses, dept, trim(mesg))

    end subroutine section_delete


!    subroutine section_edit(device, NumSections, Section, NumBlocks, Block)
!        integer, intent (in) :: device
!        integer, intent (in) :: NumBlocks
!        type (TYPE_BLOCK), intent(in) :: Block(0:)
!        integer, intent (in out) :: NumSections
!        type (TYPE_SECTION), intent(in out) :: Section(0:)
!        character(len=MAX_LEN_CLASS_ID) :: tClassId
!        integer :: sect
!
!        ! get index to section
!        call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, sect)
!        sect = index_to_section(tClassId, NumSections, Section)
!
!        targetDepartment = Section(sect)%DeptIdx
!        targetCollege = Department(targetDepartment)%CollegeIdx
!        call html_write_header(device, 'Edit section '//tClassId)
!
!        ! go to validation
!        call section_validation_form(device, NumSections, Section, NumBlocks, Block,  &
!            1, sect, Section(sect), targetDepartment, targetDepartment)
!
!    end subroutine section_edit


    subroutine section_validate_inputs(device, NumSections, Section, Offering, NumBlocks, Block)
        integer, intent (in) :: device
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        integer, intent (in) :: NumBlocks
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        type (TYPE_OFFERED_SUBJECTS), intent(in out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        integer :: action_index, ierr, sect, Term
        integer :: teacher_dept, room_dept, dept
        type (TYPE_SECTION) :: wrk
        character(len=MAX_LEN_CLASS_ID) :: tClassId, tAction
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character (len=127) :: mesg

        call html_comment('section_validate_inputs()')

        Term = targetTerm
        call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, sect)
        sect = index_to_section(tClassId, NumSections, Section)

        dept = Section(sect)%DeptIdx
        targetDepartment = dept
        targetCollege = Department(dept)%CollegeIdx
        room_dept = 0
        teacher_dept = 0

        if (REQUEST==fnScheduleEdit) then
            mesg = 'Edit section '//tClassId
            action_index = 1
            wrk = Section(sect)

        else ! (REQUEST==fnScheduleValidate) then
            mesg = 'Proposed changes to section '//tClassId

            ! extract section info from QUERY
            call section_build_from_query (NumSections, Section, sect, wrk)

            ! action is ?
            call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
            select case (trim(tAction))

                case ('Confirm') ! Accept previously validated edits
                    mesg = 'Finished editing '//trim(wrk%ClassId)
                    Section(sect) = wrk
                    call xml_write_classes(pathToTerm, NumSections, Section, 0)
                    call offerings_summarize(NumSections, Section, Offering)
                    call count_sections_by_dept(Term, NumSections, Section)
                    call section_list_all (device, NumSections, Section, Offering, NumBlocks, Block,  &
                        fnScheduleOfClasses, dept, mesg)
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

        end if

        if (room_dept<=0) room_dept = targetDepartment
        if (teacher_dept<=0) teacher_dept = targetDepartment

        ! page heading
        call html_write_header(device, mesg)
        call section_validation_form(device, NumSections, Section, NumBlocks, Block,  &
            action_index, sect, wrk, teacher_dept, room_dept)

    end subroutine section_validate_inputs


    subroutine section_write_edit_form(device, NumSections, Section, NumBlocks, Block, &
            sect, tSection, teacher_dept, room_dept)
        integer, intent (in) :: device, sect, teacher_dept, room_dept
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent (in) :: NumBlocks
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        type (TYPE_SECTION), intent (in) :: tSection
        integer :: rdx, tdx, idx, tLen! ddx, sdx
        integer :: idx_meet, idx_select, idx_ampm
        integer :: DayIdx, bTimeIdx, eTimeIdx, RoomIdx, TeacherIdx
        !integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        character(len=32) :: tHours

        call html_comment('section_write_edit_form()')

        tLen = len_trim(Section(sect)%Code)+1
        if (is_lecture_class(sect,Section)) then
            tHours = trim(ftoa(Subject(Section(sect)%SubjectIdx)%LectHours,2))//' lecture hours'
        else
            tHours = trim(ftoa(Subject(Section(sect)%SubjectIdx)%LabHours,2))//' laboratory hours'
        end if

        ! write input form to capture edits to Section(sect); previous inputs are in tSection
        call make_form_start(device, fnScheduleValidate, Section(sect)%ClassId)

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
            nbsp//nbsp//'<i>change to </i>'//nbsp//'<input size="'//trim(itoa(tLen))//'" name="code" value="'// &
            trim(tSection%Code)//'">', &
            nbsp//nbsp//nbsp//nbsp//nbsp//nbsp//'<b>NO. OF STUDENTS</b> '//trim(itoa(Section(sect)%Slots))// &
            nbsp//nbsp//'<i>change to </i>'//nbsp//'<input size="3" name="slots" value="'//trim(itoa(tSection%Slots))//'">'
        if (targetTerm/=3) write(device,AFORMAT) & ! not summer
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
            write(device,AFORMAT) '</select>'//endtd//begintd//'<select name="teacher'// &
                trim(itoa(idx_meet))//'"><option value="0"> '
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
            '<br><i>If you made a change above,</i> &nbsp; <input type="submit" name="action" value="Validate">', &
            '</form><hr>'

    end subroutine section_write_edit_form


    subroutine section_build_from_query (NumSections, Section, section_index, tSection)
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, intent(in) :: section_index
        type (TYPE_SECTION), intent(out) :: tSection
        integer :: cgi_err, crse, idx, idx_meet, jdx
        character (len=3*MAX_LEN_ROOM_CODE) :: tRoom
        character(len=3*MAX_LEN_TEACHER_CODE) :: tLogin
        character(len=3*MAX_LEN_SECTION_CODE) :: tCode

        call html_comment('section_build_from_query()')

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

    end subroutine section_build_from_query



    subroutine section_validation_form(device, NumSections, Section, NumBlocks, Block,  &
            action_index, section_index, wrk, teacher_dept, room_dept)
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        integer, intent (in) :: device, action_index, section_index, teacher_dept, room_dept
        integer, intent (in) :: NumBlocks
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        type (TYPE_SECTION), intent(in) :: wrk
        integer :: ierr, crse, ddx, idx, jdx, mdx, rdx, sdx, tdx, idx_meet, idx_select, tLen, idxWrk
        integer, dimension(60,6) :: TimeTable
        logical :: conflict_teacher, conflict_room, flagIsUp

        call html_comment('section_validation_form()', &
            ' action_index='//itoa(action_index), &
            ' room_dept='//itoa(room_dept), &
            ' teacher_dept='//itoa(teacher_dept))

        call section_write_edit_form(device, NumSections, Section, NumBlocks, Block, &
            section_index, wrk, teacher_dept, room_dept)
        crse = Section(section_index)%SubjectIdx
        ierr = 0

        ! section code & class ID
        if (wrk%Code==SPACE) then ! code NOT specified
            ierr = ierr+1
            write(device,AFORMAT) red//'  Section code not specified?'//black//'<br>'
        else ! code specified
            idx = index_to_section(wrk%ClassId, NumSections, Section)
            if (idx>0 .and. idx/=section_index) then
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
        do idx_meet=1,wrk%NMeets
            rdx = wrk%RoomIdx(idx_meet)
            if (rdx==0 .or. Room(rdx)%Code=='TBA') cycle ! none assigned yet
            call timetable_meetings_in_room(NumSections, Section, rdx, section_index, tLen, tArray, TimeTable, conflict_room)
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
        do idx_meet=1,wrk%NMeets
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
            call timetable_meetings_of_teacher(NumSections, Section, tdx, section_index, tLen, tArray, TimeTable, conflict_teacher)
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
        if (ierr>0) write(device,AFORMAT) '<hr>'

        ! common form inputs
        idxWrk = MAX_ALL_SECTIONS ! temporary work area
        Section(idxWrk) = wrk
        call make_form_start(device, fnScheduleValidate, Section(section_index)%ClassId)
        write(device,AFORMAT) &
            '<input type="hidden" name="code" value="'//trim(wrk%Code)//'">'// &
            '<input type="hidden" name="slots" value="'//trim(itoa(wrk%Slots))//'">'
        tLen = 0
        do idx=1,wrk%NMeets
            tArray(tLen+1) = idxWrk
            tArray(tLen+2) = idx
            tArray(tLen+3) = 0
            tLen = tLen+3
            write(device,AFORMAT) &
                '<input type="hidden" name="day'//trim(itoa(idx))//'" value="'//trim(itoa(wrk%DayIdx(idx)))//'">', &
                '<input type="hidden" name="btime'//trim(itoa(idx))//'" value="'//trim(itoa(wrk%bTimeIdx(idx)))//'">', &
                '<input type="hidden" name="etime'//trim(itoa(idx))//'" value="'//trim(itoa(wrk%eTimeIdx(idx)))//'">', &
                '<input type="hidden" name="room'//trim(itoa(idx))//'" value="'//trim(Room(wrk%RoomIdx(idx))%Code)//'">', &
            '<input type="hidden" name="teacher'//trim(itoa(idx))//'" value="'//trim(Teacher(wrk%teacherIdx(idx))%TeacherID)//'">'
        end do
        tArray(tLen+1) = 0
        tArray(tLen+2) = 0
        tArray(tLen+3) = 0

        if (ierr==0 .and. REQUEST==fnScheduleValidate) then ! nothing wrong?
            call list_sections_to_edit(device, Section, tLen, tArray, 0, SPACE, SPACE, .false., &
                '<i>Previous ''Validate'' found no fatal errors. Confirm the following data for '//trim(Section(idxWrk)%ClassID)// &
                ' ?</i> ')
            write(device,AFORMAT) nbsp//nbsp//'<input type="submit" name="action" value="Confirm"><hr>'
        end if
        call initialize_section(Section(idxWrk))

        ! no days or no hours?
        if (.not. is_TBA_day_or_hours(wrk)) then

            if (conflict_room .or. action_index==3) then ! tAction=='Find rooms') then
                call room_search_given_time(device, NumSections, Section, wrk, section_index, jdx, room_dept)
            end if

            write(device,AFORMAT) 'Search for rooms in: <select name="room_dept"><option value="0"> '
            do ddx=2,NumDepartments
                if (ddx/=room_dept) then
                    idx_select = 0
                else
                    idx_select = 1
                end if
                write(device,AFORMAT) '<option value="'//trim(Department(ddx)%Code)//'"'//trim(selected(idx_select))//'> '// &
                    trim(Department(ddx)%Code)//DASH//trim(Department(ddx)%Name)
            end do
            write(device,AFORMAT) '</select>'//nbsp//'<input type="submit" name="action" value="Find rooms"><hr>'

            if (conflict_teacher .or. action_index==4) then ! tAction=='Find teachers') then
                call teacher_search_given_time(device, NumSections, Section, wrk, section_index, idx, teacher_dept)
            end if

            write(device,AFORMAT) 'Search for teachers in: <select name="teacher_dept"><option value="0"> '
            do ddx=2,NumDepartments
                if (ddx/=teacher_dept) then
                    idx_select = 0
                else
                    idx_select = 1
                end if
                write(device,AFORMAT) '<option value="'//trim(Department(ddx)%Code)//'"'//trim(selected(idx_select))//'> '// &
                    trim(Department(ddx)%Code)//DASH//trim(Department(ddx)%Name)
            end do
            write(device,AFORMAT) '</select>'//nbsp//'<input type="submit" name="action" value="Find teachers"><hr>'

        end if

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
            if (sdx==section_index) cycle ! exclude section being edited
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
        flagIsUp = .false. ! no room
        do idx_meet=1,wrk%NMeets
            if (wrk%RoomIdx(idx_meet)>0 .and. wrk%bTimeIdx(idx_meet)>0 .and. &
                    wrk%DayIdx(idx_meet)>0) then
                flagIsUp = .true.
                exit
            end if
        end do
        if (flagIsUp) then ! a room is used by this section

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
            do idx_meet=1,wrk%NMeets
                rdx = wrk%RoomIdx(idx_meet)
                if (rdx/=0) then
                    tLen = 0
                    do idx=1,idx_meet-1 ! check if encountered previously
                        if (rdx==wrk%RoomIdx(idx)) tLen = tLen + 1
                    end do
                    if (tLen==0) then ! not yet encountered
                        flagIsUp = .true.
                        ! collect classes in room rdx
                        call timetable_meetings_in_room(NumSections, Section, rdx, section_index, tLen, tArray, TimeTable, &
                            conflict_room)
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
                        if (tLen>0) call timetable_display(device, Section, TimeTable)
                        write(device,AFORMAT) '<hr>'
                    end if
                end if
            end do
            if (.not. flagIsUp) then
                write(device,AFORMAT) '<b>Other class meetings in room(s)</b><br>(None)<hr>'
            end if

        end if

        flagIsUp = .false. ! no teachers
        do idx_meet=1,wrk%NMeets
            if (wrk%TeacherIdx(idx_meet)>0 .and. wrk%bTimeIdx(idx_meet)>0 .and. &
                    wrk%DayIdx(idx_meet)>0) then
                flagIsUp = .true.
                exit
            end if
        end do
        if (flagIsUp) then ! a teacher is assigned to this section

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
            do idx_meet=1,wrk%NMeets
                tdx = wrk%TeacherIdx(idx_meet)
                if (tdx/=0) then
                    tLen = 0
                    do idx=1,idx_meet-1 ! check if encountered previously
                        if (tdx==wrk%TeacherIdx(idx)) tLen = tLen + 1
                    end do
                    if (tLen==0) then ! not yet encountered
                        flagIsUp = .true.
                        ! collect classes of teacher
                        call timetable_meetings_of_teacher(NumSections, Section, tdx, section_index, tLen, tArray, TimeTable, &
                            conflict_teacher)
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
                        if (tLen>0) call timetable_display(device, Section, TimeTable)
                        write(device,AFORMAT) '<hr>'
                    end if
                end if
            end do
            if (.not. flagIsUp) then
                write(device,AFORMAT) '<b>Other classes of teacher(s)</b><br>(None)<hr>'
            end if
        end if

!        ! show blocks where section is used
!        write(device,AFORMAT) &
!            '<a name="block"></a><table border="0" width="100%">'//begintr, &
!            tdnbspendtd//tdalignright, &
!            '[ Other '//trim(Subject(crse)%Name)//' <a href="#sections">sections</a> ] ', &
!            '[ Usage of <a href="#rooms">room</a>(s) ] ', &
!            '[ Classes of <a href="#teachers">teacher</a>(s) ] ', &
!            !'[ Other sections in <a href="#block">block</a> ] ', &
!            '[ <a href="#TOP">TOP</a> ] ', &
!            endtd//endtr//'</table>'
!
        !    do targetBlock=1,NumBlocks
        !
        !      flagIsUp = .false.
        !      do idx=1,Block(targetBlock)%NumClasses
        !        if (Block(targetBlock)%Section(idx)/=section_index) cycle
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

    end subroutine section_validation_form


end module EditSECTIONS
