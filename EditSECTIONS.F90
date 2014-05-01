!======================================================================
!
!    HEEDS (Higher Education Enrollment Decision Support) - A program
!      to create enrollment scenarios for 'next term' in a university
!    Copyright (C) 2012-2014 Ricolindo L. Carino
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


    subroutine section_offer_subject(device, thisTerm, NumSections, Section, Offering, NumBlocks, Block, eList)
        integer, intent (in) :: device, thisTerm, NumBlocks
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in) :: eList(0:)
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_OFFERED_SUBJECTS), intent(in out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=MAX_LEN_SECTION_CODE) :: tSection
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer :: crse, kdx, dept
        character (len=255) :: mesg
        logical :: criticalErr

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
            call html_write_header(device, 'Open a section', linebreak//horizontal//mesg)
            return
        end if

        call check_array_bound (NumSections+2, MAX_ALL_SECTIONS, 'MAX_ALL_SECTIONS', criticalErr)
        if (criticalErr) then
            targetDepartment = DeptIdxUser
            targetCollege = CollegeIdxUser
            call html_write_header(device, 'Open a section', linebreak//horizontal//'No more space for additional section(s)')
            return
        end if

#if defined REGIST
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
        kdx = ScheduleCount(thisTerm,targetDepartment) + 1 ! new section in department
        ScheduleCount(thisTerm,targetDepartment) = kdx
        if (kdx>99) then
            tSection = Department(targetDepartment)%SectionPrefix//itoa(kdx)
        else if (kdx>9) then
            tSection = Department(targetDepartment)%SectionPrefix//'0'//itoa(kdx)
        else
            tSection = Department(targetDepartment)%SectionPrefix//'00'//itoa(kdx)
        end if


        if (isRoleOfficial) then
            ScheduleCount(thisTerm,targetDepartment) = ScheduleCount(thisTerm,targetDepartment) - 1
            mesg = '"Open a section in '//trim(tSubject)//'" failed. '//sorryMessage

        else

            if (Subject(crse)%LectHours>0) then ! subject has lecture
                NumSections = NumSections+1
                Section(NumSections) = TYPE_SECTION (trim(Subject(crse)%Name)//SPACE//tSection, tSection, SPACE, &
                    targetDepartment, crse, Subject(crse)%MaxLectSize, Subject(crse)%MaxLectSize, 1, 0, 0, 0, 0, 0)
            end if
            if (Subject(crse)%LabHours>0) then ! subject has lab/recitation
                NumSections = NumSections+1
                tSection = trim(tSection)//DASH//'1L'
                Section(NumSections) = TYPE_SECTION (trim(Subject(crse)%Name)//SPACE//tSection, tSection, SPACE, &
                targetDepartment, crse, Subject(crse)%MaxLabSize, Subject(crse)%MaxLabSize, 1, 0, 0, 0, 0, 0)
            end if

            call xml_write_classes(pathToTerm, NumSections, Section, 0)
            call offerings_summarize(NumSections, Section, Offering)

            mesg = 'Opened a section in '//tSubject

        end if

        call section_list_classes (device, thisTerm, NumSections, Section, NumBlocks, Block, eList, &
            fnScheduleByArea, dept, tSubject, mesg)

    end subroutine section_offer_subject


    subroutine section_add_laboratory(device, thisTerm, NumSections, Section, Offering, NumBlocks, Block, eList)
        integer, intent (in) :: device, thisTerm, NumBlocks
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in) :: eList(0:)
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_OFFERED_SUBJECTS), intent(in out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        character(len=MAX_LEN_CLASS_ID) :: tClassId
        character(len=MAX_LEN_SECTION_CODE) :: tSection
        character (len=255) :: mesg
        integer :: crse, sect, dept
        logical :: criticalErr

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
            call html_write_header(device, 'Add lab', linebreak//horizontal//mesg)
            return
        end if

        call check_array_bound (NumSections+1, MAX_ALL_SECTIONS, 'MAX_ALL_SECTIONS', criticalErr)
        if (criticalErr) then
            targetDepartment = DeptIdxUser
            targetCollege = CollegeIdxUser
            call html_write_header(device, 'Open a section', linebreak//horizontal//'No more space for additional section(s)')
            return
        end if

        dept = Section(sect)%DeptIdx
        targetDepartment = dept
        targetCollege = Department(dept)%CollegeIdx
        crse = Section(sect)%SubjectIdx
        tSection = get_next_lab_section(sect, NumSections, Section)

        if (isRoleOfficial) then
            mesg = '"Open new section '//trim(Subject(crse)%Name)//SPACE//trim(tSection)//'" failed. '//sorryMessage

        else

            NumSections = NumSections+1
            Section(NumSections) = TYPE_SECTION (trim(Subject(crse)%Name)//SPACE//tSection, tSection, SPACE, &
                targetDepartment, crse, Subject(crse)%MaxLabSize, Subject(crse)%MaxLabSize, 1, 0, 0, 0, 0, 0)
            !write(*,*) 'Adding '//trim(Subject(crse)%Name)//SPACE//tSection

            call xml_write_classes(pathToTerm, NumSections, Section, 0)
            call offerings_summarize(NumSections, Section, Offering)
            mesg = 'Opened new section '//trim(Subject(crse)%Name)//SPACE//tSection

        end if

        call section_list_classes (device, thisTerm, NumSections, Section, NumBlocks, Block, eList, &
            fnScheduleByArea, dept, Subject(crse)%Name, mesg)

    end subroutine section_add_laboratory


    subroutine section_delete(device, thisTerm, NumSections, Section, Offering, NumBlocks, Block, elist)
        integer, intent (in) :: device, thisTerm, NumBlocks
        type (TYPE_BLOCK), intent(in out) :: Block(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in out) :: eList(0:)
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_OFFERED_SUBJECTS), intent(in out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        character(len=MAX_LEN_CLASS_ID) :: tClassId
        integer :: sect, crse, pos, i, dept
        character (len=127) :: mesg

        call html_comment('section_delete()')

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
            call html_write_header(device, 'Delete section', linebreak//horizontal//mesg)
            return
        end if

        dept = Section(sect)%DeptIdx
        targetDepartment = dept
        targetCollege = Department(dept)%CollegeIdx

        pos = 0 ! how many students affected
        do i=1,NumStudents+NumAdditionalStudents
            do crse=1,eList(i)%lenSubject
                if (eList(i)%Section(crse)==sect) pos = pos+1
            end do
        end do

        ! remember subject
        crse = Section(sect)%SubjectIdx

        if (pos>0) then

            mesg = '"Delete section '//trim(tClassId)//'" failed. '//trim(itoa(pos))// &
                ' students enlisted.'

        else if (isRoleOfficial) then

            mesg = '"Delete section '//trim(tClassId)//'" failed. '//sorryMessage

        else

            mesg = 'Deleted section '//tClassId

            if (.not. is_lecture_lab_subject(crse)) then ! lecture only, or lab only
                call delete_section_from_blocks(sect, Section, NumBlocks, Block)
            else ! lecture-lab subject
                if (is_lecture_class(sect, Section)) then ! remove lecture and lab sections
                    call delete_section_from_blocks(sect, Section, NumBlocks, Block)
                    tClassId = trim(tClassId)//DASH
                    i = len_trim(tClassId)
                    do pos=1,NumSections
                        if (Section(pos)%ClassId(:i)/=tClassId(:i)) cycle
                        call delete_section_from_blocks(pos, Section, NumBlocks, Block)
                    end do
                else ! remove this section only
                    call delete_section_from_blocks(sect,Section, NumBlocks, Block)
                end if
            end if
            call xml_write_blocks(pathToTerm, NumBlocks, Block,  Section, 0)
            call xml_write_classes(pathToTerm, NumSections, Section, 0)
            call offerings_summarize(NumSections, Section, Offering)
            call count_sections_by_dept(thisTerm, NumSections, Section)

        end if

        call section_list_classes (device, thisTerm, NumSections, Section, NumBlocks, Block, eList, &
            fnScheduleByArea, dept, Subject(crse)%Name, trim(mesg))

    end subroutine section_delete


    subroutine section_validate_inputs(device, thisTerm, NumSections, Section, Offering, NumBlocks, Block, eList)
        integer, intent (in) :: device, thisTerm
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        integer, intent (in) :: NumBlocks
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in) :: eList(0:)
        type (TYPE_OFFERED_SUBJECTS), intent(in out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        integer :: action_index, ierr, sect
        integer :: teacher_dept, room_dept, dept
        type (TYPE_SECTION) :: wrk
        character(len=MAX_LEN_CLASS_ID) :: tClassId, tAction
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character (len=255) :: errMesg
        character (len=80) :: header

        call html_comment('section_validate_inputs()')

        call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, sect)
        sect = index_to_section(tClassId, NumSections, Section)
        errMesg = SPACE

        dept = Section(sect)%DeptIdx
        targetDepartment = dept
        targetCollege = Department(dept)%CollegeIdx
        room_dept = 0
        teacher_dept = 0

        if (REQUEST==fnScheduleEdit) then
            header = 'Edit section '//tClassId
            action_index = 1
            wrk = Section(sect)

        else ! (REQUEST==fnScheduleValidate) then
            header = 'Proposed changes to section '//tClassId

            ! extract section info from QUERY
            call section_build_from_query (Section, sect, wrk)

            ! action is ?
            call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
            select case (trim(tAction))

                case ('Confirm') ! Accept previously validated edits
                    if (isROleOfficial) then
                        errMesg = '"Edit '//trim(tClassId)//'" failed. '//sorryMessage
                    else
!                        ! who are the students in the class?
!                        call collect_students_in_section (sect, NumSections, Section, eList, count_in_class, tArray)
!                        call html_comment(itoa(count_in_class)//'students in '//tClassId)
!
!                        ! how many with messed schedules
!                        do idx=1,count_in_class
!                            std = tArray(idx)
!                            call timetable_meetings_of_student(NumSections, Section, std, eList, sect, & ! skip target section
!                                tLen1, tArray(count_in_class+1:), TimeTable, conflicted)
!                            !if (conflicted) cycle ! there's prior conflict
!                            if (is_conflict_timetable_with_struct_section(wrk, 1, wrk%NMeets, TimeTable)) then
!                                count_affected = count_affected + 1
!                                tArray(count_affected) = std ! move to beginning of list
!                                call html_comment(text_student_curriculum(std))
!                                errMesg = ' : '//trim(Student(std)%StdNo)//errMesg
!                            end if
!                        end do
!                        if (count_affected==0) then ! no problems
                            header = 'Finished editing '//trim(tClassId)
                            Section(sect) = wrk
                            call xml_write_classes(pathToTerm, NumSections, Section, 0)
                            call offerings_summarize(NumSections, Section, Offering)
                            call count_sections_by_dept(thisTerm, NumSections, Section)
                            call section_list_classes (device, thisTerm, NumSections, Section, NumBlocks, Block, eList,  &
                                fnScheduleByArea, dept, tClassId, header)
                            return
!                        else
!                            errMesg = 'Changes not valid - will result in schedule conflicts for'//errMesg
!                            action_index = 1
!                        end if
                    end if

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
        call html_write_header(device, header, errMesg)
        call section_validation_form(device, thisTerm, NumSections, Section, eList, &
            action_index, sect, wrk, teacher_dept, room_dept)

    end subroutine section_validate_inputs


    subroutine section_write_edit_form(device, thisTerm, Section, sect, tSection, teacher_dept, room_dept)
        integer, intent (in) :: device, thisTerm, sect, teacher_dept, room_dept
        type (TYPE_SECTION), intent(in) :: Section(0:)
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
        call make_form_start(device, fnScheduleValidate, Section(sect)%ClassId, A9=thisTerm)

        write(device,AFORMAT) &
            beginbold//'SECTION CODE'//endbold, &
            trim(Section(sect)%Code)//nbsp//nbsp//beginitalic//'change to '//enditalic, &
            nbsp//'<input size="'//trim(itoa(tLen))//'" name="code" value="'//trim(tSection%Code)//'">', &
            nbsp//nbsp//nbsp//nbsp//nbsp//nbsp, &
            beginbold//'NO. OF STUDENTS'//endbold//trim(itoa(Section(sect)%Slots))// &
            nbsp//nbsp//beginitalic//'change to '//enditalic, &
            nbsp//'<input size="3" name="slots" value="'//trim(itoa(tSection%Slots))//'">'
        if (thisTerm/=3) write(device,AFORMAT) linebreak, & ! not summer
            beginitalic//'(Note: Class meetings must total  '//beginbold//trim(tHours)//endbold//') :'//enditalic

        write(device,AFORMAT) '<table border="0" width="100%">'//begintr, &
            '<td align="left">'//beginbold//'Meeting'//endbold//endtd//&
            '<td align="left">'//beginbold//'Day'//endbold//endtd// &
            '<td align="left">'//beginbold//'Begin'//endbold//endtd//&
            '<td align="left">'//beginbold//'End'//endbold//endtd// &
            '<td align="left">'//beginbold//'Room'//endbold//endtd//&
            '<td align="left">'//beginbold//'Teacher'//endbold//endtd//endtr
        do idx_meet=1,tSection%NMeets+3
            DayIdx = tSection%DayIdx(idx_meet)
            bTimeIdx = tSection%bTimeIdx(idx_meet)
            eTimeIdx = tSection%eTimeIdx(idx_meet)
            RoomIdx = tSection%RoomIdx(idx_meet)
            TeacherIdx = tSection%TeacherIdx(idx_meet)
            if (idx_meet<=tSection%NMeets) then
                write(device,AFORMAT) begintr//tdaligncenter//trim(itoa(idx_meet))//endtd
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
                    if ( is_chair_of_department(Room(rdx)%DeptIdx, orHigherUp) .and. Room(rdx)%DeptIdx==room_dept ) then
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
                    if ( is_chair_of_department(Teacher(idx)%DeptIdx, orHigherUp) .and. Teacher(idx)%DeptIdx==teacher_dept ) then
                        write(device,AFORMAT) '<option value="'//trim(Teacher(idx)%TeacherID)//'"> '//trim(Teacher(idx)%Name)
                    end if
                else
                    write(device,AFORMAT) '<option value="'//trim(Teacher(idx)%TeacherID)//'" selected="selected"> '// &
                        trim(Teacher(idx)%Name)
                end if
            end do
            write(device,AFORMAT) '</select>'//endtd//endtr

        end do
        write(device,AFORMAT) endtable, linebreak, &
            beginitalic//'If you made a change above, '//enditalic//nbsp//'<input type="submit" name="action" value="Validate">', &
            endform//horizontal

    end subroutine section_write_edit_form


    subroutine section_build_from_query (Section, section_index, tSection)
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

    end subroutine section_build_from_query



    subroutine section_validation_form(device, thisTerm, NumSections, Section, eList, &
            action_index, section_index, wrk, teacher_dept, room_dept)
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in) :: eList(0:)
        integer, intent (in) :: device, thisTerm, action_index, section_index, teacher_dept, room_dept
        type (TYPE_SECTION), intent(in) :: wrk
        integer :: ierr, crse, ddx, idx, jdx, mdx, rdx, sdx, tdx, idx_meet, idx_select, tLen, idxWrk
        integer, dimension(60,6) :: TimeTable
        logical :: conflict_teacher, conflict_room, flagIsUp, conflict_student
        integer :: count_in_class, count_affected

        call html_comment('section_validation_form()', &
            ' action_index='//itoa(action_index), &
            ' room_dept='//itoa(room_dept), &
            ' teacher_dept='//itoa(teacher_dept))

        call section_write_edit_form(device, thisTerm, Section, section_index, wrk, teacher_dept, room_dept)
        crse = Section(section_index)%SubjectIdx
        ierr = 0

        ! section code & class ID
        if (wrk%Code==SPACE) then ! code NOT specified
            ierr = ierr+1
            write(device,AFORMAT) red//'  Section code not specified?'//black//linebreak
        else ! code specified
            idx = index_to_section(wrk%ClassId, NumSections, Section)
            if (idx>0 .and. idx/=section_index) then
                ierr = ierr+1
                write(device,AFORMAT) red//'  Class ID '//wrk%ClassId//' already in use?'//black//linebreak
            end if
        end if
        ! no. of students
        if (wrk%Slots<=0) then
            ierr = ierr+1
            write(device,AFORMAT) red//'  Number of students must be more than 0?'//black//linebreak
        end if

        ! correct no. of hours?
        if (.not. is_consistent_section_hours_with_subject_defn(wrk, thisTerm)) then
            ierr = ierr+1
            write(device,AFORMAT) red//'Total meeting hours is inconsistent with subject definition hours?'//black//linebreak
        end if

        ! meeting conflicts?
        if (.not. is_conflict_free_section_hours(wrk, NumSections, Section)) then
            ierr = ierr+1
            write(device,AFORMAT) red//'Conflict in meeting times; or, if a lecture-lab subject, '// &
            'conflict with lecture section?'//black//linebreak
        end if

        ! check room conflict for each meeting
        do idx_meet=1,wrk%NMeets
            rdx = wrk%RoomIdx(idx_meet)
            if (rdx==0 .or. Room(rdx)%Code=='TBA') cycle ! none assigned yet
            call timetable_meetings_in_room(NumSections, Section, rdx, section_index, tLen, tArray, TimeTable, conflict_room)
            if (conflict_room) then
                ierr = ierr+1
                write(device,AFORMAT) red//'  Prior conflict of classes in room '//trim(Room(rdx)%Code)//black//linebreak
            else
                if (is_conflict_timetable_with_struct_section(wrk, idx_meet, idx_meet, TimeTable)) then
                    conflict_room = .true.
                    ierr = ierr+1
                    write(device,AFORMAT) red//'  Meeting '//itoa(idx_meet)//' conflicts with classes in room '// &
                        trim(Room(rdx)%Code)//black//linebreak
                end if
            end if
          !if (isRoleChair .and. Room(rdx)%DeptIdx/=DeptIdxUser) then
          !    write(device,AFORMAT) red//'  '//trim(Department(DeptIdxUser)%Code)//' cannot assign classes to room '// &
          !      trim(Room(rdx)%Code)//'@'//trim(Department(Room(rdx)%DeptIdx)%Code)//black//linebreak
          !    ierr = ierr+1
          !end if
        end do

        ! check if room capacity exceeded
        do idx_meet=1,wrk%NMeets
            rdx = wrk%RoomIdx(idx_meet)
            if (rdx==0 .or. Room(rdx)%Code=='TBA') cycle ! none assigned yet
            if (Room(rdx)%MaxCapacity<wrk%Slots) then
                ierr = ierr+1
                write(device,AFORMAT) red//'  Class size exceeds capacity of room '//trim(Room(rdx)%Code)//black//linebreak
            end if
        end do

        ! check teacher conflict for each meeting
        do idx_meet=1,wrk%NMeets
            tdx = wrk%TeacherIdx(idx_meet)
            if (tdx==0 .or. Teacher(tdx)%TeacherId=='TBA)') cycle
            call timetable_meetings_of_teacher(NumSections, Section, tdx, section_index, tLen, tArray, TimeTable, conflict_teacher)
            if (conflict_teacher) then ! teacher has conflicted schedule
                ierr = ierr+1
                write(device,AFORMAT) red//'  Prior conflict in teaching schedule of '//trim(Teacher(tdx)%Name)//black//linebreak
            else
                if (is_conflict_timetable_with_struct_section(wrk,idx_meet,idx_meet,TimeTable)) then
                    conflict_teacher = .true.
                    ierr = ierr+1
                    write(device,AFORMAT) red//'  Meeting '//itoa(idx_meet)//' conflicts with teaching schedule of '// &
                        trim(Teacher(tdx)%Name)//black//linebreak
                end if
            end if
          !if (isRoleChair .and. Teacher(tdx)%DeptIdx/=DeptIdxUser) then
          !    write(device,AFORMAT) red//'  '//trim(Department(DeptIdxUser)%Code)//' cannot assign classes to '// &
          !      trim(Teacher(tdx)%Name)//' of '//trim(Department(Teacher(tdx)%DeptIdx)%Code)//black//linebreak
          !    ierr = ierr+1
          !end if
        end do

        ! who are the students in the class?
        call collect_students_in_section (section_index, NumSections, Section, eList, count_in_class, tArray)
        call html_comment(itoa(count_in_class)//'students in '//wrk%ClassId)

        ! how many with messed schedules
        count_affected = 0
        do idx=1,count_in_class
            sdx = tArray(idx)
            call timetable_meetings_of_student(NumSections, Section, sdx, eList, section_index, & ! skip target section
                tLen, tArray(count_in_class+1:), TimeTable, conflict_student)
            if (is_conflict_timetable_with_struct_section(wrk, 1, wrk%NMeets, TimeTable)) then
                count_affected = count_affected + 1
                tArray(count_affected) = sdx ! move to beginning of list
                call html_comment(text_student_curriculum(sdx))
            end if
        end do
        if (count_affected>0) then ! schedule conflicts
            ierr = ierr + 1
            write(device,AFORMAT) red//'  Proposed class times '
            do idx=1,wrk%NMeets
                write(device,AFORMAT) trim(itoa(idx))//DOT//nbsp//txtDay(wrk%DayIdx(idx))//nbsp// &
                    trim(text_time_period(wrk%bTimeIdx(idx), wrk%eTimeIdx(idx)))//nbsp
            end do
            write(device,AFORMAT) '  will cause schedule conflicts for the following students:'//black//linebreak
            call html_student_list (device, count_affected, tArray, &
                is_dean_of_college(Department(Section(section_index)%DeptIdx)%CollegeIdx, orHigherUp))
        end if

        if (ierr>0) write(device,AFORMAT) horizontal

        ! common form inputs
        idxWrk = MAX_ALL_SECTIONS ! temporary work area
        Section(idxWrk) = wrk
        call make_form_start(device, fnScheduleValidate, Section(section_index)%ClassId, A9=thisTerm)
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
            call list_sections_to_edit(device, thisTerm, Section, tLen, tArray, 0, SPACE, SPACE, .false., &
                beginitalic//'Previous ''Validate'' found no fatal errors. Confirm the following data for '// &
                trim(Section(idxWrk)%ClassID)//' ?'//enditalic)
            write(device,AFORMAT) nbsp//nbsp//'<input type="submit" name="action" value="Confirm">', &
                horizontal
        end if
        call initialize_section(Section(idxWrk))

        ! no days or no hours?
        if (.not. is_TBA_day_or_hours(wrk)) then

            if (conflict_room .or. action_index==3) then ! tAction=='Find rooms') then
                call room_search_given_time(device, thisTerm, NumSections, Section, wrk, section_index, jdx, room_dept)
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
            write(device,AFORMAT) '</select>'//nbsp//'<input type="submit" name="action" value="Find rooms">'//horizontal

            if (conflict_teacher .or. action_index==4) then ! tAction=='Find teachers') then
                call teacher_search_given_time(device, thisTerm, NumSections, Section, wrk, section_index, idx, teacher_dept)
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
            write(device,AFORMAT) '</select>'//nbsp//'<input type="submit" name="action" value="Find teachers">'//horizontal

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
            endtd//endtr//endtable
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
        call list_sections_to_edit(device, thisTerm, Section, tLen, tArray, 0, SPACE, SPACE, .false., &
            beginbold//'Other sections'//endbold)
        write(device,AFORMAT) horizontal

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
                endtd//endtr//endtable
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
                        call list_sections_to_edit(device, thisTerm, Section, tLen, tArray, 0, SPACE, SPACE, .false., &
                            beginbold//'Meetings in '//trim(Room(rdx)%Code)//endbold)

                        ! add section meetings
                        do mdx=1,wrk%NMeets
                            ddx = wrk%DayIdx(mdx)
                            if (ddx==0) cycle
                            if (wrk%RoomIdx(mdx)==rdx) then
                                do jdx = wrk%bTimeIdx(mdx), wrk%eTimeIdx(mdx)-1
                                    !TimeTable(jdx,ddx) = -1
                                    if (TimeTable(jdx,ddx)==0) TimeTable(jdx,ddx) = -1
                                end do
                            end if
                        end do
                        if (tLen>0) call timetable_display(device, Section, TimeTable)
                        write(device,AFORMAT) horizontal
                    end if
                end if
            end do
            if (.not. flagIsUp) then
                write(device,AFORMAT) beginbold//'Other class meetings in room(s)'//endbold//BRNONEHR
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
                endtd//endtr//endtable
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
                        call list_sections_to_edit(device, thisTerm, Section, tLen, tArray, 0, SPACE, SPACE, .false., &
                            beginbold//'Class meetings of '//trim(Teacher(tdx)%Name)//endbold)
                        ! add section meetings
                        do mdx=1,wrk%NMeets
                            ddx = wrk%DayIdx(mdx)
                            if (ddx==0) cycle
                            if (wrk%TeacherIdx(mdx)==tdx) then
                                do jdx = wrk%bTimeIdx(mdx), wrk%eTimeIdx(mdx)-1
                                    !TimeTable(jdx,ddx) = -1
                                    if (TimeTable(jdx,ddx)==0) TimeTable(jdx,ddx) = -1
                                end do
                            end if
                        end do
                        if (tLen>0) call timetable_display(device, Section, TimeTable)
                        write(device,AFORMAT) horizontal
                    end if
                end if
            end do
            if (.not. flagIsUp) then
                write(device,AFORMAT) beginbold//'Other classes of teacher(s)'//endbold//BRNONEHR
            end if
        end if

        write(device,AFORMAT) endform

    end subroutine section_validation_form



    subroutine teacher_search_given_time(device, thisTerm, NumSections, Section, wrk, to_skip, teacher_count, given_teacher_dept)
        integer, intent (in) :: to_skip, device, thisTerm
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
        write(device,AFORMAT) beginbold//'Teachers in '//trim(Department(teacher_dept)%Code)//' available to teach'//endbold
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
                call meetings_of_section_by_teacher(Section, i, tdx, n_meetings, meetings)
                if (n_meetings==0) cycle ! teacher not assigned to this section
                ncol = ncol+1
                tArray(ncol) = i
                if (is_conflict_timetable_with_section_meetings(Section, i, n_meetings, meetings, TimeTable)) then
                    skip = .true. ! teacher not available
                    exit ! do not consider the remaining sections for this teacher
                end if
            end do
            if (skip) cycle ! done with this teacher
            teacher_count = teacher_count+1
            write(device,AFORMAT) trim(make_href(fnTeacherEditSchedule, Teacher(tdx)%Name, &
                A1=Teacher(tdx)%TeacherID, A9=thisTerm, &
                pre=begintr//begintd, &
                post=endtd//begintd//'['//trim(itoa(ncol))//']'))
            do j=1,ncol
                write(device,AFORMAT) ' : '//Section(tArray(j))%ClassId
            end do
            write(device,AFORMAT) endtd//endtr
        end do
        if (teacher_count == 0) write(device,AFORMAT) begintr, &
            '<td colspan="2">'//beginitalic//'(No teachers available during specified times, or time not specified)'// &
            enditalic//endtd, &
            endtr
        write(device,AFORMAT) endtable

    end subroutine teacher_search_given_time


    subroutine room_search_given_time(device, thisTerm, NumSections, Section, wrk, to_skip, room_count, given_room_dept)
        integer, intent (in) :: to_skip, device, thisTerm
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
        write(device,AFORMAT) beginbold//'Rooms in '//trim(Department(room_dept)%Code)// &
        ' available during'//endbold
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
                call meetings_of_section_in_room(Section, i, rdx, n_meetings, meetings)
                if (n_meetings==0) cycle ! room not assigned to this section
                !write(*,*) 'Section is ', Section(i)%ClassId
                ncol = ncol+1
                tArray(ncol) = i
                if (is_conflict_timetable_with_section_meetings(Section, i, n_meetings, meetings, TimeTable)) then
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
                A1=QUERY_put, A9=thisTerm, &
                pre=begintr//begintd, &
                post=endtd//begintd//'['//trim(itoa(ncol))//']'))
            do j=1,ncol
                write(device,AFORMAT) ' : '//Section(tArray(j))%ClassId
            end do
            write(device,AFORMAT) endtd//endtr
        end do
        if (room_count == 0) write(device,AFORMAT) begintr, &
            '<td colspan="2">'//beginitalic//'(No rooms available during specified times, or time not specified)'//enditalic, &
            endtd, endtr
        write(device,AFORMAT) endtable

    end subroutine room_search_given_time


    subroutine section_list_classes (device, thisTerm, NumSections, Section, NumBlocks, Block, eList, &
            fn, givenDept, givenArea, mesg)

        integer, intent (in) :: device, thisTerm, fn
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        integer, intent (in) :: NumBlocks
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in) :: eList(0:)
        integer, intent(in), optional :: givenDept
        character(len=*), intent(in), optional :: mesg, givenArea

        integer :: cdx, idx, jdx, i, rdx, sdx, tdx, ncol, maxcol=7, nopen, nsections
        integer :: owner_dept, owner_coll, previous
        character(len=127) :: header
        character(len=22) :: preText, postText
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject, tSeats, searchString
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        logical :: isLecture, okToAdd, conflicted, showStudentList
        integer, dimension(60,6) :: TimeTable

        targetDepartment = DeptIdxUser
        targetCollege = CollegeIdxUser
        call recalculate_available_seats(Section, eList)
        select case(fn)

            case (fnScheduleByArea) ! sections by subject area
                if (present(givenArea)) then
                    searchString = givenArea
                    tCollege = College(Department(givenDept)%CollegeIdx)%Code
                else
                    call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, i)
                    call cgi_get_named_string(QUERY_STRING, 'A2', searchString, i)
                end if
                targetCollege = index_to_college(tCollege)
                header = '"'//trim(searchString)//'" classes in '//tCollege
#if defined REGIST
                ! department already set above
#else
                ! Subjects administered by program
                tDepartment = tCollege
                targetDepartment = index_to_dept(tDepartment)
#endif

            case (fnTBARooms, fnTBATeachers) ! TBA rooms, teachers
                call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, i)
                targetCollege = index_to_college(tCollege)
                header = trim(tCollege)//' classes with TBA room/teacher'
#if defined REGIST
                ! department already set above
#else
                ! Subjects administered by program
                tDepartment = tCollege
                targetDepartment = index_to_dept(tDepartment)
#endif

            case (fnUnpostedGrades, fnGradesheetsToReceive) ! unposted grades, no hardcopy of gradesheets
                call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, i)
                targetCollege = index_to_college(tCollege)
                header = trim(tCollege)//' classes with unposted grades/no hardcopy of gradesheet'
#if defined REGIST
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

        if (fn==fnUnpostedGrades) then
            tArray(1:NumSections) = 0
            ! mark sections in college with unposted grades
            do tdx = 1,NumStudents+NumAdditionalStudents
                do idx=1,eList(tdx)%NPriority+eList(tdx)%NAlternates+eList(tdx)%NCurrent
                    sdx = eList(tdx)%Section(idx)
                    if (sdx>0) then ! accommodated
                        owner_coll = Department(Section(sdx)%DeptIdx)%CollegeIdx
                        if (owner_coll==targetCollege) then
                            if (gdxREGD==eList(tdx)%Grade(idx) ) then
                                tArray(sdx) = tArray(sdx)+1
                                !call html_comment(Student(tdx)%Name//Section(sdx)%ClassId// &
                                !    txtGrade(pGrade(eList(tdx)%Grade(idx))) )
                            end if
                        end if
                    end if
                end do
            end do
            ! compress
            if ( is_dean_of_college(targetCollege,orHigherUp) ) then
                do sdx=1,NumSections
                    if (tArray(sdx)==0) cycle
                    nsections = nsections+1
                    tArray(nsections) = sdx
                end do
            else
                do sdx=1,NumSections
                    if (tArray(sdx)==0) cycle
                    ! user is teacher of section?
                    i = 0
                    do ncol=1,Section(sdx)%NMeets
                        if (Section(sdx)%TeacherIdx(ncol)==requestingTeacher) i = i+1
                    end do
                    if (i>0) then
                        nsections = nsections+1
                        tArray(nsections) = sdx
                    end if
                end do
            end if
        else

            do sdx=1,NumSections
                cdx = Section(sdx)%SubjectIdx
                if (cdx==0) cycle ! section was deleted
                owner_dept = Section(sdx)%DeptIdx
                owner_coll = Department(owner_dept)%CollegeIdx
                okToAdd = .false.

                select case(fn)

                    case (fnGradesheetsToReceive) ! sections with empty GradeSubmissionDate
                        okToAdd = len_trim(Section(sdx)%GradeSubmissionDate)==0 .and. &
                            owner_coll == targetCollege

                    case (fnScheduleByArea) ! sections by subject area
                        okToAdd = index(Section(sdx)%ClassId, trim(searchString)//SPACE)==1 .and. &
                            owner_coll == targetCollege

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

        end if

        if (nsections==0) then
            write(device,AFORMAT) BRNONEHR
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
                    preText = begintr//begintd
                    postText = endtd
                else if (ncol == maxcol) then
                    preText = begintd
                    postText = endtd//endtr
                    ncol = 0
                else
                    preText = begintd
                    postText = endtd
                end if

                write(device,AFORMAT) trim(preText)//'<a href="#'//trim(tSubject)//'">'//trim(tSubject)//'</a>'//postText

            end do
            if (ncol /= 0)  then
                do i=ncol+1,maxcol
                    write(device,AFORMAT) tdnbspendtd
                end do
                write(device,AFORMAT) endtr
            end if
            write(device,AFORMAT) endtable
        end if


        write(device,AFORMAT) &
            horizontal//beginitalic//'Note: If the hyperlinks are active, the section code links to the gradesheet,', &
            ' the block name links to the block schedule, and the no. of seats links to the classlist. ', &
            ' Deleting a lecture section automatically deletes the associated laboratory or recitation sections. ', &
            ' Laboratory or recitation section codes MUST have the format "LECT-nL" or "LECT-nR" where "LECT" is ', &
            ' the code for the lecture section, and "n" is an integer.'//enditalic//linebreak//linebreak, &
            '<table border="0" width="100%">'//begintr, &
            thalignleft//'Subject'//endth//&
            thalignleft//'Section'//endth//&
            thalignleft//'Block'//endth//&
            thalignleft//'Seats/Open'//endth//&
            thalignleft//'Day'//endth//&
            thalignleft//'Time'//endth//&
            thalignleft//'Room'//endth//&
            thalignleft//'Teacher'//endth//&
            thalignleft//beginsmall//'Action'//endsmall//endth//endtr

        okToAdd = is_admin_of_college(targetCollege) .or. &
            ( is_chair_of_department(targetDepartment,orHigherUp) .and. &
              ( (thisTerm==currentTerm .and. isPeriodOne) .or. &
                thisTerm==nextTerm ) ) ! (thisTerm==nextTerm .and. (.not. isPeriodOne)) ) )

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
                begintd//'<a name="'//trim(Subject(cdx)%Name)//'"></a>'//beginsmall//'[<a href="#TOP">Top</a>]'// &
                endsmall//endtd//endtr
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
            if (fn/=fnTeacherClasses .and. (is_admin_of_college(targetCollege) .or. isRoleOfficial)) then
                write(device,AFORMAT) trim(make_href(fnEditSubject, 'Edit', A1=Subject(cdx)%Name, A9=thisTerm, &
                    pre=nbsp//beginsmall//'[ ', post=' ]'//endsmall))
            end if
            write(device,AFORMAT) endtd//endtr, &
                begintr//tdnbspendtd//'<td colspan="7">'//nbsp//'Pr. '//trim(text_prerequisite_of_subject(cdx,0))//endtd

            if (fn/=fnTeacherClasses .and. okToAdd) then
                write(device,AFORMAT) begintd//beginsmall, &
                    trim(make_href(fnScheduleOfferSubject, 'Add '//tDepartment, &
                    A1=QUERY_put, A2=Department(targetDepartment)%Code, A9=thisTerm, &
                    post=endsmall//endtd//endtr ))
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
                    tSeats = trim(itoa(Section(sdx)%Slots))//FSLASH//trim(itoa(Section(sdx)%RemSlots))
                end if
                QUERY_put = Section(sdx)%ClassId
                ! subject
                write(device,AFORMAT) begintr//tdnbspendtd

                ! section code, link to gradesheet entry form
                showStudentList = is_admin_of_college(targetCollege) .or. &
                     ( is_teacher_of_class (Section(sdx), orHigherUp) .and. &
                       ( ((isPeriodThree .or. isPeriodFour) .and. thisTerm==currentTerm) .or. &
                         (isPeriodOne .and. thisTerm==cTm1) ) )
                if ( showStudentList ) then
                    write(device,AFORMAT) trim(make_href(fnGradesheet, trim(Section(sdx)%Code), &
                        A1=QUERY_PUT, A9=thisTerm, pre=begintd, post=endtd ))
                elseif (is_teacher_of_class (Section(sdx), orHigherUp)) then
                    write(device,AFORMAT) trim(make_href(fnPrintableGradesheet, trim(Section(sdx)%Code), &
                        A1=QUERY_PUT, A9=thisTerm, pre=begintd, post=endtd ))
                else
                    write(device,AFORMAT) begintd//trim(Section(sdx)%Code)//endtd
                end if

                ! blocks
                write(device,AFORMAT) begintd
                call blocks_in_section(device, sdx, fnBlockSchedule, thisTerm, NumBlocks, Block)
                write(device,AFORMAT) endtd

                ! seats, link to classlist
                if ( is_teacher_of_class (Section(sdx), orHigherUp) ) then
                    write(device,AFORMAT) trim(make_href(fnClassList, tSeats, &
                        A1=QUERY_PUT, A9=thisTerm, pre=begintd, post=endtd))
                else
                    write(device,AFORMAT) begintd//trim(tSeats)//endtd
                end if

                ! time, day, room, teacher
                if (is_regular_schedule(sdx, Section)) then
                    tdx = Section(sdx)%TeacherIdx(1)
                    rdx = Section(sdx)%RoomIdx(1)
                    write(device,AFORMAT) &
                        begintd//trim(text_days_of_section(Section(sdx)) )//endtd// &
                        begintd//trim(text_time_period(Section(sdx)%bTimeIdx(1), Section(sdx)%eTimeIdx(1)))//endtd
                    if (rdx/=0) then
                        if (fn==fnRoomSchedule) then
                            write(device,AFORMAT) begintd//trim(Room(rdx)%Code)//endtd
                        else
                            write(device,AFORMAT) trim(make_href(fnRoomSchedule, Room(rdx)%Code, &
                                A1=Room(rdx)%Code, A9=thisTerm, pre=begintd, post=endtd))
                        end if
                    else
                        write(device,AFORMAT) begintd//red//trim(Room(rdx)%Code)//black//endtd
                    end if
                    if (tdx/=0) then
                        if (fn==fnTeacherClasses) then
                            write(device,AFORMAT) begintd//trim(Teacher(tdx)%Name)//endtd
                        else
                            write(device,AFORMAT) trim(make_href(fnTeacherClasses, Teacher(tdx)%Name, &
                                A1=Teacher(tdx)%TeacherID, A9=thisTerm, pre=begintd, post=endtd))
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
                                    A1=Room(rdx)%Code, A9=thisTerm, pre=begintd))
                            end if
                        else
                            write(device,AFORMAT) begintd//red//trim(Room(rdx)%Code)//black//endtd
                        end if
                        if (tdx/=0) then
                            if (fn==fnTeacherClasses) then
                                write(device,AFORMAT) begintd//trim(Teacher(tdx)%Name)
                            else
                                write(device,AFORMAT) trim(make_href(fnTeacherClasses, Teacher(tdx)%Name, &
                                    A1=Teacher(tdx)%TeacherID, A9=thisTerm, pre=begintd))
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
                write(device,AFORMAT) begintd//beginsmall
                if ( okToAdd ) then
                    write(device,AFORMAT) trim(make_href(fnScheduleEdit, ' Edit', A1=QUERY_put, &
                        A9=thisTerm ))
                    if (fn/=fnTeacherClasses) then
                        write(device,AFORMAT) trim(make_href(fnScheduleDelete, ' Del', A1=QUERY_put, &
                            A9=thisTerm ))
                        if (isLecture) write(device,AFORMAT) trim(make_href(fnScheduleAddLab, 'Add lab', &
                            A1=QUERY_put, A9=thisTerm ))
                    end if
                else
                    write(device,AFORMAT) nbsp
                end if
                write(device,AFORMAT) endsmall//endtd//endtr

                ! correct no. of hours?
                if (.not. is_consistent_section_hours_with_subject_defn(Section(sdx), thisTerm)) then
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
        write(device,AFORMAT) endtable//linebreak
        if (fn==fnTeacherClasses) then
            call timetable_meetings_of_teacher(NumSections, Section, targetTeacher, 0, nsections, tArray, TimeTable, conflicted)
            if (nsections>0) call timetable_display(device, Section, TimeTable)
        end if
        write(device,AFORMAT) horizontal

    end subroutine section_list_classes



    subroutine section_list_all (device)

        integer, intent (in) :: device

        integer :: cdx, idx, jdx, i, sdx, ncol, nclosed, maxcol=7, nopen, nsections
        integer :: owner_dept, owner_coll, previous, term, tTerm, tYear
        character(len=127) :: header
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject, tArea
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=20) :: preText, postText
        logical :: okToAdd

        call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, i)
        targetDepartment = index_to_dept(tDepartment)
        targetCollege = Department(targetDepartment)%CollegeIdx
        tCollege =  College(targetCollege)%Code

        call html_write_header(device, SPACE)

        ! loop for all terms
        do tTerm=termBegin,termEnd

            call qualify_term(tTerm, tYear, term, header)
            write(device,AFORMAT) '<h3>Classes in '//trim(tDepartment)//' for '//trim(header)//'</h3>'

            ! make list of sections
            nsections = 0
            do sdx=1,NumSections(term)
                cdx = Section(term,sdx)%SubjectIdx
                if (cdx==0) cycle ! section was deleted
                owner_dept = Section(term,sdx)%DeptIdx
                owner_coll = Department(owner_dept)%CollegeIdx
                if (owner_dept==targetDepartment) then
                    nsections = nsections + 1
                    tArray(nsections) = sdx
                end if

            end do ! sdx=1,NumSections

            if (nsections==0) write(device,AFORMAT) BRNONE

            ! sort sections by class id
            do idx=1,nsections-1
                do jdx=idx+1,nsections
                    if (Section(term,tArray(jdx))%ClassId<Section(term,tArray(idx))%ClassId) then
                        ncol = tArray(jdx)
                        tArray(jdx) = tArray(idx)
                        tArray(idx) = ncol
                    end if
                end do
            end do

            ! make list subjects
            nopen = 0
            previous = 0
            do idx=1,nsections
                cdx = Section(term,tArray(idx))%SubjectIdx
                if (previous/=cdx) then
                    nopen = nopen+1
                    tArray(nsections+nopen) = cdx
                    previous = cdx
                end if
            end do

            ! write shortcut to subjects
            write(device,AFORMAT) '<table border="0" width="100%">'
            ncol = 0
            do idx=1,nopen
                tSubject = Subject(tArray(nsections+idx))%Name
                i = index(tSubject, DASH)
                if (tSubject(1:3) /= 'PE ' .and. i > 0) cycle
                tArea = get_area(tSubject)
                ncol = ncol + 1

                if (ncol == 1) then
                    preText = begintr//begintd
                    postText = endtd
                else if (ncol == maxcol) then
                    preText = begintd
                    postText = endtd//endtr
                    ncol = 0
                else
                    preText = begintd
                    postText = endtd
                end if

                write(device,AFORMAT) trim(make_href(fnScheduleByArea, tSubject, A1=tCollege, A2=tArea, A9=term, &
                    pre=trim(preText), post=postText, anchor=tSubject))
            end do
            if (ncol /= 0)  then
                do i=ncol+1,maxcol
                    write(device,AFORMAT) tdnbspendtd
                end do
                write(device,AFORMAT) endtr
            end if
            write(device,AFORMAT) endtable

            nclosed = 0
            ! make list of closed subjects here, starting at tArray(nsections+nopen+1)
#if defined REGIST
            do cdx=1,NumSubjects+NumAdditionalSubjects
                if (Offering(term,cdx)%NSections>0) cycle
                if (Subject(cdx)%DeptIdx/=targetDepartment) cycle
                nclosed = nclosed+1
                tArray(nsections+nopen+nclosed) = cdx
            end do
#else
            ! Subjects administered by program
            do cdx=1,NumSubjects+NumAdditionalSubjects
                if (Offering(term,cdx)%NSections>0) cycle
                if (.not. is_used_in_college_subject(targetCollege, cdx)) cycle
                nclosed = nclosed+1
                tArray(nsections+nopen+nclosed) = cdx
            end do
#endif

            ! offer to open sections
            okToAdd = is_admin_of_college(targetCollege) .or. &
                ( is_chair_of_department(targetDepartment,orHigherUp) .and. &
                  ( (term==currentTerm .and. isPeriodOne) .or. &
                    term==nextTerm ) ) ! (term==nextTerm .and. (.not. isPeriodOne)) ) )

            if (nclosed>0 .and. okToAdd) then
                call make_form_start(device, fnScheduleOfferSubject, A2=Department(targetDepartment)%Code, A9=term)
                write(device,AFORMAT) &
                    '<table border="0" width="100%">'//begintr//'<td colspan="'//trim(itoa(maxcol))//'" align="right">', &
                    'Open a section in <select name="A1"> <option value=""> (select subject)'
                do idx=1,nclosed
                    tSubject = Subject(tArray(nsections+nopen+idx))%Name
                    write(device,AFORMAT) '<option value="'//trim(tSubject)//'"> '//tSubject
                end do
                write(device,AFORMAT) '</select> '//nbsp//nbsp//' <input type="submit" value="Submit">'// &
                endtd//endtr//endtable//endform
            end if

            write(device,AFORMAT) horizontal

        end do

    end subroutine section_list_all


end module EditSECTIONS
