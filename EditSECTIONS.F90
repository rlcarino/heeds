!======================================================================
!
!    HEEDS (Higher Education Enrollment Decision Support) - A program
!      to create enrollment scenarios for 'next term' in a university
!    Copyright (C) 2012-2015 Ricolindo L. Carino
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


    subroutine section_offer_subject(device, thisTerm)
        integer, intent (in) :: device, thisTerm
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=MAX_LEN_SECTION_CODE) :: tSection
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer :: iSubj, kdx, dept
        character (len=255) :: mesg
        logical :: criticalErr

        call html_comment('section_offer_subject()')

        ! what subject to offer ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tSubject, iSubj)
        if (iSubj/=0 .or. tSubject==SPACE) then
            mesg = 'Subject to offer not specified?'
        else
            iSubj = index_to_subject(tSubject)
            mesg = 'In section_offer_subject: Subject code '//tSubject//' is invalid?'
        end if
        if (iSubj<=0) then ! subject code is invalid
            targetCollege = CollegeIdxUser
            targetDepartment = DeptIdxUser
            call html_write_header(device, 'Open a section', linebreak//horizontal//mesg)
            return
        end if

        call check_array_bound (NumSections(thisTerm)+2, MAX_ALL_SECTIONS, 'MAX_ALL_SECTIONS', criticalErr)
        if (criticalErr) then
            targetDepartment = DeptIdxUser
            targetCollege = CollegeIdxUser
            call html_write_header(device, 'Open a section', linebreak//horizontal//'No more space for additional section(s)')
            return
        end if

#if defined UPLB
        ! Subject administered by unit
        targetDepartment = Subject(iSubj)%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx
        tDepartment = Department(targetDepartment)%Code
#else
        ! Subjects administered by program
        call cgi_get_named_string(QUERY_STRING, 'A2', tDepartment, kdx)
        targetDepartment = index_to_dept(tDepartment)
        targetCollege = Department(targetDepartment)%CollegeIdx
#endif
        dept = targetDepartment
        kdx = ScheduleCount(thisTerm,targetDepartment) + 1 ! new section in unit
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
            mesg = '"Open a section in '//trim(tSubject)//'" failed. '//sorryMessageOfficial

        else

            if (Subject(iSubj)%LectHours>0) then ! subject has lecture
                NumSections(thisTerm) = NumSections(thisTerm)+1
                Section(thisTerm,NumSections(thisTerm)) = TYPE_SECTION (trim(Subject(iSubj)%Name)//SPACE//tSection, &
                    tSection, SPACE, &
                    targetDepartment, iSubj, Subject(iSubj)%MaxLectSize, Subject(iSubj)%MaxLectSize, 1, 0, 0, 0, 0, 0)
                call class_details_write(unitXML, thisTerm, dirCLASSES(thisTerm), NumSections(thisTerm))
            end if

            if (Subject(iSubj)%LabHours>0) then ! subject has lab/recitation
                NumSections(thisTerm) = NumSections(thisTerm)+1
                tSection = trim(tSection)//DASH//'1L'
                Section(thisTerm,NumSections(thisTerm)) = TYPE_SECTION (trim(Subject(iSubj)%Name)//SPACE//tSection, &
                    tSection, SPACE, &
                    targetDepartment, iSubj, Subject(iSubj)%MaxLabSize, Subject(iSubj)%MaxLabSize, 1, 0, 0, 0, 0, 0)
                call class_details_write(unitXML, thisTerm, dirCLASSES(thisTerm), NumSections(thisTerm))
            end if

            call class_details_write(unitXML, thisTerm, indexCLASSES(thisTerm), 1, NumSections(thisTerm))

            call offerings_summarize(thisTerm)

            mesg = 'Opened a section in '//tSubject

        end if

        call section_list_classes (device, thisTerm, fnScheduleByArea, dept, tSubject, mesg)

    end subroutine section_offer_subject


    subroutine section_add_laboratory(device, thisTerm)
        integer, intent (in) :: device, thisTerm
        character(len=MAX_LEN_CLASS_ID) :: tClassId
        character(len=MAX_LEN_SECTION_CODE) :: tSection
        character (len=255) :: mesg
        integer :: iSubj, iSect, dept
        logical :: criticalErr

        call html_comment('section_add_laboratory()')

        call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, iSect)
        if (iSect/=0 .or. tClassId==SPACE) then
            mesg = 'Lecture section not specified?'
        else
            iSect = index_to_section(tClassId, thisTerm)
            mesg = 'Section "'//tClassId//'" not found?'
        end if
        if (iSect<=0) then ! section is invalid
            targetCollege = CollegeIdxUser
            targetDepartment = DeptIdxUser
            call html_write_header(device, 'Add lab', linebreak//horizontal//mesg)
            return
        end if

        call check_array_bound (NumSections(thisTerm)+1, MAX_ALL_SECTIONS, 'MAX_ALL_SECTIONS', criticalErr)
        if (criticalErr) then
            targetDepartment = DeptIdxUser
            targetCollege = CollegeIdxUser
            call html_write_header(device, 'Open a section', linebreak//horizontal//'No more space for additional section(s)')
            return
        end if

        dept = Section(thisTerm,iSect)%DeptIdx
        targetDepartment = dept
        targetCollege = Department(dept)%CollegeIdx
        iSubj = Section(thisTerm,iSect)%SubjectIdx
        tSection = get_next_lab_section(iSect, thisTerm)

        if (isRoleOfficial) then
            mesg = '"Open new section '//trim(Subject(iSubj)%Name)//SPACE//trim(tSection)//'" failed. '//sorryMessageOfficial

        else

            NumSections(thisTerm) = NumSections(thisTerm)+1
            Section(thisTerm,NumSections(thisTerm)) = TYPE_SECTION (trim(Subject(iSubj)%Name)//SPACE//tSection, tSection, SPACE, &
                targetDepartment, iSubj, Subject(iSubj)%MaxLabSize, Subject(iSubj)%MaxLabSize, 1, 0, 0, 0, 0, 0)
            !write(*,*) 'Adding '//trim(Subject(iSubj)%Name)//SPACE//tSection

            call class_details_write(unitXML, thisTerm, dirCLASSES(thisTerm), NumSections(thisTerm))

            call class_details_write(unitXML, thisTerm, indexCLASSES(thisTerm), 1, NumSections(thisTerm))

            call offerings_summarize(thisTerm)
            mesg = 'Opened new section '//trim(Subject(iSubj)%Name)//SPACE//tSection

        end if

        call section_list_classes (device, thisTerm, fnScheduleByArea, dept, Subject(iSubj)%Name, mesg)

    end subroutine section_add_laboratory


    subroutine section_delete(device, thisTerm)
        integer, intent (in) :: device, thisTerm
        character(len=MAX_LEN_CLASS_ID) :: tClassId
        integer :: iSect, iSubj, pos, i, dept
        character (len=127) :: mesg

        call html_comment('section_delete()')

        call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, iSect)
        if (iSect/=0 .or. tClassId==SPACE) then
            mesg = 'Section to delete not specified?'
        else
            iSect = index_to_section(tClassId, thisTerm)
            mesg = 'Section "'//tClassId//'" not found?'
        end if
        if (iSect<=0) then ! section is invalid
            targetCollege = CollegeIdxUser
            targetDepartment = DeptIdxUser
            call html_write_header(device, 'Delete section', linebreak//horizontal//mesg)
            return
        end if

        dept = Section(thisTerm,iSect)%DeptIdx
        targetDepartment = dept
        targetCollege = Department(dept)%CollegeIdx

        pos = 0 ! how many students affected
        do i=1,NumStudents+NumAdditionalStudents
            do iSubj=1,Student(i)%Enlistment(thisTerm)%lenSubject
                if (Student(i)%Enlistment(thisTerm)%Section(iSubj)==iSect) pos = pos+1
            end do
        end do

        ! remember subject
        iSubj = Section(thisTerm,iSect)%SubjectIdx

        if (pos>0) then

            mesg = '"Delete section '//trim(tClassId)//'" failed. '//trim(itoa(pos))// &
                ' students enlisted.'

        else if (isRoleOfficial) then

            mesg = '"Delete section '//trim(tClassId)//'" failed. '//sorryMessageOfficial

        else

            mesg = 'Deleted section '//tClassId

            if (.not. isSubject_lecture_lab(iSubj)) then ! lecture only, or lab only
                call delete_section_from_blocks(iSect, thisTerm)
            else ! lecture-lab subject
                if (is_lecture_class(iSect, thisTerm)) then ! remove lecture and lab sections
                    call delete_section_from_blocks(iSect, thisTerm)
                    tClassId = trim(tClassId)//DASH
                    i = len_trim(tClassId)
                    do pos=1,NumSections(thisTerm)
                        if (Section(thisTerm,pos)%ClassId(:i)/=tClassId(:i)) cycle
                        call delete_section_from_blocks(pos, thisTerm)
                    end do
                else ! remove this section only
                    call delete_section_from_blocks(iSect, thisTerm)
                end if
            end if
            do i=1,NumBlocks(thisTerm)
                if (Block(thisTerm,i)%isDirty) then
                    call block_details_write(unitXML, thisTerm, dirBLOCKS(thisTerm), i)
                    Block(thisTerm,i)%isDirty = .false.
                end if
            end do
            call class_details_write(unitXML, thisTerm, indexCLASSES(thisTerm), 1, NumSections(thisTerm))
            call offerings_summarize(thisTerm)
            call count_sections_by_dept(thisTerm)

        end if

        call section_list_classes (device, thisTerm, fnScheduleByArea, dept, Subject(iSubj)%Name, trim(mesg))

    end subroutine section_delete


    subroutine section_validate_inputs(device, thisTerm)
        integer, intent (in) :: device, thisTerm
        integer :: action_index, ierr, iSect
        integer :: teacher_dept, room_dept, dept
        type (TYPE_SECTION) :: wrk
        character(len=MAX_LEN_CLASS_ID) :: tClassId, tAction
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character (len=255) :: errMesg
        character (len=80) :: header

        call html_comment('section_validate_inputs()')
        errMesg = SPACE

        call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, ierr)
        iSect = index_to_section(tClassId, thisTerm)

        ! check if section was deleted previously
        if (ierr/=0 .or. iSect==0) then ! not found
            targetCollege = CollegeIdxUser
            targetDepartment = DeptIdxUser
            header = College(targetCollege)%Code//'- '//College(targetCollege)%Name
            errMesg = '"Edit '//trim(tClassId)//'" failed because it was previously deleted.'
            call html_write_header(device, header, errMesg)
            call html_college_info(device, targetCollege)
            return
        end if

        dept = Section(thisTerm,iSect)%DeptIdx
        targetDepartment = dept
        targetCollege = Department(dept)%CollegeIdx
        room_dept = 0
        teacher_dept = 0

        if (REQUEST==fnScheduleEdit) then
            header = 'Edit section '//tClassId
            action_index = 1
            wrk = Section(thisTerm,iSect)

        else ! (REQUEST==fnScheduleValidate) then
            header = 'Proposed changes to section '//tClassId

            ! extract section info from QUERY
            call section_build_from_query (thisTerm, iSect, wrk)

            ! action is ?
            call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
            select case (trim(tAction))

                case ('Confirm') ! Accept previously validated edits
                    if (isROleOfficial) then
                        errMesg = '"Edit '//trim(tClassId)//'" failed. '//sorryMessageOfficial
                    else
                        header = 'Finished editing '//trim(tClassId)
                        if (Section(thisTerm,iSect)%Code/=wrk%Code) call class_details_write(unitXML, thisTerm, &
                            indexCLASSES(thisTerm), 1, NumSections(thisTerm))
                        Section(thisTerm,iSect) = wrk
                        call class_details_write(unitXML, thisTerm, dirCLASSES(thisTerm), iSect)
                        call offerings_summarize(thisTerm)
                        call count_sections_by_dept(thisTerm)
                        call section_list_classes (device, thisTerm, fnScheduleByArea, dept, tClassId, header)
                        return
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
        call section_validation_form(device, thisTerm, action_index, iSect, wrk, teacher_dept, room_dept)

    end subroutine section_validate_inputs


    subroutine section_write_edit_form(device, thisTerm, iSect, tSection, teacher_dept, room_dept)
        integer, intent (in) :: device, thisTerm, iSect, teacher_dept, room_dept
        type (TYPE_SECTION), intent (in) :: tSection
        integer :: tdx, idx, tLen, iTeach, iRoom
        integer :: iMeet, idx_select, idx_ampm
        integer :: DayIdx, bTimeIdx, eTimeIdx, RoomIdx, TeacherIdx
        !integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        character(len=32) :: tHours

        call html_comment('section_write_edit_form()')

        tLen = len_trim(Section(thisTerm,iSect)%Code)+1
        if (is_lecture_class(iSect,thisTerm)) then
            tHours = trim(ftoa(Subject(Section(thisTerm,iSect)%SubjectIdx)%LectHours,2))//' lecture hours'
        else
            tHours = trim(ftoa(Subject(Section(thisTerm,iSect)%SubjectIdx)%LabHours,2))//' laboratory hours'
        end if

        ! write input form to capture edits to Section(thisTerm,iSect); previous inputs are in tSection
        call make_form_start(device, fnScheduleValidate, Section(thisTerm,iSect)%ClassId, A9=thisTerm)

        write(device,AFORMAT) &
            b_bold//'SECTION CODE'//e_bold, &
            trim(Section(thisTerm,iSect)%Code)//nbsp//nbsp//b_italic//'change to '//e_italic, &
            nbsp//'<input size="'//trim(itoa(tLen))//'" name="code" value="'//trim(tSection%Code)//'">', &
            nbsp//nbsp//nbsp//nbsp//nbsp//nbsp, &
            b_bold//'NO. OF STUDENTS'//e_bold//nbsp//nbsp//trim(itoa(Section(thisTerm,iSect)%Slots))// &
            nbsp//nbsp//b_italic//'change to '//e_italic, &
            nbsp//'<input size="3" name="slots" value="'//trim(itoa(tSection%Slots))//'">'
        if (thisTerm/=3) write(device,AFORMAT) linebreak, & ! not summer
            b_italic//'(Note: Class meetings must total  '//b_bold//trim(tHours)//e_bold//') :'//e_italic

        write(device,AFORMAT) '<table border="0" width="100%">'//b_tr, &
            '<td align="left">'//b_bold//'Meeting'//e_bold//e_td//&
            '<td align="left">'//b_bold//'Day'//e_bold//e_td// &
            '<td align="left">'//b_bold//'Begin'//e_bold//e_td//&
            '<td align="left">'//b_bold//'End'//e_bold//e_td// &
            '<td align="left">'//b_bold//'Room'//e_bold//e_td//&
            '<td align="left">'//b_bold//'Teacher'//e_bold//e_td//e_tr
        do iMeet=1,tSection%NMeets+3
            DayIdx = tSection%DayIdx(iMeet)
            bTimeIdx = tSection%bTimeIdx(iMeet)
            eTimeIdx = tSection%eTimeIdx(iMeet)
            RoomIdx = tSection%RoomIdx(iMeet)
            TeacherIdx = tSection%TeacherIdx(iMeet)
            if (iMeet<=tSection%NMeets) then
                write(device,AFORMAT) b_tr//b_tdac//trim(itoa(iMeet))//e_td
            else
                write(device,AFORMAT) b_tr//'<td align="center">(Add)'//e_td
            end if
            write(device,AFORMAT) b_td//'<select name="day'//trim(itoa(iMeet))//'">'
            do idx=0,7
                if (idx/=DayIdx) then
                    idx_select = 0
                else
                    idx_select = 1
                end if
                write(device,AFORMAT) '<option value="'//trim(itoa(idx))//'"'// &
                    trim(selected(idx_select))//'> '//txtDay(idx)
            end do
            write(device,AFORMAT) '</select>'//e_td//b_td//'<select name="btime'//trim(itoa(iMeet))//'"><option value="0"> '
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
            write(device,AFORMAT) '</select>'//e_td//b_td//'<select name="etime'//trim(itoa(iMeet))//'"><option value="0"> '
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
            write(device,AFORMAT) '</select>'//e_td//b_td//'<select name="room'//trim(itoa(iMeet))//'"><option value="0"> '
            do iRoom=1,NumRooms+NumAdditionalRooms
                if (iRoom/=RoomIdx) then
                    if ( isRole_chair_of_department(Room(iRoom)%DeptIdx, orHigherUp) .and. Room(iRoom)%DeptIdx==room_dept ) then
                        write(device,AFORMAT) '<option value="'//trim(Room(iRoom)%Code)//'"> '//trim(Room(iRoom)%Code)
                    end if
                else
                    write(device,AFORMAT) '<option value="'//trim(Room(iRoom)%Code)//'" selected="selected"> '// &
                    trim(Room(iRoom)%Code)
                end if
            end do
            write(device,AFORMAT) '</select>'//e_td//b_td//'<select name="teacher'// &
                trim(itoa(iMeet))//'"><option value="0"> '
            do tdx=1,NumTeachers+NumAdditionalTeachers
                iTeach = TeacherRank(tdx)
                if (iTeach/=TeacherIdx) then
                    if ( isRole_chair_of_department(Teacher(iTeach)%DeptIdx, orHigherUp) .and. &
                         Teacher(iTeach)%DeptIdx==teacher_dept ) then
                        write(device,AFORMAT) '<option value="'//trim(Teacher(iTeach)%TeacherId)//'"> '//trim(Teacher(iTeach)%Name)
                    end if
                else
                    write(device,AFORMAT) '<option value="'//trim(Teacher(iTeach)%TeacherId)//'" selected="selected"> '// &
                        trim(Teacher(iTeach)%Name)
                end if
            end do
            write(device,AFORMAT) '</select>'//e_td//e_tr

        end do
        write(device,AFORMAT) e_table, linebreak, &
            b_italic//'If you made a change above, '//e_italic//nbsp//'<input type="submit" name="action" value="Validate">', &
            e_form//horizontal

    end subroutine section_write_edit_form


    subroutine section_build_from_query (thisTerm, section_index, tSection)
        integer, intent(in) :: thisTerm, section_index
        type (TYPE_SECTION), intent(out) :: tSection
        integer :: cgi_err, iSubj, idx_meet, jdx, iMeet
        character (len=3*MAX_LEN_ROOM_CODE) :: tRoom
        character(len=3*MAX_LEN_USERNAME) :: tLogin
        character(len=3*MAX_LEN_SECTION_CODE) :: tCode

        call html_comment('section_build_from_query()')

        call initialize_section(tSection)
        iSubj = Section(thisTerm,section_index)%SubjectIdx
        tSection%DeptIdx = Section(thisTerm,section_index)%DeptIdx
        tSection%SubjectIdx = iSubj

        ! section code & class ID
        call cgi_get_named_string(QUERY_STRING, 'code', tCode, cgi_err)
        tSection%Code = tCode
        tSection%ClassId = trim(Subject(iSubj)%Name)//SPACE//tSection%Code

        ! no. of students
        call cgi_get_named_integer(QUERY_STRING, 'slots', tSection%Slots, cgi_err)

        ! class meetings
        idx_meet = 0
        do iMeet=1,MAX_SECTION_MEETINGS
            call cgi_get_named_integer(QUERY_STRING, 'day'//trim(itoa(iMeet)), jdx, cgi_err)
            tSection%DayIdx(idx_meet+1) = max(jdx,0)
            call cgi_get_named_integer(QUERY_STRING, 'btime'//trim(itoa(iMeet)), jdx, cgi_err)
            tSection%bTimeIdx(idx_meet+1) = max(jdx,0)
            call cgi_get_named_integer(QUERY_STRING, 'etime'//trim(itoa(iMeet)), jdx, cgi_err)
            tSection%eTimeIdx(idx_meet+1) = max(jdx,0)
            call cgi_get_named_string(QUERY_STRING, 'room'//trim(itoa(iMeet)), tRoom, cgi_err)
            if (cgi_err==0) tSection%RoomIdx(idx_meet+1) = index_to_room(tRoom)
            call cgi_get_named_string(QUERY_STRING, 'teacher'//trim(itoa(iMeet)), tLogin, cgi_err)
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



    subroutine section_validation_form(device, thisTerm, action_index, section_index, wrk, teacher_dept, room_dept)
        integer, intent (in) :: device, thisTerm, action_index, section_index, teacher_dept, room_dept
        type (TYPE_SECTION), intent(in) :: wrk
        integer :: ierr, ddx, idx, jdx, mdx, idx_select, tLen, idxWrk, bdx
        integer, dimension(60,7) :: TimeTable
        logical :: conflict_teacher, conflict_room, flagIsUp, conflict_student, conflict_block
        integer :: count_in_class, count_affected, iStd, iTeach, iRoom, iMeet, iSubj, iSect

        call html_comment('section_validation_form()', &
            ' action_index='//itoa(action_index), &
            ' room_dept='//itoa(room_dept), &
            ' teacher_dept='//itoa(teacher_dept))

        call section_write_edit_form(device, thisTerm, section_index, wrk, teacher_dept, room_dept)
        iSubj = Section(thisTerm,section_index)%SubjectIdx
        ierr = 0

        ! section code & class ID
        if (wrk%Code==SPACE) then ! code NOT specified
            ierr = ierr+1
            write(device,AFORMAT) red//'  Section code not specified?'//e_color//linebreak
        else ! code specified
            iSect = index_to_section(wrk%ClassId, thisTerm)
            if (iSect>0 .and. iSect/=section_index) then
                ierr = ierr+1
                write(device,AFORMAT) red//'  Class ID '//wrk%ClassId//' already in use?'//e_color//linebreak
            end if
        end if
        ! no. of students
        if (wrk%Slots<=0) then
            ierr = ierr+1
            write(device,AFORMAT) red//'  Number of students must be more than 0?'//e_color//linebreak
        end if

        ! correct no. of hours?
        if (.not. is_consistent_section_hours_with_subject_defn(wrk, thisTerm)) then
            ierr = ierr+1
            write(device,AFORMAT) red//'Total meeting hours is inconsistent with subject definition hours?'//e_color//linebreak
        end if

        ! meeting conflicts?
        if (.not. is_conflict_free_section_hours(wrk, thisTerm)) then
            ierr = ierr+1
            write(device,AFORMAT) red//'Conflict in meeting times; or, if a lecture-lab subject, '// &
            'conflict with lecture section?'//e_color//linebreak
        end if

        ! check room conflict for each meeting
        do iMeet=1,wrk%NMeets
            iRoom = wrk%RoomIdx(iMeet)
            if (iRoom==0 .or. Room(iRoom)%Code=='TBA') cycle ! none assigned yet
            call timetable_meetings_in_room(thisTerm, iRoom, section_index, tLen, tArray, TimeTable, conflict_room)
            if (conflict_room) then
                ierr = ierr+1
                write(device,AFORMAT) red//'  Prior conflict of classes in room '//trim(Room(iRoom)%Code)//e_color//linebreak
            else
                if (is_conflict_timetable_with_struct_section(wrk, iMeet, iMeet, TimeTable)) then
                    conflict_room = .true.
                    ierr = ierr+1
                    write(device,AFORMAT) red//'  Meeting '//itoa(iMeet)//' conflicts with classes in room '// &
                        trim(Room(iRoom)%Code)//e_color//linebreak
                end if
            end if
          !if (isRoleChair .and. Room(iRoom)%DeptIdx/=DeptIdxUser) then
          !    write(device,AFORMAT) red//'  '//trim(Department(DeptIdxUser)%Code)//' cannot assign classes to room '// &
          !      trim(Room(iRoom)%Code)//'@'//trim(Department(Room(iRoom)%DeptIdx)%Code)//e_color//linebreak
          !    ierr = ierr+1
          !end if
        end do

        ! check if room capacity exceeded
        do iMeet=1,wrk%NMeets
            iRoom = wrk%RoomIdx(iMeet)
            if (iRoom==0 .or. Room(iRoom)%Code=='TBA') cycle ! none assigned yet
            if (Room(iRoom)%MaxCapacity<wrk%Slots) then
                ierr = ierr+1
                write(device,AFORMAT) red//'  Class size exceeds capacity of room '//trim(Room(iRoom)%Code)//e_color//linebreak
            end if
        end do

        ! check teacher conflict for each meeting
        do iMeet=1,wrk%NMeets
            iTeach = wrk%TeacherIdx(iMeet)
            if (iTeach==0 .or. Teacher(iTeach)%TeacherId=='TBA)') cycle
            call timetable_meetings_of_teacher(thisTerm, iTeach, section_index, &
                tLen, tArray, TimeTable, conflict_teacher)
            if (conflict_teacher) then ! teacher has conflicted schedule
                ierr = ierr+1
                write(device,AFORMAT) red//'  Prior conflict in teaching schedule of '// &
                    trim(Teacher(iTeach)%Name)//e_color//linebreak
            else
                if (is_conflict_timetable_with_struct_section(wrk,iMeet,iMeet,TimeTable)) then
                    conflict_teacher = .true.
                    ierr = ierr+1
                    write(device,AFORMAT) red//'  Meeting '//itoa(iMeet)//' conflicts with teaching schedule of '// &
                        trim(Teacher(iTeach)%Name)//e_color//linebreak
                end if
            end if
          !if (isRoleChair .and. Teacher(iTeach)%DeptIdx/=DeptIdxUser) then
          !    write(device,AFORMAT) red//'  '//trim(Department(DeptIdxUser)%Code)//' cannot assign classes to '// &
          !      trim(Teacher(iTeach)%Name)//' of '//trim(Department(Teacher(iTeach)%DeptIdx)%Code)//e_color//linebreak
          !    ierr = ierr+1
          !end if
        end do

        ! who are the students in the class?
        call collect_students_in_section (thisTerm, section_index, count_in_class, tArray)
        call html_comment(itoa(count_in_class)//'students in '//Section(thisTerm,section_index)%ClassId)
        if (count_in_class>0 .and. wrk%ClassId/=Section(thisTerm,section_index)%ClassId) then
            ierr = ierr+1
            write(device,AFORMAT) red//'  Proposed class code '//wrk%Code// &
                '  will cause the following students to be delisted:'//e_color//linebreak
            call html_student_list (device, count_in_class, tArray, &
                isRole_dean_of_college(Department(Section(thisTerm,section_index)%DeptIdx)%CollegeIdx, orHigherUp), SPACE)
        end if

        ! how many with messed schedules
        count_affected = 0
        do idx=1,count_in_class
            iStd = tArray(idx)
            call timetable_meetings_of_student(thisTerm, iStd, section_index, & ! skip target section
                tLen, tArray(count_in_class+1:), TimeTable, conflict_student)
            if (is_conflict_timetable_with_struct_section(wrk, 1, wrk%NMeets, TimeTable)) then
                count_affected = count_affected + 1
                tArray(count_affected) = iStd ! move to beginning of list
                call html_comment(text_student_curriculum(iStd))
            end if
        end do
        if (count_affected>0) then ! schedule conflicts
            ierr = ierr + 1
            write(device,AFORMAT) red//'  Proposed class times '
            do iMeet=1,wrk%NMeets
                write(device,AFORMAT) trim(itoa(iMeet))//DOT//nbsp//txtDay(wrk%DayIdx(iMeet))//nbsp// &
                    trim(text_time_period(wrk%bTimeIdx(iMeet), wrk%eTimeIdx(iMeet)))//nbsp
            end do
            write(device,AFORMAT) '  will cause schedule conflicts for the following students:'//e_color//linebreak
            call html_student_list (device, count_affected, tArray, &
                isRole_dean_of_college(Department(Section(thisTerm,section_index)%DeptIdx)%CollegeIdx, orHigherUp), SPACE)
        end if

        ! check for possible block conflicts
        do bdx=1,NumBlocks(thisTerm)
            do jdx=1,Block(thisTerm,bdx)%NumClasses
                if (Block(thisTerm,bdx)%Section(jdx)/=section_index) cycle
                ! collect meetings of block
                call timetable_meetings_of_block(thisTerm, bdx, section_index, tLen, tArray, TimeTable, conflict_block)
                if (conflict_block) then
                    ierr = ierr + 1
                    write(device,AFORMAT) red//'  Prior conflict of classes in block '//trim(Block(thisTerm,bdx)%BlockID)// &
                        e_color//linebreak
                else
                    if (is_conflict_timetable_with_struct_section(wrk, 1, wrk%NMeets, TimeTable)) then
                        ierr = ierr + 1
                        conflict_block = .true.
                        write(device,AFORMAT) red//'  Proposed class times conflict with classes in block '// &
                            trim(Block(thisTerm,bdx)%BlockID)//e_color//linebreak
                    end if
                end if
                exit
            end do
        end do

        if (ierr>0) write(device,AFORMAT) horizontal

        ! common form inputs
        idxWrk = MAX_ALL_SECTIONS ! temporary work area
        Section(thisTerm,idxWrk) = wrk
        call make_form_start(device, fnScheduleValidate, Section(thisTerm,section_index)%ClassId, A9=thisTerm)
        write(device,AFORMAT) &
            '<input type="hidden" name="code" value="'//trim(wrk%Code)//'">'// &
            '<input type="hidden" name="slots" value="'//trim(itoa(wrk%Slots))//'">'
        tLen = 0
        do iMeet=1,wrk%NMeets
            tArray(tLen+1) = idxWrk
            tArray(tLen+2) = iMeet
            tArray(tLen+3) = 0
            tLen = tLen+3
            write(device,AFORMAT) &
                '<input type="hidden" name="day'//trim(itoa(iMeet))//'" value="'//trim(itoa(wrk%DayIdx(iMeet)))//'">', &
                '<input type="hidden" name="btime'//trim(itoa(iMeet))//'" value="'//trim(itoa(wrk%bTimeIdx(iMeet)))//'">', &
                '<input type="hidden" name="etime'//trim(itoa(iMeet))//'" value="'//trim(itoa(wrk%eTimeIdx(iMeet)))//'">', &
                '<input type="hidden" name="room'//trim(itoa(iMeet))//'" value="'//trim(Room(wrk%RoomIdx(iMeet))%Code)//'">', &
                '<input type="hidden" name="teacher'//trim(itoa(iMeet))//'" value="'// &
                    trim(Teacher(wrk%teacherIdx(iMeet))%TeacherId)//'">'
        end do
        tArray(tLen+1) = 0
        tArray(tLen+2) = 0
        tArray(tLen+3) = 0

        if (ierr==0 .and. REQUEST==fnScheduleValidate) then ! nothing wrong?
            call list_sections_to_edit(device, thisTerm, tLen, tArray, 0, SPACE, SPACE, .false., .true., &
                b_italic//'Previous ''Validate'' found no fatal errors. Confirm the following data for '// &
                trim(Section(thisTerm,idxWrk)%ClassId)//' ?'//e_italic)
            write(device,AFORMAT) nbsp//nbsp//'<input type="submit" name="action" value="Confirm">', &
                horizontal
        end if
        call initialize_section(Section(thisTerm,idxWrk))

        ! no days or no hours?
        if (.not. is_TBA_day_or_hours(wrk)) then

            if (conflict_room .or. action_index==3) then ! tAction=='Find rooms') then
                call room_search_given_time(device, thisTerm, wrk, section_index, jdx, room_dept)
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
                call teacher_search_given_time(device, thisTerm, wrk, section_index, idx, teacher_dept)
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
            '<a name="sections"></a><table border="0" width="100%">'//b_tr, &
            b_td_nbsp_e_td//'<td align="right">', &
            !'[ Other '//trim(Subject(iSubj)%Name)//' <a href="#sections">sections</a> ] ', &
            '[ Usage of <a href="#rooms">room</a>(s) ] ', &
            '[ Classes of <a href="#teachers">teacher</a>(s) ] ', &
            '[ Other sections in <a href="#block">block</a> ] ', &
            '[ <a href="#TOP">TOP</a> ] ', &
            e_td//e_tr//e_table
        tLen = 0
        do iSect=1,NumSections(thisTerm)
            if (iSubj/=Section(thisTerm,iSect)%SubjectIdx) cycle ! not the same subject
            if (iSect==section_index) cycle ! exclude section being edited
            do iMeet=1,Section(thisTerm,iSect)%NMeets
                tArray(tLen+1) = iSect
                tArray(tLen+2) = iMeet
                tArray(tLen+3) = 0
                tLen = tLen+3
            end do
        end do
        tArray(tLen+1) = 0
        tArray(tLen+2) = 0
        tArray(tLen+3) = 0
        !write(*,*) tLen/3, ' other sections...', &
        !  (Section(thisTerm,tArray(3*(iSect-1)+1))%ClassId, iSect=1,tLen/3)
        call list_sections_to_edit(device, thisTerm, tLen, tArray, 0, SPACE, SPACE, .false., .true., &
            b_bold//'Other sections'//e_bold ) !, .true.)
        write(device,AFORMAT) horizontal

        ! show classes in the same block, if any
        flagIsUp = .false. ! no block
        do idx=1,NumBlocks(thisTerm)
            do jdx=1,Block(thisTerm,idx)%NumClasses
                if (Block(thisTerm,idx)%Section(jdx)/=section_index) cycle
                flagIsUp = .true.
                exit
            end do
        end do
        if (flagIsUp) then ! this section belongs to a block

            do bdx=1,NumBlocks(thisTerm)
                flagIsUp = .false. ! block not found
                do jdx=1,Block(thisTerm,bdx)%NumClasses
                    if (Block(thisTerm,bdx)%Section(jdx)/=section_index) cycle
                    flagIsUp = .true.
                    exit
                end do
                if (.not. flagIsUp) cycle

                ! collect meetings of block
                call timetable_meetings_of_block(thisTerm, bdx, section_index, tLen, tArray, TimeTable, conflict_block)
                if (tLen==0) cycle

                ! display block schedule
                write(device,AFORMAT) &
                    '<a name="blocks"><table border="0" width="100%">'//b_tr, &
                    b_td_nbsp_e_td//b_tdar, &
                    '[ Other '//trim(Subject(iSubj)%Name)//' <a href="#sections">sections</a> ] ', &
                    '[ Usage of <a href="#rooms">room</a>(s) ] ', &
                    '[ Classes of <a href="#teachers">teacher</a>(s) ] ', &
                    !'[ Other sections in <a href="#block">block</a> ] ', &
                    '[ <a href="#TOP">TOP</a> ] ', &
                    e_td//e_tr//e_table

                call list_sections_to_edit(device, thisTerm, tLen, tArray, 0, SPACE, SPACE, .false., .true., &
                    b_bold//'Proposed block schedule of '//trim(Block(thisTerm,bdx)%BlockID)//e_bold)

                ! add section meetings
                do mdx=1,wrk%NMeets
                    ddx = wrk%DayIdx(mdx)
                    if (ddx==0) cycle
                    do jdx = wrk%bTimeIdx(mdx), wrk%eTimeIdx(mdx)-1
                        if (TimeTable(jdx,ddx)==0) TimeTable(jdx,ddx) = -1
                    end do
                end do

                call timetable_display(device, thisTerm, TimeTable)
                write(device,AFORMAT) horizontal

            end do


        end if

        ! show classes in the same rooms, if any
        flagIsUp = .false. ! no room
        do iMeet=1,wrk%NMeets
            if (wrk%RoomIdx(iMeet)>0 .and. wrk%bTimeIdx(iMeet)>0 .and. &
                    wrk%DayIdx(iMeet)>0) then
                flagIsUp = .true.
                exit
            end if
        end do
        if (flagIsUp) then ! a room is used by this section

            flagIsUp = .false. ! none
            write(device,AFORMAT) &
                '<a name="rooms"></a><table border="0" width="100%">'//b_tr, &
                b_td_nbsp_e_td//b_tdar, &
                '[ Other '//trim(Subject(iSubj)%Name)//' <a href="#sections">sections</a> ] ', &
                !'[ Usage of <a href="#rooms">room</a>(s) ] ', &
                '[ Classes of <a href="#teachers">teacher</a>(s) ] ', &
                '[ Other sections in <a href="#block">block</a> ] ', &
                '[ <a href="#TOP">TOP</a> ] ', &
                e_td//e_tr//e_table
            do iMeet=1,wrk%NMeets
                iRoom = wrk%RoomIdx(iMeet)
                if (iRoom/=0) then
                    tLen = 0
                    do idx=1,iMeet-1 ! check if encountered previously
                        if (iRoom==wrk%RoomIdx(idx)) tLen = tLen + 1
                    end do
                    if (tLen==0) then ! not yet encountered
                        flagIsUp = .true.
                        ! collect classes in room iRoom
                        call timetable_meetings_in_room(thisTerm, iRoom, section_index, tLen, tArray, TimeTable, &
                            conflict_room)
                        call list_sections_to_edit(device, thisTerm, tLen, tArray, 0, SPACE, SPACE, .false., .true., &
                            b_bold//'Proposed class meetings in '//trim(Room(iRoom)%Code)//e_bold)

                        ! add section meetings
                        do mdx=1,wrk%NMeets
                            ddx = wrk%DayIdx(mdx)
                            if (ddx==0) cycle
                            if (wrk%RoomIdx(mdx)==iRoom) then
                                do jdx = wrk%bTimeIdx(mdx), wrk%eTimeIdx(mdx)-1
                                    !TimeTable(jdx,ddx) = -1
                                    if (TimeTable(jdx,ddx)==0) TimeTable(jdx,ddx) = -1
                                end do
                            end if
                        end do
                        if (tLen>0) call timetable_display(device, thisTerm, TimeTable)
                        write(device,AFORMAT) horizontal
                    end if
                end if
            end do
            if (.not. flagIsUp) then
                write(device,AFORMAT) b_bold//'Other class meetings in room(s)'//e_bold//BRNONEHR
            end if

        end if

        flagIsUp = .false. ! no teachers
        do iMeet=1,wrk%NMeets
            if (wrk%TeacherIdx(iMeet)>0 .and. wrk%bTimeIdx(iMeet)>0 .and. &
                    wrk%DayIdx(iMeet)>0) then
                flagIsUp = .true.
                exit
            end if
        end do
        if (flagIsUp) then ! a teacher is assigned to this section

            ! show classes of the teachers, if any
            flagIsUp = .false. ! none
            write(device,AFORMAT) &
                '<a name="teachers"></a><table border="0" width="100%">'//b_tr, &
                b_td_nbsp_e_td//b_tdar, &
                '[ Other '//trim(Subject(iSubj)%Name)//' <a href="#sections">sections</a> ] ', &
                '[ Usage of <a href="#rooms">room</a>(s) ] ', &
                !'[ Classes of <a href="#teachers">teacher</a>(s) ] ', &
                '[ Other sections in <a href="#block">block</a> ] ', &
                '[ <a href="#TOP">TOP</a> ] ', &
                e_td//e_tr//e_table
            do iMeet=1,wrk%NMeets
                iTeach = wrk%TeacherIdx(iMeet)
                if (iTeach/=0) then
                    tLen = 0
                    do idx=1,iMeet-1 ! check if encountered previously
                        if (iTeach==wrk%TeacherIdx(idx)) tLen = tLen + 1
                    end do
                    if (tLen==0) then ! not yet encountered
                        flagIsUp = .true.
                        ! collect classes of teacher
                        call timetable_meetings_of_teacher(thisterm, iTeach, section_index, tLen, tArray, TimeTable, &
                            conflict_teacher)
                        !write(*,*) tLen/3, ' other class meetings of teacher '//Teacher(iTeach)%Name
                        call list_sections_to_edit(device, thisTerm, tLen, tArray, 0, SPACE, SPACE, .false., .true., &
                            b_bold//'Proposed class meetings of '//trim(Teacher(iTeach)%Name)//e_bold)
                        ! add section meetings
                        do mdx=1,wrk%NMeets
                            ddx = wrk%DayIdx(mdx)
                            if (ddx==0) cycle
                            if (wrk%TeacherIdx(mdx)==iTeach) then
                                do jdx = wrk%bTimeIdx(mdx), wrk%eTimeIdx(mdx)-1
                                    !TimeTable(jdx,ddx) = -1
                                    if (TimeTable(jdx,ddx)==0) TimeTable(jdx,ddx) = -1
                                end do
                            end if
                        end do
                        if (tLen>0) call timetable_display(device, thisTerm, TimeTable)
                        write(device,AFORMAT) horizontal
                    end if
                end if
            end do
            if (.not. flagIsUp) then
                write(device,AFORMAT) b_bold//'Other classes of teacher(s)'//e_bold//BRNONEHR
            end if
        end if

        write(device,AFORMAT) e_form

    end subroutine section_validation_form



    subroutine teacher_search_given_time(device, thisTerm, wrk, to_skip, teacher_count, given_teacher_dept)
        integer, intent (in) :: to_skip, device, thisTerm
        integer, intent (out) :: teacher_count
        type (TYPE_SECTION), intent (in) :: wrk
        integer, intent (in), optional :: given_teacher_dept
        integer :: i, j, ierr, idx, iTeach, teacher_dept
        integer, dimension(60,7) :: TimeTable
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        character :: ch
        integer :: ncol
        logical :: skip

        if (present(given_teacher_dept)) then
            teacher_dept = given_teacher_dept
        else
            teacher_dept = Subject(wrk%SubjectIdx)%DeptIdx
        end if
        ierr = -10
        call timetable_clear(TimeTable)
        call timetable_add_struct_section(wrk, TimeTable, ierr)

        ! collect teachers into tArray()
        write(device,AFORMAT) b_bold//'Teachers in '//trim(Department(teacher_dept)%Code)//' available to teach'//e_bold
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
            b_tr//'<th align="left">Teacher</th><th align="left">Assigned classes</th>'//e_tr
        teacher_count = 0
        do idx=1,NumTeachers+NumAdditionalTeachers
            iTeach = TeacherRank(idx)
            if (Teacher(iTeach)%DeptIdx /= teacher_dept) cycle
            skip = .false.
            !tArray = 0 ! assigned classes
            ncol = 0
            do i=1,NumSections(thisTerm)
                if (Section(thisTerm,i)%SubjectIdx==0) cycle ! section was deleted
                if (i==to_skip) cycle
                call meetings_of_section_by_teacher(thisTerm, i, iTeach, n_meetings, meetings)
                if (n_meetings==0) cycle ! teacher not assigned to this section
                ncol = ncol+1
                tArray(ncol) = i
                if (is_conflict_timetable_with_section_meetings(thisTerm, i, n_meetings, meetings, TimeTable)) then
                    skip = .true. ! teacher not available
                    exit ! do not consider the remaining sections for this teacher
                end if
            end do
            if (skip) cycle ! done with this teacher
            teacher_count = teacher_count+1
            write(device,AFORMAT) trim(make_href(fnTeacherEditSchedule, Teacher(iTeach)%Name, &
                A1=Teacher(iTeach)%TeacherId, A9=thisTerm, &
                pre=b_tr//b_td, &
                post=e_td//b_td//'['//trim(itoa(ncol))//']'))
            do j=1,ncol
                write(device,AFORMAT) ' : '//Section(thisTerm,tArray(j))%ClassId
            end do
            write(device,AFORMAT) e_td//e_tr
        end do
        if (teacher_count == 0) write(device,AFORMAT) b_tr, &
            '<td colspan="2">'//b_italic//'(No teachers available during specified times, or time not specified)'// &
            e_italic//e_td, &
            e_tr
        write(device,AFORMAT) e_table

    end subroutine teacher_search_given_time


    subroutine room_search_given_time(device, thisTerm, wrk, to_skip, room_count, given_room_dept)
        integer, intent (in) :: to_skip, device, thisTerm
        integer, intent (out) :: room_count
        type (TYPE_SECTION), intent (in) :: wrk
        integer, intent (in), optional :: given_room_dept
        integer :: i, j, ierr, iRoom, room_dept
        integer, dimension(60,7) :: TimeTable
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
        write(device,AFORMAT) b_bold//'Rooms in '//trim(Department(room_dept)%Code)// &
        ' available during'//e_bold
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
            b_tr//'<th align="left">Room</th><th align="left">[Count]: Scheduled classes</th>'//e_tr
        room_count = 0
        do iRoom=1,NumRooms+NumAdditionalRooms
            if (Room(iRoom)%DeptIdx /= room_dept) cycle
            !write(*,*) 'Room is ', Room(iRoom)%Code
            skip = .false.
            !tArray = 0 ! current classes
            ncol = 0
            do i=1,NumSections(thisTerm)
                if (Section(thisTerm,i)%SubjectIdx==0) cycle ! section was deleted
                if (i==to_skip) cycle
                call meetings_of_section_in_room(thisTerm, i, iRoom, n_meetings, meetings)
                if (n_meetings==0) cycle ! room not assigned to this section
                !write(*,*) 'Section is ', Section(thisTerm,i)%ClassId
                ncol = ncol+1
                tArray(ncol) = i
                if (is_conflict_timetable_with_section_meetings(thisTerm, i, n_meetings, meetings, TimeTable)) then
                    !write(*,*) '        '//Section(thisTerm,i)%ClassId//' is in conflict!'
                    skip = .true. ! room not available
                    exit ! do not consider the remaining sections for this room
                end if
            end do
            if (skip) cycle ! done with this room
            !write(*,*) '        '//Room(iRoom)%Code//' is OK!'
            room_count = room_count+1
            QUERY_put = Room(iRoom)%Code
            write(device,AFORMAT) trim(make_href(fnRoomSchedule, Room(iRoom)%Code, &
                A1=QUERY_put, A9=thisTerm, &
                pre=b_tr//b_td, &
                post=e_td//b_td//'['//trim(itoa(ncol))//']'))
            do j=1,ncol
                write(device,AFORMAT) ' : '//Section(thisTerm,tArray(j))%ClassId
            end do
            write(device,AFORMAT) e_td//e_tr
        end do
        if (room_count == 0) write(device,AFORMAT) b_tr, &
            '<td colspan="2">'//b_italic//'(No rooms available during specified times, or time not specified)'//e_italic, &
            e_td, e_tr
        write(device,AFORMAT) e_table

    end subroutine room_search_given_time


    subroutine section_copy_classes_last_year (device, thisTerm)

        integer, intent (in) :: device, thisTerm

        character(len=MAX_LEN_FILE_PATH) :: path, prev_index, prev_dir, line
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege, tAction
        integer :: lastNumSections, nsections, lastNumBlocks, nblocks, i, owner, stat

        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, stat)
        targetCollege = index_to_college(tCollege)
        tDepartment = tCollege
        owner = index_to_dept(tDepartment)

        call cgi_get_named_string(QUERY_STRING, 'A2', tAction, stat) ! CLASSES only, or BLOCKS also

        path = trim(dirDATA)//trim(itoa(cTm3Year))//DIRSEP
        lastNumSections = NumSections(thisTerm)
        lastNumBlocks = NumBlocks(thisTerm)

        prev_dir       = trim(path)//'CLASSES'//DIRSEP//trim(txtSemester(thisTerm))//DIRSEP
        call classes_index_read(unitIDX, thisTerm, prev_dir, .true.)

        if (lastNumSections==NumSections(thisTerm)) goto 999

        ! keep retrieved classes owned by owner
        nsections = lastNumSections
        do i=lastNumSections+1,NumSections(thisTerm)
            if (Section(thisTerm,i)%DeptIdx==owner) then
                call class_details_write(unitXML, thisTerm, dirCLASSES(thisTerm), i)
                nsections = nsections+1
                Section(thisTerm,nsections) = Section(thisTerm,i)
            end if
        end do
        do i=nsections+1,NumSections(thisTerm)
            call initialize_section(Section(thisTerm,i))
        end do
        NumSections(thisTerm) = nsections
        call class_details_write(unitXML, thisTerm, indexCLASSES(thisTerm), 1, NumSections(thisTerm))
        call log_comment(trim(itoa(nsections-lastNumSections))//' classes added from '//prev_index, out6=.true.)

        if (trim(tAction)=='CLASSES') goto 999 ! skip blocks

       ! the blocks
        prev_dir       = trim(path)//'BLOCKS'//DIRSEP//trim(txtSemester(thisTerm))//DIRSEP
        prev_index     = trim(prev_dir)//'index'

        open(unit=unitIDX, file=prev_index, iostat=stat)
        if (stat/=0) then
            call log_comment('Error '//trim(itoa(stat))//' in opening '//prev_index)
            goto 999
        end if
        do
            read(unitIDX, AFORMAT, iostat=stat) line
            if (stat<0) exit
            call block_details_read(trim(prev_dir)//trim(line)//dotXML, thisTerm, .true.)
        end do
        close(unitIDX)

        ! keep retrieved blocks owned by owner
        nblocks = lastNumBlocks
        do i=lastNumBlocks+1,NumBlocks(thisTerm)
            if (Block(thisTerm,i)%DeptIdx==owner) then
                call block_details_write(unitXML, thisTerm, dirBLOCKS(thisTerm), i)
                nblocks = nblocks+1
                Block(thisTerm,nblocks) = Block(thisTerm,i)
            end if
        end do
        do i=nblocks+1,NumBlocks(thisTerm)
            call initialize_block(Block(thisTerm,i))
        end do
        NumBlocks(thisTerm) = nblocks
        call block_details_write(unitXML, thisTerm, indexBLOCKS(thisTerm), 1, NumBlocks(thisTerm))

        call log_comment(trim(itoa(nblocks-lastNumBlocks))//' blocks added from '//prev_index, out6=.true.)

        999 call html_college_links(device, targetCollege, &
            trim(itoa(NumSections(thisTerm)-lastNumSections))//' classes, '// &
            trim(itoa(NumBlocks(thisTerm)-lastNumBlocks))//' blocks for '//trim(tCollege)//' copied from '// &
            trim(text_term_school_year(thisTerm+6,cTm3Year)) )

        ! summarize
        call offerings_summarize(thisTerm)

        ! count no. of sections by dept
        call count_sections_by_dept(thisTerm)

        ! sort blocks
        if (trim(tAction)=='BLOCKS') call sort_alphabetical_blocks(thisterm)


    end subroutine section_copy_classes_last_year



    subroutine section_list_classes (device, thisTerm, fn, givenDept, givenArea, mesg)

        integer, intent (in) :: device, thisTerm, fn
        integer, intent(in), optional :: givenDept
        character(len=*), intent(in), optional :: mesg, givenArea

        integer :: iSubj, idx, jdx, i, iSect, iTeach, ncol, maxcol=7, nopen, nsections, iStd, iRoom
        integer :: owner_dept, owner_coll, previous
        character(len=127) :: header
        character(len=22) :: preText, postText
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject, tSeats, searchString
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_USERNAME) :: tTeacher
        logical :: isLecture, okToAdd, conflicted, showStudentList, allowed_to_show
        integer, dimension(60,7) :: TimeTable

        targetDepartment = DeptIdxUser
        targetCollege = CollegeIdxUser
        call recalculate_available_seats(thisTerm)
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
#if defined UPLB
                ! unit already set above
#else
                ! Subjects administered by program
                tDepartment = tCollege
                targetDepartment = index_to_dept(tDepartment)
#endif

            case (fnTBARooms, fnTBATeachers) ! TBA rooms, teachers
                call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, i)
                targetCollege = index_to_college(tCollege)
                header = trim(tCollege)//' classes with TBA room/teacher'
#if defined UPLB
                ! unit already set above
#else
                ! Subjects administered by program
                tDepartment = tCollege
                targetDepartment = index_to_dept(tDepartment)
#endif

            case (fnUnpostedGrades, fnGradesheetsToReceive) ! unposted grades, no hardcopy of gradesheets
                call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, i)
                targetCollege = index_to_college(tCollege)
                header = trim(tCollege)//' classes with unposted grades/no hardcopy of gradesheet'
#if defined UPLB
                ! unit already set above
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
            tArray(1:NumSections(thisTerm)) = 0
            ! mark sections in college with unposted grades
            do iStd = 1,NumStudents+NumAdditionalStudents
                do idx=1,Student(iStd)%Enlistment(thisTerm)%NPriority+Student(iStd)%Enlistment(thisTerm)%NAlternates + &
                         Student(iStd)%Enlistment(thisTerm)%NCurrent
                    iSect = Student(iStd)%Enlistment(thisTerm)%Section(idx)
                    if (iSect>0) then ! accommodated
                        owner_coll = Department(Section(thisTerm,iSect)%DeptIdx)%CollegeIdx
                        if (owner_coll==targetCollege) then
                            if (gdxREGD==Student(iStd)%Enlistment(thisTerm)%Grade(idx) ) then
                                tArray(iSect) = tArray(iSect)+1
                                !call html_comment(Student(iStd)%Name//Section(thisTerm,iSect)%ClassId// &
                                !    txtGrade(pGrade(Student(iStd)%Enlistment(thisTerm)%Grade(idx))) )
                            end if
                        end if
                    end if
                end do
            end do
            ! compress
            if ( isRole_dean_of_college(targetCollege,orHigherUp) ) then
                do iSect=1,NumSections(thisTerm)
                    if (tArray(iSect)==0) cycle
                    nsections = nsections+1
                    tArray(nsections) = iSect
                end do
            else
                do iSect=1,NumSections(thisTerm)
                    if (tArray(iSect)==0) cycle
                    ! user is teacher of section?
                    i = 0
                    do ncol=1,Section(thisTerm,iSect)%NMeets
                        if (Section(thisTerm,iSect)%TeacherIdx(ncol)==requestingTeacher) i = i+1
                    end do
                    if (i>0) then
                        nsections = nsections+1
                        tArray(nsections) = iSect
                    end if
                end do
            end if
        else

            do iSect=1,NumSections(thisTerm)
                iSubj = Section(thisTerm,iSect)%SubjectIdx
                if (iSubj==0) cycle ! section was deleted
                owner_dept = Section(thisTerm,iSect)%DeptIdx
                owner_coll = Department(owner_dept)%CollegeIdx
                okToAdd = .false.

                select case(fn)

                    case (fnGradesheetsToReceive) ! sections with empty GradeSubmissionDate
                        okToAdd = len_trim(Section(thisTerm,iSect)%GradeSubmissionDate)==0 .and. &
                            owner_coll == targetCollege

                    case (fnScheduleByArea) ! sections by subject area
                        okToAdd = index(Section(thisTerm,iSect)%ClassId, trim(searchString)//SPACE)==1 .and. &
                            owner_coll == targetCollege

                    case (fnTBARooms) ! sections with TBA rooms
                        if (owner_coll==targetCollege) then
                            i = 0
                            do ncol=1,Section(thisTerm,iSect)%NMeets
                                if (Section(thisTerm,iSect)%RoomIdx(ncol)==0) i = i+1
                            end do
                            okToAdd = i>0
                        end if

                    case (fnTBATeachers) ! sections with TBA teachers
                        if (owner_coll==targetCollege) then
                            i = 0
                            do ncol=1,Section(thisTerm,iSect)%NMeets
                                if (Section(thisTerm,iSect)%TeacherIdx(ncol)==0) i = i+1
                            end do
                            okToAdd = i>0
                        end if

                    case (fnTeacherClasses)
                            i = 0
                            do ncol=1,Section(thisTerm,iSect)%NMeets
                                if (Section(thisTerm,iSect)%TeacherIdx(ncol)==targetTeacher) i = i+1
                            end do
                            okToAdd = i>0

                end select

                if (okToAdd) then
                    nsections = nsections + 1
                    tArray(nsections) = iSect
                end if

            end do ! iSect=1,NumSections(thisTerm)

        end if

        if (nsections==0) then
            write(device,AFORMAT) BRNONEHR
            return
        end if

        ! sort sections by class id
        do idx=1,nsections-1
            do jdx=idx+1,nsections
                if (Section(thisTerm,tArray(jdx))%ClassId<Section(thisTerm,tArray(idx))%ClassId) then
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
            iSubj = Section(thisTerm,tArray(idx))%SubjectIdx
            if (previous/=iSubj) then
                nopen = nopen+1
                tArray(nsections+nopen) = iSubj
                previous = iSubj
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
                    preText = b_tr//b_td
                    postText = e_td
                else if (ncol == maxcol) then
                    preText = b_td
                    postText = e_td//e_tr
                    ncol = 0
                else
                    preText = b_td
                    postText = e_td
                end if

                write(device,AFORMAT) trim(preText)//'<a href="#'//trim(tSubject)//'">'//trim(tSubject)//'</a>'//postText

            end do
            if (ncol /= 0)  then
                do i=ncol+1,maxcol
                    write(device,AFORMAT) b_td_nbsp_e_td
                end do
                write(device,AFORMAT) e_tr
            end if
            write(device,AFORMAT) e_table
        end if


        write(device,AFORMAT) &
            horizontal//b_italic//'Note: If the hyperlinks are active, the section code links to the gradesheet,', &
            ' the block name links to the block schedule, and the no. of seats links to the classlist. ', &
            ' Deleting a lecture section automatically deletes the associated laboratory or recitation sections. ', &
            ' Laboratory or recitation section codes MUST have the format "LECT-nL" or "LECT-nR" where "LECT" is ', &
            ' the code for the lecture section, and "n" is an integer.'//e_italic//linebreak//linebreak, &
            '<table border="0" width="100%">'//b_tr, &
            b_thal//'Subject'//e_th//&
            b_thal//'Section'//e_th//&
            b_thal//'Block'//e_th//&
            b_thal//'Seats/Open'//e_th//&
            b_thal//'Day'//e_th//&
            b_thal//'Time'//e_th//&
            b_thal//'Room'//e_th//&
            b_thal//'Teacher'//e_th//&
            b_thal//b_small//'Action'//e_small//e_th//e_tr

        okToAdd = isRole_admin_of_college(targetCollege) .or. &
            ( isRole_chair_of_department(targetDepartment,orHigherUp) .and. &
              College(targetCollege)%isAllowed(ToEditCLASSES,thisTerm) )

        allowed_to_show = College(targetCollege)%isAllowed(ToShowTEACHERS,thisTerm) .or. &
               isRole_chair_of_department(targetDepartment, orHigherUp)

        if (.not. allowed_to_show .and. fn==fnTeacherClasses) then
            write(device,AFORMAT) '</table>', b_para, trim(sorryMessageSchedules), e_para, &
                horizontal
            return
        end if

        do idx=1,nopen
            iSubj = tArray(nsections+idx)

            if (isSubject_lecture_lab(iSubj)) then ! subject is lecture-lab
                tDepartment = 'lect'
            else
                tDepartment = 'sect'
            end if
            QUERY_put = Subject(iSubj)%Name
            write(device,AFORMAT) &
                b_tr//'<td colspan="8">'//nbsp//e_td, &
                b_td//'<a name="'//trim(Subject(iSubj)%Name)//'"></a>'//b_small//'[<a href="#TOP">Top</a>]'// &
                e_small//e_td//e_tr
            write(device,AFORMAT) &
                b_tr//b_td//trim(Subject(iSubj)%Name)//e_td//'<td colspan="8">'// &
                trim(Subject(iSubj)%Title)//'. '//trim(ftoa(Subject(iSubj)%Units,1))//' units. '

            if (isSubject_lecture_lab(iSubj)) then
                write(device,AFORMAT) &
                    trim(ftoa(Subject(iSubj)%LectHours+Subject(iSubj)%LabHours,2))//' hrs ('// &
                    trim(ftoa(Subject(iSubj)%LectHours,2))//' lect + '// &
                    trim(ftoa(Subject(iSubj)%LabHours,2))//' lab/recit).'
            else if (Subject(iSubj)%LectHours > 0.0) then
                write(device,AFORMAT) trim(ftoa(Subject(iSubj)%LectHours,2))//' hrs lect.'
            else if (Subject(iSubj)%LabHours > 0.0) then
                write(device,AFORMAT) trim(ftoa(Subject(iSubj)%LabHours,2))//' hrs lab/recit.'
            end if

            write(device,AFORMAT) '('//trim(text_term_offered_separated(Subject(iSubj)%TermOffered))//')'
            if (fn/=fnTeacherClasses .and. (isRole_admin_of_college(targetCollege) .or. isRoleOfficial)) then
                write(device,AFORMAT) trim(make_href(fnEditSubject, 'Edit', A1=Subject(iSubj)%Name, A9=thisTerm, &
                    pre=nbsp//b_small//'[ ', post=' ]'//e_small))
            end if

            if (fn==fnTeacherClasses .and. &
                (isRole_dean_of_college(targetCollege, orHigherUp) .or. trim(USERNAME)==trim(tTeacher)) ) then
                write(device,AFORMAT) trim(make_href(fnEvaluationRatings, 'Eval', &
                    A1=tTeacher, A2='Subject', A3=Subject(iSubj)%Name, A9=thisTerm, &
                    pre=nbsp//b_small//'[ ', post=' ]'//e_small))
            end if

            write(device,AFORMAT) e_td//e_tr, &
                b_tr//b_td_nbsp_e_td//'<td colspan="7">'//nbsp//'Pr. '//trim(text_prerequisite_of_subject(iSubj,0))//e_td

            if (fn/=fnTeacherClasses .and. okToAdd ) then
                write(device,AFORMAT) b_td//b_small, &
                    trim(make_href(fnScheduleOfferSubject, 'Add '//tDepartment, &
                    A1=QUERY_put, A2=Department(targetDepartment)%Code, A9=thisTerm, &
                    post=e_small//e_td//e_tr ))
            else
                write(device,AFORMAT) b_td_nbsp_e_td//e_tr
            end if

            ! sections
!            if (.not. allowed_to_show) cycle

            do jdx=1,nsections
                iSect = tArray(jdx)
                if (Section(thisTerm,iSect)%SubjectIdx/=iSubj) cycle

                owner_dept = Section(thisTerm,iSect)%DeptIdx
                owner_coll = Department(owner_dept)%CollegeIdx

                isLecture = isSubject_lecture_lab(iSubj) .and. is_lecture_class(iSect, thisTerm) ! empty SPACE
                if (isLecture) then
                    write(device,AFORMAT) b_tr//'<td colspan="9">'//nbsp//e_td//e_tr
                    tSeats = itoa(Section(thisTerm,iSect)%Slots)
                else
                    tSeats = trim(itoa(Section(thisTerm,iSect)%Slots))//FSLASH//trim(itoa(Section(thisTerm,iSect)%RemSlots))
                end if
                QUERY_put = Section(thisTerm,iSect)%ClassId
                ! subject
                write(device,AFORMAT) b_tr//b_td_nbsp_e_td

                ! section code, link to gradesheet entry form
                showStudentList = isRole_admin_of_college(targetCollege) .or. &
                     ( isRole_teacher_of_class (Section(thisTerm,iSect), orHigherUp) .and. &
                       len_trim(Section(thisTerm,iSect)%GradeSubmissionDate)==0 .and. &
                       College(owner_coll)%isAllowed(ToEditGRADES,thisTerm) )
                if ( showStudentList ) then
                    write(device,AFORMAT) trim(make_href(fnGradesheet, trim(Section(thisTerm,iSect)%Code), &
                        A1=QUERY_PUT, A9=thisTerm, pre=b_td, post=e_td ))
                elseif (College(owner_coll)%isAllowed(ToShowTEACHERS,thisTerm) .and. &
                        isRole_teacher_of_class (Section(thisTerm,iSect))) then
                    write(device,AFORMAT) trim(make_href(fnPrintableGradesheet, trim(Section(thisTerm,iSect)%Code), &
                        A1=QUERY_PUT, A9=thisTerm, pre=b_td, post=e_td, newtab='"_blank"' ))
                else
                    write(device,AFORMAT) b_td//trim(Section(thisTerm,iSect)%Code)//e_td
                end if

                ! blocks
                write(device,AFORMAT) b_td
                call blocks_in_section(device, iSect, fnBlockSchedule, thisTerm)
                write(device,AFORMAT) e_td

                ! seats, link to classlist
                if (showStudentList) then
                    write(device,AFORMAT) trim(make_href(fnClassList, tSeats, &
                        A1=QUERY_PUT, A9=thisTerm, pre=b_td, post=e_td))
                else
                    write(device,AFORMAT) b_td//trim(tSeats)//e_td
                end if

                ! time, day, room, teacher
                if (is_regular_schedule(iSect, thisTerm)) then
                    iTeach = Section(thisTerm,iSect)%TeacherIdx(1)
                    iRoom = Section(thisTerm,iSect)%RoomIdx(1)
                    write(device,AFORMAT) &
                        b_td//trim(text_days_of_section(Section(thisTerm,iSect)) )//e_td// &
                        b_td//trim(text_time_period(Section(thisTerm,iSect)%bTimeIdx(1), Section(thisTerm,iSect)%eTimeIdx(1)))//e_td
                    if (iRoom/=0) then
                        if (fn==fnRoomSchedule) then
                            write(device,AFORMAT) b_td//trim(Room(iRoom)%Code)//e_td
                        else
                            write(device,AFORMAT) trim(make_href(fnRoomSchedule, Room(iRoom)%Code, &
                                A1=Room(iRoom)%Code, A9=thisTerm, pre=b_td, post=e_td))
                        end if
                    else
                        write(device,AFORMAT) b_td//red//trim(Room(iRoom)%Code)//e_color//e_td
                    end if
                    if (iTeach/=0) then
                        if (allowed_to_show) then
                            if (fn==fnTeacherClasses) then
                                write(device,AFORMAT) b_td//trim(Teacher(iTeach)%Name)//e_td
                            else
                                write(device,AFORMAT) trim(make_href(fnTeacherClasses, Teacher(iTeach)%Name, &
                                    A1=Teacher(iTeach)%TeacherId, A9=thisTerm, pre=b_td, post=e_td))
                            end if
                        else
                            write(device,AFORMAT) b_td//b_italic//'(hidden)'//e_italic//e_td
                        end if
                    else
                        write(device,AFORMAT) b_td//red//trim(Teacher(iTeach)%Name)//e_color//e_td
                    end if

                else

                    do ncol=1,Section(thisTerm,iSect)%NMeets
                        iTeach = Section(thisTerm,iSect)%TeacherIdx(ncol)
                        iRoom = Section(thisTerm,iSect)%RoomIdx(ncol)
                        write(device,AFORMAT) &
                            b_td//txtDay(Section(thisTerm,iSect)%DayIdx(ncol))//e_td// &
                            b_td//trim(text_time_period(Section(thisTerm,iSect)%bTimeIdx(ncol), &
                                Section(thisTerm,iSect)%eTimeIdx(ncol)))//e_td
                        if (iRoom/=0) then
                            if (fn==fnRoomSchedule) then
                                write(device,AFORMAT) b_td//trim(Room(iRoom)%Code)//e_td
                            else
                                write(device,AFORMAT) trim(make_href(fnRoomSchedule, Room(iRoom)%Code, &
                                    A1=Room(iRoom)%Code, A9=thisTerm, pre=b_td))
                            end if
                        else
                            write(device,AFORMAT) b_td//red//trim(Room(iRoom)%Code)//e_color//e_td
                        end if
                        if (iTeach/=0) then
                            if (allowed_to_show) then
                                if (fn==fnTeacherClasses) then
                                    write(device,AFORMAT) b_td//trim(Teacher(iTeach)%Name)
                                else
                                    write(device,AFORMAT) trim(make_href(fnTeacherClasses, Teacher(iTeach)%Name, &
                                        A1=Teacher(iTeach)%TeacherId, A9=thisTerm, pre=b_td))
                                end if
                            else
                                write(device,AFORMAT) b_td//b_italic//'(hidden)'//e_italic
                            end if
                        else
                            write(device,AFORMAT) b_td//red//trim(Teacher(iTeach)%Name)//e_color
                        end if
                        if (ncol<Section(thisTerm,iSect)%NMeets) then
                            write(device,AFORMAT) e_td//b_td//e_td//e_tr// &
                                b_tr//b_td//e_td//b_td//e_td//b_td//e_td//b_td//e_td
                        else
                            write(device,AFORMAT) e_td
                        end if
                    end do

                end if
                write(device,AFORMAT) b_td//b_small
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
                write(device,AFORMAT) e_small//e_td//e_tr

                ! correct no. of hours?
                if (.not. is_consistent_section_hours_with_subject_defn(Section(thisTerm,iSect), thisTerm)) then
                    write(device,AFORMAT) b_tr//'<td align="center" colspan="9">'// &
                        red//'Total meeting hours is inconsistent with the subject specifications?'//e_color//e_td//e_tr
                end if
                ! meeting conflicts?
                if (.not. is_conflict_free_section_hours(Section(thisTerm,iSect), thisTerm)) then
                    write(device,AFORMAT) b_tr//'<td align="center" colspan="9">'// &
                        red//'Conflict in meeting times, or conflict with lecture section?'//e_color//e_td//e_tr
                end if

            end do

        end do
        write(device,AFORMAT) e_table//linebreak
        if (fn==fnTeacherClasses .and. allowed_to_show) then
            call timetable_meetings_of_teacher(thisTerm, targetTeacher, 0, nsections, tArray, TimeTable, conflicted)
            if (nsections>0) call timetable_display(device, thisTerm, TimeTable)
        end if
        write(device,AFORMAT) horizontal

    end subroutine section_list_classes


    subroutine section_list_all (device)

        integer, intent (in) :: device

        integer :: iSubj, idx, jdx, i, iSect, ncol, nclosed, maxcol=7, nopen, nsections
        integer :: owner_dept, owner_coll, previous, iTerm
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
        do iTerm=firstSemester,summerTerm

            write(device,AFORMAT) '<h3>Classes in '//trim(tDepartment)//' for '// &
                trim(text_term_school_year(iTerm+3, currentYear))//'</h3>'

            ! make list of sections
            nsections = 0
            do iSect=1,NumSections(iTerm)
                iSubj = Section(iTerm,iSect)%SubjectIdx
                if (iSubj==0) cycle ! section was deleted
                owner_dept = Section(iTerm,iSect)%DeptIdx
                owner_coll = Department(owner_dept)%CollegeIdx
                if (owner_dept==targetDepartment) then
                    nsections = nsections + 1
                    tArray(nsections) = iSect
                end if

            end do ! iSect=1,NumSections(thisTerm)

            if (nsections==0) write(device,AFORMAT) BRNONE

            ! sort sections by class id
            do idx=1,nsections-1
                do jdx=idx+1,nsections
                    if (Section(iTerm,tArray(jdx))%ClassId<Section(iTerm,tArray(idx))%ClassId) then
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
                iSubj = Section(iTerm,tArray(idx))%SubjectIdx
                if (previous/=iSubj) then
                    nopen = nopen+1
                    tArray(nsections+nopen) = iSubj
                    previous = iSubj
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
                    preText = b_tr//b_td
                    postText = e_td
                else if (ncol == maxcol) then
                    preText = b_td
                    postText = e_td//e_tr
                    ncol = 0
                else
                    preText = b_td
                    postText = e_td
                end if

                write(device,AFORMAT) trim(make_href(fnScheduleByArea, tSubject, A1=tCollege, A2=tArea, A9=iTerm, &
                    pre=trim(preText), post=postText, anchor=tSubject))
            end do
            if (ncol /= 0)  then
                do i=ncol+1,maxcol
                    write(device,AFORMAT) b_td_nbsp_e_td
                end do
                write(device,AFORMAT) e_tr
            end if
            write(device,AFORMAT) e_table

            nclosed = 0
            ! make list of closed subjects here, starting at tArray(nsections+nopen+1)
#if defined UPLB
            do iSubj=1,NumSubjects+NumAdditionalSubjects
                if (Offering(iTerm,iSubj)%NSections>0) cycle
                if (Subject(iSubj)%DeptIdx/=targetDepartment) cycle
                nclosed = nclosed+1
                tArray(nsections+nopen+nclosed) = iSubj
            end do
#else
            ! Subjects administered by program
            do iSubj=1,NumSubjects+NumAdditionalSubjects
                if (Offering(iTerm,iSubj)%NSections>0) cycle
                if (.not. isSubject_used_in_college(targetCollege, iSubj)) cycle
                nclosed = nclosed+1
                tArray(nsections+nopen+nclosed) = iSubj
            end do
#endif

            ! offer to open sections
            okToAdd = isRole_admin_of_college(targetCollege) .or. &
                ( isRole_chair_of_department(targetDepartment,orHigherUp) .and. &
                  College(targetCollege)%isAllowed(ToEditCLASSES,iTerm) )

            if (nclosed>0 .and. okToAdd) then
                call make_form_start(device, fnScheduleOfferSubject, A2=Department(targetDepartment)%Code, A9=iTerm)
                write(device,AFORMAT) &
                    '<table border="0" width="100%">'//b_tr//'<td colspan="'//trim(itoa(maxcol))//'" align="right">', &
                    'Open a section in <select name="A1"> <option value=""> (select subject)'
                do idx=1,nclosed
                    tSubject = Subject(tArray(nsections+nopen+idx))%Name
                    write(device,AFORMAT) '<option value="'//trim(tSubject)//'"> '//tSubject
                end do
                write(device,AFORMAT) '</select> '//nbsp//nbsp//' <input type="submit" value="Submit">'// &
                e_td//e_tr//e_table//e_form
            end if

            write(device,AFORMAT) horizontal

        end do

    end subroutine section_list_all


end module EditSECTIONS
