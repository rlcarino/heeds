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


module EditUNIVERSITY

    use HTML

    implicit none

contains


    subroutine display_signatories(device, mesg)
        integer, intent(in) :: device
        character(len=*), intent (in) :: mesg

        integer :: iColl

        call html_comment('display_signatories()')

        targetDepartment = DeptIdxUser
        targetCollege = CollegeIdxUser

        call html_write_header(device, 'Update University data and signatories', mesg)

        call make_form_start(device, fnEditSignatories)

        write(device,AFORMAT) '<table border="0" width="100%">', &
            begintr//thalignright//'Title or Position'//endth//tdnbspendtd//thalignleft//'Name'//endth//endtr, &
            begintr//tdalignright//beginitalic//'(University name)'//enditalic//endtd//tdnbspendtd// &
                     begintd//'<input name="UniversityName" size="60" value="'//trim(UniversityName)//'">'//endtd// &
            endtr, &
            begintr//tdalignright//beginitalic//'(University address)'//enditalic//endtd//tdnbspendtd// &
                     begintd//'<input name="UniversityAddress" size="60" value="'//trim(UniversityAddress)//'">'//endtd// &
            endtr, &
            begintr//tdalignright//beginitalic//'(University phone)'//enditalic//endtd//tdnbspendtd// &
                     begintd//'<input name="UniversityPhone" size="60" value="'//trim(UniversityPhone)//'">'//endtd// &
            endtr, &
            begintr//tdalignright//beginitalic//'(University web address)'//enditalic//endtd//tdnbspendtd// &
                     begintd//'<input name="UniversityWeb" size="60" value="'//trim(UniversityWeb)//'">'//endtd// &
            endtr, &
            begintr// &
                tdalignright//'<input name="titleUniversityPresident" size="40" value="'//trim(titleUniversityPresident)//'">'// &
                endtd//tdnbspendtd// &
                begintd//'<input name="UniversityPresident" size="60" value="'//trim(UniversityPresident)//'">'//endtd// &
            endtr, &
            begintr// &
                tdalignright//'<input name="titleVPAcademicAffairs" size="40" value="'//trim(titleVPAcademicAffairs)//'">'// &
                endtd//tdnbspendtd// &
                begintd//'<input name="VPAcademicAffairs" size="60" value="'//trim(VPAcademicAffairs)//'">'//endtd// &
            endtr, &
            begintr// &
                tdalignright//'<input name="titleDeanOfCampus" size="40" value="'//trim(titleDeanOfCampus)//'">'// &
                endtd//tdnbspendtd// &
                begintd//'<input name="DeanOfCampus" size="60" value="'//trim(DeanOfCampus)//'">'//endtd// &
            endtr, &
            begintr// &
                tdalignright//'<input name="titleDeanOfInstruction" size="40" value="'//trim(titleDeanOfInstruction)//'">'// &
                endtd//tdnbspendtd// &
                begintd//'<input name="DeanOfInstruction" size="60" value="'//trim(DeanOfInstruction)//'">'//endtd// &
            endtr, &
            begintr// &
                tdalignright//'<input name="titleTheRegistrar" size="40" value="'//trim(titleTheRegistrar)//'">'// &
                endtd//tdnbspendtd// &
                begintd//'<input name="TheRegistrar" size="60" value="'//trim(TheRegistrar)//'">'//endtd// &
            endtr

        write(device,AFORMAT) endtable, linebreak, &
            nbsp//'<input name="action" type="submit" value="Update">'//endform//linebreak


        write(device,AFORMAT) '<h3>Update college signatories</h3>'

        call make_form_start(device, fnEditSignatories)

        write(device,AFORMAT) '<table border="0" width="100%">', &
            begintr//thalignright//'Title or Position'//endth//tdnbspendtd//thalignleft//'Name'//endth//endtr

        do iColl=1,NumColleges-1
            write(device,AFORMAT) &
                begintr, &
                tdalignright//beginitalic//'(College Dean, '//trim(College(iColl)%Code)//')'//enditalic//endtd, &
                tdnbspendtd, &
                begintd//'<input name="DEAN:'//trim(College(iColl)%Code)//'" size="60" value="'// &
                         trim(College(iColl)%Dean)//'">'//endtd, &
                endtr, &
                begintr, &
                tdalignright//beginitalic//'(Transcript Preparer, '//trim(College(iColl)%Code)//')'//enditalic//endtd, &
                tdnbspendtd, &
                begintd//'<input name="PREPARER:'//trim(College(iColl)%Code)//'" size="60" value="'// &
                         trim(College(iColl)%TranscriptPreparer)//'">'//endtd, &
                endtr, &
                begintr, &
                tdalignright//beginitalic//'(Transcript Checker, '//trim(College(iColl)%Code)//')'//enditalic//endtd, &
                tdnbspendtd, &
                begintd//'<input name="CHECKER:'//trim(College(iColl)%Code)//'" size="60" value="'// &
                         trim(College(iColl)%TranscriptChecker)//'">'//endtd, &
                endtr
        end do

        write(device,AFORMAT) endtable, linebreak, &
            nbsp//'<input name="action" type="submit" value="Update">'//endform//horizontal


    end subroutine display_signatories


    subroutine edit_signatories(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_TEACHER_CODE) :: tAction
        character (len=255) :: tInput
        logical :: changes
        integer :: iColl, ierr

        call html_comment('edit_signatories()')

        ! check for requested action
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)

        if (ierr/=0 .or. tAction==SPACE) then ! no action; display existing info

        else if (.not. isRoleOfficial) then ! action is Update

            ! collect changes to UNIVERSITY.XML
            changes = .false.
            call cgi_get_named_string(QUERY_STRING, 'titleUniversityPresident', tInput, ierr)
            if (ierr/=0) tInput = titleUniversityPresident
            if (tInput /= titleUniversityPresident) then
                titleUniversityPresident = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'titleVPAcademicAffairs', tInput, ierr)
            if (ierr/=0) tInput = titleVPAcademicAffairs
            if (tInput /= titleVPAcademicAffairs) then
                titleVPAcademicAffairs = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'titleDeanOfCampus', tInput, ierr)
            if (ierr/=0) tInput = titleDeanOfCampus
            if (tInput /= titleDeanOfCampus) then
                titleDeanOfCampus = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'titleDeanOfInstruction', tInput, ierr)
            if (ierr/=0) tInput = titleDeanOfInstruction
            if (tInput /= titleDeanOfInstruction) then
                titleDeanOfInstruction = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'titleTheRegistrar', tInput, ierr)
            if (ierr/=0) tInput = titleTheRegistrar
            if (tInput /= titleTheRegistrar) then
                titleTheRegistrar = tInput
                changes = .true.
                sorryMessage = trim(UniversityCode)//' officials and their friends '// &
                    ' have a "read-only" permission at this time. '// &
                    ' Please see the '//trim(titleTheRegistrar)//' if you wish to change some data.'
            end if

            call cgi_get_named_string(QUERY_STRING, 'UniversityName', tInput, ierr)
            if (ierr/=0) tInput = UniversityName
            if (tInput/=UniversityName) then
                UniversityName = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'UniversityAddress', tInput, ierr)
            if (ierr/=0) tInput = UniversityAddress
            if (tInput /= UniversityAddress) then
                UniversityAddress = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'UniversityWeb', tInput, ierr)
            if (ierr/=0) tInput = UniversityWeb
            if (tInput /= UniversityWeb) then
                UniversityWeb = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'UniversityPhone', tInput, ierr)
            if (ierr/=0) tInput = UniversityPhone
            if (tInput /= UniversityPhone) then
                UniversityPhone = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'UniversityPresident', tInput, ierr)
            if (ierr/=0) tInput = UniversityPresident
                if (tInput /= UniversityPresident) then
                UniversityPresident = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'VPAcademicAffairs', tInput, ierr)
            if (ierr/=0) tInput = VPAcademicAffairs
            if (tInput /= VPAcademicAffairs) then
                VPAcademicAffairs = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'DeanOfCampus', tInput, ierr)
            if (ierr/=0) tInput = DeanOfCampus
            if (tInput /= DeanOfCampus) then
                DeanOfCampus = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'DeanOfInstruction', tInput, ierr)
            if (ierr/=0) tInput = DeanOfInstruction
            if (tInput /= DeanOfInstruction) then
                DeanOfInstruction = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'TheRegistrar', tInput, ierr)
            if (ierr/=0) tInput = TheRegistrar
            if (tInput /= TheRegistrar) then
                TheRegistrar = tInput
                changes = .true.
            end if

            if (changes) call xml_write_university(trim(pathToYear)//'UNIVERSITY.XML')

            ! collect changes to COLLEGES.XML
            changes = .false.
            do iColl=1,NumColleges-1
                call cgi_get_named_string(QUERY_STRING, 'DEAN:'//trim(College(iColl)%Code), tInput, ierr)
                if (ierr/=0) tInput = College(iColl)%Dean
                if (tInput /= College(iColl)%Dean) then
                    College(iColl)%Dean = tInput
                    changes = .true.
                end if
                call cgi_get_named_string(QUERY_STRING, 'PREPARER:'//trim(College(iColl)%Code), tInput, ierr)
                if (ierr/=0) tInput = College(iColl)%TranscriptPreparer
                if (tInput /= College(iColl)%TranscriptPreparer) then
                    College(iColl)%TranscriptPreparer = tInput
                    changes = .true.
                end if
                call cgi_get_named_string(QUERY_STRING, 'CHECKER:'//trim(College(iColl)%Code), tInput, ierr)
                if (ierr/=0) tInput = College(iColl)%TranscriptChecker
                if (tInput /= College(iColl)%TranscriptChecker) then
                    College(iColl)%TranscriptChecker = tInput
                    changes = .true.
                end if
            end do

            if (changes) call xml_write_colleges(trim(pathToYear)//'COLLEGES.XML')

        end if

        if (len_trim(tAction)>0 .and. isRoleOfficial) then
            tInput = 'Edit signatories failed. '//sorryMessage
        else
            tInput = SPACE
        end if

        call display_signatories(device, tInput)


    end subroutine edit_signatories



    subroutine message_of_the_day(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_TEACHER_CODE) :: tAction
        integer :: ierr

        call html_comment('message_of_the_day()')

        targetDepartment = DeptIdxUser
        targetCollege = CollegeIdxUser

        ! check for requested action
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)

        if (ierr/=0 .or. tAction==SPACE) then ! no action; display existing info

            call html_write_header(device, 'Edit the message of the day', SPACE)
            call make_form_start(device, fnEditMOTD)
            write(device,AFORMAT) &
                beginitalic, &
                'Note: Type one continuous line of text. Intersperse &lt;br&gt; for line breaks', &
                enditalic, &
                linebreak, &
                '<textarea cols="80" rows="5" name="MOTD">'//trim(MOTD)//'</textarea>', &
                linebreak, &
                linebreak, &
                nbsp//'<input name="action" type="submit" value="Update">', &
                endform//horizontal

        else ! action is Update

            call cgi_get_named_string(QUERY_STRING, 'MOTD', cipher, ierr)
            if (ierr==0) then
                MOTD = cipher
                if (isRoleOfficial) then
                    call html_college_links(device, CollegeIdxUser, mesg='"Message of the day" not changed. '//sorryMessage)
                else
                    call html_college_links(device, CollegeIdxUser, mesg='Message of the day is: '//trim(MOTD))
                    open (unit=unitETC, file=trim(pathToYear)//'MOTD.TXT', status='unknown')
                    write(unitETC, AFORMAT) trim(MOTD)
                    close(unitETC)
                end if
            end if

        end if


    end subroutine message_of_the_day


    subroutine room_edit(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_ROOM_CODE) :: tRoom, tAction
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer :: ierr, rdx, j
        character (len=255) :: mesg, remark
        type (TYPE_ROOM) :: wrk
        logical :: isDirtyROOMS, criticalErr

        ! which room ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tRoom, rdx)
        if (rdx/=0) tRoom = 'TBA'

        call html_comment('room_edit('//trim(tRoom)//') ')
        rdx = index_to_room(tRoom)
        targetDepartment = Room(rdx)%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx

        wrk = Room(rdx) ! make a working copy
        isDirtyROOMS = .false.
        remark = SPACE
        mesg = SPACE

        ! check for requested action
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)

        if (ierr/=0 .or. tAction==SPACE) then ! no action; display existing info

            if (trim(tRoom)=='TBA') then
                mesg = 'Add new room'
                tAction = 'Add'
            else
                mesg = 'Edit info for room '//tRoom
                tAction = 'Update'
            end if

            call room_info(device, wrk, mesg, remark, tAction)

        else if (isRoleOfficial) then

            mesg = 'Edit info for room '//tRoom
            remark = 'Update room "'//trim(tRoom)//'" failed. '//sorryMessage
            call room_info(device, wrk, mesg, remark, tAction)

        else ! action is Add or Update; collect changes

            call cgi_get_named_integer(QUERY_STRING, 'MaxCapacity', wrk%MaxCapacity, ierr)
            !write(*,*) 'ierr=', ierr, ', MaxCapacity=', wrk%MaxCapacity
            if (ierr/=0) wrk%MaxCapacity = Room(rdx)%MaxCapacity

            call cgi_get_named_integer(QUERY_STRING, 'Cluster', wrk%Cluster, ierr)
            !write(*,*) 'ierr=', ierr, ', Cluster=', wrk%Cluster
            if (ierr/=0) wrk%Cluster = Room(rdx)%Cluster

            call cgi_get_named_string(QUERY_STRING, 'Code', mesg, ierr)
            wrk%Code = trim(mesg)
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

            if (isDirtyROOMS) then ! some changes

                if (wrk%Code /= Room(rdx)%Code) then  ! new code; check if room already exists

                    j = index_to_room(wrk%Code)
                    if (j==0) then ! not used

                        if (trim(tAction)=='Add') then

                            call check_array_bound (NumRooms+NumAdditionalRooms+1, MAX_ALL_ROOMS, 'MAX_ALL_ROOMS', criticalErr)
                            if (criticalErr) then
                                remark = ': No more space for additional room'
                            else
                                NumAdditionalRooms = NumAdditionalRooms+1
                                Room(NumRooms+NumAdditionalRooms) = wrk
                                rdx = NumRooms+NumAdditionalRooms
                                tRoom = wrk%Code
                                remark = ': Added new room '//wrk%Code
                            end if
                        else
                            ! update existing
                            Room(rdx) = wrk
                        end if

                    else

                        remark = ': Add/edit room failed; "'//trim(wrk%Code)//'" already exists.'
                        isDirtyROOMS = .false.

                    end if
                else
                    ! same code; update other fields
                    Room(rdx) = wrk
                end if

                if (isDirtyROOMS) call xml_write_rooms(trim(pathToYear)//'ROOMS.XML')

            else ! Add or Update clicked, but no changes made
                remark = ': No changes made?'

            end if

            call html_college_links(device, Department(wrk%DeptIdx)%CollegeIdx, &
                trim(tRoom)//remark)

        end if

    end subroutine room_edit


    subroutine room_list_all (device)
        integer, intent (in) :: device
        integer :: rdx, n_count, nsect(3), sdx, tdx, term, ierr
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer, dimension(60,6) :: TimeTable
        character (len=127) :: mesg
        integer :: tYear, tTerm

        ! collect rooms
        tArray = 0
        n_count = 0

        ! which dept ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)

        call html_comment('room_list_all('//trim(tDepartment)//')')

        targetDepartment = index_to_dept(tDepartment)
        targetCollege = Department(targetDepartment)%CollegeIdx
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
                begintr//begintd//JUSTNONE//endtd//tdalignright
            if (is_admin_of_college(targetCollege)) then
                write(device,AFORMAT) trim(make_href(fnEditRoom, 'Add room', &
                    A1='TBA', pre=beginsmall//'('//nbsp, post=' )'//endsmall ))
            end if
            write(device,AFORMAT) endtd//endtr//endtable
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

            if (is_admin_of_college(targetCollege) )then
                write(device,AFORMAT) trim(make_href(fnEditRoom, 'Add room', &
                    A1='TBA', pre=beginsmall//'('//nbsp, post=' )'//endsmall ))//linebreak
            end if

            write(device,AFORMAT) beginitalic//'Note: Number under Term links to classes in the room.'//enditalic//linebreak, &
                '<table border="0" width="75%">'//&
                begintr//thalignleft//'Code'//endth// &
                thaligncenter//'Cluster'//endth, &
                thaligncenter//'Capacity'//endth
            do tTerm=termBegin,termEnd
                call qualify_term (tTerm, tYear, term)
                write(device,AFORMAT) &
                    thaligncenter//txtSemester(term+6)//termQualifier(term+6)//linebreak// &
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
                    call qualify_term (tTerm, tYear, term)
                    call timetable_clear(TimeTable)
                    ! collect classes in room
                    do sdx=1,NumSections(term)
                        call meetings_of_section_in_room(Section(term,0:), sdx, rdx, n_meetings, meetings)
                        if (n_meetings==0) cycle ! room not assigned to this section
                        nsect(term) = nsect(term)+1
                        ierr = -10
                        call timetable_add_meetings_of_section(Section(term,0:), sdx, n_meetings, meetings, TimeTable, ierr)
                        if (ierr /= -10) then
                            mesg = trim(mesg)//SPACE//txtSemester(term+6)
                        end if
                    end do
                end do
                if (len_trim(mesg)>0) mesg = red//'Conflict: '//trim(mesg)//black

                QUERY_put = Room(rdx)%Code
                write(device,AFORMAT) begintr//begintd//trim(Room(rdx)%Code)
                if ( is_dean_of_college(Department(Room(rdx)%DeptIdx)%CollegeIdx, orHigherUp) ) then
                    write(device,AFORMAT) trim(make_href(fnEditRoom, 'Edit', &
                        A1=QUERY_put, pre=nbsp//beginsmall, post=endsmall))
                end if
                write(device,AFORMAT) &
                    endtd//tdaligncenter//trim(itoa(Room(rdx)%Cluster))//endtd// &
                    tdaligncenter//trim(itoa(Room(rdx)%MaxCapacity))//endtd

                do tTerm=termBegin,termEnd
                    call qualify_term (tTerm, tYear, term)
                    write(device,AFORMAT) trim(make_href(fnRoomSchedule, itoa(nsect(term)), &
                        A1=QUERY_put, A9=term, pre=tdaligncenter, post=endtd))
                end do

                write(device,AFORMAT) tdaligncenter//trim(mesg)//endtd//endtr
            end do
            write(device,AFORMAT) endtable
        end if
        write(device,AFORMAT) horizontal

    end subroutine room_list_all



    subroutine room_conflicts (device, thisTerm, NumSections, Section)
        integer, intent (in) :: device, thisTerm
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
                call meetings_of_section_in_room(Section, sdx, rdx, n_meetings, meetings)
                if (n_meetings>0) then ! room assigned to this section
                    ierr = -10
                    call timetable_add_meetings_of_section(Section, sdx, n_meetings, meetings, TimeTable, ierr)
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
            write(device,AFORMAT) BRNONE
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
                    call meetings_of_section_in_room(Section, sdx, rdx, n_meetings, meetings)
                    if (n_meetings>0) then ! room assigned to this section
                        nsect = nsect+1
                        tArray(n_count+nsect) = sdx
                        ierr = -10
                        call timetable_add_meetings_of_section(Section, sdx, n_meetings, meetings, TimeTable, ierr)
                        if (ierr /= -10) then
                            mesg = red//'Conflict!'//black
                        end if
                    end if
                end do
                QUERY_put = Room(rdx)%Code
                write(device,AFORMAT) begintr//begintd//trim(Room(rdx)%Code)
                if ( is_dean_of_college(Department(Room(rdx)%DeptIdx)%CollegeIdx, orHigherUp) ) then
                    write(device,AFORMAT) trim(make_href(fnEditRoom, 'Edit', &
                        A1=QUERY_put, pre=nbsp//beginsmall, post=endsmall))
                end if
                write(device,AFORMAT) &
                    endtd//tdaligncenter//trim(itoa(Room(rdx)%Cluster))//endtd// &
                    tdaligncenter//trim(itoa(Room(rdx)%MaxCapacity))//endtd
                !if (nsect>0) then
                write(device,AFORMAT) trim(make_href(fnRoomSchedule, itoa(nsect), &
                    A1=QUERY_put, A9=thisTerm, pre=tdaligncenter, post=endtd))
                !else
                !  write(device,AFORMAT) tdaligncenter//trim(itoa(nsect))//endtd
                !end if
                write(device,AFORMAT) tdaligncenter//trim(mesg)//endtd//endtr
            end do
            write(device,AFORMAT) endtable
        end if
        write(device,AFORMAT) horizontal

    end subroutine room_conflicts


    subroutine room_schedule(device, thisTerm, NumSections, Section, LoadSource)
        integer, intent(in), optional :: LoadSource
        integer, intent (in) :: device, thisTerm
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
        targetCollege = Department(targetDepartment)%CollegeIdx
        mesg = SPACE
        allowed_to_edit = is_admin_of_college(targetCollege) .or. &
            ( is_chair_of_department(targetDepartment,orHigherUp) .and. &
              ( (thisTerm==currentTerm .and. isPeriodOne) .or. &
                (thisTerm==nextTerm .and. (.not. isPeriodOne)) ) )

        ! check if there are other arguments
        call cgi_get_named_string(QUERY_STRING, 'A2', tAction, ierr)

        if (ierr==0) then ! action is Add or Del
            call cgi_get_named_string(QUERY_STRING, 'A3', tClassId, ierr)
            sect = index_to_section(tClassId, NumSections, Section)
            if (sect>0 .and. .not. isRoleofficial) then ! target of action is indexed by sect
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

            if (isRoleOfficial) then
                mesg = '"'//trim(tAction)//SPACE//trim(tClassId)//'" failed. '//sorryMessage
            end if

        end if

        call html_write_header(device, 'Classes in room '//tRoom, mesg)

        ! collect classes in room rdx
        call timetable_meetings_in_room(NumSections, Section, targetRoom, 0, tLen1, tArray, TimeTable, conflicted)

        call list_sections_to_edit(device, thisTerm, Section, tLen1, tArray, fnRoomSchedule, tRoom, 'Del', allowed_to_edit)

        if (tLen1>0) then
            call timetable_display(device, Section, TimeTable)
            write(device,AFORMAT) horizontal
        end if

        if (isRoleStudent .or. isRoleGuest) return

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
            if (.not. is_conflict_timetable_with_section(Section, sdx, TimeTable)) then ! add to list
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
            call list_sections_to_edit(device, thisTerm, Section, tLen2, tArray(tLen1+1), fnRoomSchedule, tRoom, 'Add', &
            allowed_to_edit, beginbold//'Classes with TBA rooms in '//trim(Department(LoadFromDept)%Code)// &
            ' that fit available times in '//trim(tRoom)//endbold)
        end if

        ! search for feasible classes in another department?
        if ( .not. isRoleFaculty ) then
            call make_form_start(device, fnRoomSchedule, tRoom, A9=thisTerm)
            write(device,AFORMAT) linebreak//'Search for feasible classes in : <select name="A4">'
            do mdx=2,NumDepartments
                if (mdx/=LoadFromDept) then
                    ierr = 0
                else
                    ierr = 1
                end if
                write(device,AFORMAT) '<option value="'//trim(Department(mdx)%Code)//'"'//trim(selected(ierr))//'> '// &
                trim(Department(mdx)%Code)//DASH//trim(Department(mdx)%Name)
            end do
            write(device,AFORMAT) '</select>'//nbsp//'<input type="submit" value="Find classes">'//endform
        end if
        write(device,AFORMAT) horizontal

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

        write(device,AFORMAT) endtable//linebreak//nbsp// &
            '<input name="action" type="submit" value="'//trim(tAction)//'">'//endform//'<pre>', &
            'NOTE: Rooms that are within walking distance of each other must belong to the same cluster.', &
            '</pre>'//horizontal

    end subroutine room_info


    subroutine subject_edit(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject, tAction, token
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer :: ierr, crse, i, j
        character (len=255) :: mesg, remark
        type (TYPE_SUBJECT) :: wrk
        logical :: isDirtySUBJECTS, criticalErr

        call html_comment('subject_edit()')

        ! which subject ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tSubject, crse)
        crse = index_to_subject(tSubject)
        wrk = Subject(crse) ! make a working copy

        ! check for other arguments
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
        isDirtySUBJECTS = .false.
        remark = SPACE

        if (trim(tAction)=='Update' .and. .not. isRoleOfficial) then

                call cgi_get_named_float(QUERY_STRING, 'Units', wrk%Units, ierr)
                if (ierr/=0) wrk%Units = Subject(crse)%Units
                if ( wrk%Units /= Subject(crse)%Units) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Units changed to '//ftoa(wrk%Units,1)
                end if

                call cgi_get_named_float(QUERY_STRING, 'Tuition', wrk%Tuition, ierr)
                if (ierr/=0) wrk%Tuition = Subject(crse)%Tuition
                if ( wrk%Tuition /= Subject(crse)%Tuition) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Tuition fee changed to '//ftoa(wrk%Tuition,2)
                end if

                call cgi_get_named_float(QUERY_STRING, 'LabFee', wrk%LabFee, ierr)
                if (ierr/=0) wrk%LabFee = Subject(crse)%LabFee
                if ( wrk%LabFee /= Subject(crse)%LabFee) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Lab fee changed to '//ftoa(wrk%LabFee,2)
                end if

                call cgi_get_named_float(QUERY_STRING, 'LectHours', wrk%LectHours, ierr)
                if (ierr/=0) wrk%LectHours = Subject(crse)%LectHours
                if ( wrk%LectHours /= Subject(crse)%LectHours) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Lecture Hours changed to '//ftoa(wrk%LectHours,2)
                end if

                call cgi_get_named_float(QUERY_STRING, 'LectLoad', wrk%LectLoad, ierr)
                if (ierr/=0) wrk%LectLoad = Subject(crse)%LectLoad
                if ( wrk%LectLoad /= Subject(crse)%LectLoad) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Lecture workload changed to '//ftoa(wrk%LectLoad,2)
                end if

                call cgi_get_named_integer(QUERY_STRING, 'MinLectSize', wrk%MinLectSize, ierr)
                if (ierr/=0) wrk%MinLectSize = Subject(crse)%MinLectSize
                if ( wrk%MinLectSize /= Subject(crse)%MinLectSize) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': MinLectSize changed to '//itoa(wrk%MinLectSize)
                end if

                call cgi_get_named_integer(QUERY_STRING, 'MaxLectSize', wrk%MaxLectSize, ierr)
                if (ierr/=0) wrk%MaxLectSize = Subject(crse)%MaxLectSize
                if ( wrk%MaxLectSize /= Subject(crse)%MaxLectSize) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': MaxLectSize changed to '//itoa(wrk%MaxLectSize)
                end if

                call cgi_get_named_float(QUERY_STRING, 'LabHours', wrk%LabHours, ierr)
                if (ierr/=0) wrk%LabHours = Subject(crse)%LabHours
                if ( wrk%LabHours /= Subject(crse)%LabHours) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': LabHours changed to '//ftoa(wrk%LabHours,2)
                end if

                call cgi_get_named_float(QUERY_STRING, 'LabLoad', wrk%LabLoad, ierr)
                if (ierr/=0) wrk%LabLoad = Subject(crse)%LabLoad
                if ( wrk%LabLoad /= Subject(crse)%LabLoad) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Lab workload changed to '//ftoa(wrk%LabLoad,2)
                end if

                call cgi_get_named_integer(QUERY_STRING, 'MinLabSize', wrk%MinLabSize, ierr)
                if (ierr/=0) wrk%MinLabSize = Subject(crse)%MinLabSize
                if ( wrk%MinLabSize /= Subject(crse)%MinLabSize) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': MinLabSize changed to '//itoa(wrk%MinLabSize)
                end if

                call cgi_get_named_integer(QUERY_STRING, 'MaxLabSize', wrk%MaxLabSize, ierr)
                if (ierr/=0) wrk%MaxLabSize = Subject(crse)%MaxLabSize
                if ( wrk%MaxLabSize /= Subject(crse)%MaxLabSize) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': MaxLabSize changed to '//itoa(wrk%MaxLabSize)
                end if

                call cgi_get_named_string(QUERY_STRING, 'Name', mesg, ierr)
                wrk%Name  = trim(mesg)
                if (ierr/=0) wrk%Name = Subject(crse)%Name
                if ( wrk%Name /= Subject(crse)%Name) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Name changed to '//wrk%Name
                end if

                call cgi_get_named_string(QUERY_STRING, 'Title', mesg, ierr)
                wrk%Title = trim(mesg)
                if (ierr/=0) wrk%Title = Subject(crse)%Title
                if ( wrk%Title /= Subject(crse)%Title) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Title changed to '//wrk%Title
                end if

                call cgi_get_named_string(QUERY_STRING, 'Department', tDepartment, ierr)
                wrk%DeptIdx = index_to_dept(tDepartment)
                if (ierr/=0 .or. wrk%DeptIdx<=0) wrk%DeptIdx = Subject(crse)%DeptIdx
                if ( wrk%DeptIdx /= Subject(crse)%DeptIdx) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Department changed to '//Department(wrk%DeptIdx)%Code
                end if

                call cgi_get_named_string(QUERY_STRING, 'TermOffered', token, ierr)
                j = 0
                if (index(token, '1')>0 ) j = j+1
                if (index(token, '2')>0 ) j = j+2
                if (index(token, 'S')>0 ) j = j+4
                wrk% TermOffered= j
                if (ierr/=0 .or. j==0) wrk%TermOffered = Subject(crse)%TermOffered
                if ( wrk%TermOffered /= Subject(crse)%TermOffered) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': TermOffered changed to '//text_term_offered_separated(wrk%TermOffered)
                end if

                call cgi_get_named_string(QUERY_STRING, 'Prerequisite', mesg, ierr)
                call tokenize_subjects(mesg, '+', MAX_ALL_SUBJECT_PREREQ, wrk%lenPreq, wrk%Prerequisite, ierr)
                if (ierr/=0) then
                    wrk%lenPreq = Subject(crse)%lenPreq
                    wrk%Prerequisite = Subject(crse)%Prerequisite
                end if


                if ( wrk%lenPreq /= Subject(crse)%lenPreq) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': lenPreq changed to '//itoa(wrk%lenPreq)
                else
                    do j=1,wrk%lenPreq
                        if (wrk%Prerequisite(j) == Subject(crse)%Prerequisite(j)) cycle
                        isDirtySUBJECTS = .true.
                        remark = trim(remark)//': Prerequisite changed'
                    end do
                end if

                call cgi_get_named_string(QUERY_STRING, 'Corequisite', mesg, ierr)
                call tokenize_subjects(mesg, '+', MAX_ALL_SUBJECT_COREQ, wrk%lenCoreq, wrk%Corequisite, ierr)
                if (ierr/=0) then
                    wrk%lenCoreq = Subject(crse)%lenCoreq
                    wrk%Corequisite = Subject(crse)%Corequisite
                end if
                if ( wrk%lenCoreq /= Subject(crse)%lenCoreq) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': lenCoreq changed to '//itoa(wrk%lenCoreq)
                else
                    do j=1,wrk%lenCoreq
                        if (wrk%Corequisite(j) == Subject(crse)%Corequisite(j)) cycle
                        isDirtySUBJECTS = .true.
                        remark = trim(remark)//': Corequisite changed'
                    end do
                end if

                call cgi_get_named_string(QUERY_STRING, 'Concurrent', mesg, ierr)
                call tokenize_subjects(mesg, '+', MAX_ALL_SUBJECT_PREREQ, wrk%lenConc, wrk%Concurrent, ierr)
                if (ierr/=0) then
                    wrk%lenConc = Subject(crse)%lenConc
                    wrk%Concurrent = Subject(crse)%Concurrent
                end if
                if ( wrk%lenConc /= Subject(crse)%lenConc) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': lenConc changed to '//itoa(wrk%lenConc)
                else
                    do j=1,wrk%lenConc
                        if (wrk%Concurrent(j) == Subject(crse)%Concurrent(j)) cycle
                        isDirtySUBJECTS = .true.
                        remark = trim(remark)//': Concurrent changed'
                    end do
                end if

                call cgi_get_named_string(QUERY_STRING, 'ConcPrerequisite', mesg, ierr)
                call tokenize_subjects(mesg, '+', MAX_ALL_SUBJECT_CONCPREQ, wrk%lenConcPreq, wrk%ConcPrerequisite, ierr)
                if (ierr/=0) then
                    wrk%lenConcPreq = Subject(crse)%lenConcPreq
                    wrk%ConcPrerequisite = Subject(crse)%ConcPrerequisite
                end if
                if ( wrk%lenConcPreq /= Subject(crse)%lenConcPreq) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': lenConcPreq changed to '//itoa(wrk%lenConcPreq)
                else
                    do j=1,wrk%lenConcPreq
                        if (wrk%ConcPrerequisite(j) == Subject(crse)%ConcPrerequisite(j)) cycle
                        isDirtySUBJECTS = .true.
                        remark = trim(remark)//': Prerequisite that can be taken concurrently changed'
                    end do
                end if

                if (isDirtySUBJECTS) then
                    if ( wrk%Name /= Subject(crse)%Name) then
                        ! add new subject?
                        j = index_to_subject(wrk%Name)
                        if (j==0) then
                            call check_array_bound (NumSubjects+NumAdditionalSubjects+1, MAX_ALL_SUBJECTS, 'MAX_ALL_SUBJECTS', &
                                criticalErr)
                            if (criticalErr) then
                                targetDepartment = DeptIdxUser
                                targetCollege = CollegeIdxUser
                                call html_college_links(device, targetCollege, &
                                    'No more space for additional subject')
                                return
                            else
                                NumAdditionalSubjects = NumAdditionalSubjects+1
                                Subject(NumSubjects+NumAdditionalSubjects) = wrk
                                crse = NumSubjects+NumAdditionalSubjects
                                tSubject = wrk%Name
                                remark = ': Added new subject '//wrk%Name
                                call get_subject_areas()
                            end if
                        else
                            remark = ': Add new subject failed; "'//trim(wrk%Name)//'" already exists.'
                        end if
                    else
                        ! update existing
                        Subject(crse) = wrk
                    end if
                end if

        end if ! (trim(tAction)=='Update' .and. .not.

        if (len_trim(tAction)>0 .and. isRoleOfficial) then
            remark = '  Update "'//trim(tSubject)//'" failed. '//sorryMessage
        end if


        if (isDirtySUBJECTS) call xml_write_subjects(trim(pathToYear)//'SUBJECTS.XML')

        targetDepartment = Subject(crse)%DeptIdx

        call html_write_header(device, 'Edit subject '//tSubject, remark(3:))
        j = index(tSubject,SPACE)
        if ( j<len_trim(tSubject) ) then
            write(device,AFORMAT) &
                trim(make_href(fnSubjectList, tSubject(:j-1), A1=tSubject(:j-1), &
                pre=beginsmall//beginitalic//'Edit another '//nbsp, post=' subject'//enditalic//endsmall))
        end if

        call make_form_start(device, fnEditSubject, tSubject)
        write(device,AFORMAT)  '<table border="0" width="100%">', &
            begintr//begintd//'Subject code'//endtd//begintd//'<input name="Name" size="'//trim(itoa(MAX_LEN_SUBJECT_CODE))// &
            '" value="'//trim(tSubject)//'"> (A new subject will be created if this is changed)'//endtd//endtr
        !begintr//begintd//'Subject code'//endtd//begintd//trim(tSubject)//' (Cannot be changed)'//endtd//endtr
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
            begintr//begintd//'Title'//endtd//begintd//'<input name="Title" size="'//trim(itoa(MAX_LEN_SUBJECT_TITLE))// &
            '" value="'//trim(Subject(crse)%Title)//'">'//endtd//endtr, &
            begintr//begintd//'Units'//endtd//begintd//'<input name="Units" size="3" value="'// &
            trim(ftoa(Subject(crse)%Units,1))//'"> (0, if non-credit; i.e., PE, NSTP)'//endtd//endtr, &
            begintr//begintd//'Term Offered'//endtd//begintd//'<input name="TermOffered" size="3" value="'// &
            trim(text_term_offered_separated(Subject(crse)%TermOffered))//'"> (1, 2, S, or combination)'//endtd//endtr, &
            begintr//begintd//'Tuition fee'//endtd//begintd//'<input name="Tuition" size="3" value="'// &
            trim(ftoa(Subject(crse)%Tuition,2))//'"> (total amount)'//endtd//endtr, &
            begintr//begintd//'Lab fee'//endtd//begintd//'<input name="LabFee" size="3" value="'// &
            trim(ftoa(Subject(crse)%LabFee,2))//'"> (additional to tuition)'//endtd//endtr
        write(device,AFORMAT)  &
            begintr//begintd//'Hours lecture class'//endtd//begintd//'<input name="LectHours" size="3" value="'// &
            trim(ftoa(Subject(crse)%LectHours,2))//'"> (0, if no lecture component)'//endtd//endtr, &
            begintr//begintd//'Workload for lecture class'//endtd//begintd//'<input name="LectLoad" size="3" value="'// &
            trim(ftoa(Subject(crse)%LectLoad,2))//'"> (0, if no lecture component)'//endtd//endtr, &
            begintr//begintd//'Min size lecture class'//endtd//begintd//'<input name="MinLectSize" size="3" value="'// &
            trim(itoa(Subject(crse)%MinLectSize))//'">'//endtd//endtr, &
            begintr//begintd//'Max size lecture class'//endtd//begintd//'<input name="MaxLectSize" size="3" value="'// &
            trim(itoa(Subject(crse)%MaxLectSize))//'">'//endtd//endtr, &
            begintr//begintd//'Hours lab/recit/comp class'//endtd//begintd//'<input name="LabHours" size="3" value="'// &
            trim(ftoa(Subject(crse)%LabHours,2))//'"> (0, if no lab/recit/computations component)'//endtd//endtr, &
            begintr//begintd//'Workload for lab class'//endtd//begintd//'<input name="LabLoad" size="3" value="'// &
            trim(ftoa(Subject(crse)%LabLoad,2))//'"> (0, if no lab/recit/computations component)'//endtd//endtr, &
            begintr//begintd//'Min size lab/recit/comp class'//endtd//begintd//'<input name="MinLabSize" size="3" value="'// &
            trim(itoa(Subject(crse)%MinLabSize))//'">'//endtd//endtr, &
            begintr//begintd//'Max size lab/recit/comp class'//endtd//begintd//'<input name="MaxLabSize" size="3" value="'// &
            trim(itoa(Subject(crse)%MaxLabSize))//'">'//endtd//endtr
        !      lenPreq, Prerequisite(MAX_ALL_SUBJECT_PREREQ), &
        i = Subject(crse)%Prerequisite(1)
        mesg = Subject(i)%Name
        do j=2,Subject(crse)%lenPreq
            i = Subject(crse)%Prerequisite(j)
            mesg = trim(mesg)//'+'//Subject(i)%Name
        end do
        write(device,AFORMAT) &
            begintr//begintd//'Prerequisite'//endtd//begintd//'<input name="Prerequisite" size="'// &
            trim(itoa(Subject(crse)%lenPreq*2*MAX_LEN_SUBJECT_CODE/3))//'" value="'//trim(mesg)//'">'//endtd//endtr
        !      lenCoreq, Corequisite(MAX_ALL_SUBJECT_COREQ), &
        i = Subject(crse)%Corequisite(1)
        mesg = Subject(i)%Name
        do j=2,Subject(crse)%lenCoreq
            i = Subject(crse)%Corequisite(j)
            mesg = trim(mesg)//'+'//Subject(i)%Name
        end do
        write(device,AFORMAT) &
            begintr//begintd//'Corequisite'//endtd//begintd//'<input name="Corequisite" size="'// &
            trim(itoa(Subject(crse)%lenCoreq*2*MAX_LEN_SUBJECT_CODE/3))//'" value="'//trim(mesg)//'">'//endtd//endtr
        !      lenConc, Concurrent(MAX_ALL_SUBJECT_CONCURRENT), &
        i = Subject(crse)%Concurrent(1)
        mesg = Subject(i)%Name
        do j=2,Subject(crse)%lenConc
            i = Subject(crse)%Concurrent(j)
            mesg = trim(mesg)//'+'//Subject(i)%Name
        end do
        write(device,AFORMAT) &
            begintr//begintd//'Concurrent with'//endtd//begintd//'<input name="Concurrent" size="'// &
            trim(itoa(Subject(crse)%lenConc*2*MAX_LEN_SUBJECT_CODE/3))//'" value="'//trim(mesg)//'">'//endtd//endtr
        !      lenConcPreq, ConcPrerequisite(MAX_ALL_SUBJECT_CONCPREQ)
        i = Subject(crse)%ConcPrerequisite(1)
        mesg = Subject(i)%Name
        do j=2,Subject(crse)%lenConcPreq
            i = Subject(crse)%ConcPrerequisite(j)
            mesg = trim(mesg)//'+'//Subject(i)%Name
        end do
        write(device,AFORMAT) &
            begintr//begintd//'Prerequisite that can be concurrent'//endtd//begintd//'<input name="ConcPrerequisite" size="'// &
            trim(itoa(Subject(crse)%lenConcPreq*2*MAX_LEN_SUBJECT_CODE/3))//'" value="'//trim(mesg)//'">'//endtd//endtr

        write(device,AFORMAT) endtable//linebreak//nbsp//'<input name="action" type="submit" value="Update">'//endform//'<pre>', &
            'NOTE: The prerequisite is in prefix notation, the tokens being separated by the "+" symbol. Patterns are', &
            '   1. NONE - no prerequisite', &
            '   2. COI  - Consent of Instructor', &
            '   3. Student classification (FRESHMAN, SOPHOMORE, JUNIOR, SENIOR)', &
            '   4. year level in curriculum (FIRST, SECOND, THIRD, FOURTH, FIFTH, GRADUATING)', &
            '   5. SUBJECT CODE - i.e., another subject', &
            '   6. OR+2+5, OR+3+5, OR+4+5', &
            '   7. OR+5+5, AND+5+5', &
            '   8. OR+6+6, OR+6+7, OR+7+7, AND+7+7', &
            '</pre>'//horizontal


    end subroutine subject_edit


    subroutine subject_list_all (device)
        integer, intent (in) :: device
        integer :: crse, idx, nSubjects, ierr, ncol, maxcol=7
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject

        ! which department ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)
        targetDepartment = index_to_dept(tDepartment)
        if (targetDepartment>1) then
            targetCollege = Department(targetDepartment)%CollegeIdx
            nSubjects = 0
#if defined REGIST
            do idx=1,NumSubjects+NumAdditionalSubjects
                if (Subject(idx)%DeptIdx == targetDepartment) then
                    nSubjects = nSubjects+1
                    tArray(nSubjects) = idx
                end if
            end do
#else
            ! Subjects administered by program
            do idx=1,NumSubjects+NumAdditionalSubjects
                if (is_used_in_college_subject(targetCollege, idx) ) then
                    nSubjects = nSubjects+1
                    tArray(nSubjects) = idx
                end if
            end do
#endif
            call html_write_header(device, 'Subjects in '//Department(targetDepartment)%Name)
        else ! try subject area
            if (ierr/=0 .or. tDepartment==SPACE) then
                targetDepartment = DeptIdxUser
                targetCollege = CollegeIdxUser
                call html_write_header(device, 'List of subjects', horizontal//linebreak//'Department or subject area not found')
                return
            else
                ! make list of subjects in subject area to display
                targetDepartment = DeptIdxUser
                nSubjects = 0
                do idx=1,NumSubjects+NumAdditionalSubjects
                    if (index(Subject(idx)%Name,trim(tDepartment)//SPACE)==1) then
                        nSubjects = nSubjects + 1
                        !write(*,*) nSubjects, Subject(idx)%Name
                        tArray(nSubjects) = idx
                        targetDepartment = Subject(idx)%DeptIdx
                    end if
                end do
                call html_write_header(device, '"'//trim(tDepartment)//'" subjects')
            end if
        end if

        if (nSubjects>0) then

            ! write shortcut to subjects
            write(device,AFORMAT) '<table border="0" width="100%">'
            ncol = 0
            do idx=1,nSubjects
                tSubject = Subject(tArray(idx))%Name
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
                do idx=ncol+1,maxcol
                    write(device,AFORMAT) tdnbspendtd
                end do
                write(device,AFORMAT) endtr
            end if
            write(device,AFORMAT) endtable//horizontal//linebreak

            write(device,AFORMAT) '<table border="0" cellspacing="0" width="100%">'//beginsmall
            do idx=1,nSubjects
                crse = tArray(idx)
                tSubject = Subject(crse)%Name

                if (mod(idx,6)==1) write(device,AFORMAT) begintr//thalignleft//'Subject'//endth// &
                    beginth//'Units'//endth//beginth//'Term'//endth//beginth//'Tuition'//endth//beginth//'Lab fee'//endth// &
                    beginth//'Lect hrs'//endth//beginth//'Lect load'//endth//beginth//'Min Size'//endth// &
                    beginth//'Max Size'//endth,  &
                    beginth//'Lab hrs'//endth//beginth//'Lab load'//endth//beginth//'Min Size'//endth// &
                    beginth//'Max Size'//endth//endtr, &
                    begintr//'<td colspan="11">'//nbsp//endtd//endtr

                write(device,AFORMAT) begintr//begintd//'<a name="'//trim(tSubject)//'">'//beginbold//'Name:'//endbold
                if (isRoleSysAd .or. isRoleOfficial) then
                    write(device,AFORMAT) trim(make_href(fnEditSubject, tSubject, A1=tSubject))
                else
                    write(device,AFORMAT) tSubject
                end if
                write(device,AFORMAT) '</a>'//endtd, &
                    tdaligncenter//trim(ftoa(Subject(crse)%Units,1))//endtd// &
                    tdaligncenter//trim(text_term_offered(Subject(crse)%TermOffered))//endtd// &
                    tdaligncenter//trim(ftoa(Subject(crse)%Tuition,2))//endtd// &
                    tdaligncenter//trim(ftoa(Subject(crse)%LabFee,2))//endtd// &
                    tdaligncenter//trim(ftoa(Subject(crse)%LectHours,2))//endtd// &
                    tdaligncenter//trim(ftoa(Subject(crse)%LectLoad,2))//endtd// &
                    tdaligncenter//trim(itoa(Subject(crse)%MinLectSize))//endtd// &
                    tdaligncenter//trim(itoa(Subject(crse)%MaxLectSize))//endtd,  &
                    tdaligncenter//trim(ftoa(Subject(crse)%LabHours,2))//endtd// &
                    tdaligncenter//trim(ftoa(Subject(crse)%LabLoad,2))//endtd// &
                    tdaligncenter//trim(itoa(Subject(crse)%MinLabSize))//endtd// &
                    tdaligncenter//trim(itoa(Subject(crse)%MaxLabSize))//endtd//endtr

                write(device,AFORMAT) &
                    begintr//'<td colspan="13">'//beginbold//'Title:'//endbold//trim(Subject(crse)%Title)//endtd//endtr, &
                    begintr//'<td colspan="13">'//beginbold//'Preq.'//endbold//trim(text_prerequisite_of_subject(crse,0))// &
                        endtd//endtr, &
                    begintr//'<td colspan="13">'//nbsp//endtd//endtr
            end do

            write(device,AFORMAT) endsmall//endtable

        else
            write(device,AFORMAT) linebreak//'No subjects in this college?'
        end if
        write(device,AFORMAT) horizontal

    end subroutine subject_list_all



    subroutine curriculum_edit(device, given)
        integer, intent(in), optional :: given
        integer, intent (in) :: device
        integer :: crse, i, j, k, ierr, idx, tdx, m, Year, Term, ptrS
        real :: tUnits, credit
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum, tAction
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=10) :: tStatus ! (ACTIVE)/(INACTIVE)

        character (len=255) :: mesg, remark, tokenizeErr
        type (TYPE_CURRICULUM) :: wrk
        logical :: changed, possibleImpact, critical1, critical2, critical3
        integer, dimension(MAX_SECTION_MEETINGS) :: subjectList

        ! which curriculum
        if (present(given)) then
            targetCurriculum = given
            tdx = 0
        else
            call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, tdx)
            targetCurriculum = index_to_curriculum(tCurriculum)
        end if

        wrk = Curriculum(targetCurriculum) ! make a working copy
        targetCollege = Curriculum(targetCurriculum)%CollegeIdx

        ! check for other arguments
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
        changed = .false.
        remark = SPACE

        if (len_trim(tAction)>0 .and. isRoleOfficial) then

            remark = '  Update "'//trim(Curriculum(targetCurriculum)%Code)//'" not enabled. '//sorryMessage

        else

        select case (trim(tAction))

            case ('Update')

                call cgi_get_named_string(QUERY_STRING, 'Code', wrk%Code, ierr)
                if (ierr/=0) wrk%Code = Curriculum(targetCurriculum)%Code
                if ( wrk%Code /= Curriculum(targetCurriculum)%Code) then
                    changed = .true.
                    remark = trim(remark)//': Code changed to '//wrk%Code
                    call html_comment('Code changed to '//wrk%Code)
                end if

                call cgi_get_named_string(QUERY_STRING, 'College', tCollege, ierr)
                wrk%CollegeIdx = index_to_college(tCollege)

                if (ierr/=0 .or. wrk%CollegeIdx<=0) wrk%CollegeIdx = Curriculum(targetCurriculum)%CollegeIdx

                if ( wrk%CollegeIdx /= Curriculum(targetCurriculum)%CollegeIdx) then
                    changed = .true.
                    remark = trim(remark)//': College changed to '//College(wrk%CollegeIdx)%Code
                    call html_comment('College changed to '//College(wrk%CollegeIdx)%Code)
                end if

                call cgi_get_named_string(QUERY_STRING, 'Title', wrk%Title, ierr)

                if (ierr/=0) wrk%Title = Curriculum(targetCurriculum)%Title

                if ( wrk%Title /= Curriculum(targetCurriculum)%Title) then
                    changed = .true.
                    remark = trim(remark)//': Title changed to '//wrk%Title
                    call html_comment('Title changed to '//wrk%Title)
                end if

                call cgi_get_named_string(QUERY_STRING, 'Specialization', wrk%Specialization, ierr)

                if (ierr/=0) wrk%Specialization = Curriculum(targetCurriculum)%Specialization

                if ( wrk%Specialization /= Curriculum(targetCurriculum)%Specialization) then
                    changed = .true.
                    remark = trim(remark)//': Specialization changed to '//wrk%Specialization
                    call html_comment('Specialization changed to '//wrk%Specialization)
                end if

                call cgi_get_named_string(QUERY_STRING, 'Remark', wrk%Remark, ierr)

                if (ierr/=0) wrk%Remark = Curriculum(targetCurriculum)%Remark

                if ( wrk%Remark /= Curriculum(targetCurriculum)%Remark) then
                    changed = .true.
                    remark = trim(remark)//': Remark changed to '//wrk%Remark
                    call html_comment('Remark changed to '//wrk%Remark)
                end if

                call cgi_get_named_string(QUERY_STRING, 'Status', tStatus, ierr)
                wrk%Active = tStatus=='Active'

                if (ierr/=0) wrk%Active = Curriculum(targetCurriculum)%Active

                if ( wrk%Active .neqv. Curriculum(targetCurriculum)%Active) then
                    changed = .true.
                    remark = trim(remark)//': Status changed to '//tStatus
                    call html_comment('Status changed to '//tStatus)
                end if

                ! initialize list of subjects
                wrk%NumTerms = 0
                wrk%NSubjects = 0
                wrk%SubjectIdx = 0
                wrk%SubjectTerm = 0
                ! collect subjects
                do tdx=1,Curriculum(targetCurriculum)%NumTerms+6
                    call rank_to_year_term(tdx, Year, Term)
                    call cgi_get_named_string(QUERY_STRING, 'Subjects'//trim(itoa(tdx)), mesg, ierr)
                    if (len_trim(mesg)==0) cycle
                    call tokenize_subjects(mesg, COMMA, MAX_SECTION_MEETINGS, m, subjectList, ierr, tokenizeErr)
                    if (len_trim(tokenizeErr)>0) remark = trim(remark)//' : '//tokenizeErr
                    if (m>0) then
                        if (len_trim(tokenizeErr)>0) call html_comment('Token error: '//tokenizeErr)
                        do i=1,m
                            if (subjectList(i)==INDEX_TO_NONE) cycle
                            idx = wrk%NSubjects + i
                            wrk%SubjectIdx(idx) = subjectList(i)
                            wrk%SubjectTerm(idx) = tdx
                        end do
                        wrk%NSubjects = wrk%NSubjects + m
                        wrk%NumTerms = tdx
                    end if
                end do
                ! check deleted subjects from original curriculum
                do idx=1,Curriculum(targetCurriculum)%NSubjects
                    crse = Curriculum(targetCurriculum)%SubjectIdx(idx)
                    tdx = Curriculum(targetCurriculum)%SubjectTerm(idx)
                    possibleImpact = .false.
                    i = index_of_subject_in_curriculum (wrk, crse)
                    if (i==0) then ! crse not in wrk, deleted from targetCurriculum
                        changed = .true.
                        possibleImpact = .true.
                        remark = trim(remark)//': Deleted '//Subject(crse)%Name
                        call html_comment('>>> Deleted '//Subject(crse)%Name)
                    else ! crse retained; check if moved to another term
                        if (tdx/=wrk%SubjectTerm(i)) then ! but moved to a different semester
                            changed = .true.
                            possibleImpact = .true.
                            remark = trim(remark)//': Moved '//Subject(crse)%Name
                            call html_comment('>>> Moved '//Subject(crse)%Name)
                        end if
                    end if
                    !if (possibleImpact) then ! check if crse is used in a block section
                    !    call rank_to_year_term(tdx, Year, Term)
                    !    do i=1,NumBlocks
                    !        if (Block(i)%CurriculumIdx==targetCurriculum .and. &
                    !            Block(i)%Year==Year .and. Block(i)%Term==Term) then
                    !            remark = trim(remark)//', affects '//Block(i)%BlockID
                    !            call html_comment('>>> Change in '//Subject(crse)%Name//'may affect '//Block(i)%Name)
                    !        end if
                    !    end do
                    !end if
                end do

                ! check for additional subjects to original curriculum
                do idx=1,wrk%NSubjects
                    crse = wrk%SubjectIdx(idx)
                    tdx = wrk%SubjectTerm(idx)
                    i = index_of_subject_in_curriculum (Curriculum(targetCurriculum), crse)
                    if (i==0) then ! crse added to targetCurriculum
                        changed = .true.
                        remark = trim(remark)//': Added '//Subject(crse)%Name
                        call html_comment('>>> Added '//Subject(crse)%Name)
                        !call rank_to_year_term(tdx, Year, Term)
                        !do i=1,NumBlocks
                        !    if (Block(i)%CurriculumIdx==targetCurriculum .and. &
                        !        Block(i)%Year==Year .and. Block(i)%Term==Term) then
                        !        remark = trim(remark)//', affects '//Block(i)%BlockID
                        !        call html_comment('>>> Addition of '//Subject(crse)%Name//' affects '//Block(i)%Name)
                        !    end if
                        !end do
                    end if
                end do

                ptrS = 0 ! non-zero later means a substitution rule was added
                call cgi_get_named_string(QUERY_STRING, 'Substitution', mesg, ierr)

                if (index(mesg,COMMA)>0 .and. ierr==0) then

                    call tokenize_subjects(mesg, COMMA, MAX_SECTION_MEETINGS, m, subjectList, ierr)
                    if (ierr==0) then

                        call check_array_bound (NumSubst+1, MAX_ALL_SUBSTITUTIONS, 'MAX_ALL_SUBSTITUTIONS', critical1)
                        call check_array_bound (ptrS+m, MAX_LEN_SUBSTITUTION_ARRAY, 'MAX_LEN_SUBSTITUTION_ARRAY', critical2)

                        if (critical1 .or. critical2) then
                            changed = .false.
                            remark = trim(remark)//': No more space for substitution rule'
                            call html_comment('No more space for substitution rule '//mesg)

                        else
                            ! delete rule if NONE is in subjectList
                            j = 0
                            do i=1,m
                                if (subjectList(i)==INDEX_TO_NONE) j = i
                            end do
                            if (j==0) then ! add rule
                                ptrS = SubstIdx(NumSubst+1)-1

                                NumSubst = NumSubst + 1
                                ptrS = ptrS+1
                                SubstIdx(NumSubst) = ptrS
                                Substitution(ptrS) = targetCurriculum
                                do i=1,m
                                    ptrS = ptrS+1
                                    Substitution(ptrS) = subjectList(i)
                                end do

                                SubstIdx(NumSubst+1) = ptrS+1

                                changed = .true.
                                remark = trim(remark)//': New substitution rule'
                                call html_comment('New substitution rule '//mesg)
                            else ! delete rule
                                do k=1,NumSubst
                                    if (Substitution(SubstIdx(k))/=targetCurriculum) cycle ! not this curriculum
                                    do j=SubstIdx(k)+1, SubstIdx(k+1)-1
                                        if (subjectList(1)==Substitution(j)) then ! matched
                                            Substitution(SubstIdx(k)) = 0 ! invalidate pointer to curriculum
                                            exit
                                        end if
                                    end do
                                end do
                                changed = .true.
                                remark = trim(remark)//': Removed substitution rule'
                                call html_comment('Removed substitution rule '//mesg)

                            end if
                        end if
                    end if
                end if

                if (changed) then
                    if ( wrk%Code /= Curriculum(targetCurriculum)%Code) then
                        ! add new ?
                        j = index_to_curriculum(wrk%Code)
                        if (j>0) then
                            remark = ' : Add new curriculum failed; "'//trim(wrk%Code)//'" already exists.'
                            call html_comment(remark)

                        else
                            call check_array_bound (NumCurricula+1, MAX_ALL_CURRICULA, 'MAX_ALL_CURRICULA', critical3)

                            if (critical3) then
                                changed = .false.
                                remark = ' : No more space for another curriculum'
                                call html_comment(remark)

                            else

                                ! add new curriculum
                                Curriculum(NumCurricula+1) = Curriculum(NumCurricula)
                                Curriculum(NumCurricula) = wrk
                                targetCurriculum = NumCurricula

                                NumCurricula = NumCurricula+1
                                targetCollege = wrk%CollegeIdx
                                tCurriculum = wrk%Code
                                remark = ' : Added new curriculum '//wrk%Code
                                call html_comment(remark)
                                ! redirect new substitution rule
                                if (ptrS>0) then
                                    Substitution(SubstIdx(NumSubst)) = targetCurriculum
                                end if
                                call make_curriculum_groups()
                            end if
                        end if
                    else
                        ! update existing
                        Curriculum(targetCurriculum) = wrk
                        targetCollege = wrk%CollegeIdx
                    end if
                end if

            case default

        end select

        end if ! (len_trim(tAction)>0 .and. isRoleOfficial) then

        if (changed) then
            call xml_write_curricula(trim(pathToYear)//'CURRICULA.XML')
        end if

        call html_write_header(device, 'Edit curriculum '//tCurriculum, remark(3:))
        write(device,AFORMAT) trim(make_href(fnCurriculumList, CurrProgCode(targetCurriculum), &
            A1=CurrProgCode(targetCurriculum), &
            pre=beginsmall//'Edit other'//nbsp, post=' option'//endsmall))//linebreak

        call make_form_start(device, fnEditCurriculum, tCurriculum)
        write(device,AFORMAT) '<table border="0" width="100%">', &
            begintr//begintd//beginbold//'Curriculum code'//endbold//endtd//begintd//'<input name="Code" size="'// &
            trim(itoa(MAX_LEN_CURRICULUM_CODE))// &
            '" value="'//trim(tCurriculum)//'"> (A new curriculum will be created if this is changed)'//endtd//endtr

        write(device,AFORMAT) &
            begintr//begintd//beginbold//'College'//endbold//endtd//begintd//'<select name="College">'
        do i=1,NumColleges
            if (i/=targetCollege) then
                j=0
            else
                j=1
            end if
            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(College(i)%Code)//'">'// &
                trim(College(i)%Name)
        end do

        if (Curriculum(targetCurriculum)%Active) then
            mesg = '<input type="radio" name="Status" value="Active" checked="yes"> Active '//nbsp// &
            '<input type="radio" name="Status" value="Inactive"> Inactive'
        else
            mesg = '<input type="radio" name="Status" value="Active"> Active '//nbsp// &
            '<input type="radio" name="Status" value="Inactive" checked="yes"> Inactive'
        end if

        write(device,AFORMAT) '</select>'//endtd//endtr, &
            begintr//begintd//beginbold//'Title'//endbold//endtd//begintd//'<input name="Title" size="'// &
                trim(itoa(MAX_LEN_CURRICULUM_NAME))//&
            '" value="'//trim(Curriculum(targetCurriculum)%Title)//'">'//endtd//endtr, &
            begintr//begintd//beginbold//'Specialization'//endbold//endtd//begintd//'<input name="Specialization" size="'// &
            trim(itoa(MAX_LEN_CURRICULUM_NAME))//'" value="'//trim(Curriculum(targetCurriculum)%Specialization)// &
            '">'//endtd//endtr, &
            begintr//begintd//beginbold//'Remark'//endbold//endtd// &
            begintd//'<input name="Remark" size="'//trim(itoa(MAX_LEN_CURRICULUM_NAME))// &
            '" value="'//trim(Curriculum(targetCurriculum)%Remark)//'">'//endtd//endtr, &
            begintr//begintd//beginbold//'Status'//endbold//endtd//begintd//trim(mesg)//endtd//endtr, &
            begintr//begintd//beginbold//'Year, Term (Units/Cumulative)'//endbold//endtd// &
            begintd//beginbold//'Comma-separated subject codes'//endbold//endtd//endtr

        tunits = 0.0

        do tdx=1,Curriculum(targetCurriculum)%NumTerms+6

            call rank_to_year_term(tdx, Year, Term)

            m = 0
            credit = 0
            mesg = SPACE

            do idx=1,Curriculum(targetCurriculum)%NSubjects
                if (Curriculum(targetCurriculum)%SubjectTerm(idx) == tdx) then
                    crse = Curriculum(targetCurriculum)%SubjectIdx(idx)
                    m = m+1
                    credit = credit + Subject(crse)%Units
                    mesg = trim(mesg)//COMMA//SPACE//Subject(crse)%Name
                end if
            end do

            tUnits = tUnits + credit

            write(device,AFORMAT) begintr//begintd// &
                trim(txtYear(Year+10))//' Year, '//trim(txtSemester(Term+6))//' Term ('// &
                trim(ftoa(credit,1))//FSLASH//trim(ftoa(tUnits,1))//')'//endtd, &
                begintd//'<input name="Subjects'//trim(itoa(tdx))//'" size="'//trim(itoa(MAX_LEN_CURRICULUM_NAME))// &
                '" value="'//trim(mesg(3:))//'">'//endtd//endtr
        end do

        write(device,AFORMAT) begintr//begintd//beginbold//'Substitution rules'//endbold//endtd, &
            begintd//'Required subjects in list will be PASSED if credits have been earned '// &
            'for the other subjects in the list'//endtd//endtr

        do tdx=1,NumSubst
            if (Substitution(SubstIdx(tdx))==targetCurriculum) then
                mesg = SPACE
                do j=SubstIdx(tdx)+1, SubstIdx(tdx+1)-1
                    mesg = trim(mesg)//COMMA//SPACE//Subject(Substitution(j))%Name
                end do
                write(device,AFORMAT) begintr//begintd//'SUBSTITUTION'//endtd//begintd//trim(mesg(3:))//endtd//endtr
            end if
        end do

        write(device,AFORMAT) begintr//begintd//'SUBSTITUTION'//endtd// &
            begintd//'<input name="Substitution" size="'//trim(itoa(MAX_LEN_CURRICULUM_NAME))// &
            '" value="(Enter new substitution rule)">'//endtd//endtr

        write(device,AFORMAT) endtable//linebreak//nbsp//'<input name="action" type="submit" value="Update">'//endform//'<pre>', &
            '</pre>'//horizontal

    end subroutine curriculum_edit


    subroutine equivalencies_edit(device)
        integer, intent (in) :: device
        integer :: idx, tdx, ierr, crse, ptrS, i, m!, j, k, Year, Term
        character (len=255) :: mesg, remark!, tokenizeErr
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        logical :: changed, critical1, critical2!, possibleImpact, critical3
        integer, dimension(MAX_SECTION_MEETINGS) :: subjectList

        changed = .false.
        remark = SPACE

        if (isRoleOfficial) then
            remark = '  Edit equivalence rule not enabled. '//sorryMessage

        else

            ! any deleted rules?
            do tdx=1,NumSubst
                if (Substitution(SubstIdx(tdx))==-1) then
                    call cgi_get_named_string(QUERY_STRING, 'del'//trim(itoa(tdx)), tSubject, ierr)
                    if (ierr/=0) cycle ! rule not found
                    ! delete rule
                    mesg = SPACE
                    do idx=SubstIdx(tdx)+1, SubstIdx(tdx+1)-1
                        mesg = trim(mesg)//COMMA//Subject(Substitution(idx))%Name
                    end do
                    Substitution(SubstIdx(tdx)) = 0
                    remark = trim(remark)//': Deleted rule '//mesg(2:)
                    changed = .true.
                end if
            end do

            ! a rule was added?
            call cgi_get_named_string(QUERY_STRING, 'Required', tSubject, ierr)
            if (len_trim(tSubject)>0) then ! new rule entered?
                crse = index_to_subject(tSubject)
                if (crse>0) then ! required subject is OK
                    call cgi_get_named_string(QUERY_STRING, 'Equivalent', mesg, ierr)

                    if (ierr==0) then  ! Equivalent list is not empty

                        ! prepend Required
                        mesg = trim(tSubject)//COMMA//mesg
                        call tokenize_subjects(mesg, COMMA, MAX_SECTION_MEETINGS, m, subjectList, ierr)
                        if (ierr==0) then ! Equivalent list is well-formed

                            ptrS = SubstIdx(NumSubst+1)-1

                            call check_array_bound (NumSubst+1, MAX_ALL_SUBSTITUTIONS, 'MAX_ALL_SUBSTITUTIONS', critical1)
                            call check_array_bound (ptrS+m, MAX_LEN_SUBSTITUTION_ARRAY, 'MAX_LEN_SUBSTITUTION_ARRAY', critical2)

                            if (critical1 .or. critical2) then
                                changed = .false.
                                remark = trim(remark)//': No more space for substitution rule'
                                call html_comment('No more space for substitution rule '//mesg, &
                                    'Limit MAX_ALL_SUBSTITUTIONS= '//itoa(MAX_ALL_SUBSTITUTIONS)// &
                                    '; currently used is '//itoa(NumSubst), &
                                    'Limit MAX_LEN_SUBSTITUTION_ARRAY= '//itoa(MAX_LEN_SUBSTITUTION_ARRAY)// &
                                    '; currently used is '//itoa(ptrS) )

                            else

                                NumSubst = NumSubst + 1
                                ptrS = ptrS+1
                                SubstIdx(NumSubst) = ptrS
                                Substitution(ptrS) = -1
                                do i=1,m
                                    ptrS = ptrS+1
                                    Substitution(ptrS) = subjectList(i)
                                end do

                                SubstIdx(NumSubst+1) = ptrS+1

                                changed = .true.
                                remark = trim(remark)//': New equivalence rule '//mesg
                                call html_comment('New equivalence rule '//mesg)
                            end if
                        end if
                    end if

                end if

            end if

            if (changed) then
                call xml_write_equivalencies(trim(pathToYear)//'EQUIVALENCIES.XML')
            else
                call cgi_get_named_string(QUERY_STRING, 'action', tSubject, ierr)
                if (ierr==0) remark = ' : No changes to equivalence rules?'
            end if

        end if


        call html_write_header(device, 'Equivalence rules applicable to all curricular programs', remark(3:))
        write(device,AFORMAT) 'Required subject will be PASSED if credits have been earned '// &
            'for Equivalent(s). Checked rules will be deleted.'//linebreak//linebreak

        call make_form_start(device, fnEditEquivalencies)
        write(device,AFORMAT) '<table border="0" width="50%">', begintr// &
            thalignleft//'Required'//endth// &
            thalignleft//'Equivalent(s)'//endth// &
            thalignleft//'Delete?'//endth//endtr
        do tdx=1,NumSubst
            if (Substitution(SubstIdx(tdx))==-1) then
                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(tdx,2))//'">'// &
                    begintd//trim(Subject(Substitution(SubstIdx(tdx)+1))%Name)//endtd
                mesg = SPACE
                do idx=SubstIdx(tdx)+2, SubstIdx(tdx+1)-1
                    mesg = trim(mesg)//COMMA//Subject(Substitution(idx))%Name
                end do
                write(device,AFORMAT) begintd//trim(mesg(2:))//endtd// &
                    begintd//'<input type="checkbox" name="del'//trim(itoa(tdx))//'">'//endtd//endtr
            end if
        end do

        write(device,AFORMAT) begintr// &
            begintd//'<input name="Required" size="'//trim(itoa(MAX_LEN_SUBJECT_CODE))// &
            '" value="">'//endtd// &
            begintd//'<input name="Equivalent" size="'//trim(itoa(MAX_LEN_SUBJECT_CODE))// &
            '" value="">'//endtd//begintd//beginbold//'Add rule'//endbold//endtd//endtr

        write(device,AFORMAT) endtable//linebreak//nbsp//'<input name="action" type="submit" value="Update">'//endform//'<pre>', &
            '</pre>'//horizontal

    end subroutine equivalencies_edit




    subroutine curriculum_list_all(device, fn)
        integer, intent (in) :: device, fn
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character(len=10) :: tStatus, tAction ! (ACTIVE)/(INACTIVE), Activate/Deactivate
        integer :: ierr, ldx, fnAction, ncurr
        character(len=255) :: mesg

        ! which program ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
        targetCurriculum = 0
        ! how many variants?
        ncurr = 0

        select case (fn)

            case (fnActivateCurriculum, fnDeactivateCurriculum)
                targetCurriculum = index_to_curriculum(tCurriculum)

            case default
                do ldx=1,NumCurricula-1
                    if (CurrProgCode(ldx) /= tCurriculum) cycle
                    targetCurriculum = ldx
                    exit
                end do
                do ldx=1,NumCurricula-1
                    if (CurrProgCode(ldx) /= tCurriculum) cycle
                    ncurr = ncurr + 1
                end do

        end select
        targetCollege = Curriculum(targetCurriculum)%CollegeIdx

        ! if ony one curriculum, display the curriculum
        if (ncurr==1) then
            call curriculum_display(device, targetCurriculum)
            return
        end if

        if (isRoleOfficial) then

            mesg = 'Activate/deactivate curriculum not enabled. '//sorryMessage

        else

            ! activate/deactivate
            select case (fn)

                case (fnActivateCurriculum)
                    Curriculum(targetCurriculum)%Active = .true.
                    mesg = 'Activated '//tCurriculum
                    tCurriculum = CurrProgCode(targetCurriculum)

                case (fnDeactivateCurriculum)
                    Curriculum(targetCurriculum)%Active = .false.
                    mesg = 'Deactivated '//tCurriculum
                    tCurriculum = CurrProgCode(targetCurriculum)

                case default
                    mesg = SPACE

            end select

            if (mesg/=SPACE) call xml_write_curricula(trim(pathToYear)//'CURRICULA.XML')

        end if

        call html_write_header(device, tCurriculum//' options', mesg)

        ! collect curricula
        write(device,AFORMAT) '<ol>'
        do ldx=1,NumCurricula-1

            if (CurrProgCode(ldx) /= tCurriculum) cycle

            write(device,AFORMAT) trim(make_href(fnCurriculum, Curriculum(ldx)%Code, &
                A1=Curriculum(ldx)%Code, &
                pre=beginitem, post=' - '//trim(Curriculum(ldx)%Title)))
            if (trim(Curriculum(ldx)%Specialization)/=SPACE) &
                write(device,AFORMAT) ' : '//trim(Curriculum(ldx)%Specialization)
            if (trim(Curriculum(ldx)%Remark)/=SPACE) &
                write(device,AFORMAT) ' : '//trim(Curriculum(ldx)%Remark)

            if (Curriculum(ldx)%Active) then
                tStatus = '(Active)'
                tAction = 'Deactivate'
                fnAction = fnDeactivateCurriculum
            else
                tStatus = '(Inactive)'
                tAction = 'Activate'
                fnAction = fnActivateCurriculum
            end if

            write(device,AFORMAT) nbsp//beginitalic//tStatus//enditalic//nbsp

            if (is_admin_of_college(targetCollege)) then
                write(device,AFORMAT) trim(make_href(fnAction, tAction, A1=Curriculum(ldx)%Code, &
                    pre=nbsp//beginsmall, post=nbsp))
                write(device,AFORMAT) trim(make_href(fnEditCurriculum, 'Edit', A1=Curriculum(ldx)%Code, &
                    pre=nbsp, post=endsmall))
            end if

            write(device,AFORMAT) enditem

        end do
        write(device,AFORMAT) '</ol>'//horizontal

    end subroutine curriculum_list_all


    subroutine curriculum_display(device, given)
        integer, intent(in), optional :: given
        integer, intent (in) :: device
        integer :: idx, tdx, m, n, Year, Term, fnAction
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum, tAction
        character(len=10) :: tStatus ! (ACTIVE)/(INACTIVE)
        real :: tUnits, cumulative

        ! which curriculum
        if (present(given)) then
            targetCurriculum = given
            tdx = 0
        else
            call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, tdx)
            targetCurriculum = index_to_curriculum(tCurriculum)
        end if
        targetCollege = Curriculum(targetCurriculum)%CollegeIdx

        call html_write_header(device, SPACE) ! Curriculum(targetCurriculum)%Code)

        write(device,AFORMAT) beginbold//trim(Curriculum(targetCurriculum)%Code)//' - '// &
        trim(Curriculum(targetCurriculum)%Title)//endbold
        if (len_trim(Curriculum(targetCurriculum)%Specialization) > 0) then
            write(device,AFORMAT) beginbold//' : '//trim(Curriculum(targetCurriculum)%Specialization)//endbold
        end if
        if (len_trim(Curriculum(targetCurriculum)%Remark) > 0) then
            write(device,AFORMAT) beginbold//' : '//trim(Curriculum(targetCurriculum)%Remark)//endbold
        end if
        if (Curriculum(targetCurriculum)%Active) then
            tStatus = '(Active)'
            tAction = 'Deactivate'
            fnAction = fnDeactivateCurriculum
        else
            tStatus = '(Inactive)'
            tAction = 'Activate'
            fnAction = fnActivateCurriculum
        end if
        write(device,AFORMAT) nbsp//beginitalic//tStatus//enditalic//nbsp
        if (is_admin_of_college(targetCollege)) then
            write(device,AFORMAT) trim(make_href(fnAction, tAction, A1=Curriculum(targetCurriculum)%Code, &
                pre=nbsp//beginsmall, post=nbsp))
            write(device,AFORMAT) trim(make_href(fnEditCurriculum, 'Edit', A1=Curriculum(targetCurriculum)%Code, &
                pre=nbsp, post=endsmall))
        end if

        write(device,AFORMAT) linebreak//'Note: A '//red//'SUBJECT'//black//' in column '//beginbold// &
            beginitalic//'Prerequisite'//enditalic//endbold, &
            ' indicates an inconsistency. Said '//red//'SUBJECT'//black// &
            ' is not present in the curriculum, or is not taken in a prior term, ', &
            ' or the prerequisite expression should be "SUBJECT1 OR SUBJECT2" where either one is taken in a prior term.', &
            linebreak//'<table border="1" width="100%">'
        cumulative = 0.0
        do tdx=1,Curriculum(targetCurriculum)%NumTerms

            call rank_to_year_term(tdx, Year, Term)
            m = 0
            tUnits = 0.0

            do idx=1,Curriculum(targetCurriculum)%NSubjects
                if (Curriculum(targetCurriculum)%SubjectTerm(idx) == tdx) then
                    m = m+1
                    tUnits = tUnits + Subject(Curriculum(targetCurriculum)%SubjectIdx(idx))%Units
                end if
            end do

            cumulative = cumulative + tUnits

            if (m > 0) then
                write(device,AFORMAT) begintr//'<td colspan="6">'//nbsp//endtd//endtr, &
                    begintr//'<td colspan="6"> '//beginbold//trim(Curriculum(targetCurriculum)%Code)//': '// &
                    trim(txtYear(Year))//' Year, '// &
                    trim(txtSemester(Term+3))//' Term ('//trim(ftoa(tUnits,1))//' units; '// &
                    trim(ftoa(cumulative,1))//' cumulative)' &
                    //endbold//endtd//endtr

                write(device,AFORMAT) begintr, &
                    tdnbspendtd//tdnbspendtd//tdnbspendtd//&
                    begintd//beginbold//beginitalic//'Lect'//enditalic//endbold//endtd, &
                    begintd//beginbold//beginitalic//'Lab'//enditalic//endbold//endtd, &
                    tdnbspendtd, endtr
                write(device,AFORMAT) begintr, &
                    begintd//beginbold//beginitalic//'Subject'//enditalic//endbold//endtd, &
                    begintd//beginbold//beginitalic//'Title'//enditalic//endbold//endtd, &
                    begintd//beginbold//beginitalic//'Units'//enditalic//endbold//endtd, &
                    begintd//beginbold//beginitalic//'Hrs'//enditalic//endbold//endtd, &
                    begintd//beginbold//beginitalic//'Hrs'//enditalic//endbold//endtd, &
                    begintd//beginbold//beginitalic//'Prerequisite'//enditalic//endbold//endtd, &
                    endtr
                do idx=1,Curriculum(targetCurriculum)%NSubjects
                    if (Curriculum(targetCurriculum)%SubjectTerm(idx) /= tdx) cycle
                    n = Curriculum(targetCurriculum)%SubjectIdx(idx)
                    if (isRoleSysAd .or. isRoleOfficial) then
                        write(device,AFORMAT) begintr//begintd, &
                            trim(make_href(fnEditSubject, Subject(n)%Name, A1=Subject(n)%Name, A2=College(targetCollege)%Code))
                    else
                        write(device,AFORMAT) begintr//begintd//trim(Subject(n)%Name)
                    end if
                    write(device,AFORMAT) endtd//begintd//trim(Subject(n)%Title)//endtd// &
                        tdaligncenter//trim(ftoa(Subject(n)%Units,1))//endtd//&
                        tdaligncenter//trim(ftoa(Subject(n)%LectHours,2))//endtd//&
                        tdaligncenter//trim(ftoa(Subject(n)%LabHours,2))//endtd//&
                        '<td width="20%">'//trim(text_prerequisite_in_curriculum(n,Curriculum(targetCurriculum)))//endtd//&
                        endtr
                end do
            end if
        end do

        write(device,AFORMAT) endtable//horizontal

    end subroutine curriculum_display


end module EditUNIVERSITY
