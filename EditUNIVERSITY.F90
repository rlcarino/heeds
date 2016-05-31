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
            b_tr//b_thar//'Title or Position'//e_th//b_td_nbsp_e_td//b_thal//'Name'//e_th//e_tr, &
            b_tr//b_tdar//b_italic//'(University name)'//e_italic//e_td//b_td_nbsp_e_td// &
                     b_td//'<input name="UniversityName" size="60" value="'//trim(UniversityName)//'">'//e_td// &
            e_tr, &
            b_tr//b_tdar//b_italic//'(University address)'//e_italic//e_td//b_td_nbsp_e_td// &
                     b_td//'<input name="UniversityAddress" size="60" value="'//trim(UniversityAddress)//'">'//e_td// &
            e_tr, &
            b_tr//b_tdar//b_italic//'(University phone)'//e_italic//e_td//b_td_nbsp_e_td// &
                     b_td//'<input name="UniversityPhone" size="60" value="'//trim(UniversityPhone)//'">'//e_td// &
            e_tr, &
            b_tr//b_tdar//b_italic//'(University web address)'//e_italic//e_td//b_td_nbsp_e_td// &
                     b_td//'<input name="UniversityWeb" size="60" value="'//trim(UniversityWeb)//'">'//e_td// &
            e_tr, &
            b_tr// &
                b_tdar//'<input name="titleUniversityPresident" size="40" value="'//trim(titleUniversityPresident)//'">'// &
                e_td//b_td_nbsp_e_td// &
                b_td//'<input name="UniversityPresident" size="60" value="'//trim(UniversityPresident)//'">'//e_td// &
            e_tr, &
            b_tr// &
                b_tdar//'<input name="titleVPAcademicAffairs" size="40" value="'//trim(titleVPAcademicAffairs)//'">'// &
                e_td//b_td_nbsp_e_td// &
                b_td//'<input name="VPAcademicAffairs" size="60" value="'//trim(VPAcademicAffairs)//'">'//e_td// &
            e_tr, &
            b_tr// &
                b_tdar//'<input name="titleDeanOfCampus" size="40" value="'//trim(titleDeanOfCampus)//'">'// &
                e_td//b_td_nbsp_e_td// &
                b_td//'<input name="DeanOfCampus" size="60" value="'//trim(DeanOfCampus)//'">'//e_td// &
            e_tr, &
            b_tr// &
                b_tdar//'<input name="titleDeanOfInstruction" size="40" value="'//trim(titleDeanOfInstruction)//'">'// &
                e_td//b_td_nbsp_e_td// &
                b_td//'<input name="DeanOfInstruction" size="60" value="'//trim(DeanOfInstruction)//'">'//e_td// &
            e_tr, &
            b_tr// &
                b_tdar//'<input name="titleTheRegistrar" size="40" value="'//trim(titleTheRegistrar)//'">'// &
                e_td//b_td_nbsp_e_td// &
                b_td//'<input name="TheRegistrar" size="60" value="'//trim(TheRegistrar)//'">'//e_td// &
            e_tr

        write(device,AFORMAT) e_table, linebreak, &
            nbsp//'<input name="action" type="submit" value="Update">'//e_form//linebreak


        write(device,AFORMAT) '<h3>Update college signatories</h3>'

        call make_form_start(device, fnEditSignatories)

        write(device,AFORMAT) '<table border="0" width="100%">', &
            b_tr//b_thar//'Title or Position'//e_th//b_td_nbsp_e_td//b_thal//'Name'//e_th//e_tr

        do iColl=1,NumColleges-1
            write(device,AFORMAT) &
                b_tr, &
                b_tdar//b_italic//'(College Dean, '//trim(College(iColl)%Code)//')'//e_italic//e_td, &
                b_td_nbsp_e_td, &
                b_td//'<input name="DEAN:'//trim(College(iColl)%Code)//'" size="60" value="'// &
                         trim(College(iColl)%Dean)//'">'//e_td, &
                e_tr, &
                b_tr, &
                b_tdar//b_italic//'(Transcript Preparer, '//trim(College(iColl)%Code)//')'//e_italic//e_td, &
                b_td_nbsp_e_td, &
                b_td//'<input name="PREPARER:'//trim(College(iColl)%Code)//'" size="60" value="'// &
                         trim(College(iColl)%TranscriptPreparer)//'">'//e_td, &
                e_tr, &
                b_tr, &
                b_tdar//b_italic//'(Transcript Checker, '//trim(College(iColl)%Code)//')'//e_italic//e_td, &
                b_td_nbsp_e_td, &
                b_td//'<input name="CHECKER:'//trim(College(iColl)%Code)//'" size="60" value="'// &
                         trim(College(iColl)%TranscriptChecker)//'">'//e_td, &
                e_tr
        end do

        write(device,AFORMAT) e_table, linebreak, &
            nbsp//'<input name="action" type="submit" value="Update">'//e_form//horizontal


    end subroutine display_signatories


    subroutine edit_signatories(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_USERNAME) :: tAction
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
                sorryMessageOfficial = trim(UniversityCode)//' officials and their friends '// &
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

            if (changes) call university_data_write(unitXML, trim(pathToYear)//'UNIVERSITY.XML')

            ! collect changes to colleges
            do iColl=1,NumColleges-1
                changes = .false.
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

                if (changes) then
                    call college_details_write(unitXML, dirCOLLEGES, iColl)
                end if

            end do

        end if

        if (len_trim(tAction)>0 .and. isRoleOfficial) then
            tInput = 'Edit signatories failed. '//sorryMessageOfficial
        else
            tInput = SPACE
        end if

        call display_signatories(device, tInput)


    end subroutine edit_signatories


    subroutine edit_timers(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_USERNAME) :: tAction
        character (len=255) :: mesg
        integer :: ierr, tTime

        call html_comment('edit_timers()')

        targetDepartment = DeptIdxUser
        targetCollege = CollegeIdxUser

        ! check for requested action
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
        mesg = SPACE

        if (ierr/=0 .or. tAction==SPACE) then ! no action; display existing info
        else ! action is Update; collect changes, if any

            call cgi_get_named_integer(QUERY_STRING, 'maxIdleTime', tTime, ierr)
            if (ierr/=0) tTime = maxIdleTime
            if (tTime /= maxIdleTime) then
                maxIdleTime = tTime
                mesg = ' : Max idle time='//trim(itoa(tTime))//mesg
            end if

            call cgi_get_named_integer(QUERY_STRING, 'maxBackupTime', tTime, ierr)
            if (ierr/=0) tTime = maxBackupTime
            if (tTime /= maxBackupTime) then
                maxBackupTime = tTime
                mesg = ' : Max backup time='//trim(itoa(tTime))//mesg
            end if

            call cgi_get_named_integer(QUERY_STRING, 'maxRefreshTime', tTime, ierr)
            if (ierr/=0) tTime = maxRefreshTime
            if (tTime /= maxRefreshTime) then
                maxRefreshTime = tTime
                mesg = ' : Max refresh time='//trim(itoa(tTime))//mesg
            end if

            call cgi_get_named_integer(QUERY_STRING, 'maxSFNA', tTime, ierr)
            if (ierr/=0) tTime = maxStudentsForNeedsAnalysis
            if (tTime /= maxStudentsForNeedsAnalysis) then
                maxStudentsForNeedsAnalysis = tTime
                mesg = ' : Max students for Needs Analysis='//trim(itoa(tTime))//mesg
            end if

        end if
        if (len_trim(mesg)>0) mesg = 'Changes'//mesg

        ! display existing timers
        call html_write_header(device, 'Edit timers', mesg)
        call make_form_start(device, fnTimers)

        write(device,AFORMAT) '<table border="0" width="50%">', &
            b_tr//b_thar//'Description'//e_th//b_td_nbsp_e_td//b_thal//'Value'//e_th//e_tr, &
            b_tr//b_tdar//b_italic//'No. of seconds for auto-logout of an idle user'//e_italic// &
                     e_td//b_td_nbsp_e_td// &
                     b_td//'<input name="maxIdleTime" value="'//trim(itoa(maxIdleTime))//'">'//e_td// &
            e_tr, &
            b_tr//b_tdar//b_italic//'No. of seconds for auto-backup of data'//e_italic// &
                     e_td//b_td_nbsp_e_td// &
                     b_td//'<input name="maxBackupTime" value="'//trim(itoa(maxBackupTime))//'">'//e_td// &
            e_tr, &
            b_tr//b_tdar//b_italic//'No. of seconds for auto-refresh of data in mirror'//e_italic// &
                     e_td//b_td_nbsp_e_td// &
                     b_td//'<input name="maxRefreshTime" value="'//trim(itoa(maxRefreshTime))//'">'//e_td// &
            e_tr, &
            b_tr//b_tdar//b_italic//'Max no. of students for Needs Analysis to avoid timeout'//e_italic// &
                     e_td//b_td_nbsp_e_td// &
                     b_td//'<input name="maxSFNA" value="'//trim(itoa(maxStudentsForNeedsAnalysis))//'">'//e_td// &
            e_tr

        write(device,AFORMAT) e_table, linebreak, &
            nbsp//'<input name="action" type="submit" value="Update">'//e_form//linebreak, horizontal

    end subroutine edit_timers



    subroutine message_of_the_day(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_USERNAME) :: tAction
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
                b_italic, &
                'Note: Type one continuous line of text. Intersperse &lt;br&gt; for line breaks', &
                e_italic, &
                linebreak, &
                '<textarea cols="80" rows="5" name="MOTD">'//trim(MOTD)//'</textarea>', &
                linebreak, &
                linebreak, &
                nbsp//'<input name="action" type="submit" value="Update">', &
                e_form//horizontal

        else ! action is Update

            call cgi_get_named_string(QUERY_STRING, 'MOTD', cipher, ierr)
            if (ierr==0) then
                MOTD = cipher
                if (isRoleOfficial) then
                    call html_college_links(device, CollegeIdxUser, mesg='"Message of the day" not changed. '//sorryMessageOfficial)
                else
                    call html_college_links(device, CollegeIdxUser, mesg='Message of the day is: '//trim(MOTD))
                    if (.not. isMirror) then
                        open (unit=unitETC, file=trim(pathToYear)//'MOTD.TXT', status='unknown')
                        write(unitETC, AFORMAT) trim(MOTD)
                        close(unitETC)
                    end if
                end if
            end if

        end if

    end subroutine message_of_the_day


    subroutine emergency_message (device)
        integer, intent(in) :: device
        character(len=MAX_LEN_USERNAME) :: tAction
        integer :: ierr

        call html_comment('emergency_message ()')

        targetDepartment = DeptIdxUser
        targetCollege = CollegeIdxUser

        ! check for requested action
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)

        if (ierr/=0 .or. tAction==SPACE) then ! no action; display existing info

            call html_write_header(device, 'Edit the emergency message', SPACE)
            call make_form_start(device, fnEditEMERGENCY)
            write(device,AFORMAT) &
                b_italic, &
                'Note: Type one continuous line of text. Intersperse &lt;br&gt; for line breaks', &
                e_italic, &
                linebreak, &
                '<textarea cols="80" rows="5" name="EMERGENCY">'//trim(EMERGENCY)//'</textarea>', &
                linebreak, &
                linebreak, &
                nbsp//'<input name="action" type="submit" value="Update">', &
                e_form//horizontal

        else ! action is Update

            call cgi_get_named_string(QUERY_STRING, 'EMERGENCY', cipher, ierr)
            if (ierr==0) then
                EMERGENCY = cipher
                if (isRoleOfficial) then
                    call html_college_links(device, CollegeIdxUser, mesg='"Emergency message" not changed. '//sorryMessageOfficial)
                else
                    call html_college_links(device, CollegeIdxUser, mesg='Emergency message: '//trim(EMERGENCY))
                end if
            end if

        end if

    end subroutine emergency_message


    subroutine room_edit(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_ROOM_CODE) :: tRoom, tAction
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer :: ierr, iRoom, j
        character (len=255) :: mesg, remark
        type (TYPE_ROOM) :: wrk
        logical :: isDirtyROOMS, criticalErr

        ! which room ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tRoom, iRoom)
        if (iRoom/=0) tRoom = 'TBA'

        call html_comment('room_edit('//trim(tRoom)//') ')
        iRoom = index_to_room(tRoom)
        targetDepartment = Room(iRoom)%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx

        wrk = Room(iRoom) ! make a working copy
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
            remark = 'Update room "'//trim(tRoom)//'" failed. '//sorryMessageOfficial
            call room_info(device, wrk, mesg, remark, tAction)

        else ! action is Add or Update; collect changes

            call cgi_get_named_integer(QUERY_STRING, 'MaxCapacity', wrk%MaxCapacity, ierr)
            !write(*,*) 'ierr=', ierr, ', MaxCapacity=', wrk%MaxCapacity
            if (ierr/=0) wrk%MaxCapacity = Room(iRoom)%MaxCapacity

            call cgi_get_named_integer(QUERY_STRING, 'Cluster', wrk%Cluster, ierr)
            !write(*,*) 'ierr=', ierr, ', Cluster=', wrk%Cluster
            if (ierr/=0) wrk%Cluster = Room(iRoom)%Cluster

            call cgi_get_named_integer(QUERY_STRING, 'EnergyFee', wrk%EnergyFee, ierr)
            !write(*,*) 'ierr=', ierr, ', EnergyFeer=', wrk%EnergyFee
            if (ierr/=0) wrk%EnergyFee = Room(iRoom)%EnergyFee

            call cgi_get_named_string(QUERY_STRING, 'Code', mesg, ierr)
            wrk%Code = trim(mesg)
            !write(*,*) 'ierr=', ierr, ', Code=', wrk%Code
            if (ierr/=0) wrk%Code = Room(iRoom)%Code

            call cgi_get_named_string(QUERY_STRING, 'Department', tDepartment, ierr)
            wrk%DeptIdx = index_to_dept(tDepartment)
            !write(*,*) 'ierr=', ierr, ', DeptIdx=', wrk%DeptIdx
            if (ierr/=0 .or. wrk%DeptIdx<=0) wrk%DeptIdx = Room(iRoom)%DeptIdx

            if (wrk%Code /= Room(iRoom)%Code) then
                isDirtyROOMS = .true.
                remark = trim(remark)//': Code changed to '//wrk%Code
            end if

            if (wrk%DeptIdx /= Room(iRoom)%DeptIdx) then
                isDirtyROOMS = .true.
                remark = trim(remark)//': Department changed to '//Department(wrk%DeptIdx)%Code
            end if

            if ( wrk%MaxCapacity /= Room(iRoom)%MaxCapacity) then
                isDirtyROOMS = .true.
                remark = trim(remark)//': Max seating capacity changed to '//itoa(wrk%MaxCapacity)
            end if

            if ( wrk%Cluster /= Room(iRoom)%Cluster) then
                isDirtyROOMS = .true.
                remark = trim(remark)//': Cluster changed to '//itoa(wrk%Cluster)
            end if

            if ( wrk%EnergyFee /= Room(iRoom)%EnergyFee) then
                isDirtyROOMS = .true.
                remark = trim(remark)//': Energy fee changed to '//itoa(wrk%EnergyFee)
            end if

            if (isDirtyROOMS) then ! some changes

                if (wrk%Code /= Room(iRoom)%Code) then  ! new code; check if room already exists

                    j = index_to_room(wrk%Code)
                    if (j==0) then ! not used

                        if (trim(tAction)=='Add') then

                            call check_array_bound (NumRooms+NumAdditionalRooms+1, MAX_ALL_ROOMS, 'MAX_ALL_ROOMS', criticalErr)
                            if (criticalErr) then
                                remark = ': No more space for additional room'
                            else
                                NumAdditionalRooms = NumAdditionalRooms+1
                                Room(NumRooms+NumAdditionalRooms) = wrk
                                iRoom = NumRooms+NumAdditionalRooms
                                tRoom = wrk%Code
                                remark = ': Added new room '//wrk%Code
                                call room_details_write(unitXML, trim(dirROOMS)//'index', 1, NumRooms+NumAdditionalRooms)
                            end if
                        else
                            ! update existing
                            Room(iRoom) = wrk
                        end if

                    else

                        remark = ': Add/edit room failed; "'//trim(wrk%Code)//'" already exists.'
                        isDirtyROOMS = .false.

                    end if
                else
                    ! same code; update other fields
                    Room(iRoom) = wrk
                end if

                if (isDirtyROOMS) call room_details_write(unitXML, dirROOMS, iRoom)

            else ! Add or Update clicked, but no changes made
                remark = ': No changes made?'

            end if

            call html_college_links(device, Department(wrk%DeptIdx)%CollegeIdx, &
                trim(tRoom)//remark)

        end if

    end subroutine room_edit


    subroutine room_list_all (device, fn)
        integer, intent (in) :: device, fn
        integer :: iRoom, n_count, ierr
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        ! collect rooms
        tArray = 0
        n_count = 0
        ! which dept ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)
        call html_comment('room_list_all('//trim(tDepartment)//')')
        targetDepartment = index_to_dept(tDepartment)
        targetCollege = Department(targetDepartment)%CollegeIdx
        do iRoom=1,NumRooms+NumAdditionalRooms
            if (targetDepartment==Room(iRoom)%DeptIdx) then
                n_count = n_count+1
                tArray(n_count) = iRoom
            end if
        end do
        call html_room_list (device, fn, n_count, tArray(0:n_count), 'Rooms in '//tDepartment, SPACE)

    end subroutine room_list_all


    subroutine html_room_list (device, fn, n_count, tArray, header, searchString)
        integer, intent (in) :: device, fn, n_count
        integer, intent (in out) :: tArray(0:n_count)
        character (len=*), intent (in) :: header
        character (len=*), intent (in) :: searchString ! for fn==fnSearchCategory

        integer :: iRoom, nsect(3), jdx, idx, iTerm, ierr, tLen, iSect
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        integer, dimension(60,7) :: TimeTable
        character (len=127) :: mesg, tString
        character(len=80) :: location

        tString = searchString
        tLen = max(len_trim(tString),1)
        call html_write_header(device, header)

        if (n_count == 0) then
            write(device,AFORMAT) '<table border="0">', &
                b_tr//b_td//JUSTNONE//e_td//b_tdar
            if ( isRole_admin_of_college(targetCollege) .and. fn/=fnSearchCategory) then
                write(device,AFORMAT) trim(make_href(fnEditRoom, 'Add room', &
                    A1='TBA', pre=b_small//'('//nbsp, post=' )'//e_small ))
            end if
            write(device,AFORMAT) e_td//e_tr//e_table
        else
            ! sort rooms
            do idx=1,n_count-1
                do jdx=idx+1,n_count
                    if (Room(tArray(jdx))%Code<Room(tArray(idx))%Code) then
                        iRoom =tArray(jdx)
                        tArray(jdx) = tArray(idx)
                        tArray(idx) = iRoom
                    end if
                end do
            end do

            if ( isRole_admin_of_college(targetCollege)  .and. fn/=fnSearchCategory )then
                write(device,AFORMAT) trim(make_href(fnEditRoom, 'Add room', &
                    A1='TBA', pre=b_small//'('//nbsp, post=' )'//e_small ))//linebreak
            end if

            write(device,AFORMAT) b_italic//'Note: Number under Term links to classes in the room.'//e_italic//linebreak, &
                '<table border="0" width="90%">'//&
                b_tr//b_thal//'Code'//e_th// &
                b_thac//'Cluster'//e_th, &
                b_thac//'Capacity'//e_th, &
                b_thac//'Energy Fee'//e_th
            do iTerm=firstSemester,summerTerm
                write(device,AFORMAT) &
                    b_thac//txtSemester(iTerm+6)//termQualifier(iTerm+6)//linebreak// &
                    text_school_year(currentYear)//e_th
            end do
            write(device,AFORMAT) b_thac//'Remark'//e_th
            if (fn/=fnSearchCategory) then
                write(device,AFORMAT) e_tr
            else
                write(device,AFORMAT) b_thal//'"'//tString(:tLen)//'" found in ...'//e_th//e_tr
            end if

            do idx=1,n_count
                iRoom = tArray(idx)

                ! check conflicts for each term
                mesg = SPACE
                nsect = 0
                do iTerm=firstSemester,summerTerm
                    call timetable_clear(TimeTable)
                    ! collect classes in room
                    do iSect=1,NumSections(iTerm)
                        call meetings_of_section_in_room(iTerm, iSect, iRoom, n_meetings, meetings)
                        if (n_meetings==0) cycle ! room not assigned to this section
                        nsect(iTerm) = nsect(iTerm)+1
                        ierr = -10
                        call timetable_add_meetings_of_section(iTerm, iSect, n_meetings, meetings, TimeTable, ierr)
                        if (ierr /= -10) then
                            mesg = trim(mesg)//SPACE//txtSemester(iTerm+6)
                        end if
                    end do
                end do
                if (len_trim(mesg)>0) mesg = red//'Conflict: '//trim(mesg)//e_color

                QUERY_put = Room(iRoom)%Code
                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(idx,2))//'">'//b_td//trim(Room(iRoom)%Code)
                if ( isRole_dean_of_college(targetCollege, orHigherUp)  .or. isRole_admin_of_college(targetCollege) ) then
                    write(device,AFORMAT) trim(make_href(fnEditRoom, 'Edit', &
                        A1=QUERY_put, pre=nbsp//b_small, post=e_small))
                end if
                write(device,AFORMAT) &
                    e_td//b_tdac//trim(itoa(Room(iRoom)%Cluster))//e_td// &
                    b_tdac//trim(itoa(Room(iRoom)%MaxCapacity))//e_td// &
                    b_tdac//trim(itoa(Room(iRoom)%EnergyFee))//e_td

                do iTerm=firstSemester,summerTerm
                    write(device,AFORMAT) trim(make_href(fnRoomSchedule, itoa(nsect(iTerm)), &
                        A1=QUERY_put, A9=iTerm, pre=b_tdac, post=e_td))
                end do

                write(device,AFORMAT) b_tdac//trim(mesg)//e_td
                if (fn/=fnSearchCategory) then
                    write(device,AFORMAT) e_tr
                else
                    call room_search_info(iRoom, tString(:tLen), location)
                    ierr = max(3, len_trim(location) )
                    write(device,AFORMAT) b_td//b_small//location(:ierr-2)//e_small//e_td//e_tr
                end if

            end do
            write(device,AFORMAT) e_table
        end if
        write(device,AFORMAT) horizontal

    end subroutine html_room_list


    subroutine room_search_info(rIdx, searchString, location)
        integer, intent(in) :: rIdx
        character(len=*), intent(in) :: searchString
        character(len=80), intent(out) :: location
        integer :: iSect, iTerm, n_meetings, meetings(MAX_SECTION_MEETINGS)

        location = SPACE
        do iTerm=summerTerm,firstSemester,-1
            do iSect=1,NumSections(iTerm)
                if (index(Section(iTerm,iSect)%ClassId,searchString)==0) cycle ! search string not in section ID
                call meetings_of_section_in_room(iTerm, iSect, rIdx, n_meetings, meetings)
                if (n_meetings==0) cycle ! room not assigned to this section
                location = trim(txtSemester(iTerm+6))//trim(termQualifier(iTerm+6))//' : '//location
                exit
            end do
        end do
        if (index(Room(rIdx)%Code,searchString)>0) location = 'Code : '//location

    end subroutine room_search_info



    subroutine room_conflicts (device, thisTerm)
        integer, intent (in) :: device, thisTerm
        integer :: iRoom, n_count, nsect, iSect, idx, ierr, jdx
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        integer, dimension(60,7) :: TimeTable
        character (len=127) :: mesg

        ! collect rooms
        tArray = 0
        n_count = 0
        ! which college
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)

        call html_comment('room_conflicts('//trim(tCollege)//')')

        targetCollege = index_to_college(tCollege)
        do iRoom=1,NumRooms+NumAdditionalRooms
            if (Department(Room(iRoom)%DeptIdx)%CollegeIdx/=targetCollege) cycle ! not in college
            call timetable_clear(TimeTable)
            ! collect classes in room
            nsect = 0
            do iSect=1,NumSections(thisTerm)
                call meetings_of_section_in_room(thisTerm, iSect, iRoom, n_meetings, meetings)
                if (n_meetings>0) then ! room assigned to this section
                    ierr = -10
                    call timetable_add_meetings_of_section(thisTerm, iSect, n_meetings, meetings, TimeTable, ierr)
                    if (ierr /= -10) then
                        nsect = nsect+1
                        exit
                    end if
                end if
            end do
            if (nsect>0) then ! conflict
                n_count = n_count + 1
                tArray(n_count) = iRoom
            end if
        end do
        mesg = 'Rooms with schedule conflicts in '//tCollege

        call html_write_header(device, mesg)

        if (n_count == 0) then
            write(device,AFORMAT) BRNONE
        else
            ! sort rooms
            do idx=1,n_count-1
                do jdx=idx+1,n_count
                    if (Room(tArray(jdx))%Code<Room(tArray(idx))%Code) then
                        iRoom =tArray(jdx)
                        tArray(jdx) = tArray(idx)
                        tArray(idx) = iRoom
                    end if
                end do
            end do

            write(device,AFORMAT) '<table border="0" width="50%">'//&
                b_tr//b_thal//'Code'//e_th// &
                b_thac//'Cluster'//e_th, &
                b_thac//'Capacity'//e_th// &
                b_thac//'Classes'//e_th// &
                b_thac//'Remark'//e_th//e_tr

            do idx=1,n_count
                iRoom = tArray(idx)
                ! check conflicts
                mesg = SPACE
                call timetable_clear(TimeTable)
                ! collect classes in room
                nsect = 0
                do iSect=1,NumSections(thisTerm)
                    call meetings_of_section_in_room(thisTerm, iSect, iRoom, n_meetings, meetings)
                    if (n_meetings>0) then ! room assigned to this section
                        nsect = nsect+1
                        tArray(n_count+nsect) = iSect
                        ierr = -10
                        call timetable_add_meetings_of_section(thisTerm, iSect, n_meetings, meetings, TimeTable, ierr)
                        if (ierr /= -10) then
                            mesg = red//'Conflict!'//e_color
                        end if
                    end if
                end do
                QUERY_put = Room(iRoom)%Code
                write(device,AFORMAT) b_tr//b_td//trim(Room(iRoom)%Code)
                if ( (isRole_dean_of_college(targetCollege, orHigherUp) .or. isRole_admin_of_college(targetCollege) ) ) then
                    write(device,AFORMAT) trim(make_href(fnEditRoom, 'Edit', &
                        A1=QUERY_put, pre=nbsp//b_small, post=e_small))
                end if
                write(device,AFORMAT) &
                    e_td//b_tdac//trim(itoa(Room(iRoom)%Cluster))//e_td// &
                    b_tdac//trim(itoa(Room(iRoom)%MaxCapacity))//e_td
                !if (nsect>0) then
                write(device,AFORMAT) trim(make_href(fnRoomSchedule, itoa(nsect), &
                    A1=QUERY_put, A9=thisTerm, pre=b_tdac, post=e_td))
                !else
                !  write(device,AFORMAT) b_tdac//trim(itoa(nsect))//e_td
                !end if
                write(device,AFORMAT) b_tdac//trim(mesg)//e_td//e_tr
            end do
            write(device,AFORMAT) e_table
        end if
        write(device,AFORMAT) horizontal

    end subroutine room_conflicts


    subroutine room_schedule(device, thisTerm, LoadSource)
        integer, intent(in), optional :: LoadSource
        integer, intent (in) :: device, thisTerm
        integer :: iMeet, iDept, tLen1, tLen2, ierr, iSect, LoadFromDept
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_ROOM_CODE) :: tRoom
        character(len=MAX_LEN_CLASS_ID) :: tAction, tClassId
        integer, dimension(60,7) :: TimeTable
        logical :: conflicted, assigned, allowed_to_edit, allowed_to_show
        character(len=127) :: mesg

        call cgi_get_named_string(QUERY_STRING, 'A1', tRoom, ierr)

        call html_comment('room_schedule('//trim(tRoom)//')')

        targetRoom = index_to_room(tRoom)
        targetDepartment = Room(targetRoom)%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx
        mesg = SPACE
        allowed_to_edit = isRole_admin_of_college(targetCollege) .or. &
            ( isRole_chair_of_department(targetDepartment,orHigherUp) .and. &
            College(targetCollege)%isAllowed(ToEditCLASSES,thisTerm) )
        allowed_to_show = College(targetCollege)%isAllowed(ToShowTEACHERS,thisTerm) .or. &
                   isRole_chair_of_department(targetDepartment, orHigherUp)

        ! check if there are other arguments
        call cgi_get_named_string(QUERY_STRING, 'A2', tAction, ierr)

        if (ierr==0) then ! action is Add or Del
            call cgi_get_named_string(QUERY_STRING, 'A3', tClassId, ierr)
            iSect = index_to_section(tClassId, thisTerm)
            if (iSect>0 .and. .not. isRoleofficial) then ! target of action is indexed by iSect
                LoadFromDept = Section(thisTerm,iSect)%DeptIdx
                if (tAction=='Add') then
                    do iMeet=1,Section(thisTerm,iSect)%NMeets
                        Section(thisTerm,iSect)%RoomIdx(iMeet) = targetRoom
                    end do
                    mesg = 'Added '//tClassId
                end if
                if (tAction=='Del') then
                    do iMeet=1,Section(thisTerm,iSect)%NMeets
                        Section(thisTerm,iSect)%RoomIdx(iMeet) = 0
                    end do
                    mesg = 'Deleted '//tClassId
                end if
                call class_details_write(unitXML, thisTerm, dirCLASSES(thisTerm), iSect)
            end if

            if (isRoleOfficial) then
                mesg = '"'//trim(tAction)//SPACE//trim(tClassId)//'" failed. '//sorryMessageOfficial
            end if

        end if

        call html_write_header(device, 'Classes in room '//tRoom, mesg)

        ! collect classes in room iRoom
        call timetable_meetings_in_room(thisTerm, targetRoom, 0, tLen1, tArray, TimeTable, conflicted)

        call list_sections_to_edit(device, thisTerm, tLen1, tArray, &
            fnRoomSchedule, tRoom, 'Del', allowed_to_edit, allowed_to_show)

        if (tLen1>0) then
            call timetable_display(device, thisTerm, TimeTable)
        end if

        if (isRoleStudent .or. isRoleGuest) then
            write(device,AFORMAT) horizontal
            return
        end if

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
        do iSect=1,NumSections(thisTerm)
            if (LoadFromDept/=Section(thisTerm,iSect)%DeptIdx) cycle ! not in this unit
            if (Section(thisTerm,iSect)%NMeets==1 .and. Section(thisTerm,iSect)%DayIdx(1)==0) cycle ! meeting days/time not specified
            ! room(s) already assigned to this section?
            assigned = .false.
            do iMeet=1,Section(thisTerm,iSect)%NMeets
                if (Section(thisTerm,iSect)%RoomIdx(iMeet)/=0) assigned = .true.
            end do
            if (assigned) cycle ! section has a teacher
            if (.not. is_conflict_timetable_with_section(thisTerm, iSect, TimeTable)) then ! add to list
                do iMeet=1,Section(thisTerm,iSect)%NMeets
                    tArray(tLen1+tLen2+1) = iSect
                    tArray(tLen1+tLen2+2) = iMeet
                    tArray(tLen1+tLen2+3) = 0
                    tLen2 = tLen2+3
                end do
            end if
        end do
        tArray(tLen1+tLen2+1) = 0
        tArray(tLen1+tLen2+2) = 0
        tArray(tLen1+tLen2+3) = 0
        if (tLen2>0) then
            write(device,AFORMAT) horizontal
            call list_sections_to_edit(device, thisTerm, tLen2, tArray(tLen1+1), &
                fnRoomSchedule, tRoom, 'Add', allowed_to_edit, allowed_to_show, &
                b_bold//'Classes with TBA rooms in '//trim(Department(LoadFromDept)%Code)// &
                ' that fit available times in '//trim(tRoom)//e_bold)
        end if

        ! search for feasible classes in another unit?
        if ( .not. isRoleFaculty ) then
            call make_form_start(device, fnRoomSchedule, tRoom, A9=thisTerm)
            write(device,AFORMAT) linebreak//'Search for feasible classes in : <select name="A4">'
            do iDept=2,NumDepartments
                if (iDept/=LoadFromDept) then
                    ierr = 0
                else
                    ierr = 1
                end if
                write(device,AFORMAT) '<option value="'//trim(Department(iDept)%Code)//'"'//trim(selected(ierr))//'> '// &
                trim(Department(iDept)%Code)//DASH//trim(Department(iDept)%Name)
            end do
            write(device,AFORMAT) '</select>'//nbsp//'<input type="submit" value="Find classes">'//e_form
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
            b_tr//b_td//'Room code'//e_td//b_td//'<input name="Code" size="'//trim(itoa(MAX_LEN_ROOM_CODE))// &
            '" value="'//trim(tRoom)//'">'//e_td//e_tr

        write(device,AFORMAT) &
            b_tr//b_td//'Responsible unit'//e_td//b_td//'<select name="Department">'
        do i=2,NumDepartments
            if (i/=targetDepartment) then
                j=0
            else
                j=1
            end if
            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(Department(i)%Code)//'">'// &
            trim(Department(i)%Name)
        end do
        write(device,AFORMAT) '</select>'//e_td//e_tr, &
            b_tr//b_td//'Maximum seating capacity'//e_td//b_td//'<input name="MaxCapacity" size="3" value="'// &
            trim(itoa(wrk%MaxCapacity))//'">'//e_td//e_tr, &
            b_tr//b_td//'Cluster'//e_td//b_td//'<input name="Cluster" size="3" value="'// &
            trim(itoa(wrk%Cluster))//'">'//e_td//e_tr, &
            b_tr//b_td//'Energy fee'//e_td//b_td//'<input name="EnergyFee" size="3" value="'// &
            trim(itoa(wrk%EnergyFee))//'">'//e_td//e_tr

        write(device,AFORMAT) e_table//linebreak//nbsp// &
            '<input name="action" type="submit" value="'//trim(tAction)//'">'//e_form//'<pre>', &
            'NOTE: Rooms that are within walking distance of each other must belong to the same cluster.', &
            '</pre>'//horizontal

    end subroutine room_info


    subroutine subject_edit(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject, tAction, token
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer :: ierr, iSubj, i, j
        character (len=255) :: mesg, remark, line
        type (TYPE_SUBJECT) :: wrk
        logical :: isDirtySUBJECTS, criticalErr

        call html_comment('subject_edit()')

        ! which subject ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tSubject, iSubj)
        iSubj = index_to_subject(tSubject)
        wrk = Subject(iSubj) ! make a working copy

        ! check for other arguments
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
        isDirtySUBJECTS = .false.
        remark = SPACE

        if (trim(tAction)=='Update' .and. .not. isRoleOfficial) then

                call cgi_get_named_float(QUERY_STRING, 'Units', wrk%Units, ierr)
                if (ierr/=0) wrk%Units = Subject(iSubj)%Units
                if ( wrk%Units /= Subject(iSubj)%Units) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Units changed to '//ftoa(wrk%Units,1)
                end if

                call cgi_get_named_float(QUERY_STRING, 'Tuition', wrk%Tuition, ierr)
                if (ierr/=0) wrk%Tuition = Subject(iSubj)%Tuition
                if ( wrk%Tuition /= Subject(iSubj)%Tuition) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Tuition fee changed to '//ftoa(wrk%Tuition,2)
                end if

                call cgi_get_named_float(QUERY_STRING, 'LabFee', wrk%LabFee, ierr)
                if (ierr/=0) wrk%LabFee = Subject(iSubj)%LabFee
                if ( wrk%LabFee /= Subject(iSubj)%LabFee) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Lab fee changed to '//ftoa(wrk%LabFee,2)
                end if

                call cgi_get_named_float(QUERY_STRING, 'LectHours', wrk%LectHours, ierr)
                if (ierr/=0) wrk%LectHours = Subject(iSubj)%LectHours
                if ( wrk%LectHours /= Subject(iSubj)%LectHours) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Lecture Hours changed to '//ftoa(wrk%LectHours,2)
                end if

                call cgi_get_named_float(QUERY_STRING, 'LectLoad', wrk%LectLoad, ierr)
                if (ierr/=0) wrk%LectLoad = Subject(iSubj)%LectLoad
                if ( wrk%LectLoad /= Subject(iSubj)%LectLoad) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Lecture workload changed to '//ftoa(wrk%LectLoad,2)
                end if

                call cgi_get_named_integer(QUERY_STRING, 'MinLectSize', wrk%MinLectSize, ierr)
                if (ierr/=0) wrk%MinLectSize = Subject(iSubj)%MinLectSize
                if ( wrk%MinLectSize /= Subject(iSubj)%MinLectSize) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': MinLectSize changed to '//itoa(wrk%MinLectSize)
                end if

                call cgi_get_named_integer(QUERY_STRING, 'MaxLectSize', wrk%MaxLectSize, ierr)
                if (ierr/=0) wrk%MaxLectSize = Subject(iSubj)%MaxLectSize
                if ( wrk%MaxLectSize /= Subject(iSubj)%MaxLectSize) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': MaxLectSize changed to '//itoa(wrk%MaxLectSize)
                end if

                call cgi_get_named_float(QUERY_STRING, 'LabHours', wrk%LabHours, ierr)
                if (ierr/=0) wrk%LabHours = Subject(iSubj)%LabHours
                if ( wrk%LabHours /= Subject(iSubj)%LabHours) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': LabHours changed to '//ftoa(wrk%LabHours,2)
                end if

                call cgi_get_named_float(QUERY_STRING, 'LabLoad', wrk%LabLoad, ierr)
                if (ierr/=0) wrk%LabLoad = Subject(iSubj)%LabLoad
                if ( wrk%LabLoad /= Subject(iSubj)%LabLoad) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Lab workload changed to '//ftoa(wrk%LabLoad,2)
                end if

                call cgi_get_named_integer(QUERY_STRING, 'MinLabSize', wrk%MinLabSize, ierr)
                if (ierr/=0) wrk%MinLabSize = Subject(iSubj)%MinLabSize
                if ( wrk%MinLabSize /= Subject(iSubj)%MinLabSize) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': MinLabSize changed to '//itoa(wrk%MinLabSize)
                end if

                call cgi_get_named_integer(QUERY_STRING, 'MaxLabSize', wrk%MaxLabSize, ierr)
                if (ierr/=0) wrk%MaxLabSize = Subject(iSubj)%MaxLabSize
                if ( wrk%MaxLabSize /= Subject(iSubj)%MaxLabSize) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': MaxLabSize changed to '//itoa(wrk%MaxLabSize)
                end if

                call cgi_get_named_string(QUERY_STRING, 'Name', line, ierr)
                wrk%Name  = trim(line)
                if (ierr/=0) wrk%Name = Subject(iSubj)%Name
                if ( wrk%Name /= Subject(iSubj)%Name) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Name changed to '//wrk%Name
                end if

                call cgi_get_named_string(QUERY_STRING, 'Title', line, ierr)
                wrk%Title = trim(line)
                if (ierr/=0) wrk%Title = Subject(iSubj)%Title
                if ( wrk%Title /= Subject(iSubj)%Title) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Title changed to '//wrk%Title
                end if

                call cgi_get_named_string(QUERY_STRING, 'Department', tDepartment, ierr)
                wrk%DeptIdx = index_to_dept(tDepartment)
                if (ierr/=0 .or. wrk%DeptIdx<=0) wrk%DeptIdx = Subject(iSubj)%DeptIdx
                if ( wrk%DeptIdx /= Subject(iSubj)%DeptIdx) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Department changed to '//Department(wrk%DeptIdx)%Code
                end if

                call cgi_get_named_string(QUERY_STRING, 'TermOffered', token, ierr)
                j = 0
                if (index(token, '1')>0 ) j = j+1
                if (index(token, '2')>0 ) j = j+2
                if (index(token, 'S')>0 ) j = j+4
                wrk% TermOffered= j
                if (ierr/=0 .or. j==0) wrk%TermOffered = Subject(iSubj)%TermOffered
                if ( wrk%TermOffered /= Subject(iSubj)%TermOffered) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': TermOffered changed to '//text_term_offered_separated(wrk%TermOffered)
                end if

                call cgi_get_named_string(QUERY_STRING, 'Prerequisite', line, ierr)
                call tokenize_subjects(line, '+', MAX_ALL_SUBJECT_PREREQ, wrk%lenPreq, wrk%Prerequisite, ierr, mesg)
                !call html_comment(mesg)
                !do j=1,wrk%lenPreq
                !    call html_comment(itoa(j)//Subject(wrk%Prerequisite(j))%Name)
                !end do
                if (ierr/=0) then
                    wrk%lenPreq = Subject(iSubj)%lenPreq
                    wrk%Prerequisite = Subject(iSubj)%Prerequisite
                end if

                if ( wrk%lenPreq /= Subject(iSubj)%lenPreq) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Prerequisite changed to '//line
                else
                    do j=1,wrk%lenPreq
                        if (wrk%Prerequisite(j) == Subject(iSubj)%Prerequisite(j)) cycle
                        isDirtySUBJECTS = .true.
                        remark = trim(remark)//': Prerequisite changed to '//line
                        exit
                    end do
                end if

                call cgi_get_named_string(QUERY_STRING, 'Corequisite', line, ierr)
                call tokenize_subjects(line, '+', MAX_ALL_SUBJECT_COREQ, wrk%lenCoreq, wrk%Corequisite, ierr, mesg)
                !call html_comment(mesg)
                !do j=1,wrk%lenPreq
                !    call html_comment(itoa(j)//Subject(wrk%Prerequisite(j))%Name)
                !end do
                if (ierr/=0) then
                    wrk%lenCoreq = Subject(iSubj)%lenCoreq
                    wrk%Corequisite = Subject(iSubj)%Corequisite
                end if
                if ( wrk%lenCoreq /= Subject(iSubj)%lenCoreq) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Corequisite changed to '//line
                else
                    do j=1,wrk%lenCoreq
                        if (wrk%Corequisite(j) == Subject(iSubj)%Corequisite(j)) cycle
                        isDirtySUBJECTS = .true.
                        remark = trim(remark)//': Corequisite changed to '//line
                        exit
                    end do
                end if

                call cgi_get_named_string(QUERY_STRING, 'Concurrent', line, ierr)
                call tokenize_subjects(line, '+', MAX_ALL_SUBJECT_PREREQ, wrk%lenConc, wrk%Concurrent, ierr, mesg)
                !call html_comment(mesg)
                !do j=1,wrk%lenPreq
                !    call html_comment(itoa(j)//Subject(wrk%Prerequisite(j))%Name)
                !end do
                if (ierr/=0) then
                    wrk%lenConc = Subject(iSubj)%lenConc
                    wrk%Concurrent = Subject(iSubj)%Concurrent
                end if
                if ( wrk%lenConc /= Subject(iSubj)%lenConc) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Concurrent changed to '//line
                else
                    do j=1,wrk%lenConc
                        if (wrk%Concurrent(j) == Subject(iSubj)%Concurrent(j)) cycle
                        isDirtySUBJECTS = .true.
                        remark = trim(remark)//': Concurrent changed to '//line
                        exit
                    end do
                end if

                call cgi_get_named_string(QUERY_STRING, 'ConcPrerequisite', line, ierr)
                call tokenize_subjects(line, '+', MAX_ALL_SUBJECT_CONCPREQ, wrk%lenConcPreq, wrk%ConcPrerequisite, ierr, mesg)
                !call html_comment(mesg)
                !do j=1,wrk%lenPreq
                !    call html_comment(itoa(j)//Subject(wrk%Prerequisite(j))%Name)
                !end do
                if (ierr/=0) then
                    wrk%lenConcPreq = Subject(iSubj)%lenConcPreq
                    wrk%ConcPrerequisite = Subject(iSubj)%ConcPrerequisite
                end if
                if ( wrk%lenConcPreq /= Subject(iSubj)%lenConcPreq) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Prerequisite that can be taken concurrently changed to '//line
                else
                    do j=1,wrk%lenConcPreq
                        if (wrk%ConcPrerequisite(j) == Subject(iSubj)%ConcPrerequisite(j)) cycle
                        isDirtySUBJECTS = .true.
                        remark = trim(remark)//': Prerequisite that can be taken concurrently changed to '//line
                        exit
                    end do
                end if

                if (isDirtySUBJECTS) then
                    if ( wrk%Name /= Subject(iSubj)%Name) then
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
                                iSubj = NumSubjects+NumAdditionalSubjects
                                call subject_details_write(unitXML, dirSUBJECTS, iSubj)
                                tSubject = wrk%Name
                                remark = ': Added new subject '//wrk%Name
                                call subject_details_write(unitXML, trim(dirSUBJECTS)//'index', NumDummySubjects, &
                                    NumSubjects+NumAdditionalSubjects)
                                call get_subject_areas()
                            end if
                        else
                            remark = ': Add new subject failed; "'//trim(wrk%Name)//'" already exists.'
                        end if
                    else
                        ! update existing
                        Subject(iSubj) = wrk
                        call subject_details_write(unitXML, dirSUBJECTS, iSubj)
                    end if
                end if

        end if ! (trim(tAction)=='Update' ...

        if (len_trim(tAction)>0 .and. isRoleOfficial) then
            remark = '  Update "'//trim(tSubject)//'" failed. '//sorryMessageOfficial
        end if

        !if (isDirtySUBJECTS) call subject_details_write(unitXML, dirSUBJECTS, iSubj)

        targetDepartment = Subject(iSubj)%DeptIdx

        call html_write_header(device, 'Edit subject '//tSubject, remark(3:))
        j = index(tSubject,SPACE)
        if ( j<len_trim(tSubject) ) then
            write(device,AFORMAT) &
                trim(make_href(fnSubjectList, tSubject(:j-1), A1=tSubject(:j-1), &
                pre=b_small//b_italic//'Edit another '//nbsp, post=' subject'//e_italic//e_small))
        end if

        call make_form_start(device, fnEditSubject, tSubject)
        write(device,AFORMAT)  '<table border="0" width="100%">', &
            b_tr//b_td//'Subject code'//e_td//b_td//'<input name="Name" size="'//trim(itoa(MAX_LEN_SUBJECT_CODE))// &
            '" value="'//trim(tSubject)//'"> (A new subject will be created if this is changed)'//e_td//e_tr
        !b_tr//b_td//'Subject code'//e_td//b_td//trim(tSubject)//' (Cannot be changed)'//e_td//e_tr
        write(device,AFORMAT) &
            b_tr//b_td//'Responsible unit'//e_td//b_td//'<select name="Department">'
        do i=2,NumDepartments
            if (i/=targetDepartment) then
                j=0
            else
                j=1
            end if
            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(Department(i)%Code)//'">'// &
                trim(Department(i)%Name)
        end do
        write(device,AFORMAT) '</select>'//e_td//e_tr, &
            b_tr//b_td//'Title'//e_td//b_td//'<input name="Title" size="'//trim(itoa(MAX_LEN_SUBJECT_TITLE))// &
            '" value="'//trim(Subject(iSubj)%Title)//'">'//e_td//e_tr, &
            b_tr//b_td//'Units'//e_td//b_td//'<input name="Units" size="3" value="'// &
            trim(ftoa(Subject(iSubj)%Units,1))//'"> (0, if non-credit; i.e., PE, NSTP)'//e_td//e_tr, &
            b_tr//b_td//'Term Offered'//e_td//b_td//'<input name="TermOffered" size="3" value="'// &
            trim(text_term_offered_separated(Subject(iSubj)%TermOffered))//'"> (1, 2, S, or combination)'//e_td//e_tr, &
            b_tr//b_td//'Tuition fee'//e_td//b_td//'<input name="Tuition" size="3" value="'// &
            trim(ftoa(Subject(iSubj)%Tuition,2))//'"> (total amount)'//e_td//e_tr, &
            b_tr//b_td//'Subject fee'//e_td//b_td//'<input name="LabFee" size="3" value="'// &
            trim(ftoa(Subject(iSubj)%LabFee,2))//'"> (total amount; additional to tuition)'//e_td//e_tr
        write(device,AFORMAT)  &
            b_tr//b_td//'Hours lecture class'//e_td//b_td//'<input name="LectHours" size="3" value="'// &
            trim(ftoa(Subject(iSubj)%LectHours,2))//'"> (0, if no lecture component)'//e_td//e_tr, &
            b_tr//b_td//'Workload for lecture class'//e_td//b_td//'<input name="LectLoad" size="3" value="'// &
            trim(ftoa(Subject(iSubj)%LectLoad,2))//'"> (0, if no lecture component)'//e_td//e_tr, &
            b_tr//b_td//'Min size lecture class'//e_td//b_td//'<input name="MinLectSize" size="3" value="'// &
            trim(itoa(Subject(iSubj)%MinLectSize))//'">'//e_td//e_tr, &
            b_tr//b_td//'Max size lecture class'//e_td//b_td//'<input name="MaxLectSize" size="3" value="'// &
            trim(itoa(Subject(iSubj)%MaxLectSize))//'">'//e_td//e_tr, &
            b_tr//b_td//'Hours lab/recit/comp class'//e_td//b_td//'<input name="LabHours" size="3" value="'// &
            trim(ftoa(Subject(iSubj)%LabHours,2))//'"> (0, if no lab/recit/computations component)'//e_td//e_tr, &
            b_tr//b_td//'Workload for lab class'//e_td//b_td//'<input name="LabLoad" size="3" value="'// &
            trim(ftoa(Subject(iSubj)%LabLoad,2))//'"> (0, if no lab/recit/computations component)'//e_td//e_tr, &
            b_tr//b_td//'Min size lab/recit/comp class'//e_td//b_td//'<input name="MinLabSize" size="3" value="'// &
            trim(itoa(Subject(iSubj)%MinLabSize))//'">'//e_td//e_tr, &
            b_tr//b_td//'Max size lab/recit/comp class'//e_td//b_td//'<input name="MaxLabSize" size="3" value="'// &
            trim(itoa(Subject(iSubj)%MaxLabSize))//'">'//e_td//e_tr
        !      lenPreq, Prerequisite(MAX_ALL_SUBJECT_PREREQ), &
        i = Subject(iSubj)%Prerequisite(1)
        mesg = Subject(i)%Name
        do j=2,Subject(iSubj)%lenPreq
            i = Subject(iSubj)%Prerequisite(j)
            mesg = trim(mesg)//'+'//Subject(i)%Name
        end do
        write(device,AFORMAT) &
            b_tr//b_td//'Prerequisite'//e_td//b_td//'<input name="Prerequisite" size="'// &
            trim(itoa(Subject(iSubj)%lenPreq*2*MAX_LEN_SUBJECT_CODE/3))//'" value="'//trim(mesg)//'">'//e_td//e_tr
        !      lenCoreq, Corequisite(MAX_ALL_SUBJECT_COREQ), &
        i = Subject(iSubj)%Corequisite(1)
        mesg = Subject(i)%Name
        do j=2,Subject(iSubj)%lenCoreq
            i = Subject(iSubj)%Corequisite(j)
            mesg = trim(mesg)//'+'//Subject(i)%Name
        end do
        write(device,AFORMAT) &
            b_tr//b_td//'Corequisite'//e_td//b_td//'<input name="Corequisite" size="'// &
            trim(itoa(Subject(iSubj)%lenCoreq*2*MAX_LEN_SUBJECT_CODE/3))//'" value="'//trim(mesg)//'">'//e_td//e_tr
        !      lenConc, Concurrent(MAX_ALL_SUBJECT_CONCURRENT), &
        i = Subject(iSubj)%Concurrent(1)
        mesg = Subject(i)%Name
        do j=2,Subject(iSubj)%lenConc
            i = Subject(iSubj)%Concurrent(j)
            mesg = trim(mesg)//'+'//Subject(i)%Name
        end do
        write(device,AFORMAT) &
            b_tr//b_td//'Concurrent with'//e_td//b_td//'<input name="Concurrent" size="'// &
            trim(itoa(Subject(iSubj)%lenConc*2*MAX_LEN_SUBJECT_CODE/3))//'" value="'//trim(mesg)//'">'//e_td//e_tr
        !      lenConcPreq, ConcPrerequisite(MAX_ALL_SUBJECT_CONCPREQ)
        i = Subject(iSubj)%ConcPrerequisite(1)
        mesg = Subject(i)%Name
        do j=2,Subject(iSubj)%lenConcPreq
            i = Subject(iSubj)%ConcPrerequisite(j)
            mesg = trim(mesg)//'+'//Subject(i)%Name
        end do
        write(device,AFORMAT) &
            b_tr//b_td//'Prerequisite that can be concurrent'//e_td//b_td//'<input name="ConcPrerequisite" size="'// &
            trim(itoa(Subject(iSubj)%lenConcPreq*2*MAX_LEN_SUBJECT_CODE/3))//'" value="'//trim(mesg)//'">'//e_td//e_tr

        write(device,AFORMAT) e_table//linebreak//nbsp//'<input name="action" type="submit" value="Update">'//e_form//'<pre>', &
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


    subroutine subject_search_info(iSubj, searchString, location)
        integer, intent(in) :: iSubj
        character(len=*), intent(in) :: searchString
        character(len=80), intent(out) :: location
        integer :: i, j
        character (len=255) :: mesg

        location = SPACE

        i = Subject(iSubj)%Prerequisite(1)
        mesg = Subject(i)%Name
        do j=2,Subject(iSubj)%lenPreq
            i = Subject(iSubj)%Prerequisite(j)
            mesg = trim(mesg)//'+'//Subject(i)%Name
        end do
        if (index(mesg,searchString)>0) location = 'Prerequisite : '//location
        i = Subject(iSubj)%Corequisite(1)
        mesg = Subject(i)%Name
        do j=2,Subject(iSubj)%lenCoreq
            i = Subject(iSubj)%Corequisite(j)
            mesg = trim(mesg)//'+'//Subject(i)%Name
        end do
        if (index(mesg,searchString)>0) location = 'Corequisite : '//location
        i = Subject(iSubj)%Concurrent(1)
        mesg = Subject(i)%Name
        do j=2,Subject(iSubj)%lenConc
            i = Subject(iSubj)%Concurrent(j)
            mesg = trim(mesg)//'+'//Subject(i)%Name
        end do
        if (index(mesg,searchString)>0) location = 'Concurrent : '//location
        i = Subject(iSubj)%ConcPrerequisite(1)
        mesg = Subject(i)%Name
        do j=2,Subject(iSubj)%lenConcPreq
            i = Subject(iSubj)%ConcPrerequisite(j)
            mesg = trim(mesg)//'+'//Subject(i)%Name
        end do
        if (index(mesg,searchString)>0) location = 'ConcPrerequisite : '//location
        if (index(Subject(iSubj)%Title,searchString)>0) location = 'Title : '//location
        if (index(Subject(iSubj)%Name,searchString)>0) location = 'Name : '//location

    end subroutine subject_search_info


    subroutine subject_list_all (device)
        integer, intent (in) :: device
        integer :: idx, nSubjects, ierr!, coll, ncol, iSubj, maxcol=7
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        !character(len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=80) :: header

        ! which unit ?
        tArray = 0
        call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)
        targetDepartment = index_to_dept(tDepartment)
        if (targetDepartment>1) then
            targetCollege = Department(targetDepartment)%CollegeIdx
            nSubjects = 0
            do idx=1,NumSubjects+NumAdditionalSubjects
#if defined UPLB
                if (Subject(idx)%DeptIdx == targetDepartment) then
                    nSubjects = nSubjects+1
                    tArray(nSubjects) = idx
                end if
#else
                if (Subject(idx)%DeptIdx == targetDepartment .or. isSubject_used_in_college(targetCollege, idx)) then
                    nSubjects = nSubjects+1
                    tArray(nSubjects) = idx
                end if
#endif
            end do
            header = 'Subjects in '//Department(targetDepartment)%Name

        else ! try subject area
            if (ierr/=0 .or. tDepartment==SPACE) then
                targetDepartment = DeptIdxUser
                targetCollege = CollegeIdxUser
                call html_write_header(device, 'List of subjects', horizontal//linebreak//'Department or subject area not found')
                return
            else
                ! make list of subjects in subject area to display
                call cgi_get_named_string(QUERY_STRING, 'A2', tCollege, ierr)
                if (ierr==0) then
                    targetCollege = index_to_college(tCollege)
                else
                    targetCollege = NumColleges
                    tCollege = College(targetCollege)%Code
                end if
                nSubjects = 0
                do idx=1,NumSubjects+NumAdditionalSubjects
                    if (index(Subject(idx)%Name,trim(tDepartment)//SPACE)==1) then
#if defined UPLB
                        nSubjects = nSubjects + 1
                        tArray(nSubjects) = idx
#else
                        if (isSubject_used_in_college (targetCollege, idx)) then
                            nSubjects = nSubjects + 1
                            tArray(nSubjects) = idx
                        end if
#endif
                    end if
                end do
                header = '"'//trim(tDepartment)//'" subjects in '//tCollege
                tDepartment = tCollege
                targetDepartment = index_to_dept(tDepartment)
            end if
        end if
        call html_subject_list (device, nSubjects, tArray(0:nSubjects), header)

    end subroutine subject_list_all


    subroutine html_subject_list (device, nSubjects, tArray, header)
        integer, intent (in) :: device, nSubjects
        integer, intent (in out) :: tArray(0:nSubjects)
        character (len=*), intent (in) :: header

        integer :: idx, iSubj, ncol, maxcol=7 !, ierr,
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject

        call html_write_header(device, header)

        if (nSubjects==0) then
            write(device,AFORMAT) JUSTNONE
        else

            ! write shortcut to subjects
            write(device,AFORMAT) '<table border="0" width="100%">'
            ncol = 0
            do idx=1,nSubjects
                tSubject = Subject(tArray(idx))%Name
                ncol = ncol + 1
                if (ncol == 1) then
                    write(device,AFORMAT) b_tr//b_td//'<a href="#'//trim(tSubject)//'">'//trim(tSubject)//'</a>'//e_td
                else if (ncol == maxcol) then
                    write(device,AFORMAT) b_td//'<a href="#'//trim(tSubject)//'">'//trim(tSubject)//'</a>'//e_td//e_tr
                    ncol = 0
                else
                    write(device,AFORMAT) b_td//'<a href="#'//trim(tSubject)//'">'//trim(tSubject)//'</a>'//e_td
                end if
            end do
            if (ncol /= 0)  then
                do idx=ncol+1,maxcol
                    write(device,AFORMAT) b_td_nbsp_e_td
                end do
                write(device,AFORMAT) e_tr
            end if
            write(device,AFORMAT) e_table//horizontal//linebreak

            write(device,AFORMAT) '<table border="0" cellspacing="0" width="100%">'//b_small
            do idx=1,nSubjects
                iSubj = tArray(idx)
                tSubject = Subject(iSubj)%Name

                if (mod(idx,6)==1) write(device,AFORMAT) b_tr//b_thal//'Subject'//e_th// &
                    b_th//'Units'//e_th//b_th//'Term'//e_th//b_th//'Tuition'//e_th//b_th//'Lab fee'//e_th// &
                    b_th//'Lect hrs'//e_th//b_th//'Lect load'//e_th//b_th//'Min Size'//e_th// &
                    b_th//'Max Size'//e_th,  &
                    b_th//'Lab hrs'//e_th//b_th//'Lab load'//e_th//b_th//'Min Size'//e_th// &
                    b_th//'Max Size'//e_th//e_tr, &
                    b_tr//'<td colspan="11">'//nbsp//e_td//e_tr

                write(device,AFORMAT) b_tr//b_td//'<a name="'//trim(tSubject)//'">'//b_bold//'Name: '//e_bold
                if (isRoleSysAd .or. isRoleStaff .or. isRoleOfficial) then
                    write(device,AFORMAT) trim(make_href(fnEditSubject, tSubject, A1=tSubject))
                else
                    write(device,AFORMAT) tSubject
                end if
                write(device,AFORMAT) '</a>'//e_td, &
                    b_tdac//trim(ftoa(Subject(iSubj)%Units,1))//e_td// &
                    b_tdac//trim(text_term_offered(Subject(iSubj)%TermOffered))//e_td// &
                    b_tdac//trim(ftoa(Subject(iSubj)%Tuition,2))//e_td// &
                    b_tdac//trim(ftoa(Subject(iSubj)%LabFee,2))//e_td// &
                    b_tdac//trim(ftoa(Subject(iSubj)%LectHours,2))//e_td// &
                    b_tdac//trim(ftoa(Subject(iSubj)%LectLoad,2))//e_td// &
                    b_tdac//trim(itoa(Subject(iSubj)%MinLectSize))//e_td// &
                    b_tdac//trim(itoa(Subject(iSubj)%MaxLectSize))//e_td,  &
                    b_tdac//trim(ftoa(Subject(iSubj)%LabHours,2))//e_td// &
                    b_tdac//trim(ftoa(Subject(iSubj)%LabLoad,2))//e_td// &
                    b_tdac//trim(itoa(Subject(iSubj)%MinLabSize))//e_td// &
                    b_tdac//trim(itoa(Subject(iSubj)%MaxLabSize))//e_td//e_tr

                write(device,AFORMAT) &
                    b_tr//'<td colspan="13">'//b_bold//'Title: '//e_bold//trim(Subject(iSubj)%Title)//e_td//e_tr, &
                    b_tr//'<td colspan="13">'//b_bold//'Preq. '//e_bold//trim(text_prerequisite_of_subject(iSubj,0))// &
                        e_td//e_tr, &
                    b_tr//'<td colspan="13">'//nbsp//e_td//e_tr
            end do

            write(device,AFORMAT) e_small//e_table

        end if
        write(device,AFORMAT) horizontal

    end subroutine html_subject_list



    subroutine curriculum_edit(device, given)
        integer, intent(in), optional :: given
        integer, intent (in) :: device
        integer :: iSubj, i, j, k, ierr, idx, tdx, m, iYear, iTerm, ptrS
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

            remark = '  Update "'//trim(Curriculum(targetCurriculum)%Code)//'" not enabled. '//sorryMessageOfficial

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
                    call rank_to_year_term(tdx, iYear, iTerm)
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
                    iSubj = Curriculum(targetCurriculum)%SubjectIdx(idx)
                    tdx = Curriculum(targetCurriculum)%SubjectTerm(idx)
                    possibleImpact = .false.
                    i = index_of_subject_in_curriculum (wrk, iSubj)
                    if (i==0) then ! iSubj not in wrk, deleted from targetCurriculum
                        changed = .true.
                        possibleImpact = .true.
                        remark = trim(remark)//': Deleted '//Subject(iSubj)%Name
                        call html_comment('>>> Deleted '//Subject(iSubj)%Name)
                    else ! iSubj retained; check if moved to another term
                        if (tdx/=wrk%SubjectTerm(i)) then ! but moved to a different semester
                            changed = .true.
                            possibleImpact = .true.
                            remark = trim(remark)//': Moved '//Subject(iSubj)%Name
                            call html_comment('>>> Moved '//Subject(iSubj)%Name)
                        end if
                    end if
                    !if (possibleImpact) then ! check if iSubj is used in a block section
                    !    call rank_to_year_term(tdx, iYear, iTerm)
                    !    do i=1,NumBlocks
                    !        if (Block(i)%CurriculumIdx==targetCurriculum .and. &
                    !            Block(i)%Year==iYear .and. Block(i)%Term==iTerm) then
                    !            remark = trim(remark)//', affects '//Block(i)%BlockID
                    !            call html_comment('>>> Change in '//Subject(iSubj)%Name//'may affect '//Block(i)%Name)
                    !        end if
                    !    end do
                    !end if
                end do

                ! check for additional subjects to original curriculum
                do idx=1,wrk%NSubjects
                    iSubj = wrk%SubjectIdx(idx)
                    tdx = wrk%SubjectTerm(idx)
                    i = index_of_subject_in_curriculum (Curriculum(targetCurriculum), iSubj)
                    if (i==0) then ! iSubj added to targetCurriculum
                        changed = .true.
                        remark = trim(remark)//': Added '//Subject(iSubj)%Name
                        call html_comment('>>> Added '//Subject(iSubj)%Name)
                        !call rank_to_year_term(tdx, iYear, iTerm)
                        !do i=1,NumBlocks
                        !    if (Block(i)%CurriculumIdx==targetCurriculum .and. &
                        !        Block(i)%Year==iYear .and. Block(i)%Term==iTerm) then
                        !        remark = trim(remark)//', affects '//Block(i)%BlockID
                        !        call html_comment('>>> Addition of '//Subject(iSubj)%Name//' affects '//Block(i)%Name)
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

                                ! update students whose curriculum index is NumCurricula
                                do k=1,NumStudents+NumAdditionalStudents
                                    if (Student(k)%CurriculumIdx == NumCurricula) Student(k)%CurriculumIdx = NumCurricula+1
                                end do

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
                                call curriculum_details_write(unitXML, trim(dirCURRICULA)//'index', 1, NumCurricula-1)
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
            call curriculum_details_write(unitXML, dirCURRICULA, targetCurriculum)
        end if

        call html_write_header(device, 'Edit curriculum '//tCurriculum, remark(3:))
        write(device,AFORMAT) trim(make_href(fnCurriculumList, CurrProgCode(targetCurriculum), &
            A1=CurrProgCode(targetCurriculum), &
            pre=b_small//'Edit other'//nbsp, post=' option'//e_small))//linebreak

        call make_form_start(device, fnEditCurriculum, tCurriculum)
        write(device,AFORMAT) '<table border="0" width="100%">', &
            b_tr//b_td//b_bold//'Curriculum code'//e_bold//e_td//b_td//'<input name="Code" size="'// &
            trim(itoa(MAX_LEN_CURRICULUM_CODE))// &
            '" value="'//trim(tCurriculum)//'"> (A new curriculum will be created if this is changed)'//e_td//e_tr

        write(device,AFORMAT) &
            b_tr//b_td//b_bold//'College'//e_bold//e_td//b_td//'<select name="College">'
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

        write(device,AFORMAT) '</select>'//e_td//e_tr, &
            b_tr//b_td//b_bold//'Title'//e_bold//e_td//b_td//'<input name="Title" size="'// &
                trim(itoa(MAX_LEN_CURRICULUM_NAME))//&
            '" value="'//trim(Curriculum(targetCurriculum)%Title)//'">'//e_td//e_tr, &
            b_tr//b_td//b_bold//'Specialization'//e_bold//e_td//b_td//'<input name="Specialization" size="'// &
            trim(itoa(MAX_LEN_CURRICULUM_NAME))//'" value="'//trim(Curriculum(targetCurriculum)%Specialization)// &
            '">'//e_td//e_tr, &
            b_tr//b_td//b_bold//'Remark'//e_bold//e_td// &
            b_td//'<input name="Remark" size="'//trim(itoa(MAX_LEN_CURRICULUM_NAME))// &
            '" value="'//trim(Curriculum(targetCurriculum)%Remark)//'">'//e_td//e_tr, &
            b_tr//b_td//b_bold//'Status'//e_bold//e_td//b_td//trim(mesg)//e_td//e_tr, &
            b_tr//b_td//b_bold//'Year, Term (Units/Cumulative)'//e_bold//e_td// &
            b_td//b_bold//'Comma-separated subject codes'//e_bold//e_td//e_tr

        tunits = 0.0

        do tdx=1,Curriculum(targetCurriculum)%NumTerms+6

            call rank_to_year_term(tdx, iYear, iTerm)

            m = 0
            credit = 0
            mesg = SPACE

            do idx=1,Curriculum(targetCurriculum)%NSubjects
                if (Curriculum(targetCurriculum)%SubjectTerm(idx) == tdx) then
                    iSubj = Curriculum(targetCurriculum)%SubjectIdx(idx)
                    m = m+1
                    credit = credit + Subject(iSubj)%Units
                    mesg = trim(mesg)//COMMA//SPACE//Subject(iSubj)%Name
                end if
            end do

            tUnits = tUnits + credit

            write(device,AFORMAT) b_tr//b_td// &
                trim(txtYear(iYear+10))//' Year, '//trim(txtSemester(iTerm+6))//' Term ('// &
                trim(ftoa(credit,1))//FSLASH//trim(ftoa(tUnits,1))//')'//e_td, &
                b_td//'<input name="Subjects'//trim(itoa(tdx))//'" size="'//trim(itoa(MAX_LEN_CURRICULUM_NAME))// &
                '" value="'//trim(mesg(3:))//'">'//e_td//e_tr
        end do

        write(device,AFORMAT) b_tr//b_td//b_bold//'Substitution rules'//e_bold//e_td, &
            b_td//'Required subjects in list will be PASSED if credits have been earned '// &
            'for the other subjects in the list'//e_td//e_tr

        do tdx=1,NumSubst
            if (Substitution(SubstIdx(tdx))==targetCurriculum) then
                mesg = SPACE
                do j=SubstIdx(tdx)+1, SubstIdx(tdx+1)-1
                    mesg = trim(mesg)//COMMA//SPACE//Subject(Substitution(j))%Name
                end do
                write(device,AFORMAT) b_tr//b_td//'SUBSTITUTION'//e_td//b_td//trim(mesg(3:))//e_td//e_tr
            end if
        end do

        write(device,AFORMAT) b_tr//b_td//'SUBSTITUTION'//e_td// &
            b_td//'<input name="Substitution" size="'//trim(itoa(MAX_LEN_CURRICULUM_NAME))// &
            '" value="(Enter new substitution rule)">'//e_td//e_tr

        write(device,AFORMAT) e_table//linebreak//nbsp//'<input name="action" type="submit" value="Update">'//e_form//'<pre>', &
            '</pre>'//horizontal

    end subroutine curriculum_edit


    subroutine equivalencies_edit(device)
        integer, intent (in) :: device
        integer :: idx, tdx, ierr, iSubj, ptrS, i, m!, j, k, iYear, iTerm
        character (len=255) :: mesg, remark!, tokenizeErr
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        logical :: changed, critical1, critical2!, possibleImpact, critical3
        integer, dimension(MAX_SECTION_MEETINGS) :: subjectList

        changed = .false.
        remark = SPACE

        if (isRoleOfficial) then
            remark = '  Edit equivalence rule not enabled. '//sorryMessageOfficial

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
                iSubj = index_to_subject(tSubject)
                if (iSubj>0) then ! required subject is OK
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
                call equivalence_data_write(unitXML, trim(pathToYear)//'EQUIVALENCIES.XML')
            else
                call cgi_get_named_string(QUERY_STRING, 'action', tSubject, ierr)
                if (ierr==0) remark = ' : No changes to equivalence rules?'
            end if

        end if


        call html_write_header(device, 'Equivalence rules applicable to all curricular programs', remark(3:))
        write(device,AFORMAT) 'Required subject will be PASSED if credits have been earned '// &
            'for Equivalent(s). Checked rules will be deleted.'//linebreak//linebreak

        call make_form_start(device, fnEditEquivalencies)
        write(device,AFORMAT) '<table border="0" width="50%">', b_tr// &
            b_thal//'Required'//e_th// &
            b_thal//'Equivalent(s)'//e_th// &
            b_thal//'Delete?'//e_th//e_tr
        do tdx=1,NumSubst
            if (Substitution(SubstIdx(tdx))==-1) then
                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(tdx,2))//'">'// &
                    b_td//trim(Subject(Substitution(SubstIdx(tdx)+1))%Name)//e_td
                mesg = SPACE
                do idx=SubstIdx(tdx)+2, SubstIdx(tdx+1)-1
                    mesg = trim(mesg)//COMMA//Subject(Substitution(idx))%Name
                end do
                write(device,AFORMAT) b_td//trim(mesg(2:))//e_td// &
                    b_td//'<input type="checkbox" name="del'//trim(itoa(tdx))//'">'//e_td//e_tr
            end if
        end do

        write(device,AFORMAT) b_tr// &
            b_td//'<input name="Required" size="'//trim(itoa(MAX_LEN_SUBJECT_CODE))// &
            '" value="">'//e_td// &
            b_td//'<input name="Equivalent" size="'//trim(itoa(MAX_LEN_SUBJECT_CODE))// &
            '" value="">'//e_td//b_td//b_bold//'Add rule'//e_bold//e_td//e_tr

        write(device,AFORMAT) e_table//linebreak//nbsp//'<input name="action" type="submit" value="Update">'//e_form//'<pre>', &
            '</pre>'//horizontal

    end subroutine equivalencies_edit




    subroutine curriculum_list_all(device, fn)
        integer, intent (in) :: device, fn
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        !character(len=10) :: tStatus, tAction ! (ACTIVE)/(INACTIVE), Activate/Deactivate
        integer :: ierr, ldx, ncurr!, fnAction
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

            mesg = 'Activate/deactivate curriculum not enabled. '//sorryMessageOfficial

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

            if (mesg/=SPACE) call curriculum_details_write(unitXML, dirCURRICULA, targetCurriculum)

        end if

        ncurr = 0
        tArray = 0
        do ldx=1,NumCurricula-1
            if (CurrProgCode(ldx) /= tCurriculum) cycle
            ncurr = ncurr+1
            tArray(ncurr) = ldx
        end do
        call html_curriculum_list(device, fn, ncurr, tArray(0:ncurr), tCurriculum//' options', mesg, SPACE)

    end subroutine curriculum_list_all


    subroutine html_curriculum_list(device, fn, ncurr, tArray, header, mesg, searchString)
        integer, intent (in) :: device, fn, ncurr, tArray(0:ncurr)
        character(len=*), intent(in) :: header, mesg, searchString
        integer :: cdx, ldx, tLen
        character (len=80) :: tString, location

        !character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character(len=10) :: tStatus, tAction ! (ACTIVE)/(INACTIVE), Activate/Deactivate
        integer :: fnAction !ierr,

        tString = searchString
        tLen = max(len_trim(tString),1)

        call html_write_header(device, header, mesg)

        ! collect curricula
        write(device,AFORMAT) '<ol>'
        do cdx=1,ncurr
            ldx =tArray(cdx)
            write(device,AFORMAT) trim(make_href(fnCurriculum, Curriculum(ldx)%Code, &
                A1=Curriculum(ldx)%Code, &
                pre=b_item, post=' - '//trim(Curriculum(ldx)%Title)))
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

            write(device,AFORMAT) nbsp//b_italic//tStatus//e_italic//nbsp

            if (isRole_admin_of_college(targetCollege)) then
                write(device,AFORMAT) trim(make_href(fnAction, tAction, A1=Curriculum(ldx)%Code, &
                    pre=nbsp//b_small, post=nbsp))
                write(device,AFORMAT) trim(make_href(fnEditCurriculum, 'Edit', A1=Curriculum(ldx)%Code, &
                    pre=nbsp, post=e_small))
            end if

            if (fn==fnSearchCategory) then
                call curriculum_search_info(ldx, tString(:tLen), location)
                ldx = len_trim(location)
                write(device,AFORMAT) nbsp//nbsp//b_small//b_italic//trim(tString)//' in '// &
                    location(:ldx-2)//e_italic//e_small
            end if

            write(device,AFORMAT) e_item

        end do
        write(device,AFORMAT) '</ol>'//horizontal

    end subroutine html_curriculum_list


    subroutine curriculum_search_info(curr, searchString, location)
        integer, intent(in) :: curr
        character(len=*), intent(in) :: searchString
        character(len=80), intent(out) :: location
        character(len=80) :: subjectInfo
        integer :: idx

        location = SPACE
        do idx=1,Curriculum(curr)%NSubjects
            call subject_search_info(Curriculum(curr)%SubjectIdx(idx), searchString, subjectInfo)
            if (len_trim(subjectInfo)>0) then
                location = 'Subjects : '//location
                exit
            end if
        end do
        if (index(Curriculum(curr)%Remark,searchString)>0) location = 'Remark : '//location
        if (index(Curriculum(curr)%Specialization,searchString)>0) location = 'Specialization : '//location
        if (index(Curriculum(curr)%Title,searchString)>0) location = 'Title : '//location
        if (index(Curriculum(curr)%Code,searchString)>0) location = 'Code : '//location

    end subroutine curriculum_search_info


    subroutine curriculum_display(device, given)
        integer, intent(in), optional :: given
        integer, intent (in) :: device
        integer :: idx, tdx, m, n, iYear, iTerm, fnAction
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

        write(device,AFORMAT) b_bold//trim(Curriculum(targetCurriculum)%Code)//' - '// &
        trim(Curriculum(targetCurriculum)%Title)//e_bold
        if (len_trim(Curriculum(targetCurriculum)%Specialization) > 0) then
            write(device,AFORMAT) b_bold//' : '//trim(Curriculum(targetCurriculum)%Specialization)//e_bold
        end if
        if (len_trim(Curriculum(targetCurriculum)%Remark) > 0) then
            write(device,AFORMAT) b_bold//' : '//trim(Curriculum(targetCurriculum)%Remark)//e_bold
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
        write(device,AFORMAT) nbsp//b_italic//tStatus//e_italic//nbsp
        if (isRole_admin_of_college(targetCollege)) then
            write(device,AFORMAT) trim(make_href(fnAction, tAction, A1=Curriculum(targetCurriculum)%Code, &
                pre=nbsp//b_small, post=nbsp))
            write(device,AFORMAT) trim(make_href(fnEditCurriculum, 'Edit', A1=Curriculum(targetCurriculum)%Code, &
                pre=nbsp, post=e_small))
        end if

        write(device,AFORMAT) linebreak//'Note: A '//red//'SUBJECT'//e_color//' in column '//b_bold// &
            b_italic//'Prerequisite'//e_italic//e_bold, &
            ' indicates an inconsistency. Said '//red//'SUBJECT'//e_color// &
            ' is not present in the curriculum, or is not taken in a prior term, ', &
            ' or the prerequisite expression should be "SUBJECT1 OR SUBJECT2" where either one is taken in a prior term.', &
            linebreak//'<table border="1" width="100%">'
        cumulative = 0.0
        do tdx=1,Curriculum(targetCurriculum)%NumTerms

            call rank_to_year_term(tdx, iYear, iTerm)
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
                write(device,AFORMAT) b_tr//'<td colspan="6">'//nbsp//e_td//e_tr, &
                    b_tr//'<td colspan="6"> '//b_bold//trim(Curriculum(targetCurriculum)%Code)//': '// &
                    trim(txtYear(iYear))//' Year, '// &
                    trim(txtSemester(iTerm+3))//' Term ('//trim(ftoa(tUnits,1))//' units; '// &
                    trim(ftoa(cumulative,1))//' cumulative)' &
                    //e_bold//e_td//e_tr

                write(device,AFORMAT) b_tr, &
                    b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td//&
                    b_td//b_bold//b_italic//'Lect'//e_italic//e_bold//e_td, &
                    b_td//b_bold//b_italic//'Lab'//e_italic//e_bold//e_td, &
                    b_td_nbsp_e_td, e_tr
                write(device,AFORMAT) b_tr, &
                    b_td//b_bold//b_italic//'Subject'//e_italic//e_bold//e_td, &
                    b_td//b_bold//b_italic//'Title'//e_italic//e_bold//e_td, &
                    b_td//b_bold//b_italic//'Units'//e_italic//e_bold//e_td, &
                    b_td//b_bold//b_italic//'Hrs'//e_italic//e_bold//e_td, &
                    b_td//b_bold//b_italic//'Hrs'//e_italic//e_bold//e_td, &
                    b_td//b_bold//b_italic//'Prerequisite'//e_italic//e_bold//e_td, &
                    e_tr
                do idx=1,Curriculum(targetCurriculum)%NSubjects
                    if (Curriculum(targetCurriculum)%SubjectTerm(idx) /= tdx) cycle
                    n = Curriculum(targetCurriculum)%SubjectIdx(idx)
                    if (isRole_admin_of_college(targetCollege) .or. isRoleOfficial) then
                        write(device,AFORMAT) b_tr//b_td, &
                            trim(make_href(fnEditSubject, Subject(n)%Name, A1=Subject(n)%Name, A2=College(targetCollege)%Code))
                    else
                        write(device,AFORMAT) b_tr//b_td//trim(Subject(n)%Name)
                    end if
                    write(device,AFORMAT) e_td//b_td//trim(Subject(n)%Title)//e_td// &
                        b_tdac//trim(ftoa(Subject(n)%Units,1))//e_td//&
                        b_tdac//trim(ftoa(Subject(n)%LectHours,2))//e_td//&
                        b_tdac//trim(ftoa(Subject(n)%LabHours,2))//e_td//&
                        '<td width="20%">'//trim(text_prerequisite_in_curriculum(n,Curriculum(targetCurriculum)))//e_td//&
                        e_tr
                end do
            end if
        end do

        write(device,AFORMAT) e_table//horizontal

    end subroutine curriculum_display


    subroutine display_fees(device, mesg)
        integer, intent(in) :: device
        character(len=*), intent (in) :: mesg

        integer :: idx

        call html_comment('display_fees()')

        targetDepartment = DeptIdxUser
        targetCollege = CollegeIdxUser

        call html_write_header(device, 'Update school fees', mesg)

        call make_form_start(device, fnEditFees)

        write(device,AFORMAT) '<table border="0" width="100%">', &
            b_tr//b_thar//'Amount'//e_th//b_td_nbsp_e_td//b_thal//'Description'//e_th//e_tr
        do idx=1,MAX_ALL_FEES
            write(device,AFORMAT) &
                b_tr//b_tdar// &
                       '<input align="right" name="amount_'//trim(itoa(idx))//'" size="6" value="'// &
                        trim(ftoa(FeeAmount(idx),1))//'">'// &
                    e_td//b_td_nbsp_e_td// &
                    b_td// &
                       '<input name="desc_'//trim(itoa(idx))//'" size="60" value="'//trim(FeeDescription(idx))//'">'// &
                    e_td// &
                e_tr
        end do

        write(device,AFORMAT) e_table, linebreak, &
            nbsp//'<input name="action" type="submit" value="Update">'//e_form//horizontal

    end subroutine display_fees


    subroutine school_fees(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_USERNAME) :: tAction
        character (len=MAX_LEN_PERSON_NAME) :: tDesc
        logical :: changes
        integer :: idx, ierr1, ierr2
        real :: tAmount

        call html_comment('school_fees()')

        ! check for requested action
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr1)

        if (ierr1/=0 .or. tAction==SPACE) then ! no action; display existing info

        else if (.not. isRoleOfficial) then ! action is Update


            ! collect changes to COLLEGES.XML
            changes = .false.
            do idx=1,MAX_ALL_FEES
                call cgi_get_named_float(QUERY_STRING, 'amount_'//trim(itoa(idx)), tAmount, ierr1)
                call cgi_get_named_string(QUERY_STRING, 'desc_'//trim(itoa(idx)), tDesc, ierr2)
                if ( tAmount==0.0 .or. len_trim(tDesc)==0 ) then
                    tDesc = SPACE
                    tAmount = 0.0
                end if
                if ( (FeeAmount(idx) /= tAmount) .or. (FeeDescription(idx) /= tDesc) ) then
                    FeeAmount(idx) = tAmount
                    FeeDescription(idx) = tDesc
                    changes = .true.
                end if
            end do

            if (changes) call university_data_write(unitXML, trim(pathToYear)//'UNIVERSITY.XML')

        end if

        if (len_trim(tAction)>0 .and. isRoleOfficial) then
            tDesc = 'Edit school fees failed. '//sorryMessageOfficial
        else
            tDesc = SPACE
        end if

        call display_fees(device, tDesc)


    end subroutine school_fees


    subroutine display_scholarships(device, mesg)
        integer, intent(in) :: device
        character(len=*), intent (in) :: mesg

        integer :: idx, jdx

        call html_comment('display_scholarships()')

        targetDepartment = DeptIdxUser
        targetCollege = CollegeIdxUser

        call html_write_header(device, 'Update scholarships info', mesg)

        do jdx=1,MAX_ALL_SCHOLARSHIPS,20

            call make_form_start(device, fnEditScholarships)

            write(device,AFORMAT) linebreak, '<table border="0" width="100%">', &
                b_tr//b_thar//'Code'//e_th//b_td_nbsp_e_td//b_thal//'Description'//e_th//e_tr

            do idx=jdx,min(jdx+19,MAX_ALL_SCHOLARSHIPS)
                write(device,AFORMAT) &
                    b_tr//b_tdar// &
                           '<input align="right" name="code_'//trim(itoa(idx))//'" size="6" value="'// &
                            trim(ScholarshipCode(idx))//'">'// &
                        e_td//b_td_nbsp_e_td// &
                        b_td// &
                           '<input name="desc_'//trim(itoa(idx))//'" size="60" value="'// &
                            trim(ScholarshipDescription(idx))//'">'// &
                        e_td// &
                    e_tr
            end do

            write(device,AFORMAT) e_table, linebreak, &
                nbsp//'<input name="action" type="submit" value="Update">'//e_form//linebreak

        end do
        write(device,AFORMAT) horizontal

    end subroutine display_scholarships


    subroutine scholarships_list(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_USERNAME) :: tAction, tCode
        character (len=MAX_LEN_COLLEGE_NAME) :: tDesc
        logical :: changes
        integer :: idx, ierr1, ierr2

        call html_comment('scholarships_list()')

        ! check for requested action
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr1)

        if (ierr1/=0 .or. tAction==SPACE) then ! no action; display existing info

        else if (.not. isRoleOfficial) then ! action is Update

            ! collect changes to COLLEGES.XML
            changes = .false.
            do idx=1,MAX_ALL_SCHOLARSHIPS
                call cgi_get_named_string(QUERY_STRING, 'code_'//trim(itoa(idx)), tCode, ierr1)
                call cgi_get_named_string(QUERY_STRING, 'desc_'//trim(itoa(idx)), tDesc, ierr2)
                if (ierr1/=0 .or. ierr2/=0) cycle

                if ( len_trim(tCode)==0 .or. len_trim(tDesc)==0 ) then
                    tDesc = SPACE
                    tCode = SPACE
                end if
                if ( (ScholarshipCode(idx) /= tCode) .or. (ScholarshipDescription(idx) /= tDesc) ) then
                    ScholarshipCode(idx) = tCode
                    ScholarshipDescription(idx) = tDesc
                    changes = .true.
                end if
            end do

            if (changes) call university_data_write(unitXML, trim(pathToYear)//'UNIVERSITY.XML')

        end if

        if (len_trim(tAction)>0 .and. isRoleOfficial) then
            tDesc = 'Edit scholarships list failed. '//sorryMessageOfficial
        else
            tDesc = SPACE
        end if

        call display_scholarships(device, tDesc)

    end subroutine scholarships_list


end module EditUNIVERSITY
