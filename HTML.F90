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


module HTML

    use XMLIO

    implicit none

    ! index to server functions
    integer, parameter ::  &
        fnLogin                   =  1, & ! login user
        fnGenerateTeacherPassword =  2, & ! generate new password for teacher
        fnChangeTeacherPassword   =  3, & ! change password for teacher
        fnLogout                  =  6, & ! logout user
        fnSuspendProgram          =  7, & ! suspend the program
        fnToggleTrainingMode      =  8, & ! toggle training mode
        fnStop                    =  9, & ! terminate program
        fnEditSignatories         = 10, & ! edit signatories
        fnRecentTeacherActivity   = 12, & ! recent teacher activity
        fnOnlineTeachers          = 14, & ! online teachers
        fnFindTeacher             = 16, & ! find a teacher
        fnDeleteTeacher           = 17, & ! delete teacher
        fnResetPasswords          = 18, & ! reset all passwords except for ADMINS, SCHEDULERS
        fnDownloadXML             = 19, & ! download XML data file
        !
        fnCollegeLinks            = 20, & ! index to unit info
        fnSubjectList             = 21, & ! view list of subjects administered by a department
        fnEditSubject             = 22, & ! edit subject
        fnCurriculumList          = 23, & ! view list of curricular programs administered by a unit
        fnCurriculum              = 24, & ! view a curricular program
        fnEditCurriculum          = 25, & ! edit curriculum
        fnActivateCurriculum      = 26, & ! activate curriculum
        fnDeactivateCurriculum    = 27, & ! deactivate curriculum
        fnRoomList                = 28, & ! view list rooms administered by a unit
        fnTeachersByDept          = 29, & ! view list teachers belonging to a unit
        fnTeachersByName          = 30, & ! view alphabetical list of teachers in a unit
        fnEditRoom                = 31, & ! edit room parameters
        fnEditTeacher             = 32, & ! edit teacher record
        !
        fnScheduleOfClasses       = 50, & ! display schedule of classes for editing
        fnScheduleOfferSubject    = 51, & ! offer a subject
        fnScheduleAddLab          = 52, & ! add a lab section
        fnScheduleDelete          = 53, & ! delete a section
        fnScheduleEdit            = 54, & ! edit a section
        fnScheduleValidate        = 55, & ! check correctness of edit inputs
        fnScheduleByArea          = 56, & ! display schedule of classes for editing, by area
        !
        fnTeacherClasses          = 60, & ! view classes of a teacher
        fnTeacherEditSchedule     = 61, & ! edit weekly schedule of a teacher
        fnTeacherConflicts        = 62, & ! view list of teachers with conflicts
        fnTBATeachers             = 63, & ! view list of sections with TBA teachers
        fnPrintableWorkload       = 64, & ! printable teaching load
        fnRoomSchedule            = 65, & ! view weekly schedule of a room
        fnRoomConflicts           = 66, & ! view list of rooms with conflicts
        fnTBARooms                = 67, & ! view list of sections with TBA rooms
        !
        fnBlockSchedule           = 70, & ! display schedule of block section
        fnBlockEditName           = 71, & ! edit name of block section
        fnBlockDeleteNotClasses   = 72, & ! delete block, but keep its sections
        fnBlockDeleteAlsoClasses  = 73, & ! delete block and its sections
        fnBlockNewSelect          = 74, & ! select parameters for a new block
        fnBlockNewAdd             = 75, & ! add a new block
        fnBlockCopy               = 76, & ! copy block
        fnBlockEditSection        = 77, & ! edit section in block
        fnBlockEditSubject        = 78, & ! update subjects in block
        fnBlockList               = 79, & ! list blocks
        fnBlockConflicts          = 80    ! conflicts in block schedules

    ! the requested server function, index to requesting, user
    integer :: REQUEST, requestingTeacher, requestingStudent

    ! the target of the function
    integer :: targetCollege, targetDepartment, targetSubject, targetRoom, targetTeacher, &
        targetCurriculum, targetStudent, targetBlock, targetSection

    ! work arrays
    integer :: tArray(2*MAX_ALL_SUBJECTS)
    character (len=80) :: QUERY_put, termDescription


contains

    function fnDescription (fn)
        character(len=60) :: fnDescription
        integer, intent (in) :: fn

        select case (fn)

            case (fnStop)
                    fnDescription = 'stop program'

            case (fnLogin)
                    fnDescription = 'login'

            case (fnLogout )
                    fnDescription = 'logout'

            case (fnGenerateTeacherPassword )
                    fnDescription = 'generate new password for teacher'

            case (fnChangeTeacherPassword)
                    fnDescription = 'change password for Teacher'

            case (fnResetPasswords)
                    fnDescription = 'reset all passwords except for ADMINS, SCHEDULERS'

            case (fnToggleTrainingMode)
                    fnDescription = 'toggle training mode'

            case (fnEditSignatories)
                    fnDescription = 'edit signatories'

            case (fnRecentTeacherActivity)
                    fnDescription = 'recent teacher activity'

            case (fnOnlineTeachers)
                    fnDescription = 'list online teachers'

            case (fnFindTeacher)
                    fnDescription = 'find a teacher'

            case (fnDeleteTeacher)
                    fnDescription = 'delete teacher'

            case (fnCollegeLinks )
                    fnDescription = 'index to unit info'

            case (fnSubjectList)
                    fnDescription = 'view list of subjects administered by a department'

            case (fnEditSubject)
                    fnDescription = 'edit subject'

            case (fnCurriculumList)
                    fnDescription = 'view list of curricular programs administered by a unit'

            case (fnCurriculum )
                    fnDescription = 'view a curricular program'

            case (fnEditCurriculum)
                    fnDescription = 'edit curriculum'

            case (fnActivateCurriculum)
                    fnDescription = 'activate curriculum'

            case (fnDeactivateCurriculum)
                    fnDescription = 'deactivate curriculum'

            case (fnEditRoom )
                    fnDescription = 'edit room parameters'

            case (fnEditTeacher)
                    fnDescription = 'edit teacher record'

            case (fnScheduleOfClasses )
                    fnDescription = 'display schedule of classes for editing'

            case (fnScheduleOfferSubject)
                    fnDescription = 'offer a subject'

            case (fnScheduleAddLab)
                    fnDescription = 'add a lab section'

            case (fnScheduleDelete)
                    fnDescription = 'delete a section'

            case (fnScheduleEdit )
                    fnDescription = 'edit a section'

            case (fnScheduleValidate)
                    fnDescription = 'check correctness of edit inputs'

            case (fnTeachersByDept)
                    fnDescription = 'view list teachers belonging to a unit'

            case (fnTeachersByName)
                    fnDescription = 'view alphabetical list of teachers in a unit'

            case (fnTeacherClasses )
                    fnDescription = 'view classes of a teacher'

            case (fnTeacherEditSchedule )
                    fnDescription = 'edit weekly schedule of a teacher'

            case (fnRoomList )
                    fnDescription = 'view list rooms administered by a unit'

            case (fnRoomSchedule )
                    fnDescription = 'view weekly schedule of a room'

            case (fnRoomConflicts)
                    fnDescription = 'view list of rooms with conflicts'

            case (fnTeacherConflicts)
                    fnDescription = 'view list of teachers with conflicts'

            case (fnTBARooms )
                    fnDescription = 'view list of sections with TBA rooms'

            case (fnTBATeachers)
                    fnDescription = 'view list of sections with TBA teachers'

            case (fnBlockSchedule)
                    fnDescription = 'display schedule of block section'

            case (fnBlockEditName)
                    fnDescription = 'edit name of block section'

            case (fnBlockDeleteNotClasses )
                    fnDescription = 'delete block, but keep its sections'

            case (fnBlockDeleteAlsoClasses)
                    fnDescription = 'delete block and its sections'

            case (fnBlockNewSelect)
                    fnDescription = 'select parameters for a new block'

            case (fnBlockNewAdd)
                    fnDescription = 'add a new block'

            case (fnBlockCopy)
                    fnDescription = 'copy block'

            case (fnBlockList)
                    fnDescription = 'list blocks'

            case (fnBlockConflicts)
                    fnDescription = 'conflicts in block schedules'

            case (fnBlockEditSection)
                    fnDescription = 'edit section in block'

            case (fnBlockEditSubject)
                    fnDescription = 'update subjects in block'

            case (fnScheduleByArea)
                    fnDescription = 'display schedule of classes for editing, by area'

            case (fnPrintableWorkload )
                    fnDescription = 'printable teaching load'

            case (fnSuspendProgram)
                    fnDescription = 'toggle suspend mode'

            case (fnDownloadXML)
                    fnDescription = 'download XML data file'

        end select

    end function fnDescription


    function fnAvailable (fn)
        logical :: fnAvailable
        integer, intent (in) :: fn

        select case (fn)

            case (fnStop)
                    fnAvailable = .true.

            case (fnLogin)
                    fnAvailable = .true.

            case (fnLogout )
                    fnAvailable = .true.

            case (fnGenerateTeacherPassword )
                    fnAvailable = .true.

            case (fnChangeTeacherPassword)
                    fnAvailable = .true.

            case (fnResetPasswords)
                    fnAvailable = .true.

            case (fnToggleTrainingMode)
                    fnAvailable = .true.

            case (fnEditSignatories)
                    fnAvailable = .true.

            case (fnRecentTeacherActivity)
                    fnAvailable = .true.

            case (fnOnlineTeachers)
                    fnAvailable = .true.

            case (fnFindTeacher)
                    fnAvailable = .true.

            case (fnDeleteTeacher)
                    fnAvailable = .true.

            case (fnCollegeLinks )
                    fnAvailable = .true.

            case (fnSubjectList)
                    fnAvailable = .true.

            case (fnEditSubject)
                    fnAvailable = .true.

            case (fnCurriculumList)
                    fnAvailable = .true.

            case (fnCurriculum )
                    fnAvailable = .true.

            case (fnEditCurriculum)
                    fnAvailable = .true.

            case (fnActivateCurriculum)
                    fnAvailable = .true.

            case (fnDeactivateCurriculum)
                    fnAvailable = .true.

            case (fnEditRoom )
                    fnAvailable = .true.

            case (fnEditTeacher)
                    fnAvailable = .true.

            case (fnScheduleOfClasses )
                    fnAvailable = .true.

            case (fnScheduleOfferSubject)
                    fnAvailable = .true.

            case (fnScheduleAddLab)
                    fnAvailable = .true.

            case (fnScheduleDelete)
                    fnAvailable = .true.

            case (fnScheduleEdit )
                    fnAvailable = .true.

            case (fnScheduleValidate)
                    fnAvailable = .true.

            case (fnTeachersByDept)
                    fnAvailable = .true.

            case (fnTeachersByName)
                    fnAvailable = .true.

            case (fnTeacherClasses )
                    fnAvailable = .true.

            case (fnTeacherEditSchedule )
                    fnAvailable = .true.

            case (fnRoomList )
                    fnAvailable = .true.

            case (fnRoomSchedule )
                    fnAvailable = .true.

            case (fnRoomConflicts)
                    fnAvailable = .true.

            case (fnTeacherConflicts)
                    fnAvailable = .true.

            case (fnTBARooms )
                    fnAvailable = .true.

            case (fnTBATeachers)
                    fnAvailable = .true.

            case (fnBlockSchedule)
                    fnAvailable = .true.

            case (fnBlockEditName)
                    fnAvailable = .true.

            case (fnBlockDeleteNotClasses )
                    fnAvailable = .true.

            case (fnBlockDeleteAlsoClasses)
                    fnAvailable = .true.

            case (fnBlockNewSelect)
                    fnAvailable = .true.

            case (fnBlockNewAdd)
                    fnAvailable = .true.

            case (fnBlockCopy)
                    fnAvailable = .true.

            case (fnBlockList)
                    fnAvailable = .true.

            case (fnBlockConflicts)
                    fnAvailable = .true.

            case (fnBlockEditSection)
                    fnAvailable = .true.

            case (fnBlockEditSubject)
                    fnAvailable = .true.

            case (fnScheduleByArea)
                    fnAvailable = .true.

            case (fnPrintableWorkload )
                    fnAvailable = .true.

            case (fnSuspendProgram)
                    fnAvailable = .true.

            case (fnDownloadXML)
                    fnAvailable = .true.

        end select

    end function fnAvailable



    function calculate_check_sum(cipher)
        character(len=*), intent (in) :: cipher
        integer :: calculate_check_sum
        integer :: checkSum, kStart, lenCipher

        lenCipher = len_trim(cipher)
        checkSum = 0
        do kStart=1,lenCipher
            checkSum = checkSum + kStart*ichar(cipher(kStart:kStart))
        end do
        calculate_check_sum = checkSum

    end function calculate_check_sum


    function make_href(fn, label, pre, post, A1, A2, A3, A4, A5, anchor, newtab, alt)
    ! build HTML href, like:
    !   pre<a href="CGI_PATH?F=fn&A1=A1&...A5=A5 #anchor target=newtab">label</a>post

        integer, intent (in) :: fn
        character(len=*), intent (in) :: label
        character(len=*), intent (in), optional :: A1, A2, A3, A4, A5
        character(len=*), intent (in), optional :: pre, post, anchor, newtab, alt

        character(len=MAX_LEN_QUERY_STRING) :: make_href
        character(len=MAX_CGI_WRK_LEN) :: cgi_wrk
        integer :: kStart, checkSum
        real :: harvest ! random number

        if (.not. fnAvailable(fn)) then
            if (present(alt)) then
                make_href = alt
            else
                make_href = pre//trim(label)//SPACE//post
            end if
            return
        end if

        ! the function and user name
        cipher = 'F='//trim(itoa(fn))//'&N='//USERNAME
        ! force term to be current term ?
        if (targetTerm>0) then ! specified
            cipher = trim(cipher)//'&A9='//itoa(targetTerm)
        end if

        ! the arguments to the function
        if (present(A1)) then
            call cgi_url_encode(A1,cgi_wrk)
            cipher = trim(cipher)//'&A1='//cgi_wrk
        end if
        if (present(A2)) then
            call cgi_url_encode(A2,cgi_wrk)
            cipher = trim(cipher)//'&A2='//cgi_wrk
        end if
        if (present(A3)) then
            call cgi_url_encode(A3,cgi_wrk)
            cipher = trim(cipher)//'&A3='//cgi_wrk
        end if
        if (present(A4)) then
            call cgi_url_encode(A4,cgi_wrk)
            cipher = trim(cipher)//'&A4='//cgi_wrk
        end if
        if (present(A5)) then
            call cgi_url_encode(A5,cgi_wrk)
            cipher = trim(cipher)//'&A5='//cgi_wrk
        end if

        ! calculate checksum
        checkSum = calculate_check_sum(cipher)
        ! encrypt
        call random_number(harvest)
        kStart = MAX_LEN_QUERY_STRING/2 + int(harvest*MAX_LEN_QUERY_STRING/2)
        call encrypt(queryEncryptionKey(:kStart), cipher)
        cipher = trim(cipher)//itoa(kStart)
        cipher = '<a href="'//trim(CGI_PATH)//'?s='//trim(itoa(checkSum))//'&q='//trim(cipher)

        ! preamble (text before href)
        if (present(pre)) cipher = pre//cipher

        ! the anchor
        if (present(anchor)) cipher = trim(cipher)//'#'//anchor

        ! the target
        if (present(newtab)) cipher = trim(cipher)//' target='//newtab

        ! end href & the label
        cipher = trim(cipher)//'">'//trim(label)//'</a>'

        ! the text after the href
        if (present(post)) cipher = trim(cipher)//post

        make_href = cipher

    end function make_href



    subroutine make_form_start(device, fn, A1, A2, A3, A4, A5)

        integer, intent (in) :: device, fn
        character(len=*), intent (in), optional :: A1, A2, A3, A4, A5

        character(len=MAX_CGI_WRK_LEN) :: cgi_wrk
        integer :: kStart, checkSum
        real :: harvest ! random number

        ! the function and user name
        cipher = 'F='//trim(itoa(fn))//'&N='//USERNAME
        ! force term to be current term ?
        if (targetTerm>0) then ! specified
            cipher = trim(cipher)//'&A9='//itoa(targetTerm)
        end if

        ! the arguments to the function
        if (present(A1)) then
            call cgi_url_encode(A1,cgi_wrk)
            cipher = trim(cipher)//'&A1='//cgi_wrk
        end if
        if (present(A2)) then
            call cgi_url_encode(A2,cgi_wrk)
            cipher = trim(cipher)//'&A2='//cgi_wrk
        end if
        if (present(A3)) then
            call cgi_url_encode(A3,cgi_wrk)
            cipher = trim(cipher)//'&A3='//cgi_wrk
        end if
        if (present(A4)) then
            call cgi_url_encode(A4,cgi_wrk)
            cipher = trim(cipher)//'&A4='//cgi_wrk
        end if
        if (present(A5)) then
            call cgi_url_encode(A5,cgi_wrk)
            cipher = trim(cipher)//'&A5='//cgi_wrk
        end if

        ! calculate checksum
        checkSum = calculate_check_sum(cipher)
        ! encrypt
        call random_number(harvest)
        kStart = MAX_LEN_QUERY_STRING/2 + int(harvest*MAX_LEN_QUERY_STRING/2)
        call encrypt(queryEncryptionKey(:kStart), cipher)
        cipher = trim(cipher)//itoa(kStart)
        write(device,AFORMAT) &
          '<form name="input" method="post" action="'//trim(CGI_PATH)//'">', &
          '<input type="hidden" name="s" value="'//trim(itoa(checkSum))//'">', &
          '<input type="hidden" name="q" value="'//trim(cipher)//'">'

    end subroutine make_form_start


    subroutine html_copyright(device)
        integer, intent(in) :: device

        write(device,AFORMAT) &
            '<small><i>'//PROGNAME//nbsp//COPYRIGHT//'<br>', &
            'This program comes with ABSOLUTELY NO WARRANTY; for details see the GNU GENERAL PUBLIC LICENSE Version 3 ', &
            '(<a target="0" href="http://www.gnu.org/licenses/gpl-3.0.html">GPLv3</a>).<br>', &
            'This program is free software, and you are welcome to redistribute it under certain conditions; ', &
            ' see the GPLv3 for details.<br>', &
            ' Support for the development of this program was provided by: '// &
            ' University of the Philippines Los Banos (1997-2001); '// &
            ' BalikScientist Program of the Department of Science and Technology (2010); '// &
            ' Isabela State University (2011); and '// &
            ' Cagayan State University (2012). ', &
            'The source code is available at <a target="0" href="'//WEB//'">'//WEB//'</a><br>', &
            CONTACT//'</i></small><hr>'


    end subroutine html_copyright


    subroutine html_landing_page(device, mesg)
        integer, intent(in) :: device
        character(len=*), intent(in) :: mesg
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        character (len=MAX_LEN_PASSWD_VAR) :: tPassword

        call html_comment('html_landing_page()')
        tTeacher = GUEST
        tPassword = GUEST

        write(device,AFORMAT) &
            '<html><head><title>'//trim(UniversityCode)//SPACE//PROGNAME//'</title></head><body>'

        if (len_trim(mesg)>0) then ! Stop/Logout page
            write(device,aformat) '<h2>'//trim(UniversityCode)//SPACE//PROGNAME//'</h2>', &
                red//trim(mesg)//black
            if (REQUEST==fnStop)  &
                write(device,AFORMAT)'<br><br>Go to <a href="/">'//PROGNAME//' Index</a>'
        else ! Login page
            write(device,AFORMAT) &
                '<h2>Welcome to '//trim(UniversityCode)//SPACE//PROGNAME//SPACE//trim(ACTION)//' !</h2><hr>', &
                '<table border="0" width="100%">'//begintr, &
                '<td valign="top" width="25%">'
            call make_form_start(device, fnLogin)
            write(device,AFORMAT) &
                '<b>Username</b>:<br>', &
                '<input size="20" type="text" name="U" value="'//trim(tTeacher)//'"><br><br>', &
                '<b>Password:</b><br>', &
                '<input size="20" type="password" name="P" value="'//trim(tPassword)//'"><br><br>', &
                '<input type="submit" value="Login">', &
                nbsp//'<i><small>(or, go back to <a href="/">'//PROGNAME//' Index</a>)</small></i></form>'
            if (len_trim(loginCheckMessage)>0) write(device,AFORMAT) &
                '<br>'//red//trim(loginCheckMessage)//black//'<br>'
            write(device,AFORMAT) endtd//begintd, &
                '<b>Forgot your Username and/or Password?</b> <i>Visit your Dean or the Registrar; '// &
                'bring University-issued ID.</i><br>', &
                '<br><b>Tips:</b><ul>', &
                '<li>Change your initial password. Do not forget to logout.</li>', &
                '<li>Do not use the browser ''Back'' button unless '// &
                ' there are no hyperlinks or ''Submit'' buttons on the displayed page.</li>', &
                '<li>HEEDS is best viewed using the Mozilla Firefox browser.</li>', &
                endtd//endtr//'</table><hr>'
            call html_copyright(device)

        end if

    end subroutine html_landing_page


    subroutine html_login(fname, mesg)
        character(len=*), intent(in) :: fname, mesg
        integer :: device=7

        call html_comment('html_login()')

        open(unit=device, file=fname, form='formatted', status='unknown')
        call html_landing_page(device, mesg)
        write(device,AFORMAT) '</body></html>'
        close(device)


    end subroutine html_login


    subroutine timetable_display(device, Section, TimeTable)
        integer, intent(in) :: device, TimeTable(60,6)
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, parameter :: period = 2 ! no. of 15 minute intervals
        integer :: i, color, ncol, sect, j, mcol, minTime, maxTime
        character (len=1024) :: line
        integer :: colorIdx(60,6)

        call html_comment('timetable_display()')

        minTime = 56
        maxTime = 1
        ! background colors
        colorIdx = 0
        color = 0
        do ncol=1,56,period
            do i=6,1,-1
                if (TimeTable(ncol,i)<=0) cycle ! no class at this time
                if (ncol<minTime) minTime = ncol
                if (ncol>maxTime) maxTime = ncol
                if (colorIdx(ncol,i)/=0) cycle ! already has a color
                ! section not colored yet
                sect = TimeTable(ncol,i)
                color = color + 1
                do mcol=ncol,56,period
                    do j=6,1,-1
                        if (sect==TimeTable(mcol,j)) colorIdx(mcol,j) = mod(color,15)
                    end do
                end do
            end do
        end do

        write(device,AFORMAT) '<br><b>Weekly Timetable</b>'// &
            '<table border="1" width="100%"><small>'
        write(device,AFORMAT) begintr//beginth//'Time'//endth, &
            thaligncenter//'Monday'//endth//thaligncenter//'Tuesday'//endth//thaligncenter//'Wednesday'//endth//&
            thaligncenter//'Thursday'//endth//thaligncenter//'Friday'//endth//thaligncenter//'Saturday'//endth//&
            endtr
        do ncol=minTime,maxTime,period ! 1,56,period
            line = SPACE
            do i=6,1,-1
                sect = TimeTable(ncol,i)
                if (sect==0) then
                    line = tdnbspendtd//line
                elseif (sect<0) then
                    line = tdaligncenter//green//'<b><i>proposed</i></b>'//black//endtd//line
                else
                    line = '<td align="center" bgcolor="'//bgcolor(colorIdx(ncol,i))//'">'// &
                        trim(Section(sect)%ClassId)//endtd//line
                end if
            end do
            line = '<td width="100">'//trim(text_time_period(ncol,ncol+period))//endtd//line
            write (device,AFORMAT) begintr//trim(line)//endtr
        end do
        write(device,AFORMAT) '</small></table>'

    end subroutine timetable_display


    subroutine list_sections_to_edit(device, Section, lenSL, SectionList, &
            target_fn, target_name, target_action, permitted, heading)
        integer, intent(in) :: device, lenSL, SectionList(3*lenSL+3), target_fn
        type (TYPE_SECTION), intent(in) :: Section(0:)
        character(len=*), intent(in) :: target_name, target_action
        logical, intent(in) :: permitted
        character (len=*), intent(in), optional :: heading
        integer :: crse, idx, mdx, rdx, sdx, tdx, previous, conflict, dept
        character (len=10) :: note
        logical :: countUnits, sectionDone
        real :: totalUnits, meetingUnits, totalHours, meetingHours

        call html_comment('list_sections_to_edit()')

        if (present(heading)) write(device,AFORMAT) heading
        if (lenSL < 3) then
            write(device,AFORMAT) '<br>(None)<br>'
            return
        end if

        countUnits = target_action=='Del' .and. target_fn/=fnRoomSchedule
        totalUnits = 0.0
        totalHours = 0.0

        if (countUnits) then
            write(device,AFORMAT) '<table border="0" width="100%">'//begintr, &
                thalignleft//'Subject'//endth//thalignleft//'Section'//endth//&
                thalignleft//'Units'//endth//thalignleft//'Hours'//endth//thalignleft//'Day'//endth//&
                thalignleft//'Time'//endth//thalignleft//'Room'//endth//&
                thalignleft//'Teacher'//endth
        else
            write(device,AFORMAT) '<table border="0" width="100%">'//begintr, &
                thalignleft//'Subject'//endth//thalignleft//'Section'//endth//&
                tdnbspendtd// & ! thalignleft//'Block'//endth// &
                thalignleft//'Seats'//endth//thalignleft//'Day'//endth//&
                thalignleft//'Time'//endth//thalignleft//'Room'//endth//&
                thalignleft//'Teacher'//endth
        end if
        if (target_fn>0) then
            write(device,AFORMAT) thalignleft//'<small>Action</small>'//endth//endtr
        else
            write(device,AFORMAT) beginth//nbsp//endth//endtr
        end if

        previous = 0
        sectionDone = .false.
        do idx=1,lenSL,3
            sdx=SectionList(idx)
            mdx=SectionList(idx+1)
            conflict=SectionList(idx+2)
            crse = Section(sdx)%SubjectIdx
            dept = Section(sdx)%DeptIdx
            QUERY_put = Section(sdx)%ClassId

            if (conflict>=0) then
                note = SPACE
            else ! negative conflict is the undesirablity index
                note = itoa(-conflict)
            end if
            !new section ?
            if (sdx/=previous) then ! include subject, section, units/blockname, seats/hours, time, day

                sectionDone = .false.
                if (is_lecture_lab_subject(crse)) then
                    if (is_lecture_class(sdx, Section)) then ! lecture of lecture-lab
                        meetingUnits = Subject(crse)%Units
                        meetingHours = Subject(crse)%LectHours
                    else ! lab of lecture-lab
                        meetingUnits = 0.0
                        meetingHours = Subject(crse)%LabHours
                    end if
                else if (Subject(crse)%LectHours>0.0) then ! lecture-only
                    meetingUnits = Subject(crse)%Units
                    meetingHours = Subject(crse)%LectHours
                else if (Subject(crse)%LabHours>0.0) then ! lab-only
                    meetingUnits = Subject(crse)%Units
                    meetingHours = Subject(crse)%LabHours
                end if

                totalHours = totalHours + meetingHours
                totalUnits = totalUnits + meetingUnits


                previous = sdx
                if (permitted .and. (REQUEST==fnBlockEditSection .or. &
                        REQUEST==fnBlockEditSubject .or. &
                        REQUEST==fnBlockEditName .or. &
                        REQUEST==fnBlockCopy .or. &
                        REQUEST==fnBlockSchedule)) then ! link section code to edit section
                    write(device,AFORMAT) &
                        trim(make_href(fnScheduleOfClasses, Subject(crse)%Name, &
                            A1=Department(dept)%Code, pre=begintr//begintd, post=endtd, anchor=Subject(crse)%Name)), &
                        trim(make_href(fnScheduleEdit, Section(sdx)%Code, &
                            A1=QUERY_put, pre=begintd, post=endtd))
                else
                    write(device,AFORMAT) &
                        begintr//begintd//trim(Subject(crse)%Name)//endtd, & !  subject
                        begintd//trim(Section(sdx)%Code)//endtd ! code
                end if
                if (countUnits) then
                    if (meetingUnits>0.0) then
                        write(device,AFORMAT) begintd//trim(ftoa(meetingUnits,1))//endtd
                    else
                        write(device,AFORMAT) tdnbspendtd ! units
                    end if
                    if (meetingHours>0.0) then
                        write(device,AFORMAT) begintd//trim(ftoa(meetingHours,2))//endtd ! hours
                    else
                        write(device,AFORMAT) tdnbspendtd ! hours
                    end if
                    !write(device,AFORMAT)  &
                    !    begintd//txtDay(Section(sdx)%DayIdx(mdx))//endtd// &
                    !    begintd//trim(text_time_period(Section(sdx)%bTimeIdx(mdx), Section(sdx)%eTimeIdx(mdx)))//endtd
                else
                    write(device,AFORMAT)  &
!                        begintd//trim(Section(sdx)%BlockID)//endtd// & ! BlockID
                        tdnbspendtd// & ! BlockID
                        begintd//trim(itoa(Section(sdx)%Slots))//endtd!// & ! seats
                    !    begintd//txtDay(Section(sdx)%DayIdx(mdx))//endtd// &
                    !    begintd//trim(text_time_period(Section(sdx)%bTimeIdx(mdx), Section(sdx)%eTimeIdx(mdx)))//endtd
                end if


                if (is_regular_schedule(sdx, Section)) then
                    sectionDone = .true.

                    ! time, day 
                    write(device,AFORMAT) begintd//text_days_of_section(sdx, 0, Section)//endtd// &
                        begintd//trim(text_time_period(Section(sdx)%bTimeIdx(mdx), Section(sdx)%eTimeIdx(mdx)))//endtd

                    ! room
                    rdx = Section(sdx)%RoomIdx(mdx)
                    if (rdx > 0) then
                        write(device,AFORMAT) begintd//trim(Room(rdx)%Code)//endtd
                    else
                        write(device,AFORMAT) begintd//'TBA'//endtd
                    end if

                    ! teacher
                    tdx = Section(sdx)%TeacherIdx(mdx)
                    if (tdx > 0) then
                        write(device,AFORMAT) begintd//trim(Teacher(tdx)%Name)//endtd
                    else
                        write(device,AFORMAT) begintd//'TBA'//endtd
                    end if

                    if (target_fn==fnRoomSchedule .or. target_fn==fnTeacherEditSchedule) then
                        if ( permitted ) then
                            write(device,AFORMAT) trim(make_href(target_fn, target_action, &
                                A1=target_name, A2=target_action, A3=QUERY_put, &
                                pre=begintd//'<small>', post='</small>'//endtd//endtr))
                        else
                            write(device,AFORMAT) tdnbspendtd//endtr
                        end if
                    elseif (target_fn==fnTeacherClasses) then
                        if ( permitted .and. tdx>0) then
                            write(device,AFORMAT) trim(make_href(target_fn, target_action, &
                                A1=Teacher(tdx)%TeacherID, &
                                pre=begintd//'<small>', post='</small>'//endtd//endtr))
                        else
                            write(device,AFORMAT) tdnbspendtd//endtr
                        end if
                    elseif (target_fn==fnBlockEditSection) then
                        if ( permitted ) then
                            ! operate on lab classes, not lecture classes
                            if (is_lecture_lab_subject(Section(sdx)%SubjectIdx) .and.  is_lecture_class(sdx, Section)) then
                                write(device,AFORMAT) tdnbspendtd//endtr
                            else
                                write(device,AFORMAT) trim(make_href(target_fn, target_action, &
                                    A1=target_name, A2=target_action, A3=QUERY_put, &
                                    pre=begintd//'<small>', post='</small>'//trim(note)//endtd//endtr))
                            end if
                        else
                            write(device,AFORMAT) tdnbspendtd//endtr
                        end if
                    end if
                    if (conflict>0) write(device,AFORMAT) &
                        begintr//'<td align="center" colspan="8">'//red//'CONFLICT between '//trim(Section(sdx)%ClassId)// &
                        ' and '//trim(Section(conflict)%ClassId)//black//endtd//endtr

                    cycle

                else
                    write(device,AFORMAT)  &
                        begintd//txtDay(Section(sdx)%DayIdx(mdx))//endtd// &
                        begintd//trim(text_time_period(Section(sdx)%bTimeIdx(mdx), Section(sdx)%eTimeIdx(mdx)))//endtd
                end if

            else ! time, day only

                if (sectionDone) then ! conflict ?
                    if (conflict>0) write(device,AFORMAT) &
                        begintr//'<td align="center" colspan="8">'//red//'CONFLICT between '//trim(Section(sdx)%ClassId)// &
                        ' and '//trim(Section(conflict)%ClassId)//black//endtd//endtr
                    cycle
                end if

                write(device,AFORMAT) &
                    begintd//txtDay(Section(sdx)%DayIdx(mdx))//endtd// &
                    begintd//trim(text_time_period(Section(sdx)%bTimeIdx(mdx), Section(sdx)%eTimeIdx(mdx)))//endtd
            end if

            ! room
            rdx = Section(sdx)%RoomIdx(mdx)
            if (rdx > 0) then
                write(device,AFORMAT) begintd//trim(Room(rdx)%Code)//endtd
            else
                write(device,AFORMAT) begintd//'TBA'//endtd
            end if

            ! teacher
            tdx = Section(sdx)%TeacherIdx(mdx)
            if (tdx > 0) then
                write(device,AFORMAT) begintd//trim(Teacher(tdx)%Name)//endtd
            else
                write(device,AFORMAT) begintd//'TBA'//endtd
            end if

            if (sdx==SectionList(idx+3)) then ! NOT the last meeting; add SPACE for next line
                write(device,AFORMAT) begintd//endtd//endtr
                if (conflict>0) write(device,AFORMAT) &
                    begintr//'<td align="center" colspan="8">'//red//'CONFLICT between '//trim(Section(sdx)%ClassId)//' and '// &
                    trim(Section(conflict)%ClassId)//black//endtd//endtr
                write(device,AFORMAT) begintr//begintd//endtd//begintd//endtd//begintd//endtd//begintd//endtd
            else
                if (target_fn==fnRoomSchedule .or. target_fn==fnTeacherEditSchedule) then
                    if ( permitted ) then
                        write(device,AFORMAT) trim(make_href(target_fn, target_action, &
                            A1=target_name, A2=target_action, A3=QUERY_put, &
                            pre=begintd//'<small>', post='</small>'//endtd//endtr))
                    else
                        write(device,AFORMAT) tdnbspendtd//endtr
                    end if
                elseif (target_fn==fnBlockEditSection) then
                    if ( permitted ) then
                        ! operate on lab classes, not lecture classes
                        if (is_lecture_lab_subject(Section(sdx)%SubjectIdx) .and.  is_lecture_class(sdx, Section)) then
                            write(device,AFORMAT) tdnbspendtd//endtr
                        else
                            write(device,AFORMAT) trim(make_href(target_fn, target_action, &
                                A1=target_name, A2=target_action, A3=QUERY_put, &
                                pre=begintd//'<small>', post='</small>'//trim(note)//endtd//endtr))
                        end if
                    else
                        write(device,AFORMAT) tdnbspendtd//endtr
                    end if
                end if
                if (conflict>0) write(device,AFORMAT) &
                    begintr//'<td align="center" colspan="8">'//red//'CONFLICT between '//trim(Section(sdx)%ClassId)//' and '// &
                    trim(Section(conflict)%ClassId)//black//endtd//endtr
            end if
        end do
        if (countUnits) then
            write(device,AFORMAT) begintr//'<td colspan="9"><hr>'//endtd//endtr, &
                begintr//tdnbspendtd//begintd//'<b>Totals</b> : '//endtd// & ! code
                begintd//trim(ftoa(totalUnits,2))//endtd//begintd//trim(ftoa(totalHours,2))//endtd// & ! hours
                tdnbspendtd// tdnbspendtd// tdnbspendtd// tdnbspendtd// tdnbspendtd//endtr, &
                begintr//'<td colspan="9"><hr>'//endtd//endtr
        end if
        write(device,AFORMAT) '</table>'


    end subroutine list_sections_to_edit



    subroutine html_write_header(device, header, errmsg)
        integer, intent (in) :: device
        character(len=*), intent (in) :: header
        character(len=*), intent (in), optional :: errmsg
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer :: gdx
        character (len=80) :: description

        call html_comment('html_write_header('//trim(header)//')')
        ! override text for TERM+YEAR ?

        if (termBegin==termEnd) then
            description = termDescription
            termDescription = SPACE
        else
            description = ' for '//text_school_year(currentYear)
            if (REQUEST<fnScheduleOfClasses) termDescription = SPACE
        end if

        ! page title, start of body
        write(device,AFORMAT) &
            '<html><head><title>'//trim(UniversityCode)//SPACE//PROGNAME//VERSION// &
            '</title></head><body><a name="TOP"></a>'

        ! banner line 1: user context & shortcuts
        write(device,AFORMAT) &
            '<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
            begintr//'<td width="50%"><h2>'//trim(UniversityCode)//SPACE//trim(PROGNAME)// &
            trim(description)//'</h2>'
        if (present(errmsg)) then
            if (errmsg/=SPACE) write(device,AFORMAT) red//'<i>'//trim(errmsg)//'</i>'//black
        end if

        write(device,AFORMAT) endtd//'<td align="right" valign="top"><small>'// &
            '[ <b>User is '//trim(USERNAME)//'</b>.'
        QUERY_put = USERNAME

        if ( trim(USERNAME)/=trim(GUEST) .and. ROLE/=ADMINISTRATION) then
            if (REQUEST/=fnTeacherClasses) then
                if (targetTerm==0) then
                    targetTerm = currentTerm
                        write(device,AFORMAT) &
                            trim(make_href(fnTeacherClasses, 'Classes', A1=QUERY_put, pre=nbsp, &
                            post=DASH//txtSemester(targetTerm+6)))
                    targetTerm = 0
                else
                    write(device,AFORMAT) &
                        trim(make_href(fnTeacherClasses, 'Classes', A1=QUERY_put, pre=nbsp, &
                        post=DASH//txtSemester(targetTerm+6)))
                end if
            end if
        end if

        ! Logout for all users
        write(device,AFORMAT) trim(make_href(fnLogout, 'Logout', pre=nbsp))

        ! emergency stop
#if defined no_password_check
        if (isRoleAdmin) &
            write(device,AFORMAT) trim(make_href(fnStop, 'Stop '//PROGNAME, pre=nbsp))
#endif

        ! end of line 1
        write(device,AFORMAT) ' ]</small><br><small>[ <b>Go to</b> :'
        do gdx = 1,NumColleges
            if (.not. College(gdx)%hasInfo) cycle
            ! fnCollegeLinks requested already printed above?
            write(device,AFORMAT) trim(make_href(fnCollegeLinks, College(gdx)%Code, &
                A1=College(gdx)%Code, pre=nbsp))
        end do
        write(device,AFORMAT) ' ]</small>'

        ! a teacher ?
        if (targetTeacher>0 .and. requestingTeacher/=targetTeacher) then
            write(device,AFORMAT) '<br><small>[ <b>'//trim(Teacher(targetTeacher)%Name)//'</b> '

            if (isRoleAdmin .or. &
                (isRoleDean .and. CollegeIdxUser==Department(Teacher(targetTeacher)%DeptIdx)%CollegeIdx ) .or. &
                (isRoleChair .and. DeptIdxUser==Teacher(targetTeacher)%DeptIdx) ) then

                if (REQUEST/=fnEditTeacher .and. REQUEST/=fnGenerateTeacherPassword) then
                    write(device,AFORMAT) trim(make_href(fnEditTeacher, 'Edit info', &
                        A1=Teacher(targetTeacher)%TeacherID, pre=nbsp))
                end if
                if (targetTerm>0) then
                    if (REQUEST/=fnTeacherEditSchedule) then
                        write(device,AFORMAT) trim(make_href(fnTeacherEditSchedule, 'Edit load', &
                            A1=Teacher(targetTeacher)%TeacherID, pre=nbsp, post=DASH//txtSemester(targetTerm+6) ))
                    end if
                else
                    targetTerm = currentTerm
                    if (REQUEST/=fnTeacherEditSchedule) then
                        write(device,AFORMAT) trim(make_href(fnTeacherEditSchedule, 'Edit load', &
                            A1=Teacher(targetTeacher)%TeacherID, pre=nbsp, post=DASH//txtSemester(targetTerm+6) ))
                    end if
                    targetTerm = 0
                end if
            end if

            if (targetTerm>0) then
                if (REQUEST/=fnTeacherClasses) then
                    write(device,AFORMAT) trim(make_href(fnTeacherClasses, 'Classes', &
                        A1=Teacher(targetTeacher)%TeacherID, pre=nbsp, post=DASH//txtSemester(targetTerm+6)))
                end if
                if (REQUEST/=fnPrintableWorkload) then
                    write(device,AFORMAT) trim(make_href(fnPrintableWorkload, 'Load form', &
                        A1=Teacher(targetTeacher)%TeacherID, pre=nbsp, post=DASH//txtSemester(targetTerm+6)))
                end if
            else
                targetTerm = currentTerm
                if (REQUEST/=fnTeacherClasses) then
                    write(device,AFORMAT) trim(make_href(fnTeacherClasses, 'Classes', &
                        A1=Teacher(targetTeacher)%TeacherID, pre=nbsp, post=DASH//txtSemester(targetTerm+6)))
                end if
                if (REQUEST/=fnPrintableWorkload) then
                    write(device,AFORMAT) trim(make_href(fnPrintableWorkload, 'Load form', &
                        A1=Teacher(targetTeacher)%TeacherID, pre=nbsp, post=DASH//txtSemester(targetTerm+6)))
                end if
                targetTerm = 0
            end if

            if (REQUEST/=fnRecentTeacherActivity) then
                write(device,AFORMAT) trim(make_href(fnRecentTeacherActivity, 'Activity', &
                    A1=Teacher(targetTeacher)%TeacherID, pre=nbsp))
            end if
            write(device,AFORMAT) ' ]</small>'
        end if

        ! show department options ?
        if (targetDepartment>0 .and. REQUEST/=fnLogin) then
            tDepartment = Department(targetDepartment)%Code
            write(device,AFORMAT) '<br><small>[ <b>'//trim(tDepartment)//'</b>'
            call info_department(device, tDepartment)
            write(device,AFORMAT) ' ]</small>'
        end if

        write(device,AFORMAT) endtd//endtr, &
            '</table><hr>'

        ! start of body
        if (targetTerm==1) then
            termDescription = Fuchsia//trim(termDescription)//black
        elseif (targetTerm==2) then
            termDescription = Lime//trim(termDescription)//black
        end if
        if (len_trim(header)>0) write(device,AFORMAT) '<h3>'//trim(header)// &
            trim(termDescription)//'</h3>'

    end subroutine html_write_header


    subroutine html_write_footer(device)

        integer, intent(in) :: device

        if (REQUEST==fnDownloadXML) return

        call html_comment('html_write_footer()')

        if (REQUEST/=fnStop .and. &
            REQUEST/=fnLogout .and. &
            REQUEST/=fnChangeTeacherPassword .and. &
            REQUEST/=fnPrintableWorkload) then

            write(device,AFORMAT) &
                '<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
                begintr//begintd// &
                '<small><i>Generated '//currentDate(1:4)//FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8)// &
                DASH//currentTime(1:2)//':'//currentTime(3:4)//' by '//PROGNAME//VERSION//'.'
            if (.not. isRoleAdmin) write(device,AFORMAT) &
                nbsp//'Please report errors to the Registrar.'
            ! Change password, logout
            write(device,AFORMAT) '</i></small>'//endtd//tdalignright//'<small>'
            if (.not. isSuspendMode) then
                write(device,AFORMAT) 'User is '//trim(USERNAME)//'@'//REMOTE_ADDR//nbsp
                if (USERNAME/=GUEST) then
                    write(device,AFORMAT) &
                        trim(make_href(fnChangeTeacherPassword, 'Change password'))
                end if
                write(device,AFORMAT) &
                    trim(make_href(fnLogout, 'Logout', pre=nbsp))
            end if
            write(device,AFORMAT) '</small>'//endtd//endtr//'</table>'

            if (isSuspendMode) then
                write(device,AFORMAT)  &
                    '<small><i>'//red//PROGNAME//FSLASH//ACTION//' is in suspend-mode. '// &
                    'Only the Registrar, Student and Guest roles are allowed at this time.'// &
                    black//'</i></small>'
            end if
        end if

        write(device,AFORMAT) '</body></html>'


    end subroutine html_write_footer


    subroutine html_college_links(device, given, mesg)

        integer, intent(in) :: device
        integer, intent(in), optional :: given
        character(len=*), intent(in), optional :: mesg

        integer :: cdx
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege

        call html_comment('html_college_links()')

        if (present(given)) then
            targetCOLLEGE = given
            cdx = 0
        else
            call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, cdx)
            targetCOLLEGE = index_to_college(tCollege)
        end if

        if (cdx/=0 .or. targetCOLLEGE==0) then ! force to select a unit
            call html_write_header(device, 'Select a unit', mesg)
            ! start of body
            write(device,aformat) '<ul>'
            do cdx = 1,NumColleges
                if (.not. College(cdx)%hasInfo) cycle
                tCollege = College(cdx)%Code
                write(device,aformat) trim(make_href(fnCollegeLinks, tCollege, &
                    A1=tCollege, pre='<li>', post=' - '//trim(College(cdx)%Name)//'</li>'))
            end do
            write(device,aformat) '</ul><hr>'
        else
            call html_write_header(device, College(targetCOLLEGE)%Code//'- '//College(targetCOLLEGE)%Name, mesg)
            call html_college_info(device, targetCOLLEGE)
        end if

    end subroutine html_college_links


    subroutine info_department(device, tDepartment)

        integer, intent(in) :: device
        character(len=MAX_LEN_DEPARTMENT_CODE), intent (in) :: tDepartment

        call html_comment('info_department()')

        !if (REQUEST/=fnSubjectList) then
        !    write(device,AFORMAT) trim(make_href(fnSubjectList, 'Subjects', A1=tDepartment, pre=nbsp))
        !end if

        if (REQUEST/=fnTeachersByDept) then
            write(device,AFORMAT) trim(make_href(fnTeachersByDept, 'Teachers', A1=tDepartment, pre=nbsp))
        end if

        if (REQUEST/=fnRoomList) then
            write(device,AFORMAT) trim(make_href(fnRoomList, 'Rooms', A1=tDepartment, pre=nbsp))
        end if

        if (REQUEST/=fnScheduleOfClasses .and. targetTerm>0) then
            write(device,AFORMAT) trim(make_href(fnScheduleOfClasses, 'Classes', A1=tDepartment, &
                pre=nbsp, post=DASH//txtSemester(targetTerm+6) ))
        end if

    end subroutine info_department


    subroutine html_college_info(device, coll)

        integer, intent(in) :: device
        integer, intent(in) :: coll
        integer :: tdx, rdx, ldx, cdx, dept, n_curr, n_count, tLen
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character (len=1) :: ch
        character (len=80) :: description
        logical :: hasRooms, hasTeachers, hasStudents, hasSubjects

        call html_comment('html_college_info()')

        tCollege = College(coll)%Code
        hasRooms = .false.
        hasTeachers = .false.
        hasStudents = .false.
        hasSubjects = .false.

        ! any curricular programs
        n_curr = 0
        do cdx=1,NumCurricula-1
            if (Curriculum(cdx)%CollegeIdx /= coll) cycle
            n_curr = n_curr+1
            exit
        end do

        ! subject areas in unit
        tLen = 0
#if defined REGIST
        do cdx=1,NumSubjectAreas
            if (SubjectArea(cdx)%CollegeIdx==coll) then
                tLen = tLen+1
                tArray(tLen) = cdx
            end if
        end do
#else
        ! Subjects administered by program
        do cdx=1,NumSubjectAreas
            do ldx=1,NumCurricula-1
                if (Curriculum(ldx)%CollegeIdx/=coll) cycle
                if (is_used_in_curriculum_subject_area(Curriculum(ldx), trim(SubjectArea(cdx)%Code)//SPACE)) then
                    tLen = tLen+1
                    tArray(tLen) = cdx
                    exit
                end if
            end do
        end do
#endif
        hasSubjects = n_curr>0 .or. tLen>0

        ! start of body
        write(device,AFORMAT) '<ul>'

        if (isRoleAdmin .and. coll==CollegeIdxUSER) then

            write(device,AFORMAT) trim(make_href(fnStop, 'Stop', &
                pre='<li><b>', post='</b> '//PROGNAME//red// &
                ' - Are you sure you want to do this? '//black//'View ')), &
                trim(make_href(fnOnlineTeachers, 'online users', pre=nbsp//'<b>', post='.</b>')), &
                trim(make_href(fnDownloadXML, 'Backup', A1='BACKUP.XML', &
                pre=nbsp//'<b>', post='</b> data.</li>'))

            if (isSuspendMode) then ! suspended mode
                write(device,AFORMAT) trim(make_href(fnSuspendProgram, 'Turn it OFF', &
                    pre='<li><b>Suspend-mode is '//red//'ON'//black//'</b>. ', &
                    post=' to allow the Adviser and Scheduler roles.</li>'))
            else
                write(device,AFORMAT) trim(make_href(fnSuspendProgram, 'Turn it ON', &
                    pre='<li><b>Suspend-mode is '//green//'OFF'//black//'</b>. ', &
                    post=' to allow only the Registrar, Student and Guest roles.</li>'))
            end if

            write(device,AFORMAT) trim(make_href(fnEditSignatories, 'signatories', &
                pre='<li><b>Edit '//nbsp, post=nbsp//'</b>in teaching load form.</li>'))

            write(device,AFORMAT) '<li><b>'//trim(make_href(fnResetPasswords, 'Reset', &
                post='</b> passwords. <b>Download</b> : right-click on link, then "Save Link As..." ')), &
                trim(make_href(fnDownloadXML, 'PASSWORDS-TEACHERS.CSV', A1='PASSWORDS-TEACHERS.CSV', post=nbsp)), &
                trim(make_href(fnDownloadXML, 'PASSWORDS-STUDENTS.CSV', A1='PASSWORDS-STUDENTS.CSV', post='</li>'))

        end if

        if (hasSubjects) then
            write(device,AFORMAT) '<li><b>Curricular programs</b> : '
            done = .false.
            do cdx=1,NumCurricula-1
                if (Curriculum(cdx)%CollegeIdx /= coll) cycle
                if (done(cdx)) cycle
                n_count = 1
                do ldx=cdx+1,NumCurricula-1
                    if (CurrProgCode(ldx)/=CurrProgCode(cdx)) cycle
                    n_count = n_count+1
                end do

                write(device,AFORMAT) trim(make_href(fnCurriculumList, CurrProgCode(cdx), &
                    A1=CurrProgCode(cdx), post='('//trim(itoa(n_count))//')'//nbsp))
                do ldx=cdx+1,NumCurricula-1
                    if (CurrProgCode(ldx) == CurrProgCode(cdx)) done(ldx) = .true.
                end do
            end do
            write(device,AFORMAT) '</li>'
        end if

        ! subjects
        call links_to_subjects(device, coll, tLen, tArray(1))

        if (hasSubjects) write(device,AFORMAT) '<hr>'

        ! rooms
        n_count = 0
        do rdx=1,NumRooms+NumAdditionalRooms
            if (Department(Room(rdx)%DeptIdx)%CollegeIdx /= coll) cycle
            n_count = n_count+1
            exit
        end do
        hasRooms = n_count>0
        if (hasRooms) then

            call html_comment('room links()')

            write(device,AFORMAT) '<li><b>Rooms by department</b> : '
            do dept=2,NumDepartments
                if (Department(dept)%CollegeIdx /= coll) cycle
                n_count = 0
                do rdx=1,NumRooms+NumAdditionalRooms
                    if (Room(rdx)%DeptIdx /= dept) cycle
                    n_count = n_count+1
                end do
                if (n_count==0) cycle
                tDepartment = Department(dept)%Code
                write(device,AFORMAT) trim(make_href(fnRoomList, tDepartment, &
                    A1=tDepartment, pre=nbsp, post='('//trim(itoa(n_count))//')'))
            end do
            write(device,AFORMAT) '</li>'
        end if

        ! teachers
        n_count = 0
        do tdx=1,NumTeachers+NumAdditionalTeachers
            if (Department(Teacher(tdx)%DeptIdx)%CollegeIdx /= coll) cycle
            n_count = n_count+1
            exit
        end do
        hasTeachers = n_count>0
        if (hasTeachers) then

            call html_comment('teacher links()')

            write(device,AFORMAT) '<li><b>Teachers by department</b> : '
            do dept=2,NumDepartments
                if (Department(dept)%CollegeIdx /= coll) cycle
                n_count = 0
                do tdx=1,NumTeachers+NumAdditionalTeachers
                    if (Teacher(tdx)%DeptIdx /= dept) cycle
                    if (Teacher(tdx)%Role==ADMINISTRATION) cycle
                    n_count = n_count+1
                end do
                if (n_count==0) cycle
                tDepartment = Department(dept)%Code
                write(device,AFORMAT) trim(make_href(fnTeachersByDept, tDepartment, &
                    A1=tDepartment, pre=nbsp, post='('//trim(itoa(n_count))//')'))
            end do
            write(device,AFORMAT) '</li><li><b>Teachers by last name</b> : '
            do dept=iachar('A'), iachar('Z')
                ch = achar(dept)
                n_count = 0
                do tdx=1,NumTeachers+NumAdditionalTeachers
                    if (Teacher(tdx)%Name(1:1) /= ch) cycle
                    if (Department(Teacher(tdx)%DeptIdx)%CollegeIdx /= coll) cycle
                    if (Teacher(tdx)%Role==ADMINISTRATION) cycle
                    n_count = n_count+1
                end do
                if (n_count==0) cycle
                write(device,AFORMAT) trim(make_href(fnTeachersByName, ch, &
                    A1=tCollege, A2=ch, pre=nbsp, post='('//trim(itoa(n_count))//')'))
            end do
            write(device,AFORMAT) '</li>'

            call make_form_start(device, fnFindTeacher)
            write(device,AFORMAT) '<li><table border="0" cellpadding="0" cellspacing="0">', &
                begintr//begintd//'<b>Find teachers</b> with <input name="A1" value=""> in name. ', &
                '<input type="submit" name="action" value="Search"></form>'// &
                endtd//endtr//'</table></li>'

        end if

        if (hasRooms .or. hasTeachers) write(device,AFORMAT) '<hr>'


        ! per term
        cdx = targetTerm ! remember targetTerm
        do ldx=termBegin,termEnd

            call qualify_term (ldx, rdx, targetTerm, description)
            write(device,AFORMAT) '<li><b>'//trim(description)//'</b><ul>'

            ! blocks
!#if defined REGIST
!#else
            if (n_curr>0) call links_to_blocks(device, coll, targetTerm)
!#endif

            ! classes
            call links_to_sections(device, coll, tLen, tArray(1), targetTerm)

            write(device,AFORMAT) '</ul></li>'
        end do
        targetTerm = cdx ! restore

        write(device,AFORMAT) '</ul><hr>'

    end subroutine html_college_info


    subroutine links_to_depts(device, coll, fn, heading)
        integer, intent (in) :: device, coll, fn
        character(len=*), intent (in) :: heading
        integer :: dept, crse, n_count
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        call html_comment('links_to_depts()')

        write(device,AFORMAT) '<li>'//heading//' : '
        do dept=2,NumDepartments
            if (Department(dept)%CollegeIdx/=coll) cycle
            n_count = 0
#if defined REGIST
            do crse=1,NumSubjects+NumAdditionalSubjects
                if (Subject(crse)%DeptIdx /= dept) cycle
                n_count = n_count+1
                exit
            end do
#else
            ! Subjects administered by program
            do crse=1,NumSubjects+NumAdditionalSubjects
                if (.not. is_used_in_college_subject(coll, crse)) cycle
                n_count = n_count+1
                exit
            end do
#endif
            if (n_count==0) cycle
            tDepartment = Department(dept)%Code
            write(device,AFORMAT) trim(make_href(fn, tDepartment, A1=tDepartment, post=nbsp))
        end do
        write(device,AFORMAT) '</li>'


    end subroutine links_to_depts


    subroutine links_to_subjects(device, coll, numAreas, AreaList)
        integer, intent (in) :: device, coll, numAreas
        integer, intent (in) :: AreaList(1:numAreas)
        integer :: dept, n_count
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
#if defined REGIST
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer :: crse
#endif
        if (numAreas==0) return

        call html_comment('links_to_subjects()')

#if defined REGIST
        write(device,AFORMAT) '<li><b>Subjects</b> in :'
        do dept=2,NumDepartments
            if (Department(dept)%CollegeIdx /= coll) cycle
            n_count = 0
            do crse=1,NumSubjects+NumAdditionalSubjects
                if (Subject(crse)%DeptIdx /= dept) cycle
                n_count = n_count+1
                !exit
            end do
!#else
!            ! Subjects administered by program
!            do crse=1,NumSubjects+NumAdditionalSubjects
!                if (.not. is_used_in_college_subject(coll, crse)) cycle
!                n_count = n_count+1
!                !exit
!            end do
!#endif
            if (n_count==0) cycle
            tDepartment = Department(dept)%Code
            write(device,AFORMAT) trim(make_href(fnSubjectList, tDepartment, A1=tDepartment, &
                pre=nbsp, post='('//trim(itoa(n_count))//')'))
        end do
#endif
        write(device,AFORMAT) '</li><li><b>Subjects</b> by area :'
        do dept=1,numAreas
            tSubject = SubjectArea(AreaList(dept))%Code
            n_count = SubjectArea(AreaList(dept))%Count
            write(device,AFORMAT) trim(make_href(fnSubjectList, tSubject, A1=tSubject, &
                pre=nbsp, post='('//trim(itoa(n_count))//')'))
        end do
        write(device,AFORMAT) '</li>'

    end subroutine links_to_subjects


    subroutine links_to_sections(device, coll, numAreas, AreaList, term)
        integer, intent (in) :: device, coll, numAreas, term
        integer, intent (in) :: AreaList(1:numAreas)
        integer :: ddx, dept, sect, n_count, m_count, mdx, k1, k2
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        if (numAreas==0) return

        call html_comment('links_to_sections()')

        tCollege = College(coll)%Code
#if defined REGIST
        ddx = 0
#else
        tDepartment = tCollege
        ddx = index_to_dept(tDepartment)
#endif

#if defined REGIST
        write(device,AFORMAT) '<li><b>Classes</b> in : '
        do dept=2,NumDepartments
            if (Department(dept)%CollegeIdx /= coll) cycle
            n_count = 0
            do k1=1,NumSubjects+NumAdditionalSubjects
                if (Subject(k1)%DeptIdx /= dept) cycle
                n_count = n_count+1
                exit
            end do
            if (n_count==0) cycle ! no subjects in this department

            ! how many sections currently open
            n_count = 0
            do sect=1,NumSections(term)
                if (dept/=Section(term,sect)%DeptIdx) cycle ! not in department
                if (Section(term,sect)%SubjectIdx==0) cycle ! deleted
                n_count = n_count+1
            end do
            tDepartment = Department(dept)%Code
            write(device,AFORMAT) trim(make_href(fnScheduleOfClasses, tDepartment, &
                A1=tDepartment, post='('//trim(itoa(n_count))//')'//nbsp))
        end do

        write(device,AFORMAT) '</li> '
#endif

        write(device,AFORMAT) '<li><b>Classes by subject area</b> : '
        do dept=1,numAreas
            ! the code
            tSubject = SubjectArea(AreaList(dept))%Code
            k1 = len_trim(tSubject)+1
            n_count = 0 ! how many sections with this code currently open
            do sect=1,NumSections(term)
#if defined REGIST
                if (Section(term,sect)%ClassId(:k1)==tSubject(:k1)) n_count = n_count+1
#else
                if (Section(term,sect)%ClassId(:k1)==tSubject(:k1) .and. &
                    ddx==Section(term,sect)%DeptIdx) n_count = n_count+1
#endif
            end do
            write(device,AFORMAT) trim(make_href(fnScheduleByArea, tSubject, &
                A1=tCollege, A2=tSubject, post='('//trim(itoa(n_count))//')'//nbsp))
        end do

        write(device,AFORMAT) '</li><li><b>Classes with </b> '
        n_count = 0 ! how many sections with TBA teachers
        m_count = 0 ! how many sections with TBA rooms
        do sect=1,NumSections(term)
            if ( coll/=Department(Section(term,sect)%DeptIdx)%CollegeIdx ) cycle
            k1 = 0
            k2 = 0
            do mdx=1,Section(term,sect)%NMeets
                if (Section(term,sect)%TeacherIdx(mdx)==0) k1 = k1+1
                if (Section(term,sect)%RoomIdx(mdx)==0) k2 = k2+1
            end do
            if (k1>0) n_count = n_count+1
            if (k2>0) m_count = m_count+1
        end do
        write(device,AFORMAT) trim(make_href(fnTBATeachers, 'TBA teachers', A1=tCollege, &
            pre=nbsp, post='('//trim(itoa(n_count))//')'))
        write(device,AFORMAT) trim(make_href(fnTBARooms, 'TBA rooms', A1=tCollege, &
            pre=nbsp, post='('//trim(itoa(m_count))//')'))

        write(device,AFORMAT) '</li><li><b>Conflicts in schedules of </b> '
        write(device,AFORMAT) trim(make_href(fnTeacherConflicts, 'teachers', A1=tCollege, pre=nbsp))
        write(device,AFORMAT) trim(make_href(fnRoomConflicts, 'rooms', A1=tCollege, pre=nbsp))
        write(device,AFORMAT) trim(make_href(fnBlockConflicts, 'blocks', A1=tCollege, pre=nbsp))
        write(device,AFORMAT) '</li>'

    end subroutine links_to_sections


    subroutine links_to_blocks(device, coll, term)
        integer, intent (in) :: device, coll, term
        integer :: cdx, ldx, blk, ncurr

        call html_comment('links_to_blocks()')

        ncurr = 0
        write(device,AFORMAT) '<li><b>Blocks</b> : '//nbsp
        done = .false.
        do cdx=1,NumCurricula-1
            if (Curriculum(cdx)%CollegeIdx /= coll) cycle
            if (done(cdx)) cycle
            if (Curriculum(cdx)%NumTerms==0) cycle
            ldx = 0
            do blk=1,NumBlocks(term)
                if (CurrProgCode(Block(term,blk)%CurriculumIdx)/=CurrProgCode(cdx)) cycle
                ldx = ldx+1
            end do
            ncurr = ncurr + 1
            write(device,AFORMAT) trim(make_href(fnBlockList, CurrProgCode(cdx), &
                A1=CurrProgCode(cdx), post='('//trim(itoa(ldx))//')'//nbsp))
            do ldx=cdx+1,NumCurricula-1
                if (CurrProgCode(ldx) == CurrProgCode(cdx)) done(ldx) = .true.
            end do
        end do
        write(device,AFORMAT) '</li>'

    end subroutine links_to_blocks


    subroutine blocks_in_section(device, sect, fn, NumBlocks, Block)
        integer, intent(in) :: device, sect, fn, NumBlocks
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        integer :: idx, jdx

        call html_comment('blocks_in_section()')

        do idx=1,NumBlocks
            do jdx=1,Block(idx)%NumClasses
                if (Block(idx)%Section(jdx)/=sect) cycle
                if (fn>0) then
                    write(device,AFORMAT) trim(make_href(fn, Block(idx)%BlockID, A1=Block(idx)%BlockID))
                else
                    write(device,AFORMAT) trim(Block(idx)%BlockID)
                end if
                exit
            end do
        end do

    end subroutine blocks_in_section


    subroutine recent_teacher_activity(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher, tRole
        integer :: ldx, iTmp !, nDays, sYear, sMonth, sDay, eYear, eMonth, eDay
        character (len=MAX_LEN_FILE_PATH) :: logFile
        logical :: logExists

        ! which teacher ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, iTmp)
        targetTeacher = index_to_teacher(tTeacher)
        call blank_to_underscore(tTeacher, tRole)
        tTeacher = tRole
        iTmp = Teacher(targetTeacher)%DeptIdx
        ldx = Department(iTmp)%CollegeIdx
        tRole = Teacher(targetTeacher)%Role

        call html_comment('recent_teacher_activity()')

        call html_write_header(device, 'Recent activity of '//trim(Teacher(targetTeacher)%Name), SPACE)

        logFile = trim(dirLOG)// &
            trim(College(ldx)%Code)//DIRSEP// &
            trim(tTeacher)//'.log'
        inquire(file=logFile, exist=logExists)
        if (logExists) then
            call copy_to_unit(logFile, device)
        else
            write(device,AFORMAT) '<br>(None)<hr>'
        end if

    end subroutine recent_teacher_activity


    subroutine copy_to_unit(logFile, device)
        integer, intent(in) :: device
        character (len=MAX_LEN_FILE_PATH), intent(in) :: logFile
        integer :: iTmp

        call html_comment('copy_to_unit('//trim(logFile)//')')

        write(device,AFORMAT) '<pre>'
        open(unit=unitETC, file=logFile, status='old')
        do
            read(unitETC, AFORMAT, iostat=iTmp) cipher
            if (iTmp<0) exit
            write(device,AFORMAT) trim(cipher)
        end do
        close(unitETC)
        write(device,AFORMAT) '</pre><hr>'

    end subroutine copy_to_unit


    subroutine timetable_meetings_of_teacher(NumSections, Section, teacher_index, to_skip, len_list, list, TimeTable, conflicted)
        type (TYPE_SECTION), intent(in) :: Section(0:)
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

    end subroutine timetable_meetings_of_teacher


    subroutine timetable_meetings_in_room(NumSections, Section, room_index, to_skip, len_list, list, TimeTable, conflicted)
        type (TYPE_SECTION), intent(in) :: Section(0:)
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

    end subroutine timetable_meetings_in_room


end module HTML
