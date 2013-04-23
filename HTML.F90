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

    use BLOCKS
    use TIMETABLES
    use ADVISING

    implicit none

    ! index to server functions
    integer, parameter ::  &
        fnLogin                   =  1, & ! login user
        fnGeneratePassword        =  2, & ! generate new password
        fnChangePassword          =  3, & ! change password
        fnLogout                  =  4, & ! logout user
        fnSuspendProgram          =  5, & ! suspend the program
        fnToggleTrainingMode      =  6, & ! toggle training mode
        fnEditSignatories         =  7, & ! edit signatories
        !
        fnCollegeLinks            =  8, & ! index to college info
        fnSubjectList             =  9, & ! view list of subjects administered by a department
        fnEditSubject             = 10, & ! edit subject
        fnCurriculumList          = 11, & ! view list of curricular programs administered by a college
        fnCurriculum              = 12, & ! view a curricular program
        fnEditCurriculum          = 13, & ! edit curriculum
        fnActivateCurriculum      = 14, & ! activate curriculum
        fnDeactivateCurriculum    = 15, & ! deactivate curriculum
        fnEditRoom                = 16, & ! edit room parameters
        fnEditTeacher             = 17, & ! edit teacher record
        fnStop                    = 18, & ! terminate program
        !
        fnStudentsByProgram       = 20, & ! view list students in a program
        fnStudentsByCurriculum    = 21, & ! view list students in a curriculum
        fnStudentsByName          = 22, & ! view alphabetical list of students in a college
        fnStudentsByYear          = 23, & ! view list of students in college by year
        fnStudentsDistribution    = 24, & ! view distribution of students, by curriculum and by college
        fnStudentAdd              = 25, & ! add a student
        fnStudentAddPrompt        = 26, & ! entry form for 'add a student'
        fnStudentPerformance      = 27, & ! view student performance
        fnEditCheckList           = 28, & ! display checklist for editing
        !
        fnScheduleOfClasses       = 30, & ! display schedule of classes for editing
        fnScheduleOfferSubject    = 31, & ! offer a subject
        fnScheduleAddLab          = 32, & ! add a lab section
        fnScheduleDelete          = 33, & ! delete a section
        fnScheduleEdit            = 34, & ! edit a section
        fnScheduleValidate        = 35, & ! check correctness of edit inputs
        fnTeachersByDept          = 36, & ! view list teachers belonging to a college
        fnTeachersByName          = 37, & ! view alphabetical list of teachers in a college
        fnTeacherSchedule         = 38, & ! view weekly schedule of a teacher
        fnRoomList                = 39, & ! view list rooms administered by a college
        fnRoomSchedule            = 40, & ! view weekly schedule of a room
        fnRoomConflicts           = 41, & ! view list of rooms with conflicts
        fnTeacherConflicts        = 42, & ! view list of teachers with conflicts
        fnTBARooms                = 43, & ! view list of sections with TBA rooms
        fnTBATeachers             = 44, & ! view list of sections with TBA teachers
        fnPrintableWorkload       = 45, & ! printable teaching load
        fnScheduleByArea          = 46, & ! display schedule of classes for editing, by area
        !
        fnBlockSchedule           = 50, & ! display schedule of block section
        fnBlockEditName           = 51, & ! edit name of block section
        fnBlockDeleteName         = 52, & ! delete block, but keep its sections
        fnBlockDeleteAll          = 53, & ! delete block and its sections
        fnBlockNewSelect          = 54, & ! select parameters for a new block
        fnBlockNewAdd             = 55, & ! add a new block
        fnBlockCopy               = 56, & ! copy block
        fnBlockEditSection        = 57, & ! edit section in block
        fnBlockEditSubject        = 58, & ! update subjects in block
        fnBlockList               = 59, & ! list blocks
        !
        fnDemandFreshmen          = 60, & ! view demand for subjects by incoming students
        fnUpdateDemandFreshmen    = 61, & ! update no. of incoming students
        fnPrintableSchedule       = 62, & ! printable weekly timetable
        fnDemandForSubjects       = 63, & ! view demand for subjects
        fnPotentialStudents       = 64, & ! list of potential students of a subject
        !
        fnChangeMatriculation     = 65, & ! change matriculation
        fnFindBlock               = 66, & ! find a block for student
        fnSelectSubjects          = 67, & ! manually select subjects for student
        !
        fnEnlistmentSummary       = 70, & ! summary of enlistment by subject
        fnNotAccommodated         = 71, & ! students not accommodated in a priority subject
        fnBottleneck              = 72, & ! "bottleneck" subjects
        fnExtraSlots              = 73, & ! subjects with excess slots
        fnUnderloadSummary        = 74, & ! summary of underloading
        fnUnderloadedStudents     = 75, & ! underloaded students
        fnClassList               = 76, & ! view list of students in a class
        fnGradeSheet              = 77, &    ! enter grades
        fnDownloadXML             = 78 ! download XML data file

    ! the requested server function
    integer :: REQUEST

    ! argument 'ACTION' from command line
    character (len=20) :: ACTION

    ! the target of the fucntion
    integer :: targetCollege, targetDepartment, targetSubject, targetRoom, targetTeacher, &
        targetCurriculum, targetStudent, targetBlock, targetSection, targetLogin

    ! work arrays
    integer :: tArray(max(MAX_ALL_STUDENTS,2*MAX_ALL_SUBJECTS))
    character (len=80) :: QUERY_put, termDescription


contains

    function fnDescription (fn)
        character(len=60) :: fnDescription
        integer, intent (in) :: fn

        select case (fn)

            case (fnStop                   )
                    fnDescription = 'stop program'

            case (fnLogin                   )
                    fnDescription = 'login'

            case (fnLogout                )
                    fnDescription = 'logout'

            case (fnGeneratePassword   )
                    fnDescription = 'generate new password'

            case (fnChangePassword          )
                    fnDescription = 'change password'

            case (fnToggleTrainingMode      )
                    fnDescription = 'toggle training mode'

            case (fnEditSignatories)
                    fnDescription = 'edit signatories'

            case (fnCollegeLinks            )
                    fnDescription = 'index to college info'

            case (fnSubjectList             )
                    fnDescription = 'view list of subjects administered by a department'

            case (fnEditSubject             )
                    fnDescription = 'edit subject'

            case (fnCurriculumList          )
                    fnDescription = 'view list of curricular programs administered by a college'

            case (fnCurriculum              )
                    fnDescription = 'view a curricular program'

            case (fnEditCurriculum          )
                    fnDescription = 'edit curriculum'

            case (fnActivateCurriculum      )
                    fnDescription = 'activate curriculum'

            case (fnDeactivateCurriculum    )
                    fnDescription = 'deactivate curriculum'

            case (fnEditRoom                )
                    fnDescription = 'edit room parameters'

            case (fnEditTeacher             )
                    fnDescription = 'edit teacher record'

            case (fnStudentsByProgram       )
                    fnDescription = 'view list students in a program'

            case (fnStudentsByCurriculum    )
                    fnDescription = 'view list students in a curriculum'

            case (fnStudentsByName          )
                    fnDescription = 'view alphabetical list of students in a college'

            case (fnStudentsByYear          )
                    fnDescription = 'view list of students in college by year'

            case (fnStudentsDistribution    )
                    fnDescription = 'view distribution of students, by curriculum and by college'

            case (fnStudentAdd              )
                    fnDescription = 'add a student'

            case (fnStudentAddPrompt        )
                    fnDescription = 'entry form for "add a student"'

            case (fnStudentPerformance      )
                    fnDescription = 'view student performance'

            case (fnEditCheckList           )
                    fnDescription = 'display checklist for editing'

            case (fnChangeMatriculation     )
                    fnDescription = 'change matriculation'

            case (fnFindBlock               )
                    fnDescription = 'find a block for student'

            case (fnSelectSubjects)
                    fnDescription = 'manually select subjects for student'

            case (fnScheduleOfClasses       )
                    fnDescription = 'display schedule of classes for editing'

            case (fnScheduleOfferSubject    )
                    fnDescription = 'offer a subject'

            case (fnScheduleAddLab          )
                    fnDescription = 'add a lab section'

            case (fnScheduleDelete          )
                    fnDescription = 'delete a section'

            case (fnScheduleEdit            )
                    fnDescription = 'edit a section'

            case (fnScheduleValidate        )
                    fnDescription = 'check correctness of edit inputs'

            case (fnTeachersByDept          )
                    fnDescription = 'view list teachers belonging to a college'

            case (fnTeachersByName          )
                    fnDescription = 'view alphabetical list of teachers in a college'

            case (fnTeacherSchedule         )
                    fnDescription = 'view weekly schedule of a teacher'

            case (fnRoomList                )
                    fnDescription = 'view list rooms administered by a college'

            case (fnRoomSchedule            )
                    fnDescription = 'view weekly schedule of a room'

            case (fnRoomConflicts           )
                    fnDescription = 'view list of rooms with conflicts'

            case (fnTeacherConflicts        )
                    fnDescription = 'view list of teachers with conflicts'

            case (fnTBARooms                )
                    fnDescription = 'view list of sections with TBA rooms'

            case (fnTBATeachers             )
                    fnDescription = 'view list of sections with TBA teachers'

            case (fnBlockSchedule           )
                    fnDescription = 'display schedule of block section'

            case (fnBlockEditName           )
                    fnDescription = 'edit name of block section'

            case (fnBlockDeleteName         )
                    fnDescription = 'delete block, but keep its sections'

            case (fnBlockDeleteAll          )
                    fnDescription = 'delete block and its sections'

            case (fnBlockNewSelect          )
                    fnDescription = 'select parameters for a new block'

            case (fnBlockNewAdd             )
                    fnDescription = 'add a new block'

            case (fnBlockCopy               )
                    fnDescription = 'copy block'

            case (fnBlockList               )
                    fnDescription = 'list blocks'

            case (fnBlockEditSection        )
                    fnDescription = 'edit section in block'

            case (fnBlockEditSubject        )
                    fnDescription = 'update subjects in block'

            case (fnScheduleByArea          )
                    fnDescription = 'display schedule of classes for editing, by area'

            case (fnPrintableWorkload       )
                    fnDescription = 'printable teaching load'

            case (fnEnlistmentSummary       )
                    fnDescription = 'summary of enlistment by subject'

            case (fnNotAccommodated         )
                    fnDescription = 'students not accommodated in a priority subject'

            case (fnBottleneck              )
                    fnDescription = '"bottleneck" subjects'

            case (fnExtraSlots              )
                    fnDescription = 'subjects with excess slots'

            case (fnUnderloadSummary        )
                    fnDescription = 'summary of underloading'

            case (fnUnderloadedStudents     )
                    fnDescription = 'underloaded students'

            case (fnClassList               )
                    fnDescription = 'view list of students in a class'

            case (fnGradeSheet              )
                    fnDescription = 'enter grades'

            case (fnDemandFreshmen          )
                    fnDescription = 'view demand for subjects by incoming students'

            case (fnUpdateDemandFreshmen    )
                    fnDescription = 'update no. of incoming students'

            case (fnPrintableSchedule       )
                    fnDescription = 'printable weekly timetable'

            case (fnDemandForSubjects       )
                    fnDescription = 'view demand for subjects'

            case (fnPotentialStudents       )
                    fnDescription = 'list of potential students of a subject'

            case (fnSuspendProgram             )
                    fnDescription = 'toggle suspend mode'

            case (fnDownloadXML             )
                    fnDescription = 'download XML data file'

        end select
        return
    end function fnDescription


    function fnAvailable (fn)
        logical :: fnAvailable
        integer, intent (in) :: fn

        select case (fn)

            case (fnStop                   )
                    fnAvailable = .true.

            case (fnLogin                   )
                    fnAvailable = .true.

            case (fnLogout                )
                    fnAvailable = .true.

            case (fnGeneratePassword   )
                    fnAvailable = .true.

            case (fnChangePassword          )
                    fnAvailable = .true.

            case (fnToggleTrainingMode      )
                    fnAvailable = .true.

            case (fnEditSignatories)
                    fnAvailable = .true.

            case (fnCollegeLinks            )
                    fnAvailable = .true.

            case (fnSubjectList             )
                    fnAvailable = .true.

            case (fnEditSubject             )
                    fnAvailable = .true.

            case (fnCurriculumList          )
                    fnAvailable = .true.

            case (fnCurriculum              )
                    fnAvailable = .true.

            case (fnEditCurriculum          )
                    fnAvailable = .true.

            case (fnActivateCurriculum      )
                    fnAvailable = .true.

            case (fnDeactivateCurriculum    )
                    fnAvailable = .true.

            case (fnEditRoom                )
                    fnAvailable = .true.

            case (fnEditTeacher             )
                    fnAvailable = .true.

            case (fnStudentsByProgram       )
                    fnAvailable = .true.

            case (fnStudentsByCurriculum    )
                    fnAvailable = .true.

            case (fnStudentsByName          )
                    fnAvailable = .true.

            case (fnStudentsByYear          )
                    fnAvailable = .true.

            case (fnStudentsDistribution    )
                    fnAvailable = .true.

            case (fnStudentAdd              )
                    fnAvailable = .true.

            case (fnStudentAddPrompt        )
                    fnAvailable = .true.

            case (fnStudentPerformance      )
                    fnAvailable = .true.

            case (fnEditCheckList           )
                    fnAvailable = advisingPeriod

            case (fnChangeMatriculation     )
                    fnAvailable = currentTerm==nextTerm

            case (fnFindBlock               )
                    fnAvailable = currentTerm==nextTerm

            case (fnSelectSubjects)
                    fnAvailable = currentTerm==nextTerm

            case (fnScheduleOfClasses       )
                    fnAvailable = .true.

            case (fnScheduleOfferSubject    )
                    fnAvailable = advisingPeriod

            case (fnScheduleAddLab          )
                    fnAvailable = advisingPeriod

            case (fnScheduleDelete          )
                    fnAvailable = advisingPeriod

            case (fnScheduleEdit            )
                    fnAvailable = advisingPeriod

            case (fnScheduleValidate        )
                    fnAvailable = advisingPeriod

            case (fnTeachersByDept          )
                    fnAvailable = .true.

            case (fnTeachersByName          )
                    fnAvailable = .true.

            case (fnTeacherSchedule         )
                    fnAvailable = .true.

            case (fnRoomList                )
                    fnAvailable = .true.

            case (fnRoomSchedule            )
                    fnAvailable = .true.

            case (fnRoomConflicts           )
                    fnAvailable = .true.

            case (fnTeacherConflicts        )
                    fnAvailable = .true.

            case (fnTBARooms                )
                    fnAvailable = .true.

            case (fnTBATeachers             )
                    fnAvailable = .true.

            case (fnBlockSchedule           )
                    fnAvailable = .true.

            case (fnBlockEditName           )
                    fnAvailable = advisingPeriod

            case (fnBlockDeleteName         )
                    fnAvailable = advisingPeriod

            case (fnBlockDeleteAll          )
                    fnAvailable = advisingPeriod

            case (fnBlockNewSelect          )
                    fnAvailable = advisingPeriod

            case (fnBlockNewAdd             )
                    fnAvailable = advisingPeriod

            case (fnBlockCopy               )
                    fnAvailable = advisingPeriod

            case (fnBlockList               )
                    fnAvailable = .true.

            case (fnBlockEditSection        )
                    fnAvailable = advisingPeriod

            case (fnBlockEditSubject        )
                    fnAvailable = advisingPeriod

            case (fnScheduleByArea          )
                    fnAvailable = .true.

            case (fnPrintableWorkload       )
                    fnAvailable = .true.

            case (fnEnlistmentSummary       )
                    fnAvailable = currentTerm==nextTerm

            case (fnNotAccommodated         )
                    fnAvailable = currentTerm==nextTerm

            case (fnBottleneck              )
                    fnAvailable = currentTerm==nextTerm

            case (fnExtraSlots              )
                    fnAvailable = currentTerm==nextTerm

            case (fnUnderloadSummary        )
                    fnAvailable = currentTerm==nextTerm

            case (fnUnderloadedStudents     )
                    fnAvailable = currentTerm==nextTerm

            case (fnClassList               )
                    fnAvailable = currentTerm==nextTerm

            case (fnGradeSheet              )
                    fnAvailable = .false. !currentTerm==nextTerm

            case (fnDemandFreshmen          )
                    fnAvailable = .true.

            case (fnUpdateDemandFreshmen    )
                    fnAvailable = .true.

            case (fnPrintableSchedule       )
                    fnAvailable = currentTerm==nextTerm

            case (fnDemandForSubjects       )
                    fnAvailable = .true.

            case (fnPotentialStudents       )
                    fnAvailable = .true.

            case (fnSuspendProgram             )
                    fnAvailable = .true.

            case (fnDownloadXML             )
                    fnAvailable = .true.

        end select
        return
    end function fnAvailable


    subroutine html_copyright(device)
        integer, intent(in) :: device

        write(device,AFORMAT) '<!-- '//'html_copyright()'//' -->'

        write(device,AFORMAT) &
            '<small><i>'//PROGNAME//nbsp//COPYRIGHT//'<br>', &
            'This program comes with ABSOLUTELY NO WARRANTY; for details see the ', &
            'GNU GENERAL PUBLIC LICENSE Version 3 ', &
            '(<a target="0" href="http://www.gnu.org/licenses/gpl-3.0.html">GPLv3</a>).<br>', &
            'This program is free software, and you are welcome to redistribute it under certain conditions; ', &
            ' see the GPLv3 for details.<br>', &
            'The source code is available at <a target="0" href="'//WEB//'">'// &
            WEB//'</a><br>', &
            CONTACT//'</i></small><hr>'

        return
    end subroutine html_copyright


    subroutine html_landing_page(device, mesg)
        integer, intent(in) :: device
        character(len=*), intent(in) :: mesg
        integer :: j, k

        write(device,AFORMAT) &
            '<html><head><title>'//trim(UniversityCode)//SPACE//PROGNAME//'</title></head><body>'

        if (len_trim(mesg)>0) then ! Stop/Logout page
            write(device,aformat) '<h2>'//trim(UniversityCode)//SPACE//PROGNAME// &
                ' for '//text_school_year(currentYear)//'</h2>', &
                red//trim(mesg)//black
            if (REQUEST==fnStop)  &
                write(device,AFORMAT)'<br><br><a href="/">Index</a>'
        else ! Login page
            write(device,AFORMAT) &
                '<h2>Welcome to '//trim(UniversityCode)//SPACE//PROGNAME//FSLASH//trim(ACTION)// &
                ' for '//text_school_year(currentYear)//'</h2><hr>'
            write(device,AFORMAT) '<table border="0" width="100%">'//begintr, &
                '<td valign="top" width="25%">', &
                '<form method="post" action="'//trim(CGI_PATH)//'">', &
                '<input type="hidden" name="F" value="'//trim(itoa(fnLogin))//'">', &
                '<b>Username</b> (case sensitive):<br>', &
                '<input size="20" type="text" name="N" value="Guest">', & !OUR">', & !
                '<br><br>', &
                '<b>Password:</b><br>', &
                '<input size="20" type="password" name="P" value="Guest">', & !q">', & ! 
                '<br><br>', &
                '<input type="submit" value="Login"></form>'
            if (len_trim(loginCheckMessage)>0) write(device,AFORMAT) &
                '<br>'//red//trim(loginCheckMessage)//black//'<br>'
            write(device,AFORMAT) endtd//begintd, &
                '<b>Role - <i>privilege</i> ( Username )</b><ul>', &
                '<li>Anybody - <i>view all data except student records</i> ( Guest )</li>'
            write(device,AFORMAT) &
                '<li>Curriculum advisers - <i>view all data, modify records of students in curriculum</i> ( TeacherID '
            done = .false.
            do k=1,NumCurricula-2
                if (done(k)) cycle
                write(device,AFORMAT) trim(CurrProgCode(k))//nbsp
                do j = k+1,NumCurricula-1
                    if (CurrProgCode(k)==CurrProgCode(j)) done(j) = .true.
                end do
            end do
            write(device,AFORMAT) &
                ' ) </li><li>Teaching load schedulers - <i>view all data; modify teacher info, room info, class schedules</i>'//&
                ' ( TeacherID '
            do k=2,NumDepartments-1
                write(device,AFORMAT) trim(Department(k)%Code)//nbsp
            end do
            write(device,AFORMAT) &
                ' ) </li><li>Registrar - <i>view all, modify all</i> ( '//trim(REGISTRAR)//' )</li>'
            write(device,AFORMAT) '</ul>'//endtd//endtr//'</table><hr>'
            call html_copyright(device)

        end if

        return
    end subroutine html_landing_page


    subroutine html_login(fname, mesg)
        character(len=*), intent(in) :: fname, mesg
        integer :: device=7

        open(unit=device, file=fname, form='formatted', status='unknown')
        call html_landing_page(device, mesg)
        write(device,AFORMAT) '</body></html>'
        close(device)

        return
    end subroutine html_login


    subroutine timetable_display(device, Section, TimeTable)
        integer, intent(in) :: device, TimeTable(60,6)
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer, parameter :: period = 2 ! no. of 15 minute intervals
        integer :: i, color, ncol, sect, j, mcol
        character (len=1024) :: line
        integer :: colorIdx(60,6)

        write(device,AFORMAT) '<!-- '//'timetable_display()'//' -->'

        ! background colors
        colorIdx = 0
        color = 0
        do ncol=1,56,period
            do i=6,1,-1
                if (TimeTable(ncol,i)<=0) cycle ! no class at this time
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
            thaligncenter//'Mon'//endth//thaligncenter//'Tue'//endth//thaligncenter//'Wed'//endth//&
            thaligncenter//'Thu'//endth//thaligncenter//'Fri'//endth//thaligncenter//'Sat'//endth//&
            endtr
        do ncol=1,56,period
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
        return
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

        write(device,AFORMAT) '<!-- '//'list_sections_to_edit()'//' -->'

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
                thalignleft//'Block'//endth//thalignleft//'Seats'//endth//thalignleft//'Day'//endth//&
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
                        begintd//trim(Section(sdx)%BlockID)//endtd// & ! BlockID
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

                    if (target_fn==fnRoomSchedule .or. target_fn==fnTeacherSchedule) then
                        if ( permitted ) then
                            write(device,AFORMAT) trim(make_href(target_fn, target_action, &
                                A1=target_name, A2=target_action, A3=QUERY_put, &
                                pre=begintd//'<small>', post='</small>'//endtd//endtr))
                        else
                            write(device,AFORMAT) tdnbspendtd//endtr
                        end if
                    elseif (target_fn==fnChangeMatriculation .or. target_fn==fnBlockEditSection) then
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
                if (target_fn==fnRoomSchedule .or. target_fn==fnTeacherSchedule) then
                    if ( permitted ) then
                        write(device,AFORMAT) trim(make_href(target_fn, target_action, &
                            A1=target_name, A2=target_action, A3=QUERY_put, &
                            pre=begintd//'<small>', post='</small>'//endtd//endtr))
                    else
                        write(device,AFORMAT) tdnbspendtd//endtr
                    end if
                elseif (target_fn==fnChangeMatriculation .or. target_fn==fnBlockEditSection) then
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

        return
    end subroutine list_sections_to_edit

  
    subroutine links_to_students (device, fn)
        integer, intent (in) :: device, fn

        integer :: ldx, n_count, tdx, std, ierr, sect, ncol
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character(len=127) :: header
        character(len=1) :: ch

        write(device,AFORMAT) '<!-- '//'links_to_students()'//' -->'

        ! collect students
        n_count = 0
        select case (fn)

            case (fnStudentsByProgram)
                ! which program ?
                call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
                targetCurriculum = 0
                do ldx=1,NumCurricula
                    if (CurrProgCode(ldx) /= tCurriculum) cycle
                    targetCurriculum = ldx ! trigger links to this curriculum's college
                    exit
                end do
                do tdx=1,NumStudents
                    std = StdRank(tdx)
                    if (CurrProgCode(Student(std)%CurriculumIdx) /= tCurriculum) cycle
                    n_count = n_count+1
                    tArray(n_count) = std
                end do
                header = 'Students in '//tCurriculum

            case (fnStudentsByCurriculum)
                ! which Curriculum ?
                call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
                targetCurriculum = index_to_curriculum(tCurriculum)
                do tdx=1,NumStudents
                    std = StdRank(tdx)
                    if (Student(std)%CurriculumIdx /= targetCurriculum) cycle
                    n_count = n_count+1
                    tArray(n_count) = std
                end do
                header = 'Students in '//tCurriculum

            case (fnStudentsByname)
                ! which college ?
                call cgi_get_named_string(QUERY_STRING, 'A2', ch, ierr)
                call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
                targetCollege = index_to_college(tCollege)
                do tdx=1,NumStudents
                    std = StdRank(tdx)
                    if (Curriculum(Student(std)%CurriculumIdx)%CollegeIdx /= targetCollege) cycle
                    if (Student(std)%Name(1:1) /= ch) cycle
                    n_count = n_count+1
                    tArray(n_count) = std
                end do
                header = '"'//ch//'" students in '//tCollege

            case (fnStudentsByYear)
                    ! which college ?
                    call cgi_get_named_string(QUERY_STRING, 'A2', tYear, ierr)
                    call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
                    targetCollege = index_to_college(tCollege)
                    sect = len_trim(tYear)
                    do tdx=1,NumStudents
                       std = StdRank(tdx)
                       if (Curriculum(Student(std)%CurriculumIdx)%CollegeIdx /= targetCollege) cycle
                       if (Student(std)%StdNo(1:sect) /= tYear(:sect)) cycle
                       n_count = n_count+1
                       tArray(n_count) = std
                    end do
                    header = '"'//tYear//'" students in '//tCollege

        end select

        call html_write_header(device, header)

        if (n_count == 0) then
            write(device,AFORMAT) '(None?)'
        else
            write(device,AFORMAT) '<table border="0" width="100%">'
            do tdx=1,n_count
                std = tArray(tdx)
                ldx = Student(std)%CurriculumIdx
                write(device,AFORMAT) begintr//begintd//trim(itoa(tdx))//'.'//endtd//begintd//Student(std)%StdNo//endtd, &
                    begintd//trim(Student(std)%Name)//endtd//begintd//Curriculum(ldx)%Code//endtd//begintd

                if (isRoleGuest) then
                    ! do not provide student info to Guest
                else
                    ncol = 0 ! how many enlisted subjects
                    do sect=1,Preenlisted(std)%lenSubject
                        if (Preenlisted(std)%Section(sect)>0) ncol=ncol+1
                    end do
                    if (ncol>0) then
                        write(device,AFORMAT) trim(make_href(fnChangeMatriculation, 'schedule', &
                            A1=Student(std)%StdNo, &
                            pre=' [ ', post=' ]'))
                    end if
                    write(device,AFORMAT) trim(make_href(fnEditCheckList, 'checklist', &
                        A1=Student(std)%StdNo, &
                        pre=' [ ', post=' ]'))
                    write(device,AFORMAT) trim(make_href(fnStudentPerformance, 'performance', &
                        A1=Student(std)%StdNo, &
                        pre=' [ ', post=' ]'))
                end if

                write(device,AFORMAT) endtd//endtr
            end do
            write(device,AFORMAT) '</table>'
        end if
        write(device,AFORMAT) '<hr>'

        return
    end subroutine links_to_students


    subroutine list_students(device, n_count, tArray, crse, Preenlisted)
        integer, intent (in) :: device, n_count, tArray(n_Count), crse
        type (TYPE_PRE_ENLISTMENT), intent(in) :: Preenlisted(0:)
        integer :: ldx, tdx, std, ncol
        character (len=MAX_LEN_SUBJECT_CODE) :: tNum

        write(device,AFORMAT) '<!-- '//'list_students()'//' -->'

        if (n_count == 0) then
            write(device,AFORMAT) '<br>None?<br>'
        else
            write(device,AFORMAT) '<table border="0" width="100%">', &
                begintr//thalignleft//'Count'//endth// &
                beginth//'Contrib'//endth// &
                thalignleft//'STD NO'//endth// &
                thalignleft//'NAME'//endth// &
                thalignleft//'CURRICULUM'//endth// &
                thalignleft//'LINKS'//endth// &
                endtr
            do tdx=1,n_count
                std = tArray(tdx)
                ldx = Student(std)%CurriculumIdx
                ! find contribution
                do ncol=1,Preenlisted(std)%NPriority+Preenlisted(std)%NAlternates+Preenlisted(std)%NCurrent
                    if (crse==Preenlisted(std)%Subject(ncol)) exit
                end do
                write(tNum, '(f6.4)') Preenlisted(std)%Contrib(ncol)

                write(device,AFORMAT) begintr// &
                    tdaligncenter//trim(itoa(tdx))//'.'//endtd// &
                    begintd//trim(tNum)//endtd// &
                    begintd//Student(std)%StdNo//endtd// &
                    begintd//trim(Student(std)%Name)//endtd// &
                    begintd//trim(Curriculum(ldx)%Code)//endtd//begintd

                if (isRoleGuest) then
                    ! do not provide student info to Guest
                else
                    if (ncol>0 .and. fnAvailable(fnChangeMatriculation)) &
                        write(device,AFORMAT) trim(make_href(fnChangeMatriculation, 'schedule', &
                            A1=Student(std)%StdNo, &
                            pre=' [ ', post=' ]'))
                    if (fnAvailable(fnEditCheckList)) &
                        write(device,AFORMAT) trim(make_href(fnEditCheckList, 'checklist', &
                            A1=Student(std)%StdNo, &
                            pre=' [ ', post=' ]'))
                    write(device,AFORMAT) trim(make_href(fnStudentPerformance, 'performance', &
                        A1=Student(std)%StdNo, &
                        pre=' [ ', post=' ]'))
                end if

                write(device,AFORMAT) endtd//endtr
            end do
            write(device,AFORMAT) '</table>'
        end if

        return
    end subroutine list_students


    subroutine class_list (device, NumSections, Section, Preenlisted)

        integer, intent (in) :: device, NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in) :: Preenlisted(0:)

        integer :: ldx, n_count, tdx, std, ierr, sect, ncol, crse
        character(len=MAX_LEN_CLASS_ID) :: tClassId

        write(device,AFORMAT) '<!-- '//'class_list()'//' -->'

        ! which section?
        call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, ierr)
        targetSection = index_to_section(tClassId, NumSections, Section)
        crse = Section(targetSection)%SubjectIdx
        targetDepartment = Subject(crse)%DeptIdx

        ! collect students
        n_count = 0
        do tdx=1,NumStudents
            std = StdRank(tdx)
            do ncol=1,Preenlisted(std)%lenSubject
                sect = Preenlisted(std)%Section(ncol)
                if (sect==0) cycle
                if (targetSection == sect) then
                    n_count = n_count+1
                    tArray(n_count) = std
                    exit
                elseif (Preenlisted(std)%Subject(ncol)==crse .and. is_lecture_lab_subject(crse)) then
                    ldx = index(Section(sect)%ClassId,DASH)
                    if (ldx>0) then ! student is accommodated in a lab section
                        if (trim(tClassId)==Section(sect)%ClassId(:ldx-1)) then ! lab of lecture
                            n_count = n_count+1
                            tArray(n_count) = std
                        end if
                    end if
                end if
            end do
        end do

        call html_write_header(device, 'Students in '//Section(targetSection)%ClassId)

        if (n_count == 0) then
            write(device,AFORMAT) '(None?)'
        else
            write(device,AFORMAT) '<table border="0" width="100%">'
            do tdx=1,n_count
                std = tArray(tdx)
                ldx = Student(std)%CurriculumIdx
                write(device,AFORMAT) begintr//begintd//trim(itoa(tdx))//'.'//endtd//begintd//Student(std)%StdNo//endtd, &
                    begintd//trim(Student(std)%Name)//endtd//begintd//Curriculum(ldx)%Code//endtd//begintd

                if (isRoleGuest) then
                    ! do not provide student info to Guest
                else
                    ncol = 0 ! how many enlisted subjects
                    do sect=1,Preenlisted(std)%lenSubject
                        if (Preenlisted(std)%Section(sect)>0) ncol=ncol+1
                    end do
                    if (ncol>0) then
                        write(device,AFORMAT) trim(make_href(fnChangeMatriculation, 'schedule', &
                            A1=Student(std)%StdNo, &
                            pre=' [ ', post=' ]'))
                    end if
                    write(device,AFORMAT) trim(make_href(fnEditCheckList, 'checklist', &
                        A1=Student(std)%StdNo, &
                        pre=' [ ', post=' ]'))
                    write(device,AFORMAT) trim(make_href(fnStudentPerformance, 'performance', &
                        A1=Student(std)%StdNo, &
                        pre=' [ ', post=' ]'))
                end if

                write(device,AFORMAT) endtd//endtr
            end do
            write(device,AFORMAT) '</table>'
        end if
        write(device,AFORMAT) '<hr>'

        return
    end subroutine class_list


    subroutine html_write_header(device, header, errmsg)
        integer, intent (in) :: device
        character(len=*), intent (in) :: header
        character(len=*), intent (in), optional :: errmsg
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        integer :: fdx, gdx, nItems !, cdx
        character (len=80) :: description

        write(device,AFORMAT) '<!-- html_write_header('//trim(header)//') -->'
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
            begintr//begintd//'<h2>'//trim(UniversityCode)//SPACE//PROGNAME//FSLASH//trim(ACTION)// &
            trim(description)//'</h2>'//endtd, &
            '<td align="right" valign="top"><small>User is '

            if (USERNAME/=ROLE) then
                write(device,AFORMAT) trim(USERNAME)//'/'//trim(ROLE)//'.'
            else
                write(device,AFORMAT) trim(USERNAME)//'.'
            endif

        ! primary links for Department Chair
        if (isRoleChair) then
            if (REQUEST/=fnLogin) then
                call info_department(device, Department(DeptIdxUser)%Code)
            end if

        ! primary links for Registration Adviser
        else if (isRoleSRE) then
            if (REQUEST/=fnCurriculum) then
                write(device,AFORMAT) trim(make_href(fnCurriculumList, 'Curriculum', &
                    A1=CurrProgCode(CurriculumIdxUser), pre=nbsp))
            end if

        end if

        ! link to other links
        tCollege = College(Department(DeptIdxUser)%CollegeIdx)%Code
        write(device,AFORMAT) trim(make_href(fnCollegeLinks, &
            tCollege, A1=tCollege, pre=nbsp))

        ! Logout for all users
        write(device,AFORMAT) trim(make_href(fnLogout, 'Logout', pre=nbsp))

#if defined PRODUCTION
        ! no Stop link
#else
        if (isRoleAdmin) write(device,AFORMAT) &
            trim(make_href(fnStop, 'Stop '//PROGNAME, pre=nbsp))
#endif

        ! end of line 1
        write(device,AFORMAT) '</small>'//endtd//endtr

        ! line 2 for banner: higher level context for selected function
        write(device,AFORMAT) begintr//'<td colspan="2"><small>'

        ! show student options ?
        if (targetStudent>0 .and. &
            (IsRoleAdmin .or. &
            (IsRoleSRE .and. CurrProgCode(Student(targetStudent)%CurriculumIdx)==CurrProgCode(CurriculumIdxUser))) ) then

            nItems = 0 ! how many enlisted subjects
            do fdx=1,Preenlisted(targetStudent)%lenSubject
                if (Preenlisted(targetStudent)%Section(fdx)>0) nItems=nItems+1
            end do

            tStdNo = Student(targetStudent)%StdNo
            write(device,AFORMAT) '[ <b>'//trim(tStdNo)//'</b>'

            !if (NumEnlistmentRecords>0 .and. nItems==0 .and. REQUEST/=fnFindBlock) then
            if (.not. advisingPeriod .and. nItems==0 .and. REQUEST/=fnFindBlock) then
                write(device,AFORMAT) trim(make_href(fnFindBlock, 'Find block', &
                    A1=tStdNo, pre=nbsp))
            end if

            !if (NumEnlistmentRecords>0 .and. nItems>0 .and. REQUEST/=fnChangeMatriculation) then
            if (.not. advisingPeriod .and. nItems>0 .and. REQUEST/=fnChangeMatriculation) then
                write(device,AFORMAT) trim(make_href(fnChangeMatriculation, 'Schedule', &
                    A1=tStdNo, pre=nbsp))
            end if

            if (REQUEST/=fnEditCheckList) then
                write(device,AFORMAT) trim(make_href(fnEditCheckList, 'Checklist', &
                    A1=tStdNo, pre=nbsp))
            end if

            if (REQUEST/=fnStudentPerformance) then
                write(device,AFORMAT) trim(make_href(fnStudentPerformance, 'Performance', &
                    A1=tStdNo, pre=nbsp))
            end if

            write(device,AFORMAT) ' ] '//nbsp

        end if

        ! students of curriculum ?
        if (targetCurriculum>0) then
            if (REQUEST/=fnCollegeLinks .and. REQUEST/=fnStudentsByProgram) then
                write(device,AFORMAT) trim(make_href(fnStudentsByProgram, 'students', &
                    A1=CurrProgCode(targetCurriculum), &
                    pre='[ <b>'//trim(CurrProgCode(targetCurriculum))//'</b>'//SPACE, post=' ] '//nbsp))
            end if
        end if

        ! show department options ?
        if (targetDepartment>0 .and. DeptIdxUser/=targetDepartment .and. REQUEST/=fnLogin) then
            tDepartment = Department(targetDepartment)%Code
            write(device,AFORMAT) '[ <b>'//trim(tDepartment)//'</b>'
            call info_department(device, tDepartment)
            write(device,AFORMAT) ' ] '//nbsp
        end if

        write(device,AFORMAT) '[ <b>Colleges:</b>'
        do gdx = 1,NumColleges
            if (.not. College(gdx)%hasInfo) cycle
            ! fnCollegeLinks requested already printed above?
            if (REQUEST==fnCollegeLinks .and. gdx==targetCollege) cycle
            write(device,AFORMAT) trim(make_href(fnCollegeLinks, College(gdx)%Code, &
                A1=College(gdx)%Code, pre=nbsp))
        end do
        write(device,AFORMAT) ' ]</small>'//endtd//endtr

        ! line 3 of banner, if any
        if (present(errmsg)) then
            if (errmsg/=SPACE) write(device,AFORMAT) begintr//'<td colspan="2">'// &
                red//'<i>'//trim(errmsg)//'</i>'//black//endtd//endtr
        end if
        write(device,AFORMAT) '</table><hr>'

        ! start of body
        if (len_trim(header)>0) write(device,AFORMAT) '<h3>'//trim(header)// &
            trim(termDescription)//'</h3>'

        return
    end subroutine html_write_header


    subroutine html_write_footer(device)

        integer, intent(in) :: device

        if (REQUEST==fnDownloadXML) return

        write(device,AFORMAT) '<!-- '//'html_write_footer()'//' -->'

        if (REQUEST/=fnStop .and. &
            REQUEST/=fnLogout .and. &
            REQUEST/=fnChangePassword .and. &
            REQUEST/=fnPrintableWorkload .and. &
            REQUEST/=fnPrintableSchedule) then

            write(device,AFORMAT) &
                '<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
                begintr//begintd// &
                '<small><i>Generated '//currentDate(1:4)//FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8)// &
                DASH//currentTime(1:2)//':'//currentTime(3:4)//' by '//PROGNAME//VERSION//'.'
            if (.not. isRoleAdmin) write(device,AFORMAT) &
                nbsp//'Please report errors to the Registrar.'
            ! Change password, logout
            write(device,AFORMAT) '</i></small>'//endtd//tdalignright//'<small>'
            if (.not. isSuspended) then
                write(device,AFORMAT) 'User is '//trim(USERNAME)//'.'
                if (USERNAME/=GUEST) write(device,AFORMAT) &
                    trim(make_href(fnChangePassword, 'Change password', pre=nbsp))
                write(device,AFORMAT) &
                    trim(make_href(fnLogout, 'Logout', pre=nbsp))
            end if
            write(device,AFORMAT) '</small>'//endtd//endtr//'</table>'

            if (noWrites) then ! training mode
                write(device,AFORMAT) &
                    '<small><i>'//red//'The program is in read-only mode. Changes to data will be lost on exit.'// &
                    black//'</i></small>'
            end if
            if (isSuspended) then
                write(device,AFORMAT)  &
                    '<small><i>'//red//'The program is in suspend-mode. Non-''Admin'' roles are locked out.'// &
                    black//'</i><small>'
            end if
        end if

        write(device,AFORMAT) '</small></body></html>'

        return
    end subroutine html_write_footer


    subroutine html_college_links(device, given, mesg)

        integer, intent(in) :: device
        integer, intent(in), optional :: given
        character(len=*), intent(in), optional :: mesg

        integer :: cdx
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege

        write(device,AFORMAT) '<!-- '//'html_college_links()'//' -->'

        if (present(given)) then
            targetCOLLEGE = given
            cdx = 0
        else
            call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, cdx)
            targetCOLLEGE = index_to_college(tCollege)
        end if

        if (cdx/=0 .or. targetCOLLEGE==0) then ! force to select a college
            call html_write_header(device, 'Select a college', mesg)
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

        return
    end subroutine html_college_links


    subroutine info_department(device, tDepartment)
        integer, intent(in) :: device
        character(len=MAX_LEN_DEPARTMENT_CODE), intent (in) :: tDepartment


        write(device,AFORMAT) '<!-- '//'info_department()'//' -->'

        if (REQUEST/=fnSubjectList) then
            write(device,AFORMAT) trim(make_href(fnSubjectList, 'Subjects', &
                A1=tDepartment, pre=nbsp))
        end if

        if (REQUEST/=fnTeachersByDept) then
            write(device,AFORMAT) trim(make_href(fnTeachersByDept, 'Teachers', &
                A1=tDepartment, pre=nbsp))
        end if

        if (REQUEST/=fnRoomList) then
            write(device,AFORMAT) trim(make_href(fnRoomList, 'Rooms', &
                A1=tDepartment, pre=nbsp))
        end if

        if (REQUEST/=fnDemandForSubjects .and. NumPredictionRecords>0 .and. targetTerm==nextTerm) then
            write(device,AFORMAT) trim(make_href(fnDemandForSubjects, 'Demand', &
                A1=tDepartment, pre=nbsp))
        end if

        if (REQUEST/=fnScheduleOfClasses .and. targetTerm>0) then
            write(device,AFORMAT) trim(make_href(fnScheduleOfClasses, 'Classes', &
                A1=tDepartment, pre=nbsp))
        end if

        return
    end subroutine info_department


    subroutine html_college_info(device, coll)
        integer, intent(in) :: device
        integer, intent(in) :: coll
        integer :: tdx, rdx, ldx, cdx, dept, n_curr, n_count, std, tLen
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character (len=4) :: tYear
        character (len=1) :: ch
        character (len=80) :: description
        logical :: addHR

        write(device,AFORMAT) '<!-- '//'html_college_info()'//' -->'

        tCollege = College(coll)%Code

        ! any curricular programs
        n_curr = 0
        do cdx=1,NumCurricula-1
            if (Curriculum(cdx)%CollegeIdx /= coll) cycle
            n_curr = n_curr+1
            exit
        end do

        ! subject areas in college
        tLen = 0
#if defined UPLB
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

        ! start of body
        write(device,AFORMAT) '<ul>'
        if (coll==NumColleges .and. .not. isRoleGuest) then
            write(device,AFORMAT) '<li><b>Download .XML files.</b> '//&
                'Right-click, then "Save Link As..." to '//trim(dirXML)//trim(pathToYear), &
                trim(make_href(fnDownloadXML, 'UNIVERSITY.XML', A1='UNIVERSITY.XML',  pre='<small><br>')), &
                trim(make_href(fnDownloadXML, 'COLLEGES.XML', A1='COLLEGES.XML',  pre=nbsp)), &
                trim(make_href(fnDownloadXML, 'DEPARTMENTS.XML', A1='DEPARTMENTS.XML',  pre=nbsp)), &
                trim(make_href(fnDownloadXML, 'SUBJECTS.XML', A1='SUBJECTS.XML',  pre=nbsp)), &
                trim(make_href(fnDownloadXML, 'FAILRATES.XML', A1='FAILRATES.XML',  pre=nbsp)), &
                trim(make_href(fnDownloadXML, 'CURRICULA.XML', A1='CURRICULA.XML',  pre=nbsp)), &
                trim(make_href(fnDownloadXML, 'EQUIVALENCIES.XML', A1='EQUIVALENCIES.XML',  pre=nbsp)), &
                trim(make_href(fnDownloadXML, 'ROOMS.XML', A1='ROOMS.XML',  pre=nbsp)), &
                trim(make_href(fnDownloadXML, 'TEACHERS.XML', A1='TEACHERS.XML', pre=nbsp, &
                post='</small>'))
        end if

        if (isRoleAdmin .and. coll==CollegeIdxUSER) then

            if (noWrites) then ! training mode
                write(device,AFORMAT) trim(make_href(fnToggleTrainingMode, 'Turn it OFF', &
                    pre='<li><b>Training-mode is '//red//'ON'//black//'</b>. ', post='</li>'))
            else
                write(device,AFORMAT) trim(make_href(fnToggleTrainingMode, 'Turn it ON', &
                    pre='<li><b>Training-mode is '//green//'OFF'//black//'</b>. ', post='</li>'))
            end if

            if (isSuspended) then ! suspended mode
                write(device,AFORMAT) trim(make_href(fnSuspendProgram, 'Turn it OFF', &
                    pre='<li><b>Suspend-mode is '//red//'ON'//black//'</b>. ', post='</li>'))
            else
                write(device,AFORMAT) trim(make_href(fnSuspendProgram, 'Turn it ON', &
                    pre='<li><b>Suspend-mode is '//green//'OFF'//black//'</b>. ', post='</li>'))
            end if
            write(device,AFORMAT) trim(make_href(fnEditSignatories, 'signatories', &
                pre='<li><b>Edit '//nbsp, post=nbsp//'in teaching load form</b></li>'))

        end if

        if (coll==NumColleges) then

            if (NumStudents>0) then
                write(device,AFORMAT) trim(make_href(fnStudentsDistribution, 'Distribution', &
                    A1=ADMINISTRATION, pre='<li><b>', post=' of students in '//trim(UniversityCode)//'</b>'))
                write(device,AFORMAT) trim(make_href(fnStudentAddPrompt, 'Add', &
                    A1=tCollege, pre=' (', post=' a student)</li>'))
            end if

            if (NumEnlistmentRecords>0) then

                write(device,AFORMAT) '<li><b>Summary of overall enlistment</b><ul>'
                write(device,AFORMAT) trim(make_href(fnBottleneck, 'demand > available seats', &
                    A1=tCollege, pre='<li>Top 100 subjects for which ', post='</li>'))
                write(device,AFORMAT) trim(make_href(fnExtraSlots, 'available seats > demand', &
                    A1=tCollege, pre='<li>Top 100 subjects for which ', post='</li>'))
                write(device,AFORMAT) trim(make_href(fnUnderloadSummary, 'underloads', &
                    A1=tCollege, pre='<li>Summary of ', post='</li>'))
                write(device,AFORMAT) '</ul></li>'
            end if

            if (NumStudents+NumEnlistmentRecords>0) write(device,AFORMAT) '<hr>'

        end if

        addHR = n_curr>0
        if (addHR) then
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
                do ldx=cdx+1,NumCurricula
                    if (CurrProgCode(ldx) == CurrProgCode(cdx)) done(ldx) = .true.
                end do
            end do
            write(device,AFORMAT) '</li>'
        end if

        ! subjects
        call links_to_subjects(device, coll, tLen, tArray(1))

        if (addHR) write(device,AFORMAT) '<hr>'

        ! teachers
        n_count = 0
        do tdx=1,NumTeachers+NumAdditionalTeachers
            if (Department(Teacher(tdx)%DeptIdx)%CollegeIdx /= coll) cycle
            n_count = n_count+1
            exit
        end do
        addHR = n_count>0
        if (addHR) then
            write(device,AFORMAT) '<!-- '//'teacher links()'//' -->', &
                '<li><b>Teachers by department</b> : '
            do dept=2,NumDepartments
                if (Department(dept)%CollegeIdx /= coll) cycle
                n_count = 0
                do tdx=1,NumTeachers+NumAdditionalTeachers
                    if (Teacher(tdx)%DeptIdx /= dept) cycle
                    if (trim(Teacher(tdx)%Role)==trim(REGISTRAR)) cycle
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
                    if (trim(Teacher(tdx)%Role)==trim(REGISTRAR)) cycle
                    n_count = n_count+1
                end do
                if (n_count==0) cycle
                write(device,AFORMAT) trim(make_href(fnTeachersByName, ch, &
                    A1=tCollege, A2=ch, pre=nbsp, post='('//trim(itoa(n_count))//')'))
            end do
            write(device,AFORMAT) '</li>'
        end if

        ! rooms
        n_count = 0
        do rdx=1,NumRooms+NumAdditionalRooms
            if (Department(Room(rdx)%DeptIdx)%CollegeIdx /= coll) cycle
            n_count = n_count+1
            exit
        end do
        if (n_count>0) then
            addHR = .true.
            write(device,AFORMAT) '<!-- '//'room links()'//' -->', &
                '<li><b>Rooms by department</b> : '
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

        if (addHR) write(device,AFORMAT) '<hr>'

        ! students
        n_count = 0
        do std=1,NumStudents
            if (Curriculum(Student(std)%CurriculumIdx)%CollegeIdx /= coll) cycle
            n_count = n_count+1
            exit
        end do
        if (n_count>0) then
            write(device,AFORMAT) '<!-- '//'student links()'//' -->', &
                '<li><b>Students by last name</b> : '
            do cdx=iachar('A'), iachar('Z')
                ch = achar(cdx)
                n_count = 0
                do std=1,NumStudents
                    if (Curriculum(Student(std)%CurriculumIdx)%CollegeIdx /= coll) cycle
                    if (Student(std)%Name(1:1) /= ch) cycle
                    n_count = n_count+1
                  !exit
                end do
                if (n_count > 0) then
                    write(device,AFORMAT) trim(make_href(fnStudentsByName, ch, &
                        A1=tCollege, A2=ch, &
                        post='('//trim(itoa(n_count))//')'//nbsp))
                !else
                !  write(device,AFORMAT) ch//nbsp
                end if
            end do
            write(device,AFORMAT) '</li><li><b>Students by number</b> : '
            do ldx=len_trim(StdNoPrefix),1,-1
                if (StdNoPrefix(ldx:ldx)/=':') cycle ! do ldx=
                cdx = ldx-1
                if (cdx==0) exit ! do ldx=
                do while (cdx>0)
                    if (StdNoPrefix(cdx:cdx)/=':') then
                        cdx = cdx-1
                    else
                        exit ! while (cdx>0)
                    end if
                end do
                tYear = StdNoPrefix(cdx+1:ldx-1)
                cdx = len_trim(tYear)
                n_count = 0
                do std=1,NumStudents
                    if (Curriculum(Student(std)%CurriculumIdx)%CollegeIdx/=coll) cycle
                    if (tYear(:cdx)==Student(std)%StdNo(:cdx)) n_count = n_count+1
                end do
                if (n_count == 0) cycle
                write(device,AFORMAT) trim(make_href(fnStudentsByYear, tYear, &
                    A1=tCollege, A2=tYear, &
                    post='('//trim(itoa(n_count))//')'//nbsp))
            end do
            write(device,AFORMAT) '</li><li><b>Students by curriculum</b> : '
            done = .false.
            do cdx=1,NumCurricula
                if (Curriculum(cdx)%CollegeIdx /= coll) cycle
                if (done(cdx)) cycle
                n_count = 0
                do std=1,NumStudents
                    if (CurrProgCode(Student(std)%CurriculumIdx) /= CurrProgCode(cdx)) cycle
                    n_count = n_count+1
                  !exit
                end do
                if (n_count > 0) then
                    write(device,AFORMAT) trim(make_href(fnStudentsByProgram, CurrProgCode(cdx), &
                        A1=CurrProgCode(cdx), &
                        post='('//trim(itoa(n_count))//')'//nbsp))
                    do ldx=cdx+1,NumCurricula
                        if (CurrProgCode(ldx) == CurrProgCode(cdx)) done(ldx) = .true.
                    end do
                end if
            end do
            write(device,AFORMAT) trim(make_href(fnStudentsDistribution, 'Distribution among curricula', &
                A1=tCollege)), '</li><hr>'

        end if

        ! per term
        cdx = targetTerm ! remember targetTerm
        do ldx=termBegin,termEnd

            call qualify_term (ldx, rdx, targetTerm, description)
            write(device,AFORMAT) '<li><b>'//trim(description)//'</b><ul>'
            if (coll==NumColleges .and. .not. isRoleGuest) then
                write(device,AFORMAT) &
                    '<li><b>Download .XML files.</b> Right-click, then "Save Link As..." to '// &
                    trim(dirXML)//trim(itoa(rdx))//DIRSEP//trim(txtSemester(targetTerm))//DIRSEP, &
                    trim(make_href(fnDownloadXML, 'BLOCKS.XML', A1='BLOCKS.XML',  pre='<small><br>')), &
                    trim(make_href(fnDownloadXML, 'CLASSES.XML', A1='CLASSES.XML',  pre=nbsp, &
                    post='</small>'))
            end if

            ! demand for subjects next term
            if (targetTerm==nextTerm .and. NumPredictionRecords>0 .and. tLen>0) &
                call links_to_depts(device, coll, fnDemandForSubjects, '<b>Demand for subjects</b>')

            ! blocks
#if defined UPLB
#else
            if (n_curr>0) call links_to_blocks(device, coll, targetTerm)
#endif

            ! classes
            call links_to_sections(device, coll, tLen, tArray(1), targetTerm)

            ! enlistment summary
            if (targetTerm==currentTerm .and. NumEnlistmentRecords>0 .and. tLen>0) &
                call links_to_depts(device, coll, fnEnlistmentSummary, '<b>Summary of enlistment</b>')

            write(device,AFORMAT) '</ul></li>'
        end do
        targetTerm = cdx ! restore

        write(device,AFORMAT) '</ul><hr>'
        return
    end subroutine html_college_info


    subroutine links_to_depts(device, coll, fn, heading)
        integer, intent (in) :: device, coll, fn
        character(len=*), intent (in) :: heading
        integer :: dept, crse, n_count
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        write(device,AFORMAT) '<!-- '//'links_to_depts()'//' -->'

        write(device,AFORMAT) '<li>'//heading//' : '
        do dept=2,NumDepartments
            if (Department(dept)%CollegeIdx/=coll) cycle
            n_count = 0
#if defined UPLB
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

        return
    end subroutine links_to_depts


    subroutine links_to_subjects(device, coll, numAreas, AreaList)
        integer, intent (in) :: device, coll, numAreas
        integer, intent (in) :: AreaList(1:numAreas)
        integer :: dept, crse, n_count
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        if (numAreas==0) return

        write(device,AFORMAT) '<!-- '//'links_to_subjects()'//' -->'

        write(device,AFORMAT) '<li><b>Subjects</b> in :'
        do dept=2,NumDepartments
            if (Department(dept)%CollegeIdx /= coll) cycle
            n_count = 0
#if defined UPLB
            do crse=1,NumSubjects+NumAdditionalSubjects
                if (Subject(crse)%DeptIdx /= dept) cycle
                n_count = n_count+1
                !exit
            end do
#else
            ! Subjects administered by program
            do crse=1,NumSubjects+NumAdditionalSubjects
                if (.not. is_used_in_college_subject(coll, crse)) cycle
                n_count = n_count+1
                !exit
            end do
#endif
            if (n_count==0) cycle
            tDepartment = Department(dept)%Code
            write(device,AFORMAT) trim(make_href(fnSubjectList, tDepartment, A1=tDepartment, &
                pre=nbsp, post='('//trim(itoa(n_count))//')'))
        end do
        write(device,AFORMAT) '</li><li><b>Subjects by area</b> :'
        do dept=1,numAreas
            tSubject = SubjectArea(AreaList(dept))%Code
            n_count = SubjectArea(AreaList(dept))%Count
            write(device,AFORMAT) trim(make_href(fnSubjectList, tSubject, A1=tSubject, &
                pre=nbsp, post='('//trim(itoa(n_count))//')'))
        end do
        write(device,AFORMAT) '</li>'
        return
    end subroutine links_to_subjects


    subroutine links_to_sections(device, coll, numAreas, AreaList, term)
        integer, intent (in) :: device, coll, numAreas, term
        integer, intent (in) :: AreaList(1:numAreas)
        integer :: dept, crse, sect, n_count, m_count, mdx, k1, k2
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        if (numAreas==0) return

        write(device,AFORMAT) '<!-- '//'links_to_sections()'//' -->'

        tCollege = College(coll)%Code

        write(device,AFORMAT) '<li><b>Classes</b> in : '
        do dept=2,NumDepartments
            if (Department(dept)%CollegeIdx /= coll) cycle
            n_count = 0
#if defined UPLB
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
        write(device,AFORMAT) '</li><li><b>Classes by subject area</b> : '
        do dept=1,numAreas
            ! the code
            tSubject = SubjectArea(AreaList(dept))%Code
            k1 = len_trim(tSubject)+1
            n_count = 0 ! how many sections with this code currently open
            do sect=1,NumSections(term)
                if (Section(term,sect)%ClassId(:k1)==tSubject(:k1)) n_count = n_count+1
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

        write(device,AFORMAT) trim(make_href(fnTeacherConflicts, 'teachers', &
            A1=tCollege, pre='; <b>Conflicts in schedules of </b> '//nbsp))
        write(device,AFORMAT) trim(make_href(fnRoomConflicts, 'rooms', &
            A1=tCollege, pre=nbsp))

        QUERY_put = USERNAME
        write(device,AFORMAT) &
            trim(make_href(fnTeacherSchedule, 'schedule', &
                A1=QUERY_put, pre=' ; <b>My </b> '//nbsp)), &
            trim(make_href(fnPrintableWorkload, 'form', &
                A1=QUERY_put, pre=nbsp//'and teaching load'//nbsp))

        write(device,AFORMAT) '</li>'
        return
    end subroutine links_to_sections


    subroutine links_to_blocks(device, coll, term)
        integer, intent (in) :: device, coll, term
        integer :: cdx, ldx, blk, ncurr
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege

        write(device,AFORMAT) '<!-- '//'links_to_blocks()'//' -->'
        ncurr = 0
        tCollege = College(coll)%Code
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
            do ldx=cdx+1,NumCurricula
                if (CurrProgCode(ldx) == CurrProgCode(cdx)) done(ldx) = .true.
            end do
        end do
!        if (ncurr>0 .and. isRoleChair .or. isRoleAdmin) then
!            write(device,AFORMAT) trim(make_href(fnBlockNewSelect, 'Add', &
!                A1=tCollege, pre='<b> (', post=' block)</b>'))
!        end if
        write(device,AFORMAT) '</li>'

        return
    end subroutine links_to_blocks


    subroutine blocks_in_section(device, sect, fn, NumBlocks, Block)
        integer, intent(in) :: device, sect, fn, NumBlocks
        type (TYPE_BLOCK), dimension(0:), intent(in) :: Block
        integer :: idx, jdx

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
        return
    end subroutine blocks_in_section


    function make_href(fn, label, pre, post, A1, A2, A3, A4, A5, anchor, newtab)
    ! build HTML href, like:
    !   pre<a href="CGI_PATH?F=fn&A1=A1&...A5=A5 #anchor target=newtab">label</a>post

        integer, intent (in) :: fn
        character(len=*), intent (in) :: label
        character(len=*), intent (in), optional :: A1, A2, A3, A4, A5
        character(len=*), intent (in), optional :: pre, post, anchor, newtab

        character(len=MAX_LEN_QUERY_STRING) :: make_href
        character(len=MAX_CGI_WRK_LEN) :: cgi_wrk
        integer :: kStart

        ! random number
        real :: harvest

        ! the function and user name
        cipher = 'F='//trim(itoa(fn))//'&N='//USERNAME
        ! the term if specified
        if (targetTerm>0) then
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

        ! encrypt
        call random_number(harvest)
        kStart = MAX_LEN_QUERY_STRING/2 + int(harvest*MAX_LEN_QUERY_STRING/2)
        call encrypt(queryEncryptionKey(:kStart), cipher)

        ! begin href
        cipher = '<a href="'//trim(CGI_PATH)//'?q='//trim(cipher)//itoa(kStart)

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

        return
    end function make_href



    subroutine make_form_start(device, fn, A1, A2, A3, A4, A5)
    ! write to device the start an HTML form, like:
    !   <form name="input" method="post" action="/heeds">
    !   <input type="hidden" name="F" value="91">
    !   <input type="hidden" name="N" value="REGISTRAR">
    !   <input type="hidden" name="A1" value="CLINLAB">

        integer, intent (in) :: device, fn
        character(len=*), intent (in), optional :: A1, A2, A3, A4, A5

        character(len=MAX_CGI_WRK_LEN) :: cgi_wrk
        integer :: kStart

        ! random number
        real :: harvest

        ! the function and user name
        cipher = 'F='//trim(itoa(fn))//'&N='//USERNAME
        ! the term if specified
        if (targetTerm>0) then
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

        ! encrypt
        call random_number(harvest)
        kStart = MAX_LEN_QUERY_STRING/2 + int(harvest*MAX_LEN_QUERY_STRING/2)
        call encrypt(queryEncryptionKey(:kStart), cipher)

        write(device,AFORMAT) &
          '<form name="input" method="post" action="'//trim(CGI_PATH)//'">', &
          '<input type="hidden" name="q" value="'//trim(cipher)//trim(itoa(kStart))//'">'

        return
    end subroutine make_form_start

end module HTML
