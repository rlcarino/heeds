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
    fnChangeInitialPassword   =  2, & ! change initial password
    fnChangePassword          =  3, & ! change password
    fnStopUser                =  4, & ! logout user
    fnSearch                  =  5, & ! search for an object
    fnToggleTrainingMode      =  6, & ! toggle training mode
    fnCollegeLinks            =  7, & ! index to college info
    fnSubjectList             =  8, & ! view list of subjects administered by a department
    fnEditSubject             =  9, & ! edit subject
    fnCurriculumList          = 10, & ! view list of curricular programs administered by a college
    fnCurriculum              = 11, & ! view a curricular program
    fnEditCurriculum          = 12, & ! edit curriculum
    fnActivateCurriculum      = 13, & ! activate curriculum
    fnDeactivateCurriculum    = 14, & ! deactivate curriculum
    fnEditRoom                = 15, & ! edit room parameters
    fnEditTeacher             = 16, & ! edit teacher record
    !
    fnStudentsByProgram       = 17, & ! view list students in a program
    fnStudentsByCurriculum    = 18, & ! view list students in a curriculum
    fnStudentsByName          = 19, & ! view alphabetical list of students in a college
    fnStudentsByYear          = 20, & ! view list of students in college by year
    fnStudentsDistribution    = 21, & ! view distribution of students, by curriculum and by college
    !
    fnStudentAdd              = 22, & ! add a student
    fnStudentAddPrompt        = 23, & ! entry form for 'add a student'
    fnStudentPerformance      = 24, & ! view student performance
    fnEditCheckList           = 25, & ! display checklist for editing
    fnChangeMatriculation     = 26, & ! change matriculation
    fnFindBlock               = 27, & ! find a block for student
    ! = 28
    !
    fnScheduleOfClasses       = 31, & ! display schedule of classes for editing
    fnScheduleOfferSubject    = 32, & ! offer a subject
    fnScheduleAddLab          = 33, & ! add a lab section
    fnScheduleDelete          = 34, & ! delete a section
    fnScheduleEdit            = 35, & ! edit a section
    fnScheduleValidate        = 36, & ! check correctness of edit inputs
    fnTeachersByDept          = 37, & ! view list teachers belonging to a college
    fnTeachersByName          = 38, & ! view alphabetical list of teachers in a college
    fnTeacherSchedule         = 39, & ! view weekly schedule of a teacher
    fnRoomList                = 40, & ! view list rooms administered by a college
    fnRoomSchedule            = 41, & ! view weekly schedule of a room
    fnRoomConflicts           = 42, & ! view list of rooms with conflicts
    fnTeacherConflicts        = 43, & ! view list of teachers with conflicts
    fnTBARooms                = 44, & ! view list of sections with TBA rooms
    fnTBATeachers             = 45, & ! view list of sections with TBA teachers
    fnBlockSchedule           = 46, & ! display schedule of block section
    fnBlockEditName           = 47, & ! edit name of block section
    fnBlockDeleteName         = 48, & ! delete block, but keep its sections
    fnBlockDeleteAll          = 49, & ! delete block and its sections
    fnBlockNewSelect          = 50, & ! select parameters for a new block
    fnBlockNewAdd             = 51, & ! add a new block
    fnBlockCopy               = 52, & ! copy block
    fnBlockEditSection        = 53, & ! edit section in block
    fnBlockEditSubject        = 54, & ! update subjects in block
    fnScheduleByArea          = 55, & ! display schedule of classes for editing, by area
    fnPrintableWorkload       = 56, & ! printable teaching load
    ! = 56
    !
    fnEnlistmentSummary       = 61, & ! students not accommodated in a priority subject
    fnNotAccommodated         = 62, & ! students not accommodated in a priority subject
    fnBottleneck              = 63, & ! "bottleneck" subjects
    fnExtraSlots              = 64, & ! subjects with excess slots
    fnUnderloadSummary        = 65, & ! summary of underloading
    fnUnderloadedStudents     = 66, & ! underloaded students
    fnClassList               = 67, & ! view list of students in a class
    fnGradeSheet              = 68, & ! enter grades
    fnRebuildChecklists       = 69, & ! rebuild checklists from grades each semester
    ! = 60
    !
    fnNextScheduleOfClasses   = 81, & ! display next semester's schedule of classes for editing
    fnNextScheduleOfferSubject = 82, & ! offer a subject
    fnNextScheduleAddLab      = 83, & ! add a lab section
    fnNextScheduleDelete      = 84, & ! delete a section
    fnNextScheduleEdit        = 85, & ! edit a section
    fnNextScheduleValidate    = 86, & ! check correctness of edit inputs
    fnNextTeachersByDept      = 87, & ! view list teachers belonging to a college
    fnNextTeachersByName      = 88, & ! view alphabetical list of teachers in a college
    fnNextTeacherSchedule     = 89, & ! view weekly schedule of a teacher
    fnNextRoomList            = 90, & ! view list rooms administered by a college
    fnNextRoomSchedule        = 91, & ! view weekly schedule of a room
    fnNextRoomConflicts       = 92, & ! view list of rooms with conflicts
    fnNextTeacherConflicts    = 93, & ! view list of teachers with conflicts
    fnNextTBARooms            = 94, & ! view list of sections with TBA rooms
    fnNextTBATeachers         = 95, & ! view list of sections with TBA teachers
    fnNextBlockSchedule       = 96, & ! display schedule of block section
    fnNextBlockEditName       = 97, & ! edit name of block section
    fnNextBlockDeleteName     = 98, & ! delete block, but keep sections
    fnNextBlockDeleteAll      = 99, & ! delete block and its sections
    fnNextBlockNewSelect      =100, & ! select parameters for a new block
    fnNextBlockNewAdd         =101, & ! add a new block
    fnNextBlockCopy           =102, & ! copy block
    fnNextBlockEditSection    =103, & ! edit section in block
    fnNextBlockEditSubject    =104, & ! update subjects in block
    fnNextScheduleByArea      =105, & ! display schedule of classes for editing, by area
    fnNextPrintableWorkload   =106, & ! printable teaching load
    ! = 106
    !
    fnDemandFreshmen          = 111, & ! view demand for subjects by incoming students
    fnUpdateDemandFreshmen    = 112, & ! update no. of incoming students
    fnPrintableSchedule       = 113, & ! printable weekly timetable
    fnAdviseStudent           = 114, & ! generate Prediction()
    fnDemandForSubjects       = 115, & ! view demand for subjects
    fnPotentialStudents       = 116, & ! list of potential students of a subject
    !
    fnStopProgram             = 120    ! should be the last (used as array extent)

    integer, parameter :: fnNextOffset = fnNextScheduleOfClasses - fnScheduleOfClasses

    logical, dimension(0:fnStopProgram) :: available ! modified in set_feature_availability()
    ! assume all functions are available throughout the term

    ! the requested server function
    integer :: REQUEST

    ! the target of the fucntion
    integer :: targetCollege, targetDepartment, targetSubject, targetRoom, targetTeacher, &
        targetCurriculum, targetStudent, targetBlock, targetSection, targetLogin

    ! work arrays
    integer :: tArray(max(MAX_ALL_STUDENTS,2*MAX_ALL_SUBJECTS))
    character (len=80) :: QUERY_put, TermQualifier

    ! private tokens
    character (len=MAX_LEN_FILE_PATH), private :: fileName
    character (len=MAX_LEN_XML_LINE), private :: line
    integer, private :: unitNum=2, eof !, ndels, pos(30)


contains

    function fnDescription (fn)
        character(len=60) :: fnDescription
        integer, intent (in) :: fn

        select case (fn)
            case (fnLogin                   )
                    fnDescription = 'login'
            case (fnStopUser                )
                    fnDescription = 'logout'
            case (fnChangeInitialPassword   )
                    fnDescription = 'change initial password'
            case (fnChangePassword          )
                    fnDescription = 'change password'
            case (fnSearch                  )
                    fnDescription = 'search for an object'
            case (fnToggleTrainingMode      )
                    fnDescription = 'toggle training mode'
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
            case (fnBlockEditSection        )
                    fnDescription = 'edit section in block'
            case (fnBlockEditSubject        )
                    fnDescription = 'update subjects in block'
            case (fnScheduleByArea          )
                    fnDescription = 'display schedule of classes for editing, by area'
            case (fnPrintableWorkload       )
                    fnDescription = 'printable teaching load'
            case (fnEnlistmentSummary       )
                    fnDescription = 'students not accommodated in a priority subject'
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
            case (fnRebuildChecklists       )
                    fnDescription = 'rebuild checklists from grades each semester'
            case (fnNextScheduleOfClasses   )
                    fnDescription = 'display next semester''s schedule of classes for editing'
            case (fnNextScheduleOfferSubject)
                    fnDescription = 'offer a subject'
            case (fnNextScheduleAddLab      )
                    fnDescription = 'add a lab section'
            case (fnNextScheduleDelete      )
                    fnDescription = 'delete a section'
            case (fnNextScheduleEdit        )
                    fnDescription = 'edit a section'
            case (fnNextScheduleValidate    )
                    fnDescription = 'check correctness of edit inputs'
            case (fnNextTeachersByDept      )
                    fnDescription = 'view list teachers belonging to a college'
            case (fnNextTeachersByName      )
                    fnDescription = 'view alphabetical list of teachers in a college'
            case (fnNextTeacherSchedule     )
                    fnDescription = 'view weekly schedule of a teacher'
            case (fnNextRoomList            )
                    fnDescription = 'view list rooms administered by a college'
            case (fnNextRoomSchedule        )
                    fnDescription = 'view weekly schedule of a room'
            case (fnNextRoomConflicts       )
                    fnDescription = 'view list of rooms with conflicts'
            case (fnNextTeacherConflicts    )
                    fnDescription = 'view list of teachers with conflicts'
            case (fnNextTBARooms            )
                    fnDescription = 'view list of sections with TBA rooms'
            case (fnNextTBATeachers         )
                    fnDescription = 'view list of sections with TBA teachers'
            case (fnNextBlockSchedule       )
                    fnDescription = 'display schedule of block section'
            case (fnNextBlockEditName       )
                    fnDescription = 'edit name of block section'
            case (fnNextBlockDeleteName     )
                    fnDescription = 'delete block, but keep sections'
            case (fnNextBlockDeleteAll      )
                    fnDescription = 'delete block and its sections'
            case (fnNextBlockNewSelect      )
                    fnDescription = 'select parameters for a new block'
            case (fnNextBlockNewAdd         )
                    fnDescription = 'add a new block'
            case (fnNextBlockCopy           )
                    fnDescription = 'copy block'
            case (fnNextBlockEditSection    )
                    fnDescription = 'edit section in block'
            case (fnNextBlockEditSubject    )
                    fnDescription = 'update subjects in block'
            case (fnNextScheduleByArea      )
                    fnDescription = 'display schedule of classes for editing, by area'
            case (fnNextPrintableWorkload   )
                    fnDescription = 'printable teaching load'
            case (fnDemandFreshmen          )
                    fnDescription = 'view demand for subjects by incoming students'
            case (fnUpdateDemandFreshmen    )
                    fnDescription = 'update no. of incoming students'
            case (fnPrintableSchedule       )
                    fnDescription = 'printable weekly timetable'
            case (fnAdviseStudent           )
                    fnDescription = 'generate Prediction()'
            case (fnDemandForSubjects       )
                    fnDescription = 'view demand for subjects'
            case (fnPotentialStudents       )
                    fnDescription = 'list of potential students of a subject'
            case (fnStopProgram             )
                    fnDescription = 'stop program'

        end select
        return
    end function fnDescription

    subroutine set_feature_availability ()
        !
        !  Modify availability of functions based on data that is loaded
        !

        !  Relevant dates during (currentYear, currentTerm)
        !  Note: currentYear is the start year of the current Academic Year,
        !        currentTerm is First Semester or Second Semester or Summer
        !
        !    Date 1: First day of registration
        !    Date 2: Last day for late registration
        !    Date 3: Last day for submission of grades
        !
        !  Define:
        !    Period I=[Date 1, Date 2] (enlistment Period)
        !       - change matriculation
        !    Period II=(Date 2, Date 3] (mid-term)
        !       - REGD subjects are in TCG
        !       - probabilistic forecasts
        !    Period III=(Date 3,Date 1 of next term) (term break)
        !       - latest grades are available, in TCG
        !       - deterministic forecasts
        !

        available = .true. ! all functions are initially available; reset below
        available(0) = .false.

!        if (NumStudents==0) then
!            available(fnStudentsByProgram) = .false.
!            available(fnStudentsByCurriculum) = .false.
!            available(fnStudentsByName) = .false.
!            available(fnStudentsByYear) = .false.
!            available(fnStudentsDistribution) = .false.
!            available(fnEditCheckList) = .false.
!            available(fnStudentPerformance) = .false.
!        endif
!        if (NumStudents==0 .or. NumEnlistmentRecords==0) then
!            available(fnChangeMatriculation) = .false.
!            available(fnFindBlock) = .false.
!            available(fnBottleneck) = .false.
!            available(fnExtraSlots) = .false.
!            available(fnUnderLoadSummary) = .false.
!            available(fnUnderLoadedStudents) = .false.
!            available(fnClassList) = .false.
!            available(fnGradeSheet) = .false.
!            available(fnEnlistmentSummary) = .false.
!        endif
!        if (NumStudents==0 .or. NumPredictionRecords==0) then
!            available(fnDemandForSubjects) = .false.
!            available(fnPotentialStudents) = .false.
!        endif
!
!        if (NumStudents==0 .or. NumPredictionRecords>0) then
!            available(fnAdviseStudent) = .false.
!        endif
!
!        if (NumTeachers<=1) then
!            available(fnTeachersByDept) = .false.
!            available(fnTeachersByName) = .false.
!            available(fnTeacherSchedule) = .false.
!        endif
!
!        if (NumRooms<=1) then
!            available(fnRoomList) = .false.
!            available(fnRoomSchedule) = .false.
!        endif
!
!        if (NumCurrentSections==0) then
!            available(fnScheduleOfClasses) = .false.
!            available(fnTeacherSchedule) = .false.
!            available(fnBlockSchedule) = .false.
!            available(fnRoomSchedule) = .false.
!            available(fnRoomConflicts) = .false.
!            available(fnTeacherConflicts) = .false.
!            available(fnTBARooms) = .false.
!            available(fnTBATeachers) = .false.
!            available(fnFindBlock) = .false.
!        endif
!
!        if (NumCurricula==0) then
!            available(fnCurriculumList) = .false.
!            available(fnCurriculum) = .false.
!        endif
!
!        if (Period==1 .or. NumNextSections==0) then
!            available(fnNextScheduleOfClasses) = .false.
!            available(fnNextScheduleOfferSubject) = .false.
!            available(fnNextScheduleAddLab) = .false.
!            available(fnNextScheduleDelete) = .false.
!            available(fnNextScheduleEdit) = .false.
!            available(fnNextScheduleValidate) = .false.
!            available(fnNextTeachersByDept) = .false.
!            available(fnNextTeachersByName) = .false.
!            available(fnNextTeacherSchedule) = .false.
!            available(fnNextRoomList) = .false.
!            available(fnNextRoomSchedule) = .false.
!            available(fnNextBlockSchedule) = .false.
!            available(fnNextRoomConflicts) = .false.
!            available(fnNextTeacherConflicts) = .false.
!            available(fnNextTBARooms) = .false.
!            available(fnNextTBATeachers) = .false.
!        end if

        if (Period==1) then
            ! disallow advising
            available(fnAdviseStudent) = .false.
            ! allow enlistment
            available(fnChangeMatriculation) = .true.
            available(fnFindBlock) = .true.
            available(fnBottleneck) = .true.
            available(fnExtraSlots) = .true.
            available(fnUnderLoadSummary) = .true.
            available(fnUnderLoadedStudents) = .true.
            available(fnClassList) = .true.
            available(fnGradeSheet) = .true.
            available(fnEnlistmentSummary) = .true.
        else ! if (Period/=1) then
            available(fnChangeMatriculation) = .false.
            available(fnFindBlock) = .false.
        end if

        ! deactivate "Advise all students"
        available(fnAdviseStudent) = .false.
        ! deactivate "Rebuild all individual COGs"
        available(fnRebuildChecklists) = .false.

        ! any freshman INTAKE data?
        available(fnDemandFreshmen) = sum(NFintake)>0

        ! university-specific customizations

        return
    end subroutine set_feature_availability


    subroutine html_copyright(device)
        integer, intent(in) :: device

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

        write(device,AFORMAT) &
            '<html><head><title>'//PROGNAME//VERSION//'</title></head><body>'

        if (len_trim(mesg)>0) then
            write(device,aformat) &
                '<h1>'//trim(UniversityCode)//nbsp//PROGNAME//'</h1>', &
                red//trim(mesg)//black
            if (isRoleAdmin)  &
                write(device,AFORMAT)'<br><br><a href="'//CGI_PATH//'">Verify</a>'
                !write(device,AFORMAT)'<br><br><a href="/">Verify</a>'

        else
            write(device,AFORMAT) &
                '<h1>Welcome to '//trim(UniversityCode)//nbsp//PROGNAME//' !</h1><hr>', &
                '<form method="post" action="'//CGI_PATH//'">', &
                '<input type="hidden" name="F" value="'//trim(itoa(fnLogin))//'"><br>', &
                '<b>Username</b> (case sensitive):<br>', &
                '<input size="20" type="text" name="N" value="Guest">', &
                '<br><br>', &
                '<b>Password:</b><br>', &
                '<input size="20" type="password" name="P" value="Guest">', &
                '<br><br>', &
                '<input type="submit" value="Login"></form>'
            if (len_trim(loginCheckMessage)>0) write(device,AFORMAT) &
                '<br>'//red//trim(loginCheckMessage)//black//'<br>'
            write(device,AFORMAT) '<hr>'
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
        type (TYPE_SECTION), intent(in), dimension (0:MAX_ALL_SECTIONS) :: Section
        integer, parameter :: period = 2 ! no. of 15 minute intervals
        integer :: i, color, ncol, sect, j, mcol
        character (len=1024) :: line
        integer :: colorIdx(60,6)

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
        type (TYPE_SECTION), intent(in), dimension (0:MAX_ALL_SECTIONS) :: Section
        character(len=*), intent(in) :: target_name, target_action
        logical, intent(in) :: permitted
        character (len=*), intent(in), optional :: heading
        integer :: crse, idx, mdx, rdx, sdx, tdx, previous, conflict, dept
        character (len=10) :: note
        logical :: countUnits, sectionDone
        real :: totalUnits, meetingUnits, totalHours, meetingHours

        if (present(heading)) write(device,AFORMAT) heading
        if (lenSL < 3) then
            write(device,AFORMAT) '<br>(None)<br>'
            return
        end if

        countUnits = target_action=='Del' .and. target_fn/=fnOFFSET+fnRoomSchedule
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
#if defined DO_NOT_ENCODE
            QUERY_put = Section(sdx)%ClassId
#else
            call cgi_url_encode(Section(sdx)%ClassId, QUERY_put)
#endif

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
                if (permitted .and. (REQUEST==fnOFFSET+fnBlockEditSection .or. &
                        REQUEST==fnOFFSET+fnBlockEditSubject .or. &
                        REQUEST==fnOFFSET+fnBlockEditName .or. &
                        REQUEST==fnOFFSET+fnBlockCopy .or. &
                        REQUEST==fnOFFSET+fnBlockSchedule)) then ! link section code to edit section
                    write(device,AFORMAT) &
                        trim(make_href(fnOFFSET+fnScheduleOfClasses, Subject(crse)%Name, &
                            A1=Department(dept)%Code, pre=begintr//begintd, post=endtd, anchor=Subject(crse)%Name)), &
                        trim(make_href(fnOFFSET+fnScheduleEdit, Section(sdx)%Code, &
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

                    if (target_fn==fnOFFSET+fnRoomSchedule .or. target_fn==fnOFFSET+fnTeacherSchedule) then
                        if ( permitted ) then
                            write(device,AFORMAT) trim(make_href(target_fn, target_action, &
                                A1=target_name, A2=target_action, A3=QUERY_put, &
                                pre=begintd//'<small>', post='</small>'//endtd//endtr))
                        else
                            write(device,AFORMAT) tdnbspendtd//endtr
                        end if
                    elseif (target_fn==fnChangeMatriculation .or. target_fn==fnOFFSET+fnBlockEditSection) then
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
                if (sectionDone) cycle
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
                if (target_fn==fnOFFSET+fnRoomSchedule .or. target_fn==fnOFFSET+fnTeacherSchedule) then
                    if ( permitted ) then
                        write(device,AFORMAT) trim(make_href(target_fn, target_action, &
                            A1=target_name, A2=target_action, A3=QUERY_put, &
                            pre=begintd//'<small>', post='</small>'//endtd//endtr))
                    else
                        write(device,AFORMAT) tdnbspendtd//endtr
                    end if
                elseif (target_fn==fnChangeMatriculation .or. target_fn==fnOFFSET+fnBlockEditSection) then
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

  
    subroutine links_to_students (device, fn, NumSections, Section)
        integer, intent (in) :: device, fn
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in), dimension (0:MAX_ALL_SECTIONS) :: Section
        integer :: ldx, n_count, tdx, std, ierr, ncol, sect, crse

        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character(len=MAX_LEN_CLASS_ID) :: tClassId
        character(len=127) :: header
        character(len=1) :: ch

        ! collect students
        tArray = 0
        n_count = 0
        select case (fn)

            case (fnSearch)
                ! triggers for bottom menu
                targetCollege = CollegeIdxUser
                if (isRoleChair) targetDepartment = DeptIdxUser

                ! search string ?
                call cgi_get_named_string(QUERY_STRING, 'A2', header, ierr)
                if (header==SPACE) then
                    write(device,AFORMAT) '<br>'//red//'Search string not specified.'//black//'<br><hr>'
                    return
                else
                    do tdx=1,NumStudents
                        std = StdRank(tdx)
                        if (isRoleSRE .and. CollegeIdxUser/=Curriculum(std)%CollegeIdx) cycle
                        if (index(Student(std)%StdNo,trim(header))>0) then
                            n_count = n_count+1
                            tArray(n_count) = std
                        else if (index(Student(std)%Name,trim(header))>0) then
                            n_count = n_count+1
                            tArray(n_count) = std
                        end if
                    end do
                end if
                header = 'Search results for "'//trim(header)//'" students'

            case (fnClassList)
                ! which section?
                call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, ierr)
                targetSection = index_to_section(tClassId, NumSections, Section)
                if (ierr/=0 .or. targetSection==0) then
                    write(device,AFORMAT) '<br>'//red//'Class "'//tClassId//'" not found.'//black//'<br><hr>'
                    targetCollege  = NumColleges ! trigger 'All colleges' link for Admin
                    return
                else
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
                end if
                !write(device,AFORMAT) '<b>Students in '//trim(Section(targetSection)%ClassId)//'</b><hr><br>'
                header = 'Students in '//Section(targetSection)%ClassId

            case (fnStudentsByProgram)
                ! which program ?
                call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
                targetCurriculum = 0
                do ldx=1,NumCurricula
                    if (CurrProgCode(ldx) /= tCurriculum) cycle
                    targetCurriculum = ldx ! trigger links to this college
                    exit
                end do
                if (ierr/=0 .or. targetCurriculum<=0) then
                    write(device,AFORMAT) '<br>'//red//'Curriculum "'//tCurriculum//'" not found.'//black//'<br><hr>'
                    targetCollege  = NumColleges ! trigger 'All colleges' link for Admin
                    return
                else
                    do tdx=1,NumStudents
                        std = StdRank(tdx)
                        if (CurrProgCode(Student(std)%CurriculumIdx) /= tCurriculum) cycle
                        n_count = n_count+1
                        tArray(n_count) = std
                    end do
                end if
                !write(device,AFORMAT) '<b>Students in '//tCurriculum//'</b><br><hr>'
                header = 'Students in '//tCurriculum

            case (fnStudentsByCurriculum)
                ! which Curriculum ?
                call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
                targetCurriculum = index_to_curriculum(tCurriculum)
                if (ierr/=0 .or. targetCurriculum<=0) then
                    write(device,AFORMAT) '<br>'//red//'Curriculum "'//tCurriculum//'" not found.'//black//'<br><hr>'
                    targetCollege  = NumColleges ! trigger 'All colleges' link for Admin
                    return
                else
                    do tdx=1,NumStudents
                        std = StdRank(tdx)
                        if (Student(std)%CurriculumIdx /= targetCurriculum) cycle
                        n_count = n_count+1
                        tArray(n_count) = std
                    end do
                end if
                !write(device,AFORMAT) '<b>Students in '//tCurriculum//'</b><br><hr>'
                header = 'Students in '//tCurriculum

            case (fnStudentsByname)
                ! which college ?
                call cgi_get_named_string(QUERY_STRING, 'A2', ch, ierr)
                call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
                targetCollege = index_to_college(tCollege)
                if (ierr/=0 .or. targetCollege<=0 .or. ch==SPACE) then
                    write(device,AFORMAT) '<br>'//red//'College "'//tCollege// &
                        '" or starting letter "'//ch//'" for name not found.'//black//'<hr><br>'
                    targetCollege  = NumColleges ! trigger 'All colleges' link for Admin
                    return
                else
                    do tdx=1,NumStudents
                        std = StdRank(tdx)
                        if (Curriculum(Student(std)%CurriculumIdx)%CollegeIdx /= targetCollege) cycle
                        if (Student(std)%Name(1:1) /= ch) cycle
                        n_count = n_count+1
                        tArray(n_count) = std
                    end do
                        !if (n_count>0) then
                        !        targetCurriculum = Student(tArray(n_count))%CurriculumIdx ! trigger links to this college
                        !else
                        !        targetCollege  = NumColleges ! trigger 'All colleges' link for Admin
                        !end if
                end if
                !write(device,AFORMAT) '<b>"'//ch//'" students in '//tCollege//'</b><br><hr>'
                header = '"'//ch//'" students in '//tCollege

            case (fnStudentsByYear)
                    ! which college ?
                    call cgi_get_named_string(QUERY_STRING, 'A2', tYear, ierr)
                    call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
                    targetCollege = index_to_college(tCollege)
                    if (ierr/=0 .or. targetCollege<=0 .or. tYear==SPACE) then
                            write(device,AFORMAT) '<br>'//red//'College "'//tCollege// &
                                  '" or student number year "'//tYear//'" not found.'//black//'<hr><br>'
                            targetCollege  = NumColleges ! trigger 'All colleges' link for Admin
                            return
                    else
                            sect = len_trim(tYear)
                            do tdx=1,NumStudents
                               std = StdRank(tdx)
                               if (Curriculum(Student(std)%CurriculumIdx)%CollegeIdx /= targetCollege) cycle
                               if (Student(std)%StdNo(1:sect) /= tYear(:sect)) cycle
                               n_count = n_count+1
                               tArray(n_count) = std
                            end do
                            !if (n_count>0) then
                            !        targetCurriculum = Student(tArray(n_count))%CurriculumIdx ! trigger links to this college
                            !else
                            !        targetCollege  = NumColleges ! trigger 'All colleges' link for Admin
                            !end if
                    end if
                    !write(device,AFORMAT) '<b>"'//tYear//'" students in '//tCollege//'</b><br><hr>'
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
                    if (available(fnChangeMatriculation) ) then
                        write(device,AFORMAT) trim(make_href(fnChangeMatriculation, 'schedule', &
                            A1=Student(std)%StdNo, &
                            pre=' [ ', post=' ]'))
                    end if
                    if (available(fnEditCheckList) ) then
                        write(device,AFORMAT) trim(make_href(fnEditCheckList, 'checklist', &
                            A1=Student(std)%StdNo, &
                            pre=' [ ', post=' ]'))
                    end if
                    if (available(fnStudentPerformance) ) then
                        write(device,AFORMAT) trim(make_href(fnStudentPerformance, 'performance', &
                            A1=Student(std)%StdNo, &
                            pre=' [ ', post=' ]'))
                    end if
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
                    if (available(fnChangeMatriculation) ) then
                        write(device,AFORMAT) trim(make_href(fnChangeMatriculation, 'schedule', &
                            A1=Student(std)%StdNo, &
                            pre=' [ ', post=' ]'))
                    end if
                    if (available(fnEditCheckList) ) then
                        write(device,AFORMAT) trim(make_href(fnEditCheckList, 'checklist', &
                            A1=Student(std)%StdNo, &
                            pre=' [ ', post=' ]'))
                    end if
                    if (available(fnStudentPerformance) ) then
                        write(device,AFORMAT) trim(make_href(fnStudentPerformance, 'performance', &
                            A1=Student(std)%StdNo, &
                            pre=' [ ', post=' ]'))
                    end if
                end if

                write(device,AFORMAT) endtd//endtr
            end do
            write(device,AFORMAT) '</table>'
        end if

        return
    end subroutine list_students


    subroutine html_write_header(device, header, errmsg)
        integer, intent (in) :: device
        character(len=*), intent (in) :: header
        character(len=*), intent (in), optional :: errmsg
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        integer :: cdx, fdx, gdx, nItems

        ! page title, start of body
        write(device,AFORMAT) &
            '<html><head><title>'//trim(UniversityCode)//space//PROGNAME//VERSION// &
            '</title></head><body><a name="TOP"></a>'

        ! banner line 1: user context & shortcuts
        write(device,AFORMAT) &
            '<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
            begintr//begintd//'<b>'//trim(UniversityCode)//', '// &
            trim(txtSemester(currentTerm+6))//' Term, SY '//trim(itoa(currentYear))//dash// &
            trim(itoa(mod(currentYear+1,1000)))//' '//trim(txtPeriod(Period))//'</b>'//endtd

            if (USERNAME/=ROLE) then
                write(device,AFORMAT) tdalignright//'<small>User is '//trim(USERNAME)//'/'//trim(ROLE)//'.'
            else
                write(device,AFORMAT) tdalignright//'<small>User is '//trim(USERNAME)//'.'
            endif

        ! primary links for Department Chair
        if (isRoleChair) then
            if (REQUEST/=fnLogin) then
                call info_department(device, Department(DeptIdxUser)%Code)
            end if

        ! primary links for Registration Adviser
        else if (isRoleSRE) then
            if (available(fnCurriculum) .and. REQUEST/=fnCurriculum) then
                write(device,AFORMAT) trim(make_href(fnCurriculumList, 'Curriculum', &
                    A1=CurrProgCode(CurriculumIdxUser), pre=nbsp))
            end if

        end if

        ! link to other links
        tCollege = College(Department(DeptIdxUser)%CollegeIdx)%Code
        write(device,AFORMAT) trim(make_href(fnCollegeLinks, &
            tCollege, A1=tCollege, pre=nbsp))

        ! Stop HEEDS for Admin & Logout for all users
        write(device,AFORMAT) trim(make_href(fnStopUser, 'Logout', pre=nbsp))
        if (isRoleAdmin) write(device,AFORMAT) &
            trim(make_href(fnStopProgram, 'Stop '//PROGNAME, pre=nbsp))

        ! end of line 1
        write(device,AFORMAT) '</small>'//endtd//endtr

        ! line 2 for banner: higher level context for selected function
        write(device,AFORMAT) begintr//begintd//'<small>'

        ! show student options ?
        if (targetStudent>0) then
            nItems = 0 ! how many enlisted subjects
            do fdx=1,Preenlisted(targetStudent)%lenSubject
                if (Preenlisted(targetStudent)%Section(fdx)>0) nItems=nItems+1
            end do

            tStdNo = Student(targetStudent)%StdNo
            write(device,AFORMAT) '[ '//tStdNo

            if (nItems==0 .and. available(fnFindBlock) .and. REQUEST/=fnFindBlock .and. &
            (IsRoleAdmin .or. &
            (IsRoleSRE .and. CurrProgCode(Student(targetStudent)%CurriculumIdx)==CurrProgCode(CurriculumIdxUser))) ) then
                write(device,AFORMAT) trim(make_href(fnFindBlock, 'Find block', &
                    A1=tStdNo, pre=nbsp))
            end if

            if (available(fnChangeMatriculation) .and. REQUEST/=fnChangeMatriculation) then
                write(device,AFORMAT) trim(make_href(fnChangeMatriculation, 'Schedule', &
                    A1=tStdNo, pre=nbsp))
            end if

            if (available(fnEditCheckList) .and. REQUEST/=fnEditCheckList) then
                write(device,AFORMAT) trim(make_href(fnEditCheckList, 'Checklist', &
                    A1=tStdNo, pre=nbsp))
            end if

            if (available(fnStudentPerformance) .and. REQUEST/=fnStudentPerformance) then
                write(device,AFORMAT) trim(make_href(fnStudentPerformance, 'Performance', &
                    A1=tStdNo, pre=nbsp))
            end if

            write(device,AFORMAT) ' ]'

        end if

        ! students of curriculum ?
        if (targetCurriculum>0) then
            if (REQUEST/=fnCollegeLinks .and. REQUEST/=fnStudentsByProgram .and. available(fnStudentsByProgram)) then
                write(device,AFORMAT) trim(make_href(fnStudentsByProgram, 'students', &
                    A1=CurrProgCode(targetCurriculum), &
                    pre=' [ '//trim(CurrProgCode(targetCurriculum))//SPACE, post=' ] '))
            end if
        end if

        ! show department options ?
        if (targetDepartment>0 .and. DeptIdxUser/=targetDepartment .and. REQUEST/=fnLogin) then
            tDepartment = Department(targetDepartment)%Code
            write(device,AFORMAT) ' [ '//tDepartment
            call info_department(device, tDepartment)
            write(device,AFORMAT) ' ] '
        end if

        ! college links ?
        if (REQUEST/=fnLogin .and. REQUEST/=fnCollegeLinks) then

            if (targetDepartment>0) then
                cdx = Department(targetDepartment)%CollegeIdx
            elseif (targetCurriculum>0) then
                cdx = Curriculum(targetCurriculum)%CollegeIdx
            else
                cdx = CollegeIdxUSER
            end if
            tCollege =College(cdx)%Code
            write(device,AFORMAT) trim(make_href(fnCollegeLinks, 'links', &
                A1=tCollege, pre=' [ '//tCollege, post=' ] '))
        end if

        write(device,AFORMAT) '<small>'//endtd//tdalignright//'<small>'

        write(device,AFORMAT) 'Colleges:'
        do gdx = 1,NumColleges
            if (.not. College(gdx)%hasInfo) cycle
            ! my college links already printed above?
            if (gdx==CollegeIdxUser) cycle
            ! fnCollegeLinks requested already printed above?
            if (REQUEST==fnCollegeLinks .and. gdx==targetCollege) cycle
            write(device,AFORMAT) trim(make_href(fnCollegeLinks, College(gdx)%Code, &
                A1=College(gdx)%Code, pre=nbsp))
        end do
        write(device,AFORMAT) '</small>'//endtd//endtr

        ! line 3 of banner, if any
        if (present(errmsg)) then
            if (errmsg/=SPACE) write(device,AFORMAT) begintr//'<td colspan="2">'// &
                red//'<i>'//trim(errmsg)//'</i>'//black//endtd//endtr
        end if
        write(device,AFORMAT) '</table><hr>'

        ! start of body
        if (len_trim(header)>0) write(device,AFORMAT) '<b>'//trim(header)//nbsp//nbsp// &
            trim(TermQualifier)//'</b><br>'

        return
    end subroutine html_write_header


    subroutine html_write_footer(device)

        integer, intent(in) :: device

        if (REQUEST/=fnStopProgram .and. REQUEST/=fnStopUser .and. &
            REQUEST/=fnChangePassword .and. &
            REQUEST/=fnChangeInitialPassword .and. &
            REQUEST/=fnPrintableWorkload+fnOFFSET .and. &
            REQUEST/=fnPrintableSchedule) then
            ! last piece of info on the page
            write(device,AFORMAT) &
                '<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
                begintr//begintd// &
                '<small><i>Generated '//currentDate(1:4)//FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8)// &
                DASH//currentTime(1:2)//':'//currentTime(3:4)//' by '//PROGNAME//trim(VERSION)//'.'// &
                nbsp//'Please report errors to the '//trim(REGISTRAR)//'.</i></small>'//endtd
            ! Change password, logout
            write(device,AFORMAT) tdalignright//'<small>User is '//trim(USERNAME)//'.'
            if (USERNAME/=GUEST) write(device,AFORMAT) &
                trim(make_href(fnChangePassword, 'Change password', pre=nbsp))
            write(device,AFORMAT) &
                trim(make_href(fnStopUser, 'Logout', pre=nbsp))
            ! Stop for Admin
            if (isRoleAdmin) write(device,AFORMAT) &
                trim(make_href(fnStopProgram, 'Stop '//PROGNAME, pre=nbsp))
            write(device,AFORMAT) '</small>'//endtd//endtr//'</table>'

            if (noWrites) then ! training mode
                write(device,AFORMAT) '<b><small><i>'//red//'The '//PROGNAME// &
                    ' program is in read-only mode. Changes to data will be lost on exit.'// &
                    black//'</i></small></b>'
            end if
        end if

        write(device,AFORMAT) '</body></html>'

        return
    end subroutine html_write_footer

    subroutine html_college_links(device, given, mesg)

        integer, intent(in) :: device
        integer, intent(in), optional :: given
        character(len=*), intent(in), optional :: mesg

        integer :: cdx
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege

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


        if (REQUEST/=fnSubjectList .and. available(fnSubjectList)) then
            write(device,AFORMAT) trim(make_href(fnSubjectList, 'Subjects', &
                A1=tDepartment, pre=nbsp))
        end if

        if (REQUEST/=fnOFFSET+fnTeachersByDept .and. available(fnOFFSET+fnTeachersByDept)) then
            write(device,AFORMAT) trim(make_href(fnOFFSET+fnTeachersByDept, 'Teachers', &
                A1=tDepartment, pre=nbsp))
        end if

        if (REQUEST/=fnOFFSET+fnRoomList .and. available(fnOFFSET+fnRoomList)) then
            write(device,AFORMAT) trim(make_href(fnOFFSET+fnRoomList, 'Rooms', &
                A1=tDepartment, pre=nbsp))
        end if

        if (REQUEST/=fnOFFSET+fnScheduleOfClasses .and. available(fnOFFSET+fnScheduleOfClasses)) then
            write(device,AFORMAT) trim(make_href(fnOFFSET+fnScheduleOfClasses, 'Classes', &
                A1=tDepartment, pre=nbsp))
        end if

        if (REQUEST/=fnDemandForSubjects .and. available(fnDemandForSubjects)) then
            write(device,AFORMAT) trim(make_href(fnDemandForSubjects, 'Demand', &
                A1=tDepartment, pre=nbsp))
        end if

        return
    end subroutine info_department


    subroutine html_college_info(device, coll)
        integer, intent(in) :: device
        integer, intent(in) :: coll
        integer :: ldx, cdx, n_count, std, tLen
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        !character (len=4) :: tYear
        character (len=1) :: ch

        tCollege = College(coll)%Code

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
            do ldx=1,NumCurricula
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

        if (isRoleAdmin .and. coll==CollegeIdxUSER) then

            write(device,AFORMAT) trim(make_href(fnStopProgram, 'Stop', &
                pre='<li><b>', post='</b> the '//PROGNAME//' program</li>'))

            if (noWrites) then ! training mode
                write(device,AFORMAT) trim(make_href(fnToggleTrainingMode, 'Turn it OFF', &
                    pre='<li><b>Training mode is '//red//'ON'//black//'</b>. ', post='</li>'))
            else
                write(device,AFORMAT) trim(make_href(fnToggleTrainingMode, 'Turn it ON', &
                    pre='<li><b>Training mode is '//green//'OFF'//black//'</b>. ', post='</li>'))
            end if

        end if

        if (coll==NumColleges) then

            if (available(fnStudentsDistribution)) then
                write(device,AFORMAT) trim(make_href(fnStudentsDistribution, 'Distribution', &
                    A1=ADMINISTRATION, pre='<li><b>', post=' of students in '//trim(UniversityCode)//'</b>'))
                write(device,AFORMAT) trim(make_href(fnStudentAddPrompt, 'Add', &
                    A1=tCollege, pre=' (', post=' a student)</li>'))
            end if

            if (NumEnlistmentRecords>0) then

                write(device,AFORMAT) '<li><b>Summary of overall enlistment</b><ul>'
                if (available(fnBottleneck)) then
                    write(device,AFORMAT) trim(make_href(fnBottleneck, 'demand > available seats', &
                        A1=tCollege, pre='<li>Top 100 subjects for which ', post='</li>'))
                end if

                if (available(fnExtraSlots)) then
                    write(device,AFORMAT) trim(make_href(fnExtraSlots, 'available seats > demand', &
                        A1=tCollege, pre='<li>Top 100 subjects for which ', post='</li>'))
                end if

                if (available(fnUnderloadSummary)) then
                    write(device,AFORMAT) trim(make_href(fnUnderloadSummary, 'underloads', &
                        A1=tCollege, pre='<li>Summary of ', post='</li>'))
                end if

                write(device,AFORMAT) '</ul></li>'
            end if

        end if

        n_count = 0 ! any students?
        do std=1,NumStudents
            if (Curriculum(Student(std)%CurriculumIdx)%CollegeIdx /= coll) cycle
            n_count = n_count+1
            exit
        end do

        if (n_count>0 .and. &
            (available(fnStudentsByName) .or. available(fnStudentsByYear) .or. available(fnStudentsByProgram))) then
            write(device,AFORMAT) '<li><b>Students in '//trim(tCollege)//'</b><ul>'
            if (available(fnStudentsByName)) then
                write(device,AFORMAT) '<li><b>By last name</b> : '
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
                write(device,AFORMAT) '</li>'
            end if

            if (available(fnStudentsByYear)) then
                write(device,AFORMAT) '<li><b>By number</b> : '
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
                write(device,AFORMAT) '</li>'
            end if

            if (available(fnStudentsByProgram)) then
                write(device,AFORMAT) '<li><b>By curriculum</b> : '
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
                    A1=tCollege))
                write(device,AFORMAT) '</li>'
            end if
            write(device,AFORMAT) '</ul><hr></li>'
        end if

        ! any curricular programs
        n_count = 0
        do cdx=1,NumCurricula
            if (Curriculum(cdx)%CollegeIdx /= coll) cycle
            n_count = n_count+1
            exit
        end do
        if (available(fnCurriculumList)) then
            if (n_count>0) then
                write(device,AFORMAT) '<li><b>Curricular programs</b> : '//nbsp
                done = .false.
                do cdx=1,NumCurricula
                    if (Curriculum(cdx)%CollegeIdx /= coll) cycle
                    if (done(cdx)) cycle
                    write(device,AFORMAT) trim(make_href(fnCurriculumList, CurrProgCode(cdx), &
                        A1=CurrProgCode(cdx), post=nbsp))
                    do ldx=cdx+1,NumCurricula
                        if (CurrProgCode(ldx) == CurrProgCode(cdx)) done(ldx) = .true.
                    end do
                end do
                write(device,AFORMAT) '</li><hr>'
            end if
        end if

        ! subjects
        call links_to_subjects(device, coll, tLen, tArray(1))
     
        ! CURRENT SEMESTER
        write(device,AFORMAT) &
            '<li><b>'//trim(txtSemester(currentTerm+3)//' Term, SY '//trim(itoa(currentYear))//DASH// &
            trim(itoa(currentYear+1))//' '//txtPeriod(Period))//'</b><ul>'

        ! blocks
        if (n_count>0) call links_to_blocks(device, 0, coll)

        ! classes
        call links_to_sections(device, 0, coll, tLen, tArray(1))

        ! enlistment summary
        if (tLen>0) call links_to_depts(device, coll, fnEnlistmentSummary, '<b>Summary of enlistment</b>')

        ! teachers
        call links_to_teachers(device, 0, coll)

        ! rooms
        call links_to_rooms(device, 0, coll)

        ! user's links
        call links_to_user(device, 0)

        write(device,AFORMAT) '</ul></li>'

        if (Period>1) then
            ! NEXT SEMESTER blocks
            write(device,AFORMAT) '<hr>', &
                '<li><b>'//green//trim(txtSemester(targetTerm+3))//' Term, SY '// &
                trim(itoa(targetYear))//DASH//itoa(targetYear+1)//black//'</b><ul>'

            if (available(fnDemandFreshmen) .and. tLen>0 ) then
                write(device,AFORMAT) trim(make_href(fnDemandFreshmen, 'new freshmen', &
                    A1=tCollege, pre='<li><b>Demand for subjects by ', post='</b></li>'))
            end if

            ! demand for subjects
            if (tLen>0) call links_to_depts(device, coll, fnDemandForSubjects, '<b>Demand for subjects</b>')

            ! blocks
            if (n_count>0) call links_to_blocks(device, fnNextOFFSET, coll)

            ! classes
            call links_to_sections(device, fnNextOFFSET, coll, tLen, tArray(1))

            ! teachers
            call links_to_teachers(device, fnNextOFFSET, coll)

            ! rooms
            call links_to_rooms(device, fnNextOFFSET, coll)

            ! user's links
            call links_to_user(device, fnNextOFFSET)

            write(device,AFORMAT) '</ul></li>'
        end if
        write(device,AFORMAT) '</ul><hr>'
        return
    end subroutine html_college_info


    subroutine links_to_depts(device, coll, fn, heading)
        integer, intent (in) :: device, coll, fn
        character(len=*), intent (in) :: heading
        integer :: dept, crse, n_count
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        if (.not. available(fn)) return

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

        if (.not. available(fnSubjectList) .or. numAreas==0) return

        write(device,AFORMAT) '<li><b>Subjects</b> in : '
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
            tDepartment = Department(dept)%Code
            write(device,AFORMAT) trim(make_href(fnSubjectList, tDepartment, A1=tDepartment, post=nbsp))
        end do
        write(device,AFORMAT) '</li><li><b>Subjects by area</b> : '
        do dept=1,numAreas
            tSubject = SubjectArea(AreaList(dept))%Code
            write(device,AFORMAT) trim(make_href(fnSubjectList, tSubject, A1=tSubject, post=nbsp))
        end do
        write(device,AFORMAT) '</li><hr>'
        return
    end subroutine links_to_subjects


    subroutine links_to_sections(device, OFFSET, coll, numAreas, AreaList)
        integer, intent (in) :: device, OFFSET, coll, numAreas
        integer, intent (in) :: AreaList(1:numAreas)
        integer :: dept, crse, n_count
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        if (.not. available(OFFSET+fnScheduleOfClasses) .or. numAreas==0) return
        tCollege = College(coll)%Code

        write(device,AFORMAT) '<li><b>Class schedules</b> in : '
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
            tDepartment = Department(dept)%Code
            write(device,AFORMAT) trim(make_href(OFFSET+fnScheduleOfClasses, tDepartment, &
                A1=tDepartment, post=nbsp))
        end do
        write(device,AFORMAT) '; '//nbsp//' Classes with ', &
            trim(make_href(OFFSET+fnTBATeachers, 'TBA teachers', A1=tCollege, pre=nbsp//'<b>')), &
            trim(make_href(OFFSET+fnTBARooms, 'TBA rooms', A1=tCollege, pre=nbsp, post='</b></li>'))
        write(device,AFORMAT) '<li><b>Class schedules by subject area</b> : '
        do dept=1,numAreas
            tSubject = SubjectArea(AreaList(dept))%Code
            write(device,AFORMAT) trim(make_href(OFFSET+fnScheduleByArea, tSubject, &
                A1=tCollege, A2=tSubject, post=nbsp))
        end do
        write(device,AFORMAT) '</li>'
        return
    end subroutine links_to_sections


    subroutine links_to_blocks(device, OFFSET, coll)
        integer, intent (in) :: device, OFFSET, coll
        integer :: blk
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege

        if (.not. (available(OFFSET+fnBlockSchedule) .or. available(OFFSET+fnBlockNewSelect)) ) return
        tCollege = College(coll)%Code

        write(device,AFORMAT) '<li><b>Block schedules</b> : '
        if (OFFSET>0) then
            do blk=1,NumNextBlocks
                if (Curriculum(NextBlock(blk)%CurriculumIdx)%CollegeIdx/=coll) cycle
                write(device,AFORMAT) trim(make_href(fnNextBlockSchedule, NextBlock(blk)%BlockID, &
                    A1=NextBlock(blk)%BlockID, post=nbsp))
            end do
        else
            do blk=1,NumCurrentBlocks
                if (Curriculum(CurrentBlock(blk)%CurriculumIdx)%CollegeIdx/=coll) cycle
                write(device,AFORMAT) trim(make_href(fnBlockSchedule, CurrentBlock(blk)%BlockID, &
                    A1=CurrentBlock(blk)%BlockID, post=nbsp))
            end do
        end if
        if (isRoleChair) tCollege = College(CollegeIdxUser)%Code
        if (isRoleChair .or. isRoleAdmin) then
            write(device,AFORMAT) trim(make_href(OFFSET+fnBlockNewSelect, 'Add', &
                A1=tCollege, pre=nbsp//' <b>(', post=' block)</b>'))
        end if
        write(device,AFORMAT) '</li>'
        return
    end subroutine links_to_blocks


    subroutine links_to_teachers(device, OFFSET, coll)
        integer, intent (in) :: device, OFFSET, coll
        integer :: tdx, dept, n_count
        character :: ch
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        if (.not. available(OFFSET+fnTeachersByName)) return
        tCollege = College(coll)%Code

        n_count = 0
        do tdx=1,NumTeachers+NumAdditionalTeachers
            if (Department(Teacher(tdx)%DeptIdx)%CollegeIdx /= coll) cycle
            n_count = n_count+1
            exit
        end do
        if (n_count==0) return

        write(device,AFORMAT) '<li><b>Teachers by department</b> : '
        do dept=2,NumDepartments
            if (Department(dept)%CollegeIdx /= coll) cycle
            n_count = 0
            do tdx=1,NumTeachers+NumAdditionalTeachers
                if (Teacher(tdx)%DeptIdx /= dept) cycle
                n_count = n_count+1
            end do
            if (n_count==0) cycle
            tDepartment = Department(dept)%Code
            write(device,AFORMAT) trim(make_href(OFFSET+fnTeachersByDept, tDepartment, &
                A1=tDepartment, pre=nbsp, post='('//trim(itoa(n_count))//')'))
        end do
        if (available(OFFSET+fnTeacherConflicts)) then
            write(device,AFORMAT) trim(make_href(OFFSET+fnTeacherConflicts, 'conflicts', &
                A1=tCollege, pre='; Schedule'//nbsp//'<b>', post='</b>'))
        end if
        write(device,AFORMAT) '</li><li><b>Teachers by last name</b> : '
        do dept=iachar('A'), iachar('Z')
            ch = achar(dept)
            n_count = 0
            do tdx=1,NumTeachers+NumAdditionalTeachers
                if (Teacher(tdx)%Name(1:1) /= ch) cycle
                if (Department(Teacher(tdx)%DeptIdx)%CollegeIdx /= coll) cycle
                n_count = n_count+1
            end do
            if (n_count==0) cycle
            write(device,AFORMAT) trim(make_href(OFFSET+fnTeachersByName, ch, &
                A1=tCollege, A2=ch, pre=nbsp, post='('//trim(itoa(n_count))//')'))
        end do
        write(device,AFORMAT) '</li>'

        return
    end subroutine links_to_teachers


    subroutine links_to_rooms(device, OFFSET, coll)
        integer, intent (in) :: device, OFFSET, coll
        integer :: rdx, dept, n_count
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        if (.not. available(OFFSET+fnRoomList)) return
        tCollege = College(coll)%Code

        n_count = 0
        do rdx=1,NumRooms+NumAdditionalRooms
            if (Department(Room(rdx)%DeptIdx)%CollegeIdx /= coll) cycle
            n_count = n_count+1
            exit
        end do
        if (n_count==0) return

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
            write(device,AFORMAT) trim(make_href(OFFSET+fnRoomList, tDepartment, &
                A1=tDepartment, pre=nbsp, post='('//trim(itoa(n_count))//')'))
        end do
        if (available(OFFSET+fnRoomConflicts)) then
            write(device,AFORMAT) trim(make_href(OFFSET+fnRoomConflicts, 'conflicts', &
                A1=tCollege, pre='; Schedule'//nbsp//'<b>', post='</b>'))
        end if
        write(device,AFORMAT) '</li>'

        return
    end subroutine links_to_rooms


    subroutine links_to_user(device, OFFSET)
        integer, intent (in) :: device, OFFSET

#if defined DO_NOT_ENCODE
        QUERY_put = USERNAME
#else
        call cgi_url_encode(USERNAME, QUERY_put)
#endif
        write(device,AFORMAT) , &
            trim(make_href(OFFSET+fnTeacherSchedule, 'Classes', &
                A1=QUERY_put, pre='<li><b>My links: </b>', post=COMMA//nbsp)), &
            trim(make_href(fnPrintableWorkload+OFFSET, 'form', &
                A1=QUERY_put, pre='Teaching load'//nbsp, post='.</li>'))

        return
    end subroutine links_to_user


    subroutine blocks_in_section(device, sect, fn, NumBlocks, Block)
        integer, intent(in) :: device, sect, fn, NumBlocks
        type (TYPE_BLOCK), dimension(0:), intent(in) :: Block
        integer :: idx, jdx
        do idx=1,NumBlocks
            do jdx=1,Block(idx)%NumClasses
                if (Block(idx)%Section(jdx)/=sect) cycle
                if (available(fn)) then
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

        ! the function and user name
        cipher = 'F='//trim(itoa(fn))//'&N='//USERNAME

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
        cipher = '<a href="'//CGI_PATH//'?q='//trim(cipher)//itoa(kStart)

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

        ! the function and user name
        cipher = 'F='//trim(itoa(fn))//'&N='//USERNAME

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
          '<form name="input" method="post" action="'//CGI_PATH//'">', &
          '<input type="hidden" name="q" value="'//trim(cipher)//trim(itoa(kStart))//'">'

        return
    end subroutine make_form_start

end module HTML
