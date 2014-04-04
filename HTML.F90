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


module HTML

    use XMLIO

    implicit none

    ! index to server functions
    integer, parameter ::  &
        fnLogin                   =  1, & ! login user
        fnGenerateTeacherPassword =  2, & ! generate new password for teacher
        fnChangeTeacherPassword   =  3, & ! change password for teacher
        fnGenerateStudentPassword =  4, & ! generate new password for student
        fnChangeStudentPassword   =  5, & ! change password for student
        fnLogout                  =  6, & ! logout user
        fnToggleEditGrade         =  7, & ! toggle enable edit grade
        fnStop                    =  9, & ! terminate program
        fnEditSignatories         = 10, & ! edit signatories
        fnRecentStudentActivity   = 11, & ! recent student activity
        fnRecentTeacherActivity   = 12, & ! recent teacher activity
        fnOnlineStudents          = 13, & ! online students
        fnOnlineTeachers          = 14, & ! online teachers
        fnFindStudent             = 15, & ! find a student
        fnFindTeacher             = 16, & ! find a teacher
        fnDeleteTeacher           = 17, & ! delete teacher
        fnResetPasswords          = 18, & ! reset all passwords except for ADMINS, SCHEDULERS
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
        fnEditEquivalencies       = 33, & ! edit equivalence rules
        fnEditMOTD                = 34, & ! edit message of the day
        fnSwitchTerm              = 35, & ! set term of school year
        fnSwitchPeriod            = 36, & ! set period of term
        !
        fnAdvisersByTeacher       = 40, & ! change adviser, list by teacher
        fnAdvisersByCurriculum    = 41, & ! change adviser, list by curriculum
        fnStudentsByCurriculum    = 42, & ! view list students in a curriculum
        fnStudentsDistribution    = 43, & ! view distribution of students, by curriculum and by unit
        fnStudentAdd              = 44, & ! add a student
        fnStudentEdit             = 45, & ! display student info for editing
        fnStudentGrades           = 46, & ! display grades of student
        fnEditCheckList           = 47, & ! display checklist for editing
        fnGradeCertification      = 48, & ! printable grade certification
        fnTranscript              = 49, & ! printable transcript
        !
        fnScheduleOfClasses       = 50, & ! display list of classes in department
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
        fnBlockConflicts          = 80, & ! conflicts in block schedules
        !
        fnResetDemandForSubjects  = 85, & ! update demand for subjects
        fnAllowStudentsToEnlist   = 86, & ! allow students to enlist
        fnClearTimetables         = 87, & ! clear timetable/initial schedules of students
        fnGenerateTimetables      = 88, & ! generate timetable/initial schedules of students
        fnStudentClasses          = 89, & ! view timetable/class schedule of student
        fnPrintableSchedule       = 90, & ! printable weekly timetable
        fnChangeMatriculation     = 91, & ! change matriculation
        fnFindBlock               = 92, & ! find a block for student
        !
        fnDemandFreshmen          = 93, & ! view demand for subjects by incoming students
        fnUpdateDemandFreshmen    = 94, & ! update no. of incoming students
        fnDemandForSubjects       = 95, & ! view demand for subjects
        fnPotentialStudents       = 96, & ! list of potential students of a subject
        fnStudentsWithConflicts   = 97, & ! list students with timetable conflicts
        fnStudentForceEnlist      = 98, & ! force-enlist student into section
        fnUpdateDemandForSubjects = 99, & ! update demand for subjects
        !
        fnEnlistmentSummary       =100, & ! summary of enlistment by subject
        fnNotAccommodated         =101, & ! students not accommodated in a priority subject
        fnBottleneck              =102, & ! "bottleneck" subjects
        fnExtraSlots              =103, & ! subjects with excess slots
        fnUnderloadSummary        =104, & ! summary of underloading
        fnUnderloadedStudents     =105, & ! underloaded students
        fnClassList               =106, & ! view list of students in a class
        fnPrintableClassList      =107, & ! printable classlist
        fnGradesheet              =108, & ! enter grades
        fnPrintableGradesheet     =109, & ! printable grades
        fnUnpostedGrades          =110, & ! classes with unposted grades
        fnGradesheetsToReceive    =111, & ! classes with no hardcopy of gradesheet
        fnStudentsNotEnrolled     =112, & ! students without classes
        !
        fnStudentPriority         =120, & ! student group by needs analysis
        fnTimetabling             =121, & ! preenlist/delist students
        !
        fnSummaryWAG              =122, & ! summary list of student GWA
        !
        fnFileDownload            =130    ! download XML data file

    ! the target of the requested function
    integer :: targetCollege, targetDepartment, targetSubject, targetRoom, targetTeacher, &
        targetCurriculum, targetStudent, targetBlock, targetSection

    ! work arrays
    integer :: tArray(max(3*MAX_ALL_STUDENTS/2,2*MAX_ALL_SUBJECTS))
    character (len=80) :: QUERY_put, termDescription
    character (len=MAX_LEN_QUERY_STRING), private :: wrkCipher


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

            case (fnToggleEditGrade)
                    fnDescription = 'toggle enable edit grade'

            case (fnGenerateTeacherPassword   )
                    fnDescription = 'generate new password for teacher'

            case (fnGenerateStudentPassword   )
                    fnDescription = 'generate new password for student'

            case (fnChangeTeacherPassword          )
                    fnDescription = 'change password for Teacher'

            case (fnChangeStudentPassword          )
                    fnDescription = 'change password for student'

            case (fnResetPasswords)
                    fnDescription = 'reset all passwords except for ADMINS, SCHEDULERS'

            case (fnEditSignatories)
                    fnDescription = 'edit signatories'

            case (fnRecentStudentActivity)
                    fnDescription = 'show recent student activity'

            case (fnRecentTeacherActivity)
                    fnDescription = 'recent teacher activity'

            case (fnOnlineStudents)
                    fnDescription = 'list online students'

            case (fnOnlineTeachers)
                    fnDescription = 'list online teachers'

            case (fnFindStudent)
                    fnDescription = 'find a student'

            case (fnFindTeacher)
                    fnDescription = 'find a teacher'

            case (fnDeleteTeacher)
                    fnDescription = 'delete teacher'

            case (fnCollegeLinks            )
                    fnDescription = 'index to unit info'

            case (fnSubjectList             )
                    fnDescription = 'view list of subjects administered by a department'

            case (fnEditSubject             )
                    fnDescription = 'edit subject'

            case (fnCurriculumList          )
                    fnDescription = 'view list of curricular programs administered by a unit'

            case (fnCurriculum              )
                    fnDescription = 'view a curricular program'

            case (fnEditCurriculum          )
                    fnDescription = 'edit curriculum'

            case (fnActivateCurriculum      )
                    fnDescription = 'activate curriculum'

            case (fnDeactivateCurriculum    )
                    fnDescription = 'deactivate curriculum'

            case (fnEditEquivalencies)
                    fnDescription = 'edit equivalence rules'

            case (fnEditRoom                )
                    fnDescription = 'edit room parameters'

            case (fnEditTeacher             )
                    fnDescription = 'edit teacher record'

            case (fnEditMOTD)
                    fnDescription = 'edit message of the day'

            case (fnAdvisersByTeacher       )
                    fnDescription = 'change adviser, list by teacher'

            case (fnAdvisersByCurriculum)
                    fnDescription = 'change adviser, list by curriculum'

            case (fnStudentsByCurriculum    )
                    fnDescription = 'view list students in a curriculum'

            case (fnStudentsDistribution    )
                    fnDescription = 'view distribution of students, by curriculum and by unit'

            case (fnStudentAdd              )
                    fnDescription = 'add a student'

            case (fnStudentEdit             )
                    fnDescription = 'display student info for editing'

            case (fnStudentGrades)
                    fnDescription = 'display grades of student'

            case (fnGradeCertification)
                    fnDescription = 'printable grade certification'

            case (fnTranscript)
                    fnDescription = 'printable transcript'

            case (fnEditCheckList           )
                    fnDescription = 'display checklist for editing'

            case (fnStudentClasses)
                    fnDescription = 'view timetable/class schedule of student'

            case (fnClearTimetables)
                    fnDescription = 'reset initial timetables of students'

            case (fnGenerateTimetables)
                    fnDescription = 'generate initial timetables of students'

            case (fnChangeMatriculation     )
                    fnDescription = 'change matriculation'

            case (fnAllowStudentsToEnlist)
                    fnDescription = 'allow students to enlist'

            case (fnFindBlock               )
                    fnDescription = 'find a block for student'

            case (fnStudentForceEnlist)
                    fnDescription = 'force-enlist student into section'

            case (fnScheduleOfClasses       )
                    fnDescription = 'display subjects in department with open sections'

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
                    fnDescription = 'view list teachers belonging to a unit'

            case (fnTeachersByName          )
                    fnDescription = 'view alphabetical list of teachers in a unit'

            case (fnTeacherClasses         )
                    fnDescription = 'view classes of a teacher'

            case (fnTeacherEditSchedule         )
                    fnDescription = 'edit weekly schedule of a teacher'

            case (fnRoomList                )
                    fnDescription = 'view list rooms administered by a unit'

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

            case (fnBlockDeleteNotClasses         )
                    fnDescription = 'delete block, but keep its sections'

            case (fnBlockDeleteAlsoClasses          )
                    fnDescription = 'delete block and its sections'

            case (fnBlockNewSelect          )
                    fnDescription = 'select parameters for a new block'

            case (fnBlockNewAdd             )
                    fnDescription = 'add a new block'

            case (fnBlockCopy               )
                    fnDescription = 'copy block'

            case (fnBlockList               )
                    fnDescription = 'list blocks'

            case (fnBlockConflicts)
                    fnDescription = 'conflicts in block schedules'

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

            case (fnPrintableClassList               )
                    fnDescription = 'printable classlist'

            case (fnGradesheet              )
                    fnDescription = 'enter grades'

            case (fnPrintableGradesheet       )
                    fnDescription = 'printable gradesheet'

            case (fnDemandFreshmen          )
                    fnDescription = 'view demand for subjects by incoming students'

            case (fnUpdateDemandFreshmen    )
                    fnDescription = 'update no. of incoming students'

            case (fnPrintableSchedule       )
                    fnDescription = 'printable weekly timetable'

            case (fnResetDemandForSubjects)
                    fnDescription = 'reset demand for subjects'

            case (fnDemandForSubjects       )
                    fnDescription = 'view demand for subjects'

            case (fnUpdateDemandForSubjects       )
                    fnDescription = 'update demand for subjects'

            case (fnPotentialStudents       )
                    fnDescription = 'list of potential students of a subject'

            case (fnFileDownload             )
                    fnDescription = 'download XML data file'

            case (fnStudentsWithConflicts        )
                    fnDescription = 'list students with timetable conflicts'

            case (fnSwitchTerm)
                    fnDescription = 'set term of school year'

            case (fnSwitchPeriod)
                    fnDescription = 'set period of term'

            case (fnUnpostedGrades)
                    fnDescription = 'classes with unposted grades'

            case (fnGradesheetsToReceive)
                    fnDescription = 'classes with no hardcopy of gradesheet'

            case (fnStudentPriority)
                    fnDescription = 'student groupings by needs analysis'

            case (fnStudentsNotEnrolled)
                    fnDescription = 'list students without classes'

            case (fnTimetabling)
                    fnDescription = 'preenlist/delist students'

            case (fnSummaryWAG)
                    fnDescription = 'summary list of student GWA'

            case default
                    fnDescription = 'Function description not available - '//itoa(fn)

        end select

    end function fnDescription


    function calculate_check_sum(encryptedText)
        character(len=*), intent (in) :: encryptedText
        integer :: calculate_check_sum
        integer :: checkSum, kStart, lenCipher

        lenCipher = len_trim(encryptedText)
        checkSum = 0
        do kStart=1,lenCipher
            checkSum = checkSum + kStart*ichar(encryptedText(kStart:kStart))
        end do
        calculate_check_sum = checkSum

    end function calculate_check_sum


    function make_href(fn, label, pre, post, A1, A2, A3, A4, A5, A9, anchor, newtab)
    ! build HTML href, like:
    !   pre<a href="CGI_PATH?F=fn&A1=A1&...A5=A5 #anchor target=newtab">label</a>post

        integer, intent (in) :: fn
        integer, intent (in), optional :: A9
        character(len=*), intent (in) :: label
        character(len=*), intent (in), optional :: A1, A2, A3, A4, A5
        character(len=*), intent (in), optional :: pre, post, anchor, newtab

        character(len=MAX_LEN_QUERY_STRING) :: make_href
        character(len=MAX_CGI_WRK_LEN) :: cgi_wrk
        integer :: kStart, checkSum

        ! term provided ?
        if (present(A9)) then
            kStart = A9
        else ! force to be currentTerm
            kStart = currentTerm
        end if

        ! the function, user name and target term
        wrkCipher = 'F='//trim(itoa(fn))//'&N='//trim(USERNAME)//'&A9='//itoa(kStart)

        ! the arguments to the function
        if (present(A1)) then
            call cgi_url_encode(A1,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A1='//cgi_wrk
        end if
        if (present(A2)) then
            call cgi_url_encode(A2,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A2='//cgi_wrk
        end if
        if (present(A3)) then
            call cgi_url_encode(A3,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A3='//cgi_wrk
        end if
        if (present(A4)) then
            call cgi_url_encode(A4,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A4='//cgi_wrk
        end if
        if (present(A5)) then
            call cgi_url_encode(A5,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A5='//cgi_wrk
        end if

        ! calculate checksum
        checkSum = calculate_check_sum(wrkCipher)
        ! encrypt
!#if defined no_password_check
!#else
        call random_number(harvest)
        kStart = MAX_LEN_QUERY_STRING/2 + int(harvest*MAX_LEN_QUERY_STRING/2)
        call encrypt(queryEncryptionKey(:kStart), wrkCipher)
        wrkCipher = trim(wrkCipher)//itoa(kStart)
!#endif
        wrkCipher = '<a href="'//trim(CGI_PATH)//'?r='//trim(itoa(len_trim(wrkCipher)))// &
            '&s='//trim(itoa(checkSum))//'&q='//trim(wrkCipher)

        ! preamble (text before href)
        if (present(pre)) wrkCipher = pre//wrkCipher

        ! the anchor
        if (present(anchor)) wrkCipher = trim(wrkCipher)//'#'//anchor

        ! the target
        if (present(newtab)) wrkCipher = trim(wrkCipher)//' target='//newtab

        ! end href & the label
        wrkCipher = trim(wrkCipher)//'">'//trim(label)//'</a>'

        ! the text after the href
        if (present(post)) wrkCipher = trim(wrkCipher)//post

        make_href = wrkCipher

    end function make_href


    subroutine make_form_start(device, fn, A1, A2, A3, A4, A5, A9)

        integer, intent (in) :: device, fn
        integer, intent (in), optional :: A9
        character(len=*), intent (in), optional :: A1, A2, A3, A4, A5

        character(len=MAX_CGI_WRK_LEN) :: cgi_wrk
        integer :: kStart, checkSum

        ! term provided ?
        if (present(A9)) then
            kStart = A9
        else ! force to be currentTerm
            kStart = currentTerm
        end if

        ! the function, user name and target term
        wrkCipher = 'F='//trim(itoa(fn))//'&N='//trim(USERNAME)//'&A9='//itoa(kStart)

        ! the arguments to the function
        if (present(A1)) then
            call cgi_url_encode(A1,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A1='//cgi_wrk
        end if
        if (present(A2)) then
            call cgi_url_encode(A2,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A2='//cgi_wrk
        end if
        if (present(A3)) then
            call cgi_url_encode(A3,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A3='//cgi_wrk
        end if
        if (present(A4)) then
            call cgi_url_encode(A4,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A4='//cgi_wrk
        end if
        if (present(A5)) then
            call cgi_url_encode(A5,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A5='//cgi_wrk
        end if

        ! calculate checksum
        checkSum = calculate_check_sum(wrkCipher)
        ! encrypt
!#if defined no_password_check
!#else
        call random_number(harvest)
        kStart = MAX_LEN_QUERY_STRING/2 + int(harvest*MAX_LEN_QUERY_STRING/2)
        call encrypt(queryEncryptionKey(:kStart), wrkCipher)
        wrkCipher = trim(wrkCipher)//itoa(kStart)
!#endif
        write(device,AFORMAT) &
          '<form name="input" method="post" action="'//trim(CGI_PATH)//'">', &
          '<input type="hidden" name="s" value="'//trim(itoa(checkSum))//'">', &
          '<input type="hidden" name="q" value="'//trim(wrkCipher)//'">', &
          '<input type="hidden" name="r" value="'//trim(itoa(len_trim(wrkCipher)))//'">'

    end subroutine make_form_start


    subroutine html_copyright(device)
        integer, intent(in) :: device

        write(device,AFORMAT) &
            beginsmall//beginitalic//PROGNAME//nbsp//COPYRIGHT//linebreak, &
            'This program comes with ABSOLUTELY NO WARRANTY; for details see the GNU GENERAL PUBLIC LICENSE Version 3 ', &
            '(<a target="0" href="http://www.gnu.org/licenses/gpl-3.0.html">GPLv3</a>).'//linebreak, &
            'This program is free software, and you are welcome to redistribute it under certain conditions; ', &
            ' see the GPLv3 for details.'//linebreak, &
            ' Support for the development of this program was provided by: '// &
            ' University of the Philippines Los Banos (1997-2001); '// &
            ' BalikScientist Program of the Department of Science and Technology (2010); '// &
            ' Isabela State University (2011); and '// &
            ' Cagayan State University (2012, 2013). ', &
            'The source code is available at <a target="0" href="'//WEB//'">'//WEB//'</a>'//linebreak, &
            CONTACT//' This is '//PROGNAME//' Version '//VERSION//enditalic//endsmall//horizontal


    end subroutine html_copyright


    subroutine html_landing_page(device, mesg)
        integer, intent(in) :: device
        character(len=*), intent(in) :: mesg
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        character (len=MAX_LEN_PASSWD_VAR) :: tPassword

        call html_comment('html_landing_page('//trim(mesg)//')')
#if defined no_password_check
        tTeacher = PROGNAME
#else
        tTeacher = GUEST
#endif
        tPassword = GUEST

        write(device,AFORMAT) &
            '<html><head><title>'//trim(UniversityCode)//SPACE//PROGNAME//'</title></head><body>'

        if (len_trim(mesg)>0) then ! Stop/Exit page
            write(device,aformat) '<h2>'//trim(UniversityCode)//SPACE//PROGNAME//'</h2>', &
                red//trim(mesg)//black
            if (REQUEST==fnStop)  &
                write(device,AFORMAT)linebreak//'Go to <a href="http://'//IP_ADDR//'">'//PROGNAME//' Index</a>'
        else ! Login page
            write(device,AFORMAT) &
                '<h2>Welcome to '//trim(UniversityCode)//SPACE//PROGNAME//' !</h2>'//horizontal, &
                '<table border="0" width="100%">'//begintr, &
                '<td valign="top" width="25%">'
            call make_form_start(device, fnLogin)
            write(device,AFORMAT) &
                beginbold//'Username'//endbold//':'//linebreak, &
                '<input size="20" type="text" name="U" value="'//trim(tTeacher)//'">'//linebreak//linebreak, &
                beginbold//'Password:'//endbold//linebreak, &
                '<input size="20" type="password" name="P" value="'//trim(tPassword)//'">'//linebreak//linebreak, &
                '<input type="submit" value="Login">', &
                nbsp//beginitalic//beginsmall//'(or, go back to <a href="http://'//IP_ADDR//'">'//PROGNAME// &
                ' Index</a>)'//endsmall//enditalic//endform
            if (len_trim(loginCheckMessage)>0) write(device,AFORMAT) &
                linebreak//red//trim(loginCheckMessage)//black//linebreak
            write(device,AFORMAT) endtd//begintd
            if ( len_trim(MOTD)>0 ) then
                write(device,AFORMAT) red//beginbold//'IMPORTANT !'//endbold//nbsp//black//trim(MOTD)//linebreak//linebreak
            end if
            write(device,AFORMAT) &
                beginbold//'Forgot your Username and/or Password?'//endbold, &
                beginitalic//' Visit your Dean or the Registrar; bring University-issued ID.'//enditalic, &
                linebreak, &
                linebreak//beginbold//'Tips:'//endbold//'<ul>', &
                beginitem//'Change your initial password. Do not forget to logout.'//enditem, &
                beginitem//'Do not use the browser ''Back'' button unless '// &
                ' there are no hyperlinks or ''Submit'' buttons on the displayed page.'//enditem, &
                beginitem//'HEEDS is best viewed using the Mozilla Firefox browser.'//enditem, &
                endtd//endtr//endtable//horizontal
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
                if (TimeTable(ncol,i)==0) cycle ! no class at this time
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

        write(device,AFORMAT) linebreak//beginbold//'Weekly Timetable'//endbold// &
            '<table border="1" width="100%">'//beginsmall
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
                    line = tdaligncenter//green//beginbold//beginitalic//'proposed'//enditalic//endbold//black//endtd//line
                else
                    line = '<td align="center" bgcolor="'//bgcolor(colorIdx(ncol,i))//'">'// &
                        trim(Section(sect)%ClassId)//endtd//line
                end if
            end do
            line = '<td width="100">'//trim(text_time_period(ncol,ncol+period))//endtd//line
            write (device,AFORMAT) begintr//trim(line)//endtr
        end do
        write(device,AFORMAT) endsmall//endtable

    end subroutine timetable_display


    subroutine list_sections_to_edit(device, Term, Section, lenSL, SectionList, &
            target_fn, target_name, target_action, permitted, heading)

        integer, intent(in) :: device, Term, lenSL, SectionList(3*lenSL+3), target_fn
        type (TYPE_SECTION), intent(in) :: Section(0:)
        character(len=*), intent(in) :: target_name, target_action
        logical, intent(in) :: permitted
        character (len=*), intent(in), optional :: heading
        integer :: crse, idx, mdx, rdx, sdx, tdx, previous, conflict, dept
        character (len=10) :: note
        logical :: countUnits, sectionDone
        real :: totalUnits, meetingUnits, totalHours, meetingHours

        call html_comment('list_sections_to_edit('//itoa(lenSL/3)//')')

        if (present(heading)) write(device,AFORMAT) heading
        if (lenSL < 3) then
            write(device,AFORMAT) BRNONE
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
        if (target_fn>0 .and. permitted) then
            write(device,AFORMAT) thalignleft//beginsmall//'Action'//endsmall//endth//endtr
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
                        trim(make_href(fnScheduleByArea, Subject(crse)%Name, &
                            A1=Department(dept)%Code, pre=begintr//begintd, post=endtd, A2=Subject(crse)%Name, A9=Term)), &
                        trim(make_href(fnScheduleEdit, Section(sdx)%Code, &
                            A1=QUERY_put, A9=Term, pre=begintd, post=endtd))
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
                    write(device,AFORMAT) begintd//text_days_of_section(Section(sdx))//endtd// &
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
                                A1=target_name, A2=target_action, A3=QUERY_put, A9=Term, &
                                pre=begintd//beginsmall, post=endsmall//endtd//endtr))
                        else
                            write(device,AFORMAT) tdnbspendtd//endtr
                        end if
                    elseif (target_fn==fnTeacherClasses) then
                        if ( permitted .and. tdx>0) then
                            write(device,AFORMAT) trim(make_href(target_fn, target_action, &
                                A1=Teacher(tdx)%TeacherID, A9=Term, &
                                pre=begintd//beginsmall, post=endsmall//endtd//endtr))
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
                                    A1=target_name, A2=target_action, A3=QUERY_put, A9=Term, &
                                    pre=begintd//beginsmall, post=endsmall//trim(note)//endtd//endtr))
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
                            A1=target_name, A2=target_action, A3=QUERY_put, A9=Term, &
                            pre=begintd//beginsmall, post=endsmall//endtd//endtr))
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
                                A1=target_name, A2=target_action, A3=QUERY_put, A9=Term, &
                                pre=begintd//beginsmall, post=endsmall//trim(note)//endtd//endtr))
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
            write(device,AFORMAT) begintr//'<td colspan="9">'//horizontal//endtd//endtr, &
                begintr//tdnbspendtd//begintd//beginbold//'Totals'//endbold//' : '//endtd// & ! code
                begintd//trim(ftoa(totalUnits,2))//endtd//begintd//trim(ftoa(totalHours,2))//endtd// & ! hours
                tdnbspendtd// tdnbspendtd// tdnbspendtd// tdnbspendtd// tdnbspendtd//endtr, &
                begintr//'<td colspan="9">'//horizontal//endtd//endtr
        end if
        write(device,AFORMAT) endtable


    end subroutine list_sections_to_edit


    subroutine html_write_header(device, header, errmsg)
        integer, intent (in) :: device
        character(len=*), intent (in) :: header
        character(len=*), intent (in), optional :: errmsg
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        integer :: gdx, tdx, nClasses(3), nAdvised(3), tYear, term
        character (len=80) :: description

        call html_comment('html_write_header('//trim(header)//')')

        description = ' for '//text_school_year(currentYear)
        if (REQUEST<fnScheduleOfClasses) then
            termDescription = SPACE
        else
            termDescription = ' for '//termDescription
        end if

        ! page title, start of body
        write(device,AFORMAT) '<html><head><title>'//trim(UniversityCode)//SPACE//PROGNAME//VERSION// &
            '</title></head><body><a name="TOP"></a>'

        ! banner line 1: user context & shortcuts
        write(device,AFORMAT) &
            '<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
            begintr//'<td width="50%"><h2>'//trim(UniversityCode)//SPACE//trim(PROGNAME)// &
            trim(description)//'</h2>'
        if (present(errmsg)) then
            if (errmsg/=SPACE) write(device,AFORMAT) red//beginitalic//trim(errmsg)//enditalic//black
        end if

        write(device,AFORMAT) endtd//'<td align="right" valign="top">'//beginsmall
        QUERY_put = USERNAME
        if (len_trim(ROLE)>0) then
            write(device,AFORMAT) &
                '[ '//beginbold//'User is '//trim(USERNAME)//endbold//' ('//trim(ROLE)//').'
        else
            write(device,AFORMAT) &
                '[ '//beginbold//'User is '//trim(USERNAME)//endbold//DOT
        end if

        if ((isRoleSysAd .or. isRoleOfficial) .or. isRoleGuest .or. isRoleStaff) then
        else if ( isRoleDean .or. isRoleChair .or. isRoleFaculty ) then ! primary links for entry in Teacher()

            if (Teacher(requestingTeacher)%NumAdvisees>0) write(device,AFORMAT) &
                trim(make_href(fnAdvisersByTeacher, 'Advisees', A1=QUERY_put, A9=currentTerm, pre=nbsp))

            call count_teacher_load(requestingTeacher, 0, nClasses)
            if (sum(nClasses)>0) then
                write(device,AFORMAT) nbsp//'Classes: '
                do tdx=termBegin,termEnd
                    call qualify_term (tdx, tYear, term)
                    if (nClasses(term)>0) write(device,AFORMAT) &
                        trim(make_href(fnTeacherClasses, txtSemester(term+6), A1=QUERY_put, A9=term, pre=nbsp))
                end do
                write(device,AFORMAT) nbsp//'Load form: '
                do tdx=termBegin,termEnd
                    call qualify_term (tdx, tYear, term)
                    if (nClasses(term)>0) write(device,AFORMAT) &
                        trim(make_href(fnPrintableWorkload, txtSemester(term+6), A1=QUERY_put, A9=term, pre=nbsp))
                end do
            end if

        else if (isRoleStudent) then ! primary links for a student

            call count_preenlistment(requestingStudent, 0, nClasses, nAdvised)

            if (len_trim(Student(requestingStudent)%Adviser)>0) write(device,AFORMAT) &
                trim(make_href(fnTeacherClasses, 'Adviser', A1=Student(requestingStudent)%Adviser, &
                    A9=currentTerm, pre=nbsp))
            write(device,AFORMAT) &
                trim(make_href(fnStudentGrades, 'Grades', A1=QUERY_put, pre=nbsp)), &
                trim(make_href(fnEditCheckList, 'Checklist', A1=QUERY_put, pre=nbsp))
            do tdx=termBegin,termEnd
                call qualify_term (tdx, tYear, term)
                if (nClasses(term)+nAdvised(term)>0) write(device,AFORMAT) &
                    trim(make_href(fnStudentClasses, txtSemester(term+6), A1=QUERY_put, A9=term, pre=nbsp))
            end do

        end if

        ! Exit for all users
        write(device,AFORMAT) trim(make_href(fnLogout, 'Quit', pre=nbsp))

        ! emergency stop
#if defined no_password_check
        write(device,AFORMAT) trim(make_href(fnStop, 'Stop '//PROGNAME, pre=nbsp))
#endif

        ! end of line 1
        write(device,AFORMAT) ' ]'//endsmall//linebreak//beginsmall//'[ '//beginbold//'Go to'//endbold//' :'
        do gdx = 1,NumColleges
            if (.not. College(gdx)%hasInfo) cycle
            ! fnCollegeLinks requested already printed above?
            write(device,AFORMAT) trim(make_href(fnCollegeLinks, College(gdx)%Code, &
                A1=College(gdx)%Code, A9=currentTerm, pre=nbsp))
        end do
        write(device,AFORMAT) ' ]'//endsmall

        ! show student options ?
        if (targetStudent>0 .and. requestingStudent/=targetStudent .and. is_adviser_of_student(targetStudent,orHigherUp) ) then

            call count_preenlistment(targetStudent, 0, nClasses, nAdvised)

            tStdNo = Student(targetStudent)%StdNo
            write(device,AFORMAT) linebreak//beginsmall//'[  '//beginbold//trim(tStdNo)//endbold//' :', &
                trim(make_href(fnStudentEdit, 'Info', A1=tStdNo, pre=nbsp)), &
                trim(make_href(fnRecentStudentActivity, 'Log', A1=tStdNo, pre=nbsp))

            if (len_trim(Student(targetStudent)%Adviser)>0) write(device,AFORMAT) &
                trim(make_href(fnTeacherClasses, 'Adviser', A1=Student(targetStudent)%Adviser, pre=nbsp))

            if (REQUEST/=fnStudentGrades) write(device,AFORMAT) &
                trim(make_href(fnStudentGrades, 'Grades', A1=tStdNo, pre=nbsp))

            if ( is_admin_of_college(Curriculum(Student(targetStudent)%CurriculumIdx)%CollegeIdx) .and. &
                 REQUEST/=fnStudentGrades) then
                write(device,AFORMAT) trim(make_href(fnTranscript, 'Transcript', A1=tStdNo, pre=nbsp) )
            end if

            if ( REQUEST/=fnEditCheckList) then
                write(device,AFORMAT) trim(make_href(fnEditCheckList, 'Checklist', A1=tStdNo, pre=nbsp))
            end if

            if (isPeriodOne .and. nAdvised(currentTerm)>0 .and. nClasses(currentTerm)==0 ) then
                write(device,AFORMAT) trim(make_href(fnFindBlock, 'Find block', A1=tStdNo, A9=currentTerm, pre=nbsp))
            end if

            if (isPeriodFour .and. nAdvised(nextTerm)>0 .and. nClasses(nextTerm)==0 ) then
                write(device,AFORMAT) trim(make_href(fnFindBlock, 'Find block', A1=tStdNo, A9=nextTerm, pre=nbsp))
            end if

            do tdx=termBegin,termEnd
                call qualify_term (tdx, tYear, term)
                if (nClasses(term)+nAdvised(term)>0) write(device,AFORMAT) &
                    trim(make_href(fnStudentClasses, txtSemester(term+6), A1=tStdNo, A9=term, pre=nbsp))
            end do

            write(device,AFORMAT) ' ]'//endsmall

        end if

        ! a teacher ?
        if (targetTeacher>0 .and. requestingTeacher/=targetTeacher .and. &
            Teacher(targetTeacher)%Role/=STAFF) then
            tTeacher = Teacher(targetTeacher)%TeacherId

            call count_teacher_load(targetTeacher, 0, nClasses)

            write(device,AFORMAT) linebreak//beginsmall//'[  '//beginbold//trim(Teacher(targetTeacher)%Name)//endbold//SPACE

            if ( is_dean_of_college(Department(Teacher(targetTeacher)%DeptIdx)%CollegeIdx,orHigherUp) ) then

                if (REQUEST/=fnEditTeacher .and. REQUEST/=fnGenerateTeacherPassword) then
                    write(device,AFORMAT) trim(make_href(fnEditTeacher, 'Info', A1=tTeacher, pre=nbsp))
                end if

                if (Teacher(targetTeacher)%NumAdvisees>0) write(device,AFORMAT) &
                    trim(make_href(fnAdvisersByTeacher, 'Advisees', A1=tTeacher, pre=nbsp))

                write(device,AFORMAT) &
                    trim(make_href(fnRecentTeacherActivity, 'Log', A1=tTeacher, pre=nbsp))

                write(device,AFORMAT) nbsp//'Edit load:'
                do tdx=termBegin,termEnd
                    call qualify_term (tdx, tYear, term)
                    write(device,AFORMAT) &
                        trim(make_href(fnTeacherEditSchedule, txtSemester(term+6), A1=tTeacher, A9=term, pre=nbsp ))
                end do

            end if

            if (sum(nClasses)>0) then
                write(device,AFORMAT) nbsp//'Classes: '
                do tdx=termBegin,termEnd
                    call qualify_term (tdx, tYear, term)
                    if (nClasses(term)>0) write(device,AFORMAT) &
                        trim(make_href(fnTeacherClasses, txtSemester(term+6), A1=tTeacher, A9=term, pre=nbsp))
                end do
                write(device,AFORMAT) nbsp//'Load form: '
                do tdx=termBegin,termEnd
                    call qualify_term (tdx, tYear, term)
                    if (nClasses(term)>0) write(device,AFORMAT) &
                        trim(make_href(fnPrintableWorkload, txtSemester(term+6), A1=tTeacher, A9=term, pre=nbsp))
                end do
            end if

            write(device,AFORMAT) ' ]'//endsmall
        end if

        ! show department options ?
        if (targetDepartment>1 .and. REQUEST/=fnLogin) then
            tDepartment = Department(targetDepartment)%Code
            write(device,AFORMAT) linebreak//beginsmall//'[  '//beginbold//trim(tDepartment)//endbold, &
                trim(make_href(fnTeachersByDept, 'Teachers', A1=tDepartment, pre=nbsp)), &
                trim(make_href(fnRoomList, 'Rooms', A1=tDepartment, pre=nbsp)), &
                trim(make_href(fnScheduleOfClasses, 'Classes', A1=tDepartment, pre=nbsp))
            if ((.not. isPeriodOne) .and. .not. (isRoleStudent .or. isRoleGuest) )&
                write(device,AFORMAT) &
                    trim(make_href(fnDemandForSubjects, 'Demand', A1=tDepartment, A9=nextTerm, pre=nbsp))
            write(device,AFORMAT) ' ]'//endsmall
        end if

        if ( REQUEST==fnEditCheckList) then
            call checklist_links(device, .true.)
        end if

        write(device,AFORMAT) endtd//endtr, &
            endtable//horizontal

        ! start of body
        if (len_trim(header)>0) write(device,AFORMAT) '<h3>'//trim(header)// &
            nbsp//trim(termDescription)//'</h3>'

    end subroutine html_write_header


    subroutine html_write_footer(device)

        integer, intent(in) :: device

        if (REQUEST==fnFileDownload) return
        call html_comment('html_write_footer()')

        select case (REQUEST)

            case (fnStop)

            case (fnLogout)

            case (fnChangeTeacherPassword)

            case (fnChangeStudentPassword)

            case (fnPrintableClasslist)

            case (fnPrintableGradesheet)

            case (fnPrintableWorkload)

            case (fnGradeCertification)

            case (fnTranscript)

            case (fnPrintableSchedule)

            case default

                write(device,AFORMAT) &
                    '<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
                    begintr//begintd// &
                    beginsmall//beginitalic//'Generated '//currentDate(1:4)//FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8)// &
                    DASH//currentTime(1:2)//':'//currentTime(3:4)//' by '// &
                    '<a target="0" href="'//WEB//'">'//PROGNAME//'</a>'// &
                    VERSION//DOT
                if (.not. isRoleSysAd) write(device,AFORMAT) nbsp//'Please report errors to the Registrar.'
                ! Change password, logout
                write(device,AFORMAT) enditalic//endsmall//endtd//tdalignright//beginsmall, &
                    'User is '//trim(USERNAME)//'@'//REMOTE_ADDR//nbsp
                if (USERNAME/=GUEST) then
                    if (isRoleStudent) then
                        write(device,AFORMAT) &
                            trim(make_href(fnChangeStudentPassword, 'Change password'))
                    else
                        write(device,AFORMAT) &
                            trim(make_href(fnChangeTeacherPassword, 'Change password'))
                    end if
                end if
                write(device,AFORMAT) trim(make_href(fnLogout, 'Quit', pre=nbsp)), &
                    endsmall//endtd//endtr//endtable

        end select

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
            targetCollege = given
            cdx = 0
        else
            call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, cdx)
            targetCollege = index_to_college(tCollege)
        end if

        if (cdx/=0 .or. targetCollege==0) then ! force to select a unit
            call html_write_header(device, 'Select a unit', mesg)
            ! start of body
            write(device,aformat) '<ul>'
            do cdx = 1,NumColleges
                if (.not. College(cdx)%hasInfo) cycle
                tCollege = College(cdx)%Code
                write(device,aformat) trim(make_href(fnCollegeLinks, tCollege, &
                    A1=tCollege, pre=beginitem, post=' - '//trim(College(cdx)%Name)//enditem))
            end do
            write(device,aformat) '</ul>'//horizontal
        else
            call html_write_header(device, College(targetCollege)%Code//'- '//College(targetCollege)%Name, mesg)
            call html_college_info(device, targetCollege)
        end if

    end subroutine html_college_links


    subroutine html_college_info(device, coll)

        integer, intent(in) :: device
        integer, intent(in) :: coll
        integer :: blk, tdx, rdx, ldx, cdx, dept, n_curr, n_count, numAreas
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        !character (len=4) :: tYear
        !character (len=1) :: ch
        character (len=80) :: description, typeNeedsAnalysis
        logical :: hasRooms, hasTeachers, hasStudents, hasSubjects, hasBlocks, someCheckboxes

        integer :: CollegeCount(0:MAX_ALL_COLLEGES), ProgramCount(0:MAX_ALL_CURRICULA), CurriculumCount(0:MAX_ALL_CURRICULA)
        integer :: sdx, n, idxCurr

        call html_comment('html_college_info()')

        tCollege = College(coll)%Code
        hasRooms = .false.
        hasTeachers = .false.
        hasStudents = .false.
        hasSubjects = .false.

        ! no. of students
        ProgramCount= 0 ! in a degree program
        CurriculumCount= 0 ! in a specific curriculum
        CollegeCount = 0 ! per unit
        do sdx=1,NumStudents+NumAdditionalStudents
            ldx = Student(sdx)%CurriculumIdx
            CurriculumCount(ldx) = CurriculumCount(ldx)+1
            cdx = CurrProgNum(ldx)
            n = Curriculum(ldx)%CollegeIdx
            ProgramCount(cdx) = ProgramCount(cdx)+1
            CollegeCount(n) = CollegeCount(n) + 1
        end do

        ! any curricular programs
        n_curr = 0
        do cdx=1,NumCurricula-1
            if (Curriculum(cdx)%CollegeIdx /= coll) cycle
            n_curr = n_curr+1
            exit
        end do

        ! subject areas in unit
        numAreas = 0
#if defined REGIST
        do cdx=1,NumSubjectAreas
            if (SubjectArea(cdx)%CollegeIdx==coll) then
                numAreas = numAreas+1
                tArray(numAreas) = cdx
            end if
        end do
#else
        ! Subjects administered by program
        do cdx=1,NumSubjectAreas
            do ldx=1,NumCurricula-1
                if (Curriculum(ldx)%CollegeIdx/=coll) cycle
                if (is_used_in_curriculum_subject_area(Curriculum(ldx), trim(SubjectArea(cdx)%Code)//SPACE)) then
                    numAreas = numAreas+1
                    tArray(numAreas) = cdx
                    exit
                end if
            end do
        end do
#endif
        hasSubjects = n_curr>0 .or. numAreas>0

        ! start of body
        write(device,AFORMAT) '<ul>'

        if (coll==NumColleges .and. (isRoleSysAd .or. isRoleOfficial)) then

!            call make_form_start(device, fnSwitchPeriod)
!            write(device,AFORMAT) beginitem//beginbold//'Select'//endbold//' Period to enable '//beginbold//'Role'//endbold//' do '// &
!                beginbold//beginitalic//'Action'//enditalic//endbold//' for  '//beginbold//nbsp// &
!                Red//trim(txtSemester(cTm1+6))//' (previous)'//trim(termQualifier(cTm1+6))//black//nbsp// &
!                Lime//trim(txtSemester(currentTerm+6))//' (current)'//trim(termQualifier(currentTerm+6))//black//nbsp// &
!                Fuchsia//trim(txtSemester(nextTerm+6))//' (next)'//trim(termQualifier(nextTerm+6))//black//endbold//DOT, &
!                linebreak//'A '//beginbold//'Dean'//endbold//' can do the actions of Adviser, Chair, or Teacher.'
!
!            write(device,AFORMAT) &
!                '<table>', &
!                begintr// &
!                    tdnbspendtd// &
!                    begintd//'|'//endtd, &
!                    beginth//'Adviser'//endth, &
!                    begintd//'|'//endtd, &
!                    beginth//'Adviser'//endth, &
!                    begintd//'|'//endtd, &
!                    beginth//'Teacher'//endth, &
!                    begintd//'|'//endtd, &
!                    beginth//'Chair'//nbsp//endth, &
!                    begintd//'|'//endtd, &
!                    beginth//'Teacher'//endth, &
!                    begintd//'|'//endtd, &
!                endtr
!
!            write(device,AFORMAT) &
!                begintr// &
!                    begintd//'Period'//endtd, &
!                    '<td>|'//linebreak//'|</td>', &
!                    '<th>'//beginitalic//'Advice'//linebreak//'students'//enditalic//'</th>', &
!                    '<td>|'//linebreak//'|</td>', &
!                    '<th colspan="3">'//beginitalic//'Edit'//linebreak//'classlists'//enditalic//'</th>', &
!                    '<td>|'//linebreak//'|</td>', &
!                    '<th>'//beginitalic//'Edit'//linebreak//'schedules'//enditalic//'</th>', &
!                    '<td>|'//linebreak//'|</td>', &
!                    '<th>'//beginitalic//'Enter'//linebreak//'grades'//enditalic//'</th>', &
!                    '<td>|'//linebreak//'|</td>', &
!                endtr
!            if (isPeriodOne) then
!                write(device,AFORMAT) &
!                    begintr//begintd//'<input type="radio" checked="yes" name="A1" value="Classlists">1 - Registration'//endtd
!            else
!                write(device,AFORMAT) &
!                    begintr//begintd//'<input type="radio" name="A1" value="Classlists">1 - Registration'//endtd
!            end if
!            write(device,AFORMAT) &
!                    begintd//'|'//endtd, &
!                    '<td bgcolor="#00FF00">'//nbsp//endtd, &
!                    begintd//'|'//endtd, &
!                    '<td bgcolor="#00FF00">'//nbsp//endtd, &
!                    begintd//'|'//endtd, &
!                    '<td bgcolor="#00FF00">'//nbsp//endtd, &
!                    begintd//'|'//endtd, &
!                    '<td bgcolor="#00FF00">'//nbsp//endtd, &
!                    begintd//'|'//endtd, &
!                    '<td bgcolor="#FF0000">'//nbsp//endtd, &
!                    begintd//'|'//endtd, &
!                endtr
!
!            if (isPeriodTwo) then
!                write(device,AFORMAT) &
!                    begintr//begintd//'<input type="radio" checked="yes" name="A1" value="Advising">2 - Advising'//endtd
!            else
!                write(device,AFORMAT) &
!                    begintr//begintd//'<input type="radio" name="A1" value="Advising">2 - Advising'//endtd
!            end if
!            write(device,AFORMAT) &
!                    begintd//'|'//endtd, &
!                    '<td bgcolor="#FF00FF">'//nbsp//endtd, &
!                    begintd//'|'//endtd, &
!                    tdnbspendtd, &
!                    begintd//'|'//endtd, &
!                    tdnbspendtd, &
!                    begintd//'|'//endtd, &
!                    '<td bgcolor="#FF00FF">'//nbsp//endtd, &
!                    begintd//'|'//endtd, &
!                    tdnbspendtd, &
!                    begintd//'|'//endtd, &
!                endtr
!
!            if (isPeriodThree) then
!                write(device,AFORMAT) &
!                    begintr//begintd//'<input type="radio" checked="yes" name="A1" value="Gradesheets">3 - Grading'//endtd
!            else
!                write(device,AFORMAT) &
!                    begintr//begintd//'<input type="radio" name="A1" value="Gradesheets">3 - Grading'//endtd
!            end if
!            write(device,AFORMAT) &
!                    begintd//'|'//endtd, &
!                    '<td bgcolor="#FF00FF">'//nbsp//endtd, &
!                    begintd//'|'//endtd, &
!                    tdnbspendtd, &
!                    begintd//'|'//endtd, &
!                    tdnbspendtd, &
!                    begintd//'|'//endtd, &
!                    '<td bgcolor="#FF00FF">'//nbsp//endtd, &
!                    begintd//'|'//endtd, &
!                    '<td bgcolor="#00FF00">'//nbsp//endtd, &
!                    begintd//'|'//endtd, &
!                endtr
!
!            if (isPeriodFour) then
!                write(device,AFORMAT) begintr// &
!                    begintd//'<input type="radio" checked="yes" name="A1" value="Preregistration">4 - Preregistration'//endtd
!            else
!                write(device,AFORMAT) &
!                    begintr//begintd//'<input type="radio" name="A1" value="Preregistration">4 - Preregistration'//endtd
!            end if
!            write(device,AFORMAT) &
!                    begintd//'|'//endtd, &
!                    '<td align="center" colspan="9">'//beginbold//'Registrar''s Office '//beginitalic//'ONLY'//enditalic//endbold//endtd, &
!                    begintd//'|'//endtd, &
!                endtr
!
!            write(device,AFORMAT) &
!                endtable, &
!                '<input type="submit" name="action" value="Submit"> '// &
!                red//'then wait at least 30 seconds for files to load.'//black// &
!                endform//enditem
!
!            if (isPeriodOne .or. isPeriodTwo .or. isPeriodThree .or. isPeriodFour) then
!                write(device,AFORMAT) trim(make_href(fnSwitchPeriod, 'Reset', A1='Reset', &
!                    pre=beginitem//beginbold, post=endbold//' Period to disallow above '//beginbold//beginitalic//'Actions'//enditalic//endbold//'. '// &
!                    'Grades will still be available.'//enditem))
!            end if
!
!            write(device,AFORMAT) trim(make_href(fnSwitchTerm, 'Advance', A1=itoa(nextTerm), &
!                pre=beginitem//'The  '//beginbold//Lime//'current'//black//endbold//' term is '// &
!                trim(txtSemester(currentTerm+6))//trim(termQualifier(currentTerm+6))//'  '//beginbold, &
!                post=endbold//' to '//nbsp//Fuchsia//trim(txtSemester(nextTerm+6))//' (next)'// &
!                trim(termQualifier(nextTerm+6))//black//DOT//enditem) )
!
!            if (isEnabledEditGrade) then
!                write(device,AFORMAT) trim(make_href(fnToggleEditGrade, 'DISABLE', &
!                    pre=beginitem//'Editing of grades is '//red//'enabled'//black//'.  '//beginbold, &
!                    post=endbold//' it. In any case, edits will be overwritten when checklists are regenerated.'//enditem))
!            else
!                write(device,AFORMAT) trim(make_href(fnToggleEditGrade, 'ENABLE', &
!                    pre=beginitem//'Editing of grades is '//green//'disabled'//black//'.  '//beginbold, &
!                    post=endbold//' it, for advising purposes only; edits will be overwritten when checklists are regenerated.'//enditem))
!            end if

            write(device,AFORMAT) beginitem//'The '//beginbold//Lime//'current'//black//endbold//' term is  '//beginbold// &
                    trim(txtSemester(currentTerm+3))//trim(termQualifier(currentTerm+3))//endbold//','
            if (isPeriodOne) then
                write(device,AFORMAT) trim(make_href(fnSwitchPeriod, 'Advance to advising', A1='Advising', &
                    pre=' registration period. '//beginbold, post=endbold//' period for next term.') )
            else if (isPeriodTwo) then
                write(device,AFORMAT) trim(make_href(fnSwitchPeriod, 'Advance to grade entry', A1='Gradesheets', &
                    pre=' advising period for next term.  '//beginbold, post=endbold//' period for current term.') )
            else if (isPeriodThree) then
                write(device,AFORMAT) trim(make_href(fnSwitchPeriod, 'Advance to preregistration', A1='Preregistration', &
                    pre=' grade entry period. '//beginbold, post=endbold//' period for next term.') )
            else if (isPeriodFour) then
                write(device,AFORMAT) trim(make_href(fnSwitchTerm, 'Advance to registration', A1=itoa(nextTerm), &
                    pre=' preregistration period for next term. '//beginbold, &
                    post=endbold//' period for '//nbsp//Fuchsia//trim(txtSemester(nextTerm+3))//' (next)'// &
                    trim(termQualifier(nextTerm+3))//black//DOT) )
            end if
            write(device,AFORMAT) &
                trim(make_href(fnFileDownload, 'Backup', A1='BACKUP.XML', pre=nbsp//beginbold, post=endbold//' data.')), &
                trim(make_href(fnStop, 'Stop', pre=nbsp//beginbold, post=endbold//SPACE//PROGNAME//DOT//enditem))

            write(device,AFORMAT) beginitem//beginbold//'Online users'//endbold//' are:', &
                trim(make_href(fnOnlineStudents, 'students', pre=nbsp, post=COMMA)), &
                trim(make_href(fnOnlineTeachers, 'teachers', pre=nbsp, post=DOT)), &
                trim(make_href(fnResetPasswords, 'Reset', pre=nbsp//beginbold, &
                post=endbold//SPACE//red//'ALL'//black//' passwords.')), &
                beginbold//'Download'//endbold//' : right-click on link, then "Save Link As..." ', &
                trim(make_href(fnFileDownload, 'PASSWORDS-TEACHERS.CSV', A1='PASSWORDS-TEACHERS.CSV', post=nbsp)), &
                trim(make_href(fnFileDownload, 'PASSWORDS-STUDENTS.CSV', A1='PASSWORDS-STUDENTS.CSV', post=enditem))

            write(device,AFORMAT) beginitem, &
                trim(make_href(fnEditSignatories, 'signatories', pre='Edit the'//nbsp//beginbold, post=endbold// &
                    ' in various forms.')), &
                trim(make_href(fnEditMOTD, 'message of the day', pre=nbsp//'Edit the '//beginbold, post=endbold//DOT)), &
                trim(make_href(fnEditEquivalencies, 'equivalence rules', pre=nbsp//'Edit the '//beginbold, post=endbold//DOT)), &
                enditem

            call make_form_start(device, fnStudentAdd)
            write(device,AFORMAT) &
                beginitem//'<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
                begintr//begintd//beginbold//'Add student number'//endbold//' <input name="A1" value=""> '//nbsp// &
                '<input type="submit" name="action" value="Add">'// &
                endform//endtd//endtr//endtable//enditem//horizontal

        end if

        if (hasSubjects) then
            write(device,AFORMAT) beginitem//beginbold//'Curricular programs'//endbold//' : '
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
            write(device,AFORMAT) enditem
        end if

        ! subjects
        call links_to_subjects(device, coll, numAreas, tArray(1))

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

            write(device,AFORMAT) beginitem//beginbold//'Rooms'//endbold//' in : '
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
            write(device,AFORMAT) enditem
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

            write(device,AFORMAT) beginitem//beginbold//'Teachers'//endbold//' in : '
            do dept=2,NumDepartments
                if (Department(dept)%CollegeIdx /= coll) cycle
                n_count = 0
                do tdx=1,NumTeachers+NumAdditionalTeachers
                    if (Teacher(tdx)%DeptIdx /= dept) cycle
                    if (Teacher(tdx)%Role==SYSAD) cycle
                    n_count = n_count+1
                end do
                if (n_count==0) cycle
                tDepartment = Department(dept)%Code
                write(device,AFORMAT) trim(make_href(fnTeachersByDept, tDepartment, &
                    A1=tDepartment, pre=nbsp, post='('//trim(itoa(n_count))//')'))
            end do
            write(device,AFORMAT) enditem

            if (trim(USERNAME)/=trim(GUEST)) then
                call make_form_start(device, fnFindTeacher)
                write(device,AFORMAT) beginitem//'<table border="0" cellpadding="0" cellspacing="0">', &
                    begintr//begintd//beginbold//'Find teachers'//endbold//' with <input name="A1" value=""> in name. ', &
                    '<input type="submit" name="action" value="Search">'//endform// &
                    endtd//endtr//endtable//enditem
            end if
        end if

        ! students
        hasStudents = CollegeCount(coll)>0
        if (hasStudents) then

            call html_comment('student links()')

            if (.not. (isRoleStudent .or. isRoleGuest) ) then
                call make_form_start(device, fnFindStudent)
                write(device,AFORMAT) beginitem//'<table border="0" cellpadding="0" cellspacing="0">', &
                    begintr//begintd//beginbold//'Find students'//endbold//' with <input name="A1" value="">', &
                    ' in name or number. <input type="submit" name="action" value="Search">', &
                    endform//endtd//endtr//endtable//enditem
            end if

            write(device,AFORMAT) beginitem//beginbold//'Student '//endbold// &
                trim(make_href(fnStudentsDistribution, 'distribution', A1=tCollege))// &
                ' ( '//trim(itoa(CollegeCount(coll)))//' ).'

            if ((isRoleSysAd .or. isRoleOfficial) .and. coll==NumColleges) then
                write(device,AFORMAT) trim(make_href(fnFileDownload, 'ALL student info', &
                    A1='STUDENT-INFO.CSV', A9=currentTerm, pre=nbsp//'Download '//nbsp))
            end if

            if (.not. (isRoleStudent .or. isRoleGuest) ) then

                write(device,AFORMAT) '<ul>'
                ! curriculum-level statistics
                done = .false.
                do idxCurr=1,NumCurricula
                    if (Curriculum(idxCurr)%CollegeIdx /= coll) cycle
                    if (done(idxCurr)) cycle ! done

                    cdx = CurrProgNum(idxCurr)  ! generic program
                    if (ProgramCount(cdx)==0) cycle

                    write(device,AFORMAT) &
                        beginitem//beginbold//trim(CurrProgCode(idxCurr))//endbold//' ('//trim(itoa(ProgramCount(cdx)))//') : '

                    do ldx=1,NumCurricula ! specific curriculum
                        if (CurrProgNum(ldx) /= cdx) cycle
                        if (done(ldx)) cycle
                        if (CurriculumCount(ldx) > 0) then
                            write(device,AFORMAT) trim(make_href(fnAdvisersByCurriculum, Curriculum(ldx)%Code, &
                                A1=Curriculum(ldx)%Code, pre=nbsp, post='('//trim(itoa(CurriculumCount(ldx)))//')'))
                        end if
                        done(ldx) = .true. ! signal done
                    end do
                    if (is_dean_of_college(coll, orHigherUp) ) then
                        write(device,AFORMAT) &
                            trim(make_href(fnSummaryWAG, 'Grade Averages', A1=CurrProgCode(idxCurr), A2='CUMULATIVE', &
                            pre=' : '//beginbold, post=endbold))
                    end if

                    write(device,AFORMAT) enditem

                end do
                write(device,AFORMAT) '</ul>'

            end if
            write(device,AFORMAT) enditem

        end if

        ! per term
        do ldx=termBegin,termEnd

            call qualify_term (ldx, rdx, tdx, description)
                write(device,AFORMAT) beginitem//beginbold//trim(description)//endbold//'<ul>'

            ! blocks
            if (n_curr>0) call links_to_blocks(device, coll, tdx)

            ! classes
            if (numAreas>0) call links_to_sections(device, coll, numAreas, tArray(1), tdx, tArray(numAreas+1) )

            if (isRoleStudent  .or. USERNAME==GUEST .or. .not. hasStudents) then
                write(device,AFORMAT) '</ul>'//enditem
                cycle
            end if

            if (tdx==nextTerm) then ! prediction links

                ! demand for subjects next term
                if (numAreas>0 .and. (.not. isPeriodOne)) then
                    if (isPeriodTwo) then
                        typeNeedsAnalysis = beginbold//'Needs Analysis (probabilistic)'//endbold//' for'
                    else
                        typeNeedsAnalysis = beginbold//'Needs Analysis (deterministic)'//endbold//' for'
                    end if
                    if ((isRoleSysAd .or. isRoleOfficial)) then
                        if (coll==NumColleges) then
                            description = ' for ALL colleges'
                        else
                            description = ' only for '//College(coll)%Code
                        end if
!                        call links_to_depts(device, coll, fnDemandForSubjects, tdx, typeNeedsAnalysis, &
!                            trim(make_href(fnUpdateDemandForSubjects, description, A1=tCollege, A9=tdx, &
!                            post=red//' - click once only, may take a while; use browser''s "Back" button in case of timeout'// &
!                            black)) )
                        call links_to_depts(device, coll, fnDemandForSubjects, tdx, typeNeedsAnalysis)
                        write(device,AFORMAT) beginitem, &
                            trim(make_href(fnResetDemandForSubjects, 'Reset', A1=tCollege, A9=tdx, &
                                pre=beginbold, post=endbold//' the analysis, or ')), &
                            trim(make_href(fnUpdateDemandForSubjects, 'Update', A1=tCollege, A9=tdx, pre=nbsp//beginbold, &
                            post=endbold//trim(description)//red// &
                            ' - click once only, may take a while; use browser''s "Back" button in case of timeout'// &
                            black)), enditem
                    else
                        if (NumEnlistment(tdx)>0) &
                            call links_to_depts(device, coll, fnDemandForSubjects, tdx, typeNeedsAnalysis)
                    end if

                    ! results of Needs Analysis
                    description = SPACE
                    tCollege = SPACE

                    if ((isRoleSysAd .or. isRoleOfficial) .and. coll==NumColleges) then
                        tCollege = 'ALL'
                        description = 'ALL students'

                    else if ( (is_dean_of_college(coll) .and. coll==CollegeIdxUser) .or. is_admin_of_college(coll) )then
                        tCollege = College(coll)%Code
                        description = trim(College(coll)%Code)//' students'

                    else if (requestingTeacher>0) then
                        if (Teacher(requestingTeacher)%NumAdvisees>0 .and. coll==CollegeIdxUser) then
                            tCollege = 'ADVISEES'
                            description = 'My advisees'
                        end if
                    end if

                    if (len_trim(description)>0) then

                        call make_form_start(device, fnStudentPriority, A1=tCollege, A9=tdx)

                        write(device,AFORMAT) beginitem//'<table border="0" cellpadding="0" cellspacing="0">', &
                            begintr//begintd//beginbold//trim(description)//endbold//' <select name="A2">', &
                            '<option value="20"> with NSTP 11/12 track mismatch', &
                            '<option value="10"> with no subjects remaining', &
                            '<option value="2"> with 27 units or less remaining', &
                            '<option value="9"> with no feasible subjects but have units remaining', &
                            '<option value="6"> who failed (75-100%] of units last sem', &
                            '<option value="5"> who failed (50-75%] of units last sem', &
                            '<option value="4"> who failed (0-50%] of units last sem', &
                            '<option value="3"> who did not fail any subject (or on LOA) last term'

                        write(device,AFORMAT) '</select>'//nbsp, &
                            '<input type="submit" name="action" value="Submit">', &
                            endform//endtd//endtr//endtable//enditem
                    end if

                    if (isRoleSysAd .and. isPeriodFour) then

                        tCollege = College(coll)%Code
                        ! preenlist/delist all students in college
                        call make_form_start(device, fnTimetabling, A1=tCollege, A9=tdx)
                        if (coll==NumColleges) then
                            description = SPACE
                        else
                            description = beginbold//trim(tCollege)//endbold
                        end if
                        write(device,AFORMAT) beginitem//'<table border="0" cellpadding="0" cellspacing="0">', &
                            begintr//begintd//'<input type="submit" name="action" value="Preenlist ALL"> '//trim(description)// &
                            ' students'//red// &
                            ' - click once only, may take a while; use browser''s "Back" button in case of timeout. '//black// &
                            nbsp//nbsp//nbsp// &
                            '<input type="submit" name="action" value="Delist ALL"> '//trim(description)//' students.', &
                            endform//endtd//endtr//endtable//enditem

                        ! preenlist/delist selected students in college
                        if (coll/=NumColleges) then
                            call make_form_start(device, fnTimetabling, A1=tCollege, A9=tdx)
                            write(device,AFORMAT) beginitem//'<table border="0" cellpadding="0" cellspacing="0">', &
                                begintr//begintd//'Or, check curricula below and click button to '//beginbold// &
                                'Preenlist/Delist SELECTED'//endbold//' students.'

                            ! per curriculum
                            done = .false.
                            do idxCurr=1,NumCurricula
                                if (Curriculum(idxCurr)%CollegeIdx /= coll) cycle
                                if (done(idxCurr)) cycle ! done

                                cdx = CurrProgNum(idxCurr)  ! generic program
                                if (ProgramCount(cdx)==0) cycle

                                write(device,AFORMAT) linebreak//beginbold//trim(CurrProgCode(idxCurr))//endbold//' students'
                                someCheckboxes = .false.
                                do sdx=1,NumCurricula ! specific curriculum
                                    if (CurrProgNum(sdx) /= cdx) cycle
                                    if (done(sdx)) cycle
                                    ! any blocks open?
                                    hasBlocks = .false.
                                    do blk=1,NumBlocks(tdx)
                                        if (Block(tdx,blk)%CurriculumIdx/=sdx) cycle
                                        hasBlocks = .true.
                                        exit
                                    end do
                                    if (hasBlocks .and. CurriculumCount(sdx)>0) then
                                        write(device,AFORMAT) ' : <input type="checkbox" name="CHECKED:'// &
                                            trim(Curriculum(sdx)%Code)//'">'//Curriculum(sdx)%Code
                                        someCheckboxes = .true.
                                    end if
                                    done(sdx) = .true. ! signal done
                                end do
                                if (.not. someCheckboxes) then
                                    write(device,AFORMAT) ' - no blocked sections?'
                                end if
                            end do

                            write(device,AFORMAT) linebreak//nbsp//nbsp, &
                                '<input type="submit" name="action" value="Preenlist SELECTED"> '// &
                                nbsp//nbsp//nbsp//nbsp//nbsp//nbsp// &
                                '<input type="submit" name="action" value="Delist SELECTED">'//endform, &
                                endtd//endtr//endtable//enditem

                        end if

                    end if

                end if

            end if

            if (tdx/=nextTerm .or. isPeriodFour) then
                tCollege = College(coll)%Code
                write(device,AFORMAT) &
                    trim(make_href(fnEnlistmentSummary, 'enlistment', &
                        A1=tCollege, A9=tdx, pre=beginitem//beginbold//'Summary'//endbold//' of ', post='; ')), &
                    trim(make_href(fnBottleneck, 'demand > available seats', &
                        A1=tCollege, A9=tdx, pre='top subjects for which ', post=COMMA)), &
                    trim(make_href(fnExtraSlots, 'available seats > demand', &
                        A1=tCollege, A9=tdx, pre=nbsp, post='; ')), &
                    trim(make_href(fnUnderloadSummary, 'underloads', &
                        A1=tCollege, A9=tdx, pre=nbsp//'summary of ', post='. ')), &
                    trim(make_href(fnStudentsNotEnrolled, 'without classes', &
                        A1=tCollege, A9=tdx, pre=nbsp//'Students'//nbsp, post=DOT)), &
                    enditem
            end if

            ! allow students to enlist?
            if (isPeriodOne .and. tdx==currentTerm .and. (isRoleSysAd .or. isRoleOfficial) .and. CollegeCount(coll)>0) then
                if ( College(coll)%AllowEnlistmentByStudents(tdx) ) then
                    write(device,AFORMAT) beginitem//'Students are  '//beginbold//red//'allowed'//black//endbold// &
                        ' to self-enlist. ', &
                        trim(make_href(fnAllowStudentsToEnlist, 'DISALLOW', &
                        A1=tCollege, A9=tdx, pre=beginbold//green, post=endbold//' self-enlistment by students.'//black))
                else
                    write(device,AFORMAT) beginitem//'Students are '//beginbold//green//'disallowed'//black//endbold// &
                        ' from self-enlisting. ', trim(make_href(fnAllowStudentsToEnlist, 'ALLOW', &
                        A1=tCollege, A9=tdx, pre=beginbold//red, post=endbold//' students to self-enlist.'//black))
                end if
            end if

            if (coll==NumColleges .and. (isRoleSysAd .or. isRoleOfficial) ) then
                if (NumBlocks(tdx)+NumSections(tdx)>0) then
                    write(device,AFORMAT) beginitem//beginbold//'Download'//endbold// &
                        ' : right-click on link, then "Save Link As..." ', &
                        trim(make_href(fnFileDownload, 'CLASSES-BLOCKS-'//trim(txtSemester(tdx))//'.CSV', &
                        A1='CLASSES-BLOCKS.CSV', A9=tdx))
                    if (NumEnlistment(tdx)>0) then
                        write(device,AFORMAT) &
                            trim(make_href(fnFileDownload, 'ENLISTMENT-'//trim(txtSemester(tdx))//'.CSV', &
                            A1='ENLISTMENT.CSV', A9=tdx, pre=nbsp//nbsp))
                    end if
                    write(device,AFORMAT) enditem
                end if
            end if

            write(device,AFORMAT) '</ul>'//enditem

        end do

        write(device,AFORMAT) '</ul>'//horizontal

    end subroutine html_college_info


    subroutine links_to_depts(device, coll, fn, term, header, footer)
        integer, intent (in) :: device, coll, fn, term
        character(len=*), intent (in) :: header
        character(len=*), intent (in), optional :: footer
        integer :: dept, crse, n_count
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        call html_comment('links_to_depts()')

        write(device,AFORMAT) beginitem//header//' : '
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
            write(device,AFORMAT) trim(make_href(fn, tDepartment, A1=tDepartment, A9=term, post=nbsp))
        end do
        if (present(footer)) then
            if (len_trim(footer)>0) write(device,AFORMAT) nbsp//footer
        end if
        write(device,AFORMAT) enditem

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

        call html_comment('links_to_subjects('//College(coll)%Code//')')

#if defined REGIST
        write(device,AFORMAT) beginitem//beginbold//'Subjects'//endbold//' in :'
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
        write(device,AFORMAT) enditem
#endif
        write(device,AFORMAT) beginitem//beginbold//'Subjects'//endbold//' by area :'
        do dept=1,numAreas
            tSubject = SubjectArea(AreaList(dept))%Code
            n_count = SubjectArea(AreaList(dept))%Count
            write(device,AFORMAT) trim(make_href(fnSubjectList, tSubject, A1=tSubject, &
                pre=nbsp, post='('//trim(itoa(n_count))//')'))
        end do
        write(device,AFORMAT) enditem

    end subroutine links_to_subjects


    subroutine links_to_sections(device, coll, numAreas, AreaList, thisTerm, AreaCount)
        integer, intent (in) :: device, coll, numAreas, thisTerm
        integer, intent (in) :: AreaList(1:numAreas)
        integer, intent (out) :: AreaCount(1:numAreas)
        integer :: ddx, sect, n_count, m_count, mdx, k1, k2, idxArea
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        tCollege = College(coll)%Code
        call html_comment('links_to_sections('//tCollege//')')
        AreaCount(1:numAreas) = 0

#if defined REGIST
        ddx = 0
#else
        tDepartment = tCollege
        ddx = index_to_dept(tDepartment)
#endif

#if defined REGIST
        write(device,AFORMAT) beginitem//beginbold//'Classes'//endbold//' in : '
        do idxArea=2,NumDepartments
            if (Department(idxArea)%CollegeIdx /= coll) cycle
            n_count = 0
            do k1=1,NumSubjects+NumAdditionalSubjects
                if (Subject(k1)%DeptIdx /= idxArea) cycle
                n_count = n_count+1
                exit
            end do
            if (n_count==0) cycle ! no subjects in this department

            ! how many sections currently open
            n_count = 0
            do sect=1,NumSections(thisTerm)
                if (idxArea/=Section(thisTerm,sect)%DeptIdx) cycle ! not in department
                if (Section(thisTerm,sect)%SubjectIdx==0) cycle ! deleted
                n_count = n_count+1
            end do
            tDepartment = Department(idxArea)%Code
            write(device,AFORMAT) trim(make_href(fnScheduleOfClasses, tDepartment, &
                A1=tDepartment, A9=thisTerm, post='('//trim(itoa(n_count))//')'//nbsp))
        end do

        write(device,AFORMAT) enditem
#endif

        ! count how many sections per subject area
        do idxArea=1,numAreas
            ! the code
            tSubject = SubjectArea(AreaList(idxArea))%Code
            k1 = len_trim(tSubject)+1
            n_count = 0 ! how many sections with this code currently open
            do sect=1,NumSections(thisTerm)
#if defined REGIST
                if (Section(thisTerm,sect)%ClassId(:k1)==tSubject(:k1)) n_count = n_count+1
#else
                if (Section(thisTerm,sect)%ClassId(:k1)==tSubject(:k1) .and. &
                    ddx==Section(thisTerm,sect)%DeptIdx) n_count = n_count+1
#endif
            end do
            AreaCount(idxArea) = n_count ! number of sections in area
        end do

        if (sum(AreaCount(1:numAreas))==0) return ! no classes in this college

        write(device,AFORMAT) beginitem//beginbold//'Classes by subject area'//endbold//' : '
        do idxArea=1,numAreas
            n_count = AreaCount(idxArea) ! number of sections in area
            if (n_count==0) cycle ! skip area if none
            ! the code
            tSubject = SubjectArea(AreaList(idxArea))%Code
            write(device,AFORMAT) trim(make_href(fnScheduleByArea, tSubject, &
                A1=tCollege, A2=tSubject, A9=thisTerm, post='('//trim(itoa(n_count))//')'//nbsp))
        end do
        write(device,AFORMAT) enditem

        write(device,AFORMAT) beginitem//beginbold//'Classes with '//endbold
        n_count = 0 ! how many sections with TBA teachers
        m_count = 0 ! how many sections with TBA rooms
        do sect=1,NumSections(thisTerm)
            if ( coll/=Department(Section(thisTerm,sect)%DeptIdx)%CollegeIdx ) cycle
            k1 = 0
            k2 = 0
            do mdx=1,Section(thisTerm,sect)%NMeets
                if (Section(thisTerm,sect)%TeacherIdx(mdx)==0) k1 = k1+1
                if (Section(thisTerm,sect)%RoomIdx(mdx)==0) k2 = k2+1
            end do
            if (k1>0) n_count = n_count+1
            if (k2>0) m_count = m_count+1
        end do
        write(device,AFORMAT) trim(make_href(fnTBATeachers, 'TBA teachers', A1=tCollege, A9=thisTerm, &
            pre=nbsp, post='('//trim(itoa(n_count))//')')), &
            trim(make_href(fnTBARooms, 'TBA rooms', A1=tCollege, A9=thisTerm, &
                pre=nbsp, post='('//trim(itoa(m_count))//')'))

        if (.not. (isRoleGuest .or. isRoleStudent) .and. &
             ( (isRoleSysAd .or. isRoleOfficial .or. isRoleStaff)  .or. &
               ( (.not. isPeriodOne) .and. thisTerm==currentTerm) .or. &
               (isPeriodOne .and. thisTerm==cTm1) ) ) write(device,AFORMAT) &
            trim(make_href(fnUnpostedGrades, 'unposted grades', A1=tCollege, A9=thisTerm, pre=nbsp)), &
            trim(make_href(fnGradesheetsToReceive, 'no hardcopy of gradesheet', A1=tCollege, A9=thisTerm, pre=nbsp))

        write(device,AFORMAT) enditem//beginitem//beginbold//'Conflicts in schedules of '//endbold, &
            trim(make_href(fnTeacherConflicts, 'teachers', A1=tCollege, A9=thisTerm, pre=nbsp)), &
            trim(make_href(fnRoomConflicts, 'rooms', A1=tCollege, A9=thisTerm, pre=nbsp)), &
            trim(make_href(fnBlockConflicts, 'blocks', A1=tCollege, A9=thisTerm, pre=nbsp))
        if (.not. (isRoleGuest .or. isRoleStudent) ) then
            write(device,AFORMAT) &
                trim(make_href(fnStudentsWithConflicts, 'students', A1=tCollege, A9=thisTerm, pre=nbsp))
        end if
        write(device,AFORMAT) enditem

    end subroutine links_to_sections


    subroutine links_to_blocks(device, coll, thisTerm)
        integer, intent (in) :: device, coll, thisTerm
        integer :: cdx, ldx, blk

        call html_comment('links_to_blocks('//College(coll)%Code//')')
        write(device,AFORMAT) beginitem//beginbold//'Blocks'//endbold//' : '//nbsp
        done = .false.
        do cdx=1,NumCurricula-1
            if (Curriculum(cdx)%CollegeIdx /= coll) cycle
            if (done(cdx)) cycle
            if (Curriculum(cdx)%NumTerms==0) cycle
            ldx = 0
            do blk=1,NumBlocks(thisTerm)
                if (CurrProgCode(Block(thisTerm,blk)%CurriculumIdx)/=CurrProgCode(cdx)) cycle
                ldx = ldx+1
            end do
            write(device,AFORMAT) trim(make_href(fnBlockList, CurrProgCode(cdx), &
                A1=CurrProgCode(cdx), A9=thisTerm, post='('//trim(itoa(ldx))//')'//nbsp))
            do ldx=cdx+1,NumCurricula-1
                if (CurrProgCode(ldx) == CurrProgCode(cdx)) done(ldx) = .true.
            end do
        end do
        write(device,AFORMAT) enditem

    end subroutine links_to_blocks


    subroutine blocks_in_section(device, sect, fn, thisTerm, NumBlocks, Block)
        integer, intent(in) :: device, sect, fn, thisTerm, NumBlocks
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        integer :: idx, jdx

        call html_comment('blocks_in_section()')

        do idx=1,NumBlocks
            do jdx=1,Block(idx)%NumClasses
                if (Block(idx)%Section(jdx)/=sect) cycle
                if (fn>0) then
                    write(device,AFORMAT) trim(make_href(fn, Block(idx)%BlockID, A1=Block(idx)%BlockID, A9=thisTerm))
                else
                    write(device,AFORMAT) trim(Block(idx)%BlockID)
                end if
                exit
            end do
        end do

    end subroutine blocks_in_section


    subroutine list_students(device, thisTerm, n_count, sList, crse, eList)
        integer, intent (in) :: device, thisTerm, n_count, sList(n_Count), crse
        type (TYPE_PRE_ENLISTMENT), intent(in) :: eList(0:)
        integer :: sect, ldx, tdx, std, ncol, fac, nClasses(3), nAdvised(3)
        character (len=MAX_LEN_SUBJECT_CODE) :: tNum
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo

        call html_comment('list_students()')

        if (n_count == 0) then
            write(device,AFORMAT) BRNONE
        else
            write(device,AFORMAT) '<table border="0" width="100%">', &
                begintr//thalignleft//'#'//endth// &
                thalignleft//'CONTRIB'//endth// &
                thalignleft//'STDNO'//endth// &
                thalignleft//'NAME OF STUDENT'//endth// &
                thalignleft//'PROGRAM'//endth// &
                tdnbspendtd// &
                endtr
            do tdx=1,n_count
                std = sList(tdx)
                tStdNo = Student(std)%StdNo
                ldx = Student(std)%CurriculumIdx
                fac = index_to_teacher(Student(std)%Adviser)

                call count_preenlistment(std, 0, nClasses, nAdvised)

                ! find contribution
                do ncol=1,eList(std)%NPriority+eList(std)%NAlternates+eList(std)%NCurrent
                    if (crse==eList(std)%Subject(ncol)) exit
                end do
                write(tNum, '(f6.4)') eList(std)%Contrib(ncol)

                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(tdx,2))//'">'// & ! begintr// &
                    tdaligncenter//trim(itoa(tdx))//DOT//endtd// &
                    begintd//trim(tNum)//endtd// &
                    begintd//tStdNo//endtd// &
                    begintd//trim(Student(std)%Name)//endtd// &
                    begintd//trim(Curriculum(ldx)%Code)//endtd

                if ( is_adviser_of_student(std,orHigherUp) ) then

                    write(device,AFORMAT) begintd//beginsmall
                    if (isRoleSysAd .or. isRoleOfficial .or. isRoleDean .or. isRoleStaff) then
                        write(device,AFORMAT) &
                            trim(make_href(fnStudentEdit, 'Info', A1=tStdNo, A9=thisTerm, pre=nbsp)), &
                            trim(make_href(fnRecentStudentActivity, 'Log', A1=tStdNo, A9=thisTerm, pre=nbsp))
                    end if

                    write(device,AFORMAT)  &
                        trim(make_href(fnTeacherClasses, 'Adviser', A1=Teacher(fac)%TeacherId, A9=currentTerm, pre=nbsp)), &
                        trim(make_href(fnStudentGrades, 'Grades', A1=tStdNo, A9=thisTerm, pre=nbsp)), &
                        trim(make_href(fnEditCheckList, 'Checklist', A1=tStdNo, A9=thisTerm, pre=nbsp))

                    do sect=termBegin,termEnd
                        call qualify_term (sect, ldx, ncol)
                        if (nClasses(ncol)+nAdvised(ncol)>0) write(device,AFORMAT) &
                            trim(make_href(fnStudentClasses, txtSemester(ncol+6), A1=tStdNo, A9=ncol, pre=nbsp))
                    end do

                end if
                write(device,AFORMAT) endsmall//endtd//endtr

            end do
            write(device,AFORMAT) endtable
        end if

    end subroutine list_students


    subroutine student_distribution(device)
        integer, intent (in), optional :: device
        integer :: CollegeCount(0:MAX_ALL_COLLEGES), ProgramCount(0:MAX_ALL_CURRICULA), CurriculumCount(0:MAX_ALL_CURRICULA)
        integer :: cdx, gdx, ierr, ldx, sdx, n, maxcount, idxCurr
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        !character (len=255) :: longline

        call html_comment('student_distribution()')

        ! which unit ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        targetCollege = index_to_college(tCollege)
        if (ierr/=0 .or. targetCollege==0) then
            targetDepartment = DeptIdxUser
            targetCollege = CollegeIdxUser
            call html_write_header(device, 'Add block', linebreak//'College"'//trim(tCollege)//'" not found')
            return
        end if
        ! no. of students
        ProgramCount= 0 ! in a degree program
        CurriculumCount= 0 ! in a specific curriculum
        CollegeCount = 0 ! per unit
        do sdx=1,NumStudents+NumAdditionalStudents
            ldx = Student(sdx)%CurriculumIdx
            CurriculumCount(ldx) = CurriculumCount(ldx)+1
            cdx = CurrProgNum(ldx)
            n = Curriculum(ldx)%CollegeIdx
            ProgramCount(cdx) = ProgramCount(cdx)+1
            CollegeCount(n) = CollegeCount(n) + 1
        end do

        call html_write_header(device, 'Distribution of students')

        if (tCollege==SYSAD) then
            write(device,AFORMAT) '<table width="100%" cellspacing="0" cellpadding="0">', &
                begintr//'<td width="10%">.'//endtd// &
                '<td width="60%">'//beginbold//'DISTRIBUTION OF STUDENTS'//endbold//endtd// &
                '<td width="10%" align="right">count'//endtd// &
                '<td width="10%" align="right">% coll'//endtd// &
                '<td width="10%" align="right">% univ'//endtd//endtr
            write(device,AFORMAT) &
                begintr//'<td width="10%">.'//endtd// &
                '<td width="60%" align="right">'//beginbold//'Total no. of students in '//UniversityCode//endbold//endtd// &
                '<td width="10%" align="right"> '//beginbold//trim(itoa(NumStudents+NumAdditionalStudents))//endbold//endtd// &
                '<td width="10%" align="right">.'//endtd//'<td width="10%" align="right">'//beginbold//'100'//endbold//endtd// &
                endtr
            write(device,AFORMAT) &
                begintr//'<td width="10%">College'//endtd// &
                '<td width="60%">Percent, relative to total in university'//endtd// &
                '<td width="10%" align="right">.'//endtd//'<td width="10%" align="right">.'//endtd// &
                '<td width="10%" align="right">.'//endtd// &
                endtr
            maxcount = NumStudents+NumAdditionalStudents
            do gdx=1,NumColleges
                if (CollegeCount(gdx) <= 0) cycle

                ! code
                write(device,AFORMAT) trim(make_href(fnStudentsDistribution, College(gdx)%Code, &
                    A1=College(gdx)%Code, &
                    pre=begintr//'<td width="10%">'//beginsmall, post=endsmall//endtd))

                ! bar chart
                write(device,AFORMAT) &
                    '<td width="60%"><table width="100%" border="0" cellpadding="0">'//begintr
                n = (100*CollegeCount(gdx))/maxcount! relative width
                if (CollegeCount(gdx)>0) then
                    write(device,AFORMAT) '<td width="'//trim(itoa(n))//'%" bgcolor="blue">.'//endtd
                else
                    write(device,AFORMAT) begintd//DOT//endtd
                end if
                ! empty filler
                n = max( 100*(maxcount-CollegeCount(gdx))/maxcount, 1)
                write(device,AFORMAT) '<td width="'//trim(itoa(n))//'%">.'//endtd//endtr//endtable
                ! count
                write(device,AFORMAT) tdalignright//trim(itoa(CollegeCount(gdx)))//endtd
                ! % in unit and % in university
                write(device,'(a,f6.2,a)') &
                    '<td width="10%" align="right">100'//endtd// &
                    '<td width="10%" align="right">', 100.0*CollegeCount(gdx)/maxcount, endtd//endtr
            end do
            write(device,AFORMAT) endtable

        else

            if (CollegeCount(targetCollege) <= 0) then
                write(device,AFORMAT) linebreak//'No students in this unit.'//linebreak//horizontal
                return
            end if
            ! program-level statistics
            maxcount = CollegeCount(targetCollege)
            !maxcount = 0
            !do ldx=1,NumCurricula
            !  if (Curriculum(ldx)%CollegeIdx /= targetCollege) cycle
            !  cdx = CurrProgNum(ldx)
            !  if (ProgramCount(cdx) > maxcount) maxcount = ProgramCount(cdx)
            !end do
            write(device,AFORMAT) '<table width="100%" cellspacing="0" cellpadding="0">', &
                begintr//'<td width="10%">.'//endtd// &
                '<td width="60%">'//beginbold//'DISTRIBUTION OF STUDENTS IN '//trim(College(targetCollege)%Code)//endbold//endtd// &
                '<td width="10%" align="right">count'//endtd// &
                '<td width="10%" align="right">% coll'//endtd// &
                '<td width="10%" align="right">% univ'//endtd//endtr
            write(device,AFORMAT) &
                begintr//'<td width="10%">.'//endtd// &
                '<td width="60%" align="right">'//beginbold//'Total no. of students in '//UniversityCode//endbold//endtd// &
                '<td width="10%" align="right"> '//beginbold//trim(itoa(NumStudents+NumAdditionalStudents))//endbold//endtd// &
                '<td width="10%" align="right">.'//endtd//'<td width="10%" align="right">.'//endtd// &
                endtr
            write(device,'(a,f6.2,a)') &
                begintr//'<td width="10%">.'//endtd// &
                '<td width="60%" align="right">'//beginbold//'Total no. of students in '// &
                    trim(College(targetCollege)%Code)//endbold//endtd// &
                '<td width="10%" align="right"> '//beginbold//trim(itoa(CollegeCount(targetCollege)))//endbold//endtd// &
                '<td width="10%" align="right">'//beginbold//'100'//endbold//endtd// &
                '<td width="10%" align="right"> '//beginbold, &
                    100.0*CollegeCount(targetCollege)/(NumStudents+NumAdditionalStudents), &
                endbold//endtd// &
                begintr
            write(device,AFORMAT) &
                begintr//'<td width="10%">Program'//endtd// &
                '<td width="60%">Percent of students in program, relative to total in unit'//endtd// &
                '<td width="10%" align="right">.'//endtd//'<td width="10%" align="right">.'//endtd// &
                '<td width="10%" align="right">.'//endtd// &
                endtr

            done = .false.
            do ldx=1,NumCurricula
                if (Curriculum(ldx)%CollegeIdx /= targetCollege) cycle
                cdx = CurrProgNum(ldx)
                if (done(cdx)) cycle ! done
                ! code
                write(device,AFORMAT) &
                    begintr//'<td width="10%">'//beginsmall//'<a href="#'//trim(CurrProgCode(ldx))//'">'// &
                    CurrProgCode(ldx)//'</a>'//endsmall//endtd
                ! bar chart
                write(device,AFORMAT) &
                    '<td width="60%"><table width="100%" border="0" cellpadding="0">'//begintr
                n = (100*ProgramCount(cdx))/maxcount ! relative width
                if (ProgramCount(cdx)>0) then
                    write(device,AFORMAT) '<td width="'//trim(itoa(n))//'%" bgcolor="blue">.'//endtd
                else
                    write(device,AFORMAT) begintd//DOT//endtd
                end if
                ! empty filler
                n = max( 100*(maxcount-ProgramCount(cdx))/maxcount, 1)
                write(device,AFORMAT) '<td width="'//trim(itoa(n))//'%">.'//endtd//endtr//endtable
                ! count
                write(device,AFORMAT) tdalignright//trim(itoa(ProgramCount(cdx)))//endtd
                ! % in unit and % in university
                write(device,'(a,f6.2,a)') &
                    '<td width="10%" align="right">', 100.0*ProgramCount(cdx)/CollegeCount(targetCollege), endtd, &
                    '<td width="10%" align="right">', 100.0*ProgramCount(cdx)/(NumStudents+NumAdditionalStudents), endtd//endtr
                done(cdx) = .true. ! signal done
            end do
            write(device,AFORMAT) endtable//linebreak

            ! curriculum-level statistics
            done = .false.
            do idxCurr=1,NumCurricula
                if (Curriculum(idxCurr)%CollegeIdx /= targetCollege) cycle
                if (done(idxCurr)) cycle ! done

                cdx = CurrProgNum(idxCurr)  ! generic program
                maxcount = max(ProgramCount(cdx),1)

                write(device,AFORMAT) '<p><a name="'//trim(CurrProgCode(idxCurr))//'"></a>'//linebreak//horizontal
                write(device,AFORMAT) '<table width="100%" cellspacing="0" cellpadding="0">', &
                    begintr//'<td width="10%">.'//endtd// &
                    '<td width="60%">'//beginbold//'Detailed distribution of students in program '// &
                        CurrProgCode(idxCurr)//endbold//endtd// &
                    '<td width="10%" align="right">count'//endtd// &
                    '<td width="10%" align="right">% coll'//endtd// &
                    '<td width="10%" align="right">% univ'//endtd//endtr
                write(device,'(a,f6.2,a)') &
                    begintr//'<td width="10%">.'//endtd// &
                    '<td width="60%" align="right">'//beginbold//'Total no. of students in '// &
                    trim(College(targetCollege)%Code)//endbold//endtd// &
                    '<td width="10%" align="right"> '//beginbold//trim(itoa(CollegeCount(targetCollege)))//endbold//endtd// &
                    '<td width="10%" align="right">'//beginbold//'100'//endbold//endtd// &
                    '<td width="10%" align="right"> '//beginbold, &
                    100.0*CollegeCount(targetCollege)/(NumStudents+NumAdditionalStudents), endbold//endtd// &
                    begintr
                write(device,'(a,f6.2,a)') &
                    begintr//'<td width="10%">.'//endtd// &
                    '<td width="60%" align="right">'//beginbold//'Total no. of students in '// &
                        trim(CurrProgCode(idxCurr))//endbold//endtd// &
                    '<td width="10%" align="right">'//trim(itoa(ProgramCount(cdx)))//endtd// &
                    '<td width="10%" align="right">', 100.0*ProgramCount(cdx)/CollegeCount(targetCollege), endtd, &
                    '<td width="10%" align="right">', 100.0*ProgramCount(cdx)/(NumStudents+NumAdditionalStudents), endtd//endtr

                write(device,AFORMAT) &
                    begintr//'<td width="10%">Curriculum'//endtd// &
                    '<td width="60%">Percent of students in curriculum, relative to total in program'//endtd// &
                    '<td width="10%" align="right">.'//endtd//'<td width="10%" align="right">.'//endtd// &
                    '<td width="10%" align="right">.'//endtd// &
                    endtr

                do ldx=1,NumCurricula ! specific curriculum
                    if (CurrProgNum(ldx) /= cdx) cycle
                    ! code
                    write(device,AFORMAT) &
                        trim(make_href(fnCurriculum, Curriculum(ldx)%Code, &
                        A1=Curriculum(ldx)%Code, &
                        pre=begintr//'<td width="10%">'//beginsmall, post=endsmall//endtd))
                    ! bar chart
                    write(device,AFORMAT) &
                        '<td width="60%"><table width="100%" border="0" cellpadding="0">'//begintr
                    n = (100*CurriculumCount(ldx))/maxcount ! relative width
                    if (CurriculumCount(ldx)>0) then
                        write(device,AFORMAT) '<td width="'//trim(itoa(n))//'%" bgcolor="blue">.'//endtd
                    else
                        write(device,AFORMAT) begintd//DOT//endtd
                    end if
                    ! empty filler
                    n = max( 100*(maxcount-CurriculumCount(ldx))/maxcount, 1)
                    write(device,AFORMAT) '<td width="'//trim(itoa(n))//'%">.'//endtd//endtr//endtable
                    ! count
                    !write(device,AFORMAT) tdalignright//trim(itoa(CurriculumCount(ldx)))//endtd
                    if (CurriculumCount(ldx) .gt. 0) then
                        if (.not. (iSRoleStudent .or. isRoleGuest)) then
                            write(device,AFORMAT) trim(make_href(fnStudentsByCurriculum, itoa(CurriculumCount(ldx)), &
                                A1=Curriculum(ldx)%Code, &
                                pre=tdalignright, post=endtd))
                        else
                            write(device,AFORMAT) tdalignright//itoa(CurriculumCount(ldx))//endtd
                        end if
                    else
                        write(device,AFORMAT) tdalignright//'0'//endtd
                    end if
                    ! % in unit and % in university
                    write(device,'(a,f6.2,a)') &
                        '<td width="10%" align="right">', 100.0*CurriculumCount(ldx)/CollegeCount(targetCollege), endtd, &
                        '<td width="10%" align="right">', 100.0*CurriculumCount(ldx)/(NumStudents+NumAdditionalStudents), &
                        endtd//endtr

                    done(ldx) = .true. ! signal done
                end do
                write(device,AFORMAT) endtable

            end do

        end if
        write(device,AFORMAT) horizontal

    end subroutine student_distribution


    subroutine class_list (device, thisTerm, NumSections, Section, eList, idx, mesg)

        integer, intent (in) :: device, thisTerm, NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in) :: eList(0:)
        integer, intent (in), optional :: idx
        character(len=*), intent (in), optional :: mesg

        integer :: n_count, tdx, ierr, ncol, crse
        character(len=MAX_LEN_CLASS_ID) :: tClassId
        character(len=255) :: header, errMsg

        call html_comment('class_list()')
        if (present(mesg)) then
            errMsg = mesg
        else
            errMsg = SPACE
        end if

        ! which section?
        if (present(idx)) then
            targetSection = idx
            tClassId = Section(idx)%ClassId
        else
            call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, ierr)
            targetSection = index_to_section(tClassId, NumSections, Section)
        end if
        crse = Section(targetSection)%SubjectIdx
#if defined REGIST
        targetDepartment = Subject(crse)%DeptIdx
#else
        targetDepartment = Section(targetSection)%DeptIdx
#endif
        targetCollege = Department(targetDepartment)%CollegeIdx

        call collect_students_in_section (targetSection, NumSections, Section, eList, n_count, tArray)
!        ! collect students
!        n_count = 0
!        do tdx=1,NumStudents+NumAdditionalStudents
!            std = StdRank(tdx)
!            do ncol=1,eList(std)%lenSubject
!                sect = eList(std)%Section(ncol)
!                if (sect==0) cycle
!                if (targetSection == sect) then
!                    n_count = n_count+1
!                    tArray(n_count) = std
!                    exit
!                elseif (eList(std)%Subject(ncol)==crse .and. is_lecture_lab_subject(crse)) then
!                    ldx = index(Section(sect)%ClassId,DASH)
!                    if (ldx>0) then ! student is accommodated in a lab section
!                        if (trim(tClassId)==Section(sect)%ClassId(:ldx-1)) then ! lab of lecture
!                            n_count = n_count+1
!                            tArray(n_count) = std
!                        end if
!                    end if
!                end if
!            end do
!        end do

        if (is_lecture_lab_subject(crse)) then
            header = trim(ftoa(Subject(crse)%LectHours+Subject(crse)%LabHours,2))//' hrs ('// &
                trim(ftoa(Subject(crse)%LectHours,2))//' lect + '// &
                trim(ftoa(Subject(crse)%LabHours,2))//' lab/recit).'
        else if (Subject(crse)%LectHours > 0.0) then
            header = trim(ftoa(Subject(crse)%LectHours,2))//' hrs lect.'
        else if (Subject(crse)%LabHours > 0.0) then
            header = trim(ftoa(Subject(crse)%LabHours,2))//' hrs lab/recit.'
        end if
        header = trim(Subject(crse)%Title)//'/ '//trim(ftoa(Subject(crse)%Units,1))//' units/ '//header

        ncol = n_count
        do tdx=1,Section(targetSection)%NMeets
            tArray(ncol+1) = targetSection
            tArray(ncol+2) = tdx
            tArray(ncol+3) = 0
            ncol = ncol+3
        end do
        tArray(ncol+1) = 0
        tArray(ncol+2) = 0
        tArray(ncol+3) = 0

        call html_write_header(device, 'CLASSLIST', errMsg)
        call list_sections_to_edit(device, thisTerm, Section, ncol-n_count, tArray(n_count+1:), 0, SPACE, SPACE, .false., &
            beginbold//trim(Subject(crse)%Name)//' - '//trim(header)//endbold//SPACE// &
            trim(make_href(fnPrintableClasslist, 'Printable', &
                    A1=tClassId, A9=thisTerm, pre=nbsp//beginsmall//'( ', post=' )'//endsmall//linebreak//linebreak)) )

        call html_student_list (device, n_count, tArray)
        write(device,AFORMAT) horizontal

        if ( (isPeriodOne .and. is_teacher_of_class(Section(targetSection),orHigherUp)) .or. &
              is_admin_of_college(Department(Section(targetSection)%DeptIdx)%CollegeIdx) ) then
            call make_form_start(device, fnStudentForceEnlist, A9=thisTerm)
            write(device,AFORMAT) &
                '<input type="hidden" name="A2" value="'//trim(tClassId)//'">', &
                '<table border="0" width="60%" cellpadding="0" cellspacing="0">', &
                begintr//begintd//beginbold//'Student number'//endbold//': <input name="A1" value=""> '//nbsp// &
                '<input type="submit" name="action" value="Add">'//nbsp//nbsp//nbsp// &
                ' or '//nbsp//nbsp//nbsp, &
                '<input type="submit" name="action" value="Delete"> (Specify ALL to delete all students)'// &
                endform//endtd//endtr//endtable//horizontal
        end if

    end subroutine class_list


    subroutine collect_advice(Advice, n_changes, mesg)

        type (TYPE_PRE_ENLISTMENT), intent(out) :: Advice
        character(len=*), intent (out) :: mesg
        integer, intent (out) :: n_changes

        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=3*MAX_LEN_SUBJECT_CODE) :: search_string

        integer :: crse, idx, jdx, ierr, i, j, l
        real :: tUnits

        call html_comment('collect_advice()')

        call initialize_pre_enlistment(Advice)
        mesg = SPACE
        n_changes = 0

        call cgi_get_named_float(QUERY_STRING, 'earned', tUnits, ierr)
        if (ierr==0) Advice%UnitsEarned = tUnits
        call cgi_get_named_integer(QUERY_STRING, 'classification', i, ierr)
        if (ierr==0) Advice%StdClassification = i
        call cgi_get_named_integer(QUERY_STRING, 'year', i, ierr)
        if (ierr==0) Advice%StdYear = i
        call cgi_get_named_float(QUERY_STRING, 'allowed', tUnits, ierr)
        if (ierr==0) Advice%AllowedLoad = tUnits
        call cgi_get_named_integer(QUERY_STRING, 'errnstp', i, ierr)
        if (ierr==0) Advice%errNSTP = i
        call cgi_get_named_integer(QUERY_STRING, 'group', i, ierr)
        if (ierr==0) Advice%StdPriority = i
        call cgi_get_named_integer(QUERY_STRING, 'priority', i, ierr)
        if (ierr==0) Advice%NPriority = i
        call cgi_get_named_integer(QUERY_STRING, 'alternate', i, ierr)
        if (ierr==0) Advice%NAlternates = i
        call cgi_get_named_integer(QUERY_STRING, 'current', i, ierr)
        if (ierr==0) Advice%NCurrent = i

        Advice%lenSubject = Advice%NPriority+Advice%NAlternates+Advice%NCurrent
        do l=1,Advice%lenSubject
            call cgi_get_named_string(QUERY_STRING, 'pri'//itoa(l), search_string, ierr)
            j = index(search_string,COMMA)
            tSubject = search_string(j+1:)
            read(tSubject,'(f6.4)') Advice%Contrib(l)
            tSubject = search_string(1:j-1)
            crse = index_to_subject(tSubject)
            Advice%Subject(l) = crse
        end do

        ! apply deletes
        idx = Advice%NPriority
        jdx = Advice%NAlternates
        do l=1,Advice%lenSubject
            call cgi_get_named_string(QUERY_STRING, 'del'//itoa(l), search_string, ierr)
            if (ierr==-1) cycle ! not found
            if (l<=Advice%NPriority) then
                idx = idx-1
            elseif (l<=Advice%NPriority+Advice%NAlternates) then
                jdx = jdx-1
            end if
            crse = Advice%Subject(l)
            mesg = trim(mesg)//' : Del '//Subject(crse)%Name
            Advice%Contrib(l) = 0.0
            Advice%Subject(l) = 0
            n_changes = n_changes + 1
            call html_comment('Delete '//itoa(l)//SPACE//Subject(crse)%Name )

            ! check if deleted subject is in WaiverCOI
            i = 0
            do j=1,WaiverCOI(targetStudent)%lenSubject
                if (WaiverCOI(targetStudent)%Subject(j)/=crse) cycle
                i = j
                exit
            end do
            if (i/=0) then ! remove from WaiverCOI()
                do j=i,WaiverCOI(targetStudent)%lenSubject-1
                    WaiverCOI(targetStudent)%Subject(j) = WaiverCOI(targetStudent)%Subject(j+1)
                end do
                WaiverCOI(targetStudent)%lenSubject = WaiverCOI(targetStudent)%lenSubject-1
                isDirtyWaiverCOI = .true.
                !write(device,*) '  from WaiverCOI() also'
            end if
        end do
        if (n_changes>0) then ! there were deletions
            j = 0
            do l=1,Advice%lenSubject
                if (Advice%Subject(l)>0) then
                    j = j+1
                    Advice%Subject(j) = Advice%Subject(l)
                    Advice%Contrib(j) = Advice%Contrib(l)
                end if
            end do
            Advice%NPriority = idx
            Advice%NAlternates = jdx
            Advice%lenSubject = Advice%lenSubject-n_changes !  n_changes = no. of deletions
        end if

        ! check if subject added (COI, Waived prereq)
        call cgi_get_named_string(QUERY_STRING, 'additional', search_string, ierr)
        if (search_string/=SPACE) then ! something there
            tSubject = search_string
            crse = index_to_subject(tSubject)
            if (crse>=0) then ! add
                call html_comment('Add '//Subject(crse)%Name )
                mesg = trim(mesg)//' : Add '//tSubject
                do l=Advice%lenSubject,1,-1
                    Advice%Subject(l+1) = Advice%Subject(l)
                    Advice%Contrib(l+1) = Advice%Contrib(l)
                end do
                Advice%Subject(1) = crse
                Advice%Contrib(1) = 1.0
                Advice%NPriority = 1+Advice%NPriority
                Advice%lenSubject = 1+Advice%lenSubject
                ! add to WaiverCOI
                l = WaiverCOI(targetStudent)%lenSubject+1
                WaiverCOI(targetStudent)%lenSubject = l
                WaiverCOI(targetStudent)%Subject(l) = crse
                isDirtyWaiverCOI = .true.
                n_changes = n_changes + 1
            end if
        end if

    end subroutine collect_advice


    subroutine copy_to_unit(logFile, device)
        integer, intent(in) :: device
        character (len=MAX_LEN_FILE_PATH), intent(in) :: logFile
        integer :: iTmp

        call html_comment('copy_to_unit('//trim(logFile)//')')

        write(device,AFORMAT) '<pre>'
        open(unit=unitETC, file=logFile, status='old')
        do
            read(unitETC, AFORMAT, iostat=iTmp) wrkCipher
            if (iTmp<0) exit
            write(device,AFORMAT) trim(wrkCipher)
        end do
        close(unitETC)
        write(device,AFORMAT) '</pre>'//horizontal

    end subroutine copy_to_unit


    subroutine html_student_list (device, sLen, sList, addLinks)

        integer, intent (in) :: device, sLen, sList(1:sLen)
        logical, intent (in), optional :: addLinks
        integer :: ldx, tdx, std, fac, nClasses(3), nAdvised(3), term, tYear, tTerm
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo

        call html_comment('html_student_list()')

        if (sLen == 0) then
            write(device,AFORMAT) BRNONE
        else
            if (present(addLinks)) then
                write(device,AFORMAT) linebreak//'<table border="0" width="100%">', &
                    begintr//thalignleft//'#'//endth//thalignleft//'STDNO'//endth, &
                    thalignleft//'NAME OF STUDENT'//endth//thalignleft//'PROGRAM'//endth, &
                    thalignleft//'LINKS'//endth//endtr
            else
                write(device,AFORMAT) linebreak//'<table border="0" width="70%">', &
                    begintr//thalignleft//'#'//endth//thalignleft//'STDNO'//endth, &
                    thalignleft//'NAME OF STUDENT'//endth//thalignleft//'PROGRAM'//endth//endtr
            end if

            do tdx=1,sLen
                std = sList(tdx)
                tStdNo = Student(std)%StdNo
                fac = index_to_teacher(Student(std)%Adviser)
                ldx = Student(std)%CurriculumIdx

                call count_preenlistment(std, 0, nClasses, nAdvised)

                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(tdx,2))//'">'// &
                    begintd//trim(itoa(tdx))//DOT//endtd//begintd//tStdNo//endtd, &
                    begintd//trim(Student(std)%Name)//endtd, &
                    begintd//trim(Curriculum(ldx)%Code)//endtd

                if ( is_adviser_of_student(std,orHigherUp) .and. present(addLinks)  ) then

                    write(device,AFORMAT) &
                        begintd//beginsmall, &
                        trim(make_href(fnStudentEdit, 'Info', A1=tStdNo, pre=nbsp)), &
                        trim(make_href(fnRecentStudentActivity, 'Log', A1=tStdNo, pre=nbsp)), &
                        trim(make_href(fnTeacherClasses, 'Adviser', A1=Teacher(fac)%TeacherId)), &
                        trim(make_href(fnStudentGrades, 'Grades', A1=tStdNo, pre=nbsp)), &
                        trim(make_href(fnEditCheckList, 'Checklist', A1=tStdNo, pre=nbsp))

                    do tTerm=termBegin,termEnd
                        call qualify_term (tTerm, tYear, term)
                        if (nClasses(term)+nAdvised(term)>0) write(device,AFORMAT) &
                            trim(make_href(fnStudentClasses, txtSemester(term+6), A1=tStdNo, A9=term, pre=nbsp))
                    end do
                    write(device,AFORMAT) endsmall//endtd
                end if
                write(device,AFORMAT) endtr
            end do
            write(device,AFORMAT) endtable
        end if

    end subroutine html_student_list


    subroutine collect_students_in_section (section_index, NumSections, Section, eList, n_count, sList)

        integer, intent (in) :: section_index, NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in) :: eList(0:)

        integer, intent (out) :: n_count, sList(1:)

        integer :: ldx, tdx, std, ncol, crse, sect
        character(len=MAX_LEN_CLASS_ID) :: tClassId

        call html_comment('collect_students_in_section()')

        ! what subject ?
        crse = Section(section_index)%SubjectIdx

        ! collect students in section
        n_count = 0
        do tdx=1,NumStudents+NumAdditionalStudents
            std = StdRank(tdx)
            do ncol=1,eList(std)%lenSubject
                sect = eList(std)%Section(ncol)
                if (sect==0) cycle
                if (section_index == sect) then
                    n_count = n_count+1
                    sList(n_count) = std
                    exit
                elseif (eList(std)%Subject(ncol)==crse .and. is_lecture_lab_subject(crse)) then
                    ldx = index(Section(sect)%ClassId,DASH)
                    if (ldx>0) then ! student is accommodated in a lab section
                        if (trim(tClassId)==Section(sect)%ClassId(:ldx-1)) then ! lab of lecture
                            n_count = n_count+1
                            sList(n_count) = std
                            exit
                        end if
                    end if
                end if
            end do
        end do

    end subroutine collect_students_in_section


    subroutine checklist_links(device, first)
        integer, intent (in) :: device
        logical, intent (in), optional :: first

        if (present(first)) then
            write(device,AFORMAT) linebreak//beginsmall, &
                '[ <b>Checklist:</b> <a href="#FEASIBLE SUBJECTS">Feasible</a> Subjects, ', &
                '<a href="#ADVISED SUBJECTS">Advised</a> Subjects, ', &
                'Approved <a href="#Approved SUBSTITUTIONS">Substitutions</a> ] ', &
                endsmall
        else
            write(device,AFORMAT) horizontal, &
                '<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
                begintr//tdnbspendtd//tdalignright//beginsmall, &
                '[ <a href="#FEASIBLE SUBJECTS">Feasible</a> Subjects ] ', &
                '[ <a href="#ADVISED SUBJECTS">Advised</a> Subjects ] ', &
                '[ Approved <a href="#Approved SUBSTITUTIONS">Substitutions</a> ] ', &
                '[ Change <a href="#TOP">Curriculum</a> ] ', &
                '[ <a href="#TOP">Top</a> ] ', &
                endsmall//endtd//endtr//endtable//horizontal
        end if

    end subroutine checklist_links


end module HTML
