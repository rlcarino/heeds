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

    use IO
!    use PREDICTION

    implicit none

    ! index to server functions
    integer, parameter ::  &
        fnLogin                   =  1, & ! login user
        fnGenerateTeacherPassword =  2, & ! generate new password for teacher
        fnChangeTeacherPassword   =  3, & ! change password for teacher
        fnGenerateStudentPassword =  4, & ! generate new password for student
        fnChangeStudentPassword   =  5, & ! change password for student
        fnShowStudentPassword     =  6, & ! show password for student
        fnLogout                  =  7, & ! logout user
        fnSuspendProgram          =  8, & ! suspend the program
        fnToggleTrainingMode      =  9, & ! toggle training mode
        fnStop                    = 10, & ! terminate program
        fnEditSignatories         = 11, & ! edit signatories
        fnRecentStudentActivity   = 12, & ! recent student activity
        fnRecentTeacherActivity   = 13, & ! recent teacher activity
        fnOnlineStudents          = 14, & ! online students
        fnOnlineTeachers          = 15, & ! online teachers
        !
        fnCollegeLinks            = 20, & ! index to college info
        fnSubjectList             = 21, & ! view list of subjects administered by a department
        fnEditSubject             = 22, & ! edit subject
        fnCurriculumList          = 23, & ! view list of curricular programs administered by a college
        fnCurriculum              = 24, & ! view a curricular program
        fnEditCurriculum          = 25, & ! edit curriculum
        fnActivateCurriculum      = 26, & ! activate curriculum
        fnDeactivateCurriculum    = 27, & ! deactivate curriculum
        fnEditRoom                = 28, & ! edit room parameters
        fnEditTeacher             = 29, & ! edit teacher record
        !
        fnStudentsByProgram       = 30, & ! view list students in a program
        fnStudentsByCurriculum    = 31, & ! view list students in a curriculum
        fnStudentsByName          = 32, & ! view alphabetical list of students in a college
        fnStudentsByYear          = 33, & ! view list of students in college by year
        fnStudentsDistribution    = 34, & ! view distribution of students, by curriculum and by college
        fnStudentAdd              = 35, & ! add a student
        fnStudentAddPrompt        = 36, & ! entry form for 'add a student'
        fnStudentPerformance      = 37, & ! view student performance
        fnEditCheckList           = 38, & ! display checklist for editing
        !
        fnScheduleOfClasses       = 40, & ! display schedule of classes for editing
        fnScheduleOfferSubject    = 41, & ! offer a subject
        fnScheduleAddLab          = 42, & ! add a lab section
        fnScheduleDelete          = 43, & ! delete a section
        fnScheduleEdit            = 44, & ! edit a section
        fnScheduleValidate        = 45, & ! check correctness of edit inputs
        fnTeachersByDept          = 46, & ! view list teachers belonging to a college
        fnTeachersByName          = 47, & ! view alphabetical list of teachers in a college
        fnTeacherClasses          = 48, & ! view classes of a teacher
        fnTeacherEditSchedule     = 49, & ! edit weekly schedule of a teacher
        fnRoomList                = 50, & ! view list rooms administered by a college
        fnRoomSchedule            = 51, & ! view weekly schedule of a room
        fnRoomConflicts           = 52, & ! view list of rooms with conflicts
        fnTeacherConflicts        = 53, & ! view list of teachers with conflicts
        fnTBARooms                = 54, & ! view list of sections with TBA rooms
        fnTBATeachers             = 55, & ! view list of sections with TBA teachers
        fnPrintableWorkload       = 56, & ! printable teaching load
        fnScheduleByArea          = 57, & ! display schedule of classes for editing, by area
        !
        fnBlockSchedule           = 60, & ! display schedule of block section
        fnBlockEditName           = 61, & ! edit name of block section
        fnBlockDeleteNotClasses         = 62, & ! delete block, but keep its sections
        fnBlockDeleteIncludingClasses          = 63, & ! delete block and its sections
        fnBlockNewSelect          = 64, & ! select parameters for a new block
        fnBlockNewAdd             = 65, & ! add a new block
        fnBlockCopy               = 66, & ! copy block
        fnBlockEditSection        = 67, & ! edit section in block
        fnBlockEditSubject        = 68, & ! update subjects in block
        fnBlockList               = 69, & ! list blocks
        fnBlockConflicts          = 70, & ! conflicts in block schedules
        !
        fnDemandFreshmen          = 80, & ! view demand for subjects by incoming students
        fnUpdateDemandFreshmen    = 81, & ! update no. of incoming students
        fnPrintableSchedule       = 82, & ! printable weekly timetable
        fnDemandForSubjects       = 83, & ! view demand for subjects
        fnPotentialStudents       = 84, & ! list of potential students of a subject
        !
        fnChangeMatriculation     = 85, & ! change matriculation
        fnFindBlock               = 86, & ! find a block for student
        fnSelectSubjects          = 87, & ! manually select subjects for student
        !
        fnEnlistmentSummary       = 90, & ! summary of enlistment by subject
        fnNotAccommodated         = 91, & ! students not accommodated in a priority subject
        fnBottleneck              = 92, & ! "bottleneck" subjects
        fnExtraSlots              = 93, & ! subjects with excess slots
        fnUnderloadSummary        = 94, & ! summary of underloading
        fnUnderloadedStudents     = 95, & ! underloaded students
        fnClassList               = 96, & ! view list of students in a class
        fnGradeSheet              = 97, & ! enter grades
        fnDownloadXML             = 98 ! download XML data file

    ! the requested server function, index to requesting, user
    integer :: REQUEST, requestingTeacher!, requestingStudent

    ! the target of the function
    integer :: targetCollege, targetDepartment, targetSubject, targetRoom, targetTeacher, &
        targetCurriculum, targetBlock, targetSection!, targetStudent

    ! work arrays
!    integer :: tArray(max(MAX_ALL_STUDENTS,2*MAX_ALL_SUBJECTS))
    integer :: tArray(2*MAX_ALL_SUBJECTS)
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

            case (fnGenerateTeacherPassword   )
                    fnDescription = 'generate new password for teacher'

            case (fnGenerateStudentPassword   )
                    fnDescription = 'generate new password for student'

            case (fnChangeTeacherPassword          )
                    fnDescription = 'change password for Teacher'

            case (fnChangeStudentPassword          )
                    fnDescription = 'change password for student'

            case (fnShowStudentPassword          )
                    fnDescription = 'show password for student'

            case (fnToggleTrainingMode      )
                    fnDescription = 'toggle training mode'

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

            case (fnTeacherClasses         )
                    fnDescription = 'view classes of a teacher'

            case (fnTeacherEditSchedule         )
                    fnDescription = 'edit weekly schedule of a teacher'

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

            case (fnBlockDeleteNotClasses         )
                    fnDescription = 'delete block, but keep its sections'

            case (fnBlockDeleteIncludingClasses          )
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

            case (fnGenerateTeacherPassword   )
                    fnAvailable = .true.

            case (fnGenerateStudentPassword   )
                    fnAvailable = .true.

            case (fnChangeTeacherPassword          )
                    fnAvailable = .true.

            case (fnChangeStudentPassword          )
                    fnAvailable = .true.

            case (fnShowStudentPassword          )
                    fnAvailable = .true.

            case (fnToggleTrainingMode      )
                    fnAvailable = .true.

            case (fnEditSignatories)
                    fnAvailable = .true.

            case (fnRecentStudentActivity)
                    fnAvailable = .true.

            case (fnRecentTeacherActivity)
                    fnAvailable = .true.

            case (fnOnlineStudents)
                    fnAvailable = .true.

            case (fnOnlineTeachers)
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
                    fnAvailable = .true.

            case (fnChangeMatriculation     )
                    fnAvailable = isActionClasslists

            case (fnFindBlock               )
                    fnAvailable = isActionClasslists

            case (fnSelectSubjects)
                    fnAvailable = isActionClasslists

            case (fnScheduleOfClasses       )
                    fnAvailable = .true.

            case (fnScheduleOfferSubject    )
                    fnAvailable = .true.

            case (fnScheduleAddLab          )
                    fnAvailable = .true.

            case (fnScheduleDelete          )
                    fnAvailable = .true.

            case (fnScheduleEdit            )
                    fnAvailable = .true.

            case (fnScheduleValidate        )
                    fnAvailable = .true.

            case (fnTeachersByDept          )
                    fnAvailable = .true.

            case (fnTeachersByName          )
                    fnAvailable = .true.

            case (fnTeacherClasses         )
                    fnAvailable = .true.

            case (fnTeacherEditSchedule         )
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
                    fnAvailable = .true.

            case (fnBlockDeleteNotClasses         )
                    fnAvailable = .true.

            case (fnBlockDeleteIncludingClasses          )
                    fnAvailable = .true.

            case (fnBlockNewSelect          )
                    fnAvailable = .true.

            case (fnBlockNewAdd             )
                    fnAvailable = .true.

            case (fnBlockCopy               )
                    fnAvailable = .true.

            case (fnBlockList               )
                    fnAvailable = .true.

            case (fnBlockConflicts)
                    fnAvailable = .true.

            case (fnBlockEditSection        )
                    fnAvailable = .true.

            case (fnBlockEditSubject        )
                    fnAvailable = .true.

            case (fnScheduleByArea          )
                    fnAvailable = .true.

            case (fnPrintableWorkload       )
                    fnAvailable = .true.

            case (fnEnlistmentSummary       )
                    fnAvailable = isActionClasslists

            case (fnNotAccommodated         )
                    fnAvailable = isActionClasslists

            case (fnBottleneck              )
                    fnAvailable = isActionClasslists

            case (fnExtraSlots              )
                    fnAvailable = isActionClasslists

            case (fnUnderloadSummary        )
                    fnAvailable = isActionClasslists

            case (fnUnderloadedStudents     )
                    fnAvailable = isActionClasslists

            case (fnClassList               )
                    fnAvailable = isActionClasslists

            case (fnGradeSheet              )
                    fnAvailable = isActionClasslists

            case (fnDemandFreshmen          )
                    fnAvailable = .true.

            case (fnUpdateDemandFreshmen    )
                    fnAvailable = .true.

            case (fnPrintableSchedule       )
                    fnAvailable = isActionClasslists

            case (fnDemandForSubjects       )
                    fnAvailable = .true.

            case (fnPotentialStudents       )
                    fnAvailable = .true.

            case (fnSuspendProgram             )
                    fnAvailable = .true.

            case (fnDownloadXML             )
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
    ! write to device the start an HTML form, like:
    !   <form name="input" method="post" action="/heeds">
    !   <input type="hidden" name="F" value="91">
    !   <input type="hidden" name="N" value="REGISTRAR">
    !   <input type="hidden" name="A1" value="CLINLAB">

        integer, intent (in) :: device, fn
        character(len=*), intent (in), optional :: A1, A2, A3, A4, A5

        character(len=MAX_CGI_WRK_LEN) :: cgi_wrk
        integer :: kStart, checkSum
        real :: harvest ! random number

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
            'This program comes with ABSOLUTELY NO WARRANTY; for details see the ', &
            'GNU GENERAL PUBLIC LICENSE Version 3 ', &
            '(<a target="0" href="http://www.gnu.org/licenses/gpl-3.0.html">GPLv3</a>).<br>', &
            'This program is free software, and you are welcome to redistribute it under certain conditions; ', &
            ' see the GPLv3 for details.<br>', &
            'The source code is available at <a target="0" href="'//WEB//'">'// &
            WEB//'</a><br>', &
            CONTACT//'</i></small><hr>'


    end subroutine html_copyright


    subroutine html_landing_page(device, mesg)
        integer, intent(in) :: device
        character(len=*), intent(in) :: mesg
        integer :: j, k
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        character (len=MAX_LEN_PASSWD_VAR) :: tPassword

        tTeacher = GUEST
        j = index_to_teacher(tTeacher)
        call get_teacher_password(j, tPassword)

        write(device,AFORMAT) &
            '<html><head><title>'//trim(UniversityCode)//SPACE//PROGNAME//'</title></head><body>'

        if (len_trim(mesg)>0) then ! Stop/Logout page
            write(device,aformat) '<h2>'//trim(UniversityCode)//SPACE//PROGNAME// &
                ' for '//text_school_year(currentYear)//'</h2>', &
                red//trim(mesg)//black
            if (REQUEST==fnStop)  &
                write(device,AFORMAT)'<br><br>Go to <a href="/">'//PROGNAME//' Index</a>'
        else ! Login page
            write(device,AFORMAT) &
                '<h2>Welcome to '//trim(UniversityCode)//SPACE//trim(ACTION)// &
                ' for '//text_school_year(currentYear)//'</h2><hr>'
            write(device,AFORMAT) '<table border="0" width="100%">'//begintr, &
                '<td valign="top" width="25%">', &
                '<form method="post" action="'//trim(CGI_PATH)//'">', &
                '<input type="hidden" name="F" value="'//trim(itoa(fnLogin))//'">', &
                '<b>Username</b> (case sensitive):<br>', &
                '<input size="20" type="text" name="N" value="'//trim(tTeacher)//'">', &
                '<br><br>', &
                '<b>Password:</b><br>', &
                '<input size="20" type="password" name="P" value="'//trim(tPassword)//'">', &
                '<br><br>', &
                '<input type="submit" value="Login">', &
                nbsp//'<i><small>(or, go back to <a href="/">'//PROGNAME//' Index</a>)</small></i></form>'
            if (len_trim(loginCheckMessage)>0) write(device,AFORMAT) &
                '<br>'//red//trim(loginCheckMessage)//black//'<br>'
            write(device,AFORMAT) endtd//begintd, &
                '<b>Role - <i>privilege</i> ( Username )</b><ul>', &
                '<li>Anybody - <i>view all data except student records</i> ( Guest )</li>', &
                '<li>Student - <i>view all data and own records</i> ( StdNo )</li>'
            write(device,AFORMAT) &
                '<li>Curriculum advisers - <i>view all data, modify records of students in curriculum</i> ( TeacherID '
            done = .false.
            do k=1,NumCurricula-1
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
            write(device,AFORMAT) '</ul>'//endtd//endtr//'</table>', &
                red//'<b>Warning:</b> Do not use the browser "Back" button unless '// &
                ' the webpage on display does not have hyperlinks or input controls.'//black//'<hr>'
            call html_copyright(device)

        end if

    end subroutine html_landing_page


    subroutine html_login(fname, mesg)
        character(len=*), intent(in) :: fname, mesg
        integer :: device=7

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
                if (target_fn==fnRoomSchedule .or. target_fn==fnTeacherEditSchedule) then
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


    end subroutine list_sections_to_edit


    subroutine html_write_header(device, header, errmsg)
        integer, intent (in) :: device
        character(len=*), intent (in) :: header
        character(len=*), intent (in), optional :: errmsg
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
!        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        integer :: gdx!, fdx, nEnlisted, nAdvised
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
            begintr//begintd//'<h2>'//trim(UniversityCode)//SPACE//trim(ACTION)// &
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

        ! primary links for a teacher
        else if (isRoleTeacher) then
            QUERY_put = USERNAME
            if (REQUEST/=fnTeacherClasses .and. targetTerm/=0) &
                write(device,AFORMAT) &
                    trim(make_href(fnTeacherClasses, 'Classes', A1=QUERY_put, pre=nbsp, alt=SPACE))
            if (REQUEST/=fnPrintableWorkload .and. targetTerm/=0) &
                write(device,AFORMAT) &
                    trim(make_href(fnPrintableWorkload, 'Load form', A1=QUERY_put, pre=nbsp, alt=SPACE))

        ! primary links for a student
        else if (isRoleStudent) then
            QUERY_put = USERNAME
            if (REQUEST/=fnEditCheckList) then
                write(device,AFORMAT) trim(make_href(fnEditCheckList, 'Checklist', &
                    A1=QUERY_put, pre=nbsp, alt=SPACE))
            end if
            if (REQUEST/=fnStudentPerformance) then
                write(device,AFORMAT) trim(make_href(fnStudentPerformance, 'Performance', &
                    A1=QUERY_put, pre=nbsp, alt=SPACE))
            end if
        end if

        ! Logout for all users
        write(device,AFORMAT) trim(make_href(fnLogout, 'Logout', pre=nbsp))

        ! end of line 1
        write(device,AFORMAT) '</small>'//endtd//endtr

        ! line 2 for banner: colleges
        write(device,AFORMAT) begintr//'<td colspan="2"><small>[ <b>Colleges:</b>'
        do gdx = 1,NumColleges
            if (.not. College(gdx)%hasInfo) cycle
            ! fnCollegeLinks requested already printed above?
            write(device,AFORMAT) trim(make_href(fnCollegeLinks, College(gdx)%Code, &
                A1=College(gdx)%Code, pre=nbsp))
        end do
        write(device,AFORMAT) ' ]</small>'//endtd//endtr

        ! line 3 for banner: higher level context for selected function
        write(device,AFORMAT) begintr//'<td colspan="2"><small>'

        ! a teacher ?
        if (targetTeacher>0) then
            write(device,AFORMAT) '[ <b>'//trim(Teacher(targetTeacher)%Name)//'</b> '

            if (isRoleAdmin .or. (isRoleChair .and. DeptIdxUser==Teacher(targetTeacher)%DeptIdx) ) then
                if (REQUEST/=fnEditTeacher) then
                    write(device,AFORMAT) trim(make_href(fnEditTeacher, 'Edit info', &
                        A1=Teacher(targetTeacher)%TeacherID, pre=nbsp, alt=SPACE))
                end if
                if (REQUEST/=fnTeacherEditSchedule) then
                    write(device,AFORMAT) trim(make_href(fnTeacherEditSchedule, 'Edit load', &
                        A1=Teacher(targetTeacher)%TeacherID, pre=nbsp))
                end if
            end if
            if (REQUEST/=fnTeacherClasses) then
                write(device,AFORMAT) trim(make_href(fnTeacherClasses, 'Classes', &
                    A1=Teacher(targetTeacher)%TeacherID, pre=nbsp))
            end if
            if (REQUEST/=fnPrintableWorkload) then
                write(device,AFORMAT) trim(make_href(fnPrintableWorkload, 'Load form', &
                    A1=Teacher(targetTeacher)%TeacherID, pre=nbsp))
            end if
            if (isRoleAdmin .and. REQUEST/=fnRecentTeacherActivity) then
                write(device,AFORMAT) trim(make_href(fnRecentTeacherActivity, 'Activity', &
                    A1=Teacher(targetTeacher)%TeacherID, pre=nbsp, alt=SPACE))
            end if
            write(device,AFORMAT) ' ] '//nbsp
        end if

        ! show department options ?
        if (targetDepartment>0 .and. DeptIdxUser/=targetDepartment .and. REQUEST/=fnLogin) then
            tDepartment = Department(targetDepartment)%Code
            write(device,AFORMAT) '[ <b>'//trim(tDepartment)//'</b>'
            call info_department(device, tDepartment)
            write(device,AFORMAT) ' ] '//nbsp
        end if

        write(device,AFORMAT) '</small>'//endtd//endtr

        ! line 4 of banner, if any
        if (present(errmsg)) then
            if (errmsg/=SPACE) write(device,AFORMAT) begintr//'<td colspan="2">'// &
                red//'<i>'//trim(errmsg)//'</i>'//black//endtd//endtr
        end if
        write(device,AFORMAT) '</table><hr>'

        ! start of body
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
                write(device,AFORMAT) 'User is '//trim(USERNAME)//'@'//REMOTE_ADDR//nbsp
                if (USERNAME/=GUEST) then
                    if (isRoleStudent) then
                        write(device,AFORMAT) &
                            trim(make_href(fnChangeStudentPassword, 'Change password'))
                    else
                        write(device,AFORMAT) &
                            trim(make_href(fnChangeTeacherPassword, 'Change password'))
                    end if
                end if
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


    end subroutine html_college_links


    subroutine info_department(device, tDepartment)
        integer, intent(in) :: device
        character(len=MAX_LEN_DEPARTMENT_CODE), intent (in) :: tDepartment


        call html_comment('info_department()')

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

        if (REQUEST/=fnScheduleOfClasses .and. targetTerm>0) then
            write(device,AFORMAT) trim(make_href(fnScheduleOfClasses, 'Classes', &
                A1=tDepartment, pre=nbsp))
        end if


    end subroutine info_department


    subroutine html_college_info(device, coll)
        integer, intent(in) :: device
        integer, intent(in) :: coll
        integer :: tdx, rdx, ldx, cdx, dept, n_curr, n_count, tLen!, std
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
!        character (len=4) :: tYear
        character (len=1) :: ch
        character (len=80) :: description
        logical :: addHR

        call html_comment('html_college_info()')

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

        if (isRoleAdmin .and. coll==CollegeIdxUSER) then

            write(device,AFORMAT) trim(make_href(fnStop, 'Stop', &
                pre='<li><b>', post='</b> '//PROGNAME//red//' - Are you sure you want to do this?'//black//'</li>'))

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

            write(device,AFORMAT) '<li><b>Online</b> ', &
                trim(make_href(fnOnlineTeachers, 'teachers', pre=nbsp)), &
                '</li>'

            write(device,AFORMAT) trim(make_href(fnEditSignatories, 'signatories', &
                pre='<li><b>Edit '//nbsp, post=nbsp//'in teaching load form</b></li>'))

            write(device,AFORMAT) '<li><b>Download academic catalog.</b> '//&
                'Right-click on link, then "Save Link As..." '//trim(pathToYear), &
                trim(make_href(fnDownloadXML, 'CATALOG.XML', A1='CATALOG.XML', post='</li>'))

            write(device,AFORMAT) '<li><b>Download passwords of teachers</b> '//&
                'Right-click on link, then "Save Link As..." ', &
                trim(make_href(fnDownloadXML, 'PASSWORDS-TEACHERS.CSV', A1='PASSWORDS-TEACHERS.CSV', post='</li>'))

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
                do ldx=cdx+1,NumCurricula-1
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

            call html_comment('teacher links()')

            write(device,AFORMAT) '<li><b>Teachers by department</b> : '
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

        if (addHR) write(device,AFORMAT) '<hr>'

        ! per term
        cdx = targetTerm ! remember targetTerm
        do ldx=termBegin,termEnd

            call qualify_term (ldx, rdx, targetTerm, description)
            write(device,AFORMAT) '<li><b>'//trim(description)//'</b><ul>'
            if (coll==NumColleges .and. .not. (isRoleGuest .or. isRoleStudent)) then
                write(device,AFORMAT) &
                    '<li><b>Download Schedule of Classes.</b> Right-click on link, then "Save Link As..." '// &
                    trim(dirDATA)//trim(itoa(rdx))//DIRSEP//trim(txtSemester(targetTerm))//DIRSEP, &
                    trim(make_href(fnDownloadXML, 'CLASSES.XML', A1='CLASSES.XML', post='</li>'))
                write(device,AFORMAT) &
                    '<li><b>Download blocked sections.</b> Right-click on link, then "Save Link As..." '// &
                    trim(dirDATA)//trim(itoa(rdx))//DIRSEP//trim(txtSemester(targetTerm))//DIRSEP, &
                    trim(make_href(fnDownloadXML, 'BLOCKS.XML', A1='BLOCKS.XML', post='</li>'))

            end if

            ! blocks
#if defined UPLB
#else
            if (n_curr>0) call links_to_blocks(device, coll, targetTerm)
#endif

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


    end subroutine links_to_depts


    subroutine links_to_subjects(device, coll, numAreas, AreaList)
        integer, intent (in) :: device, coll, numAreas
        integer, intent (in) :: AreaList(1:numAreas)
        integer :: dept, n_count!, crse
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        !character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        if (numAreas==0) return

        call html_comment('links_to_subjects()')

#if defined UPLB
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
        write(device,AFORMAT) '</li><li><b>Subjects</b> :'
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
        integer :: ddx, dept, sect, n_count, m_count, mdx, k1, k2!, crse
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        if (numAreas==0) return

        call html_comment('links_to_sections()')

        tCollege = College(coll)%Code
#if defined UPLB
        ddx = 0
#else
        tDepartment = tCollege
        ddx = index_to_dept(tDepartment)
#endif

!        write(device,AFORMAT) '<li><b>Classes</b> in : '
!        do dept=2,NumDepartments
!            if (Department(dept)%CollegeIdx /= coll) cycle
!            n_count = 0
!#if defined UPLB
!            do crse=1,NumSubjects+NumAdditionalSubjects
!                if (Subject(crse)%DeptIdx /= dept) cycle
!                n_count = n_count+1
!                exit
!            end do
!#else
!            ! Subjects administered by program
!            do crse=1,NumSubjects+NumAdditionalSubjects
!                if (.not. is_used_in_college_subject(coll, crse)) cycle
!                n_count = n_count+1
!                exit
!            end do
!#endif
!            if (n_count==0) cycle ! no subjects in this department
!
!            ! how many sections currently open
!            n_count = 0
!            do sect=1,NumSections(term)
!                if (dept/=Section(term,sect)%DeptIdx) cycle ! not in department
!                if (Section(term,sect)%SubjectIdx==0) cycle ! deleted
!                n_count = n_count+1
!            end do
!            tDepartment = Department(dept)%Code
!            write(device,AFORMAT) trim(make_href(fnScheduleOfClasses, tDepartment, &
!                A1=tDepartment, post='('//trim(itoa(n_count))//')'//nbsp))
!        end do
!
!        write(device,AFORMAT) '</li> '

        write(device,AFORMAT) '<li><b>Classes</b> : '
        do dept=1,numAreas
            ! the code
            tSubject = SubjectArea(AreaList(dept))%Code
            k1 = len_trim(tSubject)+1
            n_count = 0 ! how many sections with this code currently open
            do sect=1,NumSections(term)
#if defined UPLB
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
        write(device,AFORMAT) trim(make_href(fnTeacherConflicts, 'teachers', &
            A1=tCollege, pre=nbsp))
        write(device,AFORMAT) trim(make_href(fnRoomConflicts, 'rooms', &
            A1=tCollege, pre=nbsp))
        write(device,AFORMAT) trim(make_href(fnBlockConflicts, 'blocks', &
            A1=tCollege, pre=nbsp))
        write(device,AFORMAT) '</li>'

        if (isRoleTeacher) then
            QUERY_put = USERNAME
            write(device,AFORMAT) &
                trim(make_href(fnTeacherClasses, 'classes', &
                    A1=QUERY_put, pre='<li><b>My </b>'//nbsp)), &
                trim(make_href(fnPrintableWorkload, 'form', &
                    A1=QUERY_put, pre=nbsp//'and teaching load'//nbsp, post='</li>'))
        end if

    end subroutine links_to_sections


    subroutine links_to_blocks(device, coll, term)
        integer, intent (in) :: device, coll, term
        integer :: cdx, ldx, blk, ncurr
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege

        call html_comment('links_to_blocks()')

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
            do ldx=cdx+1,NumCurricula-1
                if (CurrProgCode(ldx) == CurrProgCode(cdx)) done(ldx) = .true.
            end do
        end do
!        if (ncurr>0 .and. isRoleChair .or. isRoleAdmin) then
!            write(device,AFORMAT) trim(make_href(fnBlockNewSelect, 'Add', &
!                A1=tCollege, pre='<b> (', post=' block)</b>'))
!        end if
        write(device,AFORMAT) '</li>'


    end subroutine links_to_blocks


    subroutine blocks_in_section(device, sect, fn, NumBlocks, Block)
        integer, intent(in) :: device, sect, fn, NumBlocks
        type (TYPE_BLOCK), intent(in) :: Block(0:)
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

    end subroutine blocks_in_section



    subroutine recent_teacher_activity(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher, tRole
        integer :: ldx, iTmp, nDays, sYear, sMonth, sDay, eYear, eMonth, eDay
        character (len=MAX_LEN_FILE_PATH) :: logFile
        logical :: logExists

        ! which teacher ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, iTmp)
        targetTeacher = index_to_teacher(tTeacher)
        iTmp = Teacher(targetTeacher)%DeptIdx
        ldx = Department(iTmp)%CollegeIdx
        tRole = Teacher(targetTeacher)%Role

        call html_write_header(device, 'Recent activity of '//trim(Teacher(targetTeacher)%Name), SPACE)

        if (trim(tRole)==GUEST) then
            logFile = trim(dirLOG)// &
                trim(College(ldx)%Code)//DIRSEP// &
                trim(tTeacher)//'.log'
            inquire(file=logFile, exist=logExists)
            if (logExists) then
                call copy_to_unit(logFile, device)
            else
                write(device,AFORMAT) '<br>(None)<hr>'
            end if
        else
            nDays = 0 ! no. of days active
            sYear = atoi(startDateTime(1:4))
            sMonth = atoi(startDateTime(5:6))
            sDay = atoi(startDateTime(7:8))
            eYear = atoi(currentDate(1:4))
            eMonth = atoi(currentDate(5:6))
            eDay = atoi(currentDate(7:8))

            do
                logFile = trim(dirLOG)// &
                    trim(College(ldx)%Code)//DIRSEP// &
                    trim(tTeacher)//DASH//currentDate//'.log'
                inquire(file=logFile, exist=logExists)
                if (logExists) then
                    nDays = nDays + 1
                    write(device,AFORMAT) '<br><b>'// &
                        trim(itoa(sYear))//DASH//trim(itoa2bz(sMonth))//DASH//trim(itoa2bz(sDay))//'</b>'
                    call copy_to_unit(logFile, device)
                end if
                if (sYear==eYear .and. sMonth==eMonth .and. sDay==eDay) exit
                sDay = sDay + 1
                if (sDay>31) then
                    sDay = 1
                    sMonth = sMonth + 1
                end if
                if (sMonth>12) then
                    sMonth = 1
                    sYear = sYear + 1
                end if
            end do
            if (nDays==0) then
                logFile = trim(dirLOG)// &
                    trim(College(ldx)%Code)//DIRSEP// &
                    trim(tTeacher)//'.log'
                inquire(file=logFile, exist=logExists)
                if (logExists) then
                    call copy_to_unit(logFile, device)
                else
                    write(device,AFORMAT) '<br>(None)<hr>'
                end if
            end if
        end if


    end subroutine recent_teacher_activity


    subroutine copy_to_unit(logFile, device)
        integer, intent(in) :: device
        character (len=MAX_LEN_FILE_PATH), intent(in) :: logFile
        integer :: iTmp

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


end module HTML
