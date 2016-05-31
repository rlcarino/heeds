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
        !
        fnReload                  =  7, & ! reload data
        fnLoginByNonEditors       =  8, & ! toggle login permission for non-editor roles
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
        fnTimers                  = 19, & ! edit timers
        !
        fnCollegeLinks            = 20, & ! index to unit info
        fnSubjectList             = 21, & ! view list of subjects administered by a unit
        fnEditSubject             = 22, & ! edit subject
        fnCurriculumList          = 23, & ! view list of curricular programs administered by a unit
        fnCurriculum              = 24, & ! view a curricular program
        fnEditCurriculum          = 25, & ! edit curriculum
        fnActivateCurriculum      = 26, & ! activate curriculum
        fnDeactivateCurriculum    = 27, & ! deactivate curriculum
        fnRoomList                = 28, & ! view list rooms administered by a unit
        fnTeachersByDept          = 29, & ! view list teachers belonging to a unit
        !fnTeachersByName          = 30, & ! view alphabetical list of teachers in a unit
        fnEditRoom                = 31, & ! edit room parameters
        fnEditTeacher             = 32, & ! edit teacher record
        fnEditEquivalencies       = 33, & ! edit equivalence rules
        fnEditMOTD                = 34, & ! edit message of the day
        !
        fnEditFees                = 37, & ! edit school fees
        fnEditScholarships        = 38, & ! edit scholarships
        fnListBeneficiaries       = 39, & ! list beneficiaries
        !
        fnAdvisersByTeacher       = 40, & ! change adviser, list by teacher
        fnAdvisersByCurriculum    = 41, & ! change adviser, list by curriculum
        fnStudentsByCurriculum    = 42, & ! view list students in a curriculum
        fnAdvisingEnlistmentStatus= 43, & ! distribution of students by advising and enlistment status
        fnStudentAdd              = 44, & ! add a student
        fnStudentEdit             = 45, & ! display student info for editing
        fnStudentGrades           = 46, & ! display grades of student
        fnEditCheckList           = 47, & ! display checklist for editing
        fnGradeCertification      = 48, & ! printable grade certification
        fnTranscript              = 49, & ! printable transcript
        !
        fnScheduleOfClasses       = 50, & ! display list of classes in unit
        fnScheduleOfferSubject    = 51, & ! offer a subject
        fnScheduleAddLab          = 52, & ! add a lab section
        fnScheduleDelete          = 53, & ! delete a section
        fnScheduleEdit            = 54, & ! edit a section
        fnScheduleValidate        = 55, & ! check correctness of edit inputs
        fnScheduleByArea          = 56, & ! display schedule of classes for editing, by area
        fnScheduleCopyLastYear    = 57, & ! copy schedule of classes last year, same term
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
        fnAdvisingStatusStudents  = 81, & ! list students by advising status
        fnAdvisingStatus          = 82, & ! distribution of students by advising status
        fnEnlistmentStatus        = 83, & ! distribution of students by enlistment status
        fnEnlistmentStatusStudents= 84, & ! list students by enlistment status
        fnResetDemandForSubjects  = 85, & ! reset demand for subjects
        fnClearTimetables         = 87, & ! clear timetable/initial schedules of students
        fnGenerateTimetables      = 88, & ! generate timetable/initial schedules of students
        fnStudentClasses          = 89, & ! view timetable/class schedule of student
        fnPrintableSchedule       = 90, & ! printable weekly timetable
        fnChangeMatriculation     = 91, & ! change matriculation
        fnFindBlock               = 92, & ! find a block for student
        !
        fnGenderDistribution      = 93, & ! gender distribution
        fnGenderStudents          = 94, & ! list of students by gender
        !
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
        fnAdvisingCode            =120, & ! student with advising specified code determined by needs analysis
        fnTimetabling             =121, & ! preenlist/delist students
        !
        fnSummaryWAG              =122, & ! summary list of student GWA
        !
        fnUploadPicture              =129, & ! upload file
        fnFileDownload            =130, & ! download XML data file
        fnUploadCSV         =131, & ! upload file containing new students
        !
        fnEvaluationDuties               = 183, & ! assign peer evaluators
        fnEvaluationRatings              = 184, & ! summary of evaluation ratings
        fnEvaluationForm                 = 185, & ! teaching effectiveness evaluation form
        !
        fnLoginPage                      = 186, & ! return login page
        fnHomePage                       = 187, & ! redirect to index.html
        fnUpdateAdviceBasis              = 188, & ! update units earned, year level, etc, but not advised subjects
        fnFees                           = 189, & ! display assessment, discount, fees paid by term
        fnDistributionPSGC               = 190, & ! student distribution by PSGC
        fnSelectTongue                   = 191, & ! select mother tongue
        fnSelectPSGC                     = 192, & ! select PSGC
        fnSwitchTerm                     = 193, & ! set term of school year
        fnSearchCategory                 = 194, & ! search a category containing user-specified string
        fnEditEMERGENCY                  = 195, & ! edit emergency message
        fnTogglePermission               = 196, & ! ADMIN toggles a permission
        fnSwitchUser                     = 197, & ! ADMIN logs in user without password
        fnConfirmTimetabling             = 198, & ! confirm preenlist students
        fnConfirmUpdateDemandForSubjects = 199, & ! confirm update demand for subjects
        fnConfirmResetDemandForSubjects  = 200    ! confirm reset  demand for subjects

    ! the target of the requested function
    integer :: targetCollege, targetDepartment, targetSubject, targetRoom, targetTeacher, &
        targetCurriculum, targetStudent, targetBlock, targetSection, targetTerm

    ! work arrays
    integer :: tArray(max(3*MAX_ALL_STUDENTS/2,2*MAX_ALL_SUBJECTS))
    character (len=80) :: QUERY_put


contains

    function fnDescription (fn)
        character(len=60) :: fnDescription
        integer, intent (in) :: fn

        select case (fn)

            case (fnStop )
                    fnDescription = 'stop program'

            case (fnLogin )
                    fnDescription = 'login'

            case (fnLogout)
                    fnDescription = 'logout'

            case (fnReload)
                    fnDescription = 'reload data'

            case (fnTimers)
                    fnDescription = 'edit timers'

            case (fnLoginByNonEditors)
                    fnDescription = 'toggle login permission for non-editor roles'

            case (fnGenerateTeacherPassword )
                    fnDescription = 'generate new password for teacher'

            case (fnGenerateStudentPassword )
                    fnDescription = 'generate new password for student'

            case (fnChangeTeacherPassword)
                    fnDescription = 'change password for Teacher'

            case (fnChangeStudentPassword)
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

            case (fnCollegeLinks)
                    fnDescription = 'index to unit info'

            case (fnSubjectList )
                    fnDescription = 'list subjects administered by a unit'

            case (fnEditSubject )
                    fnDescription = 'edit subject'

            case (fnCurriculumList)
                    fnDescription = 'list curricular programs administered by a unit'

            case (fnCurriculum)
                    fnDescription = 'view a curricular program'

            case (fnEditCurriculum)
                    fnDescription = 'edit curriculum'

            case (fnActivateCurriculum)
                    fnDescription = 'activate curriculum'

            case (fnDeactivateCurriculum)
                    fnDescription = 'deactivate curriculum'

            case (fnEditEquivalencies)
                    fnDescription = 'edit equivalence rules'

            case (fnEditFees)
                    fnDescription = 'edit school fees'

            case (fnEditScholarships)
                    fnDescription = 'edit scholarships'

            case (fnListBeneficiaries)
                    fnDescription = 'list beneficiaries'

            case (fnEditRoom)
                    fnDescription = 'edit room parameters'

            case (fnEditTeacher )
                    fnDescription = 'edit teacher record'

            case (fnEditMOTD)
                    fnDescription = 'edit message of the day'

            case (fnEditEMERGENCY)
                    fnDescription = 'edit emergency message'

            case (fnAdvisersByTeacher )
                    fnDescription = 'change adviser, list by teacher'

            case (fnAdvisersByCurriculum)
                    fnDescription = 'change adviser, list by curriculum'

            case (fnStudentsByCurriculum)
                    fnDescription = 'view list students in a curriculum'

            case (fnStudentAdd)
                    fnDescription = 'add a student'

            case (fnStudentEdit )
                    fnDescription = 'display student info for editing'

            case (fnStudentGrades)
                    fnDescription = 'display grades of student'

            case (fnGradeCertification)
                    fnDescription = 'printable grade certification'

            case (fnTranscript)
                    fnDescription = 'printable transcript'

            case (fnEditCheckList )
                    fnDescription = 'display checklist for editing'

            case (fnStudentClasses)
                    fnDescription = 'view timetable/class schedule of student'

            case (fnClearTimetables)
                    fnDescription = 'reset initial timetables of students'

            case (fnGenerateTimetables)
                    fnDescription = 'generate initial timetables of students'

            case (fnChangeMatriculation )
                    fnDescription = 'change matriculation'

            case (fnAdvisingEnlistmentStatus)
                    fnDescription = 'distribution of students by advising and enlistment status'

            case (fnAdvisingStatus)
                    fnDescription = 'distribution students by advising status'

            case (fnAdvisingStatusStudents)
                    fnDescription = 'advising status of students in a curriculum'

            case (fnEnlistmentStatus)
                    fnDescription = 'distribution students by enlistment status'

            case (fnEnlistmentStatusStudents)
                    fnDescription = 'enlistment status of students in a curriculum '

            case (fnFindBlock)
                    fnDescription = 'find a block for student'

            case (fnStudentForceEnlist)
                    fnDescription = 'force-enlist student into section'

            case (fnScheduleOfClasses)
                    fnDescription = 'display subjects in unit with open sections'

            case (fnScheduleOfferSubject)
                    fnDescription = 'offer a subject'

            case (fnScheduleAddLab)
                    fnDescription = 'add a lab section'

            case (fnScheduleDelete)
                    fnDescription = 'delete a section'

            case (fnScheduleEdit)
                    fnDescription = 'edit a section'

            case (fnScheduleValidate)
                    fnDescription = 'check correctness of edit inputs'

            case (fnScheduleCopyLastYear)
                    fnDescription = 'copy schedule of classes last year, same term'

            case (fnTeachersByDept)
                    fnDescription = 'view list teachers belonging to a unit'

            case (fnEvaluationForm)
                    fnDescription = 'enter/modify evaluation form for teaching effectiveness'

            case (fnEvaluationRatings)
                    fnDescription = 'summary of evaluation ratings'

            case (fnEvaluationDuties)
                    fnDescription = 'assign peer evaluation duties'

            case (fnTeacherClasses)
                    fnDescription = 'view classes of a teacher'

            case (fnTeacherEditSchedule)
                    fnDescription = 'edit weekly schedule of a teacher'

            case (fnRoomList)
                    fnDescription = 'view list rooms administered by a unit'

            case (fnRoomSchedule)
                    fnDescription = 'view weekly schedule of a room'

            case (fnRoomConflicts)
                    fnDescription = 'view list of rooms with conflicts'

            case (fnTeacherConflicts)
                    fnDescription = 'view list of teachers with conflicts'

            case (fnTBARooms)
                    fnDescription = 'view list of sections with TBA rooms'

            case (fnTBATeachers)
                    fnDescription = 'view list of sections with TBA teachers'

            case (fnBlockSchedule)
                    fnDescription = 'display schedule of block section'

            case (fnBlockEditName)
                    fnDescription = 'edit name of block section'

            case (fnBlockDeleteNotClasses)
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

            case (fnPrintableWorkload)
                    fnDescription = 'printable teaching load'

            case (fnEnlistmentSummary)
                    fnDescription = 'summary of enlistment by subject'

            case (fnNotAccommodated)
                    fnDescription = 'students not accommodated in a priority subject'

            case (fnBottleneck)
                    fnDescription = '"bottleneck" subjects'

            case (fnExtraSlots)
                    fnDescription = 'subjects with excess slots'

            case (fnUnderloadSummary)
                    fnDescription = 'summary of underloading'

            case (fnUnderloadedStudents)
                    fnDescription = 'underloaded students'

            case (fnClassList)
                    fnDescription = 'view list of students in a class'

            case (fnPrintableClassList)
                    fnDescription = 'printable classlist'

            case (fnGradesheet)
                    fnDescription = 'enter grades'

            case (fnPrintableGradesheet)
                    fnDescription = 'printable gradesheet'

            case (fnPrintableSchedule )
                    fnDescription = 'printable weekly timetable'

            case (fnResetDemandForSubjects)
                    fnDescription = 'reset demand for subjects'

            case (fnConfirmResetDemandForSubjects)
                    fnDescription = 'confirm reset demand for subjects'

            case (fnUpdateDemandForSubjects )
                    fnDescription = 'update demand for subjects'

            case (fnConfirmUpdateDemandForSubjects )
                    fnDescription = 'confirm update demand for subjects'

            case (fnUploadPicture)
                    fnDescription = 'upload ID picture'

            case (fnFileDownload )
                    fnDescription = 'download XML data file'

            case (fnStudentsWithConflicts)
                    fnDescription = 'list students with timetable conflicts'

            case (fnSwitchTerm)
                    fnDescription = 'set term of school year'

            case (fnUnpostedGrades)
                    fnDescription = 'classes with unposted grades'

            case (fnGradesheetsToReceive)
                    fnDescription = 'classes with no hardcopy of gradesheet'

            case (fnAdvisingCode)
                    fnDescription = 'list students based on advising analysis code'

            case (fnStudentsNotEnrolled)
                    fnDescription = 'list students without classes'

            case (fnTimetabling)
                    fnDescription = '(preen|de)list based on advising|enlistment status'

            case (fnConfirmTimetabling)
                    fnDescription = 'confirm (preen|de)list based on advising|enlistment status'

            case (fnSummaryWAG)
                    fnDescription = 'summary list of student GWA'

            case (fnTogglePermission)
                    fnDescription = 'ADMIN toggles a permission'

            case (fnSwitchUser)
                    fnDescription = 'ADMIN logs in user without password'

            case (fnSearchCategory)
                    fnDescription = 'search a category containing user-specified string'

            case (fnSelectPSGC)
                    fnDescription = 'select PSGC'

            case (fnSelectTongue)
                    fnDescription = 'select mother tongue'

            case (fnDistributionPSGC)
                    fnDescription = 'student distribution by PSGC'

            case (fnFees)
                    fnDescription = 'display assessment, discount, fees paid by term'

            case (fnUpdateAdviceBasis)
                    fnDescription = 'update units earned, year level, etc, but not advised subjects'

            case (fnUploadCSV)
                    fnDescription = 'upload file containing new students'

            case (fnHomePage)
                    fnDescription = 'redirect to home page'

            case (fnLoginPage)
                    fnDescription = 'show login page'

            case (fnGenderDistribution)
                    fnDescription = 'gender distribution'

            case (fnGenderStudents)
                    fnDescription = 'list of students by gender'

            case default
                    fnDescription = 'Function description not available for FN = '//itoa(fn)

        end select

    end function fnDescription


    subroutine blocks_in_section(device, iSect, fn, thisTerm)
        integer, intent(in) :: device, iSect, fn, thisTerm
        integer :: idx, jdx

        call html_comment('blocks_in_section()')

        do idx=1,NumBlocks(thisTerm)
            do jdx=1,Block(thisTerm,idx)%NumClasses
                if (Block(thisTerm,idx)%Section(jdx)/=iSect) cycle
                if (fn>0) then
                    write(device,AFORMAT) trim(make_href(fn, Block(thisTerm,idx)%BlockID, A1=Block(thisTerm,idx)%BlockID, &
                        A9=thisTerm))
                else
                    write(device,AFORMAT) trim(Block(thisTerm,idx)%BlockID)
                end if
                exit
            end do
        end do

    end subroutine blocks_in_section


    subroutine checklist_links(device, first)
        integer, intent (in) :: device
        logical, intent (in), optional :: first

        if (present(first)) then
            write(device,AFORMAT) linebreak//b_small, &
                '<b>Checklist:</b> <a href="#FEASIBLE SUBJECTS">FEASIBLE</a> subjects, ', &
                '<a href="#ADVISED SUBJECTS">ADVISED</a> subjects, ', &
                'Approved <a href="#Approved SUBSTITUTIONS">SUBSTITUTIONS</a>', &
                e_small
        else
            write(device,AFORMAT) horizontal, &
                '<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
                b_tr//b_td_nbsp_e_td//b_tdar//b_small, &
                '<a href="#FEASIBLE SUBJECTS">FEASIBLE</a> subjects |', &
                '<a href="#ADVISED SUBJECTS">ADVISED</a> subjects |', &
                'Approved <a href="#Approved SUBSTITUTIONS">SUBSTITUTIONS</a> |', &
                'Change <a href="#TOP">Curriculum</a> |', &
                '<a href="#TOP">Top</a>', &
                e_small//e_td//e_tr//e_table//horizontal
        end if

    end subroutine checklist_links


    subroutine class_list (device, thisTerm, idx, mesg)

        integer, intent (in) :: device, thisTerm
        integer, intent (in), optional :: idx
        character(len=*), intent (in), optional :: mesg

        integer :: n_count, tdx, ierr, ncol, iSubj
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
            tClassId = Section(thisTerm,idx)%ClassId
        else
            call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, ierr)
            targetSection = index_to_section(tClassId, thisTerm)
        end if
        iSubj = Section(thisTerm,targetSection)%SubjectIdx
#if defined UPLB
        targetDepartment = Subject(iSubj)%DeptIdx
#else
        targetDepartment = Section(thisTerm,targetSection)%DeptIdx
#endif
        targetCollege = Department(targetDepartment)%CollegeIdx

        call collect_students_in_section (thisTerm, targetSection, n_count, tArray)

        if (isSubject_lecture_lab(iSubj)) then
            header = trim(ftoa(Subject(iSubj)%LectHours+Subject(iSubj)%LabHours,2))//' hrs ('// &
                trim(ftoa(Subject(iSubj)%LectHours,2))//' lect + '// &
                trim(ftoa(Subject(iSubj)%LabHours,2))//' lab/recit).'
        else if (Subject(iSubj)%LectHours > 0.0) then
            header = trim(ftoa(Subject(iSubj)%LectHours,2))//' hrs lect.'
        else if (Subject(iSubj)%LabHours > 0.0) then
            header = trim(ftoa(Subject(iSubj)%LabHours,2))//' hrs lab/recit.'
        end if
        header = trim(Subject(iSubj)%Title)//'/ '//trim(ftoa(Subject(iSubj)%Units,1))//' units/ '//header

        ncol = n_count
        do tdx=1,Section(thisTerm,targetSection)%NMeets
            tArray(ncol+1) = targetSection
            tArray(ncol+2) = tdx
            tArray(ncol+3) = 0
            ncol = ncol+3
        end do
        tArray(ncol+1) = 0
        tArray(ncol+2) = 0
        tArray(ncol+3) = 0

        call html_write_header(device, 'CLASSLIST', errMsg)
        call list_sections_to_edit(device, thisTerm, ncol-n_count, tArray(n_count+1:), 0, SPACE, SPACE, .false., .true., &
            b_bold//trim(Subject(iSubj)%Name)//' - '//trim(header)//e_bold//SPACE// &
            trim(make_href(fnPrintableClasslist, 'Printable', A1=tClassId, A9=thisTerm, &
            pre=nbsp//b_small//'( ', post=' )'//e_small//linebreak//linebreak, newtab='"_blank"')) ) !, .true. )

        call html_student_list (device, n_count, tArray, &
                isRole_dean_of_college(Department(Section(thisTerm,targetSection)%DeptIdx)%CollegeIdx, orHigherUp), SPACE )
        write(device,AFORMAT) horizontal

        if ( ( College(targetCollege)%isAllowed(ToEditCLASSLISTS,thisTerm) .and. &
               isRole_teacher_of_class(Section(thisTerm,targetSection),orHigherUp) ) .or. &
              isRole_admin_of_college(targetCollege) ) then
            call make_form_start(device, fnStudentForceEnlist, A9=thisTerm)
            write(device,AFORMAT) &
                '<input type="hidden" name="A2" value="'//trim(tClassId)//'">', &
                '<table border="0" width="80%" cellpadding="0" cellspacing="0">', &
                b_tr//b_td//b_bold//'Student number'//e_bold//': <input name="A1" value=""> '//nbsp// &
                '<input type="submit" name="action" value="Add">'//nbsp//nbsp//nbsp// &
                ' or '//nbsp//nbsp//nbsp, &
                '<input type="submit" name="action" value="Delete"> (Specify ALL to delete all students)'// &
                e_form//e_td//e_tr//e_table//horizontal
        end if

    end subroutine class_list


    subroutine collect_advice(Advice, n_changes, mesg)

        type (TYPE_PRE_ENLISTMENT), intent(out) :: Advice
        character(len=*), intent (out) :: mesg
        integer, intent (out) :: n_changes

        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=3*MAX_LEN_SUBJECT_CODE) :: search_string

        integer :: iSubj, idx, jdx, ierr, i, j, l
        real :: tUnits

        call html_comment('collect_advice()')

        call initialize_pre_enlistment(Advice)
        mesg = SPACE
        n_changes = 0

        call cgi_get_named_float(QUERY_STRING, 'earned', tUnits, ierr)
        if (ierr==0) Advice%UnitsEarned = tUnits
        call cgi_get_named_integer(QUERY_STRING, 'classification', i, ierr)
        if (ierr==0) Advice%levelClassification = i
        call cgi_get_named_integer(QUERY_STRING, 'year', i, ierr)
        if (ierr==0) Advice%levelYear = i
        call cgi_get_named_float(QUERY_STRING, 'allowed', tUnits, ierr)
        if (ierr==0) Advice%AllowedLoad = tUnits
        call cgi_get_named_integer(QUERY_STRING, 'group', i, ierr)
        if (ierr==0) Advice%levelPriority = i
        call cgi_get_named_integer(QUERY_STRING, 'priority', i, ierr)
        if (ierr==0) Advice%NPriority = i
        call cgi_get_named_integer(QUERY_STRING, 'alternate', i, ierr)
        if (ierr==0) Advice%NAlternates = i
        call cgi_get_named_integer(QUERY_STRING, 'current', i, ierr)
        if (ierr==0) Advice%NCurrent = i
        call cgi_get_named_integer(QUERY_STRING, 'status', i, ierr)
        if (ierr==0) Advice%statusAdvising = i

        Advice%lenSubject = Advice%NPriority+Advice%NAlternates+Advice%NCurrent
        do l=1,Advice%lenSubject
            call cgi_get_named_string(QUERY_STRING, 'pri'//itoa(l), search_string, ierr)
            j = index(search_string,COMMA)
            tSubject = search_string(j+1:)
            read(tSubject,'(f6.4)') Advice%Contrib(l)
            tSubject = search_string(1:j-1)
            iSubj = index_to_subject(tSubject)
            Advice%Subject(l) = iSubj
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
            iSubj = Advice%Subject(l)
            mesg = trim(mesg)//' : Del '//Subject(iSubj)%Name
            Advice%Contrib(l) = 0.0
            Advice%Subject(l) = 0
            n_changes = n_changes + 1
            call html_comment('Delete '//itoa(l)//SPACE//Subject(iSubj)%Name )

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
            iSubj = index_to_subject(tSubject)
            if (iSubj>=0) then ! add
                call html_comment('Add '//Subject(iSubj)%Name )
                mesg = trim(mesg)//' : Add '//tSubject
                do l=Advice%lenSubject,1,-1
                    Advice%Subject(l+1) = Advice%Subject(l)
                    Advice%Contrib(l+1) = Advice%Contrib(l)
                end do
                Advice%Subject(1) = iSubj
                Advice%Contrib(1) = 1.0
                Advice%NPriority = 1+Advice%NPriority
                Advice%lenSubject = 1+Advice%lenSubject
                n_changes = n_changes + 1
            end if
        end if

    end subroutine collect_advice


    subroutine collect_students_in_section (thisTerm, section_index, n_count, sList)

        integer, intent (in) :: thisTerm, section_index
        integer, intent (out) :: n_count, sList(1:)

        integer :: ldx, tdx, iStd, ncol, iSubj, iSect
        character(len=MAX_LEN_CLASS_ID) :: tClassId

        call html_comment('collect_students_in_section()')

        ! what subject ?
        iSubj = Section(thisTerm,section_index)%SubjectIdx

        ! collect students in section
        n_count = 0
        do tdx=1,NumStudents+NumAdditionalStudents
            iStd = StdRank(tdx)
            do ncol=1,Student(iStd)%Enlistment(thisTerm)%lenSubject
                iSect = Student(iStd)%Enlistment(thisTerm)%Section(ncol)
                if (iSect==0) cycle
                if (section_index == iSect) then
                    n_count = n_count+1
                    sList(n_count) = iStd
                    exit
                elseif (Student(iStd)%Enlistment(thisTerm)%Subject(ncol)==iSubj .and. isSubject_lecture_lab(iSubj)) then
                    ldx = index(Section(thisTerm,iSect)%ClassId,DASH)
                    if (ldx>0) then ! student is accommodated in a lab section
                        if (trim(tClassId)==Section(thisTerm,iSect)%ClassId(:ldx-1)) then ! lab of lecture
                            n_count = n_count+1
                            sList(n_count) = iStd
                            exit
                        end if
                    end if
                end if
            end do
        end do

    end subroutine collect_students_in_section


    subroutine html_college_info(device, iColl)

        integer, intent(in) :: device
        integer, intent(in) :: iColl

        integer :: tdx, rdx, ldx, cdx, dept, n, n_curr, n_count, numAreas, iCurr
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege, tGroup
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character (len=80) :: description
        logical :: hasRooms, hasTeachers, hasStudents, hasSubjects
        character (len=20) :: strCurrent, strNext, colorCurrent

        tCollege = College(iColl)%Code
        call html_comment('html_college_info('//tCollege//')')

        ! students
        call student_counts(targetTerm)
        hasStudents = CollegeCount(iColl)>0

        ! rooms
        n_count = 0
        do rdx=1,NumRooms+NumAdditionalRooms
            if (Department(Room(rdx)%DeptIdx)%CollegeIdx /= iColl) cycle
            n_count = n_count+1
            exit
        end do
        hasRooms = n_count>0

        ! teachers
        n_count = 0
        do tdx=1,NumTeachers+NumAdditionalTeachers
            if (Department(Teacher(tdx)%DeptIdx)%CollegeIdx /= iColl) cycle
            n_count = n_count+1
            exit
        end do
        hasTeachers = n_count>0

        ! any curricular programs
        n_curr = 0
        do iCurr=1,NumCurricula-1
            if (Curriculum(iCurr)%CollegeIdx /= iColl) cycle
            n_curr = n_curr+1
            exit
        end do

        ! subject areas in unit
        numAreas = 0
#if defined UPLB
        do cdx=1,NumSubjectAreas
            if (SubjectArea(cdx)%CollegeIdx==iColl) then
                numAreas = numAreas+1
                tArray(numAreas) = cdx
            end if
        end do
#else
        ! Subjects administered by program
        do cdx=1,NumSubjectAreas
            do iCurr=1,NumCurricula-1
                if (Curriculum(iCurr)%CollegeIdx/=iColl) cycle
                if (isSubject_area_used_in_curriculum(Curriculum(iCurr), trim(SubjectArea(cdx)%Code)//SPACE)) then
                    numAreas = numAreas+1
                    tArray(numAreas) = cdx
                    exit
                end if
            end do
        end do
#endif
        hasSubjects = n_curr>0 .or. numAreas>0

        write(device,AFORMAT) '<ul>', b_item

        ! search box
        call make_form_start(device, fnSearchCategory, A3=tCollege, A9=targetTerm)
        write(device,AFORMAT) b_bold//'Search '//e_bold//' for <input name="A1" value=""> in '
        !if (hasStudents .and. isRole_dean_of_college(iColl, orHigherUp) ) &
        if ( (isRoleSysAd .or. isRoleStaff .or. isRoleDean .or. isRoleChair .or. isRoleOfficial) .and. hasStudents) &
            write(device,AFORMAT) nbsp//nbsp//' <input type="submit" name="A2" value="Students">'
        if (hasTeachers) write(device,AFORMAT) nbsp//nbsp//' <input type="submit" name="A2" value="Teachers">'
        if (hasRooms) write(device,AFORMAT) nbsp//nbsp//' <input type="submit" name="A2" value="Rooms">'
        write(device,AFORMAT) &
            nbsp//nbsp//' <input type="submit" name="A2" value="Subjects">', &
            nbsp//nbsp//' <input type="submit" name="A2" value="Curricula">', &
            nbsp//nbsp//' <input type="submit" name="A2" value="Blocks">', &
            e_form, e_item

        ! subjects
        if (n_curr>0) then
            write(device,AFORMAT) b_item//b_bold//'Curricular programs'//e_bold//' : '
            done = .false.
            do cdx=1,NumCurricula-1
                if (Curriculum(cdx)%CollegeIdx /= iColl) cycle
                if (done(cdx)) cycle
                n_count = 1
                do iCurr=cdx+1,NumCurricula-1
                    if (CurrProgCode(iCurr)/=CurrProgCode(cdx)) cycle
                    n_count = n_count+1
                end do

                write(device,AFORMAT) trim(make_href(fnCurriculumList, CurrProgCode(cdx), &
                    A1=CurrProgCode(cdx), post='('//trim(itoa(n_count))//')'//nbsp))
                do iCurr=cdx+1,NumCurricula-1
                    if (CurrProgCode(iCurr) == CurrProgCode(cdx)) done(iCurr) = .true.
                end do
            end do
            write(device,AFORMAT) e_item
        end if
        call links_to_subjects(device, iColl, numAreas, tArray(1))

        ! rooms
        if (hasRooms) then
            call html_comment('room links()')
            write(device,AFORMAT) b_item//b_bold//'Rooms'//e_bold//' in : '
            do dept=2,NumDepartments
                if (Department(dept)%CollegeIdx /= iColl) cycle
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
            write(device,AFORMAT) e_item
        end if

        ! teachers
        if (hasTeachers) then
            call html_comment('teacher links()')
            write(device,AFORMAT) b_item//b_bold//'Teachers'//e_bold//' in : '
            do dept=2,NumDepartments
                if (Department(dept)%CollegeIdx /= iColl) cycle
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
            write(device,AFORMAT) e_item

        end if

        ! blocks
        if (n_curr>0 ) then
            call html_comment('links_to_blocks('//College(iColl)%Code//')')
            call links_to_blocks(device, iColl, targetTerm)
        end if

        ! classes
        if (numAreas>0 .and. hasStudents) then
            call html_comment('links_to_sections('//tCollege//')')
            call links_to_sections(device, iColl, numAreas, tArray(1), targetTerm, tArray(numAreas+1) )
        end if

        ! students
        if (hasStudents) then

            call html_comment('student links()')

            write(device,AFORMAT) b_item, b_bold//'Student distribution'//e_bold, &
                trim(make_href(fnDistributionPSGC, 'by home address', A1=tCollege, A2='ALL', pre=nbsp, post=COMMA)), &
                trim(make_href(fnAdvisingEnlistmentStatus, 'by advising and enlistment', A1=tCollege, A2='ALL', &
                    A9=targetTerm, pre=nbsp, post=' status, and')), &
                trim(make_href(fnGenderDistribution, 'by gender', A1=tCollege, A9=targetTerm, pre=nbsp, post=DOT))
            if (isRegistrar .or. isRoleOfficial) then
                write(device,AFORMAT)  b_bold//'Hide'//e_bold//nbsp//trim(tCollege)//' students', &
                    trim(make_href(fnStudentsNotEnrolled, 'not enrolled this year.', A1=tCollege, pre=nbsp))
            end if
            write(device,AFORMAT) e_item

            if (isRole_dean_of_college(iColl, orHigherUp) ) then
                write(device,AFORMAT) b_item//b_bold//'Grade averages'//e_bold//' :'
                done = .false.
                do cdx=1,NumCurricula-1
                    if (Curriculum(cdx)%CollegeIdx /= iColl) cycle
                    if (done(cdx)) cycle
                    write(device,AFORMAT) trim(make_href(fnSummaryWAG, CurrProgCode(cdx), &
                        A1=CurrProgCode(cdx), A2='CUMULATIVE', pre=nbsp))
                    do iCurr=cdx+1,NumCurricula-1
                        if (CurrProgCode(iCurr) == CurrProgCode(cdx)) done(iCurr) = .true.
                    end do
                end do
                write(device,AFORMAT) e_item
            end if

        end if

        ! display for students & guests ends here
        if ( isRoleStudent .or. isRoleGuest .or. isRoleBenefactor ) then
            write(device,AFORMAT) '</ul>'//horizontal
            return
        end if

        if (numAreas>0 .and. hasStudents) then

            ! results of Needs Analysis
            description = SPACE
            tGroup = SPACE

            if (iColl==NumColleges) then
                tGroup = 'ALL'
                description = 'ALL students'

            else if ( (isRole_dean_of_college(iColl) .and. iColl==CollegeIdxUser) .or. isRole_admin_of_college(iColl) )then
                tGroup = tCollege
                description = trim(tCollege)//' students'

            else if (requestingTeacher>0) then
                if (Teacher(requestingTeacher)%NumAdvisees>0 .and. iColl==CollegeIdxUser) then
                    tGroup = 'ADVISEES'
                    description = 'My advisees'
                end if
            end if

            if (len_trim(description)>0) then

                write(device,AFORMAT) b_item
                call make_form_start(device, fnAdvisingCode, A1=tGroup, A9=targetTerm)
                write(device,AFORMAT) 'List '//b_bold//trim(description)//e_bold//' <select name="A2">'
                do ldx=MAX_ALL_SCHOLASTIC_STATUS+MAX_ALL_ANALYSIS_CODES,1,-1
                    write(device,AFORMAT) '<option value="'//trim(itoa(ldx))//'"> '//trim(descScholastic(ldx))
                end do
                write(device,AFORMAT) '</select>'//nbsp, &
                    '<input type="submit" name="action" value="Submit">', &
                    e_form, e_item
            end if

            ! needs analysis
#if defined UPLB
            call links_to_depts(device, iColl, fnEnlistmentSummary, targetTerm, &
                b_bold//'Demand for subjects'//e_bold )
            call links_to_depts(device, iColl, fnBottleneck, targetTerm, &
                b_bold//'Subjects for which demand > available seats'//e_bold )
            call links_to_depts(device, iColl, fnExtraSlots, targetTerm, &
                b_bold//'Subjects for which available seats > demand'//e_bold )
#else
            write(device,AFORMAT) b_item, b_bold//'Needs Analysis'//e_bold//' :',&
                trim(make_href(fnEnlistmentSummary, 'summary', &
                    A1=tCollege, A9=targetTerm, pre=nbsp, post=';')), &
                trim(make_href(fnBottleneck, 'demand > available seats', &
                    A1=tCollege, A9=targetTerm, pre=' top subjects for which ', post=COMMA)), &
                trim(make_href(fnExtraSlots, 'available seats > demand', &
                    A1=tCollege, A9=targetTerm, pre=nbsp, post=DOT)), &
                e_item
#endif

            ! enlistment
            write(device,AFORMAT) b_item, b_bold//'Enlistment'//e_bold//' :', &
                trim(make_href(fnStudentsWithConflicts, 'conflicts', A1=tCollege, A9=targetTerm, pre=nbsp, post=';')), &
                trim(make_href(fnUnderloadSummary, 'underloads/overloads', A1=tCollege, A9=targetTerm, pre=nbsp, post=DOT)), &
                e_item

            ! evaluation summary for college
            if (isRole_dean_of_college(iColl, orHigherUp) ) then
                write(device,AFORMAT) b_item, b_bold//'Evaluation '//e_bold//' : summary report of', &
                    trim(make_href(fnEvaluationRatings, 'instruction/teaching effectiveness', &
                    A1=tCollege, A2='College', A9=targetTerm, post=DOT )), e_item
            end if

            ! gradesheets
            write(device,AFORMAT) b_item, &
                b_bold//'Gradesheets'//e_bold//' :', &
                trim(make_href(fnUnpostedGrades, 'unposted', A1=tCollege, A9=targetTerm, pre=nbsp)), &
                trim(make_href(fnGradesheetsToReceive, 'no hardcopy', A1=tCollege, A9=targetTerm, pre=nbsp, post=DOT)), &
                e_item

        end if

        write(device,AFORMAT) '</ul>'

        ! Registrar or Official only for the remainder of page
        if (.not. (isRegistrar .or. isRoleOfficial) ) then
            write(device,AFORMAT) horizontal
            return
        end if

#if defined UPLB
        if (iColl==NumColleges) then
#endif
        if (hasStudents) then

            write(device,AFORMAT) horizontal, b_bold//'Permissions for '//trim(text_school_year(currentYear))//' / '// &
                trim(txtSemester(targetTerm+6))//trim(termQualifier(targetTerm+6))//e_bold, '<ul>'

            ! disable/enable changes to data
            do n=AdviseOpenSubjects,ToEditGRADES
                if (College(iColl)%isAllowed(n,targetTerm)) then
                    strCurrent = 'enabled'
                    colorCurrent = red
                    strNext = 'DISABLE'
                else
                    strCurrent = 'disabled'
                    colorCurrent = green
                    strNext = 'ENABLE'
                end if
                write(device,AFORMAT) b_item, &
                    trim(AllowedAction(n))//' is '//colorCurrent//trim(strCurrent)//e_color//DOT, &
                    trim(make_href(fnTogglePermission, strNext, A1=tCollege, A2=itoa(n), &
                        A9=targetTerm, pre=nbsp, post=DOT)), e_item
            end do
            write(device,AFORMAT) '</ul>'

        end if

#if defined UPLB
        end if !(iColl==NumColleges)
#endif

        ! preenlist/delist students
        if (hasSubjects .and. hasStudents .and. targetTerm==currentTerm) then
            write(device,AFORMAT) horizontal
            call needs_analysis_and_preenlistment_menu(device, targetTerm, iColl, 2, .false.)
        end if

        if (iColl==NumColleges) then

#if defined UPLB
            ! preenlist/delist students
            if (targetTerm==currentTerm) then
                write(device,AFORMAT) horizontal
                call needs_analysis_and_preenlistment_menu(device, targetTerm, iColl, 1, .false.)
            end if
#endif

            write(device,AFORMAT) horizontal, b_bold//'Other'//e_bold, '<ul>', &
                b_item, b_bold//'Online users'//e_bold//' are:', &
                trim(make_href(fnOnlineStudents, 'students', pre=nbsp, post=COMMA)), &
                trim(make_href(fnOnlineTeachers, 'teachers', pre=nbsp, post=DOT))
            if (isReadOnly) then
                write(device,AFORMAT) trim(make_href(fnReload, 'Reload', pre=nbsp, post=' data.'))
            end if
            write(device,AFORMAT) &
                trim(make_href(fnFileDownload, 'Backup', A1='BACKUP.XML', pre=nbsp, post=' data.')), &
                trim(make_href(fnResetPasswords, 'Reset', pre=nbsp, post=' passwords.')), &
                trim(make_href(fnStop, 'Stop', pre=nbsp, post=SPACE//PROGNAME//DOT)), &
                e_item

            if (.not. isReadOnly) then
                write(device,AFORMAT) b_item//b_bold//'Student, Official, and Guest roles'//e_bold//' are '
                if (isAllowedNonEditors) then
                    write(device,AFORMAT) red//'allowed'//e_color//' to log in.', &
                        trim(make_href(fnLoginByNonEditors, 'DISALLOW', pre=nbsp, post=DOT//e_item))
                else
                    write(device,AFORMAT) green//'not allowed'//e_color//' to log in. ', &
                        trim(make_href(fnLoginByNonEditors, 'ALLOW', pre=nbsp, post=DOT//e_item))
                end if
            end if

            write(device,AFORMAT) &
                b_item, b_bold//'Edit : '//e_bold, &
                trim(make_href(fnEditEMERGENCY, 'emergency message', pre=nbsp, post=COMMA)), &
                trim(make_href(fnEditMOTD, 'message of the day', pre=nbsp, post=COMMA)), &
                trim(make_href(fnTimers, 'timers', pre=nbsp, post=COMMA)), &
                trim(make_href(fnEditSignatories, 'signatories', pre=nbsp, post=COMMA)), &
                trim(make_href(fnEditFees, 'school fees', pre=nbsp, post=COMMA)), &
                trim(make_href(fnEditScholarships, 'scholarships', pre=nbsp, post=COMMA)), &
                trim(make_href(fnEditEquivalencies, 'equivalence rules', pre=nbsp, post=DOT)), &
                e_item

            write(device,AFORMAT) b_item, &
                b_bold//'Download'//e_bold, &
                trim(make_href(fnFileDownload, 'STUDENT-INFO.CSV', A1='STUDENT-INFO.CSV', A9=targetTerm, pre=nbsp, post=DOT)), &
                nbsp//' Right-click on link, then "Save Link As ..." ', &
                trim(make_href(fnFileDownload, 'PASSWORDS-TEACHERS.CSV', A1='PASSWORDS-TEACHERS.CSV', pre=nbsp)), &
                trim(make_href(fnFileDownload, 'PASSWORDS-STUDENTS.CSV', A1='PASSWORDS-STUDENTS.CSV', pre=nbsp))

            if (NumBlocks(targetTerm)+NumSections(targetTerm)>0) then
                write(device,AFORMAT) &
                    trim(make_href(fnFileDownload, 'CLASSES-BLOCKS-'//trim(txtSemester(targetTerm))//'.CSV', &
                        A1='CLASSES-BLOCKS.CSV', A9=targetTerm, pre=nbsp)), &
                    trim(make_href(fnFileDownload, 'ENLISTMENT-'//trim(txtSemester(targetTerm))//'.CSV', &
                        A1='ENLISTMENT.CSV', A9=targetTerm, pre=nbsp))
            end if
            write(device,AFORMAT) e_item, b_item

            call make_form_start(device, fnStudentAdd)
            write(device,AFORMAT)  &
                !b_bold//'Remove'//e_bold//nbsp//trim(tCollege)//' students', &
                !trim(make_href(fnStudentsNotEnrolled, 'not enrolled this year', A1=tCollege, post=DOT//nbsp)), &
                !nbsp//
                b_bold//'Add'//e_bold//' student number <input name="A1" value=""> '//nbsp// &
                '<input type="submit" name="action" value="Add">'//e_form, e_item, b_item

            call make_form_start(device, fnUploadCSV, enctype=' enctype="multipart/form-data"')
            write(device,AFORMAT) '<input type="file" name="file">', &
                '<input type="submit" value="Upload"> '// &
                b_small//' new students: format is STDNO,LASTNAME,FIRST JR/III, MIDDLE or '// &
                    'STDNO,"LASTNAME,FIRST JR/III, MIDDLE"'//e_small, &
                '</form>', &
                e_item, '</ul>'

!            ! local file
!            write(device,AFORMAT) '<input type="file" name="file">'
!            ! year
!            write(device,AFORMAT) nbsp//'for <select name="year">', '<option value="0"> '//SPACE
!            do ldx=currentYear,baseYear,-1
!                write(device,AFORMAT) '<option value="'//trim(itoa(ldx))//'"> '//trim(text_school_year(ldx))
!            end do
!            write(device,AFORMAT) '</select>'//nbsp
!            ! term
!            write(device,AFORMAT) nbsp//' <select name="term">', '<option value="0"> '//SPACE
!            do ldx=firstSemester,summerTerm
!                write(device,AFORMAT) '<option value="'//trim(itoa(ldx))//'"> '//trim(txtSemester(ldx+3))//termQualifier(ldx+3)
!            end do
!            write(device,AFORMAT) '</select>'//nbsp, &
!                '<input type="submit" value="Upload"> '// &
!                '</form>', &
!                e_item, '</ul>'

        end if

        write(device,AFORMAT) horizontal

    end subroutine html_college_info


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
                    A1=tCollege, pre=b_item, post=' - '//trim(College(cdx)%Name)//e_item))
            end do
            write(device,aformat) '</ul>'//horizontal
        else
            call html_write_header(device, College(targetCollege)%Code//'- '//College(targetCollege)%Name, mesg)
            call html_college_info(device, targetCollege)
        end if

    end subroutine html_college_links


!    subroutine html_copyright(device)
!        integer, intent(in) :: device
!
!        write(device,AFORMAT) &
!            b_small//b_italic//PROGNAME//nbsp//COPYRIGHT//linebreak, &
!            'This program comes with ABSOLUTELY NO WARRANTY; for details see the GNU GENERAL PUBLIC LICENSE Version 3 ', &
!            '(<a target="0" href="http://www.gnu.org/licenses/gpl-3.0.html">GPLv3</a>).'//linebreak, &
!            'This program is free software, and you are welcome to redistribute it under certain conditions; ', &
!            ' see the GPLv3 for details.'//linebreak, &
!            ' Support for the development of this program was provided by: '// &
!            ' University of the Philippines Los Banos (1997-2001); '// &
!            ' BalikScientist Program of the Department of Science and Technology (2010); '// &
!            ' Isabela State University (2011); and '// &
!            ' Cagayan State University (2012, 2013). ', &
!            'The source code is available at <a target="0" href="'//WEB//'">'//WEB//'</a>'//linebreak, &
!            CONTACT//' This is '//PROGNAME//' Version '//VERSION//e_italic//e_small//horizontal
!
!    end subroutine html_copyright
!
!
!    subroutine html_home_page(device, mesg)
!        integer, intent(in) :: device
!        character(len=*), intent(in) :: mesg
!
!        call html_comment('html_home_page('//trim(mesg)//')')
!        write(device,AFORMAT) &
!            '<html><head><title>'//trim(UniversityCode)//SPACE//PROGNAME//'</title></head><body>', &
!            '<h1>'//trim(UniversityCode)//SPACE//PROGNAME//'</h1>', &
!            '<h3>'//red//trim(mesg)//e_color//'</h3>', &
!            b_small//b_italic//'<a href="'//trim(SERVER_PROTOCOL)//trim(SERVER_NAME)//'">Home</a>'//e_italic//e_small
!
!    end subroutine html_home_page


    subroutine html_login_page(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_USERNAME) :: tTeacher
        character (len=MAX_LEN_PASSWD_VAR) :: tPassword

        call html_comment('html_login_page()')
#if defined no_password_check
        tTeacher = PROGNAME
#else
        tTeacher = GUEST
#endif
        tPassword = GUEST

        write(device,AFORMAT) &
            '<html><head><title>'//trim(UniversityCode)//SPACE//PROGNAME//'</title></head><body>', &
            '<h2>Welcome to '//trim(UniversityCode)//SPACE//PROGNAME//' for '// &
            trim(text_school_year(currentYear))//' !</h2>'//horizontal, &
            '<table border="0" width="100%">'//b_tr, &
            '<td valign="top" width="25%">'
        call make_form_start(device, fnLogin)
        write(device,AFORMAT) &
            b_bold//'Username'//e_bold//':'//linebreak, &
            '<input type="text" name="U" value="'//trim(tTeacher)//'">'//linebreak//linebreak, &
            b_bold//'Password:'//e_bold//linebreak, &
            '<input type="password" name="P" value="'//trim(tPassword)//'">'//linebreak//linebreak, &
            '<input type="submit" value="Login">', &
            nbsp//b_small//b_italic//'<a href="'//trim(SERVER_PROTOCOL)//trim(SERVER_NAME)// &
            '">Home</a>'//e_italic//e_small//e_form
        if (len_trim(UserRequestCheckMessage)>0) write(device,AFORMAT) &
            linebreak//red//b_italic//trim(UserRequestCheckMessage)//e_italic//e_color//linebreak
        write(device,AFORMAT) e_td//b_td

        if (isMirror) then
            write(device,AFORMAT) red//b_bold//'IMPORTANT !'//e_bold//nbsp//e_color, &
                'You will be logged in to a mirror of '//trim(UniversityCode)//SPACE//PROGNAME//DOT, &
                'Information here is updated every '//itoa(maxRefreshTime/60)//' minutes.', &
                e_color//e_bold//linebreak//linebreak
        else if ( len_trim(MOTD)>0 ) then
            write(device,AFORMAT) red//b_bold//'IMPORTANT !'//e_bold//e_color//nbsp//trim(MOTD)//linebreak//linebreak
        end if

        write(device,AFORMAT) &
            b_bold//'Forgot your Username and/or Password?'//e_bold, &
            b_italic//' Visit your Dean or the Registrar; bring University-issued ID.'//e_italic, &
            linebreak, &
            linebreak//b_bold//'Tips:'//e_bold//'<ul>', &
            b_item//'Change your initial password. Do not forget to logout.'//e_item, &
            b_item//'Do not use the browser ''Back'' button unless '// &
            ' there are no hyperlinks or ''Submit'' buttons on the displayed page.'//e_item, &
            '</ul>', e_td//e_tr//e_table//horizontal
        call html_copyright(device)

    end subroutine html_login_page


    subroutine html_student_list (device, sLen, sList, addLinks, searchString)

        integer, intent (in) :: device, sLen, sList(1:sLen)
        logical, intent (in) :: addLinks
        character(len=*), intent (in) :: searchString ! for fn==fnSearchCategory

        integer :: iCurr, tdx, iStd, iTeach, nClasses(3), nAdvised(3), iTerm, ldx
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo

        integer :: tLen
        character(len=80) :: location, tString

        call html_comment('html_student_list()')
        tString = searchString
        tLen = len_trim(tString)

        if (sLen == 0) then
            write(device,AFORMAT) BRNONE
        else
            if (addLinks) then
                write(device,AFORMAT) linebreak//'<table border="0" width="100%">', &
                    b_tr//b_thal//'#'//e_th//b_thal//'STDNO'//e_th, &
                    b_thal//'NAME OF STUDENT'//e_th// &
                    b_thal//'SEX'//e_th, &
                    b_thal//'PROGRAM'//e_th, &
                    b_thal//'LINKS'//e_th
            else
                write(device,AFORMAT) linebreak//'<table border="0" width="70%">', &
                    b_tr//b_thal//'#'//e_th//b_thal//'STDNO'//e_th, &
                    b_thal//'NAME OF STUDENT'//e_th//b_thal//'SEX'//e_th//b_thal//'PROGRAM (Yr/Adv/Enl)'//e_th
            end if
            if (tLen>0) then
                write(device,AFORMAT) b_thal//'"'//tString(:tLen)//'" found in ...'//e_th
            end if
            write(device,AFORMAT) e_tr

            do tdx=1,sLen
                iStd = sList(tdx)
                tStdNo = Student(iStd)%StdNo
                iTeach = index_to_teacher(Student(iStd)%Adviser)
                iCurr = Student(iStd)%CurriculumIdx

                call count_preenlistment(iStd, 0, nClasses, nAdvised)

                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(tdx,2))//'">'// &
                    b_td//trim(itoa(tdx))//DOT//e_td//b_td//tStdNo//e_td, &
                    b_td//trim(Student(iStd)%Name)//e_td, &
                    b_td//trim(Student(iStd)%Gender)//e_td, &
                    b_td//trim(Curriculum(iCurr)%Code)//' ('// &
                    trim(txtYear(Student(iStd)%Enlistment(targetTerm)%levelYear+10))//FSLASH// &
                    trim(txtStatusCode(Student(iStd)%Enlistment(targetTerm)%statusAdvising))//FSLASH// &
                    trim(txtStatusCode(Student(iStd)%Enlistment(targetTerm)%statusEnlistment))//')'//e_td

                if ( addLinks ) then
                    if  (isRole_adviser_of_student(iStd,orHigherUp) .or. isRole_benefactor_of_student(iStd)) then

                        write(device,AFORMAT) b_td//b_small, trim(Student(iStd)%Scholarship)
                        if (.not. isRoleBenefactor) then
                            write(device,AFORMAT) &
                                trim(make_href(fnStudentEdit, 'Info', A1=tStdNo, pre=nbsp)), &
                                trim(make_href(fnRecentStudentActivity, 'Log', A1=tStdNo, pre=nbsp)), &
                                trim(make_href(fnTeacherClasses, 'Adviser', A1=Teacher(iTeach)%TeacherId, A9=currentTerm))
                        end if
                        write(device,AFORMAT) &
                            trim(make_href(fnFees, 'Fees', A1=tStdNo, pre=nbsp)), &
                            trim(make_href(fnStudentGrades, 'Grades', A1=tStdNo, pre=nbsp)), &
                            trim(make_href(fnEditCheckList, 'Checklist', A1=tStdNo, A9=currentTerm, pre=nbsp))

                        do iTerm=firstSemester,summerTerm
                            if (nClasses(iTerm)+nAdvised(iTerm)>0) write(device,AFORMAT) &
                                trim(make_href(fnStudentClasses, txtSemester(iTerm+6), A1=tStdNo, A9=iTerm, pre=nbsp))
                        end do
                        if (isRoleSysAd) then
                            write(device,AFORMAT) &
                                trim(make_href(fnSwitchUser, 'Login', A1=tStdNo, pre=nbsp, newtab='"_blank"'))
                        end if
                        write(device,AFORMAT) e_small//e_td

                    else
                        write(device,AFORMAT) b_td_nbsp_e_td
                    end if
                end if

                if (tLen>0) then
                    call student_search_info(iStd, tString(:tLen), targetTerm, sLen+1, location)
                    ldx = max(3, len_trim(location) )
                    write(device,AFORMAT) b_td//location(:ldx-2)//e_td
                end if

                write(device,AFORMAT) e_tr
            end do
            write(device,AFORMAT) e_table
        end if

    end subroutine html_student_list


    subroutine html_write_footer(device)

        integer, intent(in) :: device
        integer :: fn

        if (REQUEST==fnFileDownload) return
        call html_comment('html_write_footer()')

        select case (REQUEST)

            ! no footer
            case (fnStop, fnLogout, fnLoginPage, fnHomePage, fnChangeTeacherPassword, fnChangeStudentPassword)
            case (fnPrintableClasslist, fnPrintableGradesheet, fnPrintableWorkload)
            case (fnPrintableSchedule, fnGradeCertification, fnTranscript)

            case default

                write(device,AFORMAT) &
                    '<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
                    b_tr//b_td// &
                    b_small//b_italic//'Generated '//currentDate(1:4)//FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8)// &
                    DASH//currentTime(1:2)//':'//currentTime(3:4)//' by '// &
                    !'<a target="0" href="'//WEB//'">'//PROGNAME//'</a>'// &
                    PROGNAME//trim(VERSION)//DOT
                if (.not. isRoleSysAd) write(device,AFORMAT) nbsp//'Please report errors to the Registrar.'
                ! Change password, logout
                write(device,AFORMAT) e_italic//e_small//e_td//b_tdar//b_small, &
                    'User is '//trim(USERNAME)//'@'//REMOTE_ADDR//nbsp
                if (USERNAME/=GUEST) then
                    if (isRoleStudent) then
                        fn = fnChangeStudentPassword
                    else
                        fn = fnChangeTeacherPassword
                    end if
                    write(device,AFORMAT) trim(make_href(fn, 'Change password'))
                end if
                write(device,AFORMAT) trim(make_href(fnLogout, 'Quit', pre=nbsp))

                ! emergency stop
                if (trim(USERNAME)==trim(PROGNAME)) then
                    write(device,AFORMAT) trim(make_href(fnStop, 'Stop', pre=nbsp))
                end if

                write(device,AFORMAT) e_small//e_td//e_tr//e_table

        end select

        write(device,AFORMAT) '</body></html>'

    end subroutine html_write_footer


    subroutine html_write_header(device, header, errmsg)
        integer, intent (in) :: device
        character(len=*), intent (in) :: header
        character(len=*), intent (in), optional :: errmsg
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character(len=MAX_LEN_USERNAME) :: tTeacher
        integer :: gdx, tdx, nClasses(3), nAdvised(3), iTerm

        call html_comment('html_write_header('//trim(header)//')')

        ! page title, start of body
        write(device,AFORMAT) '<html><head><title>'//trim(UniversityCode)//SPACE//PROGNAME//VERSION// &
            '</title></head><body><a name="TOP"></a>'

        ! banner line 1: user context & shortcuts
        write(device,AFORMAT) &
            '<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
            b_tr//'<td width="50%"><h2>'//trim(UniversityCode)//SPACE//trim(PROGNAME)//' for '// &
            trim(text_school_year(currentYear))//' / '// &
            trim(txtSemester(targetTerm+6))//trim(termQualifier(targetTerm+6))//'</h2>'
        if (present(errmsg)) then
            if (errmsg/=SPACE) write(device,AFORMAT) red//b_italic//trim(errmsg)//e_italic//e_color
        end if

        write(device,AFORMAT) e_td//'<td align="right" valign="top">'//b_small
        QUERY_put = USERNAME
        if (len_trim(ROLE)>0) then
            write(device,AFORMAT) &
                b_bold//'User is '//trim(USERNAME)//e_bold//' ('//trim(ROLE)//').'
        else if (isRoleStudent) then
            write(device,AFORMAT) &
                b_bold//'User is '//trim(USERNAME)//e_bold//' ('// &
                    trim(txtYear(Student(requestingStudent)%Enlistment(targetTerm)%levelYear+10))//FSLASH// &
                    trim(txtStatusCode(Student(requestingStudent)%Enlistment(targetTerm)%statusAdvising))//FSLASH// &
                    trim(txtStatusCode(Student(requestingStudent)%Enlistment(targetTerm)%statusEnlistment))//').'
        else
            write(device,AFORMAT) &
                b_bold//'User is '//trim(USERNAME)//e_bold//DOT
        end if

        if ((isRoleSysAd .or. isRoleOfficial) .or. isRoleGuest .or. isRoleStaff) then
        else if ( isRoleDean .or. isRoleChair .or. isRoleFaculty ) then ! primary links for entry in Teacher()

            if (Teacher(requestingTeacher)%NumAdvisees>0) write(device,AFORMAT) &
                trim(make_href(fnAdvisersByTeacher, 'Advisees', A1=QUERY_put, A9=targetTerm, pre=nbsp))

            call count_teacher_load(requestingTeacher, 0, nClasses)
            if (sum(nClasses)>0) then
                write(device,AFORMAT) nbsp//'Classes: '
                do iTerm=firstSemester,summerTerm
                    if (nClasses(iTerm)>0) write(device,AFORMAT) &
                        trim(make_href(fnTeacherClasses, txtSemester(iTerm+6), A1=QUERY_put, A9=iTerm, pre=nbsp))
                end do
                write(device,AFORMAT) nbsp//'Load form: '
                do iTerm=firstSemester,summerTerm
                    if (nClasses(iTerm)>0) write(device,AFORMAT) &
                        trim(make_href(fnPrintableWorkload, txtSemester(iTerm+6), A1=QUERY_put, A9=iTerm, pre=nbsp, &
                        newtab='"_blank"'))
                end do
            end if
            if (nClasses(currentTerm)>0 .and. &
                College(Department(Teacher(requestingTeacher)%DeptIdx)%CollegeIdx)%isAllowed(ToEvaluateTeachers,currentTerm) ) &
                write(device,AFORMAT) trim(make_href(fnEvaluationForm, 'Eval', &
                    A1=QUERY_put, A2=EvalTypeDescription(1), A9=currentTerm, pre=nbsp ))

        else if (isRoleStudent) then ! primary links for a student

            call count_preenlistment(requestingStudent, 0, nClasses, nAdvised)

            write(device,AFORMAT)  trim(make_href(fnStudentEdit, 'Info', A1=QUERY_put, pre=nbsp)), &
                trim(make_href(fnRecentStudentActivity, 'Log', A1=QUERY_put, pre=nbsp))
            if (len_trim(Student(requestingStudent)%Adviser)>0) write(device,AFORMAT) &
                trim(make_href(fnTeacherClasses, 'Adviser', A1=Student(requestingStudent)%Adviser, A9=currentTerm, pre=nbsp))
            write(device,AFORMAT) &
                trim(make_href(fnFees, 'Fees', A1=QUERY_put, pre=nbsp)), &
                trim(make_href(fnStudentGrades, 'Grades', A1=QUERY_put, pre=nbsp)), &
                trim(make_href(fnEditCheckList, 'Checklist', A1=QUERY_put, A9=currentTerm, pre=nbsp))
            do iTerm=firstSemester,summerTerm
                if (nClasses(iTerm)+nAdvised(iTerm)>0) write(device,AFORMAT) &
                    trim(make_href(fnStudentClasses, txtSemester(iTerm+6), A1=QUERY_put, A9=iTerm, pre=nbsp))
            end do

        else if (isRoleBenefactor) then
            write(device,AFORMAT) &
                trim(make_href(fnListBeneficiaries, 'Beneficiaries', A1=QUERY_put, pre=nbsp))

        end if

        ! Exit for all users
        write(device,AFORMAT) trim(make_href(fnLogout, 'Quit', pre=nbsp))

        !! emergency stop
        !if (trim(USERNAME)==trim(PROGNAME)) then
        !    write(device,AFORMAT) trim(make_href(fnStop, 'Stop', pre=nbsp))
        !end if
!
        ! end of line 1
        write(device,AFORMAT) e_small

        if (.not. isRoleBenefactor) then
            write(device,AFORMAT) linebreak//b_small//b_italic
            do tdx=1,3
                if (tdx==targetTerm) cycle
                write(device,AFORMAT) trim(make_href(fnSwitchTerm, trim(txtSemester(tdx+6))//trim(termQualifier(tdx+6)), &
                    A1=itoa(tdx), pre=nbsp))
            end do
            write(device,AFORMAT) e_italic//nbsp//' :'
            do gdx = 1,NumColleges
                if (.not. College(gdx)%hasInfo) cycle
                ! fnCollegeLinks requested already printed above?
                write(device,AFORMAT) trim(make_href(fnCollegeLinks, College(gdx)%Code, &
                    A1=College(gdx)%Code, A9=targetTerm, pre=nbsp))
            end do
            write(device,AFORMAT) e_small
        end if

        ! show student options ?
        if (targetStudent>0 .and. requestingStudent/=targetStudent .and. &
            (isRole_adviser_of_student(targetStudent,orHigherUp) .or. isRole_benefactor_of_student(targetStudent) ) ) then

            call count_preenlistment(targetStudent, 0, nClasses, nAdvised)

            tStdNo = Student(targetStudent)%StdNo
            write(device,AFORMAT) linebreak//b_small//b_bold//trim(tStdNo)//e_bold//' ('// &
                    trim(txtYear(Student(targetStudent)%Enlistment(targetTerm)%levelYear+10))//FSLASH// &
                    trim(txtStatusCode(Student(targetStudent)%Enlistment(targetTerm)%statusAdvising))//FSLASH// &
                    trim(txtStatusCode(Student(targetStudent)%Enlistment(targetTerm)%statusEnlistment))//') : '

            if (.not. isRoleBenefactor) then
                write(device,AFORMAT)  trim(make_href(fnStudentEdit, 'Info', A1=tStdNo, pre=nbsp)), &
                    trim(make_href(fnRecentStudentActivity, 'Log', A1=tStdNo, pre=nbsp))
                if (len_trim(Student(targetStudent)%Adviser)>0) write(device,AFORMAT) &
                    trim(make_href(fnTeacherClasses, 'Adviser', A1=Student(targetStudent)%Adviser, pre=nbsp))
            end if

            if (REQUEST/=fnStudentGrades) write(device,AFORMAT) &
                trim(make_href(fnStudentGrades, 'Grades', A1=tStdNo, pre=nbsp))

            if (REQUEST/=fnFees) write(device,AFORMAT) &
                trim(make_href(fnFees, 'Fees', A1=tStdNo, pre=nbsp))

            if ( isRole_admin_of_college(Curriculum(Student(targetStudent)%CurriculumIdx)%CollegeIdx) ) then
                if (REQUEST/=fnStudentGrades) then
                    write(device,AFORMAT) trim(make_href(fnTranscript, 'Transcript', A1=tStdNo, pre=nbsp, newtab='"_blank"') )
                end if
            end if

            if ( REQUEST/=fnEditCheckList) then
                write(device,AFORMAT) trim(make_href(fnEditCheckList, 'Checklist', A1=tStdNo, A9=currentTerm, pre=nbsp))
            end if

            if (.not. isRoleBenefactor) then
                if ( ( (isRoleSysAd .and. targetTerm==currentTerm) .or. &
                     College(Curriculum(Student(targetStudent)%CurriculumIdx)%CollegeIdx)%isAllowed(ToEnlistStudents,targetTerm)) &
                     .and. nAdvised(targetTerm)>0 .and. nClasses(targetTerm)==0 ) then
                    write(device,AFORMAT) trim(make_href(fnFindBlock, 'Find block', A1=tStdNo, A9=targetTerm, pre=nbsp))
                end if

            end if

            do iTerm=firstSemester,summerTerm
                if (nClasses(iTerm)+nAdvised(iTerm)>0) write(device,AFORMAT) &
                    trim(make_href(fnStudentClasses, txtSemester(iTerm+6), A1=tStdNo, A9=iTerm, pre=nbsp))
            end do

            if (isRoleSysAd) then
                write(device,AFORMAT) &
                    trim(make_href(fnSwitchUser, 'Login', A1=tStdNo, pre=nbsp, newtab='"_blank"'))
            end if

            write(device,AFORMAT) e_small

        end if

        ! a teacher ?
        if (targetTeacher>0 .and. requestingTeacher/=targetTeacher .and. &
            Teacher(targetTeacher)%Role/=STAFF .and. .not. isRoleBenefactor) then
            tTeacher = Teacher(targetTeacher)%TeacherId

            call count_teacher_load(targetTeacher, 0, nClasses)

            write(device,AFORMAT) linebreak//b_small//b_bold//trim(Teacher(targetTeacher)%Name)//e_bold//SPACE

            if ( isRole_dean_of_college(Department(Teacher(targetTeacher)%DeptIdx)%CollegeIdx,orHigherUp) ) then

                if (REQUEST/=fnEditTeacher .and. REQUEST/=fnGenerateTeacherPassword) then
                    write(device,AFORMAT) trim(make_href(fnEditTeacher, 'Info', A1=tTeacher, pre=nbsp))
                end if

                if (Teacher(targetTeacher)%NumAdvisees>0) write(device,AFORMAT) &
                    trim(make_href(fnAdvisersByTeacher, 'Advisees', A1=tTeacher, pre=nbsp))

                if (Teacher(targetTeacher)%Role==BENEFACTOR) write(device,AFORMAT) &
                    trim(make_href(fnListBeneficiaries, 'Beneficiaries', A1=tTeacher, pre=nbsp))

                write(device,AFORMAT) &
                    trim(make_href(fnRecentTeacherActivity, 'Log', A1=tTeacher, pre=nbsp))

                write(device,AFORMAT) nbsp//'Edit load:'
                do iTerm=firstSemester,summerTerm
                    write(device,AFORMAT) &
                        trim(make_href(fnTeacherEditSchedule, txtSemester(iTerm+6), A1=tTeacher, A9=iTerm, pre=nbsp ))
                end do

            end if

            if (sum(nClasses)>0) then
                write(device,AFORMAT) nbsp//'Classes: '
                do iTerm=firstSemester,summerTerm
                    if (nClasses(iTerm)>0) write(device,AFORMAT) &
                        trim(make_href(fnTeacherClasses, txtSemester(iTerm+6), A1=tTeacher, A9=iTerm, pre=nbsp))
                end do
                write(device,AFORMAT) nbsp//'Load form: '
                do iTerm=firstSemester,summerTerm
                    if (nClasses(iTerm)>0) write(device,AFORMAT) &
                        trim(make_href(fnPrintableWorkload, txtSemester(iTerm+6), A1=tTeacher, A9=iTerm, pre=nbsp, &
                        newtab='"_blank"'))
                end do
                if (isRole_dean_of_college(targetCollege, orHigherUp) .or. trim(USERNAME)==trim(tTeacher)) then
                    write(device,AFORMAT) nbsp//'Evaluation: '
                    do iTerm=firstSemester,summerTerm
                        if (nClasses(iTerm)>0) write(device,AFORMAT) &
                            trim(make_href(fnEvaluationRatings, txtSemester(iTerm+6), A1=tTeacher, A2='Teacher', &
                            A9=iTerm, pre=nbsp ))
                    end do
                    if ( isRole_dean_of_college(Department(Teacher(targetTeacher)%DeptIdx)%CollegeIdx,orHigherUp) .and. &
                         REQUEST/=fnEvaluationDuties ) then
                        write(device,AFORMAT) &
                            trim(make_href(fnEvaluationDuties, 'Duties', A1=tTeacher, A9=currentTerm))
                    end if
                end if

            end if

            if (isRoleSysAd) then
                write(device,AFORMAT) &
                    trim(make_href(fnSwitchUser, 'Login', A1=tTeacher, pre=nbsp, newtab='"_blank"'))
            end if

            write(device,AFORMAT) e_small
        end if

        ! show unit options ?
        if (targetDepartment>1 .and. REQUEST/=fnLogin .and. .not. isRoleBenefactor) then
            tDepartment = Department(targetDepartment)%Code
            write(device,AFORMAT) linebreak//b_small//b_bold//trim(tDepartment)//e_bold, &
                trim(make_href(fnTeachersByDept, 'Teachers', A1=tDepartment, pre=nbsp)), &
                trim(make_href(fnRoomList, 'Rooms', A1=tDepartment, pre=nbsp)), &
                trim(make_href(fnScheduleOfClasses, 'Classes', A1=tDepartment, pre=nbsp)), &
                e_small
        end if

        if ( REQUEST==fnEditCheckList) then
            call checklist_links(device, .true.)
        end if

        write(device,AFORMAT) e_td//e_tr, &
            e_table//horizontal

        if (isMirror) then
            write(device,AFORMAT) b_bold//red, &
                'You are logged in to the '//trim(UniversityCode)//SPACE//PROGNAME//' mirror. ', &
                'Information here was updated '//itoa(secsLastRefresh/60)//' minute(s) ago; the next update is approximately ', &
                itoa(max(secsNextRefresh,1))//' seconds(s) from now if there are active users.', &
                e_color//e_bold//horizontal
        end if

        ! emergency message
        if (len_trim(EMERGENCY)>0) write(device,AFORMAT) trim(EMERGENCY), horizontal

        ! start of body
        if (len_trim(header)>0) write(device,AFORMAT) '<h3>'//trim(header)//'</h3>'

    end subroutine html_write_header



    subroutine links_to_blocks(device, iColl, thisTerm)
        integer, intent (in) :: device, iColl, thisTerm
        integer :: iCurr, ldx, iBlk!, nBlocks

        write(device,AFORMAT) b_item//b_bold//'Blocks'//e_bold//' :'
        done = .false.
        do iCurr=1,NumCurricula-1
            if (Curriculum(iCurr)%CollegeIdx /= iColl) cycle
            if (done(iCurr)) cycle
            if (Curriculum(iCurr)%NumTerms==0) cycle
            ldx = 0
            do iBlk=1,NumBlocks(thisTerm)
                if (CurrProgCode(Block(thisTerm,iBlk)%CurriculumIdx)/=CurrProgCode(iCurr)) cycle
                ldx = ldx+1
            end do
            write(device,AFORMAT) trim(make_href(fnBlockList, CurrProgCode(iCurr), &
                A1=CurrProgCode(iCurr), A9=thisTerm, pre=nbsp, post='('//trim(itoa(ldx))//')'))
            do ldx=iCurr+1,NumCurricula-1
                if (CurrProgCode(ldx) == CurrProgCode(iCurr)) done(ldx) = .true.
            end do
        end do
        write(device,AFORMAT) e_item

    end subroutine links_to_blocks


    subroutine links_to_depts(device, iColl, fn, thisTerm, header, footer)
        integer, intent (in) :: device, iColl, fn, thisTerm
        character(len=*), intent (in) :: header
        character(len=*), intent (in), optional :: footer
        integer :: dept, iSubj, n_count
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        call html_comment('links_to_depts()')

        write(device,AFORMAT) b_item//header//' : '
        do dept=2,NumDepartments
            if (Department(dept)%CollegeIdx/=iColl) cycle
            n_count = 0
#if defined UPLB
            do iSubj=1,NumSubjects+NumAdditionalSubjects
                if (Subject(iSubj)%DeptIdx /= dept) cycle
                n_count = n_count+1
                exit
            end do
#else
            ! Subjects administered by program
            do iSubj=1,NumSubjects+NumAdditionalSubjects
                if (.not. isSubject_used_in_college(iColl, iSubj)) cycle
                n_count = n_count+1
                exit
            end do
#endif
            if (n_count==0) cycle
            tDepartment = Department(dept)%Code
            write(device,AFORMAT) trim(make_href(fn, tDepartment, A1=tDepartment, A9=thisTerm, post=nbsp))
        end do
        if (present(footer)) then
            if (len_trim(footer)>0) write(device,AFORMAT) nbsp//footer
        end if
        write(device,AFORMAT) e_item

    end subroutine links_to_depts


    subroutine links_to_sections(device, iColl, numAreas, AreaList, thisTerm, AreaCount)
        integer, intent (in) :: device, iColl, numAreas, thisTerm
        integer, intent (in) :: AreaList(1:numAreas)
        integer, intent (out) :: AreaCount(1:numAreas)
        integer :: ddx, iSect, n_count, m_count, mdx, k1, k2, idxArea
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        tCollege = College(iColl)%Code
        AreaCount(1:numAreas) = 0

#if defined UPLB
        ddx = 0
#else
        tDepartment = tCollege
        ddx = index_to_dept(tDepartment)
#endif

#if defined UPLB
        write(device,AFORMAT) b_item//b_bold//'Classes'//e_bold//' in : '
        do idxArea=2,NumDepartments
            if (Department(idxArea)%CollegeIdx /= iColl) cycle
            n_count = 0
            do k1=1,NumSubjects+NumAdditionalSubjects
                if (Subject(k1)%DeptIdx /= idxArea) cycle
                n_count = n_count+1
                exit
            end do
            if (n_count==0) cycle ! no subjects in this unit

            ! how many sections currently open
            n_count = 0
            do iSect=1,NumSections(thisTerm)
                if (idxArea/=Section(thisTerm,iSect)%DeptIdx) cycle ! not in unit
                if (Section(thisTerm,iSect)%SubjectIdx==0) cycle ! deleted
                n_count = n_count+1
            end do
            tDepartment = Department(idxArea)%Code
            write(device,AFORMAT) trim(make_href(fnScheduleOfClasses, tDepartment, &
                A1=tDepartment, A9=thisTerm, post='('//trim(itoa(n_count))//')'//nbsp))
        end do

        write(device,AFORMAT) e_item
#endif

        ! count how many sections per subject area
        do idxArea=1,numAreas
            ! the code
            tSubject = SubjectArea(AreaList(idxArea))%Code
            k1 = len_trim(tSubject)+1
            n_count = 0 ! how many sections with this code currently open
            do iSect=1,NumSections(thisTerm)
#if defined UPLB
                if (Section(thisTerm,iSect)%ClassId(:k1)==tSubject(:k1)) n_count = n_count+1
#else
                if (Section(thisTerm,iSect)%ClassId(:k1)==tSubject(:k1) .and. &
                    ddx==Section(thisTerm,iSect)%DeptIdx) n_count = n_count+1
#endif
            end do
            AreaCount(idxArea) = n_count ! number of sections in area
        end do
        write(device,AFORMAT) b_item//b_bold//'Classes by subject area'//e_bold//' : '

        if (sum(AreaCount(1:numAreas))==0) then ! no classes in this college
            write(device,AFORMAT) trim(make_href(fnScheduleCopyLastYear, 'classes ONLY', &
                A1=tCollege, A2='CLASSES', A9=thisTerm, pre='None; copy '//trim(tCollege)//nbsp, &
                post=nbsp//' from '//trim(text_term_school_year(thisTerm+6,cTm3Year))//COMMA )), &
                trim(make_href(fnScheduleCopyLastYear, 'classes AND blocks', &
                A1=tCollege, A2='BLOCKS', A9=thisTerm, pre='or, copy'//nbsp, post=DOT )), &
                e_item
            return
        end if

        do idxArea=1,numAreas
            n_count = AreaCount(idxArea) ! number of sections in area
            if (n_count==0) cycle ! skip area if none
            ! the code
            tSubject = SubjectArea(AreaList(idxArea))%Code
            write(device,AFORMAT) trim(make_href(fnScheduleByArea, tSubject, &
                A1=tCollege, A2=tSubject, A9=thisTerm, post='('//trim(itoa(n_count))//')'//nbsp))
        end do
        write(device,AFORMAT) e_item

        write(device,AFORMAT) b_item//b_bold//'Classes with '//e_bold
        n_count = 0 ! how many sections with TBA teachers
        m_count = 0 ! how many sections with TBA rooms
        do iSect=1,NumSections(thisTerm)
            if ( iColl/=Department(Section(thisTerm,iSect)%DeptIdx)%CollegeIdx ) cycle
            k1 = 0
            k2 = 0
            do mdx=1,Section(thisTerm,iSect)%NMeets
                if (Section(thisTerm,iSect)%TeacherIdx(mdx)==0) k1 = k1+1
                if (Section(thisTerm,iSect)%RoomIdx(mdx)==0) k2 = k2+1
            end do
            if (k1>0) n_count = n_count+1
            if (k2>0) m_count = m_count+1
        end do
        write(device,AFORMAT) trim(make_href(fnTBATeachers, 'TBA teachers', A1=tCollege, A9=thisTerm, &
                pre=nbsp, post='('//trim(itoa(n_count))//'), ')), &
            trim(make_href(fnTBARooms, 'TBA rooms', A1=tCollege, A9=thisTerm, &
                pre=nbsp, post='('//trim(itoa(m_count))//'). ')), &
            b_bold//'Conflicts in schedules of '//e_bold, &
                trim(make_href(fnTeacherConflicts, 'teachers', A1=tCollege, A9=thisTerm, pre=nbsp, post=COMMA)), &
                trim(make_href(fnRoomConflicts, 'rooms', A1=tCollege, A9=thisTerm, pre=nbsp, post=COMMA)), &
                trim(make_href(fnBlockConflicts, 'blocks', A1=tCollege, A9=thisTerm, pre=nbsp, post=DOT)), &
            e_item ! , linebreak

    end subroutine links_to_sections


    subroutine links_to_subjects(device, iColl, numAreas, AreaList)
        integer, intent (in) :: device, iColl, numAreas
        integer, intent (in) :: AreaList(1:numAreas)
        integer :: dept
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
#if defined UPLB
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer :: iSubj, n_count
#endif
        if (numAreas==0) return

        call html_comment('links_to_subjects('//College(iColl)%Code//')')

#if defined UPLB
        write(device,AFORMAT) b_item//b_bold//'Subjects'//e_bold//' in :'
        do dept=2,NumDepartments
            if (Department(dept)%CollegeIdx /= iColl) cycle
            n_count = 0
            do iSubj=1,NumSubjects+NumAdditionalSubjects
                if (Subject(iSubj)%DeptIdx /= dept) cycle
                n_count = n_count+1
            end do
            if (n_count==0) cycle
            tDepartment = Department(dept)%Code
            write(device,AFORMAT) trim(make_href(fnSubjectList, tDepartment, A1=tDepartment, &
                pre=nbsp, post='('//trim(itoa(n_count))//')'))
        end do
        write(device,AFORMAT) e_item
#endif
        write(device,AFORMAT) b_item//b_bold//'Subjects'//e_bold//' by area :'
        do dept=1,numAreas
            tSubject = SubjectArea(AreaList(dept))%Code
            !n_count = SubjectArea(AreaList(dept))%Count
            !write(device,AFORMAT) trim(make_href(fnSubjectList, tSubject, A1=tSubject, &
            !    pre=nbsp, post='('//trim(itoa(n_count))//')'))
            write(device,AFORMAT) trim(make_href(fnSubjectList, tSubject, A1=tSubject, &
                A2=College(iColl)%Code, pre=nbsp))
        end do
        write(device,AFORMAT) e_item

    end subroutine links_to_subjects


    subroutine list_sections_to_edit(device, thisTerm, lenSL, SectionList, &
            target_fn, target_name, target_action, permitted, allowed_to_show, heading)!, noTitle)

        integer, intent(in) :: device, thisTerm, lenSL, SectionList(3*lenSL+3), target_fn
        character(len=*), intent(in) :: target_name, target_action
        logical, intent(in) :: permitted, allowed_to_show
        character (len=*), intent(in), optional :: heading
        !logical, intent(in), optional :: noTitle

        integer :: iBlk, iSubj, idx, mdx, rdx, sdx, tdx, previous, conflict, dept, colorIdx
        character (len=10) :: note, classType
        character (len=MAX_LEN_BLOCK_CODE) :: blkCode
        logical :: countUnits, sectionDone, isRegularSchedule, isLectureClass, evaluationLink
        real :: totalUnits, meetingUnits, totalHours, meetingHours
        type (TYPE_SECTION) :: tSection

        call html_comment('list_sections_to_edit('//itoa(lenSL/3)//')')

        evaluationLink = .false.
        if (present(heading)) then
            write(device,AFORMAT) heading
            evaluationLink = currentTerm==thisTerm .and. index(heading,'Enlisted subjects')>0 .and. isRoleStudent
        end if
        if (lenSL < 3) then
            write(device,AFORMAT) BRNONE
            return
        end if

        countUnits = target_action=='Del' .and. target_fn/=fnRoomSchedule
        totalUnits = 0.0
        totalHours = 0.0

        if (countUnits) then
            write(device,AFORMAT) '<table border="0" width="100%">'//b_tr, &
                b_thal//'Subject'//e_th// &
                b_thal//'Section'//e_th// &
                b_thal//'Units'//e_th// &
                b_thal//'Hours'//e_th// &
                b_thal//'Day'//e_th// &
                b_thal//'Time'//e_th// &
                b_thal//'Room'//e_th// &
                b_thal//'Teacher'//e_th
        else
            write(device,AFORMAT) '<table border="0" width="100%">'//b_tr, &
                b_thal//'Subject'//e_th// &
                b_thal//'Section'//e_th//&
                b_thal//'Seats'//e_th// &
                b_thal//'Day'//e_th//&
                b_thal//'Time'//e_th// &
                b_thal//'Room'//e_th//&
                b_thal//'Teacher'//e_th
        end if
        if (target_fn>0 .and. permitted) then
            write(device,AFORMAT) b_thal//b_small//'Action'//e_small//e_th//e_tr
        else
            write(device,AFORMAT) b_td_nbsp_e_td//e_tr
        end if

        previous = 0
        colorIdx = 0
        sectionDone = .false.
        do idx=1,lenSL,3
            sdx = SectionList(idx)
            mdx = SectionList(idx+1)
            conflict = SectionList(idx+2)
            iSubj = Section(thisTerm,sdx)%SubjectIdx
            dept = Section(thisTerm,sdx)%DeptIdx
            QUERY_put = Section(thisTerm,sdx)%ClassId

            tSection = Section(thisTerm,sdx)
            isRegularSchedule = is_regular_schedule(sdx, thisTerm)
            isLectureClass = is_lecture_class(sdx, thisTerm)

            if (conflict>=0) then
                note = SPACE
            else ! negative conflict is the undesirablity index
                note = ' ('//trim(itoa(-conflict))//')'
            end if
            !new section ?
            if (sdx/=previous) then ! include subject, section, units/blockname, seats/hours, time, day

                colorIdx = colorIdx+1
                sectionDone = .false.
                if (isSubject_lecture_lab(iSubj)) then
                    if (isLectureClass) then ! lecture of lecture-lab
                        meetingUnits = Subject(iSubj)%Units
                        meetingHours = Subject(iSubj)%LectHours
                    else ! lab of lecture-lab
                        meetingUnits = 0.0
                        meetingHours = Subject(iSubj)%LabHours
                    end if
                else if (Subject(iSubj)%LectHours>0.0) then ! lecture-only
                    meetingUnits = Subject(iSubj)%Units
                    meetingHours = Subject(iSubj)%LectHours
                else if (Subject(iSubj)%LabHours>0.0) then ! lab-only
                    meetingUnits = Subject(iSubj)%Units
                    meetingHours = Subject(iSubj)%LabHours
                end if

                totalHours = totalHours + meetingHours
                totalUnits = totalUnits + meetingUnits

                previous = sdx
                iBlk = find_section_in_blocks(tSection%ClassId, thisTerm)
                if (iBlk>0) then
                    blkCode = FSLASH//Block(thisTerm, iBlk)%BlockID
                else
                    blkCode = SPACE
                end if

                if ( permitted .and. (REQUEST==fnBlockEditSection .or. &
                        REQUEST==fnBlockEditSubject .or. &
                        REQUEST==fnBlockEditName .or. &
                        REQUEST==fnBlockCopy .or. &
                        REQUEST==fnBlockSchedule)) then ! link section code to edit section

                    write(device,AFORMAT) &
                        trim(make_href(fnScheduleByArea, Subject(iSubj)%Name, &
                            A1=Department(dept)%Code, pre='<tr bgcolor="'//bgcolor(mod(colorIdx,2))//'">'// &
                            b_td, post=e_td, A2=Subject(iSubj)%Name, A9=thisTerm)), &
                        trim(make_href(fnScheduleEdit, trim(tSection%Code)//blkCode, &
                            A1=QUERY_put, A9=thisTerm, pre=b_td, post=e_td))
                else
                    if (isSubject_lecture_lab(iSubj)) then
                        if (isLectureClass) then
                            classType = ' (Lect)'
                        else
                            classType = ' (Lab)'
                        end if
                    else
                        classType = ' - '
                    end if

                    write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(colorIdx,2))//'">'// & ! subject code + title
                        '<td width="30%">'// &
                            b_bold//trim(Subject(iSubj)%Name)//e_bold//trim(classType)//SPACE// &
                            b_italic//b_small//trim(Subject(iSubj)%Title)//e_small//e_italic//e_td, &
                        b_td//trim(tSection%Code)//trim(blkCode)//e_td ! section code
                end if

                if (countUnits) then
                    if (meetingUnits>0.0) then
                        write(device,AFORMAT) b_td//trim(ftoa(meetingUnits,1))//e_td
                    else
                        write(device,AFORMAT) b_td_nbsp_e_td ! units
                    end if
                    if (meetingHours>0.0) then
                        write(device,AFORMAT) b_td//trim(ftoa(meetingHours,2))//e_td ! hours
                    else
                        write(device,AFORMAT) b_td_nbsp_e_td ! hours
                    end if
                else
                    write(device,AFORMAT)  &
                        !b_td_nbsp_e_td// & ! BlockID
                        b_td//trim(itoa(tSection%Slots))//e_td!// & ! seats
                end if

                if (isRegularSchedule) then
                    sectionDone = .true.

                    ! time, day
                    write(device,AFORMAT) b_td//text_days_of_section(tSection)//e_td// &
                        b_td//trim(text_time_period(tSection%bTimeIdx(mdx), tSection%eTimeIdx(mdx)))//e_td

                    ! room
                    rdx = tSection%RoomIdx(mdx)
                    if (rdx > 0) then
                        write(device,AFORMAT) b_td//trim(Room(rdx)%Code)//e_td
                    else
                        write(device,AFORMAT) b_td//'TBA'//e_td
                    end if

                    ! teacher
                    tdx = tSection%TeacherIdx(mdx)
                    if (tdx > 0) then
                        if (allowed_to_show) then
                            write(device,AFORMAT) b_td//trim(Teacher(tdx)%Name)
                            ! evaluate link
                            if (evaluationLink .and. &
                                College(Department(Teacher(tdx)%DeptIdx)%CollegeIdx)%isAllowed(ToEvaluateTeachers,thisTerm)) then
                                write(device,AFORMAT) trim(make_href(fnEvaluationForm, 'Eval', &
                                    A1=Teacher(tdx)%TeacherId, A2=EvalTypeDescription(4), A3=QUERY_put, A9=thisTerm, &
                                    pre=nbsp//b_small//b_italic, post=e_italic//e_small))
                            end if
                            write(device,AFORMAT) e_td
                        else
                            write(device,AFORMAT) b_td//b_italic//'(hidden)'//e_italic//e_td
                        end if
                    else
                        write(device,AFORMAT) b_td//'TBA'//e_td
                    end if

                    if (target_fn==fnRoomSchedule .or. target_fn==fnTeacherEditSchedule) then
                        if ( permitted ) then
                            write(device,AFORMAT) trim(make_href(target_fn, target_action, &
                                A1=target_name, A2=target_action, A3=QUERY_put, A9=thisTerm, &
                                pre=b_td//b_small, post=e_small//e_td//e_tr))
                        else
                            write(device,AFORMAT) b_td_nbsp_e_td//e_tr
                        end if
                    elseif (target_fn==fnTeacherClasses) then
                        if ( permitted .and. tdx>0) then
                            write(device,AFORMAT) trim(make_href(target_fn, target_action, &
                                A1=Teacher(tdx)%TeacherId, A9=thisTerm, &
                                pre=b_td//b_small, post=e_small//e_td//e_tr))
                        else
                            write(device,AFORMAT) b_td_nbsp_e_td//e_tr
                        end if
                    elseif (target_fn==fnChangeMatriculation .or. target_fn==fnBlockEditSection) then
                        if ( permitted ) then
                            ! operate on lab classes, not lecture classes
                            if (isSubject_lecture_lab(tSection%SubjectIdx) .and. isLectureClass) then
                                write(device,AFORMAT) b_td_nbsp_e_td//e_tr
                            else
                                write(device,AFORMAT) trim(make_href(target_fn, target_action, &
                                    A1=target_name, A2=target_action, A3=QUERY_put, A9=thisTerm, &
                                    pre=b_td//b_small, post=e_small//trim(note)//e_td//e_tr))
                            end if
                        else
                            write(device,AFORMAT) b_td_nbsp_e_td//e_tr
                        end if
                    end if
                    if (conflict>0) write(device,AFORMAT) &
                        b_tr//'<td align="center" colspan="8">'//red//'CONFLICT between '//trim(tSection%ClassId)// &
                        ' and '//trim(Section(thisTerm,conflict)%ClassId)//e_color//e_td//e_tr

                    cycle

                else
                    write(device,AFORMAT)  &
                        b_td//txtDay(tSection%DayIdx(mdx))//e_td// &
                        b_td//trim(text_time_period(tSection%bTimeIdx(mdx), tSection%eTimeIdx(mdx)))//e_td

                    ! force cycle for next in list if not shown
                    !sectionDone = .not. allowed_to_show
                end if

            else ! time, day only

                if (sectionDone) then ! conflict ?
                    if (conflict>0) write(device,AFORMAT) &
                        b_tr//'<td align="center" colspan="8">'//red//'CONFLICT between '//trim(tSection%ClassId)// &
                        ' and '//trim(Section(thisTerm,conflict)%ClassId)//e_color//e_td//e_tr
                    cycle
                end if

                write(device,AFORMAT) &
                    b_td//txtDay(tSection%DayIdx(mdx))//e_td// &
                    b_td//trim(text_time_period(tSection%bTimeIdx(mdx), tSection%eTimeIdx(mdx)))//e_td
            end if

            ! room
            rdx = tSection%RoomIdx(mdx)
            if (rdx > 0) then
                write(device,AFORMAT) b_td//trim(Room(rdx)%Code)//e_td
            else
                write(device,AFORMAT) b_td//'TBA'//e_td
            end if

            ! teacher
            tdx = tSection%TeacherIdx(mdx)
            if (tdx > 0) then
                if (allowed_to_show) then
                    write(device,AFORMAT) b_td//trim(Teacher(tdx)%Name)
                    if (evaluationLink .and. &
                        College(Department(Teacher(tdx)%DeptIdx)%CollegeIdx)%isAllowed(ToEvaluateTeachers,thisTerm)) then
                        write(device,AFORMAT) trim(make_href(fnEvaluationForm, 'Eval', &
                            A1=Teacher(tdx)%TeacherId, A2=EvalTypeDescription(4), A3=QUERY_put, A9=thisTerm, &
                            pre=nbsp//b_small//b_italic, post=e_italic//e_small))
                    end if
                    write(device,AFORMAT) e_td
                else
                    write(device,AFORMAT) b_td//b_italic//'(hidden)'//e_italic//e_td
                end if
            else
                write(device,AFORMAT) b_td//'TBA'//e_td
            end if

            if (sdx==SectionList(idx+3)) then ! NOT the last meeting; add SPACE for next line
                write(device,AFORMAT) b_td//e_td//e_tr
                if (conflict>0) write(device,AFORMAT) &
                    b_tr//'<td align="center" colspan="8">'//red//'CONFLICT between '//trim(tSection%ClassId)//' and '// &
                    trim(Section(thisTerm,conflict)%ClassId)//e_color//e_td//e_tr
                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(colorIdx,2))//'">'// &
                    b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td
                if (countUnits) write(device,AFORMAT) b_td_nbsp_e_td
            else
                if (target_fn==fnRoomSchedule .or. target_fn==fnTeacherEditSchedule) then
                    if ( permitted ) then
                        write(device,AFORMAT) trim(make_href(target_fn, target_action, &
                            A1=target_name, A2=target_action, A3=QUERY_put, A9=thisTerm, &
                            pre=b_td//b_small, post=e_small//e_td//e_tr))
                    else
                        write(device,AFORMAT) b_td_nbsp_e_td//e_tr
                    end if
                elseif (target_fn==fnChangeMatriculation .or. target_fn==fnBlockEditSection) then
                    if (permitted ) then
                        ! operate on lab classes, not lecture classes
                        if (isSubject_lecture_lab(tSection%SubjectIdx) .and. isLectureClass) then
                            write(device,AFORMAT) b_td_nbsp_e_td//e_tr
                        else
                            write(device,AFORMAT) trim(make_href(target_fn, target_action, &
                                A1=target_name, A2=target_action, A3=QUERY_put, A9=thisTerm, &
                                pre=b_td//b_small, post=e_small//trim(note)//e_td//e_tr))
                        end if
                    else
                        write(device,AFORMAT) b_td_nbsp_e_td//e_tr
                    end if
                end if
                if (conflict>0) write(device,AFORMAT) &
                    b_tr//'<td align="center" colspan="8">'//red//'CONFLICT between '//trim(tSection%ClassId)//' and '// &
                    trim(Section(thisTerm,conflict)%ClassId)//e_color//e_td//e_tr
            end if
        end do
        if (countUnits) then
            write(device,AFORMAT) b_tr//'<td colspan="9">'//horizontal//e_td//e_tr, &
                b_tr//b_td_nbsp_e_td//b_td//b_bold//'Totals'//e_bold//' : '//e_td// & ! code
                b_td//trim(ftoa(totalUnits,2))//e_td//b_td//trim(ftoa(totalHours,2))//e_td// & ! hours
                b_td_nbsp_e_td// b_td_nbsp_e_td// b_td_nbsp_e_td// b_td_nbsp_e_td// b_td_nbsp_e_td//e_tr, &
                b_tr//'<td colspan="9">'//horizontal//e_td//e_tr
        end if
        write(device,AFORMAT) e_table

    end subroutine list_sections_to_edit


    subroutine list_students(device, thisTerm, n_count, sList, iSubj)
        integer, intent (in) :: device, thisTerm, n_count, sList(n_Count), iSubj
        integer :: iCurr, tdx, iStd, ncol, iTeach, nClasses(3), nAdvised(3)
        character (len=MAX_LEN_SUBJECT_CODE) :: tNum
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo

        call html_comment('list_students()')

        if (n_count == 0) then
            write(device,AFORMAT) BRNONE

        else
            write(device,AFORMAT) '<table border="0" width="100%">', &
                b_tr//b_thal//'#'//e_th// &
                b_thal//'CONTRIB'//e_th// &
                b_thal//'STDNO'//e_th// &
                b_thal//'NAME OF STUDENT'//e_th// &
                b_thal//'PROGRAM'//e_th// &
                b_td_nbsp_e_td// &
                e_tr
            do tdx=1,n_count
                iStd = sList(tdx)
                tStdNo = Student(iStd)%StdNo
                iCurr = Student(iStd)%CurriculumIdx
                iTeach = index_to_teacher(Student(iStd)%Adviser)

                call count_preenlistment(iStd, 0, nClasses, nAdvised)

                ! find contribution
                do ncol=1,Student(iStd)%Enlistment(thisTerm)%NPriority+Student(iStd)%Enlistment(thisTerm)%NAlternates + &
                        Student(iStd)%Enlistment(thisTerm)%NCurrent
                    if (iSubj==Student(iStd)%Enlistment(thisTerm)%Subject(ncol)) exit
                end do
                write(tNum, '(f6.4)') Student(iStd)%Enlistment(thisTerm)%Contrib(ncol)

                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(tdx,2))//'">'// & ! b_tr// &
                    b_tdac//trim(itoa(tdx))//DOT//e_td// &
                    b_td//trim(tNum)//e_td// &
                    b_td//tStdNo//e_td// &
                    b_td//trim(Student(iStd)%Name)//e_td// &
                    b_td//trim(Curriculum(iCurr)%Code)//e_td

                if ( isRole_adviser_of_student(iStd,orHigherUp) ) then

                    write(device,AFORMAT) b_td//b_small
                    if (isRoleSysAd .or. isRoleOfficial .or. isRoleDean .or. isRoleStaff) then
                        write(device,AFORMAT) &
                            trim(make_href(fnStudentEdit, 'Info', A1=tStdNo, A9=thisTerm, pre=nbsp)), &
                            trim(make_href(fnRecentStudentActivity, 'Log', A1=tStdNo, A9=thisTerm, pre=nbsp))
                    end if

                    write(device,AFORMAT)  &
                        trim(make_href(fnTeacherClasses, 'Adviser', A1=Teacher(iTeach)%TeacherId, A9=currentTerm, pre=nbsp)), &
                        trim(make_href(fnStudentGrades, 'Grades', A1=tStdNo, A9=thisTerm, pre=nbsp)), &
                        trim(make_href(fnFees, 'Fees', A1=tStdNo, pre=nbsp)), &
                        trim(make_href(fnEditCheckList, 'Checklist', A1=tStdNo, A9=currentTerm, pre=nbsp))

                    do ncol=firstSemester,summerTerm
                        if (nClasses(ncol)+nAdvised(ncol)>0) write(device,AFORMAT) &
                            trim(make_href(fnStudentClasses, txtSemester(ncol+6), A1=tStdNo, A9=ncol, pre=nbsp))
                    end do

                end if
                write(device,AFORMAT) e_small//e_td//e_tr

            end do
            write(device,AFORMAT) e_table
        end if

    end subroutine list_students


    subroutine student_search_info(iStd, searchString, thisTerm, wrkStart, location)
        integer, intent(in) :: iStd, thisTerm, wrkStart
        character(len=*), intent(in) :: searchString
        character(len=80), intent(out) :: location

        integer :: mdx, sdx, tdx, tLen1, idx
        integer, dimension(60,7) :: TimeTable
        logical :: conflicted, match

        location = SPACE
        call timetable_meetings_of_student(thisTerm, iStd, 0, tLen1, tArray(wrkStart+1:), TimeTable, conflicted)
        do tdx=1,tLen1,3
            sdx = tArray(wrkStart+tdx)
            match = .false.
            if (index(Section(thisTerm,sdx)%ClassId,searchString)/=0) then
                location = 'Class : '//location
                match = .true.
                exit
            else
                do mdx=1,Section(thisTerm,sdx)%NMeets
                    idx = Section(thisTerm,sdx)%TeacherIdx(mdx)
                    if (index(Teacher(idx)%TeacherId,searchString)+index(Teacher(idx)%Name,searchString)>0) then
                        location = 'Teacher : '//location
                        match = .true.
                    end if
                    idx = Section(thisTerm,sdx)%RoomIdx(mdx)
                    if (index(Room(idx)%Code,searchString)>0) then
                        location = 'Classroom : '//location
                        match = .true.
                    end if
                    if (match) exit
                end do
            end if
            if (match) exit
        end do

        if (index(Student(iStd)%Scholarship,searchString)>0) location = 'Scholarship : '//location
        tdx = index_to_teacher(Student(iStd)%Adviser)
        if (index(Student(iStd)%Adviser,searchString)+index(Teacher(tdx)%Name,searchString)>0) location = 'Adviser : '//location
        if (index(Curriculum(Student(iStd)%CurriculumIdx)%Code,searchString)>0) location = 'Curriculum : '//location
        if (index(Student(iStd)%Name,searchString)>0) location = 'Name : '//location
        if (index(Student(iStd)%StdNo,searchString)>0) location = 'StdNo : '//location

    end subroutine student_search_info


    subroutine timetable_display(device, thisTerm, TimeTable, allowed_to_show)
        integer, intent(in) :: device, thisTerm, TimeTable(60,7)
        logical, intent (in), optional :: allowed_to_show
        integer, parameter :: period = 2 ! no. of 15 minute intervals
        integer :: i, color, ncol, iSect, j, mcol, minTime, maxTime
        character (len=1024) :: line
        integer :: colorIdx(60,7)
        logical :: doNotShow

        call html_comment('timetable_display()')
        if (present(allowed_to_show)) then
            doNotShow = .not. allowed_to_show
        else
            doNotShow = .false.
        end if

        minTime = 56
        maxTime = 1
        ! background colors
        colorIdx = 0
        color = 0
        do ncol=1,56,period
            do i=7,1,-1
                if (TimeTable(ncol,i)==0) cycle ! no class at this time
                if (ncol<minTime) minTime = ncol
                if (ncol>maxTime) maxTime = ncol
                if (colorIdx(ncol,i)/=0) cycle ! already has a color
                ! section not colored yet
                iSect = TimeTable(ncol,i)
                color = color + 1
                do mcol=ncol,56,period
                    do j=7,1,-1
                        if (iSect==TimeTable(mcol,j)) colorIdx(mcol,j) = mod(color,15)
                    end do
                end do
            end do
        end do

        write(device,AFORMAT) linebreak//b_bold//'Weekly Timetable'//e_bold
        if (doNotShow) then
        !    write(device,AFORMAT) linebreak, trim(sorryMessageSchedules)
            return
        end if

        write(device,AFORMAT) &
            '<table border="1" width="100%">'//b_small, &
            b_tr//b_th//'Time'//e_th, &
            b_thac//'Monday'//e_th//b_thac//'Tuesday'//e_th//b_thac//'Wednesday'//e_th//&
            b_thac//'Thursday'//e_th//b_thac//'Friday'//e_th// &
            b_thac//'Saturday'//e_th//b_thac//'Sunday'//e_th// &
            e_tr
        do ncol=minTime,maxTime,period ! 1,56,period
            line = SPACE
            do i=7,1,-1
                iSect = TimeTable(ncol,i)
                if (iSect==0) then
                    line = b_td_nbsp_e_td//line
                elseif (iSect<0) then
                    if (doNotShow) then
                        line = '<td align="center" bgcolor="'//green//'">'//DOT//e_td//line
                    else
                        line = b_tdac//green//b_bold//b_italic//'proposed'//e_italic//e_bold//e_color//e_td//line
                    end if
                else
                    if (doNotShow) then
                        line = '<td align="center" bgcolor="'//bgcolor(colorIdx(ncol,i))//'">'//DOT//e_td//line
                    else
                        line = '<td align="center" bgcolor="'//bgcolor(colorIdx(ncol,i))//'">'// &
                            trim(Section(thisTerm,iSect)%ClassId)//e_td//line
                    end if
                end if
            end do
            line = '<td width="100">'//trim(text_time_period(ncol,ncol+period))//e_td//line
            write (device,AFORMAT) b_tr//trim(line)//e_tr
        end do
        write(device,AFORMAT) e_small//e_table

    end subroutine timetable_display

#include "Distributions.F90"

end module HTML

