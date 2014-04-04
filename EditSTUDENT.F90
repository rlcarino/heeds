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


module EditSTUDENT

    use ADVISING

    implicit none

    character(len=MAX_LEN_STUDENT_CODE), private :: tStdNo
    character(len=MAX_LEN_SUBJECT_CODE), private :: tSubject
    character (len=MAX_LEN_XML_LINE), private :: line

contains


    subroutine change_current_student_password(device)
        integer, intent(in) :: device

        character (len=MAX_LEN_PASSWD_VAR) :: t0Password, t1Password, t2Password
        integer :: ierr
        logical :: flagIsUp

        call html_comment('change_current_student_password()')

        flagIsUp = .false.
        if (REQUEST==fnChangeStudentPassword) then
            call cgi_get_named_string(QUERY_STRING, 'C', t0Password, ierr)
            if ( len_trim(t0Password)>0 ) then
                t0Password(17:) = SPACE
                if (is_student_password(requestingStudent,t0Password) ) then
                    loginCheckMessage = 'Change current password.'
                else
                    loginCheckMessage = 'Current password is incorrect.'
                    flagIsUp = .true.
                end if
            else
                loginCheckMessage = ''
                flagIsUp = .true.
            end if
        end if
        if (.not. flagIsUp) then
            call cgi_get_named_string(QUERY_STRING, 'P', t1Password, ierr)
            call cgi_get_named_string(QUERY_STRING, 'R', t2Password, ierr)
            if ( len_trim(t1Password)>0 .and. len_trim(t2Password)>0 ) then
                if ( t1Password==t2Password ) then
                    t1Password(17:) = SPACE
                    if (is_student_password(requestingStudent, t1Password) .or. &
                            Student(requestingStudent)%StdNo==t2Password) then
                        loginCheckMessage = 'New password is not valid.'
                    else
                        call set_password(Student(requestingStudent)%Password, t1Password)
                        isDirtySTUDENTS = .true.
                        call xml_write_students(pathToYear, Student(requestingStudent)%CurriculumIdx)
                        loginCheckMessage = 'Successfully changed password for '//USERNAME
                        call html_college_links(device, CollegeIdxUser, mesg=loginCheckMessage)
                        REQUEST = fnCollegeLinks
                        return
                    end if
                else
                    loginCheckMessage = 'New password and repeat do not match.'
                end if
            else
                loginCheckMessage = 'New password and/or repeat not specified.'
            end if
        end if

        call html_write_header(device, 'Change password for '//trim(USERNAME), loginCheckMessage )

        call make_form_start(device, REQUEST)

        if (REQUEST==fnChangeStudentPassword) write(device,AFORMAT) &
            beginbold//'Old password:'//endbold//linebreak, &
            '<input size="20" type="password" name="C" value="">', &
            linebreak//linebreak
        write(device,AFORMAT) &
            beginbold//'New password:'//endbold//linebreak, &
            '<input size="20" type="password" name="P" value="">', &
            linebreak//linebreak, &
            beginbold//'Repeat new password:'//endbold//linebreak, &
            '<input size="20" type="password" name="R" value="">', &
            linebreak//linebreak, &
            '<input type="submit" value="Update">'//endform//horizontal

    end subroutine change_current_student_password


    subroutine generate_student_password(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character (len=MAX_LEN_PASSWD_VAR) :: Password
        integer :: idxCURR

        call html_comment('generate_student_password()')

        ! which student ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, idxCURR)
        targetStudent = index_to_student(tStdNo)
        idxCURR = Student(targetStudent)%CurriculumIdx
        if (REQUEST==fnGenerateStudentPassword) then
            call set_password(Student(targetStudent)%Password)
            isDirtySTUDENTS = .true.
        end if
        call get_student_password(targetStudent, Password)
        call html_write_header(device, Student(targetStudent)%StdNo//nbsp// &
            trim(Student(targetStudent)%Name)// &
            trim(make_href(fnCurriculum, Curriculum(idxCurr)%Code, A1=Curriculum(idxCurr)%Code, &
            pre=beginsmall//' (', post=' ) '//endsmall)) )
        if ( is_admin_of_college(Curriculum(idxCurr)%CollegeIdx) ) then
            write(device,AFORMAT) 'Password is '//nbsp//trim(Password)//  &
                trim(make_href(fnGenerateStudentPassword, 'Reset password', A1=tStdNo, pre=nbsp) )
        end if
        write(device,AFORMAT) horizontal

    end subroutine generate_student_password


    subroutine student_add(device)
        integer, intent (in) :: device

        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        integer :: ierr, std
        logical :: criticalErr

        call html_comment('student_add()')

        ! student exists?
        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
        std = index_to_student(tStdNo)

        if (ierr/=0 .or. tStdNo==SPACE) then
            call html_college_links(device, CollegeIdxUser, 'Add student: student number not specified?')
            return
        else if (std/=0) then
            REQUEST = fnStudentEdit
            call xml_read_student_info(std, ierr)
            if (ierr==0) then ! previously entered
                call student_copy_from_info(Student(std))
            else ! initialize info file
                call xml_student_info(std, Student(std))
            end if
            call student_edit_info(device, std, 'Student "'//tStdNo//'" already on record.')
            return
        end if

        ! add a record
        if (NumStudents>0) then
            NumAdditionalStudents = NumAdditionalStudents + 1
        else ! this is the first student
            NumStudents = 1
        end if
        call check_array_bound (NumStudents+NumAdditionalStudents, MAX_ALL_STUDENTS, 'MAX_ALL_STUDENTS', criticalErr)
        if (criticalErr) then
            call html_college_links(device, CollegeIdxUser, linebreak//horizontal//'No more space for another student.')
            return
        end if

        std = NumStudents+NumAdditionalStudents
        call initialize_student(Student(std))
        Student(std)%StdNo = tStdNo
        Student(std)%CurriculumIdx = NumCurricula
        call set_password(Student(std)%Password)

        ierr = year_prefix(Student(std))
        if (index(StdNoPrefix, ':'//tStdNo(:ierr))==0) then
            call make_student_directories()
        end if

        call xml_read_student_info(std, ierr)
        if (ierr==0) then ! previously entered
            call student_copy_from_info(Student(std))
        else ! initialize info file
            call xml_student_info(std, Student(std))
        end if

        REQUEST = fnStudentEdit
        call student_edit_info(device, std, 'Added student '//tStdNo)

    end subroutine student_add


    subroutine student_edit_info(device, std, mesg)
        integer, intent (in) :: device
        integer, intent (in), optional :: std
        character(len=*), intent (in), optional :: mesg

        type (TYPE_STUDENT_INFO) :: wrkStudent
        character(len=255) :: header, comment
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo, tAction
        character (len=MAX_LEN_PASSWD_VAR) :: Password
        integer :: idxCURR, j, k, l, ierr
        logical :: reSort, changed, isDirty

        ! which student ?
        if (present(std)) then
            isDirty = .true.
            targetStudent = std
            tStdNo = Student(targetStudent)%StdNo
            tAction = 'Add'
            header = 'Edit student info'
            comment = mesg
        else
            call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
            if (ierr/=0 .or. tAction==SPACE) tAction = 'Edit'
            call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
            targetStudent = index_to_student(tStdNo)
            header = 'Edit existing student info'
            comment = SPACE
            call xml_read_student_info(targetStudent, ierr)
        end if

        idxCURR = Student(targetStudent)%CurriculumIdx
        if (REQUEST==fnGenerateStudentPassword) then
            call set_password(Student(targetStudent)%Password)
            isDirty = .true.
            tAction = 'Password'
            header = 'Edit student info'
            comment = 'Changed password for '//tStdNo
        end if

        wrkStudent = StudentInfo

        call html_comment(tAction//tStdNo//itoa(targetStudent))

        if (trim(tAction)=='Update' .and. .not. isRoleOfficial) then ! collect changes

            header = 'Edit student info'
            comment = SPACE
            changed = .false.

            call cgi_get_named_string(QUERY_STRING, 'Name', wrkStudent%Name, ierr)
            reSort = wrkStudent%Name/=StudentInfo%Name
            if (reSort) then
                changed= .true.
                comment = ': Name'
                call log_student_record_change(targetStudent, 'Name='//wrkStudent%Name)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Gender', wrkStudent%Gender, ierr)
            if (wrkStudent%Gender/=StudentInfo%Gender) then
                changed= .true.
                comment = ': Gender '//comment
                call log_student_record_change(targetStudent, 'Gender='//wrkStudent%Gender)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Adviser', wrkStudent%Adviser, ierr)
            if (wrkStudent%Adviser/=StudentInfo%Adviser) then
                changed= .true.
                comment = ': Adviser '//comment
                call log_student_record_change(targetStudent, 'Adviser='//wrkStudent%Adviser)
            end if

            call cgi_get_named_string(QUERY_STRING, 'BirthDate', wrkStudent%BirthDate, ierr)
            if (wrkStudent%BirthDate/=StudentInfo%BirthDate) then
                changed= .true.
                comment = ': BirthDate '//comment
                call log_student_record_change(targetStudent, 'BirthDate='//wrkStudent%BirthDate)
            end if

            call cgi_get_named_string(QUERY_STRING, 'BirthPlace', wrkStudent%BirthPlace, ierr)
            if (wrkStudent%BirthPlace/=StudentInfo%BirthPlace) then
                changed= .true.
                comment = ': BirthPlace '//comment
                call log_student_record_change(targetStudent, 'BirthPlace='//wrkStudent%BirthPlace)
            end if

            call cgi_get_named_string(QUERY_STRING, 'EntryDate', wrkStudent%EntryDate, ierr)
            if (wrkStudent%EntryDate/=StudentInfo%EntryDate) then
                changed= .true.
                comment = ': EntryDate '//comment
                call log_student_record_change(targetStudent, 'EntryDate='//wrkStudent%EntryDate)
            end if

            call cgi_get_named_string(QUERY_STRING, 'GraduationDate', wrkStudent%GraduationDate, ierr)
            if (wrkStudent%GraduationDate/=StudentInfo%GraduationDate) then
                changed= .true.
                comment = ': GraduationDate '//comment
                call log_student_record_change(targetStudent, 'GraduationDate='//wrkStudent%GraduationDate)
            end if

            call cgi_get_named_string(QUERY_STRING, 'LastAttended', wrkStudent%LastAttended, ierr)
            if (wrkStudent%LastAttended/=StudentInfo%LastAttended) then
                changed= .true.
                comment = ': LastAttended '//comment
                call log_student_record_change(targetStudent, 'LastAttended='//wrkStudent%LastAttended)
            end if

            call cgi_get_named_string(QUERY_STRING, 'TranscriptRemark', wrkStudent%TranscriptRemark, ierr)
            if (wrkStudent%TranscriptRemark/=StudentInfo%TranscriptRemark) then
                changed= .true.
                comment = ': TranscriptRemark '//comment
                call log_student_record_change(targetStudent, 'TranscriptRemark='//wrkStudent%TranscriptRemark)
            end if

            call cgi_get_named_string(QUERY_STRING, 'AdmissionData', wrkStudent%AdmissionData, ierr)
            if (wrkStudent%AdmissionData/=StudentInfo%AdmissionData) then
                changed= .true.
                comment = ': AdmissionData '//comment
                call log_student_record_change(targetStudent, 'AdmissionData='//wrkStudent%AdmissionData)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Scholarship', wrkStudent%Scholarship, ierr)
            if (wrkStudent%Scholarship/=StudentInfo%Scholarship) then
                changed= .true.
                comment = ': Scholarship '//comment
                call log_student_record_change(targetStudent, 'Scholarship='//wrkStudent%Scholarship)
            end if

            call cgi_get_named_string(QUERY_STRING, 'HomeAddress', wrkStudent%HomeAddress, ierr)
            if (wrkStudent%HomeAddress/=StudentInfo%HomeAddress) then
                changed= .true.
                comment = ': HomeAddress '//comment
                call log_student_record_change(targetStudent, 'HomeAddress='//wrkStudent%HomeAddress )
            end if

            call cgi_get_named_string(QUERY_STRING, 'CurriculumIdx', tCurriculum, ierr)
            wrkStudent%CurriculumIdx = index_to_curriculum(tCurriculum)
            idxCURR = wrkStudent%CurriculumIdx
            if (wrkStudent%CurriculumIdx/=StudentInfo%CurriculumIdx) then
                changed= .true.
                comment = ': Curriculum '//comment
                call log_student_record_change(targetStudent, 'Curriculum='//tCurriculum )
            end if

            call cgi_get_named_integer(QUERY_STRING, 'Classification', wrkStudent%Classification, ierr)
            if (wrkStudent%Classification/=StudentInfo%Classification) then
                changed= .true.
                comment = ': Classification '//comment
                call log_student_record_change(targetStudent, 'Classification='//itoa(wrkStudent%Classification) )
            end if

            call cgi_get_named_integer(QUERY_STRING, 'CountryIdx', wrkStudent%CountryIdx, ierr)
            if (wrkStudent%CountryIdx/=StudentInfo%CountryIdx) then
                changed= .true.
                comment = ': CountryIdx '//comment
                call log_student_record_change(targetStudent, 'CountryIdx='//itoa(wrkStudent%CountryIdx) )
            end if

            if (changed) then

                ! unoficially enrolled or on leave?
                if (wrkStudent%Classification>=NotOfficiallyEnrolled) then
                    call initialize_pre_enlistment(Enlistment(currentTerm,targetStudent))
                    call xml_write_pre_enlistment(trim(pathToYear)//trim(txtSemester(currentTerm))//DIRSEP, &
                        'ENLISTMENT', Enlistment(currentTerm,0:), Section(currentTerm,0:))
                    comment = ': del entries in ENLISTMENT.XML '//comment
                    call log_student_record_change(targetStudent, 'Not enrolled: Removed subjects for '//trim(pathToYear)// &
                        trim(txtSemester(currentTerm)) )
                end if

                j = index_to_teacher(StudentInfo%Adviser)
                Teacher(j)%NumAdvisees = Teacher(j)%NumAdvisees - 1
                j = index_to_teacher(wrkStudent%Adviser)
                Teacher(j)%NumAdvisees = Teacher(j)%NumAdvisees + 1

                StudentInfo = wrkStudent
                call student_copy_from_info(Student(targetStudent))

                if (reSort) call sort_alphabetical_students()

                isDirty = .true.
                comment = 'Changes made '//comment
            else
                comment = 'No changes made?'
            end if

        end if

        if (trim(tAction)/='Edit' .and. isRoleOfficial) then
            comment = '"'//trim(tAction)//SPACE//trim(tStdno)//'" failed. '//sorryMessage
        end if

        if (isDirty) then
            isDirtySTUDENTS = .true.
            call xml_write_students(pathToYear, idxCURR)
            call xml_student_info(targetStudent)
        end if

        call html_write_header(device, header, comment)

        call make_form_start(device, fnStudentEdit, tStdNo)
        write(device,AFORMAT) &
            '<table border="0" width="100%" cellpadding="0" cellspacing="0">'

        write(device,AFORMAT) &
            begintr//begintd//'Student number:'//endtd//begintd//tStdNo//endtd//endtr

        ! password
        if ( is_admin_of_college(Curriculum(idxCURR)%CollegeIdx) .and. .not. isRoleOfficial) then
            call get_student_password(targetStudent, Password)
            write(device,AFORMAT) &
                begintr//begintd//'Password:'//endtd//begintd//trim(Password)//  &
                trim(make_href(fnGenerateStudentPassword, 'Reset password', &
                        A1=tStdNo, pre=nbsp//nbsp, post=endtd//endtr) )
        end if

        write(device,AFORMAT) &
            begintr//begintd//'Name '//beginsmall//'(LAST, FIRST MI)'//endsmall//':'//endtd//begintd// &
            '<input name="Name" size="40" value="'//trim(wrkStudent%Name)//'">'//endtd//endtr

        write(device,AFORMAT) &
            begintr//begintd//'Gender:'//endtd//begintd//'<select name="Gender">'
        if (wrkStudent%Gender=='M') then
            write(device,AFORMAT) &
                '<option value="F"> Female <option '//trim(selected(1))//' value="M"> Male'
        else if (wrkStudent%Gender=='F') then
            write(device,AFORMAT) &
                '<option '//trim(selected(1))//' value="F"> Female <option value="M"> Male'
        else
            write(device,AFORMAT) &
                '<option value="">(select) <option value="F"> Female <option value="M"> Male'
        end if
        write(device,AFORMAT) &
            '</select>'//endtd//endtr

        write(device,AFORMAT) &
            begintr//begintd//'Country:'//endtd//begintd//'<select name="CountryIdx">'
        if (wrkStudent%CountryIdx==1) then
            write(device,AFORMAT) &
                '<option '//trim(selected(1))//' value="1"> Philippines <option value="0"> Other '
        else
            write(device,AFORMAT) &
                '<option value="1"> Philippines <option '//trim(selected(1))//' value="0"> Other '
        end if
        write(device,AFORMAT) &
            '</select>'//endtd//endtr

        write(device,AFORMAT) &
            begintr//begintd//'Curriculum:'//endtd//begintd//'<select name="CurriculumIdx">'
        do l=1,NumCurricula
            k = Curriculum(l)%CollegeIdx
            if (l==idxCurr) then
                j = 1
            else
                j = 0
            end if
            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(Curriculum(l)%Code)//'"> '// &
                trim(text_curriculum_info(l))
        end do
        write(device,AFORMAT) '</select>'//endtd//endtr

        write(device,AFORMAT) &
            begintr//begintd//'Adviser:'//endtd//begintd//'<select name="Adviser">'
        write(device,AFORMAT) '<option value=""> (Select adviser)'
        do l=1,NumTeachers+NumAdditionalTeachers
            if (Department(Teacher(l)%DeptIdx)%CollegeIdx/=Curriculum(idxCurr)%CollegeIdx) cycle
            if (Teacher(l)%TeacherId==wrkStudent%Adviser) then
                j = 1
            else
                j = 0
            end if
            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(Teacher(l)%TeacherId)//'"> '// &
                trim(Teacher(l)%Name)
        end do
        write(device,AFORMAT) '</select>'//endtd//endtr

        ! classification
        write(device,AFORMAT) &
            begintr//begintd//'Classification:'//endtd//begintd//'<select name="Classification">'
        do l=1,8
            if (l==wrkStudent%Classification) then
                j = 1
            else
                j = 0
            end if
            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(itoa(l))//'"> '// &
                trim(txtYear(l+10))
        end do
        write(device,AFORMAT) '</select> '//beginsmall//beginitalic//'(Note: '// &
            'Hidden - not officially enrolled, exclude from Needs Analysis.)'//enditalic//endsmall// &
            endtd//endtr

        write(device,AFORMAT) &
            begintr//begintd//'Date of birth:'//endtd//begintd// &
            '<input name="BirthDate" size="20" value="'//trim(wrkStudent%BirthDate)//'">'//endtd//endtr

        write(device,AFORMAT) &
            begintr//begintd//'Place of birth:'//endtd//begintd// &
            '<input name="BirthPlace" size="40" value="'//trim(wrkStudent%BirthPlace)//'">'//endtd//endtr

        write(device,AFORMAT) &
            begintr//begintd//'Home address:'//endtd//begintd// &
            '<input name="HomeAddress" size="40" value="'//trim(wrkStudent%HomeAddress)//'">'//endtd//endtr

        write(device,AFORMAT) &
            begintr//begintd//'Previous school:'//endtd//begintd// &
            '<input name="LastAttended" size="40" value="'//trim(wrkStudent%LastAttended)//'">'//endtd//endtr

        write(device,AFORMAT) &
            begintr//begintd//'Scholarship:'//endtd//begintd// &
            '<input name="Scholarship" size="40" value="'//trim(wrkStudent%Scholarship)//'">'//endtd//endtr

        write(device,AFORMAT) &
            begintr//begintd//'Date of entry:'//endtd//begintd// &
            '<input name="EntryDate" size="20" value="'//trim(wrkStudent%EntryDate)//'">'//endtd//endtr

        write(device,AFORMAT) &
            begintr//begintd//'Admission data:'//endtd//begintd// &
            '<input name="AdmissionData" size="20" value="'//trim(wrkStudent%AdmissionData)//'">'//endtd//endtr

        write(device,AFORMAT) &
            begintr//begintd//'Date of graduation:'//endtd//begintd// &
            '<input name="GraduationDate" size="20" value="'//trim(wrkStudent%GraduationDate)//'">'//endtd//endtr

        write(device,AFORMAT) &
            begintr//begintd//'Remark on transcript:'//endtd//begintd// &
            '<input name="TranscriptRemark" size="80" value="'//trim(wrkStudent%TranscriptRemark)//'">'//endtd//endtr

        ! update button
        write(device,AFORMAT) &
            endtable//linebreak//'<input type="submit" name="action" value="Update"> ', &
            beginitalic//'(or, select another student below)'//enditalic//endform//horizontal

        ! add student button
        if (trim(tAction)=='Add') then
            call make_form_start(device, fnStudentAdd)
            write(device,AFORMAT) &
                '<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
                begintr//begintd//'Add student number <input name="A1" value=""> '//nbsp// &
                '<input type="submit" name="action" value="Add">'//endform// &
                endtd//endtr//endtable//horizontal

        ! find student button
        else
            call make_form_start(device, fnFindStudent)
            write(device,AFORMAT) '<table border="0" cellpadding="0" cellspacing="0">', &
                begintr//begintd//beginbold//'Find students'//endbold//' with <input name="A1" value="">', &
                'in name or number. <input type="submit" name="action" value="Search">'//endform// &
                endtd//endtr//endtable//horizontal
        end if

    end subroutine student_edit_info


    subroutine student_edit_grades (device, thisTerm, UseClasses, Offering)
        implicit none
        integer, intent (in) :: device, thisTerm
        logical, intent (in) :: UseClasses
        type (TYPE_OFFERED_SUBJECTS), intent(in), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering

        character(len=MAX_LEN_SUBJECT_CODE) :: tArea
        character(len=255) :: AreaNumbers
        real :: SumEnrolled, SumEarned, SumDown, SumUp, tUnits
        real :: GSumEnrolled, GSumEarned, GSumDown, GSumUp

        integer :: grd, Year, Term, m, tdx
        integer :: prevtaken, nINCs
        real :: up, down
        character (len=10) :: token1, token2

        logical :: FlagIsUp, isEditor
        integer :: crse, n_changes, gdx, jdx, ierr, i, k, idx, j
        character(len=MAX_LEN_SUBJECT_CODE) :: input_name1, input_name2, input_value
        character(len=3*MAX_LEN_SUBJECT_CODE) :: tAction, input_name3
        character (len=MAX_LEN_TEXT_GRADE) :: tGrade

        character(len=127) :: advising_comment
        type (TYPE_PRE_ENLISTMENT) :: Advice
        integer :: MissingPOCW, NRemaining

        ! which student?
        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)

        line = SPACE
        targetStudent = index_to_student(tStdNo)
        targetCurriculum = Student(targetStudent)%CurriculumIdx
        targetCollege = Curriculum(targetCurriculum)%CollegeIdx

        ! read checklist
        call read_student_records (targetStudent)
        n_changes = 0

        if (trim(tAction)=='Submit COMPLETION' .and. .not. isRoleOfficial) then ! look for COMP:<subject>=<grade>

            line = ' : '
            input_name1 = 'COMP:'
            loop_COMPS: &
            do while (.true.)
                call cgi_get_wild_name_value(QUERY_STRING, input_name1, input_name3, input_value, ierr)
                if (ierr/=0) exit ! no more COMP: in QUERY
                ! COMP:YYYY:T:AREA_NUM:gdx or COMP:0:0:AREA_NUM

                if (input_name3(1:3)=='0:0') then ! new completion record ?

                    input_name2 = input_name3(5:)
                    call underscore_to_blank(input_name2, tSubject)
                    crse = index_to_subject(tSubject)
                    gdx = atoi(input_value)

                    if (gdx==gdxINC .or. gdx==gdxNFE) cycle ! non-INC completion grade not specified

                    ! add a COMPLETION record
                    lenTCG = lenTCG + 1
                    TCG(lenTCG)%Code = 5 ! COMPLETION
                    TCG(lenTCG)%Year = currentYear
                    TCG(lenTCG)%Term = currentTerm
                    TCG(lenTCG)%Subject = crse
                    TCG(lenTCG)%Grade = gdx
                    call html_comment('New record TCG('//itoa(lenTCG)//') - '//tSubject//txtGrade(pGrade(gdx)) )
                    line = ' : '//trim(Subject(crse)%Name)//', INC/NFE->'//txtGrade(pGrade(gdx))//line
                    n_changes = n_changes + 1

                else
                    ! get year, term, subject, original completion grade
                    Year = atoi(input_name3(1:4))
                    Term = atoi(input_name3(6:6))
                    input_name2 = input_name3(8:)
                    i = index(input_name2,':')
                    grd = atoi(input_name2(i+1:))
                    input_name2(i:) = SPACE
                    call underscore_to_blank(input_name2, tSubject)
                    tGrade = input_value

                    crse = index_to_subject(tSubject)
                    gdx = atoi(tGrade)

                    if (grd==gdx) cycle ! no change

                    ! find matching COMPLETION record
                    do k=lenTCG,1,-1
                        if (TCG(k)%Code/=5) cycle ! not COMPLETION
                        if (TCG(k)%Subject/=crse .or. TCG(k)%Grade/=grd) cycle ! not the subject+grd
                        TCG(k)%Grade = gdx
                        call html_comment('Found at TCG('//itoa(k)//') - '//tSubject//txtGrade(pGrade(grd)) )
                        line = ' : '//trim(tSubject)//','//txtGrade(pGrade(grd))//'->'//txtGrade(pGrade(gdx))//line
                        n_changes = n_changes + 1
                        exit
                    end do
                end if

            end do loop_COMPS

            if (n_changes==0) then
                line = trim(tAction)//' : Nothing to update?'
            else
                line = trim(tAction)//line
                call xml_write_student_completions(targetStudent)
            end if

        end if

        if ( (trim(tAction)=='ADVANCE Credit' .or. trim(tAction)=='Update ADVANCE') &
              .and. .not. isRoleOfficial) then ! look for ADV:<subject>=<grade>

            line = ' : '
            input_name1 = 'ADV:'
            loop_ADVANCE: &
            do while (.true.)
                call cgi_get_wild_name_value(QUERY_STRING, input_name1, input_name2, input_value, ierr)
                if (ierr/=0) exit ! no more ADV: in QUERY

                call underscore_to_blank(input_name2, tSubject)
                tGrade = input_value

                crse = index_to_subject(tSubject)
                if (crse <= 0) cycle ! not a named subject
                gdx = index_to_grade(tGrade)

                if (gdx==0 .or. tgrade==SPACE) tGrade = 'Del'

                ! subject+grade in Record()?
                FlagIsUp = .false.
                do k=1,lenTCG
                    if (TCG(k)%Code/=2 .or. TCG(k)%Used) cycle
                    if (TCG(k)%Subject/=crse) cycle ! not the subject
                    if (gdx==TCG(k)%Grade) then
                        TCG(k)%Used = .true.
                        cycle loop_ADVANCE ! same grade
                    end if
                    FlagIsUp = .true. ! subject matched, w/ diff grade
                    TCG(k)%Grade = gdx ! make change
                    n_changes = n_changes + 1
                    line = ' : '//trim(Subject(crse)%Name)//', '//tGrade//line
                    TCG(k)%Used = .true.
                    cycle loop_ADVANCE
                end do

                if (.not. FlagIsUp .and. gdx>0) then ! not found; add an entry

                    lenTCG = lenTCG + 1
                    TCG(lenTCG)%Code = 2
                    TCG(lenTCG)%Year = currentYear
                    TCG(lenTCG)%Term = currentTerm
                    TCG(lenTCG)%Subject = crse
                    TCG(lenTCG)%Grade = gdx
                    line = ' : '//trim(Subject(crse)%Name)//', '//tGrade//line
                    n_changes = n_changes + 1

                end if

            end do loop_ADVANCE

            if (n_changes==0) then
                line = trim(tAction)//' : Nothing to update?'
            else
                line = trim(tAction)//line
                call xml_write_student_advance_credits(targetStudent)
            end if

        end if

        if (trim(tAction)=='Change GRADE' .and. .not. isRoleOfficial) then ! look for GRADE:<subject>=<grade>

            line = ' : '
            input_name1 = 'GRADE:'
            loop_GRADES: &
            do while (.true.)
                call cgi_get_wild_name_value(QUERY_STRING, input_name1, input_name3, input_value, ierr)
                if (ierr/=0) exit ! no more GRADE: in QUERY

                ! get year, term, subject
                Year = atoi(input_name3(1:4))
                Term = atoi(input_name3(6:6))
                input_name2 = input_name3(8:)
                i = index(input_name2,':')
                jdx = atoi(input_name2(i+1:))
                input_name2(i:) = SPACE
                call underscore_to_blank(input_name2, tSubject)
                tGrade = input_value

                crse = index_to_subject(tSubject)
                if (crse <= 0) cycle ! not a named subject

                if (tGrade/=SPACE) then ! not empty

                    gdx = index_to_grade(tGrade)
                    if (gdx==0) cycle ! underscores
                    if (gdx<0) then ! invalid grade or underscores
                        line = ' : invalid grade '//trim(tGrade)//line
                        cycle
                    end if

                    ! year+term+subject+grade in Record()?
                    FlagIsUp = .false.
                    do k=1,lenTCG
                        if (TCG(k)%Code==1 .or. TCG(k)%Used) cycle
                        if (TCG(k)%Subject/=crse) cycle ! not the subject
                        if (TCG(k)%Year/=Year) cycle ! not the year
                        if (TCG(k)%Term/=Term) cycle ! not the term
                        if (TCG(k)%Grade/=jdx) cycle ! not the grade
                        if (gdx==TCG(k)%Grade) then
                            TCG(k)%Used = .true.
                            cycle loop_GRADES ! same grade
                        end if
                        FlagIsUp = .true. ! subject matched, w/ diff grade
                        TCG(k)%Grade = gdx ! make change
                        n_changes = n_changes + 1
                        line = ' : '//trim(Subject(crse)%Name)//COMMA//trim(txtGrade(pGrade(gdx)))//line
                        TCG(k)%Used = .true.
                        cycle loop_GRADES
                    end do

                    if (.not. FlagIsUp) then ! not found; add an entry

                        lenTCG = lenTCG + 1
                        TCG(lenTCG)%Code = 3 ! assume FINALGRADE
                        TCG(lenTCG)%Year = currentYear
                        TCG(lenTCG)%Term = currentTerm
                        TCG(lenTCG)%Subject = crse
                        TCG(lenTCG)%Grade = gdx
                        line = ' : '//trim(Subject(crse)%Name)//', '//tGrade//line
                        n_changes = n_changes + 1

                    end if

                else ! grade is empty (to indicate 'Remove grade')

                    FlagIsUp = .false. ! subject found?
                    do k=lenTCG,1,-1
                        if (TCG(k)%Year/=Year) cycle ! not the year
                        if (TCG(k)%Term/=Term) cycle ! not the term
                        if (TCG(k)%Subject/=crse) cycle ! not the subject
                        if (TCG(k)%Grade/=jdx) cycle ! not the grade
                        FlagIsUp = .true.
                        exit
                    end do
                    if (FlagIsUp) then
                        do i=k+1,lenTCG ! shift down
                            TCG(i-1) = TCG(i)
                        end do
                        TCG(lenTCG) = TCG(lenTCG+1)
                        lenTCG = lenTCG - 1
                        n_changes = n_changes + 1
                        line = ' : Removed grade in '//trim(Subject(crse)%Name)//line
                    end if

                end if

            end do loop_GRADES

            if (n_changes==0) then
                line = trim(tAction)//' : Nothing to update?'
            else
                line = trim(tAction)//line
                call xml_write_student_grades(targetStudent)
            end if

        end if

        if (len_trim(tAction)>0 .and. isRoleOfficial) then
            line = '"'//trim(tAction)//'" failed. '//sorryMessage
        end if

        ! reload ?
        if (n_changes>0) then
            call read_student_records (targetStudent)
        end if

        call html_comment('Grades of '//trim(StudentInfo%Name))

        tStdNo = Student(targetStudent)%StdNo
        isEditor = is_admin_of_college(targetCollege)

        call html_write_header(device, trim(tStdNo)//SPACE//trim(Student(targetStudent)%Name)// &
            linebreak//text_curriculum_info(targetCurriculum), line)

        if (isEditor) then
            write(device,AFORMAT) &
                linebreak//beginbold//'UNOFFICIAL Copy of Grades and Weighted Average by Term'//endbold, &
                trim(make_href(fnTranscript, 'Printable', A1=tStdNo, pre=' ( ', post=' )')), &
                linebreak//beginitalic//'Enter or modify contents of the edit boxes, then click "Change GRADE". ', &
                ' For previous and current terms, change a grade in the corresponding gradesheet.'//enditalic
        else
            write(device,AFORMAT) &
                linebreak//beginbold//'UNOFFICIAL Copy of Grades and Weighted Average by Term'//endbold
        end if

        write(device,AFORMAT)  &
            '<table border="0" width="100%">', &
            begintr//'<td colspan="7">'//horizontal//endtd//endtr, &
            begintr//begintd//'SUBJECT'//endtd, &
            '<td colspan="4" align="left">COLLEGIATE RECORDS'//endtd, &
            tdaligncenter//'FINAL'//endtd, &
            tdnbspendtd//tdnbspendtd//tdnbspendtd//tdnbspendtd//endtr, &
            begintr//begintd//'NUMBER'//endtd, &
            '<td colspan="4" align="left">DESCRIPTIVE TITLE OF THE SUBJECT'//endtd, &
            tdaligncenter//'GRADE'//endtd, &
            tdaligncenter//'CREDIT'//endtd, &
            '<td colspan="3" align="right">WTD AVERAGE'//endtd//endtr

        ! the advance credits (PASSed subjects)
        m = 0
        do tdx=1,lenTCG
            TCG(tdx)%Used = .false.
            if (TCG(tdx)%Code==2 .and. is_grade_passing(TCG(tdx)%Grade)) m = m+1
        end do
        if (m>0) then ! there are advance credits
            if (isEditor) then
                call make_form_start(device, fnStudentGrades, tStdNo)
            end if
            write(device,AFORMAT) &
                begintr//'<td colspan="7">'//horizontal//endtd//endtr, &
                begintr//tdnbspendtd//'<td colspan="4" align="left">ADVANCE/TRANSFER CREDITS'//endtd// &
                '<td colspan="6">'//nbsp//endtd//endtr
            do tdx=1,lenTCG

                if (TCG(tdx)%Code/=2 .or. .not. is_grade_passing(TCG(tdx)%Grade)) cycle

                crse = TCG(tdx)%Subject
                grd = TCG(tdx)%Grade
                token1 = SPACE
                token2 = SPACE
                tSubject = Subject(crse)%Name
                call blank_to_underscore(tSubject, input_name2)

                line = begintr//begintd//trim(tSubject)//endtd//'<td colspan="4">'//trim(Subject(crse)%Title)//endtd

                if (isEditor) then
                    line = trim(line)//tdaligncenter//'<input type="text" size="4" name="ADV:'//trim(input_name2)// &
                        '" value="'//trim(txtGrade(pGrade(grd)))//'">'//endtd
                else
                    line = trim(line)//tdaligncenter//txtGrade(pGrade(grd))//endtd
                end if

                if (Subject(crse)%Units == 0.0 .or. tSubject(1:5)=='NSTP ') then
                    line = trim(line)//tdnbspendtd
                else
                    line = trim(line)//tdaligncenter//trim(ftoa(Subject(crse)%Units,1))//endtd
                end if

                write (device,AFORMAT) trim(line)// &
                    tdalignright//trim(token1)//endtd// &
                    tdalignright//trim(token2)//endtd// &
                    tdnbspendtd//endtr

                TCG(tdx)%Used = .true.
            end do
            if (isEditor) then
                write(device,AFORMAT) begintr// &
                    '<td colspan="6" align="right"><input type="submit" name="action" value="Update ADVANCE">'//endform// &
                    endtd//'<td colspan="4">'//endtd//endtr
            end if

        end if

        SumUp = 0.0
        SumDown = 0.0
        GSumDown = 0.0
        GSumUp = 0.0
        prevtaken = -10
        nINCs = 0
        FlagIsUp = .false.  ! true if <input...> was written
        do tdx=1,lenTCG

            ! FINALGRADES only (with completions if applicable)
            if (TCG(tdx)%Code/=3 .or. TCG(tdx)%Used .or. TCG(tdx)%ErrorCode>1) cycle

            if (prevtaken /= TCG(tdx)%Taken) then
                if (isEditor .and. prevtaken/=-10 .and. FlagIsUp .and. isEnabledEditGrade) then
                    write(device,AFORMAT) begintr, &
                        '<td colspan="6" align="right"><input type="submit" name="action" value="Change GRADE">'//endform// &
                        endtd//'<td colspan="4">'//endtd//endtr
                end if

                ! write summary for current term
                write(device,AFORMAT) begintr//'<td colspan="7">'//horizontal//endtd
                if (SumUp*SumDown>0.0) then
                    write(device,'(a,f8.2,a,f5.1,a,f8.2,a)') &
                        tdalignright, SumUp, endtd// &
                        tdalignright, SumDown, endtd// &
                        tdalignright, SumUp/SumDown, endtd//endtr
                    GSumUp = GSumUp + SumUp
                    GSumDown = GSumDown + SumDown
                 else
                    write(device,AFORMAT) endtr
                end if
                ! header for next term
                write(device,AFORMAT) &
                    begintr//tdnbspendtd//'<td colspan="4" align="left">', &
                        trim(text_term_school_year(TCG(tdx)%Term, TCG(tdx)%Year))

                if (isEditor) then
                    write(device,AFORMAT) &
                        trim(make_href(fnGradeCertification, 'Certification', &
                        A1=tStdNo, A2=itoa(TCG(tdx)%Year), A3=itoa(TCG(tdx)%Term), &
                        pre=' ( ', post=' )') )
                    call make_form_start(device, fnStudentGrades, tStdNo)
                end if
                write(device,AFORMAT) endtd//'<td colspan="6">'//nbsp//endtd//endtr

                ! re-initialize accumulators
                prevtaken = TCG(tdx)%Taken
                SumUp = 0.0
                SumDown = 0.0
                ! true if <input...> was written
                FlagIsUp = .false.
            end if

            crse = TCG(tdx)%Subject
            grd = TCG(tdx)%Grade
            tUnits = Subject(crse)%Units
            if (grd==gdxINC .or. grd==gdxNFE) nINCs = nINCs+1

            up = 0.0
            down = 0.0
            token1 = SPACE
            token2 = SPACE
            tSubject = Subject(crse)%Name
            call blank_to_underscore(tSubject, input_name2)

            line = begintr//begintd//trim(tSubject)//endtd//'<td colspan="4">'//trim(Subject(crse)%Title)//endtd

            if (TCG(tdx)%ReExam/=0) then
                line = trim(line)//tdaligncenter//txtGrade(pGrade(grd))//FSLASH// &
                txtGrade(pGrade(TCG(tdx)%ReExam))//endtd
                grd = TCG(tdx)%ReExam
            else
                if (isEditor .and. isEnabledEditGrade .and. .not. ( &
                         (currentYear==TCG(tdx)%Year .and. currentTerm==TCG(tdx)%Term) .or. &
                         (cTm1Year==TCG(tdx)%Year .and. cTm1==TCG(tdx)%Term) ) ) then
                    line = trim(line)//tdaligncenter//'<input type="text" size="4" name="GRADE:'// &
                        trim(itoa(TCG(tdx)%Year))//':'//trim(itoa(TCG(tdx)%Term))//':'//trim(input_name2)// &
                        ':'//trim(itoa(grd))//'" value="'//trim(txtGrade(pGrade(grd)))//'">'//endtd
                    FlagIsUp = .true.
                else
                    line = trim(line)//tdaligncenter//txtGrade(pGrade(grd))//endtd
                end if
            end if

            if (tUnits == 0.0 .or. Subject(crse)%Name(1:5)=='NSTP ') then ! exclude
                line = trim(line)//tdnbspendtd

            else if (grd==gdxDRP .or. grd==gdxPASS) then ! exclude
                line = trim(line)//tdnbspendtd

            else if (grd==gdxINC .or. grd==gdxNFE .or. grd==gdxREGD) then
                line = trim(line)//tdnbspendtd

            else
                up = tUnits*fGrade(grd)
                if (up/=0.0) then
                    down = tUnits
                    line = trim(line)//tdaligncenter//trim(ftoa(tUnits,1))//endtd
                else
                    down = 0.0
                    line = trim(line)//tdnbspendtd
                end if
            end if

            if (down>0.0) then
                write (token1,'(f8.2)') up
                write (token2,'(f5.1)') down
            end if
            write (device,AFORMAT) trim(line)// &
                tdalignright//trim(token1)//endtd// &
                tdalignright//trim(token2)//endtd// &
                tdnbspendtd//endtr
            SumUp = SumUp + Up
            SumDown = SumDown + Down
        end do
        ! write summary for last term
        if (isEditor .and. FlagIsUp .and. isEnabledEditGrade) then
            write(device,AFORMAT) &
                begintr//'<td colspan="6" align="right"><input type="submit" name="action" value="Change GRADE">'//&
                    endform//endtd// &
                '<td colspan="4">'//endtd//endtr
        end if
        write(device,AFORMAT) begintr//'<td colspan="7">'//horizontal//endtd
        if (SumUp*SumDown>0.0) then
            write(device,'(a,f8.2,a,f5.1,a,f8.2,a)') &
                tdalignright, SumUp, endtd// &
                tdalignright, SumDown, endtd// &
                tdalignright, SumUp/SumDown, endtd//endtr
            GSumUp = GSumUp + SumUp
            GSumDown = GSumDown + SumDown
        else
            write(device,AFORMAT) endtr
        end if

        if (GSumUp*GSumDown>0.0) then
            write(device,'(a,f8.2,a,f5.1,a,f8.2,a)') &
                begintr//'<td colspan="7" align="right">'//beginbold//'General Weighted Average : '//endbold//endtd// &
                tdalignright, GSumUp, endtd// &
                tdalignright, GSumDown, endtd// &
                tdalignright, GSumUp/GSumDown, endtd//endtr
        end if

        if (isEditor) then
            write(device,AFORMAT) endtable, horizontal

            if (nINCs>0) then
                call make_form_start(device, fnStudentGrades, tStdNo)

                write(device,AFORMAT) &
                    linebreak//linebreak//beginbold//'INCOMPLETE GRADES AND COMPLETIONS'//endbold// &
                        linebreak//beginitalic//'Select completion grades, then click "Submit COMPLETION".'//enditalic
                write(device,AFORMAT)  &
                    '<table border="0" width="80%">', &
                    begintr//'<td colspan="7">'//horizontal//endtd//endtr, &
                    begintr//begintd//'SUBJECT'//endtd, &
                    '<td colspan="4" align="left">DESCRIPTIVE TITLE OF SUBJECT'//endtd, &
                    tdaligncenter//'GRADE'//endtd, &
                    tdaligncenter//'COMPLETION'//endtd, &
                    '<td colspan="3">'//nbsp//endtd//endtr, &
                    begintr//'<td colspan="7">'//horizontal//endtd//endtr

                do tdx=1,lenTCG
                    grd = TCG(tdx)%Grade
                    if (grd/=gdxINC .and. grd/=gdxNFE) cycle ! not an INC/NFE

                    crse = TCG(tdx)%Subject
                    tSubject = Subject(crse)%Name
                    call blank_to_underscore(tSubject, input_name2)
                    write (device,AFORMAT) &
                        begintr//begintd//trim(tSubject)//endtd//'<td colspan="4">'//trim(Subject(crse)%Title)//endtd, &
                        tdaligncenter//txtGrade(pGrade(grd))//endtd

                    ! find macthing completion, if any
                    k = 0
                    do j=lenTCG,1,-1
                        if ( TCG(j)%Code/=5 .or. TCG(j)%Subject/=crse ) cycle ! not the subject
                        k = j
                        exit
                    end do
                    if (k/=0) then ! already completed

                        write (device,AFORMAT) &
                            tdaligncenter//'<select name="COMP:'// &
                            trim(itoa(TCG(k)%Year))//':'//trim(itoa(TCG(k)%Term))//':'//trim(input_name2)// &
                            ':'//trim(itoa(TCG(k)%Grade))//'">', &
                            '<option value="'//trim(itoa(grd))//'"> '//txtGrade(pGrade(grd))
                        do i=ZERO_PERCENT_GRADE+70,ZERO_PERCENT_GRADE+100
                            tGrade = txtGrade(pGrade(i))
                            if (i/=TCG(k)%Grade) then
                                j = 0
                            else
                                j = 1
                            end if
                            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(itoa(i))//'"> '//tGrade
                        end do

                    else
                        write (device,AFORMAT) &
                            tdaligncenter//'<select name="COMP:0:0:'//trim(input_name2)//'">'// &
                            '<option value="'//trim(itoa(grd))//'"> '//txtGrade(pGrade(grd))
                        do i=ZERO_PERCENT_GRADE+70,ZERO_PERCENT_GRADE+100
                            tGrade = txtGrade(pGrade(i))
                            write(device,AFORMAT) '<option value="'//trim(itoa(i))//'"> '//tGrade
                        end do
                    end if
                    write (device,AFORMAT) '</select>', &
                        tdnbspendtd//tdnbspendtd//tdnbspendtd//endtr

                end do

                write(device,AFORMAT) &
                    begintr//'<td colspan="7" align="right">'// &
                    '<input type="submit" name="action" value="Submit COMPLETION">'//endtd// &
                    '<td colspan="3">'//endtd//endtr, &
                    begintr//'<td colspan="7">'//horizontal//endtd//endtr, &
                    endtable//endform

            end if

            call advise_student (targetStudent, thisTerm, UseClasses, Offering, WaiverCOI(targetStudent), Advice, &
                MissingPOCW, NRemaining, advising_comment)

            if (len_trim(advising_comment)>0) then
                write(device,AFORMAT) linebreak//beginbold//red//trim(advising_comment)//black//endbold//linebreak//linebreak
            end if

           ! how many unearned subjects per term in curriculum
            do tdx=1,CheckList%NumTerms
                tArray(tdx) = 0
                call rank_to_year_term(tdx, Year, Term)
                do idx=1,CheckList%NSubjects
                    if (CheckList%SubjectTerm(idx)/=tdx) cycle ! not this term
                    crse = CheckList%SubjectIdx(idx)
                    if (crse<=0) cycle ! not named
                    if ( .not. is_grade_passing(CLExt(idx)%Grade) ) then  ! named subject, not passed
                        tArray(tdx) = tArray(tdx)+1
                        call html_comment('Unearned during Year '//itoa(Year)//' Term '//itoa(Term)//' is '//Subject(crse)%Name)
                    end if
                end do
            end do


            if (sum(tArray(1:Checklist%NumTerms))>0) then  ! some unearned subject

            ! ===================================
            write(device,AFORMAT) &
                linebreak//linebreak//beginbold//'ADVANCE/TRANSFER CREDITS'//endbold// &
                    linebreak//beginitalic//'Enter PASS grades for subjects to be credited, then click "ADVANCE Credit".'//enditalic

            write(device,AFORMAT)  &
                '<table border="0" width="80%">', &
                begintr//'<td colspan="7">'//horizontal//endtd//endtr, &
                begintr//begintd//'SUBJECT'//endtd, &
                '<td colspan="4" align="left">DESCRIPTIVE TITLE OF SUBJECT'//endtd, &
                tdaligncenter//'GRADE'//endtd, &
                tdaligncenter//'CREDIT'//endtd, &
                '<td colspan="3">'//nbsp//endtd//endtr

            do tdx=1,Checklist%NumTerms

                if (tArray(tdx)==0) cycle ! no unearned subject for this term

                call rank_to_year_term(tdx, Year, Term)

                call make_form_start(device, fnStudentGrades, tStdNo)

                ! header for term
                write(device,AFORMAT) &
                    begintr//'<td colspan="7">'//horizontal//endtd//endtr, &
                    begintr//tdnbspendtd//'<td colspan="4" align="left">'//trim(Checklist%Code)//': '// &
                    trim(txtYear(Year))//' Year, '//trim(txtSemester(Term+3))//' Term'//endbold//endtd// &
                    '<td colspan="6">'//nbsp//endtd//endtr

                do idx=1,Checklist%NSubjects
                    if (Checklist%SubjectTerm(idx)/=tdx) cycle ! not this term
                    crse = Checklist%SubjectIdx(idx)
                    if (crse<=0) cycle ! not named

                    tSubject = Subject(crse)%Name
                    call blank_to_underscore(tSubject, input_name2)

                    if ( .not. is_grade_passing(CLExt(idx)%Grade) ) then  ! named subject, not passed
                    !if (CLExt(idx)%Grade==0) then  ! named subject, not passed

                        write (device,AFORMAT) &
                            begintr//begintd//trim(tSubject)//endtd//'<td colspan="4">'//trim(Subject(crse)%Title)//endtd, &
                            tdaligncenter//'<input type="text" size="4" name="ADV:'//trim(input_name2)//'" value="">'//endtd, &
                            tdaligncenter//trim(ftoa(Subject(crse)%Units,1))//endtd, &
                            tdnbspendtd//tdnbspendtd//tdnbspendtd//endtr

                    !else if (CLExt(idx)%Grade==gdxPASS) then ! already recorded as ADVANCE/TRANSFER CREDIT

                    !    write (device,AFORMAT) &
                    !        begintr//begintd//trim(tSubject)//endtd//'<td colspan="4">'//trim(Subject(crse)%Title)//endtd, &
                    !        tdaligncenter//'<input type="text" size="4" name="ADV:'//trim(input_name2)// &
                    !        '" value="'//trim(txtGrade(pGrade(gdxPASS)))//'">'//endtd, &
                    !        tdaligncenter//trim(ftoa(Subject(crse)%Units,1))//endtd, &
                    !        tdnbspendtd//tdnbspendtd//tdnbspendtd//endtr
                    end if

                end do
                write(device,AFORMAT) begintr//'<td colspan="5">'//nbsp//endtd// &
                    '<td colspan="2"><input type="submit" name="action" value="ADVANCE Credit">'//endtd// &
                    '<td colspan="3">'//nbsp//endtd//endtr, &
                    endform
            end do

            write(device,AFORMAT) begintr//'<td colspan="7">'//horizontal//endtd//endtr, &
                endtable

            ! ===================================
            end if ! (sum(tArray(1:Checklist%NumTerms))>0) then  ! some unearned subject

        else
            write(device,AFORMAT) endtable//horizontal
        end if


        write(device,AFORMAT) linebreak//beginbold//'WEIGHTED AVERAGE BY SUBJECT AREA'//endbold//linebreak, horizontal, &
            '<table border="0" width="100%">'// &
            begintr//begintd//'SUBJECT AREA'//endtd// &
            tdalignright//'UNITS'//endtd// &
            tdalignright//'UNITS'//endtd// &
            tdalignright//'WEIGHTED'//endtd// &
            begintd//nbsp//'SUBJECTS'//endtd//endtr, &
            begintr//begintd//nbsp//endtd// &
            tdalignright//'ENROLLED'//endtd// &
            tdalignright//'EARNED'//endtd// &
            tdalignright//'AVERAGE'//endtd// &
            begintd//nbsp//endtd//endtr// &
            begintr//'<td colspan="5">'//horizontal//endtd//endtr

        GSumDown = 0.0
        GSumUp = 0.0
        GSumEnrolled = 0.0
        GSumEarned = 0.0
        do j=lenTCG,1,-1
            TCG(j)%Used = .false.
        end do
        do idx=1,NumSubjectAreas
            SumDown = 0.0
            SumUp = 0.0
            SumEnrolled = 0.0
            SumEarned = 0.0
            AreaNumbers = SPACE
            do j=lenTCG,1,-1
                if (TCG(j)%Used .or. TCG(j)%Code<2) cycle ! already counted or not a grade
                crse = TCG(j)%Subject
                tSubject = Subject(crse)%Name
                tArea = get_area(tSubject)
                jdx = len_trim(tArea)+1
                if (SubjectArea(idx)%Code/=tArea) cycle
                if (Subject(crse)%Units == 0.0 .or. tSubject(1:5)=='NSTP ') cycle
                gdx = TCG(j)%Grade
                if (gdx==gdxREGD) cycle
                tUnits = max(1.0,1.0*Subject(crse)%Units)
                if (gdx/=gdxLOA .and. gdx/=gdxDRP) then
                    if (TCG(j)%Code==3) then ! FINALGRADE
                        SumEnrolled = SumEnrolled + tUnits
                        AreaNumbers = COMMA//trim(tSubject(jdx:))//AreaNumbers
                    end if
                    if (is_grade_passing(gdx)) then
                        SumEarned = SumEarned + tUnits
                    elseif (is_grade_passing(TCG(j)%ReExam)) then
                        SumEarned = SumEarned + tUnits
                        gdx = TCG(j)%ReExam
                    end if
                    if (fGrade(gdx)>0) then
                        SumDown = SumDown + tUnits
                        SumUp = SumUp + tUnits*fGrade(gdx)
                    end if
                end if
                TCG(j)%Used = .true.
            end do
            if (SumEnrolled==0.0) cycle ! no units enrolled in this area
            !write(*,*) SubjectArea(idx), SumEnrolled, SumEarned, SumUp/max(1.0,SumDown), trim(AreaNumbers)
            write(device,'(2(a,f5.1),a,f8.2,a)') &
                begintr//begintd//trim(SubjectArea(idx)%Code)// &
                endtd//tdalignright, SumEnrolled, &
                endtd//tdalignright, SumEarned, &
                endtd//tdalignright, SumUp/max(1.0,SumDown), &
                endtd//begintd//nbsp//trim(SubjectArea(idx)%Code)//trim(AreaNumbers(2:))//endtd//endtr

            GSumEnrolled = GSumEnrolled + SumEnrolled
            GSumEarned = GSumEarned + SumEarned
            GSumDown = GSumDown + SumDown
            GSumUp = GSumUp + SumUp
        end do
        write(device,AFORMAT) begintr//'<td colspan="5">'//horizontal//endtd//endtr
        write(device,'(2(a,f5.1),a,f8.2,a)') &
            begintr//tdalignright//'TOTALS'//nbsp// &
            endtd//tdalignright, GSumEnrolled, &
            endtd//tdalignright, GSumEarned, &
            endtd//tdnbspendtd//tdnbspendtd//endtr
        write(device,AFORMAT) endtable//horizontal

    end subroutine student_edit_grades


    subroutine student_advisers_by_teacher (device)
        integer, intent (in) :: device

        integer :: n_count, tdx, std, ierr, nSteps, first, last, idx
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher, newAdviser
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character(len=MAX_LEN_TEACHER_CODE+MAX_LEN_STUDENT_CODE) :: tAction
        logical :: changeAdvisers
        character(len=MAX_LEN_XML_LINE) :: mesg

        call html_comment('student_advisers_by_teacher()')
        nSteps = 25

        ! which teacher
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, ierr)
        targetTeacher = index_to_teacher(tTeacher)
        targetCollege = Department(Teacher(targetTeacher)%DeptIdx)%CollegeIdx

        ! range
        call cgi_get_named_integer(QUERY_STRING, 'A2', first, ierr)
        call cgi_get_named_integer(QUERY_STRING, 'A3', last, ierr)

        ! action
        call cgi_get_named_string(QUERY_STRING, 'action'//trim(itoa(first)), tAction, ierr)
        changeAdvisers = ierr==0 .and. trim(tAction)=='Submit'
        mesg = SPACE

        ! collect students
        n_count = 0
        do tdx=1,NumStudents+NumAdditionalStudents
            std = StdRank(tdx)
            if (Student(std)%Adviser/=tTeacher) cycle
            n_count = n_count+1
            tArray(n_count) = std
        end do

        if (changeAdvisers .and. .not. isRoleOfficial) then

            do tdx=first,last
                std = tArray(tdx)
                tStdNo = Student(std)%StdNo

                call cgi_get_named_string(QUERY_STRING, 'Adviser:'//trim(tStdNo), newAdviser, ierr)
                if (ierr/=0 .or. len_trim(newAdviser)==0) then ! delete
                    idx = index_to_teacher(Student(std)%Adviser)
                    Teacher(idx)%NumAdvisees = Teacher(idx)%NumAdvisees - 1
                    Student(std)%Adviser = SPACE
                    mesg = ' : '//trim(tStdNo)//mesg
                else
                    if (Student(std)%Adviser/=newAdviser) then
                        Student(std)%Adviser = newAdviser
                        idx = index_to_teacher(newAdviser)
                        Teacher(idx)%NumAdvisees = Teacher(idx)%NumAdvisees + 1
                        mesg = ' : '//trim(tStdNo)//mesg
                        call xml_read_student_info(std, ierr)
                        StudentInfo%Adviser = newAdviser
                        call xml_student_info(std)
                        call log_student_record_change(std, 'Adviser is '//newAdviser)
                    end if
                end if
            end do

            if (mesg/=SPACE) then
                mesg = 'Changed adviser '//mesg
                call xml_write_students(pathToYear, Student(std)%CurriculumIdx)
            else
                mesg = 'No adviser changes?'
            end if
        end if

        if (changeAdvisers .and. isRoleOfficial) then
            mesg = sorryMessage
        end if

        call change_adviser_form(device, n_count, nSteps, tArray(1:n_count), &
            fnAdvisersByTeacher, tTeacher, 'Advisees of '//Teacher(targetTeacher)%Name, mesg)

    end subroutine student_advisers_by_teacher



    subroutine student_advisers_by_curriculum (device)
        integer, intent (in) :: device

        integer :: n_count, tdx, std, ierr, nSteps, first, last, idx
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character(len=MAX_LEN_TEACHER_CODE) :: newAdviser
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character(len=MAX_LEN_TEACHER_CODE+MAX_LEN_STUDENT_CODE) :: tAction
        logical :: changeAdvisers
        character(len=MAX_LEN_XML_LINE) :: mesg

        call html_comment('student_advisers_by_curriculum()')
        nSteps = 25

        ! which curriculum
        call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
        targetCurriculum = index_to_curriculum(tCurriculum)
        targetCollege = Curriculum(targetCurriculum)%CollegeIdx

        ! range
        call cgi_get_named_integer(QUERY_STRING, 'A2', first, ierr)
        call cgi_get_named_integer(QUERY_STRING, 'A3', last, ierr)

        ! action
        call cgi_get_named_string(QUERY_STRING, 'action'//trim(itoa(first)), tAction, ierr)
        changeAdvisers = ierr==0 .and. trim(tAction)=='Submit'
        mesg = SPACE

        ! collect students
        n_count = 0
        do tdx=1,NumStudents+NumAdditionalStudents
            std = StdRank(tdx)
            if (Student(std)%CurriculumIdx/=targetCurriculum) cycle
            n_count = n_count+1
            tArray(n_count) = std
        end do

        if (changeAdvisers .and. .not. isRoleOfficial) then

            do tdx=first,last
                std = tArray(tdx)
                tStdNo = Student(std)%StdNo

                call cgi_get_named_string(QUERY_STRING, 'Adviser:'//trim(tStdNo), newAdviser, ierr)
                if (ierr/=0 .or. len_trim(newAdviser)==0) then ! delete
                    idx = index_to_teacher(Student(std)%Adviser)
                    Teacher(idx)%NumAdvisees = Teacher(idx)%NumAdvisees - 1
                    Student(std)%Adviser = SPACE
                    mesg = ' : '//trim(tStdNo)//mesg
                else
                    if (Student(std)%Adviser/=newAdviser) then
                        Student(std)%Adviser = newAdviser
                        idx = index_to_teacher(newAdviser)
                        Teacher(idx)%NumAdvisees = Teacher(idx)%NumAdvisees - 1
                        mesg = ' : '//trim(tStdNo)//mesg
                        call xml_read_student_info(std, ierr)
                        StudentInfo%Adviser = newAdviser
                        call xml_student_info(std)
                        call log_student_record_change(std, 'Adviser is '//newAdviser)
                    end if
                end if
            end do

            if (mesg/=SPACE) then
                mesg = 'Changed adviser '//mesg
                call xml_write_students(pathToYear, Student(std)%CurriculumIdx)
            else
                mesg = 'No adviser changes?'
            end if
        end if

        if (changeAdvisers .and. isRoleOfficial) then
            mesg = sorryMessage
        end if

        call change_adviser_form(device, n_count, nSteps, tArray(1:n_count), &
            fnAdvisersByCurriculum, tCurriculum, 'Advisers of students in '//tCurriculum, mesg)

    end subroutine student_advisers_by_curriculum


    subroutine links_to_students (device, fn, thisTerm, NumSections, Section, eList)

        integer, intent (in) :: device, fn
        integer, intent (in), optional :: thisTerm, NumSections
        type (TYPE_SECTION), intent(in), optional :: Section(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in), optional :: eList(0:)

        integer :: ldx, n_count, tdx, std, ierr, ncol, Term
        integer, dimension(60,6) :: TimeTable
        logical :: flagIsUp
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character(len=127) :: header

        call html_comment('links_to_students()')

        if (present(thisTerm)) then
            Term = thisTerm
        else
            Term = currentTerm
        end if

        ! collect students
        n_count = 0
        select case (fn)

            case (fnFindStudent)
                call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
                ncol = len_trim(tCurriculum)
                if (ncol>0) then
                    do tdx=1,NumStudents+NumAdditionalStudents
                        std = StdRank(tdx)
                        ldx = index(Student(std)%StdNo, tCurriculum(:ncol))
                        if (ldx/=0 ) then
                            n_count = n_count+1
                            tArray(n_count) = std
                        else
                            ldx = index(Student(std)%Name, tCurriculum(:ncol))
                            if (ldx==0 ) then
                                call upper_case(tCurriculum)
                                ldx = index(Student(std)%Name, tCurriculum(:ncol))
                                if (ldx==0 ) cycle
                            end if
                            n_count = n_count+1
                            tArray(n_count) = std
                        end if
                    end do
                    header = 'Students with "'//tCurriculum(:ncol)//'" in name or number.'
                else
                    header = 'Search student failed: search string not specified.'
                end if

            case (fnOnlineStudents)
                do tdx=1,NumStudents+NumAdditionalStudents
                    std = StdRank(tdx)
                    if (Student(std)%OnlineStatus==0) cycle
                    n_count = n_count+1
                    tArray(n_count) = std
                end do
                header = 'Online students'

            case (fnStudentsByCurriculum)
                ! which Curriculum ?
                call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
                targetCurriculum = index_to_curriculum(tCurriculum)
                do tdx=1,NumStudents+NumAdditionalStudents
                    std = StdRank(tdx)
                    if (Student(std)%CurriculumIdx /= targetCurriculum) cycle
                    n_count = n_count+1
                    tArray(n_count) = std
                end do
                header = 'Students in '//tCurriculum

            case (fnStudentsWithConflicts)
                ! which college
                call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
                targetCollege = index_to_college(tCollege)
                do tdx=1,NumStudents+NumAdditionalStudents
                    std = StdRank(tdx)
                    if (Curriculum(Student(std)%CurriculumIdx)%CollegeIdx /= targetCollege) cycle
                    ! collect classes for student
                    call timetable_meetings_of_student(NumSections, Section, std, eList, 0, &
                        ldx, tArray(n_count+1:), TimeTable, flagIsUp)
                    if (flagIsUp) then ! conflict
                        n_count = n_count + 1
                        tArray(n_count) = std
                    end if
                end do
                header = trim(tCollege)//' students with schedule conflicts, '


            case (fnStudentPriority)

                ! priority
                call cgi_get_named_integer(QUERY_STRING, 'A2', ncol, ierr)

                ! which group
                call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)

                if (trim(tCollege)=='ADVISEES') then
                    tCurriculum = 'My advisees'
                    do tdx=1,NumStudents+NumAdditionalStudents
                        std = StdRank(tdx)
                        if (Student(std)%Adviser /= USERNAME) cycle
                        flagIsUp = .false.
                        select case (ncol)
                            case (20)
                                flagIsUp = eList(std)%errNSTP/=0
                            case default
                                flagIsUp = eList(std)%StdPriority==ncol
                        end select
                        if (flagIsUp) then ! include
                            n_count = n_count + 1
                            tArray(n_count) = std
                        end if
                    end do

                else if (trim(tCollege)=='ALL') then
                    tCurriculum = 'ALL students'
                    do tdx=1,NumStudents+NumAdditionalStudents
                        std = StdRank(tdx)
                        flagIsUp = .false.
                        select case (ncol)
                            case (20)
                                flagIsUp = eList(std)%errNSTP/=0
                            case default
                                flagIsUp = eList(std)%StdPriority==ncol
                        end select
                        if (flagIsUp) then ! include
                            n_count = n_count + 1
                            tArray(n_count) = std
                        end if
                    end do

                else ! specific college
                    tCurriculum = trim(tCollege)//' students'
                    targetCollege = index_to_college(tCollege)
                    do tdx=1,NumStudents+NumAdditionalStudents
                        std = StdRank(tdx)
                        if (Curriculum(Student(std)%CurriculumIdx)%CollegeIdx /= targetCollege) cycle
                        flagIsUp = .false.
                        select case (ncol)
                            case (20)
                                flagIsUp = eList(std)%errNSTP/=0
                            case default
                                flagIsUp = eList(std)%StdPriority==ncol
                        end select
                        if (flagIsUp) then ! include
                            n_count = n_count + 1
                            tArray(n_count) = std
                        end if
                    end do
                end if

                select case (ncol)
                    case (20)
                        header = ' with NSTP 11/12 track mismatch'
                    case (10)
                        header = ' with no subjects remaining'
                    case (2)
                        header = ' with 27 or less units remaining'
                    case (9)
                        header = ' with no feasible subjects but have units remaining'
                    case (6)
                        header = ' who failed (75-100%] of units last term'
                    case (5)
                        header = ' who failed (50-75%] of units last term'
                    case (4)
                        header = ' who failed (0-50%] of units last term'
                    case (3)
                        header = ' who did not fail any subject (or were on LOA) last term'

                    case default
                        header = ' (criterion not specified)'
                end select
                header = trim(tCurriculum)//header

        end select

        call html_write_header(device, header)
        call html_student_list (device, n_count, tArray, .true.)
        write(device,AFORMAT) horizontal

    end subroutine links_to_students


    subroutine recent_student_activity(device)

        integer, intent(in) :: device
        integer :: idxCurr, iTmp
        character (len=MAX_LEN_FILE_PATH) :: logFile
        logical :: logExists

        call html_comment('recent_student_activity()')

        ! which student ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, idxCURR)
        targetStudent = index_to_student(tStdNo)
        idxCurr = Student(targetStudent)%CurriculumIdx
        call html_write_header(device, 'Recent activity of '//Student(targetStudent)%StdNo//nbsp// &
            trim(Student(targetStudent)%Name)// &
            trim(make_href(fnCurriculum, Curriculum(idxCurr)%Code, A1=Curriculum(idxCurr)%Code, &
            pre=beginsmall//' (', post=' ) '//endsmall)) )

        iTmp = year_prefix(Student(targetStudent))
        logFile = trim(dirLOG)// &
            trim(Student(targetStudent)%StdNo(1:iTmp))//DIRSEP// &
            trim(Student(targetStudent)%StdNo)//'.log'
        inquire(file=logFile, exist=logExists)
        if (logExists) then
            call copy_to_unit(logFile, device)
        else
            write(device,AFORMAT) BRNONEHR
        end if

    end subroutine recent_student_activity


    subroutine change_adviser_form(device, n_count, nSteps, tArray, fn, label, header, mesg)
        integer, intent (in) :: device, fn, n_count, nSteps, tArray(1:n_count)
        character(len=*), intent (in) :: label, header, mesg

        integer :: ldx, tdx, std, j, k, l, first, last, skip, nAdvised(3), nClasses(3), Term
        logical :: allowed_to_edit

        call html_write_header(device, header, mesg)

        if (n_count == 0) then
            write(device,AFORMAT) BRNONEHR
            return
        end if

        allowed_to_edit = is_dean_of_college(targetCollege, orHigherUp)
        if ( allowed_to_edit ) then
            skip = nSteps
        else
            skip = n_count
        end if

        write(device,AFORMAT) '<table border="0" width="100%">'

        do k=1, n_count, skip
            first = k
            last = min(first+skip-1, n_count)

            if ( allowed_to_edit ) then
                call make_form_start(device, fn, label, itoa(first), itoa(last))
            end if

            write(device,AFORMAT) &
                begintr//thalignleft//'#'//endth, &
                thalignleft//'STDNO'//endth, &
                thalignleft//'Name of Student'//endth, &
                thalignleft//'Curriculum'//endth, &
                thalignleft//'Adviser'//endth//endtr

            do tdx=first,last
                std = tArray(tdx)
                tStdNo = Student(std)%StdNo
                ldx = Student(std)%CurriculumIdx

                call count_preenlistment(std, 0, nClasses, nAdvised)

                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(tdx,2))//'">'// &
                    begintd//trim(itoa(tdx))//DOT//endtd//begintd//tStdNo//endtd, &
                    begintd//trim(Student(std)%Name)//endtd//begintd//Curriculum(ldx)%Code//begintd

                if ( allowed_to_edit ) then
                    write(device,AFORMAT) '<select name="Adviser:'//trim(tStdNo)//'">', &
                         '<option value=""> (select)'
                    do l=1,NumTeachers+NumAdditionalTeachers
                        if (Teacher(l)%TeacherId==Student(std)%Adviser) then
                            j = 1
                        else
                            if (Department(Teacher(l)%DeptIdx)%CollegeIdx/=Curriculum(ldx)%CollegeIdx) cycle
                            j = 0
                        end if
                        write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(Teacher(l)%TeacherId)//'"> '// &
                            trim(Teacher(l)%Name)
                    end do
                    write(device,AFORMAT) '</select>'//endtd
                else
                    l = index_to_teacher(Student(std)%Adviser)
                    write(device,AFORMAT) trim(Teacher(l)%Name)//endtd
                end if

                if ( allowed_to_edit .or. (USERNAME == Student(std)%Adviser) ) then

                    write(device,AFORMAT) begintd//beginsmall
                    if ( allowed_to_edit ) then
                        write(device,AFORMAT) trim(make_href(fnStudentEdit, 'Info', A1=tStdNo, pre=nbsp))
                    end if
                    write(device,AFORMAT) &
                        trim(make_href(fnRecentStudentActivity, 'Log', A1=tStdNo, pre=nbsp)), &
                        trim(make_href(fnStudentGrades, 'Grades', A1=tStdNo, pre=nbsp)), &
                        trim(make_href(fnEditCheckList, 'Checklist', A1=tStdNo, pre=nbsp))

                    if (sum(nClasses)>0) then
                        do l=termBegin,termEnd
                            call qualify_term (l, j, Term)
                            if (nClasses(Term)+nAdvised(Term)>0) write(device,AFORMAT) &
                                trim(make_href(fnStudentClasses, txtSemester(Term+6), A1=tStdNo, A9=Term, pre=nbsp))
                        end do
                    end if

                end if

                write(device,AFORMAT) endsmall//endtd//endtr

            end do

            if ( allowed_to_edit ) then
                write(device,AFORMAT) &
                    begintr//'<td colspan="4">'//nbsp//endtd// &
                    begintd//linebreak//'<input type="submit" name="action'//trim(itoa(first))// &
                    '" value="Submit">'//endform//linebreak//linebreak//endtd//tdnbspendtd//endtr
            end if

        end do

        write(device,AFORMAT) endtable

        write(device,AFORMAT) horizontal

    end subroutine change_adviser_form



    subroutine grade_certification(device)
        integer, intent (in) :: device

        character (len=MAX_LEN_PERSON_NAME) :: tStdName
        integer :: tdx, grd, crse, ierr, i, j, Year, Term

        ! which student?
        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
        call cgi_get_named_integer(QUERY_STRING, 'A2', Year, ierr)
        call cgi_get_named_integer(QUERY_STRING, 'A3', Term, ierr)

        targetStudent = index_to_student(tStdNo)
        targetCurriculum = Student(targetStudent)%CurriculumIdx
        targetCollege = Curriculum(targetCurriculum)%CollegeIdx
        i = index(Student(targetStudent)%Name, COMMA)
        j = len_trim(Student(targetStudent)%Name)
        if (i>0) then
            tStdName = Student(targetStudent)%Name(i+1:j)//SPACE//Student(targetStudent)%Name(1:i-1)
        else
            tStdName = Student(targetStudent)%Name
        end if

        call html_comment('grade certification - '//tStdName)

        ! read checklist
        call read_student_records (targetStudent)

        write(device,AFORMAT) &
            '<html><head><title>'//trim(UniversityCode)//SPACE//PROGNAME//VERSION// &
            '</title></head><body>', '<table border="0" width="100%">', &
            begintr//tdaligncenter//'Republic of the Philippines'//endtd//endtr, &
            begintr//tdaligncenter//trim(UniversityName)//endtd//endtr, &
            begintr//tdaligncenter//trim(UniversityAddress)//endtd//endtr, &
            begintr//begintd//linebreak//endtd//endtr, &
            begintr//tdaligncenter//beginbold//'CERTIFICATION OF GRADES'//endbold//endtd//endtr, &
            endtable

        write(device,AFORMAT) linebreak//linebreak//linebreak//'<table border="0" width="100%">', &
            begintr, &
            '<td width="10%">'//nbsp//endtd, &
            '<td width="80%" colspan="4">'// &
                'TO WHOM IT MAY CONCERN:'//linebreak//linebreak//linebreak// &
                'This is to certify that '//trim(tStdName)// &
                ' was enrolled at '//trim(UniversityName)// &
                ' during the '//trim(txtSemester(Term+3))//trim(termQualifier(Term+3))// &
                ' of Academic Year '//trim(itoa(Year))//DASH//trim(itoa(Year+1))//COMMA// &
                ' in the subjects listed below.'//linebreak//linebreak//endtd, &
            '<td width="10%">'//nbsp//endtd, &
            endtr, &
            begintr, &
            '<td width="10%">'//nbsp//endtd, &
            '<td width="80%" colspan="4">'//horizontal// &
            '<td width="10%">'//nbsp//endtd, &
            endtr, &
            !
            begintr, &
            '<td width="10%">'//nbsp//endtd, &
            begintd//'SUBJECT'//endtd, &
            begintd//'DESCRIPTIVE TITLE'//endtd, &
            tdaligncenter//'GRADE'//endtd, &
            tdaligncenter//'CREDIT'//endtd, &
            '<td width="10%">'//nbsp//endtd, &
            endtr, &
            begintr, &
            '<td width="10%">'//nbsp//endtd, &
            '<td width="80%" colspan="4">'//horizontal// &
            '<td width="10%">'//nbsp//endtd, &
            endtr

        do tdx=1,lenTCG

            if (TCG(tdx)%Code<2 .or. TCG(tdx)%Code==4 .or. TCG(tdx)%Code==5 .or. &
                TCG(tdx)%Used .or. TCG(tdx)%ErrorCode>1) cycle
            ! ignore 4-removal/5-completion records; already in TCG()%ReExam

            if (TCG(tdx)%Year/=Year .or. TCG(tdx)%Term/=Term) cycle

            crse = TCG(tdx)%Subject
            grd = TCG(tdx)%Grade

            write(device,AFORMAT) &
                begintr, &
                '<td width="10%">'//nbsp//endtd, &
                begintd//trim(Subject(crse)%Name)//endtd, &
                begintd//trim(Subject(crse)%Title)//endtd
            if ( grd==gdxREGD ) then
                write(device,AFORMAT) &
                    tdaligncenter//beginitalic//'No grade'//enditalic//endtd, tdnbspendtd
            else if ( grd==gdxINC .or. grd==gdxNFE) then
                if (is_grade_passing(TCG(tdx)%ReExam)) then
                    write(device,AFORMAT) &
                        tdaligncenter//txtGrade(pGrade(grd))//FSLASH// &
                                       txtGrade(pGrade(TCG(tdx)%ReExam))//endtd, &
                        tdaligncenter//trim(ftoa(Subject(crse)%Units,1))//endtd
                else
                    write(device,AFORMAT) &
                        tdaligncenter//txtGrade(pGrade(grd))//endtd, tdnbspendtd
                end if
            else if ( is_grade_passing(grd) ) then
                write(device,AFORMAT) &
                    tdaligncenter//txtGrade(pGrade(grd))//endtd, &
                    tdaligncenter//trim(ftoa(Subject(crse)%Units,1))//endtd
            else
                write(device,AFORMAT) &
                    tdaligncenter//txtGrade(pGrade(grd))//endtd, tdnbspendtd
            end if
            write(device,AFORMAT) &
                '<td width="10%">'//nbsp//endtd, &
                endtr

        end do

        write(device,AFORMAT) &
            begintr, &
            '<td width="10%">'//nbsp//endtd, &
            '<td width="80%" colspan="4">----- '//beginitalic//'Nothing follows'//enditalic//' -----'// &
            '<td width="10%">'//nbsp//endtd, &
            endtr, &
            begintr, &
            '<td width="10%">'//nbsp//endtd, &
            '<td width="80%" colspan="4">'//horizontal// &
            '<td width="10%">'//nbsp//endtd, &
            endtr, &
            endtable

        write(device,AFORMAT) linebreak//'<table border="0" width="100%">', &
            begintr, &
            '<td width="10%">'//nbsp//endtd, &
            '<td width="80%">'// &
                'This certification is issued '//txtMonth(atoi(currentDate(5:6)))//currentDate(7:8)//COMMA// &
                SPACE//currentDate(1:4)//' for reference purposes; ', &
                'it is not valid without the seal of the University or the original signature of the University Registrar.'// &
                endtd, &
            '<td width="10%">'//nbsp//endtd, &
            endtr, &
            endtable

        write(device,AFORMAT) linebreak//linebreak//linebreak//linebreak//'<table border="0" width="100%">', &
            begintr, &
            '<td width="30%">'//nbsp//endtd, &
            '<td width="30%">'//nbsp//endtd, &
            '<td width="40%">'//trim(TheRegistrar)//linebreak//trim(titleTheRegistrar)//endtd, &
            endtr, &
            endtable

    end subroutine grade_certification


    subroutine write_markings (device, nLines, pageNo, nPages)
        integer, intent (in) :: device, nLines, pageNo, nPages
        integer :: idx

        write(device,AFORMAT) '<table width="100%" frame="lhs">', &
            begintr//'<td align="center" colspan="3">OFFICIAL MARKS'//endtd//endtr, & ! 1
            begintr//'<td colspan="3">'//horizontal//endtd//endtr, & ! 2
            begintr//tdnbspendtd//tdnbspendtd//tdnbspendtd//endtr, & ! 3
            begintr//tdalignright//'Percentage'//endtd//tdnbspendtd//tdaligncenter//'Numeric'//endtd//endtr, & ! 4
            begintr//tdalignright//'Equivalent'//endtd//tdnbspendtd//tdaligncenter//'Equivalent'//endtd//endtr, & ! 5
            begintr//tdnbspendtd//tdnbspendtd//tdnbspendtd//endtr, & ! 6
            begintr//tdalignright//'97 - 100'//endtd//tdnbspendtd//tdaligncenter//'1.00'//endtd//endtr, & ! 7
            begintr//tdalignright//'94 - 96'//endtd//tdnbspendtd//tdaligncenter//'1.25'//endtd//endtr, & ! 8
            begintr//tdalignright//'91 - 93'//endtd//tdnbspendtd//tdaligncenter//'1.50'//endtd//endtr, & ! 9
            begintr//tdalignright//'88 - 90'//endtd//tdnbspendtd//tdaligncenter//'1.75'//endtd//endtr, & ! 10
            begintr//tdalignright//'85 - 87'//endtd//tdnbspendtd//tdaligncenter//'2.00'//endtd//endtr, & ! 11
            begintr//tdalignright//'82 - 84'//endtd//tdnbspendtd//tdaligncenter//'2.25'//endtd//endtr, & ! 12
            begintr//tdalignright//'79 - 81'//endtd//tdnbspendtd//tdaligncenter//'2.50'//endtd//endtr, & ! 13
            begintr//tdalignright//'76 - 78'//endtd//tdnbspendtd//tdaligncenter//'2.75'//endtd//endtr, & ! 14
            begintr//tdalignright//'75'//endtd//tdnbspendtd//tdaligncenter//'3.00'//endtd//endtr, & ! 15
            begintr//tdalignright//'Below 75'//endtd//tdnbspendtd//tdaligncenter//'5.00'//endtd//endtr, & ! 16
            begintr//tdnbspendtd//tdnbspendtd//tdnbspendtd//endtr ! 17


        write(device,AFORMAT) &
            begintr//'<td colspan="3">'//horizontal//endtd//endtr, & ! 18
            begintr//'<td align="center" colspan="3">GENERAL CLASSIFICATION'//endtd//endtr, & ! 19
            begintr//'<td colspan="3">'//horizontal//endtd//endtr, & ! 20
            begintr//tdnbspendtd//tdnbspendtd//tdnbspendtd//endtr, & ! 21
            begintr//tdalignright//'1.00'//endtd//begintd//' - '//endtd//begintd//'Excellent'//endtd//endtr, & ! 22
            begintr//tdalignright//'1.25'//endtd//begintd//' - '//endtd//begintd//'Very Outstanding'//endtd//endtr, & ! 23
            begintr//tdalignright//'1.50'//endtd//begintd//' - '//endtd//begintd//'Outstanding'//endtd//endtr, & ! 24
            begintr//tdalignright//'1.75'//endtd//begintd//' - '//endtd//begintd//'Very Good'//endtd//endtr, & ! 25
            begintr//tdalignright//'2.00'//endtd//begintd//' - '//endtd//begintd//'Good'//endtd//endtr, & ! 26
            begintr//tdalignright//'2.25'//endtd//begintd//' - '//endtd//begintd//'Very Satisfactory'//endtd//endtr, & ! 27
            begintr//tdalignright//'2.50'//endtd//begintd//' - '//endtd//begintd//'Satisfactory'//endtd//endtr, & ! 28
            begintr//tdalignright//'2.75'//endtd//begintd//' - '//endtd//begintd//'Fair'//endtd//endtr, & !28
            begintr//tdalignright//'3.00'//endtd//begintd//' - '//endtd//begintd//'Passing'//endtd//endtr, & ! 29
            begintr//tdalignright//'5.00'//endtd//begintd//' - '//endtd//begintd//'Failing'//endtd//endtr, & ! 30
            begintr//tdnbspendtd//tdnbspendtd//tdnbspendtd//endtr ! 31

        write(device,AFORMAT) &
            begintr//'<td colspan="3">'//horizontal//endtd//endtr, & ! 32
            begintr//'<td align="center" colspan="3">SUPPLEMENTARY MARKS'//endtd//endtr, & ! 33
            begintr//'<td colspan="3">'//horizontal//endtd//endtr, & ! 34
            begintr//tdnbspendtd//tdnbspendtd//tdnbspendtd//endtr, & ! 35
            begintr//tdalignright//'XFR'//endtd//begintd//' - '//endtd//begintd//'Transfer credit'//endtd//endtr, & ! 36
            begintr//tdalignright//'NFE'//endtd//begintd//' - '//endtd//begintd//'No final exam'//endtd//endtr, & ! 37
            begintr//tdalignright//'INC'//endtd//begintd//' - '//endtd//begintd//'Incomplete'//endtd//endtr, & ! 38
            begintr//tdalignright//'DRP'//endtd//begintd//' - '//endtd//begintd//'Dropped'//endtd//endtr, & ! 39
            begintr//tdalignright//'P'//endtd//begintd//' - '//endtd//begintd//'Passed'//endtd//endtr, & ! 40
            begintr//tdalignright//'F'//endtd//begintd//' - '//endtd//begintd//'Failed'//endtd//endtr, & ! 41
            begintr//tdnbspendtd//tdnbspendtd//tdnbspendtd//endtr, & ! 42
            begintr//'<td colspan="3">'//horizontal//endtd//endtr ! 43

        do idx=44,nLines-2
            write(device,AFORMAT) begintr//tdnbspendtd//tdnbspendtd//tdnbspendtd//endtr
        end do
        write(device,AFORMAT) &
            begintr//'<td colspan="3">'//horizontal//endtd//endtr, &
            begintr//'<td align="center" colspan="3">Page '// trim(itoa(pageNo))//' of '//trim(itoa(nPages))//endtd//endtr, &
            '</table>'

    end subroutine write_markings


    subroutine write_transcript_header(device, nLines)
        integer, intent (in) :: device
        integer, intent (out) :: nLines

        integer :: idx
        character (len=6) :: gender

        idx = year_prefix(Student(targetStudent))
        line = '/img/id/'//trim(Student(targetStudent)%StdNo(1:idx))//FSLASH//trim(Student(targetStudent)%StdNo)//'.jpg'
        if (StudentInfo%Gender=='M') then
            gender = 'Male'
        else
            gender = 'Female'
        end if

        write(device,AFORMAT) '<table border="0" width="100%">', &
            begintr, &
            '<td width="25%" align="left" valign="middle">', &
                '<img src="/img/logo.jpg" alt="/img/logo.jpg" height="90" width="90">', &
            endtd
        write(device,AFORMAT) &
            '<td width="50%" align="center">Republic of the Philippines'//linebreak, &     ! line 1
                '<font size="5">'//trim(UniversityName)//'</font>'//linebreak, &           ! line 2
                '<font size="4">OFFICE OF THE UNIVERSITY REGISTRAR</font>'//linebreak, &   ! line 3
                trim(UniversityAddress)//linebreak, &                                      ! line 4
                trim(UniversityPhone)//linebreak, &                                        ! line 5
                trim(UniversityWeb)//linebreak, &                                          ! line 6
                '<font size="4">OFFICIAL TRANSCRIPT OF RECORDS</font>', &                  ! line 7
            endtd
        write(device,AFORMAT) &
            '<td width="25%" align="right" valign="middle">', &
                '<img src="'//trim(line)//'" alt="'//trim(line)//'" height="90" width="90">', &
            endtd, &
            endtr, &
            endtable


        write(device,AFORMAT) &
            '<table width="100%">', &
            begintr//'<td colspan="6" width="100%">'//horizontal//'</td>'//endtr, &        ! line 8
            begintr, &                                                                     ! line 9
            '<td colspan="3" width="50%">Name: '//underline//trim(StudentInfo%Name)//endunderline//endtd, &
            '<td colspan="3" width="50%">ID No.: '//underline//trim(StudentInfo%StdNo)//endunderline//endtd, &
            endtr, &
            begintr, &                                                                     ! line 10
            '<td colspan="3" width="50%">Home Address: '//underline//trim(StudentInfo%HomeAddress)//endunderline//endtd, &
            '<td colspan="3" width="50%">Gender: '//underline//trim(gender)//endunderline//endtd, &
            endtr, &
            begintr, &                                                                     ! line 11
            '<td colspan="3" width="50%">Place of Birth: '//underline//trim(StudentInfo%BirthPlace)//endunderline//endtd, &
            '<td colspan="3" width="50%">Date of Birth: '//underline//trim(StudentInfo%BirthDate)//endunderline//endtd, &
            endtr, &
            begintr, &                                                                     ! line 12
            '<td colspan="6" width="100%">Last School Attended: '//underline//trim(StudentInfo%LastAttended)// &
                endunderline//endtd, &
            endtr, &
            begintr, &                                                                     ! line 13
            '<td colspan="6" width="100%">Degree/Course: '//underline//trim(Curriculum(targetCurriculum)%Title)
        if (len_trim(Curriculum(targetCurriculum)%Specialization)>0) then
            write(device,AFORMAT) nbsp//trim(Curriculum(targetCurriculum)%Specialization)
        end if
        write(device,AFORMAT) &
                endunderline//endtd, &
            endtr, &
            begintr, &                                                                     ! line 14
            '<td colspan="2" width="34%">Date of Graduation: '//underline//trim(StudentInfo%GraduationDate)//endunderline//endtd, &
            '<td colspan="2" width="33%">Admission Data: '//underline//trim(StudentInfo%AdmissionData)//endunderline//endtd, &
            '<td colspan="2" width="33%">Admission Date: '//underline//trim(StudentInfo%EntryDate)//endunderline//endtd, &
            endtr, &
            endtable

        ! the column headers
        write(device,AFORMAT) '<table width="100%">', &
            begintr//'<td colspan="10">'//horizontal//endtd//endtr !, &                     ! line 15
        nLines = 15

    end subroutine write_transcript_header


    subroutine write_transcript_footer(device, coll)

        integer, intent (in) :: device, coll

        write(device,AFORMAT) endtable, &
            horizontal, 'Remarks: '//trim(StudentInfo%TranscriptRemark), horizontal, &
            '<table width="100%">', &
            begintr//'<td width="33%" align="left">Prepared by:'//endtd, &
                     '<td width="33%" align="left">Checked by:'//endtd, &
                     '<td width="34%" align="left">'//nbsp//endtd//endtr
        write(device,AFORMAT) &
            begintr//'<td width="33%" align="center">'//linebreak// &
                    underline//trim(College(coll)%TranscriptPreparer)//endunderline, &
                     linebreak//'Record Custodian'//endtd, &
                     '<td width="33%" align="center">'//linebreak//underline// &
                     trim(College(coll)%TranscriptChecker)//endunderline, &
                     linebreak//'Record Custodian'//endtd, &
                     '<td width="34%" align="center">'//linebreak//endtd//endtr, &
            begintr//'<td width="66%" colspan="2">'//horizontal//endtd//&
                     tdaligncenter//underline//trim(TheRegistrar)//endunderline//endtd//endtr, &
            begintr//'<td width="33%" align="left">Released by:'//endtd, &
                     '<td width="33%" align="left">Date Released:'//endtd, &
                     '<td width="34%" align="center">'//trim(titleTheRegistrar)//endtd//endtr, &
            endtable//horizontal, &
            beginsmall//beginitalic//'This transcript is not valid without the seal of the University or '// &
            ' the original signature of the University Registrar.'//enditalic//endsmall

    end subroutine write_transcript_footer


    subroutine student_transcript(device)
        integer, intent (in) :: device

        integer :: idx, tdx, grd, prevtaken, crse, ierr, lineNo
        integer :: nLines, nParts, ptrPart(30), pageNo, nPages, ptrPage(5)
        character(len=255) :: trLine(MAX_SUBJECTS_IN_CURRICULUM)

        ! which student?
        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
        targetStudent = index_to_student(tStdNo)
        targetCurriculum = Student(targetStudent)%CurriculumIdx
        targetCollege = Curriculum(targetCurriculum)%CollegeIdx

        call html_comment('Transcript of '//tStdNo)

        ! read checklist
        call read_student_records (targetStudent)

        ! generate transcript line-by-line
        nLines = 0
        trLine = SPACE
        nParts = 0
        ptrPart = 0

        ! the advance credits (PASSed subjects)
        prevtaken = 0
        do tdx=1,lenTCG
            TCG(tdx)%Used = .false.
            if (TCG(tdx)%Code==2 .and. is_grade_passing(TCG(tdx)%Grade)) prevtaken = prevtaken+1
        end do
        if (prevtaken>0) then ! there are advance credits

            nLines = nLines + 1
            trLine(nLines) = &
                '<td colspan="5" align="left">'//beginbold//'TRANSFER CREDITS'//endbold//endtd// &
                tdnbspendtd//tdnbspendtd
            nParts = nParts + 1
            ptrPart(nParts) = nLines

            do tdx=1,lenTCG

                if (TCG(tdx)%Code/=2 .or. .not. is_grade_passing(TCG(tdx)%Grade)) cycle

                crse = TCG(tdx)%Subject
                grd = TCG(tdx)%Grade
                tSubject = Subject(crse)%Name
                line = begintd//trim(tSubject)//endtd// &
                    '<td colspan="4">'//trim(Subject(crse)%Title)//endtd// &
                    tdaligncenter//'XFR'//endtd

                if (Subject(crse)%Units == 0.0 .or. tSubject(1:5)=='NSTP ') then
                    line = trim(line)//tdnbspendtd
                else if (is_grade_numeric_pass(grd)) then
                    line = trim(line)//tdaligncenter//trim(ftoa(Subject(crse)%Units,1))//endtd
                else
                    ! non numeric grade
                    if ( is_grade_passing(grd) ) then
                        line = trim(line)//tdaligncenter//trim(ftoa(Subject(crse)%Units,1))//endtd
                    else
                        line = trim(line)//tdnbspendtd
                    end if
                end if

                nLines = nLines + 1
                trLine(nLines) = line

                TCG(tdx)%Used = .true.

            end do
        end if

        prevtaken = -10
        do tdx=1,lenTCG

            ! FINALGRADES only (with completions if applicable)
            if (TCG(tdx)%Code/=3 .or. TCG(tdx)%Used .or. TCG(tdx)%ErrorCode>1) cycle

            if (prevtaken /= TCG(tdx)%Taken) then
                ! header for next term
                nLines = nLines + 1
                trLine(nLines) = '<td colspan="5" align="left"> '//beginbold// &
                    trim(text_term_school_year(TCG(tdx)%Term, TCG(tdx)%Year))//endbold//endtd// &
                    tdnbspendtd//tdnbspendtd
                nParts = nParts + 1
                ptrPart(nParts) = nLines

                prevtaken = TCG(tdx)%Taken
            end if

            crse = TCG(tdx)%Subject
            grd = TCG(tdx)%Grade
            tSubject = Subject(crse)%Name
            line = begintd//trim(tSubject)//endtd// &
                '<td colspan="4">'//trim(Subject(crse)%Title)//endtd

            if (TCG(tdx)%ReExam/=0) then
                line = trim(line)//tdaligncenter//txtGrade(pGrade(grd))//FSLASH// &
                txtGrade(pGrade(TCG(tdx)%ReExam))//endtd
                grd = TCG(tdx)%ReExam
            else
                line = trim(line)//tdaligncenter//txtGrade(pGrade(grd))//endtd
            end if

            if (Subject(crse)%Units == 0.0 .or. tSubject(1:5)=='NSTP ') then
                line = trim(line)//tdnbspendtd
            else if (is_grade_numeric_pass(grd)) then
                ! numeric pass
                if (Subject(crse)%Units*fGrade(grd)/=0.0) then
                    line = trim(line)//tdaligncenter//trim(ftoa(Subject(crse)%Units,1))//endtd
                else
                    line = trim(line)//tdnbspendtd
                end if
            !else if (grd == gdx5) then
            !    ! 5.0
            !    line = trim(line)//tdnbspendtd
            else
                ! non numeric grade
                if ( is_grade_passing(grd) ) then
                    line = trim(line)//tdaligncenter//trim(ftoa(Subject(crse)%Units,1))//endtd
                else
                    line = trim(line)//tdnbspendtd
                end if
            end if

            nLines = nLines + 1
            trLine(nLines) = line

        end do
        !do tdx=1,nLines
        !    call html_comment(itoa(tdx)//trLine(tdx))
        !end do

        ! ptr to next (non-existent) part
        ptrPart(nParts+1) = nLines+1

        nPages = nLines/51 + 1
        pageNo = 1
        ptrPage(pageNo) = 1
        do tdx=1,nParts
            if (ptrPart(tdx+1)-ptrPage(pageNo) < 51) cycle
            pageNo = pageNo+1
            ptrPage(pageNo) = ptrPart(tdx)
        end do
        ptrPage(pageNo+1) = ptrPart(nParts+1)

        write(device,AFORMAT) &
            '<html><head><title>'//PROGNAME//VERSION//' transcript for '//trim(StudentInfo%Name)// &
            '</title></head><body>'

        do pageNo=1,nPages

            call write_transcript_header(device, nLines)

            write(device,AFORMAT) begintr//'<td colspan="7" width="80%">'

            write(device,AFORMAT) '<table width="100%">', &
                begintr, &
                    begintd//'SUBJECT'//endtd, &
                    '<td colspan="4">DESCRIPTIVE TITLE'//endtd, &
                    tdaligncenter//'GRADE'//endtd, &
                    tdaligncenter//'UNITS'//endtd, &
                endtr, & ! 1
                begintr//'<td colspan="7">'//horizontal//endtd//endtr ! 2
            lineNo = 2
            do idx=ptrPage(pageNo),ptrPage(pageNo+1)-1
                lineNo = lineNo + 1
                write(device,AFORMAT) begintr//trim(trLine(idx))//endtr
            end do
            lineNo = lineNo + 2
            write(device,AFORMAT) &
                begintr//'<td colspan="7">'//horizontal//endtd//endtr, &
                begintr//'<td colspan="7" align="center">'// &
                    beginbold//beginitalic//'No entry below this line'//enditalic//endbold//endtd, &
                endtr
            do idx=lineNo,55
                write(device,AFORMAT) begintr//'<td colspan="7">'//nbsp//endtd//endtr
            end do

            write(device,AFORMAT) '</table>'//endtd//'<td colspan="3" width="20%">'

            call write_markings(device, 55, pageNo, nPages)

            write(device,AFORMAT) endtd//endtr

            ! write footer
            call write_transcript_footer(device, targetCollege)

        end do


    end subroutine student_transcript


end module EditSTUDENT
