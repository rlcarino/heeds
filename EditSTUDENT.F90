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


module EditSTUDENT

    use HTML

    implicit none

    character(len=MAX_LEN_STUDENT_CODE), private :: tStdNo
    character(len=MAX_LEN_SUBJECT_CODE), private :: tSubject
    character(len=MAX_LEN_XML_LINE), private :: line

contains


#include "Advising.F90"


    subroutine get_scholastic_performance (givenYear, givenTerm, UnitsPaid, UnitsDropped, UnitsPassed, Standing)
        integer :: Standing, givenYear, givenTerm
        integer :: gdx, cdx, i
        real :: HoursPaid, HoursDropped, HoursPassed, HoursREGD, tHours
        real :: pctFailed, UnitsPaid, UnitsDropped, UnitsPassed, tUnits, performanceUnits, UnitsREGD

        ! count units registered, passed
        UnitsPaid = 0.0
        UnitsREGD = 0.0
        UnitsDropped = 0.0
        UnitsPassed = 0.0
        HoursPaid = 0.0
        HoursDropped = 0.0
        HoursPassed = 0.0
        HoursREGD = 0.0
        do i=1,lenTCG

            ! Record(i,:) 1=type,2=year,3=term,4=subject,5=grade
            if (TCG(i)%Code/=3) cycle ! not FINALGRADE

            if (TCG(i)%Year/=givenYear .or. &
                TCG(i)%Term/=givenTerm) cycle

            cdx = TCG(i)%Subject
            gdx = TCG(i)%Grade
            if (gdx>0 .and. cdx>0) then
                tUnits = Subject(cdx)%Units
                tHours = Subject(cdx)%LectHours+Subject(cdx)%LabHours
                UnitsPaid = UnitsPaid + tUnits
                HoursPaid = HoursPaid + tHours
                if (gdx==gdxREGD) then
                    ! currently registered
                    UnitsREGD = UnitsREGD + tUnits
                    HoursREGD = HoursREGD + tHours
                    ! assume passed
                    UnitsPassed = UnitsPassed + tUnits
                    HoursPassed = HoursPassed + tHours
                else if (gdx==gdxDRP .or. gdx==gdxLOA) then
                    UnitsDropped = UnitsDropped + tUnits
                    HoursDropped = HoursDropped + tHours
                else if (isGrade_passing(gdx) ) then
                    UnitsPassed = UnitsPassed + tUnits
                    HoursPassed = HoursPassed + tHours
                else if (isGrade_passing(TCG(i)%ReExam) ) then
                    UnitsPassed = UnitsPassed + tUnits
                    HoursPassed = HoursPassed + tHours
                end if
            end if

        end do

        Standing = SCHOLASTIC_STATUS_NOT_SET
        if (HoursPaid==0.0) then ! did not register; on LOA?
            Standing = SCHOLASTIC_NOT_ENROLLED_LAST_SEM
        else if (HoursPaid==HoursDropped) then ! started LOA
            Standing = SCHOLASTIC_ALL_DRP_LAST_SEM
        else if (HoursPaid==HoursREGD) then ! grades not yet available
            Standing = SCHOLASTIC_FAILED_NONE
        else ! student received some grades; calculate standing
            if (UnitsPaid==0.0) then ! only zero-unit subjects were enrolled; use hours
                performanceUnits = HoursPaid - HoursDropped
                pctFailed = (100.0*(performanceUnits-HoursPassed))/max(1.0, performanceUnits)
            else ! non-zero unit subjects were enrolled
                performanceUnits = UnitsPaid - UnitsDropped
                pctFailed = (100.0*(performanceUnits-UnitsPassed))/max(1.0, performanceUnits)
            end if
            ! compute status
            if (HoursPassed==0.0) then
                Standing = SCHOLASTIC_ALL_FAILED_LAST_SEM ! PERMANENTLY DISQUALIFIED
            else if (pctFailed>75.0) then
                Standing = SCHOLASTIC_FAILED_75PCT_PLUS ! DISMISSED
            else if (pctFailed>50.0) then
                Standing = SCHOLASTIC_FAILED_50PCT_PLUS ! PROBATION
            else if (pctFailed>25.0) then
                Standing = SCHOLASTIC_FAILED_25PCT_PLUS ! WARNING
            else  if (pctFailed>0.0) then
                Standing = SCHOLASTIC_FAILED_1SUBJ_PLUS ! SATISFACTORY - failed<=25%
            else
                Standing = SCHOLASTIC_FAILED_NONE ! GOOD - no failures
            end if
        end if

    end subroutine get_scholastic_performance



    subroutine change_current_student_password(device)
        integer, intent(in) :: device

        character (len=MAX_LEN_PASSWD_VAR) :: t0Password, t1Password, t2Password
        integer :: ierr
        logical :: flagIsUp

        call html_comment('change_current_student_password()')

        flagIsUp = .false.
        if (REQUEST==fnChangeStudentPassword) then
            call cgi_get_named_string(QUERY_STRING, 'C', t0Password, ierr)
            if ( len_trim(t0Password)>0 .and. ierr==0) then
                t0Password(17:) = SPACE
                if (isPassword_of_student(requestingStudent,t0Password) ) then
                    UserRequestCheckMessage = 'Change current password.'
                else
                    UserRequestCheckMessage = 'Current password is incorrect.'
                    flagIsUp = .true.
                end if
            else
                UserRequestCheckMessage = ''
                flagIsUp = .true.
            end if
        end if
        if (.not. flagIsUp) then
            call cgi_get_named_string(QUERY_STRING, 'P', t1Password, ierr)
            call cgi_get_named_string(QUERY_STRING, 'R', t2Password, ierr)
            if ( len_trim(t1Password)>0 .and. len_trim(t2Password)>0 ) then
                if ( t1Password==t2Password ) then
                    t1Password(17:) = SPACE
                    if (isPassword_of_student(requestingStudent, t1Password) .or. &
                            Student(requestingStudent)%StdNo==t2Password) then
                        UserRequestCheckMessage = 'New password is not valid.'
                    else
                        call set_password(Student(requestingStudent)%Password, t1Password)
                        isDirtySTUDENTS = .true.
                        call student_details_write(unitXML, dirSTUDENTS, requestingStudent)
                        UserRequestCheckMessage = 'Successfully changed password for '//USERNAME
                        call log_student_record_change(requestingStudent, UserRequestCheckMessage)

                        REQUEST = fnStudentEdit
                        QUERY_STRING = 'T='//currentTime//'&F='//trim(itoa(REQUEST))//'&A1='//trim(USERNAME)//'&'
                        call student_edit_info(device)

                        return
                    end if
                else
                    UserRequestCheckMessage = 'New password and repeat do not match.'
                end if
            else
                UserRequestCheckMessage = 'New password and/or repeat not specified.'
            end if
        end if

        call html_write_header(device, 'Change password for '//trim(USERNAME)//' - must not be the same as Username', &
            UserRequestCheckMessage )


        call make_form_start(device, REQUEST)

        if (REQUEST==fnChangeStudentPassword) write(device,AFORMAT) &
            b_bold//'Old password:'//e_bold//linebreak, &
            '<input size="20" type="password" name="C" value="">', &
            linebreak//linebreak
        write(device,AFORMAT) &
            b_bold//'New password:'//e_bold//linebreak, &
            '<input size="20" type="password" name="P" value="">', &
            linebreak//linebreak, &
            b_bold//'Repeat new password:'//e_bold//linebreak, &
            '<input size="20" type="password" name="R" value="">', &
            linebreak//linebreak, &
            '<input type="submit" value="Update">'//e_form//horizontal

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
        call getPassword_of_student(targetStudent, Password)
        call html_write_header(device, Student(targetStudent)%StdNo//nbsp// &
            trim(Student(targetStudent)%Name)// &
            trim(make_href(fnCurriculum, Curriculum(idxCurr)%Code, A1=Curriculum(idxCurr)%Code, &
            pre=b_small//' (', post=' ) '//e_small)) )
        if ( isRole_admin_of_college(Curriculum(idxCurr)%CollegeIdx) ) then
            write(device,AFORMAT) 'Password is '//nbsp//trim(Password)//  &
                trim(make_href(fnGenerateStudentPassword, 'Reset password', A1=tStdNo, pre=nbsp) )
        end if
        write(device,AFORMAT) horizontal

    end subroutine generate_student_password


    subroutine student_add(device)
        integer, intent (in) :: device

        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        integer :: ierr, iStd
        logical :: criticalErr
        character(len=80) :: mesg

        call html_comment('student_add()')

        ! student supplied?
        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
        if (ierr/=0 .or. tStdNo==SPACE) then
            call html_college_links(device, CollegeIdxUser, 'Add student: student number not specified?')
            return
        end if

        do iStd=1,len_trim(tStdNo)
            if (tStdNo(iStd:iStd)<='9' .and. tStdNo(iStd:iStd)>='0') cycle
            if (tStdNo(iStd:iStd)==DASH) cycle
            if (tStdNo(iStd:iStd)<='Z' .and. tStdNo(iStd:iStd)>='A') cycle
            ierr = ierr+1
        end do
        if (ierr/=0) then
            call html_college_links(device, CollegeIdxUser, 'Add student: '//trim(itoa(ierr))// &
                ' illegal character(s) in student number "'//trim(tStdNo)//'"?')
            return
        end if


        ! already in RAM?
        iStd = index_to_student(tStdNo)
        if (iStd==0) then

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

            iStd = NumStudents+NumAdditionalStudents
            StdRank(iStd) = iStd
            call initialize_student(Student(iStd))
            Student(iStd)%StdNo = tStdNo
            call cgi_get_named_string(QUERY_STRING, 'A2', Student(iStd)%Name, ierr)
            Student(iStd)%CurriculumIdx = NumCurricula
            ! set student number as initial password
            call set_password(Student(iStd)%Password, trim(tStdNo))

            call collect_prefix_years()
            call make_student_directories()

            ! rebuild index
            call student_details_write(unitXML, trim(dirSTUDENTS)//'index', 1, iStd)

            mesg = 'Added student '//tStdNo

        else

            mesg = 'Student "'//tStdNo//'" already on record.'

        end if

        REQUEST = fnStudentEdit
        call student_edit_info(device, iStd, mesg)

    end subroutine student_add


    subroutine student_edit_info(device, iStd, mesg)
        integer, intent (in) :: device
        integer, intent (in), optional :: iStd
        character(len=*), intent (in), optional :: mesg

        type (TYPE_STUDENT_INFO) :: wrkStudent
        character(len=255) :: header, comment
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo, tAction
        character (len=MAX_LEN_PASSWD_VAR) :: Password, disabled
        character(len=MAX_LEN_PSGC) :: tCode
        integer :: idxCURR, j, k, l, ierr
        logical :: reSort, changed, isDirty, ownEdit
        character :: ch
        character(len=20) :: tColor
!        character (len=MAX_LEN_FILE_PATH) :: choicesCurriculum

        ! which student ?
        if (present(iStd)) then
            isDirty = .true.
            targetStudent = iStd
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
            call cgi_get_named_string(QUERY_STRING, 'errmsg', comment, ierr)
        end if

        call xml_read_student_info(targetStudent, ierr)
        if (ierr==0) then ! previously entered
            call student_copy_from_info(Student(targetStudent))
        else ! initialize info file
            call xml_student_info(targetStudent, Student(targetStudent))
        end if

        idxCURR = Student(targetStudent)%CurriculumIdx
        select case (REQUEST)
            case (fnGenerateStudentPassword)
                call set_password(Student(targetStudent)%Password)
                isDirty = .true.
                tAction = 'Password'
                header = 'Edit student info'
                comment = 'Changed password for '//tStdNo
            case (fnSelectPSGC, fnSelectTongue)
                call cgi_get_named_string(QUERY_STRING, 'A2', tCurriculum, ierr) ! HomePCSG, MotherTongue
                call cgi_get_named_string(QUERY_STRING, 'A3', tCode, ierr)
                QUERY_STRING = trim(QUERY_STRING)//'&'//trim(tCurriculum)//'='//tCode
                tAction = 'Update'
            case default
        end select

        wrkStudent = StudentInfo

        call html_comment(tAction//tStdNo//itoa(targetStudent))

        if (trim(tAction)=='Update' .and. .not. isRoleOfficial) then ! collect changes

            header = 'Edit student info'
            comment = SPACE
            changed = .false.

            call cgi_get_named_string(QUERY_STRING, 'Name', wrkStudent%Name, ierr)
            if (ierr/=0) wrkStudent%Name = StudentInfo%Name
            reSort = wrkStudent%Name/=StudentInfo%Name
            if (reSort) then
                changed= .true.
                comment = ': Name'
                call log_student_record_change(targetStudent, 'Name='//wrkStudent%Name)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Gender', wrkStudent%Gender, ierr)
            if (ierr/=0) wrkStudent%Gender = StudentInfo%Gender
            if (wrkStudent%Gender/=StudentInfo%Gender) then
                changed= .true.
                comment = ': Gender '//comment
                call log_student_record_change(targetStudent, 'Gender='//wrkStudent%Gender)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Adviser', wrkStudent%Adviser, ierr)
            if (ierr/=0) wrkStudent%Adviser = StudentInfo%Adviser
            if (wrkStudent%Adviser/=StudentInfo%Adviser) then
                changed= .true.
                comment = ': Adviser '//comment
                call log_student_record_change(targetStudent, 'Adviser='//wrkStudent%Adviser)
            end if

            call cgi_get_named_string(QUERY_STRING, 'BirthDate', wrkStudent%BirthDate, ierr)
            if (ierr/=0) wrkStudent%BirthDate = StudentInfo%BirthDate
            if (wrkStudent%BirthDate/=StudentInfo%BirthDate) then
                changed= .true.
                comment = ': BirthDate '//comment
                call log_student_record_change(targetStudent, 'BirthDate='//wrkStudent%BirthDate)
            end if

            call cgi_get_named_string(QUERY_STRING, 'BirthPlace', wrkStudent%BirthPlace, ierr)
            if (ierr/=0) wrkStudent%BirthPlace = StudentInfo%BirthPlace
            if (wrkStudent%BirthPlace/=StudentInfo%BirthPlace) then
                changed= .true.
                comment = ': BirthPlace '//comment
                call log_student_record_change(targetStudent, 'BirthPlace='//wrkStudent%BirthPlace)
            end if

            call cgi_get_named_string(QUERY_STRING, 'BirthPlacePSGC', wrkStudent%BirthPlacePSGC, ierr)
            if (ierr/=0) wrkStudent%BirthPlacePSGC = StudentInfo%BirthPlacePSGC
            if (len_trim(wrkStudent%BirthPlacePSGC)==0) wrkStudent%BirthPlacePSGC = PSGC(NumPSGC)%Code
            if (wrkStudent%BirthPlacePSGC/=StudentInfo%BirthPlacePSGC) then
                changed= .true.
                comment = ': BirthPlacePSGC '//comment
                call log_student_record_change(targetStudent, 'BirthPlacePSGC='//wrkStudent%BirthPlacePSGC)
            end if

            call cgi_get_named_string(QUERY_STRING, 'EntryDate', wrkStudent%EntryDate, ierr)
            if (ierr/=0) wrkStudent%EntryDate = StudentInfo%EntryDate
            if (wrkStudent%EntryDate/=StudentInfo%EntryDate) then
                changed= .true.
                comment = ': EntryDate '//comment
                call log_student_record_change(targetStudent, 'EntryDate='//wrkStudent%EntryDate)
            end if

            call cgi_get_named_string(QUERY_STRING, 'GraduationDate', wrkStudent%GraduationDate, ierr)
            if (ierr/=0) wrkStudent%GraduationDate = StudentInfo%GraduationDate
            if (wrkStudent%GraduationDate/=StudentInfo%GraduationDate) then
                changed= .true.
                comment = ': GraduationDate '//comment
                call log_student_record_change(targetStudent, 'GraduationDate='//wrkStudent%GraduationDate)
            end if

            call cgi_get_named_string(QUERY_STRING, 'LastAttended', wrkStudent%LastAttended, ierr)
            if (ierr/=0) wrkStudent%LastAttended = StudentInfo%LastAttended
            if (wrkStudent%LastAttended/=StudentInfo%LastAttended) then
                changed= .true.
                comment = ': LastAttended '//comment
                call log_student_record_change(targetStudent, 'LastAttended='//wrkStudent%LastAttended)
            end if

            call cgi_get_named_string(QUERY_STRING, 'TranscriptRemark', wrkStudent%TranscriptRemark, ierr)
            if (ierr/=0) wrkStudent%TranscriptRemark = StudentInfo%TranscriptRemark
            if (wrkStudent%TranscriptRemark/=StudentInfo%TranscriptRemark) then
                changed= .true.
                comment = ': TranscriptRemark '//comment
                call log_student_record_change(targetStudent, 'TranscriptRemark='//wrkStudent%TranscriptRemark)
            end if

            call cgi_get_named_string(QUERY_STRING, 'TranscriptAdditionalRemark', wrkStudent%TranscriptAdditionalRemark, ierr)
            if (ierr/=0) wrkStudent%TranscriptAdditionalRemark = StudentInfo%TranscriptAdditionalRemark
            if (wrkStudent%TranscriptAdditionalRemark/=StudentInfo%TranscriptAdditionalRemark) then
                changed= .true.
                comment = ': TranscriptAdditionalRemark '//comment
                call log_student_record_change(targetStudent, 'TranscriptAdditionalRemark='//wrkStudent%TranscriptAdditionalRemark)
            end if

            call cgi_get_named_string(QUERY_STRING, 'AdmissionData', wrkStudent%AdmissionData, ierr)
            if (ierr/=0) wrkStudent%AdmissionData = StudentInfo%AdmissionData
            if (wrkStudent%AdmissionData/=StudentInfo%AdmissionData) then
                changed= .true.
                comment = ': AdmissionData '//comment
                call log_student_record_change(targetStudent, 'AdmissionData='//wrkStudent%AdmissionData)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Scholarship', wrkStudent%Scholarship, ierr)
            if (ierr/=0) wrkStudent%Scholarship = StudentInfo%Scholarship
            if (wrkStudent%Scholarship/=StudentInfo%Scholarship) then
                changed= .true.
                comment = ': Scholarship '//comment
                call log_student_record_change(targetStudent, 'Scholarship='//wrkStudent%Scholarship)
            end if

            call cgi_get_named_string(QUERY_STRING, 'HomeStreetAddress', wrkStudent%HomeStreetAddress, ierr)
            if (ierr/=0) wrkStudent%HomeStreetAddress = StudentInfo%HomeStreetAddress
            if (wrkStudent%HomeStreetAddress/=StudentInfo%HomeStreetAddress) then
                changed= .true.
                comment = ': HomeStreetAddress '//comment
                call log_student_record_change(targetStudent, 'HomeStreetAddress='//wrkStudent%HomeStreetAddress )
            end if

            call cgi_get_named_string(QUERY_STRING, 'CurriculumIdx', tCurriculum, ierr)
            if (ierr/=0) then
                wrkStudent%CurriculumIdx = StudentInfo%CurriculumIdx
            else
                wrkStudent%CurriculumIdx = index_to_curriculum(tCurriculum)
            end if
            idxCURR = wrkStudent%CurriculumIdx
            if (wrkStudent%CurriculumIdx/=StudentInfo%CurriculumIdx) then
                changed= .true.
                comment = ': Curriculum '//comment
                call log_student_record_change(targetStudent, 'Curriculum='//tCurriculum )
            end if

            call cgi_get_named_string(QUERY_STRING, 'Email', wrkStudent%Email, ierr)
            if (ierr/=0) wrkStudent%Email = StudentInfo%Email
            if (wrkStudent%Email/=StudentInfo%Email) then
                changed= .true.
                comment = ': Email '//comment
                call log_student_record_change(targetStudent, 'Email='//wrkStudent%Email)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Father', wrkStudent%Father, ierr)
            if (ierr/=0) wrkStudent%Father = StudentInfo%Father
            if (wrkStudent%Father/=StudentInfo%Father) then
                changed= .true.
                comment = ': Father '//comment
                call log_student_record_change(targetStudent, 'Father='//wrkStudent%Father)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Mother', wrkStudent%Mother, ierr)
            if (ierr/=0) wrkStudent%Mother = StudentInfo%Mother
            if (wrkStudent%Mother/=StudentInfo%Mother) then
                changed= .true.
                comment = ': Mother '//comment
                call log_student_record_change(targetStudent, 'Mother='//wrkStudent%Mother)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Guardian', wrkStudent%Guardian, ierr)
            if (ierr/=0) wrkStudent%Guardian = StudentInfo%Guardian
            if (wrkStudent%Guardian/=StudentInfo%Guardian) then
                changed= .true.
                comment = ': Guardian '//comment
                call log_student_record_change(targetStudent, 'Guardian='//wrkStudent%Guardian)
            end if

            call cgi_get_named_string(QUERY_STRING, 'HomePSGC', wrkStudent%HomePSGC, ierr)
            if (ierr/=0) wrkStudent%HomePSGC = StudentInfo%HomePSGC
            if (len_trim(wrkStudent%HomePSGC)==0) wrkStudent%HomePSGC = PSGC(NumPSGC)%Code
            if (wrkStudent%HomePSGC/=StudentInfo%HomePSGC) then
                changed= .true.
                comment = ': HomePSGC '//comment
                call log_student_record_change(targetStudent, 'HomePSGC='//wrkStudent%HomePSGC)
            end if

            call cgi_get_named_string(QUERY_STRING, 'MotherTongue', wrkStudent%MotherTongue, ierr)
            if (ierr/=0) wrkStudent%MotherTongue = StudentInfo%MotherTongue
            if (wrkStudent%MotherTongue/=StudentInfo%MotherTongue) then
                changed= .true.
                comment = ': MotherTongue '//comment
                call log_student_record_change(targetStudent, 'MotherTongue='//wrkStudent%MotherTongue)
            end if

            if (changed) then

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
            comment = '"'//trim(tAction)//SPACE//trim(tStdno)//'" failed. '//sorryMessageOfficial
        end if

        if (isDirty) then
            isDirtySTUDENTS = .true.
            call xml_student_info(targetStudent)
        end if

        if (trim(USERNAME)==trim(tStdNo)) then
            ownEdit = .true.
            disabled = ' disabled'
        else
            ownEdit = .false.
            disabled = SPACE
        end if

        k = 0
        if (len_trim(StudentInfo%Email)==0            ) k = k+1
        if (len_trim(StudentInfo%Father)==0           ) k = k+1
        if (len_trim(StudentInfo%Mother)==0           ) k = k+1
        if (len_trim(StudentInfo%Guardian)==0         ) k = k+1
        if (len_trim(StudentInfo%BirthDate)==0        ) k = k+1
        if (len_trim(StudentInfo%BirthPlace)==0       ) k = k+1
        if (len_trim(StudentInfo%BirthPlacePSGC)==0   ) k = k+1
        if (len_trim(StudentInfo%HomeStreetAddress)==0) k = k+1
        if (len_trim(StudentInfo%HomePSGC)==0 .or. StudentInfo%HomePSGC==PSGC(NumPSGC)%Code) k = k+1
        if (len_trim(StudentInfo%MotherTongue)==0     ) k = k+1
        if (len_trim(StudentInfo%LastAttended)==0     ) k = k+1
        j = index(StudentInfo%Name, '(*)')
        if (k>0) then
            header = 'Edit '//red//'missing'//e_color//' student info'
            if (j==0) StudentInfo%Name = trim(StudentInfo%Name)//' (*)'
        else
            if (j>0) StudentInfo%Name(j:) = SPACE
        end if
        wrkStudent%Name = StudentInfo%Name
        Student(targetStudent)%Name = StudentInfo%Name

        call html_write_header(device, header, comment)


        call get_picture_file(targetStudent, line)
        if (len_trim(line)>0) then
            line = trim(urlPICTURES)//line
        else
            line = trim(urlPICTURES)//'photo.jpg'
        end if

        write(device,AFORMAT) &
            '<table border="0" width="100%" cellpadding="0" cellspacing="0">'

        write(device,AFORMAT) &
            b_tr//'<td align="left" valign="middle"><img src="'//trim(line)//'" height="120" width="120">'//linebreak//e_td//b_td
        call make_form_start(device, fnUploadPicture, tStdNo, enctype=' enctype="multipart/form-data"')
        write(device,AFORMAT) nbsp//nbsp//' <input type="file" name="file">', &
            '<input type="submit"'//trim(disabled)//' value="Upload this file as ID"> '// &
            b_small//' (Must be JPEG, PNG, or BMP)'//e_small, '</form>', &
            e_td//e_tr

        call make_form_start(device, fnStudentEdit, tStdNo)
        write(device,AFORMAT) &
            b_tr//b_td//'Student number:'//e_td//b_td//tStdNo//e_td//e_tr

        ! password
        if ( isRole_admin_of_college(Curriculum(idxCURR)%CollegeIdx) .and. .not. isRoleOfficial) then
            call getPassword_of_student(targetStudent, Password)
            write(device,AFORMAT) &
                b_tr//b_td//'Password:'//e_td//b_td//trim(Password)//  &
                trim(make_href(fnGenerateStudentPassword, 'Reset password', &
                        A1=tStdNo, pre=nbsp//nbsp//b_small//b_italic, post=e_italic//e_small//e_td//e_tr) )
        end if

        write(device,AFORMAT) &
            b_tr//b_td//'Name:'//e_td//b_td// &
            '<input name="Name" size="80" '//trim(disabled)//' value="'//trim(wrkStudent%Name)//'"> '// &
            b_small//'(LAST, FIRST JR/III, MIDDLE)'//e_small//e_td//e_tr

        if (len_trim(wrkStudent%Email)==0) then
            tColor = red
        else
            tColor = black
        end if
        write(device,AFORMAT) &
            b_tr//b_td//tColor//'Email address:'//e_color//e_td//b_td// &
            '<input name="Email" size="80" value="'//trim(wrkStudent%Email)//'"> '// &
            b_small//' (username@domain)'//e_small//e_td//e_tr

        write(device,AFORMAT) &
            b_tr//b_td//'Gender:'//e_td//b_td//'<select name="Gender">'
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
            '</select>'//e_td//e_tr

        if (len_trim(wrkStudent%Father)==0) then
            tColor = red
        else
            tColor = black
        end if
        write(device,AFORMAT) &
            b_tr//b_td//tColor//'Father:'//e_color//e_small//e_td//b_td// &
            '<input name="Father" size="80" value="'//trim(wrkStudent%Father)//'"> '// &
            b_small//'(LAST, FIRST SR/II, MIDDLE)'//e_td//e_tr

        if (len_trim(wrkStudent%Mother)==0) then
            tColor = red
        else
            tColor = black
        end if
        write(device,AFORMAT) &
            b_tr//b_td//tColor//'Mother:'//e_color//e_small//e_td//b_td// &
            '<input name="Mother" size="80" value="'//trim(wrkStudent%Mother)//'"> '// &
            b_small//'(LAST, FIRST, MIDDLE)'//e_td//e_tr

        if (len_trim(wrkStudent%Guardian)==0) then
            tColor = red
        else
            tColor = black
        end if
        write(device,AFORMAT) &
            b_tr//b_td//tColor//'Guardian:'//e_color//e_small//e_td// &
            b_td//'<input name="Guardian" size="80" value="'//trim(wrkStudent%Guardian)//'"> '// &
            b_small//'(LAST, FIRST, MIDDLE - Relationship)'//e_td//e_tr

        if (len_trim(wrkStudent%BirthDate)==0) then
            tColor = red
        else
            tColor = black
        end if
        write(device,AFORMAT) &
            b_tr//b_td//tColor//'Date of birth:'//e_color//e_td//b_td// &
            '<input name="BirthDate" size="20" value="'//trim(wrkStudent%BirthDate)//'"> '// &
            b_small//' MM/DD/YYYY'//e_small//e_td//e_tr

        if (len_trim(wrkStudent%BirthPlace)==0) then
            tColor = red
        else
            tColor = black
        end if
        write(device,AFORMAT) &
            b_tr//b_td//tColor//'Place of birth :'//e_color//e_td//b_td// &
            '<input name="BirthPlace" size="80" value="'//trim(wrkStudent%BirthPlace)//'">'//e_td//e_tr

        if (len_trim(wrkStudent%HomeStreetAddress)==0) then
            tColor = red
        else
            tColor = black
        end if
        write(device,AFORMAT) &
            b_tr//b_td//tColor//'Home street address:'//e_color//e_td//b_td// &
            '<input name="HomeStreetAddress" size="80" value="'//trim(wrkStudent%HomeStreetAddress)//'">'//e_td//e_tr

        if (len_trim(wrkStudent%LastAttended)==0) then
            tColor = red
        else
            tColor = black
        end if
        write(device,AFORMAT) &
            b_tr//b_td//tColor//'Previous school:'//e_color//e_td//b_td// &
            '<input name="LastAttended" size="80" value="'//trim(wrkStudent%LastAttended)//'">'//e_td//e_tr

!        if (Curriculum(idxCurr)%Code(1:6)=='CBEAGC') then ! allow self-select
!            ! hardcoded selections for CBEAGC
!            choicesCurriculum = ':BS-ACT-13:BSBA-FM-13:BSBA-MA-12:BSBA-MM-13:BSENT-13:BSLM-13:'
!            choicesCurriculum = ':'//trim(Curriculum(idxCurr)%Code)//choicesCurriculum
!            write(device,AFORMAT) &
!                b_tr//b_td//red//'Select curriculum:'//e_color//e_td//b_td//'<select name="CurriculumIdx">'
!            do l=1,NumCurricula
!                if (index(choicesCurriculum, ':'//trim(trim(Curriculum(l)%Code))//':')==0) cycle
!                if (l==idxCurr) then
!                    j = 1
!                else
!                    j = 0
!                end if
!                write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(Curriculum(l)%Code)//'"> '// &
!                    trim(text_curriculum_info(l))
!            end do
!            write(device,AFORMAT) '</select>'//e_td//e_tr
!
!        else
            write(device,AFORMAT) &
                b_tr//b_td//'Curriculum:'//e_td//b_td//'<select '//trim(disabled)//' name="CurriculumIdx">'
            do l=1,NumCurricula
                if (l==idxCurr) then
                    j = 1
                else
                    j = 0
                end if
                write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(Curriculum(l)%Code)//'"> '// &
                    trim(text_curriculum_info(l))
            end do
            write(device,AFORMAT) '</select>'//e_td//e_tr
!        end if

        write(device,AFORMAT) &
            b_tr//b_td//'Adviser:'//e_td//b_td//'<select '//trim(disabled)//' name="Adviser">'
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
        write(device,AFORMAT) '</select>'//e_td//e_tr

        write(device,AFORMAT) &
            b_tr//b_td//'Scholarship:'//e_td//b_td//'<select '//trim(disabled)//' name="Scholarship">'
        write(device,AFORMAT) '<option value=""> (Select scholarship)'
        do l=1,MAX_ALL_SCHOLARSHIPS
            if (len_trim(ScholarshipCode(l))==0) cycle
            if (ScholarshipCode(l)==wrkStudent%Scholarship) then
                j = 1
            else
                j = 0
            end if
            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(ScholarshipCode(l))//'"> '// &
                trim(ScholarshipCode(l))
        end do
        write(device,AFORMAT) '</select>'//e_td//e_tr

        write(device,AFORMAT) &
            b_tr//b_td//'Date of entry:'//e_td//b_td// &
            '<input name="EntryDate" size="20" '//trim(disabled)//' value="'//trim(wrkStudent%EntryDate)//'"> '// &
            b_small//' MM/DD/YYYY'//e_small//e_td//e_tr

        write(device,AFORMAT) &
            b_tr//b_td//'Admission data:'//e_td//b_td// &
            '<input name="AdmissionData" size="60" '//trim(disabled)//' value="'//trim(wrkStudent%AdmissionData)//'">'//e_td//e_tr

        write(device,AFORMAT) &
            b_tr//b_td//'Date of graduation:'//e_td//b_td// &
            '<input name="GraduationDate" size="20" '//trim(disabled)//' value="'//trim(wrkStudent%GraduationDate)//'"> '// &
            b_small//' MM/DD/YYYY'//e_small//e_td//e_tr

        write(device,AFORMAT) &
            b_tr//b_td//'Remark on transcript:'//e_td//b_td// &
            '<input name="TranscriptRemark" size="120" '//trim(disabled)//' value="'//trim(wrkStudent%TranscriptRemark)//'">'// &
            e_td//e_tr

        write(device,AFORMAT) &
            b_tr//b_td//'Additional remark on transcript:'//e_td//b_td// &
            '<input name="TranscriptAdditionalRemark" size="120" '//trim(disabled)//' value="'// &
            trim(wrkStudent%TranscriptAdditionalRemark)//'">'// &
            e_td//e_tr

        ! update button
        write(device,AFORMAT) &
            e_table//linebreak//'<input type="submit" name="action" value="Update"> ', &
            e_form//horizontal


        ! PSGC
        write(device,AFORMAT) '<table border="0" width="100%" cellpadding="0" cellspacing="0">'

        if (len_trim(wrkStudent%BirthPlacePSGC)==0 .or. wrkStudent%BirthPlacePSGC==PSGC(NumPSGC)%Code) then ! none; select province
            write(device,AFORMAT) '<tr valign="top">'//b_td//b_bold//red//'Select birth place province'// &
                e_color//e_bold//e_td//b_td
            do l=1,NumRegion
                write(device,AFORMAT) trim(PSGC(Region(l))%Name)//' : '//b_small//b_italic
                do j=1,NumProvince
                    if (PSGC(Region(l))%Code(1:2)/=PSGC(Province(j))%Code(1:2)) cycle
                    write(device,AFORMAT) trim(make_href(fnSelectPSGC, trim(PSGC(Province(j))%Name), A1=tStdNo, &
                        A2='BirthPlacePSGC', A3=PSGC(Province(j))%Code, pre=nbsp ))
                end do
                write(device,AFORMAT) e_italic//e_small//linebreak
            end do
        else if ( isPSGC_kind(wrkStudent%BirthPlacePSGC, kindProvince) ) then ! select municipality
            write(device,AFORMAT) '<tr valign="top">'//b_td//b_bold//red//'Select birth place municipality in'//linebreak// &
                trim(text_PSGC(wrkStudent%BirthPlacePSGC))//e_bold//e_color//e_td//b_td//b_small//b_italic
            do k=1,NumMunicipality
                l = Municipality(k)
                if (PSGC(l)%Code(1:4)==wrkStudent%BirthPlacePSGC(1:4)) &
                    write(device,AFORMAT) trim(make_href(fnSelectPSGC, trim(PSGC(l)%Name), A1=tStdNo, &
                        A2='BirthPlacePSGC', A3=PSGC(l)%Code, pre=nbsp ))
            end do
            write(device,AFORMAT) &
                trim(make_href(fnSelectPSGC, 'Reselect', A1=tStdNo, A2='BirthPlacePSGC', A3=PSGC(NumPSGC)%Code, &
                pre=nbsp//'(', post=')')), &
                e_italic//e_small
        else if ( isPSGC_kind(wrkStudent%BirthPlacePSGC, kindMunicipality) ) then ! select barangay
            j = index_to_PSGC(wrkStudent%BirthPlacePSGC) ! index of Municipality in PSGC()
            write(device,AFORMAT) '<tr valign="top">'//b_td//b_bold//red//'Select birth place barangay in'//linebreak// &
                trim(text_PSGC(wrkStudent%BirthPlacePSGC))//e_bold//e_color//e_td//b_td//b_small//b_italic
            k = 0
            do while (PSGC(j+k+1)%Code(:6)==PSGC(j)%Code(:6))
                k = k + 1
            end do
            do l=j+1,j+k
                write(device,AFORMAT) trim(make_href(fnSelectPSGC, trim(PSGC(l)%Name), A1=tStdNo, &
                    A2='BirthPlacePSGC', A3=PSGC(l)%Code, pre=nbsp ))
            end do
            write(device,AFORMAT) &
                trim(make_href(fnSelectPSGC, 'Reselect', A1=tStdNo, A2='BirthPlacePSGC', A3=PSGC(NumPSGC)%Code, &
                pre=nbsp//'(', post=')')), &
                e_italic//e_small
        else
            write(device,AFORMAT) b_tr//b_td//'PSGC Place of birth:'//e_td// &
                b_td//trim(text_PSGC(wrkStudent%BirthPlacePSGC)), &
                trim(make_href(fnSelectPSGC, 'Reselect', A1=tStdNo, A2='BirthPlacePSGC', A3=PSGC(NumPSGC)%Code, &
                pre=nbsp//b_small//b_italic, post=e_italic//e_small))
        end if
        write(device,AFORMAT) e_td//e_tr

        if (len_trim(wrkStudent%HomePSGC)==0 .or. trim(wrkStudent%HomePSGC)==PSGC(NumPSGC)%Code) then ! none; select province
            write(device,AFORMAT) '<tr valign="top">'//b_td//b_bold//red//'Select home province'// &
                e_color//e_bold//e_td//b_td
            do l=1,NumRegion
                write(device,AFORMAT) trim(PSGC(Region(l))%Name)//' : '//b_small//b_italic
                do j=1,NumProvince
                    if (PSGC(Region(l))%Code(1:2)/=PSGC(Province(j))%Code(1:2)) cycle
                    write(device,AFORMAT) trim(make_href(fnSelectPSGC, trim(PSGC(Province(j))%Name), A1=tStdNo, &
                        A2='HomePSGC', A3=PSGC(Province(j))%Code, pre=nbsp ))
                end do
                write(device,AFORMAT) e_italic//e_small//linebreak
            end do
        else if ( isPSGC_kind(wrkStudent%HomePSGC, kindProvince) ) then ! select municipality
            write(device,AFORMAT) '<tr valign="top">'//b_td//b_bold//red//'Select home municipality in'//linebreak// &
                trim(text_PSGC(wrkStudent%HomePSGC))//e_bold//e_color//e_td//b_td//b_small//b_italic
            do k=1,NumMunicipality
                l = Municipality(k)
                if (PSGC(l)%Code(1:4)==wrkStudent%HomePSGC(1:4)) &
                    write(device,AFORMAT) trim(make_href(fnSelectPSGC, trim(PSGC(l)%Name), A1=tStdNo, &
                        A2='HomePSGC', A3=PSGC(l)%Code, pre=nbsp ))
            end do
            write(device,AFORMAT) &
                trim(make_href(fnSelectPSGC, 'Reselect', A1=tStdNo, A2='HomePSGC', A3=PSGC(NumPSGC)%Code, &
                pre=nbsp//'(', post=')')), &
                e_italic//e_small
        else if ( isPSGC_kind(wrkStudent%HomePSGC, kindMunicipality) ) then ! select barangay
            j = index_to_PSGC(wrkStudent%HomePSGC) ! index of Municipality in PSGC()
            write(device,AFORMAT) '<tr valign="top">'//b_td//b_bold//red//'Select home barangay in'//linebreak// &
                trim(text_PSGC(wrkStudent%HomePSGC))//e_bold//e_color//e_td//b_td//b_small//b_italic
            k = 0
            do while (PSGC(j+k+1)%Code(:6)==PSGC(j)%Code(:6))
                k = k + 1
            end do
            do l=j+1,j+k
                write(device,AFORMAT) trim(make_href(fnSelectPSGC, trim(PSGC(l)%Name), A1=tStdNo, &
                    A2='HomePSGC', A3=PSGC(l)%Code, pre=nbsp ))
            end do
            write(device,AFORMAT) &
                trim(make_href(fnSelectPSGC, 'Reselect', A1=tStdNo, A2='HomePSGC', A3=PSGC(NumPSGC)%Code, &
                pre=nbsp//'(', post=')')), &
                e_italic//e_small
        else
            write(device,AFORMAT) b_tr//b_td//'PSGC Home barangay:'//e_td// &
                b_td//trim(text_PSGC(wrkStudent%HomePSGC)), &
                trim(make_href(fnSelectPSGC, 'Reselect', A1=tStdNo, A2='HomePSGC', A3=PSGC(NumPSGC)%Code, &
                pre=nbsp//b_small//b_italic))
        end if
        write(device,AFORMAT) e_td//e_tr

        if (len_trim(wrkStudent%MotherTongue)==0 .or. trim(wrkStudent%MotherTongue)=='DEL') then ! none; select tongue
            write(device,AFORMAT) '<tr valign="top">'//b_td//b_bold//red//'Select dialect'// &
                e_color//e_bold//e_td//b_td
            do j=iachar('A'), iachar('Z')
                ch = achar(j)
                k = 0
                do l=1,NumTongue
                    if (Tongue(l)%Name(1:1) /= ch) cycle
                    k = k+1
                    exit
                end do
                if (k==0) cycle
                write(device,AFORMAT) '"'//ch//'":'//b_small//b_italic
                do k=1,NumTongue
                    l = Tongue(k)%Rank
                    if (Tongue(l)%Name(1:1)/=ch) cycle
                    write(device,AFORMAT) trim(make_href(fnSelectTongue, trim(Tongue(l)%Name), A1=tStdNo, &
                        A2='MotherTongue', A3=Tongue(l)%Code, pre=nbsp  ))
                end do
                write(device,AFORMAT) e_italic//e_small//linebreak
            end do

        else
            write(device,AFORMAT) &
                b_tr//b_td//'Dialect:'//e_td//b_td//trim(Tongue(index_to_Tongue(wrkStudent%MotherTongue))%Name), &
                trim(make_href(fnSelectTongue, 'Reselect', A1=tStdNo, A2='MotherTongue', A3='DEL', &
                pre=nbsp//b_small//b_italic, post=e_italic//e_small ))
        end if
        write(device,AFORMAT) e_td//e_tr


        write(device,AFORMAT) e_table//horizontal


        ! rest of the page not for students
        if (ownEdit) return

        ! add student button
        if (trim(tAction)=='Add') then
            call make_form_start(device, fnStudentAdd)
            write(device,AFORMAT) &
                '<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
                b_tr//b_td//'Add student number <input name="A1" value=""> '//nbsp// &
                '<input type="submit" name="action" value="Add">'//e_form// &
                e_td//e_tr//e_table//horizontal

        ! find student button
        else
            call make_form_start(device, fnFindStudent)
            write(device,AFORMAT) '<table border="0" cellpadding="0" cellspacing="0">', &
                b_tr//b_td//b_bold//'Find students'//e_bold//' with <input name="A1" value="">', &
                'in name or number. <input type="submit" name="action" value="Search">'//e_form// &
                e_td//e_tr//e_table//horizontal
        end if


    end subroutine student_edit_info


    subroutine student_edit_grades (device, thisTerm)
        implicit none
        integer, intent (in) :: device, thisTerm

        real :: GSumDown, GSumUp, SumDown, SumUp, tUnits, up, down

        integer :: grd, iYear, iTerm, m, tdx
        integer :: prevtaken, nINCs
        character (len=10) :: token1, token2

        logical :: FlagIsUp, isEditor
        integer :: crse, n_changes, gdx, jdx, ierr, i, k, idx, j
        character(len=MAX_LEN_SUBJECT_CODE) :: input_name1, input_name2, input_value
        character(len=3*MAX_LEN_SUBJECT_CODE) :: tAction, input_name3
        character (len=MAX_LEN_TEXT_GRADE) :: tGrade
        character (len=MAX_LEN_FILE_PATH) :: evaluationFile

        character(len=127) :: advising_comment
        type (TYPE_PRE_ENLISTMENT) :: Advice
        integer, dimension(MAX_EVALUATION_TYPES) :: fileCount
        real, dimension(MAX_EVALUATION_TYPES,MAX_EVALUATION_CRITERIA) :: sumRating

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
        do k=1,lenTCG
            TCG(k)%Used = .false.
        end do

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
                    tSubject = fn_underscore_to_blank(input_name2)
                    crse = index_to_subject(tSubject)
                    gdx = atoi(input_value)

                    if (gdx==gdxINC .or. gdx==gdxNFE) cycle ! non-INC completion grade not specified

                    ! add a COMPLETION record
                    lenTCG = lenTCG + 1
                    TCG(lenTCG)%Code = 5 ! COMPLETION
                    TCG(lenTCG)%Year = currentYear
                    TCG(lenTCG)%Term = thisTerm
                    TCG(lenTCG)%Subject = crse
                    TCG(lenTCG)%Grade = gdx
                    call html_comment('New record TCG('//itoa(lenTCG)//') - '//tSubject//txtGrade(pGrade(gdx)) )
                    line = ' : '//trim(Subject(crse)%Name)//', INC->'//txtGrade(pGrade(gdx))//line
                    n_changes = n_changes + 1

                else
                    ! get year, term, subject, original completion grade
                    iYear = atoi(input_name3(1:4))
                    iTerm = atoi(input_name3(6:6))
                    input_name2 = input_name3(8:)
                    i = index(input_name2,':')
                    grd = atoi(input_name2(i+1:))
                    input_name2(i:) = SPACE
                    tSubject = fn_underscore_to_blank(input_name2)
                    tGrade = input_value

                    crse = index_to_subject(tSubject)
                    gdx = atoi(tGrade)

                    if (grd==gdx) cycle ! no change

                    ! find matching COMPLETION record
                    do k=lenTCG,1,-1
                        if (TCG(k)%Code/=5) cycle ! not COMPLETION
                        if (TCG(k)%Used) cycle ! used prevoiusly
                        if (TCG(k)%Subject/=crse .or. TCG(k)%Grade/=grd) cycle ! not the subject+grd
                        TCG(k)%Grade = gdx
                        TCG(k)%Used = .true.
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
                call log_student_record_change(targetStudent, line)
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

                tSubject = fn_underscore_to_blank(input_name2)
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
                    TCG(lenTCG)%Term = thisTerm
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
                call log_student_record_change(targetStudent, line)
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
                iYear = atoi(input_name3(1:4))
                iTerm = atoi(input_name3(6:6))
                input_name2 = input_name3(8:)
                i = index(input_name2,':')
                jdx = atoi(input_name2(i+1:))
                input_name2(i:) = SPACE
                tSubject = fn_underscore_to_blank(input_name2)
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
                        if (TCG(k)%Year/=iYear) cycle ! not the year
                        if (TCG(k)%Term/=iTerm) cycle ! not the term
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
                        TCG(lenTCG)%Term = thisTerm
                        TCG(lenTCG)%Subject = crse
                        TCG(lenTCG)%Grade = gdx
                        line = ' : '//trim(Subject(crse)%Name)//', '//tGrade//line
                        n_changes = n_changes + 1

                    end if

                else ! grade is empty (to indicate 'Remove grade')

                    FlagIsUp = .false. ! subject found?
                    do k=lenTCG,1,-1
                        if (TCG(k)%Year/=iYear) cycle ! not the year
                        if (TCG(k)%Term/=iTerm) cycle ! not the term
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
                call log_student_record_change(targetStudent, line)
            end if

        end if

        if (len_trim(tAction)>0 .and. isRoleOfficial) then
            line = '"'//trim(tAction)//'" failed. '//sorryMessageOfficial
        end if

        ! reload ?
        if (n_changes>0) then
            call read_student_records (targetStudent)
        end if

        call html_comment('Grades of '//trim(StudentInfo%Name))

        tStdNo = Student(targetStudent)%StdNo
        isEditor = isRole_admin_of_college(targetCollege)

        call html_write_header(device, trim(tStdNo)//SPACE//trim(Student(targetStudent)%Name)// &
            linebreak//text_curriculum_info(targetCurriculum), line)

        if (isEditor) then
            write(device,AFORMAT) &
                linebreak//b_bold//'UNOFFICIAL Copy of Grades and Weighted Average by Term'//e_bold, &
                trim(make_href(fnTranscript, 'Printable', A1=tStdNo, pre=' ( ', post=' )', newtab='"_blank"')), &
                linebreak//b_italic//'Enter or modify contents of the edit boxes, then click "Change GRADE". ', &
                ' For previous and current terms, change a grade in the corresponding gradesheet.'//e_italic
        else
            write(device,AFORMAT) &
                linebreak//b_bold//'UNOFFICIAL Copy of Grades and Weighted Average by Term'//e_bold
        end if

        write(device,AFORMAT)  &
            '<table border="0" width="100%">', &
            b_tr//'<td colspan="7">'//horizontal//e_td//e_tr, &
            b_tr//b_td//'SUBJECT'//e_td, &
            '<td colspan="4" align="left">COLLEGIATE RECORDS'//e_td, &
            b_tdac//'FINAL'//e_td, &
            b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td//e_tr, &
            b_tr//b_td//'NUMBER'//e_td, &
            '<td colspan="4" align="left">DESCRIPTIVE TITLE OF THE SUBJECT'//e_td, &
            b_tdac//'GRADE'//e_td, &
            b_tdac//'CREDIT'//e_td, &
            '<td colspan="3" align="right">WTD AVERAGE'//e_td//e_tr

        ! the advance credits (PASSed subjects)
        m = 0
        do tdx=1,lenTCG
            TCG(tdx)%Used = .false.
            if (TCG(tdx)%Code==2 .and. isGrade_passing(TCG(tdx)%Grade)) m = m+1
        end do
        if (m>0) then ! there are advance credits
            if (isEditor) then
                call make_form_start(device, fnStudentGrades, tStdNo)
            end if
            write(device,AFORMAT) &
                b_tr//'<td colspan="7">'//horizontal//e_td//e_tr, &
                b_tr//b_td_nbsp_e_td//'<td colspan="4" align="left">ADVANCE/TRANSFER CREDITS'//e_td// &
                '<td colspan="6">'//nbsp//e_td//e_tr
            do tdx=1,lenTCG

                if (TCG(tdx)%Code/=2 .or. .not. isGrade_passing(TCG(tdx)%Grade)) cycle

                crse = TCG(tdx)%Subject
                grd = TCG(tdx)%Grade
                token1 = SPACE
                token2 = SPACE
                tSubject = Subject(crse)%Name
                input_name2 = fn_blank_to_underscore(tSubject)

                line = b_tr//b_td//trim(tSubject)//e_td//'<td colspan="4">'//trim(Subject(crse)%Title)//e_td

                if (isEditor) then
                    line = trim(line)//b_tdac//'<input type="text" size="4" name="ADV:'//trim(input_name2)// &
                        '" value="'//trim(txtGrade(pGrade(grd)))//'">'//e_td
                else
                    line = trim(line)//b_tdac//txtGrade(pGrade(grd))//e_td
                end if

                if (Subject(crse)%Units == 0.0 .or. tSubject(1:5)=='NSTP ') then
                    line = trim(line)//b_td_nbsp_e_td
                else
                    line = trim(line)//b_tdac//trim(ftoa(Subject(crse)%Units,1))//e_td
                end if

                write (device,AFORMAT) trim(line)// &
                    b_tdar//trim(token1)//e_td// &
                    b_tdar//trim(token2)//e_td// &
                    b_td_nbsp_e_td//e_tr

                TCG(tdx)%Used = .true.
            end do
            if (isEditor) then
                write(device,AFORMAT) b_tr// &
                    '<td colspan="6" align="right"><input type="submit" name="action" value="Update ADVANCE">'//e_form// &
                    e_td//'<td colspan="4">'//e_td//e_tr
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
                if (isEditor .and. prevtaken/=-10 .and. FlagIsUp) then
                    write(device,AFORMAT) b_tr, &
                        '<td colspan="6" align="right"><input type="submit" name="action" value="Change GRADE">'//e_form// &
                        e_td//'<td colspan="4">'//e_td//e_tr
                end if

                ! write summary for current term
                write(device,AFORMAT) b_tr//'<td colspan="7">'//horizontal//e_td
                if (SumUp*SumDown>0.0) then
                    write(device,'(a,f8.2,a,f5.1,a,f8.2,a)') &
                        b_tdar, SumUp, e_td// &
                        b_tdar, SumDown, e_td// &
                        b_tdar, SumUp/SumDown, e_td//e_tr
                    GSumUp = GSumUp + SumUp
                    GSumDown = GSumDown + SumDown
                 else
                    write(device,AFORMAT) e_tr
                end if
                ! header for next term
                write(device,AFORMAT) &
                    b_tr//b_td_nbsp_e_td//'<td colspan="4" align="left">', &
                        trim(text_term_school_year(TCG(tdx)%Term, TCG(tdx)%Year))

                if (isEditor) then
                    write(device,AFORMAT) &
                        trim(make_href(fnGradeCertification, 'Certification', &
                        A1=tStdNo, A2=itoa(TCG(tdx)%Year), A3=itoa(TCG(tdx)%Term), &
                        pre=' ( ', post=' )', newtab='"_blank"') )
                    call make_form_start(device, fnStudentGrades, tStdNo)
                end if
                write(device,AFORMAT) e_td//'<td colspan="6">'//nbsp//e_td//e_tr

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
            if (grd==gdxINC) nINCs = nINCs+1

            up = 0.0
            down = 0.0
            token1 = SPACE
            token2 = SPACE
            tSubject = Subject(crse)%Name
            input_name2 = fn_blank_to_underscore(tSubject)

            line = b_tr//b_td//trim(tSubject)//e_td//'<td colspan="4">'//trim(Subject(crse)%Title)//e_td

            if (TCG(tdx)%ReExam/=0) then
                line = trim(line)//b_tdac//txtGrade(pGrade(grd))//FSLASH// &
                txtGrade(pGrade(TCG(tdx)%ReExam))//e_td
                grd = TCG(tdx)%ReExam
            else
                if (isEditor .and. .not. ( &
                         (currentYear==TCG(tdx)%Year .and. currentTerm==TCG(tdx)%Term) .or. &
                         (cTm1Year==TCG(tdx)%Year .and. cTm1==TCG(tdx)%Term) ) ) then
                    line = trim(line)//b_tdac//'<input type="text" size="4" name="GRADE:'// &
                        trim(itoa(TCG(tdx)%Year))//':'//trim(itoa(TCG(tdx)%Term))//':'//trim(input_name2)// &
                        ':'//trim(itoa(grd))//'" value="'//trim(txtGrade(pGrade(grd)))//'">'//e_td
                    FlagIsUp = .true.
                else if (grd==gdxREGD) then
                    line = trim(line)//b_tdac//b_italic//'No grade'//e_italic//e_td
                else
                    !line = trim(line)//b_tdac//txtGrade(pGrade(grd))//e_td
                    ! if subject taken last term, evaluation done?
                    if (trim(USERNAME)==trim(tStdNo)) then

                        if (TCG(tdx)%Year/=cTm1Year) then
                            line = trim(line)//b_tdac//txtGrade(pGrade(grd))//e_td
                        elseif (TCG(tdx)%Term/=cTm1) then
                            line = trim(line)//b_tdac//txtGrade(pGrade(grd))//e_td
                        else

                            i = 4 ! STUDENT
                            evaluationFile = trim(EvalTypeDescription(i))//SPACE//trim(USERNAME)//SPACE//Subject(crse)%Name

                            evaluationFile = trim(dirDATA)//'evaluations'//DIRSEP// &
                                trim(itoa(cTm1Year))//DIRSEP//trim(txtSemester(cTm1))//DIRSEP//'*'//DIRSEP// &
                                trim(filename_from(evaluationFile))//dotXML

                            fileCount = 0
                            sumRating = 0.0
                            call effectiveness_ratings_retrieve (i, evaluationFile, fileCount, sumRating, SPACE)
                            if (sum(fileCount)/=0) then ! some ratings for this subject
                                line = trim(line)//b_tdac//txtGrade(pGrade(grd))//e_td
                            else
                                line = trim(line)//b_tdac//'NTEv'//e_td
                                tUnits = 0.0
                            end if

                        end if

                    else
                        line = trim(line)//b_tdac//txtGrade(pGrade(grd))//e_td

                    end if
                end if
            end if

            if (tUnits == 0.0 .or. Subject(crse)%Name(1:5)=='NSTP ') then ! exclude
                line = trim(line)//b_td_nbsp_e_td

            else if (grd==gdxDRP .or. grd==gdxPASS) then ! exclude
                line = trim(line)//b_td_nbsp_e_td

            else if (grd==gdxINC .or. grd==gdxNFE .or. grd==gdxREGD) then
                !line = trim(line)//b_td_nbsp_e_td
                line = trim(line)//b_tdac//trim(ftoa(tUnits,1))//e_td
                up = 0.0
                down = tUnits

            else
                up = tUnits*fGrade(grd)
                if (up/=0.0) then
                    down = tUnits
                    if ( isGrade_passing(grd) ) then
                        line = trim(line)//b_tdac//trim(ftoa(tUnits,1))//e_td
                    else
                        line = trim(line)//b_td_nbsp_e_td
                    end if
                else
                    down = 0.0
                    line = trim(line)//b_td_nbsp_e_td
                end if
            end if

            if (down>0.0) then
                write (token1,'(f8.2)') up
                write (token2,'(f5.1)') down
            end if
            write (device,AFORMAT) trim(line)// &
                b_tdar//trim(token1)//e_td// &
                b_tdar//trim(token2)//e_td// &
                b_td_nbsp_e_td//e_tr
            SumUp = SumUp + Up
            SumDown = SumDown + Down
        end do
        ! write summary for last term
        if (isEditor .and. FlagIsUp) then
            write(device,AFORMAT) &
                b_tr//'<td colspan="6" align="right"><input type="submit" name="action" value="Change GRADE">'//&
                    e_form//e_td// &
                '<td colspan="4">'//e_td//e_tr
        end if
        write(device,AFORMAT) b_tr//'<td colspan="7">'//horizontal//e_td
        if (SumUp*SumDown>0.0) then
            write(device,'(a,f8.2,a,f5.1,a,f8.2,a)') &
                b_tdar, SumUp, e_td// &
                b_tdar, SumDown, e_td// &
                b_tdar, SumUp/SumDown, e_td//e_tr
            GSumUp = GSumUp + SumUp
            GSumDown = GSumDown + SumDown
        else
            write(device,AFORMAT) e_tr
        end if

        if (GSumUp*GSumDown>0.0) then
            write(device,'(a,f8.2,a,f5.1,a,f8.2,a)') &
                b_tr//'<td colspan="7" align="right">'//b_bold//'General Weighted Average : '//e_bold//e_td// &
                b_tdar, GSumUp, e_td// &
                b_tdar, GSumDown, e_td// &
                b_tdar, GSumUp/GSumDown, e_td//e_tr
        end if

        if (isEditor) then
            write(device,AFORMAT) e_table, horizontal

            if (nINCs>0) then
                call make_form_start(device, fnStudentGrades, tStdNo)

                write(device,AFORMAT) &
                    linebreak//linebreak//b_bold//'INCOMPLETE GRADES AND COMPLETIONS'//e_bold// &
                        linebreak//b_italic//'Select completion grades, then click "Submit COMPLETION".'//e_italic
                write(device,AFORMAT)  &
                    '<table border="0" width="80%">', &
                    b_tr//'<td colspan="7">'//horizontal//e_td//e_tr, &
                    b_tr//b_td//'SUBJECT'//e_td, &
                    '<td colspan="4" align="left">DESCRIPTIVE TITLE OF SUBJECT'//e_td, &
                    b_tdac//'GRADE, TERM RECEIVED'//e_td, &
                    b_tdac//'COMPLETION'//e_td, &
                    '<td colspan="3">'//nbsp//e_td//e_tr, &
                    b_tr//'<td colspan="7">'//horizontal//e_td//e_tr

                do tdx=1,lenTCG
                    TCG(tdx)%Used = .false.
                end do
                do tdx=lenTCG,1,-1
                    if (TCG(tdx)%Code/=3) cycle
                    grd = TCG(tdx)%Grade
                    if (grd/=gdxINC) cycle ! not INC

                    crse = TCG(tdx)%Subject
                    tSubject = Subject(crse)%Name
                    input_name2 = fn_blank_to_underscore(tSubject)
                    write (device,AFORMAT) &
                        b_tr//b_td//trim(tSubject)//e_td//'<td colspan="4">'//trim(Subject(crse)%Title)//e_td, &
                        b_tdac//txtGrade(pGrade(grd))//COMMA//SPACE// &
                        trim(text_term_school_year(TCG(tdx)%Term, TCG(tdx)%Year))//e_td

                    ! find macthing completion, if any
                    k = 0
                    do j=lenTCG,1,-1
                        if ( TCG(j)%Code/=5 .or. TCG(j)%Subject/=crse ) cycle ! not the subject
                        if ( TCG(j)%Used ) cycle ! used previously
                        k = j
                        exit
                    end do
                    if (k/=0) then ! already completed

                        call html_comment(tSubject//'completed: '//txtGrade(pGrade(TCG(k)%Grade)) )

                        write (device,AFORMAT) &
                            b_tdac//'<select name="COMP:'// &
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
                        TCG(k)%Used =  .true.

                    else

                        call html_comment(tSubject//'not completed: '//txtGrade(pGrade(grd)) )
                        write (device,AFORMAT) &
                            b_tdac//'<select name="COMP:0:0:'//trim(input_name2)//'">'// &
                            '<option value="'//trim(itoa(grd))//'"> '//txtGrade(pGrade(grd))
                        do i=ZERO_PERCENT_GRADE+70,ZERO_PERCENT_GRADE+100
                            tGrade = txtGrade(pGrade(i))
                            write(device,AFORMAT) '<option value="'//trim(itoa(i))//'"> '//tGrade
                        end do
                    end if
                    write (device,AFORMAT) '</select>', &
                        b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td//e_tr

                end do

                write(device,AFORMAT) &
                    b_tr//'<td colspan="7" align="right">'// &
                    '<input type="submit" name="action" value="Submit COMPLETION">'//e_td// &
                    '<td colspan="3">'//e_td//e_tr, &
                    b_tr//'<td colspan="7">'//horizontal//e_td//e_tr, &
                    e_table//e_form

            end if

            call advise_student (targetStudent, thisTerm, Advice, advising_comment)

            if (len_trim(advising_comment)>0) then
                write(device,AFORMAT) linebreak//b_bold//red//trim(advising_comment)//e_color//e_bold//linebreak//linebreak
            end if

           ! how many unearned subjects per term in curriculum
            do tdx=1,CheckList%NumTerms
                tArray(tdx) = 0
                call rank_to_year_term(tdx, iYear, iTerm)
                do idx=1,CheckList%NSubjects
                    if (CheckList%SubjectTerm(idx)/=tdx) cycle ! not this term
                    crse = CheckList%SubjectIdx(idx)
                    if (crse<=0) cycle ! not named
                    if ( .not. isGrade_passing(CLExt(idx)%Grade) ) then  ! named subject, not passed
                        tArray(tdx) = tArray(tdx)+1
                        call html_comment('Unearned during Year '//itoa(iYear)//' Term '//itoa(iTerm)//' is '//Subject(crse)%Name)
                    end if
                end do
            end do


            if (sum(tArray(1:Checklist%NumTerms))>0) then  ! some unearned subject

            ! ===================================
            write(device,AFORMAT) &
                linebreak//linebreak//b_bold//'ADVANCE/TRANSFER CREDITS'//e_bold// &
                    linebreak//b_italic//'Enter PASS grades for subjects to be credited, then click "ADVANCE Credit".'//e_italic

            write(device,AFORMAT)  &
                '<table border="0" width="80%">', &
                b_tr//'<td colspan="7">'//horizontal//e_td//e_tr, &
                b_tr//b_td//'SUBJECT'//e_td, &
                '<td colspan="4" align="left">DESCRIPTIVE TITLE OF SUBJECT'//e_td, &
                b_tdac//'GRADE'//e_td, &
                b_tdac//'CREDIT'//e_td, &
                '<td colspan="3">'//nbsp//e_td//e_tr

            do tdx=1,Checklist%NumTerms

                if (tArray(tdx)==0) cycle ! no unearned subject for this term

                call rank_to_year_term(tdx, iYear, iTerm)

                call make_form_start(device, fnStudentGrades, tStdNo)

                ! header for term
                write(device,AFORMAT) &
                    b_tr//'<td colspan="7">'//horizontal//e_td//e_tr, &
                    b_tr//b_td_nbsp_e_td//'<td colspan="4" align="left">'//trim(Checklist%Code)//': '// &
                    trim(txtYear(iYear))//' Year, '//trim(txtSemester(iTerm+3))//' Term'//e_bold//e_td// &
                    '<td colspan="6">'//nbsp//e_td//e_tr

                do idx=1,Checklist%NSubjects
                    if (Checklist%SubjectTerm(idx)/=tdx) cycle ! not this term
                    crse = Checklist%SubjectIdx(idx)
                    if (crse<=0) cycle ! not named

                    tSubject = Subject(crse)%Name
                    input_name2 = fn_blank_to_underscore(tSubject)

                    if ( .not. isGrade_passing(CLExt(idx)%Grade) ) then  ! named subject, not passed
                    !if (CLExt(idx)%Grade==0) then  ! named subject, not passed

                        write (device,AFORMAT) &
                            b_tr//b_td//trim(tSubject)//e_td//'<td colspan="4">'//trim(Subject(crse)%Title)//e_td, &
                            b_tdac//'<input type="text" size="4" name="ADV:'//trim(input_name2)//'" value="">'//e_td, &
                            b_tdac//trim(ftoa(Subject(crse)%Units,1))//e_td, &
                            b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td//e_tr

                    !else if (CLExt(idx)%Grade==gdxPASS) then ! already recorded as ADVANCE/TRANSFER CREDIT

                    !    write (device,AFORMAT) &
                    !        b_tr//b_td//trim(tSubject)//e_td//'<td colspan="4">'//trim(Subject(crse)%Title)//e_td, &
                    !        b_tdac//'<input type="text" size="4" name="ADV:'//trim(input_name2)// &
                    !        '" value="'//trim(txtGrade(pGrade(gdxPASS)))//'">'//e_td, &
                    !        b_tdac//trim(ftoa(Subject(crse)%Units,1))//e_td, &
                    !        b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td//e_tr
                    end if

                end do
                write(device,AFORMAT) b_tr//'<td colspan="5">'//nbsp//e_td// &
                    '<td colspan="2"><input type="submit" name="action" value="ADVANCE Credit">'//e_td// &
                    '<td colspan="3">'//nbsp//e_td//e_tr, &
                    e_form
            end do

            write(device,AFORMAT) b_tr//'<td colspan="7">'//horizontal//e_td//e_tr, &
                e_table

            ! ===================================
            end if ! (sum(tArray(1:Checklist%NumTerms))>0) then  ! some unearned subject

        else
            write(device,AFORMAT) e_table//horizontal
        end if


    end subroutine student_edit_grades


    subroutine student_advisers_by_teacher (device)
        integer, intent (in) :: device

        integer :: n_count, tdx, iStd, ierr, nSteps, first, last, idx, classification
        character(len=MAX_LEN_USERNAME) :: tTeacher, newAdviser
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character(len=MAX_LEN_USERNAME+MAX_LEN_STUDENT_CODE) :: tAction
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

        ! classification if needed
        call cgi_get_named_integer(QUERY_STRING, 'A4', classification, ierr)
        if (ierr/=0) classification = 0

        ! action
        call cgi_get_named_string(QUERY_STRING, 'action'//trim(itoa(first)), tAction, ierr)
        changeAdvisers = ierr==0 .and. trim(tAction)=='Submit'
        mesg = SPACE

        ! collect students
        n_count = 0
        do tdx=1,NumStudents+NumAdditionalStudents
            iStd = StdRank(tdx)
            if (Student(iStd)%Adviser/=tTeacher) cycle
            n_count = n_count+1
            tArray(n_count) = iStd
        end do

        if (changeAdvisers .and. .not. isRoleOfficial) then

            do tdx=first,last
                iStd = tArray(tdx)
                tStdNo = Student(iStd)%StdNo

                call cgi_get_named_string(QUERY_STRING, 'Adviser:'//trim(tStdNo), newAdviser, ierr)
                if (ierr/=0 .or. len_trim(newAdviser)==0) then ! delete
                    idx = index_to_teacher(Student(iStd)%Adviser)
                    Teacher(idx)%NumAdvisees = Teacher(idx)%NumAdvisees - 1
                    Student(iStd)%Adviser = SPACE
                    mesg = ' : '//trim(tStdNo)//mesg
                else
                    if (Student(iStd)%Adviser/=newAdviser) then
                        Student(iStd)%Adviser = newAdviser
                        idx = index_to_teacher(newAdviser)
                        Teacher(idx)%NumAdvisees = Teacher(idx)%NumAdvisees + 1
                        mesg = ' : '//trim(tStdNo)//mesg
                        call xml_read_student_info(iStd, ierr)
                        StudentInfo%Adviser = newAdviser
                        call xml_student_info(iStd)
                        call log_student_record_change(iStd, 'Adviser is '//newAdviser)
                    end if
                end if
                call student_details_write(unitXML, dirSTUDENTS, iStd)
            end do

            if (mesg/=SPACE) then
                mesg = 'Changed adviser '//mesg
            else
                mesg = 'No adviser changes?'
            end if
        end if

        if (changeAdvisers .and. isRoleOfficial) then
            mesg = sorryMessageOfficial
        end if

        call change_adviser_form(device, n_count, nSteps, tArray(1:n_count), &
            fnAdvisersByTeacher, tTeacher, classification, 'Advisees of '//Teacher(targetTeacher)%Name, mesg)

    end subroutine student_advisers_by_teacher



    subroutine student_advisers_by_curriculum (device)
        integer, intent (in) :: device

        integer :: n_count, tdx, iStd, ierr, nSteps, first, last, idx, classification, lenCode, ldx
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character(len=MAX_LEN_USERNAME) :: newAdviser
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character(len=MAX_LEN_USERNAME+MAX_LEN_STUDENT_CODE) :: tAction
        logical :: changeAdvisers
        character(len=MAX_LEN_XML_LINE) :: mesg
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege

        call html_comment('student_advisers_by_curriculum()')
        nSteps = 25


        ! which curriculum
        call cgi_get_named_string(QUERY_STRING, 'A5', tCollege, ierr)
        targetCollege = index_to_college(tCollege)

        call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
        ldx = index_to_curriculum(tCurriculum)

        ! range
        call cgi_get_named_integer(QUERY_STRING, 'A2', first, ierr)
        call cgi_get_named_integer(QUERY_STRING, 'A3', last, ierr)

        ! student classification
        call cgi_get_named_integer(QUERY_STRING, 'A4', classification, ierr)
        if (ierr/=0) classification = -1

        ! action
        call cgi_get_named_string(QUERY_STRING, 'action'//trim(itoa(first)), tAction, ierr)
        changeAdvisers = ierr==0 .and. trim(tAction)=='Submit'
        mesg = SPACE

        lenCode = len_trim(tCurriculum)

        ! collect students
        n_count = 0
        do tdx=1,NumStudents+NumAdditionalStudents
            iStd = StdRank(tdx)
            if (Student(iStd)%Enlistment(targetTerm)%levelYear /= classification) cycle
#if defined UPLB
            if (ldx/=Student(iStd)%CurriculumIdx) cycle
#else
            ldx = Student(iStd)%CurriculumIdx
            if (targetCollege/=Curriculum(ldx)%CollegeIdx) cycle
            if (Curriculum(ldx)%Code(:lenCode)/=tCurriculum(:lenCode)) cycle
#endif
            n_count = n_count+1
            tArray(n_count) = iStd
        end do

        if (changeAdvisers .and. .not. isRoleOfficial) then

            do tdx=first,last
                iStd = tArray(tdx)
                tStdNo = Student(iStd)%StdNo

                call cgi_get_named_string(QUERY_STRING, 'Adviser:'//trim(tStdNo), newAdviser, ierr)
                if (ierr/=0 .or. len_trim(newAdviser)==0) then ! delete
                    idx = index_to_teacher(Student(iStd)%Adviser)
                    Teacher(idx)%NumAdvisees = Teacher(idx)%NumAdvisees - 1
                    Student(iStd)%Adviser = SPACE
                    mesg = ' : '//trim(tStdNo)//mesg
                else
                    if (Student(iStd)%Adviser/=newAdviser) then
                        Student(iStd)%Adviser = newAdviser
                        idx = index_to_teacher(newAdviser)
                        Teacher(idx)%NumAdvisees = Teacher(idx)%NumAdvisees - 1
                        mesg = ' : '//trim(tStdNo)//mesg
                        call xml_read_student_info(iStd, ierr)
                        StudentInfo%Adviser = newAdviser
                        call xml_student_info(iStd)
                        call log_student_record_change(iStd, 'Adviser is '//newAdviser)
                    end if
                end if
                call student_details_write(unitXML, dirSTUDENTS, iStd)
            end do

            if (mesg/=SPACE) then
                mesg = 'Changed adviser '//mesg
            else
                mesg = 'No adviser changes?'
            end if
        end if

        if (changeAdvisers .and. isRoleOfficial) then
            mesg = sorryMessageOfficial
        end if

        call change_adviser_form(device, n_count, nSteps, tArray(1:n_count), &
            fnAdvisersByCurriculum, tCurriculum, classification, &
            'Students in '//trim(tCurriculum)//' with classification '//txtYear(classification), mesg)

    end subroutine student_advisers_by_curriculum


    subroutine links_to_students (device, fn, iTerm, Scholarship)

        integer, intent (in) :: device, fn
        integer, intent (in), optional :: iTerm
        character(len=*), intent (in), optional :: Scholarship

        integer :: ldx, n_count, tdx, iStd, ierr, ncol, thisTerm, iStat, iYearLevel, advisingCode
        integer, dimension(60,7) :: TimeTable
        logical :: flagIsUp
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum, tProgram
        character(len=MAX_LEN_USERNAME) :: tTeacher
        character(len=256) :: header

        if (present(iTerm)) then
            thisTerm = iTerm
        else
            thisTerm = currentTerm
        end if

        call html_comment('links_to_students(term='//itoa(thisTerm)//')')

        ! collect students
        n_count = 0
        select case (fn)

            case (fnFindStudent)
                call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
                ncol = len_trim(tCurriculum)
                if (ncol>0) then
                    do tdx=1,NumStudents+NumAdditionalStudents
                        iStd = StdRank(tdx)
                        ldx = index(Student(iStd)%StdNo, tCurriculum(:ncol))
                        if (ldx/=0 ) then
                            n_count = n_count+1
                            tArray(n_count) = iStd
                        else
                            ldx = index(Student(iStd)%Name, tCurriculum(:ncol))
                            if (ldx==0 ) then
                                call upper_case(tCurriculum)
                                ldx = index(Student(iStd)%Name, tCurriculum(:ncol))
                                if (ldx==0 ) cycle
                            end if
                            n_count = n_count+1
                            tArray(n_count) = iStd
                        end if
                    end do
                    header = 'Students with "'//tCurriculum(:ncol)//'" in name or number.'
                else
                    header = 'Search student failed: search string not specified.'
                end if

            case (fnOnlineStudents)
                do tdx=1,NumStudents+NumAdditionalStudents
                    iStd = StdRank(tdx)
                    if (Student(iStd)%OnlineStatus<=0) cycle
                    n_count = n_count+1
                    tArray(n_count) = iStd
                end do
                header = 'Online students'

            case (fnListBeneficiaries)
                ! which teacher
                if (present(Scholarship)) then
                    tTeacher = Scholarship
                    call html_comment('links_to_students( ..., Scholarship='//Scholarship//')')
                else
                    call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, ierr)
                    call html_comment('links_to_students( ..., Teacher='//tTeacher//')')
                end if
                do tdx=1,NumStudents+NumAdditionalStudents
                    iStd = StdRank(tdx)
                    if (Student(iStd)%Scholarship/=tTeacher) cycle
                    n_count = n_count+1
                    tArray(n_count) = iStd
                end do
                header = trim(tTeacher)//' Scholarship Beneficiaries'

            case (fnStudentsByCurriculum)
                ! which Curriculum ?
                call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
                targetCurriculum = index_to_curriculum(tCurriculum)
                do tdx=1,NumStudents+NumAdditionalStudents
                    iStd = StdRank(tdx)
                    if (Student(iStd)%CurriculumIdx /= targetCurriculum) cycle
                    n_count = n_count+1
                    tArray(n_count) = iStd
                end do
                header = 'Students in '//tCurriculum

            case (fnStudentsWithConflicts)
                ! which college
                call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
                targetCollege = index_to_college(tCollege)
                do tdx=1,NumStudents+NumAdditionalStudents
                    iStd = StdRank(tdx)
                    if (Curriculum(Student(iStd)%CurriculumIdx)%CollegeIdx /= targetCollege) cycle
                    ! collect classes for student
                    call timetable_meetings_of_student(thisTerm, iStd, 0, ldx, tArray(n_count+1:), TimeTable, flagIsUp)
                    if (flagIsUp) then ! conflict
                        n_count = n_count + 1
                        tArray(n_count) = iStd
                    end if
                end do
                header = trim(tCollege)//' students with schedule conflicts'

            case (fnEnlistmentStatusStudents, fnAdvisingStatusStudents)
                !A1=College(collegeIdx)%Code, &
                !A2=CurrProgCode(idxCurr), &
                !A3=Curriculum(ldx)%Code, &
                !A4=itoa(iYearLevel), &
                !A5=itoa(iStat), &

                ! which college
                call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
                targetCollege = index_to_college(tCollege)
                ! which program
                call cgi_get_named_string(QUERY_STRING, 'A2', tProgram, ierr)
                ! which curriculum
                call cgi_get_named_string(QUERY_STRING, 'A3', tCurriculum, ierr)
                targetCurriculum = index_to_curriculum(tCurriculum)
                ! which classification
                call cgi_get_named_integer(QUERY_STRING, 'A4', iYearLevel, ierr)
                if (ierr==-1) iYearLevel = -99
                ! which status
                call cgi_get_named_integer(QUERY_STRING, 'A5', iStat, ierr)
                if (ierr==-1) iStat = -99

                do tdx=1,NumStudents+NumAdditionalStudents
                    iStd = StdRank(tdx)
                    if (targetCurriculum>0) then
                        if (Student(iStd)%CurriculumIdx /= targetCurriculum) cycle
                    else
                        if (CurrProgCode(Student(iStd)%CurriculumIdx) /= tProgram) cycle
                    end if
#if defined UPLB
                    if (iStat /= -99) then
                        if (fn==fnEnlistmentStatusStudents) then
                            if (Student(iStd)%Enlistment(thisTerm)%statusEnlistment /= iStat) cycle
                        elseif (fn==fnAdvisingStatusStudents) then
                            if (iStat>=PRIORITY_NEW_STUDENT) then
                                if (Student(iStd)%Enlistment(thisTerm)%levelPriority /= iStat) cycle
                            elseif (iStat>=ADVISING_REGULAR) then
                                if (iYearLevel /= -99) then
                                    if (Student(iStd)%Enlistment(thisTerm)%levelYear /= iYearLevel) cycle
                                end if
                            else
                                if (Student(iStd)%Enlistment(thisTerm)%statusAdvising /= iStat) cycle
                            end if
                        end if
                    end if

#else
                    if (iStat /= -99) then
                        if (fn==fnEnlistmentStatusStudents) then
                            if (Student(iStd)%Enlistment(thisTerm)%statusEnlistment /= iStat) cycle
                        elseif (fn==fnAdvisingStatusStudents) then
                            if (Student(iStd)%Enlistment(thisTerm)%statusAdvising /= iStat) cycle
                        end if
                    end if
                    if (iYearLevel /= -99) then
                        if (Student(iStd)%Enlistment(thisTerm)%levelYear /= iYearLevel) cycle
                    end if
#endif
                    n_count = n_count + 1
                    tArray(n_count) = iStd
                end do

                if (targetCurriculum<=0) then
                    header = trim(tProgram)//' students'
                else
                    header = trim(tCurriculum)//' students'
                end if

#if defined UPLB
                if (iStat /= -99) then
                    if (fn==fnEnlistmentStatusStudents) then
                        header = trim(header)//' with enlistment status '//PRIME//trim(txtStatusCode(iStat))//PRIME
                    elseif (fn==fnAdvisingStatusStudents) then
                        if (iStat>=PRIORITY_NEW_STUDENT) then
                            header = trim(header)//' in priority group '//PRIME//trim(txtStatusCode(iStat))//PRIME
                        elseif (iStat>=ADVISING_REGULAR) then
                            if (iYearLevel /= -99) then
                                header = trim(header)//' with classification '//PRIME//trim(txtYear(iYearLevel+10))//PRIME
                            end if
                        else
                            header = trim(header)//' with advising status '//PRIME//trim(txtStatusCode(iStat))//PRIME
                        end if
                    end if
                end if
#else
                if (iYearLevel/=-99) then
                    header = trim(header)//' with classification '//PRIME//trim(txtYear(iYearLevel+10))//PRIME
                end if

                if (iStat/=-99) then
                    if (fn==fnEnlistmentStatusStudents) then
                        header = trim(header)//' with enlistment status '//PRIME//trim(txtStatusCode(iStat))//PRIME
                    elseif (fn==fnAdvisingStatusStudents) then
                        header = trim(header)//' with advising status '//PRIME//trim(txtStatusCode(iStat))//PRIME
                    end if
                end if
#endif


            case (fnAdvisingCode)

                ! student status determined by needs analysis
                call cgi_get_named_integer(QUERY_STRING, 'A2', advisingCode, ierr)

                ! which group
                call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)

                if (trim(tCollege)=='ADVISEES') then
                    header = 'My advisees'//descScholastic(advisingCode)
                    do tdx=1,NumStudents+NumAdditionalStudents
                        iStd = StdRank(tdx)
                        if (Student(iStd)%Adviser /= USERNAME) cycle
                        if (isAnalysisCode(Student(iStd)%Enlistment(thisTerm), advisingCode)) then ! include
                            n_count = n_count + 1
                            tArray(n_count) = iStd
                        end if
                    end do

                else if (trim(tCollege)=='ALL') then
                    header = 'ALL students'//descScholastic(advisingCode)
                    do tdx=1,NumStudents+NumAdditionalStudents
                        iStd = StdRank(tdx)
                        if (isAnalysisCode(Student(iStd)%Enlistment(thisTerm), advisingCode)) then ! include
                            n_count = n_count + 1
                            tArray(n_count) = iStd
                        end if
                    end do

                else ! specific college
                    header = trim(tCollege)//' students'//descScholastic(advisingCode)
                    targetCollege = index_to_college(tCollege)
                    do tdx=1,NumStudents+NumAdditionalStudents
                        iStd = StdRank(tdx)
                        if (Curriculum(Student(iStd)%CurriculumIdx)%CollegeIdx /= targetCollege) cycle
                        if (isAnalysisCode(Student(iStd)%Enlistment(thisTerm), advisingCode)) then ! include
                            n_count = n_count + 1
                            tArray(n_count) = iStd
                        end if
                    end do
                end if

            case default
                header = ' (incorrect function number to list students)'

        end select

        call html_write_header(device, header)
        call html_student_list (device, n_count, tArray, .true., SPACE)

        if (fn==fnListBeneficiaries) then
            write(device,AFORMAT) b_para, 'Distribution of '//trim(UniversityCode)//' students by ', &
                trim(make_href(fnDistributionPSGC, 'home address', A1=tCollege, A2='ALL', post=DOT)), e_para
        end if

        write(device,AFORMAT) horizontal

    end subroutine links_to_students


    function isAnalysisCode(Advice, code)
        type (TYPE_PRE_ENLISTMENT), intent (in) :: Advice
        integer, intent (in) :: code
        logical :: isAnalysisCode

        select case(code)

            case (SCHOLASTIC_FAILED_NONE:SCHOLASTIC_NOT_ENROLLED_LAST_SEM)
                isAnalysisCode = Advice%statusScholastic==code

            case (ANALYSIS_UNRESOLVED_INC_NFE)
                isAnalysisCode = Advice%codeConditional>0

            case (ANALYSIS_NSTP_MISMATCH)
                isAnalysisCode = Advice%codeNSTP>0

            case (ANALYSIS_NO_REMAINING_SUBJECTS)
                isAnalysisCode = Advice%statusAdvising==ADVISING_FINISHED

            case (ANALYSIS_REGULAR)
                isAnalysisCode = Advice%statusAdvising==ADVISING_REGULAR

            !case (ANALYSIS_NOT_FINISHED_NONE_FEASIBLE)
            !    isAnalysisCode = Advice%NRemaining>0

            !case (ANALYSIS_GRADUATING)

            case default
                isAnalysisCode = .false.

        end select

    end function isAnalysisCode


    subroutine recent_student_activity(device)

        integer, intent(in) :: device
        integer :: idxCurr
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
            pre=b_small//' (', post=' ) '//e_small)) )

        !iTmp = year_prefix(Student(targetStudent))
        logFile = trim(dirLOG)//trim(basefile_student(targetStudent))//'.log'
        inquire(file=logFile, exist=logExists)
        if (logExists) then
            write(device,AFORMAT) '<pre>'
            call copy_to_unit(logFile, device)
            write(device,AFORMAT) '</pre>'//horizontal
        else
            write(device,AFORMAT) BRNONEHR
        end if

    end subroutine recent_student_activity


    subroutine change_adviser_form(device, n_count, nSteps, tArray, fn, label, classification, header, mesg)
        integer, intent (in) :: device, fn, n_count, nSteps, classification, tArray(1:n_count)
        character(len=*), intent (in) :: label, header, mesg

        integer :: ldx, tdx, iStd, j, k, l, first, last, skip, nAdvised(3), nClasses(3), iTerm
        logical :: allowed_to_edit

        call html_write_header(device, header, mesg)

        if (n_count == 0) then
            write(device,AFORMAT) BRNONEHR
            return
        end if

        allowed_to_edit = isRole_dean_of_college(targetCollege, orHigherUp)
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
                call make_form_start(device, fn, label, itoa(first), itoa(last), itoa(classification))
            end if

            write(device,AFORMAT) &
                b_tr//b_thal//'#'//e_th, &
                b_thal//'STDNO'//e_th, &
                b_thal//'Name of Student'//e_th, &
                b_thal//'Curriculum'//e_th, &
                b_thal//'Adviser'//e_th//e_tr

            do tdx=first,last
                iStd = tArray(tdx)
                tStdNo = Student(iStd)%StdNo
                ldx = Student(iStd)%CurriculumIdx

                call count_preenlistment(iStd, 0, nClasses, nAdvised)

                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(tdx,2))//'">'// &
                    b_td//trim(itoa(tdx))//DOT//e_td//b_td//tStdNo//e_td, &
                    b_td//trim(Student(iStd)%Name)//e_td//b_td//Curriculum(ldx)%Code//b_td

                if ( allowed_to_edit ) then
                    write(device,AFORMAT) '<select name="Adviser:'//trim(tStdNo)//'">', &
                         '<option value=""> (select)'
                    do l=1,NumTeachers+NumAdditionalTeachers
                        if (Teacher(l)%TeacherId==Student(iStd)%Adviser) then
                            j = 1
                        else
                            if (Department(Teacher(l)%DeptIdx)%CollegeIdx/=Curriculum(ldx)%CollegeIdx) cycle
                            j = 0
                        end if
                        write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(Teacher(l)%TeacherId)//'"> '// &
                            trim(Teacher(l)%Name)
                    end do
                    write(device,AFORMAT) '</select>'//e_td
                else
                    l = index_to_teacher(Student(iStd)%Adviser)
                    write(device,AFORMAT) trim(Teacher(l)%Name)//e_td
                end if

                if ( allowed_to_edit .or. (USERNAME == Student(iStd)%Adviser) ) then

                    write(device,AFORMAT) b_td//b_small
                    if ( allowed_to_edit ) then
                        write(device,AFORMAT) trim(make_href(fnStudentEdit, 'Info', A1=tStdNo, pre=nbsp))
                    end if
                    write(device,AFORMAT) &
                        trim(make_href(fnRecentStudentActivity, 'Log', A1=tStdNo, pre=nbsp)), &
                        trim(make_href(fnStudentGrades, 'Grades', A1=tStdNo, pre=nbsp)), &
                        trim(make_href(fnEditCheckList, 'Checklist', A1=tStdNo, A9=currentTerm, pre=nbsp))

                    if (sum(nClasses)>0) then
                        do iTerm=firstSemester,summerTerm
                            if (nClasses(iTerm)+nAdvised(iTerm)>0) write(device,AFORMAT) &
                                trim(make_href(fnStudentClasses, txtSemester(iTerm+6), A1=tStdNo, A9=iTerm, pre=nbsp))
                        end do
                    end if

                    if (isRoleSysAd) then
                        write(device,AFORMAT) &
                            trim(make_href(fnSwitchUser, 'Login', A1=tStdNo, pre=nbsp, newtab='"_blank"'))
                    end if

                end if

                write(device,AFORMAT) e_small//e_td//e_tr

            end do

            if ( allowed_to_edit ) then
                write(device,AFORMAT) &
                    b_tr//'<td colspan="4">'//nbsp//e_td// &
                    b_td//linebreak//'<input type="submit" name="action'//trim(itoa(first))// &
                    '" value="Submit">'//e_form//linebreak//linebreak//e_td//b_td_nbsp_e_td//e_tr
            end if

        end do

        write(device,AFORMAT) e_table

        write(device,AFORMAT) horizontal

    end subroutine change_adviser_form



    subroutine grade_certification(device)
        integer, intent (in) :: device

        character (len=MAX_LEN_PERSON_NAME) :: tStdName
        integer :: tdx, grd, crse, ierr, i, j, iYear, iTerm

        ! which student?
        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
        call cgi_get_named_integer(QUERY_STRING, 'A2', iYear, ierr)
        call cgi_get_named_integer(QUERY_STRING, 'A3', iTerm, ierr)

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
            b_tr//b_tdac//'Republic of the Philippines'//e_td//e_tr, &
            b_tr//b_tdac//trim(UniversityName)//e_td//e_tr, &
            b_tr//b_tdac//trim(UniversityAddress)//e_td//e_tr, &
            b_tr//b_td//linebreak//e_td//e_tr, &
            b_tr//b_tdac//b_bold//'CERTIFICATION OF GRADES'//e_bold//e_td//e_tr, &
            e_table

        write(device,AFORMAT) linebreak//linebreak//linebreak//'<table border="0" width="100%">', &
            b_tr, &
            '<td width="10%">'//nbsp//e_td, &
            '<td width="80%" colspan="4">'// &
                'TO WHOM IT MAY CONCERN:'//linebreak//linebreak//linebreak// &
                'This is to certify that '//trim(tStdName)// &
                ' was enrolled at '//trim(UniversityName)// &
                ' during the '//trim(txtSemester(iTerm+3))//trim(termQualifier(iTerm+3))// &
                ' of Academic Year '//trim(itoa(iYear))//DASH//trim(itoa(iYear+1))//COMMA// &
                ' in the subjects listed below.'//linebreak//linebreak//e_td, &
            '<td width="10%">'//nbsp//e_td, &
            e_tr, &
            b_tr, &
            '<td width="10%">'//nbsp//e_td, &
            '<td width="80%" colspan="4">'//horizontal// &
            '<td width="10%">'//nbsp//e_td, &
            e_tr, &
            !
            b_tr, &
            '<td width="10%">'//nbsp//e_td, &
            b_td//'SUBJECT'//e_td, &
            b_td//'DESCRIPTIVE TITLE'//e_td, &
            b_tdac//'GRADE'//e_td, &
            b_tdac//'CREDIT'//e_td, &
            '<td width="10%">'//nbsp//e_td, &
            e_tr, &
            b_tr, &
            '<td width="10%">'//nbsp//e_td, &
            '<td width="80%" colspan="4">'//horizontal// &
            '<td width="10%">'//nbsp//e_td, &
            e_tr

        do tdx=1,lenTCG

            if (TCG(tdx)%Code<3 .or. TCG(tdx)%Code==4 .or. TCG(tdx)%Code==5 .or. &
                TCG(tdx)%Used .or. TCG(tdx)%ErrorCode>1) cycle
            ! ignore 4-removal/5-completion records; already in TCG()%ReExam

            if (TCG(tdx)%Year/=iYear .or. TCG(tdx)%Term/=iTerm) cycle

            crse = TCG(tdx)%Subject
            grd = TCG(tdx)%Grade

            write(device,AFORMAT) &
                b_tr, &
                '<td width="10%">'//nbsp//e_td, &
                b_td//trim(Subject(crse)%Name)//e_td, &
                b_td//trim(Subject(crse)%Title)//e_td
            if ( grd==gdxREGD ) then
                write(device,AFORMAT) &
                    b_tdac//b_italic//'No grade'//e_italic//e_td, b_td_nbsp_e_td
            else if ( grd==gdxINC .or. grd==gdxNFE) then
                if (isGrade_passing(TCG(tdx)%ReExam)) then
                    write(device,AFORMAT) &
                        b_tdac//txtGrade(pGrade(grd))//FSLASH// &
                                       txtGrade(pGrade(TCG(tdx)%ReExam))//e_td, &
                        b_tdac//trim(ftoa(Subject(crse)%Units,1))//e_td
                else
                    write(device,AFORMAT) &
                        b_tdac//txtGrade(pGrade(grd))//e_td, b_td_nbsp_e_td
                end if
            else if ( isGrade_passing(grd) ) then
                write(device,AFORMAT) &
                    b_tdac//txtGrade(pGrade(grd))//e_td, &
                    b_tdac//trim(ftoa(Subject(crse)%Units,1))//e_td
            else
                write(device,AFORMAT) &
                    b_tdac//txtGrade(pGrade(grd))//e_td, b_td_nbsp_e_td
            end if
            write(device,AFORMAT) &
                '<td width="10%">'//nbsp//e_td, &
                e_tr

        end do

        write(device,AFORMAT) &
            b_tr, &
            '<td width="10%">'//nbsp//e_td, &
            '<td width="80%" colspan="4">----- '//b_italic//'Nothing follows'//e_italic//' -----'// &
            '<td width="10%">'//nbsp//e_td, &
            e_tr, &
            b_tr, &
            '<td width="10%">'//nbsp//e_td, &
            '<td width="80%" colspan="4">'//horizontal// &
            '<td width="10%">'//nbsp//e_td, &
            e_tr, &
            e_table

        write(device,AFORMAT) linebreak//'<table border="0" width="100%">', &
            b_tr, &
            '<td width="10%">'//nbsp//e_td, &
            '<td width="80%">'// &
                'This certification is issued '//txtMonth(atoi(currentDate(5:6)))//currentDate(7:8)//COMMA// &
                SPACE//currentDate(1:4)//' for reference purposes; ', &
                'it is not valid without the seal of the University or the original signature of the University Registrar.'// &
                e_td, &
            '<td width="10%">'//nbsp//e_td, &
            e_tr, &
            e_table

        write(device,AFORMAT) linebreak//linebreak//linebreak//linebreak//'<table border="0" width="100%">', &
            b_tr, &
            '<td width="30%">'//nbsp//e_td, &
            '<td width="30%">'//nbsp//e_td, &
            '<td width="40%">'//trim(TheRegistrar)//linebreak//trim(titleTheRegistrar)//e_td, &
            e_tr, &
            e_table

    end subroutine grade_certification


    subroutine write_markings (device, nLines, pageNo, nPages)
        integer, intent (in) :: device, nLines, pageNo, nPages
        integer :: idx

        write(device,AFORMAT) '<table width="100%" frame="lhs">', &
            b_tr//'<td align="center" colspan="3">OFFICIAL MARKS'//e_td//e_tr, & ! 1
            b_tr//'<td colspan="3">'//horizontal//e_td//e_tr, & ! 2
            b_tr//b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td//e_tr, & ! 3
            b_tr//b_tdar//'Percentage'//e_td//b_td_nbsp_e_td//b_tdac//'Numeric'//e_td//e_tr, & ! 4
            b_tr//b_tdar//'Equivalent'//e_td//b_td_nbsp_e_td//b_tdac//'Equivalent'//e_td//e_tr, & ! 5
            b_tr//b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td//e_tr, & ! 6
            b_tr//b_tdar//'97 - 100'//e_td//b_td_nbsp_e_td//b_tdac//'1.00'//e_td//e_tr, & ! 7
            b_tr//b_tdar//'94 - 96'//e_td//b_td_nbsp_e_td//b_tdac//'1.25'//e_td//e_tr, & ! 8
            b_tr//b_tdar//'91 - 93'//e_td//b_td_nbsp_e_td//b_tdac//'1.50'//e_td//e_tr, & ! 9
            b_tr//b_tdar//'88 - 90'//e_td//b_td_nbsp_e_td//b_tdac//'1.75'//e_td//e_tr, & ! 10
            b_tr//b_tdar//'85 - 87'//e_td//b_td_nbsp_e_td//b_tdac//'2.00'//e_td//e_tr, & ! 11
            b_tr//b_tdar//'82 - 84'//e_td//b_td_nbsp_e_td//b_tdac//'2.25'//e_td//e_tr, & ! 12
            b_tr//b_tdar//'79 - 81'//e_td//b_td_nbsp_e_td//b_tdac//'2.50'//e_td//e_tr, & ! 13
            b_tr//b_tdar//'76 - 78'//e_td//b_td_nbsp_e_td//b_tdac//'2.75'//e_td//e_tr, & ! 14
            b_tr//b_tdar//'75'//e_td//b_td_nbsp_e_td//b_tdac//'3.00'//e_td//e_tr, & ! 15
            b_tr//b_tdar//'Below 75'//e_td//b_td_nbsp_e_td//b_tdac//'5.00'//e_td//e_tr, & ! 16
            b_tr//b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td//e_tr ! 17


        write(device,AFORMAT) &
            b_tr//'<td colspan="3">'//horizontal//e_td//e_tr, & ! 18
            b_tr//'<td align="center" colspan="3">GENERAL CLASSIFICATION'//e_td//e_tr, & ! 19
            b_tr//'<td colspan="3">'//horizontal//e_td//e_tr, & ! 20
            b_tr//b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td//e_tr, & ! 21
            b_tr//b_tdar//'1.00'//e_td//b_td//' - '//e_td//b_td//'Excellent'//e_td//e_tr, & ! 22
            b_tr//b_tdar//'1.25'//e_td//b_td//' - '//e_td//b_td//'Very Outstanding'//e_td//e_tr, & ! 23
            b_tr//b_tdar//'1.50'//e_td//b_td//' - '//e_td//b_td//'Outstanding'//e_td//e_tr, & ! 24
            b_tr//b_tdar//'1.75'//e_td//b_td//' - '//e_td//b_td//'Very Good'//e_td//e_tr, & ! 25
            b_tr//b_tdar//'2.00'//e_td//b_td//' - '//e_td//b_td//'Good'//e_td//e_tr, & ! 26
            b_tr//b_tdar//'2.25'//e_td//b_td//' - '//e_td//b_td//'Very Satisfactory'//e_td//e_tr, & ! 27
            b_tr//b_tdar//'2.50'//e_td//b_td//' - '//e_td//b_td//'Satisfactory'//e_td//e_tr, & ! 28
            b_tr//b_tdar//'2.75'//e_td//b_td//' - '//e_td//b_td//'Fair'//e_td//e_tr, & !28
            b_tr//b_tdar//'3.00'//e_td//b_td//' - '//e_td//b_td//'Passing'//e_td//e_tr, & ! 29
            b_tr//b_tdar//'5.00'//e_td//b_td//' - '//e_td//b_td//'Failing'//e_td//e_tr, & ! 30
            b_tr//b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td//e_tr ! 31

        write(device,AFORMAT) &
            b_tr//'<td colspan="3">'//horizontal//e_td//e_tr, & ! 32
            b_tr//'<td align="center" colspan="3">SUPPLEMENTARY MARKS'//e_td//e_tr, & ! 33
            b_tr//'<td colspan="3">'//horizontal//e_td//e_tr, & ! 34
            b_tr//b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td//e_tr, & ! 35
            b_tr//b_tdar//'XFR'//e_td//b_td//' - '//e_td//b_td//'Transfer credit'//e_td//e_tr, & ! 36
            b_tr//b_tdar//'NFE'//e_td//b_td//' - '//e_td//b_td//'No final exam'//e_td//e_tr, & ! 37
            b_tr//b_tdar//'INC'//e_td//b_td//' - '//e_td//b_td//'Incomplete'//e_td//e_tr, & ! 38
            b_tr//b_tdar//'DRP'//e_td//b_td//' - '//e_td//b_td//'Dropped'//e_td//e_tr, & ! 39
            b_tr//b_tdar//'P'//e_td//b_td//' - '//e_td//b_td//'Passed'//e_td//e_tr, & ! 40
            b_tr//b_tdar//'F'//e_td//b_td//' - '//e_td//b_td//'Failed'//e_td//e_tr, & ! 41
            b_tr//b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td//e_tr, & ! 42
            b_tr//'<td colspan="3">'//horizontal//e_td//e_tr ! 43

        do idx=44,nLines-2
            write(device,AFORMAT) b_tr//b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td//e_tr
        end do
        write(device,AFORMAT) &
            b_tr//'<td colspan="3">'//horizontal//e_td//e_tr, &
            b_tr//'<td align="center" colspan="3">Page '// trim(itoa(pageNo))//' of '//trim(itoa(nPages))//e_td//e_tr, &
            '</table>'

    end subroutine write_markings


    subroutine write_transcript_header(device, nLines)
        integer, intent (in) :: device
        integer, intent (out) :: nLines

        character (len=6) :: gender

        call get_picture_file(targetStudent, line)
        if (len_trim(line)>0) then
            line = trim(urlPICTURES)//line
        else
            line = trim(urlPICTURES)//'photo.jpg'
        end if

        if (StudentInfo%Gender=='M') then
            gender = 'Male'
        else
            gender = 'Female'
        end if

        write(device,AFORMAT) '<table border="0" width="100%">', &
            b_tr, &
            '<td width="25%" align="left" valign="middle">', &
                '<img src="'//trim(urlPICTURES)//'logo.jpg" alt="logo.jpg" height="120" width="120">', &
            e_td
        write(device,AFORMAT) &
            '<td width="50%" align="center">Republic of the Philippines'//linebreak, &     ! line 1
                '<font size="5">'//trim(UniversityName)//'</font>'//linebreak, &           ! line 2
                '<font size="4">OFFICE OF THE UNIVERSITY REGISTRAR</font>'//linebreak, &   ! line 3
                trim(UniversityAddress)//linebreak, &                                      ! line 4
                trim(UniversityPhone)//linebreak, &                                        ! line 5
                trim(UniversityWeb)//linebreak, &                                          ! line 6
                '<font size="4">OFFICIAL TRANSCRIPT OF RECORDS</font>', &                  ! line 7
            e_td
        write(device,AFORMAT) &
            '<td width="25%" align="right" valign="middle">', &
                '<img src="'//trim(line)//'" height="120" width="120">', &
            e_td, &
            e_tr, &
            e_table


        write(device,AFORMAT) &
            '<table width="100%">', &
            b_tr//'<td colspan="6" width="100%">'//horizontal//'</td>'//e_tr, &        ! line 8
            b_tr, &                                                                     ! line 9
            '<td colspan="3" width="50%">Name: '//b_underline//trim(StudentInfo%Name)//e_underline//e_td, &
            '<td colspan="3" width="50%">ID No.: '//b_underline//trim(Student(targetStudent)%StdNo)//e_underline//e_td, &
            e_tr, &
            b_tr, &                                                                     ! line 10
            '<td colspan="3" width="50%">Home Address: '//b_underline
        if (StudentInfo%HomePSGC == PSGC(NumPSGC)%Code) then
            write(device,AFORMAT) trim(StudentInfo%HomeStreetAddress)
        else
            write(device,AFORMAT) trim(text_PSGC(StudentInfo%HomePSGC))
        end if
        write(device,AFORMAT) e_underline//e_td, &
            '<td colspan="3" width="50%">Gender: '//b_underline//trim(gender)//e_underline//e_td, &
            e_tr, &
            b_tr, &                                                                     ! line 11
            '<td colspan="3" width="50%">Place of Birth: '//b_underline
        if (StudentInfo%BirthPlacePSGC == PSGC(NumPSGC)%Code) then
            write(device,AFORMAT) trim(StudentInfo%BirthPlace)
        else
            write(device,AFORMAT) trim(text_PSGC(StudentInfo%BirthPlacePSGC))
        end if
        write(device,AFORMAT) e_underline//e_td, &
            '<td colspan="3" width="50%">Date of Birth: '//b_underline//trim(StudentInfo%BirthDate)//e_underline//e_td, &
            e_tr, &
            b_tr, &                                                                     ! line 12
            '<td colspan="6" width="100%">Last School Attended: '//b_underline//trim(StudentInfo%LastAttended)// &
                e_underline//e_td, &
            e_tr, &
            b_tr, &                                                                     ! line 13
            '<td colspan="6" width="100%">Degree/Course: '//b_underline//trim(Curriculum(targetCurriculum)%Title)
        if (len_trim(Curriculum(targetCurriculum)%Specialization)>0) then
            write(device,AFORMAT) nbsp//trim(Curriculum(targetCurriculum)%Specialization)
        end if
        write(device,AFORMAT) &
                e_underline//e_td, &
            e_tr, &
            b_tr, &                                                                     ! line 14
            '<td colspan="2" width="34%">Date of Graduation: '//b_underline//trim(StudentInfo%GraduationDate)//e_underline//e_td, &
            '<td colspan="2" width="33%">Admission Data: '//b_underline//trim(StudentInfo%AdmissionData)//e_underline//e_td, &
            '<td colspan="2" width="33%">Admission Date: '//b_underline//trim(StudentInfo%EntryDate)//e_underline//e_td, &
            e_tr, &
            e_table

        ! the column headers
        write(device,AFORMAT) '<table width="100%">', &
            b_tr//'<td colspan="10">'//horizontal//e_td//e_tr !, &                     ! line 15
        nLines = 15

    end subroutine write_transcript_header


    subroutine write_transcript_footer(device, coll)

        integer, intent (in) :: device, coll

        write(device,AFORMAT) e_table, &
            horizontal, 'Remarks: '//trim(StudentInfo%TranscriptRemark)//linebreak, &
                trim(StudentInfo%TranscriptAdditionalRemark)//linebreak, horizontal, &
            '<table width="100%">', &
            b_tr//'<td width="33%" align="left">Prepared by:'//e_td, &
                     '<td width="33%" align="left">Checked by:'//e_td, &
                     '<td width="34%" align="left">'//nbsp//e_td//e_tr
        write(device,AFORMAT) &
            b_tr//'<td width="33%" align="center">'//linebreak// &
                    b_underline//trim(College(coll)%TranscriptPreparer)//e_underline, &
                     linebreak//'Record Custodian'//e_td, &
                     '<td width="33%" align="center">'//linebreak//b_underline// &
                     trim(College(coll)%TranscriptChecker)//e_underline, &
                     linebreak//'Record Custodian'//e_td, &
                     '<td width="34%" align="center">'//linebreak//e_td//e_tr, &
            b_tr//'<td width="66%" colspan="2">'//horizontal//e_td//&
                     b_tdac//b_underline//trim(TheRegistrar)//e_underline//e_td//e_tr, &
            b_tr//'<td width="33%" align="left">Released by:'//e_td, &
                     '<td width="33%" align="left">Date Released:'//e_td, &
                     '<td width="34%" align="center">'//trim(titleTheRegistrar)//e_td//e_tr, &
            e_table//horizontal, &
            b_small//b_italic//'This transcript is not valid without the seal of the University or '// &
            ' the original signature of the University Registrar.'//e_italic//e_small

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
            if (TCG(tdx)%Code==2 .and. isGrade_passing(TCG(tdx)%Grade)) prevtaken = prevtaken+1
        end do
        if (prevtaken>0) then ! there are advance credits

            nLines = nLines + 1
            trLine(nLines) = &
                '<td colspan="5" align="left">'//b_bold//'TRANSFER CREDITS'//e_bold//e_td// &
                b_td_nbsp_e_td//b_td_nbsp_e_td
            nParts = nParts + 1
            ptrPart(nParts) = nLines

            call html_comment(itoa(nParts)//itoa(nLines)//trLine(nLines))

            do tdx=1,lenTCG

                if (TCG(tdx)%Code/=2 .or. .not. isGrade_passing(TCG(tdx)%Grade)) cycle

                crse = TCG(tdx)%Subject
                grd = TCG(tdx)%Grade
                tSubject = Subject(crse)%Name
                line = b_td//trim(tSubject)//e_td// &
                    '<td colspan="4">'//trim(Subject(crse)%Title)//e_td// &
                    b_tdac//'XFR'//e_td

                if (Subject(crse)%Units == 0.0 .or. tSubject(1:5)=='NSTP ') then
                    line = trim(line)//b_td_nbsp_e_td
                else if (isGrade_numeric_pass(grd)) then
                    line = trim(line)//b_tdac//trim(ftoa(Subject(crse)%Units,1))//e_td
                else
                    ! non numeric grade
                    if ( isGrade_passing(grd) ) then
                        line = trim(line)//b_tdac//trim(ftoa(Subject(crse)%Units,1))//e_td
                    else
                        line = trim(line)//b_td_nbsp_e_td
                    end if
                end if

                nLines = nLines + 1
                trLine(nLines) = line

                call html_comment(itoa(nParts)//itoa(nLines)//trLine(nLines))

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
                trLine(nLines) = '<td colspan="5" align="left"> '//b_bold// &
                    trim(text_term_school_year(TCG(tdx)%Term, TCG(tdx)%Year))//e_bold//e_td// &
                    b_td_nbsp_e_td//b_td_nbsp_e_td
                nParts = nParts + 1
                ptrPart(nParts) = nLines

                call html_comment(itoa(nParts)//itoa(nLines)//trLine(nLines))

                prevtaken = TCG(tdx)%Taken
            end if

            crse = TCG(tdx)%Subject
            grd = TCG(tdx)%Grade
            tSubject = Subject(crse)%Name
            line = b_td//trim(tSubject)//e_td// &
                '<td colspan="4">'//trim(Subject(crse)%Title)//e_td

            if (TCG(tdx)%ReExam/=0) then
                line = trim(line)//b_tdac//txtGrade(pGrade(grd))//FSLASH// &
                txtGrade(pGrade(TCG(tdx)%ReExam))//e_td
                grd = TCG(tdx)%ReExam
            else if (grd==gdxREGD) then
                line = trim(line)//b_tdac//b_italic//'No grade'//e_italic//e_td
            else
                line = trim(line)//b_tdac//txtGrade(pGrade(grd))//e_td
            end if

            if (Subject(crse)%Units == 0.0 .or. tSubject(1:5)=='NSTP ') then
                line = trim(line)//b_td_nbsp_e_td
            else if (isGrade_numeric_pass(grd)) then
                ! numeric pass
                if (Subject(crse)%Units*fGrade(grd)/=0.0) then
                    line = trim(line)//b_tdac//trim(ftoa(Subject(crse)%Units,1))//e_td
                else
                    line = trim(line)//b_td_nbsp_e_td
                end if
            else
                ! non numeric grade
                if ( isGrade_passing(grd) ) then
                    line = trim(line)//b_tdac//trim(ftoa(Subject(crse)%Units,1))//e_td
                else
                    line = trim(line)//b_td_nbsp_e_td
                end if
            end if

            nLines = nLines + 1
            trLine(nLines) = line

            call html_comment(itoa(nParts)//itoa(nLines)//trLine(nLines))

        end do

        ! ptr to next (non-existent) part
        ptrPart(nParts+1) = nLines+1

        nPages = 0
        pageNo = 1
        ptrPage(pageNo) = 1
        do tdx=1,nParts
            if (ptrPart(tdx+1)-ptrPage(pageNo) < 51) cycle
            pageNo = pageNo+1
            ptrPage(pageNo) = ptrPart(tdx)
        end do
        nPages = pageNo
        ptrPage(pageNo+1) = ptrPart(nParts+1)

        write(device,AFORMAT) '<html><head>', &
            '<title>'//PROGNAME//VERSION//' transcript for '//trim(StudentInfo%Name)//'</title>', &
            '<style tyle="text/css">', &
            '@page { size:8.5in 14in; margin: 0.1in }', &
            '</style>', '</head><body>'
        do pageNo=1,nPages

            call write_transcript_header(device, nLines)

            write(device,AFORMAT) b_tr//'<td colspan="7" width="80%">'

            write(device,AFORMAT) '<table width="100%">', &
                b_tr, &
                    b_td//'SUBJECT'//e_td, &
                    '<td colspan="4">DESCRIPTIVE TITLE'//e_td, &
                    b_tdac//'GRADE'//e_td, &
                    b_tdac//'UNITS'//e_td, &
                e_tr, & ! 1
                b_tr//'<td colspan="7">'//horizontal//e_td//e_tr ! 2
            lineNo = 2
            do idx=ptrPage(pageNo),ptrPage(pageNo+1)-1
                lineNo = lineNo + 1
                write(device,AFORMAT) b_tr//trim(trLine(idx))//e_tr
            end do
            lineNo = lineNo + 2
            write(device,AFORMAT) &
                b_tr//'<td colspan="7">'//horizontal//e_td//e_tr, &
                b_tr//'<td colspan="7" align="center">'// &
                    b_bold//b_italic//'No entry below this line'//e_italic//e_bold//e_td, &
                e_tr
            do idx=lineNo,54
                write(device,AFORMAT) b_tr//'<td colspan="7">'//nbsp//e_td//e_tr
            end do

            write(device,AFORMAT) '</table>'//e_td//'<td colspan="3" width="20%">'

            call write_markings(device, 54, pageNo, nPages)

            write(device,AFORMAT) e_td//e_tr

            ! write footer
            call write_transcript_footer(device, targetCollege)

        end do


    end subroutine student_transcript


    subroutine student_fees(device)
        integer, intent (in) :: device

        integer :: ierr, idx, jdx, eof, tdx, prevtaken

        logical :: hasNote, hasAssessment, hasDiscount, hasPayment
        character (len=MAX_LEN_FILE_PATH) :: fileAccounts, discountCode, discountDate
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        real :: Tuition, Misc, Lab, Other, Total, Discount, Payment, Adjust, Previous, Receivable
        integer :: iYear, iTerm

        character (len=MAX_LEN_XML_LINE) :: line, value
        character (len=MAX_LEN_XML_TAG) :: tag

        integer, parameter :: MAX_ALL_TRANSACTIONS = 30
        integer :: paymentCode(0:MAX_ALL_TRANSACTIONS)
        character (len=7) :: paymentDate(0:MAX_ALL_TRANSACTIONS), paymentReceipt(0:MAX_ALL_TRANSACTIONS)
        real :: paymentAmount(0:MAX_ALL_TRANSACTIONS)
        integer :: nPayments
        character(len=MAX_LEN_ACCOUNT_CODE) :: tAccountCode

        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
        targetStudent = index_to_student(tStdNo)
        targetCurriculum = Student(targetStudent)%CurriculumIdx
        targetCollege = Curriculum(targetCurriculum)%CollegeIdx

        ! read checklist
        call read_student_records (targetStudent)
        do jdx=1,lenTCG
            TCG(jdx)%Used = .false.
        end do
        ! mark terms with final grades
        prevtaken = -10
        do tdx=1,lenTCG
            ! FINALGRADES only (with completions if applicable)
            if (TCG(tdx)%Code/=3 .or. TCG(tdx)%ErrorCode>1) cycle
            if (prevtaken /= TCG(tdx)%Taken) then
                TCG(tdx)%Used = .true.
                prevtaken = TCG(tdx)%Taken
            end if
        end do

        ! student no. year prefix
        idx = index(tStdNo,DASH)-1
        if (idx<=0) idx = StdNoChars

        call html_write_header(device, trim(tStdNo)//SPACE//trim(Student(targetStudent)%Name)// &
            linebreak//text_curriculum_info(targetCurriculum)//linebreak// &
            linebreak//'History of assessments and payments' )

        write(device,AFORMAT) '<table border="0" width="100%">'

        do tdx=1,lenTCG

            if (.not. TCG(tdx)%Used) cycle

            inquire(file=trim(dirASSESSMENTS)//trim(basefile_student(targetStudent))//dotXML, exist=hasAssessment)
            inquire(file=trim(dirDISCOUNTS)//trim(basefile_student(targetStudent))//dotXML, exist=hasDiscount)
            inquire(file=trim(dirPAYMENTS)//trim(basefile_student(targetStudent))//dotXML, exist=hasPayment)

            if (.not. (hasAssessment .or. hasDiscount .or. hasPayment) ) then
                write(device,AFORMAT) b_tr//'<td colspan="3">'//nbsp//e_td//e_tr, &
                    b_tr//'<td colspan="3">'//b_para//trim(text_term_school_year(TCG(tdx)%Term+3, TCG(tdx)%Year))// &
                        ' - No finance records found.'//e_para//e_td// &
                    e_tr
                cycle
            end if

            write(device,AFORMAT) b_tr//'<td colspan="3">'//nbsp//e_td//e_tr, &
                b_tr//'<td colspan="3">'//b_para// &
                    b_bold//trim(text_term_school_year(TCG(tdx)%Term, TCG(tdx)%Year))//e_bold//e_para//e_td// &
                e_tr, &
                b_tr//'<td colspan="3">'//horizontal//e_td//e_tr, &
                b_tr//'<td width="25%">ASSESSMENTS'//e_td// &
                      '<td width="25%">DISCOUNTS'//e_td// &
                      '<td width="50%">PAYMENTS'//e_td//e_tr// &
                b_tr//'<td colspan="3">'//horizontal//e_td//e_tr, &
                b_tr

            if (hasAssessment) then

                hasAssessment = .false.
                discountDate = SPACE
                Tuition = 0.0
                Misc = 0.0
                Lab = 0.0
                Other = 0.0
                Total = 0.0
                Discount = 0.0
                Payment = 0.0
                Adjust = 0.0
                Previous = 0.0
                Receivable = 0.0
                write(device,AFORMAT) '<td valign="top">', '<table border="0" width="85%">'

                fileAccounts = trim(dirASSESSMENTS)//trim(basefile_student(targetStudent))//dotXML
                open(unit=unitXML, file=trim(fileAccounts), status='old')
                ! examine the file line by line
                do
                    read(unitXML, AFORMAT, iostat=eof) line
                    if (eof<0) exit

                    ! get tag and value if any
                    call xml_parse_line(line, tag, value, eof)
                    if (eof/=0) exit

                    select case (trim(tag))

                        case ('/ASSESSMENTS')
                            exit

                        case ('Assessment')
                            discountDate = SPACE
                            Tuition = 0.0
                            Misc = 0.0
                            Lab = 0.0
                            Other = 0.0
                            Total = 0.0
                            Discount = 0.0
                            Payment = 0.0
                            Adjust = 0.0
                            Previous = 0.0
                            Receivable = 0.0
                            iYear = 0
                            iTerm = 0

                        case ('Date')
                            discountDate = value
                        case ('Tuition')
                            Tuition = atof(value)
                        case ('Misc')
                            Misc = atof(value)
                        case ('Lab')
                            Lab = atof(value)
                        case ('Other')
                            Other = atof(value)
                        case ('Total')
                            Total = atof(value)
                        case ('Discount')
                            Discount = atof(value)
                        case ('Payment')
                            Payment = atof(value)
                        case ('Adjust')
                            Adjust = atof(value)
                        case ('Previous')
                            Previous = atof(value)
                        case ('Receivable')
                            Receivable = atof(value)

                        case ('Term')
                            iTerm = atoi(value)

                        case ('Year')
                            iYear = atoi(value)

                        case ('/Assessment')
                            if (TCG(tdx)%Term==iTerm .and. TCG(tdx)%Year==iYear) then

                                write(device,AFORMAT)  &
                                    b_tr//'<td align="center" colspan="2">'//trim(discountDate)//e_td//e_tr, &
                                    b_tr//b_td//'Tuition'//e_td//b_tdar//ftoa(Tuition,2)//e_td//e_tr, &
                                    b_tr//b_td//'Misc'//e_td//b_tdar//ftoa(Misc,2)//e_td//e_tr, &
                                    b_tr//b_td//'Lab'//e_td//b_tdar//ftoa(Lab,2)//e_td//e_tr, &
                                    b_tr//b_td//'Other'//e_td//b_tdar//ftoa(Other,2)//e_td//e_tr, &
                                    b_tr//'<td colspan="2">'//horizontal//e_td//e_tr, &
                                    b_tr//b_td//'Total'//e_td//b_tdar//ftoa(Total,2)//e_td//e_tr, &
                                    b_tr//'<td colspan="2">'//nbsp//e_td//e_tr, &
                                    !
                                    b_tr//b_td//'Discount'//e_td//b_tdar//ftoa(Discount,2)//e_td//e_tr, &
                                    b_tr//b_td//'Payment'//e_td//b_tdar//ftoa(Payment,2)//e_td//e_tr, &
                                    b_tr//b_td//'Adjust'//e_td//b_tdar//ftoa(Adjust,2)//e_td//e_tr, &
                                    b_tr//b_td//'Previous'//e_td//b_tdar//ftoa(Previous,2)//e_td//e_tr, &
                                    b_tr//'<td colspan="2">'//horizontal//e_td//e_tr
                                if (Receivable>0.0) then
                                    write(device,AFORMAT)  &
                                        b_tr//b_td//b_bold//red//'Outstanding balance'//black//e_bold//e_td// &
                                              b_tdar//b_bold//red//ftoa(Receivable,2)//black//e_bold//e_td//e_tr
                                elseif (Receivable<0.0) then
                                    write(device,AFORMAT)  &
                                        b_tr//b_td//b_bold//green//'Temporary credit'//black//e_bold//e_td//  &
                                              b_tdar//b_bold//green//ftoa(-Receivable,2)//black//e_bold//e_td//e_tr
                                else
                                    write(device,AFORMAT)  &
                                        b_tr//b_td//'Outstanding balance'//e_td//  &
                                              b_tdar//ftoa(Receivable,2)//e_td//e_tr
                                end if
                                write(device,AFORMAT) b_tr//b_td_nbsp_e_td//b_td_nbsp_e_td//e_tr
                                hasAssessment = .true.
                            end if

                        case default
                            ! do nothing
                    end select

                end do
                close(unitXML)
                if (.not. hasAssessment) then
                    write(device,AFORMAT) b_tr//'<td colspan="2">'//b_italic//'(No record of assessment)'//e_italic//e_td//e_tr
                end if
                write(device,AFORMAT)  e_table, e_td


            else
                write(device,AFORMAT) '<td valign="top">', b_para, &
                    b_italic//'(No record of assessment)'//e_italic, &
                    e_para, e_td
            end if

            if (hasDiscount) then

                hasDiscount = .false.
                discountDate = SPACE
                discountCode = SPACE
                Discount = 0.0
                write(device,AFORMAT) '<td valign="top">', '<table border="0" width="75%">'

                fileAccounts = trim(dirDISCOUNTS)//trim(basefile_student(targetStudent))//dotXML
                open(unit=unitXML, file=trim(fileAccounts), status='old')
                ! examine the file line by line
                do
                    read(unitXML, AFORMAT, iostat=eof) line
                    if (eof<0) exit

                    ! get tag and value if any
                    call xml_parse_line(line, tag, value, eof)
                    if (eof/=0) exit

                    select case (trim(tag))

                        case ('/DISCOUNT')
                            exit

                        case ('Discount')
                            discountDate = SPACE
                            discountCode = SPACE
                            Discount = 0.0
                            iYear = 0
                            iTerm = 0

                        case ('Date')
                            discountDate = value
                        case ('Code')
                            discountCode = value
                        case ('Amount')
                            Discount = atof(value)
                        case ('Term')
                            iTerm = atoi(value)
                        case ('Year')
                            iYear = atoi(value)

                        case ('/Discount')
                            if (TCG(tdx)%Term==iTerm .and. TCG(tdx)%Year==iYear) then
                                write(device,AFORMAT) &
                                    b_tr//b_td//'Date:'//e_td//b_tdar//trim(discountDate)//e_td//e_tr, &
                                    b_tr//b_td//'Code:'//e_td//b_tdar//trim(discountCode)//e_td//e_tr, &
                                    b_tr//b_td//'Amount:'//e_td//b_tdar//ftoa(Discount,2)//e_td//e_tr, &
                                    b_tr//b_td_nbsp_e_td//b_td_nbsp_e_td//e_tr
                                hasDiscount = .true.
                            end if

                        case default
                            ! do nothing
                    end select

                end do

                close(unitXML)
                if (.not. hasDiscount) then
                    write(device,AFORMAT) b_tr//'<td colspan="2">'//b_italic//'(No record of discount)'//e_italic//e_td//e_tr
                end if

                write(device,AFORMAT) e_table, e_td

            else
                write(device,AFORMAT) '<td valign="top">', b_para, &
                    b_italic//'(No record of discount)'//e_italic, &
                    e_para, e_td
            end if


            if (hasPayment) then

                hasPayment = .false.
                hasNote = .false.
                paymentDate = SPACE
                paymentReceipt = SPACE
                paymentCode = 0.0
                paymentAmount = 0.0

                write(device,AFORMAT) '<td valign="top">', '<table border="0" width="85%">', &
                    b_tr//b_td//'Date'//e_td//b_td//'Receipt'//e_td//b_tdar//'Amount'//e_td// &
                    b_td_nbsp_e_td//b_td//'Account'//e_td//e_tr, &
                    b_tr//'<td colspan="5">'//horizontal//e_td//e_tr

                fileAccounts = trim(dirPAYMENTS)//trim(basefile_student(targetStudent))//dotXML
                open(unit=unitXML, file=trim(fileAccounts), status='old')
                ! examine the file line by line
                do
                    read(unitXML, AFORMAT, iostat=eof) line
                    if (eof<0) exit

                    ! get tag and value if any
                    call xml_parse_line(line, tag, value, eof)
                    if (eof/=0) exit

                    select case (trim(tag))

                        case ('/PAYMENTS')
                            exit

                        case ('Payment')
                            iYear = 0
                            iTerm = 0
                            nPayments = 0

                        case ('Date')
                            paymentDate(nPayments+1) = value

                        case ('Receipt')
                            paymentReceipt(nPayments+1) = value

                        case ('Account')
                            nPayments = nPayments+1
                            eof = index(value, COMMA)
                            tAccountCode = value(1:eof-1)
                            paymentCode(nPayments) = index_to_account_code(tAccountCode)
                            paymentAmount(nPayments) = atof(value(eof+1:))

                        case ('Term')
                            iTerm = atoi(value)

                        case ('Year')
                            iYear = atoi(value)

                        case ('/Payment')
                            if (TCG(tdx)%Term==iTerm .and. TCG(tdx)%Year==iYear) then
                                do jdx=1,nPayments
                                    if (index(AccountDescription(paymentCode(jdx)), '(*)')>0) hasNote = .true.
                                    write(device,AFORMAT) b_tr, &
                                        b_td//trim(paymentDate(jdx))//e_td, &
                                        b_td//trim(paymentReceipt(jdx))//e_td, &
                                        b_tdar//trim(ftoa(paymentAmount(jdx),2))//e_td, b_td_nbsp_e_td, &
                                        b_td//trim(AccountDescription(paymentCode(jdx)))//e_td, &
                                        e_tr
                                end do
                                write(device,AFORMAT) b_tr//b_td_nbsp_e_td//b_td_nbsp_e_td//e_tr
                                hasPayment = .true.
                            end if

                        case default
                            ! do nothing
                    end select

                end do

                close(unitXML)
                if (.not. hasPayment) then
                    write(device,AFORMAT) b_tr//'<td colspan="5">'//b_italic//'(No record of payments)'//e_italic//e_td//e_tr
                end if

                if (hasNote) then
                    write(device,AFORMAT) b_tr//'<td colspan="5">'//horizontal//e_td//e_tr, &
                        b_tr//'<td colspan="4">'//nbsp//e_td// &
                              b_td//' (*) '//b_italic//'Not included in assessment'//e_italic//e_td//e_tr
                    hasNote = .false.
                end if
                write(device,AFORMAT) e_table, e_td

            else
                write(device,AFORMAT) '<td valign="top">', b_para, &
                    b_italic//'(No record of payments)'//e_italic, &
                    e_para, e_td
            end if

            write(device,AFORMAT) e_tr

        end do
        write(device,AFORMAT) e_table, horizontal

    end subroutine student_fees

end module EditSTUDENT
