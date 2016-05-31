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


module EditENLISTMENT

    use HTML

    implicit none

    integer, private :: maxListLength = 200

contains


    subroutine enlistment_grades (device, thisTerm)

        integer, intent (in) :: device, thisTerm

        integer :: gdx, ldx, n_count, tdx, iStd, ierr, iSect, ncol, iSubj, idx_select, n_changes
        integer :: k, first, last, skip
        character(len=MAX_LEN_CLASS_ID) :: tClassId, tAction, disabled
        character (len=MAX_LEN_TEXT_GRADE) :: tGrade
        character(len=255) :: header, mesg
        logical :: okToEdit
#if defined UPLB
#else
        integer :: specialGrade(4) = (/ gdxREGD, gdxNFE, gdxINC, gdxDRP /)
#endif

        call html_comment('enlistment_grades()')

        ! which section?
        call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, ierr)

        ! range
        call cgi_get_named_integer(QUERY_STRING, 'A2', first, ierr)
        call cgi_get_named_integer(QUERY_STRING, 'A3', last, ierr)

        ! action
        call cgi_get_named_string(QUERY_STRING, 'action'//trim(itoa(first)), tAction, ierr)


        targetSection = index_to_section(tClassId, thisTerm)
        if (targetSection==0) then
            call html_college_links(device, CollegeIdxUser, mesg='Section "'//trim(tClassId)//'" not found.')
            return
        end if
        ! assume listed teacher first is the teacher-in-charge for the section
        targetTeacher = Section(thisTerm,targetSection)%TeacherIdx(1)

        iSubj = Section(thisTerm,targetSection)%SubjectIdx
#if defined UPLB
        targetDepartment = Subject(iSubj)%DeptIdx
#else
        targetDepartment = Section(thisTerm,targetSection)%DeptIdx
#endif
        targetCollege = Department(targetDepartment)%CollegeIdx

        ! collect students
        mesg = SPACE
        n_count = 0
        n_changes = 0
        do tdx=1,NumStudents+NumAdditionalStudents
            iStd = StdRank(tdx)
            do ncol=1,Student(iStd)%Enlistment(thisTerm)%lenSubject
                iSect = Student(iStd)%Enlistment(thisTerm)%Section(ncol)
                if (iSect==0) cycle
                if (targetSection == iSect) then
                    tArray(n_count+1) = iStd
                    tArray(n_count+2) = Student(iStd)%Enlistment(thisTerm)%Grade(ncol)
                    n_count = n_count+2
                    exit
                elseif (Student(iStd)%Enlistment(thisTerm)%Subject(ncol)==iSubj .and. isSubject_lecture_lab(iSubj)) then
                    ldx = index(Section(thisTerm,iSect)%ClassId,DASH)
                    if (ldx>0) then ! student is accommodated in a lab section
                        if (trim(tClassId)==Section(thisTerm,iSect)%ClassId(:ldx-1)) then ! lab of lecture
                            tArray(n_count+1) = iStd
                            tArray(n_count+2) = Student(iStd)%Enlistment(thisTerm)%Grade(ncol)
                            n_count = n_count+2
                            exit
                        end if
                    end if
                end if
            end do
        end do

        if (trim(tAction)=='Submit' .and. .not. isRoleOfficial) then

            do tdx=first,last,2 ! 1,n_count, 2
                iStd = tArray(tdx)
                call cgi_get_named_string(QUERY_STRING, trim(Student(iStd)%StdNo), tGrade, ierr)
                if (ierr==0) then
                    gdx = index_to_grade(tGrade)
                    if (tArray(tdx+1)==gdx) cycle ! no change
                    do ncol=1,Student(iStd)%Enlistment(thisTerm)%lenSubject
                        if (targetSection /= Student(iStd)%Enlistment(thisTerm)%Section(ncol)) cycle
                        Student(iStd)%Enlistment(thisTerm)%Grade(ncol) = gdx
                        tArray(tdx+1) = gdx
                        n_changes = n_changes+1
                        call log_student_record_change(iStd, 'Grade in '//trim(tClassId)//' is '//txtGrade(pGrade(gdx)) )
                        !call html_comment(itoa(n_changes)//Student(iStd)%StdNo//tGrade//itoa(gdx))
                        call student_details_write(unitXML, dirSTUDENTS, iStd)
                        call finalgrades_write(currentYear, iStd)
                        exit
                    end do
                end if
            end do

        end if

        if (trim(tAction)=='Mark' .and. .not. isRoleOfficial) then
            Section(thisTerm,targetSection)%GradeSubmissionDate = currentDate
            mesg = 'Marked "Received:" date as '//Section(thisTerm,targetSection)%GradeSubmissionDate
            n_changes = -1
            if (targetTeacher>0) &
                call log_teacher_record_change(targetTeacher, trim(mesg)//' for '//tClassId)
        end if

        if (trim(tAction)=='Clear' .and. .not. isRoleOfficial) then
            mesg = 'Cleared "Received:" date of '//Section(thisTerm,targetSection)%GradeSubmissionDate
            Section(thisTerm,targetSection)%GradeSubmissionDate = SPACE
            n_changes = -1
            if (targetTeacher>0) &
                call log_teacher_record_change(targetTeacher, trim(mesg)//' for '//tClassId)
        end if

        if (n_changes>0) then
            mesg = 'Updated grades of '//trim(itoa(n_Changes))//' students in '//tClassId
            if (targetTeacher>0) &
                call log_teacher_record_change(targetTeacher, mesg)
        else if (n_changes==-1) then
            call class_details_write(unitXML, thisTerm, dirCLASSES(thisTerm), targetSection)
        else if (len_trim(tAction)>0 .and. isRoleOfficial) then
            mesg = '"'//trim(tAction)//'" failed. '//sorryMessageOfficial
        end if

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

        call html_write_header(device, 'GRADESHEET', mesg)
        if (len_trim(Section(thisTerm,targetSection)%GradeSubmissionDate)==0) then
            mesg = ' not yet submitted'
            okToEdit = .true.
            disabled = SPACE
        else
            mesg = ' submitted on '//Section(thisTerm,targetSection)%GradeSubmissionDate
            okToEdit = .false.
            disabled = ' disabled'
        end if
        call list_sections_to_edit(device, thisTerm, ncol-n_count, tArray(n_count+1:), &
            fnTeacherClasses, SPACE, 'Other classes', .true., .true., &
            b_bold//trim(Subject(iSubj)%Name)//' - '//trim(header)//e_bold//SPACE// &
            trim(make_href(fnPrintableGradesheet, 'Printable', &
            A1=tClassId, A9=thisTerm, pre=nbsp//b_small//'( ', post=trim(mesg)//' )'//e_small//linebreak//linebreak, &
            newtab='"_blank"')) ) !, .true.)

        if (n_count == 0) then
            write(device,AFORMAT) linebreak//'(No students in this section?)'
        else

            if ( n_count>50 ) then
                skip = 50
            else
                skip = n_count
            end if

            write(device,AFORMAT) linebreak//'<table border="0" width="90%">'

            do k=1, n_count, skip
                first = k
                last = min(first+skip-1, n_count)

                call make_form_start(device, fnGradesheet, tClassId, itoa(first), itoa(last), A9=thisTerm)
                write(device,AFORMAT) &
                    b_tr//b_thal//'#'//e_th// &
                    b_thal//'Student No.'//e_th, &
                    b_thal//'Grade'//e_th, &
                    b_thal//'Student Name'//e_th// &
                    b_thal//'Curriculum'//e_th//e_tr

                do tdx=first,last,2 ! 1,n_count, 2
                    iStd = tArray(tdx)
                    gdx = tArray(tdx+1)
                    ldx = Student(iStd)%CurriculumIdx
                    ierr = (tdx+1)/2
                    write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(ierr,2))//'">'// &
                        b_td//trim(itoa(ierr))//DOT//e_td, &
                        b_td//Student(iStd)%StdNo//e_td, &
                        b_td//'<select '//trim(disabled)//' name="'//trim(Student(iStd)%StdNo)//'">'
#if defined UPLB
                    do ncol=1,19
                        if (gdx/=ncol) then
                            idx_select = 0
                        else
                            idx_select = 1
                        end if
                        write(device,AFORMAT) '<option value="'//trim(itoa(ncol))//'" '//trim(selected(idx_select))//'> '// &
                            txtGrade(pGrade(ncol))
                    end do
#else
                    do ncol=ZERO_PERCENT_GRADE+70,ZERO_PERCENT_GRADE+100
                        tGrade = txtGrade(pGrade(ncol))
                        if (gdx/=ncol) then
                            idx_select = 0
                        else
                            idx_select = 1
                        end if
                        write(device,AFORMAT) '<option value="'//trim(tGrade)//'" '//trim(selected(idx_select))//'> '//tGrade
                    end do

                    do ierr=1,4
                        ncol = specialGrade(ierr)
                        tGrade = txtGrade(pGrade(ncol))
                        if (gdx/=ncol) then
                            idx_select = 0
                        else
                            idx_select = 1
                        end if
                        write(device,AFORMAT) '<option value="'//trim(tGrade)//'" '//trim(selected(idx_select))//'> '//tGrade
                    end do

#endif
                    write(device,AFORMAT) '</select>'//e_td, &
                        b_td//trim(Student(iStd)%Name)//e_td// &
                        b_td//trim(Curriculum(ldx)%Code)//e_td//e_tr

                end do
                write(device,AFORMAT) b_tr//b_td_nbsp_e_td//b_td_nbsp_e_td// &
                    b_td//'<input '//trim(disabled)//' type="submit" name="action'//trim(itoa(first))//'" value="Submit">', &
                    e_form//e_td, &
                    b_td_nbsp_e_td//b_td_nbsp_e_td//e_tr// &
                    b_tr//'<td colspan="5">'//nbsp//linebreak//nbsp//e_tr

            end do
            write(device,AFORMAT) e_table

        end if

        if (isRole_admin_of_college(targetCollege)) then
            write(device,AFORMAT) horizontal
            call make_form_start(device, fnGradesheet, tClassId, A9=thisTerm)
            if (len_trim(Section(thisTerm,targetSection)%GradeSubmissionDate)==0) then
                write(device,AFORMAT) 'Hardcopy of gradesheet not yet received. ', &
                    '<input type="submit" name="action-1" value="Mark"> this gradesheet as "Received:" today.'//e_form
            else
                write(device,AFORMAT) 'Hardcopy of gradesheet received on '// &
                    trim(Section(thisTerm,targetSection)%GradeSubmissionDate)//DOT, &
                    '<input type="submit" name="action-1" value="Clear"> this "Received:" date to make changes.'//e_form
            end if
        end if

        write(device,AFORMAT) horizontal

    end subroutine enlistment_grades



    subroutine enlistment_forced (device, thisTerm)
        integer, intent (in) :: device, thisTerm

        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character(len=MAX_LEN_CLASS_ID) :: tClassId, tAction
        integer :: loc, fdx, iSubj, iSect, ierr, otherSect, tLen1
        logical :: isEnlisted
        integer, dimension(60,7) :: TimeTable
        logical :: conflicted
        character(len=255) :: statusMesg
        real :: unitsEnlisted

        ! which section?
        call cgi_get_named_string(QUERY_STRING, 'A2', tClassId, ierr)
        iSect = index_to_section(tClassId, thisTerm)
        if (ierr/=0 .or. iSect==0) then
            call html_college_links(device, CollegeIdxUser, mesg='Section '//tClassId//' not found.')
            return
        end if
        iSubj = Section(thisTerm,iSect)%SubjectIdx
        targetCollege = Department(Section(thisTerm,iSect)%DeptIdx)%CollegeIdx

        call recalculate_available_seats(thisTerm)

        ! which student?
        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
        targetStudent = index_to_student(tStdNo)

        ! action
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)

        if (trim(tAction)=='Add' .and. .not. isRoleOfficial) then

            if (targetStudent==0) then
                call class_list (device, thisTerm, idx=iSect, mesg='Student '//tStdNo//' not found.')
                return
            end if

            ! check if already enlisted
            isEnlisted = .false.
            loc = 0
            otherSect = 0
            statusMesg = SPACE
            do fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                if (Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx)/=iSubj) cycle ! not the subject
                loc = fdx
                if (Student(targetStudent)%Enlistment(thisTerm)%Section(fdx)==iSect) then
                    isEnlisted = .true.
                    exit
                elseif (Student(targetStudent)%Enlistment(thisTerm)%Section(fdx)>0) then
                    otherSect = Student(targetStudent)%Enlistment(thisTerm)%Section(fdx)
                    exit
                end if
            end do

            ! really force add if isAdmin
            if (isRole_admin_of_college(targetCollege) ) then

                if (isEnlisted) then ! already enlisted in this class
                    statusMesg = 'already in '//tClassId

                else if (otherSect>0) then ! already enlisted in another class
                    statusMesg = 'already in another section '//Section(thisTerm,otherSect)%ClassId

                else

                    ! add subject if not advised
                    if (loc==0) then
                        do fdx=Student(targetStudent)%Enlistment(thisTerm)%lenSubject, &
                               Student(targetStudent)%Enlistment(thisTerm)%NPriority+1,-1
                            Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx+1) = &
                                Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx)
                            Student(targetStudent)%Enlistment(thisTerm)%Section(fdx+1) = &
                                Student(targetStudent)%Enlistment(thisTerm)%Contrib(fdx)
                            Student(targetStudent)%Enlistment(thisTerm)%Grade(fdx+1) = &
                                Student(targetStudent)%Enlistment(thisTerm)%Grade(fdx)
                            Student(targetStudent)%Enlistment(thisTerm)%Contrib(fdx+1) = &
                                Student(targetStudent)%Enlistment(thisTerm)%Contrib(fdx)
                        end do
                        loc = Student(targetStudent)%Enlistment(thisTerm)%NPriority+1
                        Student(targetStudent)%Enlistment(thisTerm)%NPriority = loc
                        Student(targetStudent)%Enlistment(thisTerm)%Subject(loc) = iSubj
                        Student(targetStudent)%Enlistment(thisTerm)%lenSubject = 1 + &
                            Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                    end if

                    Student(targetStudent)%Enlistment(thisTerm)%statusAdvising = ADVISING_IRREGULAR
                    Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment = max(ENLISTMENT_MANUAL, &
                        Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment)

                    Student(targetStudent)%Enlistment(thisTerm)%Section(loc) = iSect
                    Student(targetStudent)%Enlistment(thisTerm)%Grade(loc) = gdxREGD
                    Student(targetStudent)%Enlistment(thisTerm)%Contrib(loc) = 1.0
                    Section(thisTerm,iSect)%RemSlots = Section(thisTerm,iSect)%RemSlots - 1

                    statusMesg = 'added to '//tClassId
                    call log_student_record_change(targetStudent, statusMesg)
                    call student_details_write(unitXML, dirSTUDENTS, targetStudent)
                    call finalgrades_write(currentYear, targetStudent)

                end if

                call class_list (device, thisTerm, idx=iSect, mesg='Student '//tStdNo//statusMesg)

                return

            end if


            if (loc==0) then ! not advised to take subject
                statusMesg = 'not advised to take '//Subject(iSubj)%Name

            else if (isEnlisted) then ! already enlisted in this class
                statusMesg = 'already in '//tClassId

            else if (otherSect>0) then ! already enlisted in another class
                statusMesg = 'already in another section '//Section(thisTerm,otherSect)%ClassId

            else if (Section(thisTerm,iSect)%RemSlots<=0) then ! class is full
                statusMesg = 'not added - '//trim(tClassId)//' is full.'

            else

                ! count units enlisted
                unitsEnlisted = 0.0
                do fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                    if (Student(targetStudent)%Enlistment(thisTerm)%Section(fdx) /= 0) then
                        unitsEnlisted = unitsEnlisted + Subject(Section(thisTerm,fdx)%SubjectIdx)%Units
                    end if
                end do

                ! allowed load will not be exceeded ?
                unitsEnlisted = unitsEnlisted + Subject(iSubj)%Units
                if ( (Student(targetStudent)%Enlistment(thisTerm)%AllowedLoad - unitsEnlisted + 0.1) > 0.0 ) then

                    ! collect classes for student; check if conflict
                    call timetable_meetings_of_student(thisTerm, targetStudent, 0, tLen1, tArray, TimeTable, conflicted)
                    if (conflicted .or. is_conflict_timetable_with_section(thisTerm, iSect, TimeTable)) then
                        statusMesg = 'not added to '//trim(tClassId)//' due to schedule conflict.'
                    else
                        statusMesg = 'added to '//tClassId
                        Student(targetStudent)%Enlistment(thisTerm)%statusAdvising = ADVISING_IRREGULAR
                        Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment = max(ENLISTMENT_MANUAL, &
                            Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment)

                        Student(targetStudent)%Enlistment(thisTerm)%Section(loc) = iSect
                        Student(targetStudent)%Enlistment(thisTerm)%Grade(loc) = gdxREGD
                        Student(targetStudent)%Enlistment(thisTerm)%Contrib(loc) = 1.0
                        Section(thisTerm,iSect)%RemSlots = Section(thisTerm,iSect)%RemSlots - 1
                        call log_student_record_change(targetStudent, statusMesg)
                        call student_details_write(unitXML, dirSTUDENTS, targetStudent)
                        call finalgrades_write(currentYear, targetStudent)
                    end if

                else
                    statusMesg = 'not added - allowed units will be exceeded'
                end if

            end if

        else if (.not. isRoleOfficial) then ! assume 'Delete'

            if (trim(tStdNo)=='ALL') then ! delete ALL enlisted
                statusMesg = 'delisted from '//tClassId
                do loc=1,NumStudents+NumAdditionalStudents
                    do fdx=1,Student(loc)%Enlistment(thisTerm)%lenSubject
                        if (Student(loc)%Enlistment(thisTerm)%Section(fdx) /= iSect) cycle
                        Student(targetStudent)%Enlistment(thisTerm)%statusAdvising = ADVISING_IRREGULAR
                        Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment = max(ENLISTMENT_MANUAL, &
                            Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment)

                        Student(loc)%Enlistment(thisTerm)%Section(fdx) = 0
                        Student(loc)%Enlistment(thisTerm)%Grade(fdx) = 0
                        Section(thisTerm,iSect)%RemSlots = Section(thisTerm,iSect)%RemSlots + 1
                        call log_student_record_change(loc, statusMesg)
                        call student_details_write(unitXML, dirSTUDENTS, loc)
                        call finalgrades_write(currentYear, loc)
                        exit
                    end do
                end do

            else if (targetStudent==0) then
                call class_list (device, thisTerm, idx=iSect, mesg='Student '//tStdNo//' not found.')
                return

            else
                ! check if enlisted
                isEnlisted = .false.
                loc = 0
                do fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                    if (Student(targetStudent)%Enlistment(thisTerm)%Section(fdx) == iSect) then
                        isEnlisted = .true.
                        loc = fdx
                    end if
                end do
                if (.not. isEnlisted) then
                    statusMesg = 'is NOT in '//tClassId
                else
                    Student(targetStudent)%Enlistment(thisTerm)%statusAdvising = ADVISING_IRREGULAR
                    Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment = max(ENLISTMENT_MANUAL, &
                        Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment)

                    Student(targetStudent)%Enlistment(thisTerm)%Section(loc) = 0
                    Student(targetStudent)%Enlistment(thisTerm)%Grade(loc) = 0
                    Section(thisTerm,iSect)%RemSlots = Section(thisTerm,iSect)%RemSlots + 1
                    statusMesg = 'deleted from '//tClassId
                    call log_student_record_change(targetStudent, statusMesg)
                    call student_details_write(unitXML, dirSTUDENTS, targetStudent)
                    call finalgrades_write(currentYear, targetStudent)
                end if

            end if

        end if

        if (len_trim(tAction)>0 .and. isRoleOfficial) then
            statusMesg = ' : "'//trim(tAction)//'" failed. '//sorryMessageOfficial
        end if

        call class_list (device, thisTerm, idx=iSect, mesg='Student '//tStdNo//statusMesg)

    end subroutine enlistment_forced


    subroutine enlistment_edit (device, thisTerm)
        integer, intent (in) :: device, thisTerm

        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character(len=MAX_LEN_BLOCK_CODE) :: tBlock
        character(len=MAX_LEN_CLASS_ID) :: tClassId
        character(len=3*MAX_LEN_SUBJECT_CODE) :: tAction
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject
        integer :: bdx, iBlk, fdx, mdx, iSubj, lect, iSect, ierr, tLen1, tLen2, pos, n_opts, idx_opt
        integer :: n_changes, nPassedPE, nAdvisedPE, nEnlistedPE, collegeOfStudent
        real :: unitsEnlisted
        integer, dimension(60,7) :: TimeTable
        logical :: conflicted, matched, addPE
        character(len=255) :: mesg
        logical :: isDirtyFORM5, selfEnlistPE, selfEnlistALL, allowed_to_edit, allowed_to_show
        type (TYPE_PRE_ENLISTMENT) :: Advice

        ! which student?
        !if (present(given)) then
        !    tStdNo = given
        !else
            call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
        !end if
        targetStudent = index_to_student(tStdNo)
        targetCurriculum = Student(targetStudent)%CurriculumIdx
        collegeOfStudent = Curriculum(targetCurriculum)%CollegeIdx

        call recalculate_available_seats(thisTerm)
        isDirtyFORM5 = .false.

        ! check for other arguments
        mesg = SPACE
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
        if (ierr==-1) then
            call cgi_get_named_string(QUERY_STRING, 'A2', tAction, ierr)
        end if

        if (.not. isRole_admin_of_college(collegeOfStudent) .and. &
            Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment>=ENLISTMENT_LOCKED) then
            tAction = 'NotAllowed'
        end if

        selfEnlistPE = trim(USERNAME)==tStdNo .and. College(collegeOfStudent)%isAllowed(ToSelfEnlistPE,thisTerm)
        selfEnlistALL = trim(USERNAME)==tStdNo .and. College(collegeOfStudent)%isAllowed(ToSelfEnlistALL,thisTerm)

        if (trim(tAction)=='Add' .and. .not. isRoleOfficial) then
            ! count units enlisted
            unitsEnlisted = 0.0
            do fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                if (Student(targetStudent)%Enlistment(thisTerm)%Section(fdx) /= 0) then
                    unitsEnlisted = unitsEnlisted + Subject(Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx))%Units
                !call html_comment(Section(thisTerm,Student(targetStudent)%Enlistment(thisTerm)%Section(thisTerm,fdx))%ClassId// &
                !    ftoa(Subject(Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx))%Units,1) )
                end if
            end do
!call html_comment('Current  enlisted/allowed = '//ftoa(unitsEnlisted,1)//'/'//ftoa(Student(targetStudent)%Enlistment(thisTerm)%AllowedLoad,1) )

            call cgi_get_named_string(QUERY_STRING, 'A3', tClassId, ierr)
            iSect = index_to_section(tClassId, thisTerm)
            if (iSect>0) then ! target of action is indexed by iSect

                if (selfEnlistPE .and. tClassId(1:3)/='PE ') then
                    mesg = trim(tClassId)//' NOT added: only PE can be added/deleted at this time.'

                else if (Section(thisTerm,iSect)%RemSlots>0) then
                    iSubj = Section(thisTerm,iSect)%SubjectIdx

                    ! allowed load will not be exceeded ?
                    unitsEnlisted = unitsEnlisted + Subject(iSubj)%Units
                    !call html_comment('Proposed enlisted/allowed = '//ftoa(unitsEnlisted,1)//'/'// &
                    !    ftoa(Student(targetStudent)%Enlistment(thisTerm)%AllowedLoad,1) )
                    !write(device,*) '<pre>', Student(targetStudent)%Enlistment(thisTerm)%AllowedLoad - unitsEnlisted, '</pre>'
                    if ( (Student(targetStudent)%Enlistment(thisTerm)%AllowedLoad - unitsEnlisted + 0.1) > 0.0 ) then

                        pos = 0
                        do fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                            if (Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx)==iSubj) then
                                Student(targetStudent)%Enlistment(thisTerm)%Section(fdx) = iSect
                                Student(targetStudent)%Enlistment(thisTerm)%Grade(fdx) = gdxREGD
                                Section(thisTerm,iSect)%RemSlots = Section(thisTerm,iSect)%RemSlots - 1
                                pos = fdx
                                Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment = ENLISTMENT_MANUAL
                                if (Student(targetStudent)%Enlistment(thisTerm)%statusAdvising<ADVISING_REGULAR) then
                                    Student(targetStudent)%Enlistment(thisTerm)%statusAdvising = ADVISING_IRREGULAR
                                end if
                                exit
                            end if
                        end do
                        if (pos==0) then ! add
                            if (tClassId(1:3)=='PE ') then ! shift down
                                do fdx=Student(targetStudent)%Enlistment(thisTerm)%lenSubject,1,-1
                                    Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx+1) = &
                                        Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx)
                                    Student(targetStudent)%Enlistment(thisTerm)%Section(fdx+1) = &
                                        Student(targetStudent)%Enlistment(thisTerm)%Section(fdx)
                                    Student(targetStudent)%Enlistment(thisTerm)%Grade(fdx+1) = &
                                        Student(targetStudent)%Enlistment(thisTerm)%Grade(fdx)
                                end do
                                ! add PE at the beginning
                                Student(targetStudent)%Enlistment(thisTerm)%Subject(1) = iSubj
                                Student(targetStudent)%Enlistment(thisTerm)%Section(1) = iSect
                                Student(targetStudent)%Enlistment(thisTerm)%Grade(1) = gdxREGD
                                Student(targetStudent)%Enlistment(thisTerm)%lenSubject =  &
                                    Student(targetStudent)%Enlistment(thisTerm)%lenSubject + 1
                                Student(targetStudent)%Enlistment(thisTerm)%NPriority = &
                                    Student(targetStudent)%Enlistment(thisTerm)%NPriority + 1
                                Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment = ENLISTMENT_MANUAL
                                if (Student(targetStudent)%Enlistment(thisTerm)%statusAdvising<ADVISING_REGULAR) then
                                    Student(targetStudent)%Enlistment(thisTerm)%statusAdvising = ADVISING_IRREGULAR
                                end if
                                Section(thisTerm,iSect)%RemSlots = Section(thisTerm,iSect)%RemSlots - 1
                                mesg = 'Added '//tClassId
                                isDirtyFORM5 = .true.

                            else
                                mesg = trim(tClassId)//' NOT added: class was cancelled'

                            end if

                        else
                            mesg = 'Added '//tClassId
                            isDirtyFORM5 = .true.

                        end if

                    else
                        mesg = trim(Subject(iSubj)%Name)//' ('//trim(ftoa(Subject(iSubj)%Units,1))// &
                        ' units) NOT added: proposed total '//trim(ftoa(unitsEnlisted,1))// &
                        ' units will exceed allowed '//trim(ftoa(Student(targetStudent)%Enlistment(thisTerm)%AllowedLoad,1))// &
                        ' units'
                    end if

                else
                    mesg = trim(tClassId)//' NOT added: class already full'

                end if
            end if

        end if

        if (trim(tAction)=='Block' .and. .not. isRoleOfficial) then

            call cgi_get_named_string(QUERY_STRING, 'A3', tBlock, ierr)
            iBlk = index_to_block(tBlock, thisTerm)
            if (iBlk>0) then ! target block found

                do bdx=1,Block(thisTerm,iBlk)%NumClasses
                    iSubj = Block(thisTerm,iBlk)%Subject(bdx)
                    iSect = Block(thisTerm,iBlk)%Section(bdx)
                    if (iSect==0) then ! section not specified in block
                        mesg = 'NOT added (no section): '//trim(Subject(iSubj)%Name)//'; '//mesg
                        cycle
                    end if
                    tClassId = Section(thisTerm,iSect)%ClassId
                    matched = .false.
                    do fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                        if (iSubj/=Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx)) cycle
                        matched = .true.
                        exit
                    end do
                    if (matched) then ! indexed by bdx/fdx

                        if (Section(thisTerm,iSect)%RemSlots>0) then
                            do fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                                if (Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx)/=iSubj) cycle
                                Student(targetStudent)%Enlistment(thisTerm)%Section(fdx) = iSect
                                Student(targetStudent)%Enlistment(thisTerm)%Grade(fdx) = gdxREGD
                                Section(thisTerm,iSect)%RemSlots = Section(thisTerm,iSect)%RemSlots - 1
                                Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment = ENLISTMENT_MANUAL
                                exit
                            end do
                            mesg = 'Added '//trim(tClassId)//'; '//mesg
                            isDirtyFORM5 = .true.
                        else
                            mesg = 'NOT added '//trim(tClassId)//' (full); '//mesg
                        end if

                    else
                        mesg = 'NOT advised '//trim(Subject(iSubj)%Name)//'; '//mesg
                    end if

                end do

            else
                mesg = 'Block "'//trim(tBlock)//'" not found'
            end if

        end if

        if (trim(tAction)=='Del' .and. .not. isRoleOfficial) then

            call cgi_get_named_string(QUERY_STRING, 'A3', tClassId, ierr)
            iSect = index_to_section(tClassId, thisTerm)
            if (selfEnlistPE .and. tClassId(1:3)/='PE ') then
                mesg = 'Cannot delete '//trim(tClassId)//': only PE can be added/deleted at this time.'
            else if (iSect>0) then ! target of action is indexed by iSect
                if (tClassId(1:3)=='PE ') then ! delete subject and section
                    ! find & clear location of target PE
                    pos = 0
                    do fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                        if (iSect==Student(targetStudent)%Enlistment(thisTerm)%Section(fdx)) then
                            Student(targetStudent)%Enlistment(thisTerm)%Section(fdx) = 0
                            Student(targetStudent)%Enlistment(thisTerm)%Grade(fdx) = 0
                            Section(thisTerm,iSect)%RemSlots = Section(thisTerm,iSect)%RemSlots + 1
                            Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment = ENLISTMENT_MANUAL
                            pos = fdx
                            exit
                        end if
                    end do
                    ! still there, or already deleted by someone else?
                    if (pos/=0) then ! shift
                        do fdx=pos,Student(targetStudent)%Enlistment(thisTerm)%lenSubject-1
                            Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx) = &
                                Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx+1)
                            Student(targetStudent)%Enlistment(thisTerm)%Section(fdx) = &
                                Student(targetStudent)%Enlistment(thisTerm)%Section(fdx+1)
                            Student(targetStudent)%Enlistment(thisTerm)%Grade(fdx) = &
                                Student(targetStudent)%Enlistment(thisTerm)%Grade(fdx+1)
                        end do
                        ! clear last location
                        Student(targetStudent)%Enlistment(thisTerm)%Subject( &
                            Student(targetStudent)%Enlistment(thisTerm)%lenSubject) = 0
                        Student(targetStudent)%Enlistment(thisTerm)%Section( &
                            Student(targetStudent)%Enlistment(thisTerm)%lenSubject) = 0
                        Student(targetStudent)%Enlistment(thisTerm)%Grade( &
                            Student(targetStudent)%Enlistment(thisTerm)%lenSubject) = 0
                        ! reduce
                        Student(targetStudent)%Enlistment(thisTerm)%lenSubject = &
                            Student(targetStudent)%Enlistment(thisTerm)%lenSubject-1
                        if (pos<=Student(targetStudent)%Enlistment(thisTerm)%NPriority) then
                            Student(targetStudent)%Enlistment(thisTerm)%NPriority = &
                                Student(targetStudent)%Enlistment(thisTerm)%NPriority-1
                        end if
                        mesg = 'Deleted '//tClassId
                        isDirtyFORM5 = .true.
                    else
                        mesg = 'No longer enlisted in '//tClassId
                    end if

                else ! delete section only
                    matched = .false.
                    do fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                        if (iSect==Student(targetStudent)%Enlistment(thisTerm)%Section(fdx)) then
                            Student(targetStudent)%Enlistment(thisTerm)%Section(fdx) = 0
                            Student(targetStudent)%Enlistment(thisTerm)%Grade(fdx) = 0
                            Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment = ENLISTMENT_MANUAL
                            Section(thisTerm,iSect)%RemSlots = Section(thisTerm,iSect)%RemSlots + 1
                            matched = .true.
                            exit
                        end if
                    end do
                    ! already deleted by someone else?
                    if (matched) then
                        mesg = 'Deleted '//tClassId
                        isDirtyFORM5 = .true.
                    else
                        mesg = 'No longer enlisted in '//tClassId
                    end if
                end if
            end if

        end if


        if (trim(tAction)=='Switch' .and. .not. isRoleOfficial) then

            call collect_advice(Advice, n_changes, mesg)
            mesg = 'Update ADVICE '//mesg

            do fdx=1,Advice%lenSubject

                iSubj = Advice%Subject(fdx)
                tSubject = Subject(iSubj)%Name
                ! check if already enlisted
                do mdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                    if (Student(targetStudent)%Enlistment(thisTerm)%Subject(mdx)/=iSubj) cycle
                    if (Student(targetStudent)%Enlistment(thisTerm)%Section(mdx)>0) then
!call html_comment('Retained section '//Section(Student(targetStudent)%Enlistment(thisTerm)%Section(mdx))%Classid)
                    else
!call html_comment('Retained subject '//tSubject)
                    end if
                    Advice%Section(fdx) = Student(targetStudent)%Enlistment(thisTerm)%Section(mdx)
                    Advice%Grade(fdx) = gdxREGD
                    Student(targetStudent)%Enlistment(thisTerm)%Section(mdx) = 0 ! remove section
                    exit
                end do

            end do

            ! free slots for deleted sections
            do fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                iSect = Student(targetStudent)%Enlistment(thisTerm)%Section(fdx)
                if (iSect>0) then
                    Section(thisTerm,iSect)%RemSlots = Section(thisTerm,iSect)%RemSlots + 1
!call html_comment('Freed a seat in '//Section(thisTerm,iSect)%ClassId)
                end if
            end do
            ! update
            if (Advice%statusAdvising<ADVISING_REGULAR) then
                Advice%statusAdvising = ADVISING_IRREGULAR
            end if
            Advice%statusEnlistment = ENLISTMENT_MANUAL
            Student(targetStudent)%Enlistment(thisTerm) = Advice
            isDirtyFORM5 = .true.

        end if

        if (trim(tAction)=='CONFIRM Schedule' .and. .not. isRoleOfficial) then

            Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment = ENLISTMENT_CONFIRMED
            mesg = 'CONFIRMED the schedule for '//txtSemester(thisTerm+3)//termQualifier(thisTerm+3)
            isDirtyFORM5 = .true.

        end if

        if (trim(tAction)=='LOCK Schedule' .and. .not. isRoleOfficial) then

            ! keep only enlisted subjects
            mdx = 0
            do fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                if (Student(targetStudent)%Enlistment(thisTerm)%Section(fdx)==0) cycle
                mdx = mdx + 1
                Student(targetStudent)%Enlistment(thisTerm)%Subject(mdx) = Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx)
                Student(targetStudent)%Enlistment(thisTerm)%Section(mdx) = Student(targetStudent)%Enlistment(thisTerm)%Section(fdx)
                Student(targetStudent)%Enlistment(thisTerm)%Grade(mdx) = Student(targetStudent)%Enlistment(thisTerm)%Grade(fdx)
                Student(targetStudent)%Enlistment(thisTerm)%Contrib(mdx) = Student(targetStudent)%Enlistment(thisTerm)%Contrib(fdx)
            end do
            Student(targetStudent)%Enlistment(thisTerm)%lenSubject = mdx
            Student(targetStudent)%Enlistment(thisTerm)%NPriority = mdx
            Student(targetStudent)%Enlistment(thisTerm)%NAlternates = 0
            Student(targetStudent)%Enlistment(thisTerm)%NCurrent = 0
            Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment = ENLISTMENT_LOCKED
            mesg = 'LOCKED schedule for '//txtSemester(thisTerm+3)//termQualifier(thisTerm+3)
            isDirtyFORM5 = .true.

        end if

        if (trim(tAction)=='UNLOCK Schedule' .and. .not. isRoleOfficial) then

            Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment = ENLISTMENT_CONFIRMED
            mesg = 'UNLOCKED schedule for '//txtSemester(thisTerm+3)//termQualifier(thisTerm+3)
            isDirtyFORM5 = .true.

        end if

        if (trim(tAction)=='PAID' .and. .not. isRoleOfficial) then

            Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment = ENLISTMENT_PAID
            mesg = 'Full or partial payment made for '//txtSemester(thisTerm+3)//termQualifier(thisTerm+3)
            isDirtyFORM5 = .true.

        end if

        if (trim(tAction)=='EXCLUDE Student' .and. .not. isRoleOfficial) then

            call initialize_pre_enlistment(Student(targetStudent)%Enlistment(thisTerm))
            Student(targetStudent)%Enlistment(thisTerm)%statusAdvising = ADVISING_EXCLUDED
            Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment = ENLISTMENT_NEEDED
            Student(targetStudent)%Enlistment(thisTerm)%levelYear = YEAR_NOT_SET
            mesg = 'Delisted student from classes for '//txtSemester(thisTerm+3)//termQualifier(thisTerm+3)
            isDirtyFORM5 = .true.

        end if

        if (trim(tAction)=='REINSTATE Student' .and. .not. isRoleOfficial) then

            Student(targetStudent)%Enlistment(thisTerm)%statusAdvising = ADVISING_NEEDED
            Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment = ENLISTMENT_NEEDED
            Student(targetStudent)%Enlistment(thisTerm)%levelYear = YEAR_NOT_SET
            mesg = 'Reinstated student for '//trim(txtSemester(thisTerm+3)//termQualifier(thisTerm+3))// &
                '. Click ''Checklist'' to advise.'
            isDirtyFORM5 = .true.

        end if

        if (len_trim(tAction)>0 .and. isRoleOfficial) then
            mesg = '"'//trim(tAction)//'" failed. '//sorryMessageOfficial
        end if

        if (trim(tAction)=='NotAllowed') then
            mesg = 'No changes made. Schedule already locked.'
        end if


        if (isDirtyFORM5) then
            call student_details_write(unitXML, dirSTUDENTS, targetStudent)
            call finalgrades_write(currentYear, targetStudent)
            call log_student_record_change(targetStudent, mesg )
        end if

        if (trim(USERNAME)==tStdNo) then
            !allowed_to_edit = (selfEnlistAll .or. selfEnlistPE) .and. Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment<ENLISTMENT_LOCKED
            allowed_to_edit = (selfEnlistAll .or. selfEnlistPE) .and. &
                Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment<ENLISTMENT_LOCKED
               !(Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment==ENLISTMENT_AUTOMATIC .or. &
               ! Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment==ENLISTMENT_MANUAL )
            allowed_to_show = College(collegeOfStudent)%isAllowed(ToShowTEACHERS,thisTerm)
        end if

        if (isRole_adviser_of_student(targetStudent, orHigherUp)) then
            allowed_to_edit = College(collegeOfStudent)%isAllowed(ToEnlistStudents,thisTerm) &
                .and. Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment<ENLISTMENT_LOCKED
            allowed_to_show = College(collegeOfStudent)%isAllowed(ToShowTEACHERS,thisTerm)
        end if

        allowed_to_show = allowed_to_show .or. isRole_dean_of_college(collegeOfStudent, orHigherUp)
        allowed_to_edit = allowed_to_edit .or. &
            (isRole_dean_of_college(collegeOfStudent, orHigherUp) .and. &
            Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment<ENLISTMENT_LOCKED)

        if (isRoleBenefactor) allowed_to_edit = .false.

        call html_write_header(device, trim(Student(targetStudent)%StdNo)//SPACE//trim(Student(targetStudent)%Name)//linebreak// &
            trim(text_curriculum_info(Student(targetStudent)%CurriculumIdx)), mesg)

        ! collect classes for student
        call timetable_meetings_of_student(thisTerm, targetStudent, 0, tLen1, tArray, TimeTable, conflicted)

        if (tlen1>2) then
            call list_sections_to_edit(device, thisTerm, tLen1, tArray, &
                fnChangeMatriculation, tStdNo, 'Del', allowed_to_edit, allowed_to_show, &
                b_bold//'Enlisted subjects'//e_bold//nbsp//trim(make_href(fnPrintableSchedule, 'Printable', &
                A1=tStdNo, A9=thisTerm, pre=b_small//'(', post=')'//e_small//linebreak, newtab='"_blank"')) )

            if (thisTerm/=currentTerm) then ! past or future term
                ! print timetable and exit if not current term
                call timetable_display(device, thisTerm, TimeTable)
                if (thisTerm>currentTerm) then ! past or future term
                    mdx = 0
                    write(device,AFORMAT) linebreak//b_bold//'Feasible subjects'//e_bold//':'
                    do fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                        if (Student(targetStudent)%Enlistment(thisTerm)%Section(fdx)==0) then
                            tSubject = Subject(Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx))%Name
                            mdx = mdx+1
                            write(device,AFORMAT) linebreak//trim(itoa(mdx))//'). '//trim(tSubject)//' - '// &
                                trim(Subject(Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx))%Title)
                        end if
                    end do
                end if
            end if

        end if

        if (thisTerm/=currentTerm) then  ! not current term
            write(device,AFORMAT) horizontal
            return
        end if

        ! current term
        call make_form_start(device, fnChangeMatriculation, A1=tStdNo, A9=thisTerm)
        write(device,AFORMAT) b_para, &
            b_bold//'Enlistment status'//e_bold//' is '''// &
            txtStatusCode(Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment)//''' ('// &
            trim(descStatusCode(Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment))//')'//DOT, &
            ' If enabled, possible actions are:', '<ul>', b_item

        select case (Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment)

            case(ENLISTMENT_NEEDED) ! Not yet enlisted

                select case (Student(targetStudent)%Enlistment(thisTerm)%statusAdvising)

                    case(ADVISING_NEEDED) ! Not advised yet
                        write(device,AFORMAT) linebreak, 'Not yet advised. '
                        if (trim(USERNAME)==tStdNo) then ! student view
                            write(device,AFORMAT) 'See your adviser.'
                        elseif (isRole_adviser_of_student(targetStudent)) then ! adviser view
                            write(device,AFORMAT) 'Click ''Checklist'' then select subjects for student'
                        elseif (isRole_admin_of_college(collegeOfStudent)) then ! Registrar/staff view
                            write(device,AFORMAT) &
                                'Click ''Checklist'' then select subjects for student', e_item, b_item, &
                                'Run ''Auto-advise students''', e_item, b_item, &
                                '<input type="submit" name="action" value="EXCLUDE Student">'
                        end if

                    case(ADVISING_FINISHED) ! No remaining subjects
                        write(device,AFORMAT) 'Celebrate?'

                    case(ADVISING_EXCLUDED) ! Excluded
                        if (trim(USERNAME)==tStdNo) then ! student view
                            write(device,AFORMAT) 'See the Registrar to be included as a current student.'
                        else if (isRole_adviser_of_student(targetStudent)) then ! adviser view
                            write(device,AFORMAT) 'Advise student to see the Registrar to be included as a current student.'
                        elseif (isRole_admin_of_college(collegeOfStudent)) then ! Registrar/staff view
                            write(device,AFORMAT) '<input type="submit" name="action" value="REINSTATE Student">'
                        end if

                    case(ADVISING_REGULAR, ADVISING_IRREGULAR)
                        ! Auto-advised by Registrar via 'Needs Analysis'
                        if (trim(USERNAME)==tStdNo) then ! student view
                            write(device,AFORMAT) 'See adviser to change the list of feasible subjects', e_item, b_item
                            if (allowed_to_edit) then
                                write(device,AFORMAT) 'Click ''Add'' or ''Del'' to enlist or delist classes'
                            else
                                write(device,AFORMAT) red//'Self-enlistment is disabled at this time'//e_color
                            end if
                        elseif (isRole_adviser_of_student(targetStudent)) then ! adviser view
                            write(device,AFORMAT) 'Click ''Checklist'' to change the advised subjects', e_item, b_item
                            if (allowed_to_edit) then
                                write(device,AFORMAT) &
                                    'Click ''Find block'' to find suitable blocked sections', e_item, b_item, &
                                    'Click ''Add'' or ''Del'' to enlist or delist classes'
                            else
                                write(device,AFORMAT) red//' Enlistment of students by advisers is disabled at this time'//e_color
                            end if
                        elseif (isRole_admin_of_college(collegeOfStudent)) then ! Registrar/staff view
                            write(device,AFORMAT) 'Click ''Checklist'' to change the advised subjects', e_item, b_item, &
                                'Click ''Find block'' to find suitable blocked sections', e_item, b_item, &
                                'Click ''Add'' or ''Del'' to enlist or delist classes', e_item, b_item, &
                                'Run ''Auto-preenlist into blocks''', e_item, b_item, &
                                '<input type="submit" name="action" value="EXCLUDE Student">'
                        end if

                    case default
                        write(device,AFORMAT) '(List possible advising actions)'

                end select

            case(ENLISTMENT_AUTOMATIC, ENLISTMENT_MANUAL)
                ! Auto-enlisted by Registrar via 'Preenlist'
                ! Enlisted by adviser, or self-enlisted
                if (trim(USERNAME)==tStdNo) then ! student view
                    if (sum(Student(targetStudent)%Enlistment(thisTerm)%Section)>0) then
                        write(device,AFORMAT) &
                            '<input type="submit" name="action" value="CONFIRM Schedule"> ', &
                            'to signify that the Registrar can lock the schedule and print the assessment form. '//linebreak, &
                            b_italic//'After confirmation, while the schedule is not locked, your adviser '// &
                            'can modify the subjects; and, if self-enlistment is still enabled, you can change classes.'// &
                            e_italic, e_item, b_item
                    end if
                    if (allowed_to_edit) then
                        write(device,AFORMAT) 'Click ''Add'' or ''Del'' to enlist or delist classes'
                    else
                        write(device,AFORMAT) red//'Self-enlistment is disabled at this time.'//e_color
                    end if
                elseif (isRole_adviser_of_student(targetStudent)) then ! adviser view
                    write(device,AFORMAT) 'Click ''Checklist'' to change the advised subjects', e_item, b_item
                    if (allowed_to_edit) then
                        write(device,AFORMAT) 'Click ''Add'' or ''Del'' to enlist or delist classes'
                    else
                        write(device,AFORMAT) red//' Enlistment of students by advisers is disabled at this time'//e_color
                    end if
                elseif (isRole_admin_of_college(collegeOfStudent)) then ! Registrar/staff view
                    write(device,AFORMAT) 'Click ''Checklist'' to change the advised subjects', e_item, b_item, &
                        'Click ''Add'' or ''Del'' to enlist or delist classes', e_item, b_item, &
                        '<input type="submit" name="action" value="EXCLUDE Student">'
                end if

            case(ENLISTMENT_CONFIRMED) ! Schedule confirmed by student or Registrar via 'CONFIRM Schedule'
                if (trim(USERNAME)==tStdNo) then ! student view
                    write(device,AFORMAT) 'See adviser to change the list of feasible subjects', e_item, b_item
                    if (allowed_to_edit) then
                        write(device,AFORMAT) 'Click ''Add'' or ''Del'' to enlist or delist classes. '// &
                            'Re-confirm after any changes.'
                    else
                        write(device,AFORMAT) red//'Self-enlistment is disabled at this time'//e_color
                    end if
                elseif (isRole_adviser_of_student(targetStudent)) then ! adviser view
                    write(device,AFORMAT) 'Click ''Checklist'' to change the advised subjects', e_item, b_item
                    if (allowed_to_edit) then
                        write(device,AFORMAT) 'Click ''Add'' or ''Del'' to enlist or delist classes.'
                    else
                        write(device,AFORMAT) red//'Enlistment of students by advisers is disabled at this time'//e_color
                    end if
                elseif (isRole_admin_of_college(collegeOfStudent)) then ! Registrar/staff view
                    write(device,AFORMAT) 'Click ''Checklist'' to change the advised subjects', e_item, b_item, &
                        'Click ''Add'' or ''Del'' to enlist or delist classes', e_item, b_item, &
                        '<input type="submit" name="action" value="LOCK Schedule"> '//nbsp//' or '//nbsp// &
                        '<input type="submit" name="action" value="EXCLUDE Student">'
                end if

            case(ENLISTMENT_LOCKED) ! Schedule printed and locked by Registrar via 'LOCK'
                if (trim(USERNAME)==tStdNo .or. isRole_adviser_of_student(targetStudent)) then ! student view or adviser view
                    write(device,AFORMAT) 'See the Registrar if changes to subjects and/or classes are needed'
                elseif (isRole_admin_of_college(collegeOfStudent)) then ! Registrar/staff view
                    write(device,AFORMAT) &
                        '<input type="submit" name="action" value="UNLOCK Schedule"> '//nbsp//' or '//nbsp// &
                        '<input type="submit" name="action" value="PAID"> '//nbsp//' or '//nbsp// &
                        '<input type="submit" name="action" value="EXCLUDE Student">'
                end if

            case (ENLISTMENT_PAID) ! Partial or full payment made
                if (trim(USERNAME)==tStdNo .or. isRole_adviser_of_student(targetStudent)) then ! student view or adviser view
                    write(device,AFORMAT) 'See the Registrar if changes to subjects and/or classes are needed'
                elseif (isRole_admin_of_college(collegeOfStudent)) then ! Registrar/staff view
                    write(device,AFORMAT) &
                        '<input type="submit" name="action" value="UNLOCK Schedule"> '//nbsp//' or '//nbsp// &
                        '<input type="submit" name="action" value="EXCLUDE Student">'
                end if

            case default
                write(device,AFORMAT) '(List possible enlistment actions)'

        end select

        write(device,AFORMAT) e_item, '</ul>', e_para, e_form


        if (tLen1>0) then
            call timetable_display(device, thisTerm, TimeTable)
        end if

        ! PE needed?
        addPE = .false.
        ! read checklist
        call read_student_records (targetStudent)

        nPassedPE = 0 ! how many passed PE
        do fdx=1,lenTCG
            if (TCG(fdx)%Code/=3 .or. TCG(fdx)%Used .or. TCG(fdx)%ErrorCode>1) cycle
            if (Subject(TCG(fdx)%Subject)%Name(1:3)/='PE ') cycle
            if ( isGrade_passing(TCG(fdx)%Grade) .or. isGrade_passing(TCG(fdx)%ReExam) ) nPassedPE = nPassedPE + 1
        end do

        nAdvisedPE = 0 ! how many advised but not enlisted
        nEnlistedPE = 0 ! how many enlisted
        do fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
            tSubject = Subject(Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx))%Name
            if (tSubject(1:3)/='PE ') cycle ! not PE
            if (Student(targetStudent)%Enlistment(thisTerm)%Section(fdx)==0) then
                nAdvisedPE = nAdvisedPE + 1 ! advised/not enlisted
            else
                nEnlistedPE = nEnlistedPE + 1 ! enlisted
            end if
        end do
!        call html_comment('Passed PE = '//trim(itoa(nPassedPE)), 'Advised PE = '//trim(itoa(nAdvisedPE)), &
!            'Enlisted PE = '//trim(itoa(nEnlistedPE)) )

        !addPE = (nPassedPE + nEnlistedPE + nAdvisedPE < 4) .and. (nEnlistedPE + nAdvisedPE <= 2) .and. &
        addPE = (nPassedPE + nEnlistedPE < 4) .and. (nEnlistedPE <= 2) !.and. allowed_to_edit

        ! make list of available sections for alternate subjects that fit the schedule of student
        tLen2 = 0
        do fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
            if (Student(targetStudent)%Enlistment(thisTerm)%Section(fdx)/=0) cycle ! already enlisted
            tLen2 = tLen2 + 1
        end do

        if (tLen2>0 .or. addPE) then !  .and. allowed_to_edit) then
            mdx = 0
            write(device,AFORMAT) linebreak//b_bold//'Other feasible subjects'//e_bold//':'
            if (addPE) then
                write(device,AFORMAT) linebreak//trim(itoa(mdx))//'). <a href="#PE">PE</a> - Physical Education'
            end if
            do fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                if (Student(targetStudent)%Enlistment(thisTerm)%Section(fdx)==0) then
                    tSubject = Subject(Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx))%Name
                    if (addPE .and. tSubject(1:3)=='PE ') cycle ! add below
                    mdx = mdx+1
                    write(device,AFORMAT) linebreak//trim(itoa(mdx))//'). <a href="#'// &
                        trim(tSubject)//'">'//trim(tSubject)//'</a> - '// &
                        trim(Subject(Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx))%Title)
                end if
            end do
            write(device,AFORMAT) linebreak

            if (addPE) then

                tSubject = 'PE'
                n_opts = 0
                do iSect=1,NumSections(thisTerm)
                    if (Section(thisTerm,iSect)%ClassId(1:3)/='PE ') cycle ! not the right subject
                    if (is_conflict_timetable_with_section(thisTerm, iSect, TimeTable)) cycle ! in conflict
                    ! place options at end of Section(thisTerm,) array
                    idx_opt = NumSections(thisTerm) + n_opts + 1
                    Section(thisTerm,idx_opt) = Section(thisTerm,iSect)
                    n_opts = n_opts + 1
                end do !  iSect=1,NumSections

                call timetable_undesirability(n_opts, thisTerm, TimeTable)

                ! available sections that fit schedule - can be added
                tLen2 = 0
                do iSect=1,n_opts
                    idx_opt = UndesirabilityRank(iSect)
                    if (Section(thisTerm,NumSections(thisTerm)+idx_opt)%RemSlots<=0) cycle ! section not available
                    !write(*,*) iSect, idx_opt, Section(thisTerm,NumSections+idx_opt)%ClassId, -Undesirability(idx_opt)
                    do mdx=1,Section(thisTerm,NumSections(thisTerm)+idx_opt)%NMeets
                        tArray(tLen1+tLen2+1) = NumSections(thisTerm)+idx_opt
                        tArray(tLen1+tLen2+2) = mdx
                        tArray(tLen1+tLen2+3) = -Undesirability(idx_opt)
                        tLen2 = tLen2+3
                    end do !  mdx=1,Section(thisTerm,iSect)%NMeets
                end do
                if (tLen2>0) then ! there are sections that can be added
                    ! end of list markers
                    tArray(tLen1+tLen2+1) = 0
                    tArray(tLen1+tLen2+2) = 0
                    tArray(tLen1+tLen2+3) = 0
                    call list_sections_to_edit(device, thisTerm, tLen2, tArray(tLen1+1), &
                        fnChangeMatriculation, tStdNo, 'Add', allowed_to_edit, allowed_to_show, &
                        '<a name="'//trim(tSubject)//'"></a>'//linebreak//horizontal// &
                        green//b_bold//'AVAILABLE sections'//e_color//' in '//trim(tSubject)// &
                        ' that fit existing schedule, sorted by undesirability.'//e_bold)

                end if

            end if

            ! non-PE
            do fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                if (Student(targetStudent)%Enlistment(thisTerm)%Section(fdx)/=0) cycle ! already enlisted
                iSubj = Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx) ! index to subject
                tSubject = Subject(iSubj)%Name
                if (addPE .and. tSubject(1:3)=='PE ') cycle

!call html_comment('Alternate subject - '//tSubject)
                n_opts = 0

                do iSect=1,NumSections(thisTerm)
                    if (iSubj/=Section(thisTerm,iSect)%SubjectIdx) cycle ! not the right subject

                    ! not in student's college
                    !if (Department(Section(thisTerm,iSect)%DeptIdx)%CollegeIdx/=Curriculum(targetCurriculum)%CollegeIdx) cycle ! not in student's college

                    ! not in ADMIN or student's college
                    !if (Department(Section(thisTerm,iSect)%DeptIdx)%CollegeIdx/=NumColleges .and. &
                    !    Department(Section(thisTerm,iSect)%DeptIdx)%CollegeIdx/=Curriculum(targetCurriculum)%CollegeIdx ) cycle

                    ! do not add lecture class if a lect-lab section
                    if (isSubject_lecture_lab(iSubj) .and. is_lecture_class(iSect, thisTerm)) then
                        cycle
                    end if

                    ! check for conflict
                    if (is_conflict_timetable_with_section(thisTerm, iSect, TimeTable)) then
!call html_comment('   Available, but schedule conflict - '//Section(thisTerm,iSect)%ClassId)
                        cycle
                    end if
                    ! lab section of lect+lab subject or lab-only subject, or section of lect-only subject, is OK
                    ! if lab section of lect+lab subject, check if lecture schedule also fits

                    ! place options at end of Section(thisTerm,) array
                    idx_opt = NumSections(thisTerm) + n_opts + 1
                    Section(thisTerm,idx_opt) = Section(thisTerm,iSect)
                    if (isSubject_lecture_lab(iSubj)) then ! find the lecture section
                        pos = index(Section(thisTerm,iSect)%ClassId, DASH)
                        tClassId = Section(thisTerm,iSect)%ClassId(:pos-1)
                        lect = index_to_section(tClassId, thisTerm)
                        if (is_conflict_timetable_with_section(thisTerm, lect, TimeTable)) then ! lecture class is not OK
!call html_comment('   Available, but lecture class schedule conflict - '//Section(thisTerm,lect)%ClassId)
                            cycle
                        end if

!call html_comment('OPTION IS - '//tClassId)

                        ! add lecture schedule to lab schedule
                        do mdx=1,Section(thisTerm,lect)%NMeets
                            pos = Section(thisTerm,idx_opt)%NMeets + 1
                            Section(thisTerm,idx_opt)%DayIdx(pos) = Section(thisTerm,lect)%DayIdx(mdx)
                            Section(thisTerm,idx_opt)%bTimeIdx(pos) = Section(thisTerm,lect)%bTimeIdx(mdx)
                            Section(thisTerm,idx_opt)%eTimeIdx(pos) = Section(thisTerm,lect)%eTimeIdx(mdx)
                            Section(thisTerm,idx_opt)%RoomIdx(pos) = Section(thisTerm,lect)%RoomIdx(mdx)
                            Section(thisTerm,idx_opt)%TeacherIdx(pos) = Section(thisTerm,lect)%TeacherIdx(mdx)
                            Section(thisTerm,idx_opt)%NMeets = pos
                        end do !  mdx=1,Section(thisTerm,lect)%NMeets
                    end if
                    n_opts = n_opts + 1

                end do !  iSect=1,NumSections

                call timetable_undesirability(n_opts, thisTerm, TimeTable)

                ! available sections that fit schedule - can be added
                tLen2 = 0
                do iSect=1,n_opts
                    idx_opt = UndesirabilityRank(iSect)
                    if (Section(thisTerm,NumSections(thisTerm)+idx_opt)%RemSlots<=0) cycle ! section not available

!call html_comment(trim(itoa(idx_opt))//Section(thisTerm,NumSections+idx_opt)%ClassId//itoa(-Undesirability(idx_opt)) )

                    do mdx=1,Section(thisTerm,NumSections(thisTerm)+idx_opt)%NMeets
                        tArray(tLen1+tLen2+1) = NumSections(thisTerm)+idx_opt
                        tArray(tLen1+tLen2+2) = mdx
                        tArray(tLen1+tLen2+3) = -Undesirability(idx_opt)
                        tLen2 = tLen2+3
                    end do !  mdx=1,Section(thisTerm,iSect)%NMeets
                end do
                if (tLen2>0) then ! there are sections that can be added
                    ! end of list markers
                    tArray(tLen1+tLen2+1) = 0
                    tArray(tLen1+tLen2+2) = 0
                    tArray(tLen1+tLen2+3) = 0
                    call list_sections_to_edit(device, thisTerm, tLen2, tArray(tLen1+1), &
                        fnChangeMatriculation, tStdNo, 'Add', allowed_to_edit, allowed_to_show, &
                        '<a name="'//trim(tSubject)//'"></a>'//linebreak//horizontal// &
                        green//b_bold//'AVAILABLE sections'//e_color//' in "'//trim(tSubject)// &
                        ' - '//trim(Subject(iSubj)%Title)// &
                        '" that fit existing schedule, sorted by undesirability.'//e_bold ) !, .true.)

                end if

            end do !  fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject

        end if
        write(device,AFORMAT) horizontal

    end subroutine enlistment_edit


    subroutine enlistment_printable  (device, thisTerm)
        integer, intent (in) :: device, thisTerm
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        integer ::  ierr, tLen1
        integer, dimension(60,7) :: TimeTable
        logical :: conflicted

        ! which student?
        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
        targetStudent = index_to_student(tStdNo)
        targetCurriculum = Student(targetStudent)%CurriculumIdx

        call timetable_meetings_of_student(thisTerm, targetStudent, 0, tLen1, tArray, TimeTable, conflicted)
        call enlistment_class_schedule(device, targetStudent, thisTerm, tLen1, tArray)

        write(device,AFORMAT) horizontal

    end subroutine enlistment_printable


    subroutine enlistment_find_block(device, thisTerm)
        integer, intent (in) :: device, thisTerm
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        integer :: ierr, iSubj, iSect, seats
        integer :: bdx, fdx, iBlk, n_blks, n_matches, in_block
        logical :: matched

        ! which student?
        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
        targetStudent = index_to_student(tStdNo)
        targetCurriculum = Student(targetStudent)%CurriculumIdx
        targetCollege = Curriculum(targetCurriculum)%CollegeIdx

        call recalculate_available_seats(thisTerm)

        call html_write_header(device, trim(Student(targetStudent)%StdNo)//SPACE//trim(Student(targetStudent)%Name)// &
            SPACE//DASH//SPACE//trim(Curriculum(targetCurriculum)%Code))

        write(device,AFORMAT) '<table border="0" width="100%">'//b_tr, &
	        b_thal//'Block ID'//e_th// &
	        b_thal//'# Matches'//e_th// &
	        b_thal//'Class ID (seats)'//e_th//e_tr

        n_blks = 0
        do iBlk=1,NumBlocks(thisTerm)
            if (CurrProgNum(Block(thisTerm,iBlk)%CurriculumIdx)/=CurrProgNum(targetCurriculum)) cycle ! not this curriculum
            if (Student(targetStudent)%Enlistment(thisTerm)%levelYear/=Block(thisTerm,iBlk)%Year) cycle ! not the same year
            !if (Block(thisTerm,iBlk)%CurriculumIdx/=targetCurriculum) cycle ! not this curriculum
            ! count how many subjects in this block match the predicted subjects of the student
            n_matches = 0
            in_block = 0 ! subjects in block with assigned sections
            do bdx=1,Block(thisTerm,iBlk)%NumClasses
                iSubj = Block(thisTerm,iBlk)%Subject(bdx)
                if (Block(thisTerm,iBlk)%Section(bdx)>0) in_block = in_block + 1;
                do fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                    if (iSubj/=Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx)) cycle
                    n_matches = n_matches+1
                    exit
                end do
            end do
            if (n_matches==0) cycle

            n_blks = n_blks + 1
            write(device,AFORMAT) b_tr// &
                b_td//trim(make_href(fnBlockSchedule, Block(thisTerm,iBlk)%BlockID, A1=Block(thisTerm,iBlk)%BlockID, &
                    A9=thisTerm))//e_td// &
                b_td//trim(itoa(n_matches))//SPACE//FSLASH//SPACE//trim(itoa(in_block))//e_td// &
                b_td
            matched = .false.
            do bdx=1,Block(thisTerm,iBlk)%NumClasses
                iSubj = Block(thisTerm,iBlk)%Subject(bdx)
                do fdx=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                    if (iSubj/=Student(targetStudent)%Enlistment(thisTerm)%Subject(fdx)) cycle
                    matched = .true.
                    exit
                end do
                if (.not. matched) cycle
                iSect = Block(thisTerm,iBlk)%Section(bdx)
                if (iSect/=0) then
                    seats = Section(thisTerm,iSect)%RemSlots
                    if (seats>0) then
                        write(device,AFORMAT) b_bold//green//trim(Section(thisTerm,iSect)%ClassId)//' ('// &
                        trim(itoa(seats))//') '//e_color//e_bold//' /'//nbsp
                    else
                        write(device,AFORMAT) b_bold//green//trim(Section(thisTerm,iSect)%ClassId)//e_color//red//' ('// &
                        trim(itoa(seats))//') '//e_color//e_bold//' /'//nbsp
                    end if
                else
                    write(device,AFORMAT) b_bold//red//trim(Subject(iSubj)%Name)//e_color//e_bold//' /'//nbsp
                end if
                if (mod(bdx,4)==0 .and. Block(thisTerm,iBlk)%NumClasses>4) then
                    write(device,AFORMAT) e_td//e_tr// & ! end row
                    b_tr//b_td_nbsp_e_td//b_td_nbsp_e_td//b_td ! new row with first 2 columns empty
                end if
            end do
            if ( (isRole_adviser_of_student(targetStudent, orHigherUp) .and. &
                  College(targetCollege)%isAllowed(ToEnlistStudents,thisTerm) ) .or. &
                  isRole_admin_of_college(targetCollege) ) &
                write(device,AFORMAT) trim(make_href(fnChangeMatriculation, 'Enlist', A1=tStdNo, A2='Block', &
                    A3=Block(thisTerm,iBlk)%BlockID, A9=thisTerm, pre=nbsp, post=e_td//e_tr ))
        end do
        if (n_blks==0) then
            write(device,AFORMAT) b_tr//'<td colspan="3">No suitable blocks for this student?'//e_td//e_tr
        end if
        write(device,AFORMAT) e_table, linebreak, &
            b_italic//'NOTES'//e_italic//' : A  '//b_bold//green//'Class ID (seats)'//e_color//e_bold//' is open. A  '// &
            b_bold//green//'Class ID '//e_color, &
            red//'(0)'//e_color//e_bold//' is NOT available. A  '//b_bold//red//'SUBJECT'//e_color//e_bold// &
            ' is NOT assigned to a section.', &
            linebreak//'"Enlist" will add the student ONLY to the open sections of a block.', &
            linebreak//'Seats in an open section may be taken while you are reading this.', &
            horizontal

    end subroutine enlistment_find_block


    subroutine enlistment_class_schedule(device, iStd, thisTerm, lenSL, SectionList)
        integer, intent(in) :: device, iStd, thisTerm, lenSL, SectionList(3*lenSL+3)
        integer :: iSubj, idx, mdx, rdx, rdx1, iSect, previous, conflict
        real :: totalUnits, classUnits, totalHours, classHours, totalTuition, classTuition, totalLabFee, classLabFee
        real :: EnergyFee, totalEnergyFee, totalFiduciaryFee, totalOtherFee

        write(device,AFORMAT) b_bold//trim(Student(iStd)%StdNo)//SPACE//trim(Student(iStd)%Name)// &
            linebreak//text_curriculum_info(Student(iStd)%CurriculumIdx)//linebreak//linebreak, &
            'Enlisted subjects'//e_bold, &
            linebreak//b_italic// &
            'Subject fees may be assessed for laboratories, NSTP/ROTC, etc.', &
            e_italic//horizontal

        if (lenSL < 3) then
            write(device,AFORMAT) linebreak//'Not enlisted in any class?'//linebreak
            return
        end if

        totalUnits = 0.0
        totalHours = 0.0
        totalTuition = 0.0
        totalLabFee = 0.0
        totalEnergyFee = 0.0

        totalFiduciaryFee = 0.0
        totalOtherFee = 0.0
        do idx=1,MAX_ALL_FEES
            if (index(FeeDescription(idx), 'FIDUCIARY') > 0) then
                totalFiduciaryFee = totalFiduciaryFee + FeeAmount(idx)
            else if (index(FeeDescription(idx), 'OTHER') > 0) then
                totalOtherFee = totalOtherFee + FeeAmount(idx)
            end if
        end do

        write(device,AFORMAT) '<table border="0" width="100%">'//b_tr, &
            b_thal//'Subject'//e_th// &
            b_thal//'Section'//e_th//&
            b_thal//'Units'//e_th// &
            b_thal//'Hours'//e_th// &
            b_thal//'Tuition'//e_th// &
            b_thal//'Subject fee'//e_th// &
            b_thal//'Time'//e_th// &
            b_thal//'Day'//e_th//&
            b_thal//'Room'//e_th//e_tr

        previous = 0
        do idx=1,lenSL,3
            iSect=SectionList(idx)
            !mdx=SectionList(idx+1)
            conflict=SectionList(idx+2)
            iSubj = Section(thisTerm,iSect)%SubjectIdx

            !new section ?
            if (iSect/=previous) then ! include subject, section, units/blockname, seats/hours, time, day

                if (isSubject_lecture_lab(iSubj)) then
                    if (is_lecture_class(iSect, thisTerm)) then ! lecture of lecture-lab
                        classUnits = Subject(iSubj)%Units
                        classHours = Subject(iSubj)%LectHours
                        classTuition = Subject(iSubj)%Tuition
                        classLabFee = 0.0
                    else ! lab of lecture-lab
                        classUnits = 0.0
                        classHours = Subject(iSubj)%LabHours
                        classTuition = 0.0
                        classLabFee = Subject(iSubj)%LabFee
                    end if
                else if (Subject(iSubj)%LectHours>0.0) then ! lecture-only
                    classUnits = Subject(iSubj)%Units
                    classHours = Subject(iSubj)%LectHours
                    classTuition = Subject(iSubj)%Tuition
                    classLabFee = Subject(iSubj)%LabFee
                else if (Subject(iSubj)%LabHours>0.0) then ! lab-only
                    classUnits = Subject(iSubj)%Units
                    classHours = Subject(iSubj)%LabHours
                    classTuition = Subject(iSubj)%Tuition
                    classLabFee = Subject(iSubj)%LabFee
                end if

                totalHours = totalHours + classHours
                totalUnits = totalUnits + classUnits
                totalTuition = totalTuition + classTuition
                totalLabFee = totalLabFee + classLabFee


                previous = iSect
                write(device,AFORMAT) &
                    b_tr//b_td//trim(Subject(iSubj)%Name)//e_td, & !  subject
                    b_td//trim(Section(thisTerm,iSect)%Code)//e_td ! code

                if (classUnits>0.0) then
                    write(device,AFORMAT) b_td//trim(ftoa(classUnits,1))//e_td
                else
                    write(device,AFORMAT) b_td_nbsp_e_td ! units
                end if

                if (classHours>0.0) then
                    write(device,AFORMAT) b_td//trim(ftoa(classHours,2))//e_td ! hours
                else
                    write(device,AFORMAT) b_td_nbsp_e_td ! hours
                end if

                if (classTuition>0.0) then
                    write(device,AFORMAT) b_td//trim(ftoa(classTuition,2))//e_td
                else
                    write(device,AFORMAT) b_td_nbsp_e_td
                end if

                if (classLabFee>0.0) then
                    write(device,AFORMAT) b_td//trim(ftoa(classLabFee,2))//e_td
                else
                    write(device,AFORMAT) b_td_nbsp_e_td
                end if

                !
                !-------------------------------------------------

                if (is_regular_schedule(iSect, thisTerm)) then
                    rdx = Section(thisTerm,iSect)%RoomIdx(1)
                    EnergyFee = Room(rdx)%EnergyFee
                    write(device,AFORMAT) &
                        b_td//trim(text_time_period(Section(thisTerm,iSect)%bTimeIdx(1), &
                            Section(thisTerm,iSect)%eTimeIdx(1)))//e_td// &
                        b_td//trim(text_days_of_section(Section(thisTerm,iSect)))//e_td// &
                        b_td//trim(Room(rdx)%Code)//e_td//e_tr
                else
                    mdx = 1
                    rdx = Section(thisTerm,iSect)%RoomIdx(mdx)
                    EnergyFee = Room(rdx)%EnergyFee
                    write(device,AFORMAT) &
                        b_td//trim(text_time_period(Section(thisTerm,iSect)%bTimeIdx(mdx), &
                            Section(thisTerm,iSect)%eTimeIdx(mdx)))//e_td// &
                        b_td//trim(txtDay(Section(thisTerm,iSect)%DayIdx(mdx)))//e_td// &
                        b_td//trim(Room(rdx)%Code)//e_td//e_tr
                    do mdx=2,Section(thisTerm,iSect)%NMeets
                        rdx1 = Section(thisTerm,iSect)%RoomIdx(mdx)
                        if (EnergyFee < Room(rdx1)%EnergyFee) then
                            rdx = rdx1
                            EnergyFee = Room(rdx)%EnergyFee
                        end if
                        write(device,AFORMAT) b_tr//'<td colspan="6">'//nbsp//e_td// &
                            b_td//trim(text_time_period(Section(thisTerm,iSect)%bTimeIdx(mdx), &
                                Section(thisTerm,iSect)%eTimeIdx(mdx)))//e_td// &
                            b_td//trim(txtDay(Section(thisTerm,iSect)%DayIdx(mdx)))//e_td// &
                            b_td//trim(Room(rdx1)%Code)//e_td//e_tr
                    end do
                end if
                totalEnergyFee = totalEnergyFee + EnergyFee
                if (conflict>0) write(device,AFORMAT) &
                    b_tr//'<td align="center" colspan="9">'//red//'CONFLICT between '//trim(Section(thisTerm,iSect)%ClassId)// &
                    ' and '//trim(Section(thisTerm,conflict)%ClassId)//e_color//e_td//e_tr

            end if

        end do
        write(device,AFORMAT) b_tr//'<td colspan="9">'//horizontal//e_td//e_tr, &
            b_tr//b_td_nbsp_e_td//b_td//b_bold//'Totals'//e_bold//' : '//e_td// & ! code
            b_td//trim(ftoa(totalUnits,1))//e_td//b_td//trim(ftoa(totalHours,2))//e_td// & ! hours
            b_td//b_bold//trim(ftoa(totalTuition,2))//e_bold//e_td// &
            b_td//b_bold//trim(ftoa(totalLabFee,2))//e_bold//e_td// & ! fees
            b_td_nbsp_e_td// b_td_nbsp_e_td// b_td_nbsp_e_td//e_tr, &
            e_table, horizontal, linebreak

        write(device,AFORMAT) b_bold//'An estimate of the total additional fee is '// &
                trim(ftoa(totalEnergyFee+totalFiduciaryFee+totalOtherFee,2))//DOT, &
                ' The Finance office in '// trim(UniversityCode)//' makes the final assessment.'// &
                e_bold//linebreak, &
            b_italic//' The list of fees below may be incomplete, and some fees may be inapplicable. '//e_italic, &
            linebreak, '<table border="0" width="80%">'

        write(device,AFORMAT) '<tr valign="top">'//b_td, '<table border="0" width="100%">'
        if (totalFiduciaryFee>0.0) then
            write(device,AFORMAT) b_tr//'<td colspan="2">'//linebreak//'<b>FIDUCIARY fees</b>'//e_td//e_tr
            do idx=1,MAX_ALL_FEES
                if (index(FeeDescription(idx), 'FIDUCIARY:')==1 .and. FeeAmount(idx)>0.0) then
                    write(device,AFORMAT) b_tr// &
                        b_td//nbsp//nbsp//trim(FeeDescription(idx)(11:))//e_td// &
                        b_tdar//trim(ftoa(FeeAmount(idx),1))//e_td//e_tr
                end if
            end do
            write(device,AFORMAT) b_tr// &
                b_td//b_bold//'Subtotal'//e_bold//e_td// &
                b_tdar//b_bold//trim(ftoa(totalFiduciaryFee,1))//e_bold//e_td//e_tr
        end if
        write(device,AFORMAT) e_table//e_td, &
            b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td//b_td_nbsp_e_td

        write(device,AFORMAT) b_td, '<table border="0" width="100%">'
        if (totalEnergyFee>0.0) then
            write(device,AFORMAT) b_tr//'<td colspan="2">'//linebreak//'<b>ENERGY fee</b>'//e_td//e_tr
            previous = 0
            do idx=1,lenSL,3
                iSect=SectionList(idx)
                iSubj = Section(thisTerm,iSect)%SubjectIdx

                !new section ?
                if (iSect/=previous) then ! include subject, section, units/blockname, seats/hours, time, day
                    previous = iSect
                    if (is_regular_schedule(iSect, thisTerm)) then
                        rdx = Section(thisTerm,iSect)%RoomIdx(1)
                        EnergyFee = Room(rdx)%EnergyFee
                    else
                        rdx = Section(thisTerm,iSect)%RoomIdx(1)
                        EnergyFee = Room(rdx)%EnergyFee
                        do mdx=2,Section(thisTerm,iSect)%NMeets
                            rdx1 = Section(thisTerm,iSect)%RoomIdx(mdx)
                            if (EnergyFee < Room(rdx1)%EnergyFee) then
                                rdx = rdx1
                                EnergyFee = Room(rdx)%EnergyFee
                            end if
                        end do
                    end if
                    if (EnergyFee>0.0) write(device,AFORMAT) b_tr// &
                        b_td//nbsp//nbsp//trim(Subject(iSubj)%Name)//SPACE//trim(Section(thisTerm,iSect)%Code)//' @ '// &
                            trim(Room(rdx)%Code)//e_td// &
                        b_tdar//trim(ftoa(EnergyFee,1))//e_td//e_tr
                end if

            end do
            write(device,AFORMAT) b_tr// &
                b_td//b_bold//'Subtotal'//e_bold//e_td// &
                b_tdar//b_bold//trim(ftoa(totalEnergyFee,1))//e_bold//e_td//e_tr, &
                b_tr//b_td_nbsp_e_td//b_td_nbsp_e_td//e_tr
        end if

        if (totalOtherFee>0.0) then
            write(device,AFORMAT) b_tr//'<td colspan="2">'//linebreak//'<b>OTHER fees</b>'//e_td//e_tr
            do idx=1,MAX_ALL_FEES
                if (index(FeeDescription(idx), 'OTHER:')==1 .and. FeeAmount(idx)>0.0) then
                    write(device,AFORMAT) b_tr// &
                        b_td//nbsp//nbsp//trim(FeeDescription(idx)(7:))//e_td// &
                        b_tdar//trim(ftoa(FeeAmount(idx),1))//e_td//e_tr
                end if
            end do
            write(device,AFORMAT) b_tr// &
                b_td//b_bold//'Subtotal'//e_bold//e_td// &
                b_tdar//b_bold//trim(ftoa(totalOtherFee,1))//e_bold//e_td//e_tr
        end if
        write(device,AFORMAT) e_table, e_td//e_tr//e_table

    end subroutine enlistment_class_schedule


    subroutine printable_class_list (device, thisTerm)
        integer, intent (in) :: device, thisTerm

        integer :: n_count, tdx, iStd, errNo, ncol, iSubj, pos, otherSect !, ldx, iSect
        character(len=MAX_LEN_CLASS_ID) :: tClassId, otherClass
        character(len=255) :: header

        call html_comment('printable_class_list()')

        ! which section?
        call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, errNo)
        targetSection = index_to_section(tClassId, thisTerm)
        iSubj = Section(thisTerm,targetSection)%SubjectIdx
#if defined UPLB
        targetDepartment = Subject(iSubj)%DeptIdx
#else
        targetDepartment = Section(thisTerm,targetSection)%DeptIdx
#endif
        targetCollege = Department(targetDepartment)%CollegeIdx

        call collect_students_in_section (thisTerm, targetSection, n_count, tArray)

        ncol = n_count
        if (isSubject_lecture_lab(iSubj)) then
            header = trim(ftoa(Subject(iSubj)%LectHours+Subject(iSubj)%LabHours,2))//' hrs ('// &
                trim(ftoa(Subject(iSubj)%LectHours,2))//' lect + '// &
                trim(ftoa(Subject(iSubj)%LabHours,2))//' lab/recit).'

            if (is_lecture_class(targetSection, thisTerm)) then ! add lecture class and all lab sections
                ! add this lecture class
                do tdx=1,Section(thisTerm,targetSection)%NMeets
                    tArray(ncol+1) = targetSection
                    tArray(ncol+2) = tdx
                    tArray(ncol+3) = 0
                    ncol = ncol+3
                end do
                ! find/add lab sections
                otherClass = trim(tClassId)//DASH
                pos = len_trim(otherClass)
                do otherSect=1,NumSections(thisTerm)
                    if (Section(thisTerm,otherSect)%ClassId(:pos)/=otherClass(:pos)) cycle
                    do tdx=1,Section(thisTerm,otherSect)%NMeets
                        tArray(ncol+1) = otherSect
                        tArray(ncol+2) = tdx
                        tArray(ncol+3) = 0
                        ncol = ncol+3
                    end do
                end do

            else  ! find/add lecture class and this lab section
                pos = index(tClassId, DASH)
                otherClass = tClassId(:pos-1)
                otherSect = index_to_section(otherClass, thisTerm)
                do tdx=1,Section(thisTerm,otherSect)%NMeets
                    tArray(ncol+1) = otherSect
                    tArray(ncol+2) = tdx
                    tArray(ncol+3) = 0
                    ncol = ncol+3
                end do
                do tdx=1,Section(thisTerm,targetSection)%NMeets
                    tArray(ncol+1) = targetSection
                    tArray(ncol+2) = tdx
                    tArray(ncol+3) = 0
                    ncol = ncol+3
                end do
            end if

        else if (Subject(iSubj)%LectHours > 0.0) then
            header = trim(ftoa(Subject(iSubj)%LectHours,2))//' hrs lect.'
            do tdx=1,Section(thisTerm,targetSection)%NMeets
                tArray(ncol+1) = targetSection
                tArray(ncol+2) = tdx
                tArray(ncol+3) = 0
                ncol = ncol+3
            end do
        else if (Subject(iSubj)%LabHours > 0.0) then
            header = trim(ftoa(Subject(iSubj)%LabHours,2))//' hrs lab/recit.'
            do tdx=1,Section(thisTerm,targetSection)%NMeets
                tArray(ncol+1) = targetSection
                tArray(ncol+2) = tdx
                tArray(ncol+3) = 0
                ncol = ncol+3
            end do
        end if
        header = trim(Subject(iSubj)%Title)//'/ '//trim(ftoa(Subject(iSubj)%Units,1))//' units/ '//header

        tArray(ncol+1) = 0
        tArray(ncol+2) = 0
        tArray(ncol+3) = 0

        write(device,AFORMAT) &
            '<html><head><title>'//trim(UniversityCode)//SPACE//PROGNAME//VERSION// &
            '</title></head><body>', '<table border="0" width="100%">', &
            b_tr//b_tdac//'Republic of the Philippines'//e_td//e_tr, &
            b_tr//b_tdac//b_bold//trim(UniversityName)//e_bold//e_td//e_tr, &
            b_tr//b_tdac//trim(UniversityAddress)//e_td//e_tr, &
            b_tr//b_td//linebreak//e_td//e_tr, &
            b_tr//b_tdac//trim(College(targetCollege)%Name)//e_td//e_tr, &
            b_tr//b_tdac//b_bold//'Classlist for '//trim(txtSemester(thisTerm+3))// &
                trim(termQualifier(thisTerm+3))//COMMA//SPACE// &
                trim(itoa(currentYear))//DASH//trim(itoa(currentYear+1))//e_bold//e_td//e_tr, &
            b_tr//b_tdac//trim(Subject(iSubj)%Name)//' - '//trim(header)//e_td//e_tr, &
            b_tr//b_td//linebreak//e_td//e_tr, &
            e_table

        call list_sections_to_edit(device, thisTerm, ncol-n_count, tArray(n_count+1:), 0, &
            SPACE, SPACE, .false., .true., SPACE )

        if (n_count == 0) then
            write(device,AFORMAT) BRNONEHR
        else
            write(device,AFORMAT) linebreak//'<table border="0" width="100%">', &
                b_tr// &
                b_thal//'#'//e_th// &
                b_thal//'STDNO'//e_th// &
                b_thal//'NAME OF STUDENT'//e_th// &
                b_thal//'PROGRAM'//e_th, &
                b_thal//'#'//e_th// &
                b_thal//'STDNO'//e_th// &
                b_thal//'NAME OF STUDENT'//e_th// &
                b_thal//'PROGRAM'//e_th, &
                e_tr

            write(device,AFORMAT) &
                b_tr// &
                '<td width="50%" colspan="4">'//horizontal//e_td// &
                '<td width="50%" colspan="4">'//horizontal//e_td// &
                e_tr
            pos = (n_count+1)/2
            do tdx=1,pos
                iStd = tArray(tdx)
                write(device,AFORMAT) b_tr// &
                    b_td//trim(itoa(tdx))//DOT//e_td// &
                    b_td//trim(Student(iStd)%StdNo)//e_td// &
                    b_td//trim(Student(iStd)%Name)//e_td// &
                    b_td//trim(CurrProgCode(Student(iStd)%CurriculumIdx))//e_td
                if (tdx+pos<=n_count) then
                    iStd = tArray(tdx+pos)
                    write(device,AFORMAT) &
                        b_td//trim(itoa(tdx+pos))//DOT//e_td// &
                        b_td//trim(Student(iStd)%StdNo)//e_td// &
                        b_td//trim(Student(iStd)%Name)//e_td// &
                        b_td//trim(CurrProgCode(Student(iStd)%CurriculumIdx))//e_td
                else
                    write(device,AFORMAT) '<td colspan="4">---- '//b_italic//'Nothing follows'//e_italic//' ----'//e_td//e_tr
                end if
            end do
            write(device,AFORMAT) e_table//horizontal
        end if

    end subroutine printable_class_list


    subroutine printable_gradesheet (device, thisTerm)

        integer, intent (in) :: device, thisTerm

        integer :: ldx, gdx, n_count, tdx, iStd, errNo, iSect, ncol, iSubj, pos, otherSect, step
        character(len=MAX_LEN_CLASS_ID) :: tClassId, otherClass, tGrade
        character(len=255) :: header

        call html_comment('printable_gradesheet()')

        ! which section?
        call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, errNo)
        targetSection = index_to_section(tClassId, thisTerm)
        iSubj = Section(thisTerm,targetSection)%SubjectIdx
#if defined UPLB
        targetDepartment = Subject(iSubj)%DeptIdx
#else
        targetDepartment = Section(thisTerm,targetSection)%DeptIdx
#endif
        targetCollege = Department(targetDepartment)%CollegeIdx

        ! collect students
        n_count = 0
        do tdx=1,NumStudents+NumAdditionalStudents
            iStd = StdRank(tdx)
            do ncol=1,Student(iStd)%Enlistment(thisTerm)%lenSubject
                iSect = Student(iStd)%Enlistment(thisTerm)%Section(ncol)
                if (iSect==0) cycle
                if (targetSection == iSect) then
                    tArray(n_count+1) = iStd
                    tArray(n_count+2) = Student(iStd)%Enlistment(thisTerm)%Grade(ncol)
                    n_count = n_count+2
                    exit
                elseif (Student(iStd)%Enlistment(thisTerm)%Subject(ncol)==iSubj .and. isSubject_lecture_lab(iSubj)) then
                    ldx = index(Section(thisTerm,iSect)%ClassId,DASH)
                    if (ldx>0) then ! student is accommodated in a lab section
                        if (trim(tClassId)==Section(thisTerm,iSect)%ClassId(:ldx-1)) then ! lab of lecture
                            tArray(n_count+1) = iStd
                            tArray(n_count+2) = Student(iStd)%Enlistment(thisTerm)%Grade(ncol)
                            n_count = n_count+2
                            exit
                        end if
                    end if
                end if
            end do
        end do

        ncol = n_count
        if (isSubject_lecture_lab(iSubj)) then
            header = trim(ftoa(Subject(iSubj)%LectHours+Subject(iSubj)%LabHours,2))//' hrs ('// &
                trim(ftoa(Subject(iSubj)%LectHours,2))//' lect + '// &
                trim(ftoa(Subject(iSubj)%LabHours,2))//' lab/recit).'

            if (is_lecture_class(targetSection, thisTerm)) then ! add lecture class and all lab sections
                ! add this lecture class
                do tdx=1,Section(thisTerm,targetSection)%NMeets
                    tArray(ncol+1) = targetSection
                    tArray(ncol+2) = tdx
                    tArray(ncol+3) = 0
                    ncol = ncol+3
                end do
                ! find/add lab sections
                otherClass = trim(tClassId)//DASH
                pos = len_trim(otherClass)
                do otherSect=1,NumSections(thisTerm)
                    if (Section(thisTerm,otherSect)%ClassId(:pos)/=otherClass(:pos)) cycle
                    do tdx=1,Section(thisTerm,otherSect)%NMeets
                        tArray(ncol+1) = otherSect
                        tArray(ncol+2) = tdx
                        tArray(ncol+3) = 0
                        ncol = ncol+3
                    end do
                end do

            else  ! find/add lecture class and this lab section
                pos = index(tClassId, DASH)
                otherClass = tClassId(:pos-1)
                otherSect = index_to_section(otherClass, thisTerm)
                do tdx=1,Section(thisTerm,otherSect)%NMeets
                    tArray(ncol+1) = otherSect
                    tArray(ncol+2) = tdx
                    tArray(ncol+3) = 0
                    ncol = ncol+3
                end do
                do tdx=1,Section(thisTerm,targetSection)%NMeets
                    tArray(ncol+1) = targetSection
                    tArray(ncol+2) = tdx
                    tArray(ncol+3) = 0
                    ncol = ncol+3
                end do
            end if

        else if (Subject(iSubj)%LectHours > 0.0) then
            header = trim(ftoa(Subject(iSubj)%LectHours,2))//' hrs lect.'
            do tdx=1,Section(thisTerm,targetSection)%NMeets
                tArray(ncol+1) = targetSection
                tArray(ncol+2) = tdx
                tArray(ncol+3) = 0
                ncol = ncol+3
            end do
        else if (Subject(iSubj)%LabHours > 0.0) then
            header = trim(ftoa(Subject(iSubj)%LabHours,2))//' hrs lab/recit.'
            do tdx=1,Section(thisTerm,targetSection)%NMeets
                tArray(ncol+1) = targetSection
                tArray(ncol+2) = tdx
                tArray(ncol+3) = 0
                ncol = ncol+3
            end do
        end if
        header = trim(Subject(iSubj)%Title)//'/ '//trim(ftoa(Subject(iSubj)%Units,1))//' units/ '//header

        tArray(ncol+1) = 0
        tArray(ncol+2) = 0
        tArray(ncol+3) = 0

        write(device,AFORMAT) &
            '<html><head><title>'//trim(UniversityCode)//SPACE//PROGNAME//VERSION// &
            '</title></head><body>', '<table border="0" width="100%">', &
            b_tr//b_tdac//'Republic of the Philippines'//e_td//e_tr, &
            b_tr//b_tdac//b_bold//trim(UniversityName)//e_bold//e_td//e_tr, &
            b_tr//b_tdac//trim(UniversityAddress)//e_td//e_tr, &
            b_tr//b_td//linebreak//e_td//e_tr, &
            b_tr//b_tdac//trim(College(targetCollege)%Name)//e_td//e_tr, &
            b_tr//b_tdac//b_bold//'Gradesheet for '//trim(txtSemester(thisTerm+3))// &
                trim(termQualifier(thisTerm+3))//COMMA//SPACE// &
                trim(itoa(currentYear))//DASH//trim(itoa(currentYear+1))//e_bold//e_td//e_tr, &
            b_tr//b_tdac//trim(Subject(iSubj)%Name)//' - '//trim(header)//e_td//e_tr, &
            b_tr//b_td//linebreak//e_td//e_tr, &
            e_table

        call list_sections_to_edit(device, thisTerm, ncol-n_count, tArray(n_count+1:), 0, &
            SPACE, SPACE, .false., .true., SPACE ) !, .true.)

        if (n_count == 0) then
            write(device,AFORMAT) BRNONE
        else
            write(device,AFORMAT) linebreak//'<table border="0" width="100%">', &
                b_tr// &
                b_thal//'#'//e_th// &
                b_thal//'STDNO'//e_th// &
                b_thal//'GRADE'//e_th, &
                b_thal//'NAME OF STUDENT'//e_th// &
                b_thal//'#'//e_th// &
                b_thal//'STDNO'//e_th// &
                b_thal//'GRADE'//e_th, &
                b_thal//'NAME OF STUDENT'//e_th// &
                e_tr

            write(device,AFORMAT) &
                b_tr// &
                '<td width="50%" colspan="4">'//horizontal//e_td// &
                '<td width="50%" colspan="4">'//horizontal//e_td// &
                e_tr
            n_count = n_count/2
            step = (n_count + 1) / 2
            do tdx=1,step
                pos = 2*tdx - 1
                iStd = tArray(pos)
                gdx = tArray(pos+1)
                if (gdx==gdxREGD) then
                    tGrade = 'No grade'
                else
                    tGrade = txtGrade(pGrade(gdx))
                end if
                write(device,AFORMAT) b_tr// &
                    b_td//trim(itoa(tdx))//DOT//e_td// &
                    b_td//trim(Student(iStd)%StdNo)//e_td// &
                    b_tdac//trim(tGrade)//e_td// &
                    b_td//trim(Student(iStd)%Name)//e_td
                if (step+tdx<=n_count) then ! pos<=n_count) then
                    pos = 2*(step+tdx) - 1
                    iStd = tArray(pos)
                    gdx = tArray(pos+1)
	                if (gdx==gdxREGD) then
	                    tGrade = 'No grade'
	                else
	                    tGrade = txtGrade(pGrade(gdx))
	                end if
                    write(device,AFORMAT) &
                        b_td//trim(itoa(step+tdx))//DOT//e_td// &
                        b_td//trim(Student(iStd)%StdNo)//e_td// &
                        b_tdac//trim(tGrade)//e_td// &
                        b_td//trim(Student(iStd)%Name)//e_td
                else
                    write(device,AFORMAT) '<td colspan="4">---- '//b_italic//'Nothing follows'//e_italic//' ----'//e_td//e_tr
                end if
            end do
            write(device,AFORMAT) b_tr//'<td width="50%" colspan="8">'//horizontal//e_td//e_tr

            ! signatories
            header = Teacher(Section(thisTerm,targetSection)%TeacherIdx(1))%Name
            tdx = index(header, COMMA//SPACE)
            if (tdx>0) then ! switch first & last names
                header = trim(header(tdx+2:))//SPACE//header(:tdx-1)
            end if
            write(device,AFORMAT) &
                b_tr// &
                '<td align="center" width="50%" colspan="4">'//linebreak//linebreak//linebreak//trim(header)// &
                    linebreak//'Teacher'//e_td// &
                '<td align="center" width="50%" colspan="4">'//linebreak//linebreak//linebreak// &
                    trim(College(targetCollege)%Dean)// &
                    linebreak//'College Dean, '//trim(College(targetCollege)%Code)//e_td// &
                e_tr

            write(device,AFORMAT) e_table
        end if

    end subroutine printable_gradesheet



    subroutine enlistment_not_accommodated(device, thisTerm)
        integer, intent (in) :: device, thisTerm

        integer :: n_count, tdx, iStd, ierr, ncol
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege, remark

        ! which subject ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tSubject, ierr)
        targetSubject = index_to_subject(tSubject)

#if defined UPLB
        targetCollege = Department(Subject(targetSubject)%DeptIdx)%CollegeIdx
        tCollege = College(targetCollege)%Code
        remark = 'S'
#else
        ! which college ?
        call cgi_get_named_string(QUERY_STRING, 'A2', tCollege, ierr)
        targetCollege = index_to_college(tCollege)
        remark = trim(tCollege)//' s'

#endif
        call html_write_header (device, trim(remark)//'tudents not accommodated in priority subject '// &
            trim(Subject(targetSubject)%Name)//SPACE//DASH//SPACE//trim(Subject(targetSubject)%Title))
        ! collect students
        n_count = 0
        !tArray = 0
        do tdx=1,NumStudents+NumAdditionalStudents
            iStd = StdRank(tdx)
#if defined UPLB
#else
            if (targetCollege/=NumColleges) then
                if (targetCollege/=Curriculum(Student(iStd)%CurriculumIdx)%CollegeIdx) cycle
            end if
#endif
            do ncol=1,Student(iStd)%Enlistment(thisTerm)%lenSubject
                if (Student(iStd)%Enlistment(thisTerm)%Section(ncol)>0) cycle     ! accommodated
                if (targetSubject/=Student(iStd)%Enlistment(thisTerm)%Subject(ncol)) cycle  ! not this subject
                if (Student(iStd)%Enlistment(thisTerm)%Contrib(ncol)<0.55) cycle     ! not counted in demand
                n_count = n_count+1
                tArray(n_count) = iStd
            end do
        end do

        call list_students(device, thisTerm, n_count, tArray, targetSubject)
        write(device,AFORMAT) horizontal

    end subroutine enlistment_not_accommodated


    subroutine enlistment_summarize (device, thisTerm, fn)
        integer, intent (in) :: device, thisTerm, fn
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment, remark
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        integer :: ierr, i, j, k, l, iStd, maxSubjects, colorIdx
        character (len=4) :: tNote
        character(len=20) :: colorBottlenecks, colorExtraSlots

        ! which unit ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)
        targetDepartment = index_to_dept(tDepartment)
        targetCollege = Department(targetDepartment)%CollegeIdx
        tCollege = College(targetCollege)%Code

#if defined UPLB
        k = 0
        remark = SPACE
#else
        if (targetDepartment==NumDepartments) then
            k = 0
        else
            k = targetDepartment
        end if
        remark = ' in '//tDepartment
#endif
        call offerings_summarize(thisTerm, k)

        ! calculate remaining slots, no. of students accomodated from all colleges
        Section(thisTerm,:)%RemSlots = Section(thisTerm,:)%Slots
        do iStd = 1,NumStudents+NumAdditionalStudents
            do i=1,Student(iStd)%Enlistment(thisTerm)%NPriority+Student(iStd)%Enlistment(thisTerm)%NAlternates + &
                    Student(iStd)%Enlistment(thisTerm)%NCurrent
                j = Student(iStd)%Enlistment(thisTerm)%Subject(i)
                k = Student(iStd)%Enlistment(thisTerm)%Section(i)
                if (k > 0) then ! accommodated or force enlisted
                    if (Section(thisTerm,k)%DeptIdx==targetDepartment .or. targetDepartment==NumDepartments) then ! section belongs to unit
                        Offering(thisTerm,j)%Accommodated = Offering(thisTerm,j)%Accommodated + 1
                        Section(thisTerm,k)%RemSlots = Section(thisTerm,k)%RemSlots - 1
                    end if
                end if
            end do
        end do

        ! students not accomodated
        do iStd = 1,NumStudents+NumAdditionalStudents
#if defined UPLB
#else
            if (targetCollege/=NumColleges) then
                if (targetCollege/=Curriculum(Student(iStd)%CurriculumIdx)%CollegeIdx) cycle
            end if
#endif
            do i=1,Student(iStd)%Enlistment(thisTerm)%NPriority+Student(iStd)%Enlistment(thisTerm)%NAlternates + &
                    Student(iStd)%Enlistment(thisTerm)%NCurrent
                if (Student(iStd)%Enlistment(thisTerm)%Contrib(i)>0.5) then ! contributes to demand
                    j = Student(iStd)%Enlistment(thisTerm)%Subject(i)
                    Offering(thisTerm,j)%Demand = Offering(thisTerm,j)%Demand + 1
                    k = Student(iStd)%Enlistment(thisTerm)%Section(i)
                    if (k==0) Offering(thisTerm,j)%PriorityNotAccommodated = Offering(thisTerm,j)%PriorityNotAccommodated + 1
                end if
            end do
        end do

        ! calculate remaining seats, open sections
        do i=1,NumSections(thisTerm)
#if defined UPLB
#else
            if (Section(thisTerm,i)%DeptIdx/=targetDepartment .and. targetDepartment/=NumDepartments) cycle
#endif
            j = Section(thisTerm,i)%SubjectIdx
            l = Section(thisTerm,i)%RemSlots
            if (l==0) cycle
            ! lecture-lab ?
            if (.not. isSubject_lecture_lab(j)) then ! lecture only or lab only
                Offering(thisTerm,j)%OpenSlots = Offering(thisTerm,j)%OpenSlots + l
                Offering(thisTerm,j)%OpenSections = Offering(thisTerm,j)%OpenSections + 1
            else ! a lecture-lab subject
                if (is_lecture_class(i, thisTerm)) then ! this is the lecture section
                else ! this is the lab section
                    Offering(thisTerm,j)%OpenSlots = Offering(thisTerm,j)%OpenSlots + l
                    Offering(thisTerm,j)%OpenSections = Offering(thisTerm,j)%OpenSections + 1
                end if
            end if
        end do

        select case (fn)

            case (fnEnlistmentSummary)

                colorBottlenecks =  Black
                colorExtraSlots = Black
                call html_write_header(device, 'Summary of enlistment'//remark)
                do i=1,NumSubjects+NumAdditionalSubjects
                    SubjectRank(i) = i
                end do
                maxSubjects = NumSubjects+NumAdditionalSubjects

            case (fnBottleneck)
                colorBottlenecks =  Red
                colorExtraSlots = Black
                do i=1,NumSubjects+NumAdditionalSubjects
                    SubjectRank(i) = i
                    Offering(thisTerm,i)%SortKey = Offering(thisTerm,i)%PriorityNotAccommodated
                end do
                do i=1,NumSubjects+NumAdditionalSubjects-1
                    do j=i+1,NumSubjects+NumAdditionalSubjects
                        if (Offering(thisTerm,SubjectRank(i))%SortKey <= Offering(thisTerm,SubjectRank(j))%SortKey) then
                            k = SubjectRank(i)
                            SubjectRank(i) = SubjectRank(j)
                            SubjectRank(j) = k
                        end if
                    end do
                end do
                maxSubjects = maxListLength
                call html_write_header(device, 'Top '//trim(itoa(maxListLength))// &
                    ' subjects'//trim(remark)//' for which demand > available seats')

            case (fnExtraSlots)
                colorBottlenecks =  Black
                colorExtraSlots = Red
                do i=1,NumSubjects+NumAdditionalSubjects
                    SubjectRank(i) = i
                    Offering(thisTerm,i)%SortKey = Offering(thisTerm,i)%OpenSlots
                end do
                do i=1,NumSubjects+NumAdditionalSubjects-1
                    do j=i+1,NumSubjects+NumAdditionalSubjects
                        if (Offering(thisTerm,SubjectRank(i))%SortKey <= Offering(thisTerm,SubjectRank(j))%SortKey) then
                            k = SubjectRank(i)
                            SubjectRank(i) = SubjectRank(j)
                            SubjectRank(j) = k
                        end if
                    end do
                end do
                maxSubjects = maxListLength
                call html_write_header(device, 'Top '//trim(itoa(maxListLength))// &
                    ' subjects'//trim(remark)//' for which available seats > demand')

        end select

        if (NumSections(thisTerm)==0) then
            write(device,AFORMAT) BRNONEHR
            return
        end if

        l = 0
        k = 0
        colorIdx = 0
        write(device,AFORMAT) '<table border="0" width="100%">'
        do i=1,NumSubjects+NumAdditionalSubjects
            j = SubjectRank(i)
#if defined UPLB
            if (Subject(j)%DeptIdx/=targetDepartment) cycle
#else
#endif
            if (Offering(thisTerm,j)%Demand == 0) cycle
            if (mod(l,20)==0) then

                write(device,AFORMAT) b_tr//'<td width="50%">'//b_italic//b_para//'Subject'//linebreak//e_para//e_italic//e_td, & ! subject
                    b_tdar//b_italic//b_para//'No. of'//linebreak//'sections'//e_para//e_italic//e_td, & ! no. of sections
                    b_tdar//b_italic//b_para//'Total'//linebreak//'seats'//e_para//e_italic//e_td, & ! total seats
                    b_tdar//b_italic//b_para//'Total'//linebreak//'accom'//e_para//e_italic//e_td, &    ! total accom
                    b_tdar//b_italic//b_para//colorExtraSlots//'Open'//linebreak//'seats'//e_color//e_para//e_italic//e_td, & !  open seats
                    b_tdar//b_italic//b_para//'Priority'//linebreak//'demand'//e_para//e_italic//e_td, & ! priority demand
                    b_tdar//b_italic//b_para//colorBottlenecks//'Priority'//linebreak//'not acc'//e_color//e_para// &
                    e_italic//e_td//e_tr ! priority not accom
            end if

            colorIdx = colorIdx + 1
            tSubject = Subject(j)%Name

            ! subject
            if (Offering(thisTerm,j)%NSections>0) then
                tNote = SPACE
            else
                tNote = ' (*)'
            end if
!            write(device,AFORMAT)  b_tr//'<td width="15%">'//trim(tSubject)//tNote//e_td
            write(device,AFORMAT) '<tr width="50%" bgcolor="'//bgcolor(mod(colorIdx,2))//'">'// & ! subject code + title
                b_td//trim(tSubject)//SPACE//DASH//SPACE//b_italic//b_small//trim(Subject(j)%Title)//e_small//e_italic//tNote//e_td
            ! no. of sections, total seats
            write(device,AFORMAT) b_tdar//trim(itoa(Offering(thisTerm,j)%NSections))//e_td, &
                b_tdar//trim(itoa(Offering(thisTerm,j)%TotalSlots))//e_td
            ! total accom
            write(device,AFORMAT) b_tdar//itoa(Offering(thisTerm,j)%Accommodated)//e_td

            ! open seats
            write(device,AFORMAT) b_tdar//trim(itoa(Offering(thisTerm,j)%OpenSlots))//e_td
            ! priority demand
            write(device,AFORMAT) b_tdar//trim(itoa(Offering(thisTerm,j)%Demand))//e_td
            ! priority not accom
            if (Offering(thisTerm,j)%PriorityNotAccommodated>0) then
                if (.not. (isRoleStudent .or. isRoleGuest) ) then
                    write(device,AFORMAT) trim(make_href(fnNotAccommodated, &
                        itoa(Offering(thisTerm,j)%PriorityNotAccommodated), &
                        A1=tSubject, A2=tCollege, A9=thisTerm, pre=b_tdar, post=e_td//e_tr ))
                else
                    write(device,AFORMAT) b_tdar//itoa(Offering(thisTerm,j)%PriorityNotAccommodated)//e_td//e_tr
                end if
            else
                write(device,AFORMAT) b_tdar//'0'//e_td//e_tr
            end if

            l = l+1

            k = k+1
            if (k>=maxSubjects) exit
        end do

        write(device,AFORMAT) e_table, &
            linebreak//'Legends:', &
            linebreak//b_italic//'No. of sections'//e_italic//' = sections/labs in unit', &
            linebreak//b_italic//'Total seats'//e_italic// &
                ' = Total number of seats from sections in unit', &
            linebreak//b_italic//'Total accom'//e_italic// &
                ' = No. of students (from all colleges) accommodated in the sections', &
            linebreak//b_italic//'Priority demand'//e_italic// &
                ' = No. of students in college who need the subject as specified in their curriculum, or as a back subject', &
            linebreak//b_italic//'Priority not accom'//e_italic//' = priority demand not satisfied', &
            linebreak//'(*) = no sections open, or subject is needed by graduating students'
        write(device,AFORMAT) horizontal

    end subroutine enlistment_summarize


    subroutine underload_summarize (device, thisTerm)
        integer, intent (in) :: device, thisTerm
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege, remark
        integer :: j, k, l, n, iStd, pos(0:90), iuLoad, iMaxLoad
        real :: feasible_units, enlisted_units, tUnits, rMax

        ! which college ?
#if defined UPLB
        targetCollege = NumColleges
        tCollege = College(targetCollege)%Code
        remark = SPACE
        rMax = 24.0
#else
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, j)
        targetCollege = index_to_college(tCollege)
        remark = ' in '//tCollege
        rMax = 30.0
#endif

        MaxLoad = 0.0 ! maximum allowed load
        do iStd = 1,NumStudents+NumAdditionalStudents
            if (targetCollege/=NumColleges) then
                if (targetCollege/=Curriculum(Student(iStd)%CurriculumIdx)%CollegeIdx) cycle
            end if
            if (MaxLoad<Student(iStd)%Enlistment(thisTerm)%AllowedLoad) then
                MaxLoad = Student(iStd)%Enlistment(thisTerm)%AllowedLoad
                !call html_comment(Student(iStd)%StdNo//trim(Student(iStd)%Name)//' is allowed '//ftoa(MaxLoad,1))
            end if
        end do
        iMaxLoad = min(rMax, MaxLoad)
        ! adjust excessive loads
        do iStd = 1,NumStudents+NumAdditionalStudents
            if (targetCollege/=NumColleges) then
                if (targetCollege/=Curriculum(Student(iStd)%CurriculumIdx)%CollegeIdx) cycle
            end if
            iuLoad = Student(iStd)%Enlistment(thisTerm)%AllowedLoad
            if (iuLoad>iMaxLoad) Student(iStd)%Enlistment(thisTerm)%AllowedLoad = iMaxLoad
        end do

        call html_write_header(device, 'Summary of underloading/overloading'//remark)
        write(device,AFORMAT) 'Note: Entry at position ('//b_italic//'row'//e_italic//', '// &
            b_italic//'column'//e_italic//') indicates the number of students who were allowed '// &
            b_italic//'column'//e_italic//' units but were underloaded(-)/overloaded(+) by '// &
            b_italic//'row'//e_italic//' units.'
        write(device,AFORMAT) '<table border="1" width="100%">', b_tr, b_td_nbsp_e_td

        do j=iMaxLoad,0,-1
            write(device,'(a,i5,a)') b_tdar, j, e_td
        end do
        write(device,AFORMAT) b_td//'Total'//e_td, &
            b_td//'%'//e_td, &
            e_tr
        tArray(1:NumStudents+NumAdditionalStudents) = 0
        n = 0
        do iStd=1,NumStudents+NumAdditionalStudents
            if (Student(iStd)%Enlistment(thisTerm)%NPriority+Student(iStd)%Enlistment(thisTerm)%NAlternates == 0) cycle
            if (targetCollege/=NumColleges) then
                if (targetCollege/=Curriculum(Student(iStd)%CurriculumIdx)%CollegeIdx) cycle
            end if
            feasible_units = 0
            enlisted_units = 0.0
            do j = 1,Student(iStd)%Enlistment(thisTerm)%NPriority+Student(iStd)%Enlistment(thisTerm)%NAlternates
                tUnits = Subject(Student(iStd)%Enlistment(thisTerm)%Subject(j))%Units
                feasible_units = feasible_units + tUnits
                if (Student(iStd)%Enlistment(thisTerm)%Section(j) > 0) enlisted_units = enlisted_units + tUnits
            end do
            tArray(iStd) = min(Student(iStd)%Enlistment(thisTerm)%AllowedLoad, feasible_units) - enlisted_units
            n = n+1
        end do
        pos = 0
        do l=iMaxLoad,-6,-1

            k = 0
            pos(0:iMaxLoad) = 0
            do iStd = 1,NumStudents+NumAdditionalStudents
                if (targetCollege/=NumColleges) then
                    if (targetCollege/=Curriculum(Student(iStd)%CurriculumIdx)%CollegeIdx) cycle
                end if
                if (Student(iStd)%Enlistment(thisTerm)%NPriority+Student(iStd)%Enlistment(thisTerm)%NAlternates == 0 .or. &
                    tArray(iStd) /= l) cycle
                iuLoad = Student(iStd)%Enlistment(thisTerm)%AllowedLoad
                pos(iuLoad) = pos(iuLoad) + 1
                pos(iMaxLoad+1+iuLoad) = pos(iMaxLoad+1+iuLoad) + 1
                k = k+1
            end do
            write(device,AFORMAT) b_tr//b_tdac//trim(itoa(-l))//e_td
            do j=iMaxLoad,0,-1
                if (pos(j)>0) then
                    if (.not. (isRoleStudent .or. isRoleGuest) ) then
                        write(device,AFORMAT) trim(make_href(fnUnderloadedStudents, itoa(pos(j)), &
                            A1=tCollege, A2=trim(itoa(j)), A3=trim(itoa(l)), A9=thisTerm, pre=b_tdar, post=e_td))
                    else
                        write(device,AFORMAT) b_tdar//itoa(pos(j))//e_td
                    end if

                else
                    write(device,AFORMAT) b_tdar//DOT//e_td
                end if
            end do
            write(device,AFORMAT) b_tdar//trim(itoa(k))//e_td//b_tdar// &
                trim(itoa(int((100.0*k)/n+0.5)))//e_td//e_tr

        end do
        write(device,'(30a)') b_tr//b_td//'Total'//e_td, &
            (b_tdar//trim(itoabz(pos(iMaxLoad+1+j)))//e_td, j=iMaxLoad,0,-1),  &
            b_tdar//trim(itoa(n))//e_td//b_td_nbsp_e_td//e_tr

        write(device,AFORMAT) b_tr//b_td//'Load'//e_td
        do j=iMaxLoad,0,-1
            write(device,'(a,i5,a)') b_tdar, j, e_td
        end do
        write(device,AFORMAT) b_td_nbsp_e_td, b_td//'%'//e_td, e_tr, e_table
        write(device,AFORMAT) 'Note: Entry at position ('//b_italic//'row'//e_italic//', ', &
            b_italic//'column'//e_italic//') indicates the number of students who were allowed '// &
            b_italic//'column'//e_italic//' units but were underloaded(-)/overloaded(+) by '// &
            b_italic//'row'//e_italic//' units.', horizontal

    end subroutine underload_summarize


    subroutine underloaded_students (device, thisTerm)
        integer, intent (in) :: device, thisTerm
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege, remark
        integer :: ierr, i, j, n, iStd
        real :: allowed_units, underloaded_by, feasible_units, enlisted_units, tUnits

        ! which college ?
#if defined UPLB
        targetCollege = NumColleges
        tCollege = College(targetCollege)%Code
        remark = 'S'
#else
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        targetCollege = index_to_college(tCollege)
        remark = trim(tCollege)//' S'
#endif
        call cgi_get_named_float(QUERY_STRING, 'A2', allowed_units, ierr)
        call cgi_get_named_float(QUERY_STRING, 'A3', underloaded_by, ierr)

        n = 0
        tArray = 0
        do i=1,NumStudents+NumAdditionalStudents
            iStd = StdRank(i)
            if (Student(iStd)%Enlistment(thisTerm)%NPriority+Student(iStd)%Enlistment(thisTerm)%NAlternates == 0) cycle
            if (Student(iStd)%Enlistment(thisTerm)%AllowedLoad/=allowed_units) cycle
            if (targetCollege/=NumColleges) then
                if (targetCollege/=Curriculum(Student(iStd)%CurriculumIdx)%CollegeIdx) cycle
            end if
            feasible_units = 0
            enlisted_units = 0.0
            do j = 1,Student(iStd)%Enlistment(thisTerm)%NPriority+Student(iStd)%Enlistment(thisTerm)%NAlternates
                tUnits = Subject(Student(iStd)%Enlistment(thisTerm)%Subject(j))%Units
                feasible_units = feasible_units + tUnits
                if (Student(iStd)%Enlistment(thisTerm)%Section(j) > 0) enlisted_units = enlisted_units + tUnits
            end do
            if ( (min(Student(iStd)%Enlistment(thisTerm)%AllowedLoad, feasible_units) - enlisted_units) /= underloaded_by ) cycle

            n = n + 1
            tArray(n) = iStd

        end do

        call html_write_header(device, trim(remark)//'tudents allowed '//ftoa(allowed_units,1)// &
            ' units, but underloaded by '//ftoa(underloaded_by,1))
        call html_student_list (device, n, tArray, .true., SPACE)
        write(device,AFORMAT) horizontal

    end subroutine underloaded_students



    subroutine  enlistment_no_classes(device, numRemoved)
        integer, intent (in) :: device
        integer, intent (out) :: numRemoved
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege

        integer :: ierr, i, k, m, n, iStd
        integer :: ldx, tdx, fac, skip, first, last
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character :: ch
        logical :: hideCheckedAbove, hideNamed, hideAll
        integer, parameter :: toSkip = 50
        character(len=12), parameter :: HIDE_CHECKED = 'Hide_checked'
        character(len=11), parameter :: HIDE_LISTED  = 'Hide_listed'
        character(len= 8), parameter :: HIDE_ALL     = 'Hide_all'
        character(len=12) :: tAction
        character(len=MAX_LEN_FILE_PATH) :: header, mesg

        ! which college ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        targetCollege = index_to_college(tCollege)

        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
        hideCheckedAbove = trim(tAction)==HIDE_CHECKED
        hideNamed = trim(tAction)==HIDE_LISTED
        hideAll   = trim(tAction)==HIDE_ALL

        ch = SPACE
        if (hideCheckedAbove .or. hideNamed) then ! get letter
            call cgi_get_named_string(QUERY_STRING, 'A3', tAction, ierr)
            ch = tAction(1:1)
        end if

        n = 0 ! selected
        numRemoved = 0 ! excluded
        tArray = 0
        do i=1,NumStudents+NumAdditionalStudents
            iStd = StdRank(i)
            !if (targetCollege/=NumColleges) then
                if (targetCollege/=Curriculum(Student(iStd)%CurriculumIdx)%CollegeIdx) cycle
            !end if
            m = sum(Student(iStd)%Enlistment(1)%Section(:)) + sum(Student(iStd)%Enlistment(2)%Section(:)) + &
                sum(Student(iStd)%Enlistment(3)%Section(:))
            if (m>0) cycle ! has at least one class this school year

            ! only the Registrar can remove students
            if (isRegistrar) then
                if (hideCheckedAbove) then

                    if (Student(iStd)%Name(1:1)==ch) then
                        call cgi_get_named_string(QUERY_STRING, trim(Student(iStd)%StdNo), tAction, ierr)
                        if (ierr==0) then
                            Student(iStd)%ResidenceStatus = REMOVE_FROM_ROSTER
                        end if
                    end if

                else if (hideNamed) then
                    if (Student(iStd)%Name(1:1)==ch) Student(iStd)%ResidenceStatus = REMOVE_FROM_ROSTER

                else if (hideAll) then
                    Student(iStd)%ResidenceStatus = REMOVE_FROM_ROSTER

                end if
            end if

            if (Student(iStd)%ResidenceStatus == REMOVE_FROM_ROSTER) then ! excluded
                numRemoved = numRemoved + 1
            else ! include in list
                n = n + 1
                tArray(n) = iStd
            end if

        end do

        if (numRemoved>0) then
            mesg = itoa(numRemoved)//' students hidden.'
        elseif (.not. isRegistrar) then
            mesg = 'Only the Registrar can hide students.'
        end if
        header = trim(tCollege)//' students not enrolled in any term of current school year = '//itoa(n)

        call html_write_header(device, header, mesg)

        do i=iachar('A'), iachar('Z')

            ! how many for this letter?
            ch = achar(i)
            m = 0
            do tdx=1,n
                iStd = tArray(tdx)
                if (Student(iStd)%Name(1:1)==ch) then ! add to end of list
                    m = m+1
                    tArray(n+m) = iStd
                end if
            end do
            if (m==0) cycle

            if ( m>toSkip ) then
                skip = toSkip
            else
                skip = m
            end if

            do k=1, m, skip
                first = k
                last = min(first+skip-1, m)

                call make_form_start(device, fnStudentsNotEnrolled, A1=tCollege, A3=ch)

                write(device,AFORMAT) linebreak, '<h3>"'//ch//'" students of '//trim(tCollege)// &
                    ' not enrolled in any class for the school year</h3>', &
                    '<table border="0" width="75%">', &
                    b_tr//b_thal//'Hide'//e_th//b_thal//'STDNO'//e_th, &
                    b_thal//'NAME OF STUDENT'//e_th//b_thal//'PROGRAM'//e_th//e_tr

                do tdx=first,last

                    iStd = tArray(n+tdx)
                    if (Student(iStd)%Name(1:1)/=ch) cycle

                    tStdNo = Student(iStd)%StdNo
                    fac = index_to_teacher(Student(iStd)%Adviser)
                    ldx = Student(iStd)%CurriculumIdx

                    write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(tdx,2))//'">'//b_td// &
                        '<input type="checkbox" name="'//trim(tStdNo)//'">', &
                        e_td//b_td//tStdNo//e_td, &
                        b_td//trim(Student(iStd)%Name)//e_td, &
                        b_td//trim(Curriculum(ldx)%Code)//e_td

                    if ( isRole_adviser_of_student(iStd,orHigherUp) .or. isRole_benefactor_of_student(iStd) ) then

                        write(device,AFORMAT) b_td//b_small
                        if (.not. isRoleBenefactor) then
                            write(device,AFORMAT) &
                                trim(make_href(fnStudentEdit, 'Info', A1=tStdNo, pre=nbsp)), &
                                trim(make_href(fnRecentStudentActivity, 'Log', A1=tStdNo, pre=nbsp)), &
                                trim(make_href(fnTeacherClasses, 'Adviser', A1=Teacher(fac)%TeacherId, A9=currentTerm))
                        end if
                        write(device,AFORMAT) &
                            trim(make_href(fnStudentGrades, 'Grades', A1=tStdNo, pre=nbsp)), &
                            trim(make_href(fnEditCheckList, 'Checklist', A1=tStdNo, A9=currentTerm, pre=nbsp))

                        write(device,AFORMAT) e_small//e_td
                    end if
                    write(device,AFORMAT) e_tr
                end do
                write(device,AFORMAT) &
                    e_table//linebreak//'<input type="submit" name="action" value="'//HIDE_CHECKED//'"> ', &
                    ' "'//ch//'..." '//' students above, or '//nbsp//nbsp// &
                    '<input type="submit" name="action" value="'//HIDE_LISTED//'"> "'//ch//'..." '// &
                    ' students above and below, or '//nbsp//nbsp// &
                    '<input type="submit" name="action" value="'//HIDE_ALL//'"> listed students.', &
                    e_form

            end do
            write(device,AFORMAT) horizontal

        end do

    end subroutine  enlistment_no_classes


#include "Enlistment.F90"


end module EditENLISTMENT
