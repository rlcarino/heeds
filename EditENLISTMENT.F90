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


module EditENLISTMENT

    use HTML

    implicit none

    integer, private :: maxListLength = 200

contains


    subroutine enlistment_grades (device, thisTerm, NumSections, Section, eList)

        integer, intent (in) :: device, thisTerm, NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in out) :: eList(0:)

        integer :: gdx, ldx, n_count, tdx, std, ierr, sect, ncol, crse, idx_select, n_changes
        integer :: k, first, last, skip
        character(len=MAX_LEN_CLASS_ID) :: tClassId, tAction
        character (len=MAX_LEN_TEXT_GRADE) :: tGrade
        character(len=255) :: header, mesg
        integer :: specialGrade(4) = (/ gdxREGD, gdxNFE, gdxINC, gdxDRP /)

        call html_comment('enlistment_grades()')

        ! which section?
        call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, ierr)

        ! range
        call cgi_get_named_integer(QUERY_STRING, 'A2', first, ierr)
        call cgi_get_named_integer(QUERY_STRING, 'A3', last, ierr)

        ! action
        call cgi_get_named_string(QUERY_STRING, 'action'//trim(itoa(first)), tAction, ierr)


        targetSection = index_to_section(tClassId, NumSections, Section)
        if (targetSection==0) then
            call html_college_links(device, CollegeIdxUser, mesg='Section "'//trim(tClassId)//'" not found.')
            return
        end if

        crse = Section(targetSection)%SubjectIdx
#if defined REGIST
        targetDepartment = Subject(crse)%DeptIdx
#else
        targetDepartment = Section(targetSection)%DeptIdx
#endif
        targetCollege = Department(targetDepartment)%CollegeIdx

        ! collect students
        mesg = SPACE
        n_count = 0
        n_changes = 0
        do tdx=1,NumStudents+NumAdditionalStudents
            std = StdRank(tdx)
            do ncol=1,eList(std)%lenSubject
                sect = eList(std)%Section(ncol)
                if (sect==0) cycle
                if (targetSection == sect) then
                    tArray(n_count+1) = std
                    tArray(n_count+2) = eList(std)%Grade(ncol)
                    n_count = n_count+2
                    exit
                elseif (eList(std)%Subject(ncol)==crse .and. is_lecture_lab_subject(crse)) then
                    ldx = index(Section(sect)%ClassId,DASH)
                    if (ldx>0) then ! student is accommodated in a lab section
                        if (trim(tClassId)==Section(sect)%ClassId(:ldx-1)) then ! lab of lecture
                            tArray(n_count+1) = std
                            tArray(n_count+2) = eList(std)%Grade(ncol)
                            n_count = n_count+2
                            exit
                        end if
                    end if
                end if
            end do
        end do

        if (trim(tAction)=='Submit' .and. .not. isRoleOfficial) then

            do tdx=first,last,2 ! 1,n_count, 2
                std = tArray(tdx)
                call cgi_get_named_string(QUERY_STRING, trim(Student(std)%StdNo), tGrade, ierr)
                if (ierr==0) then
                    gdx = index_to_grade(tGrade)
                    if (tArray(tdx+1)==gdx) cycle ! no change
                    do ncol=1,eList(std)%lenSubject
                        if (targetSection /= eList(std)%Section(ncol)) cycle
                        eList(std)%Grade(ncol) = gdx
                        tArray(tdx+1) = gdx
                        n_changes = n_changes+1
                        call log_student_record_change(std, 'Grade in '//trim(tClassId)//' is '//txtGrade(pGrade(gdx)) )
                        !call html_comment(itoa(n_changes)//Student(std)%StdNo//tGrade//itoa(gdx))
                        exit
                    end do
                end if
            end do

        end if

        if (trim(tAction)=='Mark' .and. .not. isRoleOfficial) then
            Section(targetSection)%GradeSubmissionDate = currentDate
            mesg = 'Marked "Received:" date as '//Section(targetSection)%GradeSubmissionDate
            n_changes = -1
        end if

        if (trim(tAction)=='Clear' .and. .not. isRoleOfficial) then
            mesg = 'Cleared "Received:" date of '//Section(targetSection)%GradeSubmissionDate
            Section(targetSection)%GradeSubmissionDate = SPACE
            n_changes = -1
        end if

        if (n_changes>0) then
            call xml_write_pre_enlistment(pathToTerm, 'ENLISTMENT', eList, Section)
            mesg = 'Updated grades of '//trim(itoa(n_Changes))//' students in '//tClassId
        else if (n_changes==-1) then
            call xml_write_classes(pathToTerm, NumSections, Section, 0)
        else if (len_trim(tAction)>0 .and. isRoleOfficial) then
            mesg = '"'//trim(tAction)//'" failed. '//sorryMessageOfficial
        end if

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

        call html_write_header(device, 'GRADESHEET', mesg)
        if (len_trim(Section(targetSection)%GradeSubmissionDate)==0) then
            mesg = ' not yet submitted'
        else
            mesg = ' submitted on '//Section(targetSection)%GradeSubmissionDate
        end if
        call list_sections_to_edit(device, thisTerm, Section, ncol-n_count, tArray(n_count+1:), &
            fnTeacherClasses, SPACE, 'Other classes', .true., &
            beginbold//trim(Subject(crse)%Name)//' - '//trim(header)//endbold//SPACE// &
            trim(make_href(fnPrintableGradesheet, 'Printable', &
                    A1=tClassId, A9=thisTerm, pre=nbsp//beginsmall//'( ', post=trim(mesg)//' )'//endsmall//linebreak//linebreak)) )

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
                    begintr//thalignleft//'#'//endth// &
                    thalignleft//'Student No.'//endth, &
                    thalignleft//'Grade'//endth, &
                    thalignleft//'Student Name'//endth// &
                    thalignleft//'Curriculum'//endth//endtr

                do tdx=first,last,2 ! 1,n_count, 2
                    std = tArray(tdx)
                    gdx = tArray(tdx+1)
                    ldx = Student(std)%CurriculumIdx
                    ierr = (tdx+1)/2
                    write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(ierr,2))//'">'// &
                        begintd//trim(itoa(ierr))//DOT//endtd, &
                        begintd//Student(std)%StdNo//endtd, &
                        begintd//'<select name="'//trim(Student(std)%StdNo)//'">'
#if defined REGIST
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
                    write(device,AFORMAT) '</select>'//endtd, &
                        begintd//trim(Student(std)%Name)//endtd// &
                        begintd//trim(Curriculum(ldx)%Code)//endtd//endtr

                end do
                write(device,AFORMAT) begintr//tdnbspendtd//tdnbspendtd// &
                    begintd//'<input type="submit" name="action'//trim(itoa(first))//'" value="Submit">'//endform//endtd, &
                    tdnbspendtd//tdnbspendtd//endtr// &
                    begintr//'<td colspan="5">'//nbsp//linebreak//nbsp//endtr

            end do
            write(device,AFORMAT) endtable

        end if

        if (is_admin_of_college(targetCollege)) then
            write(device,AFORMAT) horizontal
            call make_form_start(device, fnGradesheet, tClassId, A9=thisTerm)
            if (len_trim(Section(targetSection)%GradeSubmissionDate)==0) then
                write(device,AFORMAT) 'Hardcopy of gradesheet not yet received. ', &
                    '<input type="submit" name="action-1" value="Mark"> this gradesheet as "Received:" today.'//endform
            else
                write(device,AFORMAT) 'Hardcopy of gradesheet received on '// &
                    trim(Section(targetSection)%GradeSubmissionDate)//DOT, &
                    '<input type="submit" name="action-1" value="Clear"> this "Received:" date.'//endform
            end if
        end if

        write(device,AFORMAT) horizontal

    end subroutine enlistment_grades



    subroutine enlistment_forced (device, thisTerm, NumSections, Section, eList)
        integer, intent (in) :: device, thisTerm
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in out) :: eList(0:)

        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character(len=MAX_LEN_CLASS_ID) :: tClassId, tAction
        integer :: loc, fdx, crse, sect, ierr, otherSect, tLen1, curriculumFilter
        logical :: isEnlisted, isDirty
        integer, dimension(60,7) :: TimeTable
        logical :: conflicted
        character(len=255) :: statusMesg

        ! which section?
        call cgi_get_named_string(QUERY_STRING, 'A2', tClassId, ierr)
        sect = index_to_section(tClassId, NumSections, Section)
        if (ierr/=0 .or. sect==0) then
            call html_college_links(device, CollegeIdxUser, mesg='Section '//tClassId//' not found.')
            return
        end if
        crse = Section(sect)%SubjectIdx
        targetCollege = Department(Section(sect)%DeptIdx)%CollegeIdx

        call recalculate_available_seats(Section, eList)

        ! which student?
        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
        targetStudent = index_to_student(tStdNo)

        ! action
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)

        isDirty = .false.
        if (trim(tAction)=='Add' .and. .not. isRoleOfficial) then

            if (targetStudent==0) then
                call class_list (device, thisTerm, NumSections, Section, eList, &
                    idx=sect, mesg='Student '//tStdNo//' not found.')
                return
            end if

            ! check if already enlisted
            isEnlisted = .false.
            loc = 0
            otherSect = 0
            statusMesg = SPACE
            do fdx=1,eList(targetStudent)%lenSubject
                if (eList(targetStudent)%Subject(fdx)/=crse) cycle ! not the subject
                loc = fdx
                if (eList(targetStudent)%Section(fdx)==sect) then
                    isEnlisted = .true.
                elseif (eList(targetStudent)%Section(fdx)>0) then
                    otherSect = eList(targetStudent)%Section(fdx)
                end if
            end do

            ! really force add if isAdmin
            if (loc==0 .and. is_admin_of_college(targetCollege) ) then ! add subject
                do fdx=eList(targetStudent)%lenSubject,eList(targetStudent)%NPriority+1,-1
                    eList(targetStudent)%Subject(fdx+1) = eList(targetStudent)%Subject(fdx)
                    eList(targetStudent)%Section(fdx+1) = eList(targetStudent)%Contrib(fdx)
                    eList(targetStudent)%Grade(fdx+1) = eList(targetStudent)%Grade(fdx)
                    eList(targetStudent)%Contrib(fdx+1) = eList(targetStudent)%Contrib(fdx)
                end do
                loc = eList(targetStudent)%NPriority+1
                eList(targetStudent)%NPriority = loc
                eList(targetStudent)%Subject(loc) = crse
                eList(targetStudent)%lenSubject = 1+eList(targetStudent)%lenSubject
            end if

            if (loc==0) then ! not advised to take subject
                statusMesg = 'not advised to take '//Subject(crse)%Name

            else if (isEnlisted) then ! already enlisted
                statusMesg = 'already in '//tClassId

            else if (Section(sect)%RemSlots<=0) then ! class is full
                statusMesg = 'not added - '//trim(tClassId)//' is full.'

            else
                ! collect classes for student; check if conflict
                call timetable_meetings_of_student(NumSections, Section, targetStudent, eList, &
                    0, tLen1, tArray, TimeTable, conflicted)
                if (conflicted .or. is_conflict_timetable_with_section(Section, sect, TimeTable)) then
                    statusMesg = 'not added to '//trim(tClassId)//' due to schedule conflict.'
                else
                    if (otherSect>0) then ! move student
                        Section(otherSect)%RemSlots = Section(otherSect)%RemSlots + 1
                        statusMesg = 'moved from '//Section(otherSect)%ClassId
                    else
                        statusMesg = 'added to '//tClassId
                    end if

                    eList(targetStudent)%Section(loc) = sect
                    eList(targetStudent)%Grade(loc) = gdxREGD
                    eList(targetStudent)%Contrib(loc) = 1.0
                    Section(sect)%RemSlots = Section(sect)%RemSlots - 1
                    isDirty = .true.
                    curriculumFilter = Student(targetStudent)%CurriculumIdx
                    call log_student_record_change(targetStudent, statusMesg)
                end if

            end if

        else if (.not. isRoleOfficial) then ! assume 'Delete'

            if (trim(tStdNo)=='ALL') then ! delete ALL enlisted
                statusMesg = 'delisted from '//tClassId
                do loc=1,NumStudents+NumAdditionalStudents
                    do fdx=1,eList(loc)%lenSubject
                        if (eList(loc)%Section(fdx) /= sect) cycle
                        eList(loc)%Section(fdx) = 0
                        eList(loc)%Grade(fdx) = 0
                        Section(sect)%RemSlots = Section(sect)%RemSlots + 1
                        call log_student_record_change(loc, statusMesg)
                        exit
                    end do
                end do
                isDirty = .true.
                curriculumFilter = 0

            else if (targetStudent==0) then
                call class_list (device, thisTerm, NumSections, Section, eList, &
                    idx=sect, mesg='Student '//tStdNo//' not found.')
                return

            else

                ! check if enlisted
                isEnlisted = .false.
                loc = 0
                do fdx=1,eList(targetStudent)%lenSubject
                    if (eList(targetStudent)%Section(fdx) == sect) then
                        isEnlisted = .true.
                        loc = fdx
                    end if
                end do
                if (.not. isEnlisted) then
                    statusMesg = 'is NOT in '//tClassId
                else
                    eList(targetStudent)%Section(loc) = 0
                    eList(targetStudent)%Grade(loc) = 0
                    Section(sect)%RemSlots = Section(sect)%RemSlots + 1
                    statusMesg = 'deleted from '//tClassId
                    isDirty = .true.
                    curriculumFilter = Student(targetStudent)%CurriculumIdx
                    call log_student_record_change(targetStudent, statusMesg)
                end if

            end if

        end if

        if (len_trim(tAction)>0 .and. isRoleOfficial) then
            statusMesg = ' : "'//trim(tAction)//'" failed. '//sorryMessageOfficial
        end if

        call class_list (device, thisTerm, NumSections, Section, eList, idx=sect, &
            mesg='Student '//tStdNo//statusMesg)

        if (isDirty) then
            call xml_write_pre_enlistment(pathToTerm, 'ENLISTMENT', eList, Section, curriculumFilter)
        end if

    end subroutine enlistment_forced


    subroutine enlistment_edit (device, thisTerm, NumSections, Section, NumBlocks, Block, eList)
        integer, intent (in) :: device, thisTerm
        integer, intent (in out) :: NumBlocks, NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_BLOCK), intent(in out) :: Block(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in out) :: eList(0:)
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character(len=MAX_LEN_BLOCK_CODE) :: tBlock
        character(len=MAX_LEN_CLASS_ID) :: tClassId
        character(len=3*MAX_LEN_SUBJECT_CODE) :: tAction
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject
        integer :: bdx, blk, fdx, mdx, crse, lect, sect, ierr, tLen1, tLen2, pos, n_opts, idx_opt
        integer :: n_changes, nPassedPE, nAdvisedPE, nEnlistedPE, collegeOfStudent
        integer, dimension(60,7) :: TimeTable
        logical :: conflicted, matched, addPE
        character(len=255) :: mesg
        logical :: isDirtyFORM5, selfEnlistPE, selfEnlistALL, allowed_to_edit
        type (TYPE_PRE_ENLISTMENT) :: Advice

        ! which student?
        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
        targetStudent = index_to_student(tStdNo)
        targetCurriculum = Student(targetStudent)%CurriculumIdx
        collegeOfStudent = Curriculum(targetCurriculum)%CollegeIdx

        if (is_admin_of_college(collegeOfStudent)) then
            allowed_to_edit = eList(targetStudent)%Status<SCHEDULE_IS_LOCKED
        end if

        selfEnlistPE = trim(USERNAME)==tStdNo .and. College(collegeOfStudent)%AllowEnlistmentInPE(thisTerm)
        selfEnlistALL = trim(USERNAME)==tStdNo .and. College(collegeOfStudent)%AllowEnlistmentInALL(thisTerm)
        if (trim(USERNAME)==tStdNo) then
            allowed_to_edit = &
                isPeriodOne .and. thisTerm==currentTerm .and. &
                (selfEnlistAll .or. selfEnlistPE) .and. eList(targetStudent)%Status<SCHEDULE_IS_LOCKED
        end if

        if (is_adviser_of_student(targetStudent)) then
            allowed_to_edit = &
                (isPeriodOne .and. thisTerm==currentTerm .and. eList(targetStudent)%Status<SCHEDULE_IS_LOCKED) &
                .or. (.not. isPeriodOne .and. thisTerm==nextTerm )
        end if

        !allowed_to_edit = is_admin_of_college(Curriculum(Student(targetStudent)%CurriculumIdx)%CollegeIdx) .or. &
        !        ( (selfEnlistAll .or. selfEnlistPE) .and. eList(targetStudent)%Status<SCHEDULE_IS_CONFIRMED ) .or. &
        !        ( is_adviser_of_student(targetStudent, orHigherUp) .and. eList(targetStudent)%Status<SCHEDULE_IS_LOCKED) ) .or. &
        !    (.not. isPeriodOne .and. thisTerm==nextTerm .and. is_adviser_of_student(targetStudent, orHigherUp) )

        call recalculate_available_seats(Section,eList)
        isDirtyFORM5 = .false.

        ! check for other arguments
        mesg = SPACE
        call cgi_get_named_string(QUERY_STRING, 'A2', tAction, ierr)

        if (trim(tAction)=='Add' .and. .not. isRoleOfficial) then

            call cgi_get_named_string(QUERY_STRING, 'A3', tClassId, ierr)
            sect = index_to_section(tClassId, NumSections, Section)
            if (sect>0) then ! target of action is indexed by sect
                if (selfEnlistPE .and. tClassId(1:3)/='PE ') then
                    mesg = 'Cannot add '//trim(tClassId)//': only PE can be added/deleted at this time.'
                else if (Section(sect)%RemSlots>0) then
                    crse = Section(sect)%SubjectIdx
                    pos = 0
                    do fdx=1,eList(targetStudent)%lenSubject
                        if (eList(targetStudent)%Subject(fdx)==crse) then
                            eList(targetStudent)%Section(fdx) = sect
                            eList(targetStudent)%Grade(fdx) = gdxREGD
                            Section(sect)%RemSlots = Section(sect)%RemSlots - 1
                            pos = fdx
                            exit
                        end if
                    end do
                    if (pos==0) then ! add
                        if (tClassId(1:3)=='PE ') then ! shift down
                            do fdx=eList(targetStudent)%lenSubject,1,-1
                                eList(targetStudent)%Subject(fdx+1) = eList(targetStudent)%Subject(fdx)
                                eList(targetStudent)%Section(fdx+1) = eList(targetStudent)%Section(fdx)
                                eList(targetStudent)%Grade(fdx+1) = eList(targetStudent)%Grade(fdx)
                            end do
                            ! add PE at the beginning
                            eList(targetStudent)%Subject(1) = crse
                            eList(targetStudent)%Section(1) = sect
                            eList(targetStudent)%Grade(1) = gdxREGD
                            eList(targetStudent)%lenSubject =  eList(targetStudent)%lenSubject + 1
                            eList(targetStudent)%NPriority = eList(targetStudent)%NPriority + 1
                            Section(sect)%RemSlots = Section(sect)%RemSlots - 1
                            mesg = 'Added '//tClassId
                            isDirtyFORM5 = .true.
                        else
                            mesg = 'NOT added: previously deleted '//tClassId
                        end if
                    else
                        mesg = 'Added '//tClassId
                        isDirtyFORM5 = .true.
                    end if
                else
                    mesg = 'NOT added, already full: '//tClassId
                end if
            end if

        end if

        if (trim(tAction)=='Block' .and. .not. isRoleOfficial) then

            call cgi_get_named_string(QUERY_STRING, 'A3', tBlock, ierr)
            blk = index_to_block(tBlock, NumBlocks, Block)
            if (blk>0) then ! target block found

                do bdx=1,Block(blk)%NumClasses
                    crse = Block(blk)%Subject(bdx)
                    sect = Block(blk)%Section(bdx)
                    if (sect==0) then ! section not specified in block
                        mesg = 'NOT added (no section): '//trim(Subject(crse)%Name)//'; '//mesg
                        cycle
                    end if
                    tClassId = Section(sect)%ClassId
                    matched = .false.
                    do fdx=1,eList(targetStudent)%lenSubject
                        if (crse/=eList(targetStudent)%Subject(fdx)) cycle
                        matched = .true.
                        exit
                    end do
                    if (matched) then ! indexed by bdx/fdx

                        if (Section(sect)%RemSlots>0) then
                            do fdx=1,eList(targetStudent)%lenSubject
                                if (eList(targetStudent)%Subject(fdx)/=crse) cycle
                                eList(targetStudent)%Section(fdx) = sect
                                eList(targetStudent)%Grade(fdx) = gdxREGD
                                Section(sect)%RemSlots = Section(sect)%RemSlots - 1
                                exit
                            end do
                            mesg = 'Added '//trim(tClassId)//'; '//mesg
                            isDirtyFORM5 = .true.
                        else
                            mesg = 'NOT added '//trim(tClassId)//' (full); '//mesg
                        end if

                    else
                        mesg = 'NOT advised '//trim(Subject(crse)%Name)//'; '//mesg
                    end if

                end do

            else
                mesg = 'Block "'//trim(tBlock)//'" not found'
            end if

        end if

        if (trim(tAction)=='Del' .and. .not. isRoleOfficial) then

            call cgi_get_named_string(QUERY_STRING, 'A3', tClassId, ierr)
            sect = index_to_section(tClassId, NumSections, Section)
            if (selfEnlistPE .and. tClassId(1:3)/='PE ') then
                mesg = 'Cannot delete '//trim(tClassId)//': only PE can be added/deleted at this time.'
            else if (sect>0) then ! target of action is indexed by sect
                if (tClassId(1:3)=='PE ') then ! delete subject and section
                    ! find & clear location of target PE
                    pos = 0
                    do fdx=1,eList(targetStudent)%lenSubject
                        if (sect==eList(targetStudent)%Section(fdx)) then
                            eList(targetStudent)%Section(fdx) = 0
                            eList(targetStudent)%Grade(fdx) = 0
                            Section(sect)%RemSlots = Section(sect)%RemSlots + 1
                            pos = fdx
                            exit
                        end if
                    end do
                    ! still there, or already deleted by someone else?
                    if (pos/=0) then ! shift
                        do fdx=pos,eList(targetStudent)%lenSubject-1
                            eList(targetStudent)%Subject(fdx) = eList(targetStudent)%Subject(fdx+1)
                            eList(targetStudent)%Section(fdx) = eList(targetStudent)%Section(fdx+1)
                            eList(targetStudent)%Grade(fdx) = eList(targetStudent)%Grade(fdx+1)
                        end do
                        ! clear last location
                        eList(targetStudent)%Subject(eList(targetStudent)%lenSubject) = 0
                        eList(targetStudent)%Section(eList(targetStudent)%lenSubject) = 0
                        eList(targetStudent)%Grade(eList(targetStudent)%lenSubject) = 0
                        ! reduce
                        eList(targetStudent)%lenSubject = eList(targetStudent)%lenSubject-1
                        if (pos<=eList(targetStudent)%NPriority) then
                            eList(targetStudent)%NPriority = eList(targetStudent)%NPriority-1
                        end if
                        mesg = 'Deleted '//tClassId
                        isDirtyFORM5 = .true.
                    else
                        mesg = 'No longer enlisted in '//tClassId
                    end if

                else ! delete section only
                    matched = .false.
                    do fdx=1,eList(targetStudent)%lenSubject
                        if (sect==eList(targetStudent)%Section(fdx)) then
                            eList(targetStudent)%Section(fdx) = 0
                            eList(targetStudent)%Grade(fdx) = 0
                            Section(sect)%RemSlots = Section(sect)%RemSlots + 1
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

                Advice%lenSubject = Advice%NPriority+Advice%NAlternates+Advice%NCurrent
                do fdx=1,Advice%lenSubject

                    crse = Advice%Subject(fdx)
                    tSubject = Subject(crse)%Name
                    ! check if already enlisted
                    do mdx=1,eList(targetStudent)%lenSubject
                        if (eList(targetStudent)%Subject(mdx)/=crse) cycle
                        if (eList(targetStudent)%Section(mdx)>0) then
                            call html_comment('Retained section '// &
                                Section(eList(targetStudent)%Section(mdx))%Classid)
                        else
                            call html_comment('Retained subject '//tSubject)
                        end if
                        Advice%Section(fdx) = eList(targetStudent)%Section(mdx)
                        Advice%Grade(fdx) = gdxREGD
                        eList(targetStudent)%Section(mdx) = 0 ! remove section
                        exit
                    end do

                end do

                ! free slots for deleted sections
                do fdx=1,eList(targetStudent)%lenSubject
                    sect = eList(targetStudent)%Section(fdx)
                    if (sect>0) then
                        Section(sect)%RemSlots = Section(sect)%RemSlots + 1
                        call html_comment('Freed a seat in '//Section(sect)%ClassId)
                    end if
                end do
                ! update eList()
                eList(targetStudent) = Advice
                eList(targetStudent)%Status = SUBJECTS_ARE_ADVISED
                isDirtyFORM5 = .true.

        end if

        if (trim(tAction)=='CONFIRM Schedule' .and. .not. isRoleOfficial) then

            eList(targetStudent)%Status = SCHEDULE_IS_CONFIRMED; ! status below SCHEDULE_IS_LOCKED
            mesg = 'CONFIRMED the schedule for '//txtSemester(thisTerm+3)//termQualifier(thisTerm+3)
            isDirtyFORM5 = .true.

        end if

        if (trim(tAction)=='LOCK Schedule' .and. .not. isRoleOfficial) then

            ! keep only enlisted subjects
            mdx = 0
            do fdx=1,eList(targetStudent)%lenSubject
                if (eList(targetStudent)%Section(fdx)==0) cycle
                mdx = mdx + 1
                eList(targetStudent)%Subject(mdx) = eList(targetStudent)%Subject(fdx)
                eList(targetStudent)%Section(mdx) = eList(targetStudent)%Section(fdx)
                eList(targetStudent)%Grade(mdx) = eList(targetStudent)%Grade(fdx)
                eList(targetStudent)%Contrib(mdx) = eList(targetStudent)%Contrib(fdx)
            end do
            eList(targetStudent)%lenSubject = mdx
            eList(targetStudent)%NPriority = mdx
            eList(targetStudent)%NAlternates = 0
            eList(targetStudent)%NCurrent = 0
            eList(targetStudent)%Status = SCHEDULE_IS_LOCKED;
            mesg = 'LOCKED schedule for '//txtSemester(thisTerm+3)//termQualifier(thisTerm+3)
            isDirtyFORM5 = .true.

        end if

        if (trim(tAction)=='UNLOCK Schedule' .and. .not. isRoleOfficial) then

            eList(targetStudent)%Status = SCHEDULE_IS_CONFIRMED; ! status below SCHEDULE_IS_LOCKED
            mesg = 'UNLOCKED schedule for '//txtSemester(thisTerm+3)//termQualifier(thisTerm+3)
            isDirtyFORM5 = .true.

        end if

        if (trim(tAction)=='DELIST Student' .and. .not. isRoleOfficial) then

            call initialize_pre_enlistment(eList(targetStudent))
            mesg = 'Delisted student from classes in '//txtSemester(thisTerm+3)//termQualifier(thisTerm+3)
            isDirtyFORM5 = .true.

        end if

        if (len_trim(tAction)>0 .and. isRoleOfficial) then
            mesg = '"'//trim(tAction)//'" failed. '//sorryMessageOfficial
        end if

        if (isDirtyFORM5) then
            call xml_write_pre_enlistment(pathToTerm, 'ENLISTMENT', eList, Section, &
                Student(targetStudent)%CurriculumIdx)
            call log_student_record_change(targetStudent, mesg )
        end if

        call html_write_header(device, trim(Student(targetStudent)%StdNo)//SPACE//trim(Student(targetStudent)%Name)//linebreak// &
            trim(text_curriculum_info(Student(targetStudent)%CurriculumIdx))//linebreak//linebreak//'Schedule of Classes ', mesg)

        ! collect classes for student
        call timetable_meetings_of_student(NumSections, Section, targetStudent, eList, 0, tLen1, tArray, TimeTable, &
            conflicted)

        call list_sections_to_edit(device, thisTerm, Section, tLen1, tArray, fnChangeMatriculation, tStdNo, 'Del', &
            allowed_to_edit, beginbold//'Enlisted subjects'//endbold//nbsp//trim(make_href(fnPrintableSchedule, 'Printable', &
            A1=tStdNo, A9=thisTerm, pre=beginsmall//'(', post=')'//endsmall)) )

        ! print timetable and exit if last term
        if (thisTerm/=currentTerm) then
            call timetable_display(device, Section, TimeTable)
            write(device,AFORMAT) horizontal
            return
        end if

        select case (eList(targetStudent)%Status)

            case (0) ! enlistment status NONE
                if (tLen1>0) then
                    eList(targetStudent)%Status = SECTIONS_ARE_SELECTED
                elseif (eList(targetStudent)%lenSubject>0) then
                    eList(targetStudent)%Status = SUBJECTS_ARE_ADVISED
                end if

            case (SUBJECTS_ARE_ADVISED)
                if (eList(targetStudent)%lenSubject==0) then ! no advised subjects
                    eList(targetStudent)%Status = 0
                elseif (tLen1>0) then ! some enlisted
                    eList(targetStudent)%Status = SECTIONS_ARE_SELECTED
                end if

            case (SECTIONS_ARE_SELECTED, SCHEDULE_IS_CONFIRMED, SCHEDULE_IS_LOCKED)
                if (eList(targetStudent)%lenSubject==0) then ! no advised subject
                    eList(targetStudent)%Status = 0
                elseif (tLen1==0) then ! none enlisted
                    eList(targetStudent)%Status = SUBJECTS_ARE_ADVISED
                end if

        end select

        select case (eList(targetStudent)%Status)
            case (0)
                mesg = 'Enlistment status is not set.'
            case (SUBJECTS_ARE_ADVISED)
                mesg = 'Subjects finalized by adviser.'
            case (SECTIONS_ARE_SELECTED)
                mesg = 'Student is enlisted into sections. '// &
                    'Adviser can change subjects or sections. '// &
                    'If self-enlistment is enabled, student can add feasible subjects with available sections.'
            case (SCHEDULE_IS_CONFIRMED)
                mesg = 'Schedule is confirmed by student but not locked by Registrar. '// &
                    'Adviser can change subjects or sections. '// &
                    'If self-enlistment is enabled, student can add feasible subjects with available sections.'
            case (SCHEDULE_IS_LOCKED)
                mesg = 'Schedule is locked and assessment form printed. See the Registrar to unlock.'
        end select
        if (tLen1==0) mesg = 'Pre-enlist student into feasible subjects with available sections below, or '// &
            'click Checklist to change subjects.'

        if (trim(USERNAME)==tStdNo) then ! student view
            if (tLen1==0) mesg = 'If self-enlistment is enabled, build your own schedule using the Add or Del links. '// &
                'See your adviser if a subject you need is not in the feasible list, or if you cannot enlist in any subject.'
            select case (eList(targetStudent)%Status)
                case (SECTIONS_ARE_SELECTED)
                    if (tLen1>0) then
                        call make_form_start(device, fnChangeMatriculation, A1=tStdNo, A9=thisTerm)
                        write(device,AFORMAT) '<input type="submit" name="A2" value="CONFIRM Schedule"> ', &
                            'to signify that the Registrar can lock the schedule and print the assessment form. '//linebreak, &
                            beginitalic//'After confirmation, while the schedule is not locked, your adviser '// &
                            'can modify the schedule; and, if self-enlistment is enabled, you can also modify the schedule.'// &
                            enditalic, &
                            endform
                    else
                        write(device,AFORMAT) beginitalic//trim(mesg)//enditalic//linebreak
                    end if
                case default
                    write(device,AFORMAT) beginitalic//trim(mesg)//enditalic//linebreak
            end select

        elseif (is_adviser_of_student(targetStudent)) then
            write(device,AFORMAT) beginitalic//trim(mesg)//enditalic//linebreak

        elseif (is_admin_of_college(collegeOfStudent)) then ! Registrar/staff view
            call make_form_start(device, fnChangeMatriculation, A1=tStdNo, A9=thisTerm)
            if (tLen1==0) then
                write(device,AFORMAT) beginitalic//trim(mesg)//enditalic//linebreak
            else
                select case (eList(targetStudent)%Status)
                    case (SECTIONS_ARE_SELECTED)
                        write(device,AFORMAT) '<input type="submit" name="A2" value="CONFIRM Schedule"> '// &
                            '(as requested by student)'//nbsp//nbsp//nbsp
                    case (SCHEDULE_IS_CONFIRMED)
                        write(device,AFORMAT) '<input type="submit" name="A2" value="LOCK Schedule">'//nbsp//nbsp//nbsp
                    case (SCHEDULE_IS_LOCKED)
                        write(device,AFORMAT) '<input type="submit" name="A2" value="UNLOCK Schedule">'//nbsp//nbsp//nbsp
                    case default
                        write(device,AFORMAT) beginitalic//trim(mesg)//enditalic//linebreak
                end select
            end if
            ! student not enrolling
            write(device,AFORMAT)  &
                '<input type="submit" name="A2" value="DELIST Student"> (not enrolling)'//endform
        end if

        addPE = .false.
        if (tLen1>0) then
            call timetable_display(device, Section, TimeTable)

            ! PE needed?
            ! read checklist
            call read_student_records (targetStudent)

            nPassedPE = 0 ! how many passed PE
            do fdx=1,lenTCG
                if (TCG(fdx)%Code/=3 .or. TCG(fdx)%Used .or. TCG(fdx)%ErrorCode>1) cycle
                if (Subject(TCG(fdx)%Subject)%Name(1:3)/='PE ') cycle
                if ( is_grade_passing(TCG(fdx)%Grade) .or. is_grade_passing(TCG(fdx)%ReExam) ) nPassedPE = nPassedPE + 1
            end do

            nAdvisedPE = 0 ! how many advised but not enlisted
            nEnlistedPE = 0 ! how many enlisted
            do fdx=1,eList(targetStudent)%lenSubject
                tSubject = Subject(eList(targetStudent)%Subject(fdx))%Name
                if (tSubject(1:3)/='PE ') cycle ! not PE
                if (eList(targetStudent)%Section(fdx)==0) then
                    nAdvisedPE = nAdvisedPE + 1 ! advised/not enlisted
                else
                    nEnlistedPE = nEnlistedPE + 1 ! enlisted
                end if
            end do
            call html_comment('Passed PE = '//trim(itoa(nPassedPE)), 'Advised PE = '//trim(itoa(nAdvisedPE)), &
                'Enlisted PE = '//trim(itoa(nEnlistedPE)) )

            !addPE = (nPassedPE + nEnlistedPE + nAdvisedPE < 4) .and. (nEnlistedPE + nAdvisedPE <= 2) .and. &
            addPE = (nPassedPE + nEnlistedPE < 4) .and. (nEnlistedPE <= 2) .and. &
                allowed_to_edit
        end if

        ! make list of available sections for alternate subjects that fit the schedule of student
        tLen2 = 0
        do fdx=1,eList(targetStudent)%lenSubject
            if (eList(targetStudent)%Section(fdx)/=0) cycle ! already enlisted
            tLen2 = tLen2 + 1
        end do

        if ( (tLen2>0 .or. addPE) .and. allowed_to_edit) then
            mdx = 0
            write(device,AFORMAT) linebreak//beginbold//'Other feasible subjects'//endbold//':'
            if (addPE) then
                write(device,AFORMAT) linebreak//trim(itoa(mdx))//'). <a href="#PE">PE</a> - Physical Education'
            end if
            do fdx=1,eList(targetStudent)%lenSubject
                if (eList(targetStudent)%Section(fdx)==0) then
                    tSubject = Subject(eList(targetStudent)%Subject(fdx))%Name
                    if (addPE .and. tSubject(1:3)=='PE ') cycle ! add below
                    mdx = mdx+1
                    write(device,AFORMAT) linebreak//trim(itoa(mdx))//'). <a href="#'// &
                        trim(tSubject)//'">'//trim(tSubject)//'</a> - '// &
                        trim(Subject(eList(targetStudent)%Subject(fdx))%Title)
                end if
            end do
            write(device,AFORMAT) linebreak

            if (addPE) then

                tSubject = 'PE'
                n_opts = 0
                do sect=1,NumSections
                    if (Section(sect)%ClassID(1:3)/='PE ') cycle ! not the right subject
                    if (is_conflict_timetable_with_section(Section, sect, TimeTable)) cycle ! in conflict
                    ! place options at end of Section() array
                    idx_opt = NumSections + n_opts + 1
                    Section(idx_opt) = Section(sect)
                    n_opts = n_opts + 1
                end do !  sect=1,NumSections

                call timetable_undesirability(n_opts, NumSections, Section, TimeTable)

                ! available sections that fit schedule - can be added
                tLen2 = 0
                do sect=1,n_opts
                    idx_opt = UndesirabilityRank(sect)
                    if (Section(NumSections+idx_opt)%RemSlots<=0) cycle ! section not available
                    !write(*,*) sect, idx_opt, Section(NumSections+idx_opt)%ClassId, -Undesirability(idx_opt)
                    do mdx=1,Section(NumSections+idx_opt)%NMeets
                        tArray(tLen1+tLen2+1) = NumSections+idx_opt
                        tArray(tLen1+tLen2+2) = mdx
                        tArray(tLen1+tLen2+3) = -Undesirability(idx_opt)
                        tLen2 = tLen2+3
                    end do !  mdx=1,Section(sect)%NMeets
                end do
                if (tLen2>0) then ! there are sections that can be added
                    ! end of list markers
                    tArray(tLen1+tLen2+1) = 0
                    tArray(tLen1+tLen2+2) = 0
                    tArray(tLen1+tLen2+3) = 0
                    call list_sections_to_edit(device, thisTerm, Section, tLen2, tArray(tLen1+1), &
                        fnChangeMatriculation, tStdNo, 'Add', allowed_to_edit, &
                        '<a name="'//trim(tSubject)//'"></a>'//linebreak//horizontal// &
                        green//beginbold//'AVAILABLE sections'//black//' in '//trim(tSubject)// &
                        ' that fit existing schedule, sorted by undesirability.'//endbold)

                end if

            end if

            ! non-PE
            do fdx=1,eList(targetStudent)%lenSubject
                if (eList(targetStudent)%Section(fdx)/=0) cycle ! already enlisted
                crse = eList(targetStudent)%Subject(fdx) ! index to subject
                tSubject = Subject(crse)%Name
                if (addPE .and. tSubject(1:3)=='PE ') cycle
                !write(*,*) 'Alternate subject - '//tSubject
                n_opts = 0

                do sect=1,NumSections
                    if (crse/=Section(sect)%SubjectIdx) cycle ! not the right subject

                    ! not in student's college
                    !if (Department(Section(sect)%DeptIdx)%CollegeIdx/=Curriculum(targetCurriculum)%CollegeIdx) cycle ! not in student's college

                    ! not in ADMIN or student's college
                    !if (Department(Section(sect)%DeptIdx)%CollegeIdx/=NumColleges .and. &
                    !    Department(Section(sect)%DeptIdx)%CollegeIdx/=Curriculum(targetCurriculum)%CollegeIdx ) cycle

                    ! do not add lecture class if a lect-lab section
                    if (is_lecture_lab_subject(crse) .and. is_lecture_class(sect, Section)) then
                        cycle
                    end if

                    ! check for conflict
                    if (is_conflict_timetable_with_section(Section, sect, TimeTable)) then
                        !write(*,*) '   Available, but schedule conflict - '//Section(sect)%ClassId
                        cycle
                    end if
                    ! lab section of lect+lab subject or lab-only subject, or section of lect-only subject, is OK
                    ! if lab section of lect+lab subject, check if lecture schedule also fits

                    ! place options at end of Section() array
                    idx_opt = NumSections + n_opts + 1
                    Section(idx_opt) = Section(sect)
                    if (is_lecture_lab_subject(crse)) then ! find the lecture section
                        pos = index(Section(sect)%ClassId, DASH)
                        tClassId = Section(sect)%ClassId(:pos-1)
                        lect = index_to_section(tClassId, NumSections, Section)
                        if (is_conflict_timetable_with_section(Section, lect, TimeTable)) then ! lecture class is not OK
                            !write(*,*) '   Available, but lecture class schedule conflict - '//Section(lect)%ClassId
                            cycle
                        end if
                        !write(*,*) 'OPTION IS - '//tClassId
                        ! add lecture schedule to lab schedule
                        do mdx=1,Section(lect)%NMeets
                            pos = Section(idx_opt)%NMeets + 1
                            Section(idx_opt)%DayIdx(pos) = Section(lect)%DayIdx(mdx)
                            Section(idx_opt)%bTimeIdx(pos) = Section(lect)%bTimeIdx(mdx)
                            Section(idx_opt)%eTimeIdx(pos) = Section(lect)%eTimeIdx(mdx)
                            Section(idx_opt)%RoomIdx(pos) = Section(lect)%RoomIdx(mdx)
                            Section(idx_opt)%TeacherIdx(pos) = Section(lect)%TeacherIdx(mdx)
                            Section(idx_opt)%NMeets = pos
                        end do !  mdx=1,Section(lect)%NMeets
                    end if
                    n_opts = n_opts + 1

                end do !  sect=1,NumSections

                call timetable_undesirability(n_opts, NumSections, Section, TimeTable)

                ! available sections that fit schedule - can be added
                tLen2 = 0
                do sect=1,n_opts
                    idx_opt = UndesirabilityRank(sect)
                    if (Section(NumSections+idx_opt)%RemSlots<=0) cycle ! section not available
                    !write(*,*) sect, idx_opt, Section(NumSections+idx_opt)%ClassId, -Undesirability(idx_opt)
                    do mdx=1,Section(NumSections+idx_opt)%NMeets
                        tArray(tLen1+tLen2+1) = NumSections+idx_opt
                        tArray(tLen1+tLen2+2) = mdx
                        tArray(tLen1+tLen2+3) = -Undesirability(idx_opt)
                        tLen2 = tLen2+3
                    end do !  mdx=1,Section(sect)%NMeets
                end do
                if (tLen2>0) then ! there are sections that can be added
                    ! end of list markers
                    tArray(tLen1+tLen2+1) = 0
                    tArray(tLen1+tLen2+2) = 0
                    tArray(tLen1+tLen2+3) = 0
                    call list_sections_to_edit(device, thisTerm, Section, tLen2, tArray(tLen1+1), &
                        fnChangeMatriculation, tStdNo, 'Add', allowed_to_edit, &
                        '<a name="'//trim(tSubject)//'"></a>'//linebreak//horizontal// &
                        green//beginbold//'AVAILABLE sections'//black//' in "'//trim(tSubject)// &
                        ' - '//trim(Subject(crse)%Title)// &
                        '" that fit existing schedule, sorted by undesirability.'//endbold, .true.)

                end if

            end do !  fdx=1,eList(targetStudent)%lenSubject

        end if
        write(device,AFORMAT) horizontal

    end subroutine enlistment_edit


    subroutine enlistment_printable  (device, NumSections, Section, eList)
        integer, intent (in) :: device
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in) :: eList(0:)
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        integer ::  ierr, tLen1
        integer, dimension(60,7) :: TimeTable
        logical :: conflicted

        ! which student?
        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
        targetStudent = index_to_student(tStdNo)
        targetCurriculum = Student(targetStudent)%CurriculumIdx

        call timetable_meetings_of_student(NumSections, Section, targetStudent, eList, 0, tLen1, tArray, &
            TimeTable, conflicted)
        call enlistment_class_schedule(device, targetStudent, Section, tLen1, tArray)
        !call timetable_display(device, Section, TimeTable)

        write(device,AFORMAT) horizontal

    end subroutine enlistment_printable


    subroutine enlistment_find_block(device, thisTerm, Section, NumBlocks, Block, eList)
        integer, intent (in) :: device, thisTerm
        integer, intent (in out) :: NumBlocks
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_BLOCK), intent(in out) :: Block(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in) :: eList(0:)
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        integer :: ierr, crse, sect, seats
        integer :: bdx, fdx, blk, n_blks, n_matches, in_block
        logical :: matched

        ! which student?
        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
        targetStudent = index_to_student(tStdNo)
        targetCurriculum = Student(targetStudent)%CurriculumIdx
        targetCollege = Curriculum(targetCurriculum)%CollegeIdx

        call recalculate_available_seats(Section, eList)

        call html_write_header(device, trim(Student(targetStudent)%StdNo)//SPACE//trim(Student(targetStudent)%Name)// &
            SPACE//DASH//SPACE//trim(Curriculum(targetCurriculum)%Code))

        write(device,AFORMAT) '<table border="0" width="100%">'//begintr, &
        thalignleft//'Block ID'//endth// &
        thalignleft//'# Matches'//endth// &
        thalignleft//'Class ID (seats)'//endth//endtr

        n_blks = 0
        do blk=1,NumBlocks
            !if (CurrProgCode(Block(blk)%CurriculumIdx)/=CurrProgCode(targetCurriculum)) cycle ! not this curriculum
            !if (Block(blk)%CurriculumIdx/=targetCurriculum) cycle ! not this curriculum
            ! count how many subjects in this block match the predicted subjects of the student
            n_matches = 0
            in_block = 0 ! subjects in block with assigned sections
            do bdx=1,Block(blk)%NumClasses
                crse = Block(blk)%Subject(bdx)
                if (Block(blk)%Section(bdx)>0) in_block = in_block + 1;
                do fdx=1,eList(targetStudent)%lenSubject
                    if (crse/=eList(targetStudent)%Subject(fdx)) cycle
                    n_matches = n_matches+1
                    exit
                end do
            end do
            if (n_matches==0) cycle

            n_blks = n_blks + 1
            write(device,AFORMAT) begintr// &
                begintd//trim(make_href(fnBlockSchedule, Block(blk)%BlockID, A1=Block(blk)%BlockID, A9=thisTerm))//endtd// &
                begintd//trim(itoa(n_matches))//SPACE//FSLASH//SPACE//trim(itoa(in_block))//endtd// &
                begintd
            matched = .false.
            do bdx=1,Block(blk)%NumClasses
                crse = Block(blk)%Subject(bdx)
                do fdx=1,eList(targetStudent)%lenSubject
                    if (crse/=eList(targetStudent)%Subject(fdx)) cycle
                    matched = .true.
                    exit
                end do
                if (.not. matched) cycle
                sect = Block(blk)%Section(bdx)
                if (sect/=0) then
                    seats = Section(sect)%RemSlots
                    if (seats>0) then
                        write(device,AFORMAT) beginbold//green//trim(Section(sect)%ClassId)//' ('// &
                        trim(itoa(seats))//') '//black//endbold//' /'//nbsp
                    else
                        write(device,AFORMAT) beginbold//green//trim(Section(sect)%ClassId)//black//red//' ('// &
                        trim(itoa(seats))//') '//black//endbold//' /'//nbsp
                    end if
                else
                    write(device,AFORMAT) beginbold//red//trim(Subject(crse)%Name)//black//endbold//' /'//nbsp
                end if
                if (mod(bdx,4)==0 .and. Block(blk)%NumClasses>4) then
                    write(device,AFORMAT) endtd//endtr// & ! end row
                    begintr//tdnbspendtd//tdnbspendtd//begintd ! new row with first 2 columns empty
                end if
            end do
            if ( (is_adviser_of_student(targetStudent, orHigherUp) .and. &
                  (isPeriodOne .and. thisTerm==currentTerm) .or. &
                  (.not. isPeriodOne .and. thisTerm==nextTerm) ) .or. &
                  is_admin_of_college(targetCollege) ) &
                write(device,AFORMAT) trim(make_href(fnChangeMatriculation, 'Enlist', A1=tStdNo, A2='Block', &
                    A3=Block(blk)%BlockID, A9=thisTerm, pre=nbsp, post=endtd//endtr ))
        end do
        if (n_blks==0) then
            write(device,AFORMAT) begintr//'<td colspan="3">No suitable blocks for this student?'//endtd//endtr
        end if
        write(device,AFORMAT) endtable, linebreak, &
            beginitalic//'NOTES'//enditalic//' : A  '//beginbold//green//'Class ID (seats)'//black//endbold//' is open. A  '// &
            beginbold//green//'Class ID '//black, &
            red//'(0)'//black//endbold//' is NOT available. A  '//beginbold//red//'SUBJECT'//black//endbold// &
            ' is NOT assigned to a section.', &
            linebreak//'"Enlist" will add the student ONLY to the open sections of a block.', &
            linebreak//'Seats in an open section may be taken while you are reading this.', &
            horizontal

    end subroutine enlistment_find_block


    subroutine enlistment_class_schedule(device, std, Section, lenSL, SectionList)
        integer, intent(in) :: device, std, lenSL, SectionList(3*lenSL+3)
        type (TYPE_SECTION), intent(in) :: Section(0:)
        integer :: crse, idx, mdx, rdx, rdx1, sect, previous, conflict
        real :: totalUnits, classUnits, totalHours, classHours, totalTuition, classTuition, totalLabFee, classLabFee
        real :: EnergyFee, totalEnergyFee, totalFiduciaryFee, totalOtherFee

        write(device,AFORMAT) beginbold//trim(Student(std)%StdNo)//SPACE//trim(Student(std)%Name)// &
            linebreak//text_curriculum_info(Student(std)%CurriculumIdx)//linebreak//linebreak//'Enlisted subjects for '// &
            trim(termDescription)//endbold, &
            linebreak//beginitalic// &
            'Subject fees may be assessed for laboratories and NSTP/ROTC.', &
            enditalic//horizontal

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

        write(device,AFORMAT) '<table border="0" width="100%">'//begintr, &
            thalignleft//'Subject'//endth// &
            thalignleft//'Section'//endth//&
            thalignleft//'Units'//endth// &
            thalignleft//'Hours'//endth// &
            thalignleft//'Tuition'//endth// &
            thalignleft//'Subject fee'//endth// &
            thalignleft//'Time'//endth// &
            thalignleft//'Day'//endth//&
            thalignleft//'Room'//endth//endtr

        previous = 0
        do idx=1,lenSL,3
            sect=SectionList(idx)
            !mdx=SectionList(idx+1)
            conflict=SectionList(idx+2)
            crse = Section(sect)%SubjectIdx

            !new section ?
            if (sect/=previous) then ! include subject, section, units/blockname, seats/hours, time, day

                if (is_lecture_lab_subject(crse)) then
                    if (is_lecture_class(sect, Section)) then ! lecture of lecture-lab
                        classUnits = Subject(crse)%Units
                        classHours = Subject(crse)%LectHours
                        classTuition = Subject(crse)%Tuition
                        classLabFee = 0.0
                    else ! lab of lecture-lab
                        classUnits = 0.0
                        classHours = Subject(crse)%LabHours
                        classTuition = 0.0
                        classLabFee = Subject(crse)%LabFee
                    end if
                else if (Subject(crse)%LectHours>0.0) then ! lecture-only
                    classUnits = Subject(crse)%Units
                    classHours = Subject(crse)%LectHours
                    classTuition = Subject(crse)%Tuition
                    classLabFee = Subject(crse)%LabFee
                else if (Subject(crse)%LabHours>0.0) then ! lab-only
                    classUnits = Subject(crse)%Units
                    classHours = Subject(crse)%LabHours
                    classTuition = Subject(crse)%Tuition
                    classLabFee = Subject(crse)%LabFee
                end if

                totalHours = totalHours + classHours
                totalUnits = totalUnits + classUnits
                totalTuition = totalTuition + classTuition
                totalLabFee = totalLabFee + classLabFee


                previous = sect
                write(device,AFORMAT) &
                    begintr//begintd//trim(Subject(crse)%Name)//endtd, & !  subject
                    begintd//trim(Section(sect)%Code)//endtd ! code

                if (classUnits>0.0) then
                    write(device,AFORMAT) begintd//trim(ftoa(classUnits,1))//endtd
                else
                    write(device,AFORMAT) tdnbspendtd ! units
                end if

                if (classHours>0.0) then
                    write(device,AFORMAT) begintd//trim(ftoa(classHours,2))//endtd ! hours
                else
                    write(device,AFORMAT) tdnbspendtd ! hours
                end if

                if (classTuition>0.0) then
                    write(device,AFORMAT) begintd//trim(ftoa(classTuition,2))//endtd
                else
                    write(device,AFORMAT) tdnbspendtd
                end if

                if (classLabFee>0.0) then
                    write(device,AFORMAT) begintd//trim(ftoa(classLabFee,2))//endtd
                else
                    write(device,AFORMAT) tdnbspendtd
                end if

                !
                !-------------------------------------------------

                if (is_regular_schedule(sect, Section)) then
                    rdx = Section(sect)%RoomIdx(1)
                    EnergyFee = Room(rdx)%EnergyFee
                    write(device,AFORMAT) &
                        begintd//trim(text_time_period(Section(sect)%bTimeIdx(1), Section(sect)%eTimeIdx(1)))//endtd// &
                        begintd//trim(text_days_of_section(Section(sect)))//endtd// &
                        begintd//trim(Room(rdx)%Code)//endtd//endtr
                else
                    mdx = 1
                    rdx = Section(sect)%RoomIdx(mdx)
                    EnergyFee = Room(rdx)%EnergyFee
                    write(device,AFORMAT) &
                        begintd//trim(text_time_period(Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)))//endtd// &
                        begintd//trim(txtDay(Section(sect)%DayIdx(mdx)))//endtd// &
                        begintd//trim(Room(rdx)%Code)//endtd//endtr
                    do mdx=2,Section(sect)%NMeets
                        rdx1 = Section(sect)%RoomIdx(mdx)
                        if (EnergyFee < Room(rdx1)%EnergyFee) then
                            rdx = rdx1
                            EnergyFee = Room(rdx)%EnergyFee
                        end if
                        write(device,AFORMAT) begintr//'<td colspan="6">'//nbsp//endtd// &
                            begintd//trim(text_time_period(Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)))//endtd// &
                            begintd//trim(txtDay(Section(sect)%DayIdx(mdx)))//endtd// &
                            begintd//trim(Room(rdx1)%Code)//endtd//endtr
                    end do
                end if
                totalEnergyFee = totalEnergyFee + EnergyFee
                if (conflict>0) write(device,AFORMAT) &
                    begintr//'<td align="center" colspan="9">'//red//'CONFLICT between '//trim(Section(sect)%ClassId)//' and '// &
                    trim(Section(conflict)%ClassId)//black//endtd//endtr

            end if

        end do
        write(device,AFORMAT) begintr//'<td colspan="9">'//horizontal//endtd//endtr, &
            begintr//tdnbspendtd//begintd//beginbold//'Totals'//endbold//' : '//endtd// & ! code
            begintd//trim(ftoa(totalUnits,1))//endtd//begintd//trim(ftoa(totalHours,2))//endtd// & ! hours
            begintd//beginbold//trim(ftoa(totalTuition,2))//endbold//endtd// &
            begintd//beginbold//trim(ftoa(totalLabFee,2))//endbold//endtd// & ! fees
            tdnbspendtd// tdnbspendtd// tdnbspendtd//endtr, &
            endtable, horizontal, linebreak

        write(device,AFORMAT) beginbold//'An estimate of the total additional fee is '// &
                trim(ftoa(totalEnergyFee+totalFiduciaryFee+totalOtherFee,2))//DOT, &
                ' The Finance office in '// trim(UniversityCodeNoMirror)//' makes the final assessment.'// &
                endbold//linebreak, &
            beginitalic//' The list of fees below may be incomplete, and some fees may be inapplicable. '//enditalic, &
            linebreak, '<table border="0" width="80%">'

        write(device,AFORMAT) '<tr valign="top">'//begintd, '<table border="0" width="100%">'
        if (totalFiduciaryFee>0.0) then
            write(device,AFORMAT) begintr//'<td colspan="2"><br><b>FIDUCIARY fees</b>'//endtd//endtr
            do idx=1,MAX_ALL_FEES
                if (index(FeeDescription(idx), 'FIDUCIARY:')==1 .and. FeeAmount(idx)>0.0) then
                    write(device,AFORMAT) begintr// &
                        begintd//nbsp//nbsp//trim(FeeDescription(idx)(11:))//endtd// &
                        tdalignright//trim(ftoa(FeeAmount(idx),1))//endtd//endtr
                end if
            end do
            write(device,AFORMAT) begintr// &
                begintd//beginbold//'Subtotal'//endbold//endtd// &
                tdalignright//beginbold//trim(ftoa(totalFiduciaryFee,1))//endbold//endtd//endtr
        end if
        write(device,AFORMAT) endtable//endtd//tdnbspendtd//tdnbspendtd//tdnbspendtd//tdnbspendtd//tdnbspendtd//tdnbspendtd

        write(device,AFORMAT) begintd, '<table border="0" width="100%">'
        if (totalEnergyFee>0.0) then
            write(device,AFORMAT) begintr//'<td colspan="2"><br><b>ENERGY fee</b>'//endtd//endtr
            previous = 0
            do idx=1,lenSL,3
                sect=SectionList(idx)
                crse = Section(sect)%SubjectIdx

                !new section ?
                if (sect/=previous) then ! include subject, section, units/blockname, seats/hours, time, day
                    previous = sect
                    if (is_regular_schedule(sect, Section)) then
                        rdx = Section(sect)%RoomIdx(1)
                        EnergyFee = Room(rdx)%EnergyFee
                    else
                        rdx = Section(sect)%RoomIdx(1)
                        EnergyFee = Room(rdx)%EnergyFee
                        do mdx=2,Section(sect)%NMeets
                            rdx1 = Section(sect)%RoomIdx(mdx)
                            if (EnergyFee < Room(rdx1)%EnergyFee) then
                                rdx = rdx1
                                EnergyFee = Room(rdx)%EnergyFee
                            end if
                        end do
                    end if
                    if (EnergyFee>0.0) write(device,AFORMAT) begintr// &
                        begintd//nbsp//nbsp//trim(Subject(crse)%Name)//SPACE//trim(Section(sect)%Code)//' @ '// &
                            trim(Room(rdx)%Code)//endtd// &
                        tdalignright//trim(ftoa(EnergyFee,1))//endtd//endtr
                end if

            end do
            write(device,AFORMAT) begintr// &
                begintd//beginbold//'Subtotal'//endbold//endtd// &
                tdalignright//beginbold//trim(ftoa(totalEnergyFee,1))//endbold//endtd//endtr, &
                begintr//tdnbspendtd//tdnbspendtd//endtr
        end if

        if (totalOtherFee>0.0) then
            write(device,AFORMAT) begintr//'<td colspan="2"><br><b>OTHER fees</b>'//endtd//endtr
            do idx=1,MAX_ALL_FEES
                if (index(FeeDescription(idx), 'OTHER:')==1 .and. FeeAmount(idx)>0.0) then
                    write(device,AFORMAT) begintr// &
                        begintd//nbsp//nbsp//trim(FeeDescription(idx)(7:))//endtd// &
                        tdalignright//trim(ftoa(FeeAmount(idx),1))//endtd//endtr
                end if
            end do
            write(device,AFORMAT) begintr// &
                begintd//beginbold//'Subtotal'//endbold//endtd// &
                tdalignright//beginbold//trim(ftoa(totalOtherFee,1))//endbold//endtd//endtr
        end if
        write(device,AFORMAT) endtable, endtd//endtr//endtable

    end subroutine enlistment_class_schedule


    subroutine printable_class_list (device, Term, NumSections, Section, eList)

        integer, intent (in) :: device, Term, NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in) :: eList(0:)

        integer :: n_count, tdx, std, errNo, ncol, crse, pos, otherSect !, ldx, sect
        character(len=MAX_LEN_CLASS_ID) :: tClassId, otherClass
        character(len=255) :: header

        call html_comment('printable_class_list()')

        ! which section?
        call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, errNo)
        targetSection = index_to_section(tClassId, NumSections, Section)
        crse = Section(targetSection)%SubjectIdx
#if defined REGIST
        targetDepartment = Subject(crse)%DeptIdx
#else
        targetDepartment = Section(targetSection)%DeptIdx
#endif
        targetCollege = Department(targetDepartment)%CollegeIdx

        call collect_students_in_section (targetSection, & ! NumSections,
            Section, eList, n_count, tArray)

        ncol = n_count
        if (is_lecture_lab_subject(crse)) then
            header = trim(ftoa(Subject(crse)%LectHours+Subject(crse)%LabHours,2))//' hrs ('// &
                trim(ftoa(Subject(crse)%LectHours,2))//' lect + '// &
                trim(ftoa(Subject(crse)%LabHours,2))//' lab/recit).'

            if (is_lecture_class(targetSection, Section)) then ! add lecture class and all lab sections
                ! add this lecture class
                do tdx=1,Section(targetSection)%NMeets
                    tArray(ncol+1) = targetSection
                    tArray(ncol+2) = tdx
                    tArray(ncol+3) = 0
                    ncol = ncol+3
                end do
                ! find/add lab sections
                otherClass = trim(tClassId)//DASH
                pos = len_trim(otherClass)
                do otherSect=1,NumSections
                    if (Section(otherSect)%ClassId(:pos)/=otherClass(:pos)) cycle
                    do tdx=1,Section(otherSect)%NMeets
                        tArray(ncol+1) = otherSect
                        tArray(ncol+2) = tdx
                        tArray(ncol+3) = 0
                        ncol = ncol+3
                    end do
                end do

            else  ! find/add lecture class and this lab section
                pos = index(tClassId, DASH)
                otherClass = tClassId(:pos-1)
                otherSect = index_to_section(otherClass, NumSections, Section)
                do tdx=1,Section(otherSect)%NMeets
                    tArray(ncol+1) = otherSect
                    tArray(ncol+2) = tdx
                    tArray(ncol+3) = 0
                    ncol = ncol+3
                end do
                do tdx=1,Section(targetSection)%NMeets
                    tArray(ncol+1) = targetSection
                    tArray(ncol+2) = tdx
                    tArray(ncol+3) = 0
                    ncol = ncol+3
                end do
            end if

        else if (Subject(crse)%LectHours > 0.0) then
            header = trim(ftoa(Subject(crse)%LectHours,2))//' hrs lect.'
            do tdx=1,Section(targetSection)%NMeets
                tArray(ncol+1) = targetSection
                tArray(ncol+2) = tdx
                tArray(ncol+3) = 0
                ncol = ncol+3
            end do
        else if (Subject(crse)%LabHours > 0.0) then
            header = trim(ftoa(Subject(crse)%LabHours,2))//' hrs lab/recit.'
            do tdx=1,Section(targetSection)%NMeets
                tArray(ncol+1) = targetSection
                tArray(ncol+2) = tdx
                tArray(ncol+3) = 0
                ncol = ncol+3
            end do
        end if
        header = trim(Subject(crse)%Title)//'/ '//trim(ftoa(Subject(crse)%Units,1))//' units/ '//header

        tArray(ncol+1) = 0
        tArray(ncol+2) = 0
        tArray(ncol+3) = 0

        write(device,AFORMAT) &
            '<html><head><title>'//trim(UniversityCodeNoMirror)//SPACE//PROGNAME//VERSION// &
            '</title></head><body>', '<table border="0" width="100%">', &
            begintr//tdaligncenter//'Republic of the Philippines'//endtd//endtr, &
            begintr//tdaligncenter//beginbold//trim(UniversityName)//endbold//endtd//endtr, &
            begintr//tdaligncenter//trim(UniversityAddress)//endtd//endtr, &
            begintr//begintd//linebreak//endtd//endtr, &
            begintr//tdaligncenter//trim(College(targetCollege)%Name)//endtd//endtr, &
            begintr//tdaligncenter//beginbold//'Classlist for '//trim(txtSemester(Term+3))// &
                trim(termQualifier(Term+3))//COMMA//SPACE// &
                trim(itoa(currentYear))//DASH//trim(itoa(currentYear+1))//endbold//endtd//endtr, &
            begintr//tdaligncenter//trim(Subject(crse)%Name)//' - '//trim(header)//endtd//endtr, &
            begintr//begintd//linebreak//endtd//endtr, &
            endtable

        call list_sections_to_edit(device, Term, Section, ncol-n_count, tArray(n_count+1:), 0, SPACE, SPACE, .false., SPACE)

        if (n_count == 0) then
            write(device,AFORMAT) BRNONEHR
        else
            write(device,AFORMAT) linebreak//'<table border="0" width="100%">', &
                begintr// &
                thalignleft//'#'//endth// &
                thalignleft//'STDNO'//endth// &
                thalignleft//'NAME OF STUDENT'//endth// &
                thalignleft//'PROGRAM'//endth, &
                thalignleft//'#'//endth// &
                thalignleft//'STDNO'//endth// &
                thalignleft//'NAME OF STUDENT'//endth// &
                thalignleft//'PROGRAM'//endth, &
                endtr

            write(device,AFORMAT) &
                begintr// &
                '<td width="50%" colspan="4">'//horizontal//endtd// &
                '<td width="50%" colspan="4">'//horizontal//endtd// &
                endtr
            pos = (n_count+1)/2
            do tdx=1,pos
                std = tArray(tdx)
                write(device,AFORMAT) begintr// &
                    begintd//trim(itoa(tdx))//DOT//endtd// &
                    begintd//trim(Student(std)%StdNo)//endtd// &
                    begintd//trim(Student(std)%Name)//endtd// &
                    begintd//trim(CurrProgCode(Student(std)%CurriculumIdx))//endtd
                if (tdx+pos<=n_count) then
                    std = tArray(tdx+pos)
                    write(device,AFORMAT) &
                        begintd//trim(itoa(tdx+pos))//DOT//endtd// &
                        begintd//trim(Student(std)%StdNo)//endtd// &
                        begintd//trim(Student(std)%Name)//endtd// &
                        begintd//trim(CurrProgCode(Student(std)%CurriculumIdx))//endtd
                else
                    write(device,AFORMAT) '<td colspan="4">---- '//beginitalic//'Nothing follows'//enditalic//' ----'//endtd//endtr
                end if
            end do
            write(device,AFORMAT) endtable//horizontal
        end if

    end subroutine printable_class_list


    subroutine printable_gradesheet (device, Term, NumSections, Section, eList)

        integer, intent (in) :: device, Term, NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in) :: eList(0:)

        integer :: ldx, gdx, n_count, tdx, std, errNo, sect, ncol, crse, pos, otherSect, step
        character(len=MAX_LEN_CLASS_ID) :: tClassId, otherClass, tGrade
        character(len=255) :: header

        call html_comment('printable_gradesheet()')

        ! which section?
        call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, errNo)
        targetSection = index_to_section(tClassId, NumSections, Section)
        crse = Section(targetSection)%SubjectIdx
#if defined REGIST
        targetDepartment = Subject(crse)%DeptIdx
#else
        targetDepartment = Section(targetSection)%DeptIdx
#endif
        targetCollege = Department(targetDepartment)%CollegeIdx

        ! collect students
        n_count = 0
        do tdx=1,NumStudents+NumAdditionalStudents
            std = StdRank(tdx)
            do ncol=1,eList(std)%lenSubject
                sect = eList(std)%Section(ncol)
                if (sect==0) cycle
                if (targetSection == sect) then
                    tArray(n_count+1) = std
                    tArray(n_count+2) = eList(std)%Grade(ncol)
                    n_count = n_count+2
                    exit
                elseif (eList(std)%Subject(ncol)==crse .and. is_lecture_lab_subject(crse)) then
                    ldx = index(Section(sect)%ClassId,DASH)
                    if (ldx>0) then ! student is accommodated in a lab section
                        if (trim(tClassId)==Section(sect)%ClassId(:ldx-1)) then ! lab of lecture
                            tArray(n_count+1) = std
                            tArray(n_count+2) = eList(std)%Grade(ncol)
                            n_count = n_count+2
                            exit
                        end if
                    end if
                end if
            end do
        end do

        ncol = n_count
        if (is_lecture_lab_subject(crse)) then
            header = trim(ftoa(Subject(crse)%LectHours+Subject(crse)%LabHours,2))//' hrs ('// &
                trim(ftoa(Subject(crse)%LectHours,2))//' lect + '// &
                trim(ftoa(Subject(crse)%LabHours,2))//' lab/recit).'

            if (is_lecture_class(targetSection, Section)) then ! add lecture class and all lab sections
                ! add this lecture class
                do tdx=1,Section(targetSection)%NMeets
                    tArray(ncol+1) = targetSection
                    tArray(ncol+2) = tdx
                    tArray(ncol+3) = 0
                    ncol = ncol+3
                end do
                ! find/add lab sections
                otherClass = trim(tClassId)//DASH
                pos = len_trim(otherClass)
                do otherSect=1,NumSections
                    if (Section(otherSect)%ClassId(:pos)/=otherClass(:pos)) cycle
                    do tdx=1,Section(otherSect)%NMeets
                        tArray(ncol+1) = otherSect
                        tArray(ncol+2) = tdx
                        tArray(ncol+3) = 0
                        ncol = ncol+3
                    end do
                end do

            else  ! find/add lecture class and this lab section
                pos = index(tClassId, DASH)
                otherClass = tClassId(:pos-1)
                otherSect = index_to_section(otherClass, NumSections, Section)
                do tdx=1,Section(otherSect)%NMeets
                    tArray(ncol+1) = otherSect
                    tArray(ncol+2) = tdx
                    tArray(ncol+3) = 0
                    ncol = ncol+3
                end do
                do tdx=1,Section(targetSection)%NMeets
                    tArray(ncol+1) = targetSection
                    tArray(ncol+2) = tdx
                    tArray(ncol+3) = 0
                    ncol = ncol+3
                end do
            end if

        else if (Subject(crse)%LectHours > 0.0) then
            header = trim(ftoa(Subject(crse)%LectHours,2))//' hrs lect.'
            do tdx=1,Section(targetSection)%NMeets
                tArray(ncol+1) = targetSection
                tArray(ncol+2) = tdx
                tArray(ncol+3) = 0
                ncol = ncol+3
            end do
        else if (Subject(crse)%LabHours > 0.0) then
            header = trim(ftoa(Subject(crse)%LabHours,2))//' hrs lab/recit.'
            do tdx=1,Section(targetSection)%NMeets
                tArray(ncol+1) = targetSection
                tArray(ncol+2) = tdx
                tArray(ncol+3) = 0
                ncol = ncol+3
            end do
        end if
        header = trim(Subject(crse)%Title)//'/ '//trim(ftoa(Subject(crse)%Units,1))//' units/ '//header

        tArray(ncol+1) = 0
        tArray(ncol+2) = 0
        tArray(ncol+3) = 0

        write(device,AFORMAT) &
            '<html><head><title>'//trim(UniversityCodeNoMirror)//SPACE//PROGNAME//VERSION// &
            '</title></head><body>', '<table border="0" width="100%">', &
            begintr//tdaligncenter//'Republic of the Philippines'//endtd//endtr, &
            begintr//tdaligncenter//beginbold//trim(UniversityName)//endbold//endtd//endtr, &
            begintr//tdaligncenter//trim(UniversityAddress)//endtd//endtr, &
            begintr//begintd//linebreak//endtd//endtr, &
            begintr//tdaligncenter//trim(College(targetCollege)%Name)//endtd//endtr, &
            begintr//tdaligncenter//beginbold//'Gradesheet for '//trim(txtSemester(Term+3))// &
                trim(termQualifier(Term+3))//COMMA//SPACE// &
                trim(itoa(currentYear))//DASH//trim(itoa(currentYear+1))//endbold//endtd//endtr, &
            begintr//tdaligncenter//trim(Subject(crse)%Name)//' - '//trim(header)//endtd//endtr, &
            begintr//begintd//linebreak//endtd//endtr, &
            endtable

        call list_sections_to_edit(device, Term, Section, ncol-n_count, tArray(n_count+1:), 0, SPACE, SPACE, .false., SPACE)

        if (n_count == 0) then
            write(device,AFORMAT) BRNONE
        else
            write(device,AFORMAT) linebreak//'<table border="0" width="100%">', &
                begintr// &
                thalignleft//'#'//endth// &
                thalignleft//'STDNO'//endth// &
                thalignleft//'GRADE'//endth, &
                thalignleft//'NAME OF STUDENT'//endth// &
                thalignleft//'#'//endth// &
                thalignleft//'STDNO'//endth// &
                thalignleft//'GRADE'//endth, &
                thalignleft//'NAME OF STUDENT'//endth// &
                endtr

            write(device,AFORMAT) &
                begintr// &
                '<td width="50%" colspan="4">'//horizontal//endtd// &
                '<td width="50%" colspan="4">'//horizontal//endtd// &
                endtr
            n_count = n_count/2
            step = (n_count + 1) / 2
            do tdx=1,step
                pos = 2*tdx - 1
                std = tArray(pos)
                gdx = tArray(pos+1)
                if (gdx==gdxREGD) then
                    tGrade = 'No grade'
                else
                    tGrade = txtGrade(pGrade(gdx))
                end if
                write(device,AFORMAT) begintr// &
                    begintd//trim(itoa(tdx))//DOT//endtd// &
                    begintd//trim(Student(std)%StdNo)//endtd// &
                    tdaligncenter//trim(tGrade)//endtd// &
                    begintd//trim(Student(std)%Name)//endtd
                if (step+tdx<=n_count) then ! pos<=n_count) then
                    pos = 2*(step+tdx) - 1
                    std = tArray(pos)
                    gdx = tArray(pos+1)
	                if (gdx==gdxREGD) then
	                    tGrade = 'No grade'
	                else
	                    tGrade = txtGrade(pGrade(gdx))
	                end if
                    write(device,AFORMAT) &
                        begintd//trim(itoa(step+tdx))//DOT//endtd// &
                        begintd//trim(Student(std)%StdNo)//endtd// &
                        tdaligncenter//trim(tGrade)//endtd// &
                        begintd//trim(Student(std)%Name)//endtd
                else
                    write(device,AFORMAT) '<td colspan="4">---- '//beginitalic//'Nothing follows'//enditalic//' ----'//endtd//endtr
                end if
            end do
            write(device,AFORMAT) begintr//'<td width="50%" colspan="8">'//horizontal//endtd//endtr

            ! signatories
            header = Teacher(Section(targetSection)%TeacherIdx(1))%Name
            tdx = index(header, COMMA//SPACE)
            if (tdx>0) then ! switch first & last names
                header = trim(header(tdx+2:))//SPACE//header(:tdx-1)
            end if
            write(device,AFORMAT) &
                begintr// &
                '<td align="center" width="50%" colspan="4">'//linebreak//linebreak//linebreak//trim(header)// &
                    linebreak//'Teacher'//endtd// &
                '<td align="center" width="50%" colspan="4">'//linebreak//linebreak//linebreak// &
                    trim(College(targetCollege)%Dean)// &
                    linebreak//'College Dean, '//trim(College(targetCollege)%Code)//endtd// &
                endtr

            write(device,AFORMAT) endtable
        end if

    end subroutine printable_gradesheet



    subroutine enlistment_not_accommodated(device, thisTerm, eList)
        integer, intent (in) :: device, thisTerm
        type (TYPE_PRE_ENLISTMENT), intent (in) :: eList(0:)

        integer :: n_count, tdx, std, ierr, ncol
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege

        ! which subject ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tSubject, ierr)
        targetSubject = index_to_subject(tSubject)

        ! which college ?
        call cgi_get_named_string(QUERY_STRING, 'A2', tCollege, ierr)
        targetCollege = index_to_college(tCollege)

        call html_write_header (device, trim(tCollege)//' students not accommodated in priority subject '// &
            trim(Subject(targetSubject)%Name)//SPACE//DASH//SPACE//trim(Subject(targetSubject)%Title))

        ! collect students
        n_count = 0
        !tArray = 0
        do tdx=1,NumStudents+NumAdditionalStudents
            std = StdRank(tdx)
            if (targetCollege/=Curriculum(Student(std)%CurriculumIdx)%CollegeIdx) cycle
            do ncol=1,eList(std)%lenSubject
                if (eList(std)%Section(ncol)>0) cycle     ! accommodated
                if (eList(std)%Contrib(ncol)<0.55) cycle     ! not onted in demand
                if (targetSubject/=eList(std)%Subject(ncol)) cycle  ! not this subject
                n_count = n_count+1
                tArray(n_count) = std
            end do
        end do

        call list_students(device, thisTerm, n_count, tArray, targetSubject, eList)
        write(device,AFORMAT) horizontal

    end subroutine enlistment_not_accommodated


    subroutine enlistment_summarize (device, thisTerm, NumSections, Section, Offering, eList, fn)
        integer, intent (in) :: device, thisTerm, fn
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_PRE_ENLISTMENT), intent (in) :: eList(0:)
        type (TYPE_OFFERED_SUBJECTS), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS), intent (in out) :: Offering
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        integer :: ierr, i, j, k, l, std, maxSubjects
        character (len=4) :: tNote

        ! which department ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)
        targetDepartment = index_to_dept(tDepartment)
        targetCollege = Department(targetDepartment)%CollegeIdx
        tCollege = College(targetCollege)%Code

        if (targetDepartment==NumDepartments) then
            k = 0
        else
            k = targetDepartment
        end if
        call offerings_summarize(NumSections, Section, Offering, k)

        ! calculate remaining slots, no. of students accomodated from all colleges
        Section(:)%RemSlots = Section(:)%Slots
        do std = 1,NumStudents+NumAdditionalStudents
            do i=1,eList(std)%NPriority+eList(std)%NAlternates+eList(std)%NCurrent
                j = eList(std)%Subject(i)
                k = eList(std)%Section(i)
                if (k > 0) then ! accommodated or force enlisted
                    if (Section(k)%DeptIdx==targetDepartment .or. targetDepartment==NumDepartments) then ! section belongs to department
                        Offering(j)%Accommodated = Offering(j)%Accommodated + 1
                        Section(k)%RemSlots = Section(k)%RemSlots - 1
                    end if
                end if
            end do
        end do

        ! students in target college not accomodated
        do std = 1,NumStudents+NumAdditionalStudents
            if (targetCollege/=NumColleges) then
                if (targetCollege/=Curriculum(Student(std)%CurriculumIdx)%CollegeIdx) cycle
            end if
            do i=1,eList(std)%NPriority+eList(std)%NAlternates+eList(std)%NCurrent
                j = eList(std)%Subject(i)
                if (i<=eList(std)%NPriority) then ! priority demand
                    Offering(j)%Demand = Offering(j)%Demand + 1
                end if
                k = eList(std)%Section(i)
                if (k==0) then ! not accommodated
                    if (i<=eList(std)%NPriority) then ! priority not accommodated
                        Offering(j)%PriorityNotAccommodated = Offering(j)%PriorityNotAccommodated + 1
                    end if
                end if
            end do
        end do

        ! calculate remaining seats, open sections
        do i=1,NumSections
            if (Section(i)%DeptIdx/=targetDepartment .and. targetDepartment/=NumDepartments) cycle
            j = Section(i)%SubjectIdx
            l = Section(i)%RemSlots
            if (l==0) cycle
            ! lecture-lab ?
            if (.not. is_lecture_lab_subject(j)) then ! lecture only or lab only
                Offering(j)%OpenSlots = Offering(j)%OpenSlots + l
                Offering(j)%OpenSections = Offering(j)%OpenSections + 1
            else ! a lecture-lab subject
                if (is_lecture_class(i, Section)) then ! this is the lecture section
                else ! this is the lab section
                    Offering(j)%OpenSlots = Offering(j)%OpenSlots + l
                    Offering(j)%OpenSections = Offering(j)%OpenSections + 1
                end if
            end if
        end do

        select case (fn)

            case (fnEnlistmentSummary)

                call html_write_header(device, 'Summary of enlistment in '//tCollege)
                do i=1,NumSubjects+NumAdditionalSubjects
                    SubjectRank(i) = i
                end do
                maxSubjects = NumSubjects+NumAdditionalSubjects

            case (fnBottleneck)
                do i=1,NumSubjects+NumAdditionalSubjects
                    SubjectRank(i) = i
                    Offering(i)%SortKey = Offering(i)%PriorityNotAccommodated
                end do
                do i=1,NumSubjects+NumAdditionalSubjects-1
                    do j=i+1,NumSubjects+NumAdditionalSubjects
                        if (Offering(SubjectRank(i))%SortKey <= Offering(SubjectRank(j))%SortKey) then
                            k = SubjectRank(i)
                            SubjectRank(i) = SubjectRank(j)
                            SubjectRank(j) = k
                        end if
                    end do
                end do
                maxSubjects = maxListLength
                call html_write_header(device, 'Top '//trim(itoa(maxListLength))// &
                    ' subjects '//trim(tCollege)//' for which demand > available seats')

            case (fnExtraSlots)
                do i=1,NumSubjects+NumAdditionalSubjects
                    SubjectRank(i) = i
                    Offering(i)%SortKey = Offering(i)%OpenSlots
                end do
                do i=1,NumSubjects+NumAdditionalSubjects-1
                    do j=i+1,NumSubjects+NumAdditionalSubjects
                        if (Offering(SubjectRank(i))%SortKey <= Offering(SubjectRank(j))%SortKey) then
                            k = SubjectRank(i)
                            SubjectRank(i) = SubjectRank(j)
                            SubjectRank(j) = k
                        end if
                    end do
                end do
                maxSubjects = maxListLength
                call html_write_header(device, 'Top '//trim(itoa(maxListLength))// &
                    ' subjects '//trim(tCollege)//' for which available seats > demand')

        end select

        l = 0
        k = 0
        write(device,AFORMAT) '<table border="1" width="87%">'
        do i=1,NumSubjects+NumAdditionalSubjects
            j = SubjectRank(i)
            if (Offering(j)%Demand == 0) cycle
            if (mod(l,20)==0) &
                write(device,AFORMAT) begintr//'<td width="15%">'//beginitalic//'<p>Subject'//linebreak//'</p>'//enditalic//endtd, & ! subject
                    '<td width="8%" align="right">'//beginitalic//'<p>No. of'//linebreak//'sections</p>'//enditalic//endtd, & ! no. of sections
                    '<td width="8%" align="right">'//beginitalic//'<p>Total'//linebreak//'seats</p>'//enditalic//endtd, & ! total seats
                    '<td width="8%" align="right">'//beginitalic//'<p>Total'//linebreak//'accom</p>'//enditalic//endtd, & ! total accom
                    '<td width="8%" align="right">'//beginitalic//'<p>Open'//linebreak//'seats</p>'//enditalic//endtd, & !  open seats
                    '<td width="4%">'//nbsp//endtd// &
                    '<td width="8%" align="right">'//beginitalic//'<p>Priority'//linebreak//'demand</p>'//enditalic//endtd, & ! priority demand
                    '<td width="8%" align="right">'//beginitalic//'<p>Priority'//linebreak//'not acc</p>'//enditalic//endtd//endtr ! priority not accom

            tSubject = Subject(j)%Name

            ! subject
            if (Offering(j)%NSections>0) then
                tNote = SPACE
            else
                tNote = ' (*)'
            end if
            write(device,AFORMAT)  begintr//'<td width="15%">'//trim(tSubject)//tNote//endtd
            ! no. of sections, total seats
            write(device,AFORMAT) tdalignright//trim(itoa(Offering(j)%NSections))//endtd, &
                tdalignright//trim(itoa(Offering(j)%TotalSlots))//endtd
            ! total accom
            write(device,AFORMAT) tdalignright//itoa(Offering(j)%Accommodated)//endtd

            ! open seats
            write(device,AFORMAT) tdalignright//trim(itoa(Offering(j)%OpenSlots))//endtd//tdnbspendtd
            ! priority demand
            write(device,AFORMAT) tdalignright//trim(itoa(Offering(j)%Demand))//endtd
            ! priority not accom
            if (Offering(j)%PriorityNotAccommodated>0) then
                write(device,AFORMAT) trim(make_href(fnNotAccommodated, &
                    itoa(Offering(j)%PriorityNotAccommodated), &
                    A1=tSubject, A2=tCollege, A9=thisTerm, pre=tdalignright, post=endtd//endtr ))
            else
                write(device,AFORMAT) tdalignright//'0'//endtd//endtr
            end if

            l = l+1

            k = k+1
            if (k>=maxSubjects) exit
        end do

        write(device,AFORMAT) endtable, &
            linebreak//'Legends:', &
            linebreak//beginitalic//'No. of sections'//enditalic//' = sections/labs in department', &
            linebreak//beginitalic//'Total seats'//enditalic// &
                ' = Total number of seats from sections in department', &
            linebreak//beginitalic//'Total accom'//enditalic// &
                ' = No. of students (from all colleges) accommodated in the sections', &
            linebreak//beginitalic//'Priority demand'//enditalic// &
                ' = No. of students in college who need the subject as specified in their curriculum, or as a back subject', &
            linebreak//beginitalic//'Priority not accom'//enditalic//' = priority demand not satisfied', &
            linebreak//'(*) = no sections open, or subject is needed by graduating students'
        write(device,AFORMAT) horizontal

    end subroutine enlistment_summarize


    subroutine underload_summarize (device, thisTerm, eList)
        integer, intent (in) :: device, thisTerm
        type (TYPE_PRE_ENLISTMENT), intent (in) :: eList(0:)
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        integer :: ierr, j, k, l, n, std, pos(0:90), iMaxLoad, iuLoad
        real :: feasible_units, enlisted_units, tUnits

        ! which college ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        targetCollege = index_to_college(tCollege)

        MaxLoad = 0.0 ! maximum allowed load
        do std = 1,NumStudents+NumAdditionalStudents
            if (targetCollege/=NumColleges) then
                if (targetCollege/=Curriculum(Student(std)%CurriculumIdx)%CollegeIdx) cycle
            end if
            if (MaxLoad<eList(std)%AllowedLoad) then
                MaxLoad = eList(std)%AllowedLoad
                !call html_comment(Student(std)%StdNo//trim(Student(std)%Name)//' is allowed '//ftoa(MaxLoad,1))
            end if
        end do
        iMaxLoad = min(30.0,MaxLoad)

        call html_write_header(device, 'Summary of underloading/overloading in '//tCollege)

        write(device,AFORMAT) 'Note: Entry at position ('//beginitalic//'row'//enditalic//', '// &
            beginitalic//'column'//enditalic//') indicates the number of students who were allowed '// &
            beginitalic//'column'//enditalic//' units but were underloaded(-)/overloaded(+) by '// &
            beginitalic//'row'//enditalic//' units.'
        write(device,AFORMAT) '<table border="1" width="100%">', begintr, tdnbspendtd

        do j=iMaxLoad,0,-1
            write(device,'(a,i5,a)') tdalignright, j, endtd
        end do
        write(device,AFORMAT) begintd//'Total'//endtd, &
            begintd//'%'//endtd, &
            endtr
        tArray(1:NumStudents+NumAdditionalStudents) = 0
        n = 0
        do std=1,NumStudents+NumAdditionalStudents
            if (eList(std)%NPriority+eList(std)%NAlternates == 0) cycle
            if (targetCollege/=NumColleges) then
                if (targetCollege/=Curriculum(Student(std)%CurriculumIdx)%CollegeIdx) cycle
            end if
            feasible_units = 0
            enlisted_units = 0.0
            do j = 1,eList(std)%NPriority+eList(std)%NAlternates
                tUnits = Subject(eList(std)%Subject(j))%Units
                feasible_units = feasible_units + tUnits
                if (eList(std)%Section(j) > 0) enlisted_units = enlisted_units + tUnits
            end do
            tArray(std) = min(eList(std)%AllowedLoad, feasible_units) - enlisted_units
            n = n+1
        end do
        pos = 0
        do l=iMaxLoad,-6,-1

            k = 0
            pos(0:iMaxLoad) = 0
            do std = 1,NumStudents+NumAdditionalStudents
                if (targetCollege/=NumColleges) then
                    if (targetCollege/=Curriculum(Student(std)%CurriculumIdx)%CollegeIdx) cycle
                end if
                if (eList(std)%NPriority+eList(std)%NAlternates == 0 .or. tArray(std) /= l) cycle
                iuLoad = eList(std)%AllowedLoad
                pos(iuLoad) = pos(iuLoad) + 1
                pos(iMaxLoad+1+iuLoad) = pos(iMaxLoad+1+iuLoad) + 1
                k = k+1
            end do
            write(device,AFORMAT) begintr//tdaligncenter//trim(itoa(-l))//endtd
            do j=iMaxLoad,0,-1
                if (pos(j)>0) then
                    write(device,AFORMAT) trim(make_href(fnUnderloadedStudents, itoa(pos(j)), &
                        A1=tCollege, A2=trim(itoa(j)), A3=trim(itoa(l)), A9=thisTerm, pre=tdalignright, post=endtd))
                else
                    write(device,AFORMAT) tdalignright//DOT//endtd
                end if
            end do
            write(device,AFORMAT) tdalignright//trim(itoa(k))//endtd//tdalignright// &
                trim(itoa(int((100.0*k)/n+0.5)))//endtd//endtr

        end do
        write(device,'(30a)') begintr//begintd//'Total'//endtd, &
            (tdalignright//trim(itoabz(pos(iMaxLoad+1+j)))//endtd, j=iMaxLoad,0,-1),  &
            tdalignright//trim(itoa(n))//endtd//tdnbspendtd//endtr

        write(device,AFORMAT) begintr//begintd//'Load'//endtd
        do j=iMaxLoad,0,-1
            write(device,'(a,i5,a)') tdalignright, j, endtd
        end do
        write(device,AFORMAT) tdnbspendtd, begintd//'%'//endtd, endtr, endtable
        write(device,AFORMAT) 'Note: Entry at position ('//beginitalic//'row'//enditalic//', ', &
            beginitalic//'column'//enditalic//') indicates the number of students who were allowed '// &
            beginitalic//'column'//enditalic//' units but were underloaded(-)/overloaded(+) by '// &
            beginitalic//'row'//enditalic//' units.', horizontal

    end subroutine underload_summarize


    subroutine underloaded_students (device, eList)
        integer, intent (in) :: device
        type (TYPE_PRE_ENLISTMENT), intent (in) :: eList(0:)
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        integer :: ierr, i, j, n, std
        real :: allowed_units, underloaded_by, feasible_units, enlisted_units, tUnits

        ! which college ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        targetCollege = index_to_college(tCollege)

        call cgi_get_named_float(QUERY_STRING, 'A2', allowed_units, ierr)
        call cgi_get_named_float(QUERY_STRING, 'A3', underloaded_by, ierr)

        n = 0
        tArray = 0
        do i=1,NumStudents+NumAdditionalStudents
            std = StdRank(i)
            if (eList(std)%NPriority+eList(std)%NAlternates == 0) cycle
            if (eList(std)%AllowedLoad/=allowed_units) cycle
            if (targetCollege/=NumColleges) then
                if (targetCollege/=Curriculum(Student(std)%CurriculumIdx)%CollegeIdx) cycle
            end if
            feasible_units = 0
            enlisted_units = 0.0
            do j = 1,eList(std)%NPriority+eList(std)%NAlternates
                tUnits = Subject(eList(std)%Subject(j))%Units
                feasible_units = feasible_units + tUnits
                if (eList(std)%Section(j) > 0) enlisted_units = enlisted_units + tUnits
            end do
            if ( (min(eList(std)%AllowedLoad, feasible_units) - enlisted_units) /= underloaded_by ) cycle

            n = n + 1
            tArray(n) = std

        end do

        call html_write_header(device, trim(tCollege)//' students allowed '//ftoa(allowed_units,1)// &
            ' units, but underloaded by '//ftoa(underloaded_by,1))
        call html_student_list (device, n, tArray, .true.)
        write(device,AFORMAT) horizontal

    end subroutine underloaded_students



    subroutine  enlistment_no_classes(device, eList)
        integer, intent (in) :: device
        type (TYPE_PRE_ENLISTMENT), intent (in) :: eList(0:)
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege, tAction, tAge
        character(len=3) :: StdNumYear
        integer :: ierr, i, k, m, n, std
        integer :: ldx, tdx, fac, skip, first, last
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character :: ch
        logical :: hideStudents, findOlder, hideAll
        integer, parameter :: toSkip = 50

        ! which college ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        targetCollege = index_to_college(tCollege)
        StdNumYear = trim(itoa(mod(currentYear,1000)))//DASH

        ! which students ?
        call cgi_get_named_string(QUERY_STRING, 'A2', tAge, ierr)
        findOlder = trim(tAge)=='Older'

        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
        hideStudents = trim(tAction)=='Hide'
        hideAll = trim(tAction)=='Hide ALL'

        ch = SPACE
        if (hideStudents .or. hideAll) then ! get letter
            call cgi_get_named_string(QUERY_STRING, 'A3', tAction, ierr)
            ch = tAction(1:1)
        end if

        n = 0
        tArray = 0
        do i=1,NumStudents+NumAdditionalStudents
            std = StdRank(i)
            if (targetCollege/=NumColleges) then
                if (targetCollege/=Curriculum(Student(std)%CurriculumIdx)%CollegeIdx) cycle
            end if
            m = sum(eList(std)%Section(:))
            if (m>0) cycle ! has at least one class
            if (findOlder) then ! looking for older students
                if (Student(std)%StdNo(1:3)==StdNumYear) cycle ! student is new; skip
            else ! looking for new students
                if (Student(std)%StdNo(1:3)/=StdNumYear) cycle ! student is not new
            end if

            if (hideStudents .and. Student(std)%Name(1:1)==ch) then
                call cgi_get_named_string(QUERY_STRING, trim(Student(std)%StdNo), tAction, ierr)
                if (ierr==0) Student(std)%Classification = NotOfficiallyEnrolled ! student was checked
            end if

            if (hideAll .and. Student(std)%Name(1:1)==ch) then
                Student(std)%Classification = NotOfficiallyEnrolled ! student has selected letter as start of name
            end if

            if (Student(std)%Classification>=NotOfficiallyEnrolled) cycle ! already hidden

            n = n + 1
            tArray(n) = std

        end do

        call html_write_header(device, SPACE, SPACE)

        if (n == 0) then
            write(device,AFORMAT) '<h3>'//trim(tAge)//' students of '//trim(tCollege)// &
                ' not enrolled in any class</h3>', BRNONE, horizontal
        else

            do i=iachar('A'), iachar('Z')

                ! how many for this letter?
                ch = achar(i)
                m = 0
                do tdx=1,n
                    std = tArray(tdx)
                    if (Student(std)%Name(1:1)==ch) then ! add to end of list
                        m = m+1
                        tArray(n+m) = std
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

                    call make_form_start(device, fnStudentsNotEnrolled, A1=tCollege, A2=tAge, A3=ch)

                    write(device,AFORMAT) linebreak, '<h3>'//trim(tAge)//' "'//ch//'" students of '//trim(tCollege)// &
                        ' not enrolled in any class</h3>', &
                        '<table border="0" width="75%">', &
                        begintr//thalignleft//'Hide'//endth//thalignleft//'STDNO'//endth, &
                        thalignleft//'NAME OF STUDENT'//endth//thalignleft//'PROGRAM'//endth//endtr

                    do tdx=first,last

                        std = tArray(n+tdx)
                        if (Student(std)%Name(1:1)/=ch) cycle

                        tStdNo = Student(std)%StdNo
                        fac = index_to_teacher(Student(std)%Adviser)
                        ldx = Student(std)%CurriculumIdx

                        write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(tdx,2))//'">'//begintd// &
                            '<input type="checkbox" name="'//trim(tStdNo)//'" checked="yes">', &
                            endtd//begintd//tStdNo//endtd, &
                            begintd//trim(Student(std)%Name)//endtd, &
                            begintd//trim(Curriculum(ldx)%Code)//endtd

                        if ( (is_adviser_of_student(std,orHigherUp) .or. is_benefactor_of_student(std)) .and. &
                            .not. isRectify ) then

                            write(device,AFORMAT) begintd//beginsmall
                            if (.not. isRoleBenefactor) then
                                write(device,AFORMAT) &
                                    trim(make_href(fnStudentEdit, 'Info', A1=tStdNo, pre=nbsp)), &
                                    trim(make_href(fnRecentStudentActivity, 'Log', A1=tStdNo, pre=nbsp)), &
                                    trim(make_href(fnTeacherClasses, 'Adviser', A1=Teacher(fac)%TeacherId))
                            end if
                            write(device,AFORMAT) &
                                trim(make_href(fnStudentGrades, 'Grades', A1=tStdNo, pre=nbsp)), &
                                trim(make_href(fnEditCheckList, 'Checklist', A1=tStdNo, pre=nbsp))

                            write(device,AFORMAT) endsmall//endtd
                        end if
                        write(device,AFORMAT) endtr
                    end do
                    write(device,AFORMAT) &
                        endtable//linebreak//'<input type="submit" name="action" value="Hide"> ', &
                        ' the selected '//trim(tAge)//' "'//ch//'" '//' students immediately above, or '//nbsp//nbsp// &
                        '<input type="submit" name="action" value="Hide ALL"> '//trim(tAge)//' "'//ch//'" '// &
                        ' students above and/or below.', &
                        endform

                end do
                write(device,AFORMAT) horizontal

            end do

        end if

    end subroutine  enlistment_no_classes


    subroutine confirm_preenlist_or_delist(device, thisTerm)
        integer, intent (in) :: device, thisTerm
        integer :: ierr, ldx

        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character(len=2*MAX_LEN_CURRICULUM_CODE) :: search_string, tAction

        call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)
        targetDepartment = index_to_dept(tDepartment)
        targetCollege = Department(targetDepartment)%CollegeIdx
        tCollege = College(targetCollege)%Code

        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)

        call html_comment('confirm_preenlist_or_delist('//tAction//tCollege//')')

        call html_write_header(device, tAction//tCollege//' students')

        call make_form_start(device, fnTimetabling, A1=tCollege, A9=thisTerm)
        write(device,AFORMAT) '<input type="hidden" name="action" value="'//trim(tAction)//'">'

        select case(trim(tAction))

            case ('Preenlist ALL', 'Delist ALL')

            case ('Preenlist SELECTED', 'Delist SELECTED')

                do ldx=1,NumCurricula ! specific curriculum
                    search_string = 'CHECKED:'//Curriculum(ldx)%Code
                    call cgi_get_named_string(QUERY_STRING, trim(search_string), tCurriculum, ierr)
                    if (ierr==-1) cycle ! not found
                    if (len_trim(tCurriculum)==0) cycle
                    write(device,AFORMAT) '<input type="hidden" name="'//trim(search_string)//'" value="on">'
                end do

        end select

        write(device,AFORMAT) &
            'This operation will modify unlocked class schedules of students in '//tCollege// &
            ' for the specified term. Are you sure?', linebreak, linebreak, &
            nbsp//nbsp//nbsp//nbsp//'<input type="submit" name="confirm" value="Cancel">', &
            nbsp//nbsp//nbsp//nbsp//'<input type="submit" name="confirm" value="Continue">', &
            red//' - click once only, may take a while; use browser''s "Back" button in case of timeout'// &
            black//endform, &
            horizontal

    end subroutine confirm_preenlist_or_delist


    subroutine preenlist_or_delist(device, thisTerm, NumSections, Section, & ! Offering, &
        NumBlocks, Block, Enlistment, inclStudent, mesg )

        integer, intent (in) :: device, thisTerm, NumSections, NumBlocks
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        ! type (TYPE_OFFERED_SUBJECTS), intent(in out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in out) :: Enlistment(0:)
        integer, intent (in out) :: inclStudent(0:)
        character(len=*), intent (out) :: mesg

        integer :: std, ldx, ierr
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character(len=2*MAX_LEN_CURRICULUM_CODE) :: search_string, tAction, tConfirm

        call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)
        targetDepartment = index_to_dept(tDepartment)
        targetCollege = Department(targetDepartment)%CollegeIdx
        tCollege = College(targetCollege)%Code

        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)

        call cgi_get_named_string(QUERY_STRING, 'confirm', tConfirm, ierr)

        call html_comment( tConfirm//tAction//tCollege )

        mesg = tCollege//'- '//College(targetCollege)%Name//trim(tAction)//' - operation canceled.'
        if (trim(tConfirm)/='Continue') then
            return
        end if

        if (isRoleOfficial) then
            mesg = trim(mesg)//SPACE//sorryMessageOfficial
            return
        end if

        mesg = SPACE
        call html_write_header(device, tCollege//'- '//College(targetCollege)%Name)

        ! make backup
        call xml_backup(trim(pathToYear)//'BACKUP.XML'//DASH//currentDate//DASH//currentTime(:6))

        write(device,AFORMAT) '<pre>', trim(tAction)//SPACE//trim(tCollege)//' students for '// &
            txtSemester(thisTerm+3)//termQualifier(thisTerm+3)

        ! mark students to be includes
        inclStudent = 0
        select case(trim(tAction))

            case ('Preenlist ALL', 'Delist ALL')

                if (targetCollege==NumColleges) then ! all students
                    do std = 1,NumStudents+NumAdditionalStudents
                        if (Enlistment(std)%Status==SCHEDULE_IS_LOCKED) cycle ! locked
                        inclStudent(std) = 1
                    end do
                else
                    do std = 1,NumStudents+NumAdditionalStudents
                        if (Enlistment(std)%Status==SCHEDULE_IS_LOCKED) cycle ! locked
                        if (Curriculum(Student(std)%CurriculumIdx)%CollegeIdx/=targetCollege) cycle
                        inclStudent(std) = 1
                    end do
                end if

            case ('Preenlist SELECTED', 'Delist SELECTED')

                done = .true.
                do ldx=1,NumCurricula ! specific curriculum
                    if (targetCollege/=Curriculum(ldx)%CollegeIdx) cycle
                    search_string = 'CHECKED:'//Curriculum(ldx)%Code
                    call cgi_get_named_string(QUERY_STRING, trim(search_string), tCurriculum, ierr)
                    if (ierr==-1) cycle ! not found
                    done(ldx) = .false.
                    write(device,AFORMAT) trim(search_string)//' - '//tCurriculum
                end do
                do std = 1,NumStudents+NumAdditionalStudents
                    if (Enlistment(std)%Status==SCHEDULE_IS_LOCKED) cycle ! locked
                    ! skip student not in selected curricula
                    if ( done(Student(std)%CurriculumIdx) ) cycle
                    inclStudent(std) = 1
                end do

        end select
        write(device,*) '# Students selected = '//itoa(sum(inclStudent))

        ! perform action
        select case(trim(tAction))

            case ('Delist ALL', 'Delist SELECTED')

                do std = 1,NumStudents+NumAdditionalStudents
                    if (inclStudent(std)==0) cycle
                    Enlistment(std)%Section(:) = 0
                    Enlistment(std)%Grade(:) = 0
                    Enlistment(std)%Status = SUBJECTS_ARE_ADVISED
                end do

            case ('Preenlist ALL', 'Preenlist SELECTED')
!                call generate_timetables(device, thisTerm, NumSections, Section, &
!                    wrkOffering, Enlistment, inclStudent )
                call add_to_blocks(device, NumSections, Section, &
                    NumBlocks, Block, Enlistment, inclStudent )

        end select

        write(device,AFORMAT) 'Done!', '</pre>', horizontal

    end subroutine preenlist_or_delist


    subroutine add_to_blocks(device, NumSections, Section, NumBlocks, Block, Enlistment, inclStudent )

        integer, intent (in) :: device, NumSections, NumBlocks
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in out) :: Enlistment(0:)
        integer, intent (in out) :: inclStudent(0:)
        type (TYPE_PRE_ENLISTMENT) :: wrk

        ! match blocks to students of the same curriculum; all subjects in block must be advised subjects
        ! of a student
        integer :: std, blk, crse, sect, n_matches, bdx, fdx, n_assigned, in_block
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubjectA, tSubjectB
        logical :: matched

        ! count remaining slots per section
        Section(1:NumSections)%RemSlots = Section(1:NumSections)%Slots
        do std=1,NumStudents+NumAdditionalStudents
            do fdx=1,Enlistment(std)%lenSubject
                sect = Enlistment(std)%Section(fdx)
                if (sect > 0) then
                    if (Section(sect)%Slots > 0) Section(sect)%RemSlots = Section(sect)%RemSlots-1
                end if
            end do
        end do

        n_assigned = 0
        do std=1,NumStudents+NumAdditionalStudents
            if (inclStudent(std)==0) cycle ! student not selected
            if ( sum(Enlistment(std)%Section(:))>0 ) then
                write(device,*) 'Previously enlisted: '//trim(text_student_curriculum(std))
                cycle ! student already enlisted in some sections
            end if
            wrk = Enlistment(std)
            do blk=1,NumBlocks
                if (Block(blk)%CurriculumIdx/=Student(std)%CurriculumIdx) cycle ! curriculum mismatch
                in_block = 0 ! how many subjects in block matched
                n_matches = 0 ! how many subjects in block matched by advised subjects
                do bdx=1,Block(blk)%NumClasses
                    crse = Block(blk)%Subject(bdx)
                    sect = Block(blk)%Section(bdx)
                    if (sect==0) cycle
                    in_block = in_block+1
                    tSubjectB = Subject(crse)%Name
                    matched = .false.
                    do fdx=1,Enlistment(std)%lenSubject
                        if (crse==Enlistment(std)%Subject(fdx) .and. Section(sect)%RemSlots>0) then
                            wrk%Section(fdx) = sect
                            wrk%Grade(fdx) = gdxREGD
                            matched = .true.
                            exit ! subject matching
                        end if
                    end do
                    if (matched) then
                        n_matches = n_matches+1
                        cycle
                    else
                        ! try PE
                        matched = .false.
                        if (tSubjectB(1:3)=='PE ') then
                            do fdx=1,Enlistment(std)%lenSubject
                                tSubjectA = Subject(Enlistment(std)%Subject(fdx))%Name
                                if (tSubjectA(1:3)==tSubjectB(1:3) .and. Section(sect)%RemSlots>0) then
                                    wrk%Subject(fdx) = crse
                                    wrk%Section(fdx) = sect
                                    wrk%Grade(fdx) = gdxREGD
                                    matched = .true.
                                    exit ! subject matching
                                end if
                            end do
                            if (matched) then
                                n_matches = n_matches+1
                                cycle
                            end if
                        end if
                        ! try NSTP
                        matched = .false.
                        if (tSubjectB(1:5)=='NSTP ') then
                            do fdx=1,Enlistment(std)%lenSubject
                                tSubjectA = Subject(Enlistment(std)%Subject(fdx))%Name
                                if (tSubjectA(1:5)==tSubjectB(1:5) .and. Section(sect)%RemSlots>0) then
                                    wrk%Subject(fdx) = crse
                                    wrk%Section(fdx) = sect
                                    wrk%Grade(fdx) = gdxREGD
                                    matched = .true.
                                    exit ! subject matching
                                end if
                            end do
                            if (matched) then
                                n_matches = n_matches+1
                                cycle
                            end if
                        end if
                    end if
                end do
                !write(device,*) Block(blk)%BlockID//': matched subjects= ', n_matches
                if (n_matches/=in_block) cycle
                ! all subjects in block matched, and seats are available in all classes
                do bdx=1,Block(blk)%NumClasses
                    crse = Block(blk)%Subject(bdx)
                    sect = Block(blk)%Section(bdx)
                    do fdx=1,wrk%lenSubject
                        if (crse/=wrk%Subject(fdx)) cycle
                        Section(sect)%RemSlots = Section(sect)%RemSlots-1
                        exit ! subject matching
                    end do
                end do
                Enlistment(std) = wrk
                Enlistment(std)%Status = SECTIONS_ARE_SELECTED
                n_assigned = n_assigned+1
                write(device,*) n_assigned, trim(text_student_curriculum(std))//' assigned to '//Block(blk)%BlockID
                exit ! block search
            end do

        end do
        write(device,*) 'Blocks not found for '//itoa(sum(inclStudent)-n_assigned)

    end subroutine add_to_blocks


end module EditENLISTMENT
