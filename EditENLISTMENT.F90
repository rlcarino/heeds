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
#if defined REGIST
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
            mesg = '"'//trim(tAction)//'" failed. '//sorryMessage
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
        integer, dimension(60,6) :: TimeTable
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
            statusMesg = ' : "'//trim(tAction)//'" failed. '//sorryMessage
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
        integer :: n_changes
        integer, dimension(60,6) :: TimeTable
        logical :: conflicted, matched
        character(len=255) :: mesg
        logical :: isDirtyFORM5, allowed_to_edit

        type (TYPE_PRE_ENLISTMENT) :: Advice

        ! which student?
        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
        targetStudent = index_to_student(tStdNo)
        targetCurriculum = Student(targetStudent)%CurriculumIdx
        idx_opt = Curriculum(targetCurriculum)%CollegeIdx
        allowed_to_edit = is_admin_of_college(idx_opt)  .or. (isPeriodOne .and. &
            ( is_adviser_of_student(targetStudent, orHigherUp) .or. &
              ( trim(USERNAME)==tStdNo .and. thisTerm==currentTerm .and. &
                College(idx_opt)%AllowEnlistmentByStudents(thisTerm) ) ) )

        call recalculate_available_seats(Section,eList)
        isDirtyFORM5 = .false.

        ! check for other arguments
        mesg = SPACE
        call cgi_get_named_string(QUERY_STRING, 'A2', tAction, ierr)

        !select case (trim(tAction))

        if (trim(tAction)=='Add' .and. .not. isRoleOfficial) then

                call cgi_get_named_string(QUERY_STRING, 'A3', tClassId, ierr)
                sect = index_to_section(tClassId, NumSections, Section)
                if (sect>0) then ! target of action is indexed by sect
                    if (Section(sect)%RemSlots>0) then
                        crse = Section(sect)%SubjectIdx
                        do fdx=1,eList(targetStudent)%lenSubject
                            if (eList(targetStudent)%Subject(fdx)==crse) then
                                eList(targetStudent)%Section(fdx) = sect
                                eList(targetStudent)%Grade(fdx) = gdxREGD
                                Section(sect)%RemSlots = Section(sect)%RemSlots - 1
                                exit
                            end if
                        end do
                        mesg = 'Added '//tClassId
                        isDirtyFORM5 = .true.
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
                if (sect>0) then ! target of action is indexed by sect
                    do fdx=1,eList(targetStudent)%lenSubject
                        if (sect==eList(targetStudent)%Section(fdx)) then
                            eList(targetStudent)%Section(fdx) = 0
                            eList(targetStudent)%Grade(fdx) = 0
                            Section(sect)%RemSlots = Section(sect)%RemSlots + 1
                            exit
                        end if
                    end do
                    mesg = 'Deleted '//tClassId
                    isDirtyFORM5 = .true.
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
                eList(targetStudent)%Status = eList(targetStudent)%Status + 2;
                mesg = 'Locked the schedule; unlock it to allow changes.'
                isDirtyFORM5 = .true.

        end if

        if (trim(tAction)=='UNLOCK Schedule' .and. .not. isRoleOfficial) then

                eList(targetStudent)%Status = eList(targetStudent)%Status - 2;
                mesg = 'Unlocked the schedule; lock it to disallow changes.'
                isDirtyFORM5 = .true.

        end if

        if (trim(tAction)=='DELIST Student' .and. .not. isRoleOfficial) then

                call initialize_pre_enlistment(eList(targetStudent))
                mesg = 'Delisted student from classes in '//txtSemester(thisTerm+3)//termQualifier(thisTerm+3)
                call log_student_record_change(targetStudent, 'Not enrolled: Removed subjects for '// &
                    trim(txtSemester(thisTerm+3)//termQualifier(thisTerm+3)) )
                isDirtyFORM5 = .true.

        end if

        if (len_trim(tAction)>0 .and. isRoleOfficial) then
            mesg = '"'//trim(tAction)//'" failed. '//sorryMessage
        end if

        if (isDirtyFORM5) then
            call xml_write_pre_enlistment(pathToTerm, 'ENLISTMENT', eList, Section, &
                Student(targetStudent)%CurriculumIdx)
            call log_student_record_change(targetStudent, mesg )
        end if

        !if (trim(tAction)=='DELIST Student' .and. .not. isRoleOfficial) then
        !    call html_college_info(device, Curriculum(Student(targetStudent)%CurriculumIdx)%CollegeIdx)
        !    return
        !end if

        call html_write_header(device, trim(Student(targetStudent)%StdNo)//SPACE//trim(Student(targetStudent)%Name)//linebreak// &
            trim(text_curriculum_info(Student(targetStudent)%CurriculumIdx))//linebreak//linebreak//'Schedule of Classes ', mesg)

        ! collect classes for student
        call timetable_meetings_of_student(NumSections, Section, targetStudent, eList, 0, tLen1, tArray, TimeTable, &
            conflicted)
        !!
        !call enlistment_fees(device, targetStudent, NumSections, Section, linebreak//beginbold//'Estimated fees'//endbold)
        !write(device,AFORMAT) horizontal

        allowed_to_edit = allowed_to_edit .and. eList(targetStudent)%Status<2

        call list_sections_to_edit(device, thisTerm, Section, tLen1, tArray, fnChangeMatriculation, tStdNo, 'Del', &
            allowed_to_edit, beginbold//'Enlisted subjects'//endbold//nbsp//trim(make_href(fnPrintableSchedule, 'Printable', &
            A1=tStdNo, A9=thisTerm, pre=beginsmall//'(', post=')'//endsmall)) )

        if (tLen1>0 .and. allowed_to_edit) then

            ! lock or unlock
            call make_form_start(device, fnChangeMatriculation, A1=tStdNo, A9=thisTerm)
            if (eList(targetStudent)%Status<2) then ! enable Lock CLASSES
                write(device,AFORMAT) '<input type="submit" name="A2" value="LOCK Schedule">'
            else
                write(device,AFORMAT) '<input type="submit" name="A2" value="UNLOCK Schedule">'
            end if

            ! student not enrolling
            write(device,AFORMAT) nbsp//nbsp//nbsp//nbsp, &
                '<input type="submit" name="A2" value="DELIST Student"> (not enrolling)'//endform

            call timetable_display(device, Section, TimeTable)
        end if

        ! make list of available sections for alternate subjects that fit the schedule of student
        tLen2 = 0
        do fdx=1,eList(targetStudent)%lenSubject
            if (eList(targetStudent)%Section(fdx)/=0) cycle ! already enlisted
            tLen2 = tLen2 + 1
        end do

        if ( tLen2>0 .and. allowed_to_edit) then
            mdx = 0
            write(device,AFORMAT) linebreak//beginbold//'Other feasible subjects'//endbold//':'
            do fdx=1,eList(targetStudent)%lenSubject
                if (eList(targetStudent)%Section(fdx)==0) then
                    mdx = mdx+1
                    tSubject = Subject(eList(targetStudent)%Subject(fdx))%Name
                    write(device,AFORMAT) linebreak//trim(itoa(mdx))//'). <a href="#'// &
                    trim(tSubject)//'">'//trim(tSubject)//'</a> - '// &
                    trim(Subject(eList(targetStudent)%Subject(fdx))%Title)
                end if
            end do
            write(device,AFORMAT) linebreak

            do fdx=1,eList(targetStudent)%lenSubject
                if (eList(targetStudent)%Section(fdx)/=0) cycle ! already enlisted
                crse = eList(targetStudent)%Subject(fdx) ! index to subject
                tSubject = Subject(crse)%Name
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
                        green//beginbold//'AVAILABLE sections'//black//' in '//trim(tSubject)// &
                        ' that fit existing schedule, sorted by undesirability.'//endbold)

                end if

            end do !  fdx=1,eList(targetStudent)%lenSubject

        end if
        write(device,AFORMAT) horizontal

    end subroutine enlistment_edit


!    subroutine enlistment_fees(device, std, NumSections, Section, heading)
!        integer, intent(in) :: device, std
!        integer, intent (in) :: NumSections
!        type (TYPE_SECTION), intent(in) :: Section(0:)
!        character (len=*), intent(in), optional :: heading
!        integer :: idx, sect, crse
!        real :: mult, totalUnits, totalA, totalB, totalC, totalD, totalE, totalGraduate, totalLabFee
!
!        ! Tuition fees by bracket
!        real, parameter :: &
!        TuitionA              = 1500.0, &
!        TuitionB              = 1000.0, &
!        TuitionC              =  600.0, &
!        TuitionD              =  300.0, &
!        TuitionE              =    0.0, &
!        TuitionGraduate       = 1000.0, &
!        NSTPA                 = 2250.0, &
!        NSTPB                 = 1500.0, &
!        NSTPC                 =  900.0, &
!        NSTPD                 =  450.0, &
!        NSTPE                 =    0.0, &
!        MiscellaneousA        = 2000.0, &
!        MiscellaneousB        = 2000.0, &
!        MiscellaneousC        = 2000.0, &
!        MiscellaneousD        =    0.0, &
!        MiscellaneousE        =    0.0, &
!        MiscellaneousGraduate = 1065.0, &
!        StudentFund           =   45.5, &
!        EntranceFee           =   30.0, &
!        DepositFee            =  100.0, &
!        IDFee                 =  130.0, &
!        EducationDevelopment  =    0.0
!
!        if (present(heading)) write(device,AFORMAT) heading
!
!        write(device,AFORMAT) '<table border="0" width="100%">'//begintr, &
!        thalignleft//'Subject'//endth// &
!        thalignleft//'Section'//endth// &
!        thalignright//'Units'//endth//&
!        thalignright//'Bracket A'//endth//&
!        thalignright//'Bracket B'//endth// &
!        thalignright//'Bracket C'//endth//&
!        thalignright//'Bracket D'//endth// &
!        thalignright//'Bracket E'//endth// &
!        thalignright//'Graduate'//endth// &
!        thalignright//'Lab/Other'//endth//endtr
!
!        totalUnits = 0.0
!        totalA = 0.0
!        totalB = 0.0
!        totalC = 0.0
!        totalD = 0.0
!        totalE = 0.0
!        totalGraduate = 0.0
!        totalLabFee = 0.0
!        do idx=1,Preenlisted(std)%lenSubject ! loop over all entries in Preenlisted() for student
!            sect = Preenlisted(std)%Section(idx)
!            if (sect==0) cycle ! not enlisted
!            crse = Preenlisted(std)%Subject(idx)
!            mult = Subject(crse)%Units
!            totalUnits = totalUnits + mult
!            write(device,AFORMAT) begintr//begintd//trim(Subject(crse)%Name)//endtd// & !  subject
!            begintd//trim(Section(sect)%Code)//endtd ! code
!
!            ! NSTP ?
!            if (index(Subject(crse)%Name, 'CWTS') + index(Subject(crse)%Name, 'LTS')>0) then
!                write(device,'(a,f9.1,a)') &
!                tdnbspendtd// & ! units
!                tdalignright, NSTPA, endtd, & ! NSTP A
!                tdalignright, NSTPB, endtd, & ! NSTP B
!                tdalignright, NSTPC, endtd, & ! NSTP C
!                tdalignright, NSTPD, endtd, & ! NSTP D
!                tdalignright, NSTPE, endtd, & ! NSTP E
!                tdnbspendtd ! NSTP Graduate
!                totalA = totalA + NSTPA
!                totalB = totalB + NSTPB
!                totalC = totalC + NSTPC
!                totalD = totalD + NSTPD
!                totalE = totalE + NSTPE
!            else ! lecture class of a lect-lab/recit subject, lecture-only, lab-only
!                ! compute tuition here
!                write(device,'(a,f9.1,a)') &
!                tdalignright, mult, endtd, & ! units
!                tdalignright, TuitionA*mult, endtd, & ! Tuition A
!                tdalignright, TuitionB*mult, endtd, & ! Tuition B
!                tdalignright, TuitionC*mult, endtd, & ! Tuition C
!                tdalignright, TuitionD*mult, endtd, & ! Tuition D
!                tdalignright, TuitionE*mult, endtd, & ! Tuition E
!                tdalignright, TuitionGraduate*mult, endtd ! Tuition Graduate
!                totalA = totalA + TuitionA*mult
!                totalB = totalB + TuitionB*mult
!                totalC = totalC + TuitionC*mult
!                totalD = totalD + TuitionD*mult
!                totalE = totalE + TuitionE*mult
!                totalGraduate = totalGraduate + TuitionGraduate*mult
!            end if
!            if (Subject(crse)%LabFee>0.0) then ! subject has additional fee
!                write(device,'(a,f9.1,a)') &
!                tdalignright, Subject(crse)%LabFee, endtd//endtr
!                totalLabFee = totalLabFee + Subject(crse)%LabFee
!            else
!                write(device,AFORMAT) tdnbspendtd//endtr
!            end if
!        end do
!
!        ! sub totals
!        write(device,AFORMAT) &
!        begintr//'<td colspan="10">'//horizontal//endtd//endtr, &
!        begintr//'<td colspan="2" align="right">Tuition Subtotal'//endtd
!        write(device,'(a,f9.1,a)') &
!        tdalignright, totalUnits, endtd, & ! units
!        tdalignright, totalA, endtd, & ! Total A
!        tdalignright, totalB, endtd, & ! Total B
!        tdalignright, totalC, endtd, & ! Total C
!        tdalignright, totalD, endtd, & ! Total D
!        tdalignright, totalE, endtd, & ! Total E
!        tdalignright, totalGraduate, endtd, & ! Total Graduate
!        tdalignright, totalLabFee, endtd//endtr, & ! Total Lab
!        begintr//'<td colspan="10">'//horizontal//endtd//endtr
!
!        ! miscellaneous
!        write(device,AFORMAT) begintr//'<td colspan="2" align="right">Miscellaneous'//endtd//tdnbspendtd
!        write(device,'(a,f9.1,a)') &
!        tdalignright, MiscellaneousA, endtd, & ! Misc A
!        tdalignright, MiscellaneousB, endtd, & ! Misc B
!        tdalignright, MiscellaneousC, endtd, & ! Misc C
!        tdalignright, MiscellaneousD, endtd, & ! Misc D
!        tdalignright, MiscellaneousE, endtd, & ! Misc E
!        tdalignright, MiscellaneousGraduate, endtd, & ! Misc Graduate
!        tdnbspendtd//endtr ! Lab
!        totalA = totalA + MiscellaneousA
!        totalB = totalB + MiscellaneousB
!        totalC = totalC + MiscellaneousC
!        totalD = totalD + MiscellaneousD
!        totalE = totalE + MiscellaneousE
!        totalGraduate = totalGraduate + MiscellaneousGraduate
!        ! StudentFund
!        write(device,AFORMAT) begintr//'<td colspan="2" align="right">Student Fund'//endtd//tdnbspendtd
!        write(device,'(a,f9.1,a)') &
!        tdalignright, StudentFund, endtd, & ! A
!        tdalignright, StudentFund, endtd, & ! B
!        tdalignright, StudentFund, endtd, & ! C
!        tdalignright, StudentFund, endtd, & ! D
!        tdalignright, StudentFund, endtd, & ! E
!        tdalignright, StudentFund, endtd, & ! Graduate
!        tdnbspendtd//endtr ! Lab
!        totalA = totalA + StudentFund
!        totalB = totalB + StudentFund
!        totalC = totalC + StudentFund
!        totalD = totalD + StudentFund
!        totalE = totalE + StudentFund
!        totalGraduate = totalGraduate + StudentFund
!        ! EntranceFee
!        write(device,AFORMAT) begintr//'<td colspan="2" align="right">Entrance Fee'//endtd//tdnbspendtd
!        write(device,'(a,f9.1,a)') &
!        tdalignright, EntranceFee, endtd, & ! A
!        tdalignright, EntranceFee, endtd, & ! B
!        tdalignright, EntranceFee, endtd, & ! C
!        tdalignright, EntranceFee, endtd, & ! D
!        tdalignright, EntranceFee, endtd, & ! E
!        tdalignright, EntranceFee, endtd, & ! Graduate
!        tdnbspendtd//endtr ! Lab
!        totalA = totalA + EntranceFee
!        totalB = totalB + EntranceFee
!        totalC = totalC + EntranceFee
!        totalD = totalD + EntranceFee
!        totalE = totalE + EntranceFee
!        totalGraduate = totalGraduate + EntranceFee
!        ! DepositFee
!        write(device,AFORMAT) begintr//'<td colspan="2" align="right">Deposit'//endtd//tdnbspendtd
!        write(device,'(a,f9.1,a)') &
!        tdalignright, DepositFee, endtd, & ! A
!        tdalignright, DepositFee, endtd, & ! B
!        tdalignright, DepositFee, endtd, & ! C
!        tdalignright, DepositFee, endtd, & ! D
!        tdalignright, DepositFee, endtd, & ! E
!        tdalignright, DepositFee, endtd, & ! Graduate
!        tdnbspendtd//endtr ! Lab
!        totalA = totalA + DepositFee
!        totalB = totalB + DepositFee
!        totalC = totalC + DepositFee
!        totalD = totalD + DepositFee
!        totalE = totalE + DepositFee
!        totalGraduate = totalGraduate + DepositFee
!        ! IDFee
!        write(device,AFORMAT) begintr//'<td colspan="2" align="right">ID Fee'//endtd//tdnbspendtd
!        write(device,'(a,f9.1,a)') &
!        tdalignright, IDFee, endtd, & ! A
!        tdalignright, IDFee, endtd, & ! B
!        tdalignright, IDFee, endtd, & ! C
!        tdalignright, IDFee, endtd, & ! D
!        tdalignright, IDFee, endtd, & ! E
!        tdalignright, IDFee, endtd, & ! Graduate
!        tdnbspendtd//endtr ! Lab
!        totalA = totalA + IDFee
!        totalB = totalB + IDFee
!        totalC = totalC + IDFee
!        totalD = totalD + IDFee
!        totalE = totalE + IDFee
!        totalGraduate = totalGraduate + IDFee
!
!        ! EducationDevelopment
!        write(device,AFORMAT) begintr//'<td colspan="2" align="right">Education Development'//endtd//tdnbspendtd
!        write(device,'(a,f9.1,a)') &
!        '<td colspan="6">'//nbsp//endtd//tdalignright, EducationDevelopment, endtd//endtr ! Lab
!        totalLabFee = totalLabFee+ EducationDevelopment
!
!        ! totals
!        write(device,AFORMAT) &
!        begintr//'<td colspan="10">'//horizontal//endtd//endtr, &
!        begintr//'<th colspan="3" align="right">TOTAL'//endth
!        write(device,'(a,f9.1,a)') &
!        thalignright, totalA, endth, & ! Total A
!        thalignright, totalB, endth, & ! Total B
!        thalignright, totalC, endth, & ! Total C
!        thalignright, totalD, endth, & ! Total D
!        thalignright, totalE, endth, & ! Total E
!        thalignright, totalGraduate, endth, & ! Total Graduate
!        thalignright, totalLabFee, endth//endtr ! Total Lab
!
!        write(device,AFORMAT) endtable
!
!
!    end subroutine enlistment_fees


    subroutine enlistment_printable  (device, NumSections, Section, eList)
        integer, intent (in) :: device
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_PRE_ENLISTMENT), intent(in) :: eList(0:)
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        integer ::  ierr, tLen1
        integer, dimension(60,6) :: TimeTable
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
            if ( (is_adviser_of_student(targetStudent, orHigherUp) .and. isPeriodOne) .or. &
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
        integer :: crse, idx, mdx, sect, previous, conflict
        real :: totalUnits, classUnits, totalHours, classHours, totalTuition, classTuition, totalLabFee, classLabFee

        write(device,AFORMAT) beginbold//trim(Student(std)%StdNo)//SPACE//trim(Student(std)%Name)// &
            linebreak//text_curriculum_info(Student(std)%CurriculumIdx)//linebreak//linebreak//'Enlisted subjects for '// &
            trim(termDescription)//endbold//horizontal

        if (lenSL < 3) then
            write(device,AFORMAT) linebreak//'Not enlisted in any class?'//linebreak
            return
        end if

        totalUnits = 0.0
        totalHours = 0.0
        totalTuition = 0.0
        totalLabFee = 0.0

        write(device,AFORMAT) '<table border="0" width="100%">'//begintr, &
            thalignleft//'Subject'//endth// &
            thalignleft//'Section'//endth//&
            thalignleft//'Units'//endth// &
            thalignleft//'Hours'//endth// &
            thalignleft//'Tuition'//endth// &
            thalignleft//'Lab Fee'//endth// &
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
                    classLabFee = 0.0
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
                    write(device,AFORMAT) &
                        begintd//trim(text_time_period(Section(sect)%bTimeIdx(1), Section(sect)%eTimeIdx(1)))//endtd// &
                        begintd//trim(text_days_of_section(Section(sect)))//endtd// &
                        begintd//trim(Room(Section(sect)%RoomIdx(1))%Code)//endtd//endtr
                else
                    mdx = 1
                    write(device,AFORMAT) &
                        begintd//trim(text_time_period(Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)))//endtd// &
                        begintd//trim(txtDay(Section(sect)%DayIdx(mdx)))//endtd// &
                        begintd//trim(Room(Section(sect)%RoomIdx(mdx))%Code)//endtd//endtr
                    do mdx=2,Section(sect)%NMeets
                        write(device,AFORMAT) begintr//'<td colspan="6">'//nbsp//endtd// &
                            begintd//trim(text_time_period(Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)))//endtd// &
                            begintd//trim(txtDay(Section(sect)%DayIdx(mdx)))//endtd// &
                            begintd//trim(Room(Section(sect)%RoomIdx(mdx))%Code)//endtd//endtr
                    end do
                end if
                if (conflict>0) write(device,AFORMAT) &
                    begintr//'<td align="center" colspan="9">'//red//'CONFLICT between '//trim(Section(sect)%ClassId)//' and '// &
                    trim(Section(conflict)%ClassId)//black//endtd//endtr

            end if

        end do
        write(device,AFORMAT) begintr//'<td colspan="9">'//horizontal//endtd//endtr, &
            begintr//tdnbspendtd//begintd//beginbold//'Totals'//endbold//' : '//endtd// & ! code
            begintd//trim(ftoa(totalUnits,1))//endtd//begintd//trim(ftoa(totalHours,2))//endtd// & ! hours
            begintd//trim(ftoa(totalTuition,2))//endtd//begintd//trim(ftoa(totalLabFee,2))//endtd// & ! fees
            tdnbspendtd// tdnbspendtd// tdnbspendtd//endtr, &
            endtable


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
!                            exit
!                        end if
!                    end if
!                end if
!            end do
!        end do

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
            '<html><head><title>'//trim(UniversityCode)//SPACE//PROGNAME//VERSION// &
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
        character(len=MAX_LEN_CLASS_ID) :: tClassId, otherClass
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
            '<html><head><title>'//trim(UniversityCode)//SPACE//PROGNAME//VERSION// &
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

                write(device,AFORMAT) begintr// &
                    begintd//trim(itoa(tdx))//DOT//endtd// &
                    begintd//trim(Student(std)%StdNo)//endtd// &
                    tdaligncenter//trim(txtGrade(pGrade(gdx)))//endtd// &
                    begintd//trim(Student(std)%Name)//endtd
                if (step+tdx<=n_count) then ! pos<=n_count) then
                    pos = 2*(step+tdx) - 1
                    std = tArray(pos)
                    gdx = tArray(pos+1)
                    write(device,AFORMAT) &
                        begintd//trim(itoa(step+tdx))//DOT//endtd// &
                        begintd//trim(Student(std)%StdNo)//endtd// &
                        tdaligncenter//trim(txtGrade(pGrade(gdx)))//endtd// &
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
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        integer :: ierr, i, m, n, std

        ! which college ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        targetCollege = index_to_college(tCollege)

        n = 0
        tArray = 0
        do i=1,NumStudents+NumAdditionalStudents
            std = StdRank(i)
            if (targetCollege/=NumColleges) then
                if (targetCollege/=Curriculum(Student(std)%CurriculumIdx)%CollegeIdx) cycle
            end if
            m = sum(eList(std)%Section(:))
            if (m>0) cycle ! has at least one class

            n = n + 1
            tArray(n) = std

        end do

        call html_write_header(device, trim(tCollege)//' students not enrolled in any class')
        call html_student_list (device, n, tArray, .true.)
        write(device,AFORMAT) horizontal

    end subroutine  enlistment_no_classes


end module EditENLISTMENT
