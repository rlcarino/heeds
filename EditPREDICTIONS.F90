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


module EditPREDICTIONS

    use HTML

    implicit none

    type (TYPE_STUDENT), private :: wrkStudent
    integer, private :: lenGrade, lenPlan

contains


    subroutine checklist_edit (device, UseClasses, Section, Offering, path)
        implicit none
        integer, intent (in) :: device
        logical, intent (in) :: UseClasses
        type (TYPE_OFFERED_SUBJECTS), intent(in), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        type (TYPE_SECTION), intent(in) :: Section(0:)
        character(len=*), intent(in) :: path

        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject, input_name1, input_name2, input_value, currentSubject, update
        character(len=3*MAX_LEN_SUBJECT_CODE) :: tAction
        character (len=MAX_LEN_TEXT_YEAR) :: tYear
        character (len=MAX_LEN_TEXT_SEMESTER) :: tTerm
        character (len=MAX_LEN_TEXT_GRADE) :: tGrade
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character(len=127) :: mesg, TCGline
        logical :: FlagIsUp, isDirtyPlan, isDirtyGrade

        integer :: crse, year, term, nsubs, nreqs, n_changes
        integer :: crse_required, crse_current, crse_update, rank
        integer :: cdx, gdx, idx, jdx, ierr, i, j, k, l
        type (TYPE_PRE_ENLISTMENT) :: Advice

        ! which student, action?
        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)

        targetStudent = index_to_student(tStdNo)
        targetCurriculum = Student(targetStudent)%CurriculumIdx
        targetCollege = Curriculum(targetCurriculum)%CollegeIdx

        call html_comment('checklist_edit('//tStdNo//trim(tAction)//')')

        ! read checklist
        call read_student_records (targetStudent)
        wrkStudent = Student(targetStudent)
        lenGrade = wrkStudent%Record(1,0)
        lenPlan = wrkStudent%Reqd(0,0)

        call html_comment('# grades='//itoa(lenGrade), '# plans='//itoa(lenPlan))

        isDirtyPlan = .false. ! no changes yet
        isDirtyGrade = .false. ! no changes yet
        isDirtyPREDICTIONS = .false. ! no changes yet
        isDirtyWAIVERCOI = .false. ! no changes yet

        TCGline = SPACE
        mesg = SPACE
        select case (trim(tAction))

            case ('Change CURRICULUM')
                ! change to which curriculum?
                call cgi_get_named_string(QUERY_STRING, 'A2', tCurriculum, ierr)
                cdx = index_to_curriculum(tCurriculum)
                !write(*,*) tCurriculum, cdx, Curriculum(cdx)%Code
                if (ierr/=0 .or. cdx<=0) then
                    mesg = 'Index to replacement curriculum not valid?'
                else
                    Student(targetStudent)%CurriculumIdx = cdx
                    targetCurriculum = cdx
                    mesg = trim(tAction)//' to '//Curriculum(cdx)%Code
                    isDirtySTUDENTS = .true. ! trigger rewrite of STUDENTS
                end if


            case ('ADDITIONAL subject')
                call cgi_get_named_string(QUERY_STRING, 'subject', tSubject, crse)
                call cgi_get_named_string(QUERY_STRING, 'year', tYear, year)
                call cgi_get_named_string(QUERY_STRING, 'term', tTerm, term)
                if (crse/=0 .or. year/=0 .or. term/=0) then ! a name was not specified?
                    mesg = trim(tAction)//' : "year" or "term" or "subject" not spelled correctly?'
                else if (tSubject==SPACE .or. tYear==SPACE .or. tTerm==SPACE) then ! a value was not specified?
                    mesg = trim(tAction)//' : "year" or "term" or "subject" not specified?'
                else
                    crse = index_to_subject(tSubject)
                    term = index_to_term(tTerm)
                    year = index_to_year(tYear)
                    if (crse<=0 .or. term<0 .or. year<=0) then
                        mesg = trim(tAction)//' : Year or Term or Subject" not valid?'
                    else ! add ADDITIONAL subject to Reqd()
                        mesg = trim(tAction)//' : '//trim(tYear)//COMMA//trim(tTerm)//COMMA//tSubject
                        isDirtyPlan = .true.

                        input_name1 = 'ADDITIONAL'
                        crse_required = index_to_subject(input_name1)

                        lenPlan = lenPlan + 1
                        wrkStudent%Reqd(0,0) = lenPlan
                        wrkStudent%Reqd(-1,lenPlan) = 3*(year-1) + term
                        wrkStudent%Reqd(0,lenPlan) = 1
                        wrkStudent%Reqd(1,lenPlan) = crse_required
                        wrkStudent%Subst(0,lenPlan) = 1
                        wrkStudent%Subst(1,lenPlan) = crse

                        Student(targetStudent)%Reqd = wrkStudent%Reqd
                        Student(targetStudent)%Subst = wrkStudent%Subst

                    end if
                end if

            case ('Cancel ADDITIONAL')
                call cgi_get_named_string(QUERY_STRING, 'subject', tSubject, crse)
                if (crse/=0) then ! a name was not specified?
                    mesg = trim(tAction)//' : subject not spelled correctly?'
                else if (tSubject==SPACE) then ! a value was not specified?
                    mesg = trim(tAction)//' : subject not specified?'
                else
                    crse = index_to_subject(tSubject)
                    if (crse<=0) then
                        mesg = trim(tAction)//' : subject not valid?'
                    else
                        mesg = trim(tAction)//' : subject to delete not found in checklist - '//tSubject
                        ! check TCG for specified ADDITIONAL subject
                        input_name1 = 'ADDITIONAL'
                        crse_required = index_to_subject(input_name1)

                        FlagIsUp = .false. ! not found
                        do k=1,lenPlan
                            if (wrkStudent%Reqd(1,k)==crse_required .and. &
                                wrkStudent%Subst(1,k)==crse) then
                                FlagIsUp = .true. ! found
                                exit
                            end if
                        end do
                        if (FlagIsUp) then ! shift-erase
                            do i=k+1,lenPlan
                                wrkStudent%Reqd(:,i-1) = wrkStudent%Reqd(:,i)
                                wrkStudent%Subst(:,i-1) = wrkStudent%Subst(:,i)
                            end do
                            ! re-initialize previous last location
                            wrkStudent%Reqd(:,lenPlan) = 0
                            wrkStudent%Subst(:,lenPlan) = 0
                            lenPlan = lenPlan - 1
                            wrkStudent%Reqd(0,0) = lenPlan ! reset last location

                            isDirtyPlan = .true.
                            mesg = tAction//tSubject

                            Student(targetStudent)%Reqd = wrkStudent%Reqd
                            Student(targetStudent)%Subst = wrkStudent%Subst
                        end if

                    end if
                end if

            case ('Subject SUBSTITUTION')

                nsubs = 0
                nreqs = 0
                TCGline = SPACE

                ! PlanOfStudy,Year,Term,Reqd(1),Reqd(2),...,Reqd(m),Subst(1),Subst(2),...,Subst(n)
                !1                2    3    4                           5
                ! get substitute subjects
                do i=5,1,-1
                    call cgi_get_named_string(QUERY_STRING, 'sub'//trim(itoa(i)), tSubject, ierr)
                    if (ierr/=0 .or. tSubject==SPACE) cycle
                    crse = index_to_subject(tSubject)
                    if (crse <= 0) then
                        mesg = trim(tAction)//' : '//tSubject//'- Subject code not valid?'
                    else
                        j = index_of_subject_in_curriculum(Curriculum(targetCurriculum), crse)
                        if (j>0) then ! a required subject
                            mesg = trim(tAction)//' : '//tSubject//'- substitute subject already in curriculum?'
                            nsubs = 0
                            exit
                        else
                            TCGline = COMMA//trim(tSubject)//TCGline
                            nsubs = nsubs + 1
                            wrkStudent%Subst(0,lenPlan+1) = nsubs
                            wrkStudent%Subst(nsubs,lenPlan+1) = crse
                        end if
                    end if
                end do

                ! get required subjects
                do i=5,1,-1
                    call cgi_get_named_string(QUERY_STRING, 'req'//trim(itoa(i)), tSubject, ierr)
                    if (ierr/=0 .or. tSubject==SPACE) cycle
                    crse = index_to_subject(tSubject)
                    if (crse <= 0) then
                        mesg = trim(tAction)//' : '//tSubject//'- Subject code not valid?'
                    else
                        j = index_of_subject_in_curriculum(Curriculum(targetCurriculum), crse)
                        if (j>0) then ! a required subject
                            ! check if already required in another substitution rule
                            FlagIsUp = .false.
                            do k=1,lenPlan
                                do l=1,wrkStudent%Reqd(0,k)
                                    if (crse==wrkStudent%Reqd(l,k)) then
                                        FlagIsUp = .true.
                                        exit
                                    end if
                                end do
                                if (FlagIsUp) exit
                            end do
                            if (.not. FlagIsUp) then
                                TCGline = COMMA//trim(tSubject)//TCGline
                                nreqs = nreqs + 1
                                wrkStudent%Reqd(-1,lenPlan+1) = Curriculum(targetCurriculum)%SubjectTerm(j)
                                wrkStudent%Reqd(0,lenPlan+1) = nreqs
                                wrkStudent%Reqd(nsubs,lenPlan+1) = crse
                            else
                                mesg = trim(tAction)//' : '//tSubject// &
                                    '- required subject already used in another substitution.'
                                nreqs = 0
                                exit
                            end if
                        else
                            mesg = trim(tAction)//' : '//tSubject//'- required subject not in curriculum?'
                            nreqs = 0
                            exit
                        end if
                    end if
                end do

                if (nsubs*nreqs>0) then
                    mesg = trim(tAction)//' : '//TCGline(2:)
                    lenPlan = lenPlan + 1
                    wrkStudent%Reqd(0,0) = lenPlan
                    if (nreqs>1) then ! group substitution; no year/term
                        wrkStudent%Reqd(-1,lenPlan) = 0
                    end if
                    ! copy to Student(std)
                    Student(targetStudent)%Reqd = wrkStudent%Reqd
                    Student(targetStudent)%Subst = wrkStudent%Subst

                    isDirtyPlan = .true.
                end if

            case ('Cancel SUBSTITUTION')

                call cgi_get_named_string(QUERY_STRING, 'subject', tSubject, crse)

                if (crse/=0) then ! a name was not specified?
                    mesg = trim(tAction)//' : subject not spelled correctly?'

                else if (tSubject==SPACE) then ! a value was not specified?
                    mesg = trim(tAction)//' : required subject not specified?'

                else ! some subject was specified
                    crse = index_to_subject(tSubject)
                    if (crse<=0) then ! not a named subject
                        mesg = trim(tAction)//' : subject not valid?'
                    else
                        mesg = trim(tAction)//' : rule not found in checklist - '//tSubject
                        ! check Student(std)%Reqd() for specified SUBSTITUTION
                        idx = 0
                        do k=1,lenPlan
                            do i=1,wrkStudent%Reqd(0,k)
                                if (wrkStudent%Reqd(i,k)==crse) then
                                    isDirtyPlan = .true.
                                    idx = k
                                    exit
                                end if
                            end do
                            if (isDirtyPlan) exit
                        end do
                        if (isDirtyPlan) then ! required subject found
                            ! shift substitutions down
                            do k=idx+1,lenPlan
                                wrkStudent%Reqd(:,k-1) = wrkStudent%Reqd(:,k)
                                wrkStudent%Subst(:,k-1) = wrkStudent%Subst(:,k)
                            end do
                            ! re-initialize previous last location
                            wrkStudent%Reqd(:,lenPlan) = 0
                            wrkStudent%Subst(:,lenPlan) = 0
                            lenPlan = lenPlan - 1 ! decrease counter
                            wrkStudent%Reqd(0,0) = lenPlan
                            mesg = trim(tAction)//' for '//tSubject
                            ! copy to Student(std)
                            Student(targetStudent)%Reqd = wrkStudent%Reqd
                            Student(targetStudent)%Subst = wrkStudent%Subst
                        end if
                    end if
                end if

            case ('Update ELECTIVE')

                n_changes = 0
                do idx=1,Curriculum(targetCurriculum)%NSubjects
                    crse_required = Curriculum(targetCurriculum)%SubjectIdx(idx)
                    if (crse_required > 0) cycle ! named subject
                    rank = Curriculum(targetCurriculum)%SubjectTerm(idx)

                    call blank_to_underscore(Subject(crse_required)%Name, input_name1)
                    input_name1 = trim(input_name1)//DASH//trim(itoa(rank))//':'
                    !write(*,*) 'Looking for '//input_name1
                    call cgi_get_wild_name_value(QUERY_STRING, input_name1, input_name2, input_value, ierr)
                    if (ierr/=0) cycle ! not found

                    call rank_to_year_term(rank, Year, Term)
                    call underscore_to_blank(input_name2, currentSubject)
                    call underscore_to_blank(input_value, update)

                    !write(*,*) '  Found : '//Subject(crse_required)%Name, currentSubject, update

                    crse_current = index_to_subject(currentSubject)

                    if (update/=SPACE) then ! something in update

                        crse_update = index_to_subject(update)
                        if (crse_update <= 0) then
                            !mesg = trim(tAction)//' : '//update//'- code not valid?'
                            !write(*,*) trim(mesg)
                            !write(*,*) trim(tAction)//' : '//update//'- code not valid?'
                            cycle
                        end if

                        jdx = index_of_subject_in_curriculum(Curriculum(targetCurriculum), crse_update)
                        if (jdx>0) then ! a required subject
                            !mesg = trim(tAction)//' : '//update//'- already required?'
                            !write(*,*) trim(mesg)
                            !write(*,*) trim(tAction)//' : '//update//'- already required?'
                            cycle
                        end if

                        if (currentSubject == update) then ! no change
                            !mesg = trim(tAction)//' : No change to '//currentSubject
                            !write(*,*) trim(mesg)
                            !write(*,*) trim(tAction)//' : No change to '//currentSubject
                            cycle
                        end if

                        ! make change
                        if (crse_current>0) then ! previously specified
                            FlagIsUp = .true.
                            do k=1,lenPlan
                                if (wrkStudent%Reqd(1,k)==crse_required .and. &
                                    wrkStudent%Subst(1,k)==crse_current) then
                                    wrkStudent%Subst(1,k) = crse_update
                                    FlagIsUp = .false.
                                    mesg = trim(mesg)//', '//trim(Subject(crse_update)%Name)
                                    exit
                                end if
                            end do

                            if (FlagIsUp) then ! make new entry
                                lenPlan = lenPlan + 1
                                wrkStudent%Reqd(0,0) = lenPlan
                                wrkStudent%Reqd(-1,lenPlan) = rank
                                wrkStudent%Reqd(0,lenPlan) = 1
                                wrkStudent%Reqd(1,lenPlan) = crse_required
                                wrkStudent%Subst(0,lenPlan) = 1
                                wrkStudent%Subst(1,lenPlan) = crse_update
                                mesg = trim(mesg)//', '//trim(Subject(crse_update)%Name)

                            end if

                        else ! make new entry in TCG
                            lenPlan = lenPlan + 1
                            wrkStudent%Reqd(0,0) = lenPlan
                            wrkStudent%Reqd(-1,lenPlan) = rank
                            wrkStudent%Reqd(0,lenPlan) = 1
                            wrkStudent%Reqd(1,lenPlan) = crse_required
                            wrkStudent%Subst(0,lenPlan) = 1
                            wrkStudent%Subst(1,lenPlan) = crse_update
                            mesg = trim(mesg)//', '//trim(Subject(crse_update)%Name)

                        end if
                        n_changes = n_changes + 1

                    else ! update is blank? remove current elective
                        if (crse_current>0) then ! previously specified

                            FlagIsUp = .false.
                            do k=1,lenPlan
                                if (wrkStudent%Reqd(1,k)==crse_required .and. &
                                    wrkStudent%Subst(1,k)==crse_current) then
                                    FlagIsUp = .true.
                                    exit
                                end if
                            end do
                            if (FlagIsUp) then
                                do i=k+1,lenPlan
                                    wrkStudent%Reqd(:,i-1) = wrkStudent%Reqd(:,i)
                                    wrkStudent%Subst(:,i-1) = wrkStudent%Subst(:,i)
                                end do
                                ! re-initialize previous last location
                                wrkStudent%Reqd(:,lenPlan) = 0
                                wrkStudent%Subst(:,lenPlan) = 0
                                lenPlan = lenPlan - 1
                                wrkStudent%Reqd(0,0) = lenPlan ! reset last location
                                n_changes = n_changes + 1
                            end if

                        end if
                    end if

                end do

                if (n_changes==0) then
                    mesg = trim(tAction)//' : Nothing to update?'
                else
                    ! copy to Student(std)
                    Student(targetStudent)%Reqd = wrkStudent%Reqd
                    Student(targetStudent)%Subst = wrkStudent%Subst
                    mesg = trim(tAction)//mesg
                    isDirtyPlan = .true.
                end if


            case ('Change GRADE') ! look for GRADE:<subject>=<grade>

                n_changes = 0
                input_name1 = 'GRADE:'
                loop_GRADES: &
                do while (.true.)
                    call cgi_get_wild_name_value(QUERY_STRING, input_name1, input_name2, input_value, ierr)
                    if (ierr/=0) exit ! no more GRADE: in QUERY

                    call underscore_to_blank(input_name2, tSubject)
                    !call underscore_to_blank(input_value, tGrade)
                    tGrade = input_value

                    !write(*,*) tSubject, tGrade

                    crse = index_to_subject(tSubject)
                    if (crse <= 0) cycle ! not a named subject

                    if (tGrade/=SPACE) then ! not empty

                        gdx = index_to_grade(tGrade)
                        if (gdx==0) cycle ! underscores
                        if (gdx<0) then ! invalid grade or underscores
                            mesg = ' : invalid grade '//tGrade
                            cycle
                        end if

                        ! subject+grade in Record()?
                        FlagIsUp = .false.
                        do k=lenGrade,1,-1
                            if (wrkStudent%Record(4,k)/=crse) cycle ! not the subject
                            if (gdx==wrkStudent%Record(5,k)) cycle loop_GRADES ! same grade
                            FlagIsUp = .true. ! subject matched, w/ diff grade
                            wrkStudent%Record(5,k) = gdx ! make change
                            n_changes = n_changes + 1
                            mesg = ' : '//trim(Subject(crse)%Name)//mesg
                            cycle loop_GRADES
                        end do

                        if (.not. FlagIsUp) then ! not found; add an entry

                            lenGrade = lenGrade + 1
                            wrkStudent%Record(1,0) = lenGrade
                            wrkStudent%Record(1,lenGrade) = 1
                            wrkStudent%Record(2,lenGrade) = currentYear
                            wrkStudent%Record(3,lenGrade) = currentTerm
                            wrkStudent%Record(4,lenGrade) = crse
                            wrkStudent%Record(5,lenGrade) = gdx
                            mesg = ' : '//trim(Subject(crse)%Name)//', to '//tGrade//mesg
                            n_changes = n_changes + 1

                        end if

                    else ! grade is empty (to indicate 'Remove grade')

                        FlagIsUp = .false. ! subject found?
                        do k=lenGrade,1,-1
                            if (wrkStudent%Record(4,k)/=crse) cycle ! not the subject
                            FlagIsUp = .true.
                            exit
                        end do
                        if (FlagIsUp) then
                            do i=k+1,lenGrade ! shift down
                                wrkStudent%Record(:,i-1) = wrkStudent%Record(:,i)
                            end do
                            wrkStudent%Record(:,lenGrade) = 0
                            lenGrade = lenGrade - 1
                            wrkStudent%Record(1,0) = lenGrade
                            n_changes = n_changes + 1
                            mesg = ' : Removed grade in '//trim(Subject(crse)%Name)//mesg
                        end if

                    end if

                end do loop_GRADES

                if (n_changes==0) then
                    mesg = trim(tAction)//' : Nothing to update?'
                else
                    mesg = trim(tAction)//mesg
                    Student(targetStudent)%Record = wrkStudent%Record
                    isDirtyGrade = .true.
                end if


            case ('As ADVICE')

                call collect_advice(Advice, n_changes, mesg)
                NumPredictionRecords = NumPredictionRecords - Advised(targetStudent)%lenSubject + Advice%lenSubject
                Advised(targetStudent) = Advice
                isDirtyPREDICTIONS = .true.
                mesg = trim(tAction)//' : '//trim(itoa(Advice%lenSubject))//' entries.'


            case ('Update ADVICE')

                call collect_advice(Advice, n_changes, mesg)
                if (n_changes>0) then
                    mesg = trim(tAction)//mesg
                    Advised(targetStudent) = Advice
                    isDirtyPREDICTIONS = .true.
                end if

            case default


        end select

        if (isDirtyPlan) then
            call xml_write_substitutions(targetStudent)
        end if

        if (isDirtyGrade) then
            call xml_write_student_grades(targetStudent)
        end if

        if (isDirtyWAIVERCOI) then
            call xml_write_waivers(path, Section)
        end if

        if (isDirtyPREDICTIONS) then
            call xml_write_pre_enlistment(path, 'PREDICTIONS', Advised, Section, targetCurriculum)
        end if

        call checklist_write_menu (device, UseClasses, isDirtyPlan .or. isDirtyGrade, Offering, mesg)


    end subroutine checklist_edit


    subroutine checklist_write_menu (device, UseClasses, isDirtyMCL, Offering, mesg)
        integer, intent (in) :: device
        logical, intent (in) :: UseClasses, isDirtyMCL
        type (TYPE_OFFERED_SUBJECTS), intent(in), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        character(len=*), intent (in)  :: mesg

        character(len=6) :: tProb
        type (TYPE_PRE_ENLISTMENT) :: Advice, prevAdvice
        integer :: idxCurr, j, k, l, MissingPOCW, NRemaining, lenSubject, notSpecified ! , checklistout

        call html_comment('checklist_write_menu()')

        ! Plan of Study
        idxCurr = Student(targetStudent)%CurriculumIdx
        notSpecified = 0 ! how many entries in plan for this curriculum?
        do k=1,Curriculum(idxCurr)%NSubjects
            if (Curriculum(idxCurr)%SubjectIdx(k)<0) then
                notSpecified = notSpecified + 1
                exit
            end if
        end do

        call html_write_header(device, Student(targetStudent)%StdNo//nbsp// &
            trim(Student(targetStudent)%Name)// &
            trim(make_href(fnCurriculum, Curriculum(idxCurr)%Code, A1=Curriculum(idxCurr)%Code, &
            pre=' <small> (', post=' ) </small>')), mesg)

        if (isRoleAdmin .or. (isRoleSRE .and. CurrProgCode(CurriculumIdxUser)==CurrProgCode(targetCurriculum) ) ) then
            ! change curriculum form
            call make_form_start(device, fnEditCheckList, Student(targetStudent)%StdNo)
            write(device,AFORMAT) &
                '<select name="A2">'
            do l=1,NumCurricula
                k = Curriculum(l)%CollegeIdx
                !if (College(k)%Code == ADMINISTRATION) cycle
                if (l==idxCurr) then
                    j = 1
                else
                    j = 0
                end if
                write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(Curriculum(l)%Code)//'"> '// &
                    trim(text_curriculum_info(l))
            end do
            write(device,AFORMAT) '</select>', &
                nbsp//nbsp//'<input type="submit" name="action" value="Change CURRICULUM"></form>'

        else

            write(device,AFORMAT) trim(text_curriculum_info(idxCurr))//'<br>'

        end if

        !call checklist_page_links(device, .true.)

        call advise_student (targetStudent, UseClasses, Offering, WaiverCOI(targetStudent), Advice, MissingPOCW, NRemaining)
        lenSubject = Advice%NPriority+Advice%NAlternates+Advice%NCurrent

        call checklist_display (device, targetStudent, Advice, MissingPOCW, NRemaining)

        ! offer Advice for PREDICTION or ENLISTMENT depending on period
        if (isRoleAdmin .or. (isRoleSRE .and. CurrProgCode(CurriculumIdxUser)==CurrProgCode(targetCurriculum) ) ) then
            if (lenSubject>0) then
                if (advisingPeriod) then
                    call make_form_start(device, fnEditCheckList, Student(targetStudent)%StdNo)
                else
                    call make_form_start(device, fnChangeMatriculation, Student(targetStudent)%StdNo, 'Switch')
                end if
                write(device,AFORMAT) &
                    '<input type="hidden" name="earned" value="'//trim(itoa(Advice%UnitsEarned))//'">', &
                    '<input type="hidden" name="classification" value="'//trim(itoa(Advice%StdClassification))//'">', &
                    '<input type="hidden" name="year" value="'//trim(itoa(Advice%StdYear))//'">', &
                    '<input type="hidden" name="allowed" value="'//trim(itoa(Advice%AllowedLoad))//'">', &
                    '<input type="hidden" name="group" value="'//trim(itoa(Advice%StdPriority))//'">', &
                    '<input type="hidden" name="priority" value="'//trim(itoa(Advice%NPriority))//'">', &
                    '<input type="hidden" name="alternate" value="'//trim(itoa(Advice%NAlternates))//'">', &
                    '<input type="hidden" name="current" value="'//trim(itoa(Advice%NCurrent))//'">'
                do l=1,lenSubject
                    write(tProb, '(f6.4)') Advice%Contrib(l)
                    write(device,AFORMAT) &
                        '<input type="hidden" name="pri'//trim(itoa(l))//'" value="'//trim(Subject(Advice%Subject(l))%Name)// &
                        COMMA//tProb//'">'
                end do
                write(device,AFORMAT) '<br><b>Use FEASIBLE subjects </b>'//nbsp, &
                    '<input type="submit" name="action" value="As ADVICE"></form>'
            end if
            write(device,AFORMAT) '<hr>'

        else
            return
        end if

        write(device,AFORMAT) &
            '<br><b>UPDATE SUBJECTS for '//txtSemester(nextTerm+6)// &
            ' Semester, '//text_school_year(nextYear)//'</b>'

        if (advisingPeriod) then

            call make_form_start(device, fnEditCheckList, Student(targetStudent)%StdNo)
            prevAdvice = Advised(targetStudent)
            lenSubject = prevAdvice%NPriority + prevAdvice%NAlternates + prevAdvice%NCurrent

            write(device,AFORMAT) &
                '<br><i>Allowed load </i> '//nbsp//' <input type="text" name="allowed" size="5" value="'// &
                trim(itoa(prevAdvice%AllowedLoad))//'">', &
                nbsp//nbsp//' <i>Priority group </i> '//nbsp//' <input type="text" name="group" size="5" value="'// &
                trim(itoa(prevAdvice%StdPriority))//'"> <small>(1=New freshman, 2=Graduating, '// &
                '3=Good standing, 4=Warning, 5=Probation, 6=dismissed/PD)</small>', &
                '<input type="hidden" name="earned" value="'//trim(itoa(prevAdvice%UnitsEarned))//'">', &
                '<input type="hidden" name="classification" value="'//trim(itoa(prevAdvice%StdClassification))//'">', &
                '<input type="hidden" name="year" value="'//trim(itoa(prevAdvice%StdYear))//'">', &
                '<input type="hidden" name="priority" value="'//trim(itoa(prevAdvice%NPriority))//'">', &
                '<input type="hidden" name="alternate" value="'//trim(itoa(prevAdvice%NAlternates))//'">', &
                '<input type="hidden" name="current" value="'//trim(itoa(prevAdvice%NCurrent))//'">'

            do l=1,lenSubject
                k = prevAdvice%Subject(l)
                write(tProb, '(f6.4)') prevAdvice%Contrib(l)
                write(device,AFORMAT) &
                    '<input type="hidden" name="pri'//trim(itoa(l))//'" value="'//trim(Subject(k)%Name)//COMMA//tProb//'">'
            end do

            write(device,AFORMAT) '<br><table border="0" width="80%">', &
                begintr//thaligncenter//'Action'//endth// &
                tdnbspendtd// &
                thalignleft//'Subject'//endth// &
                thaligncenter//'Credit'//endth// &
                thalignleft//'Title'//endth//endtr, &
                begintr//tdaligncenter//'<i>Add</i>'//endtd// &
                tdnbspendtd// &
                begintd//'<input type="text" name="additional" size="12" value="">'//endtd// &
                tdnbspendtd// &
                begintd//'<i>(Approved COI or WAIVER of prerequisite)</i>'//endtd//endtr

            do l=1,prevAdvice%NPriority+prevAdvice%NAlternates
                k = prevAdvice%Subject(l)
                write(device,AFORMAT) &
                    begintr//tdaligncenter//'<i>Del '//trim(itoa(l))//'</i>'//endtd// &
                    tdaligncenter//'<input type="checkbox" name="del'//trim(itoa(l))//'">'//endtd// &
                    begintd//trim(Subject(k)%Name)//endtd// &
                    tdaligncenter//trim(ftoa(Subject(k)%Units,1))//endtd// &
                    begintd//trim(Subject(k)%Title)//endtd//endtr
            end do

            if (prevAdvice%NCurrent>0) then
                write(device,AFORMAT) &
                    begintr//thaligncenter//'Fail'//endth// &
                    thalignleft//'Contrib'//endth// &
                    thalignleft//'Subject'//endth// &
                    thaligncenter//'Credit'//endth// &
                    thalignleft//'Title'//endth//endtr
                do l=prevAdvice%NPriority+prevAdvice%NAlternates+1, &
                        prevAdvice%NPriority+prevAdvice%NAlternates+prevAdvice%NCurrent
                    k = prevAdvice%Subject(l)
                    write(tProb, '(f6.4)') prevAdvice%Contrib(l)
                    write(device,AFORMAT) &
                        begintr//tdaligncenter//trim(itoa(l))//endtd// &
                        begintd//tProb//endtd// &
                        begintd//trim(Subject(k)%Name)//endtd// &
                        tdaligncenter//trim(ftoa(Subject(k)%Units,1))//endtd// &
                        begintd//trim(Subject(k)%Title)//endtd//endtr
                end do
            end if
            write(device,AFORMAT) '</table><br>', &
                '<input type="submit" name="action" value="Update ADVICE">', &
                '</form><hr>'

        else ! (.not. advisingPeriod) then

            call make_form_start(device, fnChangeMatriculation, Student(targetStudent)%StdNo, 'Switch')
            prevAdvice = Preenlisted(targetStudent)
            lenSubject = prevAdvice%NPriority + prevAdvice%NAlternates

            write(device,AFORMAT) &
                '<br><i>Allowed load </i> '//nbsp//' <input type="text" name="allowed" size="5" value="'// &
                trim(itoa(max(prevAdvice%AllowedLoad,Advice%AllowedLoad)))//'">', &
                '<input type="hidden" name="earned" value="'//trim(itoa(max(prevAdvice%UnitsEarned,Advice%UnitSEarned)))//'">', &
                '<input type="hidden" name="classification" value="'//trim(itoa(max(prevAdvice%StdClassification, &
                    Advice%StdClassification)))//'">', &
                '<input type="hidden" name="year" value="'//trim(itoa(max(prevAdvice%StdYear,Advice%StdYear)))//'">', &
                '<input type="hidden" name="priority" value="'//trim(itoa(prevAdvice%NPriority))//'">', &
                '<input type="hidden" name="alternate" value="'//trim(itoa(prevAdvice%NAlternates))//'">'

            do l=1,lenSubject
                k = prevAdvice%Subject(l)
                write(tProb, '(f6.4)') prevAdvice%Contrib(l)
                write(device,AFORMAT) &
                    '<input type="hidden" name="pri'//trim(itoa(l))//'" value="'//trim(Subject(k)%Name)//COMMA//tProb//'">'
            end do

            write(device,AFORMAT) '<br><table border="0" width="80%">', &
                begintr//thaligncenter//'Action'//endth// &
                tdnbspendtd// &
                thalignleft//'Subject'//endth// &
                thaligncenter//'Credit'//endth// &
                thalignleft//'Title'//endth//endtr, &
                begintr//tdaligncenter//'<i>Add</i>'//endtd// &
                tdnbspendtd// &
                begintd//'<input type="text" name="additional" size="12" value="">'//endtd// &
                tdnbspendtd// &
                begintd//'<i>(Approved COI or WAIVER of prerequisite)</i>'//endtd//endtr

            do l=1,lenSubject
                k = prevAdvice%Subject(l)
                write(device,AFORMAT) &
                    begintr//tdaligncenter//'<i>Del '//trim(itoa(l))//'</i>'//endtd// &
                    tdaligncenter//'<input type="checkbox" name="del'//trim(itoa(l))//'">'//endtd// &
                    begintd//trim(Subject(k)%Name)//endtd// &
                    tdaligncenter//trim(ftoa(Subject(k)%Units,1))//endtd// &
                    begintd//trim(Subject(k)%Title)//endtd//endtr
            end do

            write(device,AFORMAT) '</table><br>', &
                '<input type="submit" name="action" value="Use FOR ENLISTMENT">', &
                    '</form><hr>'
        end if

        if (notSpecified>0) then
            call substitution_form(device,targetStudent, &
                Advice%UnitsEarned, Advice%StdClassification, Advice%StdYear, MissingPOCW, &
                Advice%AllowedLoad, Advice%StdPriority, Advice%NPriority, Advice%NAlternates, Advice%NCurrent, NRemaining)
        end if

        ! ChangeOfGrade
        call change_grade_form(device,targetStudent)

        !    ! additional subject
        !    write(device,AFORMAT) &
        !      '<a name="ADDITIONAL subject"></a><hr>'// &
        !      trim(text_student_curriculum(targetStudent))// &
        !      '<form name="input" method="post" action="'//trim(CGI_PATH)//'">', &
        !'<input type="hidden" name="N" value="'//trim(USERNAME)//'">', &
        !      '<input type="hidden" name="F" value="'//trim(itoa(fnEditCheckList))//'">'// &
        !      '<input type="hidden" name="A1" value="'//trim(Student(targetStudent)%StdNo)//'">', &
        !      '<b>ADDITIONAL subject</b>: '//nbsp//' Subject <input name="subject" value="">'//nbsp//, &
        !      ' Year <input name="year" value="">'//nbsp//, &
        !      ' Term <input name="term" value="">'//nbsp, &
        !      nbsp//nbsp//'<input type="submit" name="action" value="ADDITIONAL subject">', &
        !      '<br><i>Note: Year=(THIRD,FOURTH,FIFTH,...) in curriculum, Term=(SUMMER,FIRST,SECOND)</i>', &
        !      '</form><hr>'
        !    call checklist_page_links(device) !, .true.)
        !
        !    ! Cancel additional subject
        !    write(device,AFORMAT) &
        !      '<a name="Cancel ADDITIONAL"></a><hr>'// &
        !      trim(text_student_curriculum(targetStudent))// &
        !      '<form name="input" method="post" action="'//trim(CGI_PATH)//'">', &
        !'<input type="hidden" name="N" value="'//trim(USERNAME)//'">', &
        !      '<input type="hidden" name="F" value="'//trim(itoa(fnEditCheckList))//'">'// &
        !      '<input type="hidden" name="A1" value="'//trim(Student(targetStudent)%StdNo)//'">', &
        !      '<b>Cancel ADDITIONAL subject</b> '//nbsp//' <input type="text" name="subject" value="">'//nbsp//, &
        !      nbsp//nbsp//'<input type="submit" name="action" value="Cancel ADDITIONAL">', &
        !      '</form><hr>'
        !    call checklist_page_links(device) !, .true.)

        ! subject substitution
        call make_form_start(device, fnEditCheckList, Student(targetStudent)%StdNo)
        write(device,AFORMAT) &
            '<br><b>Subject SUBSTITUTION</b>: Enter the required and substitute subjects<br>'// &
            '<table border="0" width="100%">'// &
            begintr//begintd//'Required :'//endtd//begintd// &
                '<input type="text" size="15" name="req1" value="">', &
                '<input type="text" size="15" name="req2" value="">', &
                '<input type="text" size="15" name="req3" value="">', &
                '<input type="text" size="15" name="req4" value="">', &
                '<input type="text" size="15" name="req5" value="">'//endtd//endtr, &
            begintr//begintd//'Substitute :'//endtd//begintd// &
                '<input type="text" size="15" name="sub1" value="">', &
                '<input type="text" size="15" name="sub2" value="">', &
                '<input type="text" size="15" name="sub3" value="">', &
                '<input type="text" size="15" name="sub4" value="">', &
                '<input type="text" size="15" name="sub5" value="">'//endtd//endtr, &
            '</table><br>'// &
            '<input type="submit" name="action" value="Subject SUBSTITUTION">', &
            '</form><hr>'

        call make_form_start(device, fnEditCheckList, Student(targetStudent)%StdNo)
        write(device,AFORMAT) &
            '<br><b>Cancel SUBSTITUTION</b>: Enter required subject in which credit is earned through substitution<br>', &
            '<table border="0" width="33%">'//begintr// &
            begintd//'Required :'//endtd// &
            begintd//'<input type="text" size="15" name="subject" value="">'//endtd// &
            endtr//'</table><br>', &
            '<input type="submit" name="action" value="Cancel SUBSTITUTION">', &
            '</form><hr>'


    end subroutine checklist_write_menu


    subroutine change_grade_form(device, std)
        integer, intent (in) :: std, device
        integer :: idx, tdx, Year, Term, m, n, rank
        integer, dimension(MAX_LEN_STUDENT_RECORD) :: p, q

!        write(device,AFORMAT) '<a name="Change GRADE"></a><hr>'
        call make_form_start(device, fnEditCheckList, Student(std)%StdNo)
        write(device,AFORMAT) &
            '<br><b>TEMPORARY CHANGE OF GRADE</b> for advising purposes. '// &
            red//'Student''s actual record will NOT be changed.'//black// &
            !'<br>'//trim(text_student_curriculum(std)), &
            '<br><i>Enter or modify contents of the edit boxes below, then click "Change GRADE"</i>', &
            '<table border="0" width="90%">'
        do tdx=1,CheckList%NumTerms,3
            call rank_to_year_term(tdx, Year, Term)
            write(device,AFORMAT) begintr//'<td colspan="4"><br><b>'//trim(txtYear(Year))// &
                ' Year, First Semester</b> ('//trim(itoa(TermUnits(tdx)))//' units)'//endtd// &
                tdnbspendtd//'<td colspan="4"><br><b>'//trim(txtYear(Year))// &
                ' Year, Second Semester</b> ('//trim(itoa(TermUnits(tdx+1)))//' units)'//endtd//endtr
            m = 0
            n = 0
            p = 0
            q = 0
            do idx=1,CheckList%NSubjects
                rank = CheckList%SubjectTerm(idx)
                if (rank == tdx) then
                    m = m+1
                    p(m) = idx
                else if (rank == tdx+1) then
                    n = n+1
                    q(n) = idx
                end if
            end do ! idx=1,CheckList%NSubjects
            do idx=1,min(m,n)
                write(device,AFORMAT) begintr//begintd//trim(CLExt(p(idx))%Disp_Subject)//endtd//&
                    begintd//trim(CLExt(p(idx))%Disp_Comment)//endtd//&
                    begintd//trim(CLExt(p(idx))%Disp_Input_Grade)//endtd//&
                    begintd//trim(CLExt(p(idx))%Disp_Units)//endtd//tdnbspendtd, &
                    begintd//trim(CLExt(q(idx))%Disp_Subject)//endtd//&
                    begintd//trim(CLExt(q(idx))%Disp_Comment)//endtd//&
                    begintd//trim(CLExt(q(idx))%Disp_Input_Grade)//endtd//&
                    begintd//trim(CLExt(q(idx))%Disp_Units)//endtd//endtr
            end do
            if (m > n) then
                do idx=n+1,m
                    write(device,AFORMAT) begintr//begintd//trim(CLExt(p(idx))%Disp_Subject)//endtd//&
                        begintd//trim(CLExt(p(idx))%Disp_Comment)//endtd//&
                        begintd//trim(CLExt(p(idx))%Disp_Input_Grade)//endtd//&
                        begintd//trim(CLExt(p(idx))%Disp_Units)//endtd//&
                        '<td colspan="5">'//nbsp//endtd//endtr
                end do
            end if
            if (n > m) then
                do idx=m+1,n
                    write(device,AFORMAT) begintr//'<td colspan="5">'//nbsp//endtd// &
                        begintd//trim(CLExt(q(idx))%Disp_Subject)//endtd//&
                        begintd//trim(CLExt(q(idx))%Disp_Comment)//endtd//&
                        begintd//trim(CLExt(q(idx))%Disp_Input_Grade)//endtd//&
                        begintd//trim(CLExt(q(idx))%Disp_Units)//endtd//endtr
                end do
            end if
            ! summer
            m = 0
            p = 0
            do idx=1,CheckList%NSubjects
                rank = CheckList%SubjectTerm(idx)
                if (rank == tdx+2) then
                    m = m+1
                    p(m) = idx
                end if
            end do ! idx=1,CheckList%NSubjects
            if (m > 0) then
                write(device,AFORMAT) begintr//'<td colspan="4"><br><b>'//trim(txtYear(Year))// &
                ' Year, Summer</b> ('//trim(itoa(TermUnits(tdx+2)))//' units)'//endtd// &
                '<td colspan="5">'//nbsp//endtd//endtr
                do idx=1,m
                    write(device,AFORMAT) begintr//begintd//trim(CLExt(p(idx))%Disp_Subject)//endtd//&
                        begintd//trim(CLExt(p(idx))%Disp_Comment)//endtd//&
                        begintd//trim(CLExt(p(idx))%Disp_Input_Grade)//endtd//&
                        begintd//trim(CLExt(p(idx))%Disp_Units)//endtd//&
                        '<td colspan="5">'//nbsp//endtd//endtr
                end do
            end if
        end do
        write(device,AFORMAT) '</table>', &
        '<br><input type="submit" name="action" value="Change GRADE"></form><hr>'

    end subroutine change_grade_form


    subroutine substitution_form(device, std, &
        UnitsEarned, StdClassification, StdYear, MissingPOCW, &
        AllowedLoad, StdPriority, NPriority, NAlternates, NCurrent, NRemaining)
        integer, intent (in) :: std, device, &
            UnitsEarned, StdClassification, StdYear, MissingPOCW, &
            AllowedLoad, StdPriority, NPriority, NAlternates, NCurrent, NRemaining
        integer :: idx, tdx, Year, Term, m, n, rank
        integer, dimension(MAX_LEN_STUDENT_RECORD) :: p, q

!        write(device,AFORMAT) '<a name="Update ELECTIVE"></a><hr>'
        call make_form_start(device, fnEditCheckList, Student(std)%StdNo)
        write(device,AFORMAT) '<br><b>PLAN OF STUDY update form</b>', &
            '<br><i>Enter or modify contents of the edit boxes below, then click "Update ELECTIVE"</i>'// &
            '<table border="0" width="90%">'
        do tdx=1,CheckList%NumTerms,3
            call rank_to_year_term(tdx, Year, Term)
            write(device,AFORMAT) begintr//'<td colspan="4"><br><b>'//trim(txtYear(Year))// &
                ' Year, First Semester</b> ('//trim(itoa(TermUnits(tdx)))//' units)'//endtd// &
                tdnbspendtd//'<td colspan="4"><br><b>'//trim(txtYear(Year))// &
                ' Year, Second Semester</b> ('//trim(itoa(TermUnits(tdx+1)))//' units)'//endtd//endtr
            m = 0
            n = 0
            p = 0
            q = 0
            do idx=1,CheckList%NSubjects
                rank = CheckList%SubjectTerm(idx)
                if (rank == tdx) then
                    m = m+1
                    p(m) = idx
                else if (rank == tdx+1) then
                    n = n+1
                    q(n) = idx
                end if
            end do ! idx=1,CheckList%NSubjects
            do idx=1,min(m,n)
                write(device,AFORMAT) begintr//begintd//trim(CLExt(p(idx))%Disp_Subject)//endtd//&
                    begintd//trim(CLExt(p(idx))%Disp_Input_Elective)//endtd//&
                    begintd//trim(CLExt(p(idx))%Disp_Grade)//endtd//&
                    begintd//trim(CLExt(p(idx))%Disp_Units)//endtd//tdnbspendtd, &
                    begintd//trim(CLExt(q(idx))%Disp_Subject)//endtd//&
                    begintd//trim(CLExt(q(idx))%Disp_Input_Elective)//endtd//&
                    begintd//trim(CLExt(q(idx))%Disp_Grade)//endtd//&
                    begintd//trim(CLExt(q(idx))%Disp_Units)//endtd//endtr
            end do
            if (m > n) then
                do idx=n+1,m
                    write(device,AFORMAT) begintr//begintd//trim(CLExt(p(idx))%Disp_Subject)//endtd//&
                        begintd//trim(CLExt(p(idx))%Disp_Input_Elective)//endtd//&
                        begintd//trim(CLExt(p(idx))%Disp_Grade)//endtd//&
                        begintd//trim(CLExt(p(idx))%Disp_Units)//endtd//&
                        '<td colspan="5">'//nbsp//endtd//endtr
                end do
            end if
            if (n > m) then
                do idx=m+1,n
                    write(device,AFORMAT) begintr//'<td colspan="5">'//nbsp//endtd// &
                        begintd//trim(CLExt(q(idx))%Disp_Subject)//endtd//&
                        begintd//trim(CLExt(q(idx))%Disp_Input_Elective)//endtd//&
                        begintd//trim(CLExt(q(idx))%Disp_Grade)//endtd//&
                        begintd//trim(CLExt(q(idx))%Disp_Units)//endtd//endtr
                end do
            end if
            ! summer
            m = 0
            p = 0
            do idx=1,CheckList%NSubjects
                rank = CheckList%SubjectTerm(idx)
                if (rank == tdx+2) then
                    m = m+1
                    p(m) = idx
                end if
            end do ! idx=1,CheckList%NSubjects
            if (m > 0) then
                write(device,AFORMAT) begintr//'<td colspan="4"><br><b>'//trim(txtYear(Year))// &
                    ' Year, Summer</b> ('//trim(itoa(TermUnits(tdx+2)))//' units)'//endtd// &
                    '<td colspan="5">'//nbsp//endtd//endtr
                do idx=1,m
                    write(device,AFORMAT) begintr//begintd//trim(CLExt(p(idx))%Disp_Subject)//endtd//&
                        begintd//trim(CLExt(p(idx))%Disp_Input_Elective)//endtd//&
                        begintd//trim(CLExt(p(idx))%Disp_Grade)//endtd//&
                        begintd//trim(CLExt(p(idx))%Disp_Units)//endtd//&
                        '<td colspan="5">'//nbsp//endtd//endtr
                end do
            end if
        end do
        write(device,AFORMAT) '</table>', &
            '<br><input type="submit" name="action" value="Update ELECTIVE"></form><hr>'

    end subroutine substitution_form


    subroutine checklist_page_links(device, first)
        integer, intent (in) :: device
        logical, intent (in), optional :: first
        if (present(first)) then
            write(device,AFORMAT) '<small>'
            if (Advised(targetStudent)%lenSubject>0) write(device,AFORMAT) &
                '[ Revise <a href="#Revise PREDICTION">PREDICTION</a> ]'
            write(device,AFORMAT) &
                '[ Change <a href="#Change GRADE">GRADE</a> ]', &
                '[ Subject <a href="#Subject SUBSTITUTION">SUBSTITUTION</a> ] ', &
                '</small><br><br>'
                !'[ <a href="#ADDITIONAL subject">ADDITIONAL</a> subject ] ', &
        else
            write(device,AFORMAT) '<small>[ <a href="#TOP">CHECKLIST</a> ] '
            if (Advised(targetStudent)%lenSubject>0) write(device,AFORMAT) &
                '[ Revise <a href="#Revise PREDICTION">PREDICTION</a> ]'
            write(device,AFORMAT) &
                '[ Change <a href="#Change GRADE">GRADE</a> ]', &
                '[ Subject <a href="#Subject SUBSTITUTION">SUBSTITUTION</a> ] ', &
                '</small><br><br><br><br>'
                !'[ <a href="#ADDITIONAL subject">ADDITIONAL</a> subject ] ', &
                !'[ Change <a href="#Change CURRICULUM">CURRICULUM</a> ] ', &
                !'[ Update <a href="#Update ELECTIVE">ELECTIVE</a> ] ', &
                !'[ Update <a href="#Update ELECTIVE">ELECTIVE</a> ] ', &
        end if

    end subroutine checklist_page_links


    subroutine checklist_display  (device, std, Advice, MissingPOCW, NRemaining)
        integer, intent (in) :: std, device, MissingPOCW, NRemaining
        type (TYPE_PRE_ENLISTMENT), intent (in) :: Advice
        integer :: idx, tdx, Year, Term, m, n, rank, idxCURR, k, l
        integer, dimension(MAX_LEN_STUDENT_RECORD) :: p, q
        integer:: UnitsPaid, UnitsDropped, UnitsPassed, Standing
        character(len=6) :: tProb

        call html_comment('checklist_display()')

        idxCURR = Student(std)%CurriculumIdx

        k = Curriculum(idxCURR)%NumTerms
        n = SpecifiedUnits(k)
        write(device,AFORMAT) '<b>Units to earn classifications</b>', &
            (': '//nbsp//trim(txtStanding(l))//'<='//trim(itoa(int((0.25*l)*n))), l=1,4)
        write(device,AFORMAT) '<br><b>Units to achieve YEAR</b>', &
            (': '//nbsp//trim(txtYear(l))//'<'//trim(itoa(SpecifiedUnits(3*l))), l=1,(k+2)/3)

        write(device,AFORMAT) '<table border="0" width="100%">'
        do tdx=1,CheckList%NumTerms,3
            call rank_to_year_term(tdx, Year, Term)
            write(device,AFORMAT) begintr//'<td colspan="5"><br><b>'//trim(txtYear(Year))// &
                ' Year, First Semester</b> ('//trim(itoa(TermUnits(tdx)))//' units)'//endtd// &
                tdnbspendtd//'<td colspan="5"><br><b>'//trim(txtYear(Year))// &
                ' Year, Second Semester</b> ('//trim(itoa(TermUnits(tdx+1)))//' units)'//endtd//endtr
            m = 0
            n = 0
            p = 0
            q = 0
            do idx=1,CheckList%NSubjects
                rank = CheckList%SubjectTerm(idx)
                if (rank == tdx) then
                    m = m+1
                    p(m) = idx
                else if (rank == tdx+1) then
                    n = n+1
                    q(n) = idx
                end if
            end do ! idx=1,CheckList%NSubjects
            do idx=1,min(m,n)
                write(device,AFORMAT) &
                    begintr// &
                    begintd//CLExt(p(idx))%Disp_Subject//endtd//&
                    begintd//CLExt(p(idx))%Disp_Comment//endtd//&
                    begintd//CLExt(p(idx))%Disp_Grade//endtd//&
                    begintd//CLExt(p(idx))%Disp_Units//endtd//&
                    begintd//CLExt(p(idx))%Disp_Remarks//endtd//&
                    tdnbspendtd// &
                    begintd//CLExt(q(idx))%Disp_Subject//endtd//&
                    begintd//CLExt(q(idx))%Disp_Comment//endtd//&
                    begintd//CLExt(q(idx))%Disp_Grade//endtd//&
                    begintd//CLExt(q(idx))%Disp_Units//endtd// &
                    begintd//CLExt(q(idx))%Disp_Remarks//endtd//&
                    endtr
            end do
            if (m > n) then
                do idx=n+1,m
                    write(device,AFORMAT) &
                        begintr// &
                        begintd//CLExt(p(idx))%Disp_Subject//endtd//&
                        begintd//CLExt(p(idx))%Disp_Comment//endtd//&
                        begintd//CLExt(p(idx))%Disp_Grade//endtd//&
                        begintd//CLExt(p(idx))%Disp_Units//endtd//&
                        begintd//CLExt(p(idx))%Disp_Remarks//endtd//&
                        '<td colspan="6">'//nbsp//endtd//&
                        endtr
                end do
            end if
            if (n > m) then
                do idx=m+1,n
                    write(device,AFORMAT) &
                        begintr// &
                        '<td colspan="6">'//nbsp//endtd// &
                        begintd//CLExt(q(idx))%Disp_Subject//endtd//&
                        begintd//CLExt(q(idx))%Disp_Comment//endtd//&
                        begintd//CLExt(q(idx))%Disp_Grade//endtd//&
                        begintd//CLExt(q(idx))%Disp_Units//endtd//&
                        begintd//CLExt(q(idx))%Disp_Remarks//endtd//&
                        endtr
                end do
            end if
            ! summer
            m = 0
            p = 0
            do idx=1,CheckList%NSubjects
                rank = CheckList%SubjectTerm(idx)
                if (rank == tdx+2) then
                    m = m+1
                    p(m) = idx
                end if
            end do ! idx=1,CheckList%NSubjects
            if (m > 0) then
                !write(device,AFORMAT) SPACE, SPACE//trim(txtYear(Year))//' Year, Summer'
                write(device,AFORMAT) begintr//'<td colspan="5"><br><b>'//trim(txtYear(Year))// &
                    ' Year, Summer</b> ('//trim(itoa(TermUnits(tdx+2)))//' units)'//endtd// &
                    '<td colspan="6">'//nbsp//endtd//endtr
                do idx=1,m
                    write(device,AFORMAT) &
                        begintr// &
                        begintd//CLExt(p(idx))%Disp_Subject//endtd//&
                        begintd//CLExt(p(idx))%Disp_Comment//endtd//&
                        begintd//CLExt(p(idx))%Disp_Grade//endtd//&
                        begintd//CLExt(p(idx))%Disp_Units//endtd//&
                        begintd//CLExt(p(idx))%Disp_Remarks//endtd//&
                        '<td colspan="6">'//nbsp//endtd//&
                        endtr
                end do
            end if
        end do
        write(device,AFORMAT) '</table>', &
            '<br><b>LEGENDS</b> for grades and remarks:'//nbsp//' * - specify subject/GE/PE 2 activity;'//&
            nbsp//' # - failed, or lapsed 4/INC;'//nbsp//' % - conditional; (NG) - no grade;', &
            '<br>'//nbsp//' PASS - credit earned/exempted;'//nbsp//' REGD - currently registered;', &
            nbsp//' PriN - Nth priority subject;'//nbsp//' AltK - Kth alternate subject'

        call get_scholastic_three_terms (std, cTm3Year, cTm3, UnitsPaid, UnitsDropped, UnitsPassed, Standing)
        write(device,AFORMAT) '<br><br><b>SUMMARY for '//txtSemester(cTm3+6)// &
            ' Semester '//trim(itoa(cTm3Year))//DASH//trim(itoa(cTm3Year+1)), &
            '</b>: Units registered='//trim(itoa(UnitsPaid)), &
            ': '//nbsp//' Dropped='//trim(itoa(UnitsDropped)), &
            ': '//nbsp//' Earned='//trim(itoa(UnitsPassed)), &
            ': '//nbsp//' Scholastic standing='//trim(txtScholastic(Standing))

        call get_scholastic_three_terms (std, cTm2Year, cTm2, UnitsPaid, UnitsDropped, UnitsPassed, Standing)
        write(device,AFORMAT) '<br><b>SUMMARY for '//txtSemester(cTm2+6)// &
            ' Semester '//trim(itoa(cTm2Year))//DASH//trim(itoa(cTm2Year+1)), &
            '</b>: Units registered='//trim(itoa(UnitsPaid)), &
            ': '//nbsp//' Dropped='//trim(itoa(UnitsDropped)), &
            ': '//nbsp//' Earned='//trim(itoa(UnitsPassed)), &
            ': '//nbsp//' Scholastic standing='//trim(txtScholastic(Standing))

        call get_scholastic_three_terms (std, cTm1Year, cTm1, UnitsPaid, UnitsDropped, UnitsPassed, Standing)
        write(device,AFORMAT) '<br><b>SUMMARY for '//txtSemester(cTm1+6)// &
            ' Semester '//trim(itoa(cTm1Year))//DASH//trim(itoa(cTm1Year+1)), &
            '</b>: Units registered='//trim(itoa(UnitsPaid)), &
            ': '//nbsp//' Dropped='//trim(itoa(UnitsDropped)), &
            ': '//nbsp//' Earned='//trim(itoa(UnitsPassed)), &
            ': '//nbsp//' Scholastic standing='//trim(txtScholastic(Standing))

        if (nextTerm/=currentTerm) then

            call get_scholastic_three_terms (std, currentYear, currentTerm, UnitsPaid, UnitsDropped, UnitsPassed, Standing)
            write(device,AFORMAT) '<br><b>SUMMARY for '//txtSemester(currentTerm+6)// &
                ' Semester '//trim(itoa(currentYear))//DASH//trim(itoa(currentYear+1)), &
                '</b>: Units registered='//trim(itoa(UnitsPaid)), &
                ': '//nbsp//' Dropped='//trim(itoa(UnitsDropped)), &
                ': '//nbsp//' Earned='//trim(itoa(UnitsPassed)), &
                ': '//nbsp//' Scholastic standing='//trim(txtScholastic(Standing))
        end if

        if (MissingPOCW>0) then
            write(device,AFORMAT) '<br><br>'//red//'<b>MISSING ELECTIVEs/entries in Plan of Study = '// &
                trim(itoa(MissingPOCW))//'</b>'//black
        end if

        ! extra subjects
        n = 0
        q = 0
        do idx=1,lenTCG
            if (TCG(idx)%Code /= 2 .or. TCG(idx)%Used) cycle
            if (TCG(idx)%Subject /= 0 .and. is_grade_passing(TCG(idx)%Grade)) then
                n = n+1
                q(n) = idx
            end if
        end do
        if (n > 0) then
            write(device,AFORMAT) '<br><br><b>EXTRA subjects</b>, or subjects not specified in Plan Of Study'
            do m=1,n
                write(device,AFORMAT) ' : '//Subject(TCG(q(m))%Subject)%Name//DASH//txtGrade(pGrade(TCG(q(m))%Grade))
            end do
        end if

        if (advisingPeriod) then ! not enlistment period
            write(device,AFORMAT) '<br><br><b>ASSUMPTION at the end of '//txtSemester(currentTerm+6)// &
                ' Semester '//trim(itoa(currentYear))//DASH//trim(itoa(currentYear+1)), &
                '</b>: Units earned='//trim(itoa(Advice%UnitsEarned)), &
                ': '//nbsp//' Classification='//trim(txtStanding(Advice%StdClassification)), &
                ': '//nbsp//' Year in curriculum='//trim(txtYear(Advice%StdYear))
        end if

        select case (Advice%StdPriority)

            case (10)
                write(device,AFORMAT) '<br><br>PROGRAM COMPLETE?'

            case (9)
                write(device,AFORMAT) &
                    '<br><br><b>NO FEASIBLE SUBJECTS?</b> PREREQUISITES NOT SATISFIED or PLAN OF STUDY IS INCOMPLETE', &
                    '<br>Some remaining subject are : <table border="0" width="80%">', &
                    begintr//thalignleft//'Subject'//endth// &
                    thaligncenter//'Credit'//endth// &
                    thalignleft//'Title'//endth// &
                    thalignleft//'Prereq'//endth//endtr
                do l=1,NRemaining
                    tdx = CLExt(l)%PriorityRank
                    idx = CheckList%SubjectIdx(tdx)
                    write(device,AFORMAT) &
                        begintr//begintd//trim(Subject(idx)%Name)//endtd// &
                        tdaligncenter//trim(ftoa(Subject(idx)%Units,1))//endtd// &
                        begintd//trim(Subject(idx)%Title)//endtd// &
                        begintd//trim(text_prerequisite_in_curriculum(idx))//endtd//endtr
                end do
                write(device,AFORMAT) '</table>'

            case (8)
                write(device,AFORMAT) '<br><br>CHECK RECORDS! SUBJECTS ARE FOR A NEW FRESHMAN?'

            case (7)
                write(device,AFORMAT) '<br><br>GRADUATE, DIPLOMA, NON-DEGREE or SPECIAL STUDENT?'

            !case (6)
            !  write(device,AFORMAT) SPACE, ' DISMISSED or PERMANENTLY DISQUALIFIED?'

            case default

!                ! extra subjects
!                n = 0
!                q = 0
!                do idx=1,lenTCG
!                    if (TCG(idx)%Code /= 2 .or. TCG(idx)%Used) cycle
!                    if (TCG(idx)%Subject /= 0 .and. is_grade_passing(TCG(idx)%Grade)) then
!                        n = n+1
!                        q(n) = idx
!                    end if
!                end do
!                if (n > 0) then
!                    write(device,AFORMAT) '<br><br><b>EXTRA subjects</b>, or subjects not specified in Plan Of Study'
!                    do m=1,n
!                        write(device,AFORMAT) ' : '//Subject(TCG(q(m))%Subject)%Name//DASH//txtGrade(pGrade(TCG(q(m))%Grade))
!                    end do
!                    write(device,AFORMAT) '<br>'
!                end if

                write(device,AFORMAT) '<br><br><b>FEASIBLE subjects for '//txtSemester(nextTerm+6)// &
                    ' Term, '//text_school_year(nextYear), &
                    '</b> (ALLOWED load = '//trim(itoa(Advice%AllowedLoad))//') <br><table border="0" width="80%">'
                if (Advice%NPriority>0) then
                    write(device,AFORMAT) &
                        begintr//thaligncenter//'Pri'//endth// &
                        thalignleft//'Contrib'//endth// &
                        thalignleft//'Subject'//endth// &
                        thaligncenter//'Credit'//endth// &
                        thalignleft//'Title'//endth//endtr
                    do l=1,Advice%NPriority
                        tdx = l
                        idx = Advice%Subject(tdx)
                        write(tProb, '(f6.4)') Advice%Contrib(tdx)
                        write(device,AFORMAT) &
                            begintr//tdaligncenter//trim(itoa(tdx))//endtd// &
                            begintd//tProb//endtd// &
                            begintd//trim(Subject(idx)%Name)//endtd// &
                            tdaligncenter//trim(ftoa(Subject(idx)%Units,1))//endtd// &
                            begintd//trim(Subject(idx)%Title)//endtd//endtr
                    end do
                end if

                if (Advice%NAlternates>0) then
                    write(device,AFORMAT) &
                        begintr//thaligncenter//'Alt'//endth// &
                        thalignleft//'Contrib'//endth// &
                        thalignleft//'Subject'//endth// &
                        thaligncenter//'Credit'//endth// &
                        thalignleft//'Title'//endth//endtr
                    do l=1,Advice%NAlternates
                        tdx = Advice%NPriority+l
                        idx = Advice%Subject(tdx)
                        write(tProb, '(f6.4)') Advice%Contrib(tdx)
                        write(device,AFORMAT) &
                            begintr//tdaligncenter//trim(itoa(tdx))//endtd// &
                            begintd//tProb//endtd// &
                            begintd//trim(Subject(idx)%Name)//endtd// &
                            tdaligncenter//trim(ftoa(Subject(idx)%Units,1))//endtd// &
                            begintd//trim(Subject(idx)%Title)//endtd//endtr
                    end do
                end if

                if (Advice%NCurrent>0) then
                    write(device,AFORMAT) &
                        begintr//thaligncenter//'Fail'//endth// &
                        thalignleft//'Contrib'//endth// &
                        thalignleft//'Subject'//endth// &
                        thaligncenter//'Credit'//endth// &
                        thalignleft//'Title'//endth//endtr
                    do l=1,Advice%NCurrent
                        tdx = Advice%NPriority+Advice%NAlternates+l
                        idx = Advice%Subject(tdx)
                        write(tProb, '(f6.4)') Advice%Contrib(tdx)
                        write(device,AFORMAT) &
                            begintr//tdaligncenter//trim(itoa(tdx))//endtd// &
                            begintd//tProb//endtd// &
                            begintd//trim(Subject(idx)%Name)//endtd// &
                            tdaligncenter//trim(ftoa(Subject(idx)%Units,1))//endtd// &
                            begintd//trim(Subject(idx)%Title)//endtd//endtr
                    end do
                end if
                write(device,AFORMAT) '</table> <br> <i>Note:</i> '// &
                    '<b>Pri</b>=<i>Priority</i>, <b>Alt</b>=<i>Alternate</i>, <b>Fail</b>=<i>"if failed"</i>, '// &
                    '<b>Contrib</b>=<i>contribution to demand for subject</i>.'

        end select


    end subroutine checklist_display


end module EditPREDICTIONS
