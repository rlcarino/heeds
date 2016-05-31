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


module EditCHECKLIST

    use EditSTUDENT

    implicit none

contains


    subroutine checklist_edit (device, thisTerm) !, Section, path)
        implicit none
        integer, intent (in) :: device, thisTerm
        !type (TYPE_OFFERED_SUBJECTS), intent(in), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        !type (TYPE_SECTION), intent(in) :: Section(0:)
        !character(len=*), intent(in) :: path

        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject, input_name1, input_name2, input_value, currentSubject, update
        character(len=3*MAX_LEN_SUBJECT_CODE) :: tAction
        character (len=MAX_LEN_TEXT_YEAR) :: tYear
        character (len=MAX_LEN_TEXT_SEMESTER) :: tTerm
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character(len=127) :: TCGline
        character(len=255) :: mesg
        logical :: FlagIsUp, isDirtyPlan, useNextAdvice
        logical :: isDirtyPREDICTIONS

        integer :: crse, year, term, nsubs, nreqs, n_changes
        integer :: crse_required, crse_current, crse_update, rank
        integer :: cdx, idx, jdx, ierr, i, j, k, l
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

        isDirtyPlan = .false. ! no changes yet
        isDirtyPREDICTIONS = .false. ! no changes yet
        useNextAdvice = .false.

        TCGline = SPACE
        mesg = SPACE

        if (.not. isRoleOfficial .and. trim(tAction)=='Change CURRICULUM') then

            ! change to which curriculum?
            call cgi_get_named_string(QUERY_STRING, 'A2', tCurriculum, ierr)
            cdx = index_to_curriculum(tCurriculum)
            !write(*,*) tCurriculum, cdx, Curriculum(cdx)%Code
            if (ierr/=0 .or. cdx<=0) then
                mesg = 'Index to replacement curriculum not valid?'
            else
                Student(targetStudent)%CurriculumIdx = cdx
                StudentInfo%CurriculumIdx = cdx
                call xml_student_info(targetStudent)
                targetCurriculum = cdx
                mesg = trim(tAction)//' to '//Curriculum(cdx)%Code
                useNextAdvice = .true. ! remember next Advice
                isDirtySTUDENTS = .true. ! trigger rewrite of STUDENTS
            end if

        end if


        if (.not. isRoleOfficial .and. trim(tAction)=='ADDITIONAL subject') then

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
                    useNextAdvice = .true. ! remember next Advice

                    input_name1 = 'ADDITIONAL'
                    crse_required = index_to_subject(input_name1)

                    lenTCG = lenTCG+1
                    TCG(lenTCG)%Code = 1
                    TCG(lenTCG)%Used = .false.
                    TCG(lenTCG)%Year = year
                    TCG(lenTCG)%Term = term
                    TCG(lenTCG)%Reqd(0) = 1
                    TCG(lenTCG)%Reqd(1) = crse_required
                    TCG(lenTCG)%Subst(0) = 1
                    TCG(lenTCG)%Subst(1) = crse

                end if
            end if

        end if

        if (.not. isRoleOfficial .and. trim(tAction)=='Cancel ADDITIONAL') then

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
                    do k=1,lenTCG
                        if (TCG(k)%Reqd(1)==crse_required .and. &
                            TCG(k)%Subst(1)==crse) then
                            FlagIsUp = .true. ! found
                            exit
                        end if
                    end do
                    if (FlagIsUp) then ! shift-erase
                        do i=k+1,lenTCG
                            TCG(i-1) = TCG(i)
                        end do
                        ! re-initialize previous last location
                        TCG(lenTCG) = TCG(lenTCG+1)
                        lenTCG = lenTCG - 1

                        isDirtyPlan = .true.
                        useNextAdvice = .true. ! remember next Advice
                        mesg = tAction//tSubject

                    end if

                end if
            end if

        end if

        if (.not. isRoleOfficial .and. trim(tAction)=='Add SUBSTITUTION') then

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
                    mesg = trim(tAction)//' : '//tSubject//'- Substitute subject code not valid?'
                    nsubs = 0
                    exit
                else
                    if (isRoleStudent .and. tSubject(1:3)/='PE ') then
                        mesg = trim(tAction)//' : '//tSubject//'- students allowed to substitute PE only.'
                        nsubs = 0
                        exit
                    end if
                    j = index_of_subject_in_curriculum(Curriculum(targetCurriculum), crse)
                    if (j>0) then ! a required subject
                        mesg = trim(tAction)//' : '//tSubject//'- substitute subject already in curriculum?'
                        nsubs = 0
                        exit
                    else
                        TCGline = COMMA//trim(tSubject)//TCGline
                        nsubs = nsubs + 1
                        TCG(lenTCG+1)%Subst(0) = nsubs
                        TCG(lenTCG+1)%Subst(nsubs) = crse
                    end if
                end if
            end do

            ! get required subjects
            do i=5,1,-1
                call cgi_get_named_string(QUERY_STRING, 'req'//trim(itoa(i)), tSubject, ierr)
                if (ierr/=0 .or. tSubject==SPACE) cycle
                crse = index_to_subject(tSubject)
                if (crse <= 0) then
                    mesg = trim(tAction)//' : '//tSubject//'- Required subject code not valid?'
                    nreqs = 0
                    exit
                else
                    if (isRoleStudent .and. tSubject(1:4)/='PE 1') then
                        mesg = trim(tAction)//' : '//tSubject//'- students allowed to substitute PE only.'
                        nreqs = 0
                        exit
                    end if
                    j = index_of_subject_in_curriculum(Curriculum(targetCurriculum), crse)
                    if (j>0) then ! a required subject
                        ! check if already required in another substitution rule
                        FlagIsUp = .false.
                        do k=1,lenTCG
                            if (TCG(k)%Code/=1) cycle
                            do l=1,TCG(k)%Reqd(0)
                                if (crse==TCG(k)%Reqd(l)) then
                                    FlagIsUp = .true.
                                    exit
                                end if
                            end do
                            if (FlagIsUp) exit
                        end do
                        if (.not. FlagIsUp) then
                            TCGline = COMMA//trim(tSubject)//TCGline
                            nreqs = nreqs + 1

                            rank = Curriculum(targetCurriculum)%SubjectTerm(j)
                            call rank_to_year_term (rank, year, term)
                            TCG(lenTCG+1)%Year = year
                            TCG(lenTCG+1)%Term = term
                            TCG(lenTCG+1)%Reqd(0) = nreqs
                            TCG(lenTCG+1)%Reqd(nreqs) = crse

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
                lenTCG = lenTCG + 1
                TCG(lenTCG)%Code = 1
                useNextAdvice = .true. ! remember next Advice
                if (nreqs>1) then ! group substitution; no year/term
                    TCG(lenTCG)%Year = 0
                    TCG(lenTCG)%Term = 0
                end if

                isDirtyPlan = .true.
            end if

        end if

        if (.not. isRoleOfficial .and. trim(tAction)=='Cancel SUBSTITUTION') then

            call cgi_get_named_string(QUERY_STRING, 'subject', tSubject, crse)

            if (crse/=0) then ! a name was not specified?
                mesg = trim(tAction)//' : substitute subject not spelled correctly?'

            else if (tSubject==SPACE) then ! a value was not specified?
                mesg = trim(tAction)//' : substitute subject not specified?'

            else ! some subject was specified
                crse = index_to_subject(tSubject)
                if (crse<=0) then ! not a named subject
                    mesg = trim(tAction)//' : subject not valid?'
                else
                    if (isRoleStudent .and. tSubject(1:3)/='PE ') then
                        mesg = trim(tAction)//' : '//tSubject//'- students allowed to cancel PE substitutions only.'
                    else
                        mesg = trim(tAction)//' : rule not found in checklist - '//tSubject
                        ! check Reqd() for specified SUBSTITUTION
                        idx = 0
                        do k=1,lenTCG
                            if (TCG(k)%Code/=1) cycle
                            do i=1,TCG(k)%Subst(0)
                                if (TCG(k)%Subst(i)==crse) then
                                    isDirtyPlan = .true.
                                    idx = k
                                    exit
                                end if
                            end do
                            if (isDirtyPlan) exit
                        end do
                    end if
                    if (isDirtyPlan) then ! required subject found
                        ! shift substitutions down
                        do k=idx+1,lenTCG
                            TCG(k-1) = TCG(k)
                        end do
                        ! re-initialize previous last location
                        TCG(lenTCG) = TCG(lenTCG+1)
                        lenTCG = lenTCG - 1 ! decrease counter
                        mesg = trim(tAction)//' for '//tSubject
                        useNextAdvice = .true. ! remember next Advice
                    end if
                end if
            end if

        end if

        if (.not. isRoleOfficial .and. trim(tAction)=='Update PLAN') then

            n_changes = 0
            do idx=1,Curriculum(targetCurriculum)%NSubjects
                crse_required = Curriculum(targetCurriculum)%SubjectIdx(idx)
                if (crse_required > 0) cycle ! named subject
                rank = Curriculum(targetCurriculum)%SubjectTerm(idx)

                input_name1 = fn_blank_to_underscore(Subject(crse_required)%Name)
                input_name1 = trim(input_name1)//DASH//trim(itoa(rank))//':'

                call html_comment('Looking for '//input_name1)

                call cgi_get_wild_name_value(QUERY_STRING, input_name1, input_name2, input_value, ierr)
                if (ierr/=0) cycle ! not found

                call rank_to_year_term(rank, Year, Term)
                currentSubject = fn_underscore_to_blank(input_name2)
                update = fn_underscore_to_blank(input_value)

                call html_comment('  Found : '//Subject(crse_required)%Name//currentSubject//update)

                crse_current = index_to_subject(currentSubject)

                if (update/=SPACE) then ! something in update

                    crse_update = index_to_subject(update)
                    if (crse_update <= 0) then
                        call html_comment(trim(tAction)//' : '//update//'- code not valid?')
                        cycle
                    end if

                    jdx = index_of_subject_in_curriculum(Curriculum(targetCurriculum), crse_update)
                    if (jdx>0) then ! a required subject
                        call html_comment(trim(tAction)//' : '//update//'- already required?')
                        cycle
                    end if

                    if (currentSubject == update) then ! no change
                        call html_comment(trim(tAction)//' : No change to '//currentSubject)
                        cycle
                    end if

                    ! make change
                    if (crse_current>0) then ! previously specified
                        FlagIsUp = .true.
                        do k=1,lenTCG
                            if (TCG(k)%Code/=1) cycle
                            if (TCG(k)%Reqd(1)==crse_required .and. &
                                TCG(k)%Subst(1)==crse_current) then
                                TCG(k)%Subst(1) = crse_update
                                FlagIsUp = .false.
                                mesg = trim(mesg)//', '//trim(Subject(crse_required)%Name)//'='//trim(Subject(crse_update)%Name)
                                exit
                            end if
                        end do

                        if (FlagIsUp) then ! make new entry
                            lenTCG = lenTCG + 1

                            call rank_to_year_term (rank, TCG(lenTCG)%Year, TCG(lenTCG)%Term)
                            TCG(lenTCG)%Code = 1
                            TCG(lenTCG)%Reqd(0) = 1
                            TCG(lenTCG)%Reqd(1) = crse_required
                            TCG(lenTCG)%Subst(0) = 1
                            TCG(lenTCG)%Subst(1) = crse_update
                            mesg = trim(mesg)//', '//trim(Subject(crse_required)%Name)//'='//trim(Subject(crse_update)%Name)

                        end if

                    else ! make new entry in TCG
                        lenTCG = lenTCG + 1
                        call rank_to_year_term (rank, TCG(lenTCG)%Year, TCG(lenTCG)%Term)
                        TCG(lenTCG)%Code = 1
                        TCG(lenTCG)%Reqd(0) = 1
                        TCG(lenTCG)%Reqd(1) = crse_required
                        TCG(lenTCG)%Subst(0) = 1
                        TCG(lenTCG)%Subst(1) = crse_update
                        mesg = trim(mesg)//', '//trim(Subject(crse_required)%Name)//'='//trim(Subject(crse_update)%Name)

                    end if
                    n_changes = n_changes + 1

                else ! update is blank? remove current elective
                    if (crse_current>0) then ! previously specified

                        FlagIsUp = .false.
                        do k=1,lenTCG
                            if (TCG(k)%Code/=1) cycle
                            if (TCG(k)%Reqd(1)==crse_required .and. &
                                TCG(k)%Subst(1)==crse_current) then
                                FlagIsUp = .true.
                                exit
                            end if
                        end do
                        if (FlagIsUp) then
                            do i=k+1,lenTCG
                                TCG(i-1) = TCG(i)
                            end do
                            ! re-initialize previous last location
                            TCG(lenTCG) = TCG(lenTCG+1)
                            lenTCG = lenTCG - 1
                            n_changes = n_changes + 1
                        end if

                    end if
                end if

            end do

            if (n_changes==0) then
                mesg = trim(tAction)//' : Nothing to update?'
            else
                ! copy to Student(std)
                mesg = trim(tAction)//mesg
                isDirtyPlan = .true.
                useNextAdvice = .true. ! remember next Advice
            end if

        end if


        if (.not. isRoleOfficial .and. trim(tAction)=='As ADVISED') then

            call collect_advice(Advice, n_changes, mesg)
            NumEnlistment(thisTerm) = NumEnlistment(thisTerm)- Student(targetStudent)%Enlistment(thisTerm)%lenSubject + &
                Advice%lenSubject
            Student(targetStudent)%Enlistment(thisTerm) = Advice
            isDirtyPREDICTIONS = .true.
            mesg = trim(tAction)//' : '//trim(itoa(Advice%lenSubject))//' entries.'

        end if


        if (.not. isRoleOfficial .and. trim(tAction)=='Update ADVISED') then

            call collect_advice(Advice, n_changes, mesg)
            if (n_changes>0) then
                mesg = trim(tAction)//mesg
                NumEnlistment(thisTerm) = NumEnlistment(thisTerm)- Student(targetStudent)%Enlistment(thisTerm)%lenSubject + &
                    Advice%lenSubject
                if (Advice%statusAdvising<ADVISING_REGULAR) then
                    Advice%statusAdvising = ADVISING_IRREGULAR
                end if
                Student(targetStudent)%Enlistment(thisTerm) = Advice
                isDirtyPREDICTIONS = .true.
            elseif (Advice%AllowedLoad /= Student(targetStudent)%Enlistment(thisTerm)%AllowedLoad ) then
                mesg = trim(tAction)//' load '//mesg
                Student(targetStudent)%Enlistment(thisTerm)%AllowedLoad = Advice%AllowedLoad
                if (Student(targetStudent)%Enlistment(thisTerm)%statusAdvising<ADVISING_REGULAR) then
                    Student(targetStudent)%Enlistment(thisTerm)%statusAdvising = ADVISING_IRREGULAR
                end if
                isDirtyPREDICTIONS = .true.
            elseif (isRole_adviser_of_student(targetStudent, orHigherUp) .and. Advice%NPriority>0) then
                NumEnlistment(thisTerm) = NumEnlistment(thisTerm)- Student(targetStudent)%Enlistment(thisTerm)%lenSubject + &
                    Advice%lenSubject
                if (Advice%statusAdvising<ADVISING_REGULAR) then
                    Advice%statusAdvising = ADVISING_IRREGULAR
                    Student(targetStudent)%Enlistment(thisTerm) = Advice
                end if
            end if

        end if


        if (.not. isRoleOfficial .and. trim(tAction)=='EXCLUDE Student') then

            call initialize_pre_enlistment(Student(targetStudent)%Enlistment(thisTerm))
            Student(targetStudent)%Enlistment(thisTerm)%statusAdvising = ADVISING_EXCLUDED
            Student(targetStudent)%Enlistment(thisTerm)%levelYear = YEAR_NOT_SET
            mesg = 'Deleted all advised subjects for '//txtSemester(thisTerm+3)//termQualifier(thisTerm+3)
            isDirtyPREDICTIONS = .true.

        end if

        if (trim(tAction)=='REINSTATE Student' .and. .not. isRoleOfficial) then

            useNextAdvice = .true. ! remember next Advice
            mesg = 'Reinstated student for '//trim(txtSemester(thisTerm+3)//termQualifier(thisTerm+3))

        end if

        if (len_trim(tAction)>0 .and. isRoleOfficial) then
            mesg = '"'//trim(tAction)//'" failed. '//sorryMessageOfficial
        end if

        if (isDirtyPlan) then
            call xml_write_substitutions(targetStudent)
            call log_student_record_change(targetStudent, trim(mesg) )
        end if

        if (isDirtyPREDICTIONS) then
            call student_details_write(unitXML, dirSTUDENTS, targetStudent)
        end if

        call checklist_write_menu (device, thisTerm, mesg, useNextAdvice)

    end subroutine checklist_edit


    subroutine checklist_write_menu (device, thisTerm, mesg, useAdvice)
        integer, intent (in) :: device, thisTerm
        logical, intent (in) :: useAdvice
        character(len=*), intent (in) :: mesg

        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character(len=6) :: tProb
        character(len=10) :: helpFile, disabled
        character(len=127) :: advising_comment
        type (TYPE_PRE_ENLISTMENT) :: Advice, prevAdvice
        integer :: idxCurr, j, k, l, lenSubject, notSpecified, collegeOfStudent

        tStdNo = Student(targetStudent)%StdNo
        call html_comment('checklist_write_menu()')

        ! Plan of Study
        idxCurr = Student(targetStudent)%CurriculumIdx
        collegeOfStudent = Curriculum(idxCURR)%CollegeIdx

        ! how many entries in plan for this curriculum?
        notSpecified = 0
        do k=1,Curriculum(idxCurr)%NSubjects
            if (Curriculum(idxCurr)%SubjectIdx(k)<0) then
                notSpecified = notSpecified + 1
            end if
        end do

        ! which help file to target
        if (College(collegeOfStudent)%isAllowed(ToEnlistStudents,thisTerm)) then
            helpFile = 'Enlistment'
        else
            helpFile = 'Checklists'
        end if
        call html_write_header(device, tStdNo//nbsp// &
            trim(Student(targetStudent)%Name)// &
            trim(make_href(fnCurriculum, Curriculum(idxCurr)%Code, A1=Curriculum(idxCurr)%Code, &
            pre=b_small//' (', post=' ) '))// &
            nbsp//' <a target="0" href="/Checklists.html">Help</a>'//e_small, mesg)

        ! change curriculum form
        if ( isRole_admin_of_college(collegeOfStudent) .or. ( isRole_adviser_of_student(targetStudent, orHigherUp) .and. &
            College(collegeOfStudent)%isAllowed(ToEditCHECKLISTS,thisTerm) ) ) then

            call make_form_start(device, fnEditCheckList, tStdNo, A9=thisTerm)
            write(device,AFORMAT) '<select name="A2">'
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
            write(device,AFORMAT) '</select>', &
                nbsp//nbsp//'<input type="submit" name="action" value="Change CURRICULUM">'//e_form

        else

            write(device,AFORMAT) trim(text_curriculum_info(idxCurr))//linebreak

        end if

        ! run advising algorithm
        call advise_student (targetStudent, thisTerm, Advice, advising_comment)
        if (len_trim(advising_comment)>0) then
            write(device,AFORMAT) b_bold//red//trim(advising_comment)//e_color//e_bold//linebreak//linebreak
        end if

        ! use Advice?
        if (useAdvice) then
            NumEnlistment(thisTerm) = NumEnlistment(thisTerm) - Student(targetStudent)%Enlistment(thisTerm)%lenSubject + &
                Advice%lenSubject
            Student(targetStudent)%Enlistment(thisTerm) = Advice

        elseif (Student(targetStudent)%Enlistment(thisTerm)%levelYear==YEAR_NOT_SET) then ! manually advised after reset of demand for subjects
            Student(targetStudent)%Enlistment(thisTerm)%UnitsEarned = Advice%UnitsEarned
            Student(targetStudent)%Enlistment(thisTerm)%levelClassification = Advice%levelClassification
            Student(targetStudent)%Enlistment(thisTerm)%levelYear = Advice%levelYear
            Student(targetStudent)%Enlistment(thisTerm)%levelPriority = Advice%levelPriority
            Student(targetStudent)%Enlistment(thisTerm)%statusScholastic = Advice%statusScholastic
            Student(targetStudent)%Enlistment(thisTerm)%codeNSTP = Advice%codeNSTP
            Student(targetStudent)%Enlistment(thisTerm)%codeConditional = Advice%codeConditional
            Student(targetStudent)%Enlistment(thisTerm)%NRemaining = Advice%NRemaining
            Student(targetStudent)%Enlistment(thisTerm)%MissingPOCW = Advice%MissingPOCW
            Student(targetStudent)%Enlistment(thisTerm)%AllowedLoad = 0.0
            do l=1,Student(targetStudent)%Enlistment(thisTerm)%lenSubject
                Student(targetStudent)%Enlistment(thisTerm)%AllowedLoad = &
                    Student(targetStudent)%Enlistment(thisTerm)%AllowedLoad + &
                    Subject(Student(targetStudent)%Enlistment(thisTerm)%Subject(l))%Units
            end do

        end if

        ! display checklist
        call checklist_display (device, targetStudent, Advice)

        ! non-editors exit here
        if (trim(USERNAME)==trim(tStdNo)) then ! student view
        elseif (isRole_adviser_of_student(targetStudent)) then ! adviser view
        elseif (isRole_admin_of_college(collegeOfStudent)) then ! Registrar/staff view
        else
            write(device,AFORMAT) horizontal
            return
        end if

        ! alert on possible actions, depending on enlistment status that was previously set (not Advised%statusAdvising)
        call make_form_start(device, fnEditCheckList, A1=tStdNo, A9=thisTerm)
        write(device,AFORMAT) b_para, &
            b_bold//'ADVISING STATUS'//e_bold//' is '''// &
            txtStatusCode(Student(targetStudent)%Enlistment(thisTerm)%statusAdvising)//''' ('// &
            trim(descStatusCode(Student(targetStudent)%Enlistment(thisTerm)%statusAdvising))//')'//DOT, &
            ' If enabled, possible actions are:', '<ul>', b_item

        select case (Student(targetStudent)%Enlistment(thisTerm)%statusAdvising)

            case (ADVISING_NEEDED) ! Not advised yet
                if (trim(USERNAME)==trim(tStdNo)) then ! student view
                    write(device,AFORMAT) &
                        'Enter specific PE subject below for remaining PE 11/PE 12/PE 13/PE 14', e_item, b_item, &
                        'See your adviser for other subjects'
                elseif (isRole_adviser_of_student(targetStudent)) then ! adviser view
                    write(device,AFORMAT) &
                        '''Use FEASIBLE subjects As ADVISED subjects''', e_item, b_item, &
                        '''Update ADVISED Subjects'''
                elseif (isRole_admin_of_college(collegeOfStudent)) then ! Registrar/staff view
                    write(device,AFORMAT) &
                        '''Use FEASIBLE subjects As ADVISED subjects''', e_item, b_item, &
                        '''Update ADVISED Subjects''', e_item, b_item, &
                        'Run ''Auto-advise students''', e_item, b_item, &
                        '''EXCLUDE Student'''
                        !'<input type="submit" name="action" value="EXCLUDE Student">'
                end if

            case (ADVISING_FINISHED) ! No remaining subjects
                write(device,AFORMAT) 'Celebrate?'

            case (ADVISING_EXCLUDED) ! Excluded
                if (trim(USERNAME)==trim(tStdNo)) then ! student view
                    write(device,AFORMAT) 'See the Registrar to be included as a current student'
                else if (isRole_adviser_of_student(targetStudent)) then ! adviser view
                    write(device,AFORMAT) 'Advise student to see the Registrar to be included as a current student'
                elseif (isRole_admin_of_college(collegeOfStudent)) then ! Registrar/staff view
                    write(device,AFORMAT) '<input type="submit" name="action" value="REINSTATE Student">'
                end if

            case (ENLISTMENT_LOCKED) ! Schedule printed and locked by Registrar via 'LOCK'
                if (trim(USERNAME)==trim(tStdNo) .or. isRole_adviser_of_student(targetStudent)) then ! student view or adviser view
                    write(device,AFORMAT) 'See the Registrar if changes to ADVISED subjects are needed'
                elseif (isRole_admin_of_college(collegeOfStudent)) then ! Registrar/staff view
                    write(device,AFORMAT) &
                        ''''//trim(txtSemester(thisTerm+6))//''' for enlistment options', e_item, b_item, &
                        !' <input type="submit" name="action" value="UNLOCK Schedule">  or '// &
                        !nbsp//nbsp//' <input type="submit" name="action" value="PAID"> or '// &
                        '''EXCLUDE Student'''
                        !' <input type="submit" name="action" value="EXCLUDE Student">'
                end if

            case (ENLISTMENT_PAID) ! Partial or full payment made
                if (trim(USERNAME)==trim(tStdNo) .or. isRole_adviser_of_student(targetStudent)) then ! student view or adviser view
                    write(device,AFORMAT) 'See the Registrar if changes to ADVISED subjects are needed'
                elseif (isRole_admin_of_college(collegeOfStudent)) then ! Registrar/staff view
                    write(device,AFORMAT) &
                        ''''//trim(txtSemester(thisTerm+6))//''' for enlistment options', e_item, b_item, &
                        !' <input type="submit" name="action" value="UNLOCK Schedule">  or '// &
                        '''EXCLUDE Student'''
                        !' <input type="submit" name="action" value="EXCLUDE Student">'
                end if

            case (ADVISING_NO_SUBJECTS)
                ! = 0, & ! No subject
                if (trim(USERNAME)==trim(tStdNo)) then ! student view
                    write(device,AFORMAT) &
                        'Enter specific PE subject below for remaining PE 11/PE 12/PE 13/PE 14', e_item, b_item, &
                        'See your adviser for other subjects'
                elseif (isRole_adviser_of_student(targetStudent)) then ! adviser view
                    write(device,AFORMAT) &
                        '''Use FEASIBLE subjects As ADVISED subjects''', e_item, b_item, &
                        '''Update ADVISED Subjects'''
                elseif (isRole_admin_of_college(collegeOfStudent)) then ! Registrar/staff view
                    write(device,AFORMAT) &
                        '''Use FEASIBLE subjects As ADVISED subjects''', e_item, b_item, &
                        '''Update ADVISED Subjects''', e_item, b_item, &
                        'Run ''Auto-advise students''', e_item, b_item, &
                        '''EXCLUDE Student'''
                        !'<input type="submit" name="action" value="EXCLUDE Student">'
                end if

            case(ADVISING_REGULAR, ADVISING_IRREGULAR)
                ! Auto-advised by Registrar via 'Needs Analysis'

                if (trim(USERNAME)==trim(tStdNo)) then ! student view
                    write(device,AFORMAT) &
                        'Enter specific PE subject below for remaining PE 11/PE 12/PE 13/PE 14', e_item, b_item, &
                        'See your adviser to change the list of FEASIBLE subjects', e_item, b_item, &
                        ''''//trim(txtSemester(thisTerm+6))//''' for manual enlistment options'
                elseif (isRole_adviser_of_student(targetStudent)) then ! adviser view
                    write(device,AFORMAT) &
                        '''Use FEASIBLE subjects As ADVISED subjects''', e_item, b_item, &
                        '''Update ADVISED Subjects''', e_item, b_item, &
                        '''Find block''', e_item, b_item, &
                        ''''//trim(txtSemester(thisTerm+6))//''' for manual enlistment options'
                elseif (isRole_admin_of_college(collegeOfStudent)) then ! Registrar/staff view
                    write(device,AFORMAT) &
                        '''Use FEASIBLE subjects As ADVISED subjects''', e_item, b_item, &
                        '''Update ADVISED Subjects''', e_item, b_item, &
                        '''Find block''', e_item, b_item, &
                        ''''//trim(txtSemester(thisTerm+6))//''' for manual enlistment options', e_item, b_item, &
                        'Run ''Auto-preenlist into blocks''', e_item, b_item, &
                        '''EXCLUDE Student'''
                        !'<input type="submit" name="action" value="EXCLUDE Student">'
                end if

            case(ENLISTMENT_AUTOMATIC, ENLISTMENT_MANUAL, ENLISTMENT_CONFIRMED)
                ! Auto-enlisted by Registrar via 'Preenlist'
                ! Enlisted by adviser, or self-enlisted
                ! Schedule confirmed by student
                if (trim(USERNAME)==tStdNo) then ! student view
                    write(device,AFORMAT) &
                        'Enter specific PE subject below for remaining PE 11/PE 12/PE 13/PE 14', e_item, b_item, &
                        'See your adviser to change the list of FEASIBLE subjects', e_item, b_item, &
                        ''''//trim(txtSemester(thisTerm+6))//''' for manual enlistment options'
                elseif (isRole_adviser_of_student(targetStudent)) then ! adviser view
                    write(device,AFORMAT) &
                        '''Use FEASIBLE subjects As ADVISED subjects''', e_item, b_item, &
                        '''Update ADVISED Subjects''', e_item, b_item, &
                        ''''//trim(txtSemester(thisTerm+6))//''' for manual enlistment options'
                elseif (isRole_admin_of_college(collegeOfStudent)) then ! Registrar/staff view
                    write(device,AFORMAT) &
                        '''Use FEASIBLE subjects As ADVISED subjects''', e_item, b_item, &
                        '''Update ADVISED Subjects''', e_item, b_item, &
                        ''''//trim(txtSemester(thisTerm+6))//''' for enlistment options', e_item, b_item, &
                        '''EXCLUDE Student'''
                        !'<input type="submit" name="action" value="EXCLUDE Student">'
                end if


            case default
                write(device,AFORMAT) '(List possible actions)'


        end select
        write(device,AFORMAT) e_item, '</ul>', e_para, e_form

        if ( Student(targetStudent)%Enlistment(thisTerm)%statusAdvising==ADVISING_EXCLUDED ) then
            write(device,AFORMAT) horizontal
            return
        end if

#if defined UPLB
#else
        !allow student to specify PE
        if (trim(USERNAME)==tStdNo) then

            write(device,AFORMAT) '<a name="Approved SUBSTITUTIONS"></a>'//horizontal, b_para, &
                b_bold//'SUBSTITUTIONS for PE 11/12/13/14'//e_bold// &
                    '. Credit will be earned for Required PE 11/12/13/14 if Substitute is passed.'//linebreak// &
                '<table border="0" width="50%">'
            j = 0
            PEsubst: do l=1,lenTCG
                if (TCG(l)%Code/=1) cycle ! not plan of study
                advising_comment = 'Required (PE 11/12/13/14):'
                do k=1,TCG(l)%Reqd(0)
                    if (Subject(TCG(l)%Reqd(k))%Name(1:3)/='PE ') cycle PEsubst
                    advising_comment = trim(advising_comment)//SPACE//Subject(TCG(l)%Reqd(k))%Name
                    j = j+1
                end do
                write(device,AFORMAT) b_tr//b_td//trim(advising_comment)//e_td
                advising_comment = 'Substitute (PE A/B/C/etc):'
                do k=1,TCG(l)%Subst(0)
                    advising_comment = trim(advising_comment)//SPACE//Subject(TCG(l)%Subst(k))%Name
                end do
                write(device,AFORMAT) b_td//trim(advising_comment)//e_td//e_tr
            end do PEsubst
            if (j==0) write(device,AFORMAT) b_tr//b_td//JUSTNONE//e_td//e_tr
            write(device,AFORMAT) e_table, e_para

            if (Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment<ENLISTMENT_LOCKED ) then

            if (j>0) then
                call make_form_start(device, fnEditCheckList, tStdNo, A9=thisTerm)
                write(device,AFORMAT) b_para, &
                    b_bold//'Cancel SUBSTITUTION'//e_bold//': Enter Substitute PE.'//linebreak, &
                    '<table border="0" width="33%">'//b_tr// &
                    b_td//'Substitute :'//e_td// &
                    b_td//'<input type="text" size="15" name="subject" value="">'//e_td// &
                    e_tr//e_table, &
                    '<input type="submit" name="action" value="Cancel SUBSTITUTION">', &
                    e_form, e_para
            end if

            ! subject substitution
            call make_form_start(device, fnEditCheckList, tStdNo, A9=thisTerm)
            write(device,AFORMAT) b_para, &
                b_bold//'Add SUBSTITUTION'//e_bold// &
                ': Enter a Required PE in the curriculum and Substitute/desired PE activity.'//linebreak// &
                '<table border="0" width="33%">'// &
                b_tr//b_td//'Required :'//e_td//b_td// &
                    '<input type="text" size="15" name="req1" value="">'//e_td//e_tr, &
                b_tr//b_td//'Substitute :'//e_td//b_td// &
                    '<input type="text" size="15" name="sub1" value="">'//e_td//e_tr, &
                e_table, &
                '<input type="submit" name="action" value="Add SUBSTITUTION">', &
                e_form, e_para

            end if

        end if
#endif

!        ! Auto-advise
!        select case (Advice%levelPriority)
!
!            case (10)
!                write(device,AFORMAT) horizontal, b_para, 'PROGRAM COMPLETE?', e_para
!
!            case (9)
!                write(device,AFORMAT) &
!                    horizontal, b_para, b_bold//'NO FEASIBLE SUBJECTS?'//e_bold// &
!                    ' SUBJECTS NOT OFFERED or PREREQUISITES NOT SATISFIED or PLAN OF STUDY IS INCOMPLETE', &
!                    linebreak//'Some remaining subject are : <table border="0" width="100%">', &
!                    b_tr//b_thal//'Subject'//e_th// &
!                    b_thac//'Credit'//e_th// &
!                    b_thal//'Title'//e_th// &
!                    b_thal//'Prereq'//e_th//e_tr
!                do l=1,NRemaining
!                    k = CLExt(l)%PriorityRank
!                    j = CheckList%SubjectIdx(k)
!                    write(device,AFORMAT) &
!                        b_tr//b_td//trim(Subject(j)%Name)//e_td// &
!                        b_tdac//trim(ftoa(Subject(j)%Units,1))//e_td// &
!                        b_td//trim(Subject(j)%Title)//e_td// &
!                        b_td//trim(text_prerequisite_in_curriculum(j))//e_td//e_tr
!                end do
!                write(device,AFORMAT) e_table, e_para
!
!            case (8)
!                write(device,AFORMAT) horizontal, b_para, 'CHECK RECORDS! SUBJECTS ARE FOR A NEW FRESHMAN?', e_para
!
!            case (7)
!                write(device,AFORMAT) horizontal, b_para, 'GRADUATE, DIPLOMA, NON-DEGREE or SPECIAL STUDENT?', e_para
!
!            case (6)
!              write(device,AFORMAT) horizontal, b_para, ' DISMISSED or PERMANENTLY DISQUALIFIED?', e_para
!
!            case default

        lenSubject = Advice%NPriority+Advice%NAlternates+Advice%NCurrent
        if ( lenSubject>0) then

                write(device,AFORMAT) b_para, '<a name="FEASIBLE SUBJECTS"></a>'

                call checklist_links(device)

                write(device,AFORMAT) b_bold//'FEASIBLE subjects for '// &
                    txtSemester(thisTerm+3)//termQualifier(thisTerm+3), &
                    e_bold//' (ALLOWED load = '//trim(ftoa(Advice%AllowedLoad,1))//') ', linebreak, &
                        '<table border="0" width="100%">'
                if (Advice%NPriority>0) then
                    write(device,AFORMAT) &
                        b_tr//b_thac//'Pri'//e_th// &
                        b_thal//'Contrib'//e_th// &
                        b_thal//'Subject'//e_th// &
                        b_thac//'Credit'//e_th// &
                        b_thal//'Title'//e_th//e_tr
                    do l=1,Advice%NPriority
                        k = l
                        j = Advice%Subject(k)
                        write(tProb, '(f6.4)') Advice%Contrib(k)
                        write(device,AFORMAT) &
                            b_tr//b_tdac//trim(itoa(k))//e_td// &
                            b_td//tProb//e_td// &
                            b_td//trim(Subject(j)%Name)//e_td// &
                            b_tdac//trim(ftoa(Subject(j)%Units,1))//e_td// &
                            b_td//trim(Subject(j)%Title)//e_td//e_tr
                    end do
                end if

                if (Advice%NAlternates>0) then
                    write(device,AFORMAT) &
                        b_tr//b_thac//'Alt'//e_th// &
                        b_thal//'Contrib'//e_th// &
                        b_thal//'Subject'//e_th// &
                        b_thac//'Credit'//e_th// &
                        b_thal//'Title'//e_th//e_tr
                    do l=1,Advice%NAlternates
                        k = Advice%NPriority+l
                        j = Advice%Subject(k)
                        write(tProb, '(f6.4)') Advice%Contrib(k)
                        write(device,AFORMAT) &
                            b_tr//b_tdac//trim(itoa(k))//e_td// &
                            b_td//tProb//e_td// &
                            b_td//trim(Subject(j)%Name)//e_td// &
                            b_tdac//trim(ftoa(Subject(j)%Units,1))//e_td// &
                            b_td//trim(Subject(j)%Title)//e_td//e_tr
                    end do
                end if

                if (Advice%NCurrent>0) then
                    write(device,AFORMAT) &
                        b_tr//b_thac//'Fail'//e_th// &
                        b_thal//'Contrib'//e_th// &
                        b_thal//'Subject'//e_th// &
                        b_thac//'Credit'//e_th// &
                        b_thal//'Title'//e_th//e_tr
                    do l=1,Advice%NCurrent
                        k = Advice%NPriority+Advice%NAlternates+l
                        j = Advice%Subject(k)
                        write(tProb, '(f6.4)') Advice%Contrib(k)
                        write(device,AFORMAT) &
                            b_tr//b_tdac//trim(itoa(k))//e_td// &
                            b_td//tProb//e_td// &
                            b_td//trim(Subject(j)%Name)//e_td// &
                            b_tdac//trim(ftoa(Subject(j)%Units,1))//e_td// &
                            b_td//trim(Subject(j)%Title)//e_td//e_tr
                    end do
                end if
                write(device,AFORMAT) e_table, linebreak, &
                    b_italic//'Note:'//e_italic, &
                    b_bold//'Pri'//e_bold//'='//b_italic//'Priority'//e_italic//',', &
                    b_bold//'Alt'//e_bold//'='//b_italic//'Alternate'//e_italic//',', &
                    b_bold//'Fail'//e_bold//'='//b_italic//'"if failed"'//e_italic//',', &
                    b_bold//'Contrib'//e_bold//'='//b_italic//'contribution to demand for subject'//e_italic//DOT, &
                    e_para
        end if

!
!        end select

        ! disable controls if not allowed to edit checklists
        if ( isRole_admin_of_college(collegeOfStudent) ) then
             disabled = SPACE
        elseif ( isRole_adviser_of_student(targetStudent, orHigherUp) .and. &
                 College(collegeOfStudent)%isAllowed(ToEditCHECKLISTS,thisTerm) .and. &
                 Student(targetStudent)%Enlistment(thisTerm)%statusEnlistment<ENLISTMENT_LOCKED ) then
             disabled = SPACE
        else
             disabled = ' disabled '
        end if

        ! make form for Use FEASIBLE subjects As ADVISED
        if ( lenSubject>0 .and. len_trim(disabled)==0) then
            call make_form_start(device, fnEditCheckList, tStdNo, A9=thisTerm)
            write(device,AFORMAT) &
                '<input type="hidden" name="allowed" value="'//trim(ftoa(Advice%AllowedLoad,1))//'">', &
                '<input type="hidden" name="alternate" value="'//trim(itoa(Advice%NAlternates))//'">', &
                '<input type="hidden" name="current" value="'//trim(itoa(Advice%NCurrent))//'">', &
                '<input type="hidden" name="priority" value="'//trim(itoa(Advice%NPriority))//'">', &
                '<input type="hidden" name="status" value="'//trim(itoa(Advice%statusAdvising))//'">', &
                '<input type="hidden" name="classification" value="'//trim(itoa(Advice%levelClassification))//'">', &
                '<input type="hidden" name="group" value="'//trim(itoa(Advice%levelPriority))//'">', &
                '<input type="hidden" name="year" value="'//trim(itoa(Advice%levelYear))//'">', &
                '<input type="hidden" name="earned" value="'//trim(ftoa(Advice%UnitsEarned,1))//'">'
            do l=1,lenSubject
                write(tProb, '(f6.4)') Advice%Contrib(l)
                write(device,AFORMAT) &
                    '<input type="hidden" name="pri'//trim(itoa(l))//'" value="'//trim(Subject(Advice%Subject(l))%Name)// &
                    COMMA//tProb//'">'
            end do
            write(device,AFORMAT) b_bold//'Use FEASIBLE subjects '//e_bold//nbsp, &
                '<input type="submit" name="action" value="As ADVISED"> subjects'//e_form
        else
            write(device,AFORMAT) red//'''Use FEASIBLE subjects As ADVISED subjects'' is disabled, or '// &
                'there are no feasible subjects.'//e_color
        end if

        ! previously advised subjects
        write(device,AFORMAT) '<a name="ADVISED SUBJECTS"></a>'
        call checklist_links(device)

        write(device,AFORMAT) b_para, &
            b_bold//'ADVISED subjects for '//txtSemester(thisTerm+3)//termQualifier(thisTerm+3)//e_bold

        prevAdvice = Student(targetStudent)%Enlistment(thisTerm)
        lenSubject = prevAdvice%NPriority + prevAdvice%NAlternates + prevAdvice%NCurrent

        call make_form_start(device, fnEditCheckList, tStdNo, A9=thisTerm)

        write(device,AFORMAT) &
            b_italic//'Allowed load '//e_italic//nbsp//' <input type="text" name="allowed" size="5" value="'// &
            trim(ftoa(prevAdvice%AllowedLoad,1))//'" '//trim(disabled)//'>'
#if defined UPLB
        write(device,AFORMAT) &
            nbsp//nbsp, &
            b_italic//'Priority group '//e_italic//nbsp//' <input type="text" name="group" size="5" value="'// &
            trim(itoa(prevAdvice%levelPriority))//'" '//trim(disabled)//'> '//b_small//'(1=New freshman, 2=Graduating, '// &
            '3=Good standing, 4=Warning, 5=Probation, 6=dismissed/PD)'//e_small
#else
        write(device,AFORMAT) &
            '<input type="hidden" name="group" value="'//trim(itoa(prevAdvice%levelPriority))//'">'
#endif
        write(device,AFORMAT) &
            '<input type="hidden" name="status" value="'//trim(itoa(prevAdvice%statusAdvising))//'">', &
            '<input type="hidden" name="earned" value="'//trim(ftoa(prevAdvice%UnitsEarned,1))//'">', &
            '<input type="hidden" name="classification" value="'//trim(itoa(prevAdvice%levelClassification))//'">', &
            '<input type="hidden" name="year" value="'//trim(itoa(prevAdvice%levelYear))//'">', &
            '<input type="hidden" name="priority" value="'//trim(itoa(prevAdvice%NPriority))//'">', &
            '<input type="hidden" name="alternate" value="'//trim(itoa(prevAdvice%NAlternates))//'">', &
            '<input type="hidden" name="current" value="'//trim(itoa(prevAdvice%NCurrent))//'">'

        do l=1,lenSubject
            k = prevAdvice%Subject(l)
            write(tProb, '(f6.4)') prevAdvice%Contrib(l)
            write(device,AFORMAT) &
                '<input type="hidden" name="pri'//trim(itoa(l))//'" value="'//trim(Subject(k)%Name)//COMMA//tProb//'">'
        end do

        write(device,AFORMAT) linebreak//'<table border="0" width="80%">', b_tr, &
            b_thac//'Action'//e_th, &
            b_td_nbsp_e_td, &
            b_thal//'Subject'//e_th, &
            b_thac//'Credit'//e_th, &
            b_thal//'Title'//e_th, &
            e_tr

        write(device,AFORMAT) b_tr, &
            b_tdac//b_italic//'Add'//e_italic//e_td, &
            b_td_nbsp_e_td, &
            b_td//'<input type="text" name="additional" size="12" value="" '//trim(disabled)//'>'//e_td, &
            b_td_nbsp_e_td, &
            b_td//b_italic//'(Subject to add; no prerequisite check.)'//e_italic//e_td, &
            e_tr

        do l=1,prevAdvice%NPriority+prevAdvice%NAlternates
            k = prevAdvice%Subject(l)
            write(device,AFORMAT) b_tr, &
                b_tdac//b_italic//'Del '//trim(itoa(l))//e_italic//e_td, &
                b_tdac//'<input type="checkbox" name="del'//trim(itoa(l))//'" '//trim(disabled)//'>'//e_td, &
                b_td//trim(Subject(k)%Name)//e_td, &
                b_tdac//trim(ftoa(Subject(k)%Units,1))//e_td, &
                b_td//trim(Subject(k)%Title)//e_td, &
                e_tr
        end do

        if (prevAdvice%NCurrent>0) then
            write(device,AFORMAT) b_tr, &
                b_thac//'Fail'//e_th, &
                b_thal//'Contrib'//e_th, &
                b_thal//'Subject'//e_th, &
                b_thac//'Credit'//e_th, &
                b_thal//'Title'//e_th, &
                e_tr
            do l=prevAdvice%NPriority+prevAdvice%NAlternates+1, &
                    prevAdvice%NPriority+prevAdvice%NAlternates+prevAdvice%NCurrent
                k = prevAdvice%Subject(l)
                write(tProb, '(f6.4)') prevAdvice%Contrib(l)
                write(device,AFORMAT) b_tr, &
                    b_tdac//trim(itoa(l))//e_td, &
                    b_td//tProb//e_td, &
                    b_td//trim(Subject(k)%Name)//e_td, &
                    b_tdac//trim(ftoa(Subject(k)%Units,1))//e_td, &
                    b_td//trim(Subject(k)%Title)//e_td, &
                    e_tr
            end do
        end if

        write(device,AFORMAT) e_table, linebreak
        if (len_trim(disabled)==0) then
            write(device,AFORMAT) '<input type="submit" name="action" value="Update ADVISED"> subjects'
            if ( isRole_admin_of_college(collegeOfStudent) ) then
                write(device,AFORMAT) ' or '//nbsp//nbsp//nbsp//nbsp//nbsp, &
                    '<input type="submit" name="action" value="EXCLUDE Student">'
            end if
        else
            write(device,AFORMAT) red//'''Update ADVISED subjects'' is disabled'//e_color
        end if
        write(device,AFORMAT) e_form, e_para

#if defined UPLB
        if (notSpecified>0 .and. Advice%MissingPOCW>0) then
            call substitution_form(device,targetStudent)
        end if
#endif
        !    ! additional subject
        !   call make_form_start(device, fnEditCheckList)
        !    write(device,AFORMAT) &
        !      '<a name="ADDITIONAL subject"></a>'//horizontal// &
        !      trim(text_student_curriculum(targetStudent))// &
        !      '<input type="hidden" name="A1" value="'//trim(tStdNo)//'">', &
        !      b_bold//'ADDITIONAL subject'//e_bold//': '//nbsp//' Subject <input name="subject" value="">'//nbsp//, &
        !      ' Year <input name="year" value="">'//nbsp//, &
        !      ' Term <input name="term" value="">'//nbsp, &
        !      nbsp//nbsp//'<input type="submit" name="action" value="ADDITIONAL subject">', &
        !      linebreak//b_italic//'Note: Year=(THIRD,FOURTH,FIFTH,...) in curriculum, Term=(SUMMER,FIRST,SECOND)'//e_italic, &
        !      e_form//horizontal

        !
        !    ! Cancel additional subject
        !    call make_form_start(device, fnEditCheckList)
        !    write(device,AFORMAT) &
        !      '<a name="Cancel ADDITIONAL"></a>'//horizontal// &
        !      trim(text_student_curriculum(targetStudent))// &
        !      '<input type="hidden" name="A1" value="'//trim(tStdNo)//'">', &
        !      b_bold//'Cancel ADDITIONAL subject'//e_bold//nbsp//' <input type="text" name="subject" value="">'//nbsp//, &
        !      nbsp//nbsp//'<input type="submit" name="action" value="Cancel ADDITIONAL">', &
        !      e_form//horizontal

        write(device,AFORMAT) '<a name="Approved SUBSTITUTIONS"></a>'
        call checklist_links(device)

        write(device,AFORMAT) b_para, &
            b_bold//'Approved SUBSTITUTIONS'//e_bold// &
                '. Credit will be earned for Required subject if Substitute is passed.'//linebreak// &
            '<table border="0" width="50%">'
        j = 0
        do l=1,lenTCG
            if (TCG(l)%Code/=1) cycle ! not plan of study
            advising_comment = 'Required :'
            do k=1,TCG(l)%Reqd(0)
                advising_comment = trim(advising_comment)//SPACE//Subject(TCG(l)%Reqd(k))%Name
                j = j+1
            end do
            write(device,AFORMAT) b_tr//b_td//trim(advising_comment)//e_td
            advising_comment = 'Substitute :'
            do k=1,TCG(l)%Subst(0)
                advising_comment = trim(advising_comment)//SPACE//Subject(TCG(l)%Subst(k))%Name
            end do
            write(device,AFORMAT) b_td//trim(advising_comment)//e_td//e_tr
        end do
        if (j==0) write(device,AFORMAT) b_tr//b_td//BRNONE//e_td//e_tr
        write(device,AFORMAT) e_table, e_para

        if ( len_trim(disabled)>0 ) then
            write(device,AFORMAT) horizontal
            return
        end if

        if (j>0) then
            call make_form_start(device, fnEditCheckList, tStdNo, A9=thisTerm)
            write(device,AFORMAT) b_para, &
                b_bold//'Cancel SUBSTITUTION'//e_bold//': Enter Substitute subject.'//linebreak, &
                '<table border="0" width="33%">'//b_tr// &
                b_td//'Substitute :'//e_td// &
                b_td//'<input type="text" size="15" name="subject" value="">'//e_td// &
                e_tr//e_table//linebreak, &
                '<input type="submit" name="action" value="Cancel SUBSTITUTION">', &
                e_form, e_para
        end if

        ! subject substitution
        call make_form_start(device, fnEditCheckList, tStdNo, A9=thisTerm)
        write(device,AFORMAT) b_para, &
            b_bold//'Add SUBSTITUTION'//e_bold//': Enter the Required and Substitute subjects.'//linebreak// &
            '<table border="0" width="100%">'// &
            b_tr//b_td//'Required :'//e_td//b_td// &
                '<input type="text" size="15" name="req1" value="">', &
                '<input type="text" size="15" name="req2" value="">', &
                '<input type="text" size="15" name="req3" value="">', &
                '<input type="text" size="15" name="req4" value="">', &
                '<input type="text" size="15" name="req5" value="">'//e_td//e_tr, &
            b_tr//b_td//'Substitute :'//e_td//b_td// &
                '<input type="text" size="15" name="sub1" value="">', &
                '<input type="text" size="15" name="sub2" value="">', &
                '<input type="text" size="15" name="sub3" value="">', &
                '<input type="text" size="15" name="sub4" value="">', &
                '<input type="text" size="15" name="sub5" value="">'//e_td//e_tr, &
            e_table//linebreak// &
            '<input type="submit" name="action" value="Add SUBSTITUTION">', &
            e_form, e_para, horizontal

    end subroutine checklist_write_menu


end module EditCHECKLIST
