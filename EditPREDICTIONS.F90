!======================================================================
!
!    HEEDS (Higher Education Enrollment Decision Support) - A program
!      to create enrollment scenarios for 'next term' in a university
!    Copyright (C) 2012 Ricolindo L Carino
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

contains


    subroutine checklist_edit (device, UseClasses, Section, Offering)
        implicit none
        integer, intent (in) :: device
        logical, intent (in) :: UseClasses
        type (TYPE_OFFERED_SUBJECTS), intent(in), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section

        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject, input_name1, input_name2, input_value, currentSubject, update
        character(len=3*MAX_LEN_SUBJECT_CODE) :: tAction, search_string
        character (len=MAX_LEN_TEXT_YEAR) :: tYear
        character (len=MAX_LEN_TEXT_SEMESTER) :: tTerm
        character (len=MAX_LEN_TEXT_GRADE) :: tGrade
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character(len=127) :: mesg, TCGline
        logical :: FlagIsUp, isDirtyGrades, isDirtySubstitutions

        integer :: crse, year, term, nsubs, nreqs, n_changes
        integer :: crse_required, crse_current, crse_update, rank
        integer :: cdx, gdx, idx, jdx, ierr, i, j, k, l
        type (TYPE_PRE_ENLISTMENT) :: Advice

        ! which student?
        call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
        targetStudent = index_to_student(tStdNo)

        if (ierr/=0 .or. targetStudent==0) then
            targetDepartment = DeptIdxUser
            targetCollege = CollegeIdxUser
            call html_write_header(device, 'Student checklist', '<hr><br>Student "'//tStdNo//'" - not listed?')
            return
        else
            targetCurriculum = Student(targetStudent)%CurriculumIdx
            targetCollege = Curriculum(targetCurriculum)%CollegeIdx
            if (College(targetCollege)%Code == 'GS' .or.  College(targetCollege)%Code == ADMINISTRATION) then  
                targetDepartment = DeptIdxUser
                targetCollege = CollegeIdxUser
                call html_write_header(device, 'Student checklist', &
                '<hr><br>Checklists for graduate or special students are not available.')
                return
            end if
        end if
        !write(*,*) text_student_info(targetStudent)

        ! read checklist
        isDirtySubstitutions = .false. ! no changes yet
        isDirtyPREDICTIONS = .false. ! no changes yet
        isDirtyWAIVERCOI = .false. ! no changes yet
        call read_student_records (targetStudent)

        call date_and_time (date=currentDate,time=currentTime) ! timetamp for change
        TCGline = SPACE

        ! check for other arguments
        mesg = SPACE
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
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
                    isDirtySubstitutions = .true.

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
                    else ! add ADDITIONAL subject to TCG
                        mesg = trim(tAction)//' : '//trim(tYear)//COMMA//trim(tTerm)//COMMA//tSubject
                        isDirtySubstitutions = .true.

                        input_name1 = 'ADDITIONAL'
                        crse_required = index_to_subject(input_name1)
                        lenTCG = lenTCG + 1
                        TCG(lenTCG)%Code = 1 ! substitution
                        TCG(lenTCG)%Year = year
                        TCG(lenTCG)%Term = term
                        TCG(lenTCG)%Reqd(0) = 1
                        TCG(lenTCG)%Reqd(1) = crse_required ! ADDITIONAL
                        TCG(lenTCG)%Subst(0) = 1
                        TCG(lenTCG)%Subst(1) = crse ! actual

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
                        do l=1,lenTCG
                            if (TCG(l)%Code/=1) cycle ! not Substitution
                            if (TCG(l)%Reqd(1)/=crse_required) cycle ! not ADDITIONAL
                            if (crse/=TCG(l)%Subst(1)) cycle ! ADDITIONAL subject not the one to be deleted
                            TCG(l)%Code = 0 ! erase
                            isDirtySubstitutions = .true.
                            mesg = tAction//tSubject
                            exit
                        end do
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
                        mesg = trim(tAction)//' : '//tSubject//'- Subject code not valid?'
                    else
                        j = index_of_subject_in_curriculum(Curriculum(targetCurriculum), crse)
                        if (j>0) then ! a required subject
                            rank = Curriculum(targetCurriculum)%SubjectTerm(j)
                            call rank_to_year_term(rank, Year, Term)
                            TCGline = COMMA//trim(tSubject)//TCGline
                            nreqs = nreqs + 1
                            TCG(lenTCG+1)%Year = Year
                            TCG(lenTCG+1)%Term = Term
                            TCG(lenTCG+1)%Reqd(0) = nreqs
                            TCG(lenTCG+1)%Reqd(nreqs) = crse
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
                    if (nreqs>1) then ! group substitution; no year/term
                        TCG(lenTCG)%Year = 0
                        TCG(lenTCG)%Term = 0
                    end if
                    isDirtySubstitutions = .true.
                end if

            case ('Cancel SUBSTITUTION')
                call cgi_get_named_string(QUERY_STRING, 'subject', tSubject, crse)
                if (crse/=0) then ! a name was not specified?
                    mesg = trim(tAction)//' : subject not spelled correctly?'
                else if (tSubject==SPACE) then ! a value was not specified?
                    mesg = trim(tAction)//' : required subject not specified?'
                else
                    crse = index_to_subject(tSubject)
                    if (crse<=0) then
                        mesg = trim(tAction)//' : subject not valid?'
                    else
                        mesg = trim(tAction)//' : rule not found in checklist - '//tSubject
                        ! check TCG for specified SUBSTITUTION
                        do l=1,lenTCG
                            if (TCG(l)%Code/=1) cycle ! not PlanOfStudy
                            isDirtySubstitutions = .false.
                            do i=1,TCG(l)%Reqd(0)
                                if (TCG(l)%Reqd(i)==crse) then
                                    isDirtySubstitutions = .true.
                                    exit
                                end if
                            end do
                            if (.not. isDirtySubstitutions) cycle ! required subject not found
                            TCG(l)%Code = 0 ! erase substitution
                            mesg = trim(tAction)//' for '//tSubject
                            exit
                        end do
                    end if
                end if

            case ('Update ELECTIVE')

                n_changes = 0
                do idx=1,Curriculum(targetCurriculum)%NSubjects
                    crse_required = Curriculum(targetCurriculum)%SubjectIdx(idx)
                    if (crse_required > 0) cycle ! named subject
                    rank = Curriculum(targetCurriculum)%SubjectTerm(idx)

                    call blank_to_underscore(Subject(crse_required)%Name, input_name1)
                    input_name1 = trim(input_name1)//dash//trim(itoa(rank))//':'
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
                            do k=1,lenTCG
                                if (TCG(k)%Code/=1) cycle ! not Substitution
                                if (TCG(k)%Reqd(1)==crse_required .and. TCG(k)%Subst(1)==crse_current) then ! found
                                    FlagIsUp = .false.
                                    TCG(k)%Subst(1) = crse_update
                                    mesg = trim(mesg)//', '//trim(Subject(crse_update)%Name)
                                    exit
                                end if
                            end do

                            if (FlagIsUp) then ! make new entry
                                lenTCG = lenTCG + 1
                                mesg = trim(mesg)//', '//trim(Subject(crse_update)%Name)
                                TCG(lenTCG)%Code = 1 ! substitution
                                TCG(lenTCG)%Year = Year
                                TCG(lenTCG)%Term = Term
                                TCG(lenTCG)%Reqd(0) = 1
                                TCG(lenTCG)%Reqd(1) = crse_required
                                TCG(lenTCG)%Subst(0) = 1
                                TCG(lenTCG)%Subst(1) = crse_update
                            end if

                        else ! make new entry in TCG
                            lenTCG = lenTCG + 1
                            TCG(lenTCG)%Code = 1
                            mesg = trim(mesg)//', '//trim(Subject(crse_update)%Name)
                            TCG(lenTCG)%Year = Year
                            TCG(lenTCG)%Term = Term
                            TCG(lenTCG)%Reqd(0) = 1
                            TCG(lenTCG)%Reqd(1) = crse_required
                            TCG(lenTCG)%Subst(0) = 1
                            TCG(lenTCG)%Subst(1) = crse_update
                        end if
                        n_changes = n_changes + 1

                    else ! update is blank? remove current elective?
                        if (crse_current>0) then ! previously specified
                            FlagIsUp = .false.
                            do k=1,lenTCG
                                if (TCG(k)%Code/=1) cycle ! not Substitution
                                if (TCG(k)%Reqd(1)==crse_required .and. TCG(k)%Subst(1)==crse_current) then ! found
                                    FlagIsUp = .true.
                                    TCG(k)%Code = 0
                                end if
                            end do
                            if (FlagIsUp) n_changes = n_changes + 1
                        end if
                    end if

                end do

                if (n_changes==0) then
                    mesg = trim(tAction)//' : Nothing to update?'
                else
                    mesg = trim(tAction)//mesg
                    isDirtySubstitutions = .true.
                end if


            case ('Change GRADE') ! look for GRADE:<subject>=<grade>

                n_changes = 0
                input_name1 = 'GRADE:'
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

                        ! check Record() to update existing grade
                        FlagIsUp = .false. ! subject is in Record()?
                        do l=Student(targetStudent)%Record(1,0),1,-1 ! Record(i,:) 1=type,2=year,3=term,4=subject,5=grade
                            if (crse/=Student(targetStudent)%Record(4,l)) cycle ! not the required subject
                            FlagIsUp = .true. ! found
                            if (gdx==Student(targetStudent)%Record(5,l)) exit ! same grade
                            ! make change
                            Student(targetStudent)%Record(5,l) = gdx
                            n_changes = n_changes + 1
                            mesg = ' : '//trim(Subject(crse)%Name)//mesg
                            exit
                        end do

                        if (.not. FlagIsUp) then ! not in Record(); add an entry
                            l = Student(targetStudent)%Record(1,0) + 1
                            Student(targetStudent)%Record(1,0) = l ! no. of records
                            Student(targetStudent)%Record(1,l) = 0 ! grade type 'APE"
                            Student(targetStudent)%Record(2,l) = currentYear
                            Student(targetStudent)%Record(3,l) = currentTerm
                            Student(targetStudent)%Record(4,l) = crse
                            Student(targetStudent)%Record(5,l) = gdx
                            n_changes = n_changes + 1
                            mesg = ' : '//trim(Subject(crse)%Name)//', to '//tGrade//mesg
                        end if

                    else ! grade is empty (to indicate 'Remove grade')

                        ! check Record() to remove existing grade
                        do l=Student(targetStudent)%Record(1,0),1,-1 ! Record(i,:) 1=type,2=year,3=term,4=subject,5=grade
                            if (crse/=Student(targetStudent)%Record(4,l)) cycle ! not the required subject
                            ! make change
                            Student(targetStudent)%Record(5,l) = 0
                            n_changes = n_changes + 1
                            mesg = ' : Removed grade in '//trim(Subject(crse)%Name)//mesg
                            exit
                        end do


                    end if

                end do

                if (n_changes==0) then
                    mesg = trim(tAction)//' : Nothing to update?'
                else
                    mesg = trim(tAction)//mesg
                    isDirtyGrades = .true.
                end if


            case ('Revise PREDICTION', 'For PREDICTION')

                call initialize_pre_enlistment(Advice)

                call cgi_get_named_integer(QUERY_STRING, 'earned', Advice%UnitsEarned, ierr)
                call cgi_get_named_integer(QUERY_STRING, 'classification', Advice%StdClassification, ierr)
                call cgi_get_named_integer(QUERY_STRING, 'year', Advice%StdYear, ierr)
                !write(*,*) 'earned=', Advice%UnitsEarned, ', classif=', Advice%StdClassification, ', year=', Advice%StdYear

                call cgi_get_named_integer(QUERY_STRING, 'allowed', Advice%AllowedLoad, ierr)
                call cgi_get_named_integer(QUERY_STRING, 'group', Advice%StdPriority, ierr)
                call cgi_get_named_integer(QUERY_STRING, 'priority', Advice%NPriority, ierr)
                call cgi_get_named_integer(QUERY_STRING, 'alternate', Advice%NAlternates, ierr)
                call cgi_get_named_integer(QUERY_STRING, 'current', Advice%NCurrent, ierr)
                !write(*,*) 'allow=', Advice%AllowedLoad, ', grp=', Advice%StdPriority, ', NP=', &
                !  Advice%NPriority, ', NA=', Advice%NAlternates, ', NC=', Advice%NCurrent

                Advice%lenSubject = Advice%NPriority+Advice%NAlternates+Advice%NCurrent
                do l=1,Advice%lenSubject
                    call cgi_get_named_string(QUERY_STRING, 'pri'//itoa(l), search_string, ierr)
                    j = index(search_string,COMMA)
                    tSubject = search_string(j+1:)
                    read(tSubject,'(f6.4)') Advice%Contrib(l)
                    tSubject = search_string(1:j-1)
                    crse = index_to_subject(tSubject)
                    Advice%Subject(l) = crse
                !write(*,*) l, crse, tSubject, Advice%Contrib(l)
                end do


                if (trim(tAction)=='For PREDICTION') then
                    ! update Advised()
                    NumPredictionRecords = NumPredictionRecords - Advised(targetStudent)%lenSubject + Advice%lenSubject
                    Advised(targetStudent) = Advice
                    isDirtyPREDICTIONS = .true. 
                    mesg = trim(tAction)//' : '//trim(itoa(Advice%lenSubject))//' entries.'
                else ! (trim(tAction)=='Revise PREDICTION')
                    ! apply deletes
                    n_changes = 0
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
                        isDirtyPREDICTIONS = .true.
                        !write(*,*) 'delete', l, Subject(crse)%Name
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
                        !write(*,*) '  from WaiverCOI() also'
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
                        isDirtyPREDICTIONS = .true.
                    end if
                    ! check if subject added (COI, Waived prereq)
                    call cgi_get_named_string(QUERY_STRING, 'additional', search_string, ierr)
                    if (search_string/=SPACE) then ! something there
                        tSubject = search_string
                        crse = index_to_subject(tSubject)
                        if (crse>=0) then ! add
                            mesg = trim(mesg)//' : Add '//tSubject
                            do l=Advice%lenSubject,1,-1
                                Advice%Subject(l+1) = Advice%Subject(l)
                                Advice%Contrib(l+1) = Advice%Contrib(l)
                            end do
                            Advice%Subject(1) = crse
                            Advice%Contrib(1) = 1.0
                            Advice%NPriority = 1+Advice%NPriority
                            Advice%lenSubject = 1+Advice%lenSubject
                            isDirtyPREDICTIONS = .true.
                            ! add to WaiverCOI
                            l = WaiverCOI(targetStudent)%lenSubject+1
                            WaiverCOI(targetStudent)%lenSubject = l
                            WaiverCOI(targetStudent)%Subject(l) = crse
                            isDirtyWaiverCOI = .true.
                            n_changes = n_changes + 1
                        end if
                    end if
                    if (n_changes>0) then
                        mesg = trim(tAction)//mesg
                        Advised(targetStudent) = Advice
                    end if
                end if

            case default


        end select
        !write(*,*) trim(mesg)

        if (isDirtyGrades) then
            call xml_write_student_grades(targetStudent)
        end if

        if (isDirtySubstitutions) then
            call xml_write_substitutions(targetStudent)
        end if

        if (isDirtyWAIVERCOI) then
            call xml_write_pre_enlistment(pathToTarget, 'WAIVER-COI', Preenlisted, Section)
        end if

        if (isDirtyPREDICTIONS) then
            call xml_write_pre_enlistment(pathToTarget, 'PREDICTIONS-'//CurrProgCode(targetCurriculum), &
                Advised, Section, targetCurriculum)
        end if

        call checklist_write_menu (device, UseClasses, isDirtyGrades .or. isDirtySubstitutions, Offering, mesg)

        return
    end subroutine checklist_edit


    subroutine checklist_write_menu (device, UseClasses, isDirtyMCL, Offering, mesg)
        integer, intent (in) :: device
        logical, intent (in) :: UseClasses, isDirtyMCL
        type (TYPE_OFFERED_SUBJECTS), intent(in), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        character(len=*), intent (in)  :: mesg

        character(len=6) :: tProb
        type (TYPE_PRE_ENLISTMENT) :: Advice
        integer :: checklistout, k, l, MissingPOCW, NRemaining, lenSubject, notSpecified

        ! Plan of Study
        l = Student(targetStudent)%CurriculumIdx
        notSpecified = 0 ! how many entries in plan for this curriculum?
        do k=1,Curriculum(l)%NSubjects
            if (Curriculum(l)%SubjectIdx(k)<0) then
                notSpecified = notSpecified + 1
                exit
            end if
        end do

        call html_write_header(device, Student(targetStudent)%StdNo//nbsp// &
        trim(Student(targetStudent)%Name)//SPACE//dash//SPACE//Curriculum(targetCurriculum)%Code, mesg)

        call advise_student (targetStudent, UseClasses, Offering, WaiverCOI(targetStudent), Advice, MissingPOCW, NRemaining)
        lenSubject = Advice%NPriority+Advice%NAlternates+Advice%NCurrent

        call checklist_page_links(device, .true.)

        call checklist_display  (device, targetStudent, Advice, MissingPOCW, NRemaining)

        ! make copy of updated checklist for upload
        if (isDirtyMCL) then
            checklistout = stderr-3
            StdNoYearLen = index(Student(targetStudent)%StdNo,dash)-1
            if (StdNoYearLen<=0) StdNoYearLen = StdNoChars
            call open_for_write(checklistout, trim(dirUploadCHECKLISTS)// &
                DIRSEP//Student(targetStudent)%StdNo(1:StdNoYearLen)//DIRSEP//trim(Student(targetStudent)%StdNo)//'.html')
            write(checklistout,AFORMAT) '<b>MINI-CHECKLIST for '//Student(targetStudent)%StdNo//nbsp// &
                trim(Student(targetStudent)%Name)//SPACE//dash//SPACE//Curriculum(targetCurriculum)%Code

            call checklist_display (checklistout, targetStudent, Advice, MissingPOCW, NRemaining)
            close(checklistout)
        end if


        !write(*,*) 'user='//trim(USER), ' Unit='//trim(CurrProgCode(targetCurriculum)), &
        !  ' curriculum='//trim(Curriculum(targetCurriculum)%Code)
        !if (isRoleAdmin .or. (isRoleSRE .and. CollegeIdxUser==Curriculum(targetCurriculum)%CollegeIdx) ) then
        if (isRoleAdmin .or. (isRoleSRE .and. CurrProgCode(CurriculumIdxUser)==CurrProgCode(targetCurriculum) ) ) then
            if (lenSubject>0) then
                write(device,AFORMAT) &
                    '<form name="input" method="post" action="'//CGI_SCRIPT//'">', &
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

                if (Period==1) then ! change mat; switch to subjects from new analysis

                    write(device,AFORMAT) &
                    '<input type="hidden" name="F" value="'//trim(itoa(fnChangeMatriculation))//'">'// &
                    '<input type="hidden" name="A1" value="'//trim(Student(targetStudent)%StdNo)//'">'// &
                    '<input type="hidden" name="A2" value="Switch">', &
                    '<br>Use <b>FEASIBLE</b> subjects '// &
                    '<input type="submit" name="action" value="For ENLISTMENT"></form>'

                else ! Period=2 or 3

                    write(device,AFORMAT) &
                    '<input type="hidden" name="F" value="'//trim(itoa(fnEditCheckList))//'">'// &
                    '<input type="hidden" name="A1" value="'//trim(Student(targetStudent)%StdNo)//'">'// &
                    '<br>Use <b>FEASIBLE</b> subjects '// &
                    '<input type="submit" name="action" value="For PREDICTION"></form>'

                end if

                call checklist_page_links(device) !, .true.)

            end if

        else
            return
        end if

        ! revise predictions
        lenSubject = Advised(targetStudent)%NPriority+Advised(targetStudent)%NAlternates+Advised(targetStudent)%NCurrent
        if (lenSubject>0) then

            Advice = Advised(targetStudent)

            write(device,AFORMAT) '<a name="Revise PREDICTION"></a><hr>'// &
                trim(text_student_curriculum(targetStudent))// &
                '<br><b>REVISE PREDICTION for '//txtSemester(targetTerm+6)// &
                ' Semester '//trim(itoa(targetYear))//dash//trim(itoa(targetYear+1))//'</b>', &
                '<form name="input" method="post" action="'//CGI_SCRIPT//'">'// &
                '<input type="hidden" name="F" value="'//trim(itoa(fnEditCheckList))//'">'// &
                '<input type="hidden" name="A1" value="'//trim(Student(targetStudent)%StdNo)//'">'
            write(device,AFORMAT) &
                '<br><i>Allowed load </i> '//nbsp//' <input type="text" name="allowed" size="5" value="'// &
                trim(itoa(Advice%AllowedLoad))//'">'// &
                nbsp//nbsp//' <i>Priority group </i> '//nbsp//' <input type="text" name="group" size="5" value="'// &
                trim(itoa(Advice%StdPriority))//'"> <small>(1=New freshman, 2=Graduating, '// &
                '3=Good standing, 4=Warning, 5=Probation, 6=dismissed/PD)</small>', &
                '<table border="0" width="80%">'
            write(device,AFORMAT) &
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
            do l=1,Advice%NPriority+Advice%NAlternates
                k = Advice%Subject(l)
                write(device,AFORMAT) &
                    begintr//tdaligncenter//'<i>Del '//trim(itoa(l))//'</i>'//endtd// &
                    tdaligncenter//'<input type="checkbox" name="del'//trim(itoa(l))//'">'//endtd// &
                    begintd//trim(Subject(k)%Name)//endtd// &
                    tdaligncenter//trim(ftoa(Subject(k)%Units,1))//endtd// &
                    begintd//trim(Subject(k)%Title)//endtd//endtr
            end do

            !if (Advice%NAlternates>0) then
            !    write(device,AFORMAT) &
            !        begintr//thaligncenter//'Alt'//endth// &
            !                 thalignleft//'Contrib'//endth// &
            !                 thalignleft//'Subject'//endth// &
            !                 thaligncenter//'Credit'//endth// &
            !                 thalignleft//'Title'//endth//endtr
            !        do l=Advice%NPriority+1,Advice%NPriority+Advice%NAlternates
            !          k = Advice%Subject(l)
            !          write(tProb, '(f6.4)') Advice%Contrib(l)
            !          write(device,AFORMAT) &
            !            begintr//tdaligncenter//trim(itoa(l))//endtd// &
            !                     begintd//tProb//endtd// &
            !                     begintd//trim(Subject(k)%Name)//endtd// &
            !                     tdaligncenter//trim(ftoa(Subject(k)%Units))//endtd// &
            !                     begintd//trim(Subject(k)%Title)//endtd//endtr
            !        end do
            !end if

            if (Advice%NCurrent>0) then
                write(device,AFORMAT) &
                    begintr//thaligncenter//'Fail'//endth// &
                    thalignleft//'Contrib'//endth// &
                    thalignleft//'Subject'//endth// &
                    thaligncenter//'Credit'//endth// &
                    thalignleft//'Title'//endth//endtr
                do l=Advice%NPriority+Advice%NAlternates+1,Advice%NPriority+Advice%NAlternates+Advice%NCurrent
                    k = Advice%Subject(l)
                    write(tProb, '(f6.4)') Advice%Contrib(l)
                    write(device,AFORMAT) &
                        begintr//tdaligncenter//trim(itoa(l))//endtd// &
                        begintd//tProb//endtd// &
                        begintd//trim(Subject(k)%Name)//endtd// &
                        tdaligncenter//trim(ftoa(Subject(k)%Units,1))//endtd// &
                        begintd//trim(Subject(k)%Title)//endtd//endtr
                end do
            end if
            write(device,AFORMAT) '</table>'! // &
            !'<i>Notes: Pri=Priority, Alt=Alternate, Fail="if failed", Contrib=contribution to subject demand.</i>'

            write(device,AFORMAT) &
                '<input type="hidden" name="earned" value="'//trim(itoa(Advice%UnitsEarned))//'">', &
                '<input type="hidden" name="classification" value="'//trim(itoa(Advice%StdClassification))//'">', &
                '<input type="hidden" name="year" value="'//trim(itoa(Advice%StdYear))//'">', &
                '<input type="hidden" name="priority" value="'//trim(itoa(Advice%NPriority))//'">', &
                '<input type="hidden" name="alternate" value="'//trim(itoa(Advice%NAlternates))//'">', &
                '<input type="hidden" name="current" value="'//trim(itoa(Advice%NCurrent))//'">'
            do l=1,lenSubject
                k = Advice%Subject(l)
                write(tProb, '(f6.4)') Advice%Contrib(l)
                write(device,AFORMAT) &
                    '<input type="hidden" name="pri'//trim(itoa(l))//'" value="'//trim(Subject(k)%Name)//COMMA//tProb//'">'
            end do

            write(device,AFORMAT) &
                '<br><input type="submit" name="action" value="Revise PREDICTION">', &
                '</form><hr>'
            call checklist_page_links(device) !, .true.)
        end if

        ! change curriculum
        write(device,AFORMAT) &
            '<a name="Change CURRICULUM"></a><hr>'// &
            trim(text_student_curriculum(targetStudent))// &
            '<form name="input" method="post" action="'//CGI_SCRIPT//'">', &
            '<input type="hidden" name="F" value="'//trim(itoa(fnEditCheckList))//'">', &
            '<input type="hidden" name="A1" value="'//trim(Student(targetStudent)%StdNo)//'">', &
            '<b>CHANGE CURRICULUM</b> from '//trim(Curriculum(Student(targetStudent)%CurriculumIdx)%Code)//' to: <br>', &
            '<select name="A2">', &
            '<option value="0"> (select curriculum)'
        do l=1,NumCurricula
            k = Curriculum(l)%CollegeIdx
            if (College(k)%Code == 'GS' .or.  College(k)%Code == ADMINISTRATION) cycle
            if (l==Student(targetStudent)%CurriculumIdx) cycle
            write(device,AFORMAT) '<option value="'//trim(Curriculum(l)%Code)//'"> '//trim(Curriculum(l)%Code)//' - '// &
                trim(Curriculum(l)%Title)
            if (len_trim(Curriculum(l)%Specialization) > 0) then
                write(device,AFORMAT) ', '//trim(Curriculum(l)%Specialization)
            end if
            if (len_trim(Curriculum(l)%Remark) > 0) then
                write(device,AFORMAT) ', '//trim(Curriculum(l)%Remark)
            end if
        end do
        write(device,AFORMAT) &
            '</select>', &
            nbsp//nbsp//'<input type="submit" name="action" value="Change CURRICULUM">', &
            '</form><hr>'
        call checklist_page_links(device) !, .true.)

        if (notSpecified>0) then
            call substitution_form(device,targetStudent, &
                Advice%UnitsEarned, Advice%StdClassification, Advice%StdYear, MissingPOCW, &
                Advice%AllowedLoad, Advice%StdPriority, Advice%NPriority, Advice%NAlternates, Advice%NCurrent, NRemaining)
            call checklist_page_links(device) !, .true.)
        end if

        ! ChangeOfGrade
        call change_grade_form(device,targetStudent)
        call checklist_page_links(device) !, .true.)

        !    ! additional subject
        !    write(device,AFORMAT) &
        !      '<a name="ADDITIONAL subject"></a><hr>'// &
        !      trim(text_student_curriculum(targetStudent))// &
        !      '<form name="input" method="post" action="'//CGI_SCRIPT//'">', &
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
        !      '<form name="input" method="post" action="'//CGI_SCRIPT//'">', &
        !      '<input type="hidden" name="F" value="'//trim(itoa(fnEditCheckList))//'">'// &
        !      '<input type="hidden" name="A1" value="'//trim(Student(targetStudent)%StdNo)//'">', &
        !      '<b>Cancel ADDITIONAL subject</b> '//nbsp//' <input type="text" name="subject" value="">'//nbsp//, &
        !      nbsp//nbsp//'<input type="submit" name="action" value="Cancel ADDITIONAL">', &
        !      '</form><hr>'
        !    call checklist_page_links(device) !, .true.)

        ! subject substitution
        write(device,AFORMAT) &
            '<a name="Subject SUBSTITUTION"></a><hr>'// &
            trim(text_student_curriculum(targetStudent))// &
            '<form name="input" method="post" action="'//CGI_SCRIPT//'">', &
            '<input type="hidden" name="F" value="'//trim(itoa(fnEditCheckList))//'">'// &
            '<input type="hidden" name="A1" value="'//trim(Student(targetStudent)%StdNo)//'">', &
            '<b>Subject SUBSTITUTION</b>: Enter the required and substitute subjects'// &
            '<table border="0" width="100%">'// &
            begintr//begintd//'Required :'//endtd// &
            begintd//'<input type="text" name="req1" value="">'//endtd// &
            begintd//'<input type="text" name="req2" value="">'//endtd// &
            begintd//'<input type="text" name="req3" value="">'//endtd// &
            begintd//'<input type="text" name="req4" value="">'//endtd// &
            begintd//'<input type="text" name="req5" value="">'//endtd//endtr, &
            begintr//begintd//'Substitute :'//endtd// &
            begintd//'<input type="text" name="sub1" value="">'//endtd// &
            begintd//'<input type="text" name="sub2" value="">'//endtd// &
            begintd//'<input type="text" name="sub3" value="">'//endtd// &
            begintd//'<input type="text" name="sub4" value="">'//endtd// &
            begintd//'<input type="text" name="sub5" value="">'//endtd//endtr, &
            '</table>'// &
            '<input type="submit" name="action" value="Subject SUBSTITUTION">', &
            '</form><hr>'
        call checklist_page_links(device) !, .true.)

        ! cancel subject substitution
        write(device,AFORMAT) &
            '<a name="Cancel SUBSTITUTION"></a><hr>'// &
            trim(text_student_curriculum(targetStudent))// &
            '<form name="input" method="post" action="'//CGI_SCRIPT//'">', &
            '<input type="hidden" name="F" value="'//trim(itoa(fnEditCheckList))//'">'// &
            '<input type="hidden" name="A1" value="'//trim(Student(targetStudent)%StdNo)//'">', &
            '<b>Cancel SUBSTITUTION for subject</b> '//nbsp//' <input type="text" name="subject" value="">'//nbsp, &
            nbsp//nbsp//'<input type="submit" name="action" value="Cancel SUBSTITUTION">', &
            '</form><hr>'
        call checklist_page_links(device) !, .true.)

        !    ! change grade
        !    write(device,AFORMAT) '<form name="input" method="post" action="'//CGI_SCRIPT//'">', &
        !      '<input type="hidden" name="F" value="'//trim(itoa(fnEditCheckList))//'">'// &
        !      '<input type="hidden" name="A1" value="'//trim(Student(targetStudent)%StdNo)//'">', &
        !      '<b>Change grade</b>: '//nbsp//' Subject <input name="subject" value="">'//nbsp//, &
        !      ' Grade <input name="grade" value="">'//nbsp//, &
        !      ' Year earned: <input name="year" value="">'//nbsp//, &
        !      ' Term <input name="term" value="">'//nbsp, &
        !      '<br><i>Note: Grade=(numeric, S, INC, PASS, REGD), Year=(19XX,2XXX), Term=(SUMMER,FIRST,SECOND)</i>', &
        !      '<br><input type="submit" name="action" value="Change grade">', &
        !      '</form><hr>'

        return
    end subroutine checklist_write_menu


    subroutine change_grade_form(device, std)
        integer, intent (in) :: std, device
        integer :: idx, tdx, Year, Term, m, n, rank
        integer, dimension(MAX_LEN_STUDENT_RECORD) :: p, q

        write(device,AFORMAT) &
            '<a name="Change GRADE"></a><hr>'// &
            '<form name="input" method="post" action="'//CGI_SCRIPT//'">', &
            '<input type="hidden" name="F" value="'//trim(itoa(fnEditCheckList))//'">'// &
            '<input type="hidden" name="A1" value="'//trim(Student(std)%StdNo)//'">', &
            '<b>CHANGE OF GRADE</b> for '// trim(text_student_curriculum(std)), &
            '<br><i>Enter or modify contents of the edit boxes below, then click "Change GRADE"</i>'// &
            '<table border="0" width="90%">'
        do tdx=1,CheckList%NumTerms,3
            Year = tdx/3+1
            Term = mod(tdx,3)
            if (Term == 0) Year = Year-1
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
        return
    end subroutine change_grade_form


    subroutine substitution_form(device, std, &
        UnitsEarned, StdClassification, StdYear, MissingPOCW, &
        AllowedLoad, StdPriority, NPriority, NAlternates, NCurrent, NRemaining)
        integer, intent (in) :: std, device, &
            UnitsEarned, StdClassification, StdYear, MissingPOCW, &
            AllowedLoad, StdPriority, NPriority, NAlternates, NCurrent, NRemaining
        integer :: idx, tdx, Year, Term, m, n, rank
        integer, dimension(MAX_LEN_STUDENT_RECORD) :: p, q

        write(device,AFORMAT) &
            '<a name="Update ELECTIVE"></a><hr>'// &
            '<form name="input" method="post" action="'//CGI_SCRIPT//'">'// &
            '<input type="hidden" name="F" value="'//trim(itoa(fnEditCheckList))//'">'// &
            '<input type="hidden" name="A1" value="'//trim(Student(std)%StdNo)//'">', &
            '<b>PLAN OF STUDY update form</b> for '// trim(text_student_curriculum(std)), &
            '<br><i>Enter or modify contents of the edit boxes below, then click "Update ELECTIVE"</i>'// &
            '<table border="0" width="90%">'
        do tdx=1,CheckList%NumTerms,3
            Year = tdx/3+1
            Term = mod(tdx,3)
            if (Term == 0) Year = Year-1
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
        return
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
                '[ Update <a href="#Update ELECTIVE">ELECTIVE</a> ] ', &
                '[ Subject <a href="#Subject SUBSTITUTION">SUBSTITUTION</a> ] ', &
                '</small><br><br>'
                !'[ <a href="#ADDITIONAL subject">ADDITIONAL</a> subject ] ', &
        else
            write(device,AFORMAT) '<small>[ <a href="#TOP">CHECKLIST</a> ] '
            if (Advised(targetStudent)%lenSubject>0) write(device,AFORMAT) &
                '[ Revise <a href="#Revise PREDICTION">PREDICTION</a> ]'
            write(device,AFORMAT) &
                '[ Change <a href="#Change CURRICULUM">CURRICULUM</a> ] ', &
                '[ Change <a href="#Change GRADE">GRADE</a> ]', &
                '[ Update <a href="#Update ELECTIVE">ELECTIVE</a> ] ', &
                '[ Subject <a href="#Subject SUBSTITUTION">SUBSTITUTION</a> ] ', &
                '</small><br><br><br><br>'
                !'[ <a href="#ADDITIONAL subject">ADDITIONAL</a> subject ] ', &
        end if
        return
    end subroutine checklist_page_links


    subroutine checklist_display  (device, std, Advice, MissingPOCW, NRemaining)
        integer, intent (in) :: std, device, MissingPOCW, NRemaining
        type (TYPE_PRE_ENLISTMENT), intent (in) :: Advice
        integer :: idx, tdx, Year, Term, m, n, rank, idxCURR, k, l
        integer, dimension(MAX_LEN_STUDENT_RECORD) :: p, q
        integer:: UnitsPaid, UnitsDropped, UnitsPassed, Standing
        character(len=6) :: tProb

        idxCURR = Student(std)%CurriculumIdx

        k = Curriculum(idxCURR)%NumTerms
        n = SpecifiedUnits(k)
        write(device,AFORMAT) '<br><b>Units to earn classifications</b>', &
            (': '//nbsp//trim(txtStanding(l))//'<='//trim(itoa(int((0.25*l)*n))), l=1,4)
        write(device,AFORMAT) '<br><b>Units to achieve YEAR</b>', &
            (': '//nbsp//trim(txtYear(l))//'<'//trim(itoa(SpecifiedUnits(3*l))), l=1,(k+1)/3)

        write(device,AFORMAT) '<table border="0" width="100%">'
        do tdx=1,CheckList%NumTerms,3
            Year = tdx/3+1
            Term = mod(tdx,3)
            if (Term == 0) Year = Year-1
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

        call get_scholastic_three_terms (prevYearYear, prevYearTerm, UnitsPaid, UnitsDropped, UnitsPassed, Standing)
        write(device,AFORMAT) '<br><br><b>SUMMARY for '//txtSemester(prevYearTerm+6)// &
            ' Semester '//trim(itoa(prevYearYear))//dash//trim(itoa(prevYearYear+1)), &
            '</b>: Units registered='//trim(itoa(UnitsPaid)), &
            ': '//nbsp//' Dropped='//trim(itoa(UnitsDropped)), &
            ': '//nbsp//' Earned='//trim(itoa(UnitsPassed)), &
            ': '//nbsp//' Scholastic standing='//trim(txtScholastic(Standing))

        call get_scholastic_three_terms (prevTermYear, prevTermTerm, UnitsPaid, UnitsDropped, UnitsPassed, Standing)
        write(device,AFORMAT) '<br><b>SUMMARY for '//txtSemester(prevTermTerm+6)// &
            ' Semester '//trim(itoa(prevTermYear))//dash//trim(itoa(prevTermYear+1)), &
            '</b>: Units registered='//trim(itoa(UnitsPaid)), &
            ': '//nbsp//' Dropped='//trim(itoa(UnitsDropped)), &
            ': '//nbsp//' Earned='//trim(itoa(UnitsPassed)), &
            ': '//nbsp//' Scholastic standing='//trim(txtScholastic(Standing))

        call get_scholastic_three_terms (currentYear, currentTerm, UnitsPaid, UnitsDropped, UnitsPassed, Standing)
        write(device,AFORMAT) '<br><b>SUMMARY for '//txtSemester(currentTerm+6)// &
            ' Semester '//trim(itoa(currentYear))//dash//trim(itoa(currentYear+1)), &
            '</b>: Units registered='//trim(itoa(UnitsPaid)), &
            ': '//nbsp//' Dropped='//trim(itoa(UnitsDropped)), &
            ': '//nbsp//' Earned='//trim(itoa(UnitsPassed)), &
            ': '//nbsp//' Scholastic standing='//trim(txtScholastic(Standing))

        if (MissingPOCW>0) then
            write(device,AFORMAT) '<br><br>'//red//'<b>MISSING ELECTIVEs/entries in Plan of Study = '// &
                trim(itoa(MissingPOCW))//'</b>'//black
        end if

        if (Period>1) then
            write(device,AFORMAT) '<br><br><b>ASSUMPTION at the end of '//txtSemester(currentTerm+6)// &
                ' Semester '//trim(itoa(currentYear))//dash//trim(itoa(currentYear+1)), &
                '</b>: Units earned='//trim(itoa(Advice%UnitsEarned)), &
                ': '//nbsp//' Classification='//trim(txtStanding(Advice%StdClassification)), &
                ': '//nbsp//' Year in curriculum='//trim(txtYear(Advice%StdYear))
        end if

        select case (Advice%StdPriority)

            case (10)
                write(device,AFORMAT) '<br><br>PROGRAM COMPLETE?'

            case (9)
                write(device,AFORMAT) &
                    '<br><br><b>NO FEASIBLE SUBJECTS ?</b>. PREREQUISITES NOT SATISFIED or PLAN OF STUDY IS INCOMPLETE', &
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
                        write(device,AFORMAT) ' : '//Subject(TCG(q(m))%Subject)%Name//dash//txtGrade(pGrade(TCG(q(m))%Grade))
                    end do
                    write(device,AFORMAT) '<br>'
                end if

                write(device,AFORMAT) '<br><b>FEASIBLE subjects for '//txtSemester(targetTerm+6)// &
                    ' Semester '//trim(itoa(targetYear))//dash//trim(itoa(targetYear+1)), &
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
                write(device,AFORMAT) '</table>'// &
                '<i>Notes: Pri=Priority, Alt=Alternate, Fail="if failed", Contrib=contribution to demand for subject.</i>'


        end select
        write(device,AFORMAT) '<hr>'

        return
    end subroutine checklist_display


end module EditPREDICTIONS
