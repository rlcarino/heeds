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


module EditCHECKLIST

    use EditSTUDENT

    implicit none

contains


    subroutine needs_analysis(thisTerm, UseClasses, Section, Offering, eList, path, mesg)

        integer, intent (in) :: thisTerm
        logical, intent (in) :: UseClasses
        type (TYPE_OFFERED_SUBJECTS), intent(in), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        type (TYPE_SECTION), intent(in) :: Section(0:)
        type (TYPE_PRE_ENLISTMENT), intent (in out) :: eList(0:)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: mesg

        integer :: numEntries, std
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=127) :: advising_comment
        integer :: MissingPOCW, NRemaining
        type (TYPE_PRE_ENLISTMENT) :: Advice

        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, numEntries)
        targetCollege = index_to_college(tCollege)

        call html_comment('needs_analysis('//tCollege//')')

        if (isRoleOfficial) then
            mesg = 'Needs Analysis not updated. '//sorryMessage
            return
        end if

        do std = 1,NumStudents+NumAdditionalStudents

            if (Student(std)%Classification>=NotOfficiallyEnrolled) then ! exclude
                call initialize_pre_enlistment(eList(std))
                cycle
            end if

            if (targetCollege/=NumColleges) then ! only some students; check if student is in targetCollege
                if (Curriculum(Student(std)%CurriculumIdx)%CollegeIdx/=targetCollege) cycle
                if (eList(std)%Status/=0) cycle
                ! other check here to skip std
            end if

            numEntries = eList(std)%NPriority + eList(std)%NAlternates + eList(std)%NCurrent

            call advise_student (std, thisTerm, UseClasses, Offering, &
                WaiverCOI(std), Advice, MissingPOCW, NRemaining, advising_comment)

            NumEnlistment(thisTerm) = NumEnlistment(thisTerm) - numEntries + &
                Advice%NPriority+Advice%NAlternates+Advice%NCurrent
            eList(std) = Advice

        end do

        call xml_write_pre_enlistment(path, 'ENLISTMENT', eList, Section)

        mesg = 'Needs Analysis updated '

    end subroutine needs_analysis



    subroutine checklist_display  (device, std, thisTerm, Advice, MissingPOCW, NRemaining)
        integer, intent (in) :: std, thisTerm, device, MissingPOCW, NRemaining
        type (TYPE_PRE_ENLISTMENT), intent (in) :: Advice
        integer :: idx, tdx, Year, Term, m, n, rank, idxCURR, k, l, Standing
        integer, dimension(MAX_LEN_STUDENT_RECORD) :: p, q
        real :: UnitsPaid, UnitsDropped, UnitsPassed, tUnits
        character(len=6) :: tProb

        call html_comment('checklist_display()')

        idxCURR = Student(std)%CurriculumIdx

        k = Curriculum(idxCURR)%NumTerms
        tUnits = SpecifiedUnits(k)
        write(device,AFORMAT) beginbold//'Units to earn classifications'//endbold, &
            (': '//nbsp//trim(txtStanding(l))//'<'//trim(itoa(int(0.25*l*tUnits+0.5))), l=1,4)
        write(device,AFORMAT) linebreak//beginbold//'Units to achieve YEAR'//endbold, &
            (': '//nbsp//trim(txtYear(l))//'<'//trim(ftoa(SpecifiedUnits(3*l),1)), l=1,(k+2)/3)

        write(device,AFORMAT) '<table border="0" width="100%">'
        do tdx=1,CheckList%NumTerms,3
            call rank_to_year_term(tdx, Year, Term)
            write(device,AFORMAT) begintr//'<td colspan="5">'//linebreak//beginbold//trim(txtYear(Year))// &
                ' Year, First Semester'//endbold//' ('//trim(ftoa(TermUnits(tdx),1))//' units)'//endtd// &
                tdnbspendtd//'<td colspan="5">'//linebreak//beginbold//trim(txtYear(Year))// &
                ' Year, Second Semester'//endbold//' ('//trim(ftoa(TermUnits(tdx+1),1))//' units)'//endtd//endtr
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
                    begintd//trim(CLExt(p(idx))%Disp_Subject)//endtd//&
                    begintd//trim(CLExt(p(idx))%Disp_Comment)//endtd//&
                    begintd//CLExt(p(idx))%Disp_Grade//endtd//&
                    begintd//CLExt(p(idx))%Disp_Units//endtd//&
                    begintd//CLExt(p(idx))%Disp_Remarks//endtd//&
                    tdnbspendtd// &
                    begintd//trim(CLExt(q(idx))%Disp_Subject)//endtd//&
                    begintd//trim(CLExt(q(idx))%Disp_Comment)//endtd//&
                    begintd//CLExt(q(idx))%Disp_Grade//endtd//&
                    begintd//CLExt(q(idx))%Disp_Units//endtd// &
                    begintd//CLExt(q(idx))%Disp_Remarks//endtd//&
                    endtr
            end do
            if (m > n) then
                do idx=n+1,m
                    write(device,AFORMAT) &
                        begintr// &
                        begintd//trim(CLExt(p(idx))%Disp_Subject)//endtd//&
                        begintd//trim(CLExt(p(idx))%Disp_Comment)//endtd//&
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
                        begintd//trim(CLExt(q(idx))%Disp_Subject)//endtd//&
                        begintd//trim(CLExt(q(idx))%Disp_Comment)//endtd//&
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
                write(device,AFORMAT) begintr//'<td colspan="5">'//linebreak//beginbold//trim(txtYear(Year))// &
                    ' Year, Summer'//endbold//' ('//trim(ftoa(TermUnits(tdx+2),1))//' units)'//endtd// &
                    '<td colspan="6">'//nbsp//endtd//endtr
                do idx=1,m
                    write(device,AFORMAT) &
                        begintr// &
                        begintd//trim(CLExt(p(idx))%Disp_Subject)//endtd//&
                        begintd//trim(CLExt(p(idx))%Disp_Comment)//endtd//&
                        begintd//CLExt(p(idx))%Disp_Grade//endtd//&
                        begintd//CLExt(p(idx))%Disp_Units//endtd//&
                        begintd//CLExt(p(idx))%Disp_Remarks//endtd//&
                        '<td colspan="6">'//nbsp//endtd//&
                        endtr
                end do
            end if
        end do
        write(device,AFORMAT) endtable, &
            linebreak//beginbold//'LEGENDS'//endbold//' for grades and remarks:'//nbsp//' * - specify subject/GE/PE activity;'//&
            nbsp//' # - failed;'//nbsp//' % - conditional;', &
            linebreak//nbsp//' PASS - credit earned/exempted;'//nbsp//' REGD - currently registered;', &
            nbsp//' PriN - Nth priority subject;'//nbsp//' AltK - Kth alternate subject'

        call get_scholastic_three_terms (cTm3Year, cTm3, UnitsPaid, UnitsDropped, UnitsPassed, Standing)
        write(device,AFORMAT) linebreak//linebreak//beginbold//'SUMMARY for '//text_term_school_year(cTm3+6,cTm3Year), &
            endbold//': Units registered='//trim(ftoa(UnitsPaid,1)), &
            ': '//nbsp//' Dropped='//trim(ftoa(UnitsDropped,1)), &
            ': '//nbsp//' Earned='//trim(ftoa(UnitsPassed,1)), &
            ': '//nbsp//' Scholastic standing='//trim(txtScholastic(Standing))

        call get_scholastic_three_terms (cTm2Year, cTm2, UnitsPaid, UnitsDropped, UnitsPassed, Standing)
        write(device,AFORMAT) linebreak//beginbold//'SUMMARY for '//text_term_school_year(cTm2+6,cTm2Year), &
            endbold//': Units registered='//trim(ftoa(UnitsPaid,1)), &
            ': '//nbsp//' Dropped='//trim(ftoa(UnitsDropped,1)), &
            ': '//nbsp//' Earned='//trim(ftoa(UnitsPassed,1)), &
            ': '//nbsp//' Scholastic standing='//trim(txtScholastic(Standing))

        call get_scholastic_three_terms (cTm1Year, cTm1, UnitsPaid, UnitsDropped, UnitsPassed, Standing)
        write(device,AFORMAT) linebreak//beginbold//'SUMMARY for '//text_term_school_year(cTm1+6,cTm1Year), &
            endbold//': Units registered='//trim(ftoa(UnitsPaid,1)), &
            ': '//nbsp//' Dropped='//trim(ftoa(UnitsDropped,1)), &
            ': '//nbsp//' Earned='//trim(ftoa(UnitsPassed,1)), &
            ': '//nbsp//' Scholastic standing='//trim(txtScholastic(Standing))

        if (isProbabilistic) then

            call get_scholastic_three_terms (currentYear, currentTerm, UnitsPaid, UnitsDropped, UnitsPassed, Standing)
            write(device,AFORMAT) linebreak//beginbold//'SUMMARY for '// &
                text_term_school_year(currentTerm+6,currentYear), &
                endbold//': Units registered='//trim(ftoa(UnitsPaid,1)), &
                ': '//nbsp//' Dropped='//trim(ftoa(UnitsDropped,1)), &
                ': '//nbsp//' Earned='//trim(ftoa(UnitsPassed,1)), &
                ': '//nbsp//' Scholastic standing='//trim(txtScholastic(Standing))
        end if

        if (MissingPOCW>0) then
            write(device,AFORMAT) linebreak//linebreak//red//beginbold//'MISSING ELECTIVEs/entries in Plan of Study = '// &
                trim(itoa(MissingPOCW))//endbold//black
        end if

        ! extra subjects
        n = 0
        q = 0
        do idx=1,lenTCG
            if (TCG(idx)%Code<2 .or. TCG(idx)%Used) cycle
            if (TCG(idx)%Subject /= 0 .and. is_grade_passing(TCG(idx)%Grade)) then
                n = n+1
                q(n) = idx
            end if
        end do
        if (n > 0) then
            write(device,AFORMAT) linebreak//linebreak//beginbold//'EXTRA subjects'//endbold//' or unused units after substitutions'
            do m=1,n
                write(device,AFORMAT) ' : '//Subject(TCG(q(m))%Subject)%Name//DASH//txtGrade(pGrade(TCG(q(m))%Grade))
            end do
        end if

        if (Advice%errNSTP/=0) write(device,AFORMAT) linebreak//linebreak//red//beginbold//'Check NSTP!'//endbold//black

        if (isProbabilistic) then ! not enlistment period
            write(device,AFORMAT) linebreak//linebreak//beginbold//'ASSUMPTION at the end of '// &
                text_term_school_year(currentTerm+6,currentYear), &
                endbold//': Units earned='//trim(ftoa(Advice%UnitsEarned,1))//FSLASH//trim(ftoa(tUnits,1)), &
                ': '//nbsp//' Classification='//trim(txtStanding(Advice%StdClassification)), &
                ': '//nbsp//' Year in curriculum='//trim(txtYear(Advice%StdYear))
        end if

        select case (Advice%StdPriority)

            case (10)
                write(device,AFORMAT) linebreak//linebreak//'PROGRAM COMPLETE?'

            case (9)
                write(device,AFORMAT) &
                    linebreak//linebreak//beginbold//'NO FEASIBLE SUBJECTS?'//endbold// &
                        ' PREREQUISITES NOT SATISFIED or PLAN OF STUDY IS INCOMPLETE', &
                    linebreak//'Some remaining subject are : <table border="0" width="100%">', &
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
                write(device,AFORMAT) endtable

            case (8)
                write(device,AFORMAT) linebreak//linebreak//'CHECK RECORDS! SUBJECTS ARE FOR A NEW FRESHMAN?'

            case (7)
                write(device,AFORMAT) linebreak//linebreak//'GRADUATE, DIPLOMA, NON-DEGREE or SPECIAL STUDENT?'

            !case (6)
            !  write(device,AFORMAT) SPACE, ' DISMISSED or PERMANENTLY DISQUALIFIED?'

            case default

                write(device,AFORMAT) linebreak//linebreak//beginbold//'FEASIBLE subjects for '// &
                    txtSemester(thisTerm+6)//termQualifier(thisTerm+6), &
                    endbold//' (ALLOWED load = '//trim(ftoa(Advice%AllowedLoad,1))//') '//linebreak// &
                        '<table border="0" width="100%">'
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
                write(device,AFORMAT) endtable, linebreak, &
                    beginitalic//'Note:'//enditalic, &
                    beginbold//'Pri'//endbold//'='//beginitalic//'Priority'//enditalic//',', &
                    beginbold//'Alt'//endbold//'='//beginitalic//'Alternate'//enditalic//',', &
                    beginbold//'Fail'//endbold//'='//beginitalic//'"if failed"'//enditalic//',', &
                    beginbold//'Contrib'//endbold//'='//beginitalic//'contribution to demand for subject'//enditalic//DOT

        end select

    end subroutine checklist_display


    subroutine checklist_edit (device, thisTerm, UseClasses, Section, Offering, eList, path)
        implicit none
        integer, intent (in) :: device, thisTerm
        logical, intent (in) :: UseClasses
        type (TYPE_OFFERED_SUBJECTS), intent(in), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        type (TYPE_SECTION), intent(in) :: Section(0:)
        type (TYPE_PRE_ENLISTMENT), intent (in out) :: eList(0:)
        character(len=*), intent(in) :: path

        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject, input_name1, input_name2, input_value, currentSubject, update
        character(len=3*MAX_LEN_SUBJECT_CODE) :: tAction
        character (len=MAX_LEN_TEXT_YEAR) :: tYear
        character (len=MAX_LEN_TEXT_SEMESTER) :: tTerm
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character(len=127) :: TCGline
        character(len=255) :: mesg
        logical :: FlagIsUp, isDirtyPlan
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
        isDirtyWaiverCOI = .false. ! no changes yet

        TCGline = SPACE
        mesg = SPACE

        !select case (trim(tAction))

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
                            mesg = tAction//tSubject

                        end if

                    end if
                end if

            end if

            if (.not. isRoleOfficial .and. trim(tAction)=='Subject SUBSTITUTION') then

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
                    mesg = trim(tAction)//' : subject not spelled correctly?'

                else if (tSubject==SPACE) then ! a value was not specified?
                    mesg = trim(tAction)//' : required subject not specified?'

                else ! some subject was specified
                    crse = index_to_subject(tSubject)
                    if (crse<=0) then ! not a named subject
                        mesg = trim(tAction)//' : subject not valid?'
                    else
                        mesg = trim(tAction)//' : rule not found in checklist - '//tSubject
                        ! check Reqd() for specified SUBSTITUTION
                        idx = 0
                        do k=1,lenTCG
                            if (TCG(k)%Code/=1) cycle
                            do i=1,TCG(k)%Reqd(0)
                                if (TCG(k)%Reqd(i)==crse) then
                                    isDirtyPlan = .true.
                                    idx = k
                                    exit
                                end if
                            end do
                            if (isDirtyPlan) exit
                        end do
                        if (isDirtyPlan) then ! required subject found
                            ! shift substitutions down
                            do k=idx+1,lenTCG
                                TCG(k-1) = TCG(k)
                            end do
                            ! re-initialize previous last location
                            TCG(lenTCG) = TCG(lenTCG+1)
                            lenTCG = lenTCG - 1 ! decrease counter
                            mesg = trim(tAction)//' for '//tSubject
                            ! copy to Student(std)
                        end if
                    end if
                end if

            end if

            if (.not. isRoleOfficial .and. trim(tAction)=='Update ELECTIVE') then

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
                            do k=1,lenTCG
                                if (TCG(k)%Code/=1) cycle
                                if (TCG(k)%Reqd(1)==crse_required .and. &
                                    TCG(k)%Subst(1)==crse_current) then
                                    TCG(k)%Subst(1) = crse_update
                                    FlagIsUp = .false.
                                    mesg = trim(mesg)//', '//trim(Subject(crse_update)%Name)
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
                                mesg = trim(mesg)//', '//trim(Subject(crse_update)%Name)

                            end if

                        else ! make new entry in TCG
                            lenTCG = lenTCG + 1
                            call rank_to_year_term (rank, TCG(lenTCG)%Year, TCG(lenTCG)%Term)
                            TCG(lenTCG)%Code = 1
                            TCG(lenTCG)%Reqd(0) = 1
                            TCG(lenTCG)%Reqd(1) = crse_required
                            TCG(lenTCG)%Subst(0) = 1
                            TCG(lenTCG)%Subst(1) = crse_update
                            mesg = trim(mesg)//', '//trim(Subject(crse_update)%Name)
                            mesg = trim(mesg)//', '//trim(Subject(crse_update)%Name)

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
                end if

            end if


            if (.not. isRoleOfficial .and. trim(tAction)=='As ADVICE') then

                call collect_advice(Advice, n_changes, mesg)
                NumEnlistment(thisTerm) = NumEnlistment(thisTerm)- eList(targetStudent)%lenSubject + Advice%lenSubject
                eList(targetStudent) = Advice
                isDirtyPREDICTIONS = .true.
                mesg = trim(tAction)//' : '//trim(itoa(Advice%lenSubject))//' entries.'

            end if


            if (.not. isRoleOfficial .and. trim(tAction)=='Update ADVICE') then

                call collect_advice(Advice, n_changes, mesg)
                if (n_changes>0) then
                    mesg = trim(tAction)//mesg
                    eList(targetStudent) = Advice
                    isDirtyPREDICTIONS = .true.
                end if

            end if


            if (.not. isRoleOfficial .and. trim(tAction)=='Delete ALL') then

                call collect_advice(Advice, n_changes, mesg)
                do l=1,Advice%lenSubject
                    Advice%Contrib(l) = 0
                    Advice%Subject(l) = 0
                    Advice%Section(l) = 0
                end do
                Advice%AllowedLoad = 0
                Advice%StdPriority = 0
                Advice%NPriority = 0
                Advice%NAlternates = 0
                Advice%NCurrent = 0
                Advice%lenSubject = 0

                mesg = 'Deleted all advised subjects.'
                eList(targetStudent) = Advice
                isDirtyPREDICTIONS = .true.

            end if


            if (.not. isRoleOfficial .and. trim(tAction)=='Lock ADVICE') then

                call collect_advice(Advice, n_changes, mesg)
                Advice%Status = 1 ! lock advice
                mesg = 'Locked advice.'
                eList(targetStudent) = Advice
                isDirtyPREDICTIONS = .true.

            end if

            if (.not. isRoleOfficial .and. trim(tAction)=='Unlock ADVICE') then

                call collect_advice(Advice, n_changes, mesg)
                Advice%Status = 0 ! unlock advice
                mesg = 'Unlocked advice.'
                eList(targetStudent) = Advice
                isDirtyPREDICTIONS = .true.

            end if


        !end select

        if (len_trim(tAction)>0 .and. isRoleOfficial) then
            mesg = '"'//trim(tAction)//'" failed. '//sorryMessage
        end if

        if (isDirtyPlan) then
            call xml_write_substitutions(targetStudent)
            call log_student_record_change(targetStudent, trim(mesg) )
        end if

        if (isDirtyWaiverCOI) then
            call xml_write_waivers(path, Section, thisTerm)
        end if

        if (isDirtyPREDICTIONS) then
            call xml_write_pre_enlistment(path, 'ENLISTMENT', eList, Section, targetCurriculum)
        end if

        call checklist_write_menu (device, thisTerm, UseClasses, Offering, eList, mesg)

    end subroutine checklist_edit


    subroutine checklist_write_menu (device, thisTerm, UseClasses, Offering, eList, mesg)
        integer, intent (in) :: device, thisTerm
        logical, intent (in) :: UseClasses
        type (TYPE_OFFERED_SUBJECTS), intent(in), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        type (TYPE_PRE_ENLISTMENT), intent (in) :: eList(0:)
        character(len=*), intent (in) :: mesg

        character(len=6) :: tProb
        character(len=10) :: helpFile
        character(len=127) :: advising_comment
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

        if (.not. isPeriodOne) then
            helpFile = 'Checklists'
        else
            helpFile = 'Enlistment'
        end if
        call html_write_header(device, Student(targetStudent)%StdNo//nbsp// &
            trim(Student(targetStudent)%Name)// &
            trim(make_href(fnCurriculum, Curriculum(idxCurr)%Code, A1=Curriculum(idxCurr)%Code, &
            pre=beginsmall//' (', post=' ) '))// &
            nbsp//' <a target="0" href="/'//helpFile//'.html">Help</a>'//endsmall, mesg)

        if ( is_adviser_of_student(targetStudent, orHigherUp) ) then

            !if (isRoleStudent) then
            !    write(device,AFORMAT) red//'Warning: '// &
            !        'This is the student sandbox where you can investigate "What if ...?" scenarios '// &
            !        'regarding subjects to be enlisted. However, any changes you make are not binding. '// &
            !        'Visit your Adviser or the Registrar to make the actual changes.'//black//linebreak
            !end if

            ! change curriculum form
            call make_form_start(device, fnEditCheckList, Student(targetStudent)%StdNo, A9=thisTerm)
            write(device,AFORMAT) &
                '<select name="A2">'
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
                nbsp//nbsp//'<input type="submit" name="action" value="Change CURRICULUM">'//endform

        else

            write(device,AFORMAT) trim(text_curriculum_info(idxCurr))//linebreak

        end if

        call advise_student (targetStudent, thisTerm, UseClasses, Offering, WaiverCOI(targetStudent), Advice, &
            MissingPOCW, NRemaining, advising_comment)

        if (len_trim(advising_comment)>0) then
            write(device,AFORMAT) beginbold//red//trim(advising_comment)//black//endbold//linebreak//linebreak
        end if

        lenSubject = Advice%NPriority+Advice%NAlternates+Advice%NCurrent

        call checklist_display (device, targetStudent, thisTerm, Advice, MissingPOCW, NRemaining)

        ! offer Advice for PREDICTION or ENLISTMENT depending on period
        if ( is_adviser_of_student(targetStudent, orHigherUp) ) then
            if (lenSubject>0) then
                if (.not. isPeriodOne) then
                    call make_form_start(device, fnEditCheckList, Student(targetStudent)%StdNo, A9=thisTerm)
                else
                    call make_form_start(device, fnChangeMatriculation, Student(targetStudent)%StdNo, 'Switch', A9=thisTerm)
                end if
                write(device,AFORMAT) &
                    '<input type="hidden" name="errnstp" value="'//trim(itoa(Advice%errNSTP))//'">', &
                    '<input type="hidden" name="earned" value="'//trim(ftoa(Advice%UnitsEarned,1))//'">', &
                    '<input type="hidden" name="classification" value="'//trim(itoa(Advice%StdClassification))//'">', &
                    '<input type="hidden" name="year" value="'//trim(itoa(Advice%StdYear))//'">', &
                    '<input type="hidden" name="allowed" value="'//trim(ftoa(Advice%AllowedLoad,1))//'">', &
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

                if (eList(targetStudent)%Status==0) then ! allow changes
                    write(device,AFORMAT) linebreak//beginbold//'Use FEASIBLE subjects '//endbold//nbsp, &
                        '<input type="submit" name="action" value="As ADVICE">'//endform
                else
                    write(device,AFORMAT) linebreak//'Advice is locked. Select "Unlock ADVICE" below.'//endform
                end if

            end if
            write(device,AFORMAT) horizontal

        else ! un-authorized users do not see the remainder of the checklist
            write(device,AFORMAT) horizontal
            return

        end if

        write(device,AFORMAT) &
            linebreak//beginbold//'ADVISED SUBJECTS for '//txtSemester(thisTerm+3)//termQualifier(thisTerm+3)//endbold

        if (.not. isPeriodOne) then

            call make_form_start(device, fnEditCheckList, Student(targetStudent)%StdNo, A9=thisTerm)
            prevAdvice = eList(targetStudent)
            lenSubject = prevAdvice%NPriority + prevAdvice%NAlternates + prevAdvice%NCurrent

            write(device,AFORMAT) linebreak, &
                beginitalic//'Allowed load '//enditalic//nbsp//' <input type="text" name="allowed" size="5" value="'// &
                trim(ftoa(prevAdvice%AllowedLoad,1))//'">', &
                nbsp//nbsp, &
                beginitalic//'Priority group '//enditalic//nbsp//' <input type="text" name="group" size="5" value="'// &
                trim(itoa(prevAdvice%StdPriority))//'"> '//beginsmall//'(1=New freshman, 2=Graduating, '// &
                '3=Good standing, 4=Warning, 5=Probation, 6=dismissed/PD)'//endsmall, &
                '<input type="hidden" name="errnstp" value="'//trim(itoa(prevAdvice%errNSTP))//'">', &
                '<input type="hidden" name="earned" value="'//trim(ftoa(prevAdvice%UnitsEarned,1))//'">', &
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

            write(device,AFORMAT) linebreak//'<table border="0" width="80%">', begintr, &
                thaligncenter//'Action'//endth, &
                tdnbspendtd, &
                thalignleft//'Subject'//endth, &
                thaligncenter//'Credit'//endth, &
                thalignleft//'Title'//endth, &
                endtr

            if (prevAdvice%Status==0) then ! allow changes

                write(device,AFORMAT) begintr, &
                    tdaligncenter//beginitalic//'Add'//enditalic//endtd, &
                    tdnbspendtd, &
                    begintd//'<input type="text" name="additional" size="12" value="">'//endtd, &
                    tdnbspendtd, &
                    begintd//beginitalic//'(Approved COI or WAIVER of prerequisite)'//enditalic//endtd, &
                    endtr

                do l=1,prevAdvice%NPriority+prevAdvice%NAlternates
                    k = prevAdvice%Subject(l)
                    write(device,AFORMAT) begintr, &
                        tdaligncenter//beginitalic//'Del '//trim(itoa(l))//enditalic//endtd, &
                        tdaligncenter//'<input type="checkbox" name="del'//trim(itoa(l))//'">'//endtd, &
                        begintd//trim(Subject(k)%Name)//endtd, &
                        tdaligncenter//trim(ftoa(Subject(k)%Units,1))//endtd, &
                        begintd//trim(Subject(k)%Title)//endtd, &
                        endtr
                end do

            else

                do l=1,prevAdvice%NPriority+prevAdvice%NAlternates
                    k = prevAdvice%Subject(l)
                    write(device,AFORMAT) begintr, &
                        tdaligncenter//beginitalic//trim(itoa(l))//enditalic//endtd, &
                        tdnbspendtd, &
                        begintd//trim(Subject(k)%Name)//endtd, &
                        tdaligncenter//trim(ftoa(Subject(k)%Units,1))//endtd, &
                        begintd//trim(Subject(k)%Title)//endtd, &
                        endtr
                end do

            end if

            if (prevAdvice%NCurrent>0) then
                write(device,AFORMAT) begintr, &
                    thaligncenter//'Fail'//endth, &
                    thalignleft//'Contrib'//endth, &
                    thalignleft//'Subject'//endth, &
                    thaligncenter//'Credit'//endth, &
                    thalignleft//'Title'//endth, &
                    endtr
                do l=prevAdvice%NPriority+prevAdvice%NAlternates+1, &
                        prevAdvice%NPriority+prevAdvice%NAlternates+prevAdvice%NCurrent
                    k = prevAdvice%Subject(l)
                    write(tProb, '(f6.4)') prevAdvice%Contrib(l)
                    write(device,AFORMAT) begintr, &
                        tdaligncenter//trim(itoa(l))//endtd, &
                        begintd//tProb//endtd, &
                        begintd//trim(Subject(k)%Name)//endtd, &
                        tdaligncenter//trim(ftoa(Subject(k)%Units,1))//endtd, &
                        begintd//trim(Subject(k)%Title)//endtd, &
                        endtr
                end do
            end if

            write(device,AFORMAT) endtable, linebreak

            if (prevAdvice%Status==0) then ! allow changes

                write(device,AFORMAT) &
                    '<input type="submit" name="action" value="Update ADVICE">', nbsp//nbsp//nbsp//nbsp//nbsp, &
                    '<input type="submit" name="action" value="Delete ALL">', nbsp//nbsp//nbsp//nbsp//nbsp, &
                    '<input type="submit" name="action" value="Lock ADVICE">'

            else ! Unlock only

                write(device,AFORMAT) 'Advice is locked. ', &
                    '<input type="submit" name="action" value="Unlock ADVICE"> to change subjects.'

            end if

            write(device,AFORMAT) endform//horizontal

        else ! if (.not. isRoleStudent) then ! prevent students from editing their schedules

            call make_form_start(device, fnChangeMatriculation, Student(targetStudent)%StdNo, 'Switch', A9=thisTerm)
            prevAdvice = eList(targetStudent)
            lenSubject = prevAdvice%NPriority + prevAdvice%NAlternates

            write(device,AFORMAT) linebreak, &
                beginitalic//'Allowed load '//enditalic//nbsp//' <input type="text" name="allowed" size="5" value="'// &
                trim(ftoa(max(prevAdvice%AllowedLoad,Advice%AllowedLoad),1))//'">', &
                '<input type="hidden" name="earned" value="'//trim(ftoa(max(prevAdvice%UnitsEarned, Advice%UnitSEarned),1))//'">',&
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

            write(device,AFORMAT) linebreak//'<table border="0" width="80%">', begintr, &
                thaligncenter//'Action'//endth, &
                tdnbspendtd, &
                thalignleft//'Subject'//endth, &
                thaligncenter//'Credit'//endth, &
                thalignleft//'Title'//endth, &
                endtr, &
                begintr, &
                tdaligncenter//beginitalic//'Add'//enditalic//endtd, &
                tdnbspendtd, &
                begintd//'<input type="text" name="additional" size="12" value="">'//endtd, &
                tdnbspendtd, &
                begintd//beginitalic//'(Approved COI or WAIVER of prerequisite)'//enditalic//endtd, &
                endtr

            do l=1,lenSubject
                k = prevAdvice%Subject(l)
                write(device,AFORMAT) begintr, &
                    tdaligncenter//beginitalic//'Del '//trim(itoa(l))//enditalic//endtd,  &
                    tdaligncenter//'<input type="checkbox" name="del'//trim(itoa(l))//'">'//endtd,  &
                    begintd//trim(Subject(k)%Name)//endtd, &
                    tdaligncenter//trim(ftoa(Subject(k)%Units,1))//endtd,  &
                    begintd//trim(Subject(k)%Title)//endtd, &
                    endtr
            end do

            write(device,AFORMAT) endtable, linebreak, &
                '<input type="submit" name="action" value="Use FOR ENLISTMENT">', &
                endform, horizontal
        end if

        if (notSpecified>0) then
            call substitution_form(device,targetStudent) !, &
                !Advice%UnitsEarned, Advice%StdClassification, Advice%StdYear, MissingPOCW, &
                !Advice%AllowedLoad, Advice%StdPriority, Advice%NPriority, Advice%NAlternates, Advice%NCurrent, NRemaining)
        end if

        !    ! additional subject
        !   call make_form_start(device, fnEditCheckList)
        !    write(device,AFORMAT) &
        !      '<a name="ADDITIONAL subject"></a>'//horizontal// &
        !      trim(text_student_curriculum(targetStudent))// &
        !      '<input type="hidden" name="A1" value="'//trim(Student(targetStudent)%StdNo)//'">', &
        !      beginbold//'ADDITIONAL subject'//endbold//': '//nbsp//' Subject <input name="subject" value="">'//nbsp//, &
        !      ' Year <input name="year" value="">'//nbsp//, &
        !      ' Term <input name="term" value="">'//nbsp, &
        !      nbsp//nbsp//'<input type="submit" name="action" value="ADDITIONAL subject">', &
        !      linebreak//beginitalic//'Note: Year=(THIRD,FOURTH,FIFTH,...) in curriculum, Term=(SUMMER,FIRST,SECOND)'//enditalic, &
        !      endform//horizontal

        !
        !    ! Cancel additional subject
        !    call make_form_start(device, fnEditCheckList)
        !    write(device,AFORMAT) &
        !      '<a name="Cancel ADDITIONAL"></a>'//horizontal// &
        !      trim(text_student_curriculum(targetStudent))// &
        !      '<input type="hidden" name="A1" value="'//trim(Student(targetStudent)%StdNo)//'">', &
        !      beginbold//'Cancel ADDITIONAL subject'//endbold//nbsp//' <input type="text" name="subject" value="">'//nbsp//, &
        !      nbsp//nbsp//'<input type="submit" name="action" value="Cancel ADDITIONAL">', &
        !      endform//horizontal

        write(device,AFORMAT) &
            linebreak//beginbold//'Approved SUBSTITUTIONS'//endbold// &
                '. Credit will be earned for required subject if substitute is passed.'//linebreak// &
            '<table border="0" width="50%">'
        j = 0
        do l=1,lenTCG
            if (TCG(l)%Code/=1) cycle ! not plan of study
            advising_comment = 'Required :'
            do k=1,TCG(l)%Reqd(0)
                advising_comment = trim(advising_comment)//SPACE//Subject(TCG(l)%Reqd(k))%Name
                j = j+1
            end do
            write(device,AFORMAT) begintr//begintd//trim(advising_comment)//endtd
            advising_comment = 'Substitute :'
            do k=1,TCG(l)%Subst(0)
                advising_comment = trim(advising_comment)//SPACE//Subject(TCG(l)%Subst(k))%Name
            end do
            write(device,AFORMAT) begintd//trim(advising_comment)//endtd//endtr
        end do
        if (j==0) write(device,AFORMAT) begintr//begintd//BRNONE//endtd//endtr
        write(device,AFORMAT) endtable//linebreak

        if (j>0) then
            call make_form_start(device, fnEditCheckList, Student(targetStudent)%StdNo, A9=thisTerm)
            write(device,AFORMAT) &
                beginbold//'Cancel SUBSTITUTION'//endbold//': Enter required subject.'//linebreak, &
                '<table border="0" width="33%">'//begintr// &
                begintd//'Required :'//endtd// &
                begintd//'<input type="text" size="15" name="subject" value="">'//endtd// &
                endtr//endtable//linebreak, &
                '<input type="submit" name="action" value="Cancel SUBSTITUTION">', &
                endform
        end if

        ! subject substitution
        call make_form_start(device, fnEditCheckList, Student(targetStudent)%StdNo, A9=thisTerm)
        write(device,AFORMAT) &
            linebreak//beginbold//'Additional SUBSTITUTION'//endbold//': Enter the required and substitute subjects.'//linebreak// &
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
            endtable//linebreak// &
            '<input type="submit" name="action" value="Subject SUBSTITUTION">', &
            endform//horizontal

    end subroutine checklist_write_menu


    subroutine substitution_form(device, std) !, &
        !UnitsEarned, StdClassification, StdYear, MissingPOCW, &
        !AllowedLoad, StdPriority, NPriority, NAlternates, NCurrent, NRemaining)
        integer, intent (in) :: std, device!, &
            !StdClassification, StdYear, MissingPOCW, &
            !StdPriority, NPriority, NAlternates, NCurrent, NRemaining
        !real, intent (in) :: UnitsEarned, AllowedLoad
        integer :: idx, tdx, Year, Term, m, n, rank
        integer, dimension(MAX_LEN_STUDENT_RECORD) :: p, q

!        write(device,AFORMAT) '<a name="Update ELECTIVE"></a>'//horizontal
        call make_form_start(device, fnEditCheckList, Student(std)%StdNo)
        write(device,AFORMAT) linebreak//beginbold//'PLAN OF STUDY update form'//endbold, &
            linebreak//beginitalic//'Enter or modify contents of the edit boxes below, then click "Update ELECTIVE"'//enditalic// &
            '<table border="0" width="90%">'
        do tdx=1,CheckList%NumTerms,3
            call rank_to_year_term(tdx, Year, Term)
            write(device,AFORMAT) begintr//'<td colspan="4">'//linebreak//beginbold//trim(txtYear(Year))// &
                ' Year, First Semester'//endbold//' ('//trim(ftoa(TermUnits(tdx),1))//' units)'//endtd// &
                tdnbspendtd//'<td colspan="4">'//linebreak//beginbold//trim(txtYear(Year))// &
                ' Year, Second Semester'//endbold//' ('//trim(ftoa(TermUnits(tdx+1),1))//' units)'//endtd//endtr
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
                write(device,AFORMAT) begintr//'<td colspan="4">'//linebreak//beginbold//trim(txtYear(Year))// &
                    ' Year, Summer'//endbold//' ('//trim(ftoa(TermUnits(tdx+2),1))//' units)'//endtd// &
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
        write(device,AFORMAT) endtable, &
            linebreak//'<input type="submit" name="action" value="Update ELECTIVE">'//endform//horizontal

    end subroutine substitution_form


    subroutine demand_by_new_freshmen(device, Offering)
        integer, intent (in) :: device
        type (TYPE_OFFERED_SUBJECTS), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS), intent (in out) :: Offering
        integer :: nstd, idxCURR
        integer :: crse, ierr, j, k, l
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege

        ! which college ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        targetCollege = index_to_college(tCollege)

        ! initialize counters
        do crse=MAX_ALL_DUMMY_SUBJECTS,MAX_ALL_SUBJECTS
            Offering(crse)%Demand = 0
        end do

        if (REQUEST==fnDemandFreshmen) then

            ! check
            NFintake(0) = sum(NFintake(1:))
            if (NFintake(0) == 0) then
                write(device,AFORMAT) linebreak//red//'Total no. of new freshmen is 0 ?'//black//linebreak
                return
            end if

        else ! extract from QUERY_STRING
            ! INTAKE
            do idxCURR=1,NumCurricula
                call cgi_get_named_integer(QUERY_STRING, trim(Curriculum(idxCURR)%Code), nstd, ierr)
                if (ierr==0) then
                    write(*,*) Curriculum(idxCURR)%Code, nstd, ierr
                    NFintake(idxCURR) = nstd
                end if
            end do
            ! edited value
            !write(*,*) trim(QUERY_STRING)
            call cgi_get_named_string(QUERY_STRING, 'curriculum', tCurriculum, ierr)
            if (ierr==0) then
                idxCURR = abs(index_to_curriculum(tCurriculum))
                !write(*,*) idxCURR, tCurriculum
                if (idxCURR/=0) then
                    !write(*,*) trim(QUERY_STRING)
                    call cgi_get_named_integer(QUERY_STRING, 'count', nstd, ierr)
                    if (ierr==0) then
                        NFintake(idxCURR) = nstd
                      !write(*,*) tCurriculum, nstd
                    end if
                end if
            end if
            ! check
            NFintake(0) = sum(NFintake(1:))
            if (NFintake(0) == 0) then
                write(device,AFORMAT) linebreak//red//'Internal error: total no. of new freshmen is 0 ?'//black//linebreak
                return
            end if

            ! rewrite INTAKE file
            call xml_write_intake(pathToTerm)

        end if

        call html_write_header(device, 'Demand for subjects by New Freshmen entering ')

        ! count demand for 1st sem subjects of each specified curriculum
        do idxCURR=1,NumCurricula
            if (NFintake(idxCURR)==0) cycle
            do j=1,Curriculum(idxCURR)%NSubjects
                if (Curriculum(idxCURR)%SubjectTerm(j) /= 1) cycle
                crse = Curriculum(idxCURR)%SubjectIdx(j)
                Offering(crse)%Demand = Offering(crse)%Demand + NFintake(idxCURR)
            end do
        end do

        ! enable edit
        if (is_dean_of_college(targetCollege, orHigherUp)) then

            call make_form_start(device, fnUpdateDemandFreshmen, tCollege)
            do idxCURR=1,NumCurricula
                if (NFintake(idxCURR)/=0) then
                    write(device,AFORMAT) '<input type="hidden" name="'//trim(Curriculum(idxCURR)%Code)// &
                        '" value="'//trim(itoa(NFintake(idxCURR)))//'">'
                end if
            end do
            write(device,AFORMAT) &
                beginitalic//'Change count for <select name="curriculum">', &
                '<option value="select"> -Select curriculum-'
            do idxCURR=1,NumCurricula
                if ( is_dean_of_college(Curriculum(idxCURR)%CollegeIdx, orHigherUp) .and. &
                     Curriculum(idxCURR)%NSubjects/=0) then
                    write(device,AFORMAT) &
                        '<option value="'//trim(Curriculum(idxCURR)%Code)//'"> '//Curriculum(idxCURR)%Code
                end if
            end do
            write(device,AFORMAT) &
                '</select>', &
                nbsp//' to '//nbsp//nbsp//'<input type="text" size="4" name="count" value="">', &
                nbsp//nbsp//'<input type="submit" value="Submit">', &
                enditalic//endform

        end if

        ! write HTML table
        write(device,AFORMAT) '<table border="1" width="100%">'
        write(device,AFORMAT) begintr//'<td valign="bottom">COURSE'//endtd//&
        !'<td align="center" valign="bottom"><p>E'//linebreak//'X'//linebreak//'C'//linebreak//'E'//linebreak//'S'//linebreak//'S</p>'//endtd//&
        !'<td align="center" valign="bottom"><p>A'//linebreak//'V'//linebreak//'A'//linebreak//'I'//linebreak//'L</p>'//endtd//&
        '<td align="center" valign="bottom"><p>D'//linebreak//'E'//linebreak//'M'//linebreak//'A'// &
            linebreak//'N'//linebreak//'D</p>'//endtd
        do idxCURR=1,NumCurricula
            if (NFintake(idxCURR)==0) cycle
            tCurriculum = Curriculum(idxCURR)%Code
            l = len_trim(tCurriculum)
            write(device,'(22a)') '<td align="center" valign="top"><p>'// &
            trim(itoa(NFintake(idxCURR)))//linebreak//nbsp//linebreak, &
            (tCurriculum(k:k)//linebreak, k=1,l-1), tCurriculum(l:l)//'</p>'//endtd
        end do
        ! approximate blocks
        write(device,AFORMAT) endtr//begintr//'<td width="8%">BLOCKS'//endtd//tdnbspendtd
        do idxCURR=1,NumCurricula
            if (NFintake(idxCURR)==0) cycle
            write(device,AFORMAT) tdalignright//trim(itoa((1+NFintake(idxCURR))/25))//endtd
        end do
        write(device,AFORMAT) endtr
        ! write demand per subject
        do crse=1,MAX_ALL_SUBJECTS
            if (Offering(crse)%Demand == 0) cycle
            write(device,'(4a)') begintr//'<td width="8%">'//trim(Subject(crse)%Name)//endtd, &
            !'tdalignright//trim(itoabz(-Offering(crse)%Demand+Offering(crse)%TotalSlots))//endtd, &
            !'tdalignright//trim(itoabz(Offering(crse)%TotalSlots))//endtd, &
            tdalignright//trim(itoabz(Offering(crse)%Demand))//endtd
            do idxCURR=1,NumCurricula
                if (NFintake(idxCURR)==0) cycle
                j =  index_of_subject_in_curriculum (Curriculum(idxCURR), crse)
                if (j==0) then ! crse not in curriculum
                    write(device,AFORMAT) tdnbspendtd
                    cycle
                end if
                if (Curriculum(idxCURR)%SubjectTerm(j)==1) then ! crse is for 1st sem, 1st year
                    write(device,AFORMAT) tdalignright//trim(itoabz(NFintake(idxCURR)))//endtd
                else
                    write(device,AFORMAT) tdnbspendtd
                end if
            end do
            write(device,AFORMAT) endtr
        end do
        write(device,AFORMAT) endtable//horizontal

    end subroutine demand_by_new_freshmen



    subroutine demand_for_subjects (device, thisTerm, NumSections, Section, Offering, eList )
        integer, intent (in) :: device, thisTerm
        integer, intent (in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_OFFERED_SUBJECTS), intent(in out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        type (TYPE_PRE_ENLISTMENT), intent(in) :: eList(0:)

        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer :: ierr, i, j, l, std, owner_dept, owner_coll
        integer :: crse, nlines
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=4) :: tNote
        real, dimension(0:MAX_ALL_SUBJECTS) :: tCount

        ! which department ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)
        targetDepartment = index_to_dept(tDepartment)
        targetCollege = Department(targetDepartment)%CollegeIdx

        call html_comment('demand_for_subjects('//tDepartment//')')

        if (targetDepartment==NumDepartments) then ! SYSAD
            owner_dept = 0
            owner_coll = 0
        else
            owner_dept = targetDepartment
            owner_coll = targetCollege
        end if
        call offerings_summarize(NumSections, Section, Offering, owner_dept)

        ! calculate priority demand, priority accomodated/not accommodated
        Section(:)%RemSlots = Section(:)%Slots

        ! use eList(:)%Contrib for Offering(:)%Demand
        tCount = 0.0
        do std=1,NumStudents+NumAdditionalStudents
#if defined REGIST
#else
            if (owner_coll>0) then ! not SYSAD
                if (owner_coll/=Curriculum(Student(std)%CurriculumIdx)%CollegeIdx) cycle ! student not in target college
            end if
#endif
            ! count all contributions
            do i = 1,eList(std)%NPriority+eList(std)%NAlternates+eList(std)%NCurrent
                j = eList(std)%Subject(i)
                tCount(j) = tCount(j) + eList(std)%Contrib(i)
            end do
        end do

        do j=1,NumSubjects+NumAdditionalSubjects
            Offering(j)%Demand = tCount(j) + 0.55
            Offering(j)%PriorityNotAccommodated = tCount(j) + 0.55
        end do

        ! calculate remaining slots, open sections
        do i=1,NumSections
            if (index(Section(i)%Code,'+')>0) cycle ! an additional schedule
            j = Section(i)%SubjectIdx
            if (j==0) cycle ! deleted
            l = Section(i)%RemSlots
            if (l==0) cycle

            if (targetCollege/=NumColleges) then ! check
                owner_dept = Section(i)%DeptIdx
                owner_coll = Department(owner_dept)%CollegeIdx
                if (owner_dept/=targetDepartment) cycle
            end if

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

        nlines = 0
        call html_write_header(device, 'Demand for subjects and available seats in '//trim(tDepartment)//COMMA)
        write(device,AFORMAT) '<table border="1" width="50%">'
        do crse=1,NumSubjects+NumAdditionalSubjects
#if defined REGIST
            if (Subject(crse)%DeptIdx /= targetDepartment) cycle
#else
            if (targetCollege/=NumColleges) then
                if (.not. is_used_in_college_subject(targetCollege, crse) ) cycle
            end if
#endif
            if (Offering(crse)%OpenSections==0 .and. Offering(crse)%Demand==0) cycle

            if (mod(nlines,20)==0) &
                write(device,AFORMAT) begintr//begintd//beginitalic//'<p>Subject'//linebreak//'</p>'//enditalic//endtd, & ! subject
                    tdalignright//beginitalic//'<p>No. of'//linebreak//'sects</p>'//enditalic//endtd, & ! no. of sections
                    tdalignright//beginitalic//'<p>Total'//linebreak//'seats</p>'//enditalic//endtd, & ! total seats
                    tdalignright//beginitalic//'<p>Priority'//linebreak//'demand</p>'//enditalic//endtd ! priority demand

            if (Offering(crse)%NSections>0) then
                tNote = SPACE
            else
                tNote = ' (*)'
            end if
            tSubject = Subject(crse)%Name
            write(device,AFORMAT)  begintr// &
                begintd//trim(tSubject)//tNote//endtd// & ! subject
                tdalignright//trim(itoa(Offering(crse)%NSections))//endtd// & ! no. of sections, total seats
                tdalignright//trim(itoa(Offering(crse)%TotalSlots))//endtd// & ! total seats
                !tdalignright//trim(itoa(Offering(crse)%Demand))//endtd//endtr ! priority demand
                trim(make_href(fnPotentialStudents, itoa(Offering(crse)%Demand), &
                A1=tSubject, A2=Department(targetDepartment)%Code, A9=thisTerm, pre=tdalignright, post=endtd//endtr ))
            nlines = nlines+1
        end do
        write(device,AFORMAT) endtable, &
            linebreak//'Legends:', &
            linebreak//beginitalic//'No. of sects'//enditalic//' = number of sections/labs offered', &
            linebreak//beginitalic//'Total seats'//enditalic//' = total seats available', &
            linebreak//beginitalic//'Priority demand'//enditalic//' = number of students who need the subject', &
            linebreak//'(*) = no sections open; subject may be needed by graduating students'
        write(device,AFORMAT) horizontal

        ! reset Offering()
        call offerings_summarize(NumSections, Section, Offering)

    end subroutine demand_for_subjects


    subroutine list_potential_students(device, thisTerm, eList)
        integer, intent (in) :: device, thisTerm
        type (TYPE_PRE_ENLISTMENT), intent(in) :: eList(0:)
        integer :: n_count, tdx, std, ierr, ncol
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        ! which subject ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tSubject, ierr)
        targetSubject = index_to_subject(tSubject)
#if defined REGIST
        targetDepartment = Subject(targetSubject)%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx
        tDepartment = Department(targetDepartment)%Code
#else
        call cgi_get_named_string(QUERY_STRING, 'A2', tDepartment, ierr)
        targetDepartment = index_to_dept(tDepartment)
        targetCollege = Department(targetDepartment)%CollegeIdx
#endif
        n_count = 0
        !tArray = 0
        do tdx=1,NumStudents+NumAdditionalStudents
            std = StdRank(tdx)
#if defined REGIST
#else
            if (targetCollege/=NumColleges) then
                if (targetCollege/=Curriculum(Student(std)%CurriculumIdx)%CollegeIdx) cycle ! student not in target college
            end if
#endif
            do ncol=1,eList(std)%lenSubject
                if (targetSubject/=eList(std)%Subject(ncol)) cycle  ! not this subject
                if (eList(std)%Contrib(ncol)==0.0) cycle  ! alternate
                if (eList(std)%Section(ncol)>0) cycle     ! accommodated
                n_count = n_count+1
                tArray(n_count) = std
            end do
        end do
        call html_write_header(device, 'Demand for '//trim(Subject(targetSubject)%Name)// &
            ' in '//trim(tDepartment)//COMMA)
        call list_students(device, thisTerm, n_count, tArray, targetSubject, eList)
        write(device,AFORMAT) horizontal

    end subroutine list_potential_students


end module EditCHECKLIST
