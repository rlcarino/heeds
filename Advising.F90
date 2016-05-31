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


    subroutine advise_student (std, thisTerm, Advice, advising_comment)

        integer, intent (in) :: std, thisTerm
        type (TYPE_PRE_ENLISTMENT), intent (out)  :: Advice
        character(len=*), intent (in out) :: advising_comment

        integer :: idxCURR, idxCOLL, idx, tdx
        integer :: codeConditional, codeNSTP, codePriority, codeScholastic, codeAdvising
        integer :: NPriority, NAlternates, NCurrent, StdClassification, StdTerm, StdYear, NRemaining, MissingPOCW
        real :: AllowedLoad, UnitsEarned, UnitsPaid, UnitsDropped, UnitsPassed

        call read_student_records (std)

        call initialize_pre_enlistment(Advice)
        advising_comment = SPACE

        idxCURR = Student(std)%CurriculumIdx
        idxCOLL = Curriculum(idxCURR)%CollegeIdx
        CheckList = Curriculum(idxCURR)
        CLExt = TYPE_CHECKLIST_EXTENSION (0, 0, 0, 0, 0, 0, 0.0, .false., .false., .false., .false., &
            SPACE, SPACE, SPACE, SPACE, SPACE, SPACE, SPACE)

        if (Curriculum(idxCURR)%NSubjects==0) then ! no subjects specified in curriculum
            Advice%statusAdvising = ADVISING_NO_SUBJECTS
            return
        end if

        ! previous full term
        select case (currentTerm)
            case (1)
                StdTerm = 2
                StdYear = currentYear-1
            case (2)
                StdTerm = 1
                StdYear = currentYear
            case (3)
                StdTerm = 2
                StdYear = currentYear
        end select
        call get_scholastic_performance (StdYear, StdTerm, UnitsPaid, UnitsDropped, UnitsPassed, codeScholastic)

        call checklist_analyze (std, thisTerm, &
            codePriority, codeScholastic, codeAdvising, codeConditional, codeNSTP, &
            UnitsEarned, StdClassification, StdTerm, StdYear, MissingPOCW, &
            AllowedLoad, NPriority, NAlternates, NCurrent, NRemaining, advising_comment)

        if (NPriority==0) then ! no priority subjects?
            NAlternates = 0
            NCurrent = 0
        else if (AllowedLoad==0) then ! 0-unit subjects left?
            NPriority = 0
            NAlternates = 0
            NCurrent = 0
        end if

        Advice%UnitsEarned = UnitsEarned
        Advice%levelClassification = StdClassification
        Advice%levelYear = StdYear
        Advice%AllowedLoad = AllowedLoad
        Advice%levelPriority = codePriority
        Advice%statusAdvising = codeAdvising
        Advice%statusScholastic = codeScholastic
        Advice%codeNSTP = codeNSTP
        Advice%codeConditional = codeConditional
        Advice%NPriority = NPriority
        Advice%NAlternates = NAlternates
        Advice%NCurrent = NCurrent
        Advice%NRemaining = NRemaining
        Advice%MissingPOCW = MissingPOCW
        Advice%lenSubject = NPriority + NAlternates + NCurrent

        ! recommended subjects
        do idx=1,Advice%lenSubject
            tdx = CLExt(idx)%PriorityRank
            Advice%Subject(idx) = CheckList%SubjectIdx(tdx)
            Advice%Contrib(idx) = CLExt(tdx)%Contrib
        end do

        ! check is advised subjects are regular
        if (codeAdvising==ADVISING_IRREGULAR) then
            call check_for_regular_advised_subjects(idxCURR, thisTerm, Advice)
        end if

        return
    end subroutine advise_student


    subroutine checklist_display  (device, std, Advice)
        integer, intent (in) :: std, device
        type (TYPE_PRE_ENLISTMENT), intent (in) :: Advice
        integer :: idx, tdx, iYear, iTerm, m, n, rank, idxCURR, k, l, codeScholastic, collegeOfStudent
        integer, dimension(MAX_LEN_STUDENT_RECORD) :: p, q
        real :: UnitsPaid, UnitsDropped, UnitsPassed, tUnits
        !character(len=6) :: tProb

        call html_comment('checklist_display()')

        idxCURR = Student(std)%CurriculumIdx
        collegeOfStudent = Curriculum(idxCURR)%CollegeIdx

        k = Curriculum(idxCURR)%NumTerms
        tUnits = SpecifiedUnits(k)
        write(device,AFORMAT) b_bold//'Units to earn classifications'//e_bold, &
            (': '//nbsp//trim(txtStanding(l))//'<'//trim(itoa(int(0.25*l*tUnits+0.5))), l=1,4)
        write(device,AFORMAT) linebreak//b_bold//'Units to achieve YEAR'//e_bold, &
            (': '//nbsp//trim(txtYear(l))//'<'//trim(ftoa(SpecifiedUnits(3*l),1)), l=1,(k+2)/3)

        write(device,AFORMAT) '<table border="0" width="100%">'
        do tdx=1,CheckList%NumTerms,3
            call rank_to_year_term(tdx, iYear, iTerm)
            write(device,AFORMAT) b_tr//'<td colspan="5">'//linebreak//b_bold//trim(txtYear(iYear))// &
                ' Year, '//txtSemester(iTerm)//termQualifier(iTerm)//e_bold//' ('//trim(ftoa(TermUnits(tdx),1))// &
                ' units)'//e_td// &
                b_td_nbsp_e_td//'<td colspan="5">'//linebreak//b_bold//trim(txtYear(iYear))// &
                ' Year, '//txtSemester(iTerm+1)//termQualifier(iTerm+1)//e_bold//' ('//trim(ftoa(TermUnits(tdx+1),1))// &
                ' units)'// &
                e_td//e_tr
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
                    b_tr// &
                    b_td//trim(CLExt(p(idx))%Disp_Subject)//e_td//&
                    b_td//trim(CLExt(p(idx))%Disp_Comment)//e_td//&
                    b_td//trim(CLExt(p(idx))%Disp_Grade)//e_td//&
                    b_td//CLExt(p(idx))%Disp_Units//e_td//&
                    b_td//CLExt(p(idx))%Disp_Remarks//e_td//&
                    b_td_nbsp_e_td// &
                    b_td//trim(CLExt(q(idx))%Disp_Subject)//e_td//&
                    b_td//trim(CLExt(q(idx))%Disp_Comment)//e_td//&
                    b_td//trim(CLExt(q(idx))%Disp_Grade)//e_td//&
                    b_td//CLExt(q(idx))%Disp_Units//e_td// &
                    b_td//CLExt(q(idx))%Disp_Remarks//e_td//&
                    e_tr
            end do
            if (m > n) then
                do idx=n+1,m
                    write(device,AFORMAT) &
                        b_tr// &
                        b_td//trim(CLExt(p(idx))%Disp_Subject)//e_td//&
                        b_td//trim(CLExt(p(idx))%Disp_Comment)//e_td//&
                        b_td//trim(CLExt(p(idx))%Disp_Grade)//e_td//&
                        b_td//CLExt(p(idx))%Disp_Units//e_td//&
                        b_td//CLExt(p(idx))%Disp_Remarks//e_td//&
                        '<td colspan="6">'//nbsp//e_td//&
                        e_tr
                end do
            end if
            if (n > m) then
                do idx=m+1,n
                    write(device,AFORMAT) &
                        b_tr// &
                        '<td colspan="6">'//nbsp//e_td// &
                        b_td//trim(CLExt(q(idx))%Disp_Subject)//e_td//&
                        b_td//trim(CLExt(q(idx))%Disp_Comment)//e_td//&
                        b_td//trim(CLExt(q(idx))%Disp_Grade)//e_td//&
                        b_td//CLExt(q(idx))%Disp_Units//e_td//&
                        b_td//CLExt(q(idx))%Disp_Remarks//e_td//&
                        e_tr
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
                !write(device,AFORMAT) SPACE, SPACE//trim(txtYear(iYear))//' Year, Summer'
                write(device,AFORMAT) b_tr//'<td colspan="5">'//linebreak//b_bold//trim(txtYear(iYear))// &
                    ' Year, '//txtSemester(iTerm+2)//termQualifier(iTerm+2)//e_bold//' ('//trim(ftoa(TermUnits(tdx+2),1))// &
                    ' units)'//e_td// &
                    '<td colspan="6">'//nbsp//e_td//e_tr
                do idx=1,m
                    write(device,AFORMAT) &
                        b_tr// &
                        b_td//trim(CLExt(p(idx))%Disp_Subject)//e_td//&
                        b_td//trim(CLExt(p(idx))%Disp_Comment)//e_td//&
                        b_td//trim(CLExt(p(idx))%Disp_Grade)//e_td//&
                        b_td//CLExt(p(idx))%Disp_Units//e_td//&
                        b_td//CLExt(p(idx))%Disp_Remarks//e_td//&
                        '<td colspan="6">'//nbsp//e_td//&
                        e_tr
                end do
            end if
        end do
        write(device,AFORMAT) e_table, &
            linebreak//b_bold//'LEGENDS'//e_bold//' for grades and remarks:'//nbsp//' * - specify subject or PE activity;'//&
            nbsp//' # - failed;'//nbsp//' % - conditional;'//nbsp//' PASS - earned or transfer/advance credit;', &
            linebreak//nbsp//' (NG) - registered, no grade, assume earned for advising purposes;', &
            nbsp//' PriN - Nth priority subject;'//nbsp//' AltK - Kth alternate subject'

        call get_scholastic_performance (cTm3Year, cTm3, UnitsPaid, UnitsDropped, UnitsPassed, codeScholastic)
        write(device,AFORMAT) linebreak//linebreak//b_bold//'SUMMARY for '//text_term_school_year(cTm3+6,cTm3Year), &
            e_bold//': Units registered='//trim(ftoa(UnitsPaid,1)), &
            ': '//nbsp//' Dropped='//trim(ftoa(UnitsDropped,1)), &
            ': '//nbsp//' Earned='//trim(ftoa(UnitsPassed,1)), &
            ': '//nbsp//' Scholastic standing='//trim(txtScholastic(codeScholastic))

        call get_scholastic_performance (cTm2Year, cTm2, UnitsPaid, UnitsDropped, UnitsPassed, codeScholastic)
        write(device,AFORMAT) linebreak//b_bold//'SUMMARY for '//text_term_school_year(cTm2+6,cTm2Year), &
            e_bold//': Units registered='//trim(ftoa(UnitsPaid,1)), &
            ': '//nbsp//' Dropped='//trim(ftoa(UnitsDropped,1)), &
            ': '//nbsp//' Earned='//trim(ftoa(UnitsPassed,1)), &
            ': '//nbsp//' Scholastic standing='//trim(txtScholastic(codeScholastic))

        call get_scholastic_performance (cTm1Year, cTm1, UnitsPaid, UnitsDropped, UnitsPassed, codeScholastic)
        write(device,AFORMAT) linebreak//b_bold//'SUMMARY for '//text_term_school_year(cTm1+6,cTm1Year), &
            e_bold//': Units registered='//trim(ftoa(UnitsPaid,1)), &
            ': '//nbsp//' Dropped='//trim(ftoa(UnitsDropped,1)), &
            ': '//nbsp//' Earned='//trim(ftoa(UnitsPassed,1)), &
            ': '//nbsp//' Scholastic standing='//trim(txtScholastic(codeScholastic))

        if (Advice%MissingPOCW>0) then
            write(device,AFORMAT) linebreak//linebreak//red//b_bold//'MISSING ELECTIVEs/entries in Plan of Study = '// &
                trim(itoa(Advice%MissingPOCW))//e_bold//e_color
        end if

        ! extra subjects
        n = 0
        q = 0
        do idx=1,lenTCG
            if (TCG(idx)%Code<2 .or. TCG(idx)%Used) cycle
            if (TCG(idx)%Subject /= 0 .and. isGrade_passing(TCG(idx)%Grade)) then
                n = n+1
                q(n) = idx
            end if
        end do
        if (n > 0) then
            write(device,AFORMAT) b_para, b_bold//'EXTRA subjects'//e_bold//' or unused units after substitutions'
            do m=1,n
                write(device,AFORMAT) ' : '//Subject(TCG(q(m))%Subject)%Name//DASH//txtGrade(pGrade(TCG(q(m))%Grade))
            end do
            write(device,AFORMAT) e_para
        end if

        if (Advice%codeNSTP/=0) write(device,AFORMAT) b_para, red//b_bold//'Check NSTP!'//e_bold//e_color, e_para

        ! assume enlistment period
        write(device,AFORMAT) b_para, b_bold//'ASSUMPTION for start of '// &
            text_term_school_year(currentTerm+6,currentYear), &
            e_bold//': Units earned='//trim(ftoa(Advice%UnitsEarned,1))//FSLASH//trim(ftoa(tUnits,1)), &
            ': '//nbsp//' Classification (25% rule) ='//trim(txtStanding(Advice%levelClassification)), &
            ': '//nbsp//' Year in curriculum='//trim(txtYear(Advice%levelYear)), e_para

    end subroutine checklist_display


    subroutine substitution_form(device, std)
        integer, intent (in) :: std, device
        integer :: idx, tdx, iYear, iTerm, m, n, rank
        integer, dimension(MAX_LEN_STUDENT_RECORD) :: p, q

        write(device,AFORMAT) '<a name="Update PLAN"></a>'
        call checklist_links(device)

        call make_form_start(device, fnEditCheckList, Student(std)%StdNo)
        write(device,AFORMAT) b_bold//'PLAN OF STUDY update form'//e_bold, &
            linebreak//b_italic//'Enter or modify contents of the edit boxes below, then click "Update PLAN"'//e_italic// &
            '<table border="0" width="90%">'
        do tdx=1,CheckList%NumTerms,3
            call rank_to_year_term(tdx, iYear, iTerm)
            write(device,AFORMAT) b_tr//'<td colspan="4">'//linebreak//b_bold//trim(txtYear(iYear))// &
                ' Year, '//txtSemester(iTerm)//termQualifier(iTerm)//e_bold// &
                ' ('//trim(ftoa(TermUnits(tdx),1))//' units)'//e_td// &
                b_td_nbsp_e_td//'<td colspan="4">'//linebreak//b_bold//trim(txtYear(iYear))// &
                ' Year, '//txtSemester(iTerm+1)//termQualifier(iTerm+1)//e_bold// &
                ' ('//trim(ftoa(TermUnits(tdx+1),1))//' units)'//e_td//e_tr

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
                write(device,AFORMAT) b_tr//b_td//trim(CLExt(p(idx))%Disp_Subject)//e_td//&
                    b_td//trim(CLExt(p(idx))%Disp_Input_Elective)//e_td//&
                    b_td//trim(CLExt(p(idx))%Disp_Grade)//e_td//&
                    b_td//trim(CLExt(p(idx))%Disp_Units)//e_td//b_td_nbsp_e_td, &
                    b_td//trim(CLExt(q(idx))%Disp_Subject)//e_td//&
                    b_td//trim(CLExt(q(idx))%Disp_Input_Elective)//e_td//&
                    b_td//trim(CLExt(q(idx))%Disp_Grade)//e_td//&
                    b_td//trim(CLExt(q(idx))%Disp_Units)//e_td//e_tr
            end do
            if (m > n) then
                do idx=n+1,m
                    write(device,AFORMAT) b_tr//b_td//trim(CLExt(p(idx))%Disp_Subject)//e_td//&
                        b_td//trim(CLExt(p(idx))%Disp_Input_Elective)//e_td//&
                        b_td//trim(CLExt(p(idx))%Disp_Grade)//e_td//&
                        b_td//trim(CLExt(p(idx))%Disp_Units)//e_td//&
                        '<td colspan="5">'//nbsp//e_td//e_tr
                end do
            end if
            if (n > m) then
                do idx=m+1,n
                    write(device,AFORMAT) b_tr//'<td colspan="5">'//nbsp//e_td// &
                        b_td//trim(CLExt(q(idx))%Disp_Subject)//e_td//&
                        b_td//trim(CLExt(q(idx))%Disp_Input_Elective)//e_td//&
                        b_td//trim(CLExt(q(idx))%Disp_Grade)//e_td//&
                        b_td//trim(CLExt(q(idx))%Disp_Units)//e_td//e_tr
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
                write(device,AFORMAT) b_tr//'<td colspan="4">'//linebreak//b_bold//trim(txtYear(iYear))// &
                    ' Year, '//txtSemester(iTerm+2)//termQualifier(iTerm+2)//e_bold// &
                    ' ('//trim(ftoa(TermUnits(tdx+2),1))//' units)'//e_td// &
                    '<td colspan="5">'//nbsp//e_td//e_tr
                do idx=1,m
                    write(device,AFORMAT) b_tr//b_td//trim(CLExt(p(idx))%Disp_Subject)//e_td//&
                        b_td//trim(CLExt(p(idx))%Disp_Input_Elective)//e_td//&
                        b_td//trim(CLExt(p(idx))%Disp_Grade)//e_td//&
                        b_td//trim(CLExt(p(idx))%Disp_Units)//e_td//&
                        '<td colspan="5">'//nbsp//e_td//e_tr
                end do
            end if
        end do
        write(device,AFORMAT) e_table, &
            linebreak//'<input type="submit" name="action" value="Update PLAN">'//e_form

    end subroutine substitution_form


    subroutine checklist_analyze (std, thisTerm, &
        codePriority, codeScholastic, codeAdvising, codeConditional, codeNSTP, &
        UnitsEarned, StdClassification, StdTerm, StdYear, MissingPOCW, &
        AllowedLoad, NPriority, NAlternates, NCurrent, NRemaining, advising_comment)

        ! Routine determines the remaining subjects to be taken by a student based on curriculum,
        !   enrollment record, plan of study, and thisTerm

        implicit none
        integer, intent (in) :: std, thisTerm, codeScholastic
        integer, intent (out) :: codePriority, codeAdvising, codeConditional, codeNSTP
        integer, intent (out) :: NPriority, NAlternates, NCurrent, NRemaining, &
            StdClassification, StdTerm, StdYear, MissingPOCW
        real, intent (out) :: AllowedLoad, UnitsEarned
        character(len=*), intent (out) :: advising_comment

        integer :: idxCURR
#ifdef DBperformance
        integer :: StdPerf, PERFUnitsEnrolled, PERFUnitsEarned ! (from raw TCG)
#endif
        real :: UnitsExpected, UnitsTarget, UnitsFailed, UnitsConditional, UnitsToRepeat
        real :: PercentEarned, PredictedLoad, addnl, credit

        !integer, dimension(0:MAX_SUBJECTS_IN_CURRICULUM,0:MAX_ALL_SUBJECT_PREREQ) :: CLPreq, CPPreq
        integer, dimension(0:MAX_SUBJECTS_IN_CURRICULUM) :: EarlyTime, LateTime, Slack
        integer :: iYear, iTerm, Latest, Earliest, tCount
        integer :: ntokens
        integer, dimension(0:MAX_ALL_SUBJECT_PREREQ) :: tmpPreq
        real, dimension(0:MAX_ALL_SUBJECT_PREREQ) :: prob
        real :: fCredit, pLoad ! probabilistic load
        logical :: OKSubst, FlagIsUp, AddCoReq1, AddCoReq2, UseClasses
        logical :: passedNSTP11A, passedNSTP11B, passedNSTP11C
        logical :: passedNSTP12A, passedNSTP12B, passedNSTP12C

        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject, token
        character (len=MAX_LEN_SUBJECT_CODE) :: input_name1, input_name2, input_value
        integer :: i, j, k, l, m, n
        integer :: cdx, gdx, idx, jdx, kdx, tdx
        integer :: crse, coreq, concpreq, rank, now
        integer, dimension(MAX_LEN_STUDENT_RECORD) :: p, q
        !integer :: ddx
#ifdef DBprereq
        character (len=MAX_LEN_SUBJECT_CODE) :: tNote
#endif

        now = 3*(CurrentYear - BaseYear + 1) + CurrentTerm
        idxCURR = Student(std)%CurriculumIdx
        UseClasses = College(Curriculum(idxCURR)%CollegeIdx)%isAllowed(AdviseOpenSubjects,thisTerm)
        AllowedLoad = 0
        codeConditional = 0
        codeNSTP = 0
        codePriority = PRIORITY_GOOD
        codeAdvising = ADVISING_NO_SUBJECTS
        StdYear = YEAR_NOT_SET
        StdClassification = YEAR_LEVEL_NOT_SET
        NPriority = 0
        NAlternates = 0
        NRemaining = 0
        NCurrent = 0
        PredictedLoad = 0
        UnitsToRepeat = 0
        UnitsExpected = 0.0
        UnitsEarned = 0
        UnitsTarget = 0
        PercentEarned = 0.0
        UnitsConditional = 0

        passedNSTP11A = .false.
        passedNSTP11B = .false.
        passedNSTP11C = .false.
        passedNSTP12A = .false.
        passedNSTP12B = .false.
        passedNSTP12C = .false.
        advising_comment = SPACE

        ! use ReExam grades if available
        do tdx = 1,lenTCG
            if (TCG(tdx)%Code<2) cycle ! not a grade
            if (TCG(tdx)%ReExam/=0) then
                !call html_comment(Subject(TCG(tdx)%Subject)%Name//txtGrade(pGrade(TCG(tdx)%Grade))// &
                !    'has ReExam '//txtGrade(pGrade(TCG(tdx)%ReExam)))
                TCG(tdx)%Grade = TCG(tdx)%ReExam
            end if
        end do

        do tdx = 1,lenTCG
            if (TCG(tdx)%Code<2) cycle ! not a grade
            tSubject = Subject(TCG(tdx)%Subject)%Name
            gdx = TCG(tdx)%Grade
            select case (trim(tSubject))
                case ('NSTP 11A')
                    if ( isGrade_numeric_pass(gdx) ) then
                        passedNSTP11A = .true.
                        advising_comment = ' : Passed '//tSubject
                    end if
                case ('NSTP 11B')
                    if ( isGrade_numeric_pass(gdx) ) then
                        passedNSTP11B = .true.
                        advising_comment = ' : Passed '//tSubject
                    end if
                case ('NSTP 11C')
                    if ( isGrade_numeric_pass(gdx) ) then
                        passedNSTP11C = .true.
                        advising_comment = ' : Passed '//tSubject
                    end if
                case ('NSTP 12A')
                    if ( isGrade_numeric_pass(gdx) ) then
                        passedNSTP12A = .true.
                        advising_comment = trim(advising_comment)//' : Passed '// tSubject
                    end if
                case ('NSTP 12B')
                    if ( isGrade_numeric_pass(gdx) ) then
                        passedNSTP12B = .true.
                        advising_comment = trim(advising_comment)//' : Passed '// tSubject
                    end if
                case ('NSTP 12C')
                    if ( isGrade_numeric_pass(gdx) ) then
                        passedNSTP12C = .true.
                        advising_comment = trim(advising_comment)//' : Passed '// tSubject
                    end if
            end select
        end do
        ! NSTP 11 & 12 both passed ?
        if ( (passedNSTP11A .or. passedNSTP11B .or. passedNSTP11C) .and. &
            (passedNSTP12A .or. passedNSTP12B .or. passedNSTP12C) ) then
            if (passedNSTP11A .and. passedNSTP12A) then
                advising_comment = SPACE
            else if (passedNSTP11B .and. passedNSTP12B) then
                advising_comment = SPACE
            else if (passedNSTP11C .and. passedNSTP12C) then
                advising_comment = SPACE
            else
                advising_comment = 'NSTP component mismatch '//advising_comment
                codeNSTP = 1
            end if
        else
            advising_comment = SPACE
        end if

        ! calculate prescribed units per term
        TermUnits = 0.0
        do kdx=1,CheckList%NSubjects
            rank = CheckList%SubjectTerm(kdx)
            crse = CheckList%SubjectIdx(kdx)
            TermUnits(rank) = TermUnits(rank) + Subject(crse)%Units
        end do

        ! parse PLAN OF STUDY/SUBSTITUTIONS
        n = 0 ! entries in plan of study for later binding
        p = 0
        loop_POS : &
        do tdx = 1,lenTCG
            if (TCG(tdx)%Code/=1) cycle ! not plan of study
            if (TCG(tdx)%Used) cycle      ! already used for PE 2, GE(XXX)
            if (TCG(tdx)%ErrorCode > 1) cycle
            !     if (Reqd(1) == ADDITIONAL) then
            !       ADD Subst(1) to Year,Term in curriculum
            if (TCG(tdx)%Reqd(0) == 1) then
                token = Subject(TCG(tdx)%Reqd(1))%Name

#ifdef DBsubst
                call html_comment('? substitute '//token)
#endif
                if (token == 'ADDITIONAL') then
                    i = CheckList%NSubjects
                    do j = 1,TCG(tdx)%Subst(0)
                        CheckList%NSubjects = i+j
                        CheckList%SubjectTerm(i+j) = TCG(tdx)%Term
                        CheckList%SubjectIdx(i+j) = TCG(tdx)%Subst(j)
                        CLExt(i+j)%Disp_Subject = token
                        input_value = Subject(TCG(tdx)%Subst(1))%Name
                        input_name2 = fn_blank_to_underscore(input_value)
                        CLExt(i+j)%Disp_Comment = input_value
                        CLExt(i+j)%Disp_Input_Elective = '<input type="text" size="12" name="'// &
                            'ADDITIONAL-'//trim(itoa(CheckList%SubjectTerm(i+j)))//':'// &
                            trim(input_name2)//'" value="'//trim(input_value)//'">'
                    end do
                    TCG(tdx)%Used = .true.
                !     else if (Reqd(1) is a dummy subject) then
                !       REPLACE Reqd(1) in Year,Term of curriculum with Subst(1)
                else if (TCG(tdx)%Reqd(1) < 0) then
                    k = 0
                    do i=1,CheckList%NSubjects
                        if (CheckList%SubjectTerm(i) == TCG(tdx)%Term .and. &
                                CheckList%SubjectIdx(i) == TCG(tdx)%Reqd(1)) then
                            k = i
                            exit
                        end if
                    end do
                    if (k > 0) then ! found exact position in curriculum
                        CheckList%SubjectIdx(k) = TCG(tdx)%Subst(1)
                        input_value = Subject(TCG(tdx)%Subst(1))%Name
#ifdef DBsubst
                        call html_comment('   ===> '//input_value)
#endif
                        input_name1 = fn_blank_to_underscore(token)
                        input_name2 = fn_blank_to_underscore(input_value)
                        CLExt(k)%Disp_Subject = token
                        CLExt(k)%Disp_Comment = input_value
                        CLExt(k)%Disp_Input_Elective = '<input type="text" size="12" name="'// &
                            trim(input_name1)//DASH//trim(itoa(CheckList%SubjectTerm(k)))//':'// &
                            trim(input_name2)//'" value="'//trim(input_value)//'">'
                        TCG(tdx)%Used = .true.
                    else ! postpone binding
                        ! not found, or extra dummy subject; save for later processing
#ifdef DBsubst
                        call html_comment('   not found, or extra; for later ')
#endif

                        p(n+1) =  TCG(tdx)%Reqd(1)
                        p(n+2) = TCG(tdx)%Subst(1)
                        n = n+2
                    end if
                !     else if (Reqd(1) is a named subject) then
                !       REPLACE Reqd(1) with Subst(1..n)
                !     end if
                else ! if (TCG(tdx)%Reqd(1) > 0) then ! explicit substitution
                    k = 0
                    do i=1,CheckList%NSubjects
                        if (CheckList%SubjectIdx(i) == TCG(tdx)%Reqd(1)) then
                            k = i
                            exit
                        end if
                    end do
                    if (k > 0) then ! found exact position in curriculum
                        CheckList%SubjectIdx(k) = TCG(tdx)%Subst(1)
                        CLExt(k)%Disp_Comment = token
                        CLExt(k)%Disp_Input_Elective = token
                        i = CheckList%NSubjects
#ifdef DBsubst
                        call html_comment('   '//token//'for '//Subject(TCG(tdx)%Subst(1))%Name)
#endif
                        do j = 2,TCG(tdx)%Subst(0)
                            CheckList%NSubjects = i+j-1
                            CheckList%SubjectTerm(i+j-1) = CheckList%SubjectTerm(k)
                            CheckList%SubjectIdx(i+j-1) = TCG(tdx)%Subst(j)
                            CLExt(i+j-1)%Disp_Comment = token
#ifdef DBsubst
                            call html_comment('   '//token//'for '//Subject(TCG(tdx)%Subst(j))%Name)
#endif
                            !input_value = Subject(TCG(tdx)%Subst(j))%Name
                            !input_name2 = fn_blank_to_underscore(input_value)
                            !input_name1 = fn_blank_to_underscore(token)
                            !CLExt(i+j-1)%Disp_Subject = 'SUBST FOR '//token
                            !CLExt(i+j-1)%Disp_Input_Elective = '<input type="text" size="12" name="'// &
                            !  trim(input_name1)//DASH//trim(itoa(CheckList%SubjectTerm(k)))//':'// &
                            !  trim(input_name2)//'" value="'//trim(input_value)//'">'
                        end do
                        TCG(tdx)%Used = .true.

#ifdef DBsubst
                        call html_comment('   (explicit substitution)')
#endif

                    else ! not found; duplicate entry?
#ifdef DBsubst
                        call html_comment('   (not found, or duplicate)')
#endif

                    end if
                end if
            !   else {group substitution}
            else
                !     do i=1,min(m,n)
                !       REPLACE Reqd(i) with Susbt(i)
                !     end do
                do cdx=1,min( TCG(tdx)%Reqd(0), TCG(tdx)%Subst(0) )
                    token = Subject(TCG(tdx)%Reqd(cdx))%Name
                    k = 0
                    do i=1,CheckList%NSubjects
                        if (CheckList%SubjectIdx(i) == TCG(tdx)%Reqd(cdx)) then
                            k = i
                            exit
                        end if
                    end do
                    if (k > 0) then ! found exact position in curriculum
                        CheckList%SubjectIdx(k) = TCG(tdx)%Subst(cdx)
                        CLExt(k)%Disp_Comment = token
                        CLExt(k)%Disp_Input_Elective = token
                    !else ! not found; duplicate entry?
                    end if
                end do
                !     do i=min(m,n)+1,m
                !       REMOVE Reqd(i)
                !     end do
                do cdx=min( TCG(tdx)%Reqd(0), TCG(tdx)%Subst(0) )+1, TCG(tdx)%Reqd(0)
                    k = 0
                    do i=1,CheckList%NSubjects
                        if (CheckList%SubjectIdx(i) == TCG(tdx)%Reqd(cdx)) then
                            k = i
                            exit
                        end if
                    end do
                    if (k > 0) then ! found exact position in curriculum
                        CheckList%SubjectIdx(k) = 0
                        CheckList%SubjectTerm(k) = 0
                    end if
                end do
                !     do i=min(m,n)+1,n
                !       ADD Subst(i) to Year,Term in curriculum
                !     end do
                do cdx=min( TCG(tdx)%Reqd(0), TCG(tdx)%Subst(0) )+1, TCG(tdx)%Subst(0)
                    i = CheckList%NSubjects+1
                    CheckList%NSubjects = i
                    CheckList%SubjectTerm(i) = TCG(tdx)%Term
                    CheckList%SubjectIdx(i) = TCG(tdx)%Subst(cdx)
                    CLExt(i)%Disp_Comment = 'SUBSTITUTE'
                    CLExt(i)%Disp_Input_Elective = 'SUBSTITUTE'
                end do
                TCG(tdx)%Used = .true.
            !   end if

            end if

        end do loop_POS

        if (n>0) then
            ! bind summer seasonal subjects in PLAN OF STUDY
            do kdx=1,CheckList%NSubjects
                if (CheckList%SubjectIdx(kdx) < 0) then
                    k = mod(CheckList%SubjectTerm(kdx),3) ! term in curriculum
                    if (k == 0) k = 4  ! summer only
                    do i=2,n,2
                        if (p(i-1) == CheckList%SubjectIdx(kdx)) then
                            if (Subject(p(i))%TermOffered == k) then
                                CheckList%SubjectIdx(kdx) = p(i)
                                input_value = Subject(p(i))%Name
                                input_name1 = fn_blank_to_underscore(Subject(p(i-1))%Name)
                                input_name2 = fn_blank_to_underscore(input_value)
                                CLExt(kdx)%Disp_Subject = Subject(p(i-1))%Name
                                CLExt(kdx)%Disp_Comment = input_value
                                CLExt(kdx)%Disp_Input_Elective = '<input type="text" size="12" name="'// &
                                trim(input_name1)//DASH//trim(itoa(CheckList%SubjectTerm(kdx)))//':'// &
                                trim(input_name2)//'" value="'//trim(input_value)//'">'
                                p(i) = 0
                                p(i-1) = 0
                                exit
                            end if
                        end if
                    end do
                end if
            end do

            ! bind other subjects in PLAN OF STUDY
            do kdx=1,CheckList%NSubjects
                if (CheckList%SubjectIdx(kdx) < 0) then
                    k = mod(CheckList%SubjectTerm(kdx),3) ! term in curriculum of dummy
                    do i=2,n,2
                        if (p(i-1) == CheckList%SubjectIdx(kdx)) then
                            j = Subject(p(i))%TermOffered
                            FlagIsUp = j == 7 .or. &
                            (k==1 .and. (j==1 .or. j==3 .or. j==5) ) .or. &
                            (k==2 .and. (j==2 .or. j==3 .or. j==6) ) .or. &
                            (k==0 .and. (j==4 .or. j==5 .or. j==6) )
                            if (FlagIsUp) then
                                CheckList%SubjectIdx(kdx) = p(i)
                                input_value = Subject(p(i))%Name
                                input_name2 = fn_blank_to_underscore(input_value)
                                input_name1 = fn_blank_to_underscore(Subject(p(i-1))%Name)
                                CLExt(kdx)%Disp_Subject = Subject(p(i-1))%Name
                                CLExt(kdx)%Disp_Comment = input_value
                                CLExt(kdx)%Disp_Input_Elective = '<input type="text" size="12" name="'// &
                                trim(input_name1)//DASH//trim(itoa(CheckList%SubjectTerm(kdx)))//':'// &
                                trim(input_name2)//'" value="'//trim(input_value)//'">'
                                p(i) = 0
                                p(i-1) = 0
                                exit
                            end if
                        end if
                    end do
                end if
            end do

            ! left overs
            do kdx=1,CheckList%NSubjects
                if (CheckList%SubjectIdx(kdx) < 0) then
                    do i=2,n,2
                        if (p(i-1) == CheckList%SubjectIdx(kdx)) then
                            CheckList%SubjectIdx(kdx) = p(i)
                            input_value = Subject(p(i))%Name
                            input_name2 = fn_blank_to_underscore(input_value)
                            input_name1 = fn_blank_to_underscore(Subject(p(i-1))%Name)
                            CLExt(kdx)%Disp_Subject = Subject(p(i-1))%Name
                            CLExt(kdx)%Disp_Comment = input_value
                            CLExt(kdx)%Disp_Input_Elective = '<input type="text" size="12" name="'// &
                            trim(input_name1)//DASH//trim(itoa(CheckList%SubjectTerm(kdx)))//':'// &
                            trim(input_name2)//'" value="'//trim(input_value)//'">'
                            p(i) = 0
                            p(i-1) = 0
                            exit
                        end if
                    end do
                end if
            end do

        end  if ! (n>0)

        ! collect grades of required subjects and subjects in Plan of Study
        MissingPOCW = 0
        do idx=1,CheckList%NSubjects
            if (isGrade_numeric_pass(CLExt(idx)%Grade)) cycle ! subject has a grade; ignore
            crse = CheckList%SubjectIdx(idx)
            ! count missing entries in plan of study
            if (crse < 0) then ! elective not specified
                if (Subject(crse)%Units>0) MissingPOCW = MissingPOCW+1 ! count only non-zero units
                input_name1 = fn_blank_to_underscore(Subject(crse)%Name)
                CLExt(idx)%Disp_Subject = Subject(crse)%Name
                CLExt(idx)%Disp_Comment = SPACE
                CLExt(idx)%Disp_Input_Elective = '<input type="text" size="12" name="'// &
                trim(input_name1)//DASH//trim(itoa(CheckList%SubjectTerm(idx)))//':">'
                CLExt(idx)%Disp_Remarks = '*'
            else
                do i=1,lenTCG
                    if (TCG(i)%Code<2) cycle
                    if ((.not. TCG(i)%Used) .and. TCG(i)%Subject == crse) then
                        CLExt(idx)%Grade = TCG(i)%Grade
                        if (index(CLExt(idx)%Disp_Input_Elective,'<input')>0) then
                            CLExt(idx)%Disp_Comment = Subject(crse)%Name
                        ! uncomment next line to disallow change of passed electives
                        !CLExt(idx)%Disp_Input_Elective = Subject(crse)%Name
                        end if

                        TCG(i)%Used = .true.

                        ! use first encountered passing grade
                        if (isGrade_numeric_pass(CLExt(idx)%Grade)) then
                            exit
                        end if
                    end if
                end do
            end if
        end do


        ! automatic substitutions / equivalencies
        do idx=1,CheckList%NSubjects
            if (isGrade_passing(CLExt(idx)%Grade)) cycle ! consider earned
            crse = CheckList%SubjectIdx(idx)
#ifdef DBsubst
            call html_comment(Subject(crse)%Name//ftoa(Subject(crse)%Units,1)//' units not yet earned?')
#endif
            loop_subst : &
            do i=1,NumSubst
                k = SubstIdx(i)
                if (Substitution(k+1) == crse .and. &
                    (Substitution(k)==idxCURR .or. Substitution(k)==-1) ) then
                    p = 0
                    do j=k+2, SubstIdx(i+1)-1
                        OKSubst = .false.
#ifdef DBsubst
                        call html_comment('       equivalent is '//Subject(Substitution(j))%Name// &
                            ftoa(Subject(Substitution(j))%Units,1)//' units')
#endif
                        do l=1,lenTCG
                            if (TCG(l)%Code<2) cycle
                            if (TCG(l)%Subject==Substitution(j) .and. &
                                isGrade_numeric_pass(TCG(l)%Grade) &
                                .and. (.not. TCG(l)%Used) ) then
                                p(j-k) = l
                                gdx = TCG(l)%Grade
                                OKSubst = .true.
#ifdef DBsubst
                                call html_comment('         grade in '//Subject(Substitution(j))%Name//' is '// &
                                    txtGrade(pGrade(gdx)))
#endif
                                ! use first encountered passing grade
                                if (isGrade_numeric_pass(CLExt(idx)%Grade)) then
                                    exit
                                end if
                            end if
                        end do
                        if (.not. OKSubst) then
#ifdef DBsubst
                            call html_comment('           '//Subject(Substitution(j))%Name// &
                                ' not yet earned; equivalence not applicable')
#endif
                            cycle loop_subst
                        end if
                    end do
                    CLExt(idx)%Grade = gdx ! PASS
                    do j=k+2, SubstIdx(i+1)-1
                        l = p(j-k)
                        TCG(l)%UnitsUsed = TCG(l)%UnitsUsed + Subject(crse)%Units
                        TCG(l)%Used =  TCG(l)%UnitsUsed>=Subject(TCG(l)%Subject)%Units ! .true.
                        if (j == k+2) then
                            tSubject = Subject(Substitution(j))%Name
                        else
                            tSubject = trim(tSubject)//'+'
                        end if
                    end do
                    CLExt(idx)%Disp_Subject = Subject(crse)%Name
                    CLExt(idx)%Disp_Comment = tSubject
                    CLExt(idx)%Disp_Input_Elective = tSubject

                    !if (SubstIdx(i+1)-SubstIdx(i) == 3 .and. & ! only 1 substitute subject
                    !    Subject(crse)%Units <= Subject(TCG(l)%Subject)%Units ) then ! ok units
                    !  CheckList%SubjectIdx(idx) = TCG(l)%Subject
                    !  CLExt(idx)%Grade = TCG(l)%Grade
                    !  CLExt(idx)%Disp_Input_Elective = Subject(TCG(l)%Subject)%Name
                    !end if
                    exit loop_subst
                end if
            end do loop_subst
        end do

        ! units per sem, including updated dummy subjects
        SpecifiedUnits = 0.0
        do idx=1,CheckList%NSubjects
            credit = Subject(CheckList%SubjectIdx(idx))%Units
            rank = CheckList%SubjectTerm(idx)
            SpecifiedUnits(rank) = SpecifiedUnits(rank) + credit
            ! units passed, conditional in curriculum; expected from REGD subjects
            if (CLExt(idx)%Grade==gdxREGD ) then
!                if (isProbabilistic) then
!                    UnitsExpected = UnitsExpected + credit*(1.0-Subject(CheckList%SubjectIdx(idx))%Failrate(cTm1))
!                else
                    UnitsExpected = UnitsExpected + credit
!                end if
                UnitsEarned = UnitsEarned + credit ! optimistic units earned
            else if (isGrade_passing(CLExt(idx)%Grade) ) then
                UnitsEarned = UnitsEarned + credit
            else if (isGrade_conditional(CLExt(idx)%Grade) ) then
                UnitsConditional = UnitsConditional + credit
                codeConditional = codeConditional + 1
            else if (isGrade_failing(CLExt(idx)%Grade) ) then
                UnitsToRepeat = UnitsToRepeat + credit
            end if
        end do
        ! units failed (all subjects registered)
        UnitsFailed = 0
        do idx=1,lenTCG
            if (TCG(idx)%Code<2) cycle
            crse = TCG(idx)%Subject
            if (isGrade_failing(TCG(idx)%Grade)) then
                UnitsFailed = UnitsFailed + Subject(crse)%Units
            end if
        end do
        ! student performance
#ifdef DBperformance
        PERFUnitsEnrolled = 0
        PERFUnitsEarned = 0
        do idx=1,lenTCG
            if (TCG(idx)%Code/=3) cycle ! not enrolled FINALGRADE
            if (TCG(idx)%Grade==gdxDRP .or. &
                TCG(idx)%Grade==gdxLOA ) cycle ! .or. &
                !(TCG(idx)%Grade==gdxREGD .and. isProbabilistic ) ) cycle ! do not count
            crse = TCG(idx)%Subject
            credit = Subject(crse)%Units
            gdx = TCG(idx)%Grade
            ! count towards units enrolled
            PERFUnitsEnrolled = PERFUnitsEnrolled + credit
            ! count towards units earned
            if (isGrade_passing(gdx)) then
                PERFUnitsEarned = PERFUnitsEarned + credit
            else if (isGrade_conditional(gdx) .and. isGrade_passing(TCG(idx)%ReExam)) then
                PERFUnitsEarned = PERFUnitsEarned + credit
            end if
        end do
        if (PERFUnitsEnrolled>=30) then
            StdPerf = (100*PERFUnitsEarned)/PERFUnitsEnrolled
        else
            StdPerf = 100
        end if
#endif

        ! compute term in curriculum, allowed load
        StdTerm = Curriculum(idxCURR)%NumTerms
        do i=1,StdTerm+3
            SpecifiedUnits(i+1) = SpecifiedUnits(i+1)  + SpecifiedUnits(i)
        end do
        UnitsTarget = SpecifiedUnits(StdTerm)
#ifdef DBunits
        write(*,'(a,20i4)')    '       Unit load by term :', (TermUnits(j), j=1,Curriculum(idxCURR)%NumTerms)
        write(*,'(a,20i4)')    ' Cumulative units by term:', (SpecifiedUnits(j), j=1,Curriculum(idxCURR)%NumTerms)
        write(*,'(a,7(a,i3))') ' Cumulative units by year:', &
        (SPACE//trim(txtYear(j))//DASH,SpecifiedUnits(3*j), j=1,(Curriculum(idxCURR)%NumTerms+1)/3)
        write(*,'(a,4(a,f7.2))') '     Classification units:', &
        (SPACE//trim(txtStanding(j))//DASH,(0.25*j)*UnitsTarget, j=1,4)
#endif
!        do i=Curriculum(idxCURR)%NumTerms,1,-1
!            if (UnitsEarned < SpecifiedUnits(i)) StdTerm = i
!        end do
!        ! adjust stdterm according to thisTerm
!        if (thisTerm==3) then
!#if defined UPLB
!            ! next school year
!            StdTerm = 3*( (StdTerm-1)/3 + 1)
!#else
!            ! same school year
!            StdTerm = 3*( (StdTerm-1)/3 )
!#endif
!        else if (thisTerm==1) then
!            if (mod(StdTerm,3)/=1) StdTerm = 3*( (StdTerm-1)/3 + 1) + 1
!        else
!            if (mod(StdTerm,3)/=2) StdTerm = 3*((StdTerm-1)/3) + 2
!            ! special case for new freshmen entering 2nd semester
!            if (UnitsEarned < TermUnits(1)) StdTerm = 1
!        end if
!        StdYear = min(StdTerm,Curriculum(idxCURR)%NumTerms)/3 + 1

        do j=max(2,(Curriculum(idxCURR)%NumTerms+1)/3),1,-1
            if (UnitsEarned<SpecifiedUnits(3*j)) StdYear = j
        end do
        StdTerm = (StdYear-1)*3 + thisTerm

        AllowedLoad = TermUnits(min(StdTerm,Curriculum(idxCURR)%NumTerms))
        if (AllowedLoad == 0) then
            if (thisTerm==3) then
                AllowedLoad = 9
            else
                AllowedLoad = 27
            end if
        end if

        ! compute classification
        PercentEarned = (100.0*UnitsEarned)/UnitsTarget + 0.55
        if (PercentEarned>75.0) then
            StdClassification = YEAR_LEVEL_FOURTH
        elseif (PercentEarned>50.0) then
            StdClassification = YEAR_LEVEL_THIRD
        elseif (PercentEarned>25.0) then
            StdClassification = YEAR_LEVEL_SECOND
        else
            StdClassification = YEAR_LEVEL_FIRST
        end if

!        call html_comment( &
!            'Target='//ftoa(UnitsTarget,1), &
!            'Earned='//ftoa(UnitsEarned,1), &
!            '% earned='//ftoa((100.0*UnitsEarned)/UnitsTarget,1) )
!        call html_comment( &
!            'StdClassification='//itoa(StdClassification), &
!            'StdTerm='//itoa(StdTerm),  &
!            'StdYear='//itoa(Stdyear), &
!            'Allowed='//ftoa(AllowedLoad,1))

        ! collect prerequisites; simplify ANDs and ORs
        do idx=1,CheckList%NSubjects
            CLExt(idx)%CLPreq = 0
            CLExt(idx)%CPPreq = 0
        end do
        do idx=1,CheckList%NSubjects
            crse = CheckList%SubjectIdx(idx)
            rank = CheckList%SubjectTerm(idx)
            call rank_to_year_term(rank, iYear, iTerm)
            if (crse > 0) then ! NOT a special subject
#ifdef DBsimplify
                call html_comment(itoa(iYear)//itoa(iTerm)//Subject(crse)%Name)
#endif
                ! collect prerequisites
                ntokens = Subject(crse)%lenPreq
                CLExt(idx)%CLPreq(0) = ntokens
                do j = 1, ntokens
                    ! succeeding elements are indices to special subjects or index of subject in checklist
                    k = Subject(crse)%Prerequisite(j)
                    tSubject = Subject(k)%Name
                    if (k < 0) then ! special
                        CLExt(idx)%CLPreq(j) = k
                        ! if no waiver/COI file, change COI prerequisite to standing when taken in curriculum
                        !if (NumWaiverRecords==0 .and. ntokens == 1 .and. tSubject == 'COI') then
                        if (ntokens == 1 .and. tSubject == 'COI') then
                            tSubject = txtYear(iYear)
                            jdx = index_to_subject(tSubject)
                            CLExt(idx)%CLPreq(1) = jdx
                        end if
                        ! set JUNIOR/SENIOR prerequisite to year taken in curriculum if the subject
                        ! is taken earlier than 3d/4th year
                        !if ((tSubject == 'JUNIOR' .and. iYear < 3) .or. &
                        !    (tSubject == 'SENIOR' .and. iYear < 4)) then
                        !  tSubject = txtStanding(iYear)
                        !  jdx = index_to_subject(tSubject)
                        !  CLExt(idx)%CLPreq(1) = jdx
                        !end if
                    else
                        jdx = index_of_subject_in_curriculum(CheckList, k)
                        if (jdx == 0) then
                            ! prerequisite not present in curriculum
                            jdx = index_to_subject(tSubject)
                            jdx = NumDummySubjects - jdx
                        end if
                        ! save index of prerequisite subject in checklist
                        CLExt(idx)%CLPreq(j) = jdx
                    end if

#ifdef DBsimplify
                    call html_comment(tSubject//itoa(CLExt(idx)%CLPreq(j)))
#endif

                end do
            else
                ! a special subject; Plan of Study not properly accomplished
                ! set prerequisite to year taken in curriculum
                tSubject = txtStanding(iYear)
                CLExt(idx)%CLPreq(1) = index_to_subject(tSubject)
                CLExt(idx)%CLPreq(0) = 1
            end if ! (crse > 0)

            ! remove ORs in prerequisite for critical path computations
            ntokens = CLExt(idx)%CLPreq(0)
            tmpPreq = 0
            do j=0,ntokens
                tmpPreq(j) = CLExt(idx)%CLPreq(j)
            end do
            if (ntokens > 1) then
                reduction_loop : & ! loop while boolean simplifications occurred
                do
                    do j = ntokens,1,-1
                        jdx = tmpPreq(j)
                        if (jdx < NumDummySubjects) then ! not in curriculum
                            jdx = -(jdx-NumDummySubjects)
                            tmpPreq(j) = 0
                            cycle
                        else if (jdx > 0) then ! in curriculum
                            cycle
                        end if
                        FlagIsUp = .false.
                        if (Subject(jdx)%Name == 'OR') then
                            if (tmpPreq(j+1) == 0 .or. tmpPreq(j+2) == 0) then
                                if (tmpPreq(j+1) == 0 .and. tmpPreq(j+2) == 0) then
                                    tmpPreq(j) = 0
                                else if (tmpPreq(j+1) /= 0) then
                                    tmpPreq(j) = tmpPreq(j+1)
                                else if (tmpPreq(j+2) /= 0) then
                                    tmpPreq(j) = tmpPreq(j+2)
                                end if
                                FlagIsUp = .true.
                            else if (tmpPreq(j+1)==tmpPreq(j+2) .and. tmpPreq(j+1)>0) then
                                tmpPreq(j) = tmpPreq(j+1)
                                FlagIsUp = .true.
                            else if (tmpPreq(j+1) < 0) then ! special subject
                                if (Subject(tmpPreq(j+1))%Name .ne. 'AND') then
                                    tmpPreq(j) = tmpPreq(j+2)
                                    FlagIsUp = .true.
                                end if
                            end if
                        else if (Subject(jdx)%Name == 'AND') then
                            if (tmpPreq(j+1) == 0 .or. tmpPreq(j+2) == 0) then
                                tmpPreq(j) = 0
                                FlagIsUp = .true.
                            else if (tmpPreq(j+1) < 0) then ! special subject
                                if (Subject(tmpPreq(j+1))%Name .ne. 'OR') then
                                    tmpPreq(j) = tmpPreq(j+2)
                                    FlagIsUp = .true.
                                end if
                            end if
                        end if
                        if (FlagIsUp) then ! a reduction
                            tmpPreq(j+1) = 0
                            tmpPreq(j+2) = 0
                            do k=j+3,ntokens
                                tmpPreq(k-2) = tmpPreq(k)
                            end do
                            tmpPreq(0) = tmpPreq(0)-2
                        end if
                    end do
                    if (ntokens == tmpPreq(0)) then
                        exit reduction_loop
                    else
                        ntokens = tmpPreq(0)
                    end if
                end do reduction_loop
            end if
            ! remove ANDs
            i = 0
            do j=1,ntokens
                if (tmpPreq(j) > 0) then
                    i = i+1
                    CLExt(idx)%CPPreq(i) = tmpPreq(j)
                end if
            end do
            CLExt(idx)%CPPreq(0) = i
        end do ! idx=1,CheckList%NSubjects

        ! critical path
        EarlyTime = 0
        LateTime = 0
        Slack = 0
        do idx=1,CheckList%NSubjects
            gdx = CLExt(idx)%Grade
            if (gdx==0 .or. isGrade_failing(gdx)) EarlyTime(idx) = 1
        end do
        Latest = 0
        do ! loop while an EarlyTime() changed
            Latest = Latest + 1
            !     write(sdxout,*) 'Late time =', Latest
            tCount = 0
            do idx=1,CheckList%NSubjects
                if (EarlyTime(idx) == Latest) then
                    ! write(sdxout,*) 'Subject is '//Subject(CheckList%SubjectIdx(idx))%Name
                    ! subject <idx> starts at time <Latest>; search for successor subjects
                    do jdx=idx+1,CheckList%NSubjects
                        if (EarlyTime(jdx) /= 0) then
                            ! subject <jdx> is not yet passed...
                            do kdx=1,CLExt(jdx)%CPPreq(0)
                                if (CLExt(jdx)%CPPreq(kdx) == idx) then
                                    ! and it is a successor to subject <idx>;
                                    ! set start time of subject <jdx> to <Latest+1>
                                    ! write(sdxout,*) 'Successor subject is '//Subject(CheckList%SubjectIdx(jdx))%Name
                                    EarlyTime(jdx) = max(EarlyTime(jdx), Latest+1)
                                    tCount = tCount + 1
                                    exit
                                end if
                            end do
                        end if
                    end do
                end if
            end do
            if (tCount == 0) exit
        end do
        ! initial late start times
        do idx=1,CheckList%NSubjects
            if (EarlyTime(idx) /= 0) then
                ! subject not passed; assume no subject follows; it can start at Latest time
                LateTime(idx) = Latest
                ! verify if it is really not a prerequisite to any subject
                do jdx=idx+1,CheckList%NSubjects
                    do kdx=1,CLExt(jdx)%CPPreq(0)
                        if (CLExt(jdx)%CPPreq(kdx) == idx) then
                            ! it is a prerequisite; postpone computation of late start time
                            LateTime(idx) = Latest+1
                            exit
                        end if
                    end do
                end do
            end if
        end do
        ! now compute late start time
        Earliest = Latest+1
        do while (Earliest > 1)
            Earliest = Earliest - 1
            !     write(sdxout,*) 'Early time =', Earliest
            do idx=1,CheckList%NSubjects
                if (LateTime(idx) == Earliest) then
                    do jdx=1,idx-1 ! CheckList%NSubjects
                        if (LateTime(jdx) /= 0) then
                            do kdx=1,CLExt(idx)%CPPreq(0)
                                if (CLExt(idx)%CPPreq(kdx) == jdx) then
                                    LateTime(jdx) = min(LateTime(jdx), Earliest-1)
                                end if
                            end do
                        end if
                    end do
                end if
            end do
        end do
        ! slack times
#ifdef DBsimplify
        call html_comment('Zero-slack subjects, rank, early start...')
#endif
        do idx=1,CheckList%NSubjects
            Slack(idx) = LateTime(idx) - EarlyTime(idx)
            CLExt(idx)%EarlyTime = EarlyTime(idx)
            CLExt(idx)%LateTime = LateTime(idx)
#ifdef DBsimplify
            if (Slack(idx) == 0 .and. EarlyTime(idx) /= 0) then
                call html_comment(Subject(CheckList%SubjectIdx(idx))%Name//itoa(CheckList%SubjectTerm(idx))// &
                    itoa(EarlyTime(idx)) )
            end if
#endif
        end do

        ! identify subjects not passed, OK prerequisite, and offered
        do idx=1,CheckList%NSubjects
            crse = CheckList%SubjectIdx(idx)
            rank = CheckList%SubjectTerm(idx)
#ifdef DBprereq
            call html_comment(itoa(idx)//itoa(rank)//Subject(crse)%Name)
#endif
            ! display string
            tSubject = Subject(crse)%Name
            if (len_trim(CLExt(idx)%Disp_Subject)==0) CLExt(idx)%Disp_Subject = tSubject
            CLExt(idx)%Disp_Units = ftoa(Subject(crse)%Units,1)
            !if (isRole_adviser_of_student(std, orHigherUp) .or. isRoleStudent .or. isRole_benefactor_of_student(std)) then ! Adviser or SYSAD
                CLExt(idx)%Disp_Grade = txtGrade(pGrade(CLExt(idx)%Grade))
            !elseif (isGrade_passing(CLExt(idx)%Grade)) then ! Dept. Chair
            !    CLExt(idx)%Disp_Grade = 'PASS'
            !else
            !    CLExt(idx)%Disp_Grade = txtGrade(pGrade(CLExt(idx)%Grade))
            !end  if
            input_name2 = fn_blank_to_underscore(tSubject)
            if (len_trim(CLExt(idx)%Disp_Comment)==0) then
                CLExt(idx)%Disp_Input_Grade = '<input type="text" size="4" name="GRADE:'//trim(input_name2)//'" value="'// &
                    trim(CLExt(idx)%Disp_Grade)//'">'
            else
                CLExt(idx)%Disp_Input_Grade = CLExt(idx)%Disp_Grade
            end if

            if (isRoleStudent .and. trim(USERNAME)==trim(Student(requestingStudent)%StdNo) .and. &
                isGrade_passing(CLExt(idx)%Grade)) then ! student owner
                CLExt(idx)%Disp_Grade = 'PASS'
            end  if

            ! evaluate prerequisite
            tmpPreq = 0
            prob = 0.0
            ntokens = CLExt(idx)%CLPreq(0)
            tmpPreq(0:ntokens) = CLExt(idx)%CLPreq(0:ntokens)
            FlagIsUp = .false.   ! standing prerequisite?
            do j = ntokens,1,-1
                jdx = tmpPreq(j)
                if (jdx>0) then ! a subject in the checklist
                    tSubject = Subject(CheckList%SubjectIdx(jdx))%Name
                    if (CLExt(jdx)%Grade==gdxREGD) then
!                        if (isProbabilistic) then ! probabilistic
!                            prob(j) = 1.0-Subject(CheckList%SubjectIdx(jdx))%Failrate(cTm1)
!                            tmpPreq(j) = 1
!                        else ! enlistment period

                            ! REGD == No grade == fail
                            !tmpPreq(j) = 0
                            !prob(j) = 0.0

                            ! REGD == PASS
                            tmpPreq(j) = 1
                            prob(j) = 1.0
!                        end if
#ifdef DBprereq
                        tNote = 'REGISTERED'
#endif
                    else if (isGrade_passing(CLExt(jdx)%Grade)) then
                        tmpPreq(j) = 1
                        prob(j) = 1.0
#ifdef DBprereq
                        tNote = 'EARNED'
#endif
                    else
                        tmpPreq(j) = 0
                        prob(j) = 0.0
#ifdef DBprereq
                        tNote = 'NOT PASSED'
#endif
                    end if

#ifdef DBprereq
                    call html_comment('In checklist, '//tNote//tSubject)
#endif

                else if (jdx >= NumDummySubjects) then
                    ! a special subject
#ifdef DBprereq
                    tNote = 'STANDING'
#endif
                    tSubject = Subject(jdx)%Name
                    if (tSubject == 'NONE' .or. tSubject == 'FRESHMAN') then
                        prob(j) = 1.0
                        tmpPreq(j) = 1
                    else if (tSubject == 'COI') then ! fixed after loop end
                        prob(j) = 0.0
                        tmpPreq(j) = 0
                    else if (tSubject == 'SOPHOMORE') then
                        if (StdClassification >= YEAR_LEVEL_SECOND) then
                            prob(j) = 1.0
                            tmpPreq(j) = 1
                        else
                            prob(j) = 0.0
                            tmpPreq(j) = 0
                        end if
                    else if (tSubject == 'JUNIOR') then
                        FlagIsUp = .true.
                        if (StdClassification >= YEAR_LEVEL_THIRD) then
                            prob(j) = 1.0
                            tmpPreq(j) = 1
                        else
                            prob(j) = 0.0
                            tmpPreq(j) = 0
                        end if
                    else if (tSubject == 'SENIOR') then
                        FlagIsUp = .true.
                        if (StdClassification >= YEAR_LEVEL_FOURTH) then
                            prob(j) = 1.0
                            tmpPreq(j) = 1
                        else
                            prob(j) = 0.0
                            tmpPreq(j) = 0
                        end if
                    else if (tSubject == 'FIRST') then
                        FlagIsUp = .true.
                        if (StdYear >= 1) then
                            prob(j) = 1.0
                            tmpPreq(j) = 1
                        else
                            prob(j) = 0.0
                            tmpPreq(j) = 0
                        end if
                    else if (tSubject == 'SECOND') then
                        FlagIsUp = .true.
                        if (StdYear >= 2) then
                            prob(j) = 1.0
                            tmpPreq(j) = 1
                        else
                            prob(j) = 0.0
                            tmpPreq(j) = 0
                        end if
                    else if (tSubject == 'THIRD') then
                        FlagIsUp = .true.
                        if (StdYear >= 3) then
                            prob(j) = 1.0
                            tmpPreq(j) = 1
                        else
                            prob(j) = 0.0
                            tmpPreq(j) = 0
                        end if
                    else if (tSubject == 'FOURTH') then
                        FlagIsUp = .true.
                        if (StdYear >= 4) then
                            prob(j) = 1.0
                            tmpPreq(j) = 1
                        else
                            prob(j) = 0.0
                            tmpPreq(j) = 0
                        end if
                    else if (tSubject == 'FIFTH') then
                        FlagIsUp = .true.
                        if (StdYear >= 5) then
                            prob(j) = 1.0
                            tmpPreq(j) = 1
                        else
                            prob(j) = 0.0
                            tmpPreq(j) = 0
                        end if
                    else if (tSubject == 'SIXTH') then
                        FlagIsUp = .true.
                        if (StdYear >= 6) then
                            prob(j) = 1.0
                            tmpPreq(j) = 1
                        else
                            prob(j) = 0.0
                            tmpPreq(j) = 0
                        end if
                    else if (tSubject == 'GRADUATING') then
                        FlagIsUp = .true.
                        if (UnitsTarget-UnitsEarned <= 30.0) then
                            prob(j) = 1.0
                            tmpPreq(j) = 1
                        else
                            prob(j) = 0.0
                            tmpPreq(j) = 0
                        end if
                    else if (tSubject == 'OR') then
                        prob(j) = amax1(prob(j+1), prob(j+2))
                        prob(j+1) = 0.0
                        prob(j+2) = 0.0
                        tmpPreq(j) = tmpPreq(j+1) + tmpPreq(j+2)
                        tmpPreq(j+1) = 0
                        tmpPreq(j+2) = 0
                        do k=j+3,ntokens
                            prob(k-2) = prob(k)
                            tmpPreq(k-2) = tmpPreq(k)
                        end do
#ifdef DBprereq
                        tNote = '(OPERATOR:OR)'
#endif
                    else if (tSubject == 'AND') then
                        prob(j) = prob(j+1)*prob(j+2)
                        prob(j+1) = 0.0
                        prob(j+2) = 0.0
                        tmpPreq(j) = tmpPreq(j+1) * tmpPreq(j+2)
                        tmpPreq(j+1) = 0
                        tmpPreq(j+2) = 0
                        do k=j+3,ntokens
                            prob(k-2) = prob(k)
                            tmpPreq(k-2) = tmpPreq(k)
                        end do
#ifdef DBprereq
                        tNote = '(OPERATOR:AND)'
#endif
                    end if

#ifdef DBprereq
                    call html_comment('Special, '//tNote//tSubject)
#endif

                else ! not in curriculum; try all registered subjects
                    prob(j) = 0.0
                    tmpPreq(j) = 0
#ifdef DBprereq
                    tNote = 'Not taken'
#endif
                    do k=1,lenTCG
                        if (TCG(k)%Code<2) cycle
                        if (-TCG(k)%Subject == jdx-NumDummySubjects .and. &
                            isGrade_passing(TCG(k)%Grade)) then
                            prob(j) = 1.0
                            tmpPreq(j) = 1
#ifdef DBprereq
                            tNote = 'EARNED'
#endif
                        end if
                    end do
                    tSubject = Subject(-(jdx-NumDummySubjects))%Name

#ifdef DBprereq
                    call html_comment('Not in curriculum, '//tNote//tSubject)
#endif

                end if
            end do
            ! satisfied?
            CLExt(idx)%OKPreq = tmpPreq(1) > 0
            CLExt(idx)%Contrib = prob(1)

#ifdef DBprereq
            tNote = SPACE
#endif
!            if (WaiverCOI%lenSubject>0) then ! there are forced subjects; ignore prerequisite
!                do k=1,WaiverCOI%lenSubject
!                    if (crse==WaiverCOI%Subject(k)) then
!                        CLExt(idx)%OKPreq = .true.
!                        CLExt(idx)%Contrib = 1.0
!                        exit
!                    end if
!                end do
!#ifdef DBprereq
!                tNote = 'WAIVER/COI'
!#endif
!
!            end if

#ifdef DBprereq
            if (CLExt(idx)%OKPreq) then
                call html_comment(trim(tNote)//' OKpreq=TRUE')
            else
                call html_comment(trim(tNote)//' OKpreq=FALSE')
            end if
#endif
            ! problem with records?
            if (CLExt(idx)%Grade==gdxREGD) then
                !write(token, '(f5.2)') 1.0-Failrate(crse,cTm1)
                CLExt(idx)%Disp_Grade = '(NG)'
                CLExt(idx)%Disp_Remarks = SPACE
            else if (isGrade_passing(CLExt(idx)%Grade) ) then
                CLExt(idx)%Disp_Remarks = SPACE
            ! missing grade of prerequisite
            !if (.not. CLExt(idx)%OKPreq .and. .not. FlagIsUp) &
            else if (isGrade_conditional(CLExt(idx)%Grade) ) then
                CLExt(idx)%Disp_Remarks = '%'

                ! fail "lapsed" 4.0 and INC
                do tdx = lenTCG,1,-1
                    if (TCG(tdx)%Code<2) cycle ! not a grade
                    if (TCG(tdx)%Subject == crse .and. TCG(tdx)%Grade == CLExt(idx)%Grade ) then
                        if (now-TCG(tdx)%Taken > 3) then
                            CLExt(idx)%Grade =ZERO_PERCENT_GRADE+70 ! gdx5
                            CLExt(idx)%Disp_Remarks = '#'
                        end if
                        exit
                    end if
                end do
                CLExt(idx)%Disp_Grade = red//trim(CLExt(idx)%Disp_Grade)//e_color
                CLExt(idx)%Disp_Subject = red//trim(CLExt(idx)%Disp_Subject)//e_color

            else if (isGrade_failing(CLExt(idx)%Grade) ) then
                CLExt(idx)%Disp_Remarks = '#'
                CLExt(idx)%Disp_Grade = red//trim(CLExt(idx)%Disp_Grade)//e_color
                CLExt(idx)%Disp_Subject = red//trim(CLExt(idx)%Disp_Subject)//e_color

            else if (CLExt(idx)%OKPreq) then
                CLExt(idx)%Disp_Subject = green//b_bold//trim(CLExt(idx)%Disp_Subject)//e_bold//e_color
            end if
        end do ! idx=1,CheckList%NSubjects


        ! split remaining subjects accd to satisfaction of prerequisites
        ! for subjects with corequisites, the corequisite must be a named subject, not AND/OR expression
        ! same with subjects that have prerequisites that can be taken concurrently
        n = 0
        p = 0
        m = 0
        q = 0
        !r = 0.0
        do idx=1,CheckList%NSubjects
            gdx = CLExt(idx)%Grade
            if ( gdx==0 .or. isGrade_failing(gdx) .or. isGrade_conditional(gdx) ) then  ! no grade or failed or conditional not passed
                if (CLExt(idx)%OKPreq .and. CheckList%SubjectIdx(idx) > 0) then ! named subject, not passed, prereq is satisfied

                    FlagIsUp = .true. ! by default, add to list of subjects with satisfied prereqs

                    ! check if subject has a co-requisite or if subject has a prerequisite that can be taken concurrently
                    ! logic works only if co-req is a named subject, not AND/OR expression

                    ! check if there is a corequisite
                    cdx = Subject(CheckList%SubjectIdx(idx))%Corequisite(1)
                    if (cdx>0) then ! corequisite is a named subject
#ifdef DBcoreq
                        call html_comment('OK preq: '//Subject(CheckList%SubjectIdx(idx))%Name// &
                            ' is not passed, but prereq is satisfied; corequisite is '//Subject(cdx)%Name)
#endif
                        kdx = index_of_subject_in_curriculum(CheckList, cdx)
                        if (kdx>0) then ! found in checklist; should be passed, or at least, prereq is satisfied
                            if (isGrade_passing(CLExt(kdx)%Grade) ) then ! co-req is passed
#ifdef DBcoreq
                                call html_comment('  - which is a required subject already passed; ADD.')
#endif
                                FlagIsUp = .true.
                            else if (CLExt(kdx)%OKPreq) then ! co-req's prereq is satisfied (co-req is predicted)
#ifdef DBcoreq
                                call html_comment('  - which is a required subject whose prereq is already satisfied; ADD.')
#endif
                                FlagIsUp = .true.
                            else ! co-requisite not passed, nor is its prereq satisfied
#ifdef DBcoreq
                                call html_comment('  - which is a required subject not passed nor satisfied prereq; DO NOT ADD')
#endif
                                FlagIsUp = .false.
                            end if

                        else ! co-requisite is not in student's curriculum
#ifdef DBcoreq
                            call html_comment('  - which is NOT a required subject; curriculum is poorly desgined; DO NOT ADD!')
#endif
                            FlagIsUp = .false.
                        end if

                    else ! no corequisite

                        ! check if there is a prerequisite that can be taken concurrently
                        cdx = Subject(CheckList%SubjectIdx(idx))%Concprerequisite(1)
                        if (cdx>0) then ! conc. prerequisite is a named subject
#ifdef DBconcpreq
                            call html_comment('OK preq: '//Subject(CheckList%SubjectIdx(idx))%Name// &
                                ' is not passed and prereq is satisfied; addnl prerequisite that can be taken concurrently is '// &
                                Subject(cdx)%Name)
#endif
                            kdx = index_of_subject_in_curriculum(CheckList, cdx)
                            if (kdx>0) then ! found in checklist; should be passed, or at least, prereq is satisfied
                                if (isGrade_passing(CLExt(kdx)%Grade)) then ! conc. pre-req is passed
#ifdef DBconcpreq
                                    call html_comment('  - which is a required subject already passed; ADD.')
#endif
                                    CLExt(idx)%Contrib = CLExt(kdx)%Contrib
                                    FlagIsUp = .true.
                                else if (CLExt(kdx)%OKPreq) then ! conc. pre-req's prereq is satisfied (conc. pre-req is predicted)
#ifdef DBconcpreq
                                    call html_comment('  - which is a required subject whose prereq is already satisfied; ADD.')
#endif
                                    CLExt(idx)%Contrib = CLExt(kdx)%Contrib
                                    FlagIsUp = .true.
                                else
#ifdef DBconcpreq
                                    call html_comment('  - which is a required subject not passed nor satisfied prereq; DO NOT ADD')
#endif
                                    FlagIsUp = .false.
                                end if

                            else
#ifdef DBconcpreq
                                call html_comment('  - which is NOT a required subject; curriculum is poorly desgined; DO NOT ADD!')
#endif
                                FlagIsUp = .false.
                            end if

                        end if

                    end if

                    if (FlagIsUp) then ! ok to add
                        n = n+1
                        p(n) = idx
#ifdef DBpriority
                        call html_comment('OK to add '//itoa(n)//Subject(CheckList%SubjectIdx(idx))%Name)
#endif
                    end if

                else ! not passed and not satisfied prereq

                    m = m+1
                    q(m) = idx

                end if

            end if
        end do ! idx=1,CheckList%NSubjects

        ! no remaining subjects with unsatisfied prerequisites
        if (m == 0) then

            if (n == 0) then
                ! finished all academic subjects in curriculum
                codeAdvising = ADVISING_FINISHED
                StdYear = YEAR_NOT_SET
                return

            else if (StdClassification >= YEAR_LEVEL_FOURTH .and. UnitsTarget-UnitsEarned <= 27) then
                ! graduating student: recommend all subjects even if not offered
                codePriority = PRIORITY_GRADUATING
                codeAdvising = ADVISING_IRREGULAR ! adjusted in advise_student()
                credit = 0
                do i = 1,n
                    crse = CheckList%SubjectIdx(p(i))
                    credit = credit + Subject(crse)%Units
                end do
                AllowedLoad = max(AllowedLoad,credit)

                ! add as priority subjects
                do i = 1,n
                    if (p(i)==0) cycle
                    NPriority = NPriority+1
#ifdef DBpriority
                    call html_comment('PriorityA #'//itoa(NPriority)//Subject(CheckList%SubjectIdx(p(i)))%Name)
#endif
                    CLExt(NPriority)%PriorityRank = p(i)
                    CLExt(p(i))%Disp_Remarks = 'Pri'//itoa(NPriority)
                end do

                return

            end if

        end if

        ! if passed NSTP 11(A,B,C), rename NSTP 12
        do idx=1,n
            if (p(idx) == 0) cycle
            tSubject = Subject(CheckList%SubjectIdx(p(idx)))%Name
            if (tSubject=='NSTP 12') then
                if (passedNSTP11A) then
                    token = trim(tSubject)//'A'
                else if (passedNSTP11B) then
                    token = trim(tSubject)//'B'
                else if (passedNSTP11C) then
                    token = trim(tSubject)//'C'
                else
                    token = tSubject
                end if
                CheckList%SubjectIdx(p(idx)) = index_to_subject(token)
            end if
        end do

        ! remove subjects not offered
        if (UseClasses) then
            do idx=1,n
                if (p(idx) == 0) cycle
                if (Offering(thisTerm,CheckList%SubjectIdx(p(idx)))%NSections == 0) then
#ifdef DBpriority
                    call html_comment('No open sections '//itoa(idx)//Subject(CheckList%SubjectIdx(p(idx)))%Name)
#endif
                    p(idx) = 0
                end if
            end do
        else
            do idx=1,n
                if (p(idx) == 0) cycle
                if (.not. isSubject_offered(CheckList%SubjectIdx(p(idx)),thisTerm) ) then
#ifdef DBpriority
                    call html_comment('Not offered '//itoa(idx)//Subject(CheckList%SubjectIdx(p(idx)))%Name)
#endif
                    p(idx) = 0
                end if
            end do
        end if

#ifdef DBpriority
        call html_comment('Feasible')
        do i=1,n
            idx = p(i)
            if (idx==0) cycle
            call html_comment(itoa(i)//Subject(CheckList%SubjectIdx(idx))%Name)
        end do
#endif

        ! compress
        idx = 1
        do while (idx <= n)
            if (p(idx) == 0) then
                do i=idx+1,n
                    p(i-1) = p(i)
                end do
                p(n) = 0
                n = n-1
            else
                idx = idx+1
            end if
        end do

#ifdef DBpriority
        call html_comment('Compressed')
        do i=1,n
            idx = p(i)
            call html_comment(itoa(i)//Subject(CheckList%SubjectIdx(idx))%Name)
        end do
#endif

        ! sort same rank subjects according to increasing slack time
        idx = 1
        do while (idx < n)
            jdx = idx
            do while (CheckList%SubjectTerm(p(jdx+1)) == CheckList%SubjectTerm(p(idx)))
                jdx=jdx+1
                if (jdx == n) exit
            end do
            do i=idx,jdx-1
                kdx = p(i)
                do j=i+1,jdx
                    k = p(j)
                    if (Slack(k) < Slack(kdx)) then
                        p(i) = k
                        p(j) = kdx
                        kdx = k
                    end if
                end do
            end do
            idx = jdx+1

        end do

#ifdef DBpriority
        call html_comment('Sorted by slack time')
        do i=1,n
            idx = p(i)
            call html_comment(itoa(i)//Subject(CheckList%SubjectIdx(idx))%Name)
        end do
#endif

        ! "fudge" units
        if (UnitsEarned==0 .or. AllowedLoad>=20) then
            addnl = 0 ! no fudge for new freshmen; high load semesters
        else
            addnl = 1 ! 1 unit fudge for old students
        end if

        ! collect subjects up to allowed load;

        ! add 0-credit subjects as Priority 1
        do i=1,n
            idx = p(i)
            if (Subject(CheckList%SubjectIdx(idx))%Units == 0.0) then
                NPriority = NPriority+1
                CLExt(NPriority)%PriorityRank = idx
                CLExt(idx)%Disp_Remarks = 'Pri'//itoa(NPriority)
#ifdef DBpriority
                call html_comment('Priority1 #'//itoa(NPriority)//Subject(CheckList%SubjectIdx(idx))%Name)
#endif
                ! erase from p()
                p(i) = 0
            end if
        end do

        !  Priority 2, the scheduled subjects
        do i = 1,n
            idx = p(i)
            if (idx==0) cycle ! already listed
            crse = CheckList%SubjectIdx(idx)
            !tSubject = Subject(crse)%Name
            !if (index(tSubject, ' 200') + index(tSubject, ' 190') > 0) cycle ! do not recommend 190/200
            if (CheckList%SubjectTerm(idx) == StdTerm) then
                NPriority = NPriority+1
                CLExt(NPriority)%PriorityRank = idx
                PredictedLoad = PredictedLoad + Subject(crse)%Units
                CLExt(idx)%Disp_Remarks = 'Pri'//itoa(NPriority)
#ifdef DBpriority
                call html_comment('Priority2 #'//itoa(NPriority)//Subject(crse)%Name)
#endif
                ! erase from p()
                p(i) = 0
            end if
        end do

        ! GROUP 3, the subjects scheduled during previous terms, but not passed or taken
        do tdx=1,StdTerm-1

            ! Subgroup 3a, the subjects with co-requisites
            do i=1,n
                idx = p(i) ! index to Checklist()
                if (idx<=0) cycle ! already listed, or postponed for later
                if (CheckList%SubjectTerm(idx)/=tdx) cycle ! not taken in an earlier stage
                crse = CheckList%SubjectIdx(idx) ! index to Subject()
                ! check if there is a corequisite
                coreq = Subject(crse)%Corequisite(1)
                if (coreq<=0) cycle ! co-requisite is not a named subject

                ! find co-requisite if it is further down the p() list
                kdx = index_of_subject_in_curriculum(CheckList, coreq) ! index of co-requisite in Checklist()
                FlagIsUp = .false.
                do j=i+1,n
                    if (p(j)==kdx) then
                        FlagIsUp = .true. ! found
                        exit ! remember j
                    end if
                end do

                ! NEED TO CHECK THIS LOGIC WHEN ACTUAL GRADES ON SPLIT COURSES ARE COLLECTED
                if (FlagIsUp) then ! both crse & coreq are in "prerequisite is satisfied" list
                    ! total load from crse, coreq
                    credit = Subject(crse)%Units + Subject(coreq)%Units
                    if (PredictedLoad+credit <= AllowedLoad + addnl) then ! add both crse & coreq as priority subjects
                        AddCoReq1 = .true.
                        AddCoReq2 = .true.
#ifdef DBcoreq
                        call html_comment('Priority3a.2 #'//itoa(NPriority)//Subject(crse)%Name, &
                            itoa(NPriority+1)//Subject(coreq)%Name)
#endif
                    else ! add both crse & coreq as alternate subjects later
                        p(i) = -p(i)
                        p(j) = -p(j)
                        AddCoReq1 = .false.
                        AddCoReq2 = .false.
#ifdef DBcoreq
                        call html_comment('Add as ALTERNATE subjects: '//Subject(crse)%Name//Subject(coreq)%Name)
#endif
                    end if

                else ! only crse is a priority subject, if allowed load not exceeded
                    AddCoReq2 = .false.
                    credit = Subject(crse)%Units
                    if (PredictedLoad+credit <= AllowedLoad + addnl) then ! add both crse & coreq as priority subjects
                        AddCoReq1 = .true.
#ifdef DBcoreq
                        call html_comment('Priority3a.1 #'//itoa(NPriority)//Subject(crse)%Name)
#endif
                    else
                        AddCoReq1 = .false.
                    end if
                end if

                ! add crse?
                if (AddCoreq1) then
                    NPriority = NPriority+1
                    CLExt(NPriority)%PriorityRank = idx
                    PredictedLoad = PredictedLoad + Subject(crse)%Units
                    CLExt(idx)%Disp_Remarks = 'Pri'//itoa(NPriority)
                    ! erase from p()
                    p(i) = 0
                end if

                ! add co-requisite?
                if (AddCoReq2) then
                    NPriority = NPriority+1
                    CLExt(NPriority)%PriorityRank = kdx
                    PredictedLoad = PredictedLoad + Subject(coreq)%Units
                    CLExt(kdx)%Disp_Remarks = 'Pri'//itoa(NPriority)
                    ! erase from p()
                    p(j) = 0
                end if
            end do


            ! Subgroup 3b, the subjects with prerequisite that can be taken concurrently
            do i=1,n
                idx = p(i) ! index to Checklist()
                if (idx<=0) cycle ! already listed, or postponed for later
                if (CheckList%SubjectTerm(idx)/=tdx) cycle ! not taken in an earlier stage
                concpreq = CheckList%SubjectIdx(idx) ! index to Subject()
                ! is concpreq a prerequisite (that can be taken concurrently) to another subject?
                ! find crse to which concpreq is a conc prerequisite if it is in the p() list
                FlagIsUp = .false.
                do j=1,n
                    kdx = p(j)
                    if (i==j .or. kdx<=0) cycle
                    crse = CheckList%SubjectIdx(kdx) ! index to Subject()
                    if (concpreq == Subject(crse)%Concprerequisite(1)) then
                        FlagIsUp = .true. ! found
                        exit ! remember j
                    end if
                end do

                ! NEED TO CHECK THIS LOGIC WHEN ACTUAL GRADES ON SPLIT COURSES ARE COLLECTED
                if (FlagIsUp) then ! both crse & conpreq are in "prerequisite is satisfied" list
                    ! total load from crse, concpreq
                    credit = Subject(crse)%Units + Subject(concpreq)%Units
                    if (PredictedLoad+credit <= AllowedLoad + addnl) then ! add both crse & concpreq as priority subjects
                        AddCoReq1 = .true.
                        AddCoReq2 = .true.
#ifdef DBconcpreq
                        call html_comment('Priority3b.2 #'//itoa(NPriority)//Subject(crse)%Name// &
                            itoa(NPriority+1)//Subject(concpreq)%Name)
#endif
                    else ! add both crse & concpreq as alternate subjects later
                        p(i) = -p(i)
                        p(j) = -p(j)
                        AddCoReq1 = .false.
                        AddCoReq2 = .false.
#ifdef DBconcpreq
                        call html_comment('Add as ALTERNATE subjects: '//Subject(crse)%Name//Subject(concpreq)%Name)
#endif
                    end if
                else ! only concpreq is a priority subject, if allowed load not exceeded
                    AddCoReq2 = .false.
                    credit = Subject(concpreq)%Units
                    if (PredictedLoad+credit <= AllowedLoad + addnl) then ! add both crse as priority subject
                        AddCoReq1 = .true.
#ifdef DBcoreq
                        call html_comment('Priority3b.1 #'//itoa(NPriority)//Subject(concpreq)%Name)
#endif
                    else
                        AddCoReq1 = .false.
                    end if
                end if

                ! add concpreq?
                if (AddCoreq1) then
                    NPriority = NPriority+1
                    CLExt(NPriority)%PriorityRank = idx
                    PredictedLoad = PredictedLoad + Subject(concpreq)%Units
                    CLExt(idx)%Disp_Remarks = 'Pri'//itoa(NPriority)
                    ! erase from p()
                    p(i) = 0
                end if

                ! add crse?
                if (AddCoReq2) then
                    NPriority = NPriority+1
                    CLExt(NPriority)%PriorityRank = kdx
                    PredictedLoad = PredictedLoad + Subject(crse)%Units
                    CLExt(kdx)%Disp_Remarks = 'Pri'//itoa(NPriority)
                    ! erase from p()
                    p(j) = 0
                end if
            end do

            ! Subgroup 3c, the rest of the subjects scheduled during previous terms, but not passed or taken
            do i=1,n
                idx = p(i)
                if (idx<=0) cycle ! already listed, or postponed for later
                if (CheckList%SubjectTerm(idx)/=tdx) cycle
                crse = CheckList%SubjectIdx(idx)
                credit = Subject(crse)%Units
                if (PredictedLoad+credit <= AllowedLoad + addnl) then
                    NPriority = NPriority+1
                    CLExt(NPriority)%PriorityRank = idx
                    CLExt(idx)%Disp_Remarks = 'Pri'//itoa(NPriority)
                    PredictedLoad = PredictedLoad + credit
#ifdef DBpriority
                    call html_comment('PriorityD #'//itoa(NPriority)//Subject(crse)%Name)
#endif
                    ! erase from p()
                    p(i) = 0
                end if
            end do

        end do ! tdx=1,StdTerm-1

        ! GROUP 4, subjects scheduled later ! (if plan is complete)

        do tdx=StdTerm+1,Curriculum(idxCURR)%NumTerms

            ! Subgroup 4a, the subjects with co-requisites
            do i=1,n
                idx = p(i) ! index to Checklist()
                if (idx<=0) cycle ! already listed, or postponed for later
                if (CheckList%SubjectTerm(idx)/=tdx) cycle ! not taken in a later stage
                crse = CheckList%SubjectIdx(idx) ! index to Subject()
                ! check if there is a corequisite
                coreq = Subject(crse)%Corequisite(1)
                if (coreq<=0) cycle ! co-requisite is not a named subject

                ! find co-requisite if it is further down the p() list
                kdx = index_of_subject_in_curriculum(CheckList, coreq) ! index of co-requisite in Checklist()
                FlagIsUp = .false.
                do j=i+1,n
                    if (p(j)==kdx) then
                        FlagIsUp = .true. ! found
                        exit ! remember j
                    end if
                end do

                ! NEED TO CHECK THIS LOGIC WHEN ACTUAL GRADES ON SPLIT COURSES ARE COLLECTED
                if (FlagIsUp) then ! both crse & coreq are in "prerequisite is satisfied" list
                    ! total load from crse, coreq
                    credit = Subject(crse)%Units + Subject(coreq)%Units
                    if (PredictedLoad+credit <= AllowedLoad + addnl) then ! add both crse & coreq as priority subjects
                        AddCoReq1 = .true.
                        AddCoReq2 = .true.
#ifdef DBcoreq
                        call html_comment('Priority4a.2 #'//itoa(NPriority)//Subject(crse)%Name, &
                            itoa(NPriority+1)//Subject(coreq)%Name)
#endif
                    else ! add both crse & coreq as alternate subjects later
                        p(i) = -p(i)
                        p(j) = -p(j)
                        AddCoReq1 = .false.
                        AddCoReq2 = .false.
#ifdef DBcoreq
                        call html_comment('Add as ALTERNATE subjects: '//Subject(crse)%Name//Subject(coreq)%Name)
#endif
                    end if

                else ! only crse is a priority subject, if allowed load not exceeded
                    AddCoReq2 = .false.
                    credit = Subject(crse)%Units
                    if (PredictedLoad+credit <= AllowedLoad + addnl) then ! add both crse as priority subject
                        AddCoReq1 = .true.
#ifdef DBcoreq
                        call html_comment('Priority4a.1 #'//itoa(NPriority)//Subject(crse)%Name)
#endif
                    else
                        AddCoReq1 = .false.
                    end if
                end if

                ! add crse?
                if (AddCoreq1) then
                    NPriority = NPriority+1
                    CLExt(NPriority)%PriorityRank = idx
                    PredictedLoad = PredictedLoad + Subject(crse)%Units
                    CLExt(idx)%Disp_Remarks = 'Pri'//itoa(NPriority)
                    ! erase from p()
                    p(i) = 0
                end if

                ! add co-requisite?
                if (AddCoReq2) then
                    NPriority = NPriority+1
                    CLExt(NPriority)%PriorityRank = kdx
                    PredictedLoad = PredictedLoad + Subject(coreq)%Units
                    CLExt(kdx)%Disp_Remarks = 'Pri'//itoa(NPriority)
                    ! erase from p()
                    p(j) = 0
                end if
            end do

            ! Subgroup 4b, the subjects with prerequisite that can be taken concurrently
            do i=1,n
                idx = p(i) ! index to Checklist()
                if (idx<=0) cycle ! already listed, or postponed for later
                if (CheckList%SubjectTerm(idx)/=tdx) cycle ! not taken in a later stage
                concpreq = CheckList%SubjectIdx(idx) ! index to Subject()
                ! is concpreq a prerequisite (that can be taken concurrently) to another subject?
                ! find crse to which concpreq is a conc prerequisite if it is in the p() list
                FlagIsUp = .false.
                do j=1,n
                    kdx = p(j)
                    if (i==j .or. kdx<=0) cycle
                    crse = CheckList%SubjectIdx(kdx) ! index to Subject()
                    if (concpreq == Subject(crse)%Concprerequisite(1)) then
                        FlagIsUp = .true. ! found
                        exit ! remember j
                    end if
                end do

                ! NEED TO CHECK THIS LOGIC WHEN ACTUAL GRADES ON SPLIT COURSES ARE COLLECTED
                if (FlagIsUp) then ! both crse & conpreq are in "prerequisite is satisfied" list
                    ! total load from crse, concpreq
                    credit = Subject(crse)%Units + Subject(concpreq)%Units
                    if (PredictedLoad+credit <= AllowedLoad + addnl) then ! add both crse & concpreq as priority subjects
                        AddCoReq1 = .true.
                        AddCoReq2 = .true.
#ifdef DBconcpreq
                        call html_comment('Priority4b.2 #'//itoa(NPriority)//Subject(crse)%Name//&
                            itoa(NPriority+1)//Subject(concpreq)%Name)
#endif
                    else ! add both crse & concpreq as alternate subjects later
                        p(i) = -p(i)
                        p(j) = -p(j)
                        AddCoReq1 = .false.
                        AddCoReq2 = .false.
#ifdef DBconcpreq
                        call html_comment('Add as ALTERNATE subjects: '//Subject(crse)%Name//Subject(concpreq)%Name)
#endif
                    end if
                else ! only concpreq is a priority subject, if allowed load not exceeded
                    AddCoReq2 = .false.
                    credit = Subject(concpreq)%Units
                    if (PredictedLoad+credit <= AllowedLoad + addnl) then ! add both crse as priority subject
                        AddCoReq1 = .true.
#ifdef DBcoreq
                        call html_comment('Priority4b.1 #'//itoa(NPriority)//Subject(concpreq)%Name)
#endif
                    else
                        AddCoReq1 = .false.
                    end if
                end if

                ! add concpreq?
                if (AddCoreq1) then
                    NPriority = NPriority+1
                    CLExt(NPriority)%PriorityRank = idx
                    PredictedLoad = PredictedLoad + Subject(concpreq)%Units
                    CLExt(idx)%Disp_Remarks = 'Pri'//itoa(NPriority)
                    ! erase from p()
                    p(i) = 0
                end if

                ! add crse?
                if (AddCoReq2) then
                    NPriority = NPriority+1
                    CLExt(NPriority)%PriorityRank = kdx
                    PredictedLoad = PredictedLoad + Subject(crse)%Units
                    CLExt(kdx)%Disp_Remarks = 'Pri'//itoa(NPriority)
                    ! erase from p()
                    p(j) = 0
                end if
            end do

            ! Subgroup 4c, the rest of the subjects scheduled during later terms, and passed prerequisite
            !if (.not. IncompletePOCW) then
            do i=1,n
                idx = p(i)
                if (idx<=0) cycle ! already listed, or postponed for later
                crse = CheckList%SubjectIdx(idx)
                if (CheckList%SubjectTerm(idx)/=tdx) cycle
                credit = Subject(crse)%Units
                if (PredictedLoad+credit <= AllowedLoad + addnl) then
                    NPriority = NPriority+1
                    CLExt(NPriority)%PriorityRank = idx
                    CLExt(idx)%Disp_Remarks = 'Pri'//itoa(NPriority)
                    PredictedLoad = PredictedLoad + credit
#ifdef DBpriority
                    call html_comment('PriorityE #'//itoa(NPriority)//Subject(crse)%Name)
#endif
                    ! erase from p()
                    p(i) = 0
                end if
            end do

        end do ! tdx=StdTerm+1,Curriculum(idxCURR)%NumTerms

        ! collect alternates
        do i=1,n
            idx = abs(p(i)) ! co-requisite subjects, subjects whose prereq can be taken concurrently
            ! that cannot be added together have p(i)<0
            if (idx==0) cycle ! already listed
            crse = CheckList%SubjectIdx(idx)
            if (crse<=0) cycle ! subject not specified in PlanOfStudy
            credit = Subject(crse)%Units
            ! list as alternate; no contribution to forecast
            NAlternates = NAlternates + 1
            CLExt(NPriority+NAlternates)%PriorityRank = idx
            CLExt(idx)%Disp_Remarks = 'Alt'//itoa(NAlternates)
            CLExt(idx)%Contrib = 0.0
        end do
        !
        ! collect currently enrolled subjects
        n = 0
        p = 0
        prob = 0
!        if (isProbabilistic) then
!            do idx=1,CheckList%NSubjects
!                crse = CheckList%SubjectIdx(idx)
!                if (crse<=0) cycle ! subject not specified in PlanOfStudy
!                if (CLExt(idx)%Grade==gdxREGD .and. &
!                    (isSubject_offered(crse,thisTerm) .or. Offering(thisTerm,crse)%NSections>0)) then
!                    n = n+1
!                    p(n) = idx
!                    prob(n) = Subject(crse)%Failrate(cTm1)
!#ifdef DBpriority
!                    call html_comment(itoa(n)//Subject(crse)%Name//ftoa(prob(n),3))
!#endif
!                end if
!            end do
!        end if
!        ! sort according to decreasing failrate
!        do i=1,n-1
!            do j=i+1,n
!                if (prob(i) <= prob(j)) then
!                    k = p(i)
!                    p(i) = p(j)
!                    p(j) = k
!                    pLoad = prob(i)
!                    prob(i) = prob(j)
!                    prob(j) = pLoad
!                end if
!            end do
!#ifdef DBpriority
!        call html_comment(itoa(i)//Subject(CheckList%SubjectIdx(p(i)))%Name//ftoa(prob(i),3))
!#endif
!        end do
        ! calculate probabilistic load from priority subjects
        pLoad = 0.0
        do i=1,NPriority
            pLoad = pLoad + Subject(CheckList%SubjectIdx(CLExt(i)%PriorityRank))%Units
        end do
        ! add high failrate subjects as alternates, up to allowed load
        do i=1,n
            idx = p(i)
            if (idx==0) cycle
            crse = CheckList%SubjectIdx(idx)
            fCredit = Subject(crse)%Failrate(cTm1)*Subject(crse)%Units
#ifdef DBpriority
            call html_comment(itoa(i)//Subject(crse)%Name//ftoa(Subject(crse)%Failrate(cTm1),3)//ftoa(fCredit,1))
#endif
            !if (pLoad + fCredit < AllowedLoad + addnl) then
            if (pLoad < AllowedLoad + addnl) then
                ! list as alternate; contribute failure rate to forecast
                CLExt(idx)%Contrib = Subject(crse)%Failrate(cTm1) ! CHANGE THIS LATER TO CONDITIONAL PROBABILITY
                NCurrent = NCurrent + 1
                CLExt(NPriority+NAlternates+NCurrent)%PriorityRank = idx
                pLoad = pLoad + fCredit
                p(i) = 0

                ! check if there is a corequisite
                coreq = Subject(crse)%Corequisite(1)
                if (coreq>0) then ! co-requisite is a named subject
                    ! find co-requisite if it is further down the p() list
                    kdx = index_of_subject_in_curriculum(CheckList, coreq) ! index of co-requisite in Checklist()
                    FlagIsUp = .false.
                    do j=i+1,n
                        if (p(j)==kdx) then
                            FlagIsUp = .true. ! found
                            exit ! remember j
                        end if
                    end do
                    if (FlagIsUp) then ! found co-requisite
#ifdef DBpriority
                        call html_comment('Adding current as alternate: '//Subject(crse)%Name// &
                            ' and as co-requisite: '//Subject(coreq)%Name)
#endif
                        fCredit = Subject(coreq)%Failrate(cTm1)*Subject(coreq)%Units
                        ! list as alternate; contribute failure rate to forecast
                        CLExt(kdx)%Contrib = Subject(coreq)%Failrate(cTm1) ! CHANGE THIS LATER TO CONDITIONAL PROBABILITY
                        NCurrent = NCurrent + 1
                        CLExt(NPriority+NAlternates+NCurrent)%PriorityRank = kdx
                        pLoad = pLoad + fCredit
                        p(j) = 0 ! erase
                        cycle
                    end if
                end if

                ! check if there is a prerequisite that can be taken concurrently
                concpreq = Subject(crse)%Concprerequisite(1)
                FlagIsUp = .false.
                do j=1,n
                    kdx = p(j)
                    if (i==j .or. kdx<=0) cycle
                    crse = CheckList%SubjectIdx(kdx) ! index to Subject()
                    if (concpreq == crse) then
                        FlagIsUp = .true. ! found
                        exit ! remember j
                    end if
                end do
                if (FlagIsUp) then ! found concpreq
#ifdef DBpriority
                    call html_comment('Adding current as alternate: '//Subject(CheckList%SubjectIdx(idx))%Name// &
                        ' and its concurrent pre-requisite: '//Subject(concpreq)%Name)
#endif
                    fCredit = Subject(concpreq)%Failrate(cTm1)*Subject(concpreq)%Units
                    ! list as alternate; contribute failure rate to forecast
                    CLExt(kdx)%Contrib = Subject(concpreq)%Failrate(cTm1) ! CHANGE THIS LATER TO CONDITIONAL PROBABILITY
                    NCurrent = NCurrent + 1
                    CLExt(NPriority+NAlternates+NCurrent)%PriorityRank = kdx
                    pLoad = pLoad + fCredit
                    p(j) = 0 ! erase
                    cycle
                end if

                concpreq = CheckList%SubjectIdx(idx)
                ! is concpreq a prerequisite (that can be taken concurrently) to another subject?
                ! find crse to which concpreq is a conc prerequisite if it is in the p() list
                FlagIsUp = .false.
                do j=1,n
                    kdx = p(j)
                    if (i==j .or. kdx<=0) cycle
                    crse = CheckList%SubjectIdx(kdx) ! index to Subject()
                    if (concpreq == Subject(crse)%Concprerequisite(1)) then
                        FlagIsUp = .true. ! found
                        exit ! remember j
                    end if
                end do
                if (FlagIsUp) then ! found crse
#ifdef DBpriority
                    call html_comment('Adding current as alternate: '//Subject(CheckList%SubjectIdx(idx))%Name// &
                        ' which is concurrent pre-requisite to: '//Subject(concpreq)%Name)
#endif
                    fCredit = Subject(crse)%Failrate(cTm1)*Subject(crse)%Units
                    ! list as alternate; contribute failure rate to forecast
                    CLExt(kdx)%Contrib = Subject(crse)%Failrate(cTm1) ! CHANGE THIS LATER TO CONDITIONAL PROBABILITY
                    NCurrent = NCurrent + 1
                    CLExt(NPriority+NAlternates+NCurrent)%PriorityRank = kdx
                    pLoad = pLoad + fCredit
                    p(j) = 0 ! erase
                    cycle
                end if

            end if
        end do
        if (NPriority /= 0) then ! there are subjects to register

            if (codeScholastic == SCHOLASTIC_FAILED_NONE .or. codeScholastic == SCHOLASTIC_ALL_DRP_LAST_SEM) then   ! no failures, LOA
                codePriority = PRIORITY_GOOD
            else if (codeScholastic == SCHOLASTIC_FAILED_1SUBJ_PLUS .or. &  ! failed <= 25%
                     codeScholastic == SCHOLASTIC_FAILED_25PCT_PLUS) then   ! warning
                codePriority = PRIORITY_WARNING
            else if (codeScholastic == SCHOLASTIC_FAILED_50PCT_PLUS) then   ! probation
                codePriority = PRIORITY_PROBATION
#if defined UPLB
                if (thisTerm/=3) AllowedLoad = 15 ! max load for probation
#endif
            else if (codeScholastic == SCHOLASTIC_FAILED_75PCT_PLUS .or. &  ! dismissed
                     codeScholastic == SCHOLASTIC_ALL_FAILED_LAST_SEM) then   ! PD
                codePriority = PRIORITY_DISMISSED
#if defined UPLB
                if (thisTerm/=3) AllowedLoad = 12 ! max load for dismissed, PD
#endif
            else
                codePriority = PRIORITY_GOOD ! Assume good standing
            end if

            codeAdvising = ADVISING_IRREGULAR
            ! checked in advise_student() if priority subjects are as scheduled in curriculum

        else if (m /= 0) then
            ! none to register, and prerequisite of remaining subjects not satisfied; pity the student!
            codeAdvising = ADVISING_NO_SUBJECTS
            NRemaining = m
            do i=1,m
                CLExt(i)%PriorityRank = q(i)
                CLExt(q(i))%Contrib = 0.0
            end do

        !else ! no priority subjects, but something to enroll - cannot happen!

        end if

    end subroutine checklist_analyze

