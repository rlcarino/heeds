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


subroutine advise_student (std, UseClasses, Offering, WaiverCOI, Advice, MissingPOCW, NRemaining)

    integer, intent (in) :: std
    logical, intent (in) :: UseClasses
    type (TYPE_OFFERED_SUBJECTS), intent(in), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
    type (TYPE_WAIVER), intent (in)  :: WaiverCOI
    type (TYPE_PRE_ENLISTMENT), intent (out)  :: Advice
    integer, intent (out) :: MissingPOCW, NRemaining

    integer :: idxCURR, idxCOLL, idx, tdx, WaiverUnits
    integer :: AllowedLoad, Group, NPriority, NAlternates, NCurrent, &
    UnitsEarned, StdClassification, StdTerm, StdYear
    integer:: UnitsPaid, UnitsDropped, UnitsPassed, Standing

    call initialize_pre_enlistment(Advice)
    idxCURR = Student(std)%CurriculumIdx
    idxCOLL = Curriculum(idxCURR)%CollegeIdx
    WaiverUnits = 0
    do idx=1,WaiverCOI%lenSubject
        WaiverUnits = WaiverUnits + Subject(WaiverCOI%Subject(idx))%Units
    end do
    if (College(idxCOLL)%Code == ADMINISTRATION) then

        CheckList = Curriculum(idxCURR)
        CLExt = TYPE_CHECKLIST_EXTENSION (0, 0, 0, 0, 0.0, .false., .false., .false., &
        SPACE, SPACE, SPACE, SPACE, SPACE, SPACE, SPACE)

        ! set in custom_read_waivers(); get subjects from WaiverCOI
        Advice%AllowedLoad = WaiverUnits
        Advice%StdPriority = 0
        Advice%NPriority = WaiverCOI%lenSubject
        Advice%NAlternates = 0
        Advice%NCurrent = 0
        Advice%lenSubject = Advice%NPriority
        do idx=1,Advice%NPriority
            Advice%Subject(idx) = WaiverCOI%Subject(idx)
            Advice%Section(idx) = WaiverCOI%Section(idx)
            Advice%Contrib(idx) = 1.0
        end do
        MissingPOCW = 0
        NRemaining = 0
        return
    end if

    !call read_student_records (std)

    CheckList = Curriculum(idxCURR)
    CLExt = TYPE_CHECKLIST_EXTENSION (0, 0, 0, 0, 0.0, .false., .false., .false., &
    SPACE, SPACE, SPACE, SPACE, SPACE, SPACE, SPACE)

    call get_scholastic_three_terms (cTm1Year, cTm1, UnitsPaid, UnitsDropped, UnitsPassed, Standing)

    call analyze_checklist (std, UseClasses, Offering, WaiverCOI, Standing, &
        UnitsEarned, StdClassification, StdTerm, StdYear, MissingPOCW, &
        AllowedLoad, Group, NPriority, NAlternates, NCurrent, NRemaining)

    !write(unitLOG,*) &
    !  UnitsEarned, StdClassification, StdTerm, StdYear, MissingPOCW, &
    !  AllowedLoad, Group, NPriority, NAlternates, NCurrent, NRemaining

    if (NPriority==0) then ! no priority subjects?
        NAlternates = 0
        NCurrent = 0
    else if (AllowedLoad==0) then ! 0-unit subjects left?
        NPriority = 0
        NAlternates = 0
        NCurrent = 0
    end if

    Advice%UnitsEarned = UnitsEarned
    Advice%StdClassification = StdClassification
    Advice%StdYear = StdYear
    Advice%AllowedLoad = AllowedLoad
    Advice%StdPriority = Group
    Advice%NPriority = NPriority
    Advice%NAlternates = NAlternates
    Advice%NCurrent = NCurrent

    !write(unitLOG,*) &
    !  UnitsEarned, StdClassification, StdTerm, StdYear, MissingPOCW, &
    !  AllowedLoad, Group, NPriority, NAlternates, NCurrent, NRemaining
    ! recommended subjects
    do idx=1,NPriority+NAlternates+NCurrent
        tdx = CLExt(idx)%PriorityRank
        Advice%Subject(idx) = CheckList%SubjectIdx(tdx)
        Advice%Contrib(idx) = CLExt(tdx)%Contrib
    !write(unitLOG,*) idx, Subject(Advice%Subject(idx))%Name
    end do
    !idx=NPriority+NAlternates+NCurrent
    !write(unitLOG,*) idx, Subject(Advice%Subject(idx))%Name

    ! update from waiver-coi
    if (WaiverUnits>0) then
        ! set in custom_read_waivers(); get subjects from WaiverCOI
        Advice%AllowedLoad = WaiverUnits
        Advice%StdPriority = 0
        Advice%NPriority = WaiverCOI%lenSubject
        Advice%NAlternates = 0
        Advice%NCurrent = 0
        do idx=1,Advice%NPriority
            Advice%Subject(idx) = WaiverCOI%Subject(idx)
            Advice%Section(idx) = WaiverCOI%Section(idx)
            Advice%Contrib(idx) = 1.0
        end do
        MissingPOCW = 0
        NRemaining = 0
    end if
    Advice%lenSubject = Advice%NPriority+Advice%NAlternates+Advice%NCurrent

    return
end subroutine advise_student



subroutine analyze_checklist (std, UseClasses, Offering, WaiverCOI, stdScholastic, &
    UnitsEarned, StdClassification, StdTerm, StdYear, MissingPOCW, &
    AllowedLoad, Group, NPriority, NAlternates, NCurrent, NRemaining)

    ! Routine determines the remaining subjects to be taken by a student based on curriculum,
    !   enrollment record, plan of study, and nextTerm

    ! Student groupings based on grades in the last semester
    !   Group 1 - New Students (no record of any grade) & graduated? (no remaining subjects)
    !   Group 2 - Graduating (only 24 units or less to earn in curriculum)
    !   Group 3 - students who did not fail any subject
    !   Group 4 - students who failed <= 50% of units
    !   Group 5 - students who failed <= 75% of units, or on LOA
    !   Group 6 - students who failed > 75%
    !   Group 7 - Graduate students; non-degree students; diploma students
    !   Group 8 - Students who earned > 15 units whose priority subjects are for new freshman
    !   Group 9 - no predicted subjects but there are remaining subjects
    !   Group 10- finished

    implicit none
    integer, intent (in) :: std, stdScholastic
    logical, intent (in) :: UseClasses
    type (TYPE_OFFERED_SUBJECTS), intent(in), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
    type (TYPE_WAIVER) :: WaiverCOI
    integer, intent (out) :: AllowedLoad, Group, NPriority, NAlternates, NCurrent, NRemaining, &
    UnitsEarned, StdClassification, StdTerm, StdYear, MissingPOCW

    integer :: idxCURR, PredictedLoad
#ifdef DBperformance
    integer :: StdPerf, PERFUnitsEnrolled, PERFUnitsEarned ! (from raw TCG)
#endif
    integer :: UnitsTarget, UnitsFailed, UnitsConditional ! (from processed checklist)
    real    :: UnitsExpected, PercentEarned
    integer :: addnl, credit, UnitsToRepeat

    integer, dimension(0:MAX_SUBJECTS_IN_CURRICULUM,0:MAX_ALL_SUBJECT_PREREQ) :: CLPreq, CPPreq
    integer, dimension(0:MAX_SUBJECTS_IN_CURRICULUM) :: EarlyTime, LateTime, Slack
    integer :: Year, Term, Latest, Earliest, tCount
    integer :: ntokens
    integer, dimension(0:MAX_ALL_SUBJECT_PREREQ) :: tmpPreq
    real, dimension(0:MAX_ALL_SUBJECT_PREREQ) :: prob
    real :: fCredit, pLoad ! probabilistic load
    logical :: OKSubst, FlagIsUp, AddCoReq1, AddCoReq2

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
    AllowedLoad = 0
    Group = 0
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
    StdYear = 0
    StdClassification = 0
    !
    ! calculate prescribed units per term
    TermUnits = 0
    do kdx=1,CheckList%NSubjects
        rank = CheckList%SubjectTerm(kdx)
        crse = CheckList%SubjectIdx(kdx)
        TermUnits(rank) = TermUnits(rank) + Subject(crse)%Units
    end do

    ! add graded PE x  subjects to checklist
    do tdx = 1,lenTCG
        if (TCG(tdx)%Code /= 2) cycle ! not a grade
        crse = TCG(tdx)%Subject
        tSubject = Subject(crse)%Name
        if ( (tSubject(1:3) == 'PE ') .and. &
        is_grade_numeric_pass(TCG(tdx)%Grade) ) then
            ! (is_grade_numeric_pass(TCG(tdx)%Grade) .or. is_grade_conditional(TCG(tdx)%Grade) ) ) then
            do kdx=1,CheckList%NSubjects
                tSubject = Subject(CheckList%SubjectIdx(kdx))%Name
                if (tSubject(1:4) /= 'PE 1') cycle
                if (CLExt(kdx)%Grade /= 0) cycle
                CheckList%SubjectIdx(kdx) = crse
                CLExt(kdx)%Grade = TCG(tdx)%Grade
                CLExt(kdx)%Disp_Subject = tSubject ! (1:4)
                CLExt(kdx)%Disp_Input_Elective = Subject(crse)%Name
                CLExt(kdx)%Disp_Comment = Subject(crse)%Name
                if (.not. isRoleChair) then
                    CLExt(kdx)%Disp_Grade = txtGrade(pGrade(TCG(tdx)%Grade))
                elseif (is_grade_passing(CLExt(kdx)%Grade)) then
                    CLExt(kdx)%Disp_Grade = 'PASS'
                else
                    CLExt(kdx)%Disp_Grade = txtGrade(pGrade(CLExt(kdx)%Grade))
                end  if
                call blank_to_underscore(Subject(crse)%Name, input_name2)
                CLExt(kdx)%Disp_Input_Grade = '<input type="text" size="4" name="GRADE:'//trim(input_name2)//'" value="'// &
                trim(CLExt(kdx)%Disp_Grade)//'">'

                TCG(tdx)%Used = .true.
                exit
            end do
            ! if PE activity appears in POS, mark as used
            do kdx=1,lenTCG
                if (TCG(kdx)%Code /= 1) cycle ! not a plan
                if (TCG(kdx)%Subst(1) == crse) then
                    TCG(kdx)%Used = .true.
                end if
            end do
        end if
    end do
    !        ! add planned PE 2/3  subjects to checklist
    !        jdx = 1
    !        do tdx = 1,lenTCG
    !            if (TCG(tdx)%Code /= 1) cycle ! not PlanOfStudy
    !            crse = TCG(tdx)%Subst(1)
    !            token = Subject(crse)%Name
    !            if (token(1:4) /= 'PE 2' .and. token(1:4) /= 'PE 3') cycle
    !            if (TCG(tdx)%Used) cycle ! added previously
    !            call blank_to_underscore(token, input_name2)
    !            do kdx=jdx,CheckList%NSubjects
    !                tSubject = Subject(CheckList%SubjectIdx(kdx))%Name
    !                if (tSubject(1:4) /= 'PE 2') cycle
    !                if (CLExt(kdx)%Grade /= 0) cycle
    !                if (tSubject == token) exit ! planned PE 2 already taken
    !                CheckList%SubjectIdx(kdx) = crse
    !
    !                CLExt(kdx)%Disp_Subject = tSubject(1:4)
    !                CLExt(kdx)%Disp_Comment = token
    !                CLExt(kdx)%Disp_Input_Elective = '<input type="text" size="12" name="'// &
    !                    'PE_2'//DASH//trim(itoa(CheckList%SubjectTerm(kdx)))//':'// &
    !                    trim(input_name2)//'" value="'//trim(token)//'">'
    !
    !                TCG(tdx)%Used = .true.
    !                jdx = kdx+1
    !                exit
    !            end do
    !            if (.not. TCG(tdx)%Used) then ! extra PE 2 ?
    !                TCG(tdx)%Used = .true.
    !            end if
    !        end do
    !        ! warn about missing PE 2 activities
    !        tSubject = 'PE 2'
    !        call blank_to_underscore(tSubject, input_name1)
    !        do kdx=1,CheckList%NSubjects
    !            if (tSubject /= Subject(CheckList%SubjectIdx(kdx))%Name) cycle
    !            CLExt(kdx)%Disp_Comment = SPACE
    !            CLExt(kdx)%Disp_Input_Elective = '<input type="text" size="12" name="'// &
    !                trim(input_name1)//DASH//trim(itoa(CheckList%SubjectTerm(kdx)))//':">'
    !            CLExt(kdx)%Disp_Remarks = '*'
    !        end do
    !

    ! parse PLAN OF STUDY/SUBSTITUTIONS
    n = 0 ! entries in plan of study for later binding
    p = 0
    loop_POS : &
    do tdx = 1,lenTCG
        if (TCG(tdx)%Code /= 1) cycle ! not plan of study
        if (TCG(tdx)%Used) cycle      ! already used for PE 2, GE(XXX)
        if (TCG(tdx)%ErrorCode > 1) cycle
        !     if (Reqd(1) == ADDITIONAL) then
        !       ADD Subst(1) to Year,Term in curriculum
        if (TCG(tdx)%Reqd(0) == 1) then
            token = Subject(TCG(tdx)%Reqd(1))%Name

#ifdef DBsubst
            write(unitLOG,*) '? substitute '//token
#endif
            if (token == 'ADDITIONAL') then
                i = CheckList%NSubjects
                do j = 1,TCG(tdx)%Subst(0)
                    CheckList%NSubjects = i+j
                    CheckList%SubjectTerm(i+j) = TCG(tdx)%Term
                    CheckList%SubjectIdx(i+j) = TCG(tdx)%Subst(j)
                    CLExt(i+j)%Disp_Subject = token
                    input_value = Subject(TCG(tdx)%Subst(1))%Name
                    call blank_to_underscore(input_value, input_name2)
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
                    write(unitLOG,*) '   ---> '//trim(input_value)
#endif
                    call blank_to_underscore(token, input_name1)
                    call blank_to_underscore(input_value, input_name2)
                    CLExt(k)%Disp_Subject = token
                    CLExt(k)%Disp_Comment = input_value
                    CLExt(k)%Disp_Input_Elective = '<input type="text" size="12" name="'// &
                    trim(input_name1)//DASH//trim(itoa(CheckList%SubjectTerm(k)))//':'// &
                    trim(input_name2)//'" value="'//trim(input_value)//'">'
                    TCG(tdx)%Used = .true.
                else ! postpone binding
                    ! not found, or extra dummy subject; save for later processing
#ifdef DBsubst
                    write(unitLOG,*) '   not found, or extra; for later '
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
                    do j = 2,TCG(tdx)%Subst(0)
                        CheckList%NSubjects = i+j-1
                        CheckList%SubjectTerm(i+j-1) = CheckList%SubjectTerm(k)
                        CheckList%SubjectIdx(i+j-1) = TCG(tdx)%Subst(j)
                        CLExt(i+j-1)%Disp_Comment = token
                        !input_value = Subject(TCG(tdx)%Subst(j))%Name
                        !call blank_to_underscore(input_value, input_name2)
                        !call blank_to_underscore(token, input_name1)
                        !CLExt(i+j-1)%Disp_Subject = 'SUBST FOR '//token
                        !CLExt(i+j-1)%Disp_Input_Elective = '<input type="text" size="12" name="'// &
                        !  trim(input_name1)//DASH//trim(itoa(CheckList%SubjectTerm(k)))//':'// &
                        !  trim(input_name2)//'" value="'//trim(input_value)//'">'
                    end do
                    TCG(tdx)%Used = .true.

#ifdef DBsubst
                    write(unitLOG,*) '   (explicit substitution)'
#endif

                else ! not found; duplicate entry?

                    !write(unitLOG,*) '   (not found, or duplicate)'

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
                            call blank_to_underscore(Subject(p(i-1))%Name, input_name1)
                            call blank_to_underscore(input_value, input_name2)
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
                            call blank_to_underscore(input_value, input_name2)
                            call blank_to_underscore(Subject(p(i-1))%Name, input_name1)
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
                        call blank_to_underscore(input_value, input_name2)
                        call blank_to_underscore(Subject(p(i-1))%Name, input_name1)
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
        if (is_grade_numeric_pass(CLExt(idx)%Grade)) cycle ! subject has a grade; ignore
        crse = CheckList%SubjectIdx(idx)
        ! count missing entries in plan of study
        if (crse < 0) then ! elective not specified
            if (Subject(crse)%Units>0) MissingPOCW = MissingPOCW+1 ! count only non-zero units
            call blank_to_underscore(Subject(crse)%Name, input_name1)
            CLExt(idx)%Disp_Subject = Subject(crse)%Name
            CLExt(idx)%Disp_Comment = SPACE
            CLExt(idx)%Disp_Input_Elective = '<input type="text" size="12" name="'// &
            trim(input_name1)//DASH//trim(itoa(CheckList%SubjectTerm(idx)))//':">'
            CLExt(idx)%Disp_Remarks = '*'
        else
            do i=1,lenTCG
                if (TCG(i)%Code /= 2) cycle
                if ((.not. TCG(i)%Used) .and. TCG(i)%Subject == crse) then
                    CLExt(idx)%Grade = TCG(i)%Grade
                    if (index(CLExt(idx)%Disp_Input_Elective,'<input')>0) then
                        CLExt(idx)%Disp_Comment = Subject(crse)%Name
                    ! uncomment next line to disallow change of passed electives
                    !CLExt(idx)%Disp_Input_Elective = Subject(crse)%Name
                    end if
                    !!!!!!!!!!! use REMOVAL/COMPLETION grade?
                    !!!!!!!!!!if (TCG(i)%ReExam /= 0) CLExt(idx)%Grade = TCG(i)%ReExam
                    TCG(i)%Used = .true.
                    if (is_grade_numeric_pass(CLExt(idx)%Grade)) then
                        exit
                    end if
                end if
            end do
        end if
    end do


    ! automatic substitutions / equivalencies
    do idx=1,CheckList%NSubjects
        if (is_grade_passing(CLExt(idx)%Grade)) cycle ! consider earned
        crse = CheckList%SubjectIdx(idx)
#ifdef DBsubst
        write(unitLOG,*) crse, Subject(crse)%Name//' not yet earned?'
#endif
        loop_subst : &
        do i=1,NumSubst
            k = SubstIdx(i)
            if (Substitution(k+1) == crse .and. &
            (Substitution(k) == idxCURR .or. Substitution(k) == NumCurricula+1) ) then
                p = 0
                do j=k+2, SubstIdx(i+1)-1
                    OKSubst = .false.
#ifdef DBsubst
                    write(unitLOG,*) '       substitute is '//Subject(Substitution(j))%Name
#endif
                    do l=1,lenTCG
                        if (TCG(l)%Code /= 2) cycle
                        if (TCG(l)%Subject == Substitution(j) .and. &
                        !is_grade_numeric_pass(TCG(l)%Grade) ) then
                            is_grade_passing(TCG(l)%Grade,advisingPeriod) &
                            .and. (.not. TCG(l)%Used) ) then
                            p(j-k) = l
                            gdx = TCG(l)%Grade
                            OKSubst = .true.
#ifdef DBsubst
                            write(unitLOG,*) '       grade in '//Subject(Substitution(j))%Name//' is '//txtGrade(pGrade(gdx))
#endif
                            exit
                        end if
                    end do
                    if (.not. OKSubst) then
#ifdef DBsubst
                        write(unitLOG,*) '       '//Subject(Substitution(j))%Name//' not yet earned; substitution not valid'
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
    SpecifiedUnits = 0
    do idx=1,CheckList%NSubjects
        k = Subject(CheckList%SubjectIdx(idx))%Units
        rank = CheckList%SubjectTerm(idx)
        SpecifiedUnits(rank) = SpecifiedUnits(rank) + k
        ! units passed, conditional in curriculum; expected from REGD subjects
        if (CLExt(idx)%Grade==gdxREGD ) then
            if (advisingPeriod) then
                UnitsExpected = UnitsExpected + k*Failrate(CheckList%SubjectIdx(idx),cTm1)
                UnitsEarned = UnitsEarned + k ! optimistic units earned
            end if
        else if (is_grade_passing(CLExt(idx)%Grade) ) then
            UnitsEarned = UnitsEarned + k
        else if (is_grade_conditional(CLExt(idx)%Grade) ) then
            UnitsConditional = UnitsConditional + k
        else if (is_grade_failing(CLExt(idx)%Grade) ) then
            UnitsToRepeat = UnitsToRepeat + k
        end if
    end do
    ! units failed (all subjects registered)
    UnitsFailed = 0
    do idx=1,lenTCG
        if (TCG(idx)%Code /= 2) cycle
        crse = TCG(idx)%Subject
        if (is_grade_failing(TCG(idx)%Grade)) then
            UnitsFailed = UnitsFailed + Subject(crse)%Units
        end if
    end do
    ! student performance
#ifdef DBperformance
    PERFUnitsEnrolled = 0
    PERFUnitsEarned = 0
    do idx=1,lenTCG
        if (TCG(idx)%Code /= 2) cycle
        if (index(TCG(idx)%txtLine, 'APE')>0) cycle ! not enrolled
        if (index(TCG(idx)%txtLine, 'REMOVAL')>0) cycle ! not enrolled
        if (index(TCG(idx)%txtLine, 'COMPLETION')>0) cycle ! not enrolled
        if (TCG(idx)%Grade==gdxDRP .or. &
        TCG(idx)%Grade==gdxLOA .or. &
        TCG(idx)%Grade==gdxREGD) cycle ! do not count
        crse = TCG(idx)%Subject
        credit = Subject(crse)%Units
        gdx = TCG(idx)%Grade
        ! count towards units enrolled
        PERFUnitsEnrolled = PERFUnitsEnrolled + credit
        ! count towards units earned
        if (is_grade_passing(gdx,advisingPeriod)) then
            PERFUnitsEarned = PERFUnitsEarned + credit
        else if (is_grade_conditional(gdx) .and. is_grade_passing(TCG(idx)%ReExam)) then
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
    do i=1,StdTerm
        SpecifiedUnits(i+1) = SpecifiedUnits(i+1)  + SpecifiedUnits(i)
    end do
    UnitsTarget = SpecifiedUnits(StdTerm)
#ifdef DBunits
    write(*,AFORMAT) trim(text_student_info(std))
    write(*,'(a,20i4)')    '       Unit load by term :', (TermUnits(j), j=1,Curriculum(idxCURR)%NumTerms)
    write(*,'(a,20i4)')    ' Cumulative units by term:', (SpecifiedUnits(j), j=1,Curriculum(idxCURR)%NumTerms)
    write(*,'(a,7(a,i3))') ' Cumulative units by year:', &
    (SPACE//trim(txtYear(j))//DASH,SpecifiedUnits(3*j), j=1,(Curriculum(idxCURR)%NumTerms+1)/3)
    write(*,'(a,4(a,f7.2))') '     Classification units:', &
    (SPACE//trim(txtStanding(j))//DASH,(0.25*j)*UnitsTarget, j=1,4)
#endif
    do i=Curriculum(idxCURR)%NumTerms,1,-1
        if (UnitsEarned < SpecifiedUnits(i)) StdTerm = i
    end do
    ! adjust stdterm according to nextTerm
    if (nextTerm==3) then
        StdTerm = 3*( (StdTerm-1)/3 + 1)
    else if (nextTerm==1) then
        if (mod(StdTerm,3)/=1) StdTerm = 3*( (StdTerm-1)/3 + 1) + 1
    else
        if (mod(StdTerm,3)/=2) StdTerm = 3*((StdTerm-1)/3) + 2
        ! special case for new freshmen entering 2nd semester
        if (UnitsEarned < TermUnits(1)) StdTerm = 1
    end if
    StdYear = min(StdTerm,Curriculum(idxCURR)%NumTerms)/3
    AllowedLoad = TermUnits(min(StdTerm,Curriculum(idxCURR)%NumTerms))
    if (AllowedLoad == 0) AllowedLoad = 18

    ! compute classification
    PercentEarned = (100.0*UnitsEarned)/UnitsTarget + 0.55
    if (PercentEarned>75.0) then
        StdClassification = 4
    elseif (PercentEarned>50.0) then
        StdClassification = 3
    elseif (PercentEarned>25.0) then
        StdClassification = 2
    else
        StdClassification = 1
    end if

#ifdef DBunits
    write(unitLOG,*) 'Target=', UnitsTarget, &
    'Earned=', UnitsEarned, &
    ', % earned=', (100.0*UnitsEarned)/UnitsTarget, &
    ', StdClassification=', StdClassification, &
    ', StdTerm=', StdTerm,  &
    ', StdYear=', Stdyear, &
    ', Allowed=', AllowedLoad
#endif

    ! collect prerequisites; simplify ANDs and ORs
    CLPreq = 0
    CPPreq = 0
    do idx=1,CheckList%NSubjects
        crse = CheckList%SubjectIdx(idx)
        rank = CheckList%SubjectTerm(idx)
        call rank_to_year_term(rank, Year, Term)
        if (crse > 0) then ! NOT a special subject
#ifdef DBsimplify
            write(unitLOG,*) Year, Term, Subject(crse)%Name
#endif
            ! collect prerequisites
            ntokens = Subject(crse)%lenPreq
            CLPreq(idx,0) = ntokens
            do j = 1, ntokens
                ! succeeding elements are indices to special subjects or index of subject in checklist
                k = Subject(crse)%Prerequisite(j)
                tSubject = Subject(k)%Name
                if (k < 0) then ! special
                    CLPreq(idx,j) = k
                    ! if no waiver/COI file, change COI prerequisite to standing when taken in curriculum
                    if (NumWaiverRecords==0 .and. ntokens == 1 .and. tSubject == 'COI') then
                        tSubject = txtYear(Year)
                        jdx = index_to_subject(tSubject)
                        CLPreq(idx,1) = jdx
                    end if
                    ! set JUNIOR/SENIOR prerequisite to year taken in curriculum if the subject
                    ! is taken earlier than 3d/4th year
                    !if ((tSubject == 'JUNIOR' .and. Year < 3) .or. &
                    !    (tSubject == 'SENIOR' .and. Year < 4)) then
                    !  tSubject = txtStanding(Year)
                    !  jdx = index_to_subject(tSubject)
                    !  CLPreq(idx,1) = jdx
                    !end if
                else
                    jdx = index_of_subject_in_curriculum(CheckList, k)
                    if (jdx == 0) then
                        ! prerequisite not present in curriculum
                        jdx = index_to_subject(tSubject)
                        jdx = NumDummySubjects - jdx
                    end if
                    ! save index of prerequisite subject in checklist
                    CLPreq(idx,j) = jdx
                end if

#ifdef DBsimplify
                write(unitLOG,*) j, k, SPACE, tSubject, CLPreq(idx,j)
#endif

            end do
        else
            ! a special subject; Plan of Study not properly accomplished
            ! set prerequisite to year taken in curriculum
            tSubject = txtStanding(Year)
            CLPreq(idx,1) = index_to_subject(tSubject)
            CLPreq(idx,0) = 1
        end if ! (crse > 0)

        ! remove ORs in prerequisite for critical path computations
        ntokens = CLPreq(idx,0)
        if (ntokens > 1) then
            tmpPreq = 0
            do j=0,ntokens
                tmpPreq(j) = CLPreq(idx,j)
            end do
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
                CPPreq(idx,i) = tmpPreq(j)
            end if
        end do
        CPPreq(idx,0) = i
    end do ! idx=1,CheckList%NSubjects

    ! critical path
    EarlyTime = 0
    LateTime = 0
    Slack = 0
    do idx=1,CheckList%NSubjects
        gdx = CLExt(idx)%Grade
        if (gdx==0 .or. is_grade_failing(gdx) .or. (.not. advisingPeriod .and. gdx==gdxREGD) ) EarlyTime(idx) = 1
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
                        do kdx=1,CPPreq(jdx,0)
                            if (CPPreq(jdx,kdx) == idx) then
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
                do kdx=1,CPPreq(jdx,0)
                    if (CPPreq(jdx,kdx) == idx) then
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
                        do kdx=1,CPPreq(idx,0)
                            if (CPPreq(idx,kdx) == jdx) then
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
    write(unitLOG,*) 'Zero-slack subjects, rank, early start...'
#endif
    do idx=1,CheckList%NSubjects
        Slack(idx) = LateTime(idx) - EarlyTime(idx)
        CLExt(idx)%EarlyTime = EarlyTime(idx)
        CLExt(idx)%LateTime = LateTime(idx)
#ifdef DBsimplify
        if (Slack(idx) == 0 .and. EarlyTime(idx) /= 0) then
            write(*,'(1x,a,3i4)') Subject(CheckList%SubjectIdx(idx))%Name, CheckList%SubjectTerm(idx), EarlyTime(idx)
        end if
#endif
    end do

    ! identify subjects not passed, OK prerequisite, and offered
    do idx=1,CheckList%NSubjects
        crse = CheckList%SubjectIdx(idx)
        rank = CheckList%SubjectTerm(idx)
#ifdef DBprereq
        write(unitLOG,*) idx, rank, Subject(crse)%Name
#endif
        ! display string
        tSubject = Subject(crse)%Name
        if (len_trim(CLExt(idx)%Disp_Subject)==0) CLExt(idx)%Disp_Subject = tSubject
        CLExt(idx)%Disp_Units = ftoa(Subject(crse)%Units,1)
        if (.not. isRoleChair) then ! Adviser or REGISTRAR
            CLExt(idx)%Disp_Grade = txtGrade(pGrade(CLExt(idx)%Grade))
        elseif (is_grade_passing(CLExt(idx)%Grade,advisingPeriod)) then ! Dept. Chair
            CLExt(idx)%Disp_Grade = 'PASS'
        else
            CLExt(idx)%Disp_Grade = txtGrade(pGrade(CLExt(idx)%Grade))
        end  if
        call blank_to_underscore(tSubject, input_name2)
        CLExt(idx)%Disp_Input_Grade = '<input type="text" size="4" name="GRADE:'//trim(input_name2)//'" value="'// &
            trim(CLExt(idx)%Disp_Grade)//'">'

        ! evaluate prerequisite
        tmpPreq = 0
        prob = 0.0
        ntokens = CLPreq(idx,0)
        tmpPreq(0:ntokens) = CLPreq(idx,0:ntokens)
        FlagIsUp = .false.   ! standing prerequisite?
        do j = ntokens,1,-1
            jdx = tmpPreq(j)
            if (jdx>0) then ! a subject in the checklist
                tSubject = Subject(CheckList%SubjectIdx(jdx))%Name
                if (CLExt(jdx)%Grade==gdxREGD) then
                    if (advisingPeriod) then
                        prob(j) = 1.0-Failrate(CheckList%SubjectIdx(jdx),cTm1)
                        tmpPreq(j) = 1
                    else
                        tmpPreq(j) = 0
                        prob(j) = 0.0
                    end if
#ifdef DBprereq
                    tNote = 'REGISTERED'
#endif
                else if (is_grade_passing(CLExt(jdx)%Grade)) then
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
                write(unitLOG,*) 'In checklist, ', tNote, j, jdx, SPACE, tSubject, tmpPreq(j), prob(j)
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
                    if (StdClassification >= 2) then
                        prob(j) = 1.0
                        tmpPreq(j) = 1
                    else
                        prob(j) = 0.0
                        tmpPreq(j) = 0
                    end if
                else if (tSubject == 'JUNIOR') then
                    FlagIsUp = .true.
                    if (StdClassification >= 3) then
                        prob(j) = 1.0
                        tmpPreq(j) = 1
                    else
                        prob(j) = 0.0
                        tmpPreq(j) = 0
                    end if
                else if (tSubject == 'SENIOR') then
                    FlagIsUp = .true.
                    if (StdClassification >= 4) then
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
                    if (UnitsTarget-UnitsEarned <= 24) then
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
                write(unitLOG,*) 'Special, ', tNote, j, jdx, SPACE, tSubject, tmpPreq(j), prob(j)
#endif

            else ! not in curriculum; try all registered subjects
                prob(j) = 0.0
                tmpPreq(j) = 0
#ifdef DBprereq
                tNote = 'Not taken'
#endif
                do k=1,lenTCG
                    if (TCG(k)%Code /= 2) cycle
                    if (-TCG(k)%Subject == jdx-NumDummySubjects .and. &
                        is_grade_passing(TCG(k)%Grade,advisingPeriod) ) then
                        prob(j) = 1.0
                        tmpPreq(j) = 1
#ifdef DBprereq
                        tNote = 'EARNED'
#endif
                    end if
                end do
                tSubject = Subject(-(jdx-NumDummySubjects))%Name

#ifdef DBprereq
                write(unitLOG,*) 'Not in curriculum, ', tNote, j, jdx, SPACE, tSubject, tmpPreq(j), prob(j)
#endif

            end if
        end do
        ! satisfied?
        CLExt(idx)%OKPreq = tmpPreq(1) > 0
        CLExt(idx)%Contrib = prob(1)

#ifdef DBprereq
        tNote = ' '
#endif
        if (WaiverCOI%lenSubject>0) then ! there are forced subjects; ignore prerequisite
            do k=1,WaiverCOI%lenSubject
                if (crse==WaiverCOI%Subject(k)) then
                    CLExt(idx)%OKPreq = .true.
                    CLExt(idx)%Contrib = 1.0
                    !write(unitLOG,*)  'Waiver/COI: '//Subject(crse)%Name//trim(text_student_info(std))
                    exit
                end if
            end do
#ifdef DBprereq
            tNote = 'WAIVER/COI'
#endif

        end if

#ifdef DBprereq
        write(unitLOG,*) trim(tNote), ' OKpreq=', CLExt(idx)%OKPreq
#endif
        ! problem with records?
        if (CLExt(idx)%Grade==gdxREGD) then
            !write(token, '(f5.2)') 1.0-Failrate(crse,cTm1)
            if (.not. advisingPeriod) CLExt(idx)%Disp_Grade = '(NG)'
            CLExt(idx)%Disp_Remarks = SPACE
        else if (is_grade_passing(CLExt(idx)%Grade) ) then
            CLExt(idx)%Disp_Remarks = SPACE
        ! missing grade of prerequisite
        !if (.not. CLExt(idx)%OKPreq .and. .not. FlagIsUp) &
        else if (is_grade_conditional(CLExt(idx)%Grade) ) then
            CLExt(idx)%Disp_Remarks = '%'

            ! fail "lapsed" 4.0 and INC
            do tdx = lenTCG,1,-1
                if (TCG(tdx)%Code /= 2) cycle ! not a grade
                if (TCG(tdx)%Subject == crse .and. TCG(tdx)%Grade == CLExt(idx)%Grade ) then
                    if (now-TCG(tdx)%Taken > 3) then
                        CLExt(idx)%Grade = gdx5
                        CLExt(idx)%Disp_Remarks = '#'
                    end if
                    exit
                end if
            end do
        !
        else if (is_grade_failing(CLExt(idx)%Grade) ) then
            CLExt(idx)%Disp_Remarks = '#'
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
        if ( gdx==0 .or. is_grade_failing(gdx) .or. (gdx==gdxREGD .and. .not. advisingPeriod) ) then ! no grade or failed
            if (CLExt(idx)%OKPreq .and. CheckList%SubjectIdx(idx) > 0) then ! named subject, not passed, prereq is satisfied

                FlagIsUp = .true. ! by default, add to list of subjects with satisfied prereqs

                ! check if subject has a co-requisite or if subject has a prerequisite that can be taken concurrently
                ! logic works only if co-req is a named subject, not AND/OR expression

                ! check if there is a corequisite
                cdx = Subject(CheckList%SubjectIdx(idx))%Corequisite(1)
                if (cdx>0) then ! corequisite is a named subject
#ifdef DBcoreq
                    write(unitLOG,*) 'OK preq: '//Subject(CheckList%SubjectIdx(idx))%Name// &
                    ' is not passed, but prereq is satisfied; corequisite is '//Subject(cdx)%Name
#endif
                    kdx = index_of_subject_in_curriculum(CheckList, cdx)
                    if (kdx>0) then ! found in checklist; should be passed, or at least, prereq is satisfied
                        if (is_grade_passing(CLExt(kdx)%Grade,advisingPeriod)) then ! co-req is passed
#ifdef DBcoreq
                            write(unitLOG,*) '  - which is a required subject already passed; ADD.'
#endif
                            FlagIsUp = .true.
                        else if (CLExt(kdx)%OKPreq) then ! co-req's prereq is satisfied (co-req is predicted)
#ifdef DBcoreq
                            write(unitLOG,*) '  - which is a required subject whose prereq is already satisfied; ADD.'
#endif
                            FlagIsUp = .true.
                        else ! co-requisite not passed, nor is its prereq satisfied
#ifdef DBcoreq
                            write(unitLOG,*) '  - which is a required subject not passed nor satisfied prereq; DO NOT ADD'
#endif
                            FlagIsUp = .false.
                        end if

                    else ! co-requisite is not in student's curriculum
#ifdef DBcoreq
                        write(unitLOG,*) '  - which is NOT a required subject; curriculum is poorly desgined; DO NOT ADD!'
#endif
                        FlagIsUp = .false.
                    end if

                else ! no corequisite

                    ! check if there is a prerequisite that can be taken concurrently
                    cdx = Subject(CheckList%SubjectIdx(idx))%Concprerequisite(1)
                    if (cdx>0) then ! conc. prerequisite is a named subject
#ifdef DBconcpreq
                        write(unitLOG,*) 'OK preq: '//Subject(CheckList%SubjectIdx(idx))%Name// &
                        ' is not passed and prereq is satisfied; addnl prerequisite that can be taken concurrently is '// &
                        Subject(cdx)%Name
#endif
                        kdx = index_of_subject_in_curriculum(CheckList, cdx)
                        if (kdx>0) then ! found in checklist; should be passed, or at least, prereq is satisfied
                            if (is_grade_passing(CLExt(kdx)%Grade,advisingPeriod)) then ! conc. pre-req is passed
#ifdef DBconcpreq
                                write(unitLOG,*) '  - which is a required subject already passed; ADD.'
#endif
                                CLExt(idx)%Contrib = CLExt(kdx)%Contrib
                                FlagIsUp = .true.
                            else if (CLExt(kdx)%OKPreq) then ! conc. pre-req's prereq is satisfied (conc. pre-req is predicted)
#ifdef DBconcpreq
                                write(unitLOG,*) '  - which is a required subject whose prereq is already satisfied; ADD.'
#endif
                                CLExt(idx)%Contrib = CLExt(kdx)%Contrib
                                FlagIsUp = .true.
                            else
#ifdef DBconcpreq
                                write(unitLOG,*) '  - which is a required subject not passed nor satisfied prereq; DO NOT ADD'
#endif
                                FlagIsUp = .false.
                            end if

                        else
#ifdef DBconcpreq
                            write(unitLOG,*) '  - which is NOT a required subject; curriculum is poorly desgined; DO NOT ADD!'
#endif
                            FlagIsUp = .false.
                        end if

                    end if

                end if

                if (FlagIsUp) then ! ok to add
                    n = n+1
                    p(n) = idx
                end if

            else ! not passed and not satisfied prereq

                m = m+1
                q(m) = idx

            end if

        end if
    end do ! idx=1,CheckList%NSubjects
    !write(unitLOG,*) Student(std)%StdNo, ', n=', n, ', m=', m

!    write(unitLOG,*) 'OK prerequisite...'
!    do i=1,n
!        idx = p(i)
!        if (idx==0) cycle
!        write(unitLOG,*) i, Subject(CheckList%SubjectIdx(idx))%Name
!    end do


    ! no remaining subjects with unsatisfied prerequisites
    if (m == 0) then
        if (n == 0) then
            ! finished all academic subjects in curriculum
            Group = 10
            return
        else if (StdClassification >= 4 .and. UnitsTarget-UnitsEarned <= 24) then
            ! graduating student: recommend all subjects even if not offered
            Group = 2
            j = 0
            do i = 1,n
                crse = CheckList%SubjectIdx(p(i))
                j = j + Subject(crse)%Units
            end do
            AllowedLoad = amax0(AllowedLoad,j)

            ! add as priority subjects
            do i = 1,n
                if (p(i)==0) cycle
                NPriority = NPriority+1
                !write(unitLOG,*) 'PriorityA #', NPriority, Subject(CheckList%SubjectIdx(p(i)))%Name
                CLExt(NPriority)%PriorityRank = p(i)
                CLExt(p(i))%Disp_Remarks = 'Pri'//itoa(NPriority)
            end do

            ! add currently enrolled subjects as alternates
            do idx=1,CheckList%NSubjects
                crse = CheckList%SubjectIdx(idx)
                if (crse<=0) cycle ! subject not specified in PlanOfStudy
                if (CLExt(idx)%Grade==gdxREGD .and. advisingPeriod .and. &
                    (is_offered(crse,nextTerm) .or. Offering(crse)%NSections>0)) then
                    CLExt(idx)%Contrib = Failrate(crse,cTm1) ! CHANGE THIS LATER TO CONDITIONAL PROBABILITY
                    ! list as alternate; contribute failure rate to forecast
                    NCurrent = NCurrent + 1
                    CLExt(NPriority+NAlternates+NCurrent)%PriorityRank = idx
                end if
            end do

            return
        end if
    end if


    !        ! remove PE 2/PE 3 without activity
    !        do idx=1,n
    !            if (p(idx) == 0) cycle
    !            tSubject = Subject(CheckList%SubjectIdx(p(idx)))%Name
    !            if (tSubject == 'PE 2') then
    !                p(idx) = 0
    !            end if
    !        end do

    ! remove subjects not offered
    if (UseClasses) then
        do idx=1,n
            if (p(idx) == 0) cycle
            if (Offering(CheckList%SubjectIdx(p(idx)))%NSections == 0) then
                p(idx) = 0
            end if
        end do
    else
        do idx=1,n
            if (p(idx) == 0) cycle
            if (.not. is_offered(CheckList%SubjectIdx(p(idx)),nextTerm) ) then
                p(idx) = 0
            end if
        end do
    end if

!    write(unitLOG,*) 'Offered during term=', nextTerm
!    do i=1,n
!        idx = p(i)
!        if (idx==0) cycle
!        write(unitLOG,*) i, Subject(CheckList%SubjectIdx(idx))%Name
!    end do

    !        ! remove PE 2/3 if PE 1 is present
    !        do idx=1,n
    !            if (p(idx) == 0) cycle
    !            tSubject = Subject(CheckList%SubjectIdx(p(idx)))%Name
    !            if (tSubject(1:4) == 'PE 1') then ! if present, remove PE 2
    !                do i=idx+1,n
    !                    if (p(i) == 0) cycle
    !                    token = Subject(CheckList%SubjectIdx(p(i)))%Name
    !                    if (token(1:4) == 'PE 2' .or. token(1:4) == 'PE 3') then
    !                        p(i) = 0
    !                    !write(unitLOG,*) 'PE 1 recommended; removed '//token
    !                    end if
    !                end do
    !            end if
    !        end do
    !        ! only one PE 2 must be present
    !        do idx=1,n
    !            if (p(idx) == 0) cycle
    !            tSubject = Subject(CheckList%SubjectIdx(p(idx)))%Name
    !            if (tSubject(1:4) == 'PE 2') then  ! there must be only one PE 2
    !                do i=idx+1,n
    !                    if (p(i) == 0) cycle
    !                    token = Subject(CheckList%SubjectIdx(p(i)))%Name
    !                    if (token(1:4) == 'PE 2' .or. token(1:4) == 'PE 3') then
    !                        p(i) = 0
    !                    !write(unitLOG,*) trim(tSubject)//' recommended; removed '//token
    !                    end if
    !                end do
    !            end if
    !        end do

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

!    write(unitLOG,*) 'Compressed...'
!    do i=1,n
!        idx = p(i)
!        write(unitLOG,*) i, Subject(CheckList%SubjectIdx(idx))%Name
!    end do


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

!    write(unitLOG,*) 'Sorted by slack time...'
!    do i=1,n
!        idx = p(i)
!        write(unitLOG,*) i, Subject(CheckList%SubjectIdx(idx))%Name
!    end do

    ! "fudge" units
    if (UnitsEarned==0 .or. AllowedLoad>=20) then
        addnl = 0 ! no fudge for new freshmen; high load semesters
    else
        addnl = 1 ! 1 unit fudge for old students
    end if

    ! collect subjects up to allowed load;

    ! add 0-credit subjects as Group 1
    do i=1,n
        idx = p(i)
        if (Subject(CheckList%SubjectIdx(idx))%Units == 0) then
            NPriority = NPriority+1
            CLExt(NPriority)%PriorityRank = idx
            CLExt(idx)%Disp_Remarks = 'Pri'//itoa(NPriority)
            !write(unitLOG,*) 'Priority1 #', NPriority, Subject(CheckList%SubjectIdx(idx))%Name
            ! erase from p()
            p(i) = 0
        end if
    end do

    ! Group 2, the scheduled subjects
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
            !write(unitLOG,*) 'Priority2 #', NPriority, Subject(crse)%Name
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
                    write(unitLOG,*) 'Priority3a.2 #', NPriority, Subject(crse)%Name, NPriority+1, Subject(coreq)%Name
#endif
                else ! add both crse & coreq as alternate subjects later
                    p(i) = -p(i)
                    p(j) = -p(j)
                    AddCoReq1 = .false.
                    AddCoReq2 = .false.
#ifdef DBcoreq
                    write(unitLOG,*) 'Add as ALTERNATE subjects: ', Subject(crse)%Name, Subject(coreq)%Name
#endif
                end if

            else ! only crse is a priority subject, if allowed load not exceeded
                AddCoReq2 = .false.
                credit = Subject(crse)%Units
                if (PredictedLoad+credit <= AllowedLoad + addnl) then ! add both crse & coreq as priority subjects
                    AddCoReq1 = .true.
#ifdef DBcoreq
                    write(unitLOG,*) 'Priority3a.1 #', NPriority, Subject(crse)%Name
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
                    write(unitLOG,*) 'Priority3b.2 #', NPriority, Subject(crse)%Name, NPriority+1, Subject(concpreq)%Name
#endif
                else ! add both crse & concpreq as alternate subjects later
                    p(i) = -p(i)
                    p(j) = -p(j)
                    AddCoReq1 = .false.
                    AddCoReq2 = .false.
#ifdef DBconcpreq
                    write(unitLOG,*) 'Add as ALTERNATE subjects: ', Subject(crse)%Name, Subject(concpreq)%Name
#endif
                end if
            else ! only concpreq is a priority subject, if allowed load not exceeded
                AddCoReq2 = .false.
                credit = Subject(concpreq)%Units
                if (PredictedLoad+credit <= AllowedLoad + addnl) then ! add both crse as priority subject
                    AddCoReq1 = .true.
#ifdef DBcoreq
                    write(unitLOG,*) 'Priority3b.1 #', NPriority, Subject(concpreq)%Name
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
                !write(unitLOG,*) 'PriorityD #', NPriority, Subject(crse)%Name
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
                    write(unitLOG,*) 'Priority4a.2 #', NPriority, Subject(crse)%Name, NPriority+1, Subject(coreq)%Name
#endif
                else ! add both crse & coreq as alternate subjects later
                    p(i) = -p(i)
                    p(j) = -p(j)
                    AddCoReq1 = .false.
                    AddCoReq2 = .false.
#ifdef DBcoreq
                    write(unitLOG,*) 'Add as ALTERNATE subjects: ', Subject(crse)%Name, Subject(coreq)%Name
#endif
                end if

            else ! only crse is a priority subject, if allowed load not exceeded
                AddCoReq2 = .false.
                credit = Subject(crse)%Units
                if (PredictedLoad+credit <= AllowedLoad + addnl) then ! add both crse as priority subject
                    AddCoReq1 = .true.
#ifdef DBcoreq
                    write(unitLOG,*) 'Priority4a.1 #', NPriority, Subject(crse)%Name
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
                    write(unitLOG,*) 'Priority4b.2 #', NPriority, Subject(crse)%Name, NPriority+1, Subject(concpreq)%Name
#endif
                else ! add both crse & concpreq as alternate subjects later
                    p(i) = -p(i)
                    p(j) = -p(j)
                    AddCoReq1 = .false.
                    AddCoReq2 = .false.
#ifdef DBconcpreq
                    write(unitLOG,*) 'Add as ALTERNATE subjects: ', Subject(crse)%Name, Subject(concpreq)%Name
#endif
                end if
            else ! only concpreq is a priority subject, if allowed load not exceeded
                AddCoReq2 = .false.
                credit = Subject(concpreq)%Units
                if (PredictedLoad+credit <= AllowedLoad + addnl) then ! add both crse as priority subject
                    AddCoReq1 = .true.
#ifdef DBcoreq
                    write(unitLOG,*) 'Priority4b.1 #', NPriority, Subject(concpreq)%Name
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
                !write(unitLOG,*) 'PriorityE #', NPriority, Subject(crse)%Name
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
    do idx=1,CheckList%NSubjects
        crse = CheckList%SubjectIdx(idx)
        if (crse<=0) cycle ! subject not specified in PlanOfStudy
        if (CLExt(idx)%Grade==gdxREGD .and. advisingPeriod .and. &
            (is_offered(crse,nextTerm) .or. Offering(crse)%NSections>0)) then
            n = n+1
            p(n) = idx
            prob(n) = Failrate(crse,cTm1)
        !write(unitLOG,*) n, Subject(crse)%Name, prob(n)
        end if
    end do
    ! sort according to decreasing failrate
    do i=1,n-1
        do j=i+1,n
            if (prob(i) <= prob(j)) then
                k = p(i)
                p(i) = p(j)
                p(j) = k
                pLoad = prob(i)
                prob(i) = prob(j)
                prob(j) = pLoad
            end if
        end do
    !write(unitLOG,*) i, Subject(CheckList%SubjectIdx(p(i)))%Name, prob(i)
    end do
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
        fCredit = Failrate(crse,cTm1)*Subject(crse)%Units
        !write(unitLOG,*) i, Subject(crse)%Name, Failrate(crse,cTm1), fCredit
        !if (pLoad + fCredit < AllowedLoad + addnl) then
        if (pLoad < AllowedLoad + addnl) then
            ! list as alternate; contribute failure rate to forecast
            CLExt(idx)%Contrib = Failrate(crse,cTm1) ! CHANGE THIS LATER TO CONDITIONAL PROBABILITY
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
                    !write(unitLOG,*) 'Adding current as alternate: '//Subject(crse)%Name//' and as co-requisite: ' &
                    !    //Subject(coreq)%Name
                    fCredit = Failrate(coreq,cTm1)*Subject(coreq)%Units
                    ! list as alternate; contribute failure rate to forecast
                    CLExt(kdx)%Contrib = Failrate(coreq,cTm1) ! CHANGE THIS LATER TO CONDITIONAL PROBABILITY
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
                !write(unitLOG,*) 'Adding current as alternate: '//Subject(CheckList%SubjectIdx(idx))%Name// &
                !  ' and its concurrent pre-requisite: '//Subject(concpreq)%Name
                fCredit = Failrate(concpreq,cTm1)*Subject(concpreq)%Units
                ! list as alternate; contribute failure rate to forecast
                CLExt(kdx)%Contrib = Failrate(concpreq,cTm1) ! CHANGE THIS LATER TO CONDITIONAL PROBABILITY
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
                !write(unitLOG,*) 'Adding current as alternate: '//Subject(CheckList%SubjectIdx(idx))%Name// &
                !  ' which is concurrent pre-requisite to: '//Subject(concpreq)%Name
                fCredit = Failrate(crse,cTm1)*Subject(crse)%Units
                ! list as alternate; contribute failure rate to forecast
                CLExt(kdx)%Contrib = Failrate(crse,cTm1) ! CHANGE THIS LATER TO CONDITIONAL PROBABILITY
                NCurrent = NCurrent + 1
                CLExt(NPriority+NAlternates+NCurrent)%PriorityRank = kdx
                pLoad = pLoad + fCredit
                p(j) = 0 ! erase
                cycle
            end if

        end if
    end do

    !!
    !! add currently enrolled subjects as alternates
    !    do idx=1,CheckList%NSubjects
    !      crse = CheckList%SubjectIdx(idx)
    !      if (crse<=0) cycle ! subject not specified in PlanOfStudy
    !      if (CLExt(idx)%Grade==gdxREGD .and. (is_offered(crse,nextTerm) .or. Offering(crse)%NSections>0)) then
    !        CLExt(idx)%Contrib = Failrate(crse,cTm1) ! CHANGE THIS LATER TO CONDITIONAL PROBABILITY
    !        ! list as alternate; contribute failure rate to forecast
    !        NCurrent = NCurrent + 1
    !        CLExt(NPriority+NAlternates+NCurrent)%PriorityRank = idx
    !      end if
    !    end do

    if (NPriority /= 0) then ! there are subjects to register
        ! rlc - ! count missing grades for NF subjects
        ! rlc -       idx = 0
        ! rlc -       do i=1,min(NPriority,MAX_SUBJECTS_PER_TERM) ! max of 12 subjects
        ! rlc -         if (CheckList%SubjectTerm(ToRegister(i)) == 1) idx = idx+1 ! freshman subjects
        ! rlc -       end do
        ! rlc - ! groupings
        ! rlc -       if (UnitsEarned > SpecifiedUnits(1) .and. idx > 5) then
        ! rlc -         ! priority subjects are for new freshmen
        ! rlc -         Group = 8
        ! rlc -         NPriority = 0
        ! rlc -         NAlternates = 0
        ! special case for new freshmen
        ! rlc -       else if (UnitsFailed == 0 .and. UnitsEarned == 0 .and. &
        ! rlc -           (IncompletePOCW .or. CheckList%Code == 'BSN')) then
        if (stdScholastic == 1 .or. stdScholastic == 7) then   ! no failures, LOA
            Group = 3
        else if (stdScholastic == 2 .or. &  ! failed <= 25%
        stdScholastic == 3) then   ! warning
            Group = 4
        else if (stdScholastic == 4) then   ! probation
            Group = 5
            AllowedLoad = 15 ! max load for probation
        else if (stdScholastic == 5 .or. &  ! dismissed
        stdScholastic == 6) then   ! PD
            Group = 6
            AllowedLoad = 12 ! max load for dismissed, PD
        else
            Group = 3 ! Assume good standing
        end if

    else if (m /= 0) then
        ! none to register, and prerequisite of remaining subjects not satisfied
        !   pity the student!
        Group = 9
        NRemaining = m
        do i=1,m
            CLExt(i)%PriorityRank = q(i)
            CLExt(q(i))%Contrib = 0.0
        end do
    end if

    return
end subroutine analyze_checklist
