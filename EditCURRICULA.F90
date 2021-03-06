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


module EditCURRICULA

    use DisplayCURRICULA

    implicit none

contains


    subroutine curriculum_edit(device, given)
        integer, intent(in), optional :: given
        integer, intent (in) :: device
        integer :: crse, i, j, k, ierr, idx, tdx, m, Year, Term, ptrS
        real :: tUnits, credit
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum, tAction
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=10) :: tStatus ! (ACTIVE)/(INACTIVE)

        character (len=255) :: mesg, remark, tokenizeErr
        type (TYPE_CURRICULUM) :: wrk
        logical :: changed, possibleImpact, critical1, critical2, critical3
        integer, dimension(MAX_SECTION_MEETINGS) :: subjectList

        ! which curriculum
        if (present(given)) then
            targetCurriculum = given
            tdx = 0
        else
            call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, tdx)
            targetCurriculum = index_to_curriculum(tCurriculum)
        end if

        wrk = Curriculum(targetCurriculum) ! make a working copy
        targetCollege = Curriculum(targetCurriculum)%CollegeIdx

        ! check for other arguments
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
        changed = .false.
        remark = SPACE

        select case (trim(tAction))

            case ('Update')
                call cgi_get_named_string(QUERY_STRING, 'Code', wrk%Code, ierr)
                if (ierr/=0) wrk%Code = Curriculum(targetCurriculum)%Code
                if ( wrk%Code /= Curriculum(targetCurriculum)%Code) then
                    changed = .true.
                    remark = trim(remark)//': Code changed to '//wrk%Code
                    call html_comment('Code changed to '//wrk%Code)
                end if

                call cgi_get_named_string(QUERY_STRING, 'College', tCollege, ierr)
                wrk%CollegeIdx = index_to_college(tCollege)

                if (ierr/=0 .or. wrk%CollegeIdx<=0) wrk%CollegeIdx = Curriculum(targetCurriculum)%CollegeIdx

                if ( wrk%CollegeIdx /= Curriculum(targetCurriculum)%CollegeIdx) then
                    changed = .true.
                    remark = trim(remark)//': College changed to '//College(wrk%CollegeIdx)%Code
                    call html_comment('College changed to '//College(wrk%CollegeIdx)%Code)
                end if

                call cgi_get_named_string(QUERY_STRING, 'Title', wrk%Title, ierr)

                if (ierr/=0) wrk%Title = Curriculum(targetCurriculum)%Title

                if ( wrk%Title /= Curriculum(targetCurriculum)%Title) then
                    changed = .true.
                    remark = trim(remark)//': Title changed to '//wrk%Title
                    call html_comment('Title changed to '//wrk%Title)
                end if

                call cgi_get_named_string(QUERY_STRING, 'Specialization', wrk%Specialization, ierr)

                if (ierr/=0) wrk%Specialization = Curriculum(targetCurriculum)%Specialization

                if ( wrk%Specialization /= Curriculum(targetCurriculum)%Specialization) then
                    changed = .true.
                    remark = trim(remark)//': Specialization changed to '//wrk%Specialization
                    call html_comment('Specialization changed to '//wrk%Specialization)
                end if

                call cgi_get_named_string(QUERY_STRING, 'Remark', wrk%Remark, ierr)

                if (ierr/=0) wrk%Remark = Curriculum(targetCurriculum)%Remark

                if ( wrk%Remark /= Curriculum(targetCurriculum)%Remark) then
                    changed = .true.
                    remark = trim(remark)//': Remark changed to '//wrk%Remark
                    call html_comment('Remark changed to '//wrk%Remark)
                end if

                call cgi_get_named_string(QUERY_STRING, 'Status', tStatus, ierr)
                wrk%Active = tStatus=='Active'

                if (ierr/=0) wrk%Active = Curriculum(targetCurriculum)%Active

                if ( wrk%Active .neqv. Curriculum(targetCurriculum)%Active) then
                    changed = .true.
                    remark = trim(remark)//': Status changed to '//tStatus
                    call html_comment('Status changed to '//tStatus)
                end if

                ! initialize list of subjects
                wrk%NumTerms = 0
                wrk%NSubjects = 0
                wrk%SubjectIdx = 0
                wrk%SubjectTerm = 0
                ! collect subjects
                do tdx=1,Curriculum(targetCurriculum)%NumTerms+6
                    call rank_to_year_term(tdx, Year, Term)
                    call cgi_get_named_string(QUERY_STRING, 'Subjects'//trim(itoa(tdx)), mesg, ierr)
                    if (len_trim(mesg)==0) cycle
                    call tokenize_subjects(mesg, COMMA, MAX_SECTION_MEETINGS, m, subjectList, ierr, tokenizeErr)
                    if (len_trim(tokenizeErr)>0) remark = trim(remark)//' : '//tokenizeErr
                    if (m>0) then
                        if (len_trim(tokenizeErr)>0) call html_comment('Token error: '//tokenizeErr)
                        do i=1,m
                            if (subjectList(i)==INDEX_TO_NONE) cycle
                            idx = wrk%NSubjects + i
                            wrk%SubjectIdx(idx) = subjectList(i)
                            wrk%SubjectTerm(idx) = tdx
                        end do
                        wrk%NSubjects = wrk%NSubjects + m
                        wrk%NumTerms = tdx
                    end if
                end do
                ! check deleted subjects from original curriculum
                do idx=1,Curriculum(targetCurriculum)%NSubjects
                    crse = Curriculum(targetCurriculum)%SubjectIdx(idx)
                    tdx = Curriculum(targetCurriculum)%SubjectTerm(idx)
                    possibleImpact = .false.
                    i = index_of_subject_in_curriculum (wrk, crse)
                    if (i==0) then ! crse not in wrk, deleted from targetCurriculum
                        changed = .true.
                        possibleImpact = .true.
                        remark = trim(remark)//': Deleted '//Subject(crse)%Name
                        call html_comment('>>> Deleted '//Subject(crse)%Name)
                    else ! crse retained; check if moved to another term
                        if (tdx/=wrk%SubjectTerm(i)) then ! but moved to a different semester
                            changed = .true.
                            possibleImpact = .true.
                            remark = trim(remark)//': Moved '//Subject(crse)%Name
                            call html_comment('>>> Moved '//Subject(crse)%Name)
                        end if
                    end if
                    !if (possibleImpact) then ! check if crse is used in a block section
                    !    call rank_to_year_term(tdx, Year, Term)
                    !    do i=1,NumBlocks
                    !        if (Block(i)%CurriculumIdx==targetCurriculum .and. &
                    !            Block(i)%Year==Year .and. Block(i)%Term==Term) then
                    !            remark = trim(remark)//', affects '//Block(i)%BlockID
                    !            call html_comment('>>> Change in '//Subject(crse)%Name//'may affect '//Block(i)%Name)
                    !        end if
                    !    end do
                    !end if
                end do

                ! check for additional subjects to original curriculum
                do idx=1,wrk%NSubjects
                    crse = wrk%SubjectIdx(idx)
                    tdx = wrk%SubjectTerm(idx)
                    i = index_of_subject_in_curriculum (Curriculum(targetCurriculum), crse)
                    if (i==0) then ! crse added to targetCurriculum
                        changed = .true.
                        remark = trim(remark)//': Added '//Subject(crse)%Name
                        call html_comment('>>> Added '//Subject(crse)%Name)
                        !call rank_to_year_term(tdx, Year, Term)
                        !do i=1,NumBlocks
                        !    if (Block(i)%CurriculumIdx==targetCurriculum .and. &
                        !        Block(i)%Year==Year .and. Block(i)%Term==Term) then
                        !        remark = trim(remark)//', affects '//Block(i)%BlockID
                        !        call html_comment('>>> Addition of '//Subject(crse)%Name//' affects '//Block(i)%Name)
                        !    end if
                        !end do
                    end if
                end do

                ptrS = 0 ! non-zero later means a substitution rule was added
                call cgi_get_named_string(QUERY_STRING, 'Substitution', mesg, ierr)

                if (index(mesg,COMMA)>0 .and. ierr==0) then

                    call tokenize_subjects(mesg, COMMA, MAX_SECTION_MEETINGS, m, subjectList, ierr)
                    if (ierr==0) then

                        call check_array_bound (NumSubst+1, MAX_ALL_SUBSTITUTIONS, 'MAX_ALL_SUBSTITUTIONS', critical1)
                        call check_array_bound (ptrS+m, MAX_LEN_SUBSTITUTION_ARRAY, 'MAX_LEN_SUBSTITUTION_ARRAY', critical2)

                        if (critical1 .or. critical2) then
                            changed = .false.
                            remark = trim(remark)//': No more space for substitution rule'
                            call html_comment('No more space for substitution rule '//mesg)

                        else
                            ! delete rule if NONE is in subjectList
                            j = 0
                            do i=1,m
                                if (subjectList(i)==INDEX_TO_NONE) j = i
                            end do
                            if (j==0) then ! add rule
                                ptrS = SubstIdx(NumSubst+1)-1

                                NumSubst = NumSubst + 1
                                ptrS = ptrS+1
                                SubstIdx(NumSubst) = ptrS
                                Substitution(ptrS) = targetCurriculum
                                do i=1,m
                                    ptrS = ptrS+1
                                    Substitution(ptrS) = subjectList(i)
                                end do

                                SubstIdx(NumSubst+1) = ptrS+1

                                changed = .true.
                                remark = trim(remark)//': New substitution rule'
                                call html_comment('New substitution rule '//mesg)
                            else ! delete rule
                                do k=1,NumSubst
                                    if (Substitution(SubstIdx(k))/=targetCurriculum) cycle ! not this curriculum
                                    do j=SubstIdx(k)+1, SubstIdx(k+1)-1
                                        if (subjectList(1)==Substitution(j)) then ! matched
                                            Substitution(SubstIdx(k)) = 0 ! invalidate pointer to curriculum
                                            exit
                                        end if
                                    end do
                                end do
                                changed = .true.
                                remark = trim(remark)//': Removed substitution rule'
                                call html_comment('Removed substitution rule '//mesg)

                            end if
                        end if
                    end if
                end if

                if (changed) then
                    if ( wrk%Code /= Curriculum(targetCurriculum)%Code) then
                        ! add new ?
                        j = index_to_curriculum(wrk%Code)
                        if (j>0) then
                            remark = ' : Add new curriculum failed; "'//trim(wrk%Code)//'" already exists.'
                            call html_comment(remark)

                        else
                            call check_array_bound (NumCurricula+1, MAX_ALL_CURRICULA, 'MAX_ALL_CURRICULA', critical3)

                            if (critical3) then
                                changed = .false.
                                remark = ' : No more space for another curriculum'
                                call html_comment(remark)

                            else

                                ! add new curriculum
                                Curriculum(NumCurricula+1) = Curriculum(NumCurricula)
                                Curriculum(NumCurricula) = wrk
                                targetCurriculum = NumCurricula

                                NumCurricula = NumCurricula+1
                                targetCollege = wrk%CollegeIdx
                                tCurriculum = wrk%Code
                                remark = ' : Added new curriculum '//wrk%Code
                                call html_comment(remark)
                                ! redirect new substitution rule
                                if (ptrS>0) then
                                    Substitution(SubstIdx(NumSubst)) = targetCurriculum
                                end if
                                call make_curriculum_groups()
                            end if
                        end if
                    else
                        ! update existing
                        Curriculum(targetCurriculum) = wrk
                        targetCollege = wrk%CollegeIdx
                    end if
                end if

            case default

        end select

        if (changed) then
            call xml_write_curricula(trim(pathToYear)//'CURRICULA.XML')
        end if

        call html_write_header(device, 'Edit curriculum '//tCurriculum, remark(3:))
        write(device,AFORMAT) trim(make_href(fnCurriculumList, CurrProgCode(targetCurriculum), &
            A1=CurrProgCode(targetCurriculum), &
            pre=beginsmall//'Edit other'//nbsp, post=' option'//endsmall))//linebreak

        call make_form_start(device, fnEditCurriculum, tCurriculum)
        write(device,AFORMAT) '<table border="0" width="100%">', &
            begintr//begintd//beginbold//'Curriculum code'//endbold//endtd//begintd//'<input name="Code" size="'// &
            trim(itoa(MAX_LEN_CURRICULUM_CODE))// &
            '" value="'//trim(tCurriculum)//'"> (A new curriculum will be created if this is changed)'//endtd//endtr

        write(device,AFORMAT) &
            begintr//begintd//beginbold//'College'//endbold//endtd//begintd//'<select name="College">'
        do i=1,NumColleges
            if (i/=targetCollege) then
                j=0
            else
                j=1
            end if
            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(College(i)%Code)//'">'// &
                trim(College(i)%Name)
        end do

        if (Curriculum(targetCurriculum)%Active) then
            mesg = '<input type="radio" name="Status" value="Active" checked="yes"> Active '//nbsp// &
            '<input type="radio" name="Status" value="Inactive"> Inactive'
        else
            mesg = '<input type="radio" name="Status" value="Active"> Active '//nbsp// &
            '<input type="radio" name="Status" value="Inactive" checked="yes"> Inactive'
        end if

        write(device,AFORMAT) '</select>'//endtd//endtr, &
            begintr//begintd//beginbold//'Title'//endbold//endtd//begintd//'<input name="Title" size="'//trim(itoa(MAX_LEN_CURRICULUM_NAME))//&
            '" value="'//trim(Curriculum(targetCurriculum)%Title)//'">'//endtd//endtr, &
            begintr//begintd//beginbold//'Specialization'//endbold//endtd//begintd//'<input name="Specialization" size="'// &
            trim(itoa(MAX_LEN_CURRICULUM_NAME))//'" value="'//trim(Curriculum(targetCurriculum)%Specialization)// &
            '">'//endtd//endtr, &
            begintr//begintd//beginbold//'Remark'//endbold//endtd// &
            begintd//'<input name="Remark" size="'//trim(itoa(MAX_LEN_CURRICULUM_NAME))// &
            '" value="'//trim(Curriculum(targetCurriculum)%Remark)//'">'//endtd//endtr, &
            begintr//begintd//beginbold//'Status'//endbold//endtd//begintd//trim(mesg)//endtd//endtr, &
            begintr//begintd//beginbold//'Year, Term (Units/Cumulative)'//endbold//endtd// &
            begintd//beginbold//'Comma-separated subject codes'//endbold//endtd//endtr

        tunits = 0.0

        do tdx=1,Curriculum(targetCurriculum)%NumTerms+6

            call rank_to_year_term(tdx, Year, Term)

            m = 0
            credit = 0
            mesg = SPACE

            do idx=1,Curriculum(targetCurriculum)%NSubjects
                if (Curriculum(targetCurriculum)%SubjectTerm(idx) == tdx) then
                    crse = Curriculum(targetCurriculum)%SubjectIdx(idx)
                    m = m+1
                    credit = credit + Subject(crse)%Units
                    mesg = trim(mesg)//COMMA//SPACE//Subject(crse)%Name
                end if
            end do

            tUnits = tUnits + credit

            write(device,AFORMAT) begintr//begintd// &
                trim(txtYear(Year+10))//' Year, '//trim(txtSemester(Term+6))//' Term ('// &
                trim(ftoa(credit,1))//FSLASH//trim(ftoa(tUnits,1))//')'//endtd, &
                begintd//'<input name="Subjects'//trim(itoa(tdx))//'" size="'//trim(itoa(MAX_LEN_CURRICULUM_NAME))// &
                '" value="'//trim(mesg(3:))//'">'//endtd//endtr
        end do

        write(device,AFORMAT) begintr//begintd//beginbold//'Substitution rules'//endbold//endtd, &
            begintd//'Required subjects in list will be PASSED if credits have been earned '// &
            'for the other subjects in the list'//endtd//endtr

        do tdx=1,NumSubst
            if (Substitution(SubstIdx(tdx))==targetCurriculum) then
                mesg = SPACE
                do j=SubstIdx(tdx)+1, SubstIdx(tdx+1)-1
                    mesg = trim(mesg)//COMMA//SPACE//Subject(Substitution(j))%Name
                end do
                write(device,AFORMAT) begintr//begintd//'SUBSTITUTION'//endtd//begintd//trim(mesg(3:))//endtd//endtr
            end if
        end do

        write(device,AFORMAT) begintr//begintd//'SUBSTITUTION'//endtd// &
            begintd//'<input name="Substitution" size="'//trim(itoa(MAX_LEN_CURRICULUM_NAME))// &
            '" value="(Enter new substitution rule)">'//endtd//endtr

        write(device,AFORMAT) endtable//linebreak//nbsp//'<input name="action" type="submit" value="Update">'//endform//'<pre>', &
            '</pre>'//horizontal

    end subroutine curriculum_edit


    subroutine equivalencies_edit(device)
        integer, intent (in) :: device
        integer :: idx, tdx, ierr, crse, ptrS, i, m!, j, k, Year, Term
        character (len=255) :: mesg, remark!, tokenizeErr
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        logical :: changed, critical1, critical2!, possibleImpact, critical3
        integer, dimension(MAX_SECTION_MEETINGS) :: subjectList

        changed = .false.
        remark = SPACE

        ! any deleted rules?
        do tdx=1,NumSubst
            if (Substitution(SubstIdx(tdx))==-1) then
                call cgi_get_named_string(QUERY_STRING, 'del'//trim(itoa(tdx)), tSubject, ierr)
                if (ierr/=0) cycle ! rule not found
                ! delete rule
                mesg = SPACE
                do idx=SubstIdx(tdx)+1, SubstIdx(tdx+1)-1
                    mesg = trim(mesg)//COMMA//Subject(Substitution(idx))%Name
                end do
                Substitution(SubstIdx(tdx)) = 0
                remark = trim(remark)//': Deleted rule '//mesg(2:)
                changed = .true.
            end if
        end do

        ! a rule was added?
        call cgi_get_named_string(QUERY_STRING, 'Required', tSubject, ierr)
        if (len_trim(tSubject)>0) then ! new rule entered?
            crse = index_to_subject(tSubject)
            if (crse>0) then ! required subject is OK
                call cgi_get_named_string(QUERY_STRING, 'Equivalent', mesg, ierr)

                if (ierr==0) then  ! Equivalent list is not empty

                    ! prepend Required
                    mesg = trim(tSubject)//COMMA//mesg
                    call tokenize_subjects(mesg, COMMA, MAX_SECTION_MEETINGS, m, subjectList, ierr)
                    if (ierr==0) then ! Equivalent list is well-formed

                        ptrS = SubstIdx(NumSubst+1)-1

                        call check_array_bound (NumSubst+1, MAX_ALL_SUBSTITUTIONS, 'MAX_ALL_SUBSTITUTIONS', critical1)
                        call check_array_bound (ptrS+m, MAX_LEN_SUBSTITUTION_ARRAY, 'MAX_LEN_SUBSTITUTION_ARRAY', critical2)

                        if (critical1 .or. critical2) then
                            changed = .false.
                            remark = trim(remark)//': No more space for substitution rule'
                            call html_comment('No more space for substitution rule '//mesg, &
                                'Limit MAX_ALL_SUBSTITUTIONS= '//itoa(MAX_ALL_SUBSTITUTIONS)// &
                                '; currently used is '//itoa(NumSubst), &
                                'Limit MAX_LEN_SUBSTITUTION_ARRAY= '//itoa(MAX_LEN_SUBSTITUTION_ARRAY)// &
                                '; currently used is '//itoa(ptrS) )

                        else

                            NumSubst = NumSubst + 1
                            ptrS = ptrS+1
                            SubstIdx(NumSubst) = ptrS
                            Substitution(ptrS) = -1
                            do i=1,m
                                ptrS = ptrS+1
                                Substitution(ptrS) = subjectList(i)
                            end do

                            SubstIdx(NumSubst+1) = ptrS+1

                            changed = .true.
                            remark = trim(remark)//': New equivalence rule '//mesg
                            call html_comment('New equivalence rule '//mesg)
                        end if
                    end if
                end if

            end if

        end if

        if (changed) then
            call xml_write_equivalencies(trim(pathToYear)//'EQUIVALENCIES.XML')
        else
            call cgi_get_named_string(QUERY_STRING, 'action', tSubject, ierr)
            if (ierr==0) remark = ' : No changes to equivalence rules?'
        end if

        call html_write_header(device, 'Equivalence rules applicable to all curricular programs', remark(3:))
        write(device,AFORMAT) 'Required subject will be PASSED if credits have been earned '// &
            'for Equivalent(s). Checked rules will be deleted.'//linebreak//linebreak

        call make_form_start(device, fnEditEquivalencies)
        write(device,AFORMAT) '<table border="0" width="50%">', begintr// &
            thalignleft//'Required'//endth// &
            thalignleft//'Equivalent(s)'//endth// &
            thalignleft//'Delete?'//endth//endtr
        do tdx=1,NumSubst
            if (Substitution(SubstIdx(tdx))==-1) then
                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(tdx,2))//'">'// &
                    begintd//trim(Subject(Substitution(SubstIdx(tdx)+1))%Name)//endtd
                mesg = SPACE
                do idx=SubstIdx(tdx)+2, SubstIdx(tdx+1)-1
                    mesg = trim(mesg)//COMMA//Subject(Substitution(idx))%Name
                end do
                write(device,AFORMAT) begintd//trim(mesg(2:))//endtd// &
                    begintd//'<input type="checkbox" name="del'//trim(itoa(tdx))//'">'//endtd//endtr
            end if
        end do

        write(device,AFORMAT) begintr// &
            begintd//'<input name="Required" size="'//trim(itoa(MAX_LEN_SUBJECT_CODE))// &
            '" value="">'//endtd// &
            begintd//'<input name="Equivalent" size="'//trim(itoa(MAX_LEN_SUBJECT_CODE))// &
            '" value="">'//endtd//begintd//beginbold//'Add rule'//endbold//endtd//endtr

        write(device,AFORMAT) endtable//linebreak//nbsp//'<input name="action" type="submit" value="Update">'//endform//'<pre>', &
            '</pre>'//horizontal

    end subroutine equivalencies_edit

end module EditCURRICULA

