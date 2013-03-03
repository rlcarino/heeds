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

  use HTML

  implicit none

contains
  

  subroutine curriculum_list_all(device, fn)
    integer, intent (in) :: device, fn
    character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
    character(len=10) :: tStatus, tAction ! (ACTIVE)/(INACTIVE), Activate/Deactivate
    integer :: ierr, ldx, fnAction, ncurr
    character(len=80) :: mesg

    ! which program ?
    call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
    targetCurriculum = 0
    ! how many variants?
    ncurr = 0

    select case (fn)
      case (fnActivateCurriculum, fnDeactivateCurriculum)
            targetCurriculum = index_to_curriculum(tCurriculum)

      case default
        do ldx=1,NumCurricula
            if (CurrProgCode(ldx) /= tCurriculum) cycle
            targetCurriculum = ldx
            exit
        end do
        do ldx=1,NumCurricula
            if (CurrProgCode(ldx) /= tCurriculum) cycle
            ncurr = ncurr + 1
        end do

    end select
    if (ierr/=0 .or. targetCurriculum<=0) then
            write(device,AFORMAT) '<br>'//red//'Curriculum "'//tCurriculum//'" not found.'//black//'<br>'
            targetCollege = CollegeIdxUser
            return
    end if

    targetCollege = Curriculum(targetCurriculum)%CollegeIdx

    ! if ony one curriculum, display the curriculum
    if (ncurr==1) then
            call curriculum_display(device, targetCurriculum)
            return
    end if

    ! activate/deactivate
    select case (fn)
      case (fnActivateCurriculum)
            Curriculum(targetCurriculum)%Active = .true.
            mesg = 'Activated '//tCurriculum
            tCurriculum = CurrProgCode(targetCurriculum)
      case (fnDeactivateCurriculum)
            Curriculum(targetCurriculum)%Active = .false.
            mesg = 'Deactivated '//tCurriculum
            tCurriculum = CurrProgCode(targetCurriculum)
      case default
            mesg = SPACE
    end select
    if (mesg/=SPACE) call xml_write_curricula(pathToCurrent)

    call html_write_header(device, tCurriculum//' options', mesg)

    ! collect curricula 
    write(device,AFORMAT) '<ol>'
    do ldx=1,NumCurricula
      if (CurrProgCode(ldx) /= tCurriculum) cycle
      write(device,AFORMAT) trim(make_href(fnCurriculum, Curriculum(ldx)%Code, &
        A1=Curriculum(ldx)%Code, &
        pre='<li>', post=' - '//trim(Curriculum(ldx)%Title)//SPACE// &
        trim(Curriculum(ldx)%Specialization)//SPACE//trim(Curriculum(ldx)%Remark)))
      if (Curriculum(ldx)%Active) then
              tStatus = '(Active)'
              tAction = 'Deactivate'
              fnAction = fnDeactivateCurriculum
      else
              tStatus = '(Inactive)'
              tAction = 'Activate'
              fnAction = fnActivateCurriculum
      end if
      write(device,AFORMAT) nbsp//'<i> '//tStatus//'</i>'//nbsp
      if (isRoleAdmin) then
          write(device,AFORMAT) trim(make_href(fnAction, tAction, A1=Curriculum(ldx)%Code, &
              pre=nbsp//'<small>', post=nbsp))
          write(device,AFORMAT) trim(make_href(fnEditCurriculum, 'Edit', A1=Curriculum(ldx)%Code, &
              pre=nbsp, post='</small>'))
      end if
      write(device,AFORMAT) '</li>'
    end do
    write(device,AFORMAT) '</ol><hr>'

    return
  end subroutine curriculum_list_all


  subroutine curriculum_display(device, given)
    integer, intent(in), optional :: given
    integer, intent (in) :: device
    integer :: idx, tdx, m, n, cumulative, Year, Term, fnAction
    character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum, tAction
    character(len=10) :: tStatus ! (ACTIVE)/(INACTIVE)

    ! which curriculum
    if (present(given)) then
            targetCurriculum = given
            tdx = 0
    else
            call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, tdx)
            targetCurriculum = index_to_curriculum(tCurriculum)
    end if
    if (tdx/=0 .or. targetCurriculum<=0) then
            write(device,AFORMAT) '<br>'//red//'Curriculum "'//tCurriculum//'" not found.'//black//'<br>'
            targetCollege = CollegeIdxUser
            return
    end if
    targetCollege = Curriculum(targetCurriculum)%CollegeIdx

    call html_write_header(device, Curriculum(targetCurriculum)%Code)

    write(device,AFORMAT) '<b>'//trim(Curriculum(targetCurriculum)%Code)//' - '// &
      trim(Curriculum(targetCurriculum)%Title)//'</b>'
    if (len_trim(Curriculum(targetCurriculum)%Specialization) > 0) then
            write(device,AFORMAT) '<b> : '//trim(Curriculum(targetCurriculum)%Specialization)//'</b>'
    end if
    if (len_trim(Curriculum(targetCurriculum)%Remark) > 0) then
            write(device,AFORMAT) '<b> : '//trim(Curriculum(targetCurriculum)%Remark)//'</b>'
    end if
    if (Curriculum(targetCurriculum)%Active) then
            tStatus = '(Active)'
            tAction = 'Deactivate'
            fnAction = fnDeactivateCurriculum
    else
            tStatus = '(Inactive)'
            tAction = 'Activate'
            fnAction = fnActivateCurriculum
    end if
    write(device,AFORMAT) nbsp//'<i> '//tStatus//'</i>'//nbsp
    if (isRoleAdmin) then
        write(device,AFORMAT) trim(make_href(fnAction, tAction, A1=Curriculum(targetCurriculum)%Code, &
            pre=nbsp//'<small>', post=nbsp))
        write(device,AFORMAT) trim(make_href(fnEditCurriculum, 'Edit', A1=Curriculum(targetCurriculum)%Code, &
            pre=nbsp, post='</small>'))
    end if

    write(device,AFORMAT) '<br><table border="1" width="100%">'
    cumulative = 0
    do tdx=1,Curriculum(targetCurriculum)%NumTerms
      Year = tdx/3+1
      Term = mod(tdx,3)
      if (Term == 0) Year = Year-1
      m = 0
      n = 0
      do idx=1,Curriculum(targetCurriculum)%NSubjects
        if (Curriculum(targetCurriculum)%SubjectTerm(idx) == tdx) then
          m = m+1
          n = n+ Subject(Curriculum(targetCurriculum)%SubjectIdx(idx))%Units
        end if 
      end do
      cumulative = cumulative + n
      if (m > 0) then
        write(device,AFORMAT) begintr//'<td colspan="6">'//nbsp//endtd//endtr, &
          begintr//'<td colspan="6"><b>'//trim(Curriculum(targetCurriculum)%Code)//': '// &
          trim(txtYear(Year))//' Year, '// &
          trim(txtSemester(Term+3))//' Term ('//trim(itoa(n))//' units; '//trim(itoa(cumulative))//' cumulative)' &
          //'</b>'//endtd//endtr

        write(device,AFORMAT) begintr//tdnbspendtd//tdnbspendtd//tdnbspendtd//&
          begintd//'<b><i>Lect</i></b>'//endtd//begintd//'<b><i>Lab</i></b>'//endtd//tdnbspendtd//endtr
        write(device,AFORMAT) begintr//begintd//'<b><i>Subject</i></b>'//endtd//&
          begintd//'<b><i>Title</i></b>'//endtd//begintd//'<b><i>Units</i></b>'//endtd//&
          begintd//'<b><i>Hrs</i></b>'//endtd//begintd//'<b><i>Hrs</i></b>'//endtd//&
          begintd//'<b><i>Prerequisite</i></b>'//endtd//endtr
        do idx=1,Curriculum(targetCurriculum)%NSubjects
          if (Curriculum(targetCurriculum)%SubjectTerm(idx) /= tdx) cycle
          n = Curriculum(targetCurriculum)%SubjectIdx(idx)
          write(device,AFORMAT) begintr
          if (isRoleAdmin) then
            write(device,AFORMAT) trim(make_href(fnEditSubject, Subject(n)%Name, &
              A1=Subject(n)%Name, A2=College(targetCollege)%Code, pre=begintd, post=endtd))
          else
            write(device,AFORMAT) begintd//trim(Subject(n)%Name)//endtd
          end if
          write(device,AFORMAT) &
            begintd//trim(Subject(n)%Title)//endtd//&
            '<td align="center">'//trim(ftoa(Subject(n)%Units,1))//endtd//&
            '<td align="center">'//trim(ftoa(Subject(n)%LectHours,2))//endtd//&
            '<td align="center">'//trim(ftoa(Subject(n)%LabHours,2))//endtd//&
            '<td width="20%">'//trim(text_prerequisite_in_curriculum(n,Curriculum(targetCurriculum)))//endtd//&
            endtr
        end do
      end if
    end do
    write(device,AFORMAT) '</table><hr>'
    return
  end subroutine curriculum_display
   

  subroutine curriculum_edit(device, given)
    integer, intent(in), optional :: given
    integer, intent (in) :: device
    integer :: crse, i, j, k, ierr, idx, tdx, m, n, Year, Term, tUnits, ptrS
    character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum, tAction
    character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
    character(len=10) :: tStatus ! (ACTIVE)/(INACTIVE)

    character (len=255) :: mesg, remark
    type (TYPE_CURRICULUM) :: wrk
    logical :: changed
    integer, dimension(MAX_SECTION_MEETINGS) :: subjectList

    ! which curriculum
    if (present(given)) then
            targetCurriculum = given
            tdx = 0
    else
            call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, tdx)
            targetCurriculum = index_to_curriculum(tCurriculum)
    end if
    if (tdx/=0 .or. targetCurriculum<=0) then
            write(device,AFORMAT) '<br>'//red//'Curriculum "'//tCurriculum//'" not found.'//black//'<hr><br>'
            targetCollege = CollegeIdxUser
            return
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
                write(unitLOG,*) trim(remark)
            end if

            call cgi_get_named_string(QUERY_STRING, 'College', tCollege, ierr)
            wrk%CollegeIdx = index_to_college(tCollege)
            if (ierr/=0 .or. wrk%CollegeIdx<=0) wrk%CollegeIdx = Curriculum(targetCurriculum)%CollegeIdx
            if ( wrk%CollegeIdx /= Curriculum(targetCurriculum)%CollegeIdx) then
                changed = .true.
                remark = trim(remark)//': College changed to '//College(wrk%CollegeIdx)%Code
                write(unitLOG,*) trim(remark)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Title', wrk%Title, ierr)
            if (ierr/=0) wrk%Title = Curriculum(targetCurriculum)%Title
            if ( wrk%Title /= Curriculum(targetCurriculum)%Title) then
                changed = .true.
                remark = trim(remark)//': Title changed to '//wrk%Title
                write(unitLOG,*) trim(remark)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Specialization', wrk%Specialization, ierr)
            if (ierr/=0) wrk%Specialization = Curriculum(targetCurriculum)%Specialization
            if ( wrk%Specialization /= Curriculum(targetCurriculum)%Specialization) then
                changed = .true.
                remark = trim(remark)//': Specialization changed to '//wrk%Specialization
                write(unitLOG,*) trim(remark)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Remark', wrk%Remark, ierr)
            if (ierr/=0) wrk%Remark = Curriculum(targetCurriculum)%Remark
            if ( wrk%Remark /= Curriculum(targetCurriculum)%Remark) then
                changed = .true.
                remark = trim(remark)//': Remark changed to '//wrk%Remark
                write(unitLOG,*) trim(remark)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Status', tStatus, ierr)
            wrk%Active = tStatus=='Active'
            if (ierr/=0) wrk%Active = Curriculum(targetCurriculum)%Active
            if ( wrk%Active .neqv. Curriculum(targetCurriculum)%Active) then
                changed = .true.
                remark = trim(remark)//': Status changed to '//tStatus
                write(unitLOG,*) trim(remark)
            end if

            wrk%NumTerms = 0
            wrk%NSubjects = 0
            wrk%SubjectIdx = 0
            wrk%SubjectTerm = 0
            j = 0 ! number changed/reordered subjects
            do tdx=1,Curriculum(targetCurriculum)%NumTerms+6
              Year = tdx/3+1
              Term = mod(tdx,3)
              if (Term == 0) Year = Year-1
              call cgi_get_named_string(QUERY_STRING, 'Subjects'//trim(itoa(tdx)), mesg, ierr)

              if (len_trim(mesg)==0) then ! erase
                  do i=1,Curriculum(targetCurriculum)%NumTerms
                      if (wrk%SubjectTerm(i)==tdx) then ! term matches
                          j = j + 1
                          wrk%SubjectIdx(i) = 0
                          wrk%SubjectTerm(i) = 0
                      end if
                  end do
                  cycle
              end if

              call tokenize_subjects(mesg, COMMA, MAX_SECTION_MEETINGS, m, subjectList, ierr)
              if (ierr==0) then
                    write(unitLOG,*) 'TOKENIZE TERM: ierr=',ierr, ('; '//trim(Subject(subjectList(i))%Name),i=1,m)
                    do i=1,m
                        if (subjectList(i)==INDEX_TO_NONE) cycle
                        idx = wrk%NSubjects + i
                        wrk%SubjectIdx(idx) = subjectList(i)
                        wrk%SubjectTerm(idx) = tdx
                        if (wrk%SubjectIdx(idx)/=Curriculum(targetCurriculum)%SubjectIdx(idx) .or. &
                            wrk%SubjectTerm(idx)/=Curriculum(targetCurriculum)%SubjectTerm(idx)) then  
                          j = j + 1
                          write(unitLOG,*) j, ':', txtYear(Year), ', ', txtSemester(Term), ', ', Subject(subjectList(i))%Name
                        end if
                    end do
                    wrk%NSubjects = wrk%NSubjects + m
                    wrk%NumTerms = tdx
              end if
            end do
            if (j>0 .or. Curriculum(targetCurriculum)%NumTerms/=wrk%NumTerms) then
                    changed = .true.
                    remark = trim(remark)//': curriculum changed.'
                    write(unitLOG,*) trim(remark)
            end if

            ptrS = 0 ! non-zero later means a substitution rule was added
            call cgi_get_named_string(QUERY_STRING, 'Substitution', mesg, ierr)
            if (index(mesg,COMMA)>0 .and. ierr==0) then
              call tokenize_subjects(mesg, COMMA, MAX_SECTION_MEETINGS, m, subjectList, ierr)
              write(unitLOG,*) 'TOKENIZE SUBS: ierr=',ierr, ('; '//trim(Subject(subjectList(i))%Name),i=1,m)
              if (ierr==0) then
                      ptrS = SubstIdx(NumSubst+1)-1

                      NumSubst = NumSubst + 1
                      SubstIdx(NumSubst) = ptrS+1
                      Substitution(ptrS+1) = targetCurriculum
                      do i=1,m
                        Substitution(ptrS+1+i) = subjectList(i)
                      end do
                      ptrS = ptrS+m+1
                      
                      SubstIdx(NumSubst+1) = ptrS+1

                      changed = .true.
                      remark = trim(remark)//': New substitution rule'
                      write(unitLOG,*) trim(remark)
              end if
            end if

            if (changed) then
                    if ( wrk%Code /= Curriculum(targetCurriculum)%Code) then
                            ! add new ?
                            j = index_to_curriculum(wrk%Code)
                            if (j>0) then
                                    remark = 'Add new curriculum failed; "'//trim(wrk%Code)//'" already exists.'
                                    write(unitLOG,*) trim(remark)
                            else
                                    ! redirect global substitution rules before incrementing NumCurricula
                                    do i=1,NumSubst
                                      k = SubstIdx(i)
                                      if (Substitution(k) == NumCurricula+1) Substitution(k) = NumCurricula+2
                                    end do

                                    NumCurricula = NumCurricula+1
                                    Curriculum(NumCurricula) = wrk
                                    targetCurriculum = NumCurricula
                                    targetCollege = wrk%CollegeIdx
                                    tCurriculum = wrk%Code
                                    remark = 'Added new curriculum '//wrk%Code
                                    write(unitLOG,*) trim(remark)
                                    ! redirect new substitution rule
                                    if (ptrS>0) then
write(unitLOG,*) 'NumSubst=', NumSubst, ' SubstIdx(.)=', SubstIdx(NumSubst), &
  'Substitution(.)=', Substitution(SubstIdx(NumSubst))
                                            Substitution(SubstIdx(NumSubst)) = targetCurriculum
                                    end if
                                    call make_curriculum_groups()
                            end if
                    else
                            ! update existing
                            Curriculum(targetCurriculum) = wrk
                    end if
            end if

        case default

    end select

    if (changed) then
        call xml_write_curricula(pathToCurrent)
    end if

    call html_write_header(device, 'Edit curriculum '//tCurriculum, remark(3:))
    call make_form_start(device, fnEditCurriculum, tCurriculum)
    write(device,AFORMAT) '<table border="0" width="100%">', &
        begintr//begintd//'<b>Curriculum code</b>'//endtd//begintd//'<input name="Code" size="'// &
        trim(itoa(MAX_LEN_CURRICULUM_CODE))// &
        '" value="'//trim(tCurriculum)//'"> (A new curriculum will be created if this is changed)'//endtd//endtr

! type :: TYPE_CURRICULUM
!   logical :: Active
!   character (len=MAX_LEN_CURRICULUM_CODE) :: Code
!   character (len=MAX_LEN_CURRICULUM_NAME) :: Title
!   character (len=MAX_LEN_CURRICULUM_NAME) :: Specialization
!   character (len=MAX_LEN_CURRICULUM_NAME) :: Remark
!   integer :: CollegeIdx, NumTerms, NSubjects
!   integer, dimension(MAX_SUBJECTS_IN_CURRICULUM) :: SubjectIdx, SubjectTerm
! end type TYPE_CURRICULUM
    write(device,AFORMAT) &
      begintr//begintd//'<b>College</b>'//endtd//begintd//'<select name="College">'
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
      begintr//begintd//'<b>Title</b>'//endtd//begintd//'<input name="Title" size="'//trim(itoa(MAX_LEN_CURRICULUM_NAME))// &
        '" value="'//trim(Curriculum(targetCurriculum)%Title)//'">'//endtd//endtr, &
      begintr//begintd//'<b>Specialization</b>'//endtd//begintd//'<input name="Specialization" size="'// &
        trim(itoa(MAX_LEN_CURRICULUM_NAME))//'" value="'//trim(Curriculum(targetCurriculum)%Specialization)//'">'//endtd//endtr, &
      begintr//begintd//'<b>Remark</b>'//endtd//begintd//'<input name="Remark" size="'//trim(itoa(MAX_LEN_CURRICULUM_NAME))// &
        '" value="'//trim(Curriculum(targetCurriculum)%Remark)//'">'//endtd//endtr, &
      begintr//begintd//'<b>Status</b>'//endtd//begintd//trim(mesg)//endtd//endtr, &
      begintr//begintd//'<b>Year, Term (Units/Cumulative)</b>'//endtd// &
      begintd//'<b>Comma-separated subject codes</b>'//endtd//endtr

    tunits = 0
    do tdx=1,Curriculum(targetCurriculum)%NumTerms+6
      Year = tdx/3+1
      Term = mod(tdx,3)
      if (Term == 0) Year = Year-1
      m = 0
      n = 0
      mesg = SPACE
      do idx=1,Curriculum(targetCurriculum)%NSubjects
        if (Curriculum(targetCurriculum)%SubjectTerm(idx) == tdx) then
          crse = Curriculum(targetCurriculum)%SubjectIdx(idx)
          m = m+1
          n = n+ Subject(crse)%Units
          mesg = trim(mesg)//COMMA//SPACE//Subject(crse)%Name
        end if 
      end do
      tUnits = tUnits + n
      !if (Term==0) write(device,AFORMAT) begintr//'<td colspan="2">'//nbsp//endtd//endtr
      write(device,AFORMAT) begintr//begintd// &
        trim(txtYear(Year+9))//' Year, '//trim(txtSemester(Term+6))//' Term ('// &
        trim(itoa(n))//FSLASH//trim(itoa(tUnits))//')'//endtd, &
        begintd//'<input name="Subjects'//trim(itoa(tdx))//'" size="'//trim(itoa(MAX_LEN_CURRICULUM_NAME))// &
        '" value="'//trim(mesg(3:))//'">'//endtd//endtr
    end do

    write(device,AFORMAT) begintr//begintd//'<b>Substitution rules</b>'//endtd, &
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
    
    write(device,AFORMAT) '</table><br>'//nbsp//'<input name="action" type="submit" value="Update"></form><pre>', &
      !'NOTE: Subjects for a term are specified by COMMA-separated subjects codes.', &
      '</pre><hr>'

    return
  end subroutine curriculum_edit



end module EditCURRICULA

