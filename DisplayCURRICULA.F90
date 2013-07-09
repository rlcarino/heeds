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


module DisplayCURRICULA

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
                do ldx=1,NumCurricula-1
                    if (CurrProgCode(ldx) /= tCurriculum) cycle
                    targetCurriculum = ldx
                    exit
                end do
                do ldx=1,NumCurricula-1
                    if (CurrProgCode(ldx) /= tCurriculum) cycle
                    ncurr = ncurr + 1
                end do

        end select
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

        if (mesg/=SPACE) call xml_write_curricula(trim(pathToYear)//'CURRICULA.XML')

        call html_write_header(device, tCurriculum//' options', mesg)

        ! collect curricula
        write(device,AFORMAT) '<ol>'
        do ldx=1,NumCurricula-1

            if (CurrProgCode(ldx) /= tCurriculum) cycle

            write(device,AFORMAT) trim(make_href(fnCurriculum, Curriculum(ldx)%Code, &
                A1=Curriculum(ldx)%Code, &
                pre='<li>', post=' - '//trim(Curriculum(ldx)%Title)))
            if (trim(Curriculum(ldx)%Specialization)/=SPACE) &
                write(device,AFORMAT) ' : '//trim(Curriculum(ldx)%Specialization)
            if (trim(Curriculum(ldx)%Remark)/=SPACE) &
                write(device,AFORMAT) ' : '//trim(Curriculum(ldx)%Remark)

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
                    pre=nbsp//'<small>', post=nbsp, alt=SPACE))
                write(device,AFORMAT) trim(make_href(fnEditCurriculum, 'Edit', A1=Curriculum(ldx)%Code, &
                    pre=nbsp, post='</small>', alt=SPACE))
            end if

            write(device,AFORMAT) '</li>'

        end do
        write(device,AFORMAT) '</ol><hr>'

    end subroutine curriculum_list_all


    subroutine curriculum_display(device, given)
        integer, intent(in), optional :: given
        integer, intent (in) :: device
        integer :: idx, tdx, m, n, Year, Term, fnAction
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum, tAction
        character(len=10) :: tStatus ! (ACTIVE)/(INACTIVE)
        real :: tUnits, cumulative

        ! which curriculum
        if (present(given)) then
            targetCurriculum = given
            tdx = 0
        else
            call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, tdx)
            targetCurriculum = index_to_curriculum(tCurriculum)
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
                pre=nbsp//'<small>', post=nbsp, alt=SPACE))
            write(device,AFORMAT) trim(make_href(fnEditCurriculum, 'Edit', A1=Curriculum(targetCurriculum)%Code, &
                pre=nbsp, post='</small>', alt=SPACE))
        end if

        write(device,AFORMAT) '<br>Note: A '//red//'SUBJECT'//black//' in column <b><i>Prerequisite</i></b> ', &
            ' indicates an inconsistency. Said '//red//'SUBJECT'//black// &
            ' is not present in the curriculum, or is not taken in a prior term, ', &
            ' or the prerequisite expression should be "SUBJECT1 OR SUBJECT2" where either one is taken in a prior term.', &
            '<br><table border="1" width="100%">'
        cumulative = 0.0
        do tdx=1,Curriculum(targetCurriculum)%NumTerms

            call rank_to_year_term(tdx, Year, Term)
            m = 0
            tUnits = 0.0

            do idx=1,Curriculum(targetCurriculum)%NSubjects
                if (Curriculum(targetCurriculum)%SubjectTerm(idx) == tdx) then
                    m = m+1
                    tUnits = tUnits + Subject(Curriculum(targetCurriculum)%SubjectIdx(idx))%Units
                end if
            end do

            cumulative = cumulative + tUnits

            if (m > 0) then
                write(device,AFORMAT) begintr//'<td colspan="6">'//nbsp//endtd//endtr, &
                    begintr//'<td colspan="6"><b>'//trim(Curriculum(targetCurriculum)%Code)//': '// &
                    trim(txtYear(Year))//' Year, '// &
                    trim(txtSemester(Term+3))//' Term ('//trim(ftoa(tUnits,1))//' units; '// &
                    trim(ftoa(cumulative,1))//' cumulative)' &
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

    end subroutine curriculum_display


end module DisplayCURRICULA

