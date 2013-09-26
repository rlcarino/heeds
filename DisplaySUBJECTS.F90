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


module DisplaySUBJECTS

    use HTML

    implicit none

contains

    subroutine subject_list_all (device)
        integer, intent (in) :: device
        integer :: crse, idx, nSubjects, ierr, ncol, maxcol=7
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject

        ! which department ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)
        targetDepartment = index_to_dept(tDepartment)
        if (targetDepartment>0) then
            targetCollege = Department(targetDepartment)%CollegeIdx
            nSubjects = 0
#if defined REGIST
            do idx=1,NumSubjects+NumAdditionalSubjects
                if (Subject(idx)%DeptIdx == targetDepartment) then
                    nSubjects = nSubjects+1
                    tArray(nSubjects) = idx
                end if
            end do
#else
            ! Subjects administered by program
            do idx=1,NumSubjects+NumAdditionalSubjects
                if (is_used_in_college_subject(targetCollege, idx) ) then
                    nSubjects = nSubjects+1
                    tArray(nSubjects) = idx
                end if
            end do
#endif
            call html_write_header(device, 'Subjects in '//Department(targetDepartment)%Name)
        else ! try subject area
            if (ierr/=0 .or. tDepartment==SPACE) then
                targetDepartment = DeptIdxUser
                targetCollege = CollegeIdxUser
                call html_write_header(device, 'List of subjects', '<hr><br>Department or subject area not found')
                return
            else
                ! make list of subjects in subject area to display
                targetDepartment = DeptIdxUser
                nSubjects = 0
                do idx=1,NumSubjects+NumAdditionalSubjects
                    if (index(Subject(idx)%Name,trim(tDepartment)//SPACE)==1) then
                        nSubjects = nSubjects + 1
                        !write(*,*) nSubjects, Subject(idx)%Name
                        tArray(nSubjects) = idx
                        targetDepartment = Subject(idx)%DeptIdx
                    end if
                end do
                call html_write_header(device, '"'//trim(tDepartment)//'" subjects')
            end if
        end if

        if (nSubjects>0) then

            ! write shortcut to subjects
            write(device,AFORMAT) '<table border="0" width="100%">'
            ncol = 0
            do idx=1,nSubjects
                tSubject = Subject(tArray(idx))%Name
                ncol = ncol + 1
                if (ncol == 1) then
                    write(device,AFORMAT) begintr//begintd//'<a href="#'//trim(tSubject)//'">'//trim(tSubject)//'</a>'//endtd
                else if (ncol == maxcol) then
                    write(device,AFORMAT) begintd//'<a href="#'//trim(tSubject)//'">'//trim(tSubject)//'</a>'//endtd//endtr
                    ncol = 0
                else
                    write(device,AFORMAT) begintd//'<a href="#'//trim(tSubject)//'">'//trim(tSubject)//'</a>'//endtd
                end if
            end do
            if (ncol /= 0)  then
                do idx=ncol+1,maxcol
                    write(device,AFORMAT) tdnbspendtd
                end do
                write(device,AFORMAT) endtr
            end if
            write(device,AFORMAT) '</table><hr><br>'

            write(device,AFORMAT) '<table border="0" cellspacing="0" width="100%"><small>'
            do idx=1,nSubjects
                crse = tArray(idx)
                tSubject = Subject(crse)%Name

                if (mod(idx,6)==1) write(device,AFORMAT) begintr//thalignleft//'Subject'//endth// &
                    beginth//'Units'//endth//beginth//'Term'//endth//beginth//'Tuition'//endth//beginth//'Lab fee'//endth// &
                    beginth//'Lect hrs'//endth//beginth//'Lect load'//endth//beginth//'Min Size'//endth// &
                    beginth//'Max Size'//endth,  &
                    beginth//'Lab hrs'//endth//beginth//'Lab load'//endth//beginth//'Min Size'//endth// &
                    beginth//'Max Size'//endth//endtr, &
                    begintr//'<td colspan="11">'//nbsp//endtd//endtr

                write(device,AFORMAT) begintr//begintd//'<a name="'//trim(tSubject)//'"><b>Name:</b> '
                if (isRoleAdmin) then
                    write(device,AFORMAT) trim(make_href(fnEditSubject, tSubject, A1=tSubject))
                else
                    write(device,AFORMAT) trim(tSubject)
                end if
                write(device,AFORMAT) '</a>'//endtd, &
                    tdaligncenter//trim(ftoa(Subject(crse)%Units,1))//endtd// &
                    tdaligncenter//trim(text_term_offered(Subject(crse)%TermOffered))//endtd// &
                    tdaligncenter//trim(ftoa(Subject(crse)%Tuition,2))//endtd// &
                    tdaligncenter//trim(ftoa(Subject(crse)%LabFee,2))//endtd// &
                    tdaligncenter//trim(ftoa(Subject(crse)%LectHours,2))//endtd// &
                    tdaligncenter//trim(ftoa(Subject(crse)%LectLoad,2))//endtd// &
                    tdaligncenter//trim(itoa(Subject(crse)%MinLectSize))//endtd// &
                    tdaligncenter//trim(itoa(Subject(crse)%MaxLectSize))//endtd,  &
                    tdaligncenter//trim(ftoa(Subject(crse)%LabHours,2))//endtd// &
                    tdaligncenter//trim(ftoa(Subject(crse)%LabLoad,2))//endtd// &
                    tdaligncenter//trim(itoa(Subject(crse)%MinLabSize))//endtd// &
                    tdaligncenter//trim(itoa(Subject(crse)%MaxLabSize))//endtd//endtr

                write(device,AFORMAT) &
                    begintr//'<td colspan="13"><b>Title:</b> '//trim(Subject(crse)%Title)//endtd//endtr, &
                    begintr//'<td colspan="13"><b>Preq.</b> '//trim(text_prerequisite_of_subject(crse,0))//endtd//endtr, &
                    begintr//'<td colspan="13">'//nbsp//endtd//endtr
            end do

            write(device,AFORMAT) '</small></table>'

        else
            write(device,AFORMAT) '<br>No subjects in this college?'
        end if
        write(device,AFORMAT) '<hr>'

    end subroutine subject_list_all


end module DisplaySUBJECTS
