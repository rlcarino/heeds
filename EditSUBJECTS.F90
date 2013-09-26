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


module EditSUBJECTS

    use DisplaySUBJECTS

    implicit none

contains


    subroutine subject_edit(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject, tAction, token
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer :: ierr, crse, i, j
        character (len=255) :: mesg, remark
        type (TYPE_SUBJECT) :: wrk
        logical :: isDirtySUBJECTS, criticalErr

        call html_comment('subject_edit()')

        ! which subject ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tSubject, crse)
        crse = index_to_subject(tSubject)
        wrk = Subject(crse) ! make a working copy

        ! check for other arguments
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
        isDirtySUBJECTS = .false.
        remark = SPACE

        select case (trim(tAction))

            case ('Update')

                call cgi_get_named_float(QUERY_STRING, 'Units', wrk%Units, ierr)
                if (ierr/=0) wrk%Units = Subject(crse)%Units
                if ( wrk%Units /= Subject(crse)%Units) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Units changed to '//ftoa(wrk%Units,1)
                end if

                call cgi_get_named_float(QUERY_STRING, 'Tuition', wrk%Tuition, ierr)
                if (ierr/=0) wrk%Tuition = Subject(crse)%Tuition
                if ( wrk%Tuition /= Subject(crse)%Tuition) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Tuition fee changed to '//ftoa(wrk%Tuition,2)
                end if

                call cgi_get_named_float(QUERY_STRING, 'LabFee', wrk%LabFee, ierr)
                if (ierr/=0) wrk%LabFee = Subject(crse)%LabFee
                if ( wrk%LabFee /= Subject(crse)%LabFee) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Lab fee changed to '//ftoa(wrk%LabFee,2)
                end if

                call cgi_get_named_float(QUERY_STRING, 'LectHours', wrk%LectHours, ierr)
                if (ierr/=0) wrk%LectHours = Subject(crse)%LectHours
                if ( wrk%LectHours /= Subject(crse)%LectHours) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Lecture Hours changed to '//ftoa(wrk%LectHours,2)
                end if

                call cgi_get_named_float(QUERY_STRING, 'LectLoad', wrk%LectLoad, ierr)
                if (ierr/=0) wrk%LectLoad = Subject(crse)%LectLoad
                if ( wrk%LectLoad /= Subject(crse)%LectLoad) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Lecture workload changed to '//ftoa(wrk%LectLoad,2)
                end if

                call cgi_get_named_integer(QUERY_STRING, 'MinLectSize', wrk%MinLectSize, ierr)
                if (ierr/=0) wrk%MinLectSize = Subject(crse)%MinLectSize
                if ( wrk%MinLectSize /= Subject(crse)%MinLectSize) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': MinLectSize changed to '//itoa(wrk%MinLectSize)
                end if

                call cgi_get_named_integer(QUERY_STRING, 'MaxLectSize', wrk%MaxLectSize, ierr)
                if (ierr/=0) wrk%MaxLectSize = Subject(crse)%MaxLectSize
                if ( wrk%MaxLectSize /= Subject(crse)%MaxLectSize) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': MaxLectSize changed to '//itoa(wrk%MaxLectSize)
                end if

                call cgi_get_named_float(QUERY_STRING, 'LabHours', wrk%LabHours, ierr)
                if (ierr/=0) wrk%LabHours = Subject(crse)%LabHours
                if ( wrk%LabHours /= Subject(crse)%LabHours) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': LabHours changed to '//ftoa(wrk%LabHours,2)
                end if

                call cgi_get_named_float(QUERY_STRING, 'LabLoad', wrk%LabLoad, ierr)
                if (ierr/=0) wrk%LabLoad = Subject(crse)%LabLoad
                if ( wrk%LabLoad /= Subject(crse)%LabLoad) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Lab workload changed to '//ftoa(wrk%LabLoad,2)
                end if

                call cgi_get_named_integer(QUERY_STRING, 'MinLabSize', wrk%MinLabSize, ierr)
                if (ierr/=0) wrk%MinLabSize = Subject(crse)%MinLabSize
                if ( wrk%MinLabSize /= Subject(crse)%MinLabSize) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': MinLabSize changed to '//itoa(wrk%MinLabSize)
                end if

                call cgi_get_named_integer(QUERY_STRING, 'MaxLabSize', wrk%MaxLabSize, ierr)
                if (ierr/=0) wrk%MaxLabSize = Subject(crse)%MaxLabSize
                if ( wrk%MaxLabSize /= Subject(crse)%MaxLabSize) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': MaxLabSize changed to '//itoa(wrk%MaxLabSize)
                end if

                call cgi_get_named_string(QUERY_STRING, 'Name', mesg, ierr)
                wrk%Name  = trim(mesg)
                if (ierr/=0) wrk%Name = Subject(crse)%Name
                if ( wrk%Name /= Subject(crse)%Name) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Name changed to '//wrk%Name
                end if

                call cgi_get_named_string(QUERY_STRING, 'Title', mesg, ierr)
                wrk%Title = trim(mesg)
                if (ierr/=0) wrk%Title = Subject(crse)%Title
                if ( wrk%Title /= Subject(crse)%Title) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Title changed to '//wrk%Title
                end if

                call cgi_get_named_string(QUERY_STRING, 'Department', tDepartment, ierr)
                wrk%DeptIdx = index_to_dept(tDepartment)
                if (ierr/=0 .or. wrk%DeptIdx<=0) wrk%DeptIdx = Subject(crse)%DeptIdx
                if ( wrk%DeptIdx /= Subject(crse)%DeptIdx) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': Department changed to '//Department(wrk%DeptIdx)%Code
                end if

                call cgi_get_named_string(QUERY_STRING, 'TermOffered', token, ierr)
                j = 0
                if (index(token, '1')>0 ) j = j+1
                if (index(token, '2')>0 ) j = j+2
                if (index(token, 'S')>0 ) j = j+4
                wrk% TermOffered= j
                if (ierr/=0 .or. j==0) wrk%TermOffered = Subject(crse)%TermOffered
                if ( wrk%TermOffered /= Subject(crse)%TermOffered) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': TermOffered changed to '//text_term_offered_separated(wrk%TermOffered)
                end if

                call cgi_get_named_string(QUERY_STRING, 'Prerequisite', mesg, ierr)
                call tokenize_subjects(mesg, '+', MAX_ALL_SUBJECT_PREREQ, wrk%lenPreq, wrk%Prerequisite, ierr)
                if (ierr/=0) then
                    wrk%lenPreq = Subject(crse)%lenPreq
                    wrk%Prerequisite = Subject(crse)%Prerequisite
                end if


                if ( wrk%lenPreq /= Subject(crse)%lenPreq) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': lenPreq changed to '//itoa(wrk%lenPreq)
                else
                    do j=1,wrk%lenPreq
                        if (wrk%Prerequisite(j) == Subject(crse)%Prerequisite(j)) cycle
                        isDirtySUBJECTS = .true.
                        remark = trim(remark)//': Prerequisite changed'
                    end do
                end if

                call cgi_get_named_string(QUERY_STRING, 'Corequisite', mesg, ierr)
                call tokenize_subjects(mesg, '+', MAX_ALL_SUBJECT_COREQ, wrk%lenCoreq, wrk%Corequisite, ierr)
                if (ierr/=0) then
                    wrk%lenCoreq = Subject(crse)%lenCoreq
                    wrk%Corequisite = Subject(crse)%Corequisite
                end if
                if ( wrk%lenCoreq /= Subject(crse)%lenCoreq) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': lenCoreq changed to '//itoa(wrk%lenCoreq)
                else
                    do j=1,wrk%lenCoreq
                        if (wrk%Corequisite(j) == Subject(crse)%Corequisite(j)) cycle
                        isDirtySUBJECTS = .true.
                        remark = trim(remark)//': Corequisite changed'
                    end do
                end if

                call cgi_get_named_string(QUERY_STRING, 'Concurrent', mesg, ierr)
                call tokenize_subjects(mesg, '+', MAX_ALL_SUBJECT_PREREQ, wrk%lenConc, wrk%Concurrent, ierr)
                if (ierr/=0) then
                    wrk%lenConc = Subject(crse)%lenConc
                    wrk%Concurrent = Subject(crse)%Concurrent
                end if
                if ( wrk%lenConc /= Subject(crse)%lenConc) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': lenConc changed to '//itoa(wrk%lenConc)
                else
                    do j=1,wrk%lenConc
                        if (wrk%Concurrent(j) == Subject(crse)%Concurrent(j)) cycle
                        isDirtySUBJECTS = .true.
                        remark = trim(remark)//': Concurrent changed'
                    end do
                end if

                call cgi_get_named_string(QUERY_STRING, 'ConcPrerequisite', mesg, ierr)
                call tokenize_subjects(mesg, '+', MAX_ALL_SUBJECT_CONCPREQ, wrk%lenConcPreq, wrk%ConcPrerequisite, ierr)
                if (ierr/=0) then
                    wrk%lenConcPreq = Subject(crse)%lenConcPreq
                    wrk%ConcPrerequisite = Subject(crse)%ConcPrerequisite
                end if
                if ( wrk%lenConcPreq /= Subject(crse)%lenConcPreq) then
                    isDirtySUBJECTS = .true.
                    remark = trim(remark)//': lenConcPreq changed to '//itoa(wrk%lenConcPreq)
                else
                    do j=1,wrk%lenConcPreq
                        if (wrk%ConcPrerequisite(j) == Subject(crse)%ConcPrerequisite(j)) cycle
                        isDirtySUBJECTS = .true.
                        remark = trim(remark)//': Prerequisite that can be taken concurrently changed'
                    end do
                end if

                if (isDirtySUBJECTS) then
                    if ( wrk%Name /= Subject(crse)%Name) then
                        ! add new subject?
                        j = index_to_subject(wrk%Name)
                        if (j==0) then
                            call check_array_bound (NumSubjects+NumAdditionalSubjects+1, MAX_ALL_SUBJECTS, 'MAX_ALL_SUBJECTS', &
                                criticalErr)
                            if (criticalErr) then
                                targetDepartment = DeptIdxUser
                                targetCollege = CollegeIdxUser
                                call html_college_links(device, targetCollege, &
                                    'No more space for additional subject')
                                return
                            else
                                NumAdditionalSubjects = NumAdditionalSubjects+1
                                Subject(NumSubjects+NumAdditionalSubjects) = wrk
                                crse = NumSubjects+NumAdditionalSubjects
                                tSubject = wrk%Name
                                remark = ': Added new subject '//wrk%Name
                                call get_subject_areas()
                            end if
                        else
                            remark = ': Add new subject failed; "'//trim(wrk%Name)//'" already exists.'
                        end if
                    else
                        ! update existing
                        Subject(crse) = wrk
                    end if
                end if

            case default


        end select

        if (isDirtySUBJECTS) call xml_write_subjects(trim(pathToYear)//'SUBJECTS.XML')

        targetDepartment = Subject(crse)%DeptIdx

        call html_write_header(device, 'Edit subject '//tSubject, remark(3:))
        j = index(tSubject,SPACE)
        if ( j<len_trim(tSubject) ) then
            write(device,AFORMAT) &
                trim(make_href(fnSubjectList, tSubject(:j-1), A1=tSubject(:j-1), &
                pre='<small><i>Edit another '//nbsp, post=' subject</i></small>', alt=SPACE))
        end if

        call make_form_start(device, fnEditSubject, tSubject)
        write(device,AFORMAT)  '<table border="0" width="100%">', &
            begintr//begintd//'Subject code'//endtd//begintd//'<input name="Name" size="'//trim(itoa(MAX_LEN_SUBJECT_CODE))// &
            '" value="'//trim(tSubject)//'"> (A new subject will be created if this is changed)'//endtd//endtr
        !begintr//begintd//'Subject code'//endtd//begintd//trim(tSubject)//' (Cannot be changed)'//endtd//endtr
        write(device,AFORMAT) &
            begintr//begintd//'Responsible department'//endtd//begintd//'<select name="Department">'
        do i=2,NumDepartments
            if (i/=targetDepartment) then
                j=0
            else
                j=1
            end if
            write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(Department(i)%Code)//'">'// &
                trim(Department(i)%Name)
        end do
        write(device,AFORMAT) '</select>'//endtd//endtr, &
            begintr//begintd//'Title'//endtd//begintd//'<input name="Title" size="'//trim(itoa(MAX_LEN_SUBJECT_TITLE))// &
            '" value="'//trim(Subject(crse)%Title)//'">'//endtd//endtr, &
            begintr//begintd//'Units'//endtd//begintd//'<input name="Units" size="3" value="'// &
            trim(ftoa(Subject(crse)%Units,1))//'"> (0, if non-credit; i.e., PE, NSTP)'//endtd//endtr, &
            begintr//begintd//'Term Offered'//endtd//begintd//'<input name="TermOffered" size="3" value="'// &
            trim(text_term_offered_separated(Subject(crse)%TermOffered))//'"> (1, 2, S, or combination)'//endtd//endtr, &
            begintr//begintd//'Tuition fee'//endtd//begintd//'<input name="Tuition" size="3" value="'// &
            trim(ftoa(Subject(crse)%Tuition,2))//'"> (total amount)'//endtd//endtr, &
            begintr//begintd//'Lab fee'//endtd//begintd//'<input name="LabFee" size="3" value="'// &
            trim(ftoa(Subject(crse)%LabFee,2))//'"> (additional to tuition)'//endtd//endtr
        write(device,AFORMAT)  &
            begintr//begintd//'Hours lecture class'//endtd//begintd//'<input name="LectHours" size="3" value="'// &
            trim(ftoa(Subject(crse)%LectHours,2))//'"> (0, if no lecture component)'//endtd//endtr, &
            begintr//begintd//'Workload for lecture class'//endtd//begintd//'<input name="LectLoad" size="3" value="'// &
            trim(ftoa(Subject(crse)%LectLoad,2))//'"> (0, if no lecture component)'//endtd//endtr, &
            begintr//begintd//'Min size lecture class'//endtd//begintd//'<input name="MinLectSize" size="3" value="'// &
            trim(itoa(Subject(crse)%MinLectSize))//'">'//endtd//endtr, &
            begintr//begintd//'Max size lecture class'//endtd//begintd//'<input name="MaxLectSize" size="3" value="'// &
            trim(itoa(Subject(crse)%MaxLectSize))//'">'//endtd//endtr, &
            begintr//begintd//'Hours lab/recit/comp class'//endtd//begintd//'<input name="LabHours" size="3" value="'// &
            trim(ftoa(Subject(crse)%LabHours,2))//'"> (0, if no lab/recit/computations component)'//endtd//endtr, &
            begintr//begintd//'Workload for lab class'//endtd//begintd//'<input name="LabLoad" size="3" value="'// &
            trim(ftoa(Subject(crse)%LabLoad,2))//'"> (0, if no lab/recit/computations component)'//endtd//endtr, &
            begintr//begintd//'Min size lab/recit/comp class'//endtd//begintd//'<input name="MinLabSize" size="3" value="'// &
            trim(itoa(Subject(crse)%MinLabSize))//'">'//endtd//endtr, &
            begintr//begintd//'Max size lab/recit/comp class'//endtd//begintd//'<input name="MaxLabSize" size="3" value="'// &
            trim(itoa(Subject(crse)%MaxLabSize))//'">'//endtd//endtr
        !      lenPreq, Prerequisite(MAX_ALL_SUBJECT_PREREQ), &
        i = Subject(crse)%Prerequisite(1)
        mesg = Subject(i)%Name
        do j=2,Subject(crse)%lenPreq
            i = Subject(crse)%Prerequisite(j)
            mesg = trim(mesg)//'+'//Subject(i)%Name
        end do
        write(device,AFORMAT) &
            begintr//begintd//'Prerequisite'//endtd//begintd//'<input name="Prerequisite" size="'// &
            trim(itoa(Subject(crse)%lenPreq*2*MAX_LEN_SUBJECT_CODE/3))//'" value="'//trim(mesg)//'">'//endtd//endtr
        !      lenCoreq, Corequisite(MAX_ALL_SUBJECT_COREQ), &
        i = Subject(crse)%Corequisite(1)
        mesg = Subject(i)%Name
        do j=2,Subject(crse)%lenCoreq
            i = Subject(crse)%Corequisite(j)
            mesg = trim(mesg)//'+'//Subject(i)%Name
        end do
        write(device,AFORMAT) &
            begintr//begintd//'Corequisite'//endtd//begintd//'<input name="Corequisite" size="'// &
            trim(itoa(Subject(crse)%lenCoreq*2*MAX_LEN_SUBJECT_CODE/3))//'" value="'//trim(mesg)//'">'//endtd//endtr
        !      lenConc, Concurrent(MAX_ALL_SUBJECT_CONCURRENT), &
        i = Subject(crse)%Concurrent(1)
        mesg = Subject(i)%Name
        do j=2,Subject(crse)%lenConc
            i = Subject(crse)%Concurrent(j)
            mesg = trim(mesg)//'+'//Subject(i)%Name
        end do
        write(device,AFORMAT) &
            begintr//begintd//'Concurrent with'//endtd//begintd//'<input name="Concurrent" size="'// &
            trim(itoa(Subject(crse)%lenConc*2*MAX_LEN_SUBJECT_CODE/3))//'" value="'//trim(mesg)//'">'//endtd//endtr
        !      lenConcPreq, ConcPrerequisite(MAX_ALL_SUBJECT_CONCPREQ)
        i = Subject(crse)%ConcPrerequisite(1)
        mesg = Subject(i)%Name
        do j=2,Subject(crse)%lenConcPreq
            i = Subject(crse)%ConcPrerequisite(j)
            mesg = trim(mesg)//'+'//Subject(i)%Name
        end do
        write(device,AFORMAT) &
            begintr//begintd//'Prerequisite that can be concurrent'//endtd//begintd//'<input name="ConcPrerequisite" size="'// &
            trim(itoa(Subject(crse)%lenConcPreq*2*MAX_LEN_SUBJECT_CODE/3))//'" value="'//trim(mesg)//'">'//endtd//endtr

        write(device,AFORMAT) '</table><br>'//nbsp//'<input name="action" type="submit" value="Update"></form><pre>', &
            'NOTE: The prerequisite is in prefix notation, the tokens being separated by the "+" symbol. Patterns are', &
            '   1. NONE - no prerequisite', &
            '   2. COI  - Consent of Instructor', &
            '   3. Student classification (FRESHMAN, SOPHOMORE, JUNIOR, SENIOR)', &
            '   4. year level in curriculum (FIRST, SECOND, THIRD, FOURTH, FIFTH, GRADUATING)', &
            '   5. SUBJECT CODE - i.e., another subject', &
            '   6. OR+2+5, OR+3+5, OR+4+5', &
            '   7. OR+5+5, AND+5+5', &
            '   8. OR+6+6, OR+6+7, OR+7+7, AND+7+7', &
            '</pre><hr>'


    end subroutine subject_edit


end module EditSUBJECTS
