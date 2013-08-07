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


module EditTEACHERS

    use DisplayTEACHERS

    implicit none

contains


    subroutine teacher_edit(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher, tAction
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        integer :: ierr, tdx, j
        character (len=255) :: header, remark, tmpRHS
        type (TYPE_TEACHER) :: wrk
        logical :: isDirtyTEACHERS
        !character (len=MAX_LEN_PASSWD_VAR) :: Password
        !integer :: lenP

        call html_comment('teacher_edit()')

        isDirtyTEACHERS = .false.
        remark = SPACE

        ! which teacher ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, tdx)
        if (tdx/=0 .or. tTeacher==SPACE) tTeacher = 'Guest'
        tdx = index_to_teacher(tTeacher)

        targetTeacher = tdx
        targetDepartment = Teacher(targetTeacher)%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx

        if (REQUEST==fnGenerateTeacherPassword) then
            call set_password(Teacher(tdx)%Password)
            call xml_write_teachers(trim(pathToYear)//'TEACHERS.XML')
            call teacher_info(device, Teacher(tdx), 'Edit info for teacher '//tTeacher, &
                SPACE, 'Update', tdx)
            return
        end if

        wrk = Teacher(tdx)

        ! check for requested action
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)

        if (ierr/=0 .or. tAction==SPACE) then ! no action; display existing info

            if (trim(tTeacher)=='Guest') then
                header = 'Add new teacher'
                tAction = 'Add'
            else
                header = 'Edit info for teacher '//tTeacher
                tAction = 'Update'
            end if

            call teacher_info(device, wrk, header, remark, tAction, tdx)

        else ! action is Add or Update; collect changes

            call cgi_get_named_string(QUERY_STRING, 'Login', wrk%TeacherID, ierr)
            !write(*,*) 'ierr=', ierr, ', Login=', wrk%TeacherID
            if (ierr/=0) wrk%TeacherID = Teacher(tdx)%TeacherID
            if (wrk%TeacherID /= Teacher(tdx)%TeacherID) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Username='//wrk%TeacherID
            end if

            call cgi_get_named_string(QUERY_STRING, 'Name', tmpRHS, ierr)
            wrk%Name = tmpRHS
            !write(*,*) 'ierr=', ierr, ', Name=', wrk%Name
            if (ierr/=0) wrk%Name = Teacher(tdx)%Name
            if (wrk%Name /= Teacher(tdx)%Name) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Name='//wrk%Name
            end if

            call cgi_get_named_string(QUERY_STRING, 'Bachelor', tmpRHS, ierr)
            wrk%Bachelor = tmpRHS
            !write(*,*) 'ierr=', ierr, ', Bachelor=', wrk%Bachelor
            if (ierr/=0) wrk%Bachelor = Teacher(tdx)%Bachelor
            if (wrk%Bachelor /= Teacher(tdx)%Bachelor) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Bachelor='//wrk%Bachelor
            end if

            call cgi_get_named_string(QUERY_STRING, 'Master', tmpRHS, ierr)
            wrk%Master = tmpRHS
            !write(*,*) 'ierr=', ierr, ', Master=', wrk%Master
            if (ierr/=0) wrk%Master = Teacher(tdx)%Master
            if (wrk%Master /= Teacher(tdx)%Master) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Master='//wrk%Master
            end if

            call cgi_get_named_string(QUERY_STRING, 'Doctorate', tmpRHS, ierr)
            wrk%Doctorate = tmpRHS
            !write(*,*) 'ierr=', ierr, ', Doctorate=', wrk%Doctorate
            if (ierr/=0) wrk%Doctorate = Teacher(tdx)%Doctorate
            if (wrk%Doctorate /= Teacher(tdx)%Doctorate) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Doctorate='//wrk%Doctorate
            end if

            call cgi_get_named_string(QUERY_STRING, 'Specialization', tmpRHS, ierr)
            wrk%Specialization = tmpRHS
            !write(*,*) 'ierr=', ierr, ', Specialization=', wrk%Specialization
            if (ierr/=0) wrk%Specialization = Teacher(tdx)%Specialization
            if (wrk%Specialization /= Teacher(tdx)%Specialization) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Specialization='//wrk%Specialization
            end if

            call cgi_get_named_string(QUERY_STRING, 'Role', wrk%Role, ierr)
            !write(*,*) 'ierr=', ierr, ', Role=', wrk%Role
            if (ierr/=0) wrk%Role = Teacher(tdx)%Role
            if (wrk%Role /= Teacher(tdx)%Role) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Role='//wrk%Role
            end if

            call cgi_get_named_integer(QUERY_STRING, 'Load', wrk%MaxLoad, ierr)
            !write(*,*) 'ierr=', ierr, ', MaxLoad=', wrk%MaxLoad
            if (ierr/=0 .or. wrk%MaxLoad<=0) wrk%MaxLoad = Teacher(tdx)%MaxLoad
            if (wrk%MaxLoad /= Teacher(tdx)%MaxLoad) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Max load='//itoa(wrk%MaxLoad)
            end if

            call cgi_get_named_integer(QUERY_STRING, 'Rank', wrk%Rank, ierr)
            !write(*,*) 'ierr=', ierr, ', Rank=', wrk%Rank
            if (ierr/=0 .or. wrk%Rank<=0) wrk%Rank = Teacher(tdx)%Rank
            if (wrk%Rank /= Teacher(tdx)%Rank) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Rank='//AcademicRank(wrk%Rank)
            end if

            call cgi_get_named_integer(QUERY_STRING, 'Step', wrk%Step, ierr)
            !write(*,*) 'ierr=', ierr, ', Step=', wrk%Step
            if (ierr/=0 .or. wrk%Step<=0) wrk%Step = Teacher(tdx)%Step
            if (wrk%Step /= Teacher(tdx)%Step) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Step='//RankStep(wrk%Step)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Department', tDepartment, ierr)
            wrk%DeptIdx = index_to_dept(tDepartment)
            !write(*,*) 'ierr=', ierr, ', DeptIdx=', wrk%DeptIdx
            if (ierr/=0 .or. wrk%DeptIdx<=0) wrk%DeptIdx = Teacher(tdx)%DeptIdx
            if (wrk%DeptIdx /= Teacher(tdx)%DeptIdx) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Unit='//Department(wrk%DeptIdx)%Code
            end if


            if (isDirtyTEACHERS) then  ! some changes

                if (wrk%TeacherID /= Teacher(tdx)%TeacherID) then  ! new username
                    j = index_to_teacher(wrk%TeacherID)
                    if (j==0) then ! not used

                        if (trim(tAction)=='Add') then
                            NumTeachers = NumTeachers+1
                            tdx = NumTeachers
                            tTeacher = wrk%TeacherID
                            remark = ': Added new teacher'//remark
                        else
                            remark = ': Updated teacher'//remark
                        end if
                        Teacher(tdx) = wrk
                        call sort_alphabetical_teachers()

                    else
                        remark = ': Invalid username: "'//trim(wrk%TeacherID)//'" is already taken by '//Teacher(j)%Name
                        isDirtyTEACHERS = .false.
                    end if

                else ! same username
                    remark = ': Updated teacher'//remark
                    Teacher(tdx) = wrk
                end if

                if (trim(tAction)=='Add') then
                    header = 'Add new teacher'
                else
                    header = 'Edit info for teacher '//tTeacher
                end if
                call teacher_info(device, wrk, header, remark(3:), tAction, tdx)

                if (isDirtyTEACHERS) call xml_write_teachers(trim(pathToYear)//'TEACHERS.XML')

            else ! Add or Update clicked, but no changes made
                remark = ': No changes made?'
                call html_college_links(device, Department(wrk%DeptIdx)%CollegeIdx, &
                    trim(tTeacher)//remark)
            end if

        end if

    end subroutine teacher_edit


    subroutine teacher_delete (device)
        integer, intent (in) :: device
        integer :: fac, nsect(3), sdx, tdx, term
        integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
        character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
        character (len=127) :: mesg
        integer, dimension(60,6) :: TimeTable
        character (len=80) :: tDesc
        integer :: tYear, tTerm

        call html_comment('teacher_delete()')

        ! which teacher ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, tdx)
        fac = index_to_teacher(tTeacher)
        if (fac==0) then
            call html_college_links(device, CollegeIdxUser, mesg='Teacher "'//trim(tTeacher)//'" not found.')
            return
        end if

        ! count classes handled by teacher
        nsect = 0
        mesg = SPACE
        do tTerm=termBegin,termEnd
            call qualify_term (tTerm, tYear, term, tDesc)
            call timetable_clear(TimeTable)
            do sdx=1,NumSections(term)
                call meetings_of_section_by_teacher(NumSections(term), Section(term,0:), &
                    sdx, fac, n_meetings, meetings)
                if (n_meetings==0) cycle ! teacher not assigned to this section
                nsect(term) = nsect(term)+1
            end do
            if (nsect(term)>0) then
                mesg = trim(mesg)//COMMA//SPACE//txtSemester(term+6)
            end if
        end do
        if (sum(nsect)>0) then
            mesg = 'Delete "'//trim(tTeacher)//'" failed; has classes during '//trim(mesg(2:))//' term.'
            call html_college_links(device, CollegeIdxUser, mesg)
            return
        end if

        call initialize_teacher(Teacher(fac))
        Teacher(fac)%DeptIdx = 0
        call xml_write_teachers(trim(pathToYear)//'TEACHERS.XML')
        call html_college_links(device, CollegeIdxUser, mesg='Teacher "'//trim(tTeacher)//'" deleted.')

    end subroutine teacher_delete


end module EditTEACHERS
