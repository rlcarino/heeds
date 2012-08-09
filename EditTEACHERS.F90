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


module EditTEACHERS

  use HTML

  implicit none

contains

  subroutine teacher_list_all (device, NumSections, Section, fn)
    integer, intent (in) :: device, fn
    integer, intent (in) :: NumSections
    type (TYPE_SECTION), intent(in), dimension (0:MAX_ALL_SECTIONS) :: Section
    integer :: fac, nfacs, nsect, mdx, sdx, tdx, ierr, crse
    integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
    character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
    character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
    character (len=127) :: mesg
    integer, dimension(60,6) :: TimeTable
    character(len=1) :: ch
    real :: total_hrs, total_units
    character(len=5) :: str_hrs, str_units ! xx.x

    ! collect teachers
    tArray = 0
    nfacs = 0
    select case (fn)

           case (fnSearch)
                   targetDepartment = DeptIdxUser
                   targetCollege  = Department(targetDepartment)%CollegeIdx
                   ! search string ?
                   call cgi_get_named_string(QUERY_STRING, 'A2', mesg, ierr)
                   if (mesg==SPACE) then
                           targetCollege = CollegeIdxUser
                           targetDepartment = DeptIdxUser
                           call html_write_header(device, 'Search', '<br><hr>Search string not specified')
                           return
                   else
                           do tdx=1,NumTeachers+NumAdditionalTeachers
                              fac = TeacherRank(tdx)
                              if (isRoleChair .and. Teacher(fac)%DeptIdx/=DeptIdxUser) cycle
                              if (index(Teacher(fac)%Name,trim(mesg)//SPACE)>0) then
                                nfacs = nfacs+1
                                tArray(nfacs) = fac
                              end if
                           end do
                   end if
                   mesg = 'Search results for "'//trim(mesg)//'" teachers'

           case (fnTeachersByDept, fnNextTeachersByDept)
                   ! which department ?
                   call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)
                   targetDepartment = index_to_dept(tDepartment)
                   if (ierr/=0 .or. targetDepartment<=0) then
                           targetCollege = CollegeIdxUser
                           targetDepartment = DeptIdxUser
                           call html_write_header(device, 'Teachers', '<br><hr>Dept "'//tDepartment//'" not found')
                           return
                   else
                           do tdx=1,NumTeachers+NumAdditionalTeachers
                              fac = TeacherRank(tdx)
                              if (targetDepartment/=Teacher(fac)%DeptIdx) cycle
                              nfacs = nfacs+1
                              tArray(nfacs) = fac
                           end do
                   end if
                   mesg = 'Teachers in '//tDepartment

           case (fnTeachersByname, fnNextTeachersByname)
                   ! which college ?
                   call cgi_get_named_string(QUERY_STRING, 'A2', ch, ierr)
                   call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
                   targetCollege = index_to_college(tCollege)
                   if (ierr/=0 .or. targetCollege<=0 .or. ch==SPACE) then
                           targetCollege = CollegeIdxUser
                           targetDepartment = DeptIdxUser
                           call html_write_header(device, 'Teachers', '<br><hr>College "'//tCollege// &
                             '" or starting letter "'//ch//'" for name not found')
                           return
                   else
                           do tdx=1,NumTeachers+NumAdditionalTeachers
                              fac = TeacherRank(tdx)
                              if (Department(Teacher(fac)%DeptIdx)%CollegeIdx /= targetCollege) cycle
                              if (Teacher(fac)%Name(1:1) /= ch) cycle
                              nfacs = nfacs+1
                              tArray(nfacs) = fac
                           end do
                           if (nfacs>0) then
                                   targetDepartment = Teacher(tArray(nfacs))%DeptIdx
                                   targetCollege = Department(targetDepartment)%CollegeIdx
                           else
                                   targetDepartment = DeptIdxUser
                                   targetCollege = CollegeIdxUser
                           end if
                   end if
                   mesg = '"'//ch//'" teachers in '//tCollege

           case (fnTeacherConflicts, fnNextTeacherConflicts)
                   ! which college ?
                   call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
                   targetCollege = index_to_college(tCollege)
                   if (ierr/=0 .or. targetCollege<=0) then
                           targetCollege = CollegeIdxUser
                           targetDepartment = DeptIdxUser
                           call html_write_header(device, 'Conflicts', '<br><hr>College "'//tCollege//'" not found')
                           return
                   else
                           ! collect teachers
                           do tdx=1,NumTeachers+NumAdditionalTeachers
                             fac = TeacherRank(tdx)
                             if (Department(Teacher(fac)%DeptIdx)%CollegeIdx/=targetCollege) cycle ! not in college
                             ! collect classes of teacher
                             call timetable_clear(TimeTable)
                             nsect = 0 ! how many conflicts
                             do sdx=1,NumSections
                               call meetings_of_section_by_teacher(NumSections, Section, sdx, fac, n_meetings, meetings)
                               if (n_meetings>0) then ! teacher assigned to this section
                                       ierr = -10
                                       call timetable_add_meetings_of_section(NumSections, Section, sdx, &
                                            n_meetings, meetings, TimeTable, ierr)
                                       if (ierr /= -10) then ! conflict
                                               nsect = nsect+1
                                               exit
                                       end if
                               end if
                             end do
                             if (nsect>0) then ! conflict
                                     nfacs = nfacs + 1
                                     tArray(nfacs) = fac
                             end if
                           end do
                           
                   end if
                   mesg = 'Teachers with schedule conflicts in '//tCollege

    end select
    
    call html_write_header(device, mesg)

    if (nfacs == 0) then
            write(device,AFORMAT) '(None?)'
    else
            ! sort teachers
            do tdx=1,nfacs-1
              do sdx=tdx+1,nfacs
                if (Teacher(tArray(sdx))%Name<Teacher(tArray(tdx))%Name) then
                        fac =tArray(sdx)
                        tArray(sdx) = tArray(tdx)
                        tArray(tdx) = fac
                end if
              end do
            end do
            
            write(device,AFORMAT) '<table border="0" width="60%">'//&
              begintr//thalignleft//'Name'//endth// &
                       thaligncenter//'Classes'//endth// &
                       thaligncenter//'Units'//endth// &
                       thaligncenter//'Hours'//endth// &
                       thaligncenter//'Remark'//endth//endtr
   
            do tdx=1,nfacs
              fac = tArray(tdx)
              ! check conflicts
              mesg = SPACE
              call timetable_clear(TimeTable)
              ! collect classes of teacher
              nsect = 0
              total_hrs = 0.0
              total_units = 0.0
              do sdx=1,NumSections
                call meetings_of_section_by_teacher(NumSections, Section, sdx, fac, n_meetings, meetings)
                if (n_meetings>0) then ! teacher assigned to this section
                        crse = Section(sdx)%SubjectIdx
                        nsect = nsect+1
                        tArray(nfacs+nsect) = sdx
                        if (is_lecture_class(sdx, Section)) then
                                total_units = total_units + Subject(crse)%LectHours
                        else
                                total_units = total_units + Subject(crse)%Units - Subject(crse)%LectHours
                        end if
                        !total_units = total_units + Subject(Section(sdx)%SubjectIdx)%Units
                        do mdx=1,n_meetings
                          total_hrs = total_hrs + 0.25*(Section(sdx)%eTimeIdx(mdx)-Section(sdx)%bTimeIdx(mdx))
                        end do
                        ierr = -10
                        call timetable_add_meetings_of_section(NumSections, Section, sdx, n_meetings, meetings, TimeTable, ierr)
                        if (ierr /= -10) then
                                mesg = red//'Conflict!'//black
                        end if
                end if
              end do
#if defined DO_NOT_ENCODE
              QUERY_put = Teacher(fac)%TeacherID
#else
              call cgi_url_encode(Teacher(fac)%TeacherID, QUERY_put)
#endif

              write(str_hrs,'(f5.1)') total_hrs
              write(str_units,'(f5.1)') total_units
              write(device,AFORMAT) begintr//begintd//trim(Teacher(fac)%Name)
              if (isRoleAdmin .or. (isRoleChair .and.  DeptIdxUser==Teacher(fac)%DeptIdx)) then
                write(device,AFORMAT) trim(cgi_make_href(fnEditTeacher, targetUser, 'Edit', &
                  A1=QUERY_put, pre='&nbsp;<small>', post='</small>'))
              end if
              !if (nsect>0) then
                write(device,AFORMAT) trim(cgi_make_href(fnOFFSET+fnTeacherSchedule, targetUser, itoa(nsect), &
                  A1=QUERY_put, pre=endtd//tdaligncenter, post=endtd))
              !else
              !  write(device,AFORMAT) endtd//tdaligncenter//trim(itoa(nsect))//endtd
              !end if
              write(device,AFORMAT) &
                tdaligncenter//str_units//endtd// &
                tdaligncenter//str_hrs//endtd// &
                tdaligncenter//trim(mesg)//endtd//endtr
            end do
            write(device,AFORMAT) '</table>'

    end if
    write(device,AFORMAT) '<hr>'

    return
  end subroutine teacher_list_all


  subroutine teacher_schedule(device, NumSections, Section, LoadSource)
    integer, intent(in), optional :: LoadSource
    integer, intent (in) :: device
    integer, intent (in) :: NumSections
    type (TYPE_SECTION), intent(in out), dimension (0:MAX_ALL_SECTIONS) :: Section
    integer :: mdx, sdx, tLen1, tLen2, ierr, sect, LoadFromDept
    character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
    character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
    character(len=MAX_LEN_CLASS_ID) :: tAction, tClassId
    integer, dimension(60,6) :: TimeTable
    logical :: conflicted, assigned, allowed_to_edit
    character(len=127) :: mesg

    ! which teacher?
    call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, ierr)
    targetTeacher = index_to_teacher(tTeacher)
    if (ierr/=0 .or. targetTeacher==0) then
            targetCollege = CollegeIdxUser
            targetDepartment = DeptIdxUser
            call html_write_header(device, 'Teacher schedule', '<br><hr>Teacher "'//tTeacher//'" not found')
            return
    end if
    targetDepartment = Teacher(targetTeacher)%DeptIdx
    targetCollege = Department(targetDepartment)%CollegeIdx
    allowed_to_edit = isRoleAdmin .or. (isRoleChair .and. targetDepartment==DeptIdxUser)
    mesg = SPACE

    ! check if there are other arguments
    call cgi_get_named_string(QUERY_STRING, 'A2', tAction, ierr)
    if (ierr==0) then ! action is Add or Del
            call cgi_get_named_string(QUERY_STRING, 'A3', tClassId, ierr)
            sect = index_to_section(tClassId, NumSections, Section)
            if (sect>0) then ! target of action is indexed by sect
                    LoadFromDept = Section(sect)%DeptIdx
                    if (tAction=='Add') then
                            do mdx=1,Section(sect)%NMeets
                              Section(sect)%TeacherIdx(mdx) = targetTeacher
                            end do
                            mesg = 'Added '//tClassId
                    end if
                    if (tAction=='Del') then
                            do mdx=1,Section(sect)%NMeets
                              Section(sect)%TeacherIdx(mdx) = 0
                            end do
                            mesg = 'Deleted '//tClassId
                    end if
                    call xml_write_sections(pathToSections, NumSections, Section, 0)
                    call xml_write_sections(pathToSectionUpdates, &
                      NumSections, Section, LoadFromDept)
            end if
    end if

    call html_write_header(device, 'Teaching schedule of '//Teacher(targetTeacher)%Name, mesg)

    ! collect meetings of teacher targetTeacher
    call timetable_meetings_of_teacher(NumSections, Section, targetTeacher, 0, tLen1, tArray, TimeTable, conflicted)
    !write(*, '(3i6)') (tArray(mdx), mdx=1,tLen1)
    !if (conflicted) write(*,*) 'Conflict in schedule '//trim(Teacher(targetTeacher)%Name)
    call list_sections_to_edit(device, Section, tLen1, tArray, fnOFFSET+fnTeacherSchedule, tTeacher, 'Del', allowed_to_edit)
    !write(*, '(3i6)') (tArray(mdx), mdx=1,tLen1)
    call timetable_display(device, Section, TimeTable)

    write(device,AFORMAT) '<hr>'
    ! make list of TBA sections LoadSource that fit the schedule of teacher
    if (present(LoadSource)) then
            LoadFromDept = LoadSource
    else
            call cgi_get_named_string(QUERY_STRING, 'A4', tDepartment, ierr)
            LoadFromDept = index_to_dept(tDepartment)
            if (ierr/=0 .or. LoadFromDept<=0) then
                    LoadFromDept = targetDepartment
            else
                    mesg = 'Searched for feasible classes in '//tDepartment
            end if
    end if
    
    tLen2 = 0
    do sdx=1,NumSections
      if (LoadFromDept/=Section(sdx)%DeptIdx) cycle ! not in this department
      if (Section(sdx)%NMeets==1 .and. Section(sdx)%DayIdx(1)==0) cycle ! meeting days/time not specified 
      ! teacher(s) already assigned to this section?
      assigned = .false.
      do mdx=1,Section(sdx)%NMeets
        if (Section(sdx)%TeacherIdx(mdx)/=0) assigned = .true.
      end do
      if (assigned) cycle ! section has a teacher
      if (.not. is_conflict_timetable_with_section(NumSections, Section, sdx, TimeTable)) then ! add to list
              do mdx=1,Section(sdx)%NMeets
                tArray(tLen1+tLen2+1) = sdx
                tArray(tLen1+tLen2+2) = mdx
                tArray(tLen1+tLen2+3) = 0
                tLen2 = tLen2+3
              end do
      end if
    end do
    tArray(tLen1+tLen2+1) = 0
    tArray(tLen1+tLen2+2) = 0
    tArray(tLen1+tLen2+3) = 0
    if (tLen2>0) then
      call list_sections_to_edit(device, Section, tLen2, tArray(tLen1+1), fnOFFSET+fnTeacherSchedule, tTeacher, 'Add', &
        allowed_to_edit, &
        '<b>Classes with TBA teachers in '//trim(Department(LoadFromDept)%Code)// &
        ' that fit the schedule of '//trim(Teacher(targetTeacher)%Name)//'</b>')
    end if

    ! search for feasible classes in another department?
    write(device,AFORMAT) &
        '<br><form name="input" method="post" action="'//CGI_PATH//'">', &
        '<input type="hidden" name="F" value="'//trim(itoa(fnOFFSET+fnTeacherSchedule))//'">', &
        '<input type="hidden" name="U" value="'//trim(itoa(targetUser))//'">', &
        '<input type="hidden" name="A1" value="'//trim(tTeacher)//'">'


    write(device,AFORMAT) '<br>Search for feasible classes in : <select name="A4">'
    do mdx=2,NumDepartments
      if (mdx/=LoadFromDept) then
                ierr = 0
      else
                ierr = 1
      end if
      write(device,AFORMAT) '<option value="'//trim(Department(mdx)%Code)//'"'//trim(selected(ierr))//'> '// &
        trim(Department(mdx)%Code)//dash//trim(Department(mdx)%Name)
    end do
    write(device,AFORMAT) '</select>&nbsp;<input type="submit" value="Find classes"><hr>'
    return
  end subroutine teacher_schedule


  subroutine teacher_edit(device)
    integer, intent(in) :: device
    character(len=MAX_LEN_TEACHER_CODE) :: tTeacher, tAction
    character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
    integer :: ierr, tdx, i, j
    character (len=255) :: mesg, remark
    type (TYPE_TEACHER) :: wrk
    logical :: isDirtyTEACHERS

    ! which subject ?
    call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, tdx)
    if (tdx/=0 .or. tTeacher==SPACE) then
            mesg = 'Teacher record to edit not specified?'
    else
            tdx = index_to_teacher(tTeacher)
            mesg = 'Teacher code '//tTeacher//' is invalid?'
    end if
    if (tdx<=0) then ! subject code is invalid
            targetCollege = CollegeIdxUser
            targetDepartment = DeptIdxUser
            call html_write_header(device, 'Edit teacher record', '<br><hr>'//trim(mesg))
            return
    end if

    wrk = Teacher(tdx) ! make a working copy

    ! check for other arguments
    call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
    !write(*,*) 'ierr=', ierr, ', action=', tAction
    isDirtyTEACHERS = .false.
    remark = SPACE

    select case (trim(tAction))

        case ('Update')
            
            call cgi_get_named_string(QUERY_STRING, 'Login', wrk%TeacherID, ierr)
            !write(*,*) 'ierr=', ierr, ', Login=', wrk%TeacherID
            if (ierr/=0) wrk%TeacherID = Teacher(tdx)%TeacherID
            if (wrk%TeacherID /= Teacher(tdx)%TeacherID) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': '//trim(tTeacher)//' - Login changed to '//wrk%TeacherID
            end if
            
            call cgi_get_named_string(QUERY_STRING, 'Name', wrk%Name, ierr)
            !write(*,*) 'ierr=', ierr, ', Name=', wrk%Name
            if (ierr/=0) wrk%Name = Teacher(tdx)%Name
            if (wrk%Name /= Teacher(tdx)%Name) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': '//trim(tTeacher)//' - Name changed to '//wrk%Name
            end if

            call cgi_get_named_integer(QUERY_STRING, 'Load', wrk%MaxLoad, ierr)
            !write(*,*) 'ierr=', ierr, ', MaxLoad=', wrk%MaxLoad
            if (ierr/=0 .or. wrk%MaxLoad<=0) wrk%MaxLoad = Teacher(tdx)%MaxLoad
            if (wrk%DeptIdx /= Teacher(tdx)%DeptIdx) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': '//trim(tTeacher)//' - Max load changed to '//itoa(wrk%MaxLoad)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Department', tDepartment, ierr)
            wrk%DeptIdx = index_to_dept(tDepartment)
            !write(*,*) 'ierr=', ierr, ', DeptIdx=', wrk%DeptIdx
            if (ierr/=0 .or. wrk%DeptIdx<=0) wrk%DeptIdx = Teacher(tdx)%DeptIdx
            if (wrk%DeptIdx /= Teacher(tdx)%DeptIdx) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': '//trim(tTeacher)//' - Department changed to '//Department(wrk%DeptIdx)%Code
            end if

            if (isDirtyTEACHERS) then
                    if ( wrk%TeacherID /= Teacher(tdx)%TeacherID) then
                            ! add new subject?
                            j = index_to_teacher(wrk%TeacherID)
                            if (j==0) then
                                    NumAdditionalTeachers = NumAdditionalTeachers+1
                                    Teacher(NumTeachers+NumAdditionalTeachers) = wrk
                                    tdx = NumTeachers+NumAdditionalTeachers
                                    tTeacher = wrk%TeacherID
                                    remark = ': Added new teacher '//wrk%TeacherID
                            else
                                    remark = ': Add new teacher failed; "'//trim(wrk%TeacherID)//'" already exists.'
                                    isDirtyTEACHERS = .false.
                            end if
                    else
                            ! update existing
                            Teacher(tdx) = wrk
                    end if
            end if

        case default
                !write(*,*) 'Unknown action: '//tAction


    end select
    
    if (isDirtyTEACHERS) then
            call xml_write_teachers(pathToCurrent)
            call html_college_links(device, CollegeIdxUser, remark(3:))
            return
    end if


    targetDepartment = Teacher(tdx)%DeptIdx
    targetCollege = Department(targetDepartment)%CollegeIdx

    call html_write_header(device, 'Edit teacher '//tTeacher, remark(3:))

    write(device,AFORMAT) &
      '<form name="input" method="post" action="'//CGI_PATH//'">', &
      '<input type="hidden" name="U" value="'//trim(itoa(targetUser))//'">', &
      '<input type="hidden" name="F" value="'//trim(itoa(fnEditTeacher))//'">'// &
      '<input type="hidden" name="A1" value="'//trim(tTeacher)//'">', &
      '<table border="0" width="100%">'

    write(device,AFORMAT) &
      begintr//begintd//'Login name '//endtd//begintd//'<input name="Login" size="'//trim(itoa(MAX_LEN_TEACHER_CODE))// &
        '" value="'//trim(tTeacher)//'"> (A new teacher record will be created if name is changed)'//endtd//endtr, &
      begintr//begintd//'Teacher name '//endtd//begintd//'<input name="Name" size="'//trim(itoa(MAX_LEN_TEACHER_NAME))// &
        '" value="'//trim(Teacher(tdx)%Name)//'">'//endtd//endtr, &
      begintr//begintd//'Max load '//endtd//begintd//'<input name="Load" size="3" value="'// &
        trim(itoa(Teacher(tdx)%MaxLoad))//'">'//endtd//endtr
    write(device,AFORMAT) &
      begintr//begintd//'Department '//endtd//begintd//'<select name="Department">'
    do i=2,NumDepartments
      if (i/=targetDepartment) then
              j=0
      else
              j=1
      end if
      write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(Department(i)%Code)//'">'// &
        trim(Department(i)%Name)
    end do

    write(device,AFORMAT) '</table><br>&nbsp;<input name="action" type="submit" value="Update"></form><hr>'

    return
  end subroutine teacher_edit


end module EditTEACHERS
