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
    integer :: fac, nfacs, nsect, mdx, sdx, tdx, ierr
    integer :: n_meetings, meetings(MAX_SECTION_MEETINGS)
    character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
    character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
    character (len=127) :: mesg
    integer, dimension(60,6) :: TimeTable
    character(len=1) :: ch
    real :: totalLect, lectHours, totalLab, labHours, totalUnits, meetingUnits

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
            
            write(device,AFORMAT) '<table border="0" width="90%">'//&
              begintr//thalignleft//'Name (Specialization)'//endth// &
                       thaligncenter//'No. of<br>classes'//endth// &
                       thaligncenter//'Lect<br>hours'//endth// &
                       thaligncenter//'Lab<br>hours'//endth// &
                       thaligncenter//'Teaching<br>load'//endth// &
                       thaligncenter//'Remark'//endth//endtr
   
            do tdx=1,nfacs
              fac = tArray(tdx)
              ! check conflicts
              mesg = SPACE
              call timetable_clear(TimeTable)
              ! collect classes of teacher
              nsect = 0
              totalUnits = 0.0
              totalLect = 0.0
              totalLab = 0.0
              do sdx=1,NumSections
                call meetings_of_section_by_teacher(NumSections, Section, sdx, fac, n_meetings, meetings)
                if (n_meetings>0) then ! teacher assigned to this section
                        nsect = nsect+1
                        tArray(nfacs+nsect) = sdx
                        do mdx=1,n_meetings
                            call class_hours_and_load(mdx, sdx, Section, meetingUnits, lectHours, labHours)
                            totalLect = totalLect + lectHours
                            totalLab = totalLab + labHours
                            totalUnits = totalUnits + meetingUnits
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

              write(device,AFORMAT) begintr//begintd//trim(Teacher(fac)%Name)//' ('//trim(Teacher(fac)%Specialization)//')'
              if (isRoleAdmin .or. (isRoleChair .and.  DeptIdxUser==Teacher(fac)%DeptIdx)) then
                write(device,AFORMAT) trim(cgi_make_href(fnEditTeacher, 'Edit', &
                  A1=QUERY_put, pre=nbsp//'<small>', post='</small>'))
              end if
              write(device,AFORMAT) trim(cgi_make_href(fnOFFSET+fnTeacherSchedule, 'Edit', &
                  A1=QUERY_put, pre=endtd//tdaligncenter//itoa(nsect)//'<small>', post='</small>'//endtd))
              write(device,'(2(a,f5.1), a,f5.2,a)') tdaligncenter, totalLect, &
                    endtd//tdaligncenter, totalLab, &
                    endtd//tdaligncenter, totalUnits, &
                    '/'//trim(itoa(Teacher(fac)%MaxLoad))// &
                    trim(cgi_make_href(fnPrintableWorkload+fnOFFSET, 'Printable', &
                    A1=QUERY_put, pre=nbsp//'<small>', post='</small>'))//endtd// &
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
                    call xml_write_sections(pathToSOURCE, NumSections, Section, 0)
                    call xml_write_sections(pathToUPDATES, &
                      NumSections, Section, LoadFromDept)
            end if
    end if

    call html_write_header(device, cgi_make_href(fnPrintableWorkload+fnOFFSET, 'Printable', &
        A1=tTeacher, post=' teaching schedule of '//Teacher(targetTeacher)%Name), mesg)

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
        '<br><form name="input" method="post" action="'//CGI_SCRIPT//'">', &
        '<input type="hidden" name="F" value="'//trim(itoa(fnOFFSET+fnTeacherSchedule))//'">', &
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
    write(device,AFORMAT) '</select>'//nbsp//'<input type="submit" value="Find classes"><hr>'
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
                remark = trim(remark)//': Login changed to '//wrk%TeacherID
            end if
            
            call cgi_get_named_string(QUERY_STRING, 'Name', wrk%Name, ierr)
            !write(*,*) 'ierr=', ierr, ', Name=', wrk%Name
            if (ierr/=0) wrk%Name = Teacher(tdx)%Name
            if (wrk%Name /= Teacher(tdx)%Name) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Name changed to '//wrk%Name
            end if

            call cgi_get_named_string(QUERY_STRING, 'Bachelor', wrk%Bachelor, ierr)
            !write(*,*) 'ierr=', ierr, ', Bachelor=', wrk%Bachelor
            if (ierr/=0) wrk%Bachelor = Teacher(tdx)%Bachelor
            if (wrk%Bachelor /= Teacher(tdx)%Bachelor) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Bachelor changed to '//wrk%Bachelor
            end if

            call cgi_get_named_string(QUERY_STRING, 'Master', wrk%Master, ierr)
            !write(*,*) 'ierr=', ierr, ', Master=', wrk%Master
            if (ierr/=0) wrk%Master = Teacher(tdx)%Master
            if (wrk%Master /= Teacher(tdx)%Master) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Master changed to '//wrk%Master
            end if

            call cgi_get_named_string(QUERY_STRING, 'Doctorate', wrk%Doctorate, ierr)
            !write(*,*) 'ierr=', ierr, ', Doctorate=', wrk%Doctorate
            if (ierr/=0) wrk%Doctorate = Teacher(tdx)%Doctorate
            if (wrk%Doctorate /= Teacher(tdx)%Doctorate) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Doctorate changed to '//wrk%Doctorate
            end if

            call cgi_get_named_string(QUERY_STRING, 'Specialization', wrk%Specialization, ierr)
            !write(*,*) 'ierr=', ierr, ', Specialization=', wrk%Specialization
            if (ierr/=0) wrk%Specialization = Teacher(tdx)%Specialization
            if (wrk%Specialization /= Teacher(tdx)%Specialization) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Specialization changed to '//wrk%Specialization
            end if

            call cgi_get_named_integer(QUERY_STRING, 'Load', wrk%MaxLoad, ierr)
            !write(*,*) 'ierr=', ierr, ', MaxLoad=', wrk%MaxLoad
            if (ierr/=0 .or. wrk%MaxLoad<=0) wrk%MaxLoad = Teacher(tdx)%MaxLoad
            if (wrk%MaxLoad /= Teacher(tdx)%MaxLoad) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Max load changed to '//itoa(wrk%MaxLoad)
            end if

            call cgi_get_named_integer(QUERY_STRING, 'Rank', wrk%Rank, ierr)
            !write(*,*) 'ierr=', ierr, ', Rank=', wrk%Rank
            if (ierr/=0 .or. wrk%Rank<=0) wrk%Rank = Teacher(tdx)%Rank
            if (wrk%Rank /= Teacher(tdx)%Rank) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Rank changed to '//AcademicRank(wrk%Rank)
            end if

            call cgi_get_named_integer(QUERY_STRING, 'Step', wrk%Step, ierr)
            !write(*,*) 'ierr=', ierr, ', Step=', wrk%Step
            if (ierr/=0 .or. wrk%Step<=0) wrk%Step = Teacher(tdx)%Step
            if (wrk%Step /= Teacher(tdx)%Step) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Step changed to '//RankStep(wrk%Step)
            end if

            call cgi_get_named_string(QUERY_STRING, 'Department', tDepartment, ierr)
            wrk%DeptIdx = index_to_dept(tDepartment)
            !write(*,*) 'ierr=', ierr, ', DeptIdx=', wrk%DeptIdx
            if (ierr/=0 .or. wrk%DeptIdx<=0) wrk%DeptIdx = Teacher(tdx)%DeptIdx
            if (wrk%DeptIdx /= Teacher(tdx)%DeptIdx) then
                isDirtyTEACHERS = .true.
                remark = trim(remark)//': Unit changed to '//Department(wrk%DeptIdx)%Code
            end if

            if (isDirtyTEACHERS) then
                    if ( wrk%TeacherID /= Teacher(tdx)%TeacherID) then
                            ! add new teacher?
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
            call html_college_links(device, Department(wrk%DeptIdx)%CollegeIdx, trim(tTeacher)//remark)
            return
    end if


    targetDepartment = Teacher(tdx)%DeptIdx
    targetCollege = Department(targetDepartment)%CollegeIdx

    call html_write_header(device, 'Edit teacher '//tTeacher, remark(3:))

    write(device,AFORMAT) &
      '<form name="input" method="post" action="'//CGI_SCRIPT//'">', &
      '<input type="hidden" name="F" value="'//trim(itoa(fnEditTeacher))//'">'// &
      '<input type="hidden" name="A1" value="'//trim(tTeacher)//'">', &
      '<table border="0" width="100%">'

    write(device,AFORMAT) &
      begintr//begintd//'Login name '//endtd//begintd//'<input name="Login" size="'//trim(itoa(MAX_LEN_TEACHER_CODE))// &
        '" value="'//trim(tTeacher)//'"> (A new teacher record will be created if login name is changed)'//endtd//endtr, &
      begintr//begintd//'Teacher name '//endtd//begintd//'<input name="Name" size="'//trim(itoa(MAX_LEN_TEACHER_NAME))// &
        '" value="'//trim(Teacher(tdx)%Name)//'">'//endtd//endtr
    ! dept
    write(device,AFORMAT) &
      begintr//begintd//'Unit '//endtd//begintd//'<select name="Department">'
    do i=2,NumDepartments
      if (i/=targetDepartment) then
              j=0
      else
              j=1
      end if
      write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(Department(i)%Code)//'">'// &
        trim(Department(i)%Name)
    end do
    write(device,AFORMAT) '</select>'//endtd//endtr
    ! rank
    write(device,AFORMAT) &
      begintr//begintd//'Rank '//endtd//begintd//'<select name="Rank">'
    do i=0,4
      if (i/=Teacher(tdx)%Rank) then
              j=0
      else
              j=1
      end if
      write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(itoa(i))//'">'// &
        trim(AcademicRank(i))
    end do
    write(device,AFORMAT) '</select>'//endtd//endtr
    ! step
    write(device,AFORMAT) &
      begintr//begintd//'Step '//endtd//begintd//'<select name="Step">'
    do i=0,12
      if (i/=Teacher(tdx)%Step) then
              j=0
      else
              j=1
      end if
      write(device,AFORMAT) '<option '//trim(selected(j))//' value="'//trim(itoa(i))//'">'// &
        trim(RankStep(i))
    end do
    write(device,AFORMAT) '</select>'//endtd//endtr
    ! bachelor
    write(device,AFORMAT) &
      begintr//begintd//'Bachelor '//endtd//begintd//'<input name="Bachelor" size="'// &
        trim(itoa(MAX_LEN_TEACHER_DEGREE))//'" value="'//trim(Teacher(tdx)%Bachelor)//'">'//endtd//endtr
    ! master
    write(device,AFORMAT) &
      begintr//begintd//'Master '//endtd//begintd//'<input name="Master" size="'// &
        trim(itoa(MAX_LEN_TEACHER_DEGREE))//'" value="'//trim(Teacher(tdx)%Master)//'">'//endtd//endtr
    ! doctorate
    write(device,AFORMAT) &
      begintr//begintd//'Doctorate '//endtd//begintd//'<input name="Doctorate" size="'// &
        trim(itoa(MAX_LEN_TEACHER_DEGREE))//'" value="'//trim(Teacher(tdx)%Doctorate)//'">'//endtd//endtr
    ! specialization
    write(device,AFORMAT) &
      begintr//begintd//'Specialization '//endtd//begintd//'<input name="Specialization" size="'// &
        trim(itoa(MAX_LEN_TEACHER_DEGREE))//'" value="'//trim(Teacher(tdx)%Specialization)//'">'//endtd//endtr
    ! max load
    write(device,AFORMAT) &
      begintr//begintd//'Max load '//endtd//begintd//'<input name="Load" size="3" value="'// &
        trim(itoa(Teacher(tdx)%MaxLoad))//'">'//endtd//endtr

    write(device,AFORMAT) '</table><br>'//nbsp//'<input name="action" type="submit" value="Update"></form><hr>'

    return
  end subroutine teacher_edit


  subroutine teacher_schedule_printable(device, NumSections, Section, NumBlocks, Block)
    integer, intent (in) :: device
    integer, intent (in) :: NumSections
    type (TYPE_SECTION), intent(in out), dimension (0:MAX_ALL_SECTIONS) :: Section
        integer, intent (in) :: NumBlocks
        type (TYPE_BLOCK), dimension(0:), intent(in) :: Block
    integer :: tLen1, ierr
    character(len=MAX_LEN_TEACHER_CODE) :: tTeacher
    integer, dimension(60,6) :: TimeTable
    logical :: conflicted

    ! which teacher?
    call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, ierr)
    targetTeacher = index_to_teacher(tTeacher)
    if (ierr/=0 .or. targetTeacher==0) then
            targetCollege = CollegeIdxUser
            targetDepartment = DeptIdxUser
            call html_write_header(device, 'Teacher schedule', '<br><hr>Teacher "'//tTeacher//'" not found')
            return
    end if

    ! collect meetings of teacher targetTeacher
    call timetable_meetings_of_teacher(NumSections, Section, targetTeacher, 0, tLen1, tArray, TimeTable, conflicted)
    if (conflicted) then
            targetCollege = CollegeIdxUser
            targetDepartment = DeptIdxUser
            call html_write_header(device, 'Teacher schedule', '<br><hr>Teacher "'//tTeacher//'": conflict in schedule')
            return
    end if

    targetDepartment = Teacher(targetTeacher)%DeptIdx
    targetCollege = Department(targetDepartment)%CollegeIdx

    write(device,AFORMAT) '<table border="0" width="100%">', &
        begintr//tdaligncenter//'Republic of the Philippines'//endtd//endtr, &
        begintr//tdaligncenter//'<b>'//trim(UniversityName)//'</b>'//endtd//endtr, &
        begintr//tdaligncenter//trim(UniversityAddress)//endtd//endtr, &
        begintr//begintd//'<br>'//endtd//endtr, &
        begintr//tdaligncenter//trim(College(targetCollege)%Name)//endtd//endtr, &
        begintr//tdaligncenter//'<b>INDIVIDUAL FACULTY TEACHING LOAD</b>'//endtd//endtr, &
        begintr//tdaligncenter//trim(txtSemester(currentTerm+6))//' Semester, SY '// &
            trim(itoa(currentYear))//dash//trim(itoa(currentYear+1))//endtd//endtr, &
        begintr//begintd//'<br>'//endtd//endtr, &
        '</table>'

    write(device,AFORMAT) '<table border="0" width="100%">', &
        begintr//'<td width="50%">Name: <b>'//trim(Teacher(targetTeacher)%Name)//'</b>'//endtd, &
                 '<td width="50%">Area of Retraining: '//trim(Teacher(targetTeacher)%Specialization)//endtd//endtr
    ! bachelor
    write(device,AFORMAT) &
        begintr//'<td width="50%">Bachelor: '//trim(Teacher(targetTeacher)%Bachelor)//'</b>'//endtd, &
                 '<td width="50%">Faculty Rank: '//trim(AcademicRank(Teacher(targetTeacher)%Rank))//nbsp// &
                         trim(RankStep(Teacher(targetTeacher)%Step))//endtd//endtr
    ! master
    write(device,AFORMAT) &
        begintr//'<td width="50%">Master: '//trim(Teacher(targetTeacher)%Master)//'</b>'//endtd, &
                 '<td width="50%">Length of Service in University: '//nbsp//endtd//endtr
    ! doctorate
    write(device,AFORMAT) &
        begintr//'<td width="50%">Doctorate: '//trim(Teacher(targetTeacher)%Doctorate)//'</b>'//endtd, &
                begintd//nbsp//endtd//endtr
    write(device,AFORMAT) '</table><hr>'

    call teacher_workload(device, Section, tLen1, tArray, NumBlocks, Block)

    write(device,AFORMAT) &
      '<form name="input" method="post" action="'//CGI_SCRIPT//'">', &
      '<input type="hidden" name="F" value="0'//trim(itoa(fnPrintableWorkload+fnOFFSET))//'">'// &
      '<input type="hidden" name="A1" value="'//trim(tTeacher)//'">'

    write(device,AFORMAT) '<br><br><br><br><table border="0" width="100%">', &
        begintr//'<td width="50%">Prepared by:<br><br><br><br><br><br>'// &
                 '<input name="Dean" size="'//trim(itoa(MAX_LEN_TEACHER_NAME))// &
                 '" value="(College Dean)">'// &
                 '<br>College Dean<br><br><br>'//endtd, &
                 '<td width="50%">Received by:<br><br><br><br><br><br>'// &
                 '<input name="DeanCollege" size="'//trim(itoa(MAX_LEN_TEACHER_NAME))// &
                 '" value="'//trim(Teacher(targetTeacher)%Name)//'">'// &
                 '<br>Faculty<br><br><br>'//endtd//endtr
    write(device,AFORMAT) &
        begintr//'<td width="50%">'//nbsp//endtd, &
                 '<td width="50%">Recommending Approval:<br><br><br><br><br><br>'// &
                 '<input name="DeanInstruction" size="'//trim(itoa(MAX_LEN_TEACHER_NAME))// &
                 '" value="(Dean of Instruction)">'// &
                 '<br>Dean of Instruction<br><br><br>'//endtd//endtr
    write(device,AFORMAT) &
        begintr//'<td width="50%">Approved by:<br><br><br><br><br><br>'// &
                 '<input name="President" size="'//trim(itoa(MAX_LEN_TEACHER_NAME))// &
                 '" value="(President)">'// &
                 '<br>Office of the President'//endtd, &
                 '<td width="50%">'//nbsp//endtd//endtr
    write(device,AFORMAT) '</table>'

    !call timetable_display(device, Section, TimeTable)
    !write(device,AFORMAT) '<hr>'

    return
  end subroutine teacher_schedule_printable


  subroutine teacher_workload(device, Section, lenSL, SectionList, NumBlocks, Block)
        integer, intent(in) :: device, lenSL, SectionList(3*lenSL+3)
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section
        integer, intent (in) :: NumBlocks
        type (TYPE_BLOCK), dimension(0:), intent(in) :: Block
        integer :: crse, idx, mdx, rdx, sdx, previous
        real :: totalUnits, meetingUnits
        real :: lectHours, labHours, totalLect, totalLab

        if (lenSL < 3) then
            write(device,AFORMAT) '<br>No teaching load?<br>'
            return
        end if

        totalUnits = 0.0
        totalLect = 0.0
        totalLab = 0.0

        write(device,AFORMAT) '<table border="0" width="100%">'//begintr, &
            thalignleft//'Subject<br>Code'//endth// &
            thalignleft//'Descriptive Title (Units)'//endth// &
            thalignleft//'Program<br>Year/Sec'//endth// &
            thalignleft//'Class<br>Code'//endth// &
            thalignleft//'Class<br>Size'//endth // &
            thalignleft//'Day'//endth// &
            thalignleft//'Time'//endth// &
            thalignleft//'Room'//endth// &
            thalignleft//'Hrs.<br>Lect'//endth// &
            thalignleft//'Hrs.<br>Lab'//endth// &
            thalignleft//'Work<br>Load'//endth// &
            endtr

        previous = 0
        do idx=1,lenSL,3
            sdx=SectionList(idx)
            mdx=SectionList(idx+1)
            crse = Section(sdx)%SubjectIdx

            !new section ?
            if (sdx/=previous) then

                previous = sdx

                !  subject
                write(device,AFORMAT) begintr//begintd//trim(Subject(crse)%Name)//endtd
                !  subject title, credit
                write(device,'(a,f5.1,a)') begintd//trim(Subject(crse)%Title)//' (', Subject(crse)%Units, ')'//endtd
                ! block
                write(device,AFORMAT) begintd
                call blocks_in_section(device, sdx, 0, NumBlocks, Block)
                write(device,AFORMAT) endtd
                ! section code
                write(device,AFORMAT) begintd//trim(Section(sdx)%Code)//endtd
                ! class size
                write(device,AFORMAT) begintd//trim(itoa(Section(sdx)%Slots))//endtd

            else
                write(device,AFORMAT) begintr// &
                    tdnbspendtd// & ! subject code
                    tdnbspendtd// & ! title, credit
                    tdnbspendtd// & ! block
                    tdnbspendtd// & ! section code
                    tdnbspendtd     ! class size

            end if

            ! day
            write(device,AFORMAT) begintd//txtDay(Section(sdx)%DayIdx(mdx))//endtd
            ! time
            write(device,AFORMAT) begintd//trim(text_time_period(Section(sdx)%bTimeIdx(mdx), &
                Section(sdx)%eTimeIdx(mdx)))//endtd
            ! room
            rdx = Section(sdx)%RoomIdx(mdx)
            if (rdx > 0) then
                write(device,AFORMAT) begintd//trim(Room(rdx)%Code)//endtd
            else
                write(device,AFORMAT) begintd//'TBA'//endtd
            end if

            call class_hours_and_load(mdx, sdx, Section, meetingUnits, lectHours, labHours)
            totalLect = totalLect + lectHours
            totalLab = totalLab + labHours
            totalUnits = totalUnits + meetingUnits

            if (lectHours>0.0) then
                write(device,'(a,f5.2,a)') begintd, lectHours, endtd ! lect hours
            else
                write(device,AFORMAT) tdnbspendtd
            end if

            if (labHours>0.0) then
                write(device,'(a,f5.2,a)') begintd, labHours, endtd ! lab hours
            else
                write(device,AFORMAT) tdnbspendtd
            end if

            write(device,'(a,f5.2,a)') begintd, meetingUnits, endtd//endtr

        end do
        write(device,AFORMAT) begintr//'<td colspan="11"><hr>'//endtd//endtr, &
            begintr//tdnbspendtd// & ! subject code
            tdnbspendtd// & ! title, credit
            tdnbspendtd// & ! block
            tdnbspendtd// & ! section code
            tdnbspendtd//  & ! class size
            tdnbspendtd//  & ! day
            tdnbspendtd//  & ! time
            begintd//'<b>Totals</b> : '//endtd ! room
        write(device,'(3(a,f5.2,a))') begintd, totalLect, endtd, & ! hours lect
            begintd, totalLab, endtd, & ! hours lab
            begintd, totalUnits, endtd//endtr ! load
        !write(device,AFORMAT) begintr//'<td colspan="11"><hr>'//endtd//endtr

        write(device,AFORMAT) '</table>'

        return
  end subroutine teacher_workload


  subroutine class_hours_and_load(mdx, sdx, Section, meetingUnits, lectHours, labHours)
        integer, intent (in) :: mdx, sdx
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section
        real, intent(out) :: meetingUnits, lectHours, labHours
        integer :: crse
        real :: meetingHours

        crse = Section(sdx)%SubjectIdx
        meetingHours = (Section(sdx)%eTimeIdx(mdx) - Section(sdx)%bTimeIdx(mdx))/4.0
        if (is_lecture_lab_subject(crse)) then
            if (is_lecture_class(sdx, Section)) then ! lecture of lecture-lab
                meetingUnits = meetingHours*Subject(crse)%LectLoad/Subject(crse)%LectHours
                lectHours = meetingHours
                labHours = 0.0
            else ! lab of lecture-lab
                meetingUnits = meetingHours*Subject(crse)%LabLoad/Subject(crse)%LabHours
                lectHours = 0.0
                labHours = meetingHours
            end if
        else if (Subject(crse)%LectHours>0.0) then ! lecture-only
            meetingUnits = meetingHours*Subject(crse)%LectLoad/Subject(crse)%LectHours
            lectHours = meetingHours
            labHours = 0.0
        else if (Subject(crse)%LabHours>0.0) then ! lab-only
            meetingUnits = meetingHours*Subject(crse)%LabLoad/Subject(crse)%LabHours
            lectHours = 0.0
            labHours = meetingHours
        end if

        return
  end subroutine class_hours_and_load


end module EditTEACHERS
