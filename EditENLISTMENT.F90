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


module EditENLISTMENT

  use HTML

  implicit none

contains
  

  
  subroutine enlistment_grades (device, NumSections, Section)
    integer, intent (in) :: device
    integer, intent (in) :: NumSections
    type (TYPE_SECTION), intent(in), dimension (0:) :: Section
    integer :: ldx, n_count, tdx, std, ierr, ncol, sect, crse, idx_select
    character(len=MAX_LEN_CLASS_ID) :: tClassId
    logical :: isDirtyFORM5

    ! which section?
    call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, ierr)
    targetSection = index_to_section(tClassId, NumSections, Section)
    if (ierr/=0 .or. targetSection==0) then
            targetDepartment = DeptIdxUser
            targetCollege = CollegeIdxUser
            call html_write_header(device, 'Gradesheet', '<hr><br>Class "'//tClassId//'" not found')
            return
    end if
    targetDepartment = Section(targetSection)%DeptIdx
    targetCollege = Department(targetDepartment)%CollegeIdx
    crse = Section(targetSection)%SubjectIdx

    ! collect students
    n_count = 0
    isDirtyFORM5 = .false.
    do tdx=1,NumStudents
        std = StdRank(tdx)
        do ncol=1,Preenlisted(std)%lenSubject
            sect = Preenlisted(std)%Section(ncol)
            if (sect==0) cycle
            if (targetSection == sect) then
                call cgi_get_named_integer(QUERY_STRING, trim(Student(std)%StdNo), idx_select, ierr)
                if (ierr==0) then
                        isDirtyFORM5 = .true.
                        Preenlisted(std)%Grade(ncol) = idx_select
                end if
                tArray(n_count+1) = std
                tArray(n_count+2) = Preenlisted(std)%Grade(ncol)
                n_count = n_count+2
                exit
            elseif (Preenlisted(std)%Subject(ncol)==crse .and. is_lecture_lab_subject(crse)) then
                ldx = index(Section(sect)%ClassId,DASH)
                if (ldx>0) then ! student is accommodated in a lab section
                   if (trim(tClassId)==Section(sect)%ClassId(:ldx-1)) then ! lab of lecture 
                      call cgi_get_named_integer(QUERY_STRING, trim(Student(std)%StdNo), idx_select, ierr)
                      if (ierr==0) then
                              isDirtyFORM5 = .true.
                              Preenlisted(std)%Grade(ncol) = idx_select 
                      end if
                      tArray(n_count+1) = std
                      tArray(n_count+2) = Preenlisted(std)%Grade(ncol)
                      n_count = n_count+2
                   end if
                end if
            end if
        end do
    end do

!    if (isDirtyFORM5) call write_all_enlistment(trim(dirXML)//trim(path)//'ENLISTMENT', &
!    Preenlisted, Section, currentYear, currentTerm)
    if (isDirtyFORM5) call xml_write_pre_enlistment(pathToCurrent, 'ENLISTMENT', Preenlisted, Section)

    call html_write_header(device,'Gradesheet for '//tClassId)

    if (n_count == 0) then
      write(device,AFORMAT) 'No students in this section?'
    else
      call make_form_start(device, fnGradeSheet, tClassId)
      write(device,AFORMAT) '<table border="0" width="80%">'
      do tdx=1,n_count,2
        std = tArray(tdx)
        ldx = Student(std)%CurriculumIdx
        write(device,AFORMAT) &
          begintr//begintd//trim(itoa((tdx+1)/2))//'.'//endtd// &
                   begintd//trim(Student(std)%StdNo)//endtd, &
                   begintd//trim(Student(std)%Name)//endtd// &
                   begintd//Curriculum(ldx)%Code//endtd//begintd
      
        write(device,AFORMAT) '<select name="'//trim(Student(std)%StdNo)//'">'
        do ncol=1,19
          if (tArray(tdx+1)/=ncol) then
                  idx_select = 0
          else
                  idx_select = 1
          end if
          write(device,AFORMAT) '<option value="'//trim(itoa(ncol))//'" '//trim(selected(idx_select))//'> '// &
            txtGrade(pGrade(ncol)) 
        end do
        write(device,AFORMAT) '</select>'//endtd//endtr
      end do
      write(device,AFORMAT) begintr//tdnbspendtd//tdnbspendtd//tdnbspendtd//tdnbspendtd//begintd, &
        '<input type="submit" value="Submit">'//endtd//endtr//'</table></form>'

    end if
    write(device,AFORMAT) '<hr>'

    return
  end subroutine enlistment_grades 


  subroutine enlistment_edit (device, NumSections, Section, NumBlocks, Block)
    integer, intent (in) :: device
    integer, intent (in out) :: NumBlocks, NumSections
    type (TYPE_SECTION), intent(in out), dimension (0:) :: Section
    type (TYPE_BLOCK), dimension(0:), intent(in out) :: Block
    character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
    character(len=MAX_LEN_BLOCK_CODE) :: tBlock
    character(len=MAX_LEN_CLASS_ID) :: tClassId
    character(len=3*MAX_LEN_SUBJECT_CODE) :: tAction, search_string
    character(len=MAX_LEN_SUBJECT_CODE) :: tSubject
    integer ::  bdx, blk, fdx, mdx, crse, lect, sect, ierr, tLen1, tLen2, pos, n_opts, idx_opt
    integer, dimension(60,6) :: TimeTable
    logical :: conflicted, matched
    character(len=255) :: mesg
    logical :: isDirtyFORM5, allowed_to_edit

    type (TYPE_PRE_ENLISTMENT) :: Advice
    
    ! which student?
    call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
    targetStudent = index_to_student(tStdNo)
    if (ierr/=0 .or. targetStudent==0) then
            targetDepartment = DeptIdxUser
            targetCollege = CollegeIdxUser
            call html_write_header(device, 'Edit enlistment', '<hr><br>Student "'//tStdNo//'" not found')
            return
    end if
    targetCurriculum = Student(targetStudent)%CurriculumIdx
    allowed_to_edit = isRoleAdmin .or. (isRoleSRE .and. &
                      CurrProgCode(CurriculumIdxUser)==CurrProgCode(targetCurriculum))

    call recalculate_available_seats(Section)
    isDirtyFORM5 = .false.

    ! check for other arguments
    mesg = SPACE
    call cgi_get_named_string(QUERY_STRING, 'A2', tAction, ierr)
    select case (trim(tAction))

        case ('Add')
            call cgi_get_named_string(QUERY_STRING, 'A3', tClassId, ierr)
            sect = index_to_section(tClassId, NumSections, Section)
            if (sect>0) then ! target of action is indexed by sect
                if (Section(sect)%RemSlots>0) then
                    crse = Section(sect)%SubjectIdx
                    do fdx=1,Preenlisted(targetStudent)%lenSubject
                      if (Preenlisted(targetStudent)%Subject(fdx)==crse) then 
                              Preenlisted(targetStudent)%Section(fdx) = sect
                              Preenlisted(targetStudent)%Grade(fdx) = gdxREGD
                              Section(sect)%RemSlots = Section(sect)%RemSlots - 1
                              exit
                      end if
                    end do
                    mesg = 'Added '//tClassId
                    isDirtyFORM5 = .true.
                 else
                    mesg = 'NOT added, already full: '//tClassId
                 end if
            end if

        case ('Block')
            call cgi_get_named_string(QUERY_STRING, 'A3', tBlock, ierr)
            blk = index_to_block(tBlock, NumBlocks, Block)
            if (blk>0) then ! target block found

              do bdx=1,Block(blk)%NumClasses
                crse = Block(blk)%Subject(bdx)
                sect = Block(blk)%Section(bdx)
                if (sect==0) then ! section not specified in block
                        mesg = 'NOT added (no section): '//trim(Subject(crse)%Name)//'; '//mesg
                        cycle
                end if
                tClassId = Section(sect)%ClassId
                matched = .false.
                do fdx=1,Preenlisted(targetStudent)%lenSubject
                  if (crse/=Preenlisted(targetStudent)%Subject(fdx)) cycle
                  matched = .true.
                  exit
                end do
                if (matched) then ! indexed by bdx/fdx

                  if (Section(sect)%RemSlots>0) then
                    do fdx=1,Preenlisted(targetStudent)%lenSubject
                      if (Preenlisted(targetStudent)%Subject(fdx)/=crse) cycle
                        Preenlisted(targetStudent)%Section(fdx) = sect
                        Preenlisted(targetStudent)%Grade(fdx) = gdxREGD
                        Section(sect)%RemSlots = Section(sect)%RemSlots - 1
                        exit
                    end do
                    mesg = 'Added '//trim(tClassId)//'; '//mesg
                    isDirtyFORM5 = .true.
                  else
                    mesg = 'NOT added '//trim(tClassId)//' (full); '//mesg
                  end if

                else
                  mesg = 'NOT advised '//trim(Subject(crse)%Name)//'; '//mesg
                end if

              end do

            else
                    mesg = 'Block "'//trim(tBlock)//'" not found'
            end if

        case ('Prerog')
            call cgi_get_named_string(QUERY_STRING, 'A3', tClassId, ierr)
            sect = index_to_section(tClassId, NumSections, Section)
            if (sect>0) then ! target of action is indexed by sect
                if (Section(sect)%RemSlots>0) then
                    crse = Section(sect)%SubjectIdx
                    do fdx=1,Preenlisted(targetStudent)%lenSubject
                      if (Preenlisted(targetStudent)%Subject(fdx)==crse) then 
                              Preenlisted(targetStudent)%Section(fdx) = sect
                              Preenlisted(targetStudent)%Grade(fdx) = gdxREGD
                              Section(sect)%RemSlots = Section(sect)%RemSlots - 1
                              exit
                      end if
                    end do
                    mesg = 'Added '//tClassId
                    isDirtyFORM5 = .true.
                 else
                    crse = Section(sect)%SubjectIdx
                    do fdx=1,Preenlisted(targetStudent)%lenSubject
                      if (Preenlisted(targetStudent)%Subject(fdx)==crse) then 
                              Preenlisted(targetStudent)%Section(fdx) = sect
                              Preenlisted(targetStudent)%Grade(fdx) = gdxREGD
                              Section(sect)%Slots = Section(sect)%Slots + 1
                              exit
                      end if
                    end do
                    mesg = 'Enlisted by teacher''s prerogative in '//tClassId
                    isDirtyFORM5 = .true.
                 end if
            end if

        case ('Del')
            call cgi_get_named_string(QUERY_STRING, 'A3', tClassId, ierr)
            sect = index_to_section(tClassId, NumSections, Section)
            if (sect>0) then ! target of action is indexed by sect
                    do fdx=1,Preenlisted(targetStudent)%lenSubject
                      if (sect==Preenlisted(targetStudent)%Section(fdx)) then 
                              Preenlisted(targetStudent)%Section(fdx) = 0
                              Preenlisted(targetStudent)%Grade(fdx) = 0
                              Section(sect)%RemSlots = Section(sect)%RemSlots + 1
                              exit
                      end if
                    end do
                    mesg = 'Deleted '//tClassId
                    isDirtyFORM5 = .true.
            end if

        case ('Switch')
            call initialize_pre_enlistment(Advice)

            call cgi_get_named_integer(QUERY_STRING, 'earned', Advice%UnitsEarned, ierr)
            call cgi_get_named_integer(QUERY_STRING, 'classification', Advice%StdClassification, ierr)
            call cgi_get_named_integer(QUERY_STRING, 'year', Advice%StdYear, ierr)
            !write(*,*) Advice%UnitsEarned, Advice%StdClassification, Advice%StdYear

            call cgi_get_named_integer(QUERY_STRING, 'allowed', Advice%AllowedLoad, ierr)
            call cgi_get_named_integer(QUERY_STRING, 'group', Advice%StdPriority, ierr)
            call cgi_get_named_integer(QUERY_STRING, 'priority', Advice%NPriority, ierr)
            call cgi_get_named_integer(QUERY_STRING, 'alternate', Advice%NAlternates, ierr)
            call cgi_get_named_integer(QUERY_STRING, 'current', Advice%NCurrent, ierr)
            !write(*,*) Advice%AllowedLoad, Advice%StdPriority, Advice%NPriority, Advice%NAlternates

            Advice%lenSubject = Advice%NPriority+Advice%NAlternates+Advice%NCurrent
            do fdx=1,Advice%lenSubject
              call cgi_get_named_string(QUERY_STRING, 'pri'//itoa(fdx), search_string, ierr)
              pos = index(search_string,COMMA)
              tSubject = search_string(pos+1:)
              read(tSubject,'(f6.4)') Advice%Contrib(fdx)
              tSubject = search_string(1:pos-1)
              crse = index_to_subject(tSubject)
              !write(*,*) crse, tSubject
              Advice%Subject(fdx) = crse
              ! check if already enlisted
              do mdx=1,Preenlisted(targetStudent)%lenSubject
                if (Preenlisted(targetStudent)%Subject(mdx)==crse) then
                        if (Preenlisted(targetStudent)%Section(mdx)>0) then
                          write(*,*) 'Retained section '//Section(Preenlisted(targetStudent)%Section(mdx))%Classid
                        else
                          write(*,*) 'Retained subject '//tSubject
                        end if
                        Advice%Section(fdx) = Preenlisted(targetStudent)%Section(mdx)
                        Advice%Grade(fdx) = gdxREGD
                        Preenlisted(targetStudent)%Section(mdx) = 0 ! remove section
                        exit
                end if
              end do
            end do
            ! free slots for deleted sections
            do fdx=1,Preenlisted(targetStudent)%lenSubject
              sect = Preenlisted(targetStudent)%Section(fdx)
              if (sect>0) then
                      Section(sect)%RemSlots = Section(sect)%RemSlots + 1
                      write(*,*) 'Freed a seat in '//Section(sect)%ClassId
              end if
            end do
            ! update Preenlisted()
            Preenlisted(targetStudent) = Advice
            isDirtyFORM5 = .true.

    end select

    if (isDirtyFORM5) then
        call xml_write_pre_enlistment(pathToCurrent, 'ENLISTMENT', Preenlisted, Section)
      end if
    
    call html_write_header(device, trim(Student(targetStudent)%StdNo)//SPACE//trim(Student(targetStudent)%Name)// &
      SPACE//DASH//SPACE//trim(Curriculum(Student(targetStudent)%CurriculumIdx)%Code), mesg)

    ! collect classes for student 
    call timetable_meetings_of_student(NumSections, Section, targetStudent, Preenlisted, 0, tLen1, tArray, TimeTable, conflicted)
    !!
    !call enlistment_fees(device, targetStudent, NumSections, Section, '<br><b>Estimated fees</b>')
    !write(device,AFORMAT) '<hr>'

    call list_sections_to_edit(device, Section, tLen1, tArray, fnChangeMatriculation, tStdNo, 'Del', allowed_to_edit, &
      '<b>Enlisted subjects</b> '//nbsp//trim(make_href(fnPrintableSchedule, 'Printable', &
      A1=tStdNo, pre='<small>(', post=')</small>')) )
    if (tLen1>0) call timetable_display(device, Section, TimeTable)

    ! make list of available sections for alternate subjects that fit the schedule of student
    tLen2 = 0
    do fdx=1,Preenlisted(targetStudent)%lenSubject
      if (Preenlisted(targetStudent)%Section(fdx)/=0) cycle ! already enlisted
      tLen2 = tLen2 + 1
    end do
    if (tLen2>0) then
      mdx = 0
      write(device,AFORMAT) '<br><b>Other subjects that may be enlisted</b>: '
      do fdx=1,Preenlisted(targetStudent)%lenSubject
        if (Preenlisted(targetStudent)%Section(fdx)==0) then
          mdx = mdx+1
          tSubject = Subject(Preenlisted(targetStudent)%Subject(fdx))%Name
          write(device,AFORMAT) nbsp//nbsp//trim(itoa(mdx))//'). <a href="#'// &
            trim(tSubject)//'">'//trim(tSubject)//'</a>'
        end if
      end do
      write(device,AFORMAT) '<br>'
      do fdx=1,Preenlisted(targetStudent)%lenSubject
        if (Preenlisted(targetStudent)%Section(fdx)/=0) cycle ! already enlisted
        crse = Preenlisted(targetStudent)%Subject(fdx) ! index to subject
        tSubject = Subject(crse)%Name
        !write(*,*) 'Alternate subject - '//tSubject
        !tLen2 = 0
        n_opts = 0
        
        do sect=1,NumSections
          if (crse/=Section(sect)%SubjectIdx) cycle ! not the right subject
          ! add lecture class if a lab class is selected
          if (is_lecture_lab_subject(crse) .and. is_lecture_class(sect, Section)) then
                  cycle
          end if
          ! check for conflict
          if (is_conflict_timetable_with_section(NumSections, Section, sect, TimeTable)) then
                  !write(*,*) '   Available, but schedule conflict - '//Section(sect)%ClassId
                  cycle
          end if
          ! lab section of lect+lab subject or lab-only subject, or section of lect-only subject, is OK
          ! if lab section of lect+lab subject, check if lecture schedule also fits

          ! place options at end of Section() array
          idx_opt = NumSections + n_opts + 1
          Section(idx_opt) = Section(sect)
          if (is_lecture_lab_subject(crse)) then ! find the lecture section
            pos = index(Section(sect)%ClassId, DASH)
            tClassId = Section(sect)%ClassId(:pos-1)
            lect = index_to_section(tClassId, NumSections, Section)
            if (is_conflict_timetable_with_section(NumSections, Section, lect, TimeTable)) then ! lecture class is not OK
              !write(*,*) '   Available, but lecture class schedule conflict - '//Section(lect)%ClassId
              cycle
            end if
            !write(*,*) 'OPTION IS - '//tClassId
            ! add lecture schedule to lab schedule
            do mdx=1,Section(lect)%NMeets
                pos = Section(idx_opt)%NMeets + 1 
                Section(idx_opt)%DayIdx(pos) = Section(lect)%DayIdx(mdx)
                Section(idx_opt)%bTimeIdx(pos) = Section(lect)%bTimeIdx(mdx)
                Section(idx_opt)%eTimeIdx(pos) = Section(lect)%eTimeIdx(mdx)
                Section(idx_opt)%RoomIdx(pos) = Section(lect)%RoomIdx(mdx)
                Section(idx_opt)%TeacherIdx(pos) = Section(lect)%TeacherIdx(mdx)
                Section(idx_opt)%NMeets = pos
            end do !  mdx=1,Section(lect)%NMeets
          end if
          n_opts = n_opts + 1

        end do !  sect=1,NumSections
        
        call timetable_undesirability(n_opts, NumSections, Section, TimeTable)

        ! available sections that fit schedule - can be added
        tLen2 = 0
        do sect=1,n_opts
          idx_opt = UndesirabilityRank(sect)
          if (Section(NumSections+idx_opt)%RemSlots==0) cycle ! section not available
          !write(*,*) sect, idx_opt, Section(NumSections+idx_opt)%ClassId, -Undesirability(idx_opt)
          do mdx=1,Section(NumSections+idx_opt)%NMeets
            tArray(tLen1+tLen2+1) = NumSections+idx_opt
            tArray(tLen1+tLen2+2) = mdx
            tArray(tLen1+tLen2+3) = -Undesirability(idx_opt)
            tLen2 = tLen2+3
          end do !  mdx=1,Section(sect)%NMeets
        end do
        if (tLen2>0) then ! there are sections that can be added
          ! end of list markers
          tArray(tLen1+tLen2+1) = 0
          tArray(tLen1+tLen2+2) = 0
          tArray(tLen1+tLen2+3) = 0
          call list_sections_to_edit(device, Section, tLen2, tArray(tLen1+1), &
            fnChangeMatriculation, tStdNo, 'Add', allowed_to_edit, &
            '<a name="'//trim(tSubject)//'"></a><br><b>Available sections in '//trim(tSubject)// &
            ' that fit existing schedule, sorted by undesirability.</b>')
        else ! sections suitable for 'prerog'
          do sect=1,n_opts
            idx_opt = UndesirabilityRank(sect)
            if (Section(NumSections+idx_opt)%RemSlots>0) cycle ! no need to pre-rog here
            !write(*,*) sect, idx_opt, Section(NumSections+idx_opt)%ClassId, -Undesirability(idx_opt)
            do mdx=1,Section(NumSections+idx_opt)%NMeets
              tArray(tLen1+tLen2+1) = NumSections+idx_opt
              tArray(tLen1+tLen2+2) = mdx
              tArray(tLen1+tLen2+3) = -Undesirability(idx_opt)
              tLen2 = tLen2+3
            end do !  mdx=1,Section(sect)%NMeets
          end do
          ! end of list markers
          tArray(tLen1+tLen2+1) = 0
          tArray(tLen1+tLen2+2) = 0
          tArray(tLen1+tLen2+3) = 0
          call list_sections_to_edit(device, Section, tLen2, tArray(tLen1+1), fnChangeMatriculation, tStdNo, 'Prerog', &
            allowed_to_edit, '<a name="'//trim(tSubject)//'"></a><br><b>"TEACHER''S PREROGATIVE" sections in '// &
            trim(tSubject)//' that fit existing schedule, sorted by undesirability.</b>')
          write(device,AFORMAT) trim(make_href(fnScheduleOfClasses, &
              'here', A1=Department(Subject(crse)%DeptIdx)%Code, &
              pre='(The '//trim(tSubject)//' sections are ', post=')<br>', anchor=tSubject))


        end if
          
      end do !  fdx=1,Preenlisted(targetStudent)%lenSubject

    end if
    write(device,AFORMAT) '<hr>'
    return
  end subroutine enlistment_edit

 
  subroutine enlistment_fees(device, std, NumSections, Section, heading)
    integer, intent(in) :: device, std
    integer, intent (in) :: NumSections
    type (TYPE_SECTION), intent(in), dimension (0:) :: Section
    character (len=*), intent(in), optional :: heading
    integer :: idx, sect, crse
    real :: mult, totalUnits, totalA, totalB, totalC, totalD, totalE, totalGraduate, totalLabFee

    ! Tuition fees by bracket
    real, parameter :: &
      TuitionA              = 1500.0, &          
      TuitionB              = 1000.0, &         
      TuitionC              =  600.0, &          
      TuitionD              =  300.0, &          
      TuitionE              =    0.0, &          
      TuitionGraduate       = 1000.0, &         
      NSTPA                 = 2250.0, &
      NSTPB                 = 1500.0, &
      NSTPC                 =  900.0, &
      NSTPD                 =  450.0, &
      NSTPE                 =    0.0, &
      MiscellaneousA        = 2000.0, &          
      MiscellaneousB        = 2000.0, &         
      MiscellaneousC        = 2000.0, &          
      MiscellaneousD        =    0.0, &          
      MiscellaneousE        =    0.0, &          
      MiscellaneousGraduate = 1065.0, &         
      StudentFund           =   45.5, &
      EntranceFee           =   30.0, &         
      DepositFee            =  100.0, &         
      IDFee                 =  130.0, &         
      EducationDevelopment  =    0.0

    if (present(heading)) write(device,AFORMAT) heading

    write(device,AFORMAT) '<table border="0" width="100%">'//begintr, &
        thalignleft//'Subject'//endth// &
        thalignleft//'Section'//endth// &
        thalignright//'Units'//endth//&
        thalignright//'Bracket A'//endth//&
        thalignright//'Bracket B'//endth// & 
        thalignright//'Bracket C'//endth//&
        thalignright//'Bracket D'//endth// & 
        thalignright//'Bracket E'//endth// & 
        thalignright//'Graduate'//endth// & 
        thalignright//'Lab/Other'//endth//endtr

    totalUnits = 0.0
    totalA = 0.0
    totalB = 0.0
    totalC = 0.0
    totalD = 0.0
    totalE = 0.0
    totalGraduate = 0.0
    totalLabFee = 0.0
    do idx=1,Preenlisted(std)%lenSubject ! loop over all entries in Preenlisted() for student
        sect = Preenlisted(std)%Section(idx)
        if (sect==0) cycle ! not enlisted
        crse = Preenlisted(std)%Subject(idx)
        mult = Subject(crse)%Units
        totalUnits = totalUnits + mult
        write(device,AFORMAT) begintr//begintd//trim(Subject(crse)%Name)//endtd// & !  subject
          begintd//trim(Section(sect)%Code)//endtd ! code

        ! NSTP ?
        if (index(Subject(crse)%Name, 'CWTS') + index(Subject(crse)%Name, 'LTS')>0) then
                write(device,'(a,f9.1,a)') &
                  tdnbspendtd// & ! units
                  tdalignright, NSTPA, endtd, & ! NSTP A
                  tdalignright, NSTPB, endtd, & ! NSTP B
                  tdalignright, NSTPC, endtd, & ! NSTP C
                  tdalignright, NSTPD, endtd, & ! NSTP D
                  tdalignright, NSTPE, endtd, & ! NSTP E
                  tdnbspendtd ! NSTP Graduate
                totalA = totalA + NSTPA
                totalB = totalB + NSTPB
                totalC = totalC + NSTPC
                totalD = totalD + NSTPD
                totalE = totalE + NSTPE
        else ! lecture class of a lect-lab/recit subject, lecture-only, lab-only
                ! compute tuition here
                write(device,'(a,f9.1,a)') &
                  tdalignright, mult, endtd, & ! units
                  tdalignright, TuitionA*mult, endtd, & ! Tuition A
                  tdalignright, TuitionB*mult, endtd, & ! Tuition B
                  tdalignright, TuitionC*mult, endtd, & ! Tuition C
                  tdalignright, TuitionD*mult, endtd, & ! Tuition D
                  tdalignright, TuitionE*mult, endtd, & ! Tuition E
                  tdalignright, TuitionGraduate*mult, endtd ! Tuition Graduate
                totalA = totalA + TuitionA*mult
                totalB = totalB + TuitionB*mult
                totalC = totalC + TuitionC*mult
                totalD = totalD + TuitionD*mult
                totalE = totalE + TuitionE*mult
                totalGraduate = totalGraduate + TuitionGraduate*mult
        end if
        if (Subject(crse)%LabFee>0.0) then ! subject has additional fee
                write(device,'(a,f9.1,a)') &
                tdalignright, Subject(crse)%LabFee, endtd//endtr
                totalLabFee = totalLabFee + Subject(crse)%LabFee
        else
                write(device,AFORMAT) tdnbspendtd//endtr
        end if
    end do

    ! sub totals
    write(device,AFORMAT) &
      begintr//'<td colspan="10"><hr>'//endtd//endtr, &
      begintr//'<td colspan="2" align="right">Tuition Subtotal'//endtd
    write(device,'(a,f9.1,a)') &
      tdalignright, totalUnits, endtd, & ! units
      tdalignright, totalA, endtd, & ! Total A
      tdalignright, totalB, endtd, & ! Total B
      tdalignright, totalC, endtd, & ! Total C
      tdalignright, totalD, endtd, & ! Total D
      tdalignright, totalE, endtd, & ! Total E
      tdalignright, totalGraduate, endtd, & ! Total Graduate
      tdalignright, totalLabFee, endtd//endtr, & ! Total Lab
      begintr//'<td colspan="10"><hr>'//endtd//endtr

    ! miscellaneous
    write(device,AFORMAT) begintr//'<td colspan="2" align="right">Miscellaneous'//endtd//tdnbspendtd
    write(device,'(a,f9.1,a)') &
      tdalignright, MiscellaneousA, endtd, & ! Misc A
      tdalignright, MiscellaneousB, endtd, & ! Misc B
      tdalignright, MiscellaneousC, endtd, & ! Misc C
      tdalignright, MiscellaneousD, endtd, & ! Misc D
      tdalignright, MiscellaneousE, endtd, & ! Misc E
      tdalignright, MiscellaneousGraduate, endtd, & ! Misc Graduate
      tdnbspendtd//endtr ! Lab
    totalA = totalA + MiscellaneousA
    totalB = totalB + MiscellaneousB
    totalC = totalC + MiscellaneousC
    totalD = totalD + MiscellaneousD
    totalE = totalE + MiscellaneousE
    totalGraduate = totalGraduate + MiscellaneousGraduate
    ! StudentFund
    write(device,AFORMAT) begintr//'<td colspan="2" align="right">Student Fund'//endtd//tdnbspendtd
    write(device,'(a,f9.1,a)') &
      tdalignright, StudentFund, endtd, & ! A
      tdalignright, StudentFund, endtd, & ! B
      tdalignright, StudentFund, endtd, & ! C
      tdalignright, StudentFund, endtd, & ! D
      tdalignright, StudentFund, endtd, & ! E
      tdalignright, StudentFund, endtd, & ! Graduate
      tdnbspendtd//endtr ! Lab
    totalA = totalA + StudentFund   
    totalB = totalB + StudentFund   
    totalC = totalC + StudentFund   
    totalD = totalD + StudentFund   
    totalE = totalE + StudentFund   
    totalGraduate = totalGraduate + StudentFund
    ! EntranceFee
    write(device,AFORMAT) begintr//'<td colspan="2" align="right">Entrance Fee'//endtd//tdnbspendtd
    write(device,'(a,f9.1,a)') &
      tdalignright, EntranceFee, endtd, & ! A
      tdalignright, EntranceFee, endtd, & ! B
      tdalignright, EntranceFee, endtd, & ! C
      tdalignright, EntranceFee, endtd, & ! D
      tdalignright, EntranceFee, endtd, & ! E
      tdalignright, EntranceFee, endtd, & ! Graduate
      tdnbspendtd//endtr ! Lab
    totalA = totalA + EntranceFee   
    totalB = totalB + EntranceFee   
    totalC = totalC + EntranceFee   
    totalD = totalD + EntranceFee   
    totalE = totalE + EntranceFee   
    totalGraduate = totalGraduate + EntranceFee
    ! DepositFee
    write(device,AFORMAT) begintr//'<td colspan="2" align="right">Deposit'//endtd//tdnbspendtd
    write(device,'(a,f9.1,a)') &
      tdalignright, DepositFee, endtd, & ! A
      tdalignright, DepositFee, endtd, & ! B
      tdalignright, DepositFee, endtd, & ! C
      tdalignright, DepositFee, endtd, & ! D
      tdalignright, DepositFee, endtd, & ! E
      tdalignright, DepositFee, endtd, & ! Graduate
      tdnbspendtd//endtr ! Lab
    totalA = totalA + DepositFee   
    totalB = totalB + DepositFee   
    totalC = totalC + DepositFee   
    totalD = totalD + DepositFee   
    totalE = totalE + DepositFee   
    totalGraduate = totalGraduate + DepositFee
    ! IDFee
    write(device,AFORMAT) begintr//'<td colspan="2" align="right">ID Fee'//endtd//tdnbspendtd
    write(device,'(a,f9.1,a)') &
      tdalignright, IDFee, endtd, & ! A
      tdalignright, IDFee, endtd, & ! B
      tdalignright, IDFee, endtd, & ! C
      tdalignright, IDFee, endtd, & ! D
      tdalignright, IDFee, endtd, & ! E
      tdalignright, IDFee, endtd, & ! Graduate
      tdnbspendtd//endtr ! Lab
    totalA = totalA + IDFee   
    totalB = totalB + IDFee   
    totalC = totalC + IDFee   
    totalD = totalD + IDFee   
    totalE = totalE + IDFee   
    totalGraduate = totalGraduate + IDFee

    ! EducationDevelopment
    write(device,AFORMAT) begintr//'<td colspan="2" align="right">Education Development'//endtd//tdnbspendtd
    write(device,'(a,f9.1,a)') &
    '<td colspan="6">'//nbsp//endtd//tdalignright, EducationDevelopment, endtd//endtr ! Lab
    totalLabFee = totalLabFee+ EducationDevelopment

    ! totals
    write(device,AFORMAT) &
      begintr//'<td colspan="10"><hr>'//endtd//endtr, &
      begintr//'<th colspan="3" align="right">TOTAL'//endth
    write(device,'(a,f9.1,a)') &
      thalignright, totalA, endth, & ! Total A
      thalignright, totalB, endth, & ! Total B
      thalignright, totalC, endth, & ! Total C
      thalignright, totalD, endth, & ! Total D
      thalignright, totalE, endth, & ! Total E
      thalignright, totalGraduate, endth, & ! Total Graduate
      thalignright, totalLabFee, endth//endtr ! Total Lab

    write(device,AFORMAT) '</table>'

    return
  end subroutine enlistment_fees


  subroutine enlistment_printable  (device, NumSections, Section)
    integer, intent (in) :: device
    integer, intent (in out) :: NumSections
    type (TYPE_SECTION), intent(in out), dimension (0:) :: Section
    character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
    integer ::  ierr, tLen1
    integer, dimension(60,6) :: TimeTable
    logical :: conflicted

    ! which student?
    call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
    targetStudent = index_to_student(tStdNo)
    if (ierr/=0 .or. targetStudent==0) then
            targetDepartment = DeptIdxUser
            targetCollege = CollegeIdxUser
            call html_write_header(device, 'Pre-registration form', '<hr><br>student "'//tStdNo//'" not found')
            return
    end if
    targetCurriculum = Student(targetStudent)%CurriculumIdx

    call timetable_meetings_of_student(NumSections, Section, targetStudent, Preenlisted, 0, tLen1, tArray, TimeTable, conflicted)
    call enlistment_class_schedule(device, targetStudent, NumSections, Section, tLen1, tArray)
    !call timetable_display(device, Section, TimeTable)

    write(device,AFORMAT) '<hr>'
    return
  end subroutine enlistment_printable  


  subroutine enlistment_find_block(device, NumSections, Section, NumBlocks, Block)
    integer, intent (in) :: device
    integer, intent (in out) :: NumBlocks, NumSections
    type (TYPE_SECTION), intent(in out), dimension (0:) :: Section
    type (TYPE_BLOCK), dimension(0:), intent(in out) :: Block
    character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
    integer :: ierr, crse, sect, seats
    integer :: bdx, fdx, blk, n_blks, n_matches
    logical :: matched
    !character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
    !character(len=3*MAX_LEN_SUBJECT_CODE) :: tAction, search_string
    !character(len=MAX_LEN_SUBJECT_CODE) :: tSubject
    !character(len=127) :: mesg
    !logical :: allowed_to_edit


    ! which student?
    call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
    targetStudent = index_to_student(tStdNo)
    if (ierr/=0 .or. targetStudent==0) then
            targetDepartment = DeptIdxUser
            targetCollege = CollegeIdxUser
            call html_write_header(device, 'Find block', '<hr><br>student "'//tStdNo//'" not found')
            return
    end if

    targetCurriculum = Student(targetStudent)%CurriculumIdx
    targetCollege = Curriculum(targetCurriculum)%CollegeIdx
    !tDepartment = College(targetCollege)%Code
    !targetDepartment = index_to_dept(tDepartment)
    !allowed_to_edit = isRoleAdmin .or. (isRoleSRE .and. CurrProgCode(CurriculumIdxUser)==CurrProgCode(targetCurriculum))

    call recalculate_available_seats(Section)

    call html_write_header(device, trim(Student(targetStudent)%StdNo)//SPACE//trim(Student(targetStudent)%Name)// &
      SPACE//DASH//SPACE//trim(Curriculum(targetCurriculum)%Code))

    write(device,AFORMAT) '<table border="0" width="100%">'//begintr, &
        thalignleft//'Block ID'//endth// &
        thalignleft//'# Matches'//endth// &
        thalignleft//'Class ID (seats)'//endth//endtr

    n_blks = 0
    do blk=1,NumBlocks
      if (CurrProgCode(Block(blk)%CurriculumIdx)/=CurrProgCode(targetCurriculum)) cycle ! not this curriculum
      ! count how many subjects in this block match the predicted subjects of the student
      n_matches = 0
      do bdx=1,Block(blk)%NumClasses
        crse = Block(blk)%Subject(bdx)
        do fdx=1,Preenlisted(targetStudent)%lenSubject
          if (crse/=Preenlisted(targetStudent)%Subject(fdx)) cycle
          n_matches = n_matches+1
          exit
        end do
      end do
      if (n_matches==0) cycle
      n_blks = n_blks + 1
      !write(device,'(a,3(i3,a))') '<br>'//Block(blk)%BlockID//' :', &
      !  n_matches, '/'(blk)%NumClasses, ' matches, from ', &
      !  Preenlisted(targetStudent)%lenSubject, ' feasible subjects of '//Student(targetStudent)%StdNo
      write(device,AFORMAT) begintr// &
        begintd//trim(make_href(fnBlockSchedule, Block(blk)%BlockID, A1=Block(blk)%BlockID))//endtd// &
        begintd//trim(itoa(n_matches))//SPACE//FSLASH//SPACE//trim(itoa(Block(blk)%NumClasses))//endtd// &
        begintd
      matched = .false.
      do bdx=1,Block(blk)%NumClasses
        crse = Block(blk)%Subject(bdx)
        do fdx=1,Preenlisted(targetStudent)%lenSubject
          if (crse/=Preenlisted(targetStudent)%Subject(fdx)) cycle
          matched = .true.
          exit
        end do
        if (.not. matched) cycle
        sect = Block(blk)%Section(bdx)
        if (sect/=0) then
                seats = Section(sect)%RemSlots
                if (seats>0) then
                        write(device,AFORMAT) green//trim(Section(sect)%ClassId)//' ('// &
                          trim(itoa(seats))//') '//black//' /'//nbsp
                else
                        write(device,AFORMAT) green//trim(Section(sect)%ClassId)//black//red//' ('// &
                          trim(itoa(seats))//') '//black//' /'//nbsp
                end if
        else
                write(device,AFORMAT) red//trim(Subject(crse)%Name)//black//' /'//nbsp
        end if
        if (mod(bdx,4)==0) then
                write(device,AFORMAT) endtd//endtr// & ! end row
                  begintr//tdnbspendtd//tdnbspendtd//begintd ! new row with first 2 columns empty
        end if
      end do
      write(device,AFORMAT) trim(make_href(fnChangeMatriculation, 'Enlist', A1=tStdNo, A2='Block', &
        A3=Block(blk)%BlockID, pre=nbsp, post=endtd//endtr))
    end do
    if (n_blks==0) then
            write(device,AFORMAT) begintr//'<td colspan="3">No suitable blocks for this student?'//endtd//endtr
    end if
    write(device,AFORMAT) '</table>', &
      '<br><i>NOTES</i> : A '//green//'Class ID (seats)'//black//' is open. A '//green//'Class ID '//black// &
      red//'(0)'//black//' is NOT available. A '//red//'SUBJECT'//black//' is NOT assigned to a section.' // &
      ' "Enlist" will enlist the student ONLY in the open sections of a block.', &
      '<hr>'
    return
  end subroutine enlistment_find_block


  subroutine enlistment_class_schedule(device, std, NumSections, Section, lenSL, SectionList)
    integer, intent(in) :: device, std, NumSections, lenSL, SectionList(3*lenSL+3)
    type (TYPE_SECTION), intent(in), dimension (0:) :: Section
    integer :: crse, idx, mdx, sect, previous, conflict
    real :: totalUnits, classUnits, totalHours, classHours, totalTuition, classTuition, totalLabFee, classLabFee

    write(device,AFORMAT) '<b>'//trim(Student(std)%StdNo)//SPACE//trim(Student(std)%Name)// &
      SPACE//DASH//SPACE//trim(Curriculum(Student(std)%CurriculumIdx)%Code)//'<br>'// &
      trim(txtSemester(currentTerm+3))//' Term, SY '//trim(itoa(currentYear))//DASH// &
      trim(itoa(currentYear+1))//'</b><hr>'

    if (lenSL < 3) then
      write(device,AFORMAT) '<br>Not pre-registered in any class?<br>'
      return
    end if

    totalUnits = 0.0
    totalHours = 0.0
    totalTuition = 0.0
    totalLabFee = 0.0

    write(device,AFORMAT) '<table border="0" width="100%">'//begintr, &
        thalignleft//'Subject'//endth// &
        thalignleft//'Section'//endth//&
        thalignleft//'Units'//endth// &
        thalignleft//'Hours'//endth// &
        thalignleft//'Tuition'//endth// &
        thalignleft//'Lab Fee'//endth// &
        thalignleft//'Time'//endth// &
        thalignleft//'Day'//endth//&
        thalignleft//'Room'//endth//endtr

    previous = 0
    do idx=1,lenSL,3
        sect=SectionList(idx)
        !mdx=SectionList(idx+1)
        conflict=SectionList(idx+2)
        crse = Section(sect)%SubjectIdx
        
        !new section ?
        if (sect/=previous) then ! include subject, section, units/blockname, seats/hours, time, day

            if (is_lecture_lab_subject(crse)) then
                if (is_lecture_class(sect, Section)) then ! lecture of lecture-lab
                        classUnits = Subject(crse)%Units
                        classHours = Subject(crse)%LectHours
                        classTuition = Subject(crse)%Tuition
                        classLabFee = 0.0 
                else ! lab of lecture-lab
                        classUnits = 0.0
                        classHours = Subject(crse)%LabHours
                        classTuition = 0.0 
                        classLabFee = Subject(crse)%LabFee
                end if
            else if (Subject(crse)%LectHours>0.0) then ! lecture-only
                        classUnits = Subject(crse)%Units
                        classHours = Subject(crse)%LectHours
                        classTuition = Subject(crse)%Tuition
                        classLabFee = 0.0 
            else if (Subject(crse)%LabHours>0.0) then ! lab-only
                        classUnits = Subject(crse)%Units
                        classHours = Subject(crse)%LabHours
                        classTuition = Subject(crse)%Tuition
                        classLabFee = Subject(crse)%LabFee
            end if

            totalHours = totalHours + classHours
            totalUnits = totalUnits + classUnits
            totalTuition = totalTuition + classTuition
            totalLabFee = totalLabFee + classLabFee


            previous = sect
            write(device,AFORMAT) &
              begintr//begintd//trim(Subject(crse)%Name)//endtd, & !  subject
              begintd//trim(Section(sect)%Code)//endtd ! code

            if (classUnits>0.0) then
              write(device,AFORMAT) begintd//trim(ftoa(classUnits,1))//endtd
            else
              write(device,AFORMAT) tdnbspendtd ! units
            end if

            if (classHours>0.0) then
              write(device,AFORMAT) begintd//trim(ftoa(classHours,2))//endtd ! hours
            else
              write(device,AFORMAT) tdnbspendtd ! hours
            end if

            if (classTuition>0.0) then
              write(device,AFORMAT) begintd//trim(ftoa(classTuition,2))//endtd
            else
              write(device,AFORMAT) tdnbspendtd
            end if

            if (classLabFee>0.0) then
              write(device,AFORMAT) begintd//trim(ftoa(classLabFee,2))//endtd
            else
              write(device,AFORMAT) tdnbspendtd 
            end if

!
!-------------------------------------------------

            if (is_regular_schedule(sect, Section)) then
                write(device,AFORMAT) &
                  begintd//trim(text_time_period(Section(sect)%bTimeIdx(1), Section(sect)%eTimeIdx(1)))//endtd// &
                  begintd//trim(text_days_of_section(sect, NumSections, Section))//endtd// &
                  begintd//trim(Room(Section(sect)%RoomIdx(1))%Code)//endtd//endtr
            else
                mdx = 1
                write(device,AFORMAT) &
                  begintd//trim(text_time_period(Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)))//endtd// &
                  begintd//trim(txtDay(Section(sect)%DayIdx(mdx)))//endtd// &
                  begintd//trim(Room(Section(sect)%RoomIdx(mdx))%Code)//endtd//endtr
                do mdx=2,Section(sect)%NMeets
                write(device,AFORMAT) begintr//'<td colspan="6">'//nbsp//endtd// &
                    begintd//trim(text_time_period(Section(sect)%bTimeIdx(mdx), Section(sect)%eTimeIdx(mdx)))//endtd// &
                    begintd//trim(txtDay(Section(sect)%DayIdx(mdx)))//endtd// &
                    begintd//trim(Room(Section(sect)%RoomIdx(mdx))%Code)//endtd//endtr
                end do
            end if
            if (conflict>0) write(device,AFORMAT) &
                begintr//'<td align="center" colspan="9">'//red//'CONFLICT between '//trim(Section(sect)%ClassId)//' and '// &
                trim(Section(conflict)%ClassId)//black//endtd//endtr

        end if

    end do
    write(device,AFORMAT) begintr//'<td colspan="9"><hr>'//endtd//endtr, &
        begintr//tdnbspendtd//begintd//'<b>Totals</b> : '//endtd// & ! code
        begintd//trim(ftoa(totalUnits,1))//endtd//begintd//trim(ftoa(totalHours,2))//endtd// & ! hours
        begintd//trim(ftoa(totalTuition,2))//endtd//begintd//trim(ftoa(totalLabFee,2))//endtd// & ! fees
        tdnbspendtd// tdnbspendtd// tdnbspendtd//endtr, &
        '</table>'

    return
  end subroutine enlistment_class_schedule


    subroutine timetable_meetings_of_student(NumSections, Section, iStd, eList, to_skip, &
    len_list, list, TimeTable, conflicted)
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section
        type (TYPE_PRE_ENLISTMENT), dimension(0:), intent(in) :: eList
        integer, intent(in) :: iStd, to_skip
        integer, intent (out) :: len_list, list(:)
        integer, dimension(60,6), intent (out) :: TimeTable
        logical, intent(out) :: conflicted
        integer :: i, j, conflict_loc, sdx, sect, crse, lect
        integer :: meetings(MAX_SECTION_MEETINGS)
        character(len=MAX_LEN_CLASS_ID) :: tClassId

        len_list = 0 ! initialize list
        call timetable_clear(TimeTable) ! initialize weekly schedule
        conflicted = .false.
        do sdx=1,eList(iStd)%NPriority+eList(iStd)%NAlternates+eList(iStd)%NCurrent ! loop over enries in eList()
            sect = eList(iStd)%Section(sdx)
            if (sect==0) cycle ! not accommodated
            if (sect==to_skip) cycle ! skip specified section (if, for example, the section is being edited)
            crse = Section(sect)%SubjectIdx

            if (is_lecture_lab_subject(crse)) then ! subject is lecture-lab
                ! add lecture
                j = index(Section(sect)%Code,DASH)
                tClassId = trim(Subject(crse)%Name)//SPACE//Section(sect)%Code(:j-1)
                lect = index_to_section(tClassId, NumSections, Section)
                do i=1,Section(lect)%NMeets
                    meetings(1) = i
                    list(len_list+1) = lect
                    list(len_list+2) = i
                    conflict_loc = -10 ! assume no conflicting section
                    call timetable_add_meetings_of_section(NumSections, Section, lect, 1, meetings, TimeTable, conflict_loc) ! add meeting to weekly schedule
                    if (conflict_loc/=-10) then
                        conflicted = .true.
                    else
                        conflict_loc = 0
                    end if
                    list(len_list+3) = conflict_loc ! index to conflicting section, if any
                    len_list = len_list+3
                end do
            end if
            ! add section
            do i=1,Section(sect)%NMeets
                meetings(1) = i
                list(len_list+1) = sect
                list(len_list+2) = i
                conflict_loc = -10 ! assume no conflicting section
                call timetable_add_meetings_of_section(NumSections, Section, sect, 1, meetings, TimeTable, conflict_loc) ! add meeting to weekly schedule
                if (conflict_loc/=-10) then
                    conflicted = .true.
                else
                    conflict_loc = 0
                end if
                list(len_list+3) = conflict_loc ! index to conflicting section, if any
                len_list = len_list+3
            end do
        end do
        ! end markers
        list(len_list+1) = 0
        list(len_list+2) = 0
        list(len_list+3) = 0
        return
    end subroutine timetable_meetings_of_student


end module EditENLISTMENT
