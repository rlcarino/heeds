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


module EditBLOCKS  

    use DisplayBLOCKS

    implicit none

contains


    subroutine edit_block(device, NumSections, Section, Offering, NumBlocks, Block, fn)
        integer, intent (in) :: device, fn
        integer, intent (in out) :: NumBlocks, NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_OFFERED_SUBJECTS), intent(in out) :: Offering(MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS)
        type (TYPE_BLOCK), intent(in out) :: Block(0:)
        integer :: tLen1, fdx, ierr, blk, jdx, Term! idx, kdx
        integer ::  crse, sect, pos, lect, repl
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject, input_name1, input_name2, input_value
        character(len=MAX_LEN_CLASS_ID) :: tClassId, tAction
        character(len=MAX_LEN_BLOCK_CODE) :: tBlock, newBlock
        logical :: allowed_to_edit, updateBLOCKS, updateCLASSES
        character (len=127) :: mesg

        Term = targetTerm

        ! which block?
        call cgi_get_named_string(QUERY_STRING, 'A1', tBlock, ierr)
        targetBlock = index_to_block(tBlock, NumBlocks, Block)

        tBlock = Block(targetBlock)%BlockID
        targetCollege = Curriculum(Block(targetBlock)%CurriculumIdx)%CollegeIdx
        targetDepartment = Block(targetBlock)%DeptIdx

#if defined UPLB
        ! Subject administered by departments
        allowed_to_edit = isRoleAdmin .or. & ! USER is the ADMINISTRATOR
            (isRoleChair .and. targetDepartment==DeptIdxUser) ! USER is Chair, and Department is the same as that of the Block
#else
        ! Subjects administered by program
        allowed_to_edit = isRoleAdmin .or. & ! USER is the ADMINISTRATOR
            (isRoleChair .and. targetCollege==CollegeIdxUser ) ! USER is Dean, and College is the same as that of the Block
#endif

        mesg = SPACE
        updateBLOCKS = .false.
        updateCLASSES = .false.

        select case (fn)

            case (fnBlockNewAdd)
                updateBLOCKS = .true.
                mesg = 'Added new block '//tBlock

            case (fnBlockEditSubject)

                call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)

                tLen1 = 0 ! no. of cjanges
                if (index(tAction, 'Replace')==1) then

                    ! look for REPL:input_name2=input_value
                    input_name1 = 'REPL:'
                    do while (.true.)
                        call cgi_get_wild_name_value(QUERY_STRING, input_name1, input_name2, input_value, ierr)
                        if (ierr/=0) exit ! no more REPL: in QUERY
                        call html_comment('Replace '//input_name2//' with '//input_value)

                        call underscore_to_blank(input_name2, tSubject)
                        crse = index_to_subject(tSubject) ! current subject in list

                        if (input_value==SPACE) then ! course was erased

                            ! delete crse and assigned section (if any)
                            do fdx=1,Block(targetBlock)%NumClasses
                                if (Block(targetBlock)%Subject(fdx)==crse) then
                                    Block(targetBlock)%Subject(fdx) = 0
                                    Block(targetBlock)%Section(fdx) = 0
                                    mesg = ' : Removed '//trim(tSubject)//mesg
                                    call html_comment('Removed '//tSubject)
                                    tLen1 = tLen1+1
                                    exit
                                end if
                            end do

                            ! get next REPL:
                            cycle

                        end if

                        ! replacement subject
                        repl = index_to_subject(input_value)
                        if (crse==repl) then ! no change
                            call html_comment('No change for '//tSubject)

                            ! get next REPL:
                            cycle

                        end if

                        ! find crse, replace with repl, delete section
                        do fdx=1,Block(targetBlock)%NumClasses
                            if (Block(targetBlock)%Subject(fdx)==crse) then
                                Block(targetBlock)%Subject(fdx) = repl
                                Block(targetBlock)%Section(fdx) = 0
                                mesg = ' : Replaced '//trim(tSubject)//' with '//trim(input_value)//mesg
                                call html_comment('Replaced '//tSubject//' with '//input_value)
                                tLen1 = tLen1+1
                                exit
                            end if
                        end do

                    end do

                else if (index(tAction, 'Add, DO NOT')==1) then

                    ! add subject (only) to block, do not make a new class
                    call cgi_get_named_string(QUERY_STRING, 'add', input_value, ierr)
                    if (input_value/=SPACE) then
                        crse = index_to_subject(input_value) ! subject to add
                        if (crse>0) then
                            fdx = 1+Block(targetBlock)%NumClasses
                            Block(targetBlock)%NumClasses = fdx
                            Block(targetBlock)%Subject(fdx) = crse
                            Block(targetBlock)%Section(fdx) = 0
                            mesg = ' : Added '//trim(input_value)//mesg
                            call html_comment('Added '//input_value)
                            tLen1 = tLen1+1
                        else
                            mesg = ' : ERROR - NOT added '//trim(input_value)//mesg
                        end if
                    end if

                else if (index(tAction, 'Add, create')==1) then

                    ! add course to block, create TBA class
                    call cgi_get_named_string(QUERY_STRING, 'add', input_value, ierr)
                    if (input_value/=SPACE) then
                        crse = index_to_subject(input_value) ! subject to add
                        if (crse>0) then
                            fdx = 1+Block(targetBlock)%NumClasses
                            Block(targetBlock)%NumClasses = fdx
                            Block(targetBlock)%Subject(fdx) = crse
                            call create_section_for_block (crse, targetBlock, Term, NumBlocks, Block,  NumSections, Section)
                            Block(targetBlock)%Section(fdx) = NumSections
                            mesg = ' : Added '//trim(Section(NumSections)%ClassId)//mesg
                            call html_comment('Added '//Section(NumSections)%ClassId)
                            tLen1 = tLen1+1
                            updateCLASSES = .true.
                        else
                            mesg = ' : ERROR - NOT added '//trim(input_value)//mesg
                        end if
                    end if

                end if


                if (tLen1==0) then
                    mesg = ' : Nothing to update?'
                    call html_comment('Nothing to update in '//Block(targetBlock)%BlockID)
                else
                    ! ensure (last+1) is empty
                    jdx = Block(targetBlock)%NumClasses+1
                    Block(targetBlock)%Subject(jdx) = 0
                    Block(targetBlock)%Section(jdx) = 0
                    ! compress in case a subject was deleted
                    fdx = 1
                    do while (fdx<=Block(targetBlock)%NumClasses)
                        if (Block(targetBlock)%Subject(fdx)==0) then
                            ! shift left
                            do jdx=fdx,Block(targetBlock)%NumClasses
                                Block(targetBlock)%Subject(jdx) = Block(targetBlock)%Subject(jdx+1)
                                Block(targetBlock)%Section(jdx) = Block(targetBlock)%Section(jdx+1)
                            end do
                            Block(targetBlock)%NumClasses = Block(targetBlock)%NumClasses-1
                        end if
                        fdx = fdx+1
                    end do
                    updateBLOCKS = .true.
                end if

            case (fnBlockDeleteNotClasses)
                ! delete block
                call initialize_block(Block(targetBlock))
                updateBLOCKS = .true.
                mesg ='Deleted block '//trim(tBlock)//', kept sections (if any)'

            case (fnBlockDeleteIncludingClasses)
                ! delete sections
                do fdx=1,Block(targetBlock)%NumClasses
                    sect = Block(targetBlock)%Section(fdx)
                    if (sect>0) then
                        crse = Block(targetBlock)%Subject(fdx)
                        if (is_lecture_lab_subject(crse)) then ! count how many lab sections
                            pos = index(Section(sect)%ClassId, DASH)
                            tClassId = Section(sect)%ClassId(:pos)
                            tLen1 = 0
                            do jdx=1,NumSections
                                if (Section(jdx)%ClassId(:pos)==tClassId(:pos)) tLen1 = tLen1+1
                            end do
                            if (tLen1==1) then ! just this lab section; delete lecture section also
                                tClassId = Section(sect)%ClassId(:pos-1)
                                lect = index_to_section(tClassId, NumSections, Section)
                                call initialize_section(Section(Lect))
                            end if
                        end if
                        call initialize_section(Section(sect))
                        updateCLASSES = .true.
                    end if
                end do
                ! delete block
                call initialize_block(Block(targetBlock))
                updateBLOCKS = .true.
                mesg = 'Deleted block '//trim(tBlock)//' and sections (if any)'

            case (fnBlockEditName)
                call cgi_get_named_string(QUERY_STRING, 'BlockID', newBlock, ierr)
                if (newBlock==SPACE .or. ierr/=0) then
                    mesg = 'New block name not specified.'
                else if (tBlock==newBlock) then
                    mesg = 'Block name not changed.'
                else
                    blk = index_to_block(newBlock, NumBlocks, Block)
                    if (blk>0) then
                        mesg = 'Block name '//trim(newBlock)//' already in use.'
                    else
                        mesg = 'Block '//trim(tBlock)//' renamed to '//newBlock
                        updateBLOCKS = .true.
                        Block(targetBlock)%BlockID = newBlock
                        tBlock = newBlock
                    end if
                end if

            case (fnBlockCopy)
                call cgi_get_named_string(QUERY_STRING, 'BlockID', newBlock, ierr)
                if (newBlock==SPACE .or. ierr/=0) then
                    mesg = 'Name for copy of block not specified.'
                else if (tBlock==newBlock) then
                    mesg = 'Name for copy of block should be different from '//trim(tBlock)//'.'
                else
                    blk = index_to_block(newBlock, NumBlocks, Block)
                    if (blk>0) then
                        mesg = 'Block name '//trim(newBlock)//' already in use.'
                    else ! copy Block(targetBlock) to end
                        NumBlocks = NumBlocks+1
                        Block(NumBlocks) = Block(targetBlock)
                        ! make new block the target
                        targetBlock = NumBlocks
                        Block(targetBlock)%BlockID = newBlock

                        ! initialize for new sections
                        do fdx=1,Block(targetBlock)%NumClasses
                            Block(targetBlock)%Section(fdx) = 0
                        end do
                        ! create new sections
                        call block_add_and_create_sections (targetBlock, Term, &
                            NumBlocks, Block,  NumSections, Section)

                        updateBLOCKS = .true.
                        updateCLASSES = .true.
                        mesg = 'Block '//trim(tBlock)//' copied to '//trim(newBlock)// &
                            ', new sections added'

                        tBlock = newBlock

                    end if
                end if

            case (fnBlockEditSection)

                ! check for other arguments
                call cgi_get_named_string(QUERY_STRING, 'A2', tAction, ierr)
                select case (trim(tAction))

                    case ('Add')
                        call cgi_get_named_string(QUERY_STRING, 'A3', tClassId, ierr)
                        sect = index_to_section(tClassId, NumSections, Section)
                        if (sect>0) then ! target of action is indexed by sect
                            crse = Section(sect)%SubjectIdx
                            do fdx=1,Block(targetBlock)%NumClasses
                                if (Block(targetBlock)%Subject(fdx)==crse) then
                                    Block(targetBlock)%Section(fdx) = sect
                                    mesg = 'Added '//tClassId
                                    updateBLOCKS = .true.
                                    updateCLASSES = .true.
                                    call html_comment('Added '//trim(tClassId)//' to '//Block(targetBlock)%BlockID)
                                    exit
                                end if
                            end do
                        end if

                    case ('Del')
                        call cgi_get_named_string(QUERY_STRING, 'A3', tClassId, ierr)
                        sect = index_to_section(tClassId, NumSections, Section)
                        if (sect>0) then ! target of action is indexed by sect
                            do fdx=1,Block(targetBlock)%NumClasses
                                if (sect==Block(targetBlock)%Section(fdx)) then
                                    Block(targetBlock)%Section(fdx) = 0
                                    !Section(sect)%BlockID = SPACE
                                    mesg = 'Deleted '//tClassId
                                    updateBLOCKS = .true.
                                    updateCLASSES = .true.
                                    call html_comment('Removed '//trim(tClassId)//' from '//Block(targetBlock)%BlockID)
                                    exit
                                end if
                            end do
                        end if

                end select

        end select

        if (updateBLOCKS) then
            call xml_write_blocks(pathToTerm, NumBlocks, Block,  Section, 0)
        end if
        if (updateCLASSES) then
            call offerings_summarize(NumSections, Section, Offering)
            call xml_write_sections(pathToTerm, NumSections, Section, 0)
            call count_sections_by_dept(Term, NumSections, Section)
        end if

        if (fn==fnBlockDeleteIncludingClasses .or. fn==fnBlockDeleteNotClasses) then
            call html_college_links(device, targetCollege, mesg)
        else
            blk = targetBlock
            call block_schedule(device, NumSections, Section, Offering, NumBlocks, Block, blk, mesg)
        end if

    end subroutine edit_block


    subroutine block_select_curriculum_year(device, NumSections, Section, NumBlocks, Block)
        integer, intent (in) :: device
        integer, intent (in out) :: NumBlocks, NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_BLOCK), intent(in out) :: Block(0:)
        integer :: ierr, ldx
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege

        call html_comment('block_select_curriculum_year()')

        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        targetCollege = index_to_college(tCollege)
        call html_write_header(device, 'Add block(s) in '//tCollege)
        ierr = 0
        do ldx=1,NumCurricula-1
            if (.not. Curriculum(ldx)%Active .or. Curriculum(ldx)%NumTerms==0) cycle
            if (targetCollege/=Curriculum(ldx)%CollegeIdx) cycle
            ierr = ierr+1
            exit
        end do
        if (ierr==0) then
            write(device,AFORMAT) '( No curricular programs in '//tCollege//'?)<hr>'
            return
        end if

        ! add block
        call make_form_start(device, fnBlockNewAdd)
        write(device,AFORMAT) '<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
            begintr//begintd//'Curriculum:'//endtd//begintd//'<select name="A1">', &
            '<option value=""> (select curriculum)'
        do ldx=1,NumCurricula-1
            if (.not. Curriculum(ldx)%Active) cycle
            if (targetCollege/=Curriculum(ldx)%CollegeIdx) cycle
            write(device,AFORMAT) '<option value="'//trim(Curriculum(ldx)%Code)//'"> '// &
                trim(Curriculum(ldx)%Code)//' - '//trim(Curriculum(ldx)%Title)
        end do
        write(device,AFORMAT) '</select>'//endtd//endtr, &
            begintr//begintd//'Year level:'//endtd//begintd//'<select name="A2">', &
            '<option value="ALL"> All years'
        do ldx=1,7
            write(device,AFORMAT) '<option value="'//trim(txtYear(ldx))//'"> '//trim(txtYear(ldx+9))//' Year'
        end do
        write(device,AFORMAT) '</select>'//nbsp//txtSemester(targetTerm+3)//' Term'//endtd//endtr, &
            begintr//begintd//'Number of blocks:'//endtd//begintd, &
            '<input name="A3" size="2" value="1">', &
            endtd//endtr//'</table>', &
            '<br><b>Add block</b> '//nbsp//nbsp//'<input type="submit" name="action" value="Do NOT create sections">', &
            nbsp//nbsp//nbsp//nbsp//'<input type="submit" name="action" value="Create TBA sections">', &
            '</form><hr>'

    end subroutine block_select_curriculum_year


    subroutine block_add(device, Term, NumSections, Section, Offering, NumBlocks, Block)
        integer, intent(in) :: Term, device
        integer, intent(in out) :: NumBlocks, NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_BLOCK), intent(in out) :: Block(0:)
        type (TYPE_OFFERED_SUBJECTS), intent(in out), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS) :: Offering
        integer :: ierr, blk, copy, crse, idx, jdx, ncopies, Rank, Year, YearFirst, YearLast
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character (len=MAX_LEN_BLOCK_CODE) :: tBlock
        character (len=MAX_LEN_TEXT_YEAR) :: tYear
        character (len=127) :: mesg
        character (len=50) :: tAction
        logical :: inputError, createClasses
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        call html_comment('block_add()')

        inputError = .false.

        ! curriculum
        call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
        targetCurriculum = index_to_curriculum(tCurriculum)
        if (ierr/=0 .or. targetCurriculum<=0) then
            mesg = 'Curriculum not specified, or not found.'
            inputError = .true.
        else

            ! year
            call cgi_get_named_string(QUERY_STRING, 'A2', tYear, ierr)
            if (tYear=='ALL') then
                YearFirst = 1
                YearLast = (Curriculum(targetCurriculum)%NumTerms+2)/3
            else
                YearFirst = index_to_year(tYear)
                YearLast = YearFirst
                if (ierr/=0 .or. YearFirst==0 .or. ((YearFirst-1)*3+Term>Curriculum(targetCurriculum)%NumTerms) ) then
                    mesg = 'Year "'//tYear//'" not valid.'
                    inputError = .true.
                end if
            end if

        end if

        ! no of. copies
        call cgi_get_named_integer(QUERY_STRING, 'A3', ncopies, ierr)
        if (ierr/=0 .or. ncopies<=0) ncopies = 1

        if (inputError) then
            targetDepartment = DeptIdxUser
            targetCollege = CollegeIdxUser
            call html_write_header(device, 'Add block', '<hr><br>'//mesg)
            return
        end if

        ! action
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
        createClasses = index(tAction, ' TBA ')>0

        targetCollege = Curriculum(targetCurriculum)%CollegeIdx

#if defined UPLB
        ! Subject administered by departments
        targetDepartment = DeptIdxUser
        tDepartment =  Department(targetDepartment)%Code
#else
        ! Subjects administered by program
        tDepartment = College(targetCollege)%Code
        targetDepartment = index_to_dept(tDepartment)
#endif

        do Year=YearFirst,YearLast

            Rank = (Year-1)*3+Term

            do copy=1,ncopies

                ! the block
                if (ncopies>1) then
                    tBlock = trim(Curriculum(targetCurriculum)%Code)//DASH//trim(itoa(Year))//'A'
                else
                    tBlock = trim(Curriculum(targetCurriculum)%Code)//DASH//itoa(Year)
                end if
                !call html_comment('Proposed new block is '//tBlock)
                blk = index_to_block(tBlock, NumBlocks, Block)
                if (blk>0) then ! exists
                    do idx=iachar('A'), iachar('Z')
                        tBlock = trim(Curriculum(targetCurriculum)%Code)//DASH//trim(itoa(Year))//achar(idx)
                        blk = index_to_block(tBlock, NumBlocks, Block)
                        if (blk==0) exit
                        !call html_comment('Found '//tBlock)
                    end do
                end if
                !call html_comment('New block name is '//tBlock)
                targetBlock = NumBlocks+1
                call initialize_block(Block(targetBlock))

                Block(targetBlock)%Name = trim(Curriculum(targetCurriculum)%Code)//SPACE// &
                trim(txtYear(Year))//' Year '//trim(txtSemester(Term))//' Term'
                Block(targetBlock)%BlockID = tBlock
                Block(targetBlock)%Size = 50
                Block(targetBlock)%DeptIdx = targetDepartment
                Block(targetBlock)%CurriculumIdx = targetCurriculum
                Block(targetBlock)%Year = Year
                Block(targetBlock)%Term = Term
                NumBlocks = NumBlocks+1

                ! the subjects
                Block(targetBlock)%NumClasses = 0
                do jdx=1,Curriculum(targetCurriculum)%NSubjects
                    if (Curriculum(targetCurriculum)%SubjectTerm(jdx)/=Rank) cycle
                    crse = Curriculum(targetCurriculum)%SubjectIdx(jdx)
                    idx = Block(targetBlock)%NumClasses + 1
                    Block(targetBlock)%NumClasses = idx
                    Block(targetBlock)%Subject(idx) = crse
                    Block(targetBlock)%Section(idx) = 0
                end do

                ! the sections
                if (createClasses) then
                    call block_add_and_create_sections (targetBlock, Term, NumBlocks, Block,  NumSections, Section)
                end if
            end do ! copy=1,copies
        end do ! Year=YearFirst,YearLast

        call xml_write_blocks(pathToTerm, NumBlocks, Block,  Section, 0)

        if (createClasses) then

            call offerings_summarize(NumSections, Section, Offering)
            call xml_write_sections(pathToTerm, NumSections, Section, 0)
            call count_sections_by_dept(Term, NumSections, Section)

        end if

        call html_college_links(device, targetCollege, 'Added block(s) in '//trim(tCurriculum))

    end subroutine block_add


    subroutine block_add_and_create_sections (block_idx, Term, NumBlocks, Block,  NumSections, Section)
        integer, intent(in) :: Term, block_idx
        integer, intent(in out) :: NumBlocks, NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_BLOCK), intent(in out) :: Block(0:)
        integer :: crse, idx

        call html_comment('block_add_and_create_sections()')
        do idx=1,Block(block_idx)%NumClasses
            crse = Block(block_idx)%Subject(idx)
            if (crse<=0) cycle

            call create_section_for_block (crse, block_idx, Term, NumBlocks, Block,  NumSections, Section)
            Block(block_idx)%Section(idx) = NumSections ! add to newly created block

        end do

    end subroutine block_add_and_create_sections


    subroutine create_section_for_block (crse, block_idx, Term, NumBlocks, Block,  NumSections, Section)
        integer, intent(in) :: crse, Term, block_idx
        integer, intent(in out) :: NumBlocks, NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_BLOCK), intent(in out) :: Block(0:)
        integer :: dept, kdx

        dept = Block(block_idx)%DeptIdx
        kdx = ScheduleCount(Term,dept) + 1 ! new section in department
        ScheduleCount(Term,dept) = kdx

        call html_comment('create_section_for_block()', 'block='//Block(block_idx)%BlockID, &
            'term='//itoa(Term), 'dept='//Department(dept)%Code, 'nsect='//itoa(kdx))

        if (Subject(crse)%LectHours>0) then
            NumSections = NumSections + 1
            Section(NumSections)%SubjectIdx =  crse
            Section(NumSections)%DeptIdx =  dept
            if (kdx<10) then
                Section(NumSections)%Code = Department(dept)%SectionPrefix//'00'//itoa(kdx)
            else if (kdx<100) then
                Section(NumSections)%Code = Department(dept)%SectionPrefix//'0'//itoa(kdx)
            else ! (kdx>100)
                Section(NumSections)%Code = Department(dept)%SectionPrefix//itoa(kdx)
            end if
            Section(NumSections)%ClassId = trim(Subject(crse)%Name)//SPACE//Section(NumSections)%Code
            Section(NumSections)%Slots = Subject(crse)%MaxLectSize
            Section(NumSections)%NMeets = 1
            Section(NumSections)%DayIdx(1) = 0
        end if

        if (Subject(crse)%LabHours>0) then
            NumSections = NumSections + 1
            Section(NumSections)%SubjectIdx =  crse
            Section(NumSections)%DeptIdx =  dept
            if (kdx<10) then
                Section(NumSections)%Code = Department(dept)%SectionPrefix//'00'//trim(itoa(kdx))//'-1L'
            else if (kdx<100) then
                Section(NumSections)%Code = Department(dept)%SectionPrefix//'0'//trim(itoa(kdx))//'-1L'
            else ! (kdx>100)
                Section(NumSections)%Code = Department(dept)%SectionPrefix//trim(itoa(kdx))//'-1L'
            end if
            Section(NumSections)%ClassId = trim(Subject(crse)%Name)//SPACE//Section(NumSections)%Code
            Section(NumSections)%Slots = Subject(crse)%MaxLabSize
            Section(NumSections)%NMeets = 1
            Section(NumSections)%DayIdx(1) = 0
        end if

    end subroutine create_section_for_block


end module EditBLOCKS  

