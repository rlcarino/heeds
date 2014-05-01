!======================================================================
!
!    HEEDS (Higher Education Enrollment Decision Support) - A program
!      to create enrollment scenarios for 'next term' in a university
!    Copyright (C) 2012-2014 Ricolindo L. Carino
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

    use HTML

    implicit none

contains


    subroutine edit_block(device, Term, NumSections, Section, Offering, NumBlocks, Block, fn)
        integer, intent (in) :: device, Term, fn
        integer, intent (in out) :: NumBlocks, NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_OFFERED_SUBJECTS), intent(in out) :: Offering(MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS)
        type (TYPE_BLOCK), intent(in out) :: Block(0:)
        integer :: tLen1, fdx, ierr, blk, jdx
        integer ::  crse, sect, pos, lect, repl
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject, input_name1, input_name2, input_value
        character(len=MAX_LEN_CLASS_ID) :: tClassId, tAction
        character(len=MAX_LEN_BLOCK_CODE) :: tBlock, newBlock
        logical :: allowed_to_edit, updateBLOCKS, updateCLASSES
        character (len=127) :: mesg

        ! which block?
        call cgi_get_named_string(QUERY_STRING, 'A1', tBlock, ierr)
        targetBlock = index_to_block(tBlock, NumBlocks, Block)

        tBlock = Block(targetBlock)%BlockID
        targetCollege = Curriculum(Block(targetBlock)%CurriculumIdx)%CollegeIdx
        targetDepartment = Block(targetBlock)%DeptIdx
        allowed_to_edit = is_chair_of_department(targetDepartment, orHigherUp)

        mesg = SPACE
        updateBLOCKS = .false.
        updateCLASSES = .false.

        if (isRoleOfficial) then
            blk = targetBlock
            call block_schedule(device, Term, NumSections, Section, NumBlocks, Block, blk, &
                'Block operation failed. '//sorryMessage)
            return
        end if

        select case (fn)

            case (fnBlockNewAdd)
                updateBLOCKS = .true.
                mesg = ' : Added new block '//tBlock

            case (fnBlockEditSubject)

                call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)

                tLen1 = 0 ! no. of changes
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
                            call create_section_for_block (crse, targetBlock, Term, Block,  NumSections, Section)
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
                mesg =' : Deleted block '//trim(Block(targetBlock)%BlockID)//', kept sections (if any)'
                call initialize_block(Block(targetBlock))
                updateBLOCKS = .true.

            case (fnBlockDeleteAlsoClasses)
                ! check if students are enlisted
                pos = 0
                do fdx=1,Block(targetBlock)%NumClasses
                    sect = Block(targetBlock)%Section(fdx)
                    if (sect==0) cycle
                    do jdx=1,NumStudents+NumAdditionalStudents
                        do crse=1,Enlistment(Term,jdx)%lenSubject
                            if (Enlistment(Term,jdx)%Section(crse)==sect) pos = pos+1
                        end do
                    end do
                    if (pos>0) then
                        mesg = ' : Cannot delete block '//trim(Block(targetBlock)%BlockID)// &
                            ' because students are enlisted in '//Section(sect)%ClassId
                        exit
                    end if
                end do

                if (pos==0) then

                    mesg = ' : Deleted block '//trim(Block(targetBlock)%BlockID)//' and sections (if any)'
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

                end if

            case (fnBlockEditName)
                call cgi_get_named_string(QUERY_STRING, 'BlockID', newBlock, ierr)
                if (newBlock==SPACE .or. ierr/=0) then
                    mesg = ' : New block name not specified.'
                else if (tBlock==newBlock) then
                    mesg = ' : Block name not changed.'
                else
                    blk = index_to_block(newBlock, NumBlocks, Block)
                    if (blk>0) then
                        mesg = ' : Block name '//trim(newBlock)//' already in use.'
                    else
                        mesg = ' : Block '//trim(tBlock)//' renamed to '//newBlock
                        updateBLOCKS = .true.
                        Block(targetBlock)%BlockID = newBlock
                        tBlock = newBlock
                    end if
                end if

            case (fnBlockCopy)
                call cgi_get_named_string(QUERY_STRING, 'BlockID', newBlock, ierr)
                if (newBlock==SPACE .or. ierr/=0) then
                    mesg = ' : Name for copy of block not specified.'
                else if (tBlock==newBlock) then
                    mesg = ' : Name for copy of block should be different from '//trim(tBlock)//DOT
                else
                    blk = index_to_block(newBlock, NumBlocks, Block)
                    if (blk>0) then
                        mesg = ' : Block name '//trim(newBlock)//' already in use.'
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
                        call block_add_and_create_sections (targetBlock, Term, Block,  NumSections, Section)

                        updateBLOCKS = .true.
                        updateCLASSES = .true.
                        mesg = ' : Block '//trim(tBlock)//' copied to '//trim(newBlock)// &
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
                                    mesg = ' : Added '//tClassId
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
                                    mesg = ' : Deleted '//tClassId
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
            call xml_write_classes(pathToTerm, NumSections, Section, 0)
            call count_sections_by_dept(Term, NumSections, Section)
        end if

        if (fn==fnBlockDeleteAlsoClasses .or. fn==fnBlockDeleteNotClasses) then
            if (updateBLOCKS) then
                call html_college_links(device, targetCollege, mesg(3:))
            else
                blk = targetBlock
                call block_schedule(device, Term, NumSections, Section, NumBlocks, Block, blk, mesg(3:))
            end if
        else
            blk = targetBlock
            call block_schedule(device, Term, NumSections, Section, NumBlocks, Block, blk, mesg(3:))
        end if

    end subroutine edit_block


    subroutine block_select_curriculum_year(device, Term)
        integer, intent (in) :: device, Term
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
            write(device,AFORMAT) '( No curricular programs in '//tCollege//'?)'//horizontal
            return
        end if

        ! add block
        call make_form_start(device, fnBlockNewAdd, A9=Term)
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
            write(device,AFORMAT) '<option value="'//trim(txtYear(ldx))//'"> '//trim(txtYear(ldx+11))//' Year'
        end do
        write(device,AFORMAT) '</select>'//nbsp//txtSemester(Term+3)//termQualifier(Term+3)//endtd//endtr, &
            begintr//begintd//'Number of blocks:'//endtd//begintd, &
            '<input name="A3" size="2" value="1">', &
            endtd//endtr//endtable, &
            linebreak//beginbold//'Add block'//endbold//nbsp//nbsp// &
                '<input type="submit" name="action" value="Do NOT create sections">', &
            nbsp//nbsp//nbsp//nbsp//'<input type="submit" name="action" value="Create TBA sections">', &
            endform//horizontal

    end subroutine block_select_curriculum_year


    subroutine block_add(device, Term, NumSections, Section, Offering, NumBlocks, Block)
        integer, intent(in) :: device, Term
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
            call html_write_header(device, 'Add block', linebreak//horizontal//mesg)
            return
        end if

        ! how many to add?
        jdx = ncopies*(YearLast-YearFirst+1)
        call check_array_bound (NumBlocks+jdx, MAX_ALL_BLOCKS, 'MAX_ALL_BLOCKS', inputError)
        if (inputError) then
            targetDepartment = DeptIdxUser
            targetCollege = CollegeIdxUser
            call html_write_header(device, 'Add block', linebreak//horizontal//'No more space for additional block(s)')
            return
        end if

        ! action
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
        createClasses = index(tAction, ' TBA ')>0

        targetCollege = Curriculum(targetCurriculum)%CollegeIdx

#if defined REGIST
        ! Subject administered by departments
        targetDepartment = DeptIdxUser
        tDepartment =  Department(targetDepartment)%Code
#else
        ! Subjects administered by program
        tDepartment = College(targetCollege)%Code
        targetDepartment = index_to_dept(tDepartment)
#endif

        if (isRoleOfficial) then
            call html_college_links(device, targetCollege, '"Add block" failed. '//trim(sorryMessage))
            return
        end if

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
                    call check_array_bound (NumSections+2, MAX_ALL_SECTIONS, 'MAX_ALL_SECTIONS', inputError)
                    if (.not. inputError) then
                        call block_add_and_create_sections (targetBlock, Term, Block,  NumSections, Section)
                    else
                        createClasses = .false.
                    end if
                end if
            end do ! copy=1,copies
        end do ! Year=YearFirst,YearLast

        call xml_write_blocks(pathToTerm, NumBlocks, Block,  Section, 0)

        if (createClasses) then

            call offerings_summarize(NumSections, Section, Offering)
            call xml_write_classes(pathToTerm, NumSections, Section, 0)
            call count_sections_by_dept(Term, NumSections, Section)

        end if

        call html_college_links(device, targetCollege, 'Added block(s) in '//trim(tCurriculum))

    end subroutine block_add


    subroutine block_add_and_create_sections (block_idx, Term, Block, NumSections, Section)
        integer, intent(in) :: Term, block_idx
        integer, intent(in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_BLOCK), intent(in out) :: Block(0:)
        integer :: crse, idx

        call html_comment('block_add_and_create_sections()')
        do idx=1,Block(block_idx)%NumClasses
            crse = Block(block_idx)%Subject(idx)
            if (crse<=0) cycle

            call create_section_for_block (crse, block_idx, Term, Block,  NumSections, Section)
            Block(block_idx)%Section(idx) = NumSections ! add to newly created block

        end do

    end subroutine block_add_and_create_sections


    subroutine create_section_for_block (crse, block_idx, Term, Block, NumSections, Section)
        integer, intent(in) :: crse, Term, block_idx
        integer, intent(in out) :: NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_BLOCK), intent(in out) :: Block(0:)
        integer :: dept, kdx
        character(len=MAX_LEN_SECTION_CODE) :: tSection

        dept = Block(block_idx)%DeptIdx
        kdx = ScheduleCount(Term,dept) + 1 ! new section in department
        ScheduleCount(Term,dept) = kdx

        call html_comment('create_section_for_block()', 'block='//Block(block_idx)%BlockID, &
            'term='//itoa(Term), 'dept='//Department(dept)%Code, 'nsect='//itoa(kdx))

        if (Subject(crse)%LectHours>0) then
            NumSections = NumSections + 1
            if (kdx<10) then
                tSection = Department(dept)%SectionPrefix//'00'//itoa(kdx)
            else if (kdx<100) then
                tSection = Department(dept)%SectionPrefix//'0'//itoa(kdx)
            else ! (kdx>100)
                tSection = Department(dept)%SectionPrefix//itoa(kdx)
            end if
            Section(NumSections) = TYPE_SECTION (trim(Subject(crse)%Name)//SPACE//tSection, tSection, SPACE, &
                dept, crse, Subject(crse)%MaxLectSize, Subject(crse)%MaxLectSize, 1, 0, 0, 0, 0, 0)
        end if

        if (Subject(crse)%LabHours>0) then
            NumSections = NumSections + 1
            if (kdx<10) then
                tSection = Department(dept)%SectionPrefix//'00'//trim(itoa(kdx))//'-1L'
            else if (kdx<100) then
                tSection = Department(dept)%SectionPrefix//'0'//trim(itoa(kdx))//'-1L'
            else ! (kdx>100)
                tSection = Department(dept)%SectionPrefix//trim(itoa(kdx))//'-1L'
            end if
            Section(NumSections) = TYPE_SECTION (trim(Subject(crse)%Name)//SPACE//tSection, tSection, SPACE, &
                dept, crse, Subject(crse)%MaxLabSize, Subject(crse)%MaxLabSize, 1, 0, 0, 0, 0, 0)
        end if

    end subroutine create_section_for_block


    subroutine timetable_meetings_of_block(NumSections, Section, block_index, Block, to_skip, &
            len_list, list, TimeTable, conflicted)
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
        type (TYPE_BLOCK), intent(in) :: Block(0:)
        integer, intent(in) :: block_index, to_skip
        integer, intent (out) :: len_list, list(:)
        integer, dimension(60,6), intent (out) :: TimeTable
        logical, intent(out) :: conflicted
        integer :: i, j, conflict_loc, sdx, sect, crse, lect
        integer :: meetings(MAX_SECTION_MEETINGS)
        character(len=MAX_LEN_CLASS_ID) :: tClassId

        len_list = 0 ! initialize list
        call timetable_clear(TimeTable) ! initialize weekly schedule
        conflicted = .false.
        do sdx=1,Block(block_index)%NumClasses ! loop over enries in Block()
            sect = Block(block_index)%Section(sdx)
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
                    call timetable_add_meetings_of_section(Section, lect, 1, meetings, TimeTable, conflict_loc) ! add meeting to weekly schedule
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
                call timetable_add_meetings_of_section(Section, sect, 1, meetings, TimeTable, conflict_loc) ! add meeting to weekly schedule
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

    end subroutine timetable_meetings_of_block



    subroutine block_list_all(device, thisTerm, NumBlocks, Block, NumSections, Section)
        integer, intent (in) :: device, thisTerm
        integer, intent (in) :: NumBlocks, NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
        type (TYPE_BLOCK), intent(in) :: Block(0:)

        integer :: ierr, blk, ldx, tLen1, nblks
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        integer, dimension(60,6) :: TimeTable
        logical :: conflicted

        ! which program ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
        call html_comment('block_list_all('//tCurriculum//')')

        targetCurriculum = 0
        do ldx=1,NumCurricula-1
            if (CurrProgCode(ldx) /= tCurriculum) cycle
            targetCurriculum = ldx
            exit
        end do

        targetCollege = Curriculum(targetCurriculum)%CollegeIdx
        tCollege = College(targetCollege)%Code
        call html_write_header(device, trim(tCurriculum)//' blocks')

        if (isRoleChair .or. isRoleDean) tCollege = College(CollegeIdxUser)%Code
        ldx = index_to_dept(tCollege)
        if ( is_chair_of_department(ldx, orHigherUp) ) then
            write(device,AFORMAT) trim(make_href(fnBlockNewSelect, 'Add', &
                A1=tCollege, A9=thisTerm, pre=beginbold//'(', post=' block)'//endbold))
        end if
        ! count how many
        nblks = 0
        do blk=1,NumBlocks
            if (CurrProgCode(Block(blk)%CurriculumIdx)/=tCurriculum) cycle
            nblks = nblks + 1
            tArray(nblks) = blk
            call html_comment('Found '//trim(itoa(nblks))//'. '//Block(tArray(nblks))%BlockID)
        end do
        ! sort
        do blk=1,nblks-1
            do ldx=blk+1,nblks
                if (Block(tArray(blk))%BlockID > Block(tArray(ldx))%BlockID) then
                    ierr = tArray(blk)
                    tArray(blk) = tArray(ldx)
                    tArray(ldx) = ierr
                end if
            end do
            call html_comment('Rank '//trim(itoa(blk))//'. '//Block(tArray(blk))%BlockID)
        end do

        write(device,AFORMAT) '<table border="0" width="50%" cellpadding="0" cellspacing="0">'
        do ldx=1,nblks
            blk = tArray(ldx)
            call html_comment('Rank '//trim(itoa(ldx))//' is '//trim(itoa(blk))//'. '//Block(blk)%BlockID)
            ! collect meetings of block
            call timetable_meetings_of_block(NumSections, Section, blk, Block, 0, tLen1, tArray(nblks+1:), TimeTable, conflicted)
            write(device,AFORMAT) trim(make_href(fnBlockSchedule, Block(blk)%BlockID, &
                A1=Block(blk)%BlockID, A9=thisTerm, pre=begintr//begintd, &
                post=endtd//begintd//trim(Block(blk)%Name)//endtd ) )
            if (conflicted) then
                write(device,AFORMAT) begintd//red//'Conflict!'//black//endtd//endtr
            else
                write(device,AFORMAT) tdnbspendtd//endtr
            end if
        end do
        write(device,AFORMAT) endtable
        if (nblks==0) write(device,AFORMAT) BRNONE
        write(device,AFORMAT) horizontal

    end subroutine block_list_all


    subroutine block_schedule(device, thisTerm, NumSections, Section, NumBlocks, Block, given, mesg)
        integer, intent (in) :: device, thisTerm
        integer, intent(in), optional :: given
        character(len=*), intent(in), optional :: mesg
        integer, intent (in out) :: NumBlocks, NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_BLOCK), intent(in out) :: Block(0:)
        integer :: tLen1, tLen2, fdx, mdx, ierr, blk, jdx!, idx, kdx
        integer ::  crse, sect, pos, n_opts, idx_opt, lect, unassigned
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject, input_name2
        character(len=MAX_LEN_CLASS_ID) :: tClassId
        character(len=MAX_LEN_BLOCK_CODE) :: tBlock, newBlock
        integer, dimension(60,6) :: TimeTable
        logical :: conflicted, allowed_to_edit
        character (len=1) :: ch

        ! which block?
        if (present(given)) then
            targetBlock = given
            ierr = 0
        else
            call cgi_get_named_string(QUERY_STRING, 'A1', tBlock, ierr)
            targetBlock = index_to_block(tBlock, NumBlocks, Block)
        end if

        tBlock = Block(targetBlock)%BlockID
        targetCollege = Curriculum(Block(targetBlock)%CurriculumIdx)%CollegeIdx
        targetDepartment = Block(targetBlock)%DeptIdx

        allowed_to_edit = isRoleSysAd .or. isRoleOfficial .or. &
            (is_chair_of_department(targetDepartment, orHigherUp) .and. &
             ( (thisTerm==currentTerm .and. isPeriodOne) .or. &
               thisTerm==nextTerm ) )
               !(thisTerm==nextTerm .and. (.not. isPeriodOne)) ) )


        call html_comment('targetBlock='//trim(Block(targetBlock)%BlockID)//'@'//itoa(targetBlock))
        if (present(mesg)) then
            call html_write_header(device, 'Block schedule '//tBlock, mesg)
        else
            call html_write_header(device, 'Block schedule '//tBlock)
        end if

        ! collect meetings of block targetBlock
        call timetable_meetings_of_block(NumSections, Section, targetBlock, Block, 0, tLen1, tArray, TimeTable, conflicted)
        call list_sections_to_edit(device, thisTerm, Section, tLen1, tArray, fnBlockEditSection, tBlock, &
            'Del', allowed_to_edit, beginbold//'Classes of '//trim(tBlock)//endbold)

        if (allowed_to_edit) then
            ! propose a new block name
            jdx = len_trim(tBlock)
            ch = tBlock(jdx:jdx)
            do while (jdx>0 .and. ch>='A' .and. ch<='Z')
                jdx = jdx-1
                ch = tBlock(jdx:jdx)
            end do
            do sect=iachar('A'), iachar('Z')
                newBlock = tBlock(:jdx)//achar(sect)
                blk = index_to_block(newBlock, NumBlocks, Block)
                if (blk==0) exit
            end do

            write(device,AFORMAT) linebreak//'<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
            ! actions on individual subjects
                begintr//'<td valign="top" size="50%">'//beginbold//'Actions on subjects'//endbold, &
                '<table border="0" cellpadding="0" cellspacing="0">'
            call make_form_start(device, fnBlockEditSubject, tBlock, A9=thisTerm)
            do fdx=1,Block(targetBlock)%NumClasses
                crse = Block(targetBlock)%Subject(fdx) ! index to subject
                sect = Block(targetBlock)%Section(fdx)

                tSubject = Subject(crse)%Name
                call blank_to_underscore(tSubject, input_name2)

                write(device,AFORMAT) begintr//begintd//'Replace '//nbsp//trim(tSubject)//endtd// &
                    begintd//' with '//endtd// &
                    begintd//nbsp//' <input type="text" name="REPL:'//trim(input_name2)// &
                    '" value="'//trim(tSubject)//'">'//endtd//endtr
            end do
            write(device,AFORMAT) &
                begintr//'<td colspan="3">'//nbsp//nbsp//'<input type="submit" name="action" value="Replace"> ', &
                nbsp//beginitalic//'(Section is deleted from block if subject is replaced.'//enditalic//')'//endtd//endtr, &
                begintr//'<td colspan="3">'//nbsp//endtd//endtr//endform

            call make_form_start(device, fnBlockEditSubject, tBlock, A9=thisTerm)
            write(device,AFORMAT) &
                begintr//'<td colspan="2"> Add subject '//endtd// &
                begintd//nbsp//' <input name="add" value="">'//endtd//endtr, &
                begintr//'<td colspan="3">'//nbsp//nbsp//'<input type="submit" name="action" value="Add, create new section"> ', &
                nbsp//nbsp//'<input type="submit" name="action" value="Add, DO NOT create new section"> ' //endtd//endtr// &
                endform//endtable

            ! action on whole block
            write(device,AFORMAT) '<td valign="top" size="50%">'//beginbold//'Actions on block'//endbold, &
                '<table border="0" cellpadding="0" cellspacing="0">', &
                begintr//begintd
            call make_form_start(device, fnBlockCopy, tBlock, A9=thisTerm)
            write(device,AFORMAT) &
                linebreak//'Copy block with new sections, to '//nbsp//endtd// &
                begintd//'<input name="BlockID" value="'//trim(newBlock)//'">'//endtd// &
                begintd//nbsp//' <input type="submit" name="action" value="Copy">'//endtd// &
                endform//endtr

            write(device,AFORMAT) begintr//begintd
            call make_form_start(device, fnBlockEditName, tBlock, A9=thisTerm)
            write(device,AFORMAT) &
                'Rename block, same sections, to '//nbsp//endtd//begintd//'<input name="BlockID" value="'//trim(newBlock)//'">'// &
                endtd//begintd//nbsp//' <input type="submit" name="action" value="Rename">'//endtd// &
                endform//endtr, &
                trim(make_href(fnBlockDeleteNotClasses, 'KEEP', A1=tBlock, A9=thisTerm, &
                pre=begintr//'<td colspan="3">Delete block, but '//nbsp, post=nbsp//' its sections.'//endtd//endtr)), &
                trim(make_href(fnBlockDeleteAlsoClasses, 'DELETE', A1=tBlock, A9=thisTerm, &
                pre=begintr//'<td colspan="3">'//linebreak//'Delete block, and '//nbsp, post=nbsp//' its sections.'//endtd//endtr))

            jdx = Block(targetBlock)%CurriculumIdx
            write(device,AFORMAT) trim(make_href(fnBlockList, 'other '//CurrProgCode(jdx), A1=CurrProgCode(jdx), A9=thisTerm, &
                pre=begintr//'<td colspan="3">'//linebreak//'List'//nbsp, post=nbsp//' blocks.'//endtd//endtr)), &
                endtable, &
                endtd//endtr//endtable//linebreak

        end if

        call timetable_display(device, Section, TimeTable)

        ! make list of available sections for alternate subjects that fit the block schedule
        tLen2 = 0
        do fdx=1,Block(targetBlock)%NumClasses
            if (Block(targetBlock)%Subject(fdx)==0) cycle ! subject not specified
            if (Block(targetBlock)%Section(fdx)/=0) cycle ! already enlisted
            tLen2 = tLen2 + 1
        end do
        if (tLen2>0) then
            mdx = 0
            write(device,AFORMAT) linebreak//beginbold//'Other subjects that may be added'//endbold//': '
            do fdx=1,Block(targetBlock)%NumClasses
                if (Block(targetBlock)%Subject(fdx)==0) cycle ! subject not specified
                if (Block(targetBlock)%Section(fdx)==0) then
                    mdx = mdx+1
                    tSubject = Subject(Block(targetBlock)%Subject(fdx))%Name
                    write(device,AFORMAT) nbsp//nbsp//trim(itoa(mdx))//'). <a href="#'// &
                        trim(tSubject)//'">'//trim(tSubject)//'</a>'
                end if
            end do
            write(device,AFORMAT) linebreak
            ! list subjects according to no. of options
            unassigned = 0
            do fdx=1,Block(targetBlock)%NumClasses
                if (Block(targetBlock)%Subject(fdx)==0) cycle ! subject not specified
                if (Block(targetBlock)%Section(fdx)/=0) cycle ! already enlisted
                crse = Block(targetBlock)%Subject(fdx) ! index to subject

                n_opts = 0
                do sect=1,NumSections
                    if (crse/=Section(sect)%SubjectIdx) cycle ! not the right subject
                    ! do not add lecture class if lect-lab subject; lect added to lab schedule later
                    if (is_lecture_lab_subject(crse) .and. is_lecture_class(sect, Section)) then
                        cycle
                    end if
                    ! check for conflict
                    if (is_conflict_timetable_with_section(Section, sect, TimeTable)) then
                        call html_comment('  Available, but schedule conflict - '//Section(sect)%ClassId)
                        cycle
                    end if
                    ! lab section of lect+lab subject or lab-only subject, or section of lect-only subject, is OK
                    ! if lab section of lect+lab subject, check if lecture schedule also fits

                    if (is_lecture_lab_subject(crse)) then ! find the lecture section
                        pos = index(Section(sect)%ClassId, DASH)
                        tClassId = Section(sect)%ClassId(:pos-1)
                        lect = index_to_section(tClassId, NumSections, Section)
                        if (is_conflict_timetable_with_section(Section, lect, TimeTable)) then ! lecture class is not OK
                            call html_comment('  Available, but lecture class schedule conflict - '//Section(lect)%ClassId)
                            cycle
                        end if
                    end if
                    n_opts = n_opts + 1
                end do !  sect=1,NumSections
                tArray(unassigned+1) = fdx
                tArray(unassigned+2) = n_opts
                unassigned = unassigned + 2
            end do ! fdx=1,Block(targetBlock)%NumClasses

            ! sort unassigned subjects accd to no. of options
            do mdx=1,unassigned-3,2
                do fdx=mdx+2,unassigned-1,2
                    if (tArray(fdx+1)<tArray(mdx+1)) then
                        crse = tArray(mdx)
                        tArray(mdx) = tArray(fdx)
                        tArray(fdx) = crse
                        crse = tArray(mdx+1)
                        tArray(mdx+1) = tArray(fdx+1)
                        tArray(fdx+1) = crse
                    end if
                end do
            end do

            do jdx=1,unassigned,2
                fdx = tArray(jdx)
                crse = Block(targetBlock)%Subject(fdx) ! index to subject
                tSubject = Subject(crse)%Name
                call html_comment('Alternate subject - '//tSubject)
                !tLen2 = 0
                n_opts = 0

                do sect=1,NumSections
                    if (crse/=Section(sect)%SubjectIdx) cycle ! not the right subject
                    ! do not add lecture class if lect-lab subject; lect added to lab schedule later
                    if (is_lecture_lab_subject(crse) .and. is_lecture_class(sect, Section)) then
                        cycle
                    end if
                    ! check for conflict
                    if (is_conflict_timetable_with_section(Section, sect, TimeTable)) then
                        call html_comment('   Available, but schedule conflict - '//Section(sect)%ClassId)
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
                        if (is_conflict_timetable_with_section(Section, lect, TimeTable)) then ! lecture class is not OK
                            call html_comment('   Available, but lecture class schedule conflict - '//Section(lect)%ClassId)
                            cycle
                        end if
                        call html_comment('OPTION IS - '//tClassId)
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

                ! sections that fit schedule - can be added
                tLen2 = 0
                do sect=1,n_opts
                    idx_opt = UndesirabilityRank(sect)
                    !if (Section(NumSections+idx_opt)%RemSlots==0) cycle ! section not available
                    call html_comment(itoa(sect)//itoa(idx_opt)//Section(NumSections+idx_opt)%ClassId// &
                        itoa(-Undesirability(idx_opt)) )
                    do mdx=1,Section(NumSections+idx_opt)%NMeets
                        tArray(tLen1+unassigned+tLen2+1) = NumSections+idx_opt
                        tArray(tLen1+unassigned+tLen2+2) = mdx
                        tArray(tLen1+unassigned+tLen2+3) = -Undesirability(idx_opt)
                        tLen2 = tLen2+3
                    end do !  mdx=1,Section(sect)%NMeets
                end do
                if (tLen2>0) then ! there are sections that can be added
                    ! end of list markers
                    tArray(tLen1+unassigned+tLen2+1) = 0
                    tArray(tLen1+unassigned+tLen2+2) = 0
                    tArray(tLen1+unassigned+tLen2+3) = 0
                    call list_sections_to_edit(device, thisTerm, Section, tLen2, tArray(tLen1+unassigned+1), fnBlockEditSection, &
                        tBlock, 'Add', allowed_to_edit, &
                        '<a name="'//trim(tSubject)//'"></a>'//linebreak//beginbold//'Sections in '//trim(tSubject)// &
                        ' that fit existing block schedule, sorted by undesirability.'//endbold)
                end if

            end do ! jdx=1,unassigned,2

        end if

        write(device,AFORMAT) horizontal

    end subroutine block_schedule



    subroutine block_conflicts(device, thisTerm, NumBlocks, Block, NumSections, Section)

        integer, intent (in) :: device, thisTerm
        integer, intent (in) :: NumBlocks, NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
        type (TYPE_BLOCK), intent(in) :: Block(0:)

        integer :: ierr, blk, tLen1, n_count
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        integer, dimension(60,6) :: TimeTable
        logical :: conflicted

        ! which college?
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        targetCollege = index_to_college(tCollege)

        call html_comment('block_conflicts('//trim(tCollege)//')')
        call html_write_header(device, 'Blocks with schedule conflicts in '//tCollege)

        n_count = 0
        write(device,AFORMAT) '<table border="0" width="50%" cellpadding="0" cellspacing="0">'
        do blk=1,NumBlocks
            ! block in college?
            if (Curriculum(Block(blk)%CurriculumIdx)%CollegeIdx/=targetCollege) cycle
            ! collect meetings of block
            call timetable_meetings_of_block(NumSections, Section, blk, Block, 0, tLen1, tArray, TimeTable, conflicted)
            if (conflicted) then
                write(device,AFORMAT) trim(make_href(fnBlockSchedule, Block(blk)%BlockID, &
                    A1=Block(blk)%BlockID, A9=thisTerm, pre=begintr//begintd, &
                    post=endtd//begintd//trim(Block(blk)%Name)//endtd// &
                    begintd//red//'Conflict!'//black//endtd//endtr))
                n_count = n_count + 1
            end if
        end do
        write(device,AFORMAT) endtable
        if (n_count == 0) write(device,AFORMAT) BRNONE
        write(device,AFORMAT) horizontal

    end subroutine block_conflicts

end module EditBLOCKS  

