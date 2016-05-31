!======================================================================
!
!    HEEDS (Higher Education Enrollment Decision Support) - A program
!      to create enrollment scenarios for 'next term' in a university
!    Copyright (C) 2012-2015 Ricolindo L. Carino
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


    subroutine edit_block(device, thisTerm, fn)
        integer, intent (in) :: device, thisTerm, fn
        integer :: tLen1, fdx, ierr, iBlk, jdx
        integer ::  crse, sect, pos, lect, repl
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject, input_name1, input_name2, input_value
        character(len=MAX_LEN_CLASS_ID) :: tClassId, tAction
        character(len=MAX_LEN_BLOCK_CODE) :: tBlock, newBlock
        logical :: allowed_to_edit, updateBLOCK, reindexCLASSES, reindexBLOCKS
        character (len=127) :: mesg

        ! which block?
        call cgi_get_named_string(QUERY_STRING, 'A1', tBlock, ierr)
        targetBlock = index_to_block(tBlock, thisTerm)

        tBlock = Block(thisTerm,targetBlock)%BlockID
        targetCollege = Curriculum(Block(thisTerm,targetBlock)%CurriculumIdx)%CollegeIdx
        targetDepartment = Block(thisTerm,targetBlock)%DeptIdx
        allowed_to_edit = isRole_chair_of_department(targetDepartment, orHigherUp)

        mesg = SPACE
        updateBLOCK = .false.
        reindexCLASSES = .false.
        reindexBLOCKS = .false.

        if (isRoleOfficial) then
            iBlk = targetBlock
            call block_schedule(device, thisTerm, iBlk, 'Block operation failed. '//sorryMessageOfficial)
            return
        end if

        select case (fn)

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

                        tSubject = fn_underscore_to_blank(input_name2)
                        crse = index_to_subject(tSubject) ! current subject in list

                        if (input_value==SPACE) then ! course was erased

                            ! delete crse and assigned section (if any)
                            do fdx=1,Block(thisTerm,targetBlock)%NumClasses
                                if (Block(thisTerm,targetBlock)%Subject(fdx)==crse) then
                                    Block(thisTerm,targetBlock)%Subject(fdx) = 0
                                    Block(thisTerm,targetBlock)%Section(fdx) = 0
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
                        do fdx=1,Block(thisTerm,targetBlock)%NumClasses
                            if (Block(thisTerm,targetBlock)%Subject(fdx)==crse) then
                                Block(thisTerm,targetBlock)%Subject(fdx) = repl
                                Block(thisTerm,targetBlock)%Section(fdx) = 0
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
                            fdx = 1+Block(thisTerm,targetBlock)%NumClasses
                            Block(thisTerm,targetBlock)%NumClasses = fdx
                            Block(thisTerm,targetBlock)%Subject(fdx) = crse
                            Block(thisTerm,targetBlock)%Section(fdx) = 0
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
                            fdx = 1+Block(thisTerm,targetBlock)%NumClasses
                            Block(thisTerm,targetBlock)%NumClasses = fdx
                            Block(thisTerm,targetBlock)%Subject(fdx) = crse
                            call create_section_for_block (crse, targetBlock, thisTerm)
                            Block(thisTerm,targetBlock)%Section(fdx) = NumSections(thisTerm)
                            mesg = ' : Added '//trim(Section(thisTerm,NumSections(thisTerm))%ClassId)//mesg
                            call html_comment('Added '//Section(thisTerm,NumSections(thisTerm))%ClassId)
                            tLen1 = tLen1+1
                            reindexCLASSES = .true.
                        else
                            mesg = ' : ERROR - NOT added '//trim(input_value)//mesg
                        end if
                    end if

                end if


                if (tLen1==0) then
                    mesg = ' : Nothing to update?'
                    call html_comment('Nothing to update in '//Block(thisTerm,targetBlock)%BlockID)
                else
                    ! ensure (last+1) is empty
                    jdx = Block(thisTerm,targetBlock)%NumClasses+1
                    Block(thisTerm,targetBlock)%Subject(jdx) = 0
                    Block(thisTerm,targetBlock)%Section(jdx) = 0
                    ! compress in case a subject was deleted
                    fdx = 1
                    do while (fdx<=Block(thisTerm,targetBlock)%NumClasses)
                        if (Block(thisTerm,targetBlock)%Subject(fdx)==0) then
                            ! shift left
                            do jdx=fdx,Block(thisTerm,targetBlock)%NumClasses
                                Block(thisTerm,targetBlock)%Subject(jdx) = Block(thisTerm,targetBlock)%Subject(jdx+1)
                                Block(thisTerm,targetBlock)%Section(jdx) = Block(thisTerm,targetBlock)%Section(jdx+1)
                            end do
                            Block(thisTerm,targetBlock)%NumClasses = Block(thisTerm,targetBlock)%NumClasses-1
                        end if
                        fdx = fdx+1
                    end do
                    updateBLOCK = .true.
                end if

            case (fnBlockDeleteNotClasses)
                ! delete block
                mesg =' : Deleted block '//trim(Block(thisTerm,targetBlock)%BlockID)//', kept sections (if any)'
                call initialize_block(Block(thisTerm,targetBlock))
                updateBLOCK = .true.
                reindexBLOCKS = .true.

            case (fnBlockDeleteAlsoClasses)
                ! check if students are enlisted
                pos = 0
                do fdx=1,Block(thisTerm,targetBlock)%NumClasses
                    sect = Block(thisTerm,targetBlock)%Section(fdx)
                    if (sect==0) cycle
                    do jdx=1,NumStudents+NumAdditionalStudents
                        do crse=1,Student(jdx)%Enlistment(thisTerm)%lenSubject
                            if (Student(jdx)%Enlistment(thisTerm)%Section(crse)==sect) pos = pos+1
                        end do
                    end do
                    if (pos>0) then
                        mesg = ' : Cannot delete block '//trim(Block(thisTerm,targetBlock)%BlockID)// &
                            ' because students are enlisted in '//Section(thisTerm,sect)%ClassId
                        exit
                    end if
                end do

                if (pos==0) then

                    mesg = ' : Deleted block '//trim(Block(thisTerm,targetBlock)%BlockID)//' and sections (if any)'
                    ! delete sections
                    do fdx=1,Block(thisTerm,targetBlock)%NumClasses
                        sect = Block(thisTerm,targetBlock)%Section(fdx)
                        if (sect>0) then
                            crse = Block(thisTerm,targetBlock)%Subject(fdx)
                            if (isSubject_lecture_lab(crse)) then ! count how many lab sections
                                pos = index(Section(thisTerm,sect)%ClassId, DASH)
                                tClassId = Section(thisTerm,sect)%ClassId(:pos)
                                tLen1 = 0
                                do jdx=1,NumSections(thisTerm)
                                    if (Section(thisTerm,jdx)%ClassId(:pos)==tClassId(:pos)) tLen1 = tLen1+1
                                end do
                                if (tLen1==1) then ! just this lab section; delete lecture section also
                                    tClassId = Section(thisTerm,sect)%ClassId(:pos-1)
                                    lect = index_to_section(tClassId, thisTerm)
                                    call initialize_section(Section(thisTerm,Lect))
                                end if
                            end if
                            call initialize_section(Section(thisTerm,sect))
                            reindexCLASSES = .true.
                        end if
                    end do
                    ! delete block
                    call initialize_block(Block(thisTerm,targetBlock))
                    updateBLOCK = .true.
                    reindexBLOCKS = .true.

                end if

            case (fnBlockEditName)
                call cgi_get_named_string(QUERY_STRING, 'BlockID', newBlock, ierr)
                if (newBlock==SPACE .or. ierr/=0) then
                    mesg = ' : New block name not specified.'
                else if (tBlock==newBlock) then
                    mesg = ' : Block name not changed.'
                else
                    iBlk = index_to_block(newBlock, thisTerm)
                    if (iBlk>0) then
                        mesg = ' : Block name '//trim(newBlock)//' already in use.'
                    else
                        mesg = ' : Block '//trim(tBlock)//' renamed to '//newBlock
                        updateBLOCK = .true.
                        reindexBLOCKS = .true.
                        Block(thisTerm,targetBlock)%BlockID = newBlock
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
                    iBlk = index_to_block(newBlock, thisTerm)
                    if (iBlk>0) then
                        mesg = ' : Block name '//trim(newBlock)//' already in use.'
                    else ! copy Block(thisTerm,targetBlock) to end
                        NumBlocks(thisTerm) = NumBlocks(thisTerm)+1
                        Block(thisTerm,NumBlocks(thisTerm)) = Block(thisTerm,targetBlock)
                        ! make new block the target
                        targetBlock = NumBlocks(thisTerm)
                        Block(thisTerm,targetBlock)%BlockID = newBlock

                        ! initialize for new sections
                        do fdx=1,Block(thisTerm,targetBlock)%NumClasses
                            Block(thisTerm,targetBlock)%Section(fdx) = 0
                        end do
                        ! create new sections
                        call block_add_and_create_sections (targetBlock, thisTerm)

                        updateBLOCK = .true.
                        reindexBLOCKS = .true.
                        reindexCLASSES = .true.
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
                        sect = index_to_section(tClassId, thisTerm)
                        if (sect>0) then ! target of action is indexed by sect
                            crse = Section(thisTerm,sect)%SubjectIdx
                            do fdx=1,Block(thisTerm,targetBlock)%NumClasses
                                if (Block(thisTerm,targetBlock)%Subject(fdx)==crse) then
                                    Block(thisTerm,targetBlock)%Section(fdx) = sect
                                    mesg = ' : Added '//tClassId
                                    updateBLOCK = .true.
                                    call html_comment('Added '//trim(tClassId)//' to '//Block(thisTerm,targetBlock)%BlockID)
                                    exit
                                end if
                            end do
                        end if

                    case ('Del')
                        call cgi_get_named_string(QUERY_STRING, 'A3', tClassId, ierr)
                        sect = index_to_section(tClassId, thisTerm)
                        if (sect>0) then ! target of action is indexed by sect
                            do fdx=1,Block(thisTerm,targetBlock)%NumClasses
                                if (sect==Block(thisTerm,targetBlock)%Section(fdx)) then
                                    Block(thisTerm,targetBlock)%Section(fdx) = 0
                                    !Section(thisTerm,sect)%BlockID = SPACE
                                    mesg = ' : Deleted '//tClassId
                                    updateBLOCK = .true.
                                    call html_comment('Removed '//trim(tClassId)//' from '//Block(thisTerm,targetBlock)%BlockID)
                                    exit
                                end if
                            end do
                        end if

                end select

        end select

        if (updateBLOCK) then
            call block_details_write(unitXML, thisTerm, dirBLOCKS(thisTerm), targetBlock)
        end if

        if (reindexBLOCKS) then
            call block_details_write(unitXML, thisTerm, indexBLOCKS(thisTerm), 1, NumBlocks(thisTerm))
        end if

        if (reindexCLASSES) then
            call class_details_write(unitXML, thisTerm, indexCLASSES(thisTerm), 1, NumSections(thisTerm))
            call offerings_summarize(thisTerm)
            call count_sections_by_dept(thisTerm)
        end if

        if (fn==fnBlockDeleteAlsoClasses .or. fn==fnBlockDeleteNotClasses) then
            if (updateBLOCK) then
                call html_college_links(device, targetCollege, mesg(3:))
            else
                iBlk = targetBlock
                call block_schedule(device, thisTerm, iBlk, mesg(3:))
            end if
        else
            iBlk = targetBlock
            call block_schedule(device, thisTerm, iBlk, mesg(3:))
        end if

    end subroutine edit_block


    subroutine block_select_curriculum_year(device, thisTerm)
        integer, intent (in) :: device, thisTerm
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
        call make_form_start(device, fnBlockNewAdd, A9=thisTerm)
        write(device,AFORMAT) '<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
            b_tr//b_td//'Curriculum:'//e_td//b_td//'<select name="A1">', &
            '<option value=""> (select curriculum)'
        do ldx=1,NumCurricula-1
            if (.not. Curriculum(ldx)%Active) cycle
            if (targetCollege/=Curriculum(ldx)%CollegeIdx) cycle
            write(device,AFORMAT) '<option value="'//trim(Curriculum(ldx)%Code)//'"> '// &
                trim(Curriculum(ldx)%Code)//' - '//trim(Curriculum(ldx)%Title)
        end do
        write(device,AFORMAT) '</select>'//e_td//e_tr, &
            b_tr//b_td//'Year level:'//e_td//b_td//'<select name="A2">', &
            '<option value="ALL"> All years'
        do ldx=1,7
            write(device,AFORMAT) '<option value="'//trim(txtYear(ldx))//'"> '//trim(txtYear(ldx+10))//' Year'
        end do
        write(device,AFORMAT) '</select>'//nbsp//txtSemester(thisTerm+3)//termQualifier(thisTerm+3)//e_td//e_tr, &
            b_tr//b_td//'Number of blocks:'//e_td//b_td, &
            '<input name="A3" size="2" value="1">', &
            e_td//e_tr//e_table, &
            linebreak//b_bold//'Add block'//e_bold//nbsp//nbsp// &
                '<input type="submit" name="action" value="Do NOT create sections">', &
            nbsp//nbsp//nbsp//nbsp//'<input type="submit" name="action" value="Create TBA sections">', &
            e_form//horizontal

    end subroutine block_select_curriculum_year


    subroutine block_add(device, thisTerm)
        integer, intent(in) :: device, thisTerm
        integer :: ierr, iBlk, copy, crse, idx, jdx, ncopies, Rank, iYear, YearFirst, YearLast
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character (len=MAX_LEN_BLOCK_CODE) :: tBlock
        character (len=MAX_LEN_TEXT_YEAR) :: tYear
        character (len=127) :: mesg
        character (len=50) :: tAction
        logical :: inputError, reindexCLASSES
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
                if (ierr/=0 .or. YearFirst==0 .or. ((YearFirst-1)*3+thisTerm>Curriculum(targetCurriculum)%NumTerms) ) then
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
        call check_array_bound (NumBlocks(thisTerm)+jdx, MAX_ALL_BLOCKS, 'MAX_ALL_BLOCKS', inputError)
        if (inputError) then
            targetDepartment = DeptIdxUser
            targetCollege = CollegeIdxUser
            call html_write_header(device, 'Add block', linebreak//horizontal//'No more space for additional block(s)')
            return
        end if

        ! action
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)
        reindexCLASSES = index(tAction, ' TBA ')>0

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

        if (isRoleOfficial) then
            call html_college_links(device, targetCollege, '"Add block" failed. '//trim(sorryMessageOfficial))
            return
        end if

        do iYear=YearFirst,YearLast

            Rank = (iYear-1)*3+thisTerm

            do copy=1,ncopies

                ! the block
                if (ncopies>1) then
                    tBlock = trim(Curriculum(targetCurriculum)%Code)//DASH//trim(itoa(iYear))//'A'
                else
                    tBlock = trim(Curriculum(targetCurriculum)%Code)//DASH//itoa(iYear)
                end if
                !call html_comment('Proposed new block is '//tBlock)
                iBlk = index_to_block(tBlock, thisTerm)
                if (iBlk>0) then ! exists
                    do idx=iachar('A'), iachar('Z')
                        tBlock = trim(Curriculum(targetCurriculum)%Code)//DASH//trim(itoa(iYear))//achar(idx)
                        iBlk = index_to_block(tBlock, thisTerm)
                        if (iBlk==0) exit
                        !call html_comment('Found '//tBlock)
                    end do
                end if
                !call html_comment('New block name is '//tBlock)
                targetBlock = NumBlocks(thisTerm)+1
                call initialize_block(Block(thisTerm,targetBlock))

                Block(thisTerm,targetBlock)%Name = trim(Curriculum(targetCurriculum)%Code)//SPACE// &
                    trim(txtYear(iYear))//' Year '//trim(txtSemester(thisTerm))//' Term'
                Block(thisTerm,targetBlock)%BlockID = tBlock
                Block(thisTerm,targetBlock)%Size = 50
                Block(thisTerm,targetBlock)%DeptIdx = targetDepartment
                Block(thisTerm,targetBlock)%CurriculumIdx = targetCurriculum
                Block(thisTerm,targetBlock)%Year = iYear
                Block(thisTerm,targetBlock)%Term = thisTerm
                NumBlocks(thisTerm) = NumBlocks(thisTerm)+1

                ! the subjects
                Block(thisTerm,targetBlock)%NumClasses = 0
                do jdx=1,Curriculum(targetCurriculum)%NSubjects
                    if (Curriculum(targetCurriculum)%SubjectTerm(jdx)/=Rank) cycle
                    crse = Curriculum(targetCurriculum)%SubjectIdx(jdx)
                    idx = Block(thisTerm,targetBlock)%NumClasses + 1
                    Block(thisTerm,targetBlock)%NumClasses = idx
                    Block(thisTerm,targetBlock)%Subject(idx) = crse
                    Block(thisTerm,targetBlock)%Section(idx) = 0
                end do

                ! the sections
                if (reindexCLASSES) then
                    call check_array_bound (NumSections(thisTerm)+2, MAX_ALL_SECTIONS, 'MAX_ALL_SECTIONS', inputError)
                    if (.not. inputError) then
                        call block_add_and_create_sections (targetBlock, thisTerm)
                    else
                        reindexCLASSES = .false.
                    end if
                end if

                call block_details_write(unitXML, thisTerm, dirBLOCKS(thisTerm), targetBlock)

            end do ! copy=1,copies
        end do ! iYear=YearFirst,YearLast

        call block_details_write(unitXML, thisTerm, indexBLOCKS(thisTerm), 1, NumBlocks(thisTerm))

        if (reindexCLASSES) then

            call offerings_summarize(thisTerm)
            call class_details_write(unitXML, thisTerm, indexCLASSES(thisTerm), 1, NumSections(thisTerm))
            call count_sections_by_dept(thisTerm)

        end if

        call html_college_links(device, targetCollege, 'Added block(s) of '//trim(tCurriculum))

    end subroutine block_add


    subroutine block_add_and_create_sections (block_idx, thisTerm)
        integer, intent(in) :: thisTerm, block_idx
        integer :: crse, idx

        call html_comment('block_add_and_create_sections()')
        do idx=1,Block(thisTerm,block_idx)%NumClasses
            crse = Block(thisTerm,block_idx)%Subject(idx)
            if (crse<=0) cycle

            call create_section_for_block (crse, block_idx, thisTerm)
            Block(thisTerm,block_idx)%Section(idx) = NumSections(thisTerm) ! add to newly created block

        end do

    end subroutine block_add_and_create_sections


    subroutine create_section_for_block (crse, block_idx, thisTerm)
        integer, intent(in) :: crse, thisTerm, block_idx
        integer :: dept, kdx
        character(len=MAX_LEN_SECTION_CODE) :: tSection

        dept = Block(thisTerm,block_idx)%DeptIdx
        kdx = ScheduleCount(thisTerm,dept) + 1 ! new section in department
        ScheduleCount(thisTerm,dept) = kdx

        call html_comment('create_section_for_block()', 'block='//Block(thisTerm,block_idx)%BlockID, &
            'term='//itoa(thisTerm), 'dept='//Department(dept)%Code, 'nsect='//itoa(kdx))

        if (Subject(crse)%LectHours>0) then
            NumSections(thisTerm) = NumSections(thisTerm) + 1
            if (kdx<10) then
                tSection = Department(dept)%SectionPrefix//'00'//itoa(kdx)
            else if (kdx<100) then
                tSection = Department(dept)%SectionPrefix//'0'//itoa(kdx)
            else ! (kdx>100)
                tSection = Department(dept)%SectionPrefix//itoa(kdx)
            end if
            Section(thisTerm,NumSections(thisTerm)) = TYPE_SECTION (trim(Subject(crse)%Name)//SPACE//tSection, tSection, SPACE, &
                dept, crse, Subject(crse)%MaxLectSize, Subject(crse)%MaxLectSize, 1, 0, 0, 0, 0, 0)

            call class_details_write(unitXML, thisTerm, dirCLASSES(thisTerm), NumSections(thisTerm))
        end if

        if (Subject(crse)%LabHours>0) then
            NumSections(thisTerm) = NumSections(thisTerm) + 1
            if (kdx<10) then
                tSection = Department(dept)%SectionPrefix//'00'//trim(itoa(kdx))//'-1L'
            else if (kdx<100) then
                tSection = Department(dept)%SectionPrefix//'0'//trim(itoa(kdx))//'-1L'
            else ! (kdx>100)
                tSection = Department(dept)%SectionPrefix//trim(itoa(kdx))//'-1L'
            end if
            Section(thisTerm,NumSections(thisTerm)) = TYPE_SECTION (trim(Subject(crse)%Name)//SPACE//tSection, tSection, SPACE, &
                dept, crse, Subject(crse)%MaxLabSize, Subject(crse)%MaxLabSize, 1, 0, 0, 0, 0, 0)

            call class_details_write(unitXML, thisTerm, dirCLASSES(thisTerm), NumSections(thisTerm))
        end if

    end subroutine create_section_for_block


!    subroutine timetable_meetings_of_block(thisTerm, block_index, to_skip, len_list, list, TimeTable, conflicted)
!        integer, intent(in) :: thisTerm, block_index, to_skip
!        integer, intent (out) :: len_list, list(:)
!        integer, dimension(60,7), intent (out) :: TimeTable
!        logical, intent(out) :: conflicted
!        integer :: i, j, conflict_loc, sdx, sect, crse, lect
!        integer :: meetings(MAX_SECTION_MEETINGS)
!        character(len=MAX_LEN_CLASS_ID) :: tClassId
!
!        len_list = 0 ! initialize list
!        call timetable_clear(TimeTable) ! initialize weekly schedule
!        conflicted = .false.
!        do sdx=1,Block(thisTerm,block_index)%NumClasses ! loop over enries in Block(thisTerm,)
!            sect = Block(thisTerm,block_index)%Section(sdx)
!            if (sect==0) cycle ! not accommodated
!            if (sect==to_skip) cycle ! skip specified section (if, for example, the section is being edited)
!            crse = Section(thisTerm,sect)%SubjectIdx
!
!            if (isSubject_lecture_lab(crse)) then ! subject is lecture-lab
!                ! add lecture
!                j = index(Section(thisTerm,sect)%Code,DASH)
!                tClassId = trim(Subject(crse)%Name)//SPACE//Section(thisTerm,sect)%Code(:j-1)
!                lect = index_to_section(tClassId, thisTerm)
!                do i=1,Section(thisTerm,lect)%NMeets
!                    meetings(1) = i
!                    list(len_list+1) = lect
!                    list(len_list+2) = i
!                    conflict_loc = -10 ! assume no conflicting section
!                    call timetable_add_meetings_of_section(thisTerm, lect, 1, meetings, TimeTable, conflict_loc) ! add meeting to weekly schedule
!                    if (conflict_loc/=-10) then
!                        conflicted = .true.
!                    else
!                        conflict_loc = 0
!                    end if
!                    list(len_list+3) = conflict_loc ! index to conflicting section, if any
!                    len_list = len_list+3
!                end do
!            end if
!            ! add section
!            do i=1,Section(thisTerm,sect)%NMeets
!                meetings(1) = i
!                list(len_list+1) = sect
!                list(len_list+2) = i
!                conflict_loc = -10 ! assume no conflicting section
!                call timetable_add_meetings_of_section(thisTerm, sect, 1, meetings, TimeTable, conflict_loc) ! add meeting to weekly schedule
!                if (conflict_loc/=-10) then
!                    conflicted = .true.
!                else
!                    conflict_loc = 0
!                end if
!                list(len_list+3) = conflict_loc ! index to conflicting section, if any
!                len_list = len_list+3
!            end do
!        end do
!        ! end markers
!        list(len_list+1) = 0
!        list(len_list+2) = 0
!        list(len_list+3) = 0
!
!    end subroutine timetable_meetings_of_block
!


    subroutine block_list_all(device, thisTerm)
        integer, intent (in) :: device, thisTerm

        integer :: ierr, iBlk, ldx, tLen1, nblks
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        integer, dimension(60,7) :: TimeTable
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
        if ( isRole_chair_of_department(ldx, orHigherUp) ) then
            write(device,AFORMAT) trim(make_href(fnBlockNewSelect, 'Add', &
                A1=tCollege, A9=thisTerm, pre=b_bold//'(', post=' block)'//e_bold))
        end if
        ! count how many
        nblks = 0
        do iBlk=1,NumBlocks(thisTerm)
            if (CurrProgCode(Block(thisTerm,iBlk)%CurriculumIdx)/=tCurriculum) cycle
            nblks = nblks + 1
            tArray(nblks) = iBlk
            call html_comment('Found '//trim(itoa(nblks))//'. '//Block(thisTerm,tArray(nblks))%BlockID)
        end do
        ! sort
        do iBlk=1,nblks-1
            do ldx=iBlk+1,nblks
                if (Block(thisTerm,tArray(iBlk))%BlockID > Block(thisTerm,tArray(ldx))%BlockID) then
                    ierr = tArray(iBlk)
                    tArray(iBlk) = tArray(ldx)
                    tArray(ldx) = ierr
                end if
            end do
            call html_comment('Rank '//trim(itoa(iBlk))//'. '//Block(thisTerm,tArray(iBlk))%BlockID)
        end do

        write(device,AFORMAT) '<table border="0" width="50%" cellpadding="0" cellspacing="0">'
        do ldx=1,nblks
            iBlk = tArray(ldx)
            call html_comment('Rank '//trim(itoa(ldx))//' is '//trim(itoa(iBlk))//'. '//Block(thisTerm,iBlk)%BlockID)
            ! collect meetings of block
            call timetable_meetings_of_block(thisTerm, iBlk, 0, tLen1, tArray(nblks+1:), TimeTable, conflicted)
            write(device,AFORMAT) trim(make_href(fnBlockSchedule, Block(thisTerm,iBlk)%BlockID, &
                A1=Block(thisTerm,iBlk)%BlockID, A9=thisTerm, pre='<tr bgcolor="'//bgcolor(mod(ldx,2))//'">'//b_td, &
                post=e_td//b_td//trim(Block(thisTerm,iBlk)%Name)//e_td ) )
            if (conflicted) then
                write(device,AFORMAT) b_td//red//'Conflict!'//e_color//e_td//e_tr
            else
                write(device,AFORMAT) b_td_nbsp_e_td//e_tr
            end if
        end do
        write(device,AFORMAT) e_table
        if (nblks==0) write(device,AFORMAT) BRNONE
        write(device,AFORMAT) horizontal

    end subroutine block_list_all


    subroutine block_schedule(device, thisTerm, given, mesg)
        integer, intent (in) :: device, thisTerm
        integer, intent(in), optional :: given
        character(len=*), intent(in), optional :: mesg
        integer :: tLen1, tLen2, fdx, mdx, ierr, iBlk, jdx!, idx, kdx
        integer ::  crse, sect, pos, n_opts, idx_opt, lect, unassigned
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject, input_name2
        character(len=MAX_LEN_CLASS_ID) :: tClassId
        character(len=MAX_LEN_BLOCK_CODE) :: tBlock, newBlock
        integer, dimension(60,7) :: TimeTable
        logical :: conflicted, allowed_to_edit, allowed_to_show
        character (len=1) :: ch

        ! which block?
        if (present(given)) then
            targetBlock = given
            ierr = 0
        else
            call cgi_get_named_string(QUERY_STRING, 'A1', tBlock, ierr)
            targetBlock = index_to_block(tBlock, thisTerm)
        end if

        tBlock = Block(thisTerm,targetBlock)%BlockID
        targetCollege = Curriculum(Block(thisTerm,targetBlock)%CurriculumIdx)%CollegeIdx
        targetDepartment = Block(thisTerm,targetBlock)%DeptIdx

        allowed_to_edit = isRoleSysAd .or. isRoleOfficial .or. &
            (isRole_chair_of_department(targetDepartment, orHigherUp) .and. &
             College(targetCollege)%isAllowed(ToEditCLASSES,thisTerm) )
        allowed_to_show = College(targetCollege)%isAllowed(ToShowTEACHERS,thisTerm) .or. allowed_to_edit

        call html_comment('targetBlock='//trim(Block(thisTerm,targetBlock)%BlockID)//'@'//itoa(targetBlock))
        if (present(mesg)) then
            call html_write_header(device, 'Block schedule '//tBlock, mesg)
        else
            call html_write_header(device, 'Block schedule '//tBlock)
        end if

        ! collect meetings of block targetBlock
        call timetable_meetings_of_block(thisTerm, targetBlock, 0, tLen1, tArray, TimeTable, conflicted)
        call list_sections_to_edit(device, thisTerm, tLen1, tArray, fnBlockEditSection, tBlock, &
            'Del', allowed_to_edit, allowed_to_show) ! , b_bold//'Classes of '//trim(tBlock)//e_bold)

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
                iBlk = index_to_block(newBlock, thisTerm)
                if (iBlk==0) exit
            end do

            write(device,AFORMAT) linebreak//'<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
            ! actions on individual subjects
                b_tr//'<td valign="top" size="50%">'//b_bold//'Actions on subjects'//e_bold, &
                '<table border="0" cellpadding="0" cellspacing="0">'
            call make_form_start(device, fnBlockEditSubject, tBlock, A9=thisTerm)
            do fdx=1,Block(thisTerm,targetBlock)%NumClasses
                crse = Block(thisTerm,targetBlock)%Subject(fdx) ! index to subject
                sect = Block(thisTerm,targetBlock)%Section(fdx)

                tSubject = Subject(crse)%Name
                input_name2 = fn_blank_to_underscore(tSubject)

                write(device,AFORMAT) b_tr//b_td//'Replace '//nbsp//trim(tSubject)//e_td// &
                    b_td//' with '//e_td// &
                    b_td//nbsp//' <input type="text" name="REPL:'//trim(input_name2)// &
                    '" value="'//trim(tSubject)//'">'//e_td//e_tr
            end do
            write(device,AFORMAT) &
                b_tr//'<td colspan="3">'//nbsp//nbsp//'<input type="submit" name="action" value="Replace"> ', &
                nbsp//b_italic//'(Section is deleted from block if subject is replaced.'//e_italic//')'//e_td//e_tr, &
                b_tr//'<td colspan="3">'//nbsp//e_td//e_tr//e_form

            call make_form_start(device, fnBlockEditSubject, tBlock, A9=thisTerm)
            write(device,AFORMAT) &
                b_tr//'<td colspan="2"> Add subject '//e_td// &
                b_td//nbsp//' <input name="add" value="">'//e_td//e_tr, &
                b_tr//'<td colspan="3">'//nbsp//nbsp//'<input type="submit" name="action" value="Add, create new section"> ', &
                nbsp//nbsp//'<input type="submit" name="action" value="Add, DO NOT create new section"> ' //e_td//e_tr// &
                e_form//e_table

            ! action on whole block
            write(device,AFORMAT) '<td valign="top" size="50%">'//b_bold//'Actions on block'//e_bold, &
                '<table border="0" cellpadding="0" cellspacing="0">', &
                b_tr//b_td
            call make_form_start(device, fnBlockCopy, tBlock, A9=thisTerm)
            write(device,AFORMAT) &
                linebreak//'Copy block with new sections, to '//nbsp//e_td// &
                b_td//'<input name="BlockID" value="'//trim(newBlock)//'">'//e_td// &
                b_td//nbsp//' <input type="submit" name="action" value="Copy">'//e_td// &
                e_form//e_tr

            write(device,AFORMAT) b_tr//b_td
            call make_form_start(device, fnBlockEditName, tBlock, A9=thisTerm)
            write(device,AFORMAT) &
                'Rename block, same sections, to '//nbsp//e_td//b_td//'<input name="BlockID" value="'//trim(newBlock)//'">'// &
                e_td//b_td//nbsp//' <input type="submit" name="action" value="Rename">'//e_td// &
                e_form//e_tr, &
                trim(make_href(fnBlockDeleteNotClasses, 'KEEP', A1=tBlock, A9=thisTerm, &
                pre=b_tr//'<td colspan="3">Delete block, but '//nbsp, post=nbsp//' its sections.'//e_td//e_tr)), &
                trim(make_href(fnBlockDeleteAlsoClasses, 'DELETE', A1=tBlock, A9=thisTerm, &
                pre=b_tr//'<td colspan="3">'//linebreak//'Delete block, and '//nbsp, post=nbsp//' its sections.'//e_td//e_tr))

            jdx = Block(thisTerm,targetBlock)%CurriculumIdx
            write(device,AFORMAT) trim(make_href(fnBlockList, 'other '//CurrProgCode(jdx), A1=CurrProgCode(jdx), A9=thisTerm, &
                pre=b_tr//'<td colspan="3">'//linebreak//'List'//nbsp, post=nbsp//' blocks.'//e_td//e_tr)), &
                e_table, &
                e_td//e_tr//e_table//linebreak

        end if

        call timetable_display(device, thisTerm, TimeTable)

        ! make list of available sections for alternate subjects that fit the block schedule
        tLen2 = 0
        do fdx=1,Block(thisTerm,targetBlock)%NumClasses
            if (Block(thisTerm,targetBlock)%Subject(fdx)==0) cycle ! subject not specified
            if (Block(thisTerm,targetBlock)%Section(fdx)/=0) cycle ! already enlisted
            tLen2 = tLen2 + 1
        end do
        if (tLen2>0) then
            mdx = 0
            write(device,AFORMAT) linebreak//b_bold//'Other subjects that may be added'//e_bold//': '
            do fdx=1,Block(thisTerm,targetBlock)%NumClasses
                if (Block(thisTerm,targetBlock)%Subject(fdx)==0) cycle ! subject not specified
                if (Block(thisTerm,targetBlock)%Section(fdx)==0) then
                    mdx = mdx+1
                    tSubject = Subject(Block(thisTerm,targetBlock)%Subject(fdx))%Name
                    write(device,AFORMAT) nbsp//nbsp//trim(itoa(mdx))//'). <a href="#'// &
                        trim(tSubject)//'">'//trim(tSubject)//'</a>'
                end if
            end do
            write(device,AFORMAT) linebreak
            ! list subjects according to no. of options
            unassigned = 0
            do fdx=1,Block(thisTerm,targetBlock)%NumClasses
                if (Block(thisTerm,targetBlock)%Subject(fdx)==0) cycle ! subject not specified
                if (Block(thisTerm,targetBlock)%Section(fdx)/=0) cycle ! already enlisted
                crse = Block(thisTerm,targetBlock)%Subject(fdx) ! index to subject

                n_opts = 0
                do sect=1,NumSections(thisTerm)
                    if (crse/=Section(thisTerm,sect)%SubjectIdx) cycle ! not the right subject
                    ! do not add lecture class if lect-lab subject; lect added to lab schedule later
                    if (isSubject_lecture_lab(crse) .and. is_lecture_class(sect, thisTerm)) then
                        cycle
                    end if
                    ! check for conflict
                    if (is_conflict_timetable_with_section(thisTerm, sect, TimeTable)) then
                        call html_comment('  Available, but schedule conflict - '//Section(thisTerm,sect)%ClassId)
                        cycle
                    end if
                    ! lab section of lect+lab subject or lab-only subject, or section of lect-only subject, is OK
                    ! if lab section of lect+lab subject, check if lecture schedule also fits

                    if (isSubject_lecture_lab(crse)) then ! find the lecture section
                        pos = index(Section(thisTerm,sect)%ClassId, DASH)
                        tClassId = Section(thisTerm,sect)%ClassId(:pos-1)
                        lect = index_to_section(tClassId, thisTerm)
                        if (is_conflict_timetable_with_section(thisTerm, lect, TimeTable)) then ! lecture class is not OK
                            call html_comment('  Available, but lecture class schedule conflict - '// &
                                Section(thisTerm,lect)%ClassId)
                            cycle
                        end if
                    end if
                    n_opts = n_opts + 1
                end do !  sect=1,NumSections
                tArray(unassigned+1) = fdx
                tArray(unassigned+2) = n_opts
                unassigned = unassigned + 2
            end do ! fdx=1,Block(thisTerm,targetBlock)%NumClasses

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
                crse = Block(thisTerm,targetBlock)%Subject(fdx) ! index to subject
                tSubject = Subject(crse)%Name
                call html_comment('Alternate subject - '//tSubject)
                !tLen2 = 0
                n_opts = 0

                do sect=1,NumSections(thisTerm)
                    if (crse/=Section(thisTerm,sect)%SubjectIdx) cycle ! not the right subject
                    ! do not add lecture class if lect-lab subject; lect added to lab schedule later
                    if (isSubject_lecture_lab(crse) .and. is_lecture_class(sect, thisTerm)) then
                        cycle
                    end if
                    ! check for conflict
                    if (is_conflict_timetable_with_section(thisTerm, sect, TimeTable)) then
                        call html_comment('   Available, but schedule conflict - '//Section(thisTerm,sect)%ClassId)
                        cycle
                    end if
                    ! lab section of lect+lab subject or lab-only subject, or section of lect-only subject, is OK
                    ! if lab section of lect+lab subject, check if lecture schedule also fits

                    ! place options at end of Section() array
                    idx_opt = NumSections(thisTerm) + n_opts + 1
                    Section(thisTerm,idx_opt) = Section(thisTerm,sect)
                    if (isSubject_lecture_lab(crse)) then ! find the lecture section
                        pos = index(Section(thisTerm,sect)%ClassId, DASH)
                        tClassId = Section(thisTerm,sect)%ClassId(:pos-1)
                        lect = index_to_section(tClassId, thisTerm)
                        if (is_conflict_timetable_with_section(thisTerm, lect, TimeTable)) then ! lecture class is not OK
                            call html_comment('   Available, but lecture class schedule conflict - '// &
                                Section(thisTerm,lect)%ClassId)
                            cycle
                        end if
                        call html_comment('OPTION IS - '//tClassId)
                        ! add lecture schedule to lab schedule
                        do mdx=1,Section(thisTerm,lect)%NMeets
                            pos = Section(thisTerm,idx_opt)%NMeets + 1
                            Section(thisTerm,idx_opt)%DayIdx(pos) = Section(thisTerm,lect)%DayIdx(mdx)
                            Section(thisTerm,idx_opt)%bTimeIdx(pos) = Section(thisTerm,lect)%bTimeIdx(mdx)
                            Section(thisTerm,idx_opt)%eTimeIdx(pos) = Section(thisTerm,lect)%eTimeIdx(mdx)
                            Section(thisTerm,idx_opt)%RoomIdx(pos) = Section(thisTerm,lect)%RoomIdx(mdx)
                            Section(thisTerm,idx_opt)%TeacherIdx(pos) = Section(thisTerm,lect)%TeacherIdx(mdx)
                            Section(thisTerm,idx_opt)%NMeets = pos
                        end do !  mdx=1,Section(thisTerm,lect)%NMeets
                    end if
                    n_opts = n_opts + 1

                end do !  sect=1,NumSections

                call timetable_undesirability(n_opts, thisTerm, TimeTable)

                ! sections that fit schedule - can be added
                tLen2 = 0
                do sect=1,n_opts
                    idx_opt = UndesirabilityRank(sect)
                    !if (Section(NumSections+idx_opt)%RemSlots==0) cycle ! section not available
                    call html_comment(itoa(sect)//itoa(idx_opt)//Section(thisTerm,NumSections(thisTerm)+idx_opt)%ClassId// &
                        itoa(-Undesirability(idx_opt)) )
                    do mdx=1,Section(thisTerm,NumSections(thisTerm)+idx_opt)%NMeets
                        tArray(tLen1+unassigned+tLen2+1) = NumSections(thisTerm)+idx_opt
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
                    call list_sections_to_edit(device, thisTerm, tLen2, tArray(tLen1+unassigned+1), fnBlockEditSection, &
                        tBlock, 'Add', allowed_to_edit, allowed_to_show, &
                        '<a name="'//trim(tSubject)//'"></a>'//linebreak//b_bold//'Sections in '//trim(tSubject)// &
                        ' that fit existing block schedule, sorted by undesirability.'//e_bold)
                end if

            end do ! jdx=1,unassigned,2

        end if

        write(device,AFORMAT) horizontal

    end subroutine block_schedule



    subroutine block_conflicts(device, thisTerm)

        integer, intent (in) :: device, thisTerm

        integer :: ierr, iBlk, tLen1, n_count
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        integer, dimension(60,7) :: TimeTable
        logical :: conflicted

        ! which college?
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        targetCollege = index_to_college(tCollege)

        call html_comment('block_conflicts('//trim(tCollege)//')')
        n_count = 0
        tArray = 0
        do iBlk=1,NumBlocks(thisTerm)
            ! block in college?
            if (Curriculum(Block(thisTerm,iBlk)%CurriculumIdx)%CollegeIdx/=targetCollege) cycle
            ! collect meetings of block
            call timetable_meetings_of_block(thisTerm, iBlk, 0, tLen1, tArray, TimeTable, conflicted)
            if (conflicted) then
                n_count = n_count + 1
                tArray(MAX_ALL_BLOCKS-NumBlocks(thisTerm)+n_count) = iBlk
                call html_comment(itoa(n_count)//Block(thisTerm,iBlk)%BlockID)
            end if
        end do

        call html_block_list (device, fnBlockConflicts, thisTerm, n_count, tArray(MAX_ALL_BLOCKS-NumBlocks(thisTerm):), &
            'Blocks with schedule conflicts in '//tCollege, SPACE)

    end subroutine block_conflicts


    subroutine block_search_info(blockIdx, searchString, thisTerm, wrkStart, location)
        integer, intent(in) :: blockIdx, thisTerm, wrkStart
        character(len=*), intent(in) :: searchString
        character(len=80), intent(out) :: location

        integer :: mdx, sdx, tdx, tLen1, idx
        integer, dimension(60,7) :: TimeTable
        logical :: conflicted, match

        location = SPACE
        ! collect meetings of block
        call timetable_meetings_of_block(thisTerm, blockIdx, 0, tLen1, tArray(wrkStart+1:), TimeTable, conflicted)
        if (conflicted .and. index(searchString,'onfl')>0) location = 'Conflict : '//location

        do tdx=1,tLen1,3
            sdx = tArray(wrkStart+tdx)
            match = .false.
            if (index(Section(thisTerm,sdx)%ClassId,searchString)/=0) then
                location = 'Class : '//location
                match = .true.
                exit
            else
                do mdx=1,Section(thisTerm,sdx)%NMeets
                    idx = Section(thisTerm,sdx)%TeacherIdx(mdx)
                    if (index(Teacher(idx)%TeacherId,searchString)+index(Teacher(idx)%Name,searchString)>0) then
                        location = 'Teacher ID or Name : '//location
                        match = .true.
                    end if
                    idx = Section(thisTerm,sdx)%RoomIdx(mdx)
                    if (index(Room(idx)%Code,searchString)>0) then
                        location = 'Classroom : '//location
                        match = .true.
                    end if
                    if (match) exit
                end do
            end if
            if (match) exit
        end do

        if (index(Block(thisTerm,blockIdx)%Name,searchString)>0) location = 'Name : '//location
        if (index(Block(thisTerm,blockIdx)%BlockID,searchString)>0) location = 'BlockID : '//location

    end subroutine block_search_info


    subroutine html_block_list (device, fn, thisTerm, nblks, tArray, header, searchString)
        integer, intent (in) :: device, fn, thisTerm, nblks
        integer, intent (in out) :: tArray(0:nblks)
        character (len=*), intent (in) :: header
        character (len=*), intent (in) :: searchString ! for fn==fnSearchCategory

        integer :: bdx, sdx, tdx, ldx, mdx, tLen
        character(len=80) :: location, tString

        tString = searchString
        tLen = max(len_trim(tString),1)
        call html_write_header(device, header)

        if (nblks == 0) then
            write(device,AFORMAT) BRNONE
        else
            ! sort blocks
            do tdx=1,nblks-1
                do sdx=tdx+1,nblks
                    if (Block(thisTerm,tArray(sdx))%BlockID<Block(thisTerm,tArray(tdx))%BlockID) then
                        bdx =tArray(sdx)
                        tArray(sdx) = tArray(tdx)
                        tArray(tdx) = bdx
                    end if
                end do
            end do
	        write(device,AFORMAT) '<table border="0" width="60%" cellpadding="0" cellspacing="0">', &
	           b_thal//'Block ID'//e_th//b_thal//'Block Name'//e_th
            if (fn/=fnSearchCategory) then
                write(device,AFORMAT) e_tr
            else
                write(device,AFORMAT) b_thal//'"'//tString(:tLen)//'" found in ...'//e_th//e_tr
            end if

	        do ldx=1,nblks
	            bdx = tArray(ldx)
	            write(device,AFORMAT) trim(make_href(fnBlockSchedule, Block(thisTerm,bdx)%BlockID, &
	                A1=Block(thisTerm,bdx)%BlockID, A9=thisTerm, pre='<tr bgcolor="'//bgcolor(mod(ldx,2))//'">'//b_td, &
	                post=e_td//b_td//trim(Block(thisTerm,bdx)%Name)//e_td ) )
	            if (fn/=fnSearchCategory) then
	                write(device,AFORMAT) b_td_nbsp_e_td//e_tr
	            else
                    call block_search_info(bdx, tString(:tLen), thisTerm, nblks+1, location)
                    mdx = max(3, len_trim(location) )
                    write(device,AFORMAT) b_td//b_small//location(:mdx-2)//e_small//e_td//e_tr
	            end if
	        end do
	        write(device,AFORMAT) e_table

        end if
        write(device,AFORMAT) horizontal

    end subroutine html_block_list

end module EditBLOCKS  

