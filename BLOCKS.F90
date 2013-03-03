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


module BLOCKS

    use CURRICULA
    use SECTIONS

    implicit none

    ! blocks and their schedules
    type :: TYPE_BLOCK
        character(len=MAX_LEN_BLOCK_CODE) :: BlockID  ! block code
        character(len=MAX_LEN_CURRICULUM_NAME) :: Name ! block description
        integer :: Size ! size of block
        integer :: DeptIdx ! index to department that created the block
        integer :: CurriculumIdx ! index to curriculum
        integer :: Year ! year level in curriculum
        integer :: Term ! term
        integer :: NumClasses ! no of subjects for this block
        integer, dimension(MAX_SUBJECTS_PER_TERM) :: Subject, Section ! subjects and corresponding sections
    end type TYPE_BLOCK

    integer, parameter :: MAX_ALL_BLOCKS = 6*MAX_ALL_CURRICULA ! max all blocks
    type (TYPE_BLOCK), dimension(0:MAX_ALL_BLOCKS) :: CurrentBlock, NextBlock
    integer :: NumCurrentBlocks, NumNextBlocks

    ! private tokens
    character (len=MAX_LEN_FILE_PATH), private :: fileName
    character (len=MAX_LEN_XML_LINE), private :: line
    integer, private :: eof, ndels, pos(30)


contains


    subroutine initialize_block(B)
        type (TYPE_BLOCK) :: B
        B = TYPE_BLOCK ('Block Code', 'Block Name and Description', 0, 0, 0, 0, 0, 0, 0, 0)
        return
    end subroutine initialize_block


    function index_to_block(tBlock, NumBlocks, Block)
        integer :: index_to_block
        character (len=MAX_LEN_BLOCK_CODE), intent (in) :: tBlock
        integer, intent(in) :: NumBlocks
        type (TYPE_BLOCK), dimension(0:), intent(in) :: Block
        integer :: i, j, sdx

        i = 1
        j = NumBlocks
        do
            if (i>j) then
                sdx = 0
                exit
            else
                sdx = (i + j)/2
                if (tBlock==Block(sdx)%BlockID) then
                    exit
                else if (tBlock<Block(sdx)%BlockID) then
                    j = sdx-1
                else
                    i = sdx+1
                end if
            end if
        end do
        index_to_block = sdx
        return
    end function index_to_block


    subroutine sort_alphabetical_blocks(NumBlocks, Block)
        integer, intent(in) :: NumBlocks
        type (TYPE_BLOCK), dimension(0:), intent(in out) :: Block
        integer :: idx, jdx, kdx
        ! sort blocks
        kdx = 0
        do jdx=1,NumBlocks-1
            do idx=jdx+1,NumBlocks
                if (Block(jdx)%BlockID>Block(idx)%BlockID) then
                    Block(kdx) = Block(idx)
                    Block(idx) = Block(jdx)
                    Block(jdx) = Block(kdx)
                end if
            end do
        end do
        call initialize_block(Block(kdx))
        return
    end subroutine sort_alphabetical_blocks


    subroutine delete_blocks_from_dept(NumBlocks, Block,  DeptIdx)
        integer, intent(in out) :: NumBlocks
        type (TYPE_BLOCK), dimension(0:), intent(in out) :: Block
        integer, intent (in) :: DeptIdx
        integer :: blk, idx
        write(*,*) 'Removing blocks in '//Department(DeptIdx)%Code ! , ', total=',  NumBlocks
        do blk=1,NumBlocks
            if (DeptIdx==Block(blk)%DeptIdx) then
                !write(*,*) 'Removing block '//Block(blk)%BlockID
                call initialize_block(Block(blk))
            end if
        end do
        blk = 0
        do idx=1,NumBlocks
            if (Block(idx)%CurriculumIdx/=0) then
                blk = blk+1
                Block(blk) = Block(idx)
            end if
        end do
        NumBlocks = blk
        !write(*,*) NumBlocks, ' left'
        return
    end subroutine delete_blocks_from_dept


    subroutine delete_section_from_blocks(sect, NumSections, Section, NumBlocks, Block)
        integer, intent(in) :: sect, NumBlocks
        type (TYPE_BLOCK), dimension(0:), intent(in out) :: Block
        type (TYPE_SECTION), intent(in out), dimension (0:) :: Section
        integer, intent (in out) :: NumSections
        integer :: i, j
        do i=1,NumBlocks
            do j=1,Block(i)%NumClasses
                if (Block(i)%Section(j)/=sect) cycle
                Block(i)%Section(j) = 0
                exit
            end do
        end do
        call initialize_section(Section(sect))
        !integer :: pos
        !do pos=sect,NumSections-1
        !  Section(pos) = Section(pos+1)
        !end do
        !call initialize_section(Section(NumSections))
        !NumSections = NumSections - 1
        return
    end subroutine delete_section_from_blocks


    subroutine xml_write_blocks(path, NumBlocks, Block, Section, iDept)

        integer, intent (in) :: iDept, NumBlocks
        character(len=*), intent(in) :: path
        type (TYPE_BLOCK), dimension(0:), intent(in) :: Block
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section

        integer :: blk, sect, i

        ! training only?
        if (noWrites) return

        ! generate file name
        if (iDept>0) then
            fileName = trim(dirXML)//trim(path)//'BLOCKS-'//trim(Department(iDept)%Code)//'.XML'
        else
            fileName = trim(dirXML)//trim(path)//'BLOCKS.XML'
        end if

        ! write file
        call xml_open_file(unitXML, XML_ROOT_BLOCKS, fileName, i)
        write(unitXML,AFORMAT) &
        '    <comment>', &
        '        Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                    FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
        '        Code - Block identifier', &
        '        Curriculum - Curricular program for block', &
        '        Description - Block description', &
        '        Year - Year level in curriculum', &
        '        Term - Term of the year in curriculum', &
        '        Owner - Department that created (thus, can modify) block', &
        '        Section - Section already assigned to a subject', &
        '        Subject - Subject not yet assigned a section', &
        '    </comment>'

        ! loop over blocks
        do blk=1,NumBlocks

            ! block was created / is modifiable by given department?
            if (iDept>0 .and. Block(blk)%DeptIdx/=iDept) cycle

            ! any classes assigned to this block?
            if (Block(blk)%NumClasses==0) cycle

            ! block info start
            call xml_write_character(unitXML, indent0, 'Block')
            call xml_write_character(unitXML, indent1, 'Code', Block(blk)%BlockID)
            call xml_write_character(unitXML, indent1, 'Description', Block(blk)%Name)
            call xml_write_character(unitXML, indent1, 'Curriculum', Curriculum(Block(blk)%CurriculumIdx)%Code)
            call xml_write_integer(unitXML,   indent1, 'Year', Block(blk)%Year)
            call xml_write_integer(unitXML,   indent1, 'Term', Block(blk)%Term)
            call xml_write_character(unitXML, indent1, 'Owner', Department(Block(blk)%DeptIdx)%Code)

            ! write classes assigned to block
            do i=1,Block(blk)%NumClasses
                sect = Block(blk)%Section(i)
                if (sect==0) then
                    call xml_write_character(unitXML, indent2, 'Subject', Subject(Block(blk)%Subject(i))%Name)
                else
                    call xml_write_character(unitXML, indent2, 'Section', Section(sect)%ClassId)
                end if
            end do

            ! block info end
            call xml_write_character(unitXML, indent0, '/Block')
        end do

        ! close file for blocks
        call xml_close_file(unitXML, XML_ROOT_BLOCKS)
        return
    end subroutine xml_write_blocks


    subroutine read_blocks(path, NumBlocks, Block, NumSections, Section, errNo)

        character(len=*), intent(in) :: path
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section
        integer, intent (out) :: NumBlocks
        type (TYPE_BLOCK), dimension(0:), intent(out) :: Block
        integer, intent (out) :: errNo

        integer :: ddx, ierr, mainEntries, numEntries, numUpdates, partialEntries
        logical :: noXML = .false.

        NumBlocks = 0
        call initialize_block(Block(0))
        Block = Block(0)
        errNo = 0 ! no blocks is OK; none may be defined yet

        fileName = trim(dirXML)//trim(path)//'BLOCKS.XML'
        call xml_read_blocks(fileName, NumBlocks, Block, NumSections, Section, ierr)
        numEntries = NumBlocks
        mainEntries = NumBlocks
        noXML = mainEntries==0
        ! check for blocks edited by departments
        do ddx=2,NumDepartments-1
            fileName = trim(dirXML)//UPDATES//trim(path)//'BLOCKS-'//trim(Department(ddx)%Code)//'.XML'
            call xml_read_blocks(fileName, NumBlocks, Block, NumSections, Section, ierr)
            partialEntries = NumBlocks-numEntries
            numEntries = NumBlocks
            if (partialEntries>0) then ! remove blocks of dept from monolithic file
                call delete_blocks_from_dept(mainEntries, Block, ddx)
            end if
            if (ierr==0) call move_to_backup(filename)
        end do
        numUpdates = NumBlocks-mainEntries

        if (NumBlocks==0) then ! really no XML BLOCKS files; try the custom format
            fileName = trim(dirRAW)//trim(path)//'BLOCKS'
            call custom_read_blocks(fileName, NumBlocks, Block, NumSections, Section, ierr)
            mainEntries = NumBlocks
            numUpdates = 0
        end if

        ! compress block array
        numEntries = 0
        do ddx=1,NumBlocks
            !write(*,*) ddx, Block(ddx)%BlockID, Block(ddx)%CurriculumIdx
            if (Block(ddx)%CurriculumIdx/=0) then
                numEntries = numEntries+1
                Block(numEntries) = Block(ddx)
                !write(*,*) 'Adding ', mainEntries, Block(ddx)%BlockID
            end if
        end do
        NumBlocks = numEntries

        call sort_alphabetical_blocks(NumBlocks, Block)

        ! write the XML blocks file?
        if ( (noXML .and. NumBlocks>0) .or. numUpdates>0 ) &
            call xml_write_blocks(path, NumBlocks, Block,  Section, 0)

        return
    end subroutine read_blocks


    subroutine xml_read_blocks(fName, NumBlocks, Block, NumSections, Section, errNo)

        character(len=*), intent(in) :: fName ! YEAR/TERM/BLOCKS(-CURR)
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section
        integer, intent (out) :: NumBlocks
        type (TYPE_BLOCK), dimension(0:), intent(out) :: Block
        integer, intent (out) :: errNo

        type (TYPE_BLOCK) :: wrkBlock
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character (len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_CLASS_ID) :: tSection
        character (len=MAX_LEN_XML_LINE) :: value
        character (len=MAX_LEN_XML_TAG) :: tag
        logical :: flag
        integer :: idxCurr

        ! open file, return on any error
        call xml_open_file(unitXML, XML_ROOT_BLOCKS, fName, errNo, forReading)
        if (errNo/=0) return

        ! examine the file line by line
        do
            read(unitXML, AFORMAT, iostat=eof) line
            if (eof<0) exit
            ! get tag and value if any
            call xml_parse_line(line, tag, value, eof)
            if (eof/=0) exit
            select case (trim(tag))

                case ('Block')
                    call initialize_block(wrkBlock)

                case ('Code')
                    wrkBlock%BlockID = adjustl(value)

                case ('Description')
                    wrkBlock%Name = adjustl(value)

                case ('Curriculum')
                    tCurriculum = adjustl(value)
                    idxCurr = index_to_curriculum(tCurriculum)
                    if (idxCurr<0) then
                        idxCurr = -idxCurr
                    else if (idxCurr==0) then
                        idxCurr =NumCurricula
                    end if
                    wrkBlock%CurriculumIdx = idxCurr

                case ('Year')
                    wrkBlock%Year = atoi(value)

                case ('Term')
                    wrkBlock%Term = atoi(value)

                case ('Owner')
                    tDepartment = adjustl(value)
                    wrkBlock%DeptIdx = index_to_dept(tDepartment)
                    if (wrkBlock%DeptIdx==0) wrkBlock%DeptIdx = NumDepartments

                case ('Subject')
                    wrkBlock%NumClasses = wrkBlock%NumClasses + 1
                    tSubject = adjustl(value)
                    wrkBlock%Subject(wrkBlock%NumClasses) = index_to_subject(tSubject)

                case ('Section')
                    wrkBlock%NumClasses = wrkBlock%NumClasses + 1
                    tSection = adjustl(value)
                    wrkBlock%Section(wrkBlock%NumClasses) = index_to_section(tSection, NumSections, Section)
                    wrkBlock%Subject(wrkBlock%NumClasses) = Section(wrkBlock%Section(wrkBlock%NumClasses))%SubjectIdx

                case ('/Block')
                    flag = .true.
                    do idxCurr=1,NumBlocks
                        if (wrkBlock%BlockID/=Block(idxCurr)%BlockID) cycle
                        flag = .false.
                        exit
                    end do
                    if (flag) then
                        NumBlocks = NumBlocks + 1
                        call check_array_bound (NumBlocks, MAX_ALL_BLOCKS, 'MAX_ALL_BLOCKS')
                        Block(NumBlocks) = wrkBlock
                    else
                        call file_log_message ('In '//trim(fName)//' : '//trim(WrkBlock%BlockID)// &
                            ' owned by '//trim(tDepartment)//' - duplicate block; ignored.')
                    end if

                case default
                    ! do nothing
            end select
        end do
        call xml_close_file(unitXML)
        call file_log_message (itoa(NumBlocks)//' blocks after reading '//fName)

        return
    end subroutine xml_read_blocks


    subroutine custom_read_blocks(fName, NumBlocks, Block, NumSections, Section, errNo)

        character(len=*), intent (in) :: fName ! YEAR/TERM/BLOCKS
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in), dimension (0:) :: Section
        integer, intent (out) :: NumBlocks
        type (TYPE_BLOCK), dimension(0:), intent(out) :: Block
        integer, intent (out) :: errNo

        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character (len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_CLASS_ID) :: tSection
        character (len=255) :: line
        integer :: i

        open(unitXML, file=fName, status='old', iostat=errNo)
        if (errNo/=0) return

        call file_log_message ('Retrieving blocks from '//fName)
        block_loop : &
        do
            read(unitRAW, AFORMAT,iostat=eof) line
            if (eof<0) exit block_loop

            if (line(1:1)=='#' .or. line(1:3)=='   ') cycle block_loop
            !     write(device,AFORMAT) '#', &
            !       ! from Block()
            !1      trim(Block(blk)%BlockID)//COMMA// &
            !2      trim(Block(blk)%Name)//COMMA// &
            !3      trim(Block(blk)%Gender)//COMMA// &
            !4      trim(Department(Block(blk)%DeptIdx)%Code)//COMMA// &
            !5      trim(Curriculum(i)%Code)//COMMA// &
            !6      trim(College(Curriculum(i)%CollegeIdx)%Code)//COMMA// &
            !7      trim(itoa(Block(blk)%Year))//COMMA// &
            !8      trim(itoa(Block(blk)%Term))//COMMA// &
            !       ! from Block()
            !9      trim(itoa(Block(blk)%UnitsEarned))//COMMA// &
            !10     trim(itoa(Block(blk)%StdClassification))//COMMA// &
            !11     trim(itoa(Block(blk)%StdYear))//COMMA// &
            !12     trim(itoa(Block(blk)%AllowedLoad))//COMMA// &
            !13     trim(itoa(Block(blk)%StdPriority))//COMMA// &
            !14     trim(itoa(Block(blk)%NumClasses))//COMMA// &
            !15     trim(itoa(Block(blk)%NAlternates))//COMMA// &
            !16     trim(itoa(Block(blk)%NumClasses))

            call index_to_delimiters(COMMA, line, ndels, pos)

            NumBlocks = NumBlocks+1
            Block(NumBlocks)%BlockID = line(1:pos(2)-1)
            Block(NumBlocks)%Name = line(pos(2)+1:pos(3)-1)
            Block(NumBlocks)%Size = 0
            tDepartment = line(pos(4)+1:pos(5)-1)
            Block(NumBlocks)%DeptIdx = index_to_dept(tDepartment)
            tCurriculum = line(pos(5)+1:pos(6)-1)
            Block(NumBlocks)%CurriculumIdx = abs(index_to_curriculum(tCurriculum))
            Block(NumBlocks)%Year = atoi(line(pos(7)+1:pos(8)-1))
            Block(NumBlocks)%Term = atoi(line(pos(8)+1:pos(9)-1))

            Block(NumBlocks)%NumClasses = atoi(line(pos(14)+1:pos(15)-1))

            !write(*,*) Block(NumBlocks)%NumClasses
            !1     2    3      4        5    6 78 9      0
            !BSCS-4,2011,SECOND,CMSC 142,C-1L,3,,1,1.0000
            !BSCS-4,2011,SECOND,CMSC 190-2,AR1(2),2,,2,1.0000
            !BSCS-4,2011,SECOND,PI 10(SSP),,3,,3,1.0000
            !BSCS-4,2011,SECOND,NASC 4(MST),E,3,,4,1.0000
            !BSCS-4,2011,SECOND,CMSC 161,UV-1L,3,,5,1.0000
            !BSCS-4,2011,SECOND,CMSC 170,U-2L,3,,6,1.0000
            !BSCS-4,2011,SECOND,MGT 101,C-1R,3,,7,1.0000

            do i=1,Block(NumBlocks)%NumClasses
                read (unitRAW, AFORMAT) line

                call index_to_delimiters(COMMA, line, ndels, pos)
                ! subject
                tSubject = line(pos(4)+1:pos(5)-1)
                call check_array_bound (i, MAX_SUBJECTS_PER_TERM, 'MAX_SUBJECTS_PER_TERM')
                Block(NumBlocks)%Subject(i) = index_to_subject(tSubject)
                ! section
                tSection = adjustl(line(pos(5)+1:pos(6)-1))
                if (tSection==SPACE) then ! not accommodated
                    Block(NumBlocks)%Section(i) = 0
                else
                    tSection = trim(tSubject)//SPACE//tSection
                    Block(NumBlocks)%Section(i) = index_to_section(tSection, NumSections, Section)
                end if
            end do

        end do block_loop
        close (unitRAW)
        call file_log_message (itoa(NumBlocks)//' blocks after reading '//fName)

        return
    end subroutine custom_read_blocks



end module BLOCKS
