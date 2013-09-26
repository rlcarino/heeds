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


module DisplayBLOCKS

    use HTML

    implicit none

contains


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

    end subroutine timetable_meetings_of_block



    subroutine block_list_all(device, NumBlocks, Block, NumSections, Section)
        integer, intent (in) :: device
        integer, intent (in) :: NumBlocks, NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
        type (TYPE_BLOCK), intent(in) :: Block(0:)

        integer :: ierr, blk, ldx, tLen1, nblks
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        integer, dimension(60,6) :: TimeTable
        logical :: conflicted

        call html_comment('block_list_all()')

        ! which program ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
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
        if ( ((isRoleChair .or. isRoleDean) .and. targetCollege==CollegeIdxUser ) .or. isRoleAdmin) then
            write(device,AFORMAT) trim(make_href(fnBlockNewSelect, 'Add', &
                A1=tCollege, pre='<b>(', post=' block)</b>'))
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
        call html_comment('Rank '//trim(itoa(nblks))//'. '//Block(tArray(nblks))%BlockID)

        write(device,AFORMAT) '<table border="0" width="50%" cellpadding="0" cellspacing="0">'
        do ldx=1,nblks
            blk = tArray(ldx)
            call html_comment('Rank '//trim(itoa(ldx))//' is '//trim(itoa(blk))//'. '//Block(blk)%BlockID)
            ! collect meetings of block
            call timetable_meetings_of_block(NumSections, Section, blk, Block, 0, tLen1, tArray(nblks+1:), TimeTable, conflicted)
            write(device,AFORMAT) trim(make_href(fnBlockSchedule, Block(blk)%BlockID, &
                A1=Block(blk)%BlockID, pre=begintr//begintd, &
                post=endtd//begintd//trim(Block(blk)%Name)//endtd ) )
            if (conflicted) then
                write(device,AFORMAT) begintd//red//'Conflict!'//black//endtd//endtr
            else
                write(device,AFORMAT) tdnbspendtd//endtr
            end if
        end do
        write(device,AFORMAT) '</table>'
        if (nblks==0) write(device,AFORMAT) '( None )'
        write(device,AFORMAT) '<hr>'

    end subroutine block_list_all


    subroutine block_schedule(device, NumSections, Section, Offering, NumBlocks, Block, given, mesg)
        integer, intent (in) :: device
        integer, intent(in), optional :: given
        character(len=*), intent(in), optional :: mesg
        integer, intent (in out) :: NumBlocks, NumSections
        type (TYPE_SECTION), intent(in out) :: Section(0:)
        type (TYPE_OFFERED_SUBJECTS), intent(in out) :: Offering(MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS)
        type (TYPE_BLOCK), intent(in out) :: Block(0:)
        integer :: tLen1, tLen2, fdx, mdx, ierr, blk, jdx, Term! idx, kdx
        integer ::  crse, sect, pos, n_opts, idx_opt, lect, unassigned
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubject, input_name2
        character(len=MAX_LEN_CLASS_ID) :: tClassId
        character(len=MAX_LEN_BLOCK_CODE) :: tBlock, newBlock
        integer, dimension(60,6) :: TimeTable
        logical :: conflicted, allowed_to_edit
        character (len=1) :: ch

        Term = targetTerm

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

#if defined REGIST
        ! Subject administered by departments
        allowed_to_edit = isRoleAdmin .or. & ! USER is the ADMINISTRATOR
            (isRoleChair .and. targetDepartment==DeptIdxUser) ! USER is Chair, and Department is the same as that of the Block
#else
        ! Subjects administered by program
        allowed_to_edit = isRoleAdmin .or. &
            (isRoleDean .and. targetCollege==CollegeIdxUser) .or. &
            (isRoleChair .and. targetDepartment==DeptIdxUser)
#endif

        call html_comment('targetBlock='//trim(Block(targetBlock)%BlockID)//'@'//itoa(targetBlock))
        if (present(mesg)) then
            call html_write_header(device, 'Block schedule '//tBlock, mesg)
        else
            call html_write_header(device, 'Block schedule '//tBlock)
        end if

        ! collect meetings of block targetBlock
        call timetable_meetings_of_block(NumSections, Section, targetBlock, Block, 0, tLen1, tArray, TimeTable, conflicted)
        call list_sections_to_edit(device, Section, tLen1, tArray, fnBlockEditSection, tBlock, &
            'Del', allowed_to_edit, '<b>Classes of '//trim(tBlock)//'</b>')

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

            write(device,AFORMAT) '<br><table border="0" width="100%" cellpadding="0" cellspacing="0">', &
            ! actions on individual subjects
                begintr//'<td valign="top" size="50%"><b>Actions on subjects</b>', &
                '<table border="0" cellpadding="0" cellspacing="0">'
            call make_form_start(device, fnBlockEditSubject, tBlock)
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
                nbsp//' <i>(Section is deleted from block if subject is replaced.</i>)'//endtd//endtr, &
                begintr//'<td colspan="3">'//nbsp//endtd//endtr//'</form>'

            call make_form_start(device, fnBlockEditSubject, tBlock)
            write(device,AFORMAT) &
                begintr//'<td colspan="2"> Add subject '//endtd// &
                begintd//nbsp//' <input name="add" value="">'//endtd//endtr, &
                begintr//'<td colspan="3">'//nbsp//nbsp//'<input type="submit" name="action" value="Add, create new section"> ', &
                nbsp//nbsp//'<input type="submit" name="action" value="Add, DO NOT create new section"> ' //endtd//endtr// &
                '</form></table>'

            ! action on whole block
            write(device,AFORMAT) '<td valign="top" size="50%"><b>Actions on block</b>', &
                '<table border="0" cellpadding="0" cellspacing="0">', &
                begintr//begintd
            call make_form_start(device, fnBlockCopy, tBlock)
            write(device,AFORMAT) &
                '<br>Copy block with new sections, to '//nbsp//endtd// &
                begintd//'<input name="BlockID" value="'//trim(newBlock)//'">'//endtd// &
                begintd//nbsp//' <input type="submit" name="action" value="Copy">'//endtd// &
                '</form>'//endtr

            write(device,AFORMAT) begintr//begintd
            call make_form_start(device, fnBlockEditName, tBlock)
            write(device,AFORMAT) &
                'Rename block, same sections, to '//nbsp//endtd//begintd//'<input name="BlockID" value="'//trim(newBlock)//'">'// &
                endtd//begintd//nbsp//' <input type="submit" name="action" value="Rename">'//endtd// &
                '</form>'//endtr, &
                trim(make_href(fnBlockDeleteNotClasses, 'KEEP', A1=tBlock, &
                pre=begintr//'<td colspan="3">Delete block, but '//nbsp, post=nbsp//' its sections.'//endtd//endtr)), &
                trim(make_href(fnBlockDeleteAlsoClasses, 'DELETE', A1=tBlock, &
                pre=begintr//'<td colspan="3"><br>Delete block, and '//nbsp, post=nbsp//' its sections.'//endtd//endtr))

            jdx = Block(targetBlock)%CurriculumIdx
            write(device,AFORMAT) trim(make_href(fnBlockList, 'other '//CurrProgCode(jdx), A1=CurrProgCode(jdx), &
                pre=begintr//'<td colspan="3"><br>List'//nbsp, post=nbsp//' blocks.'//endtd//endtr)), &
                '</table>', &
                endtd//endtr//'</table><br>'

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
            write(device,AFORMAT) '<br><b>Other subjects that may be added</b>: '
            do fdx=1,Block(targetBlock)%NumClasses
                if (Block(targetBlock)%Subject(fdx)==0) cycle ! subject not specified
                if (Block(targetBlock)%Section(fdx)==0) then
                    mdx = mdx+1
                    tSubject = Subject(Block(targetBlock)%Subject(fdx))%Name
                    write(device,AFORMAT) nbsp//nbsp//trim(itoa(mdx))//'). <a href="#'// &
                        trim(tSubject)//'">'//trim(tSubject)//'</a>'
                end if
            end do
            write(device,AFORMAT) '<br>'
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
                    if (is_conflict_timetable_with_section(NumSections, Section, sect, TimeTable)) then
                        call html_comment('  Available, but schedule conflict - '//Section(sect)%ClassId)
                        cycle
                    end if
                    ! lab section of lect+lab subject or lab-only subject, or section of lect-only subject, is OK
                    ! if lab section of lect+lab subject, check if lecture schedule also fits

                    if (is_lecture_lab_subject(crse)) then ! find the lecture section
                        pos = index(Section(sect)%ClassId, DASH)
                        tClassId = Section(sect)%ClassId(:pos-1)
                        lect = index_to_section(tClassId, NumSections, Section)
                        if (is_conflict_timetable_with_section(NumSections, Section, lect, TimeTable)) then ! lecture class is not OK
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
                    if (is_conflict_timetable_with_section(NumSections, Section, sect, TimeTable)) then
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
                        if (is_conflict_timetable_with_section(NumSections, Section, lect, TimeTable)) then ! lecture class is not OK
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
                    call list_sections_to_edit(device, Section, tLen2, tArray(tLen1+unassigned+1), fnBlockEditSection, &
                        tBlock, 'Add', allowed_to_edit, &
                        '<a name="'//trim(tSubject)//'"></a><br><b>Sections in '//trim(tSubject)// &
                        ' that fit existing block schedule, sorted by undesirability.</b>')
                end if

            end do ! jdx=1,unassigned,2

        end if

        write(device,AFORMAT) '<hr>'

    end subroutine block_schedule



    subroutine block_conflicts(device, NumBlocks, Block, NumSections, Section)

        integer, intent (in) :: device
        integer, intent (in) :: NumBlocks, NumSections
        type (TYPE_SECTION), intent(in) :: Section(0:)
        type (TYPE_BLOCK), intent(in) :: Block(0:)

        integer :: ierr, blk, tLen1, Term, n_count
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        integer, dimension(60,6) :: TimeTable
        logical :: conflicted

        Term = targetTerm

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
                    A1=Block(blk)%BlockID, pre=begintr//begintd, &
                    post=endtd//begintd//trim(Block(blk)%Name)//endtd// &
                    begintd//red//'Conflict!'//black//endtd//endtr))
                n_count = n_count + 1
            end if
        end do
        write(device,AFORMAT) '</table>'
        if (n_count == 0) then
            write(device,AFORMAT) '(None?)'
        end if
        write(device,AFORMAT) '<hr>'

    end subroutine block_conflicts


end module DisplayBLOCKS

