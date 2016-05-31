

    subroutine add_to_blocks(device, thisTerm, inclStudent )

        integer, intent (in) :: device, thisTerm
        integer, intent (in out) :: inclStudent(:)
        type (TYPE_PRE_ENLISTMENT) :: wrk

        ! match blocks to students of the same curriculum; all subjects in block must be advised subjects
        ! of a student
        integer :: iStd, iBlk, crse, sect, n_matches, bdx, fdx, n_assigned, in_block, nLines, tdx, inclAll
        character(len=MAX_LEN_SUBJECT_CODE) :: tSubjectA, tSubjectB
        logical :: matched, assigned

        inclAll = sum(inclStudent)
        tdx = 0

        ! count remaining slots per section
        Section(thisTerm,1:NumSections(thisTerm))%RemSlots = Section(thisTerm,1:NumSections(thisTerm))%Slots
        do iStd=1,NumStudents+NumAdditionalStudents
            do fdx=1,Student(iStd)%Enlistment(thisTerm)%lenSubject
                sect = Student(iStd)%Enlistment(thisTerm)%Section(fdx)
                if (sect > 0) then
                    if (Section(thisTerm,sect)%Slots > 0) Section(thisTerm,sect)%RemSlots = Section(thisTerm,sect)%RemSlots-1
                end if
            end do
        end do

        n_assigned = 0
        nLines = 0
        do iStd=1,NumStudents+NumAdditionalStudents
            if (inclStudent(iStd)==0) cycle ! student not selected
            tdx = tdx + 1
#if defined DBblocks
            call html_comment( itoa(tdx)//FSLASH//itoa(inclALL)//trim(text_student_curriculum(iStd)) )
#endif
            if ( sum(Student(iStd)%Enlistment(thisTerm)%Section(:))>0 ) then
#if defined DBblocks
                call html_comment('  Previously enlisted')
#endif
                cycle ! student already enlisted in some sections
            end if
            assigned = .false.
            do iBlk=1,NumBlocks(thisTerm)
                !if (CurrProgNum(Block(thisTerm,iBlk)%CurriculumIdx)/=CurrProgNum(Student(iStd)%CurriculumIdx)) cycle ! program mismatch
                if (Block(thisTerm,iBlk)%CurriculumIdx/=Student(iStd)%CurriculumIdx) cycle ! curriculum mismatch
                if (Block(thisTerm,iBlk)%Year/=Student(iStd)%Enlistment(thisTerm)%levelYear) cycle ! year level mismatch

#if defined DBblocks
                call html_comment('Candidate block is '//Block(thisTerm,iBlk)%BlockID)
#endif

                wrk = Student(iStd)%Enlistment(thisTerm)
                in_block = 0 ! how many subjects in block matched
                n_matches = 0 ! how many subjects in block matched by advised subjects
                do bdx=1,Block(thisTerm,iBlk)%NumClasses
                    crse = Block(thisTerm,iBlk)%Subject(bdx)
                    sect = Block(thisTerm,iBlk)%Section(bdx)

                    if (sect==0) cycle

                    in_block = in_block+1
                    tSubjectB = Subject(crse)%Name
                    matched = .false.
                    do fdx=1,Student(iStd)%Enlistment(thisTerm)%lenSubject
                        if (crse==Student(iStd)%Enlistment(thisTerm)%Subject(fdx) .and. Section(thisTerm,sect)%RemSlots>0) then
                            wrk%Section(fdx) = sect
                            wrk%Grade(fdx) = gdxREGD
                            matched = .true.
                            exit ! subject matching
                        end if
                    end do
                    if (matched) then
                        n_matches = n_matches+1
#if defined DBblocks
                        call html_comment(itoa(n_matches)//tSubjectB//' matched by '//Section(thisTerm,sect)%ClassId)
#endif
                        cycle

                    else
                        ! try PE
                        matched = .false.
                        if (tSubjectB(1:3)=='PE ') then
                            do fdx=1,Student(iStd)%Enlistment(thisTerm)%lenSubject
                                tSubjectA = Subject(Student(iStd)%Enlistment(thisTerm)%Subject(fdx))%Name
                                if (tSubjectA(1:3)==tSubjectB(1:3) .and. Section(thisTerm,sect)%RemSlots>0) then
                                    wrk%Subject(fdx) = crse
                                    wrk%Section(fdx) = sect
                                    wrk%Grade(fdx) = gdxREGD
                                    matched = .true.
                                    exit ! subject matching
                                end if
                            end do
                            if (matched) then
                                n_matches = n_matches+1
#if defined DBblocks
                                call html_comment(itoa(n_matches)//tSubjectB//' matched by '//Section(thisTerm,sect)%ClassId)
#endif
                                cycle
                            end if
                        end if

                        ! try NSTP
                        matched = .false.
                        if (tSubjectB(1:5)=='NSTP ') then
                            do fdx=1,Student(iStd)%Enlistment(thisTerm)%lenSubject
                                tSubjectA = Subject(Student(iStd)%Enlistment(thisTerm)%Subject(fdx))%Name
                                if (tSubjectA(1:5)==tSubjectB(1:5) .and. Section(thisTerm,sect)%RemSlots>0) then
                                    wrk%Subject(fdx) = crse
                                    wrk%Section(fdx) = sect
                                    wrk%Grade(fdx) = gdxREGD
                                    matched = .true.
                                    exit ! subject matching
                                end if
                            end do
                            if (matched) then
                                n_matches = n_matches+1
#if defined DBblocks
                                call html_comment(itoa(n_matches)//tSubjectB//' matched by '//Section(thisTerm,sect)%ClassId)
#endif
                                cycle
                            end if
                        end if

                    end if

#if defined DBblocks
                    call html_comment('NOT matched: '//tSubjectB )
#endif

                end do
#if defined DBblocks
                call html_comment(Block(thisTerm,iBlk)%BlockID//': matched subjects= '//itoa(n_matches) )
#endif
                if (n_matches/=in_block) then
                    cycle
                end if
                ! all subjects in block matched, and seats are available in all classes
                do bdx=1,Block(thisTerm,iBlk)%NumClasses
                    crse = Block(thisTerm,iBlk)%Subject(bdx)
                    sect = Block(thisTerm,iBlk)%Section(bdx)
                    if (sect==0) cycle
                    do fdx=1,wrk%lenSubject
                        if (crse/=wrk%Subject(fdx)) cycle
                        Section(thisTerm,sect)%RemSlots = Section(thisTerm,sect)%RemSlots-1
                        exit ! subject matching
                    end do
                end do
                Student(iStd)%Enlistment(thisTerm) = wrk
                Student(iStd)%Enlistment(thisTerm)%statusEnlistment = ENLISTMENT_AUTOMATIC
                n_assigned = n_assigned+1
                assigned = .true.
                call student_details_write(unitXML, dirSTUDENTS, iStd)
                call finalgrades_write(currentYear, iStd)
#if defined DBblocks
                call html_comment('  -> '//itoa(n_assigned)//' assigned to '//Block(thisTerm,iBlk)%BlockID)
#endif
                exit ! block search
            end do

            nLines = nLines+1
            write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(nLines,2))//'">'// &
                b_td//trim(itoa(n_assigned))//DOT//e_td//b_td//Student(iStd)%StdNo//e_td, &
                b_td//trim(Student(iStd)%Name)//e_td, &
                b_td//trim(Curriculum(Student(iStd)%CurriculumIdx)%Code)//e_td

            if (assigned) then
                write(device,AFORMAT) trim(make_href(fnStudentClasses, 'Preenlisted', A1=Student(iStd)%StdNo, A9=thisTerm, &
                        pre=b_td, post=' in '//trim(Block(thisTerm,iBlk)%BlockID)//e_td)), e_tr
            else
                write(device,AFORMAT) trim(make_href(fnStudentClasses, 'Not', A1=Student(iStd)%StdNo, A9=thisTerm, &
                        pre=b_td, post=' preenlisted'//e_td)), e_tr
            end if

        end do
        write(device,AFORMAT) b_tr//'<td colspan="5">'// &
            'Not preenlisted = '//itoa(sum(inclStudent)-n_assigned)//e_td//e_tr

    end subroutine add_to_blocks
