    subroutine advising_enlistment_status(device, thisTerm, levelOfDetail, &
            collegeIdx, inProg, inCurr, inYearLevel, inStat, fnCollegeCode, fnProgramCode, fnStatus, reCount)
        integer, intent (in) :: device, thisTerm, collegeIdx, levelOfDetail, inProg, inCurr, inYearLevel, inStat
        integer, intent (inout) :: fnCollegeCode, fnProgramCode, fnStatus(ADVISING_EXCLUDED:PRIORITY_DISMISSED)

        integer :: blk, cdx, gdx, ldx, colorIdx, iYearLevel, iStat, nrows, maxCount, rsum, psum, MaxYearLevelCollege, &
            nCols, n_count, tdx, iStd
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        logical :: showLink, reCount
        character(len=256) :: header

        if (reCount) call student_counts(thisTerm)

        tCollege = College(collegeIdx)%Code
        call html_comment('advising_enlistment_status(college='//trim(tCollege)//' details='//trim(itoa(levelOfDetail))//')')

        ! calculate MaxYearLevel
        maxcount = 0
        if (collegeIdx/=NumColleges) then
            do ldx=1,NumCurricula
                if (Curriculum(ldx)%CollegeIdx/=collegeIdx) cycle
                if (CurriculumCount(ldx)==0) cycle
                maxcount = max(maxcount, (Curriculum(ldx)%NumTerms+2)/3)
            end do
        else
            do ldx=1,NumCurricula
                if (CurriculumCount(ldx)==0) cycle
                maxcount = max(maxcount, (Curriculum(ldx)%NumTerms+2)/3)
            end do
        end if
        MaxYearLevelCollege = maxcount

        if (levelOfDetail==8) then ! students of a curriculum

            n_count = 0
            do tdx=1,NumStudents+NumAdditionalStudents
                iStd = StdRank(tdx)
                if (inCurr>0) then
                    if (Student(iStd)%CurriculumIdx /= inCurr) cycle
                else
                    if (CurrProgNum(Student(iStd)%CurriculumIdx) /= CurrProgNum(inProg)) cycle
                end if

                if (inStat>=ENLISTMENT_AUTOMATIC .and. instat<=ENLISTMENT_LOCKED) then
                    if (Student(iStd)%Enlistment(thisTerm)%statusEnlistment /= inStat) cycle
                    if (Student(iStd)%Enlistment(thisTerm)%levelYear /= inYearLevel) cycle
                elseif (inStat>=ADVISING_NO_SUBJECTS .and. instat<=ADVISING_IRREGULAR) then
                    if (Student(iStd)%Enlistment(thisTerm)%statusAdvising /= inStat) cycle
                    if (Student(iStd)%Enlistment(thisTerm)%levelYear /= inYearLevel) cycle
                elseif (inStat>=ADVISING_EXCLUDED .and. instat<=ADVISING_FINISHED) then
                    if (Student(iStd)%Enlistment(thisTerm)%statusAdvising /= inStat) cycle
                end if

                n_count = n_count + 1
                tArray(n_count) = iStd
            end do

            if (inCurr<=0) then
                header = trim(CurrProgCode(inProg))//' students'
            else
                header = trim(Curriculum(inCurr)%Code)//' students'
            end if

            if (inYearLevel/=-1) then
                header = trim(header)//', classification '//PRIME//trim(txtYear(inYearLevel+10))//PRIME
            end if

            if (inStat>=ENLISTMENT_AUTOMATIC .and. instat<=ENLISTMENT_LOCKED) then
                header = trim(header)//', enlistment status '//PRIME//trim(txtStatusCode(inStat))//PRIME
            else ! if (inStat>=ADVISING_NO_SUBJECTS .and. instat<=ADVISING_IRREGULAR) then
                header = trim(header)//', advising status '//PRIME//trim(txtStatusCode(inStat))//PRIME
            end if

            write(device,AFORMAT) '<h3>'//trim(header)//'</h3>'
            call html_student_list (device, n_count, tArray, .true., SPACE)
            write(device,AFORMAT) horizontal

        end if

        if (levelOfDetail==4 .or. levelOfDetail==8) then ! curriculum-level statistics

            done = .false.
            cdx = CurrProgNum(inProg)  ! generic program
            nCols = 3+ADVISING_FINISHED-ADVISING_EXCLUDED + &
                (MaxYearLevelCollege+1)*(ADVISING_IRREGULAR-ADVISING_NO_SUBJECTS+1)

            write(device,AFORMAT) &
                '<h3>Summary of advising and enlistment of '//trim(CurrProgCode(inProg))//' students</h3>', &
                b_para, '<table width="100%" cellspacing="0" cellpadding="0">', &
                ! table header line 1
                b_tr, (b_td_nbsp_e_td, iStat=ADVISING_EXCLUDED,2+ADVISING_FINISHED)
            do iStat=ADVISING_NO_SUBJECTS,ADVISING_IRREGULAR
                write(device,AFORMAT) '<td align="right" colspan="'//trim(itoa(1+MaxYearLevelCollege))//'">'// &
                    trim(txtStatusCode(iStat))//' - '//b_italic//trim(descStatusCode(iStat))//e_italic//e_td
            end do
            write(device,AFORMAT) e_tr

            ! table header line 2
            write(device,AFORMAT) b_tr//b_td//b_small//'CURRICULUM'//e_small//e_td, b_tdar//'Total'//e_td, &
                (b_tdar//trim(txtStatusCode(iStat))//e_td, iStat=ADVISING_EXCLUDED,ADVISING_FINISHED)
            do iStat=ADVISING_NO_SUBJECTS,ADVISING_IRREGULAR
                do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                    write(device,AFORMAT) b_tdar//trim(txtYear(iYearLevel+10))//e_td
                end do
            end do
            write(device,AFORMAT) e_tr, b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr

            colorIdx = 0
            do ldx=1,NumCurricula ! specific curriculum
                if (CurrProgNum(ldx) /= cdx) cycle
                done(ldx) = .true. ! signal done
                if (CurriculumCount(ldx)==0) cycle

                rsum = sum(CurriculumYearStatus(ldx,YEAR_NOT_SET:MaxYearLevelCollege,ADVISING_EXCLUDED:ADVISING_IRREGULAR))
                if (rsum==0) cycle
                colorIdx = colorIdx+1
                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(colorIdx,2))//'">', &
                    ! curriculum, row sum
                    b_td//trim(Curriculum(ldx)%Code)//e_td//b_tdar//trim(itoa(rsum))//e_td

                do iStat=ADVISING_EXCLUDED,ADVISING_FINISHED
                    psum = sum(CurriculumYearStatus(ldx,YEAR_NOT_SET:MaxYearLevelCollege,iStat))
                    if (psum==0) then
                        write(device,AFORMAT) b_td_nbsp_e_td
                    else
                        if ( isRole_dean_of_college(collegeIdx, orHigherUp) ) then
                            write(device,AFORMAT) trim(make_href(fnAdvisingEnlistmentStatus, & !fnAdvisingStatusStudents, &
                                itoa(psum), &
                                A1=College(collegeIdx)%Code, &
                                A2=CurrProgCode(inProg), &
                                A3=Curriculum(ldx)%Code, &
                                !A4=itoa(iYearLevel), &
                                A5=itoa(iStat), &
                                A9=thisTerm, pre=b_tdar, post=e_td))
                        else
                            write(device,AFORMAT) b_tdar//trim(itoa(psum))//e_td
                        end if
                    end if
                end do

                do iStat=ADVISING_NO_SUBJECTS,ADVISING_IRREGULAR
                    do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                        psum = CurriculumYearStatus(ldx,iYearLevel,iStat)
                        if (psum==0) then
                            write(device,AFORMAT) b_td_nbsp_e_td
                        else
                            if ( isRole_dean_of_college(collegeIdx, orHigherUp) ) then
                                write(device,AFORMAT) trim(make_href(fnAdvisingEnlistmentStatus, & ! fnAdvisingStatusStudents, &
                                    itoa(psum), &
                                    A1=College(collegeIdx)%Code, &
                                    A2=CurrProgCode(inProg), &
                                    A3=Curriculum(ldx)%Code, &
                                    A4=itoa(iYearLevel), &
                                    A5=itoa(iStat), &
                                    A9=thisTerm, pre=b_tdar, post=e_td))
                            else
                                write(device,AFORMAT) b_tdar//trim(itoa(psum))//e_td
                            end if
                        end if
                    end do
                end do

                write(device,AFORMAT) e_tr

            end do

            write(device,AFORMAT) &
                b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr, &
                e_table, e_para

            done = .false.
            cdx = CurrProgNum(inProg)  ! generic program
            nCols = 1 + (MaxYearLevelCollege+1)*(ENLISTMENT_LOCKED-ENLISTMENT_NEEDED+1)

            write(device,AFORMAT) b_para, '<table width="100%" cellspacing="0" cellpadding="0">', &
                ! table header line 1
                b_tr, b_td_nbsp_e_td
            do iStat=ENLISTMENT_NEEDED,ENLISTMENT_LOCKED
                write(device,AFORMAT) '<td align="right" colspan="'//trim(itoa(1+MaxYearLevelCollege))//'">'// &
                    trim(txtStatusCode(iStat))//' - '//b_italic//trim(descStatusCode(iStat))//e_italic//e_td
            end do
            write(device,AFORMAT) e_tr

            ! table header line 2
            write(device,AFORMAT) b_tr, b_td//b_small//'CURRICULUM'//e_small//e_td
            do iStat=ENLISTMENT_NEEDED,ENLISTMENT_LOCKED
                do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                    write(device,AFORMAT) b_tdar//trim(txtYear(iYearLevel+10))//e_td
                end do
            end do
            write(device,AFORMAT) e_tr, b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr

            colorIdx = 0
            do ldx=1,NumCurricula ! specific curriculum
                if (CurrProgNum(ldx) /= cdx) cycle
                done(ldx) = .true. ! signal done
                if (CurriculumCount(ldx)==0) cycle

                rsum = sum(CurriculumYearStatus(ldx,YEAR_NOT_SET:MaxYearLevelCollege,ADVISING_EXCLUDED:ADVISING_IRREGULAR))
                if (rsum==0) cycle

                colorIdx = colorIdx+1
                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(colorIdx,2))//'">', &
                    b_td//trim(Curriculum(ldx)%Code)//e_td

                do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                    psum = sum(CurriculumYearStatus(ldx,iYearLevel,ADVISING_REGULAR:ADVISING_IRREGULAR)) - &
                        sum(CurriculumYearStatus(ldx,iYearLevel,ENLISTMENT_AUTOMATIC:ENLISTMENT_PAID))
                    write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
                end do

                do iStat=ENLISTMENT_AUTOMATIC,ENLISTMENT_LOCKED
                    do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                        psum = CurriculumYearStatus(ldx,iYearLevel,iStat)
                        !if (fnStatus(iStat)/=0 .and. psum/=0 .and.  isRole_dean_of_college(collegeIdx, orHigherUp) ) then
                        if (psum/=0 .and.  isRole_dean_of_college(collegeIdx, orHigherUp) ) then
                            write(device,AFORMAT) trim(make_href(fnAdvisingEnlistmentStatus, & ! fnEnlistmentStatusStudents, &
                                itoa(psum), &
                                A1=College(collegeIdx)%Code, &
                                A2=CurrProgCode(ldx), &
                                A3=Curriculum(ldx)%Code, &
                                A4=itoa(iYearLevel), &
                                A5=itoa(iStat), &
                                A9=thisTerm, pre=b_tdar, post=e_td))
                        else
                            write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
                        end if
                    end do
                end do

                write(device,AFORMAT) e_tr

            end do

            write(device,AFORMAT) &
                b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr, &
                e_table, e_para

            fnCollegeCode = 0
            fnProgramCode = fnAdvisingEnlistmentStatus
            fnStatus = 0

        end if

        if (levelOfDetail==2 .or. levelOfDetail==4 .or. levelOfDetail==8) then ! curriculum-level statistics

            nCols = 4+ADVISING_FINISHED-ADVISING_EXCLUDED + &
                (MaxYearLevelCollege+1)*(ADVISING_IRREGULAR-ADVISING_NO_SUBJECTS+1)

            write(device,AFORMAT) &
                '<h3>Summary of advising and enlistment in '//trim(tCollege)//'</h3>', &
                b_para, &
                '<table width="100%" cellspacing="0" cellpadding="0">'
            ! table header line 1
            write(device,AFORMAT) b_tr, (b_td_nbsp_e_td, iStat=ADVISING_EXCLUDED,3+ADVISING_FINISHED)
            do iStat=ADVISING_NO_SUBJECTS,ADVISING_IRREGULAR
                write(device,AFORMAT) '<td align="right" colspan="'//trim(itoa(1+MaxYearLevelCollege))//'">'// &
                    trim(txtStatusCode(iStat))//' - '//b_italic//trim(descStatusCode(iStat))//e_italic//e_td
            end do
            write(device,AFORMAT) e_tr

            ! table header line 2
            write(device,AFORMAT) b_tr, b_td//b_small//'PROGRAM'//e_small//e_td, &
                b_tdar//'Total'//e_td, b_tdar//'%coll'//e_td, &
                (b_tdar//trim(txtStatusCode(iStat))//e_td, iStat=ADVISING_EXCLUDED,ADVISING_FINISHED)
            do iStat=ADVISING_NO_SUBJECTS,ADVISING_IRREGULAR
                do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                    write(device,AFORMAT) b_tdar//trim(txtYear(iYearLevel+10))//e_td
                end do
            end do
            write(device,AFORMAT) e_tr, b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr

            done = .false.
            colorIdx = 0
            nrows = 0
            maxcount = 0
            do ldx=1,NumCurricula
                if (Curriculum(ldx)%CollegeIdx /= collegeIdx) cycle
                maxcount = maxcount + sum(CurriculumYearStatus(ldx,:,ADVISING_EXCLUDED:ADVISING_IRREGULAR))
            end do
            do ldx=1,NumCurricula
                if (Curriculum(ldx)%CollegeIdx /= collegeIdx) cycle
                if (done(ldx)) cycle ! done

                CurriculumYearStatus(0,:,:) = CurriculumYearStatus(ldx,:,:)
                done(ldx) = .true. ! signal done
                do cdx=ldx+1,NumCurricula ! specific curriculum
                    if (CurrProgNum(cdx) /= CurrProgNum(ldx)) cycle ! not the program under consideration
                    CurriculumYearStatus(0,:,:) = CurriculumYearStatus(0,:,:) + CurriculumYearStatus(cdx,:,:)
                    done(cdx) = .true.
                end do
                rsum = sum(CurriculumYearStatus(0,:,ADVISING_EXCLUDED:ADVISING_IRREGULAR))
                if (rsum==0) cycle

                colorIdx = colorIdx+1
                nrows = nrows + 1
                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(colorIdx,2))//'">'
                if (fnProgramCode/=0) then
                    write(device,AFORMAT) trim(make_href(fnProgramCode, CurrProgCode(ldx), &
                        A1=College(collegeIdx)%Code, &
                        A2=CurrProgCode(ldx), &
                        !A3=Curriculum(ldx)%Code, &
                        !A4=itoa(iYearLevel), &
                        !A5=itoa(iStat), &
                        A9=thisTerm, pre=b_td, post=e_td, anchor=CurrProgCode(ldx)))
                else
                    write(device,AFORMAT) b_td//trim(CurrProgCode(ldx))//e_td
                end if

                ! program total and percentage wrt college
                write(device,AFORMAT) b_tdar//trim(itoa(rsum))//e_td, b_tdar//trim(ftoa((100.0*rsum)/maxcount,1))//e_td

                ! without year levels
                do iStat=ADVISING_EXCLUDED,ADVISING_FINISHED
                    psum = sum(CurriculumYearStatus(0,YEAR_NOT_SET:MaxYearLevelCollege,iStat))
                    if (fnStatus(iStat)/=0 .and. psum/=0) then
                        write(device,AFORMAT) trim(make_href(fnStatus(iStat), itoa(psum), &
                            A1=College(collegeIdx)%Code, &
                            A2=CurrProgCode(ldx), &
                            !A3=Curriculum(ldx)%Code, &
                            !A4=itoa(iYearLevel), &
                            A5=itoa(iStat), &
                            A9=thisTerm, pre=b_tdar, post=e_td))
                    elseif (iStat==ADVISING_NEEDED .and. psum==0 .and. levelOfDetail==2 .and. fnStatus(iStat)/=0) then
                        write(device,AFORMAT) trim(make_href(fnConfirmResetDemandForSubjects, &
                            b_small//b_italic//'Reset'//e_italic//e_small, &
                            A1=College(collegeIdx)%Code, &
                            A2=CurrProgCode(ldx), &
                            !A3=Curriculum(ldx)%Code, &
                            !A4=itoa(iYearLevel), &
                            A5=itoa(iStat), &
                            A9=thisTerm, pre=b_tdar, post=e_td))
                    else
                        write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
                    end if
                end do

                ! with year levels
                do iStat=ADVISING_NO_SUBJECTS,ADVISING_IRREGULAR
                    do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                        psum = CurriculumYearStatus(0,iYearLevel,iStat)
                        if (fnStatus(iStat)/=0 .and. psum/=0) then
                            showLink = .false.
                            if (fnStatus(iStat)==fnConfirmTimetabling) then ! see if there is an open blocked sections
                                do blk=1,NumBlocks(thisTerm)
                                    if (CurrProgNum(Block(thisTerm,blk)%CurriculumIdx)/=CurrProgNum(ldx)) cycle ! block not for curriculum
                                    if (Block(thisTerm,blk)%Year/=iYearLevel) cycle ! block not for year
                                    showLink = .true.
                                    exit
                                end do
                            else
                                showLink = .true.
                            end if
                            if (showLink) then
                                write(device,AFORMAT) trim(make_href(fnStatus(iStat), itoa(psum), &
                                    A1=College(collegeIdx)%Code, &
                                    A2=CurrProgCode(ldx), &
                                    !A3=Curriculum(ldx)%Code, &
                                    A4=itoa(iYearLevel), &
                                    A5=itoa(iStat), &
                                    A9=thisTerm, pre=b_tdar, post=e_td))
                            else
                                write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
                            end if
                        else
                            write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
                        end if
                    end do
                end do

                write(device,AFORMAT) e_tr

            end do

            ! column total
            if (nrows>1) then
                CurriculumYearStatus(0,:,:) = 0
                do ldx=1,NumCurricula
                    if (Curriculum(ldx)%CollegeIdx /= collegeIdx) cycle ! not in college
                    CurriculumYearStatus(0,:,:) = CurriculumYearStatus(0,:,:) + CurriculumYearStatus(ldx,:,:)
                end do
                rsum = sum(CurriculumYearStatus(0,YEAR_NOT_SET:MaxYearLevelCollege,ADVISING_EXCLUDED:ADVISING_IRREGULAR))

                ! column totals
                write(device,AFORMAT) &
                    b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr, &
                    b_tr//b_td_nbsp_e_td//b_tdar//trim(itoa(rsum))//e_td//b_td_nbsp_e_td
                do iStat=ADVISING_EXCLUDED,ADVISING_FINISHED
                    psum = sum(CurriculumYearStatus(0,YEAR_NOT_SET:MaxYearLevelCollege,iStat))
                    write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
                end do
                do iStat=ADVISING_NO_SUBJECTS,ADVISING_IRREGULAR
                    do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                        psum = CurriculumYearStatus(0,iYearLevel,iStat)
                        write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
                    end do
                end do

                write(device,AFORMAT) e_tr

            end if

            nCols = 1 + (MaxYearLevelCollege+1)*(ENLISTMENT_LOCKED-ENLISTMENT_NEEDED+1)

            write(device,AFORMAT) e_table, e_para, &
                b_para, &
                '<table width="100%" cellspacing="0" cellpadding="0">', &
                ! table header line 1
                b_tr, b_td_nbsp_e_td
            do iStat=ENLISTMENT_NEEDED,ENLISTMENT_LOCKED
                write(device,AFORMAT) '<td align="right" colspan="'//trim(itoa(1+MaxYearLevelCollege))//'">'// &
                    trim(txtStatusCode(iStat))//' - '//b_italic//trim(descStatusCode(iStat))//e_italic//e_td
            end do
            write(device,AFORMAT) e_tr

            ! table header line 2
            write(device,AFORMAT) b_tr, b_td//b_small//'PROGRAM'//e_small//e_td
            do iStat=ENLISTMENT_NEEDED,ENLISTMENT_LOCKED
                do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                    write(device,AFORMAT) b_tdar//trim(txtYear(iYearLevel+10))//e_td
                end do
            end do
            write(device,AFORMAT) e_tr, b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr

            done = .false.
            colorIdx = 0
            nrows = 0
            do ldx=1,NumCurricula
                if (Curriculum(ldx)%CollegeIdx /= collegeIdx) cycle
                if (done(ldx)) cycle ! done

                CurriculumYearStatus(0,:,:) = CurriculumYearStatus(ldx,:,:)
                done(ldx) = .true. ! signal done
                do cdx=ldx+1,NumCurricula ! specific curriculum
                    if (CurrProgNum(cdx) /= CurrProgNum(ldx)) cycle ! not the program under consideration
                    CurriculumYearStatus(0,:,:) = CurriculumYearStatus(0,:,:) + CurriculumYearStatus(cdx,:,:)
                    done(cdx) = .true.
                end do
                rsum = sum(CurriculumYearStatus(0,:,ADVISING_EXCLUDED:ADVISING_IRREGULAR))
                if (rsum==0) cycle

                colorIdx = colorIdx+1
                nrows = nrows + 1
                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(colorIdx,2))//'">'
                if (fnProgramCode/=0) then
                    write(device,AFORMAT) trim(make_href(fnProgramCode, CurrProgCode(ldx), &
                        A1=College(collegeIdx)%Code, &
                        A2=CurrProgCode(ldx), &
                        !A3=Curriculum(ldx)%Code, &
                        !A4=itoa(iYearLevel), &
                        !A5=itoa(iStat), &
                        A9=thisTerm, pre=b_td, post=e_td)) ! , anchor=CurrProgCode(ldx)))
                else
                    write(device,AFORMAT) b_td//trim(CurrProgCode(ldx))//e_td
                end if

                do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                    psum = sum(CurriculumYearStatus(0,iYearLevel,ADVISING_REGULAR:ADVISING_IRREGULAR)) - &
                        sum(CurriculumYearStatus(0,iYearLevel,ENLISTMENT_AUTOMATIC:ENLISTMENT_PAID))
                    write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
                end do

                do iStat=ENLISTMENT_AUTOMATIC,ENLISTMENT_LOCKED
                    do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                        psum = CurriculumYearStatus(0,iYearLevel,iStat)
                        if (fnStatus(iStat)/=0 .and. psum/=0) then
                            write(device,AFORMAT) trim(make_href(fnStatus(iStat), itoa(psum), &
                                A1=College(collegeIdx)%Code, &
                                A2=CurrProgCode(ldx), &
                                !A3=Curriculum(ldx)%Code, &
                                A4=itoa(iYearLevel), &
                                A5=itoa(iStat), &
                                A9=thisTerm, pre=b_tdar, post=e_td))
                        else
                            write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
                        end if
                    end do
                end do

                write(device,AFORMAT) e_tr

            end do

            ! column total
            if (nrows>1) then
                CurriculumYearStatus(0,:,:) = 0
                do ldx=1,NumCurricula
                    if (Curriculum(ldx)%CollegeIdx /= collegeIdx) cycle ! not in college
                    CurriculumYearStatus(0,:,:) = CurriculumYearStatus(0,:,:) + CurriculumYearStatus(ldx,:,:)
                end do

                write(device,AFORMAT) e_tr, b_tr//'<td colspan="'// &
                    trim(itoa(1 + (MaxYearLevelCollege+1)*(ENLISTMENT_LOCKED-ENLISTMENT_NEEDED+1) ))//'">'// &
                    horizontal//e_td//e_tr

                write(device,AFORMAT) b_tr, b_td//b_small//'TOTALS'//e_small//e_td
                do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                    psum = sum(CurriculumYearStatus(0,iYearLevel,ADVISING_REGULAR:ADVISING_IRREGULAR)) - &
                        sum(CurriculumYearStatus(0,iYearLevel,ENLISTMENT_AUTOMATIC:ENLISTMENT_PAID))
                    write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
                end do
                do iStat=ENLISTMENT_AUTOMATIC,ENLISTMENT_LOCKED
                    do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                        psum = CurriculumYearStatus(0,iYearLevel,iStat)
                        write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
                    end do
                end do
                write(device,AFORMAT) e_tr

            end if

            write(device,AFORMAT) e_table, e_para, &
                b_para, b_small, 'Legends : YNS - <i>Year not set</i>', &
                (nbsp//': '//trim(txtStatusCode(iStat))//' - '//b_italic//trim(descStatusCode(iStat))//e_italic, &
                iStat=ADVISING_EXCLUDED,ADVISING_FINISHED), e_small, e_para

        end if


        if (levelOfDetail==1) then ! college-level counts

            nCols = 4+ADVISING_FINISHED-ADVISING_EXCLUDED + &
                (MaxYearLevelCollege+1)*(ADVISING_IRREGULAR-ADVISING_NO_SUBJECTS+1)
            write(device,AFORMAT) &
                '<h3>Summary of advising and enlistment in ALL colleges</h3>', &
                b_para, &
                '<table width="100%" cellspacing="0" cellpadding="0">'
            ! table header line 1
            write(device,AFORMAT) b_tr, (b_td_nbsp_e_td, iStat=ADVISING_EXCLUDED,3+ADVISING_FINISHED)
            do iStat=ADVISING_NO_SUBJECTS,ADVISING_IRREGULAR
                write(device,AFORMAT) '<td align="right" colspan="'//trim(itoa(1+MaxYearLevelCollege))//'">'// &
                    trim(txtStatusCode(iStat))//' - '//b_italic//trim(descStatusCode(iStat))//e_italic//e_td
            end do
            write(device,AFORMAT) e_tr

            ! table header line 2
            write(device,AFORMAT) b_tr, b_td//b_small//'COLLEGE'//e_small//e_td, &
                b_tdar//'Total'//e_td, b_tdar//'%univ'//e_td, &
                (b_tdar//trim(txtStatusCode(iStat))//e_td, iStat=ADVISING_EXCLUDED,ADVISING_FINISHED)
            do iStat=ADVISING_NO_SUBJECTS,ADVISING_IRREGULAR
                do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                    write(device,AFORMAT) b_tdar//trim(txtYear(iYearLevel+10))//e_td
                end do
            end do
            write(device,AFORMAT) e_tr, b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr

            maxcount = sum(CurriculumYearStatus(1:,:,ADVISING_EXCLUDED:ADVISING_IRREGULAR))
            colorIdx = 0
            nrows = 0
            do gdx=1,NumColleges
                if (CollegeCount(gdx) <= 0) cycle

                colorIdx = colorIdx+1
                nrows = nrows + 1

                ! accummulate for all curricula in college
                CurriculumYearStatus(0,:,:) = 0
                do ldx=1,NumCurricula
                    if (Curriculum(ldx)%CollegeIdx /= gdx) cycle ! not in college
                    CurriculumYearStatus(0,:,:) = CurriculumYearStatus(0,:,:) + CurriculumYearStatus(ldx,:,:)
                end do
                rsum = sum(CurriculumYearStatus(0,:,ADVISING_EXCLUDED:ADVISING_IRREGULAR))

                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(colorIdx,2))//'">'
                ! college
                if (fnCollegeCode/=0) then
                    write(device,AFORMAT) trim(make_href(fnCollegeCode, College(gdx)%Code, &
                            A1=College(gdx)%Code, &
                            !A2=CurrProgCode(ldx), &
                            !A3=Curriculum(ldx)%Code, &
                            !A4=itoa(iYearLevel), &
                            !A5=itoa(iStat), &
                            A9=thisTerm, pre=b_td, post=e_td))
                else
                    write(device,AFORMAT) b_td//trim(College(gdx)%Code)//e_td
                end if

                write(device,AFORMAT) &
                    b_tdar//trim(itoa(rsum))//e_td, &
                    b_tdar//trim(ftoa((100.0*rsum)/maxcount,1))//e_td

                ! without year levels
                do iStat=ADVISING_EXCLUDED,ADVISING_FINISHED
                    psum = sum(CurriculumYearStatus(0,YEAR_NOT_SET:MaxYearLevelCollege,iStat))
                    write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
                end do

                ! with year levels
                do iStat=ADVISING_NO_SUBJECTS,ADVISING_IRREGULAR
                    do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                        psum = CurriculumYearStatus(0,iYearLevel,iStat)
                        write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
                    end do
                end do

                write(device,AFORMAT) e_tr

            end do
            write(device,AFORMAT) b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr

            ! column totals
            CurriculumYearStatus(0,:,:) = 0
            do ldx=1,NumCurricula
                CurriculumYearStatus(0,:,:) = CurriculumYearStatus(0,:,:) + CurriculumYearStatus(ldx,:,:)
            end do
            rsum = sum(CurriculumYearStatus(0,YEAR_NOT_SET:MaxYearLevelCollege,ADVISING_EXCLUDED:ADVISING_IRREGULAR))

            ! no name, total, no %
            write(device,AFORMAT) b_tr//b_td_nbsp_e_td//b_tdar//trim(itoa(rsum))//e_td//b_td_nbsp_e_td

            ! status with no year levels
            do iStat=ADVISING_EXCLUDED,ADVISING_FINISHED
                psum = sum(CurriculumYearStatus(0,YEAR_NOT_SET:MaxYearLevelCollege,iStat))
                write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
            end do
            ! status with year levels
            do iStat=ADVISING_NO_SUBJECTS,ADVISING_IRREGULAR
                do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                    psum = CurriculumYearStatus(0,iYearLevel,iStat)
                    write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
                end do
            end do

            nCols = 1 + (MaxYearLevelCollege+1)*(ENLISTMENT_LOCKED-ENLISTMENT_NEEDED+1)
            write(device,AFORMAT) e_tr, &
                e_table, e_para, b_para, &
                '<table width="100%" cellspacing="0" cellpadding="0">', &
                ! table header line 1
                b_tr, b_td_nbsp_e_td ! college
            do iStat=ENLISTMENT_NEEDED,ENLISTMENT_LOCKED
                write(device,AFORMAT) '<td align="right" colspan="'//trim(itoa(1+MaxYearLevelCollege))//'">'// &
                    trim(txtStatusCode(iStat))//' - '//b_italic//trim(descStatusCode(iStat))//e_italic//e_td
            end do
            write(device,AFORMAT) e_tr

            ! table header line 2
            write(device,AFORMAT) b_tr, b_td//b_small//'COLLEGE'//e_small//e_td
            do iStat=ENLISTMENT_NEEDED,ENLISTMENT_LOCKED
                do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                    write(device,AFORMAT) b_tdar//trim(txtYear(iYearLevel+10))//e_td
                end do
            end do
            write(device,AFORMAT) e_tr, b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr

            colorIdx = 0
            nrows = 0
            do gdx=1,NumColleges
                if (CollegeCount(gdx) <= 0) cycle

                colorIdx = colorIdx+1
                nrows = nrows + 1

                ! accummulate for all curricula in college
                CurriculumYearStatus(0,:,:) = 0
                do ldx=1,NumCurricula
                    if (Curriculum(ldx)%CollegeIdx /= gdx) cycle ! not in college
                    CurriculumYearStatus(0,:,:) = CurriculumYearStatus(0,:,:) + CurriculumYearStatus(ldx,:,:)
                end do

                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(colorIdx,2))//'">'
                ! college
                if (fnCollegeCode/=0) then
                    write(device,AFORMAT) trim(make_href(fnAdvisingEnlistmentStatus, College(gdx)%Code, &
                            A1=College(gdx)%Code, &
                            !A2=CurrProgCode(ldx), &
                            !A3=Curriculum(ldx)%Code, &
                            !A4=itoa(iYearLevel), &
                            !A5=itoa(iStat), &
                            A9=thisTerm, pre=b_td, post=e_td))
                else
                    write(device,AFORMAT) b_td//trim(College(gdx)%Code)//e_td
                end if

                ! with year levels
                do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                    psum = sum(CurriculumYearStatus(0,iYearLevel,ADVISING_REGULAR:ADVISING_IRREGULAR)) - &
                        sum(CurriculumYearStatus(0,iYearLevel,ENLISTMENT_AUTOMATIC:ENLISTMENT_PAID))
                    write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
                end do
                do iStat=ENLISTMENT_AUTOMATIC,ENLISTMENT_LOCKED
                    do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                        psum = CurriculumYearStatus(0,iYearLevel,iStat)
                        write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
                    end do
                end do

                write(device,AFORMAT) e_tr

            end do
            write(device,AFORMAT) b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr

            ! column totals
            CurriculumYearStatus(0,:,:) = 0
            do ldx=1,NumCurricula
                CurriculumYearStatus(0,:,:) = CurriculumYearStatus(0,:,:) + CurriculumYearStatus(ldx,:,:)
            end do

            ! no name
            write(device,AFORMAT) b_tr, b_td//b_small//'TOTALS'//e_small//e_td
            ! status with year levels
            do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                psum = sum(CurriculumYearStatus(0,iYearLevel,ADVISING_REGULAR:ADVISING_IRREGULAR)) - &
                    sum(CurriculumYearStatus(0,iYearLevel,ENLISTMENT_AUTOMATIC:ENLISTMENT_PAID))
                write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
            end do
            do iStat=ENLISTMENT_AUTOMATIC,ENLISTMENT_LOCKED
                do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                    psum = CurriculumYearStatus(0,iYearLevel,iStat)
                    write(device,AFORMAT) b_tdar//trim(itoabz(psum))//e_td
                end do
            end do

            write(device,AFORMAT) e_tr, &
                e_table, e_para, &
                b_para, b_small, 'Legends : YNS - <i>Year not set</i>', &
                (nbsp//': '//trim(txtStatusCode(iStat))//' - '//b_italic//trim(descStatusCode(iStat))//e_italic, &
                iStat=ADVISING_EXCLUDED,ADVISING_FINISHED), e_small, e_para

        end if

    end subroutine advising_enlistment_status


    subroutine needs_analysis_and_preenlistment_menu(device, thisTerm, collegeIdx, levelOfDetail, reCount)
        integer, intent (in) :: device, thisTerm, collegeIdx, levelOfDetail
        logical, intent(in) :: reCount
        integer :: fnCollegeCode, fnProgramCode, fnStatus(ADVISING_EXCLUDED:PRIORITY_DISMISSED)
        integer :: iClass, idxProg, iCurr, iYearLevel, iStat

        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege

        if (reCount) call student_counts(thisTerm)

        tCollege = College(collegeIdx)%Code
        call html_comment('Advising and Needs Analysis in '//tCollege)

        write(device,AFORMAT) b_bold// &
            ' Advising and enlistment of '//trim(tCollege)//' students for '//trim(text_school_year(currentYear))//' / '// &
            trim(txtSemester(thisTerm+6))//trim(termQualifier(thisTerm+6))//e_bold

        write(device,AFORMAT) '<ul>', b_item, &
            trim(make_href(fnConfirmResetDemandForSubjects, 'ALL', A1=tCollege, A2='ALL', A9=thisTerm, &
            pre=b_bold//'Reset'//nbsp, post=red//' (careful!)'//e_color//e_bold)), ' : clear all '// &
            ' advised subjects AND preenlisted classes of '//trim(tCollege)//' students', &
            e_item

        write(device,AFORMAT) b_item, &
            '<b>Auto-advise students</b> by curriculum : click a number under column '// &
            PRIME//trim(txtStatusCode(ADVISING_NEEDED))//PRIME, &
            e_item

        write(device,AFORMAT) b_item, &
            '<b>Reset auto-advised subjects</b> of '//red//'ALL students'//e_color//' of a curricular program : click '// &
            b_italic//'Reset'//e_italic//' under column ', &
            PRIME//trim(txtStatusCode(ADVISING_NEEDED))//PRIME, &
            e_item

        write(device,AFORMAT) b_item, &
            '<b>Reset auto-advised subjects</b> of '//red//'irregular students only'//e_color// &
            ' of a curricular program : under ', &
            PRIME//trim(txtStatusCode(ADVISING_IRREGULAR))//PRIME//' columns, click ', &
            (nbsp//PRIME//trim(txtYear(iClass+10))//PRIME, iClass=1,2), ' ... link', &
            e_item

        write(device,AFORMAT) b_item, &
            '<b>Auto-update year level</b> of '//red//'irregular students with "Year not set"'//e_color// &
            ' : click YNS link under '//PRIME//trim(txtStatusCode(ADVISING_IRREGULAR))//PRIME, &
            e_item

        write(device,AFORMAT) b_item, &
            '<b>Auto-preenlist into blocks</b> by curriculum, by year : under ', &
            nbsp//PRIME//trim(txtStatusCode(ADVISING_REGULAR))//PRIME//' columns, click ', &
            (nbsp//PRIME//trim(txtYear(iClass+10))//PRIME, iClass=1,2), ' ... link', &
            e_item

        write(device,AFORMAT) b_item, &
            trim(make_href(fnConfirmTimetabling, 'ALL', A1=tCollege, A2='ALL', A9=thisTerm, &
            pre=b_bold//'Delist'//nbsp, post=red//' (careful!)'//e_color//e_bold//' : clear all preenlisted classes of '// &
            trim(tCollege)//' students who are NOT '//PRIME//trim(txtStatusCode(ENLISTMENT_LOCKED))//PRIME)), &
            e_item

        write(device,AFORMAT) b_item, &
            '<b>Delist students</b> by curriculum, by year : under '// &
            PRIME//trim(txtStatusCode(ENLISTMENT_AUTOMATIC))//PRIME, &
            PRIME//trim(txtStatusCode(ENLISTMENT_MANUAL))//PRIME, &
            ' columns, click ', &
            (nbsp//PRIME//trim(txtYear(iClass+10))//PRIME, iClass=1,2), ' ... link', &
            e_item

        write(device,AFORMAT) b_item, &
            '<b>Enumerate students</b> satisfying a specific curriculum+year+status combination : click a PROGRAM code'

        idxProg = 0
        iCurr = 0
        iYearLevel = 0
        iStat = 0
        fnCollegeCode = 0
        fnProgramCode = fnAdvisingEnlistmentStatus
        fnStatus = 0
        fnStatus(ADVISING_NEEDED) = fnConfirmUpdateDemandForSubjects
        fnStatus(ADVISING_REGULAR) = fnConfirmTimetabling
        fnStatus(ADVISING_IRREGULAR) = fnConfirmResetDemandForSubjects
        fnStatus(ENLISTMENT_AUTOMATIC) = fnConfirmTimetabling
        fnStatus(ENLISTMENT_MANUAL) = fnConfirmTimetabling

        call advising_enlistment_status(device, thisTerm, levelOfDetail, &
            collegeIdx, idxProg, iCurr, iYearLevel, iStat, fnCollegeCode, fnProgramCode, fnStatus, reCount)

        write(device,AFORMAT) e_item, '</ul>'

    end subroutine needs_analysis_and_preenlistment_menu
