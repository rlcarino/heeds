
#if defined UPLB

    subroutine confirm_advising_enlistment_action(device, thisTerm, thisAction)
        integer, intent (in) :: device, thisTerm, thisAction

        integer :: ierr, levelOfDetail, nextAction
        integer :: idxProg, iCurr, iYearLevel, iStat
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege

        character(len=MAX_LEN_CURRICULUM_CODE) :: tProgram, tCurriculum, tAction
        integer :: fnCollegeCode, fnProgramCode, fnStatus(ADVISING_EXCLUDED:PRIORITY_DISMISSED)
        character(len=127) :: header

        ! A1=College()%Code
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        targetCollege = index_to_college(tCollege)

        ! A2=CurrProgCode()
        call cgi_get_named_string(QUERY_STRING, 'A2', tProgram, ierr)
        tAction = tProgram
        if (trim(tProgram)=='ALL') then
            idxProg = NumCurricula
        elseif (len_trim(tProgram)==0) then
            tProgram = 'ALL'
            idxProg = NumCurricula
        else
            do idxProg=1,NumCurricula
                if (CurrProgCode(idxProg)==tProgram) exit
            end do
        end if

        ! A3=Curriculum()%Code
        call cgi_get_named_string(QUERY_STRING, 'A3', tCurriculum, ierr)
        iCurr = index_to_curriculum(tCurriculum)

        ! A4=iYearLevel
        call cgi_get_named_integer(QUERY_STRING, 'A4', iYearLevel, ierr)

        ! A5=iStat
        call cgi_get_named_integer(QUERY_STRING, 'A5', iStat, ierr)


        select case (thisAction)

            case (fnConfirmResetDemandForSubjects)
                nextAction = fnResetDemandForSubjects
                header = 'RESET advised subjects AND preenlisted classes of ALL students'

            case (fnConfirmUpdateDemandForSubjects)
                nextAction = fnUpdateDemandForSubjects
                header = 'AUTO-ADVISE students in '//tCollege

            case (fnConfirmTimetabling)
                nextAction = fnTimetabling
                header = trim(tAction)//' students in priority group '//PRIME//trim(txtStatusCode(iStat))//PRIME

            case (fnAdvisingEnlistmentStatus)

                levelOfDetail = 2 ! summary for specified college
                header = SPACE ! 'Summary of advising and enlistment of '//trim(tCollege)//' students'
                fnCollegeCode = fnAdvisingEnlistmentStatus
                fnProgramCode = fnAdvisingEnlistmentStatus
                fnStatus = 0

                if (iCurr>0) then ! curriculum specified
                    levelOfDetail = 15
                elseif (idxProg>0 .and. idxProg<NumCurricula) then ! program specified
                    levelOfDetail = 7
                else ! college code specified
                    levelOfDetail = 3
                end if

        end select


        call html_write_header(device, header)

        select case (thisAction)

            case (fnAdvisingEnlistmentStatus)

            case default
                call make_form_start(device, nextAction, A1=tCollege, A2=tAction, A3=tCurriculum, &
                    A4=itoa(iYearLevel), A5=itoa(iStat), A9=thisTerm)
                write(device,AFORMAT) &
                    b_para, 'This operation will '//trim(header), e_para, &
                    nbsp//nbsp//nbsp//nbsp//'<input type="submit" name="confirm" value="Cancel">', &
                    nbsp//nbsp//nbsp//nbsp//'<input type="submit" name="confirm" value="Continue">', &
                    red//' - click once only, may take a while; re-login in case of browser timeout.', &
                    e_color, &
                    e_form

                levelOfDetail = 1 ! summary for all colleges
                fnCollegeCode = fnAdvisingEnlistmentStatus
                fnProgramCode = 0
                fnStatus = 0
                if (thisAction==fnConfirmTimetabling) write(device,AFORMAT) &
                    b_italic, &
                    'Note: This operaton will also DELIST students in priority groups ', &
                    (nbsp//PRIME//trim(txtStatusCode(ierr))//PRIME, ierr=iStat+1,PRIORITY_DISMISSED), &
                    ' (except those with locked schedules)', &
                    e_italic
                write(device,AFORMAT) horizontal

        end select

        call advising_enlistment_status(device, thisTerm, levelOfDetail, targetCollege, idxProg, iCurr, iYearLevel, iStat, &
            fnCollegeCode, fnProgramCode, fnStatus, .true.)

        write(device,AFORMAT) horizontal

    end subroutine confirm_advising_enlistment_action


    subroutine execute_advising_enlistment_action(device, thisTerm, thisAction, mesg)
        integer, intent (in) :: device, thisTerm, thisAction
        character(len=*), intent(out) :: mesg

        integer :: ierr, nextAction, std, ldx, kdx, numEntries, idx
        integer :: idxProg, iCurr, iYearLevel, iStat
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_CURRICULUM_CODE) :: tProgram, tCurriculum, tConfirm, tAction
        character(len=127) :: header, advising_comment
        type (TYPE_PRE_ENLISTMENT) :: Advice

        mesg = SPACE
        nextAction = thisAction

        ! A1=College()%Code
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        targetCollege = index_to_college(tCollege)

        ! A2=CurrProgCode()
        call cgi_get_named_string(QUERY_STRING, 'A2', tProgram, ierr)
        tAction = tProgram
        if (trim(tProgram)=='ALL') then
            idxProg = NumCurricula
        elseif (len_trim(tProgram)==0) then
            tProgram = 'ALL'
            idxProg = NumCurricula
        else
            do idxProg=1,NumCurricula
                if (CurrProgCode(idxProg)==tProgram) exit
            end do
        end if

        ! A3=Curriculum()%Code
        call cgi_get_named_string(QUERY_STRING, 'A3', tCurriculum, ierr)
        iCurr = index_to_curriculum(tCurriculum)

        ! A4=iYearLevel
        call cgi_get_named_integer(QUERY_STRING, 'A4', iYearLevel, ierr)

        ! A5=iStat
        call cgi_get_named_integer(QUERY_STRING, 'A5', iStat, ierr)

        select case (thisAction)

            case (fnResetDemandForSubjects)
                header = 'RESET advised subjects AND preenlisted classes of ALL students'

            case (fnUpdateDemandForSubjects)
                header = 'AUTO-ADVISE students in '//tCollege

            case (fnTimetabling)
                header = trim(tAction)//' students in priority group '//PRIME//trim(txtStatusCode(iStat))//PRIME

        end select

        ! Continue/Cancel
        call cgi_get_named_string(QUERY_STRING, 'confirm', tConfirm, ierr)

        if (trim(tConfirm)/='Continue') then
            mesg = trim(header)//' - Operation cancelled.'
            nextAction = 0
        end if

        if (isRoleOfficial) then
            mesg = trim(header)//' - Operation cancelled. '//sorryMessageOfficial
            nextAction = 0
        end if

        select case (nextAction)

            case (fnResetDemandForSubjects)
                kdx = 0
                do std = 1,NumStudents+NumAdditionalStudents
                    !ldx = Student(std)%CurriculumIdx
                    !if (Curriculum(ldx)%CollegeIdx/=targetCollege) cycle
                    if (Student(std)%Enlistment(thisTerm)%statusAdvising==ADVISING_EXCLUDED) cycle
                    if (Student(std)%Enlistment(thisTerm)%statusEnlistment>=ENLISTMENT_LOCKED) cycle
                    call initialize_pre_enlistment(Student(std)%Enlistment(thisTerm))
                    call student_details_write(unitXML, dirSTUDENTS, std)
                    call finalgrades_write(currentYear, std)
                    kdx = kdx + 1
                    !numEntries = FCGI_puts(itoa(kdx)//trim(Student(std)%StdNo//Student(std)%Name//Curriculum(ldx)%Code)// &
                    !    linebreak//NUL)
                end do
                call html_college_links(device, NumColleges, trim(header)//' : records changed = '//itoa(kdx))
                isDirtyData = .true.

            case (fnUpdateDemandForSubjects)

                call html_write_header(device, header)
                write(device,AFORMAT) '<table border="0" width="75%">', &
                    b_tr//b_thal//'#'//e_th//b_thal//'STDNO'//e_th, &
                    b_thal//'NAME OF STUDENT'//e_th//b_thal//'PROGRAM'//e_th//b_thal//'REMARK'//e_th, e_tr

                kdx = 0
                do ierr = 1,NumStudents+NumAdditionalStudents
                    std = StdRank(ierr)
                    ldx = Student(std)%CurriculumIdx
                    if (Curriculum(ldx)%CollegeIdx/=targetCollege) cycle
                    if (Student(std)%Enlistment(thisTerm)%statusAdvising/=ADVISING_NEEDED) cycle ! skip
                    kdx = kdx+1
                    numEntries = Student(std)%Enlistment(thisTerm)%NPriority + Student(std)%Enlistment(thisTerm)%NAlternates + &
                        Student(std)%Enlistment(thisTerm)%NCurrent

                    call advise_student (std, thisTerm, Advice, advising_comment)
                    !numEntries = FCGI_puts (itoa(kdx)//trim(Student(std)%StdNo//Student(std)%Name//Curriculum(ldx)%Code)// &
                    !    linebreak//NUL)

                    write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(kdx,2))//'">'// &
                        b_td//trim(itoa(kdx))//DOT//e_td//b_td//Student(std)%StdNo//e_td, &
                        b_td//trim(Student(std)%Name)//e_td, b_td//trim(Curriculum(ldx)%Code)//e_td, &
                        trim(make_href(fnEditCheckList, txtYear(Advice%levelYear+10), A1=Student(std)%StdNo, &
                            pre=b_td, post=' Year'//e_td)), e_tr

                    NumEnlistment(thisTerm) = NumEnlistment(thisTerm) - numEntries + Advice%lenSubject
                    Student(std)%Enlistment(thisTerm) = Advice
                    call student_details_write(unitXML, dirSTUDENTS, std)
                    call finalgrades_write(currentYear, std)

                    if (kdx>=maxStudentsForNeedsAnalysis) exit

                end do
                isDirtyData = .true.

                write(device,AFORMAT) '</table>', b_para

                if (kdx>=maxStudentsForNeedsAnalysis) then
                    write(device,AFORMAT) b_bold, &
                        red//'Auto-advising terminated after '//trim(itoa(kdx))//' students to avoid timeout', &
                        linebreak, linebreak, 'Run auto-advising again for '//tCollege//e_color, e_bold
                end if
                write(device,AFORMAT) e_para

            case (fnUpdateAdviceBasis)

                call html_write_header(device, header)
                write(device,AFORMAT) '<table border="0" width="75%">', &
                    b_tr//b_thal//'#'//e_th//b_thal//'STDNO'//e_th, &
                    b_thal//'NAME OF STUDENT'//e_th//b_thal//'PROGRAM'//e_th//b_thal//'REMARK'//e_th, e_tr

                kdx = 0
                do ierr = 1,NumStudents+NumAdditionalStudents
                    std = StdRank(ierr)
                    ldx = Student(std)%CurriculumIdx

                    if (CurrProgCode(ldx)/=tProgram) cycle ! not idxProg
                    if (Student(std)%Enlistment(thisTerm)%levelYear/=YEAR_NOT_SET) cycle ! year level is set
                    kdx = kdx+1

                    call advise_student (std, thisTerm, Advice, advising_comment)

                    ! copy manually advised subjects and enlisted classes
                    Student(std)%Enlistment(thisTerm)%UnitsEarned = Advice%UnitsEarned
                    Student(std)%Enlistment(thisTerm)%levelClassification = Advice%levelClassification
                    Student(std)%Enlistment(thisTerm)%levelYear = Advice%levelYear
                    Student(std)%Enlistment(thisTerm)%levelPriority = Advice%levelPriority
                    Student(std)%Enlistment(thisTerm)%statusScholastic = Advice%statusScholastic
                    Student(std)%Enlistment(thisTerm)%codeNSTP = Advice%codeNSTP
                    Student(std)%Enlistment(thisTerm)%codeConditional = Advice%codeConditional
                    Student(std)%Enlistment(thisTerm)%NRemaining = Advice%NRemaining
                    Student(std)%Enlistment(thisTerm)%MissingPOCW = Advice%MissingPOCW
                    Student(std)%Enlistment(thisTerm)%AllowedLoad = 0.0
                    do idx=1,Student(std)%Enlistment(thisTerm)%lenSubject
                        Student(std)%Enlistment(thisTerm)%AllowedLoad = Student(std)%Enlistment(thisTerm)%AllowedLoad + &
                            Subject(Student(std)%Enlistment(thisTerm)%Subject(idx))%Units
                    end do
                    call student_details_write(unitXML, dirSTUDENTS, std)
                    call finalgrades_write(currentYear, std)

                    write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(kdx,2))//'">'// &
                        b_td//trim(itoa(kdx))//DOT//e_td//b_td//Student(std)%StdNo//e_td, &
                        b_td//trim(Student(std)%Name)//e_td, b_td//trim(Curriculum(ldx)%Code)//e_td, &
                        trim(make_href(fnEditCheckList, txtYear(Student(std)%Enlistment(thisTerm)%levelYear+10), &
                            A1=Student(std)%StdNo, pre=b_td, post=' Year'//e_td)), e_tr

                    if (kdx>=maxStudentsForNeedsAnalysis) exit

                end do
                isDirtyData = .true.

                write(device,AFORMAT) '</table>', b_para

                if (kdx>=maxStudentsForNeedsAnalysis) then
                    write(device,AFORMAT) b_bold, &
                        red//'Auto-update of year level terminated after '//trim(itoa(kdx))//' students to avoid timeout', &
                        linebreak, linebreak, 'Run auto-update year level again for '//tCollege//e_color, e_bold
                end if
                write(device,AFORMAT) e_para

            case (fnTimetabling)

                call html_write_header(device, trim(header)//' for '//trim(txtSemester(thisTerm+6))//termQualifier(thisTerm+3))

                write(device,AFORMAT) b_bold//'Begin'//e_bold, '<pre>'

                ! delist auto-preenlisted students of specified priority group and lower priority groups
                ldx = 0
                do std = 1,NumStudents+NumAdditionalStudents
                    if (Student(std)%Enlistment(thisTerm)%levelPriority<iStat) cycle ! higher-priority group or specified-priority group
                    if (Student(std)%Enlistment(thisTerm)%statusEnlistment>=ENLISTMENT_LOCKED) cycle ! locked, paid
                    Student(std)%Enlistment(thisTerm)%Section(:) = 0
                    Student(std)%Enlistment(thisTerm)%Grade(:) = 0
                    Student(std)%Enlistment(thisTerm)%statusEnlistment = ENLISTMENT_NEEDED
                    call student_details_write(unitXML, dirSTUDENTS, std)
                    call finalgrades_write(currentYear, std)
                    ldx = ldx+1
                end do
                write(device,*) '# of students delisted in Priority Group '//trim(txtStatusCode(iStat))// &
                    ' and lower priority groups = '//itoa(ldx)

                if (trim(tAction)=='PREENLIST') then
                    tArray = 0
                    do std = 1,NumStudents+NumAdditionalStudents
                        if (Student(std)%Enlistment(thisTerm)%levelPriority==iStat) tArray(std) = 1
                    end do
                    write(device,*) '# of students selected = '//itoa(sum(tArray))
                    call generate_timetables(device, thisTerm, tArray, ierr )
                    write(device,*) 'exit code = '//itoa(ierr)
                    if (ierr==0) then
                        do std=1,NumStudents+NumAdditionalStudents
                            if (tArray(std)==1) then
                                call student_details_write(unitXML, dirSTUDENTS, std)
                                call finalgrades_write(currentYear, std)
                            end if
                        end do
                    end if
                end if

                write(device,AFORMAT) '</pre>', b_bold//'End'//e_bold

                isDirtyData = .true.


            case default
                call html_college_links(device, NumColleges, mesg)
                return

        end select

        write(device,AFORMAT) b_para
        call needs_analysis_and_preenlistment_menu(device, thisTerm, targetCollege, 1, .true.)
        write(device,AFORMAT) e_para, horizontal

    end subroutine execute_advising_enlistment_action


#else


    subroutine confirm_advising_enlistment_action(device, thisTerm, thisAction)
        integer, intent (in) :: device, thisTerm, thisAction

        integer :: ierr, levelOfDetail, nextAction
        integer :: idxProg, iCurr, iYearLevel, iStat
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_CURRICULUM_CODE) :: tProgram, tCurriculum
        integer :: fnCollegeCode, fnProgramCode, fnStatus(ADVISING_EXCLUDED:PRIORITY_DISMISSED)
        character(len=127) :: header
        !character(len=2*MAX_LEN_CURRICULUM_CODE) :: tAction

        ! A1=College()%Code
        call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)
        targetDepartment = index_to_dept(tDepartment)
        targetCollege = Department(targetDepartment)%CollegeIdx
        tCollege = College(targetCollege)%Code

        ! A2=CurrProgCode()
        call cgi_get_named_string(QUERY_STRING, 'A2', tProgram, ierr)
        if (trim(tProgram)=='ALL') then
            idxProg = NumCurricula
        elseif (len_trim(tProgram)==0) then
            tProgram = 'ALL'
            idxProg = NumCurricula
        else
            do idxProg=1,NumCurricula
                if (CurrProgCode(idxProg)==tProgram) exit
            end do
        end if

        ! A3=Curriculum()%Code
        call cgi_get_named_string(QUERY_STRING, 'A3', tCurriculum, ierr)
        iCurr = index_to_curriculum(tCurriculum)

        ! A4=iYearLevel
        call cgi_get_named_integer(QUERY_STRING, 'A4', iYearLevel, ierr)

        ! A5=iStat
        call cgi_get_named_integer(QUERY_STRING, 'A5', iStat, ierr)

        select case (thisAction)

            case (fnConfirmResetDemandForSubjects)
                nextAction = fnResetDemandForSubjects
                if (trim(tProgram)=='ALL') then
                    header = 'RESET advised subjects AND preenlisted classes of '//trim(tCollege)//' students'
                elseif (iStat==ADVISING_IRREGULAR) then
                    if (iYearLevel==YEAR_NOT_SET) then
                        header = 'AUTO-UPDATE the year level of irregular '//trim(tProgram)//' students with "Year not set"'
                    else
                        header = 'RESET auto-advised subjects of irregular '//trim(txtYear(iYearLevel+10))//' year '// &
                            trim(tProgram)//' students only'
                    end if
                else
                    header = 'RESET auto-advised subjects of ALL '//trim(tProgram)//' students'
                end if

            case (fnConfirmUpdateDemandForSubjects)
                nextAction = fnUpdateDemandForSubjects
                header = 'AUTO-ADVISE '//b_bold//trim(tProgram)//e_bold//' students with status'// &
                    nbsp//PRIME//trim(txtStatusCode(ADVISING_NEEDED))//PRIME

            case (fnConfirmTimetabling)
                nextAction = fnTimetabling

                if (trim(tProgram)=='ALL') then ! .and. targetCollege==NumColleges) then
                    header = 'DELIST ALL '//trim(tCollege)//' students who are NOT '// &
                        PRIME//trim(txtStatusCode(ENLISTMENT_LOCKED))//PRIME
                else
                    select case (iStat)
                        case (ADVISING_REGULAR)
                            header = 'PREENLIST '//trim(tProgram)//' students with status '// &
                                PRIME//trim(txtStatusCode(iStat))//PRIME// &
                                ' and year level '//PRIME//trim(txtYear(iYearLevel+10))//PRIME
                        !case (ENLISTMENT_MANUAL, ENLISTMENT_AUTOMATIC)
                        case default
                            header = 'DELIST '//trim(tProgram)//' students with status '// &
                                PRIME//trim(txtStatusCode(iStat))//PRIME// &
                                ' and year level '//PRIME//trim(txtYear(iYearLevel+10))//PRIME
                    end select
                end if

            case (fnAdvisingEnlistmentStatus)

                levelOfDetail = 2 ! summary for specified college
                header = SPACE ! 'Summary of advising and enlistment of '//trim(tCollege)//' students'
                fnCollegeCode = fnAdvisingEnlistmentStatus
                fnProgramCode = fnAdvisingEnlistmentStatus
                fnStatus = 0

                if (trim(tProgram)=='ALL') then
                    if (targetCollege==NumColleges) then
                        levelOfDetail = 1 ! summary for all colleges
                    else
                        levelOfDetail = 2 ! summary for a program
                    end if
                elseif (iCurr>0) then
                    levelOfDetail =  8 ! list students for specified curriculum
                    fnCollegeCode = 0
                    fnStatus = fnAdvisingEnlistmentStatus
                else
                    levelOfDetail =  4 ! summary for specified curriculum
                    fnCollegeCode = 0
                    fnStatus = fnAdvisingEnlistmentStatus
                end if

        end select


        call html_write_header(device, header)

        select case (thisAction)

            case (fnAdvisingEnlistmentStatus)

            case default
                call make_form_start(device, nextAction, A1=tCollege, A2=tProgram, A3=tCurriculum, &
                    A4=itoa(iYearLevel), A5=itoa(iStat), A9=thisTerm)
                write(device,AFORMAT) &
                    b_para, 'This operation will '//trim(header), e_para, &
                    nbsp//nbsp//nbsp//nbsp//'<input type="submit" name="confirm" value="Cancel">', &
                    nbsp//nbsp//nbsp//nbsp//'<input type="submit" name="confirm" value="Continue">', &
                    red//' - click once only, may take a while; re-login in case of browser timeout.', &
                    e_color, &
                    e_form

                levelOfDetail = 2 ! summary for specified college
                fnCollegeCode = 0
                fnProgramCode = fnAdvisingEnlistmentStatus
                write(device,AFORMAT) horizontal
                fnStatus = 0

        end select

        call advising_enlistment_status(device, thisTerm, levelOfDetail, targetCollege, idxProg, iCurr, iYearLevel, iStat, &
            fnCollegeCode, fnProgramCode, fnStatus, .true.)

        write(device,AFORMAT) horizontal

    end subroutine confirm_advising_enlistment_action


    subroutine execute_advising_enlistment_action(device, thisTerm, thisAction, mesg)
        integer, intent (in) :: device, thisTerm, thisAction
        character(len=*), intent(out) :: mesg

        integer :: ierr, nextAction, std, ldx, kdx, numEntries, idx
        integer :: idxProg, iCurr, iYearLevel, iStat
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_CURRICULUM_CODE) :: tProgram, tCurriculum, tConfirm
        character(len=127) :: header, advising_comment
        type (TYPE_PRE_ENLISTMENT) :: Advice

        mesg = SPACE
        nextAction = thisAction

        ! A1=College()%Code
        call cgi_get_named_string(QUERY_STRING, 'A1', tDepartment, ierr)
        targetDepartment = index_to_dept(tDepartment)
        targetCollege = Department(targetDepartment)%CollegeIdx
        tCollege = College(targetCollege)%Code

        ! A2=CurrProgCode()
        call cgi_get_named_string(QUERY_STRING, 'A2', tProgram, ierr)
        if (trim(tProgram)=='ALL') then
            idxProg = NumCurricula
        elseif (len_trim(tProgram)==0) then
            tProgram = 'ALL'
            idxProg = NumCurricula
        else
            do idxProg=1,NumCurricula
                if (CurrProgCode(idxProg)==tProgram) exit
            end do
        end if

        ! A3=Curriculum()%Code
        call cgi_get_named_string(QUERY_STRING, 'A3', tCurriculum, ierr)
        iCurr = index_to_curriculum(tCurriculum)

        ! A4=iYearLevel
        call cgi_get_named_integer(QUERY_STRING, 'A4', iYearLevel, ierr)

        ! A5=iStat
        call cgi_get_named_integer(QUERY_STRING, 'A5', iStat, ierr)

        select case (thisAction)

            case (fnResetDemandForSubjects)
                if (trim(tProgram)=='ALL') then
                    header = 'RESET advised subjects AND preenlisted classes of '//trim(tCollege)//' students'
                elseif (iStat==ADVISING_IRREGULAR) then
                    if (iYearLevel==YEAR_NOT_SET) then
                        nextAction = fnUpdateAdviceBasis
                        header = 'AUTO-UPDATE the year level of irregular '//trim(tProgram)//' students with "Year not set"'
                    else
                        header = 'RESET auto-advised subjects of irregular '//trim(txtYear(iYearLevel+10))//' year '// &
                            trim(tProgram)//' students only'
                    end if
                else
                    header = 'RESET auto-advised subjects of ALL '//trim(tProgram)//' students'
                end if

            case (fnUpdateDemandForSubjects)
                header = 'AUTO-ADVISE '//b_bold//trim(tProgram)//e_bold//' students with status'// &
                    nbsp//PRIME//trim(txtStatusCode(ADVISING_NEEDED))//PRIME

            case (fnTimetabling)

                if (trim(tProgram)=='ALL') then ! .and. targetCollege==NumColleges) then
                    header = 'DELIST ALL '//trim(tCollege)//' students who are NOT '// &
                        PRIME//trim(txtStatusCode(ENLISTMENT_LOCKED))//PRIME
                else
                    select case (iStat)
                        case (ADVISING_REGULAR)
                            header = 'PREENLIST '//trim(tProgram)//' students with status '// &
                                PRIME//trim(txtStatusCode(iStat))//PRIME// &
                                ' and year level '//PRIME//trim(txtYear(iYearLevel+10))//PRIME
                        !case (ENLISTMENT_MANUAL, ENLISTMENT_AUTOMATIC)
                        case default
                            header = 'DELIST '//trim(tProgram)//' students with status '// &
                                PRIME//trim(txtStatusCode(iStat))//PRIME// &
                                ' and year level '//PRIME//trim(txtYear(iYearLevel+10))//PRIME
                    end select
                end if

        end select

        ! Continue/Cancel
        call cgi_get_named_string(QUERY_STRING, 'confirm', tConfirm, ierr)

        if (trim(tConfirm)/='Continue') then
            mesg = trim(header)//' - Operation cancelled.'
            nextAction = 0
        end if

        if (isRoleOfficial) then
            mesg = trim(header)//' - Operation cancelled. '//sorryMessageOfficial
            nextAction = 0
        end if

        select case (nextAction)

            case (fnResetDemandForSubjects)
                kdx = 0
                if (trim(tProgram)=='ALL') then
                    do std = 1,NumStudents+NumAdditionalStudents
                        ldx = Student(std)%CurriculumIdx
                        if (Curriculum(ldx)%CollegeIdx/=targetCollege) cycle
                        if (Student(std)%Enlistment(thisTerm)%statusAdvising==ADVISING_EXCLUDED) cycle
                        call initialize_pre_enlistment(Student(std)%Enlistment(thisTerm))
                        call student_details_write(unitXML, dirSTUDENTS, std)
                        call finalgrades_write(currentYear, std)
                        kdx = kdx + 1
                        !numEntries = FCGI_puts(itoa(kdx)//trim(Student(std)%StdNo//Student(std)%Name//Curriculum(ldx)%Code)// &
                        !    linebreak//NUL)
                    end do
                elseif (iStat==ADVISING_IRREGULAR) then
                    do std = 1,NumStudents+NumAdditionalStudents
                        ldx = Student(std)%CurriculumIdx
                        if (CurrProgCode(ldx)/=tProgram) cycle
                        if (Student(std)%Enlistment(thisTerm)%statusAdvising==ADVISING_IRREGULAR .and. &
                                Student(std)%Enlistment(thisTerm)%statusEnlistment==ENLISTMENT_NEEDED .and. &
                                Student(std)%Enlistment(thisTerm)%levelYear==iYearLevel) then
                            call initialize_pre_enlistment(Student(std)%Enlistment(thisTerm))
                            call student_details_write(unitXML, dirSTUDENTS, std)
                            call finalgrades_write(currentYear, std)
                            kdx = kdx + 1
                            !numEntries = FCGI_puts(itoa(kdx)//trim(Student(std)%StdNo//Student(std)%Name//Curriculum(ldx)%Code)// &
                            !    linebreak//NUL)
                        end if
                    end do
                else
                    do std = 1,NumStudents+NumAdditionalStudents
                        ldx = Student(std)%CurriculumIdx
                        if (CurrProgCode(ldx)/=tProgram) cycle
                        if (Student(std)%Enlistment(thisTerm)%statusAdvising==ADVISING_REGULAR .or. &
                            Student(std)%Enlistment(thisTerm)%statusAdvising==ADVISING_IRREGULAR .or. &
                            Student(std)%Enlistment(thisTerm)%statusAdvising==ADVISING_NO_SUBJECTS) then
                            call initialize_pre_enlistment(Student(std)%Enlistment(thisTerm))
                            call student_details_write(unitXML, dirSTUDENTS, std)
                            call finalgrades_write(currentYear, std)
                            kdx = kdx + 1
                            !numEntries = FCGI_puts(itoa(kdx)//trim(Student(std)%StdNo//Student(std)%Name//Curriculum(ldx)%Code)// &
                            !    linebreak//NUL)
                        end if
                    end do
                end if

                call html_college_links(device, targetCollege, trim(header)//' : records changed = '//itoa(kdx))


            case (fnUpdateDemandForSubjects)

                call html_write_header(device, header)
                write(device,AFORMAT) '<table border="0" width="75%">', &
                    b_tr//b_thal//'#'//e_th//b_thal//'STDNO'//e_th, &
                    b_thal//'NAME OF STUDENT'//e_th//b_thal//'PROGRAM'//e_th//b_thal//'REMARK'//e_th, e_tr

                kdx = 0
                do ierr = 1,NumStudents+NumAdditionalStudents
                    std = StdRank(ierr)
                    ldx = Student(std)%CurriculumIdx

                    if (CurrProgCode(ldx)/=tProgram) cycle ! not idxProg
                    if (Student(std)%Enlistment(thisTerm)%statusAdvising/=ADVISING_NEEDED) cycle ! skip
                    kdx = kdx+1
                    numEntries = Student(std)%Enlistment(thisTerm)%NPriority + Student(std)%Enlistment(thisTerm)%NAlternates + &
                        Student(std)%Enlistment(thisTerm)%NCurrent

                    call advise_student (std, thisTerm, Advice, advising_comment)
                    !numEntries = FCGI_puts (itoa(kdx)//trim(Student(std)%StdNo//Student(std)%Name//Curriculum(ldx)%Code)// &
                    !    linebreak//NUL)

                    write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(kdx,2))//'">'// &
                        b_td//trim(itoa(kdx))//DOT//e_td//b_td//Student(std)%StdNo//e_td, &
                        b_td//trim(Student(std)%Name)//e_td, b_td//trim(Curriculum(ldx)%Code)//e_td, &
                        trim(make_href(fnEditCheckList, txtYear(Advice%levelYear+10), A1=Student(std)%StdNo, &
                            pre=b_td, post=' Year'//e_td)), e_tr

                    NumEnlistment(thisTerm) = NumEnlistment(thisTerm) - numEntries + Advice%lenSubject
                    Student(std)%Enlistment(thisTerm) = Advice
                    call student_details_write(unitXML, dirSTUDENTS, std)
                    call finalgrades_write(currentYear, std)

                    if (kdx>=maxStudentsForNeedsAnalysis) exit

                end do

                write(device,AFORMAT) '</table>', b_para

                if (kdx>=maxStudentsForNeedsAnalysis) then
                    write(device,AFORMAT) b_bold, &
                        red//'Auto-advising terminated after '//trim(itoa(kdx))//' students to avoid timeout', &
                        linebreak, linebreak, 'Run auto-advising again for '//tProgram//e_color, e_bold
                end if
                write(device,AFORMAT) e_para, b_para

                call needs_analysis_and_preenlistment_menu(device, thisTerm, targetCollege, 2, .true.)
                write(device,AFORMAT) e_para, horizontal

            case (fnUpdateAdviceBasis)

                call html_write_header(device, header)
                write(device,AFORMAT) '<table border="0" width="75%">', &
                    b_tr//b_thal//'#'//e_th//b_thal//'STDNO'//e_th, &
                    b_thal//'NAME OF STUDENT'//e_th//b_thal//'PROGRAM'//e_th//b_thal//'REMARK'//e_th, e_tr

                kdx = 0
                do ierr = 1,NumStudents+NumAdditionalStudents
                    std = StdRank(ierr)
                    ldx = Student(std)%CurriculumIdx

                    if (CurrProgCode(ldx)/=tProgram) cycle ! not idxProg
                    if (Student(std)%Enlistment(thisTerm)%statusAdvising/=ADVISING_IRREGULAR) cycle ! not irregular
                    if (Student(std)%Enlistment(thisTerm)%levelYear/=YEAR_NOT_SET) cycle ! year level is set
                    kdx = kdx+1

                    call advise_student (std, thisTerm, Advice, advising_comment)

                    ! copy manually advised subjects and enlisted classes
                    Student(std)%Enlistment(thisTerm)%UnitsEarned = Advice%UnitsEarned
                    Student(std)%Enlistment(thisTerm)%levelClassification = Advice%levelClassification
                    Student(std)%Enlistment(thisTerm)%levelYear = Advice%levelYear
                    Student(std)%Enlistment(thisTerm)%levelPriority = Advice%levelPriority
                    Student(std)%Enlistment(thisTerm)%statusScholastic = Advice%statusScholastic
                    Student(std)%Enlistment(thisTerm)%codeNSTP = Advice%codeNSTP
                    Student(std)%Enlistment(thisTerm)%codeConditional = Advice%codeConditional
                    Student(std)%Enlistment(thisTerm)%NRemaining = Advice%NRemaining
                    Student(std)%Enlistment(thisTerm)%MissingPOCW = Advice%MissingPOCW
                    Student(std)%Enlistment(thisTerm)%AllowedLoad = 0.0
                    do idx=1,Student(std)%Enlistment(thisTerm)%lenSubject
                        Student(std)%Enlistment(thisTerm)%AllowedLoad = Student(std)%Enlistment(thisTerm)%AllowedLoad + &
                            Subject(Student(std)%Enlistment(thisTerm)%Subject(idx))%Units
                    end do
                    call student_details_write(unitXML, dirSTUDENTS, std)


                    write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(kdx,2))//'">'// &
                        b_td//trim(itoa(kdx))//DOT//e_td//b_td//Student(std)%StdNo//e_td, &
                        b_td//trim(Student(std)%Name)//e_td, b_td//trim(Curriculum(ldx)%Code)//e_td, &
                        trim(make_href(fnEditCheckList, txtYear(Student(std)%Enlistment(thisTerm)%levelYear+10), &
                            A1=Student(std)%StdNo, pre=b_td, post=' Year'//e_td)), e_tr

                    if (kdx>=maxStudentsForNeedsAnalysis) exit

                end do

                write(device,AFORMAT) '</table>', b_para

                if (kdx>=maxStudentsForNeedsAnalysis) then
                    write(device,AFORMAT) b_bold, &
                        red//'Auto-update of year level terminated after '//trim(itoa(kdx))//' students to avoid timeout', &
                        linebreak, linebreak, 'Run auto-update year level again for '//tProgram//e_color, e_bold
                end if
                write(device,AFORMAT) e_para, b_para

                call needs_analysis_and_preenlistment_menu(device, thisTerm, targetCollege, 2, .true.)
                write(device,AFORMAT) e_para, horizontal


            case (fnTimetabling)

                call html_write_header(device, header)
                write(device,AFORMAT) '<table border="0" width="75%">', &
                    b_tr//b_thal//'#'//e_th//b_thal//'STDNO'//e_th, &
                    b_thal//'NAME OF STUDENT'//e_th//b_thal//'PROGRAM'//e_th//b_thal//'REMARK'//e_th, e_tr

                ! mark students to be included
                tArray = 0

                select case (iStat)

                    case (ADVISING_REGULAR) ! tAction = 'PREENLIST'
                        do std = 1,NumStudents+NumAdditionalStudents
                            ldx = Student(std)%CurriculumIdx
                            if (CurrProgCode(ldx)/=tProgram) cycle
                            if (Student(std)%Enlistment(thisTerm)%levelYear/=iYearLevel) cycle
                            if (Student(std)%Enlistment(thisTerm)%statusAdvising==iStat) then
                                tArray(std) = 1
                                !ierr = FCGI_puts(trim(Student(std)%StdNo//Student(std)%Name//Curriculum(ldx)%Code)// &
                                !    linebreak//NUL)
                            end if
                        end do
                        call add_to_blocks(device, thisTerm, tArray )

                    !case (ENLISTMENT_MANUAL, ENLISTMENT_AUTOMATIC)
                    case default ! tAction = 'DELIST'
                        if (trim(tProgram)/='ALL') then
                            do std = 1,NumStudents+NumAdditionalStudents
                                ldx = Student(std)%CurriculumIdx
                                if (CurrProgCode(ldx)/=tProgram) cycle
                                if (Student(std)%Enlistment(thisTerm)%statusEnlistment/=iStat) cycle
                                if (Student(std)%Enlistment(thisTerm)%levelYear/=iYearLevel) cycle
                                tArray(std) = 1
                                !ierr = FCGI_puts(trim(Student(std)%StdNo//Student(std)%Name//Curriculum(ldx)%Code)// &
                                !    linebreak//NUL)
                            end do
                        else ! ALL
                            do std = 1,NumStudents+NumAdditionalStudents
                                ldx = Student(std)%CurriculumIdx
                                if (Curriculum(ldx)%CollegeIdx/=targetCollege) cycle
                                if (Student(std)%Enlistment(thisTerm)%statusEnlistment>=ENLISTMENT_AUTOMATIC .and. &
                                    Student(std)%Enlistment(thisTerm)%statusEnlistment<=ENLISTMENT_CONFIRMED) then ! not locked
                                        tArray(std) = 1
                                    !ierr = FCGI_puts (trim(Student(std)%StdNo//Student(std)%Name//Curriculum(ldx)%Code)// &
                                    !   linebreak//NUL)
                                end if
                            end do
                        end if
                        kdx = 0
                        do ldx = 1,NumStudents+NumAdditionalStudents
                            std = StdRank(ldx)
                            if (tArray(std)==0) cycle
                            kdx = kdx+1
                            Student(std)%Enlistment(thisTerm)%Section(:) = 0
                            Student(std)%Enlistment(thisTerm)%Grade(:) = 0
                            Student(std)%Enlistment(thisTerm)%statusEnlistment = ENLISTMENT_NEEDED

                            call student_details_write(unitXML, dirSTUDENTS, std)
                            call finalgrades_write(currentYear, std)

                            write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(kdx,2))//'">'// &
                                b_td//trim(itoa(kdx))//DOT//e_td//b_td//Student(std)%StdNo//e_td, &
                                b_td//trim(Student(std)%Name)//e_td, &
                                b_td//trim(Curriculum(Student(std)%CurriculumIdx)%Code)//e_td, &
                                b_td//'Delisted'//e_td, e_tr
                        end do

                end select

                write(device,AFORMAT) '</table>'
                write(device,AFORMAT) b_para
                call needs_analysis_and_preenlistment_menu(device, thisTerm, targetCollege, 2, .true.)
                write(device,AFORMAT) e_para, horizontal

            case default
                call html_college_links(device, targetCollege, mesg)

        end select

    end subroutine execute_advising_enlistment_action

#endif


    subroutine download_file(device, thisTerm)
        integer, intent(in) :: device, thisTerm

        character (len=MAX_LEN_FILE_PATH) :: fileName, backupFile
!#if defined GLNX
        character (len=MAX_LEN_XML_LINE) :: line
!#endif
        integer :: errNo, i, j
        character (len=MAX_LEN_PASSWD_VAR) :: XMLfile, Password

        call cgi_get_named_string(QUERY_STRING, 'A1', XMLfile, errNo)

        call html_comment('download_file('//trim(XMLfile)//')')

        select case(trim(XMLfile))

            case ('PASSWORDS-TEACHERS.CSV')

                write(device,AFORMAT) &
                    '#', &
                    '#  !!!!!!!!! FOR THE REGISTRAR''S EYES ONLY !!!!!!!!! ', &
                    '#  !!!!!!!!!     PASSWORDS-TEACHERS.CSV     !!!!!!!!! ', &
                    '#', &
                    '# Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                                FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
                    '# NAME - name of teacher', &
                    '# UNIT - academic unit', &
                    '# USERNAME - HEEDS login name', &
                    '# PASSWORD - HEEDS password', &
                    '# ROLE - HEEDS role', &
                    '# COMMENT - Date received/Initials', &
                    '#', &
                    '"NAME","UNIT","USERNAME","PASSWORD","ROLE","COMMENT"'

                do i=1,NumTeachers+NumAdditionalTeachers
                    if (Teacher(i)%Role==SYSAD ) cycle
                    call getPassword_of_teacher(i, Password)
                    write(device,AFORMAT) &
                        '"'//Teacher(i)%Name//'","'// &
                             Department(Teacher(i)%DeptIdx)%Code//'","'// &
                             Teacher(i)%TeacherId//'","'// &
                             Password(:MAX_LEN_PASSWORD)//'","'// &
                             Teacher(i)%Role//'"'
                end do

            case ('PASSWORDS-STUDENTS.CSV')

                write(device,AFORMAT) &
                    '#', &
                    '#  !!!!!!!!! FOR THE REGISTRAR''S EYES ONLY !!!!!!!!! ', &
                    '#  !!!!!!!!!     PASSWORDS-STUDENTS.CSV     !!!!!!!!! ', &
                    '#', &
                    '# Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                                FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
                    '# NAME - name of student', &
                    '# CURRICULUM - curriculum', &
                    '# USERNAME - HEEDS login name', &
                    '# PASSWORD - HEEDS password', &
                    '# COMMENT - Date received/Initials', &
                    '#', &
                    '"NAME","CURRICULUM","USERNAME","PASSWORD","COMMENT"'

                do j=1,NumStudents+NumAdditionalStudents
                    i = StdRank(j)
                    call getPassword_of_student(i, Password)
                    write(device,AFORMAT) &
                        '"'//Student(i)%Name//'","'// &
                             Curriculum(Student(i)%CurriculumIdx)%Code//'","'// &
                             Student(i)%StdNo//'","'// &
                             Password(:MAX_LEN_PASSWORD)//'"'
                end do


            case ('BACKUP.XML')

                backupFile = trim(UniversityCode)//'-BACKUP.XML-'//currentDate//DASH//currentTime(:6)

                ! write backup
                isDirtyData = .true.
                fileName = trim(pathToYear)//trim(UniversityCode)//'-BACKUP.XML'
                call backup_write(fileName)

                ! copy to download directory
                call system(cpCmd//trim(fileName)//SPACE//trim(dirDOWNLOADS)//trim(backupFile), errNo )
                call terminate(errNo, 'Error in copying backup file to '//dirDOWNLOADS)

                ! compress
                fileName = trim(dirDOWNLOADS)//backupFile
                call system('gzip -r '//trim(fileName), errNo )
                call terminate(errNo, 'Error in compressing backup file for download')

                ! make download link
                fileName = trim(backupFile)//'.gz'
                call html_write_header(device, trim(fileName)// ' created', SPACE)
                write(device,AFORMAT)'Right-click <a href="'//trim(urlDOWNLOADS)//trim(fileName)// &
                    '">here</a>, then "Save Link As ..."'//horizontal
                REQUEST = fnCollegeLinks


            case ('CLASSES-BLOCKS.CSV')

                call write_classes_blocks_csv (device, thisTerm, NumSections(thisTerm), Section(thisTerm,0:), &
                    NumBlocks(thisTerm), Block(thisTerm,0:) )


            case ('ENLISTMENT.CSV')
                call write_enlistment_csv (device, thisTerm)


            case ('STUDENT-INFO.CSV')

                fileName = trim(dirDOWNLOADS)//XMLfile
                open(unit=unitETC, file=trim(fileName))
                call write_student_info_csv (unitETC, thisTerm)
                close(unitETC)

                call html_write_header(device, trim(XMLfile)// ' created', SPACE)
#if defined GLNX
                line = 'gzip '//fileName
                call system(trim(line), errNo)
                call log_comment(itoa(errNo)//' returned by: '//line)
                if (errNo==0) XMLfile = trim(XMLfile)//'.gz'
#else
                line = '"C:\Program Files\7-Zip\7z.exe" a -t7z '//trim(fileName)//'.7z '//fileName
                call system(trim(line), errNo)
                call log_comment(itoa(errNo)//' returned by: '//line)
                if (errNo==0) XMLfile = trim(XMLfile)//'.7z'
#endif
                write(device,AFORMAT)'Right-click <a href="'//trim(urlDOWNLOADS)//trim(XMLfile)// &
                    '">here</a>, then "Save Link As ..."'//horizontal
                REQUEST = fnCollegeLinks

            case default

                targetCollege = CollegeIdxUser
                targetDepartment = DeptIdxUser
                call html_write_header(device, SPACE, horizontal//'File not recognized "'//trim(XMLfile))
                REQUEST = 0


        end select

    end subroutine download_file


    subroutine regenerate_obfuscator()
        integer :: iTmp, jTmp

        call log_comment('regenerate_obfuscator()')

        ! initialize QUERY_STRING encryption key to invalidate old links
        do iTmp=1,MAX_LEN_QUERY_STRING
            call random_number(harvest)
            jTmp = 1 + int(255*harvest)
            queryEncryptionKey(iTmp:iTmp) = achar(jTmp)
        end do
        !call html_comment('KEY='//queryEncryptionKey)

    end subroutine regenerate_obfuscator


    subroutine toggle_permission(device, thisTerm)

        integer, intent(in) :: device, thisTerm

        integer :: cdx, permission
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege, setting

        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, cdx)
        targetCollege = index_to_college(tCollege)
        call cgi_get_named_integer(QUERY_STRING, 'A2', permission, cdx)

        call html_comment('toggle_permission('//tCollege//AllowedAction(permission)//')')

        if (isRoleOfficial) then
            call html_write_header(device, trim(tCollege)//' - '//College(targetCollege)%Name, &
                sorryMessageOfficial)
        else
            College(targetCollege)%isAllowed(permission,thisTerm) = &
                .not. College(targetCollege)%isAllowed(permission,thisTerm)
            if (targetCollege==NumColleges) then ! copy ADMIN setting to all colleges
                College(1:targetCollege)%isAllowed(permission,thisTerm) = College(targetCollege)%isAllowed(permission,thisTerm)
            end if
            if (College(targetCollege)%isAllowed(permission,thisTerm)) then
                setting = 'ENABLED'
            else
                setting = 'DISABLED'
            end if
            call html_write_header(device, trim(tCollege)//' - '//College(targetCollege)%Name, &
                setting//tCollege//AllowedAction(permission))

            ! only one of ToSelfEnlistPE or ToSelfEnlistALL must be active
            if (permission==ToSelfEnlistALL) then
                if (College(targetCollege)%isAllowed(ToSelfEnlistALL,thisTerm)) then
                    College(targetCollege)%isAllowed(ToSelfEnlistPE,thisTerm) = .false.
                    if (targetCollege==NumColleges) then ! copy ADMIN setting to all colleges
                        College(1:targetCollege)%isAllowed(ToSelfEnlistPE,thisTerm) = .false.
                    end if
                end if
            elseif (permission==ToSelfEnlistPE) then
                if (College(targetCollege)%isAllowed(ToSelfEnlistPE,thisTerm)) then
                    College(targetCollege)%isAllowed(ToSelfEnlistALL,thisTerm) = .false.
                    if (targetCollege==NumColleges) then ! copy ADMIN setting to all colleges
                        College(1:targetCollege)%isAllowed(ToSelfEnlistALL,thisTerm) = .false.
                    end if
                end if
            end if
        end if
        if (targetCollege==NumColleges) then
            do cdx=1,NumColleges-1
                call college_details_write(unitXML, dirCOLLEGES, cdx)
            end do
        else
            call college_details_write(unitXML, dirCOLLEGES, targetCollege)
        end if
        call html_college_info(device, targetCollege)

    end subroutine toggle_permission


    subroutine write_student_info_csv(device, thisTerm)

        integer, intent (in) :: device, thisTerm

        integer :: ierr, std, idx
        character (len=MAX_LEN_XML_LINE) :: line

        write(device,AFORMAT) &
            '#', &
            '#  Student info for '//UniversityCode, &
            '#', &
            '# Generated by '//PROGNAME//VERSION//' on '//currentDate(1:4)// &
                        FSLASH//currentDate(5:6)//FSLASH//currentDate(7:8), &
            '#', '#', &
            '"StdNo","Name","Email","Gender","Curriculum","Year","Adviser","Residency",'// &
            '"BirthDate","BirthPlace","BirthPlacePSGC","HomeStreetAddress","HomePSGC","MotherTongue",'// &
            '"Father","Mother","Guardian",'// &
            '"EntryDate","GraduationDate","LastAttended","AdmissionData","Scholarship","TranscriptRemark",'// &
            '"TranscriptAdditionalRemark"'

        do idx=1,NumStudents+NumAdditionalStudents

            std = StdRank(idx)
            call xml_read_student_info(std, ierr)

            line = COMMA//'"'//trim(StudentInfo%TranscriptAdditionalRemark)//'"'
            line = COMMA//'"'//trim(StudentInfo%TranscriptRemark)//'"'
            line = COMMA//'"'//trim(StudentInfo%Scholarship)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%AdmissionData)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%LastAttended)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%GraduationDate)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%EntryDate)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%Guardian)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%Mother)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%Father)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%MotherTongue)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%HomePSGC)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%HomeStreetAddress)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%BirthPlacePSGC)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%BirthPlace)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%BirthDate)//'"'//line
            line = COMMA//'"'//trim(itoa(Student(std)%ResidenceStatus))//'"'//line
            line = COMMA//'"'//trim(Student(std)%Adviser)//'"'//line
            line = COMMA//'"'//trim(txtYear(Student(std)%Enlistment(thisTerm)%levelYear))//'"'//line
            line = COMMA//'"'//trim(Curriculum(Student(std)%CurriculumIdx)%Code)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%Gender)//'"'//line
            line = COMMA//'"'//trim(StudentInfo%Email)//'"'//line
            line = COMMA//'"'//trim(Student(std)%Name)//'"'//line
            line = '"'//trim(Student(std)%StdNo)//'"'//line

            write(device,AFORMAT) trim(line)
        end do
        close(device)

    end subroutine write_student_info_csv


    subroutine check_permission()
        logical :: permitted

        permitted = .true.
        select case (REQUEST)

#if defined no_password_check
#else
            case (fnStop)
                permitted = .false.
#endif
            case (  fnTogglePermission, &
                    fnReload, &
                    fnFileDownload, &
                    fnResetPasswords, &
                    fnLoginByNonEditors, &
                    fnEditEMERGENCY, &
                    fnEditMOTD, &
                    fnTimers, &
                    fnEditSignatories, &
                    fnEditFees, &
                    fnEditScholarships, &
                    fnEditEquivalencies, &
                    fnStudentsNotEnrolled, &
                    fnUploadCSV )
                permitted = .false.

            case (  fnGenerateTimetables, &
                    fnClearTimetables, &
                    fnTimetabling, &
                    fnResetDemandForSubjects, &
                    fnUpdateDemandForSubjects )
                permitted = .false.

        end select

        if (.not. permitted) then
            UserRequestCheckMessage = 'Operation "'//trim(fnDescription(REQUEST) )//'" failed. '//sorryMessageOfficial
            REQUEST = fnLogin
        end if

    end subroutine check_permission


    subroutine average_grade_summary_list(device)
        integer, intent (in) :: device

        integer :: ierr, idx, ldx, tdx, std, nClasses(3), nAdvised(3), iTerm, n_count
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo, tAction
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum

        real :: SumEnrolled, SumEarned, SumDown, SumUp, tUnits
        real :: GSumEnrolled, GSumEarned, GSumDown, GSumUp

        character(len=10) :: str_cumulative = 'CUMULATIVE'
        character(len= 9) :: str_last_term  = 'LAST TERM'

        integer :: prevtaken, grd, crse
        real :: up, down

        ! which Curriculum ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
        call cgi_get_named_string(QUERY_STRING, 'A2', tAction, ierr)
        call html_comment('average_grade_summary_list('//trim(tCurriculum)//' by '//tAction//')')

        call html_write_header(device, 'Weighted average grade of '//trim(tCurriculum)//' students')

        write(device,AFORMAT) b_italic//str_cumulative//' and '//str_last_term// &
            ' numbers are : Weighted Average Grade, Units Enrolled, Units Earned.'//e_italic

        write(device,AFORMAT) linebreak, linebreak, &
            '<table border="0" width="100%">', &
            b_tr//b_thal//'#'//e_th//b_thal//'STDNO'//e_th, &
            b_thal//'NAME OF STUDENT'//e_th//b_thal//'CURRIC'//e_th

        if (trim(tAction)==str_cumulative) then
            write(device,AFORMAT) b_thal//trim(str_cumulative)//e_th
        else
            write(device,AFORMAT) &
                trim(make_href(fnSummaryWAG, str_cumulative, A1=tCurriculum, A2=str_cumulative, pre=b_thal, post=e_th))
        end if

        if (trim(tAction)==str_last_term) then
            write(device,AFORMAT) b_thal//trim(str_last_term)//e_th
        else
            write(device,AFORMAT) &
                trim(make_href(fnSummaryWAG, str_last_term, A1=tCurriculum, A2=str_last_term, pre=b_thal, post=e_th))
        end if

        write(device,AFORMAT) b_thal//'LINKS'//e_th//e_tr

        n_count = 0
        tArray = 0
        do idx=1,NumStudents+NumAdditionalStudents
            std = StdRank(idx)
            ldx = Student(std)%CurriculumIdx
            if (CurrProgCode(ldx) /= tCurriculum) cycle

            ! remember index to student
            tArray(n_count+1) = std

            ! read checklist
            call read_student_records (std)

            SumUp = 0.0
            SumDown = 0.0
            GSumDown = 0.0
            GSumUp = 0.0
            SumEnrolled = 0.0
            SumEarned = 0.0
            GSumEnrolled = 0.0
            GSumEarned = 0.0
            prevtaken = -10

            do tdx=1,lenTCG

                ! FINALGRADES only (with completions if applicable)
                if (TCG(tdx)%Code/=3 .or. TCG(tdx)%Used .or. TCG(tdx)%ErrorCode>1) cycle

                if (prevtaken /= TCG(tdx)%Taken) then

                    ! write summary for current term
                    if (SumUp*SumDown>0.0) then

                        tArray(n_count+2) = int(1.0E5*SumUp/SumDown)
                        tArray(n_count+3) = int(SumEnrolled)
                        tArray(n_count+4) = int(SumEarned)

                        GSumEnrolled = GSumEnrolled + SumEnrolled
                        GSumEarned = GSumEarned + SumEarned

                        GSumUp = GSumUp + SumUp
                        GSumDown = GSumDown + SumDown

                    end if

                    ! re-initialize accumulators
                    prevtaken = TCG(tdx)%Taken
                    SumUp = 0.0
                    SumDown = 0.0
                    SumEnrolled = 0.0
                    SumEarned = 0.0

                end if

                crse = TCG(tdx)%Subject
                grd = TCG(tdx)%Grade
                tUnits = Subject(crse)%Units

                up = 0.0
                down = 0.0

                if (TCG(tdx)%ReExam/=0) then
                    grd = TCG(tdx)%ReExam
                end if

                if (tUnits == 0.0 .or. Subject(crse)%Name(1:5)=='NSTP ') then
                    ! exclude

                else if (grd==gdxDRP .or. grd==gdxPASS) then
                    ! exclude

                else if (grd==gdxINC .or. grd==gdxNFE .or. grd==gdxREGD) then
                    SumEnrolled = SumEnrolled + tUnits
                    up = 0.0
                    down = tUnits

                else
                    SumEnrolled = SumEnrolled + tUnits
                    if (isGrade_numeric_pass(grd)) SumEarned = SumEarned + tUnits
                    up = tUnits*fGrade(grd)
                    if (up/=0.0) then
                        down = tUnits
                    else
                        down = 0.0
                    end if
                end if

                SumUp = SumUp + Up
                SumDown = SumDown + Down
            end do

            ! write summary for last term
            if (SumUp*SumDown>0.0) then

                tArray(n_count+2) = int(1.0E5*SumUp/SumDown)
                tArray(n_count+3) = int(SumEnrolled)
                tArray(n_count+4) = int(SumEarned)

                GSumEnrolled = GSumEnrolled + SumEnrolled
                GSumEarned = GSumEarned + SumEarned

                GSumUp = GSumUp + SumUp
                GSumDown = GSumDown + SumDown

            end if

            ! cumulative average
            if (GSumUp*GSumDown>0.0) then

                tArray(n_count+5) = int(1.0E5*GSumUp/GSumDown)
                tArray(n_count+6) = int(GSumEnrolled)
                tArray(n_count+7) = int(GSumEarned)
            end if

            n_count = n_count + 7

        end do

        if (trim(tAction)==str_cumulative) then ! sort by +5
            tdx = 5
        else ! if (trim(tAction)==str_last_term) then ! sort by +2
            tdx = 2
        end if
        do idx=0,n_count-8,7
            do ldx=idx+7,n_count,7
                if (tArray(idx+tdx)<tArray(ldx+tdx)) then ! swap
                    tArray(n_count+1:n_count+7) = tArray(idx+1:idx+7)
                    tArray(idx+1:idx+7) = tArray(ldx+1:ldx+7)
                    tArray(ldx+1:ldx+7) = tArray(n_count+1:n_count+7)
                end if
            end do
        end do

        do idx=0,n_count-1,7
            tdx = idx/7 + 1
            std = tArray(idx+1)
            tStdNo = Student(std)%StdNo
            ldx = Student(std)%CurriculumIdx
            call count_preenlistment(std, 0, nClasses, nAdvised)

            write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(tdx,2))//'">'// &
                b_td//trim(itoa(tdx))//DOT//e_td//b_td//tStdNo//e_td, &
                b_td//trim(Student(std)%Name)//e_td, &
                b_td//trim(Curriculum(ldx)%Code)//e_td

            ! cumulative average
            if (tArray(idx+5)>0) then
                write(device,'(a,f8.2,a,i4,a,i4, a)') &
                    b_td, tArray(idx+5)/1.0E5, COMMA, tArray(idx+6), COMMA, tArray(idx+7), e_td
            else
                write(device,AFORMAT) b_td_nbsp_e_td
            end if

            ! write summary for last term
            if (tArray(idx+2)>0) then
                write(device,'(a,f8.2,a,i4,a,i4, a)') &
                    b_td, tArray(idx+2)/1.0E5, COMMA, tArray(idx+3), COMMA, tArray(idx+4), e_td
            else
                write(device,AFORMAT) b_td_nbsp_e_td
            end if

            ! links
            write(device,AFORMAT) b_td//b_small, &
                trim(make_href(fnStudentGrades, 'Grades', A1=tStdNo, A9=currentTerm, pre=nbsp)), &
                trim(make_href(fnEditCheckList, 'Checklist', A1=tStdNo, A9=currentTerm, pre=nbsp))
            do iTerm=firstSemester,summerTerm
                if (nClasses(iTerm)+nAdvised(iTerm)>0) write(device,AFORMAT) &
                    trim(make_href(fnStudentClasses, txtSemester(iTerm+6), A1=tStdNo, A9=iTerm, pre=nbsp))
            end do
            write(device,AFORMAT) e_small//e_td, e_tr

        end do

        write(device,AFORMAT) e_table, horizontal

    end subroutine average_grade_summary_list


    subroutine search_category (device, thisTerm)
        integer, intent (in) :: device, thisTerm

        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_SUBJECT_CODE) :: tAction
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        integer :: coll, dept, tLen, ierr, nFound, tdx, idx, iUnit, iStd
        character(len=80) :: location, searchString
        logical :: checkAll
        character (len=MAX_LEN_FILE_PATH) :: fileName

        ! the search string
        call cgi_get_named_string(QUERY_STRING, 'A1', searchString, ierr)
        call cgi_get_named_string(QUERY_STRING, 'A2', tAction, ierr)
        call cgi_get_named_string(QUERY_STRING, 'A3', tCollege, ierr)
        coll = index_to_college(tCollege)
        tDepartment = tCollege
        dept = index_to_dept(tDepartment)
        tLen = len_trim(searchString)
        if (tLen==0 .or. len_trim(tAction)==0) then
            call html_college_links(device, coll, mesg='Text to search for not specified.')
            return
        end if
        checkAll = isRoleSysAd .and. coll==NumColleges

        call html_comment('search_category("'//trim(tAction)//'" for "'//searchString(:tLen)//'")' )
        tArray = 0
        nFound = 0

        select case (trim(tAction))

            case ('Subjects')
                do idx=1,NumSubjects+NumAdditionalSubjects
                    if (.not. checkAll ) then  ! no restrictions for ADMIN
                        if (.not. (Subject(idx)%DeptIdx == dept .or. isSubject_used_in_college(coll, idx)) ) cycle
                    end if
                    call subject_search_info(idx, searchString(:tLen), location)
                    if (len_trim(location)==0) cycle
                    call html_comment(trim(Subject(idx)%Name)//' - '//searchString(:tLen)//'@'//trim(location))
                    nFound = nFound+1
                    tArray(nFound) = idx
                end do
                if (checkAll) then
                    tDepartment = 'S'
                else
                    tDepartment = trim(tDepartment)//' s'
                end if
                call html_subject_list (device, nFound, tArray(0:nFound), &
                    trim(tDepartment)//'ubjects matched by search string "'//searchString(:tLen)//'"')

            case ('Curricula')
                do idx=1,NumCurricula-1
                    if (.not. checkAll ) then  ! no restrictions for ADMIN
                        if (Curriculum(idx)%CollegeIdx /= coll) cycle
                    end if
                    call curriculum_search_info(idx, searchString(:tLen), location)
                    if (len_trim(location)==0) cycle
                    call html_comment(trim(Curriculum(idx)%Code)//' - '//searchString(:tLen)//'@'//trim(location))
                    nFound = nFound+1
                    tArray(nFound) = idx
                end do
                if (checkAll) then
                    tDepartment = 'C'
                else
                    tDepartment = trim(tDepartment)//' c'
                end if
                call html_curriculum_list (device, fnSearchCategory, nFound, tArray(0:nFound), &
                    trim(tDepartment)//'urricular programs matched by search string "'//searchString(:tLen)//'"', &
                    SPACE, searchString(:tLen))

            case ('Rooms')
                ! collect rooms
                do idx=1,NumRooms+NumAdditionalRooms
                    if (.not. checkAll ) then  ! no restrictions for ADMIN
                        if (Room(idx)%DeptIdx/=dept) cycle
                    end if
                    call room_search_info(idx, searchString(:tLen), location)
                    if (len_trim(location)==0) cycle
                    nFound = nFound+1
                    tArray(nFound) = idx
                end do
                if (checkAll) then
                    tDepartment = 'R'
                else
                    tDepartment = trim(tDepartment)//' r'
                end if
                call html_room_list (device, fnSearchCategory, nFound, tArray(0:nFound), &
                    trim(tDepartment)//'ooms matched by search string "'//searchString(:tLen)//'"', searchString(:tLen))

            case ('Teachers')
                call upper_case(searchString)
                ! collect teachers
                do tdx=1,NumTeachers+NumAdditionalTeachers
                    idx = TeacherRank(tdx)
                    if (.not. checkAll ) then  ! no restrictions for ADMIN
                        if (Teacher(idx)%DeptIdx/=dept) cycle
                        if (Teacher(idx)%Role==SYSAD .and. .not. isRegistrar) cycle
                    end if
                    call teacher_search_info(idx, searchString(:tLen), location)
                    if (len_trim(location)==0) cycle
                    nFound = nFound+1
                    tArray(nFound) = idx
                end do
                if (checkAll) then
                    tDepartment = 'T'
                else
                    tDepartment = trim(tDepartment)//' t'
                end if
                call html_teacher_list (device, fnSearchCategory, nFound, tArray(0:nFound), &
                    trim(tDepartment)//'eachers matched by search string "'//searchString(:tLen)//'"', searchString(:tLen))

            case ('Students')
                call upper_case(searchString)
                ! collect students
                do tdx=1,NumStudents+NumAdditionalStudents
                    iStd = StdRank(tdx)
                    if (.not. checkAll ) then  ! no restrictions for ADMIN
                        !if (Curriculum(Student(iStd)%CurriculumIdx)%CollegeIdx/=coll) cycle
                        if ( .not. (Curriculum(Student(iStd)%CurriculumIdx)%CollegeIdx==coll .or. &
                              Curriculum(Student(iStd)%CurriculumIdx)%CollegeIdx==NumColleges) ) cycle
                    end if
                    call student_search_info(iStd, searchString(:tLen), thisTerm, nFound+1, location)
                    if (len_trim(location)==0) cycle
                    nFound = nFound+1
                    tArray(nFound) = iStd
                end do
                if (checkAll) tDepartment = SPACE

                call html_write_header(device, 'Current '//trim(tDepartment)//' students matched by search string "'// &
                    searchString(:tLen)//'"')
                call html_student_list (device, nFound, tArray(1:nFound), .true., searchString(:tLen))
                write(device,AFORMAT) horizontal, &
                    '<h3>Additional matches from the masterlist, if any</h3>', &
                    b_italic//'(Click "Add" to include as current student)'//e_italic, '<ol>'

                fileName = 'matches-from-masterlist'
                call system('grep '//trim(searchString)//SPACE//trim(dirDATA)//'info'//DIRSEP// &
                    'masterlist-STUDENTS > '//trim(fileName))

                iUnit = unitHTML+1
                open(unit=iUnit, file=fileName)
                do
                    read (iUnit, AFORMAT, iostat=ierr) searchString
                    if (ierr < 0) exit
                    idx = index(searchString,COMMA)
                    tStdNo = searchString(1:idx-1)
                    iStd = index_to_student(tStdNo)
                    if (iStd/=0) cycle ! already in current roster
                    write(device, AFORMAT) &
                        trim(make_href(fnStudentAdd, 'Add', A1=tStdNo, A2=searchString(idx+1:), &
                            pre=b_item//tStdNo//' - '//trim(searchString(idx+1:))//nbsp// &
                            b_small, post=e_small//e_item))
                end do
                close(iUnit)
                write(device,AFORMAT) '</ol>', horizontal

            case ('Blocks')
                call upper_case(searchString)
                !collect blocks
                do idx=1,NumBlocks(thisTerm)
                    if (.not. checkAll ) then  ! no restrictions for ADMIN
                        if (Curriculum(Block(thisTerm,idx)%CurriculumIdx)%CollegeIdx /= coll) cycle
                    end if
                    call block_search_info(idx, searchString(:tLen), thisTerm, nFound+1, location)
                    if (len_trim(location)==0) cycle
                    call html_comment(trim(Block(thisTerm,idx)%BlockID)//' - '//searchString(:tLen)//'@'//trim(location))
                    nFound = nFound+1
                    tArray(nFound) = idx
                end do
                if (checkAll) then
                    tDepartment = 'B'
                else
                    tDepartment = trim(tDepartment)//' b'
                end if
                call html_block_list (device, fnSearchCategory, thisTerm, nFound, tArray(0:nFound), &
                    trim(tDepartment)//'locks matched by search string "'//searchString(:tLen)//'"', &
                    searchString(:tLen))

        end select

    end subroutine search_category


    subroutine file_uploadIDpicture(device)
        integer, intent (in) :: device

        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        integer :: std, k, ierr
        character(len=MAX_LEN_FILE_PATH) :: filename, filetype
        character(len=3) :: imageExt

        call cgi_get_named_string(QUERY_STRING, 'file', filename, ierr)
        if (len_trim(filename)/=0) then
            call cgi_get_named_string(QUERY_STRING, 'content', filetype, ierr)
            if (index(filetype, 'image/jpeg')>0) then
                imageExt = 'jpg'
            else if (index(filetype, 'image/png')>0) then
                imageExt = 'png'
            else if (index(filetype, 'image/x-ms-bmp')>0) then
                imageExt = 'bmp'
            else
                imageExt = SPACE
            end if
            if (len_trim(imageExt)>0) then
                filename = trim(dirUPLOADS)//filename
                call cgi_get_named_string(QUERY_STRING, 'A1', tStdNo, ierr)
                QUERY_STRING = 'A1='//trim(tStdNo)//'&'

                std = index_to_student(tStdNo)
                call get_picture_file(std, filetype)
                if (len_trim(filetype)>0) then ! ID exists; rename
                    k = len_trim(filetype)
!                    filetype = trim(dirPICTURES)//trim(basefile_student(std))//filetype(k-3:)
!#if defined GLNX
                    call rename (filetype, trim(filetype)//DASH//currentDate//currentTime(:6), ierr)
!#else
!                    call system(mvCmd//trim(filetype)//SPACE//trim(filetype)//DASH//currentDate//currentTime(:6), ierr)
!#endif
                end if
                filetype = trim(dirPICTURES)//trim(basefile_student(std))//DOT//imageExt

!#if defined GLNX
                call rename (filename, filetype, ierr)
!#else
!                call system(mvCmd//trim(filename)//SPACE//trim(filetype), ierr)
!#endif
                if (ierr/=0) call html_comment('Status='//trim(itoa(ierr))//' in moving '//trim(filename)// &
                    ' to '//trim(filetype) )
            else
                QUERY_STRING = trim(QUERY_STRING)//'&errmsg="'//trim(filename)//'" is not acceptable as ID picture.'
            end if
        else
            QUERY_STRING = trim(QUERY_STRING)//'&errmsg=No ID picture selected?'
        end if

        REQUEST = fnStudentEdit
        call student_edit_info(device)

    end subroutine file_uploadIDpicture


    subroutine reset_all_passwords(device)
        integer, intent(in) :: device

        integer :: i, j
        character (len=MAX_LEN_ROLE) :: tAction

        call cgi_get_named_string(QUERY_STRING, 'action', tAction, i)

        if (i==-1 .or. len_trim(tAction)==0) then ! no action specified; print confirmation form
            call html_write_header(device, 'Confirm password reset')
            call make_form_start(device, fnResetPasswords)
            write(device,AFORMAT) linebreak, &
                'This operation will reset passwords of users with the role you select below. Choose carefully.', &
                linebreak, b_para, '<input type="submit" name="action" value="CANCEL">', &
                nbsp//nbsp//nbsp//nbsp//'<input type="submit" name="action" value="Student">'
            do j=0,MAX_ALL_ROLES
                write(device,AFORMAT) nbsp//nbsp//nbsp//nbsp//nbsp// &
                    '<input type="submit" name="action" value="'//trim(txtRole(j))//'">'
            end do
            write(device,AFORMAT) e_para, e_form, horizontal

        else
            j = 0
            if (trim(tAction)=='CANCEL') then ! cancelled
                call html_write_header(device, College(CollegeIdxUser)%Code//'- '//College(CollegeIdxUser)%Name, &
                    'Password reset operation cancelled.')
            elseif (isRoleOfficial) then
                call html_write_header(device, College(CollegeIdxUser)%Code//'- '//College(CollegeIdxUser)%Name, &
                    '"Reset passwords" failed. '//sorryMessageOfficial)
            elseif (trim(tAction)=='Student') then ! students
                do i=1,NumStudents+NumAdditionalStudents
                    call set_password(Student(i)%Password)
                    call student_details_write(unitXML, dirSTUDENTS, i)
                    j = j + 1
                end do
                isDirtySTUDENTS = .true.
                call html_write_header(device, College(CollegeIdxUser)%Code//'- '//College(CollegeIdxUser)%Name, &
                    trim(itoa(j))// ' passwords were reset (users with '''//trim(tAction)//''' role)')
            else ! tAction is a teacher's role
                call html_write_header(device, College(CollegeIdxUser)%Code//'- '//College(CollegeIdxUser)%Name)
                write(device,AFORMAT) 'Passwords for the following users were reset: '
                do i=1,NumTeachers+NumAdditionalTeachers
                    if ( Teacher(i)%Role/=tAction ) cycle
                    call set_password(Teacher(i)%Password)
                    call teacher_details_write(unitXML, dirTEACHERS, i)
                    j = j + 1
                    write(device,AFORMAT) linebreak//trim(itoa(j))//') '//trim(Teacher(i)%TeacherId)//' : '// &
                        trim(Teacher(i)%Name)//' : '//trim(Teacher(i)%Role)//'@'//Department(Teacher(i)%DeptIdx)%Code
                end do
                write(device,AFORMAT) horizontal
            end if
            call html_college_info(device, CollegeIdxUser)
        end if

    end subroutine reset_all_passwords


    subroutine student_purge_removed(device)
        integer, intent(in) :: device
        integer :: i, j, k, l

        j = 0
        k = NumStudents
        l = NumAdditionalStudents
        write(device,AFORMAT) 'Students hidden:', '<pre>'
        do i=1,NumStudents+NumAdditionalStudents
            if (Student(i)%ResidenceStatus/=REMOVE_FROM_ROSTER) then
                j = j + 1
                Student(j) = Student(i)
            else
                write(device,AFORMAT) trim(text_student_curriculum(i))
                if (i .le. NumStudents) then
                    k = k-1
                else
                    l = l-1
                end if
            end if
        end do
        write(device,AFORMAT) '</pre>'
        NumStudents = k
        NumAdditionalStudents = l
        call sort_alphabetical_students()
        call student_details_write(unitXML, trim(dirSTUDENTS)//'index', 1, NumStudents+NumAdditionalStudents)

    end subroutine student_purge_removed


    subroutine  distribution_by_PSGC(device)
        integer, intent(in) :: device

        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=MAX_LEN_PSGC) :: tCode, tHome, tAction
        ! RR0000000 - Region, RRPP00000 - Province/District, RRPPMM000 - Municipality, RRPPMMBBB - Barangay
        integer :: ierr, pdx, std, ldx, cdx, collegeIdx, row, nChars, offset

        ! A1=College(collegeIdx)%Code
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        collegeIdx = index_to_college(tCollege)

        ! A2=PSGC
        call cgi_get_named_string(QUERY_STRING, 'A2', tCode, ierr)
        pdx = index_to_PSGC(tCode)

        ! A2=action
        call cgi_get_named_string(QUERY_STRING, 'A3', tAction, ierr)

        call html_comment('distribution_by_PSGC(college='//trim(tCollege)//' PSGC='//trim(tCode)//')')
        PSGCcount = 0

        if (pdx==0) then ! no PSGC code specified; assume all regions

            nChars = 2
            do std=1,NumStudents+NumAdditionalStudents

                call xml_read_student_info(std, ierr)
                Student(std)%HomePSGC = StudentInfo%HomePSGC
                if (len_trim(Student(std)%HomePSGC)==0 .or. Student(std)%HomePSGC(1:3)=='DEL') then 
                    Student(std)%HomePSGC = PSGC(NumPSGC)%Code
                end if
                tHome = Student(std)%HomePSGC
                cdx = Curriculum(Student(std)%CurriculumIdx)%CollegeIdx
                ldx = index_to_PSGCsubset(tHome, nChars, NumRegion, Region)
                if (ldx==0) then 
                    !call html_comment(Student(std)%StdNo//' - '//tHome//' - '//Curriculum(Student(std)%CurriculumIdx)%Code )
                    ldx = NumPSGC
                end if
                PSGCcount(ldx,cdx) = PSGCcount(ldx,cdx) + 1
            end do
            row = 0
            do ldx=1,NumRegion
                row = row+1
                PSGCcount(row,0) = Region(ldx) ! index to PSGC region code
                PSGCcount(row,1:NumColleges) = PSGCcount(ldx,1:NumColleges)
                PSGCcount(row,NumColleges+1) = sum(PSGCcount(ldx,1:NumColleges)) ! region sum
                !call html_comment('---> '//itoa(row)//PSGC(Region(ldx))%Name)
            end do
            do cdx=1,NumColleges+1
                PSGCcount(0,cdx) = sum(PSGCcount(1:row,cdx)) ! college sum
            end do
            call display_PSGC_count(device, row, NumColleges, trim(tCollege), 'Region', &
                'Distribution of '//trim(UniversityCode)//' students by home address', SPACE, 'MENU')

        elseif (trim(tAction)=='LIST') then

            tArray = 0
            row = 0

            if (isPSGC_kind(tCode, kindRegion)) then ! region code
                nChars = kindRegion-1

            elseif (isPSGC_kind(tCode, kindProvince)) then ! province code
                nChars = kindProvince-1

            elseif (isPSGC_kind(tCode, kindMunicipality)) then ! municipality code
                nChars = kindMunicipality-1

            else ! barangay code
                nChars = MAX_LEN_PSGC

            end if

            do ldx=1,NumStudents+NumAdditionalStudents
                std = StdRank(ldx)
                if (Student(std)%HomePSGC(:nChars)/=tCode(:nChars)) cycle
                row = row + 1
                tArray(row) = std
            end do

            call html_write_header(device, 'Students from '//text_PSGC(tCode))
            call html_student_list (device, row, tArray, .true., SPACE)

            if (isPSGC_kind(tCode, kindRegion)) then ! region code
                write(device,AFORMAT) b_para, trim(make_href(fnDistributionPSGC, 'Other region', &
                    A1=tCollege, A2='ALL', pre=b_small//b_italic, post=e_italic//e_small)), e_para

            elseif (isPSGC_kind(tCode, kindProvince)) then ! province code
                write(device,AFORMAT) b_para, trim(make_href(fnDistributionPSGC, 'Other province', &
                    A1=tCollege, A2=makePSGC_kind(tCode, kindRegion), pre=b_small//b_italic, post=e_italic//e_small)), e_para

            elseif (isPSGC_kind(tCode, kindMunicipality)) then ! municipality code
                write(device,AFORMAT) b_para, trim(make_href(fnDistributionPSGC, 'Other municipality', &
                    A1=tCollege, A2=makePSGC_kind(tCode, kindProvince), pre=b_small//b_italic, post=e_italic//e_small)), e_para

            else ! barangay code
                write(device,AFORMAT) b_para, trim(make_href(fnDistributionPSGC, 'Other barangay', &
                    A1=tCollege, A2=makePSGC_kind(tCode, kindMunicipality), pre=b_small//b_italic, post=e_italic//e_small)), e_para

            end if

        else ! assume action is MENU

            if (isPSGC_kind(tCode, kindRegion)) then ! region code
                nChars = 4
                do std=1,NumStudents+NumAdditionalStudents
                    tHome = Student(std)%HomePSGC
                    if (tHome(1:2)/=tCode(1:2)) cycle ! not from the region
                    cdx = Curriculum(Student(std)%CurriculumIdx)%CollegeIdx
                    ldx = index_to_PSGCsubset(tHome, nChars, NumProvince, Province) ! index_to_province(tHome)
                    PSGCcount(ldx,cdx) = PSGCcount(ldx,cdx) + 1
                    !call html_comment(Student(std)%StdNo//' - '//tHome//' - '//PSGC(Province(ldx))%Name)
                end do
                row = 0
                do ldx=1,NumProvince
                    if (PSGC(Province(ldx))%Code(1:2)/=tCode(1:2)) cycle
                    row = row+1
                    PSGCcount(row,0) = Province(ldx) ! index to PSGC province code
                    PSGCcount(row,1:NumColleges) = PSGCcount(ldx,1:NumColleges)
                    PSGCcount(row,NumColleges+1) = sum(PSGCcount(ldx,1:NumColleges)) ! province sum
                    !call html_comment('---> '//itoa(row)//PSGC(Province(ldx))%Name)
                end do
                do cdx=1,NumColleges+1
                    PSGCcount(0,cdx) = sum(PSGCcount(1:row,cdx)) ! college sum
                end do
                call display_PSGC_count(device, row, NumColleges, trim(tCollege), 'Province', &
                    'Students from '//text_PSGC(tCode), 'LIST', 'MENU', &
                    trim(make_href(fnDistributionPSGC, 'Other region', &
                    A1=tCollege, A2='ALL', pre=b_small//b_italic, post=e_italic//e_small)) )

            elseif (isPSGC_kind(tCode, kindProvince)) then ! province code
                nChars = 6
                do std=1,NumStudents+NumAdditionalStudents
                    tHome = Student(std)%HomePSGC
                    if (tHome(1:4)/=tCode(1:4)) cycle ! not from the province
                    cdx = Curriculum(Student(std)%CurriculumIdx)%CollegeIdx
                    ldx = index_to_PSGCsubset(tHome, nChars, NumMunicipality, Municipality) ! index_to_municipality(tHome)
                    PSGCcount(ldx,cdx) = PSGCcount(ldx,cdx) + 1
                    !call html_comment(Student(std)%StdNo//' - '//tHome//' - '//PSGC(Municipality(ldx))%Name)
                end do
                row = 0
                do ldx=1,NumMunicipality
                    if (PSGC(Municipality(ldx))%Code(1:4)/=tCode(1:4)) cycle ! not same province
                    row = row+1
                    PSGCcount(row,0) = Municipality(ldx) ! index to PSGC Municipality code
                    PSGCcount(row,1:NumColleges) = PSGCcount(ldx,1:NumColleges)
                    PSGCcount(row,NumColleges+1) = sum(PSGCcount(ldx,1:NumColleges)) ! province sum
                    !call html_comment('---> '//itoa(row)//PSGC(Municipality(ldx))%Name)
                end do
                do cdx=1,NumColleges+1
                    PSGCcount(0,cdx) = sum(PSGCcount(1:row,cdx)) ! college sum
                end do
                call display_PSGC_count(device, row, NumColleges, trim(tCollege), 'Municipality', &
                    'Students from '//text_PSGC(tCode), 'LIST', 'MENU', &
                    trim(make_href(fnDistributionPSGC, 'Other province', &
                    A1=tCollege, A2=makePSGC_kind(tCode, kindRegion), pre=b_small//b_italic, post=e_italic//e_small))  )

            elseif (isPSGC_kind(tCode, kindMunicipality)) then ! municipality code
                nChars = 6
                offset = index_to_PSGC(tCode)
                row = 0
                do while (PSGC(offset+row+1)%Code(:nChars)==PSGC(offset)%Code(:nChars))
                    row = row + 1
                end do

                do std=1,NumStudents+NumAdditionalStudents
                    tHome = Student(std)%HomePSGC
                    if (tHome(1:nChars)/=tCode(1:nChars)) cycle ! not from the municipality
                    cdx = Curriculum(Student(std)%CurriculumIdx)%CollegeIdx
                    ldx = index_to_PSGC(tHome)
                    PSGCcount(ldx-offset,cdx) = PSGCcount(ldx-offset,cdx) + 1
                    !call html_comment(Student(std)%StdNo//' - '//tHome//' - '//PSGC(ldx)%Name)
                end do

                do ldx=1,row
                    PSGCcount(ldx,0) = offset+ldx
                    PSGCcount(ldx,NumColleges+1) = sum(PSGCcount(ldx,1:NumColleges)) ! barangay sum
                    !call html_comment('---> '//itoa(ldx)//PSGC(ldx+offset)%Name)
                end do
                do cdx=1,NumColleges+1
                    PSGCcount(0,cdx) = sum(PSGCcount(1:row,cdx)) ! college sum
                end do
                call display_PSGC_count(device, row, NumColleges, trim(tCollege), 'Barangay', &
                    'Students from '//text_PSGC(tCode), 'LIST', SPACE, &
                    trim(make_href(fnDistributionPSGC, 'Other municipality', &
                    A1=tCollege, A2=makePSGC_kind(tCode, kindProvince), pre=b_small//b_italic, post=e_italic//e_small)) )

            end if

        end if

        write(device,AFORMAT) horizontal

    end subroutine  distribution_by_PSGC


    subroutine display_PSGC_count(device, nrows, ncols, group, location, header, action1, action2, link)
        integer, intent(in) :: device, nrows, ncols
        character(len=*), intent(in) :: group, header, location, action1, action2
        character(len=*), intent(in), optional :: link

        integer :: i, j, colorIdx
        character (len=MAX_LEN_PSGC_NAME) :: tName

        call html_write_header(device, header)

        write(device,AFORMAT) '<table width="100%" cellspacing="0" cellpadding="0">', b_tr, &
            b_tdac//b_bold//b_small//'Total'//e_small//e_bold//e_td,&
            b_td//b_bold//location//e_bold//e_td, &
            (b_tdac//trim(College(j)%Code)//e_td, j=1,ncols), &
            e_tr, &
            b_tr//'<td colspan="'//trim(itoa(2+ncols))//'">'//horizontal//e_td//e_tr

        colorIdx = 0
        do i=1,nrows
            colorIdx = colorIdx+1
            write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(colorIdx,2))//'">', b_tdac
            ! total
            if (PSGCcount(i,ncols+1)>0 .and. len_trim(action1)>0 .and. &
                    .not. (isRoleChair .or. isRoleFaculty .or. isRoleStudent .or. isRoleGuest) ) then
                write(device,AFORMAT) trim(make_href(fnDistributionPSGC, itoa( PSGCcount(i,ncols+1) ), &
                    A1=group, A2=PSGC(PSGCcount(i,0))%Code, A3=action1))
            else
                write(device,AFORMAT) trim(itoabz( PSGCcount(i,ncols+1) ) )
            end if
            write(device,AFORMAT) e_td//b_td//b_small
            ! Name of place
            tName = PSGC(PSGCcount(i,0))%Name
            if (len_trim(action2)>0 .and. PSGCcount(i,ncols+1)>0) then
                write(device,AFORMAT) trim(make_href(fnDistributionPSGC, tName, &
                    A1=group, A2=PSGC(PSGCcount(i,0))%Code, A3=action2))
            else
                write(device,AFORMAT) trim(tName)
            end if
            write(device,AFORMAT) e_small//e_td, & ! name
                (b_tdac//trim(itoabz( PSGCcount(i,j) ) )//e_td, j=1,ncols), &
                e_tr
        end do
        ! column totals
        write(device,AFORMAT) &
            b_tr//'<td colspan="'//trim(itoa(2+ncols))//'">'//horizontal//e_td//e_tr, b_tr, &
            b_tdac//trim(itoabz(PSGCcount(0,ncols+1)))//e_td, &
            b_td_nbsp_e_td, & !b_td//b_bold//b_small//'Total'//e_small//e_bold//e_td, &
            (b_tdac//trim(itoa(PSGCcount(0,j)))//e_td, j=1,ncols), &
            e_tr, &
            b_tr, &
            b_tdac//b_bold//b_small//'Total'//e_small//e_bold//e_td,&
            b_td//b_bold//location//e_bold//e_td, &
            (b_tdac//trim(College(j)%Code)//e_td, j=1,ncols), &
            e_tr
        write(device,AFORMAT) e_table

        if (present(link)) write(device,AFORMAT) b_para, link, e_para

    end subroutine display_PSGC_count


    subroutine file_uploadCSV(device)
        integer, intent (in) :: device

        integer :: numEntries, iYear, iTerm, ierr
        character(len=MAX_LEN_FILE_PATH) :: filename, filenameInCAPS

        call cgi_get_named_string(QUERY_STRING, 'file', filename, ierr)
        call cgi_get_named_integer(QUERY_STRING, 'year', iYear, ierr)
        call cgi_get_named_integer(QUERY_STRING, 'term', iTerm, ierr)

        filenameInCAPS = trim(dirUPLOADS)//trim(filename)

        numEntries = index(filenameInCAPS, '.gz')
        if (numEntries==0) numEntries = index(filenameInCAPS, '.GZ')
        if (numEntries>0) then
            call system('gunzip '//trim(filenameInCAPS), ierr)
            if (ierr/=0) then
                UserRequestCheckMessage = 'Error '//trim(itoa(ierr))//' in "gunzip '//trim(filename)//'"'
                call html_college_links(device, CollegeIdxUser, mesg=UserRequestCheckMessage)
                return
            else
                filenameInCAPS(numEntries:) = SPACE
                numEntries = index(filename, '.gz')
                if (numEntries==0) numEntries = index(filename, '.GZ')
                filename(numEntries:) = SPACE
            end if
        end if

!        numEntries = index(filenameInCAPS, '.tar')
!        if (numEntries==0) numEntries = index(filenameInCAPS, '.TAR')
!        if (numEntries>0) then
!            call system('tar xf '//trim(filenameInCAPS)//' -C '//trim(dirUPLOADS), ierr)
!            if (ierr/=0) then
!                UserRequestCheckMessage = 'Error '//trim(itoa(ierr))//' in "tar xf '//trim(filename)//'"'
!                call html_college_links(device, CollegeIdxUser, mesg=UserRequestCheckMessage)
!                return
!            else
!                filenameInCAPS(numEntries:) = SPACE
!                numEntries = index(filename, '.tar')
!                if (numEntries==0) numEntries = index(filename, '.TAR')
!                filename(numEntries:) = SPACE
!            end if
!        end if

        filenameInCAPS = filename
        call upper_case(filenameInCAPS)

        if (filenameInCAPS(1:8)=='STUDENTS') then
            call read_new_students (trim(dirUPLOADS)//trim(filename), numEntries)
            if (numEntries>0) call student_details_write(unitXML, trim(dirSTUDENTS)//'index', 1, NumStudents+NumAdditionalStudents)
            UserRequestCheckMessage = itoa(numEntries)//' students added from '//fileName

!        else ! if (filenameInCAPS(1:10)=='FINALGRADE' .or. filenameInCAPS(1:27)=='STDNO-NAME-SUBJ-TITLE-GRADE') then
!            if (iYear<baseYear .or. iYear>currentYear .or. iTerm<firstSemester .or. iTerm>summerTerm) then
!                UserRequestCheckMessage = 'Invalid year/term '//trim(itoa(iYear))//FSLASH//itoa(iTerm)
!            else
!                ! make directory
!                call make_directory( trim(dirDATA)//trim(itoa(iYear))//DIRSEP//trim(txtSemester(iTerm)) )
!                filenameInCAPS = trim(dirDATA)//trim(itoa(iYear))//DIRSEP//trim(txtSemester(iTerm))//DIRSEP//filenameInCAPS
!                ! move file
!                call rename (trim(dirUPLOADS)//trim(filename), filenameInCAPS, ierr)
!                if (ierr==0) then
!                    UserRequestCheckMessage = trim(fileName)//' uploaded to '//fileNameInCAPS
!                else
!                    UserRequestCheckMessage = 'Error in uploading '//fileName
!                end if
!            end if

        else
            UserRequestCheckMessage = trim(filename)//' - file not recognized as a data source.'

        end if
        call html_college_links(device, CollegeIdxUser, mesg=UserRequestCheckMessage)

    end subroutine file_uploadCSV


    subroutine distribution_by_gender(device, thisTerm)
        integer, intent (in) :: device, thisTerm

        integer :: ierr, levelOfDetail, idxProg, idxCurr, idxYear, iYearLevel, iGender
        integer :: cdx, gdx, ldx, colorIdx, nrows, nCols, maxCount, rsum, psum, MaxYearLevelCollege
        integer :: std, n_count, tdx
        logical :: addLinks

        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_CURRICULUM_CODE) :: tProgram, tCurriculum

        character(len=13) :: genderDesc(MALE:FEMALE) = (/'Male         ', 'Not Specified', 'Female       ' /)
        character(len=1) :: genderCode(MALE:FEMALE) = (/'M', ' ', 'F' /)

        call student_counts(thisTerm)
        levelOfDetail = 1 ! university summary

        ! A1=College()%Code
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        targetCollege = index_to_college(tCollege)
        if (targetCollege/=NumColleges) levelOfDetail = levelOfDetail + 2 ! university + programs in college
        addLinks = isRole_dean_of_college(targetCollege, orHigherUp)

        ! A2=CurrProgCode()
        idxProg = NumCurricula
        call cgi_get_named_string(QUERY_STRING, 'A2', tProgram, ierr)
        if (len_trim(tProgram)==0) then
            tProgram = 'ALL'
        else
            do idxProg=1,NumCurricula-1
                if (CurrProgCode(idxProg)==tProgram) exit
            end do
            levelOfDetail = levelOfDetail + 4 ! university + programs in college + curriculum
        end if

        ! A3=Curriculum()%Code
        call cgi_get_named_string(QUERY_STRING, 'A3', tCurriculum, ierr)
        idxCurr = index_to_curriculum(tCurriculum)
        if (idxCurr/=0) levelOfDetail = levelOfDetail + 8 ! university + programs in college + a curriculum + students of curriculum

        ! A4=idxYear
        call cgi_get_named_integer(QUERY_STRING, 'A4', idxYear, ierr)

        ! A5=iGender
        call cgi_get_named_integer(QUERY_STRING, 'A5', iGender, ierr)

        ! start page
        call html_write_header(device, SPACE)


        call html_comment('distribution_by_gender(college='//trim(tCollege)//' details='//trim(itoa(levelOfDetail))//')')

        ! calculate MaxYearLevel of curricula in college
        maxcount = 0
        do ldx=1,NumCurricula
            if (CurriculumCount(ldx)==0) cycle
            maxcount = max(maxcount, (Curriculum(ldx)%NumTerms+2)/3)
        end do
        MaxYearLevelCollege = maxcount

        ! colspan for line separator
        nCols = (2 + MaxYearLevelCollege-YEAR_NOT_SET+1)*3
        ! [(level, total, %prev_level) + (all years M,U,F) + (#yearlevels M,U,F)] * 3 (MALE, UNKNOWN, FEMALE)

        if (levelOfDetail>=8) then  ! students of a curriculum

            do iGender=MALE,FEMALE
                tArray = 0
                n_count = 0
                do tdx=1,NumStudents+NumAdditionalStudents
                    std = StdRank(tdx)
                    if (Student(std)%CurriculumIdx /= idxCurr) cycle
                    if (Student(std)%Gender /= genderCode(iGender)) cycle
                    if (idxYear>=YEAR_NOT_SET .and. idxYear<=MaxYearLevelCollege) then
                        if (Student(std)%Enlistment(thisTerm)%levelYear/=idxYear ) cycle
                    end if
                    n_count = n_count + 1
                    tArray(n_count) = std
                end do
                if (n_count==0) cycle

                write(device,AFORMAT) b_para//linebreak//b_bold//trim(tCollege)//' students'// &
                    ', Gender = '//genderDesc(iGender)//', Curriculum = '//trim(Curriculum(idxCurr)%Code)// &
                    ', Year level = '//txtYear(idxYear+10), &
                    e_bold//e_para
                call html_student_list (device, n_count, tArray, addLinks, SPACE)
                write(device,AFORMAT) horizontal

            end do

            levelOfDetail = levelOfDetail - 8
        end if

        if (idxCurr==0 .and. levelOfDetail>=4 .and. idxYear>=YEAR_NOT_SET .and. idxYear<=MaxYearLevelCollege) then  ! students of a program

            do iGender=MALE,FEMALE
                tArray = 0
                n_count = 0
                do tdx=1,NumStudents+NumAdditionalStudents
                    std = StdRank(tdx)
                    if (CurrProgNum(Student(std)%CurriculumIdx) /= CurrProgNum(idxProg) ) cycle
                    if (Student(std)%Gender /= genderCode(iGender)) cycle
                    if (idxYear>=YEAR_NOT_SET .and. idxYear<=MaxYearLevelCollege) then
                        if (Student(std)%Enlistment(thisTerm)%levelYear/=idxYear ) cycle
                    end if
                    n_count = n_count + 1
                    tArray(n_count) = std
                end do
                if (n_count==0) cycle

                write(device,AFORMAT) b_para//linebreak//b_bold//trim(tCollege)//' students'// &
                    ', Gender = '//genderDesc(iGender)//', Program = '//trim(CurrProgCode(idxProg))// &
                    ', Year level = '//txtYear(idxYear+10), &
                    e_bold//e_para
                call html_student_list (device, n_count, tArray, addLinks, SPACE)
                write(device,AFORMAT) horizontal

            end do

        end if

        write(device,AFORMAT) b_para, &
            '<table width="100%" cellspacing="0" cellpadding="0">'

        if (levelOfDetail>=4) then  ! specific curricula of a program + programs in college + colleges

            write(device,AFORMAT) b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//b_para//linebreak//  &
                b_bold//' Gender distribution - '//trim(CurrProgCode(idxProg))//' students'//e_bold, &
                e_para//e_td//e_tr, &
                b_tr, &
                '<td colspan="3">'//nbsp//e_td, &
                '<td align="right" colspan="3">ALL YEAR LEVELS'//e_td, &
                '<td align="right" colspan="3">Done or YNS'//e_td, &
                ('<td align="right" colspan="3">'//trim(txtYear(iYearLevel))//' YEAR'//e_td, &
                    iYearLevel=YEAR_NOT_SET+1,MaxYearLevelCollege), &
                e_tr, &
                b_tr, &
                b_td//b_small//'CURRICULUM'//e_small//e_td//b_tdar//'Total'//e_td//b_tdar//'%prog'//e_td, &
                (b_tdar//'M'//e_td//b_tdar//'?'//e_td//b_tdar//'F'//e_td, iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege+1), &
                e_tr,  &
                b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr

            done = .false.
            cdx = CurrProgNum(idxProg)  ! generic program

            maxcount = 0
            do ldx=1,NumCurricula
                if (CurrProgNum(ldx) /= cdx) cycle
                maxcount = maxcount + sum(GenderCount(ldx,:,MALE:FEMALE))
            end do

            colorIdx = 0
            nrows = 0
            do ldx=1,NumCurricula ! specific curriculum
                if (CurrProgNum(ldx) /= cdx) cycle
                done(ldx) = .true. ! signal done
                if (CurriculumCount(ldx)==0) cycle

                rsum = sum(GenderCount(ldx,YEAR_NOT_SET:MaxYearLevelCollege,MALE:FEMALE))
                if (rsum==0) cycle

                colorIdx = colorIdx+1
                nrows = nrows + 1
                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(colorIdx,2))//'">', &
                    ! curriculum, row sum, empty column
                    b_td//b_small//trim(Curriculum(ldx)%Code)//e_small//e_td, &
                    b_tdar//trim(itoa(rsum))//e_td, &
                    b_tdar//trim(ftoa((100.0*rsum)/maxcount,1))//e_td

                ! all year levels
                do iGender=MALE,FEMALE
                    psum = sum(GenderCount(ldx,YEAR_NOT_SET:MaxYearLevelCollege,iGender))
                    if (psum==0) then
                        write(device,AFORMAT) b_td_nbsp_e_td
                    else
                        write(device,AFORMAT) b_tdar//trim(itoa(psum))//e_td
                    end if
                end do

                ! each year level
                do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                    do iGender=MALE,FEMALE
                        psum = GenderCount(ldx,iYearLevel,iGender)
                        if (psum==0) then
                            write(device,AFORMAT) b_td_nbsp_e_td
                        else
                            if ( addLinks ) then
                                write(device,AFORMAT) trim(make_href(fnGenderDistribution, itoa(psum), &
                                    A1=College(targetCollege)%Code, &
                                    A2=CurrProgCode(ldx), &
                                    A3=Curriculum(ldx)%Code, &
                                    A4=itoa(iYearLevel), &
                                    A5=itoa(iGender), &
                                    A9=thisTerm, pre=b_tdar, post=e_td))
                            else
                                write(device,AFORMAT) b_tdar//trim(itoa(psum))//e_td
                            end if
                        end if
                    end do
                end do

                write(device,AFORMAT) e_tr

            end do

            ! column totals
            if (nrows>1) then
                GenderCount(0,:,:) = 0
                do ldx=1,NumCurricula
                    if (CurrProgNum(ldx) /= cdx) cycle
                    GenderCount(0,:,:) = GenderCount(0,:,:) + GenderCount(ldx,:,:)
                end do
                rsum = sum(GenderCount(0,YEAR_NOT_SET:MaxYearLevelCollege,MALE:FEMALE))
                write(device,AFORMAT) &
                    b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr, &
                    b_tr//b_td_nbsp_e_td//b_tdar//trim(itoa(rsum))//e_td//b_td_nbsp_e_td, &
                    (b_tdar//trim(itoabz(sum(GenderCount(0,YEAR_NOT_SET:MaxYearLevelCollege,iGender))))//e_td, &
                        iGender=MALE,FEMALE)

                ! each year level
                !write(device,AFORMAT) &
                !    ((b_tdar//trim(itoabz(GenderCount(0,iYearLevel,iGender)))//e_td, iGender=MALE,FEMALE), &
                !        iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege), &
                do iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege
                    do iGender=MALE,FEMALE
                        psum = GenderCount(0,iYearLevel,iGender)
                        if (psum==0) then
                            write(device,AFORMAT) b_td_nbsp_e_td
                        else
                            if ( addLinks ) then
                                write(device,AFORMAT) trim(make_href(fnGenderDistribution, itoa(psum), &
                                    A1=College(targetCollege)%Code, &
                                    A2=CurrProgCode(idxProg), &
                                    !A3=Curriculum(ldx)%Code, &
                                    A4=itoa(iYearLevel), &
                                    A5=itoa(iGender), &
                                    A9=thisTerm, pre=b_tdar, post=e_td))
                            else
                                write(device,AFORMAT) b_tdar//trim(itoa(psum))//e_td
                            end if
                        end if
                    end do
                end do

                write(device,AFORMAT) e_tr

            else
                write(device,AFORMAT) &
                    b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr
            end if

            levelOfDetail = levelOfDetail - 4

        end if


        if (levelOfDetail>=2) then  ! programs in college + university

            write(device,AFORMAT) b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//b_para//linebreak//  &
                b_bold//' Gender distribution - '//trim(tCollege)//' students'//e_bold, &
                e_para//e_td//e_tr, &
                b_tr, &
                '<td colspan="3">'//nbsp//e_td, &
                '<td align="right" colspan="3">ALL YEAR LEVELS'//e_td, &
                '<td align="right" colspan="3">Done or YNS'//e_td, &
                ('<td align="right" colspan="3">'//trim(txtYear(iYearLevel))//' YEAR'//e_td, &
                    iYearLevel=YEAR_NOT_SET+1,MaxYearLevelCollege), &
                e_tr, &
                b_tr, &
                b_td//b_small//'PROGRAM'//e_small//e_td//b_tdar//'Total'//e_td//b_tdar//'%coll'//e_td, &
                (b_tdar//'M'//e_td//b_tdar//'?'//e_td//b_tdar//'F'//e_td, iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege+1), &
                e_tr,  &
                b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr

            done = .false.
            colorIdx = 0
            nrows = 0
            maxcount = 0
            do ldx=1,NumCurricula
                if (Curriculum(ldx)%CollegeIdx /= targetCollege) cycle
                maxcount = maxcount + sum(GenderCount(ldx,:,MALE:FEMALE))
            end do
            do ldx=1,NumCurricula
                if (Curriculum(ldx)%CollegeIdx /= targetCollege) cycle
                if (done(ldx)) cycle ! done

                GenderCount(0,:,:) = GenderCount(ldx,:,:)
                done(ldx) = .true. ! signal done
                do cdx=ldx+1,NumCurricula ! specific curriculum
                    if (CurrProgNum(cdx) /= CurrProgNum(ldx)) cycle ! not the program under consideration
                    GenderCount(0,:,:) = GenderCount(0,:,:) + GenderCount(cdx,:,:)
                    done(cdx) = .true.
                end do
                rsum = sum(GenderCount(0,:,MALE:FEMALE))
                if (rsum==0) cycle

                colorIdx = colorIdx+1
                nrows = nrows + 1
                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(colorIdx,2))//'">'
                write(device,AFORMAT) trim(make_href(fnGenderDistribution, CurrProgCode(ldx), &
                    A1=College(targetCollege)%Code, &
                    A2=CurrProgCode(ldx), &
                    !A3=Curriculum(ldx)%Code, &
                    !A4=itoa(iYearLevel), &
                    !A5=itoa(iGender), &
                    A9=thisTerm, pre=b_td, post=e_td, anchor=CurrProgCode(ldx))), &
                    ! program total and percentage wrt college
                    b_tdar//trim(itoa(rsum))//e_td, b_tdar//trim(ftoa((100.0*rsum)/maxcount,1))//e_td

                ! all year levels
                write(device,AFORMAT) &
                    (b_tdar//trim(itoabz(sum(GenderCount(0,YEAR_NOT_SET:MaxYearLevelCollege,iGender))))//e_td, &
                     iGender=MALE,FEMALE)

                ! each year level
                write(device,AFORMAT) &
                    ((b_tdar//trim(itoabz(GenderCount(0,iYearLevel,iGender)))//e_td, iGender=MALE,FEMALE), &
                        iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege), &
                    e_tr

            end do

            ! column totals
            if (nrows>1) then
                GenderCount(0,:,:) = 0
                do ldx=1,NumCurricula
                    if (Curriculum(ldx)%CollegeIdx /= targetCollege) cycle ! not in college
                    GenderCount(0,:,:) = GenderCount(0,:,:) + GenderCount(ldx,:,:)
                end do
                rsum = sum(GenderCount(0,YEAR_NOT_SET:MaxYearLevelCollege,MALE:FEMALE))
                write(device,AFORMAT) &
                    b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr, &
                    b_tr//b_td_nbsp_e_td//b_tdar//trim(itoa(rsum))//e_td//b_td_nbsp_e_td, &
                    (b_tdar//trim(itoabz(sum(GenderCount(0,YEAR_NOT_SET:MaxYearLevelCollege,iGender))))//e_td, &
                        iGender=MALE,FEMALE), &
                    ((b_tdar//trim(itoabz(GenderCount(0,iYearLevel,iGender)))//e_td, iGender=MALE,FEMALE), &
                        iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege), &
                     e_tr

            else
                write(device,AFORMAT) &
                    b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr
            end if

            levelOfDetail = levelOfDetail - 2

        end if

        if (levelOfDetail>=1) then  ! colleges in university

            write(device,AFORMAT) b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//b_para//linebreak//  &
                b_bold//' Gender distribution - all students'//e_bold, &
                e_para//e_td//e_tr, &
                b_tr, &
                '<td colspan="3">'//nbsp//e_td, &
                '<td align="right" colspan="3">ALL YEAR LEVELS'//e_td, &
                '<td align="right" colspan="3">Done or YNS'//e_td, &
                ('<td align="right" colspan="3">'//trim(txtYear(iYearLevel))//' YEAR'//e_td, &
                    iYearLevel=YEAR_NOT_SET+1,MaxYearLevelCollege), &
                e_tr, &
                b_tr, &
                b_td//b_small//'COLLEGE'//e_small//e_td//b_tdar//'Total'//e_td//b_tdar//'%univ'//e_td, &
                (b_tdar//'M'//e_td//b_tdar//'?'//e_td//b_tdar//'F'//e_td, iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege+1), &
                e_tr,  &
                b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr

            maxcount = sum(GenderCount(1:,:,:)) ! all curricula, all year levels, (M, U, F)
            colorIdx = 0
            nrows = 0
            do gdx=1,NumColleges
                if (CollegeCount(gdx) <= 0) cycle

                colorIdx = colorIdx+1
                nrows = nrows + 1

                ! accummulate for all curricula in college
                GenderCount(0,:,:) = 0
                do ldx=1,NumCurricula
                    if (Curriculum(ldx)%CollegeIdx /= gdx) cycle ! not in college
                    GenderCount(0,:,:) = GenderCount(0,:,:) + GenderCount(ldx,:,:)
                end do
                rsum = sum(GenderCount(0,:,:))

                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(colorIdx,2))//'">', &
                    trim(make_href(fnGenderDistribution, College(gdx)%Code, &
                    A1=College(gdx)%Code, &
                    !A2=CurrProgCode(ldx), &
                    !A3=Curriculum(ldx)%Code, &
                    !A4=itoa(iYearLevel), &
                    !A5=itoa(iGender), &
                    A9=thisTerm, pre=b_td, post=e_td)), &
                    b_tdar//trim(itoa(rsum))//e_td, &
                    b_tdar//trim(ftoa((100.0*rsum)/maxcount,1))//e_td

                ! all year levels
                write(device,AFORMAT) &
                    (b_tdar//trim(itoabz(sum(GenderCount(0,YEAR_NOT_SET:MaxYearLevelCollege,iGender))))//e_td, &
                     iGender=MALE,FEMALE)

                ! each year level
                write(device,AFORMAT) &
                    ((b_tdar//trim(itoabz(GenderCount(0,iYearLevel,iGender)))//e_td, iGender=MALE,FEMALE), &
                        iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege), &
                    e_tr

            end do
            ! line separator
            write(device,AFORMAT) b_tr//'<td colspan="'//trim(itoa(nCols))//'">'//horizontal//e_td//e_tr

            ! column totals
            GenderCount(0,:,:) = 0
            do ldx=1,NumCurricula
                GenderCount(0,:,:) = GenderCount(0,:,:) + GenderCount(ldx,:,:)
            end do
            rsum = sum(GenderCount(0,YEAR_NOT_SET:MaxYearLevelCollege,MALE:FEMALE))

            ! no name, total, no %
            write(device,AFORMAT) &
                b_tr//b_td_nbsp_e_td//b_tdar//trim(itoa(rsum))//e_td//b_td_nbsp_e_td, &
                (b_tdar//trim(itoabz(sum(GenderCount(0,YEAR_NOT_SET:MaxYearLevelCollege,iGender))))//e_td, &
                    iGender=MALE,FEMALE), &
                ((b_tdar//trim(itoabz(GenderCount(0,iYearLevel,iGender)))//e_td, iGender=MALE,FEMALE), &
                    iYearLevel=YEAR_NOT_SET,MaxYearLevelCollege), &
                 e_tr

        end if

        write(device,AFORMAT) e_table, e_para

        ! legends
        write(device,AFORMAT)  b_para, b_small, &
            b_bold//'Year levels'//e_bold//nbsp, &
            ' : Done - '//b_italic//'Completed academic requirements'//e_italic, &
            ' : YNS - '//b_italic//'Year not set'//e_italic, &
            ' : FIRST, SECOND, ... - '//b_italic//'Year in curriculum'//e_italic, &
            linebreak, &
            b_bold//'Gender'//e_bold//nbsp, &
            ' : M - '//b_italic//'Male'//e_italic, &
            ' : ? - '//b_italic//'Not specified'//e_italic, &
            ' : F - '//b_italic//'Female'//e_italic, &
            e_small, e_para

        !end page
        write(device,AFORMAT) horizontal

    end subroutine distribution_by_gender

