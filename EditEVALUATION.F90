!======================================================================
!
!    HEEDS (Higher Education Enrollment Decision Support) - A program
!      to create enrollment scenarios for 'next term' in a university
!    Copyright (C) 2012-2016 Ricolindo L. Carino
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


module EditEVALUATION

    use HTML

    implicit none

contains


    subroutine  effectiveness_summary_table (device, thisTerm, fileCount, sumRating, &
            remarksFile, header1, header2)
        integer, intent(in) :: device, thisTerm
        integer, dimension(MAX_EVALUATION_TYPES), intent(in) :: fileCount
        real, dimension(MAX_EVALUATION_TYPES,MAX_EVALUATION_CRITERIA), intent(in) :: sumRating
        character (len=*), intent(in) :: remarksFile, header1
        character (len=*), optional, intent(in) :: header2

        integer :: grpIdx, criterionIdx, iType, NumEvaluatorGroups, colorIdx
        real :: groupTotal(MAX_EVALUATION_TYPES), grandTotal(MAX_EVALUATION_TYPES), aver

        call html_write_header(device, 'Summary Ratings for Instruction/Teaching Effectiveness')
        write(device,AFORMAT) &
            b_italic//'Rating period : '//e_italic//trim(text_term_school_year (thisTerm, currentYear)), linebreak
        if (len_trim(header1)>0) write(device,AFORMAT) trim(header1), linebreak
        if (present(header2)) write(device,AFORMAT) header2, linebreak

        write(device,AFORMAT) b_italic//'Evaluator count : '//e_italic
        NumEvaluatorGroups = 0
        do iType=1,MAX_EVALUATION_TYPES
            if (fileCount(iType)>0) then
                numEvaluatorGroups = numEvaluatorGroups + 1
                write(device,AFORMAT) b_bold//EvalTypeDescription(iType)(1:4)//e_bold// &
                    trim(EvalTypeDescription(iType)(5:))//' = '//trim(itoa(fileCount(iType)))//' /'//nbsp
            end if
        end do
        if (NumEvaluatorGroups==0) then
            write(device,AFORMAT) red//' No evaluation records found.'//e_color
            return
        end if

        write(device,AFORMAT) &
            linebreak, &
            b_italic//'Report generated : '//e_italic//currentDate//FSLASH//currentTime(1:2)//':'//currentTime(3:4), linebreak, &
            b_italic//'Ratings : '//e_italic, &
            (trim(EvalRating(grpIdx))//' /'//nbsp, grpIdx=1,MAX_EVALUATION_RATINGS), &
            linebreak, &
            linebreak, &
            '<table>'

        grandTotal = 0.0
        do grpIdx=1,NumEvalCategories

            write(device,AFORMAT) b_tr
            do iType=1,MAX_EVALUATION_TYPES
                if (fileCount(iType)>0) &
                    write(device,AFORMAT) b_th//EvalTypeDescription(iType)(1:4)//e_th
            end do
            write(device,AFORMAT) &
                b_thal//EvalCategory(grpIdx)%Code//DOT, &
                trim(EvalCategory(grpIdx)%Description)//' ('// &
                trim(itoa(EvalCategory(grpIdx)%Weight))//'%)'//e_th, &
                e_tr

            groupTotal = 0
            colorIdx = 0
            do criterionIdx=1,NumEvalCriteria
                if (grpIdx/=Evaluation(criterionIdx)%Category) cycle

                colorIdx = colorIdx + 1
                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(colorIdx,2))//'">' ! b_tr
                do iType=1,MAX_EVALUATION_TYPES
                    aver = sumRating(iType,criterionIdx) / max(1,fileCount(iType))
                    groupTotal(iType) = groupTotal(iType) + aver
                    if (fileCount(iType)>0) &
                        write(device,'(a,f6.2,a)') b_tdar, aver, e_td
                end do

                write(device,AFORMAT) &
                    b_td//EvalCategory(grpIdx)%Code//trim(itoa(Evaluation(criterionIdx)%Item))//DOT// &
                        SPACE//trim(Evaluation(criterionIdx)%Criterion)//e_td, &
                    e_tr
            end do
            ! weight groupTotal; accumulate
            do iType=1,MAX_EVALUATION_TYPES
                groupTotal(iType) = groupTotal(iType)*EvalCategory(grpIdx)%Weight/(100.0/NumEvalCategories)
                grandTotal(iType) = grandTotal(iType) + groupTotal(iType)
            end do

            write(device,AFORMAT) b_tr
            do iType=1,MAX_EVALUATION_TYPES
                if (fileCount(iType)>0) &
                    write(device,'(a,f6.2,a)') b_th, groupTotal(iType), e_th
            end do

            write(device,AFORMAT) &
                b_thal//'Weighted subtotal'//e_th, &
                e_tr, &
                b_tr//'<td colspan="'//trim(itoa(NumEvaluatorGroups+1))//'">'//nbsp//e_td//e_tr
        end do
        write(device,AFORMAT) b_tr//'<td colspan="'//trim(itoa(NumEvaluatorGroups+1))//'">'//nbsp//e_td//e_tr

        write(device,AFORMAT) b_tr
        do iType=1,MAX_EVALUATION_TYPES
            if (fileCount(iType)>0) &
                write(device,AFORMAT) b_th//EvalTypeDescription(iType)(1:4)//e_th
        end do
        write(device,AFORMAT) b_td_nbsp_e_td, e_tr

        ! average accumulated group totals
        do iType=1,MAX_EVALUATION_TYPES
            grandTotal(iType) = grandTotal(iType)/NumEvaluatorGroups
        end do

        write(device,AFORMAT) b_tr
        do iType=1,MAX_EVALUATION_TYPES
            if (fileCount(iType)>0) &
                write(device,'(a,f6.2,a)') b_th, grandTotal(iType), e_th
        end do
        write(device,AFORMAT) b_thal//'Total weighted score'//e_th, e_tr, &
            b_tr//'<td colspan="'//trim(itoa(NumEvaluatorGroups+1))//'">'//nbsp//e_td//e_tr

        if (NumEvaluatorGroups>1) &
            write(device,AFORMAT) b_tr//'<td colspan="'//trim(itoa(NumEvaluatorGroups-1))//'">'//nbsp//e_td
        write(device,'(a,f6.2,a)') b_th, sum(grandTotal), e_th
        write(device,AFORMAT) b_thal//'TOTAL SCORE'//e_th, e_tr, e_table

        if (len_trim(remarksFile)>0) then
            write(device,AFORMAT) b_para, b_bold//'Remarks'//e_bold, e_para
            call copy_to_unit(remarksFile, device)
        end if
        write(device,AFORMAT) horizontal

    end subroutine effectiveness_summary_table


    subroutine evaluation_ratings(device, thisTerm)
        integer, intent(in) :: device, thisTerm
        character(len=MAX_LEN_USERNAME) :: tTeacher
        character(len=MAX_LEN_EVAL_TYPE_DESCRIPTION) :: tEvalTypeDesc
        character (len=MAX_LEN_CLASS_ID) :: tClassId
        integer :: ierr

        ! which teacher, what evaluation type ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, ierr)
        call cgi_get_named_string(QUERY_STRING, 'A2', tEvalTypeDesc, ierr)
        call cgi_get_named_string(QUERY_STRING, 'A3', tClassID, ierr)

        if (trim(tEvalTypeDesc)=='College') then
            targetCollege = index_to_college(tTeacher)
            call effectiveness_summary_college(device, targetCollege, thisTerm)

        else
            targetTeacher = index_to_teacher(tTeacher)
            if (targetTeacher==0) then
                call html_college_links(device, CollegeIdxUser, mesg='Teacher "'//trim(tTeacher)//'" not found.')

            else
                targetDepartment = Teacher(targetTeacher)%DeptIdx
                targetCollege = Department(targetDepartment)%CollegeIdx

                if (trim(tEvalTypeDesc)=='Teacher') then
                    call effectiveness_summary_teacher(device, targetTeacher, thisTerm)

                else if (trim(tEvalTypeDesc)=='Subject') then
                    call effectiveness_summary_subject_by_teacher(device, targetTeacher, thisTerm, tClassID)

                end if

            end if
        end if

    end subroutine evaluation_ratings


    subroutine evaluation_form(device, thisTerm)
        integer, intent(in) :: device, thisTerm
        character(len=MAX_LEN_USERNAME) :: tTeacher, tAction
        character(len=MAX_LEN_EVAL_TYPE_DESCRIPTION) :: tEvalTypeDesc
        character(len=MAX_LEN_PERSON_NAME) :: tName
        integer :: ierr, grpIdx, criterionIdx, iType, tdx, idx_select, score, colorIdx
        real :: groupTotal, grandTotal
        character (len=MAX_LEN_CLASS_ID+3) :: tClassId
        character (len=MAX_LEN_FILE_PATH) :: evaluationFile
        character (len=2048) :: tComments
        character (len=255) :: remark

        ! which teacher, what evaluation type ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, ierr)
        call cgi_get_named_string(QUERY_STRING, 'A2', tEvalTypeDesc, ierr)
        call cgi_get_named_string(QUERY_STRING, 'A3', tClassID, ierr)

        targetTeacher = index_to_teacher(tTeacher)
        if (targetTeacher==0) then
            call html_college_links(device, CollegeIdxUser, mesg='Teacher "'//trim(tTeacher)//'" not found.')
            return
        end if
        targetDepartment = Teacher(targetTeacher)%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx

        ! evaluation file
        do iType=1,MAX_EVALUATION_TYPES
            if (tEvalTypeDesc==EvalTypeDescription(iType)) exit
        end do
        evaluationFile = trim(tEvalTypeDesc)//SPACE//trim(USERNAME)//SPACE//tClassID
        ! add path and file type
        evaluationFile = trim(dirEVALUATIONS(thisTerm))//trim(tTeacher)//DIRSEP// &
            trim(filename_from(evaluationFile))//dotXML

        call html_comment('teacher_evaluate('//trim(evaluationFile)//')')

        tName = Teacher(targetTeacher)%Name
        ierr = index(tName, COMMA//SPACE)
        if (ierr>0) then ! switch first & last names
            tName = trim(tName(ierr+2:))//SPACE//tName(:ierr-1)
        end if

        remark = SPACE
        tComments = SPACE

        call evaluation_details_read(evaluationFile)
        if (sum(EvaluationForm%Rating)==0 .and. len_trim(EvaluationForm%Comments)==0) then ! no previous record
            EvaluationForm%EvalType = iType
            EvaluationForm%Year = currentYear
            EvaluationForm%Term = thisTerm
            EvaluationForm%EvaluatedID = tTeacher
            EvaluationForm%EvaluatorID = USERNAME
            EvaluationForm%ClassID = tClassID
            EvaluationForm%Comments = tComments
            EvaluationForm%LastModified = currentDate//FSLASH//currentTime(1:2)//':'//currentTime(3:4)
            remark = 'No prior evaluation of '//trim(tTeacher)//' by '//USERNAME
        else
            remark = 'Retrieved '//EvaluationForm%LastModified//' evaluation of '//trim(tTeacher)//' by '//USERNAME
        end if

        ! update?
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)

        if (trim(tAction)=='Update') then

            do criterionIdx=1,NumEvalCriteria
                call cgi_get_named_integer(QUERY_STRING, 'score'//itoa(criterionIdx), score, ierr)
                if (ierr/=0) cycle
                EvaluationForm%Rating(criterionIdx) = score
            end do

            call cgi_get_named_string(QUERY_STRING, 'Comments', tComments, ierr)
            if ( ierr==0 .and. len_trim(tComments)>0) EvaluationForm%Comments = tComments

            call date_and_time (date=currentDate, time=currentTime)
            EvaluationForm%LastModified = currentDate//FSLASH//currentTime(1:2)//':'//currentTime(3:4)

            call evaluation_details_write(evaluationFile)

        end if

        ! display evaluation form
        call html_write_header(device, 'Instrument for Instruction/Teaching Effectiveness '//tClassID, remark)
        write(device,AFORMAT) &
            b_italic//'Rating period : '//e_italic//trim(text_term_school_year (thisTerm, currentYear)), linebreak, &
            b_italic//'Evaluator : '//e_italic//trim(tEvalTypeDesc)//' ('//trim(USERNAME)//')', linebreak, &
            b_italic//'Name of faculty : '//e_italic//trim(tName)
        if (len_trim(tClassID)>0) write(device,AFORMAT) ' ('//trim(tClassID)//')'
        write(device,AFORMAT)linebreak,  &
            b_italic//'Academic rank : '//e_italic//trim(AcademicRank(Teacher(targetTeacher)%Rank))//nbsp// &
                trim(RankStep(Teacher(targetTeacher)%Step)), linebreak, &
            b_italic//'Last modified : '//e_italic//trim(EvaluationForm%LastModified), linebreak, &
            b_italic//'Ratings : '//e_italic, &
            (trim(EvalRating(idx_select))//' /'//nbsp, idx_select=1,MAX_EVALUATION_RATINGS), &
            linebreak, linebreak, &
            '<table>'

        grandTotal = 0.0
        call make_form_start(device, fnEvaluationForm, tTeacher, tEvalTypeDesc, tClassId)
        do grpIdx=1,NumEvalCategories

            write(device,AFORMAT) b_tr, &
                b_th//'Score'//e_th, &
                b_thal//EvalCategory(grpIdx)%Code//DOT, &
                    trim(EvalCategory(grpIdx)%Description)//' ('// &
                    trim(itoa(EvalCategory(grpIdx)%Weight))//'%)'//e_th, &
                e_tr

            colorIdx = 0
            groupTotal = 0.0
            do criterionIdx=1,NumEvalCriteria
                if (grpIdx/=Evaluation(criterionIdx)%Category) cycle

                colorIdx = colorIdx + 1
                write(device,AFORMAT) '<tr bgcolor="'//bgcolor(mod(colorIdx,2))//'">', & ! b_tr
                    b_td//'<select name="score'//trim(itoa(criterionIdx))//'">'
                do score=0,MAX_EVALUATION_RATINGS
                    if (score/=EvaluationForm%Rating(criterionIdx)) then
                        idx_select = 0
                    else
                        idx_select = 1
                        groupTotal = groupTotal + score
                    end if
                    write(device,AFORMAT) '<option value="'//trim(itoa(score))//'" '//trim(selected(idx_select))//'> '// &
                        EvalRating(score)
                end do
                write(device,AFORMAT) e_select//e_td, &
                    b_td//EvalCategory(grpIdx)%Code//trim(itoa(Evaluation(criterionIdx)%Item))//DOT// &
                        SPACE//trim(Evaluation(criterionIdx)%Criterion)//e_td, &
                    e_tr
            end do
            groupTotal = groupTotal*EvalCategory(grpIdx)%Weight/(100.0/NumEvalCategories)
            grandTotal = grandTotal + groupTotal

            write(device,AFORMAT) b_tr, &
                b_th//ftoa(groupTotal,2)//e_th, &
                b_thal//'Weighted subtotal'//e_th, &
                e_tr, &
                b_tr//'<td colspan="2">'//nbsp//e_td//e_tr

        end do

        write(device,AFORMAT) b_tr//'<td colspan="2">'//nbsp//e_td//e_tr, &
            b_tr, &
            b_th//ftoa(grandTotal,2)//e_th, &
            b_thal//'TOTAL SCORE'//e_th, &
            e_tr, &
            e_table, linebreak, linebreak

        write(device,AFORMAT) &
            b_para//b_bold//'Remarks'//e_bold//e_para, &
            linebreak, &
            '<textarea cols="40" rows="5" name="Comments">'// &
                trim(EvaluationForm%Comments)// &
            '</textarea>', &
            linebreak
        write(device,AFORMAT) &
            linebreak//nbsp//nbsp//' <input type="submit" name="action" value="Update">', &
            linebreak//linebreak//e_form

        ! links to summary, evaluation assignments
        if (trim(USERNAME)==trim(tTeacher) ) then

            write(device,AFORMAT) linebreak, &
                trim(make_href(fnEvaluationRatings, 'report', &
                A1=tTeacher, A2='Teacher', A9=thisTerm, &
                pre=b_para//b_bold//'My evaluation'//nbsp, post=e_bold//e_para))

            write(device,AFORMAT) linebreak, &
                b_bold//'My peer evaluation assignments'//e_bold, e_para, &
                '<table>'

            evaluationFile = trim(dirEVALUATIONS(thisTerm))//'EVALUATION_DUTIES'
            idx_select = 0
            tArray = 0
            call evaluation_duty_read(evaluationFile, idx_select, tArray)

            score = 0
            do iType=1,idx_select
                if (tArray(2*iType-1)/=requestingTeacher) cycle
                score = score + 1
                tdx = tArray(2*iType)
                write(device,AFORMAT) b_tr, &
                    b_tdac//trim(itoa(score))//DOT//e_td, &
                    b_td//trim(Teacher(tdx)%Name)//e_td
                if (tdx==targetTeacher) then ! currently being evaluated above
                    write(device,AFORMAT) b_td_nbsp_e_td//e_tr
                else
                    write(device,AFORMAT) trim(make_href(fnEvaluationForm, 'Evaluate', &
                        A1=Teacher(tdx)%TeacherID, A2=EvalTypeDescription(2), A9=thisTerm, &
                        pre=b_td, post=e_td//e_tr ))
                end if
            end do
            write(device,AFORMAT) e_table
            if (score==0) then
                write(device,AFORMAT) JUSTNONE
            end if

        end if
        write(device,AFORMAT) horizontal

    end subroutine evaluation_form


    subroutine  effectiveness_summary_teacher (device, iTeach, thisTerm)
        integer, intent(in) :: device, iTeach, thisTerm

        character(len=MAX_LEN_USERNAME) :: tTeacher
        character(len=MAX_LEN_PERSON_NAME) :: tName
        integer :: iType, ierr
        character (len=MAX_LEN_FILE_PATH) :: evalFilePattern, remarksFile
        integer, dimension(MAX_EVALUATION_TYPES) :: fileCount
        real, dimension(MAX_EVALUATION_TYPES,MAX_EVALUATION_CRITERIA) :: sumRating

        tTeacher = Teacher(iTeach)%TeacherId
        tName = Teacher(iTeach)%Name
        ierr = index(tName, COMMA//SPACE)
        if (ierr>0) then ! switch first & last names
            tName = trim(tName(ierr+2:))//SPACE//tName(:ierr-1)
        end if

        fileCount = 0
        sumRating = 0.0
        remarksFile = trim(dirEVALUATIONS(thisTerm))//trim(tTeacher)//DIRSEP//'REMARKS_Teacher'

        call open_for_write(unitREM, remarksFile, ierr)
        do iType=1,MAX_EVALUATION_TYPES

            evalFilePattern = trim(dirEVALUATIONS(thisTerm))//trim(tTeacher)//DIRSEP// &
                trim(EvalTypeDescription(iType))//'_*'//dotXML

            call effectiveness_ratings_retrieve (iType, evalFilePattern, fileCount, sumRating, remarksFile)

        end do
        close(unitREM)

        ! display summary evaluation form
        call effectiveness_summary_table (device, thisTerm, fileCount, sumRating, remarksFile, &
            b_italic//'Name of faculty : '//e_italic//trim(tName), &
            b_italic//'Academic rank : '//e_italic//trim(AcademicRank(Teacher(iTeach)%Rank))//nbsp// &
                trim(RankStep(Teacher(iTeach)%Step)) )

    end subroutine effectiveness_summary_teacher


    subroutine  effectiveness_summary_college (device, iColl, thisTerm)
        integer, intent(in) :: device, iColl, thisTerm

        character(len=MAX_LEN_USERNAME) :: tTeacher
        integer :: iTeach, iType, nClasses(3)
        character (len=MAX_LEN_FILE_PATH) :: evalFilePattern, remarksFile
        integer, dimension(MAX_EVALUATION_TYPES) :: fileCount
        real, dimension(MAX_EVALUATION_TYPES,MAX_EVALUATION_CRITERIA) :: sumRating

        fileCount = 0 ! how many of each evaluation type?
        sumRating = 0.0
        remarksFile = SPACE ! probably too many remarks in a college

        ! process evaluations of each teacher in college active during term
        do iTeach=1,NumTeachers
            if (Department(Teacher(iTeach)%DeptIdx)%CollegeIdx/=iColl) cycle ! not in college
            call count_teacher_load(iTeach, 0, nClasses) ! how many classes?
            if (nClasses(thisTerm)==0) cycle ! teacher has no classes this term

            tTeacher = Teacher(iTeach)%TeacherId
            do iType=1,MAX_EVALUATION_TYPES

                evalFilePattern = trim(dirEVALUATIONS(thisTerm))//trim(tTeacher)//DIRSEP// &
                    trim(EvalTypeDescription(iType))//'_*'//dotXML

                call effectiveness_ratings_retrieve (iType, evalFilePattern, fileCount, sumRating, remarksFile)

            end do

        end do

        ! display summary evaluation form
        call effectiveness_summary_table (device, thisTerm, fileCount, sumRating, remarksFile, &
            b_italic//'Name of unit : '//e_italic//trim(College(iColl)%Code)//' - '//trim(College(iColl)%Name) )

    end subroutine  effectiveness_summary_college


    subroutine  effectiveness_summary_subject_by_teacher (device, iTeach, thisTerm, tSubject)
        integer, intent(in) :: device, iTeach, thisTerm
        character (len=MAX_LEN_SUBJECT_CODE), intent(in) :: tSubject

        character(len=MAX_LEN_USERNAME) :: tTeacher
        character(len=MAX_LEN_PERSON_NAME) :: tName
        integer :: iType, ierr
        character (len=MAX_LEN_FILE_PATH) :: evalFilePattern, remarksFile
        integer, dimension(MAX_EVALUATION_TYPES) :: fileCount
        real, dimension(MAX_EVALUATION_TYPES,MAX_EVALUATION_CRITERIA) :: sumRating

        fileCount = 0 ! how many of each evaluation type?
        sumRating = 0.0
        iType = 4 ! get evaluations of type STUDENT only
        remarksFile = trim(dirEVALUATIONS(thisTerm))//trim(tTeacher)//DIRSEP//'REMARKS_Subject'

        tTeacher = Teacher(iTeach)%TeacherId
        tName = Teacher(iTeach)%Name
        ierr = index(tName, COMMA//SPACE)
        if (ierr>0) then ! switch first & last names
            tName = trim(tName(ierr+2:))//SPACE//tName(:ierr-1)
        end if

        evalFilePattern = trim(dirEVALUATIONS(thisTerm))//trim(tTeacher)//DIRSEP// &
            trim(EvalTypeDescription(iType))//'_*_'//trim(filename_from(tSubject))//dotXML

        call open_for_write(unitREM, remarksFile, ierr)
        call effectiveness_ratings_retrieve (iType, evalFilePattern, fileCount, sumRating, remarksFile)
        close(unitREM)

        ! display summary evaluation form
        call effectiveness_summary_table (device, thisTerm, fileCount, sumRating, remarksFile, &
            b_italic//'Name of faculty : '//e_italic//trim(tName), &
            b_italic//'Academic rank : '//e_italic//trim(AcademicRank(Teacher(iTeach)%Rank))//nbsp// &
                trim(RankStep(Teacher(iTeach)%Step))//linebreak// &
            b_italic//'Subject evaluated : '//e_italic//trim(tSubject) )

    end subroutine effectiveness_summary_subject_by_teacher


    subroutine evaluation_duties (device, thisTerm)
        integer, intent(in) :: device, thisTerm

        character(len=MAX_LEN_USERNAME) :: tTeacher, evaluatorID, evaluatedID, tAction
        character(len=2*MAX_LEN_USERNAME) :: tAssignment
        integer :: ierr, tLen, idx, jdx, kdx, tdx, toGive, toReceive, numChanges, numAssigned, nClasses(3)
        character (len=MAX_LEN_FILE_PATH) :: evalDutyFile
        logical :: assigned

        ! which teacher
        call cgi_get_named_string(QUERY_STRING, 'A1', tTeacher, ierr)
        ! delete/add evaluation duty ?
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)

        targetTeacher = index_to_teacher(tTeacher)
        if (targetTeacher==0) then
            call html_college_links(device, CollegeIdxUser, mesg='Teacher "'//trim(tTeacher)//'" not found.')
            return
        end if
        targetDepartment = Teacher(targetTeacher)%DeptIdx
        targetCollege = Department(targetDepartment)%CollegeIdx

        evalDutyFile = trim(dirEVALUATIONS(thisTerm))//'EVALUATION_DUTIES' !_'//College(targetCollege)%Code

        call html_comment('evaluation_duties()', tTeacher, evalDutyFile)

        tLen = 0
        tArray = 0
        call evaluation_duty_read(evalDutyFile, tLen, tArray)
        ! Teacher(tArray(2*i-1))%TeacherId evaluates Teacher(tArray(2*i))%TeacherId]
        call html_comment('evaluation assignments = '//itoa(tLen))

        numChanges = 0
        if (trim(tAction)=='Update') then

            ! get deletions
            do while (.true.)
                call cgi_get_named_string(QUERY_STRING, 'del', tAssignment, ierr)
                if (tAssignment==SPACE) exit
                ! parse assignment
                idx = index(tAssignment,COMMA)
                evaluatorID = tAssignment(:idx-1)
                evaluatedID = tAssignment(idx+1:)
                idx = index_to_teacher(evaluatorID)
                jdx = index_to_teacher(evaluatedID)
                if (idx==0 .or. jdx==0) cycle

                ! find position of assignment in tArray()
                kdx = 0
                do tdx=1,tLen
                    if (tArray(2*tdx-1)/=idx) cycle
                    if (tArray(2*tdx  )/=jdx) cycle
                    kdx = tdx
                    exit
                end do
                ! if found, delete
                if (kdx/=0) then
                    tArray(2*kdx-1) = 0
                    tArray(2*kdx  ) = 0
                    numChanges = numChanges + 1
                    call html_comment('Removed '//itoa(kdx)//tAssignment)
                end if
            end do

            ! get additions
            do while (.true.)
                call cgi_get_named_string(QUERY_STRING, 'add', tAssignment, ierr)
                if (tAssignment==SPACE) exit
                ! parse assignment
                idx = index(tAssignment,COMMA)
                evaluatorID = tAssignment(:idx-1)
                evaluatedID = tAssignment(idx+1:)
                idx = index_to_teacher(evaluatorID)
                jdx = index_to_teacher(evaluatedID)
                if (idx==0 .or. jdx==0) cycle

                ! find position of assignment in tArray()
                kdx = 0
                do tdx=1,tLen
                    if (tArray(2*tdx-1)/=idx) cycle
                    if (tArray(2*tdx  )/=jdx) cycle
                    kdx = tdx
                    exit
                end do

                ! if found, keep
                if (kdx/=0) cycle

                ! add
                tLen = tLen + 1
                tArray(2*tLen-1) = idx
                tArray(2*tLen  ) = jdx
                numChanges = numChanges + 1
                call html_comment('Added '//itoa(tLen)//tAssignment)
            end do

        end if

        ! any changes
        if (numChanges>0) then
            call evaluation_duty_write(evalDutyFile, tLen, tArray)
        end if

        ! counts for tTeacher
        toGive = 0
        toReceive = 0
        do idx=1,tLen
            if (tArray(2*idx-1)*tArray(2*idx)==0) cycle ! one or both not found
            if (targetTeacher==tArray(2*idx-1)) toGive = toGive + 1
            if (targetTeacher==tArray(2*idx  )) toReceive = toReceive + 1
        end do

        call html_write_header(device,'Evaluation assignments for '//Teacher(targetTeacher)%Name)

        call make_form_start(device, fnEvaluationDuties, tTeacher)

        ! evaluations to be given by teacher
        if (toGive==0) then
            write(device,AFORMAT) b_para, red//trim(tTeacher)//' has no peers to evaluate.'//e_color, e_para

        else

            write(device,AFORMAT) b_para, &
                b_bold//'Peers to be evaluated by '//trim(tTeacher)//' (check to remove)'//e_bold, &
                '<table>'
            do idx=1,tLen
                if (tArray(2*idx-1)/=targetTeacher) cycle
                tdx = tArray(2*idx)
                write(device,AFORMAT) b_tr, &
                    b_tdac//'<input type="checkbox" name="del" value="'// &
                    trim(tTeacher)//COMMA//trim(Teacher(tdx)%TeacherID)//'">'//e_td, &
                    b_td//trim(Teacher(tdx)%Name)//e_td, &
                    e_tr
            end do
            write(device,AFORMAT) e_table, &
                linebreak
        end if
        write(device,AFORMAT) &
            b_para//'Add a peer to be evaluated by '//trim(tTeacher)//' (number indicates evaluators of peer)'//e_para, &
            '<select name="add">', &
            '<option value="nobody">(select)'
        do tdx=1,NumTeachers
            if (Teacher(tdx)%DeptIdx/=targetDepartment) cycle
            if (tdx==targetTeacher) cycle
            ! how many assiged to evaluate peer
            numAssigned = 0
            do idx=1,tLen
                if (tdx==tArray(2*idx) ) numAssigned = numAssigned + 1
            end do
            ! peer already assigned to teacher?
            assigned = .false.
            do idx=1,tLen
                if (tArray(2*idx-1)/=targetTeacher) cycle
                if (tdx==tArray(2*idx) ) then
                    assigned = .true.
                    exit
                end if
            end do
            if (assigned) cycle
            ! any classes for peer?
            call count_teacher_load(tdx, thisTerm, nClasses)
            if (nClasses(thisTerm)==0) cycle
            write(device,AFORMAT) '<option value="'//trim(tTeacher)//COMMA//trim(Teacher(tdx)%TeacherID)//'">'// &
                trim(Teacher(tdx)%Name)//' ('//trim(itoa(numAssigned))//')'
        end do
        write(device,AFORMAT) e_select, &
            linebreak, linebreak


        ! evaluations to be received by teacher
        if (toReceive==0) then
            write(device,AFORMAT) b_para, red//'No one has been assigned to evaluate '//trim(tTeacher)//e_color, &
                e_para

        else
            write(device,AFORMAT) b_para, &
                b_bold//'Evaluators of '//trim(tTeacher)//' (check to remove)'//e_bold, &
                '<table>'
            do idx=1,tLen
                if (tArray(2*idx)/=targetTeacher) cycle
                tdx = tArray(2*idx-1)
                ! teaching any classes?
                call count_teacher_load(tdx, thisTerm, nClasses)
                if (nClasses(thisTerm)==0) then ! not teaching; remove
                    tArray(2*idx-1) = 0
                    tArray(2*idx  ) = 0
                    cycle
                end if
                ! how many assigned to evaluator
                numAssigned = 0
                do ierr=1,tLen
                    if (tdx==tArray(2*ierr-1) ) numAssigned = numAssigned + 1
                end do

                write(device,AFORMAT) b_tr, &
                    b_tdac//'<input type="checkbox" name="del" value="'// &
                    trim(Teacher(tdx)%TeacherID)//COMMA//trim(tTeacher)//'">'//e_td, &
                    b_td//trim(Teacher(tdx)%Name)//' ('//trim(itoa(numAssigned))//')'//e_td, &
                    e_tr
            end do
            write(device,AFORMAT) e_table, &
                linebreak
        end if

        write(device,AFORMAT) b_para, &
            'Add evaluator of '//trim(tTeacher)//' (number indicates assignments of evaluator)', e_para, &
            '<select name="add">', &
            '<option value="nobody">(select)'
        do tdx=1,NumTeachers
            if (Teacher(tdx)%DeptIdx/=targetDepartment) cycle
            if (tdx==targetTeacher) cycle
            numAssigned = 0
            do idx=1,tLen
                if (tdx==tArray(2*idx-1) ) numAssigned = numAssigned + 1
            end do
            ! already assigned to evaluate target teacher?
            assigned = .false.
            do idx=1,tLen
                if (tArray(2*idx)/=targetTeacher) cycle
                if (tdx==tArray(2*idx-1) ) then
                    assigned = .true.
                    exit
                end if
            end do
            if (assigned) cycle
            ! any classes
            call count_teacher_load(tdx, thisTerm, nClasses)
            if (nClasses(thisTerm)==0) cycle
            write(device,AFORMAT) '<option value="'//trim(Teacher(tdx)%TeacherID)//COMMA// &
                trim(tTeacher)//'">'// &
                trim(Teacher(tdx)%Name)//' ('//trim(itoa(numAssigned))//')'
        end do
        write(device,AFORMAT) e_select, linebreak, linebreak, &
            '<input type="submit" name="action" value="Update">', &
            e_form, horizontal

    end subroutine evaluation_duties


end module EditEVALUATION

