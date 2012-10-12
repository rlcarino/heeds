!======================================================================
!
!    HEEDS (Higher Education Enrollment Decision Support) - A program
!      to create enrollment scenarios for 'next term' in a university
!    Copyright (C) 2012 Ricolindo L Carino
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


module HTML

    use USERFUNCTIONS

    implicit none

contains


    subroutine html_sorry(fileName, mesg)

        character(len=*), intent(in) :: fileName, mesg

        integer :: device=7

        call open_for_write(device, fileName)
        write(device,AFORMAT) &
        '   <html><head><title>'//PROGNAME//'</title></head><body>', &
            '<table border="0" width="100%">'// &
            begintr//begintd//'<big><b>'//PROGNAME//VERSION//' for '// &
            trim(UniversityName)//'</b></big>'//endtd// &
            tdalignright//'<a target="0" href="http://code.google.com/p/heeds/">Help</a>'//endtd// &
            endtr//'</table><hr>'
            !tdalignright//'<a target="0" href="'//trim(PROGNAME)//'-Help.html">Help</a>'//endtd// &

        if (mesg/=space) write(device,AFORMAT)  '<br>'//red//mesg//black//'<br><br>'

        write(device,AFORMAT) '<hr>'
        call html_copyright(device)
        write(device,AFORMAT) '</body></html>'

        close(device)

        return
    end subroutine html_sorry


    subroutine html_copyright(device)
        integer, intent(in) :: device

        write(device,AFORMAT) &
            '<i>'//PROGNAME//nbsp//COPYRIGHT//'<br>', &
            'This program comes with ABSOLUTELY NO WARRANTY; for details see the ', &
            'GNU GENERAL PUBLIC LICENSE Version 3 ', &
            '(<a target="0" href="http://www.gnu.org/licenses/gpl-3.0.html">GPLv3</a>).<br>', &
            'This program is free software, and you are welcome to redistribute it under certain conditions; ', &
            ' see the GPLv3 for details.<br>', &
            CONTACT//'</i><hr>'

        return
    end subroutine html_copyright


    subroutine html_login(fname, mesg)
        character(len=*), intent(in) :: fname
        character(len=*), intent(in), optional :: mesg
        character(len=20) :: note
        character(len=MAX_LEN_USER_CODE) :: tUser
        integer :: device=7, i, j

        call open_for_write(device, fname)
        if (present(mesg)) then
            note = SPACE
        else
            note = PROGNAME//VERSION//' for'
        end if
        write(device,AFORMAT) &
            '<html><head><title>'//PROGNAME//' </title></head><body>', &
            '<form name="input" method="post" action="'//CGI_PATH//'">', &
            '<table border="0" width="100%">'// &
            begintr//begintd//'<big><b>'//trim(note)//SPACE//trim(UniversityName)//'</b></big>'//endtd// &
            tdalignright//'<a target="0" href="http://code.google.com/p/heeds/">Help</a>'//endtd// &
            endtr//'</table><hr>'
            !tdalignright//' <a target="0" href="/static/'//trim(PROGNAME)//'-Help.html">Help</a>'//endtd// &

        if (present(mesg)) write(device,AFORMAT)  '<br><br>'//red//mesg//black//'<br><br>'

        tUser = REGISTRAR
        j = index_to_user(tUser)
        write(device,AFORMAT) '<br><b>Role is : </b> <select name="U">', & ! '<option value=""> -select role-', &
            '<option value="'//trim(itoa(j))//'"> '//trim(UserList(j)%Name)
        do i=1,NumUsers
            if (i/=j) &
                write(device,AFORMAT) '<option value="'//trim(itoa(i))//'"> '//trim(UserList(i)%Name)
        end do
        write(device,AFORMAT) '</select>'//nbsp//nbsp
        if (checkPassword) then
            write(device,AFORMAT) &
                '<b>Password is : </b> <input type="password" name="P" value="">'//nbsp//nbsp
        end if
        write(device,AFORMAT) &
            '<input type="hidden" name="F" value="1"><input type="submit" value="Login">', &
            '</form><hr>'
        call html_copyright(device)
        write(device,AFORMAT) '</body></html>'
        close(device)
        return
    end subroutine html_login


    subroutine timetable_display(device, Section, TimeTable)
        integer, intent(in) :: device, TimeTable(60,6)
        type (TYPE_SECTION), intent(in), dimension (0:MAX_ALL_SECTIONS) :: Section
        integer, parameter :: period = 2 ! no. of 15 minute intervals
        integer :: i, color, ncol, sect, j, mcol
        character (len=1024) :: line
        integer :: colorIdx(60,6)

        ! background colors
        colorIdx = 0
        color = 0
        do ncol=1,56,period
            do i=6,1,-1
                if (TimeTable(ncol,i)<=0) cycle ! no class at this time
                if (colorIdx(ncol,i)/=0) cycle ! already has a color
                ! section not colored yet
                sect = TimeTable(ncol,i)
                color = color + 1
                do mcol=ncol,56,period
                    do j=6,1,-1
                        if (sect==TimeTable(mcol,j)) colorIdx(mcol,j) = mod(color,15)
                    end do
                end do
            end do
        end do

        write(device,AFORMAT) '<br><b>Weekly Timetable</b>'// &
            '<table border="1" width="100%"><small>'
        write(device,AFORMAT) begintr//beginth//'Time'//endth, &
            thaligncenter//'Mon'//endth//thaligncenter//'Tue'//endth//thaligncenter//'Wed'//endth//&
            thaligncenter//'Thu'//endth//thaligncenter//'Fri'//endth//thaligncenter//'Sat'//endth//&
            endtr
        do ncol=1,56,period
            line = SPACE
            do i=6,1,-1
                sect = TimeTable(ncol,i)
                if (sect==0) then
                    line = tdnbspendtd//line
                elseif (sect<0) then
                    line = tdaligncenter//green//'<b><i>proposed</i></b>'//black//endtd//line
                else
                    line = '<td align="center" bgcolor="'//bgcolor(colorIdx(ncol,i))//'">'// &
                        trim(Section(sect)%ClassId)//endtd//line
                end if
            end do
            line = '<td width="100">'//trim(text_time_period(ncol,ncol+period))//endtd//line
            write (device,AFORMAT) begintr//trim(line)//endtr
        end do
        write(device,AFORMAT) '</small></table>'
        return
    end subroutine timetable_display


    subroutine list_sections_to_edit(device, Section, lenSL, SectionList, &
    target_fn, target_name, target_action, permitted, heading)
        integer, intent(in) :: device, lenSL, SectionList(3*lenSL+3), target_fn
        type (TYPE_SECTION), intent(in), dimension (0:MAX_ALL_SECTIONS) :: Section
        character(len=*), intent(in) :: target_name, target_action
        logical, intent(in) :: permitted
        character (len=*), intent(in), optional :: heading
        integer :: crse, idx, mdx, rdx, sdx, tdx, previous, conflict, dept
        character (len=10) :: note
        logical :: countUnits
        real :: totalUnits, meetingUnits, totalHours, meetingHours

        if (present(heading)) write(device,AFORMAT) heading
        if (lenSL < 3) then
            write(device,AFORMAT) '<br>(None)<br>'
            return
        end if

        countUnits = target_action=='Del' .and. target_fn/=fnOFFSET+fnRoomSchedule
        totalUnits = 0.0
        totalHours = 0.0

        if (countUnits) then
            write(device,AFORMAT) '<table border="0" width="100%">'//begintr, &
            thalignleft//'Subject'//endth//thalignleft//'Section'//endth//&
            thalignleft//'Units'//endth//thalignleft//'Hours'//endth//thalignleft//'Day'//endth//&
            thalignleft//'Time'//endth//thalignleft//'Room'//endth//&
            thalignleft//'Teacher'//endth
        else
            write(device,AFORMAT) '<table border="0" width="100%">'//begintr, &
            thalignleft//'Subject'//endth//thalignleft//'Section'//endth//&
            thalignleft//'Block'//endth//thalignleft//'Seats'//endth//thalignleft//'Day'//endth//&
            thalignleft//'Time'//endth//thalignleft//'Room'//endth//&
            thalignleft//'Teacher'//endth
        end if
        if (target_fn>0) then
            write(device,AFORMAT) thalignleft//'<small>Action</small>'//endth//endtr
        else
            write(device,AFORMAT) beginth//nbsp//endth//endtr
        end if

        previous = 0
        do idx=1,lenSL,3
            sdx=SectionList(idx)
            mdx=SectionList(idx+1)
            conflict=SectionList(idx+2)
            crse = Section(sdx)%SubjectIdx
            dept = Section(sdx)%DeptIdx
#if defined DO_NOT_ENCODE
            QUERY_put = Section(sdx)%ClassId
#else
            call cgi_url_encode(Section(sdx)%ClassId, QUERY_put)
#endif

            if (conflict>=0) then
                note = SPACE
            else ! negative conflict is the undesirablity index
                note = itoa(-conflict)
            end if
            !new section ?
            if (sdx/=previous) then ! include subject, section, units/blockname, seats/hours, time, day

                if (is_lecture_lab_subject(crse)) then
                    if (is_lecture_class(sdx, Section)) then ! lecture of lecture-lab
                        meetingUnits = Subject(crse)%Units
                        meetingHours = Subject(crse)%LectHours
                    else ! lab of lecture-lab
                        meetingUnits = 0.0
                        meetingHours = Subject(crse)%LabHours
                    end if
                else if (Subject(crse)%LectHours>0.0) then ! lecture-only
                    meetingUnits = Subject(crse)%Units
                    meetingHours = Subject(crse)%LectHours
                else if (Subject(crse)%LabHours>0.0) then ! lab-only
                    meetingUnits = Subject(crse)%Units
                    meetingHours = Subject(crse)%LabHours
                end if

                totalHours = totalHours + meetingHours
                totalUnits = totalUnits + meetingUnits


                previous = sdx
                if (permitted .and. (REQUEST==fnOFFSET+fnBlockEditSection .or. &
                REQUEST==fnOFFSET+fnBlockEditSubject .or. &
                REQUEST==fnOFFSET+fnBlockEditName .or. &
                REQUEST==fnOFFSET+fnBlockCopy .or. &
                REQUEST==fnOFFSET+fnBlockSchedule)) then ! link section code to edit section
                    write(device,AFORMAT) &
                    trim(cgi_make_href(fnOFFSET+fnScheduleOfClasses, targetUser, Subject(crse)%Name, &
                    A1=Department(dept)%Code, pre=begintr//begintd, post=endtd, anchor=Subject(crse)%Name)), &
                    trim(cgi_make_href(fnOFFSET+fnScheduleEdit, targetUser, Section(sdx)%Code, &
                    A1=QUERY_put, pre=begintd, post=endtd))
                else
                    write(device,AFORMAT) &
                    begintr//begintd//trim(Subject(crse)%Name)//endtd, & !  subject
                    begintd//trim(Section(sdx)%Code)//endtd ! code
                end if
                if (countUnits) then
                    if (meetingUnits>0.0) then
                        write(device,AFORMAT) begintd//trim(ftoa(meetingUnits,1))//endtd
                    else
                        write(device,AFORMAT) tdnbspendtd ! units
                    end if
                    if (meetingHours>0.0) then
                        write(device,AFORMAT) begintd//trim(ftoa(meetingHours,2))//endtd ! hours
                    else
                        write(device,AFORMAT) tdnbspendtd ! hours
                    end if
                    write(device,AFORMAT)  &
                    begintd//txtDay(Section(sdx)%DayIdx(mdx))//endtd// &
                    begintd//trim(text_time_period(Section(sdx)%bTimeIdx(mdx), Section(sdx)%eTimeIdx(mdx)))//endtd
                else
                    write(device,AFORMAT)  &
                    begintd//trim(Section(sdx)%BlockID)//endtd// & ! BlockID
                    begintd//trim(itoa(Section(sdx)%Slots))//endtd// & ! seats
                    begintd//txtDay(Section(sdx)%DayIdx(mdx))//endtd// &
                    begintd//trim(text_time_period(Section(sdx)%bTimeIdx(mdx), Section(sdx)%eTimeIdx(mdx)))//endtd
                end if
            else ! time, day only
                write(device,AFORMAT) &
                begintd//txtDay(Section(sdx)%DayIdx(mdx))//endtd// &
                begintd//trim(text_time_period(Section(sdx)%bTimeIdx(mdx), Section(sdx)%eTimeIdx(mdx)))//endtd
            end if

            ! room
            rdx = Section(sdx)%RoomIdx(mdx)
            if (rdx > 0) then
                write(device,AFORMAT) begintd//trim(Room(rdx)%Code)//endtd
            else
                write(device,AFORMAT) begintd//'TBA'//endtd
            end if

            ! teacher
            tdx = Section(sdx)%TeacherIdx(mdx)
            if (tdx > 0) then
                write(device,AFORMAT) begintd//trim(Teacher(tdx)%Name)//endtd
            else
                write(device,AFORMAT) begintd//'TBA'//endtd
            end if

            if (sdx==SectionList(idx+3)) then ! NOT the last meeting; add SPACE for next line
                write(device,AFORMAT) begintd//endtd//endtr
                if (conflict>0) write(device,AFORMAT) &
                begintr//'<td align="center" colspan="8">'//red//'CONFLICT between '//trim(Section(sdx)%ClassId)//' and '// &
                trim(Section(conflict)%ClassId)//black//endtd//endtr
                write(device,AFORMAT) begintr//begintd//endtd//begintd//endtd//begintd//endtd//begintd//endtd
            else
                if (target_fn==fnOFFSET+fnRoomSchedule .or. target_fn==fnOFFSET+fnTeacherSchedule) then
                    if ( permitted ) then
                        write(device,AFORMAT) trim(cgi_make_href(target_fn, targetUser, target_action, &
                        A1=target_name, A2=target_action, A3=QUERY_put, &
                        pre=begintd//'<small>', post='</small>'//endtd//endtr))
                    else
                        write(device,AFORMAT) tdnbspendtd//endtr
                    end if
                elseif (target_fn==fnChangeMatriculation .or. target_fn==fnOFFSET+fnBlockEditSection) then
                    if ( permitted ) then
                        ! operate on lab classes, not lecture classes
                        if (is_lecture_lab_subject(Section(sdx)%SubjectIdx) .and.  is_lecture_class(sdx, Section)) then
                            write(device,AFORMAT) tdnbspendtd//endtr
                        else
                            write(device,AFORMAT) trim(cgi_make_href(target_fn, targetUser, target_action, &
                            A1=target_name, A2=target_action, A3=QUERY_put, &
                            pre=begintd//'<small>', post='</small>'//trim(note)//endtd//endtr))
                        end if
                    else
                        write(device,AFORMAT) tdnbspendtd//endtr
                    end if
                end if
                if (conflict>0) write(device,AFORMAT) &
                begintr//'<td align="center" colspan="8">'//red//'CONFLICT between '//trim(Section(sdx)%ClassId)//' and '// &
                trim(Section(conflict)%ClassId)//black//endtd//endtr
            end if
        end do
        if (countUnits) then
            write(device,AFORMAT) begintr//'<td colspan="9"><hr>'//endtd//endtr, &
            begintr//tdnbspendtd//begintd//'<b>Totals</b> : '//endtd// & ! code
            begintd//trim(ftoa(totalUnits,2))//endtd//begintd//trim(ftoa(totalHours,2))//endtd// & ! hours
            tdnbspendtd// tdnbspendtd// tdnbspendtd// tdnbspendtd// tdnbspendtd//endtr, &
            begintr//'<td colspan="9"><hr>'//endtd//endtr
        end if
        write(device,AFORMAT) '</table>'

        return
    end subroutine list_sections_to_edit

  
    subroutine links_to_students (device, fn, NumSections, Section)
        integer, intent (in) :: device, fn
        integer, intent (in) :: NumSections
        type (TYPE_SECTION), intent(in), dimension (0:MAX_ALL_SECTIONS) :: Section
        integer :: ldx, n_count, tdx, std, ierr, ncol, sect, crse

        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character(len=MAX_LEN_CLASS_ID) :: tClassId
        character(len=127) :: header
        !character(len=4) :: tYear
        character(len=1) :: ch

        ! collect students
        tArray = 0
        n_count = 0
        select case (fn)

            case (fnSearch)
                ! triggers for bottom menu
                targetCollege = CollegeIdxUser
                if (isRoleChair) targetDepartment = DeptIdxUser

                ! search string ?
                call cgi_get_named_string(QUERY_STRING, 'A2', header, ierr)
                if (header==SPACE) then
                    write(device,AFORMAT) '<br>'//red//'Search string not specified.'//black//'<br><hr>'
                    return
                else
                    do tdx=1,NumStudents
                        std = StdRank(tdx)
                        if (isRoleSRE .and. CollegeIdxUser/=Curriculum(std)%CollegeIdx) cycle
                        if (index(Student(std)%StdNo,trim(header))>0) then
                            n_count = n_count+1
                            tArray(n_count) = std
                        else if (index(Student(std)%Name,trim(header))>0) then
                            n_count = n_count+1
                            tArray(n_count) = std
                        end if
                    end do
                end if
                header = 'Search results for "'//trim(header)//'" students'

            case (fnClassList)
                ! which section?
                call cgi_get_named_string(QUERY_STRING, 'A1', tClassId, ierr)
                targetSection = index_to_section(tClassId, NumSections, Section)
                if (ierr/=0 .or. targetSection==0) then
                    write(device,AFORMAT) '<br>'//red//'Class "'//tClassId//'" not found.'//black//'<br><hr>'
                    targetCollege  = NumColleges ! trigger 'All colleges' link for Admin
                    return
                else
                    crse = Section(targetSection)%SubjectIdx
                    targetDepartment = Subject(crse)%DeptIdx
                    ! collect students
                    n_count = 0
                    do tdx=1,NumStudents
                        std = StdRank(tdx)
                        do ncol=1,Preenlisted(std)%lenSubject
                            sect = Preenlisted(std)%Section(ncol)
                            if (sect==0) cycle
                            if (targetSection == sect) then
                                n_count = n_count+1
                                tArray(n_count) = std
                                exit
                            elseif (Preenlisted(std)%Subject(ncol)==crse .and. is_lecture_lab_subject(crse)) then
                                ldx = index(Section(sect)%ClassId,dash)
                                if (ldx>0) then ! student is accommodated in a lab section
                                    if (trim(tClassId)==Section(sect)%ClassId(:ldx-1)) then ! lab of lecture
                                        n_count = n_count+1
                                        tArray(n_count) = std
                                    end if
                                end if
                            end if
                        end do
                    end do
                end if
                !write(device,AFORMAT) '<b>Students in '//trim(Section(targetSection)%ClassId)//'</b><hr><br>'
                header = 'Students in '//Section(targetSection)%ClassId

            case (fnStudentsByProgram)
                ! which program ?
                call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
                targetCurriculum = 0
                do ldx=1,NumCurricula
                    if (CurrProgCode(ldx) /= tCurriculum) cycle
                    targetCurriculum = ldx ! trigger links to this college
                    exit
                end do
                if (ierr/=0 .or. targetCurriculum<=0) then
                    write(device,AFORMAT) '<br>'//red//'Curriculum "'//tCurriculum//'" not found.'//black//'<br><hr>'
                    targetCollege  = NumColleges ! trigger 'All colleges' link for Admin
                    return
                else
                    do tdx=1,NumStudents
                        std = StdRank(tdx)
                        if (CurrProgCode(Student(std)%CurriculumIdx) /= tCurriculum) cycle
                        n_count = n_count+1
                        tArray(n_count) = std
                    end do
                end if
                !write(device,AFORMAT) '<b>Students in '//tCurriculum//'</b><br><hr>'
                header = 'Students in '//tCurriculum

            case (fnStudentsByCurriculum)
                ! which Curriculum ?
                call cgi_get_named_string(QUERY_STRING, 'A1', tCurriculum, ierr)
                targetCurriculum = index_to_curriculum(tCurriculum)
                if (ierr/=0 .or. targetCurriculum<=0) then
                    write(device,AFORMAT) '<br>'//red//'Curriculum "'//tCurriculum//'" not found.'//black//'<br><hr>'
                    targetCollege  = NumColleges ! trigger 'All colleges' link for Admin
                    return
                else
                    do tdx=1,NumStudents
                        std = StdRank(tdx)
                        if (Student(std)%CurriculumIdx /= targetCurriculum) cycle
                        n_count = n_count+1
                        tArray(n_count) = std
                    end do
                end if
                !write(device,AFORMAT) '<b>Students in '//tCurriculum//'</b><br><hr>'
                header = 'Students in '//tCurriculum

            case (fnStudentsByname)
                ! which college ?
                call cgi_get_named_string(QUERY_STRING, 'A2', ch, ierr)
                call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
                targetCollege = index_to_college(tCollege)
                if (ierr/=0 .or. targetCollege<=0 .or. ch==SPACE) then
                    write(device,AFORMAT) '<br>'//red//'College "'//tCollege// &
                    '" or starting letter "'//ch//'" for name not found.'//black//'<hr><br>'
                    targetCollege  = NumColleges ! trigger 'All colleges' link for Admin
                    return
                else
                    do tdx=1,NumStudents
                        std = StdRank(tdx)
                        if (Curriculum(Student(std)%CurriculumIdx)%CollegeIdx /= targetCollege) cycle
                        if (Student(std)%Name(1:1) /= ch) cycle
                        n_count = n_count+1
                        tArray(n_count) = std
                    end do
                        !if (n_count>0) then
                        !        targetCurriculum = Student(tArray(n_count))%CurriculumIdx ! trigger links to this college
                        !else
                        !        targetCollege  = NumColleges ! trigger 'All colleges' link for Admin
                        !end if
                end if
                !write(device,AFORMAT) '<b>"'//ch//'" students in '//tCollege//'</b><br><hr>'
                header = '"'//ch//'" students in '//tCollege

        !           case (fnStudentsByYear)
        !                   ! which college ?
        !                   call cgi_get_named_string(QUERY_STRING, 'A2', tYear, ierr)
        !                   call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        !                   targetCollege = index_to_college(tCollege)
        !                   if (ierr/=0 .or. targetCollege<=0 .or. tYear==SPACE) then
        !                           write(device,AFORMAT) '<br>'//red//'College "'//tCollege// &
        !                             '" or student number year "'//tYear//'" not found.'//black//'<hr><br>'
        !                           targetCollege  = NumColleges ! trigger 'All colleges' link for Admin
        !                           return
        !                   else
        !                           do tdx=1,NumStudents
        !                              std = StdRank(tdx)
        !                              if (Curriculum(Student(std)%CurriculumIdx)%CollegeIdx /= targetCollege) cycle
        !                              if (Student(std)%StdNo(1:StdNoYearLen) /= tYear) cycle
        !                              n_count = n_count+1
        !                              tArray(n_count) = std
        !                           end do
        !                           !if (n_count>0) then
        !                           !        targetCurriculum = Student(tArray(n_count))%CurriculumIdx ! trigger links to this college
        !                           !else
        !                           !        targetCollege  = NumColleges ! trigger 'All colleges' link for Admin
        !                           !end if
        !                   end if
        !                   !write(device,AFORMAT) '<b>"'//tYear//'" students in '//tCollege//'</b><br><hr>'
        !                   header = '"'//tYear//'" students in '//tCollege
        end select


        call html_write_header(device, header)

        if (n_count == 0) then
            write(device,AFORMAT) '(None?)'
        else
            write(device,AFORMAT) '<table border="0" width="100%">'
            do tdx=1,n_count
                std = tArray(tdx)
                ldx = Student(std)%CurriculumIdx
                write(device,AFORMAT) begintr//begintd//trim(itoa(tdx))//'.'//endtd//begintd//Student(std)%StdNo//endtd, &
                begintd//trim(Student(std)%Name)//endtd//begintd//Curriculum(ldx)%Code//endtd//begintd

                if (available(fnChangeMatriculation) ) then
                    write(device,AFORMAT) trim(cgi_make_href(fnChangeMatriculation, targetUser, 'schedule', &
                    A1=Student(std)%StdNo, &
                    pre=' [ ', post=' ]'))
                end if
                if (available(fnEditCheckList) ) then
                    write(device,AFORMAT) trim(cgi_make_href(fnEditCheckList, targetUser, 'checklist', &
                    A1=Student(std)%StdNo, &
                    pre=' [ ', post=' ]'))
                end if
                if (available(fnStudentPerformance) ) then
                    write(device,AFORMAT) trim(cgi_make_href(fnStudentPerformance, targetUser, 'performance', &
                    A1=Student(std)%StdNo, &
                    pre=' [ ', post=' ]'))
                end if

                write(device,AFORMAT) endtd//endtr
            end do
            write(device,AFORMAT) '</table>'
        end if
        write(device,AFORMAT) '<hr>'

        return
    end subroutine links_to_students


    subroutine list_students(device, n_count, tArray, crse, Preenlisted)
        integer, intent (in) :: device, n_count, tArray(n_Count), crse
        type (TYPE_PRE_ENLISTMENT), intent(in) :: Preenlisted(0:)
        integer :: ldx, tdx, std, ncol
        character (len=MAX_LEN_SUBJECT_CODE) :: tNum

        if (n_count == 0) then
            write(device,AFORMAT) '<br>None?<br>'
        else
            write(device,AFORMAT) '<table border="0" width="100%">', &
            begintr//thalignleft//'Count'//endth// &
            beginth//'Contrib'//endth// &
            thalignleft//'STD NO'//endth// &
            thalignleft//'NAME'//endth// &
            thalignleft//'CURRICULUM'//endth// &
            thalignleft//'LINKS'//endth// &
            endtr
            do tdx=1,n_count
                std = tArray(tdx)
                ldx = Student(std)%CurriculumIdx
                ! find contribution
                do ncol=1,Preenlisted(std)%NPriority+Preenlisted(std)%NAlternates+Preenlisted(std)%NCurrent
                    if (crse==Preenlisted(std)%Subject(ncol)) exit
                end do
                write(tNum, '(f6.4)') Preenlisted(std)%Contrib(ncol)

                write(device,AFORMAT) begintr// &
                tdaligncenter//trim(itoa(tdx))//'.'//endtd// &
                begintd//trim(tNum)//endtd// &
                begintd//Student(std)%StdNo//endtd// &
                begintd//trim(Student(std)%Name)//endtd// &
                begintd//trim(Curriculum(ldx)%Code)//endtd//begintd

                if (available(fnChangeMatriculation) ) then
                    write(device,AFORMAT) trim(cgi_make_href(fnChangeMatriculation, targetUser, 'schedule', &
                    A1=Student(std)%StdNo, &
                    pre=' [ ', post=' ]'))
                end if
                if (available(fnEditCheckList) ) then
                    write(device,AFORMAT) trim(cgi_make_href(fnEditCheckList, targetUser, 'checklist', &
                    A1=Student(std)%StdNo, &
                    pre=' [ ', post=' ]'))
                end if
                if (available(fnStudentPerformance) ) then
                    write(device,AFORMAT) trim(cgi_make_href(fnStudentPerformance, targetUser, 'performance', &
                    A1=Student(std)%StdNo, &
                    pre=' [ ', post=' ]'))
                end if

                write(device,AFORMAT) endtd//endtr
            end do
            write(device,AFORMAT) '</table>'
        end if

        return
    end subroutine list_students


    subroutine html_write_header(device, header, errmsg)
        integer, intent (in) :: device
        character(len=*), intent (in) :: header
        character(len=*), intent (in), optional :: errmsg
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment
        character(len=MAX_LEN_STUDENT_CODE) :: tStdNo
        integer :: cdx, fdx, gdx, nItems

        ! page title, start of body
        write(device,AFORMAT) &
            '<html><head><title>'//trim(UniversityCode)//space//PROGNAME//VERSION// &
            '</title></head><body><a name="TOP"></a>'

        write(device,AFORMAT) &
            '<form name="input" method="post" action="'//CGI_PATH//'">', &
            '<input type="hidden" name="U" value="'//trim(itoa(targetUser))//'">', &
            '<input type="hidden" name="F" value="'//trim(itoa(fnStopProgram))//'">'

        ! banner line 1: user context & shortcuts
        write(device,AFORMAT) &
            '<table border="0" width="100%" cellpadding="0" cellspacing="0">', &
            begintr//begintd//'<b>'//PROGNAME//VERSION//' for '//trim(UniversityCode)//', '// &
            trim(txtSemester(currentTerm+3))//' Semester, SY '//trim(itoa(currentYear))//dash// &
            trim(itoa(currentYear+1))//' '//trim(txtPeriod(Period))//'</b>'//endtd

        write(device,AFORMAT) &
            tdalignright//'<i>User is '//trim(USER)//'.</i><small>'

        ! primary links for Department Chair
        if (isRoleChair) then
            if (REQUEST/=fnLogin) then
                call info_department(device, Department(DeptIdxUser)%Code)
            end if

        ! primary links for Registration Adviser
        else if (isRoleSRE) then
            if (available(fnCurriculum) .and. REQUEST/=fnCurriculum) then
                write(device,AFORMAT) trim(cgi_make_href(fnCurriculumList, targetUser, 'Curriculum', &
                    A1=CurrProgCode(CurriculumIdxUser), pre=nbsp))
            end if

        end if

        ! link to other links
        tCollege = College(Department(DeptIdxUser)%CollegeIdx)%Code
        write(device,AFORMAT) trim(cgi_make_href(fnCollegeLinks, targetUser, &
            tCollege, A1=tCollege, pre=nbsp))

        ! stop program by Admin
        if (isRoleAdmin) write(device,AFORMAT) &
            '<input name="A" type="submit" value="Stop '//PROGNAME//'">'

        ! logout link for all users
        write(device,AFORMAT) &
            '<input name="A" type="submit" value="Logout">', &
            !trim(cgi_make_href(fnStopUser, targetUser, 'Logout', pre=nbsp)), &
            '</small>'//endtd//endtr

        ! line 2 for banner: higher level context for selected function
        write(device,AFORMAT) begintr//begintd//'<small>'

        ! show student options ?
        if (targetStudent>0) then
            nItems = 0 ! how many enlisted subjects
            do fdx=1,Preenlisted(targetStudent)%lenSubject
                if (Preenlisted(targetStudent)%Section(fdx)>0) nItems=nItems+1
            end do

            tStdNo = Student(targetStudent)%StdNo
            write(device,AFORMAT) '[ '//tStdNo

            if (nItems==0 .and. available(fnFindBlock) .and. REQUEST/=fnFindBlock .and. &
            (IsRoleAdmin .or. &
            (IsRoleSRE .and. CurrProgCode(Student(targetStudent)%CurriculumIdx)==CurrProgCode(CurriculumIdxUser))) ) then
                write(device,AFORMAT) trim(cgi_make_href(fnFindBlock, targetUser, 'Find block', &
                    A1=tStdNo, pre=nbsp))
            end if

            if (available(fnChangeMatriculation) .and. REQUEST/=fnChangeMatriculation) then
                write(device,AFORMAT) trim(cgi_make_href(fnChangeMatriculation, targetUser, 'Schedule', &
                    A1=tStdNo, pre=nbsp))
            end if

            if (available(fnEditCheckList) .and. REQUEST/=fnEditCheckList) then
                write(device,AFORMAT) trim(cgi_make_href(fnEditCheckList, targetUser, 'Checklist', &
                    A1=tStdNo, pre=nbsp))
            end if

            if (available(fnStudentPerformance) .and. REQUEST/=fnStudentPerformance) then
                write(device,AFORMAT) trim(cgi_make_href(fnStudentPerformance, targetUser, 'Performance', &
                A1=tStdNo, pre=nbsp))
            end if

            write(device,AFORMAT) ' ]'

        end if

        ! students of curriculum ?
        if (targetCurriculum>0) then
            if (REQUEST/=fnCollegeLinks .and. REQUEST/=fnStudentsByProgram .and. available(fnStudentsByProgram)) then
                write(device,AFORMAT) trim(cgi_make_href(fnStudentsByProgram, targetUser, 'students', &
                    A1=CurrProgCode(targetCurriculum), &
                    pre=' [ '//trim(CurrProgCode(targetCurriculum))//SPACE, post=' ] '))
            end if
        end if

        ! show department options ?
        if (targetDepartment>0 .and. DeptIdxUser/=targetDepartment .and. REQUEST/=fnLogin) then
            tDepartment = Department(targetDepartment)%Code
            write(device,AFORMAT) ' [ '//tDepartment
            call info_department(device, tDepartment)
            write(device,AFORMAT) ' ] '
        end if

        ! college links ?
        if (REQUEST/=fnLogin .and. REQUEST/=fnCollegeLinks) then

            if (targetDepartment>0) then
                cdx = Department(targetDepartment)%CollegeIdx
            elseif (targetCurriculum>0) then
                cdx = Curriculum(targetCurriculum)%CollegeIdx
            else
                cdx = CollegeIdxUSER
            end if
            tCollege =College(cdx)%Code
            write(device,AFORMAT) trim(cgi_make_href(fnCollegeLinks, targetUser, 'links', &
                A1=tCollege, pre=' [ '//tCollege, post=' ] '))
        end if

        write(device,AFORMAT) '<small>'//endtd//tdalignright//'<small>'

        write(device,AFORMAT) trim(UniversityCode)//' colleges:'
        do gdx = 1,NumColleges
            if (.not. College(gdx)%hasInfo) cycle
            ! my college links already printed above?
            if (gdx==CollegeIdxUser) cycle
            ! fnCollegeLinks requested already printed above?
            if (REQUEST==fnCollegeLinks .and. gdx==targetCollege) cycle
            write(device,AFORMAT) trim(cgi_make_href(fnCollegeLinks, targetUser, College(gdx)%Code, &
                A1=College(gdx)%Code, pre=nbsp))
        end do
        write(device,AFORMAT) '</small>'//endtd//endtr

        ! line 3 of banner, if any
        if (present(errmsg)) then
            if (errmsg/=SPACE) write(device,AFORMAT) begintr//'<td colspan="2">'// &
                red//'<i>'//trim(errmsg)//'</i>'//black//endtd//endtr
        end if
        write(device,AFORMAT) '</table></form><hr>'

        ! start of body
        if (len_trim(header)>0) write(device,AFORMAT) '<b>'//trim(header)//nbsp//nbsp// &
            trim(TermQualifier)//'</b><br>'

        return
    end subroutine html_write_header


    subroutine html_write_footer(device)

        integer, intent(in) :: device

        if (REQUEST/=fnPrintableWorkload+fnOFFSET .and. REQUEST/=fnPrintableSchedule) then
            ! last piece of info on the page
            call date_and_time (date=currentDate,time=currentTime)
            write(device,AFORMAT) &
                '<small><i>Generated '//currentDate(1:4)//fslash//currentDate(5:6)//fslash//currentDate(7:8)// &
                dash//currentTime(1:2)//':'//currentTime(3:4)//'.'// &
                nbsp//nbsp//' Please report errors to '//trim(UniversityCode)//space//trim(REGISTRAR)//'.'
            if (noWrites) then ! training mode
                write(device,AFORMAT) nbsp//nbsp//'<b>'//red//PROGNAME// &
                    ' is in training mode. Any changes made will be lost after the program exits.'//black//'</b>'
            end if
            write(device,AFORMAT) &
                nbsp//nbsp//' <a target="0" href="http://code.google.com/p/heeds/">Help</a>.', &
                '</i></small>'
        end if
        write(device,AFORMAT) '</body></html>'

        close(device)

        return
    end subroutine html_write_footer


    subroutine html_college_links(device, given, mesg)

        integer, intent(in) :: device
        integer, intent(in), optional :: given
        character(len=*), intent(in), optional :: mesg

        integer :: cdx
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege

        if (present(given)) then
            targetCOLLEGE = given
            cdx = 0
        else
            call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, cdx)
            targetCOLLEGE = index_to_college(tCollege)
        end if

        if (cdx/=0 .or. targetCOLLEGE==0) then
            targetDEPARTMENT = DeptIdxUSER
            targetCOLLEGE = CollegeIdxUSER
            call html_write_header(device, 'Links to college information', '<hr><br>College"'//tCollege//'" not found')
            return
        end if

!        if (targetCOLLEGE/=CollegeIdxUSER) then
!
        call html_write_header(device, College(targetCOLLEGE)%Code//'- '//College(targetCOLLEGE)%Name, mesg)
        call html_college_info(device, targetCOLLEGE)
!
!        else
!
!            call html_write_header(device, space, mesg)
!
!        end if

        return
    end subroutine html_college_links


    subroutine info_department(device, tDepartment)
        integer, intent(in) :: device
        character(len=MAX_LEN_DEPARTMENT_CODE), intent (in) :: tDepartment


        if (REQUEST/=fnSubjectList .and. available(fnSubjectList)) then
            write(device,AFORMAT) trim(cgi_make_href(fnSubjectList, targetUser, 'Subjects', &
            A1=tDepartment, pre=nbsp))
        end if

        if (REQUEST/=fnOFFSET+fnTeachersByDept .and. available(fnOFFSET+fnTeachersByDept)) then
            write(device,AFORMAT) trim(cgi_make_href(fnOFFSET+fnTeachersByDept, targetUser, 'Teachers', &
            A1=tDepartment, pre=nbsp))
        end if

        if (REQUEST/=fnOFFSET+fnRoomList .and. available(fnOFFSET+fnRoomList)) then
            write(device,AFORMAT) trim(cgi_make_href(fnOFFSET+fnRoomList, targetUser, 'Rooms', &
            A1=tDepartment, pre=nbsp))
        end if

        if (REQUEST/=fnOFFSET+fnScheduleOfClasses .and. available(fnOFFSET+fnScheduleOfClasses)) then
            write(device,AFORMAT) trim(cgi_make_href(fnOFFSET+fnScheduleOfClasses, targetUser, 'Classes', &
            A1=tDepartment, pre=nbsp))
        end if

        if (REQUEST/=fnDemandForSubjects .and. available(fnDemandForSubjects)) then
            write(device,AFORMAT) trim(cgi_make_href(fnDemandForSubjects, targetUser, 'Demand', &
            A1=tDepartment, pre=nbsp))
        end if

        return
    end subroutine info_department


    subroutine html_college_info(device, coll)
        integer, intent(in) :: device
        integer, intent(in) :: coll
        integer :: ldx, cdx, n_count, std, tLen
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        !character (len=4) :: tYear
        character (len=1) :: ch

        tCollege = College(coll)%Code

        ! subject areas in college
        tLen = 0
#if defined UPLB
        do cdx=1,NumSubjectAreas
            if (SubjectArea(cdx)%CollegeIdx==coll) then
                tLen = tLen+1
                tArray(tLen) = cdx
            end if
        end do
#else
        ! Subjects administered by program
        do cdx=1,NumSubjectAreas
            do ldx=1,NumCurricula
                if (Curriculum(ldx)%CollegeIdx/=coll) cycle
                if (is_used_in_curriculum_subject_area(Curriculum(ldx), trim(SubjectArea(cdx)%Code)//SPACE)) then
                    tLen = tLen+1
                    tArray(tLen) = cdx
                    exit
                end if
            end do
        end do
#endif

        ! start of body
        write(device,AFORMAT) '<ul>'

        if (isRoleAdmin .and. coll==CollegeIdxUSER) then

            write(device,AFORMAT) '<li><b>Logout a user</b>', &
                trim(cgi_make_href(fnCollegeLinks, targetUSER, &
                'refresh', A1=College(CollegeIdxUSER)%Code, pre=' ( ', post=' list )')), &
                '<table border="0" width="100%">', &
                begintr//thalignleft//'User code'//endtd// &
                thalignleft//'Last action: Time-IPaddress?QUERY_STRING'//endtd//endtr
            ldx = 0
            do cdx=1,NumUsers
                if (UserList(cdx)%Session==-1) cycle
                ldx = ldx+1
                write(device,AFORMAT) trim(cgi_make_href(fnStopUSERbyAdmin, targetUSER, UserList(cdx)%Code, &
                    A1=UserList(cdx)%Code, &
                    pre=begintr//begintd//trim(itoa(ldx))//'.'//nbsp, post=endtd)), &
                    begintd//trim(UserList(cdx)%lastQUERY)//endtd//endtr
            end do
            write(device,AFORMAT) '</table></li>'

            if (noWrites) then ! training mode
                write(device,AFORMAT) trim(cgi_make_href(fnToggleTrainingMode, targetUSER, 'Turn it OFF', &
                    pre='<li><b>Training mode is '//red//'ON'//black//'</b>. ', post='</li>'))
            else
                write(device,AFORMAT) trim(cgi_make_href(fnToggleTrainingMode, targetUSER, 'Turn it ON', &
                    pre='<li><b>Training mode is '//green//'OFF'//black//'</b>. ', post='</li>'))
            end if

        end if

        if (coll==NumColleges) then

            if (available(fnStudentsDistribution)) then
                write(device,AFORMAT) trim(cgi_make_href(fnStudentsDistribution, targetUser, 'Distribution', &
                    A1=ADMINISTRATION, pre='<li><b>', post=' of students in '//trim(UniversityCode)//'</b>'))
                write(device,AFORMAT) trim(cgi_make_href(fnStudentAddPrompt, targetUser, 'Add', &
                    A1=tCollege, pre=' (', post=' a student)</li>'))
            end if

            if (NumEnlistmentRecords>0) then

                write(device,AFORMAT) '<li><b>Summary of overall enlistment</b><ul>'
                if (available(fnBottleneck)) then
                    write(device,AFORMAT) trim(cgi_make_href(fnBottleneck, targetUser, 'demand > available seats', &
                        A1=tCollege, pre='<li>Top 100 subjects for which ', post='</li>'))
                end if

                if (available(fnExtraSlots)) then
                    write(device,AFORMAT) trim(cgi_make_href(fnExtraSlots, targetUser, 'available seats > demand', &
                        A1=tCollege, pre='<li>Top 100 subjects for which ', post='</li>'))
                end if

                if (available(fnUnderloadSummary)) then
                    write(device,AFORMAT) trim(cgi_make_href(fnUnderloadSummary, targetUser, 'underloads', &
                        A1=tCollege, pre='<li>Summary of ', post='</li>'))
                end if

                write(device,AFORMAT) '</ul></li>'
            end if

        end if

        n_count = 0 ! any students?
        do std=1,NumStudents
            if (Curriculum(Student(std)%CurriculumIdx)%CollegeIdx /= coll) cycle
            n_count = n_count+1
            exit
        end do

        if (n_count>0 .and. &
            (available(fnStudentsByName) .or. available(fnStudentsByYear) .or. available(fnStudentsByProgram))) then
            write(device,AFORMAT) '<li><b>Students in '//trim(tCollege)//'</b><ul>'
            if (available(fnStudentsByName)) then
                write(device,AFORMAT) '<li><b>By last name</b> : '
                do cdx=iachar('A'), iachar('Z')
                    ch = achar(cdx)
                    n_count = 0
                    do std=1,NumStudents
                        if (Curriculum(Student(std)%CurriculumIdx)%CollegeIdx /= coll) cycle
                        if (Student(std)%Name(1:1) /= ch) cycle
                        n_count = n_count+1
                      !exit
                    end do
                    if (n_count > 0) then
                        write(device,AFORMAT) trim(cgi_make_href(fnStudentsByName, targetUser, ch, &
                            A1=tCollege, A2=ch, &
                            post='('//trim(itoa(n_count))//')'//nbsp))
                    !else
                    !  write(device,AFORMAT) ch//nbsp
                    end if
                end do
                write(device,AFORMAT) '</li>'
            end if

            !      if (available(fnStudentsByYear)) then
            !        write(device,AFORMAT) '<li><b>By number</b> : '
            !        do ldx=1960,currentYear+1
            !          tYear = itoa(ldx)
            !          n_count = 0
            !          do std=1,NumStudents
            !            if (Curriculum(Student(std)%CurriculumIdx)%CollegeIdx /= coll) cycle
            !            if (tYear == Student(std)%StdNo(1:StdNoYearLen)) then
            !              n_count = n_count+1
            !              !exit
            !            end if
            !          end do
            !          if (n_count == 0) cycle
            !          write(device,AFORMAT) trim(cgi_make_href(fnStudentsByYear, targetUser, tYear, &
            !              A1=tCollege, A2=tYear, &
            !              post='('//trim(itoa(n_count))//')'//nbsp))
            !        end do
            !        write(device,AFORMAT) '</li>'
            !      end if

            if (available(fnStudentsByProgram)) then
                write(device,AFORMAT) '<li><b>By curriculum</b> : '
                done = .false.
                do cdx=1,NumCurricula
                    if (Curriculum(cdx)%CollegeIdx /= coll) cycle
                    if (done(cdx)) cycle
                    n_count = 0
                    do std=1,NumStudents
                        if (CurrProgCode(Student(std)%CurriculumIdx) /= CurrProgCode(cdx)) cycle
                        n_count = n_count+1
                      !exit
                    end do
                    if (n_count > 0) then
                        write(device,AFORMAT) trim(cgi_make_href(fnStudentsByProgram, targetUser, CurrProgCode(cdx), &
                            A1=CurrProgCode(cdx), &
                            post='('//trim(itoa(n_count))//')'//nbsp))
                        do ldx=cdx+1,NumCurricula
                            if (CurrProgCode(ldx) == CurrProgCode(cdx)) done(ldx) = .true.
                        end do
                    end if
                end do
                write(device,AFORMAT) trim(cgi_make_href(fnStudentsDistribution, targetUser, 'Distribution among curricula', &
                    A1=tCollege))
                write(device,AFORMAT) '</li>'
            end if
            write(device,AFORMAT) '</ul><hr></li>'
        end if

        ! any curricular programs
        n_count = 0
        do cdx=1,NumCurricula
            if (Curriculum(cdx)%CollegeIdx /= coll) cycle
            n_count = n_count+1
            exit
        end do
        if (available(fnCurriculumList)) then
            if (n_count>0) then
                write(device,AFORMAT) '<li><b>Curricular programs</b> : '//nbsp
                done = .false.
                do cdx=1,NumCurricula
                    if (Curriculum(cdx)%CollegeIdx /= coll) cycle
                    if (done(cdx)) cycle
                    write(device,AFORMAT) trim(cgi_make_href(fnCurriculumList, targetUser, CurrProgCode(cdx), &
                        A1=CurrProgCode(cdx), post=nbsp))
                    do ldx=cdx+1,NumCurricula
                        if (CurrProgCode(ldx) == CurrProgCode(cdx)) done(ldx) = .true.
                    end do
                end do
                write(device,AFORMAT) '</li><hr>'
            end if
        end if

        ! subjects
        call links_to_subjects(device, coll, tLen, tArray(1))
     
        ! CURRENT SEMESTER
        write(device,AFORMAT) &
            '<li><b>'//trim(txtSemester(currentTerm+3)//' Semester, SY '//trim(itoa(currentYear))//dash// &
            trim(itoa(currentYear+1))//' '//txtPeriod(Period))//'</b><ul>'

        ! blocks
        if (n_count>0) call links_to_blocks(device, 0, coll)

        ! classes
        call links_to_sections(device, 0, coll, tLen, tArray(1))

        ! enlistment summary
        if (tLen>0) call links_to_depts(device, coll, fnEnlistmentSummary, '<b>Summary of enlistment</b>')

        ! teachers
        call links_to_teachers(device, 0, coll)

        ! rooms
        call links_to_rooms(device, 0, coll)

        write(device,AFORMAT) '</ul></li>'

        if (Period>1) then
            ! NEXT SEMESTER blocks
            write(device,AFORMAT) '<hr>', &
            '<li><b>'//green//trim(txtSemester(targetTerm+3))//' Semester, SY '// &
            trim(itoa(targetYear))//dash//itoa(targetYear+1)//black//'</b><ul>'

            if (available(fnDemandFreshmen) .and. tLen>0 ) then
                write(device,AFORMAT) trim(cgi_make_href(fnDemandFreshmen, targetUser, 'new freshmen', &
                    A1=tCollege, pre='<li><b>Demand for subjects by ', post='</b></li>'))
            end if

            ! demand for subjects
            if (tLen>0) call links_to_depts(device, coll, fnDemandForSubjects, '<b>Demand for subjects</b>')

            ! blocks
            if (n_count>0) call links_to_blocks(device, fnNextOFFSET, coll)

            ! classes
            call links_to_sections(device, fnNextOFFSET, coll, tLen, tArray(1))

            ! teachers
            call links_to_teachers(device, fnNextOFFSET, coll)

            ! rooms
            call links_to_rooms(device, fnNextOFFSET, coll)

            write(device,AFORMAT) '</ul></li>'
        end if
        write(device,AFORMAT) '</ul><hr>'
        return
    end subroutine html_college_info


    subroutine links_to_depts(device, coll, fn, heading)
        integer, intent (in) :: device, coll, fn
        character(len=*), intent (in) :: heading
        integer :: dept, crse, n_count
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        if (.not. available(fn)) return

        write(device,AFORMAT) '<li>'//heading//' : '
        do dept=2,NumDepartments
            if (Department(dept)%CollegeIdx/=coll) cycle
            n_count = 0
#if defined UPLB
            do crse=1,NumSubjects+NumAdditionalSubjects
                if (Subject(crse)%DeptIdx /= dept) cycle
                n_count = n_count+1
                exit
            end do
#else
            ! Subjects administered by program
            do crse=1,NumSubjects+NumAdditionalSubjects
                if (.not. is_used_in_college_subject(coll, crse)) cycle
                n_count = n_count+1
                exit
            end do
#endif
            if (n_count==0) cycle
            tDepartment = Department(dept)%Code
            write(device,AFORMAT) trim(cgi_make_href(fn, targetUser, tDepartment, A1=tDepartment, post=nbsp))
        end do
        write(device,AFORMAT) '</li>'

        return
    end subroutine links_to_depts


    subroutine links_to_subjects(device, coll, numAreas, AreaList)
        integer, intent (in) :: device, coll, numAreas
        integer, intent (in) :: AreaList(1:numAreas)
        integer :: dept, crse, n_count
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        if (.not. available(fnSubjectList) .or. numAreas==0) return

        write(device,AFORMAT) '<li><b>Subjects</b> in : '
        do dept=2,NumDepartments
            if (Department(dept)%CollegeIdx /= coll) cycle
            n_count = 0
#if defined UPLB
            do crse=1,NumSubjects+NumAdditionalSubjects
                if (Subject(crse)%DeptIdx /= dept) cycle
                n_count = n_count+1
                exit
            end do
#else
            ! Subjects administered by program
            do crse=1,NumSubjects+NumAdditionalSubjects
                if (.not. is_used_in_college_subject(coll, crse)) cycle
                n_count = n_count+1
                exit
            end do
#endif
            if (n_count==0) cycle
            tDepartment = Department(dept)%Code
            write(device,AFORMAT) trim(cgi_make_href(fnSubjectList, targetUser, tDepartment, A1=tDepartment, post=nbsp))
        end do
        write(device,AFORMAT) '</li><li><b>Subjects by area</b> : '
        do dept=1,numAreas
            tSubject = SubjectArea(AreaList(dept))%Code
            write(device,AFORMAT) trim(cgi_make_href(fnSubjectList, targetUser, tSubject, A1=tSubject, post=nbsp))
        end do
        write(device,AFORMAT) '</li><hr>'
        return
    end subroutine links_to_subjects


    subroutine links_to_sections(device, OFFSET, coll, numAreas, AreaList)
        integer, intent (in) :: device, OFFSET, coll, numAreas
        integer, intent (in) :: AreaList(1:numAreas)
        integer :: dept, crse, n_count
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        if (.not. available(OFFSET+fnScheduleOfClasses) .or. numAreas==0) return
        tCollege = College(coll)%Code

        write(device,AFORMAT) '<li><b>Class schedules</b> in : '
        do dept=2,NumDepartments
            if (Department(dept)%CollegeIdx /= coll) cycle
            n_count = 0
#if defined UPLB
            do crse=1,NumSubjects+NumAdditionalSubjects
                if (Subject(crse)%DeptIdx /= dept) cycle
                n_count = n_count+1
                exit
            end do
#else
            ! Subjects administered by program
            do crse=1,NumSubjects+NumAdditionalSubjects
                if (.not. is_used_in_college_subject(coll, crse)) cycle
                n_count = n_count+1
                exit
            end do
#endif
            if (n_count==0) cycle
            tDepartment = Department(dept)%Code
            write(device,AFORMAT) trim(cgi_make_href(OFFSET+fnScheduleOfClasses, targetUser, tDepartment, &
                A1=tDepartment, post=nbsp))
        end do
        write(device,AFORMAT) '; '//nbsp//' Classes with ', &
        trim(cgi_make_href(OFFSET+fnTBATeachers, targetUser, 'TBA teachers', A1=tCollege, pre=nbsp//'<b>')), &
        trim(cgi_make_href(OFFSET+fnTBARooms, targetUser, 'TBA rooms', A1=tCollege, pre=nbsp, post='</b></li>'))
        write(device,AFORMAT) '<li><b>Class schedules by subject area</b> : '
        do dept=1,numAreas
            tSubject = SubjectArea(AreaList(dept))%Code
            write(device,AFORMAT) trim(cgi_make_href(OFFSET+fnScheduleByArea, targetUser, tSubject, &
            A1=tCollege, A2=tSubject, post=nbsp))
        end do
        write(device,AFORMAT) '</li>'
        return
    end subroutine links_to_sections


    subroutine links_to_blocks(device, OFFSET, coll)
        integer, intent (in) :: device, OFFSET, coll
        integer :: blk
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege

        if (.not. (available(OFFSET+fnBlockSchedule) .or. available(OFFSET+fnBlockNewSelect)) ) return
        tCollege = College(coll)%Code

        write(device,AFORMAT) '<li><b>Block schedules</b> : '
        if (OFFSET>0) then
            do blk=1,NumNextBlocks
                if (Curriculum(NextBlock(blk)%CurriculumIdx)%CollegeIdx/=coll) cycle
                write(device,AFORMAT) trim(cgi_make_href(fnNextBlockSchedule, targetUser, NextBlock(blk)%BlockID, &
                A1=NextBlock(blk)%BlockID, post=nbsp))
            end do
        else
            do blk=1,NumCurrentBlocks
                if (Curriculum(CurrentBlock(blk)%CurriculumIdx)%CollegeIdx/=coll) cycle
                write(device,AFORMAT) trim(cgi_make_href(fnBlockSchedule, targetUser, CurrentBlock(blk)%BlockID, &
                A1=CurrentBlock(blk)%BlockID, post=nbsp))
            end do
        end if
        if (isRoleChair) tCollege = College(CollegeIdxUser)%Code
        if (isRoleChair .or. isRoleAdmin) then
            write(device,AFORMAT) trim(cgi_make_href(OFFSET+fnBlockNewSelect, targetUser, 'Add', &
            A1=tCollege, pre=nbsp//' <b>(', post=' block)</b>'))
        end if
        write(device,AFORMAT) '</li>'
        return
    end subroutine links_to_blocks


    subroutine links_to_teachers(device, OFFSET, coll)
        integer, intent (in) :: device, OFFSET, coll
        integer :: tdx, dept, n_count
        character :: ch
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        if (.not. available(OFFSET+fnTeachersByName)) return
        tCollege = College(coll)%Code

        n_count = 0
        do tdx=1,NumTeachers+NumAdditionalTeachers
            if (Department(Teacher(tdx)%DeptIdx)%CollegeIdx /= coll) cycle
            n_count = n_count+1
            exit
        end do
        if (n_count==0) return

        write(device,AFORMAT) '<li><b>Teachers by department</b> : '
        do dept=2,NumDepartments
            if (Department(dept)%CollegeIdx /= coll) cycle
            n_count = 0
            do tdx=1,NumTeachers+NumAdditionalTeachers
                if (Teacher(tdx)%DeptIdx /= dept) cycle
                n_count = n_count+1
            end do
            if (n_count==0) cycle
            tDepartment = Department(dept)%Code
            write(device,AFORMAT) trim(cgi_make_href(OFFSET+fnTeachersByDept, targetUser, tDepartment, &
            A1=tDepartment, pre=nbsp, post='('//trim(itoa(n_count))//')'))
        end do
        if (available(OFFSET+fnTeacherConflicts)) then
            write(device,AFORMAT) trim(cgi_make_href(OFFSET+fnTeacherConflicts, targetUser, 'conflicts', &
            A1=tCollege, pre='; Schedule'//nbsp//'<b>', post='</b>'))
        end if
        write(device,AFORMAT) '</li><li><b>Teachers by last name</b> : '
        do dept=iachar('A'), iachar('Z')
            ch = achar(dept)
            n_count = 0
            do tdx=1,NumTeachers+NumAdditionalTeachers
                if (Department(Teacher(tdx)%DeptIdx)%CollegeIdx /= coll) cycle
                if (Teacher(tdx)%Name(1:1) /= ch) cycle
                n_count = n_count+1
            end do
            if (n_count==0) cycle
            write(device,AFORMAT) trim(cgi_make_href(OFFSET+fnTeachersByName, targetUser, ch, &
            A1=tCollege, A2=ch, pre=nbsp, post='('//trim(itoa(n_count))//')'))
        end do
        write(device,AFORMAT) '</li>'

        return
    end subroutine links_to_teachers


    subroutine links_to_rooms(device, OFFSET, coll)
        integer, intent (in) :: device, OFFSET, coll
        integer :: rdx, dept, n_count
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character(len=MAX_LEN_DEPARTMENT_CODE) :: tDepartment

        if (.not. available(OFFSET+fnRoomList)) return
        tCollege = College(coll)%Code

        n_count = 0
        do rdx=1,NumRooms+NumAdditionalRooms
            if (Department(Room(rdx)%DeptIdx)%CollegeIdx /= coll) cycle
            n_count = n_count+1
            exit
        end do
        if (n_count==0) return

        write(device,AFORMAT) '<li><b>Rooms by department</b> : '
        do dept=2,NumDepartments
            if (Department(dept)%CollegeIdx /= coll) cycle
            n_count = 0
            do rdx=1,NumRooms+NumAdditionalRooms
                if (Room(rdx)%DeptIdx /= dept) cycle
                n_count = n_count+1
            end do
            if (n_count==0) cycle
            tDepartment = Department(dept)%Code
            write(device,AFORMAT) trim(cgi_make_href(OFFSET+fnRoomList, targetUser, tDepartment, &
            A1=tDepartment, pre=nbsp, post='('//trim(itoa(n_count))//')'))
        end do
        if (available(OFFSET+fnRoomConflicts)) then
            write(device,AFORMAT) trim(cgi_make_href(OFFSET+fnRoomConflicts, targetUser, 'conflicts', &
            A1=tCollege, pre='; Schedule'//nbsp//'<b>', post='</b>'))
        end if
        write(device,AFORMAT) '</li>'

        return
    end subroutine links_to_rooms


    subroutine blocks_in_section(device, sect, fn, NumBlocks, Block)
        integer, intent(in) :: device, sect, fn, NumBlocks
        type (TYPE_BLOCK), dimension(0:), intent(in) :: Block
        integer :: idx, jdx
        do idx=1,NumBlocks
            do jdx=1,Block(idx)%NumClasses
                if (Block(idx)%Section(jdx)/=sect) cycle
                if (available(fn)) then
                    write(device,AFORMAT) trim(cgi_make_href(fn, targetUser, Block(idx)%BlockID, A1=Block(idx)%BlockID))
                else
                    write(device,AFORMAT) trim(Block(idx)%BlockID)
                end if
                exit
            end do
        end do
        return
    end subroutine blocks_in_section


end module HTML
