!======================================================================
!
!    HEEDS (Higher Education Enrollment Decision Support) - A program
!      to create enrollment scenarios for 'next term' in a university
!    Copyright (C) 2012, 2013 Ricolindo L Carino
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

subroutine custom_read_classes(fName, NumSections, Section, errNo)

    character (len=*), intent (in) :: fName
    integer, intent (in out) :: NumSections
    type (TYPE_SECTION), intent(in out), dimension (0:) :: Section
    integer, intent(out) :: errNo

    integer :: ddx

    call SIAS_read_classes(trim(fName)//'.CSV', NumSections, Section, errNo) ! try SIAS format
    if (errNo/=0) then ! try UPLB format
        call UPLB_read_classes(fName, NumSections, Section, errNo)
        if (errNo/=0) then ! try by college
            do ddx=2,NumDepartments-1
                call UPLB_read_classes(trim(fName)//DASH//Department(ddx)%Code, NumSections, Section, errNo)
            end do
        end if
    end if
    return
end subroutine custom_read_classes


subroutine SIAS_read_classes(fName, NumSections, Section, errNo)

    character (len=*), intent (in) :: fName
    integer, intent (in out) :: NumSections
    type (TYPE_SECTION), intent(in out), dimension (0:) :: Section
    integer, intent(out) :: errNo

    character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
    character (len=MAX_LEN_ROOM_CODE) :: tRoom
    character (len=MAX_LEN_SECTION_CODE) :: tBlock, tSection
    character (len=MAX_LEN_TEACHER_NAME) :: tTeacher

    character (len=1) :: ch
    integer :: kdx, rdx, tdx, i, j, subj
    integer :: btime, dayidx(6), etime, ndays, iidx, pDASH
    character (len=MAX_LEN_TEXT_TIME) :: strBTime, strETime

    open (unit=unitNum, file=fName, status='old', iostat=errNo)
    if (errNo/=0) return

    call file_log_message('Retrieving classes from '//fName)
    ! skip first line
    read(unitNum, AFORMAT) line
    do
        read(unitNum, AFORMAT, iostat=eof) line
        if (eof<0) exit
        if (line==SPACE .or. line(1:1)=='#') cycle
        call index_to_delimiters('"', line, ndels, pos)
        !days,time,units,room,classcd,sectioncd,subjcd,subjnm,instnm,inst,timein,daysdigits
        !2  3 4  5 6   7 8  9 10   11 12     13 14  15 16  17 18  19
        !"M","7-9 AM","2.0","AT1B","S030","AT-1B","PE 11","Physical Fitness","Octobre, Robert E.",56,12/30/1899 07:00:00,"2"
        !2 3 4      5 6   7 8    9 10  11 12   13 14   15 16              17 18                19
        !"M","7-9 AM","2.0","BEED1B","T016","BEED-1B","PE 11","Physical Fitness","Bumanglag, Ceejay A.",318,12/30/1899 07:00:00,"2"
        !"M","7-10 AM","1.0","BSIT1B","F045","BSIT-1B","IT 31 LAB","Programming I","Dalisay, Aimee",290,12/30/1899 07:00:00,"2"

        ! room
        tRoom = line(pos(8)+1:pos(9)-1)
        rdx = index_to_room (tRoom)
        ! teacher
        tTeacher = SPACE
        j = 0
        do i=pos(18)+1,pos(19)-1
            ch = line(i:i)
            if (ch==COMMA .or. index(SPECIAL,ch)>0) cycle
            j = j+1
            tTeacher(j:j) = ch
        end do
        if (tTeacher=='Not Available' .or. tTeacher==SPACE) then
            tdx = 0
        else
            tdx = index_to_teacher (tTeacher)
        end if
        ! begin time
        strBTime = SPACE
        j = 0
        kdx = pos(4)+1 ! position of '-'
        do i=pos(4)+1,pos(5)-1
            ch = line(i:i)
            if (ch==DASH) then
                kdx = i
                exit
            end if
            if (index(DECDIGITS,ch)>0 .or. ch==':') then
                j = j+1
                strBtIme(j:j) = ch
            end if
        end do
        ! end time
        strETime = SPACE
        j = 0
        do i=kdx,pos(5)-1
            ch = line(i:i)
            if (index(DECDIGITS,ch)>0 .or. ch==':') then
                j = j+1
                strETime(j:j) = ch
            end if
        end do
        btime = index_to_time(strBTime)
        etime = index_to_time(strETime)
        if (etime<btime) then
            etime = etime+48
        end if
        !write(*,*) strBTime, btime, strETime, etime

        ! subject
        tSubject = line(pos(14)+1:pos(15)-1)
        subj = index_to_subject(tSubject)
        if (subj<=0) then
            call file_log_message (line, 'Subject not in catalog; ignored')
            cycle
        end if
        NumSections = NumSections + 1

        ! section code
        tSection = line(pos(10)+1:pos(11)-1)

        ! block
        tBlock = line(pos(12)+1:pos(13)-1)
        if (tSection==SPACE) tSection = tBlock

        ! days
        ndays = 0
        dayidx = 0
        if (line(pos(2)+1:pos(3)-1)/='TBA') then
            pDASH = -1
            do i=pos(2)+1,pos(3)-1
                ch = line(i:i)
                iidx = 0
                if (ch=='M') then
                    iidx = 1
                else if (ch=='-') then
                    pDASH = i
                else if (ch=='T') then
                    if (line(i+1:i+1)=='h' .or. line(i+1:i+1)=='H') then
                        iidx = 4
                    else
                        iidx = 2
                    end if
                else if (ch=='W') then
                    iidx = 3
                else if (ch=='F') then
                    iidx = 5
                else if (ch=='S') then
                    iidx = 6
                end if
                if (iidx>0) then
                    ndays = ndays+1
                    dayidx(ndays) = iidx
                    if (pDASH==i-1) then
                        do j=dayidx(ndays-1)+1,iidx
                            dayidx(ndays) = j
                            ndays = ndays+1
                        end do
                        ndays = ndays-1
                    end if
                end if
            end do
        else
            ndays = 1 ! count TBA day-time as 1 meeting
        end if

        ! transfer to section
        Section(NumSections)%SubjectIdx = subj
        Section(NumSections)%DeptIdx = Subject(subj)%DeptIdx
        do i=2,NumDepartments
            if (tSection(1:1)/=Department(i)%SectionPrefix)  cycle
            Section(NumSections)%DeptIdx = i
            exit
        end do
        Section(NumSections)%Code = tSection
        Section(NumSections)%ClassId = trim(tSubject)//SPACE//tSection
        Section(NumSections)%Slots = 50
        Section(NumSections)%NMeets = ndays
        Section(NumSections)%DayIdx(1:ndays) = dayidx(1:ndays)
        Section(NumSections)%bTimeIdx(1:ndays) = btime
        Section(NumSections)%eTimeIdx(1:ndays) = etime
        Section(NumSections)%RoomIdx(1:ndays) = rdx
        Section(NumSections)%TeacherIdx(1:ndays) = tdx
        if (is_lecture_lab_subject(subj) .and. index(tSection,DASH)==0) then ! no blockname for lecture class
            Section(NumSections)%BlockID = SPACE
        else
            Section(NumSections)%BlockID = tBlock
        end if

    end do

    close(unitNum)
    !call file_log_message (itoa(NumSections)//' sections after reading '//fName)

    return
end subroutine SIAS_read_classes


subroutine UPLB_read_classes  (fName, NumSections, Section, errNo)
    character (len=*), intent (in) :: fName
    type (TYPE_SECTION), intent(in out), dimension (0:) :: Section
    integer, intent (in out) :: NumSections
    integer, intent (out) :: errNo
    integer :: eof, i, j, sect, ier
    type (TYPE_SECTION) :: wrk

    open (unit=unitNum, file=fName, status='old', iostat=errNo)
    if (errNo/=0) return

    call file_log_message('Retrieving classes from '//fName)
    do
        read (unitNum, AFORMAT, iostat=eof) line
        if (eof<0) exit
        if (line(1:1)=='#' .or. line(1:1)==SPACE) cycle
        !write(*,*) trim(line)
        call ValidateSection (NumSections, Section, line, wrk, ier)
        if (ier==0) then
            ! if '+', add to base section
            i = index(wrk%ClassId, '+')
            if (i>0) then
                wrk%ClassId(i:) = SPACE
                sect = NumSections
                do while (sect>0)
                    if (wrk%ClassId==Section(sect)%ClassId) exit
                    !write(*,*) Section(sect)%ClassId
                    sect = sect-1
                end do
                if (sect==0) then
                    ier = 140
                    call file_log_message (line, 'Base section "'//trim(wrk%ClassId)//'" not found; ignored')
                else
                    i = Section(sect)%NMeets
                    j = wrk%NMeets
                    Section(sect)%DayIdx(i+1:i+j) = wrk%dayidx(1:j)
                    Section(sect)%bTimeIdx(i+1:i+j) = wrk%bTimeIdx(1:j)
                    Section(sect)%eTimeIdx(i+1:i+j) = wrk%eTimeIdx(1:j)
                    Section(sect)%RoomIdx(i+1:i+j) = wrk%RoomIdx(1:j)
                    Section(sect)%TeacherIdx(i+1:i+j) = wrk%teacherIdx(1:j)
                    Section(sect)%NMeets = i+j
                end if
            else
                NumSections = NumSections+1
                Section(NumSections) = wrk
            end if
        end if
    end do
    close (unitNum)
    !call file_log_message (itoa(NumSections)//' sections after reading '//fName)

    return
end subroutine UPLB_read_classes


subroutine ValidateSection (NumSections, Section, line, wrk, ier)
    type (TYPE_SECTION), intent(in), dimension (0:) :: Section
    integer, intent (in) :: NumSections
    type (TYPE_SECTION), intent(out) :: wrk
    integer, intent (out) :: ier
    character (len=255), intent (in) :: line
    !     integer :: TimeTable(60,6)
    integer :: btime, dayidx(6), etime, ndays, iidx, pDASH
    integer :: i, j, sect, crse, rmidx, tidx
    integer :: ndels, pos(30)
    character (len = 1) :: ch
    character (len=5) :: strBTime, strETime
    character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
    character (len=MAX_LEN_CLASS_ID) :: tSection
    character (len=MAX_LEN_ROOM_CODE) :: tRoom
    character (len=MAX_LEN_TEACHER_CODE) :: tTeacher
    !
    ier = 0
    call initialize_section(wrk)
    call index_to_delimiters(COMMA, line, ndels, pos)
    !subject,section,slots,btime-etime,days,room,teacher
    !  1      2      3        4       5    6    7      8
    if (ndels<7) then
        ier = 140
        call file_log_message (line, 'Incorrect number of data items; ignored')
        return
    end if
    !   get subject name
    tSubject = line(:pos(2)-1)
    tSection = line(pos(2)+1:pos(3)-1)
    crse = index_to_subject(tSubject)
    if (crse<=0) then
        ier = 141
        call file_log_message (line, 'Subject not in catalog; ignored')
        return
    end if
    wrk%SubjectIdx = crse
    wrk%DeptIdx = Subject(crse)%DeptIdx
    do i=2,NumDepartments
        if (tSection(1:1)/=Department(i)%SectionPrefix)  cycle
        wrk%DeptIdx = i
        exit
    end do
    wrk%Code = tSection

    !   get class id, slots
    tSection = trim(tSubject)//SPACE//tSection
    sect = index_to_section(tSection, NumSections, Section)
    if (sect/=0) then
        ier = 143
        call file_log_message (line, 'Duplicate section; ignored')
        return
    end if
    wrk%ClassId = tSection
    ! slots
    i = atoi(line(pos(3)+1:pos(4)-1))
    !if (i==0 .and. index(tSection,'+')==0) then !not an additional section
    !  ier = 144
    !  call file_log_message (line, 'slots is zero; ignored')
    !  return
    !end if
    wrk%Slots = i
    !   begin, end times
    tSubject = line(pos(4)+1:pos(5)-1)
    j = index(tSubject, '-')
    if (j==0) then
        btime = 0
        etime = 0
    else
        strBTime = tSubject(1:j-1)
        strETime = tSubject(j+1:)
        btime = index_to_time(strBTime)
        etime = index_to_time(strETime)
        if (etime<btime) then
            etime = etime+48
        !       call file_log_message (trim(line)//' - assuming '//strETime//' is evening...')
        end if
    end if
    !   days
    ndays = 0
    dayidx = 0
    if (line(pos(5)+1:pos(6)-1)/='TBA') then
        pDASH = -1
        do i=pos(5)+1,pos(6)-1
            ch = line(i:i)
            iidx = 0
            if (ch=='M') then
                iidx = 1
            else if (ch=='-') then
                pDASH = i
            else if (ch=='T') then
                if (line(i+1:i+1)=='h' .or. line(i+1:i+1)=='H') then
                    iidx = 4
                else
                    iidx = 2
                end if
            else if (ch=='W') then
                iidx = 3
            else if (ch=='F') then
                iidx = 5
            else if (ch=='S') then
                iidx = 6
            end if
            if (iidx>0) then
                ndays = ndays+1
                if (ndays>6) then
                    write(*,*) 'Too many days: '//trim(line)
                    ndays = 1 ! force to be TBA
                    dayidx = 0
                    btime = 0
                    etime = 0
                    exit
                end if
                dayidx(ndays) = iidx
                if (pDASH==i-1) then
                    do j=dayidx(ndays-1)+1,iidx
                        dayidx(ndays) = j
                        ndays = ndays+1
                    end do
                    ndays = ndays-1
                end if
            end if
        end do
    else
        ndays = 1 ! count TBA day-time as 1 meeting
    end if
    ! room
    tRoom = line(pos(6)+1:pos(7)-1)
    if (tRoom=='TBA') then
        rmidx = 0
    else
        rmidx = index_to_room (tRoom)
        if (rmidx==0) then
            write(stderr,AFORMAT) trim(line)//' - '//trim(tRoom)//' room is not valid; using TBA'
          !call file_log_message (trim(line)//' - '//trim(tRoom)//' room is not valid; using TBA')
        end if
    end if
    ! teacher
    tTeacher = trim(line(pos(7)+1:pos(8)-1))
    call upper_case(tTeacher)
    if (tTeacher=='TBA') then
        tidx = 0
    else
        !do i=1,len_trim(tTeacher)
        !  ch = tTeacher(i:i)
        !  if ( ('a'<=ch .and. ch<='z') .or. ('A'<=ch .and. ch<='Z') .or. ('0'<=ch .and. ch<='9')) cycle
        !  tTeacher(i:i) = DASH
        !end do
        tidx = index_to_teacher (tTeacher)
        if (tidx==0) then
            write(stderr,AFORMAT) trim(line)//' - '//trim(tTeacher)//' teacher is not valid; using TBA'
          !call file_log_message (trim(line)//' - '//trim(tTeacher)//' teacher is not valid; using TBA')
        end if
    end if
    ! block
    if (ndels>7) then
        if (is_lecture_lab_subject(wrk%SubjectIdx) .and. index(wrk%Code,DASH)==0) then ! no blockname for lecture class
            wrk%BlockID = SPACE
        else
            wrk%BlockID = line(pos(8)+1:pos(9)-1)
        end if
    end if
    ! transfer to section
    wrk%NMeets = ndays
    wrk%DayIdx(1:ndays) = dayidx(1:ndays)
    wrk%bTimeIdx(1:ndays) = btime
    wrk%eTimeIdx(1:ndays) = etime
    wrk%RoomIdx(1:ndays) = rmidx
    wrk%TeacherIdx(1:ndays) = tidx
    return
end subroutine ValidateSection

