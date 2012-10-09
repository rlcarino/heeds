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


subroutine custom_read_students (path, numEntries, ier)
    character (len=*), intent (in) :: path ! YEAR/TERM/
    integer, intent (out) :: numEntries, ier

    call SIAS_read_students_from_enlistment(trim(path)//'FINALGRADE', numEntries, ier)
    if (numEntries==0) call SIAS_read_students (trim(path)//'STUDENTS', numEntries, ier)
    ier = 0

    return
end subroutine custom_read_students


subroutine SIAS_read_students (filePath, numEntries, ier)
    character (len=*), intent (in) :: filePath ! YEAR/TERM/STUDENTS
    integer, intent (out) :: numEntries, ier

    type (TYPE_STUDENT) :: wrkStudent
    character (len=MAX_LEN_CURRICULUM_CODE) :: StdCurriculum
    character :: ch
    integer :: idxCURR, i, j, indexLoc

    numEntries = 0
    fileName = trim(dirRAW)//trim(filePath)//'.CSV'
    open(unit=unitNo, file=fileName, status='old', iostat=ier)
    if (ier/=0) return

    call file_log_message ('Retrieving list of students from '//fileName)
    ! skip first line
    read(unitNo,AFORMAT,iostat=eof) line
    student_loop : &
    do
        read(unitNo,AFORMAT,iostat=eof) line
        if (eof<0) exit student_loop
        if (line(1:1)=='#' .or. line(1:3)=='   ') cycle student_loop
        call index_to_delimiters('"', line, ndels, pos)
        !no,code,name,subject,year,address,age,sexmale,sexfemale,civilstatus,scholarship,units,new,old,transferee,info1,info2,info3,info4
        !1 2   3 4  5 6    7      8     9     10   11 12     13

        if (ndels<13) then
            call file_log_message (line,'There must be at least 13 data items')
            cycle student_loop !         return
        end if

        call initialize_student(wrkStudent)

        StdCurriculum = line(pos(6)+1:pos(7)-1)
        idxCURR = index_to_curriculum(StdCurriculum)
        if (idxCURR==0) then
            StdCurriculum = 'OTHER'
            idxCURR = index_to_curriculum(StdCurriculum)
            !call file_log_message (trim(line), 'Curriculum "'//trim(StdCurriculum)//'" not in catalog')
            !cycle student_loop
        else if (idxCURR<=0) then
            idxCURR = -idxCURR
            !write (*,*) line(:pos(13)-1)//' : using curriculum '// Curriculum(idxCURR)%Code
        end if
        wrkStudent%CurriculumIdx = idxCURR
        ! remove punctuation from name
        wrkStudent%Name = SPACE
        j = 0
        do i=pos(4)+1,pos(5)-1
            ch = line(i:i)
            if (ch==COMMA .or. index(SPECIAL,ch)>0) cycle
            j = j+1
            wrkStudent%Name(j:j) = ch
        end do
        call upper_case(wrkStudent%Name)

        wrkStudent%StdNo = line(pos(2)+1:pos(3)-1)
        if (line(pos(10)+1:pos(11)-1)/=SPACE) then
            wrkStudent%Gender = line(pos(10)+1:pos(11)-1)
        else
            wrkStudent%Gender = line(pos(12)+1:pos(13)-1)
        end if
        wrkStudent%CountryIdx = 1
        wrkStudent%Classification = atoi(line(pos(7)+2:pos(8)-2))

        call update_student_info(wrkStudent, indexLoc)
        if (indexLoc<0) numEntries = numEntries + 1

    end do student_loop
    close (unitNo)
    call file_log_message (itoa(numEntries)//' students in '//fileName)

    return
end subroutine SIAS_read_students


subroutine SIAS_read_students_from_enlistment (filePath, numEntries, ier)
    character (len=*), intent (in) :: filePath
    integer, intent (out) :: numEntries, ier

    type (TYPE_STUDENT) :: wrkStudent
    character (len=MAX_LEN_CURRICULUM_CODE) :: StdCurriculum
    character :: ch
    integer :: idxCURR, i, j, indexLoc

    numEntries = 0
    fileName = trim(dirRAW)//trim(filePath)//'.CSV'
    open(unit=unitNo, file=fileName, status='old', iostat=ier)
    if (ier/=0) return

    call file_log_message ('Retrieving list of students from '//fileName)
    ! skip first line
    read (unitNo, AFORMAT, iostat = eof) line
    do
        read (unitNo, AFORMAT, iostat = eof) line
        if (eof < 0) exit
        if (line(1:1) == '#' .or. line(1:3) == '   ') cycle
        call index_to_delimiters('"', line, ndels, pos)
        !no,code,name,crscd,year,f1,g1,u1,f2,g2,u2,f3,g3,u3,f4,g4,u4,units
        !1,"266441","Abad, Aiza  L.","BSBA-HRDM",3,"BL 30","2.25","3","HRDM 50","2.5","3","HRDM 51","1.5","3","ELECT 1 (BSBA)","2.5","3",15.00
        !1 2      3 4              5 6         7   8     9 1    1 1 1 1       1 1   1 1 1 2       2 2   2 2 2 2              2 2   2 3 3
        !                                                  0    1 2 3 4       5 6   7 8 9 0       1 2   3 4 5 6              7 8   9 0 1
        !0,"","","",0,"HRDM 52","2.0","3","","","","","","","","","",0.00
        !1 23 45 67   8       9 1   1 1 1 11 11 11 22 22 22 22 22 33
        !                       0   1 2 3 45 67 89 01 23 45 67 89 01
        if (pos(3)-pos(2)>1) then ! new student

            call initialize_student(wrkStudent)

            StdCurriculum = line(pos(6)+1:pos(7)-1)
            idxCURR = index_to_curriculum(StdCurriculum)
            if (idxCURR==0) then
                StdCurriculum = 'OTHER'
                idxCURR = index_to_curriculum(StdCurriculum)
            elseif (idxCURR<=0) then
                idxCURR = -idxCURR
              !write (*,*) line(:pos(8))//'... using curriculum '// Curriculum(idxCURR)%Code
            end if
            wrkStudent%StdNo = adjustl(line(pos(2)+1:pos(3)-1))
            wrkStudent%Name = SPACE
            ! remove punctuation
            j = 0
            do i=pos(4)+1,pos(5)-1
                ch = line(i:i)
                if (index(SPECIAL,ch)>0 .or. ch==COMMA) cycle
                j = j+1
                if (j>MAX_LEN_STUDENT_NAME) cycle
                wrkStudent%Name(j:j) = ch
            end do
            call upper_case(wrkStudent%Name)
            wrkStudent%Gender = 'X'
            wrkStudent%CountryIdx = 1
            wrkStudent%CurriculumIdx = idxCURR
            wrkStudent%Classification = atoi(line(pos(7)+2:pos(8)-2))

            call update_student_info(wrkStudent, indexLoc)
            if (indexLoc<0) numEntries = numEntries + 1

        end if
    end do
    close(unitNo)
    call file_log_message (itoa(numEntries)//' students added from '//fileName)

    return
end subroutine SIAS_read_students_from_enlistment


