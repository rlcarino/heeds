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


    subroutine extract_student_grades()

        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
        character (len=MAX_LEN_TEXT_GRADE) :: tGrade
        type (TYPE_STUDENT) :: wrkStudent
        character (len=4) :: dirYear
        character (len=1) :: ch
        integer :: idxYear, idxTerm, idxGrd, idxCurr
        integer :: cdx, fdx, gdx, idx, std, ier, i, j
        integer, dimension(MAX_ALL_SUBJECTS,2,0:2) :: GrandTotal

        GrandTotal = 0 ! grade counter

        do idxYear = baseYear,currentYear
            dirYear = itoa(idxYear)
            do idxTerm = 0,2

                ! skip future records
                if (idxYear>currentYear .or. (idxYear==currentYear .and. idxTerm>currentTerm)) cycle

                do idxGrd = 1,1 ! 0,3
                    fileName = trim(dirRAW)//dirYEAR//DIRSEP//trim(txtSemester(idxTerm))//DIRSEP// &
                    trim(txtGradeType(idxGrd))//'.CSV'

                    open(unit=unitNo,file=fileName,status='old', iostat=ier)
                    if (ier/=0) then
                        write(*,*) 'File not found: '//trim(fileName)
                        cycle
                    end if
                    write(*,*) 'Retrieving enrolled records from '//trim(fileName)
                    ! skip first line
                    read (unitNo, AFORMAT, iostat = eof) line
                    call initialize_student(wrkStudent)

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

                            if (wrkStudent%StdNo(1:5)/='####-') then ! add to Student()
                                call update_student_info(wrkStudent, std)
                            end if

                            call initialize_student(wrkStudent)

                            tCurriculum = line(pos(6)+1:pos(7)-1)
                            idxCURR = index_to_curriculum(tCurriculum)
                            if (idxCURR==0) then
                                tCurriculum = 'OTHER'
                                idxCURR = index_to_curriculum(tCurriculum)
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
                            wrkStudent%CurriculumIdx = idxCURR
                            wrkStudent%Classification = atoi(line(pos(7)+2:pos(8)-2))
                            std = index_to_student(wrkStudent%StdNo)
                            if (std/=0) wrkStudent%Record = Student(std)%Record
                        end if

                        do idx=8,31,6
                            if (pos(idx+1)-pos(idx)==1) cycle ! empty subject
                            ! subject, grade
                            tSubject = adjustl(line(pos(idx)+1:pos(idx+1)-1))
                            cdx = index_to_subject(tSubject)
                            if (tSubject/=SPACE .and. cdx<=0) then
                                call file_io_log (trim(wrkStudent%Name)//' - "'//trim(tSubject)// &
                                '" not in catalog', QUIETLY)
                                cycle
                            end if
                            tGrade  = adjustl(line(pos(idx+2)+1:pos(idx+3)-1))
                            if (tGrade==SPACE) then
                                gdx = 0
                            else
                                gdx = index_to_grade(tGrade)
                            end if
                            fdx = wrkStudent%Record(1,0) + 1
                            wrkStudent%Record(1,0) = fdx

                            wrkStudent%Record(1,fdx) = idxGrd
                            wrkStudent%Record(2,fdx) = idxYear
                            wrkStudent%Record(3,fdx) = idxTerm
                            wrkStudent%Record(4,fdx) = cdx
                            wrkStudent%Record(5,fdx) = gdx
                            if (is_grade_numeric_pass(gdx)) then  ! grade counter
                                GrandTotal(cdx,1,idxTerm) = GrandTotal(cdx,1,idxTerm) + 1
                            else
                                GrandTotal(cdx,2,idxTerm) = GrandTotal(cdx,2,idxTerm) + 1
                            end if
                        end do

                    end do
                    close(unitNo)
                    ! last student in file
                    call update_student_info(wrkStudent, std)
                    write(*,*) NumStudents, ' students'

                end do

            end do
        end do

        call xml_write_failrates(pathToCurrent, GrandTotal)

        return
    end subroutine extract_student_grades

