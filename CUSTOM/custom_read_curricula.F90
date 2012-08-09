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


    subroutine custom_read_curricula(path, errNo)

        character(len=*), intent(in) :: path
        integer, intent(out) :: errNo

        logical :: FlagIsUp
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character (len=MAX_LEN_COLLEGE_CODE) :: tCollege
        character (len=MAX_LEN_TEXT_SEMESTER) :: strTerm
        character (len=MAX_LEN_TEXT_YEAR) :: strYear
        integer :: idxCURR, idxterm, idxCOLL
        integer :: i, j, year, term
        character (len=MAX_LEN_SUBJECT_CODE) :: token
        character (len=MAX_LEN_FILE_PATH) :: currFile
        character (len=1) :: ch

        fileName = trim(dirRAW)//trim(path)//'CURRICULA.CSV'
        open (unit=1, file=fileName, status='old', iostat=errNo)
        if (errNo/=0) return

        call file_log_message('Retrieving curricular programs from '//fileName)
        ! skip first line
        read(1, AFORMAT) line
        do
            read(1, AFORMAT, iostat=eof) line
            if (eof<0) exit
            if (line(1:1)=='#' .or. line=='   ') cycle

            !----------------------------------------------------------------------------
            !id,code,name,deptcode,fileName
            !43,"BAPA","Bachelor of Arts in Public Administration","CBAPA","BAPA.CSV"
            !1  2    3 4                                         5 6     7 8        9
            call index_to_delimiters('"', line, ndels, pos)

            ! remove non-alphabetic, non-dash
            tCurriculum = SPACE
            j = 0
            do i=pos(2)+1,pos(3)-1
                ch = line(i:i)
                if ((ch>='A' .and. ch<='Z') .or. index(DECDIGITS,ch)>0 .or. ch==dash) then
                    j = j+1
                    tCurriculum(j:j) = ch
                end if
            end do
            idxCURR = index_to_curriculum(tCurriculum)
            if (idxCURR>0) then
                call file_log_message (line, trim(tCurriculum)//' - curriculum already listed')
                cycle
            end if

            currFile = trim(dirRAW)//trim(path)//line(pos(2)+1:pos(3)-1)//'.CSV'
            inquire(file=currFile, exist=FlagIsUp)
            if (.not. FlagIsUp) then
                write(*,*) 'File not found: '//trim(currFile)
                if (ndels<8 .or. pos(9)-pos(8)<=1) cycle
                currFile =trim(dirRAW)//trim(path)//line(pos(8)+1:pos(9)-1)
                inquire(file=currFile, exist=FlagIsUp)
                if (.not. FlagIsUp) then
                    write(*,*) 'File not found: '//trim(currFile)
                    cycle
                end if
            end if

            tCollege = line(pos(6)+1:pos(7)-1)
            idxCOLL = 0
            do j=0,NumColleges
                if (tcollege==College(j)%Code) idxCOLL = j
            end do
            if (idxCOLL==0) then
                call file_log_message (line, trim(tcollege)//' - college not known')
                cycle
            end if
            College(idxCOLL)%hasInfo = .true.
            call check_array_bound (NumCurricula+1, MAX_ALL_CURRICULA, 'MAX_ALL_CURRICULA')
            NumCurricula = NumCurricula + 1
            Curriculum(NumCurricula)%Code = tCurriculum
            Curriculum(NumCurricula)%CollegeIdx = idxCOLL
            Curriculum(NumCurricula)%Title = line(pos(4)+1:pos(5)-1)
            Curriculum(NumCurricula)%Specialization = '(Edit specialization)'
            Curriculum(NumCurricula)%Remark = '(Edit remark)'
            Curriculum(NumCurricula)%NSubjects = 0

            ! read subjects
            call file_log_message ('Retrieving '//trim(currFile)//' for '//tCurriculum)
            open (unit=2, file=currFile, status='old')
            ! skip first line
            read(2, AFORMAT) line
            do
                read(2, AFORMAT, iostat=eof) line
                if (eof<0) exit
                if (line==SPACE .or. line(1:1)=='#') cycle
                call index_to_delimiters('"', line, ndels, pos)
                !no,code,name,units,year,term,optional,subject,curr,id
                !0,"FIN 30B","Basic Finance",3.0000,1,"1",0,1226,19,1120
                !0,"PE 11","Physical Fitness",2.0000,1,"1",0,639,19,1123
                !0,"ACCTG 30D","Fundamentals of Accounting, Part 1",3.0000,1,"1",0,1199,19,1121
                !  2         3 4                                  5          6 7
                strTerm = line(pos(6)+1:pos(7)-1)
                term = atoi(strTerm)
                if (term==6) then
                    term = 3
                    strTerm = 'S'
                end if

                strYear = line(pos(6)-2:pos(6)-2)
                year = atoi(strYear)
                if (year>0 .and. term>0) then ! ok

                    ! collect subjects
                    idxTerm = (year-1)*3 + term
                    Curriculum(NumCurricula)%NumTerms = max(idxTerm, Curriculum(NumCurricula)%NumTerms)

                    token = line(pos(2)+1:pos(3)-1)
                    ! convert to uppercase
                    call upper_case(token)
                    i = index_to_subject (token)
                    if (i/=0) then
                        j = Curriculum(NumCurricula)%NSubjects+1
                        Curriculum(NumCurricula)%NSubjects = j
                        Curriculum(NumCurricula)%SubjectIdx(j) = i
                        Curriculum(NumCurricula)%SubjectTerm(j) = idxTerm
                        if (is_offered(i,mod(term,3)) ) then
                            if (.not. is_prerequisite_satisfiable_in_curriculum(i,NumCurricula)) then
                                ! errNo = 126 ! subject prerequisite cannot be satisfied in this curriculum
                                call file_log_message (trim(Curriculum(NumCurricula)%Code)//', '// &
                                trim(strYear)//' year, '//trim(strTerm)//' term, '//trim(token)// &
                                ': preq '//trim(text_prerequisite_in_curriculum(i))//' not specified earlier!')
                            end if
                        else
                            ! subject not offered in semester taken
                            call file_log_message (token//line, 'Subject not offered during '//strTerm//' Term')
                        end if
                    else
                        ! subject not in catalog
                        call file_log_message (line, token//' Subject not in catalog')
                    end if
                end if

            end do
            close(2)
        !----------------------------------------------------------------------------

        end do
        close(1)

        return
    end subroutine custom_read_curricula

