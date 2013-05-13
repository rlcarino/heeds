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


subroutine custom_read_pre_enlistment(path, basename, NumSections, Section, cList, numEntries, ier)
    character(len=*), intent(in) :: path, basename
    type (TYPE_SECTION), intent(in out) :: Section(0:)
    type (TYPE_PRE_ENLISTMENT), intent(out) :: cList(0:)
    integer, intent (in) :: NumSections
    integer, intent (out) :: numEntries, ier

    character (len=MAX_LEN_STUDENT_CODE) :: tStdNo
    character (len=MAX_LEN_SUBJECT_CODE) :: tSubject
    character (len=MAX_LEN_CLASS_ID) :: tSection
    integer :: cdx, k, sdx, std

    numEntries = 0
    fileName = trim(dirDATA)//trim(path)//basename//'.CSV'
    open(unit=unitRAW, file=fileName, form='formatted', status='old', iostat=ier)
    if (ier/=0) return

    call file_log_message ('Retrieving '//fileName)

    loop_WRITEIN  : &
    do
        read (unitRAW, AFORMAT, iostat = eof) line
        if (eof<0) exit loop_WRITEIN
        if (line(1:1)=='#' .or. line(1:3)=='   ') cycle loop_WRITEIN

    !#STUDNO,SUBJECT CODE,CLASS CODE,SutdName,Course,TERM,COLLEGE,TEACHER,SECTION
    !1      2            3          4        5      6    7       8       9       10       11      12     13        14          15
    !08-09517,EM 218,G007,"Abbariao, Cristopher Adolfo",MAED-SS,13-S,GS,To Be Assigned,GS
    !08-09517,MA 204,G022,"Abbariao, Cristopher Adolfo",MAED-SS,13-S,GS,To Be Assigned,GS
    !08-09517,SOC SCI 218A,G052,"Abbariao, Cristopher Adolfo",MAED-SS,13-S,GS,To Be Assigned,GS
    !08-09517,SS 217,G053,"Abbariao, Cristopher Adolfo",MAED-SS,13-S,GS,To Be Assigned,GS
    !06-00593,BA 72,B120,"Accad, Leonard Andrew Bramaje",BSENT,13-S,CBEA,"Gonzaga, Jeremiah",BSENT-3C
    !06-00593,BA 66,B128,"Accad, Leonard Andrew Bramaje",BSENT,13-S,CBEA,"Singson, Marcial",BSENT-3C
    !08-02956,ENG 13,E028,"Adviento, Baby Jane Concepcion",BSED-FIL,13-S,CTE,"Clemente, Beatriz",BSED 1K
    !
        call index_to_delimiters(COMMA, line, ndels, pos)

        ! student
        tStdNo = line(1:pos(2)-1)
        std = index_to_student(tStdNo)
        if (std==0) then
            call file_log_message('Student not in list: '//line)
            cycle loop_WRITEIN
        end if

        ! subject
        tSubject = line(pos(2)+1:pos(3)-1)
        cdx = index_to_subject(tSubject)
        if (cdx<=0) then
            call file_log_message(tSubject//' - subject not in catalog: '//line)
            cycle loop_WRITEIN
        end if

        ! section
        tSection = adjustl(line(pos(3)+1:pos(4)-1))
        if (len_trim(tSection)==0) then ! not accommodated
            sdx = 0
        else
            tSection = trim(tSubject)//SPACE//tSection
            sdx = index_to_section(tSection, NumSections, Section)
            if (sdx==0) then
                call file_log_message (tSection//' - no such section: - '//line)
            else ! try lab section
                if (is_lecture_lab_subject(cdx)) then
                    tSection = trim(tSection)//DASH//'1L'
                    k = index_to_section(tSection, NumSections, Section)
                    if (k>0) sdx = k
                end if
            end if
        end if
        k = cList(std)%NPriority + 1
        call check_array_bound (k, MAX_SUBJECTS_PER_TERM, 'MAX_SUBJECTS_PER_TERM')

        cList(std)%Subject(k) = cdx
        cList(std)%Section(k) = sdx
        cList(std)%Grade(k) = gdxREGD
        cList(std)%NPriority = k
        cList(std)%AllowedLoad = cList(std)%AllowedLoad + Subject(cdx)%Units
        cList(std)%lenSubject = k

        numEntries = numEntries + 1

        !write(*,*) std, tStdno, cList(std)%lenSubject, tSubject, tSection

    end do loop_WRITEIN
    close(unitRAW)
    call file_log_message (itoa(numEntries)//' entries in '//fileName)

    return
end subroutine custom_read_pre_enlistment

