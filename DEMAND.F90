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


module DEMAND

    use HTML

    implicit none

contains


#include "custom_demand.F90"


    subroutine demand_by_new_freshmen(device, Offering)
        integer, intent (in) :: device
        type (TYPE_OFFERED_SUBJECTS), dimension (MAX_ALL_DUMMY_SUBJECTS:MAX_ALL_SUBJECTS), intent (in out) :: Offering
        integer :: nstd, idxCURR
        integer :: crse, ierr, j, k, l
        character (len=MAX_LEN_CURRICULUM_CODE) :: tCurriculum
        character(len=MAX_LEN_COLLEGE_CODE) :: tCollege
 
        ! which college ?
        call cgi_get_named_string(QUERY_STRING, 'A1', tCollege, ierr)
        targetCollege = index_to_college(tCollege)

        ! initialize counters
        do crse=MAX_ALL_DUMMY_SUBJECTS,MAX_ALL_SUBJECTS
            Offering(crse)%Demand = 0
        end do

        if (REQUEST==fnDemandFreshmen) then

            ! check
            NFintake(0) = sum(NFintake(1:))
            if (NFintake(0) == 0) then
                write(device,AFORMAT) '<br>'//red//'Total no. of new freshmen is 0 ?'//black//'<br>'
                return
            end if

        else ! extract from QUERY_STRING
            ! INTAKE
            do idxCURR=1,NumCurricula
                call cgi_get_named_integer(QUERY_STRING, trim(Curriculum(idxCURR)%Code), nstd, ierr)
                if (ierr==0) then
                    write(*,*) Curriculum(idxCURR)%Code, nstd, ierr
                    NFintake(idxCURR) = nstd
                end if
            end do
            ! edited value
            !write(*,*) trim(QUERY_STRING)
            call cgi_get_named_string(QUERY_STRING, 'curriculum', tCurriculum, ierr)
            if (ierr==0) then
                idxCURR = abs(index_to_curriculum(tCurriculum))
                !write(*,*) idxCURR, tCurriculum
                if (idxCURR/=0) then
                    !write(*,*) trim(QUERY_STRING)
                    call cgi_get_named_integer(QUERY_STRING, 'count', nstd, ierr)
                    if (ierr==0) then
                        NFintake(idxCURR) = nstd
                      !write(*,*) tCurriculum, nstd
                    end if
                end if
            end if
            ! check
            NFintake(0) = sum(NFintake(1:))
            if (NFintake(0) == 0) then
                write(device,AFORMAT) '<br>'//red//'Internal error: total no. of new freshmen is 0 ?'//black//'<br>'
                return
            end if

            ! rewrite INTAKE file
            call xml_write_intake(pathToTarget)

        end if

        call html_write_header(device, 'Demand for subjects by New Freshmen entering ')

        ! count demand for 1st sem subjects of each specified curriculum
        do idxCURR=1,NumCurricula
            if (NFintake(idxCURR)==0) cycle
            do j=1,Curriculum(idxCURR)%NSubjects
                if (Curriculum(idxCURR)%SubjectTerm(j) /= 1) cycle
                crse = Curriculum(idxCURR)%SubjectIdx(j)
                Offering(crse)%Demand = Offering(crse)%Demand + NFintake(idxCURR)
            end do
        end do
        ! enable edit
        write(device,AFORMAT) &
        '<form name="input" method="post" action="'//CGI_PATH//'">', &
        '<input type="hidden" name="F" value="'//trim(itoa(fnUpdateDemandFreshmen))//'">', &
        '<input type="hidden" name="A1" value="'//trim(tCollege)//'">'
        do idxCURR=1,NumCurricula
            if (NFintake(idxCURR)/=0) then
                write(device,AFORMAT) '<input type="hidden" name="'//trim(Curriculum(idxCURR)%Code)// &
                '" value="'//trim(itoa(NFintake(idxCURR)))//'">'
            end if
        end do
        write(device,AFORMAT) &
        '<i>Change count for <select name="curriculum">', &
        '<option value="select"> -Select curriculum-'
        do idxCURR=1,NumCurricula
            if ( (Curriculum(idxCURR)%CollegeIdx==CollegeIdxUser .or. isRoleAdmin) .and. &
            Curriculum(idxCURR)%NSubjects/=0) then
                write(device,AFORMAT) &
                '<option value="'//trim(Curriculum(idxCURR)%Code)//'"> '//Curriculum(idxCURR)%Code
            end if
        end do
        write(device,AFORMAT) &
        '</select>', &
        nbsp//' to '//nbsp//nbsp//'<input type="text" size="4" name="count" value="">', &
        nbsp//nbsp//'<input type="submit" value="Submit">', &
        '</i></form>'
    
        ! write HTML table
        write(device,AFORMAT) '<table border="1" width="100%">'
        write(device,AFORMAT) begintr//'<td valign="bottom">COURSE'//endtd//&
        !'<td align="center" valign="bottom"><p>E<br>X<br>C<br>E<br>S<br>S</p>'//endtd//&
        !'<td align="center" valign="bottom"><p>A<br>V<br>A<br>I<br>L</p>'//endtd//&
        '<td align="center" valign="bottom"><p>D<br>E<br>M<br>A<br>N<br>D</p>'//endtd
        do idxCURR=1,NumCurricula
            if (NFintake(idxCURR)==0) cycle
            tCurriculum = Curriculum(idxCURR)%Code
            l = len_trim(tCurriculum)
            write(device,'(22a)') '<td align="center" valign="top"><p>'// &
            trim(itoa(NFintake(idxCURR)))//'<br>'//nbsp//'<br>', &
            (tCurriculum(k:k)//'<br>', k=1,l-1), tCurriculum(l:l)//'</p>'//endtd
        end do
        ! approximate blocks
        write(device,AFORMAT) endtr//begintr//'<td width="8%">BLOCKS'//endtd//tdnbspendtd
        do idxCURR=1,NumCurricula
            if (NFintake(idxCURR)==0) cycle
            write(device,AFORMAT) tdalignright//trim(itoa((1+NFintake(idxCURR))/25))//endtd
        end do
        write(device,AFORMAT) endtr
        ! write demand per subject
        do crse=1,MAX_ALL_SUBJECTS
            if (Offering(crse)%Demand == 0) cycle
            write(device,'(4a)') begintr//'<td width="8%">'//trim(Subject(crse)%Name)//endtd, &
            !'tdalignright//trim(itoabz(-Offering(crse)%Demand+Offering(crse)%TotalSlots))//endtd, &
            !'tdalignright//trim(itoabz(Offering(crse)%TotalSlots))//endtd, &
            tdalignright//trim(itoabz(Offering(crse)%Demand))//endtd
            do idxCURR=1,NumCurricula
                if (NFintake(idxCURR)==0) cycle
                j =  index_of_subject_in_curriculum (Curriculum(idxCURR), crse)
                if (j==0) then ! crse not in curriculum
                    write(device,AFORMAT) tdnbspendtd
                    cycle
                end if
                if (Curriculum(idxCURR)%SubjectTerm(j)==1) then ! crse is for 1st sem, 1st year
                    write(device,AFORMAT) tdalignright//trim(itoabz(NFintake(idxCURR)))//endtd
                else
                    write(device,AFORMAT) tdnbspendtd
                end if
            end do
            write(device,AFORMAT) endtr
        end do
        write(device,AFORMAT) '</table><hr>'

        return
    end subroutine demand_by_new_freshmen


end module DEMAND
