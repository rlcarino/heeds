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


module EditUNIVERSITY

    use HTML

    implicit none

contains


    subroutine display_signatories(device)
        integer, intent(in) :: device
        integer :: iColl

        targetDepartment = DeptIdxUser
        targetCollege = CollegeIdxUser

        call html_write_header(device, 'Update signatories in teaching load form', SPACE)

        call make_form_start(device, fnEditSignatories)

        write(device,AFORMAT) '<table border="0" width="100%">', &
            begintr//thalignright//'Title or Position'//endth//tdnbspendtd//thalignleft//'Name'//endth//endtr, &
            begintr//tdalignright//'<i>(University name)</i>'//endtd//tdnbspendtd// &
                     begintd//'<input name="UniversityName" size="60" value="'//trim(UniversityName)//'">'//endtd// &
            endtr, &
            begintr//tdalignright//'<i>(University address)</i>'//endtd//tdnbspendtd// &
                     begintd//'<input name="UniversityAddress" size="60" value="'//trim(UniversityAddress)//'">'//endtd// &
            endtr, &
            begintr// &
                tdalignright//'<input name="titleUniversityPresident" size="40" value="'//trim(titleUniversityPresident)//'">'// &
                endtd//tdnbspendtd// &
                begintd//'<input name="UniversityPresident" size="60" value="'//trim(UniversityPresident)//'">'//endtd// &
            endtr, &
            begintr// &
                tdalignright//'<input name="titleVPAcademicAffairs" size="40" value="'//trim(titleVPAcademicAffairs)//'">'// &
                endtd//tdnbspendtd// &
                begintd//'<input name="VPAcademicAffairs" size="60" value="'//trim(VPAcademicAffairs)//'">'//endtd// &
            endtr, &
            begintr// &
                tdalignright//'<input name="titleDeanOfCampus" size="40" value="'//trim(titleDeanOfCampus)//'">'// &
                endtd//tdnbspendtd// &
                begintd//'<input name="DeanOfCampus" size="60" value="'//trim(DeanOfCampus)//'">'//endtd// &
            endtr, &
            begintr// &
                tdalignright//'<input name="titleDeanOfInstruction" size="40" value="'//trim(titleDeanOfInstruction)//'">'// &
                endtd//tdnbspendtd// &
                begintd//'<input name="DeanOfInstruction" size="60" value="'//trim(DeanOfInstruction)//'">'//endtd// &
            endtr

        do iColl=1,NumColleges-1
            write(device,AFORMAT) &
                begintr//tdalignright//'<i>(College Dean, '//trim(College(iColl)%Code)//')</i>'//endtd//tdnbspendtd// &
                         begintd//'<input name="DEAN:'//trim(College(iColl)%Code)//'" size="60" value="'// &
                         trim(College(iColl)%Dean)//'">'//endtd// &
                endtr
        end do

        write(device,AFORMAT) &
            '</table><br>'//nbsp//'<input name="action" type="submit" value="Update"></form><hr>'


    end subroutine display_signatories


    subroutine edit_signatories(device)
        integer, intent(in) :: device
        character(len=MAX_LEN_TEACHER_CODE) :: tAction
        character (len=255) :: tInput
        logical :: changes
        integer :: iColl, ierr

        ! check for requested action
        call cgi_get_named_string(QUERY_STRING, 'action', tAction, ierr)

        if (ierr/=0 .or. tAction==SPACE) then ! no action; display existing info

        else ! action is Update

            ! collect changes to UNIVERSITY.XML
            changes = .false.
            call cgi_get_named_string(QUERY_STRING, 'titleUniversityPresident', tInput, ierr)
            if (ierr/=0) tInput = titleUniversityPresident
            if (tInput /= titleUniversityPresident) then
                titleUniversityPresident = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'titleVPAcademicAffairs', tInput, ierr)
            if (ierr/=0) tInput = titleVPAcademicAffairs
            if (tInput /= titleVPAcademicAffairs) then
                titleVPAcademicAffairs = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'titleDeanOfCampus', tInput, ierr)
            if (ierr/=0) tInput = titleDeanOfCampus
            if (tInput /= titleDeanOfCampus) then
                titleDeanOfCampus = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'titleDeanOfInstruction', tInput, ierr)
            if (ierr/=0) tInput = titleDeanOfInstruction
            if (tInput /= titleDeanOfInstruction) then
                titleDeanOfInstruction = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'UniversityName', tInput, ierr)
            if (ierr/=0) tInput = UniversityName
            if (tInput/=UniversityName) then
                UniversityName = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'UniversityAddress', tInput, ierr)
            if (ierr/=0) tInput = UniversityAddress
            if (tInput /= UniversityAddress) then
                UniversityAddress = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'UniversityPresident', tInput, ierr)
            if (ierr/=0) tInput = UniversityPresident
                if (tInput /= UniversityPresident) then
                UniversityPresident = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'VPAcademicAffairs', tInput, ierr)
            if (ierr/=0) tInput = VPAcademicAffairs
            if (tInput /= VPAcademicAffairs) then
                VPAcademicAffairs = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'DeanOfCampus', tInput, ierr)
            if (ierr/=0) tInput = DeanOfCampus
            if (tInput /= DeanOfCampus) then
                DeanOfCampus = tInput
                changes = .true.
            end if

            call cgi_get_named_string(QUERY_STRING, 'DeanOfInstruction', tInput, ierr)
            if (ierr/=0) tInput = DeanOfInstruction
            if (tInput /= DeanOfInstruction) then
                DeanOfInstruction = tInput
                changes = .true.
            end if

            if (changes) call xml_write_university(pathToYear)

            ! collect changes to COLLEGES.XML
            changes = .false.
            do iColl=1,NumColleges-1
                call cgi_get_named_string(QUERY_STRING, 'DEAN:'//trim(College(iColl)%Code), tInput, ierr)
                if (ierr/=0) tInput = College(iColl)%Dean
                if (tInput /= College(iColl)%Dean) then
                    College(iColl)%Dean = tInput
                    changes = .true.
                end if
            end do

            if (changes) call xml_write_colleges(pathToYear)

        end if


        call display_signatories(device)


    end subroutine edit_signatories


end module EditUNIVERSITY
