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


module ADVISING

    use WAIVERS
    use CHECKLISTS
    use CGI

    implicit none

contains


#include "custom_advising.F90"


    subroutine get_scholastic_three_terms (givenYear, givenTerm, UnitsPaid, UnitsDropped, UnitsPassed, Standing)
        integer :: Standing, UnitsPaid, UnitsDropped, UnitsPassed, givenYear, givenTerm
        integer :: gdx, cdx, i, tUnits, performanceUnits, UnitsREGD, tHours
        integer :: HoursPaid, HoursDropped, HoursPassed, HoursREGD
        real :: pctFailed
        ! count units registered, passed
        UnitsPaid = 0
        UnitsREGD = 0
        UnitsDropped = 0
        UnitsPassed = 0
        HoursPaid = 0
        HoursDropped = 0
        HoursPassed = 0
        HoursREGD = 0
        do i=1,lenTCG
            if (TCG(i)%ErrorCode/=0 .or. TCG(i)%Code/=2) cycle
            ! ignore not enrolled subjects
            if (index(TCG(i)%txtLine, 'APE')>0) cycle ! not enrolled
            if (index(TCG(i)%txtLine, 'REMOVAL')>0) cycle ! not enrolled
            if (index(TCG(i)%txtLine, 'COMPLETION')>0) cycle ! not enrolled
            !
            if (TCG(i)%Year==givenYear .and. TCG(i)%Term==givenTerm) then

                gdx = TCG(i)%Grade
                cdx = TCG(i)%Subject
                if (gdx>0 .and. cdx>0) then
                    tUnits = Subject(cdx)%Units
                    tHours = Subject(cdx)%LectHours+Subject(cdx)%LabHours
                    UnitsPaid = UnitsPaid + tUnits
                    HoursPaid = HoursPaid + tHours
                    if (gdx==gdxREGD) then
                        UnitsREGD = UnitsREGD + tUnits
                        HoursREGD = HoursREGD + tHours
                    else if (gdx==gdxDRP .or. gdx==gdxLOA) then
                        UnitsDropped = UnitsDropped + tUnits
                        HoursDropped = HoursDropped + tHours
                    else if (is_grade_passing(gdx) ) then
                        UnitsPassed = UnitsPassed + tUnits
                        HoursPassed = HoursPassed + tHours
                    else if (is_grade_passing(TCG(i)%ReExam,Period==2) ) then
                        UnitsPassed = UnitsPassed + tUnits
                        HoursPassed = HoursPassed + tHours
                    end if
                end if
            end if
        end do

        Standing = 0
        if (HoursPaid==0) then ! did not register; on LOA?
            Standing = 8
        else if (HoursPaid==HoursDropped) then ! started LOA
            Standing = 7
        else if (HoursPaid==HoursREGD) then ! grades not yet available
            Standing = 8
        else ! student received some grades; calculate standing
            if (UnitsPaid==0) then ! only zero-unit subjects were enrolled; use hours
                performanceUnits = HoursPaid - HoursDropped
                pctFailed = (100.0*(performanceUnits-HoursPassed))/amax0(1, performanceUnits)
            else ! non-zero unit subjects were enrolled
                performanceUnits = UnitsPaid - UnitsDropped
                pctFailed = (100.0*(performanceUnits-UnitsPassed))/amax0(1, performanceUnits)
            end if
            ! compute status
            if (HoursPassed==0) then
                Standing = 6 ! PERMANENTLY DISQUALIFIED
            else if (pctFailed>75.0) then
                Standing = 5 ! DISMISSED
            else if (pctFailed>50.0) then
                Standing = 4 ! PROBATION
            else if (pctFailed>25.0) then
                Standing = 3 ! WARNING
            !else  if (pctFailed>0.0) then
            !        Standing = 2 ! failed<=25%
            else  
                Standing = 1 ! no failures
            end if
        end if
        return
    end subroutine get_scholastic_three_terms


    subroutine advise_all_students()
        ! executed during period 2 (enlisted subjects are finalized, schedule of classes
        ! for next term are not yet set) to generate probabilistic forecast of demand for subjects
        !
        ! executed during period 3 (grades are available, schedule of classes for next
        ! term are available) to generate deterministic forecast of demand for subjects

        integer :: std, NRemaining, MissingPOCW, idxSTD

        call initialize_pre_enlistment(Advised(0))
        Advised = Advised(0)

        write(*,*)  'Generating the advice for each student... please wait...'
        do idxSTD = 1,NumStudents
            if (mod(idxSTD,1000) == 0) then
                write(*,*) trim(itoa(idxSTD))//' / '//itoa(NumStudents)//' done...'
            end if
            std = StdRank(idxSTD)
            call advise_student (std, UseNextClasses, NextOffering, WaiverCOI(std), Advised(std), MissingPOCW, NRemaining)
        end do

        return
    end subroutine advise_all_students

end module ADVISING
