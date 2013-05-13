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


module ADVISING

    use WAIVERS
    use CHECKLISTS
    use CGI

    implicit none

contains


#include "custom_advising.F90"


    subroutine get_scholastic_three_terms (std, givenYear, givenTerm, UnitsPaid, UnitsDropped, UnitsPassed, Standing)
        integer :: std, Standing, UnitsPaid, UnitsDropped, UnitsPassed, givenYear, givenTerm
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
        do i=1,Student(std)%Record(1,0)

            ! Record(i,:) 1=type,2=year,3=term,4=subject,5=grade
            if (Student(std)%Record(1,i)/=1) cycle ! not FINALGRADE

            if (Student(std)%Record(2,i)/=givenYear .or. &
                Student(std)%Record(3,i)/=givenTerm) cycle

            cdx = Student(std)%Record(4,i)
            gdx = Student(std)%Record(5,i)
            if (gdx>0 .and. cdx>0) then
                tUnits = Subject(cdx)%Units
                tHours = Subject(cdx)%LectHours+Subject(cdx)%LabHours
                UnitsPaid = UnitsPaid + tUnits
                HoursPaid = HoursPaid + tHours
                if (gdx==gdxREGD) then
                    if (advisingPeriod) then ! currently registered
                        UnitsREGD = UnitsREGD + tUnits
                        HoursREGD = HoursREGD + tHours
                    else ! assume passed
                        UnitsPassed = UnitsPassed + tUnits
                        HoursPassed = HoursPassed + tHours
                    end if
                else if (gdx==gdxDRP .or. gdx==gdxLOA) then
                    UnitsDropped = UnitsDropped + tUnits
                    HoursDropped = HoursDropped + tHours
                else if (is_grade_passing(gdx) ) then
                    UnitsPassed = UnitsPassed + tUnits
                    HoursPassed = HoursPassed + tHours
                else if (is_grade_passing(TCG(i)%ReExam,advisingPeriod) ) then
                    UnitsPassed = UnitsPassed + tUnits
                    HoursPassed = HoursPassed + tHours
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

    end subroutine get_scholastic_three_terms



    subroutine advise_all_students(UseCLASSES, Offering)
        logical, intent (in) :: UseCLASSES
        type (TYPE_OFFERED_SUBJECTS), intent(in) :: Offering(MAX_ALL_DUMMY_SUBJECTS:)

        ! executed during period 2 (enlisted subjects are finalized, schedule of classes
        ! for next term are not yet set) to generate probabilistic forecast of demand for subjects
        !
        ! executed during period 3 (grades are available, schedule of classes for next
        ! term are available) to generate deterministic forecast of demand for subjects

        integer :: std, NRemaining, MissingPOCW, idxSTD

        write(*,*)  'Generating the advice for each student... please wait...'
        do idxSTD = 1,NumStudents
            if (mod(idxSTD,1000) == 0) then
                write(*,*) trim(itoa(idxSTD))//' / '//itoa(NumStudents)//' done...'
            end if
            std = StdRank(idxSTD)
            call advise_student (std, UseCLASSES, Offering, WaiverCOI(std), Advised(std), MissingPOCW, NRemaining)
        end do


    end subroutine advise_all_students


end module ADVISING
