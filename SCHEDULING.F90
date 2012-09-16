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

module SCHEDULING

    use ADVISING
    use TIMETABLES

    implicit none

contains


    subroutine generate_initial_schedules(idxGrp, MaxAlternates)

        integer, intent (in) :: idxGrp, MaxAlternates
        integer :: i, j, nb, ns, std
        integer, parameter :: MaxIterations = 100
        integer :: ier, crse, sect

        ! any students?
        if (NumStudents==0) then
                write(*,*) 'No students ?'
                return
        end if

        ! merge lecture-lab times
        call sections_compound(NumCurrentSections, CurrentSection, ier, .true.) ! true = ignore errors
        call file_log_message('# compounded sections ='//itoa(NumCurrentSections))
        if (ier>0 .or. NumCurrentSections==0) then
                write(*,*) 'No classes ?'
                return
        end if

        ! read ENLISTMENT files for earlier priority groups
        call read_pre_enlistment(pathToTarget, 'ENLISTMENT', 0, idxGrp-1, &
            NumCurrentSections, CurrentSection, Preenlisted, NumEnlistmentRecords, ier)
        call file_log_message('# enlistment records in previous runs ='//itoa(NumEnlistmentRecords))

        ! subtract committed slots
        nb = 0
        CurrentSection(1:NumCurrentSections)%RemSlots = CurrentSection(1:NumCurrentSections)%Slots
        do std=1,NumStudents
            if (Preenlisted(std)%lenSubject == 0) cycle ! this student not enlisted
            ns = 0
            do j=1,Preenlisted(std)%lenSubject
                sect = Preenlisted(std)%Section(j)
                if (sect > 0) then
                    ns = ns+1
                    if (CurrentSection(sect)%Slots > 0) CurrentSection(sect)%RemSlots = CurrentSection(sect)%RemSlots-1
                end if
            end do
            if (ns>0) then
                nb = nb+1
                Preenlisted(std)%lenSubject = -Preenlisted(std)%lenSubject
            end if
        end do
        call file_log_message('# students enlisted in previous runs ='//itoa(nb))

        ! remove full sections
        ns = 0
        StdRank = 0
        do sect=1,NumCurrentSections
            if (CurrentSection(sect)%RemSlots < 0) then  ! section is overbooked
                if (CurrentSection(sect)%RemSlots < -5) then  ! really overbooked
                    call file_log_message(trim(CurrentSection(sect)%ClassId)//' is overbooked by '// &
                        itoa(-CurrentSection(sect)%RemSlots))
                    !call Pause()
                else
                    call file_log_message(trim(CurrentSection(sect)%ClassId)//' is overbooked by <=5; adjusted')
                    CurrentSection(sect)%Slots = CurrentSection(sect)%Slots-CurrentSection(sect)%RemSlots
                    !call Pause()
                end if
            else if (CurrentSection(sect)%RemSlots == 0) then
                if (CurrentSection(sect)%Slots > 0) then  ! section is full
                    call file_log_message(trim(CurrentSection(sect)%ClassId)//' is full!')
                    call initialize_section(CurrentSection(sect))
                else ! section has no limit on the no. of students?
                    ns = ns+1
                    StdRank(ns) = sect
                end if
            else
                ! set to remaining slots
                CurrentSection(sect)%Slots = CurrentSection(sect)%RemSlots
                ns = ns+1
                StdRank(ns) = sect
            end if
        end do
        do i=1,ns
            CurrentSection(i) = CurrentSection(StdRank(i))
        end do
        call file_log_message( '# sections removed ='//itoa(NumCurrentSections - ns))
        NumCurrentSections = ns
        if (NumCurrentSections==0) then
                write(*,*) 'No open sections remaining ?'
                return
        end if

        ! count remaining sections, total seats for each subject
        CurrentOffering = TYPE_OFFERED_SUBJECTS (0, 0, 0, 0, 0, 0, 0, 0)
        do sect=1,NumCurrentSections
            crse = CurrentSection(sect)%SubjectIdx
            CurrentOffering(crse)%TotalSlots = CurrentOffering(crse)%TotalSlots + CurrentSection(sect)%Slots
            CurrentOffering(crse)%NSections = CurrentOffering(crse)%NSections + 1
        end do

        ! retrieve predictions
        call initialize_pre_enlistment(Preenlisted(0))
        Preenlisted(1:) = Preenlisted(0)
        NumEnlistmentRecords = 0
        call read_predictions(pathToTarget, NumCurrentSections, CurrentSection, Preenlisted, NumEnlistmentRecords, ier)
        call file_log_message('# prediction records ='//itoa(NumEnlistmentRecords))
        if (ier/=0 .or. NumEnlistmentRecords==0) then
                write(*,*) 'No PREDICTIONS.XML ?'
                return
        end if

        ! remove students not in this group and those without predicted subjects
        StdRank = 0
        nb = 0
        do i=1,NumStudents
#if defined CUSTOM
            ! use student's year in curriculum as his/her priority group
            Preenlisted(i)%StdPriority = Preenlisted(i)%StdYear
#endif
            if (Preenlisted(i)%lenSubject>0 .and. Preenlisted(i)%StdPriority==idxGrp) then
                nb = nb + 1
                StdRank(nb) = i
            end if
        end do
        do i=1,nb
            Student(i) = Student(StdRank(i))
            Preenlisted(i) = Preenlisted(StdRank(i))
        end do
        NumStudents = nb
        call file_log_message('# students in priority group '//trim(itoa(idxGrp))//' ='//itoa(nb))
        if (nb==0) then
                write(*,*) 'No students to schedule ?'
                return
        end if

        write(*,*) 'The scheduling algorithm needs to be customized for '//UniversityCode

        return
    end subroutine generate_initial_schedules



end module SCHEDULING
