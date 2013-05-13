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


module TIMES

    use BASE

    implicit none

    ! years, year levels
    integer, parameter :: MAX_LEN_TEXT_YEAR = 7
    character (len=MAX_LEN_TEXT_YEAR), dimension(0:17) :: txtYear = (/ &
        'ERROR  ', 'FIRST  ', 'SECOND ', 'THIRD  ', 'FOURTH ', 'FIFTH  ', 'SIXTH  ', &
        'SEVENTH', 'EIGHTH ', &
        '-------', 'First  ', 'Second ', 'Third  ', 'Fourth ', 'Fifth  ', 'Sixth  ', &
        'Seventh', 'Eighth ' /)

     ! academic terms
    integer, parameter :: MAX_LEN_TEXT_SEMESTER = 6
    character (len=MAX_LEN_TEXT_SEMESTER), dimension(0:9) :: txtSemester = (/ &
        'ERROR ','FIRST ', 'SECOND', 'SUMMER', &
                 'First ', 'Second', 'Summer',&
                 '1st   ', '2nd   ', 'Summer' /)
    character (len=10) :: termQualifier(0:9) = (/ &
        ' ERROR    ', ' SEMESTER ', ' SEMESTER ', ' TERM     ', &
                      ' Semester ', ' Semester ', ' Term     ', &
                      ' Sem.     ', ' Sem.     ', ' Term     ' /)

    ! days
    character (len = 3), dimension (0:6) :: txtDay = (/       &
    '   ','Mon','Tue','Wed','Thu','Fri','Sat'/)
    !character (len = 2), dimension (0:6) :: txtDay2 = (/       &
    !    '  ','M ','T ','W ','Th','F ','S '/)

    ! times
    integer, parameter :: MAX_LEN_TEXT_TIME = 5
    character (len=MAX_LEN_TEXT_TIME), dimension (0:57) :: txtTime = (/ &
    '     ', '7    ', &    !       1
    '7:15 ', '7:30 ', '7:45 ', '8    ', &    !  2 -  5
    '8:15 ', '8:30 ', '8:45 ', '9    ', &    !  6 -  9
    '9:15 ', '9:30 ', '9:45 ', '10   ', &    ! 10 - 13
    '10:15', '10:30', '10:45', '11   ', &    ! 14 - 17
    '11:15', '11:30', '11:45', '12   ', &    ! 18 - 21
    '12:15', '12:30', '12:45', '1    ', &    ! 22 - 25
    '1:15 ', '1:30 ', '1:45 ', '2    ', &    ! 26 - 29
    '2:15 ', '2:30 ', '2:45 ', '3    ', &    ! 30 - 33
    '3:15 ', '3:30 ', '3:45 ', '4    ', &    ! 34 - 37
    '4:15 ', '4:30 ', '4:45 ', '5    ', &    ! 38 - 41
    '5:15 ', '5:30 ', '5:45 ', '6    ', &    ! 42 - 45
    '6:15 ', '6:30 ', '6:45 ', '7p   ', &    ! 46 - 49
    '7:15p', '7:30p', '7:45p', '8p   ', &    ! 50 - 53
    '8:15p', '8:30p', '8:45p', '9p   ' /)    ! 54 - 57

    character (len=3) :: ampm(0:2) = (/ ' nn', ' am', ' pm' /)

    ! special times
    integer, parameter :: &
        TIME_INDEX_EARLY_DAY=5,  &            ! index of 8:00 am; time before is 'early'
        TIME_INDEX_BEGIN_LUNCH=13, &          ! begin lunchtime (10:00 am)
        TIME_INDEX_END_LUNCH=29, &            ! end lunchtime (2:00 pm)
        TIME_INDEX_LATE_DAY=45                ! index of 6:00 pm


contains


    function index_to_year (tYear)
        ! returns index of tYear in the list of Years
        integer :: index_to_year
        character (len=MAX_LEN_TEXT_YEAR), intent (in) :: tYear
        integer :: i, idx

        idx = 0
        do i=1,17
            if (tYear==txtYear(i)) then
                idx = i
                exit
            end if
        end do
        if (idx>9) idx = idx-9
        index_to_year = idx


    end function index_to_year


    function text_time_period(startime, endtime)
        character (len=2*MAX_LEN_TEXT_TIME+1) :: text_time_period
        integer, intent (in) :: startime, endtime
        if (startime>=1 .and. endtime<=57 .and. startime<endtime) then
            text_time_period = trim(txtTime(startime))//"-"//trim(txtTime(endtime))
        else
            text_time_period = 'TBA'
          !call file_log_message('Error in time index. Aborting...')
          !stop
        end if

    end function text_time_period


    function index_to_time(tTime)
        integer :: index_to_time
        character (len=MAX_LEN_TEXT_TIME), intent (in) :: tTime
        integer :: hdx, i
        hdx = 0
        do i=1,57
            if (txtTime(i)==tTime) then
                hdx = i
                exit
            end if
        end do
        index_to_time = hdx

    end function index_to_time


    function index_to_term (tTerm)

        ! returns index of tTerm in the list of Terms
        integer :: index_to_term
        character (len=MAX_LEN_TEXT_SEMESTER), intent (in) :: tTerm
        integer :: i
        index_to_term = 0
        do i=1,9
            if (tTerm==txtSemester(i)) then
                index_to_term = i
                exit
            end if
        end do
        if (index_to_term>6) index_to_term = index_to_term - 3
        if (index_to_term>3) index_to_term = index_to_term - 3

    end function index_to_term


    subroutine rank_to_year_term (rank, Year, Term)
        ! rank: 1 2 3 4 5 6 7 8 9 10 11 12
        ! year: 1 1 1 2 2 2 3 3 3  4  4  4
        ! term: 1 2 3 1 2 3 1 2 3  1  2  3
        integer, intent(in) :: rank
        integer, intent(out) :: Year, Term
        Year = (rank+2)/3
        Term = rank-3*(Year-1)

    end subroutine rank_to_year_term


    subroutine qualify_term (plusCurrent, Year, Term, description)
        integer, intent(in) :: plusCurrent
        integer, intent(out) :: Year, Term
        character(len=*), intent (out) :: description

        if (plusCurrent<=3) then
            Year = currentYear
            Term = plusCurrent
        else
            Year = currentYear+1
            Term = plusCurrent-3
        end if
        if (Term<3) then
            description = trim(txtSemester(Term+6))//' Semester, '//text_school_year(Year)
        else
            description = 'Summer Term, '//text_school_year(Year)
        end if

    end subroutine qualify_term


    function text_term_offered (num)
        ! returns the text representation of TermOffered of subj

        character (len=3) :: text_term_offered
        integer, intent (in) :: num

        character (len=3) :: term
        integer :: i

        if (num==0) then
            text_term_offered = '0'
            return
        end if
        term = SPACE
        i = num
        if (i>=4) then
            term = 'S'//term
            i = i-4
        end if
        if (i>=2) then
            term = '2'//term
            i = i-2
        end if
        if (i==1) term = '1'//term
        text_term_offered = term


    end function text_term_offered


    function text_term_offered_separated (num)
        ! returns the text representation of TermOffered of subj

        character (len=5) :: text_term_offered_separated
        integer, intent (in) :: num

        character (len=5) :: term
        integer :: i

        if (num==0) then
            text_term_offered_separated = '0'
            return
        end if

        term = SPACE
        i = num
        if (i>=4) then
            term = 'S' ! 'Summer'
            i = i-4
            if (i>0) term = ','//term ! ', '//term
        end if

        if (i>=2) then
            term = '2'//term ! 'Second Semester'//term
            i = i-2
            if (i>0) term = ','//term ! ', '//term
        end if

        if (i==1) term = '1'//term ! 'First Semester'//term

        text_term_offered_separated = term


    end function text_term_offered_separated


    function text_school_year (year)
        ! returns the text representation of SY year-(year+1)

        character (len=10) :: text_school_year
        integer, intent (in) :: year

        text_school_year = 'SY '//trim(itoa(year))//dash//itoa2bz(mod(year+1,1000))


    end function text_school_year


end module TIMES
