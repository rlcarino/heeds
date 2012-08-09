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


module GRADES

    use BASE

    implicit none

    ! grade types
    character (len=10), dimension(0:3) :: txtGradeType = (/ &
        'APE       ', 'FINALGRADE', 'REMOVAL   ', 'COMPLETION' /)

    ! grades
    integer, parameter :: MAX_LEN_TEXT_GRADE = 4
    character (len = MAX_LEN_TEXT_GRADE), dimension(0:49) :: txtGrade = (/  &
        '____',                             & !  0, 0
        '1.00',  '1.0 ',  '1   ',           & !  1, 1-3
        '1.25',                             & !  2, 4
        '1.50',  '1.5 ',                    & !  3, 5-6
        '1.75',                             & !  4, 7
        '2.00',  '2.0 ',  '2   ',           & !  5, 8-10
        '2.25',                             & !  6, 11
        '2.50',  '2.5 ',                    & !  7, 12-13
        '2.75',                             & !  8, 14
        '3.00',  '3.0 ',  '3   ',           & !  9, 15-17
        '4.00',  '4.0 ',  '4   ',           & ! 10, 18-20
        'INC.',  'INC ',  'Inc ',  'Inc.',  & ! 11, 21-24
        '5.00',  '5.0 ',  '5   ',           & ! 12, 25-27
        'DRP.',  'DRP ',  'Drp.',  'Drp ',  & ! 13, 28-31
        'S   ',  'S.  ',  's   ',  's.  ',  & ! 14, 32-35
        'U   ',  'U.  ',  'u   ',  'u.  ',  & ! 15, 36-39
        'PASS',  'Pass',                    & ! 16, 40-41
        'LOA ',  'LOA.',  'Loa ',  'Loa.',  & ! 17, 42-45
        'REGD',                             & ! 18, 46
        'FAIL',  'Fail',                    & ! 19, 47-48
        '****' /)                             ! 20, 49

    ! pointer to grade
    integer, dimension(0:20) :: pGrade = (/ &
        0,  1,  4,  5,  7,  8, 11, 12, 14, 15, &
        18, 21, 25, 28, 32, 36, 40, 42, 46, 47, 49/)

    ! shorcuts to certain grades
    integer ::  &
        gdx4    = 10, gdxINC  = 11, gdx5     = 12, gdxDRP = 13, &
        gdxS    = 14, gdxU    = 15, gdxPASS  = 16, gdxLOA = 17, &
        gdxREGD = 18, gdxFAIL = 19, gdxRECOM = 20

    real, dimension(0:20) :: fGrade = (/  & ! float value for grades
        0.00,                   & ! error
        1.00, 1.25, 1.50, 1.75, & ! 1, 1.25, 1.5, 1.75
        2.00, 2.25, 2.50, 2.75, & ! 2, 2.25, 2.5, 2.75
        3.00, 4.00, 0.00, 5.00, & ! 3, 4, INC, 5
        0.00, 0.00, 0.00, 0.00, & ! DRP, S, U, PASS
        0.00, 0.00, 0.00, 0.00 /) ! LOA, REGD, FAIL, ****


    ! scholastic standing
    character (len=12), dimension(0:8) :: txtScholastic = (/ &
        'ERROR       ', 'Good        ', 'Satisfactory', 'Warning     ', 'Probation   ', &
        'Dismissed   ', 'Perm Disq   ', 'LOA         ', '(NA)        '/)

    ! classification
    character (len=9), dimension(0:10) :: txtStanding = (/ &
        'ERROR    ', 'FRESHMAN ', 'SOPHOMORE', 'JUNIOR   ', 'SENIOR   ', 'FIFTH    ', &
        'SIXTH    ', 'SEVENTH  ', 'EIGHTH   ', 'NINTH    ', 'GRADUATE '/)


contains



    function is_grade_numeric_pass(GradeIdx, checkREGD)
        logical :: is_grade_numeric_pass
        integer, intent (in) :: GradeIdx
        logical, intent (in), optional :: checkREGD
        logical :: includeREGD
        if (present(checkREGD)) then
            includeREGD = checkREGD
        else
            includeREGD = .true.
        end if
        is_grade_numeric_pass = (GradeIdx > 0 .and. GradeIdx < 10) .or. &
        GradeIdx == gdxPASS .or. (GradeIdx == 18 .and. includeREGD)
        return
    end function is_grade_numeric_pass

    function is_grade_passing(GradeIdx, checkREGD)
        logical :: is_grade_passing
        integer, intent (in) :: GradeIdx
        logical, intent (in), optional :: checkREGD
        logical :: includeREGD
        if (present(checkREGD)) then
            includeREGD = checkREGD
        else
            includeREGD = .true.
        end if
        is_grade_passing = GradeIdx == gdxS .or. &
        (GradeIdx > 0 .and. GradeIdx < 10) .or. &
        GradeIdx == gdxPASS .or. (GradeIdx == 18 .and. includeREGD)
        return
    end function is_grade_passing

    function is_grade_failing(GradeIdx)
        logical :: is_grade_failing
        integer, intent (in) :: GradeIdx
        is_grade_failing = GradeIdx==gdx5 .or. GradeIdx==gdxU .or. &
        GradeIdx==gdxDRP .or. GradeIdx==gdxLOA .or. GradeIdx==gdxFAIL
        return
    end function is_grade_failing

    function is_grade_conditional(GradeIdx)
        logical :: is_grade_conditional
        integer, intent (in) :: GradeIdx
        is_grade_conditional = GradeIdx==gdx4 .or. GradeIdx==gdxINC
        return
    end function is_grade_conditional

    function index_to_grade (Token)
        integer :: index_to_grade
        character (len=MAX_LEN_TEXT_GRADE), intent (in) :: Token
        integer :: i, j, Idx
        idx = -99
        do i = 0, 19
            do j=pGrade(i), pGrade(i+1)-1
                if (txtGrade(j)==Token) then
                    Idx = i
                    exit
                end if
            end do
        end do
        if (Idx<0) then
            i = atoi(Token)
            select case (i)
                case (99:100) ! 1.0
                    Idx = 1
                case (96:98) ! 1.25
                    Idx = 2
                case (93:95) ! 1.5
                    Idx = 3
                case (90:92) ! 1.75
                    Idx = 4
                case (87:89) ! 2.0
                    Idx = 5
                case (84:86) ! 2.25
                    Idx = 6
                case (81:83) ! 2.5
                    Idx = 7
                case (78:80) ! 2.75
                    Idx = 8
                case (75:77) ! 3.0
                    Idx = 9
                case default ! 5.0
                    Idx = 12
            end select
        end if
        index_to_grade = Idx
        return
    end function index_to_grade


end module GRADES
