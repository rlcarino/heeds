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


module CGI

  use BASE

    implicit none

    ! background color in timetables
    character(len=7), parameter, dimension(0:14) :: bgcolor = (/ &
        '#f0f8ff', & ! aliceblue
        '#faebd7', & ! antiquewhite
        '#7fffd4', & ! aquamarine
        '#deb887', & ! burlywood
        '#5f9ea0', & ! cadetblue
        '#7fff00', & ! chartreuse
        '#d2691e', & ! chocolate
        '#ff7f50', & ! coral
        '#6495ed', & ! cornflowerblue
        '#dcdcdc', & ! gainsboro
        '#bdb76b', & ! darkkhaki
        '#ffa07a', & ! lightsalmon
        '#ff00ff', & ! fuchsia
        '#ffd700', & ! gold
        '#da70d6' & ! orchid
     /)

    ! some HTML tokens
    character(len=20), parameter :: Blue = '<font color=#0000FF>'
    character(len=20), parameter :: Fuchsia = '<font color=#FF00FF>'
    character(len=20), parameter :: Gray = '<font color=#808080>'
    character(len=20), parameter :: Green = '<font color=#008000>'
    character(len=20), parameter :: Lime = '<font color=#00FF00>'
    character(len=20), parameter :: Maroon = '<font color=#800000>'
    character(len=20), parameter :: Navy = '<font color=#000080>'
    character(len=20), parameter :: Olive = '<font color=#808000>'
    character(len=20), parameter :: Purple = '<font color=#800080>'
    character(len=20), parameter :: Red = '<font color=#FF0000>'
    character(len=20), parameter :: Silver = '<font color=#C0C0C0>'
    character(len=20), parameter :: Teal = '<font color=#008080>'
    character(len=20), parameter :: White = '<font color=#FFFFFF>'
    character(len=20), parameter :: Yellow = '<font color=#FFFF00>'
    character(len= 7), parameter :: black='</font>'
    character(len= 6), parameter :: nbsp='&nbsp;'

    character(len=19), parameter :: tdaligncenter= '<td align="center">', thaligncenter= '<th align="center">'
    character(len=18), parameter :: tdalignright= '<td align="right">', thalignright= '<th align="right">'
    character(len=17), parameter :: thalignleft= '<th align="left">'
    character(len= 4), parameter :: begintd= '<td>', beginth= '<th>', begintr= '<tr>'
    character(len= 5), parameter :: endtd= '</td>', endth= '</th>', endtr= '</tr>'
    character(len=15), parameter :: tdnbspendtd= '<td>&nbsp;</td>'

    character(len=20), parameter :: selected(0:1) = (/ '                    ', ' selected="selected"' /)
    character(len=14), parameter :: checked(0:1) = (/ '              ', ' checked="yes"' /)

    ! string lengths
    integer, parameter :: MAX_QUERY_STRING_LEN = 4096
    integer, parameter :: MAX_CGI_WRK_LEN = 2048
    integer, parameter :: MAX_CGI_INT_LEN = 10
    integer, parameter :: MAX_CGI_FLT_LEN = 12

    ! local work areas
    character(len=MAX_CGI_WRK_LEN), private :: cgi_wrk
    character(len=MAX_CGI_INT_LEN), private :: cgi_int
    character(len=MAX_CGI_FLT_LEN), private :: cgi_flt

    logical :: isRoleSRE = .false., isRoleChair = .false., isRoleAdmin = .false.
    logical :: isRoleStudent = .false., isRoleGuest = .false.
    character(len=40) :: defaultUSER

contains


    subroutine cgi_url_encode(str_in, str_out)
    ! encode special/reserved characters in str_in using hex representation

        character(len=*), intent (in) :: str_in
        character(len=*), intent (out) :: str_out

        character(len=1) :: ch
        integer :: code, i, idx_out, j, k, len_str_out
        logical :: encode

        str_out = ' ' ! default return values
        idx_out = 1 ! index to str_out
        len_str_out = len(str_out) ! dimension of str_out
        do i=1,len_trim(str_in)

            ! enough SPACE in str_out?
            ch = str_in(i:i) ! the character
            code = ichar(ch)   ! the code
            encode = .false. ! assume not to encode

            if (code<=31 .or. code>=127) encode = .true. ! 01-1F, 7F-FF

            if (index(SPECIAL,ch)/=0) encode = .true. ! special, unsafe, reserved

            if (ch==BSLASH .or. ch==PRIME) encode = .true. ! \, '

            if (ch==SPACE) then  ! encode as +
                str_out(idx_out:idx_out) = '+'

            elseif (encode) then ! encode as %hh
                j = 1 + code/16
                k = 1 + mod(code, 16)
                call check_array_bound (idx_out+2, len_str_out, 'cgi_url_encode ('//str_in//', ...)')
                str_out(idx_out:idx_out+2) = '%'//HEXDIGITS(j:j)//HEXDIGITS(k:k)
                idx_out = idx_out + 2 ! advance idx_out by 2

            else ! copy as is
                str_out(idx_out:idx_out) = ch
            end if

            idx_out = idx_out + 1 ! advance idx_out by 1
        end do

        return
    end subroutine cgi_url_encode


    subroutine cgi_url_decode (str_in, str_out)
    ! decode hexed characters in str_in into human readable format

        character(len=*), intent (in) :: str_in
        character(len=*), intent (out) :: str_out

        integer :: code, i, idx_in, idx_out, j, len_str_in, len_str_out
        character(len=1) :: ch
        character(len=2) :: hex_code

        str_out = ' ' ! default return values
        len_str_out = len(str_out) ! dimension of str_out
        idx_out = 1 ! index to str_out

        idx_in = 1 ! index to str_in
        len_str_in = len_trim(str_in) ! length of str_in
        do while (idx_in<=len_str_in)
            ch = str_in(idx_in:idx_in)

            if (ch=='+') then ! replace with blank
                str_out(idx_out:idx_out) = SPACE

            elseif (ch=='%') then ! %hh
                hex_code = str_in(idx_in+1:idx_in+2)
                i = index(HEXDIGITS,hex_code(1:1))-1
                j = index(HEXDIGITS,hex_code(2:2))-1
                code = 16*i+j
                call check_array_bound (idx_out, len_str_out, 'cgi_url_decode output')
                str_out(idx_out:idx_out) = char(code)
                idx_in = idx_in + 2

            else ! copy as is
                str_out(idx_out:idx_out) = ch
            end if
            idx_in = idx_in + 1
            idx_out = idx_out + 1

        end do

        return
    end subroutine cgi_url_decode


    subroutine cgi_get_name_value(string, lname, rvalue, ierr)
    ! parse [lname=][rvalue&] from string, rvalue will be string

        character(len=*), intent (in out) :: string
        character(len=*), intent (in) :: lname
        character(len=*), intent (out) :: rvalue
        integer, intent (out) :: ierr

        integer :: i_start, j_end, l_string, l_lname

        ! default return values
        rvalue = ' '
        ierr = 0
        l_string = len_trim(string)

        ! URL encode lname
        call cgi_url_encode(lname, cgi_wrk)
        l_lname = len_trim(cgi_wrk)

        ! encoded lname not in string?
        i_start = index(string, cgi_wrk(:l_lname)//'=')
        if (i_start==0) then ! not found
            ierr = -1
            return
        end if

        if (i_start+l_lname==l_string) return ! empty rvalue (i.e., nothing follows lname=)

        cgi_wrk = string(i_start+l_lname+1:) ! collect remainder after [lname=]

        j_end = index(cgi_wrk, '&') ! find &
        if (j_end==0) j_end = len_trim(cgi_wrk) + 1 ! assume & at end of string
        string(i_start:) = cgi_wrk(j_end+1:) ! update string
        rvalue = cgi_wrk(:j_end-1)
        !write(*,*) lname, '=', rvalue

        return
    end subroutine cgi_get_name_value


    subroutine cgi_get_named_string(string, lname, rvalue, ierr)
    ! parse [lname=][string_rvalue&] from string

        character(len=*), intent (in out) :: string
        character(len=*), intent (in) :: lname
        character(len=*), intent (out) :: rvalue
        integer, intent (out) :: ierr

        character(len=MAX_CGI_WRK_LEN) :: tmp

        call cgi_get_name_value(string, lname, tmp, ierr)
        if (ierr==-1) then ! not found
            rvalue = ' '
        else
            call cgi_url_decode(tmp, rvalue) ! decode
        end if

        return
    end subroutine cgi_get_named_string


    subroutine cgi_get_named_integer(string, lname, rvalue, ierr)
    ! parse [lname=][rvalue&] from string, rvalue will be integer

        character(len=*), intent (in out) :: string
        character(len=*), intent (in) :: lname
        integer, intent (out) :: rvalue
        integer, intent (out) :: ierr

        call cgi_get_name_value(string, lname, cgi_int, ierr)
        if (ierr==-1) then ! not found
            rvalue = -1
        else
            rvalue = atoi(cgi_int) ! convert
        end if

        return
    end subroutine cgi_get_named_integer


    subroutine cgi_get_named_float(string, lname, rvalue, ierr)

        ! parse [lname=][rvalue&] from string, rvalue will be float

        character(len=*), intent (in out) :: string
        character(len=*), intent (in) :: lname
        real, intent (out) :: rvalue
        integer, intent (out) :: ierr

        call cgi_get_name_value(string, lname, cgi_flt, ierr)
        if (ierr==-1) then ! not found
            rvalue = 0.0
        else
            rvalue = atof(cgi_flt) ! convert
        end if

        return
    end subroutine cgi_get_named_float



    subroutine cgi_get_wild_name_value(string, wild, lname, rvalue, ierr)
    ! find wild in string, then parse [wild//lname=][rvalue&]

        character(len=*), intent (in out) :: string
        character(len=*), intent (in) :: wild
        character(len=*), intent (out) :: lname, rvalue
        integer, intent (out) :: ierr

        integer :: w_start, i_start, j_end, l_string, l_lname, l_wild
        character(len=MAX_CGI_WRK_LEN) :: tmp

        ! default return values
        lname  = ' '
        rvalue = ' '
        ierr = 0
        l_string = len_trim(string)

        ! URL encode wild
        call cgi_url_encode(wild, cgi_wrk)

        ! encoded wild in string?
        l_wild = len_trim(cgi_wrk)
        w_start = index(string, cgi_wrk(:l_wild))
        if (w_start==0) then ! not found
            ierr = -1
            return
        end if

        ! yes; collect lname
        i_start = w_start+l_wild
        l_lname = 0
        do while (string(i_start+l_lname:i_start+l_lname) .ne. '=')
            l_lname = l_lname + 1
        end do
        tmp = string(i_start:i_start+l_lname-1)

        ! decode
        call cgi_url_decode(tmp, lname)

        if (i_start+l_lname==l_string) return ! empty rvalue (i.e., nothing follows lname=)

        cgi_wrk = string(i_start+l_lname+1:) ! collect remainder after [lname=]
        j_end = index(cgi_wrk, '&') ! find &
        if (j_end==0) j_end = len_trim(cgi_wrk) + 1 ! assume & at end of string

        string(w_start:) = cgi_wrk(j_end+1:) ! update string

        tmp = cgi_wrk(:j_end-1)
        call cgi_url_decode(tmp, rvalue) ! decode

        return
    end subroutine cgi_get_wild_name_value


    function cgi_make_href(fn, label, pre, post, A1, A2, A3, A4, A5, anchor, newtab)
    ! build HTML href, like:
    !   pre<a href="CGI_PATH?F=fn&A1=A1&...A5=A5 #anchor target=newtab">label</a>post

        integer, intent (in) :: fn
        character(len=*), intent (in) :: label
        character(len=*), intent (in), optional :: A1, A2, A3, A4, A5
        character(len=*), intent (in), optional :: pre, post, anchor, newtab

        character(len=MAX_QUERY_STRING_LEN) :: cgi_make_href, tmp

        real :: harvest

        ! a random token
        call random_number(harvest)
        tmp = 'T='//trim(itoa(int(1.0E5*harvest)))

        ! the arguments to the function
        if (present(A1)) then
            call cgi_url_encode(A1,cgi_wrk)
            tmp = trim(tmp)//'&A1='//cgi_wrk
        end if
        if (present(A2)) then
            call cgi_url_encode(A2,cgi_wrk)
            tmp = trim(tmp)//'&A2='//cgi_wrk
        end if
        if (present(A3)) then
            call cgi_url_encode(A3,cgi_wrk)
            tmp = trim(tmp)//'&A3='//cgi_wrk
        end if
        if (present(A4)) then
            call cgi_url_encode(A4,cgi_wrk)
            tmp = trim(tmp)//'&A4='//cgi_wrk
        end if
        if (present(A5)) then
            call cgi_url_encode(A5,cgi_wrk)
            tmp = trim(tmp)//'&A5='//cgi_wrk
        end if

        ! the function
        tmp = trim(tmp)//'&F='//itoa(fn)

        ! begin href
        tmp = '<a href="'//CGI_PATH//'?'//tmp

        ! preamble (text before href)
        if (present(pre)) tmp = pre//tmp

        ! the anchor
        if (present(anchor)) tmp = trim(tmp)//'#'//anchor

        ! the target
        if (present(newtab)) tmp = trim(tmp)//' target='//newtab

        ! end href & the label
        tmp = trim(tmp)//'">'//trim(label)//'</a>'

        ! the text after the href
        if (present(post)) tmp = trim(tmp)//post

        cgi_make_href = tmp

        return
    end function cgi_make_href


end module CGI

