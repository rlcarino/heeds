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


module UTILITIES

    implicit none

!===========================================================
! the software
!===========================================================

    character(len= 7), parameter :: VERSION   = ' v.4.20'
    character(len= 5), parameter :: PROGNAME  = 'HEEDS'
    character(len=45), parameter :: COPYRIGHT = 'Copyright (C) 2012, 2013 Ricolindo L. Carino'
    character(len=38), parameter :: EMAIL     = 'Ricolindo.Carino@AcademicForecasts.com'
    character(len=72), parameter :: CONTACT   = 'E-mail inquiries about '//PROGNAME//' to '//EMAIL//'.'
    character(len=32), parameter :: WEB       = 'http://code.google.com/p/heeds/'

!===========================================================
! the functionality
!===========================================================
    
    character(len= 9), parameter :: ACTION    = 'Schedules'

    logical :: advisingPeriod = .false., &
        isActionClasslists = .false., &
        isActionAdvising = .false.


!===========================================================
! OS-related parameters
!===========================================================
#if defined GLNX
    ! file separator; delete, directory, mkdir commands
    character(len= 6), parameter :: delCmd = 'rm -f '
    character(len= 9), parameter :: mkdirCmd = 'mkdir -p '
    character(len= 6), parameter :: mvCmd = 'mv -f '
    character(len= 1), parameter :: DIRSEP = '/'
#else
    ! file separator; delete, directory, mkdir commands
    character(len= 7), parameter :: delCmd = 'del /q '
    character(len= 6), parameter :: mkdirCmd = 'mkdir '
    character(len= 8), parameter :: mvCmd = 'move /y '
    character(len= 1), parameter :: DIRSEP = '\'
#endif


!===========================================================
! passwords
!===========================================================
    integer, parameter :: &  ! length of password variables
        MAX_LEN_PASSWD_VAR=32, &
        MIN_LEN_PASSWORD=8, &
        MAX_LEN_PASSWORD=12, &
        lenPasswordEncryptionKey = 16
    character(len=lenPasswordEncryptionKey) :: &
                          passwordEncryptionKey = 'w!thUr0wnr3pL@c3'


!===========================================================
! Academic Year, Term, times
!===========================================================

    integer :: currentYear ! year of start of Academic Year
    integer :: currentTerm ! current term 1=1st sem, 2=2nd sem; 3=summer
    integer :: nextYear, nextTerm, targetTerm, termBegin, termEnd
    integer :: cTm1Year, cTm1, cTm2Year, cTm2, cTm3Year, cTm3
    character(len=10) :: currentTime ! current time
    character(len= 8) :: currentDate ! current date
    character(len=18) :: startDateTime ! program start date & time


!===========================================================
! file paths
!===========================================================

    integer, parameter :: MAX_LEN_FILE_PATH = 256 ! Max length of file path+name
    integer, parameter :: &
        unitHTML = 999, &   ! file unit for HTML to webserver
        unitUSER = 998, &   ! file unit for user activities
        unitXML  = 997, &   ! file unit for XML input/output
        unitRAW  = 996, &   ! file unit for custom inputs
        unitLOG  = 995, &   ! file unit for log messages
        unitREQ  = 994, &   ! file unit for requests
        unitETC  = 993

    ! data & output paths
    character (len=MAX_LEN_FILE_PATH) :: &
        dirUNIV, & ! HEEDS root directory
        dirBACKUP, & ! directory for backup files
        dirDATA, & ! directory for XML data files
        dirLOG, &  ! directory for log files
!        dirSUBSTITUTIONS, & ! directory for subject substitutions
!        dirTRANSCRIPTS, & ! directory for individual enrollment records
        pathToYear, pathToNextYear, pathToTerm, &  ! path data files for the year, next year
        fileEXE, & ! name of executable
        fileLOG, & ! name of log file
        fileREQ, & ! name of file for requests
        CGI_PATH ! URI

    ! position of last character in dirDATA (to simplify derivation of path to backup)
    integer :: lenDirDAT

    logical :: hasCATALOG = .false.

    ! flag to control generation of log files, backups
    logical :: noWrites = .false. ! .true. means do not change data files
    logical :: isSuspended = .false.  ! only REGISTRAR role can work

    ! constants
    character(len= 1), parameter :: &
        SPACE = ' ', COMMA = ',', DASH ='-', FSLASH = '/', BSLASH = '\', PRIME = '''', DOT = '.'
    character(len= 1), parameter :: NUL = achar(0)
    character(len= 2), parameter :: CRLF = achar(10)//achar(13)
    character(len= 3), parameter :: AFORMAT = '(a)'
    character(len= 8), parameter :: ZFORMAT = '(16z0.2)'
    character(len=10), parameter :: DECDIGITS = '0123456789'
    character(len=16), parameter :: HEXDIGITS = '0123456789ABCDEF'
    character(len=24), parameter :: SPECIAL = '<>"#%{}|^~[]`;/?:=&$+().'

    ! some HTML colors
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

!===========================================================
! XML-related parameters
!===========================================================

    ! maximum line size in an XML file
    integer, parameter :: MAX_LEN_XML_LINE = 1000

    ! maximum characters in a tag
    integer, parameter :: MAX_LEN_XML_TAG = 40

    ! root names                                                 12345678901234567890
    character(len=10), parameter :: XML_ROOT_UNIVERSITY     = 'UNIVERSITY'
    character(len=16), parameter :: XML_ROOT_COLLEGES       = 'LIST_OF_COLLEGES'
    character(len=19), parameter :: XML_ROOT_DEPARTMENTS    = 'LIST_OF_DEPARTMENTS'
    character(len=16), parameter :: XML_ROOT_SUBJECTS       = 'LIST_OF_SUBJECTS'
    character(len=13), parameter :: XML_ROOT_ROOMS          = 'LIST_OF_ROOMS'
    character(len=16), parameter :: XML_ROOT_TEACHERS       = 'LIST_OF_TEACHERS'
    character(len=16), parameter :: XML_ROOT_SECTIONS       = 'LIST_OF_SECTIONS'
    character(len=17), parameter :: XML_ROOT_CURRICULA      = 'LIST_OF_CURRICULA'
    character(len=21), parameter :: XML_ROOT_EQUIVALENCIES  = 'LIST_OF_EQUIVALENCIES'
    character(len=14), parameter :: XML_ROOT_BLOCKS         = 'LIST_OF_BLOCKS'
    character(len=16), parameter :: XML_ROOT_STUDENTS       = 'LIST_OF_STUDENTS'
    character(len=11), parameter :: XML_ROOT_PREDICTIONS    = 'PREDICTIONS'
    character(len=11), parameter :: XML_ROOT_WAIVERS        = 'WAIVERS_COI'
    character(len=10), parameter :: XML_ROOT_ENLISTMENT     = 'ENLISTMENT'
    character(len=11), parameter :: XML_ROOT_GRADESHEETS    = 'GRADESHEETS'
    character(len=14), parameter :: XML_ROOT_STUDENT_RECORD = 'STUDENT_RECORD'
    character(len= 9), parameter :: XML_ROOT_FAILRATES      = 'FAILRATES'
    character(len=15), parameter :: XML_ROOT_INTAKE         = 'FRESHMAN_INTAKE'
    character(len=13), parameter :: XML_ROOT_SUBSTITUTIONS  = 'SUBSTITUTIONS'
    ! root names                                                 12345678901234567890
    character(len=44), parameter :: XML_DOC = &
        '<?xml version="1.0" encoding="ISO-8859-1" ?>'

    ! indentation
    integer, parameter :: INDENT_INCR = 4 ! no. SPACEs for next indent
    integer             :: indent0 = INDENT_INCR, & ! indentation levels
        indent1 = INDENT_INCR*2, &
        indent2 = INDENT_INCR*3, &
        indent3 = INDENT_INCR*4, &
        indent4 = INDENT_INCR*5, &
        indent5 = INDENT_INCR*6
    character(len=80)  :: indentation = ' '


!===========================================================
! CGI-related parameters
!===========================================================

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
    integer, parameter :: MAX_LEN_QUERY_STRING = 4096
    integer, parameter :: MAX_CGI_WRK_LEN = 2048
    integer, parameter :: MAX_CGI_INT_LEN = 10
    integer, parameter :: MAX_CGI_FLT_LEN = 12

    ! the script
    character (len=MAX_LEN_FILE_PATH)    :: DOCUMENT_URI

    ! the IP address xxx.xxx.xxx.xxx
    character (len=16)                   :: REMOTE_ADDR

    ! the QUERY, encryption key, cipher text
    character (len=MAX_LEN_QUERY_STRING) :: QUERY_STRING, queryEncryptionKey, cipher

    ! login check message, user role
    character(len=MAX_LEN_FILE_PATH) :: loginCheckMessage
    logical :: isRoleSRE = .false., &
        isRoleChair = .false., &
        isRoleAdmin = .false., &
        isRoleTeacher = .false., &
        isRoleStudent = .false., &
        isRoleGuest = .false.

    ! local work areas
    character(len=MAX_CGI_WRK_LEN), private :: cgi_wrk
    character(len=MAX_CGI_INT_LEN), private :: cgi_int
    character(len=MAX_CGI_FLT_LEN), private :: cgi_flt


contains

!===========================================================
! Diagnostic messages
!===========================================================

    subroutine usage(mesg)

        character(len=*), intent(in), optional :: mesg

        ! fileEXE is set in the main program
        write(*,AFORMAT) trim(fileEXE)//' ('//VERSION//')'
        if (present(mesg)) write(*,AFORMAT) trim(mesg)
#if defined GLNX
        write(*,AFORMAT) &
            'Webserver usage: spawn-fcgi -a IP_ADDRESS -p PORT_NUM -- '//trim(fileEXE)//' UNIV YEAR TERM'
#else
        write(*,AFORMAT) &
            'Webserver usage: spawn-fcgi -a IP_ADDRESS -p PORT_NUM -f "'//trim(fileEXE)//' UNIV YEAR TERM"'
#endif
        write(*,AFORMAT) &
            '  IP_ADDRESS, PORT_NUM - as specified by fastcgi_pass in nginx configuration', &
            '  UNIV - code for university', &
            '  YEAR - chronological year when the current School Year started', &
            '  TERM - current term: 1=1st Sem, 2=2nd Sem, S=summer', &
            SPACE
         stop

    end subroutine usage



    subroutine check_array_bound(current, limit, msg)
        character (len=*), intent (in) :: msg
        integer, intent (in) :: current, limit

        if (current > limit) then
            call log_comment('Aborting due to insufficient array size; increase '//msg, &
                'Limit is '//itoa(limit)//'; currently used is '//itoa(current) )
            stop
        end if

    end subroutine check_array_bound



    subroutine log_comment(mesg1, mesg2, mesg3, mesg4, mesg5)
        character (len=*), intent (in) :: mesg1
        character (len=*), intent (in), optional :: mesg2, mesg3, mesg4, mesg5

        logical :: logExists

        fileLOG = trim(dirLOG)//trim(fileEXE)//DASH//currentDate//'.log'
        inquire(file=trim(fileLOG), exist=logExists)
        if (.not. logExists) then
            open(unit=unitLOG, file=trim(fileLOG), status='new')
        else
            open(unit=unitLOG, file=trim(fileLOG), status='old', position='append')
        end if
        write(unitLOG,AFORMAT) trim(mesg1)
        if (present(mesg2)) then
            write(unitLOG,AFORMAT) trim(mesg2)
            if (present(mesg3)) then
                write(unitLOG,AFORMAT) trim(mesg3)
                if (present(mesg4)) then
                    write(unitLOG,AFORMAT) trim(mesg4)
                    if (present(mesg5)) then
                        write(unitLOG,AFORMAT) trim(mesg5)
                    end if
                end if
            end if
        end if
        close(unitLOG)

    end subroutine log_comment



    subroutine log_request(fileNo, fileName, mesg1, mesg2, mesg3, mesg4, mesg5)
        integer, intent (in) :: fileNo
        character (len=*), intent (in) :: fileName, mesg1
        character (len=*), intent (in), optional :: mesg2, mesg3, mesg4, mesg5

        logical :: logExists

        inquire(file=fileName, exist=logExists)
        if (.not. logExists) then
            open(unit=fileNo, file=fileName, status='new')
        else
            open(unit=fileNo, file=fileName, status='old', position='append')
        end if
        write(fileNo,AFORMAT) trim(mesg1)
        if (present(mesg2)) then
            write(fileNo,AFORMAT) trim(mesg2)
            if (present(mesg3)) then
                write(fileNo,AFORMAT) trim(mesg3)
                if (present(mesg4)) then
                    write(fileNo,AFORMAT) trim(mesg4)
                    if (present(mesg5)) then
                        write(fileNo,AFORMAT) trim(mesg5)
                    end if
                end if
            end if
        end if
        close(fileNo)

    end subroutine log_request


    subroutine html_comment(str1, str2, str3, str4, str5)
        character(len=*), intent(in) :: str1
        character(len=*), intent(in), optional :: str2, str3, str4, str5

#if defined PRODUCTION
#else
        if (present(str2)) then
            write(unitHTML,AFORMAT) '<!--', trim(str1), trim(str2)
            if (present(str3)) then
                write(unitHTML,AFORMAT) trim(str3)
                if (present(str4)) then
                    write(unitHTML,AFORMAT) trim(str4)
                    if (present(str5)) then
                        write(unitHTML,AFORMAT) trim(str5)
                    end if
                end if
            end if
            write(unitHTML,AFORMAT) '--> '
        else
            write(unitHTML,AFORMAT) '<!-- '//trim(str1)//' -->'
        end if
#endif

    end subroutine html_comment



    subroutine terminate(msg)

        character(len=*), intent (in) :: msg

        call log_comment(msg, 'Ends '//currentDate//DASH//currentTime )

        stop
    end subroutine terminate


!===========================================================
! (weak) encryption routines
!===========================================================

    subroutine initialize_random_seed()

        integer :: i, n, clock
        integer, dimension(:), allocatable :: seed

        ! initialize the random seed based on the system's time.
        ! (example from http://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html#RANDOM_005fSEED)
        call random_seed(size = n)
        allocate(seed(n))

        call system_clock(count=clock)

        seed = clock + 37 * (/ (i - 1, i = 1, n) /)
        call random_seed(put = seed)

        deallocate(seed)

    end subroutine initialize_random_seed



    subroutine encrypt(key, text)
        character(len=*), intent (in) :: key
        character(len=*), intent (inout) :: text
        integer :: i, j, k, lenKey, lenText
        lenKey=len_trim(key)
        lenText=len_trim(text)
        i = lenKey
        do j=lenText,1,-1
            k = 2*j
            write(text(k-1:k),'(z0.2)') char(ieor(ichar(key(i:i)),ichar(text(j:j))))
            i = i-1
            if (i==0) i = lenKey
        end do

    end subroutine encrypt



    subroutine decrypt(key, text)
        character(len=*), intent (in) :: key
        character(len=*), intent (in out) :: text ! in=cipher, out=plaintext
        integer :: i, j, k, lenKey, lenText, intText

        call html_comment('decrypt('//trim(text)//')')

        lenKey = len_trim(key)
        lenText = len_trim(text)/2
        i = lenKey
        do j=lenText,1,-1
            k = 2*j
            read(text(k-1:k), '(z2)') intText
            text(j+lenText:j+lenText) = char( ieor(ichar(key(i:i)),intText) )
            i = i-1
            if (i==0) i = lenKey
        end do
        text = text(lenText+1:) ! move to front

    end subroutine decrypt


    subroutine set_password(Password, forcedPassword)
        character (len=MAX_LEN_PASSWD_VAR), intent (in out) :: Password
        character (len=*), intent (in), optional :: forcedPassword
        integer :: i, j, lenP, lenS
        real :: harvest ! random number
        character (len=34) :: choice = 'A2B3D45F6G7H8J9bcdfghjkmnprstvwxyz'

        if (present(forcedPassword)) then
            Password = forcedPassword
            lenP = len_trim(Password)
            if (lenP>MAX_LEN_PASSWORD) then ! force to be at most MAX_LEN_PASSWORD characters
                lenP = MAX_LEN_PASSWORD
                Password(lenP+1:) = SPACE
            end if
        else ! generate password with MIN_LEN_PASSWORD to MAX_LEN_PASSWORD characters
            call random_number(harvest)
            lenP = MIN_LEN_PASSWORD + int(harvest*(MAX_LEN_PASSWORD-MIN_LEN_PASSWORD+1))
            do i=1,lenP
                call random_number(harvest)
                j = 1 + int(harvest*34)
                Password(i:i) = choice(j:j)
            end do
        end if
        ! add salt
        lenS = lenPasswordEncryptionKey-lenP ! length of salt
        Password(lenS+1:) = Password(:lenP) ! right-justify Password() to make space for salt
        do i=1,lenS ! generate random salt
            call random_number(harvest)
            j = 1 + int(harvest*35)
            Password(i:i) = choice(j:j)
        end do
        ! add length to beginning+1
        Password(3:4) = itoa2bz(lenP)
        !write(*,*) lenS, Password(:lenS), lenP, Password(lenS+1:)!, lenS+lenP, Password
        ! encrypt salted password
        call encrypt(passwordEncryptionKey, Password)

    end subroutine set_password


!===========================================================
! text-numeric and character conversions
!===========================================================

    subroutine blank_to_underscore (inString, outString)
        character(len=*), intent(in) :: inString
        character(len=*), intent(out) :: outString
        integer :: i, l
        l = len_trim(inString)
        outString = inString
        do i=1,l
            if (outString(i:i)==SPACE) outString(i:i) = '_'
        end do

    end subroutine blank_to_underscore



    subroutine underscore_to_blank (inString, outString)
        character(len=*), intent(in) :: inString
        character(len=*), intent(out) :: outString
        integer :: i, l
        l = len_trim(inString)
        outString = inString
        do i=1,l
            if (outString(i:i)=='_') outString(i:i) = SPACE
        end do

    end subroutine underscore_to_blank



    subroutine upper_case(string)
        ! change string to upper case
        character(len=*), intent (inout) :: string
        integer :: i,length
        length=len_trim(string)
        do i=1,length
            if (string(i:i) .lt. 'a') cycle
            if (string(i:i) .gt. 'z') cycle
            string(i:i) = char(ichar(string(i:i))-32)
        end do

    end subroutine upper_case



    subroutine lower_case(string)
        ! change string to lower case
        character(len=*), intent (inout) :: string
        integer :: i,length
        length=len_trim(string)
        do i=1,length
            if (string(i:i) .lt. 'A') cycle
            if (string(i:i) .gt. 'Z') cycle
            string(i:i) = char(ichar(string(i:i))+32)
        end do

    end subroutine lower_case



    function atoi(inString)
        ! extract integer from string
        integer :: atoi
        character (len=*), intent (in) :: inString
        integer :: i, j, num, ll, start, pref
        character (len=20) :: string
        num = 0
        pref = 1
        string = adjustl(inString)
        ll = len_trim(string)
        if (ll > 0) then
            if (string(1:1) == DASH) then
                start = 2
                pref = -1
            else
                start = 1
                pref = 1
            end if
            do i=start,ll
                j = index(DECDIGITS, string(i:i))
                if (j > 0) then
                    num = 10*num + j - 1
                else
                    exit ! on first character that is not a decimal digit
                end if
            end do
        end if
        atoi = num*pref

    end function atoi



    function atof(inString)
        ! extract float from string
        real :: atof
        character (len=*), intent (in) :: inString
        integer :: i, j, ll, start
        real :: num, pref, mult
        character (len=20) :: string
        num = 0.0
        mult = 0.0
        pref = 1.0
        string = adjustl(inString)
        ll = len_trim(string)
        if (ll > 0) then
            if (string(1:1) == DASH) then
                start = 2
                pref = -1.0
            else
                start = 1
                pref = 1.0
            end if
            do i=start,ll
                j = index(DECDIGITS, string(i:i))
                if (j > 0) then
                    num = 10.0*num + 1.0*j - 1.0
                    mult = mult*0.1
                elseif (string(i:i)=='.') then ! decimal point
                    if (mult==0.0) then ! first decimal point
                        mult = 1.0
                    else
                        exit ! second decimal point?
                    end if
                else
                    exit ! on first character that is not a digit
                end if
            end do
        end if
        if (mult>0.0) then
            atof = mult*num*pref
        else
            atof = num*pref
        end if

    end function atof


  
    function itoa(num)
        ! convert num to string
        character (len=10) :: itoa
        integer, intent (in) :: num
        character (len=10) :: str10
        integer :: j, k
        str10 = SPACE
        k = abs(num)
        do
            j = mod(k,10)+1
            k = k/10
            str10 = DECDIGITS(j:j)//str10
            if (k == 0) exit
        end do
        if (num < 0) str10 = DASH//str10
        itoa = str10

    end function itoa



    function ftoa(num, dadp)
        ! convert positive num to string

        character (len=10) :: ftoa
        real, intent (in) :: num
        integer, intent (in), optional :: dadp ! digits after the decimal point

        character (len=10) :: str10
        real :: frac
        integer :: i, j, k, l

        k = int(abs(num))
        frac = abs(num) - k
        str10 = itoa(k) ! integral part
        l = len_trim(str10)+1 ! position of decimal point
        str10(l:l) = '.' ! add decimal point
        i = 0
        do
            l = l + 1
            i = i + 1
            j = frac*10.0 ! shift decimal pt to the right
            str10(l:l) = DECDIGITS(j+1:j+1)
            frac = frac*10.0 - j ! remainder
            if (frac==0.0 .or. i==dadp .or. l==10) exit
        end do
        if (l<10 .and. str10(l:l)=='.') str10(l+1:l+1) = '0' ! add 0 after decimal point
        if (num < 0) str10 = DASH//str10
        ftoa = str10

    end function ftoa



    function itoabz(num)
        character (len=10) :: itoabz
        integer, intent (in) :: num
        if (num == 0) then
            itoabz = ' .'
        else
            itoabz = itoa(num)
        end if

    end function itoabz

  

    function itoa2bz(num)
        character (len=2) :: itoa2bz
        integer, intent (in) :: num
        character (len=2) :: str2
        integer :: j, k, l
        if (num > 99) then
            str2 = '**'
        else
            str2 = '00'
            k = num
            l = 2
            do
                j = mod(k,10)+1
                k = k/10
                str2(l:l) = DECDIGITS(j:j)
                if (k == 0) exit
                l = l-1
            end do
        end if
        itoa2bz = str2

    end function itoa2bz



    function itoa3bz(num)
        character (len=3) :: itoa3bz
        integer, intent (in) :: num
        character (len=3) :: str3
        integer :: j, k, l
        if (num <= 0) then
            str3 = '  .'
        else if (num > 999) then
            str3 = '***'
        else
            str3 = '000'
            k = num
            l = 3
            do
                j = mod(k,10)+1
                k = k/10
                str3(l:l) = DECDIGITS(j:j)
                if (k == 0) exit
                l = l-1
            end do
        end if
        itoa3bz = str3

    end function itoa3bz


!===========================================================
! search delimiter in a string
!===========================================================

    subroutine index_to_delimiters(symbol, string, nsymbols, pos)
        character (len=*), intent (in) :: symbol
        character (len=*), intent (in) :: string
        integer, intent (out) :: nsymbols
        integer, dimension(:), intent (out) :: pos
        integer :: j, k, l
        nsymbols = 1
        pos = 0
        k = len_trim(string)
        l = len_trim(symbol)
        do j=1,k
            if (string(j:j+l-1) == symbol) then
                nsymbols = nsymbols+1
                pos(nsymbols) = j
            end if
        end do
        pos(nsymbols+1) = k+1

    end subroutine index_to_delimiters



!===========================================================
! IO-related routines
!===========================================================


    subroutine make_directory(dirName, clean)
        character(len=*), intent (in) :: dirName
        logical, intent (in), optional :: clean
        logical :: pathExists
        integer :: ierr

        inquire(file=trim(dirName), exist=pathExists)
        if (.not. pathExists) then
            call system (mkdirCmd//trim(dirName), ierr)
        else if (present(clean)) then
            call system(delCmd//trim(dirName)//'*', ierr)
        end if

    end subroutine make_directory



    subroutine move_to_backup(fname)
        character (len=*), intent (in) :: fname ! must be in dirDATA
        character (len=MAX_LEN_FILE_PATH) :: path
        integer :: iStat

        if (noWrites) return ! no backups

        path = trim(dirBACKUP)//fname(lenDirDAT+1:)
        call rename (fname, path, iStat)
        if (iStat/=0) call log_comment('Status='//trim(itoa(iStat))//' in moving to '//trim(path) )

    end subroutine move_to_backup



!===========================================================
! XML-related routines
!===========================================================

    subroutine xml_read_file(device, rootName, fileName, errNo)
        integer, intent (in) :: device
        character (len=*), intent (in) :: fileName, rootName
        integer, intent (in out) :: errNo
        character(len=MAX_LEN_XML_LINE) :: xmlLine
        logical :: found
        integer :: eof

        errNo = -1
        inquire(file=fileName, exist=found)
        if (.not. found) return ! not there

        ! open & look for rootName in file
        open (unit=device, file=fileName, status='old', iostat=eof)
        do
            read(device, AFORMAT, iostat=eof) xmlLine
            if (eof<0) exit
            if (index(xmlLine, '<'//rootName//'>')==0) cycle
            errNo = 0
            exit
        end do

    end subroutine xml_read_file



    subroutine xml_write_character(device, indent, tag, value)
        integer, intent (in) :: device, indent
        character (len=*), intent (in) :: tag
        character (len=*), intent (in), optional :: value
        character(len=MAX_LEN_XML_LINE) :: xmlLine
        integer :: idx ! position of ' & ' in value

        if (present(value)) then ! convert ' & ' to ' and '
            xmlLine = value
            idx = index(xmlLine, ' & ')
            do while (idx>0)
                xmlLine = xmlLine(:idx)//'and'//xmlLine(idx+2:)
                idx = index(xmlLine, ' & ')
            end do
            write(device, AFORMAT) indentation(:indent)// &
                '<'//trim(tag)//'>'//trim(xmlLine)//'</'//trim(tag)//'>'
        else
            write(device, AFORMAT) indentation(:indent)//'<'//trim(tag)//'>'
        end if

    end subroutine xml_write_character



    subroutine xml_write_integer(device, indent, tag, value)
        integer, intent (in) :: device, indent
        character (len=*), intent (in) :: tag
        integer, intent (in) :: value
        call xml_write_character(device, indent, tag, itoa(value))

    end subroutine xml_write_integer



    subroutine xml_write_float(device, indent, tag, value, dadp)
        integer, intent (in) :: device, indent
        character (len=*), intent (in) :: tag
        real, intent (in) :: value
        integer, intent (in), optional :: dadp

        call xml_write_character(device, indent, tag, ftoa(value,dadp))

    end subroutine xml_write_float



    subroutine xml_parse_line(line, tag, value, errNo)
        character(len=MAX_LEN_XML_LINE), intent (in out) :: line
        character(len=MAX_LEN_XML_TAG), intent (out) :: tag
        integer, intent (in out) :: errNo
        character(len=MAX_LEN_XML_LINE), intent (out) :: value
        ! locals
        integer :: lenLine ! last non-blank character in line
        integer :: nL, nR, pos(10,2) ! number & positions of < and >
        integer :: i

        ! initialize return values
        line = adjustl(line)
        tag = SPACE
        value = SPACE

        ! get positions of < and >
        lenLine = len_trim(line)
        nL = 0
        nR = 0
        pos = 0
        do i = 1,lenLine
            if (line(i:i)=='<') then
                nL = nL+1
                pos(nL,1) = i
            elseif (line(i:i)=='>') then
                nR = nR+1
                pos(nR,2) = i
            end if
        end do

        ! any < or > ?
        if (nL==0 .and. nR==0) return

        ! same number of < and > ?
        if (nL/=nR) then
            call log_comment('Unmatched < or > in line '//trim(line))
            errNo = 701
            return
        end if

        ! assume line has <tag> only, or </tag> only, or <tag>value</tag>
        tag = line(pos(1,1)+1:pos(1,2)-1)
        if (nL>1) then ! <tag>value</tag>
            value = line(pos(1,2)+1:pos(2,1)-1)
        end if

    end subroutine xml_parse_line



!===========================================================
! CGI-routines
!===========================================================

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

        call html_comment('cgi_url_decode() = '//str_out)

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

        call html_comment('cgi_get_name_value() : '//lname//'='//trim(rvalue)//', ierr='//itoa(ierr))

    end subroutine cgi_get_name_value



    subroutine cgi_get_named_string(string, lname, rvalue, ierr)
    ! parse [lname=][string_rvalue&] from string

        character(len=*), intent (in out) :: string
        character(len=*), intent (in) :: lname
        character(len=*), intent (out) :: rvalue
        integer, intent (out) :: ierr

        character(len=MAX_CGI_WRK_LEN) :: tmpIN, tmpOUT

        call cgi_get_name_value(string, lname, tmpIN, ierr)
        if (ierr==-1) then ! not found
            rvalue = ' '
        else
            call cgi_url_decode(tmpIN, tmpOUT) ! decode
            rvalue = trim(tmpOUT)
        end if

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
        call html_comment('cgi_get_named_integer() : '//lname//'='//itoa(rvalue)//', ierr='//itoa(ierr))

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

        call html_comment('cgi_get_wild_name_value() : '//wild//lname//'= '//rvalue)

    end subroutine cgi_get_wild_name_value


end module UTILITIES

