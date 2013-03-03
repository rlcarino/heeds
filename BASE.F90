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


module BASE

    implicit none

    ! Pasword encryption key (16 characters)                 1234567890123456
    character(len=16), parameter :: passwordEncryptionKey = 'r3pL@c3w!thUr0wn'

    ! OS-specific variables (set in MAIN.F90)
#if defined GLNX
    ! HEEDS root directory and CGI script
    character(len=18) :: dirHEEDS ! = '/home/heeds/HEEDS/'
    ! file separator; delete, directory, mkdir commands
    character(len= 6) :: delCmd ! = 'rm -f '
    character(len= 9) :: mkdirCmd ! = 'mkdir -p '
    character(len= 6) :: mvCmd ! = 'mv -f '
#else
    ! HEEDS root directory and CGI script
    character(len= 7) :: dirHEEDS ! = '\HEEDS\'
    ! file separator; delete, directory, mkdir commands
    character(len= 7) :: delCmd ! = 'del /q '
    character(len= 6) :: mkdirCmd ! = 'mkdir '
    character(len= 8) :: mvCmd ! = 'move /y '
#endif

    character(len= 1) :: DIRSEP ! = '/' or '\'
    character(len= 8) :: UPDATES ! = 'UPDATES/' or 'UPDATES\'

    ! CGI script
    character(len= 6), parameter :: CGI_PATH = '/heeds'

    ! flag to control generation of backups
    logical, parameter :: DO_NOT_BACKUP = .false. ! .false. ! create backups?

    integer :: currentYear ! year of start of Academic Year
    integer :: currentTerm ! current term 1=1st sem, 2=2nd sem; 3=summer
    integer :: nextYear, nextTerm, targetTerm, targetYear
    integer :: prevTermYear, prevTermTerm, prevYearYear, prevYearTerm

    character(len=10) :: currentTime ! current time
    character(len= 8) :: currentDate ! current date
    character(len=18) :: startDateTime ! program start date & time

    integer, parameter :: MAX_LEN_FILE_PATH = 256 ! Max length of file path+name
    integer :: unitHTML = 999   ! file unit for HTML to webserver
    integer :: unitUSER = 998   ! file unit for user activities
    integer :: unitXML  = 997   ! file unit for XML input/output
    integer :: unitRAW  = 996   ! file unit for custom inputs
    integer :: unitLOG  = 995   ! file unit for log messages
    integer :: unitREQ  = 994   ! file unit for requests
    integer :: unitETC  = 993   ! file unit for requests

    ! flag to control generation of log files, backups
    logical :: noWrites = .false. ! .true. means do not change data files

    ! data & output locations
    character (len=MAX_LEN_FILE_PATH) :: &
        fileExecutable, & ! name of executable
        dirWWW, & ! directory where web pages will be served (must be writable by HEEDS)
        dirBak, & ! directory for backup files
        dirLog, & ! directory for log files
        dirSUBSTITUTIONS, & ! directory for input/UNEDITED checklists from Registrar
        dirTRANSCRIPTS, & ! directory for raw transcripts
        dirEditedCHECKLISTS, & ! directory for output/EDITED checklists from College Secretaries
        dirRAW, & ! directory for raw data files
        dirXML, & ! directory for XML data files
        pathToYear, &  ! path data files for the year
        pathToTerm, &  ! path to updated files by stand-alone users
        pathToCurrent, & ! path to files for currentYear+currentTerm
        pathToTarget, &  ! path to files for targetYear+targetTerm
        pathToSOURCE, &  ! pathToCurrent or pathToTarget
        pathToUPDATES    ! path to files of changes by stand-alone users

    ! position of last character in dirXML (to simplify derivation of path to backup)
    integer :: lenDirXML

    ! constants
    character(len= 1), parameter :: &
        SPACE = ' ', COMMA = ',', DASH ='-', FSLASH = '/', BSLASH = '\', PRIME = ''''
    character(len= 1), parameter :: NUL = achar(0)
    character(len= 2), parameter :: CRLF = achar(10)//achar(13)
    character(len= 3), parameter :: AFORMAT = '(a)'
    character(len= 8), parameter :: ZFORMAT = '(16z0.2)'
    character(len=10), parameter :: DECDIGITS = '0123456789'
    character(len=16), parameter :: HEXDIGITS = '0123456789ABCDEF'
    character(len=24), parameter :: SPECIAL = '<>"#%{}|^~[]`;/?:=&$+().'

    ! software version
    character(len= 5), parameter :: PROGNAME  = 'HEEDS'
    character(len= 8), parameter :: VERSION   = ' v.4.04 '
    character(len=45), parameter :: COPYRIGHT = 'Copyright (C) 2012, 2013 Ricolindo L. Carino'
    character(len=38), parameter :: EMAIL     = 'Ricolindo.Carino@AcademicForecasts.com'
    character(len=72), parameter :: CONTACT   = 'E-mail inquiries about '//PROGNAME//' to '//EMAIL//'.'
    character(len=32), parameter :: WEB       = 'http://code.google.com/p/heeds/'

    ! University name
    integer, parameter :: &
        MAX_LEN_UNIVERSITY_CODE=20, & ! length of college codes
        MAX_LEN_UNIVERSITY_NAME=60, & ! length of college names
        MAX_LEN_COLLEGE_CODE=10, & ! length of college codes
        MAX_LEN_DEPARTMENT_CODE=10 ! length of dept codes
    character (len= MAX_LEN_UNIVERSITY_CODE) :: UniversityCode = SPACE
    character (len=MAX_LEN_UNIVERSITY_NAME) :: &
        UniversityName = '(Specify NAME in UNIVERSITY.XML)', &
        UniversityAddress = '(Specify ADDRESS in UNIVERSITY.XML)', &
        UniversityPresident = 'Firstname MI LastName, PhD', &
        VPAcademicAffairs = 'Firstname MI LastName, PhD', &
        DeanOfCampus = 'Firstname MI LastName, PhD', &
        DeanOfInstruction = 'Firstname MI LastName, PhD'

    ! 'Administrative' college, for data not under the academic colleges
    character (len=MAX_LEN_COLLEGE_CODE) :: ADMINISTRATION = 'ADMIN'

    ! 'Administrative' department, for data not under the academic departments
    character (len=MAX_LEN_DEPARTMENT_CODE) :: REGISTRAR = 'Registrar'

    integer :: baseYear = 2008 ! year that records usable by HEEDS are available in the database
    integer :: StdNoYearLen ! no. of characters in StdNo to use for directory name
    integer, parameter :: StdNoChars = 2 ! no. of characters in StdNo to use for directory name


contains


    subroutine blank_to_underscore (inString, outString)
        character(len=*), intent(in) :: inString
        character(len=*), intent(out) :: outString
        integer :: i, l
        l = len_trim(inString)
        outString = inString
        do i=1,l
            if (outString(i:i)==SPACE) outString(i:i) = '_'
        end do
        return
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
        return
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
        return
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
        return
    end subroutine lower_case


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
        return
    end subroutine encrypt


    subroutine decrypt(key, text)
        character(len=*), intent (in) :: key
        character(len=*), intent (inout) :: text
        integer :: i, j, k, lenKey, lenText, intText
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
        text = text(lenText+1:) ! discard 1st half
        return
    end subroutine decrypt



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
        return
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
        return
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
        return
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

        return
    end function ftoa


    function itoabz(num)
        character (len=10) :: itoabz
        integer, intent (in) :: num
        if (num == 0) then
            itoabz = ' .'
        else
            itoabz = itoa(num)
        end if
        return
    end function itoabz
  

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
        return
    end function itoa3bz

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
        return
    end subroutine index_to_delimiters


    subroutine check_array_bound(current, limit, msg)
        character (len = *), intent (in) :: msg
        integer, intent (in) :: current, limit
        if (current > limit) then
            write(unitLOG,AFORMAT) 'Aborting due to insufficient array size; increase '//msg
            write(unitLOG,'(1x,2(a,i5))')  'Limit is ', limit, '; currently used is ', current
            stop
        end if

        return
    end subroutine check_array_bound


    subroutine file_log_message(mesg1, mesg2, mesg3, mesg4, mesg5)
        character (len=*), intent (in) :: mesg1
        character (len=*), intent (in), optional :: mesg2, mesg3, mesg4, mesg5

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

        flush(unitLOG)

        return
    end subroutine file_log_message


    subroutine move_to_backup(fname)
        character (len=*), intent (in) :: fname ! must be in dirXML
        character (len=MAX_LEN_FILE_PATH) :: path
        integer :: iStat

        if (noWrites) return ! no backups

        path = trim(dirBAK)//fname(lenDirXML+1:)
        call rename (fname, path, iStat)
        if (iStat/=0) call file_log_message('Status='//trim(itoa(iStat))//' in moving to '//trim(path) )

        return
    end subroutine move_to_backup


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

        return
    end subroutine initialize_random_seed
  
end module BASE

