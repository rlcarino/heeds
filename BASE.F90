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


module BASE

    implicit none

    ! HEEDS root directory
#if defined GNULINUX
    character(len=18), parameter :: dirHEEDS = '/home/heeds/HEEDS/'
#else
    character(len= 7), parameter :: dirHEEDS = '\HEEDS\'
#endif

    ! CGI script
    character(len= 9), parameter :: CGI_SCRIPT = 'relay.php'
    character(len=18), parameter :: CGI_PATH = '/cgi-bin/'//CGI_SCRIPT

    ! flags to control generation of log files, backups
    logical, parameter :: MANY_LOG_FILES = .true. ! a log file for every run?
    logical, parameter :: DO_NOT_BACKUP = .false. ! create backups?

    ! directory separator; delete, directory, mkdir commands
#if defined GNULINUX
    character(len= 1), parameter :: DIRSEP = '/'
    character(len= 6), parameter :: delCmd = 'rm -f '
    character(len= 9), parameter :: dirCmd = 'dir -1tr '
    character(len= 9), parameter :: mkdirCmd = 'mkdir -p '
    character(len= 6), parameter :: mvCmd = 'mv -f '
#else
    character(len= 1), parameter :: DIRSEP = '\'
    character(len= 7), parameter :: delCmd = 'del /q '
    character(len=17), parameter :: dirCmd = 'dir /b /o:d /t:c '
    character(len= 6), parameter :: mkdirCmd = 'mkdir '
    character(len= 8), parameter :: mvCmd = 'move /y '
#endif

    integer :: currentYear ! year of start of Academic Year
    integer :: currentTerm ! current term 1=1st sem, 2=2nd sem; summer not yet allowed
    integer :: nextYear, nextTerm, targetTerm, targetYear
    integer :: prevTermYear, prevTermTerm, prevYearYear, prevYearTerm

    character(len=10) :: currentTime ! current time
    character(len= 8) :: currentDate ! current date

    integer :: stderr = 999 ! unit number of file for error messages
    integer :: adminPassword = 0 ! randomly generated password for REGISTRAR
    logical :: checkPassword = .true. ! check the password

    ! Max length of file path+name
    integer, parameter :: MAX_LEN_FILE_PATH = 256

    ! data & output locations
    character (len=MAX_LEN_FILE_PATH) :: &
        dirTmp, & ! directory for files to/from CGI script (must be readable/writable by HEEDS & Apache)
        dirWWW, & ! directory where web pages will be served (must be writable by HEEDS)
        dirCGI, & ! where CGI script is stored
        dirBak, & ! directory for backup files
        dirLog, & ! directory for log files
        dirSUBSTITUTIONS, & ! directory for input/UNEDITED checklists from Registrar
        dirTRANSCRIPTS, & ! directory for raw transcripts
        dirUploadCHECKLISTS, & ! directory where individual checklists for upload will be written
        dirUploadENLISTMENT, & ! directory where enlisted classes by student will be written
        dirEditedCHECKLISTS, & ! directory for output/EDITED checklists from College Secretaries
        dirRAW, & ! directory for raw data files
        dirXML, & ! directory for XML data files
        pathToCurrent, & ! path to files for currentYear+currentTerm
        pathToTarget, &  ! path to files for targetYear+targetTerm
        pathToSections, &
        pathToSectionUpdates

    ! constants
    character(len= 1), parameter :: &
        SPACE = ' ', comma = ',', dash ='-', fslash = '/', bslash = '\', dot = '.', prime = ''''
    character(len= 3), parameter :: AFORMAT = '(a)'
    character(len=10), parameter :: DECDIGITS = '0123456789'
    character(len=16), parameter :: HEXDIGITS = '0123456789ABCDEF'
    character(len=24), parameter :: SPECIAL = '<>"#%{}|^~[]`;/?:=&$+().'

    ! software version
    character(len= 5), parameter :: PROGNAME =  'HEEDS'
    character(len= 6), parameter :: VERSION =   ' v.3.0'
    character(len=38), parameter :: COPYRIGHT = 'Copyright (C) 2012 Ricolindo L. Carino'
    character(len=38), parameter :: EMAIL =     'Ricolindo.Carino@AcademicForecasts.com'
    character(len=76), parameter :: CONTACT =   'E-mail inquiries about '//PROGNAME//' to '//EMAIL//'.'

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
        length=len(string)
        do i=1,length
            if (string(i:i) .lt. 'a') cycle
            if (string(i:i) .gt. 'z') cycle
            string(i:i) = char(ichar(string(i:i))-32)
        end do
        return
    end subroutine upper_case


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
            if (string(1:1) == dash) then
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
            if (string(1:1) == dash) then
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
        if (num < 0) str10 = dash//str10
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
        if (num < 0) str10 = dash//str10
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


    subroutine Pause()
        write(*,*) 'Press the Enter key to continue'
        read (*,*)
        return
    end subroutine Pause


    subroutine check_array_bound(current, limit, msg)
        character (len = *), intent (in) :: msg
        integer, intent (in) :: current, limit
        if (current > limit) then
            write(*,*) 'Aborting due to insufficient array size; increase '//msg
            write(*,'(1x,2(a,i5))')  'Limit is ', limit, '; currently used is ', current
            call Pause()
            stop
        end if
        return
    end subroutine check_array_bound


    subroutine file_io_log(mesg, silent)
        character (len=*), intent (in) :: mesg
        logical, intent (in), optional :: silent

        if (.not. present(silent)) write(*,*) trim(mesg)
        write(stderr,AFORMAT) trim(mesg)

        return
    end subroutine file_io_log


    subroutine file_log_message(mesg1, mesg2, mesg3)
        character (len=*), intent (in) :: mesg1
        character (len=*), intent (in), optional :: mesg2, mesg3
        write(*,*) trim(mesg1)
        write(stderr,AFORMAT) trim(mesg1)
        if (present(mesg2)) then
            write(*,*) trim(mesg2)
            write(stderr,AFORMAT) trim(mesg2)
        end if
        if (present(mesg3)) then
            write(*,*) trim(mesg3)
            write(stderr,AFORMAT) trim(mesg3)
        end if
        return
    end subroutine file_log_message


    subroutine open_for_write(iounit, filename)
        integer, intent (in) :: iounit
        character (len=*), intent (in) :: filename
        integer :: i, ier, n, pdel
        character (len=MAX_LEN_FILE_PATH) :: fname

        fname = filename
        n = len_trim(fname)-1
        pdel = 0
        do i=1,n
            if (index('=+\/',fname(i:i))==0) cycle
            fname(i:i) = DIRSEP ! directory separator
            pdel = i-1
        end do
        open(unit=iounit,file=fname,form='formatted', status='replace', iostat=ier)
        if (ier>0) then ! create directory
            !write(stderr,AFORMAT) 'Creating '//fname(:pdel)
            call system (mkdirCmd//fname(:pdel))
            open(unit=iounit,file=fname,form='formatted')
        end if
        return
    end subroutine open_for_write


    subroutine write_lock_file(fname)
        character (len=*), intent (in) :: fname
        call open_for_write(4, trim(fname)//'.lock')
        write(4,AFORMAT) 'If '//PROGNAME//' is NOT running, it is safe to delete this lock file.'
        close(4)
        return
    end subroutine write_lock_file


    subroutine move_to_backup(fname)
        character (len=*), intent (in) :: fname
        character (len=MAX_LEN_FILE_PATH) :: path
        integer :: iStat, first!, last
        logical :: flagIsUp

        if (DO_NOT_BACKUP) return ! no backups

        first = index(fname, 'xml')
        if (first==0) return ! do not backup files not in 'xml' directory

        inquire(file=fname, exist=flagIsUp)
        if (.not. flagIsUp) return ! does not exist anyway

        call date_and_time (date=currentDate,time=currentTime)
        path = trim(fname)//dash//currentDate//dash//currentTime(1:6)
        path(first:first+2) = 'bak'
        call rename (fname, path, iStat)
        write(*,*) 'Status=', iStat, ' in moving '//trim(fname)//' to '//trim(path)

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

