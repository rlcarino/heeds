!======================================================================
!
!    HEEDS (Higher Education Enrollment Decision Support) - A program
!      to create enrollment scenarios for 'next term' in a university
!    Copyright (C) 2012-2015 Ricolindo L. Carino
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

    character(len= 5), parameter :: PROGNAME  = 'HEEDS'
    character(len=44), parameter :: COPYRIGHT = 'Copyright (C) 2012-2015 Ricolindo L. Carino'
    character(len=38), parameter :: EMAIL     = 'Ricolindo.Carino@AcademicForecasts.com'
    character(len=72), parameter :: CONTACT   = 'E-mail inquiries about '//PROGNAME//' to '//EMAIL//'.'
    character(len=32), parameter :: WEB       = 'http://code.google.com/p/heeds/'
    character(len=12)            :: VERSION   = ' RYYYY-MM-YY'

!===========================================================
! OS-related parameters
!===========================================================
#if defined GLNX
    ! file separator; delete, directory, mkdir commands
    character(len= 8), parameter :: dirCmd = 'ls -1tr '
    character(len= 7), parameter :: delCmd = 'rm -rf '
    character(len= 6), parameter :: mvCmd = 'mv -f '
    character(len= 6), parameter :: cpCmd = 'cp -f '
    character(len= 9), parameter :: mkdirCmd = 'mkdir -p '
    character(len= 1), parameter :: DIRSEP = '/'

#else
    ! file separator; delete, directory, mkdir commands
    character(len=17), parameter :: dirCmd = 'dir /b /o:d /t:c '
    character(len= 7), parameter :: delCmd = 'del /q '
    character(len= 8), parameter :: mvCmd = 'move /y '
    character(len= 8), parameter :: cpCmd = 'copy /y '
    character(len= 6), parameter :: mkdirCmd = 'mkdir '
    character(len= 1), parameter :: DIRSEP = '\'

#endif


!===========================================================
! passwords
!===========================================================
    real :: harvest    ! random number
    character (len=34) :: allowedPasswordChars = 'A2B3D45F6G7H8J9bcdfghjkmnprstvwxyz'
    integer, parameter :: &  ! length of password variables
        MAX_LEN_PASSWD_VAR=32, &
        MIN_LEN_PASSWORD=8, &
        MAX_LEN_PASSWORD=12, &
        lenPasswordEncryptionKey = 16
    character(len=lenPasswordEncryptionKey) :: & ! replace for your Institution
        passwordEncryptionKey = 'w!thUr0wnr3pL@c3'


!===========================================================
! University code; Academic Year & Term
!===========================================================

    character (len=20) :: UniversityCode = 'DEMO'
    integer :: currentYear=0 ! year of start of Academic Year
    integer :: currentTerm=0 ! current term 1=1st sem, 2=2nd sem; 3=summer
    integer :: cTm1Year=0, cTm1=0, cTm2Year=0, cTm2=0, cTm3Year=0, cTm3=0

!===========================================================
! times
!===========================================================

    integer :: maxIdleTime = 900 ! seconds before auto-logout
    integer :: maxRefreshTime = 1800 ! seconds before auto-refresh
    integer :: maxBackupTime = 1800 ! seconds before auto-backup

    integer :: maxStudentsForNeedsAnalysis = 700 ! Max no. of students for Needs Analysis to avoid timeout

    character(len=10) :: currentTime ! current time - HHMMSS.LLL
    character(len= 8) :: currentDate, previousDate ! current, previous dates - YYYYMMDD
    character(len=15) :: startDateTime ! YYYYMMDD-HHMMSS
    !character(len=8) :: startDateTime ! YYYYMMDD
    integer(8) :: tick, count_rate, count_max ! system clock
    integer(8) :: tickLastRefresh, tickLastBackup ! clock-tick when last refreshed, last auto-backup
    integer :: secsNextRefresh, secsLastRefresh ! estimated time in seconds before next refresh, after last refresh

!===========================================================
! flags
!===========================================================

    logical :: isServer = .false., isReadOnly = .false., isMirror = .false.
    logical :: isDirtyData = .false., isAllowedNonEditors = .true.

!===========================================================
! file paths
!===========================================================

    integer, parameter :: MAX_LEN_FILE_PATH = 256 ! Max length of file path+name
    integer, parameter :: &
        unitHTML = 999, &   ! file unit for HTML to webserver
        unitUSER = 998, &   ! file unit for user log of activities
        unitXML  = 997, &   ! file unit for XML input/output
        unitRAW  = 996, &   ! file unit for custom inputs
        unitLOG  = 995, &   ! file unit for log messages
        unitREQ  = 994, &   ! file unit for requests by user
        unitIP   = 993, &   ! file unit for requests by IP address
        unitIDX  = 992, &
        unitREM  = 991, &   ! file unit for concatenating evaluation remarks
        unitETC  = 990      ! file unit for almost-atomic "open/read-or-write/close" operation

    ! data & output paths
    character (len=MAX_LEN_FILE_PATH) :: &
        dirUNIV, & ! directory for university data
        dirBACKUP, & ! directory for backup files
        dirDATA, & ! directory for XML data files
        dirLOG, &  ! directory for log files
        dirSUBSTITUTIONS, & ! directory for subject substitutions
        dirTRANSCRIPTS, & ! directory for individual enrollment records
        dirSTUDENTINFO, & ! directory for individual student info
        dirADVANCECREDITS, & ! directory for advance/transfer credit records
        dirCOMPLETIONS, & ! directory for completion records
        dirUPLOADS, & ! directory for uploads
        dirASSESSMENTS, & ! directory for assesment
        dirDISCOUNTS, & ! directory for discounts
        dirPAYMENTS, & ! directory for accounting transactions
        fileEXE, & ! name of executable
        fileLOG, & ! name of log file diagnostic messages
        fileIP, & ! name of log file for IP address
        fileUSER, & ! name of log file for user
        fileREQ, & ! name of file for all requests
        pathToYear, pathToTerm  ! path data files

    ! position of last character in dirDATA (to simplify derivation of path to backup)
    integer :: lenDirDATA

    ! constants
    character(len= 1), parameter :: &
        SPACE = ' ', COMMA = ',', DASH ='-', FSLASH = '/', BSLASH = '\', PRIME = '''', DOT = '.', LPAR = '(', RPAR = ')'
    character(len= 1), parameter :: NUL = achar(0), LF = achar(10), CR = achar(13)
    character(len= 2), parameter :: CRLF = achar(10)//achar(13)
    character(len= 3), parameter :: AFORMAT = '(a)'
    character(len= 4), parameter :: dotXML = '.XML'
    character(len= 5), parameter :: dotPART = '.PART'
    character(len= 8), parameter :: ZFORMAT = '(16z0.2)'
    character(len=10), parameter :: DECDIGITS = '0123456789'
    character(len=16), parameter :: HEXDIGITS = '0123456789ABCDEF'
    character(len=24), parameter :: SPECIAL = '<>"#%{}|^~[]`;/?:=&$+().'

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
    character(len=20), parameter :: Black = '<font color=#000000>'
    character(len= 7), parameter :: e_color = '</font>'

!===========================================================
! XML-related parameters
!===========================================================

    ! maximum line size in an XML file
    integer, parameter :: MAX_LEN_XML_LINE = 2048

    ! maximum characters in a tag
    integer, parameter :: MAX_LEN_XML_TAG = 40

    ! root names                                           123456789012345678901234567890123456789012345678901234567890
    character(len=42), parameter :: XML_DOC             = '?xml version="1.0" encoding="ISO-8859-1" ?'
    character(len=36), parameter :: ROOT_PSGC           = 'PHILIPPINE_STANDARD_GEOGRAPHIC_CODES'
    character(len=14), parameter :: ROOT_TONGUES        = 'MOTHER_TONGUES'
    character(len=10), parameter :: ROOT_EVALUATION     = 'EVALUATION'
    character(len=10), parameter :: ROOT_UNIVERSITY     = 'UNIVERSITY'
    character(len=16), parameter :: ROOT_COLLEGES       = 'LIST_OF_COLLEGES'
    character(len=19), parameter :: ROOT_DEPARTMENTS    = 'LIST_OF_DEPARTMENTS'
    character(len=16), parameter :: ROOT_SUBJECTS       = 'LIST_OF_SUBJECTS'
    character(len=13), parameter :: ROOT_ROOMS          = 'LIST_OF_ROOMS'
    character(len=16), parameter :: ROOT_TEACHERS       = 'LIST_OF_TEACHERS'
    character(len=17), parameter :: ROOT_CURRICULA      = 'LIST_OF_CURRICULA'
    character(len=21), parameter :: ROOT_EQUIVALENCIES  = 'LIST_OF_EQUIVALENCIES'
    character(len=16), parameter :: ROOT_STUDENTS       = 'LIST_OF_STUDENTS'
    character(len=14), parameter :: ROOT_STUDENT_RECORD = 'STUDENT_RECORD'
    character(len=12), parameter :: ROOT_STUDENT_INFO   = 'STUDENT_INFO'
    character(len=13), parameter :: ROOT_SUBSTITUTIONS  = 'SUBSTITUTIONS'
    character(len=11), parameter :: ROOT_ENLISTMENT     = 'ENLISTMENT_'
    character(len=15), parameter :: ROOT_BLOCKS         = 'LIST_OF_BLOCKS_'
    character(len=17), parameter :: ROOT_SECTIONS       = 'LIST_OF_SECTIONS_'
    character(len=16), parameter :: ROOT_ACCOUNTS       = 'LIST_OF_ACCOUNTS'
    character(len= 7), parameter :: COMMENT             = 'COMMENT'
    ! root names                                           123456789012345678901234567890123456789012345678901234567890

    ! indentation
    integer, parameter :: INDENT_INCR = 4 ! no. SPACEs for next indent
    integer :: & ! indentation levels
        indent0 = 0, &
        indent1 = INDENT_INCR,   &
        indent2 = INDENT_INCR*2, &
        indent3 = INDENT_INCR*3, &
        indent4 = INDENT_INCR*4, &
        indent5 = INDENT_INCR*5, &
        indent6 = INDENT_INCR*6
    character(len=80)  :: indentation = SPACE


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
    character(len=19), parameter :: b_tdac= '<td align="center">', b_thac= '<th align="center">'
    character(len=18), parameter :: b_tdar= '<td align="right">', b_thar= '<th align="right">'
    character(len=17), parameter :: b_thal= '<th align="left">'
    character(len=15), parameter :: b_td_nbsp_e_td= '<td>&nbsp;</td>'
    character(len= 8), parameter :: e_table = '</table>'
    character(len= 7), parameter :: b_small = '<small>'
    character(len= 8), parameter :: e_small = '</small>'
    character(len= 7), parameter :: e_form = '</form>'
    character(len= 3), parameter :: b_para = '<p>'
    character(len= 4), parameter :: e_para = '</p>'
    character(len= 3), parameter :: b_bold = '<b>'
    character(len= 4), parameter :: e_bold = '</b>'
    character(len= 3), parameter :: b_underline = '<u>'
    character(len= 4), parameter :: e_underline = '</u>'
    character(len= 3), parameter :: b_italic = '<i>'
    character(len= 4), parameter :: e_italic = '</i>'
    character(len= 4), parameter :: horizontal = '<hr>'
    character(len= 4), parameter :: linebreak = '<br>'
    character(len= 4), parameter :: b_item = '<li>'
    character(len= 5), parameter :: e_item = '</li>'
    character(len= 4), parameter :: b_td= '<td>', b_th= '<th>', b_tr= '<tr>'
    character(len= 5), parameter :: e_td= '</td>', e_th= '</th>', e_tr= '</tr>'
    character(len= 6), parameter :: nbsp='&nbsp;'
    character(len= 9), parameter :: e_select = '</select>'

    character(len=16), parameter :: JUSTNONE = linebreak//'( None )'//linebreak
    character(len=20), parameter :: BRNONE = JUSTNONE//linebreak
    character(len=24), parameter :: BRNONEHR = BRNONE//horizontal

    character(len=20), parameter :: selected(0:1) = (/ '                    ', ' selected="selected"' /)
    character(len=14), parameter :: checked(0:1) = (/ '              ', ' checked="yes"' /)

    ! string lengths
    integer, parameter :: MAX_LEN_QUERY_STRING = 4096
    integer, parameter :: MAX_CGI_WRK_LEN = 2048
    integer, parameter :: MAX_CGI_INT_LEN = 10
    integer, parameter :: MAX_CGI_FLT_LEN = 12

    character(len=20)  :: SERVER_PROTOCOL = SPACE
    character (len=MAX_LEN_FILE_PATH) :: &
        dirDOWNLOADS = SPACE, urlDOWNLOADS = SPACE, &  ! directory, URL for files to download
        dirPICTURES = SPACE, urlPICTURES = SPACE, & ! directory, URL for pictures in transcripts
        HEEDS_DIRECTORY = SPACE, SERVER_NAME = SPACE, DOCUMENT_ROOT = SPACE, CGI_PATH = SPACE ! path to university data, wwwdir, URI

    ! the script
    character (len=MAX_LEN_FILE_PATH)    :: DOCUMENT_URI = SPACE, HTTP_USER_AGENT = SPACE

    ! the IP address xxx.xxx.xxx.xxx
    character (len=16)                   :: REMOTE_ADDR = SPACE

    ! the QUERY, encryption key, cipher text
    character (len=MAX_LEN_QUERY_STRING) :: QUERY_STRING, queryEncryptionKey, cipher, MOTD, EMERGENCY, wrkCipher

    ! functionality
    character (len=255) :: sorryMessageOfficial, sorryMessageStudent, sorryMessageSchedules
    character (len=15) :: ACTION
    logical :: isEnabledGuest = .true.
#if defined UPLB
    logical :: isProbabilistic = .false.
#endif

    ! local work areas
    character(len=MAX_CGI_WRK_LEN), private :: cgi_wrk
    character(len=MAX_CGI_INT_LEN), private :: cgi_int
    character(len=MAX_CGI_FLT_LEN), private :: cgi_flt

    ! Users
    integer, parameter :: MAX_LEN_USERNAME = 20 ! length of login name
    integer, parameter :: MAX_LOGIN_ATTEMPTS = 5 ! max failed login attempts per day
    character (len=MAX_LEN_USERNAME) :: USERNAME

    ! the requested function
    integer :: REQUEST

    ! login check message, user role
    character(len=MAX_LEN_FILE_PATH) :: UserRequestCheckMessage

    ! interface to FastCGI routines in C
    interface

        ! The function to accept a request from the webserver
        function FCGI_Accept () bind(C, NAME='FCGI_Accept')
            use ISO_C_BINDING
            implicit none
            integer(C_INT) :: FCGI_Accept
        end function FCGI_Accept

        ! The function to retrieve POSTed data
        function FCGI_getchar () bind(C, NAME='FCGI_getchar')
            use ISO_C_BINDING
            implicit none
            character(C_CHAR) :: FCGI_getchar
        end function FCGI_getchar

        ! The function to write back to the webserver
        function FCGI_puts (s) bind(C, NAME='FCGI_puts')
            use ISO_C_BINDING
            implicit none
            integer(C_INT) :: FCGI_puts
            character(C_CHAR), dimension(*) :: s
        end function FCGI_puts

    end interface

contains

!===========================================================
! Diagnostic messages
!===========================================================

    subroutine usage(mesg)

        character(len=*), intent(in), optional :: mesg

        if (present(mesg)) write(*,AFORMAT) SPACE, trim(mesg)
        write(*,AFORMAT) &
            'USAGE: spawn-fcgi -a SERVER_NAME -p PORT_NUM -f "'//trim(fileEXE)//' UNIV YEAR TERM ACTION"', &
            '  SERVER_NAME, PORT_NUM - as specified by fastcgi_pass in nginx configuration', &
            '  UNIV - code for University', &
            '  YEAR - chronological year when the School Year started', &
            '  TERM - term of School Year for which plans are being made', &
            '  ACTION - Import, Checklists, Restore, Special, Passwords, Server, Mirror ', &
            SPACE
        stop

    end subroutine usage



    subroutine check_array_bound(current, limit, mesg, critical)
        character (len=*), intent (in) :: mesg
        integer, intent (in) :: current, limit
        logical, optional, intent (out) :: critical

        logical :: flag

        flag = current > limit
        if (flag) then
            if (present(critical)) then
                critical = flag
            else
                call terminate(current, 'Aborting due to insufficient array size; increase '//trim(mesg)// &
                    '. Limit is '//itoa(limit)//'; currently used is '//itoa(current) )
            end if
        else
            if (present(critical)) then
                critical = flag
            end if
        end if

    end subroutine check_array_bound

    subroutine open_for_write(device, filename, stat)
        character (len=*), intent (in) :: filename
        integer, intent (in) :: device
        integer, intent (out) :: stat
        integer :: loc

        open(unit=device, file=filename, iostat=stat)
        if (stat==0) return
        loc = 0 ! get name of directory
        do loc=len_trim(filename),1,-1
            if (filename(loc:loc)==DIRSEP) exit
        end do
        call system (mkdirCmd//filename(:loc-1), stat)
        if (stat==0) open(unit=device, file=filename, iostat=stat)

    end subroutine open_for_write


    subroutine open_for_append(device, filename, stat, backtrack)
        character (len=*), intent (in) :: filename
        integer, intent (in) :: device
        integer, intent (out) :: stat
        logical, optional, intent (in) :: backtrack

        logical :: fileExists

        inquire(file=filename, exist=fileExists)
        if (fileExists) then
            open(unit=device, file=filename, position='append', iostat=stat)
        else
            call open_for_write(device, filename, stat)
            if (present(backtrack)) backspace(device, iostat=stat)
        end if

    end subroutine open_for_append


    subroutine log_comment(mesg1, mesg2, mesg3, mesg4, mesg5, out6)
        character (len=*), intent (in) :: mesg1
        character (len=*), intent (in), optional :: mesg2, mesg3, mesg4, mesg5
        logical, intent (in), optional :: out6

        logical :: logExists, stdout

        if (present(out6)) then
            stdout = out6
        else
            stdout = .false.
        end if

        fileLOG = trim(dirLOG)//currentDate//DIRSEP//currentTime(:6)//DASH//trim(ACTION)//'-message.log'
        !fileLOG = trim(dirLOG)//currentDate//DIRSEP//startDateTime//'-message.log'
        inquire(file=trim(fileLOG), exist=logExists)
        if (.not. logExists) then
            open(unit=unitLOG, file=trim(fileLOG), status='new')
        else
            open(unit=unitLOG, file=trim(fileLOG), status='old', position='append')
        end if
        write(unitLOG,AFORMAT) trim(mesg1)
        if (stdout) write(*,*) trim(mesg1)
        if (present(mesg2)) then
            write(unitLOG,AFORMAT) trim(mesg2)
            if (stdout) write(*,*) trim(mesg2)
            if (present(mesg3)) then
                write(unitLOG,AFORMAT) trim(mesg3)
                if (stdout) write(*,*) trim(mesg3)
                if (present(mesg4)) then
                    write(unitLOG,AFORMAT) trim(mesg4)
                    if (stdout) write(*,*) trim(mesg4)
                    if (present(mesg5)) then
                        write(unitLOG,AFORMAT) trim(mesg5)
                        if (stdout) write(*,*) trim(mesg5)
                    end if
                end if
            end if
        end if
        call flush(unitLOG)
        close(unitLOG)

    end subroutine log_comment



    subroutine log_request(fileNo, fileName, mesg1, mesg2, mesg3, mesg4, mesg5)
        integer, intent (in) :: fileNo
        character (len=*), intent (in) :: fileName, mesg1
        character (len=*), intent (in), optional :: mesg2, mesg3, mesg4, mesg5

        logical :: logExists
        integer :: ierr

        inquire(file=fileName, exist=logExists)
        if (.not. logExists) then
            open(unit=fileNo, file=fileName, status='new', iostat=ierr, err=999)
        else
            open(unit=fileNo, file=fileName, status='old', position='append', iostat=ierr, err=999)
        end if
        write(fileNo,AFORMAT, iostat=ierr, err=998) trim(mesg1)
        if (present(mesg2)) then
            write(fileNo,AFORMAT, iostat=ierr, err=998) trim(mesg2)
            if (present(mesg3)) then
                write(fileNo,AFORMAT, iostat=ierr, err=998) trim(mesg3)
                if (present(mesg4)) then
                    write(fileNo,AFORMAT, iostat=ierr, err=998) trim(mesg4)
                    if (present(mesg5)) then
                        write(fileNo,AFORMAT, iostat=ierr, err=998) trim(mesg5)
                    end if
                end if
            end if
        end if
        call flush(fileNo)
        998 close(fileNo)
        999 call terminate(ierr, 'log_request( unit='//trim(itoa(fileNo))// &
            ', file='//trim(fileName)//', mesg='//trim(mesg1)//' )' )

    end subroutine log_request


    subroutine html_comment(str1, str2, str3, str4, str5)
        character(len=*), intent(in) :: str1
        character(len=*), intent(in), optional :: str2, str3, str4, str5

        write(unitHTML,AFORMAT) 'DBG::'//trim(str1)
        if (present(str2)) then
            write(unitHTML,AFORMAT) 'DBG::'//trim(str2)
            if (present(str3)) then
                write(unitHTML,AFORMAT) 'DBG::'//trim(str3)
                if (present(str4)) then
                    write(unitHTML,AFORMAT) 'DBG::'//trim(str4)
                    if (present(str5)) then
                        write(unitHTML,AFORMAT) 'DBG::'//trim(str5)
                    end if
                end if
            end if
        end if
        call flush(unitHTML)

    end subroutine html_comment


    subroutine terminate(IOerr, mesg)
        integer, intent (in) :: IOerr
        character(len=*), intent (in) :: mesg

        select case (IOerr)

            case (0) ! no error
                return

            case (-1) ! normal server termination

                ! backspace the response file for additional "call html_comment()" before stop
                if (isServer) backspace (unitHTML)

            case default ! write-to-disk error

                call system(cpCmd//trim(DOCUMENT_ROOT)//DIRSEP//'50x-write-to-disk-failed.html '// &
                    trim(DOCUMENT_ROOT)//DIRSEP//'50x.html')

                if (isServer) then ! diagnostic messages

                    call html_home_page(unitHTML, mesg)

!                    ! activity by USERNAME
!                    if (len_trim(fileUSER)>0) then
!                        write(unitHTML,AFORMAT) 'Last user: '//trim(fileUSER)
!                        write(unitHTML,AFORMAT) '<pre>'
!                        call copy_to_unit(fileUSER, unitHTML)
!                        write(unitHTML,AFORMAT) '</pre>'//horizontal
!                    end if

!                    ! activity on IP
!                        if (len_trim(fileIP)>0) then
!                        write(unitHTML,AFORMAT) 'Last IP: '//trim(fileIP)
!                        write(unitHTML,AFORMAT) '<pre>'
!                        call copy_to_unit(fileIP, unitHTML)
!                        write(unitHTML,AFORMAT) '</pre>'//horizontal
!                    end if

                    write(unitHTML,AFORMAT) '</body></html>'

                    ! send page to server
                    call FCGI_putfile(unitHTML)

                    ! backspace the response file for additional "call html_comment()" before stop
                    backspace (unitHTML)
                end if

        end select

        call html_comment(mesg)
        call flush(unitHTML)
        close(unitHTML)

        call log_comment(mesg, '-------', 'Ends '//currentDate//DASH//currentTime, '-------' )

        write(*,*) mesg
        write(*,*) 'See '// trim(fileLOG)//' for other messages.'
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
        integer :: i, j, k, lenKey, lenText, intText, ioERR

        lenKey = len_trim(key)
        lenText = len_trim(text)/2
        i = lenKey
        do j=lenText,1,-1
            k = 2*j
            read(text(k-1:k), '(z2)', iostat=ioERR) intText
            if (ioERR/=0) then
                call html_comment('Error '//itoa(ioERR)//' in decrypt('//trim(text)//')')
                text = SPACE
                return
            end if
            text(j+lenText:j+lenText) = char( ieor(ichar(key(i:i)),intText) )
            i = i-1
            if (i==0) i = lenKey
        end do
        text = text(lenText+1:) ! move to front

    end subroutine decrypt


    function calculate_check_sum(encryptedText)
        character(len=*), intent (in) :: encryptedText
        integer :: calculate_check_sum
        integer :: checkSum, kStart, lenCipher

        lenCipher = len_trim(encryptedText)
        checkSum = 0
        do kStart=1,lenCipher
            checkSum = checkSum + kStart*ichar(encryptedText(kStart:kStart))
        end do
        calculate_check_sum = checkSum

    end function calculate_check_sum


    subroutine set_password(Password, forcedPassword)
        character (len=MAX_LEN_PASSWD_VAR), intent (in out) :: Password
        character (len=*), intent (in), optional :: forcedPassword
        integer :: i, j, lenP, lenS

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
                j = 1 + int(harvest*33)
                Password(i:i) = allowedPasswordChars(j:j)
            end do
        end if
        ! add salt
        lenS = lenPasswordEncryptionKey-lenP ! length of salt
        Password(lenS+1:) = Password(:lenP) ! right-justify Password() to make space for salt
        do i=1,lenS ! generate random salt
            call random_number(harvest)
            j = 1 + int(harvest*33)
            Password(i:i) = allowedPasswordChars(j:j)
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
        outString = fn_blank_to_underscore(inString)
    end subroutine blank_to_underscore


    function fn_blank_to_underscore (inString)
        character(len=*), intent(in) :: inString
        character(len=MAX_LEN_FILE_PATH) :: fn_blank_to_underscore, outString

        integer :: i, l
        l = len_trim(inString)
        outString = inString
        do i=1,l
            if (outString(i:i)==SPACE) outString(i:i) = '_'
        end do
        fn_blank_to_underscore = outString

    end function fn_blank_to_underscore


    subroutine underscore_to_blank (inString, outString)
        character(len=*), intent(in) :: inString
        character(len=*), intent(out) :: outString
        outString = fn_underscore_to_blank(inString)
    end subroutine underscore_to_blank


    function fn_underscore_to_blank (inString)
        character(len=*), intent(in) :: inString
        character(len=MAX_LEN_FILE_PATH) :: fn_underscore_to_blank, outString

        integer :: i, l
        l = len_trim(inString)
        outString = inString
        do i=1,l
            if (outString(i:i)=='_') outString(i:i) = SPACE
        end do
        fn_underscore_to_blank = outString

    end function fn_underscore_to_blank


    subroutine upper_case(string)
        ! change lower-case letters in string to upper case
        character(len=*), intent (inout) :: string
        integer :: i,length, code
        length=len_trim(string)
        do i=1,length
            if (string(i:i) .lt. 'a') cycle
            code = ichar(string(i:i))
            if ( string(i:i) .le. 'z' .or. code .gt. 223 ) then
                string(i:i) = char(code-32)
            end if
        end do

    end subroutine upper_case



    subroutine lower_case(string)
        ! change string to lower case
        character(len=*), intent (inout) :: string
        integer :: i,length, code
        length=len_trim(string)
        do i=1,length
            if (string(i:i) .lt. 'A') cycle
            code = ichar(string(i:i))
            if (string(i:i) .le. 'Z' .or. code .gt. 191) then
                string(i:i) = char(ichar(string(i:i))+32)
            end if
        end do

    end subroutine lower_case



    subroutine proper_case(string)
        ! change string to proper case
        character(len=*), intent (inout) :: string
        integer :: i,length, code
        length=len_trim(string)
        do i=2,length
            if (string(i:i) .lt. 'A') cycle
            if (string(i-1:i-1) .eq. SPACE) cycle
            if (string(i-1:i-1) .eq. LPAR) cycle
            code = ichar(string(i:i))
            if (string(i:i) .le. 'Z' .or. code .gt. 191) then
                string(i:i) = char(ichar(string(i:i))+32)
            end if
        end do

    end subroutine proper_case


    function isAlphaNumeric(inString)
        character(len=*), intent (in) :: inString
        logical :: isAlphaNumeric, isOK
        character :: ch
        integer :: i

        isOK = .true.
        do i=1,len_trim(inString)
            ch = inString(i:i)
            if (ch .ge. 'A' .and. ch .le. 'Z') cycle
            if (ch .ge. '0' .and. ch .le. '9') cycle
            if (ch .eq. DASH) cycle
            isOK = .false.
            exit
        end do
        isAlphaNumeric = isOK

    end function isAlphaNumeric


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
                elseif (string(i:i)==DOT) then ! decimal point
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



    function iptoint8(inString)
        ! convert IP address to integer
        integer*8 :: iptoint8, num
        character (len=16), intent (in) :: inString
        integer :: i, j, ll
        character (len=16) :: string
        num = 0
        string = adjustl(inString)
        ll = len_trim(string)
        do i=1,ll
            j = index(DECDIGITS, string(i:i))
            if (j > 0) then
                num = 10*num + j - 1
            end if
        end do
        iptoint8 = num

    end function iptoint8

  
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



    function ftoa(tNum, dadp)
        ! convert positive num to string

        character (len=10) :: ftoa
        real, intent (in) :: tNum
        integer, intent (in) :: dadp ! digits after the decimal point, optional

        character (len=10) :: str10
        real :: frac, num
        integer :: i, j, k, l

        ! round to dadp
        num = tNum + sign(0.5/10**dadp,tNum)

        k = int(abs(num))
        frac = abs(num) - k
        str10 = itoa(k) ! integral part
        l = len_trim(str10)+1 ! position of decimal point
        str10(l:l) = DOT ! add decimal point
        i = 0
        do
            l = l + 1
            i = i + 1
            j = frac*10.0 ! shift decimal pt to the right
            str10(l:l) = DECDIGITS(j+1:j+1)
            frac = frac*10.0 - j ! remainder
            if (frac==0.0 .or. i==dadp .or. l==10) exit
        end do
        if (l<10 .and. str10(l:l)==DOT) str10(l+1:l+1) = '0' ! add 0 after decimal point
        if (num < 0) str10 = DASH//str10
        ftoa = str10

    end function ftoa



    function itoabz(num)
        character (len=10) :: itoabz
        integer, intent (in) :: num
        if (num == 0) then
            itoabz = nbsp ! ' .'
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


    function filename_from(token)
        character (len=*), intent(in) :: token
        character (len=MAX_LEN_FILE_PATH) :: filename_from, tmpStr
        integer :: i
        character :: ch

        tmpStr = fn_blank_to_underscore(token)
        call upper_case(tmpStr)
        do i=1,len_trim(tmpStr)
            ch = tmpStr(i:i)
            if (ch .ge. 'A' .and. ch .le. 'Z') cycle
            if (ch .ge. '0' .and. ch .le. '9') cycle
            if (ch .eq. '_') cycle
            tmpStr(i:i) = 'x'
        end do
        filename_from = tmpStr(1:i-1)

    end function filename_from


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
            call system(delCmd//trim(dirName)//DIRSEP//'*', ierr)
        end if

    end subroutine make_directory


    subroutine move_to_backup(fname)
        character (len=*), intent (in) :: fname ! must be in dirDATA
        character (len=MAX_LEN_FILE_PATH) :: path
        integer :: iStat
        logical :: fileExists

        inquire(file=trim(fname), exist=fileExists)
        if (.not. fileExists) return

        path = trim(dirBACKUP)//trim(fname(lenDirDATA+1:))//DASH//currentDate//DASH//currentTime
        call log_comment('move_to_backup('//trim(path)//')')

        call rename (fname, path, iStat)
        call terminate(iStat, 'Error in moving '//trim(fname)//' to '//path )

        ! compress backup
#if defined GLNX
        call system('gzip '//trim(path), iStat)
        call terminate(iStat, 'Error in compressing '//path )
!#else
!        call system('"C:\Program Files\7-Zip\7z.exe" a -t7z '//trim(path)//'.7z '//trim(path), iStat)
!        if (iStat/=0) call log_comment(itoa(iStat)//' returned by: '// &
!            '"C:\Program Files\7-Zip\7z.exe" a -t7z '//trim(path)//'.7z '//trim(path))
#endif

    end subroutine move_to_backup



    subroutine copy_to_unit(fileName, device)
        integer, intent(in) :: device
        character (len=*), intent(in) :: fileName
        integer :: iTmp

        call html_comment('copy_to_unit('//trim(fileName)//')')
        wrkCipher = '(opening file)'
        open(unit=unitRAW, file=fileName, status='old', iostat=iTmp, err=999)
        do
            read(unitRAW, AFORMAT, iostat=iTmp, end=998, err=998) wrkCipher
            write(device,AFORMAT, iostat=iTmp, err=999) trim(wrkCipher)
        end do
        998 iTmp = 0
        close(unitRAW)
        999 call terminate(iTmp, 'copy_to_unit( device='//trim(itoa(device))// &
            ', code='//trim(itoa(iTmp))//', file='//trim(fileName)//', line='//trim(wrkCipher)//' )' )

    end subroutine copy_to_unit


!===========================================================
! XML-related routines
!===========================================================

    subroutine xml_read_file(device, rootName, fileName, errNo)
        integer, intent (in) :: device
        character (len=*), intent (in) :: fileName, rootName
        integer, intent (in out) :: errNo
        character(len=MAX_LEN_XML_LINE) :: xmlLine
        integer :: ierr

        ! open & look for rootName in file
        open(unit=device, file=fileName, status='old', iostat=ierr, err=999)
        ierr = 1 ! assume not found
        do
            read(device, AFORMAT, end=998, err=998) xmlLine
            if (index(xmlLine, '<'//rootName//'>')==0) cycle
            ierr = 0 ! found
            exit
        end do
        998 if (ierr/=0) close(device)
        999 errNo = ierr


    end subroutine xml_read_file



    subroutine xml_write_character(device, indent, tag, value)
        integer, intent (in) :: device, indent
        character (len=*), intent (in) :: tag
        character (len=*), intent (in), optional :: value
        character(len=MAX_LEN_XML_LINE) :: xmlLine
        integer :: idx, stat ! position of ' & ' in value

        if (present(value)) then ! convert ' & ' to ' and '
            xmlLine = value
            idx = index(xmlLine, ' & ')
            do while (idx>0)
                xmlLine = xmlLine(:idx)//'and'//xmlLine(idx+2:)
                idx = index(xmlLine, ' & ')
            end do
            write(device, AFORMAT, iostat=stat, err=999) indentation(:indent)// &
                '<'//trim(tag)//'>'//trim(xmlLine)//'</'//trim(tag)//'>'
        else
            write(device, AFORMAT, iostat=stat, err=999) indentation(:indent)//'<'//trim(tag)//'>'
        end if

        999 call terminate(stat, 'xml_write_character( device='//trim(itoa(device))// &
            ', indent='//trim(itoa(indent))//', tag='//trim(tag)//', value='//trim(value)//' )' )

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
        errNo = 0

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

    function make_href(fn, label, pre, post, A1, A2, A3, A4, A5, A9, anchor, newtab)
    ! build HTML href, like:
    !   pre<a href="CGI_PATH?F=fn&A1=A1&...A5=A5 #anchor target=newtab">label</a>post

        integer, intent (in) :: fn
        integer, intent (in), optional :: A9
        character(len=*), intent (in) :: label
        character(len=*), intent (in), optional :: A1, A2, A3, A4, A5
        character(len=*), intent (in), optional :: pre, post, anchor, newtab

        character(len=MAX_LEN_QUERY_STRING) :: make_href
        character(len=MAX_CGI_WRK_LEN) :: cgi_wrk
        integer :: kStart, checkSum

        ! term provided ?
        if (present(A9)) then
            kStart = A9
        else ! force to be currentTerm
            kStart = currentTerm
        end if

        ! the function, user name and target term
        wrkCipher = 'T='//currentTime//'&F='//trim(itoa(fn))//'&N='//trim(USERNAME)//'&A9='//itoa(kStart)

        ! the arguments to the function
        if (present(A1)) then
            call cgi_url_encode(A1,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A1='//cgi_wrk
        end if
        if (present(A2)) then
            call cgi_url_encode(A2,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A2='//cgi_wrk
        end if
        if (present(A3)) then
            call cgi_url_encode(A3,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A3='//cgi_wrk
        end if
        if (present(A4)) then
            call cgi_url_encode(A4,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A4='//cgi_wrk
        end if
        if (present(A5)) then
            call cgi_url_encode(A5,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A5='//cgi_wrk
        end if

        ! calculate checksum
        checkSum = calculate_check_sum(wrkCipher)

        ! obfuscate
        call random_number(harvest)
        kStart = MAX_LEN_QUERY_STRING/2 + int(harvest*MAX_LEN_QUERY_STRING/2)
        call encrypt(queryEncryptionKey(:kStart), wrkCipher)
        wrkCipher = trim(wrkCipher)//itoa(kStart)

        ! the target
        if (present(newtab)) then
            wrkCipher = '<a target='//newtab//' href="'//trim(CGI_PATH)//'?r='//trim(itoa(len_trim(wrkCipher)))// &
                '&s='//trim(itoa(checkSum))//'&q='//trim(wrkCipher)
        else
            wrkCipher = '<a href="'//trim(CGI_PATH)//'?r='//trim(itoa(len_trim(wrkCipher)))// &
                '&s='//trim(itoa(checkSum))//'&q='//trim(wrkCipher)
        end if

        ! preamble (text before href)
        if (present(pre)) wrkCipher = pre//wrkCipher

        ! the anchor
        if (present(anchor)) wrkCipher = trim(wrkCipher)//'#'//anchor

        ! end href & the label
        wrkCipher = trim(wrkCipher)//'">'//trim(label)//'</a>'

        ! the text after the href
        if (present(post)) wrkCipher = trim(wrkCipher)//post

        make_href = wrkCipher

    end function make_href


    subroutine make_form_start(device, fn, A1, A2, A3, A4, A5, A9, enctype)

        integer, intent (in) :: device, fn
        integer, intent (in), optional :: A9
        character(len=*), intent (in), optional :: A1, A2, A3, A4, A5, enctype

        character(len=MAX_CGI_WRK_LEN) :: cgi_wrk
        integer :: kStart, checkSum

        ! term provided ?
        if (present(A9)) then
            kStart = A9
        else ! force to be currentTerm
            kStart = currentTerm
        end if

        ! the function, user name and target term
        wrkCipher = 'T='//currentTime//'&F='//trim(itoa(fn))//'&N='//trim(USERNAME)//'&A9='//itoa(kStart)

        ! the arguments to the function
        if (present(A1)) then
            call cgi_url_encode(A1,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A1='//cgi_wrk
        end if
        if (present(A2)) then
            call cgi_url_encode(A2,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A2='//cgi_wrk
        end if
        if (present(A3)) then
            call cgi_url_encode(A3,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A3='//cgi_wrk
        end if
        if (present(A4)) then
            call cgi_url_encode(A4,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A4='//cgi_wrk
        end if
        if (present(A5)) then
            call cgi_url_encode(A5,cgi_wrk)
            wrkCipher = trim(wrkCipher)//'&A5='//cgi_wrk
        end if

        ! calculate checksum
        checkSum = calculate_check_sum(wrkCipher)

        ! obfuscate
        call random_number(harvest)
        kStart = MAX_LEN_QUERY_STRING/2 + int(harvest*MAX_LEN_QUERY_STRING/2)
        call encrypt(queryEncryptionKey(:kStart), wrkCipher)
        wrkCipher = trim(wrkCipher)//itoa(kStart)

        if (present(enctype)) then
            cgi_wrk = enctype
        else
            cgi_wrk = SPACE
        end if

        write(device,AFORMAT) &
          '<form name="input" method="post" action="'//trim(CGI_PATH)//'"'//trim(cgi_wrk)//'>', &
          '<input type="hidden" name="s" value="'//trim(itoa(checkSum))//'">', &
          '<input type="hidden" name="q" value="'//trim(wrkCipher)//'">', &
          '<input type="hidden" name="r" value="'//trim(itoa(len_trim(wrkCipher)))//'">'

    end subroutine make_form_start



    subroutine cgi_url_encode(str_in, str_out)
    ! encode special/reserved characters in str_in using hex representation

        character(len=*), intent (in) :: str_in
        character(len=*), intent (out) :: str_out

        character(len=1) :: ch
        integer :: code, i, idx_out, j, k, len_str_out
        logical :: encode

        str_out = SPACE ! default return values
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

        str_out = SPACE ! default return values
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

#if defined DBcgi
        call html_comment('cgi_url_decode() = '//str_out)
#endif

    end subroutine cgi_url_decode



    subroutine cgi_get_name_value(string, lname, rvalue, ierr)
    ! parse [lname=][rvalue&] from string, rvalue will be string

        character(len=*), intent (in out) :: string
        character(len=*), intent (in) :: lname
        character(len=*), intent (out) :: rvalue
        integer, intent (out) :: ierr

        integer :: i_start, j_end, l_string, l_lname

        ! default return values
        rvalue = SPACE
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
#if defined DBcgi
        call html_comment('cgi_get_name_value() : '//lname//'='//trim(rvalue)//', ierr='//itoa(ierr))
#endif
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
            rvalue = SPACE
        else
            call cgi_url_decode(tmpIN, tmpOUT) ! decode
            rvalue = trim(tmpOUT)
        end if
#if defined DBcgi
        call html_comment('cgi_get_named_string() : '//lname//'='//trim(rvalue)//', ierr='//itoa(ierr))
#endif
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
#if defined DBcgi
        call html_comment('cgi_get_named_integer() : '//lname//'='//itoa(rvalue)//', ierr='//itoa(ierr))
#endif
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
        lname  = SPACE
        rvalue = SPACE
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
#if defined DBcgi
        call html_comment('cgi_get_wild_name_value() : '//wild//lname//'= '//rvalue)
#endif
    end subroutine cgi_get_wild_name_value


    subroutine FCGI_getquery( unitNo )
        ! Retrieve FastCGI environment variables DOCUMENT_URI and QUERY_STRING
        ! Invoked after FCGI_Accept() has completed
        ! Write debugging information to file unit number 'unitNo', which must already be open
        ! Debugging information should be <!-- HTML remark -->

        integer, intent(in)               :: unitNo

        integer                           :: i, j, lenDivider
        integer                           :: iLen, nRet, jLen!, kLen
        character(len=7)                  :: cLen

        character(len=MAX_LEN_FILE_PATH)  :: tName, divider !, httpVariable
        logical :: boundaryReached !, isText

        ! write to the beginning of file unitNo
        rewind (unitNo)

        call html_comment('FCGI_getquery()')

        call get_environment_variable('HTTP_USER_AGENT', value=HTTP_USER_AGENT, length=iLen, status=i)
        if (iLen==0) HTTP_USER_AGENT = 'test'
        call html_comment('HTTP_USER_AGENT='//HTTP_USER_AGENT)

!        call get_environment_variable('REMOTE_PORT', value=httpVariable, length=iLen, status=i)
!        call html_comment('REMOTE_PORT='//trim(httpVariable)//SPACE//itoa(iLen)//itoa(i))
!
!        call get_environment_variable('REMOTE_IDENT', value=httpVariable, length=iLen, status=i)
!        call html_comment('REMOTE_IDENT='//trim(httpVariable)//SPACE//itoa(iLen)//itoa(i))
!
!        call get_environment_variable('REMOTE_HOST', value=httpVariable, length=iLen, status=i)
!        call html_comment('REMOTE_HOST='//trim(httpVariable)//SPACE//itoa(iLen)//itoa(i))
!
!        call get_environment_variable('REMOTE_USER', value=httpVariable, length=iLen, status=i)
!        call html_comment('REMOTE_USER='//trim(httpVariable)//SPACE//itoa(iLen)//itoa(i))

        ! the remote IP
        call get_environment_variable('REMOTE_ADDR', value=REMOTE_ADDR, length=iLen, status=i)
        if (iLen==0) REMOTE_ADDR = 'test'
        call html_comment('REMOTE_ADDR='//REMOTE_ADDR//SPACE//itoa(iLen) )

        ! the requested script ('/' if none)
        call get_environment_variable('DOCUMENT_URI', value=DOCUMENT_URI)
        iLen = len_trim(DOCUMENT_URI)
        if ( iLen == 0 ) then
            ! default is /
            DOCUMENT_URI = '/'
        endif
#if defined DBcgi
        call html_comment('DOCUMENT_URI='//DOCUMENT_URI(:iLen))
#endif

        ! for other environment variables, see <nginx directory>/conf/fastcgi_params
        !call html_comment('KEY='//queryEncryptionKey)

        ! QUERY_STRING (request method was GET) ?
        call get_environment_variable( "QUERY_STRING", value=QUERY_STRING, length=iLen )
        if ( iLen > 0 ) then
            if (iLen>MAX_LEN_QUERY_STRING) QUERY_STRING = '(QUERY_STRING too long.)'
            return
        end if

        ! request method was POST; get CONTENT_LENGTH
        call get_environment_variable( "CONTENT_LENGTH", value=cLen, length=iLen )
        if ( iLen==0 ) then ! nothing sent
            QUERY_STRING = '(QUERY_STRING is empty.)'
            return
        end if

        ! assume request was POST
        call html_comment('CONTENT_LENGTH='//trim(cLen))
        read( cLen, * ) iLen
        QUERY_STRING = SPACE

        ! get characters up to first CR (or last character) into work area
        divider = SPACE
        lenDivider = 0
        wrkCipher = SPACE ! initialize work area
        nRet = 0 ! characters retrieved
        boundaryReached = .false.
        call FCGI_getline (nRet, iLen, wrkCipher, jLen, MAX_LEN_QUERY_STRING)
#if defined DBcgi
        call html_comment('INITIAL DATA='//trim(wrkCipher))
#endif

        ! multipart/form-data ?
        if (wrkCipher(1:5)=='-----') then
            divider = wrkCipher
            lenDivider = len_trim(divider)-2
            boundaryReached = .true.
        end if

        if (.not. boundaryReached) then ! not multipart
            QUERY_STRING = wrkCipher(:jLen)
            return
        end if

        ! build QUERY_STRING and upload file from multipart/form-data
        do while (nRet<iLen)

            ! get characters up to next CR
            call FCGI_getline (nRet, iLen, wrkCipher, jLen, MAX_LEN_QUERY_STRING)
            boundaryReached = divider(:lenDivider)==wrkCipher(:lenDivider)
            if (boundaryReached) then ! boundary reached
#if defined DBcgi
                call html_comment('#1 PARTIAL_QUERY_STRING='//trim(QUERY_STRING))
#endif
                cycle
            end if

           ! check for filename="..."
            j = index(wrkCipher(:jLen), '; filename="')
            if (j>0) then ! get filename= and name=
                tName = wrkCipher(j+12:jLen-3)

                wrkCipher(j:) = SPACE ! erase
                jLen = j-1
                j = index(wrkCipher(:jLen), '; name="')
                QUERY_STRING = trim(QUERY_STRING)//'&'//wrkCipher(j+8:jLen-1)//'='//tName
#if defined DBcgi
                call html_comment(itoa(nRet)//itoa(jLen)//wrkCipher(j+8:jLen-1)//'='//tName)
#endif
                if (len_trim(tName)==0) exit ! no file selected

                ! get content-type
                call FCGI_getline (nRet, iLen, wrkCipher, jLen, MAX_LEN_QUERY_STRING)
                do while (wrkCipher(jLen:jLen)<'a' .or. wrkCipher(jLen:jLen)>'z')
                    jLen = jLen-1
                end do
                QUERY_STRING = trim(QUERY_STRING)//'&content='//wrkCipher(15:jLen)
#if defined DBcgi
                call html_comment(itoa(nRet)//itoa(jLen)//'content='//wrkCipher(15:jLen))
#endif
                !isText = index(wrkCipher(:jLen), 'text')>0

                ! get empty line
                call FCGI_getline (nRet, iLen, wrkCipher, jLen, MAX_LEN_QUERY_STRING)

                ! write file contents
                open(unit=unitETC, file=trim(dirUPLOADS)//tName, status='unknown')
                rewind(unitETC)
                boundaryReached = .false.
                do while (.not. boundaryReached)

                    ! fill buffer from POSTed data
                    call FCGI_getline (nRet, iLen, wrkCipher, jLen, MAX_LEN_QUERY_STRING)
                    boundaryReached = divider(:lenDivider)==wrkCipher(:lenDivider)

                    if (boundaryReached) exit

                    if (jLen/=2) then
                        write(unitETC, AFORMAT, advance='no') wrkCipher(:jLen)
#if defined DBcgi
                        call html_comment('PARTIAL_CONTENT (long)='//wrkCipher(:jLen))
#endif
                    elseif (wrkCipher(:jLen)/=SPACE) then
                        write(unitETC, AFORMAT, advance='no') wrkCipher(:jLen)
#if defined DBcgi
                        call html_comment('PARTIAL_CONTENT (short)='//wrkCipher(:jLen))
#endif
                    end if

                end do
                close(unitETC)

            else ! Content-Disposition: form-data; name=value

                ! get name=
                j = index(wrkCipher(:jLen), '; name="')
                tName = wrkCipher(j+8:jLen-3)

                ! get empty line
                call FCGI_getline (nRet, iLen, wrkCipher, jLen, MAX_LEN_QUERY_STRING)

                ! get value
                call FCGI_getline (nRet, iLen, wrkCipher, jLen, MAX_LEN_QUERY_STRING)
                QUERY_STRING = trim(QUERY_STRING)//'&'//trim(tName)//'='//wrkCipher(:jLen-2)
#if defined DBcgi
                call html_comment('#2 PARTIAL_QUERY_STRING='//trim(QUERY_STRING))
#endif
            end if

        end do
        call html_comment('QUERY_STRING='//trim(QUERY_STRING))

    end subroutine FCGI_getquery


    subroutine FCGI_getline (current, maxChars, str, strLen, maxStrLen)

        integer, intent (in out) :: current
        integer, intent (in) :: maxChars, maxStrLen
        character(len=*), intent (out) :: str
        integer, intent (out) :: strLen

        character :: ch

        strLen = 0
        do while (current<maxChars)
            ch = FCGI_getchar()
            current = current + 1
            strLen = strLen+1
            str(strLen:strLen) = ch
            if (ch==LF) exit
            if (strLen==maxStrLen) exit
        end do
#if defined DBcgi
!        call html_comment('FCGI_getline(): '//trim(itoa(strlen))//' of '// &
!            trim(itoa(maxStrLen))//', "'//trim(str)//'"')
        call html_comment('FCGI_getline(): '//trim(itoa(current))//'/'//trim(itoa(strlen))//' - '//str(:strLen) )
#endif

    end subroutine FCGI_getline


    subroutine FCGI_putfile ( unitNo )
        ! Copy file 'unitNo' line by line to the webserver via FCGI_puts()
        ! File must already exist, expected to contain the response to some query

        integer, intent(in) :: unitNo
        integer :: iStat

        call html_comment('FCGI_putfile()')

        ! flush any pending writes
        flush(unitNo)

        ! copy line by line to webserver
        rewind(unitNo)
        do while (.true.)
            read(unitNo, AFORMAT, iostat=iStat) cipher
            if (iStat < 0) exit ! no more lines
            if (cipher(1:5)=='DBG::') cycle
            iStat = FCGI_puts (trim(cipher)//NUL) ! FCGI_puts expects NULL terminated strings
        end do

    end subroutine FCGI_putfile


    subroutine html_copyright(device)
        integer, intent(in) :: device

        write(device,AFORMAT) &
            b_small//b_italic//PROGNAME//' (version '//VERSION//') '//nbsp//COPYRIGHT//linebreak, &
            'This program comes with ABSOLUTELY NO WARRANTY; for details see the GNU GENERAL PUBLIC LICENSE Version 3 ', &
            '(<a target="0" href="http://www.gnu.org/licenses/gpl-3.0.html">GPLv3</a>).'//linebreak, &
            'This program is free software, and you are welcome to redistribute it under certain conditions; ', &
            ' see the GPLv3 for details.'//linebreak, &
            ' Support for program development was provided by: '// &
            ' University of the Philippines Los Banos (1997-2001); '// &
            ' BalikScientist Program of the Department of Science and Technology (2010); '// &
            ' Isabela State University (2011); and '// &
            ' Cagayan State University (2012, 2013). ', &
            'The source code is available at <a target="0" href="'//WEB//'">'//WEB//'</a>'//linebreak, &
            CONTACT//e_italic//e_small//horizontal

    end subroutine html_copyright


    subroutine html_home_page(device, mesg)
        integer, intent(in) :: device
        character(len=*), intent(in) :: mesg

        call html_comment('html_home_page('//trim(mesg)//')')
        write(device,AFORMAT) &
            '<html><head><title>'//trim(UniversityCode)//SPACE//PROGNAME//'</title></head><body>', &
            '<h1>'//trim(UniversityCode)//SPACE//PROGNAME//'</h1>', &
            '<h3>'//red//trim(mesg)//e_color//'</h3>', &
            b_small//b_italic//'<a href="'//trim(SERVER_PROTOCOL)//trim(SERVER_NAME)//'">Home</a>'//e_italic//e_small

    end subroutine html_home_page


end module UTILITIES

