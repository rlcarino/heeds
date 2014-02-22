@echo off

rem
rem USAGE: %0 UNIV YEAR TERM ACTION  (Note: Arguments are case-sensitive!)
rem   UNIV is the University code
rem   YEAR is the year when the current School Year started (ex. 2012 for SY 2012-13)
rem   TERM is one of: 1, 2, S (1=First Semester, 2=Second Semester, S=Summer Term)
rem   ACTION is one of Classlists, Advising, Gradesheets, Preregistration, Checklists, Restore, Import
rem

SETLOCAL ENABLEEXTENSIONS
SETLOCAL DISABLEDELAYEDEXPANSION

rem =======================================
:checkUNIV
rem =======================================

set University=CSU-Aparri
set University_Port=58020
if "z%1"=="z%University%" goto checkACTION

set University=CSU-Carig
set University_Port=58030
if "z%1"=="z%University%" goto checkACTION

set University=CSU-Gonzaga
set University_Port=58040
if "z%1"=="z%University%" goto checkACTION

set University=CSU-Lal-lo
set University_Port=58050
if "z%1"=="z%University%" goto checkACTION

set University=CSU-Lasam
set University_Port=58060
if "z%1"=="z%University%" goto checkACTION

set University=CSU-Piat
set University_Port=58070
if "z%1"=="z%University%" goto checkACTION

set University=CSU-Sanchez-Mira
set University_Port=58080
if "z%1"=="z%University%" goto checkACTION

set University=BNHS
set University_Port=59000
if "z%1"=="z%University%" goto checkACTION

set University=DEMO
set University_Port=60000
if "z%1"=="z%University%" goto checkACTION

set University=DEMO-UNIV
set University_Port=60010
if "z%1"=="z%University%" goto checkACTION

set University=DEMO-SIAS
set University_Port=60020
if "z%1"=="z%University%" goto checkACTION

set University=MUW
set University_Port=60030
if "z%1"=="z%University%" goto checkACTION

rem default UNIV
set University=CSU-Andrews
set University_Port=58010


rem =======================================
:checkACTION
rem =======================================

set HEEDS_Executable=.\HEEDS_STATIC
set RUN_COMMAND=%HEEDS_Executable% %University% %2 %3 %4 %5 %6

if "zRestore"=="z%4" goto runACTION

if "zChecklists"=="z%4" goto runACTION

if "zImport"=="z%4" goto runACTION

rem =======================================
rem Assume a Server-mode ACTION
rem =======================================

set HEEDS_Executable=.\HEEDS_SERVER
set RUN_COMMAND=spawn-fcgi -a 127.0.0.1 -p %University_Port% -f "%HEEDS_Executable% %University% %2 %3 %4 %5 %6"

rem =======================================
:runACTION
rem =======================================

if not exist %HEEDS_Executable%.exe goto noEXE

echo %RUN_COMMAND% 
%RUN_COMMAND% 
goto quit

rem =======================================
:noEXE
rem =======================================

echo ERROR: %HEEDS_Executable% not found!
goto quit


rem =======================================
:usage
rem =======================================
echo %ERRMSG%
echo.
echo USAGE: %0 UNIV YEAR TERM ACTION (Note: Arguments are case-sensitive!)
echo   UNIV is the code/abbreviated University name
echo   YEAR is the year when the current School Year started (ex. 2012 for SY 2012-13)
echo   TERM is the current term (1=First Semester, 2=Second Semester, S=Summer Term)
echo   ACTION is one of:
echo     Classlists - Edit Schedule of Classes and Add/delete classes of students
echo     Advising - Advise students and Edit Schedule of Classes
echo     Gradesheets - Enter grades
echo     Preregistration - Generate initial timetables of students (careful, existing classlists for TERM will be overwritten)
echo     Checklists - Generate checklists from grade files (careful, existing student records will be overwritten)
echo     Restore - Restore from backup (careful, existing data will be overwritten)
echo     Import - Convert custom CSV data into HEEDS XML format (careful, existing data will be overwritten)
echo.

rem =======================================
:quit
rem =======================================
echo.

