@echo off

rem
rem USAGE: %0 UNIV YEAR TERM ACTION  (Note: Arguments are case-sensitive!)
rem   UNIV is the University code
rem   YEAR is the year when the current School Year started (ex. 2012 for SY 2012-13)
rem   TERM is one of: 1, 2, S (1=First Semester, 2=Second Semester, S=Summer Term)
rem
rem   ACTION                Period=I   PERIOD=II  PERIOD=III PERIOD=IV
rem      Classlists (S)        X                      
rem      Probabilistic (B)                X                     
rem      Advising (S)                     X
rem      Gradesheets (S)                             X
rem      Checklists (B)                                         X
rem      Deterministic (B)                                      X
rem      Preenlist (B)                                          X
rem
rem      Schedules      (S)    X          X          X          X
rem      Resetpasswords (B)    X          X          X          X
rem      Students       (B)    X          X          X          X
rem      Import         (B)    X          X          X          X
rem
rem   Legend: S=server-mode, B=batch-mode, X=execute during this Period
rem

SETLOCAL ENABLEEXTENSIONS
SETLOCAL DISABLEDELAYEDEXPANSION

rem 
rem =======================================
rem There must be at least 4 arguments: UNIV YEAR TERM ACTION
rem =======================================
rem
set ERRMSG=ERROR: There must be at least 4 arguments
if "z%4"=="z" goto usage


rem =======================================
:checkUNIV
set ERRMSG=ERROR: UNIV '%1' not recognized.
rem =======================================

if "z%1"=="zUPLB" (
    set HEEDS_Executable=.\HEEDS_UPLB_%4
    set University_Port=600
    goto checkACTION
)

if "z%1"=="zISU" (
    set HEEDS_Executable=.\HEEDS_SIAS_%4
    set University_Port=590
    goto checkACTION
)

if "z%1"=="zCSU" (
    set HEEDS_Executable=.\HEEDS_SIAS_%4
    set University_Port=580
    goto checkACTION
)

if "z%1"=="zKASC" (
    set HEEDS_Executable=.\HEEDS_SIAS_%4
    set University_Port=570
    goto checkACTION
)

if "z%1"=="zCSU-Demo" (
    set HEEDS_Executable=.\HEEDS_SIAS_%4
    set University_Port=560
    goto checkACTION
)

if "z%1"=="zDEMO" (
    set HEEDS_Executable=.\HEEDS_UPLB_%4
    set University_Port=550
    goto checkACTION
)

goto usage


rem =======================================
:checkACTION
rem =======================================

if "z%4"=="zClasslists" (
    set Port_Number=%University_Port%10
    goto runSERVER
)

if "z%4"=="zAdvising" (
    set Port_Number=%University_Port%20
    goto runSERVER
)

if "z%4"=="zGradesheets" (
    set Port_Number=%University_Port%30
    goto runSERVER
)

if "z%4"=="zSchedules" (
    set Port_Number=%University_Port%40
    goto runSERVER
)


rem =======================================
:runBATCH
rem =======================================
if exist %HEEDS_Executable%.exe (
    %HEEDS_Executable% %*
) else (
    goto noEXE
)
goto quit


rem =======================================
:runSERVER
rem =======================================
if exist %HEEDS_Executable%.exe (
    if "z%5"=="z" (
        echo spawn-fcgi -a 127.0.0.1 -p %Port_Number% -f "%HEEDS_Executable% %*"
        spawn-fcgi -a 127.0.0.1 -p %Port_Number% -f "%HEEDS_Executable% %*"
    ) else (
        echo %HEEDS_Executable% %*
        %HEEDS_Executable% %*
    )
    goto quit
)


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
echo USAGE: %0 UNIV ACTION YEAR TERM (Note: Arguments are case-sensitive!)
echo   UNIV is one of: UPLB, ISU, CSU, KASC, CSU-Demo
echo   YEAR is the year when the current School Year started (ex. 2012 for SY 2012-13)
echo   TERM is one of: 1, 2, S (1=First Semester, 2=Second Semester, S=Summer Term)
echo   ACTION is one of:
echo     Classlists - Add/delete classes of students for current term
echo     Schedules - Edit Schedule of Classes for current term
echo     Advising - Advise students for next term
echo     Gradesheets - Enter grades for current term
echo     Checklists - Generate checklists
echo     Deterministic - Deterministic demand for subjects (needs analysis) next term
echo     Probabilistic - Probabilistic demand for subjects (needs analysis) next term
echo     Preenlist - Preenlist students into classes 
echo     Resetpasswords - Reset all user passwords; will display password for Registrar
echo     Students - Create list of students from available FINALGRADE.CSV for YEAR
echo     Import - Import raw data into HEEDS format
echo.


rem =======================================
:quit
rem =======================================
echo.

