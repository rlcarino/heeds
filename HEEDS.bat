@echo off

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
rem check UNIV
rem =======================================
set ERRMSG=ERROR: UNIV '%1' not recognized.

if NOT "z%1"=="zUPLB" goto notUPLB
set HEEDS_Base=.\HEEDS_MSWIN_UPLB
goto checkACTION


:notUPLB
if NOT "z%1"=="zCSU" goto notCSU
set HEEDS_Base=.\HEEDS_MSWIN_SIAS
goto checkACTION


:notCSU
if NOT "z%1"=="zISU" goto usage
set HEEDS_Base=.\HEEDS_MSWIN_SIAS
goto checkACTION

rem =======================================
:checkACTION
set ERRMSG=ERROR: ACTION '%4' not recognized.
rem =======================================

if NOT "z%4"=="zClasslists" goto notClasslists
set Port_Number=60000
goto spawnFCGI


:notClasslists
if NOT "z%4"=="zAdvising" goto notAdvising
set Port_Number=60010
goto spawnFCGI


:notAdvising
if NOT "z%4"=="zResetpasswords" goto notResetpasswords
goto runEXE


:notResetpasswords
if NOT "z%4"=="zChecklists" goto notChecklists
goto runEXE


:notChecklists
if NOT "z%4"=="zPredict" goto notPredict
goto runEXE


:notPredict
if NOT "z%4"=="zPreenlist" goto usage
goto runEXE


rem =======================================
:usage
rem =======================================
echo.
echo USAGE: %0 UNIV ACTION YEAR TERM  (Note: Arguments are case-sensitive!)
echo   UNIV is one of: UPLB, CSU, ISU
echo   ACTION is one of: Resetpasswords, Checklists, Predict, Preenlist, Classlists, Advising
echo   YEAR is the year when the current School Year started (ex. 2012 for SY 2012-13)
echo   TERM is one of: 1, 2, S (1=First Semester, 2=Second Semester, S=Summer Term)
echo.
echo %ERRMSG%
goto quit


rem =======================================
:runEXE
rem =======================================
set HEEDS_Executable=%HEEDS_Base%_BATCH.EXE
if exist %HEEDS_Executable% (
    %HEEDS_Executable% %*
) else (
    goto noEXE
)
goto quit


rem =======================================
:spawnFCGI
rem =======================================
set HEEDS_Executable=%HEEDS_Base%_SERVER.EXE
if exist %HEEDS_Executable% (
    echo spawn-fcgi -a 127.0.0.1 -p %Port_Number% -f "%HEEDS_Executable% %*"
    spawn-fcgi -a 127.0.0.1 -p %Port_Number% -f "%HEEDS_Executable% %*"
) else (
    goto noEXE
)
goto quit


rem =======================================
:noEXE
rem =======================================
echo ERROR: %HEEDS_Executable% not found!
goto quit


rem =======================================
:quit
rem =======================================
echo.

