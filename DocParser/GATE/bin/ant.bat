@echo off

if "%OS%"=="Windows_NT" goto nt
if "%OS%"=="WINNT" goto nt
goto notnt

:nt
rem If we're running on an NT-style system, we can use %~dp0 to get the
rem directory containing this batch file, and setlocal so we don't clobber
rem the real ANT_HOME.
setlocal
set REAL_ANT="%~dp0real-ant.bat"

goto donesetup
:notnt
rem if we're on 95/98/ME we don't have %~dp0, so try and locate gate\bin
rem using GATE_HOME.
if "%GATE_HOME%"=="" goto noGateHome
set REAL_ANT="%GATE_HOME%\bin\real-ant.bat"

:donesetup

rem Clear ANT_HOME, so we don't accidentally pick up libs from an older Ant
rem version
set ANT_HOME=

%REAL_ANT% %1 %2 %3 %4 %5 %6 %7 %8 %9
goto end

:noGateHome
echo Can't find real-ant.bat - make sure GATE_HOME is set correctly
:end
