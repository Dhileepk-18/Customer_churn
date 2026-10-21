@echo off
title Predicting Customer Churn - Shiny Dashboard
echo ==============================================================================
echo  Predicting Customer Churn - Starting R Shiny Dashboard
echo ==============================================================================
echo.

set "RSCRIPT="

:: Check if Rscript is in PATH
where Rscript >nul 2>nul
if %ERRORLEVEL% equ 0 (
    set "RSCRIPT=Rscript"
)

:: Check known 64-bit R installation paths
if not defined RSCRIPT (
    if exist "C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe" (
        set "RSCRIPT=C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe"
    )
)

:: Check any installed R versions under Program Files
if not defined RSCRIPT (
    for /d %%D in ("C:\Program Files\R\R-*") do (
        if exist "%%D\bin\x64\Rscript.exe" (
            set "RSCRIPT=%%D\bin\x64\Rscript.exe"
        )
    )
)

if not defined RSCRIPT (
    echo [ERROR] Rscript.exe was not found on your system!
    echo Please verify that R is installed from https://cloud.r-project.org/
    echo.
    pause
    exit /b 1
)

echo [OK] Using R executable: "%RSCRIPT%"
echo [OK] Launching Shiny Web Dashboard in your browser ...
echo [INFO] Close this command prompt or press Ctrl+C when you want to stop the app.
echo.

cd /d "%~dp0"
"%RSCRIPT%" -e "tryCatch(shiny::runApp('.', port = 3838, host = '127.0.0.1', launch.browser = TRUE), error = function(e) { cat('Port 3838 busy, switching to automatic port...\n'); shiny::runApp('.', host = '127.0.0.1', launch.browser = TRUE) })"

pause
