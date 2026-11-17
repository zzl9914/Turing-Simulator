@echo off
chcp 65001 >nul
cd /d "%~dp0"

netstat -ano | findstr /R /C:":8765 .*LISTENING" >nul
if %errorlevel%==0 (
  start "" http://127.0.0.1:8765/web/
  echo 服务已在运行，已打开页面。
  pause
  exit /b 0
)

start "" cmd /c "ping -n 2 127.0.0.1 >nul & start http://127.0.0.1:8765/web/"
python -m http.server 8765
if errorlevel 1 (
  echo.
  echo 启动失败。请确认已安装 Python，并且当前目录是 Turing Machine。
  pause
)
