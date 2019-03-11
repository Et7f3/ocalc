@set PATH=%cd%\deps\lib;%cd%\deps\bin;%PATH%
@start XWin.exe -multiwindow
@set DISPLAY=:0
@timeout 2
@%cd%\bin\final\topGui.exe
@taskkill /IM:XWin.exe
@exit
