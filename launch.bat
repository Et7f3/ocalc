@set PATH=%cd%\deps\bin;%cd%\deps\lib;%PATH%

@if not defined %OCAMLLIB% (set OCAMLLIB=%cd%\deps\lib\ocaml) else (set OCAMLLIB=%cd%\deps\lib\ocaml;%OCAMLLIB%)

@start XWin.exe -multiwindow

@set DISPLAY=:0

@%SystemRoot%\System32\timeout.exe 2

@.\bin\final\topGui.exe

@taskkill /IM:XWin.exe
