set PATH=%PATH%;%cd%\deps\bin
if not defined %OCAMLLIB% (set OCAMLLIB=%cd%\deps\lib\ocaml) else (set OCAMLLIB=%OCAMLLIB%;%cd%\deps\lib\ocaml)
.\deps\bin\ocaml.exe unix.cma compile.ml nettoyer construire
