@set PATH=%cd%\deps\bin;%PATH%

@if not defined %OCAMLLIB% (set OCAMLLIB=%cd%\deps\lib\ocaml) else (set OCAMLLIB=%cd%\deps\lib\ocaml;%OCAMLLIB%)

@.\deps\bin\ocaml.exe unix.cma compile.ml nettoyer construire -- %*
