#include "windows.h"

int main(void)
{
  MessageBox(NULL, "Voulez-vous le composant graphique", "graphique", MB_YESNOCANCEL | MB_DEFBUTTON1 | MB_ICONQUESTION);
  system("opam switch list");
  return 0;
}
/*
int WINAPI
WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nShowCmd)
{
    char msg[] = "Hello, world!\n";
    HANDLE stdout = GetStdHandle(STD_OUTPUT_HANDLE);
    WriteFile(stdout, msg, sizeof(msg), (DWORD[]){0}, NULL);
    return 0;
}
*/
