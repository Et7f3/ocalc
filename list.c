#include <windows.h>
#include <commctrl.h>

#ifdef STRICT
  WNDPROC wpOrigEditProc ;
#else
  FARPROC wpOrigEditProc ;
#endif

LRESULT CALLBACK MainWndProc(HWND, UINT, WPARAM, LPARAM);

LRESULT APIENTRY EditProc(HWND, UINT, WPARAM, LPARAM);

HINSTANCE hinst;

int WINAPI WinMain(HINSTANCE hinstance, HINSTANCE hPrevInstance,
                                                  LPSTR lpCmdLine, int nCmdShow)
{
    HWND hwnd;
    MSG msg;
    WNDCLASS wc;

    hinst = hinstance;

    wc.style = 0 ;
    wc.lpfnWndProc = MainWndProc;
    wc.cbClsExtra = 0;
    wc.cbWndExtra = 0;
    wc.hInstance = hinstance;
    wc.hIcon = NULL;
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground = NULL;
    wc.lpszMenuName =  NULL;
    wc.lpszClassName = "MaWinClass";

    if(!RegisterClass(&wc)) return FALSE;

    hwnd = CreateWindow("MaWinClass", "List View", WS_OVERLAPPEDWINDOW,
                                   CW_USEDEFAULT, CW_USEDEFAULT, 400, 300,
                                                   NULL, NULL, hinstance, NULL);
    if (!hwnd)  return FALSE;

    ShowWindow(hwnd, nCmdShow);

    while (GetMessage(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
    return msg.wParam;
}

/******************************************************************************/

LRESULT CALLBACK MainWndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    static HWND hEdit;
    static HWND hListView;

    switch (uMsg)
    {
        case WM_CREATE:
           {
            LV_ITEM lvi;
            HINSTANCE dllhinst;
            HICON hIcon;
            HIMAGELIST himgList;
            InitCommonControls();
            himgList = ImageList_Create(GetSystemMetrics(SM_CXICON),
                               GetSystemMetrics(SM_CYICON), ILC_COLOR32 , 3, 3);
            ImageList_SetBkColor(himgList, GetSysColor(COLOR_WINDOW));
            dllhinst = LoadLibrary("shell32.dll");
            hIcon = LoadIcon(dllhinst, MAKEINTRESOURCE(4));
            ImageList_AddIcon(himgList, hIcon);
            hIcon = LoadIcon(dllhinst, MAKEINTRESOURCE(6));
            ImageList_AddIcon(himgList, hIcon);
            hIcon = LoadIcon(dllhinst, MAKEINTRESOURCE(12));
            ImageList_AddIcon(himgList, hIcon);
            FreeLibrary(dllhinst);

            ZeroMemory(&lvi, sizeof(LV_ITEM));
            lvi.mask = LVIF_TEXT | LVIF_IMAGE ;
            hListView =CreateWindowEx(WS_EX_CLIENTEDGE , WC_LISTVIEW, "",
                  WS_CHILD | WS_VISIBLE , 0, 0, 0, 0, hwnd, NULL, hinst, NULL);

            hEdit =CreateWindowEx(WS_EX_CLIENTEDGE , "edit",
                                       "\r\nTest de la ListView.",
              WS_CHILD | WS_VISIBLE | ES_MULTILINE | ES_WANTRETURN | WS_VSCROLL,
                                           0, 0, 0, 0, hwnd, NULL, hinst, NULL);
            SetClassLong(hEdit, GCL_HCURSOR, (LONG)LoadCursor(NULL, IDC_ARROW));
            wpOrigEditProc = (PVOID)SetWindowLong(hEdit,
                                                   GWL_WNDPROC, (LONG)EditProc);

            ListView_SetImageList(hListView, himgList, LVSIL_NORMAL);
            lvi.pszText = "Option 3";
            lvi.iImage = 0;
            ListView_InsertItem(hListView, &lvi);
            lvi.pszText = "Option 2";
            lvi.iImage = 1;
            ListView_InsertItem(hListView, &lvi);
            lvi.pszText = "Option 1";
            lvi.iImage = 2;
            ListView_InsertItem(hListView, &lvi);
            return 0;
           }

        case WM_NOTIFY:
           {
             LPNM_LISTVIEW pnlv = (LPNM_LISTVIEW)lParam;
             if(pnlv->hdr.code == LVN_ITEMCHANGED)
              {
                if(pnlv->iItem == 0)
                   SetWindowText(hEdit,
                         "\r\nBien !\r\n\r\nVous avez sélectionné l'option 1.");
                if(pnlv->iItem == 1)
                   SetWindowText(hEdit,
                        "\r\nBravo !\r\n\r\nVous avez sélectionné l'option 2.");
                if(pnlv->iItem == 2)
                   SetWindowText(hEdit,
                      "\r\nParfait !\r\n\r\nVous avez sélectionné l'option 3.");
              }
             return 0;
           }
        case WM_SIZE :
            MoveWindow(hEdit, 100, 0, LOWORD(lParam)-100, HIWORD(lParam), TRUE);
            MoveWindow(hListView, 0, 0, 100, HIWORD(lParam), TRUE);
            return 0;

        case WM_DESTROY :
            SetWindowLong(hEdit, GWL_WNDPROC,(LONG) wpOrigEditProc);
            PostQuitMessage(0);
            return 0;

        default :
            return DefWindowProc(hwnd, uMsg, wParam, lParam);
    }
}

/******************************************************************************/

LRESULT APIENTRY EditProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
   if (uMsg == WM_SETFOCUS || uMsg == WM_CONTEXTMENU || uMsg == WM_CHAR ||
                                                             uMsg == WM_KEYDOWN)
                                                                       return 0;
   return CallWindowProc(wpOrigEditProc, hwnd, uMsg, wParam, lParam);
}
