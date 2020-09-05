/*
   GNU Maverik - a system for managing display and interaction in 
              Virtual Environment applications.
   Copyright (C) 2008  Advanced Interfaces Group

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.

   The authors can be contacted via:
   www   - http://aig.cs.man.ac.uk
   email - maverik@aig.cs.man.ac.uk
   mail  - Advanced Interfaces Group, Room 2.94, Kilburn Building, 
        University of Manchester, Manchester, M13 9PL, UK
*/


#include "maverik.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>
#include <d3d8.h>

#include "mavlib_d3d.h"

/* minimum tracking Y (over non-client height) */
/* (stops window from being shrunk too far) */
#define MAVLIB_MIN_Y 2


/* D3D globals */
LPDIRECT3D8 mavlib_D3D= NULL;
LPDIRECT3DDEVICE8 mavlib_D3DDevice= NULL;
LPDIRECT3DSURFACE8 mavlib_dummyBB= NULL;
LPDIRECT3DSURFACE8 mavlib_dummyZB= NULL;
D3DFORMAT mavlib_buffFormat;
D3DFORMAT mavlib_texRGBA= D3DFMT_A8R8G8B8;
D3DFORMAT mavlib_texRGB= D3DFMT_A8R8G8B8;
HINSTANCE mavlib_d3dlib= NULL;
D3DDISPLAYMODE mavlib_dm;

/* type of the D3D create function */
#ifdef __CYGWIN__
typedef struct IDirect3D8 * WINAPI (*d3dfunctype) (UINT);
#else
typedef FARPROC d3dfunctype;
#endif

/* current colour */
int mavlib_col_r= 0;
int mavlib_col_g= 0;
int mavlib_col_b= 0;

/* raster pos */
int mavlib_2d_x= 0;
int mavlib_2d_y= 300;

/* 2d text info */
typedef struct
{
  int win;
  char text[1000];
  int x;
  int y;
  COLORREF colour;
  HFONT font;
} MAVLIB_gfxString;

MAV_list *mavlib_stringList;

/* window info */
int mavlib_mainWin= -1;
int mavlib_creatingWin= 0;

/* Certain features only available to Cygwin users */
#ifdef __CYGWIN__
#define TME 1
#endif

/* font handles */
MAV_list *mavlib_fontList= NULL;

/* globals for window event handler */
int mavlib_win32EventInfo[50]; /* Event info stored here */
int mavlib_win32EventGood=0;   /* Indicates type of event */
int mavlib_win32Create=-1;     /* Indicates id of window for create event */
int mavlib_win32MouseCaught= 0; /* count of capture requests */

/* button press windows */
HWND mavlib_win32MouseWin[4]= {NULL, NULL, NULL, NULL};
int mavlib_win32MouseEventsPending= 0; /* for flushing button up events */

/* config file */
UINT mavlib_config_adapter= D3DADAPTER_DEFAULT;
D3DDEVTYPE mavlib_config_device= D3DDEVTYPE_HAL;
DWORD mavlib_config_vertex= D3DCREATE_SOFTWARE_VERTEXPROCESSING;
int mavlib_config_w= MAVLIB_WIDTH;
int mavlib_config_h= MAVLIB_HEIGHT;
int mavlib_config_d= MAVLIB_BITS;
D3DFORMAT mavlib_config_f= D3DFMT_R5G6B5;
D3DFORMAT mavlib_config_winFormat= D3DFMT_R5G6B5;
D3DFORMAT mavlib_config_fullFormat= D3DFMT_R5G6B5;
D3DFORMAT mavlib_config_winRGB= 0;
D3DFORMAT mavlib_config_winRGBA= 0;
D3DFORMAT mavlib_config_fullRGB= 0;
D3DFORMAT mavlib_config_fullRGBA= 0;
int mavlib_config_1a= 0;

/* options */
int mavlib_gfx_fullscreen= 0;
int mavlib_gfx_textureBits= 16;

/* flag for between begin and end scene */
int mavlib_gfx_inScene= 0;

/* screen handle */
HINSTANCE mavlib_dpy;

/* windows event handler */
LONG WINAPI mavlib_winEventHandler(HWND hwnd, UINT umsg, WPARAM wparam, LPARAM lparam);

/* mav_win_mouse HWND */
HWND mavlib_hwnd= NULL;

/* Data structure to store window and context of each MAV_window */

typedef struct
{
  MAV_window *mavwin;			/* maverik window handle */
  HDC hdc;				/* device context */
  HWND win;				/* win32 window handle */
  LPDIRECT3DSWAPCHAIN8 swapchain;	/* non-main window swap chain */
  LPDIRECT3DSURFACE8 backbuffer;	/* back buffer handle */
  LPDIRECT3DSURFACE8 zbuffer;		/* Z buffer handle */
  int win_x;				/* window parameters ... */
  int win_y;				/* (for switching back from */
  int win_width;			/* fullscreen mode) */
  int win_height;
  int width;				/* event values */
  int height;
  int resized;
  int set_aspect;			/* (flag for need aspect change) */
  int exposed;
  int mapped;
#ifdef TME
  TRACKMOUSEEVENT tme;			/* structure for tracking crossings */
#endif
  int enter;
  int leave;
} MAVLIB_winhand;

MAVLIB_winhand mavlib_winhand[MAV_MAX_WIN+1]; /* need extra space */
int mavlib_currwin= 0;


/**************************/
/*                        */
/* gfx utility functions  */
/*                        */
/**************************/

/* Begin scene and end scene */
void mavlib_beginScene(void *ignored)
{
  if (mavlib_D3DDevice->lpVtbl->BeginScene(mavlib_D3DDevice) != D3D_OK)
  {
	fprintf(stderr, "Warning: BeginScene failed\n");
  }
  else
  {
    mavlib_gfx_inScene= 1;
  }
}

void mavlib_endScene(void)
{
  if (mavlib_gfx_inScene)
  {
    MAVD3DWARN(mavlib_D3DDevice->lpVtbl->EndScene(mavlib_D3DDevice),
	"end scene failed");

    mavlib_gfx_inScene= 0;
  }
}

/* open D3D */
void mavlib_D3DSetUp(void)
{
  d3dfunctype d3dfunc;

/* get handle to D3D library */
  mavlib_d3dlib= LoadLibrary("D3D8.DLL");
  if (!mavlib_d3dlib)
  {
    fprintf(stderr, "Error: LoadLibrary (D3D8.DLL) failed\n");
    exit(1);
  }

/* get create D3D interface function */
  d3dfunc= (d3dfunctype) GetProcAddress(mavlib_d3dlib, "Direct3DCreate8");
  if (!d3dfunc)
  {
    fprintf(stderr, "Error: GetProcAddress (Direct3DCreate8) failed\n");
    exit(1);
  }

/* get main D3D interface */
#ifdef __CYGWIN__
  mavlib_D3D= (*d3dfunc) (D3D_SDK_VERSION);
#else
  mavlib_D3D= (LPDIRECT3D8) ((*d3dfunc) (D3D_SDK_VERSION));
#endif
  if (!mavlib_D3D)
  {
    fprintf(stderr, "Error: Direct3DCreate8 failed\n");
    exit(1);
  }
}

/* clean up d3d surfaces */
void mavlib_gfxReleaseSurfaces(void)
{
  int i;

/* release texture handles... */
  for (i=0; i<GFX_MAX_TEXTURE; i++)
    if (mavlib_textures[i])
    {
      mavlib_textures[i]->lpVtbl->Release(mavlib_textures[i]);
      mavlib_textures[i]= NULL;
    }

  for (i=0; i<MAV_MAX_WIN; i++)
  {
    if (mavlib_winhand[i].win)
    {
/* ...back buffers... */
      if (mavlib_winhand[i].backbuffer)
        mavlib_winhand[i].backbuffer->lpVtbl->Release(
		mavlib_winhand[i].backbuffer);

/* ...Z buffers... */ 
      if (mavlib_winhand[i].zbuffer)
        mavlib_winhand[i].zbuffer->lpVtbl->Release(
		mavlib_winhand[i].zbuffer);

/* ...swap chains... */
      if (mavlib_winhand[i].swapchain)
	mavlib_winhand[i].swapchain->lpVtbl->Release(
		mavlib_winhand[i].swapchain);
 
      mavlib_winhand[i].backbuffer= NULL;
      mavlib_winhand[i].zbuffer= NULL;
      mavlib_winhand[i].swapchain= NULL;
    }
  }

/* ...dummy render surfaces */
  if (mavlib_dummyBB)
  {
    mavlib_dummyBB->lpVtbl->Release(mavlib_dummyBB);
    mavlib_dummyBB= NULL;
  }

  if (mavlib_dummyZB)
  {
    mavlib_dummyZB->lpVtbl->Release(mavlib_dummyZB);
    mavlib_dummyZB= NULL;
  }
}

/* create surfaces for windows to render to offscreen */
void mavlib_gfxCreateDummySurfaces(void)
{
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->CreateRenderTarget(
	mavlib_D3DDevice, 4, 4, mavlib_config_fullFormat, D3DMULTISAMPLE_NONE,
	FALSE, &mavlib_dummyBB),
	"failed to create dummy back buffer");

  MAVD3DERR(mavlib_D3DDevice->lpVtbl->CreateDepthStencilSurface(
	mavlib_D3DDevice, 4, 4, D3DFMT_D16, D3DMULTISAMPLE_NONE,
	&mavlib_dummyZB),
	"failed to create dummy Z buffer");
}

/* create a secondary swap chain */
void mavlib_gfxSwapChain(int id, int w, int h)
{
  D3DPRESENT_PARAMETERS d3dpp;

/* set present parameters */
  ZeroMemory(&d3dpp, sizeof (d3dpp));
  d3dpp.Windowed= TRUE;
  d3dpp.BackBufferWidth= w;
  d3dpp.BackBufferHeight= h;
  d3dpp.hDeviceWindow= mavlib_winhand[id].win;
  d3dpp.SwapEffect= D3DSWAPEFFECT_COPY_VSYNC;
  d3dpp.BackBufferFormat= mavlib_config_winFormat;

/* create new swap chain */
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->CreateAdditionalSwapChain(
	mavlib_D3DDevice, &d3dpp, &mavlib_winhand[id].swapchain),
	"failed to create swap chain");

/* new back buffer */
  MAVD3DERR(mavlib_winhand[id].swapchain->lpVtbl->GetBackBuffer(
	mavlib_winhand[id].swapchain, 0, D3DBACKBUFFER_TYPE_MONO,
	&mavlib_winhand[id].backbuffer),
	"failed to get back buffer handle");

/* new Z buffer */
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->CreateDepthStencilSurface(
	mavlib_D3DDevice, w, h, D3DFMT_D16, D3DMULTISAMPLE_NONE,
	&mavlib_winhand[id].zbuffer),
	"failed to create Z buffer");
}

/* recreate the main swap window (due to fullscreen/window mode change) */
void mavlib_gfxRecreateSwap(int id)
{
  D3DPRESENT_PARAMETERS d3dpp;
  int i;

/* clean up d3d surfaces */
  mavlib_gfxReleaseSurfaces();

/* free d3d device */
  mavlib_D3DDevice->lpVtbl->Release(mavlib_D3DDevice);

/* get rid of previous window */
  ReleaseDC(mavlib_winhand[id].win, mavlib_winhand[id].hdc);
  DestroyWindow(mavlib_winhand[id].win);

/* create new window as id MAV_MAX_WIN (should never be used otherwise) */
  mavlib_win32Create= MAV_MAX_WIN;

/* set flag for D3D events */
  mavlib_creatingWin= 1;

/* create window - for fullscreen mode window can be any size */
  if (mavlib_gfx_fullscreen)
    mavlib_winhand[MAV_MAX_WIN].win= CreateWindowEx(WS_EX_TOPMOST, "Maverik",
			mavlib_winhand[id].mavwin->name,
			WS_POPUP,
			0, 0, 200,200,
			NULL, NULL, mavlib_dpy, NULL);
  else
    mavlib_winhand[MAV_MAX_WIN].win= CreateWindow("Maverik",
			mavlib_winhand[id].mavwin->name,
			WS_OVERLAPPEDWINDOW,
			CW_USEDEFAULT, CW_USEDEFAULT,
			mavlib_winhand[id].win_width,
			mavlib_winhand[id].win_height,
                        NULL, NULL, mavlib_dpy, NULL);
  mavlib_win32Create=-1;

/* reposition and resize window to original placement */
  if (!mavlib_gfx_fullscreen)
  {
    RECT wr;

/* get adjusted window size */
    wr.left= mavlib_winhand[id].win_x;
    wr.right= mavlib_winhand[id].win_width + mavlib_winhand[id].win_x;
    wr.top= mavlib_winhand[id].win_y;
    wr.bottom= mavlib_winhand[id].win_height + mavlib_winhand[id].win_y;

    AdjustWindowRect(&wr, WS_OVERLAPPEDWINDOW, FALSE);

    MoveWindow(mavlib_winhand[MAV_MAX_WIN].win, wr.left, wr.top,
		wr.right-wr.left, wr.bottom-wr.top, FALSE);
  }

  if (!mavlib_winhand[MAV_MAX_WIN].win)
  {
    fprintf(stderr, "Error: couldn't open window\n");
    exit(1);
  }

/* set this win as new mavlib_mainWin */
  mavlib_winhand[id].win= mavlib_winhand[MAV_MAX_WIN].win;
  mavlib_winhand[id].hdc= mavlib_winhand[MAV_MAX_WIN].hdc;
  mavlib_winhand[MAV_MAX_WIN].win= NULL;

/* Display the new window */
  ShowWindow(mavlib_winhand[id].win, SW_SHOW);
  BringWindowToTop(mavlib_winhand[id].win);

/* recreate device on original window for windowed mode */
  if (!mavlib_gfx_fullscreen) id= mav_win_orig->id;
  mavlib_mainWin= id;
  mavlib_currwin= id;

/* reset D3D flag */
  mavlib_creatingWin= 0;

/* set the present parameters */
  ZeroMemory(&d3dpp, sizeof (d3dpp));

  if (mavlib_gfx_fullscreen)
  {
    d3dpp.Windowed= FALSE;
    d3dpp.BackBufferWidth= mavlib_config_w;
    d3dpp.BackBufferHeight= mavlib_config_h;
    d3dpp.SwapEffect= D3DSWAPEFFECT_DISCARD;
    d3dpp.FullScreen_RefreshRateInHz= D3DPRESENT_RATE_DEFAULT;
    d3dpp.FullScreen_PresentationInterval= D3DPRESENT_INTERVAL_DEFAULT;
    d3dpp.BackBufferFormat= mavlib_config_fullFormat;
    mavlib_texRGB= mavlib_config_fullRGB;
    mavlib_texRGBA= mavlib_config_fullRGBA;
  }
  else
  {
    d3dpp.Windowed= TRUE;
    d3dpp.SwapEffect= D3DSWAPEFFECT_COPY_VSYNC;
    d3dpp.BackBufferFormat= mavlib_config_winFormat;
    mavlib_texRGB= mavlib_config_winRGB;
    mavlib_texRGBA= mavlib_config_winRGBA;
  }
  d3dpp.hDeviceWindow= mavlib_winhand[id].win;
  d3dpp.EnableAutoDepthStencil= TRUE;
  d3dpp.AutoDepthStencilFormat= D3DFMT_D16;

  mavlib_buffFormat= d3dpp.BackBufferFormat;

/* create the D3D device */
  MAVD3DERR(mavlib_D3D->lpVtbl->CreateDevice(mavlib_D3D,
	mavlib_config_adapter, mavlib_config_device, mavlib_winhand[id].win,
	mavlib_config_vertex, &d3dpp, &mavlib_D3DDevice),
	"failed to create D3D device");

/* get new buffer handles */
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->GetBackBuffer(mavlib_D3DDevice,
	0, D3DBACKBUFFER_TYPE_MONO, &mavlib_winhand[id].backbuffer),
	"failed to get main back buffer handle");

  MAVD3DERR(mavlib_D3DDevice->lpVtbl->GetDepthStencilSurface(
	mavlib_D3DDevice, &mavlib_winhand[id].zbuffer),
	"failed to get z buffer handle");

/* reset render states */
  mavlib_gfxRenderStateSet();

  if (!mavlib_gfx_fullscreen)
  {
/* recreate secondary swap chains */
  for (i=0; i<MAV_MAX_WIN; i ++)
    if (mavlib_winhand[i].win &&
	!mavlib_winhand[i].backbuffer)
      mavlib_gfxSwapChain(i, mavlib_winhand[i].width,
		mavlib_winhand[i].height);
  }
  else
  {
    mavlib_gfxCreateDummySurfaces();
  }

/* set window to set perspective */
  mav_gfxWindowSet(id);
}


/* resize the main swap window */
void mavlib_gfxResizeSwap(int id)
{
  D3DPRESENT_PARAMETERS d3dpp;
  int i;

/* clean up d3d handles */
  mavlib_gfxReleaseSurfaces();

/* set the present parameters */
  ZeroMemory(&d3dpp, sizeof(d3dpp));

/* will usually be windowed for resizes - fullscreen mode means that */
/* the window is being remapped */
  if (mavlib_gfx_fullscreen)
  {
    d3dpp.Windowed= FALSE;
    d3dpp.BackBufferWidth= mavlib_config_w;
    d3dpp.BackBufferHeight= mavlib_config_h;
    d3dpp.SwapEffect= D3DSWAPEFFECT_DISCARD;
    d3dpp.FullScreen_RefreshRateInHz= D3DPRESENT_RATE_DEFAULT;
    d3dpp.FullScreen_PresentationInterval= D3DPRESENT_INTERVAL_DEFAULT;
  }
  else
  {
    d3dpp.Windowed= TRUE;
    d3dpp.SwapEffect= D3DSWAPEFFECT_COPY_VSYNC;
  }
  d3dpp.hDeviceWindow= mavlib_winhand[id].win;
  d3dpp.BackBufferFormat= mavlib_buffFormat;
  d3dpp.EnableAutoDepthStencil= TRUE;
  d3dpp.AutoDepthStencilFormat= D3DFMT_D16;

/* for fullscreen delete and recreate the device */
  if (mavlib_gfx_fullscreen)
  {
    mavlib_D3DDevice->lpVtbl->Release(mavlib_D3DDevice);
    MAVD3DERR(mavlib_D3D->lpVtbl->CreateDevice(mavlib_D3D,
	mavlib_config_adapter, mavlib_config_device, mavlib_winhand[id].win,
	mavlib_config_vertex, &d3dpp, &mavlib_D3DDevice),
	"failed to create D3D device");
  }
  else
  {
/* for windowed just reset the device */
     MAVD3DERR(mavlib_D3DDevice->lpVtbl->Reset(mavlib_D3DDevice,
	&d3dpp),
	"failed to reset d3d device");
  }

/* get new buffer handles */
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->GetBackBuffer(mavlib_D3DDevice,
	0, D3DBACKBUFFER_TYPE_MONO, &mavlib_winhand[id].backbuffer),
	"failed to get main back buffer handle");

  MAVD3DERR(mavlib_D3DDevice->lpVtbl->GetDepthStencilSurface(
	mavlib_D3DDevice, &mavlib_winhand[id].zbuffer),
	"failed to get z buffer handle");

/* reset render states */
  mavlib_gfxRenderStateSet();

  if (!mavlib_gfx_fullscreen)
  {
/* recreate secondary swap chains */
  for (i=0; i<MAV_MAX_WIN; i ++)
    if (mavlib_winhand[i].win &&
	!mavlib_winhand[i].backbuffer)
      mavlib_gfxSwapChain(i, mavlib_winhand[i].width,
		mavlib_winhand[i].height);
  }
  else
  {
    mavlib_gfxCreateDummySurfaces();
  }
}


/* ctrl-F1 callback for screen mode changing */
void mavlib_gfxFullscreen(MAV_window *w)
{
/* switch fullscreen flags */
  mavlib_gfx_fullscreen= !mavlib_gfx_fullscreen;
  mav_opt_fullscreen= mavlib_gfx_fullscreen;

/* recreate device with new settings (id of win that will become */
/* fullscreen, or currently is fullscreen) */
  if (!mavlib_gfx_fullscreen)
    mavlib_gfxRecreateSwap(mavlib_mainWin);
  else
    mavlib_gfxRecreateSwap(w->id);
}



/******************************************/
/*                                        */
/* gfx module init and clean up functions */
/*                                        */
/******************************************/

/* Routines to initialise the graphics module */

char *mav_gfxModuleID(void)
{
  return "Graphics (D3D and Windows)";
}

/* clean up on exit function */
void mavlib_gfxExit(void)
{
  int i;
  int font_id;
  HFONT font;

/* free D3D interfaces */

/* release textures */
  for (i=0; i<GFX_MAX_TEXTURE; i++)
    if (mavlib_textures[i])
      mavlib_textures[i]->lpVtbl->Release(mavlib_textures[i]);

/* release window surfaces */
  for (i=0; i<MAV_MAX_WIN; i++)
  {
    if (mavlib_winhand[i].backbuffer)
      mavlib_winhand[i].backbuffer->lpVtbl->Release(
		mavlib_winhand[i].backbuffer);
    if (mavlib_winhand[i].zbuffer)
      mavlib_winhand[i].zbuffer->lpVtbl->Release(
		mavlib_winhand[i].zbuffer);
    if (mavlib_winhand[i].swapchain)
      mavlib_winhand[i].swapchain->lpVtbl->Release(
		mavlib_winhand[i].swapchain);
  }

/* release dummy surfaces */
  if (mavlib_dummyBB)
    mavlib_dummyBB->lpVtbl->Release(mavlib_dummyBB);

  if (mavlib_dummyZB)
    mavlib_dummyZB->lpVtbl->Release(mavlib_dummyZB);

/* release D3D device */
  if (mavlib_D3DDevice)
    mavlib_D3DDevice->lpVtbl->Release(mavlib_D3DDevice);

/* release D3D interface */
  if (mavlib_D3D)
    mavlib_D3D->lpVtbl->Release(mavlib_D3D);

/* release D3D library */
  if (mavlib_d3dlib)
    FreeLibrary(mavlib_d3dlib);

/* release font handles */
  mav_listPointerReset(mavlib_fontList);
  while (mav_listItemsNext(mavlib_fontList, (void **) &font_id,
	(void **) &font))
  {
    DeleteObject(font);
  }

  /* shut down windows nicely */
  for (i=0; i<MAV_MAX_WIN; i++)
  {
    if (mavlib_winhand[i].win)
    {
      ReleaseDC(mavlib_winhand[i].win, mavlib_winhand[i].hdc);
      DestroyWindow(mavlib_winhand[i].win);
    }
  }
}

/* read string to end of line (used by mav_gfxModuleInit) */
void mavlib_readline(FILE *fp, char *str)
{
  char ch;
  int i= 0;

  ch= getc (fp);

  while (ch != '\n')
  {
    str[i]= ch;
    i ++;
    ch= getc(fp);
  }

  str[i]=(char) NULL;
}


/* gfx init */
int mav_gfxModuleInit()
{
  /* Initialise data structure */
  int i;
  WNDCLASS wc;
  FILE *fp;
  char filename[256];
  int f_val;
  char adapter[1000];
  int device_val;
  int adapter_count;
  D3DADAPTER_IDENTIFIER8 info;
  D3DCAPS8 caps;

/* parse config file */
  adapter[0]= (char) NULL;

  sprintf (filename, "%s/.mavd3d8_config", getenv("HOME"));
  fp= fopen(filename, "r");

  if (fp)
  {
    mavlib_readline(fp, adapter);
    fscanf(fp, "%d %d %d %d %d", &device_val, &mavlib_config_w,
			&mavlib_config_h, &mavlib_config_d, &f_val);
    mavlib_config_device= device_val;
    mavlib_config_f= f_val;
    fscanf(fp, "%d", &mavlib_gfx_textureBits);
    fscanf(fp, "%d %d", &mavlib_config_1a, &mavlib_gfx_fullscreen);
    fclose(fp);
  }

/* reset window handles */
  for (i=0; i<MAV_MAX_WIN; i++)
  {
    mavlib_winhand[i].win= (HWND) NULL;
    mavlib_winhand[i].swapchain= NULL;
    mavlib_winhand[i].backbuffer= NULL;
    mavlib_winhand[i].zbuffer= NULL;
  }

/* reset font handle list */
  mavlib_fontList= mav_listNew();

/* reset string list */
  mavlib_stringList= mav_listNew();

/* reset texture handles */
  for (i=0; i<GFX_MAX_TEXTURE; i++)
  {
    mavlib_textures[i]= NULL;
    mavlib_mavTextures[i]= NULL;
  }

  /* add the new module */
  mav_moduleNew(mav_gfxModuleID);  

  /* Open connection to display */  
  mavlib_dpy = GetModuleHandle(NULL);
  if (!mavlib_dpy)
  {
    fprintf(stderr, "Error: cannot connect to screen\n");
    exit(1);
  }

  /* Register class */
  wc.style= CS_OWNDC;
  wc.lpfnWndProc= (WNDPROC) mavlib_winEventHandler;
  wc.cbClsExtra= 0;
  wc.cbWndExtra= 0;
  wc.hInstance= mavlib_dpy;
  wc.hIcon= 0;
  wc.hCursor= LoadCursor(NULL, IDC_ARROW);
  wc.hbrBackground= NULL;
  wc.lpszMenuName= NULL;
  wc.lpszClassName= "Maverik";
    
  if (!RegisterClass(&wc))
  {
    fprintf(stderr, "Error: failed to register class\n");
    exit(1);
  }

/* need to add function that calls BeginScene to Frame2 */
  mav_frameFn2Add(mavlib_beginScene, NULL);

/* set clean up at exit callback */
  atexit(mavlib_gfxExit);

/* set up D3D */
  mavlib_D3DSetUp();

/* search for config adapter */
  adapter_count= mavlib_D3D->lpVtbl->GetAdapterCount(mavlib_D3D);

  for (i=0; i<adapter_count; i++)
  {
    mavlib_D3D->lpVtbl->GetAdapterIdentifier(mavlib_D3D, i,
		D3DENUM_NO_WHQL_LEVEL, &info);

    if (!strcmp(adapter, info.Driver))
    {
      mavlib_config_adapter= i;
    }
    else
    {
      fprintf(stderr,
		"Warning: couldn't find display adapter - using default\n");
    }
  }

/* grab current display mode */
  MAVD3DERR(mavlib_D3D->lpVtbl->GetAdapterDisplayMode(mavlib_D3D,
	D3DADAPTER_DEFAULT, &mavlib_dm),
	"failed to get display mode");

/* get display formats for windowed and fullscreen rendering */
  if (mavlib_D3D->lpVtbl->CheckDeviceType(mavlib_D3D, mavlib_config_adapter,
	mavlib_config_device, mavlib_dm.Format, mavlib_config_f,
	TRUE) == D3D_OK)
  {
    mavlib_config_winFormat= mavlib_config_f;
  }
  else if (mavlib_D3D->lpVtbl->CheckDeviceType(mavlib_D3D,
	mavlib_config_adapter, mavlib_config_device, mavlib_dm.Format,
	mavlib_config_winFormat, TRUE) != D3D_OK)
  {
    mavlib_config_winFormat= mavlib_dm.Format;
  }

  if (mavlib_D3D->lpVtbl->CheckDeviceType(mavlib_D3D, mavlib_config_adapter,
	mavlib_config_device, mavlib_config_f, mavlib_config_f,
	FALSE) == D3D_OK)
  {
    mavlib_config_fullFormat= mavlib_config_f;
  }
  else if (mavlib_D3D->lpVtbl->CheckDeviceType(mavlib_D3D,
	mavlib_config_adapter, mavlib_config_device, mavlib_config_fullFormat,
	mavlib_config_fullFormat, FALSE) != D3D_OK)
  {
    mavlib_config_fullFormat= mavlib_dm.Format;
  }

/* look for compatible texture formats */
  if (mavlib_gfx_textureBits==16)
  {
    if (mavlib_D3D->lpVtbl->CheckDeviceFormat(mavlib_D3D,
		mavlib_config_adapter, mavlib_config_device,
		mavlib_dm.Format, 0,
		D3DRTYPE_TEXTURE, D3DFMT_R5G6B5) == D3D_OK)
    {
      mavlib_config_winRGB= D3DFMT_R5G6B5;
    }
    else if (mavlib_D3D->lpVtbl->CheckDeviceFormat(mavlib_D3D,
		mavlib_config_adapter, mavlib_config_device,
		mavlib_dm.Format, 0,
		D3DRTYPE_TEXTURE, D3DFMT_X1R5G5B5) == D3D_OK)
    {
      mavlib_config_winRGB= D3DFMT_X1R5G5B5;
    }

    if (mavlib_D3D->lpVtbl->CheckDeviceFormat(mavlib_D3D,
		mavlib_config_adapter, mavlib_config_device,
		mavlib_dm.Format, 0,
		D3DRTYPE_TEXTURE, D3DFMT_A4R4G4B4) == D3D_OK)
    {
      mavlib_config_winRGBA= D3DFMT_A4R4G4B4;
    }
    if (mavlib_D3D->lpVtbl->CheckDeviceFormat(mavlib_D3D,
		mavlib_config_adapter, mavlib_config_device,
		mavlib_dm.Format, 0,
		D3DRTYPE_TEXTURE, D3DFMT_A1R5G5B5) == D3D_OK)
    {
      if (!mavlib_config_winRGBA || mavlib_config_1a)
	mavlib_config_winRGBA= D3DFMT_A1R5G5B5;
    }
  }

/* now check for 32 bit textures */
  if (mavlib_D3D->lpVtbl->CheckDeviceFormat(mavlib_D3D,
		mavlib_config_adapter, mavlib_config_device,
		mavlib_dm.Format, 0,
		D3DRTYPE_TEXTURE, D3DFMT_X8R8G8B8) == D3D_OK)
  {
    if (!mavlib_config_winRGB)
      mavlib_config_winRGB= D3DFMT_X8R8G8B8;
  }

  if (mavlib_D3D->lpVtbl->CheckDeviceFormat(mavlib_D3D,
		mavlib_config_adapter, mavlib_config_device,
		mavlib_dm.Format, 0,
		D3DRTYPE_TEXTURE, D3DFMT_A8R8G8B8) == D3D_OK)
  {
    if (!mavlib_config_winRGBA)
      mavlib_config_winRGBA= D3DFMT_A8R8G8B8;
  }

/* check we got some formats */
  if (!mavlib_config_winRGB)
    mavlib_config_winRGB= mavlib_config_winRGBA;

  if (!mavlib_config_winRGBA)
    mavlib_config_winRGBA= mavlib_config_winRGB;

  if (!mavlib_config_winRGB || !mavlib_config_winRGBA)
  {
    fprintf(stderr,
	"Warning: couldn't find a compatible window texture format\n");
  }


/* need to do the same again for fullscreen */
  if (mavlib_gfx_textureBits==16)
  {
    if (mavlib_D3D->lpVtbl->CheckDeviceFormat(mavlib_D3D,
		mavlib_config_adapter, mavlib_config_device,
		mavlib_config_fullFormat, 0,
		D3DRTYPE_TEXTURE, D3DFMT_R5G6B5) == D3D_OK)
    {
      mavlib_config_fullRGB= D3DFMT_R5G6B5;
    }
    else if (mavlib_D3D->lpVtbl->CheckDeviceFormat(mavlib_D3D,
		mavlib_config_adapter, mavlib_config_device,
		mavlib_config_fullFormat, 0,
		D3DRTYPE_TEXTURE, D3DFMT_X1R5G5B5) == D3D_OK)
    {
      mavlib_config_fullRGB= D3DFMT_X1R5G5B5;
    }

    if (mavlib_D3D->lpVtbl->CheckDeviceFormat(mavlib_D3D,
		mavlib_config_adapter, mavlib_config_device,
		mavlib_config_fullFormat, 0,
		D3DRTYPE_TEXTURE, D3DFMT_A4R4G4B4) == D3D_OK)
    {
      mavlib_config_fullRGBA= D3DFMT_A4R4G4B4;
    }
    if (mavlib_D3D->lpVtbl->CheckDeviceFormat(mavlib_D3D,
		mavlib_config_adapter, mavlib_config_device,
		mavlib_config_fullFormat, 0,
		D3DRTYPE_TEXTURE, D3DFMT_A1R5G5B5) == D3D_OK)
    {
      if (!mavlib_config_fullRGBA || mavlib_config_1a)
	mavlib_config_fullRGBA= D3DFMT_A1R5G5B5;
    }
  }

/* now check for 32 bit textures */
  if (mavlib_D3D->lpVtbl->CheckDeviceFormat(mavlib_D3D,
		mavlib_config_adapter, mavlib_config_device,
		mavlib_config_fullFormat, 0,
		D3DRTYPE_TEXTURE, D3DFMT_X8R8G8B8) == D3D_OK)
  {
    if (!mavlib_config_fullRGB)
      mavlib_config_fullRGB= D3DFMT_X8R8G8B8;
  }

  if (mavlib_D3D->lpVtbl->CheckDeviceFormat(mavlib_D3D,
		mavlib_config_adapter, mavlib_config_device,
		mavlib_config_fullFormat, 0,
		D3DRTYPE_TEXTURE, D3DFMT_A8R8G8B8) == D3D_OK)
  {
    if (!mavlib_config_fullRGBA)
      mavlib_config_fullRGBA= D3DFMT_A8R8G8B8;
  }

/* check we got some formats */
  if (!mavlib_config_fullRGB)
    mavlib_config_fullRGB= mavlib_config_fullRGBA;

  if (!mavlib_config_fullRGBA)
    mavlib_config_fullRGBA= mavlib_config_fullRGB;

  if (!mavlib_config_fullRGB || !mavlib_config_fullRGBA)
  {
    fprintf(stderr,
	"Warning: couldn't find a compatible fullscreen texture format\n");
  }

/* check for hardware vertex processing */
  if (mavlib_config_device == D3DDEVTYPE_HAL)
  {
    MAVD3DERR(mavlib_D3D->lpVtbl->GetDeviceCaps(mavlib_D3D,
	mavlib_config_adapter, mavlib_config_device, &caps),
	"failed to get device caps");
    if (caps.DevCaps & D3DDEVCAPS_HWTRANSFORMANDLIGHT)
      mavlib_config_vertex= D3DCREATE_HARDWARE_VERTEXPROCESSING;
  }

  return 1;
}



/******************************/
/*                            */
/* 2d text handling functions */
/*                            */
/******************************/

/* function to calculate text width */
int mav_gfxWindowStringLength(int win, char *s, int font)
{
  SIZE size;
  int font_id;
  HFONT font_handle;

/* find font */
  mav_listPointerReset(mavlib_fontList);
  while (mav_listItemsNext(mavlib_fontList, (void **) &font_id,
	(void **) &font_handle))
  {
    if (font_id==font)
    {
/* make font current */
      SelectObject(mavlib_winhand[mavlib_currwin].hdc, font_handle);

/* get size of text in current font */
      GetTextExtentPoint32(mavlib_winhand[mavlib_currwin].hdc, s,
		strlen (s), &size);

/* only want x extent */
      return size.cx;
    }
  }

  return 0;
}


/* Routine to define a 2D raster font */
int mav_gfxWindowFontSet(char *s, int i, int *width)
{
  int pos= 1;
  int weight= FW_NORMAL;
  BOOL italic= FALSE;
  int family= FF_SWISS;
  int height= -1;
  char typeface[100];
  int tf= 0;
  int font_id;
  HFONT font;

/* delete any font with the same id */
  mav_listPointerReset(mavlib_fontList);
  while (mav_listItemsNext(mavlib_fontList, (void **) &font_id,
		(void **) &font))
  {
    if (font_id==i)
    {
      mav_listItemsRmv(mavlib_fontList, (void *) font_id, (void *) font);
      DeleteObject(font);
    }
  }

/* make a best guess at the font (the size at least should be ok) */
/* company */
  while (s[pos] != '-') pos ++;
  pos ++;

/* typeface */
  while (s[pos] != '-')
  {
    typeface[tf]= s[pos];
    tf ++;
    pos ++;
  }
  typeface[tf]= (char) NULL;
  pos ++;

/* weight */
  if (s[pos]=='b') weight= FW_BOLD;
  else if (s[pos]=='m') weight= FW_MEDIUM;
  while (s[pos] != '-') pos ++;
  pos ++;

/* slant */
  if (s[pos]=='i') italic= TRUE;
  if (s[pos]=='o' && s[pos+1] != 't')
    family= FF_MODERN;
  if (s[pos]=='r' && s[pos+1]=='-')
    family= FF_ROMAN;
  while (s[pos] != '-') pos ++;
  pos ++;

/* ignored */
  while (s[pos] != '-') pos ++;
  pos ++;

/* ignored */
  while (s[pos] != '-') pos ++;
  pos ++;

/* pixel height */
  if (s[pos] != '-' && s[pos] != '*')
  {
    sscanf (&s[pos], "%d", &height);
  }
  while (s[pos] != '-') pos ++;
  pos ++;

/* point height (use if no pixel height) */
  if (height==-1)
  {
    if (s[pos] != '-' && s[pos] != '*')
    {
      sscanf (&s[pos], "%d", &height);
      height /= 10;
    }
  }

/* use default if no height defined */
  if (height==-1) height= 0;

  font= CreateFont(height, 0, 0, 0, weight, italic, FALSE, FALSE,
	ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
	DEFAULT_PITCH|family, typeface);

/* add to font list */
  mav_listItemsAdd(mavlib_fontList, (void *) i, (void *) font);
 
  return 0;
}

/* Routine to display a string in a 2D raster font */ 
void mav_gfxWindowStringDisplay(char *s, int font)
{
  int font_id;
  HFONT font_handle;
  MAVLIB_gfxString *str;

  str= mav_malloc(sizeof(MAVLIB_gfxString));

/* store text info for display later in WindowBuffersSwap */
  str->win= mavlib_currwin;

/* position */
  str->x= mavlib_2d_x;
  str->y= mavlib_2d_y;

/* text */
  sprintf(str->text, "%s", s);

/* colour */
  str->colour= RGB(mavlib_col_r, mavlib_col_g, mavlib_col_b);

/* font */
  mav_listPointerReset(mavlib_fontList);
  while (mav_listItemsNext(mavlib_fontList, (void **) &font_id,
	(void **) &font_handle))
  {
    if (font_id==font)
    {
      str->font= font_handle;
    }
  }

/* add to string list */
  mav_listItemAdd(mavlib_stringList, (void *) str);
}



/****************************/
/*                          */
/* get/set window functions */
/*                          */
/****************************/


/* Routine to get the resolution of the display */
void mav_gfxWindowResGet(int *xres, int *yres)
{
  *xres= GetSystemMetrics(SM_CXSCREEN);
  *yres= GetSystemMetrics(SM_CYSCREEN);
}

/* Routine to return the position of the mouse in a window and root coords */
int mav_gfxWindowPointerGet(int win, int *x, int *y, int *rx, int *ry, int *buts)
{
  int rv=1;
  POINT point;

  if (win>0 && win<MAV_MAX_WIN && mavlib_winhand[win].win) 
  {
    if (GetCursorPos(&point)==0)
    {
      fprintf (stderr, "Error: failed to get cursor pos\n");
      exit (1);
    }

    /* Store root cords */
    *rx= point.x;
    *ry= point.y;
  
    /* Get in coords of specified window */
    if (ScreenToClient(mavlib_winhand[win].win, &point)==0)
    {
      fprintf (stderr, "Error: failed to convert cursor pos\n");
      exit (1);
    }

    /* Store window cords */
    *x= point.x;
    *y= point.y;

/* get mouse button status */
    if (buts)
    {
      if (GetKeyState (VK_LBUTTON)<0) buts[0]= 0;
      else buts[0]= 1;
      if (GetKeyState (VK_MBUTTON)<0) buts[1]= 0;
      else buts[1]= 1;
      if (GetKeyState (VK_RBUTTON)<0) buts[2]= 0;
      else buts[2]= 1;
     }
  }
  else
  {
    rv=0;
  }

  return rv;
}


/* Routine to set the mouses position */
void mav_gfxWindowPointerSet(int win, int x, int y)
{
  POINT point;
  
  /* Point in coords of specified window */
  point.x= x;
  point.y= y;

  /* Get in coords of root window */
  if (ClientToScreen(mavlib_winhand[win].win, &point)==0)
  {
    fprintf (stderr, "Error: failed to convert cursor pos\n");
    exit (1);
  }

  /* Set cursor pos */
  SetCursorPos(point.x, point.y);
}


/* Routine to return the state of a key */
int mav_gfxWindowKeyGet(int key)
{
  int vk;

  /* Convert to keycode */
  switch (key)
  {
  case 300: vk= VK_F1; break;
  case 301: vk= VK_F2; break;
  case 302: vk= VK_F3; break;
  case 303: vk= VK_F4; break;
  case 304: vk= VK_F5; break;
  case 305: vk= VK_F6; break;
  case 306: vk= VK_F7; break;
  case 307: vk= VK_F8; break;
  case 308: vk= VK_F9; break;
  case 309: vk= VK_F10; break;
  case 310: vk= VK_F11; break;
  case 311: vk= VK_F12; break;
  case 312: vk= VK_UP; break;
  case 313: vk= VK_DOWN; break;
  case 314: vk= VK_LEFT; break;
  case 315: vk= VK_RIGHT; break;
  case 316: vk= VK_PRIOR; break;
  case 317: vk= VK_NEXT; break;
  case 318: vk= VK_SHIFT; break;
  case 319: vk= VK_SHIFT; break;
  case 320: vk= VK_MENU; break;
  case 321: vk= VK_MENU; break;
  case 324: vk= VK_HOME; break;
  case 325: vk= VK_END; break;
  case 326: vk= VK_INSERT; break;
  case 327: vk= VK_CONTROL; break;
  case 328: vk= VK_CONTROL; break;
  case 329: vk= VK_CAPITAL; break;
  default: vk= key; break;
  }

  /* Get key state */
  if (GetKeyState(vk)<0)
  {
    return 0; /* Pressed */
  }
  else
  {
    return 1; /* Released */
  }
}

/* Routine to set the current window */
void mav_gfxWindowSet(int i)
{
  int was_in_scene;

/* need to be out of a scene */
  was_in_scene= mavlib_gfx_inScene;
  if (mavlib_gfx_inScene)
    mavlib_endScene();

/* set current window id */
  mavlib_currwin= i;

/* get matrices */
  mavlib_D3DWorld= mavlib_convertMavMatrix(mavlib_winhand[i].mavwin->viewMat);
  mavlib_D3DProj= mavlib_convertMavMatrix(mavlib_winhand[i].mavwin->projMat);

/* set new matrices */
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTransform(mavlib_D3DDevice,
	D3DTS_WORLD, &mavlib_D3DWorld),
	"failed set world matrix");
  MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetTransform(mavlib_D3DDevice,
	D3DTS_PROJECTION, &mavlib_D3DProj),
	"failed set projection matrix");

/* set render targets */
  if (!mavlib_gfx_fullscreen || mavlib_mainWin==i)
  {
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderTarget(mavlib_D3DDevice,
	mavlib_winhand[i].backbuffer, mavlib_winhand[i].zbuffer),
	"failed to set render target");
  }
  else
  {
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->SetRenderTarget(mavlib_D3DDevice,
	mavlib_dummyBB, mavlib_dummyZB),
	"failed to set dummy render targets");
  }

/* restore scene status */
  if (was_in_scene)
    mavlib_beginScene(NULL);
}

/* Routine to swap the buffers of the current window */
void mav_gfxWindowBuffersSwap(void)
{
  MAVLIB_gfxString *str;

/* call end scene */
  mavlib_endScene();

  if (mavlib_currwin==mavlib_mainWin)
  {
/* present main window buffer */
    mavlib_D3DDevice->lpVtbl->Present(mavlib_D3DDevice,
	NULL, NULL, NULL, NULL);
  }
  else
  {
/* present other window buffer */
    if (mavlib_winhand[mavlib_currwin].swapchain)
      mavlib_winhand[mavlib_currwin].swapchain->lpVtbl->Present(
		mavlib_winhand[mavlib_currwin].swapchain,
			NULL, NULL, NULL, NULL);
  }

/* draw GDI text */
  mav_listPointerReset(mavlib_stringList);
  while (mav_listItemNext(mavlib_stringList, (void **) &str))
  {
    if (str->win==mavlib_currwin)
    {
      SetBkMode(mavlib_winhand[str->win].hdc, TRANSPARENT);
      SelectObject(mavlib_winhand[str->win].hdc, str->font);
      SetTextAlign(mavlib_winhand[str->win].hdc, TA_BOTTOM|TA_LEFT);
      SetTextColor(mavlib_winhand[str->win].hdc, str->colour);
      ExtTextOut(mavlib_winhand[str->win].hdc, str->x, str->y, 0, NULL,
		str->text, strlen (str->text), NULL);

/* delete string */
      mav_listItemRmv(mavlib_stringList, (void *) str);
      mav_free(str);
    }
  }
}



/****************************/
/*                          */
/* event handling functions */
/*                          */
/****************************/

/* get the window ID from the HWND */
int mavlib_winlookup(HWND w)
{
  int i;
  
  for (i=0; i<MAV_MAX_WIN; i++)
  {
    if (mavlib_winhand[i].win==w) return i;
  }

  return -1;
}

/* function to get the mouse position associated with an event */
void mavlib_msgPointerGet(int win, int *x, int *y, int *rx, int *ry)
{
  DWORD mouse_pos;
  POINT point;

/* get the mouse position at the time of the last message */
  mouse_pos= GetMessagePos();
  point.x= LOWORD(mouse_pos);
  point.y= HIWORD(mouse_pos);

  *rx= point.x;
  *ry= point.y;

  /* Get in coords of specified window */
  if (ScreenToClient(mavlib_winhand[win].win, &point)==0)
  {
    fprintf (stderr, "Error: failed to convert cursor pos\n");
    exit (1);
  }

  /* Store window cords */
  *x= point.x;
  *y= point.y;
}


/* This callback executed by DispatchMessage in mav_gfxWindowEventGet */
LONG WINAPI mavlib_winEventHandler(HWND hwnd, UINT umsg, WPARAM wparam, LPARAM lparam) 
{
  LONG rv= 1;
  int v1=-1, v2=-1;
  LPMINMAXINFO mmi;
  char ch;

  /* Set event indicator to zero - not a managed event */
  mavlib_win32EventGood=0;

  switch (umsg)
  {
  case WM_GETMINMAXINFO:
/* stop windows being resized too small */
    mmi= (LPMINMAXINFO) lparam;
    mmi->ptMinTrackSize.y += MAVLIB_MIN_Y;
    return 0;
  case WM_CREATE: /* Create Window event */
    if (mavlib_win32Create!=-1) 
    {
      mavlib_currwin= mavlib_win32Create;
      mavlib_winhand[mavlib_win32Create].hdc= GetDC(hwnd);

#ifdef TME
      mavlib_winhand[mavlib_win32Create].tme.cbSize= sizeof(TRACKMOUSEEVENT);
      mavlib_winhand[mavlib_win32Create].tme.dwFlags= TME_LEAVE;
      mavlib_winhand[mavlib_win32Create].tme.hwndTrack= hwnd;
#endif
    }
    else
    {
      fprintf (stderr, "Error: unexpected create message\n");
      exit (1);
    }
    break;
  case WM_CLOSE: /* Pressed on close icon */
    exit(1);
    break;
  case WM_SYSKEYDOWN: /* Keyboard event - fill in info */
  case WM_KEYDOWN:
    if (v1==-1) v1=0;
  case WM_SYSKEYUP:
  case WM_KEYUP:
    if (v1==-1) v1=1;

    /* Bit 30 of lparam is set if key already held down */
    if (v1==0 && lparam & (1 << 30)) break;

    /* The return value of this event */
    mavlib_win32EventGood=1; 

    /* Get the id of the window in which the event occurred */
    mavlib_win32EventInfo[0]= mavlib_winlookup(hwnd);

    /* Get pointer position at time of message */
    mavlib_msgPointerGet(mavlib_win32EventInfo[0], &mavlib_win32EventInfo[1], &mavlib_win32EventInfo[2], &mavlib_win32EventInfo[3], &mavlib_win32EventInfo[4]);
 

    /* Pressed or released */
    mavlib_win32EventInfo[5]=v1;

    /* Get modifier status */
    if (GetKeyState(VK_SHIFT)<0) /* Shift key up/down */ 
    {
      mavlib_win32EventInfo[7]= 1; /* Pressed (which is represented by 0 elsewhere!) */
    }
    else
    {
      mavlib_win32EventInfo[7]= 0; /* Released */
    }

    if (GetKeyState(VK_CONTROL)<0) /* Ctrl key up/down */ 
    {
      mavlib_win32EventInfo[8]= 1;
    }
    else
    {
      mavlib_win32EventInfo[8]= 0;
    }
    
    if (GetKeyState(VK_MENU)<0) /* Alt key up/down */ 
    {
      mavlib_win32EventInfo[9]= 1;
    }
    else
    {
      mavlib_win32EventInfo[9]= 0;
    }

    /* Translate keycode into ASCII value or #defines */
    mavlib_win32EventInfo[6]=0;
    switch (wparam)
    {
    case VK_DELETE: mavlib_win32EventInfo[6]= 127; break;
    case VK_F1: mavlib_win32EventInfo[6]= 300; break;
    case VK_F2: mavlib_win32EventInfo[6]= 301; break;
    case VK_F3: mavlib_win32EventInfo[6]= 302; break;
    case VK_F4: mavlib_win32EventInfo[6]= 303; break;
    case VK_F5: mavlib_win32EventInfo[6]= 304; break;
    case VK_F6: mavlib_win32EventInfo[6]= 305; break;
    case VK_F7: mavlib_win32EventInfo[6]= 306; break;
    case VK_F8: mavlib_win32EventInfo[6]= 307; break;
    case VK_F9: mavlib_win32EventInfo[6]= 308; break;
    case VK_F10: mavlib_win32EventInfo[6]= 309; break;
    case VK_F11: mavlib_win32EventInfo[6]= 310; break;
    case VK_F12: mavlib_win32EventInfo[6]= 311; break;
    case VK_UP: mavlib_win32EventInfo[6]= 312; break;
    case VK_DOWN: mavlib_win32EventInfo[6]= 313; break;
    case VK_LEFT: mavlib_win32EventInfo[6]= 314; break;
    case VK_RIGHT: mavlib_win32EventInfo[6]= 315; break;
    case VK_PRIOR: mavlib_win32EventInfo[6]= 316; break;
    case VK_NEXT: mavlib_win32EventInfo[6]= 317; break;
    case VK_SHIFT: ch= (char) (lparam >> 16);
		   if (ch==MapVirtualKey (VK_SHIFT, 0))
		     mavlib_win32EventInfo[6]= 318; /* left shift */
		   else
		     mavlib_win32EventInfo[6]= 319; /* right shift */
		   break;
    case VK_MENU: if (lparam & (1<<24))
		    mavlib_win32EventInfo[6]= 321; /* right alt */
		  else
		    mavlib_win32EventInfo[6]= 320; /* left alt */
		  break;
    case VK_HOME: mavlib_win32EventInfo[6]= 324; break;
    case VK_END: mavlib_win32EventInfo[6]= 325; break;
    case VK_INSERT: mavlib_win32EventInfo[6]= 326; break;
    case VK_CONTROL: if (lparam & (1<<24))
		       mavlib_win32EventInfo[6]= 328; /* right ctrl */
		     else
		       mavlib_win32EventInfo[6]= 327; /* left ctrl */
		     break;
    case VK_CAPITAL: mavlib_win32EventInfo[6]= 329; break;
    default: mavlib_win32EventInfo[6]= MapVirtualKey(wparam, 2); break;
    }

    /* Windows reports everything as uppercase - compensate for this */
    if (mavlib_win32EventInfo[6]>='A' && mavlib_win32EventInfo[6]<='Z' && !mavlib_win32EventInfo[7]) mavlib_win32EventInfo[6]+=32;

    /* check for shift key and change punctuation characters */
    if (mavlib_win32EventInfo[7]) {
      switch (mavlib_win32EventInfo[6]) {
	case '`': mavlib_win32EventInfo[6]= '~'; break;
        case '1': mavlib_win32EventInfo[6]= '!'; break;
        case '2': mavlib_win32EventInfo[6]= '\"'; break;
        case '3': mavlib_win32EventInfo[6]= '#'; break;
        case '4': mavlib_win32EventInfo[6]= '$'; break;
        case '5': mavlib_win32EventInfo[6]= '%'; break;
        case '6': mavlib_win32EventInfo[6]= '^'; break;
        case '7': mavlib_win32EventInfo[6]= '&'; break;
        case '8': mavlib_win32EventInfo[6]= '*'; break;
        case '9': mavlib_win32EventInfo[6]= '('; break;
        case '0': mavlib_win32EventInfo[6]= ')'; break;
        case '-': mavlib_win32EventInfo[6]= '_'; break;
        case '=': mavlib_win32EventInfo[6]= '+'; break;
        case '\\': mavlib_win32EventInfo[6]= '|'; break;
        case '[': mavlib_win32EventInfo[6]= '{'; break;
        case ']': mavlib_win32EventInfo[6]= '}'; break;
        case ';': mavlib_win32EventInfo[6]= ':'; break;
        case '\'': mavlib_win32EventInfo[6]= '@'; break;
        case '#': mavlib_win32EventInfo[6]= '~'; break;
        case ',': mavlib_win32EventInfo[6]= '<'; break;
        case '.': mavlib_win32EventInfo[6]= '>'; break;
        case '/': mavlib_win32EventInfo[6]= '?'; break;
	default: /* do nothing */;
      }
    }

    /* No event if we cant translate keycode */
    if (mavlib_win32EventInfo[6]==0) mavlib_win32EventGood=0;

    /* End of event */
    mavlib_win32EventInfo[10]= -999;
    break;

  case WM_LBUTTONDOWN: /* Mouse button event - fill in info */
    if (v1==-1) v1=1;
    if (v2==-1) v2=0;
  case WM_MBUTTONDOWN:
    if (v1==-1) v1=2;
    if (v2==-1) v2=0;
  case WM_RBUTTONDOWN:
    if (v1==-1) v1=3;
    if (v2==-1) v2=0;
  case WM_LBUTTONUP:
    if (v1==-1) v1=1;
    if (v2==-1) v2=1;
  case WM_MBUTTONUP:
    if (v1==-1) v1=2;
    if (v2==-1) v2=1;
  case WM_RBUTTONUP:
    if (v1==-1) v1=3;
    if (v2==-1) v2=1;

/* catch the mouse to trap button up events */
    if (v2==0)
    {
/* button pressed */
/* catch the mouse */
      if (!mavlib_win32MouseCaught) SetCapture(hwnd); 
      mavlib_win32MouseWin[v1]= hwnd; /* store the window for this event */
      mavlib_win32MouseCaught ++; /* keep count of number of captures */
    }
    else
    {
/* release the mouse */
      mavlib_win32MouseCaught --;
      mavlib_win32MouseWin[v1]= NULL;
      if (!mavlib_win32MouseCaught)
      {
        ReleaseCapture();
      }
    }

  case 522: /* For some reason WM_MOUSEWHEEL is not defined for me */
    if (v1==-1) {
      short del= HIWORD(wparam);
      if (del>0)
      {
	v1=4; /* wheel up */
      }
      else
      {
	v1=5; /* wheel down */
      }
    }
    if (v2==-1) v2=0; /* consider it as a button press event although a release is never generated */

    /* The return value of this event */
    mavlib_win32EventGood=2;

    /* Get the id of the window in which the event occurred */
    mavlib_win32EventInfo[0]= mavlib_winlookup(hwnd);

    /* Get pointer position */
    mavlib_msgPointerGet(mavlib_win32EventInfo[0], &mavlib_win32EventInfo[1], &mavlib_win32EventInfo[2], &mavlib_win32EventInfo[3], &mavlib_win32EventInfo[4]);


    /* Pressed or released */
    mavlib_win32EventInfo[5]=v2;

    /* Which button */
    mavlib_win32EventInfo[6]=v1;

    /* Get modifier status */
    if (GetKeyState(VK_SHIFT)<0) /* Shift key up/down */ 
    {
      mavlib_win32EventInfo[7]= 1; /* Pressed (which is represented by 0 elsewhere!) */
    }
    else
    {
      mavlib_win32EventInfo[7]= 0; /* Released */
    }

    if (GetKeyState(VK_CONTROL)<0) /* Ctrl key up/down */ 
    {
      mavlib_win32EventInfo[8]= 1;
    }
    else
    {
      mavlib_win32EventInfo[8]= 0;
    }
    
    if (GetKeyState(VK_MENU)<0) /* Alt key up/down */ 
    {
      mavlib_win32EventInfo[9]= 1;
    }
    else
    {
      mavlib_win32EventInfo[9]= 0;
    }
    
    /* End of event */
    mavlib_win32EventInfo[10]=-999;
    break;
  case WM_MOUSEMOVE:
    if (hwnd != mavlib_hwnd)
    {
      v1= mavlib_winlookup (hwnd);
      mavlib_winhand[v1].enter= 1;
      mavlib_hwnd= hwnd;
    }
/* TrackMouseEvent anyway to catch crossing mess ups */
#ifdef TME
    TrackMouseEvent(&mavlib_winhand[v1].tme);
    break;
  case WM_MOUSELEAVE:
    mavlib_winhand[mavlib_winlookup(hwnd)].leave= 1;
/* reset mavlib_hwnd unless already changed */
/* (enter events of the next window may be processed first) */
    if (mavlib_hwnd==hwnd) mavlib_hwnd= NULL;
#endif
    break;
  case WM_MOVE: /* move event - remember for window mode switching */
    v1= mavlib_winlookup(hwnd);
    if (!mavlib_gfx_fullscreen)
    {
      mavlib_winhand[v1].win_x= LOWORD (lparam);
      mavlib_winhand[v1].win_y= HIWORD (lparam);
    }
    break;
  case WM_SIZE:  /* Resize event - store in winhand for now */
    v1= mavlib_winlookup(hwnd);
    if (wparam==SIZE_MINIMIZED)
    {
      mavlib_winhand[v1].mapped= 2; /* unmap (+1) */
    }
    else
    {
      mavlib_winhand[v1].width= LOWORD(lparam);
      mavlib_winhand[v1].height= HIWORD(lparam);
      mavlib_winhand[v1].resized= 1;
      if (mavlib_gfx_fullscreen && v1==mavlib_mainWin)
      {
/* override message values */
	mavlib_winhand[v1].width= mavlib_config_w;
	mavlib_winhand[v1].height= mavlib_config_h;
      }
      else
      {
/* remember window size */
	mavlib_winhand[v1].win_width= mavlib_winhand[v1].width;
	mavlib_winhand[v1].win_height= mavlib_winhand[v1].height;
      }

      if (mavlib_D3DDevice && !mavlib_creatingWin)
      {

/* ok now... resizing a window involves releasing and recreating */
/* a D3D swap chain (otherwise the resolution of the back buffer */
/* and z buffer are not changed). If it is a secondary swap chain */
/* we can just go ahead and change it and keep the current device */
/* render states (lights, textures etc.) : */

        if (v1 != mavlib_mainWin)
        {
/* free previous buffers and swap chain */
	  if (mavlib_winhand[v1].backbuffer)
	    mavlib_winhand[v1].backbuffer->lpVtbl->Release(
			mavlib_winhand[v1].backbuffer);
	  if (mavlib_winhand[v1].zbuffer)
	    mavlib_winhand[v1].zbuffer->lpVtbl->Release(
			mavlib_winhand[v1].zbuffer);
	  if (mavlib_winhand[v1].swapchain)
	    mavlib_winhand[v1].swapchain->lpVtbl->Release(
			mavlib_winhand[v1].swapchain);

/* create a new swap chain for this window */
	  mavlib_gfxSwapChain(v1, mavlib_winhand[v1].width,
		mavlib_winhand[v1].height);
	}
        else
        {
/* resizing the main window. This involves resetting the device, */
/* which means releasing all D3D handles created beneath the device. */

/* resize device */
          mavlib_gfxResizeSwap(v1);
	}
      }
    }
    break;
  case WM_QUERYOPEN: /* sent before opening an iconic window */
    v1= mavlib_winlookup(hwnd);
    mavlib_winhand[v1].mapped= 1; /* map (+1) */
    break;
  case WM_PAINT: /* expose event */
/* WM not happy unless you validate the update region */
/* NULL validates the whole window */
    ValidateRect(hwnd, NULL);
    v1= mavlib_winlookup(hwnd);
/* store for checking in mav_gfxWindowEventGet */
    mavlib_winhand[v1].exposed= 1;
    break;
  default: /* Let the system deal with all other events */
    rv= DefWindowProc(hwnd, umsg, wparam, lparam);
  }

  return rv;
}


/* 
   Check if any events are outstanding (do not block if there are not) 
   Return value gives the type of event.
*/
int mav_gfxWindowEventPeek(void)
{
  int rv=0;
  int winid=0;

  return (rv + (winid<<8));
}

/* main event check/get function */
int mav_gfxWindowEventGet(int *info)
{
  int rv=0;
  MSG msg;
  int i=0;

  /* Look for mapping, resize or expose events */
  for (i=0; i<MAV_MAX_WIN; i++)
  {
    if (mavlib_winhand[i].win && mavlib_winhand[i].mapped)
    {
      info[0]= i;
      info[1]= mavlib_winhand[i].mapped - 1;
      mavlib_winhand[i].mapped= 0;
      return (4 + (info[0]<<8)); /* map/unmap event */
    }
    if (mavlib_winhand[i].win && mavlib_winhand[i].resized)
    {
      info[0]= i;
      info[1]= mavlib_winhand[i].width;
      info[2]= mavlib_winhand[i].height;
      mavlib_winhand[i].resized=0;
/* remember to adjust aspect for fullscreen */
      if (mavlib_gfx_fullscreen) mavlib_winhand[i].set_aspect= 1;
      return (3 + (info[0]<<8)); /* Indicates a resize event */
    }
    if (mavlib_winhand[i].set_aspect)
    {
/* need to adjust fullscreen aspect */
      mavlib_winhand[i].set_aspect= 0;
      if (mavlib_gfx_fullscreen)
      {
	if (mavlib_winhand[i].mavwin->orthogonal)
	  mav_windowOrthogonalSet(mavlib_winhand[i].mavwin,
		mavlib_winhand[i].mavwin->ncp,
		mavlib_winhand[i].mavwin->fcp,
		mavlib_winhand[i].mavwin->ortho_size,
		640.0/480.0);
	else
	  mav_windowPerspectiveSet(mavlib_winhand[i].mavwin,
		mavlib_winhand[i].mavwin->ncp,
		mavlib_winhand[i].mavwin->fcp,
		mavlib_winhand[i].mavwin->fov,
		640.0/480.0);
      }
    }
    if (mavlib_winhand[i].win && mavlib_winhand[i].exposed)
    {
      info[0]= i;
      mavlib_winhand[i].exposed= 0;
      return (6 + (info[0]<<8)); /* Indicates an expose event */
    }
    if (mavlib_winhand[i].win && mavlib_winhand[i].enter)
    {
      info[0]= i;
      mavlib_winhand[i].enter= 0;
      info[1]= 0;
      return (5 + (info[0]<<8)); /* Indicates an enter event */
    }
    if (mavlib_winhand[i].win && mavlib_winhand[i].leave)
    {
      info[0]= i;
      mavlib_winhand[i].leave= 0;
      info[1]= 1;
      return (5 + (info[0]<<8)); /* Indicates a leave event */
    }
  }

/* check for released mouse capture */
/* this section isn't really necessary unless there are other processes */
/* running which capture the mouse (or you press the windows key) */
  if (mavlib_win32MouseEventsPending || mavlib_win32MouseCaught)
  {
    if (!GetCapture())
    {
/* mouse no longer caught - simulate button up event for all pressed buttons */
      int i;
      int not_got_one= 1;

      mavlib_win32MouseCaught= 0;

      for (i=1; not_got_one && i<4; i++)
      {
	if (mavlib_win32MouseWin[i])
        {
	  not_got_one= 0;
	  info[0]= mavlib_winlookup(mavlib_win32MouseWin[i]);
	  mav_gfxWindowPointerGet(info[0], &info[1], &info[2], &info[3],
			&info[4], NULL);
	  info[5]= 1; /* released */
	  info[6]= i; /* button */
/* get modifier status */
	  if (GetKeyState(VK_SHIFT)<0)
	    info[7]= 1;
	  else
	    info[7]= 0;

	  if (GetKeyState(VK_CONTROL)<0)
	    info[8]= 1;
	  else
	    info[8]= 0;

	  if (GetKeyState(VK_MENU)<0)
	    info[9]= 1;
	  else
	    info[9]= 0;

	  info[10]= -999;
/* reset mouse win */
	  mavlib_win32MouseWin[i]= NULL;
	}
      }

      if (not_got_one)
      {
	mavlib_win32MouseEventsPending= 0;
      }
      else
      {
	mavlib_win32MouseEventsPending= 1;
	return (2 + (info[0]<<8)); /* button event */
      }
    }
  }
/* end of mouse-capture-maybe-not-necessary section */


  /* Get next event - non blocking */
  if (PeekMessage(&msg, 0, 0, 0, PM_REMOVE))
  {	

    /* Deal with event - this calls the mavlib_winEventHandler callback fn */
/* no point in calling TranslateMessage as Maverik interprets virtual keys */
/* and it just sends an extra (ignored) message to the queue */
    DispatchMessage(&msg);
      
    /* Is this an event dealt with by Maverik - value set by callback fn */
    if (mavlib_win32EventGood)
    {
      /* Copy values filled in by callback fn into info */
      i=0;
      while (mavlib_win32EventInfo[i]!=-999)
      {
	info[i]=mavlib_win32EventInfo[i];
	i++;
	rv=mavlib_win32EventGood;
      }
    }
  }

  return (rv + (info[0]<<8));
}



/*********************/
/*                   */
/* open/close window */
/*                   */
/*********************/


/* Routine to open a window */
void mav_gfxWindowOpen(int id, int x, int y, int width, int height, char *nm, char *disp, int wmplacement, int sb, int qb, int ms, int ab, int stenb, int desta, int *wret, int *hret)
{
  RECT wr;
  D3DPRESENT_PARAMETERS d3dpp;
  MAV_window *mavwin;

/* get the MAV_window for this id - Maverik is coy about giving it */
/* directly to gfx but we can search the window list to find it */
  mav_listPointerReset(mav_win_list);
  while (mav_listItemNext(mav_win_list, (void **) &mavwin))
  {
    if (mavwin->id==id) mavlib_winhand[id].mavwin= mavwin;
  }

/* store window info (for fullscreen/window mode switching) */
  mavlib_winhand[id].win_x= x;
  mavlib_winhand[id].win_y= y;
  mavlib_winhand[id].win_width= width;
  mavlib_winhand[id].win_height= height;

/* fullscreen or window mode parameters */
/* only create the main window as fullscreen */
  if ((mavlib_gfx_fullscreen || mav_opt_fullscreen) && mavlib_mainWin==-1)
  {
    mavlib_gfx_fullscreen= 1;
    x= 0;
    y= 0;
    width= mavlib_config_w;
    height= mavlib_config_h;
/* suppress text output */
    mav_opt_output= 0;
  }
  else
  {
/* get adjusted window size */
    wr.left= x;
    wr.right= width + x;
    wr.top= y;
    wr.bottom= height + y;

    AdjustWindowRect(&wr, WS_OVERLAPPEDWINDOW, FALSE);
  }

  if (qb)
  {
    fprintf(stderr, "Error: Quad buffer visuals not supported on this platform\n");
    exit (1);
  }

  if (ms)
  {
    fprintf(stderr, "Error: Multisampled visuals not supported on this platform\n");
    exit (1);
  }

/* stencil buffers are supported - add later */
  if (stenb)
  {
    fprintf(stderr, "Error: Stencil buffer visuals not supported on this platform\n");
    exit (1);
  }

  if (desta)
  {
    fprintf(stderr, "Error: Destination alpha buffer visuals not supported on this platform\n");
    exit (1);
  }

/* assume we'll get the correct window size */
/* probably ought to check it (after the initial resize event) */
  *wret= width;
  *hret= height;

/* set values for WM_CREATE event */
  mavlib_win32Create=id;

/* set flag for D3D events */
  mavlib_creatingWin= 1;

/* create fullscreen only if this is the first window */
  if (mavlib_gfx_fullscreen && mavlib_mainWin==-1)
  {
    mavlib_winhand[id].win= CreateWindowEx(WS_EX_TOPMOST, "Maverik", nm,
			WS_POPUP,
			0, 0, width, height,
			NULL, NULL, mavlib_dpy, NULL);
  }
  else
  {
    mavlib_winhand[id].win= CreateWindow("Maverik", nm,
			 WS_OVERLAPPEDWINDOW,
			 CW_USEDEFAULT, CW_USEDEFAULT, width, height,
                         NULL, NULL, mavlib_dpy, NULL);
/* reposition and resize window to original placement */
    MoveWindow(mavlib_winhand[id].win, wr.left, wr.top, wr.right-wr.left,
		wr.bottom-wr.top, FALSE);
  }

  mavlib_win32Create= -1;

/* set event values */
  mavlib_winhand[id].width=-1;
  mavlib_winhand[id].height=-1;
  mavlib_winhand[id].resized=0;
  mavlib_winhand[id].set_aspect= 0;
  mavlib_winhand[id].exposed= 0;
  mavlib_winhand[id].mapped= 0;
  mavlib_winhand[id].enter= 0;
  mavlib_winhand[id].leave= 0;

  if (!mavlib_winhand[id].win)
  {
    fprintf(stderr, "Error: couldn't open window\n");
    exit(1);
  }

  ShowWindow(mavlib_winhand[id].win, SW_SHOW);

/* reset D3D flag */
  mavlib_creatingWin= 0;

  if (mavlib_mainWin == -1)
  {
    mavlib_mainWin= id;

/* set present parameters */
    ZeroMemory(&d3dpp, sizeof (d3dpp));

    if (mavlib_gfx_fullscreen)
    {
      d3dpp.Windowed= FALSE;
      d3dpp.BackBufferWidth= width;
      d3dpp.BackBufferHeight= height;
      d3dpp.SwapEffect= D3DSWAPEFFECT_DISCARD;
      d3dpp.FullScreen_RefreshRateInHz= D3DPRESENT_RATE_DEFAULT;
      d3dpp.FullScreen_PresentationInterval= D3DPRESENT_INTERVAL_ONE;
      d3dpp.BackBufferFormat= mavlib_config_fullFormat;
      mavlib_texRGB= mavlib_config_fullRGB;
      mavlib_texRGBA= mavlib_config_fullRGBA;
    }
    else
    {
      d3dpp.Windowed= TRUE;
      d3dpp.SwapEffect= D3DSWAPEFFECT_COPY_VSYNC;
      d3dpp.BackBufferFormat= mavlib_config_winFormat;
      mavlib_texRGB= mavlib_config_winRGB;
      mavlib_texRGBA= mavlib_config_winRGBA;
    }

    d3dpp.hDeviceWindow= mavlib_winhand[id].win;
    d3dpp.EnableAutoDepthStencil= TRUE;
    d3dpp.AutoDepthStencilFormat= D3DFMT_D16;

    mavlib_buffFormat= d3dpp.BackBufferFormat;

/* create the D3D device with the main back buffer and z buffer */
    MAVD3DERR(mavlib_D3D->lpVtbl->CreateDevice(mavlib_D3D,
	mavlib_config_adapter, mavlib_config_device, mavlib_winhand[id].win,
	mavlib_config_vertex, &d3dpp, &mavlib_D3DDevice),
	"failed to create D3D device");

/* get handles to the back buffer and z buffer */
    MAVD3DERR(mavlib_D3DDevice->lpVtbl->GetBackBuffer(mavlib_D3DDevice,
	0, D3DBACKBUFFER_TYPE_MONO, &mavlib_winhand[id].backbuffer),
	"failed to get main back buffer handle");

    MAVD3DERR(mavlib_D3DDevice->lpVtbl->GetDepthStencilSurface(
	mavlib_D3DDevice, &mavlib_winhand[id].zbuffer),
	"failed to get z buffer handle");

/* set up default render states */
    mavlib_gfxRenderStateSet();

/* create dummy render surfaces */
    if (mavlib_gfx_fullscreen)
      mavlib_gfxCreateDummySurfaces();
  }
  else
  {
/* create extra back buffer chain for the d3d device */
/* unless another window is already in fullscreen mode */
    if (!mavlib_gfx_fullscreen)
      mavlib_gfxSwapChain(id, width, height);
  }

  SetFocus(mavlib_winhand[id].win);
  BringWindowToTop(mavlib_winhand[id].win);

/* at this point a resize event will have occurred but (hopefully) */
/* to the size that was originally chosen */

  /* Set to active window */
  mav_gfxWindowSet(id);
}



/* Routine to close a window */
void mav_gfxWindowClose(int id)
{
/* this function should never be called for the original (id==1) window */
/* but might be called for the current fullscreen window - need to change */
/* back to windowed mode before deleting it */

  if (id==mavlib_mainWin)
  {
    mavlib_gfx_fullscreen= 0;
    mav_opt_fullscreen= 0;
    mavlib_gfxRecreateSwap(id);
  }

/* release D3D surfaces */
  if (mavlib_winhand[id].backbuffer)
    mavlib_winhand[id].backbuffer->lpVtbl->Release(
		mavlib_winhand[id].backbuffer);
  if (mavlib_winhand[id].zbuffer)
    mavlib_winhand[id].zbuffer->lpVtbl->Release(
		mavlib_winhand[id].zbuffer);
  if (mavlib_winhand[id].swapchain)
    mavlib_winhand[id].swapchain->lpVtbl->Release(
		mavlib_winhand[id].swapchain);

  mavlib_winhand[id].backbuffer= NULL;
  mavlib_winhand[id].zbuffer= NULL;
  mavlib_winhand[id].swapchain= NULL;

/* remove the window */
  if (mavlib_winhand[id].win)
  {
    ReleaseDC(mavlib_winhand[id].win, mavlib_winhand[id].hdc);
    DestroyWindow(mavlib_winhand[id].win);
    mavlib_winhand[id].win= (HWND) NULL;
  }
}

/******************/
/*                */
/* dummy routines */
/*                */
/******************/


/* Routines specific to Voodoo */

void mav_gfx3DfxModeSet(int fullscreen)
{
}

int mav_gfx3DfxBoardSet(int bd)
{
  int rv= 0;

  return rv;
}
