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


/* These are the only bits of Maverik we need. Keep interface to window
   manager as light as possible to make its easy for other implementation */

#ifdef __cplusplus
extern "C" {
#endif
void mav_gfxWindowOpen(int id, int x, int y, int w, int h, char *name, char *disp, int wmp, int sb, int qb, int ms, int ab, int stenb, int desta, int *wret, int *hret);
void mav_gfxWindowClose(int id);
void mav_gfxWindowSet(int id);
void mav_gfxWindowBuffersSwap(void);
void mav_gfxWindowResGet(int *x, int *y);
int  mav_gfxWindowEventGet(int *info);
int  mav_gfxWindowEventPeek(void);
int  mav_gfxWindowPointerGet(int id, int *x, int *y, int *rx, int *ry, int *buts);
void mav_gfxWindowPointerSet(int win, int x, int y);
int  mav_gfxWindowKeyGet(int key);
int  mav_gfxWindowFontSet(char *s, int font, int *width);
void mav_gfxWindowStringDisplay(char *s, int font);
void mav_moduleNew(char *fn(void));
int  mav_gfxModuleInit(void);
char *mav_gfxModuleID(void);
void mav_gfx3DfxModeSet(int fullscreen);
int  mav_gfx3DfxBoardSet(int bd);
#ifdef __cplusplus
}
#endif



/* Routines to initialise the graphics module */

char *mav_gfxModuleID(void)
{
  return "Graphics (None)";
}

int mav_gfxModuleInit()
{
  /* add the new module */
  mav_moduleNew(mav_gfxModuleID);  

  return 1;
}



/* Routine to get the resolution of the display */

void mav_gfxWindowResGet(int *xres, int *yres)
{
  *xres= 1280;
  *yres= 1024;
}



/* Routine to set the current window */

void mav_gfxWindowSet(int i)
{
}



/* Routine to swap the buffers of the current window */

void mav_gfxWindowBuffersSwap(void)
{
}



/* Routine to read a 2D raster font */

int mav_gfxWindowFontSet(char *s, int i, int *width)
{
  return 0;
}



/* Routine to display a string in a 2D raster font */ 

void mav_gfxWindowStringDisplay(char *s, int font)
{
}



/* Routine to open a window */

void mav_gfxWindowOpen(int id, int x, int y, int width, int height, char *nm, char *disp, int wmplacement, int sb, int qb, int ms, int ab, int stenb, int desta, int *wret, int *hret)
{
  *wret= width;
  *hret= height;
}



/* Routine to close a window */

void mav_gfxWindowClose(int id)
{
}



/* 
   Check if any events are outstanding (do not block if there are not) 
   Return value gives the type of event.
*/

int mav_gfxWindowEventPeek(void)
{
  return 0;
}



/* 
   Get next event returning data in info array (again, do not block if there are no event) 
   Return value gives the type of event.
*/

int mav_gfxWindowEventGet(int *info)
{
  return 0;
}



/* Routine to return the position of the mouse in a window and root coords */

int mav_gfxWindowPointerGet(int win, int *x, int *y, int *rx, int *ry, int *buts)
{
  *x=10;
  *y=20;
  *rx=30;
  *ry=40;

  return 1;
}



/* Routine to set the mouses position */

void mav_gfxWindowPointerSet(int win, int x, int y)
{
}



/* Routine to return the state of a key */

int mav_gfxWindowKeyGet(int key)
{
  return 1;
}



/* Routines specific to Voodoo */

void mav_gfx3DfxModeSet(int fullscreen)
{
}

int mav_gfx3DfxBoardSet(int bd)
{
  return 0;
}
