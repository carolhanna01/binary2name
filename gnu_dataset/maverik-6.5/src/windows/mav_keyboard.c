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


#include "mavlib_windows.h"
#include <stdlib.h>
#include <stdio.h>

MAV_callback *mav_callback_keyboard;
MAV_callback *mav_callback_sysKeyboard;



/* Wrapper routines to set and execute the keyboard callbacks */

void mav_callbackKeyboardSet(MAV_window *w, MAV_class *c, MAV_callbackKeyboardFn fn)
{
  mav_callbackSet(mav_callback_keyboard, w, c, (MAV_callbackFn) fn);
}

int mav_callbackKeyboardExec(MAV_window *w, MAV_object *o, MAV_keyboardEvent *ke)
{
  return (mav_callbackExec(mav_callback_keyboard, w, o, (void *) ke, NULL));
}

void mav_callbackSysKeyboardSet(MAV_window *w, MAV_class *c, MAV_callbackKeyboardFn fn)
{
  mav_callbackSet(mav_callback_sysKeyboard, w, c, (MAV_callbackFn) fn);
}

int mav_callbackSysKeyboardExec(MAV_window *w, MAV_object *o, MAV_keyboardEvent *ke)
{
  return (mav_callbackExec(mav_callback_sysKeyboard, w, o, (void *) ke, NULL));
}



/* Routines to implement functionality of reserved keystrokes */

void mavlib_sf1(MAV_window *w)
{
  if (w->vmp) {
    w->vmp->offset*=0.9;
    fprintf(stderr, "Offset %f\n", w->vmp->offset);
  }
}

void mavlib_sf2(MAV_window *w)
{
  if (w->vmp) {
    w->vmp->offset*=0.99;
    fprintf(stderr, "Offset %f\n", w->vmp->offset);
  }
}

void mavlib_sf3(MAV_window *w)
{
  if (w->vmp) {
    w->vmp->offset*=1.01;
    fprintf(stderr, "Offset %f\n", w->vmp->offset);
  }
}

void mavlib_sf4(MAV_window *w)
{
  if (w->vmp) {
    w->vmp->offset*=1.1;
    fprintf(stderr, "Offset %f\n", w->vmp->offset);
  }
}

void mavlib_sf5(MAV_window *w)
{
  MAV_viewModifierFn fn;

  if (mav_opt_stereo) {
    fn= mav_win_left->mod;
    mav_win_left->mod= mav_win_right->mod;
    mav_win_right->mod= fn;
    fprintf(stderr, "Windows swapped\n");
  }
}

float mavlib_offsetbackup=0.0;

void mavlib_sf6(MAV_window *w)
{
  if (w->vmp) {
    if (w->vmp->offset==0.0)
    {
      w->vmp->offset= mavlib_offsetbackup;
    }
    else
    {
      mavlib_offsetbackup= w->vmp->offset;
      w->vmp->offset=0.0;
    }
    
    fprintf(stderr, "Offset %f\n", w->vmp->offset);
  }
}

void mavlib_sf7(MAV_window *w)
{
  printf("\nCurrent window:\n");
  printf(" name: %s\n", w->name);
  printf(" width: %i\n", w->width);
  printf(" height: %i\n", w->height);
  if (w->orthogonal)
  {
    printf(" ortho size: %f\n", w->ortho_size);
  }
  else
  {
    printf(" fov: %f\n", w->fov);
  }
  printf(" apsect: %f\n", w->aspect);
  printf(" ncp: %f\n", w->ncp);
  printf(" fcp: %f\n", w->fcp);
  mav_viewParamsPrint("\nCurrent view parameters:\n", *w->vp);
}

int mavlib_filledorwire=1;

void mavlib_sf8(MAV_window *w)
{
  mavlib_filledorwire=!mavlib_filledorwire;
  mav_windowPolygonModeSet(mav_win_all, mavlib_filledorwire);
}

int mavlib_multiSample=1;

void mavlib_sf9(MAV_window *w)
{
  if (mav_opt_multiSample)
  {
    mavlib_multiSample=!mavlib_multiSample;
    mav_windowMultiSampleSet(mav_win_all, mavlib_multiSample);
    fprintf(stderr, "multisample %i\n", mavlib_multiSample);
  }
  else
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "multisample option not selected\n");
  }
}

int mavlib_drawMouse=0;

void mavlib_sf10(MAV_window *w)
{
  mavlib_drawMouse=!mavlib_drawMouse;

  if (mavlib_drawMouse)
  {
    fprintf(stderr, "Drawing mouse\n");
    mav_frameFn2Add(mav_mouseDraw, NULL);
    mav_needFrameDraw++;
  }
  else
  {
    fprintf(stderr, "Not drawing mouse\n");
    mav_frameFn2Rmv(mav_mouseDraw, NULL);
    mav_needFrameDraw--;
    mav_drawingMouse=MAV_FALSE;
  }
}

int mavlib_snapcnt=0;

void mavlib_sf11(MAV_window *w)
{
  char fn[100];
  sprintf(fn, "snap%i.ppm", mavlib_snapcnt);
  mav_windowDump(w, fn);
  mavlib_snapcnt++;
}

void mavlib_sf12(MAV_window *w)
{
  int i, r, g, b, a, d, doub, ar, ag, ab, aa, sb, msb;

  fprintf(stderr, "\n\n");
  mav_moduleDump();
  if (w->vmp) {
    fprintf(stderr, "Shift-F1 decrease stereo offset by 10%%\n");
    fprintf(stderr, "Shift-F2 decrease stereo offset by 1%%\n");
    fprintf(stderr, "Shift-F3 increase stereo offset by 1%%\n");
    fprintf(stderr, "Shift-F4 increase stereo offset by 10%%\n");
    fprintf(stderr, "Shift-F5 swap windows\n");
    fprintf(stderr, "Shift-F6 toggle stereo offset between value and 0\n");
  }
  fprintf(stderr, "Shift-F7 print window and view information\n");
  fprintf(stderr, "Shift-F8 toggle wireframe/filled\n");
  fprintf(stderr, "Shift-F9 toggle multisample (where applicable)\n");
  fprintf(stderr, "Shift-F10 toggle drawing mouse at world coordinates\n");
  fprintf(stderr, "Shift-F11 dump the window the mouse is in as snap[n].ppm\n");
  fprintf(stderr, "Shift-F12 info\n");
  fprintf(stderr, "Shift-Esc quit\n");
  
  for (i=0; i<15; i++) if (mav_ctrlF_desc[i]) fprintf(stderr, "%s\n", mav_ctrlF_desc[i]);

  fprintf(stderr, "\n");
  if (w->vmp) fprintf(stderr, "Current stereo offset: %f\n", w->vmp->offset);
  fprintf(stderr, "Frame rate: %f fps (%f secs per frame)\n", mav_fps_avg, 1.0/mav_fps_avg);

  if (mav_gfxVisualInfoGet(&r, &g, &b, &a, &d, &doub, &ar, &ag, &ab, &aa, &sb, &msb)) {
    fprintf(stderr, "RGBA bits: %i %i %i %i  Depth bits: %i  ", r, g, b, a, d);
    if (doub)
    {
      fprintf(stderr, "Double buffered\n");
    }
    else
    {
      fprintf(stderr, "Single buffered\n");
    }

    if (mav_opt_accumBuf) fprintf(stderr, "Accumulation RGBA bits: %i %i %i %i\n", ar, ag, ab, aa);
    if (mav_opt_stencilBuf) fprintf(stderr, "Stencil bits: %i\n", sb);
    if (mav_opt_multiSample) fprintf(stderr, "Multisamples: %i\n", msb);
  }

  if (mav_gfx_vendor) fprintf(stderr, "Vendor: %s\n", mav_gfx_vendor);
  if (mav_gfx_renderer) fprintf(stderr, "Renderer: %s\n", mav_gfx_renderer);
  if (mav_gfx_version) fprintf(stderr, "Version: %s\n", mav_gfx_version);
}



/* Routine to deal with this event */

int mavlib_dealWithKeyboardEvent(int *info)
{
  MAV_keyboardEvent ke;
  int i, rv= MAV_FALSE;

  /* Make up event data structure */

  ke.win= mavlib_getWindow(info[0]);
  ke.x= info[1];
  ke.y= info[2];
  ke.root_x= info[3];
  ke.root_y= info[4];

  switch (info[5]) {
  case 0:
    ke.movement= MAV_PRESSED;
    break;
  case 1:
    ke.movement= MAV_RELEASED;
    break;
  }

  ke.key= info[6];

  for (i=0; i<MAV_MODIFIER_MAX; i++) {
    if (info[i+7]) 
    {
      ke.modifiers[i]= MAV_PRESSED;
    }
    else
    {
      ke.modifiers[i]= MAV_RELEASED;
    }
  }

  /* Check for reserved keystrokes */

  if (ke.movement==MAV_PRESSED && ke.modifiers[MAV_MODIFIER_SHIFT]==MAV_PRESSED) {
    if (ke.key==MAV_KEY_F1) {mavlib_sf1(ke.win); return -100;}
    if (ke.key==MAV_KEY_F2) {mavlib_sf2(ke.win); return -100;}
    if (ke.key==MAV_KEY_F3) {mavlib_sf3(ke.win); return -100;}
    if (ke.key==MAV_KEY_F4) {mavlib_sf4(ke.win); return -100;}
    if (ke.key==MAV_KEY_F5) {mavlib_sf5(ke.win); return -100;}
    if (ke.key==MAV_KEY_F6) {mavlib_sf6(ke.win); return -100;}
    if (ke.key==MAV_KEY_F7) {mavlib_sf7(ke.win); return -100;}
    if (ke.key==MAV_KEY_F8) {mavlib_sf8(ke.win); return -100;}
    if (ke.key==MAV_KEY_F9) {mavlib_sf9(ke.win); return -100;}
    if (ke.key==MAV_KEY_F10) {mavlib_sf10(ke.win); return -100;}
    if (ke.key==MAV_KEY_F11) {mavlib_sf11(ke.win); return -100;}
    if (ke.key==MAV_KEY_F12) {mavlib_sf12(ke.win); return -100;}
    if (ke.key==27) exit(1);
  }

  if (ke.movement==MAV_PRESSED && ke.modifiers[MAV_MODIFIER_CTRL]==MAV_PRESSED) {
    for (i=1; i<=12; i++) {
      if (ke.key==MAV_KEY_F1+i-1 && mav_ctrlF[i]) {
	(*mav_ctrlF[i])(ke.win);
	return -100;
      }
    }
  }

  /* Check if system callback is defined - used for keyboard navigation */

  if (mav_callbackQuery(mav_callback_sysKeyboard, ke.win, mav_object_world))
  {
    rv= mav_callbackSysKeyboardExec(ke.win, mav_object_world, &ke);
    if (rv) return rv;
  }

  /* See what we hit */

  ke.line= mav_lineFrom2DPoint(ke.win, ke.x, ke.y);
  ke.intersects= mav_SMSIntersectLineAll(ke.win, ke.line, &ke.objint, &ke.obj);

  /* Check if any callbacks are defined for the world object */

  if (mav_callbackQuery(mav_callback_keyboard, ke.win, mav_object_world))
  {
    rv= mav_callbackKeyboardExec(ke.win, mav_object_world, &ke);
  }
  else
  {
    /* If we intersected, check for any class callbacks before specific class ones */

    if (ke.intersects) 
    {
      if (mav_callbackQuery(mav_callback_keyboard, ke.win, mav_object_any))
      {
	rv= mav_callbackKeyboardExec(ke.win, mav_object_any, &ke);
      }
      else
      {
	if (mav_callbackQuery(mav_callback_keyboard, ke.win, ke.obj))
	{
	  rv= mav_callbackKeyboardExec(ke.win, ke.obj, &ke);
	}
      }
    }
    else
    {
      /*  If no intersection check for none class */

      if (mav_callbackQuery(mav_callback_keyboard, ke.win, mav_object_none))
      {
	rv= mav_callbackKeyboardExec(ke.win, mav_object_none, &ke);
      }
    }
  }

  return rv;
}



/* Routine to query the status (pressed or released) of a key */

int mav_keyboardGet(int key)
{
  return (mav_gfxWindowKeyGet(key));
}
