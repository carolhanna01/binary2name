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


#ifdef __CYGWIN__
#include <windows.h>
#endif

#include "mavlib_tr.h"
#include <stdio.h>
#include <stdlib.h>



int mavlib_trDL;
int mavlib_trDR;

void mavlib_TRfn1(void *ignored)
{
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Compiling display list...");

  /* Remove this function */
  mav_frameFn1Rmv(mavlib_TRfn1, NULL);

  /* Get empty display list numbers */
  mavlib_trDL= mav_gfxListsNew(1);

  if (mavlib_trDL==-1) {
    fprintf(stderr, "Error: failed to get a display list\n");
    exit(1);
  }

  if (mav_opt_stereo) {
    mavlib_trDR= mav_gfxListsNew(1);
    if (mavlib_trDR==-1) {
      fprintf(stderr, "Error: failed to get a display list\n");
      exit(1);
    }
  }

  /* Start display list */
  if (mav_opt_stereo)
  {
    if (mav_win_current==mav_win_left)
    {
      mav_gfxListNew(mavlib_trDL, MAV_DLISTS_COMPILE);
      mav_windowSet(mav_win_right);
      mav_gfxListNew(mavlib_trDR, MAV_DLISTS_COMPILE);
      mav_windowSet(mav_win_left);
    }
    else
    {
      mav_gfxListNew(mavlib_trDR, MAV_DLISTS_COMPILE);
      mav_windowSet(mav_win_left);
      mav_gfxListNew(mavlib_trDL, MAV_DLISTS_COMPILE);
      mav_windowSet(mav_win_right);
    }
  }
  else
  {
    mav_gfxListNew(mavlib_trDL, MAV_DLISTS_COMPILE);
  }
}



int mavlib_TROS;
extern int mavlib_snapcnt;

int mavlib_TRKey(MAV_object *o, MAV_keyboardEvent *ke)
{
  int rv=-1;

  if (ke->movement==MAV_PRESSED) {
    if (ke->key=='0' || ke->key=='2' || ke->key=='4' || ke->key=='8') {
      rv= -862;
      mavlib_TROS=ke->key-'0';
    }
    if (ke->key==27) {
      rv= -862;
      mavlib_TROS=0;
    }
   if (ke->key==13) rv= -862;
  }
  
  return rv;
}

int mavlib_TRKey2(MAV_object *o, MAV_keyboardEvent *ke)
{
  int rv=-1;

  if (ke->movement==MAV_PRESSED) {
    if (ke->key=='a') rv= -862;
    if (ke->key==27) rv= -862;
    if (ke->key==13) rv= -862;

    if (ke->key=='o') {
      rv= -862;
      mavlib_TROS= 0;
    }
  }
  
  return rv;
}



void mavlib_TRDump(MAV_window *w, int os, int dl, int aa)
{
#ifdef MAV_TR
  TRcontext *tr;
  GLubyte *tile, *img, *rowPtr;
  char filename[200];
  int r,c,more;
  float v, fov, aspect, ncp, fcp;
  int i, j, k, sx, sy, dx, dy;
  FILE *f;
  int tw, th; /* tile width, height */
  int ctw, cth; /* current tile width, height */

  /* Keep a copy of perspective info and keyboard callback */
  fov= w->fov;
  aspect= w->aspect;
  ncp= w->ncp;
  fcp= w->fcp;
    
  /* Create TR context */
  tr = trNew();
/* pick the largest tile size divisible by os but no bigger than w */
  trTileSize(tr, (w->width/os)*os, (w->height/os)*os, 0);
  tile = mav_malloc(w->width * w->height * 3 * sizeof(GLubyte));
  if (aa)
  {
    img = mav_malloc(w->width * w->height * 3 * sizeof(GLubyte));
  }
  else
  {
    img = mav_malloc(w->width * os * w->height * os * 3 * sizeof(GLubyte));
  }
  trTileBuffer(tr, GL_RGB, GL_UNSIGNED_BYTE, tile);
  trImageSize(tr, w->width*os, w->height*os);
  trPerspective(tr, w->fov, w->aspect, w->ncp, w->fcp);

  /* Draw tiles */
  more = 1;
  mav_gfxBufferReadSet(MAV_FRONT);

/* get general tile width and height */
  tw= trGet(tr, TR_TILE_WIDTH);
  th= trGet(tr, TR_TILE_HEIGHT);


  while (more) {
    trBeginTile(tr);
    r=trGet(tr, TR_CURRENT_ROW);
    c=trGet(tr, TR_CURRENT_COLUMN);

/* get this tile's width and height */
    ctw= trGet (tr, TR_CURRENT_TILE_WIDTH);
    cth= trGet (tr, TR_CURRENT_TILE_HEIGHT);

    mav_gfxListExec(dl);
    mav_gfxWindowBuffersSwap();
    more = trEndTile(tr);

    if (aa) 
    {
      /* Scale tile and copy into final image */
      for (sy=0, dy=r*th/os; sy<cth; sy+=os, dy++) {
	for (sx=0, dx=c*tw/os; sx<ctw; sx+=os, dx++) {
	  for (k=0; k<3; k++) {	    
	    v=0;
	    for (i=0; i<os; i++) {
	      for (j=0; j<os; j++) {
		v+= tile[((sy+j)*ctw*3)+((sx+i)*3)+k];
	      }
	    }
	    img[(dy*w->width*3)+(dx*3)+k]= v/(os*os);
	  }
	}
      }
    }
    else
    {
      /* Copy into final image */
      for (sy=0, dy=r*th; sy<cth; sy++, dy++) {
	for (sx=0, dx=c*tw; sx<ctw; sx++, dx++) {
	  for (k=0; k<3; k++) {
	    img[(dy*w->width*os*3)+(dx*3)+k]= tile[(sy*ctw*3)+(sx*3)+k];
	  }
	}
      }
    }
  }

  /* Write file */
  sprintf(filename, "snap%i.ppm", mavlib_snapcnt);
  f = fopen(filename, "w");
  if (!f) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Couldn't open image file: %s\n", filename);

    /* Free up context */
    trDelete(tr);
    mav_free(tile);
    mav_free(img);
  
    /* Restore original perspective */
    mav_windowPerspectiveSet(w, ncp, fcp, fov, aspect);

    return;
  }

  fprintf(f,"P6\n");
  fprintf(f,"# PPM-file created by Maverik\n");
  if (aa)
  {
    fprintf(f,"%i %i\n", w->width, w->height);
  }
  else
  {
    fprintf(f,"%i %i\n", w->width*os, w->height*os);
  }
  fprintf(f,"255\n");
  fclose(f);
  f = fopen(filename, "ab");  /* now append binary data */
  if (!f) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Couldn't append to image file: %s\n", filename);

    /* Free up context */
    trDelete(tr);
    mav_free(tile);
    mav_free(img);
  
    /* Restore original perspective */
    mav_windowPerspectiveSet(w, ncp, fcp, fov, aspect);

    return;
  }
  
  if (aa) 
  {
    for (i=0; i<w->height; i++) {
      /* Remember, OpenGL images are bottom to top.  Have to reverse. */
      rowPtr = img + (w->height-1-i) * w->width*3;
      fwrite(rowPtr, 1, w->width*3, f);
    }
  }
  else
  {
    for (i=0; i<w->height*os; i++) {
      /* Remember, OpenGL images are bottom to top.  Have to reverse. */
      rowPtr = img + (w->height*os-1-i) * w->width*os*3;
      fwrite(rowPtr, 1, w->width*os*3, f);
    }
  }

  fclose(f);

  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "written %s\n", filename);

  mavlib_snapcnt++;

  /* Free up context */
  trDelete(tr);
  mav_free(tile);
  mav_free(img);

  /* Restore original perspective */
  mav_windowPerspectiveSet(w, ncp, fcp, fov, aspect);
#endif
}



void mavlib_TRfn3(void *ignored)
{
  MAV_window *w= mav_win_current;
  MAV_callbackFn keyb;
  MAV_timer tim;
  int os=4, rv;
  float lw;
  int aa=0;

  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "done.\n");   

  /* Remove this function */
  mav_frameFn3Rmv(mavlib_TRfn3, NULL);

  /* End display lists */
  if (mav_opt_stereo)
  {
    if (mav_win_current==mav_win_left)
    {
      mav_gfxListEnd();
      mav_windowSet(mav_win_right);
      mav_gfxListEnd();
      mav_windowSet(mav_win_left);
    }
    else
    {
      mav_gfxListEnd();
      mav_windowSet(mav_win_left);
      mav_gfxListEnd();
      mav_windowSet(mav_win_right);
    }
  }
  else
  {
    mav_gfxListEnd();
  }

  /* Keep a copy of the keyboard callback */
  {
    MAV_object mav_object_all;
    mav_object_all.the_class= mav_class_all;
    mav_object_all.the_data= NULL;
    keyb= mav_callbackQuery(mav_callback_keyboard, mav_win_all, &mav_object_all);
  }

  /* Display image and prompt for number of oversamples */
  mav_windowSet(mav_win_left);
  mav_gfxListExec(mavlib_trDL);
  mav_stringDisplay(mav_win_left, "Got this image. Number of oversamples? (2, 4, or 8)", MAV_COLOUR_BLACK, 0, -0.9, 0.9);
  mav_stringDisplay(mav_win_left, "Got this image. Number of oversamples? (2, 4, or 8)", MAV_COLOUR_WHITE, 0, -0.9, 0.8);
  mav_gfxWindowBuffersSwap();

  if (mav_opt_stereo) {
    mav_windowSet(mav_win_right);
    mav_gfxListExec(mavlib_trDR);
    mav_stringDisplay(mav_win_right, "Got this image. Number of oversamples? (2, 4, or 8)", MAV_COLOUR_BLACK, 0, -0.9, 0.9);
    mav_stringDisplay(mav_win_right, "Got this image. Number of oversamples? (2, 4, or 8)", MAV_COLOUR_WHITE, 0, -0.9, 0.8);
    mav_gfxWindowBuffersSwap();
    mav_windowSet(mav_win_left);
  }

  /* Get response from user */
  mav_callbackKeyboardSet(mav_win_all, mav_class_all, mavlib_TRKey);
  mavlib_TROS=-1;
  rv= -1;
  mav_timerStart(&tim);
  do {
    rv= mav_eventsCheck();
    mav_timerStop(&tim);
  } while (!(tim.wall>5 || rv==-862));
  mav_callbackKeyboardSet(mav_win_all, mav_class_all, (MAV_callbackKeyboardFn) keyb);

  /* Set number of oversamples */
  if (mavlib_TROS!=-1) os= mavlib_TROS;

  if (mav_opt_output==MAV_VERBOSE && os!=0) fprintf(stderr, "using %i oversamples\n", os);
  
  if (os!=0) {

    /* Display image and prompt for antialiased or oversized image */
    mav_windowSet(mav_win_left);
    mav_gfxListExec(mavlib_trDL);
    mav_stringDisplay(mav_win_left, "Anti-aliased (a) or oversized (o) image?", MAV_COLOUR_BLACK, 0, -0.9, 0.9);
    mav_stringDisplay(mav_win_left, "Anti-aliased (a) or oversized (o) image?", MAV_COLOUR_WHITE, 0, -0.9, 0.8);
    mav_gfxWindowBuffersSwap();

    if (mav_opt_stereo) {
      mav_windowSet(mav_win_right);
      mav_gfxListExec(mavlib_trDR);
      mav_stringDisplay(mav_win_left, "Anti-aliased (a) or oversized (o) image?", MAV_COLOUR_BLACK, 0, -0.9, 0.9);
      mav_stringDisplay(mav_win_left, "Anti-aliased (a) or oversized (o) image?", MAV_COLOUR_WHITE, 0, -0.9, 0.8);
      mav_gfxWindowBuffersSwap();
      mav_windowSet(mav_win_left);
    }

    /* Get response from user */
    mav_callbackKeyboardSet(mav_win_all, mav_class_all, mavlib_TRKey2);
    mavlib_TROS=-1;
    rv= -1;
    mav_timerStart(&tim);
    do {
      rv= mav_eventsCheck();
      mav_timerStop(&tim);
    } while (!(tim.wall>5 || rv==-862));
    mav_callbackKeyboardSet(mav_win_all, mav_class_all, (MAV_callbackKeyboardFn) keyb);
    if (mavlib_TROS==-1) aa= 1;

    /* Set line width and dump windows */
    lw= mav_gfxLineWidthGet();
    mav_gfxLineWidthSet(lw*os);
    mavlib_TRDump(mav_win_left, os, mavlib_trDL, aa);
    mav_gfxLineWidthSet(lw);

    if (mav_opt_stereo) {
      printf("%c\n", 07);
      mav_sleep(3);
      mav_windowSet(mav_win_right);
      lw= mav_gfxLineWidthGet();
      mav_gfxLineWidthSet(lw*os);
      mavlib_TRDump(mav_win_right, os, mavlib_trDR, aa);
      mav_gfxLineWidthSet(lw);
    }
  }

  /* Set back to original window */
  mav_windowSet(w);

  /* Delete display lists */
  if (mav_opt_stereo)
  {
    if (mav_win_current==mav_win_left)
    {
      mav_gfxListsDelete(mavlib_trDL, 1);
      mav_windowSet(mav_win_right);
      mav_gfxListsDelete(mavlib_trDR, 1);
      mav_windowSet(mav_win_left);
    }
    else
    {
      mav_gfxListsDelete(mavlib_trDR, 1);
      mav_windowSet(mav_win_left);
      mav_gfxListsDelete(mavlib_trDL, 1);
      mav_windowSet(mav_win_right);
    }
  }
  else
  {
    mav_gfxListsDelete(mavlib_trDL, 1);
  }
}



/* Routine to activate TR on key press */

void mavlib_cf11(MAV_window *w)
{
  mav_frameFn1Add(mavlib_TRfn1, NULL);
  mav_frameFn3Add(mavlib_TRfn3, NULL);
}



/* Routines to initialise the module */

char *mav_TRModuleID(void)
{
  return "TR";
}

int mav_TRModuleInit(void)
{
  /* Add the new module */
  mav_moduleNew(mav_TRModuleID);

#ifdef MAV_TR
  /* Add Ctrl-Function key press to activate grab */
  if (mav_ctrlF[11] && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Ctrl-F11 key press already reserved, overwriting\n"); 
  mav_ctrlF[11]= mavlib_cf11;
  mav_ctrlF_desc[11]= "Ctrl-F11 dump hires image of screen as snap[n].ppm";
#else
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: code not compiled with TR option, ignoring\n");
#endif

  return 1;
}
