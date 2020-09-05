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

int drawlines=0;

/* Define a box */
void defBox(MAV_box *b, int col)
{
  b->size.x= 1.0; /* Specify its size */
  b->size.y= 2.0;
  b->size.z= 3.0;
  b->matrix= MAV_ID_MATRIX; /* Position and orientation */

  /* Define its "surface parameters", i.e. the colour with which its rendered */
  /* Use the sign of col to indicate a material or texture, and the value of col gives */
  /* the material or texture index to use */

  if (col>=0)
  {
    b->sp= mav_surfaceParamsNew(MAV_MATERIAL, 0, col, 0); /* Use material index col */
  }
  else
  {
    b->sp= mav_surfaceParamsNew(MAV_TEXTURE, 0, 0, -col); /* Use texture index col */
  }
}

/* Keyboard interaction callback */
int keyb(MAV_object *o, MAV_keyboardEvent *ke)
{
  if (ke->movement==MAV_PRESSED) {
    switch (ke->key) {
    case 'u': /* Toggle underlining */
      drawlines=!drawlines;
      break;
    }
  }

  return 1;
}

/* Render a frame */
void drawFrame(MAV_SMS *sms)
{
  int i;
  char s[100];

  /* Check for and act on any events */
  mav_eventsCheck();
    
  /* Request start of a new frame */
  mav_frameBegin();
    
  /* Display the SMS in all windows */
  mav_SMSDisplay(mav_win_all, sms);

  /* Display a string in each font */
  for (i=0; i<10; i++) {
    sprintf(s, "Some text in font %i", i);
    if (mav_palette_default->fontlist[i].defined) /* ensure font has been defined */
    {
      mav_stringDisplay(mav_win_all, s, MAV_COLOUR_BLACK, i, -0.9, -1.0+(i/5.0));

      /* Draw underlining lines if applicable */
      if (drawlines) {
	MAV_vector v1, v2;
	MAV_surfaceParams sp;
	int len;
	
	/* Calculate length of string in pixels */
	len= mav_stringLength(mav_win_current, s, i);

	/* Calculate endpoints of line in world coordinates */
        v1=mav_vectorWorldPos(mav_vectorSet(-0.9, -1.0+(i/5.0), 0.1));
        v2=mav_vectorWorldPos(mav_vectorSet(-0.9+(2.0*len)/mav_win_current->width, -1.0+(i/5.0), 0.1));
	
	/* Use the colour red */
	sp.mode= MAV_COLOUR;
	sp.colour= MAV_COLOUR_RED;
	mav_surfaceParamsUse(&sp);

	/* Draw a line */
	mav_gfxLineBegin();
	mav_gfxVertex(v1);
	mav_gfxVertex(v2);
	mav_gfxLineEnd();
      }
    }
    else
    {
      printf("font %i not defined\n", i);
    }
  }

  /* Request end of the frame */
  mav_frameEnd();
}

int main(int argc, char *argv[])
{
  MAV_box box;
  MAV_object *obj;
  MAV_SMS *sms;

  /* Initialise the Maverik system */
  mav_initialise(&argc, argv);

  /* Define a box object */
  defBox(&box, 2);

  /* Register the box as a Maverik object */
  obj= mav_objectNew(mav_class_box, &box);

  /* Create a SMS */
  sms= mav_SMSObjListNew();

  /* Add object to SMS */
  mav_SMSObjectAdd(sms, obj);

  /* Use default mouse navigation */
  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

  /* Define some fonts - use xfontsel to see what fonts are available */
  mav_paletteFontSet(mav_palette_default, 1, "-adobe-helvetica-bold-r-*-*-14-140-*-*-*-*-*-*");
  mav_paletteFontSet(mav_palette_default, 2, "-adobe-helvetica-bold-r-*-*-34-*-*-*-*-*-*-*");
  mav_paletteFontSet(mav_palette_default, 3, "-adobe-new century schoolbook-medium-r-*-*-25-*-*-*-*-*-*-*");
  mav_paletteFontSet(mav_palette_default, 4, "-adobe-new century schoolbook-medium-i-*-*-25-*-*-*-*-*-*-*");
  mav_paletteFontSet(mav_palette_default, 5, "-adobe-symbol-*-*-*-*-25-*-*-*-*-*-*-*");
  mav_paletteFontSet(mav_palette_default, 6, "-adobe-new century schoolbook-medium-r-*-*-7-*-*-*-*-*-*-*");
  mav_paletteFontSet(mav_palette_default, 7, "-sgi-rock-*-*-*-*-40-*-*-*-*-*-*-*");
  mav_paletteFontSet(mav_palette_default, 8, "-sgi-sorority-*-*-*-*-24-*-*-*-*-*-*-*");
  mav_paletteFontSet(mav_palette_default, 9, "-sgi-djb-*-*-*-*-24-*-*-*-*-*-*-*");

  /* Define keyboard interaction callback */
  mav_callbackKeyboardSet(mav_win_all, mav_class_world, keyb);

  /* Rendering loop */
  while (1) drawFrame(sms);
}
