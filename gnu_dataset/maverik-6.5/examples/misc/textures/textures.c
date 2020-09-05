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

MAV_sphere s;
MAV_rectangle r;
MAV_composite c;
int ab=0;



/* Routine to set the alpha value of the texture using the mouses x position */

void blend(void *ignored)
{
  if (s.sp->texture==3) 
  {
    mav_paletteTextureColourAlphaSet(mav_palette_default, s.sp->texture, 0,0,0, ((float) mav_mouse_x)/mav_win_current->width);
  }
  else
  {
    mav_paletteTextureAlphaSet(mav_palette_default, s.sp->texture, ((float) mav_mouse_x)/mav_win_current->width);
  }
}



/* Keyboard callback routine */

int keyb(MAV_object *o, MAV_keyboardEvent *e)
{
  switch (e->key) {
  case '1': /* Use a particular texture */
  case '2':
  case '3':
    s.sp->texture=e->key-48;
    r.sp->texture=e->key-48;
    break;

  case 'm': /* Use material */
    s.sp->mode=MAV_MATERIAL;
    r.sp->mode=MAV_MATERIAL;
    break;

  case 't':  /* Use decal texture */
    s.sp->mode=MAV_TEXTURE;
    r.sp->mode=MAV_TEXTURE;
    break;

  case 'l': /* Use lit texture */
    s.sp->mode=MAV_LIT_TEXTURE;
    r.sp->mode=MAV_LIT_TEXTURE;
    break;
    
  case 'b': /* Use blended texture */
    s.sp->mode=MAV_BLENDED_TEXTURE;
    r.sp->mode=MAV_BLENDED_TEXTURE;
    break;

  case 'a': /* toggle alpha blending */
    if (e->movement==MAV_PRESSED) {
      ab=!ab;
      if (ab) 
      {
	mav_windowBlendSet(mav_win_all, MAV_BLEND_1);
	printf("enabled alpha blending\n");
      }
      else
      {
	mav_windowBlendSet(mav_win_all, MAV_BLEND_OFF);
	printf("disabled alpha blending\n");
      }
    }
    break;

  case 'z': /* set alpha of texture from mouse position */
    if (e->movement==MAV_PRESSED) 
    {
      mav_frameFn2Add(blend, NULL);
    }
    else
    {
      mav_frameFn2Rmv(blend, NULL);
    }
    break;
  }

  return 1;
}



int main(int argc, char *argv[])
{
  MAV_object *o1, *o2;
  MAV_SMS *sms, *bobj;

  /* Initialise the Maverik system */
  mav_initialise(&argc, argv);

  /* Define two objects to texture */
  s.radius=2;
  s.nverts= 20;
  s.nchips= 10;
  s.sp= mav_surfaceParamsNew(MAV_MATERIAL, 0, 1, 1);
  s.matrix= mav_matrixSet(20,10,34, -3,0,0);
  o1= mav_objectNew(mav_class_sphere, &s);

  r.width= 4;
  r.height= 4;
  r.sp= mav_surfaceParamsNew(MAV_MATERIAL, 0, 1, 1);
  r.xtile=1.0;
  r.ytile=1.0;
  r.matrix= mav_matrixSet(0,0,20, 3,0,0);
  o2= mav_objectNew(mav_class_rectangle, &r);

  /* Put them in an SMS */
  sms= mav_SMSObjListNew();
  mav_SMSObjectAdd(sms, o1);
  mav_SMSObjectAdd(sms, o2);

  /* Define lights, materials and textures */
  mav_paletteLightingModelSet(mav_palette_default, 0.3, 0.3, 0.3, 1.0, MAV_TRUE);

  mav_paletteLightSet(mav_palette_default, 1, 0,0,0,0, 1,1,1,1, 1,1,1,1);
  mav_paletteLightPos(mav_palette_default, 1, mav_vectorSet(10,15,5));

  mav_paletteMaterialSet(mav_palette_default, 1, 0.3, 0.3, 0.3, 1.0,  0.6, 0.6, 0.6, 1.0, 1.0, 1.0, 1.0, 1.0,  0.0, 0.0, 0.0, 1.0, 125.0);

  mav_paletteTextureSet(mav_palette_default, 1, "marble_floor.ppm");
  mav_paletteTextureSet(mav_palette_default, 2, "stainedglass.ppm");
  mav_paletteTextureSet(mav_palette_default, 3, "tree3.ppm");

  /* Define a background object to check alpha blending */  
  mav_compositeReadAC3D("./background.ac", &c, MAV_ID_MATRIX);
  c.matrix= mav_matrixScaleSet(mav_matrixSet(0,0,0,0,0,-5), 4.0);
  bobj= mav_SMSObjListNew();
  mav_SMSObjectAdd(bobj, mav_objectNew(mav_class_composite, &c));

  /* Define keyboard interaction */
  mav_callbackKeyboardSet(mav_win_all, mav_class_world, keyb);

  /* Main loop */
  while (1) {
    mav_eventsCheck();
    mav_frameBegin();
    mav_SMSDisplay(mav_win_all, bobj);
    mav_SMSDisplay(mav_win_all, sms);
    mav_frameEnd();
  }
}
