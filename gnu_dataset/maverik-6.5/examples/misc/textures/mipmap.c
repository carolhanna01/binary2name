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

int mip=1;



int keyb(MAV_object *o, MAV_keyboardEvent *ke)
{
  /* Toggle mipmapping of texture 1 on any key press */
  if (ke->movement==MAV_PRESSED) {
    mip=!mip;
    mav_paletteTextureMipmappingSet(mav_palette_default, 1, mip);
  }

  return 1;
}



int main(int argc, char *argv[])
{
  MAV_rectangle r1, r2;
  MAV_object *o1, *o2;
  MAV_SMS *sms;

  /* Initialise the Maverik system */
  /* mav_opt_mipmapping= MAV_TRUE; */ /* this causes all textures to be mipmapped by default */
  mav_initialise(&argc, argv);

  /* Define two rectangles to texture */
  r1.width= 4.0;
  r1.height= 4.0;
  r1.sp= mav_surfaceParamsNew(MAV_TEXTURE, 0, 0, 1);
  r1.xtile= 1.0;
  r1.ytile= 1.0;
  r1.matrix= mav_matrixSet(0,0,0, 3,0,0);
  o1= mav_objectNew(mav_class_rectangle, &r1);

  r2.width= 4.0;
  r2.height= 4.0;
  r2.sp= mav_surfaceParamsNew(MAV_TEXTURE, 0, 0, 2);
  r2.xtile= 1.0;
  r2.ytile= 1.0;
  r2.matrix= mav_matrixSet(0,0,0, -3,0,0);
  o2= mav_objectNew(mav_class_rectangle, &r2);

  /* Put them in an SMS */
  sms= mav_SMSObjListNew();
  mav_SMSObjectAdd(sms, o1);
  mav_SMSObjectAdd(sms, o2);

  /* Define textures (the same image but one is mipmapped the other not) */
  /* If a texture is to be mipmapped this must be specified before its defined */
  mav_paletteTextureMipmappingSet(mav_palette_default, 1, MAV_TRUE);
  mav_paletteTextureSet(mav_palette_default, 1, "marble_floor.ppm");
  mav_paletteTextureSet(mav_palette_default, 2, "marble_floor.ppm");

  /* Define keyboard interaction */
  mav_callbackKeyboardSet(mav_win_all, mav_class_world, keyb);

  /* Use default mouse navigation */
  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

  /* Main loop */
  while (1) {
    mav_eventsCheck();
    mav_frameBegin();
    mav_SMSDisplay(mav_win_all, sms);
    mav_frameEnd();
  }
}
