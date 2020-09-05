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

/* Define a cylinder */
void defCyl(MAV_cylinder *c, int col)
{
  c->radius= mav_random()*10.0;
  c->height= mav_random()*10.0;
  c->nverts= 5+mav_random()*5;
  c->endcap= MAV_TRUE;
  c->matrix= mav_matrixSet(mav_random()*360.0, mav_random()*360.0, mav_random()*360.0, 0,0,0);

  if (col>=0)
  {
    c->sp= mav_surfaceParamsNew(MAV_MATERIAL, 0, col, 0); /* Use material index col */
  }
  else
  {
    c->sp= mav_surfaceParamsNew(MAV_TEXTURE, 0, 0, -col); /* Use texture index col */
  }
}

/* Render a frame */
void drawFrame(MAV_SMS *sms)
{
  /* Check for and act on any events */
  mav_eventsCheck();
    
  /* Request start of a new frame */
  mav_frameBegin();
    
  /* Display the SMS in all windows */
  mav_SMSDisplay(mav_win_all, sms);

  /* Request end of the frame */
  mav_frameEnd();
}

MAV_cylinder cyl;
int rot=0;

int keyb(MAV_object *o, MAV_keyboardEvent *ke)
{
  if (ke->movement==MAV_PRESSED) {
    switch (ke->key) {
    case 'c':
      defCyl(&cyl, 1);
      break;
    case 'r':
      rot=!rot;
      break;
    case 'l':
      mav_opt_curveLOD=!mav_opt_curveLOD;
      printf("LOD %i\n", mav_opt_curveLOD);
      break;
    case '=':
      mav_opt_curveFactor*=1.1;
      printf("curve factor %f\n", mav_opt_curveFactor);
      break;
    case '-':
      mav_opt_curveFactor*=0.9;
      printf("curve factor %f\n", mav_opt_curveFactor);
      break;
    case ']':
      mav_opt_vertsMin++;
      printf("minimum vertices %i\n", mav_opt_vertsMin);
      break;
    case '[':
      mav_opt_vertsMin--;
      printf("minimum vertices %i\n", mav_opt_vertsMin);
      break;
    }
  }

  return 1;
}

int main(int argc, char *argv[])
{
  MAV_object *obj;
  MAV_SMS *sms;
  float r,p,y;

  /* Enable LOD option */
  mav_opt_curveLOD= MAV_TRUE;

  /* Initialise the Maverik system */
  mav_initialise(&argc, argv);

  /* Define a cylinder object */
  defCyl(&cyl, 1);

  /* Use default mouse navigation */
  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

  /* Register the cylinder as a Maverik object */
  obj= mav_objectNew(mav_class_cylinder, &cyl);

  /* Create a SMS */
  sms= mav_SMSObjListNew();

  /* Add object to SMS */
  mav_SMSObjectAdd(sms, obj);

  /* Define keyboard callback */
  mav_callbackKeyboardSet(mav_win_all, mav_class_world, keyb);

  r=0;
  p=0;
  y=0;

  /* Rendering loop */
  while (1) {
    if (rot) {
      r+=1.2;
      p+=0.3;
      y+=0.7;
      cyl.matrix= mav_matrixSet(r,p,y,0,0,0);
    }

    drawFrame(sms);
  }
}
