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
#include "mav_avatar.h"
#include <stdio.h>
#include <math.h>

MAV_avatar *a;
float yaw=0.0;
int drawbb=0;
int handmode=0;
MAV_vector lefthand, righthand;
MAV_matrix *mat=NULL;
float hitDist;
MAV_vector hitOffset;

int fc=0;
float ox, oy;

/* Move left hand in a circle for 60 frames */
void shake(void *ignored)
{
  lefthand.x= ox+cos(MAV_DEG2RAD(fc*6.0))*0.2-0.2;
  lefthand.y= oy+sin(MAV_DEG2RAD(fc*6.0))*0.2;

  fc++;
  if (fc>60) {
    mav_frameFn1Rmv(shake, NULL);
    fc=0;
  }
}

/* Move right hand away from and then back to body over 20 frames */
void punch(void *ignored)
{
  righthand.z-= (10-fc)*0.004;

  fc++;
  if (fc>20) {
    mav_frameFn1Rmv(punch, NULL);
    fc=0;
  }
}

int ouch(MAV_object *o, MAV_mouseEvent *me)
{
  if (me->movement==MAV_PRESSED) {
    printf("ouch, got me\n");
  }

  return 1;
}



void mover(void *ignored)
{
  MAV_vector v;

  /* calculate new position of object */
  v= mav_vectorAdd(mav_win_current->vp->eye, mav_vectorScalar(mav_mouse_dir, hitDist));

  *mat= mav_matrixXYZSet(*mat, mav_vectorSub(v, hitOffset));
}

int selected(MAV_object *obj, MAV_mouseEvent *me)
{
  if (me->movement==MAV_PRESSED)
  {
    obj= me->obj;
    mav_callbackGetMatrixExec(mav_win_all, obj, &mat);

/* store the distance to the intersection and the distance from that
   to the centre of the object */

    hitDist= me->objint.pt1;
    hitOffset= mav_vectorAdd(me->line.pt, mav_vectorScalar(me->line.dir, hitDist));
    hitOffset= mav_vectorSub(hitOffset, mav_matrixXYZGet(*mat));

    mav_frameFn2Add(mover, NULL);
    mav_callbackMouseSet(MAV_MIDDLE_BUTTON, mav_win_all, mav_class_world, selected);
  }
  else
  {
    mav_frameFn2Rmv(mover, NULL);
    mav_callbackMouseSet(MAV_MIDDLE_BUTTON, mav_win_all, mav_class_world, NULL);
  }

  return 1;
}

int key(MAV_object *o, MAV_keyboardEvent *ke)
{
  if (ke->movement==MAV_PRESSED) {
    switch (ke->key) {
    case 'q': /* increase walking speed */
      a->speed+=0.1;
      break;
    case 'w': /* decrease walking speed */
      a->speed-=0.1;
      break;
    case 'a': /* toggle walking animation */
      a->animate=!a->animate;
      break;
    case 'm': /* toggle update position */
      a->move=!a->move;
      break;
    case 'y': /* yaw the avatar */
      yaw+=5;
      a->matrix= mav_matrixRPYSet(a->matrix, 0,0,yaw);
      break;
    case 'b': /* toggle drawing BB */
      drawbb=!drawbb;
      break;
    case '=': /* hands further away */
      righthand.z-=0.03;
      lefthand.z-=0.03;
      break;
    case '-': /* hands closer in */
      righthand.z+=0.03;
      lefthand.z+=0.03;
      break;
    case MAV_KEY_UP: /* hands up */
      righthand.y+=0.03;
      lefthand.y+=0.03;
      break;
    case MAV_KEY_DOWN: /* hands down */
      righthand.y-=0.03;
      lefthand.y-=0.03;
      break;
    case MAV_KEY_RIGHT: /* hands right */
      righthand.x+=0.03;
      lefthand.x+=0.03;
      break;
    case MAV_KEY_LEFT: /* hands left */
      righthand.x-=0.03;
      lefthand.x-=0.03;
      break;
    case 's': /* shake it all about */
      if (fc==0) {
	ox= lefthand.x;
	oy= lefthand.y;
	mav_frameFn1Add(shake, NULL);
      }
      break;
    case 'p': /* punch */
      if (fc==0) mav_frameFn1Add(punch, NULL);
      break;
    case 'h': /* various hand modes */
      handmode++;
      if (handmode==3) handmode=0;
      if (handmode==0) printf("walking hand animation\n");
      if (handmode==1) printf("hands controled by cursor keys\n");
      if (handmode==2) printf("hands point to box\n");
      break;
    }
  }

  return 1;
}



int main(int argc, char *argv[]) 
{
  MAV_SMS *sms;
  MAV_object *o;
  MAV_BB bb;
  MAV_rectangle gp;
  MAV_box box;

/* Initialise the Maverik system and avatar module */

  mav_initialise(&argc, argv);
  mav_avatarModuleInit();

/* Create avatar */

  a= mav_avatarNew(NULL);
  a->matrix= MAV_ID_MATRIX;

/* Initial hand mode and position */

  handmode=0;
  lefthand= mav_vectorSet(0,-0.3,-0.5);
  righthand= mav_vectorSet(0,-0.3,-0.5);

/* Define event callbacks */

  mav_callbackKeyboardSet(mav_win_all, mav_class_world, key);
  mav_callbackMouseSet(MAV_MIDDLE_BUTTON, mav_win_all, mav_class_avatar, ouch);
  mav_callbackMouseSet(MAV_MIDDLE_BUTTON, mav_win_all, mav_class_box, selected);

/* Use default mouse navigation */

  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

/* Define an SMS to contain the avatar and it in it */
  
  sms= mav_SMSObjListNew();
  o= mav_objectNew(mav_class_avatar, a);
  mav_SMSObjectAdd(sms, o);

/* Define a ground plane */

  if (argc==1) {
    gp.width=25;
    gp.height=25;
    gp.xtile=5;
    gp.ytile=5;
    mav_paletteTextureSet(mav_palette_default, 1, "../../MPG/marble_floor.ppm");
    gp.sp= mav_surfaceParamsNew(MAV_TEXTURE, 0, 0, 1);
    gp.matrix= mav_matrixSet(0,-90,0,0,-1.72,0);
    mav_SMSObjectAdd(sms, mav_objectNew(mav_class_rectangle, &gp));
  }

/* Define a box */

  box.size= mav_vectorSet(0.1,0.1,0.1);
  box.sp= mav_sp_default;
  box.matrix= mav_matrixSet(0,0,0,0,0,-2);
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_box, &box));

/* Main loop */

  while (1) {

/* Check for, and act on, any events */

    mav_eventsCheck();

/* Do what needs to be done at the start of a frame */

    mav_frameBegin();

/* Set hand mode */

    switch (handmode) {

    case 0: /* normal animation */

      /* Set "holding" field to false to indicate hand are not positioned */
      a->holding_left= MAV_FALSE;
      a->holding_right= MAV_FALSE;
      break;

    case 1: /* hands controlled by cursor keys */

      /* Set "holding" field to true to indicate hands are positioned */
      a->holding_left= MAV_TRUE;
      a->holding_right= MAV_TRUE;

      /* Hand position is in world coordinates */
      a->left_hand= mav_vectorMult(lefthand, a->matrix);
      a->right_hand= mav_vectorMult(righthand, a->matrix);
      break;

    case 2: /* hands point to box */ 

      /* Set "holding" field to true to indicate hands are positioned */
      a->holding_left= MAV_TRUE;
      a->holding_right= MAV_TRUE;

      /* Set hands to point to box's position */
      a->left_hand= mav_matrixXYZGet(box.matrix);
      a->right_hand= mav_matrixXYZGet(box.matrix);
      break;
    }

/* Display the SMS to all windows */

    mav_SMSDisplay(mav_win_all, sms);

/* Draw BB if required */    

    if (drawbb) {
      mav_callbackBBExec(mav_win_current, o, &bb);
      mav_BBDisplay(mav_win_current, bb);
    }

/* Do what needs to be done at the end of a frame */

    mav_frameEnd();
  }
}


