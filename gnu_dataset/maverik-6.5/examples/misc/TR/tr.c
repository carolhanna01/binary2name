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
#include "mav_tr.h"
#include <stdio.h>
#include <math.h>

MAV_avatar *a;
float yaw=0.0;
int drawbb=0;

int fc=0;
float ox, oy;

/* Move left hand in a circle for 60 frames */
void shake(void *ignored)
{
  a->left_hand.x= ox+cos(MAV_DEG2RAD(fc*6.0))*0.2-0.2;
  a->left_hand.y= oy+sin(MAV_DEG2RAD(fc*6.0))*0.2;

  fc++;
  if (fc>60) {
    mav_frameFn1Rmv(shake, NULL);
    fc=0;
  }
}

/* Move right hand away from and then back to body over 20 frames */
void punch(void *ignored)
{
  a->right_hand.z-= (10-fc)*0.004;

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
      a->right_hand.z-=0.01;
      a->left_hand.z-=0.01;
      break;
    case '-': /* hands closer in */
      a->right_hand.z+=0.01;
      a->left_hand.z+=0.01;
      break;
    case MAV_KEY_UP: /* hands up */
      a->right_hand.y+=0.01;
      a->left_hand.y+=0.01;
      break;
    case MAV_KEY_DOWN: /* hands down */
      a->right_hand.y-=0.01;
      a->left_hand.y-=0.01;
      break;
    case MAV_KEY_RIGHT: /* hands right */
      a->right_hand.x+=0.01;
      a->left_hand.x+=0.01;
      break;
    case MAV_KEY_LEFT: /* hands left */
      a->right_hand.x-=0.01;
      a->left_hand.x-=0.01;
      break;
    case 's': /* shake it all about */
      if (fc==0) {
	ox= a->left_hand.x;
	oy= a->left_hand.y;
	mav_frameFn1Add(shake, NULL);
      }
      break;
    case 'p': /* punch */
      if (fc==0) mav_frameFn1Add(punch, NULL);
      break;
    case 'o': /* hands on and off */
      a->holding_left= !a->holding_left;
      a->holding_right= !a->holding_right;
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

/* Initialise the Maverik system and avatar module */

  mav_initialise(&argc, argv);
  mav_avatarModuleInit();

/* Initialise the TR module */

  mav_TRModuleInit();

/* Create avatar */

  a= mav_avatarNew(NULL);
  a->matrix= MAV_ID_MATRIX;

/* Define event callbacks */

  mav_callbackKeyboardSet(mav_win_all, mav_class_world, key);
  mav_callbackMouseSet(MAV_MIDDLE_BUTTON, mav_win_all, mav_class_avatar, ouch);

/* Use default mouse navigation */

  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

/* Define an SMS to contain the avatar and it in it */
  
  sms= mav_SMSObjListNew();
  o= mav_objectNew(mav_class_avatar, a);
  mav_SMSObjectAdd(sms, o);

/* Main loop */

  while (1) {

/* Check for, and act on, any events */

    mav_eventsCheck();

/* Do what needs to be done at the start of a frame */

    mav_frameBegin();

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
