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
#include <math.h>

typedef struct {
  int frame;
  MAV_vector vec;
} MoveInfo;



void mover(void *o)
{
  /* Retrieve all information */
  MAV_object *obj= (MAV_object *) o;
  MAV_sphere *sph= (MAV_sphere *) mav_objectDataGet(obj);
  MoveInfo *mi= (MoveInfo *) sph->userdef;

  /* Update objects position */
  sph->matrix= mav_matrixXYZSet(sph->matrix, mav_vectorAdd(mav_matrixXYZGet(sph->matrix), mi->vec));

  /* Delete object when appropriate */
  mi->frame--;
  if (mi->frame==0) {
    mav_free(sph->userdef);
    mav_free(sph);
    mav_objectDelete(mav_objectDataWith(sph));
    mav_frameFn0Rmv(mover, o);
  }
}



int main(int argc, char *argv[]) 
{
  MAV_SMS *sms;
  MAV_sphere *sph;
  MAV_object *obj;
  MAV_timer t;
  MoveInfo *mi;
    
  /* Initialise the Maverik system */
  mav_initialise(&argc, argv);

  /* Define an SMS */
  sms= mav_SMSObjListNew();

  /* Use default mouse navigation */
  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

  /* Start timer */
  mav_timerStart(&t);

  /* Main loop */
  while (1) {

    /* Generate a new object every tenth of a second */
    mav_timerStop(&t);
    if (t.wall>0.1) {

      /* Reset timer */
      mav_timerStart(&t);

      /* Create new object */
      sph= (MAV_sphere *) mav_malloc(sizeof(MAV_sphere));
      sph->radius=1.0;
      sph->nchips=4;
      sph->nverts=8;
      sph->matrix= MAV_ID_MATRIX;
      sph->sp= mav_sp_default;

      /* Define for how long and in which direction the object moves */
      mi= (MoveInfo *) mav_malloc(sizeof(MoveInfo));
      mi->frame= 20+mav_random()*30;
      mi->vec= mav_vectorSet(-1+mav_random()*2, mav_random(), -1+mav_random()*2);

      /* Store this movement info in the sphere's data structure */
      sph->userdef= mi;

      /* Add object to the SMS */
      obj= mav_objectNew(mav_class_sphere, sph);
      mav_SMSObjectAdd(sms, obj);

      /* Define a function to update the objects position */
      mav_frameFn0Add(mover, obj);
    }

    /* Check for, and act on, any events */
    mav_eventsCheck();

    /* Do what needs to be done at the start of a frame */
    mav_frameBegin();

    /* Display the objects to all windows */
    mav_SMSDisplay(mav_win_all, sms);

    /* Do what needs to be done at the end of a frame */
    mav_frameEnd();
  }
}


