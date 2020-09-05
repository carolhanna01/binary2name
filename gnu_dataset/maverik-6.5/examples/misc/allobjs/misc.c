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


#include "allobjs.h"
#include <stdlib.h>
#include <stdio.h>

void spinner(void *ignored)
{
  MAV_matrix *m;

/* vary the matrix of selected object by some amount */

  if (selected) {
    mav_callbackGetMatrixExec(mav_win_all, selected, &m);
    *m= mav_matrixMult(*m, mav_matrixSet(2.5, 5.2, 1.3, 0,0,0));
  }
}

void picker(void *ignored)
{
  MAV_line ln;
  MAV_objectIntersection objint;
  MAV_mouseEvent bogus;
  MAV_vector scrpos;

/* calculate line from eye point through current mouse position */

  ln= mav_lineFrom2DPoint(mav_win_left, mav_mouse_x, mav_mouse_y);
  bogus.win= mav_win_left;

/* intersect this line with the object and select/unselect accordingly */

  if (mav_SMSIntersectLineAll(mav_win_all, ln, &objint, &bogus.obj)) 
  {
    select_obj(NULL, &bogus);
  }
  else
  {
    unselect_obj(NULL, &bogus);
  }

/* display a message next to the cursor */

  scrpos= mav_vectorScrnPos(mav_mouse_pos);
  mav_stringDisplay(mav_win_all, "auto select", MAV_COLOUR_RED, 0, scrpos.x+0.1, scrpos.y);
}


void boxer(void *ignored)
{
  MAV_BB bb;

/* draw BB of selected object */

  if (selected) {
    mav_callbackBBExec(mav_win_all, selected, &bb);
    mav_BBDisplay(mav_win_all, bb);
  }
}



void lookabout(MAV_window *w)
{
  float ax, ay;

/* calcaulated translated view parameters */

  w->vp->trans_eye= w->vp->eye;
  w->vp->trans_view= w->vp->view;
  w->vp->trans_up= w->vp->up;
  w->vp->trans_right= w->vp->right;

/* rotate view parameters by an amount governed by the mouse pos */

  ax= (320-mav_mouse_x)*0.01;
  ay= (256-mav_mouse_y)*0.01;

/* pitch view by amount ay */

  w->vp->trans_view= mav_vectorRotate(w->vp->trans_view, w->vp->trans_right, ay);
  w->vp->trans_up= mav_vectorRotate(w->vp->trans_up, w->vp->trans_right, ay);

/* yaw view by amount ax */

  w->vp->trans_view= mav_vectorRotate(w->vp->trans_view, w->vp->trans_up, ax);
  w->vp->trans_right= mav_vectorRotate(w->vp->trans_right, w->vp->trans_up, ax);
}

