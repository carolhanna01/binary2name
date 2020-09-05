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


#include "mavlib_navigation.h"



/* Various navigator routines to peform view parameter transformation */

void mav_navigateNull(MAV_viewParams *vp, float amount, float ls, float as)
{
  /* no movement */
}

void mav_navigateTransX(MAV_viewParams *vp, float amount, float ls, float as)
{
  /* x axis shift */
  vp->eye.x += (amount*ls);
}

void mav_navigateTransY(MAV_viewParams *vp, float amount, float ls, float as)
{
  /* y axis shift */
  vp->eye.y += (amount*ls);
}

void mav_navigateTransZ(MAV_viewParams *vp, float amount, float ls, float as)
{
  /* z axis shift */
  vp->eye.z += (amount*ls);
}

void mav_navigateRotRight(MAV_viewParams *vp, float amount, float ls, float as)
{
/* rotate view vectors and eye about view right */
  vp->eye= mav_vectorAdd(vp->eye, mav_vectorScalar(mav_nav_center, -1));
  vp->eye= mav_vectorRotate(vp->eye, vp->right, amount*as);
  vp->eye= mav_vectorAdd(vp->eye, mav_nav_center);
  vp->view= mav_vectorRotate(vp->view, vp->right, amount*as);
  vp->up= mav_vectorRotate(vp->up, vp->right, amount*as);
}

void mav_navigateRotUp(MAV_viewParams *vp, float amount, float ls, float as)
{
/* rotate view vectors and eye about view up */
  vp->eye= mav_vectorAdd(vp->eye, mav_vectorScalar(mav_nav_center, -1));
  vp->eye= mav_vectorRotate(vp->eye, vp->up, amount*as);
  vp->eye= mav_vectorAdd(vp->eye, mav_nav_center);
  vp->view= mav_vectorRotate(vp->view, vp->up, amount*as);
  vp->right= mav_vectorRotate(vp->right, vp->up, amount*as);
}

void mav_navigateRotFixedUp(MAV_viewParams *vp, float amount, float ls, float as)
{
/* rotate view vectors and eye about fixed view up */
  vp->eye= mav_vectorAdd(vp->eye, mav_vectorScalar(mav_nav_center, -1));
  vp->eye= mav_vectorRotate(vp->eye, vp->fixed_up, amount*as);
  vp->eye= mav_vectorAdd(vp->eye, mav_nav_center);
  vp->view= mav_vectorRotate(vp->view, vp->fixed_up, amount*as);
  vp->right= mav_vectorRotate(vp->right, vp->fixed_up, amount*as);
  vp->up= mav_vectorRotate(vp->up, vp->fixed_up, amount*as);
}

void mav_navigateForwards(MAV_viewParams *vp, float amount, float ls, float as)
{
  /* view direction shift */
  mav_navigateTransX(vp, vp->view.x * amount, ls, as);
  mav_navigateTransY(vp, vp->view.y * amount, ls, as);
  mav_navigateTransZ(vp, vp->view.z * amount, ls, as);
}

void mav_navigateUp(MAV_viewParams *vp, float amount, float ls, float as)
{
  /* view up shift */
  mav_navigateTransX(vp, vp->up.x * amount, ls, as);
  mav_navigateTransY(vp, vp->up.y * amount, ls, as);
  mav_navigateTransZ(vp, vp->up.z * amount, ls, as);
}

void mav_navigateUpFixedUp(MAV_viewParams *vp, float amount, float ls, float as)
{
  /* view fixed up shift */
  mav_navigateTransX(vp, vp->fixed_up.x * amount, ls, as);
  mav_navigateTransY(vp, vp->fixed_up.y * amount, ls, as);
  mav_navigateTransZ(vp, vp->fixed_up.z * amount, ls, as);
}

void mav_navigateRight(MAV_viewParams *vp, float amount, float ls, float as)
{
  /* view right shift */
  mav_navigateTransX(vp, vp->right.x * amount, ls, as);
  mav_navigateTransY(vp, vp->right.y * amount, ls, as);
  mav_navigateTransZ(vp, vp->right.z * amount, ls, as);
}

void mav_navigateForwardsFixedUp(MAV_viewParams *vp, float amount, float ls, float as)
{
  MAV_vector vr, tmp;

  /* forward shift perpendicular to fixed up vector */
  if (mav_vectorDotProduct(vp->up, vp->fixed_up)<0.0) amount= -1.0*amount;
    
  vr= mav_vectorNormalize(mav_vectorCrossProduct(vp->view, vp->fixed_up));
  tmp= mav_vectorNormalize(mav_vectorCrossProduct(vp->fixed_up, vr));
    
  mav_navigateTransX(vp, tmp.x * amount, ls, as);
  mav_navigateTransY(vp, tmp.y * amount, ls, as);
  mav_navigateTransZ(vp, tmp.z * amount, ls, as);
}

void mav_navigateTransForwardsFixedUp(MAV_viewParams *vp, float amount, float ls, float as)
{
  MAV_vector vr, tmp;

  /* trans_forward shift perpendicular to fixed up vector */
  if (mav_vectorDotProduct(vp->trans_up, vp->fixed_up)<0.0) amount= -1.0*amount;
    
  vr= mav_vectorNormalize(mav_vectorCrossProduct(vp->trans_view, vp->fixed_up));
  tmp= mav_vectorNormalize(mav_vectorCrossProduct(vp->fixed_up, vr));
    
  mav_navigateTransX(vp, tmp.x * amount, ls, as);
  mav_navigateTransY(vp, tmp.y * amount, ls, as);
  mav_navigateTransZ(vp, tmp.z * amount, ls, as);
}

void mav_navigateRightFixedUp(MAV_viewParams *vp, float amount, float ls, float as)
{
  MAV_vector vr;
  
  /* right shift perpendicular to fixed up vector */
  if (mav_vectorDotProduct(vp->up, vp->fixed_up)<0.0) amount= -1.0*amount;
  
  vr= mav_vectorNormalize(mav_vectorCrossProduct(vp->view, vp->fixed_up));
  
  mav_navigateTransX(vp, vr.x * amount, ls, as);
  mav_navigateTransY(vp, vr.y * amount, ls, as);
  mav_navigateTransZ(vp, vr.z * amount, ls, as);
}


void mav_navigateRoll(MAV_viewParams *vp, float amount, float ls, float as)
{
  /* roll */
  vp->up= mav_vectorRotate(vp->up, vp->view, amount*as);
  vp->right= mav_vectorRotate(vp->right, vp->view, amount*as);
}

void mav_navigatePitch(MAV_viewParams *vp, float amount, float ls, float as)
{
  /* pitch */
  vp->view= mav_vectorRotate(vp->view, vp->right, amount*as);
  vp->up= mav_vectorRotate(vp->up, vp->right, amount*as);
}

void mav_navigateYaw(MAV_viewParams *vp, float amount, float ls, float as)
{
  /* yaw */
  vp->view= mav_vectorRotate(vp->view, vp->up, amount*as);
  vp->right= mav_vectorRotate(vp->right, vp->up, amount*as);
}

void mav_navigatePitchFixedUp(MAV_viewParams *vp, float amount, float ls, float as)
{
  MAV_vector vr;

  /* pitch w.r.t. fixed up vector */
  if (mav_vectorDotProduct(vp->up, vp->fixed_up)<0.0) amount= -1.0*amount;

  vr= mav_vectorNormalize(mav_vectorCrossProduct(vp->view, vp->fixed_up));
  
  vp->view= mav_vectorRotate(vp->view, vr, amount*as);
  vp->up= mav_vectorRotate(vp->up, vr, amount*as);
}

void mav_navigateYawFixedUp(MAV_viewParams *vp, float amount, float ls, float as)
{
  /* yaw w.r.t. fixed up vector */
  if (mav_vectorDotProduct(vp->up, vp->fixed_up)<0.0) amount= -1.0*amount;

  vp->view= mav_vectorRotate(vp->view, vp->fixed_up, amount*as);
  vp->up= mav_vectorRotate(vp->up, vp->fixed_up, amount*as);
  vp->right= mav_vectorRotate(vp->right, vp->fixed_up, amount*as);
}

