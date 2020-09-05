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


#include "mavlib_callbacks.h"
#include <math.h>
#include <stdio.h>



/* Routine to calculate clip planes of a (partial) frustum */

MAV_clipPlanes mav_clipPlanesGet(MAV_window *the_win, float min_fx, float max_fx, float min_fy, float max_fy, float min_fz, float max_fz)
{
  MAV_vector bot_left, top_right;
  MAV_vector eye, view, up, vr;
  MAV_vector centre, temp;
  MAV_clipPlanes clip_planes;
  float height, width;

  eye= the_win->eye;
  view= the_win->view;
  up= the_win->up;
  vr= the_win->right;

  clip_planes.num= 6;

  if (the_win->orthogonal) {
/* get orthogonal clip planes */
    bot_left= mav_vectorAdd (the_win->eye, mav_vectorAdd (
	mav_vectorScalar (the_win->right,
		min_fx*0.5*the_win->ortho_size*the_win->aspect),
	mav_vectorScalar (the_win->up, min_fy*0.5*the_win->ortho_size)));
    top_right= mav_vectorAdd (the_win->eye, mav_vectorAdd (
	mav_vectorScalar (the_win->right,
		max_fx*0.5*the_win->ortho_size*the_win->aspect),
	mav_vectorScalar (the_win->up, max_fy*0.5*the_win->ortho_size)));

/* compute clip planes */

/* near */
    clip_planes.planes[0].norm= mav_vectorScalar (the_win->view, -1.0);
    clip_planes.planes[0].d= mav_vectorDotProduct (
		mav_vectorAdd (
			the_win->eye,
			mav_vectorScalar (the_win->view, min_fz*the_win->fcp)),
		clip_planes.planes[0].norm);

/* left */
    clip_planes.planes[1].norm= mav_vectorScalar (the_win->right, -1.0);
    clip_planes.planes[1].d= mav_vectorDotProduct (bot_left,
		clip_planes.planes[1].norm);

/* right */
    clip_planes.planes[2].norm= the_win->right;
    clip_planes.planes[2].d= mav_vectorDotProduct (top_right,
		clip_planes.planes[2].norm);

/* top */
    clip_planes.planes[3].norm= the_win->up;
    clip_planes.planes[3].d= mav_vectorDotProduct (top_right,
		clip_planes.planes[3].norm);

/* bottom */
    clip_planes.planes[4].norm= mav_vectorScalar (the_win->up, -1.0);
    clip_planes.planes[4].d= mav_vectorDotProduct (bot_left,
		clip_planes.planes[4].norm);

/* far */
    clip_planes.planes[5].norm= the_win->view;
    clip_planes.planes[5].d= mav_vectorDotProduct (
		mav_vectorAdd (
			the_win->eye,
			mav_vectorScalar (the_win->view, max_fz*the_win->fcp)),
		clip_planes.planes[5].norm);
  } else {
/* get perspective clip planes */
    height= tan (the_win->fov/2.0*MAV_PI_OVER_180) * (the_win->ncp);
    width= height * the_win->aspect;

    centre.x= eye.x + view.x * the_win->ncp;
    centre.y= eye.y + view.y * the_win->ncp;
    centre.z= eye.z + view.z * the_win->ncp;

    bot_left.x= centre.x + width * vr.x * min_fx + height * up.x * min_fy;
    bot_left.y= centre.y + width * vr.y * min_fx + height * up.y * min_fy;
    bot_left.z= centre.z + width * vr.z * min_fx + height * up.z * min_fy;

    top_right.x= centre.x + width * vr.x * max_fx + height * up.x * max_fy;
    top_right.y= centre.y + width * vr.y * max_fx + height * up.y * max_fy;
    top_right.z= centre.z + width * vr.z * max_fx + height * up.z * max_fy;

    /* compute clip planes */

    /* near plane */
    clip_planes.planes[0].norm.x= -view.x;
    clip_planes.planes[0].norm.y= -view.y;
    clip_planes.planes[0].norm.z= -view.z;

    temp.x= eye.x + view.x * the_win->fcp * min_fz;
    temp.y= eye.y + view.y * the_win->fcp * min_fz;
    temp.z= eye.z + view.z * the_win->fcp * min_fz;

    clip_planes.planes[0].d= mav_vectorDotProduct (temp, clip_planes.planes[0].norm);

    /* left plane */
    temp= mav_vectorSub (bot_left, eye);
    clip_planes.planes[1].norm= mav_vectorNormalize(mav_vectorCrossProduct (up, temp));
    clip_planes.planes[1].d= mav_vectorDotProduct (eye, clip_planes.planes[1].norm);

    /* right plane */
    temp= mav_vectorSub (top_right, eye);
    clip_planes.planes[2].norm= mav_vectorNormalize(mav_vectorCrossProduct (temp, up));
    clip_planes.planes[2].d= mav_vectorDotProduct (eye, clip_planes.planes[2].norm);

    /* top plane */
    clip_planes.planes[3].norm= mav_vectorNormalize(mav_vectorCrossProduct (vr, temp));
    clip_planes.planes[3].d= mav_vectorDotProduct (eye, clip_planes.planes[3].norm);

    /* bottom plane */
    temp= mav_vectorSub (bot_left, eye);
    clip_planes.planes[4].norm= mav_vectorNormalize(mav_vectorCrossProduct (temp, vr));
    clip_planes.planes[4].d= mav_vectorDotProduct (eye, clip_planes.planes[4].norm);

    /* far plane */
    clip_planes.planes[5].norm= view;
    temp.x= eye.x + view.x * the_win->fcp * max_fz;
    temp.y= eye.y + view.y * the_win->fcp * max_fz;
    temp.z= eye.z + view.z * the_win->fcp * max_fz;
    clip_planes.planes[5].d= mav_vectorDotProduct (temp, view);
  }

  return clip_planes;
}



/* As above but for a pixel defined viewport */

MAV_clipPlanes mav_clipPlanesGetFromPixels(MAV_window *the_win, int min_x, int max_x, int min_y, int max_y, float min_fz, float max_fz)
{
  float min_fx, max_fx, min_fy, max_fy;

  min_fx= ((float)min_x / (float)(the_win->width) - 0.5) * 2.0;
  min_fy= ((float)min_y / (float)(the_win->height) - 0.5) * 2.0;
  max_fx= ((float)max_x / (float)(the_win->width) - 0.5) * 2.0;
  max_fy= ((float)max_y / (float)(the_win->height) - 0.5) * 2.0;

  return (mav_clipPlanesGet (the_win, min_fx, max_fx, min_fy, max_fy, min_fz, max_fz));
}



/* As above but for a BB */

MAV_clipPlanes mav_clipPlanesGetFromBB(MAV_BB bb)
{
  MAV_clipPlanes rv;

  /* Make a set of clip planes out of the BB */
  rv.num= 6;

  rv.planes[0].norm= mav_vectorSet(-1,0,0);
  rv.planes[0].d= mav_vectorDotProduct(bb.min, rv.planes[0].norm);

  rv.planes[1].norm= mav_vectorSet(0,-1,0);
  rv.planes[1].d= mav_vectorDotProduct(bb.min, rv.planes[1].norm);

  rv.planes[2].norm= mav_vectorSet(0,0,-1);
  rv.planes[2].d= mav_vectorDotProduct(bb.min, rv.planes[2].norm);

  rv.planes[3].norm= mav_vectorSet(1,0,0);
  rv.planes[3].d= mav_vectorDotProduct(bb.max, rv.planes[3].norm);

  rv.planes[4].norm= mav_vectorSet(0,1,0);
  rv.planes[4].d= mav_vectorDotProduct(bb.max, rv.planes[4].norm);

  rv.planes[5].norm= mav_vectorSet(0,0,1);
  rv.planes[5].d= mav_vectorDotProduct(bb.max, rv.planes[5].norm);

  return rv;
}



/* Routines to perform either simple BB culling or accurate BB culling */

#if 0
int mav_BBIntersectsClipPlanes (MAV_BB bb, int *corner_list, MAV_clipPlanes *clip_planes)
{
/* simple fast BB culling */
/* this routine will return some false positives but no false negatives */
/* i.e. some objects that are outside the frustum may be flagged as */
/* being inside */
  int result= 1;
  int n;
  float res_1= 0, res_2= 0;

  /* n is current planes */
  n= 0;

  while ((n < clip_planes->num) && result) {
  /* check each plane in turn */
  /* only need to check 2 corners per plane */
  /* corner_list gives the corners that need checking */

    switch (corner_list[n]) {
      case 0:
        res_1= mav_vectorDotProduct (bb.min, clip_planes->planes[n].norm);
        res_2= mav_vectorDotProduct (bb.max, clip_planes->planes[n].norm);
        break;
      case 1:
        res_1= bb.min.x * clip_planes->planes[n].norm.x +
                    bb.min.y * clip_planes->planes[n].norm.y +
                    bb.max.z * clip_planes->planes[n].norm.z;
        res_2= bb.max.x * clip_planes->planes[n].norm.x +
                    bb.max.y * clip_planes->planes[n].norm.y +
                    bb.min.z * clip_planes->planes[n].norm.z;
        break;
      case 2:
        res_1= bb.min.x * clip_planes->planes[n].norm.x +
                    bb.max.y * clip_planes->planes[n].norm.y +
                    bb.max.z * clip_planes->planes[n].norm.z;
        res_2= bb.max.x * clip_planes->planes[n].norm.x +
                    bb.min.y * clip_planes->planes[n].norm.y +
                    bb.min.z * clip_planes->planes[n].norm.z;
        break;
      case 3:
        res_1= bb.min.x * clip_planes->planes[n].norm.x +
                    bb.max.y * clip_planes->planes[n].norm.y +
                    bb.min.z * clip_planes->planes[n].norm.z;
        res_2= bb.max.x * clip_planes->planes[n].norm.x +
                    bb.min.y * clip_planes->planes[n].norm.y +
                    bb.max.z * clip_planes->planes[n].norm.z;
    }

  /* check for enclosure */
    if ((res_1 > clip_planes->planes[n].d) && (res_2 > clip_planes->planes[n].d))
  /* no intersection */
      result= 0;
    else if ((res_1 > clip_planes->planes[n].d) || (res_2 > clip_planes->planes[n].d))
  /* crosses plane */
    result= 2;

    n++;
  }

  /* if the result is still 1 at this point then the frustum completely */
  /* encloses the bb */
  return (result);
}

#else

/****************  accurate culling routines *****************/


void mavlib_getXYZInt (MAV_vector pt, MAV_vector *int_pt, MAV_clipPlane cp)
{
  if (cp.norm.x != 0.0)
    int_pt->x= (cp.d - pt.y*cp.norm.y - pt.z*cp.norm.z)/cp.norm.x;
  if (cp.norm.y != 0.0)
    int_pt->y= (cp.d - pt.x*cp.norm.x - pt.z*cp.norm.z)/cp.norm.y;
  if (cp.norm.z != 0.0)
    int_pt->z= (cp.d - pt.x*cp.norm.x - pt.y*cp.norm.y)/cp.norm.z;
}

int mav_BBIntersectsClipPlanes (MAV_BB bb, int *corner_list, MAV_clipPlanes *clip_planes)
{
/* accurate BB culling */
  int result= 1;
  int n;
  float res_1=0, res_2=0;
  MAV_vector pos_corner, neg_corner, xyz_int;

  /* n is current plane */
  n= 0;

  while ((n < clip_planes->num) && result) {
  /* check each plane in turn */
  /* only need to check 2 corners per plane */
  /* corner_list gives the corners that need checking */
    switch (corner_list[n]) {
      case 0:
        res_1= mav_vectorDotProduct (bb.min, clip_planes->planes[n].norm);
        res_2= mav_vectorDotProduct (bb.max, clip_planes->planes[n].norm);
        break;
      case 1:
        res_1= bb.min.x * clip_planes->planes[n].norm.x +
                bb.min.y * clip_planes->planes[n].norm.y +
                bb.max.z * clip_planes->planes[n].norm.z;
        res_2= bb.max.x * clip_planes->planes[n].norm.x +
                bb.max.y * clip_planes->planes[n].norm.y +
                bb.min.z * clip_planes->planes[n].norm.z;
        break;
      case 2:
        res_1= bb.min.x * clip_planes->planes[n].norm.x +
                bb.max.y * clip_planes->planes[n].norm.y +
                bb.max.z * clip_planes->planes[n].norm.z;
        res_2= bb.max.x * clip_planes->planes[n].norm.x +
                bb.min.y * clip_planes->planes[n].norm.y +
                bb.min.z * clip_planes->planes[n].norm.z;
        break;
      case 3:
        res_1= bb.min.x * clip_planes->planes[n].norm.x +
                bb.max.y * clip_planes->planes[n].norm.y +
                bb.min.z * clip_planes->planes[n].norm.z;
        res_2= bb.max.x * clip_planes->planes[n].norm.x +
                bb.min.y * clip_planes->planes[n].norm.y +
                bb.max.z * clip_planes->planes[n].norm.z;
    }

  /* check for total enclosure */
    if ((res_1 > clip_planes->planes[n].d) && (res_2 > clip_planes->planes[n].d))
  /* no intersection */
      result= 0;
    else if ((res_1 > clip_planes->planes[n].d) || (res_2 > clip_planes->planes[n].d)) {
  /* partial enclosure */
      result= 2;

  /* now we need to clip the bb before continuing */
  /* this is necessary as some objects can be (partially) on the */
  /* positive side of all planes but still lie outside the frustum */
     if (res_1 <= clip_planes->planes[n].d) {
  /* corner 1 is on the +ve side of the plane */
        switch (corner_list[n]) {
          case 0:
  /* bb.min */
            pos_corner= bb.min;
            neg_corner= bb.max;
            xyz_int= neg_corner;
            mavlib_getXYZInt (pos_corner, &xyz_int, clip_planes->planes[n]);
            if (xyz_int.x < neg_corner.x) bb.max.x= xyz_int.x;
            if (xyz_int.y < neg_corner.y) bb.max.y= xyz_int.y;
            if (xyz_int.z < neg_corner.z) bb.max.z= xyz_int.z;
            break;
          case 1:
  /* bb.min.x, bb.min.y, bb.max.z */
            pos_corner= bb.min;
            pos_corner.z= bb.max.z;
            neg_corner= bb.max;
            neg_corner.z= bb.min.z;
            xyz_int= neg_corner;
            mavlib_getXYZInt (pos_corner, &xyz_int, clip_planes->planes[n]);
            if (xyz_int.x < neg_corner.x) bb.max.x= xyz_int.x;
            if (xyz_int.y < neg_corner.y) bb.max.y= xyz_int.y;
            if (xyz_int.z > neg_corner.z) bb.min.z= xyz_int.z;
            break;
          case 2:
  /* bb.min.x, bb.max.y, bb.max.z */
            pos_corner= bb.max;
            pos_corner.x= bb.min.x;
            neg_corner= bb.min;
            neg_corner.x= bb.max.x;
            xyz_int= neg_corner;
            mavlib_getXYZInt (pos_corner, &xyz_int, clip_planes->planes[n]);
            if (xyz_int.x < neg_corner.x) bb.max.x= xyz_int.x;
            if (xyz_int.y > neg_corner.y) bb.min.y= xyz_int.y;
            if (xyz_int.z > neg_corner.z) bb.min.z= xyz_int.z;
            break;
          case 3:
  /* bb.min.x, bb.max.y, bb.min.z */
            pos_corner= bb.min;
            pos_corner.y= bb.max.y;
            neg_corner= bb.max;
            neg_corner.y= bb.min.y;
            xyz_int= neg_corner;
            mavlib_getXYZInt (pos_corner, &xyz_int, clip_planes->planes[n]);
            if (xyz_int.x < neg_corner.x) bb.max.x= xyz_int.x;
            if (xyz_int.y > neg_corner.y) bb.min.y= xyz_int.y;
            if (xyz_int.z < neg_corner.z) bb.max.z= xyz_int.z;
        }
      } else {
  /* corner 2 is on the +ve side */
        switch (corner_list[n]) {
          case 0:
  /* bb.max */
            pos_corner= bb.max;
            neg_corner= bb.min;
            xyz_int= neg_corner;
            mavlib_getXYZInt (pos_corner, &xyz_int, clip_planes->planes[n]);
            if (xyz_int.x > neg_corner.x) bb.min.x= xyz_int.x;
            if (xyz_int.y > neg_corner.y) bb.min.y= xyz_int.y;
            if (xyz_int.z > neg_corner.z) bb.min.z= xyz_int.z;
            break;
          case 1:
  /* bb.max.x, bb.max.y, bb.min.z */
            pos_corner= bb.max;
            pos_corner.z= bb.min.z;
            neg_corner= bb.min;
            neg_corner.z= bb.max.z;
            xyz_int= neg_corner;
            mavlib_getXYZInt (pos_corner, &xyz_int, clip_planes->planes[n]);
            if (xyz_int.x > neg_corner.x) bb.min.x= xyz_int.x;
            if (xyz_int.y > neg_corner.y) bb.min.y= xyz_int.y;
            if (xyz_int.z < neg_corner.z) bb.max.z= xyz_int.z;
            break;
          case 2:
  /* bb.max.x, bb.min.y, bb.min.z */
            pos_corner= bb.min;
            pos_corner.x= bb.max.x;
            neg_corner= bb.max;
            neg_corner.x= bb.min.x;
            xyz_int= neg_corner;
            mavlib_getXYZInt (pos_corner, &xyz_int, clip_planes->planes[n]);
            if (xyz_int.x > neg_corner.x) bb.min.x= xyz_int.x;
            if (xyz_int.y < neg_corner.y) bb.max.y= xyz_int.y;
            if (xyz_int.z < neg_corner.z) bb.max.z= xyz_int.z;
            break;
          case 3:
  /* bb.max.x, bb.min.y, bb.max.z */
            pos_corner= bb.max;
            pos_corner.y= bb.min.y;
            neg_corner= bb.min;
            neg_corner.y= bb.max.y;
            xyz_int= neg_corner;
            mavlib_getXYZInt (pos_corner, &xyz_int, clip_planes->planes[n]);
            if (xyz_int.x > neg_corner.x) bb.min.x= xyz_int.x;
            if (xyz_int.y < neg_corner.y) bb.max.y= xyz_int.y;
            if (xyz_int.z > neg_corner.z) bb.min.z= xyz_int.z;
          }
        }
      }

    n ++;
  }

  /* if result is still 1 at this point then the frustum competely */
  /* encloses the bb */
  return (result);
}

#endif



/* support routine to above */

int mav_BBGetCorner (MAV_vector norm)
{
/* returns an integer defining which corner pair of an axis-aligned */
/* bounding box would have to be tested against a plane with normal */
/* 'norm' to test which side of the plane the box was on */
  if (norm.x > 0.0) {
    if (norm.y > 0.0) {
      if (norm.z > 0.0)
/* x>0 y>0 z>0 */
        return (0);
      else
/* x>0 y>0 z<=0 */
        return (1);
    } else {
      if (norm.z > 0.0)
/* x>0 y<=0 z>0 */
        return (3);
      else
/* x>0 y<=0 z<=0 */
        return (2);
    }
  } else {
    if (norm.y > 0.0) {
      if (norm.z > 0.0)
/* x<=0 y>0 z>0 */
        return (2);
      else
/* x<=0 y>0 z<=0 */
        return (3);
    } else {
      if (norm.z > 0.0)
/* x<=0 y<=0 z>0 */
        return (1);
      else
/* x<=0 y<=0 z<=0 */
        return (0);
    }
  }
}



/* Routine to cull a BB */

int mav_BBCull (MAV_BB bb)
{
  MAV_clipPlanes clip_planes;
  int n, corner_list[6];

  /* calculate the clip planes of the full frustum of the current window */
  clip_planes= mav_clipPlanesGet(mav_win_current, -1.0, 1.0, -1.0, 1.0, mav_win_current->ncp/mav_win_current->fcp, 1.0);

  /* calculate the corner list we will need to check against */
  for (n=0; n<6; n++) corner_list[n]= mav_BBGetCorner(clip_planes.planes[n].norm);

  /* perform the cull and return 0 if outside, 1 if inside, 2 if intersects */
  return mav_BBIntersectsClipPlanes(bb, corner_list, &clip_planes);
}



/* As above but given a set of clip planes */

int mav_BBCullToClipPlanes(MAV_BB bb, MAV_clipPlanes cp)
{
  int n, corner_list[6];

  /* calculate the corner list we will need to check against */
  for (n=0; n<6; n++) corner_list[n]= mav_BBGetCorner(cp.planes[n].norm);

  /* perform the cull and return 0 if outside, 1 if inside, 2 if intersects */
  return mav_BBIntersectsClipPlanes(bb, corner_list, &cp);
}



/* Routines to print a clip plane(s) */

void mav_clipPlanePrint(char *s, MAV_clipPlane cp)
{
  printf("%s", s);
  mav_vectorPrint("norm ", cp.norm);
  printf("%f\n", cp.d);
}

void mav_clipPlanesPrint(char *s, MAV_clipPlanes cp)
{
  int i;

  printf("%s", s);
  printf("num %i\n", cp.num);
  for (i=0; i<cp.num; i++) {
    printf("plane %i\n", i);
    mav_clipPlanePrint("", cp.planes[i]);
  }
}
