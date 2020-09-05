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
#include "mavlib_avatar.h"
#include <stdio.h>
#include <math.h>

MAV_class *mav_class_avatar;
MAV_list *mavlib_avatarList=NULL;



void mavlib_avatarUpdate(MAV_avatar *a)
{
  MAV_vector pos, vec;
  float dist;

/* Calculate elapsed time since last call */
  
  mav_timerStop(&a->timer);
  if (a->last_time<0) a->last_time=a->timer.wall;
  a->time= (a->timer.wall-a->last_time);
  a->last_time= a->timer.wall;

/* Animate and move as needed */

  if (a->animFromMat) 
  {    
    pos= mav_matrixXYZGet(a->matrix);
    vec= mav_vectorSub(pos, a->last_pos);
    a->last_pos= pos;
    
    dist= mav_vectorMag(vec);
    if (a->time) 
    {
      a->speed= dist/a->time;
    }
    else
    {
      a->speed= 0.0;
    }

    mav_avatarAnimate(a);
  }
  else
  {
    if (a->move) mav_avatarMove(a);
    if (a->animate) mav_avatarAnimate(a);
  }
}

void mavlib_avatarUpdateFn(void *ignored)
{
  MAV_avatar *a;

  /* Update all avatars positions as appropriate */
  mav_listPointerReset(mavlib_avatarList);
  while (mav_listItemNext(mavlib_avatarList, (void **) &a)) {
    mavlib_avatarUpdate(a);
  }
}



/* Routine to render an avatar part */

void mav_avatarPartDraw(MAV_avatar *avatar, MAV_avatarPart *part, MAV_drawInfo *di)
{
  MAV_avatarPartPtr *p;

  /* If the avatar is pointing to something with an arm,
     don't draw the walking version of the arm now */
  if ((avatar->holding_left && part->num==LEFT_UPPER_ARM) ||
      (avatar->holding_right &&  part->num==RIGHT_UPPER_ARM))
  {
     return;
  };

/* Store the current transformation matrix - then multiply it by the local transformations */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(part->position);
  mav_gfxMatrixMult(part->rotation);

/* Draw the part */

  mav_callbackDrawExec(mav_win_current, part->obj, di);

/* And its childern */

      p= part->children;
      while (p) {
        mav_avatarPartDraw(avatar, p->part, di);
        p= p->next;
      }

/* Restore original transformation matrix */

  mav_gfxMatrixPop();
}

/* Routine to draw arms with hands at given position */

void mav_avatarDrawHoldingArms(MAV_avatar *avatar, 
			       int clavicle,
			       int upper_arm, 
                               int lower_arm, 
                               int hand,
			       MAV_vector handp, 
                               MAV_drawInfo *di, 
                               MAV_object *obj)
{
    MAV_matrix shoulder, armPart, spare;
    MAV_vector p1,p2,p3, p3p1, p3p2, p1p2, p1p3, p1n; 
    MAV_BB lenBB, handBB;
    float d1, d2, a, c, s;
    float theta1, theta2, theta3;
    float angle,extra_reach;
    int laser= 0, behind= 0;
    MAV_vector handpos;

    /* convert the hand position from world co-ords to local co-ords */
    handpos= mav_vectorMult(handp, mav_matrixInverse(avatar->matrix));
    handpos= mav_vectorMult(handpos, mav_matrixInverse(avatar->vertical));
    handpos= mav_vectorMult(handpos, mav_matrixInverse(avatar->rotation));

    /*  get position of shoulder joint in body relative co-ordinates */

    shoulder= MAV_ID_MATRIX; 
    shoulder= mav_matrixMult(shoulder,avatar->part[HIPS]->position);
    shoulder= mav_matrixMult(shoulder,avatar->part[HIPS]->rotation);

    shoulder= mav_matrixMult(shoulder,avatar->part[LOWER_TORSO]->position);
    shoulder= mav_matrixMult(shoulder,avatar->part[LOWER_TORSO]->rotation);

    shoulder= mav_matrixMult(shoulder,avatar->part[UPPER_TORSO]->position);
    shoulder= mav_matrixMult(shoulder,avatar->part[UPPER_TORSO]->rotation);

    shoulder= mav_matrixMult(shoulder,avatar->part[clavicle]->position);
    shoulder= mav_matrixMult(shoulder,avatar->part[clavicle]->rotation);

    shoulder= mav_matrixMult(shoulder,avatar->part[upper_arm]->position);

    /* p1 is the shoulder */
    p1.x= shoulder.mat[MAV_MATRIX_XCOMP];
    p1.y= shoulder.mat[MAV_MATRIX_YCOMP];
    p1.z= shoulder.mat[MAV_MATRIX_ZCOMP];

    /* calculate angle of yaw for the arm, and the extra reach necessary
       to compensate for this */

    angle= -atan((p1.x-(handpos.x))/(p1.z-handpos.z));
    extra_reach= ((p1.z-handpos.z)/cos(angle))-(p1.z-handpos.z);

    mav_callbackBBExec(mav_win_all, avatar->part[hand]->obj, &handBB);

    /* p2 is the position that the hand needs to reach */
    p2.x= 0;
    p2.y= handpos.y;
    p2.z= handpos.z-extra_reach-(handBB.max.z-handBB.min.z);

    /* if the hand is behind the body, put it in line with the body */ 
    if (p2.z<0)  
    {
        p2.z= 0;
        behind= 1;
    }

    /* distance from shoulder to elbow */ 
    mav_callbackBBExec(mav_win_all, avatar->part[upper_arm]->obj, &lenBB);
    d1= lenBB.max.y-lenBB.min.y;

    /* distance from elbow to wrist */
    mav_callbackBBExec(mav_win_all, avatar->part[lower_arm]->obj, &lenBB);
    d2= lenBB.max.y-lenBB.min.y;

    /* distance from shoulder to wrist */
    a= sqrt( ((p2.z-p1.z)*(p2.z-p1.z)) + ((p2.y-p1.y)*(p2.y-p1.y)) );

    c= ( ((a*a) + (d1*d1) - (d2*d2)) / (2*d1*a) );
    s= (1-(c*c));

    /* p3 is the position of the elbow */
    p3.z= (p1.z+ (d1*c*(p2.z-p1.z)/a)+(d1*s*(p2.y-p1.y)/a));
    p3.y= (p1.y+ (d1*c*(p2.y-p1.y)/a)-(d1*s*(p2.z-p1.z)/a));
    p3.x= 0;

    /* if avatar is pointing beyond length of arm */
    if (a>(d1+d2))
    {
	MAV_vector v;
	v=mav_vectorNormalize(mav_vectorSub(p2,p1));
        p2= mav_vectorAdd(p1, mav_vectorScalar(v, d1+d2));

        a= d1+d2;
        c= ( ((a*a) + (d1*d1) - (d2*d2)) / (2*d1*a) );
        s= (1-(c*c));

        p3.z= (p1.z+ (d1*c*(p2.z-p1.z)/a)+(d1*s*(p2.y-p1.y)/a));
        p3.y= (p1.y+ (d1*c*(p2.y-p1.y)/a)-(d1*s*(p2.z-p1.z)/a));
        p3.x= 0;

	laser= 1;
    }

    p1.x=0;

    p3p1= mav_vectorNormalize(mav_vectorSub(p1,p3));
    p3p2= mav_vectorNormalize(mav_vectorSub(p2,p3));
    theta2= MAV_RAD2DEG(acos(mav_vectorDotProduct(p3p1,p3p2)));

    p1p2= mav_vectorNormalize(mav_vectorSub(p2,p1));
    p1p3= mav_vectorNormalize(mav_vectorSub(p3,p1));
    theta1= MAV_RAD2DEG(acos(mav_vectorDotProduct(p1p2,p1p3)));

    p1n= mav_vectorSet(0,1,0);
    theta3= MAV_RAD2DEG(acos(mav_vectorDotProduct(p1n,p1p2)));

    p1.x= shoulder.mat[MAV_MATRIX_XCOMP];
    armPart= MAV_ID_MATRIX;
    spare= mav_matrixXYZSet(MAV_ID_MATRIX, p1);
    armPart= mav_matrixMult(armPart, spare);
    spare= mav_matrixRPYSet(MAV_ID_MATRIX, 0,0,MAV_RAD2DEG(-angle));
    armPart= mav_matrixMult(armPart, spare);
    spare= mav_matrixRPYSet(MAV_ID_MATRIX,0,-(180.0-(theta3+theta1)),0);
    armPart= mav_matrixMult(armPart, spare);

    mav_gfxMatrixPush();
    mav_gfxMatrixMult(armPart);
    mav_callbackDrawExec(mav_win_current, avatar->part[upper_arm]->obj, di);    
    spare= mav_matrixXYZSet(MAV_ID_MATRIX, mav_vectorSet(0,-d1,0)); 
    mav_gfxMatrixMult(spare);
    spare= mav_matrixRPYSet(MAV_ID_MATRIX, 0,-(180.0-theta2),0);
    mav_gfxMatrixMult(spare);
    mav_callbackDrawExec(mav_win_current, avatar->part[lower_arm]->obj, di);
    mav_gfxMatrixMult(avatar->part[hand]->position);

    mav_gfxMatrixMult(avatar->part[hand]->rotation);
    mav_callbackDrawExec(mav_win_current, avatar->part[hand]->obj, di);
    mav_gfxMatrixPop();

    /* if pointing beyond reach and in front, draw a laser-pointer */
    if (laser && !behind)
    {
        mav_surfaceParamsUse(avatar->laser_sp);
        mav_gfxLineBegin();

        /* Start a point at the shoulder */
        p1.x= shoulder.mat[MAV_MATRIX_XCOMP];
        p1.y= shoulder.mat[MAV_MATRIX_YCOMP];
        p1.z= shoulder.mat[MAV_MATRIX_ZCOMP];

        /* Shift it along the vector from shoulder to arm */
        p1= mav_vectorScalar(mav_vectorNormalize(mav_vectorSub(handpos, p1)),d1+d2);
        p1.x+= shoulder.mat[MAV_MATRIX_XCOMP];
        p1.y+= shoulder.mat[MAV_MATRIX_YCOMP];
        p1.z+= shoulder.mat[MAV_MATRIX_ZCOMP];
        mav_gfxVertex(p1);

        mav_gfxVertex(handpos);

        mav_gfxLineEnd();
    }
}



/* Routine to render an avatar */

int mav_avatarDraw(MAV_object *obj, MAV_drawInfo *di)
{
  MAV_avatar *a= (MAV_avatar *) mav_objectDataGet(obj);

/* Store the current transformation matrix - then multiply it by the local transformations */

  mav_gfxMatrixPush();
  mav_gfxMatrixMult(a->matrix);
  mav_gfxMatrixMult(a->vertical);
  mav_gfxMatrixMult(a->rotation);

/* Draw the root avatar part (the hips - the rest follows) */

  mav_avatarPartDraw(a, a->root, di);
  
  if (a->holding_left)
  {
     mav_avatarDrawHoldingArms(a, LEFT_CLAVICLE, LEFT_UPPER_ARM,
				  LEFT_LOWER_ARM, LEFT_HAND, a->left_hand, di, obj);
  }

  if (a->holding_right)
  {
     mav_avatarDrawHoldingArms(a, RIGHT_CLAVICLE, RIGHT_UPPER_ARM,
				  RIGHT_LOWER_ARM, RIGHT_HAND, a->right_hand, di, obj);
  }

/* Restore original transformation matrix */

  mav_gfxMatrixPop();

  return 1;
}



/* Routine to calculate the bounding box of an avatar (quick but overestimates) */

int mav_avatarBB(MAV_object *obj, MAV_BB *bb)
{
  MAV_avatar *a= (MAV_avatar *) mav_objectDataGet(obj);
  MAV_BB tmp;

/* Local coordinate frame BB */

  tmp.min.x= -0.3;
  tmp.max.x= 0.3;

  tmp.min.y= -1.7;
  tmp.max.y= 0.1;

  tmp.min.z= -1.0;
  tmp.max.z= 1.0;

/* Global axis align it */ 

  mav_BBAlign(tmp, a->matrix, bb);

/* If the arms are active, include the endpoint in the BB */
  if (a->holding_left) mav_BBCompPt(a->left_hand, bb);
  if (a->holding_right) mav_BBCompPt(a->right_hand, bb);


  return 1;
}



/* Routine to calculate the bounding box of an avatar */

void mav_avatarPartBBox(MAV_avatarPart *part, MAV_matrix mat, MAV_BB *bb)
{
  MAV_avatarPartPtr *p;
  MAV_BB objbb;
  MAV_BB bbox;

  /* account for parts transformation */
  mat= mav_matrixMult(mat, part->position);
  mat= mav_matrixMult(mat, part->rotation);

  /* calculate composite BB of children */
  p= part->children;
  while (p) {
    mav_avatarPartBBox(p->part, mat, bb);
    p= p->next;
  }

  /* calculate composite BB of part */
  if (mav_callbackBBExec(mav_win_current, part->obj, &objbb)) {
    mav_BBAlign(objbb, mat, &bbox);
    mav_BBCompBB(bbox, bb);
  }
}



/* Routine to calculate the bounding box of an avatar (slow but accurate) */

int mav_avatarBB2(MAV_object *obj, MAV_BB *bb)
{
  MAV_avatar *a= (MAV_avatar *) mav_objectDataGet(obj);
  MAV_matrix mat;
  MAV_BB bbox;

  /* Calculate pre-multiplying matrix of avatar */
  mat= a->vertical;
  mat= mav_matrixMult(mat, a->rotation);

  /* Calculate BB of avatar parts */
  mav_BBCompInit(&bbox);
  mav_avatarPartBBox(a->root, mat, &bbox);

  /* Global axis align it */
  mav_BBAlign(bbox, a->matrix, bb);

/* if the arms are active then include the endpoint in the bb */
  if (a->holding_left) mav_BBCompPt(a->left_hand, bb);
  if (a->holding_right) mav_BBCompPt(a->right_hand, bb);

  return 1;
}



/* Routine to intersect an avatar part */

int mav_avatarPartIntersect(MAV_avatarPart *part, MAV_line ln, MAV_objectIntersection *oi)
{
  MAV_avatarPartPtr *p;
  MAV_matrix mat;
  MAV_line ln2;
  MAV_objectIntersection oi2;
  int rv=0;

  oi2.pt1=-100;
  oi2.pt2=-100;

  /* Rotate and translate line so that the avatar part is centered and axis aligned */
  mat= part->position;
  mat= mav_matrixMult(mat, part->rotation);
  ln2= mav_lineTransFrame(ln, mat);

  /* calculate intersection with children */
  p= part->children;
  while (p) {
    rv|=mav_avatarPartIntersect(p->part, ln2, oi);
    p= p->next;
  }

  /* calculate intersection of part */
  if (mav_callbackIntersectExec(mav_win_current, part->obj, ln2, &oi2)) {
    if (oi2.pt1<oi->pt1) *oi=oi2;
    rv=1;
  }

  return rv;
}



/* Routine to intersect an avatar */

int mav_avatarIntersect(MAV_object *obj, MAV_line *ln, MAV_objectIntersection *oi)
{
  MAV_avatar *a= (MAV_avatar *) mav_objectDataGet(obj);
  MAV_line ln2;
  MAV_matrix mat;
  int rv;

  oi->pt1=MAV_INFINITY;

/* Rotate and translate line so that the avatar is centered and axis aligned */

  mat= a->matrix;
  mat= mav_matrixMult(mat, a->vertical);
  mat= mav_matrixMult(mat, a->rotation);
  ln2= mav_lineTransFrame(*ln, mat);

  if (mav_avatarPartIntersect(a->root, ln2, oi))
  {
    oi->pt1*= mav_matrixScaleGet(mat);
    oi->pt2*= mav_matrixScaleGet(mat);
    rv=1;
  }
  else
  {
    oi->pt1=-100.0;
    oi->pt2=-100.0;
    rv=0;
  }

  return rv;
}



/* Routine to identify an avatar */

int mav_avatarID(MAV_object *o, char **id)
{
  *id= "avatar";

  return 1;
}



/* Routine to return the userdef field of an avatar */

int mav_avatarGetUserdef(MAV_object *o, void ***ud)
{
  MAV_avatar *a= (MAV_avatar *) mav_objectDataGet(o);

  *ud= &a->userdef;

  return 1;
}



/* Routine to return the matrix field of an avatar */

int mav_avatarGetMatrix(MAV_object *o, MAV_matrix **m)
{
  MAV_avatar *a= (MAV_avatar *) mav_objectDataGet(o);

  *m= &a->matrix;

  return 1;
}



/* Routine to return the surface params field of an avatar */

int mav_avatarGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp)
{
  MAV_avatar *a= (MAV_avatar *) mav_objectDataGet(o);

  *sp= a->sp;

  return 1;
}



/* Routine to dump an avatar */

int mav_avatarDump(MAV_object *o)
{
  MAV_avatar *a= (MAV_avatar *) mav_objectDataGet(o);

  printf("*** Dumping object %p - a MAV_avatar with data pointer %p\n", o, mav_objectDataGet(o));
  printf("movement %i\n", a->movement);
  printf("speed %f\n", a->speed);
  printf("offset %f\n", a->offset);
  printf("animate %i\n", a->animate);
  printf("move %i\n", a->move);
  if (a->sp[0]) mav_surfaceParamsPrint("surface params (skin) ", *a->sp[0]);
  if (a->sp[1]) mav_surfaceParamsPrint("surface params (hair) ", *a->sp[1]);
  if (a->sp[2]) mav_surfaceParamsPrint("surface params (face) ", *a->sp[2]);
  if (a->sp[3]) mav_surfaceParamsPrint("surface params (jumper) ", *a->sp[3]);
  if (a->sp[4]) mav_surfaceParamsPrint("surface params (trousers) ", *a->sp[4]);
  mav_surfaceParamsPrint("surface params (laser) ", *a->laser_sp);
  mav_matrixPrint("matrix\n", a->matrix);
  mav_vectorPrint("left hand\n", a->left_hand);
  mav_vectorPrint("right hand\n", a->right_hand);
  printf("userdef %p\n", a->userdef);

  return 1;
}
