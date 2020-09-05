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

/* eg12.c */
#include "maverik.h"
#include <math.h>
#include <stdlib.h>

/* The data structure and object class to represent the dodecahedron */

typedef struct {
  float r;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
} MAV_dodec;

MAV_class *mav_class_dodec;

/* The vertices of a unit sized dodecahedron */

#define V1 0.381966
#define V2 0.618034

MAV_vector vecs[]={{-V1,0,1},{V1,0,1},{-V2,-V2,-V2},{-V2,-V2,V2},{-V2,V2,-V2},{-V2,V2,V2},
 {V2,-V2,-V2},{V2,-V2,V2},{V2,V2,-V2},{V2,V2,V2},{1,V1,0},{1,-V1,0},{-1,V1,0},{-1,-V1,0},
 {-V1,0,-1},{V1,0,-1},{0,1,V1},{0,1,-V1},{0,-1,V1},{0,-1,-V1}};

/* Routine to render a pentagon given the vertices and size r */

void pentagon(int a, int b, int c, int d, int e, float r)
{
  MAV_vector v1, v2, norm;

  /* Calculate normal of pentagon from crossproduct of the 2 edges */
  v1= mav_vectorSub(vecs[a], vecs[b]);
  v2= mav_vectorSub(vecs[b], vecs[c]);  
  norm= mav_vectorNormalize(mav_vectorCrossProduct(v1, v2));
  
  /* Render the pentagon as a polygon. Vecs contain unit pentagon, so mult by r */
  mav_gfxPolygonBegin();
  mav_gfxNormal(norm);
  mav_gfxVertex(mav_vectorScalar(vecs[a], r));
  mav_gfxVertex(mav_vectorScalar(vecs[b], r));
  mav_gfxVertex(mav_vectorScalar(vecs[c], r));
  mav_gfxVertex(mav_vectorScalar(vecs[d], r));
  mav_gfxVertex(mav_vectorScalar(vecs[e], r));
  mav_gfxPolygonEnd();
}



/* Routine to render the dodecahedron */

int mav_dodecDraw(MAV_object *o, MAV_drawInfo *di)
{
  MAV_dodec *dodec;

  /* Convert from generic Maverik object to the dodecahedron object */
  dodec= (MAV_dodec *) mav_objectDataGet(o);

  /* Set the correct colouring */
  mav_surfaceParamsUse(dodec->sp);

  /* Store the current transformation matrix - then multiply it by the local transformation */
  mav_gfxMatrixPush();
  mav_gfxMatrixMult(dodec->matrix);

  /* Render the 12 pentagons that make up the dodecahedron */
  pentagon(0, 1, 9, 16, 5, dodec->r);
  pentagon(1, 0, 3, 18, 7, dodec->r);
  pentagon(1, 7, 11, 10, 9, dodec->r);
  pentagon(11, 7, 18, 19, 6, dodec->r);
  pentagon(8, 17, 16, 9, 10, dodec->r);
  pentagon(2, 14, 15, 6, 19, dodec->r);
  pentagon(2, 13, 12, 4, 14, dodec->r);
  pentagon(2, 19, 18, 3, 13, dodec->r);
  pentagon(3, 0, 5, 12, 13, dodec->r);
  pentagon(6, 15, 8, 10, 11, dodec->r);
  pentagon(4, 17, 8, 15, 14, dodec->r);
  pentagon(4, 12, 5, 16, 17, dodec->r);

  /* Restore original transformation matrix */
  mav_gfxMatrixPop();
  
  return MAV_TRUE;
}



/* Routine to calculate the BB of the dodecahedron - quick but overestimates */

int mav_dodecBB(MAV_object *o, MAV_BB *bb)
{
  MAV_dodec *dodec;
  MAV_BB local;

  /* Convert from generic Maverik object to the dodecahedron object */
  dodec= (MAV_dodec *) mav_objectDataGet(o);

  /* Local coordinate frame axis aligned bounding box */
  local.min.x= -dodec->r;
  local.min.y= -dodec->r;
  local.min.z= -dodec->r;
  local.max.x= dodec->r;
  local.max.y= dodec->r;
  local.max.z= dodec->r;

  /* Align local coordinate frame with the parent (in this case the world) frame */
  mav_BBAlign(local, dodec->matrix, bb);

  return MAV_TRUE;
}



/* Routine to calculate the BB of the dodecahedron - slower but more accurate */

int mav_dodecBB2(MAV_object *o, MAV_BB *bb)
{
  MAV_dodec *dodec;
  int i;

  /* Convert from generic Maverik object to the dodecahedron object */
  dodec= (MAV_dodec *) mav_objectDataGet(o);

  /* Find BB enclosed by the points after size and position of dodec have been accounted for */
  mav_BBCompInit(bb);
  for (i=0; i<20; i++) {
    mav_BBCompPt(mav_vectorMult(mav_vectorScalar(vecs[i], dodec->r), dodec->matrix), bb);
  }

  return MAV_TRUE;
}



/* Routine to calculate the object-line intersection of a pentagon */

void pentagonIntersect(MAV_line ln, MAV_objectIntersection *oi, int a, int b, int c, int d, int e, float r)
{
  MAV_polygon apoly;
  MAV_vector v1, v2, norm;

  /* Calculate normal of pentagon from crossproduct of the 2 edges */
  v1= mav_vectorSub(vecs[a], vecs[b]);
  v2= mav_vectorSub(vecs[b], vecs[c]);  
  norm= mav_vectorNormalize(mav_vectorCrossProduct(v1, v2));

  /* Make up a MAV_polygon to represent the pentagon */
  apoly.np= 5;
  apoly.norm= norm;
  apoly.vert= mav_malloc(apoly.np*sizeof(MAV_vector));
  apoly.vert[0]= mav_vectorScalar(vecs[a], r);
  apoly.vert[1]= mav_vectorScalar(vecs[b], r);
  apoly.vert[2]= mav_vectorScalar(vecs[c], r);
  apoly.vert[3]= mav_vectorScalar(vecs[d], r);
  apoly.vert[4]= mav_vectorScalar(vecs[e], r);
  apoly.matrix= MAV_ID_MATRIX;

  /* Calculate line-polygon intersection */
  mav_linePolygonIntersection(&apoly, ln, oi);

  /* Free up polygon vertex memory */
  mav_free(apoly.vert);
}



/* Routine to calculate the object-line intersection of the dodecahedron */

int mav_dodecIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi)
{
  MAV_dodec *dodec;
  MAV_objectIntersection pentInt[12];
  MAV_line ln2;

  /* Convert from generic Maverik object to the dodecahedron object */
  dodec= (MAV_dodec *) mav_objectDataGet(o);

  /* Initialise object intersection data structure */
  oi->pt1=-100.0;

  /* Rotate and translate line so that the dodecahedron is centered and axis aligned */
  ln2= mav_lineTransFrame(*ln, dodec->matrix);

  /* Intersect the 12 pentagons that make up the dodecahedron */
  pentagonIntersect(ln2, &pentInt[0], 0, 1, 9, 16, 5, dodec->r);
  pentagonIntersect(ln2, &pentInt[1], 1, 0, 3, 18, 7, dodec->r);
  pentagonIntersect(ln2, &pentInt[2], 1, 7, 11, 10, 9, dodec->r);
  pentagonIntersect(ln2, &pentInt[3], 11, 7, 18, 19, 6, dodec->r);
  pentagonIntersect(ln2, &pentInt[4], 8, 17, 16, 9, 10, dodec->r);
  pentagonIntersect(ln2, &pentInt[5], 2, 14, 15, 6, 19, dodec->r);
  pentagonIntersect(ln2, &pentInt[6], 2, 13, 12, 4, 14, dodec->r);
  pentagonIntersect(ln2, &pentInt[7], 2, 19, 18, 3, 13, dodec->r);
  pentagonIntersect(ln2, &pentInt[8], 3, 0, 5, 12, 13, dodec->r);
  pentagonIntersect(ln2, &pentInt[9], 6, 15, 8, 10, 11, dodec->r);
  pentagonIntersect(ln2, &pentInt[10], 4, 17, 8, 15, 14, dodec->r);
  pentagonIntersect(ln2, &pentInt[11], 4, 12, 5, 16, 17, dodec->r);

  /* Sort intersection and return appropriate value */
  return (mav_objectIntersectionsSort(12, pentInt, mav_matrixScaleGet(dodec->matrix), oi));
}



/* Define a dodecahedron */

void defDodec(MAV_dodec *d)
{
  d->r=2.5;
  d->sp= mav_sp_default;
  d->matrix= MAV_ID_MATRIX;
}



/* Render a frame */

void drawFrame(MAV_SMS *sms, MAV_BB bb)
{
  /* Check for and act on any events */
  mav_eventsCheck();
    
  /* Request start of a new frame */
  mav_frameBegin();

  /* Display the BB */
  mav_BBDisplay(mav_win_current, bb);
  
  /* Display the SMS in all windows */
  mav_SMSDisplay(mav_win_all, sms);

  /* Request end of the frame */
  mav_frameEnd();
}



/* Keyboard event callback */

int keyEvent(MAV_object *o, MAV_keyboardEvent *ke)
{
  MAV_callbackBBFn fn;

  if (ke->movement==MAV_PRESSED) {
    if (ke->key=='q') exit(1); /* Quit */

    if (ke->key=='b') { /* Toggle calc BB callback function */
      fn= (MAV_callbackBBFn) mav_callbackQuery(mav_callback_BB, mav_win_all, o);
      if (fn==mav_dodecBB)
      {
	mav_callbackBBSet(mav_win_all, mav_class_dodec, mav_dodecBB2);
      }
      else
      {
	mav_callbackBBSet(mav_win_all, mav_class_dodec, mav_dodecBB);
      }
    }

    if (ke->key=='s') { /* Increase size of dodecahedron */
      MAV_dodec *dodec= (MAV_dodec *) mav_objectDataGet(o);
      dodec->r*=1.1;
    }
  }

  return 1;
}



int main(int argc, char *argv[])
{
  MAV_dodec dodec;
  MAV_object *obj;
  MAV_SMS *sms;
  float r=0;

  /* Initialise the Maverik system */
  mav_initialise(&argc, argv);

  /* Create a new class to represent the dodecahedron */
  mav_class_dodec= mav_classNew();

  /* Set the draw callback for this new class */
  mav_callbackDrawSet(mav_win_all, mav_class_dodec, mav_dodecDraw);

  /* Set the calculate BB callback for this new class */
  mav_callbackBBSet(mav_win_all, mav_class_dodec, mav_dodecBB);

  /* Set the calculate object-line intersection callback for this new class */
  mav_callbackIntersectSet(mav_win_all, mav_class_dodec, mav_dodecIntersect);

  /* Define a dodecahedron */
  defDodec(&dodec);

  /* Register the dodecahedron as a Maverik object */
  obj= mav_objectNew(mav_class_dodec, &dodec);

  /* Create a SMS */
  sms= mav_SMSObjListNew();

  /* Add object to SMS */
  mav_SMSObjectAdd(sms, obj);

  /* Use default mouse navigation */
  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

  /* Define keyboard event callback */
  mav_callbackKeyboardSet(mav_win_all, mav_class_dodec, keyEvent);

  /* Rendering loop */
  while (1) {
    MAV_BB bb;

    /* Spin the dodecahedron */
    r+=1;
    dodec.matrix= mav_matrixSet(r,r*2,r/2, 0,0,0);

    /* Execute the calculate BB callback for the dodecahedron */
    mav_callbackBBExec(mav_win_current, mav_objectDataWith(&dodec), &bb);

    /* Draw a frame */
    drawFrame(sms, bb);
  }
}
