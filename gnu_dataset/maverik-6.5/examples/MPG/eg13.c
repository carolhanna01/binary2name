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

/* eg13.c */
#include "maverik.h"
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

MAV_surfaceParams *sp[4];
MAV_matrix *objMat1, *objMat2;
float objDist;
int fc=0;

/* Define a rectangle */
void defRect(MAV_rectangle *r)
{
  r->width= 500.0; /* Size */
  r->height= 500.0;
  r->xtile= 3; /* Texture repeat tiling */
  r->ytile= 3;
  r->matrix= mav_matrixSet(0,-90,0, 0,-2,0); /* Orientation (RPY 0,-90,0) and position (XYZ 0,-2,0) */
  r->sp= mav_surfaceParamsNew(MAV_TEXTURE, 0, 0, 5); /* Use decal texture with index 5 */
}

/* Define a box */
void defBox(MAV_box *b)
{
  /* Random box size, position/orientation and set of surface params */
  b->size.x= mav_random()*30;
  b->size.y= mav_random()*30;
  b->size.z= mav_random()*30;
  b->matrix= mav_matrixSet(0,0,mav_random()*360, -200+mav_random()*400,0,-200+mav_random()*400);
  b->sp= sp[(int) (mav_random()*4)];
}

/* Define a cylinder */
void defCyl(MAV_cylinder *c)
{
  /* Random cylinder size, position/orientation and set of surface params */
  c->radius= mav_random()*20;
  c->height= mav_random()*20;
  c->endcap= 1;
  c->nverts= 10;
  c->matrix= mav_matrixSet(0,mav_random()*360,0, -200+mav_random()*400,0,-200+mav_random()*400);
  c->sp= sp[(int) (mav_random()*4)];
}

/* Define a composite object */
void defComp(MAV_composite *c)
{
  /* Read AC3D object from file */
  if (!mav_compositeReadAC3D("mavlogo.ac", c, MAV_ID_MATRIX)) {
    printf("failed to read mavlogo.ac\n");
    exit(1);
  }

  c->matrix= mav_matrixSet(0,0,0, 0,0.2,-15); /* Fixed position and orientation */
}

/* Render a frame */
void drawFrame(MAV_SMS *sms1, MAV_SMS *sms2)
{
  /* Check for and act on any events */
  mav_eventsCheck();
    
  /* Request start of a new frame */
  mav_frameBegin();
    
  /* Display the two SMS in all windows */
  mav_SMSDisplay(mav_win_all, sms1);
  mav_SMSDisplay(mav_win_all, sms2);

  /* Request end of the frame */
  mav_frameEnd();
}

/* Mouse event for cylinders */
int cylEvent(MAV_object *obj, MAV_mouseEvent *ev)
{
  MAV_cylinder *cyl;

  /* Convert from generic Maverik object to a cylinder object */
  cyl= (MAV_cylinder *) mav_objectDataGet(obj);

  if (ev->movement==MAV_PRESSED) { /* Only consider button presses */
    cyl->radius+=1; /* Increase cylinder radius */    
  }

  return 1;
}

/* Mouse event for composites */
int compEvent(MAV_object *obj, MAV_mouseEvent *ev)
{
  if (ev->movement==MAV_PRESSED) {
    MAV_composite *comp= (MAV_composite *) mav_objectDataGet(obj);
    comp->matrix= mav_matrixScaleSet(comp->matrix, 1.1); /* Scale composite by a factor of 1.1 */
  }

  return 1;
}

/* Display a help message */
void helpMsg(void *ignored)
{
 mav_stringDisplay(mav_win_all, "Left mouse button - navigate forward/backward and yaw", MAV_COLOUR_BLACK, 0, -0.95, 0.90);
  mav_stringDisplay(mav_win_all, "Right mouse button - navigate up/down and left/right", MAV_COLOUR_BLACK, 0, -0.95, 0.83);
  mav_stringDisplay(mav_win_all, "Middle mouse click on cylinder - increase radius", MAV_COLOUR_BLACK, 0, -0.95, 0.76);
  mav_stringDisplay(mav_win_all, "Middle mouse click on composite (Maverik logo) - increase scale", MAV_COLOUR_BLACK, 0, -0.95, 0.69);
  mav_stringDisplay(mav_win_all, "h - help", MAV_COLOUR_BLACK, 0, -0.95, 0.60);
  mav_stringDisplay(mav_win_all, "q - quit", MAV_COLOUR_BLACK, 0, -0.95, 0.53);
  mav_stringDisplay(mav_win_all, "d - delete object", MAV_COLOUR_BLACK, 0, -0.95, 0.46);
  mav_stringDisplay(mav_win_all, "b - change size of a box", MAV_COLOUR_BLACK, 0, -0.95, 0.39);
  mav_stringDisplay(mav_win_all, "c - change colour of an object", MAV_COLOUR_BLACK, 0, -0.95, 0.32);
  mav_stringDisplay(mav_win_all, "j - make object jump", MAV_COLOUR_BLACK, 0, -0.95, 0.25);
  mav_stringDisplay(mav_win_all, "p - pick an object", MAV_COLOUR_BLACK, 0, -0.95, 0.18);
}

/* Routine to make object jump */
void jump(void *ignored)
{
  /* Increase Y component of matrix by an ammount which ranges +4 to -4 over 60 interactions */
  objMat1->mat[MAV_MATRIX_YCOMP]+=cos(MAV_DEG2RAD(fc*3.0))*4.0;

  /* Stop executing this function after 60 frames */
  fc++;
  if (fc>60) {
    fc=0;
    mav_frameFn1Rmv(jump, NULL);
  }
}

/* Routine to drag object with mouse */
void pick(void *ignored)
{
  MAV_vector pos;

  /* Calculate the position of a point a distance objDist away from the eye along */
  /* the normalized vector defined by the eye point and the mouse's projection */
  /* onto the near clip plane (this is mav_mouse_dir) */
  pos= mav_vectorAdd(mav_win_current->vp->eye, mav_vectorScalar(mav_mouse_dir, objDist));

  /* Set the objects matrix to this position */
  *objMat2= mav_matrixXYZSet(*objMat2, pos);
}

/* Keyboard event */
int keyEvent(MAV_object *obj, MAV_keyboardEvent *ke)
{
  MAV_surfaceParams **spptr;

  switch (ke->key) {
  case 'q': /* Quit */
    exit(1);
    break;
    
  case 'h': /* Help */    
    if (ke->movement==MAV_PRESSED)
    {
      mav_frameFn3Add(helpMsg, NULL); /* Begin executing function helpMsg at the end of each frame */
    }
    else
    {
      mav_frameFn3Rmv(helpMsg, NULL); /* Stop executing function helpMsg at the end of each frame */
    }
    break;
  }

  if (ke->intersects) { /* Only consider event if the mouse was pointing at an object */
    if (ke->movement==MAV_PRESSED) { /* Only consider button press event */ 
      switch (ke->key) {
      case 'd':  /* Delete an object */
        mav_objectDelete(ke->obj);
        break;

      case 'b': /* Increase size of box */
        if (mav_objectClassGet(ke->obj)==mav_class_box) /* Ensure object is a box */
        {
          MAV_box *box= (MAV_box *) mav_objectDataGet(ke->obj); /* Convert from generic Maverik object to a box object */
          box->size.x+=0.5; /* Increase size of box */
        }
        else
        {
          printf("Object is not a box\n");
        }
        break;
 
      case 'c': /* Change colour */
        if (mav_callbackGetSurfaceParamsExec(mav_win_current, ke->obj, &spptr)) { /* Get a ptr to the surfaceParmas field of the object */
	  *spptr= sp[(int) (mav_random()*4)]; /* Set it to some random value */
	}
        break;

      case 'j': /* Make object jump */
        if (fc==0) { /* Only if something is not currently in flight */
          if (mav_callbackGetMatrixExec(mav_win_current, ke->obj, &objMat1)) { /* Get a ptr to the matrix field of the object */
	    mav_frameFn1Add(jump, NULL); /* Begin executing function jump at the start of each frame */
	  }
	}
        break;
      }
    }

    switch (ke->key) {
    case 'p': /* Pick object */
      if (ke->movement==MAV_PRESSED) 
      {
        if (mav_callbackGetMatrixExec(mav_win_current, ke->obj, &objMat2)) { /* Get a ptr to the matrix field of the object */
	  objDist= ke->objint.pt1; /* Remember distance from eye to object intersection */
	  mav_frameFn2Add(pick, NULL); /* Begin executing function pick after the view has been set */
	}
      }
      else
      {
        mav_frameFn2Rmv(pick, NULL); /* Stop executing function pick after the view has been set */
      }
      break;
    }
  }

  return 1;
}



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
  MAV_texCoord t;
  float ang=0;

  /* Calculate normal of pentagon from crossproduct of the 2 edges */
  v1= mav_vectorSub(vecs[a], vecs[b]);
  v2= mav_vectorSub(vecs[b], vecs[c]);  
  norm= mav_vectorNormalize(mav_vectorCrossProduct(v1, v2));
  
  /* Render the pentagon as a polygon. Vecs contain unit pentagon, so mult by r */
  mav_gfxPolygonBegin();
  mav_gfxNormal(norm);

  t.s=cos(ang);
  t.t=sin(ang);
  mav_gfxTexCoord(t);
  ang+=0.4*MAV_PI; /* 72 degrees */
  mav_gfxVertex(mav_vectorScalar(vecs[a], r));

  t.s=cos(ang);
  t.t=sin(ang);
  mav_gfxTexCoord(t);
  ang+=0.4*MAV_PI;
  mav_gfxVertex(mav_vectorScalar(vecs[b], r));

  t.s=cos(ang);
  t.t=sin(ang);
  mav_gfxTexCoord(t);
  ang+=0.4*MAV_PI;
  mav_gfxVertex(mav_vectorScalar(vecs[c], r));

  t.s=cos(ang);
  t.t=sin(ang);
  mav_gfxTexCoord(t);
  ang+=0.4*MAV_PI;
  mav_gfxVertex(mav_vectorScalar(vecs[d], r));

  t.s=cos(ang);
  t.t=sin(ang);
  mav_gfxTexCoord(t);
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



/* Routine to return a pointer to the matrix field of the dodecahedron */

int mav_dodecGetMatrix(MAV_object *o, MAV_matrix **m)
{
  MAV_dodec *dodec= (MAV_dodec *) mav_objectDataGet(o);

  *m= &dodec->matrix;

  return MAV_TRUE;
}



/* Routine to return a pointer to the surfaceParams field of the dodecahedron */

int mav_dodecGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp)
{
  MAV_dodec *dodec= (MAV_dodec *) mav_objectDataGet(o);

  *sp= &dodec->sp;

  return MAV_TRUE;  
}



/* Define a dodecahedron */
void defDodec(MAV_dodec *d)
{
  d->r= 15.0;
  d->sp= sp[(int) (mav_random()*4)];
  d->matrix= mav_matrixSet(0,mav_random()*360,0, -200+mav_random()*400,0,-200+mav_random()*400);
}



int main(int argc, char *argv[])
{
  MAV_rectangle gp;
  MAV_SMS *groundPlane;
  MAV_box box[10];
  MAV_cylinder cyl[10];
  MAV_composite comp;
  MAV_SMS *objs;
  int i;
  MAV_dodec dodec;

  /* Initialise the Maverik system */
  mav_initialise(&argc, argv);

  /* Create a new class to represent the dodecahedron */
  mav_class_dodec= mav_classNew();

  /* Set the various callbacks for this new class */
  mav_callbackDrawSet(mav_win_all, mav_class_dodec, mav_dodecDraw);
  mav_callbackBBSet(mav_win_all, mav_class_dodec, mav_dodecBB2);
  mav_callbackIntersectSet(mav_win_all, mav_class_dodec, mav_dodecIntersect);
  mav_callbackGetMatrixSet(mav_win_all, mav_class_dodec, mav_dodecGetMatrix);
  mav_callbackGetSurfaceParamsSet(mav_win_all, mav_class_dodec, mav_dodecGetSurfaceParams);
  
  /* Define a texture map from file */
  mav_paletteTextureSet(mav_palette_default, 5, "marble_floor.ppm"); /* Texture index 5 */

  /* Define a set of "surface parameters", i.e. the colour with which an object is rendered */
  sp[0]= mav_surfaceParamsNew(MAV_MATERIAL, 0, 1, 0); /* Use material index 1 */
  sp[1]= mav_surfaceParamsNew(MAV_MATERIAL, 0, 2, 0); /* Use material index 2 */
  sp[2]= mav_surfaceParamsNew(MAV_MATERIAL, 0, 3, 0); /* Use material index 3 */
  sp[3]= mav_surfaceParamsNew(MAV_LIT_TEXTURE,  0, 2, 1); /* Use texture 1 modulated with material 2 */

  /* Define a rectangle to act as the ground plane */
  defRect(&gp);

  /* Create an SMS for the ground plane and add rectangle object to it */
  groundPlane= mav_SMSObjListNew();
  mav_SMSObjectAdd(groundPlane, mav_objectNew(mav_class_rectangle, &gp));

  /* Make objects in groundPlane SMS unselectable to keyboard and mouse event */
  mav_SMSSelectabilitySet(groundPlane, mav_win_all, MAV_FALSE);

  /* Create an SMS for the objects */
  objs= mav_SMSObjListNew();

  /* Create 10 boxes and cylinders */
  for (i=0; i<10; i++) {

    /* Define a box and a cylinder */
    defBox(&box[i]);
    defCyl(&cyl[i]);

    /* Add the box and cylinder to the objs SMS */
    mav_SMSObjectAdd(objs, mav_objectNew(mav_class_box, &box[i]));
    mav_SMSObjectAdd(objs, mav_objectNew(mav_class_cylinder, &cyl[i]));
  }

  /* Define a composite object and add it to objs SMS */  
  defComp(&comp);
  mav_SMSObjectAdd(objs, mav_objectNew(mav_class_composite, &comp));

  /* Define a dodecahedron and add it to objs SMS */  
  defDodec(&dodec);
  mav_SMSObjectAdd(objs, mav_objectNew(mav_class_dodec, &dodec));

  /* Use default mouse navigation */
  mav_navigationMouse(mav_win_all, mav_navigationMouseDefault);

  /* Define mouse event callbacks */
  mav_callbackMouseSet(MAV_MIDDLE_BUTTON, mav_win_all, mav_class_cylinder, cylEvent);
  mav_callbackMouseSet(MAV_MIDDLE_BUTTON, mav_win_all, mav_class_composite, compEvent);

  /* Define keyboard event callback */
  mav_callbackKeyboardSet(mav_win_all, mav_class_world, keyEvent);

  /* Rendering loop */
  while (1) drawFrame(groundPlane, objs);
}
