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
#include <stdlib.h>



/* Wrapper routines to set and execute bounding box callback */

MAV_callback *mav_callback_BB;

void mav_callbackBBSet(MAV_window *w, MAV_class *c, MAV_callbackBBFn fn)
{
  mav_callbackSet(mav_callback_BB, w, c, (MAV_callbackFn) fn);
}

int mav_callbackBBExec(MAV_window *w, MAV_object *o, MAV_BB *bb)
{
  return (mav_callbackExec(mav_callback_BB, w, o, (void *) bb, NULL));
}



/* Support routines for bounding boxes */

/* Routine to print the contents of a bounding box */

void mav_BBPrint(char *s, MAV_BB bb)
{
  printf("%s min %f %f %f max %f %f %f\n", s, bb.min.x, bb.min.y, bb.min.z, bb.max.x, bb.max.y, bb.max.z);
}



/* Routine to axis align a BB defined in one coordinate frame to another */

void mav_BBAlign(MAV_BB orig, MAV_matrix matrix, MAV_BB *aligned)
{
  MAV_vector vert[8];
  int i;

/* initialise the aligned BB */
  mav_BBCompInit(aligned);

/* Store the 8 corner vertices of the BB */
  vert[0].x=orig.min.x;
  vert[0].y=orig.min.y;
  vert[0].z=orig.min.z;

  vert[1].x=orig.min.x;
  vert[1].y=orig.max.y;
  vert[1].z=orig.min.z;

  vert[2].x=orig.max.x;
  vert[2].y=orig.min.y;
  vert[2].z=orig.min.z;

  vert[3].x=orig.max.x;
  vert[3].y=orig.max.y;
  vert[3].z=orig.min.z;

  vert[4].x=orig.min.x;
  vert[4].y=orig.min.y;
  vert[4].z=orig.max.z;

  vert[5].x=orig.min.x;
  vert[5].y=orig.max.y;
  vert[5].z=orig.max.z;

  vert[6].x=orig.max.x;
  vert[6].y=orig.min.y;
  vert[6].z=orig.max.z;

  vert[7].x=orig.max.x;
  vert[7].y=orig.max.y;
  vert[7].z=orig.max.z;

/* Calculate the min and max x,y, and z values of the transformed points */
  
  for (i=0; i<8; i++) mav_BBCompPt(mav_vectorMult(vert[i], matrix), aligned);
}



/* Routine to intersect a line with a BB */

int mav_BBIntersectsLine(MAV_BB bb, MAV_line ln, MAV_objectIntersection *o) 
{
  /* intersect bounding box 'bb' */
  MAV_vector distvect, pt;
  int rv= MAV_FALSE;
  float this_exit;

  /* reset hit and exit */
  o->pt1=-100;
  o->pt2=-100;
  
  pt= ln.pt;
  
  if (!(((pt.x<=bb.min.x)&&(ln.dir.x<0))|| ((pt.y<=bb.min.y)&&(ln.dir.y<0))||
        ((pt.z<=bb.min.z)&&(ln.dir.z<0))|| ((pt.x>=bb.max.x)&&(ln.dir.x>0))||
        ((pt.y>=bb.max.y)&&(ln.dir.y>0))|| ((pt.z>=bb.max.z)&&(ln.dir.z>0)))) {
    
    /* ray is either inside box or is pointing towards box */
    
    /* intersect ray with each plane face updating value of pt */
    
    /* test x values */
    if ((pt.x < bb.min.x) && (ln.dir.x > 0)) {
      pt.y += ln.dir.y * (bb.min.x - pt.x)/ ln.dir.x;
      pt.z += ln.dir.z * (bb.min.x - pt.x)/ ln.dir.x;
      pt.x= bb.min.x;
    } else if ((pt.x > bb.max.x) && (ln.dir.x < 0)) {
      pt.y += ln.dir.y * (bb.max.x - pt.x)/ ln.dir.x;
      pt.z += ln.dir.z * (bb.max.x - pt.x)/ ln.dir.x;
      pt.x= bb.max.x;
    }
    
    /* test y values */
    if ((pt.y < bb.min.y) && (ln.dir.y > 0)) {
      pt.x += ln.dir.x * (bb.min.y - pt.y)/ln.dir.y;
      pt.z += ln.dir.z * (bb.min.y - pt.y)/ln.dir.y;
      pt.y= bb.min.y;
    } else if ((pt.y > bb.max.y) && (ln.dir.y < 0)) {
      pt.x += ln.dir.x * (bb.max.y - pt.y)/ln.dir.y;
      pt.z += ln.dir.z * (bb.max.y - pt.y)/ln.dir.y;
      pt.y= bb.max.y;
    }
    
    /* test z values */
    if ((pt.z < bb.min.z) && (ln.dir.z > 0)) {
      pt.x += ln.dir.x * (bb.min.z - pt.z)/ln.dir.z;
      pt.y += ln.dir.y * (bb.min.z - pt.z)/ln.dir.z;
      pt.z= bb.min.z;
    } else if ((pt.z>bb.max.z) && (ln.dir.z < 0)) {
      pt.x += ln.dir.x * (bb.max.z - pt.z)/ln.dir.z;
      pt.y += ln.dir.y * (bb.max.z - pt.z)/ln.dir.z;
      pt.z= bb.max.z;
    }
    
    /* test new pt */
    if ((pt.x>=bb.min.x) && (pt.x <=bb.max.x) &&
        (pt.y>=bb.min.y) && (pt.y<=bb.max.y) &&
        (pt.z>=bb.min.z) && (pt.z<=bb.max.z)) {
      /* pt is inside box so ray intersects box */
      rv= MAV_TRUE;
      /* get distance to intersection */
      distvect= mav_vectorSub (ln.pt, pt);
      o->pt1= mav_vectorMag (distvect);

      /* calculate distance to exit point */
      /* very similar to entrance point calculation */
      /* except we just need the distance to the nearest */
      /* min or max plane */
      if ((pt.x <= bb.max.x) && (ln.dir.x > 0))
	o->pt2= (bb.max.x-pt.x) / ln.dir.x;
      else if ((pt.x >= bb.min.x) && (ln.dir.x < 0))
	o->pt2= (bb.min.x-pt.x) / ln.dir.x;

      if ((pt.y <= bb.max.y) && (ln.dir.y > 0)) {
	this_exit= (bb.max.y-pt.y) / ln.dir.y;
	if ((o->pt2<-1.0) || (this_exit<o->pt2))
	  o->pt2= this_exit;
      } else if ((pt.y >= bb.min.y) && (ln.dir.y < 0)) {
	this_exit= (bb.min.y-pt.y) / ln.dir.y;
        if ((o->pt2<-1.0) || (this_exit<o->pt2))
          o->pt2= this_exit;
      }

      if ((pt.z <= bb.max.z) && (ln.dir.z > 0)) {
        this_exit= (bb.max.z-pt.z) / ln.dir.z;
        if ((o->pt2<-1.0) || (this_exit<o->pt2))
          o->pt2= this_exit;
      } else if ((pt.z >= bb.min.z) && (ln.dir.z < 0)) {
        this_exit= (bb.min.z-pt.z) / ln.dir.z;
        if ((o->pt2<-1.0) || (this_exit<o->pt2))
          o->pt2= this_exit;
      }

      /* this check needed to avoid missing the second intersection */
      /* point due to rounding errors (when ray just touches an edge) */
      if (o->pt2 < 0.0) o->pt2= 0.0;

      /* current value is distance from o->pt1 so add that value for */
      /* distance from original point */
      o->pt2 += o->pt1;
    }
  }

  return (rv);
}



/* Routine to intersect two BB's */

int mav_BBIntersectsBB(MAV_BB bb1, MAV_BB bb2) 
{
  if (bb2.min.x > bb1.max.x) return MAV_FALSE;
  if (bb2.min.y > bb1.max.y) return MAV_FALSE;
  if (bb2.min.z > bb1.max.z) return MAV_FALSE;
  if (bb2.max.x < bb1.min.x) return MAV_FALSE;
  if (bb2.max.y < bb1.min.y) return MAV_FALSE;
  if (bb2.max.z < bb1.min.z) return MAV_FALSE;

  return MAV_TRUE;
}



/* Routine to calculate complete enclosure of two BB's */

int mav_BBInsideBB(MAV_BB bb1, MAV_BB bb2)
{
  if ((bb2.min.x > bb1.min.x && bb2.max.x < bb1.max.x) &&
      (bb2.min.y > bb1.min.y && bb2.max.y < bb1.max.y) &&
      (bb2.min.z > bb1.min.z && bb2.max.z < bb1.max.z))
      return MAV_TRUE;
  if ((bb1.min.x > bb2.min.x && bb1.max.x < bb2.max.x) &&
      (bb1.min.y > bb2.min.y && bb1.max.y < bb2.max.y) &&
      (bb1.min.z > bb2.min.z && bb1.max.z < bb2.max.z))
      return MAV_TRUE;
  return MAV_FALSE;
}



/* Various routines to draw BB's */

void mavlib_BBDisplayWithColourToAll(MAV_BB bb, int colour)
{
  MAV_window *w;

  /* step through list of all windows */
  mav_listPointerReset(mav_win_list);
  while (mav_listItemNext(mav_win_list, (void **) &w)) mav_BBDisplayWithColour(w, bb, colour);
}

void mav_BBDisplayWithColour(MAV_window *w, MAV_BB bb, int colour)
{
  MAV_window *orig_win= mav_win_current;
  MAV_surfaceParams sp;
  MAV_vector vert[8];

  if (w==mav_win_all) 
  {
    /* trap for mav_win_all */
    mavlib_BBDisplayWithColourToAll(bb, colour);
  }
  else
  {
    /* switch to correct window */
    if (w!=orig_win) mav_windowSet(w);

    /* calculate the 8 vertices of the BB */
    vert[0].x=bb.min.x;
    vert[0].y=bb.min.y;
    vert[0].z=bb.min.z;

    vert[1].x=bb.min.x;
    vert[1].y=bb.max.y;
    vert[1].z=bb.min.z;
    
    vert[2].x=bb.max.x;
    vert[2].y=bb.min.y;
    vert[2].z=bb.min.z;

    vert[3].x=bb.max.x;
    vert[3].y=bb.max.y;
    vert[3].z=bb.min.z;
    
    vert[4].x=bb.min.x;
    vert[4].y=bb.min.y;
    vert[4].z=bb.max.z;
    
    vert[5].x=bb.min.x;
    vert[5].y=bb.max.y;
    vert[5].z=bb.max.z;

    vert[6].x=bb.max.x;
    vert[6].y=bb.min.y;
    vert[6].z=bb.max.z;
    
    vert[7].x=bb.max.x;
    vert[7].y=bb.max.y;
    vert[7].z=bb.max.z;
    
    /* set the correct colour */
    sp.mode= MAV_COLOUR;
    sp.colour= colour;
    sp.material= 0;
    sp.texture= 0;
    mav_surfaceParamsUse(&sp);
    
    /* draw BB as wireframe box */
    mav_gfxLineClosedBegin();
    mav_gfxVertex(vert[0]);
    mav_gfxVertex(vert[1]);
    mav_gfxVertex(vert[3]);
    mav_gfxVertex(vert[2]);
    mav_gfxLineClosedEnd();

    mav_gfxLineClosedBegin();
    mav_gfxVertex(vert[4]);
    mav_gfxVertex(vert[5]);
    mav_gfxVertex(vert[7]);
    mav_gfxVertex(vert[6]);
    mav_gfxLineClosedEnd();
    
    mav_gfxLineBegin();
    mav_gfxVertex(vert[0]);
    mav_gfxVertex(vert[4]);
    mav_gfxLineEnd();
    
    mav_gfxLineBegin();
    mav_gfxVertex(vert[1]);
    mav_gfxVertex(vert[5]);
    mav_gfxLineEnd();
    
    mav_gfxLineBegin();
    mav_gfxVertex(vert[2]);
    mav_gfxVertex(vert[6]);
    mav_gfxLineEnd();
    
    mav_gfxLineBegin();
    mav_gfxVertex(vert[3]);
    mav_gfxVertex(vert[7]);
    mav_gfxLineEnd();

    /* switch back to original window */
    if (w!=orig_win) mav_windowSet(orig_win);
  }
}

void mav_BBDisplay(MAV_window *w, MAV_BB bb)
{
  /* draw with colour black */
  mav_BBDisplayWithColour(w, bb, MAV_COLOUR_BLACK);
}



void mavlib_BBDisplayWithSurfaceParamsToAll(MAV_BB bb, MAV_surfaceParams *sp)
{
  MAV_window *w;

  /* step through list of all windows */
  mav_listPointerReset(mav_win_list);
  while (mav_listItemNext(mav_win_list, (void **) &w)) mav_BBDisplayWithSurfaceParams(w, bb, sp);
}

void mav_BBDisplayWithSurfaceParams(MAV_window *w, MAV_BB bb, MAV_surfaceParams *sp)
{
  MAV_window *orig_win= mav_win_current;
  MAV_vector vert[8], norm;
  MAV_texCoord tex[4];

  if (w==mav_win_all) 
  {
    /* trap for mav_win_all */
    mavlib_BBDisplayWithSurfaceParamsToAll(bb, sp);
  }
  else
  {
    /* switch to correct window */
    if (w!=orig_win) mav_windowSet(w);

    /* calculate the 8 vertices of the BB */
    vert[0].x=bb.min.x;
    vert[0].y=bb.min.y;
    vert[0].z=bb.min.z;

    vert[1].x=bb.min.x;
    vert[1].y=bb.max.y;
    vert[1].z=bb.min.z;
    
    vert[2].x=bb.max.x;
    vert[2].y=bb.min.y;
    vert[2].z=bb.min.z;

    vert[3].x=bb.max.x;
    vert[3].y=bb.max.y;
    vert[3].z=bb.min.z;
    
    vert[4].x=bb.min.x;
    vert[4].y=bb.min.y;
    vert[4].z=bb.max.z;
    
    vert[5].x=bb.min.x;
    vert[5].y=bb.max.y;
    vert[5].z=bb.max.z;

    vert[6].x=bb.max.x;
    vert[6].y=bb.min.y;
    vert[6].z=bb.max.z;
    
    vert[7].x=bb.max.x;
    vert[7].y=bb.max.y;
    vert[7].z=bb.max.z;
    
    /* set the correct colour */
    mav_surfaceParamsUse(sp);
    
    /* calculate texture coordinates if applicable */
    if (sp->mode>=MAV_TEXTURE) {
      tex[0].s = 0.0;
      tex[0].t = 0.0;
      
      tex[1].s = 0.0;
      tex[1].t = 1.0;
      
      tex[2].s = 1.0;
      tex[2].t = 0.0;
      
      tex[3].s = 1.0;
      tex[3].t = 1.0;
    }

/* draw BB as filled box */

/* Face 0,  Norm 0, 0, -1, Verts 0132 */ 

    if (sp->mode==MAV_MATERIAL || sp->mode>=MAV_LIT_TEXTURE) {
      norm.x=0.0;
      norm.y=0.0;
      norm.z=-1.0;
      mav_gfxNormal(norm);
    }
    
    mav_gfxPolygonBegin();  
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[2]);
    mav_gfxVertex(vert[0]);
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[3]);
    mav_gfxVertex(vert[1]);
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[1]);
    mav_gfxVertex(vert[3]);
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[0]);
    mav_gfxVertex(vert[2]);
    mav_gfxPolygonEnd();
    
/* Face 1,  Norm 0, 0, 1, Verts 4675 */ 
    
    if (sp->mode==MAV_MATERIAL || sp->mode>=MAV_LIT_TEXTURE) {
      norm.z=1.0;
      mav_gfxNormal(norm);
    }
    
    mav_gfxPolygonBegin();
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[0]);
    mav_gfxVertex(vert[4]);
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[2]);
    mav_gfxVertex(vert[6]);
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[3]);
    mav_gfxVertex(vert[7]);
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[1]);
    mav_gfxVertex(vert[5]);
    mav_gfxPolygonEnd();

/* Face 2,  Norm 1, 0, 0, Verts 2376 */ 

    if (sp->mode==MAV_MATERIAL || sp->mode>=MAV_LIT_TEXTURE) {
      norm.x=1.0;
      norm.z=0.0;
      mav_gfxNormal(norm);
    }

    mav_gfxPolygonBegin();
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[2]);
    mav_gfxVertex(vert[2]);
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[3]);
    mav_gfxVertex(vert[3]);
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[1]);
    mav_gfxVertex(vert[7]);
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[0]);
    mav_gfxVertex(vert[6]);
    mav_gfxPolygonEnd();
    
/* Face 3,  Norm -1, 0, 0, Verts 0451 */ 

    if (sp->mode==MAV_MATERIAL || sp->mode>=MAV_LIT_TEXTURE) {
      norm.x=-1.0;
      mav_gfxNormal(norm);
    }

    mav_gfxPolygonBegin();
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[0]);
    mav_gfxVertex(vert[0]);
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[2]);
    mav_gfxVertex(vert[4]);
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[3]);
    mav_gfxVertex(vert[5]);
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[1]);
    mav_gfxVertex(vert[1]);
    mav_gfxPolygonEnd();

/* Face 4,  Norm 0, 1, 0, Verts 1573 */ 

    if (sp->mode==MAV_MATERIAL || sp->mode>=MAV_LIT_TEXTURE) {
      norm.x=0.0;
      norm.y=1.0;
      mav_gfxNormal(norm);
    }
    
    mav_gfxPolygonBegin();
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[1]);
    mav_gfxVertex(vert[1]);
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[0]);
    mav_gfxVertex(vert[5]);
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[2]);
    mav_gfxVertex(vert[7]);
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[3]);
    mav_gfxVertex(vert[3]);
    mav_gfxPolygonEnd();
    
/* Face 5,  Norm 0, -1, 0, Verts 0264 */ 

    if (sp->mode==MAV_MATERIAL || sp->mode>=MAV_LIT_TEXTURE) {
      norm.y=-1.0;  
      mav_gfxNormal(norm);
    }

    mav_gfxPolygonBegin();
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[1]);
    mav_gfxVertex(vert[0]);
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[3]);
    mav_gfxVertex(vert[2]);
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[2]);
    mav_gfxVertex(vert[6]);
    if (sp->mode>=MAV_TEXTURE) mav_gfxTexCoord(tex[0]);
    mav_gfxVertex(vert[4]);
    mav_gfxPolygonEnd();
    
    if (w!=orig_win) mav_windowSet(orig_win);
  }
}



/* Routines to calculate a BB which is composed of other BB's or points */

void mav_BBCompInit(MAV_BB *bb)
{
  bb->min.x=MAV_INFINITY;
  bb->min.y=MAV_INFINITY;
  bb->min.z=MAV_INFINITY;
  bb->max.x=-MAV_INFINITY;
  bb->max.y=-MAV_INFINITY;
  bb->max.z=-MAV_INFINITY;
}

void mav_BBCompBB(MAV_BB bb, MAV_BB *comp)
{
  if (bb.min.x < comp->min.x) comp->min.x=bb.min.x;
  if (bb.min.y < comp->min.y) comp->min.y=bb.min.y;
  if (bb.min.z < comp->min.z) comp->min.z=bb.min.z;
  if (bb.max.x > comp->max.x) comp->max.x=bb.max.x;
  if (bb.max.y > comp->max.y) comp->max.y=bb.max.y;
  if (bb.max.z > comp->max.z) comp->max.z=bb.max.z;
}

void mav_BBCompPt(MAV_vector pt, MAV_BB *comp)
{
  if (pt.x < comp->min.x) comp->min.x=pt.x;
  if (pt.y < comp->min.y) comp->min.y=pt.y;
  if (pt.z < comp->min.z) comp->min.z=pt.z;
  if (pt.x > comp->max.x) comp->max.x=pt.x;
  if (pt.y > comp->max.y) comp->max.y=pt.y;
  if (pt.z > comp->max.z) comp->max.z=pt.z;
}
