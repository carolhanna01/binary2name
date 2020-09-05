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
#include "mavlib_vrad.h"
#ifdef WIN32
#include <windows.h>
#endif
#include <GL/gl.h>

MAV_class *mavlib_vradElemClass;

#define v3f(X) glVertex3fv((float *) &(X))
#define bgntmesh() glBegin(GL_TRIANGLE_STRIP)
#define endtmesh() glEnd()
#if 1
#define c4s(X) glColor4fv(X)
#else
#define c4s(X) glColor4f(0,0,0,0)
#endif


int mavlib_vradElemDraw(MAV_object *o, MAV_drawInfo *di)
{
  MAVLIB_vradElem *elem= (MAVLIB_vradElem *) mav_objectDataGet(o);

  switch (elem->type) {
    case 0 :
    case 1 :
      bgntmesh();
      c4s(elem->verts[0].colour);
      v3f(elem->verts[0].vertex);
      c4s(elem->verts[1].colour);
      v3f(elem->verts[1].vertex);
      c4s(elem->verts[2].colour);
      v3f(elem->verts[2].vertex);
      c4s(elem->verts[3].colour);
      v3f(elem->verts[3].vertex);
      endtmesh();
      break;
    case 2 :
    case 3 :
    case 4 :
    case 5 :
      bgntmesh();
      c4s(elem->verts[0].colour);
      v3f(elem->verts[0].vertex);
      c4s(elem->verts[1].colour);
      v3f(elem->verts[1].vertex);
      c4s(elem->verts[2].colour);
      v3f(elem->verts[2].vertex);
      c4s(elem->verts[3].colour);
      v3f(elem->verts[3].vertex);
      c4s(elem->verts[4].colour);
      v3f(elem->verts[4].vertex);
      endtmesh();
      break;
    case 6 :
    case 8 :
    case 9 :
    case 11 :
      bgntmesh();
      c4s(elem->verts[0].colour);
      v3f(elem->verts[0].vertex);
      c4s(elem->verts[1].colour);
      v3f(elem->verts[1].vertex);
      c4s(elem->verts[2].colour);
      v3f(elem->verts[2].vertex);
      c4s(elem->verts[3].colour);
      v3f(elem->verts[3].vertex);
      c4s(elem->verts[4].colour);
      v3f(elem->verts[4].vertex);
      endtmesh();
      bgntmesh();
      c4s(elem->verts[1].colour);
      v3f(elem->verts[1].vertex);
      c4s(elem->verts[5].colour);
      v3f(elem->verts[5].vertex);
      c4s(elem->verts[3].colour);
      v3f(elem->verts[3].vertex);
      endtmesh();
      break;
    case 7 :
    case 10 :
      bgntmesh();
      c4s(elem->verts[0].colour);
      v3f(elem->verts[0].vertex);
      c4s(elem->verts[1].colour);
      v3f(elem->verts[1].vertex);
      c4s(elem->verts[2].colour);
      v3f(elem->verts[2].vertex);
      c4s(elem->verts[3].colour);
      v3f(elem->verts[3].vertex);
      c4s(elem->verts[4].colour);
      v3f(elem->verts[4].vertex);
      c4s(elem->verts[5].colour);
      v3f(elem->verts[5].vertex);
      endtmesh();
      break;
    case 12 :
    case 13 :
    case 14 :
    case 15 :
      bgntmesh();
      c4s(elem->verts[0].colour);
      v3f(elem->verts[0].vertex);
      c4s(elem->verts[1].colour);
      v3f(elem->verts[1].vertex);
      c4s(elem->verts[2].colour);
      v3f(elem->verts[2].vertex);
      c4s(elem->verts[3].colour);
      v3f(elem->verts[3].vertex);
      c4s(elem->verts[4].colour);
      v3f(elem->verts[4].vertex);
      endtmesh();
      bgntmesh();
      c4s(elem->verts[4].colour);
      v3f(elem->verts[4].vertex);
      c4s(elem->verts[5].colour);
      v3f(elem->verts[5].vertex);
      c4s(elem->verts[2].colour);
      v3f(elem->verts[2].vertex);
      c4s(elem->verts[6].colour);
      v3f(elem->verts[6].vertex);
      endtmesh();
      break;
    case 16 :
      bgntmesh();
      c4s(elem->verts[0].colour);
      v3f(elem->verts[0].vertex);
      c4s(elem->verts[1].colour);
      v3f(elem->verts[1].vertex);
      c4s(elem->verts[2].colour);
      v3f(elem->verts[2].vertex);
      c4s(elem->verts[3].colour);
      v3f(elem->verts[3].vertex);
      c4s(elem->verts[4].colour);
      v3f(elem->verts[4].vertex);
      endtmesh();
      bgntmesh();
      c4s(elem->verts[7].colour);
      v3f(elem->verts[7].vertex);
      c4s(elem->verts[3].colour);
      v3f(elem->verts[3].vertex);
      c4s(elem->verts[6].colour);
      v3f(elem->verts[6].vertex);
      c4s(elem->verts[1].colour);
      v3f(elem->verts[1].vertex);
      c4s(elem->verts[5].colour);
      v3f(elem->verts[5].vertex);
      endtmesh();
      break;
    }

  return 1;
}



int mavlib_vradElemBB(MAV_object *o, MAV_BB *bb)
{
  MAVLIB_vradElem *elem= (MAVLIB_vradElem *) mav_objectDataGet(o);

  *bb= elem->bb;

  return 1;
}
