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

/******************************************************************
*
*	VRML library for C++
*
*	Copyright (C) Satoshi Konno 1996-1997
*
*	File:	SphereNode.cpp
*
******************************************************************/

#ifdef SUPPORT_OPENGL
#ifdef WIN32
#include <windows.h>
#endif
#include <GL/gl.h>
#include <GL/glu.h>
#endif

#include "SphereNode.h"

#ifdef SUPPORT_OPENGL
////////////////////////////////////////////////
//	SphereNode::recomputeDisplayList
////////////////////////////////////////////////

void SphereNode::recomputeDisplayList() {
	unsigned int nCurrentDisplayList = getDisplayList();
	if (0 < nCurrentDisplayList)
		glDeleteLists(nCurrentDisplayList, 1);

	unsigned int nNewDisplayList = glGenLists(1);
	glNewList(nNewDisplayList, GL_COMPILE);
		glFrontFace(GL_CCW);

	    glPushMatrix ();
	
		glMatrixMode(GL_TEXTURE);
		glLoadIdentity();
	    glRotatef (180.0, 0.0, 1.0, 0.0);
		
		glMatrixMode(GL_MODELVIEW);

	    glRotatef (90.0, 1.0, 0.0, 0.0);
	    glRotatef (180.0, 0.0, 0.0, 1.0);

	    GLUquadricObj *quadObj = gluNewQuadric ();
	    gluQuadricDrawStyle(quadObj, GLU_FILL);
	    gluQuadricNormals(quadObj, GLU_SMOOTH);
	    gluQuadricTexture(quadObj, GL_TRUE);
	    gluSphere(quadObj, getRadius(), 16, 16);
		gluDeleteQuadric(quadObj);

	    glPopMatrix ();

	glEndList();

	setDisplayList(nNewDisplayList);
};

#endif
