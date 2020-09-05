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
*	File:	BoxNode.cpp
*
******************************************************************/

#ifdef SUPPORT_OPENGL
#ifdef WIN32
#include <windows.h>
#endif
#include <GL/gl.h>
#include <GL/glu.h>
#endif

#include "BoxNode.h"

////////////////////////////////////////////////
//	BoxNode::recomputeBoundingBox
////////////////////////////////////////////////

void BoxNode::recomputeBoundingBox() 
{
	setBoundingBoxCenter(0.0f, 0.0f, 0.0f);
	setBoundingBoxSize(getX()/2.0f, getY()/2.0f, getZ()/2.0f);
}

////////////////////////////////////////////////
//	DrawBox
//
//	   4+--------+5
//	   /|       /|
//	  / |      / |
//	0+--------+1 |
//	 |  |     |  |
//	 | 7+-----|--+6
//	 | /      | /
//	 |/       |/
//	3+--------+2
//
////////////////////////////////////////////////

#ifdef SUPPORT_OPENGL

static void DrawBox(float x0, float x1, float y0, float y1,	float z0, float z1)
{
    static float n[6][3] = {
			{0.0, 0.0, 1.0}, {0.0, -1.0, 0.0}, {0.0, 0.0, 1.0},
			{0.0, 1.0, 0.0}, {1.0, 0.0, 0.0}, {-1.0, 0.0, 0.0}};

    static int faces[6][4] = {
			{ 3, 2, 1, 0 }, { 7, 6, 2, 3 }, { 4, 5, 6, 7 },
			{ 0, 1, 5, 4 }, { 1, 2, 6, 5 }, { 3, 0, 4, 7 }};

    static float t[4][2] = {
			{ 0.0f, 1.0f }, { 1.0f, 1.0f },
			{ 1.0f, 0.0f }, { 0.0f, 0.0f } };

    float	v[8][3];

	v[0][0] = v[3][0] = v[4][0] = v[7][0] = x0;
	v[1][0] = v[2][0] = v[5][0] = v[6][0] = x1;
	v[2][1] = v[3][1] = v[6][1] = v[7][1] = y0;
	v[0][1] = v[1][1] = v[4][1] = v[5][1] = y1;
	v[4][2] = v[5][2] = v[6][2] = v[7][2] = z0;
	v[0][2] = v[1][2] = v[2][2] = v[3][2] = z1;

	glFrontFace(GL_CCW);

    for (int i = 0; i < 6; i++) {
		glBegin(GL_POLYGON);
		glNormal3fv(n[i]);
		glTexCoord2fv(t[0]);
		glVertex3fv(v[faces[i][0]]);
		glTexCoord2fv(t[1]);
		glVertex3fv(v[faces[i][1]]);
		glTexCoord2fv(t[2]);
		glVertex3fv(v[faces[i][2]]);
		glTexCoord2fv(t[3]);
		glVertex3fv(v[faces[i][3]]);
		glEnd();
    }
}

////////////////////////////////////////////////
//	BoxNode::recomputeDisplayList
////////////////////////////////////////////////

void BoxNode::recomputeDisplayList() {
	unsigned int nCurrentDisplayList = getDisplayList();
	if (0 < nCurrentDisplayList)
		glDeleteLists(nCurrentDisplayList, 1);

	unsigned int nNewDisplayList = glGenLists(1);
	glNewList(nNewDisplayList, GL_COMPILE);
	    DrawBox(-getX()/2.0f, getX()/2.0f, -getY()/2.0f, getY()/2.0f, -getZ()/2.0f, getZ()/2.0f);
	glEndList();

	setDisplayList(nNewDisplayList);
};

#endif
