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
*	File:	PointSetNode.cpp
*
******************************************************************/

#ifdef SUPPORT_OPENGL
#ifdef WIN32
#include <windows.h>
#endif
#include <GL/gl.h>
#include <GL/glaux.h>
#endif

#include "PointSetNode.h"

////////////////////////////////////////////////////////////
//	PointSetNode::recomputeBoundingBox
////////////////////////////////////////////////////////////

void PointSetNode::recomputeBoundingBox() 
{
	CoordinateNode *coordinate = getCoordinateNodes();
	if (!coordinate) {
		setBoundingBoxCenter(0.0f, 0.0f, 0.0f);
		setBoundingBoxSize(-1.0f, -1.0f, -1.0f);
		return;
	}

	BoundingBox bbox;
	float		point[3];

	int nCoordinatePoints = coordinate->getNPoints();
	for (int n=0; n<nCoordinatePoints; n++) {
		coordinate->getPoint(n, point);
		bbox.addPoint(point);
	}

	setBoundingBox(&bbox);
}

////////////////////////////////////////////////
//	PointSetNode::recomputeDisplayList
////////////////////////////////////////////////

#ifdef SUPPORT_OPENGL

static void DrawPointSet(PointSetNode *pointSet)
{
	CoordinateNode *coordinate = pointSet->getCoordinateNodes();
	if (!coordinate)
		return;

	NormalNode	*normal = pointSet->getNormalNodes();
	ColorNode	*color = pointSet->getColorNodes();

	float	vpoint[3];
	float	pcolor[3];

	glColor3f(1.0f, 1.0f, 1.0f);

	glBegin(GL_POINTS);

	int nCoordinatePoint = coordinate->getNPoints();
	for (int n=0; n<nCoordinatePoint; n++) {

		if (color) {
			color->getColor(n, pcolor);
			glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, pcolor);
//			glColor3fv(pcolor);
		}

		coordinate->getPoint(n, vpoint);
		glVertex3fv(vpoint);
	}

	glEnd();
}

void PointSetNode::recomputeDisplayList() 
{
	CoordinateNode *coordinate = getCoordinateNodes();
	if (!coordinate)
		return;

	unsigned int nCurrentDisplayList = getDisplayList();
	if (0 < nCurrentDisplayList)
		glDeleteLists(nCurrentDisplayList, 1);

	unsigned int nNewDisplayList = glGenLists(1);
	glNewList(nNewDisplayList, GL_COMPILE);
		DrawPointSet(this);
	glEndList();

	setDisplayList(nNewDisplayList);
};

#endif
