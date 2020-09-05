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
*	File:	IndexedLineSetNode.cpp
*
******************************************************************/

#ifdef SUPPORT_OPENGL
#ifdef WIN32
#include <windows.h>
#endif
#include <GL/gl.h>
#include <GL/glaux.h>
#endif

#include "IndexedLineSetNode.h"

////////////////////////////////////////////////////////////
//	IndexedLineSetNode::recomputeBoundingBox
////////////////////////////////////////////////////////////

void IndexedLineSetNode::recomputeBoundingBox() 
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
//	IndexedLineSetNode::recomputeDisplayList
////////////////////////////////////////////////

#ifdef SUPPORT_OPENGL

static void DrawIdxLineSet(IndexedLineSetNode *idxLineSet)
{
	CoordinateNode *coordinate = idxLineSet->getCoordinateNodes();
	if (!coordinate)
		return;

	NormalNode	*normal = idxLineSet->getNormalNodes();
	ColorNode	*color = idxLineSet->getColorNodes();
	int		bColorPerVertex =idxLineSet->getColorPerVertex();

	bool	bLineBegin = true;
	bool	bLineClose = true;
	int		nLine = 0;

	float	vpoint[3];
	float	pcolor[3];

	glColor3f(1.0f, 1.0f, 1.0f);

	int nCoordIndexes = idxLineSet->getNCoordIndexes();
	for (int nCoordIndex=0; nCoordIndex<nCoordIndexes; nCoordIndex++) {

		int coordIndex = idxLineSet->getCoordIndex(nCoordIndex);

		if (bLineBegin) {
			glBegin(GL_LINE_STRIP);
			bLineBegin = false;
			bLineClose = false;

			if (color && !bColorPerVertex) {
				color->getColor(nLine, pcolor);
				glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, pcolor);
//				glColor3fv(pcolor);
			}

			nLine++;
		}

		if (coordIndex != -1) {
			coordinate->getPoint(coordIndex, vpoint);
			glVertex3fv(vpoint);

			if (color && bColorPerVertex) {
				color->getColor(coordIndex, pcolor);
				glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, pcolor);
//				glColor3fv(pcolor);
			}
		}
		else {
			glEnd();
			bLineBegin = true;
			bLineClose = true;
		}
	}

	if (bLineClose == false)
		glEnd();
}

void IndexedLineSetNode::recomputeDisplayList() 
{
	CoordinateNode *coordinate = getCoordinateNodes();
	if (!coordinate)
		return;

	unsigned int nCurrentDisplayList = getDisplayList();
	if (0 < nCurrentDisplayList)
		glDeleteLists(nCurrentDisplayList, 1);

	unsigned int nNewDisplayList = glGenLists(1);
	glNewList(nNewDisplayList, GL_COMPILE);
		DrawIdxLineSet(this);
	glEndList();

	setDisplayList(nNewDisplayList);
};

#endif
