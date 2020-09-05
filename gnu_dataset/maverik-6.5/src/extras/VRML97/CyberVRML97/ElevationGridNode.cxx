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
*	File:	ElevationGridNode.cpp
*
******************************************************************/

#ifdef SUPPORT_OPENGL
#ifdef WIN32
#include <windows.h>
#endif
#include <GL/gl.h>
#include <GL/glaux.h>
#endif

#include "ElevationGridNode.h"

////////////////////////////////////////////////
//	GroupingNode::recomputeBoundingBox
////////////////////////////////////////////////

void ElevationGridNode::initialize() 
{
	if (!isInitialized()) {
#ifdef SUPPORT_OPENGL
		recomputeDisplayList();
#endif
		recomputeBoundingBox();
		setInitialized(true);
	}
}

////////////////////////////////////////////////
//	GroupingNode::recomputeBoundingBox
////////////////////////////////////////////////

void ElevationGridNode::recomputeBoundingBox()
{
	float xSize = getXDimension()*getXSpacing();
	float zSize = getZDimension()*getZSpacing();

	setBoundingBoxCenter(xSize/2.0f, 0.0f, zSize/2.0f);
	setBoundingBoxSize(xSize, 0.0f, zSize);
}

////////////////////////////////////////////////
//	GroupingNode::recomputeDisplayList
////////////////////////////////////////////////

#ifdef SUPPORT_OPENGL

void GetNormalFromVertices(float vpoint[3][3], float vector[3]);

static void DrawElevationGrid(ElevationGridNode *eg)
{
	int xDimension = eg->getXDimension();
	int zDimension = eg->getZDimension();

	int nPoints = xDimension * zDimension;

	float xSpacing = eg->getXSpacing();
	float zSpacing = eg->getZSpacing();

	int x, z;

	SFVec3f *point = new SFVec3f[nPoints];
	for (x=0; x<xDimension; x++) {
		for (z=0; z<zDimension; z++) {
			float xpos = xSpacing * x;
			float ypos = eg->getHeight(x + z*zDimension);
			float zpos = zSpacing * z;
			point[x + z*xDimension].setValue(xpos, ypos, zpos);
		}
	}
	
	ColorNode				*color		= eg->getColorNodes();
	NormalNode				*normal		= eg->getNormalNodes();
	TextureCoordinateNode	*texCoord	= eg->getTextureCoordinateNodes();

	bool	bColorPerVertex		= eg->getColorPerVertex();
	bool	bNormalPerVertex	= eg->getNormalPerVertex();

	bool ccw = eg->getCCW();
	if (ccw == true)
		glFrontFace(GL_CCW);
	else
		glFrontFace(GL_CW);

	bool solid = eg->getSolid();
	if (solid == false)
		glDisable(GL_CULL_FACE);
	else
		glEnable(GL_CULL_FACE);

	glNormal3f(0.0f, 1.0f, 0.0f);
	for (x=0; x<xDimension-1; x++) {
		for (z=0; z<zDimension-1; z++) {

			int n;

			if (bColorPerVertex == false && color) {
				float	egColor[4]; egColor[3] = 1.0f;
				color->getColor(x + z*zDimension, egColor);
				glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, egColor);
//				glColor3fv(egColor);
			}
			if (bNormalPerVertex == false && normal) {
				float egNormal[3];
				normal->getVector(x + z*zDimension, egNormal);
				glNormal3fv(egNormal);
			}

			float	egPoint[4][3]; 
			float	egColor[4][4]; 
			float	egNormal[4][3];
			float	egTexCoord[4][2];

			egColor[0][3] = egColor[1][3] = egColor[2][3] = egColor[3][3] = 1.0f;

			for (n=0; n<4; n++) {
				
				int xIndex = x + ((n < 2) ? 0 : 1);
				int zIndex = z + (n%2);
				int index = xIndex + zIndex*xDimension;

				if (bColorPerVertex == true && color)
					color->getColor(index, egColor[n]);

				if (bNormalPerVertex == true && normal)
					normal->getVector(index, egNormal[n]);

				if (texCoord)
					texCoord->getPoint(index, egTexCoord[n]);
				else {
					egTexCoord[n][0] = (float)xIndex / (float)(xDimension-1);
					egTexCoord[n][1] = (float)zIndex / (float)(zDimension-1);
				}

				point[index].getValue(egPoint[n]);
			}
	
			glBegin(GL_POLYGON);
			for (n=0; n<3; n++) {
				if (bColorPerVertex == true && color) {
					glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, egColor[n]);
//					glColor3fv(egColor);
				}
				if (bNormalPerVertex == true && normal)
					glNormal3fv(egNormal[n]);
				glTexCoord2fv(egTexCoord[n]);
				glVertex3fv(egPoint[n]);
			}
 			glEnd();

			glBegin(GL_POLYGON);
			for (n=3; 0<n; n--) {
				if (bColorPerVertex == true && color) {
					glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, egColor[n]);
//					glColor3fv(egColor);
				}
				if (bNormalPerVertex == true && normal)
					glNormal3fv(egNormal[n]);
				glTexCoord2fv(egTexCoord[n]);
				glVertex3fv(egPoint[n]);
			}
 			glEnd();
		}
	}

	if (ccw == false)
		glFrontFace(GL_CCW);

	if (solid == false)
		glEnable(GL_CULL_FACE);

	delete []point;
}

void ElevationGridNode::recomputeDisplayList()
{
	unsigned int nCurrentDisplayList = getDisplayList();
	if (0 < nCurrentDisplayList)
		glDeleteLists(nCurrentDisplayList, 1);

	unsigned int nNewDisplayList = glGenLists(1);
	glNewList(nNewDisplayList, GL_COMPILE);
		DrawElevationGrid(this);
	glEndList();

	setDisplayList(nNewDisplayList);
}

#endif
