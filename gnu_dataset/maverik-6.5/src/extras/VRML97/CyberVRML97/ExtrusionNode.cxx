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
*	File:	ExtrusionNode.cpp
*
******************************************************************/

#ifdef SUPPORT_OPENGL
#ifdef WIN32
#include <windows.h>
#endif
#include <GL/gl.h>
#include <GL/glaux.h>
#endif

#include "ExtrusionNode.h"

////////////////////////////////////////////////
//	GroupingNode::recomputeBoundingBox
////////////////////////////////////////////////

static void AddDefaultParameters(ExtrusionNode *ex)
{
	if (ex->getNCrossSections() == 0) {
		ex->addCrossSection(1.0f, 1.0);
		ex->addCrossSection(1.0f, -1.0);
		ex->addCrossSection(-1.0f, -1.0);
		ex->addCrossSection(-1.0f, 1.0);
		ex->addCrossSection(1.0f, 1.0);
	}
	if (ex->getNSpines() == 0) {
		ex->addSpine(0.0f, 0.0f, 0.0f);
		ex->addSpine(0.0f, 1.0f, 0.0f);
	}
}

void ExtrusionNode::initialize() 
{
	if (!isInitialized()) {
		AddDefaultParameters(this);
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

void ExtrusionNode::recomputeBoundingBox()
{
}

////////////////////////////////////////////////
//	GroupingNode::recomputeBoundingBox
////////////////////////////////////////////////

#ifdef SUPPORT_OPENGL

void GetNormalFromVertices(float vpoint[3][3], float vector[3]);

static void diff(
float	result[3], 
float	value1[3], 
float	value2[3] )
{
	result[0] = value1[0] - value2[0];
	result[1] = value1[1] - value2[1];
	result[2] = value1[2] - value2[2];
}

static void cross(
float	result[3], 
float	value1[3], 
float	value2[3] )
{
	result[0] = value1[1]*value2[2] - value1[2]*value2[1];
	result[1] = value1[2]*value2[0] - value1[0]*value2[2];
	result[2] = value1[0]*value2[1] - value1[1]*value2[0];
}

static float length(
float	value[3] )
{
	return (float)sqrt(value[0]*value[0]+value[1]*value[1]+value[2]*value[2]);
}

static bool equals(
float	value1[3], 
float	value2[3] )
{
	if (value1[0] == value2[0] && value1[1] == value2[1] && value1[2] == value2[2])
		return true;
	else
		return false;
}

static void initializePoint(ExtrusionNode *ex, SFVec3f *point)
{
	int nCrossSections = ex->getNCrossSections();
	for (int n=0; n<nCrossSections; n++) {
		float cs[2];
		ex->getCrossSection(n, cs);
		point[n].setValue(cs[0], 0.0f, cs[1]);
	}
}

static void transformPoint(SFVec3f *point, float scale[2], float scp[3][3], float orientation[4], float spine[3])
{
	point->scale(scale[0], 1.0f, scale[1]);
	
	float value[3];
	point->getValue(value);

	if (0.0f < length(scp[0]) && 0.0f < length(scp[1]) && 0.0f < length(scp[2])) {
		float x = value[0]*scp[0][0]+value[1]*scp[1][0]+value[2]*scp[2][0];
		float y = value[0]*scp[0][1]+value[1]*scp[1][1]+value[2]*scp[2][1];
		float z = value[0]*scp[0][2]+value[1]*scp[1][2]+value[2]*scp[2][2];
		value[0] = x;
		value[1] = y;
		value[2] = z;
	}

	point->setValue(value);

	point->translate(spine);
	point->rotate(orientation);
}

static void DrawExtrusion(ExtrusionNode *ex)
{
	bool ccw = ex->getCCW();
	if (ccw == true)
		glFrontFace(GL_CCW);
	else
		glFrontFace(GL_CW);

	bool solid = ex->getSolid();
//	if (solid == false)
		glDisable(GL_CULL_FACE);
//	else
//		glEnable(GL_CULL_FACE);

	int nCrossSections = ex->getNCrossSections();

	SFVec3f *point[2];
	point[0] = new SFVec3f[nCrossSections];
	point[1] = new SFVec3f[nCrossSections];

	int		nOrientations	= ex->getNOrientations();
	int		nScales			= ex->getNScales();
	int		nSpines			= ex->getNSpines();

	float	spineStart[3];
	float	spineEnd[3];
	bool	bClosed;

	ex->getSpine(0,			spineStart);
	ex->getSpine(nSpines-1, spineEnd);
	bClosed = equals(spineStart, spineEnd);
	
	float	scale[2];
	float	orientation[4];
	float	spine[3];
	float	scp[3][3];

	for (int n=0; n<(nSpines-1); n++) {
		initializePoint(ex, point[0]);
		initializePoint(ex, point[1]);

		for (int i=0; i<2; i++) {
			
			if (nScales == 1)
				ex->getScale(0, scale);
			else  if ((n+i) < nScales) 
				ex->getScale(n+i, scale);
			else {
				scale[0] = 1.0f; 
				scale[1] = 1.0f;
			}

			if (nOrientations == 1)
				ex->getOrientation(0, orientation);
			else if ((n+i) < nOrientations)
				ex->getOrientation(n+i, orientation);
			else {
				orientation[0] = 0.0f; 
				orientation[1] = 0.0f; 
				orientation[2] = 1.0f; 
				orientation[3] = 0.0f;
			}

			ex->getSpine(n+i, spine);

			// SCP Y
			float spine0[3], spine1[3], spine2[3];
			if (bClosed && (n+i == 0 || n+i == (nSpines-1))) {
				ex->getSpine(1,			spine1);
				ex->getSpine(nSpines-2,	spine2);
			}
			else if (n+i == 0) {
				ex->getSpine(0, spine1);
				ex->getSpine(1, spine2);
			}
			else if (n+i == (nSpines-1)) {
				ex->getSpine(nSpines-2, spine1);
				ex->getSpine(nSpines-1, spine2);
			}
			else {
				ex->getSpine(n+i+1, spine1);
				ex->getSpine(n+i-1, spine2);
			}
			diff(scp[1], spine1, spine2);

			// SCP Z
			float v1[3], v2[3];
			if (bClosed && (n+i == 0 || n+i == (nSpines-1))) {
				ex->getSpine(0,			spine0);
				ex->getSpine(nSpines-2,	spine1);
				ex->getSpine(1,			spine2);
			}
			else if (n+i == 0) {
				ex->getSpine(1,	spine0);
				ex->getSpine(0,	spine1);
				ex->getSpine(2,	spine2);
			}
			else if (n+i == (nSpines-1)) {
				ex->getSpine(nSpines-2, spine1);
				ex->getSpine(nSpines-3, spine1);
				ex->getSpine(nSpines-1, spine2);
			}
			else {
				ex->getSpine(n+i,	spine0);
				ex->getSpine(n+i-1,	spine1);
				ex->getSpine(n+i+1,	spine2);
			}
			diff(v1, spine1, spine0);
			diff(v2, spine2, spine0);
			cross(scp[2], v1, v2);

			// SCP X
			cross(scp[0], scp[1], scp[2]);

			for (int j=0; j<nCrossSections; j++)  
				transformPoint(&point[i][j], scale, scp, orientation, spine);
		}

		for (int k=0; k<nCrossSections-1; k++) {

			float	vpoint[3][3];
			float	normal[3];
			
			point[1][k].getValue(vpoint[0]); 
			point[0][k].getValue(vpoint[1]);
			point[1][(k+1)%nCrossSections].getValue(vpoint[2]);
			GetNormalFromVertices(vpoint, normal);
			glNormal3fv(normal);
			
			SFVec3f	*vertex;

			glBegin(GL_POLYGON);
			vertex = &point[1][(k+1)%nCrossSections];	glVertex3f(vertex->getX(), vertex->getY(), vertex->getZ());
			vertex = &point[1][k];						glVertex3f(vertex->getX(), vertex->getY(), vertex->getZ());
			vertex = &point[0][k];						glVertex3f(vertex->getX(), vertex->getY(), vertex->getZ());
			glEnd();

			glBegin(GL_POLYGON);
			vertex = &point[0][(k+1)%nCrossSections];	glVertex3f(vertex->getX(), vertex->getY(), vertex->getZ());
			vertex = &point[1][(k+1)%nCrossSections];	glVertex3f(vertex->getX(), vertex->getY(), vertex->getZ());
			vertex = &point[0][k];						glVertex3f(vertex->getX(), vertex->getY(), vertex->getZ());
			glEnd();
		}

		if (n==0 && ex->getBeginCap() == true) {
			glBegin(GL_POLYGON);
			for (int k=0; k<nCrossSections; k++)
				glVertex3f(point[0][k].getX(), point[0][k].getY(), point[0][k].getZ());
			glEnd();
		}

		if (n==(nSpines-1)-1 && ex->getEndCap() == true) {
			glBegin(GL_POLYGON);
			for (int k=0; k<nCrossSections; k++)
				glVertex3f(point[1][k].getX(), point[1][k].getY(), point[1][k].getZ());
			glEnd();
		}
	}

	if (ccw == false)
		glFrontFace(GL_CCW);

//	if (solid == false)
		glEnable(GL_CULL_FACE);

	delete []point[0];
	delete []point[1];
}		

void ExtrusionNode::recomputeDisplayList()
{
	unsigned int nCurrentDisplayList = getDisplayList();
	if (0 < nCurrentDisplayList)
		glDeleteLists(nCurrentDisplayList, 1);

	unsigned int nNewDisplayList = glGenLists(1);
	glNewList(nNewDisplayList, GL_COMPILE);
		DrawExtrusion(this);
	glEndList();

	setDisplayList(nNewDisplayList);
}

#endif

