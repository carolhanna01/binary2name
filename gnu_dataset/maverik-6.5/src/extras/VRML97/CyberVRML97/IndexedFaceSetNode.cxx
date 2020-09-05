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
*	File:	IndexedFaceSetNode.cpp
*
******************************************************************/

#ifdef SUPPORT_OPENGL
#ifdef WIN32
#include <windows.h>
#endif
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glaux.h>
#endif

#include "SceneGraph.h"
#include "BoundingBox.h"

////////////////////////////////////////////////////////////
//	IndexedFaceSetNode::generateNormals
////////////////////////////////////////////////////////////

void GetNormalFromVertices(float point[3][3], float vector[3])
{
	vector[0] = (point[1][1] - point[0][1])*(point[2][2] - point[1][2]) - (point[1][2] - point[0][2])*(point[2][1] - point[1][1]);
	vector[1] = (point[1][2] - point[0][2])*(point[2][0] - point[1][0]) - (point[1][0] - point[0][0])*(point[2][2] - point[1][2]);
	vector[2] = (point[1][0] - point[0][0])*(point[2][1] - point[1][1]) - (point[1][1] - point[0][1])*(point[2][0] - point[1][0]);
	float mag = (float)sqrt(vector[0]*vector[0] + vector[1]*vector[1] + vector[2]*vector[2]);
	if (mag != 0.0f) {
		vector[0] /= mag;
		vector[1] /= mag;
		vector[2] /= mag;
	}
	else {
		vector[0] = 0.0f;
		vector[1] = 0.0f;
		vector[2] = 1.0f;
	}
}

bool IndexedFaceSetNode::generateNormals() 
{
	NormalNode *normal = getNormalNodes();
	if (normal)
		return false;

	CoordinateNode *coordinateNode = getCoordinateNodes();
	if (!coordinateNode)
		return false;

	normal = new NormalNode();

	int		nPolygon = 0;
	int		nVertex = 0;
	float	point[3][3];
	float	vector[3];

	int		nCoordIndexes = getNCoordIndexes();

	for (int nCoordIndex=0; nCoordIndex<nCoordIndexes; nCoordIndex++) {

		int coordIndex = getCoordIndex(nCoordIndex);

		if (coordIndex != -1) {
			if (nVertex < 3)
				coordinateNode->getPoint(coordIndex, point[nVertex]);
			nVertex++;
		}
		else {
			GetNormalFromVertices(point, vector);
			normal->addVector(vector);

			nVertex = 0;
			nPolygon++;
		}
	}

	addChildNode(normal);

	setNormalPerVertex(false);

	return true;
}

////////////////////////////////////////////////////////////
//	IndexedFaceSetNode::recomputeBoundingBox
////////////////////////////////////////////////////////////

void IndexedFaceSetNode::recomputeBoundingBox() 
{
	CoordinateNode *coordinateNode = getCoordinateNodes();
	if (!coordinateNode) {
		setBoundingBoxCenter(0.0f, 0.0f, 0.0f);
		setBoundingBoxSize(-1.0f, -1.0f, -1.0f);
		return;
	}

	BoundingBox bbox;
	float		point[3];

	int nPoints = coordinateNode->getNPoints();
	for (int n=0; n<nPoints; n++) {
		coordinateNode->getPoint(n, point);
		bbox.addPoint(point);
	}

	setBoundingBox(&bbox);
}

////////////////////////////////////////////////
//	IndexedFaceSetNode::initialize
////////////////////////////////////////////////

void IndexedFaceSetNode::initialize() 
{
	if (!getSceneGraph())
		return;

	if (!isInitialized()) {
		if (getSceneGraph()->getOption() & SCENEGRAPH_NORMAL_GENERATION)
			generateNormals();

		if (getSceneGraph()->getOption() & SCENEGRAPH_TEXTURE_GENERATION) {
			Node *parentNode = getParentNode();
			if (parentNode) {
				AppearanceNode *appearance = parentNode->getAppearanceNodes();
				if (appearance) {
					if (appearance->getTextureNode())
						generateTextureCoordinate();
				}
			}
		}

#ifdef SUPPORT_OPENGL
		recomputeDisplayList();
#endif
		//recomputeBoundingBox();JMC
		setInitialized(true);
	}
}

////////////////////////////////////////////////
//	IndexedFaceSetNode::recomputeDisplayList
////////////////////////////////////////////////

#ifdef SUPPORT_OPENGL

static void DrawIdxFaceSet(IndexedFaceSetNode *idxFaceSet)
{
	CoordinateNode *coordinateNode = idxFaceSet->getCoordinateNodes();
	if (!coordinateNode)
		return;

	TextureCoordinateNode	*texCoordNode	= idxFaceSet->getTextureCoordinateNodes();
	NormalNode				*normalNode		= idxFaceSet->getNormalNodes();
	ColorNode				*colorNode		= idxFaceSet->getColorNodes();

	bool colorPerVertex =idxFaceSet->getColorPerVertex();
	bool normalPerVertex =idxFaceSet->getNormalPerVertex();

	bool ccw = idxFaceSet->getCCW();
	if (ccw == true)
		glFrontFace(GL_CCW);
	else
		glFrontFace(GL_CW);

	bool solid = idxFaceSet->getSolid();
	if (solid == false)
		glDisable(GL_CULL_FACE);
	else
		glEnable(GL_CULL_FACE);

	bool convex = idxFaceSet->getConvex();
	GLUtriangulatorObj *tessObj = NULL;
	if (convex == false) {
		//drawScene();
		tessObj = gluNewTess();
		gluTessCallback(tessObj, GLU_BEGIN,		(GLUtessEndProc)glBegin);
		gluTessCallback(tessObj, GLU_VERTEX,	(GLUtessEndProc)glVertex3dv);
		gluTessCallback(tessObj, GLU_END,		(GLUtessEndProc)glEnd);

	}

	bool	bPolygonBegin = true;
	bool	bPolygonClose = true;
	
	int		nPolygon	= 0;
	int		nVertex		= 0;

	float	point[3];
	float	vector[3];
	float	color[4]; color[3] = 1.0f;
	float	coord[2];
	double	(*tessPoint)[3];

	if ((idxFaceSet->getColorPerVertex() && idxFaceSet->getColorNodes()) || (idxFaceSet->getNormalPerVertex() && idxFaceSet->getNormalNodes()))
		glShadeModel (GL_SMOOTH);
	else
		glShadeModel (GL_FLAT);


	int nColorIndexes = idxFaceSet->getNColorIndexes();
	int nNormalIndexes = idxFaceSet->getNNormalIndexes();
	int nTexCoordIndexes = idxFaceSet->getNTexCoordIndexes();
	int nCoordIndexes = idxFaceSet->getNCoordIndexes();

	for (int nCoordIndex=0; nCoordIndex<nCoordIndexes; nCoordIndex++) {

		int coordIndex = idxFaceSet->getCoordIndex(nCoordIndex);

		if (bPolygonBegin) {

			if (convex == false) 
				gluBeginPolygon(tessObj);
			else
				glBegin(GL_POLYGON);

			bPolygonBegin = false;
			bPolygonClose = false;

			int nVertices = 0;
			int index = coordIndex;
			while (index != -1) {
				nVertices++;
				int nIndex = nCoordIndex + nVertices;
				if (nIndex < nCoordIndexes)
					index = idxFaceSet->getCoordIndex(nIndex);
				else
					break;
			}

			if (convex == false)
				tessPoint = new double[nVertices][3];

			// dafault color
			//glColor3f(1.0f, 1.0f, 1.0f);

			// default normal
			if ((nCoordIndex + 2) < nCoordIndexes) {
				float point[3][3];
				float normal[3];
				for (int n=0; n<3; n++) {
					int index = idxFaceSet->getCoordIndex(nCoordIndex+n);
					coordinateNode->getPoint(index, point[n]);
				}
				GetNormalFromVertices(point, normal);
				glNormal3fv(normal);
			}
			else
				glNormal3f(0.0f, 0.0f, 1.0f);

			if (colorNode && !colorPerVertex) {
				if (0 < nColorIndexes)
					colorNode->getColor(idxFaceSet->getColorIndex(nPolygon), color);
				else
					colorNode->getColor(nPolygon, color);
				glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, color);
//				glColor3fv(color);
			}

			if (normalNode && !normalPerVertex) {
				if (0 < nNormalIndexes)
					normalNode->getVector(idxFaceSet->getNormalIndex(nPolygon), vector);
				else
					normalNode->getVector(nPolygon, vector);
				glNormal3fv(vector);
			}

			nPolygon++;
			nVertex = 0;
		}

		if (coordIndex != -1) {

			if (colorNode && colorPerVertex) {
				if (0 < nColorIndexes)
					colorNode->getColor(idxFaceSet->getColorIndex(nCoordIndex), color);
				else
					colorNode->getColor(coordIndex, color);
				glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, color);
//				glColor3fv(color);
			}

			if (normalNode && normalPerVertex) {
				if (0 < nNormalIndexes)
					normalNode->getVector(idxFaceSet->getNormalIndex(nCoordIndex), vector);
				else
					normalNode->getVector(coordIndex, vector);
				glNormal3fv(vector);
			}

			if (texCoordNode) {
				if (0 < nTexCoordIndexes)
					texCoordNode->getPoint(idxFaceSet->getTexCoordIndex(nCoordIndex), coord);
				else
					texCoordNode->getPoint(coordIndex, coord);
				coord[1] = 1.0f - coord[1];
				glTexCoord2fv(coord);
			}

			coordinateNode->getPoint(coordIndex, point);
			if (convex == false) { 
				tessPoint[nVertex][0] = point[0];
				tessPoint[nVertex][1] = point[1];
				tessPoint[nVertex][2] = point[2];
				gluTessVertex(tessObj, tessPoint[nVertex], tessPoint[nVertex]);
			}
			else
				glVertex3fv(point);

			nVertex++;
		}
		else {
			if (convex == false)  {
				gluEndPolygon(tessObj);
				delete[] tessPoint;
			}
			else
				glEnd();
			bPolygonBegin = true;
			bPolygonClose = true;
		}
	}

	if (bPolygonClose == false) {
		if (convex == false) { 
			gluEndPolygon(tessObj);
			delete[] tessPoint;
		}	
		else
			glEnd();
	}

	if (ccw == false)
		glFrontFace(GL_CCW);

	if (solid == false)
		glEnable(GL_CULL_FACE);

	if (convex == false)
		gluDeleteTess(tessObj);

	glShadeModel(GL_SMOOTH);
}

void IndexedFaceSetNode::recomputeDisplayList() 
{
	CoordinateNode *coordinateNode = getCoordinateNodes();
	if (!coordinateNode)
		return;

	unsigned int nCurrentDisplayList = getDisplayList();
	if (0 < nCurrentDisplayList)
		glDeleteLists(nCurrentDisplayList, 1);

	unsigned int nNewDisplayList = glGenLists(1);
	glNewList(nNewDisplayList, GL_COMPILE);
		DrawIdxFaceSet(this);
	glEndList();

	setDisplayList(nNewDisplayList);
}

#endif

////////////////////////////////////////////////////////////
//	IndexedFaceSetNode::getNPolygons
////////////////////////////////////////////////////////////

int IndexedFaceSetNode::getNPolygons() 
{
	CoordinateNode *coordinateNode = getCoordinateNodes();
	if (!coordinateNode)
		return 0;

	int nCoordIndexes = getNCoordIndexes();
	int nCoordIndex = 0;
	for (int n=0; n<nCoordIndexes; n++) {
		if (getCoordIndex(n) == -1)
			nCoordIndex++;
		else if (n == (nCoordIndexes-1))
			nCoordIndex++;
	}
	return nCoordIndex;
}

////////////////////////////////////////////////////////////
//	IndexedFaceSetNode::generateTextureCoordinate
////////////////////////////////////////////////////////////

static void GetRotateMatrixFromNormal(
float		normal[3],
SFMatrix	&matrix)
{
	SFMatrix	mx;
	SFMatrix	my;
	float		mxValue[4][4];
	float		myValue[4][4];

	mx.getValue(mxValue);
	my.getValue(myValue);

	float d = (float)sqrt(normal[1]*normal[1] + normal[2]*normal[2]);

	if (d) {
		float cosa = normal[2] / d;
		float sina = normal[1] / d;
		mxValue[0][0] = 1.0;
		mxValue[0][1] = 0.0;
		mxValue[0][2] = 0.0;
		mxValue[1][0] = 0.0;
		mxValue[1][1] = cosa;
		mxValue[1][2] = sina;
		mxValue[2][0] = 0.0;
		mxValue[2][1] = -sina;
		mxValue[2][2] = cosa;
	}
	
	float cosb = d;
	float sinb = normal[0];
	
	myValue[0][0] = cosb;
	myValue[0][1] = 0.0;
	myValue[0][2] = sinb;
	myValue[1][0] = 0.0;
	myValue[1][1] = 1.0;
	myValue[1][2] = 0.0;
	myValue[2][0] = -sinb;
	myValue[2][1] = 0.0;
	myValue[2][2] = cosb;

	mx.setValue(mxValue);
	my.setValue(myValue);

	matrix.init();
	matrix.add(&my);
	matrix.add(&mx);
}

static void SetExtents(
SFVec3f	&maxExtents,
SFVec3f	&minExtents,
float	point[3])
{
	if (maxExtents.getX() < point[0])
		maxExtents.setX(point[0]);
	if (maxExtents.getY() < point[1])
		maxExtents.setY(point[1]);
	if (maxExtents.getZ() < point[2])
		maxExtents.setZ(point[2]);
	if (minExtents.getX() > point[0])
		minExtents.setX(point[0]);
	if (minExtents.getY() > point[1])
		minExtents.setY(point[1]);
	if (minExtents.getZ() > point[2])
		minExtents.setZ(point[2]);
}

bool IndexedFaceSetNode::generateTextureCoordinate() 
{
	TextureCoordinateNode *texCoord = getTextureCoordinateNodes();
	if (texCoord)
		return false;

	CoordinateNode *coordinateNode = getCoordinateNodes();
	if (!coordinateNode)
		return false;

	texCoord = new TextureCoordinateNode();

	int nPolygon = getNPolygons();

	if (nPolygon <= 0)
		return false;

	float	(*normal)[3] = new float[nPolygon][3];
	SFVec3f	*center = new SFVec3f[nPolygon];
	SFVec3f	*maxExtents = new SFVec3f[nPolygon];
	SFVec3f	*minExtents = new SFVec3f[nPolygon];

	bool	bPolygonBegin;
	int		polyn;

	float	point[3][3];
	float	coord[3];

	int		vertexn = 0;
	int		n;


	bPolygonBegin = true;
	polyn = 0;
/*
	int nColorIndexes = idxFaceSet->getNColorIndexes();
	int nNormalIndexes = idxFaceSet->getNNormalIndexes();
	int nTexCoordIndexes = idxFaceSet->getNTexCoordIndexes();
*/
	int nCoordIndexes = getNCoordIndexes();


	for (n=0; n<nCoordIndexes; n++) {
		int coordIndex = getCoordIndex(n);
		if (coordIndex != -1) {

			if (vertexn < 3)
				coordinateNode->getPoint(coordIndex, point[vertexn]);

			float point[3];
			coordinateNode->getPoint(coordIndex, point);
			if (bPolygonBegin) {
				maxExtents[polyn].setValue(point);
				minExtents[polyn].setValue(point);
				center[polyn].setValue(point);
				bPolygonBegin = false;
			}
			else {
				SetExtents(maxExtents[polyn], minExtents[polyn], point);
				center[polyn].add(point);
			}

			vertexn++;
		}
		else {
			GetNormalFromVertices(point, normal[polyn]);
			center[polyn].scale(1.0f / (float)vertexn);
			maxExtents[polyn].sub(center[polyn]);
			minExtents[polyn].sub(center[polyn]);
			vertexn = 0;
			bPolygonBegin = true;
			polyn++;
		}
	}

	float		minx=0, miny=0, xlength=0, ylength=0; //JMC
	SFMatrix	matrix;

	bPolygonBegin = true;
	polyn = 0;

	for (n=0; n<nCoordIndexes; n++) {
		int coordIndex = getCoordIndex(n);
		if (coordIndex != -1) {

			if (bPolygonBegin) {
				GetRotateMatrixFromNormal(normal[polyn], matrix);
				matrix.multi(&minExtents[polyn]);
				matrix.multi(&maxExtents[polyn]);
				minx = minExtents[polyn].getX();
				miny = minExtents[polyn].getY();
				//JMC maxx = maxExtents[polyn].getX();
				//JMC maxy = maxExtents[polyn].getY();
				xlength = (float)fabs(maxExtents[polyn].getX() - minExtents[polyn].getX());
				ylength = (float)fabs(maxExtents[polyn].getY() - minExtents[polyn].getY());

				if (xlength == 0.0f || ylength == 0.0f) {
					delete texCoord;
					delete []minExtents;
					delete []maxExtents;
					delete []center;
					delete []normal;
					return false;
				}

				bPolygonBegin = false;
			}

			coordinateNode->getPoint(coordIndex, coord);

			coord[0] -= center[polyn].getX();
			coord[1] -= center[polyn].getY();
			coord[2] -= center[polyn].getZ();

			matrix.multi(coord);

			coord[0] = (float)fabs(coord[0] - minx) / xlength;
			coord[1] = (float)fabs(coord[1] - miny) / ylength;

			texCoord->addPoint(coord);
		}
		else {
//			coord[0] = coord[1] = 0.0f;
//			texCoord->addPoint(coord);
			bPolygonBegin = true;
			polyn++;
		}
	}

	addChildNode(texCoord);

	delete []minExtents;
	delete []maxExtents;
	delete []center;
	delete []normal;

	return true;
}
