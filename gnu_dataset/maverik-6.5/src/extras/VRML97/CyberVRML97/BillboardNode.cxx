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
*	File:	BillboardNode.cpp
*
******************************************************************/

#include "SceneGraph.h"

////////////////////////////////////////////////
//	BillboardNode::getBillboardNodeToViewerVector
////////////////////////////////////////////////

void BillboardNode::getBillboardToViewerVector(float vector[3])
{
	SceneGraph *sg = getSceneGraph();
	ViewpointNode *view = sg->getViewpointNode();
	if (view == NULL)
		view = sg->getDefaultViewpointNode();

	float		viewPos[3];
	view->getPosition(viewPos);

	float		bboardPos[] = {0.0f, 0.0f, 0.0f};
	SFMatrix	mx;

	getTransformMatrix(&mx);
	mx.multi(bboardPos);

	SFVec3f	resultVector(viewPos);
	resultVector.sub(bboardPos);
	resultVector.normalize();

	resultVector.getValue(vector);
}

////////////////////////////////////////////////
//	BillboardNode::getAxisOfRotationAndBillboardNodeToViewerPlaneVector
////////////////////////////////////////////////

void BillboardNode::getAxisOfRotationAndBillboardToViewerPlaneVector(float vector[3])
{
}

////////////////////////////////////////////////
//	BillboardNode::getZAxisVectorOnAxisRotationAndBillboardNodeToViewerPlane
////////////////////////////////////////////////

void BillboardNode::getZAxisVectorOnAxisRotationAndBillboardToViewerPlane(float vector[3])
{
}

////////////////////////////////////////////////
//	BillboardNode::getSFMatrix
////////////////////////////////////////////////

void BillboardNode::getSFMatrix(SFMatrix *mOut)
{
}
