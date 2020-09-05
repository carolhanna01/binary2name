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
*	File:	Node.cpp
*
******************************************************************/

#include "SceneGraph.h"

////////////////////////////////////////////////
//	LodNode::update
////////////////////////////////////////////////

void UpdateLod(LodNode *lod)
{
	int nNodes = lod->getNPrivateNodeElements();
	for (int n=0; n<nNodes; n++) {
		Node *node = lod->getPrivateNodeElementAt(n);
		node->remove();
	}

	SceneGraph *sg = lod->getSceneGraph();

	ViewpointNode *vpoint = sg->getViewpointNode();
	if (vpoint == NULL)
		vpoint = sg->getDefaultViewpointNode();

	if (vpoint) {
		SFMatrix	viewMatrix;
		float		viewPosition[3];
		vpoint->getTransformMatrix(&viewMatrix);
		vpoint->getPosition(viewPosition);
		viewMatrix.multi(viewPosition);

		SFMatrix	lodMatrix;
		float		lodCenter[3];
		lod->getTransformMatrix(&lodMatrix);
		lod->getCenter(lodCenter);
		lodMatrix.multi(lodCenter);

		float lx = lodCenter[0] - viewPosition[0];
		float ly = lodCenter[1] - viewPosition[1];
		float lz = lodCenter[2] - viewPosition[2];
		float distance = (float)sqrt(lx*lx + ly*ly + lz*lz);

		int nRange = lod->getNRanges();
		int n; // JMC
		for (n=0; n<nRange; n++) { //JMC
			if (distance < lod->getRange(n))
				break;
		}

		Node *node = lod->getPrivateNodeElementAt(n);
		if (!node)
			node = lod->getPrivateNodeElementAt(lod->getNPrivateNodeElements() - 1);
		assert(node);
		lod->addChildNode(node);
	}
}

void LodNode::update() 
{
	UpdateLod(this);
}

////////////////////////////////////////////////
//	LodNode::initialize
////////////////////////////////////////////////

void InitializeLod(LodNode *lod)
{
	lod->uninitialize();

	//	int nNode = lod->getNChildNodes(); JMC
	Node *node = lod->getChildNodes();
	while (node) {
		Node *nextNode = node->next();
//		node->remove();
		lod->addPrivateNodeElement(node);
		node = nextNode;
	}
/*
	Node *firstNode = lod->getPrivateNodeElementAt(0);
	if (firstNode)
		lod->addChildNode(firstNode);
*/
	//	int nNodes = lod->getNPrivateNodeElements(); JMC
}

void LodNode::initialize() 
{
	if (isInitialized() == false) {
		InitializeLod(this);
		setInitialized(true);
	}
}

////////////////////////////////////////////////
//	LodNode::uninitialize
////////////////////////////////////////////////

void UninitializeLod(LodNode *lod) 
{
	int nNodes = lod->getNPrivateNodeElements();
	for (int n=0; n<nNodes; n++) {
		Node *node = lod->getPrivateNodeElementAt(n);
		node->remove();
		lod->addChildNode(node);
	}
	lod->removeAllNodeElement();
	//	int nNode = lod->getNChildNodes(); JMC
}

void LodNode::uninitialize() 
{
	if (isInitialized() == true) {
		UninitializeLod(this);
		setInitialized(false);
	}
}

