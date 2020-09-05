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


#include "mavlib_cvcomp.h"

/******************************************************************
*
*	VRML library for C++
*
*	Copyright (C) Satoshi Konno 1996-1997
*
*	File:	GroupingNode.h
*
******************************************************************/

#ifndef _GROUPINGNODE_H_
#define _GROUPINGNODE_H_

#include "vrmlfields.h"
#include "Node.h"
#include "BoundingBox.h"

#define	addChildrenEventIn			"addChildren"
#define	removeChildrenEventIn		"removeChildren"
#define	bboxCenterFieldName			"bboxCenter"
#define	bboxSizeFieldName			"bboxSize"

class GroupingNode : public Node {

public:

	GroupingNode() {
		setHeaderFlag(false);
/*
		// addChildren eventout field
		MFNode addNodes = new MFNode();
		addEventIn(addChildrenEventIn, addNodes);

		// removeChildren eventout field
		MFNode removeChildren = new MFNode();
		addEventIn(removeChildrenEventIn, removeChildren);
*/
		// bboxCenter field
		SFVec3f *bboxCenter = new SFVec3f(0.0f, 0.0f, 0.0f);
		bboxCenter->setName(bboxCenterFieldName);
		addField(bboxCenter);

		// bboxSize field
		SFVec3f *bboxSize = new SFVec3f(-1.0f, -1.0f, -1.0f);
		bboxSize->setName(bboxSizeFieldName);
		addField(bboxSize);
	}

	virtual ~GroupingNode() {
	}

	////////////////////////////////////////////////
	//	BoundingBoxSize
	////////////////////////////////////////////////

	void setBoundingBoxSize(float value[]) {
		SFVec3f *bboxSize = (SFVec3f *)getField(bboxSizeFieldName);
		bboxSize->setValue(value);
	}
	void setBoundingBoxSize(float x, float y, float z) {
		SFVec3f *bboxSize = (SFVec3f *)getField(bboxSizeFieldName);
		bboxSize->setValue(x, y, z);
	}
	void getBoundingBoxSize(float value[]) {
		SFVec3f *bboxSize = (SFVec3f *)getField(bboxSizeFieldName);
		bboxSize->getValue(value);
	}

	////////////////////////////////////////////////
	//	BoundingBoxCenter
	////////////////////////////////////////////////

	void setBoundingBoxCenter(float value[]) {
		SFVec3f *bboxCenter = (SFVec3f *)getField(bboxCenterFieldName);
		bboxCenter->setValue(value);
	}
	void setBoundingBoxCenter(float x, float y, float z) {
		SFVec3f *bboxCenter = (SFVec3f *)getField(bboxCenterFieldName);
		bboxCenter->setValue(x, y, z);
	}
	void getBoundingBoxCenter(float value[]) {
		SFVec3f *bboxCenter = (SFVec3f *)getField(bboxCenterFieldName);
		bboxCenter->getValue(value);
	}

	////////////////////////////////////////////////
	//	BoundingBox
	////////////////////////////////////////////////

	void setBoundingBox(BoundingBox *bbox) {
		float center[3];
		float size[3];
		bbox->getCenter(center);
		bbox->getSize(size);
		setBoundingBoxCenter(center);
		setBoundingBoxSize(size);
	}

	void recomputeBoundingBox();

	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	GroupingNode *next() {
		for (Node *node = Node::next(); node != NULL; node = node->next()) {
			if (node->isGroupNode() || node->isTransformNode() || node->isBillboardNode() || node->isCollisionNode() || node->isLodNode() || node->isSwitchNode() || node->isInlineNode())
				return (GroupingNode *)node;
		}
		return NULL;
	}

	GroupingNode *nextTraversal() {
		for (Node *node = Node::nextTraversal(); node != NULL; node = node->nextTraversal()) {
			if (node->isGroupNode() || node->isTransformNode() || node->isBillboardNode() || node->isCollisionNode() || node->isLodNode() || node->isSwitchNode() || node->isInlineNode())
				return (GroupingNode *)node;
		}
		return NULL;
	}
};

#endif

