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
*	File:	GroupingNode.cpp
*
******************************************************************/

#include <float.h>
#include "GroupingNode.h"
#include "GeometryNode.h"

////////////////////////////////////////////////
//	GroupingNode::recomputeBoundingBox
////////////////////////////////////////////////

static void RecomputeExtents(
Node		*node,
BoundingBox	*bbox)
{
	if (node->isGeometryNode()) {
		GeometryNode *gnode = (GeometryNode *)node;
		gnode->recomputeBoundingBox();

		float	bboxCenter[3];
		float	bboxSize[3];

		gnode->getBoundingBoxCenter(bboxCenter);
		gnode->getBoundingBoxSize(bboxSize);

		SFMatrix	mx;
		gnode->getTransformMatrix(&mx);

		for (int n=0; n<8; n++) {
			float	point[3];
			point[0] = (n < 4)			? bboxCenter[0] - bboxSize[0] : bboxCenter[0] + bboxSize[0];
			point[1] = (n % 2)			? bboxCenter[1] - bboxSize[1] : bboxCenter[1] + bboxSize[1];
			point[2] = ((n % 4) < 2)	? bboxCenter[2] - bboxSize[2] : bboxCenter[2] + bboxSize[2];
			mx.multi(point);
			bbox->addPoint(point);
		}
	}

	for (Node *cnode=node->getChildNodes(); cnode; cnode=cnode->next()) 
		RecomputeExtents(cnode, bbox);
}

void GroupingNode::recomputeBoundingBox()
{
	BoundingBox bbox;

	for (Node *node=getChildNodes(); node; node=node->next()) 
		RecomputeExtents(node, &bbox);

	setBoundingBox(&bbox);
}

