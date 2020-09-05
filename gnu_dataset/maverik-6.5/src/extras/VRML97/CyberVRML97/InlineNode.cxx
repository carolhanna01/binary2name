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
*	File:	InlineNode.cpp
*
******************************************************************/

#include "SceneGraph.h"

////////////////////////////////////////////////////////////
//	InlineNode::initialize
////////////////////////////////////////////////////////////

void InlineNode::initialize()
{
	if (isInstanceNode() == false && isInitialized() == false) {
		SceneGraph	sg;
		if (getSceneGraph() != NULL)
			sg.setOption(getSceneGraph()->getOption());

		int nUrls = getNUrls();
		for (int n=0; n<nUrls; n++) {
			sg.load(getUrl(n));
			Node *node = sg.getNodes();
			while (node) {
				Node *nextNode = node->next();
				moveChildNode(node);
				node = nextNode;
			}
			for (Route *route = sg.getRoutes(); route; route = route->next()) {
				getSceneGraph()->addRoute(route->getEventOutNode()->getName(), route->getEventOutField()->getName(),
											route->getEventInNode()->getName(), route->getEventInField()->getName());
			}
			sg.clear();
		}
		setInitialized(true);
	}
}

////////////////////////////////////////////////////////////
//	InlineNode::uninitialize
////////////////////////////////////////////////////////////

void InlineNode::uninitialize()
{
	Node *node=getChildNodes();
	while (node) {
		Node *nextNode = node->next();
		node->remove();
		delete node;
		node = nextNode;
	}
	setInitialized(false);
}
