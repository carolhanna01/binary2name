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
*	File:	ProximitySensorNode.cpp
*
******************************************************************/

#include "SceneGraph.h"

////////////////////////////////////////////////
//	ProximitySensorNode::initialize
////////////////////////////////////////////////

void ProximitySensorNode::initialize() 
{
	setInRegion(false);
}

////////////////////////////////////////////////
//	ProximitySensorNode::uninitialize
////////////////////////////////////////////////

void ProximitySensorNode::uninitialize() 
{
}

////////////////////////////////////////////////
//	ProximitySensorNode::update
////////////////////////////////////////////////

static bool isRegion(float vpos[], float center[], float size[])
{
	for (int n=0; n<3; n++) {
		if (vpos[n] < center[n] - size[n]/2.0f)
			return false;
		if (center[n] + size[n]/2.0f < vpos[n])
			return false;
	}

	return true;
}
void ProximitySensorNode::update() 
{
	if (!isEnabled())
		return;

	SceneGraph *sg = getSceneGraph();
	if (!sg)
		return;

	ViewpointNode *vpoint = sg->getViewpointNode();
	if (vpoint == NULL)
		vpoint = sg->getDefaultViewpointNode();

	float vpos[3];
	vpoint->getPosition(vpos);

	float center[3];
	getCenter(center);

	float size[3];
	getSize(size);

	if (inRegion() == false) {
		if (isRegion(vpos, center, size) == true) {
			setInRegion(true);
			double time = GetCurrentSystemTime();
			setEnterTime(time);
			sendEvent(getEventOut(enterTimeFieldString));
			setIsActive(true);
			sendEvent(getEventOut(isActiveFieldString));
		}
	}
	else {
		if (isRegion(vpos, center, size) == false) {
			setInRegion(false);
			double time = GetCurrentSystemTime();
			setExitTime(time);
			sendEvent(getEventOut(exitTimeFieldString));
			setIsActive(false);
			sendEvent(getEventOut(isActiveFieldString));
		}
	}
}

