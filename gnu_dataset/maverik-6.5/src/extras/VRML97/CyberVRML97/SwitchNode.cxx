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
*	File:	SwitchNode.h
*
******************************************************************/

#include "SwitchNode.h"

////////////////////////////////////////////////
//	SwitchNode::update
////////////////////////////////////////////////

void UpdateSwitch(SwitchNode *snode)
{
	int nNodes = snode->getNPrivateNodeElements();
	for (int n=0; n<nNodes; n++) {
		Node *node = snode->getPrivateNodeElementAt(n);
		node->remove();
	}
	Node *node = snode->getPrivateNodeElementAt(snode->getWhichChoice());
	if (node)
		snode->addChildNode(node);
}

void SwitchNode::update() 
{
	UpdateSwitch(this);
}
	
////////////////////////////////////////////////
//	SwitchNode::initialize
////////////////////////////////////////////////

void InitializeSwitch(SwitchNode *snode)
{
	snode->uninitialize();

	Node *node = snode->getChildNodes();
	while (node) {
		Node *nextNode = node->next();
//		node->remove();
		snode->addPrivateNodeElement(node);
		node = nextNode;
	}
/*
	Node *selectedNode = snode->getPrivateNodeElementAt(snode->getWhichChoice());
	if (selectedNode)
		snode->addChildNode(selectedNode);
*/
}

void SwitchNode::initialize() 
{
	if (isInitialized() == false) {
		InitializeSwitch(this);
		setInitialized(true);
	}
}

////////////////////////////////////////////////
//	SwitchNode::uninitialize
////////////////////////////////////////////////

void UninitializeSwitch(SwitchNode *snode)
{
	int nNodes = snode->getNPrivateNodeElements();
	for (int n=0; n<nNodes; n++) {
		Node *node = snode->getPrivateNodeElementAt(n);
		node->remove();
		snode->addChildNode(node);
	}
	snode->removeAllNodeElement();
}

void SwitchNode::uninitialize() 
{
	if (isInitialized() == true) {
		UninitializeSwitch(this);
		setInitialized(false);
	}
}

