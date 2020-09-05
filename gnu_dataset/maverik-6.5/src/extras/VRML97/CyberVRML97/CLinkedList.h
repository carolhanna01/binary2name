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
*	File:	CLinkedList.h
*
******************************************************************/

#ifndef _CLINKEDLIST_H_
#define _CLINKEDLIST_H_

#include "CLinkedListNode.h"

template <class T>
class CLinkedList {

	CLinkedListNode<T>	*mHeaderNode;		
	CLinkedListNode<T>	*currentNode; // JMC

public:

	CLinkedList () {
		mHeaderNode = new CLinkedListNode<T>(1);
		currentNode = mHeaderNode; // JMC
	}

	~CLinkedList () {
		deleteNodes();
		delete mHeaderNode;
	}

	void setRootNode(CLinkedListNode<T> *obj) {
		mHeaderNode = obj;
	}

	T *getRootNode () {
		return (T *)mHeaderNode;
	}

	T *getNodes () {
		return (T *)mHeaderNode->next();
	}

	T *getNode (int number) {
		if (number < 0)
			return (T *)NULL;
		CLinkedListNode<T> *node = (CLinkedListNode<T> *)getNodes();
		for (int n=0; n<number && node; n++)
			node = (CLinkedListNode<T> *)node->next();
		return (T *)node;
	}

	T *getNextNode() { // JMC
	  currentNode= (CLinkedListNode<T> *)currentNode->nextCircular(); // JMC
	  return (T *) currentNode; // JMC
	} // JMC
	
	T *getLastNode () {
		CLinkedListNode<T> *lastNode = (CLinkedListNode<T> *)mHeaderNode->prev();
		if (lastNode->isHeaderNode())
			return NULL;
		else
			return (T *)lastNode;
	}

	int getNNodes()	{
		int n = 0;
		for (CLinkedListNode<T> *listNode = (CLinkedListNode<T> *)getNodes(); listNode; listNode = (CLinkedListNode<T> *)listNode->next())
			n++;
		return n;
	}

	void addNode(CLinkedListNode<T> *node) {
		node->remove();
		node->insert((CLinkedListNode<T> *)mHeaderNode->prev());
	}

	void addNodeAtFirst(CLinkedListNode<T> *node) {
		node->remove();
		node->insert(mHeaderNode);
	}

	void deleteNodes() {
		CLinkedListNode<T> *rootNode = (CLinkedListNode<T> *)getRootNode();
		if (!rootNode)
			return;
		while (rootNode->next())
			delete rootNode->mNextNode;
	}
};

#endif

