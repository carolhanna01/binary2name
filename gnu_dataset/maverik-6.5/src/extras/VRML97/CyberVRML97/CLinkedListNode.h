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
*	File:	CLinkedListNode.h
*
******************************************************************/

#ifndef _CLINKEDLISTNODE_H_
#define _CLINKEDLISTNODE_H_

#include <stdio.h>

template <class T>
class CLinkedListNode {

public:

	bool			mbHeader;
	CLinkedListNode	*mPrevNode;
	CLinkedListNode	*mNextNode;

	CLinkedListNode () {
		setHeaderFlag(0);
		mPrevNode = mNextNode = this;
	}

	CLinkedListNode (bool header) {
		setHeaderFlag(header);
		mPrevNode = mNextNode = this;
	}

	CLinkedListNode (CLinkedListNode *prevNode) {
		setHeaderFlag(0);
		insert(prevNode);
	}

	virtual ~CLinkedListNode(void)
	{
		remove();
	}

	bool isHeaderNode() {
		return mbHeader;
	}

	void setHeaderFlag(bool bHeader) {
		mbHeader = bHeader;
	}

	T *next() {
		if (mNextNode->isHeaderNode())
			return NULL;
		else
			return (T *)mNextNode;
	}

	T* prev() {
		return (T *)mPrevNode;
	}

	T* nextCircular() {
		if (mNextNode->isHeaderNode())
			return (T *)mNextNode->mNextNode;
		else
			return (T *)mNextNode;
	}

	T *prevCircular() {
		if (mPrevNode->isHeaderNode())
			return (T *)mPrevNode->mPrevNode;
		else
			return (T *)mPrevNode;
	}

	void insert(CLinkedListNode *prevNode)
	{
		remove();

		mPrevNode = prevNode;
		mNextNode = prevNode->mNextNode;
		prevNode->mNextNode->mPrevNode = this;
		prevNode->mNextNode = this;
	}

	void remove() {
		mNextNode->mPrevNode = mPrevNode;
		mPrevNode->mNextNode = mNextNode;
		mPrevNode = mNextNode = this;
	}
};

#endif //JMC
