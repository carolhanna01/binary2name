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
*	File:	JVector.h
*
******************************************************************/

#ifndef _JVECTOR_H_
#define _JVECTOR_H_
 
#include "CLinkedList.h"

template <class T>
class JVectorElement : public CLinkedListNode<T> {
	int	mbDeleteObject;
	T	*mObj;
public:
	JVectorElement() : CLinkedListNode<T>(1) {
		setObject(NULL);
	}
	JVectorElement(T *obj, bool bDeleteObject = true) : CLinkedListNode<T>(false) {
		mbDeleteObject = bDeleteObject;
		setObject(obj);
	}
	~JVectorElement() { 
		remove();
		if (mbDeleteObject)
			delete mObj;
	}
	void setObject(T *obj)	{
		mObj = obj;
	}
	T *getObject()	{
		return mObj;
	}
};

template <class T>
class JVector {
	CLinkedList<T>	 mElementList;
public:
	
	JVector() {
	}

	~JVector() {
		removeAllElements();
	}

	void addElement(T *obj, bool bDeleteObject = true) {
		JVectorElement<T> *element = new JVectorElement<T>(obj, bDeleteObject);
		mElementList.addNode(element);
	}

	int contains(void *elem) {
		for (int n=0; n<size(); n++) {
			if (elem == elementAt(n))
				return 1;
		}
		return 0;
	}

	T *elementAt(int index) {
		JVectorElement<T> *element = (JVectorElement<T> *)mElementList.getNode(index);
		return element ? element->getObject() : (T *) NULL; //JMC
	}
	
	T* nextElement() { // JMC
	  JVectorElement<T> *element = (JVectorElement<T> *)mElementList.getNextNode(); // JMC
	  return element ? element->getObject() : (T *) NULL; //JMC
	} // JMC
	
	T *firstElement() {
		JVectorElement<T> *element = (JVectorElement<T> *)mElementList.getNodes();
		return element ? element->getObject() : (T *) NULL; //JMC
	}

	int	indexOf(T *elem) {
		for (int n=0; n<size(); n++) {
			if (elem == elementAt(n))
				return n;
		}
		return -1;
	}

	int indexOf(T *elem, int index) {
		for (int n=index; n<size(); n++) {
			if (elem == elementAt(n))
				return n;
		}
		return -1;
	}

	void	insertElementAt(T *obj, int index, bool bDeleteObject = true) {
		JVectorElement<T> *element = (JVectorElement<T> *)mElementList.getNode(index);
		if (element) {
			JVectorElement<T> *newElement = new JVectorElement<T>(obj, bDeleteObject);
			newElement->insert((JVectorElement<T> *)element->prev());
		}
	}

	bool isEmpty() {
		return mElementList.getNodes() ? false : true;
	}

	T *lastElement() {
		JVectorElement<T> *element = (JVectorElement<T> *)mElementList.getNode(size()-1);
		return element ? element->getObject() : (T *) NULL; //JMC
	}

	int	lastIndexOf(T *elem);
	int	lastIndexOf(T *elem, int index);

	void removeAllElements() {
		mElementList.deleteNodes();
	}

	void removeElement(T *obj) {
		removeElementAt(indexOf(obj));
	}

	void removeElementAt(int index) {
		JVectorElement<T> *element = (JVectorElement<T> *)mElementList.getNode(index);
		if (element)
			delete element;
	}

	void setElementAt(T *obj, int index) {
		JVectorElement<T> *element = (JVectorElement<T> *)mElementList.getNode(index);
		if (element)  
			element->setObject(obj);
	}

	int	size() {
		return mElementList.getNNodes();
	}
};

#endif 
