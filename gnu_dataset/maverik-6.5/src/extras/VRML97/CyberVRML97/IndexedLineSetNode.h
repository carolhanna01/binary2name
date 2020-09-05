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
*	File:	IndexedLinSet.h
*
******************************************************************/

#ifndef _INDEXEDLINESET_H_
#define _INDEXEDLINESET_H_

#include "GeometryNode.h"
#include "ColorNode.h"
#include "CoordinateNode.h"

class IndexedLineSetNode : public GeometryNode {
	
public:

        MFInt32 *ci; // JMC
	IndexedLineSetNode() {
	        ci=NULL; // JMC
		setHeaderFlag(false);
		setType(indexedLineSetNodeString);

		///////////////////////////
		// Field 
		///////////////////////////

		// colorPerVertex  field
		SFBool *colorPerVertex = new SFBool(true);
		colorPerVertex->setName(colorPerVertexFieldString);
		addField(colorPerVertex);

		// coordIndex  field
		MFInt32 *coordIndex = new MFInt32();
		coordIndex->setName(coordIndexFieldString);
		addField(coordIndex);

		// colorIndex  field
		MFInt32 *colorIndex = new MFInt32();
		colorIndex->setName(colorIndexFieldString);
		addField(colorIndex);

		///////////////////////////
		// EventIn
		///////////////////////////

		// coordIndex  EventIn
		coordIndex = new MFInt32();
		coordIndex->setName(coordIndexFieldString);
		addEventIn(coordIndex);

		// colorIndex  EventIn
		colorIndex = new MFInt32();
		colorIndex->setName(colorIndexFieldString);
		addEventIn(colorIndex);
	}

	~IndexedLineSetNode() {
	}
	
	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	IndexedLineSetNode *next() {
		return (IndexedLineSetNode *)Node::next(getType());
	}

	IndexedLineSetNode *nextTraversal() {
		return (IndexedLineSetNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	ColorPerVertex
	////////////////////////////////////////////////
	
	void setColorPerVertex(bool value) {
		SFBool *colorPerVertex = (SFBool *)getField(colorPerVertexFieldString);
		colorPerVertex->setValue(value);
	}

	void setColorPerVertex(int value) {
		setColorPerVertex(value ? true : false);
	}

	bool getColorPerVertex() {
		SFBool *colorPerVertex = (SFBool *)getField(colorPerVertexFieldString);
		return colorPerVertex->getValue();
	}

	////////////////////////////////////////////////
	// CoordIndex
	////////////////////////////////////////////////

	void addCoordIndex(int value) {
		MFInt32 *coordIndex = (MFInt32 *)getField(coordIndexFieldString);
		coordIndex->addValue(value);
	}
	int getNCoordIndexes() {
		MFInt32 *coordIndex = (MFInt32 *)getField(coordIndexFieldString);
		return coordIndex->getSize();
	}
	int getCoordIndex(int index) {
		MFInt32 *coordIndex = (MFInt32 *)getField(coordIndexFieldString);
		return coordIndex->get1Value(index);
	}
	int getCoordIndexNext() { // JMC
	  if (!ci) ci = (MFInt32 *)getField(coordIndexFieldString); //JMC
	  return ci->get1ValueNext(); //JMC
	} //JMC
	void clearCoordIndex() {
		MFInt32 *coordIndex = (MFInt32 *)getField(coordIndexFieldString);
		coordIndex->clear();
	}
	
	////////////////////////////////////////////////
	// ColorIndex
	////////////////////////////////////////////////

	void addColorIndex(int value) {
		MFInt32 *colorIndex = (MFInt32 *)getField(colorIndexFieldString);
		colorIndex->addValue(value);
	}
	int getNColorIndexes() {
		MFInt32 *colorIndex = (MFInt32 *)getField(colorIndexFieldString);
		return colorIndex->getSize();
	}
	int getColorIndex(int index) {
		MFInt32 *colorIndex = (MFInt32 *)getField(colorIndexFieldString);
		return colorIndex->get1Value(index);
	}
	void clearColorIndex() {
		MFInt32 *colorIndex = (MFInt32 *)getField(colorIndexFieldString);
		colorIndex->clear();
	}

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		if (node->isColorNode() || node->isCoordinateNode())
			return true;
		else
			return false;
	}

	void initialize() {
		if (!isInitialized()) {
#ifdef SUPPORT_OPENGL
			recomputeDisplayList();
#endif
			recomputeBoundingBox();
			setInitialized(true);
		}
	}

	void uninitialize() {
	}

	void update() {
	}

	////////////////////////////////////////////////
	//	BoundingBox
	////////////////////////////////////////////////

	void recomputeBoundingBox();

	////////////////////////////////////////////////
	//	recomputeDisplayList
	////////////////////////////////////////////////

#ifdef SUPPORT_OPENGL
	void recomputeDisplayList();
#endif

	////////////////////////////////////////////////
	//	Infomation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		SFBool *colorPerVertex = (SFBool *)getField(colorPerVertexFieldString);

		printStream << indentString << "\t" << "colorPerVertex " << colorPerVertex << endl;

		ColorNode *color = getColorNodes();
		if (color != NULL) {
			if (color->isInstanceNode() == false) {
				if (color->getName() != NULL && strlen(color->getName()))
					printStream << indentString << "\t" << "color " << "DEF " << color->getName() << " Color {" << endl;
				else
					printStream << indentString << "\t" << "color Color {" << endl;
				color->Node::outputContext(printStream, indentString, "\t");
				printStream << indentString << "\t" << "}" << endl;
			}
			else 
				printStream << indentString << "\t" << "color USE " << color->getName() << endl;
		}

		CoordinateNode *coord = getCoordinateNodes();
		if (coord != NULL) {
			if (coord->isInstanceNode() == false) {
				if (coord->getName() != NULL && strlen(coord->getName()))
					printStream << indentString << "\t" << "coord " << "DEF " << coord->getName() << " Coordinate {" << endl;
				else
					printStream << indentString << "\t" << "coord Coordinate {" << endl;
				coord->Node::outputContext(printStream, indentString, "\t");
				printStream << indentString << "\t" << "}" << endl;
			}
			else 
				printStream << indentString << "\t" << "coord USE " << coord->getName() << endl;
		}

		if (0 < getNCoordIndexes()) {
			MFInt32 *coordIndex = (MFInt32 *)getField(coordIndexFieldString);
			printStream << indentString << "\t" << "coordIndex [" << endl;
			coordIndex->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}

		if (0 < getNColorIndexes()) {
			MFInt32 *colorIndex = (MFInt32 *)getField(colorIndexFieldString);
			printStream << indentString << "\t" << "colorIndex [" << endl;
			colorIndex->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
	}
};

#endif //JMC
