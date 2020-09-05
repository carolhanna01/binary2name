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
*	File:	IndexedFaceSetNode.h
*
******************************************************************/

#ifndef _INDEXEDFACESET_H_
#define _INDEXEDFACESET_H_

#include "GeometryNode.h"
#include "NormalNode.h"
#include "ColorNode.h"
#include "CoordinateNode.h"
#include "TextureCoordinateNode.h"

void GetNormalFromVertices(float vpoint[3][3], float vector[3]);

class IndexedFaceSetNode : public GeometryNode {
	
public:

        MFInt32 *ci; // JMC

	IndexedFaceSetNode() {
	        ci=NULL; // JMC
	  
	        setHeaderFlag(false);
		setType(indexedFaceSetNodeString);

		///////////////////////////
		// Field 
		///////////////////////////

		// ccw  field
		SFBool *ccw = new SFBool(true);
		ccw->setName(ccwFieldString);
		addField(ccw);

		// colorPerVertex  field
		SFBool *colorPerVertex = new SFBool(true);
		colorPerVertex->setName(colorPerVertexFieldString);
		addField(colorPerVertex);

		// normalPerVertex  field
		SFBool *normalPerVertex = new SFBool(true);
		normalPerVertex->setName(normalPerVertexFieldString);
		addField(normalPerVertex);

		// solid  field
		SFBool *solid = new SFBool(true);
		solid->setName(solidFieldString);
		addField(solid);

		// convex  field
		SFBool *convex = new SFBool(true);
		convex->setName(convexFieldString);
		addField(convex);

		// creaseAngle  field
		SFFloat *creaseAngle = new SFFloat(0.0f);
		creaseAngle->setName(creaseAngleFieldString);
		addField(creaseAngle);

		// coordIndex  field
		MFInt32 *coordIndex = new MFInt32();
		coordIndex->setName(coordIndexFieldString);
		addField(coordIndex);

		// texCoordIndex  field
		MFInt32 *texCoordIndex = new MFInt32();
		texCoordIndex->setName(texCoordIndexFieldString);
		addField(texCoordIndex);

		// colorIndex  field
		MFInt32 *colorIndex = new MFInt32();
		colorIndex->setName(colorIndexFieldString);
		addField(colorIndex);

		// normalIndex  field
		MFInt32 *normalIndex = new MFInt32();
		normalIndex->setName(normalIndexFieldString);
		addField(normalIndex);

		///////////////////////////
		// EventIn
		///////////////////////////

		// coordIndex  EventIn
		coordIndex = new MFInt32();
		coordIndex->setName(coordIndexFieldString);
		addEventIn(coordIndex);

		// texCoordIndex  EventIn
		texCoordIndex = new MFInt32();
		texCoordIndex->setName(texCoordIndexFieldString);
		addEventIn(texCoordIndex);

		// colorIndex  EventIn
		colorIndex = new MFInt32();
		colorIndex->setName(colorIndexFieldString);
		addEventIn(colorIndex);

		// normalIndex  EventIn
		normalIndex = new MFInt32();
		normalIndex->setName(normalIndexFieldString);
		addEventIn(normalIndex);
	}

	~IndexedFaceSetNode() {
	}
	
	////////////////////////////////////////////////
	//	List
	////////////////////////////////////////////////

	IndexedFaceSetNode *next() {
		return (IndexedFaceSetNode *)Node::next(getType());
	}

	IndexedFaceSetNode *nextTraversal() {
		return (IndexedFaceSetNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	//	CCW
	////////////////////////////////////////////////
	
	void setCCW(bool value) {
		SFBool *ccw = (SFBool *)getField(ccwFieldString);
		ccw->setValue(value);
	}

	void setCCW(int value) {
		setCCW(value ? true : false);
	}

	bool getCCW() {
		SFBool *ccw = (SFBool *)getField(ccwFieldString);
		return ccw->getValue();
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
	//	NormalPerVertex
	////////////////////////////////////////////////
	
	void setNormalPerVertex(bool value) {
		SFBool *normalPerVertex = (SFBool *)getField(normalPerVertexFieldString);
		normalPerVertex->setValue(value);
	}

	void setNormalPerVertex(int value) {
		setNormalPerVertex(value ? true : false);
	}

	bool getNormalPerVertex() {
		SFBool *normalPerVertex = (SFBool *)getField(normalPerVertexFieldString);
		return normalPerVertex->getValue();
	}

	////////////////////////////////////////////////
	//	Solid
	////////////////////////////////////////////////
	
	void setSolid(bool value) {
		SFBool *solid = (SFBool *)getField(solidFieldString);
		solid->setValue(value);
	}

	void setSolid(int value) {
		setSolid(value ? true : false);
	}

	bool getSolid() {
		SFBool *solid = (SFBool *)getField(solidFieldString);
		return solid->getValue();
	}

	////////////////////////////////////////////////
	//	Convex
	////////////////////////////////////////////////
	
	void setConvex(bool value) {
		SFBool *convex = (SFBool *)getField(convexFieldString);
		convex->setValue(value);
	}

	void setConvex(int value) {
		setConvex(value ? true : false);
	}

	bool getConvex() {
		SFBool *convex = (SFBool *)getField(convexFieldString);
		return convex->getValue();
	}

	////////////////////////////////////////////////
	//	CreaseAngle
	////////////////////////////////////////////////
	
	void setCreaseAngle(float value) {
		SFFloat *creaseAngle = (SFFloat *)getField(creaseAngleFieldString);
		creaseAngle->setValue(value);
	}

	float getCreaseAngle() {
		SFFloat *creaseAngle = (SFFloat *)getField(creaseAngleFieldString);
		return creaseAngle->getValue();
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
	
	////////////////////////////////////////////////
	// TexCoordIndex
	////////////////////////////////////////////////

	void addTexCoordIndex(int value) {
		MFInt32 *coordIndex = (MFInt32 *)getField(texCoordIndexFieldString);
		coordIndex->addValue(value);
	}
	int getNTexCoordIndexes() {
		MFInt32 *coordIndex = (MFInt32 *)getField(texCoordIndexFieldString);
		return coordIndex->getSize();
	}
	int getTexCoordIndex(int index) {
		MFInt32 *coordIndex = (MFInt32 *)getField(texCoordIndexFieldString);
		return coordIndex->get1Value(index);
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

	////////////////////////////////////////////////
	// NormalIndex
	////////////////////////////////////////////////

	void addNormalIndex(int value) {
		MFInt32 *normalIndex = (MFInt32 *)getField(normalIndexFieldString);
		normalIndex->addValue(value);
	}
	int getNNormalIndexes() {
		MFInt32 *normalIndex = (MFInt32 *)getField(normalIndexFieldString);
		return normalIndex->getSize();
	}
	int getNormalIndex(int index) {
		MFInt32 *normalIndex = (MFInt32 *)getField(normalIndexFieldString);
		return normalIndex->get1Value(index);
	}

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		if (node->isColorNode() || node->isCoordinateNode() || node->isNormalNode() || node->isTextureCoordinateNode())
			return true;
		else
			return false;
	}

	void initialize();

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
		SFBool *convex = (SFBool *)getField(convexFieldString);
		SFBool *solid = (SFBool *)getField(solidFieldString);
		SFBool *normalPerVertex = (SFBool *)getField(normalPerVertexFieldString);
		SFBool *colorPerVertex = (SFBool *)getField(colorPerVertexFieldString);
		SFBool *ccw = (SFBool *)getField(ccwFieldString);

		printStream << indentString << "\t" << "ccw " << ccw << endl;
		printStream << indentString << "\t" << "colorPerVertex " << colorPerVertex << endl;
		printStream << indentString << "\t" << "normalPerVertex " << normalPerVertex << endl;
		printStream << indentString << "\t" << "convex " << convex << endl;
		printStream << indentString << "\t" << "creaseAngle " << getCreaseAngle() << endl;
		printStream << indentString << "\t" << "solid " << solid << endl;

		NormalNode *normal = getNormalNodes();
		if (normal != NULL) {
			if (normal->isInstanceNode() == false) {
				if (normal->getName() != NULL && strlen(normal->getName()))
					printStream << indentString << "\t" << "normal " << "DEF " << normal->getName() << " Normal {" << endl;
				else
					printStream << indentString << "\t" << "normal Normal {" << endl;
				normal->Node::outputContext(printStream, indentString , "\t");
				printStream << indentString << "\t" << "}" << endl;
			}
			else 
				printStream << indentString << "\t" << "normal USE " << normal->getName() << endl;
		}

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

		TextureCoordinateNode *texCoord = getTextureCoordinateNodes();
		if (texCoord != NULL) {
			if (texCoord->isInstanceNode() == false) {
				if (texCoord->getName() != NULL && strlen(texCoord->getName()))
					printStream << indentString << "\t" << "texCoord " << "DEF " << texCoord->getName() << " TextureCoordinate {" << endl;
				else
					printStream << indentString << "\t" << "texCoord TextureCoordinate {" << endl;
				texCoord->Node::outputContext(printStream, indentString, "\t");
				printStream << indentString << "\t" << "}" << endl;
			}
			else 
				printStream << indentString << "\t" << "texCoord USE " << texCoord->getName() << endl;
		}

		if (0 < getNCoordIndexes()) {
			MFInt32 *coordIndex = (MFInt32 *)getField(coordIndexFieldString);
			printStream << indentString << "\t" << "coordIndex [" << endl;
			coordIndex->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
		
		if (0 < getNTexCoordIndexes()) {
			MFInt32 *texCoordIndex = (MFInt32 *)getField(texCoordIndexFieldString);
			printStream << indentString << "\t" << "texCoordIndex [" << endl;
			texCoordIndex->MField::outputContext(printStream, indentString,"\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
		
		if (0 < getNColorIndexes()) {
			MFInt32 *colorIndex = (MFInt32 *)getField(colorIndexFieldString);
			printStream << indentString << "\t" << "colorIndex [" << endl;
			colorIndex->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
		
		if (0 < getNNormalIndexes()) {
			MFInt32 *normalIndex = (MFInt32 *)getField(normalIndexFieldString);
			printStream << indentString << "\t" << "normalIndex [" << endl;
			normalIndex->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}
	}

	////////////////////////////////////////////////
	//	Polygon
	////////////////////////////////////////////////

	int		getNPolygons();

	////////////////////////////////////////////////
	//	Normal
	////////////////////////////////////////////////

	bool generateNormals();

	////////////////////////////////////////////////
	//	TextureCoordinate
	////////////////////////////////////////////////
	
	bool generateTextureCoordinate();

};

#endif

