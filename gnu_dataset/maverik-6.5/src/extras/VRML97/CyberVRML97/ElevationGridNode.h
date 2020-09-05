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
*	File:	ElevationGridNode.h
*
******************************************************************/

#ifndef _ELEVATIONGRID_H_
#define _ELEVATIONGRID_H_

#include "GeometryNode.h"
#include "ColorNode.h"
#include "NormalNode.h"
#include "TextureCoordinateNode.h"

class ElevationGridNode : public GeometryNode {

public:

	ElevationGridNode() {

		setHeaderFlag(false);
		setType(elevationGridNodeString);

		// xSpacing field
		SFFloat *xSpacing = new SFFloat(0.0f);
		addField(xSpacingFieldString, xSpacing);

		// zSpacing field
		SFFloat *zSpacing = new SFFloat(0.0f);
		addField(zSpacingFieldString, zSpacing);

		// xDimension field
		SFInt32 *xDimension = new SFInt32(0);
		addField(xDimensionFieldString, xDimension);

		// zDimension field
		SFInt32 *zDimension = new SFInt32(0);
		addField(zDimensionFieldString, zDimension);

		// colorPerVertex exposed field
		SFBool *colorPerVertex = new SFBool(true);
		colorPerVertex->setName(colorPerVertexFieldString);
		addField(colorPerVertex);

		// normalPerVertex exposed field
		SFBool *normalPerVertex = new SFBool(true);
		normalPerVertex->setName(normalPerVertexFieldString);
		addField(normalPerVertex);

		// ccw exposed field
		SFBool *ccw = new SFBool(true);
		ccw->setName(ccwFieldString);
		addField(ccw);

		// solid exposed field
		SFBool *solid = new SFBool(true);
		solid->setName(solidFieldString);
		addField(solid);

		// creaseAngle exposed field
		SFFloat *creaseAngle = new SFFloat(0.0f);
		creaseAngle->setName(creaseAngleFieldString);
		addField(creaseAngle);

		// height exposed field
		MFFloat *height = new MFFloat();
		addField(heightFieldString, height);

		// height eventIn
		MFFloat *setHeight = new MFFloat();
		addEventIn(heightFieldString, setHeight);
	}

	~ElevationGridNode() {
	}

	////////////////////////////////////////////////
	//	xSpacing
	////////////////////////////////////////////////

	void setXSpacing(float value) {
		SFFloat *space = (SFFloat *)getField(xSpacingFieldString);
		space->setValue(value);
	}
	float getXSpacing() {
		SFFloat *space = (SFFloat *)getField(xSpacingFieldString);
		return space->getValue();
	}

	////////////////////////////////////////////////
	//	zSpacing
	////////////////////////////////////////////////

	void setZSpacing(float value) {
		SFFloat *space = (SFFloat *)getField(zSpacingFieldString);
		space->setValue(value);
	}
	float getZSpacing() {
		SFFloat *space = (SFFloat *)getField(zSpacingFieldString);
		return space->getValue();
	}

	////////////////////////////////////////////////
	//	xDimension
	////////////////////////////////////////////////

	void setXDimension(int value) {
		SFInt32 *dimension = (SFInt32 *)getField(xDimensionFieldString);
		dimension->setValue(value);
	}
	int getXDimension() {
		SFInt32 *dimension = (SFInt32 *)getField(xDimensionFieldString);
		return dimension->getValue();
	}

	////////////////////////////////////////////////
	//	zDimension
	////////////////////////////////////////////////

	void setZDimension(int value) {
		SFInt32 *dimension = (SFInt32 *)getField(zDimensionFieldString);
		dimension->setValue(value);
	}
	int getZDimension() {
		SFInt32 *dimension = (SFInt32 *)getField(zDimensionFieldString);
		return dimension->getValue();
	}

	////////////////////////////////////////////////
	//	ColorPerVertex
	////////////////////////////////////////////////
	
	void setColorPerVertex(bool  value) {
		SFBool *colorPerVertex = (SFBool *)getField(colorPerVertexFieldString);
		colorPerVertex->setValue(value);
	}

	void setColorPerVertex(int value) {
		setColorPerVertex(value ? true : false);
	}

	bool  getColorPerVertex() {
		SFBool *colorPerVertex = (SFBool *)getField(colorPerVertexFieldString);
		return colorPerVertex->getValue();
	}

	////////////////////////////////////////////////
	//	NormalPerVertex
	////////////////////////////////////////////////
	
	void setNormalPerVertex(bool  value) {
		SFBool *normalPerVertex = (SFBool *)getField(normalPerVertexFieldString);
		normalPerVertex->setValue(value);
	}

	void setNormalPerVertex(int value) {
		setNormalPerVertex(value ? true : false);
	}

	bool  getNormalPerVertex() {
		SFBool *normalPerVertex = (SFBool *)getField(normalPerVertexFieldString);
		return normalPerVertex->getValue();
	}

	////////////////////////////////////////////////
	//	CCW
	////////////////////////////////////////////////
	
	void setCCW(bool  value) {
		SFBool *ccw = (SFBool *)getField(ccwFieldString);
		ccw->setValue(value);
	}

	void setCCW(int value) {
		setCCW(value ? true : false);
	}

	bool  getCCW() {
		SFBool *ccw = (SFBool *)getField(ccwFieldString);
		return ccw->getValue();
	}

	////////////////////////////////////////////////
	//	Solid
	////////////////////////////////////////////////
	
	void setSolid(bool  value) {
		SFBool *solid = (SFBool *)getField(solidFieldString);
		solid->setValue(value);
	}

	void setSolid(int value) {
		setSolid(value ? true : false);
	}

	bool  getSolid() {
		SFBool *solid = (SFBool *)getField(solidFieldString);
		return solid->getValue();
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
	//	List
	////////////////////////////////////////////////

	ElevationGridNode *next() {
		return (ElevationGridNode *)Node::next(getType());
	}

	ElevationGridNode *nextTraversal() {
		return (ElevationGridNode *)Node::nextTraversalByType(getType());
	}

	////////////////////////////////////////////////
	// height
	////////////////////////////////////////////////

	void addHeight(float value) {
		MFFloat *height = (MFFloat *)getField(heightFieldString);
		height->addValue(value);
	}
	int getNHeights() {
		MFFloat *height = (MFFloat *)getField(heightFieldString);
		return height->getSize();
	}
	float getHeight(int index) {
		MFFloat *height = (MFFloat *)getField(heightFieldString);
		return height->get1Value(index);
	}

	////////////////////////////////////////////////
	//	functions
	////////////////////////////////////////////////
	
	bool isChildNodeType(Node *node){
		if (node->isColorNode() || node->isNormalNode() || node->isTextureCoordinateNode())
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

	void recomputeDisplayList();

	////////////////////////////////////////////////
	//	Infomation
	////////////////////////////////////////////////

	void outputContext(ostream &printStream, String indentString) {
		SFBool *ccw = (SFBool *)getField(ccwFieldString);
		SFBool *solid = (SFBool *)getField(solidFieldString);
		SFBool *colorPerVertex = (SFBool *)getField(colorPerVertexFieldString);
		SFBool *normalPerVertex = (SFBool *)getField(normalPerVertexFieldString);

		printStream << indentString << "\t" << "xDimension " << getXDimension() << endl;
		printStream << indentString << "\t" << "xSpacing " << getXSpacing() << endl;
		printStream << indentString << "\t" << "zDimension " << getZDimension() << endl;
		printStream << indentString << "\t" << "zSpacing " << getZSpacing() << endl;

		if (0 < getNHeights()) {
			MFFloat *height = (MFFloat *)getField(heightFieldString);
			printStream << indentString << "\t" << "height [" << endl;
			height->MField::outputContext(printStream, indentString, "\t\t");
			printStream << indentString << "\t" << "]" << endl;
		}

		printStream << indentString << "\t" << "colorPerVertex " << colorPerVertex << endl;
		printStream << indentString << "\t" << "normalPerVertex " << normalPerVertex << endl;
		printStream << indentString << "\t" << "ccw " << ccw << endl;
		printStream << indentString << "\t" << "solid " << solid << endl;
		printStream << indentString << "\t" << "creaseAngle " << getCreaseAngle() << endl;
		
		NormalNode *normal = getNormalNodes();
		if (normal != NULL) {
			if (normal->isInstanceNode() == false) {
				if (normal->getName() != NULL && strlen(normal->getName()))
					printStream << indentString << "\t" << "normal " << "DEF " << normal->getName() << " Normal {" << endl;
				else
					printStream << indentString << "\t" << "normal Normal {" << endl;
				normal->Node::outputContext(printStream, indentString, "\t");
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
	}
};

#endif

