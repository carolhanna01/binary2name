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
*	Copyright (C) Satoshi Konno 1996-1998
*
*	File:	Node.h
*
******************************************************************/

#ifndef _NODE_H_
#define _NODE_H_

#include <iostream.h>
#include <fstream.h>
#include <assert.h>
#include "JVector.h"
#include "Field.h"
#include "JString.h"
#include "CLinkedList.h"
#include "SFMatrix.h"
#include "JNode.h"

typedef char	*String;

#include "vrmlstring.h"

class	SceneGraph;
	
class	AnchorNode;
class	AppearanceNode;
class	AudioClipNode;
class	BackgroundNode;
class	BillboardNode;
class	BoxNode;
class	CollisionNode;
class	ColorNode;
class	ColorInterpolatorNode;
class	ConeNode;
class	CoordinateNode;
class	CoordinateInterpolatorNode;
class	CylinderNode;
class	CylinderSensorNode;
class	DirectionalLightNode;
class	ElevationGridNode;
class	ExtrusionNode;
class	FogNode;
class	FontStyleNode;
class	GroupNode;
class	ImageTextureNode;
class	IndexedFaceSetNode;
class	IndexedLineSetNode;
class	InlineNode;
class	LodNode;
class	MaterialNode;
class	MovieTextureNode;
class	NavigationInfoNode;
class	NormalNode;
class	NormalInterpolatorNode;
class	OrientationInterpolatorNode;
class	PixelTextureNode;
class	PlaneSensorNode;
class	PointLightNode;
class	PointSetNode;
class	PositionInterpolatorNode;
class	ProximitySensorNode;
class	ScalarInterpolatorNode;
class	ScriptNode;
class	ShapeNode;
class	SoundNode;
class	SphereNode;
class	SphereSensorNode;
class	SpotLightNode;
class	SwitchNode;
class	TextNode;
class	TextureCoordinateNode;
class	TextureTransformNode;
class	TimeSensorNode;
class	TouchSensorNode;
class	TransformNode;
class	ViewpointNode;
class	VisibilitySensorNode;
class	WorldInfoNode;

class	GroupingNode;

class	DefNode;

class Node : public CLinkedListNode<Node> {

public:
	JString				*mName;
	JString				*mType;
	JVector<Field>		*mExposedField;
	JVector<Field>		*mEventInField;
	JVector<Field>		*mEventOutField;
	JVector<Field>		*mField;
	JVector<Field>		*mPrivateField;
	JVector<Node>		*mPrivateNodeVector;
	bool				*mInitialized;

	JString				*mOrgName;
	JString				*mOrgType;
	JVector<Field>		*mOrgExposedField;
	JVector<Field>		*mOrgEventInField;
	JVector<Field>		*mOrgEventOutField;
	JVector<Field>		*mOrgField;
	JVector<Field>		*mOrgPrivateField;

private:
	Node				*mParentNode;
	CLinkedList<Node>	*mChildNodes;
	SceneGraph			*mSceneGraph;

#ifdef SUPPORT_JSAI
	JNode				*mJNode;
#endif

	void				*mValue;
	Node				*mReferenceNode;

public:

	Node();
	
	Node(char * nodeType, char * nodeName);
	
	virtual ~Node();

	void initializeMember();

	void remove();

	void setName(char * name) {
		JString	nodeName(name);
		char *nameString = nodeName.getValue(); 
		for (int n=0; n<nodeName.length(); n++) {
			if (nameString[n] <= 0x20)
				nameString[n] = '_';
		}
		mName->setValue(nameString);
	}

	char *getName() {
		return mName->getValue();
	}

	void setType(char * name) {
		mType->setValue(name);
	}

	char *getType() {
		return mType->getValue();
	}

	////////////////////////////////////////////////
	//	Java
	////////////////////////////////////////////////

#ifdef SUPPORT_JSAI

	void createJavaNodeObject() {
		mJNode = new JNode(this);
	}

	void setJavaNodeObject(JNode *jnode) {
		mJNode = jnode;
	}

	JNode *getJavaNodeObject() {
		return mJNode;
	}

#endif

	////////////////////////////////////////////////
	//	Field
	////////////////////////////////////////////////

	Field *createField(int type);

	////////////////////////////////////////////////
	//	EventIn
	////////////////////////////////////////////////

	Field *getEventIn(char * fieldString) {

		JString fieldName(fieldString);
		
		int nEventIn = getNEventIn();
		for (int n=0; n<nEventIn; n++) {
			Field *field = getEventIn(n);
			if (fieldName.compareTo(field->getName()) == 0)
				return field;
			if (fieldName.startsWith(eventInStripString) == 0) {
				if (fieldName.endsWith(field->getName()) == 0)
					return field;
			}
		}

		return NULL;
	}

	int getNEventIn() {
		return mEventInField->size();
	}

	void addEventIn(Field *field) {
		assert(field->getName() && strlen(field->getName()));
		assert(!getEventIn(field->getName()));
		mEventInField->addElement(field);
	}

	void addEventIn(char * name, Field *field) {
		assert(name && strlen(name));
		assert(!getEventIn(name));
		field->setName(name);
		mEventInField->addElement(field);
	}

	void addEventIn(char * name, int fieldType) {
		addEventIn(name, createField(fieldType));
	}

	Field *getEventIn(int index) {
		return (Field *)mEventInField->elementAt(index);
	}

	int getEventInNumber(Field *field) {
		int nEventIn = getNEventIn();
		for (int n=0; n<nEventIn; n++) {
			if (getEventIn(n) == field)
				return n;
		}
		return -1;
	}

	////////////////////////////////////////////////
	//	EventOut
	////////////////////////////////////////////////

	Field *getEventOut(char *fieldString) {

		JString fieldName(fieldString);

		int nEventOut = getNEventOut();
		for (int n=0; n<nEventOut; n++) {
			Field *field = getEventOut(n);
			if (fieldName.compareTo(field->getName()) == 0)
				return field;
			if (fieldName.endsWith(eventOutStripString) == 0) {
				if (fieldName.startsWith(field->getName())  == 0)
					return field;
			}
		}
		return NULL;
	}

	int getNEventOut() {
		return mEventOutField->size();
	}

	void addEventOut(Field *field) {
		assert(field->getName() && strlen(field->getName()));
		assert(!getEventOut(field->getName()));
		mEventOutField->addElement(field);
	}

	void addEventOut(char *name, Field *field) {
		assert(name && strlen(name));
		assert(!getEventOut(name));
		field->setName(name);
		mEventOutField->addElement(field);
	}

	void addEventOut(char * name, int fieldType) {
		addEventOut(name, createField(fieldType));
	}

	Field *getEventOut(int index) {
		return (Field *)mEventOutField->elementAt(index);
	}

	int getEventOutNumber(Field *field) {
		int nEventOut = getNEventOut();
		for (int n=0; n<nEventOut; n++) {
			if (getEventOut(n) == field)
				return n;
		}
		return -1;
	}

	////////////////////////////////////////////////
	//	ExposedField
	////////////////////////////////////////////////

	Field *getExposedField(char * fieldString) {
		
		JString fieldName(fieldString);

		int nExposedField = getNExposedFields();
		for (int n=0; n<nExposedField; n++) {
			Field *field = getExposedField(n);
			if (fieldName.compareTo(field->getName()) == 0)
				return field;
			if (fieldName.startsWith(eventInStripString) == 0) {
				if (fieldName.endsWith(field->getName()) == 0)
					return field;
			}
			if (fieldName.endsWith(eventOutStripString) == 0) {
				if (fieldName.startsWith(field->getName()) == 0)
					return field;
			}
		}
		return NULL;
	}

	int getNExposedFields() {
		return mExposedField->size();
	}

	void addExposedField(Field *field) {
		assert(field->getName() && strlen(field->getName()));
		assert(!getExposedField(field->getName()));
		mExposedField->addElement(field);
	}

	void addExposedField(char * name, Field *field) {
		assert(name && strlen(name));
		assert(!getExposedField(name));
		field->setName(name);
		mExposedField->addElement(field);
	}

	void addExposedField(char * name, int fieldType) {
		addExposedField(name, createField(fieldType));
	}

	Field *getExposedField(int index) {
		return (Field *)mExposedField->elementAt(index);
	}

	int getExposedFieldNumber(Field *field) {
		int nExposedField = getNExposedFields();
		for (int n=0; n<nExposedField; n++) {
			if (getExposedField(n) == field)
				return n;
		}
		return -1;
	}

	////////////////////////////////////////////////
	//	Field
	////////////////////////////////////////////////

	// Get an exposed field by name-> 
	//   Throws an InvalidExposedFieldException if fieldName isn't a valid
	//   exposed field name for a node of this type->
	Field *getField(char *fieldString) {
		
		JString fieldName(fieldString);

		int nField = getNFields();
		for (int n=0; n<nField; n++) {
			Field *field = getField(n);
			if (fieldName.compareTo(field->getName()) == 0)
				return field;
		}
		return NULL;
	}

	int getNFields() {
		return mField->size();
	}

	void addField(Field *field) {
		assert(field->getName() && strlen(field->getName()));
		assert(!getField(field->getName()));
		mField->addElement(field);
	}

	void addField(char * name, Field *field) {
		assert(name && strlen(name));
		assert(!getField(name));
		field->setName(name);
		mField->addElement(field);
	}

	void addField(char * name, int fieldType) {
		addField(name, createField(fieldType));
	}

	Field *getField(int index) {
		return (Field *)mField->elementAt(index);
	}

	int getFieldNumber(Field *field) {
		int nField = getNFields();
		for (int n=0; n<nField; n++) {
			if (getField(n) == field)
				return n;
		}
		return -1;
	}

	////////////////////////////////////////////////
	//	PrivateField
	////////////////////////////////////////////////

	Field *getPrivateField(char *fieldString) {
		
		JString fieldName(fieldString);

		int nPrivateField = getNPrivateFields();
		for (int n=0; n<nPrivateField; n++) {
			Field *field = getPrivateField(n);
			if (fieldName.compareTo(field->getName()) == 0)
				return field;
		}
		return NULL;
	}

	int getNPrivateFields() {
		return mPrivateField->size();
	}

	void addPrivateField(Field *field) {
		assert(field->getName() && strlen(field->getName()));
		assert(!getPrivateField(field->getName()));
		mPrivateField->addElement(field);
	}

	void addPrivateField(char * name, Field *field) {
		assert(name && strlen(name));
		assert(!getPrivateField(name));
		field->setName(name);
		mPrivateField->addElement(field);
	}

	Field *getPrivateField(int index) {
		return (Field *)mPrivateField->elementAt(index);
	}

	int getPrivateFieldNumber(Field *field) {
		int nPrivateField = getNPrivateFields();
		for (int n=0; n<nPrivateField; n++) {
			if (getPrivateField(n) == field)
				return n;
		}
		return -1;
	}

	////////////////////////////////////////////////
	//	PrivateField
	////////////////////////////////////////////////

	int getNPrivateNodeElements() {
		return mPrivateNodeVector->size();
	}

	void addPrivateNodeElement(Node *node) {
		mPrivateNodeVector->addElement(node, false);
	}

	Node *getPrivateNodeElementAt(int n) {
		return mPrivateNodeVector->elementAt(n);
	}

	void removeAllNodeElement() {
		mPrivateNodeVector->removeAllElements();
	}

	////////////////////////////////////////////////
	//	Parent node
	////////////////////////////////////////////////

	void setParentNode(Node *parentNode) {
		mParentNode = parentNode;
	}

	Node *getParentNode() {
		return mParentNode;
	}

	bool isParentNode(Node *node) {
		return (getParentNode() == node) ? true : false;
	}

	bool isAncestorNode(Node *node) {
		for (Node *parentNode = getParentNode(); parentNode; parentNode = parentNode->getParentNode()) {
			if (node == parentNode)
					return true;
		}
		return false;
	}

	////////////////////////////////////////////////
	//	Traversal node list
	////////////////////////////////////////////////

	Node *nextTraversal() {
		Node *nextNode = getChildNodes();
		if (nextNode != NULL)
			return nextNode;

		nextNode = next();
		if (nextNode == NULL) {
			Node *parentNode = getParentNode();
			while (parentNode != NULL) { 
				Node *parentNextNode = parentNode->next();
				if (parentNextNode != NULL)
					return parentNextNode;
				parentNode = parentNode->getParentNode();
			}
		}
		return nextNode;
	}

	Node *nextTraversalByType(char *typeString) {
		if (typeString == NULL)
			return NULL;
		
		JString type(typeString);

		for (Node *node = nextTraversal(); node != NULL; node = node->nextTraversal()) {
			if (node->getType() != NULL) {
				if (type.compareTo(node->getType()) == 0)
					return node;
			}
		}
		return NULL;
	}

	Node *nextTraversalByName(char *nameString) {
		if (nameString == NULL)
			return NULL;

		JString name(nameString);

		for (Node *node = nextTraversal(); node != NULL; node = node->nextTraversal()) {
			if (node->getName() != NULL) {
				if (name.compareTo(node->getName()) == 0)
					return node;
			}
		}
		return NULL;
	}

	////////////////////////////////////////////////
	//	next node list
	////////////////////////////////////////////////

	Node *next() {
		return CLinkedListNode<Node>::next(); 
	}

	Node *next(char *typeString) {

		JString type(typeString);

		for (Node *node = next(); node != NULL; node = node->next()) {
			if (type.compareTo(node->getType()) == 0)
				return node;
		}
		return NULL;
	}

	////////////////////////////////////////////////
	//	child node list
	////////////////////////////////////////////////

	Node *getChildNodes() {
		return mChildNodes->getNodes();
	}

	Node *getChildNode(char *typeString) {

	  JString type(typeString);
		
	  for (Node *node = getChildNodes(); node != NULL; node = node->next()) { //JMC
	    if (type.compareTo(node->getType()) == 0) return node;  //JMC
	    if (node->getReferenceNode()) {  //JMC
	      Node *r= node->getReferenceNode();  //JMC
	      while (r->getReferenceNode()) r= r->getReferenceNode();  //JMC
	      if (type.compareTo(r->getType()) == 0) return r;  //JMC
	    }  //JMC
	  }  //JMC
	  
	  return NULL;
	}

	Node *getChildNode(int n) {
		return mChildNodes->getNode(n);
	}

	int getNChildNodes() {
		return mChildNodes->getNNodes();
	}

	virtual bool isChildNodeType(Node *node) = 0;

	void addChildNode(Node *node, bool initialize = true);
	void addChildNodeAtFirst(Node *node, bool initialize = true);

	void moveChildNode(Node *node);
	void moveChildNodeAtFirst(Node *node);

	void deleteChildNodes(void);

	void removeRoutes();
	void removeSFNodes();
	void removeInstanceNodes();

	////////////////////////////////////////////////
	//	Add / Remove children (for Groupingnode)
	////////////////////////////////////////////////

	bool isChildNode(Node *parentNode, Node *node) {
		for (Node *cnode = parentNode->getChildNodes(); cnode != NULL; cnode = cnode->next()) {
			if (cnode == node)
				return true;
			if (isChildNode(cnode, node) == true)
				return true;
		}
		return false;
	}

	bool isChildNode(Node *node) {
		for (Node *cnode = getChildNodes(); cnode != NULL; cnode = cnode->next()) {
			if (isChildNode(cnode, node) == true)
				return true;
		}
		return false;
	}
/*
	void	addChildren(Node *node[]) {
		for (int n=0; n<node->length; n++) {
			if (!isChild(node[n]))
				addChildNode(node[n]);
		}

	}

	void	removeChildren(Node *node[]) {
		for (int n=0; n<node->length; n++) {
			if (isChild(node[n]))
				delete node[n];
		}

	}
*/
	////////////////////////////////////////////////
	//	get child node list
	////////////////////////////////////////////////

	GroupingNode *getGroupingNodes() {
		for (Node *node = getChildNodes(); node != NULL; node = node->next()) {
			if (node->isGroupingNode())
				return (GroupingNode *)node;
		}
		return NULL;
	}

	Node *getGeometryNode() {
		for (Node *node = getChildNodes(); node != NULL; node = node->next()) {
			if (node->isGeometryNode())
				return node;
		}
		return NULL;
	}

	Node *getTextureNode() {
		for (Node *node = getChildNodes(); node != NULL; node = node->next()) {
			if (node->isTextureNode())
				return node;
		}
		return NULL;
	}

	AnchorNode *getAnchorNodes() {
		return (AnchorNode *)getChildNode(anchorNodeString);
	}

	AppearanceNode *getAppearanceNodes() {
		return (AppearanceNode *)getChildNode(appearanceNodeString);
	}

	AudioClipNode *getAudioClipNodes() {
		return (AudioClipNode *)getChildNode(audioClipNodeString);
	}

	BackgroundNode *getBackgroundNodes() {
		return (BackgroundNode *)getChildNode(backgroundNodeString);
	}

	BillboardNode *getBillboardNodes() {
		return (BillboardNode *)getChildNode(billboardNodeString);
	}

	BoxNode *getBoxeNodes() {
		return (BoxNode *)getChildNode(boxNodeString);
	}

	CollisionNode *getCollisionNodes() {
		return (CollisionNode *)getChildNode(collisionNodeString);
	}

	ColorNode *getColorNodes() {
		return (ColorNode *)getChildNode(colorNodeString);
	}

	ColorInterpolatorNode *getColorInterpolatorNodes() {
		return (ColorInterpolatorNode *)getChildNode(colorInterpolatorNodeString);
	}

	ConeNode *getConeNodes() {
		return (ConeNode *)getChildNode(coneNodeString);
	}

	CoordinateNode *getCoordinateNodes() {
		return (CoordinateNode *)getChildNode(coordinateNodeString);
	}

	CoordinateInterpolatorNode *getCoordinateInterpolatorNodes() {
		return (CoordinateInterpolatorNode *)getChildNode(coordinateInterpolatorNodeString);
	}

	CylinderNode *getCylinderNodes() {
		return (CylinderNode *)getChildNode(cylinderNodeString);
	}

	CylinderSensorNode *getCylinderSensorNodes() {
		return (CylinderSensorNode *)getChildNode(cylinderSensorNodeString);
	}

	DirectionalLightNode *getDirectionalLightNodes() {
		return (DirectionalLightNode *)getChildNode(directionalLightNodeString);
	}

	ElevationGridNode *getElevationGridNodes() {
		return (ElevationGridNode *)getChildNode(elevationGridNodeString);
	}

	ExtrusionNode *getExtrusionNodes() {
		return (ExtrusionNode *)getChildNode(extrusionNodeString);
	}

	FogNode *getFogNodes() {
		return (FogNode *)getChildNode(fogNodeString);
	}

	FontStyleNode *getFontStyleNodes() {
		return (FontStyleNode *)getChildNode(fontStyleNodeString);
	}

	GroupNode *getGroupNodes() {
		return (GroupNode *)getChildNode(groupNodeString);
	}

	ImageTextureNode *getImageTextureNodes() {
		return (ImageTextureNode *)getChildNode(imageTextureNodeString);
	}

	IndexedFaceSetNode *getIndexedFaceSetNodes() {
		return (IndexedFaceSetNode *)getChildNode(indexedFaceSetNodeString);
	}

	IndexedLineSetNode *getIndexedLineSetNodes() {
		return (IndexedLineSetNode *)getChildNode(indexedLineSetNodeString);
	}

	InlineNode *getInlineNodes() {
		return (InlineNode *)getChildNode(inlineNodeString);
	}

	LodNode *getLodNodes() {
		return (LodNode *)getChildNode(lodNodeString);
	}

	MaterialNode *getMaterialNodes() {
		return (MaterialNode *)getChildNode(materialNodeString);
	}

	MovieTextureNode *getMovieTextureNodes() {
		return (MovieTextureNode *)getChildNode(movieTextureNodeString);
	}

	NavigationInfoNode *getNavigationInfoNodes() {
		return (NavigationInfoNode *)getChildNode(navigationInfoNodeString);
	}

	NormalNode *getNormalNodes() {
		return (NormalNode *)getChildNode(normalNodeString);
	}

	NormalInterpolatorNode *getNormalInterpolatorNodes() {
		return (NormalInterpolatorNode *)getChildNode(normalInterpolatorNodeString);
	}

	OrientationInterpolatorNode *getOrientationInterpolatorNodes() {
		return (OrientationInterpolatorNode *)getChildNode(orientationInterpolatorNodeString);
	}

	PixelTextureNode *getPixelTextureNodes() {
		return (PixelTextureNode *)getChildNode(pixelTextureNodeString);
	}

	PlaneSensorNode *getPlaneSensorNodes() {
		return (PlaneSensorNode *)getChildNode(planeSensorNodeString);
	}

	PointLightNode *getPointLightNodes() {
		return (PointLightNode *)getChildNode(pointLightNodeString);
	}

	PointSetNode *getPointSetNodes() {
		return (PointSetNode *)getChildNode(pointSetNodeString);
	}

	PositionInterpolatorNode *getPositionInterpolatorNodes() {
		return (PositionInterpolatorNode *)getChildNode(positionInterpolatorNodeString);
	}

	ProximitySensorNode *getProximitySensorNodes() {
		return (ProximitySensorNode *)getChildNode(proximitySensorNodeString);
	}

	ScalarInterpolatorNode *getScalarInterpolatorNodes() {
		return (ScalarInterpolatorNode *)getChildNode(scalarInterpolatorNodeString);
	}

	ScriptNode *getScriptNodes() {
		return (ScriptNode *)getChildNode(scriptNodeString);
	}

	ShapeNode *getShapeNodes() {
		return (ShapeNode *)getChildNode(shapeNodeString);
	}

	SoundNode *getSoundNodes() {
		return (SoundNode *)getChildNode(soundNodeString);
	}

	SphereNode *getSphereNodes() {
		return (SphereNode *)getChildNode(sphereNodeString);
	}

	SphereSensorNode *getSphereSensorNodes() {
		return (SphereSensorNode *)getChildNode(sphereSensorNodeString);
	}

	SpotLightNode *getSpotLightNodes() {
		return (SpotLightNode *)getChildNode(spotLightNodeString);
	}

	SwitchNode *getSwitchNodes() {
		return (SwitchNode *)getChildNode(switchNodeString);
	}

	TextNode *getTextNodes() {
		return (TextNode *)getChildNode(textNodeString);
	}

	TextureCoordinateNode *getTextureCoordinateNodes() {
		return (TextureCoordinateNode *)getChildNode(textureCoordinateNodeString);
	}

	TextureTransformNode *getTextureTransformNodes() {
		return (TextureTransformNode *)getChildNode(textureTransformNodeString);
	}

	TimeSensorNode *getTimeSensorNodes() {
		return (TimeSensorNode *)getChildNode(timeSensorNodeString);
	}

	TouchSensorNode *getTouchSensorNodes() {
		return (TouchSensorNode *)getChildNode(touchSensorNodeString);
	}

	TransformNode *getTransformNodes() {
		return (TransformNode *)getChildNode(transformNodeString);
	}

	ViewpointNode *getViewpointNodes() {
		return (ViewpointNode *)getChildNode(viewpointNodeString);
	}

	VisibilitySensorNode *getVisibilitySensorNodes() {
		return (VisibilitySensorNode *)getChildNode(visibilitySensorNodeString);
	}

	WorldInfoNode *getWorldInfoNodes() {
		return (WorldInfoNode *)getChildNode(worldInfoNodeString);
	}

	////////////////////////////////////////////////
	//	
	////////////////////////////////////////////////

	bool isNode(char * nodeType) {
		if (!nodeType)
			return false;
		char *nodeString = getType();
		if (!nodeString)
			return false;
		if (strcmp(nodeString, nodeType) == 0)
			return true;
		else {
		  if (getReferenceNode()) return getReferenceNode()->isNode(nodeType);
		  return false; //JMC
		}
	}

	bool isRootNode() {
		return isNode(rootNodeString);
	}

	bool isDefNode() {
		return isNode(defNodeString);
	}

	bool isInlineChildNode() {
		Node *parentNode = getParentNode();
		while (parentNode != NULL) {
			if (parentNode->isInlineNode() == true)
				return true;
			parentNode = parentNode->getParentNode();
		}
		return false;
	}

	bool isGroupingNode() {
		if (isAnchorNode() || isBillboardNode() || isCollisionNode() || isGroupNode() || isTransformNode())
			return true;
		else
			return false;
	}

	bool isSpecialGroupNode() {
		if (isInlineNode() || isLodNode() || isSwitchNode())
			return true;
		else
			return false;
	}

	bool isCommonNode() {
		if (isLightNode() || isAudioClipNode() || isScriptNode() || isShapeNode() || isSoundNode() || isWorldInfoNode())
			return true;
		else
			return false;
	}

	bool isLightNode() {
		if (isDirectionalLightNode() || isSpotLightNode() || isPointLightNode())
			return true;
		else
			return false;
	}

	bool isGeometryNode() {
		if (isBoxNode() || isConeNode() || isCylinderNode() || isElevationGridNode() || isExtrusionNode() || isIndexedFaceSetNode() || isIndexedLineSetNode() || isPointSetNode() || isSphereNode() || isTextNode())
			return true;
		else
			return false;
	}

	bool isGeometryPropertyNode() {
		if (isColorNode() || isCoordinateNode() || isNormalNode() || isTextureCoordinateNode())
			return true;
		else
			return false;
	}

	bool isTextureNode() {
		if (isMovieTextureNode() || isPixelTextureNode() || isImageTextureNode() )
			return true;
		else
			return false;
	}

	bool isSensorNode() {
		if (isCylinderSensorNode() || isPlaneSensorNode() || isSphereSensorNode() || isProximitySensorNode() || isTimeSensorNode() || isTouchSensorNode() || isVisibilitySensorNode())
			return true;
		else
			return false;
	}

	bool isInterpolatorNode() {
		if (isColorInterpolatorNode() || isCoordinateInterpolatorNode() || isNormalInterpolatorNode() || isOrientationInterpolatorNode() || isPositionInterpolatorNode() || isScalarInterpolatorNode())
			return true;
		else
			return false;
	}

	bool isBindableNode() {
		if (isBackgroundNode() || isFogNode() || isNavigationInfoNode() || isViewpointNode())
			return true;
		else
			return false;
	}


	bool isAnchorNode() {
		return isNode(anchorNodeString);
	}

	bool isAppearanceNode() {
		return isNode(appearanceNodeString);
	}

	bool isAudioClipNode() {
		return isNode(audioClipNodeString);
	}

	bool isBackgroundNode() {
		return isNode(backgroundNodeString);
	}

	bool isBillboardNode() {
		return isNode(billboardNodeString);
	}

	bool isBoxNode() {
		return isNode(boxNodeString);
	}

	bool isCollisionNode() {
		return isNode(collisionNodeString);
	}

	bool isColorNode() {
		return isNode(colorNodeString);
	}

	bool isColorInterpolatorNode() {
		return isNode(colorInterpolatorNodeString);
	}

	bool isConeNode() {
		return isNode(coneNodeString);
	}

	bool isCoordinateNode() {
		return isNode(coordinateNodeString);
	}

	bool isCoordinateInterpolatorNode() {
		return isNode(coordinateInterpolatorNodeString);
	}

	bool isCylinderNode() {
		return isNode(cylinderNodeString);
	}

	bool isCylinderSensorNode() {
		return isNode(cylinderSensorNodeString);
	}

	bool isDirectionalLightNode() {
		return isNode(directionalLightNodeString);
	}

	bool isElevationGridNode() {
		return isNode(elevationGridNodeString);
	}

	bool isExtrusionNode() {
		return isNode(extrusionNodeString);
	}

	bool isFogNode() {
		return isNode(fogNodeString);
	}

	bool isFontStyleNode() {
		return isNode(fontStyleNodeString);
	}

	bool isGroupNode() {
		return isNode(groupNodeString);
	}

	bool isImageTextureNode() {
		return isNode(imageTextureNodeString);
	}

	bool isIndexedFaceSetNode() {
		return isNode(indexedFaceSetNodeString);
	}

	bool isIndexedLineSetNode() {
		return isNode(indexedLineSetNodeString);
	}

	bool isInlineNode() {
		return isNode(inlineNodeString);
	}

	bool isLodNode() {
		return isNode(lodNodeString);
	}

	bool isMaterialNode() {
		return isNode(materialNodeString);
	}

	bool isMovieTextureNode() {
		return isNode(movieTextureNodeString);
	}

	bool isNavigationInfoNode() {
		return isNode(navigationInfoNodeString);
	}

	bool isNormalNode() {
		return isNode(normalNodeString);
	}

	bool isNormalInterpolatorNode() {
		return isNode(normalInterpolatorNodeString);
	}

	bool isOrientationInterpolatorNode() {
		return isNode(orientationInterpolatorNodeString);
	}

	bool isPixelTextureNode() {
		return isNode(pixelTextureNodeString);
	}

	bool isPlaneSensorNode() {
		return isNode(planeSensorNodeString);
	}

	bool isPointLightNode() {
		return isNode(pointLightNodeString);
	}

	bool isPointSetNode() {
		return isNode(pointSetNodeString);
	}

	bool isPositionInterpolatorNode() {
		return isNode(positionInterpolatorNodeString);
	}

	bool isProximitySensorNode() {
		return isNode(proximitySensorNodeString);
	}

	bool isScalarInterpolatorNode() {
		return isNode(scalarInterpolatorNodeString);
	}

	bool isScriptNode() {
		return isNode(scriptNodeString);
	}

	bool isShapeNode() {
		return isNode(shapeNodeString);
	}

	bool isSoundNode() {
		return isNode(soundNodeString);
	}

	bool isSphereNode() {
		return isNode(sphereNodeString);
	}

	bool isSphereSensorNode() {
		return isNode(sphereSensorNodeString);
	}

	bool isSpotLightNode() {
		return isNode(spotLightNodeString);
	}

	bool isSwitchNode() {
		return isNode(switchNodeString);
	}

	bool isTextNode() {
		return isNode(textNodeString);
	}

	bool isTextureCoordinateNode() {
		return isNode(textureCoordinateNodeString);
	}

	bool isTextureTransformNode() {
		return isNode(textureTransformNodeString);
	}

	bool isTimeSensorNode() {
		return isNode(timeSensorNodeString);
	}

	bool isTouchSensorNode() {
		return isNode(touchSensorNodeString);
	}

	bool isTransformNode() {
		return isNode(transformNodeString);
	}

	bool isViewpointNode() {
		return isNode(viewpointNodeString);
	}

	bool isVisibilitySensorNode() {
		return isNode(visibilitySensorNodeString);
	}

	bool isWorldInfoNode() {
		return isNode(worldInfoNodeString);
	}

	////////////////////////////////////////////////
	//	output
	////////////////////////////////////////////////

	char *getIndentLevelString(int nIndentLevel);

	void outputHead(ostream& printStream, char *indentString);

	void outputTail(ostream& printStream, char * indentString);

	virtual void outputContext(ostream &printStream, char *indentString) = 0;

	void outputContext(ostream& printStream, char *indentString1, char *indentString2);

	void output(ostream& printStream, int indentLevet);

	void print(ostream& printStream, int indentLevel) {
		output(printStream, indentLevel);
	}

	void print(){
		output(cout, 0);
	}
/*
	void save(ofstream outputStream){
		output(outputStream, 0);
	}
*/

	int save(char * filename) {
		ofstream outputFile(filename, ios::out);
		if (outputFile) {
			output(outputFile, 0);
			return 1;
		}
		else
			return 0;
	}

	////////////////////////////////////////////////
	//	Virtual functions
	////////////////////////////////////////////////

	virtual void update()		= 0;
	virtual void initialize()	= 0;
	virtual void uninitialize()	= 0;

	////////////////////////////////////////////////
	//	Transform matrix
	////////////////////////////////////////////////

	void	getTransformMatrix(SFMatrix *matrix);
	void	getTransformMatrix(float value[4][4]);

	////////////////////////////////////////////////
	//	SceneGraph
	////////////////////////////////////////////////

	void setSceneGraph(SceneGraph *sceneGraph)	{
		mSceneGraph = sceneGraph;
		for (Node *node = getChildNodes(); node; node = node->next()) {
				node->setSceneGraph(sceneGraph);
		}
	}

	SceneGraph	*getSceneGraph() {
		return mSceneGraph;
	}

	////////////////////////////////////////////////
	//	Route
	////////////////////////////////////////////////

	void		sendEvent(Field *eventOutField);
	void		sendEvent(char *eventOutFieldString);

	////////////////////////////////////////////////
	//	Value
	////////////////////////////////////////////////

	void		setValue(void *value)	{ mValue = value; }
	void		*getValue()				{ return mValue; }

	////////////////////////////////////////////////
	//	Initialized
	////////////////////////////////////////////////

	void		setInitialized(bool flag)	{ *mInitialized = flag; }
	bool		isInitialized()				{ return *mInitialized; }

	////////////////////////////////////////////////
	//	BoundingBox
	////////////////////////////////////////////////

	virtual void recomputeBoundingBox() {
	}

	////////////////////////////////////////////////
	//	DisplayList
	////////////////////////////////////////////////

#ifdef SUPPORT_OPENGL

	virtual void recomputeDisplayList() {
	}

#endif

	////////////////////////////////////////////////
	//	Instance node
	////////////////////////////////////////////////

	bool isInstanceNode() {
		return (getReferenceNode() != NULL ? true : false);
	}

	void setReferenceNodeMembers(Node *node);

	void setOriginalMembers();
	
	void setReferenceNode(Node *node) {
		mReferenceNode = node;
	}
	
	Node *getReferenceNode() {
		return mReferenceNode;
	}

	void setAsInstanceNode(Node *node) {
		setReferenceNode(node);
		setReferenceNodeMembers(node);
	}
	
	Node *createInstanceNode();

	////////////////////////////////////////////////
	//	DEF node
	////////////////////////////////////////////////

	DefNode *createDefNode();
};

#endif
