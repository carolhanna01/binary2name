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
*	File:	Node.cpp
*
******************************************************************/

#include <assert.h>
#include "Node.h"
#include "TransformNode.h"
#include "SceneGraph.h"

////////////////////////////////////////////////
//	Node::Node
////////////////////////////////////////////////

void Node::initializeMember()
{
	mName				= mOrgName				= new JString();
	mType				= mOrgType				= new JString();
	mExposedField		= mOrgExposedField		= new JVector<Field>();
	mEventInField		= mOrgEventInField		= new JVector<Field>();
	mEventOutField		= mOrgEventOutField		= new JVector<Field>();
	mField				= mOrgField				= new JVector<Field>();
	mPrivateField		= mOrgPrivateField		= new JVector<Field>();

	mPrivateNodeVector	= new JVector<Node>();
	mInitialized		= new bool;

	mChildNodes			= new CLinkedList<Node>();

	setName(NULL);
	setParentNode(NULL);
	setSceneGraph(NULL);
#ifdef SUPPORT_JSAI
	setJavaNodeObject(NULL);
#endif
	setValue(NULL);
	setInitialized(false);
	setName(NULL);
	setReferenceNode(NULL);
}

Node::Node() 
{
	setHeaderFlag(true);
	initializeMember();
}
	
Node::Node(char * nodeType, char * nodeName) 
{
	setHeaderFlag(false);
	initializeMember();

	setType(nodeType);
	setName(nodeName);
}
	
////////////////////////////////////////////////
//	Node::~Node
////////////////////////////////////////////////

void Node::deleteChildNodes(void)
{
	Node *node=getChildNodes();
	while (node) {
		Node *nextNode = node->next();
		delete node;
		node = nextNode;
	}
}

Node::~Node() 
{
	deleteChildNodes();

	SceneGraph *sg = getSceneGraph();
	if (sg) {
		if (sg->getSelectedShapeNode() == this)
			sg->setSelectedShapeNode(NULL);
		if (sg->getSelectedNode() == this)
			sg->setSelectedNode(NULL);
	}

	remove();

	if (isInstanceNode() == true)
		setOriginalMembers();

#ifdef SUPPORT_JSAI
	delete mJNode;
#endif

	delete mName;
	delete mType;
	delete mExposedField;
	delete mEventInField;
	delete mEventOutField;
	delete mField;
	delete mPrivateField;
	delete mPrivateNodeVector;
	delete mChildNodes;
	delete mInitialized;
}

////////////////////////////////////////////////
//	Node::addChildNode
////////////////////////////////////////////////

void Node::addChildNode(Node *node, bool initialize) {
	moveChildNode(node);
	if (initialize)
		node->initialize();
}

void Node::addChildNodeAtFirst(Node *node, bool initialize) {
	moveChildNodeAtFirst(node);
	if (initialize)
		node->initialize();
}

////////////////////////////////////////////////
//	Node::moveChildNode
////////////////////////////////////////////////

void Node::moveChildNode(Node *node) {
	mChildNodes->addNode(node); 
	node->setParentNode(this);
	node->setSceneGraph(getSceneGraph());
}

void Node::moveChildNodeAtFirst(Node *node) {
	mChildNodes->addNodeAtFirst(node); 
	node->setParentNode(this);
	node->setSceneGraph(getSceneGraph());
}

////////////////////////////////////////////////
//	Node::remove
////////////////////////////////////////////////

void Node::removeRoutes() 
{
	SceneGraph *sg = getSceneGraph();
	if (sg) {
		Route *route=sg->getRoutes();
		while (route) {
			Route *nextRoute = route->next();
			if (route->getEventInNode() == this || route->getEventOutNode() == this)
				delete route;
			route = nextRoute;
		}
	}
}

void Node::removeSFNodes() 
{
	SceneGraph *sg = getSceneGraph();
	if (sg) {
		for (ScriptNode *script = sg->findScriptNode(); script; script=script->nextTraversal()) {
			for (int n=0; n<script->getNFields(); n++) {
				Field *field = script->getField(n);
				if (field->getType() == fieldTypeSFNode) {
					SFNode *sfnode = (SFNode *)field;
					if (sfnode->getValue() == this)
						sfnode->setValue((Node *)NULL);
				}
			}
		}
	}
}

void Node::removeInstanceNodes() 
{
	SceneGraph *sg = getSceneGraph();
	if (sg && isInstanceNode() == false) {
		Node *node = sg->getNodes();
		while (node) {
			Node *nextNode = node->nextTraversal();
			if (node->isInstanceNode() == true) {
				Node *refNode = node->getReferenceNode();
				while (refNode->isInstanceNode() == true)
					refNode = refNode->getReferenceNode();
				if (refNode == this) {
					node->deleteChildNodes();
					nextNode = node->nextTraversal();
					delete node;
				}
			}
			node = nextNode;
		}
	
	}
}

void Node::remove() 
{
	CLinkedListNode<Node>::remove();

	if (isInstanceNode() == false) {
		removeRoutes();
		removeSFNodes();
		removeInstanceNodes();

		if (isBindableNode()) {
			SceneGraph *sceneGraph = getSceneGraph();
			if (sceneGraph)
				sceneGraph->setBindableNode((BindableNode *)this, false);			
		}
	}

	setParentNode(NULL);
	setSceneGraph(NULL);
}

////////////////////////////////////////////////
//	Node::createField
////////////////////////////////////////////////

Field *Node::createField(int type)
{
	Field	*field = NULL;

	switch (type) {
	case fieldTypeSFBool:
		field = new SFBool();
		break;
	case fieldTypeSFFloat:
		field = new SFFloat();
		break;
	case fieldTypeSFInt32:
		field = new SFInt32();
		break;
	case fieldTypeSFVec2f:
		field = new SFVec2f();
		break;
	case fieldTypeSFVec3f:
		field = new SFVec3f();
		break;
	case fieldTypeSFString:
		field = new SFString();
		break;
	case fieldTypeSFColor:
		field = new SFColor();
		break;
	case fieldTypeSFTime:
		field = new SFTime();
		break;
	case fieldTypeSFRotation:
		field = new SFRotation();
		break;
	}

	assert(field != NULL);

	return field;
}
	
////////////////////////////////////////////////
//	Node::getTransformMatrix(SFMatrix *matrix)
////////////////////////////////////////////////

void Node::getTransformMatrix(SFMatrix *mxOut)
{
	mxOut->init();

	for (Node *node=this; node; node=node->getParentNode()) {
		if (node->isTransformNode()) {
			SFMatrix	mxTransform;
			((TransformNode *)node)->getSFMatrix(&mxTransform);
			mxTransform.add(mxOut);
			mxOut->setValue(&mxTransform);
		}
	}
}

////////////////////////////////////////////////
//	Node::getTransformMatrix(float value[4][4])
////////////////////////////////////////////////

void Node::getTransformMatrix(float value[4][4])
{
	SFMatrix	mx;
	getTransformMatrix(&mx);
	mx.getValue(value);
}

////////////////////////////////////////////////
//	Node::Route
////////////////////////////////////////////////

void Node::sendEvent(Field *eventOutField) {
	getSceneGraph()->updateRoute(this, eventOutField);
}

void Node::sendEvent(char *eventOutFieldString) {
	getSceneGraph()->updateRoute(this, getEventOut(eventOutFieldString));
}

////////////////////////////////////////////////
//	Node::output
////////////////////////////////////////////////

char *Node::getIndentLevelString(int nIndentLevel) 
{
	char *indentString = new char[nIndentLevel+1];
	for (int n=0; n<nIndentLevel; n++)
		indentString[n] = '\t';
	indentString[nIndentLevel] = '\0';
	return indentString;
}

void Node::outputHead(ostream& printStream, char *indentString) 
{
	if (getName() != NULL && strlen(getName()))
		printStream << indentString << "DEF " << getName() << " " << getType() << " {" << endl;
	else
		printStream << indentString << getType() << " {" << endl;
}

void Node::outputTail(ostream& printStream, char * indentString) 
{
	printStream << indentString << "}" << endl;
}

void Node::outputContext(ostream& printStream, char *indentString1, char *indentString2) 
{
	char *indentString = new char[strlen(indentString1)+strlen(indentString2)+1];
	strcpy(indentString, indentString1);
	strcat(indentString, indentString2);
	outputContext(printStream, indentString);
	delete indentString;
}

void Node::output(ostream& printStream, int indentLevet) 
{
	char *indentString = getIndentLevelString(indentLevet);

	if (isInstanceNode() == false) {
		outputHead(printStream, indentString);
		outputContext(printStream, indentString);
	
		if (!isElevationGridNode() && !isShapeNode() && !isSoundNode() && !isPointSetNode() && !isIndexedFaceSetNode() && 
			!isIndexedLineSetNode() && !isTextNode() && !isAppearanceNode()) {
			if (getChildNodes() != NULL) {
				if (isLodNode()) 
					printStream << indentString << "\tlevel [" << endl;
				else if (isSwitchNode()) 
					printStream << indentString << "\tchoice [" << endl;
				else
					printStream << indentString <<"\tchildren [" << endl;
			
				for (Node *node = getChildNodes(); node; node = node->next()) {
					if (node->isInstanceNode() == false) 
						node->output(printStream, indentLevet+2);
					else
						node->output(printStream, indentLevet+2);
				}
			
				printStream << indentString << "\t]" << endl;
			}
		}
		outputTail(printStream, indentString);
	}
	else 
		printStream << indentString << "USE " << getName() << endl;

	delete indentString;
}

////////////////////////////////////////////////
//	InstanceNode
////////////////////////////////////////////////

void Node::setReferenceNodeMembers(Node *node) 
{
	if (!node)
		return;

	mName				= node->mName;
	//mType				= node->mType;
	mExposedField		= node->mExposedField;
	mEventInField		= node->mEventInField;
	mEventOutField		= node->mEventOutField;
	mField				= node->mField;
	mPrivateField		= node->mPrivateField;
}

void Node::setOriginalMembers() 
{
	mName				= mOrgName;
	//mType				= mOrgType;
	mExposedField		= mOrgExposedField;
	mEventInField		= mOrgEventInField;
	mEventOutField		= mOrgEventOutField;
	mField				= mOrgField;
	mPrivateField		= mOrgPrivateField;
}
	

Node *Node::createInstanceNode() 
{
	Node *instanceNode = NULL;
		
	if (isAnchorNode())
		instanceNode = new AnchorNode();
	else if (isAppearanceNode()) 
		instanceNode = new AppearanceNode();
	else if (isAudioClipNode())
		instanceNode = new AudioClipNode();
	else if (isBackgroundNode())
		instanceNode = new BackgroundNode();
	else if (isBillboardNode())
		instanceNode = new BillboardNode();
	else if (isBoxNode())
		instanceNode = new BoxNode();
	else if (isCollisionNode())
		instanceNode = new CollisionNode();
	else if (isColorNode())
		instanceNode = new ColorNode();
	else if (isColorInterpolatorNode())
		instanceNode = new ColorInterpolatorNode();
	else if (isConeNode())
		instanceNode = new ConeNode();
	else if (isCoordinateNode())
		instanceNode = new CoordinateNode();
	else if (isCoordinateInterpolatorNode())
		instanceNode = new CoordinateInterpolatorNode();
	else if (isCylinderNode())
		instanceNode = new CylinderNode();
	else if (isCylinderSensorNode())
		instanceNode = new CylinderSensorNode();
	else if (isDirectionalLightNode())
		instanceNode = new DirectionalLightNode();
	else if (isElevationGridNode())
		instanceNode = new ElevationGridNode();
	else if (isExtrusionNode())
		instanceNode = new ExtrusionNode();
	else if (isFogNode())
		instanceNode = new FogNode();
	else if (isFontStyleNode())
		instanceNode = new FontStyleNode();
	else if (isGroupNode())
		instanceNode = new GroupNode();
	else if (isImageTextureNode())
		instanceNode = new ImageTextureNode();
	else if (isIndexedFaceSetNode())
		instanceNode = new IndexedFaceSetNode();
	else if (isIndexedLineSetNode()) 
		instanceNode = new IndexedLineSetNode();
	else if (isInlineNode()) 
		instanceNode = new InlineNode();
	else if (isLodNode())
		instanceNode = new LodNode();
	else if (isMaterialNode())
		instanceNode = new MaterialNode();
	else if (isMovieTextureNode())
		instanceNode = new MovieTextureNode();
	else if (isNavigationInfoNode())
		instanceNode = new NavigationInfoNode();
	else if (isNormalNode())
		instanceNode = new NormalNode();
	else if (isNormalInterpolatorNode())
		instanceNode = new NormalInterpolatorNode();
	else if (isOrientationInterpolatorNode())
		instanceNode = new OrientationInterpolatorNode();
	else if (isPixelTextureNode())
		instanceNode = new PixelTextureNode();
	else if (isPlaneSensorNode())
		instanceNode = new PlaneSensorNode();
	else if (isPointLightNode())
		instanceNode = new PointLightNode();
	else if (isPointSetNode())
		instanceNode = new PointSetNode();
	else if (isPositionInterpolatorNode())
		instanceNode = new PositionInterpolatorNode();
	else if (isProximitySensorNode())
		instanceNode = new ProximitySensorNode();
	else if (isScalarInterpolatorNode())
		instanceNode = new ScalarInterpolatorNode();
	else if (isScriptNode())
		instanceNode = new ScriptNode();
	else if (isShapeNode())
		instanceNode = new ShapeNode();
	else if (isSoundNode())
		instanceNode = new SoundNode();
	else if (isSphereNode())
		instanceNode = new SphereNode();
	else if (isSphereSensorNode())
		instanceNode = new SphereSensorNode();
	else if (isSpotLightNode())
		instanceNode = new SpotLightNode();
	else if (isSwitchNode())
		instanceNode = new SwitchNode();
	else if (isTextNode())
		instanceNode = new TextNode();
	else if (isTextureCoordinateNode())
		instanceNode = new TextureCoordinateNode();
	else if (isTextureTransformNode())
		instanceNode = new TextureTransformNode();
	else if (isTimeSensorNode())
		instanceNode = new TimeSensorNode();
	else if (isTouchSensorNode())
		instanceNode = new TouchSensorNode();
	else if (isTransformNode())
		instanceNode = new TransformNode();
	else if (isViewpointNode())
		instanceNode = new ViewpointNode();
	else if (isVisibilitySensorNode())
		instanceNode = new VisibilitySensorNode();
	else if (isWorldInfoNode())
		instanceNode = new WorldInfoNode();

	assert(instanceNode);

	if (instanceNode) {
		Node *refNode = this;
		while (refNode->isInstanceNode() == true) 
			refNode = refNode->getReferenceNode();
		instanceNode->setAsInstanceNode(refNode);
		for (Node *cnode=getChildNodes(); cnode; cnode = cnode->next()) {
			Node *childInstanceNode = cnode->createInstanceNode();
			instanceNode->addChildNode(childInstanceNode);
		}
	}		
		
	return instanceNode;
}

////////////////////////////////////////////////
//	DEF node
////////////////////////////////////////////////

DefNode *Node::createDefNode() 
{
	DefNode *defNode = new DefNode();

	Node *refNode = this;
	while (refNode->isInstanceNode() == true) 
		refNode = refNode->getReferenceNode();
	defNode->setAsInstanceNode(refNode);

	return defNode;
}
