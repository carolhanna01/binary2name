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
*	File:	SceneGraph.cpp
*
******************************************************************/

#include "SceneGraph.h"

////////////////////////////////////////////////////////////
//	SceneGraph::SceneGraph
////////////////////////////////////////////////////////////

SceneGraph::SceneGraph() 
{
//	setHeaderFlag(false);
	setOption(SCENEGRAPH_OPTION_NONE);
	setBoundingBoxCenter(0.0f, 0.0f, 0.0f);
	setBoundingBoxSize(-1.0f, -1.0f, -1.0f);
	setSelectedShapeNode(NULL);
	setSelectedNode(NULL);

	mBackgroundNodeVector		= new JVector<BindableNode>;
	mFogNodeVector				= new JVector<BindableNode>;
	mNavigationInfoNodeVector	= new JVector<BindableNode>;
	mViewpointNodeVector		= new JVector<BindableNode>;	

	mDefaultBackgroundNode		= new BackgroundNode();
	mDefaultFogNode				= new FogNode();
	mDefaultNavigationInfoNode	= new NavigationInfoNode();
	mDefaultViewpointNode		= new ViewpointNode();

#ifdef SUPPORT_URL
	mUrl = new UrlFile();
#endif
}

////////////////////////////////////////////////////////////
//	SceneGraph::SceneGraph
////////////////////////////////////////////////////////////

#ifdef SUPPORT_JSAI

void SceneGraph::setJavaEnv(char *javaClassPath, jint (JNICALL *printfn)(FILE *fp, const char *format, va_list args)) 
{
	CreateJavaVM(javaClassPath, printfn);
}

#endif

////////////////////////////////////////////////////////////
//	SceneGraph::~SceneGraph
////////////////////////////////////////////////////////////

SceneGraph::~SceneGraph() 
{
	Node *node=getNodes();
	while (node) {
		delete node;
		node = getNodes();
	}
	Route *route=getRoutes();
	while (route) {
		Route *nextRoute=route->next();
		delete route;
		route = nextRoute;
	}

	delete mBackgroundNodeVector;
	delete mFogNodeVector;
	delete mNavigationInfoNodeVector;
	delete mViewpointNodeVector;	

	delete mDefaultBackgroundNode;
	delete mDefaultFogNode;
	delete mDefaultNavigationInfoNode;
	delete mDefaultViewpointNode;

#ifdef SUPPORT_URL
	delete mUrl;
#endif

#ifdef SUPPORT_JSAI
	DeleteJavaVM();
#endif
}

////////////////////////////////////////////////////////////
//	SceneGraph::load
////////////////////////////////////////////////////////////

void SceneGraph::clear() 
{
	clearNodeList();
	clearRouteList();
}

////////////////////////////////////////////////////////////
//	SceneGraph::load
////////////////////////////////////////////////////////////

void SceneGraph::load(char *filename, bool bInitialize, void (*callbackFn)(int nLine, void *info), void *callbackFnInfo)
{
	clear();

	Parser::load(filename, callbackFn, callbackFnInfo);

	if (bInitialize)
		initialize();

	setBackgroundNode(findBackgroundNode(), true);
	setFogNode(findFogNode(), true);
	setNavigationInfoNode(findNavigationInfoNode(), true);
	setViewpointNode(findViewpointNode(), true);
}

void SceneGraph::add(char *filename, bool bInitialize, void (*callbackFn)(int nLine, void *info), void *callbackFnInfo)
{
	Parser::load(filename, callbackFn, callbackFnInfo);

	if (bInitialize)
		initialize();

	setBackgroundNode(findBackgroundNode(), true);
	setFogNode(findFogNode(), true);
	setNavigationInfoNode(findNavigationInfoNode(), true);
	setViewpointNode(findViewpointNode(), true);
}

////////////////////////////////////////////////////////////
//	SceneGraph::save
////////////////////////////////////////////////////////////
	
int SceneGraph::save(char *filename, void (*callbackFn)(int nNode, void *info), void *callbackFnInfo)
{
	
	ofstream outputFile(filename);

	if (!outputFile)
		return 0;

	uninitialize();

	outputFile << "#VRML V2.0 utf8" << endl;

	int nNode = 0;
	for (Node *node = Parser::getNodes(); node; node = node->next()) {
		node->output(outputFile, 0);
		nNode++;
		if (callbackFn)
			callbackFn(nNode, callbackFnInfo);
	}
	for (Route *route = Parser::getRoutes(); route; route = route->next()) {
		route->output(outputFile);
	}

	initialize();

	return 1;
}

////////////////////////////////////////////////////////////
//	SceneGraph::initialize
////////////////////////////////////////////////////////////

void SceneGraph::initialize(void (*callbackFn)(int nNode, void *info), void *callbackFnInfo) 
{
	Node *node;

	int nNode = 0;
	for (node = Parser::getNodes(); node; node = node->nextTraversal()) {
		node->setSceneGraph(this);
		if (node->isInstanceNode() == false)		
			node->initialize();
		nNode++;
		if (callbackFn)
			callbackFn(nNode, callbackFnInfo);
	}

	// Convert from InstanceNode into DefNode 
	node = Parser::getNodes();
	while(node != NULL) {
		Node *nextNode = node->nextTraversal();
		if (node->isInstanceNode() == true && node->isDefNode() == false) {
			Node *referenceNode	= node->getReferenceNode();
			Node *parentNode	= node->getParentNode();
			Node *defNode;
			
			defNode = referenceNode->createDefNode();
			if (parentNode != NULL)
				parentNode->addChildNode(defNode, false);
			else
				addNode(defNode, false);

			node->remove();
			delete node;

			nextNode = defNode->nextTraversal();
		}
		node = nextNode;
	}

	// Convert from DefNode into InstanceNode 
	node = Parser::getNodes();
	while(node != NULL) {
		Node *nextNode = node->nextTraversal();

		if (node->isDefNode() == true) {
			Node *defNode = findNode(node->getName());
			assert(defNode);
			if (defNode) {	
				Node *instanceNode = defNode->createInstanceNode();
				Node *parentNode = node->getParentNode();
				if (parentNode != NULL)
					parentNode->moveChildNode(instanceNode);
				else
					moveNode(instanceNode);
				node->remove();
				delete node;
			}
		}

		node = nextNode;
	}

	recomputeBoundingBox();

	for (Route *route = Parser::getRoutes(); route; route = route->next())
		route->initialize();
}

////////////////////////////////////////////////////////////
//	SceneGraph::uninitialize
////////////////////////////////////////////////////////////

void SceneGraph::uninitialize(void (*callbackFn)(int nNode, void *info), void *callbackFnInfo) 
{
	int nNode = 0;
	for (Node *node = Parser::getNodes(); node; node = node->nextTraversal()) {
		node->uninitialize();
		nNode++;
		if (callbackFn)
			callbackFn(nNode, callbackFnInfo);
	}
}

////////////////////////////////////////////////////////////
//	SceneGraph::recomputeBoundingBox
////////////////////////////////////////////////////////////

void SceneGraph::recomputeBoundingBox() 
{
	Node	*node;
	float	center[3];
	float	size[3];

	BoundingBox bbox;

	for (node=getNodes(); node; node=node->nextTraversal()) {
		if (node->isGroupingNode()) {
			GroupingNode *gnode = (GroupingNode *)node;
			gnode->getBoundingBoxCenter(center);
			gnode->getBoundingBoxSize(size);
			bbox.addBoundingBox(center, size);
		}
	}

	setBoundingBox(&bbox);
}

///////////////////////////////////////////////
//	Bindable Nodes
///////////////////////////////////////////////

void SceneGraph::setBindableNode(JVector<BindableNode> *nodeVector, BindableNode *node, int bind)
{
	if (!node)
		return;

	BindableNode *topNode = nodeVector->lastElement();

	if (bind) {
		if (topNode != node) {
			if (topNode) {
				topNode->setIsBound(0);
				topNode->sendEvent(topNode->getIsBoundField());
			}

			nodeVector->removeElement(node);
			nodeVector->addElement(node, 0);

			node->setIsBound(1);
			node->sendEvent(node->getIsBoundField());
		}
	}
	else {
		if (topNode == node) {
			node->setIsBound(0);
			node->sendEvent(node->getIsBoundField());

			nodeVector->removeElement(node);

			BindableNode *newTopNode = nodeVector->lastElement();
			if (newTopNode) {
				newTopNode->setIsBound(1);
				newTopNode->sendEvent(newTopNode->getIsBoundField());
			}
		}
		else {
			nodeVector->removeElement(node);
		}
	}
}

///////////////////////////////////////////////
//	Zoom All Viewpoint
///////////////////////////////////////////////

void SceneGraph::zoomAllViewpoint() 
{
	float	bboxCenter[3];
	float	bboxSize[3];

	getBoundingBoxCenter(bboxCenter);
	getBoundingBoxSize(bboxSize);

	ViewpointNode *view = getViewpointNode();
	if (view == NULL)
		view = getDefaultViewpointNode();

	float fov = view->getFieldOfView();
	float zoffset = bboxSize[0] / (float)tan(fov);
	view->setPosition(bboxCenter[0], bboxCenter[1], bboxCenter[2] + zoffset*5.0f);
	view->setOrientation(0.0f, 0.0f, 1.0f, 0.0f);
}
