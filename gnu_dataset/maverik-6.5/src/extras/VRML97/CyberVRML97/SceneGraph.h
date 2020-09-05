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
*	File:	SceneGraph.h
*
******************************************************************/

#ifndef _SCENEGRAPH_H_
#define _SCENEGRAPH_H_

#include <iostream.h>
#include <fstream.h>
#include "JString.h"
#include "vrmlfields.h"
#include "vrmlnodes.h"
#include "Parser.h"
#include "CJavaVM.h"
#include "UrlFile.h"
#include "BoundingBox.h"

enum {
SCENEGRAPH_OPTION_NONE			= 0x00,
SCENEGRAPH_NORMAL_GENERATION	= 0x01,
SCENEGRAPH_TEXTURE_GENERATION	= 0x02,
};

#ifdef SUPPORT_JSAI
class SceneGraph : public Parser, public CJavaVM {
#else
class SceneGraph : public Parser {
#endif

	int						mOption;
	float					boundingBoxCenter[3];
	float					boundingBoxSize[3];

	JVector<BindableNode>	*mBackgroundNodeVector;
	JVector<BindableNode>	*mFogNodeVector;
	JVector<BindableNode>	*mNavigationInfoNodeVector;
	JVector<BindableNode>	*mViewpointNodeVector;	

	ShapeNode				*mSelectedShapeNode;
	Node					*mSelectedNode;
	
	BackgroundNode			*mDefaultBackgroundNode;
	FogNode					*mDefaultFogNode;
	NavigationInfoNode		*mDefaultNavigationInfoNode;
	ViewpointNode			*mDefaultViewpointNode;

#ifdef SUPPORT_URL
	UrlFile					*mUrl;
#endif

public:

	SceneGraph();

#ifdef SUPPORT_JSAI
	void setJavaEnv(char *javaClassPath = NULL, jint (JNICALL *printfn)(FILE *fp, const char *format, va_list args) = NULL);
#endif

	~SceneGraph();

	////////////////////////////////////////////////
	//	Option
	////////////////////////////////////////////////

	void setOption(int option) {
		mOption = option;
	}

	int getOption() {
		return mOption;
	}

	////////////////////////////////////////////////
	//	child node list
	////////////////////////////////////////////////

	int getNAllNodes() {
		int nNode = 0;
		for (Node *node = Parser::getNodes(); node; node = node->nextTraversal())
			nNode++;
		return nNode;
	}

	int getNNodes() {
		int nNode = 0;
		for (Node *node = Parser::getNodes(); node; node = node->next())
			nNode++;
		return nNode;
	}

	Node *getNodes(char *typeName) {
		Node *node = Parser::getNodes();
		if (node == NULL)
			return NULL;
		JString nodeString(node->getType());
		if (nodeString.compareTo(typeName) == 0)
			return node;
		else
			return node->next(typeName);
	}

	Node *getNodes() {
		return Parser::getNodes();
	}

	////////////////////////////////////////////////
	//	find node
	////////////////////////////////////////////////

	Node *findNode(char *name) {
		return Parser::findNodeByName(name);
	}

	bool hasNode(Node *targetNode) {
		for (Node *node = Parser::getNodes(); node; node = node->nextTraversal()) {
			if (node == targetNode)
				return true;
		}
		return false;
	}

	////////////////////////////////////////////////
	//	child node list
	////////////////////////////////////////////////

	GroupingNode *getGroupingNodes() {
		for (Node *node = Parser::getNodes(); node; node = node->next()) {
			if (node->isGroupingNode())
				return (GroupingNode *)node;
		}
		return NULL;
	}

	AnchorNode *getAnchorNodes() {
		return (AnchorNode *)getNodes(anchorNodeString);
	}

	AppearanceNode *getAppearanceNodes() {
		return (AppearanceNode *)getNodes(appearanceNodeString);
	}

	AudioClipNode *getAudioClipNodes() {
		return (AudioClipNode *)getNodes(audioClipNodeString);
	}

	BackgroundNode *getBackgroundNodes() {
		return (BackgroundNode *)getNodes(backgroundNodeString);
	}

	BillboardNode *getBillboardNodes() {
		return (BillboardNode *)getNodes(billboardNodeString);
	}

	BoxNode *getBoxeNodes() {
		return (BoxNode *)getNodes(boxNodeString);
	}

	CollisionNode *getCollisionNodes() {
		return (CollisionNode *)getNodes(collisionNodeString);
	}

	ColorNode *getColorNodes() {
		return (ColorNode *)getNodes(colorNodeString);
	}

	ColorInterpolatorNode *getColorInterpolatorNodes() {
		return (ColorInterpolatorNode *)getNodes(colorInterpolatorNodeString);
	}

	ConeNode *getConeNodes() {
		return (ConeNode *)getNodes(coneNodeString);
	}

	CoordinateNode *getCoordinateNodes() {
		return (CoordinateNode *)getNodes(coordinateNodeString);
	}

	CoordinateInterpolatorNode *getCoordinateInterpolatorNodes() {
		return (CoordinateInterpolatorNode *)getNodes(coordinateInterpolatorNodeString);
	}

	CylinderNode *getCylinderNodes() {
		return (CylinderNode *)getNodes(cylinderNodeString);
	}

	CylinderSensorNode *getCylinderSensorNodes() {
		return (CylinderSensorNode *)getNodes(cylinderSensorNodeString);
	}

	DirectionalLightNode *getDirectionalLightNodes() {
		return (DirectionalLightNode *)getNodes(directionalLightNodeString);
	}

	ElevationGridNode *getElevationGridNodes() {
		return (ElevationGridNode *)getNodes(elevationGridNodeString);
	}

	ExtrusionNode *getExtrusionNodes() {
		return (ExtrusionNode *)getNodes(extrusionNodeString);
	}

	FogNode *getFogNodes() {
		return (FogNode *)getNodes(fogNodeString);
	}

	FontStyleNode *getFontStyleNodes() {
		return (FontStyleNode *)getNodes(fontStyleNodeString);
	}

	GroupNode *getGroupNodes() {
		return (GroupNode *)getNodes(groupNodeString);
	}

	ImageTextureNode *getImageTextureNodes() {
		return (ImageTextureNode *)getNodes(imageTextureNodeString);
	}

	IndexedFaceSetNode *getIndexedFaceSetNodes() {
		return (IndexedFaceSetNode *)getNodes(indexedFaceSetNodeString);
	}

	IndexedLineSetNode *getIndexedLineSetNodes() {
		return (IndexedLineSetNode *)getNodes(indexedLineSetNodeString);
	}

	InlineNode *getInlineNodes() {
		return (InlineNode *)getNodes(inlineNodeString);
	}

	LodNode *getLodNodes() {
		return (LodNode *)getNodes(lodNodeString);
	}

	MaterialNode *getMaterialNodes() {
		return (MaterialNode *)getNodes(materialNodeString);
	}

	MovieTextureNode *getMovieTextureNodes() {
		return (MovieTextureNode *)getNodes(movieTextureNodeString);
	}

	NavigationInfoNode *getNavigationInfoNodes() {
		return (NavigationInfoNode *)getNodes(navigationInfoNodeString);
	}

	NormalNode *getNormalNodes() {
		return (NormalNode *)getNodes(normalNodeString);
	}

	NormalInterpolatorNode *getNormalInterpolatorNodes() {
		return (NormalInterpolatorNode *)getNodes(normalInterpolatorNodeString);
	}

	OrientationInterpolatorNode *getOrientationInterpolatorNodes() {
		return (OrientationInterpolatorNode *)getNodes(orientationInterpolatorNodeString);
	}

	PixelTextureNode *getPixelTextureNodes() {
		return (PixelTextureNode *)getNodes(pixelTextureNodeString);
	}

	PlaneSensorNode *getPlaneSensorNodes() {
		return (PlaneSensorNode *)getNodes(planeSensorNodeString);
	}

	PointLightNode *getPointLightNodes() {
		return (PointLightNode *)getNodes(pointLightNodeString);
	}

	PointSetNode *getPointSetNodes() {
		return (PointSetNode *)getNodes(pointSetNodeString);
	}

	PositionInterpolatorNode *getPositionInterpolatorNodes() {
		return (PositionInterpolatorNode *)getNodes(positionInterpolatorNodeString);
	}

	ProximitySensorNode *getProximitySensorNodes() {
		return (ProximitySensorNode *)getNodes(proximitySensorNodeString);
	}

	ScalarInterpolatorNode *getScalarInterpolatorNodes() {
		return (ScalarInterpolatorNode *)getNodes(scalarInterpolatorNodeString);
	}

	ScriptNode *getScriptNodes() {
		return (ScriptNode *)getNodes(scriptNodeString);
	}

	ShapeNode *getShapeNodes() {
		return (ShapeNode *)getNodes(shapeNodeString);
	}

	SoundNode *getSoundNodes() {
		return (SoundNode *)getNodes(soundNodeString);
	}

	SphereNode *getSphereNodes() {
		return (SphereNode *)getNodes(sphereNodeString);
	}

	SphereSensorNode *getSphereSensorNodes() {
		return (SphereSensorNode *)getNodes(sphereSensorNodeString);
	}

	SpotLightNode *getSpotLightNodes() {
		return (SpotLightNode *)getNodes(spotLightNodeString);
	}

	SwitchNode *getSwitchNodes() {
		return (SwitchNode *)getNodes(switchNodeString);
	}

	TextNode *getTextNodes() {
		return (TextNode *)getNodes(textNodeString);
	}

	TextureCoordinateNode *getTextureCoordinateNodes() {
		return (TextureCoordinateNode *)getNodes(textureCoordinateNodeString);
	}

	TextureTransformNode *getTextureTransformNodes() {
		return (TextureTransformNode *)getNodes(textureTransformNodeString);
	}

	TimeSensorNode *getTimeSensorNodes() {
		return (TimeSensorNode *)getNodes(timeSensorNodeString);
	}

	TouchSensorNode *getTouchSensorNodes() {
		return (TouchSensorNode *)getNodes(touchSensorNodeString);
	}

	TransformNode *getTransformNodes() {
		return (TransformNode *)getNodes(transformNodeString);
	}

	ViewpointNode *getViewpointNodes() {
		return (ViewpointNode *)getNodes(viewpointNodeString);
	}

	VisibilitySensorNode *getVisibilitySensorNodes() {
		return (VisibilitySensorNode *)getNodes(visibilitySensorNodeString);
	}

	WorldInfoNode *getWorldInfoNodes() {
		return (WorldInfoNode *)getNodes(worldInfoNodeString);
	}

	////////////////////////////////////////////////
	//	child node list
	////////////////////////////////////////////////

	GroupingNode *findGroupingNode() {
		for (Node *node = (getRootNode())->nextTraversal() ; node; node = node->nextTraversal()) {
			if (node->isGroupingNode())
				return (GroupingNode *)node;
		}
		return NULL;
	}

	AnchorNode *findAnchorNode() {
		return (AnchorNode *)findNodeByType(anchorNodeString);
	}

	AppearanceNode *findAppearanceNode() {
		return (AppearanceNode *)findNodeByType(appearanceNodeString);
	}

	AudioClipNode *findAudioClipNode() {
		return (AudioClipNode *)findNodeByType(audioClipNodeString);
	}

	BackgroundNode *findBackgroundNode() {
		return (BackgroundNode *)findNodeByType(backgroundNodeString);
	}

	BillboardNode *findBillboardNode() {
		return (BillboardNode *)findNodeByType(billboardNodeString);
	}

	BoxNode *findBoxNode() {
		return (BoxNode *)findNodeByType(boxNodeString);
	}

	CollisionNode *findCollisionNode() {
		return (CollisionNode *)findNodeByType(collisionNodeString);
	}

	ColorNode *findColorNode() {
		return (ColorNode *)findNodeByType(colorNodeString);
	}

	ColorInterpolatorNode *findColorInterpolatorNode() {
		return (ColorInterpolatorNode *)findNodeByType(colorInterpolatorNodeString);
	}

	ConeNode *findConeNode() {
		return (ConeNode *)findNodeByType(coneNodeString);
	}

	CoordinateNode *findCoordinateNode() {
		return (CoordinateNode *)findNodeByType(coordinateNodeString);
	}

	CoordinateInterpolatorNode *findCoordinateInterpolatorNode() {
		return (CoordinateInterpolatorNode *)findNodeByType(coordinateInterpolatorNodeString);
	}

	CylinderNode *findCylinderNode() {
		return (CylinderNode *)findNodeByType(cylinderNodeString);
	}

	CylinderSensorNode *findCylinderSensorNode() {
		return (CylinderSensorNode *)findNodeByType(cylinderSensorNodeString);
	}

	DirectionalLightNode *findDirectionalLightNode() {
		return (DirectionalLightNode *)findNodeByType(directionalLightNodeString);
	}

	ElevationGridNode *findElevationGridNode() {
		return (ElevationGridNode *)findNodeByType(elevationGridNodeString);
	}

	ExtrusionNode *findExtrusionNode() {
		return (ExtrusionNode *)findNodeByType(extrusionNodeString);
	}

	FogNode *findFogNode() {
		return (FogNode *)findNodeByType(fogNodeString);
	}

	FontStyleNode *findFontStyleNode() {
		return (FontStyleNode *)findNodeByType(fontStyleNodeString);
	}

	GroupNode *findGroupNode() {
		return (GroupNode *)findNodeByType(groupNodeString);
	}

	ImageTextureNode *findImageTextureNode() {
		return (ImageTextureNode *)findNodeByType(imageTextureNodeString);
	}

	IndexedFaceSetNode *findIndexedFaceSetNode() {
		return (IndexedFaceSetNode *)findNodeByType(indexedFaceSetNodeString);
	}

	IndexedLineSetNode *findIndexedLineSetNode() {
		return (IndexedLineSetNode *)findNodeByType(indexedLineSetNodeString);
	}

	InlineNode *findInlineNode() {
		return (InlineNode *)findNodeByType(inlineNodeString);
	}

	LodNode *findLodNode() {
		return (LodNode *)findNodeByType(lodNodeString);
	}

	MaterialNode *findMaterialNode() {
		return (MaterialNode *)findNodeByType(materialNodeString);
	}

	MovieTextureNode *findMovieTextureNode() {
		return (MovieTextureNode *)findNodeByType(movieTextureNodeString);
	}

	NavigationInfoNode *findNavigationInfoNode() {
		return (NavigationInfoNode *)findNodeByType(navigationInfoNodeString);
	}

	NormalNode *findNormalNode() {
		return (NormalNode *)findNodeByType(normalNodeString);
	}

	NormalInterpolatorNode *findNormalInterpolatorNode() {
		return (NormalInterpolatorNode *)findNodeByType(normalInterpolatorNodeString);
	}

	OrientationInterpolatorNode *findOrientationInterpolatorNode() {
		return (OrientationInterpolatorNode *)findNodeByType(orientationInterpolatorNodeString);
	}

	PixelTextureNode *findPixelTextureNode() {
		return (PixelTextureNode *)findNodeByType(pixelTextureNodeString);
	}

	PlaneSensorNode *findPlaneSensorNode() {
		return (PlaneSensorNode *)findNodeByType(planeSensorNodeString);
	}

	PointLightNode *findPointLightNode() {
		return (PointLightNode *)findNodeByType(pointLightNodeString);
	}

	PointSetNode *findPointSetNode() {
		return (PointSetNode *)findNodeByType(pointSetNodeString);
	}

	PositionInterpolatorNode *findPositionInterpolatorNode() {
		return (PositionInterpolatorNode *)findNodeByType(positionInterpolatorNodeString);
	}

	ProximitySensorNode *findProximitySensorNode() {
		return (ProximitySensorNode *)findNodeByType(proximitySensorNodeString);
	}

	ScalarInterpolatorNode *findScalarInterpolatorNode() {
		return (ScalarInterpolatorNode *)findNodeByType(scalarInterpolatorNodeString);
	}

	ScriptNode *findScriptNode() {
		return (ScriptNode *)findNodeByType(scriptNodeString);
	}

	ShapeNode *findShapeNode() {
		return (ShapeNode *)findNodeByType(shapeNodeString);
	}

	SoundNode *findSoundNode() {
		return (SoundNode *)findNodeByType(soundNodeString);
	}

	SphereNode *findSphereNode() {
		return (SphereNode *)findNodeByType(sphereNodeString);
	}

	SphereSensorNode *findSphereSensorNode() {
		return (SphereSensorNode *)findNodeByType(sphereSensorNodeString);
	}

	SpotLightNode *findSpotLightNode() {
		return (SpotLightNode *)findNodeByType(spotLightNodeString);
	}

	SwitchNode *findSwitchNode() {
		return (SwitchNode *)findNodeByType(switchNodeString);
	}

	TextNode *findTextNode() {
		return (TextNode *)findNodeByType(textNodeString);
	}

	TextureCoordinateNode *findTextureCoordinateNode() {
		return (TextureCoordinateNode *)findNodeByType(textureCoordinateNodeString);
	}

	TextureTransformNode *findTextureTransformNode() {
		return (TextureTransformNode *)findNodeByType(textureTransformNodeString);
	}

	TimeSensorNode *findTimeSensorNode() {
		return (TimeSensorNode *)findNodeByType(timeSensorNodeString);
	}

	TouchSensorNode *findTouchSensorNode() {
		return (TouchSensorNode *)findNodeByType(touchSensorNodeString);
	}

	TransformNode *findTransformNode() {
		return (TransformNode *)findNodeByType(transformNodeString);
	}

	ViewpointNode *findViewpointNode() {
		return (ViewpointNode *)findNodeByType(viewpointNodeString);
	}

	VisibilitySensorNode *findVisibilitySensorNode() {
		return (VisibilitySensorNode *)findNodeByType(visibilitySensorNodeString);
	}

	WorldInfoNode *findWorldInfoNode() {
		return (WorldInfoNode *)findNodeByType(worldInfoNodeString);
	}

	////////////////////////////////////////////////
	//	child node list
	////////////////////////////////////////////////

	AnchorNode *findAnchorNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (AnchorNode *node = findAnchorNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	AppearanceNode *findAppearanceNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (AppearanceNode *node = findAppearanceNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	AudioClipNode *findAudioClipNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (AudioClipNode *node = findAudioClipNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	BackgroundNode *findBackgroundNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (BackgroundNode *node = findBackgroundNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	BillboardNode *findBillboardNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (BillboardNode *node = findBillboardNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	BoxNode *findBoxNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (BoxNode *node = findBoxNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	CollisionNode *findCollisionNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (CollisionNode *node = findCollisionNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	ColorNode *findColorNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (ColorNode *node = findColorNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	ColorInterpolatorNode *findColorInterpolatorNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (ColorInterpolatorNode *node = findColorInterpolatorNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	ConeNode *findConeNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (ConeNode *node = findConeNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	CoordinateNode *findCoordinateNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (CoordinateNode *node = findCoordinateNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	CoordinateInterpolatorNode *findCoordinateInterpolatorNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (CoordinateInterpolatorNode *node = findCoordinateInterpolatorNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	CylinderNode *findCylinderNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (CylinderNode *node = findCylinderNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	CylinderSensorNode *findCylinderSensorNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (CylinderSensorNode *node = findCylinderSensorNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	DirectionalLightNode *findDirectionalLightNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (DirectionalLightNode *node = findDirectionalLightNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	ElevationGridNode *findElevationGridNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (ElevationGridNode *node = findElevationGridNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	ExtrusionNode *findExtrusionNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (ExtrusionNode *node = findExtrusionNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	FogNode *findFogNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (FogNode *node = findFogNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	FontStyleNode *findFontStyleNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (FontStyleNode *node = findFontStyleNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	GroupNode *findGroupNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (GroupNode *node = findGroupNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	ImageTextureNode *findImageTextureNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (ImageTextureNode *node = findImageTextureNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	IndexedFaceSetNode *findIndexedFaceSetNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (IndexedFaceSetNode *node = findIndexedFaceSetNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	IndexedLineSetNode *findIndexedLineSetNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (IndexedLineSetNode *node = findIndexedLineSetNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	InlineNode *findInlineNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (InlineNode *node = findInlineNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	LodNode *findLodNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (LodNode *node = findLodNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	MaterialNode *findMaterialNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (MaterialNode *node = findMaterialNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	MovieTextureNode *findMovieTextureNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (MovieTextureNode *node = findMovieTextureNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	NavigationInfoNode *findNavigationInfoNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (NavigationInfoNode *node = findNavigationInfoNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	NormalNode *findNormalNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (NormalNode *node = findNormalNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	NormalInterpolatorNode *findNormalInterpolatorNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (NormalInterpolatorNode *node = findNormalInterpolatorNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	OrientationInterpolatorNode *findOrientationInterpolatorNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (OrientationInterpolatorNode *node = findOrientationInterpolatorNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	PixelTextureNode *findPixelTextureNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (PixelTextureNode *node = findPixelTextureNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	PlaneSensorNode *findPlaneSensorNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (PlaneSensorNode *node = findPlaneSensorNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	PointLightNode *findPointLightNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (PointLightNode *node = findPointLightNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	PointSetNode *findPointSetNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (PointSetNode *node = findPointSetNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	PositionInterpolatorNode *findPositionInterpolatorNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (PositionInterpolatorNode *node = findPositionInterpolatorNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	ProximitySensorNode *findProximitySensorNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (ProximitySensorNode *node = findProximitySensorNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	ScalarInterpolatorNode *findScalarInterpolatorNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (ScalarInterpolatorNode *node = findScalarInterpolatorNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	ScriptNode *findScriptNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (ScriptNode *node = findScriptNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	ShapeNode *findShapeNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (ShapeNode *node = findShapeNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	SoundNode *findSoundNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (SoundNode *node = findSoundNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	SphereNode *findSphereNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (SphereNode *node = findSphereNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	SphereSensorNode *findSphereSensorNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (SphereSensorNode *node = findSphereSensorNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	SpotLightNode *findSpotLightNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (SpotLightNode *node = findSpotLightNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	SwitchNode *findSwitchNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (SwitchNode *node = findSwitchNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	TextNode *findTextNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (TextNode *node = findTextNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	TextureCoordinateNode *findTextureCoordinateNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (TextureCoordinateNode *node = findTextureCoordinateNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	TextureTransformNode *findTextureTransformNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (TextureTransformNode *node = findTextureTransformNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	TimeSensorNode *findTimeSensorNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (TimeSensorNode *node = findTimeSensorNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	TouchSensorNode *findTouchSensorNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (TouchSensorNode *node = findTouchSensorNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	TransformNode *findTransformNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (TransformNode *node = findTransformNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	ViewpointNode *findViewpointNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (ViewpointNode *node = findViewpointNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	VisibilitySensorNode *findVisibilitySensorNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (VisibilitySensorNode *node = findVisibilitySensorNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	WorldInfoNode *findWorldInfoNode(char *name) {
		if (!name || strlen(name) <= 0)
			return NULL;
		for (WorldInfoNode *node = findWorldInfoNode(); node; node = node->nextTraversal()) {
			char *nodeName = node->getName();
			if (nodeName && strlen(nodeName)) {
				if (!strcmp(name, nodeName))
					return node;
			}
		}
		return NULL;
	}

	////////////////////////////////////////////////
	//	Node Number
	////////////////////////////////////////////////

	unsigned int getNodeNumber(Node *node) {
		unsigned int nNode = 1;
		for (Node *n = getNodes(); n; n = n->nextTraversal()) {
			if (n == node)
				return nNode;
			nNode++;
		}
		return 0;
	}

	////////////////////////////////////////////////
	//	initialize
	////////////////////////////////////////////////

	void initialize(void (*callbackFn)(int nNode, void *info) = NULL, void *callbackFnInfo = NULL);

	void uninitialize(void (*callbackFn)(int nNode, void *info) = NULL, void *callbackFnInfo = NULL);

	////////////////////////////////////////////////
	//	update
	////////////////////////////////////////////////

	void update() {
		for (Node *node = Parser::getNodes(); node; node = node->nextTraversal()) {
			node->update();
		}
	}

	void updateRoute(Node *eventOutNode, Field *eventOutField) {
		for (Route *route = Parser::getRoutes(); route; route = route->next()) {
			if (route->getEventOutNode() == eventOutNode && route->getEventOutField() == eventOutField) {
				route->update();
				route->getEventInNode()->update();
				updateRoute(route->getEventInNode(), route->getEventInField());
			}
		}
	}

	///////////////////////////////////////////////
	//	Output node infomations
	///////////////////////////////////////////////
	
	void print() {
		uninitialize();

		for (Node *node = Parser::getNodes(); node; node = node->next()) {
			node->print();
		}
		for (Route *route = Parser::getRoutes(); route; route = route->next()) {
			route->output(cout);
		}

		initialize();
	}

	///////////////////////////////////////////////
	//	Delete/Remove Node
	///////////////////////////////////////////////

	void removeNode(Node *node) {
		deleteRoutes(node);
		node->remove();
	}

	void deleteNode(Node *node) {
		deleteRoutes(node);
		delete node;
	}

	///////////////////////////////////////////////
	//	Bindable Nodes
	///////////////////////////////////////////////

	void setBindableNode(JVector<BindableNode> *nodeVector, BindableNode *node, int bind);

	void setBindableNode(BindableNode *node, int bind) {
		if (node->isBackgroundNode())		setBackgroundNode((BackgroundNode *)node, bind);
		if (node->isFogNode())				setFogNode((FogNode *)node, bind);
		if (node->isNavigationInfoNode())	setNavigationInfoNode((NavigationInfoNode *)node, bind);
		if (node->isViewpointNode())		setViewpointNode((ViewpointNode *)node, bind);
	}

	void setBackgroundNode(BackgroundNode *bg, int bind) {
		setBindableNode(mBackgroundNodeVector, bg, bind);
	}

	void setFogNode(FogNode *fog, int bind) {
		setBindableNode(mFogNodeVector, fog, bind);
	}

	void setNavigationInfoNode(NavigationInfoNode *navInfo, int bind) {
		setBindableNode(mNavigationInfoNodeVector, navInfo, bind);
	}

	void setViewpointNode(ViewpointNode *view, int bind) {
		setBindableNode(mViewpointNodeVector, view, bind);
	}

	BackgroundNode *getBackgroundNode() {
		return (BackgroundNode *)mBackgroundNodeVector->lastElement();
	}

	FogNode *getFogNode() {
		return (FogNode *)mFogNodeVector->lastElement();
	}

	NavigationInfoNode *getNavigationInfoNode() {
		return (NavigationInfoNode *)mNavigationInfoNodeVector->lastElement();
	}

	ViewpointNode *getViewpointNode() {
		return (ViewpointNode *)mViewpointNodeVector->lastElement();
	}

	////////////////////////////////////////////////
	//	BoundingBoxSize
	////////////////////////////////////////////////

	void setBoundingBoxSize(float value[]) {
		boundingBoxSize[0] = value[0];
		boundingBoxSize[1] = value[1];
		boundingBoxSize[2] = value[2];
	}
	void setBoundingBoxSize(float x, float y, float z) {
		boundingBoxSize[0] = x;
		boundingBoxSize[1] = y;
		boundingBoxSize[2] = z;
	}
	void getBoundingBoxSize(float value[]) {
		value[0] = boundingBoxSize[0];
		value[1] = boundingBoxSize[1];
		value[2] = boundingBoxSize[2];
	}

	////////////////////////////////////////////////
	//	BoundingBoxCenter
	////////////////////////////////////////////////

	void setBoundingBoxCenter(float value[]) {
		boundingBoxCenter[0] = value[0];
		boundingBoxCenter[1] = value[1];
		boundingBoxCenter[2] = value[2];
	}
	void setBoundingBoxCenter(float x, float y, float z) {
		boundingBoxCenter[0] = x;
		boundingBoxCenter[1] = y;
		boundingBoxCenter[2] = z;
	}
	void getBoundingBoxCenter(float value[]) {
		value[0] = boundingBoxCenter[0];
		value[1] = boundingBoxCenter[1];
		value[2] = boundingBoxCenter[2];
	}

	////////////////////////////////////////////////
	//	BoundingBox
	////////////////////////////////////////////////

	void setBoundingBox(BoundingBox *bbox) {
		float center[3];
		float size[3];
		bbox->getCenter(center);
		bbox->getSize(size);
		setBoundingBoxCenter(center);
		setBoundingBoxSize(size);
	}

	void recomputeBoundingBox();

	///////////////////////////////////////////////
	//	Load
	///////////////////////////////////////////////

	void clear();

	void load(char *filename, bool bInitialize = true, void (*callbackFn)(int nLine, void *info) = NULL, void *callbackFnInfo = NULL);

	void add(char *filename, bool bInitialize = true, void (*callbackFn)(int nLine, void *info) = NULL, void *callbackFnInfo = NULL);

	///////////////////////////////////////////////
	//	Save node infomations
	///////////////////////////////////////////////
	
	int save(char *filename, void (*callbackFn)(int nNode, void *info) = NULL, void *callbackFnInfo = NULL);

	///////////////////////////////////////////////
	//	URL
	///////////////////////////////////////////////

#ifdef SUPPORT_URL

	void	setUrl(char *url)				{ mUrl->setUrl(url); }
	char	*getUrl()						{ return mUrl->getUrl(); }
	bool	getUrlStream(char *urlStrnig)	{ return mUrl->getStream(urlStrnig); }
	char	*getUrlOutputFilename()			{ return mUrl->getOutputFilename(); }
	bool	deleteUrlOutputFilename()		{ return mUrl->deleteOutputFilename(); }

#endif

	//////////////////////////////////////////////////
	// Selected Shape/Node
	//////////////////////////////////////////////////

	void			setSelectedShapeNode(ShapeNode *shape)	{ mSelectedShapeNode = shape; }
	ShapeNode		*getSelectedShapeNode()					{ return mSelectedShapeNode; }

	void			setSelectedNode(Node *node)				{ mSelectedNode = node; }
	Node			*getSelectedNode()						{ return mSelectedNode; }

	//////////////////////////////////////////////////
	// Default Bindable Nodes
	//////////////////////////////////////////////////

	BackgroundNode		*getDefaultBackgroundNode()		{ return mDefaultBackgroundNode; }
	FogNode				*getDefaultFogNode()			{ return mDefaultFogNode; }
	NavigationInfoNode	*getDefaultNavigationInfoNode()	{ return mDefaultNavigationInfoNode; }
	ViewpointNode		*getDefaultViewpointNode()		{ return mDefaultViewpointNode; }

	//////////////////////////////////////////////////
	// Zoom All
	//////////////////////////////////////////////////

	void			zoomAllViewpoint();
};

#endif
