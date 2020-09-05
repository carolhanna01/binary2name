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
*	File:	vrmlsetinfo.cpp
*
******************************************************************/

#include "SceneGraph.h"
#include "vrmlsetinfo.h"
#include "vrmlnodetype.h"

void AddSFColor(float color[3])
{
    switch (GetCurrentNodeType()) {
	case VRML_NODETYPE_COLOR:
		{
			((ColorNode *)GetCurrentNodeObject())->addColor(color);
		}
		break;
    case VRML_NODETYPE_BACKGROUND_GROUNDCOLOR:
		{
			BackgroundNode *bg = (BackgroundNode *)GetCurrentNodeObject();
			bg->addGroundColor(color);
		}		
	    break;
    case VRML_NODETYPE_BACKGROUND_SKYCOLOR:
		{
			BackgroundNode *bg = (BackgroundNode *)GetCurrentNodeObject();
			bg->addSkyColor(color);
		}		
	    break;
	case VRML_NODETYPE_INTERPOLATOR_KEYVALUE:
		switch (GetPrevNodeType()) {
		case VRML_NODETYPE_COLORINTERPOLATOR:
			{
				ColorInterpolatorNode *colorInterp = (ColorInterpolatorNode *)GetCurrentNodeObject();
				colorInterp->addKeyValue(color);
			}
			break;
		}
		break;
    }
}

void AddSFRotation(float rotation[4])
{	
    switch (GetCurrentNodeType()) {
	case VRML_NODETYPE_EXTRUSION_ORIENTATION:
		{
			ExtrusionNode *ex = (ExtrusionNode *)GetCurrentNodeObject();
			ex->addOrientation(rotation);
		}
		break;
	case VRML_NODETYPE_INTERPOLATOR_KEYVALUE:
		switch (GetPrevNodeType()) {
		case VRML_NODETYPE_ORIENTATIONINTERPOLATOR:
			{
				OrientationInterpolatorNode *oriInterp = (OrientationInterpolatorNode *)GetCurrentNodeObject();
				oriInterp->addKeyValue(rotation);
			}
			break;
		}
	}
}

void AddSFVec3f(float vector[3])
{	
    switch (GetCurrentNodeType()) {
	case VRML_NODETYPE_NORMAL:
		{
			((NormalNode *)GetCurrentNodeObject())->addVector(vector);
		}
	    break;
	case VRML_NODETYPE_COORDINATE:
		{
			((CoordinateNode *)GetCurrentNodeObject())->addPoint(vector);
		}
		break;
	case VRML_NODETYPE_INTERPOLATOR_KEYVALUE:
		switch (GetPrevNodeType()) {
		case VRML_NODETYPE_COORDINATEINTERPOLATOR:
			{
				CoordinateInterpolatorNode *coordInterp = (CoordinateInterpolatorNode *)GetCurrentNodeObject();
				coordInterp->addKeyValue(vector);
			}
			break;
		case VRML_NODETYPE_NORMALINTERPOLATOR:
			{
				NormalInterpolatorNode *normInterp = (NormalInterpolatorNode *)GetCurrentNodeObject();
				normInterp->addKeyValue(vector);
			}
			break;
		case VRML_NODETYPE_POSITIONINTERPOLATOR:
			{
				PositionInterpolatorNode *posInterp = (PositionInterpolatorNode *)GetCurrentNodeObject();
				posInterp->addKeyValue(vector);
			}
			break;
		}
		break;
	case VRML_NODETYPE_EXTRUSION_SPINE:
		{
			ExtrusionNode *ex = (ExtrusionNode *)GetCurrentNodeObject();
			ex->addSpine(vector);
		}
		break;
	}
}

void AddSFVec2f(float vector[2])
{	
	switch (GetCurrentNodeType()) {
	case VRML_NODETYPE_TEXTURECOODINATE:
		{
			((TextureCoordinateNode *)GetCurrentNodeObject())->addPoint(vector);
		}
	    break;
	case VRML_NODETYPE_EXTRUSION_CROSSSECTION:
		{
			ExtrusionNode *ex = (ExtrusionNode *)GetCurrentNodeObject();
			ex->addCrossSection(vector);
		}
		break;
	case VRML_NODETYPE_EXTRUSION_SCALE:
		{
			ExtrusionNode *ex = (ExtrusionNode *)GetCurrentNodeObject();
			ex->addScale(vector);
		}
		break;
	}
}

void AddSFInt32(int	value)
{	
    switch (GetPrevNodeType()) {
    case VRML_NODETYPE_INDEXEDFACESET:
		{
			IndexedFaceSetNode *idxFaceSet = (IndexedFaceSetNode *)GetCurrentNodeObject();
			switch (GetCurrentNodeType()) {
			case VRML_NODETYPE_COLOR_INDEX:
				idxFaceSet->addColorIndex(value); break;
			case VRML_NODETYPE_COORDINATE_INDEX:
				idxFaceSet->addCoordIndex(value); break;
			case VRML_NODETYPE_NORMAL_INDEX:
				idxFaceSet->addNormalIndex(value); break;
			case VRML_NODETYPE_TEXTURECOODINATE_INDEX:
				idxFaceSet->addTexCoordIndex(value); break;
			}
		}
	    break;
    case VRML_NODETYPE_INDEXEDLINESET:
		{
			IndexedLineSetNode *idxLineSet = (IndexedLineSetNode *)GetCurrentNodeObject();
			switch (GetCurrentNodeType()) {
			case VRML_NODETYPE_COLOR_INDEX:
				idxLineSet->addColorIndex(value); break;
			case VRML_NODETYPE_COORDINATE_INDEX:
				idxLineSet->addCoordIndex(value); break;
			}
		}		
		break;
    case VRML_NODETYPE_PIXELTEXTURE:
		{
			PixelTextureNode *pixTexture = (PixelTextureNode *)GetCurrentNodeObject();
			switch (GetCurrentNodeType()) {
			case VRML_NODETYPE_PIXELTEXTURE_IMAGE:
				pixTexture->addImage(value); break;
			}
		}	
		break;
    }

}

void AddSFFloat(float value)
{	
    switch (GetCurrentNodeType()) {
	case VRML_NODETYPE_ELEVATIONGRID_HEIGHT:
		{
			ElevationGridNode *elev = (ElevationGridNode *)GetCurrentNodeObject();
			elev->addHeight(value);
		}
		break;
    case VRML_NODETYPE_BACKGROUND_GROUNDANGLE:
		{
			BackgroundNode *bg = (BackgroundNode *)GetCurrentNodeObject();
			bg->addGroundAngle(value);
		}		
	    break;
    case VRML_NODETYPE_BACKGROUND_SKYANGLE:
		{
			BackgroundNode *bg = (BackgroundNode *)GetCurrentNodeObject();
			bg->addSkyAngle(value);
		}		
	    break;
	case VRML_NODETYPE_INTERPOLATOR_KEY:
		switch (GetPrevNodeType()) {
		case VRML_NODETYPE_COLORINTERPOLATOR:
			{
				ColorInterpolatorNode *colorInterp = (ColorInterpolatorNode *)GetCurrentNodeObject();
				colorInterp->addKey(value);
			}
			break;
		case VRML_NODETYPE_COORDINATEINTERPOLATOR:
			{
				CoordinateInterpolatorNode *coordInterp = (CoordinateInterpolatorNode *)GetCurrentNodeObject();
				coordInterp->addKey(value);
			}
			break;
		case VRML_NODETYPE_NORMALINTERPOLATOR:
			{
				NormalInterpolatorNode *normInterp = (NormalInterpolatorNode *)GetCurrentNodeObject();
				normInterp->addKey(value);
			}
			break;
		case VRML_NODETYPE_ORIENTATIONINTERPOLATOR:
			{
				OrientationInterpolatorNode *oriInterp = (OrientationInterpolatorNode *)GetCurrentNodeObject();
				oriInterp->addKey(value);
			}
			break;
		case VRML_NODETYPE_POSITIONINTERPOLATOR:
			{
				PositionInterpolatorNode *posInterp = (PositionInterpolatorNode *)GetCurrentNodeObject();
				posInterp->addKey(value);
			}
			break;
		case VRML_NODETYPE_SCALARINTERPOLATOR:
			{
				ScalarInterpolatorNode *scalarInterp = (ScalarInterpolatorNode *)GetCurrentNodeObject();
				scalarInterp->addKey(value);
			}
			break;
		}
		break;
	case VRML_NODETYPE_INTERPOLATOR_KEYVALUE:
		switch (GetPrevNodeType()) {
		case VRML_NODETYPE_SCALARINTERPOLATOR:
			{
				ScalarInterpolatorNode *scalarInterp = (ScalarInterpolatorNode *)GetCurrentNodeObject();
				scalarInterp->addKeyValue(value);
			}
			break;
		}
		break;
	case VRML_NODETYPE_LOD_RANGE:
		{
			((LodNode *)GetCurrentNodeObject())->addRange(value);
		}
		break;
	case VRML_NODETYPE_NAVIGATIONINFO_AVATARSIZE:
		{
			NavigationInfoNode *navInfo = (NavigationInfoNode *)GetCurrentNodeObject();
			navInfo->addAvatarSize(value);
		}
		break;
	case VRML_NODETYPE_TEXT_LENGTH:
		{
			TextNode *text = (TextNode *)GetCurrentNodeObject();
			text->addLength(value);
		}
		break;
    }
}


void AddSFString(char *string)
{	
	switch (GetCurrentNodeType()) {
	case VRML_NODETYPE_ANCHOR_PARAMETER:
		{
			((AnchorNode *)GetCurrentNodeObject())->addParameter(string);
		}
		break;
	case VRML_NODETYPE_ANCHOR_URL:
		{
			((AnchorNode *)GetCurrentNodeObject())->addUrl(string);
		}
		break;
	case VRML_NODETYPE_INLINE_URL:
		{
			((InlineNode *)GetCurrentNodeObject())->addUrl(string);
		}
		break;
	case VRML_NODETYPE_AUDIOCLIP_URL:
		{
			AudioClipNode *aclip = (AudioClipNode *)GetCurrentNodeObject();
			aclip->addUrl(string);
		}
		break;
	case VRML_NODETYPE_BACKGROUND_BACKURL:
		{
			BackgroundNode *bg = (BackgroundNode *)GetCurrentNodeObject();
			bg->addBackUrl(string);
		}
		break;
	case VRML_NODETYPE_BACKGROUND_BOTTOMURL:
		{
			BackgroundNode *bg = (BackgroundNode *)GetCurrentNodeObject();
			bg->addBottomUrl(string);
		}
		break;
	case VRML_NODETYPE_BACKGROUND_FRONTURL:
		{
			BackgroundNode *bg = (BackgroundNode *)GetCurrentNodeObject();
			bg->addFrontUrl(string);
		}
		break;
	case VRML_NODETYPE_BACKGROUND_LEFTURL:
		{
			BackgroundNode *bg = (BackgroundNode *)GetCurrentNodeObject();
			bg->addLeftUrl(string);
		}
		break;
	case VRML_NODETYPE_BACKGROUND_RIGHTURL:
		{
			BackgroundNode *bg = (BackgroundNode *)GetCurrentNodeObject();
			bg->addRightUrl(string);
		}
		break;
	case VRML_NODETYPE_BACKGROUND_TOPURL:
		{
			BackgroundNode *bg = (BackgroundNode *)GetCurrentNodeObject();
			bg->addTopUrl(string);
		}
		break;
	case VRML_NODETYPE_FONTSTYLE_JUSTIFY:
		{
			FontStyleNode *fs = (FontStyleNode *)GetCurrentNodeObject();
			fs->addJustify(string);
		}
		break;
	case VRML_NODETYPE_IMAGETEXTURE_URL:
		{
			ImageTextureNode *image = (ImageTextureNode *)GetCurrentNodeObject();
			image->addUrl(string);
		}
		break;
	case VRML_NODETYPE_MOVIETEXTURE_URL:
		{
			MovieTextureNode *image = (MovieTextureNode *)GetCurrentNodeObject();
			image->addUrl(string);
		}
		break;
	case VRML_NODETYPE_NAVIGATIONINFO_TYPE:
		{
			NavigationInfoNode *navInfo = (NavigationInfoNode *)GetCurrentNodeObject();
			navInfo->addType(string);
		}
		break;
	case VRML_NODETYPE_SCRIPT_URL:
		{
			ScriptNode *script = (ScriptNode *)GetCurrentNodeObject();
			script->addUrl(string);
		}
		break;
	case VRML_NODETYPE_TEXT_STRING:
		{
			TextNode *text = (TextNode *)GetCurrentNodeObject();
			text->addString(string);
		}
		break;
	case VRML_NODETYPE_WORLDINFO_INFO:
		{
			WorldInfoNode *worldInfo = (WorldInfoNode *)GetCurrentNodeObject();
			worldInfo->addInfo(string);
		}
		break;
	}
}
