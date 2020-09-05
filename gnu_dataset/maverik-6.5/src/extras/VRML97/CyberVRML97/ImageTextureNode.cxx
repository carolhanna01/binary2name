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
*	File:	ImageTextureNode.cpp
*
******************************************************************/
#ifdef SUPPORT_OPENGL
#ifdef WIN32
#include <windows.h>
#endif
#include <GL/gl.h>
#include <GL/glaux.h>
#endif

#include "SceneGraph.h"
#include "ImageTextureNode.h"
//JMC #include "FileGIF89a.h"
#include "FileJPEG.h"
#include "FileTarga.h"
#include "FilePNG.h"

#if 0 // JMC
static int GetFileType(char *filename) 
{
	FILE *fp = fopen(filename, "rb");
	if (!fp)	
		return FILETYPE_NONE;

	unsigned char signature[4];

	if (fread(signature, 4, 1, fp) != 1) {
		fclose(fp);
		return FILETYPE_NONE;
	}

	fclose(fp);

	int fileType = FILETYPE_NONE;

	//JMC	if (!strncmp("GIF", (char *)signature, 3))
	//JMC		fileType = FILETYPE_GIF;

	if (signature[0] == 0xff && signature[1] == 0xd8)
		fileType = FILETYPE_JPEG;

	if (!strncmp("PNG", (char *)(signature+1), 3))
		fileType = FILETYPE_PNG;

	return fileType;
}
#endif

#ifdef SUPPORT_OPENGL //JMC
static int GetTextureSize(int size) 
{
	int n = 1;
	while ((1 << n) <= size)
		n++;

	return (1 << (n-1));
}
#endif

////////////////////////////////////////////////
//	ImageTextureNode::ImageTextureNode
////////////////////////////////////////////////

ImageTextureNode::ImageTextureNode() 
{
	setHeaderFlag(false);
	setType(imageTextureNodeString);

	///////////////////////////
	// Exposed Field 
	///////////////////////////
 
	// url field
	MFString *url = new MFString();
	addExposedField(urlFieldString, url);

	///////////////////////////
	// Field 
	///////////////////////////

	// repeatS field
	SFBool *repeatS = new SFBool(true);
	addField(repeatSFieldString, repeatS);

	// repeatT field
	SFBool *repeatT = new SFBool(true);
	addField(repeatTFieldString, repeatT);

#ifdef SUPPORT_OPENGL

	///////////////////////////
	// Private Field 
	///////////////////////////

	// texture name field
	SFInt32 *texName = new SFInt32(0);
	texName->setName(textureNamePrivateFieldString);
	addPrivateField(texName);

	// texture name field
	SFBool *hasTransColor = new SFBool(false);
	hasTransColor->setName(hasTransparencyColorPrivateFieldString);
	addPrivateField(hasTransColor);

#endif

	mImageBuffer	= NULL;
	mFileImage		= NULL;
}

////////////////////////////////////////////////
//	ImageTextureNode::~ImageTextureNode
////////////////////////////////////////////////

ImageTextureNode::~ImageTextureNode() 
{
	if (mImageBuffer)
		delete []mImageBuffer;
	if (mFileImage)
		delete mFileImage;
}

////////////////////////////////////////////////
//	ImageTextureNode::createImage
////////////////////////////////////////////////

bool ImageTextureNode::createImage()
{
	if (mFileImage) {
		delete mFileImage;
		mFileImage = NULL;
		mWidth	= 0;
		mHeight	= 0;
	}

	if (getNUrls() <= 0)
		return false;

	//	SceneGraph *sg = getSceneGraph(); JMC

	char *filename = getUrl(0);
	if (filename == NULL)
		return false;

#ifdef SUPPORT_URL //JMC
	bool downloaded = false;
#endif
	
	FILE *fp = fopen(filename, "rt");
	if (fp == NULL){
#ifdef SUPPORT_URL
		if (sg->getUrlStream(filename)) {
			downloaded = true;
			char *filename = sg->getUrlOutputFilename();
		}
		else
			return false;
#else
		return false;
#endif
	}
	else
		fclose(fp);

	mFileImage = NULL;

#if 0 // JMC
	switch (GetFileType(filename)) {
	  //JMC	case FILETYPE_GIF:
	  //JMC		mFileImage = new FileGIF89a(filename);
	  //JMC		break;
#ifdef SUPPORT_JPEG
	case FILETYPE_JPEG:
		mFileImage = new FileJPEG(filename);
		break;
#endif
#ifdef SUPPORT_PNG
	case FILETYPE_PNG:
		mFileImage = new FilePNG(filename);
		break;
#endif
	}
#endif // JMC

#ifdef SUPPORT_URL
	if (downloaded)
		sg->deleteUrlOutputFilename();
#endif

	if (!mFileImage)
		return false;

#ifdef SUPPORT_OPENGL
	mWidth	= GetTextureSize(mFileImage->getWidth());
	mHeight	= GetTextureSize(mFileImage->getHeight());
#else
	mWidth	= mFileImage->getWidth();
	mHeight	= mFileImage->getHeight();
#endif

	if (mImageBuffer != NULL)
		delete []mImageBuffer;

	mImageBuffer = mFileImage->getRGBAImage(mWidth, mHeight);
	
	if (mImageBuffer == NULL) {
		mWidth	= 0;
		mHeight	= 0;
	}

#ifdef SUPPORT_OPENGL
	setHasTransparencyColor(mFileImage->hasTransparencyColor());
#endif

	return true;
}

////////////////////////////////////////////////
//	ImageTextureNode::createImage
////////////////////////////////////////////////

void ImageTextureNode::initialize() 
{
#ifdef SUPPORT_OPENGL
	if (createImage() == false)
		return;

	if (getWidth() == 0 || getHeight() == 0)
		return;

	unsigned int texName = getTextureName();
	if (0 < texName)
		glDeleteTextures(1, &texName);

	glGenTextures(1, &texName);
	if (0 < texName) {
		glBindTexture(GL_TEXTURE_2D, texName);
		glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
		glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
		glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
		glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
		glTexImage2D(GL_TEXTURE_2D, 0, 4, getWidth(), getHeight(), 0, GL_RGBA, GL_UNSIGNED_BYTE, getImage());
		setTextureName(texName);
	}
#endif
}

////////////////////////////////////////////////
//	ImageTextureNode::createImage
////////////////////////////////////////////////

void ImageTextureNode::uninitialize() 
{
}

