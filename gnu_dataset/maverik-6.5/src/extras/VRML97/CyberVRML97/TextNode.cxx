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
*	File:	TextNode.cpp
*
******************************************************************/

#include <assert.h>
#include <string.h>
#include "TextNode.h"

#ifdef SUPPORT_OPENGL
#ifdef WIN32
#include <windows.h>
#endif
#include <GL/gl.h>
#include <GL/glaux.h>
#endif

#ifdef SUPPORT_OPENGL
#ifdef WIN32
CLinkedList<OGLFontOutline> *TextNode::mOGLFontOutlines = new CLinkedList<OGLFontOutline>();
#endif
#endif

////////////////////////////////////////////////
//	TextNode::recomputeBoundingBox
////////////////////////////////////////////////

void TextNode::recomputeBoundingBox() 
{
	int nStrings = getNStrings();
	char *string = NULL;
	if (0 < nStrings) {
		string = getString(0);
		if (string != NULL) {
			if (strlen(string) <= 0)
				string = NULL;
		}
	}
	
	if (string != NULL) {
		float width = (float)strlen(string);
		setBoundingBoxCenter(-width/4.0f/1.0f, 0.5f, 0.0f);
		setBoundingBoxSize(width/4.0f, 0.5f, 0.5f);
	}
	else {
		setBoundingBoxCenter(0.0f, 0.0f, 0.0f);
		setBoundingBoxSize(-1.0f, -1.0f, -1.0f);
	}
}

////////////////////////////////////////////////
//	TextNode::createUseFontOutline
////////////////////////////////////////////////

#if defined(SUPPORT_OPENGL) && defined(WIN32)

unsigned int TextNode::createUseFontOutline(int family, int style)
{
	char *fontName = NULL;
	switch (family) {
	case FONTSTYLE_FAMILY_SERIF:
		fontName ="Times New Roman";
		break;
	case FONTSTYLE_FAMILY_SANS:
		fontName ="Helvetica";
		break;
	case FONTSTYLE_FAMILY_TYPEWRITER:
		fontName ="Courier";
		break;
	}

	assert(fontName != NULL);

	LOGFONT lf;

	lf.lfHeight = -MulDiv(12, 96, 72);
	lf.lfWidth = 0;
	lf.lfEscapement = 0;
	lf.lfOrientation = 0;
	lf.lfWeight = (style == FONTSTYLE_STYLE_BOLD || style == FONTSTYLE_STYLE_BOLDITALIC)? 700 : 400;
	lf.lfItalic = (style == FONTSTYLE_STYLE_ITALIC || style == FONTSTYLE_STYLE_BOLDITALIC) ? TRUE : FALSE;
	lf.lfUnderline = FALSE;
	lf.lfStrikeOut = FALSE;
	lf.lfCharSet = ANSI_CHARSET;
	lf.lfOutPrecision = OUT_DEFAULT_PRECIS;
	lf.lfClipPrecision = CLIP_DEFAULT_PRECIS;
	lf.lfQuality = DEFAULT_QUALITY;
	lf.lfPitchAndFamily = FF_DONTCARE|DEFAULT_PITCH;
	strcpy(lf.lfFaceName, fontName);

	HFONT font = CreateFontIndirect(&lf);
	HDC hdc = wglGetCurrentDC();

	HFONT oldFont = SelectObject(hdc, font);		

	unsigned int id = glGenLists(256);
	GLYPHMETRICSFLOAT gmf[256];
	wglUseFontOutlines(hdc, 0, 255, id, 1.0f, 0.1f, WGL_FONT_POLYGONS, gmf);

	SelectObject(hdc, oldFont);		

	return id;
}

#endif

////////////////////////////////////////////////
//	TextNode::draw
////////////////////////////////////////////////

#ifdef SUPPORT_OPENGL

void TextNode::draw()
{
	unsigned int nDisplayList = getDisplayList();
	if (nDisplayList == 0)
		return;

	int nStrings = getNStrings();
	char *string = NULL;
	if (0 < nStrings) {
		string = getString(0);
		if (string != NULL) {
			if (strlen(string) <= 0)
				string = NULL;
		}
	}

	if (string == NULL)
		return;

	glListBase(nDisplayList);
	glCallLists(strlen(string), GL_UNSIGNED_BYTE, (const GLvoid*)string);
}

#endif

////////////////////////////////////////////////
//	PointSet::recomputeDisplayList
////////////////////////////////////////////////

#ifdef SUPPORT_OPENGL

void TextNode::recomputeDisplayList() 
{
	int family	= getFontStyleFamilyNumber();
	int style	= getFontStyleStyleNumber();

	OGLFontOutline *fontOutline = getOGLFontOutline(family, style);
	
	unsigned int id = 0;

	if (fontOutline != NULL) {
		id = fontOutline->getListBaseID();
	}
	else {
		id = createUseFontOutline(family, style);
		addOGLFontOutline(family, style, id);
	}

	assert(id != 0);

	setDisplayList(id);
}	

#endif
