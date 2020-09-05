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
*	File:	FontStyleNode.cpp
*
******************************************************************/

#include <string.h>
#include "FontStyleNode.h"

////////////////////////////////////////////////
//	Text::getFamilyNumber
////////////////////////////////////////////////

int FontStyleNode::getFamilyNumber()
{
	char *family = getFamily();

	if (family == NULL)
		return FONTSTYLE_FAMILY_SERIF;

	if (strcmp(family, "SERIF") == 0)
		return FONTSTYLE_FAMILY_SERIF;

	if (strcmp(family, "SANS") == 0)
		return FONTSTYLE_FAMILY_SANS;

	if (strcmp(family, "TYPEWRITER") == 0)
		return FONTSTYLE_FAMILY_TYPEWRITER;

	return FONTSTYLE_FAMILY_SERIF;
}

////////////////////////////////////////////////
//	Text::getStyleNumber
////////////////////////////////////////////////

int FontStyleNode::getStyleNumber()
{
	char *style = getStyle();

	if (style == NULL)
		return FONTSTYLE_STYLE_PLAIN;

	if (strcmp(style, "PLAIN") == 0)
		return FONTSTYLE_STYLE_PLAIN;

	if (strcmp(style, "BOLD") == 0)
		return FONTSTYLE_STYLE_BOLD;

	if (strcmp(style, "ITALIC") == 0)
		return FONTSTYLE_STYLE_ITALIC;

	if (strcmp(style, "BOLD ITALIC") == 0)
		return FONTSTYLE_STYLE_BOLDITALIC;

	return FONTSTYLE_STYLE_PLAIN;
}

