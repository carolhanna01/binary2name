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
*	File:	MField.cpp
*
******************************************************************/

#include "MField.h"

////////////////////////////////////////////////
//	MField::setValue
////////////////////////////////////////////////

void MField::setValue(char *buffer)
{
	char value[128];
	char *bp = buffer;
	int nSize = getSize();
	for (int n=0; n<nSize; n++) {
		int l=0;
		while (bp[l] != ',' && bp[l] != '\0')
			l++;
		if (bp[l] == '\0')
			return;
		strncpy(value, bp, l); 
		Field *field = getObject(n);
		field->setValue(value);
		bp += l;
	}
}

////////////////////////////////////////////////
//	MField::getValue
////////////////////////////////////////////////

char *MField::getValue(char *buffer, int bufferLen)
{
	buffer[0] = '\0';
	
	int		nString = 0;
	int		nSize = getSize();
	char	value[128];

	for (int n=0; n<nSize; n++) {
		Field *field = getObject(n);
		field->getValue(value);
		int l = strlen(value);
		if ((nString + l + 2) < bufferLen) {
			if (0 < nString)
				strcat(buffer, ", ");
			strcat(buffer, value);
			if (0 < nString)
				nString += (l + 2);
			else
				nString += l;
		}
		else
			break;
	}

	return buffer;
}
