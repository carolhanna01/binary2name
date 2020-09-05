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
*	File:	PROTO.cpp
*
******************************************************************/

#include "PROTO.h"
#include "vrmlfields.h"

int GetFieldTypeFromString(char *typeString)
{
	SFBool	field;
	field.setType(typeString);
	return field.getType();
}	

PROTO::PROTO(char *name, char *string, char *fieldString)
{
	setName(name);
	setString(string);
	addDefaultFields(fieldString);
}

PROTO::~PROTO(void)
{
}

void PROTO::addFieldValues(
char		*fieldString, 
int			bDefaultField)
{
	char	string[256];
	char	fieldTypeName[32];
	char	fieldName[256];

	char *token = strtok(fieldString, FIELD_SEPARATORS  );
	while( token != NULL ) {

		if (bDefaultField) {
			sscanf(token, "%s", string);
			if (strcmp(string, "field") != 0 && strcmp(string, "exposedField") != 0)
				return;
			/* Get field type */
			token = strtok( NULL, FIELD_SEPARATORS  );
			sscanf(token, "%s", fieldTypeName);
			token = strtok( NULL, FIELD_SEPARATORS  );
		}

		/* Get field name */
		sscanf(token, "%s", fieldName);

		int fieldType;
		if (bDefaultField)
			fieldType = GetFieldTypeFromString(fieldTypeName);
		else
			fieldType = getFieldType(fieldName);

		Field *field = NULL;

		switch (fieldType) {
		case fieldTypeSFString:
			{
				field = new SFString();
				token = strtok( NULL, FIELD_SEPARATORS  );
				((SFString *)field)->setValue(token);
				break;
			}
		case fieldTypeSFFloat:
			{
				field = new SFFloat();
				token = strtok( NULL, FIELD_SEPARATORS  );
				float value = (float)atof(token);
				((SFFloat *)field)->setValue(value);
				break;
			}
		case fieldTypeSFInt32:
			{
				field = new SFInt32();
				token = strtok( NULL, FIELD_SEPARATORS  );
				int value = atoi(token);
				((SFInt32 *)field)->setValue(value);
				break;
			}
		case fieldTypeSFVec2f:
			{
				field = new SFVec2f();
				float	vec2f[2];
				for (int n=0; n<2; n++) {
					token = strtok( NULL, FIELD_SEPARATORS  );
					vec2f[n] = (float)atof(token);
				}
				((SFVec2f *)field)->setValue(vec2f);
				break;
			}
		case fieldTypeSFVec3f:
			{
				field = new SFVec3f();
				float	vec3f[3];
				for (int n=0; n<3; n++) {
					token = strtok( NULL, FIELD_SEPARATORS  );
					vec3f[n] = (float)atof(token);
				}
				((SFVec3f *)field)->setValue(vec3f);
				break;
			}
		case fieldTypeSFColor:
			{
				field = new SFColor();
				float color[3];
				for (int n=0; n<3; n++) {
					token = strtok( NULL, FIELD_SEPARATORS  );
					color[n] = (float)atof(token);
				}
				((SFColor *)field)->setValue(color);
				break;
			}
		case fieldTypeSFBool:
			{
				field = new SFBool();
				token = strtok( NULL, FIELD_SEPARATORS  );
				bool btrue = !strcmp(token, "TRUE") ? true : false; 
				((SFBool *)field)->setValue(btrue);
				break;
			}
		case fieldTypeSFRotation:
			{
				field = new SFRotation();
				float rot[4];
				for (int n=0; n<4; n++) {
					token = strtok( NULL, FIELD_SEPARATORS  );
					rot[n] = (float)atof(token);
				}
				((SFRotation *)field)->setValue(rot);
				break;
			}
		case fieldTypeSFTime:
			{
				field = new SFTime();
				token = strtok( NULL, FIELD_SEPARATORS  );
				double time = atof(token);
				((SFTime *)field)->setValue(time);
				break;
			}
		}

		assert(field);

		field->setName(fieldName);
		if (bDefaultField)
			addDefaultField(field);
		else
			addField(field);

		token = strtok( NULL, FIELD_SEPARATORS  );
	}

}

void PROTO::getString(char *returnBuffer)
{
	returnBuffer[0] = '\0';

	char *string = getString();
	if (!string || !strlen(string))
		return;

//	char *defaultString = strdup(string);
	char *defaultString = new char[strlen(string)+1];
	strcpy(defaultString, string);

	char *token = strtok(defaultString, FIELD_SEPARATORS);
	while( token != NULL ) {
		if (!strcmp(token, "IS")) {
			token = strtok( NULL, FIELD_SEPARATORS  );
			Field *field = getField(token);
			if (field) {
				char	value[128];
				field->getValue(value);
				sprintf(&returnBuffer[strlen(returnBuffer)], "%s ", value);
			}
		}
		else
			sprintf(&returnBuffer[strlen(returnBuffer)], "%s ", token);
		token = strtok( NULL, FIELD_SEPARATORS  );
	}

//	free(defaultString);
	delete[] defaultString;
}

Field *PROTO::getField(char *name)
{
	Field	*field;
	int		n;

	int nField = getNFields();
	for (n = 0; n<nField; n++) {
		field = getField(n);
		if (!strcmp(field->getName(), name))
			return field;
	}

	int nDefaultField = getNDefaultFields();
	for (n = 0; n<nDefaultField; n++) {
		field = getDefaultField(n);
		if (!strcmp(field->getName(), name))
			return field;
	}

	return NULL;
}

int PROTO::getFieldType(char *name)
{
	int nDefaultField = getNDefaultFields();
	for (int n = 0; n<nDefaultField; n++) {
		Field *field = getDefaultField(n);
		if (!strcmp(field->getName(), name))
			return field->getType();
	}
	return fieldTypeNone;
}
