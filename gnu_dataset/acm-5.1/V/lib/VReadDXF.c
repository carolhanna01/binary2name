/*
 *  acm : an aerial combat simulator for X
 *  Copyright (C) 1991-1998  Riley Rainey
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software Foundaation,
 *  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/*
 *  DXF to V-library (obv) file converter
 */

#define DEBUG

#ifdef HAVE_STDLIB
#include <stdlib.h>
#endif
#include <stdio.h>
#include <stdarg.h>
#include <Vlib.h>
#include <string.h>
#include <math.h>

//#undef printf
//#undef fprintf


char *colors[] =
{
	"black",
	"red",
	"yellow",
	"green",
	"cyan",
	"blue",
	"magenta",
	"white"
};

#define Colors	8

extern VPolygon *ScalePolygon(VPolygon * in, VPoint * origin, VPoint * scale,
							  VPoint *, double);
extern VObject *VExtrudeObject(VObject * obj, VPoint * e);

#define POLY_MAX	(10 * 1024)
#define BOBJECT_MAX	128
#define POINT_MAX	32768

VObject *bobject[BOBJECT_MAX];
int btop = 0;

#ifdef DEBUG
#define PDEBUG(a)	printf a
#else
#define PDEBUG(a)
#endif

int lineno = 0;

#define COMPARE(a,b)	strcmp(a,b)

enum _token_id {
		DXF_NULL,
		DXF_EOF,
		DXF_X_COORD,
		DXF_Y_COORD,
		DXF_Z_COORD,
		DXF_X_SCALE,
		DXF_Y_SCALE,
		DXF_Z_SCALE,
		DXF_ROTATE,
		DXF_SECTION,
		DXF_ENDSEC,
		DXF_SEQEND,
		DXF_TITLE,
		DXF_POLYLINE,
		DXF_3DFACE,
		DXF_ENTITIES,
		DXF_VERTEX,
		DXF_BLOCKS,
		DXF_INSERT,
		DXF_X_EXTRUDE,
		DXF_Y_EXTRUDE,
		DXF_Z_EXTRUDE,
		DXF_M_COUNT,
		DXF_N_COUNT,
		DXF_THIRD_VERTEX,
		DXF_FOURTH_VERTEX,
		DXF_FLAGS,
		DXF_ITEM,
		DXF_TABLES,
		DXF_TABLE,
		DXF_ENDTAB,
		DXF_LAYER,
		DXF_STYLE,
		DXF_LTYPE,
		DXF_ATTDEF,
		DXF_ATTRIB,
		DXF_DICTIONARY,	/* found in OBJECTS section */
		DXF_MLINESTYLE,
		DXF_ACAD_GROUP,
		DXF_ACAD_MLINESTYLE,
		DXF_APPID,		/* found in TABLES and BLOCKS section */
		DXF_BLOCK,
		DXF_ENDBLK,
		DXF_VPORT,
		DXF_VIEW,
		DXF_COLOR_INDEX
};

typedef enum _token_id dxf_token_id;

typedef struct {
    dxf_token_id id;
    char *name;
} token_table;

token_table a[] =
{
    {DXF_SECTION,    "SECTION"},
    {DXF_ENDSEC,     "ENDSEC"},
    {DXF_POLYLINE,   "POLYLINE"},
    {DXF_3DFACE,     "3DFACE"},
    {DXF_VERTEX,     "VERTEX"},
    {DXF_SEQEND,     "SEQEND"},
    {DXF_EOF,        "EOF"},
    {DXF_BLOCK,      "BLOCK"},
    {DXF_ENDBLK,     "ENDBLK"},
    {DXF_INSERT,     "INSERT"},
    {DXF_TABLE,      "TABLE"},
    {DXF_ENDTAB,     "ENDTAB"},
    {DXF_LAYER,      "LAYER"},
    {DXF_STYLE,      "STYLE"},
    {DXF_LTYPE,      "LTYPE"},
    {DXF_VPORT,      "VPORT"},
    {DXF_DICTIONARY, "DICTIONARY"},
    {DXF_MLINESTYLE, "MLINESTYLE"},
    {DXF_APPID,      "APPID"},
    {DXF_ACAD_GROUP, "ACAD_GROUP"},
    {DXF_ACAD_MLINESTYLE, "ACAD_MLINESTYLE"},
    {DXF_ATTDEF,    "ATTDEF"},
    {DXF_ATTRIB,     "ATTRIB"},
    {DXF_EOF, NULL}
};

static int int_value;


static dxf_token_id
ReadToken(FILE *f, double *fp_value, char *cp_value, int *code, char *string)
{
    long i;
    int len;
    token_table *p;
    char buf1[512], buf2[512], *r1, *r2;
	
    r1 = fgets(buf1, sizeof(buf1), f);
    r2 = fgets(buf2, sizeof(buf2), f);
    lineno += 2;
	
	/*
	 *  Remove the trailing newline ...
	 */
	
    len = strlen(buf1);
    if (len > 0) {
		buf1[len - 1] = '\0';
    }
	
	/*
	 *  File in MSDOS format?
	 */
	
    if (len >= 2 && buf1[len - 2] == '\r') {
		buf1[len - 2] = '\0';
    }
	
	/*
	 *  Remove the trailing newline ...
	 */
	
    len = strlen(buf2);
    if (len > 0) {
		buf2[len - 1] = '\0';
    }
	
	/*
	 *  File in MSDOS format?
	 */
	
    if (len >= 2 && buf2[len - 2] == '\r') {
		buf2[len - 2] = '\0';
    }
	
    strcpy(cp_value, buf2);
    strcpy(string, buf2);
	
    if (r1 != (char *) NULL && r2 != (char *) NULL) {
		i = strtol(buf1, (char **) NULL, 0);
		*code = i;
		if (i == 0) {
			for (p = a; p->name != (char *) NULL; ++p) {
				if (COMPARE(p->name, buf2) == 0) {
					/*	    PDEBUG(("token: %s  --> ", p->name)); */
					return p->id;
				}
			}
			printf ("Warning: unrecognized directive, \"%s\"\n",
				buf2);
		}
		else if (i == 2) {
			if (COMPARE("ENTITIES", buf2) == 0) {
				return DXF_ENTITIES;
			}
			else if (COMPARE("BLOCKS", buf2) == 0) {
				return DXF_BLOCKS;
			}
			else if (COMPARE("TABLES", buf2) == 0) {
				return DXF_TABLES;
			}
			else {
				return DXF_TITLE;
			}
		}
		else if (i >= 10 && i <= 19) {
			*fp_value = strtod(buf2, (char **) NULL);
			return DXF_X_COORD;
		}
		else if (i >= 20 && i <= 29) {
			*fp_value = strtod(buf2, (char **) NULL);
			return DXF_Y_COORD;
		}
		else if (i >= 30 && i <= 39) {
			*fp_value = strtod(buf2, (char **) NULL);
			return DXF_Z_COORD;
		}
		else if (i == 41) {
			*fp_value = strtod(buf2, (char **) NULL);
			return DXF_X_SCALE;
		}
		else if (i == 42) {
			*fp_value = strtod(buf2, (char **) NULL);
			return DXF_Y_SCALE;
		}
		else if (i == 43) {
			*fp_value = strtod(buf2, (char **) NULL);
			return DXF_Z_SCALE;
		}
		else if (i == 50) {
			*fp_value = strtod(buf2, (char **) NULL);
			return DXF_ROTATE;
		}
		else if (i == 62) {
			int_value = strtol(buf2, (char **) NULL, 0);
			return DXF_COLOR_INDEX;
		}
		else if (i == 70) {
			int_value = strtol(buf2, (char **) NULL, 0);
			return DXF_FLAGS;
		}
		else if (i == 71) {
			int_value = strtol(buf2, (char **) NULL, 0);
			return DXF_M_COUNT;
		}
		else if (i == 72) {
			int_value = strtol(buf2, (char **) NULL, 0);
			return DXF_N_COUNT;
		}
		else if (i == 73) {
			int_value = strtol(buf2, (char **) NULL, 0);
			return DXF_THIRD_VERTEX;
		}
		else if (i == 74) {
			int_value = strtol(buf2, (char **) NULL, 0);
			return DXF_FOURTH_VERTEX;
		}
		else if (i == 210) {
			*fp_value = strtod(buf2, (char **) NULL);
			return DXF_X_EXTRUDE;
		}
		else if (i == 220) {
			*fp_value = strtod(buf2, (char **) NULL);
			return DXF_Y_EXTRUDE;
		}
		else if (i == 230) {
			*fp_value = strtod(buf2, (char **) NULL);
			return DXF_Z_EXTRUDE;
		}
		else {
			return DXF_ITEM;
		}
    }
    return DXF_EOF;
}

static int lookahead_valid = 0;
static double fp_la;
static dxf_token_id token_id_la;
static char cp_la[256], string_la[256];
static int code_la;

static dxf_token_id
NextToken(FILE * f, double *fp_value, char *cp_value, int * code, char *string)
{
    if (lookahead_valid) {
		lookahead_valid = 0;
		*fp_value = fp_la;
		strcpy(cp_value, cp_la);
		strcpy(string, string_la);
		*code = code_la;
		return token_id_la;
	}
    else {
		return ReadToken(f, fp_value, cp_value, code, string);
    }
}

static void
PushToken(dxf_token_id id, double fp_value, char *cp_value, int code, char *string)
{
    lookahead_valid = 1;
    fp_la = fp_value;
    token_id_la = id;
    strcpy(cp_la, cp_value);
    strcpy(string_la, string);
    code_la = code;
}

static void
InsertBlock(char *name, VPoint * o, VPoint * scale, VPoint * extrude, double r,
			VPolygon **poly, int *ptop)
{
    VObject **p;
    int i, j, extrusion = 0;
	
    PDEBUG(("looking for \"%s\"; offset %g, %g, %g; scale %g, %g, %g; extrude %g, %g, %g; rotate %g\n", name,
		o->x, o->y, o->z, scale->x, scale->y, scale->z,
		extrude->x, extrude->y, extrude->z, r));
	
    for (p = bobject, i = 0; i < btop; ++i, ++p) {
		if (strcmp((*p)->name, name) == 0) {
			
		/*
		 *  If this is an extrusion, create a temporary object representing the
		 *  given extrusion direction.
		 */
			
#ifdef notdef
			if (extrude->x != 0.0 || extrude->y != 0.0 || extrude->z != 0.0) {
				PDEBUG(("extrusion (%f, %f, %f)\n", extrude->x,
					extrude->y, extrude->z));
				extrusion = 1;
				ex_tmp = VExtrudeObject(*p, extrude);
				p = &ex_tmp;
			}
#endif
			
			/*
			 *  Add points to the block ...
			 */
			
			PDEBUG(("adding %d polygons to %d existing\n",
				(*p)->numPolys, (*ptop)));
			for (j = 0; j < (*p)->numPolys; ++j) {
				poly[(*ptop)++] = ScalePolygon((*p)->polygon[j], o, scale, extrude, r);
			}
			
			/*
			 *  Delete the temporarily created extrusion object
			 */
			
			if (extrusion) {
				for (j = 0; j < (*p)->numPolys; ++j) {
					VDestroyPolygon((*p)->polygon[j]);
					free((char *) (*p));
				}
			}
			
			return;
		}
    }
}

#define STATE_NORMAL		0
#define STATE_ENTITIES		1
#define STATE_POLYLINE		2
#define STATE_3DFACE		3
#define STATE_VERTEX		4
#define STATE_BLOCK		5
#define STATE_INSERT		6
#define STATE_TABLES		7
#define STATE_GENERAL_ENTITY	8

VObject  *
VReadDXFObject(FILE *f)
{
	return VReadDepthCueuedObject(f, 0);
}

VObject  *
VReadDepthCueuedDXFObject(FILE *f, int flag)
{
	
    double value, rotate = 0;
    dxf_token_id id, saved_id = DXF_NULL;
    int i, j, m0 = 0, n0 = 0, vertices_listed_by_index = 0;
    VPoint temp[POINT_MAX], temp1[4], *p, scale, pt, bpt, extrude;
    VPolygon template;
    VObject *object;
    int top = 0;
    int state = STATE_NORMAL, submode = 0;
    char cp[256], title[256], insert_title[256], *stop_block = "<none>";
    int order = 0;
    int indices[4], num_indices = 0;
    VMatrix m;
    int code, polymode;
    char string[256];

	VColor *blackColor = VAllocColor("black");

	VPolygon *poly[POLY_MAX];
	int ptop = 0;
	
	/*
	 *  Start with the first of our selected colors.  If ordering was reqested,
	 *  make the same color the "backface color".
	 */
	
	memset(&template, 0, sizeof(VPolygon));

    template.color = blackColor;
    template.backColor = NULL;
    if (order) {
        template.backColor = template.color;
    }
	
	/*
	 *  I'm not sure about backface clipping, yet.
	 */
    template.flags = 0;
	/*  template.flags = PolyClipBackface; */
    template.assignedDepth = -1;
	
    p = &temp[top];
	
    while (1) {
		id = NextToken(f, &value, cp, &code, string);
		switch (state) {
		case STATE_NORMAL:
			///    PDEBUG(("NORMAL: %s\n", string));
			switch (id) {
			case DXF_ENTITIES:
				submode = 0;
				state = STATE_ENTITIES;
				break;
			case DXF_BLOCKS:
				submode = 1;
				state = STATE_ENTITIES;
				break;
			case DXF_TABLES:
				state = STATE_TABLES;
				break;
			case DXF_EOF:
				
			/*
			 *  Build the object structure
		     */
				
				object = (VObject *) Vmalloc(sizeof(VObject));
				memset (object, 0, sizeof(VObject));
				object->name = strdup("name");
				object->numPolys = ptop;
				object->polygon = (VPolygon **) Vmalloc(ptop * sizeof(VPolygon *));
				memcpy(object->polygon, poly, ptop * sizeof(VPolygon *));
				object->order = (unsigned short *) NULL;
				VComputeObjectExtent(object);

				if (VObjectNeedsOrdering(object)) {
					VComputePolygonOrdering(object);
				}

				/*
				 *  Change to V library axes (Z-axis = down);
				 */
				
				VIdentMatrix(&m);
#ifdef notdef
				m.m[2][1] = 1.0;
				m.m[2][2] = 0.0;
				m.m[1][1] = 0.0;
				m.m[1][2] = -1.0;
#endif
				m.m[2][2] = -1.0;
				m.m[1][1] = -1.0;
				
				for (i = 0; i < object->numPolys; ++i) {
					for (j = 0; j < object->polygon[i]->numVtces; ++j) {
						VTransform_(&object->polygon[i]->vertex[j], &m, &pt);
						object->polygon[i]->vertex[j] = pt;
					}
				}
				
				return object;
				
			default:
				break;
			}
			break;
			
			/*
			 *  Skip table definitions (for now) ...
			 */
			
			case STATE_TABLES:
				switch (id) {
				case DXF_ENDSEC:
					state = STATE_NORMAL;
					break;
				default:
					break;
				}
				break;
				
				case STATE_ENTITIES:
					if (code == 0) {
						// close-out last entity
						saved_id = DXF_NULL;
#ifdef notdef
						printf ("Entity closed 0, \"%s\"\n", string);
#endif
					}
					// PDEBUG(("ENTITIES: %s\n", string));
					switch (id) {
					case DXF_ATTDEF:
					case DXF_ATTRIB:
						saved_id = id;
						state = STATE_GENERAL_ENTITY;
						break;
					case DXF_POLYLINE:
						state = STATE_POLYLINE;
						p = &temp[0];
						top = 0;
						m0 = n0 = 0;
						vertices_listed_by_index = num_indices = 0;
						polymode = 0;
						break;
					case DXF_3DFACE:
						state = STATE_3DFACE;
						p = &temp[0];
						top = 0;
						break;
					case DXF_BLOCK:
						state = STATE_BLOCK;
						strcpy (title, "*none*");
						bpt.x = bpt.y = bpt.z = 0.0;
						break;
					case DXF_ENDBLK:
						state = STATE_BLOCK;
						PushToken(id, value, cp, code, string);
						break;
					case DXF_INSERT:
						pt.x = pt.y = pt.z = 0.0;
						scale.x = 1.0;
						scale.y = 1.0;
						scale.z = 1.0;
						rotate = 0.0;
						extrude = pt;
						extrude.z = 1.0;
						state = STATE_INSERT;
						strcpy (insert_title, "*not-specified*");
						break;
					case DXF_ENDSEC:
						state = STATE_NORMAL;
						break;
					default:
						if (code != 0) {
							printf ("hmm, %d \"%s\"\n", code, string);
						}
						else {
							saved_id = id;
							state = STATE_GENERAL_ENTITY;
						}
						break;
					case DXF_EOF:
						break;
					}
					break;
					
					case STATE_POLYLINE:
						//PDEBUG(("POLYLINE: %s\n", string));
						switch (id) {
						case DXF_COLOR_INDEX:
							if (int_value < 8) {
								template.color = VAllocColor (colors[i]);
								if (order) {
									template.backColor = template.color;
								}
							}
							else {
								printf ("color index %d\n", int_value);
							}
							break;

						case DXF_SEQEND:
							
						/*
						 * Mesh?
						 */
							
							if (vertices_listed_by_index == 0) {
								if (m0 != 0 || n0 != 0) {
									for (i = 1; i < m0; ++i) {
										for (j = 1; j < n0; ++j) {
											temp1[0] = temp[(i - 1) * n0 + j - 1];
											temp1[1] = temp[(i) * n0 + j - 1];
											temp1[2] = temp[(i) * n0 + j];
											poly[ptop++] = VCreatePolygonFromTemplate(3, temp1,
												&template);
											
											temp1[0] = temp[(i) * n0 + j];
											temp1[1] = temp[(i - 1) * n0 + j];
											temp1[2] = temp[(i - 1) * n0 + j - 1];
											poly[ptop++] = VCreatePolygonFromTemplate(3, temp1,
												&template);
										}
									}
									PDEBUG(("mesh %d x %d\n", m0, n0));
								}
								else {
									poly[ptop++] = VCreatePolygonFromTemplate(top, temp,
										&template);
								}
							}
							PDEBUG(("added POLYLINE polygon number %d\n", ptop - 1));
#ifdef notdef
							VPrintPolygon(stdout, poly[ptop - 1]);
#endif
							p = &temp[0];
							top = 0;
							state = STATE_ENTITIES;
							break;
						case DXF_FLAGS:
							if ((int_value & 2) || (int_value & 4)) {
								printf ("Warning: POLYLINE splines or curves not supported by Vlib (line %d).\n", lineno);
							}
							if (int_value & 64) {
								polymode = 2;
							}
							else if (int_value & 16) {
								polymode = 1;
							}
							break;
						case DXF_VERTEX:
							state = STATE_VERTEX;
							break;
						case DXF_M_COUNT:
							m0 = int_value;
							break;
						case DXF_N_COUNT:
							n0 = int_value;
							break;
						default:
							break;
						}
						break;
						
						case STATE_3DFACE:
							// PDEBUG(("3DFACE: %s\n", string));
							switch (id) {
							case DXF_COLOR_INDEX:
								if (int_value < 8) {
									template.color = VAllocColor (colors[int_value]);
									if (order) {
										template.backColor = template.color;
									}
								}
								else {
									printf ("color index %d\n", int_value);
								}
								break;

							case DXF_ENDBLK:
								state = STATE_BLOCK;
								PushToken(id, value, cp, code, string);
								/* create polygon */
								poly[ptop++] = VCreatePolygonFromTemplate(top, temp,
									&template);
								p = &temp[0];
								top = 0;
								break;
								
							case DXF_X_COORD:
								p->x = value;
								if ((id = NextToken(f, &p->y, cp, &code, string)) != DXF_Y_COORD) {
									printf("syntax error4 %d\n", id);
								}
								
								if ((id = NextToken(f, &p->z, cp, &code, string)) != DXF_Z_COORD) {
									printf("syntax error5 %d\n", id);
								}
								
								++p;
								++top;
								if (top == POINT_MAX) {
									printf ("Point overflow, increase POINT_MAX.\n");
								}
								break;
								
							case DXF_3DFACE:
							case DXF_POLYLINE:
							case DXF_ENDSEC:
							default:
								if (code == 0) {
								/*
								*  Turbocad bug
									*/
									if (temp[top-1].x == temp[top-2].x &&
										temp[top-1].y == temp[top-2].y &&
										temp[top-1].z == temp[top-2].z) {
										-- top;
									}
									/* create polygon */
									poly[ptop++] = VCreatePolygonFromTemplate(top, temp,
										&template);
#ifdef notdef
									printf("polygon %d\n", ptop - 1);
									VPrintPolygon(stdout, poly[ptop - 1]);
#endif
									p = &temp[0];
									top = 0;
									PushToken(id, value, cp, code, string);
									state = STATE_ENTITIES;
								}
								
								break;
							}
							break;
							
							/*
							*  Get X,Y,Z components following a VERTEX directive
							*/
							
							case STATE_VERTEX:
								// PDEBUG(("VERTEX: %d\n", id));
								switch (id) {
								case DXF_COLOR_INDEX:
								if (int_value < 8) {
									template.color = VAllocColor (colors[i]);
									if (order) {
										template.backColor = template.color;
									}
								}
								else {
									printf ("color index %d\n", int_value);
								}
								break;

								case DXF_X_COORD:
									p->x = value;
									break;
								case DXF_Y_COORD:
									p->y = value;
									break;
								case DXF_Z_COORD:
									p->z = value;
									break;
									
									/*
									*  M_COUNT and N_COUNT really are the first and second vertex indices in
									*  a VERTEX.
									*/
								case DXF_M_COUNT:
									vertices_listed_by_index = 1;
									if (int_value < 0) {
										int_value = - int_value;
									}
									indices[0] = int_value - 1;
									num_indices = 1;
									break;
								case DXF_N_COUNT:
									vertices_listed_by_index = 1;
									if (int_value < 0) {
										int_value = - int_value;
									}
									indices[1] = int_value - 1;
									num_indices = 2;
									break;
								case DXF_THIRD_VERTEX:
									vertices_listed_by_index = 1;
									if (int_value < 0) {
										int_value = - int_value;
									}
									indices[2] = int_value - 1;
									num_indices = 3;
									break;
								case DXF_FOURTH_VERTEX:
									vertices_listed_by_index = 1;
									if (int_value < 0) {
										int_value = - int_value;
									}
									indices[3] = int_value - 1;
									num_indices = 4;
									break;
								case DXF_SEQEND:
								case DXF_VERTEX:
									if (vertices_listed_by_index) {
										for (i=0; i<num_indices; ++i) {
											if (indices[i] >= top) {
												printf ("internal error polygon vertex out of range: %d (max %d) -- %d\n",
													indices[i], top, i);
											}
											temp1[i] = temp[indices[i]];
										}
										poly[ptop++] = VCreatePolygonFromTemplate(num_indices, temp1,
											&template);
									}
									else {
										++p;
										++top;
										if (top == POINT_MAX) {
											printf ("Point overflow, increase POINT_MAX.\n");
										}
									}
									PushToken(id, value, cp, code, string);
									state = STATE_POLYLINE;
									break;
								default:
									break;
								}
								
								break;
								
								case STATE_INSERT:
									// PDEBUG(("INSERT: %s\n", string));
									switch (id) {
										
										break;
									case DXF_ENDBLK:
										state = STATE_BLOCK;
										PushToken(id, value, cp, code, string);
										
										InsertBlock(insert_title, &pt, &scale,
											&extrude, rotate, poly, &ptop);
										
										break;
									case DXF_TITLE:
										strcpy(insert_title, cp);
										break;
									case DXF_X_COORD:
										pt.x = value;
										break;
									case DXF_Y_COORD:
										pt.y = value;
										break;
									case DXF_Z_COORD:
										pt.z = value;
										break;
									case DXF_X_SCALE:
										scale.x = value;
										break;
									case DXF_Y_SCALE:
										scale.y = value;
										break;
									case DXF_Z_SCALE:
										scale.z = value;
										break;
									case DXF_ROTATE:
										rotate = value;
										break;
									case DXF_X_EXTRUDE:
										extrude.x = value;
										break;
									case DXF_Y_EXTRUDE:
										extrude.y = value;
										break;
									case DXF_Z_EXTRUDE:
										extrude.z = value;
										break;
									case DXF_INSERT:
									case DXF_POLYLINE:
									case DXF_3DFACE:
									case DXF_ENDSEC:
									default:
										if (code == 0) {
											state = STATE_ENTITIES;
											PushToken(id, value, cp, code, string);
											
											InsertBlock(insert_title, &pt, &scale,
												&extrude, rotate, poly, &ptop);
										}
										break;
									}
									break;
									
									case STATE_GENERAL_ENTITY:
										if (code == 0) {
											state = STATE_ENTITIES;
											PushToken(id, value, cp, code, string);
										}
										break;
										
									case STATE_BLOCK:
										PDEBUG(("BLOCK: %s\n", string));
										switch (id) {
										case DXF_ENDBLK:
										/*
										 *  Build the object structure and place it on the block list
									     */
											object = (VObject *) Vmalloc(sizeof(VObject));
											object->name = malloc(strlen(title) + 1);
											strcpy(object->name, title);
											PDEBUG(("added block \"%s\" %d polygons (line %d)\n", title, ptop, lineno));
											object->numPolys = ptop;
											object->polygon = (VPolygon **)
												Vmalloc(ptop * sizeof(VPolygon *));
											memcpy((char *) object->polygon,
												(char *) poly, ptop * sizeof(VPolygon *));
											ptop = 0;
											object->order = (unsigned short *) NULL;
											PDEBUG(("Block offset %f, %f, %f\n",
												bpt.x, bpt.y, bpt.z));
											
											for (i = 0; i < object->numPolys; ++i) {
												for (j = 0; j < object->polygon[i]->numVtces; ++j) {
													object->polygon[i]->vertex[j].x += bpt.x;
													object->polygon[i]->vertex[j].y += bpt.y;
													object->polygon[i]->vertex[j].z += bpt.z;
												}
											}
											
											bobject[btop++] = object;
											
											state = STATE_ENTITIES;
											if (strcmp(stop_block, object->name) == 0) {
												VIdentMatrix(&m);
#ifdef notdef
												m.m[2][1] = 1.0;
												m.m[2][2] = 0.0;
												m.m[1][1] = 0.0;
												m.m[1][2] = -1.0;
#endif
												m.m[2][2] = -1.0;
												m.m[1][1] = -1.0;
												
												for (i = 0; i < object->numPolys; ++i) {
													for (j = 0; j < object->polygon[i]->numVtces; ++j) {
														VTransform(&object->polygon[i]->vertex[j], &m, &pt);
														object->polygon[i]->vertex[j] = pt;
													}
												}
												
												return object;
											}
											
											break;
										case DXF_TITLE:
											strcpy(title, cp);
											break;
										case DXF_X_COORD:
											bpt.x = value;
											break;
										case DXF_Y_COORD:
											bpt.y = value;
											break;
										case DXF_Z_COORD:
											bpt.z = value;
											break;
										case DXF_POLYLINE:
										case DXF_3DFACE:
										case DXF_INSERT:
										default:
											if (code == 0) {
												state = STATE_ENTITIES;
												PushToken(id, value, cp, code, string);
											}
											break;
										}
	}
    }
	return 0;
}

static void
ArbitraryAxis (VPoint *normal, VMatrix *out)
{
	double minval = 1.0 / 64.0, d;
	VPoint Ax, Ay, Az;

	d = sqrt (normal->x * normal->x + normal->y * normal->y + normal->z * normal->z);
	Az.x = normal->x / d;
	Az.y = normal->y / d;
	Az.z = normal->z / d;

	if (fabs (Az.x) < minval && fabs (Az.y) < minval) {
		VCrossProd(&_VUnitVectorJ, &Az, &Ax);
	}
	else {
		VCrossProd(&_VUnitVectorK, &Az, &Ax);
	}

	VCrossProd (&Az, &Ax, &Ay);

	out->m[0][0] = Ax.x;
	out->m[0][1] = Ax.y;
	out->m[0][2] = Ax.z;

	out->m[1][0] = Ay.x;
	out->m[1][1] = Ay.y;
	out->m[1][2] = Ay.z;

	out->m[2][0] = Az.x;
	out->m[2][1] = Az.y;
	out->m[2][2] = Az.z;

	out->m[3][0] = out->m[3][1] = out->m[3][2] = 0.0;
	out->m[0][3] = out->m[1][3] = out->m[2][3] = 0.0;
	out->m[3][3] = 1.0;
}
