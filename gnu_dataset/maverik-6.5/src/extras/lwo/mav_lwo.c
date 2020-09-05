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

/**********
 * 
 * Lightwave object parser
 *
 * TODO: doublesided surfaces, textures, smoothing thresholds
 *
 *****************************************************************************/

#include "maverik.h"
#include "mav_lwo.h"

#if defined(WIN32) || defined(__CYGWIN__)
#include <windows.h>
#endif
#include <GL/glu.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if !defined(WIN32) || defined(__CYGWIN__)
char *strdup (const char *s1);
#endif

typedef struct {
  unsigned int length;
  char type[5];
  FILE * fd;
} MAVLIB_LWOBIFFFile;

typedef struct {
  char type[5];
  unsigned int length;
} MAVLIB_LWOBChunk;

typedef struct {
  int red;
  int green;
  int blue;
} MAVLIB_LWOBColour;

typedef struct {
  MAV_vector v;
  MAV_vector normal;
} MAVLIB_LWOBVertex;

typedef struct {
  int numverts;
  int * vertices;
  int surface;
} MAVLIB_LWOBPolygon;

typedef struct MAVLIB_LWOBPOLYGONLIST {
  int numverts;
  int * vertices;
  int surface;
  MAV_vector normal;
  struct MAVLIB_LWOBPOLYGONLIST* next;
} MAVLIB_LWOBPolygonList;

typedef struct MAVLIB_LWOBSURFACENAME {
  char name[256];
  struct MAVLIB_LWOBSURFACENAME * next;
  MAVLIB_LWOBColour colour;
  int smoothing;
  int doublesided;
  int isDefault;
} MAVLIB_LWOBSurfaceName;;

typedef struct {
  MAV_surfaceParams *sp;
  int smoothing;
  int doublesided;
} MAVLIB_LWOBSurface;



void mavlib_lwobprinterror(char * msg)
{
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr,"\nParse error: %s\n", msg);
}

/* Read a MSFB first integer of arbitrary number of bytes */
int mavlib_LWOBReadI(unsigned int * result, MAVLIB_LWOBIFFFile * file, int bytes)
{
    unsigned int character;
    int i;
    *result=0;

    for(i=bytes-1; i >= 0; i--)
    {
	character=fgetc(file->fd);

	if (character!=EOF)
	    *result+=character<<(i*8);
	else
	{
	    mavlib_lwobprinterror("Unexpected EOF reading value");
	    return -1;
	}
    }    
    return bytes;
}

int mavlib_LWOBReadF(float *val, MAVLIB_LWOBIFFFile * file)
{
    return mavlib_LWOBReadI((unsigned int *)val,file,4);
}

int mavlib_LWOBReadI1(int * val, MAVLIB_LWOBIFFFile * file)
{
    return mavlib_LWOBReadI((unsigned int *)val, file,1);
} 

int mavlib_LWOBReadI2(int * val, MAVLIB_LWOBIFFFile * file)
{
    return mavlib_LWOBReadI((unsigned int *)val, file, 2);
}

int mavlib_LWOBReadI4(int * val, MAVLIB_LWOBIFFFile * file)
{
    return mavlib_LWOBReadI((unsigned int *)val, file,4);
}

int mavlib_LWOBReadU1(unsigned int * val, MAVLIB_LWOBIFFFile * file)
{
    return mavlib_LWOBReadI(val, file,1);
}

int mavlib_LWOBReadU2(unsigned int * val, MAVLIB_LWOBIFFFile * file)
{
    return mavlib_LWOBReadI(val, file,2);
}

int mavlib_LWOBReadU4(unsigned int * val, MAVLIB_LWOBIFFFile * file)
{
    return mavlib_LWOBReadI(val, file,4);
}

int mavlib_LWOBReadS(char * string, MAVLIB_LWOBIFFFile * file, unsigned int bufflen)
{
    unsigned int length=0;
    unsigned int character;
    string[bufflen-1]=0;

    do {
	character=fgetc(file->fd);
	
	if (character==EOF)
	{
	    mavlib_lwobprinterror("Unexpected EOF reading string");
	    return -1;   /* unterminated string - eof */
	}

	/* do not overrun buffer */
	if (length<bufflen-1)
	    string[length++]=(char) character;
	else
	    length++;
    } while (character!=0);
    
    if(length > bufflen)
	mavlib_lwobprinterror("Warning - Truncating string (buffer too short)");

    /* strings are always even length */
    if (length%2==1)
    {
	length++;
	fgetc(file->fd);
    }

    return length;
}

int mavlib_LWOBIFFReadID(char * dest, MAVLIB_LWOBIFFFile * file)
{
    unsigned int character;
    int i;

    dest[4]=0;
    
    for(i=0; i<4; i++)
    {
	character=fgetc(file->fd);
	if (character!=EOF)
	    dest[i]=(char) character;
	else
	{
	    mavlib_lwobprinterror("Unexpected EOF reading ID");
	    strcpy(dest,"EOF");
	    return -1;
	}
    }
    return 4;
}

int mavlib_LWOBIFFReadChunk(MAVLIB_LWOBChunk * chunk, MAVLIB_LWOBIFFFile * file)
{
    if (mavlib_LWOBIFFReadID(chunk->type, file)!=4)
    {
	mavlib_lwobprinterror("Chunk read failed due to ID read fail");
	return -1;
    }
    chunk->type[4]=0;

    if(mavlib_LWOBReadI4((int *) &chunk->length, file)!=4)
    {
	mavlib_lwobprinterror("Chunk read failed due to length read fail");
	return -1;
    }

    return 8;
}

/* Close an MAVLIB_LWOBIFFFile */
void mavlib_LWOBIFFClose(MAVLIB_LWOBIFFFile * file)
{
    if(file!=NULL)
    {
	fclose (file->fd);
	mav_free(file);
    }
    else
	mavlib_lwobprinterror("Trying to close a NULL MAVLIB_LWOBIFFFile");
}

/* Open a file, checking if it is an IFF */
MAVLIB_LWOBIFFFile * mavlib_LWOBIFFOpen(char * filename)
{
    MAVLIB_LWOBIFFFile * file;
    MAVLIB_LWOBChunk chunk;
    file = (MAVLIB_LWOBIFFFile *) mav_malloc(sizeof(MAVLIB_LWOBIFFFile));

    if(file==NULL) 
    {
	mavlib_lwobprinterror("Failed to open IFF due to failure to allocate memory for file");
	return NULL; /* mav_malloc failed */
    }
    file->fd=fopen(filename,"r");

    if(file->fd==NULL)
    {
	mavlib_lwobprinterror("Failed to open IFF due to failure to open disk file");
	mav_free(file);
	return NULL; /* error - file open error*/
    }

    if(mavlib_LWOBIFFReadChunk(&chunk, file)!=8)
    {
	mavlib_lwobprinterror("Failed to open IFF due to failure to read FORM chunk");
	mavlib_LWOBIFFClose(file);
	return NULL;
    }
    if(0!=strcmp(chunk.type,"FORM"))
    {
	mavlib_LWOBIFFClose(file);
	mavlib_lwobprinterror("Not an IFF file");
	return NULL; /* Not IFF */
    }

    if(mavlib_LWOBIFFReadID(file->type, file)!=4)
    {
	mavlib_lwobprinterror("Failed to open IFF due to failure to read FORM type");
	mavlib_LWOBIFFClose(file);
	return NULL;
    };

    file->length=chunk.length-4; /* subtract 4 because we have type ID */

    return file;
}

int mavlib_LWOBReadSubChunk(MAVLIB_LWOBChunk * chunk, MAVLIB_LWOBIFFFile * file)
{
    if(mavlib_LWOBIFFReadID(chunk->type, file)!=4)
    {
	mavlib_lwobprinterror("Subchunk read failed due to ID read fail");
	return -1;
    }
    chunk->type[4]=0;

    if(mavlib_LWOBReadI2((int *) &chunk->length,file)!=2)
    {
	mavlib_lwobprinterror("Subchunk read failed due to length read fail");
	return -1;
    }
    return 6;
}

int mavlib_LWOBReadSRFS(MAVLIB_LWOBSurfaceName ** surfaces, MAVLIB_LWOBIFFFile * file, int bytes)
{
    unsigned int bytes_read=0;
    int result;
    MAVLIB_LWOBSurfaceName * this_surface;
    MAVLIB_LWOBSurfaceName * last_surface=NULL;
    
    *surfaces=NULL;
    do 
    {
	this_surface=(MAVLIB_LWOBSurfaceName *) mav_malloc (sizeof (MAVLIB_LWOBSurfaceName));
	if(this_surface==NULL)
	{
	    mavlib_lwobprinterror("Failed to read surface description due to failure to allocate"
		       " memory");
	    return -1;
	}
	this_surface->next=NULL;
	this_surface->colour.red=0;
	this_surface->colour.green=0;
	this_surface->colour.blue=0;
	this_surface->isDefault=1;

	if(*surfaces==NULL) *surfaces=this_surface;
	
	result=mavlib_LWOBReadS(this_surface->name, file, 256);
	if(result==-1)
	{
	    mavlib_lwobprinterror("Failed to read surface description");
	    return -1;
	}
	bytes_read+=result;
	if(last_surface!=NULL)
	    last_surface->next=this_surface;

	last_surface=this_surface;
/*	printf("    %s\n",last_surface->name);*/
    } while (bytes_read < bytes);
    
    return bytes;
}

int mavlib_LWOBReadCOLR(MAVLIB_LWOBColour * colour, MAVLIB_LWOBIFFFile * file)
{
    if(mavlib_LWOBReadU1((unsigned int *) &colour->red,file)==-1)
    {
	mavlib_lwobprinterror("Failed to read colour red description");
	return -1;
    }

    if(mavlib_LWOBReadU1((unsigned int *) &colour->green, file)==-1)
    {
	mavlib_lwobprinterror("Failed to read colour green description");
	return -1;
    } 

    if(mavlib_LWOBReadU1((unsigned int *) &colour->blue, file)==-1)
    {
	mavlib_lwobprinterror("Failed to read colour blue description");
	return -1;
    }
    
    fgetc(file->fd);

    return 4;
}

int mavlib_LWOBReadSURF(MAVLIB_LWOBSurfaceName *surfaces, MAVLIB_LWOBIFFFile * file, int bytes)
{
    char surface[500];
    unsigned int bytes_read=0;
    MAVLIB_LWOBChunk subchunk;
    MAVLIB_LWOBColour col;
    MAVLIB_LWOBSurfaceName *s=surfaces;	
    int smoothing=0;
    int doublesided=0;
    bytes_read+=mavlib_LWOBReadS(surface, file, 500);
    if(bytes_read==-1)
    {
	mavlib_lwobprinterror("Failed to read SURF name");
	return -1;
    }
    
    do 
    {
	if(mavlib_LWOBReadSubChunk(&subchunk, file)!=6)
	{
	    mavlib_lwobprinterror("Failed to read SURF subchunk");
	    return -1;
	}
	bytes_read+=6;
	
/*	printf("\n    [%s %d bytes]\t",subchunk.type,subchunk.length);*/
	
	if(!strcmp(subchunk.type,"COLR"))
	{
/*	    printf("Base Colour\n");*/
	    if(mavlib_LWOBReadCOLR(&col,file)!=4)
	    {
		mavlib_lwobprinterror("Failed to read SURF COLR subchunk");
		return -1;
	    }
	    
	    bytes_read+=4;
	    
/*	    printf("      Red=%d, Green=%d, Blue=%d\n", 
	    col.red, col.green, col.blue);*/
	}
	else if(!strcmp(subchunk.type,"FLAG"))
	{
	    int flag;
	    if(mavlib_LWOBReadU2((unsigned int *) &flag,file)!=2)
	    {
		mavlib_lwobprinterror("Failed to read SURF FLAG subchunk");
		return -1;
	    }		
	 
	    if(flag & 4)
	    {
		smoothing=1;
	    }

	    if(flag & 256)
	    {
		doublesided=1;
	    }
	    bytes_read+=2;
	} else 	{
/*	    printf("... Skipping unsupported subchunk.\n");*/
	    if(fseek(file->fd, subchunk.length, SEEK_CUR)!=0)
	    {
		mavlib_lwobprinterror("Failed to skip unsupported SURF subchunk");
		return -1;
	    }

	    bytes_read+=subchunk.length;
	}
	
    } while (bytes_read < bytes);

    s=surfaces;
    while(s!=NULL && strcmp(s->name,surface))
    {
	s=s->next;
    }

    if(s==NULL)
    {
	mavlib_lwobprinterror("Found SURF chunk for nonexistant surface");
	return -1;
    }	
    s->smoothing=smoothing;
    s->doublesided=doublesided;
    s->colour=col;
    s->isDefault=0;
    return bytes;
}

int mavlib_LWOBReadPNTS(MAVLIB_LWOBVertex * points, MAVLIB_LWOBIFFFile * file, int chunklength)
{
    int i;
    float x, y, z;
    for (i=0; i <chunklength/12; i++)
    {

	if(mavlib_LWOBReadF(&x, file)!=4)
	{
	    mavlib_lwobprinterror("Failed to read PNTS x value");
	    return -1;
	}
	
	if(mavlib_LWOBReadF(&y, file)!=4)
	{
	    mavlib_lwobprinterror("Failed to read PNTS y value");
	    return -1;
	}
	if(mavlib_LWOBReadF(&z, file)!=4)
	{
	    mavlib_lwobprinterror("Failed to read PNTS z value");
	    return -1;
	}

	points[i].v=mav_vectorSet(x,y,z);
	points[i].normal=mav_vectorSet(0,0,0);
/*	printf("    (%f, %f, %f)\n",x,y,z);*/
    }
    return chunklength;
}

void mavlib_LWOBfreePOLSList(MAVLIB_LWOBPolygonList * plist)
{
    MAVLIB_LWOBPolygonList * next_polygon;
    MAVLIB_LWOBPolygonList * this_polygon=plist->next;
    while (this_polygon != NULL)
    {
	next_polygon=this_polygon->next;
	mav_free(this_polygon->vertices);
	mav_free(this_polygon);
	this_polygon=next_polygon;
    }
    mav_free(plist);
}

int mavlib_LWOBReadPOLS(MAVLIB_LWOBPolygonList ** plist, MAVLIB_LWOBIFFFile * file, int chunklength)
{
    unsigned int bytesread=0;
    int i;

    MAVLIB_LWOBPolygonList * last_polygon;
    MAVLIB_LWOBPolygonList * new_polygon;
    *plist=mav_malloc(sizeof (MAVLIB_LWOBPolygonList));
    if(*plist==NULL)
    {
	mavlib_lwobprinterror("Failed to read POLS due to failure to allocate memory for list element");
	return -1;
    }
    (*plist)->numverts=0;

    last_polygon=*plist;

    do {
	new_polygon=mav_malloc(sizeof (MAVLIB_LWOBPolygonList));
	if(new_polygon==NULL)
	{
	    mavlib_lwobprinterror("Failed to read POLS due to failure to allocate memory for list element");
	    mavlib_LWOBfreePOLSList(*plist);
	    return -1;
	}	
	
	new_polygon->next=NULL;

	if(mavlib_LWOBReadU2((unsigned int *) &new_polygon->numverts, file)!=2)
	{
	    mavlib_LWOBfreePOLSList(*plist);
	    mav_free(new_polygon);
	    mavlib_lwobprinterror("Failed to read POLS due to failure to number of vertices");
	    return -1;
	}
	
/*	printf("    %d vertices:", new_polygon->numverts);*/

	new_polygon->vertices=mav_calloc(sizeof(int), new_polygon->numverts);
	if(new_polygon->vertices==NULL)
	{
	    mavlib_LWOBfreePOLSList(*plist);
	    mav_free(new_polygon);
	    mavlib_lwobprinterror("Failed to read POLS due to failure to allocate memory for vertex array");
	    return -1;
	}

	for(i=0; i <new_polygon->numverts; i++)
	{ 
	    if(mavlib_LWOBReadU2((unsigned int *) &new_polygon->vertices[i],file)!=2)
	    {
		mavlib_LWOBfreePOLSList(*plist);
		mav_free(new_polygon);
		mav_free(new_polygon->vertices);
		mavlib_lwobprinterror("Failed to read POLS due to failure to read vertex");
		return -1;
	    }		
/*	    printf(" %d", new_polygon->vertices[i]);*/
	}

	if(mavlib_LWOBReadI2(&new_polygon->surface,file)!=2)
	{
	    mavlib_LWOBfreePOLSList(*plist);
	    mav_free(new_polygon);
	    mav_free(new_polygon->vertices);
	    mavlib_lwobprinterror("Failed to read POLS due to failure to read surface reference");
	    return -1;
	}							

/*	printf(". Surface %d.\n",new_polygon->surface);*/
	bytesread+= 4 + new_polygon->numverts*2;

	last_polygon->next=new_polygon;
	(*plist)->numverts++;	
	last_polygon=new_polygon;
    } while (bytesread<chunklength);
    
    return chunklength;
}

int mavlib_LWOBparse_file( MAVLIB_LWOBSurfaceName ** surfaces, MAVLIB_LWOBVertex ** points, 
		MAVLIB_LWOBPolygonList ** polygons, int *numpoints, char * filename)
{
    unsigned int position=0;
    MAVLIB_LWOBIFFFile * file;
    MAVLIB_LWOBChunk chunk;

    int pnts_read=0, pols_read=0, srfs_read=0, surf_read=0;
   
    file=mavlib_LWOBIFFOpen(filename);
    if(file==NULL)
    {
	mavlib_lwobprinterror("Failed to read LWOB due to failure to open file");
	return 1;	    
    }   

    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Reading lightwave object data from %s...",filename);
/*    printf("%s is %d bytes\n",file->type,file->length);*/

    while (position < file->length)
    {
	if(mavlib_LWOBIFFReadChunk(&chunk, file)!=8)
	{
	    mavlib_lwobprinterror("Failed to read LWOB due to failure to read chunk");
	    mavlib_LWOBIFFClose(file);
	    return 1;	    
	}

/*	printf("\n  [%s %d bytes]\t", chunk.type, chunk.length);*/

	if(!strcmp(chunk.type,"SRFS"))
	{
/*	    printf("Surface List\n");*/
	    if(mavlib_LWOBReadSRFS(surfaces,file, chunk.length)==-1)
	    {
		mavlib_lwobprinterror("Failed to read LWOB due to failure to read SRFS");
		mavlib_LWOBIFFClose(file);
		return 1;
	    }
	    srfs_read=1;
	}
	else if(!strcmp(chunk.type,"SURF"))
	{
	    if(!srfs_read)
	    {
		mavlib_lwobprinterror("Failed to read LWOB due to finding SURF chunk before SRFS");
		mavlib_LWOBIFFClose(file);
		return 1;
	    }	
/*	    printf("Surface Definition\n");*/
	    if(mavlib_LWOBReadSURF(*surfaces, file, chunk.length)==-1)
	    {
		mavlib_lwobprinterror("Failed to read LWOB due to failure to read SURF");
		mavlib_LWOBIFFClose(file);
		return 1;
	    }
	    surf_read=1;
	}
	else if(!strcmp(chunk.type, "PNTS"))
	{
/*	    printf("Points List (%d points)\n",chunk.length/12);*/
	    *points=(MAVLIB_LWOBVertex *) mav_malloc(chunk.length/12 * sizeof(MAVLIB_LWOBVertex));
	    if(*points==NULL)
	    {
		mavlib_lwobprinterror("Failed to read LWOB due to failure to allocate memory for PNTS");
		mavlib_LWOBIFFClose(file);
		return 1;
	    }
	    if(mavlib_LWOBReadPNTS(*points, file, chunk.length)!=chunk.length)
	    {
		mavlib_lwobprinterror("Failed to read LWOB due to failure to read PNTS");
		mav_free(*points);
		mavlib_LWOBIFFClose(file);
		return 1;
	    }		
	    pnts_read=1;
	    *numpoints=chunk.length/12;
	}
	else if(!strcmp(chunk.type, "POLS"))
	{
	    if(pnts_read!=1)
	    {
		mavlib_lwobprinterror("Failed to read LWOB due to finding POLS chunk before PNTS");
		mavlib_LWOBIFFClose(file);
		return 1;
	    }				
/*	    printf("Polygon List\n");*/
	    if(mavlib_LWOBReadPOLS(polygons, file, chunk.length)!=chunk.length)
	    {
		mavlib_lwobprinterror("Failed to read LWOB due to failure to read POLS chunk");
		mav_free(*points);
		mavlib_LWOBIFFClose(file);
		return 1;
	    }
	    pols_read=1;
	}
	else
	{
/*	    printf("... Skipping unsupported chunk.\n");*/
	    if(fseek(file->fd, chunk.length, SEEK_CUR)!=0)
	    {
		mavlib_lwobprinterror("Failed to read LWOB due to failure to skip unsupported chunk");
		if(pols_read)
		    mavlib_LWOBfreePOLSList(*polygons);
		if(pnts_read)
		    mav_free(*points);
		mavlib_LWOBIFFClose(file);
		return 1;
	    };	
	}
	position+=chunk.length+8; /* 8 bytes ID and size */
    }

    if(!surf_read) {
	char buf[500];
	MAVLIB_LWOBSurfaceName * s= *surfaces;
	mavlib_lwobprinterror("WARNING - The file did not contain a SURF chunk");
	while(s!=NULL) {
	    sprintf(buf,"WARNING - Surface \"%s\" has now been assigned default surface parameters",s->name);
	    mavlib_lwobprinterror(buf);
	    s=s->next;
	}
    }

    if(!(pols_read && pnts_read && srfs_read))
    {
	mavlib_lwobprinterror("Failed to read LWOB due to missing required chunk(s)");
	if(!pols_read)
	    mavlib_lwobprinterror("  Missing POLS");
	if(!pnts_read)
	    mavlib_lwobprinterror("  Missing PNTS");
	if(!srfs_read)
	    mavlib_lwobprinterror("  Missing SRFS");
	if(pols_read)
	    mavlib_LWOBfreePOLSList(*polygons);
	if(pnts_read)
	    mav_free(*points);
	
	mavlib_LWOBIFFClose(file);
	return 1;	
    }
    
    if (mav_opt_output==MAV_VERBOSE) {
      fprintf(stderr, "\rReading lightwave object data from %s... Data loaded OK.\n", filename);
      fprintf(stderr, "Object contains %d points and %d polygons.\n",*numpoints,(*polygons)->numverts);
    }

    mavlib_LWOBIFFClose(file);
    return 0;
}

void mavlib_LWOBfreeFacetArrays(MAV_facet * facet)
{
    int i;
    /* free all arrays */

    for(i=0; i<facet->npolys; i++)
    {

	if(facet->norm[i]) {
	    mav_free (facet->norm[i]);
	}
	    
	if(facet->tex[i]) {
	    mav_free (facet->tex[i]);
	}
	    
	if(facet->vert) {
	    mav_free (facet->vert[i]);   
	}
    }

    if (facet->np)
	mav_free(facet->np);
    if(facet->norm)
	mav_free(facet->norm);
    if(facet->tex)
	mav_free(facet->tex);
    if(facet->vert)
	mav_free(facet->vert);
    if(facet->sp)
	mav_free(facet->sp);
}



typedef struct MAVLIB_LWOBLWOBTRIANGLELIST {
  int vertices [3];
  struct MAVLIB_LWOBLWOBTRIANGLELIST *next;
} MAVLIB_LWOBTriangleList;

int mavlib_LWOBTriCount;
int mavlib_LWOBVertCount;
MAVLIB_LWOBTriangleList *mavlib_LWOBCurrTri;

/* For old versions of GLU */

#ifndef GLU_VERSION_1_2
#define GLUtesselator GLUtriangulatorObj
#define gluTessBeginPolygon(X,Y) gluBeginPolygon((X))
#define gluTessBeginContour(X) gluNextContour((X), GLU_UNKNOWN)
#define gluTessEndContour(X) 
#define gluTessEndPolygon(X) gluEndPolygon((X))
#endif

#ifndef CALLBACK
#define CALLBACK
#endif

typedef void (CALLBACK *MAVLIB_GLUCB)();

void CALLBACK mavlib_LWOBTessBgn(GLenum type)
{
    mavlib_LWOBVertCount=0;
}

void CALLBACK mavlib_LWOBTessv3f(void *data)
{
    if (mavlib_LWOBVertCount==0) 
    {
	MAVLIB_LWOBTriangleList *t=
	    (MAVLIB_LWOBTriangleList *) mav_malloc(sizeof(MAVLIB_LWOBTriangleList));
	t->next= mavlib_LWOBCurrTri;
	mavlib_LWOBCurrTri=t;
	mavlib_LWOBTriCount++;
    }
    
    mavlib_LWOBCurrTri->vertices[mavlib_LWOBVertCount]= *((int *) data);
    mavlib_LWOBVertCount++;
    
    if (mavlib_LWOBVertCount==3) mavlib_LWOBVertCount=0;
}

void CALLBACK mavlib_LWOBTessEnd(void)
{
    if (mavlib_LWOBVertCount!=0) 
    {
	mavlib_lwobprinterror("Error - Facet tessellation ended with vertex count != 0");
	exit(1);
    }
}

void CALLBACK mavlib_LWOBTessEdge(GLboolean f)
{
}

void CALLBACK mavlib_LWOBTessErr(GLenum en)
{
    char message[]="Warning - Triangulation failed because there were";
    char * buf=(char *) mav_malloc(strlen((char *)gluErrorString(en))+strlen(message)+2);
    sprintf(buf, "%s %s", message, gluErrorString(en));
    mavlib_lwobprinterror(buf);
    mav_free(buf);
}



int mav_compositeReadLWO(char *filename, MAV_composite *c, MAV_matrix mat)
{
    MAVLIB_LWOBVertex * points;
    MAVLIB_LWOBPolygonList * polygons;
    MAVLIB_LWOBPolygonList * current_polygon;
    MAVLIB_LWOBPolygonList * last_polygon;
    MAVLIB_LWOBSurfaceName * surfaces;
    MAVLIB_LWOBSurfaceName * s;
    MAVLIB_LWOBSurfaceName * old_s;
    int numpoints;

    int numcolours, currentcolour=0;
    int i, j;

    int surfacesTriangulated=0;
    int newSurfaces=0;
    int deletedSurfaces=0;

    MAV_facet *facet= (MAV_facet *) mav_malloc(sizeof(MAV_facet));

    MAVLIB_LWOBSurface* palettemap;


    
    facet->np=NULL;
    facet->norm=NULL;
    facet->tex=NULL;
    facet->vert=NULL;
    facet->sp=NULL;
    facet->userdef=NULL;
    facet->matrix=mat;
    facet->npolys=0;

    if(mavlib_LWOBparse_file(&surfaces, &points, &polygons, &numpoints, filename))
	return MAV_FALSE;

    /* The first entry in the polygon list is not
       a polygon but holds the number of polygons in
       the list */
    facet->npolys=polygons->numverts;    
    
    /* Calculate normals for all surfaces and vertices */
    
    last_polygon=polygons;
    current_polygon=polygons->next;

    for(i=0; i<facet->npolys; i++)
    {
	MAV_vector pt0=mav_vectorSub(points[current_polygon->vertices[1]].v,
				     points[current_polygon->vertices[0]].v);
	MAV_vector ptn;
	float dp=1;

	for(j=2; j<current_polygon->numverts; j++)
	{
	    ptn=mav_vectorSub(points[current_polygon->vertices[j]].v,
			      points[current_polygon->vertices[0]].v);
	    
	    dp=mav_vectorDotProduct(pt0, ptn);
	    if(dp > -1 && dp<1) break;
	}

	if (dp==1 || dp == -1)
	{
	    mavlib_lwobprinterror("Warning - polygon found without normal");
	    current_polygon->normal=mav_vectorSet(0,0,1);
	}

	current_polygon->normal=mav_vectorNormalise(mav_vectorCrossProduct(pt0,ptn));

	for(j=0; j<current_polygon->numverts; j++)
	{
	    points[current_polygon->vertices[j]].normal=
		mav_vectorAdd(current_polygon->normal,
			      points[current_polygon->vertices[j]].normal);
	}
	
	if(current_polygon->numverts < 3) {
	    /* Invalid polygon */
	    mavlib_lwobprinterror("Warning - Deleteing polygon with less than 3 vertices");
	    last_polygon->next=current_polygon->next;
	    mav_free(current_polygon->vertices);
	    mav_free(current_polygon);
	    current_polygon=last_polygon;
	    deletedSurfaces++;
	}
	else if(current_polygon->numverts > 3)	{

	    /******** Triangulate polygon ********/

	    int surface= current_polygon->surface;
	    MAV_vector normal= current_polygon->normal;
	    MAVLIB_LWOBPolygonList* next= current_polygon->next;
	    MAVLIB_LWOBTriangleList *temp_tri;
	    GLUtesselator *g= gluNewTess();

	    gluTessCallback(g, GLU_TESS_BEGIN, (MAVLIB_GLUCB) mavlib_LWOBTessBgn);
	    gluTessCallback(g, GLU_TESS_VERTEX, (MAVLIB_GLUCB) mavlib_LWOBTessv3f);
	    gluTessCallback(g, GLU_TESS_END, (MAVLIB_GLUCB) mavlib_LWOBTessEnd);
	    gluTessCallback(g, GLU_TESS_EDGE_FLAG, (MAVLIB_GLUCB) mavlib_LWOBTessEdge);
	    gluTessCallback(g, GLU_TESS_ERROR, (MAVLIB_GLUCB) mavlib_LWOBTessErr);

	    surfacesTriangulated++;
	    
	    mavlib_LWOBCurrTri= NULL;
	    mavlib_LWOBTriCount= 0;
	    mavlib_LWOBVertCount= 0;
	    
	    gluTessBeginPolygon(g, NULL);
	    gluTessBeginContour(g);

	    for (j=0; j<current_polygon->numverts; j++) {
		GLdouble thisVertex[3];
		
		thisVertex[0]=points[current_polygon->vertices[j]].v.x;
		thisVertex[1]=points[current_polygon->vertices[j]].v.y;
		thisVertex[2]=points[current_polygon->vertices[j]].v.z;
		
		gluTessVertex(g, thisVertex,
			      &current_polygon->vertices[j]);
	    }
	    
	    gluTessEndContour(g);
	    gluTessEndPolygon(g);
	    gluDeleteTess(g);	    

	    if(mavlib_LWOBCurrTri == NULL) {
		/* Failed to triangulate polygon */
		mavlib_lwobprinterror("Warning - Deleteing polygon that failed to triangulate");
		last_polygon->next=next;
		mav_free(current_polygon->vertices);
		mav_free(current_polygon);
		current_polygon=last_polygon;
		deletedSurfaces++;
	    } else {
		mav_free(current_polygon->vertices);

		while (mavlib_LWOBCurrTri != NULL)
		{
		    current_polygon->numverts=3;
		    current_polygon->vertices=mav_malloc(sizeof(int)*3);
		    
		    if(mavlib_LWOBCurrTri->vertices[0] >= numpoints || mavlib_LWOBCurrTri->vertices[1] >= numpoints || mavlib_LWOBCurrTri->vertices[2] >= numpoints) {
			mavlib_lwobprinterror("Triangulation returned invalid point");
			exit(1);
		    }
		    current_polygon->vertices[0]=mavlib_LWOBCurrTri->vertices[0];
		    current_polygon->vertices[1]=mavlib_LWOBCurrTri->vertices[1];
		    current_polygon->vertices[2]=mavlib_LWOBCurrTri->vertices[2];
		    current_polygon->surface=surface;
		    current_polygon->normal=normal;
		    
		    if(mavlib_LWOBCurrTri->next!=NULL)
		    {
			newSurfaces++;
			current_polygon->next=
			    (MAVLIB_LWOBPolygonList *) mav_malloc(sizeof (MAVLIB_LWOBPolygonList));
			current_polygon=current_polygon->next;
		    }
		    
		    temp_tri=mavlib_LWOBCurrTri;
		    mavlib_LWOBCurrTri=mavlib_LWOBCurrTri->next;
		    mav_free(temp_tri);
		}
	    } 
	    current_polygon->next=next;

	    /*************************************/
	}

	last_polygon=current_polygon;
	current_polygon=current_polygon->next;
    }	
    
    facet->npolys+=newSurfaces-deletedSurfaces;
    polygons->numverts=facet->npolys;

    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Triangulated %d polygons, creating %d new polygons and deleting %d\n",
	   surfacesTriangulated, newSurfaces, deletedSurfaces);


    current_polygon=polygons->next;

    facet->np=(int *)mav_malloc(sizeof(int) * facet->npolys);
    if(facet->np==NULL)
    {
	mavlib_lwobprinterror("Failed to allocate facet point number array");
	mavlib_LWOBfreeFacetArrays(facet);	
	return MAV_FALSE;
    }

    facet->norm=(MAV_vector **)mav_calloc(facet->npolys, sizeof(MAV_vector *));
    if(facet->norm==NULL)
    {
	mavlib_lwobprinterror("Failed to allocate facet normal coordinate array");
	mavlib_LWOBfreeFacetArrays(facet);	
	return MAV_FALSE;
    } 

    facet->tex=(MAV_texCoord **)mav_calloc(facet->npolys, sizeof(MAV_texCoord *));
    if(facet->tex==NULL)
    {
	mavlib_lwobprinterror("Failed to allocate texture coordinate array");
	mavlib_LWOBfreeFacetArrays(facet);	
	return MAV_FALSE;
    } 

    facet->vert=(MAV_vector **)mav_calloc(facet->npolys, sizeof(MAV_vector *));
    if(facet->vert==NULL)
    {
	mavlib_lwobprinterror("Failed to allocate vertex coordinate array");
	mavlib_LWOBfreeFacetArrays(facet);	
	return MAV_FALSE;
    }    

    facet->sp=(MAV_surfaceParams **) mav_calloc(facet->npolys, sizeof(MAV_surfaceParams *));
    if(facet->sp==NULL)
    {
	mavlib_lwobprinterror("Failed to allocate surface coordinate array");
	mavlib_LWOBfreeFacetArrays(facet);	
	return MAV_FALSE;
    } 

    /* Create surface parameters */
    s=surfaces;    
    for(numcolours=0; s!=NULL; numcolours++)
    {
	s=s->next;
    }
    palettemap=(MAVLIB_LWOBSurface*)mav_malloc(sizeof(MAVLIB_LWOBSurface)*numcolours);

    s=surfaces;
    currentcolour=0;
    while(s!=NULL)
    {
	int c;
	float red,green,blue;

	if(s->isDefault) {
	    palettemap[currentcolour].sp= mav_sp_default;
	    palettemap[currentcolour].doublesided= 1;
	    palettemap[currentcolour].smoothing= 0;
	} else {
	    red=s->colour.red/255.0;
	    green=s->colour.green/255.0;
	    blue=s->colour.blue/255.0;
	    
	    c= mav_paletteMaterialIndexMatchGet(mav_palette_default,
						red, green, blue, 1.0 /* 1.0-trans */, 
						red, green, blue, 1.0 /* 1.0-trans */, 
						0/*spec*/, 0, 0, 1.0 /*1.0-trans*/, 
						0 /*emis*/, 0, 0, 1.0 /*1.0-trans*/, 0 /*shin*128.0*/);
	
	    if (c<0) 
	    {
		/* No joy, find empty material index to use */
		c= mav_paletteMaterialIndexEmptyGet(mav_palette_default);
	    
		if (c>=0) {
		    /* Set this material */
		    mav_paletteMaterialSet(mav_palette_default, c,
					   red, green, blue, 1.0 /* 1.0-trans */, 
					   red, green, blue, 1.0 /* 1.0-trans */, 
					   0/*spec*/, 0, 0, 1.0 /*1.0-trans*/, 
					   0 /*emis*/, 0, 0, 1.0 /*1.0-trans*/, 0 /*shin*128.0*/);
		
		    /* Construct and return the surface parameters */
		}
	    }
/*	printf("%d: %s %f %f %f\n",currentcolour, s->name,red,blue,green);*/
 
	    palettemap[currentcolour].sp= mav_surfaceParamsNew(MAV_MATERIAL, 0, c, 0);	
	    palettemap[currentcolour].doublesided= s->doublesided;
	    palettemap[currentcolour].smoothing= s->smoothing;
	}
	currentcolour++;
	old_s=s;
	s=s->next;
	mav_free(old_s);
    }


    /* Create polygons */
    current_polygon=polygons->next;
    for(i=0; i<facet->npolys; i++)
    {
	facet->np[i]=current_polygon->numverts;

	facet->norm[i]=(MAV_vector *) mav_malloc(facet->np[i]*sizeof(MAV_vector));
	if(facet->norm[i]==NULL)
	{
	    mavlib_lwobprinterror("Failed to allocate normal array");
	    mavlib_LWOBfreeFacetArrays(facet);	
	    return MAV_FALSE;
	} 
	facet->tex[i]=(MAV_texCoord *) mav_malloc(facet->np[i]*sizeof(MAV_texCoord));
	if(facet->tex[i]==NULL)
	{
	    mavlib_lwobprinterror("Failed to allocate tex array");
	    mavlib_LWOBfreeFacetArrays(facet);	
	    return MAV_FALSE;
	} 
	facet->vert[i]=(MAV_vector *) mav_malloc(facet->np[i]*sizeof(MAV_vector));
	if(facet->vert[i]==NULL)
	{
	    mavlib_lwobprinterror("Failed to allocate vertex array");
	    mavlib_LWOBfreeFacetArrays(facet);	
	    return MAV_FALSE;
	} 
	facet->sp[i]=palettemap[current_polygon->surface-1].sp;
	
	for (j=0; j<facet->np[i]; j++)
	{
	    /* Copy and reverse order of points */
	    facet->vert[i][j]=points[current_polygon->vertices[facet->np[i]-1-j]].v;
	    facet->vert[i][j].z*=-1;
	    if(palettemap[current_polygon->surface-1].smoothing)
		facet->norm[i][j]=mav_vectorNormalise(points[current_polygon->vertices[facet->np[i]-1-j]].normal);
	    else
		facet->norm[i][j]=mav_vectorNormalise(current_polygon->normal);

	    facet->norm[i][j].z*=-1;
	    /*	    mav_vectorPrint("Normal ",facet->norm[i][j]);*/
	    facet->tex[i][j].s=0;
	    facet->tex[i][j].t=0;
	}
    
	current_polygon=current_polygon->next;
    }

    mav_free(palettemap);
    mavlib_LWOBfreePOLSList(polygons);
    mav_free(points);    

    c->numobj=1;
    c->obj= mav_malloc(c->numobj*sizeof(MAV_object *));
    c->obj[0]= mav_objectNew(mav_class_facet, facet);
    if (mav_opt_compositeSetMatrix) c->matrix= MAV_ID_MATRIX;
    c->filename= strdup(filename);

    /* Calculate and store its BB */
    mav_compositeCalcBB(c);

    return MAV_TRUE;
}
