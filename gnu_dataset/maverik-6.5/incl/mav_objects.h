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

#ifndef _MAV_OBJECTS_INCLUDE
#define _MAV_OBJECTS_INCLUDE

#ifdef MAVAPI
#undef MAVAPI
#endif
#if defined(WIN32) && !defined(__CYGWIN__) 
#ifdef LIBMAVERIK_EXPORTS
#define MAVAPI __declspec(dllexport) 
#else
#define MAVAPI __declspec(dllimport) 
#endif
#else
#define MAVAPI 
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Common objects */

/* Box */

typedef struct {
  MAV_vector size;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
  void *userdef;
} MAV_box;

MAVAPI extern MAV_class *mav_class_box;
MAVAPI int mav_boxDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_boxBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_boxBB2(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_boxIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi);
MAVAPI int mav_boxID(MAV_object *o, char **id);
MAVAPI int mav_boxGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_boxGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_boxGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_boxDump(MAV_object *o);

/* Pyramid */

typedef struct {
  float bot_size_x;
  float bot_size_y;
  float top_size_x;
  float top_size_y;
  float offset_x;
  float offset_y;
  float height;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
  void *userdef;
} MAV_pyramid;

MAVAPI extern MAV_class *mav_class_pyramid;
MAVAPI int mav_pyramidDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_pyramidBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_pyramidBB2(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_pyramidIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi);
MAVAPI int mav_pyramidID(MAV_object *o, char **id);
MAVAPI int mav_pyramidGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_pyramidGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_pyramidGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_pyramidDump(MAV_object *o);

/* Cylinder */

typedef struct {
  float radius;
  float height;  
  int nverts;
  int endcap;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
  void *userdef;
} MAV_cylinder;

MAVAPI extern MAV_class *mav_class_cylinder;
MAVAPI int mav_cylinderDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_cylinderBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_cylinderBB2(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_cylinderIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi);
MAVAPI int mav_cylinderID(MAV_object *o, char **id);
MAVAPI int mav_cylinderGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_cylinderGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_cylinderGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_cylinderDump(MAV_object *o);

/* Cone */

typedef struct {
  float rt;
  float rb;
  float height;  
  int nverts;
  int endcap;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
  void *userdef;
} MAV_cone;

MAVAPI extern MAV_class *mav_class_cone;
MAVAPI int mav_coneDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_coneBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_coneBB2(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_coneIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi);
MAVAPI int mav_coneID(MAV_object *o, char **id);
MAVAPI int mav_coneGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_coneGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_coneGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_coneDump(MAV_object *o);

/* Sphere */

typedef struct {
  float radius;
  int nverts;
  int nchips;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
  void *userdef;
} MAV_sphere;

MAVAPI extern MAV_class *mav_class_sphere;
MAVAPI int mav_sphereDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_sphereBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_sphereBB2(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_sphereIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi);
MAVAPI int mav_sphereID(MAV_object *o, char **id);
MAVAPI int mav_sphereGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_sphereGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_sphereGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_sphereDump(MAV_object *o);

/* Half sphere */

typedef struct {
  float radius;
  int nverts;
  int nchips;
  int endcap;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
  void *userdef;
} MAV_hsphere;

MAVAPI extern MAV_class *mav_class_hsphere;
MAVAPI int mav_hsphereDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_hsphereBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_hsphereBB2(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_hsphereIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi);
MAVAPI int mav_hsphereID(MAV_object *o, char **id);
MAVAPI int mav_hsphereGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_hsphereGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_hsphereGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_hsphereDump(MAV_object *o);

/* Ellipse */

typedef struct {
  float radius;
  float height;
  int nverts;
  int nchips;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
  void *userdef;
} MAV_ellipse;

MAVAPI extern MAV_class *mav_class_ellipse;
MAVAPI int mav_ellipseDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_ellipseBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_ellipseBB2(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_ellipseIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi);
MAVAPI int mav_ellipseID(MAV_object *o, char **id);
MAVAPI int mav_ellipseGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_ellipseGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_ellipseGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_ellipseDump(MAV_object *o);

/* Half ellipse */

typedef struct {
  float radius;
  float height;
  int nverts;
  int nchips;
  int endcap;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
  void *userdef;
} MAV_hellipse;

MAVAPI extern MAV_class *mav_class_hellipse;
MAVAPI int mav_hellipseDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_hellipseBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_hellipseBB2(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_hellipseIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi);
MAVAPI int mav_hellipseID(MAV_object *o, char **id);
MAVAPI int mav_hellipseGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_hellipseGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_hellipseGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_hellipseDump(MAV_object *o);

/* Circular torus */

typedef struct {
  float rmajor;
  float rminor;
  float angle;
  int nverts;
  int nchips;
  int endcap;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
  void *userdef;
} MAV_ctorus;

MAVAPI extern MAV_class *mav_class_ctorus;
MAVAPI int mav_ctorusDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_ctorusBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_ctorusBB2(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_ctorusIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi);
MAVAPI int mav_ctorusID(MAV_object *o, char **id);
MAVAPI int mav_ctorusGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_ctorusGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_ctorusGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_ctorusDump(MAV_object *o);

/* Rectangular torus */

typedef struct {
  float radius;
  float width;
  float height;
  float angle;
  int nchips;
  int endcap;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
  void *userdef;
} MAV_rtorus;

MAVAPI extern MAV_class *mav_class_rtorus;
MAVAPI int mav_rtorusDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_rtorusBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_rtorusBB2(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_rtorusIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi);
MAVAPI int mav_rtorusID(MAV_object *o, char **id);
MAVAPI int mav_rtorusGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_rtorusGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_rtorusGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_rtorusDump(MAV_object *o);

/* Polygon */

typedef struct {
  int np;
  MAV_vector norm;
  MAV_texCoord *tex;
  MAV_vector *vert;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
  void *userdef;
} MAV_polygon;

MAVAPI extern MAV_class *mav_class_polygon;
MAVAPI int mav_polygonDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_polygonBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_polygonBB2(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_polygonIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi);
MAVAPI int mav_polygonID(MAV_object *o, char **id);
MAVAPI int mav_polygonGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_polygonGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_polygonGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_polygonDump(MAV_object *o);

/* Polygon group */

typedef struct {
  int npolys;
  int *np;
  MAV_vector *norm;
  MAV_texCoord **tex;
  MAV_vector **vert;
  MAV_surfaceParams **sp;
  MAV_matrix matrix;
  void *userdef;
} MAV_polygonGrp;

MAVAPI extern MAV_class *mav_class_polygonGrp;
MAVAPI int mav_polygonGrpDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_polygonGrpBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_polygonGrpBB2(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_polygonGrpIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi);
MAVAPI int mav_polygonGrpID(MAV_object *o, char **id);
MAVAPI int mav_polygonGrpGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_polygonGrpGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_polygonGrpGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_polygonGrpDump(MAV_object *o);

/* Facet */

typedef struct {
  int npolys;
  int *np;
  MAV_vector **norm;
  MAV_texCoord **tex;
  MAV_vector **vert;
  MAV_surfaceParams **sp;
  MAV_matrix matrix;
  void *userdef;
} MAV_facet;

MAVAPI extern MAV_class *mav_class_facet;
MAVAPI int mav_facetDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_facetBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_facetBB2(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_facetIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi);
MAVAPI int mav_facetID(MAV_object *o, char **id);
MAVAPI int mav_facetGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_facetGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_facetGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_facetDump(MAV_object *o);

/* Rectangle */

typedef struct {
  float width;
  float height;
  float xtile;
  float ytile;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
  void *userdef;
} MAV_rectangle;

MAVAPI extern MAV_class *mav_class_rectangle;
MAVAPI int mav_rectangleDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_rectangleBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_rectangleIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi);
MAVAPI int mav_rectangleID(MAV_object *o, char **id);
MAVAPI int mav_rectangleGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_rectangleGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_rectangleGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_rectangleDump(MAV_object *o);

/* Polyline */

typedef struct {
  int nlines;
  int *np;
  int *closed;
  MAV_vector **vert;
  MAV_surfaceParams **sp;
  MAV_matrix matrix;
  void *userdef;
} MAV_polyline;

MAVAPI extern MAV_class *mav_class_polyline;
MAVAPI int mav_polylineDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_polylineBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_polylineBB2(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_polylineID(MAV_object *o, char **id);
MAVAPI int mav_polylineGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_polylineGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_polylineGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_polylineDump(MAV_object *o);

/* Text */

#define MAV_CENTER_JUSTIFY 1
#define MAV_CENTRE_JUSTIFY 1
#define MAV_LEFT_JUSTIFY 2
#define MAV_RIGHT_JUSTIFY 3
#define MAV_STROKE_FONT 1
#define MAV_OUTLINE_FONT 2
#define MAV_FILLED_FONT 3

typedef struct {
  char *text;
  int style;
  int justify;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
  void *userdef;
} MAV_text;

MAVAPI extern MAV_class *mav_class_text;
MAVAPI int mav_textDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_textBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_textID(MAV_object *o, char **id);
MAVAPI int mav_textGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_textGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_textGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_textDump(MAV_object *o);

/* Composite */

typedef struct {
  int numobj;
  MAV_object **obj;
  MAV_BB bb;
  int selobj;
  char *filename;
  MAV_matrix matrix;
  void *userdef;
} MAV_composite;

MAVAPI extern MAV_class *mav_class_composite;
MAVAPI int mav_compositeDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_compositeBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_compositeIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi);
MAVAPI int mav_compositeID(MAV_object *o, char **id);
MAVAPI int mav_compositeGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_compositeGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_compositeGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_compositeDump(MAV_object *o);
MAVAPI void mav_compositeEmpty(MAV_composite *c);
MAVAPI int mav_compositeCalcBB(MAV_composite *c);

/* Composite file format support */

#define MAV_MAX_COMPOSITE_FORMATS 10

typedef int (*MAV_compositeReadFn)(char *, MAV_composite *, MAV_matrix);

typedef struct {
  int defined;
  char *ext;
  MAV_compositeReadFn fn;
} MAV_compositeFormat;

MAVAPI extern MAV_compositeFormat mav_compositeFormat[MAV_MAX_COMPOSITE_FORMATS];

MAVAPI int mav_compositeRead(char *filename, MAV_composite *c, MAV_matrix m);
MAVAPI int mav_compositeReadAC3D(char *filename, MAV_composite *c, MAV_matrix m);
MAVAPI int mav_compositeReadAC3DBuf(char *buf, MAV_composite *c, MAV_matrix m);
MAVAPI int mav_compositeReadJIF(char *filename, MAV_composite *c, MAV_matrix m);



/* SMS object */

typedef struct {
  MAV_SMS *sms;
  MAV_object *selobj;
  MAV_matrix matrix;
  void *userdef;
} MAV_SMSObj;

MAVAPI extern MAV_class *mav_class_SMSObj;
MAVAPI int mav_SMSObjDraw(MAV_object *o, MAV_drawInfo *di);
MAVAPI int mav_SMSObjBB(MAV_object *o, MAV_BB *bb);
MAVAPI int mav_SMSObjIntersect(MAV_object *o, MAV_line *ln, MAV_objectIntersection *oi);
MAVAPI int mav_SMSObjID(MAV_object *o, char **id);
MAVAPI int mav_SMSObjGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_SMSObjGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_SMSObjGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_SMSObjDump(MAV_object *o);



/* Teapot */

typedef enum { TETLEY, PG_TIPS, EARL_GREY } MAV_teabag;

typedef struct {
  float size;
  int subdivisions;
  MAV_teabag teabag;
  int lumps;
  MAV_surfaceParams *sp;
  MAV_matrix matrix;
  void *userdef;
} MAV_teapot;

MAVAPI extern MAV_class *mav_class_teapot;
MAVAPI int mav_teapotDraw(MAV_object *obj, MAV_drawInfo *di);
MAVAPI int mav_teapotBB(MAV_object *obj, MAV_BB *bb);
MAVAPI int mav_teapotID(MAV_object *o, char **id);
MAVAPI int mav_teapotGetUserdef(MAV_object *o, void ***ud);
MAVAPI int mav_teapotGetMatrix(MAV_object *o, MAV_matrix **m);
MAVAPI int mav_teapotGetSurfaceParams(MAV_object *o, MAV_surfaceParams ***sp);
MAVAPI int mav_teapotDump(MAV_object *o);



/* Supporting routines */

MAVAPI int mav_linePolygonIntersection(MAV_polygon *p, MAV_line ln, MAV_objectIntersection *oi);



/* Options */

#define MAV_BB_FAST     1
#define MAV_BB_ACCURATE 2

MAVAPI extern int mav_opt_BBMethod;
MAVAPI extern int mav_opt_compositeSetMatrix;
MAVAPI extern int mav_opt_splash;
MAVAPI extern int mav_opt_curveLOD;
MAVAPI extern float mav_opt_curveFactor;
MAVAPI extern int mav_opt_vertsMin;
MAVAPI extern int mav_opt_vertsMax;
MAVAPI extern float mav_opt_drawNormals;



/* Module initialise */

MAVAPI int mav_objectsModuleInit(void);
MAVAPI char *mav_objectsModuleID(void);

#ifdef __cplusplus
}
#endif
#endif
