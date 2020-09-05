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

#ifndef _MAV_KERNEL_INCLUDE
#define _MAV_KERNEL_INCLUDE

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

#include <stdio.h>
#include <inttypes.h>

#ifdef __cplusplus
extern "C" {
#endif

/* #define constants */

#define MAV_VERSION(X,Y) ((X)*256+(Y))
#define MAV_THIS_VERSION MAV_VERSION(6,2)

#define MAV_FALSE 0
#define MAV_TRUE 1
#define MAV_DONTCARE 2
#define MAV_UNDEFINED -1

#define MAV_MAX_CBS 100
#define MAV_MAX_WIN 10

#define MAV_COLOUR 1
#define MAV_MATERIAL 2
#define MAV_TEXTURE 3
#define MAV_LIT_TEXTURE 4 
#define MAV_BLENDED_TEXTURE 5

#define MAV_COLOUR_BLACK -10
#define MAV_COLOUR_WHITE -11
#define MAV_COLOUR_RED   -12
#define MAV_COLOUR_GREEN -13
#define MAV_COLOUR_BLUE  -14

#define MAV_LIGHT_RELATIVE 0
#define MAV_LIGHT_ABSOLUTE 1

#define MAV_SILENT 0
#define MAV_VERBOSE 1

#define MAV_EPSILON 0.001
#define MAV_INFINITY 1.0E+20
#define MAV_PI_OVER_180 0.017453292
#define MAV_180_OVER_PI 57.29578
#define MAV_PI 3.1415927
#define MAV_PI_OVER_2 1.5707963
#define MAV_2_PI 6.2831853
#define MAV_DEG2RAD(X) ((X)*MAV_PI_OVER_180)
#define MAV_RAD2DEG(X) ((X)*MAV_180_OVER_PI)
#define MAV_MATRIX_XCOMP 0][3
#define MAV_MATRIX_YCOMP 1][3
#define MAV_MATRIX_ZCOMP 2][3
#define MAV_MATRIX_XAXIS_X 0][0
#define MAV_MATRIX_XAXIS_Y 1][0
#define MAV_MATRIX_XAXIS_Z 2][0
#define MAV_MATRIX_YAXIS_X 0][1
#define MAV_MATRIX_YAXIS_Y 1][1
#define MAV_MATRIX_YAXIS_Z 2][1
#define MAV_MATRIX_ZAXIS_X 0][2
#define MAV_MATRIX_ZAXIS_Y 1][2
#define MAV_MATRIX_ZAXIS_Z 2][2

#define MAV_REDEFINE_WARN 1
#define MAV_REDEFINE_NOWARN 2

#define MAV_MAX_CLIP_PLANES 10

#define MAV_ANIMATE_TIME  0
#define MAV_ANIMATE_FRAME 1
#define MAV_ANIMATE_DISTANCE 2
#define MAV_ANIMATE_LINEAR 16
#define MAV_ANIMATE_S 32

#define mav_vectorNormalise mav_vectorNormalize
#define mav_initialize mav_initialise
#define mav_initializeNoArgs mav_initialiseNoArgs
#define mav_vectorMagnitude mav_vectorMag

/* Maverik data types */

/* Maths */

typedef struct {
  float x;
  float y;
  float z;
} MAV_vector;

typedef struct {
  float s;
  float t;
} MAV_texCoord;

typedef struct {
  float mat[4][4];
} MAV_matrix;

typedef struct {
  float w;
  float x;
  float y;
  float z;
} MAV_quaternion;

typedef struct {
  uint32_t sec;
  uint32_t usec;
  uint32_t cpu;
} MAV_time;

typedef struct {
  MAV_time start;
  MAV_time end;
  MAV_time elapsed;
  float wall;
  float cpu;
} MAV_timer;

/* Callbacks, classes and objects */

typedef struct {
  int num;
} MAV_callback;

typedef struct {
  void *the_data;
  struct MAV_CLASS *the_class;
} MAV_object;

typedef int (*MAV_callbackFn)(MAV_object *, void *, void *);

typedef struct MAV_CLASS {
  MAV_callbackFn fn[MAV_MAX_CBS][MAV_MAX_WIN];
} MAV_class;

/* As above for SMS's */

typedef struct {
  int num;
} MAV_SMSCallback;

typedef struct {
  void *the_data;
  struct MAV_SMS_CLASS *the_class;
  int selectable[MAV_MAX_WIN];
  void *userdef;
} MAV_SMS;

typedef int (*MAV_SMSCallbackFn)(MAV_SMS *, void *, void *, void *, void *);

typedef struct MAV_SMS_CLASS {
  MAV_SMSCallbackFn fn[MAV_MAX_CBS];
} MAV_SMSClass;

/* Lists */

typedef struct MAV_LISTITEM {
  void *data1;
  void *data2;
  struct MAV_LISTITEM *next;
  struct MAV_LISTITEM *prev;
} MAV_listItem;

typedef struct MAV_LISTPOINTER {
  MAV_listItem *item;
  struct MAV_LISTPOINTER *next;
} MAV_listPointer;

typedef struct {
  int length;
  MAV_listItem *head;
  MAV_listItem *tail;
  MAV_listPointer *current;
} MAV_list;

/* Palette */

typedef struct {
  int id;
  int defwarn;
  int defined;
  float colour[4];
} MAV_colour;

typedef struct {
  int id;
  int defwarn;
  int defined;
  float ambient[4];
  int localviewer;
} MAV_lightingModel;

typedef struct {
  int id;
  int index;
  int defwarn;
  int defined;
  float ambient[4];
  float diffuse[4];
  float specular[4];
  MAV_vector pos;
  int positioning;
} MAV_light; 

typedef struct {
  int id;
  int defwarn;
  int defined;
  float ambient[4];
  float diffuse[4];
  float specular[4];
  float emission[4];
  float shine;
} MAV_material;

struct MAV_TEXTURE_STRUCT;
typedef void (*MAV_texEnvFn)(struct MAV_TEXTURE_STRUCT *);

typedef struct MAV_TEXTURE_STRUCT {
  int id;
  int defwarn;
  int defined;
  int width;
  int height;
  uint32_t *mem;
  char *filename;
  MAV_texEnvFn texEnv;
  int transparent;
  int mipmapped;
  int nmaps;
  int *xsize;
  int *ysize;
  uint32_t **mipmap;
} MAV_texture;

typedef struct {
  int id;
  int defwarn;
  int defined;
  char *name;
  int width[256];
} MAV_font;

typedef struct {
  int defwarn;
  int lm_defwarn;
  MAV_lightingModel lm;
  int light_defwarn;
  MAV_light *lightlist;
  int col_defwarn;
  MAV_colour *collist;
  int mat_defwarn;
  MAV_material *matlist;
  int tex_defwarn;
  MAV_texture *texlist;
  MAV_texEnvFn texEnv;
  int font_defwarn;
  MAV_font *fontlist;
} MAV_palette;

/* Surface and window view modifier parameters (i.e. stereo params) */

typedef struct {
  int mode;
  int colour;
  int material;
  int texture;
} MAV_surfaceParams;

typedef struct {
  float offset;
  float angle;
  void *userdef;
} MAV_viewModifierParams;

/* Clip planes */

typedef struct {
  MAV_vector norm;
  float d;
} MAV_clipPlane;

typedef struct {
  int num;
  MAV_clipPlane planes[MAV_MAX_CLIP_PLANES];
} MAV_clipPlanes;

/* Window and view parameters */

struct MAV_WINDOW;
typedef void (*MAV_viewModifierFn)(struct MAV_WINDOW *);

typedef struct {
  MAV_vector eye;
  MAV_vector view;
  MAV_vector up;
  MAV_vector fixed_up;
  MAV_viewModifierFn mod;
  MAV_vector right;
  MAV_vector trans_eye;
  MAV_vector trans_view;
  MAV_vector trans_up;
  MAV_vector trans_right;
  void *userdef;
} MAV_viewParams;

typedef struct MAV_WINDOW {
  int id;
  char *name;
  int x;
  int y;
  int width;
  int height;
  MAV_viewParams *vp;
  MAV_viewModifierFn mod;
  MAV_viewModifierParams *vmp;
  MAV_vector eye;
  MAV_vector view;
  MAV_vector up;
  MAV_vector right;
  int orthogonal;
  float ncp;
  float fcp;
  float fov;
  float aspect;
  float offset;
  float angle;
  float ortho_size;
  MAV_matrix viewMat;
  MAV_matrix projMat;
  MAV_matrix pdvMat;
  float background_red;
  float background_green;
  float background_blue;
  MAV_palette *palette;
  MAV_vector ncpv[5];
  MAV_vector fcpv[5];
  void *userdef;
} MAV_window;



/* Maverik prototypes */

/* Object, classes and callbacks  */

MAVAPI MAV_object *mav_objectNew(MAV_class *c, void *d);
MAVAPI void mav_objectDelete(MAV_object *o);
MAVAPI MAV_class *mav_objectClassGet(MAV_object *o);
MAVAPI void *mav_objectDataGet(MAV_object *o);
MAVAPI MAV_object *mav_objectDataWith(void *d);
MAVAPI MAV_list *mav_objectSMSsGet(MAV_object *o);
MAVAPI void mav_objectTablesSMSAdd(MAV_object *o, MAV_SMS *s);
MAVAPI void mav_objectTablesSMSRmv(MAV_object *o, MAV_SMS *s); 

MAVAPI MAV_class *mav_classNew(void);

MAVAPI MAV_callback *mav_callbackNew(void);
MAVAPI void mav_callbackSet(MAV_callback *cb, MAV_window *w, MAV_class *c, MAV_callbackFn fn);
MAVAPI int  mav_callbackExec(MAV_callback *cb, MAV_window *w, MAV_object *o, void *d1, void *d2);
MAVAPI MAV_callbackFn mav_callbackQuery(MAV_callback *cb, MAV_window *w, MAV_object *o);

/* SMS's */

MAVAPI MAV_SMS *mav_SMSNew(MAV_SMSClass *sc, void *d);
MAVAPI void mav_SMSDelete(MAV_SMS *s, int o);
MAVAPI void *mav_SMSDataGet(MAV_SMS *s);
MAVAPI MAV_SMSClass *mav_SMSClassGet(MAV_SMS *s);
MAVAPI void mav_SMSSelectabilitySet(MAV_SMS *s, MAV_window *w, int v);

MAVAPI MAV_SMSClass *mav_SMSClassNew(void);

MAVAPI MAV_SMSCallback *mav_SMSCallbackNew(void);
MAVAPI void mav_SMSCallbackSet(MAV_SMSCallback *scb, MAV_SMSClass *sc, MAV_SMSCallbackFn fn);
MAVAPI int  mav_SMSCallbackExec(MAV_SMSCallback *scb, MAV_SMS *s, void *d1, void *d2, void *d3, void *d4);
MAVAPI MAV_SMSCallbackFn mav_SMSCallbackQuery(MAV_SMSCallback *scb, MAV_SMS *s);

/* Lists */

MAVAPI MAV_list *mav_listNew(void);
MAVAPI void mav_listItemAdd(MAV_list *l, void *d);
MAVAPI void mav_listItemRmv(MAV_list *l, void *d);
MAVAPI int  mav_listItemNext(MAV_list *l, void **d);
MAVAPI int  mav_listItemContains(MAV_list *l, void *d);
MAVAPI void mav_listPointerReset(MAV_list *l);
MAVAPI void mav_listPointerPush(MAV_list *l);
MAVAPI void mav_listPointerPop(MAV_list *l);
MAVAPI void mav_listEmpty(MAV_list *l);
MAVAPI void mav_listDelete(MAV_list *l); 
MAVAPI int  mav_listSize(MAV_list *l);
MAVAPI void mav_listPrint(char *s, MAV_list *l);

MAVAPI void mav_listItemsAdd(MAV_list *l, void *d1, void *d2);
MAVAPI void mav_listItemsRmv(MAV_list *l, void *d1, void *d2);
MAVAPI int  mav_listItemsNext(MAV_list *l, void **d1, void **d2);
MAVAPI int  mav_listItemsContains(MAV_list *l, void *d1, void *d2);

/* Window */

MAVAPI MAV_window *mav_windowNew(int x, int y, int w, int h, char *name, char *disp);
MAVAPI void mav_windowDelete(MAV_window *w);
MAVAPI void mav_windowDeleteID(int id);
MAVAPI void mav_windowSet(MAV_window *w);
MAVAPI void mav_windowBackgroundColourSet(MAV_window *w, float r, float g, float b);
MAVAPI void mav_windowBackfaceCullSet(MAV_window *w, int v);
MAVAPI int  mav_windowBackfaceCullGet(MAV_window *w);
MAVAPI void mav_windowBlendSet(MAV_window *w, int v);
MAVAPI void mav_windowLineWidthSet(MAV_window *w, float v);
MAVAPI void mav_windowLineStippleSet(MAV_window *w, int factor, unsigned short pattern);
MAVAPI void mav_windowFogSet(MAV_window *w, int type, float data1, float data2, float r, float g, float b);
MAVAPI void mav_windowPerspectiveSet(MAV_window *w, float ncp, float fcp, float fov, float aspect);
MAVAPI void mav_windowOrthogonalSet(MAV_window *w, float ncp, float fcp, float size, float aspect);
MAVAPI void mav_windowPolygonModeSet(MAV_window *w, int v);
MAVAPI void mav_windowMultiSampleSet(MAV_window *w, int v);
MAVAPI void mav_windowViewParamsSet(MAV_window *w, MAV_viewParams *vp);
MAVAPI void mav_windowViewModifierSet(MAV_window *w, MAV_viewModifierParams *vmp, MAV_viewModifierFn fn);
MAVAPI void mav_eyeMono(MAV_window *w);

/* View parameters */

MAVAPI void mav_viewParamsPrint(char *s, MAV_viewParams vp);
MAVAPI void mav_viewParamsFixed(MAV_window *w);
MAVAPI MAV_viewParams mav_viewParamsInterpolate(MAV_viewParams st, MAV_viewParams fi, float val);
MAVAPI void mav_viewParamsAnimate(MAV_viewParams *targ, MAV_viewParams st, MAV_viewParams fi, float len, int unit);

/* Frame control */

typedef void (*MAV_frameFn)(void *);
MAVAPI void mav_frameBegin(void);
MAVAPI void mav_frameEnd(void);
MAVAPI void mav_frameFn0Add(MAV_frameFn fn, void *d);
MAVAPI void mav_frameFn0Rmv(MAV_frameFn fn, void *d);
MAVAPI void mav_frameFn1Add(MAV_frameFn fn, void *d);
MAVAPI void mav_frameFn1Rmv(MAV_frameFn fn, void *d);
MAVAPI void mav_frameFn2Add(MAV_frameFn fn, void *d);
MAVAPI void mav_frameFn2Rmv(MAV_frameFn fn, void *d);
MAVAPI void mav_frameFn3Add(MAV_frameFn fn, void *d);
MAVAPI void mav_frameFn3Rmv(MAV_frameFn fn, void *d);
MAVAPI void mav_frameFn4Add(MAV_frameFn fn, void *d);
MAVAPI void mav_frameFn4Rmv(MAV_frameFn fn, void *d);

/* Palette */

MAVAPI MAV_palette *mav_paletteNew(void);
MAVAPI void mav_windowPaletteSet(MAV_window *w, MAV_palette *p);
MAVAPI void mav_paletteTextureEnvPaletteSet(MAV_palette *p, MAV_texEnvFn fn);
MAVAPI void mav_paletteColourSet(MAV_palette *p, int index, float r, float g, float b, float a);
MAVAPI void mav_paletteMaterialSet(MAV_palette *p, int index, float ar, float ag, float ab, float aa, 
  		     float dr, float dg, float db, float da, float sr, float sg, float sb, float sa, 
                     float er, float eg, float eb, float ea, float shin);
MAVAPI int  mav_paletteTextureSet(MAV_palette *p, int index, char *filename);
MAVAPI void mav_paletteTextureAlphaSet(MAV_palette *p, int index, float a);
MAVAPI void mav_paletteTextureColourAlphaSet(MAV_palette *p, int index, int r, int g, int b, float a);
MAVAPI void mav_paletteTextureMipmappingSet(MAV_palette *p, int index, int v);
MAVAPI int  mav_paletteTextureSetFromMem(MAV_palette *p, int index, int width, int height, uint32_t *mem);
MAVAPI int  mav_paletteTextureEnvSet(MAV_palette *p, int index, MAV_texEnvFn fn);
MAVAPI void mav_texEnvDefault(MAV_texture *tex);
MAVAPI void mav_texEnvClamp(MAV_texture *tex);
MAVAPI void mav_paletteTextureFree(MAV_palette *p, int index);
MAVAPI void mav_paletteLightSet(MAV_palette *p, int index, float ar, float ag, float ab, float aa, 
		  float dr, float dg, float db, float da, float sr, float sg, float sb, float sa);
MAVAPI void mav_paletteLightPos(MAV_palette *p, int index, MAV_vector pos);
MAVAPI void mav_paletteLightPositioning(MAV_palette *p, int index, int pos);
MAVAPI void mav_paletteLightingModelSet(MAV_palette *p, float ar, float ag, float ab, float aa, int local);
MAVAPI void mav_paletteFontSet(MAV_palette *p, int index, char *s);

MAVAPI int mav_paletteColourIndexEmptyGet(MAV_palette *p);
MAVAPI int mav_paletteColourIndexMatchGet(MAV_palette *p, float r, float g, float b, float a);
MAVAPI int mav_paletteMaterialIndexEmptyGet(MAV_palette *p);
MAVAPI int mav_paletteMaterialIndexMatchGet(MAV_palette *p, float ar, float ag, float ab, float aa, 
                                     float dr, float dg, float db, float da, float sr, float sg, float sb, float sa, 
                                     float er, float eg, float eb, float ea, float shin);
MAVAPI int mav_paletteTextureIndexEmptyGet(MAV_palette *p);
MAVAPI int mav_paletteTextureIndexMatchGet(MAV_palette *p, char *filename);
MAVAPI int mav_paletteLightIndexEmptyGet(MAV_palette *p);
MAVAPI int mav_paletteLightIndexMatchGet(MAV_palette *p, float ar, float ag, float ab, float aa, 
		                  float dr, float dg, float db, float da, float sr, float sg, float sb, float sa);
MAVAPI int mav_paletteFontIndexEmptyGet(MAV_palette *p);
MAVAPI int mav_paletteFontIndexMatchGet(MAV_palette *p, char *s);

MAVAPI void mav_paletteWarnSet(MAV_palette *p, int v);
MAVAPI void mav_paletteColourWarnSet(MAV_palette *p, int v);
MAVAPI void mav_paletteMaterialWarnSet(MAV_palette *p, int v);
MAVAPI void mav_paletteTextureWarnSet(MAV_palette *p, int v);
MAVAPI void mav_paletteLightWarnSet(MAV_palette *p, int v);
MAVAPI void mav_paletteLightingModelWarnSet(MAV_palette *p, int v);
MAVAPI void mav_paletteFontWarnSet(MAV_palette *p, int v);
MAVAPI void mav_paletteColourIndexWarnSet(MAV_palette *p, int idx, int v);
MAVAPI void mav_paletteMaterialIndexWarnSet(MAV_palette *p, int idx, int v);
MAVAPI void mav_paletteTextureIndexWarnSet(MAV_palette *p, int idx, int v);
MAVAPI void mav_paletteLightIndexWarnSet(MAV_palette *p, int idx, int v);
MAVAPI void mav_paletteFontIndexWarnSet(MAV_palette *p, int idx, int v);

MAVAPI MAV_surfaceParams *mav_surfaceParamsNew(int mode, int colour, int material, int texture);
MAVAPI void mav_surfaceParamsUse(MAV_surfaceParams *sp);
MAVAPI void mav_surfaceParamsUndefine(void);
MAVAPI void mav_surfaceParamsPrint(char *s, MAV_surfaceParams sp);
MAVAPI int  mav_surfaceParamsIsTransparent(MAV_window *w, MAV_surfaceParams *sp);
MAVAPI int  mav_surfaceParamsIsTextured(MAV_window *w, MAV_surfaceParams *sp);
MAVAPI void mav_surfaceParamsFlagSet(int use_params);

/* Maths functions */

MAVAPI MAV_vector mav_vectorSet(float x, float y, float z);
MAVAPI MAV_vector mav_vectorNormalize(MAV_vector v);
MAVAPI MAV_vector mav_vectorAdd(MAV_vector v1, MAV_vector v2);
MAVAPI MAV_vector mav_vectorSub(MAV_vector v1, MAV_vector v2);
MAVAPI MAV_vector mav_vectorScalar(MAV_vector v1, float f);
MAVAPI float mav_vectorMag(MAV_vector v1);
MAVAPI float mav_vectorDotProduct(MAV_vector v1, MAV_vector v2);
MAVAPI MAV_vector mav_vectorCrossProduct(MAV_vector v1, MAV_vector v2);
MAVAPI MAV_vector mav_vectorRotate(MAV_vector v, MAV_vector ax, float ang);
MAVAPI MAV_vector mav_vectorMult(MAV_vector v, MAV_matrix m);
MAVAPI MAV_vector mav_vectorMult3x3(MAV_vector v, MAV_matrix m);
MAVAPI MAV_vector mav_vectorMult4x4(MAV_vector v, MAV_matrix m);
MAVAPI MAV_vector mav_vectorScrnPos(MAV_vector v);
MAVAPI MAV_vector mav_vectorWorldPos(MAV_vector v);
MAVAPI void mav_vectorPrint(char *s, MAV_vector v);

MAVAPI MAV_matrix mav_matrixSet(float roll, float pitch, float yaw, float x, float y, float z);
MAVAPI MAV_matrix mav_matrixSetOld(float roll, float pitch, float yaw, float x, float y, float z);
MAVAPI MAV_matrix mav_matrixXYZSet(MAV_matrix m, MAV_vector v);
MAVAPI MAV_matrix mav_matrixRPYSet(MAV_matrix m, float roll, float pitch, float yaw);
MAVAPI MAV_vector mav_matrixXYZGet(MAV_matrix m);
MAVAPI void mav_matrixRPYGet(MAV_matrix m, float *r, float *p, float *y);
MAVAPI MAV_matrix mav_matrixScaleSet(MAV_matrix m, float sc);
MAVAPI float mav_matrixScaleGet(MAV_matrix m);
MAVAPI MAV_vector mav_matrixXAxisGet(MAV_matrix m);
MAVAPI MAV_vector mav_matrixYAxisGet(MAV_matrix m);
MAVAPI MAV_vector mav_matrixZAxisGet(MAV_matrix m);
MAVAPI MAV_matrix mav_matrixXAxisSet(MAV_matrix m, MAV_vector v);
MAVAPI MAV_matrix mav_matrixYAxisSet(MAV_matrix m, MAV_vector v);
MAVAPI MAV_matrix mav_matrixZAxisSet(MAV_matrix m, MAV_vector v);
MAVAPI MAV_matrix mav_matrixMult(MAV_matrix m1, MAV_matrix m2);
MAVAPI MAV_matrix mav_matrixInverse(MAV_matrix m);
MAVAPI MAV_matrix mav_matrixQuaternionConvert(MAV_quaternion q);
MAVAPI void mav_matrixPrint(char *s, MAV_matrix m);

MAVAPI MAV_quaternion mav_quaternionSet(MAV_vector ax, float ang);
MAVAPI MAV_quaternion mav_quaternionMatrixConvert(MAV_matrix m);
MAVAPI MAV_quaternion mav_quaternionInterpolate(MAV_quaternion st, MAV_quaternion fi, float val);
MAVAPI void mav_quaternionPrint(char *s, MAV_quaternion q);

MAVAPI void mav_texCoordPrint(char *s, MAV_texCoord t);

MAVAPI MAV_time mav_timeGet(void);
MAVAPI void mav_timePrint(char *s, MAV_time t);
MAVAPI void mav_timerStart(MAV_timer *t);
MAVAPI void mav_timerStop(MAV_timer *t);
MAVAPI void mav_timerPrint(char *s, MAV_timer t);

MAVAPI float mav_random(void);
MAVAPI void mav_randomSeed(long seed);

MAVAPI char *mav_getTempDir(void);
MAVAPI int mav_getPID(void);

MAVAPI void mav_matrixStackPush(MAV_matrix m);
MAVAPI MAV_matrix mav_matrixStackGet(void);
MAVAPI void mav_matrixStackPop(void);

/* Devices */

typedef void (*MAV_devicePollFn)(void);
typedef void (*MAV_deviceCalcFn)(void);
typedef int  (*MAV_deviceEventFn)(void);

MAVAPI void mav_deviceNew(MAV_devicePollFn dpfn, MAV_deviceCalcFn dcfn, MAV_deviceEventFn defn);
MAVAPI void mav_devicePoll(void);
MAVAPI void mav_deviceCalc(void);
MAVAPI int  mav_eventsCheck(void);

/* Modules */

typedef char *(*MAV_moduleIDFn)(void);
typedef int (*MAV_moduleInitFn)(void);
MAVAPI void mav_moduleNew(MAV_moduleIDFn fn);
MAVAPI void mav_moduleDump(void);

/* Misc */

MAVAPI void mav_initialise(int *argc, char *argv[]);
MAVAPI void mav_initialiseNoArgs(void);
MAVAPI char *mav_kernelID(void);
MAVAPI void *mav_malloc(int amount);
MAVAPI void *mav_calloc(int nelem, int elemsize);
MAVAPI void mav_free(void *d);
MAVAPI void mav_windowDump(MAV_window *w, char *filename);
MAVAPI void mav_frustumDisplay(MAV_window *w, MAV_window *f);
MAVAPI void mav_sleep(float len);



/* Kernel defined callbacks */

/* Delete an object */

MAVAPI extern MAV_callback *mav_callback_delete;
typedef int (*MAV_callbackDeleteFn)(MAV_object *);
MAVAPI void mav_callbackDeleteSet(MAV_window *w, MAV_class *c, MAV_callbackDeleteFn fn);
MAVAPI int  mav_callbackDeleteExec(MAV_window *w, MAV_object *o);

/* Delete an SMS */

MAVAPI extern MAV_SMSCallback *mav_SMSCallback_delete;
typedef int (*MAV_SMSCallbackDeleteFn)(MAV_SMS *, int *);
MAVAPI void mav_SMSCallbackDeleteSet(MAV_SMSClass *sc, MAV_SMSCallbackDeleteFn fn);
MAVAPI int mav_SMSCallbackDeleteExec(MAV_SMS *s, int o);

/* Remove object from SMS */

MAVAPI extern MAV_SMSCallback *mav_SMSCallback_objectRmv;
typedef int (*MAV_SMSCallbackObjectRmvFn)(MAV_SMS *, MAV_object *);
MAVAPI void mav_SMSCallbackObjectRmvSet(MAV_SMSClass *sc, MAV_SMSCallbackObjectRmvFn fn);
MAVAPI int mav_SMSCallbackObjectRmvExec(MAV_SMS *s, MAV_object *o);
MAVAPI int mav_SMSObjectRmv(MAV_SMS *s, MAV_object *o);



/* Global variables */

MAVAPI extern MAV_vector MAV_NULL_VECTOR;
MAVAPI extern MAV_vector MAV_X_VECTOR;
MAVAPI extern MAV_vector MAV_Y_VECTOR;
MAVAPI extern MAV_vector MAV_Z_VECTOR;
MAVAPI extern MAV_matrix MAV_ID_MATRIX;
MAVAPI extern MAV_quaternion MAV_ID_QUATERNION;

MAVAPI extern MAV_list *mav_win_list;
MAVAPI extern MAV_list *mav_sms_list;
MAVAPI extern MAV_list *mav_module_list;
MAVAPI extern MAV_list *mav_palette_list;
MAVAPI extern MAV_list *mav_object_list;

MAVAPI extern MAV_window *mav_win_all;
MAVAPI extern MAV_window *mav_win_orig;
MAVAPI extern MAV_palette *mav_palette_default;
MAVAPI extern MAV_viewParams mav_vp_default;
MAVAPI extern MAV_class *mav_class_all;

MAVAPI extern MAV_window *mav_win_current;
MAVAPI extern MAV_surfaceParams *mav_sp_current;
MAVAPI extern MAV_surfaceParams *mav_sp_default;

MAVAPI extern int mav_opt_output;
MAVAPI extern int mav_opt_objectTables;
MAVAPI extern int mav_opt_WMPlacement;
MAVAPI extern int mav_opt_singleBuf; 
MAVAPI extern int mav_opt_quadBuf; 
MAVAPI extern int mav_opt_multiSample; 
MAVAPI extern int mav_opt_accumBuf;
MAVAPI extern int mav_opt_stencilBuf;
MAVAPI extern int mav_opt_destAlpha;
MAVAPI extern int mav_opt_fixedRnd;
MAVAPI extern int mav_opt_shareContexts;
MAVAPI extern int mav_opt_bindTextures;
MAVAPI extern int mav_opt_syncSwap;
MAVAPI extern int mav_opt_finish;
MAVAPI extern int mav_opt_flush;

MAVAPI extern int mav_opt_maxColours;
MAVAPI extern int mav_opt_maxMaterials;
MAVAPI extern int mav_opt_maxTextures;
MAVAPI extern int mav_opt_maxFonts;
MAVAPI extern int mav_opt_maxLights;
MAVAPI extern int mav_opt_paletteWarn;
MAVAPI extern int mav_opt_mipmapping;
MAVAPI extern int mav_opt_objectTableSize;
MAVAPI extern int mav_opt_defaultInit;

MAVAPI extern int mav_argc;
MAVAPI extern char **mav_argv;
MAVAPI extern FILE *mav_userconf;

MAVAPI extern float mav_fps;
MAVAPI extern float mav_fps_avg;
MAVAPI extern int mav_firstFrame;
MAVAPI extern int mav_needFrameDraw;
MAVAPI extern int mav_mallocCount;
MAVAPI extern char mav_hostName[];
MAVAPI extern int mav_frameCount;
MAVAPI extern int mav_this_version;

MAVAPI extern MAV_frameFn mav_windowChgFn;

#ifdef __cplusplus
}
#endif
#endif
