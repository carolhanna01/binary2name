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

#include <stdio.h>
#include <stdlib.h>
#include "maverik.h"
#include "mav_escapeavatar.h"
#include "city_macros.h"
#include "city_types.h"

MAV_callback *mav_callback_enlist;
int mav_objectEnlist (MAV_object *obj, void *v1, void *v2);

int mav_cityCellGetMatrix (MAV_object *obj, MAV_matrix **mat);
int mav_cityCellGetUserdef (MAV_object *obj, void ***ud);
int mav_cityCellBBox (MAV_object *obj, MAV_BB *bb);
int mav_cityCellDraw (MAV_object *obj, MAV_drawInfo *di);

int mav_occluderGetMatrix (MAV_object *obj, MAV_matrix **mat);
int mav_occluderGetUserdef (MAV_object *obj, void ***ud);
int mav_occluderBBox (MAV_object *obj, MAV_BB *bb);
int mav_occluderDraw (MAV_object *obj, MAV_drawInfo *di);

int mav_billboardGetMatrix (MAV_object *obj, MAV_matrix **mat);
int mav_billboardGetUserdef (MAV_object *obj, void ***ud);
int mav_billboardBBox (MAV_object *obj, MAV_BB *bb);
int mav_billboardDraw (MAV_object *obj, MAV_drawInfo *di);

#ifdef COMPOSITES
int mav_compositeDrawWire (MAV_object *obj, MAV_drawInfo *di);
#endif

void Build_Cityscape (void);
void WalkAround (void);

extern MAV_list *list_of_objects;

MAV_class *mav_class_avatar;
MAV_class *mav_class_citycell;
MAV_class *mav_class_occluder;
MAV_class *mav_class_billboard;
MAV_SMS *city_sms;
int num_cells= 0;
int num_visible_objects= 0;
int routereader=0;
int routewriter=0;
Visible_Object *visible_objects;
int total_objects= 0;
MAV_composite composites[NUM_COMP];

int main(int argc, char **argv)
{
  MAV_matrix transform;
  MAV_object *obj;

#ifdef STEREO
  mav_opt_stereo= MAV_STEREO_TWO_WINS;
  mav_opt_width=1024;
  mav_opt_height=768;
  mav_opt_right_width=1024;
  mav_opt_right_height=768;
#endif

  mav_opt_fixedRnd= MAV_TRUE;
  mav_opt_bindTextures= MAV_TRUE;
  mav_initialise(&argc, argv);

  if (argc==2) {
    int v= atoi(argv[1]);
    if (v==1) routereader=1;
    if (v==2) routewriter=1;
  }


  /* create the enlist callback */
  mav_callback_enlist= mav_callbackNew();

  /* initialise classes */
  mav_class_avatar= mav_classNew();
  mav_callbackDrawSet (mav_win_all, mav_class_avatar, mav_avatarDraw);
  mav_callbackBBSet (mav_win_all, mav_class_avatar, mav_avatarBBox);
  mav_callbackGetMatrixSet (mav_win_all, mav_class_avatar, mav_avatarGetMatrix);
  mav_callbackGetUserdefSet (mav_win_all, mav_class_avatar, mav_avatarGetUserdef);  
  mav_callbackSet (mav_callback_enlist, mav_win_all, mav_class_avatar, mav_objectEnlist);

  mav_class_citycell= mav_classNew();
  mav_callbackDrawSet (mav_win_all, mav_class_citycell, mav_cityCellDraw);
  mav_callbackBBSet (mav_win_all, mav_class_citycell, mav_cityCellBBox);
  mav_callbackGetMatrixSet (mav_win_all, mav_class_citycell, mav_cityCellGetMatrix);
  mav_callbackGetUserdefSet (mav_win_all, mav_class_citycell, mav_cityCellGetUserdef);
  mav_callbackSet (mav_callback_enlist, mav_win_all, mav_class_citycell, mav_objectEnlist);

  mav_class_occluder= mav_classNew();
  mav_callbackDrawSet (mav_win_all, mav_class_occluder, mav_occluderDraw);
  mav_callbackBBSet (mav_win_all, mav_class_occluder, mav_occluderBBox);
  mav_callbackGetMatrixSet (mav_win_all, mav_class_occluder, mav_occluderGetMatrix);
  mav_callbackGetUserdefSet (mav_win_all, mav_class_occluder, mav_occluderGetUserdef);
  mav_callbackSet (mav_callback_enlist, mav_win_all, mav_class_occluder, mav_objectEnlist);

  mav_class_billboard= mav_classNew();
  mav_callbackDrawSet (mav_win_all, mav_class_billboard, mav_billboardDraw);
  mav_callbackBBSet (mav_win_all, mav_class_billboard, mav_billboardBBox);
  mav_callbackGetMatrixSet (mav_win_all, mav_class_billboard, mav_billboardGetMatrix);
  mav_callbackGetUserdefSet (mav_win_all, mav_class_billboard, mav_billboardGetUserdef);
  mav_callbackSet (mav_callback_enlist, mav_win_all, mav_class_billboard, mav_objectEnlist);

#ifdef COMPOSITES
  mav_callbackDrawSet (mav_win_all, mav_class_composite, mav_compositeDrawWire);
  mav_callbackSet (mav_callback_enlist, mav_win_all, mav_class_composite, mav_objectEnlist);
#endif

  /* materials and colours */
  mav_paletteMaterialSet (mav_palette_default, MAT_DARK_BLUE, 0.0,0.0,0.3,0.0, 0.0,0.0,0.7,0.0, 0.2,0.2,0.4,0.0, 0.0,0.0,0.0,0.0, 30.0);
  mav_paletteMaterialSet (mav_palette_default, MAT_GRASS, 0.0,0.3,0.0,0.0, 0.0,0.3,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
  mav_paletteMaterialSet (mav_palette_default, MAT_WHITE, 0.7,0.7,0.7,0.0, 1.0,1.0,1.0,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
  mav_paletteMaterialSet (mav_palette_default, MAT_WHITE2, 0.7,0.65,0.65,0.0, 1.0,0.95,0.95,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
  mav_paletteMaterialSet (mav_palette_default, MAT_WHITE3, 0.65,0.65,0.7,0.0, 0.95,0.95,1.0,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
#ifndef TEXTURES
  mav_paletteMaterialSet (mav_palette_default, MAT_ASPHALT, 0.1,0.1,0.1,0.0, 0.2,0.2,0.2,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
#endif
  mav_paletteMaterialSet (mav_palette_default, MAT_PAVEMENT, 0.4,0.4,0.4,0.0, 1.0,1.0,1.0,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
  mav_paletteMaterialSet (mav_palette_default, MAT_MARK, 0.4,0.4,0.4,0.0, 1.0,1.0,1.0,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
  mav_paletteMaterialSet (mav_palette_default, MAT_DIRT, 0.125,0.105,0.025,0.0, 0.25,0.21,0.05,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
  mav_paletteMaterialSet (mav_palette_default, MAT_METAL, 0.2,0.2,0.2,0.0, 0.7,0.7,0.7,0.0, 1.0,1.0,1.0,0.0, 0.0,0.0,0.0,0.0, 30.0);
  mav_paletteMaterialSet (mav_palette_default, MAT_CONCRETE, 0.2,0.2,0.2,0.0, 0.7,0.7,0.7,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
  mav_paletteMaterialSet (mav_palette_default, MAT_GLASS, 0.2,0.2,0.2,0.0, 0.9,0.9,1.0,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
  mav_paletteMaterialSet (mav_palette_default, MAT_STRUTS, 0.1,0.1,0.1,0.0, 0.35,0.27,0.27,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
#ifndef TEXTURES
  mav_paletteMaterialSet (mav_palette_default, MAT_NOTEX, 0.35,0.35,0.35,0.0, 0.6,0.6,0.6,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
  mav_paletteMaterialSet (mav_palette_default, MAT_NOTEX2, 0.35,0.3,0.3,0.0, 0.6,0.5,0.5,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
  mav_paletteMaterialSet (mav_palette_default, MAT_NOTEX3, 0.3,0.3,0.35,0.0, 0.5,0.5,0.6,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
#endif
  mav_paletteMaterialSet (mav_palette_default, MAT_TROUSERS1, 0.094,0.0,0.0,0.0, 0.529,0.282,0.043,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
  mav_paletteMaterialSet (mav_palette_default, MAT_TROUSERS2, 0.0,0.094,0.0,0.0, 0.282,0.529,0.043,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
  mav_paletteMaterialSet (mav_palette_default, MAT_TROUSERS3, 0.0,0.0,0.094,0.0, 0.043,0.282,0.529,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
  mav_paletteMaterialSet (mav_palette_default, MAT_JUMPER1, 0.408,0.071,0.063,0.0, 0.878,0.753,0.627,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
  mav_paletteMaterialSet (mav_palette_default, MAT_JUMPER2, 0.071,0.408,0.063,0.0, 0.753,0.878,0.627,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);
  mav_paletteMaterialSet (mav_palette_default, MAT_JUMPER3, 0.063,0.071,0.408,0.0, 0.627,0.753,0.878,0.0, 0.0,0.0,0.0,0.0, 0.0,0.0,0.0,0.0, 0.0);

  mav_paletteColourSet (mav_palette_default, MAV_COLOUR_YELLOW, 1.0,1.0,0.0,0.0);

  /* trees */
#ifdef TEXTURES
  mav_paletteTextureSet (mav_palette_default, TEX_TREE1, "textures/tree.ppm");
  mav_paletteTextureColourAlphaSet (mav_palette_default, TEX_TREE1, 0, 0, 0, 0.0);
#endif
  mav_paletteTextureSet (mav_palette_default, TEX_TREE3, "textures/tree3.ppm");
  mav_paletteTextureColourAlphaSet (mav_palette_default, TEX_TREE3, 0, 0, 0, 0.0);

  /* sky */
  mav_paletteTextureSet (mav_palette_default, TEX_SKY, "textures/sky.ppm");

  /* misc textures */
  mav_paletteTextureSet (mav_palette_default, TEX_PAVEMENT, "textures/pavement.ppm");
#ifdef TEXTURES
  mav_paletteTextureSet (mav_palette_default, TEX_ASPHALT, "textures/asphalt.ppm");
  mav_paletteTextureSet (mav_palette_default, TEX_DIRT, "textures/dirt.ppm");
  mav_paletteTextureSet (mav_palette_default, TEX_GRASS, "textures/grass.ppm");
  mav_paletteTextureSet (mav_palette_default, TEX_BUILDING_SIDE1, "textures/building_side1.ppm");
  mav_paletteTextureSet (mav_palette_default, TEX_BUILDING_SIDE2, "textures/building_side2.ppm");
  mav_paletteTextureSet (mav_palette_default, TEX_BUILDING_SIDE3, "textures/building_side3.ppm");
  mav_paletteTextureSet (mav_palette_default, TEX_BUILDING_SIDE4, "textures/building_side4.ppm");
  mav_paletteTextureSet (mav_palette_default, TEX_BUILDING_SIDE5, "textures/building_side5.ppm");
  mav_paletteTextureSet (mav_palette_default, TEX_BUILDING_SIDE6, "textures/building_side6.ppm");
  mav_paletteTextureSet (mav_palette_default, TEX_BUILDING_SIDE7, "textures/building_side7.ppm");
  mav_paletteTextureSet (mav_palette_default, TEX_BUILDING_TOP, "textures/building_top.ppm");
  mav_paletteTextureSet (mav_palette_default, TEX_WINDOW, "textures/window.ppm");
  mav_paletteTextureSet (mav_palette_default, TEX_WINDOW_TOP, "textures/window_top.ppm");
  mav_paletteTextureSet (mav_palette_default, TEX_WINDOW_ROOF, "textures/window_roof.ppm");
  mav_paletteTextureSet (mav_palette_default, TEX_WINDOW2, "textures/window2.ppm");
  mav_paletteTextureSet (mav_palette_default, TEX_JUMPER1, "textures/jumper1.ppm");
  mav_paletteTextureSet (mav_palette_default, TEX_JUMPER2, "textures/jumper2.ppm");
  mav_paletteTextureSet (mav_palette_default, TEX_TROUSERS1, "textures/trousers1.ppm");
  mav_paletteTextureSet (mav_palette_default, TEX_TROUSERS2, "textures/trousers2.ppm");
#endif

#ifdef COMPOSITES
  transform= mav_matrixSet (0.0,0.0,0.0, -1.75,1.785921+PAVE_HEIGHT,0.199917);
  mav_compositeReadAC3D ("objects/trafficlight.ac", &composites[COMP_TRAFFICLIGHT], transform);
  transform= mav_matrixSet (0.0,0.0,0.0, -1.1054405,1.785921+PAVE_HEIGHT,0.0);
  mav_compositeReadAC3D ("objects/streetlight.ac", &composites[COMP_STREETLIGHT], transform);
  transform= mav_matrixSet (0.0,0.0,0.0, 2.0,1.1,0.0);
  /*
  mav_compositeReadAC3D ("objects/fence.ac", &composites[COMP_FENCE], transform);
  transform= mav_matrixSet (0.0,0.0,0.0, 0.0,1.002063,0.0);
  */
  mav_compositeReadAC3D ("objects/fountain.ac", &composites[COMP_FOUNTAIN], transform);
  transform= mav_matrixSet (0.0,0.0,0.0, 0.0,1.002063,0.0);
  mav_compositeReadAC3D ("objects/red_fountain.ac", &composites[COMP_RED_FOUNTAIN], transform);
  transform= mav_matrixSet (0.0,0.0,0.0, 0.0,1.1,0.0);
  mav_compositeReadAC3D ("objects/door.ac", &composites[COMP_DOOR], transform);
  transform= mav_matrixSet (0.0,0.0,0.0, 0.0,2.0,0.0);
  mav_compositeReadAC3D ("objects/easter.ac", &composites[COMP_EASTER], transform);
  transform= mav_matrixSet (0.0,0.0,0.0, 0.0,2.25,0.0);
  mav_compositeReadAC3D ("objects/gazebo.ac", &composites[COMP_GAZEBO], transform);
  transform= mav_matrixSet (0.0,0.0,0.0, 0.0,1.5,0.0);
  mav_compositeReadAC3D ("objects/sthenge.ac", &composites[COMP_STHENGE], transform);
  transform= mav_matrixSet (0.0,0.0,0.0, 0.0,0.599799,0.0);
  mav_compositeReadAC3D ("objects/legoman.ac", &composites[COMP_LEGOMAN], transform);
  transform= mav_matrixSet (0.0,0.0,0.0, 0.0,2.760199,0.0);
  mav_compositeReadAC3D ("objects/recognizer.ac", &composites[COMP_RECOGNIZER], transform);
  transform= mav_matrixSet (0.0,0.0,0.0, 0.0,0.191143,0.0);
  mav_compositeReadAC3D ("objects/fish.ac", &composites[COMP_FISH], transform);
  transform= mav_matrixSet (0.0,0.0,0.0, 0.0,0.9,0.0);
  mav_compositeReadAC3D ("objects/standing_man.ac", &composites[COMP_STANDINGMAN], transform);
  transform= mav_matrixSet (0.0,0.0,0.0, 0.0,0.822622,0.0);
  mav_compositeReadAC3D ("objects/woman.ac", &composites[COMP_WOMAN], transform);
  transform= mav_matrixSet (0.0,0.0,0.0, 0.0,2.5,0.0);
  mav_compositeReadAC3D ("objects/carosel.ac", &composites[COMP_CAROSEL], transform);
#endif

  Build_Cityscape ();

  visible_objects= mav_malloc(total_objects*sizeof(Visible_Object));

  /* build sms */
  city_sms= mav_SMSHBBNew ();

  mav_listPointerReset (list_of_objects);
  while (mav_listItemNext (list_of_objects, (void **)&obj))
    mav_SMSObjectAdd (city_sms, obj);

  WalkAround ();

  return 1;
}
