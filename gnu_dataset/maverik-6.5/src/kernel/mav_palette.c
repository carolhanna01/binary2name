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


#include "mavlib_kernel.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#if (!defined(WIN32) || defined(__CYGWIN__)) && !defined(macintosh)
#include <sys/types.h>
#include <unistd.h>
#endif


#if !defined(WIN32) || defined(__CYGWIN__)
char *strdup (const char *s1);
#endif

MAV_surfaceParams mavlib_surfaceParams={-1,-1,-1,-1};
MAV_surfaceParams *mav_sp_current= &mavlib_surfaceParams;
MAV_colour mavlib_colBlack={1,1,1,{0,0,0,1}};
MAV_colour mavlib_colWhite={1,1,1,{1,1,1,1}};
MAV_colour mavlib_colRed={1,1,1,{1,0,0,1}};
MAV_colour mavlib_colGreen={1,1,1,{0,1,0,1}};
MAV_colour mavlib_colBlue={1,1,1,{0,0,1,1}};

int mavlib_num_palettes= 0;
int mavlib_use_surface_params= 1;

int mav_opt_maxColours=150;
int mav_opt_maxMaterials=150;
int mav_opt_maxTextures=150;
int mav_opt_maxFonts=10;
int mav_opt_maxLights=5;
int mav_opt_paletteWarn= MAV_TRUE;
int mav_opt_mipmapping=0;

extern int mavlib_voodoo;
void mavlib_paletteDefaultValues(MAV_palette *p);



/* Routine to enable or disable surface params */

void mav_surfaceParamsFlagSet(int mode)
{
  mavlib_use_surface_params= mode;
}


/* Routine to create a new set of surface params */

MAV_surfaceParams *mav_surfaceParamsNew(int mode, int colour, int material, int texture)
{
  MAV_surfaceParams *rv= mav_malloc(sizeof(MAV_surfaceParams));
  
  rv->mode=mode;
  rv->colour=colour;
  rv->material=material;
  rv->texture=texture;

  return rv;
}



/* Routine to undefine the current used surface params */

void mav_surfaceParamsUndefine(void)
{
  mavlib_surfaceParams.mode=-1;
  mavlib_surfaceParams.colour=-1;
  mavlib_surfaceParams.material=-1;
  mavlib_surfaceParams.texture=-1;
}



/* Routine to use a set of surface params */

void mav_surfaceParamsUse(MAV_surfaceParams *sp)
{
  if (mavlib_use_surface_params) {
    /* return if sp are NULL (keep using current sp's) */
    if (sp==NULL) return;

    /* return if we are already using this set of sp's */
    if (sp->mode==mavlib_surfaceParams.mode && sp->colour==mavlib_surfaceParams.colour &&
        sp->material==mavlib_surfaceParams.material && sp->texture==mavlib_surfaceParams.texture) return;

    /* set to correct mode if we are not already using it */
    if (sp->mode!=mavlib_surfaceParams.mode) {
      mav_gfxColouringModeUse(mav_win_current->palette, sp->mode);
      mavlib_surfaceParams.colour=-1;
      mavlib_surfaceParams.material=-1;
      mavlib_surfaceParams.texture=-1;
    }    

    /* set to correct colour/mat/texture if we are not already using it */
    switch (sp->mode) {
    case MAV_COLOUR:
      if (sp->colour!=mavlib_surfaceParams.colour) {
        if (sp->colour<0) { /* build in colours */
  	  switch (sp->colour) {
  	  case MAV_COLOUR_BLACK:
  	    mav_gfxColourUse(mavlib_colBlack);
  	    break;
  	  case MAV_COLOUR_WHITE:
	    mav_gfxColourUse(mavlib_colWhite);
	    break;
	  case MAV_COLOUR_RED:
	    mav_gfxColourUse(mavlib_colRed);
	    break;
	  case MAV_COLOUR_GREEN:
	    mav_gfxColourUse(mavlib_colGreen);
	    break;
	  case MAV_COLOUR_BLUE:
	    mav_gfxColourUse(mavlib_colBlue);
	    break;
	  }
        }
        else
        {
	  mav_gfxColourUse(mav_win_current->palette->collist[sp->colour]);
        }
      }
      break;

    case MAV_MATERIAL:
      if (sp->material!=mavlib_surfaceParams.material) mav_gfxMaterialUse(mav_win_current->palette->matlist[sp->material]);
      break;

    case MAV_TEXTURE:
      if (sp->texture!=mavlib_surfaceParams.texture) mav_gfxTextureUse(mav_win_current->palette->texlist[sp->texture], mav_win_current->palette->texEnv);
      mav_gfxTextureEnv2Set(1);
      mav_gfxColourUse(mavlib_colWhite); /* needed since above is modulated against current fragment colour */
      break;

    case MAV_LIT_TEXTURE:
      if (sp->texture!=mavlib_surfaceParams.texture) mav_gfxTextureUse(mav_win_current->palette->texlist[sp->texture], mav_win_current->palette->texEnv);
      mav_gfxTextureEnv2Set(1);
      if (sp->material!=mavlib_surfaceParams.material) mav_gfxMaterialUse(mav_win_current->palette->matlist[sp->material]);
      break;

    case MAV_BLENDED_TEXTURE:
      if (sp->texture!=mavlib_surfaceParams.texture) mav_gfxTextureUse(mav_win_current->palette->texlist[sp->texture], mav_win_current->palette->texEnv);
      mav_gfxTextureEnv2Set(2);
      if (sp->material!=mavlib_surfaceParams.material) mav_gfxMaterialUse(mav_win_current->palette->matlist[sp->material]);
      break;
    }

    mavlib_surfaceParams= *sp;
  }
}


/* Routine to create a new palette */

MAV_palette *mav_paletteNew(void)
{
  MAV_palette *rv= (MAV_palette *) mav_malloc(sizeof(MAV_palette));
  int i;

  /* Add to list of palettes */
  mav_listItemAdd(mav_palette_list, (void *) rv);

  /* Warn on redefinition of contents */
  rv->defwarn= MAV_TRUE;
  rv->lm_defwarn= MAV_TRUE;
  rv->light_defwarn= MAV_TRUE;
  rv->col_defwarn= MAV_TRUE;
  rv->mat_defwarn= MAV_TRUE;
  rv->tex_defwarn= MAV_TRUE;
  rv->font_defwarn= MAV_TRUE;

 /* 
     Clear colour, material, texture and light defined lists 
     Give each a unique ID to define and bind against.
 */
  
  rv->collist= mav_malloc(mav_opt_maxColours*sizeof(MAV_colour));
  for (i=0; i<mav_opt_maxColours; i++) {
    rv->collist[i].id= (mavlib_num_palettes*mav_opt_maxColours)+i;
    rv->collist[i].defwarn=MAV_TRUE;
    rv->collist[i].defined=0;
  }

  rv->matlist= mav_malloc(mav_opt_maxMaterials*sizeof(MAV_material));
  for (i=0; i<mav_opt_maxMaterials; i++) {
    rv->matlist[i].id= (mavlib_num_palettes*mav_opt_maxMaterials)+i;
    rv->matlist[i].defwarn=MAV_TRUE;
    rv->matlist[i].defined=0;
  }

  rv->texlist= mav_malloc(mav_opt_maxTextures*sizeof(MAV_texture));
  for (i=0; i<mav_opt_maxTextures; i++) {
    rv->texlist[i].id= (mavlib_num_palettes*mav_opt_maxTextures)+i;
    rv->texlist[i].defwarn=MAV_TRUE;
    rv->texlist[i].defined=0;
    rv->texlist[i].transparent=0;
    rv->texlist[i].mipmapped=mav_opt_mipmapping;
    rv->texlist[i].nmaps=0;
    rv->texlist[i].mipmap=NULL;
  }

  rv->lm.id= mavlib_num_palettes;
  rv->lm.defined=0;

  rv->lightlist= mav_malloc(mav_opt_maxLights*sizeof(MAV_light));
  for (i=0; i<mav_opt_maxLights; i++) {
    rv->lightlist[i].id= (mavlib_num_palettes*mav_opt_maxLights)+i;
    rv->lightlist[i].index=i;
    rv->lightlist[i].defwarn=MAV_TRUE;
    rv->lightlist[i].defined=0;
    rv->lightlist[i].positioning= MAV_LIGHT_RELATIVE;
  }

  rv->fontlist= mav_malloc(mav_opt_maxFonts*sizeof(MAV_font));
  for (i=0; i<mav_opt_maxFonts; i++) {
    rv->fontlist[i].id= (mavlib_num_palettes*mav_opt_maxFonts)+i;
    rv->fontlist[i].defwarn=MAV_TRUE;
    rv->fontlist[i].defined=0;
  }

  mav_paletteTextureEnvPaletteSet(rv, mav_texEnvDefault);

  mavlib_num_palettes++;

  mavlib_paletteDefaultValues(rv);

  return rv;
}



/* 
   Routines to call when a light is updated so as the change effects all windows
   which share this palette.
*/

void mavlib_lightUpd(int i, MAV_palette *p)
{
  MAV_window *orig_win= mav_win_current;
  MAV_window *win;

  mav_listPointerReset(mav_win_list);

  while (mav_listItemNext(mav_win_list, (void **) &win)) {
    /* Set in each window which shares this palette */
    if (win->palette==p) {
      if (win!=mav_win_current) mav_windowSet(win);
      mav_gfxLightUse(p->lightlist[i]);
    }
  }

  if (mav_win_current!=orig_win) mav_windowSet(orig_win);

  mav_surfaceParamsUndefine();
}

void mavlib_lightPosUpd(int i, MAV_palette *p)
{
  MAV_window *orig_win= mav_win_current;
  MAV_window *win;

  mav_listPointerReset(mav_win_list);

  while (mav_listItemNext(mav_win_list, (void **) &win)) {
    if (win->palette==p) {
      if (win!=mav_win_current) mav_windowSet(win);
      mav_gfxLightPos(p->lightlist[i]);
    }
  }

  if (mav_win_current!=orig_win) mav_windowSet(orig_win);

  mav_surfaceParamsUndefine();
}

void mavlib_lightingModelUpd(MAV_palette *p)
{
  MAV_window *orig_win= mav_win_current;
  MAV_window *win;
  
  mav_listPointerReset(mav_win_list);
  
  while (mav_listItemNext(mav_win_list, (void **) &win)) {
    if (win->palette==p) {
      if (win!=mav_win_current) mav_windowSet(win);
      mav_gfxLightingModelUse(p->lm);
    }
  }

  if (mav_win_current!=orig_win) mav_windowSet(orig_win);

  mav_surfaceParamsUndefine();
}



/* Routine to handle absolute lights */

void mavlib_lightPosFix(void)
{
  MAV_palette *p;
  int i;

  mav_listPointerReset(mav_palette_list);
  
  while (mav_listItemNext(mav_palette_list, (void **) &p)) {
    for (i=0; i<mav_opt_maxLights; i++) {
      if (p->lightlist[i].defined && p->lightlist[i].positioning==MAV_LIGHT_ABSOLUTE) mavlib_lightPosUpd(i,p);
    }
  }
}




/* Routine to set the palette for a given window */

void mav_windowPaletteSet(MAV_window *w, MAV_palette *p)
{
  MAV_window *orig_win= mav_win_current;
  int i;

  w->palette= p;
  if (w==mav_win_all) return;

  /* set to requested window and enable lighting model and lights */
  
  if (orig_win!=w) mav_windowSet(w);  

  mav_gfxLightingModelUse(p->lm);
  for (i=0; i<mav_opt_maxLights; i++) {
    mav_gfxLightUse(p->lightlist[i]);
    mav_gfxLightPos(p->lightlist[i]);
  }

  if (orig_win!=w) mav_windowSet(orig_win);

  mav_surfaceParamsUndefine();  
}



/* Routine to define a palette's texture environment */

void mav_paletteTextureEnvPaletteSet(MAV_palette *p, MAV_texEnvFn fn)
{
  p->texEnv= fn;
}



/* Routine to define an emmisive colour in a palette */

void mav_paletteColourSet(MAV_palette *p, int index, float r, float g, float b, float a)
{
/* Check index is within range */

  if (index > mav_opt_maxColours-1) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Colour index %i too big (max %i), ignoring\n", index, mav_opt_maxColours-1);
  }
  else
  {

/* Warn if index is already defined, then define it */

    if (mav_opt_paletteWarn && p->defwarn && p->col_defwarn && p->collist[index].defwarn && p->collist[index].defined==MAV_REDEFINE_WARN && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Colour index %i already defined in palette, overwriting\n", index);
    
    p->collist[index].defined = MAV_REDEFINE_WARN;
    
    p->collist[index].colour[0] = r;
    p->collist[index].colour[1] = g;
    p->collist[index].colour[2] = b;
    p->collist[index].colour[3] = a;
    
    mav_gfxColourSet(p->collist[index]);
    mav_surfaceParamsUndefine();
  }
}



/* Routine to define a material in a palette */

void mav_paletteMaterialSet(MAV_palette *p, int index, float a1, float a2, float a3, float a4,
                                                float d1, float d2, float d3, float d4, 
		                                float s1, float s2, float s3, float s4, 
		                                float e1, float e2, float e3, float e4, float shine)
{
/* Check index is within range */

  if (index > mav_opt_maxMaterials-1) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Material index %i too big (max %i), ignoring\n", index, mav_opt_maxMaterials-1);
  }
  else
  {

/* Warn if index is already defined, then define it */

    if (mav_opt_paletteWarn && p->defwarn && p->mat_defwarn && p->matlist[index].defwarn && p->matlist[index].defined==MAV_REDEFINE_WARN && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Material index %i already defined in palette, overwriting\n", index);
    
    p->matlist[index].defined = MAV_REDEFINE_WARN;
      
    p->matlist[index].ambient[0] = a1;
    p->matlist[index].ambient[1] = a2;
    p->matlist[index].ambient[2] = a3;
    p->matlist[index].ambient[3] = a4;
    
    p->matlist[index].diffuse[0] = d1;
    p->matlist[index].diffuse[1] = d2; 
    p->matlist[index].diffuse[2] = d3;
    p->matlist[index].diffuse[3] = d4;
    
    p->matlist[index].specular[0] = s1;
    p->matlist[index].specular[1] = s2;
    p->matlist[index].specular[2] = s3;
    p->matlist[index].specular[3] = s4;
    
    p->matlist[index].emission[0] = e1;
    p->matlist[index].emission[1] = e2;
    p->matlist[index].emission[2] = e3;
    p->matlist[index].emission[3] = e4;
    
    p->matlist[index].shine=shine;
    
    mav_gfxMaterialSet(p->matlist[index]);
    mav_surfaceParamsUndefine();
  }
}



/* Routine to read a texture from a raw or ASCII PPM file */

int mavlib_readPPM(char *filename, int *width, int *height, uint32_t **mem)
{
  FILE *file;
  float rf, gf, bf;
  int r, g, b, i, j, mv;
  char str[100];
  unsigned char *c;
  uint32_t *map;
  int bin=0;

/* Open PPM file */

  file=fopen(filename, "rb");
  if (!file) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: can not read texture file %s, ignoring\n", filename);
    return(MAV_FALSE);
  }

/* Check magic number */
      
  fscanf(file, "%s", str);

  if (strcmp(str, "P3") && strcmp(str, "P6")) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: bad magic number %s (expecting P3 or P6), ignoring\n", str);
    return(MAV_FALSE);
  }
  
  if (!strcmp(str, "P6")) bin=1;

  fscanf(file, "%s", str);
  while (str[0]=='#') { /* comment - read to end of line */
#ifdef macintosh
    while (str[0]!='\r') fscanf(file, "%c", &str[0]);
#else
    while (str[0]!=10) fscanf(file, "%c", &str[0]);
#endif
    fscanf(file, "%s", str);
  }
  
  *width= atoi(str);
  
  fscanf(file, "%s", str);
  while (str[0]=='#') { /* comment - read to end of line */
#ifdef macintosh
    while (str[0]!='\r') fscanf(file, "%c", &str[0]);
#else
    while (str[0]!=10) fscanf(file, "%c", &str[0]);
#endif
    fscanf(file, "%s", str);
  }
  
  *height= atoi(str);

  fscanf(file, "%i", &mv);

  /* if binary, read until newline */
  if (bin) {
    do {
      fscanf(file, "%c", &str[0]);
    } while (str[0]!=10);
  }

/* Parse */
  
  *mem=(uint32_t *) mav_malloc(*width**height*sizeof(uint32_t));
  map=*mem;

  for (i=*height-1; i>=0; i--) {
    for (j=0; j<*width; j++) {	  

      if (bin) 
      {
	fread(str, 1, 3, file);
	r= (unsigned char) str[0];
	g= (unsigned char) str[1];
	b= (unsigned char) str[2];
      }
      else
      {
	fscanf(file, "%i %i %i", &r, &g, &b);
      }

      rf=((float) r)/mv;
      gf=((float) g)/mv;
      bf=((float) b)/mv;

      c= (unsigned char *) &map[i**width+j];
#ifdef WIN32
      /* Textures have RGBA ordering */
      c[0]= (unsigned char) (rf*255);
      c[1]= (unsigned char) (gf*255);
      c[2]= (unsigned char) (bf*255);
      c[3]= 255;
#else
      /* Textures have ABGR ordering */
      c[0]= 255;
      c[1]= (unsigned char) (bf*255);
      c[2]= (unsigned char) (gf*255);
      c[3]= (unsigned char) (rf*255);
#endif
    }
  }

  fclose(file);

  return(MAV_TRUE);
}



/* Routine to read a texture (with alpha channel) from a PNG file */

#ifdef MAV_PNG
#include <png.h>

#define PNG_BYTES_TO_CHECK 8
#define MAV_TRANSPARENT 2

int mavlib_readPNG(char *filename, int *width, int *height, uint32_t **mem)
{
  FILE *file;
  png_byte hdr[PNG_BYTES_TO_CHECK];
  png_structp png_ptr;
  png_infop info_ptr;
  int depth, colour_type;
  png_bytep *row_pointer;
  int row;

  /* open file and check it's in PNG format */

  file=fopen(filename, "rb");
  if (!file) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: cannot open texture file %s, ignoring\n", filename);
    return MAV_FALSE;
  }

  if (fread(hdr, 1, PNG_BYTES_TO_CHECK, file) != PNG_BYTES_TO_CHECK) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: cannot read texture file %s's header, ignoring\n", filename);
    fclose(file);
    return MAV_FALSE;
  }

  if (!png_check_sig(hdr, PNG_BYTES_TO_CHECK)) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: %s is not a PNG file, ignoring\n", filename);
    fclose(file);
    return MAV_FALSE;
  }

  /* initialise libpng structures */
  
  png_ptr= png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: cannot create png_struct for %s, ignoring\n", filename);
    fclose(file);
    return MAV_FALSE;
  }

  info_ptr= png_create_info_struct(png_ptr);
  if (!info_ptr) {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: cannot create png_info for %s, ignoring\n", filename);
    png_destroy_read_struct(&png_ptr, NULL, NULL);
    fclose(file);
    return MAV_FALSE;
  }

  /* set up error handling */
  /* libpng prints msgs, we just need to clean up and exit */
  if (setjmp(png_ptr->jmpbuf)) {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    fclose(file);
    return MAV_FALSE;
  }

  /* tell libpng which file we're reading from and how much we've already read */
  png_init_io(png_ptr, file);
  png_set_sig_bytes(png_ptr, PNG_BYTES_TO_CHECK);

  /* read file information */

  png_read_info(png_ptr, info_ptr);

  *width= (int)png_get_image_width(png_ptr, info_ptr);
  *height= (int)png_get_image_height(png_ptr, info_ptr);
  depth= png_get_bit_depth(png_ptr, info_ptr);
  colour_type= png_get_color_type(png_ptr, info_ptr);

  /* set up transformations so we get an 8-bit per channel, RGBA image */

  /* strip 16-bit channels down to 8 */
  if (depth == 16) png_set_strip_16(png_ptr);

  /* convert paletted to RGB */
  if (colour_type == PNG_COLOR_TYPE_PALETTE) png_set_expand(png_ptr);

  /* convert greyscale to RGB */
  if (colour_type == PNG_COLOR_TYPE_GRAY ||
      colour_type == PNG_COLOR_TYPE_GRAY_ALPHA) {
    png_set_expand(png_ptr);
    png_set_gray_to_rgb(png_ptr);
  }

  /* convert simple transparency to a full alpha channel */
  if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS))
    png_set_expand(png_ptr);

  /* expand plain RGB into RGBA */
  if (colour_type == PNG_COLOR_TYPE_RGB ||
      colour_type == PNG_COLOR_TYPE_GRAY ||
      colour_type == PNG_COLOR_TYPE_PALETTE) {
#ifdef WIN32
    png_set_filler(png_ptr, 0xff, PNG_FILLER_AFTER);
#else
    png_set_filler(png_ptr, 0xff, PNG_FILLER_BEFORE);
#endif
  }

#ifndef WIN32
  /* Textures have ABGR ordering */
  png_set_bgr(png_ptr);
  png_set_swap_alpha(png_ptr);
#endif                

  /* allocate memory for the image */

  *mem= (uint32_t *)mav_malloc(sizeof(uint32_t) * (*width) * (*height));
  row_pointer= (png_bytep *)mav_malloc(sizeof(png_bytep) * (*height));

  for (row=0; row<(*height); row++)
    row_pointer[(*height)-row-1]= (png_bytep)((*mem) + (*width) * row);

  /* now read it */

  png_read_image(png_ptr, row_pointer);

  mav_free(row_pointer);
  png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
  fclose(file);

  /* let calling fn know if texture isn't entirely opaque */
  if (colour_type & PNG_COLOR_MASK_ALPHA ||
      png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS))
    return MAV_TRANSPARENT;
  else
    return MAV_TRUE;
}
#endif



/* Routines to define a texture in a given palette (from file) */

#define L2 0.301029995

int mavlib_paletteTextureSetSC(MAV_palette *p, int index, char *filename)
{
  int width, height, rv= MAV_FALSE;

/* Check index is within range */

  if (index > mav_opt_maxTextures-1) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Texture index %i too big (max %i), ignoring\n", index, mav_opt_maxTextures-1);
  }
  else
  {
    
    /* Warn if index is already defined */
    
    if (mav_opt_paletteWarn && p->defwarn && p->tex_defwarn && p->texlist[index].defwarn && p->texlist[index].defined==MAV_REDEFINE_WARN && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Texture index %i already defined in palette, overwriting\n", index);
    
    /* We can read PPM and PNG directly, otherwise use ImageMagick's covert program */
    
    if (strstr(filename, ".pnm") || strstr(filename, ".ppm")) 
    {
      rv= mavlib_readPPM(filename, &p->texlist[index].width, &p->texlist[index].height, &p->texlist[index].mem);
    }
#ifdef MAV_PNG
    else if (strstr(filename, ".png")) 
    {
       rv= mavlib_readPNG(filename, &p->texlist[index].width, &p->texlist[index].height, &p->texlist[index].mem);
       if (rv == MAV_TRANSPARENT) p->texlist[index].transparent= MAV_TRUE;
     }
#endif
    else
    {
#if !defined(macintosh)
      char buf[500], f1[500], f2[500];
      int srv;

#ifdef MAV_PNG
      sprintf(f1, "%s/mavtex%i.png", mav_getTempDir(), mav_getPID());
#else
      sprintf(f1, "%s/mavtex%i.ppm", mav_getTempDir(), mav_getPID());
#endif
      sprintf(f2, "%s/mavtexlog%i", mav_getTempDir(), mav_getPID());

      sprintf(buf, "convert -matte %s %s 2>%s", filename, f1, f2);
      srv= system(buf);

      if (srv==0)
      {
#ifdef MAV_PNG
	rv= mavlib_readPNG(f1, &p->texlist[index].width, &p->texlist[index].height, &p->texlist[index].mem);
	if (rv == MAV_TRANSPARENT) p->texlist[index].transparent= MAV_TRUE;
#else
	rv= mavlib_readPPM(f1, &p->texlist[index].width, &p->texlist[index].height, &p->texlist[index].mem);
#endif
	if (rv != MAV_FALSE) {
	  unlink(f1);
	  unlink(f2);
	}
      }
      else
#endif
      {
	if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: can not read texture file %s, ignoring\n", filename);	
	rv= MAV_FALSE;
      }
    }
    
    /* Only integer powers of 2 allowed for height and width, use ImageMagick's covert program to scale */
    
    if (rv) {
      width= p->texlist[index].width;
      height= p->texlist[index].height;
      
      if (fabs(log10(width)/L2 - (int) (log10(width)/L2))>0.0001 || 
	  fabs(log10(height)/L2 - (int) (log10(height)/L2))>0.0001) {
	int tw, th;

	/* Calculate required size */
	tw= pow(2, ((int) (log10(width)/L2))+1);
	th= pow(2, ((int) (log10(height)/L2))+1);
	
	/* Max texture size of 256x256 on Voodoo */
	if (mavlib_voodoo) {
	  if (tw>256) tw=256;
	  if (th>256) th=256;
	}

	/* Free old texture memory */
	mav_free(p->texlist[index].mem);

	{
#if !defined(macintosh)
	  char buf[500], f1[500], f2[500];
	  int srv;

#ifdef MAV_PNG
	  sprintf(f1, "%s/mavtex%i.png", mav_getTempDir(), mav_getPID());
#else
	  sprintf(f1, "%s/mavtex%i.ppm", mav_getTempDir(), mav_getPID());
#endif
	  sprintf(f2, "%s/mavtexlog%i", mav_getTempDir(), mav_getPID());

	  sprintf(buf, "convert -matte -geometry %ix%i! %s %s 2>%s", tw, th, filename, f1, f2);

	  srv= system(buf);

	  if (srv==0)
	  {
#ifdef MAV_PNG
	    rv= mavlib_readPNG(f1, &p->texlist[index].width, &p->texlist[index].height, &p->texlist[index].mem);
	    if (rv == MAV_TRANSPARENT) p->texlist[index].transparent= MAV_TRUE;
#else
	    rv= mavlib_readPPM(f1, &p->texlist[index].width, &p->texlist[index].height, &p->texlist[index].mem);
#endif
	    if (rv != MAV_FALSE) {
	      unlink(f1);
	      unlink(f2);
	    }
	  }
	  else
#endif
	  {
	    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: width and height (%ix%i) must be an integer power of 2, ignoring\n", width, height);
	    rv=MAV_FALSE;
	  }
	}
      }
    }
    
    /* set defined flag */
    
    if (rv) {
      p->texlist[index].defined=MAV_REDEFINE_WARN;
      p->texlist[index].filename= strdup(filename);
      p->texlist[index].texEnv= NULL;
      
      mav_gfxTextureSet(&p->texlist[index], p->texEnv);
      mav_surfaceParamsUndefine();
    }
  }
  
  return(rv);
}

int mav_paletteTextureSet(MAV_palette *p, int index, char *filename)
{
  int rv= MAV_TRUE;

  if (mav_opt_shareContexts)
  {
    rv= mavlib_paletteTextureSetSC(p, index, filename);
  }
  else
  {
    MAV_window *orig= mav_win_current, *win;
    int pw= mav_opt_paletteWarn;
    mav_opt_paletteWarn= MAV_FALSE;

    /* Set texture in each window */
    mav_listPointerReset(mav_win_list);
    while (mav_listItemNext(mav_win_list, (void **) &win)) {
      mav_windowSet(win);    
      rv|= mavlib_paletteTextureSetSC(p, index, filename);
    }

    mav_windowSet(orig);    
    mav_opt_paletteWarn= pw;
  }
  
  return rv;
}



/* Routines to set the alpha component of a texture to a given value */

void mavlib_paletteTextureAlphaSetSC(MAV_palette *p, int index, float a)
{
  MAV_texture *tex;
  int i, v;
  unsigned char *c;

  /* Clamp alpha value and turn into an integer */
  if (a>1.0) a=1.0;
  if (a<0.0) a=0.0;
  v=255*a;

  /* Check index is within range */

  if (index > mav_opt_maxTextures-1) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Texture index %i too big (max %i), ignoring\n", index, mav_opt_maxTextures-1);
  }
  else
  {
      
    /* Warn if index is not already defined, then define it */
    
    if (!p->texlist[index].defined && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Texture index %i not defined in palette, overwriting\n", index);

    tex= &p->texlist[index];
    for (i=0; i<tex->width*tex->height; i++) {
      c= (unsigned char *) &tex->mem[i];
#ifdef WIN32
      /* Textures have RGBA ordering */
      c[3]= (unsigned char) v;
#else
      /* Textures have ABGR ordering */
      c[0]= (unsigned char) v;
#endif		
    }

    /* set transparency flag */
    if (v!=255) 
    {
      tex->transparent=1;
    }
    else
    {
      tex->transparent=0;
    }
    
    mav_gfxTextureSet(&p->texlist[index], p->texEnv);
    mav_surfaceParamsUndefine();
  }
}

void mav_paletteTextureAlphaSet(MAV_palette *p, int index, float a)
{
  if (mav_opt_shareContexts)
  {
    mavlib_paletteTextureAlphaSetSC(p, index, a);
  }
  else
  {
    MAV_window *orig= mav_win_current, *win;
    int pw= mav_opt_paletteWarn;
    mav_opt_paletteWarn= MAV_FALSE;

    /* Set texture in each window */
    mav_listPointerReset(mav_win_list);
    while (mav_listItemNext(mav_win_list, (void **) &win)) {
      mav_windowSet(win);    
      mavlib_paletteTextureAlphaSetSC(p, index, a);
    }

    mav_windowSet(orig);    
    mav_opt_paletteWarn= pw;
  }
}



/* Routines to set the alpha component of a texture whose pixels are coloured rgb */

void mavlib_paletteTextureColourAlphaSetSC(MAV_palette *p, int index, int r, int g, int b, float a)
{
  MAV_texture *tex;
  int i, v;
  unsigned char *c;

  /* Clamp alpha value and turn into an integer */
  if (a>1.0) a=1.0;
  if (a<0.0) a=0.0;
  v=255*a;

  /* Check index is within range */
  
  if (index > mav_opt_maxTextures-1) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Texture index %i too big (max %i), ignoring\n", index, mav_opt_maxTextures-1);
  }
  else
  {

/* Warn if index is not already defined, then define it */
    
    if (!p->texlist[index].defined && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Texture index %i not defined in palette, overwriting\n", index);
    
    tex= &p->texlist[index];
    for (i=0; i<tex->width*tex->height; i++) {
      c= (unsigned char *) &tex->mem[i];
#ifdef WIN32
      /* Textures have RGBA ordering */      
      if (c[0]==r && c[1]==g && c[2]==b) c[3]=(unsigned char) v;
#else
      /* Textures have ABGR ordering */      
      if (c[3]==r && c[2]==g && c[1]==b) c[0]=(unsigned char) v;
#endif		
    }

    /* set transparency flag */
    if (v!=255) 
    {
      tex->transparent=1;
    }
    else
    {
      tex->transparent=0;
    }
    
    mav_gfxTextureSet(&p->texlist[index], p->texEnv);
    mav_surfaceParamsUndefine();
  }
}

void mav_paletteTextureColourAlphaSet(MAV_palette *p, int index, int r, int g, int b, float a)
{
  if (mav_opt_shareContexts)
  {
    mavlib_paletteTextureColourAlphaSetSC(p, index, r, g, b, a);
  }
  else
  {
    MAV_window *orig= mav_win_current, *win;
    int pw= mav_opt_paletteWarn;
    mav_opt_paletteWarn= MAV_FALSE;

    /* Set texture in each window */
    mav_listPointerReset(mav_win_list);
    while (mav_listItemNext(mav_win_list, (void **) &win)) {
      mav_windowSet(win);
      mavlib_paletteTextureColourAlphaSetSC(p, index, r, g, b, a);
    }

    mav_windowSet(orig);    
    mav_opt_paletteWarn= pw;
  }
}



/* Routine to define if a texture is mipmapped */

void mav_paletteTextureMipmappingSet(MAV_palette *p, int index, int v)
{
  /* Check index is within range */
  
  if (index > mav_opt_maxTextures-1) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Texture index %i too big (max %i), ignoring\n", index, mav_opt_maxTextures-1);
  }
  else
  {
    p->texlist[index].mipmapped= v;
  }
}



/* Routine to define a texture in a given palette (from mem) */

int mavlib_paletteTextureSetFromMemSC(MAV_palette *p, int index, int width, int height, uint32_t *mem)
{
  int rv= MAV_TRUE;

/* Check index is within range */

  if (index > mav_opt_maxTextures-1)
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Texture index %i too big (max %i), ignoring\n", index, mav_opt_maxTextures-1);
    rv= MAV_FALSE;
  }
  else
  {
    
    /* Warn if index is already defined */
    
    if (mav_opt_paletteWarn && p->defwarn && p->tex_defwarn && p->texlist[index].defwarn && p->texlist[index].defined==MAV_REDEFINE_WARN && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Texture index %i already defined in palette, overwriting\n", index);

    /* Only interger powers of 2 allowed for height and width */

    if (fabs(log10(width)/L2 - (int) (log10(width)/L2))>0.0001 || 
	fabs(log10(height)/L2 - (int) (log10(height)/L2))>0.0001) {
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: width and height (%ix%i) must be an integer power of 2, ignoring\n", width, height);
      rv=MAV_FALSE;
    }
  
    if (rv) {
      p->texlist[index].defined=MAV_REDEFINE_WARN;
      p->texlist[index].filename= strdup("from memory");
      p->texlist[index].texEnv= NULL;
      p->texlist[index].width= width;
      p->texlist[index].height= height;
      p->texlist[index].mem= mem;
      
      mav_gfxTextureSet(&p->texlist[index], p->texEnv);
      mav_surfaceParamsUndefine();
    }
  }
  
  return rv;
}

int mav_paletteTextureSetFromMem(MAV_palette *p, int index, int width, int height, uint32_t *mem)
{
  int rv= MAV_TRUE;

  if (mav_opt_shareContexts)
  {
    rv= mavlib_paletteTextureSetFromMemSC(p, index, width, height, mem);
  }
  else
  {
    MAV_window *orig= mav_win_current, *win;
    int pw= mav_opt_paletteWarn;
    mav_opt_paletteWarn= MAV_FALSE;

    /* Set texture in each window */
    mav_listPointerReset(mav_win_list);
    while (mav_listItemNext(mav_win_list, (void **) &win)) {
      mav_windowSet(win);
      rv|= mavlib_paletteTextureSetFromMemSC(p, index, width, height, mem);
    }

    mav_windowSet(orig);    
    mav_opt_paletteWarn= pw;
  }
  
  return rv;
}



/* Define texture environment */

int mav_paletteTextureEnvSet(MAV_palette *p, int index, MAV_texEnvFn fn)
{
  int rv= MAV_TRUE;

/* Check index is within range */

  if (index > mav_opt_maxTextures-1)
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Texture index %i too big (max %i), ignoring\n", index, mav_opt_maxTextures-1);
    rv= MAV_FALSE;
  }
  else
  {
    /* Warn if index is not already defined, then define it */
    
    if (!p->texlist[index].defined && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Texture index %i not defined in palette, overwriting\n", index);
    
    p->texlist[index].texEnv= fn;
  }

  return rv;
}



/* Default texture environment */

void mav_texEnvDefault(MAV_texture *tex)
{
  if (tex->mipmapped)
  {
    mav_gfxTextureEnv1Set(3);
  }
  else
  {
    mav_gfxTextureEnv1Set(1);
  }
}



/* Clamped (non-repeating) texture environment */

void mav_texEnvClamp(MAV_texture *tex)
{
  if (tex->mipmapped)
  {
    mav_gfxTextureEnv1Set(4);
  }
  else
  {
    mav_gfxTextureEnv1Set(2);
  }
}



/* Routine to free a texture */

void mav_paletteTextureFree(MAV_palette *p, int index)
{
  if (p->texlist[index].defined) {
    if (p->texlist[index].filename) mav_free(p->texlist[index].filename);
    if (p->texlist[index].mem) mav_free(p->texlist[index].mem);
    if (p->texlist[index].mipmapped) {
      int i;
      for (i=0; i<p->texlist[index].nmaps; i++) mav_free(p->texlist[index].mipmap[i]);
      if (p->texlist[index].nmaps!=0) {
	mav_free(p->texlist[index].xsize);
	mav_free(p->texlist[index].ysize);
	mav_free(p->texlist[index].mipmap);
      }
    }
    p->texlist[index].defined= MAV_FALSE;
  }
}



/* Routine to define a light in a given palette */

void mav_paletteLightSet(MAV_palette *p, int index, float a1, float a2, float a3, float a4, 
		                             float d1, float d2, float d3, float d4,
		                             float s1, float s2, float s3, float s4)
{
  /* Check index is within range */
  
  if (index > mav_opt_maxLights-1) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Light index %i too big (max %i), ignoring\n", index, mav_opt_maxLights-1);
  }
  else
  {
      
/* Warn if index is already defined, then define it */

    if (mav_opt_paletteWarn && p->defwarn && p->light_defwarn && p->lightlist[index].defwarn && p->lightlist[index].defined==MAV_REDEFINE_WARN && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Light index %i already defined in palette, overwriting\n", index);
    
    p->lightlist[index].defined= MAV_REDEFINE_WARN;
    p->lightlist[index].index= index;

    p->lightlist[index].ambient[0]=a1;
    p->lightlist[index].ambient[1]=a2;
    p->lightlist[index].ambient[2]=a3;
    p->lightlist[index].ambient[3]=a4;
      
    p->lightlist[index].diffuse[0]=d1;
    p->lightlist[index].diffuse[1]=d2;
    p->lightlist[index].diffuse[2]=d3;
    p->lightlist[index].diffuse[3]=d4;
    
    p->lightlist[index].specular[0]=s1;
    p->lightlist[index].specular[1]=s2;
    p->lightlist[index].specular[2]=s3;
    p->lightlist[index].specular[3]=s4;

    mav_gfxLightSet(p->lightlist[index]);
    /* set changes in all windows with this palette */
    mavlib_lightUpd(index, p);
  }
}



/* Routine to position a light */

void mav_paletteLightPos(MAV_palette *p, int index, MAV_vector pos) 
{
  /* Check index is within range */
  
  if (index >= mav_opt_maxLights) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Light index %i too big (max %i), ignoring\n", index, mav_opt_maxLights-1);
  }
  else
  {
    /* Warn if index is not defined */
    
    if (!p->lightlist[index].defined && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Light index %i not defined in palette\n", index);
    
    p->lightlist[index].pos= pos;

    /* set changes in all windows with this palette */
    mavlib_lightPosUpd(index, p);
  }
}



/* Routine to set positioning (absolute or relative) of a light */

void mav_paletteLightPositioning(MAV_palette *p, int index, int pos) 
{
  /* Check index is within range */
  
  if (index >= mav_opt_maxLights) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Light index %i too big (max %i), ignoring\n", index, mav_opt_maxLights-1);
  }
  else
  {
    /* Warn if index is not defined */
    
    if (!p->lightlist[index].defined && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Light index %i not defined in palette\n", index);

    /* set positioning */
    if (pos==MAV_LIGHT_RELATIVE)
    {
      p->lightlist[index].positioning= MAV_LIGHT_RELATIVE;
    }
    else
    {
      p->lightlist[index].positioning= MAV_LIGHT_ABSOLUTE;
    }
  }
}



/* Routine to define the lighting model for a given palette */

void mav_paletteLightingModelSet(MAV_palette *p, float a1, float a2, float a3, float a4, int local_viewer)
{
  if (mav_opt_paletteWarn && p->defwarn && p->lm_defwarn && p->lm.defwarn && p->lm.defined==MAV_REDEFINE_WARN && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Lighting model already defined in palette, overwriting\n");

  p->lm.defined=MAV_REDEFINE_WARN;
  p->lm.ambient[0]=a1;
  p->lm.ambient[1]=a2;
  p->lm.ambient[2]=a3;
  p->lm.ambient[3]=a4;
  p->lm.localviewer=local_viewer;

  mav_gfxLightingModelSet(p->lm);
  /* set changes in all windows with this palette */
  mavlib_lightingModelUpd(p);
}



/* Routine to check if surface params are transparent */

int mav_surfaceParamsIsTransparent(MAV_window *win, MAV_surfaceParams *sp)
{
  int rv= MAV_FALSE;

  if (sp==NULL) return (mav_surfaceParamsIsTransparent(win, mav_sp_current));

  switch (sp->mode) {
  case MAV_COLOUR:
    if (sp->colour>0 && win->palette->collist[sp->colour].colour[3]<0.99) rv= MAV_TRUE;
    break;
  case MAV_MATERIAL:
    if (win->palette->matlist[sp->material].ambient[3]<0.99 ||
	win->palette->matlist[sp->material].diffuse[3]<0.99 ||
	win->palette->matlist[sp->material].specular[3]<0.99 ||
	win->palette->matlist[sp->material].emission[3]<0.99) rv= MAV_TRUE;
    break;
  case MAV_TEXTURE:
    if (win->palette->texlist[sp->texture].transparent) rv= MAV_TRUE;
    break;
  case MAV_LIT_TEXTURE:
  case MAV_BLENDED_TEXTURE:
    if (win->palette->texlist[sp->texture].transparent) rv= MAV_TRUE;
    if (win->palette->matlist[sp->material].ambient[3]<0.99 ||
	win->palette->matlist[sp->material].diffuse[3]<0.99 ||
	win->palette->matlist[sp->material].specular[3]<0.99 ||
	win->palette->matlist[sp->material].emission[3]<0.99) rv= MAV_TRUE;
    break;
  }

  return rv;
}



/* Routine to check if surface params are textured */

int mav_surfaceParamsIsTextured(MAV_window *win, MAV_surfaceParams *sp)
{
  int rv= MAV_FALSE;

  if (sp==NULL) return (mav_surfaceParamsIsTextured(win, mav_sp_current));

  if (sp->mode>=MAV_TEXTURE) rv= MAV_TRUE;

  return rv;
}



/* Routines to define a font */

void mavlib_paletteFontSetSC(MAV_palette *p, int index, char *s)
{
/* Check index is within range */

  if (index > mav_opt_maxFonts-1) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Font index %i too big (max %i), ignoring\n", index, mav_opt_maxFonts-1);
  }
  else
  {

/* Warn if index is already defined, then define it */

    if (mav_opt_paletteWarn && p->defwarn && p->font_defwarn && p->fontlist[index].defwarn && p->fontlist[index].defined==MAV_REDEFINE_WARN && mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Font index %i already defined in palette, overwriting\n", index);

    if (mav_gfxWindowFontSet(s, index, p->fontlist[index].width)!=0) 
    {
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: can not find font %s, ignoring\n", s);
    }
    else
    {
      p->fontlist[index].name= strdup(s);
      p->fontlist[index].defined = MAV_REDEFINE_WARN;
    }
  }
}

void mav_paletteFontSet(MAV_palette *p, int index, char *s)
{
  if (mav_opt_shareContexts)
  {
    mavlib_paletteFontSetSC(p, index, s);
  }
  else
  {
    MAV_window *orig= mav_win_current, *win;
    int pw= mav_opt_paletteWarn;
    mav_opt_paletteWarn= MAV_FALSE;

    /* Set font in each window */
    mav_listPointerReset(mav_win_list);
    while (mav_listItemNext(mav_win_list, (void **) &win)) {
      mav_windowSet(win);    
      mavlib_paletteFontSetSC(p, index, s);
    }

    mav_windowSet(orig);    
    mav_opt_paletteWarn= pw;
  }
}



/* Routines to return an empty index in the palette */

int mav_paletteColourIndexEmptyGet(MAV_palette *p)
{
  int i;

  for (i=0; i<mav_opt_maxColours; i++) if (p->collist[i].defined==MAV_FALSE) return i;
  
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: can not find empty colour index in palette\n");  

  return -1;
}

int mav_paletteMaterialIndexEmptyGet(MAV_palette *p)
{
  int i;

  for (i=0; i<mav_opt_maxMaterials; i++) if (p->matlist[i].defined==MAV_FALSE) return i;
  
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: can not find empty material index in palette\n");  

  return -1;
}

int mav_paletteTextureIndexEmptyGet(MAV_palette *p)
{
  int i;

  for (i=0; i<mav_opt_maxTextures; i++) if (p->texlist[i].defined==MAV_FALSE) return i;
  
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: can not find empty texture index in palette\n");  

  return -1;
}

int mav_paletteLightIndexEmptyGet(MAV_palette *p)
{
  int i;

  for (i=0; i<mav_opt_maxLights; i++) if (p->lightlist[i].defined==MAV_FALSE) return i;
  
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: can not find empty light index in palette\n");  

  return -1;
}

int mav_paletteFontIndexEmptyGet(MAV_palette *p)
{
  int i;

  for (i=0; i<mav_opt_maxFonts; i++) if (p->fontlist[i].defined==MAV_FALSE) return i;
  
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: can not find empty font index in palette\n");  

  return -1;
}



/* Routines to return a matching index in the palette */

int mav_paletteColourIndexMatchGet(MAV_palette *p, float r, float g, float b, float a)
{
  int i;
  
  for (i=0; i<mav_opt_maxColours; i++) {
    if (p->collist[i].defined && fabs(p->collist[i].colour[0]-r)<0.01 && 
	fabs(p->collist[i].colour[1]-g)<0.01 && fabs(p->collist[i].colour[2]-b) <0.01 && 
	fabs(p->collist[i].colour[3]-a)<0.01) return i;
  }

  return -1;
}

int mav_paletteMaterialIndexMatchGet(MAV_palette *p, float ar, float ag, float ab, float aa, 
                                     float dr, float dg, float db, float da, float sr, float sg, float sb, float sa, 
                                     float er, float eg, float eb, float ea, float shin)
{
  int i;
  
  for (i=0; i<mav_opt_maxMaterials; i++) {
    if (p->matlist[i].defined && 
	fabs(p->matlist[i].ambient[0]-ar)<0.01 && fabs(p->matlist[i].ambient[1]-ag)<0.01 && 
	fabs(p->matlist[i].ambient[2]-ab)<0.01 && fabs(p->matlist[i].ambient[3]-aa)<0.01 && 
	fabs(p->matlist[i].diffuse[0]-dr)<0.01 && fabs(p->matlist[i].diffuse[1]-dg)<0.01 && 
	fabs(p->matlist[i].diffuse[2]-db)<0.01 && fabs(p->matlist[i].diffuse[3]-da)<0.01 && 
	fabs(p->matlist[i].specular[0]-sr)<0.01 && fabs(p->matlist[i].specular[1]-sg)<0.01 && 
	fabs(p->matlist[i].specular[2]-sb)<0.01 && fabs(p->matlist[i].specular[3]-sa)<0.01 && 
	fabs(p->matlist[i].emission[0]-er)<0.01 && fabs(p->matlist[i].emission[1]-eg)<0.01 && 
	fabs(p->matlist[i].emission[2]-eb)<0.01 && fabs(p->matlist[i].emission[3]-ea)<0.01 && 
	fabs(p->matlist[i].shine-shin)<0.01) return i;
  }

  return -1;
}

int mav_paletteTextureIndexMatchGet(MAV_palette *p, char *filename)
{
  int i;
  
  for (i=0; i<mav_opt_maxTextures; i++) {
    if (p->texlist[i].defined && p->texlist[i].filename && !strcmp(p->texlist[i].filename, filename)) return i;
  }

  return -1;
}

int mav_paletteLightIndexMatchGet(MAV_palette *p, float ar, float ag, float ab, float aa, 
		                  float dr, float dg, float db, float da, float sr, float sg, float sb, float sa)
{
  int i;
  
  for (i=0; i<mav_opt_maxLights; i++) {
    if (p->lightlist[i].defined && 
	fabs(p->lightlist[i].ambient[0]-ar)<0.01 && fabs(p->lightlist[i].ambient[1]-ag)<0.01 &&
	fabs(p->lightlist[i].ambient[2]-ab)<0.01 && fabs(p->lightlist[i].ambient[3]-aa)<0.01 &&
	fabs(p->lightlist[i].diffuse[0]-dr)<0.01 && fabs(p->lightlist[i].diffuse[1]-dg)<0.01 &&
	fabs(p->lightlist[i].diffuse[2]-db)<0.01 && fabs(p->lightlist[i].diffuse[3]-da)<0.01 &&
	fabs(p->lightlist[i].specular[0]-sr)<0.01 && fabs(p->lightlist[i].specular[1]-sg)<0.01 &&
	fabs(p->lightlist[i].specular[2]-sb)<0.01 && fabs(p->lightlist[i].specular[3]-sa)<0.01) return i;
  }

  return -1;
}

int mav_paletteFontIndexMatchGet(MAV_palette *p, char *s)
{
  int i;
  
  for (i=0; i<mav_opt_maxFonts; i++) {
    if (p->fontlist[i].defined && p->fontlist[i].name && !strcmp(p->fontlist[i].name, s)) return i;
  }

  return -1;
}



int mavlib_defaultColours[][3]={
{148,   0, 211},             /* darkviolet */
{178,  34,  34},             /* firebrick */
{162, 205,  90},             /* darkolivegreen3 */
{135, 206, 255},             /* skyblue1 */
{112, 112, 112},             /* gray44 */
{255, 250, 205},             /* lemonchiffon1 */
{105, 105, 105},             /* gray41 */
{208,  32, 144},             /* violetred */
{255, 250, 205},             /* lemon chiffon */
{143, 143, 143},             /* grey56 */
{199,  21, 133},             /* mediumvioletred */
{132, 132, 132},             /* sgigrey52 */
{180, 238, 180},             /* darkseagreen2 */
{222, 222, 222},             /* grey87 */
{119, 136, 153},             /* lightslategray */
{ 64,  64,  64},             /* gray25 */
{255, 228, 196},             /* bisque1 */
{125,  38, 205},             /* purple3 */
{198, 113, 113},             /* sgi salmon */
{130, 130, 130},             /* gray51 */
{132, 112, 255},             /* light slate blue */
{137, 104, 205},             /* mediumpurple3 */
{ 51,  51,  51},             /* sgi gray 20 */
{240, 128, 128},             /* lightcoral */
{ 48,  48,  48},             /* gray19 */
{135, 206, 235},             /* sky blue */
{139, 123, 139},             /* thistle4 */
{238, 213, 210},             /* mistyrose2 */
{143, 143, 143},             /* gray56 */
{255, 174, 185},             /* lightpink1 */
{105, 105, 105},             /* dimgrey */
{255, 246, 143},             /* khaki1 */
{139,   0,   0},             /* darkred */
{238,  92,  66},             /* tomato2 */
{112, 112, 112},             /* sgi gray 44 */
{205, 129,  98},             /* lightsalmon3 */
{245, 245, 245},             /* white smoke */
{102, 102, 102},             /* grey40 */
{238, 130,  98},             /* salmon2 */
{105, 105, 105},             /* dimgray */
{ 84,  84,  84},             /* grey33 */
{175, 238, 238},             /* paleturquoise */
{169, 169, 169},             /* dark grey */
{198, 226, 255}              /* slategray1 */
};

extern int mavlib_texNum;
extern uint32_t mavlib_tex[][16384];

void mavlib_paletteDefaultValues(MAV_palette *p)
{
  int i;

  mav_paletteLightingModelSet(p, 0.4, 0.4, 0.4, 1.0, MAV_TRUE);
  mav_paletteLightSet(p, 0,  0.0, 0.0, 0.0, 1.0,  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0);
  mav_paletteLightPos(p, 0, mav_vectorSet(100,150,150));
    
  /* do not warn on redefine */
  p->lm.defined= MAV_REDEFINE_NOWARN;
  p->lightlist[0].defined= MAV_REDEFINE_NOWARN;

  for (i=0; i<20; i++) {
    mav_paletteColourSet(p, i, mavlib_defaultColours[i][0]/255.0, mavlib_defaultColours[i][1]/255.0, mavlib_defaultColours[i][2]/255.0, 1.0);
    
    mav_paletteMaterialSet(p, i, mavlib_defaultColours[i][0]/255.0, mavlib_defaultColours[i][1]/255.0, mavlib_defaultColours[i][2]/255.0, 1.0, mavlib_defaultColours[i][0]/255.0, mavlib_defaultColours[i][1]/255.0, mavlib_defaultColours[i][2]/255.0, 1.0, mavlib_defaultColours[i][0]/255.0, mavlib_defaultColours[i][1]/255.0, mavlib_defaultColours[i][2]/255.0, 1.0, 0.0, 0.0, 0.0, 1.0, 30.0);
    
    p->matlist[i].defined= MAV_REDEFINE_NOWARN;
    p->collist[i].defined= MAV_REDEFINE_NOWARN;
  }
  
  for (i=0; i<mavlib_texNum; i++) {
    mav_paletteTextureSetFromMem(p, i+1, 128, 128, mavlib_tex[i]);
    p->texlist[i+1].defined= MAV_REDEFINE_NOWARN;
  }

  mav_paletteFontSet(p, 0, "-adobe-helvetica-bold-r-*-*-14-140-*-*-*-*-*-*");
  p->fontlist[0].defined= MAV_REDEFINE_NOWARN;

  /* Define deault sp */
  if (!mav_sp_default) mav_sp_default= mav_surfaceParamsNew(MAV_MATERIAL, 0, 1, 0);
}



/* Routines to set re-definition warning */

void mav_paletteWarnSet(MAV_palette *p, int v)
{
  p->defwarn= v;
}

void mav_paletteColourWarnSet(MAV_palette *p, int v)
{
  p->col_defwarn= v;
}

void mav_paletteMaterialWarnSet(MAV_palette *p, int v)
{
  p->mat_defwarn= v;
}

void mav_paletteTextureWarnSet(MAV_palette *p, int v)
{
  p->tex_defwarn= v;
}

void mav_paletteLightWarnSet(MAV_palette *p, int v)
{
  p->light_defwarn= v;
}

void mav_paletteLightingModelWarnSet(MAV_palette *p, int v)
{
  p->lm_defwarn= v;
}

void mav_paletteFontWarnSet(MAV_palette *p, int v)
{
  p->font_defwarn= v;
}

void mav_paletteColourIndexWarnSet(MAV_palette *p, int index, int v)
{
  if (index > mav_opt_maxColours-1) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Colour index %i too big (max %i), ignoring\n", index, mav_opt_maxColours-1);
  }
  else
  {
    p->collist[index].defwarn = v;
  }
}

void mav_paletteMaterialIndexWarnSet(MAV_palette *p, int index, int v)
{
  if (index > mav_opt_maxMaterials-1) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Material index %i too big (max %i), ignoring\n", index, mav_opt_maxMaterials-1);
  }
  else
  {
    p->matlist[index].defwarn = v;
  }
}

void mav_paletteTextureIndexWarnSet(MAV_palette *p, int index, int v)
{
  if (index > mav_opt_maxTextures-1) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Texture index %i too big (max %i), ignoring\n", index, mav_opt_maxTextures-1);
  }
  else
  {
    p->texlist[index].defwarn = v;
  }
}

void mav_paletteLightIndexWarnSet(MAV_palette *p, int index, int v)
{
  if (index > mav_opt_maxLights-1) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Light index %i too big (max %i), ignoring\n", index, mav_opt_maxLights-1);
  }
  else
  {
    p->lightlist[index].defwarn= v;
  }
}

void mav_paletteFontIndexWarnSet(MAV_palette *p, int index, int v)
{
  if (index > mav_opt_maxFonts-1) 
  {
    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Error: Font index %i too big (max %i), ignoring\n", index, mav_opt_maxFonts-1);
  }
  else
  {
    p->fontlist[index].defwarn= v;
  }
}

