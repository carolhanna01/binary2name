/* Copyright (C) 2000 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
  to anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU General Public License for full details.
  
  Everyone is granted permission to copy, modify and redistribute GNU
  Ghostscript, but only under the conditions described in the GNU General
  Public License.  A copy of this license is supposed to have been given
  to you along with GNU Ghostscript so you can know your rights and
  responsibilities.  It should be in a file named COPYING.  Among other
  things, the copyright notice and this notice must be preserved on all
  copies.
*/

/*$RCSfile: gstparam.h,v $ $Revision: 1.7.2.1 $ */
/* Transparency parameter definitions */

#ifndef gstparam_INCLUDED
#  define gstparam_INCLUDED

#include "gsccolor.h"
#include "gsrefct.h"

/* Define the names of the known blend modes. */
typedef enum {
    BLEND_MODE_Compatible,
    BLEND_MODE_Normal,
    BLEND_MODE_Multiply,
    BLEND_MODE_Screen,
    BLEND_MODE_Difference,
    BLEND_MODE_Darken,
    BLEND_MODE_Lighten,
    BLEND_MODE_ColorDodge,
    BLEND_MODE_ColorBurn,
    BLEND_MODE_Exclusion,
    BLEND_MODE_HardLight,
    BLEND_MODE_Overlay,
    BLEND_MODE_SoftLight,
    BLEND_MODE_Luminosity,
    BLEND_MODE_Hue,
    BLEND_MODE_Saturation,
    BLEND_MODE_Color
#define MAX_BLEND_MODE BLEND_MODE_Color
} gs_blend_mode_t;
#define GS_BLEND_MODE_NAMES\
  "Compatible", "Normal", "Multiply", "Screen", "Difference",\
  "Darken", "Lighten", "ColorDodge", "ColorBurn", "Exclusion",\
  "HardLight", "Overlay", "SoftLight", "Luminosity", "Hue",\
  "Saturation", "Color"

/* Define the common part for a transparency stack state. */
typedef enum {
    TRANSPARENCY_STATE_Group = 1,	/* must not be 0 */
    TRANSPARENCY_STATE_Mask
} gs_transparency_state_type_t;
#define GS_TRANSPARENCY_STATE_COMMON\
    gs_transparency_state_t *saved;\
    gs_transparency_state_type_t type
typedef struct gs_transparency_state_s gs_transparency_state_t;
struct gs_transparency_state_s {
    GS_TRANSPARENCY_STATE_COMMON;
};

/* Define the common part for a cached transparency mask. */
#define GS_TRANSPARENCY_MASK_COMMON\
    rc_header rc
typedef struct gs_transparency_mask_s {
    GS_TRANSPARENCY_MASK_COMMON;
} gs_transparency_mask_t;

/* Define the parameter structure for a transparency group. */
#ifndef gs_color_space_DEFINED
#  define gs_color_space_DEFINED
typedef struct gs_color_space_s gs_color_space;
#endif
/* (Update gs_trans_group_params_init if these change.) */
typedef struct gs_transparency_group_params_s {
    const gs_color_space *ColorSpace;
    bool Isolated;
    bool Knockout;
} gs_transparency_group_params_t;

/* Define the parameter structure for a transparency mask. */
typedef enum {
    TRANSPARENCY_MASK_Alpha,
    TRANSPARENCY_MASK_Luminosity
} gs_transparency_mask_subtype_t;
#define GS_TRANSPARENCY_MASK_SUBTYPE_NAMES\
  "Alpha", "Luminosity"
/* (Update gs_trans_mask_params_init if these change.) */
typedef struct gs_transparency_mask_params_s {
    gs_transparency_mask_subtype_t subtype;
    bool has_Background;
    float Background[GS_CLIENT_COLOR_MAX_COMPONENTS];
    int (*TransferFunction)(P3(floatp in, float *out, void *proc_data));
    void *TransferFunction_data;
} gs_transparency_mask_params_t;

/* Select the opacity or shape parameters. */
typedef enum {
    TRANSPARENCY_CHANNEL_Opacity = 0,
    TRANSPARENCY_CHANNEL_Shape = 1
} gs_transparency_channel_selector_t;

#endif /* gstparam_INCLUDED */
