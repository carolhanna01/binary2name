/* Copyright (C) 1995, 1996, 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: sisparam.h,v $ $Revision: 1.2.2.1 $ */
/* Generic image scaling stream definitions */
/* Requires strimpl.h */

#ifndef sisparam_INCLUDED
#  define sisparam_INCLUDED

/*
 * Image scaling streams all use a common set of parameters to define the
 * input and output data.  That is what we define here.
 */

/* Input values */
/*typedef byte PixelIn; */  /* per BitsPerComponentIn */
/*#define MaxValueIn 255 */  /* per MaxValueIn */

/* Output values */
/*typedef byte PixelOut; */  /* per BitsPerComponentOut */
/*#define MaxValueOut 255 */  /* per MaxValueOut */

/*
 * The 'support' S of a digital filter is the value such that the filter is
 * guaranteed to be zero for all arguments outside the range [-S..S].  We
 * limit the support so that we can put an upper bound on the time required
 * to compute an output value and on the amount of storage required for
 * X-filtered input data; this also allows us to use pre-scaled fixed-point
 * values for the weights if we wish.
 *
 * 8x8 pixels should be enough for any reasonable application....
 */
#define LOG2_MAX_ISCALE_SUPPORT 3
#define MAX_ISCALE_SUPPORT (1 << LOG2_MAX_ISCALE_SUPPORT)

/* Define image scaling stream parameters. */
typedef struct stream_image_scale_params_s {
    int Colors;			/* >= 1 */
    int BitsPerComponentIn;	/* bits per input value, 8 or 16 */
    uint MaxValueIn;		/* max value of input component, */
				/* 0 < MaxValueIn < 1 << BitsPerComponentIn */
    int WidthIn, HeightIn;	/* > 0 */
    int BitsPerComponentOut;	/* bits per output value, 8 or 16 */
    uint MaxValueOut;		/* max value of output component, */
				/* 0 < MaxValueOut < 1 << BitsPerComponentOut*/
    int WidthOut, HeightOut;	/* > 0 */
} stream_image_scale_params_t;

/* Define a generic image scaling stream state. */

#define stream_image_scale_state_common\
    stream_state_common;\
    stream_image_scale_params_t params

typedef struct stream_image_scale_state_s {
    stream_image_scale_state_common;
} stream_image_scale_state;

#endif /* sisparam_INCLUDED */
