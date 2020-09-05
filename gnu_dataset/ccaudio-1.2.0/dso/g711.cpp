// Copyright (C) 2006 Free Software Foundation.
//  
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software 
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
// 
// As a special exception to the GNU General Public License, permission is 
// granted for additional uses of the text contained in its release 
// of ccaudio.
// 
// The exception is that, if you link the ccaudio library with other
// files to produce an executable, this does not by itself cause the
// resulting executable to be covered by the GNU General Public License.
// Your use of that executable is in no way restricted on account of
// linking the ccaudio library code into it.
// 
// This exception does not however invalidate any other reasons why
// the executable file might be covered by the GNU General Public License.
// 
// This exception applies only to the code released under the 
// name ccaudio.  If you copy code from other releases into a copy of
// ccaudio, as the General Public License permits, the exception does
// not apply to the code that you add in this way.  To avoid misleading
// anyone as to the status of such modified files, you must delete
// this exception notice from them.
// 
// If you write modifications of your own for ccaudio, it is your choice
// whether to permit this exception to apply to your modifications.
// If you do not wish that, delete this exception notice.  

#include <cc++/config.h>
#include <math.h>
#include "private.h"
#include "audio.h"

#ifdef	CCXX_NAMESPACES
namespace ost {
#endif

static class g711u : public AudioCodec
{
public:
	g711u() : AudioCodec("g.711", mulawAudio) {};

	unsigned encode(Linear buffer, void *source, unsigned lsamples);
	unsigned decode(Linear buffer, void *dest, unsigned lsamples); 
} g711u;

static class g711a : public AudioCodec
{
public:
	g711a() : AudioCodec("g.711", alawAudio) {};

	unsigned encode(Linear buffer, void *source, unsigned lsamples);
	unsigned decode(Linear buffer, void *dest, unsigned lsamples); 
} g711a;

// some asm optimized routines can be specified first, and set 
// ASM_OPTIMIZED.  This is the default codec routines if no asm optimized
// code present.

#ifndef	ASM_OPTIMIZED

unsigned g711u::encode(Linear buffer, void *dest, unsigned lsamples)
{
	static int ulaw[256] = {
        0,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,
        4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
        5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
        5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7};

	register short sample;
	int sign, exponent, mantissa, retval;
	register unsigned char *d = (unsigned char *)dest;
	unsigned count = lsamples;

	while(lsamples--)
	{
		sample = *(buffer++);
		        sign = (sample >> 8) & 0x80;
	        if(sign != 0) sample = -sample;
        	sample += 0x84;
        	exponent = ulaw[(sample >> 7) & 0xff];
        	mantissa = (sample >> (exponent + 3)) & 0x0f;
        	retval = ~(sign | (exponent << 4) | mantissa);
        	if(!retval)
                	retval = 0x02;
		*(d++) = (unsigned char)retval;
	}
	return count;
}

unsigned g711u::decode(Linear buffer, void *source, unsigned lsamples)
{
	register short t;
	register unsigned char ul;
	register unsigned char *src = (unsigned char *)source;
	unsigned count = lsamples;

	while(lsamples--)
	{
		ul = *(src++);
		ul = ~ul;		
	        t = ((ul & 0x0f) << 3) + 0x84;
        	t <<= ((unsigned)ul & 0x70) >> 4;
		*(buffer++) = ((ul & 0x80) ? (0x84 - t) : (t - 0x84));
	}
	return count;
}

#define	AMI_MASK	0x55

unsigned g711a::encode(Linear buffer, void *dest, unsigned lsamples)
{
	int mask, seg, pcm_val;
	unsigned count = lsamples;
	unsigned char *d = (unsigned char *)dest;

	static int seg_end[] = {
		0xFF, 0x1FF, 0x3FF, 0x7FF, 0xFFF, 0x1FFF, 0x3FFF, 0x7FFF};

	while(lsamples--)
	{
		pcm_val = *(buffer++);
		if(pcm_val >= 0)
			mask = AMI_MASK | 0x80;
		else
		{
			mask = AMI_MASK;
			pcm_val = -pcm_val;
		}
		for(seg = 0; seg < 8; seg++)
		{
			if(pcm_val <= seg_end[seg])
				break;
		}
		*(d++) = ((seg << 4) | ((pcm_val >> ((seg)  ?  (seg + 3)  :  4)) & 0x0F)) ^ mask;
	}
	return count;
}

unsigned g711a::decode(Linear buffer, void *source, unsigned lsamples)
{
	register short t, seg;
	register unsigned char al;
	register unsigned char *src = (unsigned char *)source;
	unsigned count = lsamples;

	while(lsamples--)
	{
		al = *(src++) ^ 0x55;
       		t = (al & 0x0f) << 4;
        	seg = ((unsigned)al & 0x70) >> 4;
        	switch (seg) {
        	case 0:
                	t += 8;
                	break;
        	case 1:
                	t += 0x108;
                	break;
        	default:
                	t += 0x108;
                	t <<= seg - 1;
       	 	}
                *(buffer++) = t;                                                               
	}
	return count;
}

#endif

#ifdef	CCXX_NAMESPACES
};
#endif
