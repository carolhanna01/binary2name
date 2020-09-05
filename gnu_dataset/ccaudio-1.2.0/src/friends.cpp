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
#include <cc++/export.h>
#include "private.h"
#include "audio.h"

#ifdef	CCXX_NAMESPACES
namespace ost {
#endif

int Audio::getCount(Encoding encoding)
{
	switch(encoding)
	{
	case gsmVoice:
		return 160;
	case unknownEncoding:
		return 0;
	case g723_3bit:
	case g723_5bit:
		return 8;
	case g721ADPCM:
	case okiADPCM:
	case voxADPCM:
	  
		return 2;
	default:
		return 1;
	}
}

int Audio::getFrame(Encoding encoding, int samples)
{
	int framing = 0;
	switch(encoding)
	{
	case gsmVoice:
		framing = 33;
		break;
	case g723_3bit:
		framing = 3;
		break;
	case g723_5bit:
		framing = 5;
		break;
	case unknownEncoding:
		return 0;
	case pcm32Stereo:
		return 8;
	case pcm32Mono:
	case pcm16Stereo:
	case cdaStereo:
		framing = 4;
		break;
	case pcm8Stereo:
	case pcm16Mono:
	case cdaMono:
		framing = 2;
		break;
	default:
		framing = 1;
	}
	if(!samples)
		return framing;

	return (samples / framing) * framing;
}

void Audio::fill(unsigned char *addr, int samples, Encoding encoding)
{
	int frame = getFrame(encoding);
	int count = getCount(encoding);

	if(!frame || !count)
		return;

	while(samples >= count)
	{
		switch(encoding)
		{
		case mulawAudio:
			*addr = 0xff;
			break;
		case alawAudio:
			*addr = 0x55;
			break;
		default:
			memset(addr, 0, frame);
			break;
		}
		addr += frame;
		samples -= count;
	}
}

Audio::Rate Audio::getRate(Encoding encoding)
{
	switch(encoding)
	{
	case pcm8Stereo:
	case pcm8Mono:
	case pcm16Stereo:
	case pcm16Mono:
	case pcm32Stereo:
	case pcm32Mono:
	case unknownEncoding:
		return rateUnknown;
	case voxADPCM:
		return rate6khz;
	case cdaStereo:
	case cdaMono:
		return rate44khz;
	default:
		return rate8khz;
	}
}

unsigned long Audio::toSamples(Encoding encoding, size_t bytes)
{
	unsigned long sf = getFrame(encoding);
	if(!bytes || !sf)
		return 0;
	unsigned long frames = bytes / sf;
	return frames * getCount(encoding);
}

unsigned long Audio::toBytes(Encoding encoding, unsigned long samples)
{
	unsigned long sc = getCount(encoding);
	if(!samples || !sc)
		return 0;
	unsigned long frames = samples / sc;
	return frames * getFrame(encoding);
}

bool Audio::isMono(Encoding encoding)
{
	switch(encoding)
	{
	case pcm8Stereo:
	case pcm16Stereo:
	case pcm32Stereo:
	case cdaStereo:
		return false;
	default:
		return true;
	}
}

bool Audio::isStereo(Encoding encoding)
{
	return !isMono(encoding);
}

#ifdef	CCXX_NAMESPACES
};
#endif

