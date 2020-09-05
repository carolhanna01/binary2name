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
#include <cmath>
#include "private.h"
#include "audio.h"

#ifndef	M_PI
#define	M_PI	3.14159265358979323846
#endif

#ifdef	CCXX_NAMESPACES
namespace ost {
#endif

AudioSample::AudioSample(unsigned size, Encoding codec, unsigned srate)
{
	if(!srate)
		srate = Audio::getRate(codec);


	encoding = codec;
	count = size;
	rate = srate;
	samples = new unsigned char[toBytes(codec, size)];
}

AudioSample::~AudioSample()
{
	if(samples)
		delete[] samples;
}

AudioTone::AudioTone(unsigned size, unsigned f1, unsigned f2, unsigned rate) :
AudioSample(size, pcm16Mono, rate)
{
	setFreq(f1, f2);
	v1 = v2 = 8000.;
	p1 = p2 = 0;

	if(!f1 && !f2)
		return;

	fill();
}

void AudioTone::fill(unsigned max)
{
	short *data = (short *)getSamples();
	unsigned i;

	if(!max)
		max = count;

	if(max > count)
		max = count;

	for(i = 0; i < max; ++i)
	{
		*(data++) = (short)((sin(p1) * v1 + sin(p2) * v2));
		p1 += fa1;
		p2 += fa2;
	}

	for(i = max; i < count; ++i)
		*(data++) =0;
}

void AudioTone::setFreq(unsigned f1, unsigned f2)
{
	fa1 = (f1 * M_PI * 2) / getRate();
	fa2 = (f2 * M_PI * 2) / getRate();
}

AudioCopy::AudioCopy(unsigned size, Encoding coding, unsigned rate) :
AudioSample(size, coding, rate)
{
	left = 0;
	next = NULL;
};

bool AudioCopy::copy(void)
{
	unsigned char *data = samples;
	int bcount = toBytes(encoding, count);
	bool ret;

	if(left)
		ret = true;
	else
		ret = false;

	while(bcount)
	{
		if(!left)
		{
			AudioSample *au = fill();
			if(au)
			{
				if(au->encoding != encoding)
					continue;
				ret = true;
				left = toBytes(au->encoding, au->count);
				next = au->samples;
			}
			else
			{

				left = 0;
				next = NULL;
				ret = false;
				break;
			}
		}
		*(data++) = *(next++);
		--left;
		--bcount;
	}

	while(bcount-- && !left)
		*(data++) = 0;

	return ret;
}

#ifdef	CCXX_NAMESPACES
};
#endif
