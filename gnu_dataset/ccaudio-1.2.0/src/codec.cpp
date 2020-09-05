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
#include <cc++/strchar.h>
#include <cc++/file.h>
#include <cc++/export.h>
#include <math.h>
#include "private.h"
#include "audio.h"

#ifndef	M_PI
#define	M_PI	3.14159265358979323846
#endif

#ifdef	CCXX_NAMESPACES
namespace ost {
#endif

Mutex AudioCodec::lock;
AudioCodec *AudioCodec::first = NULL;

AudioCodec::AudioCodec(const char *n, Encoding e)
{
	encoding = e;
	name = n;
	next = first;
	first = this;
}

bool AudioCodec::load(const char *name)
{
	char path[256];
	bool success = true;

	snprintf(path, sizeof(path), "%s/%s", AUDIO_MODULE_PATH, name);             
	try
        {
               new DSO(path);
        }
        catch(DSO *dso)
        {
               success = false;
               return false;
        }
	return success;
}

AudioCodec *AudioCodec::getCodec(Encoding e, const char *name)
{
	bool success;
	AudioCodec *codec;
	lock.enterMutex();

	codec = first;

	while(codec)
	{
		if(e == codec->encoding)
			break;
		codec = codec->next;
	}

#ifdef	AUDIO_MODULE_PATH
	if(!codec && name)
	{
		success = load(name);
		lock.leaveMutex();
		if(success)
			return getCodec(e, NULL);
		else
			return NULL;
	}
#endif

	lock.leaveMutex();

	return codec;
}

#ifdef	CCXX_NAMESPACES
};
#endif
