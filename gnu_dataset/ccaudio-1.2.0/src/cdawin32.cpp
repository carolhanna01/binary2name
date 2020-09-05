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
#include <cstdlib>
#include <cstdarg>
#include <cstdio>
#include "private.h"
#include "audio.h"

#if	defined(HAVE_CDAUDIO_WIN32) || defined(__WIN32__)

#include <mmsystem.h>

#ifdef	CCXX_NAMESPACES
namespace ost {
#endif

DWORD CDAudio::command(char *fmt, ...)
{
	char buf[256];
	va_list ap;
	
	va_start(ap, fmt);
	vsprintf(buf, fmt, ap);
	va_end(ap);

	ret[0] = 0;
	return mciSendString(buf, ret, 256, 0);
}

CDAudio::CDAudio(int nbr)
{
	err = errSuccess;
	opened = false;

	if(command("open cdaudio wait"))
		throw(this);

	opened = true;
	command("set cdaudio time format tmsf");
	paused = false;
	InitializeCriticalSection(&crit);
}

CDAudio::~CDAudio()
{
	DeleteCriticalSection(&crit);
}

bool CDAudio::isOpen(void)
{
	return opened;
}

bool CDAudio::isPaused(void)
{
	return paused;
}

bool CDAudio::isAudio(int track)
{
	bool flag = false;

	EnterCriticalSection(&crit);
	if(!command("status cdaudio type track %u", track))
		if(!strcmp(ret, "audio"))
			flag = true;
	LeaveCriticalSection(&crit);
	return flag;
}

int CDAudio::getFirst(void)
{
	DWORD rtn;
	int tracks;
	if(!opened)
		return 0;

	EnterCriticalSection(&crit);
	rtn = command("status cdaudio number of tracks");
	tracks = atoi(ret);
	LeaveCriticalSection(&crit);
	if(rtn)
		return 0;
	return 1;
}

int CDAudio::getLast(void)
{
	DWORD rtn;
	int tracks;
	if(!opened)
		return 0;

	EnterCriticalSection(&crit);
	rtn = command("status cdaudio number of tracks");
	tracks = atoi(ret);
	LeaveCriticalSection(&crit);
	if(rtn)
		return 0;
	return tracks;
}

Audio::Error CDAudio::stop(void)
{
	if(!opened)
		return errNotOpened;

	EnterCriticalSection(&crit);
	command("stop cdaudio wait");
	paused = false;
	LeaveCriticalSection(&crit);
	return errSuccess;
}

Audio::Error CDAudio::play(int first, int stop)
{
	DWORD rtn;
	int start;
	if(!stop)
		stop = first;

	if(stop < 0)
		stop = getLast();

	CDAudio::stop();
	start = MCI_MAKE_TMSF(first, 0, 0, 0);
	EnterCriticalSection(&crit);
	if(command("status cdaudio length track %u", stop))
	{
		LeaveCriticalSection(&crit);
		err = errInvalidTrack;
		return err;
	}
	sprintf(endmark, "%u:%s", stop, ret);
	rtn = command("play cdaudio from %lu to %s", start, endmark);
	LeaveCriticalSection(&crit);
	if(rtn)
		err = errPlaybackFailed;
	else
		err = errSuccess;
	return err;
}

Audio::Error CDAudio::pause(void)
{
	if(!opened)
		return errNotOpened;

	EnterCriticalSection(&crit);
	mciSendString("status cdaudio position", position, 20, 0);
	command("pause cdaudio");
	paused = true;
	LeaveCriticalSection(&crit);
	return errSuccess;
}

Audio::Error CDAudio::resume(void)
{
	DWORD rtn;

	if(!opened)
		return errNotOpened;

	if(!paused)
		return errSuccess;

	EnterCriticalSection(&crit);
	rtn = command("play cdaudio from %s to %s", position, endmark);
	paused = false;
	LeaveCriticalSection(&crit);
	if(rtn)
		return errPlaybackFailed;
	else
		return errSuccess;
}

Audio::Error CDAudio::eject(void)
{
	if(!opened)
		return errNotOpened;

	EnterCriticalSection(&crit);
	command("set cdaudio door open");
	paused = false;
	LeaveCriticalSection(&crit);
	return errSuccess;
}

Audio::Error CDAudio::reload(void)
{
	if(!opened)
		return errNotOpened;

	EnterCriticalSection(&crit);
	command("set cdaudio door closed");
	paused = false;
	LeaveCriticalSection(&crit);
	return errSuccess;
}

unsigned char CDAudio::getVolume(int speaker)
{
	return 128;
}

void CDAudio::setVolume(unsigned char left, unsigned char right)
{}

#ifdef	CCXX_NAMESPACES
};
#endif
			
#endif

