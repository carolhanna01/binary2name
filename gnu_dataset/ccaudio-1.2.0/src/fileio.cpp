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
#include <cc++/export.h>
#include <cstdlib>
#include <cstdio>
#include "private.h"
#include "audio.h"

#ifdef  __WIN32__
#define FD(x)   ((HANDLE)(x.handle))
#define SETFD(x,y) (x.handle = (void *)(y))
#ifndef	INVALID_SET_FILE_POINTER
#define	INVALID_SET_FILE_POINTER	((DWORD)(-1))
#endif
#endif

#ifdef	CCXX_NAMESPACES
namespace ost {
#endif

bool AudioFile::afCreate(const char *name)
{
	AudioFile::close();
#ifdef	__WIN32__
	SETFD(file, CreateFile(name, GENERIC_READ | GENERIC_WRITE, 0,
		NULL, TRUNCATE_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL));
#else
	// JRS: do not use creat() here.  It uses O_RDONLY by default
	// which prevents us from reading the wave header later on when 
	// setting the length during AudioFile::Close().
	file.fd = ::open(name, O_CREAT | O_TRUNC | O_RDWR, 0660);
#endif
	return isOpen();
}

bool AudioFile::afOpen(const char *name)
{
	AudioFile::close();
#ifdef	__WIN32__
	SETFD(file, CreateFile(name, GENERIC_READ | GENERIC_WRITE, 0,
		NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL));
	if(!isOpen())
		SETFD(file, CreateFile(name,GENERIC_READ, 0,
                        NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL));
#else
	file.fd = ::open(name, O_RDWR);
	if(file.fd < 0)
		file.fd = ::open(name, O_RDONLY);
#endif
	return isOpen();
}
int AudioFile::afWrite(unsigned char *data, unsigned len)
{
	modified = true;

#ifdef	__WIN32__
	DWORD count;

	if(!WriteFile(FD(file), data, (DWORD)len, &count, NULL))
		return -1;
	return count;
#else
	return ::write(file.fd, data, len);
#endif
}

int AudioFile::afRead(unsigned char *data, unsigned len)
{
#ifdef	__WIN32__
	DWORD count;

	if(!ReadFile(FD(file), data, (DWORD)len, &count, NULL))
		return -1;
	return count;
#else
	return ::read(file.fd, data, len);
#endif
}

bool AudioFile::afPeek(unsigned char *data, unsigned len)
{
	if(afRead(data, len) != (int)len)
		return false;
	return true;
}

bool AudioFile::afSeek(unsigned long pos)
{
#ifdef	__WIN32__
	if(SetFilePointer(FD(file), pos, NULL, FILE_BEGIN) != INVALID_SET_FILE_POINTER)
#else
	if(::lseek(file.fd, pos, SEEK_SET) != -1)	
#endif	
		return true;
	else
		return false;
}

bool AudioFile::isOpen(void)
{
#ifdef	__WIN32__
	if(FD(file) == INVALID_HANDLE_VALUE)
		return false;
#else
	if(file.fd < 0)
		return false;
#endif
	return true;
}

void AudioFile::afClose(void)
{
	unsigned long size = ~0;
#ifdef	__WIN32__
	if(FD(file) != INVALID_HANDLE_VALUE)
	{
		size = getPosition();
		CloseHandle(FD(file));
		if(size < minimum && pathname)
			DeleteFile(pathname);
	}
	SETFD(file, INVALID_HANDLE_VALUE);
#else
	if(file.fd > -1)
	{
		size = getPosition();
		if(size < minimum && pathname)
			::remove(pathname);
		::close(file.fd);
	}
	file.fd = -1;
#endif
}

void AudioFile::initialize(void)
{
	minimum = 0;
	pathname = NULL;
	info.annotation = NULL;
	header = 0l;
#ifdef	__WIN32__
	SETFD(file, INVALID_HANDLE_VALUE);
#else
	file.fd = -1;
#endif
}

Audio::Error AudioFile::setPosition(unsigned long samples)
{
        long pos;
        long eof;

        if(!isOpen())
                return errNotOpened;

#ifdef  __WIN32__
        eof = SetFilePointer(FD(file), 0l, NULL, FILE_END);
#else
        eof = ::lseek(file.fd, 0l, SEEK_END);
#endif
        if(samples == (unsigned long)~0l)
                return errSuccess;

        pos = header + toBytes(info.encoding, samples);
        if(pos > eof)
        {
                pos = eof;
                return errSuccess;
        }

#ifdef  __WIN32__
        SetFilePointer(FD(file), pos, NULL, FILE_BEGIN);
#else
        ::lseek(file.fd, pos, SEEK_SET);
#endif
        return errSuccess;
}

unsigned long AudioFile::getAbsolutePosition(void)
{
        unsigned long pos;
        if(!isOpen())
                return 0;
 
#ifdef  __WIN32__
        pos = SetFilePointer(FD(file), 0l, NULL, FILE_CURRENT);
	if(pos == INVALID_SET_FILE_POINTER) {
#else
        pos = ::lseek(file.fd, 0l, SEEK_CUR);
	if(pos == (unsigned long)-1l) {
#endif
		close();
		return 0;
	}
        return pos;
}

unsigned long AudioFile::getPosition(void)
{
        unsigned long pos;
        if(!isOpen())
                return 0;

#ifdef  __WIN32__
        pos = SetFilePointer(FD(file), 0l, NULL, FILE_CURRENT);
	if(pos == INVALID_SET_FILE_POINTER) {
#else
        pos = getAbsolutePosition();
	if(pos == (unsigned long)-1l) {
#endif
		close();
		return 0;
	}
        pos = toSamples(info.encoding, pos - header);
        return pos;
}

#ifdef	CCXX_NAMESPACES
};
#endif
