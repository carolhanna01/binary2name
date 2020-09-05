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
#include <cstdio>
#include "private.h"
#include "audio.h"

#ifdef HAVE_LINUX_CDROM_H

#include <sys/ioctl.h>
#include <linux/cdrom.h>

#ifdef	CCXX_NAMESPACES
namespace ost {
#endif

CDAudio::CDAudio(int nbr)
{
	char path[32];

	if(nbr)
		snprintf(path, 32, "/dev/cdrom%d", nbr);
	else
		strcpy(path, "/dev/cdrom");

	err = errSuccess;
	file.fd = open(path, O_RDONLY | O_NONBLOCK);
	if(file.fd < 0)
	{
		err = errNotOpened;
		return;
	}
	v0 = getVolumeLeft();
	v1 = getVolumeRight();
}

CDAudio::~CDAudio()
{
	if(file.fd > -1)
	{
		setVolume(v0, v1);
		close(file.fd);
	}
}

bool CDAudio::isOpen(void)
{
	return file.fd > -1;
}

bool CDAudio::isPaused(void)
{
	struct cdrom_subchnl ch;

	if(file.fd < 0)
		return false;

	memset(&ch, 0, sizeof(ch));
	ch.cdsc_format = CDROM_MSF;
	if(ioctl(file.fd, CDROMSUBCHNL, &ch))
	{
		err = errStatFailed;
		return false;
	}
	return ch.cdsc_audiostatus == CDROM_AUDIO_PAUSED;
}

bool CDAudio::isAudio(int track)
{
	struct cdrom_tocentry toc;

	if(file.fd < 0)
		return false;

	memset(&toc, 0, sizeof(toc));
	toc.cdte_track = track;
	toc.cdte_format = CDROM_MSF;
	if(ioctl(file.fd, CDROMREADTOCENTRY, &toc))
	{
		err = errTOCFailed;
		return false;
	}
	return toc.cdte_ctrl & CDROM_DATA_TRACK ? false : true;
}

int CDAudio::getFirst(void)
{
	struct cdrom_tochdr toc;

	if(file.fd < 0)
		return 0;

	if(ioctl(file.fd, CDROMREADTOCHDR, &toc))
	{
		err = errTOCFailed;
		return 0;
	}
	return toc.cdth_trk0;
}

int CDAudio::getLast(void)
{
	struct cdrom_tochdr toc;

	if(file.fd < 0)
		return 0;

	if(ioctl(file.fd, CDROMREADTOCHDR, &toc))
	{
		err = errTOCFailed;
		return 0;
	}
	return toc.cdth_trk1;
}

Audio::Error CDAudio::stop(void)
{
	if(file.fd < 0)
		return errNotOpened;

	ioctl(file.fd, CDROMSTOP);
	return errSuccess;
}

Audio::Error CDAudio::play(int first, int stop)
{
	struct cdrom_tocentry toc0, toc1;
	struct cdrom_msf msf;
	int last = getLast();

	if(file.fd < 0)
		return errNotOpened;

	if(!stop)
		stop = first;

	if(stop < 0)
		stop = last;

	if(stop == last)
		stop = CDROM_LEADOUT;
	else
		++stop;


	memset(&toc0, 0, sizeof(toc0));
	memset(&toc1, 0, sizeof(toc1));
	toc0.cdte_track = first;
	toc1.cdte_track = stop;
	toc0.cdte_format = CDROM_MSF;
	toc1.cdte_format = CDROM_MSF;
	if(ioctl(file.fd, CDROMREADTOCENTRY, &toc0))
	{
		err = errInvalidTrack;
		return err;
	}
	if(ioctl(file.fd, CDROMREADTOCENTRY, &toc1))
	{
		err = errInvalidTrack;
		return err;
	}
	msf.cdmsf_min0 = toc0.cdte_addr.msf.minute;
	msf.cdmsf_sec0 = toc0.cdte_addr.msf.second;
	msf.cdmsf_frame0 = toc0.cdte_addr.msf.frame;
	msf.cdmsf_min1 = toc1.cdte_addr.msf.minute;
	msf.cdmsf_sec1 = toc1.cdte_addr.msf.second;
	msf.cdmsf_frame1 = toc1.cdte_addr.msf.frame;

	if(ioctl(file.fd, CDROMPLAYMSF, &msf))
		err = errPlaybackFailed;
	else
		err = errSuccess;

	return err;
}

Audio::Error CDAudio::pause(void)
{
	if(file.fd < 0)
		return errNotOpened;

	ioctl(file.fd, CDROMPAUSE);
	return errSuccess;
}

Audio::Error CDAudio::resume(void)
{
	if(file.fd < 0)
		return errNotOpened;

	if(!isPaused())
		return errNotPlaying;

	ioctl(file.fd, CDROMRESUME);
	return errSuccess;
}

Audio::Error CDAudio::eject(void)
{
	if(file.fd < 0)
		return errNotOpened;

	ioctl(file.fd, CDROMEJECT);
	return errSuccess;
}

Audio::Error CDAudio::reload(void)
{
	if(file.fd < 0)
		return errNotOpened;

	ioctl(file.fd, CDROMCLOSETRAY);
	return errSuccess;
}

unsigned char CDAudio::getVolume(int speaker)
{
	struct cdrom_volctrl vol;

	if(file.fd < 0)
		return 0;

	ioctl(file.fd, CDROMVOLREAD, &vol);
	if(speaker)
		return (unsigned char)vol.channel1;
	else
		return (unsigned char)vol.channel0;
}

void CDAudio::setVolume(unsigned char left, unsigned char right)
{
	struct cdrom_volctrl vol;

	vol.channel0 = left;
	vol.channel1 = right;
	vol.channel2 = 0;
	vol.channel3 = 0;

	ioctl(file.fd, CDROMVOLCTRL, &vol);
}

#ifdef	CCXX_NAMESPACES
};
#endif

#endif

