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
#include "private.h"
#include "audio.h"

#ifdef	CCXX_NAMESPACES
namespace ost {
#endif

static const char * const ErrorStrs[] = {
	"errSuccess",
	"errReadLast",
	"errNotOpened",
	"errEndOfFile",
	"errStartOfFile",
	"errRateInvalid",
	"errEncodingInvalid",
	"errReadInterrupt",
	"errWriteInterrupt",
	"errReadFailure",
	"errWriteFailure",
	"errReadIncomplete",
	"errWriteIncomplete",
	"errRequestInvalid",
	"errTOCFailed",
	"errStatFailed",
	"errInvalidTrack",
	"errPlaybackFailed",
	"errNotPlaying"
};

AudioCodec *AudioFile::getCodec(void)
{
	Encoding e = getEncoding();
	switch(e)
	{
	case alawAudio:
		return AudioCodec::getCodec(e, "g.711");
	case mulawAudio:
		return AudioCodec::getCodec(e, "g.711");
	case g721ADPCM:
	case okiADPCM:
	case voxADPCM:
		return AudioCodec::getCodec(e, "g.721");
	case g722_7bit:
	case g722_6bit:
		return AudioCodec::getCodec(e, "g.722");
	case g723_3bit:
	case g723_5bit:
		return AudioCodec::getCodec(e, "g.723");
	default:
		return NULL;
	}
}

void AudioFile::setShort(unsigned char *data, unsigned short val)
{
	if(info.order == __BIG_ENDIAN)
	{
		data[0] = val / 256;
		data[1] = val % 256;
	}
	else
	{
		data[1] = val / 256;
		data[0] = val % 256;
	}
}

unsigned short AudioFile::getShort(unsigned char *data)
{
	if(info.order == __BIG_ENDIAN)
		return data[0] * 256 + data[1];
	else
		return data[1] * 256 + data[0];
}

void AudioFile::setLong(unsigned char *data, unsigned long val)
{
	int i = 4;

	while(i-- > 0)
	{
		if(info.order == __BIG_ENDIAN)
			data[i] = val & 0xff;
		else
			data[3 - i] = val & 0xff;
		val /= 256;
	}
}

unsigned long AudioFile::getLong(unsigned char * data)
{
	int i = 4;
	unsigned long val =0;

	while(i-- > 0)
	{
		if(info.order == __BIG_ENDIAN)
			val = (val << 8) | data[3 - i];
		else
			val = (val << 8) | data[i];
	}
	return val;
}

AudioFile::AudioFile(const char *name, unsigned long sample)
{
	memset(&this->info, 0, sizeof(this->info));
	pathname = NULL;
	initialize();
	AudioFile::open(name);
	if(!isOpen())
		return;
	setPosition(sample);
	limit = 0l;
}

AudioFile::AudioFile(const char *name, Info *info, unsigned long samples)
{
	memset(&this->info, 0, sizeof(this->info));
	pathname = NULL;
	initialize();
	AudioFile::create(name, info);
	if(!isOpen())
		return;
	setMinimum(samples);
	limit = 0l;
}

void AudioFile::create(const char *name, Info *myinfo)
{
	int pcm = 0;
	unsigned char aufile[24];
	unsigned char riffhdr[40];

	modified = true;

	if(!afCreate(name))
		return;

	memset(riffhdr, 0, sizeof(riffhdr));
	memcpy(&info, myinfo, sizeof(info));
	info.annotation = NULL;
	pathname = new char[strlen(name) + 1];
	strcpy(pathname, name);
	if(myinfo->annotation)
	{
		info.annotation = new char[strlen(myinfo->annotation) + 1];
		strcpy(info.annotation, myinfo->annotation);
	}

	switch(info.format)
	{
	case wave:
	case riff:
		/* 
		 * RIFF header
		 *
		 * 12 bytes
		 *
		 * 0-3: RIFF magic "RIFF" (0x52 49 46 46)
		 *
		 * 4-7: RIFF header chunk size 
		 *      (4 + (8 + subchunk 1 size) + (8 + subchunk 2 size))
		 *
		 * 8-11: WAVE RIFF type magic "WAVE" (0x57 41 56 45)
		 */
		if(!info.order)
			info.order = __LITTLE_ENDIAN;
		if(info.order == __LITTLE_ENDIAN)
			strncpy((char *)riffhdr, "RIFF", 4);
		else
			strncpy((char *)riffhdr, "RIFX", 4);
		if(!info.rate)
			info.rate = Audio::getRate(info.encoding);

		header = 0;

		memset(riffhdr + 4, 0xff, 4);
		strncpy((char *)riffhdr + 8, "WAVE", 4);
		if(afWrite(riffhdr, 12) != 12)
		{
			AudioFile::close();
			return;
		}

		/* 
		 * Subchunk 1: WAVE metadata
		 *
		 * Length: 24+
		 *
		 * (offset from start of subchunk 1) startoffset-endoffset
		 *
		 * (0) 12-15: WAVE metadata magic "fmt " (0x 66 6d 74 20)
		 *
		 * (4) 16-19: subchunk 1 size minus 8.  0x10 for PCM.
		 *
		 * (8) 20-21: Audio format code.  0x01 for linear PCM.
		 * More codes here:
		 * http://www.goice.co.jp/member/mo/formats/wav.html
		 *
		 * (10) 22-23: Number of channels.  Mono = 0x01,
		 * Stereo = 0x02.
		 *
		 * (12) 24-27: Sample rate in samples per second. (8000, 
		 * 44100, etc)
		 *
		 * (16) 28-31: Bytes per second = SampleRate *
		 * NumChannels * (BitsPerSample / 8)
		 *
		 * (20) 32-33: Block align (the number of bytes for
		 * one multi-channel sample) = Numchannels *
		 * (BitsPerSample / 8)
		 *
		 * (22) 34-35: Bits per single-channel sample.  8 bits
		 * per channel sample = 0x8, 16 bits per channel
		 * sample = 0x10
		 *
		 * (24) 36-37: Size of extra PCM parameters for
		 * non-PCM formats.  If a PCM format code is
		 * specified, this doesn't exist.
		 *
		 * Subchunk 3: Optional 'fact' subchunk for non-PCM formats
		 * (26) 38-41: WAVE metadata magic 'fact' (0x 66 61 63 74)
		 * 
		 * (30) 42-45: Length of 'fact' subchunk, not
		 * including this field and the fact
		 * identification field (usually 4)
		 *
		 * (34) 46-49: ??? sndrec32.exe outputs 0x 00 00 00 00
		 * here.  See 
		 * http://www.health.uottawa.ca/biomech/csb/ARCHIVES/riff-for.txt
		 *
		 */

		memset(riffhdr, 0, sizeof(riffhdr));
		strncpy((char *)riffhdr, "fmt ", 4);
		// FIXME A bunch of the following only works for PCM
		// and mulaw/alaw, so you'll probably have to fix it
		// if you want to use one of the other formats.
		if(info.encoding < cdaStereo)
			setLong(riffhdr + 4, 18);
		else
			setLong(riffhdr + 4, 16);

		setShort(riffhdr + 8, 0x01); // default in case invalid encoding specified
		if(isMono(info.encoding))
			setShort(riffhdr + 10, 1);
		else
			setShort(riffhdr + 10, 2);
		setLong(riffhdr + 12, info.rate);
		setLong(riffhdr + 16, toBytes(info.encoding, info.rate));
		setShort(riffhdr + 20, toBytes(info.encoding, 1));
		setShort(riffhdr + 22, 0);

		switch(info.encoding)
		{
		case pcm8Mono:
		case pcm8Stereo:
			setShort(riffhdr + 22, 8);
			pcm = 1;
			break;
		case pcm16Mono:
		case pcm16Stereo:
		case cdaMono:
		case cdaStereo:
			setShort(riffhdr + 22, 16);
			pcm = 1;
			break;
		case pcm32Mono:
		case pcm32Stereo:
			setShort(riffhdr + 22, 32);
			pcm = 1;
			break;
		case alawAudio:
			setShort(riffhdr + 8, 6);
			setShort(riffhdr + 22, 8);
			break;
		case mulawAudio:
			setShort(riffhdr + 8, 7);
			setShort(riffhdr + 22, 8);
			break;

			// FIXME I'm pretty sure these are supposed to
			// be writing to offset 22 instead of 24...
		case okiADPCM:
			setShort(riffhdr + 8, 0x10);
			setShort(riffhdr + 24, 4);
			break;
		case voxADPCM:
			setShort(riffhdr + 8, 0x17);
			setShort(riffhdr + 24, 4);
			break;
		case g721ADPCM:
			setShort(riffhdr + 8, 0x40);
			setShort(riffhdr + 24, 4);
			break;
		case g722Audio:
			setShort(riffhdr + 8, 0x64);
			setShort(riffhdr + 24, 8);
			break;
		case gsmVoice:
			setShort(riffhdr + 8, 0x31);
			setShort(riffhdr + 24, 260);
			break;
		case g723_3bit:
			setShort(riffhdr + 8, 0x14);
			setShort(riffhdr + 24, 3);
			break;
		case g723_5bit:
			setShort(riffhdr + 8, 0x14);
			setShort(riffhdr + 24, 5);
		default:
			break;
		}

		if(pcm == 0) {
			setShort(riffhdr + 24, 0);
			strncpy((char *)(riffhdr + 26), "fact", 4);
			setLong(riffhdr + 30, 4);
			setLong(riffhdr + 34, 0);
			if(afWrite(riffhdr, 38) != 38)
			{
				AudioFile::close();
				return;
			}
		}
		else {
			if(afWrite(riffhdr, 24) != 24)
			{
				AudioFile::close();
				return;
			}
		}
		
		/* 
		 * Subchunk 2: data subchunk
		 *
		 * Length: 8+
		 *
		 * (0) 36-39: data subchunk magic "data" (0x 64 61 74 61)
		 *
		 * (4) 40-43: subchunk 2 size = 
		 * NumSamples * NumChannels * (BitsPerSample / 8)
		 * Note that this does not include the size of this
		 * field and the previous one.
		 *
		 * (8) 44+: Samples
		 */

		memset(riffhdr, 0, sizeof(riffhdr));
		strncpy((char *)riffhdr, "data", 4);
		memset(riffhdr + 4, 0xff, 4);
		if(afWrite(riffhdr, 8) != 8)
		{
			AudioFile::close();
			return;
		}
		
		header = getAbsolutePosition();
		length = getAbsolutePosition();
		break;

	case sun:
		if(!info.order)
			info.order = __BIG_ENDIAN;

		if(!info.rate)
			info.rate = Audio::getRate(info.encoding);

		strncpy((char *)aufile, ".snd", 4);
		if(info.annotation)
			setLong(aufile + 4, 24 + strlen(info.annotation) + 1);
		else
			setLong(aufile + 4, 24);
		header = getLong(aufile + 4);
		setLong(aufile + 8, ~0l);
		switch(info.encoding)
		{
		case pcm8Stereo:
		case pcm8Mono:
			setLong(aufile + 12, 2);
			break;
		case pcm16Stereo:
		case pcm16Mono:
		case cdaStereo:
		case cdaMono:
			setLong(aufile + 12, 3);
			break;
		case pcm32Stereo:
		case pcm32Mono:
			setLong(aufile + 12, 5);
			break;
		case g721ADPCM:
			setLong(aufile + 12, 23);
			break;
		case g722Audio:
		case g722_7bit:
		case g722_6bit:
			setLong(aufile + 12, 24);
			break;
		case g723_3bit:
			setLong(aufile + 12, 25);
			break;
		case g723_5bit:
			setLong(aufile + 12, 26);
			break;
		case alawAudio:
			setLong(aufile + 12, 27);
			break;
		default:
			setLong(aufile + 12, 1);
		}
		setLong(aufile + 16, info.rate);
		if(isMono(info.encoding))
			setLong(aufile + 20, 1);
		else
			setLong(aufile + 20, 2);
		if(afWrite(aufile, 24) != 24)
		{
			AudioFile::close();
			return;
		}
		if(info.annotation)
			afWrite((unsigned char *)info.annotation,
				strlen(info.annotation) + 1);
		header = getAbsolutePosition();
		length = getAbsolutePosition();
	default:
		break;
	}
}

void AudioFile::getWaveFormat(int request)
{
	unsigned char filehdr[24];
	int bitsize;
	int channels;

	if(request > 24)
		request = 24;

	if(!afPeek(filehdr, request))
	{
		AudioFile::close();
		return;
	}
	channels = getShort(filehdr + 2);
	info.rate = getLong(filehdr + 4);

	switch(getShort(filehdr))
	{
	case 1:
		bitsize = getShort(filehdr + 14);
		switch(bitsize)
		{
		case 8:
			if(channels > 1)
				info.encoding = pcm8Stereo;
			else
				info.encoding = pcm8Mono;
			break;
		case 16:
			if(info.rate == 44100)
			{
				if(channels > 1)
					info.encoding = cdaStereo;
				else
					info.encoding = cdaMono;
				break;
			}
			if(channels > 1)
					info.encoding = pcm16Stereo;
			else
				info.encoding = pcm16Mono;
			break;
		case 32:
			if(channels > 1)
				info.encoding = pcm32Stereo;
			else
				info.encoding = pcm32Mono;
			break;
		default:
			info.encoding = unknownEncoding;
		}
		break;
	case 6:
		info.encoding = alawAudio;
		break;
	case 7:
		info.encoding = mulawAudio;
		break;
	case 0x10:
		info.encoding = okiADPCM;
		break;
	case 0x17:
		info.encoding = voxADPCM;
		break;
	case 0x40:
		info.encoding = g721ADPCM;
		break;
	case 0x65:
		info.encoding = g722Audio;
		break;
	case 0x31:
		info.encoding = gsmVoice;
		break;
	case 0x14:
		bitsize = getLong(filehdr + 8) * 8 / info.rate;
		if(bitsize == 3)
			info.encoding = g723_3bit;
		else
			info.encoding = g723_5bit;
		break;
	default:
		info.encoding = unknownEncoding;
	}
}

void AudioFile::open(const char *name)
{
	unsigned char filehdr[24];
	unsigned int count;
	char *ext;
	unsigned channels;

	modified = false;

	if(!afOpen(name))
		return;

	pathname = new char[strlen(name) + 1];
	strcpy(pathname, name);
	header = 0l;

	info.encoding = mulawAudio;
	info.format = raw;
	info.order = 0;
	ext = strrchr(pathname, '.');
	if(!ext)
		return;

	if(!stricmp(ext, ".ul"))
	{
		info.encoding = mulawAudio;
		return;
	}

	if(!stricmp(ext, ".al"))
	{
		info.encoding = alawAudio;
		return;
	}

	if(!stricmp(ext, ".sw") || !stricmp(ext, ".raw"))
	{
		info.encoding = pcm16Mono;
		return;
	}

	if(!stricmp(ext, ".vox"))
	{
		info.encoding = voxADPCM;
		return;
	}

	if(!stricmp(ext, ".adpcm"))
	{
		info.encoding = g721ADPCM;
		return;
	}

	if(!stricmp(ext, ".a24"))
	{
		info.encoding = g723_3bit;
		return;
	}

	if(!stricmp(ext, ".a40"))
	{
		info.encoding = g723_5bit;
		return;
	}

	strcpy((char *)filehdr, ".xxx");

	if(!afPeek(filehdr, 24))
	{
		AudioFile::close();
		return;
	}

	if(!strncmp((char *)filehdr, "RIFF", 4))
	{
		info.format = riff;
		info.order = __LITTLE_ENDIAN;
	}

	if(!strncmp((char *)filehdr, "RIFX", 4))
	{
		info.order = __BIG_ENDIAN;
		info.format = riff;
	}

	if(!strncmp((char *)filehdr + 8, "WAVE", 4) &&
	   info.format == riff)
	{
		header = 12;
		for(;;)
		{
			if(!afSeek(header))
			{
				AudioFile::close();
				return;
			}
			if(!afPeek(filehdr, 8))
			{
				AudioFile::close();
				return;
			}
			header += 8;
			if(!strncmp((char *)filehdr, "data", 4))
				break;

			count = getLong(filehdr + 4);
			header += count;
			if(!strncmp((char *)filehdr, "fmt ", 4))
				getWaveFormat(count);

		}
		afSeek(header);
		return;
	}
	if(!strncmp((char *)filehdr, ".snd", 4))
	{
		info.format = sun;
		info.order = __BIG_ENDIAN;
		header = getLong(filehdr + 4);
		info.rate = getLong(filehdr + 16);
		channels = getLong(filehdr + 20);

		switch(getLong(filehdr + 12))
		{
		case 2:
			if(channels > 1)
				info.encoding = pcm8Stereo;
			else
				info.encoding = pcm8Mono;
			break;
		case 3:
			if(info.rate == 44100)
			{
				if(channels > 1)
					info.encoding = cdaStereo;
				else
					info.encoding = cdaMono;
				break;
			}
			if(channels > 1)
				info.encoding = pcm16Stereo;
			else
				info.encoding = pcm16Mono;
			break;
		case 5:
			if(channels > 1)
				info.encoding = pcm32Stereo;
			else
				info.encoding = pcm32Mono;
			break;
		case 23:
			info.encoding = g721ADPCM;
			break;
		case 24:
			info.encoding = g722Audio;
			break;
		case 25:
			info.encoding = g723_3bit;
			break;
		case 26:
			info.encoding = g723_5bit;
			break;
		case 27:
			info.encoding = alawAudio;
			break;
		case 1:
			info.encoding = mulawAudio;
			break;
		default:
			info.encoding = unknownEncoding;
		}
		if(header > 24)
		{
			info.annotation = new char[header - 24];
			afSeek(24);
			afRead((unsigned char *)info.annotation, header - 24);
		}
		return;
	}
	else
		afSeek(0);
}


void AudioFile::close(void)
{
	struct stat ino;
	unsigned char buf[58];
	if(! isOpen())
		return;

	if(! modified)
	{
		afClose();
		return;
	}

	if(! afSeek(0)) {
		return;
	}

	if(afRead(buf, 58) < 58) {
		afClose();
		return;
	}
	afSeek(0);

	switch(info.format) {
	case riff:
	case wave:
		fstat(file.fd, &ino);
		length = ino.st_size;

		// RIFF header
		setLong(buf + 4, length - 8);

		// If it's a non-PCM datatype, the offsets are a bit
		// different for subchunk 2.
		if(info.encoding < cdaStereo)
			setLong(buf + 54, length - header);
		else
			setLong(buf + 40, length - header); 

		if(afWrite(buf, 58) != 58)
		{
			afClose();
			return;
		}

		break;
	default:
		// FIXME not implemented for sun .au
		break;
	}
	afClose();
	clear();
}

void AudioFile::clear(void)
{
	if(pathname)
	{
		delete[] pathname;
		pathname = NULL;
	}
	if(info.annotation)
	{
		delete[] info.annotation;
		info.annotation = NULL;
	}
	minimum = 0;
	limit = 0;
}

Audio::Error AudioFile::getInfo(Info *infobuf)
{
	if(!isOpen())
		return setError(errNotOpened);

	if(!infobuf)
		return setError(errRequestInvalid);

	memcpy(infobuf, &info, sizeof(info));
	return errSuccess;
}

Audio::Error AudioFile::setError(Error err)
{
	if(err)
		error = err;
	return err;
}

const char * AudioFile::getErrorStr(Error err)
{
	return ErrorStrs[err];
}

Audio::Error AudioFile::setMinimum(unsigned long samples)
{
	if(!isOpen())
		return setError(errNotOpened);
	minimum = samples;
	return errSuccess;
}

unsigned AudioFile::getLinear(Linear addr, unsigned samples)
{
	int count;
	AudioCodec *codec;

	if(getEncoding() == pcm16Mono)
	{
		count = getBuffer(addr, samples * 2);
		if(count < 0)
			return 0;
		return count / 2;
	}

	codec = getCodec();
	if(!codec)
		return 0;

	count = getCount(info.encoding);
	samples = (samples / count) * count;
	count = toBytes(info.encoding, samples);
	
	char buffer[count];
	count = getBuffer(buffer, count);
	if(count < 1)
		return 0;

	samples = toSamples(info.encoding, count);
	return codec->decode(addr, buffer, samples);
}

unsigned AudioFile::putLinear(Linear addr, unsigned samples)
{
	int count;
	AudioCodec *codec;

	if(getEncoding() == pcm16Mono)
	{
		count = putBuffer(addr, samples * 2);
		if(count < 0)
			return 0;
		return count / 2;
	}
	codec = getCodec();
	if(!codec)
		return 0;

	count = getCount(info.encoding);
	samples = samples / count * count;
	count = toBytes(info.encoding, samples);
	
	char buffer[count];
	samples = codec->encode(addr, buffer, samples);
	if(samples < 1)
		return 0;
	count = toBytes(info.encoding, samples);
	count = putBuffer(buffer, count);
	if(count < 0)
		return 0;
	return toSamples(info.encoding, count);
}


int AudioFile::getBuffer(void *addr, unsigned bytes)
{
	char *fname;
	unsigned char *caddr = (unsigned char *)addr;
	int count, curpos, xfer = 0;

        curpos = toBytes(info.encoding, getPosition());
        if (limit && ((curpos + bytes) > limit))
                bytes = limit - curpos;
        if (bytes < 0)
                bytes = 0;

	for(;;)
	{
		count = afRead(caddr, bytes);
		if(count < 0)
		{
			if(!xfer)
				return count;
			return xfer;
		}
		xfer += count;
		if(count == (int)bytes)
			return xfer;
		fname = getContinuation();
		if(!fname)
			return xfer;
		AudioFile::close();
		AudioFile::open(fname);
		if(!isOpen())
			return xfer;
		bytes -= count;
		caddr += count;
	}
}

Audio::Error AudioFile::getSamples(void *addr, unsigned request)
{
	char *fname;
	unsigned char *caddr = (unsigned char *)addr;
	int count;
	for(;;)
	{
		int bytes = toBytes(info.encoding, request);
		if(bytes < 1)
			return setError(errRequestInvalid);
		count = afRead(caddr, bytes);
		if(count == bytes)
			return errSuccess;

		if(count < 0)
			return errReadFailure;

		if(count > 0)
		{
			caddr += count;
			count = request - toSamples(info.encoding, count);
		}
		else
			count = request;

		fname = getContinuation();
		if(!fname)
			break;

		AudioFile::close();
		AudioFile::open(fname);
		if(!isOpen())
			break;

		request = count;
	}
	if(count)
		Audio::fill(caddr, count, info.encoding);
	return errReadIncomplete;
}

int AudioFile::putBuffer(void *addr, unsigned len)
{
	int count;

	count = afWrite((unsigned char *)addr, len);
	if(count == (int)len) {
		length += count;
		return count;
	}
	if(count < 1)
		return count;
	else {
 		length += count;
		return count;
	}
}

Audio::Error AudioFile::putSamples(void *addr, unsigned samples)
{
	int count;
	int bytes = toBytes(info.encoding, samples);
	if(bytes < 1)
		return setError(errRequestInvalid);

	count = afWrite((unsigned char *)addr, bytes);
	if(count == bytes) {
		length += count;
		return errSuccess;
	}
	if(count < 1)
		return errWriteFailure;
	else {
		length += count;
		return errWriteIncomplete;
	}
}

Audio::Error AudioFile::skip(long samples)
{
	unsigned long orig = getPosition();
	unsigned long pos = orig + samples;
	if(pos < 0)
		pos = 0;

	setPosition(pos);
	if(orig < getPosition())
		length += (getPosition() - orig);

	return errSuccess;
}

Audio::Error AudioFile::setLimit(unsigned long samples)
{
	if(!isOpen())
		return setError(errNotOpened);

	if(!samples)
	{
		limit = 0;
		return errSuccess;
	}

	samples += getPosition();
	limit = toBytes(info.encoding, samples);
	return errSuccess;
}

bool AudioFile::isSigned(void)
{
	switch(info.format)
	{
	case sun:
		return false;
	default:
		switch(info.encoding)
		{
		case pcm8Mono:
		case pcm8Stereo:
		case pcm16Mono:
		case pcm16Stereo:
		case pcm32Mono:
		case pcm32Stereo:
			return true;
		default:
			return false;
		}
	}
}

#ifdef	CCXX_NAMESPACES
};
#endif


