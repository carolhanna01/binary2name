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

#ifndef	CCXX_AUDIO_H_
#define	CCXX_AUDIO_H_

#ifndef	CCXX_CONFIG_H_
#include <cc++/config.h>
#endif

#ifndef	CCXX_THREAD_H_
#include <cc++/thread.h>
#endif

#ifdef	CCXX_NAMESPACES
namespace ost {
#endif

#define	AUDIO_SIGNED_LINEAR_RAW	1
#define	AUDIO_LINEAR_CONVERSION 1
#define	AUDIO_CODEC_MODULES	1

typedef	signed short *Linear;

typedef struct
{
	float v2;
	float v3;
	float fac;
} goertzel_state_t;

typedef struct
{
	int hit1;
	int hit2;
	int hit3;
	int hit4;
	int mhit;

	goertzel_state_t row_out[4];
	goertzel_state_t col_out[4];
	goertzel_state_t row_out2nd[4];
	goertzel_state_t col_out2nd[4];
	goertzel_state_t fax_tone;
	goertzel_state_t fax_tone2nd;
	float energy;

	int current_sample;
	char digits[129];
	int current_digits;
	int detected_digits;
	int lost_digits;
	int digit_hits[16];
	int fax_hits;
} dtmf_detect_state_t;

typedef struct
{
	float fac;
} tone_detection_descriptor_t;

class AudioCodec;

/**
 * Generic audio class to hold master data types and various useful
 * class encapsulated friend functions as per GNU Common C++ 2 coding
 * standard.
 *
 * @author David Sugar <dyfet@ostel.com>
 * @short Master audio class.
 */
class CCXX_CLASS_EXPORT Audio
{
public:
	enum	Rate
	{
		rateUnknown,
		rate6khz = 6000,
		rate8khz = 8000,
		rate44khz = 44100
	};
	typedef enum Rate Rate;

	enum	Encoding
	{
		unknownEncoding = 0,
		g721ADPCM,
		g722Audio,
		g722_7bit,
		g722_6bit,
		g723_3bit,
		g723_5bit,
		gsmVoice,
		mulawAudio,
		alawAudio,
		okiADPCM,
		voxADPCM,

		// Please keep the PCM types at the end of the list -
		// see the "is this PCM or not?" code in
		// AudioFile::close for why.
		cdaStereo,
		cdaMono,
		pcm8Stereo,
		pcm8Mono,
		pcm16Stereo,
		pcm16Mono,
		pcm32Stereo,
		pcm32Mono
	};
	typedef enum Encoding Encoding;

	enum Format
	{
		raw,
		sun,
		riff,
		wave
	};
	typedef enum Format Format;

	enum Error
	{
		errSuccess = 0,
		errReadLast,
		errNotOpened,
		errEndOfFile,
		errStartOfFile,
		errRateInvalid,
		errEncodingInvalid,
		errReadInterrupt,
		errWriteInterrupt,
		errReadFailure,
		errWriteFailure,
		errReadIncomplete,
		errWriteIncomplete,
		errRequestInvalid,
		errTOCFailed,
		errStatFailed,
		errInvalidTrack,
		errPlaybackFailed,
		errNotPlaying,
		errNoCodec
	};
	typedef enum Error Error;

	class Info
	{
	public:
		Format format;
		Encoding encoding;
		unsigned rate;
		unsigned order;
		char *annotation;
	};

	static bool isMono(Encoding encoding);
	static bool isStereo(Encoding encoding);
	static Rate getRate(Encoding encoding);

	/**
	 * Returns the number of bytes in a sample frame for the given
	 * encoding type, rounded up to the nearest integer.  A frame
	 * is defined as the minimum number of bytes necessary to
	 * create a point or points in the output waveform for all
	 * output channels.  For example, 16-bit mono PCM has a frame
	 * size of two (because those two bytes constitute a point in
	 * the output waveform).  GSM has it's own definition of a
	 * frame which involves decompressing a sequence of bytes to
	 * determine the final points on the output waveform.  The
	 * minimum number of bytes you can feed to the decompression
	 * engine is 32.5 (260 bits), so this function will return 33
	 * (because we round up) given an encoding type of GSM.  Other
	 * compressed encodings will return similar results.  Be
	 * prepared to deal with nonintuitive return values for
	 * rare encodings.
	 *
	 * @param encoding The encoding type to get the frame size for.
	 * @param samples Reserved.  Use zero.
	 *
	 * @return The number of bytes in a frame for the given encoding.
	 */
	static int getFrame(Encoding encoding, int samples = 0);

	/**
	 * Returns the number of samples in all channels for a frame
	 * in the given encoding.  For example, pcm32Stereo has a
	 * frame size of 8 bytes: Note that different codecs have
	 * different definitions of a frame - for example, compressed
	 * encodings have a rather large frame size relative to the
	 * sample size due to the way bytes are fed to the
	 * decompression engine.
	 *
	 * @param encoding The encoding to calculate the frame sample count for.
	 * @return samples The number of samples in a frame of the given encoding.
	 */
	static int getCount(Encoding encoding);
	static unsigned long toSamples(Encoding encoding, size_t bytes);
	static unsigned long toBytes(Encoding encoding, unsigned long samples);
	static void fill(unsigned char *addr, int samples, Encoding encoding);
};

/**
 * A class used to manipulate audio data.  This class provides file
 * level access to audio data stored in different formats.  This class
 * also provides the ability to write audio data into a disk file.
 *
 * @author David Sugar <dyfet@ostel.com>
 * @short audio file access.
 */
class CCXX_CLASS_EXPORT AudioFile: public Audio
{
private:
	char *pathname;
	Error error;
	Info info;
	unsigned long header;		// offset to start of audio
	unsigned long minimum;		// minimum sample size required
	unsigned long length;           // current size of file,
					// including header

	bool modified;

	void initialize(void);
	void getWaveFormat(int size);

protected:
	union
	{
		int fd;
		void *handle;
	} file;

	unsigned long limit;

	virtual bool afCreate(const char *path);
	virtual bool afOpen(const char *path);
	virtual bool afPeek(unsigned char *data, unsigned size);

	AudioCodec *getCodec(void);

	/**
	 * Read a given number of bytes from the file, starting from
	 * the current file pointer.  May be overridden by derived
	 * classes.
	 *
	 * @param data A pointer to the buffer to copy the bytes to.
	 * @param size The number of bytes to read.
	 * @return The number of bytes read, or -1 if an error occurs.
	 * On UNIX platforms, use strerror(errno) to get the
	 * human-readable error string or
	 * FormatMessage(GetLastError()) on Windows platforms.
	 */
	virtual int afRead(unsigned char *data, unsigned size);

	/** 
	 * Write a number of bytes into the file at the current file
	 * pointer.  May be overridden by derived classes.
	 * 
	 * @param data A pointer to the buffer with the bytes to write.
	 * @param size The number of bytes to write from the buffer.
	 * @return The number of bytes written, or -1 if an error
	 * occurs.  On UNIX platforms, use strerror(errno) to get the
	 * human-readable error string or
	 * FormatMessage(GetLastError()) on Windows platforms.
	 */
	virtual int afWrite(unsigned char *data, unsigned size);

	/**
	 * Seek to the given position relative to the start of the
	 * file and set the file pointer.  This does not use 64-bit
	 * clean seek functions, so seeking to positions greater than
	 * (2^32)-1 will result in undefined behavior.
	 * 
	 * @param pos The position to seek to.
	 * @return true if successful, false otherwise.
	 */
	virtual bool afSeek(unsigned long pos);
	virtual void afClose(void);

	virtual char *getContinuation(void)
		{return NULL;};

	/**
	 * Return a human-readable error message given a numeric error
	 * code of type Audio::Error.
	 *
	 * @param err The numeric error code to translate.
	 * @return A pointer to a character string containing the
	 * human-readable error message.
	 */
	const char * getErrorStr(Error err);
	Error setError(Error err);

	unsigned long getHeader(void)
		{return header;};

	unsigned short getShort(unsigned char *data);
	void setShort(unsigned char *data, unsigned short value);
	unsigned long getLong(unsigned char *data);
	void setLong(unsigned char *data, unsigned long value);

public:
	AudioFile(const char *fname, unsigned long samples = 0);
	AudioFile(const char *fname, Info *info, unsigned long min = 0);

	AudioFile()
		{initialize();};

	virtual ~AudioFile()
		{clear();};

	/**
	 * Open an audio file and associate it with this object.
	 * Called implicitly by the two-argument version of the
	 * constructor.
	 *
	 * @param fname The name of the file to open.  Don't forget to
	 * double your backslashes for DOS-style pathnames.
	 */
	void open(const char *fname);

	/**
	 * Create a new audio file and associate it with this object.
	 * Called implicitly by the three-argument version of the
	 * constructor.
	 *
	 * @param fname The name of the file to open.
	 * @param info The type of the audio file to be created.
	 */
	void create(const char *fname, Info *info);

	/**
	 * Close an object associated with an open file.  This
	 * updates the header metadata with the file length if the
	 * file length has changed.
	 */
	void close(void);

	/**
	 * Clear the AudioFile structure.  Called by
	 * AudioFile::close().  Sets all fields to zero and deletes
	 * the dynamically allocated memory pointed to by the pathname
	 * and info.annotation members.  See AudioFile::initialize()
	 * for the dynamic allocation code.
	 */
	void clear(void);

	/**
	 * Retrieve bytes from the file into a memory buffer.  This
	 * increments the file pointer so subsequent calls read further
	 * bytes.  If you want to read a number of samples rather than
	 * bytes, use getSamples().
	 *
	 * @param addr A pointer to the memory area to copy the samples to.
	 * @param len The number of bytes (not samples) to copy.
	 * @return The number of bytes (not samples) read.  Returns -1
	 * if no bytes are read and an error occurs.
	 */
	int getBuffer(void *addr, unsigned len);

	/**
	 * Retrieve and convert content to linear encoded audio data
	 * from it's original form.
	 *
	 * @param addr A pointer to copy linear data into.
	 * @param len Number of linear samples to extract.
	 * @return number of samples retrieved, 0 if no codec or eof.
	 */
	unsigned getLinear(Linear buffer, unsigned request);

	/**
	 * Insert bytes into the file from a memory buffer.  This
	 * increments the file pointer so subsequent calls append
	 * further samples.  If you want to write a number of samples
	 * rather than bytes, use putSamples().
	 *
	 * @param attr A pointer to the memory area to append the samples
	 * from.
	 * @param len The number of bytes (not samples) to append.
	 * @return The number of bytes (not samples) read.  Returns -1
	 * if an error occurs and no bytes are written.
	 */
	int putBuffer(void *attr, unsigned len);

        /**
         * Convert and store content from linear encoded audio data
         * to the format of the audio file.
         *
         * @param addr A pointer to copy linear data from.
         * @param len Number of linear samples to save.
         * @return number of samples saved, 0 if no codec or eof.
         */
        unsigned putLinear(Linear buffer, unsigned request);

	/**
	 * Retrieve samples from the file into a memory buffer.  This
	 * increments the file pointer so subsequent calls read
	 * further samples.  If a limit has been set using setLimit(),
	 * the number of samples read will be truncated to the limit
	 * position.  If you want to read a certain number of bytes
	 * rather than a certain number of samples, use getBuffer().
	 *
	 * @param addr A pointer to the memory area to copy the samples to.
	 * @param samples The number of samples to read.
	 * @return errSuccess if successful, !errSuccess if
	 * error.  Use getErrorStr() to retrieve the human-readable
	 * error string.
	 */
	Error getSamples(void *addr, unsigned samples);

	/**
	 * Insert samples into the file from a memory buffer.  This
	 * increments the file pointer so subsequent calls append
	 * further samples.  If you want to write a certain number of
	 * bytes rather than a certain number of samples, use
	 * putBuffer().
	 *
	 * @param addr A pointer to the memory area to append the samples
	 * from.
	 * @param samples The number of samples (not bytes) to append.
	 * @return errSuccess if successful, !errSuccess if
	 * error.  Use getErrorStr() to retrieve the human-readable
	 * error string.
	 */
	Error putSamples(void *addr, unsigned samples);
	Error skip(long samples);
	Error setPosition(unsigned long samples = ~0l);
	Error setLimit(unsigned long samples = 0l);
	Error getInfo(Info *info);
	Error setMinimum(unsigned long samples);

	/**
	 * Get the current file pointer in bytes relative to the start
	 * of the file.  See getPosition() to determine the position
	 * relative to the start of the sample buffer.
	 *
	 * @return The current file pointer in bytes relative to the
	 * start of the file.  Returns 0 if the file is not open, is
	 * empty, or an error has occured.
	 */
	unsigned long getAbsolutePosition(void);

	/**
	 * Get the current file pointer in samples relative to the
	 * start of the sample buffer.  Note that you must multiply
	 * this result by the result of a call to
	 * toBytes(info.encoding, 1) in order to determine the offset
	 * in bytes.
	 *
	 * @return the current file pointer in samples relative to the
	 * start of the sample buffer.  Returns 0 if the file is not
	 * open, is empty, or an error has occured.
	 */
	unsigned long getPosition(void);
	virtual bool isOpen(void);
	virtual bool hasPositioning(void)
		{return true;};

	inline Encoding getEncoding(void)
		{return info.encoding;};

	inline Format getFormat(void)
		{return info.format;};

	inline unsigned getSampleRate(void)
		{return info.rate;};

	inline char *getAnnotation(void)
		{return info.annotation;};

	inline Error getError(void)
		{return error;};

	inline bool operator!(void)
		{return (bool)!isOpen();};

	/**
	 * Return if the current content is signed or unsigned samples.
	 *
	 * @return true if signed.
	 */
	bool isSigned(void);
};

/**
 * This class allows one to control audio playback from the CD player
 * on the target platform.  Audio playback can be used to play selective
 * tracks, to eject, etc.
 *
 * @author David Sugar <dyfet@ostel.com>
 * @short control cd audio player.
 */
class CCXX_CLASS_EXPORT CDAudio : public Audio
{
private:
	union
	{
		int fd;
		void *handle;
	} file;
	unsigned char v0, v1;
#ifdef	__WIN32__
	CRITICAL_SECTION crit;
	bool paused;
	bool opened;
	char position[20];
	char endmark[24];
	char ret[256];
	DWORD command(char *fmt, ...);
#endif
	Error err;

public:
	CDAudio(int devnbr = 0);
	~CDAudio();

	Error play(int start, int end = 0);
	Error stop(void);
	Error pause(void);
	Error resume(void);
	Error eject(void);
	Error reload(void);
	int getFirst(void);
	int getLast(void);
	bool isPaused(void);
	bool isAudio(int track);
	bool isOpen(void);

	unsigned char getVolume(int speaker);
	void setVolume(unsigned char left, unsigned char right);

	inline unsigned char getVolumeLeft(void)
		{return getVolume(0);};

	inline unsigned char getVolumeRight(void)
		{return getVolume(1);};

	inline Error getError(void)
		{return err;};

	inline bool operator!(void)
		{return (bool)!isOpen();};
};

/**
 * This class is use to represent and process audio data held in memory.
 *
 * @author David Sugar <dyfet@ostel.com>
 * @short audio data in memory.
 */
class CCXX_CLASS_EXPORT AudioSample : public Audio
{
protected:
	friend class AudioCopy;

	Encoding encoding;
	unsigned rate;
	unsigned count;
	unsigned char *samples;

public:
	AudioSample(unsigned frame, Encoding coding = pcm16Mono, unsigned rate = 8000);
	~AudioSample();

	inline unsigned getCount(void)
		{return count;};

	inline unsigned getRate(void)
		{return rate;};

	inline Encoding getEncoding(void)
		{return encoding;};

	inline unsigned char *getSamples(void)
		{return samples;};
};

/**
 * The linear class is a type variation of AudioSample used specifically 
 * to tag audio conversions that are performed between "linear" and
 * some other encoding.
 */
class CCXX_CLASS_EXPORT LinearSample : public AudioSample
{
public:
	LinearSample(unsigned frame, unsigned rate = 8000) :
	AudioSample(frame, pcm16Mono, rate) {};

	inline short *getSamples(void)
		{return (short *)samples;};
};

/**
 * The codec class is a virtual used for transcoding audio samples between
 * linear frames (or other known format) and an encoded "sample" buffer.
 * This class is only abstract and describes the core interface for
 * loadable codec modules.  This class is normally merged with AudioSample.
 * A derived AudioCodecXXX will typically include a AudioRegisterXXX static
 * class to automatically initialize and register the codec with the codec
 * registry.
 *
 * @author David Sugar <dyfet@ostel.com>
 * @short process codec interface.
 */
class CCXX_CLASS_EXPORT AudioCodec : public Audio
{
protected:
	static Mutex lock;
	static AudioCodec *first;
	AudioCodec *next;
	Encoding encoding;
	const char *name;

public:
	AudioCodec(const char *n, Encoding e);
	virtual ~AudioCodec() {};

	static AudioCodec *getCodec(Encoding encoding, const char *name = NULL);
	static bool load(const char *name);

	/**
	 * Encode a linear sample frame into the codec sample buffer.
	 *
	 * @param number of bytes written.
	 * @param buffer linear sample buffer to use.
	 * @param dest buffer to store encoded results.
	 * @param lsamples The number of linear samples.
	 */
	virtual unsigned encode(Linear buffer, void *dest, unsigned lsamples) = 0;

	/**
	 * Decode the sample frame into linear samples.
	 *
	 * @return number of bytes scanned.
	 * @param buffer sample buffer to save linear samples into.
	 * @param source for encoded data.
	 * @param number of samples to extract.
	 */
	virtual unsigned decode(Linear buffer, void *source, unsigned lsamples) = 0;
};

/**
 * The tone class is used to construct or generate a tone sample in memory.
 * These tone samples can be either single or dual tones, and are created
 * as linear samples of a specified sample rate.
 *
 * @author David Sugar <dyfet@ostel.com>
 * @short generate audio sample in memory.
 */
class CCXX_CLASS_EXPORT AudioTone : public AudioSample
{
protected:
	double p1, p2, v1, v2, fa1, fa2;

public:
	AudioTone(unsigned size, unsigned f1 = 0, unsigned f2 = 0, unsigned rate = 8000);

	/**
	 * Fill the current sample frame with more tone data.
	 */
	void fill(unsigned max = 0);

	/*
	 * Set frequency.
	 *
	 * @param f1 first frequency.
	 * @param f2 second frequency.
	 */
	void setFreq(unsigned f1, unsigned f2 = 0);
};

/**
 * AudioCopy is used for certain kinds of stream join operations.
 * Essentially it fills the current sample buffer from another AudioSample
 * object and requests a new object thru a virtual every time it empties.
 * This is used in dtmf dialing, for example, to fill in the next digit
 * tone.
 *
 * @short fill an audio sample frame from another.
 * @author David Sugar <dyfet@ostel.com>.
 */
class CCXX_CLASS_EXPORT AudioCopy : public AudioSample
{
protected:
	unsigned char *next;
	unsigned left;

	virtual AudioSample *fill(void) = 0;

public:
	AudioCopy(unsigned frame, Encoding encoding = pcm16Mono, unsigned rate = 8000);
	virtual ~AudioCopy() {};
	bool copy(void);

	inline bool isEmpty(void)
		{return next == NULL;};
};

/**
 * DTMFDetect is used for detecting DTMF tones in a stream of audio.
 * It currently only supports 8000Hz input.
 */

class CCXX_CLASS_EXPORT DTMFDetect
{
public:
	DTMFDetect();
	~DTMFDetect();

	int putSamples(int16_t buffer[], int count);
	int getResult(char *buf, int max);
protected:
	void goertzelInit(goertzel_state_t *s, tone_detection_descriptor_t *t);
	void goertzelUpdate(goertzel_state_t *s, int16_t x[], int samples);
	float goertzelResult(goertzel_state_t *s);
private:
	dtmf_detect_state_t *state;
	tone_detection_descriptor_t dtmf_detect_row[4];
	tone_detection_descriptor_t dtmf_detect_col[4];
	tone_detection_descriptor_t dtmf_detect_row_2nd[4];
	tone_detection_descriptor_t dtmf_detect_col_2nd[4];
	tone_detection_descriptor_t fax_detect;
	tone_detection_descriptor_t fax_detect_2nd;
};

#ifdef	CCXX_NAMESPACES
};
#endif

#endif

