/*
 * GNUsound - a sound editor for GNOME.
 * Copyright (C) 2002-2004  Pascal Haakmat <a.haakmat@chello.nl>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#include <gnusound.h>

#ifndef HAVE_AUDIOFILE
#warning "Not building AudioFile support."
#else

#include <audiofile.h>
#include <math.h>
#include <assert.h>

/*
 * In an wholly benevolent world we might be able to query the
 * supported file formats dynamically. In an indifferent world the
 * file format identifiers might have been specified as #define's
 * rather than enum's. Only a downright evil world would require you
 * to specify the supported file formats yourself.
 *
 * Enjoy.
 */

#define HAVE_AF_FILE_AIFFC
#define HAVE_AF_FILE_AIFF
#define HAVE_AF_FILE_NEXTSND
#define HAVE_AF_FILE_WAVE
#define HAVE_AF_FILE_BICSF

/*
 * These are not supported by the 0.2.3 version of the Linux port of
 * AudioFile, but SGI's documentation mentions them. 
 */

#undef HAVE_AF_FILE_MPEG1BITSTREAM
#undef HAVE_AF_FILE_SOUNDESIGNER1
#undef HAVE_AF_FILE_SOUNDESIGNER2
#undef HAVE_AF_FILE_AVR
#undef HAVE_AF_FILE_IFF_8SVX
#undef HAVE_AF_FILE_VOC
#undef HAVE_AF_FILE_SAMPLEVISION
#undef HAVE_AF_FILE_SOUNDFONT2
#undef HAVE_AF_FILE_NIST_SPHERE

struct af_file_info {
    AFfilehandle fh;
    int file_format_id;
    int file_format_version;
    int sample_format;
    int sample_width;
    int channels;
    float rate;
};

static struct file_driver af_driver;

#define RETURN_IF_MATCH(match, id) if(!strcmp(format, match)) return id;

static int
af_format_name_to_id(const char *format) {
#ifdef HAVE_AF_FILE_AIFFC
    RETURN_IF_MATCH("AIFF-C", AF_FILE_AIFFC);
#endif
#ifdef HAVE_AF_FILE_AIFF
    RETURN_IF_MATCH("AIFF", AF_FILE_AIFF);
#endif
#ifdef HAVE_AF_FILE_NEXTSND
    RETURN_IF_MATCH("Sun .au", AF_FILE_NEXTSND);
#endif
#ifdef HAVE_AF_FILE_WAVE
    RETURN_IF_MATCH("WAVE", AF_FILE_WAVE);
#endif
#ifdef HAVE_AF_FILE_BICSF
    RETURN_IF_MATCH("Berkeley/IRCAM/CARL", AF_FILE_BICSF);
#endif
#ifdef HAVE_AF_FILE_MPEG1BITSTREAM
    RETURN_IF_MATCH("MPEG-1 Audio", AF_FILE_MPEG1BITSTREAM);
#endif
    return AF_FILE_WAVE;
}

static const char *
af_format_id_to_name(int format_id) {
    const char *file_format_name;

    switch(format_id) {
#ifdef HAVE_AF_FILE_AIFFC
    case AF_FILE_AIFFC: /* extended Audio Interchange File Format (AIFF-C) */
        file_format_name = "AIFF-C";
        break;
#endif
#ifdef HAVE_AF_FILE_AIFF
    case AF_FILE_AIFF: /* Audio Interchange File Format (AIFF) */
        file_format_name = "AIFF";
        break;
#endif
#ifdef HAVE_AF_FILE_NEXTSND
    case AF_FILE_NEXTSND: /* NeXT .snd and Sun .au */
        file_format_name = "Sun .au";
        break;
#endif
#ifdef HAVE_AF_FILE_WAVE
    case AF_FILE_WAVE: /* Waveform Audio File Format (RIFF) */
        file_format_name = "WAVE";
        break;
#endif
#ifdef HAVE_AF_FILE_BICSF
    case AF_FILE_BICSF: /* Berkeley/IRCAM/CARL Sound File format */
        file_format_name = "Berkeley/IRCAM/CARL";
        break;
#endif
#ifdef HAVE_AF_FILE_MPEG1BITSTREAM
    case AF_FILE_MPEG1BITSTREAM: /* MPEG-1 Audio Bitstream encoded data */
        file_format_name = "MPEG-1 Audio";
        break;
#endif
#ifdef HAVE_AF_FILE_SOUNDESIGNER1
    case AF_FILE_SOUNDESIGNER1: /* Sound Designer File Format I (not supported) */
        file_format_name = "Sound Designer I";
        break;        
#endif
#ifdef HAVE_AF_FILE_SOUNDESIGNER2
    case AF_FILE_SOUNDESIGNER2: /* Sound Designer File Format II */
        file_format_name = "Sound Designer II";
        break;
#endif
#ifdef HAVE_AF_FILE_AVR
    case AF_FILE_AVR: /* Audio Visual Research File Format */
        file_format_name = "Audio Visual Research";
        break;
#endif
#ifdef HAVE_AF_FILE_IFF_8SVX
    case AF_FILE_IFF_8SVX: /* Amiga IFF/8SVX File Format */
        file_format_name = "Amiga IFF/8SVX";
        break;
#endif
#ifdef HAVE_AF_FILE_VOC
    case AF_FILE_VOC: /* Creative Labs VOC File Format */
        file_format_name = "VOC";
        break;
#endif
#ifdef HAVE_AF_FILE_SAMPLEVISION
    case AF_FILE_SAMPLEVISION: /* Sample Vision File Format */
        file_format_name = "Sample Vision";
        break;
#endif
#ifdef HAVE_AF_FILE_SOUNDFONT2
    case AF_FILE_SOUNDFONT2: /* Creative Labs SoundFont2 (tm) File Format */
        file_format_name = "SoundFont2";
        break;
#endif
#ifdef HAVE_AF_FILE_NIST_SPHERE
    case AF_FILE_NIST_SPHERE: /* NIST SPHERE File Format (not supported) */
        file_format_name = "NIST SPHERE";
        break;
#endif
    default:
        file_format_name = "Unknown";
        break;
    }  
    
    return file_format_name;
}

static struct cmd_value *
af_attach(struct file *file,
          const char *format) {
    struct af_file_info *af_info = mem_calloc(1, sizeof(*af_info));
    
    if(!af_info)
        return cmd_new_error_val("Could not allocate parameters");

    af_info->sample_format = AF_SAMPFMT_TWOSCOMP;
    if(format == NULL) 
        af_info->file_format_id = AF_FILE_WAVE;
    else 
        af_info->file_format_id = af_format_name_to_id(format);
    
    file->driver = &af_driver;
    file->driver_data = af_info;

    return cmd_new_void_val();
}

static void
af_fill_params(struct file *file,
               struct file_params *params) {
    struct af_file_info *af_info = file->driver_data;
    AFfilehandle fh = af_info->fh;

    afGetSampleFormat(fh, AF_DEFAULT_TRACK, 
                      &af_info->sample_format, &af_info->sample_width);

    switch(af_info->sample_format) {
    case AF_SAMPFMT_DOUBLE:
    case AF_SAMPFMT_FLOAT:
        params->sample_type = SAMPLE_TYPE_FLOAT_32;
        break;
    case AF_SAMPFMT_UNSIGNED:
    case AF_SAMPFMT_TWOSCOMP:
        switch((int)ceil((double)af_info->sample_width / (double)8)) {
        case 1:
            params->sample_type = SAMPLE_TYPE_INT_8;
            break;
        case 2:
            params->sample_type = SAMPLE_TYPE_INT_16;
            break;
        case 3:
        case 4:
            params->sample_type = SAMPLE_TYPE_INT_32;
            break;
        default:
            DEBUG("unsupported number of bits %d, loading as 32 bit float\n",
                  af_info->sample_width);
            params->sample_type = SAMPLE_TYPE_FLOAT_32;
            break;
        }
        break;
    default:
        DEBUG("unknown sample format %d, loading as 32 bit float\n", 
              af_info->sample_format);
        params->sample_type = SAMPLE_TYPE_FLOAT_32;
        break;
    }

    params->channels = afGetChannels(fh, AF_DEFAULT_TRACK);
    params->sample_rate = afGetRate(fh, AF_DEFAULT_TRACK);
    params->frame_count = afGetFrameCount(fh, AF_DEFAULT_TRACK);
}

static struct cmd_value *
af_open_for_reading(struct file *file,
                    struct file_params *params) {
    struct af_file_info *af_info = file->driver_data;
    int virt_sample_format, virt_sample_width;

    af_info->fh = afOpenFile(file->name, "r", AF_NULL_FILESETUP);
    if(af_info->fh == AF_NULL_FILEHANDLE) 
        return cmd_new_error_val("Cannot open %s for reading",
                                 file->name);

    af_fill_params(file, params);

    virt_sample_format = AF_SAMPFMT_TWOSCOMP;
    virt_sample_width = sample_get_width(params->sample_type) * 8;

    switch(params->sample_type) {
    case SAMPLE_TYPE_INT_8:
    case SAMPLE_TYPE_INT_16:
    case SAMPLE_TYPE_INT_32:
        break;
    case SAMPLE_TYPE_FLOAT_32:
        virt_sample_format = AF_SAMPFMT_FLOAT;
        break;
    }

    af_info->file_format_id = 
        afGetFileFormat(af_info->fh, &af_info->file_format_version);
    
    afSetVirtualSampleFormat(af_info->fh,
                             AF_DEFAULT_TRACK,
                             virt_sample_format,
                             virt_sample_width);

    return cmd_new_void_val();
}

static struct cmd_value *
af_open_for_writing(struct file *file,
                    struct file_params *params) {
    struct af_file_info *af_info = file->driver_data;
    int sample_width, sample_format = AF_SAMPFMT_TWOSCOMP;
    AFfilesetup file_setup;

    assert(file->driver == &af_driver);
    assert(file->driver_data != NULL);

    file_setup = afNewFileSetup();

    if(!file_setup) 
        return cmd_new_error_val("Could not create new file setup");

    sample_width = sample_get_width(params->sample_type) * 8;

    switch(params->sample_type) {
    case SAMPLE_TYPE_INT_8:
    case SAMPLE_TYPE_INT_16:
    case SAMPLE_TYPE_INT_32:
        if(af_info->sample_format == AF_SAMPFMT_TWOSCOMP)
            sample_format = AF_SAMPFMT_TWOSCOMP;
        else
            sample_format = AF_SAMPFMT_UNSIGNED;
        break;
    case SAMPLE_TYPE_FLOAT_32:
        sample_format = AF_SAMPFMT_FLOAT;
        break;
    }

    afInitFileFormat(file_setup,
                     af_info->file_format_id);
    afInitSampleFormat(file_setup,
                       AF_DEFAULT_TRACK,
                       sample_format,
                       sample_width);
    afInitChannels(file_setup,
                   AF_DEFAULT_TRACK,
                   params->channels);
    afInitRate(file_setup,
               AF_DEFAULT_TRACK,
               params->sample_rate);

    af_info->fh = afOpenFile(file->name, "w", file_setup);

    afFreeFileSetup(file_setup);
    
    if(af_info->fh == AF_NULL_FILEHANDLE)         
        return cmd_new_error_val("Could not open %s for writing. "
                                 "Check filename and permissions", 
                                 file->name);

    return cmd_new_void_val();
}

static struct cmd_value *
af_open(struct file *file,
        const char *mode,
        struct file_params *params) {

    assert(file != NULL);
    assert(params != NULL);

    if(mode[0] == 'r')
        return af_open_for_reading(file, params);
    else if(mode[0] == 'w')
        return af_open_for_writing(file, params);
    else
        return cmd_new_error_val("Unknown mode %s", mode);
}

static long
af_read(struct file *info,
        void *buf,
        long count) {
    struct af_file_info *af_info = info->driver_data;
    return afReadFrames(af_info->fh, AF_DEFAULT_TRACK, buf, count);
}

static long
af_write(struct file *info,
         void *buf,
         long count) {
    struct af_file_info *af_info = info->driver_data;
    return afWriteFrames(af_info->fh, AF_DEFAULT_TRACK, buf, count);
}

static struct cmd_value *
af_close(struct file *info) {
    struct af_file_info *af_info = info->driver_data;
    afCloseFile(af_info->fh);
    af_info->fh = AF_NULL_FILEHANDLE;
    return cmd_new_void_val();
}

static void 
af_detach(struct file *file) {
    mem_free(file->driver_data);
    file->driver = NULL;
}

static int
af_snprint(struct file *file,
           enum file_property what,
           char *buf,
           int buflen) {
    struct af_file_info *af_info = file->driver_data;
    
    return snprintf(buf, buflen, "%s", 
                    af_format_id_to_name(af_info->file_format_id));
}

static const struct file_format formats[] = {
    { "WAVE", "wav" },
    { "AIFF", "aiff" },
    { "AIFF-C", "aifc" },
    { "Sun .au", "au" },
    { NULL, NULL },
};

static const struct file_format *
af_get_read_formats() {
    return formats;
}

static const struct file_format *
af_get_write_formats() {
    return formats;
}

static struct file_driver af_driver = {
    "AudioFile",

    af_attach,
    af_open,
    af_read,
    af_write,
    af_close,
    af_detach,

    af_snprint,

    af_get_read_formats,
    af_get_write_formats,

    NULL,
    NULL,
    NULL,

    NULL,
    NULL,
    NULL
};

static int
af_module_init(int id) {
    file_register_driver(&af_driver);
    return 0;
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "AudioFile driver",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002,2003,2004",
    "GPL",
    NULL,
    MODULE_FLAG_FACELESS,

    af_module_init,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

#endif /* HAVE_AUDIOFILE */
