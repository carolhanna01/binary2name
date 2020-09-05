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

/** 
 * @file
 * Driver for FFMPEG file I/O. FFMPEG supports quite a number 
 * of audio formats, and it's very fast. Most importantly though
 * it knows how to parse video files and extract audio streams
 * from them, this is a nice feature to have.
 *
 * For FFMPEG to be detected and HAVE_FFMPEG to be set, you
 * need to install the libavcodec and libavformats libraries
 * by doing ``make installlib'' in the ffmpeg source root
 * (information current as of ffmpeg-cvs-2004-07-12).
 */

#include <gnusound.h>

#ifndef HAVE_FFMPEG
#warning "Not building ffmpeg support."
#else

#include <ffmpeg/avformat.h>
#include <ffmpeg/avcodec.h>

struct ffmpeg_data {
    AVFormatContext *format_context;
    int audio_stream_index;
    AVStream *audio_stream;
    int decode_buf_avail;
    int decode_buf_offset;
    uint8_t decode_buf[(AVCODEC_MAX_AUDIO_FRAME_SIZE * 3) / 2];
    AVPacket packet;
    int packet_offset;
    char format_name[100];
};

static struct file_driver ffmpeg_driver;

static struct cmd_value *
ffmpeg_attach(struct file *file,
              const char *format) {
    struct ffmpeg_data *ffmpeg_data = mem_calloc(1, sizeof(*ffmpeg_data));
    
    if(!ffmpeg_data)
        return cmd_new_error_val("Could not allocate parameters");

    file->driver = &ffmpeg_driver;
    file->driver_data = ffmpeg_data;

    return cmd_new_void_val();
}

static int 
stream_component_open(AVFormatContext *fc,
                      int stream_index) {
    AVCodecContext *enc;
    AVCodec *codec;

    if(stream_index < 0 || stream_index >= fc->nb_streams)
        return -1;

    enc = &fc->streams[stream_index]->codec;
    codec = avcodec_find_decoder(enc->codec_id);
    if(!codec)
        return -1;

    if(avcodec_open(enc, codec) < 0)
        return -1;

    enc->thread_count = 1;
    return 0;
}

static struct cmd_value *
ffmpeg_open_for_reading(struct file *file,
                        struct file_params *params) {
    struct ffmpeg_data *ffmpeg_data = file->driver_data;
    AVFormatContext *fc;
    AVFormatParameters format_params;
    AVCodec *codec;
    int err, i, audio_index = -1;

    memset(&format_params, 0, sizeof(format_params));
    format_params.image_format = NULL;
    format_params.initial_pause = 1; /* we force a pause when starting an RTSP
                                        stream */

    err = av_open_input_file(&fc, file->name, NULL, 0, &format_params);
    if(err < 0) {
        DEBUG("av_open_input_file failed: %d\n", err);
        return cmd_new_error_val("Error %d", err);
    }

    err = av_find_stream_info(fc);
    if (err < 0) {
        av_close_input_file(fc);
        DEBUG("find_stream_info failed: %d\n", err);
        return cmd_new_error_val("Could not find codec parameters");
    }
    
    for(i = 0; i < fc->nb_streams; i++) {
        AVCodecContext *enc = &fc->streams[i]->codec;
        switch(enc->codec_type) {
        case CODEC_TYPE_AUDIO:
            DEBUG("found audio stream %d\n", i);
            if(audio_index < 0) {
                DEBUG("using audio stream %d\n", i);
                audio_index = i;
            } else {
                DEBUG("skipping audio stream %d\n", i);
            }
            break;
        default:
            DEBUG("skipping stream type %d\n", i);
            break;
        }
    }

    if(audio_index < 0) {
        DEBUG("no audio streams\n");
        av_close_input_file(fc);
        return cmd_new_error_val("Could not find audio stream");
    }
    
    err = stream_component_open(fc, audio_index);
    if(err < 0) {
        av_close_input_file(fc);
        return cmd_new_error_val("Could not open audio stream");
    }        

    codec = avcodec_find_decoder(fc->streams[audio_index]->codec.codec_id);
    snprintf(ffmpeg_data->format_name, sizeof(ffmpeg_data->format_name),
             "%s/%s", fc->iformat->name, codec->name);

    ffmpeg_data->format_context = fc;
    ffmpeg_data->audio_stream_index = audio_index;
    ffmpeg_data->audio_stream = fc->streams[audio_index];

    params->sample_type = SAMPLE_TYPE_INT_16;
    params->sample_rate = ffmpeg_data->audio_stream->codec.sample_rate;
    params->channels = ffmpeg_data->audio_stream->codec.channels;
    params->frame_count = -1;

    av_read_play(fc);

    return cmd_new_void_val();
}

static struct cmd_value *
ffmpeg_open(struct file *file,
            const char *mode,
            struct file_params *params) {
    if(mode[0] == 'r')
        return ffmpeg_open_for_reading(file, params);
    else if(mode[0] == 'w')
        return cmd_new_error_val("Not implemented");
    else
        return cmd_new_error_val("Unknown mode %s", mode);
}

static long
ffmpeg_read(struct file *file,
            void *buf,
            long count) {
    struct ffmpeg_data *ffmpeg_data = file->driver_data;
    int err, frame_size, len;
    long copied = 0;

    /* 
     * Fill the decode buffer if necessary.
     */

    while(ffmpeg_data->decode_buf_avail <= 0) {

        /* 
         * Get a packet from the stream if we don't already have a
         * packet. 
         */

        while(ffmpeg_data->packet.data == NULL) {
            err = av_read_frame(ffmpeg_data->format_context, 
                                &ffmpeg_data->packet);


            /*
             * If we can't get any more packets, assume we've reached
             * end of file. 
             */
            
            if(err < 0) 
                return -1;
            
            ffmpeg_data->packet_offset = 0;

            /* 
             * Skip the packet if it's not a part of the audio stream
             * we've selected.
             */

            if(ffmpeg_data->packet.stream_index != 
               ffmpeg_data->audio_stream_index) {
                av_free_packet(&ffmpeg_data->packet);
                ffmpeg_data->packet.data = NULL;
            }
        }

        /*
         * Try to decode the packet and store the decoded audio into
         * the decode buffer.
         */

        len = avcodec_decode_audio(&ffmpeg_data->audio_stream->codec,
                                   (int16_t *)ffmpeg_data->decode_buf,
                                   &frame_size,
                                   (ffmpeg_data->packet.data + 
                                    ffmpeg_data->packet_offset), 
                                   (ffmpeg_data->packet.size - 
                                    ffmpeg_data->packet_offset));

        if(len >= 0) {

            /* Something in decode buffer. */

            ffmpeg_data->decode_buf_avail = frame_size;
            ffmpeg_data->decode_buf_offset = 0;
            ffmpeg_data->packet_offset += len;

        }
        
        if(len < 0 ||
           ffmpeg_data->packet.size <= ffmpeg_data->packet_offset) {
            
            /* 
             * Either a decode error occurred or the packet was
             * exhausted, either way we need to discard the current
             * packet and read a new one.
             */            
            
            av_free_packet(&ffmpeg_data->packet);
            ffmpeg_data->packet.data = NULL;
            ffmpeg_data->packet_offset = 0;
            
        }


    }

    /*
     * Copy the decoded data into the buffer. 
     *
     * In theory we don't know how big the destination buffer is, so
     * we use an intermediary decode buffer that is guaranteed to be
     * big enough to hold the result from av_decode_audio(). In
     * practice, though, the destination buffer is always much bigger
     * than the maximum data returned by av_decode_audio(). So we
     * could save a memcpy() here, but this is cleaner.
     */

    copied = MIN(count * ffmpeg_data->audio_stream->codec.channels * 2, 
                 ffmpeg_data->decode_buf_avail);
    memcpy(buf, ffmpeg_data->decode_buf + ffmpeg_data->decode_buf_offset,
           copied);
    ffmpeg_data->decode_buf_offset += copied;
    ffmpeg_data->decode_buf_avail -= copied;

    return copied / (ffmpeg_data->audio_stream->codec.channels * 2);
}

static long
ffmpeg_write(struct file *file,
             void *buf,
             long count) {
    return -1;
}

static struct cmd_value *
ffmpeg_close(struct file *file) {
    struct ffmpeg_data *ffmpeg_data = file->driver_data;
    AVCodecContext *enc;

    if(ffmpeg_data->packet.data) 
        av_free_packet(&ffmpeg_data->packet);
        
    enc = &ffmpeg_data->audio_stream->codec;
    avcodec_close(enc);
    av_close_input_file(ffmpeg_data->format_context);

    return cmd_new_void_val();
}

static void 
ffmpeg_detach(struct file *file) {
    mem_free(file->driver_data);
    file->driver = NULL;
}

static int
ffmpeg_snprint(struct file *file,
               enum file_property what,
               char *buf,
               int buflen) {
    struct ffmpeg_data *ffmpeg_data = file->driver_data;

    switch(what) {
    case FILE_FORMAT:
    case FILE_DETAILED_FORMAT:
        return snprintf(buf, buflen, "%s", ffmpeg_data->format_name);
    default:
        return -1;
    }
}

static const struct file_format *
ffmpeg_get_read_formats() {
    return NULL;
}

static const struct file_format *
ffmpeg_get_write_formats() {
    return NULL;
}

static struct file_driver ffmpeg_driver = {
    "ffmpeg",

    ffmpeg_attach,
    ffmpeg_open,
    ffmpeg_read,
    ffmpeg_write,
    ffmpeg_close,
    ffmpeg_detach,
    
    ffmpeg_snprint,

    ffmpeg_get_read_formats,
    ffmpeg_get_write_formats,

    NULL,
    NULL,
    NULL,

    NULL,
    NULL,
    NULL
};

static int
ffmpeg_module_init(int id) {
    av_register_all();
    file_register_driver(&ffmpeg_driver);
    return 0;
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "ffmpeg driver",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2004",
    "GPL",
    NULL,
    MODULE_FLAG_FACELESS,

    ffmpeg_module_init,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

#endif /* HAVE_FFMPEG */
