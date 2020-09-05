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
 * This file implements file load/save support for the FLAC 
 * audio format. 
 */

#include <gnusound.h>

#ifndef HAVE_FLAC
#warning "Not building FLAC support."
#else

#include <FLAC/all.h>

struct flac_data {

    /* Decoder support. */

    FLAC__FileDecoder *decoder;
    
    int dec_inited;
    int dec_error;
    unsigned int dec_max_blocksize;
    unsigned int dec_channels;
    unsigned int dec_sample_type;
    unsigned int dec_sample_rate;
    int64_t dec_total_samples;
    void *dec_buffer;
    int dec_last_blocksize;
    int dec_bits_per_sample;

    /* Encoder support */

    FLAC__FileEncoder *encoder;

    int enc_inited;
    int enc_error;
    enum sample_type enc_sample_type;
    int enc_channels;
    int32_t *enc_buffer;

    /* Config support */
    
    GtkWidget *config_widget;
    struct pane *config_pane;
    int compression_level;
};

static struct file_driver flac_driver;

static int self_id;

static struct cmd_value *
flac_attach(struct file *file,
            const char *format) {
    struct flac_data *flac_data = mem_calloc(1, sizeof(*flac_data));
    
    if(!flac_data)
        return cmd_new_error_val("Could not allocate parameters");

    flac_data->enc_buffer = NULL;
    flac_data->dec_buffer = NULL;
    flac_data->compression_level = 4;

    file->driver = &flac_driver;
    file->driver_data = flac_data;

    return cmd_new_void_val();
}

#define INTERLEAVE(type, count, channels, src, dst) \
    for(i = 0; i < count; i++) { \
        for(j = 0; j < channels; j++) { \
            ((type *)dst)[(i * channels) + j] = buffer[j][i]; \
        } \
    }

static FLAC__StreamDecoderWriteStatus 
flac_dec_write_callback(const FLAC__FileDecoder *decoder,
                        const FLAC__Frame *frame,
                        const FLAC__int32 * const buffer[],
                        void *client_data) {
    struct flac_data *flac_data = ((struct file *)client_data)->driver_data;
    int i, j, count, channels;
    void *dst;

    count = frame->header.blocksize;
    channels = flac_data->dec_channels;
    dst = flac_data->dec_buffer;

    switch(flac_data->dec_sample_type) {
    case SAMPLE_TYPE_INT_8:
        INTERLEAVE(int8_t, count, channels, buffer, dst);
        break;
    case SAMPLE_TYPE_INT_16:
        INTERLEAVE(int16_t, count, channels, buffer, dst);
        break;
    case SAMPLE_TYPE_INT_32:
        INTERLEAVE(int32_t, count, channels, buffer, dst);
        break;
    }

    /* For 24 bits per sample, scale up to 32 bits. */

    if(flac_data->dec_bits_per_sample == 24) 
        for(i = 0; i < count * channels; i++) 
            ((int32_t *)dst)[i] <<= 8;
    
    flac_data->dec_last_blocksize = frame->header.blocksize;

    return FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE;
}

static void 
flac_dec_metadata_callback(const FLAC__FileDecoder *decoder,
                           const FLAC__StreamMetadata *metadata,
                           void *client_data) {    
    struct flac_data *flac_data = ((struct file *)client_data)->driver_data;
    FLAC__StreamMetadata_StreamInfo si;

    DEBUG("in metadata callback\n");
    
    if(metadata->type == FLAC__METADATA_TYPE_STREAMINFO) {

        /* Initialize parameters. */

        si = metadata->data.stream_info;
        flac_data->dec_inited = 1;
        flac_data->dec_error = 0;
        flac_data->dec_total_samples = si.total_samples;
        flac_data->dec_channels = si.channels;
        flac_data->dec_sample_rate = si.sample_rate;
        flac_data->dec_max_blocksize = si.max_blocksize;
        flac_data->dec_bits_per_sample = si.bits_per_sample;

        DEBUG("max blocksize: %d\n", si.max_blocksize);

        switch(si.bits_per_sample / 8) {
        case 1:
            flac_data->dec_sample_type = SAMPLE_TYPE_INT_8;
            break;
        case 2:
            flac_data->dec_sample_type = SAMPLE_TYPE_INT_16;
            break;
        case 3:
        case 4:
            flac_data->dec_sample_type = SAMPLE_TYPE_INT_32;
            break;
        default:
            flac_data->dec_error = 1;
            break;
        }
    }
}

static void
flac_dec_error_callback(const FLAC__FileDecoder *decoder,
                        FLAC__StreamDecoderErrorStatus status,
                        void *client_data) {
    struct flac_data *flac_data = ((struct file *)client_data)->driver_data;

    flac_data->dec_error = 1;
}

static struct cmd_value *
flac_open_for_reading(struct file *file,
                      struct file_params *params) {
    struct flac_data *flac_data = file->driver_data;
    FLAC__FileDecoder *decoder;

    decoder = FLAC__file_decoder_new();
    
    FLAC__file_decoder_set_md5_checking(decoder, false);
    FLAC__file_decoder_set_filename(decoder, file->name);
    FLAC__file_decoder_set_metadata_ignore_all(decoder);
    FLAC__file_decoder_set_metadata_respond(decoder, FLAC__METADATA_TYPE_STREAMINFO);
    FLAC__file_decoder_set_metadata_respond(decoder, FLAC__METADATA_TYPE_VORBIS_COMMENT);
    FLAC__file_decoder_set_write_callback(decoder, flac_dec_write_callback);
    FLAC__file_decoder_set_metadata_callback(decoder, flac_dec_metadata_callback);
    FLAC__file_decoder_set_error_callback(decoder, flac_dec_error_callback);
    FLAC__file_decoder_set_client_data(decoder, file);
    if(FLAC__file_decoder_init(decoder) != FLAC__FILE_DECODER_OK)
        return cmd_new_error_val("Could not initialize FLAC decoder: %s", FLAC__FileDecoderStateString[FLAC__file_decoder_get_state(decoder)]);
    
    if(!FLAC__file_decoder_process_until_end_of_metadata(decoder))
        return cmd_new_error_val("Could not process FLAC metadata: %s", FLAC__FileDecoderStateString[FLAC__file_decoder_get_state(decoder)]);
    
    flac_data->decoder = decoder;
    
    if(!flac_data->dec_inited || flac_data->dec_error) {
        FLAC__file_decoder_finish(decoder);
        FLAC__file_decoder_delete(decoder);
        return cmd_new_error_val("Could not find STREAM info or "
                                 "STREAM info was invalid");
    }

    /* This is highly unlikely. */

    if(flac_data->dec_max_blocksize > SAVE_BUF_SIZE) {
        FLAC__file_decoder_finish(decoder);
        FLAC__file_decoder_delete(decoder);
        return cmd_new_error_val("The maximum blocksize (%d) for this file "
                                 "is larger than SAVE_BUF_SIZE (%d), increase "
                                 "SAVE_BUF_SIZE and recompile", 
                                 flac_data->dec_max_blocksize, SAVE_BUF_SIZE);
    }
    
    params->sample_type = flac_data->dec_sample_type;
    params->sample_rate = flac_data->dec_sample_rate;
    params->frame_count = flac_data->dec_total_samples;
    params->channels = flac_data->dec_channels;

    return cmd_new_void_val();
}

static struct cmd_value *
flac_open_for_writing(struct file *file,
                      struct file_params *params) {
    struct flac_data *flac_data = file->driver_data;
    struct cmd_value *r;
    int err, bits_per_sample;
    FLAC__FileEncoder *encoder;
    const char *status;
    int max_lpc_order = 0,
        blocksize = 1152,
        mid_side = 0,
        adaptive_mid_side = 0,
        exhaustive_model_search = 0,
        residual_partition_order_min = 0,
        residual_partition_order_max = 0;

    flac_data->enc_buffer = 
            mem_alloc(sample_get_width(SAMPLE_TYPE_INT_32) *
                      params->channels *

                      SAVE_BUF_SIZE);
    
    if(!flac_data->enc_buffer) 
        return cmd_new_error_val("Could not allocate encoder buffer");

    encoder = FLAC__file_encoder_new();

    /* Standard properties. */

    FLAC__file_encoder_set_filename(encoder, file->name);
    FLAC__file_encoder_set_channels(encoder, params->channels);
    FLAC__file_encoder_set_sample_rate(encoder, params->sample_rate);
    switch(params->sample_type) {
    case SAMPLE_TYPE_INT_8:
        bits_per_sample = 8;
        break;
    case SAMPLE_TYPE_INT_16:
        bits_per_sample = 16;
        break;
    case SAMPLE_TYPE_INT_32:
        /* Reference encoder supports 24 bits max */
        bits_per_sample = 24;
        break;
    case SAMPLE_TYPE_FLOAT_32:
        /* Reference encoder supports 24 bits max */
        bits_per_sample = 24;
        break;
    }
    FLAC__file_encoder_set_bits_per_sample(encoder, bits_per_sample);

    /* FLAC encoding parameters. */

    switch(flac_data->compression_level) {
    case 0:
        max_lpc_order = 0;
        blocksize = 1152;
        residual_partition_order_min = 2;
        residual_partition_order_max = 2;
        break;
    case 1:
        max_lpc_order = 0;
        blocksize = 1152;
        adaptive_mid_side = 1;
        residual_partition_order_min = 2;
        residual_partition_order_max = 2;
        break;
    case 2:
        max_lpc_order = 0;
        blocksize = 1152;
        mid_side = 1;
        residual_partition_order_min = 0;
        residual_partition_order_max = 2;
        break;
    case 3:
        max_lpc_order = 6;
        blocksize = 4608;
        residual_partition_order_min = 3;
        residual_partition_order_max = 3;
        break;
    case 4:
        max_lpc_order = 8;
        blocksize = 4608;
        adaptive_mid_side = 1;
        residual_partition_order_min = 3;
        residual_partition_order_max = 3;
        break;
    case 5:
        max_lpc_order = 8;
        blocksize = 4608;
        mid_side = 1;
        residual_partition_order_min = 3;
        residual_partition_order_max = 3;
        break;
    case 6:
        max_lpc_order = 8;
        blocksize = 4608;
        mid_side = 1;
        residual_partition_order_max = 4;
        break;
    case 7:
        max_lpc_order = 8;
        blocksize = 4608;
        mid_side = 1;
        exhaustive_model_search = 1;
        residual_partition_order_max = 6;
        break;
    case 8:
        max_lpc_order = 12;
        blocksize = 4608;
        mid_side = 1;
        exhaustive_model_search = 1;
        residual_partition_order_max = 6;
        break;
    }
    
    FLAC__file_encoder_set_do_exhaustive_model_search(encoder, exhaustive_model_search);
    FLAC__file_encoder_set_blocksize(encoder, blocksize);
    FLAC__file_encoder_set_max_lpc_order(encoder, max_lpc_order);
    FLAC__file_encoder_set_min_residual_partition_order(encoder, residual_partition_order_min);
    FLAC__file_encoder_set_max_residual_partition_order(encoder, residual_partition_order_max);
    if(params->channels == 2) {
        FLAC__file_encoder_set_do_mid_side_stereo(encoder, mid_side);
        FLAC__file_encoder_set_loose_mid_side_stereo(encoder, adaptive_mid_side);
    }
    
    if((err = FLAC__file_encoder_init(encoder)) != FLAC__FILE_ENCODER_OK) {

        /* Initialization failed, get error condition. */

        if(flac_data->enc_buffer)
            mem_free(flac_data->enc_buffer);
        
        if(FLAC__file_encoder_get_state(encoder) == 
           FLAC__FILE_ENCODER_SEEKABLE_STREAM_ENCODER_ERROR) {
            if(FLAC__file_encoder_get_seekable_stream_encoder_state(encoder) ==
               FLAC__SEEKABLE_STREAM_ENCODER_STREAM_ENCODER_ERROR)
                status = FLAC__StreamEncoderStateString[FLAC__file_encoder_get_stream_encoder_state(encoder)];
            else
                status = FLAC__SeekableStreamEncoderStateString[FLAC__file_encoder_get_seekable_stream_encoder_state(encoder)];
        } else {
            status = FLAC__FileEncoderStateString[FLAC__file_encoder_get_state(encoder)];
        }
        
        FAIL("encoder initialization failure: %s\n", status);
        r = cmd_new_error_val("Could not initialize encoder: %s", status);

        FLAC__file_encoder_delete(encoder);

        return r;
        
    }
    
    flac_data->encoder = encoder;
    flac_data->enc_sample_type = params->sample_type;
    flac_data->enc_channels = params->channels;
    flac_data->enc_inited = 1;
    flac_data->enc_error = 0;

    return cmd_new_void_val();
}

static struct cmd_value *
flac_open(struct file *file,
          const char *mode,
          struct file_params *params) {
    if(mode[0] == 'r')
        return flac_open_for_reading(file, params);
    else if(mode[0] == 'w')
        return flac_open_for_writing(file, params);
    else
        return cmd_new_error_val("Unknown mode %s", mode);
}

static long
flac_read(struct file *file,
          void *buf,
          long count) {
    struct flac_data *flac_data = file->driver_data;

    flac_data->dec_buffer = buf;

    /*
     * This invokes the write callback which stuffs the data into the
     * buffer. 
     */

    if(!FLAC__file_decoder_process_single(flac_data->decoder))
        return -1;

    if(flac_data->dec_error)
        return -1;
    
    return flac_data->dec_last_blocksize;
}

static long
flac_write(struct file *file,
           void *buf,
           long count) {
    struct flac_data *flac_data = file->driver_data;
    int err, i;

    /* 
     * Convert source buffer to destination format: FLAC encoder
     * expects 8, 16 or 24 bit values in a 32 bit container.
     */

    switch(flac_data->enc_sample_type) {
    case SAMPLE_TYPE_INT_8:
        for(i = 0; i < count * flac_data->enc_channels; i++) 
            flac_data->enc_buffer[i] = ((int8_t *)buf)[i];
        break;
    case SAMPLE_TYPE_INT_16:
        for(i = 0; i < count * flac_data->enc_channels; i++) 
            flac_data->enc_buffer[i] = ((int16_t *)buf)[i];
        break;
    case SAMPLE_TYPE_INT_32:
        /*
         * Need to get rid of 8 bits since encoder supports maximum of
         * 24 bits. FIXME: Dithering might provide better quality,
         */
        for(i = 0; i < count * flac_data->enc_channels; i++) 
            flac_data->enc_buffer[i] = ((int32_t *)buf)[i] >> 8;
        break;
    case SAMPLE_TYPE_FLOAT_32:
        for(i = 0; i < count * flac_data->enc_channels; i++) 
            flac_data->enc_buffer[i] = ((float *)buf)[i] * 16777216;
        break;
    }

    err = FLAC__file_encoder_process_interleaved(flac_data->encoder,
                                                 flac_data->enc_buffer,
                                                 count);
    if(!err) {
        FAIL("encoder failure: %s\n", FLAC__FileEncoderStateString[FLAC__file_encoder_get_state(flac_data->encoder)]);
        return -1;
    }
    
    return count;
}

static struct cmd_value *
flac_close(struct file *file) {
    struct flac_data *flac_data = file->driver_data;

    if(flac_data->dec_inited) {
        FLAC__file_decoder_finish(flac_data->decoder);
        FLAC__file_decoder_delete(flac_data->decoder);
        //        if(flac_data->dec_buffer)
        //            mem_free(flac_data->dec_buffer);
        flac_data->dec_inited = 0;
    }

    if(flac_data->enc_inited) {
        FLAC__file_encoder_finish(flac_data->encoder);
        FLAC__file_encoder_delete(flac_data->encoder);
        if(flac_data->enc_buffer)
            mem_free(flac_data->enc_buffer);
        flac_data->enc_inited = 0;
    }

    return cmd_new_void_val();
}

static void 
flac_detach(struct file *file) {
    mem_free(file->driver_data);
    file->driver = NULL;
}

static int
flac_snprint(struct file *file,
             enum file_property what,
             char *buf,
             int buflen) {
    return snprintf(buf, buflen, "FLAC");
}

static const struct file_format *
flac_get_read_formats() {
    static struct file_format formats[] = {
        { "FLAC", "flac" },
        { NULL, NULL }
    };
    return formats;
}

static const struct file_format *
flac_get_write_formats() {
    static struct file_format formats[] = {
        { "FLAC", "flac" },
        { NULL, NULL }
    };
    return formats;
}


# define FLAC_GLADE_FILE "file_flac" GUI_GTK_VERSION_TAG ".glade"

static void
flac_sync_display(struct file *file) {
    struct flac_data *flac_data = file->driver_data;

    gtk_adjustment_set_value(gtk_range_get_adjustment(GTK_RANGE(pane_get_widget(flac_data->config_pane, "flac_compression_level"))), flac_data->compression_level);
}

static void
flac_close_file_config(struct file *file) {
    struct flac_data *flac_data = file->driver_data;
    pane_destroy(flac_data->config_pane);
}

static void 
flac_commit_file_config(struct file *file) {
    GtkAdjustment *adjustment;
    struct flac_data *flac_data = file->driver_data;

    adjustment = gtk_range_get_adjustment(GTK_RANGE(pane_get_widget(flac_data->config_pane, "flac_compression_level")));
    flac_data->compression_level = CLAMP(adjustment->value, 0, 8);

    pref_set_int("flac.compression_level", flac_data->compression_level);
    pref_sync();
}

static GtkWidget *
flac_open_file_config(struct file *file,
                      const char *format) {
    GladeXML *xml = NULL;
    GtkWidget *w = NULL;
    char path[4096];
    struct pane *config_pane = NULL;
    struct flac_data *flac_data = file->driver_data;

    /* Load the XML file. */

    snprintf(path, sizeof(path), "%s/%s", module_get_path(self_id), 
             FLAC_GLADE_FILE);
    DEBUG("loading interface %s\n", path);
    xml = glade_xml_new(path, NULL, NULL);
    
    if(!xml) {
        FAIL("could not find interface definition, " "looked at %s\n", 
             path);
        return NULL;
    }

    /* Associate a pane with the XML file. */

    config_pane = pane_new(xml);
    if(!config_pane) {
        FAIL("could not create pane for configuration\n");
        g_object_unref(G_OBJECT(xml));
        return NULL;
    }

    w = pane_get_widget(config_pane, "flac_config_panel");
    gtk_widget_ref(w);
    gtk_container_remove(GTK_CONTAINER(pane_get_widget(config_pane, "flac_config")), w);

    flac_data->config_widget = w;
    flac_data->config_pane = config_pane;

    flac_sync_display(file);

    return w;
}

static struct file_driver flac_driver = {
    "FLAC",

    flac_attach,
    flac_open,
    flac_read,
    flac_write,
    flac_close,
    flac_detach,
    
    flac_snprint,

    flac_get_read_formats,
    flac_get_write_formats,

    NULL,
    NULL,
    NULL,

    flac_open_file_config,
    flac_commit_file_config,
    flac_close_file_config
};

static struct pref default_prefs[] = { 

    PREF_BOOL("flac.exhaustive_model_search", 0,
              "Whether to use exhaustive model search, makes encoding "
              "much slower, may yield slightly better compression."),
    PREF_INT("flac.blocksize", 1152, 1152, 18432,
             "Undocumented."),
    PREF_INT("flac.max_lpc_order", 0, 0, 32,
             "Undocumented."),
    PREF_INT("flac.min_residual_partition_order", 2, 0, 12,
             "Undocumented."),
    PREF_INT("flac.max_residual_partition_order", 2, 0, 12,
             "Undocumented."),
    PREF_BOOL("flac.do_mid_side_stereo", 1,
              "Undocumented."),
    PREF_BOOL("flac.loose_mid_side_stereo", 0,
              "Undocumented."),
    PREF_INT("flac.compression_level", 4, 0, 8,
             "The FLAC compression level. Higher values yield higher "
             "compression ratios at the expense of slowing down the "
             "encoding process."),

};

static int
flac_module_init(int id) {
    self_id = id;
    pref_register(sizeof(default_prefs) / sizeof(default_prefs[0]),
                  default_prefs);
    pref_load("flac.*");
    file_register_driver(&flac_driver);
    return 0;
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "FLAC driver",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2004",
    "GPL",
    NULL,
    MODULE_FLAG_FACELESS,

    flac_module_init,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

#endif /* HAVE_FLAC */
