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

#ifndef HAVE_SNDFILE
#warning "Not building sndfile support."
#else

#include <sndfile.h>

static struct file_driver sndfile_driver;

struct sndfile_data {
    SNDFILE *sf;
    SF_INFO info;
    enum sample_type sample_type;
};

static struct file_format *formats = NULL;

static int
sndfile_format_name_to_id(const char *format) {
    int i, count = 0;
    SF_FORMAT_INFO format_info;

    sf_command(NULL, SFC_GET_FORMAT_MAJOR_COUNT, &count, sizeof(int));
    
    for(i = 0; i < count; i++) {
        format_info.format = i;
        sf_command(NULL, SFC_GET_FORMAT_MAJOR, &format_info, 
                   sizeof(format_info));
        if(!strcmp(format_info.name, format))
            return format_info.format & SF_FORMAT_TYPEMASK;
    }
    return SF_FORMAT_WAV;
}

static struct cmd_value *
sndfile_attach(struct file *file,
               const char *format) {
    struct sndfile_data *sfd = mem_calloc(1, sizeof(*sfd));

    if(!sfd)
        return cmd_new_error_val("Cannot allocate data structure");

    file->driver = &sndfile_driver;
    file->driver_data = sfd;
    if(format == NULL)
        sfd->info.format = SF_FORMAT_WAV;
    else
        sfd->info.format = sndfile_format_name_to_id(format);

    return cmd_new_void_val();
}

static enum sample_type
sndfile_format_to_sample_type(int format) {
    enum sample_type sample_type;

    /* Find a sample format which corresponds (roughly) to the files
       sample format. */

    switch(format & SF_FORMAT_SUBMASK) {
    case SF_FORMAT_PCM_S8:
        sample_type = SAMPLE_TYPE_INT_16;
        break;
    case SF_FORMAT_PCM_16:
        sample_type = SAMPLE_TYPE_INT_16;
        break;
    case SF_FORMAT_PCM_24:
        sample_type = SAMPLE_TYPE_INT_32;
        break;
    case SF_FORMAT_PCM_32:
        sample_type = SAMPLE_TYPE_INT_32;
        break;
    case SF_FORMAT_PCM_U8:
        sample_type = SAMPLE_TYPE_INT_16;
        break;
    case SF_FORMAT_DPCM_8:
        sample_type = SAMPLE_TYPE_INT_8;
        break;
    case SF_FORMAT_DPCM_16:
        sample_type = SAMPLE_TYPE_INT_16;
        break;
    default:
        sample_type = SAMPLE_TYPE_FLOAT_32;
        break;
    }
    return sample_type;
}

static int 
sndfile_find_best_subformat(int format,
                            int preferred_sub_format) {
    int i, count;
    SF_INFO sfinfo;
    SF_FORMAT_INFO format_info ;

    /* First check if the major format can be used with the 
       preferred subformat. */

    sfinfo.channels = 1;
    sfinfo.samplerate = 0;
    sfinfo.format = (format & SF_FORMAT_TYPEMASK) | preferred_sub_format;
    if(sf_format_check(&sfinfo)) 
        return preferred_sub_format;

    /* No, look at other subformats. */

    sf_command(NULL, SFC_GET_FORMAT_SUBTYPE_COUNT, &count, sizeof (int));

    for(i = 0; i < count; i++) {
        format_info.format = i;

        /* Pray that the first subformat returned is the best
           choice. */
        
        sf_command(NULL, SFC_GET_FORMAT_SUBTYPE, &format_info, 
                   sizeof(format_info));
        sfinfo.format = (format & SF_FORMAT_TYPEMASK) | format_info.format;
        if(sf_format_check(&sfinfo)) 
            return format_info.format;
    }

    /* Well, nothing found, return the preferred subformat. */

    return preferred_sub_format;
}

static struct cmd_value *
sndfile_open(struct file *file, 
             const char *mode,
             struct file_params *params) {
    struct sndfile_data *sfd = file->driver_data;
    int rw;

    if(mode[0] == 'r') {
        rw = SFM_READ;
        sfd->info.format = 0;
    } else {
        rw = SFM_WRITE;
        sfd->info.samplerate = params->sample_rate;
        sfd->info.channels = params->channels;
        sfd->sample_type = params->sample_type;

        /* 
         * If the format is missing a subformat (because the file was
         * never saved before and no detailed options were specified),
         * try to set one that is both close to our sound
         * representation as well as compatible with the file format.
         */

        if(!(sfd->info.format & SF_FORMAT_SUBMASK)) {
            switch(params->sample_type) {
            case SAMPLE_TYPE_INT_8:
                sfd->info.format |=
                    sndfile_find_best_subformat(sfd->info.format, 
                                                SF_FORMAT_PCM_S8);
                break;
            case SAMPLE_TYPE_INT_16:
                sfd->info.format |=
                    sndfile_find_best_subformat(sfd->info.format, 
                                                SF_FORMAT_PCM_16);
                break;
            case SAMPLE_TYPE_INT_32:
                sfd->info.format |=
                    sndfile_find_best_subformat(sfd->info.format, 
                                                SF_FORMAT_PCM_32);
                break;
            case SAMPLE_TYPE_FLOAT_32:
                sfd->info.format |=
                    sndfile_find_best_subformat(sfd->info.format, 
                                                SF_FORMAT_FLOAT);
                break;
            }
        }
    }
    
    sfd->sf = sf_open(file->name, rw, &sfd->info);

    if(!sfd->sf)
        return cmd_new_error_val("Cannot open %s: %s", file->name,
                                 sf_strerror(NULL));

    /* Fill in the parameters of the opened file. */
    
    if(mode[0] == 'r') {
        params->sample_type = sndfile_format_to_sample_type(sfd->info.format);
        params->channels = sfd->info.channels;
        params->sample_rate = sfd->info.samplerate;
        params->frame_count = sfd->info.frames;
        sfd->sample_type = params->sample_type;
    }

    return cmd_new_void_val();
}

static long
sndfile_read(struct file *file,
        void *buf,
        long count) {
    struct sndfile_data *sfd = file->driver_data;
    sf_count_t r = 0;

    switch(sfd->sample_type) {
    case SAMPLE_TYPE_INT_8:
        r = sf_read_raw(sfd->sf, buf, count * sfd->info.channels);
        break;
    case SAMPLE_TYPE_INT_16:
        r = sf_readf_short(sfd->sf, buf, count);
        break;
    case SAMPLE_TYPE_INT_32:
        r = sf_readf_int(sfd->sf, buf, count);
        break;
    case SAMPLE_TYPE_FLOAT_32:
        r = sf_readf_float(sfd->sf, buf, count);
        break;
    }

    return r;
}

static long
sndfile_write(struct file *file,
         void *buf,
         long count) {
    struct sndfile_data *sfd = file->driver_data;
    sf_count_t r = 0;

    switch(sfd->sample_type) {
    case SAMPLE_TYPE_INT_8:
        r = sf_write_raw(sfd->sf, buf, count * sfd->info.channels);
        break;
    case SAMPLE_TYPE_INT_16:
        r = sf_writef_short(sfd->sf, buf, count);
        break;
    case SAMPLE_TYPE_INT_32:
        r = sf_writef_int(sfd->sf, buf, count);
        break;
    case SAMPLE_TYPE_FLOAT_32:
        r = sf_writef_float(sfd->sf, buf, count);
        break;
    }

    return r;    
}

static struct cmd_value *
sndfile_close(struct file *file) {
    struct sndfile_data *sfd = file->driver_data;

    if(sf_close(sfd->sf))
        return cmd_new_error_val("Error closing %s", file->name);

    return cmd_new_void_val();
}

static void
sndfile_detach(struct file *file) {
    file->driver = NULL;
    mem_free(file->driver_data);
}

int
sndfile_snprint(struct file *file, 
                enum file_property what, 
                char *buf, 
                int buflen) {
    SF_FORMAT_INFO format_info;
    struct sndfile_data *sfd = file->driver_data;

    format_info.format = sfd->info.format;
    if(sf_command(sfd->sf, SFC_GET_FORMAT_INFO, &format_info, 
                  sizeof(format_info))) {
        FAIL("failed\n");
        return -1;
    }

    switch(what) {
    case FILE_FORMAT:
    case FILE_DETAILED_FORMAT:
        return snprintf(buf, buflen, "%s", format_info.name);
    }

    return -1;
}

static int
sndfile_init_formats() {
    int i, count;
    SF_FORMAT_INFO format_info;

    if(formats == NULL) {

        sf_command(NULL, SFC_GET_FORMAT_MAJOR_COUNT, &count, sizeof(int));
        
        formats = mem_alloc((count + 1) * sizeof(struct file_format));

        for(i = 0; i < count; i++) {
            format_info.format = i;
            sf_command(NULL, SFC_GET_FORMAT_MAJOR, &format_info, 
                       sizeof(format_info));
            formats[i].name = mem_alloc(strlen(format_info.name) + 1);
            strcpy(formats[i].name, format_info.name);
            formats[i].extension = mem_alloc(strlen(format_info.extension) + 1);
            strcpy(formats[i].extension, format_info.extension);
        }
        formats[i].name = NULL;
        formats[i].extension = NULL;
        return 0;

    }
    
    return 0;
}

static const struct file_format *
sndfile_get_read_formats() {
    if(sndfile_init_formats()) 
        return NULL;

    return formats;
}

static const struct file_format *
sndfile_get_write_formats() {
    if(sndfile_init_formats()) 
        return NULL;

    return formats;
}

static struct file_driver sndfile_driver = {
    "sndfile",
    
    sndfile_attach,
    sndfile_open,
    sndfile_read,
    sndfile_write,
    sndfile_close,
    sndfile_detach,

    sndfile_snprint,
    
    sndfile_get_read_formats,
    sndfile_get_write_formats,

    NULL,
    NULL,
    NULL,

    NULL,
    NULL,
    NULL
};


static int
sndfile_module_init(int id) {
    file_register_driver(&sndfile_driver);
    return 0;
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "sndfile driver",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2004",
    "GPL",
    NULL,
    MODULE_FLAG_FACELESS,

    sndfile_module_init,
    NULL,
    NULL,
    NULL,
    NULL,    
    NULL,
    NULL
};

#endif /* HAVE_SNDFILE */
