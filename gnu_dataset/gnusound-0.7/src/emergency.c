/*
 * GNUsound - a sound editor for GNOME.
 * Copyright (C) 2005  Pascal Haakmat <a.haakmat@chello.nl>
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
 * A copy of the GNU General Public License can be found in the file
 * LICENSE in the top directory of the source distribution. If not,
 * write to the Free Software * Foundation, Inc., 675 Mass Ave,
 * Cambridge, MA 02139, USA.
 *
 */

#include <config.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <pwd.h>
#include <errno.h>
#include <signal.h>
#include <limits.h>
#include <time.h>
#ifdef HAVE_BACKTRACE
#include <execinfo.h>
#endif

#include "shell.h"
#include "arbiter.h"
#include "emergency.h"

static char logfile[8192];
static int logfd = -1;
static void *mixbufi, *mixbufn[MAX_TRACKS];
static mixer *outputmixer;
sig_atomic_t is_emergency = 0;

/**
 * @file 
 *
 * Some functions to handle emergency situations.  We avoid anything
 * that depends on state which may have been corrupted
 * (i.e. everything that might allocate memory, or use locale-data, or
 * depend on locks, ...). 
 *
 * This leaves memory corruptions in the GNUsound data structures as
 * our biggest worry, so try to touch only what we absolutely
 * need. There are some rudimentary data integrity checks but this
 * could be improved.
 */

/*
 * C string replacements.
 */

static void
emergency_strcpy(char *dst, const char *src) {
    while(*src)
        *dst++ = *src++;
    *dst = '\0';
}

static size_t
emergency_strlen(const char *s) {
    size_t len = 0;
    while(*s) { len++; s++; }
    return len;
}

static char *
emergency_strrchr(const char *s, int c) {
    int i = 0;

    while(s[i])
        i++;

    for(i = i - 1; i >= 0; i--) 
        if(s[i] == c)
            return (char *)&s[i];

    return NULL;

}

/*
 * stdio-free logging and output functions.
 */

#define write_string(fd, str) write((fd), (str), strlen(str))

static int
emergency_logfd() {
    static int logfd = -1;

    if(logfd == -1) {
        
        logfd = open(logfile, O_WRONLY | O_APPEND | O_CREAT, 0600);
        
        if(logfd != -1) {

            write_string(logfd, "\n");

        } else {

            write_string(2, "*** Could not open logfile ***\n");

        }
    }

    return logfd;

}

static void
emergency_print(const char *s) {

    logfd = emergency_logfd();

    if(logfd != -1) 
        write_string(logfd, s);

    write_string(2, s);
}

static void
emergency_print_long_long(long long i) {
    long long digit, pos, negative = 0;
    char s[64] = { 0 }; /* large enough to hold repr. of ~200 bit number */

    if(i == 0) {

        emergency_print("0");
        return;

    }

    if(i < 0) 
        negative = 1;

    for(pos = (sizeof(s) / sizeof(s[0])) - 2; i != 0; pos--) {

        digit = i % 10;
        i -= digit;
        i /= 10;
        digit = digit < 0 ? -digit : digit;
        s[pos] = digit + '0';

    }

    if(negative) 
        s[pos--] = '-';
        
    emergency_print(&s[pos+1]);
}

static void
emergency_print_int(int i) {
    emergency_print_long_long(i);
}

static int
is_leap_year(int year) {
    int is_leap_year = 0;
    
    if(year % 4 == 0) 
        is_leap_year = 1;
    
    if(year % 100 == 0 && year % 400 != 0)
        is_leap_year = 0;

    return is_leap_year;
}

static void
emergency_print_time_t(time_t secs) {
    time_t days = secs / 86400, total_days = days;
    int year = 1970, month, hour, minute;
    int days_in_month[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
    static const char *months[12] = { 
        "January", "February", "March", "April", "May", "June",
        "July", "August", "September", "November", "December" 
    };

    while(days > 366) {

        days -= 365 + is_leap_year(year);
        year++;

    }

    days_in_month[1] += is_leap_year(year);

    for(month = 0; month < 12 && days > days_in_month[month]; month++) 
        days -= days_in_month[month];

    emergency_print_int(days + 1);
    emergency_print(" ");
    emergency_print(months[month]);
    emergency_print(" ");
    emergency_print_int(year);
    emergency_print(" ");

    secs -= total_days * 86400;

    hour = secs / 3600;
    secs -= hour * 3600;

    if(hour < 10)
        emergency_print_int(0);
    emergency_print_int(hour);
    emergency_print(":");

    minute = secs / 60;
    secs -= minute * 60;
    if(minute < 10)
        emergency_print_int(0);
    emergency_print_int(minute);
    emergency_print(":");

    if(secs < 10)
        emergency_print_int(0);
    emergency_print_int(secs);

}

static void
emergency_log(const char *s) {
    time_t now = time(NULL);

    emergency_print_time_t(now);
    emergency_print("    ");
    emergency_print(s);
}

static void
emergency_print_backtrace(void) {
#ifdef HAVE_BACKTRACE
    void *bt[1024];
    int bt_size;
    int logfd;

    logfd = emergency_logfd();

    if(logfd != -1) {
        
        emergency_log("------ Dumping backtrace to logfile ------\n");
        bt_size = backtrace(bt, 1024);
        backtrace_symbols_fd(bt, bt_size, logfd);

    }
#endif
}

/*
 * File functions.
 */

static int
emergency_create_file(const char *template) {
    int fd;
    char path[4096], *p;

    if(!template || !strlen(template)) {
        
        emergency_strcpy(path, "Untitled.crashed.wav");

    } else { 
        
        if(strlen(template) >= sizeof path - 14) {

            emergency_print("Name too long\n");
            emergency_strcpy(path, "Untitled.crashed.wav");

        } else {

            emergency_strcpy(path, template);
            p = emergency_strrchr(path, '.');
            if(!p) 
                p = path + emergency_strlen(path);
            
            emergency_strcpy(p, ".crashed.wav");

        }
        
    }

    fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0600);

    if(fd == -1) {

        emergency_log("Could not open dump file ");
        emergency_print(path);
        emergency_print("\n");

    } else {

        emergency_log("Writing ");
        emergency_print(path);
        emergency_print("...");

    }

    return fd;

}

/*
 * Following code adapted from wav.c in the sndfile distribution.
 */

static void 
emergency_wave_begin(int fd, 
                     enum sample_type sample_type,
                     float rate,
                     int chans,
                     size_t frame_count) {
    FACT_CHUNK fact_chunk;
    int has_fact;
    u_int32_t dword, sample_rate, bytes_per_sec;
    u_int16_t word, format, channels, bytes_per_sample, bits_per_sample;
    u_int32_t fmt_length, RIFFsize;
    unsigned int dataoffset, datalength, filelength;

    has_fact = 0;
    sample_rate = rate;
    channels = chans;
    bytes_per_sample = sample_get_width(sample_type);
    bits_per_sample = bytes_per_sample * 8;
    bytes_per_sec = sample_rate * bytes_per_sample * channels;
    format = WAVE_FORMAT_PCM;

    if(sample_type == SAMPLE_TYPE_FLOAT_32) {
        format = WAVE_FORMAT_IEEE_FLOAT;
        has_fact = 1;
    }

    fmt_length = sizeof(MIN_WAV_FMT) + (has_fact ? sizeof(FACT_CHUNK) : 0);
    dataoffset = (has_fact ? 9 : 7) * sizeof(dword) + fmt_length;
    datalength = bytes_per_sample * channels * frame_count;
    filelength = dataoffset + datalength;

    RIFFsize = filelength - 2 * sizeof(dword);

    /* RIFF */

    dword = RIFF_MARKER;
    write(fd, &dword, sizeof(dword));
    dword = LE_INT(RIFFsize) ;
    write(fd, &dword, sizeof(dword));

    /* WAVE */

    dword = WAVE_MARKER;
    write(fd, &dword, sizeof(dword));

    /* fmt  */

    dword = fmt_MARKER;
    write(fd, &dword, sizeof(dword));
    dword = LE_INT(sizeof(MIN_WAV_FMT));
    write(fd, &dword, sizeof (dword));

    word = LE_SHORT(format);
    write(fd, &word, sizeof(word));
    word = LE_SHORT(channels);
    write(fd, &word, sizeof(word));
    dword = LE_INT(sample_rate);
    write(fd, &dword, sizeof(dword));
    dword = LE_INT(bytes_per_sec);
    write(fd, &dword, sizeof(dword));
    word = LE_SHORT(bytes_per_sample);
    write(fd, &word, sizeof(word));
    word = LE_SHORT(bits_per_sample);
    write(fd, &word, sizeof(word));
    
    if(has_fact) {

        /* fact */

        dword = fact_MARKER;
        write(fd, &dword, sizeof(dword));
        dword = LE_INT(sizeof(FACT_CHUNK));
        write(fd, &dword, sizeof(dword));
        fact_chunk.samples = LE_INT(frame_count);
        write(fd, &fact_chunk, sizeof(fact_chunk));
    }

    /* data */

    dword = data_MARKER;
    write(fd, &dword, sizeof(dword));
    dword = LE_INT(datalength);
    write(fd, &dword, sizeof(dword));

}

static void 
emergency_wave_end(int fd,
                   size_t count) {

    if (fd != 1)
        close(fd);
}

/*
 * End of code adapted from wav.c
 */


static void
int16_to_le(int16_t *buf,
            int count) {
#if __BYTE_ORDER == __LITTLE_ENDIAN
#elif __BYTE_ORDER == __BIG_ENDIAN
    int i;
    
    for(i = 0; i < count; i++)
        buf[i] = bswap_16(buf[i]);
#else
# error "Unsupported endian"
#endif
}

static void
int32_to_le(int32_t *buf,
            int count) {
#if __BYTE_ORDER == __LITTLE_ENDIAN
#elif __BYTE_ORDER == __BIG_ENDIAN
    int i;
    
    for(i = 0; i < count; i++)
        buf[i] = bswap_32(buf[i]);
#else
# error "Unsupported endian"
#endif
}

static void
float32_to_le(float *buf,
              int count) {
#if __BYTE_ORDER == __LITTLE_ENDIAN
#elif __BYTE_ORDER == __BIG_ENDIAN
    int i;
    
    for(i = 0; i < count; i++)
        buf[i] = bswap_32(buf[i]);
#else
# error "Unsupported endian"
#endif
}

static void 
emergency_save(shell *shl) {
    int64_t offset, count, total_written = 0, got;
    ssize_t written;
    int fd;
    snd *sr = shl->clip->sr;

    fd = emergency_create_file(shl->file->name);
    if(fd == -1) 
        return;

    emergency_wave_begin(fd, sr->sample_type, sr->rate, sr->channels,
                         snd_frame_count(sr, MAP_ALL));

    mixer_configure(outputmixer, sr->channels, sr->channels);

    count = snd_frame_count(sr, MAP_ALL);
    offset = 0;
    got = 1;
    while(count && got) {

        if(total_written % (EMERGENCY_BUF_SIZE * 256) == 0)
            emergency_print(".");

        memset(mixbufi, 
               '\0', 
               (EMERGENCY_BUF_SIZE * sample_get_width(sr->sample_type) * 
                outputmixer->output_channels));
        
        got = snd_getn(sr, mixbufn, MAP_ALL, offset, EMERGENCY_BUF_SIZE);
        
        mixer_mixi(outputmixer, sr->sample_type, mixbufi, mixbufn, got);
        
        switch(sr->sample_type) {
        case SAMPLE_TYPE_INT_8:
            /* shut up gcc */
            break;
        case SAMPLE_TYPE_INT_16:
            int16_to_le(mixbufi, got);
            break;
        case SAMPLE_TYPE_INT_32:
            int32_to_le(mixbufi, got);
            break;
        case SAMPLE_TYPE_FLOAT_32:
            float32_to_le(mixbufi, got);
            break;
        }

        written = write(fd, mixbufi, (sample_get_width(sr->sample_type) *
                                      sr->channels * got));
        if(written == -1) 
            break;
        
        total_written += (written / (sample_get_width(sr->sample_type) *
                                     sr->channels));
        count -= got;
        offset += got;

    }

    emergency_wave_end(fd, (sample_get_width(sr->sample_type) *
                            total_written * sr->channels)); 

    emergency_print("done (");
    emergency_print_long_long(total_written);
    emergency_print(" of ");
    emergency_print_long_long(snd_frame_count(sr, MAP_ALL));
    emergency_print(" frames)\n");

    if(count || total_written != snd_frame_count(sr, MAP_ALL)) 
        emergency_log("Not all frames were written!\n");


}

static int
emergency_verify_shell_integrity(shell *shl) {
    if(shl->magic[0] != 'O' || shl->magic[1] != 'k') {
        emergency_log("Magic failed\n");
        return 1;
    }

    if(!snd_verify(shl->clip->sr)) {
        emergency_log("Not saving (verify failed)\n");
        return 1;
    }

    if(!shl->has_changed) {
        emergency_log("Not saving ");
        emergency_print(shl->file->name);
        emergency_print(" (not changed)\n");
        return 1;
    }

    return 0;
}

/*
 * Signal handler.
 */

static int signals_handled[] = { 
    SIGFPE, SIGSEGV, SIGBUS, SIGTERM, SIGABRT, SIGILL 
};
static struct sigaction oldactions[sizeof(signals_handled) / sizeof(signals_handled[0])];

RETSIGTYPE
emergency_sighandler(int sig, 
                     siginfo_t *siginfo, 
                     void *ptr) {
    static int count = 0;
    static int shell_being_salvaged = 0, n_shells = 0, original_signo;
    static shell *shells[100];
    GList *l;
    int i;

    /* Try to detect "infinite" loops. */

    if(count++ > 100) 
        goto out;

    emergency_log("Received signal ");
    emergency_print_int(siginfo->si_signo);
    emergency_print("\n");

    if(!is_emergency) {

        /* First time around build a table of all active shells. */

        for(l = arbiter_get_shells(); l && 
                n_shells < sizeof(shells) / sizeof(shells[0]); l = l->next) 
            if(l->data != NULL)
                shells[n_shells++] = l->data;

        emergency_log("GNUsound crashed or was killed.\n");
        emergency_print_backtrace();
        emergency_log("Trying to save unsaved data...\n");
        is_emergency = 1;
        original_signo = siginfo->si_signo;

    } else {

        /* If we get a signal while we were trying to salvage a shell,
           try to continue with the next shell. */

        emergency_log("Caught signal while trying to recover from "
                      "previous signal.\n");
        shell_being_salvaged++;

    }

    for(i = shell_being_salvaged; i < n_shells; i++) {

        shell_being_salvaged = i;
        
        emergency_log("Attempting to salvage shell ");
        emergency_print_int(shell_being_salvaged + 1);
        emergency_print(" of ");
        emergency_print_int(n_shells);
        emergency_print("\n");
        
        if(emergency_verify_shell_integrity(shells[i])) 
            continue;
        
        emergency_save(shells[i]);
    }

    emergency_log("Finished, continuing with default signal handler\n");

    for(i = 0; i < sizeof(signals_handled) / sizeof(signals_handled[0]); i++) 
        if(signals_handled[i] == original_signo)
            break;

 out:

    sigaction(original_signo, &oldactions[i], NULL);
    kill(0, original_signo);
}

/*
 * Setup and initialization.
 */

void
emergency_get_sighandlers() {
    int i;
    for(i = 0; i < sizeof(signals_handled) / sizeof(signals_handled[0]); i++) 
        sigaction(signals_handled[i], NULL, &oldactions[i]);
}

int
emergency_install_sighandlers() {
    int i;
    struct sigaction act;

    act.sa_flags = SA_SIGINFO | SA_NOMASK;
    sigemptyset(&act.sa_mask);
    act.sa_sigaction = emergency_sighandler;
    for(i = 0; i < sizeof(signals_handled) / sizeof(signals_handled[0]); i++) {
        if(sigaction(signals_handled[i], &act, NULL) == -1) {
            FAIL("could not install signal handler for signal %d\n",
                 signals_handled[i]);
            return 1;
        }
    }
    return 0;
}

void
emergency_exit() {
    int i;

    mixer_buffers_free(MAX_TRACKS, mixbufi, mixbufn);
    mixer_destroy(outputmixer);

    for(i = 0; i < sizeof(signals_handled) / sizeof(signals_handled[0]); i++)
        sigaction(signals_handled[i], &oldactions[i], NULL);
}

int
emergency_init() {
    struct passwd *pwd;
    char *homedir;
    int r;

    pwd = getpwuid(getuid());
    homedir = (pwd ? pwd->pw_dir : getenv("HOME"));

    if(!homedir) {
        FAIL("could not get home directory\n");
        return 1;
    }

    r = snprintf(logfile, sizeof logfile, "%s/%s", homedir, ".gnusound");

    if(r >= sizeof(logfile) || r == -1) {
        FAIL("path is too long: %s/%s\n", homedir, ".gnusound");
        return 1;
    }

    r = mkdir(logfile, 0700);

    if(r == -1) {
        if(errno != EEXIST) {
            FAIL("could not create %s\n", logfile);
            return 1;
        }
    }

    r = snprintf(logfile, sizeof logfile, "%s/%s/%s", 
                 homedir, ".gnusound", "gnusound.log");

    if(r >= sizeof(logfile) || r == -1) {
        FAIL("path is too long: %s/%s/%s\n", 
             homedir, ".gnusound", "gnusound.log\n");
        return 1;
    }

    mixbufi = mixer_buffers_alloc(sample_get_width(SAMPLE_TYPE_FLOAT_32),
                                  MAX_TRACKS,
                                  &mixbufi,
                                  mixbufn,
                                  EMERGENCY_BUF_SIZE);
    
    if(!mixbufi) {
        FAIL("cannot allocate emergency mix buffers.\n");
        return 1;
    }

    outputmixer = mixer_new(MAX_TRACKS, MAX_TRACKS);

    if(!outputmixer) {
        FAIL("cannot create emergency mixer.\n");
        return 1;
    }

    outputmixer->is_unity = 1;

    if(emergency_install_sighandlers()) {
        FAIL("fatal error installing signal handlers.\n");
        return 1;
    }    

    return 0;
}
