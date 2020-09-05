/*
 * Copyright (C) 2002,2003 Pascal Haakmat.
 * Licensed under the GNU GPL.
 * Absolutely no warranty.
 */

#include <config.h>
#include <pthread.h>
#include <sys/time.h>
#include <unistd.h>
#include "lib/cpudetect.h"
#include "lib/misc.h"
#include "lib/fastmemcpy.h"
#include "tests.h"
#include "pref.h"
#include "mixer.h"
#include "muxers.h"
#include "mem.h"
#include "pref.h"
#include "fifo.h"

extern struct mixer_muxer_table muxtable_16[NUM_MUXERS][NUM_MUXERS];

struct mixer_mux_data {
    mixer *mixer;
    frame_bits_t dst;
    frame_bits_t *src;
    int frame_count;
    int frame_width;
};

CpuCaps gCpuCaps;
int Emergency;

/*
 * Benchmarks.
 */

void 
muxer_generic(void *arg) {
    struct mixer_mux_data *md = arg;
    mixer_mux_generic(md->mixer, md->dst, md->src, md->frame_width, md->frame_count);
}

void 
muxer_specific(void *arg) {
    struct mixer_mux_data *md = arg;
    muxtable_16[md->mixer->input_channels-1][md->mixer->output_channels-1].muxer(md->mixer, md->dst, md->src, md->frame_width, md->frame_count);
}

void
time_muxing(struct benchmark *b) {
    int i, max_tracks, num_targets = 1;
    int16_t *dst;
    int16_t *src[32];
    char buf[512];
    float diff[32];
    float gen, spec;
    struct mixer_mux_data md;
    snprintf(b->message, TESTS_MSGSZ, "not run, under construction");
    return;
    
    for(i = 0; i < 32; i++)
        src[i] = malloc(8192 * sizeof(int16_t));

    dst = malloc(8192 * sizeof(int16_t) * num_targets);

    max_tracks = pref_get_as_int("max_tracks");
    md.mixer = mixer_new(max_tracks, max_tracks);
    md.src = (frame_bits_t *)src;
    md.dst = dst;
    md.frame_count = 8192;
    md.frame_width = 2;
    
    for(i = 1; i <= (NUM_MUXERS < max_tracks ? NUM_MUXERS : max_tracks); i++) {
        mixer_configure(md.mixer, num_targets, i);
        muxer_generic(&md);
        snprintf(buf, 512, "muxer_%dto%d", i, num_targets);
        muxer_specific(&md);
        diff[i] = (gen / spec) * 100;
    }
    for(i = 1; i <= (NUM_MUXERS < max_tracks ? NUM_MUXERS : max_tracks); i++) 
        printf("%d %f\n", i, diff[i]);
}

void
time_memcpy(void *(*copier)(void *d, const void *s, size_t n)) {
    size_t sz = 1024*512;
    char s[sz], d[sz];
    copier(d, s, sz);
    copier(s, d, sz);
}

void
time_memcpy_c(struct benchmark *b) {
    time_memcpy(memcpy);
}

#ifdef HAVE_ARCH_X86
void
time_memcpy_mmx(struct benchmark *b) {
    if(gCpuCaps.hasMMX)
        time_memcpy(fast_memcpy_MMX);
    else
        snprintf(b->message, TESTS_MSGSZ, "not run, no MMX");
}

void
time_memcpy_mmx2(struct benchmark *b) {
    if(gCpuCaps.hasMMX2)
        time_memcpy(fast_memcpy_MMX2);
    else
        snprintf(b->message, TESTS_MSGSZ, "not run, no MMX2");
}

void
time_memcpy_3dnow(struct benchmark *b) {
    if(gCpuCaps.has3DNow)
        time_memcpy(fast_memcpy_3DNow);
    else
        snprintf(b->message, TESTS_MSGSZ, "not run, no 3DNow!");
}
#endif

/*
 * Tests.
 */

void
tests_printf(int level,
             char *fmt,
             ...) {
    va_list ap;
    va_start(ap, fmt);
    while(level-- * 2) 
        fprintf(stderr, " ");
    vfprintf(stderr, fmt, ap);
    va_end(ap);
}

int
tests_pref(void *arg) {
    return pref_tests();
}

int
tests_fifo(void *arg) {
    return fifo_tests(1);
}

/*
 * Testing harness.
 */

struct benchmark benchmarks[] = {
    { "C memcpy", time_memcpy_c, 100, { 0 }, NULL },
#ifdef HAVE_ARCH_X86
    { "MMX memcpy", time_memcpy_mmx, 100, { 0 }, NULL },
    { "MMX2 memcpy", time_memcpy_mmx2, 100, { 0 }, NULL },
    { "3DNow! memcpy", time_memcpy_3dnow, 100, { 0 }, NULL },
#endif
    { "mux", time_muxing, 100, { 0 }, NULL },
};

struct test tests[] = {
    { "pref", tests_pref, NULL },
    { "fifo", tests_fifo, NULL },
    { NULL, NULL, NULL }
};

int
do_tests(int argc, 
         char *argv[]) {
    int i, j, r = 0, errors = 0;
    for(i = 0; tests[i].name; i++) {
        if(argc) {
            for(j = 0; j < argc; j++)
                if(!strcmp(argv[j], tests[i].name))
                    break;
            if(j >= argc)
                continue;
        }
        printf("testing %s...\n", tests[i].name);
        errors = tests[i].func(tests[i].arg);
        printf("%d errors\n", errors);
        r += errors;
    }
    return r;
}

int
do_benchmarks(int argc,
              char *argv[]) {
    int i, j, it;
    struct timeval tv_start, tv_stop;
    double seconds;
    for(i = 0; i < sizeof(benchmarks) / sizeof(benchmarks[0]); i++) {

        if(argc) {
            for(j = 0; j < argc; j++)
                if(!strcmp(argv[j], benchmarks[i].name))
                    break;
            if(j >= argc)
                continue;
        }
        gettimeofday(&tv_start, NULL);
        for(it = 0; it < benchmarks[i].times; it++) {
            printf("benchmarking %s [%d/%d]...\r", benchmarks[i].name, it + 1, 
                   benchmarks[i].times);
            benchmarks[i].func(&benchmarks[i]);
        }
        gettimeofday(&tv_stop, NULL);
        printf("benchmarking %s [%d]...", benchmarks[i].name, 
               benchmarks[i].times);
        seconds = tv_diff_secs(&tv_start, &tv_stop);
        printf(" time: %fs", seconds);
        if(strlen(benchmarks[i].message))
            printf(" (%s)", benchmarks[i].message);
        printf("\n");
    }
    return 0;
}

int
main(int argc,
     char *argv[]) {
    int r = 0, i;

    GetCpuCaps(&gCpuCaps);

    if(argc < 2) {
        do_benchmarks(0, NULL);
        r = do_tests(0, NULL);
        return r;
    }

    switch(argv[1][0]) {
    case 'b':
        do_benchmarks(argc - 2, &argv[2]);
        break;
    case 't':
        r = do_tests(argc - 2, &argv[2]);
        break;
    default:
        fprintf(stderr, "Usage: tests\n"
                "       tests t TESTS...\n"
                "       tests b BENCHMARKS...\n");
        fprintf(stderr, "Possible tests are: ");
        
        for(i = 0; tests[i].name; i++)
            fprintf(stderr, "%s%s", tests[i].name, 
                    tests[i+1].name ? ", " : "");
        fprintf(stderr, "\n");
        fprintf(stderr, "Possible benchmarks are: ");
        
        for(i = 0; benchmarks[i].name; i++)
            fprintf(stderr, "%s%s", benchmarks[i].name, 
                    benchmarks[i+1].name ? ", " : "");
        fprintf(stderr, "\n");
        r = 1;
        break;
    }
    return r;
}
