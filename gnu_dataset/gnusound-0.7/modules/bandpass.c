/*
 * GNUsound - a sound editor for GNOME.
 * Copyright (C) 2002-2005  Pascal Haakmat <a.haakmat@chello.nl>
 * Portions Copyright 1991 Lance Norskog And Sundry Contributors.
 * Some of this code was taken and slightly rewritten from the
 * soxgamma distribution, file band.c.
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

#include <gnusound.h>

#define BANDPASS_GLADE_FILE "bandpass" GUI_GTK_VERSION_TAG ".glade"

struct filter_params {
    double frequency;
    double width;
    double gain;
};

static int self_id = 0;

/*
 * Slightly rewritten version of the band_start() function from the
 * soxgamma distribution.
 */

/*
 * July 5, 1991
 * Copyright 1991 Lance Norskog And Sundry Contributors
 * This source code is freely redistributable and may be used for
 * any purpose.  This copyright notice must be maintained. 
 * Lance Norskog And Sundry Contributors are not responsible for 
 * the consequences of using this software.
 */

/*
 * Sound Tools Bandpass effect file.
 *
 * Algorithm:  2nd order recursive filter.
 * Formula stolen from MUSIC56K, a toolkit of 56000 assembler stuff.
 * Quote:
 *   This is a 2nd order recursive band pass filter of the form.                
 *   y(n)= a * x(n) - b * y(n-1) - c * y(n-2)   
 *   where :    
 *        x(n) = "IN"           
 *        "OUT" = y(n)          
 *        c = EXP(-2*pi*cBW/S_RATE)             
 *        b = -4*c/(1+c)*COS(2*pi*cCF/S_RATE)   
 *   if cSCL=2 (i.e. noise input)               
 *        a = SQT(((1+c)*(1+c)-b*b)*(1-c)/(1+c))                
 *   else       
 *        a = SQT(1-b*b/(4*c))*(1-c)            
 *   endif      
 *   note :     cCF is the center frequency in Hertz            
 *        cBW is the band width in Hertz        
 *        cSCL is a scale factor, use 1 for pitched sounds      
 *   use 2 for noise.           
 */

static inline void
calc_coeffs(double rate,
            double frequency,
            double width,
            double *ap,
            double *bp,
            double *cp) {
    double a, b, c;
    int noise = 0;

    c = exp(-2 * M_PI * width / rate);
    b = -4 * c / (1 + c) * cos(2 * M_PI * frequency / rate);
    if(noise)
        a = sqrt(((1 + c) * (1 + c) - b * b) * (1 - c) / (1 + c));
    else
        a = sqrt(1 - b * b / (4 * c)) * (1 - c);
    *ap = a;
    *bp = b;
    *cp = c;
}

static void
bandpass(shell *shl,
         int track,
         AFframecount offset,
         AFframecount count,
         double frequency,
         double width) {
    int32_t *out_buffer = mem_calloc(1, EFFECT_BUF_SIZE * sizeof(int32_t));
    int32_t out, out1, out2;
    double a, b, c;
    int i;
    ITERATOR_INIT(offset, count);

    if(!out_buffer) {
        FAIL("not enough memory for out buffer (%d bytes)\n",
             EFFECT_BUF_SIZE * sizeof(int32_t));
        ITERATOR_EXIT();
        return;
    }

    out1 = out2 = 0;
    ITERATOR(shl, shl->clip->sr->tracks[track],
             /*
              * This for-loop is a slightly rewritten version of the
              * band_flow() function in the soxgamma distribution.
              */
             for(i = 0; i < iter_read; i++) {
                 calc_coeffs(shl->clip->sr->rate, frequency, width, &a, &b, &c);
                 out = (a * int32_frame_bits[i] - b * out1) - c * out2;
                 out2 = out1;
                 out1 = out;
                 out_buffer[i] = out;
             }
             track_replace_samples_from(shl->clip->sr->tracks[track],
                                        SAMPLE_TYPE_INT_32,
                                        out_buffer,
                                        iter_frame_offset,
                                        iter_read));
    free(out_buffer);
    ITERATOR_EXIT();
    return;
}

static void
bandpass_dialog_apply(struct dialog *dialog,
                      void *user_data) {
    struct filter_params *p = mem_alloc(sizeof(*p));

    g_return_if_fail(p != NULL);

    p->frequency = gtk_spin_button_get_value(GTK_SPIN_BUTTON(pane_get_widget(dialog->pane, "frequency")));
    p->width = gtk_spin_button_get_value(GTK_SPIN_BUTTON(pane_get_widget(dialog->pane, "width")));

    shell_dispatch_as(dialog->shl, 
                      CMD_NEW("execute-module", 
                              cmd_new_shellp_val(dialog->shl),
                              cmd_new_int_val(self_id),
                              cmd_new_voidp_val_with_dtor(p, cmd_voidp_dtor)),
                      module_get(self_id)->name);
}

static void
bandpass_dialog_close(struct dialog *dialog,
                      void *user_data) {
    struct module_state *module_state = 
        shell_get_module_state(dialog->shl, self_id);

    module_state->is_open = 0;
    dialog_destroy(dialog);
}

static struct cmd_value *
bandpass_open(int id,
              shell *shl) {
    char path[4096];
    struct module_state *module_state;
    struct dialog *dialog;

    snprintf(path, 4096, "%s/%s", module_get_path(id), BANDPASS_GLADE_FILE);

    dialog = dialog_new(sizeof(*dialog),
                        path,
                        "dialog",
                        shl,
                        0,
                        NULL,
                        NULL);
    
    if(!dialog) 
        return cmd_new_error_val("Could not load interface %s.", path);

    dialog->apply = bandpass_dialog_apply;
    dialog->close = bandpass_dialog_close;

    module_state = shell_get_module_state(shl, id);
    module_state->is_open = 1;
    module_state->data = dialog;
    
    return cmd_new_void_val();
}

static void
bandpass_close(int id,
               shell *shl) {
    struct module_state *module_state = shell_get_module_state(shl, id);
    dialog_destroy((struct dialog *)module_state->data);
}

static struct cmd_value *
bandpass_execute(int id,
                 shell *shl,
                 void *data) {
    AFframecount offset = shl->select_start,
        count = shl->select_end - offset;
    int map = shl->select_channel_map;
    int t;
    double frequency, width;
    struct filter_params *p = data;
    struct cmd_value *r;
    struct cmd *cmd;
    const char *s;

    if(!p) 
        return cmd_new_error_val("Bandpass does not have defaults.");
    
    /* FIXME: deny this in interface (as well). */

    if(p->frequency > shl->clip->sr->rate / 2) 
        return cmd_new_error_val("Bandpass: frequency must be < %f.", 
                                 shl->clip->sr->rate / 2);

    if((s = constraints_test(shl->constraints,
                             region_new(map, offset, count),
                             CONSTRAINTS_OPER_REPLACE)))
        return cmd_new_error_val("Cannot do Bandpass because region is locked "
                                 "(%s)", s);
    
    constraints_push(shl->constraints,
                     "Bandpass Filter",
                     region_new(map, offset, count),
                     (CONSTRAIN_POSITION | 
                      CONSTRAIN_LENGTH | 
                      CONSTRAIN_CONTENTS));
    
    frequency = p->frequency;
    width = p->width;

    /* Preserve the selection. */
    
    cmd = CMD_NEW("preserve-snd",
                  cmd_new_shellp_val(shl),
                  cmd_new_int_val(map),
                  cmd_new_long_val(offset),
                  cmd_new_long_val(count));
    if(cmd_do_or_fail(cmd, "Cannot preserve region (%s)", &r)) {
        constraints_pop(shl->constraints);
        return r;
    }
    cmd_destroy_value(r);
        
    rwlock_rlock(&shl->clip->sr->rwl);

    for(t = 0; t < snd_track_count(shl->clip->sr); t++) 
        if((1 << t) & map)
            bandpass(shl,
                     t,
                     offset,
                     count,
                     frequency,
                     width);
    rwlock_runlock(&shl->clip->sr->rwl);

    constraints_pop(shl->constraints);

    return cmd_new_void_val();
}

static int
bandpass_init(int id) {
    self_id = id;
    return 0;
}

struct gnusound_module manifest = {
    MODULE_MAGIC,
    MODULE_API_VERSION_4,
    "Bandpass",
    "0.7",
    "Pascal Haakmat",
    "Copyright (C) 2002-2005",
    "GPL",
    NULL,
    0,

    bandpass_init,
    NULL,
    bandpass_open,
    bandpass_execute,
    bandpass_close,
    NULL,
    NULL
};

