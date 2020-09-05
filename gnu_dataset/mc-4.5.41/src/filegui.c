/* {{{ Copyright */

/* File managing.  Important notes on this file:
 *  
 * About the use of dialogs in this file:
 *   If you want to add a new dialog box (or call a routine that pops
 *   up a dialog box), you have to provide a wrapper for background
 *   operations (ie, background operations have to up-call to the parent
 *   process).
 *
 *   For example, instead of using the message() routine, in this
 *   file, you should use one of the stubs that call message with the
 *   proper number of arguments (ie, message_1s, message_2s and so on).
 *
 *   Actually, that is a rule that should be followed by any routines
 *   that may be called from this module.
 *
 */

/* File managing GUI for the text mode edition
 *
 * Copyright (C) 1994, 1995, 1996 The Free Software Foundation
 *  
 * Written by: 1994, 1995       Janne Kukonlehto
 *             1994, 1995       Fred Leeflang
 *             1994, 1995, 1996 Miguel de Icaza
 *             1995, 1996       Jakub Jelinek
 *	       1997             Norbert Warmuth
 *	       1998		Pavel Machek
 *
 * The copy code was based in GNU's cp, and was written by:
 * Torbjorn Granlund, David MacKenzie, and Jim Meyering.
 *
 * The move code was based in GNU's mv, and was written by:
 * Mike Parker and David MacKenzie.
 *
 * Janne Kukonlehto added much error recovery to them for being used
 * in an interactive program.
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
 */

/* }}} */

/* {{{ Include files */

#include <config.h>
/* Hack: the vfs code should not rely on this */
#define WITH_FULL_PATHS 1

#include <sys/types.h>
#include <dirent.h>
#include <stdio.h>
#ifdef NEEDS_IO_H
#    include <io.h>
#endif 

#include <errno.h>
#include "tty.h"
#include <ctype.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#   include <unistd.h>
#endif
#include <sys/stat.h>
#include <sys/param.h>
#include <fcntl.h>
#ifdef SCO_FLAVOR
#	include <sys/timeb.h>	/* alex: for struct timeb, used in time.h */
#endif /* SCO_FLAVOR */
#include <time.h>
#include <utime.h>
#include "eregex.h"
#include "global.h"
#include "setup.h"
#include "dialog.h"
/* Needed by query_replace */
#include "color.h"
#include "win.h"
#include "dlg.h"
#define WANT_WIDGETS
#include "widget.h"		/* Note the sequence */
#include "main.h"		/* WANT_WIDGETS-> we get the the_hint def */
#include "layout.h"
#include "wtools.h"

/* Needed for current_panel, other_panel and WTree */
#include "dir.h"
#include "panel.h"
#include "file.h"
#include "filegui.h"
#include "fileopctx.h"
#include "tree.h"
#include "key.h"
#include "../vfs/vfs.h"

#include "x.h"

/* }}} */

/* This structure describes the UI and internal data required by a file
 * operation context.
 */
typedef struct {
    /* ETA and bps */

    int showing_eta;
    int showing_bps;
    int eta_extra;

    /* Dialog and widgets for the operation progress window */

    Dlg_head *op_dlg;

    WLabel *file_label[2];
    WLabel *file_string[2];
    WLabel *progress_label[3];
    WGauge *progress_gauge[3];
    WLabel *eta_label;
    WLabel *bps_label;
    WLabel *stalled_label;

    /* Query replace dialog */

    Dlg_head *replace_dlg;
    char *replace_filename;
    int replace_result;
    struct stat *s_stat, *d_stat;
} FileOpContextUI;


/* Replace dialog: color set */
static int replace_colors [4];

#ifndef HAVE_X
/* Used to save the hint line */
static int last_hint_line;
#endif

/* File operate window sizes */
#define WX 62
#define WY 10
#define BY 10
#define WX_ETA_EXTRA  12

#define FCOPY_GAUGE_X 14
#define FCOPY_LABEL_X 5

/* Used for button result values */
enum {
    REPLACE_YES = B_USER,
    REPLACE_NO,
    REPLACE_APPEND,
    REPLACE_ALWAYS,
    REPLACE_UPDATE,
    REPLACE_NEVER,
    REPLACE_ABORT,
    REPLACE_SIZE,
    REPLACE_REGET
} FileReplaceCode;

static FileProgressStatus
check_progress_buttons (FileOpContext *ctx)
{
    int c;
    Gpm_Event event;
    FileOpContextUI *ui;

    if (ctx->ui == NULL)
	    return FILE_CONT;
    
    ui = ctx->ui;

    x_flush_events ();
    event.x = -1;         /* Don't show the GPM cursor */
    c = get_event (&event, 0, 0);
    if (c == EV_NONE)
      return FILE_CONT;
    dlg_process_event (ui->op_dlg, c, &event);
    switch (ui->op_dlg->ret_value) {
    case FILE_SKIP:
	return FILE_SKIP;
	break;
    case B_CANCEL:
    case FILE_ABORT:
	return FILE_ABORT;
	break;
    default:
	return FILE_CONT;
    }
}

/* {{{ File progress display routines */

static int
op_win_callback (struct Dlg_head *h, int id, int msg)
{
    switch (msg){
#ifndef HAVE_X    
    case DLG_DRAW:
	attrset (COLOR_NORMAL);
	dlg_erase (h);
	draw_box (h, 1, 2, h->lines-2, h->cols-4);
	return 1;
#endif
    }
    return 0;
}

void
file_op_context_create_ui (FileOpContext *ctx, FileOperation op, int with_eta)
{
    FileOpContextUI *ui;
    int x_size;
    int minus;
    int eta_offset;
    char *sixty;
    char *fifteen;

    g_return_if_fail (ctx != NULL);
    g_return_if_fail (ctx->ui == NULL);

    ui = g_new0 (FileOpContextUI, 1);
    ctx->ui = ui;

    minus = verbose ? 0 : 3;
    eta_offset = with_eta ? (WX_ETA_EXTRA) / 2 : 0;

    sixty = "";
    fifteen = "";

    ctx->recursive_result = 0;

    ui->replace_result = 0;
    ui->showing_eta = with_eta;
    ui->showing_bps = with_eta;
    ui->eta_extra = with_eta ? WX_ETA_EXTRA : 0;
    x_size = (WX + 4) + ui->eta_extra;

    ui->op_dlg = create_dlg (0, 0, WY-minus+4, x_size, dialog_colors,
			     op_win_callback, "", "opwin", DLG_CENTER);

#ifndef HAVE_X
    last_hint_line = the_hint->widget.y;
    if ((ui->op_dlg->y + ui->op_dlg->lines) > last_hint_line)
	the_hint->widget.y = ui->op_dlg->y + ui->op_dlg->lines+1;
#endif

    x_set_dialog_title (ui->op_dlg, "");

    add_widgetl (ui->op_dlg, button_new (BY-minus, WX - 19 + eta_offset, FILE_ABORT,
					 NORMAL_BUTTON, _("&Abort"), 0, 0, "abort"),
		 XV_WLAY_RIGHTOF);
    add_widgetl (ui->op_dlg, button_new (BY-minus, 14 + eta_offset, FILE_SKIP,
					 NORMAL_BUTTON, _("&Skip"), 0, 0, "skip"),
		 XV_WLAY_CENTERROW);

    add_widgetl (ui->op_dlg, ui->progress_gauge[2] = gauge_new (7, FCOPY_GAUGE_X, 0, 100, 0, "g-1"), 
		 XV_WLAY_RIGHTOF);
    add_widgetl (ui->op_dlg, ui->progress_label[2] = label_new (7, FCOPY_LABEL_X, fifteen, "l-1"), 
		 XV_WLAY_NEXTROW);
    add_widgetl (ui->op_dlg, ui->bps_label = label_new (7, WX, "", "bps-label"), XV_WLAY_NEXTROW);

    add_widgetl (ui->op_dlg, ui->progress_gauge[1] = gauge_new (8, FCOPY_GAUGE_X, 0, 100, 0, "g-2"), 
		 XV_WLAY_RIGHTOF);
    add_widgetl (ui->op_dlg, ui->progress_label[1] = label_new (8, FCOPY_LABEL_X, fifteen, "l-2"), 
		 XV_WLAY_RIGHTOF);
    add_widgetl (ui->op_dlg, ui->stalled_label = label_new (8, WX, "", "stalled"), XV_WLAY_NEXTROW);

    add_widgetl (ui->op_dlg, ui->progress_gauge[0] = gauge_new (6, FCOPY_GAUGE_X, 0, 100, 0, "g-3"), 
		 XV_WLAY_RIGHTOF);
    add_widgetl (ui->op_dlg, ui->progress_label[0] = label_new (6, FCOPY_LABEL_X, fifteen, "l-3"), 
		 XV_WLAY_RIGHTOF);
    add_widgetl (ui->op_dlg, ui->eta_label = label_new (6, WX, "", "eta_label"), XV_WLAY_NEXTROW);

    add_widgetl (ui->op_dlg, ui->file_string[1] = label_new (4, FCOPY_GAUGE_X, sixty, "fs-l-1"),
		 XV_WLAY_RIGHTOF);
    add_widgetl (ui->op_dlg, ui->file_label[1] = label_new (4, FCOPY_LABEL_X, fifteen, "fs-l-2"), 
		 XV_WLAY_NEXTROW);
    add_widgetl (ui->op_dlg, ui->file_string[0] = label_new (3, FCOPY_GAUGE_X, sixty, "fs-x-1"),
		 XV_WLAY_RIGHTOF);
    add_widgetl (ui->op_dlg, ui->file_label[0] = label_new (3, FCOPY_LABEL_X, fifteen, "fs-x-2"), 
		 XV_WLAY_NEXTROW);

    /* We will manage the dialog without any help, that's why
       we have to call init_dlg */
    init_dlg (ui->op_dlg);
    ui->op_dlg->running = 1;
}

void
file_op_context_destroy_ui (FileOpContext *ctx)
{
    FileOpContextUI *ui;

    g_return_if_fail (ctx != NULL);

    if (ctx->ui){
	    ui = ctx->ui;

	    dlg_run_done (ui->op_dlg);
	    destroy_dlg (ui->op_dlg);
	    g_free (ui);
    }

#ifndef HAVE_X
	    the_hint->widget.y = last_hint_line;
#endif

    ctx->ui = NULL;
}

static FileProgressStatus
show_no_bar (FileOpContext *ctx, int n)
{
    FileOpContextUI *ui;

    if (ctx->ui == NULL)
	    return FILE_CONT;
    
    ui = ctx->ui;

    if (n >= 0) {
    	label_set_text (ui->progress_label[n], "");
        gauge_show (ui->progress_gauge[n], 0);
    }
    return check_progress_buttons (ctx);
}

static FileProgressStatus
show_bar (FileOpContext *ctx, int n, long done, long total)
{
    FileOpContextUI *ui;

    if (ctx->ui == NULL)
	    return FILE_CONT;
    
    ui = ctx->ui;

    gauge_set_value (ui->progress_gauge[n], (int) total, (int) done);
    gauge_show (ui->progress_gauge[n], 1);
    return check_progress_buttons (ctx);
}

static void
file_eta_show (FileOpContext *ctx)
{
    int eta_hours, eta_mins, eta_s;
    char eta_buffer [BUF_TINY];
    FileOpContextUI *ui;

    if (ctx->ui == NULL)
	    return;
    
    ui = ctx->ui;

    if (!ui->showing_eta)
	return;
    
    if (ctx->eta_secs > 0.5) {
        eta_hours = ctx->eta_secs / (60 * 60);
	eta_mins  = (ctx->eta_secs - (eta_hours * 60 * 60)) / 60;
	eta_s     = ctx->eta_secs - (eta_hours * 60 * 60 + eta_mins * 60);
	g_snprintf (eta_buffer, sizeof (eta_buffer), "ETA %d:%02d.%02d", eta_hours, eta_mins, eta_s);
    } else
	*eta_buffer = 0;
    
    label_set_text (ui->eta_label, eta_buffer);
}

static void
file_bps_show (FileOpContext *ctx)
{
    char bps_buffer [BUF_TINY];
    FileOpContextUI *ui;

    if (ctx->ui == NULL)
	    return;

    ui = ctx->ui;

    if (!ui->showing_bps)
	return;

    if (ctx->bps > 1024*1024) {
        g_snprintf (bps_buffer, sizeof (bps_buffer), "%.2f MB/s", ctx->bps / (1024*1024.0));
    } else if (ctx->bps > 1024){
        g_snprintf (bps_buffer, sizeof (bps_buffer), "%.2f KB/s", ctx->bps / 1024.0);
    } else if (ctx->bps > 1){
        g_snprintf (bps_buffer, sizeof (bps_buffer), "%ld B/s", ctx->bps);
    } else
	*bps_buffer = 0;

    label_set_text (ui->bps_label, bps_buffer);
}

FileProgressStatus
file_progress_show (FileOpContext *ctx, long done, long total)
{
    FileOpContextUI *ui;

    g_return_val_if_fail (ctx != NULL, FILE_CONT);

    if (ctx->ui == NULL)
	    return FILE_CONT;

    ui = ctx->ui;

    if (!verbose)
	return check_progress_buttons (ctx);
    if (total > 0){
	label_set_text (ui->progress_label[0], _("File"));
	file_eta_show (ctx);
	file_bps_show (ctx);
	return show_bar (ctx, 0, done, total);
    } else
	return show_no_bar (ctx, 0);
}

FileProgressStatus
file_progress_show_count (FileOpContext *ctx, long done, long total)
{
    FileOpContextUI *ui;

    g_return_val_if_fail (ctx != NULL, FILE_CONT);

    if (ctx->ui == NULL)
	    return FILE_CONT;

    ui = ctx->ui;

    if (!verbose)
	return check_progress_buttons (ctx);
    if (total > 0){
	label_set_text (ui->progress_label[1], _("Count"));
	return show_bar (ctx, 1, done, total);
    } else
	return show_no_bar (ctx, 1);
}

FileProgressStatus
file_progress_show_bytes (FileOpContext *ctx, double done, double total)
{
    FileOpContextUI *ui;

    g_return_val_if_fail (ctx != NULL, FILE_CONT);

    if (ctx->ui == NULL)
	    return FILE_CONT;

    ui = ctx->ui;

    if (!verbose)
	return check_progress_buttons (ctx);
    if (total > 0){
	label_set_text (ui->progress_label[2], _("Bytes"));
	return show_bar (ctx, 2, done, total);
    } else
	return show_no_bar (ctx, 2);
}

/* }}} */

#ifndef HAVE_X
#define truncFileString(ui, s) name_trunc (s, ui->eta_extra + 47)
#else
#define truncFileString(ui, s) s
#endif

FileProgressStatus
file_progress_show_source (FileOpContext *ctx, char *s)
{
    FileOpContextUI *ui;

    g_return_val_if_fail (ctx != NULL, FILE_CONT);

    if (ctx->ui == NULL)
	    return FILE_CONT;

    ui = ctx->ui;

    if (s != NULL) {
#ifdef WITH_FULL_PATHS
    	int i = strlen (cpanel->cwd);

	/* We remove the full path we have added before */
        if (!strncmp (s, cpanel->cwd, i)){ 
            if (s[i] == PATH_SEP)
            	s += i + 1;
        }
#endif /* WITH_FULL_PATHS */
	
	label_set_text (ui->file_label[0], _("Source"));
	label_set_text (ui->file_string[0], truncFileString (ui, s));
	return check_progress_buttons (ctx);
    } else {
	label_set_text (ui->file_label[0], "");
	label_set_text (ui->file_string[0], "");
	return check_progress_buttons (ctx);
    }
}

FileProgressStatus
file_progress_show_target (FileOpContext *ctx, char *s)
{
    FileOpContextUI *ui;

    g_return_val_if_fail (ctx != NULL, FILE_CONT);

    if (ctx->ui == NULL)
	    return FILE_CONT;

    ui = ctx->ui;

    if (s != NULL) {
	label_set_text (ui->file_label[1], _("Target"));
	label_set_text (ui->file_string[1], truncFileString (ui, s));
	return check_progress_buttons (ctx);
    } else {
	label_set_text (ui->file_label[1], "");
	label_set_text (ui->file_string[1], "");
	return check_progress_buttons (ctx);
    }
}

FileProgressStatus
file_progress_show_deleting (FileOpContext *ctx, char *s)
{
    FileOpContextUI *ui;

    g_return_val_if_fail (ctx != NULL, FILE_CONT);

    if (ctx->ui == NULL)
	    return FILE_CONT;

    ui = ctx->ui;

    label_set_text (ui->file_label[0], _("Deleting"));
    label_set_text (ui->file_label[0], truncFileString (ui, s));
    return check_progress_buttons (ctx);
}

static int
replace_callback (struct Dlg_head *h, int Id, int Msg)
{
#ifndef HAVE_X

    switch (Msg){
    case DLG_DRAW:
	dialog_repaint (h, ERROR_COLOR, ERROR_COLOR);
	break;
    }
#endif
    return 0;
}

#ifdef HAVE_X
#define X_TRUNC 128
#else
#define X_TRUNC 52
#endif

/*
 * FIXME: probably it is better to replace this with quick dialog machinery,
 * but actually I'm not familiar with it and have not much time :(
 *   alex
 */
static struct
{
	char* text;
	int   ypos, xpos;	
	int   value;		/* 0 for labels */
	char* tkname;
	WLay  layout;
}
rd_widgets [] =
{
	{N_("Target file \"%s\" already exists!"),
	                 3,      4,  0,              "target-e",   XV_WLAY_CENTERROW},
	{N_("&Abort"),   BY + 3, 25, REPLACE_ABORT,  "abort",      XV_WLAY_CENTERROW},
	{N_("if &Size differs"),
	                 BY + 1, 28, REPLACE_SIZE,   "if-size",    XV_WLAY_RIGHTOF},
	{N_("non&E"),    BY,     47, REPLACE_NEVER,  "none",       XV_WLAY_RIGHTOF},
	{N_("&Update"),  BY,     36, REPLACE_UPDATE, "update",     XV_WLAY_RIGHTOF},
	{N_("al&L"),     BY,     28, REPLACE_ALWAYS, "all",        XV_WLAY_RIGHTOF},
	{N_("Overwrite all targets?"),
	                 BY,     4,  0,              "over-label", XV_WLAY_CENTERROW},
	{N_("&Reget"),   BY - 1, 28, REPLACE_REGET,  "reget",      XV_WLAY_RIGHTOF},
	{N_("ap&Pend"),  BY - 2, 45, REPLACE_APPEND, "append",     XV_WLAY_RIGHTOF},
	{N_("&No"),      BY - 2, 37, REPLACE_NO,     "no",         XV_WLAY_RIGHTOF},
	{N_("&Yes"),     BY - 2, 28, REPLACE_YES,    "yes",        XV_WLAY_RIGHTOF},
	{N_("Overwrite this target?"),
	                 BY - 2, 4,  0,              "overlab",    XV_WLAY_CENTERROW},
	{N_("Target date: %s, size %d"),
	                 6,      4,  0,              "target-date",XV_WLAY_CENTERROW},
	{N_("Source date: %s, size %d"),
	                 5,      4,  0,              "source-date",XV_WLAY_CENTERROW}
}; 

#define ADD_RD_BUTTON(i)\
	add_widgetl (ui->replace_dlg,\
		button_new (rd_widgets [i].ypos, rd_widgets [i].xpos, rd_widgets [i].value,\
		NORMAL_BUTTON, rd_widgets [i].text, 0, 0, rd_widgets [i].tkname), \
		rd_widgets [i].layout)

#define ADD_RD_LABEL(ui,i,p1,p2)\
	g_snprintf (buffer, sizeof (buffer), rd_widgets [i].text, p1, p2);\
	add_widgetl (ui->replace_dlg,\
		label_new (rd_widgets [i].ypos, rd_widgets [i].xpos, buffer, rd_widgets [i].tkname),\
		rd_widgets [i].layout)

static void
init_replace (FileOpContext *ctx, enum OperationMode mode)
{
    FileOpContextUI *ui;
    char buffer [BUF_SMALL];
    static int rd_xlen = 60, rd_trunc = X_TRUNC;

#ifdef ENABLE_NLS
    static int i18n_flag;
    if (!i18n_flag) {
	int l1, l2, l, row;
	register int i = sizeof (rd_widgets) / sizeof (rd_widgets [0]); 
	while (i--)
	    rd_widgets [i].text = _(rd_widgets [i].text);

	/* 
	 *longest of "Overwrite..." labels 
	 * (assume "Target date..." are short enough)
	 */
	l1 = max (strlen (rd_widgets [6].text), strlen (rd_widgets [11].text));

	/* longest of button rows */
	i = sizeof (rd_widgets) / sizeof (rd_widgets [0]);
	for (row = l = l2 = 0; i--;) {
	    if (rd_widgets [i].value != 0) {
		if (row != rd_widgets [i].ypos) {
		    row = rd_widgets [i].ypos;
		    l2 = max (l2, l);
		    l = 0;
		}
		l += strlen (rd_widgets [i].text) + 4;
	    }
	}
	l2 = max (l2, l); /* last row */
	rd_xlen = max (rd_xlen, l1 + l2 + 8);
	rd_trunc = rd_xlen - 6;

	/* Now place buttons */
	l1 += 5; /* start of first button in the row */
	i = sizeof (rd_widgets) / sizeof (rd_widgets [0]);
		
	for (l = l1, row = 0; --i > 1;) {
	    if (rd_widgets [i].value != 0) {
		if (row != rd_widgets [i].ypos) {
		    row = rd_widgets [i].ypos;
		    l = l1;
		}
		rd_widgets [i].xpos = l;
		l += strlen (rd_widgets [i].text) + 4;
	    }
	}
	/* Abort button is centered */
	rd_widgets [1].xpos = (rd_xlen - strlen (rd_widgets [1].text) - 3) / 2;
    }
#endif /* ENABLE_NLS */

    ui = ctx->ui;

    replace_colors [0] = ERROR_COLOR;
    replace_colors [1] = COLOR_NORMAL;
    replace_colors [2] = ERROR_COLOR;
    replace_colors [3] = COLOR_NORMAL;
    
    ui->replace_dlg = create_dlg (0, 0, 16, rd_xlen, replace_colors, replace_callback,
				  "[ Replace ]", "replace", DLG_CENTER);
    
    x_set_dialog_title (ui->replace_dlg,
			(mode == Foreground
			 ? _(" File exists ")
			 : _(" Background process: File exists ")));


    ADD_RD_LABEL(ui, 0,
		 name_trunc (ui->replace_filename, rd_trunc - strlen (rd_widgets [0].text)), 0);
    ADD_RD_BUTTON(1);

    ADD_RD_BUTTON(2);
    ADD_RD_BUTTON(3);
    ADD_RD_BUTTON(4);
    ADD_RD_BUTTON(5);
    ADD_RD_LABEL(ui, 6, 0, 0);

    /* "this target..." widgets */
    if (!S_ISDIR (ui->d_stat->st_mode)){
	if ((ui->d_stat->st_size && ui->s_stat->st_size > ui->d_stat->st_size))
	    ADD_RD_BUTTON(7);

	ADD_RD_BUTTON(8);
    }
    ADD_RD_BUTTON(9);
    ADD_RD_BUTTON(10);
    ADD_RD_LABEL(ui, 11,0,0);
    
    ADD_RD_LABEL(ui, 12, file_date (ui->d_stat->st_mtime), (int) ui->d_stat->st_size);
    ADD_RD_LABEL(ui, 13, file_date (ui->s_stat->st_mtime), (int) ui->s_stat->st_size);
}

void
file_progress_set_stalled_label (FileOpContext *ctx, char *stalled_msg)
{
    FileOpContextUI *ui;

    g_return_if_fail (ctx != NULL);

    if (ctx->ui == NULL)
	    return;

    ui = ctx->ui;

    label_set_text (ui->stalled_label, stalled_msg);
}

FileProgressStatus
file_progress_real_query_replace (FileOpContext *ctx, enum OperationMode mode,
				  char *destname, struct stat *_s_stat,
				  struct stat *_d_stat)
{
    FileOpContextUI *ui;

    g_return_val_if_fail (ctx != NULL, FILE_CONT);
    g_return_val_if_fail (ctx->ui != NULL, FILE_CONT);
    
    ui = ctx->ui;

    if (ui->replace_result < REPLACE_ALWAYS){
	ui->replace_filename = destname;
	ui->s_stat = _s_stat;
	ui->d_stat = _d_stat;
	init_replace (ctx, mode);
	run_dlg (ui->replace_dlg);
	ui->replace_result = ui->replace_dlg->ret_value;
	if (ui->replace_result == B_CANCEL)
	    ui->replace_result = REPLACE_ABORT;
	destroy_dlg (ui->replace_dlg);
    }

    switch (ui->replace_result){
    case REPLACE_UPDATE:
	do_refresh ();
	if (_s_stat->st_mtime > _d_stat->st_mtime)
	    return FILE_CONT;
	else
	    return FILE_SKIP;

    case REPLACE_SIZE:
	do_refresh ();
	if (_s_stat->st_size == _d_stat->st_size)
	    return FILE_SKIP;
	else
	    return FILE_CONT;
	
    case REPLACE_REGET:
	/* Carefull: we fall through and set do_append */
	ctx->do_reget = _d_stat->st_size;
	
    case REPLACE_APPEND:
        ctx->do_append = 1;
	
    case REPLACE_YES:
    case REPLACE_ALWAYS:
	do_refresh ();
	return FILE_CONT;
    case REPLACE_NO:
    case REPLACE_NEVER:
	do_refresh ();
	return FILE_SKIP;
    case REPLACE_ABORT:
    default:
	return FILE_ABORT;
    }
}

#define FMDY 13
#define	FMD_XLEN 64
extern int fmd_xlen;
static QuickWidget fmd_widgets [] = {

#define	FMCB0  FMDC
#define	FMCB12 0
#define	FMCB11 1
	/* follow symlinks and preserve Attributes must be the first */
    { quick_checkbox, 3, 64, 8, FMDY, N_("preserve &Attributes"), 9, 0,
      0 /* &op_preserve */, 0, XV_WLAY_BELOWCLOSE, "preserve" },
    { quick_checkbox, 3, 64, 7, FMDY, N_("follow &Links"), 7, 0, 
      0 /* &file_mask_op_follow_links */, 0, XV_WLAY_BELOWCLOSE, "follow" },
    { quick_label, 3, 64, 5, FMDY, N_("to:"), 0, 0, 0, 0, XV_WLAY_BELOWOF,"to"},
    { quick_checkbox, 37, 64, 4, FMDY, N_("&Using shell patterns"), 0, 0, 
      0 /* &source_easy_patterns */, 0, XV_WLAY_BELOWCLOSE, "using-shell" },
    { quick_input, 3, 64, 3, FMDY, "", 58, 
      0, 0, 0, XV_WLAY_BELOWCLOSE, "input-def" },
#define FMDI1 4
#define FMDI2 5
#define FMDC 3
    { quick_input, 3, 64, 6, FMDY, "", 58, 0, 
      0, 0, XV_WLAY_BELOWCLOSE, "input2" },
#define FMDI0 6	  
    { quick_label, 3, 64, 2, FMDY, "", 0, 0, 0, 0, XV_WLAY_DONTCARE, "ql" },
#define	FMBRGT 7
    { quick_button, 42, 64, 9, FMDY, N_("&Cancel"), 0, B_CANCEL, 0, 0, XV_WLAY_DONTCARE,
	  "cancel" },
#undef SKIP
#ifdef WITH_BACKGROUND
# define SKIP 5
# define FMCB21 11
# define FMCB22 10
# define FMBLFT 9
# define FMBMID 8
    { quick_button, 25, 64, 9, FMDY, N_("&Background"), 0, B_USER, 0, 0, XV_WLAY_DONTCARE, "back" },
#else /* WITH_BACKGROUND */
# define SKIP 4
# define FMCB21 10
# define FMCB22 9
# define FMBLFT 8
# undef  FMBMID
#endif
    { quick_button, 14, 64, 9, FMDY, N_("&Ok"), 0, B_ENTER, 0, 0, XV_WLAY_NEXTROW, "ok" },
    { quick_checkbox, 42, 64, 8, FMDY, N_("&Stable Symlinks"), 0, 0,
      0 /* &file_mask_stable_symlinks */, 0, XV_WLAY_BELOWCLOSE, "stab-sym" },
    { quick_checkbox, 31, 64, 7, FMDY, N_("&Dive into subdir if exists"), 0, 0, 
      0 /* &dive_into_subdirs */, 0, XV_WLAY_BELOWOF, "dive" },
    { 0 } };

void
fmd_init_i18n (int force)
{
	static int initialized = FALSE;
	register int i;
	int len;

	if (initialized && !force)
		return;

#ifdef ENABLE_NLS
	for (i = sizeof (op_names) / sizeof (op_names[0]); i--;)
		op_names [i] = _(op_names [i]);

	i = sizeof (fmd_widgets) / sizeof (fmd_widgets [0]) - 1;
	while (i--)
		if (fmd_widgets [i].text[0] != '\0')
			fmd_widgets [i].text = _(fmd_widgets [i].text);

	len = strlen (fmd_widgets [FMCB11].text)
		+ strlen (fmd_widgets [FMCB21].text) + 15;
	fmd_xlen = max (fmd_xlen, len);

	len = strlen (fmd_widgets [FMCB12].text)
		+ strlen (fmd_widgets [FMCB22].text) + 15;
	fmd_xlen = max (fmd_xlen, len);
		
	len = strlen (fmd_widgets [FMBRGT].text)
		+ strlen (fmd_widgets [FMBLFT].text) + 11;

#ifdef FMBMID
	len += strlen (fmd_widgets [FMBMID].text) + 6;
#endif

	fmd_xlen = max (fmd_xlen, len + 4);

	len = (fmd_xlen - (len + 6)) / 2;
	i = fmd_widgets [FMBLFT].relative_x = len + 3;
	i += strlen (fmd_widgets [FMBLFT].text) + 8;

#ifdef FMBMID
	fmd_widgets [FMBMID].relative_x = i;
	i += strlen (fmd_widgets [FMBMID].text) + 6;
#endif

	fmd_widgets [FMBRGT].relative_x = i;

#define	chkbox_xpos(i) \
	fmd_widgets [i].relative_x = fmd_xlen - strlen (fmd_widgets [i].text) - 6

	chkbox_xpos(FMCB0);
	chkbox_xpos(FMCB21);
	chkbox_xpos(FMCB22);

	if (fmd_xlen != FMD_XLEN)
	{
		i = sizeof (fmd_widgets) / sizeof (fmd_widgets [0]) - 1;
		while (i--)
			fmd_widgets [i].x_divisions = fmd_xlen;

		fmd_widgets [FMDI1].hotkey_pos =
			fmd_widgets [FMDI2].hotkey_pos = fmd_xlen - 6;
	}
#undef chkbox_xpos
#endif /* ENABLE_NLS */

	initialized = TRUE;
}

char *
file_mask_dialog (FileOpContext *ctx, FileOperation operation, char *text, char *def_text,
		  int only_one, int *do_background)
{
    int source_easy_patterns = easy_patterns;
    char *source_mask, *orig_mask, *dest_dir;
    const char *error;
    struct stat buf;
    int val;
    QuickDialog Quick_input;

    g_return_val_if_fail (ctx != NULL, NULL);

    fmd_init_i18n (FALSE);

    /* Set up the result pointers */

    fmd_widgets[FMCB12].result = &ctx->op_preserve;
    fmd_widgets[FMCB11].result = &ctx->follow_links;
    fmd_widgets[FMCB22].result = &ctx->stable_symlinks;
    fmd_widgets[FMCB21].result = &ctx->dive_into_subdirs;

    /* Create the dialog */

    ctx->stable_symlinks = 0;
    fmd_widgets [FMDC].result = &source_easy_patterns;
    fmd_widgets [FMDI1].text = easy_patterns ? "*" : "^\\(.*\\)$";
    Quick_input.xlen  = fmd_xlen;
    Quick_input.xpos  = -1;
    Quick_input.title = op_names [operation];
    Quick_input.help  = "[Mask Copy/Rename]";
    Quick_input.ylen  = FMDY;
    Quick_input.i18n  = 1;

    if (operation == OP_COPY) {
	Quick_input.class = "quick_file_mask_copy";
	Quick_input.widgets = fmd_widgets;
    } else { /* operation == OP_MOVE */
	Quick_input.class = "quick_file_mask_move";
	Quick_input.widgets = fmd_widgets + 2;
    }
    fmd_widgets [FMDI0].text = text;
    fmd_widgets [FMDI2].text = def_text;
    fmd_widgets [FMDI2].str_result = &dest_dir;
    fmd_widgets [FMDI1].str_result = &source_mask;

    *do_background = 0;
ask_file_mask:

    if ((val = quick_dialog_skip (&Quick_input, SKIP)) == B_CANCEL)
	return 0;

    if (ctx->follow_links && operation != OP_MOVE)
	ctx->stat_func = mc_stat;
    else
	ctx->stat_func = mc_lstat;

    if (ctx->op_preserve || operation == OP_MOVE) {
	ctx->preserve = 1;
	ctx->umask_kill = 0777777;
	ctx->preserve_uidgid = (geteuid () == 0) ? 1 : 0;
    } else {
        int i;
	ctx->preserve = ctx->preserve_uidgid = 0;
	i = umask (0);
	umask (i);
	ctx->umask_kill = i ^ 0777777;
    }

    orig_mask = source_mask;
    if (!dest_dir || !*dest_dir){
	if (source_mask)
	    g_free (source_mask);
	return dest_dir;
    }
    if (source_easy_patterns){
	source_easy_patterns = easy_patterns;
	easy_patterns = 1;
	source_mask = convert_pattern (source_mask, match_file, 1);
	easy_patterns = source_easy_patterns;
        error = re_compile_pattern (source_mask, strlen (source_mask), &ctx->rx);
        g_free (source_mask);
    } else
        error = re_compile_pattern (source_mask, strlen (source_mask), &ctx->rx);

    if (error){
	message_3s (1, MSG_ERROR, _("Invalid source pattern `%s' \n %s "),
		 orig_mask, error);
	if (orig_mask)
	    g_free (orig_mask);
	goto ask_file_mask;
    }
    if (orig_mask)
	g_free (orig_mask);
    ctx->dest_mask = strrchr (dest_dir, PATH_SEP);
    if (ctx->dest_mask == NULL)
	ctx->dest_mask = dest_dir;
    else
	ctx->dest_mask++;
    orig_mask = ctx->dest_mask;
    if (!*ctx->dest_mask || (!ctx->dive_into_subdirs && !is_wildcarded (ctx->dest_mask) &&
			     (!only_one || (!mc_stat (dest_dir, &buf) && S_ISDIR (buf.st_mode)))) ||
	(ctx->dive_into_subdirs && ((!only_one && !is_wildcarded (ctx->dest_mask)) ||
				    (only_one
				     && !mc_stat (dest_dir, &buf) && S_ISDIR (buf.st_mode)))))
	ctx->dest_mask = g_strdup ("*");
    else {
	ctx->dest_mask = g_strdup (ctx->dest_mask);
	*orig_mask = 0;
    }
    if (!*dest_dir){
	g_free (dest_dir);
	dest_dir = g_strdup ("./");
    }
    if (val == B_USER)
	*do_background = 1;
    return dest_dir;
}
