#ifndef __FILE_H
#define __FILE_H

#include "fileopctx.h"
#include "background.h"

extern int know_not_what_am_i_doing;

struct link;

int copy_file_file      (FileOpContext *ctx, char *s, char *d, int ask_overwrite,
			 long *progres_count, double *progress_bytes, 
                         int is_toplevel_file);
int move_file_file      (FileOpContext *ctx, char *s, char *d,
			 long *progres_count, double *progress_bytes);
int move_dir_dir        (FileOpContext *ctx, char *s, char *d,
			 long *progres_count, double *progress_bytes);
int copy_dir_dir        (FileOpContext *ctx, char *s, char *d, int toplevel, int move_over,
			 int delete, struct link *parent_dirs,
			 long *progres_count, double *progress_bytes);
int erase_dir           (FileOpContext *ctx, char *s, long *progres_count, double *progress_bytes);
int erase_file          (FileOpContext *ctx, char *s, long *progress_count, double *progress_bytes,
			 int is_toplevel_file);
int erase_dir_iff_empty (FileOpContext *ctx, char *s);

int   panel_operate      (void *source_panel, FileOperation op,
			  char *thedefault, int ask_user);
char *file_mask_dialog   (FileOpContext *ctx, FileOperation operation, char *text, char *def_text,
			  int only_one, int *do_background);

extern int file_op_compute_totals;

/* Error reporting routines */
    /* Report error with one file */
    int file_error (char *format, char *file);

    /* Report error with two files */
    int files_error (char *format, char *file1, char *file2);

/* Query routines */

extern int background_wait;

int is_wildcarded (char *p);
void compute_dir_size (char *dirname, long *ret_marked, double *ret_total);
#endif


