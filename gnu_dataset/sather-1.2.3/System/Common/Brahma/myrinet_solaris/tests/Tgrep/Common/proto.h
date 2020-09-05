/* main_c.c */
int main(int argc, char **argv);
int add_work(char *path, int tp);
void *search_thr(void *arg);
int continue_line(char *, FILE *, out_t *, work_t *, int *, long *);
void *cascade(void *arg);
int print_local_output(out_t *out, work_t *wt);
int add_output_local(out_t **out, work_t *wt, int lc, long bc, char *line);
void prnt_stats(void);
void notrun(void);
void uncase(char *s);
void usage(void);
int regexp_usage(void);
void debug_usage(void);

