#ifndef FILE_H
#define FILE_H

/* 
   file.h
   include file for xlogmaster file-subroutines
*/


/*{{{  Variables  */

char* load_path;
char* save_path;
char* log_path;

/*}}}*/

/*{{{  Config-File REGEX strings  */
// modes:
static regex_t* tail_regex = NULL;
static gchar* tail_string = "^TAIL *{.*}";
static regex_t* run_regex = NULL;
static gchar* run_string = "^RUN *{.*}";
static regex_t* cat_regex = NULL;
static gchar* cat_string = "^CAT *{.*}";

// filters:
static regex_t* raise_regex = NULL;
static gchar* raise_string = "RAISE.*{.*}";
static regex_t* lower_regex = NULL;
static gchar* lower_string = "LOWER.*{.*}";
static regex_t* hide_regex = NULL;
static gchar* hide_string = "HIDE.*{.*}";
static regex_t* alert_regex = NULL;
static gchar* alert_string = "ALERT.*{.*}";
static regex_t* notice_regex = NULL;
static gchar* notice_string = "NOTICE.*{.*}";
static regex_t* uniconify_regex = NULL;
static gchar* uniconify_string = "UNICONIFY.*{.*}";
static regex_t* execute_regex = NULL;
static gchar* execute_string = "EXECUTE.*{.*}.*{.*}";
static regex_t* invert_regex = NULL;
static gchar* invert_string = "INVERT.*{.*}";
static regex_t* case_sensitive_regex = NULL;
static gchar* case_sensitive_string = "CASE_SENSITIVE.*{.*}";

// other things:
static regex_t* plugin_regex = NULL;
static gchar* plugin_string = "PLUGIN.*{.*}";


static regex_t** all_regex[] =
{
  &tail_regex,
  &run_regex,
  &cat_regex,
  &raise_regex,
  &lower_regex,
  &hide_regex,
  &alert_regex,
  &notice_regex,
  &uniconify_regex,
  &execute_regex,
  &invert_regex,
  &case_sensitive_regex,
  &plugin_regex,
  NULL
};

static gchar* all_string[] =
{
  tail_string,
  run_string,
  cat_string,
  raise_string,
  lower_string,
  hide_string,
  alert_string,
  notice_string,
  uniconify_string,
  execute_string,
  invert_string,
  case_sensitive_string,
  plugin_string,
  NULL
};

static regex_t** mode_regex[] =
{
  &tail_regex,
  &run_regex,
  &cat_regex,
  NULL
};

static gint mode_flags[] =
{
  TAIL_FILE,
  RUN_FILE,
  CAT_FILE
};

static regex_t** filter_regex[] =
{
  &raise_regex,
  &lower_regex,
  &hide_regex,
  &alert_regex,
  &notice_regex,
  &uniconify_regex,
  &execute_regex,
  &invert_regex,
  &case_sensitive_regex,
  NULL
};

static gint filter_flags[] =
{
  RAISE,
  LOWER,
  HIDE,
  ALERT,
  NOTICE,
  UNICONIFY,
  EXECUTE,
  INVERT,
  CASE_SENSITIVE
};

/*}}}*/

/*{{{  Subroutines  */

Log** read_configuration_file(gchar*);
int write_configuration(char*);

/*{{{  Matching subroutines  */

gint legalize(gint, gint);
int match(char*, char*, int);
int searchfor(char, char*, int);
char* getstring(char , char* , int );
char* clean_string(char*);
/*}}}*/

/*{{{  REGEX Soubroutines  */
void create_regex_patterns();
void free_regex_patterns();

/*}}}*/

/*}}}*/


#endif /* FILE_H */
