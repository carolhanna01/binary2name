#ifndef LOGCLASS_H
#define LOGCLASS_H

/* 
   logclass.h
   Class definitions of "Log" & "Filter" classes
   to be included by xlogmaster
*/

class Filter;

/*{{{  Modes for "Log" class  */
#define EMPTY 0
#define TAIL_FILE 1
#define RUN_FILE 2
#define CAT_FILE 4

#define NO_FILTER 0
#define CLASS0_FILTER 1<<1
#define CLASS1_FILTER 1<<2

#define ENABLED 0
#define DISABLED 1

#define ABOUT 1

/*}}}*/

class Log {
/*{{{  Functions  */

public:
  Log();
  ~Log();
 
 friend class Filter;	/* jsg - added class as GCC complains friend decl */
 
  void purge();
  void init(const gchar*, const gchar*, const gchar*, const gchar*, gint, gint, gint);
  void allocate_buffers();
  void free_buffers();
  void add_filter(const gchar*, gint, const gchar*);


/*}}}*/
/*{{{  Variables  */
public:

  /*
    Primary Entry Data 
  */
  /* Absolute path to logfile / device or program with commandline */
  gchar *filename;          
  /* Help text to pop up for the button */
  gchar *help; 
  /* Text for the button itself */
  gchar *buttontext;
  /* mode of this entry element - TAIL / CAT / RUN */
  gint mode;
  /* interval between readin attempts */
  gint interval; 
  /* delay before restarting a program in RUN mode */
  gint delay;
  /* array of filter elements */
  Filter** filter;
  /* highest filter class present - determines whether we do background
     monitoring or not */
  gint filterclass; 
  /* plugin -- program to pipe text output through */
  gchar *plugin;



  /* 
     Secondary Entry Data 
  */
  /* buffer to _store_ background reading in */
  gchar* buffer;
  /* buffer to be used as temporary for background reading */
  gchar* rbuffer;
  /* "Chunksize" for background reading - this has been invented to keep
     memory usage a little lower... */
  glong chunk;
  /* buffer length */
  glong length;
  /* amount of valid bytes in the buffer */
  glong got;
  /* Pointer to the widget of the button that is connected to this
     element */
  GtkWidget *button;  
  /* Pointer to the connected style element */
  GtkStyle* style;
  /* IDs of the current alert-colors */
  gulong pixels[2];



  /* 
     Temporary Entry Data / Flags
  */
  /* keyboard accelerators */
  guint accel_key;
  guint accel_mods;
  /* gtk_timeout tag for the interrupt routines ( watchdog / plain ) */
  gint tag; 
  /* gtk_timeout tag for the alert fading routine */
  gint alert_tag;
  /* filedescriptor for reading a logfile or from a pipe to a child */
  gint fd;
  /* flag to assure this element has been activated properly - we don't want
     interrupts to crash in during reconfiguration and the like */
  gint active;
  /* current fading (in percent) between alert and normal color */
  gdouble fade;
  /* the last applied fading step to determine whether we need to update 
     the color of the button */
  gdouble last_fade;
  /* step to use for fading - also contains direction (positive or negative ) */
  gdouble fadestep;
  /* pid of the child process (in RUN mode ) */
  pid_t pid;
  /* pid of the output filter/plugin process (optional) */
  pid_t output_filter_pid;
  /* fd of the pipe into the filter/plugin */
  gint fd_into_filter;
  /* size of pipe into plugin */
  glong pipe_buf;
  /* fd of the pipe out of the filter/plugin */
  gint fd_from_filter;

  /* input buffer */
  gchar* input_buffer;
  /* input buffer size */
  glong input_buffer_size;
  /* amount of bytes in input buffer */
  glong input_buffer_used;
  /* output buffer */
  gchar* output_buffer;
  /* output buffer size */
  glong output_buffer_size;
  /* amount of bytes in output buffer */
  glong output_buffer_used;

  /* lock to keep the interrupt routine to disrupt integrity */
  gint pipe_lock;
  /* pipe_magic_interrupt tag: */
  gint pmi_tag;

  /* for TAIL mode: tells the interrupt routine whether
     this is new or old data (neccessary for TAIL entries
     with plugin) */
  gint up_to_date;

  /* for RUN mode: tells the interrupt routine whether to
     replace (TRUE) or append (FALSE) the new text */
  gint replace;


/*}}}*/
};

/*{{{  Modes for "Filter" class  */
/* mode empty definition */
#define NO_CHANGE 0


/* class0 filter definitions */
#define RAISE ( 1 << 0 )
#define LOWER ( 1 << 1 )
#define HIDE ( 1 << 2 )

#define CLASS0_MASK ( RAISE | LOWER | HIDE )

/* class1 filter definitions */
#define ALERT ( 1 << 4 )
#define NOTICE ( 1 << 5 )
#define UNICONIFY ( 1 << 6 )
#define EXECUTE ( 1 << 7 )

#define CLASS1_MASK ( ALERT | NOTICE | UNICONIFY | EXECUTE )


/* special mode flags */
#define INVERT ( 1 << 13 )
#define CASE_SENSITIVE ( 1 << 14 )

#define SPECIAL_MODE_MASK ( INVERT | CASE_SENSITIVE )


/* the mask for all things that are applied during runtime */
#define RUNTIME_MODES_MASK ( CLASS0_MASK | CLASS1_MASK | SPECIAL_MODE_MASK )


/* this is just a flag appended to the mode determining whether
   the compilation of the REGEX should be done (otherwise
   just the string and mode array are being initialized - this is
   useful for using filter elements for temporary storage */
#define COMPILE_REGEX ( 1 << 15 )

/*}}}*/

class Filter 
{
/*{{{  Functions  */
public:
  Filter();
  ~Filter();
   friend class Log;	/* jsg - added class as gcc complains about friend */
  void init(const char* , gint);
  void purge();




/*}}}*/
/*{{{  Variables  */
public:
  
  gchar* string;
  gchar* execline;
  gint mode;
  
  regex_t* regex_pattern;
  gint regex_status;
 
/*}}}*/
};

#endif /* LOGCLASS_H */
