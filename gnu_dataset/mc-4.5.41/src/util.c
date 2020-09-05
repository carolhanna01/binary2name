/* Various utilities
   Copyright (C) 1994, 1995, 1996 the Free Software Foundation.
   Written 1994, 1995, 1996 by:
   Miguel de Icaza, Janne Kukonlehto, Dugan Porter,
   Jakub Jelinek, Mauricio Plaza.

   The file_date routine is mostly from GNU's fileutils package,
   written by Richard Stallman and David MacKenzie.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <config.h>
#include <stdio.h>
#if defined(NEEDS_IO_H)            /* OS/2 need io.h! .ado */
#    include <io.h>
#endif
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
#include <signal.h>		/* my_system */
#include <limits.h>		/* INT_MAX */
#ifndef SCO_FLAVOR
#	include <sys/time.h>	/* alex: sys/select.h defines struct timeval */
#endif /* SCO_FLAVOR */
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdarg.h>
#include <errno.h>		/* my_system */
#ifdef SCO_FLAVOR
#	include <sys/timeb.h>	/* alex: for struct timeb, used in time.h */
#endif /* SCO_FLAVOR */
#include <time.h>
#ifndef HAS_NO_GRP_PWD_H
#   include <pwd.h>
#   include <grp.h>
#endif
#include <string.h>
#include <ctype.h>
#ifdef HAVE_SYS_SELECT_H
#  include <sys/select.h>
#endif

#ifdef __linux__
#    if defined(__GLIBC__) && (__GLIBC__ < 2)
#        include <linux/termios.h>	/* This is needed for TIOCLINUX */
#    else
#        include <termios.h>
#    endif
#  include <sys/ioctl.h>
#endif

#include "mountlist.h"

#if defined(HAVE_RX_H) && defined(HAVE_REGCOMP)
#include <rx.h>
#else
#include <regex.h>
#endif
#include "global.h"
#include "profile.h"
#include "user.h"		/* expand_format */
#include "../vfs/vfs.h"

/* "$Id: util.c,v 1.30 1999/09/19 23:09:12 norbert Exp $" */

char app_text [] = "Midnight-Commander";

int easy_patterns = 1;
int align_extensions = 1;
int tilde_trunc = 1;

struct mount_entry *mount_list = NULL;

#ifndef VFS_STANDALONE
int is_printable (int c)
{
    static const unsigned char xterm_printable[] = {
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
        1,1,1,1,0,0,1,1,0,1,1,1,1,0,0,0,0,1,1,1,1,1,0,0,0,1,0,0,0,0,0,0,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
    };

    extern int xterm_flag;
    extern int eight_bit_clean;
    extern int full_eight_bits;

#ifdef HAVE_GNOME
    return 1;
#endif
    
    c &= 0xff;
    if (eight_bit_clean){
        if (full_eight_bits){
	    if (xterm_flag)
	        return xterm_printable [c];
	    else
	        return (c > 31 && c != 127 && c != 155);
	} else
	    return ((c >31 && c < 127) || c >= 160);
    } else
	return (c > 31 && c < 127);
}

/* Returns the message dimensions (lines and columns) */
int msglen (char *text, int *lines)
{
    int max = 0;
    int line_len = 0;
    
    for (*lines = 1;*text; text++){
	if (*text == '\n'){
	    line_len = 0;
	    (*lines)++;
	} else {
	    line_len++;
	    if (line_len > max)
		max = line_len;
	}
    }
    return max;
}

char *trim (char *s, char *d, int len)
{
    int source_len = strlen (s);
    
    if (source_len > len){
	strcpy (d, s+(source_len-len));
	d [0] = '.';
	d [1] = '.';
	d [2] = '.';
    } else
	strcpy (d, s);
    return d;
}
#endif

char *
name_quote (const char *s, int quote_percent)
{
    char *ret, *d;
    
    d = ret = g_malloc (strlen (s)*2 + 2 + 1);
    if (*s == '-') {
        *d++ = '.';
        *d++ = '/';
    }

    for (; *s; s++, d++) {
	switch (*s)
	{
	    case '%':
		if (quote_percent)
		    *d++ = '%';
		break;
	    case '\'':
	    case '\\':
	    case '\r':
	    case '\n':
	    case '\t':
	    case '"':
	    case ':':
	    case ';':
	    case ' ':
	    case '?':
	    case '|':
	    case '[':
	    case ']':
	    case '{':
	    case '}':
	    case '<':
	    case '>':
	    case '`':
	    case '~':
	    case '!':
	    case '@':
	    case '#':
	    case '$':
	    case '^':
	    case '&':
	    case '*':
	    case '(':
	    case ')':
		*d++ = '\\';
	}
	*d = *s;
    }
    *d = '\0';
    return ret;
}

#ifndef VFS_STANDALONE
char *
fake_name_quote (const char *s, int quote_percent)
{
    return g_strdup (s);
}

/* If passed an empty txt (this usually means that there is an error)
 * in the upper layers, we return "/"
 */
char *name_trunc (char *txt, int trunc_len)
{
    static char x [MC_MAXPATHLEN+MC_MAXPATHLEN];
    int    txt_len;
    char *p;

    if (!txt)
	txt = PATH_SEP_STR;
    
    if (trunc_len > sizeof (x)-1){
	fprintf (stderr, _("name_trunc: too big"));
	trunc_len = sizeof (x)-1;
    }
    txt_len = strlen (txt);
    if (txt_len <= trunc_len)
	strcpy (x, txt);
    else if (tilde_trunc){
	int y = trunc_len % 2;
	strncpy (x, txt, (trunc_len/2)+y);
	strncpy (x+(trunc_len/2)+y, txt+txt_len-(trunc_len/2), trunc_len/2);
	x [(trunc_len/2)+y] = '~';
    } else {
	strncpy (x, txt, trunc_len-1);
	x [trunc_len-1] = '>';
    }
    x [trunc_len] = 0;
    for (p = x; *p; p++)
        if (!is_printable (*p))
            *p = '?';
    return x;
}

char *size_trunc (long int size)
{
    static char x [BUF_TINY];
    long int divisor = 1;
    char *xtra = "";
    
    if (size > 999999999L){
	divisor = 1024;
	xtra = "Kb";
	if (size/divisor > 999999999L){
	    divisor = 1024*1024;
	    xtra = "Mb";
	}
    }
    g_snprintf (x, sizeof (x), "%ld%s", (size/divisor), xtra);
    return x;
}

char *size_trunc_sep (long int size)
{
    static char x [60];
    int  count;
    char *p, *d, *y;

    p = y = size_trunc (size);
    p += strlen (p) - 1;
    d = x + sizeof (x) - 1;
    *d-- = 0;
    while (p >= y && isalpha (*p))
	*d-- = *p--;
    for (count = 0; p >= y; count++){
	if (count == 3){
	    *d-- = ',';
	    count = 0;
	}
	*d-- = *p--;
    }
    d++;
    if (*d == ',')
	d++;
    return d;
}

int is_exe (mode_t mode)
{
    if ((S_IXUSR & mode) || (S_IXGRP & mode) || (S_IXOTH & mode))
	return 1;
    return 0;
}

#define ismode(n,m) ((n & m) == m)

char *string_perm (mode_t mode_bits)
{
    static char mode [11];

    strcpy (mode, "----------");
    if (ismode (mode_bits, S_IFDIR)) mode [0] = 'd';
#ifdef S_IFSOCK
    if (ismode (mode_bits, S_IFSOCK)) mode [0] = 's';
#endif
    if (ismode (mode_bits, S_IXOTH)) mode [9] = 'x';
    if (ismode (mode_bits, S_IWOTH)) mode [8] = 'w';
    if (ismode (mode_bits, S_IROTH)) mode [7] = 'r';
    if (ismode (mode_bits, S_IXGRP)) mode [6] = 'x';
    if (ismode (mode_bits, S_IWGRP)) mode [5] = 'w';
    if (ismode (mode_bits, S_IRGRP)) mode [4] = 'r';
    if (ismode (mode_bits, S_IXUSR)) mode [3] = 'x';
    if (ismode (mode_bits, S_IWUSR)) mode [2] = 'w';
    if (ismode (mode_bits, S_IRUSR)) mode [1] = 'r';
#ifndef OS2_NT
    if (ismode (mode_bits, S_ISUID)) mode [3] = (mode [3] == 'x') ? 's' : 'S';
    if (ismode (mode_bits, S_ISGID)) mode [6] = (mode [6] == 'x') ? 's' : 'S';
    if (ismode (mode_bits, S_IFCHR)) mode [0] = 'c';
    if (ismode (mode_bits, S_IFBLK)) mode [0] = 'b';
    if (ismode (mode_bits, S_ISVTX)) mode [9] = (mode [9] == 'x') ? 't' : 'T';
    if (ismode (mode_bits, S_IFLNK)) mode [0] = 'l';
    if (ismode (mode_bits, S_IFIFO)) mode [0] = 'p';
#endif
    return mode;
}

/* p: string which might contain an url with a password (this parameter is
      modified in place).
   has_prefix = 0: The first parameter is an url without a prefix
                   (user[:pass]@]machine[:port][remote-dir). Delete
                   the password.
   has_prefix = 1: Search p for known url prefixes. If found delete
                   the password from the url. 
                   Cavevat: only the first url is found
*/ 
char *
strip_password (char *p, int has_prefix)
{
    static struct {
	char *name;
        size_t len;
    } prefixes[] = { {"/#ftp:", 6},
		     {"/#mc:", 5},
		     {"ftp://", 6},
		     {"/#smb:", 6},
    };
    char *at, *inner_colon, *dir;
    int i;
    char *result = p;
    
    for (i = 0; i < sizeof (prefixes)/sizeof (prefixes[0]); i++) {
	char *q;

	if (has_prefix) {
	    if((q = strstr (p, prefixes[i].name)) == 0)
	       continue;
            else
	        p = q + prefixes[i].len;
       	};

        if ((dir = strchr (p, PATH_SEP)) != NULL)
   	    *dir = '\0';
        /* search for any possible user */
        at = strchr (p, '@');

        /* We have a username */
        if (at) {
            *at = 0;
            inner_colon = strchr (p, ':');
  	    *at = '@';
            if (inner_colon)
                strcpy (inner_colon, at);
        }
        if (dir)
	    *dir = PATH_SEP;
	break;
    }
    return (result);
}

char *strip_home_and_password(char *dir)
{
    size_t len;
    static char newdir [MC_MAXPATHLEN];

    if (home_dir && !strncmp (dir, home_dir, len = strlen (home_dir)) && 
	(dir[len] == PATH_SEP || dir[len] == '\0')){
	newdir [0] = '~';
	strcpy (&newdir [1], &dir [strlen (home_dir)]);
	return newdir;
    } 

    /* We do not strip homes in /#ftp tree, I do not like ~'s there 
       (see ftpfs.c why) */
    strcpy (newdir, dir);
    strip_password (newdir, 1);
    return newdir;
}

static char *maybe_start_group (char *d, int do_group, int *was_wildcard)
{
    if (!do_group)
	return d;
    if (*was_wildcard)
	return d;
    *was_wildcard = 1;
    *d++ = '\\';
    *d++ = '(';
    return d;
}

static char *maybe_end_group (char *d, int do_group, int *was_wildcard)
{
    if (!do_group)
	return d;
    if (!*was_wildcard)
	return d;
    *was_wildcard = 0;
    *d++ = '\\';
    *d++ = ')';
    return d;
}

/* If shell patterns are on converts a shell pattern to a regular
   expression. Called by regexp_match and mask_rename. */
/* Shouldn't we support [a-fw] type wildcards as well ?? */
char *convert_pattern (char *pattern, int match_type, int do_group)
{
    char *s, *d;
    char *new_pattern;
    int was_wildcard = 0;

    if (easy_patterns){
	new_pattern = g_malloc (MC_MAXPATHLEN);
	d = new_pattern;
	if (match_type == match_file)
	    *d++ = '^';
	for (s = pattern; *s; s++, d++){
	    switch (*s){
	    case '*':
		d = maybe_start_group (d, do_group, &was_wildcard);
		*d++ = '.';
		*d   = '*';
		break;
		
	    case '?':
		d = maybe_start_group (d, do_group, &was_wildcard);
		*d = '.';
		break;
		
	    case '.':
		d = maybe_end_group (d, do_group, &was_wildcard);
		*d++ = '\\';
		*d   = '.';
		break;

	    default:
		d = maybe_end_group (d, do_group, &was_wildcard);
		*d = *s;
		break;
	    }
	}
	d = maybe_end_group (d, do_group, &was_wildcard);
	if (match_type == match_file)
	    *d++ = '$';
	*d = 0;
	return new_pattern;
    } else
	return  g_strdup (pattern);
}

int regexp_match (char *pattern, char *string, int match_type)
{
    static regex_t r;
    static char *old_pattern = NULL;
    static int old_type;
    int    rval;

    if (!old_pattern || STRCOMP (old_pattern, pattern) || old_type != match_type){
	if (old_pattern){
	    regfree (&r);
	    g_free (old_pattern);
	}
	pattern = convert_pattern (pattern, match_type, 0);
	if (regcomp (&r, pattern, REG_EXTENDED|REG_NOSUB|MC_ARCH_FLAGS)) {
	    g_free (pattern);
	    return -1;
	}
	old_pattern = pattern;
	old_type = match_type;
    }
    rval = !regexec (&r, string, 0, NULL, 0);
    return rval;
}

char *extension (char *filename)
{
    char *d;

    if (!strlen (filename))
	return "";
    
    d = filename + strlen (filename) - 1;
    for (;d >= filename; d--){
	if (*d == '.')
	    return d+1;
    }
    return "";
}

/* This routine uses the fact that x is at most 14 chars or so */
char *split_extension (char *x, int pad)
{
    return x;

    /* Buggy code 
    if (!align_extensions)
	return x;

    if (strlen (x) >= pad)
	return x;
    
    if ((ext = extension (x)) == x || *ext == 0)
	return x;

    strcpy (xbuf, x);
    for (i = strlen (x); i < pad; i++)
	xbuf [i] = ' ';
    xbuf [pad] = 0;

    l = strlen (ext);
    for (i = 0; i < l; i++)
	xbuf [pad-i] = *(ext+l-i-1);
    for (i = xbuf + (ext - x); i < 
    return xbuf; */
}

int get_int (char *file, char *key, int def)
{
    return GetPrivateProfileInt (app_text, key, def, file);
}

int set_int (char *file, char *key, int value)
{
    char buffer [BUF_TINY];

    g_snprintf (buffer, sizeof (buffer), "%d", value);
    return WritePrivateProfileString (app_text, key, buffer, file);
}

int exist_file (char *name)
{
    return access (name, R_OK) == 0;
}

char *load_file (char *filename)
{
    FILE *data_file;
    struct stat s;
    char *data;
    long read_size;
    
    if (stat (filename, &s) != 0){
	return 0;
    }
    if ((data_file = fopen (filename, "r")) == NULL){
	return 0;
    }
    data = (char *) g_malloc (s.st_size+1);
    read_size = fread (data, 1, s.st_size, data_file);
    data [read_size] = 0;
    fclose (data_file);

    if (read_size > 0)
	return data;
    else {
	 g_free (data);
	return 0;
    }
}

/* Check strftime() results. Some systems (i.e. Solaris) have different
short-month-name sizes for different locales */ 
size_t i18n_checktimelength (void)
{
    size_t length, a, b;
    char buf [MAX_I18NTIMELENGTH + 1];
    time_t testtime = time (NULL);
    
    a = strftime (buf, sizeof(buf)-1, _("%b %e %H:%M"), localtime(&testtime));
    b = strftime (buf, sizeof(buf)-1, _("%b %e  %Y"), localtime(&testtime));
    
    length = max (a, b);
    
    /* Don't handle big differences. Use standard value (email bug, please) */
    if ( length > MAX_I18NTIMELENGTH || length < MIN_I18NTIMELENGTH )
	length = STD_I18NTIMELENGTH;
    
    return length;
}

char *file_date (time_t when)
{
    static char timebuf [MAX_I18NTIMELENGTH + 1];
    time_t current_time = time ((time_t) 0);
    static size_t i18n_timelength = 0;
    static char *fmt, *fmtyear, *fmttime;

    if (i18n_timelength == 0){
	i18n_timelength = i18n_checktimelength() + 1;
	
	/* strftime() format string for old dates */
	fmtyear = _("%b %e  %Y");
	/* strftime() format string for recent dates */
	fmttime = _("%b %e %H:%M");
    }

    if (current_time > when + 6L * 30L * 24L * 60L * 60L /* Old. */
	|| current_time < when - 60L * 60L) /* In the future. */
	/* The file is fairly old or in the future.
	   POSIX says the cutoff is 6 months old;
	   approximate this by 6*30 days.
	   Allow a 1 hour slop factor for what is considered "the future",
	   to allow for NFS server/client clock disagreement.
	   Show the year instead of the time of day.  */

	fmt = fmtyear;
    else
	fmt = fmttime;
    
    strftime (timebuf, i18n_timelength, fmt, localtime(&when));
    return timebuf;
}

/* Like file_date, but packs the data to fit in 10 columns */
char *file_date_pck (time_t when)
{
    /* FIXME: Should return only 10 chars, not 14 */
    return file_date (when);
}

char *extract_line (char *s, char *top)
{
    static char tmp_line [BUF_MEDIUM];
    char *t = tmp_line;
    
    while (*s && *s != '\n' && (t - tmp_line) < sizeof (tmp_line)-1 && s < top)
	*t++ = *s++;
    *t = 0;
    return tmp_line;
}

/* FIXME: I should write a faster version of this (Aho-Corasick stuff) */
char * _icase_search (char *text, char *data, int *lng)
{
    char *d = text;
    char *e = data;
    int dlng = 0;

    if (lng)
	*lng = 0;
    for (;*e; e++) {
	while (*(e+1) == '\b' && *(e+2)) {
	    e += 2;
	    dlng += 2;
	}
	if (toupper((unsigned char) *d) == toupper((unsigned char) *e))
	    d++;
	else {
	    e -= d - text;
	    d = text;
	    dlng = 0;
	}
	if (!*d) {
	    if (lng)
		*lng = strlen (text) + dlng;
	    return e+1;
	}
    }
    return 0;
}

/* The basename routine */
char *x_basename (char *s)
{
    char  *where;
    return ((where = strrchr (s, PATH_SEP))) ? where + 1 : s;
}

void my_putenv (char *name, char *data)
{
    char *full;

    full = malloc (strlen (name) + strlen (data) + 2);
    strcpy (full, name);
    strcat (full, "=");
    strcat (full, data);
    putenv (full);

    /* WARNING: NEVER FREE THE full VARIABLE!!!!!!!!!!!!!!!!!!!!!!!! */
    /* It is used by putenv. Freeing it will corrupt the environment */
}

#if 0
static void my_putenv_expand (char *name, char macro_code)
{
    char *data;

    data = expand_format (macro_code);
    my_putenv (name, data);
    g_free (data);
}

/* Puts some status information in to the environment so that
   processes to be executed can access it. */
static void prepare_environment (void)
{
    my_putenv_expand ("MC_CURRENT_DIR", 'd');
    my_putenv_expand ("MC_OTHER_DIR", 'D');
    my_putenv_expand ("MC_CURRENT_FILE", 'f');
    my_putenv_expand ("MC_OTHER_FILE", 'F');
    my_putenv_expand ("MC_CURRENT_TAGGED", 't');
    my_putenv_expand ("MC_OTHER_TAGGED", 'T');
    /* MC_CONTROL_FILE has been added to environment on startup */
}
#endif
#endif /* VFS_STANDALONE */

char *unix_error_string (int error_num)
{
    static char buffer [BUF_LARGE];
	
    g_snprintf (buffer, sizeof (buffer), "%s (%d)",
		g_strerror (error_num), error_num);
    return buffer;
}

#ifndef VFS_STANDALONE	
long blocks2kilos (int blocks, int bsize)
{
    if (bsize > 1024){
	return blocks * (bsize / 1024);
    } else if (bsize < 1024){
	return blocks / (1024 /bsize);
    } else
	return blocks;
}

void init_my_statfs (void)
{
#ifndef NO_INFOMOUNT
    mount_list = read_filesystem_list (1, 1);
#endif
}

char *skip_separators (char *s)
{
    for (;*s; s++)
	if (*s != ' ' && *s != '\t' && *s != ',')
	    break;
    return s;
}

char *skip_numbers (char *s)
{
    for (;*s; s++)
	if (!isdigit (*s))
	    break;
    return s;
}

/* Remove all control sequences from the argument string.  We define
 * "control sequence", in a sort of pidgin BNF, as follows:
 *
 * control-seq = Esc non-'['
 *	       | Esc '[' (0 or more digits or ';' or '?') (any other char)
 *
 * This scheme works for all the terminals described in my termcap /
 * terminfo databases, except the Hewlett-Packard 70092 and some Wyse
 * terminals.  If I hear from a single person who uses such a terminal
 * with MC, I'll be glad to add support for it.  (Dugan)
 */

char *strip_ctrl_codes (char *s)
{
    int i;  /* Current length of the string's correct (stripped) prefix */
    int j;  /* Number of control characters we have skipped so far */

    if (!s)
	return 0;
    
    for (i = 0, j = 0; s [i+j]; ++i)
	if (s [i+j] != ESC_CHAR){
	    if (j)
		s [i] = s [i+j];
	} else {
	    ++j;
	    if (s [i+j++] == '[')
		while (strchr ("0123456789;?", s [i+j++]))
		    /* Skip the control sequence's arguments */ ;
	    --i;
	}
    s[i] = 0;
    return s;
}

#endif /* VFS_STANDALONE */

/*
 * Ok, on systems running glibc, getcwd does not use popen pwd on
 * Linux at least
 */
#ifdef __GLIBC__
#  ifdef linux
#     undef HAVE_GETWD
#  endif
#endif

/* getwd is better than getcwd, the later uses a popen ("pwd"); */
char *get_current_wd (char *buffer, int size)
{
    char *p;

#ifdef HAVE_GETWD
    p = (char *) getwd (buffer);
#else
    p = getcwd (buffer, size);
#endif
    return p;
}

#define CHECK(x) if (x == -1) return 0;

static long
get_small_endian_long (int fd)
{
    unsigned char buffer [4];

    CHECK (mc_read (fd, buffer, 4));
    return (buffer [3] << 24) | (buffer [2] << 16) | (buffer [1] << 8) | buffer [0];
}

/*
 * This constant makes the magic array on the stack be larger than
 * it needs because Linux when reading the second byte of /proc/locks
 * for example will write 2 bytes, even if we only asked for one
 */
#define LINUX_HAS_PROBLEMS_WHEN_READING_PROC_LOCKS_ON_SOME_KERNELS 40

/* This function returns 0 if the file is not in gunzip format  */
/* or how much memory must be allocated to load the gziped file */
/* Warning: this function moves the current file pointer */
long int is_gunzipable (int fd, int *type)
{
    unsigned char magic [4+LINUX_HAS_PROBLEMS_WHEN_READING_PROC_LOCKS_ON_SOME_KERNELS];
    
    *type = ISGUNZIPABLE_GUNZIP;
	
    /* Read the magic signature */
    CHECK (mc_read (fd, &magic [0], 4));
	
    /* GZIP_MAGIC and OLD_GZIP_MAGIC */
    if (magic [0] == 037 && (magic [1] == 0213 || magic [1] == 0236)){
	/* Read the uncompressed size of the file */
	mc_lseek (fd, -4, SEEK_END);
	return get_small_endian_long (fd);
    }

    /* PKZIP_MAGIC */
    if (magic [0] == 0120 && magic [1] == 0113 && magic [2] == 003 && magic [3] == 004){
	/* Read compression type */
	mc_lseek (fd, 8, SEEK_SET);
	CHECK (mc_read (fd, &magic [0], 2));
	
	/* Gzip can handle only deflated (8) or stored (0) files */
	if ((magic [0] != 8 && magic [0] != 0) || magic [1] != 0)
	     return 0;
        /* Read the uncompressed size of the first file in the archive */
	mc_lseek (fd, 22, SEEK_SET);
	return get_small_endian_long (fd);
    }

    /* PACK_MAGIC and LZH_MAGIC and compress magic */
    if (magic [0] == 037 && (magic [1] ==  036 || magic [1] == 0240 || magic [1] == 0235)){
	 /* In case the file is packed, sco lzhed or compress_magic, the */
	 /* program guesses that the uncompressed size is (at most) four */
	 /* times the length of the compressed size, if the compression  */
	 /* ratio is more than 4:1 the end of the file is not displayed  */
	 return 4*mc_lseek (fd, 0, SEEK_END);
    }

    /* BZIP and BZIP2 files */
    if ((magic[0] == 'B') && (magic[1] == 'Z') &&
	(magic [3] >= '1') && (magic [3] <= '9')){
            switch (magic[2]) {
                case '0':
                    *type = ISGUNZIPABLE_BZIP;
                    return 5*mc_lseek (fd, 0, SEEK_END);
                case 'h': 
	            *type = ISGUNZIPABLE_BZIP2;
	            return 5*mc_lseek (fd, 0, SEEK_END);
            }
    }
    return 0;
}

char *
decompress_extension (int type)
{
	switch (type){
	case ISGUNZIPABLE_GUNZIP: return "#ugz";
	case ISGUNZIPABLE_BZIP:   return "#ubz";
	case ISGUNZIPABLE_BZIP2:  return "#ubz2";
	}
	/* Should never reach this place */
	fprintf (stderr, "Fatal: decompress_extension called with an unknown argument\n");
	return 0;
}

char *
decompress_command (int type)
{
	switch (type){
	case ISGUNZIPABLE_GUNZIP:
		return "gzip -cdf";
		
	case ISGUNZIPABLE_BZIP:
		return "bzip -d";
		
	case ISGUNZIPABLE_BZIP2:
		return "bzip2 -d";
	}
	/* Should never reach this place */
	fprintf (stderr, "Fatal: decompress_command called with an unknown argument\n");
	return 0;
}

void
decompress_command_and_arg (int type, char **cmd, char **flags)
{
	switch (type){
	case ISGUNZIPABLE_GUNZIP:
		*cmd   = "gzip";
		*flags = "-cdf";
		return;

	case ISGUNZIPABLE_BZIP:
		*cmd   = "bzip";
		*flags = "-d";
		return;

		
	case ISGUNZIPABLE_BZIP2:
		*cmd   = "bzip2";
		*flags = "-d";
		return;
	}
	*cmd   = 0;
	*flags = 0;
	
	/* Should never reach this place */
	fprintf (stderr, "Fatal: decompress_command called with an unknown argument\n");
}

#ifndef VFS_STANDALONE
/* Hooks */
void add_hook (Hook **hook_list, void (*hook_fn)(void *), void *data)
{
    Hook *new_hook = g_new (Hook, 1);

    new_hook->hook_fn = hook_fn;
    new_hook->next    = *hook_list;
    new_hook->hook_data = data;
      
    *hook_list = new_hook;
}

void execute_hooks (Hook *hook_list)
{
    Hook *new_hook = 0;
    Hook *p;

    /* We copy the hook list first so tahat we let the hook
     * function call delete_hook
     */
    
    while (hook_list){
	add_hook (&new_hook, hook_list->hook_fn, hook_list->hook_data);
	hook_list = hook_list->next;
    }
    p = new_hook;
    
    while (new_hook){
	(*new_hook->hook_fn)(new_hook->hook_data);
	new_hook = new_hook->next;
    }
    
    for (hook_list = p; hook_list;){
	p = hook_list;
	hook_list = hook_list->next;
	 g_free (p);
    }
}

void delete_hook (Hook **hook_list, void (*hook_fn)(void *))
{
    Hook *current, *new_list, *next;

    new_list = 0;
    
    for (current = *hook_list; current; current = next){
	next = current->next;
	if (current->hook_fn == hook_fn)
	    g_free (current);
	else
	    add_hook (&new_list, current->hook_fn, current->hook_data);
    }
    *hook_list = new_list;
}

int hook_present (Hook *hook_list, void (*hook_fn)(void *))
{
    Hook *p;
    
    for (p = hook_list; p; p = p->next)
	if (p->hook_fn == hook_fn)
	    return 1;
    return 0;
}

void wipe_password (char *passwd)
{
    char *p = passwd;
    
    if (!p)
	return;
    for (;*p ; p++)
        *p = 0;
    g_free (passwd);
}

/* Convert "\E" -> esc character and ^x to control-x key and ^^ to ^ key */
/* Returns a newly allocated string */
char *convert_controls (char *s)
{
    char *valcopy = g_strdup (s);
    char *p, *q;

    /* Parse the escape special character */
    for (p = s, q = valcopy; *p;){
	if (*p == '\\'){
	    p++;
	    if ((*p == 'e') || (*p == 'E')){
		p++;
		*q++ = ESC_CHAR;
	    }
	} else {
	    if (*p == '^'){
		p++;
		if (*p == '^')
		    *q++ = *p++;
		else {
		    *p = (*p | 0x20);
		    if (*p >= 'a' && *p <= 'z') {
		        *q++ = *p++ - 'a' + 1;
		    } else
		        p++;
		}
	    } else
		*q++ = *p++;
	}
    }
    *q++ = 0;
    return valcopy;
}

/* Reverse the string */
char *reverse_string (char *string)
{
    int len = strlen (string);
    int i;
    const int steps = len/2;
    
    for (i = 0; i < steps; i++){
	char c = string [i];
    
	string [i] = string [len-i-1];
	string [len-i-1] = c;
    }
    return string;
}

char *resolve_symlinks (char *path)
{
    char *buf, *buf2, *p, *q, *r, c;
    int len;
    struct stat mybuf;
    
    if (*path != PATH_SEP)
        return NULL;
    r = buf = g_malloc (MC_MAXPATHLEN);
    buf2 = g_malloc (MC_MAXPATHLEN); 
    *r++ = PATH_SEP;
    *r = 0;
    p = path;
    for (;;) {
	q = strchr (p + 1, PATH_SEP);
	if (!q) {
	    q = strchr (p + 1, 0);
	    if (q == p + 1)
	        break;
	}
	c = *q;
	*q = 0;
	if (mc_lstat (path, &mybuf) < 0) {
	    g_free (buf);
	    g_free (buf2);
	    *q = c;
	    return NULL;
	}
	if (!S_ISLNK (mybuf.st_mode))
	    strcpy (r, p + 1);
	else {
	    len = mc_readlink (path, buf2, MC_MAXPATHLEN);
	    if (len < 0) {
		 g_free (buf);
		 g_free (buf2);
		*q = c;
		return NULL;
	    }
	    buf2 [len] = 0;
	    if (*buf2 == PATH_SEP)
		strcpy (buf, buf2);
	    else
		strcpy (r, buf2);
	}
	canonicalize_pathname (buf);
	r = strchr (buf, 0);
	if (!*r || *(r - 1) != PATH_SEP) {
	    *r++ = PATH_SEP;
	    *r = 0;
	}
	*q = c;
	p = q;
	if (!c)
	    break;
    }
    if (!*buf)
	strcpy (buf, PATH_SEP_STR);
    else if (*(r - 1) == PATH_SEP && r != buf + 1)
	*(r - 1) = 0;
    g_free (buf2);
    return buf;
}

/* Finds out a relative path from first to second, i.e. goes as many ..
 * as needed up in first and then goes down using second */
char *diff_two_paths (char *first, char *second) 
{
    char *p, *q, *r, *s, *buf = 0;
    int i, j, prevlen = -1, currlen;
    
    first = resolve_symlinks (first);
    if (first == NULL)
        return NULL;
    for (j = 0; j < 2; j++) {
	p = first;
	if (j) {
	    second = resolve_symlinks (second);
	    if (second == NULL) {
		 g_free (first);
	        return buf;
	    }
	}
	q = second;
	for (;;) {
	    r = strchr (p, PATH_SEP);
	    s = strchr (q, PATH_SEP);
	    if (!r || !s)
	      break;
	    *r = 0; *s = 0;
	    if (strcmp (p, q)) {
		*r = PATH_SEP; *s = PATH_SEP;
		break;
	    } else {
		*r = PATH_SEP; *s = PATH_SEP;
	    }
	    p = r + 1;
	    q = s + 1;
	}
	p--;
	for (i = 0; (p = strchr (p + 1, PATH_SEP)) != NULL; i++);
	currlen = (i + 1) * 3 + strlen (q) + 1;
	if (j) {
	    if (currlen < prevlen)
	        g_free (buf);
	    else {
		 g_free (first);
		 g_free (second);
		return buf;
	    }
	}
	p = buf = g_malloc (currlen);
	prevlen = currlen;
	for (; i >= 0; i--, p += 3)
	  strcpy (p, "../");
	strcpy (p, q);
    }
    g_free (first);
    g_free (second);
    return buf;
}

#ifndef HAVE_TRUNCATE
/* On SCO and Windows NT systems */
int my_ftruncate (int fd, long size)
{
#ifdef OS2_NT
    if(_chsize(fd, size))
	return -1;
    else 
	return 0;
#else
    struct flock lk;
    
    lk.l_whence = 0;
    lk.l_start = size;
    lk.l_len = 0;
    
    return fcntl (fd, F_FREESP, &lk);
#endif
}

int truncate (const char *path, long size)
{
    int fd;
    int res;
    
    fd = open (path, O_RDWR, 0);
    if (fd < 0)
	return fd;
    res = my_ftruncate (fd, size);
    if (res < 0)
	return res;
    close (fd);
    return 0;

}

#endif
#endif /* VFS_STANDALONE */

/* If filename is NULL, then we just append PATH_SEP to the dir */
char *
concat_dir_and_file (const char *dir, const char *file)
{
    int i = strlen (dir);
    
    if (dir [i-1] == PATH_SEP)
	return  g_strconcat (dir, file, NULL);
    else
	return  g_strconcat (dir, PATH_SEP_STR, file, NULL);
}

