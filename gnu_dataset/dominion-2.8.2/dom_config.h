/* dom_config.h - Things the user should configure */

#include <config.h>

/*
 * Copyright (C) 1990 Free Software Foundation, Inc.
 * Written by the dominion project.
 *
 * This file is part of dominion.
 *
 * dominion is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/* Here you should define what kind of system you are compiling on.  Note
   that more than one of these can be defined simultaneously.
*/

/* this section that tries to do automatic definition of BSD or SYSV;
 * note that for some weird versions of UNIX you still need to add
 * some the defines listed here.
 */
#undef BSD
#undef SYSV
#if defined(linux) || defined(_AIX) || defined(sun) || defined(NeXT) || defined(__convex__) || defined(__osf__) || defined(__bsdi__)
/* # define BSD */
/* # undef SYSV */
#else
# if defined(__sgi) || defined(__hpux)
/* #  define SYSV */
/* #  undef BSD */
# else
/* this should cause a syntax error!! */
/* #error PLEASE_DEFINE_BSD_OR_SYSV_IN_dom_config_h */
#endif /* on the #else */
#endif /* UNIX version definitions */

/* #define PMAX */           /* if you are using ultrix. Fixes mvwprintw bug */
/* #define ANDREW */         /* if you are using andrew games authentication */
/* #define ridge */          /* if you are using a ridge system              */
/* #define u3b2 */           /* if you are using ATT 3b2                     */
/* #define sun2 */           /* if you are using sun 2 system (old)          */

#ifdef UNNEEDED
/* Define HAVE_STRSTR if your system has strstr().  We provide a replacement
   which is just a quick hack.  On unix, "man strstr" may help you determine
   if you have this or not.  Typically SYSV had strstr(), and vanilla BSD
   does not.  AIX does, though, and so does linux.
*/
#if defined(SYSV) || defined(_AIX) || defined(linux) || defined(__bsdi__) || defined(NeXT) || defined(__osf__)
# define HAVE_STRSTR
#endif /* SYSV || AIX || linux || __bsdi__ || NeXT || __osf__ */
#endif /* UNNEEDED */

/* MAILER is the mail program that will be used to forward mail and news
   to a player's e-mail address.  It should be set up so that you can put
   addresses after it and then type the message.
*/

#define MAILER "Mail -s Dominion_Mail"

/* define UID_SECURITY if you are running unix and you don't want users to
   gain access to game files via shell escapes.  This will change the effective
   userid from the dominion game to the real user before invoking editors.
   This requires the unix setuid() calls.  Be sure to read the security
   section of the README file.
*/

#define UID_SECURITY

/* Define NO_FILE_ACCESS if you want a really secure game.  This will not
   allow users to call programs that may allow reading of system files.
   This will disable the user's mail-reading program option, and disable
   dumping and viewing the map and reports.
*/

#define NO_FILE_ACCESS

/* DEFAULT_EDITOR is a basic visual editor which the user will use to
   compose mail and news messages.  Emacs and vi are good choices.
         Microemacs is probably better as it is faster and probably
         a little more secure ... but not many places have it. Another
	 good choice would be pico
*/

#define DEFAULT_EDITOR "vi"	/* insecure if using dominion as shell */

/* The following probably doesn't need to be changed.  Hmmmm */

/* #define INFO_INTRO "info_intro" */

/* The following should be defined if your system lacks a "beep()" call;
   note: typically SYSV has beep(), and BSD does not.
 */
/*
#ifdef BSD
# define NOBEEP
#endif
*/ /* BSD */

/* string.h/strings.h is another sore spot btween BSD/SYSV */
#ifdef UNNEEDED
#ifdef BSD
# include <strings.h>
#else /* BSD */
# include <string.h>
#endif /* BSD */
#endif /* UNNEEDED */

/* The following determines whether or not your system has a limits.h file */
/* If your system does not have limits.h, comment out the #define line */

#if !defined(ridge) && !defined(u3b2) && !defined(sun2)
#define HAVE_LIMITS
#endif
