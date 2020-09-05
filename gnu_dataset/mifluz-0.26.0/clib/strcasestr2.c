//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later 
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: strcasestr2.c,v 1.3 2014/04/17 20:27:24 sebdiaz Exp $
//

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <ctype.h>
#include <string.h>

#include <clib.h>

//*****************************************************************************
//
const char *
strcasestr2(const char *s, const char *pattern)
{
    int		length = strlen(pattern);

    while (*s)
    {
	if (strncasecmp(s, pattern, length) == 0)
	    return s;
	s++;
    }
    return 0;
}
