//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later 
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: strncasecmp2.c,v 1.2 2014/04/17 20:27:24 sebdiaz Exp $
//

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <ctype.h>

//*****************************************************************************
//
#ifndef HAVE_STRNCASECMP
int strncasecmp(const char *str1, const char *str2, int n)
{
    if (!str1 && !str2)
	return 0;
    if (!str1)
	return 1;
    if (!str2)
	return -1;
    if (n < 0)
	return 0;
    while (n &&
	   *str1 &&
	   *str2 &&
	   tolower((unsigned char)*str1) == tolower((unsigned char)*str2))
    {
	str1++;
	str2++;
	n--;
    }

    return n == 0 ? 0 :
	tolower((unsigned char)*str1) - tolower((unsigned char)*str2);
}
#endif