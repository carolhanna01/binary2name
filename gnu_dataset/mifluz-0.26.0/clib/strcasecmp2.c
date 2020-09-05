//
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later 
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: strcasecmp2.c,v 1.2 2014/04/17 20:27:24 sebdiaz Exp $
//

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /* HAVE_CONFIG_H */

#include <ctype.h>

//*****************************************************************************
//
#ifndef HAVE_STRCASECMP
int strcasecmp(const char *str1, const char *str2)
{
    if (!str1 && !str2)
	return 0;
    if (!str1)
	return 1;
    if (!str2)
	return -1;
    while (*str1 &&
	   *str2 &&
	   tolower((unsigned char)*str1) == tolower((unsigned char)*str2))
    {
	str1++;
	str2++;
    }

    return tolower((unsigned char)*str1) - tolower((unsigned char)*str2);
}
#endif