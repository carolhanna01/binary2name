/*
 *  DIS/x : An implementation of the IEEE 1278.1 protocol
 *  Copyright (C) 1996, Riley Rainey (rainey@netcom.com)
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software Foundaation,
 *  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#include <string.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>

char     *
SIMxGetSiteName(char *result, int max_size)
{
	char     *s;
#ifndef WIN32
	char      path[MAXPATHLEN];
#endif
	static char line[64];
	FILE     *f;
	int       n;

/*
 *  First look for the environment variable "DIS_SITE_NAME" ...
 */

	s = getenv("DIS_SITE_NAME");
	if (s == (char *) NULL || strlen(s) == 0) {

/*
 *  "Cyberspace" becomes the default answer if all these other tests fail ...
 */

		strcpy(line, "Cyberspace");
		s = line;

/*
 *  Now check $HOME/.dis_site_name ...
 */
#ifndef WIN32
		strcpy(path, getenv("HOME"));
		strcat(path, "/.dis_site_name");
		if ((f = fopen(path, "r")) != NULL) {
			fgets(line, sizeof(line), f);
			n = strlen(line);
			if (n > 0 && line[n - 1] == '\n') {
				line[n - 1] = '\0';
			}
			fclose(f);
		}

/*
 *  See if /etc/dis_site_name exists ...
 */

		else
#endif
		if ((f = fopen("/etc/dis_site_name", "r")) != NULL) {
			fgets(line, sizeof(line), f);
			n = strlen(line);
			if (n > 0 && line[n - 1] == '\n') {
				line[n - 1] = '\0';
			}
			fclose(f);
		}
	}

	strncpy(result, s, max_size - 1);
	result[max_size - 1] = '\0';
	return s;
}
