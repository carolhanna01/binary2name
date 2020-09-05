/*
 *  acm : an aerial combat simulator for X
 *  Copyright (C) 1991-1998  Riley Rainey
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

#include "/usr/include/sys/types.h"
#include "/usr/include/sys/ipc.h"
#include "/usr/include/sys/shm.h"
#include "/usr/include/sys.s"

#define	SHMSYS	SYS_shmsys

#define	SHMAT	0
#define	SHMCTL	1
#define	SHMDT	2
#define	SHMGET	3

char     *
shmat(shmid, shmaddr, shmflg)
int       shmid;
char     *shmaddr;
int       shmflg;
{
	return (char *) syscall(SHMSYS, SHMAT, shmid, shmaddr, shmflg);
}

int
shmctl(shmid, cmd, buf)
int       shmid, cmd;
struct shmid_ds *buf;
{
	return syscall(SHMSYS, SHMCTL, shmid, cmd, buf);
}

int
shmdt(shmaddr)
char     *shmaddr;
{
	return syscall(SHMSYS, SHMDT, shmaddr);
}

int
shmget(key, size, shmflg)
key_t     key;
int       size, shmflg;
{
	return syscall(SHMSYS, SHMGET, key, size, shmflg);
}
