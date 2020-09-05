/* infiles.h.in -- template for "files.h" built by configure

   Copyright 2008 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

extern void	versionprint(char **, const char *, const int);
extern void	init_mcfile(void);
extern char 	*envprocess(const char *);
extern char 	*mkspdirfile(const char *);
extern char 	*mkspid(const char *, const jobno_t);

#define	GNU_SPOOL_VERSION	1.8
#define	GNU_SPOOL_MAJOR_VERSION	1
#define	GNU_SPOOL_VERSION_STRING	"1.8"

#define	MASTER_CONFIG	"/usr/local/etc/gnuspool.conf"
#define	EXTERNSPOOL	"/usr/local/etc/gnuspool.ext"
#define USER_CONFIG	".gnuspool"
#define	HOSTFILE	"${HOSTFILE-/usr/local/etc/gnuspool.hosts}"
#define	CONFIGPATH	"${GS_CONFIGPATH-~:!:.:-}"
#define	HELPPATH	"${GS_HELPPATH-.:!:~}"
#define	DEFAULT_FORM	"${DEFAULT_FORM-default}"

#define	JIMMAP_FILE	"spmm_jobi"
#define	JDMMAP_FILE	"spmm_jobd"
#define	PMMAP_FILE	"spmm_ptrs"
#define	XFMMAP_FILE	"spmm_xfer"
#define	JLOCK_FILE	"spjob.lock"
#define	PLOCK_FILE	"spptr.lock"
#define	XLOCK_FILE	"spxfer.lock"

#define	PFILE		"spshed_pfile"
#define	JFILE		"spshed_jfile"
#define	REPFILE		"spshed_reps"
#define	MISC_UCONFIG	"SPRESTCONF"
#define	LPDINTCONF	"SPLPDCONF"
#define	HELPNAME	"-${SPOOLHELP-Help}"

#define	PDEVFILE	".device"

#define	SPNAM	"SP"
#define	PFNAM	"PF"
#define	ERNAM	"ER"

#define	SPUNAME		"gnuspool"
#define	XTDEFNAME	"default"
#define	FIFO_DIR	"FIFODIR"

#define	LUMPSIZE	"${MSGTXSIZE-50}"
#define	LUMPWAIT	"${MSGTXDELAY-2}"
#define	CLOSEDELAY	"${TCPDELAY-1}"
#define	SPUFILE		"${SPOOLDIR-/usr/local/var/gnuspool}/spufile1"
#define	SPDIR		"${SPOOLDIR-/usr/local/var/gnuspool}"
#define	PTDIR		"${SPOOLPT-/usr/local/share/gnuspool/ptrs}"
#define	IPROGDIR	"${SPROGDIR-/usr/local/libexec/gnuspool}"
#define	IDATADIR	"${SDATADIR-/usr/local/share/gnuspool}"
#define	SPSHED		"${SPROGDIR-/usr/local/libexec/gnuspool}/spshed"
#define	XTNETSERV	"${SPROGDIR-/usr/local/libexec/gnuspool}/xtnetserv"
#define	DAEM		"${SPROGDIR-/usr/local/libexec/gnuspool}/spd"
#define	DAEMINIT	"${SPROGDIR-/usr/local/libexec/gnuspool}/spdinit"
#define	DUMPJOB		"${SPROGDIR-/usr/local/libexec/gnuspool}/spjobdump"
#define	WRITER		"${SPROGDIR-/usr/local/libexec/gnuspool}/spwrite"
#define	DOSWRITER	"${SPROGDIR-/usr/local/libexec/gnuspool}/dosspwrite"
#define	CFILEDIR	"${SPHELPDIR-/usr/local/share/gnuspool/help}/"
#define	INT_CONFIG	"${SPHELPDIR-/usr/local/share/gnuspool/help}/int-config"
#define	XSPQ_ICON	"${SPHELPDIR-/usr/local/share/gnuspool/help}/xspq.xpm"
#define XSPQ_MENU	"${SPHELPDIR-/usr/local/share/gnuspool/help}/xspq.menu"
#define XSPQVIEW_MENU	"${SPHELPDIR-/usr/local/share/gnuspool/help}/xspqview.menu"
#define XSPQSEL_MENU	"${SPHELPDIR-/usr/local/share/gnuspool/help}/xspqsel.menu"
#define	XSPUSER_ICON	"${SPHELPDIR-/usr/local/share/gnuspool/help}/xspuser.xpm"
#define XSPUSER_MENU	"${SPHELPDIR-/usr/local/share/gnuspool/help}/xspuser.menu"
#define	MAILER		"${MAILER-}"
#define	SHELL		"${SHELL-/bin/sh}"
#define MSGDISPATCH	"${SPROGDIR-/usr/local/libexec/gnuspool}/spmdisp"
#define EXECPROG	"${SPROGDIR-/usr/local/libexec/gnuspool}/spexec"
#define GTKSAVE		"${SPROGDIR-/usr/local/libexec/gnuspool}/gtksave"
#define	PTRMSGS		"SPPTRMSGS"
#define	SPPWPROG	"${SPROGDIR-/usr/local/libexec/gnuspool}/sppwchk"
#define	SPLPQPROG	"${SPROGDIR-/usr/local/libexec/gnuspool}/xtlpq"
#define	SPLPRMPROG	"${SPROGDIR-/usr/local/libexec/gnuspool}/xtlprm"
#define	DUMPPWFILE	"${SPOOLDIR-/usr/local/var/gnuspool}/pwdump1"
#define	GSHTMLINI	"${SDATADIR-/usr/local/share/gnuspool}/gshtml.ini"
#define	HTML_UFILE	"${SDATADIR-/usr/local/share/gnuspool}/html_ufile"
#define HTML_TMPFILE	"/tmp/gscgi%ld.%d"
#define GSPWFILE	"${SDATADIR-/usr/local/share/gnuspool}/gspwfile"

#define	XTNETTRACE	"${NETTRACE-0}"
#define	XTNETTRFILE	"${NETTRFILE-/usr/local/var/gnuspool/xtnettrace}"
