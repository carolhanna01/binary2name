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

#define	MASTER_CONFIG	"${prefix}/etc/gnuspool.conf"
#define	EXTERNSPOOL	"${prefix}/etc/gnuspool.ext"
#define USER_CONFIG	".gnuspool"
#define	HOSTFILE	"${HOSTFILE-${prefix}/etc/gnuspool.hosts}"
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
#define	SPUFILE		"${SPOOLDIR-${prefix}/var/gnuspool}/spufile1"
#define	SPDIR		"${SPOOLDIR-${prefix}/var/gnuspool}"
#define	PTDIR		"${SPOOLPT-${datarootdir}/gnuspool/ptrs}"
#define	IPROGDIR	"${SPROGDIR-${exec_prefix}/libexec/gnuspool}"
#define	IDATADIR	"${SDATADIR-${datarootdir}/gnuspool}"
#define	SPSHED		"${SPROGDIR-${exec_prefix}/libexec/gnuspool}/spshed"
#define	XTNETSERV	"${SPROGDIR-${exec_prefix}/libexec/gnuspool}/xtnetserv"
#define	DAEM		"${SPROGDIR-${exec_prefix}/libexec/gnuspool}/spd"
#define	DAEMINIT	"${SPROGDIR-${exec_prefix}/libexec/gnuspool}/spdinit"
#define	DUMPJOB		"${SPROGDIR-${exec_prefix}/libexec/gnuspool}/spjobdump"
#define	WRITER		"${SPROGDIR-${exec_prefix}/libexec/gnuspool}/spwrite"
#define	DOSWRITER	"${SPROGDIR-${exec_prefix}/libexec/gnuspool}/dosspwrite"
#define	CFILEDIR	"${SPHELPDIR-${datarootdir}/gnuspool/help}/"
#define	INT_CONFIG	"${SPHELPDIR-${datarootdir}/gnuspool/help}/int-config"
#define	XSPQ_ICON	"${SPHELPDIR-${datarootdir}/gnuspool/help}/xspq.xpm"
#define XSPQ_MENU	"${SPHELPDIR-${datarootdir}/gnuspool/help}/xspq.menu"
#define XSPQVIEW_MENU	"${SPHELPDIR-${datarootdir}/gnuspool/help}/xspqview.menu"
#define XSPQSEL_MENU	"${SPHELPDIR-${datarootdir}/gnuspool/help}/xspqsel.menu"
#define	XSPUSER_ICON	"${SPHELPDIR-${datarootdir}/gnuspool/help}/xspuser.xpm"
#define XSPUSER_MENU	"${SPHELPDIR-${datarootdir}/gnuspool/help}/xspuser.menu"
#define	MAILER		"${MAILER-}"
#define	SHELL		"${SHELL-/bin/sh}"
#define MSGDISPATCH	"${SPROGDIR-${exec_prefix}/libexec/gnuspool}/spmdisp"
#define EXECPROG	"${SPROGDIR-${exec_prefix}/libexec/gnuspool}/spexec"
#define GTKSAVE		"${SPROGDIR-${exec_prefix}/libexec/gnuspool}/gtksave"
#define	PTRMSGS		"SPPTRMSGS"
#define	SPPWPROG	"${SPROGDIR-${exec_prefix}/libexec/gnuspool}/sppwchk"
#define	SPLPQPROG	"${SPROGDIR-${exec_prefix}/libexec/gnuspool}/xtlpq"
#define	SPLPRMPROG	"${SPROGDIR-${exec_prefix}/libexec/gnuspool}/xtlprm"
#define	DUMPPWFILE	"${SPOOLDIR-${prefix}/var/gnuspool}/pwdump1"
#define	GSHTMLINI	"${SDATADIR-${datarootdir}/gnuspool}/gshtml.ini"
#define	HTML_UFILE	"${SDATADIR-${datarootdir}/gnuspool}/html_ufile"
#define HTML_TMPFILE	"/tmp/gscgi%ld.%d"
#define GSPWFILE	"${SDATADIR-${datarootdir}/gnuspool}/gspwfile"

#define	XTNETTRACE	"${NETTRACE-0}"
#define	XTNETTRFILE	"${NETTRFILE-${prefix}/var/gnuspool/xtnettrace}"
