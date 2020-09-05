/* files.h.in -- defines for various file locations etc

   Copyright 2009 Free Software Foundation, Inc.

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

/* Location of various files
   We pretend this is a shell script for autoconf as it saves too many
  defines with loads of \$s in.
  datarootdir=/usr/local/share */

#define GNU_BATCH_VERSION               1.9
#define GNU_BATCH_MAJOR_VERSION         1
#define GNU_BATCH_VERSION_STRING        "1.9"

#define MASTER_CONFIG   "/usr/local/etc/gnubatch.conf"
#define ENV_SELECT_VAR  "GNUBATCH_ENV"
#define USER_CONFIG     ".gnubatch"
#define HOME_CONFIG_DIR ".gbch"
#define HOME_CONFIG_FILE "gnubatch1"
#define HOME_CONFIG     "~/" HOME_CONFIG_DIR "/" HOME_CONFIG_FILE
#define ENVIR_FILE      "${GNUBATCHENV-/usr/local/etc/gnubatch-env}"
#define HOSTFILE        "${HOSTFILE-/usr/local/etc/gnubatch.hosts}"
#define CONFIGPATH      "${GB_CONFIGPATH-@:~:!:.:-}"
#define HELPPATH        "${GB_HELPPATH-.:!:~:@}"
#define VFILE           "btsched_vfile"
#define JFILE           "btsched_jfile"
#define JMMAP_FILE      "btmm_jobs"
#define VMMAP_FILE      "btmm_vars"
#define XFMMAP_FILE     "btmm_xfer"
#define JLOCK_FILE      "btjob.lock"
#define VLOCK_FILE      "btvar.lock"
#define XLOCK_FILE      "btxfer.lock"
#define REPFILE         "btsched_reps"
#define MISC_UCONFIG    "GBCH_RESTCONF"

#define LUMPSIZE        "${MSGTXSIZE-20}"
#define LUMPWAIT        "${MSGTXDELAY-2}"

#define DEF_CI_NAME     "sh"
#define DEF_CI_PATH     "/bin/sh"
#define DEF_CI_ARGS     "-s"
#define DEF_BASE_NICE   (-20)
#define DEF_CI_NICE     24

#define SPNAM   "SP"            /* Spool files */
#define SONAM   "SO"            /* Std output */
#define SENAM   "ER"            /* Std error (default) */
#define NTNAM   "NT"            /* Network temporary copy */

#define BATCHUNAME      "gnubatch"
#define XBDEFNAME       "default"

#define SPDIR_RAW       "/usr/local/var/gnubatch"
#define BTUFILE         "${SPOOLDIR-/usr/local/var/gnubatch}/btufile1.9"
#define BTCI            "${SPOOLDIR-/usr/local/var/gnubatch}/cifile"
#define HOLFILE         "${SPOOLDIR-/usr/local/var/gnubatch}/holfile"
#define SPDIR           "${SPOOLDIR-/usr/local/var/gnubatch}"
#define BTSCHED         "${SPROGDIR-/usr/local/libexec/gnubatch}/btsched"
#define XBNETSERV       "${SPROGDIR-/usr/local/libexec/gnubatch}/xbnetserv"
#define XBTQ_ICON       "${SPHELPDIR-/usr/local/share/gnubatch/help}/xbtq.xpm"
#define XBTQ_MENU       "${SPHELPDIR-/usr/local/share/gnubatch/help}/xbtq.menu"
#define XBTQVIEW_MENU   "${SPHELPDIR-/usr/local/share/gnubatch/help}/xbtqview.menu"
#define XBTQSEL_MENU    "${SPHELPDIR-/usr/local/share/gnubatch/help}/xbtqsel.menu"
#define XBTUSER_ICON    "${SPHELPDIR-/usr/local/share/gnubatch/help}/xbtuser.xpm"
#define XBTUSER_MENU    "${SPHELPDIR-/usr/local/share/gnubatch/help}/xbtuser.menu"
#define XBTR_ICON       "${SPHELPDIR-/usr/local/share/gnubatch/help}/xbtr.xpm"
#define XBTR_MENU       "${SPHELPDIR-/usr/local/share/gnubatch/help}/xbtr.menu"
#define MAILER          "${MAILER-}"
#define WRITER          "${SPROGDIR-/usr/local/libexec/gnubatch}/btwrite"
#define DOSWRITER       "${SPROGDIR-/usr/local/libexec/gnubatch}/dosbtwrite"
#define MSGDISPATCH     "${SPROGDIR-/usr/local/libexec/gnubatch}/btmdisp"
#define EXECPROG        "${SPROGDIR-/usr/local/libexec/gnubatch}/btexec"
#define GTKSAVE         "${SPROGDIR-/usr/local/libexec/gnubatch}/bgtksave"
#define GTKLDSAV        "${SPROGDIR-/usr/local/libexec/gnubatch}/bgtkldsv"
#define DUMPJOB         "${SPROGDIR-/usr/local/libexec/gnubatch}/jobdump"
#define XMLDUMPJOB      "${SPROGDIR-/usr/local/libexec/gnubatch}/xmljobdump"
#define CFILEDIR        "${SPHELPDIR-/usr/local/share/gnubatch/help}/"

#define INT_CONFIG      "${SPHELPDIR-/usr/local/share/gnubatch/help}/btint-config"

#define BTPWPROG        "${SPROGDIR-/usr/local/libexec/gnubatch}/btpwchk"

#define XIHTMLINI       "${SPROGDIR-/usr/local/share/gnubatch}/gbhtml.ini"
#define HTML_UFILE      "${SPROGDIR-/usr/local/share/gnubatch}/html_ufile"
#define HTML_TMPFILE    "/tmp/gbcgi%ld.%d"
#define WINUSER_MAP     "/usr/local/etc/gbuser.map"
#define XIPWFILE        "${SPROGDIR-/usr/local/libexec/gnubatch}/gbpwfile"

#define BTR_PROGRAM     "gbch-r"
#define XBTR_PROGRAM    "gbch-xr"

#define XMLJOBSUFFIX    ".gbj1"
#define NSID            "http://www.fsf.org/gnubatch/gbj1"

#define XBTIMEOUTS      "${NETTIMEOUT:1000}"

#define HALTSIG         "${HALTALL-9}"

#define BTSCHEDTRACE    "${SCHEDTRACE-0}"
#define BTSCHEDTRFILE   "${SCHEDTRFILE-/usr/local/var/gnubatch/schedtrace}"
#define XBNETTRACE      "${NETTRACE-0}"
#define XBNETTRFILE     "${NETTRFILE-/usr/local/var/gnubatch/xbnettrace}"

extern  void    versionprint(char **, const char *, const int);
extern  void    init_mcfile();
extern  void    init_xenv();
extern  char    *envprocess(const char *);
extern  char    *mkspdirfile(const char *);
extern  char    *mkspid(const char *, const jobno_t);
extern  char    **squash_envir(char **, char **);

extern  char    *envselect_name;
extern  int     envselect_value;
