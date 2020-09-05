/*
 * userv service (or standalone program)
 * for per-user IP subranges.
 *
 * This is invoked as root, directly from userv.
 * Its arguments are supposed to be, in order:
 *  <config>
 *      Specifies address ranges and gids which own them.
 *  --  Indicates that the remaining arguments are user-supplied
 *      and therefore untrusted.
 *  <local-addr>,<peer-addr>,<mtu>,<proto>
 *      As for slattach.  Supported protocols are slip, cslip, and
 *      adaptive.  Alternatively, set to `debug' to print debugging
 *      info.  <local-addr> is address of the interface on chiark;
 *      <peer-addr> is the address of the point-to-point peer.
 *  <prefix>/<mask>,<prefix>/<mask>,...
 *      List of additional routes to add for this interface.
 *      May be the empty argument, or `-' if this is problematic.
 *
 * <config> is either
 *    <gid>,<prefix>/<len>[,<junk>]
 *      indicating that that gid may allocate addresses in
 *      the relevant subspace (<junk> is ignored)
 * or #...
 *      which is a comment
 * or /<config-file-name> or ./<config-file-name> or ../<config-file-name>
 *      which refers to a file which contains lines which
 *      are each <config>
 * or *
 *      which means that anything is permitted
 * 
 * Should be run from userv with no-disconnect-hup.
 */
/*
 * Copyright (C) 1999 Ian Jackson
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with userv-utils; if not, write to the Free Software
 * Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 * $Id: service.c,v 1.10 1999/11/09 22:35:41 ian Exp $
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <errno.h>
#include <stdarg.h>
#include <ctype.h>
#include <limits.h>
#include <signal.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>

#define NARGS 4
#define MAXEXROUTES 5
#define ATXTLEN 16

static const unsigned long gidmaxval= (unsigned long)((gid_t)-2);
static const char *const protos_ok[]= { "slip", "cslip", "adaptive", 0 };
static const int signals[]= { SIGHUP, SIGINT, SIGTERM, 0 };

static const char *configstr, *proto;
static unsigned long localaddr, peeraddr, mtu;
static int localallow, peerallow, allallow;
static int nexroutes;
static struct exroute {
  unsigned long prefix, mask;
  int allow;
  char prefixtxt[ATXTLEN], masktxt[ATXTLEN];
} exroutes[MAXEXROUTES];

static char localtxt[ATXTLEN];
static char peertxt[ATXTLEN];

static struct pplace {
  struct pplace *parent;
  const char *filename;
  int lineno;
} *cpplace;


static int slpipe[2], ptmaster, undoslattach;
static const char *ifname;
static const char *ptyname;

#define NPIDS 4

static union {
  struct { pid_t sl, cout, cin, task; } byname;
  pid_t bynumber[NPIDS];
} pids;
sigset_t emptyset, fullset;


static int cleantask(void) {
  pid_t pid;

  pid= fork();
  if (!pid) return 1;
  if (pid == (pid_t)-1)
    perror("userv-ipif: fork for undo slattach failed - cannot clean up properly");
  return 0;
}

static void terminate(int estatus) {
  int i, status;
  pid_t pid;
  
  for (i=0; i<NPIDS; i++)
    if (pids.bynumber[i]) kill(pids.bynumber[i], SIGTERM);

  if (undoslattach) {
    if (cleantask()) {
      execlp("slattach", "slattach", "-p", "tty", ptyname, (char*)0);
      perror("userv-ipif: exec slattach for undo slattach failed");
      exit(-1);
    }
    if (ifname && cleantask()) {
      execlp("ifconfig", "ifconfig", ifname, "down", (char*)0);
      perror("userv-ipif: exec ifconfig for undo ifconfig failed");
      exit(-1);
    }
  }

  for (;;) {
    pid= waitpid(-1,&status,0);
    if (pid == (pid_t)-1) break;
  }
  exit(estatus);
}


static void fatal(const char *fmt, ...)
     __attribute__((format(printf,1,2)));
static void fatal(const char *fmt, ...) {
  va_list al;
  va_start(al,fmt);

  fputs("userv-ipif service: fatal error: ",stderr);
  vfprintf(stderr, fmt, al);
  putc('\n',stderr);
  terminate(8);
}
  
static void sysfatal(const char *fmt, ...)
     __attribute__((format(printf,1,2)));
static void sysfatal(const char *fmt, ...) {
  va_list al;
  int e;

  e= errno;
  va_start(al,fmt);

  fputs("userv-ipif service: fatal system error: ",stderr);
  vfprintf(stderr, fmt, al);
  fprintf(stderr,": %s\n", strerror(e));
  terminate(12);
}


static void badusage(const char *fmt, ...)
     __attribute__((format(printf,1,2)));
static void badusage(const char *fmt, ...) {
  va_list al;
  struct pplace *cpp;
  
  if (cpplace) {
    fprintf(stderr,
	    "userv-ipif service: %s:%d: ",
	    cpplace->filename, cpplace->lineno);
  } else {
    fputs("userv-ipif service: invalid usage: ",stderr);
  }
  va_start(al,fmt);
  vfprintf(stderr, fmt, al);
  putc('\n',stderr);

  if (cpplace) {
    for (cpp=cpplace->parent; cpp; cpp=cpp->parent) {
      fprintf(stderr,
	      "userv-ipif service: %s:%d: ... in file included from here\n",
	      cpp->filename, cpp->lineno);
    }
  }
  terminate(16);
}

static char *ip2txt(unsigned long addr, char *buf) {
  sprintf(buf, "%lu.%lu.%lu.%lu",
	  (addr>>24) & 0x0ff,
	  (addr>>16) & 0x0ff,
	  (addr>>8) & 0x0ff,
	  (addr) & 0x0ff);
  return buf;
}

static unsigned long eat_number(const char **argp, const char *what,
				unsigned long min, unsigned long max,
				const char *endchars, int *endchar_r) {
  /* If !endchars then the endchar must be a nul, otherwise it may be
   * a nul (resulting in *argp set to 0) or something else (*argp set
   * to point to after delim, *endchar_r set to delim).
   * *endchar_r may be 0.
   */
  unsigned long rv;
  char *ep;
  int endchar;

  if (!*argp) { badusage("missing number %s",what); }
  rv= strtoul(*argp,&ep,0);
  if ((endchar= *ep)) {
    if (!endchars) badusage("junk after number %s",what);
    if (!strchr(endchars,endchar))
      badusage("invalid character or delimiter `%c' in or after number, %s:"
	       " expected %s (or none?)", endchar,what,endchars);
    *argp= ep+1;
  } else {
    *argp= 0;
  }
  if (endchar_r) *endchar_r= endchar;
  if (rv < min || rv > max) badusage("number %s value %lu out of range %lu..%lu",
				     what, rv, min, max);
  return rv;
}

static void addrnet_mustdiffer(const char *w1, unsigned long p1, unsigned long m1,
			       const char *w2, unsigned long p2, unsigned long m2) {
  unsigned long mask;

  mask= m1&m2;
  if ((p1 & mask) != (p2 & mask)) return;
  badusage("%s %08lx/%08lx overlaps/clashes with %s %08lx/%08lx",
	   w1,p1,m1, w2,p2,m2);
}
  
static unsigned long eat_addr(const char **argp, const char *what,
			      const char *endchars, int *endchar_r) {
  char whatbuf[100];
  unsigned long rv;
  int i;

  for (rv=0, i=0;
       i<4;
       i++) {
    rv <<= 8;
    sprintf(whatbuf,"%s byte #%d",what,i);
    rv |= eat_number(argp,whatbuf, 0,255, i<3 ? "." : endchars, endchar_r);
  }

  return rv;
}

static void eat_prefixmask(const char **argp, const char *what,
			   const char *endchars, int *endchar_r,
			   unsigned long *prefix_r, unsigned long *mask_r, int *len_r) {
  /* mask_r and len_r may be 0 */
  char whatbuf[100];
  int len;
  unsigned long prefix, mask;

  prefix= eat_addr(argp,what, "/",0);
  sprintf(whatbuf,"%s length",what);
  len= eat_number(argp,whatbuf, 0,32, endchars,endchar_r);

  mask= (~0UL << (32-len));
  if (prefix & ~mask) badusage("%s prefix %08lx not fully contained in mask %08lx",
			       what,prefix,mask);
  *prefix_r= prefix;
  if (mask_r) *mask_r= mask;
  if (len_r) *len_r= len;
}

static int addrnet_isin(unsigned long prefix, unsigned long mask,
			unsigned long mprefix, unsigned long mmask) {
  return  !(~mask & mmask)  &&  (prefix & mmask) == mprefix;
}
  

static void permit(unsigned long pprefix, unsigned long pmask) {
  int i, any;
  
  assert(!(pprefix & ~pmask));
  any= 0;

  if (!proto) fputs("permits",stdout);
  if (addrnet_isin(localaddr,~0UL, pprefix,pmask)) {
    if (!proto) fputs(" local-addr",stdout);
    any= localallow= 1;
  }
  if (addrnet_isin(peeraddr,~0UL, pprefix,pmask)) {
    if (!proto) fputs(" peer-addr",stdout);
    any= peerallow= 1;
  }
  for (i=0; i<nexroutes; i++) {
    if (addrnet_isin(exroutes[i].prefix,exroutes[i].mask, pprefix,pmask)) {
      if (!proto) printf(" route#%d",i);
      any= exroutes[i].allow= 1;
    }
  }
  if (!proto) {
    if (!any) fputs(" nothing",stdout);
    putchar('\n');
  }
}

static void pconfig(const char *configstr, int truncated);

static void pfile(const char *filename) {
  FILE *file;
  char buf[PATH_MAX];
  int l, truncated, c;
  struct pplace npp, *cpp;

  for (cpp=cpplace; cpp; cpp=cpp->parent) {
    if (!strcmp(cpp->filename,filename))
      badusage("recursive configuration file `%s'",filename);
  }

  file= fopen(filename,"r");
  if (!file)
    badusage("cannot open configuration file `%s': %s", filename, strerror(errno));

  if (!proto) printf("config file `%s':\n",filename);

  npp.parent= cpplace;
  npp.filename= filename;
  npp.lineno= 0;
  cpplace= &npp;

  while (fgets(buf, sizeof(buf), file)) {
    npp.lineno++;
    l= strlen(buf);
    if (!l) continue;

    truncated= (buf[l-1] != '\n');
    while (l>0 && isspace((unsigned char) buf[l-1])) l--;
    if (!l) continue;
    buf[l]= 0;

    if (truncated) {
      while ((c= getc(file)) != EOF && c != '\n');
      if (c == EOF) break;
    }

    pconfig(buf,truncated);
  }
  if (ferror(file))
    badusage("failed while reading configuration file: %s", strerror(errno));

  cpplace= npp.parent;
}

static void pconfig(const char *configstr, int truncated) {
  unsigned long fgid, tgid, pprefix, pmask;
  int plen;
  char ptxt[ATXTLEN];
  const char *gidlist;
  
  switch (configstr[0]) {
  case '*':
    if (strcmp(configstr,"*")) badusage("`*' directive must be only thing on line");
    permit(0UL,0UL);
    return;
    
  case '#':
    return;
    
  case '/': case '.':
    if (truncated) badusage("filename too long (`%.100s...')",configstr);
    pfile(configstr);
    return;
    
  default:
    if (!isdigit((unsigned char)configstr[0]))
      badusage("unknown configuration directive");
    
    fgid= eat_number(&configstr,"gid", 0,gidmaxval, ",",0);
    eat_prefixmask(&configstr,"permitted-prefix", ",",0, &pprefix,&pmask,&plen);
    if (!configstr && truncated) badusage("gid,prefix/len,... spec too long");

    if (!proto) printf(" %5lu,%s/%d: ", fgid, ip2txt(pprefix,ptxt), plen);

    gidlist= getenv("USERV_GID");
    if (!gidlist) fatal("USERV_GID not set");
    for (;;) {
      if (!gidlist) {
	if (!proto) printf("no matching gid\n");
	return;
      }
      tgid= eat_number(&gidlist,"userv-gid", 0,gidmaxval, " ",0);
      if (tgid == fgid) break;
    }
    permit(pprefix,pmask);
    return;
  }
}

static void checkallow(int allow, const char *what,
		       const char *prefixtxt, const char *masktxt) {
  if (allow) return;
  fprintf(stderr,"userv-ipif service: access denied for %s, %s/%s\n",
	  what, prefixtxt, masktxt);
  allallow= 0;
}

static void parseargs(int argc, const char *const *argv) {
  unsigned long routeaddr, routemask;
  const char *carg;
  const char *const *cprotop;
  int i;
  char erwhatbuf[100], erwhatbuf2[100];
  
  if (argc < NARGS+1) { badusage("too few arguments"); }
  if (argc > NARGS+1) { badusage("too many arguments"); }

  configstr= *++argv;
  
  carg= *++argv;
  if (strcmp(carg,"--")) badusage("separator argument `--' not found, got `%s'",carg);

  carg= *++argv;
  localaddr= eat_addr(&carg,"local-addr", ",",0);
  peeraddr= eat_addr(&carg,"peer-addr", ",",0);
  mtu= eat_number(&carg,"mtu", 576,65536, ",",0);
  localallow= peerallow= 0;
  
  if (!strcmp(carg,"debug")) {
    proto= 0;
  } else {
    for (cprotop= protos_ok;
	 (proto= *cprotop) && strcmp(proto,carg);
	 cprotop++);
    if (!proto) fatal("invalid protocol");
  }
  
  addrnet_mustdiffer("local-addr",localaddr,~0UL, "peer-addr",peeraddr,~0UL);
  
  carg= *++argv;
  if (strcmp(carg,"-")) {
    for (nexroutes=0;
	 carg && *carg;
	 nexroutes++) {
      if (nexroutes == MAXEXROUTES)
	fatal("too many extra routes (only %d allowed)",MAXEXROUTES);
      sprintf(erwhatbuf,"route#%d",nexroutes);
    
      eat_prefixmask(&carg,erwhatbuf, ",",0, &routeaddr,&routemask,0);
      if (routemask == ~0UL) {
	addrnet_mustdiffer(erwhatbuf,routeaddr,routemask, "local-addr",localaddr,~0UL);
	addrnet_mustdiffer(erwhatbuf,routeaddr,routemask, "peer-addr",peeraddr,~0UL);
      }
      for (i=0; i<nexroutes; i++) {
	sprintf(erwhatbuf2,"route#%d",i);
	addrnet_mustdiffer(erwhatbuf,routeaddr,routemask,
			   erwhatbuf2,exroutes[i].prefix,exroutes[i].mask);
      }
      exroutes[nexroutes].prefix= routeaddr;
      exroutes[nexroutes].mask= routemask;
      exroutes[nexroutes].allow= 0;
      ip2txt(routeaddr,exroutes[nexroutes].prefixtxt);
      ip2txt(routemask,exroutes[nexroutes].masktxt);
    }
  }

  ip2txt(localaddr,localtxt);
  ip2txt(peeraddr,peertxt);
}

static void checkpermit(void) {
  int i;
  char erwhatbuf[100];
  
  allallow= 1;
  checkallow(localallow,"local-addr", localtxt,"32");
  checkallow(peerallow,"peer-addr", peertxt,"32");
  for (i=0; i<nexroutes; i++) {
    sprintf(erwhatbuf, "route#%d", i);
    checkallow(exroutes[i].allow, erwhatbuf, exroutes[i].prefixtxt, exroutes[i].masktxt);
  }
  if (!allallow) fatal("access denied");
}

static void dumpdebug(void) __attribute__((noreturn));
static void dumpdebug(void) {
  int i;
  char erwhatbuf[100];
  
  printf("protocol: debug\n"
	 "local:    %08lx == %s\n"
	 "peer:     %08lx == %s\n"
	 "mtu:      %ld\n"
	 "routes:   %d\n",
	 localaddr, localtxt,
	 peeraddr, peertxt,
	 mtu,
	 nexroutes);
  for (i=0; i<nexroutes; i++) {
    sprintf(erwhatbuf, "route#%d:", i);
    printf("%-9s %08lx/%08lx == %s/%s\n",
	   erwhatbuf,
	   exroutes[i].prefix, exroutes[i].mask,
	   exroutes[i].prefixtxt, exroutes[i].masktxt);
  }
  if (ferror(stdout) || fclose(stdout)) sysfatal("flush stdout");
  exit(0);
}


static void setsigmask(const sigset_t *ss) {
  int r;
  
  r= sigprocmask(SIG_SETMASK, ss, 0);
  if (r) sysfatal("[un]block signals");
}  

static void setsignals(void (*handler)(int), struct sigaction *sa, int chldflags) {
  const int *signalp;
  int r, sig;
  
  sa->sa_handler= handler;
  sa->sa_flags= 0; 
  for (signalp=signals; (sig=*signalp); signalp++) {
    r= sigaction(sig, sa, 0);  if (r) sysfatal("uncatch signal");
  }
  sa->sa_flags= chldflags;
  r= sigaction(SIGCHLD, sa, 0);  if (r) sysfatal("uncatch children");
}

static void infork(void) {
  struct sigaction sa;

  memset(&pids,0,sizeof(pids));
  sigemptyset(&sa.sa_mask);
  setsignals(SIG_DFL,&sa,0);
  setsigmask(&emptyset);
  undoslattach= 0;
}

static pid_t makesubproc(void (*entry)(void)) {
  pid_t pid;

  pid= fork();  if (pid == (pid_t)-1) sysfatal("fork for subprocess");
  if (pid) return pid;

  infork();
  entry();
  abort();
}

static int task(void) {
  pid_t pid;

  pid= fork();
  if (pid == (pid_t)-1) sysfatal("fork for task");
  if (!pid) { infork(); return 1; }

  pids.byname.task= pid;
  while (pids.byname.task) sigsuspend(&emptyset);
  return 0;
}

static void mdup2(int fd1, int fd2, const char *what) {
  int r;

  for (;;) {
    r= dup2(fd1,fd2); if (r==fd2) return;
    if (r!=-1) fatal("dup2 in %s gave wrong answer %d instead of %d",what,r,fd2);
    if (errno != EINTR) sysfatal("dup2 failed in %s",what);
  }
}

static void sl_entry(void) {
  mdup2(slpipe[1],1,"slattach child");
  execlp("slattach", "slattach", "-v", "-L", "-p",proto, ptyname, (char*)0);
  sysfatal("cannot exec slattach");
}

static void cin_entry(void) {
  mdup2(ptmaster,1,"cat input child");
  execlp("cat", "cat", (char*)0);
  sysfatal("cannot exec cat input");
}

static void cout_entry(void) {
  mdup2(ptmaster,0,"cat output child");
  execlp("cat", "cat", (char*)0);
  sysfatal("cannot exec cat output");
}

static void sighandler(int signum) {
  pid_t pid;
  int estatus, status;
  const char *taskfail;

  estatus= 4;
  
  if (signum == SIGCHLD) {
    for (;;) {
      pid= waitpid(-1,&status,WNOHANG);
      if (!pid || pid == (pid_t)-1) return;

      if (pid == pids.byname.task) {
	pids.byname.task= 0;
	if (!status) return;
	taskfail= "task";
      } else if (pid == pids.byname.cin) {
	pids.byname.cin= 0;
	if (status) {
	  taskfail= "input cat";
	} else {
	  taskfail= 0;
	  estatus= 0;
	}
      } else if (pid == pids.byname.cout) {
	pids.byname.cout= 0;
	taskfail= "output cat";
      } else if (pid == pids.byname.sl) {
	pids.byname.sl= 0;
	taskfail= "slattach";
      } else {
	continue;
      }
      break;
    }
    if (taskfail) {
      if (WIFEXITED(status)) {
	fprintf(stderr,
		"userv-ipif service: %s unexpectedly exited with exit status %d\n",
		taskfail, WEXITSTATUS(status));
      } else if (WIFSIGNALED(status)) {
	fprintf(stderr,
		"userv-ipif service: %s unexpectedly killed by signal %s%s\n",
		taskfail, strsignal(WTERMSIG(status)),
		WCOREDUMP(status) ? " (core dumped)" : "");
      } else {
	fprintf(stderr, "userv-ipif service: %s unexpectedly terminated"
		" with unknown status code %d\n", taskfail, status);
      }
    }
  } else {
    fprintf(stderr,
	    "userv-ipif service: received signal %d, terminating\n",
	    signum);
  }

  terminate(estatus);
}

static void startup(void) {
  int r;
  struct sigaction sa;
  
  sigfillset(&fullset);
  sigemptyset(&emptyset);

  ptmaster= getpt();  if (ptmaster==-1) sysfatal("allocate pty master");
  r= grantpt(ptmaster);  if (r) sysfatal("grab/grant pty slave");
  ptyname= ptsname(ptmaster);  if (!ptyname) sysfatal("get pty slave name");
  r= chmod(ptyname,0600);  if (r) sysfatal("chmod pty slave");
  r= unlockpt(ptmaster);  if (r) sysfatal("unlock pty");

  sigfillset(&sa.sa_mask);
  setsignals(sighandler,&sa,SA_NOCLDSTOP);
  setsigmask(&fullset);
}

static void startslattach(void) {
  static char ifnbuf[200];

  FILE *piper;
  int r, l, k;

  r= pipe(slpipe);  if (r) sysfatal("create pipe");
  piper= fdopen(slpipe[0],"r");  if (!piper) sysfatal("fdopen pipe");

  undoslattach= 1;
  pids.byname.sl= makesubproc(sl_entry);

  close(slpipe[1]);
  setsigmask(&emptyset);
  if (!fgets(ifnbuf,sizeof(ifnbuf),piper)) {
    if (ferror(piper)) sysfatal("cannot read ifname from slattach");
    else fatal("cannot read ifname from slattach");
  }
  setsigmask(&fullset);
  l= strlen(ifnbuf);
  if (l<=0 || ifnbuf[l-1] != '\n') fatal("slattach gave strange output `%s'",ifnbuf);
  ifnbuf[l-1]= 0;
  for (k=l; k>0 && ifnbuf[k-1]!=' '; k--);
  ifname= ifnbuf+k;
}

static void netconfigure(void) {
  char mtutxt[100];
  int i;

  if (task()) {
    sprintf(mtutxt,"%lu",mtu);
  
    execlp("ifconfig", "ifconfig", ifname, localtxt,
	   "netmask","255.255.255.255", "-broadcast", "pointopoint",peertxt,
	   "mtu",mtutxt, "up", (char*)0);
    sysfatal("cannot exec ifconfig");
  }

  for (i=0; i<nexroutes; i++) {
    if (task()) {
      execlp("route","route", "add", "-net",exroutes[i].prefixtxt,
	     "netmask",exroutes[i].masktxt,
	     "gw",peertxt, "dev",ifname, (char*)0);
      sysfatal("cannot exec route (for route)");
    }
  }
}

static void copydata(void) __attribute__((noreturn));
static void copydata(void) {
  int r;
  
  pids.byname.cin= makesubproc(cin_entry);
  for (;;) {
    r= write(1, "\300", 1); if (r==1) break;
    assert(r==-1);  if (errno != EINTR) sysfatal("send initial delim to confirm");
  }
  pids.byname.cout= makesubproc(cout_entry);

  for (;;) sigsuspend(&emptyset);
}

int main(int argc, const char *const *argv) {
  parseargs(argc,argv);
  pconfig(configstr,0);
  checkpermit();
  if (!proto) dumpdebug();

  startup();
  startslattach();
  netconfigure();
  copydata();
}
