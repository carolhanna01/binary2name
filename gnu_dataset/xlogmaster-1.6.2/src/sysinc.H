#ifndef SYSINC_H
#define SYSINC_H
/*
  System includes for Xlogmaster
*/

#include <X11/Xlib.h>
#include <X11/Xresource.h>
#include <gtk/gtk.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

#if HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif /* HAVE_SYS_MMAN_H */
#include <regex.h>
#include <getopt.h>

#include <iostream>
#include <csignal>
#include <cstdio>
#include <cstdlib>
#include <fcntl.h>
#include <fstream>
#include <sstream>
#include <unistd.h>
#include <cstring>
#include <string>
#include <memory>
#include <ctime>
#include <cerrno>
#include <dirent.h>

#endif /* SYSINC_H */
