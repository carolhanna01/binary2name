/*
     This file is part of gnunet-qt.
     (C) 2006 Nils Durner (and other contributing authors)

     gnunet-qt is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published
     by the Free Software Foundation; either version 2, or (at your
     option) any later version.

     gnunet-qt is distributed in the hope that it will be useful, but
     WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with GNUnet; see the file COPYING.  If not, write to the
     Free Software Foundation, Inc., 59 Temple Place - Suite 330,
     Boston, MA 02111-1307, USA.
*/

/**
 * @file src/plugins/general/startStopThread.cc
 * @brief Thread to start or stop gnunetd without blocking the UI
 * @author Nils Durner
 */

#include <errno.h>
#include <GNUnet/gnunet_util.h>
#include <GNUnet/gnunet_util_network_client.h>

#include "startStopThread.h"

#if defined(Q_OS_WIN)
  extern "C" char *_win_strerror(int errnum);
#endif

GStartStopThread::GStartStopThread(struct GNUNET_GC_Configuration *config,
    struct GNUNET_GE_Context *errorContext, QObject *parent) : QThread(parent)
{
  this->config = config;
  this->errorContext = errorContext;
}

GStartStopThread::~GStartStopThread()
{
  wait();
}

void GStartStopThread::start(bool doStart)
{
  this->doStart = doStart;
  QThread::start();
}

void GStartStopThread::run()
{
  bool ret;

  if (doStart)
    ret = GNUNET_daemon_start(errorContext, config, NULL, GNUNET_YES) != GNUNET_SYSERR;
  else
  {
    struct GNUNET_ClientServerConnection * sock;

    sock = GNUNET_client_connection_create(errorContext, config);
    ret = GNUNET_client_connection_request_daemon_shutdown(sock) != GNUNET_SYSERR;
  }

  emit finished(ret,
#if defined(Q_OS_WIN)
      _win_strerror(errno)
#else
      strerror(errno)
#endif
  );
}

/* end of startStopThread.cc */
