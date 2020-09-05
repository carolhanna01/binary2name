/*
     This file is part of gnunet-qt.
     (C) 2007 Nils Durner (and other contributing authors)

     gnunet-qt is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published
     by the Free Software Foundation; either version 2, or (at your
     option) any later version.

     gnunet-qt is distributed in the hope that it will be useful, but
     WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with gnunet-qt; see the file COPYING.  If not, write to the
     Free Software Foundation, Inc., 59 Temple Place - Suite 330,
     Boston, MA 02111-1307, USA.
*/

/**
 * @file src/plugins/stats/statsThread.cc
 * @brief thread to gather stats from gnunetd
 * @author Nils Durner
 */

#include <QTime>
#include <GNUnet/gnunet_util.h>
#include <GNUnet/gnunet_stats_lib.h>
#include "config.h"
#include "statsThread.h"

GStatsThread::GStatsThread(struct GNUNET_GC_Configuration *config,
              struct GNUNET_GE_Context *errorContext, QObject *parent)
{
  this->config = config;
  this->errorContext = errorContext;

  stopSignalled = false;
}

static int acquireStatistics(const char *name, unsigned long long value, void *param)
{
  return ((GStatsThread *) param)->processStat(name, value);
}

bool GStatsThread::processStat(const char *name, unsigned long long value)
{
   emit stat(QString::fromUtf8(name), value);

   return !stopSignalled;
}

void GStatsThread::run()
{
  int res;
  GNUNET_CronTime end;
  struct GNUNET_ClientServerConnection *sock;

  sock = GNUNET_client_connection_create(errorContext, config);
  if (!sock)
  {
    GNUNET_GE_LOG(errorContext, (GNUNET_GE_KIND) (GNUNET_GE_ERROR | GNUNET_GE_USER | GNUNET_GE_IMMEDIATE),
      qPrintable(tr("Error establishing connection with background process gnunetd.")));
    return;
  }

  while (! stopSignalled)
  {
    end = GNUNET_get_time() + 1 * GNUNET_CRON_SECONDS;
    res = GNUNET_STATS_get_statistics(errorContext,
            sock,
            &acquireStatistics,
            this);
    if (res != GNUNET_OK)
      GNUNET_GE_LOG(errorContext, (GNUNET_GE_KIND) (GNUNET_GE_ERROR | GNUNET_GE_USER | GNUNET_GE_IMMEDIATE),
        qPrintable(tr("Error reading information from background process gnunetd.")));

    while (GNUNET_get_time() < end && !stopSignalled)
      msleep(100);
  }

  GNUNET_client_connection_destroy(sock);
}

void GStatsThread::stop()
{
  stopSignalled = true;
}

