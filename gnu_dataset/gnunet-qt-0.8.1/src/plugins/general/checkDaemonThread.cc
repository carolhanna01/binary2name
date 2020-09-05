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
 * @file src/plugins/general/checkDaemonThread.cc
 * @brief Thread to check whether gnunetd is running and what applications are
 *        loaded
 * @author Nils Durner
 */

#include <GNUnet/gnunet_util.h>
#include <GNUnet/gnunet_getoption_lib.h>
#include <QStringList>

#include "checkDaemonThread.h"


GGNUnetAppDesc &GGNUnetAppDesc::operator=(const GGNUnetAppDesc &src)
{
  strApp = src.strApp;
  strDesc = src.strDesc;

  return *this;
}

GGNUnetAppDesc::GGNUnetAppDesc(const GGNUnetAppDesc &src)
{
  strApp = src.strApp;
  strDesc = src.strDesc;
}

GCheckDaemonThread::GCheckDaemonThread(struct GNUNET_GC_Configuration *config,
  struct GNUNET_GE_Context *errorContext, QObject *parent) : QThread(parent)
{
  this->config = config;
  this->errorContext = errorContext;
  checkAppsIn = 1;
  stopRequested = false;
}

GCheckDaemonThread::~GCheckDaemonThread()
{
  stop();
  wait();
}

void GCheckDaemonThread::run()
{
  bool check;

  while (!stopRequested)
  {
    GNUNET_CronTime sleepEnd;

    check = (GNUNET_test_daemon_running(errorContext, config) == GNUNET_YES);
    if (check)
    {
      checkAppsIn--;
      if (checkAppsIn == 0)
      {
        GGNUnetAppDescs *descs = new GGNUnetAppDescs();
        GNUNET_ClientServerConnection *sock = GNUNET_client_connection_create(errorContext,
          config);

        if (sock)
        {
          char *apps = GNUNET_get_daemon_configuration_value(sock, "GNUNETD", "APPLICATIONS");

          if (apps)
          {
            QString strApps = GString::fromLocal8Bit(apps);
            QStringList appList = strApps.split(QRegExp("\\s+"));

            int count = appList.count();
            while(count)
            {
              GGNUnetAppDesc appDesc;

              appDesc.strApp = appList.takeFirst();
              char *app = appDesc.strApp.toCString();
              char *desc = GNUNET_get_daemon_configuration_value(sock, "ABOUT", app);
              appDesc.strDesc = GString::fromLocal8Bit(desc);
              descs->append(appDesc);

              GNUNET_free_non_null(desc);

              count--;
            }

            GNUNET_free(apps);
          }
          GNUNET_client_connection_destroy(sock);

          checkAppsIn = 20; // 5 minutes / 15 seconds = 20 runs
        }
        else
          checkAppsIn = 1;

        emit applications(descs);
      }
    }

    emit running(check);

    sleepEnd = GNUNET_get_time() + 5 * GNUNET_CRON_SECONDS;
    while (!stopRequested && sleepEnd > GNUNET_get_time())
      msleep(100);
  }
}

void GCheckDaemonThread::stop()
{
  stopRequested = true;
}

/* end of checkDaemonThread.cc */
