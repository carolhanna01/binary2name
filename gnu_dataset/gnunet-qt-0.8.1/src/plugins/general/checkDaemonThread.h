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
 * @file src/plugins/general/checkDaemonThread.h
 * @brief Thread to check whether gnunetd is running and what applications are
 *        loaded
 * @author Nils Durner
 */

#ifndef CHECKDAEMONTHREAD_H_
#define CHECKDAEMONTHREAD_H_

#include <QString>
#include <QThread>
#include <QList>

#include "gnunet_qt_common.h"

class GGNUnetAppDesc
{
public:
  GGNUnetAppDesc() {};
  GGNUnetAppDesc(const GGNUnetAppDesc &src);
  GGNUnetAppDesc &operator=(const GGNUnetAppDesc &src);
  virtual ~GGNUnetAppDesc(){};

  GString strApp, strDesc;
};

typedef QList<GGNUnetAppDesc> GGNUnetAppDescs;

class GCheckDaemonThread : public QThread
{
  Q_OBJECT
public:
  GCheckDaemonThread(struct GNUNET_GC_Configuration *config,
    struct GNUNET_GE_Context *errorContext, QObject *parent = NULL);
  ~GCheckDaemonThread();
  void run();
  void stop();

  int checkAppsIn;
  bool stopRequested;
protected:
  struct GNUNET_GC_Configuration *config;
  struct GNUNET_GE_Context *errorContext;
signals:
  void running(bool isRunning);
  void applications(GGNUnetAppDescs *apps);
};

#endif /*CHECKDAEMONTHREAD_H_*/

/* end of checkDaemonThread.h */
