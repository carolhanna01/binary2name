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
 * @file src/plugins/general/general.h
 * @brief gnunet-qt's general tab
 * @author Nils Durner
 */

#ifndef GENERAL_H_
#define GENERAL_H_

#include <QTimer>

#include "ui_general.h"
#include "gnunet_qt_common.h"
#include "startStopThread.h"
#include "checkDaemonThread.h"

class GGeneralPlugin : public GPlugin, public Ui::WndGeneral
{
  Q_OBJECT

public:
  GGeneralPlugin(struct GNUNET_GC_Configuration *config,
    struct GNUNET_GE_Context *errorContext, QObject *parent = NULL);
  virtual ~GGeneralPlugin();

signals:
  int setStatusText(const QString &strIcon, const QString &strText);
  int setNetworkStatus(const QString &strIcon, const QString &strText);


protected:
  void updateUi();

  GStartStopThread *startStopThread;
  GCheckDaemonThread *checkDaemonThread;
  int runs;
  bool isRunning, pending;
protected slots:
  void startStopDaemon();
  void startStopDone(bool success, QString msg);
  void applications(GGNUnetAppDescs *apps);
  void running(bool isRunning);
  void checkDaemon();
  void checkDaemonDone();
};

#endif /*GENERAL_H_*/

/* end of general.h */
