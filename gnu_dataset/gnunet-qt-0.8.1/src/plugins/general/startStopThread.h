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
 * @file src/plugins/general/startStopThread.h
 * @brief Thread to start or stop gnunetd without blocking the UI
 * @author Nils Durner
 */

#ifndef STARTSTOPTHREAD_H_
#define STARTSTOPTHREAD_H_

#include <QThread>

class GStartStopThread : public QThread
{
  Q_OBJECT
public:
  GStartStopThread(struct GNUNET_GC_Configuration *config,
    struct GNUNET_GE_Context *errorContext, QObject *parent = NULL);
  ~GStartStopThread();
  void start(bool doStart);
  void run();
signals:
  void finished(bool success, QString msg);
protected:
  bool doStart;
  int numRun;
  struct GNUNET_GC_Configuration *config;
  struct GNUNET_GE_Context *errorContext;
};

#endif /*STARTSTOPTHREAD_H_*/

/* end of startStopThread.h */
