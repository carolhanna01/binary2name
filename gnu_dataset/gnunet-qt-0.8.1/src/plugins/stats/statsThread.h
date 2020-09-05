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
 * @file src/plugins/stats/statsThread.h
 * @brief thread to gather stats from gnunetd
 * @author Nils Durner
 */

#ifndef STATSTHREAD_H_
#define STATSTHREAD_H_

#include <QThread>

class GStatsThread : public QThread
{
  Q_OBJECT
public:
  GStatsThread(struct GNUNET_GC_Configuration *config,
    struct GNUNET_GE_Context *errorContext, QObject *parent = NULL);
  void run();
  void stop();
  
  bool processStat(const char *name, unsigned long long value);
  
protected:
  struct GNUNET_GC_Configuration *config;
  struct GNUNET_GE_Context *errorContext;
  bool stopSignalled;
signals:
  void stat(QString strName, qulonglong value);
};

#endif /*STATSTHREAD_H_*/
