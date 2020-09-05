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
 * @file src/plugins/stats/statsPlugin.h
 * @brief gnunet-qt's statistics tab
 * @author Nils Durner
 */

#ifndef STATSPLUGIN_H_
#define STATSPLUGIN_H_

#include <QObject>
#include "gnunet_qt_common.h"
#include "ui_stats.h"
#include "statsThread.h"

class GStatsPlugin : public GPlugin, public Ui::WndStats
{
  Q_OBJECT

public:
	GStatsPlugin(struct GNUNET_GC_Configuration *config, struct GNUNET_GE_Context *errorContext);
	virtual ~GStatsPlugin();
signals:
  int setStatusText(const QString &strIcon, const QString &strText);
  int setNetworkStatus(const QString &strIcon, const QString &strText);

protected slots:
  void processStat(QString strName, qulonglong value);

protected:
  struct GNUNET_GC_Configuration *config;
  struct GNUNET_GE_Context *errorContext;
  class GStatsThread *statsThread;
};

#endif /*STATSPLUGIN_H_*/

/* end of statsPlugin.h */
