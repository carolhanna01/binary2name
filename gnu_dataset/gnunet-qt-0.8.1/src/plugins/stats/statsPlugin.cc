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
 * @file src/plugins/stats/statsPlugin.cc
 * @brief gnunet-qt's statistics tab
 * @author Nils Durner
 */
#include "config.h"
#include "statsPlugin.h"

#include <QTreeWidgetItem>

#include <GNUnet/gnunet_directories.h>
#include <GNUnet/gnunet_util.h>
#include <GNUnet/gnunet_util_boot.h>
#include <GNUnet/gnunet_stats_lib.h>

struct GStatsParam
{
  QTreeWidget *tree;
  GStatsPlugin *plugin;
};

GStatsPlugin::GStatsPlugin(struct GNUNET_GC_Configuration *config,
  struct GNUNET_GE_Context *errorContext) : GPlugin()
{
  setupUi(this);

  this->config = config;
  this->errorContext = errorContext;
  statsThread = new GStatsThread(config, errorContext);

  connect(statsThread, SIGNAL(stat(QString, qulonglong)), this,
    SLOT(processStat(QString, qulonglong)), Qt::QueuedConnection);

  treeStats->setColumnWidth(0, 550);
  statsThread->start();
}

GStatsPlugin::~GStatsPlugin()
{
  statsThread->stop();
  if (!statsThread->wait(1000))
    statsThread->terminate();

  delete statsThread;
}

void GStatsPlugin::processStat(QString strName, qulonglong value)
{
    int count;
    bool found;

    found = false;

    /* populate statistics tree */
    count = treeStats->topLevelItemCount();
    while (count > 0)
    {
      QTreeWidgetItem *item;

      item = treeStats->topLevelItem(count - 1);

      if (item->text(0) == strName)
      {
        item->setText(1, QString::number(value));
        found = true;
      }
      count--;
    }

    if (!found)
    {
      QTreeWidgetItem *item = new QTreeWidgetItem(0);

      item->setText(0, strName);
      item->setText(1, QString::number(value));

      treeStats->addTopLevelItem(item);
    }

    /* display # of connected peers in status bar */
    if (strName == "# of connected peers")
    {
      QString strIcon;

      if (value == 0)
        strIcon = ":/pixmaps/network-status-offline.png";
      else
        strIcon = ":/pixmaps/network-status-online.png";

      emit setNetworkStatus(strIcon, QString::number(value));
    }
}


extern "C"
{

  GNUNETQT_API GPlugin *init_stats(GPluginInitParams *params)
  {
    return new GStatsPlugin(params->config, params->errorContext);
  }

  GNUNETQT_API void shutdown_stats(GPlugin *plugin)
  {
    delete (GStatsPlugin *) plugin;
  }

} // extern "C"

/* enf of statsPlugin.cc */
