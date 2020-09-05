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
 * @file src/plugins/about/general.cc
 * @brief gnunet-qt's general tab
 * @author Nils Durner
 */

#include <QPicture>

#include <QStringList>
#include "general.h"

GGeneralPlugin::GGeneralPlugin(struct GNUNET_GC_Configuration *config,
    struct GNUNET_GE_Context *errorContext, QObject *parent) : GPlugin()
{
  Q_UNUSED(parent)

  setupUi(this);

  startStopThread = new GStartStopThread(config, errorContext);
  checkDaemonThread = new GCheckDaemonThread(config, errorContext);

  QStringList headerList;
  headerList << tr("Application") << tr("Description");
  treeApps->setHeaderLabels(headerList);

  connect(pbStartStop, SIGNAL(clicked(bool)), SLOT(startStopDaemon()));
  connect(startStopThread, SIGNAL(finished(bool, QString)), this, SLOT(startStopDone(bool, QString)));
  connect(checkDaemonThread, SIGNAL(running(bool)), this, SLOT(running(bool)));
  connect(checkDaemonThread, SIGNAL(applications(GGNUnetAppDescs *)), this,
    SLOT(applications(GGNUnetAppDescs *)));
  connect(checkDaemonThread, SIGNAL(finished()), this, SLOT(checkDaemonDone()));

  pbStartStop->setEnabled(false);
  runs = 0;
  isRunning = pending = false;

  checkDaemonThread->start();
}

GGeneralPlugin::~GGeneralPlugin()
{
  delete startStopThread;
  delete checkDaemonThread;
}

void GGeneralPlugin::startStopDaemon()
{
  pbStartStop->setEnabled(false);
  startStopThread->start(!isRunning);
}

void GGeneralPlugin::updateUi()
{
  QPixmap *pic = new QPixmap();

  if (pending)
    return;

  if (isRunning)
  {
    pbStartStop->setText(tr("Stop process"));
    pbStartStop->setIcon(QIcon(":/pixmaps/stop.png"));
    lblProcessStatus->setText(tr("Server background process is running"));
    pic->load(":/pixmaps/connected.png");
  }
  else
  {
    pbStartStop->setText(tr("Start process"));
    pbStartStop->setIcon(QIcon(":/pixmaps/start.png"));
    lblProcessStatus->setText(tr("Server background process is not running"));
    pic->load(":/pixmaps/not-connected.png");
  }

  lblProcessIcon->setPixmap(*pic);
  pbStartStop->setEnabled(true);
}

void GGeneralPlugin::startStopDone(bool success, QString msg)
{
  QString strStatus;
  QString icon;

  if (success)
  {
    if (isRunning)
      strStatus = tr("Terminating server...");
    else
      strStatus = tr("Launching server...");

    icon = ":/pixmaps/clock.png";
    pbStartStop->setEnabled(false);

    pending = true;
  }
  else
  {
    if (isRunning)
      strStatus = tr("Terminating server failed");
    else
      strStatus = tr("Launching server failed: %1").arg(msg);

    icon = ":/pixmaps/error.png";
  }

  emit setStatusText(icon, strStatus);
}

void GGeneralPlugin::applications(GGNUnetAppDescs *apps)
{
  int count = apps->count();
  QTreeWidgetItem *item;

  treeApps->clear();
  while(count)
  {
    GGNUnetAppDesc desc = apps->takeFirst();
    item = new QTreeWidgetItem();

    item->setText(0, desc.strApp);
    item->setText(1, desc.strDesc);
    treeApps->addTopLevelItem(item);

    count--;
  }

  delete apps;
}

void GGeneralPlugin::checkDaemon()
{
  checkDaemonThread->start();
}

void GGeneralPlugin::checkDaemonDone()
{
  runs = 0;
}

void GGeneralPlugin::running(bool isRunning)
{
  if (this->isRunning != isRunning)
  {
    if (pending)
    {
      QString icon;

      pending = false;
      icon = ":/pixmaps/gnunet-logo-small.png";

      emit setStatusText(icon, isRunning ? tr("Server launched") :
        tr("Server terminated"));
    }

    this->isRunning = isRunning;
    updateUi();
  }
}

extern "C"
{

  GNUNETQT_API GPlugin *init_general(GPluginInitParams *params)
  {
    return new GGeneralPlugin(params->config, params->errorContext);
  }

  GNUNETQT_API void shutdown_general(GPlugin *plugin)
  {
    delete (GGeneralPlugin *) plugin;
  }

} // extern "C"

/* end of general.cc */
