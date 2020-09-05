/*
     This file is part of gnunet-qt.
     (C) 2006, 2007 Nils Durner (and other contributing authors)

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
 * @file src/core/wndMain.cc
 * @brief Main window of gnunet-qt
 * @author Nils Durner
 */

#include <QWhatsThis>
#include <QSizeGrip>

#include "wndMain.h"

class GStatusTextEvent : public QEvent
{
public:
  GStatusTextEvent(QEvent::Type t) : QEvent(t){};

  QString icon;
  QString text;
};

class GNetworkStatusEvent : public GStatusTextEvent
{
public:
  GNetworkStatusEvent(QEvent::Type t) : GStatusTextEvent(t){};
};

GMainWindow::GMainWindow(QWidget *parent) : QMainWindow(parent)
{
  QMenu *trayMenu;
  QAction *trayShow, *trayExit;

  setupUi(this);
  delete tabRoot;

  trayIcon = new QSystemTrayIcon(QIcon(":/pixmaps/gnunet-qt.png"), this);
  trayMenu = new QMenu(this);
  trayShow = trayMenu->addAction(tr("Show main window"));
  trayShow->setCheckable(true);
  trayShow->setChecked(true);
  trayMenu->setDefaultAction(trayShow);
  connect(trayShow, SIGNAL(triggered()), this, SLOT(showAndHide()));
  trayExit = trayMenu->addAction(QIcon(":/pixmaps/exit.png"), tr("Exit"));
  connect(trayIcon, SIGNAL(activated(QSystemTrayIcon::ActivationReason)), this,
          SLOT(trayActivated(QSystemTrayIcon::ActivationReason)));
  connect(trayExit, SIGNAL(triggered()), qApp, SLOT(quit()));
  trayIcon->setContextMenu(trayMenu);
  trayIcon->show();

  networkIcon->setToolTip(tr("Number of connected peers"));
  networkText->setToolTip(tr("Number of connected peers"));
}

void GMainWindow::loadMenuStruct(GMenuStruct &menuStruct)
{
  GMenuStruct::iterator itMenu;
  GMenuActionList::iterator itItem;

  for (itMenu = menuStruct.begin(); itMenu != menuStruct.end(); itMenu++)
  {
    QMenu *menu;

    menu = new QMenu(menu_Bar);
    menu->setObjectName(itMenu->name);
    menu->setTitle(itMenu->title);
    menu_Bar->addAction(menu->menuAction());

    for (itItem = itMenu->actions.begin(); itItem != itMenu->actions.end(); itItem++)
    {
      QAction *action;

      action = new QAction(menu);
      action->setObjectName(itItem->name);
      action->setIcon(itItem->icon);
      action->setText(itItem->text);
      action->setMenuRole(itItem->menuRole);
      menu->addAction(action);

      if (itItem->action)
        *itItem->action = action;

      connect(action, SIGNAL(triggered()), itItem->receiver,
          qPrintable(itItem->receiverSlot));
    }
  }

  menuStruct.clear();
}

void GMainWindow::contextHelp()
{
  if (action_Context_help->isChecked())
    QWhatsThis::enterWhatsThisMode();
  else
    QWhatsThis::leaveWhatsThisMode();
}

void GMainWindow::about()
{
  GAboutDlg dlg(this);

  dlg.exec();
}

void GMainWindow::showAndHide()
{
  QAction *trayShow = (QAction *)sender();
  this->setVisible(trayShow->isChecked());
}

void GMainWindow::trayActivated(QSystemTrayIcon::ActivationReason reason)
{
  if(reason == QSystemTrayIcon::Trigger)
  {
    QSystemTrayIcon *trayIcon = (QSystemTrayIcon *)sender();
    QMenu *trayMenu = trayIcon->contextMenu();
    QAction *trayShow = trayMenu->defaultAction();
    if(trayShow->isChecked())
      {
        this->setVisible(false);
        trayShow->setChecked(false);
      }
    else
      {
        this->setVisible(true);
        trayShow->setChecked(true);
      }
  }
}

void GMainWindow::addApplication(QWidget *wnd, const QIcon &icon, const QString &label)
{
  tabWidget->addTab(wnd, icon, label);
  connect(wnd, SIGNAL(setStatusText(const QString &, const QString &)),
    this, SLOT(setStatusText(const QString &, const QString &)));
  connect(wnd, SIGNAL(setNetworkStatus(const QString &, const QString &)),
    this, SLOT(setNetworkStatus(const QString &, const QString &)));
}

void GMainWindow::setVisible(bool visible)
{
  trayIcon->contextMenu()->defaultAction()->setChecked(visible);
  QMainWindow::setVisible(visible);
}

void GMainWindow::setStatusText(const QString &strIcon, const QString &strText)
{
  GStatusTextEvent *e;

  e = new GStatusTextEvent(QEvent::User);
  e->icon = strIcon;
  e->text = strText;

  qApp->postEvent(this, e);
}

void GMainWindow::setNetworkStatus(const QString &strIcon, const QString &strText)
{
  GNetworkStatusEvent *e;

  e = new GNetworkStatusEvent(QEvent::User);
  e->icon = strIcon;
  e->text = strText;

  qApp->postEvent(this, e);
}

bool GMainWindow::event(QEvent *event)
{
  GStatusTextEvent *e;

  e = dynamic_cast<class GNetworkStatusEvent *> (event);
  if (e)
  {
    QPixmap icon;

    icon.load(e->icon);
    networkIcon->setPixmap(icon);
    networkText->setText(e->text);
    return true;
  }
  else
  {
    e = dynamic_cast<class GStatusTextEvent *> (event);
    if (e)
    {
      QPixmap icon;

      icon.load(e->icon);
      statusIcon->setPixmap(icon);
      statusText->setText(e->text);
      return true;
    }
  }

  return QMainWindow::event(event);
}

/* end of wndMain.cc */
