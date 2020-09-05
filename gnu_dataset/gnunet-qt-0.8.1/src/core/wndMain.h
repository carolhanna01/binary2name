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
 * @file src/core/wndMain.h
 * @brief Main window of gnunet-qt
 * @author Nils Durner
 */

#ifndef WNDMAIN_H_
#define WNDMAIN_H_

#include <QWidget>
#include <QMainWindow>
#include <QLabel>
#include <QSystemTrayIcon>

#include "gnunet_qt_common.h"
#include "ui_wndMain.h"
#include "aboutDlg.h"

class GMainWindow: public QMainWindow, protected Ui::WndMain
{
  Q_OBJECT

public:
  GMainWindow(QWidget *parent = NULL);
  void addApplication(QWidget *wnd, const QIcon &icon, const QString &label);
  virtual void setVisible (bool visible);

public:
  QSystemTrayIcon *trayIcon;
  QAction *action_Context_help, *actionAbout, *actionExit;

  void loadMenuStruct(GMenuStruct &menuStruct);
protected:
  virtual bool event(QEvent *event);

public slots:
  void setStatusText(const QString &strIcon, const QString &strText);
  void setNetworkStatus(const QString &strIcon, const QString &strText);
protected slots:
  void contextHelp();
  void about();
  void showAndHide();
  void trayActivated(QSystemTrayIcon::ActivationReason reason);
};

#endif /*WNDMAIN_H_*/

/* end of wndMain.h */

