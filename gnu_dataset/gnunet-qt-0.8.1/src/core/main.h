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
 * @file src/common/main.h
 * @brief Main functions of gnunet-qt
 * @author Nils Durner
 */

#ifndef MAIN_H_
#define MAIN_H_

#include <QApplication>
#include "wndMain.h"
#include "gnunet_qt_common.h"

class GApplication: public QApplication
{
  Q_OBJECT

public:
  GApplication(int &argc, char **argv,
    struct GNUNET_GC_Configuration *cfg);
  void loadPlugins();
  void setupMenuStruct();
  void showWindow();
  void setErrorContext(struct GNUNET_GE_Context *ectx);
  GMainWindow *getWindow();

protected:
#if defined(Q_OS_WIN)
  virtual bool winEventFilter(MSG *msg, long *result);
#endif

  GMainWindow wnd;
  GPluginLoader loader;
  QString strCfgFile;

  GMenuStruct menuStruct;
  struct GNUNET_GE_Context *ectx;
  struct GNUNET_GC_Configuration *cfg;
};

#endif /*MAIN_H_*/

/* end of main.h */
