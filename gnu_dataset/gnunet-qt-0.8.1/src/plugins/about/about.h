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
 * @file src/plugins/about/about.h
 * @brief gnunet-qt's welcome screen
 * @author Nils Durner
 */


#ifndef ABOUT_H_
#define ABOUT_H_

#include "ui_about.h"
#include "gnunet_qt_common.h"

class GAboutPlugin : public GPlugin, public Ui::WndAbout
{
  Q_OBJECT

public:
  GAboutPlugin();

// unused
signals:
    int setStatusText(const QString &strIcon, const QString &strText);
    int setNetworkStatus(const QString &strIcon, const QString &strText);

protected:
  QString header();
  void welcome();
  void about();
  void notes();
protected slots:
  void linkHandler(const QUrl &link);
};

#endif /*ABOUT_H_*/

/* end of about.h */
