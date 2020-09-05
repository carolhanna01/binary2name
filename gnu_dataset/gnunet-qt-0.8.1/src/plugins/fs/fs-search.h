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
 * @file src/plugins/fs/fs-search.h
 * @brief Search result tab
 * @author Nils Durner
 */

#ifndef SEARCHRESULTS_H_
#define SEARCHRESULTS_H_

#include <QTabWidget>
#include <GNUnet/gnunet_fsui_lib.h>

#include "gnunet_qt_common.h"
#include "fs.h"
#include "ecrsuri.h"
#include "ui_fs-search-result.h"

class GFSSearch : public QWidget, protected Ui::ResultWnd
{
  Q_OBJECT

public:
  GFSSearch(class GFSPlugin *fs, QTabWidget *tab);
  void clear();
  void setModel(GItemModel *model);
  void setItemDelegate(QAbstractItemDelegate *itemDelegate);
  void setUri(GFSEcrsUri &uri);
  GItemModel *model();

signals:
  void closeSearchWnd(GFSEcrsUri &uri);
  void download(GItemModel *model, GFSEcrsUri &uri,
      GPersistentModelIndexList indexes, int anonymity, bool recursive);

protected:
  class GFSPlugin *fs;
  GFSEcrsUri uri;
  QTabWidget *tab;
  GItemModel *m;

  void setColHidden(int col, bool hidden);
  void setupColumns();
  void addColumn(QMenu *menu, QString strTitle, int nr);
protected slots:
  void closeClicked();
  void downloadClicked();
  void copyURI();
  void headerRightClicked(const QPoint &pos);
  void resultInserted();
  void resultExpanded(const QModelIndex &index);
};

#endif /*SEARCHRESULTS_H_*/

/* end of fs-search.h */
