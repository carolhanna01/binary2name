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
 * @file src/plugins/fs/searchSummaryModel.h
 * @brief MVC model for the search results display in the status tab
 * @author Nils Durner
 */

#ifndef SEARCHESMODEL_H_
#define SEARCHESMODEL_H_

#include <QObject>
#include <QAbstractItemModel>
#include <QList>
#include <GNUnet/gnunet_fsui_lib.h>
#include "gnunet_qt_common.h"
#include "ecrsuri.h"

class GFSSearchSummaryModel : public QAbstractItemModel
{
  Q_OBJECT
  
public:
  GFSSearchSummaryModel(struct GNUNET_GC_Configuration *cfg,
      struct GNUNET_GE_Context *ectx);
  QVariant headerData(int section, Qt::Orientation orientation,
    int role = Qt::DisplayRole) const;
  QModelIndex index(int row, int column, const QModelIndex &parent =
    QModelIndex()) const;
  QModelIndex parent(const QModelIndex &index) const;
  int rowCount(const QModelIndex &parent = QModelIndex()) const;
  int columnCount(const QModelIndex &parent = QModelIndex()) const;
  QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;
  void setSearch(const struct GNUNET_FSUI_SearchList *handle, unsigned int count,
    const struct GNUNET_ECRS_URI *uri = NULL);
  void setStatus(const struct GNUNET_FSUI_SearchList *handle, QString status, bool done);
  void removeSearch(const struct GNUNET_FSUI_SearchList *handle);
  void incSearch(const struct GNUNET_FSUI_SearchList *handle);
protected:
  class GFSSearchEntry
  {
    public:
      GFSSearchEntry();
      GFSSearchEntry(const GFSSearchSummaryModel::GFSSearchEntry &src);

      const struct GNUNET_FSUI_SearchList *handle;
      int count;
      GFSEcrsUri uri;
      bool done;
      QString status;
  };

  typedef QList<GFSSearchEntry> GFSSearchList;

  GFSSearchList::iterator find(const struct GNUNET_FSUI_SearchList *handle); 

  GFSSearchList results;
  struct GNUNET_GC_Configuration *cfg;
  struct GNUNET_GE_Context *ectx;
};

#endif /*SEARCHESMODEL_H_*/

/* end of searchSummaryModel.h */
