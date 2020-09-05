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
 * @file src/plugins/fs/searchSummaryModel.cc
 * @brief MVC model for the search results display in the status tab
 * @author Nils Durner
 */

#include "gnunet_qt_common.h"
#include "searchSummaryModel.h"


QVariant GFSSearchSummaryModel::headerData(int section, Qt::Orientation orientation,
    int role) const
{
  Q_UNUSED(orientation)
  
  if (role == Qt::DisplayRole)
  {
    if (section == 0)
      return QVariant(tr("Request"));
    if (section == 1)
      return QVariant(tr("Results"));
    if (section == 2)
      return QVariant(tr("Status"));
  }
  
  return QVariant();
}

GFSSearchSummaryModel::GFSSearchSummaryModel(struct GNUNET_GC_Configuration *cfg,
    struct GNUNET_GE_Context *ectx) : QAbstractItemModel()
{
  this->cfg = cfg;
  this->ectx = ectx;
}

QModelIndex GFSSearchSummaryModel::index(int row, int column,
  const QModelIndex &parent) const
{
  Q_UNUSED(parent)
  
  return createIndex(row, column);
}

QModelIndex GFSSearchSummaryModel::parent(const QModelIndex &index) const
{
  Q_UNUSED(index)
  
  return QModelIndex();
}

int GFSSearchSummaryModel::rowCount(const QModelIndex &parent) const
{
  if (parent.row() == -1 && parent.column() == -1)
    return results.count();
  else
    return 0;
}

int GFSSearchSummaryModel::columnCount(const QModelIndex &parent) const
{
  Q_UNUSED(parent)
  
  return 3;
}

QVariant GFSSearchSummaryModel::data(const QModelIndex &index, int role) const
{
  if (!index.isValid())
    return QVariant();

  GFSSearchEntry result = results[index.row()];
  if (role == Qt::DisplayRole)
  {
    switch(index.column())
    {
      case 0:
        return QVariant(result.uri.toDisplayString(cfg, ectx));
      case 1:
        return QVariant(result.count);
      case 2:
        return QVariant(result.status);
      default:
        return QVariant();
    }
  }
  
  return QVariant();
}

void GFSSearchSummaryModel::incSearch(const struct GNUNET_FSUI_SearchList *handle)
{
  GFSSearchList::iterator it = find(handle);
  
  if (it != results.end())
  {
    int idx;
    
    it->count++;
    idx = it - results.begin();
    dataChanged(index(idx, 1), index(idx, 1));
  }
}

void GFSSearchSummaryModel::setSearch(const struct GNUNET_FSUI_SearchList *handle,
  unsigned int count, const struct GNUNET_ECRS_URI *uri)
{
  int idx;
  GFSSearchList::iterator it = find(handle);
  
  idx = it - results.begin();
  if (it != results.end())
  {
    it->count = count;
    dataChanged(index(idx, 1), index(idx, 1));
  }
  else
  {
    /* Search not in model yet */
    GFSSearchEntry result;

    GNUNETQT_ASSERT(uri);

    beginInsertRows(QModelIndex(), idx, idx);
    result.uri = uri;
    result.count = count;
    result.handle = handle;

    /* Add to model */
    results.append(result);
    endInsertRows();
  }
}

void GFSSearchSummaryModel::removeSearch(const struct GNUNET_FSUI_SearchList *handle)
{
  GFSSearchList::iterator it = find(handle);
  
  if (it != results.end())
  {
    results.erase(it);
    reset(); // FIXME
  }
}

void GFSSearchSummaryModel::setStatus(const struct GNUNET_FSUI_SearchList *handle,
  QString status, bool done)
{
  GFSSearchList::iterator it = find(handle);
  
  if (it != results.end())
  {
    it->status = status;
    it->done = done;
  }
}

GFSSearchSummaryModel::GFSSearchList::iterator GFSSearchSummaryModel::find(const struct GNUNET_FSUI_SearchList *handle)
{
  GFSSearchList::iterator it;
  
  for (it = results.begin(); it != results.end(); it++)
  {
    if (it->handle == handle)
      break;
  }
  
  return it;
}

GFSSearchSummaryModel::GFSSearchEntry::GFSSearchEntry()
{
  handle = NULL;
  count = 0;
  done = false;
}

GFSSearchSummaryModel::GFSSearchEntry::GFSSearchEntry(const GFSSearchSummaryModel::GFSSearchEntry &src)
{
  handle = src.handle;
  count = src.count;
  uri = src.uri;
  done = src.done;
  status = src.status;
}

/* end of searchSummaryModel.cc */
