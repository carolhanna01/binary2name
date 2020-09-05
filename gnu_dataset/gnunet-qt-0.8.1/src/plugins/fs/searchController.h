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
 * @file src/plugins/fs/searchController.h
 * @brief Controller for *all* searches
 * @author Nils Durner
 */

#ifndef SEARCHCONTROLLER_H_
#define SEARCHCONTROLLER_H_

#include <QObject>
#include <QMap>
#include <gnunet_qt_common.h>
#include "fs.h"
#include "ecrsuri.h"
#include "fs-search.h"
#include "searchSummaryController.h"

typedef struct
{
  GItemModel *model;
  QWidget *searchWindow;
  const struct GNUNET_FSUI_SearchList *handle;
} GFSSearchInfo;

class GFSSearchController : public QObject
{
  Q_OBJECT
  
public:
	GFSSearchController(class GFSPlugin *fs);
	virtual ~GFSSearchController();
  
  GFSSearchInfo *started(struct GNUNET_FSUI_SearchList *list,
    const struct GNUNET_ECRS_URI *uri, unsigned int resultCount,
    const GNUNET_ECRS_FileInfo *results);
  void result(GFSSearchInfo *searchInfo, const GNUNET_ECRS_FileInfo *info);
  void result(GItemModel *model, const struct GNUNET_FSUI_SearchList *list,
    const GNUNET_ECRS_FileInfo *info);
  void stopped(GFSSearchInfo *info);
  void downloadCompleted(QPersistentModelIndex &idx, GString file);
  void state(GFSSearchInfo *info, GNUNET_FSUI_EventType event);
  void update(GFSSearchInfo *info, const GNUNET_ECRS_FileInfo *finfo,
      const struct GNUNET_ECRS_URI *searchURI, int avail, unsigned int cert,
      unsigned int relevance);

  bool isActive(GFSEcrsUri uri);
  
  static void addSearchResult(GItemModel *model, QModelIndex parent,
    const GNUNET_ECRS_FileInfo *info);
protected slots:
  void closed(GFSEcrsUri &uri);
  void download(GItemModel *model ,GFSEcrsUri &uri,
      GPersistentModelIndexList indexes, int anonymity, bool recurse);

protected:
  typedef QMap<class GFSEcrsUri, struct GNUNET_FSUI_SearchList *> GFSSearches;
  
  class GFSSearchSummaryController *searchSummaryCntrl;
  QAbstractItemModel *searchSummaryView;
  GFSSearches searches;

  class GFSPlugin *fs;
};

#endif /*SEARCHCONTROLLER_H_*/
