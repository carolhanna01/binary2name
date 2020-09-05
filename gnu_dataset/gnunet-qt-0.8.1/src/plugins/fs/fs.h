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
 * @file src/plugins/fs/fs.h
 * @brief gnunet-qt's FS plugin
 * @author Nils Durner
 */

#ifndef FS_H_
#define FS_H_

#include "gnunet_qt_common.h"
#include "ecrsMetaData.h"
#include "ui_fs.h"
#include "fs-search.h"
#include "searchController.h"
#include "searchItemDelegate.h"
#include "searchSummaryController.h"
#include "uploadController.h"
#include "downloadController.h"

typedef struct
{
  GItemModel *model;
  GFSEcrsUri uri;
} GFSNewSearchInfo;

typedef enum {RESULT_UNKNOWN, RESULT_DUMMY, RESULT_DOWNLOADED} GResultType;

class GFSPlugin : public GPlugin, protected Ui::FSWnd
{
  Q_OBJECT

public:
  GFSPlugin(GPluginInitParams *params);
  ~GFSPlugin();

  typedef enum {NewSearch = QEvent::User, CloseSearch = QEvent::User + 1} EventType;

  class GFSSearchController *searchController();
  class GFSUploadController *uploadController();
  class GFSDownloadController *downloadController();
  QTreeView *uploadView();
  QTreeView *downloadView();
  QTreeView *searchSummaryView();

  struct GNUNET_GC_Configuration *config();
  struct GNUNET_GE_Context *errorContext();
  struct GNUNET_FSUI_Context *context();
  QString fsuiState(GNUNET_FSUI_EventType type);

  virtual bool event(QEvent *e);

  void download(QPersistentModelIndex &searchIdx, struct GNUNET_FSUI_SearchList *handle,
    GFSEcrsUri &uri, GFSEcrsMetaData &meta, QString gnPath, QString &file,
    int anonymity, bool recurse);

protected:
  QAction *actionOpenURI;
  class GFSSearchController *searchCntrl;
  class GFSUploadController *uploadCntrl;
  class GFSDownloadController *downloadCntrl;
  struct GNUNET_FSUI_Context *fsuiContext;
  struct GNUNET_GC_Configuration *cfg;
  struct GNUNET_GE_Context *ectx;
  class GSearchItemDelegate *searchItemDelegate;

public:
signals:
  int setStatusText(const QString &strIcon, const QString &strText);
  int setNetworkStatus(const QString &strIcon, const QString &strText);

protected slots:
  void openURI();
  void searchClicked();
  void chooseClicked();
  void uploadClicked();
  void clearDLClicked();
  void cancelDLClicked();
  void clearULClicked();
  void openDownloadClicked();
  void copyUploadURIClicked();
};

#endif /*FS_H_*/

/* end of fs.h */
