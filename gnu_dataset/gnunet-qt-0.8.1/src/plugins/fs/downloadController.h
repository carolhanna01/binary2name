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
 * @file src/plugins/fs/downloadController.h
 * @brief Controller for *all* downloads
 * @author Nils Durner
 */

#ifndef DOWNLOADCONTROLLER_H_
#define DOWNLOADCONTROLLER_H_

#include <QObject>
#include "gnunet_qt_common.h"
#include "fs.h"
#include "ecrsMetaData.h"
#include "downloadItemDelegate.h"


class GFSDownloadController : public QObject
{
  Q_OBJECT

public:
  typedef enum {COL_FILENAME, COL_SIZE, COL_PROGRESS, COL_STATUS, COL_ETA,
    COL_DST_PATH, COL_COUNT} GDownloadCols;

	GFSDownloadController(GFSPlugin *fs);
	virtual ~GFSDownloadController();

  /**
   * @brief Initiates a download
   * @param parentSearch pointer to search
   * @param uri ECRS URI to download
   * @param meta meta data
   * @param gnPath path inside a GNUnet directory
   * @param name name of the file
   * @param destPath top-level destination path
   * @param anonymity desired receiver anonymity
   * @param recursive true for a recursive directory download
   */
  void start(QPersistentModelIndex &searchIdx,
    struct GNUNET_FSUI_SearchList *parentSearch, GFSEcrsUri &uri,
    GFSEcrsMetaData &meta, QString gnPath, QString name,
    QString destPath, int anonymity, bool recursive);

  QPersistentModelIndex *started(struct GNUNET_FSUI_DownloadList *handle,
    QPersistentModelIndex *parent, const GNUNET_ECRS_FileInfo *fi, QString name,
    unsigned long long total, unsigned long long completed);

  void progress(QPersistentModelIndex *idx, unsigned long long completed,
    unsigned long long total, GNUNET_CronTime eta);

  void completed(QPersistentModelIndex *idx, GFSEcrsUri uri, QString file);

  void state(QPersistentModelIndex *idx, GNUNET_FSUI_EventType type);

  void cancel(struct GNUNET_FSUI_DownloadList *handle);

  void clear();

  QAbstractItemModel *model();
protected:
  typedef struct
  {
    QPersistentModelIndex searchIdx;
    bool suggestName;
  } GDownloadInfo;

  typedef QMap<GFSEcrsUri, GDownloadInfo> GFSDownloadList;

  GFSPlugin *fs;
  GItemModel downloadModel;
  GFSDownloadItemDelegate delegate;
  GFSDownloadList downloadList;

  void setProgress(QPersistentModelIndex *idx, unsigned long long completed,
    unsigned long long total, GNUNET_CronTime eta);
};

#endif /*DOWNLOADCONTROLLER_H_*/

/* end of downloadController.h */
