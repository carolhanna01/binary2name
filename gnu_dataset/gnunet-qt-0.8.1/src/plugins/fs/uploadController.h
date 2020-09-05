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
 * @file src/plugins/fs/uploadController.h
 * @brief Controller for *all* uploads
 * @author Nils Durner
 */

#ifndef UPLOADCONTROLLER_H_
#define UPLOADCONTROLLER_H_

#include <QPersistentModelIndex>

#include "gnunet_qt_common.h"
#include "fs.h"
#include "uploadDialog.h"
#include "uploadItemDelegate.h"

class GFSUploadController : public QObject
{
  Q_OBJECT

public:
  typedef enum {COL_FILENAME, COL_PROGRESS, COL_STATUS, COL_URI, COL_COUNT} GFSUploadControllerCols;

	GFSUploadController(GFSPlugin *fs);
	virtual ~GFSUploadController();

  bool start(QWidget *parent, const QString &strPath, bool index,
    int prio, int anon);
  QPersistentModelIndex *started(QPersistentModelIndex *parent,
    const GNUNET_FSUI_Event *event);
  QPersistentModelIndex *resumed(QPersistentModelIndex *parent,
      const GNUNET_FSUI_Event *event);
  void progress(QPersistentModelIndex *idx, unsigned long long completed,
    unsigned long long total);
  void complete(QPersistentModelIndex *idx, GFSEcrsUri uri);
  void state(QPersistentModelIndex *idx, GNUNET_FSUI_EventType type);
  void clear();

  QAbstractItemModel *model();
protected:
  void setProgress(QPersistentModelIndex *idx, unsigned long long completed,
    unsigned long long total);
  QPersistentModelIndex * newUpload(QPersistentModelIndex *parent, const char *fn,
      unsigned long long total, unsigned long long complete);

  GFSPlugin *fs;
  EXTRACTOR_ExtractorList *extractors;
  GItemModel uploadModel;
  GFSUploadItemDelegate delegate;
};

#endif /*UPLOADCONTROLLER_H_*/

/* end of uploadController.h */
