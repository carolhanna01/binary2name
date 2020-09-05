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
 * @file src/plugins/fs/uploadController.cc
 * @brief Controller for *all* uploads
 * @author Nils Durner
 */

#include <math.h>
#include <QDir>
#include <QFileInfo>
#include <QStringList>
#include <QHeaderView>
#include <extractor.h>
#include <GNUnet/gnunet_ecrs_lib.h>
#include "uploadController.h"

static int getKeyword(const char *data, int,
		      void *list)
{
  ((QStringList *) list)->append(QString::fromUtf8(data));

  return GNUNET_OK;
}

static int meta_data_get_by_type(EXTRACTOR_KeywordType type, const char *data, void *list)
{
  ((GFSMetaData *) list)->insert(type, data);

  return GNUNET_OK;
}

GFSUploadController::GFSUploadController(GFSPlugin *fs)
{
  this->fs = fs;
  extractors = NULL;

  uploadModel.setColumnCount(COL_COUNT);
  uploadModel.setHeaderData(COL_FILENAME, Qt::Horizontal, tr("Filename"), Qt::DisplayRole);
  uploadModel.setHeaderData(COL_PROGRESS, Qt::Horizontal, tr("Progress"), Qt::DisplayRole);
  uploadModel.setHeaderData(COL_STATUS, Qt::Horizontal, tr("Status"), Qt::DisplayRole);
  uploadModel.setHeaderData(COL_URI, Qt::Horizontal, tr("URI"), Qt::DisplayRole);

  fs->uploadView()->setModel(uploadModel.abstractItemModel());
  fs->uploadView()->setItemDelegate(&delegate);
  fs->uploadView()->hideColumn(COL_URI);
}

GFSUploadController::~GFSUploadController()
{
  if (extractors)
    EXTRACTOR_removeAll(extractors);
}

void GFSUploadController::setProgress(QPersistentModelIndex *idx,
  unsigned long long completed, unsigned long long total)
{
  QModelIndex item, parent;
  double progress;

  // compute progress for this entry
  progress = ((double) completed) / total * 100;
  if (isnan(progress))
    progress = 0;

  uploadModel.lock();
  parent = idx->parent();
  if (idx->isValid())
  {
    item = uploadModel.index(idx->row(), COL_PROGRESS, parent);
    uploadModel.setData(item, QVariant(progress), Qt::DisplayRole);
  }
  uploadModel.unlock();
}

bool GFSUploadController::start(QWidget *parent, const QString &strPath,
  bool index, int prio, int anon)
{
  GFSMetaData metaData;
  QStringList keywords;
  struct GNUNET_MetaData *meta;
  QFileInfo fileInfo;
  struct GNUNET_ECRS_URI *keywordUri;
  int thumbSize;
  unsigned char *thumb;

  parent->setCursor(Qt::WaitCursor);

  // load libextractor
  if (! extractors)
  {
    char *config;

    extractors = EXTRACTOR_loadDefaultLibraries();
    if (GNUNET_GC_get_configuration_value_string(fs->config(), "FS", "EXTRACTORS",
      NULL, &config) == 0 && config)
    {
      extractors = EXTRACTOR_loadConfigLibraries(extractors, config);
      GNUNET_free(config);
    }
  }

  // extract meta data
  meta = GNUNET_meta_data_create();
  if (!meta)
  {
      GNUNET_GE_LOG(fs->errorContext(), (GNUNET_GE_KIND) (GNUNET_GE_ERROR | GNUNET_GE_USER | GNUNET_GE_IMMEDIATE),
        qPrintable(tr("Internal error: failed to create meta data for publication.")));
      return false;
  }

  GNUNET_meta_data_extract_from_file(fs->errorContext(), meta, strPath.toLocal8Bit().data(), extractors);
  GNUNET_meta_data_get_contents(meta, &meta_data_get_by_type, &metaData);
  thumbSize = GNUNET_meta_data_get_thumbnail(meta, &thumb);
  if (thumbSize)
    metaData.insert(EXTRACTOR_THUMBNAIL_DATA, QByteArray((const char *) thumb,
      thumbSize));

  // Keywords
  keywordUri = GNUNET_meta_data_to_uri(meta);
  GNUNET_meta_data_destroy(meta);
  if (keywordUri)
  {
    GNUNET_ECRS_uri_get_keywords_from_ksk(keywordUri, getKeyword, &keywords);
    GNUNET_ECRS_uri_destroy(keywordUri);
  }

  fileInfo.setFile(strPath);
  if (fileInfo.isDir())
  {
    QDir dir(strPath);
    QString name = dir.dirName();

    if (!keywords.contains(name))
      keywords.append(name);
  }

  parent->setCursor(Qt::ArrowCursor);

  GFSUploadDialog uploadDlg(&metaData, &keywords, fs);

  if (uploadDlg.exec() == QDialog::Accepted)
  {
    GFSMetaData::iterator it;
    struct GNUNET_ECRS_URI *globalURI, *keywordURI;
    char **keys;
    long idx;

    // prepare meta data
    meta = GNUNET_meta_data_create();
    for(it = metaData.begin(); it != metaData.end(); it++)
      GNUNET_meta_data_insert(meta, it.key(), it->data());

    // prepare global URI
    globalURI = GNUNET_ECRS_string_to_uri(fs->errorContext(), GNUNET_ECRS_URI_PREFIX
      GNUNET_ECRS_SEARCH_INFIX);

    // prepare keywords URI
    idx = keywords.count();
    keys = new char *[idx + 1];
    keys[idx] = NULL;
    idx--;
    while (idx >= 0)
    {
      keys[idx] = strdup(keywords[idx].toUtf8());

      idx--;
    }

    keywordURI = GNUNET_ECRS_keyword_command_line_to_uri(NULL, keywords.count(),
							 (const char **) keys);

    // upload
    GNUNET_FSUI_upload_start(fs->context(), strPath.toLocal8Bit(),
      (GNUNET_FSUI_DirectoryScanCallback) &GNUNET_disk_directory_scan, fs->errorContext(),
      anon, prio, index, uploadDlg.extract(), uploadDlg.useKeywords(),
      GNUNET_get_time() + 2 * GNUNET_CRON_YEARS, meta, globalURI, keywordURI);

    // cleanup
    idx = 0;
    while(keys[idx])
    {
      free(keys[idx]);
      idx++;
    }
    delete [] keys;

    GNUNET_meta_data_destroy(meta);
    if (globalURI)
      GNUNET_ECRS_uri_destroy(globalURI);
    if (keywordURI)
      GNUNET_ECRS_uri_destroy(keywordURI);

    return true;
  }

  return false;
}

void GFSUploadController::complete(QPersistentModelIndex *idx, GFSEcrsUri uri)
{
  QModelIndex index;

  uploadModel.lock();
  index = uploadModel.index(idx->row(), COL_URI, idx->parent());
  uploadModel.setData(index, QVariant::fromValue(uri));
  uploadModel.unlock();
}

void GFSUploadController::state(QPersistentModelIndex *idx, GNUNET_FSUI_EventType type)
{
  QModelIndex index;

  uploadModel.lock();
  index = uploadModel.index(idx->row(), COL_STATUS, idx->parent());

  uploadModel.setData(index, QVariant(fs->fsuiState(type)), Qt::DisplayRole);
  uploadModel.setData(index, QVariant(type == GNUNET_FSUI_upload_completed),
    Qt::UserRole);

  uploadModel.unlock();
}

QAbstractItemModel *GFSUploadController::model()
{
  return uploadModel.abstractItemModel();
}

QPersistentModelIndex *GFSUploadController::newUpload(QPersistentModelIndex *parent, const char *fn,
    unsigned long long total, unsigned long long complete)
{
  QPersistentModelIndex *idx;
  QStandardItem *item, *parentItem;
  QString displayPath;

  uploadModel.lock();
  displayPath = QFileInfo(fn).fileName();
  if (displayPath == "")
    displayPath = QDir(fn).dirName();

  if (parent)
    parentItem = uploadModel.itemFromIndex(uploadModel.index(parent->row(), parent->column(), parent->parent()));
  else
    parentItem = uploadModel.invisibleRootItem();

  item = new QStandardItem(displayPath);
  item->setColumnCount(COL_COUNT);
  parentItem->appendRow(item);

  idx = new QPersistentModelIndex(item->index());
  setProgress(idx, complete, total);
  uploadModel.unlock();

  return idx;
}

QPersistentModelIndex *GFSUploadController::resumed(QPersistentModelIndex *parent,
    const GNUNET_FSUI_Event *event)
{
  QPersistentModelIndex *idx;

  idx = newUpload(parent, event->data.UploadResumed.filename, event->data.UploadResumed.total,
      event->data.UploadResumed.completed);

  if (event->data.UploadResumed.total == event->data.UploadResumed.completed)
  {
    state(idx, GNUNET_FSUI_upload_completed);
    complete(idx, event->data.UploadResumed.uri);
  }
  else
    state(idx, event->type);

  return idx;
}

QPersistentModelIndex *GFSUploadController::started(QPersistentModelIndex *parent,
    const GNUNET_FSUI_Event *event)
{
  QPersistentModelIndex *idx;

  idx = newUpload(parent, event->data.UploadStarted.filename, event->data.UploadStarted.total, 0);

  state(idx, event->type);

  return idx;
}

void GFSUploadController::progress(QPersistentModelIndex *idx,
  unsigned long long completed, unsigned long long total)
{
  setProgress(idx, completed, total);
}

void GFSUploadController::clear()
{
  int row = 0;
  QModelIndex idx;

  uploadModel.lock();
  idx = uploadModel.index(0, COL_STATUS);

  while(idx.isValid())
  {
    if (uploadModel.data(idx, Qt::UserRole).toInt() == 1)
      uploadModel.removeRow(row);
    else
      idx = uploadModel.index(++row, COL_STATUS);
  }
  uploadModel.unlock();
}

/* end of uploadController.cc */
