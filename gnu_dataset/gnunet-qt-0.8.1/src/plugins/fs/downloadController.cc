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
 * @file src/plugins/fs/downloadController.cc
 * @brief Controller for *all* downloads
 * @author Nils Durner
 */

#include <math.h>
#include <QFileInfo>
#include <QDir>
#include <QStandardItem>
#include "gnunet_qt_common.h"
#include "downloadController.h"


GFSDownloadController::GFSDownloadController(GFSPlugin *fs)
{
  QTreeView *view;

  this->fs = fs;
  view = fs->downloadView();

  downloadModel.setColumnCount(COL_COUNT);
  downloadModel.setHeaderData(COL_FILENAME, Qt::Horizontal, tr("Filename"),
    Qt::DisplayRole);
  downloadModel.setHeaderData(COL_SIZE, Qt::Horizontal, tr("Size"),
    Qt::DisplayRole);
  downloadModel.setHeaderData(COL_PROGRESS, Qt::Horizontal, tr("Progress"),
    Qt::DisplayRole);
  downloadModel.setHeaderData(COL_STATUS, Qt::Horizontal, tr("Status"),
    Qt::DisplayRole);
  downloadModel.setHeaderData(COL_ETA, Qt::Horizontal, tr("ETA"),
    Qt::DisplayRole);
  downloadModel.setHeaderData(COL_DST_PATH, Qt::Horizontal,
    tr("Destination path"), Qt::DisplayRole);

  view->setModel(downloadModel.abstractItemModel());
  view->setItemDelegate(&delegate);
  view->hideColumn(COL_ETA);
  view->hideColumn(COL_DST_PATH);
}

GFSDownloadController::~GFSDownloadController()
{
}

void GFSDownloadController::setProgress(QPersistentModelIndex *idx,
  unsigned long long completed, unsigned long long total, GNUNET_CronTime eta)
{
  QModelIndex item, parent;
  double progress;

  // compute progress for this entry
  progress = ((double) completed) / total * 100;
  if (isnan(progress))
    progress = 0;

  downloadModel.lock();
  parent = idx->parent();
  item = downloadModel.index(idx->row(), COL_PROGRESS, parent);
  downloadModel.setData(item, QVariant(progress), Qt::DisplayRole);
  item = downloadModel.index(idx->row(), COL_ETA, parent);
  downloadModel.setData(item, QVariant(eta), Qt::DisplayRole);

  // compute overall progress for parent items
  while(parent.isValid())
  {
    int children = 0;
    double overallProgress = 0;
    GNUNET_CronTime overallETA = 0;
    QModelIndex progressIdx;

    item = parent.child(children, COL_PROGRESS);
    while (item.isValid())
    {
      children++;
      overallProgress += downloadModel.data(item).toDouble();
      item = parent.child(children, COL_ETA);
      overallETA += downloadModel.data(item).toLongLong();
      item = parent.child(children, COL_PROGRESS);
    }

    progressIdx = downloadModel.index(parent.row(), COL_PROGRESS, parent.parent());
    downloadModel.setData(progressIdx, QVariant(overallProgress / children),
      Qt::DisplayRole);
    progressIdx = downloadModel.index(parent.row(), COL_ETA, parent.parent());
    downloadModel.setData(progressIdx, QVariant(overallETA), Qt::DisplayRole);

    parent = parent.parent();
  }

  downloadModel.unlock();
}

/**
 * @brief Initiates a download
 * @param searchIdx index of the search result item
 * @param parentSearch pointer to search
 * @param uri ECRS URI to download
 * @param meta meta data
 * @param gnPath path inside a GNUnet directory
 * @param name name of the file
 * @param destPath top-level destination path
 * @param anonymity desired receiver anonymity
 * @param recursive true for a recursive directory download
 */
void GFSDownloadController::start(QPersistentModelIndex &searchIdx,
  struct GNUNET_FSUI_SearchList *parentSearch, GFSEcrsUri &uri, GFSEcrsMetaData &meta,
  QString gnPath, QString name, QString destPath, int anonymity, bool recursive)
{
  GString path;
  QString gnDir;
  GDownloadInfo info;

  // FIXME: check if download is pending

  // cleanup filename
  if (gnPath.endsWith("/") || gnPath.endsWith("\\"))
    gnPath = gnPath.left(gnPath.length() - 1);

  path = gnPath.replace("//", "/");
  while (path != gnPath)
  {
    gnPath = path;
    path = gnPath.replace("//", "/");
  }

  path = gnPath.replace("\\\\", "\\");
  while (path != gnPath)
  {
    gnPath = path;
    path = gnPath.replace("\\\\", "\\");
  }

  path = gnPath.replace("..", ".");
  while (path != gnPath)
  {
    gnPath = path;
    path = gnPath.replace("..", ".");
  }

  // generate name if needed
  if (name == "")
  {
    name = uri.toString().left(16);
    info.suggestName = true;
  }
  else
    info.suggestName = false;
  GNUNETQT_ASSERT(name != "");

  if (destPath == "")
  {
    char *dir;

    GNUNET_GC_get_configuration_value_filename(fs->config(), "FS", "INCOMINGDIR",
      "$HOME/gnunet-downloads/", &dir);
    destPath = dir;
  }

  path = destPath + QDir::separator() + gnPath +  QDir::separator() + name;

  GNUNET_FSUI_download_start(fs->context(), anonymity, recursive, uri.uri(), meta.meta(),
    path.toCString(), parentSearch, NULL /* FIXME */);

  info.searchIdx = searchIdx;
  downloadList.insert(uri, info);
}

QPersistentModelIndex *GFSDownloadController::started(struct GNUNET_FSUI_DownloadList *handle,
  QPersistentModelIndex *parent, const GNUNET_ECRS_FileInfo *fi, QString name, unsigned long long total,
  unsigned long long completed)
{
  QModelIndex tmpIdx;
  QPersistentModelIndex *idx;
  QStandardItem *item, *parentItem;
  QString displayPath;
  unsigned long long size;

  displayPath = QFileInfo(name).fileName();
  if (displayPath == "")
    displayPath = QDir(name).dirName();

  downloadModel.lock();

  if (parent)
    parentItem = downloadModel.itemFromIndex(downloadModel.index(parent->row(),
      parent->column(), parent->parent()));
  else
    parentItem = downloadModel.invisibleRootItem();

  item = new QStandardItem(displayPath);
  item->setColumnCount(COL_COUNT);
  item->setData(QVariant::fromValue((void *) handle), Qt::UserRole);
  parentItem->appendRow(item);

  tmpIdx = downloadModel.index(item->index().row(), COL_DST_PATH);
  downloadModel.setData(tmpIdx, QVariant(name), Qt::DisplayRole);

  size = GNUNET_ECRS_uri_get_file_size(fi->uri);
  tmpIdx = downloadModel.index(item->index().row(), COL_SIZE);
  downloadModel.setData(tmpIdx, QVariant(GString::fromByteSize(size)),
    Qt::DisplayRole);

  idx = new QPersistentModelIndex(item->index());

  downloadModel.unlock();

  setProgress(idx, completed, total, (GNUNET_CronTime) -1);

  state(idx, (total != completed) ? GNUNET_FSUI_download_started : GNUNET_FSUI_download_completed);

  return idx;
}

void GFSDownloadController::progress(QPersistentModelIndex *idx, unsigned long long completed,
  unsigned long long total, GNUNET_CronTime eta)
{
  setProgress(idx, completed, total, eta);
  state(idx, GNUNET_FSUI_download_progress);
}

void GFSDownloadController::completed(QPersistentModelIndex *idx, GFSEcrsUri uri, QString file)
{
  GFSDownloadList::iterator it;

  it = downloadList.find(uri);
  if (it != downloadList.end() && it->searchIdx.isValid())
    fs->searchController()->downloadCompleted(it->searchIdx, file);

  if (it->suggestName)
  {
    QString name;
    QModelIndex fidx;

    downloadModel.lock();

    fidx = downloadModel.index(idx->row(), COL_DST_PATH, idx->parent());
    name = downloadModel.data(fidx).toString();
    name = GNUNET_ECRS_suggest_better_filename(NULL, qPrintable(name));
    if (name != "")
      downloadModel.setData(fidx, name);

    QFileInfo info(name);
    fidx = downloadModel.index(idx->row(), COL_FILENAME, idx->parent());
    downloadModel.setData(fidx, info.fileName());

    downloadModel.unlock();
}

  state(idx, GNUNET_FSUI_download_completed);
}

void GFSDownloadController::state(QPersistentModelIndex *idx, GNUNET_FSUI_EventType type)
{
  QModelIndex index;

  downloadModel.lock();

  index = downloadModel.index(idx->row(), COL_STATUS, idx->parent());

  downloadModel.setData(index, QVariant(fs->fsuiState(type)), Qt::DisplayRole);
  if (type == GNUNET_FSUI_download_stopped)
    downloadModel.removeRow(idx->row());
  else
    downloadModel.setData(index,
      QVariant(type == GNUNET_FSUI_download_completed || type == GNUNET_FSUI_download_aborted),
      Qt::UserRole);

  downloadModel.unlock();
}

void GFSDownloadController::clear()
{
  int row = 0;
  QModelIndex idx;

  downloadModel.lock();

  idx = downloadModel.index(0, COL_STATUS);

  while(idx.isValid())
  {
    if (downloadModel.data(idx, Qt::UserRole).toInt() == 1)
    {
      QStandardItem *item;

      item = downloadModel.item(idx.row());
      GNUNET_FSUI_download_stop((struct GNUNET_FSUI_DownloadList *)
        item->data(Qt::UserRole).value<void *>());
    }
    else
      idx = downloadModel.index(++row, COL_STATUS);
  }

  downloadModel.unlock();
}

void GFSDownloadController::cancel(struct GNUNET_FSUI_DownloadList *handle)
{
  GNUNET_FSUI_download_abort(handle);
  GNUNET_FSUI_download_stop(handle);
}

QAbstractItemModel *GFSDownloadController::model()
{
  return downloadModel.abstractItemModel();
}

/* end of downloadController.cc */
