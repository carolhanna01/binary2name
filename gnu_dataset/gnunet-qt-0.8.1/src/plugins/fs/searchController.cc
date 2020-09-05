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
 * @file src/plugins/fs/searchController.cc
 * @brief Controller for *all* searches
 * @author Nils Durner
 */

#include <QDir>
#include <QFile>

#include "gnunet_qt_common.h"
#include "searchController.h"

GFSSearchController::GFSSearchController(class GFSPlugin *fs)
{
  this->fs = fs;
  searchSummaryCntrl = new GFSSearchSummaryController(fs);
}

GFSSearchController::~GFSSearchController()
{
  delete searchSummaryCntrl;
}

static int insertMetaData(EXTRACTOR_KeywordType type, const char *data,
  void *cls)
{
  GItemModel *model;
  QModelIndex *rowIndex, idx;
  QString content;

  rowIndex = (QModelIndex *) cls;
  model = (GItemModel *) rowIndex->model();
  model->lock();
  idx = model->index(rowIndex->row(), MODEL_IDX(type), rowIndex->parent());

  content = model->data(idx).toString();

  if (content != "")
    content += "\n";
  content += QString::fromUtf8(data);

  model->setData(idx, QVariant(content), Qt::DisplayRole);
  model->unlock();

  return GNUNET_OK;
}

GFSSearchInfo *GFSSearchController::started(struct GNUNET_FSUI_SearchList *list,
  const struct GNUNET_ECRS_URI *uri, unsigned int resultCount, const GNUNET_ECRS_FileInfo *results)
{
  GEvent *event;
  GFSSearch *view = NULL;
  GFSNewSearchInfo info;
  GFSSearchInfo *searchInfo;
  QSemaphore sem;


  info.model = new GItemModel;
  info.uri = uri;
  event = new GEvent((QEvent::Type) GFSPlugin::NewSearch, &info, (void **) &view, &sem);

  GEventDispatcher::postEvent(fs, event);
  sem.acquire(1); // wait until event is processed

  searches.insert(info.uri, list);

  connect(view, SIGNAL(closeSearchWnd(GFSEcrsUri &)), this, SLOT(closed(GFSEcrsUri &)));
  connect(view,
    SIGNAL(download(GItemModel *, GFSEcrsUri &, GPersistentModelIndexList, int, bool)),
    this,
    SLOT(download(GItemModel *, GFSEcrsUri &, GPersistentModelIndexList, int, bool)));

  searchSummaryCntrl->searchStarted(list, uri);

  // insert search results from last session
  while (resultCount > 0)
  {
    result(info.model, list, &results[resultCount - 1]);
    resultCount--;
  }

  searchInfo = new GFSSearchInfo;
  searchInfo->model = info.model;
  searchInfo->searchWindow = view;
  searchInfo->handle = list;

  return searchInfo;
}

void GFSSearchController::addSearchResult(GItemModel *model,
  QModelIndex parent, const GNUNET_ECRS_FileInfo *info)
{
  int row;
  GFSEcrsUri ecrsUri;
  GFSEcrsMetaData meta;
  size_t thumbSize;
  unsigned char *thumb;
  QStandardItem *child;
  QModelIndex childIdx, itemIdx;
  unsigned long long fileSize;

  child = new QStandardItem;
  child->setColumnCount(SEARCH_MAX_COLUMN + 1);
  model->lock();
  row = model->rowCount(parent);

  if (parent.isValid())
    model->itemFromIndex(parent)->appendRow(child);
  else
    model->appendRow(child);

  childIdx = model->index(row, 0, parent);

  // insert flat meta data
  GNUNET_meta_data_get_contents(info->meta, &insertMetaData, &childIdx);

  // insert thumbnail data
  thumbSize = GNUNET_meta_data_get_thumbnail(info->meta, &thumb);
  if (thumbSize)
  {
    QByteArray data((char *) thumb, thumbSize);

    itemIdx = model->index(row, MODEL_IDX(EXTRACTOR_THUMBNAIL_DATA), parent);
    model->setData(itemIdx, QVariant(data));
    GNUNET_free(thumb);
  }

  // display file size as reported by ECRS
  if (GNUNET_ECRS_uri_test_chk(info->uri) || GNUNET_ECRS_uri_test_loc(info->uri))
    fileSize = GNUNET_ECRS_uri_get_file_size(info->uri);
  else
    fileSize = 0;
  itemIdx = model->index(row, MODEL_IDX(EXTRACTOR_FILE_SIZE), parent);
  model->setData(itemIdx, QVariant(GString::fromByteSize(fileSize)));


  // make directories expandable
  itemIdx = model->index(row, MODEL_IDX(EXTRACTOR_MIMETYPE), parent);
  if (model->data(itemIdx).toString() == "application/gnunet-directory")
  {
    int row;
    QStandardItem *item;

    item = new QStandardItem;
    item->setColumnCount(SEARCH_MAX_COLUMN + 1);
    row = model->rowCount(childIdx);
    child->appendRow(item);

    itemIdx = model->index(0, 0, childIdx);
    model->setData(itemIdx, QVariant(RESULT_DUMMY), Qt::UserRole);
  }

  // insert serialized URI (used by download)
  ecrsUri = info->uri;
  itemIdx = model->index(row, SEARCH_URI_COLUMN, parent);
  model->setData(itemIdx, QVariant(ecrsUri.serialized()));

  // insert serialized meta data (used by download)
  meta = info->meta;
  itemIdx = model->index(row, SEARCH_META_COLUMN, parent);
  model->setData(itemIdx, QVariant(meta.serialized()));

  model->unlock();
}

/**
 * @brief Add a search result to the model
 * @param searchInfo information about the search
 * @param info file information
 * @notice called by FSUI
 */
void GFSSearchController::result(GFSSearchInfo *searchInfo,
  const GNUNET_ECRS_FileInfo *info)
{
  result(searchInfo->model, searchInfo->handle, info);
}

/**
 * @brief Adds a search result to the model
 * @param model the model
 * @param list FSUI search handle
 * @param info file information
 * @notice called by started() if there are results from the last session
 */
void GFSSearchController::result(GItemModel *model,
  const struct GNUNET_FSUI_SearchList *list, const GNUNET_ECRS_FileInfo *info)
{
  addSearchResult(model, QModelIndex(), info);

  searchSummaryCntrl->searchResult(list);
}

void GFSSearchController::stopped(GFSSearchInfo *info)
{
  GEvent *event;
  QSemaphore sem;
  void *lastTab;

  event = new GEvent((QEvent::Type) GFSPlugin::CloseSearch, info->searchWindow,
    (void **) &lastTab, &sem);
  GEventDispatcher::postEvent(fs, event);
  sem.acquire(1); // wait until event is processed

  searchSummaryCntrl->searchStopped(info->handle);
  info->searchWindow->disconnect(this);

  if (!lastTab)
    info->model->object()->deleteLater();

  delete info;
}

void GFSSearchController::state(GFSSearchInfo *info, GNUNET_FSUI_EventType event)
{
  searchSummaryCntrl->searchState(info->handle, event);
}

void GFSSearchController::update(GFSSearchInfo *info, const GNUNET_ECRS_FileInfo *finfo,
    const struct GNUNET_ECRS_URI *searchURI, int avail, unsigned int cert,
    unsigned int relevance)
{
  GItemModel *model;
  GFSEcrsUri ecrsUri;
  QModelIndexList lst;
  int row, availmax, availmin;
  GRanking rank;
  QVariant var;
  QModelIndex idx;

  model = info->model;
  ecrsUri = finfo->uri;

  model->lock();
  lst = model->match(model->index(0, SEARCH_URI_COLUMN), Qt::EditRole,
      ecrsUri.serialized(), 1, Qt::MatchExactly | Qt::MatchCaseSensitive);
  model->unlock();
  GNUNET_GE_BREAK_RETURN(fs->errorContext(), lst.count() > 0, );

  model->lock();
  row = lst.at(0).row();

  rank.availability_rank = avail;
  rank.availability_certainty = cert;
  rank.applicability_rank = relevance;
  rank.rank_sort = (int) avail + (int) (avail * (int) cert * 65536);
  rank.keywords = GNUNET_ECRS_uri_get_keyword_count_from_ksk(searchURI);

  var.setValue(rank);
  idx = model->index(row, SEARCH_RANK_COLUMN);
  model->setData(idx, var);

  availmax = (((double) avail + (GNUNET_FSUI_MAX_PROBES - cert)) / GNUNET_FSUI_MAX_PROBES) * 100;
  availmin = (double) avail / GNUNET_FSUI_MAX_PROBES * 100;
  if (availmin < 0)
    availmin = 0;

  model->setData(idx, tr("Relevance:") + " " +
      QString::number((double) relevance / rank.keywords * 100.0, 'f', 2) +
      "%\n" + tr("Availability:") + " " + QString::number(availmin) + "% " +
        ((availmax > 0 && availmax != availmin) ? " - " +
        QString::number(availmax) + "%" : ""), Qt::ToolTipRole);
  model->unlock();
}

bool GFSSearchController::isActive(GFSEcrsUri uri)
{
  return searches.contains(uri);
}

void GFSSearchController::closed(GFSEcrsUri &uri)
{
  GNUNET_FSUI_SearchList *list = searches[uri];

  if (list)
  {
    GNUNET_FSUI_search_abort(list);
    GNUNET_FSUI_search_stop(list);
  }

  searches.remove(uri);
}

void GFSSearchController::download(GItemModel *model, GFSEcrsUri &uri,
  GPersistentModelIndexList indexes, int anonymity, bool recurse)
{
  GPersistentModelIndexList::iterator it;

  for (it = indexes.begin(); it != indexes.end(); it++)
  {
    QModelIndex idx;
    GNUNET_FSUI_SearchList *handle;
    QPersistentModelIndex persistIdx(*it);
    QString gnPath;

    // get URI
    idx = model->index(it->row(), SEARCH_URI_COLUMN, it->parent());
    GFSEcrsUri fileUri(model->data(idx).toString());

    // get meta data
    idx = model->index(it->row(), SEARCH_META_COLUMN, it->parent());
    GFSEcrsMetaData meta(model->data(idx).toByteArray());

    // get filename
    idx = model->index(it->row(), MODEL_IDX(EXTRACTOR_FILENAME), it->parent());
    QString file = model->data(idx).toString();

    // get GNUnet path
    idx = idx.parent();
    while (idx.isValid())
    {
      gnPath = model->data(idx).toString() + gnPath;
      idx = idx.parent();
    }

    // start download
    handle = searches[uri];
    GNUNETQT_ASSERT(handle);

    fs->download(persistIdx, handle, fileUri, meta, gnPath, file, anonymity, recurse);
  }
}

/**
 * @brief Callback that adds content of a directory to the search list
 */
static int addFilesToDirectory(const GNUNET_ECRS_FileInfo *fi, const GNUNET_HashCode *key,
  int isRoot, void *closure)
{
  Q_UNUSED(key)

  QPersistentModelIndex *pIdx;
  GItemModel *model;
  QModelIndex parentIdx, child;

  if (isRoot == GNUNET_YES)
    return GNUNET_OK;

  GFSEcrsUri uri(fi->uri);

  pIdx = (QPersistentModelIndex *) closure;
  model = (GItemModel *) pIdx->model();
  if (!pIdx->isValid())
    return GNUNET_NO;

  parentIdx = model->index(pIdx->row(), 0, pIdx->parent());

  /* check for existing entry -- this function maybe called multiple
     times for the same directory entry */
  child = parentIdx.child(0, SEARCH_URI_COLUMN);
  while (child.isValid())
  {
    if (GFSEcrsUri(model->data(child).toString()) == uri)
      return GNUNET_OK;

    child = child.sibling(child.row() + 1, SEARCH_URI_COLUMN);
  }

  GFSSearchController::addSearchResult(model, parentIdx, fi);

  return GNUNET_OK;
}

void GFSSearchController::downloadCompleted(QPersistentModelIndex &idx, GString file)
{
  QModelIndex index;
  GItemModel *model;
  struct GNUNET_MetaData *meta;
  int idxRow;

  model = (GItemModel *) idx.model();
  model->lock();
  idxRow = idx.row();
  if (!idx.isValid())
  {
    model->unlock();
    return; // result got removed
  }

  index = model->index(idxRow, MODEL_IDX(EXTRACTOR_MIMETYPE), idx.parent());
  if (model->data(index).toString() == "application/gnunet-directory")
  {
    // mark directory as downloaded
    model->setData(idx, QVariant(RESULT_DOWNLOADED), Qt::UserRole);

    // remove dummy child
    index = model->index(idxRow, 0, idx.parent());
    model->removeRow(0, index);

    // read directory
    if (file.endsWith("/") || file.endsWith("\\"))
      file = file.left(file.length() - 1);
    file += ".gnd";

    QFile gnDir(file);
    gnDir.open(QIODevice::ReadOnly);
    QByteArray data = gnDir.readAll();

    if (data.size())
    {
      meta = NULL;
      GNUNET_ECRS_directory_list_contents(fs->errorContext(), data.data(), data.size(), NULL, &meta,
        &addFilesToDirectory, &idx);

      if (meta)
        GNUNET_meta_data_destroy(meta);
    }
  }

  model->unlock();
}

/* end of searchController.cc */
