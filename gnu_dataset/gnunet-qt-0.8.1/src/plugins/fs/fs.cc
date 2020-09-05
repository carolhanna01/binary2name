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
 * @file src/plugins/fs/fs.cc
 * @brief gnunet-qt's FS plugin
 * @author Nils Durner
 */

#include <QEvent>
#include <QMenu>
#include <QLineEdit>
#include <QMessageBox>
#include <QFileDialog>
#include <QDir>
#include <QUrl>
#include <QDesktopServices>
#include <QClipboard>
#include <GNUnet/gnunet_fsui_lib.h>


#include "gnunet_qt_common.h"
#include "fs.h"
#include "fs-search.h"
#include "ecrsuri.h"
#include "fs-open-uri.h"
#include "searchController.h"
#include "downloadController.h"

static void *fsuiEventProcessor(void *cls, const GNUNET_FSUI_Event *event)
{
  void *ret;
  int i;

  GFSSearchController *searchCntrl = ((GFSPlugin *) cls)->searchController();
  GFSUploadController *uploadCntrl = ((GFSPlugin *) cls)->uploadController();
  GFSDownloadController *downloadCntrl = ((GFSPlugin *) cls)->downloadController();
  struct GNUNET_GE_Context *errorContext = ((GFSPlugin *) cls)->errorContext();

  switch (event->type)
  {
    case GNUNET_FSUI_search_started:
      return searchCntrl->started(event->data.SearchStarted.sc.pos,
        event->data.SearchStarted.searchURI, 0, NULL);
    case GNUNET_FSUI_search_result:
      searchCntrl->result((GFSSearchInfo *) event->data.SearchResult.sc.cctx,
        &event->data.SearchResult.fi);
      break;
    case GNUNET_FSUI_search_aborted:
    case GNUNET_FSUI_search_suspended:
      searchCntrl->state((GFSSearchInfo *) event->data.SearchResult.sc.cctx,
        event->type);
      break;
    case GNUNET_FSUI_search_resumed:
      ret = searchCntrl->started(event->data.SearchResumed.sc.pos,
        event->data.SearchResumed.searchURI,
        event->data.SearchResumed.fisSize,
        event->data.SearchResumed.fis);
      for (i=0; i < event->data.SearchResumed.fisSize; i++)
        searchCntrl->update((GFSSearchInfo *) ret,
                         &event->data.SearchResumed.fis[i],
                         event->data.SearchResumed.searchURI,
                         event->data.SearchResumed.availability_rank[i],
                         event->data.SearchResumed.availability_certainty[i],
                         event->data.SearchResumed.applicability_rank[i]);
      return ret;
    case GNUNET_FSUI_search_stopped:
      searchCntrl->stopped((GFSSearchInfo *) event->data.SearchStopped.sc.cctx);
      break;
    case GNUNET_FSUI_search_update:
      searchCntrl->update((GFSSearchInfo *) event->data.SearchUpdate.sc.cctx,
          &event->data.SearchUpdate.fi, event->data.SearchUpdate.searchURI,
          event->data.SearchUpdate.availability_rank,
          event->data.SearchUpdate.availability_certainty,
          event->data.SearchUpdate.applicability_rank);
      break;
    case GNUNET_FSUI_upload_started:
      return uploadCntrl->started((QPersistentModelIndex *) event->data.UploadStarted.uc.pcctx,
        event);
    case GNUNET_FSUI_upload_resumed:
      return uploadCntrl->resumed((QPersistentModelIndex *) event->data.UploadStarted.uc.pcctx,
        event);
    case GNUNET_FSUI_upload_progress:
      uploadCntrl->progress((QPersistentModelIndex *) event->data.UploadProgress.uc.cctx,
        event->data.UploadProgress.completed, event->data.UploadProgress.total);
      uploadCntrl->state((QPersistentModelIndex *) event->data.UploadProgress.uc.cctx,
          event->type);
      break;
    case GNUNET_FSUI_upload_completed:
      uploadCntrl->complete((QPersistentModelIndex *) event->data.UploadStopped.uc.cctx,
          event->data.UploadCompleted.uri);
      break;
    case GNUNET_FSUI_upload_error:
      GNUNET_GE_LOG(errorContext, (GNUNET_GE_KIND) (GNUNET_GE_ERROR | GNUNET_GE_USER | GNUNET_GE_IMMEDIATE),
        event->data.UploadError.message);
    case GNUNET_FSUI_upload_stopped:
    case GNUNET_FSUI_upload_aborted:
    case GNUNET_FSUI_upload_suspended:
      uploadCntrl->state((QPersistentModelIndex *) event->data.UploadStopped.uc.cctx,
        event->type);
      break;
    case GNUNET_FSUI_download_started:
      return downloadCntrl->started(event->data.DownloadStarted.dc.pos,
        (QPersistentModelIndex *) event->data.DownloadStarted.dc.pcctx,
        &event->data.DownloadStarted.fi,
        event->data.DownloadStarted.filename, event->data.DownloadStarted.total, 0);
    case GNUNET_FSUI_download_resumed:
      return downloadCntrl->started(event->data.DownloadResumed.dc.pos,
        (QPersistentModelIndex *) event->data.DownloadResumed.dc.pcctx,
        &event->data.DownloadResumed.fi,
        event->data.DownloadResumed.filename, event->data.DownloadResumed.total,
        event->data.DownloadResumed.completed);
    case GNUNET_FSUI_download_progress:
      downloadCntrl->progress((QPersistentModelIndex *) event->data.DownloadProgress.dc.cctx,
        event->data.DownloadProgress.completed, event->data.DownloadProgress.total,
        event->data.DownloadProgress.eta);
      break;
    case GNUNET_FSUI_download_completed:
      downloadCntrl->completed((QPersistentModelIndex *) event->data.DownloadCompleted.dc.cctx,
        event->data.DownloadCompleted.uri,
        event->data.DownloadCompleted.filename);
      break;
    case GNUNET_FSUI_download_error:
      GNUNET_GE_LOG(errorContext, (GNUNET_GE_KIND) (GNUNET_GE_ERROR | GNUNET_GE_USER | GNUNET_GE_IMMEDIATE),
        event->data.DownloadError.message);
    case GNUNET_FSUI_download_stopped:
    case GNUNET_FSUI_download_aborted:
    case GNUNET_FSUI_download_suspended:
      downloadCntrl->state((QPersistentModelIndex *) event->data.DownloadStopped.dc.cctx,
        event->type);
      break;
    default:
      GNUNET_GE_LOG(errorContext, (GNUNET_GE_KIND) (GNUNET_GE_ERROR | GNUNET_GE_USER | GNUNET_GE_IMMEDIATE),
        qPrintable(QObject::tr("Internal error: received unhandled event from FSUI.")));
  }

  return NULL;
}

GFSPlugin::GFSPlugin(GPluginInitParams *params) : GPlugin()
{
  setupUi(this);

  qRegisterMetaType<GFSEcrsUri>("GFSEcrsUri&");
  qRegisterMetaType<GFSEcrsUri>("GFSEcrsUri");
  qRegisterMetaType<GItemModel *>("GItemModel*");
  qRegisterMetaType<QModelIndex>("QModelIndex");
  qRegisterMetaType<Qt::Orientation>("Qt::Orientation");
  qRegisterMetaType<QModelIndexList>("QModelIndexList");
  qRegisterMetaType<GPersistentModelIndexList>("GPersistentModelIndexList");
  qRegisterMetaType<GRanking>("GRanking");

  cfg = params->config;
  ectx = params->errorContext;

  // Menu
  GMenuAction action;
  GMenuStruct::iterator itMenu;

  action.name = "gnunet-qt::fs::openURI";
  action.text = tr("Open &URI");
  action.receiver = this;
  action.receiverSlot = SLOT(openURI());
  action.action = &actionOpenURI;

  for (itMenu = params->menu->begin(); itMenu != params->menu->end(); itMenu++)
    if (itMenu->name == "gnunet-qt::core::file")
      itMenu->actions.push_front(action);

  // FS tab
  connect(pbSearch, SIGNAL(clicked(bool)), this, SLOT(searchClicked()));
  connect(cmbSearchFor->lineEdit(), SIGNAL(returnPressed()), this, SLOT(searchClicked()));
  searchItemDelegate = new GSearchItemDelegate();

  treeDownloads->addAction(actionOpen_download);
  treeUploads->addAction(actionCopy_URI);
  connect(actionOpen_download, SIGNAL(triggered()), this, SLOT(openDownloadClicked()));
  connect(actionCopy_URI, SIGNAL(triggered()), this, SLOT(copyUploadURIClicked()));

  tabResults->removeTab(0); // created by Qt Designer
  GFSSearch *view = new GFSSearch(this, tabResults);
  view->setEnabled(false);
  view->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
  tabResults->addTab(view, tr("Search results"));

  // FSUI
  // Search
  searchCntrl = new GFSSearchController(this);

  // Upload
  uploadCntrl = new GFSUploadController(this);
  connect(pbChoose, SIGNAL(clicked(bool)), this, SLOT(chooseClicked()));
  connect(pbUpload, SIGNAL(clicked(bool)), this, SLOT(uploadClicked()));

  // Download
  downloadCntrl = new GFSDownloadController(this);

  // Summary
  connect(pbClearDL, SIGNAL(clicked(bool)), this, SLOT(clearDLClicked()));
  connect(pbCancelDL, SIGNAL(clicked(bool)), this, SLOT(cancelDLClicked()));
  connect(pbClearUL, SIGNAL(clicked(bool)), this, SLOT(clearULClicked()));

  treeSearches->setColumnWidth(0, 115);
  treeSearches->setColumnWidth(1, 70);
  treeSearches->setColumnWidth(2, 70);
  treeUploads->setColumnWidth(0, 210);
  treeDownloads->setColumnWidth(0, 150);
  treeDownloads->setColumnWidth(1, 70);
  treeDownloads->setColumnWidth(3, 70);

  treeDownloads->setEditTriggers(QAbstractItemView::NoEditTriggers);
  treeSearches->setEditTriggers(QAbstractItemView::NoEditTriggers);
  treeUploads->setEditTriggers(QAbstractItemView::NoEditTriggers);

  /* FIXME: allow user to configure download parallelism */
  fsuiContext = GNUNET_FSUI_start(params->errorContext, params->config, "gnunet_qt", 128, GNUNET_YES,
    fsuiEventProcessor, this);
}

GFSPlugin::~GFSPlugin()
{
  GNUNET_FSUI_stop(fsuiContext);
  delete searchCntrl;
  delete searchItemDelegate;
  delete uploadCntrl;
  delete downloadCntrl;
}

void GFSPlugin::openURI()
{
  GFSOpenURIDialog *dialog;
  QPersistentModelIndex idx;
  QString uri;
  GFSEcrsUri ecrsUri;
  GFSEcrsMetaData md;

  dialog = new GFSOpenURIDialog(this);
  if (dialog->exec() == QDialog::Accepted)
  {
    QString strIcon;

    uri = dialog->uriInput->toPlainText();
    ecrsUri = uri;
    downloadCntrl->start(idx, NULL, ecrsUri,
        md, "", dialog->file->text(), "", dialog->anon->value(),
        dialog->recursive->isChecked());

    strIcon = ":/pixmaps/download.png";
    emit setStatusText(strIcon, tr("Download of \"%0\" queued.").arg(ecrsUri.toString().left(16)));
  }
}

void GFSPlugin::openDownloadClicked()
{
  QModelIndexList sel;
  QModelIndexList::iterator it;

  sel = treeDownloads->selectionModel()->selectedIndexes();
  for (it = sel.begin(); it != sel.end(); it++)
  {
    QModelIndex idx = *it;

    if (idx.column() == GFSDownloadController::COL_DST_PATH)
    {
      QString name;

      name = downloadCntrl->model()->data(idx).toString();
      QFileInfo info(name);

      // Open file
      GDesktopServices::openDocument(qPrintable(info.absoluteFilePath()));
    }
  }
}

void GFSPlugin::copyUploadURIClicked()
{
  QModelIndexList sel;
  QModelIndexList::iterator it;
  QString strUris;

  sel = treeUploads->selectionModel()->selectedIndexes();
  for (it = sel.begin(); it != sel.end(); it++)
  {
    QModelIndex idx = *it;

    if (idx.column() == GFSUploadController::COL_URI)
    {
      GFSEcrsUri uri;

      uri = uploadCntrl->model()->data(idx).value<GFSEcrsUri>();
      if (strUris != "")
#if defined(Q_WS_WIN)
        strUris += "\r\n";
#else
        strUris += "\n";
#endif
      strUris += uri.serialized();
    }
  }

  QApplication::clipboard()->setText(strUris);
}

void GFSPlugin::searchClicked()
{
  QString strSearch, strNS;
  GNUNET_ECRS_URI *uri;

  strSearch = cmbSearchFor->lineEdit()->text();
  if (strSearch == "")
  {
    QMessageBox::critical(this, tr("Error"), tr("No keyword specified"),
      QMessageBox::Ok, QMessageBox::NoButton, QMessageBox::NoButton);
    return;
  }

  if (cmbSearchFor->findText(strSearch) == -1)
    cmbSearchFor->addItem(strSearch);

  strNS = cmbNS->lineEdit()->text();

  /* Create URI */
  if (strNS.length())
  {
    QString strUri;
    char *ustring;

    strUri = QString(GNUNET_ECRS_URI_PREFIX) + GNUNET_ECRS_SUBSPACE_INFIX + strNS + "/" +
      strSearch;
    ustring = strUri.toLocal8Bit().data();
    uri = GNUNET_ECRS_string_to_uri(ectx, ustring);
    if (!uri)
      GNUNET_GE_LOG(ectx, (GNUNET_GE_KIND) (GNUNET_GE_ERROR | GNUNET_GE_USER | GNUNET_GE_IMMEDIATE),
        tr("Failed to create namespace URI from `%s'.\n").toLocal8Bit().data(),
        ustring);
  }
  else
    uri = GNUNET_ECRS_keyword_string_to_uri(ectx, strSearch.toLocal8Bit().data());

  if (uri == NULL || searchCntrl->isActive(uri))
    return;

  /* Start search */
  GNUNET_FSUI_search_start(fsuiContext, spinSearchAnon->value(),
    uri);

  GNUNET_ECRS_uri_destroy(uri);
}

GFSSearchController *GFSPlugin::searchController()
{
  return searchCntrl;
}

GFSUploadController *GFSPlugin::uploadController()
{
  return uploadCntrl;
}

GFSDownloadController *GFSPlugin::downloadController()
{
  return downloadCntrl;
}

QTreeView *GFSPlugin::searchSummaryView()
{
  return treeSearches;
}

QTreeView *GFSPlugin::uploadView()
{
  return treeUploads;
}

QTreeView *GFSPlugin::downloadView()
{
  return treeDownloads;
}

bool GFSPlugin::event(QEvent *e)
{
  EventType type;

  GPlugin::event(e);

  GEvent *event = dynamic_cast<class GEvent *> (e);

  if (!event)
    return false;

  type = (EventType) e->type();

  if (type == NewSearch)
  {
    /* Setup new search window */
    GFSSearch *tab;
    int tabIdx;
    GFSNewSearchInfo *info = (GFSNewSearchInfo *) event->getParam();

    /* The first tab is always there. Use this one if it isn't used (disabled) yet. */
    tab = (GFSSearch *) tabResults->widget(0);
    if (tab->isEnabled())
    {
      /* first tab is already used. Create a new one. */
      tab = new GFSSearch(this, tabResults);
      tab->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
      tabResults->addTab(tab, info->uri.toDisplayString(cfg, ectx));
      tabIdx = tabResults->count() - 1;
    }
    else
    {
      tab->setEnabled(true);
      tabIdx = 0;
    }

    tab->setItemDelegate(searchItemDelegate);
    tabResults->setCurrentIndex(tabIdx);

    tab->setUri(info->uri);
    tab->setModel(info->model);
    event->setReturn(tab);

    return true;
  }
  else if (type == CloseSearch)
  {
    bool lastTab;
    GFSSearch *view = (GFSSearch *) event->getParam();

    if (tabResults->count() > 1)
    {
     tabResults->removeTab(tabResults->indexOf(view));
      view->deleteLater();
      lastTab = false;
    }
    else
    {
      /* always keep one search tab open */
      view->clear();
      view->setEnabled(false);
      tabResults->setTabText(0, tr("Search results"));
      lastTab = true;
    }

    event->setReturn((void *) lastTab); // wakeup posting thread
  }

  return false;
}

void GFSPlugin::chooseClicked()
{
  QString strFile;

  if (rbFile->isChecked())
    strFile = QFileDialog::getOpenFileName(this,
      tr("File to publish"), editName->text());
  else
    strFile = QFileDialog::getExistingDirectory(this,
      tr("Choose a directory to publish:"), editName->text()).replace("\"", "\\\"");

  editName->setText(QDir::convertSeparators(strFile));
}

void GFSPlugin::uploadClicked()
{
  QString strPath;
  QString strIcon;

  strPath = editName->text();
  if (strPath == "")
  {
    QMessageBox::critical(this, tr("Error"), tr("No filename specified"),
      QMessageBox::Ok, QMessageBox::NoButton, QMessageBox::NoButton);

    return;
  }

  if (uploadCntrl->start(this, strPath, rbIndex->isChecked(),
    spinPrio->value(), spinUploadAnon->value()))
  {
    strIcon = ":/pixmaps/upload.png";
    emit setStatusText(strIcon, tr("Publication of \"%0\" queued.").arg(strPath));
  }
}

void GFSPlugin::download(QPersistentModelIndex &searchIdx,
  struct GNUNET_FSUI_SearchList *handle, GFSEcrsUri &uri, GFSEcrsMetaData &meta,
  QString gnPath, QString &file, int anonymity, bool recurse)
{
  QString strIcon;

  downloadCntrl->start(searchIdx, handle, uri, meta, gnPath, file, "",
    anonymity, recurse);

  strIcon = ":/pixmaps/download.png";
  emit setStatusText(strIcon, tr("Download of \"%0\" queued.").arg(file));
}

void GFSPlugin::clearDLClicked()
{
  downloadCntrl->clear();
}

void GFSPlugin::cancelDLClicked()
{
  typedef QList<struct GNUNET_FSUI_DownloadList *> GCancelList;

  QModelIndexList list;
  QModelIndexList::iterator it;
  GCancelList cancelList;
  GCancelList::iterator itCancel;
  QItemSelectionModel *selModel = treeDownloads->selectionModel();
  QAbstractItemModel *dataModel = treeDownloads->model();

  list = selModel->selectedRows();

  // collect handles of downloads first because indexes shift as rows are removed
  for (it = list.begin(); it != list.end(); it++)
    cancelList.push_back((struct GNUNET_FSUI_DownloadList *) dataModel->data(*it, Qt::UserRole).value<void *>());

  // remove downloads
  for (itCancel = cancelList.begin(); itCancel != cancelList.end(); itCancel++)
    downloadCntrl->cancel(*itCancel);
}

void GFSPlugin::clearULClicked()
{
  uploadCntrl->clear();
}

struct GNUNET_GC_Configuration *GFSPlugin::config()
{
  return cfg;
}

struct GNUNET_GE_Context *GFSPlugin::errorContext()
{
  return ectx;
}

struct GNUNET_FSUI_Context *GFSPlugin::context()
{
  return fsuiContext;
}

QString GFSPlugin::fsuiState(GNUNET_FSUI_EventType type)
{
  switch(type)
  {
    case GNUNET_FSUI_search_started:
    case GNUNET_FSUI_download_started:
    case GNUNET_FSUI_upload_started:
    case GNUNET_FSUI_unindex_started:
      return tr("started");

    case GNUNET_FSUI_search_result:
    case GNUNET_FSUI_download_progress:
    case GNUNET_FSUI_upload_progress:
    case GNUNET_FSUI_unindex_progress:
      return tr("active");

    case GNUNET_FSUI_search_stopped:
    case GNUNET_FSUI_download_stopped:
    case GNUNET_FSUI_upload_stopped:
    case GNUNET_FSUI_unindex_stopped:
      return tr("stopped");

    case GNUNET_FSUI_download_completed:
    case GNUNET_FSUI_upload_completed:
    case GNUNET_FSUI_unindex_completed:
      return tr("completed");

    case GNUNET_FSUI_search_aborted:
    case GNUNET_FSUI_download_aborted:
    case GNUNET_FSUI_upload_aborted:
    case GNUNET_FSUI_unindex_aborted:
      return tr("aborted");

    case GNUNET_FSUI_download_error:
    case GNUNET_FSUI_upload_error:
    case GNUNET_FSUI_unindex_error:
      return tr("error");

    case GNUNET_FSUI_search_suspended:
    case GNUNET_FSUI_download_suspended:
    case GNUNET_FSUI_upload_suspended:
    case GNUNET_FSUI_unindex_suspended:
      return tr("suspended");

    case GNUNET_FSUI_search_resumed:
    case GNUNET_FSUI_download_resumed:
    case GNUNET_FSUI_upload_resumed:
    case GNUNET_FSUI_unindex_resumed:
      return tr("resumed");

    default:
      return QString();
  }
}

extern "C"
{
  GNUNETQT_API GPlugin *init_fs(GPluginInitParams *params)
  {
    return new GFSPlugin(params);
  }

  GNUNETQT_API void shutdown_fs(GPlugin *plugin)
  {
    delete plugin;
  }
} // extern "C"

/* end of fs.cc */
