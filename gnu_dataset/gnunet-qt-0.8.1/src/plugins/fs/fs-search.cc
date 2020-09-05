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
 * @file src/plugins/fs/fs-search.cc
 * @brief Search result tab
 * @author Nils Durner
 */

#include <QHeaderView>
#include <QMenu>
#include <QClipboard>
#include <extractor.h>

#include "gnunet_qt_common.h"
#include "fshelper.h"
#include "fs-search.h"

GFSSearch::GFSSearch(class GFSPlugin *fs, QTabWidget *tab) : QWidget()
{
  QHeaderView *header;

  setupUi(this);

  header = treeResults->header();
  header->setContextMenuPolicy(Qt::CustomContextMenu);
  treeResults->setUniformRowHeights(false);
  treeResults->addAction(actionCopy_URI);
  treeResults->addAction(actionDownload);
  treeResults->setEditTriggers(QAbstractItemView::NoEditTriggers);
  this->tab = tab;
  this->fs = fs;

  connect(pbClose, SIGNAL(clicked(bool)), this, SLOT(closeClicked()));
  connect(pbDownload, SIGNAL(clicked(bool)), this, SLOT(downloadClicked()));
  connect(actionCopy_URI, SIGNAL(triggered()), this, SLOT(copyURI()));
  connect(actionDownload, SIGNAL(triggered()), this, SLOT(downloadClicked()));
  connect(header, SIGNAL(customContextMenuRequested(const QPoint &)), this,
    SLOT(headerRightClicked(const QPoint &)));
  connect(treeResults, SIGNAL(expanded(const QModelIndex &)), this,
    SLOT(resultExpanded(const QModelIndex &)));
}

void GFSSearch::closeClicked()
{
  emit closeSearchWnd(uri);
}

void GFSSearch::downloadClicked()
{
  QModelIndexList allSel;
  GPersistentModelIndexList uniqSel;
  QModelIndexList::iterator it;

  /* Every column is treated as separate selection */
  allSel = treeResults->selectionModel()->selectedIndexes();
  for (it = allSel.begin(); it != allSel.end(); it++)
  {
    QModelIndex &idx = *it;

    if (idx.column() == 0)
      uniqSel.append(idx);
  }

  emit download(m, uri, uniqSel, spinAnon->value(), cbRecursive->isChecked());
}

void GFSSearch::copyURI()
{
  QModelIndexList allSel;
  GPersistentModelIndexList uniqSel;
  QModelIndexList::iterator it;
  QString strUris;

  /* Every column is treated as separate selection */
  allSel = treeResults->selectionModel()->selectedIndexes();
  for (it = allSel.begin(); it != allSel.end(); it++)
  {
    QModelIndex &idx = *it;

    if (idx.column() == MODEL_IDX(SEARCH_URI_COLUMN))
    {
      if (strUris != "")
#if defined(Q_WS_WIN)
        strUris += "\r\n";
#else
        strUris += "\n";
#endif
      strUris += idx.model()->data(idx).toString();
    }
  }

  QApplication::clipboard()->setText(strUris);
}

void GFSSearch::resultExpanded(const QModelIndex &index)
{
  QModelIndex idx;
  GItemModel *model;

  model = (GItemModel *) index.model();
  model->lock();
  if (model->data(index, Qt::UserRole) != RESULT_DOWNLOADED)
  {
    idx = model->index(index.row(), EXTRACTOR_MIMETYPE, index.parent());
    if (model->data(idx) == "application/gnunet-directory")
    {
      GPersistentModelIndexList list;

      list.append(index);

      model->unlock();
      emit download(m, uri, list, spinAnon->value(), false);

      return;
    }
  }

  model->unlock();
}

void GFSSearch::clear()
{
  m->clear();
  setupColumns();
}

void GFSSearch::setColHidden(int col, bool hidden)
{
  treeResults->header()->setSectionHidden(col, hidden);
  treeResults->model()->setHeaderData(col, Qt::Horizontal, QVariant(hidden), Qt::UserRole);
}

void GFSSearch::addColumn(QMenu *menu, QString strTitle, int nr)
{
  QAction *action;

  action = menu->addAction(strTitle);
  action->setCheckable(true);
  action->setData(QVariant(nr));

  if (!treeResults->header()->isSectionHidden(MODEL_IDX(nr)))
    action->setChecked(true);
}

void GFSSearch::headerRightClicked(const QPoint &pos)
{
  EXTRACTOR_KeywordType typeItem;
  QAction *item;

  QMenu *menu = new QMenu(treeResults->header());
  menu->move(pos);

  typeItem = EXTRACTOR_getHighestKeywordTypeNumber();
  while(typeItem >= (EXTRACTOR_KeywordType) 0)
  {
    QString strItem = metaTypeName(typeItem);

    if (strItem != "")
      addColumn(menu, strItem, typeItem);

    typeItem = (EXTRACTOR_KeywordType) ((int) typeItem - 1);
  }

  // add availability and relevance
  addColumn(menu, tr("Ranking"), SEARCH_RANK_COLUMN);

  item = menu->exec();
  if (item)
  {
    setColHidden(item->data().toInt(), !item->isChecked());

    // kludge: hiding rows causes recalculation of row heights.
    // This is necessary because we calculate the height for visible columns only.
    treeResults->setRowHidden(0, QModelIndex(), true);
    treeResults->setRowHidden(0, QModelIndex(), false);
  }

  delete menu;
}

void GFSSearch::resultInserted()
{
    tab->setTabText(tab->indexOf(this),
      uri.toDisplayString(fs->config(), fs->errorContext()) + " (" + QString::number(m->rowCount()) + ")");
}

void GFSSearch::setupColumns()
{
  int colIdx;
  GIntList cols;
  EXTRACTOR_KeywordType maxType;
  int typeIdx;

  maxType = EXTRACTOR_getHighestKeywordTypeNumber();
  m->setColumnCount(SEARCH_MAX_COLUMN + 1);
  for (typeIdx = 0; typeIdx <= maxType; typeIdx++)
  {
    GString label;

    label = metaTypeName(MODEL_IDX(typeIdx));
    label.proper();

    m->setHeaderData(typeIdx, Qt::Horizontal, label, Qt::DisplayRole);
  }
  m->setHeaderData(SEARCH_RANK_COLUMN, Qt::Horizontal, tr("Ranking"), Qt::DisplayRole);

  // get columns to be displayed
  // FIXME
  cols.append(EXTRACTOR_FILENAME);
  cols.append(EXTRACTOR_FILE_SIZE);
  cols.append(EXTRACTOR_MIMETYPE);
  cols.append(EXTRACTOR_THUMBNAIL_DATA);
  cols.append(SEARCH_RANK_COLUMN);

  for (colIdx = 0; colIdx < m->columnCount(); colIdx++)
    setColHidden(MODEL_IDX(colIdx), !cols.contains(colIdx));
  treeResults->setColumnWidth(0, 200);
  treeResults->setColumnWidth(MODEL_IDX(EXTRACTOR_FILE_SIZE), 50);
  treeResults->header()->moveSection(SEARCH_RANK_COLUMN, 2);
  treeResults->setColumnWidth(SEARCH_RANK_COLUMN, 50);
}

void GFSSearch::setModel(GItemModel *model)
{
  QByteArray data;

  m = model;
  treeResults->setModel(model->abstractItemModel());
  connect(model, SIGNAL(rowsInserted(const QModelIndex &, int, int)), this,
    SLOT(resultInserted()));

  setupColumns();

  resultInserted();
}

void GFSSearch::setItemDelegate(QAbstractItemDelegate *itemDelegate)
{
  treeResults->setItemDelegate(itemDelegate);
}

void GFSSearch::setUri(GFSEcrsUri &uri)
{
  this->uri = uri;
}

/* fs-search.cc */
