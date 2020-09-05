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
 * @file src/plugins/fs/searchItemDelegate.cc
 * @brief Paints search results
 * @author Nils Durner
 */

#include <QApplication>
#include <QPainter>
#include <QBrush>
#include <QColor>
#include <extractor.h>
#include <GNUnet/gnunet_fsui_lib.h>
#include "searchItemDelegate.h"
#include "fshelper.h"

QSize GSearchItemDelegate::sizeHint(const QStyleOptionViewItem &option, const QModelIndex &index) const
{
  GItemModel *model;
  QVariant var;
  QSize ret;
  
  model = (GItemModel *) index.model();
  model->lock();
  var = model->headerData(index.column(), Qt::Horizontal, Qt::UserRole);
  
  if (var.toInt() == 1)
  {
    model->unlock();
    return QSize(0, 0);
  }
  
  var = model->data(index);
  if (index.column() == MODEL_IDX(EXTRACTOR_THUMBNAIL_DATA) && var.type() == QVariant::ByteArray)
  {
    QImage img;
    
    model->unlock();
    img.loadFromData(var.toByteArray());
    
    return img.size();
  }

  ret = QItemDelegate::sizeHint(option, index);
  model->unlock();
  return ret;
}

void GSearchItemDelegate::paint(QPainter *painter, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
  QVariant var;
  GItemModel *model;
  int col;
  
  model = (GItemModel *) index.model();
  model->lock();
  var = model->data(index);
  col = index.column();
  
  if (col == MODEL_IDX(EXTRACTOR_THUMBNAIL_DATA) && var.type() == QVariant::ByteArray)
  {
    QImage img;
    int left;
    
    drawBackground(painter, option, index);
    
    img.loadFromData(var.toByteArray());
    
    left = (option.rect.width() - img.width()) / 2;
    if (left < 0)
      left = 0;
    left += option.rect.x();
    
    painter->drawImage(QPoint(left, option.rect.top()), img);
  }
  else if (col == SEARCH_RANK_COLUMN)
  {
    GRanking rank;
    QString str;
    QRect rect;
    int colwidth, colheight, w, x1, x2, ydelta, hue;
        
    rank = var.value<GRanking>();
    drawBackground(painter, option, index);

    colwidth = option.rect.width() - 3;
    colheight = option.rect.height() - 2;
    if (colheight > 16)
      colheight = 16;
    ydelta = (option.rect.height() - colheight) / 2;
    
    rect.setRect(option.rect.x() + 1, option.rect.y() + ydelta, colwidth *
        (rank.applicability_rank / (double) rank.keywords), colheight / 2);
    if (!var.isNull())
      painter->fillRect(rect, QBrush(QColor(0, 0, 255)));
    
    rect.setWidth(colwidth);
    painter->drawRect(rect);
    
    w = (int) ((colwidth / 2.0) / GNUNET_FSUI_MAX_PROBES * rank.availability_rank);
    x1 = rect.x() + colwidth / 2;
    x2 = x1 + w;
    
    if (w < 0)
    {
      w *= -1;
      hue = 0;
    }
    else
      hue = 170;
    
    rect.setRect((x1 < x2 ? x1 : x2), option.rect.y() + ydelta + (colheight / 2), w + 1, colheight / 2);
    if (!var.isNull())
      painter->fillRect(rect, QBrush(QColor::fromHsv(hue, 255, 128 + 
          (GNUNET_FSUI_MAX_PROBES - rank.availability_certainty) * ((240.0 - 128) / GNUNET_FSUI_MAX_PROBES))));

    rect.setX(option.rect.x() + 1);
    rect.setWidth(colwidth);
    rect.setHeight(colheight / 2 - 1);
    painter->drawRect(rect);
    painter->drawLine(rect.x() + colwidth / 2, rect.y(),
        rect.x() + colwidth / 2, rect.y() + colheight / 2 - 1);
  }
  else if (model->data(index, Qt::UserRole) == 1)
  {
    // expanded directory
    painter->drawText(option.rect, tr("Retrieving content..."));
  }
  else
    QItemDelegate::paint(painter, option, index);
  
  model->unlock();
}

/* end of searchItemDelegate.cc */
