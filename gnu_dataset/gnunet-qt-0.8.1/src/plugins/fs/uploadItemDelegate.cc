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
 * @file src/plugins/fs/uploadItemDelegate.cc
 * @brief Delegate that renders the upload view
 * @author Nils Durner
 */

#include <QPainter>
#include <QApplication>
#include "gnunet_qt_common.h"
#include "uploadItemDelegate.h"

QSize GFSUploadItemDelegate::sizeHint(const QStyleOptionViewItem &option, const QModelIndex &index) const
{
  QSize ret;
  GItemModel *model;

  model = (GItemModel *) index.model();
  model->lock();
  ret = QItemDelegate::sizeHint(option, index);
  model->unlock();
  
  return ret;
}

void GFSUploadItemDelegate::paint(QPainter *painter, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
  GItemModel *model;

  model = (GItemModel *) index.model();
  model->lock();

  if (index.column() == 1)
  {
    QStyleOptionProgressBarV2 barOptions;
    double percentage;
    QString str;
    
    percentage = index.model()->data(index).toDouble();
    str.setNum(percentage, 'f', 2);
    
    barOptions.maximum = 100;
    barOptions.minimum = 0;
    barOptions.progress = (int) percentage;
    barOptions.text = str + "%";
    barOptions.textVisible = true;
    barOptions.rect = option.rect;
    
    qApp->style()->drawControl(QStyle::CE_ProgressBar, &barOptions, painter);
  
    model->unlock();
    return;
  }
  
  QItemDelegate::paint(painter, option, index);
  model->unlock();
}

/* end of uploadItemDelegate.cc */
