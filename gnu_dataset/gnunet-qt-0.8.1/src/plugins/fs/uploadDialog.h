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
     along with GNUnet; see the file COPYING.  If not, write to the
     Free Software Foundation, Inc., 59 Temple Place - Suite 330,
     Boston, MA 02111-1307, USA.
*/

/**
 * @file src/plugins/fs/uploadDialog.h
 * @brief FS upload dialog that asks for meta data
 * @author Nils Durner
 */

#ifndef UPLOADDIALOG_H_
#define UPLOADDIALOG_H_

#include <QDialog>
#include <QTreeWidgetItem>
#include <QList>

#include "ui_fs-upload.h"
#include "fshelper.h"

class GFSUploadDialog : public QDialog, protected Ui::dlgUpload
{
  Q_OBJECT
  
public:
  GFSUploadDialog(GFSMetaData *metaData,
    QStringList *keywords, QWidget *parent = NULL);
    
  bool extract();
  bool useKeywords();
protected:
  GFSMetaData *metaData;
  QStringList *keywords;
protected slots:
  void metaSelectionChanged(QTreeWidgetItem *current, QTreeWidgetItem *previous);
  void metaAdd();
  void metaDel();
  void keywordSelectionChanged(QTreeWidgetItem *current, QTreeWidgetItem *previous);
  void keywordAdd();
  void keywordDel();
  void chooseThumb();
  void longMetaValClicked();
};

#endif /*UPLOADDIALOG_H_*/

/* end of uploadDialog.h */

