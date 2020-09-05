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
 * @file src/plugins/fs/uploadDialog.cc
 * @brief FS upload dialog that asks for meta data
 * @author Nils Durner
 */

#include <QPixmap>
#include <QFileDialog>
#include <QBuffer>
#include <extractor.h>

#include "gnunet_qt_common.h"
#include "uploadDialog.h"

GFSUploadDialog::GFSUploadDialog(GFSMetaData *metaData,
  QStringList *keywords, QWidget *parent) : QDialog(parent)
{
  GFSMetaData::iterator iterMeta;
  QStringList::iterator iterKeywords;
  EXTRACTOR_KeywordType i, types;
  QByteArray thumbnail;
  QStringList colHeaders;
  
  setupUi(this);
  
  // setup signal handlers
  connect(treeMeta, SIGNAL(currentItemChanged(QTreeWidgetItem *,  QTreeWidgetItem *)), this,
    SLOT(metaSelectionChanged(QTreeWidgetItem *, QTreeWidgetItem *)));
  connect(pbAddMeta, SIGNAL(clicked(bool)), this, SLOT(metaAdd()));
  connect(pbRemoveMeta, SIGNAL(clicked(bool)), this, SLOT(metaDel()));
  connect(pbLongMetaVal, SIGNAL(clicked(bool)), this, SLOT(longMetaValClicked()));
  connect(treeKeywords, SIGNAL(currentItemChanged(QTreeWidgetItem *,  QTreeWidgetItem *)), this,
    SLOT(keywordSelectionChanged(QTreeWidgetItem *, QTreeWidgetItem *)));
  connect(pbAddKeyword, SIGNAL(clicked(bool)), this, SLOT(keywordAdd()));
  connect(pbRemoveKeyword, SIGNAL(clicked(bool)), this, SLOT(keywordDel()));
  connect(pbOpenThumb, SIGNAL(clicked(bool)), this, SLOT(chooseThumb()));
  
  // setup content

  // Metadata types
  i = (EXTRACTOR_KeywordType) 0;
  types = EXTRACTOR_getHighestKeywordTypeNumber();
  while(i <= types)
  {
    cmbType->addItem(metaTypeName(i), QVariant(i));
    i = (EXTRACTOR_KeywordType) ((int) i + 1);
  }
  
  // Meta-data
  this->metaData = metaData;
  colHeaders.append(tr("Type"));
  colHeaders.append(tr("Value"));
  treeMeta->setHeaderLabels(colHeaders);
  
  for (iterMeta = metaData->begin(); iterMeta != metaData->end(); iterMeta++)
  {
    EXTRACTOR_KeywordType type;
    
    type = iterMeta.key();
    if (type != EXTRACTOR_THUMBNAIL_DATA)
    {
      QTreeWidgetItem *item = new QTreeWidgetItem();
      item->setData(0, Qt::UserRole, type);
      item->setText(0, metaTypeName(type));
      item->setText(1, QString::fromUtf8(*iterMeta));
      treeMeta->addTopLevelItem(item);      
    }
    else
      thumbnail = iterMeta.value();
  }

  // Keywords
  this->keywords = keywords;
  colHeaders.clear();
  colHeaders.append(tr("Keyword"));
  treeKeywords->setHeaderLabels(colHeaders);

  for (iterKeywords = keywords->begin(); iterKeywords != keywords->end();
    iterKeywords++)
  {
    QTreeWidgetItem *item = new QTreeWidgetItem();
    item->setText(0, *iterKeywords);
    treeKeywords->addTopLevelItem(item);
  }
 
  // Preview
  if (thumbnail.size())
  {
    QPixmap pic;
    pic.loadFromData((const uchar *) thumbnail.data(), (uint) thumbnail.size());
    preview->setPixmap(pic);
  }
}

void GFSUploadDialog::metaSelectionChanged(QTreeWidgetItem *current, QTreeWidgetItem *previous)
{
  Q_UNUSED(previous)
  
  if (current)
  {
    cmbType->setCurrentIndex(cmbType->findText(current->text(0)));
    editValue->setText(current->text(1));
  }
  else
  {
    cmbType->setCurrentIndex(0);
    editValue->setText("");    
  }
}

void GFSUploadDialog::metaAdd()
{
  QTreeWidgetItem *item;
  int type;
  
  item = new QTreeWidgetItem();
  
  type = cmbType->itemData(cmbType->currentIndex()).toInt();
  item->setData(0, Qt::UserRole, type);
  item->setText(0, metaTypeName((EXTRACTOR_KeywordType) type));
  item->setText(1, editValue->text());
  treeMeta->addTopLevelItem(item);
  treeMeta->setCurrentItem(item);
  
  metaData->insertMulti((EXTRACTOR_KeywordType) type, editValue->text().toUtf8());
}

void GFSUploadDialog::metaDel()
{
  QTreeWidgetItem *item = treeMeta->currentItem();
  
  if (item)
  {
    GFSMetaData newMeta;
    GFSMetaData::iterator it;
    EXTRACTOR_KeywordType type;
    QByteArray data;
    
    item = treeMeta->takeTopLevelItem(treeMeta->indexOfTopLevelItem(item));

    // remove meta data from internal list
    type = (EXTRACTOR_KeywordType) item->data(0, Qt::UserRole).toInt();
    data = item->text(1).toUtf8();
    for (it = metaData->begin(); it != metaData->end(); it++)
      if (type != it.key() || data != it.value())
        newMeta.insert(type, it.value());
    *metaData = newMeta;

    // select next/previous item
    item = treeMeta->currentItem();
    if (item)
      treeMeta->setItemSelected(item, true);
  }
}

void GFSUploadDialog::keywordSelectionChanged(QTreeWidgetItem *current, QTreeWidgetItem *previous)
{
  Q_UNUSED(previous)
  
  if (current)
    editKeyword->setText(current->text(0));
  else
    editKeyword->setText("");    
}

void GFSUploadDialog::keywordAdd()
{
  QTreeWidgetItem *item;
  
  item = new QTreeWidgetItem();
  
  item->setText(0, editKeyword->text());
  treeKeywords->addTopLevelItem(item);
  treeKeywords->setCurrentItem(item);

  keywords->push_back(editKeyword->text());
}

void GFSUploadDialog::keywordDel()
{
  QTreeWidgetItem *item = treeKeywords->currentItem();
  
  if (item)
  {
    keywords->removeAll(item->text(0));
    
    treeKeywords->takeTopLevelItem(treeKeywords->indexOfTopLevelItem(item));
    item = treeKeywords->currentItem();
    if (item)
      treeKeywords->setItemSelected(item, true);
  }
}

void GFSUploadDialog::chooseThumb()
{
  static QString path;
  
  path = QFileDialog::getOpenFileName(this,
      QString(), path,
        tr("All graphics (*.bmp *.gif *.jpg *.jpeg *.png *.pbm *.pgm *.ppm *.xbm "
          "*.xpm);;") +
          "Windows Bitmap (*.bmp);;"
          "Graphic Interchange Format (*.gif);;"
          "Joint Photographic Experts Group (*.jpg *.jpeg);;"
          "Portable Network Graphics (*.png);;"
          "Portable Bitmap (*.pbm);;"
          "Portable Graymap (*.pgm);;"
          "Portable Pixmap (*.ppm);;"
#ifdef HAVE_QT_SVG
          "Scalable Vector Graphics (*.svg);;"
#endif
          "X11 Bitmap (*.xbm);;"
          "X11 Pixmap (*.xpm)");
          
  if (path != "")
  {
    QImage *img;
    QByteArray bytes;
    QBuffer buffer;
    char *binary;
   
#ifdef HAVE_QT_SVG
    if (path.endsWith(".svg"))
    {
      /* Render SVG image */
      QSvgRenderer svg;
      QSize size;
  
      if (! svg.load(QByteArray(path)))
        return;
  
      size = svg.defaultSize();
      img = new QImage(size, QImage::Format_Indexed8);
  
      QPainter painter(img);
      painter.setViewport(0, 0, size.width(), size.height());
      painter.eraseRect(0, 0, size.width(), size.height());
  
      svg.render(&painter);
    }
    else
#endif
    {
      img = new QImage(path);
      *img = img->convertToFormat(QImage::Format_Indexed8);
    }
       
    if (!img->isNull())
    {
      unsigned long width, height;
 
      height = img->height();
      width = img->width();
      
      /* Resize image
       *
       * Qt's scaled() produces poor quality if the image is resized to less than
       * half the size. Therefore, we resize the image in multiple steps.
       * http://lists.trolltech.com/qt-interest/2006-04/msg00376.html */
      while(true)
      {
        width /= 2;
        if (width < 128)
          width = 128;
    
        height /= 2;
        if (height < 128)
          height = 128;
    
        *img = img->scaled(width, height, Qt::KeepAspectRatio,
          Qt::SmoothTransformation);
    
        if (width == 128 && height == 128)
          break;
      }
      
      buffer.setBuffer(&bytes);
      buffer.open(QIODevice::WriteOnly);
      img->save(&buffer, "PNG");
      binary = EXTRACTOR_binaryEncode((const unsigned char*) bytes.data(),
        bytes.length());

      if (binary)
      {
        metaData->replace(EXTRACTOR_THUMBNAIL_DATA, QByteArray(binary));
        free(binary);
      }
    }
    
    QPixmap pix = QPixmap::fromImage(*img);
    preview->setPixmap(pix);
    
    delete img;
  }
}

bool GFSUploadDialog::extract()
{
  return cbExtract->isChecked();
}

bool GFSUploadDialog::useKeywords()
{
  return cbUse->isChecked();
}

void GFSUploadDialog::longMetaValClicked()
{
  GTextEditor edit(editValue->text(), this);
  
  if (edit.exec() == QDialog::Accepted)
    editValue->setText(edit.text());
}

/* end of uploadDialog.cc */
