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
 * @file src/include/gnunet_qt_common.h
 * @brief Common functions for gnunet-qt plugins
 * @author Nils Durner
 */

#ifndef GNUNET_QT_COMMON_H_
#define GNUNET_QT_COMMON_H_

#include <QWidget>
#include <QString>
#include <QLibrary>
#include <QPersistentModelIndex>

#include "../common/desktopServices.h"
#include "../common/event.h"
#include "../common/eventDispatcher.h"
#include "../common/itemModel.h"
#include "../common/textEditor.h"
#include "../common/plugin.h"
#include "../common/pluginInitParams.h"
#include "../common/pluginLoader.h"

#define GNUNETQT_ASSERT(cond)  do { if (! (cond)) gnunet_qt_assert_quit(__FILE__, __LINE__); } while(0);

// FIXME
#ifndef _
  #define _
#endif

#ifdef Q_OS_WIN32
  #define GNUNETQT_API __declspec(dllexport)
#else
  #define GNUNETQT_API
#endif

void gnunet_qt_assert_quit(char *file, int line);

typedef QList<int> GIntList;

typedef QList<QPersistentModelIndex> GPersistentModelIndexList;

class GString : public QString
{
public:
  GString();
  GString(const char *str);
  GString(QString &src);
  ~GString();
  GString &operator=(const QString &src);
  GString &operator=(const GString &src);
  GString &operator=(const char *src);

  /**
   * @brief Capitalize every word in this string
   */
  void proper();

 /**
  * @brief Return the content as C string
  */
 char *toCString();

 /**
  * @brief Return the content as UTF-8 encoded C string
  */
 char *toUtf8CStr();

 static GString fromByteSize(qlonglong size);
protected:
  char *cstr;
};

#endif /*GNUNET_QT_COMMON_H_*/

/* end of gnunet_qt_common.h */
