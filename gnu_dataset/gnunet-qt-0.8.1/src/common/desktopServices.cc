/*
     This file is part of gnunet-qt.
     (C) 2009 Nils Durner (and other contributing authors)

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

#include "config.h"
#ifndef __WIN32__
#include <QDesktopServices>
#include <QUrl>
#else
#include <windows.h>
#endif
#include "desktopServices.h"

/**
 * @file src/common/desktopServices.cc
 * @brief Desktop integration
 * @author Nils Durner
 */

/**
 * @brief Opens a file in its default viewer
 * @param strPath path to the file
 * @return true on success
 */
bool GDesktopServices::openDocument(const char *strPath)
{
#ifdef __WIN32__
  // Qt 4.2 URL encodes files which is not understood by Windows, so
  // we call ShellExecute directly instead of openUrl()
  return ((unsigned int) ShellExecuteA(0, 0, strPath, 0, 0, SW_SHOWNORMAL)) > 32;
#else
  QUrl url;

  url.setScheme("file");
  url.setPath(QString(strPath));

  return QDesktopServices::openUrl(url);
#endif
}
