/*
     This file is part of gnunet-qt.
     (C) 2006 Nils Durner (and other contributing authors)

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
 * @file src/common/gstring.cc
 * @brief Extended QString
 * @author Nils Durner
 */

#include <QByteArray>
#include <string.h>

#include "gnunet_qt_common.h"

GString::GString(const char *str) : QString(str)
{
  cstr = NULL;
}

GString::GString() : QString()
{
  cstr = NULL;
}

GString::~GString()
{
  if (cstr)
    ::free(cstr);
}

GString &GString::operator=(const QString &src)
{
  if (cstr)
  {
    ::free(cstr);
    cstr = NULL;
  }

  QString::operator=(src);
  return *this;
}

GString &GString::operator=(const GString &src)
{
  if (cstr)
  {
    ::free(cstr);
    cstr = NULL;
  }

  QString::operator=(src);
  return *this;
}

GString &GString::operator=(const char *src)
{
  if (cstr)
  {
    ::free(cstr);
    cstr = NULL;
  }

  QString::operator=(src);
  return *this;
}

GString::GString(QString &src) : QString(src)
{
  cstr = NULL;
}

/**
 * @brief Capitalize every word in this string
 */
void GString::proper()
{
  int idx = length() - 1;
  
  if (idx < 0)
    return;

  while(true)
  {
    QChar c = at(idx);

    if (idx == 0)
    {
      if (c.isLower())
        replace(0, 1, c.toUpper());
        
      break;
    }
    else  
      if (c.isSpace())
      {
        int dst = idx + 1;
        
        if (at(dst).isLower())
          replace(dst, 1, at(dst).toUpper());
      }
    
    idx--;
  }
}

char *GString::toCString()
{
  QByteArray bytes = toLocal8Bit();
  
  if (cstr)
    ::free(cstr);
  
  return cstr = strdup(bytes.data());
}

char *GString::toUtf8CStr()
{
  QByteArray bytes = toUtf8();
  
  if (cstr)
    ::free(cstr);
  
  return cstr = strdup(bytes.data());
}

GString GString::fromByteSize(qlonglong size)
{
  GString ret;
  char *sz;
  
  sz = (char *) malloc(14);
  if (size >= 1000000000)
    snprintf(sz, 13, "%.2f %s", size / 1000000000.0, qPrintable(QObject::tr("GB")));
  else if (size >= 1000000)
    snprintf(sz, 13, "%.2f %s", size / 1000000.0, qPrintable(QObject::tr("MB")));
  else if (size >= 1000)
    snprintf(sz, 13, "%.2f %s", size / 1000.0, qPrintable(QObject::tr("KB")));
  else
    snprintf(sz, 13, "%.0f %s", (double) size, qPrintable(QObject::tr("Bytes")));

  ret = sz;
  ::free(sz);
  
  return ret;
}

/* end of gstring.cc */
