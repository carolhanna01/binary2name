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
 * @file src/common/pluginLoader.h
 * @brief Load & unload gnunet-qt plugins
 * @author Nils Durner
 */

#ifndef PLUGINLOADER_H_
#define PLUGINLOADER_H_

#include <QObject>
#include <GNUnet/gnunet_util.h>

#include "gnunet_qt_common.h"
#include "plugin.h"

typedef struct
{
  struct GNUNET_PluginHandle *lib;
  class GPlugin *wnd;
} GPluginSpec;


class GPluginLoader : public QObject
{
  Q_OBJECT

public:
  ~GPluginLoader();
  GPlugin *load(const QString &strName, GPluginInitParams *params);
  void unloadAll();

protected:
  QList<GPluginSpec> plugins;
};

typedef GPlugin *(*InitPlugin) (GPluginInitParams *params);
typedef void (*ShutdownPlugin) (GPlugin *plugin);

#endif /*PLUGINLOADER_H_*/

/* end of pluginLoader.h */
