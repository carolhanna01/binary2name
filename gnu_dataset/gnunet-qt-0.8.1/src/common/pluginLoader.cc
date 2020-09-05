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
 * @file src/common/pluginLoader.cc
 * @brief Load & unload gnunet-qt plugins
 * @author Nils Durner
 */

#include "pluginLoader.h"
#include <GNUnet/gnunet_util_error.h>

GPluginLoader::~GPluginLoader()
{
  unloadAll();
}

GPlugin *GPluginLoader::load(const QString &strName,
    GPluginInitParams *params)
{
  InitPlugin init;
  GPluginSpec spec;

  spec.lib = GNUNET_plugin_load(params->errorContext, "libgnunetqtmodule_", qPrintable(strName));
  spec.wnd = NULL;

  if (spec.lib)
  {
    plugins.append(spec);
    init = (InitPlugin) GNUNET_plugin_resolve_function(spec.lib, "init_", GNUNET_YES);
    if (init)
      spec.wnd = init(params);
  }

  return spec.wnd;
}

void GPluginLoader::unloadAll()
{
  ShutdownPlugin shutdown;
  GPluginSpec spec;

  int count = plugins.count();
  while (count)
  {
    spec = plugins.takeLast();
    shutdown = (ShutdownPlugin) GNUNET_plugin_resolve_function(spec.lib, "shutdown_", GNUNET_YES);
    if (shutdown)
      shutdown(spec.wnd);

    GNUNET_plugin_unload(spec.lib);
    count--;
  }
}

/* end of pluginLoad.cc */
