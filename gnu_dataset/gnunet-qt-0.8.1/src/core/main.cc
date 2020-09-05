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
 * @file src/core/main.cc
 * @brief Main function of gnunet-qt
 * @author Nils Durner
 */

#include "config.h"
#include "main.h"
#ifdef __WIN32__
#include "winproc.h"
#endif
#include "GNUnet/gnunet_util.h"
#include "GNUnet/gnunet_util_boot.h"
#include "GNUnet/gnunet_directories.h"

#include <QObject>
#include <QTranslator>
#include <QLocale>
#include <QDir>
#include <QMessageBox>
#include <QMutex>

static GApplication *app;
static char *cfgFilename = GNUNET_DEFAULT_CLIENT_CONFIG_FILE;

static struct GNUNET_GE_Context *ectx, *stdECtx;
static struct GNUNET_GC_Configuration *cfg;

#if defined(Q_OS_WIN)
static int debug_mode = GNUNET_NO;
#endif


/**
 * GNUnet's header #defines COMMAND_LINE_OPTION_XXX as call to gettext_noop.
 * Since we do not link against gettext, but use Qt, we #define gettext_noop here
 */
#ifndef gettext_noop
  #define gettext_noop(x) app->tr(x).toStdString().c_str()
#endif

/**
 * All gnunet-qt command line options
 */
static struct GNUNET_CommandLineOption gnunetqtOptions[] = {
  GNUNET_COMMAND_LINE_OPTION_CFG_FILE(&cfgFilename), /* -c */
#if defined(Q_OS_WIN)
  { 'd', "debug", NULL,
    gettext_noop("run in debug mode"),
    0, &GNUNET_getopt_configure_set_one, &debug_mode },
#endif
  GNUNET_COMMAND_LINE_OPTION_HELP(gettext_noop("GNUnet Qt user interface.")), /* -h */
  GNUNET_COMMAND_LINE_OPTION_HOSTNAME, /* -H */
  GNUNET_COMMAND_LINE_OPTION_LOGGING, /* -L */
  GNUNET_COMMAND_LINE_OPTION_VERSION(VERSION), /* -v */
  GNUNET_COMMAND_LINE_OPTION_VERBOSE,
  GNUNET_COMMAND_LINE_OPTION_END
};

static QString *logIcons;

typedef struct
{
  QMutex lock;
  GMainWindow *mainWnd;
} GLoggerClosure;

static void logger(void *cls, GNUNET_GE_KIND kind, const char *date, const char *msg)
{
  Q_UNUSED(date)

  QString *icon;

  if (kind & GNUNET_GE_DEBUG)
    icon = logIcons;
  else if (kind & GNUNET_GE_STATUS)
    icon = logIcons + 1;
  else if (kind & GNUNET_GE_INFO)
    icon = logIcons + 2;
  else if (kind & GNUNET_GE_WARNING)
    icon = logIcons + 3;
  else if (kind & GNUNET_GE_ERROR)
    icon = logIcons + 4;
  else if (kind & GNUNET_GE_FATAL)
    icon = logIcons + 5;

  ((GLoggerClosure *) cls)->lock.lock();
  ((GLoggerClosure *) cls)->mainWnd->setStatusText(*icon, QString(msg));
  ((GLoggerClosure *) cls)->lock.unlock();
}

GApplication::GApplication(int &argc, char **argv,
    struct GNUNET_GC_Configuration *cfg) : QApplication(argc, argv)
{
  this->ectx = NULL;
  this->cfg = cfg;

  strCfgFile = cfgFilename;
}

void GApplication::setErrorContext(struct GNUNET_GE_Context *ectx)
{
  this->ectx = ectx;
}

void GApplication::setupMenuStruct()
{
  GMenuAction action;
  GMenu menu;

  // File menu
  menu.name = "gnunet-qt::core::file";
  menu.title = QApplication::translate("WndMain", "&File", 0, QApplication::UnicodeUTF8);

  action.name = "gnunet-qt::core::file::exit";
  action.icon = QIcon(":/pixmaps/exit.png");
  action.text = QApplication::translate("WndMain", "&Exit", 0, QApplication::UnicodeUTF8);
  action.menuRole = QAction::QuitRole;
  action.receiver = this;
  action.receiverSlot = SLOT(quit());
  action.action = &wnd.actionExit;
  menu.actions.push_back(action);

  menuStruct.push_back(menu);

  //Help menu
  menu.name = "gnunet-qt::core::help";
  menu.title = QApplication::translate("WndMain", "&Help", 0, QApplication::UnicodeUTF8);
  menu.actions.clear();

  action.name = "gnunet-qt::core::help::contextHelp";
  action.icon = QIcon(":/pixmaps/whatsthis.png");
  action.text = QApplication::translate("WndMain", "&Context help", 0, QApplication::UnicodeUTF8);
  action.receiver = &wnd;
  action.receiverSlot = SLOT(contextHelp());
  action.action = &wnd.action_Context_help;
  menu.actions.push_back(action);

  action.name = "gnunet-qt::core::help::about";
  action.icon = QIcon(":/pixmaps/gnunet-logo-small.png");
  action.text = QApplication::translate("WndMain", "About", 0, QApplication::UnicodeUTF8);
  action.menuRole = QAction::AboutRole;
  action.receiver = &wnd;
  action.receiverSlot = SLOT(about());
  action.action = &wnd.actionAbout;
  menu.actions.push_back(action);

  menuStruct.push_back(menu);
}

void GApplication::loadPlugins()
{
  int count;
  char *cfgPlugins;
  GPluginInitParams params;

  params.errorContext = ectx;
  params.config = cfg;
  params.menu = &menuStruct;

  GNUNET_GC_get_configuration_value_string(cfg, "GNUNET-QT", "PLUGINS", "about general fs stats", &cfgPlugins);
  QStringList plugins = QString(cfgPlugins).split(QRegExp("\\s+"));
  count = plugins.count();

  while(count)
  {
    QWidget *child;
    QString strPlugin = plugins.takeFirst();

    child = loader.load(strPlugin, &params);
    if (child)
       wnd.addApplication(child, child->windowIcon(), child->windowTitle());

    count--;
  }

  GNUNET_free(cfgPlugins);
}

void GApplication::showWindow()
{
  wnd.loadMenuStruct(menuStruct);
  wnd.show();
}

GMainWindow *GApplication::getWindow()
{
  return &wnd;
}

#if defined(Q_OS_WIN)
bool GApplication::winEventFilter(MSG *msg, long *result)
{
  if (win_isWMClose(msg))
  {
    *result = 0;
    wnd.setVisible(false);
    return true;
  }

  return false;
}
#endif

int
#if defined(Q_OS_WIN)
 gn_main
#else
 main
#endif
(int argc, char * const *argv)
{
  int ret = 0;
  QTranslator transl;
  QString strLocale;
  GLoggerClosure logCls;

  if (GNUNET_init(argc, argv, "gnunet-qt [OPTIONS]", &cfgFilename,
        gnunetqtOptions, &stdECtx, &cfg) == GNUNET_SYSERR)
    return 1;

  app = new GApplication(argc, (char **) argv, cfg);

  logIcons = new QString[6];
  logIcons[0] = ":/pixmaps/debug.png";
  logIcons[1] = ":/pixmaps/stats.png";
  logIcons[2] = ":/pixmaps/info.png";
  logIcons[3] = ":/pixmaps/warning.png";
  logIcons[4] = ":/pixmaps/error.png";
  logIcons[5] = ":/pixmaps/fatal.png";

  logCls.mainWnd = app->getWindow();
  // FIXME: make mask configurable
  ectx = GNUNET_GE_create_context_callback((GNUNET_GE_KIND) (GNUNET_GE_USER | GNUNET_GE_ADMIN | GNUNET_GE_ERROR |
    GNUNET_GE_WARNING | GNUNET_GE_FATAL | GNUNET_GE_IMMEDIATE), &logger, &logCls, NULL, NULL);
  GNUNET_GE_setDefaultContext(ectx);

  strLocale = QLocale::system().name();
#if defined(Q_OS_WIN)
  if (strLocale == "C")
    strLocale = win_locale();
#endif
  transl.load(QString("gnunet-qt_") + strLocale,
    QString(GNUNET_get_installation_path(GNUNET_IPK_DATADIR)) + ".." + QDir::separator() +
      "gnunet-qt" + QDir::separator() + "locale" + QDir::separator());
  app->installTranslator(&transl);

  app->addLibraryPath(app->applicationDirPath() + "/../lib/GNUnet/");

  app->setupMenuStruct();
  app->loadPlugins();
  app->showWindow();

#if defined(Q_OS_WIN)
  if (!debug_mode)
    win_freeConsole();
#endif

  ret = app->exec();

  delete app;

  GNUNET_GE_free_context(ectx);
  delete [] logIcons;
  GNUNET_fini(stdECtx, cfg);

	return ret;
}

/* end of main.cc */
