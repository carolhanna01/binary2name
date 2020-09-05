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
     along with gnunet-qt; see the file COPYING.  If not, write to the
     Free Software Foundation, Inc., 59 Temple Place - Suite 330,
     Boston, MA 02111-1307, USA.
*/

/**
 * @file src/plugin/about/about.cc
 * @brief gnunet-qt's welcome screen
 * @author Nils Durner
 */

#include <config.h>
#include <QDesktopServices>
#include "about.h"

GAboutPlugin::GAboutPlugin() : GPlugin()
{
  setupUi(this);
  connect(htmAbout, SIGNAL(anchorClicked(const QUrl &)), this, SLOT(linkHandler(const QUrl &)));

  welcome();
}

QString GAboutPlugin::header()
{
  return QString(
        "<table bgcolor=\"#3F4C6B\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\" height=\"62\" width=\"100%\">"
          "<tr>"
            "<td colspan=\"3\" height=\"10\" nowrap=\"nowrap\" valign=\"middle\" />"
          "</tr>"
          "<tr>"
            "<td width=\"20\" />"
            "<td>"
              "<font color=\"white\" face=\"Arial, Helvetica\" size=\"6\"><b>GNUnet</b></font>"
              "<br>"
              "<font color=\"#d3d3d3\" size=\"4\" face=\"Bitstream Vera Sans, Lucida Grande, Trebuchet MS, Lucida Sans Unicode, Luxi Sans, Helvetica, Arial, Sans-Serif\">"
                  + tr("GNU&#8216;s decentralized anonymous and censorship-resistant P2P framework.") +
              "</font>"
            "<td align=\"right\">"
                "<img src=\"qrc:/pixmaps/gnunet-qt-logo.png\" />&nbsp;&nbsp;&nbsp;&nbsp;"
            "</td>"
          "</tr>"
        "</table>"
        "<table bgcolor=\"#3F4C6B\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\" width=\"100%\">"
          "<tr>"
            "<td colspan=\"4\" height=\"10\" nowrap=\"nowrap\" />"
          "</tr>"
        "</table>"
        "<table bgcolor=\"#3F4C6B\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\" width=\"100%\">"
          "<tr>"
            "<td bgcolor=\"#E4E4E4\" color=\"#FFFFFF\" width=\"33%\" align=\"left\"><a href=\"about\">"
              "<font face=\"Arial, Helvetica\">" + tr("About") + "</font></a></td>"
            "<td bgcolor=\"#E4E4E4\" color=\"#FFFFFF\" width=\"33%\" align=\"center\"><a href=\"welcome\">"
              "<font face=\"Arial, Helvetica\">" + tr("Welcome") + "</font></a>"
            "</td>"
            "<td bgcolor=\"#E4E4E4\" color=\"#FFFFFF\" width=\"34%\" align=\"right\"><a href=\"notes\">"
              "<font face=\"Arial, Helvetica\">" + tr("Release notes") + "</font></a>"
            "</td>"
          "</tr>"
        "</table>"
        "<br>"
  );
}

void GAboutPlugin::welcome()
{
  htmAbout->setHtml(
    "<html>"
      "<body>" +
        header() +
        "<center>"
          "<font size=\"5\"><b>" + tr("Welcome to ") + PACKAGE_STRING "</b></font>"
          "<br />"
          "<br />"
          "<table width=\"91%\">"
            "<tr>"
              "<td>"
                "<font size=\"4\">" +
    tr("gnunet-qt is an integrated user interface for GNUnet.") +
                  "<br /><br />"
                  "<font size=\"5\">" +
    tr("Features") +
                  "</font>"
                  "<ul>"
                    "<li>" +
    tr("Anonymous publication and retrieval of individual files as well as complete directory structures") +
                    "</li>"
                    "<li>" +
    tr("Search results with thumbnail previews and extensive content information, e.g. author, subject, ...") +
                    "</li>"
                     "<li>" +
    tr("GNUnet network statistics") +
                    "</li>"
                  "</ul>"
                "</font>"
              "</td>"
            "</tr>"
            "<tr>"
            "</tr>"
            "<tr>"
              "<td>"
                  "<font size=\"5\">" +
    tr("Further information") +
                  "</font>"
                  "<font size=\"4\">"
                    "<ul>"
                      "<li>" +
    tr("Documentation and Questions & Answers from the GNUnet team: ") +
                        "<a href=\"http://gnunet.org\">" +
    tr("GNUnet homepage") +
                        "</a>"
                      "</li>"
                      "<li>" +
    tr("Help forum, blogs and extended documentation: ") +
                        "<a href=\"http://gnunet.org/drupal/\">" +
    tr("GNUnet community") +
                        "</a>"
                      "</li>"
                      "<li>" +
    tr("Issue tracker: ") +
                        "<a href=\"http://gnunet.org/mantis/\">" +
    tr("Mantis") +
                        "</a>"
                      "</li>"
                    "</ul>"
                  "</font>"
              "</td>"
            "</tr>"
          "</table>"
        "</center>"
      "</body>"
    "</html>");
}

void GAboutPlugin::about()
{
  htmAbout->setHtml(
    "<html>"
      "<body>" +
        header() +
        "<center>"
          "<font face=\"Arial, Helvetica\" size=\"5\"><b>" + tr("About GNUnet") + "</b></font>"
          "<br />"
          "<br />"
          "<table width=\"90%\">"
            "<tr>"
              "<td>"
                "<b><font size=\"4\">" +
                  tr("GNUnet developers") +
                "</font></b>"
                "<ul>"
                  "<li>Christian Grothoff</li>"
                  "<li>Nils Durner</li>"
                  "<li>Milan Bouchet-Valat</li>"
                  "<li>Michael John Wensley</li>"
                "</ul>"
              "</td>"
            "</tr>"
            "<tr>"
              "<td>"
              "</td>"
            "</tr>"
            "<tr>"
              "<td>"
                "<b><font size=\"4\">" +
                  tr("Code contributors") +
                "</font></b>"
                "<ul>"
                  "<li>Anders Carlsson</li>"
                  "<li>Antti Salonen</li>"
                  "<li>Blake Matheny</li>"
                  "<li>Eric Haumant</li>"
                  "<li>Eric Noack</li>"
                  "<li>Gerd Knorr</li>"
                  "<li>Glenn McGrath</li>"
                  "<li>Hendrik Pagenhardt</li>"
                  "<li>Igor Wronsky</li>"
                  "<li>Ioana Patrascu</li>"
                  "<li>James Blackwell</li>"
                  "<li>Jussi Eloranta</li>"
                  "<li>J&uuml;rgen Appel</li>"
                  "<li>Krista Grothoff</li>"
                  "<li>Larry Waldo</li>"
                  "<li>Ludovic Court&egrave;s</li>"
                  "<li>Marko R&auml;ih&auml;</li>"
                  "<li>Paul Ruth</li>"
                  "<li>Renaldo Ferreira</li>"
                  "<li>Risto Saarelma</li>"
                  "<li>Roman Zippel</li>"
                  "<li>Romain Lievin</li>"
                  "<li>Simo Viitanen</li>"
                  "<li>Tiberius Stef</li>"
                  "<li>Tomi Tukiainen</li>"
                  "<li>Tuomas Toivonen</li>"
                  "<li>Tzvetan Horozov</li>"
                  "<li>Uli Luckas</li>"
                  "<li>Vasil Dimov</li>"
                "</ul>"
              "</td>"
            "</tr>"
            "<tr>"
              "<td>"
              "</td>"
            "</tr>"
            "<tr>"
              "<td>"
                "<b><font size=\"4\">" +
                  tr("Artwork") +
                "</font></b>"
                "<ul>"
                  "<li>Christian Muellner</li>"
                  "<li>Alex Jones</li>"
                  "<li>Nicklas Larsson</li>"
                  "<li>Jakub 'jimmac' Steiner</li>"
                  "<li>The Tango Desktop Project</li>"
                "</ul>"
              "</td>"
            "</tr>"
            "<tr>"
              "<td>"
              "</td>"
            "</tr>"
            "<tr>"
              "<td>"
                "<b><font size=\"4\">" +
                  tr("Translators") +
                "</font></b>"
                "<ul>"
                  "<li>Di Ma</li>"
                  "<li>Jens Palsberg</li>"
                  "<li>Christian Grothoff</li>"
                  "<li>Nils Durner</li>"
                  "<li>Mathieu</li>"
                  "<li>Eric Haumant</li>"
                  "<li>Milan Nali</li>"
                  "<li>Hiroshi Yamauchi</li>"
                  "<li>Adam Welc</li>"
                  "<li>Bogdan Carbunar</li>"
                  "<li>Steven Michael Murphy</li>"
                  "<li>Phan Vinh Thinh</li>"
                  "<li>Daniel Nylander</li>"
                "</ul>"
              "</td>"
            "</tr>"
            "<tr>"
              "<td>"
              "</td>"
            "</tr>"
            "<tr>"
              "<td>"
                "<b><font size=\"4\">" +
                  tr("Package maintainers") +
                "</font></b>"
                "<ul>"
                  "<li>Kirill Ponomarew</li>"
                  "<li>Daniel Baumann</li>"
                  "<li>Arnaud Kyheng</li>"
                "</ul>"

              "</td>"
            "</tr>"
          "</table>"
        "</center>"
      "</body>"
    "</html>");
}

void GAboutPlugin::notes()
{
  htmAbout->setHtml(
    "<html>"
      "<body>" +
        header() +
        "<center>"
          "<font face=\"Arial, Helvetica\" size=\"5\"><b>" + tr("Release notes") + "</b></font>"
        "</center>"
      "</body>"
    "</html>");
}

void GAboutPlugin::linkHandler(const QUrl &link)
{
  if (link.toString().startsWith("http"))
  {
    QDesktopServices::openUrl(link);
    welcome();
  }
  else if (link.toString() == "about")
    about();
  else if (link.toString() == "notes")
    notes();
  else
    welcome();
}

extern "C"
{

GNUNETQT_API GPlugin *init_about(GPluginInitParams *params)
{
  Q_UNUSED(params)

  return new GAboutPlugin();
}

GNUNETQT_API void shutdown_about(GPlugin *plugin)
{
  delete (GAboutPlugin *) plugin;
}

} // extern "C"

/* end of about.c */
