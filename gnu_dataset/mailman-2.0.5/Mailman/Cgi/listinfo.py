# Copyright (C) 1998,1999,2000 by the Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software 
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

"""Produce listinfo page, primary web entry-point to mailing lists.
"""

# No lock needed in this script, because we don't change data.

import os
import string

from Mailman import mm_cfg
from Mailman import Utils
from Mailman import MailList
from Mailman import Errors
from Mailman.htmlformat import *
from Mailman.Logging.Syslog import syslog



def main():
    parts = Utils.GetPathPieces()
    if not parts:
        FormatListinfoOverview()
        return

    listname = string.lower(parts[0])
    try:
        mlist = MailList.MailList(listname, lock=0)
    except Errors.MMListError, e:
        FormatListinfoOverview('No such list <em>%s</em>' % listname)
        syslog('error', 'listinfo: no such list "%s": %s' % (listname, e))
        return

    FormatListListinfo(mlist)



def FormatListinfoOverview(error=None):
    "Present a general welcome and itemize the (public) lists for this host."

    # XXX We need a portable way to determine the host by which we are being 
    #     visited!  An absolute URL would do...
    http_host = os.environ.get('HTTP_HOST', os.environ.get('SERVER_NAME'))
    port = os.environ.get('SERVER_PORT')
    # strip off the port if there is one
    if port and http_host[-len(port)-1:] == ':'+port:
        http_host = http_host[:-len(port)-1]
    if mm_cfg.VIRTUAL_HOST_OVERVIEW and http_host:
	host_name = http_host
    else:
	host_name = mm_cfg.DEFAULT_HOST_NAME

    doc = Document()
    legend = "%s Mailing Lists" % host_name
    doc.SetTitle(legend)

    table = Table(border=0, width="100%")
    table.AddRow([Center(Header(2, legend))])
    table.AddCellInfo(max(table.GetCurrentRowIndex(), 0), 0,
                      colspan=2, bgcolor="#99ccff")

    advertised = []
    names = Utils.list_names()
    names.sort()

    for n in names:
	mlist = MailList.MailList(n, lock=0)
	if mlist.advertised:
	    if mm_cfg.VIRTUAL_HOST_OVERVIEW and \
                    http_host and \
                    string.find(http_host, mlist.web_page_url) == -1 and \
                    string.find(mlist.web_page_url, http_host) == -1:
		# List is for different identity of this host - skip it.
		continue
	    else:
		advertised.append(mlist)

    if error:
	greeting = FontAttr(error, color="ff5060", size="+1")
    else:
	greeting = FontAttr('Welcome!', size='+2')

    if not advertised:
        welcome_items = (greeting,
			 "<p>"
			 " There currently are no publicly-advertised ",
			 Link(mm_cfg.MAILMAN_URL, "mailman"),
			 " mailing lists on %s." % host_name,
			 )
    else:
        welcome_items = (
	    greeting,
            '''<p>Below is a listing of all the public mailing lists on
            %(hostname)s.  Click on a list name to get more information about
            the list, or to subscribe, unsubscribe, and change the preferences
            on your subscription.''' % {'hostname': host_name},
            )

    welcome_items = (welcome_items +
                     (" To visit the info page for an unadvertised list,"
                      " open a URL similar to this one, but with a '/' and"
                      +
                      (" the %slist name appended."
                       % ((error and "right ") or ""))
                      +
                      '<p> List administrators, you can visit ',
                      Link(Utils.ScriptURL('admin'),
                           'the list admin overview page'),
                      " to find the management interface for your list."
                      "<p>(Send questions or comments to ",
                      Link("mailto:%s" % mm_cfg.MAILMAN_OWNER,
                           mm_cfg.MAILMAN_OWNER),
                      ".)<p>"))

    table.AddRow([apply(Container, welcome_items)])
    table.AddCellInfo(max(table.GetCurrentRowIndex(), 0), 0, colspan=2)

    if advertised:
        table.AddRow(['&nbsp;', '&nbsp;'])
        table.AddRow([Bold(FontAttr('List', size='+2')),
                      Bold(FontAttr('Description', size='+2'))
                      ])
    for mlist in advertised:
        table.AddRow(
            [Link(mlist.GetScriptURL('listinfo'), Bold(mlist.real_name)),
             mlist.description or Italic('[no description available]')])

    doc.AddItem(table)
    doc.AddItem('<hr>')
    doc.AddItem(MailmanLogo())
    print doc.Format(bgcolor="#ffffff")



def FormatListListinfo(mlist):
    "Expand the listinfo template against the list's settings, and print."

    doc = HeadlessDocument()

    replacements = mlist.GetStandardReplacements()

    if not mlist.digestable or not mlist.nondigestable:
        replacements['<mm-digest-radio-button>'] = ""
        replacements['<mm-undigest-radio-button>'] = ""
    else:
        replacements['<mm-digest-radio-button>'] = mlist.FormatDigestButton()
        replacements['<mm-undigest-radio-button>'] = \
                                                   mlist.FormatUndigestButton()
    replacements['<mm-plain-digests-button>'] = \
                                              mlist.FormatPlainDigestsButton()
    replacements['<mm-mime-digests-button>'] = mlist.FormatMimeDigestsButton()
    replacements['<mm-subscribe-box>'] = mlist.FormatBox('email', size=30)
    replacements['<mm-subscribe-button>'] = mlist.FormatButton(
        'email-button', text='Subscribe')
    replacements['<mm-new-password-box>'] = mlist.FormatSecureBox('pw')
    replacements['<mm-confirm-password>'] = mlist.FormatSecureBox('pw-conf')
    replacements['<mm-subscribe-form-start>'] = mlist.FormatFormStart(
        'subscribe')
    replacements['<mm-roster-form-start>'] = mlist.FormatFormStart('roster')
    replacements['<mm-editing-options>'] = mlist.FormatEditingOption()
    replacements['<mm-info-button>'] = SubmitButton('UserOptions',
                                                    'Edit Options').Format()
    replacements['<mm-roster-option>'] = mlist.FormatRosterOptionForUser()

    # Do the expansion.
    doc.AddItem(mlist.ParseTags('listinfo.html', replacements))
    print doc.Format()



if __name__ == "__main__":
    main()
