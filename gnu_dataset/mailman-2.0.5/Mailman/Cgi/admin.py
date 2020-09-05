# Copyright (C) 1998,1999,2000,2001 by the Free Software Foundation, Inc.
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

"""Process and produce the list-administration options forms.

"""

import sys
import os
import cgi
import string
import types
import rfc822
import signal

from Mailman import Utils
from Mailman import MailList
from Mailman import Errors
from Mailman import MailCommandHandler
from Mailman.htmlformat import *
from Mailman.Crypt import crypt
from Mailman import mm_cfg
from Mailman.Cgi import Auth
from Mailman.Logging.Syslog import syslog


CATEGORIES = [('general', "General Options"),
              ('members', "Membership Management"),
              ('privacy', "Privacy Options"),
              ('nondigest', "Regular-member (non-digest) Options"),
              ('digest', "Digest-member Options"),
              ('bounce', "Bounce Options"),
              ('archive', "Archival Options"),
	      ('gateway', "Mail-News and News-Mail gateways"),
              ('autoreply', 'Auto-responder'),
              ]



def main():
    """Process and produce list options form.

    CGI input indicates that we're returning from submission of some new
    settings, which is processed before producing the new version.

    """
    doc = Document()
    parts = Utils.GetPathPieces()
    if not parts:
        FormatAdminOverview()
        return
    # get the list object
    listname = string.lower(parts[0])
    try: 
        mlist = MailList.MailList(listname, lock=0)
    except Errors.MMListError, e:
        FormatAdminOverview('No such list <em>%s</em>' % listname)
        syslog('error', 'Someone tried to access the admin interface for a '
               'non-existent list: %s' % listname)
        return

    if len(parts) == 1:
        category = 'general'
        category_suffix = ''
    else:
        category = parts[1]
        category_suffix = category

    # If the user is not authenticated, we're done.
    cgidata = cgi.FieldStorage(keep_blank_values=1)
    try:
        Auth.authenticate(mlist, cgidata)
    except Auth.NotLoggedInError, e:
        Auth.loginpage(mlist, 'admin', e.message)
        return

    # Is this a log-out request?
    if category == 'logout':
        print mlist.ZapCookie('admin')
        Auth.loginpage(mlist, 'admin', frontpage=1)
        return

    if category not in map(lambda x: x[0], CATEGORIES):
        category = 'general'

    # is the request for variable details?
    varhelp = None
    if cgidata.has_key('VARHELP'):
        varhelp = cgidata['VARHELP'].value
    elif cgidata.has_key('request_login') and \
         os.environ.get('QUERY_STRING'):
        # POST methods, even if their actions have a query string, don't
        # get put into FieldStorage's keys :-(
        qs = cgi.parse_qs(os.environ['QUERY_STRING']).get('VARHELP')
        if qs and type(qs) == types.ListType:
            varhelp = qs[0]
    if varhelp:
        FormatOptionHelp(doc, varhelp, mlist)
        print doc.Format(bgcolor="#ffffff")
        return

    # From this point on, the MailList object must be locked.  However, we
    # must release the lock no matter how we exit.  try/finally isn't
    # enough, because of this scenario: user hits the admin page which may
    # take a long time to render; user gets bored and hits the browser's
    # STOP button; browser shuts down socket; server tries to write to
    # broken socket and gets a SIGPIPE.  Under Apache 1.3/mod_cgi, Apache
    # catches this SIGPIPE (I presume it is buffering output from the cgi
    # script), then turns around and SIGTERMs the cgi process.  Apache
    # waits three seconds and then SIGKILLs the cgi process.  We /must/
    # catch the SIGTERM and do the most reasonable thing we can in as
    # short a time period as possible.  If we get the SIGKILL we're
    # screwed (because its uncatchable and we'll have no opportunity to
    # clean up after ourselves).
    #
    # This signal handler catches the SIGTERM and unlocks the list.  The
    # effect of this is that the changes made to the MailList object will
    # be aborted, which seems like the only sensible semantics.
    #
    # BAW: This may not be portable to other web servers or cgi execution
    # models.
    def sigterm_handler(signum, frame, mlist=mlist):
        # Make sure the list gets unlocked...
        mlist.Unlock()
        # ...and ensure we exit, otherwise race conditions could cause us to
        # enter MailList.Save() while we're in the unlocked state, and that
        # could be bad!
        sys.exit(0)

    mlist.Lock()
    try:
        # Install the emergency shutdown signal handler
        signal.signal(signal.SIGTERM, sigterm_handler)
        
        if cgidata.has_key('bounce_matching_headers'):
            pairs = mlist.parse_matching_header_opt()

        if len(cgidata.keys()):
            ChangeOptions(mlist, category, cgidata, doc)
            mlist.CheckValues()

        # Sanity checks
        if not mlist.digestable and not mlist.nondigestable:
            AddErrorMessage(doc, '''You have turned off delivery of
            both digest and non-digest messages.  This is an incompatible
            state of affairs.  You must turn on either digest delivery or
            non-digest delivery or your mailing list will basically be
            unusable.''')
	if not mlist.digestable and len(mlist.GetDigestMembers()):
	    AddErrorMessage(doc, '''You have digest members,
            but digests are turned off. Those people will not receive
            mail.''')
	if not mlist.nondigestable and len(mlist.GetMembers()):
	    AddErrorMessage(doc, '''You have regular list members
            but non-digestified mail is turned off.  They will receive mail
            until you fix this problem.''')

	FormatConfiguration(doc, mlist, category, category_suffix, cgidata)
	print doc.Format(bgcolor="#ffffff")
        mlist.Save()
    finally:
        # Now be sure to unlock the list.  It's okay if we get a signal here
        # because essentially, the signal handler will do the same thing.  And
        # unlocking is unconditional, so it's not an error if we unlock while
        # we're already unlocked.
        mlist.Unlock()



# Form Production:
def FormatAdminOverview(error=None):
    "Present a general welcome and itemize the (public) lists."
    doc = Document()
    legend = "%s mailing lists - Admin Links" % mm_cfg.DEFAULT_HOST_NAME
    doc.SetTitle(legend)

    table = Table(border=0, width="100%")
    table.AddRow([Center(Header(2, legend))])
    table.AddCellInfo(max(table.GetCurrentRowIndex(), 0), 0,
                      colspan=2, bgcolor="#99ccff")

    advertised = []
    names = Utils.list_names()
    names.sort()
    for n in names:
	l = MailList.MailList(n, lock=0)
        if l.advertised:
            advertised.append(l)

    if error:
	greeting = FontAttr(error, color="ff5060", size="+1")
    else:
	greeting = "Welcome!"

    if not advertised:
        welcome_items = (greeting,
			 "<p>"
			 " There currently are no publicly-advertised ",
			 Link(mm_cfg.MAILMAN_URL, "mailman"),
			 " mailing lists on %s." % mm_cfg.DEFAULT_HOST_NAME,
			 )
    else:
        welcome_items = (
	    greeting,
            "<p>"
            " Below is the collection of publicly-advertised ",
            Link(mm_cfg.MAILMAN_URL, "mailman"),
            " mailing lists on %s." % mm_cfg.DEFAULT_HOST_NAME,
            (' Click on a list name to visit the configuration pages'
             ' for that list.'
             )
            )

    welcome_items = (welcome_items +
                     (" To visit the administrators configuration page for"
                      " an unadvertised list, open a URL similar to this"
                      +
                      (" one, but with a '/' and the %slist name appended.<p>"
                       % ((error and "right ") or ""))
                      +
                      " General list information can be found at ",
                      Link(Utils.ScriptURL('listinfo'),
                           'the mailing list overview page'),
                      "."
                      "<p>(Send questions and comments to ",
                     Link("mailto:%s" % mm_cfg.MAILMAN_OWNER,
                          mm_cfg.MAILMAN_OWNER),
                     ".)<p>"
                      )
                     )

    table.AddRow([apply(Container, welcome_items)])
    table.AddCellInfo(max(table.GetCurrentRowIndex(), 0), 0, colspan=2)

    if advertised:
        table.AddRow(['&nbsp;', '&nbsp;'])
        table.AddRow([Bold("List"), Bold("Description")])
        for l in advertised:
            table.AddRow(
                [Link(l.GetScriptURL('admin'), Bold(l.real_name)),
                 l.description or Italic('[no description available]'),
                 ])

    doc.AddItem(table)
    doc.AddItem('<hr>')
    doc.AddItem(MailmanLogo())
    print doc.Format(bgcolor="#ffffff")



def FormatConfiguration(doc, mlist, category, category_suffix, cgi_data):
    """Produce the overall doc, *except* any processing error messages."""
    for k, v in CATEGORIES:
        if k == category:
            label = v

    doc.SetTitle('%s Administration (%s)' % (mlist.real_name, label))
    doc.AddItem(Center(Header(2, '%s mailing list administration<br>%s'
                              % (mlist.real_name, label + ' Section'))))
    doc.AddItem('<hr>')

    linktable = Table(valign="top")
    linktable.AddRow([Center(Bold("Configuration Categories")),
                      Center(Bold("Other Administrative Activities"))])

    adminurl = mlist.GetScriptURL('admin')

    otherlinks = UnorderedList()
    otherlinks.AddItem(Link(mlist.GetScriptURL('admindb'), 
                            'Tend to pending administrative requests'))
    otherlinks.AddItem(Link(mlist.GetScriptURL('listinfo'),
                            'Go to the general list information page'))
    otherlinks.AddItem(Link(mlist.GetScriptURL('edithtml'),
                            'Edit the HTML for the public list pages'))
    otherlinks.AddItem(Link(mlist.GetBaseArchiveURL(), 'Go to list archives'))
    otherlinks.AddItem(Link('%s/logout' % adminurl,
                            # TBD: What I really want is a blank line :/
                            '<FONT SIZE="+2"><b>Logout</b></FONT>'))

    categorylinks = UnorderedList()
    for k, v in CATEGORIES:
        if k == category:
            categorylinks.AddItem("<em>%s</em>" % v)
        else:
            categorylinks.AddItem(Link("%s/%s" % (adminurl, k), v))

    linktable.AddRow([categorylinks, otherlinks])
    linktable.AddRowInfo(max(linktable.GetCurrentRowIndex(), 0),
                         valign="top")

    doc.AddItem(linktable)
    doc.AddItem('<hr>')
    if category_suffix:
        encoding = None
        if category_suffix == 'autoreply':
            # these have file uploads
            encoding = 'multipart/form-data'
        form = Form('%s/%s' % (adminurl, category_suffix), encoding=encoding)
    else:
        form = Form(adminurl)
    doc.AddItem(form)

    if category == 'general':
        andpassmsg = "  (You can change your password there, too.)"
    else:
        andpassmsg = ""
    form.AddItem("Make your changes below, and then submit them"
                 " using the button at the bottom.%s<p>"
                 % andpassmsg)

    form.AddItem(FormatOptionsSection(category, mlist, cgi_data))

    if category == 'general':
        form.AddItem(Center(FormatPasswordStuff()))

    form.AddItem("<p>")
    form.AddItem(Center(FormatSubmit()))
    form.AddItem(mlist.GetMailmanFooter())



def FormatOptionsSection(category, mlist, cgi_data):
    """Produce the category-specific options table."""
    if category == 'members':
        # Special case for members section.
        return FormatMembershipOptions(mlist, cgi_data)

    options = GetConfigOptions(mlist, category)

    big_table = Table(cellspacing=3, cellpadding=4)

    # Get and portray the text label for the category.
    for k, v in CATEGORIES:
        if k == category:
            label = v
    big_table.AddRow([Center(Header(2, label))])
    big_table.AddCellInfo(max(big_table.GetCurrentRowIndex(), 0), 0,
                          colspan=2, bgcolor="#99ccff")

    def ColHeader(big_table = big_table):
        big_table.AddRow([Center(Bold('Description')), Center(Bold('Value'))])
        big_table.AddCellInfo(max(big_table.GetCurrentRowIndex(), 0), 0,
                              width="15%")
        big_table.AddCellInfo(max(big_table.GetCurrentRowIndex(), 0), 1,
                              width="85%")
    did_col_header = 0

    for item in options:
        if type(item) == types.StringType:
	    # The very first banner option (string in an options list) is
	    # treated as a general description, while any others are
	    # treated as section headers - centered and italicized...
	    if did_col_header:
		item = "<center><i>" + item + "</i></center>"
            big_table.AddRow([item])
	    big_table.AddCellInfo(max(big_table.GetCurrentRowIndex(), 0),
				  0, colspan=2)
            if not did_col_header:
                # Do col header after very first string descr, if any...
                ColHeader()
                did_col_header = 1
        else:
            if not did_col_header:
                # ... but do col header before anything else.
                ColHeader()
                did_col_header = 1
	    AddOptionsTableItem(big_table, item, category, mlist)
    big_table.AddRow(['<br>'])
    big_table.AddCellInfo(big_table.GetCurrentRowIndex(), 0, colspan=2)
    return big_table



def AddOptionsTableItem(table, item, category, mlist, detailsp=1):
    """Add a row to an options table with the item description and value."""
    try:
	got = GetItemCharacteristics(item)
	varname, kind, params, dependancies, descr, elaboration = got
    except ValueError, msg:
        syslog('error', 'admin: %s' % msg)
        return Italic("<malformed option>")
    descr = GetItemGuiDescr(mlist, category, varname, descr, detailsp)
    val = GetItemGuiValue(mlist, kind, varname, params)
    table.AddRow([descr, val])
    table.AddCellInfo(max(table.GetCurrentRowIndex(), 0), 1,
		      bgcolor="#cccccc")
    table.AddCellInfo(max(table.GetCurrentRowIndex(), 0), 0,
		      bgcolor="#cccccc")



def FormatOptionHelp(doc, varref, mlist):
    item = None
    reflist = string.split(varref, '/')
    if len(reflist) == 2:
        category, varname = reflist
        options = GetConfigOptions(mlist, category)
        for i in options:
            if i and i[0] == varname:
                item = i
                break
    if not item:
        bad = 'Option %s/%s not found: %s' % (
            category, varname, os.environ.get('PATH_INFO'))
        AddErrorMessage(doc, bad)
        return
    got = GetItemCharacteristics(item)
    try:
        varname, kind, params, dependancies, descr, elaboration = got
        if elaboration is None:
            elaboration = ''
    except ValueError, msg:
        varname, kind, params, dependancies, descr = got
        elaboration = descr
    header = Table(width="100%")
    legend = ('%s Mailing list Configuration Help<br><em>%s</em> Option'
	      % (mlist.real_name, varname))
    header.AddRow([Center(Header(3, legend))])
    header.AddCellInfo(max(header.GetCurrentRowIndex(), 0), 0,
                       colspan=2, bgcolor="#99ccff")
    doc.SetTitle("Mailman %s List Option Help" % varname)
    doc.AddItem(header)
    doc.AddItem("<b>%s</b> (%s): %s<p>" % (varname, category, descr))
    doc.AddItem("%s<p>" % elaboration)

    form = Form("%s/%s" % (mlist.GetScriptURL('admin'), category))
    valtab = Table(cellspacing=3, cellpadding=4)
    AddOptionsTableItem(valtab, item, category, mlist, detailsp=0)
    form.AddItem(valtab)
    form.AddItem('<p>')
    form.AddItem(Center(FormatSubmit()))
    doc.AddItem(Center(form))
    doc.AddItem("""<em><strong>Warning:</strong> changing this option here
    could cause other screens to be out-of-sync.  Be sure to reload any other
    pages that are displaying this option for this mailing list.  You can
    also """)
    doc.AddItem(Link('%s/%s' % (mlist.GetScriptURL('admin'), category),
                     'return to the ' + category + ' options page.'))
    doc.AddItem('</em>')
    doc.AddItem(mlist.GetMailmanFooter())



def GetItemCharacteristics(table_entry):
    """Break out the components of an item description from its table entry:
      0 option-var name
      1 type
      2 entry size
      3 ?dependancies?
      4 Brief description
      5 Optional description elaboration"""    
    if len(table_entry) == 5:
        elaboration = None
        varname, kind, params, dependancies, descr = table_entry
    elif len(table_entry) == 6:
        varname, kind, params, dependancies, descr, elaboration = table_entry
    else:
	raise ValueError, ("Badly formed options entry:\n  %s"
			   % table_entry)
    return (varname, kind, params, dependancies, descr, elaboration)



def GetItemGuiValue(mlist, kind, varname, params):
    """Return a representation of an item's settings."""
    if kind == mm_cfg.Radio or kind == mm_cfg.Toggle:
        #
        # if we are sending returning the option for subscribe
        # policy and this site doesn't allow open subscribes,
        # then we have to alter the value of mlist.subscribe_policy
        # as passed to RadioButtonArray in order to compensate
        # for the fact that there is one fewer option. correspondingly,
        # we alter the value back in the change options function -scott
        #
        # TBD: this is an ugly ugly hack.
        if varname[0] == '_':
            checked = 0
        else:
            checked = getattr(mlist, varname)
        if varname == 'subscribe_policy' and not mm_cfg.ALLOW_OPEN_SUBSCRIBE:
            checked = checked - 1
        return RadioButtonArray(varname, params, checked)
    elif (kind == mm_cfg.String or kind == mm_cfg.Email or
	  kind == mm_cfg.Host or kind == mm_cfg.Number):
	return TextBox(varname, getattr(mlist, varname), params)
    elif kind == mm_cfg.Text:
	if params:
	    r, c = params
	else:
	    r, c = None, None
	val = getattr(mlist, varname)
	if not val:
	    val = ''
	return TextArea(varname, val, r, c)
    elif kind == mm_cfg.EmailList:
	if params:
	    r, c = params
	else:
	    r, c = None, None
	res = string.join(getattr(mlist, varname), '\n')
	return TextArea(varname, res, r, c, wrap='off')
    elif kind == mm_cfg.FileUpload:
        # like a text area, but also with uploading
        if params:
            r, c = params
        else:
            r, c = None, None
        val = getattr(mlist, varname)
        if not val:
            val = ''
        container = Container()
        container.AddItem('<em>Enter the text below, or...</em><br>')
        container.AddItem(TextArea(varname, val, r, c))
        container.AddItem('<br><em>...specify a file to upload</em><br>')
        container.AddItem(FileUpload(varname+'_upload', r, c))
        return container


def GetItemGuiDescr(mlist, category, varname, descr, detailsp):
    """Return the item's description, with link to details.

    Details are not included if this is a VARHELP page, because that /is/ the
    details page!
    """
    if detailsp:
        text = Container('<div ALIGN="right">' + descr + ' ',
                     Link(mlist.GetScriptURL('admin')
                              + '/?VARHELP=' + category + '/' + varname,
                          '(Details)'),
                     '</div>').Format()
    else:
        text = '<div ALIGN="right">' + descr + '</div>'
    if varname[0] == '_':
        text = text + '''<div ALIGN="right"><br><em><strong>Note:</strong>
        setting this value performs an immediate action but does not modify
        permanent state.</em></div>'''
    return text



def FormatMembershipOptions(mlist, cgi_data):
    container = Container()
    header = Table(width="100%")
    header.AddRow([Center(Header(2, "Membership Management"))])
    header.AddCellInfo(max(header.GetCurrentRowIndex(), 0), 0,
                       colspan=2, bgcolor="#99ccff")
    container.AddItem(header)
    user_table = Table(width="90%", border='2')
    user_table.AddRow([Center(Header(4, "Membership List"))])
    user_table.AddCellInfo(user_table.GetCurrentRowIndex(),
                           user_table.GetCurrentCellIndex(),
                           bgcolor="#cccccc", colspan=8)
    user_table.AddRow(
        [Center(Italic("(%s members total, max. %s at a time displayed)" %
                       (len(mlist.members)
                        + len(mlist.digest_members),
                        mlist.admin_member_chunksize)))])
    user_table.AddCellInfo(user_table.GetCurrentRowIndex(),
                           user_table.GetCurrentCellIndex(),
                           bgcolor="#cccccc", colspan=8)

    user_table.AddRow(map(Center, ['member address', 'subscr',
                                   'hide', 'nomail', 'ack', 'not metoo',
                                   'digest', 'plain']))
    rowindex = user_table.GetCurrentRowIndex()
    for i in range(8):
        user_table.AddCellInfo(rowindex, i, bgcolor='#cccccc')
    all = mlist.GetMembers() + mlist.GetDigestMembers()
    if len(all) > mlist.admin_member_chunksize:
        chunks = Utils.chunkify(all, mlist.admin_member_chunksize)
        if not cgi_data.has_key("chunk"):
            chunk = 0
        else:
            chunk = string.atoi(cgi_data["chunk"].value)
        all = chunks[chunk]
        footer = ("<p><em>To View other sections, "
                  "click on the appropriate range listed below</em>")
        chunk_indices = range(len(chunks))
        chunk_indices.remove(chunk)
        buttons = []
        for ci in chunk_indices:
            start, end = chunks[ci][0], chunks[ci][-1]
	    url = mlist.GetScriptURL('admin')
            buttons.append("<a href=%s/members?chunk=%d> from %s to %s </a>"
                           % (url, ci, start, end))
        buttons = apply(UnorderedList, tuple(buttons))
        footer = footer + buttons.Format() + "<p>"
    else:
        all.sort()
        footer = "<p>"
    for member in all:
        mtext = '<a href="%s">%s</a>' % (
            mlist.GetOptionsURL(member, obscure=1),
            mlist.GetUserSubscribedAddress(member))
        cells = [mtext + "<input type=hidden name=user value=%s>" % (member),
                 Center(CheckBox(member + "_subscribed", "on", 1).Format())]
        for opt in ("hide", "nomail", "ack", "notmetoo"):
            if mlist.GetUserOption(member,MailCommandHandler.option_info[opt]):
                value = "on"
                checked = 1
            else:
                value = "off"
                checked = 0
            box = CheckBox("%s_%s" % (member, opt), value, checked)
            cells.append(Center(box.Format()))
        if mlist.members.has_key(member):
            cells.append(Center(CheckBox(member + "_digest",
                                         "off", 0).Format()))
        else:
            cells.append(Center(CheckBox(member + "_digest",
                                         "on", 1).Format()))
        if mlist.GetUserOption(member,MailCommandHandler.option_info['plain']):
            value = 'on'
            checked = 1
        else:
            value = 'off'
            checked = 0
        cells.append(Center(CheckBox('%s_plain' % member, value, checked)))
        user_table.AddRow(cells)
    container.AddItem(Center(user_table))
    legend = UnorderedList()
    legend.AddItem('<b>subscr</b> -- Is the member subscribed?')
    legend.AddItem("<b>hide</b> -- Is the member's address "
                   "concealed on the list of subscribers?")
    legend.AddItem('<b>nomail</b> -- Is delivery to the member disabled?')
    legend.AddItem('<b>ack</b> -- '
                   'Does the member get acknowledgements of their posts?')
    legend.AddItem('<b>not metoo</b> -- '
                   'Does the member avoid copies of their own posts?')
    legend.AddItem('<b>digest</b> -- '
                   'Does the member get messages in digests? '
                   '(otherwise, individual messages)')
    legend.AddItem(
        '<b>plain</b> -- '
        'If getting digests, does the member get plain text digests? '
        '(otherwise, MIME)')
    container.AddItem(legend.Format())
    container.AddItem(footer)
    t = Table(width="90%")
    t.AddRow([Center(Header(4, "Mass Subscribe Members"))])
    t.AddCellInfo(t.GetCurrentRowIndex(),
                  t.GetCurrentCellIndex(),
                  bgcolor="#cccccc", colspan=8)
    if mlist.send_welcome_msg:
        nochecked = 0
        yeschecked = 1
    else:
        nochecked = 1
        yeschecked = 0
    t.AddRow([("<b>1.</b> Send Welcome message to this batch? "
               + RadioButton("send_welcome_msg_to_this_batch", 0,
                             nochecked).Format()
               + " no "
               + RadioButton("send_welcome_msg_to_this_batch", 1,
                             yeschecked).Format()
               + " yes ")])
    t.AddRow(["<b>2.</b> Enter one address per line: <p>"])
    container.AddItem(Center(t))
    container.AddItem(Center(TextArea(name='subscribees',
                                      rows=10,cols=60,wrap=None)))
    return container



def FormatPasswordStuff():
    change_pw_table = Table(bgcolor="#99cccc", border=0,
                            cellspacing=0, cellpadding=2,
                            valign="top")
    change_pw_table.AddRow(
        [Bold(Center('To Change The Administrator Password'))])
    change_pw_table.AddCellInfo(0, 0, align="left", colspan=2)
    old = Table(bgcolor="#99cccc", border=1,
                cellspacing=0, cellpadding=2, valign="top")
    old.AddRow(['<div ALIGN="right"> Enter current password:</div>',
                PasswordBox('adminpw')])
    new = Table(bgcolor="#99cccc", border=1,
                cellspacing=0, cellpadding=2, valign="top")
    new.AddRow(['<div ALIGN="right"> Enter new password: </div>',
                PasswordBox('newpw')])
    new.AddRow(['<div ALIGN="right">Confirm new password:</div>',
                PasswordBox('confirmpw')])
    change_pw_table.AddRow([old, new])
    change_pw_table.AddCellInfo(1, 0, align="left", valign="top")
    #change_pw_table.AddCellInfo(1, 1, align="left", valign="top")
    return change_pw_table



def FormatSubmit():
    submit = Table(bgcolor="#99ccff",
                   border=0, cellspacing=0, cellpadding=2)
    submit.AddRow([Bold(SubmitButton('submit', 'Submit Your Changes'))])
    submit.AddCellInfo(submit.GetCurrentRowIndex(), 0, align="middle")
    return submit



# XXX klm - looks like turn_on_moderation is orphaned.
#turn_on_moderation = 0

# Options processing
def GetValidValue(mlist, prop, my_type, val, dependant):
    if my_type == mm_cfg.Radio or my_type == mm_cfg.Toggle:
	if type(val) <> types.IntType:
            try:
                val = int(val)
            except ValueError:
		pass
		# Don't know what to do here...
	    return val
    elif my_type == mm_cfg.String or my_type == mm_cfg.Text:
	return val
    elif my_type == mm_cfg.Email:
	try:
            Utils.ValidateEmail(val)
            return val
        except Errors.EmailAddressError:
            # TBD: should have a way of displaying the results of the
            # operation.
            pass
	# Revert to the old value.
	return getattr(mlist, prop)
    elif my_type == mm_cfg.EmailList:
	def SafeValidAddr(addr):
	    try:
                Utils.ValidateEmail(addr)
                return 1
            except Errors.EmailAddressError:
                return 0

	val = filter(SafeValidAddr,
		     map(string.strip, string.split(val, '\n')))
##	if dependant and len(val):
##	    # Wait till we've set everything to turn it on,
##	    # as we don't want to clobber our special case.
##	    # XXX klm - looks like turn_on_moderation is orphaned?
##	    turn_on_moderation = 1
	return val
    elif my_type == mm_cfg.Host:
	return val
##
##      This code is sendmail dependant, so we'll just live w/o 
##      the error checking for now.
##
## 	# Shouldn't have to read in the whole file.
## 	file = open('/etc/sendmail.cf', 'r')
## 	lines = string.split(file.read(), '\n')
## 	file.close()
## 	def ConfirmCWEntry(item):
## 	    return item[0:2] == 'Cw'
## 	lines = filter(ConfirmCWEntry, lines)
## 	if not len(lines):
## 	    # Revert to the old value.
## 	    return getattr(list, prop)
## 	for line in lines:
## 	    if string.lower(string.strip(line[2:])) == string.lower(val):
## 		return val
## 	return getattr(list, prop)
    elif my_type == mm_cfg.Number:
        num = -1
        try:
            num = int(val)
        except ValueError:
            # TBD: a float???
            try:
                num = float(val)
            except ValueError:
                pass
        if num < 0:
            return getattr(mlist, prop)
        return num
    else:
	# Should never get here...
	return val



def ChangeOptions(mlist, category, cgi_info, document):
    confirmed = 0
    if cgi_info.has_key('newpw'):
	if cgi_info.has_key('confirmpw'):
            if cgi_info.has_key('adminpw') and cgi_info['adminpw'].value:
                try:
                    mlist.ConfirmAdminPassword(cgi_info['adminpw'].value)
                    confirmed = 1
                except Errors.MMBadPasswordError:
                    AddErrorMessage(document,
                                    'Incorrect administrator password',
                                    tag='Error: ')
            if confirmed:
                new = string.strip(cgi_info['newpw'].value)
                confirm = string.strip(cgi_info['confirmpw'].value)
                if new == '' and confirm == '':
                    AddErrorMessage(document,
                                    'Empty admin passwords are not allowed',
                                    tag='Error: ')
                elif new == confirm:
                    mlist.password = crypt(new, Utils.GetRandomSeed())
                    # Re-authenticate (to set new cookie)
                    mlist.WebAuthenticate(password=new, cookie='admin')
                else:
                    AddErrorMessage(document, 'Passwords did not match',
                                    tag='Error: ')
	else:
            AddErrorMessage(document,
                            'You must type in your new password twice',
                            tag='Error: ')
    #
    # for some reason, the login page mangles important values for the list
    # such as .real_name so we only process these changes if the category
    # is not "members" and the request is not from the login page
    # -scott 19980515
    #
    if category != 'members' and \
            not cgi_info.has_key("request_login") and \
            len(cgi_info.keys()) > 1:
        # then
        if cgi_info.has_key("subscribe_policy"):
            if not mm_cfg.ALLOW_OPEN_SUBSCRIBE:
                #
                # we have to add one to the value because the
                # page didn't present an open list as an option
                #
                page_setting = string.atoi(cgi_info["subscribe_policy"].value)
                cgi_info["subscribe_policy"].value = str(page_setting + 1)
        opt_list = GetConfigOptions(mlist, category)
        for item in opt_list:
            if type(item) <> types.TupleType or len(item) < 5:
                continue
            property, kind, args, deps, desc = item[0:5]
            if cgi_info.has_key(property+'_upload') and \
                   cgi_info[property+'_upload'].value:
                val = cgi_info[property+'_upload'].value
            elif not cgi_info.has_key(property):
                continue
            else:
                val = cgi_info[property].value
            value = GetValidValue(mlist, property, kind, val, deps)
            #
            # This is an ugly, ugly hack
            if property[0] == '_':
                # TBD: When turning on usenet->mail gating we want to
                # automatically catch up the newsgroup otherwise the mailing
                # list will suddently get flooded.  There should be a much
                # better way to do this (or for the admin to specify they want
                # this).
                if property == '_mass_catchup' and value:
                    mlist.usenet_watermark = None
            elif getattr(mlist, property) <> value:
                # TBD: Ensure that mlist.real_name differs only in letter
                # case.  Otherwise a security hole can potentially be opened
                # when using an external archiver.  This seems ad-hoc and
                # could use a more general security policy.
                if property == 'real_name' and \
                   string.lower(value) <> string.lower(mlist._internal_name):
                    # then don't install this value.
                    document.AddItem("""<p><b>real_name</b> attribute not
                    changed!  It must differ from the list's name by case
                    only.<p>""")
                    continue
                setattr(mlist, property, value)
    #
    # mass subscription processing for members category
    #
    def clean_names(name):
        return rfc822.unquote(string.strip(name))

    if cgi_info.has_key('subscribees'):
	name_text = cgi_info['subscribees'].value
        name_text = string.replace(name_text, '\r', '')
	names = filter(None, map(clean_names, string.split(name_text, '\n')))
        send_welcome_msg = string.atoi(
            cgi_info["send_welcome_msg_to_this_batch"].value)
        digest = 0
        if not mlist.digestable:
            digest = 0
        if not mlist.nondigestable:
            digest = 1
        subscribe_errors = []
        subscribe_success = []
        result = mlist.ApprovedAddMembers(names, None,
                                        digest, send_welcome_msg)
        for name in result.keys():
            if result[name] is None:
                subscribe_success.append(name)
            else:
                # `name' was not subscribed, find out why.  On failures,
                # result[name] is set from sys.exc_info()[:2]
                e, v = result[name]
                if e is Errors.MMAlreadyAMember:
                    subscribe_errors.append((name, 'Already a member'))
                elif e is Errors.MMBadEmailError:
                    if name == '':
                        name = '&lt;blank line&gt;'
                    subscribe_errors.append(
                        (name, "Bad/Invalid email address"))
                elif e is Errors.MMHostileAddress:
                    subscribe_errors.append(
                        (name, "Hostile Address (illegal characters)"))
        if subscribe_success:
            document.AddItem(Header(5, "Successfully Subscribed:"))
            document.AddItem(apply(UnorderedList, tuple((subscribe_success))))
            document.AddItem("<p>")
            # ApprovedAddMembers will already have saved the list for us.
        if subscribe_errors:
            document.AddItem(Header(5, "Error Subscribing:"))
            items = map(lambda x: "%s -- %s" % (x[0], x[1]), subscribe_errors)
            document.AddItem(apply(UnorderedList, tuple((items))))
            document.AddItem("<p>")
    #
    # do the user options for members category
    #
    if cgi_info.has_key('user'):
        user = cgi_info["user"]
        if type(user) is type([]):
            users = []
            for ui in range(len(user)):
                users.append(user[ui].value)
        else:
            users = [user.value]
        errors = []
        for user in users:
            if not cgi_info.has_key('%s_subscribed' % (user)):
                try:
                    mlist.DeleteMember(user)
                except Errors.MMNoSuchUserError:
                    errors.append((user, 'Not subscribed'))
                continue
            value = cgi_info.has_key('%s_digest' % user)
            try:
                mlist.SetUserDigest(user, value, force=1)
            except (Errors.MMNotAMemberError,
                    Errors.MMAlreadyDigested,
                    Errors.MMAlreadyUndigested):
                pass
            for opt in ("hide", "nomail", "ack", "notmetoo", "plain"):
                opt_code = MailCommandHandler.option_info[opt]
                if cgi_info.has_key("%s_%s" % (user, opt)):
                    mlist.SetUserOption(user, opt_code, 1, save_list=0)
                else:
                    mlist.SetUserOption(user, opt_code, 0, save_list=0)
        if errors:
            document.AddItem(Header(5, "Error Unsubscribing:"))
            items = map(lambda x: "%s -- %s" % (x[0], x[1]), errors)
            document.AddItem(apply(UnorderedList, tuple((items))))
            document.AddItem("<p>")



def AddErrorMessage(doc, errmsg, tag='Warning: ', *args):
    doc.AddItem(Header(3, Bold(FontAttr(
        tag, color="#ff0000", size="+2")).Format() +
                       Italic(errmsg % args).Format()))



def GetConfigOptions(mlist, category):
    return mlist.GetConfigInfo()[category]
