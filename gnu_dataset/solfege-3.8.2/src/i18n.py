# GNU Solfege - free ear training software
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2007  Tom Cato Amundsen
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin ST, Fifth Floor, Boston, MA  02110-1301  USA

"""
Modules that have translated messages need to import this module
to make pydoc work.
"""
import gettext
import locale
import os
import sys

def _i(s):
    """
    used for translated strings with a prefix, like these:
    _("interall|m3")
    _("View-menu|_Toolbar")
    This function is required because if a entry is not translated, then
    only the string after | should be returned. 
    """
    ns = _(s)
    if ns == s:
        return "%s" %(s.split('|')[-1])
    else:
        return "%s" % ns

def langs():
    ret = []
    for k in ('LANGUAGE', 'LC_ALL', 'LC_MESSAGES', 'LANG'):
        if k in os.environ:
            v = os.environ.get(k)
            if v:
                ret = v.split(':')
            break
    if 'C' not in ret:
        ret.append('C')
    retval = []
    for l in ret:
        s = locale.normalize(l)
        if len(s) >= 5 and s[2] == '_':
            retval.append(s[:5])
            retval.append(s[:2])
        else:
            retval.append(s)
    return retval

def setup(prefix, config_locale=None):
    # Gettext and gtk+ chech the variables in this order.
    locale.setlocale(locale.LC_ALL, '')
    varlist = ('LANGUAGE', 'LC_MESSAGES')
    if not config_locale:
        config_locale = 'system default'
    if config_locale != 'system default':
        for n in varlist:
            os.environ[n] = config_locale
    # FIXME can we remove this whole if block, not that set run
    # locale.setlocale(locale.LC_ALL, '') at program start??
    if (sys.platform == 'win32') and (config_locale == 'system default'):
        envar = None
        for varname in varlist:
            if varname in os.environ:
                envar = varname
                break
        if not envar:
            # We have to set the value ourselves if we don't have
            # a environment variable set.
            s = locale.getdefaultlocale()[0]
            if s == 'nb_NO':
                s = 'no_NO'
            if s:
                s = locale.normalize(s)
                os.environ['LANGUAGE'] = s
    gettext.install('solfege', os.path.join(prefix, 'share', 'locale'), unicode=1)
    __builtins__['_i'] = _i

