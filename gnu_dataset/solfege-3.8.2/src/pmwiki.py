# GNU Solfege - free ear training software
# Copyright (C) 2005, 2007 Tom Cato Amundsen
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

import urllib
import urllib2
import time

class PmWiki:
    def __init__(self, url):
        self.m_url = url
    def write_page(self, pagename, text, author):
        """
        Can raise an urllib2.URLError.
        """
        params = urllib.urlencode({
            'action': 'edit',
            'pagename': pagename,
            'text': text,
            'author': author,
            'basetime': int(time.time()),
            'post': 'Save',
        })
        u = urllib2.urlopen(self.m_url, params)
        return u
    def page_exists(self, pagename):
        u = urllib2.urlopen(self.m_url+'/'+pagename+"?action=source")
        s = u.read()
        u.close()
        return len(s) > 0


def main():
    w = PmWiki("http://localhost/~tom/pmwiki/pmwiki.php")
    r = w.write_page('SITS-Incoming.TestPage', 'This is the test page. Time: %f' % time.time(), 'tomcato');
    print "Does exist:", w.page_exists("Main.TestPage")
    print "Does not exist:", not w.page_exists("Main.TestPageXX")

if __name__ == '__main__':
    main()
