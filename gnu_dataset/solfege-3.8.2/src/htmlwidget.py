# -*- coding: iso-8859-1 -*-
# GNU Solfege - free ear training software
# Copyright (C) 2000, 2001, 2004, 2006, 2007  Tom Cato Amundsen
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

if __name__ == '__main__':
    import pygtk
    pygtk.require("2.0")
    import sys
    sys.path.append('src')
    import gtkhtml2

debug_flag = ()

import copy
import gobject
import sys
import gtk, pango
import runtime

#gtkhtml2.gtk_html_context_get().set_property('debug-painting', 1)

import htmllib, formatter
import i18n
from HTMLParser import HTMLParser
import weakref

import re, os
import cfg

import tree

def startswith_whitespace(s):
    return not s == s.lstrip()

def endswith_whitespace(s):
    return not s == s.rstrip()

def url_decode(url):
    def repf(s):
            return chr(int(s.group()[1:], 16))
    return re.sub("%[0-9A-F][0-9A-F]", repf, url)

class _HtmlWidgetCommon(object):
    def __init__(self):
        self.m_document_wd = None
    def on_key_press_event(self, *argv):
        """
        Will be used to implement scrolling when HTML is available
        """
        pass
    def read_file_abs(self, filename):
        self.m_document_wd = os.path.dirname(filename)
        self.source(self.read_and_encode_file(filename))
    def read_file_rel(self, filename):
        assert self.m_document_wd
        fn = os.path.join(self.m_document_wd, filename)
        self.m_document_wd = os.path.dirname(fn)
        self.source(self.read_and_encode_file(fn))
    def read_and_encode_file(self, filename):
        """
        Load the file named and make an unicode string of it.
        """
        s = open(filename, 'r').read()
        r = re.compile('<meta.*?\scontent="(?P<content>.*?charset=(?P<charset>.*?)")')
        m = r.search(s)
        if (not m) or (not m.group('charset')):
            charset = 'ascii'
        else:
            charset = m.group('charset')
        return s.decode(charset, 'replace')


class GtkHtml2Widget(gtk.ScrolledWindow, _HtmlWidgetCommon):
    def __init__(self, activate_cb, anchor_track_cb):
        gtk.ScrolledWindow.__init__(self)
        _HtmlWidgetCommon.__init__(self)
        self.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        self.activate_cb = activate_cb
        self.anchor_track_cb = anchor_track_cb
        self.g_document = gtkhtml2.Document()
        self.g_view = gtkhtml2.View()
        self.set_hadjustment(self.g_view.get_hadjustment())
        self.set_vadjustment(self.g_view.get_vadjustment())
        self.add(self.g_view)
        self.g_view.set_document(self.g_document)
        self.g_document.connect('link_clicked',
                 lambda widget, url: self.activate_cb(url))
        self.g_document.connect('request_url', self.on_request_url)
        if self.anchor_track_cb:
            self.g_view.connect('on-url', lambda f, url: self.anchor_track_cb(url))
        self.grab_focus = self.g_view.grab_focus
    def on_request_url(self, document, url, stream):
        """This function is called by the gtkhtml2 widget when it needs to
        load additional urls to display a file. For example images on the
        page.
        """
        #if url == "style.css":
        #    stream.set_mime_type("text/css")
        #    print stream.get_mime_type()
        url = url_decode(url)
        try:
            s = open(os.path.join(self.m_document_wd, url), 'r').read()
            stream.write(s)
        except IOError:
            if url.endswith('.png'):
                s = open('graphics/image-not-found.png').read()
                stream.write(s)
            else:
                print >> sys.stderr, "Not found:", url
    def source(self, html):
        self.g_document.clear()
        self.g_document.open_stream("text/html")
        self.g_document.write_stream(html)
        self.g_document.close_stream()
        #FIXME This is just a workaround. The problem is that when clicking on a link
        # the next page is scrolled so that the first link on a page is visible.
        def f1():
            self.get_vadjustment().set_value(0.0)
        gobject.idle_add(f1)

class MyTextView(gtk.TextView):
    indent_step = 20
    list_dot_indent = -12
    hand_cursor = gtk.gdk.Cursor(gtk.gdk.HAND2)
    regular_cursor = gtk.gdk.Cursor(gtk.gdk.XTERM)
    m_margin = 10
    def __init__(self, buffer=None):
        gtk.TextView.__init__(self, buffer)
        self.set_editable(False)
        self.set_cursor_visible(False)
        self.connect("event-after", self.on_event_after)
        self.connect("motion-notify-event", self.on_motion_notify_event)
        def rr(widget):
            width = widget.get_data('html-cell-width')
            parent_w = widget.get_parent().get_parent().get_allocation().width - self.m_margin * 2
            if width:
                try:
                    # The text view is inside a table, since only those has the
                    # data variable 'html-percentage-width' set.
                    if width.endswith('%'):
                        width = int(float(width[:-1]))
                        widget.set_size_request(parent_w * width / 100, -1)
                    else:
                        width = int(float(width))
                        widget.set_size_request(width, -1)
                except ValueError:
                    pass
            rect = widget.get_allocation()
        self.connect("realize", rr)
        self.set_wrap_mode(gtk.WRAP_WORD)
    def setup_tags(self):
        b = self.get_buffer()
        b.create_tag('ul')
        b.create_tag('body', left_margin=self.m_margin, right_margin=self.m_margin)
        for level in range(10):
            b.create_tag('dt-%i' % level,
                left_margin=self.indent_step*level+self.m_margin)
            b.create_tag('dd-%i' % level,
                left_margin=self.indent_step*level*3)
            b.create_tag('li-%i' % level,
                left_margin=self.indent_step*level+30, indent=self.list_dot_indent)
        for n, fontsize in (1, 16), (2, 15), (3, 14), (4, 12), (5, 11), (6, 10):
            b.create_tag('h%i' % n, weight=pango.WEIGHT_BOLD, font='Sans %i' % fontsize, pixels_above_lines=fontsize)
        b.create_tag('b', weight=pango.WEIGHT_BOLD)
        b.create_tag('strong', weight=pango.WEIGHT_BOLD)
        b.create_tag('em', style=pango.STYLE_ITALIC)
        b.create_tag('i', style=pango.STYLE_ITALIC)
        b.create_tag('cite', style=pango.STYLE_ITALIC)
        b.create_tag('dfn', style=pango.STYLE_ITALIC)
        b.create_tag('var', style=pango.STYLE_ITALIC)
        b.create_tag('code', family='monospace')
        b.create_tag('samp', family='monospace')
        b.create_tag('kbd', family='monospace')
        b.create_tag('pre', family='monospace', wrap_mode=gtk.WRAP_NONE)
        b.create_tag('center', justification=gtk.JUSTIFY_CENTER)
    def on_event_after(self, text_view, event):
        if event.type != gtk.gdk.BUTTON_RELEASE:
            return False
        if event.button != 1:
            return False
        buffer = text_view.get_buffer()
        x, y = text_view.window_to_buffer_coords(gtk.TEXT_WINDOW_WIDGET,
            int(event.x), int(event.y))
        iter = text_view.get_iter_at_location(x, y)
        for tag in iter.get_tags():
            if tag.get_data("page"):
                self.root_widget.activate_cb(tag.get_data("page"))
    def on_motion_notify_event(self, text_view, event):
        x, y = text_view.window_to_buffer_coords(gtk.TEXT_WINDOW_WIDGET,
            int(event.x), int(event.y))
        self.set_cursor_if_appropriate(text_view, x, y)
        text_view.window.get_pointer()
        if text_view.root_widget.anchor_track_cb:
            text_view.root_widget.anchor_track_cb(self.m_hovering_link)
            return True
        return False
    def set_cursor_if_appropriate(self, text_view, x, y):
        url = self.get_hovering_url(text_view, x, y)
        if url:
            self.m_hovering_link = url
            text_view.get_window(gtk.TEXT_WINDOW_TEXT).set_cursor(self.hand_cursor)
        else:
            self.m_hovering_link = None
            text_view.get_window(gtk.TEXT_WINDOW_TEXT).set_cursor(self.regular_cursor)
    def get_hovering_url(self, text_view, x, y):
        """
        Return the url if we are overing over a link.
        """
        buffer = text_view.get_buffer()
        iter = text_view.get_iter_at_location(x, y)

        tags = iter.get_tags()
        for tag in tags:
            if tag.get_property('name').startswith('link-'):
                page = tag.get_data("page")
                return page


class TextViewHtmlWidget(gtk.ScrolledWindow, _HtmlWidgetCommon):
    """
    Users should write
        this is a <a>test</a>
    and not
        this is a<a> test </a>
    """
    def __init__(self, activate_cb, anchor_track_cb):
        gtk.ScrolledWindow.__init__(self)
        _HtmlWidgetCommon.__init__(self)
        self.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        self.activate_cb = activate_cb
        self.anchor_track_cb = anchor_track_cb
        self.g_tw = MyTextView()
        self.g_tw.root_widget = self
        self.add(self.g_tw)
    def source(self, html):
        # We creates a completely new buffer, instead of just deleting
        # the content, because this is the easiest way to get rid of
        # all the link-nn tags. Don't know if it is smart memorywise, thought.
        # FIXME
        self.g_tw.set_buffer(gtk.TextBuffer())
        self.g_tw.setup_tags()
        #
        self.m_parser = p = NewHTMLParser()
        p.feed(html)
        self.m_parser.pop_head_out_of_tree()
        ##
        self.post_process(p)
        self.write_to_buffer(p)
        p.apply_styles(self.g_tw.get_buffer())
    def post_process(self, p):
        p.strip_whitespace()
        p.insert_newline_before_blocks()
        p.do_list_items()
        p.do_table()
        p.calc_bounds()
    def write_to_buffer(self, parser):
        buffer = self.g_tw.get_buffer()
        for c in self.m_parser.m_tree.m_children:
            c.write_to_buffer(buffer, self.g_tw, self.m_document_wd)


class ElementBaseClass(tree.TreeNode):
    def __init__(self, elementname, attrs):
        tree.TreeNode.__init__(self, elementname)
        self.m_attrs = attrs
        self.m_first = self.m_last = None
        self.m_name = elementname
        self.m_extra_tags = []
        self.m_ignore = False
    def get_attr(self, name):
        v = [x for x in self.m_attrs if x[0] == name]
        if v:
            return v[0][1]
    def set_attr(self, attrname, value):
        for idx, name in enumerate(self.m_attrs):
            if name == attrname:
                self.m_attrs[idx] = value
                return
        self.m_attrs.append((attrname, value))
    def show_children(self, level):
        print "    " * level, self, self.m_first, self.m_last
        for n in self.m_children:
            n.show_children(level + 1)
    def write_to_buffer(self, buffer, textview, document_wd):
        if self.m_name == 'a' and self.get_attr('href'):
            tag = buffer.create_tag('link-%s' % id(self), foreground="blue", underline=pango.UNDERLINE_SINGLE)
            tag.set_data('page', self.get_attr('href'))
            self.m_extra_tags.append('link-%s' % id(self))
        for e in self.m_children:
            e.write_to_buffer(buffer, textview, document_wd)
    def children_to_str(self):
        return ':%s:' % ("@".join([n.children_to_str() for n in self.m_children]))
    def __str__(self):
        if self.m_ignore:
            s = "Ignored"
        else:
            s = ""
        try:
            ll = "listlevel: %i" % self.m_list_level
        except AttributeError:
            ll = ""
        return "(%s %s children %s f:%s la:%s)" % (self.m_name, len(self.m_children), s, self.m_first, self.m_last)
    def do_children(self):
        for c in self.m_children:
            print "EE", c
            c.do_children()
    def get_as_string(self, level=0):
        """
        Return a string representation of the tree. This function is
        only used for module testing of the html parser.
        """
        s = "\n" + " " * level + "%s-%s-%s" % (self.m_name, self.m_first, self.m_last)
        for n in self.m_children:
            s += n.get_as_string(level + 1)
        return s

class Text(ElementBaseClass):
    def __init__(self, txt, pre=False):
        """
        txt -- the text
        pre -- a boolean telling if we are inside a <pre> element
        """
        ElementBaseClass.__init__(self, '::Text::', [])
        self.m_text = txt
        self.m_pre = pre
    def calc_bounds(self, idx):
        if not self.m_ignore:
            self.m_first = idx
            self.m_last = self.m_first + len(self.m_text) - 1
            idx += len(self.m_text)
        return idx
    def apply_styles(self, buffer):
        pass
    def write_to_buffer(self, buffer, textview, document_wd):
        buffer.insert(buffer.get_end_iter(), self.m_text)
    def children_to_str(self):
        return self.m_text
    def __str__(self):
        if self.m_ignore:
            s = "Ignore"
        else:
            s = ""
        if len(self.m_text) > 40:
            txt = "%s..." % self.m_text[:40]
        else:
            txt = self.m_text
        return "(%sText '%s' f:%s l:%s)" % (s, txt.replace("\n", "\\n"),
            self.m_first, self.m_last)

class Element(ElementBaseClass):
    def __init__(self, elementname, attrs):
        ElementBaseClass.__init__(self, elementname, attrs)
    def calc_bounds(self, idx):
        #print "calc_bounds(%i)" % idx, self.m_name
        if self.m_children:
            for c in self.m_children:
                idx = c.calc_bounds(idx)
            self.m_first = self.m_children[0].m_first
            self.m_last = self.m_children[-1].m_last
        else:
            self.m_first = idx
            self.m_last = idx
        return idx
    def apply_styles(self, buffer):
        if self.m_first is not None:
            it_a = buffer.get_iter_at_offset(self.m_first)
            it_b = buffer.get_iter_at_offset(self.m_last + 1)
            if self.m_name  == 'li':
                stylename = "li-%i" % self.m_list_level
            elif self.m_name == 'dd':
                stylename = "dd-%i" % self.m_list_level
            elif self.m_name == 'dt':
                stylename = "dt-%i" % (self.m_list_level-1)
            else:
                stylename = self.m_name
            try:
                for t in self.m_extra_tags:
                    buffer.apply_tag_by_name(t, it_a, it_b)
            except AttributeError:
                pass
            if buffer.tag_table.lookup(stylename):
                buffer.apply_tag_by_name(stylename, it_a, it_b)
        else:
            print "Element.apply_styles() ignoring:", self
        for c in self.m_children:
            c.apply_styles(buffer)

class Block(Element):
    def __init__(self, elementname, attrs):
        Element.__init__(self, elementname, attrs)

class Inline(Element):
    def __init__(self, elementname, attrs):
        Element.__init__(self, elementname, attrs)

class B(Inline):
    def __init__(self, attrs):
        Inline.__init__(self, 'b', attrs)
    def children_to_str(self):
        print "BOLD Children: '%s'" % (", ".join([n.children_to_str() for n in self.m_children]))
        return "<b>%s</b>" % ("".join([n.children_to_str() for n in self.m_children]))

class HR(Block):
    def write_to_buffer(self, buffer, textview, document_wd):
        anchor = buffer.create_child_anchor(buffer.get_end_iter())
        im = gtk.HSeparator()
        im.set_size_request(300, 3)#FIXME
        textview.add_child_at_anchor(im, anchor)
        buffer.insert(buffer.get_end_iter(), "\n")
    def calc_bounds(self, idx):
        self.m_first = idx
        self.m_last = idx + 1
        idx += 2
        return idx

class IMG(Inline):
    def __init__(self, attrs):
        Inline.__init__(self, 'img', attrs)
    def write_to_buffer(self, buffer, textview, document_wd):
        anchor = buffer.create_child_anchor(buffer.get_end_iter())
        im = gtk.Image()
        im.set_from_file(os.path.join(document_wd, url_decode(self.get_attr("src"))))
        im.show()
        textview.add_child_at_anchor(im, anchor)
    def calc_bounds(self, idx):
        self.m_first = idx
        self.m_last = idx
        idx += 1
        return idx

class OL(Block):
    def __init__(self, elementname, attrs):
        Block.__init__(self, elementname, attrs)
        self.m_counter = 1

class Table(Block):
    def __init__(self, elementname, attrs):
        Block.__init__(self, elementname, attrs)
    def write_to_buffer(self, buffer, textview, document_wd):
        anchor = buffer.create_child_anchor(buffer.get_end_iter())
        t = gtk.Table()
        t.set_homogeneous(False)
        for rownum, row in enumerate(self.m_rows):
            for cell in row:
                if not cell.get_attr('width'):
                    # If the only content of the cell (TD) is a image,
                    # then we set the with of the TextView that is holding
                    # the content of the cell to the width of the image.
                    # FIXME: if this is the case, then we really should just
                    # put an image in the GtkTable and not a TextView.
                    if len(cell.m_children) == 1:
                        w = cell.m_children[0].get_attr('width')
                        if w:
                            cell.set_attr('width', w)
        for rownum, row in enumerate(self.m_rows):
            for cellnum, cell in enumerate(row):
                cell_buf = gtk.TextBuffer()
                cell_tw = MyTextView(cell_buf)
                cell_tw.set_wrap_mode(gtk.WRAP_WORD)
                cell_tw.root_widget = textview.root_widget
                cell_tw.setup_tags()
                if cell.get_attr('align') == 'center':
                    cell.m_extra_tags.append('center')
                width = cell.get_attr('width')
                if width:
                    cell_tw.set_data('html-cell-width', width)
                cell.calc_bounds(0)
                #FIXME print "Fjern?"
                for c in cell.m_children:
                    c.write_to_buffer(cell_buf, cell_tw, document_wd)
                    c.apply_styles(cell_buf)
                cell.apply_styles(cell_buf)
                colspan = cell.get_attr('colspan')
                if colspan:
                    try:
                        colspan = int(colspan)
                    except ValueError, e:
                        colspan = 1
                else:
                    colspan = 1
                t.attach(cell_tw, cellnum, cellnum+colspan, rownum, rownum+1)
        t.show_all()
        textview.add_child_at_anchor(t, anchor)
        buffer.insert(buffer.get_end_iter(), "\n")
    def calc_bounds(self, idx):
        self.m_first = idx
        self.m_last = idx + 1
        idx += 2
        return idx


def create_block_element(name, attrs):
    d = {
        'hr': HR,
        'ol': OL,
        'table': Table,
    }
    if name in d:
        return d[name](name, attrs)
    else:
        return Block(name, attrs)

def create_inline_element(name, attrs):
    d = {'img': IMG, 'b': B}
    if name in d:
        return d[name](attrs)
    else:
        return Inline(name, attrs)


class NewHTMLParser(HTMLParser):
    block_elements = (
        'p', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'ul', 'li', 'dt', 'hr', 'div',
        'br',
        'dd',
        'dl',
        'ol',
        'table',
        'tr',
        'pre')
    in_pre = 0
    inline_elements = ('b', 'i', 'a', 'img')
    list_elements = ('ul', 'ol', 'dl')
    heading_elements = ('h1', 'h2', 'h3', 'h4', 'h5', 'h6')
    # elements that are written as <elem/> in xhtml, but often only
    # as a start element in html 4.01:  <elem>
    startend_elements = ('img', 'link', 'br', 'hr')
    white_space = (unichr(0x0020), unichr(0x0009), unichr(0x000A),
                   unichr(0x000C), unichr(0x200B))
    def __init__(self):
        HTMLParser.__init__(self)
        self.m_list_level = 0
        self.m_link_counter = 0
        self.m_charset = None
    def handle_starttag(self, tag, attrs):
        if 'start' in debug_flag:
            print "start", tag, attrs, self.m_list_level, self.m_link_counter
        if tag == 'html':
            self.m_tree = Block(tag, attrs)
            self.m_pointer = tree.Cursor(self.m_tree)
            assert self.m_pointer.get().m_name == 'html'
            return
        if tag == 'link':
            #FIXME maybe creating link as an inline is cheating, but it is easy
            inline = create_inline_element(tag, attrs)
            self.m_pointer.get().add(inline)
            return
        if tag == 'col':
            return
        if tag == 'meta':
            self.m_meta = attrs
            v = [x for x in attrs if x[0] == 'http-equiv']
            if v:
                if v[0][1].upper() == 'Content-Type'.upper():
                    c = [x for x in attrs if x[0] == 'content']
                    self.m_charset = c[0][1].split(";")[1].split("=")[1].strip()
            return
        if tag in self.block_elements:
            if tag in self.list_elements:
                self.m_list_level += 1
            self.m_pointer.get().add(create_block_element(tag, attrs))
            self.m_pointer.goto_last_child()
            if tag == 'pre':
                self.in_pre += 1
            if tag in ('li', 'dd', 'dt'):
                self.m_pointer.get().m_list_level = self.m_list_level
            assert self.m_pointer.get().m_name == tag
            # We do this because we don't want to add the following
            # elements as children to tag
            if tag in self.startend_elements:
                self.m_pointer.go_parent()
        elif tag in self.inline_elements:
            inline = create_inline_element(tag, attrs)
            self.m_pointer.get().add(inline)
            self.m_pointer.goto_last_child()
        else:
            #self.m_pointer.get().add(Block(tag, attrs))
            inline = Inline(tag, attrs)
            self.m_pointer.get().add(inline)
            self.m_pointer.goto_last_child()
    def handle_endtag(self, tag):
        if 'end' in debug_flag:
            print "end", tag, self.m_list_level
        if not self.m_pointer.get().m_name == tag:
            # <h1><a href="..."><img src="http://..."></a></h1>
            if self.m_pointer.get().m_name in self.startend_elements:
                self.m_pointer.go_parent()
            else:
                print "Warning: wrong end tag. Got '%s', expected '%s', ignoring:" % (tag, self.m_pointer.get().m_name)
        if tag == 'pre':
            self.in_pre -= 1
        if tag in self.list_elements:
            self.m_list_level -= 1
        self.m_pointer.go_parent()
    def handle_data(self, data):
        if 'data' in debug_flag:
            print "data '%s'" % data.replace("\n", "\\n")
            print "type:", type(data), self.m_charset
        if not data.strip():
            return
        if data:
            if not isinstance(data, unicode):
                data = data.decode('utf-8', 'replace')
            if self.m_pointer.get().m_name == 'body':
                p = Block('p', [])
                p.add(Text(data.strip(), self.in_pre))
                self.m_pointer.get().add(p)
            else:
                self.m_pointer.get().add(Text(data, self.in_pre))
    def do_table(self):
        """
        Remove the TR and TD nodes from the tree, and put the data they
        contain into the TABLE node.
        """
        def do_row(row):
            ret = []
            for td in row.m_children:
                if td.m_name in ('td', 'th'):
                    ret.append(td)
                else:
                    print "Not a td, ignoring:", td
            return ret

        def fill_table(cursor):
            table = cursor.get()
            table.m_rows = []
            # We do ignore <colgroup>
            if table.m_children[0].m_name == 'colgroup':
                del table.m_children[0]
            # We do this because  we simply ignore the <tbody> tag
            if table.m_children[0].m_name == 'tbody':
                v = table.m_children[0].m_children
            else:
                v = table.m_children
            for tablechild in v:
                # tablechild is here a <tr> or possibly some text
                if tablechild.m_name == 'tr':
                    table.m_rows.append(do_row(tablechild))
                else:
                    assert tablechild.m_name == '::Text::'
            table.m_children = []

        c = tree.Cursor(self.m_tree)
        while 1:
            if c.get().m_name == 'table':
                tc = c.copy()
                fill_table(tc)
            if not c.go_next():
                break
    def do_list_items(self):
        """
        Insert a dot before list items in a unordered list, and a 
        list number before list items in a ordered list.
        """
        c = tree.Cursor(self.m_tree)
        while c.go_next():
            if c.get().m_name == 'li':
                if c.get_parent().m_name == 'ul':
                    c.go_next()
                    c.insert(Text("%s " % unichr(0x2022)))
                elif c.get_parent().m_name == 'ol':
                    c.insert(Text("%i. " % c.get_parent().m_counter))
                    c.get_parent().m_counter += 1
                    c.go_next()
    def strip_whitespace(self):
        """
        Strip unnecessary white space from text that are in blocks.
        """
        c = tree.Cursor(self.m_tree)
        first_text_in_block = True
        while c.go_next():
            # Først i block: ikke lstrip
            e = c.get()
            if isinstance(e, Text):
                if first_text_in_block:
                    #e.m_text = e.m_text.lstrip()
                    if endswith_whitespace(e.m_text):#[-1] in self.white_space:
                        e.m_text = "%s " % e.m_text.rstrip()
                    first_text_in_block = False
                else:
                    nc = c.copy()
                    nc.go_prev()
                    while not isinstance(nc.get(), (Block, Text)):
                        nc.go_prev()
                    assert isinstance(nc.get(), Text)
                    if endswith_whitespace(nc.get().m_text):#[-1] in self.white_space:
                        e.m_text = e.m_text.lstrip()
                    else:
                        if e.m_text[0] in self.white_space:
                            e.m_text = " %s" % e.m_text.lstrip()
                        else:
                            e.m_text = e.m_text.lstrip()
                #after we have adjusted the start and end, we remove
                #unnecessary whitespace in the middle
                if not e.m_pre:
                    e.m_text = "%s%s%s" % (" " * startswith_whitespace(e.m_text), " ".join(e.m_text.split()), " " * endswith_whitespace(e.m_text))

            elif isinstance(e, Block):
                first_text_in_block = True
    def insert_newline_before_blocks(self):
        cursor = tree.Cursor(self.m_tree)
        # .go_next so we don't insert before <body>
        cursor.go_next()
        cursor.go_next()
        while cursor.go_next():
            if isinstance(cursor.get(), Block) and not isinstance(cursor.get_prev(), Block):
                if cursor.get().m_name in ('p', 'pre', 'div') \
                        and cursor.get_prev_sibling() \
                        and cursor.get_prev_sibling().m_name not in self.heading_elements:
                    t = Text("\n\n")
                else:
                    t = Text("\n")
                cursor.insert(t)
                assert cursor.get() == t
                cursor.go_next()
    def calc_bounds(self):
        idx = 0
        for node in self.m_tree.m_children:
            idx = node.calc_bounds(idx)
    def apply_styles(self, buffer):
        for block in self.m_tree.m_children:
            block.apply_styles(buffer)
    def get_last_text(self, pointer):
        if isinstance(pointer, Text):
            return pointer.m_text
        else:
            v = copy.copy(pointer.m_children)
            v.reverse()
            for c in v:#[1:]:
                return self.get_last_text(c)
    def txtformat(self):
        for e in self.m_tree.iterate_tree():
            if isinstance(e, Text):
                print e.m_text,
    def pop_head_out_of_tree(self):
        """
        Remove the <head> out of the tree and return it.
        """
        c = tree.Cursor(self.m_tree)
        while c.go_next() and c.get().m_name != 'body':
            if c.get().m_name == 'head':
                head = c.pop()
                return head



def _test():
    import gettext
    gettext.install('solfege', 'share/locale', unicode=1)
    sys.path.insert(0, ".")
    class TestWin(gtk.Window):
        def __init__(self):
            global s
            gtk.Window.__init__(self)
            self.set_size_request(700, 400)
            self.g_html1 = TextViewHtmlWidget(self.activate_cb, self.anchor_track_cb)
            self.g_html2 = GtkHtml2Widget(self.activate_cb, self.anchor_track_cb)
            if len(sys.argv) > 1:
                self.m_filename = sys.argv[-1:][0]
                if sys.argv[-1:][0].endswith('.html'):
                   fn = sys.argv[-1:][0]
                   self.g_html1.read_file_abs(fn)
                   self.g_html2.read_file_abs(fn)
            vbox = gtk.VBox()
            self.add(vbox)
            button = gtk.Button("Reload")
            button.connect('clicked', self.on_reload)
            vbox.pack_start(button, False)
            vbox.pack_start(self.g_html1)
            vbox.pack_start(self.g_html2)
            self.show_all()
            self.connect('destroy', gtk.main_quit)
        def on_reload(self, btn):
            s = open(self.m_filename, 'rU').read()
            self.g_html1.source(s)
            self.g_html2.source(s)
        def activate_cb(self, url):
            self.g_html1.read_file_rel(url)
            self.g_html2.read_file_rel(url)
        def anchor_track_cb(self, v):
            if v:
                print "ANCHOR_TRACK_CB", v
    w = TestWin()
    gtk.main()

if runtime.has_gtkhtml2():
    import gtkhtml2
    HtmlWidget = GtkHtml2Widget
else:
    HtmlWidget = TextViewHtmlWidget

if __name__ == '__main__':
    gtk.rc_parse("solfege.gtkrc")
    _test()


