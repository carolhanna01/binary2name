# GNU Solfege - free ear training software
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2007  Tom Cato Amundsen
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

from rat import Rat
import const
import gtk
import mpdutils
import operator

fetadir = "feta"
class dim20:
    linespacing = 8
    stemlen = linespacing * 3.5
    xshift = 10
    col_width = 20
    first_staff_ypos = 70
    staff_spacing = 70
    ledger_left = -4
    ledger_right = 14
    accidental_widths = { -2: 12, -1: 7, 0: 5, 1: 8, 2: 8}
    clef_yoffset = {'violin': linespacing-38,
                    'treble': linespacing-38,#treble == violin
                    'subbass': -8-2*linespacing,
                    'bass': -8-linespacing,
                    'baritone': -8-3*linespacing,
                    'varbaritone': -8,
                    'tenor': -8-2*linespacing,
                    'alto': -8-linespacing,
                    'mezzosoprano': -15 + linespacing,
                    'soprano': -15 + 2 * linespacing,
                    'french': -38 + 2 * linespacing}
class dim20tight:
    linespacing = 8
    stemlen = linespacing * 3.5
    xshift = 10
    col_width = 20
    first_staff_ypos = 70
    staff_spacing = linespacing * 6
    ledger_left = -4
    ledger_right = 14
    accidental_widths = { -2: 12, -1: 7, 0: 5, 1: 8, 2: 8}
    clef_yoffset = {'violin': linespacing-38,
                    'treble': linespacing-38,#treble == violin
                    'subbass': -8-2*linespacing,
                    'bass': -8-linespacing,
                    'baritone': -8-3*linespacing,
                    'varbaritone': -8,
                    'tenor': -8-2*linespacing,
                    'alto': -8-linespacing,
                    'mezzosoprano': -15 + linespacing,
                    'soprano': -15 + 2 * linespacing,
                    'french': -38 +linespacing}


dimentions = {20: dim20, "20-tight": dim20tight}

accidental_y_offset = {const.ACCIDENTAL__2: -7,
                       const.ACCIDENTAL__1: -7,
                       const.ACCIDENTAL_0: -5,
                       const.ACCIDENTAL_1: -6,
                       const.ACCIDENTAL_2: -2}

class Engraver:
    def __init__(self, timepos, fontsize):
        self.m_fontsize = 20
        self.m_timepos = timepos
    def get_width(self):
        """
        Get the width of all engravers that does not implement their
        own get_width function.
        """
        return 20
    def __str__(self):
        return '(Engraver)'


class ClefEngraver(Engraver):
    def __init__(self, timepos, fontsize, clef):
        Engraver.__init__(self, timepos, fontsize)
        self.m_cleftype = clef
    def set_xpos(self, dict):
        self.m_xpos = dict[self.m_timepos].m_clef
    def get_width(self):
        return 25
    def engrave(self, widget, gc, staff_yoffset):
        fn = {'violin': 'G',
              'treble': 'G',
              'subbass': 'F',
              'bass': 'F',
              'baritone': 'C',
              'varbaritone': 'F',
              'tenor': 'C',
              'alto': 'C',
              'mezzosoprano': 'C',
              'soprano': 'C',
              'french': 'G'}[self.m_cleftype]
        widget.window.draw_pixbuf(gc,
            gtk.gdk.pixbuf_new_from_file(fetadir+'/feta%i-clefs-%s.xpm' \
                % (self.m_fontsize, fn)),
            0, 0, self.m_xpos,
            dimentions[self.m_fontsize].clef_yoffset[self.m_cleftype] \
                + staff_yoffset)

class TimeSignatureEngraver(Engraver):
    def __init__(self, timepos, fontsize, timesig):
        Engraver.__init__(self, timepos, fontsize)
        self.m_timesig = timesig
    def set_xpos(self, dict):
        self.m_xpos = dict[self.m_timepos].m_timesignature
    def engrave(self, widget, gc, staff_yoffset):
        x = 0
        for idx in range(len(str(self.m_timesig.m_num))):
            n = str(self.m_timesig.m_num)[idx]
            widget.window.draw_pixbuf(gc,
                gtk.gdk.pixbuf_new_from_file(fetadir+'/feta%i-number-%s.xpm' \
                    % (self.m_fontsize, n)),
                0, 0,
                self.m_xpos+x, staff_yoffset-12)
            x  += 10
        x = 0
        for idx in range(len(str(self.m_timesig.m_den))):
            n = str(self.m_timesig.m_den)[idx]
            widget.window.draw_pixbuf(gc,
                gtk.gdk.pixbuf_new_from_file(fetadir+'/feta%i-number-%s.xpm' %\
                    (self.m_fontsize, n)),
                0, 0,
                self.m_xpos+x, staff_yoffset+3)
            x += 10
    def __str__(self):
        return '(TimeSignatureEngraver:%i/%i, xpos:%s)' % (self.m_timesig.m_den,
            self.m_timesig.m_num, self.m_xpos)


class TieEngraver(Engraver):
    def __init__(self, fontsize, pos1, pos2, shift1, shift2, ylinepos):
        Engraver.__init__(self, None, fontsize)
        self.m_pos1 = pos1
        self.m_pos2 = pos2
        self.m_shift1 = shift1
        self.m_shift2 = shift2
        self.m_ylinepos = ylinepos
    def set_xpos(self, dict):
        self.m_xpos = dict[self.m_pos1].m_music
        self.m_xpos2 = dict[self.m_pos2].m_music
    def engrave(self, widget, gc, staff_yoffset):
        dim = dimentions[self.m_fontsize]
        self.m_xpos += 3 + (self.m_shift1 + 1) * dim.xshift
        self.m_xpos2 += - 3 + self.m_shift2 * dim.xshift
        w = self.m_xpos2 - self.m_xpos
        h = 8
        widget.window.draw_arc(gc, False,
              self.m_xpos, staff_yoffset + dim.linespacing*(self.m_ylinepos-1)/2 + 3,
              w, h, 64 * 180, 64*180)


class AccidentalsEngraver(Engraver):
    def __init__(self, timepos, fontsize, accs):
        Engraver.__init__(self, timepos, fontsize)
        self.m_accs = accs
        def f(v, w=dimentions[fontsize].accidental_widths):
            x = 0
            for a in v:
                x += w[a]
            return x
        self.m_usize = reduce(operator.__add__, map(f, accs.values())) + len(accs)
    def set_xpos(self, dict, tpd):
        self.m_xpos = dict[self.m_timepos].m_accidentals + tpd[self.m_timepos].m_accidentals - self.get_width()
    def get_width(self):
        return self.m_usize
    def engrave(self, widget, gc, staff_yoffset):
        x = 0
        for y in self.m_accs:
            for acc in self.m_accs[y]:
                widget.window.draw_pixbuf(gc,
                    gtk.gdk.pixbuf_new_from_file(
                        fetadir+'/feta%i-accidentals-%i.xpm' % \
                            (self.m_fontsize, acc)),
                        0, 0,
                        self.m_xpos + x,
                        int(accidental_y_offset[acc] + staff_yoffset
                         + dimentions[self.m_fontsize].linespacing*y/2
                         + accidental_y_offset[acc]))
                x += dimentions[self.m_fontsize].accidental_widths[acc] + 1


class KeySignatureEngraver(Engraver):
    def __init__(self, timepos, fontsize, old_key, key, clef):
        Engraver.__init__(self, timepos, fontsize)
        self.m_old_key = old_key
        self.m_key = key
        self.m_clef = clef
        self.m_old_accidentals = mpdutils.key_to_accidentals(old_key)
        self.m_accidentals = mpdutils.key_to_accidentals(key)
    def set_xpos(self, dict):
        self.m_xpos = dict[self.m_timepos].m_keysignature
    def get_width(self):
        #FIXME this value depends on the fontsize, and on what kind of
        #accidental we are drawing. A sharp is wider that a flat.
        return len(self.m_accidentals) * 10 + len(self.m_old_accidentals) * 6 + 8
    def engrave(self, widget, gc, staff_yoffset):
        x = 0
        dolist = []
        # natural signs
        for acc in self.m_old_accidentals:
            if acc in self.m_accidentals:
                continue
            type = 0
            ypos = mpdutils.an_to_ylinepos(acc, self.m_clef)
            dolist.append((0, x, ypos))
            #FIXME see FIXME msg in .get_width
            x += 6
        # accidentals
        for acc in self.m_accidentals:
            if acc.endswith('eses'):
                type = -2
            elif acc.endswith('es'):
                type = -1
            elif acc.endswith('isis'):
                type = 2
            else:
                type = 1
            ypos = mpdutils.an_to_ylinepos(acc, self.m_clef)
            dolist.append((type, x, ypos))
            #FIXME see FIXME msg in .get_width
            x += 10
        for type, x, ypos in dolist:
            widget.window.draw_pixbuf(gc,
                gtk.gdk.pixbuf_new_from_file(
                    fetadir+'/feta%i-accidentals-%i.xpm' \
                    % (self.m_fontsize, type)),
                0, 0,
                self.m_xpos + x,
                int(accidental_y_offset[type] + staff_yoffset
                     + dimentions[self.m_fontsize].linespacing*ypos/2
                     + accidental_y_offset[type]))


class NoteheadEngraver(Engraver):
    def __init__(self, timepos, fontsize, shift, ypos, head, numdots, midi_int, mgroup):
        """
        m_ypos == 0 is the middle line on the staff.
        Negative value is up, positive is down.
        The value counts notesteps, not pixels!
        FIXME, right now it also draws dots, but they should be an own class
        because the dots has to be handled special with noteheads are xshifted.
        """
        Engraver.__init__(self, timepos, fontsize)
        self.m_head = head
        self.m_shift = shift
        self.m_numdots = numdots
        self.m_ypos = ypos
        self.m_midi_int = midi_int
        self.m_mgroup = mgroup
    def set_xpos(self, dict):
        self.m_xpos = dict[self.m_timepos].m_music
    def engrave(self, widget, gc, staff_yoffset):
        dim = dimentions[self.m_fontsize]
        # Have to adjust holenotes a little to the left to be on the middle
        # of the ledger line.
        if self.m_head == 0:
            xx = -2
        else:
            xx = 0
        widget.window.draw_pixbuf(gc, 
            gtk.gdk.pixbuf_new_from_file(fetadir+'/feta%i-noteheads-%i.xpm' \
                % (self.m_fontsize, self.m_head)),
            0, 0,
            self.m_xpos + self.m_shift * dim.xshift + xx,
            int(staff_yoffset + dim.linespacing*self.m_ypos/2 - 4))
        for n in range(self.m_numdots):
            widget.window.draw_pixbuf(gc,
                gtk.gdk.pixbuf_new_from_file(fetadir+'/feta20-dots-dot.xpm'),
                0, 0,
                int(self.m_xpos+dim.xshift*(self.m_shift+1.5+n/2.0)),
                -3 + staff_yoffset + dim.linespacing*self.m_ypos/2)
    def get_pixmap_id(self):
        return {0: const.NOTEHEAD_0,
                1: const.NOTEHEAD_1,
                2: const.NOTEHEAD_2}[self.m_head]
    def __str__(self):
        return "(NoteheadEngraver: xpos:%i, ypos:%i, head:%i)" % (self.m_xpos,
                                                                    self.m_ypos,
                                                                    self.m_head)

class BarlineEngraver(Engraver):
    def __init__(self, timepos, fontsize, type):
        Engraver.__init__(self, timepos, fontsize)
        self.m_type = type
    def set_xpos(self, dict):
        self.m_xpos = dict[self.m_timepos].m_barline
    def get_width(self):
        return 8
    def engrave(self, widget, gc, staff_yoffset):
        dim = dimentions[self.m_fontsize]
        widget.window.draw_line(gc, self.m_xpos, staff_yoffset - dim.linespacing*2,
                                    self.m_xpos, staff_yoffset + dim.linespacing*2)

class TupletEngraver(Engraver):
    def __init__(self, fontsize, n):
        Engraver.__init__(self, None, fontsize)
        self.m_stems = []
        self.m_den, self.m_direction = n
    def set_xpos(self, dict):
        pass
    def add_stem(self, stem):
        self.m_stems.append(stem)
    def do_layout(self):
        dim = dimentions[self.m_fontsize]
        top = []
        bottom = []
        for s in self.m_stems:
            if s.m_mgroup.m_stemdir == const.UP:
                top.append(min(s.m_yposes)-7)
                bottom.append(max(s.m_yposes)+1)
            else:
                top.append(min(s.m_yposes))
                bottom.append(max(s.m_yposes)+5)
        self.m_t = min(top) - 2
        self.m_b = max(bottom) + 2
        self.m_xpos1 = self.m_stems[0].m_xpos
        self.m_xpos2 = self.m_stems[-1].m_xpos + dim.xshift
    def engrave(self, widget, gc, staff_yoffset):
        dim = dimentions[self.m_fontsize]
        m = self.m_xpos1 + (self.m_xpos2 - self.m_xpos1)/2
        x1 = m - 6
        x2 = m + 6
        if self.m_direction == const.UP or self.m_direction == const.BOTH:
            y = min(staff_yoffset - dim.linespacing * 3,
                   staff_yoffset + self.m_t * dim.linespacing / 2)
            d = 1
        else: # == const.DOWN
            y = max(staff_yoffset + dim.linespacing * 5,
                    staff_yoffset + self.m_b * dim.linespacing /2)
            d = -1
        widget.window.draw_line(gc, self.m_xpos1, y, x1, y)
        widget.window.draw_line(gc, x2, y, self.m_xpos2, y)
        widget.window.draw_line(gc, self.m_xpos1, y, self.m_xpos1, y+5*d)
        widget.window.draw_line(gc, self.m_xpos2, y, self.m_xpos2, y+5*d)
        #FIXME shouldn't we use pango font string to be more portable?
        font = gtk.load_font("-adobe-helvetica-medium-r-normal-*-12-*-*-*-*-*-*-*")
        widget.window.draw_string(font, gc, m-3, y + 3, str(self.m_den))

class BeamEngraver(Engraver):
    def __init__(self, fontsize):
        Engraver.__init__(self, None, fontsize)
        self.m_stems = []
    def add_stem(self, stem_engraver):
        self.m_stems.append(stem_engraver)
    def set_xpos(self, dict):
        pass
    def do_layout(self):
        self.decide_beam_stemdir()
        self.set_stemlens(self.find_lowhigh_ypos())
    def set_stemlens(self, lh):
        l, h = lh
        for e in self.m_stems:
            if self.m_stemdir == const.UP:
                e.m_beamed_stem_top = l - 6
            else:
                assert self.m_stemdir == const.DOWN
                e.m_beamed_stem_top = h + 6
    def find_lowhigh_ypos(self):
        """
        Find the lowest and highest notehead in the beam.
        """
        mn = 1000
        mx = -1000
        for se in self.m_stems:
            for ylinepos in se.m_yposes:
                if mn > ylinepos:
                    mn = ylinepos
                if mx < ylinepos:
                    mx = ylinepos
        return mn, mx
    def decide_beam_stemdir(self):
        """
        Decide the direction for the stems in this beam, and set
        the stemdir for all stems.
        """
        v = {const.UP: 0, const.DOWN: 0}
        for e in self.m_stems:
            v[e.m_mgroup.m_stemdir] = v[e.m_mgroup.m_stemdir] + 1
            e.m_stemlen = 50
        if v[const.UP] > v[const.DOWN]:
            stemdir = const.UP
        else:
            stemdir = const.DOWN
        for e in self.m_stems:
            e.m_mgroup.m_stemdir = stemdir
        self.m_stemdir = stemdir
        for e in self.m_stems:
            e.m_mgroup.m_stemdir = stemdir
    def engrave(self, widget, gc, staff_yoffset):
        dim = dimentions[self.m_fontsize]
        d = 0
        if self.m_stemdir == const.UP:
            d = 1
        else:
            d = -1
        x1 = self.m_stems[0].m_xpos
        x2 = self.m_stems[-1].m_xpos
        if self.m_stems[0].m_beamed_stem_top % 2 == 1:
            for x in range(len(self.m_stems)):
                self.m_stems[x].m_beamed_stem_top =\
                    self.m_stems[x].m_beamed_stem_top - d
        y1 = self.m_stems[0].m_beamed_stem_top * dim.linespacing/2 + staff_yoffset
        beamw = 3
        beaml = 10
        for y in range(beamw):
            widget.window.draw_line(gc, x1, y1 + y*d, x2, y1 + y*d)
        for stem in self.m_stems:
            stem._beam_done = 8
        def short_beam(stem, xdir, beamnum, d=d, widget=widget, beamw=beamw, gc=gc, beaml=beaml, y1=y1):
            for y in range(beamw):
                widget.window.draw_line(gc, stem.m_xpos, y1 + y + beamnum*d*beamw*2,
                       stem.m_xpos + xdir*beaml, y1 + y + beamnum*d*beamw*2)
        for nl, yc in ((16, 0), (32, 1), (64, 2)):
            for i in range(len(self.m_stems)-1):
                if self.m_stems[i].m_mgroup.m_duration.m_nh >= nl \
                        and self.m_stems[i+1].m_mgroup.m_duration.m_nh >= nl:
                    for y in range(beamw):
                        widget.window.draw_line(gc,
                            self.m_stems[i].m_xpos, d*beamw*yc*2 + y1 + y + d*beamw*2,
                            self.m_stems[i+1].m_xpos, d*beamw*yc*2 + y1 + y + d*beamw*2)
                    self.m_stems[i]._beam_done = nl
                    self.m_stems[i+1]._beam_done = nl
                if self.m_stems[i].m_mgroup.m_duration.m_nh >= nl \
                      and self.m_stems[i+1].m_mgroup.m_duration.m_nh <= nl/2 \
                      and self.m_stems[i]._beam_done < nl:
                    if i == 0:
                        dir = 1
                    else:
                        if self.m_stems[i-1].m_mgroup.m_duration.m_nh < \
                           self.m_stems[i+1].m_mgroup.m_duration.m_nh:
                            dir = 1
                        else:
                            dir = -1
                    short_beam(self.m_stems[i], dir, yc+1, d)
                    self.m_stems[i]._beam_done = nl
            if self.m_stems[-1].m_mgroup.m_duration.m_nh >= nl \
                    and self.m_stems[-1]._beam_done <= nl/2:
                short_beam(self.m_stems[-1], -1, yc+1, d)


class StemEngraver(Engraver):
    """
    Every notehead belong to a stem, even if the stem is invisible.
    """
    def __init__(self, timepos, fontsize, yposes, mgroup, is_beamed):
        Engraver.__init__(self, timepos, fontsize)
        self.m_mgroup = mgroup
        self.m_yposes = yposes
        self.m_is_beamed = is_beamed
        if len(self.m_yposes) == 1:
            x = self.m_yposes[0]
        else:
            x = self.m_yposes[0] + self.m_yposes[-1]
        if self.m_mgroup.m_stemdir == const.UP \
            or (self.m_mgroup.m_stemdir == const.BOTH and x >= 0):
            self.m_mgroup.m_stemdir = const.UP
        else:
            self.m_mgroup.m_stemdir = const.DOWN
        self.m_stemlen = dimentions[self.m_fontsize].stemlen
    def set_xpos(self, dict):
        self.m_xpos = dict[self.m_timepos].m_music
    def calc_xpos(self):
        if self.m_mgroup.m_stempos == 1 or self.m_mgroup.m_stemdir == const.UP:
            self.m_xpos = self.m_xpos + dimentions[self.m_fontsize].xshift
    def engrave(self, widget, gc, staff_yoffset):
        dim = dimentions[self.m_fontsize]
        # draw flags
        if not self.m_is_beamed and self.m_mgroup.m_duration.m_nh > 4:
            if self.m_mgroup.m_stemdir == const.UP:
                widget.window.draw_pixbuf(gc,
                    gtk.gdk.pixbuf_new_from_file(
                        fetadir+'/feta%i-flags-%s.xpm' % \
                            (self.m_fontsize, self.get_flag(const.UP))),
                    0, 0,
                    self.m_xpos,
                    int(staff_yoffset - self.m_stemlen + self.m_yposes[0] * dim.linespacing/2))
            else:
                widget.window.draw_pixbuf(gc,
                    gtk.gdk.pixbuf_new_from_file(
                        fetadir+'/feta%i-flags-%s.xpm' \
                        % (self.m_fontsize, self.get_flag(const.DOWN))),
                    0, 0,
                    self.m_xpos,
                    int(staff_yoffset + 4 + self.m_yposes[-1] * dim.linespacing/2))
        # draw stem
        if self.m_mgroup.m_stemdir == const.DOWN:
            if self.m_is_beamed:
                yroot = self.m_beamed_stem_top
            else:
                yroot = self.m_yposes[-1] + 6
            ytop = self.m_yposes[0]
        else:
            if self.m_is_beamed:
                yroot = self.m_beamed_stem_top
            else:
                yroot = self.m_yposes[0] - 6
            ytop = self.m_yposes[-1]
        widget.window.draw_line(gc,
                     self.m_xpos,
                     ytop * dim.linespacing/2 + staff_yoffset,
                     self.m_xpos,
                     yroot * dim.linespacing/2 + staff_yoffset)
    def get_flag(self, dir):
        assert self.m_mgroup.m_duration.m_nh > 4
        return {8:{const.UP: const.FLAG_8UP,
                             const.DOWN: const.FLAG_8DOWN},
                16: {const.UP: const.FLAG_16UP,
                               const.DOWN: const.FLAG_16DOWN},
                32: {const.UP: const.FLAG_32UP,
                               const.DOWN: const.FLAG_32DOWN},
                64: {const.UP: const.FLAG_64UP,
                               const.DOWN: const.FLAG_64DOWN}} \
                              [self.m_mgroup.m_duration.m_nh][dir]
    def __str__(self):
        return "(StemEngraver)"


class LedgerLineEngraver(Engraver):
    def __init__(self, timepos, fontsize, up, down):
        """
        up: number of ledger lines above staff
        down: number of ledger lines below staff
        """
        Engraver.__init__(self, timepos, fontsize)
        self.m_up = up
        self.m_down = down
    def set_xpos(self, dict):
        self.m_xpos = dict[self.m_timepos].m_music
    def engrave(self, widget, gc, staff_yoffset):
        dim = dimentions[self.m_fontsize]
        if self.m_up:
            for y in range(self.m_up):
                widget.window.draw_line(gc,
                    self.m_xpos+dim.ledger_left, (-y-3) * dim.linespacing + staff_yoffset,
                    self.m_xpos+dim.ledger_right, (-y-3) * dim.linespacing + staff_yoffset)
        if self.m_down:
            for y in range(self.m_down):
                widget.window.draw_line(gc,
                    self.m_xpos+dim.ledger_left, (y+3) * dim.linespacing + staff_yoffset,
                    self.m_xpos+dim.ledger_right, (y+3) * dim.linespacing + staff_yoffset)
    def __str__(self):
        return "(LedgerLineEngraver xpos:%i, updown%i%i" % (
            self.m_xpos, self.m_up, self.m_down)


class RestEngraver(Engraver):
    def __init__(self, timepos, fontsize, ypos, dur):
        Engraver.__init__(self, timepos, fontsize)
        self.m_ypos = ypos
        self.m_dots = dur.m_dots
        self.m_type = {Rat(1, 1): 0, Rat(1, 2): 1, Rat(1,4): 2,
                        Rat(1, 8): 3, Rat(1, 16): 4, Rat(1, 32): 5,
                        Rat(1, 64): 6}[dur.get_rat_value()]
    def set_xpos(self, dict):
        self.m_xpos = dict[self.m_timepos].m_music
    def get_width(self):
        #FIXME write me!
        return 20
    def engrave(self, widget, gc, staff_yoffset):
        dim = dimentions[self.m_fontsize]
        if self.m_type == 0:
            my = dim.linespacing/2
        else:
            my = 0
        widget.window.draw_pixbuf(gc,
            gtk.gdk.pixbuf_new_from_file(fetadir+'/feta%i-rests-%i.xpm' \
                % (self.m_fontsize, self.m_type)),
            0, 0,
            self.m_xpos,
            int(staff_yoffset - my + dim.linespacing*self.m_ypos/2 - 4))
    def get_pixmap_id(self):
        return {1: const.REST_1,
                2: const.REST_2,
                4: const.REST_4,
                8: const.REST_8,
                16: const.REST_16,
                32: const.REST_32,
                64: const.REST_64}[self.m_resttype]
    def __str__(self):
        return "(RestEngraver)"
