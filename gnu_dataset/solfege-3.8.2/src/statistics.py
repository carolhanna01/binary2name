# GNU Solfege - free ear training software
# Copyright (C) 2000, 2001, 2002, 2003, 2004, 2006, 2007  Tom Cato Amundsen
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

# IMPORTANT:
# Remember to increase statistics_version and add code to handle
# old versions if incompatabilities are added.

import sys
import time, os, pickle
import utils
import gethomedir

# version 1, the first version was introduced with Solfege 2.0
# version 2 was introduced in the newxercisesetup changes
statistics_version = 2

STATISTICS_SAVE_DIR = os.path.join(gethomedir.get_home_dir(), '.solfege',
    'statistics')

if not os.path.exists(STATISTICS_SAVE_DIR):
    if not os.path.exists(STATISTICS_SAVE_DIR):
        os.makedirs(STATISTICS_SAVE_DIR)
    f = open(os.path.join(STATISTICS_SAVE_DIR, 'statistics_version'), 'w')
    print >> f, statistics_version
    f.close()

def get_statistics_version():
    """
    Return as an int, the version number the statistics stored in
    STATISTICS_SAVE_DIR has.
    """
    try:
        f = open(os.path.join(STATISTICS_SAVE_DIR, 'statistics_version'), 'r')
    except IOError, e:
        print >> sys.stderr, "While opening the file '%s':" % os.path.join(STATISTICS_SAVE_DIR, 'statistics_version')
        print >> sys.stderr, e
        return 0
    s = f.read()
    f.close()
    try:
        return int(s)
    except ValueError, e:
        print >> sys.stderr, "While parsing the content of '%s':" % os.path.join(STATISTICS_SAVE_DIR, 'statistics_version')
        print >> sys.stderr, e
        return 0

statistics_on_disk_version = get_statistics_version()
if statistics_on_disk_version > statistics_version:
    usable_statistics = None
    print >> sys.stderr, """The statistics stored in %(d)s have been created by a newer version of
Solfege, and that release has changed the way statistics are saved. If you want
to record statistics, you must either use that newer version of Solfege, or
delete (or rename) the directory %(d)s\n""" % {'d':STATISTICS_SAVE_DIR}
elif statistics_on_disk_version == 0:
    usable_statistics = None
    print >> sys.stderr, """
Something is strange with the statistics stored in %s.
""" % STATISTICS_SAVE_DIR
else:
    usable_statistics = 1
YEAR = 0
MONTH = 1
DAY = 2
HOURS = 3
MINUTES = 4
SECONDS = 5
WEEKDAY = 6
JULIANDAY = 7

class AbstractStatistics:
    def __init__(self, teacher):
        self.m_t = teacher
        self.m_session_stat = {}
        self.m_today_stat = {}
        self.m_last7_stat = {}
        self.m_total_stat = {}
        self.m_dicts = {'session' : self.m_session_stat,
                        'today'   : self.m_today_stat,
                        'last7'   : self.m_last7_stat,
                        'total'   : self.m_total_stat}
    def _add(self, question, answer):
        for D in [self.m_session_stat, self.m_today_stat,
                  self.m_last7_stat, self.m_total_stat]:
            if question not in D:
                D[question] = {}
            if answer not in D[question]:
                D[question][answer] = 0
            D[question][answer] = D[question][answer] + 1
    def add_correct(self, answer):
        self._add(answer, answer)
    def add_wrong(self, question, answer):
        self._add(question, answer)
    def merge_stat_dicts(self, A, B):
        "Add content of B to A"
        for k in B:
            if k not in A:
                A[k] = {}
            for n in B[k]:
                if n not in A[k]:
                    A[k][n] = 0
                A[k][n] = A[k][n] + B[k][n]
        return A
    def load_statistics(self, datadir):
        """
        datadir is the directory where the statistics files are stored.
        """
        if not usable_statistics:
            return
        for filename in os.listdir(datadir):
            t = int(filename)
            lt = time.localtime(t)
            f = open(os.path.join(datadir, filename), 'r')
            D = pickle.load(f)
            f.close()
            now = int(time.time())
            lt_today = time.localtime(time.time())
            if lt[YEAR] == lt_today[YEAR] and \
                    lt[JULIANDAY] == lt_today[JULIANDAY]:
                self.m_today_stat = self.merge_stat_dicts(self.m_today_stat, D)
            if now - t < 60*60*24*7: # 7 days
                self.m_last7_stat = self.merge_stat_dicts(self.m_last7_stat, D)
            self.m_total_stat = self.merge_stat_dicts(self.m_total_stat, D)
    def get(self):
        """Will return a 0 <= value <= 1.0 that say how many percent is
        correct in this session
        """
        c = t = 0
        for k in self.m_session_stat:
            if k in self.m_session_stat[k]:
                c = c + self.m_session_stat[k][k]
            t = t + self.get_num_guess(self.m_session_stat, k)
        if t > 0:
            return 1.0 * c / t
        else:
            return 0.0
    def display(self):
        print
        v = self.m_session_stat.keys()
        v.sort()
        for x in v:
            print x, self.m_session_stat[x]
    def get_keys(self, all=0):
        """
        by default it returns the keys for all questions that have
        been asked. If 'all' is true, it also includes the keys for
        all the wrong answers.
        """
        keys = []
        for st in self.m_session_stat, self.m_today_stat, self.m_last7_stat, self.m_total_stat:
            for k in st:
                if k not in keys:
                    keys.append(k)
                if all:
                    for wk in self.m_total_stat[k]:
                        if wk not in keys:
                            keys.append(wk)
        keys.sort()
        return keys
    def get_num_guess(self, st, key):
        if key not in st:
            return 0
        t = 0
        for i in st[key]:
            t = t + st[key][i]
        return t
    def get_percentage_correct(self, st, key):
        """
        This was added to be used for harmonic-interval.
        """
        if key not in st:
            return 0.0
        if key in st[key]:
            num_correct = st[key][key]
        else:
            num_correct = 0
        total = 0
        for n in st[key]:
            total = total + st[key][n]
        return num_correct * 100.0 / total
    def key_to_pretty_name(self, k):
        return k
    def get_label_style(self):
        return 'normal'
    def reset_session(self):
        self.m_session_stat = {}


class LessonStatistics(AbstractStatistics):
    def __init__(self, teacher):
        AbstractStatistics.__init__(self, teacher)
        self.m_cur_file = None
    def enter_test_mode(self):
        """
        We assume that the program has saved and emptied the data dict
        before calling this function.
        """
        #FIXME nuke STATISTICS_SAVE_DIR??
        self.m_savepath = os.path.join(gethomedir.get_home_dir(), '.solfege',
                'testresults', self.m_t.m_P.header.lesson_id)
    def exit_test_mode(self):
        """
        We assume that the program has saved and emptied the data dict
        before calling this function.
        """
        self.m_savepath = os.path.join(gethomedir.get_home_dir(), '.solfege',
                'statistics', self.m_t.m_P.header.lesson_id)
    def get_label_style(self):
        return self.m_t.m_P.header.labelformat
    def lessonfile_changed(self, new_file):
        if not new_file:
            return

        self.m_session_stat = {}
        self.m_today_stat = {}
        self.m_last7_stat = {}
        self.m_total_stat = {}
        self.m_cur_file = new_file
        if not usable_statistics:
            return
        self.m_savepath = os.path.join(STATISTICS_SAVE_DIR,
                self.m_t.m_P.header.lesson_id)
        self.create_statistics_dir()

        # if the lessonfile has changed, we have to
        # delete all statistics just to be save.
        if self.get_hash_of_statistics() != self.get_hash_of_lessonfile():
            if os.path.isfile("%s_hash" % self.m_savepath):
                os.remove("%s_hash" % self.m_savepath)
            for f in os.listdir(self.m_savepath):
                os.remove(os.path.join(self.m_savepath, f))
        self.load_statistics(self.m_savepath)
    def store_test_passed(self):
        f = open(os.path.join(self.m_savepath, "passed"), 'w')
        f.write(str(self.get()))
        f.close()
    def save_data(self):
        if not usable_statistics:
            print "Warning: not saving statistics because usable_statistics is False"
            return
        if self.m_session_stat == {}:
            return
        if not os.path.isdir(self.m_savepath):
            os.makedirs(self.m_savepath)
        #save the hashvalue for the lessonfile this statistics was made with
        f = open(os.path.join(self.m_savepath, "..",
            '%s_hash' % self.m_t.m_P.header.lesson_id), 'w')
        f.write(str(self.get_hash_of_lessonfile()))
        f.close()
        f = open(os.path.join(self.m_savepath,
                              str(int(time.time()))), 'w')
        pickle.dump(self.m_session_stat, f)
        f.close()
    def create_statistics_dir(self):
        if not os.path.exists(self.m_savepath):
            os.mkdir(self.m_savepath)
    def get_hash_of_statistics(self):
        # FIXME !wrong function name!
        # get the hash for the content of the lessonfile that was used last
        # time statistics was saved
        if os.path.isfile(os.path.join(self.m_savepath, "..",
            '%s_hash' % self.m_t.m_P.header.lesson_id)):
            s = open(os.path.join(self.m_savepath, "..",
                  '%s_hash' % self.m_t.m_P.header.lesson_id)).read()
            try:
                return int(s)
            except:
                return 0
        else:
            return 0
    def get_hash_of_lessonfile(self):
        return hash(open(self.m_cur_file).read())
    def key_to_pretty_name(self, key):
        for question in self.m_t.m_P.m_questions:
                if question.get_cname() == key:
                    return question.get_name()
        return key

class IntervalStatistics(LessonStatistics):
    def key_to_pretty_name(self, key):
        return utils.int_to_intervalname(key, 1, 1)

class HarmonicIntervalStatistics(LessonStatistics):
    def key_to_pretty_name(self, key):
        return utils.int_to_intervalname(key, 1, 0)

