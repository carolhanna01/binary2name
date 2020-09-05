# -*- coding: iso-8859-1 -*-
# GNU Solfege - free ear training software
# Copyright (C) 2005, 2006, 2007 Tom Cato Amundsen
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


import weakref

class NodeNotFound(Exception):
    pass

class Cursor(object):
    def __init__(self, node):
        """
        node -- The node the cursor will be pointing to when created.
        """
        self.m_tree = node
        self.m_pos = []
        while 1:
            try:
                self.m_tree = self.m_tree.parent()
            except AttributeError:
                break
    def new_point_to_node(node):
        c = Cursor(node)
        c.goto_node(node)
        return c
    new_point_to_node = staticmethod(new_point_to_node)
    def _find_node(self):
        """
        Return the node for self.m_pos. Will raise IndexError if
        self.m_pos points to a node that does not exist.
        """
        if self.m_pos == []:
            return self.m_tree
        node = self.m_tree.m_children[self.m_pos[0]]
        for x in self.m_pos[1:]:
            node = node.m_children[x]
        return node
    def get(self):
        return self._find_node()
    def get_prev(self):
        """
        Return the prev node, without changing the cursor.
        Return None if we are on the first node.
        """
        c = self.copy()
        if c.go_prev():
            return c.get()
    def get_parent(self):
        """
        Return the parent node, without changing the cursor.
        Return None if we are on the first node.
        """
        c = self.copy()
        if c.go_parent():
            return c.get()
    def goto_last_child(self):
        """
        Go to the last child in m_children.
        """
        assert self._find_node().m_children
        self.m_pos.append(len(self._find_node().m_children)-1)
    def goto_node(self, node):
        """
        Move the pointer to the node "node".
        Return True if successful.
        Raise NodeNotFound if we cannot find the node
        """
        while self.go_next():
            if self.get() == node:
                return True
        raise NodeNotFound(node)
    def go_next(self):
        """
        Go to the next node.
        Return True if we can go next.
        Return False without changing the cursor if we are on the last node.
        """
        p = self.get()
        if p.m_children:
            self.m_pos.append(0)
            return True
        else:
            # Check if the m_children we are in has more lements.
            # If so return True
            # Go up one level
            # until []

            # First go from [0,0,0] to [0,0,1]
            try:
                c = self.copy()
                c.m_pos[-1] += 1
                c._find_node()
                self.m_pos[-1] += 1
                return True
            except IndexError:
                # Then try [0, 1]
                while len(c.m_pos) > 0:
                    try:
                        c.m_pos = c.m_pos[:-1]
                        c.m_pos[-1] += 1
                        c._find_node()
                        self.m_pos = c.m_pos[:]
                        return True
                    except:
                        pass
        return False
    def go_prev(self):
        """
        Go to the previous node.
        Return True if sucessful.
        Return False without changing the cursor if we are on the first node.
        """
        if not self.m_pos:
            return False
        if self.m_pos[-1] > 0:
            self.m_pos[-1] -= 1
            if self._find_node().m_children:
                # go to last node of child
                while self._find_node().m_children:
                    self.m_pos.append(len(self._find_node().m_children)-1)
        else:
            self.m_pos = self.m_pos[:-1]
        return True
    def go_parent(self):
        """
        Go to the parent node.
        Return True if sucessfull.
        Return False if we have no parent node to move to.
        """
        if self.m_pos != []:
            self.m_pos = self.m_pos[:-1]
            return True
        return False
    def goto(self, pos):
        """
        Go to pos.
        Don't change, and raise IndexError if pos is not valid.
        """
        c = self.copy()
        c.m_pos = pos
        c._find_node()
        self.m_pos = pos
    def go_prev_sibling(self):
        """
        Given the tree
        A
         B
          C
         D
          E
        and we are at pos D, go_prev_sibling will move the cursor to B
        Return True on success.
        Return False if there is no prev sibling
        """
        level = len(self.m_pos)
        c = self.copy()
        while c.go_prev():
            if len(c.m_pos) == level:
                self.m_pos = c.m_pos[:]
                return True
        return False
    def get_prev_sibling(self):
        c = self.copy()
        if c.go_prev_sibling():
            return c.get()
    def go_next_sibling(self):
        """
        Given the tree
        A
         B
          C
         D
          E
        and we are at pos B, go_prev_sibling will move the cursor to D
        Return True on success.
        Return False if there is no prev sibling
        """
        level = len(self.m_pos)
        c = self.copy()
        while c.go_next():
            if len(c.m_pos) == level:
                self.m_pos = c.m_pos[:]
                return True
        return False
    def get_next_sibling(self):
        c = self.copy()
        if c.go_next_sibling():
            return c.get()
    def is_child_of(self, *names):
        """
        names -- the names to test for
        Return the name of the found parent.
        Return None if not ancestor.
        """
        if type(names[0]) == tuple:
            names = names[0]
        c = self.copy()
        while c.go_parent():
            if c.get().m_name in names:
                return c.get().m_name
        return None
    def copy(self):
        """Return a copy of ourselves."""
        c = Cursor(self.m_tree)
        c.m_pos = self.m_pos[:]
        return c
    def insert(self, node):
        """
        Insert the node before the node we are pointing to. Self will
        point to the newly inserted node.
        """
        c = self.copy()
        c.go_parent()
        c.get().m_children.insert(self.m_pos[-1], node)
    def pop(self):
        """
        Remove the element pointed at, and return it.
        The cursor will move to the node before the deleted node.
        """
        node = self.get()
        c = self.copy()
        c.go_parent()
        del_idx = self.m_pos[-1]
        self.go_prev()
        del c.get().m_children[del_idx]
        return node
    def delete(self):
        """
        Delete the node we are pointing too.
        The node should have no children.
        We will move to the node before the deleted node.
        Raise IndexError if we try to delete the toplevel node.
        """
        node = self.get()
        assert not node.m_children
        c = self.copy()
        if c.go_parent():
            del_idx = self.m_pos[-1]
            self.go_prev()
            del c.get().m_children[del_idx]
        else:
            # We are on the top level
            raise IndexError("We cannot delete the toplevel node.")
            pass
    def last(self):
        """
        return the last node in the tree.
        raise IndexError if the tree is empty.
        """
        self.m_pos = []
        node = self.m_tree
        while node.m_children:
            self.m_pos.append(len(node.m_children)-1)
            node = node.m_children[-1]

class TreeNode(object):
    def __init__(self, name='noname'):
        self.m_name = name
        self.m_children = []
    def add(self, child):
        child.parent = weakref.ref(self)
        self.m_children.append(child)
    def show(self, level=0):
        print "    "*level, self, self.m_name
        for n in self.m_children:
            n.show(level + 1)
    def iterate_tree(self):
        yield self
        for n in self.m_children:
            for x in n.iterate_tree():
                yield x
    def iterate_children(self):
        for n in self.m_children:
            for x in n.iterate_tree():
                yield x



