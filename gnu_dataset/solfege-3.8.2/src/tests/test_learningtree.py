# Solfege - free ear training software
# Copyright (C) 2007 Tom Cato Amundsen
# License is GPL, see file COPYING

import unittest
from src.learning_tree_editor import LearningTree

class TestLearningTree(unittest.TestCase):
    def testCreate(self):
        tree = LearningTree(None)
        tree.new_menu("Menu 1")
    def create_menu(self):
        tree = LearningTree(None)
        tree.new_menu("Menu 1")
        tree.new_topic(0, "Submenu 0")
        tree.new_topic(0, "Submenu 1")
        tree.new_menu("Menu 2")
        tree.new_topic(1, "Submenu 1-0")
        tree.new_topic(1, "Submenu 1-1")
        return tree
    def test_move_elem_up(self):
        tree = self.create_menu()
        self.assertEqual(tree.get((0, 0))['name'], 'Submenu 0')
        self.assert_(tree.move_elem_up((0, 1)))
        self.assertEqual(tree.get((0, 0))['name'], 'Submenu 1')
        self.assertEqual(tree.get((0, 1))['name'], 'Submenu 0')
        self.assertEqual(tree.move_elem_up((3, 3)), False, 
            'Should fail. Invalid path given to function.')
        self.assertEqual(tree.move_elem_up((1, 0)), False,
            'Should fail. Trying to move the first element up.')
        self.assertEqual(tree.move_elem_up((1,)), False,
            'Should fail becuase move_elem_up cannot move toplevel menus')
    def test_move_elem_to_prev_menu(self):
        tree = self.create_menu()
        self.assertEqual(tree.move_elem_to_prev_menu((1, 0)), (0, 2))
        self.assertEqual(tree.get((0, 2))['name'], 'Submenu 1-0')
    def test_iterate_all_lessons(self):
        tree = LearningTree(None)
        tree.new_menu("Menu 1")
        tree.new_topic(0, "Submenu 0")
        tree.add_lesson((0, 0), "id1")
        tree.add_lesson((0, 0), "id2")
        tree.add_lesson((0, 0), "id3")
        v = list(tree.iterate_all_lessons())
        self.assertEqual(v, ['id1', 'id2', 'id3'])
    def test_iterate_all_lessons2(self):
        tree = LearningTree(None)
        tree.new_menu("Menu 1")
        tree.new_topic(0, "Submenu 0")
        tree.add_lesson((0, 0), "id1")
        tree.add_lesson((0, 0), "id2")
        tree.add_lesson((0, 0), "id3")
        tree.new_topic(0, "Submenu 1")
        tree.add_lesson((0, 1), "id11")
        tree.add_lesson((0, 1), "id12")
        tree.add_lesson((0, 1), "id13")
        v = list(tree.iterate_all_lessons2())
        self.assertEqual(v, [('id1', (0, 0, 0)), ('id2', (0, 0, 1)), ('id3', (0, 0, 2)), ('id11', (0, 1, 0)), ('id12', (0, 1, 1)), ('id13', (0, 1, 2))])

suite = unittest.makeSuite(TestLearningTree)

