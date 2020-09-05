
import abstract
import lessonfile
import gtk

class Teacher(abstract.Teacher):
    def __init__(self, exname, app):
        abstract.Teacher.__init__(self, exname, app)
        self.lessonfileclass = lessonfile.IdByNameLessonfile

class Gui(abstract.LessonbasedGui):
    def __init__(self, teacher, window):
        abstract.LessonbasedGui.__init__(self, teacher, window, True)

