PKGLIST = \
gnu.gleem \
gnu.gleem.linalg

FILELIST = \
gnu/gleem/BSphere.java \
gnu/gleem/BSphereProvider.java \
gnu/gleem/CameraParameters.java \
gnu/gleem/ExaminerViewer.java \
gnu/gleem/HandleBoxManip.java \
gnu/gleem/HitPoint.java \
gnu/gleem/Manip.java \
gnu/gleem/ManipManager.java \
gnu/gleem/ManipMotionListener.java \
gnu/gleem/ManipPart.java \
gnu/gleem/ManipPartCube.java \
gnu/gleem/ManipPartGroup.java \
gnu/gleem/ManipPartLineSeg.java \
gnu/gleem/ManipPartSquare.java \
gnu/gleem/ManipPartTransform.java \
gnu/gleem/ManipPartTriBased.java \
gnu/gleem/ManipPartTwoWayArrow.java \
gnu/gleem/NormalCalc.java \
gnu/gleem/RayTriangleIntersection.java \
gnu/gleem/RightTruncPyrMapping.java \
gnu/gleem/ScreenToRayMapping.java \
gnu/gleem/Translate1Manip.java \
gnu/gleem/Translate2Manip.java \
gnu/gleem/linalg/DimensionMismatchException.java \
gnu/gleem/linalg/IntersectionPoint.java \
gnu/gleem/linalg/Line.java \
gnu/gleem/linalg/Mat2f.java \
gnu/gleem/linalg/Mat3f.java \
gnu/gleem/linalg/Mat4f.java \
gnu/gleem/linalg/Matf.java \
gnu/gleem/linalg/MathUtil.java \
gnu/gleem/linalg/NonSquareMatrixException.java \
gnu/gleem/linalg/Plane.java \
gnu/gleem/linalg/PlaneUV.java \
gnu/gleem/linalg/Rotf.java \
gnu/gleem/linalg/SingularMatrixException.java \
gnu/gleem/TestExaminerViewer.java \
gnu/gleem/TestHandleBox.java \
gnu/gleem/TestMultiWin.java \
gnu/gleem/TestTranslate1.java \
gnu/gleem/TestTranslate2.java \
gnu/gleem/linalg/Vec2f.java \
gnu/gleem/linalg/Vec3d.java \
gnu/gleem/linalg/Vec3f.java \
gnu/gleem/linalg/Vec4f.java \
gnu/gleem/linalg/Vecf.java \
gnu/gleem/linalg/Veci.java

JAVADOCTITLE  = 'gleem API Documentation'
JAVADOCWINDOWTITLE  = 'gleem'

default all:
	@javac -source 1.4 -deprecation -g -d . $(FILELIST)

clean:
	rm gnu/gleem/*.class gnu/gleem/linalg/*.class

gleem.jar:
	jar cvf gleem.jar gnu/gleem/*.class gnu/gleem/linalg/*.class

docs:
	@javadoc -source 1.4                        \
                 -d doc                             \
                 -doctitle $(JAVADOCTITLE)          \
                 -windowtitle $(JAVADOCWINDOWTITLE) \
                 -overview overview.html            \
                 $(PKGLIST)
