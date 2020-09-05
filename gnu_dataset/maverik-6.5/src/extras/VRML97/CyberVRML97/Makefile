
VRML97_OBJS = 	AudioClipNode.o JNode.o SFInt32.o BillboardNode.o JScript.o \
		SFMatrix.o BoundingBox.o JavaVM.o SFNode.o BoxNode.o LodNode.o \
		SFRotation.o ConeNode.o MFColor.o SFString.o CylinderNode.o \
		MFFloat.o SFTime.o DefNode.o MFInt32.o SFVec2f.o ElevationGridNode.o \
		MFRotation.o SFVec3f.o Event.o MFString.o ExtrusionNode.o MFTime.o \
		SceneGraph.o Field.o MFVec2f.o ScriptNode.o MFVec3f.o \
		SphereNode.o FileImage.o MField.o SwitchNode.o FileJPEG.o Node.o \
		TextNode.o FilePNG.o Parser.o TextureTransformNode.o FileTarga.o \
		PointSetNode.o TimeSensorNode.o FontStyleNode.o Proto.o TransformNode.o \
		GeometryNode.o ProximitySensorNode.o UrlFile.o GroupingNode.o \
		Route.o ImageTextureNode.o SFBool.o IndexedFaceSetNode.o SFColor.o \
		vrmlparser.o IndexedLineSetNode.o SFFloat.o vrmlsetinfo.o InlineNode.o \
		SFImage.o mav_vrml97.o

CXXFLAGS= -I../../../../incl ${CXXFLAG}

../../../../lib/libmav_vrml97.so:	vrml.tab.o vrml.yy.o ${VRML97_OBJS}
	${CXX} ${CXXFLAG} -shared -o libmav_vrml97.so vrml.tab.o vrml.yy.o ${VRML97_OBJS}
	cd ../../../../lib/; \
	mv ../src/extras/VRML97/CyberVRML97/libmav_vrml97.so libmav_vrml97-CyberVRML97.so; \
	rm -f libmav_vrml97.so; \
	ln -s libmav_vrml97-CyberVRML97.so libmav_vrml97.so

.SUFFIXES:.cxx

.cxx.o:
	${CXX} ${CXXFLAGS} -c $<

vrml.tab.cxx:	vrml.y
		${YACC} -d vrml.y 2>/dev/null
		mv y.tab.c vrml.tab.cxx
		mv y.tab.h vrml.tab.h

vrml.yy.cxx:	vrml.l
		${LEX} vrml.l
		mv lex.yy.c vrml.yy.cxx


clean:
	rm -f ${VRML97_OBJS} vrml.tab.o vrml.tab.cxx vrml.tab.h vrml.yy.o 
	rm -f vrml.yy.cxx *.bck *.*~ core

distclean: clean
	rm -f ../../../../lib/libmav_vrml97.so

