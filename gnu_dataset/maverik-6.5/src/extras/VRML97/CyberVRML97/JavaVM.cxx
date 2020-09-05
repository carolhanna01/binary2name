/*
   GNU Maverik - a system for managing display and interaction in 
              Virtual Environment applications.
   Copyright (C) 2008  Advanced Interfaces Group

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.

   The authors can be contacted via:
   www   - http://aig.cs.man.ac.uk
   email - maverik@aig.cs.man.ac.uk
   mail  - Advanced Interfaces Group, Room 2.94, Kilburn Building, 
        University of Manchester, Manchester, M13 9PL, UK
*/

/******************************************************************
*
*	VRML library for C++
*
*	Copyright (C) Satoshi Konno 1996-1997
*
*	File:	JavaVM.cpp
*
******************************************************************/

#ifdef SUPPORT_JSAI

#include <stdlib.h>
#include <assert.h>
#include "CJavaVM.h"

static	JavaVM	*gJavaVM	= NULL;
static	JNIEnv	*gJavaEnv	= NULL;

JavaVM *GetJavaVM(void) 
{
	return gJavaVM;
}

JNIEnv *GetJniEnv(void) 
{
	return gJavaEnv;
}

void SetJavaVM(JavaVM *jvm) 
{
	gJavaVM = jvm;
}

void SetJniEnv(JNIEnv *jniEnv) 
{
	gJavaEnv = jniEnv;
}

void CreateJavaVM(char *classpath, jint (JNICALL *printfn)(FILE *fp, const char *format, va_list args))
{
#ifdef USE_JDK12
	if (!gJavaVM && !gJavaEnv) {

		JavaVMOption options[2];
		JavaVMInitArgs vm_args;

		int	nOptions = 1;

		options[0].name = "classpath";
		if (!classpath)
			options[0].value.p = getenv("CLASSPATH");
		else
			options[0].value.p = classpath;

		if (printfn) {
			options[1].name = "vfprintf";
			options[1].value.p = printfn;
			nOptions++;
		}

		vm_args.version = JNI_VERSION_1_2;
		vm_args.options = options;
		vm_args.nOptions = nOptions;
		vm_args.result = NULL;


		JNI_CreateJavaVM(&gJavaVM, (void **)&gJavaEnv, (void *)&vm_args);
		assert(gJavaVM && gJavaEnv);
	}
#else
	if (!gJavaVM && !gJavaEnv) {
		JDK1_1InitArgs vm_args;
		JNI_GetDefaultJavaVMInitArgs(&vm_args);

		if (!classpath)
			vm_args.classpath = getenv("CLASSPATH");
		else
			vm_args.classpath = classpath;

		if (printfn)
			vm_args.vfprintf = printfn;

		JNI_CreateJavaVM(&gJavaVM, &gJavaEnv, &vm_args);

		assert(gJavaVM && gJavaEnv);
	}
#endif
}

void DeleteJavaVM(void) 
{
	if (gJavaVM)
		gJavaVM->DestroyJavaVM();
}

#endif //JMC
