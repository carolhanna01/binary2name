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


#include "mavlib_cvcomp.h"

/******************************************************************
*
*	VRML library for C++
*
*	Copyright (C) Satoshi Konno 1996-1997
*
*	File:	CJavaVM.h
*
******************************************************************/

#ifndef _CJAVAVM_H_
#define _CJAVAVM_H_

#ifdef SUPPORT_JSAI

#include <stdio.h>
#include <stdarg.h>
#include <jni.h>

JavaVM	*GetJavaVM(void);
JNIEnv	*GetJniEnv(void);
void	SetJavaVM(JavaVM *jvm);
void	SetJniEnv(JNIEnv *jniEnv);
void	CreateJavaVM(char *classpath = NULL, jint (JNICALL *printfn)(FILE *fp, const char *format, va_list args) = NULL);
void	DeleteJavaVM(void);

class CJavaVM { 

public:
	JavaVM *getJavaVM(void) {
		return GetJavaVM();
	}

	JNIEnv *getJniEnv(void) {
		return GetJniEnv();
	}
};

#endif

#endif

