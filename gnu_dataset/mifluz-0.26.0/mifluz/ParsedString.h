//
// ParsedString.h
//
// ParsedString: Contains a string. The string my contain $var, ${var}, $(var)
//               `filename`. The get method will expand those using the
//               dictionary given in argument.
// 
// Part of the ht://Dig package   <http://www.htdig.org/>
// Copyright (c) 1999, 2000, 2001 The ht://Dig Group
// For copyright details, see the file COPYING in your distribution
// or the GNU General Public License version 2 or later 
// <http://www.gnu.org/copyleft/gpl.html>
//
// $Id: ParsedString.h,v 1.5 2001/06/29 14:14:08 loic Exp $

#ifndef _ParsedString_h_
#define _ParsedString_h_

#include "Object.h"
#include "htString.h"
#include "Dictionary.h"

class ParsedString : public Object
{
public:
	//
	// Construction/Destruction
	//
					ParsedString();
					ParsedString(const String& s);
					~ParsedString();

	void			set(const String& s);
	const String		get(const Dictionary &d) const;
private:
	String			value;

	void			getFileContents(String &str, const String& filename) const;
};

#endif
