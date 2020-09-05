<?php
/*

Copyright (C) 2012-2015 GNU remotecontrol authors.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

const UINT8_MIN  = 0;
const UINT8_MAX  = 255;
const UINT16_MIN = 0;
const UINT16_MAX = 65535;
const UINT24_MIN = 0;
const UINT24_MAX = 16777215;
const UINT32_MIN = 0;
const UINT32_MAX = 4294967295;
const UINT64_MIN = 0;
const UINT64_MAX = 9223372036854775807; // PHP signed ints can't support greater

const INT8_MIN  = -128;
const INT8_MAX  =  127;
const INT16_MIN = -32768;
const INT16_MAX =  32767;
const INT24_MIN = -8388608;
const INT24_MAX =  8388607;
const INT32_MIN = -2147483648;
const INT32_MAX =  2147483647;
// minus 1 because otherwise it's parsed as a float
//const INT64_MIN = -9223372036854775808;
//const INT64_MIN = -9223372036854775807;
define(
  'INT64_MIN',    -9223372036854775807 - 1
);
const INT64_MAX =  9223372036854775807;

// for use with getimagesize:
const X = 0;
const Y = 1;

const UTF8_ENCODING = 'UTF-8';
const ASCII_ENCODING = 'ISO-8859-1';

const CR = "\r";
const LF = "\n";

const HTML4 = '4';
const XHTML = 'X';
const HTML5 = '5';

/*
const EXCEPTION_CAUGHT = 'caught';
const EXCEPTION_CREATED = 'created';
*/


const FAHRENHEIT = '1';
const CELSIUS = '2';
