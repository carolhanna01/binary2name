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

define( 'DEBUG', false );

const DB_HOST = 'localhost';
const DB_USER = 'gnurc_user';
const DB_PASS = 'default';
const DB_NAME = 'gnurc';
const DB_TZ_NAME = true;

const WEB_ROOT = '/gnurc_v2/web';

const HTML_VERSION = HTML5;
const HTML_PRETTY = false;

const DEFAULT_SCALE = FAHRENHEIT;

define( 'DEFAULT_TIMEZONE', 'Australia/Sydney' );

const WRITE_ATTEMPTS = 3;
const READ_ATTEMPTS = 3;

const RECOVER_FROM_FACTORY_RESET = true;
