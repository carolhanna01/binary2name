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

define_default( 'DEBUG', false );

define_default( 'DB_TZ_NAME', false );

define_default( 'CHECK_USER', true );

define_default( 'AJAX', false );

define_default( 'NO_LANG_CHECK', AJAX );

define_default( 'DEFAULT_TIMEZONE', 'UTC' );

define_default( 'DUMP_PATH', '/tmp/grc-debug.log' );

define_default( 'TRUNCATE_LENGTH', 57 );

define_default( 'DEFAULT_PAGE_SIZE', 50 );

define_default( 'WRITE_ATTEMPTS', 3 );
define_default( 'READ_ATTEMPTS', 3 );
define_default( 'NET_TIMEOUT', 10 );

define_default( 'RECOVER_FROM_FACTORY_RESET', false );

// see here for commentary:
// http://www.sitepoint.com/why-you-should-use-bcrypt-to-hash-stored-passwords/
// note: must begin with '$2a$20$' then have 22 alphabetic letters
//                                              1234567890123456789012
define_default( 'BCRYPT_PASSWORD_SALT', '$2a$20$UAu7Afu6yTDpKDpdqFa7hD$' );

function define_default( $define, $default ) {
  
  if ( defined( $define ) ) { return; }
  
  define( $define, $default );
  
}
