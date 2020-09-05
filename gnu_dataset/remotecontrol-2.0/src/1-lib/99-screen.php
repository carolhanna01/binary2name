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

if ( CHECK_USER && ! get_username() ) {
  header( 'WWW-Authenticate: Basic realm="GNU Remote Control - User Authentication Required"' );
  header( 'HTTP/1.0 401 Unauthorized' );
  print "Sorry - you need valid credentials to access this facility.\n";
  exit;
}

if ( read_a( $_SERVER, 'REQUEST_SCHEME' ) === null ) {
  
  $_SERVER[ 'REQUEST_SCHEME' ] = read_a( $_SERVER, 'HTTPS' ) ? 'https' : 'http';
  
}

if ( bom()->user->count() === 0 ) {
  
  bom()->user->add_admin( get_username(), 0, 1234, 'en' );
  
}

if ( ! defined( 'TEST' ) ) {

  $tz = read_a( $_GET, 'tz' );
  
  define( 'TIMEZONE', $tz ? $tz : DEFAULT_TIMEZONE );
  
  /*
  $lang = read_a( $_GET, 'lang' );
  
  if (
    ! NO_LANG_CHECK &&
    $lang !== 'en' &&
    (
      $lang === null ||
      ! array_key_exists( $lang, intl()->lang )
    )
  ) {
    
    $accept = intl()->parse_accept_header();

    $_GET[ 'lang' ] = vector_1st_value( $accept );
    
    redirect( null, $_GET );
    
    $lang = vector_1st_value( $accept );
    
  }
  */
}
else {
  
  define( 'TIMEZONE', DEFAULT_TIMEZONE );
  
}
