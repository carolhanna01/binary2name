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

function is_date( $value ) {
  
  static $patterns = array(
    '/^\d\d\d\d-\d\d-\d\d$/',
  );
  
  if ( ! is_string( $value ) ) {
    
    try {
      
      $value = strval( $value );
      
    }
    catch ( Exception $ex ) {

      return false;
      
    }
  }
  
  foreach ( $patterns as $pattern ) {
  
    if ( preg_match( $pattern, $value ) ) { return true; }
    
  }
  
  return false;
  
}

function is_time( $value ) {
  
  static $patterns = array(
    '/^\d\d:\d\d$/',
    '/^\d\d:\d\d:\d\d$/',
  );
  
  if ( ! is_string( $value ) ) {
    
    try {
      
      $value = strval( $value );
      
    }
    catch ( Exception $ex ) {

      return false;
      
    }
  }
  
  foreach ( $patterns as $pattern ) {
  
    if ( preg_match( $pattern, $value ) ) { return true; }
    
  }
  
  return false;
  
}

function is_datetime( $value ) {
  
  $result = is_object( $value ) && is_a( $value, 'DateTime' );

  if ( $result ) { return true; }
  
  static $patterns = array(
    '/^\d\d\d\d-\d\d-\d\d \d\d:\d\d$/',
    '/^\d\d\d\d-\d\d-\d\d \d\d:\d\d:\d\d$/',
  );
  
  if ( ! is_string( $value ) ) {
    
    try {
      
      $value = strval( $value );
      
    }
    catch ( Exception $ex ) {

      return false;
      
    }
  }
  
  foreach ( $patterns as $pattern ) {
  
    if ( preg_match( $pattern, $value ) ) { return true; }
    
  }
  
  return false;
  
}
