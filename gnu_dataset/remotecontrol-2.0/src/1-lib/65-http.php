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

function get_thermostat_sort( &$sort_col = null, &$sort_dir = null ) {
  
  $sort = 'id';
  
  if ( isset( $_GET[ 'thermostat_sort' ] ) ) {
    
    $sort = $_GET[ 'thermostat_sort' ];
    
  }
  
  if ( strlen( $sort ) <= 0 ) {
    
    die( "Invalid thermostat sort." );
    
  }
  
  $sort_dir = ( $sort[ 0 ] === '-' ? 'desc' : 'asc' );
  $sort_col = ltrim( $sort, '-' );

  return $sort;
  
}

function get_current_url() {
  
  $scheme = read_a( $_SERVER, 'REQUEST_SCHEME' );
  $host = read_a( $_SERVER, 'HTTP_HOST' );
  $path = read_a( $_SERVER, 'SCRIPT_NAME' );
  $query = http_build_query( $_GET );

  $result = "$scheme://$host$path";
  
  if ( $query ) { $result = "$result?$query"; }
  
  return $result;
  
}

function get_meta_redirect( $target_url ) {
  
  $scheme = read_a( $_SERVER, 'REQUEST_SCHEME' );
  $host = read_a( $_SERVER, 'HTTP_HOST' );
  $path = WEB_ROOT . '/meta-refresh.php';
  $query = array( 'goto' => $target_url );
  $query = http_build_query( $query );
  
  return "$scheme://$host$path?$query";
  
}

function redirect( $script = null, $query = array(), $fragment = '' ) {

  // make sure session has written:
  // http://php.net/manual/en/function.session-write-close.php
  session_write_close();
  
  $update_query = false;
  
  if ( $script === null || strlen( $script ) === 0 ) {
  
    $link = get_link();
    
    foreach ( $query as $key => $value ) {
      
      $link->set( $key, $value );
      
    }

    $result = $link->to_string();
    
    header( "Location: $result" );
    exit;
    
  }
  else if ( $script[ 0 ] === '/' ) {
      
    $script = WEB_ROOT . $script;
    
    $update_query = true;
    
  }
  else {
    
    header( "Location: $script" );
    exit;
    
  }
  
  if ( $update_query ) {
    
    global $global_attrs;

    foreach ( $global_attrs as $attr ) {

      if ( ! array_key_exists( $attr, $query ) ) {

        $value = read_g( $attr, null );

        if ( $value !== null ) {

          $query[ $attr ] = $value;

        }
      }
    }
  }
    
  $scheme = read_a( $_SERVER, 'REQUEST_SCHEME' );
  $host = read_a( $_SERVER, 'HTTP_HOST' );
  $query = http_build_query( $query );
  $result = "$scheme://$host$script";
  if ( strlen( $query ) ) { $result .= "?$query"; }
  if ( strlen( $fragment ) ) { $result .= "#$fragment"; }
  header( "Location: $result" );
  exit;
}

function is_post() {
  
  $result = (read_a( $_SERVER, 'REQUEST_METHOD' ) === 'POST');

  if ( $result ) {

    // Note: check xsrf token in here in order to ensure it's passed.
    
    if ( read_p( 'xsrf', false ) != session()->get_xsrf_token() ) {
      
      throw Error( 'Invalid XSRF token.' );
      
    }
  }
  
  return $result;
  
}
