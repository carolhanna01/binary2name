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

function new_xsrf_token() {
  
  return sha1( uniqid( 'hmm-salty', true ) . time() );
  
}

function throw_if( $test /* code, paramters..., context?, $previous? */ ) {
  
  if ( ! $test ) { return; }
    
  $args = func_get_args();

  array_shift( $args );

  if ( count( $args ) === 0 ) {

    $args[] = 'An unexpected error occurred.';

  }

  throw err()->get_error( $args );

}

function default_redirect( $path = null ) {
  
  global $default_redirect;

  if ( $path ) { $default_redirect = $path; }

  if ( $default_redirect === null ) { $default_redirect = '/home.php'; }
  
  return $default_redirect;
    
}

function redirect_if( $test, $path = null ) {

  return verify( ! $test, $path );
    
}

function verify( $test, $path = null ) {

  if ( user()->deleted ) { throw Error( 'User Deleted' ); }
  
  default_redirect( $path );
  
  if ( $test ) { return; }

  if ( $path ) { redirect( default_redirect() ); }

  throw Error( 'Unauthorized Access' );
  
  /*
  render_head( A( 'Unauthorized Access' ) );
  html()->tail->
    p()->
      text( 'You are not authorized to access this page.' )->
    p_end();
  render_foot();
  html()->render();
  exit();
  */
  
}

function get_username() {
  
  return read_a( $_SERVER, 'PHP_AUTH_USER' );
  
}

function is_translator() {
  
  if ( user()->deleted ) { return false; }

  return boolval( user()->is_translator );
  
}

function is_admin() {
  
  return is_administrator();
  
}

function is_administrator() {
  
  if ( user()->deleted ) { return false; }
  
  return boolval( user()->is_admin );
  
}
