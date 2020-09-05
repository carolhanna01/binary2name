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

// Table heading HTML
function TH( $message ) {

  $args = func_get_args();

  if ( ( count( $args ) % 2 ) === 1 ) { $args[] = CONTEXT_TABLE_HEADING; }
  
  return intl()->format( 'html', $args );
  
}

// Table heading HTML 01n pattern
function TH01n(
  $msg_0,
  $msg_1,
  $msg_n,
  $number,
  $context = CONTEXT_TABLE_HEADING
) {

  return intl()->format_01n( 'html', $msg_0, $msg_1, $msg_n, $number, $context );
  
}

// HTML content
function H( $message ) {

  $args = func_get_args();
  
  return intl()->format( 'html', $args );
  
}

// HTML 01n pattern
function H01n(
  $msg_0,
  $msg_1,
  $msg_n,
  $number,
  $context = CONTEXT_GLOBAL
) {

  return intl()->format_01n( 'html', $msg_0, $msg_1, $msg_n, $number, $context );
  
}

// HTML attribute content
function A( $message ) {

  $args = func_get_args();
  
  return intl()->format( 'attr', $args );
  
}

// HTML attribute 01n pattern
function A01n(
  $msg_0,
  $msg_1,
  $msg_n,
  $number,
  $context = CONTEXT_GLOBAL
) {

  return intl()->format_01n( 'attr', $msg_0, $msg_1, $msg_n, $number, $context );
  
}

// Text content
function T( $message ) {

  $args = func_get_args();
  
  return intl()->format( 'text', $args );
  
}

// Text 01n pattern
function T01n(
  $msg_0,
  $msg_1,
  $msg_n,
  $number,
  $context = CONTEXT_GLOBAL
) {

  return intl()->format_01n( 'text', $msg_0, $msg_1, $msg_n, $number, $context );
  
}

// Safe Text content
function S( $message ) {

  $args = func_get_args();
  
  return intl()->format( 'safe', $args );
  
}

// Safe Text 01n pattern
function S01n(
  $msg_0,
  $msg_1,
  $msg_n,
  $number,
  $context = CONTEXT_GLOBAL
) {

  return intl()->format_01n( 'safe', $msg_0, $msg_1, $msg_n, $number, $context );
  
}
