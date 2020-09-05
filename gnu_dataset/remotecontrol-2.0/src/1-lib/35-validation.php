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

function is_valid_text( $str, $min_length = 0, $max_length = INT32_MAX ) {
  if ( ! is_string( $str ) ) { return false; }
  $length = mb_strlen( $str );
  if ( $length < $min_length ) { return false; }
  if ( $length > $max_length ) { return false; }
  return (bool) preg_match( '//u', $str );
}

function is_valid_safe( $str, $min_length = 0, $max_length = INT32_MAX ) {
  if ( ! is_valid_text( $str, $min_length, $max_length ) ) { return false; }
  if ( preg_match( '/[<>\'"&]/', $str ) ) { return false; }
  return true;
}

function is_valid_html( $str, $min_length = 0, $max_length = INT32_MAX ) {
  if ( ! is_valid_text( $str, $min_length, $max_length ) ) {
    return false;
  }
  $delim = preg_replace( '/[^<>]*/', '', $str );
  $delim = str_replace( '<>', '', $delim );
  if ( strlen( $delim ) !== 0 ) {
    return false;
  }
  $str = strip_tags( $str );
  $str = mb_strtolower( $str, 'UTF-8' );
  $str = str_replace( '&lt;', '', $str );
  $str = str_replace( '&gt;', '', $str );
  $str = str_replace( '&amp;', '', $str );
  $str = str_replace( '&apos;', '', $str );
  $str = html_decode( $str );
  $result = ! (bool) preg_match( '/[<>&]+/', $str, $matches );
  return $result;
}

function is_valid_attr( $str, $min_length = 0, $max_length = INT32_MAX ) {
  if ( ! is_valid_text( $str, $min_length, $max_length ) ) { return false; }
  $str = mb_strtolower( $str, 'UTF-8' );
  //$str = str_replace( '&amp;', '', $str );
  //$str = html_decode( $str );
  //return ! (bool) preg_match( '/[<>&\'"]+/', $str );
  return ! (bool) preg_match( '/[<>\'"]+/', $str );
}

function is_valid_ascii_printable(
  $str,
  $min_length = 0,
  $max_length = INT32_MAX
) {
  if ( ! is_valid_text( $str, $min_length, $max_length ) ) { return false; }
  return ! (bool) preg_match( '/[^\x20-\x7e]/', $str );
}

function is_valid_ascii_username(
  $str,
  $min_length = 3,
  $max_length = 20
) {
  if ( ! is_valid_ascii_printable( $str, $min_length, $max_length ) ) { return false; }
  return (bool) preg_match( '/^[a-z][a-z0-9_]*$/', $str );
}

function is_valid_ascii_variable(
  $str,
  $min_length = 1,
  $max_length = INT32_MAX
) {
  if ( ! is_valid_ascii_printable( $str, $min_length, $max_length ) ) { return false; }
  return (bool) preg_match( '/^[a-z_][a-z0-9_]*$/', $str );
}

function is_valid_ascii_const(
  $str,
  $min_length = 1,
  $max_length = INT32_MAX
) {
  if ( ! is_valid_ascii_printable( $str, $min_length, $max_length ) ) { return false; }
  return (bool) preg_match( '/^[A-Z][A-Z0-9_]*$/', $str );
}

function is_valid_md5( $hash ) {
  if ( ! is_valid_ascii_printable( $hash, 32, 32 ) ) { return false; }
  return (bool) preg_match( '/^[a-z0-9]{32}$/', $hash );
}

function is_valid_intl_type( $type ) {
  if ( ! is_valid_ascii_printable( $type, 4, 4 ) ) { return false; }
  static $types = array(
    'html' => true,
    'attr' => true,
    'text' => true,
    'safe' => true,
  );
  return array_key_exists( $type, $types );
}

function is_valid_intl_context( $context ) {
  if ( ! is_valid_ascii_variable( $context, 1, 64 ) ) { return false; }
  return intl()->is_valid_context( $context );
}
