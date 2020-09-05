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

function sum( &$table, $column ) {
  $result = 0;
  foreach ( $table as $index => &$row ) {
    $result += intval( read_a( $row, $column ) );
  }
  return $result;
}

// a 'vector' is an array with integer keys
function is_vector( &$array ) {
  if ( ! is_array( $array ) ) { return false; }
  foreach ( $array as $key => &$val ) {
    if ( ! is_numeric( $key ) ) { return false; }
  }
  return true;  
}

// a 'map' is an array with non-integer keys
function is_map( &$array ) {
  if ( ! is_array( $array ) ) { return false; }
  foreach ( $array as $key => &$val ) {
    if ( ! is_string( $key ) ) { return false; }
  }
  return true;  
}

function &flatten( &$map ) {
  
  $result = array();
  
  foreach ( $map as $key => &$value ) {
    
    $result[] = $key;
    $result[] = &$value;
    
  }
  
  return $result;
  
}

function &unflatten( &$vector, $start = 0 ) {
  
  $result = array();

  $count = count( $vector );
  
  if ( $count % 2 === 1 ) { $count--; }
  
  for ( $i = $start; $i < $count; $i += 2 ) {
    
    $key = read_a( $vector, $i );
    $value = read_a( $vector, $i + 1 );
    
    $result[ $key ] = $value;
    
  }
    
  return $result;
  
}

// TODO: consider unrolling read_* methods for performance reasons...

// read an array
function &read_a( &$array ) {

  $num_args = func_num_args();

  $spec = array();
  
  for ( $i = 1; $i < $num_args; $i++ ) { $spec[] = func_get_arg( $i ); }
  
  return read_array( $array, $spec );
  
}

// read an array or return default
function &read_d( &$array ) {

  $num_args = func_num_args() - 1;

  $spec = array();
  
  for ( $i = 1; $i < $num_args; $i++ ) { $spec[] = func_get_arg( $i ); }
  
  $default = func_get_arg( $num_args );
  
  return read_array( $array, $spec, $default );
  
}

// read $_POST or $_GET or fail
function &read_r() {

  $num_args = func_num_args();

  $spec = array();
  
  for ( $i = 0; $i < $num_args; $i++ ) { $spec[] = func_get_arg( $i ); }
  
  $result = read_array( $_POST, $spec, $default, $success );
  
  if ( $success ) { return $result; }

  $result = read_array( $_GET, $spec, $default, $success );
  
  if ( $success ) { return $result; }

  throw Error(
    'Missing input variable "%variable%".',
    'variable', implode( ', ', $spec )
  );
  
}

// read $_POST or $_GET for a boolean (or return false)
function read_b() {
  
  $num_args = func_num_args();

  $spec = array();
  
  for ( $i = 0; $i < $num_args; $i++ ) { $spec[] = func_get_arg( $i ); }
  
  $result = read_array( $_POST, $spec, $default, $success );
  
  if ( $success ) { return dbool( $result ); }
  
  $result = read_array( $_GET, $spec, $default, $success );
  
  if ( $success ) { return dbool( $result ); }

  return 0;
  
}

// read $_POST or $_GET or return default
function &read_i() {

  $num_args = func_num_args() - 1;

  $spec = array();
  
  for ( $i = 0; $i < $num_args; $i++ ) { $spec[] = func_get_arg( $i ); }
  
  $default = func_get_arg( $num_args );
  
  $result = read_array( $_POST, $spec, $default, $success );
  
  if ( $success ) { return $result; }

  return read_array( $_GET, $spec, $default, $success );
  
}

// read $_POST or return default
function &read_p() {

  $num_args = func_num_args() - 1;

  $spec = array();
  
  for ( $i = 0; $i < $num_args; $i++ ) { $spec[] = func_get_arg( $i ); }
  
  $default = func_get_arg( $num_args );
  
  return read_array( $_POST, $spec, $default );
  
}

// read $_GET or return default
function &read_g() {

  $num_args = func_num_args() - 1;

  $spec = array();
  
  for ( $i = 0; $i < $num_args; $i++ ) { $spec[] = func_get_arg( $i ); }
  
  $default = func_get_arg( $num_args );
  
  return read_array( $_GET, $spec, $default );
  
}

// read input or default
function read_ia( &$array, $key ) {
  
  return read_i( $key, read_a( $array, $key ) );
    
}

// read input or default into $data array
function read_iar( &$array, $key, &$data ) {

  $data[ $key ] = read_ia( $array, $key );
  
}

function input( $name, $type = null, $subtype = null ) {
  
  global $default, $input;

  if ( $type === null ) { $type = $name; }
  
  //$value = read_p( $name, null );
  $value = read_i( $name, null );
  
  if ( $value === null ) {
    
    $input[ $name ] = read_a( $default, $name );
    
  }
  else {

    $input[ $name ] = $value;
    
    validate( $type, $name, $input );

    if ( $subtype ) {

      validate_array( $subtype, $name, $input );

    }
  }
}

// read data from input array to output array
function read_in( $key, &$input, &$output ) {
  
  $output[ $key ] = read_a( $input, $key );
  
}

function &read_array( &$array, &$spec, &$default = null, &$success = null ) {
  
  $success = false;
  
  if ( ! is_array( $array ) ) { return $default; }

  $value = &$array;
  
  foreach ( $spec as $key ) {
    
    if ( ! array_key_exists( $key, $value ) ) { return $default; }
    
    $value = &$value[ $key ];
    
  }

  $success = true;
  
  return $value;
  
}

function &vector_1st( &$array, $default = array( null, null ) ) {

  if ( ! is_array( $array ) ) { return $default; }

  $index = vector_1st_index( $array );
  
  if ( $index >= 0 ) {
    
    $result = array( $index, $array[ $index ] );
    
    return $result;
    
  }
  
  return $default;
  
}

function vector_1st_index( &$array, $default = null ) {

  if ( ! is_array( $array ) ) { return $default; }

  $count = count( $array );
  
  if ( $count > 0 ) { return 0; }
  
  return $default;
  
}

function &vector_1st_value( &$array, $default = null ) {

  if ( ! is_array( $array ) ) { return $default; }
  
  $index = vector_1st_index( $array );
  
  if ( $index >= 0 ) { return $array[ $index ]; }
  
  return $default;
  
}

function &vector_nth( &$array, $default = array( null, null ) ) {

  $index = vector_nth_index( $array );
  
  if ( $index >= 0 ) {
    
    return array( $index, $array[ $index ] );
    
  }
  
  return $default;
  
}

function vector_nth_index( &$array, $default = null ) {

  if ( ! is_array( $array ) ) { return $default; }

  $result = count( $array );
  
  if ( $result === 0 ) { return $default; }
    
  return $result - 1;
  
}

function &vector_nth_value( &$array, $default = null ) {

  $index = vector_nth_index( $array );
  
  if ( $index >= 0 ) { return $array[ $index ]; }
  
  return $default;
  
}

function &map_1st( &$array, $default = array( null, null ) ) {

  if ( ! is_array( $array ) ) { return $default; }
  
  foreach ( $array as $key => &$value ) {

    $result = array( $key, &$value );
    
    return $result;
    
  }
  
  return $default;
  
}

function &map_1st_key( &$array, $default = null ) {

  if ( ! is_array( $array ) ) { return $default; }
  
  foreach ( $array as $key => &$value ) {
    
    return $key;
    
  }
  
  return $default;
  
}

function &map_1st_value( &$array, $default = null ) {

  if ( ! is_array( $array ) ) { return $default; }
  
  foreach ( $array as $key => &$value ) {
    
    return $value;
    
  }
  
  return $default;
  
}

function &map_nth( &$array, $default = array( null, null ) ) {

  if ( ! is_array( $array ) ) { return $default; }
  
  $last_key = null;
  $last_value = null;
  $read_value = false;
  
  foreach ( $array as $key => &$value ) {

    $last_key = $key;
    $last_value = &$value;
    $read_value = true;
    
  }
  
  if ( ! $read_value ) { return $default; }
  
  $result = array( $last_key, &$last_value );
  
  return $result;
  
}

function &map_nth_key( &$array, $default = null ) {

  if ( ! is_array( $array ) ) { return $default; }

  $last_key = null;
  $read_value = false;
  
  foreach ( $array as $key => &$value ) {

    $last_key = $key;
    $read_value = true;
    
  }
  
  if ( ! $read_value ) { return $default; }
  
  return $last_key;
  
}

function &map_nth_value( &$array, $default = null ) {

  if ( ! is_array( $array ) ) { return $default; }

  $key = map_nth_key( $array );
  
  if ( $key === null ) { return $default; }
  
  return $array[ $key ];
  
}
