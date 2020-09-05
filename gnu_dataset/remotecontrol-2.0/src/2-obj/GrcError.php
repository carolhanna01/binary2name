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

class GrcError {

  public $spec;
  public $map;

  public function __construct() {
    
    $this->spec = array(
      'next_code' => 1,
      'name_map' => array(),
      'code_map' => array()
    );
    
    $this->map = array();
    
  }
  
  public static function Create() {
    
    return new GrcError();
    
  }
  
  public function get_valid_error_code( $code ) {
    if ( ! is_int( $code ) ) {
      throw Error(
        'Error code "%code%" is not an integer.',
        'code', $code
      );
    }
    if ( isset( $this->map[ $code ] ) ) { return $code; }
    throw Error(
      'Error code "%code%" is invalid.',
      'code', $code
    );  
  }

  /*
   * code, paramters..., context?, $previous?
   */
  public function get_error( &$args ) {
    $argc = count( $args );
    if ( $argc < 1 ) {
      throw Error(
        'Argument count "%argc%" is invalid.',
        'argc', $argc
      );
    }
    $arg = $args[ 0 ];
    if ( is_int( $arg ) ) {
      $error_code = $this->get_valid_error_code( $arg );
    }
    else {
      $error_code = 0;
    }
    $previous = null;
    if ( is_a( $args[ $argc - 1 ], 'Exception' ) ) {
      $previous = $args[ $argc - 1 ];
      array_pop( $args );
    }
    $argc = count( $args );
    if ( ( $argc % 2 ) === 0 ) {
      $argc--;
    }
    else {
      $args[] = CONTEXT_ERROR;
    }
    for ( $i = 2; $i < $argc; $i += 2 ) {
      //$args[ $i ] = truncate_text( $args[ $i ] );
    }
    $message = intl()->format( 'text', $args );
    $result = new Exception(
      $message,
      $error_code,
      $previous
    );
    //dal()->log_exception( $result, EXCEPTION_CREATED );
    return $result;
  }

  private function get_error_bits( $index, &$mask, &$size ) {
    static $bits = array(
      1, // error (1)
      5, // type (31, 6)
      8, // module/class (255, 14)
      8, // function (255, 23)
      5, // variable (31, 28)
      4, // state (15, 31)
    );
    if ( ! is_int( $index ) ) {
      throw Error(
        'Index "%index%" is not an integer.',
        'index', $index
      );
    }
    if ( ! array_key_exists( $index, $bits ) ) {
      throw Error(
        'Bits don\'t define index "%index%".',
        'index', $index
      );
    }
    $size = $bits[ $index ];
    $mask = pow( 2, $size ) - 1;
    return $size;
  }
}
