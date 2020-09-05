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

class GrcMessage {
  
  public $const_map;
  public $id_map;
  public $count;
  
  // we don't translate registered messages during bootstrap
  // after msg()->translate is called we translate registered messages
  public $is_translating;
  
  public function __construct() {
    
    $this->const_map = array();
    $this->id_map = array();
    $this->count = 0;
    $this->is_translating = false;
    
  }
  
  public static function Create() {
    
    return new GrcMessage();
    
  }
  
  function register(
    $const,
    $english,
    $type = 'text',
    $context = 'sysmsg',
    $id = null
  ) {
    static $valid_types = array( 'html', 'attr', 'text', 'safe' );
    if ( ! in_array( $type, $valid_types ) ) {
      throw Error(
        'Invalid message type "%type%".',
        'type', $type
      );
    }
    static $valid_contexts = array( 'error', 'sysmsg' );
    if ( ! in_array( $context, $valid_contexts ) ) {
      throw Error(
        'Invalid message context "%context%".',
        'context', $context
      );
    }
    if ( isset( $this->const_map[ $const ] ) ) {
      throw Error(
        'Message "%const%" already registered.',
        'const', $const
      );
    }
    if ( $id === null ) {
      $id = ++$this->count;
    }
    $spec = array(
      'type' => $type,
      'context' => $context,
      'message' => $english
    );
    if ( $this->is_translating ) {
      $this->translate_spec( $spec );
    }
    $this->id_map[ $id ] = &$spec;
    $this->const_map[ $const ] = $id;
    if ( defined( $const ) ) {
      if ( constant( $const ) !== $id ) {
        throw Error(
          'Constant "%const%" already defined.',
          'const', $const
        );
      }
    }
    else {
      define( $const, $id );
    }
  }

  function translate() {
    foreach ( $this->id_map as $id => &$spec ) {
      if ( ! array_key_exists( 'translation', $spec ) ) {
        $this->translate_spec( $spec );
      }
    }
    $this->is_translating = true;
  }
  
  function translate_spec( &$spec ) {
    $spec[ 'translation' ] = intl()->translate(
      $spec[ 'type' ],
      $spec[ 'message' ],
      $spec[ 'context' ]
    );    
  }

  function get( $id ) {
    if ( ! isset( $this->id_map[ $id ] ) ) {
      throw Error(
        'Missing message ID "%id%".',
        'id', $id
      );
    }
    $spec = &$this->id_map[ $id ];
    if ( array_key_exists( 'translation', $spec ) ) {
      return $spec[ 'translation' ];
    }
    return $spec[ 'message' ];
  }
}
