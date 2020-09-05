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

class GrcDataSort {

  public $field_list;
  public $direction_list;
  public $schema;

  public function __construct( $spec ) {

    $this->field_list = array();
    $this->direction_list = array();

    $spec = array_map( 'trim', explode( ' ', $spec ) );

    foreach ( $spec as $column_spec ) {
      if ( strlen( $column_spec ) === 0 ) { continue; }
      if ( substr( $column_spec, 0, 1 ) === '-' ) {
        $field = substr( $column_spec, 1 );
        $this->field_list[] = $field;
        $this->direction_list[ $field ] = 'desc';
      }
      else {
        $this->field_list[] = $column_spec;
        $this->direction_list[ $column_spec ] = 'asc';        
      }
    }
    
    $this->schema = null;
    
  }

  public static function Parse( $spec ) {
    
    $class = get_called_class();
    
    return new $class( $spec );
    
  }
  
  public static function Read( $default = null, $name = null ) {

    $name = $name ? $name . '_sort' : 'sort';
    
    return self::Parse( empty( $_GET[ $name ] ) ? $default : $_GET[ $name ] );

  }
  
  public static function QuoteField( $field ) {
    
    return "`" . preg_replace( "/[^a-z0-9_]*/", "", $field ) . "`";
    
  }

  public function push( $column, $direction = 'asc', $single = true ) {

    $field_list = $this->field_list;
    $direction_list = $this->direction_list;
    
    if (
      in_array( $column, $field_list )
    ) {

      if ( $single && count( $field_list ) === 1 ) {
      
        return $this->to_string();
  
      }
      
      if ( $direction_list[ $column ] === $direction ) {
        
        return $this->to_string();
        
      }
      
    }
    
    if ( ! in_array( $column, $field_list ) ) {
    
      $field_list[] = $column;
      
    }
    
    $direction_list[ $column ] = $direction;

    $result = "";
    $count = count( $field_list );
    
    for ( $i = 0; $i < $count; $i++ ) {
    
      if ( $i !== 0 ) { $result .= ' '; }

      $field = $field_list[ $i ];
      
      $result .= $direction_list[ $field ] === 'desc' ? '-' : '';
      
      $result .= $field;
    
    }
    
    return $result;
    
  }
  
  public function activate( $link, $term, $column, $single = false ) {
    
    $parts = explode( ' ', url()->get( $term ) );
    
    if ( in_array( $column, $parts ) ) {
      
      if ( $single ) {
        
        if ( count( $parts ) > 1 ) {
          
          $link->add_class( 'inactive' );

          return;
          
        }
      }
      else {

        if ( count( $parts ) < 2 ) {

          $link->add_class( 'inactive' );
          
          return;
          
        }
      }
      
      $link->add_class( 'active' );
  
    }
  }
  
  public function set_schema( $schema ) {
    
    $this->schema = $schema;
    
  }
  
  public function count() {

    return count( $this->field_list );

  }

  public function to_string() {
    
    $result = "";
    $count = $this->count();
    
    for ( $i = 0; $i < $count; $i++ ) {
    
      if ( $i !== 0 ) { $result .= ' '; }

      $field = $this->field_list[ $i ];
      
      $result .= $this->direction_list[ $field ] === 'desc' ? '-' : '';
      
      $result .= $field;
    
    }
    
    return $result;
    
  }
  
  public function to_sql() {

    $field_list = $this->field_list;
    
    $count = count( $field_list );

    if ( $count === 0 ) { return ""; }

    $result = "\nORDER BY ";

    for ( $i = 0; $i < $count; $i++ ) {

      if ( $i !== 0 ) { $result .= ", "; }

      $field = $field_list[ $i ];
      
      if ( $this->schema ) {
        
        $sql = read_a( $this->schema, $field, 'sql' );
        
        if ( $sql ) {
          
          $result .= $sql . ' ' . $this->direction_list[ $field ];
          
          continue;
          
        }
      }
            
      $result .= self::QuoteField( $field ) . " " . $this->direction_list[ $field ];

    }

    return $result;

  }
}
