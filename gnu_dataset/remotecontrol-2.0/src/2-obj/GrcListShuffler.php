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

class GrcListShuffler {
  
  public $list;
  
  public function __construct( &$list ) {
    
    $this->list = &$list;
    
  }
  
  public static function Create( &$list ) {
    
    return new GrcListShuffler( $list );
    
  }
  
  public function first( $item, &$values ) {
    
    return $this->pos( 1, $item, $values, $position );
    
  }
  
  public function left( $item, &$values ) {

    $pos = array_search( $item, $this->list );
    
    if ( $pos < 1 ) { $values = $this->list; return false; }
    
    return $this->pos( $pos, $item, $values, $position );
    
  }
  
  public function right( $item, &$values ) {

    $pos = array_search( $item, $this->list ) + 2;

    if ( $pos > count( $this->list ) ) { $values = $this->list; return false; }
    
    return $this->pos( $pos, $item, $values, $position );
    
  }
  
  public function last( $item, &$values ) {

    return $this->pos( count( $this->list ), $item, $values, $position );
    
  }
  
  public function remove( $item, &$values ) {
    
    $found = false;
    
    $values = array();
    
    foreach ( $this->list as $current_item ) {
      
      if ( $item === $current_item ) {
        
        $found = true;
        
      }
      else {
        
        $values[] = $current_item;
        
      }
    }
    
    return $found;
    
  }
  
  public function pos( $pos, $item, &$values, &$position ) {
    
    $position = format_position( $pos );

    $pos--;
    
    $location = array_search( $item, $this->list );
    
    if ( $location === $pos ) {
      
      $values = $this->list;
      
      return false;
      
    }

    $values = array();

    for ( $i = 0, $il = count( $this->list ); $i < $il; $i++ ) {

      if ( $i < $location ) {
        
        if ( $i === $pos ) {

          $values[] = $item;

        }

        if ( $this->list[ $i ] !== $item ) {

          $values[] = $this->list[ $i ];

        }
      }
      else  {
        
        if ( $this->list[ $i ] !== $item ) {

          $values[] = $this->list[ $i ];

        }

        if ( $i === $pos ) {

          $values[] = $item;

        }
      }
    }

    return true;

  }
}
