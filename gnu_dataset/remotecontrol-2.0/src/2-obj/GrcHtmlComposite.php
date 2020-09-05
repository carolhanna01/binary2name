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

class GrcHtmlComposite extends GrcHtmlElement {

  //public $elements;
  
  public function __construct() {

    parent::__construct( null, null );
    //$this->elements = array();
    
  }
  
  public static function Create( $tag = null, $parent = null ) {
    
    return new GrcHtmlComposite();
    
  }
  
  public function add( $element ) {
    
    if ( $element === null ) { return $this; }
    
    if ( is_a( $element, 'GrcHtmlComposite' ) ) {
      
      foreach ( $element->children as $el ) {
        
        $this->add( $el );
        
      }
      
      return $this;
      
    }

    /*
    if ( $element->parent ) {
      
      $element->parent->remove( $element );
      
    }
    
    $element->parent = $this;
    */
    
    $this->children[] = $element;
    
    return $this;
    
  }
}
