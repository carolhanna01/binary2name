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

class GrcHtmlView5 {
  
  private $pretty;
  
  private function __construct( $pretty ) {
    
    $this->pretty = $pretty;
    
  }
  
  public static function Create( $pretty = true ) {
    
    return new GrcHtmlView5( $pretty );
    
  }
  
  public function render( $root ) {
    
    if ( $root->tag === 'html' ) {

      echo "<!DOCTYPE html>\n";
      
    }

    $this->render_element( $root, 0 );
    
  }
  
  public function to_html( $root ) {
    
    ob_start();
    
    $this->render( $root );
    
    return ob_get_clean();
    
  }
  
  private function render_element( $element, $indent ) {
    
    if ( $element->tag === 'text' ) {
      
      echo henc( $element->get_attribute( 'content' ) );
      
      return;
      
    }
    
    if ( $element->tag === 'markup' ) {
      
      echo $element->get_attribute( 'content' );
      
      return;
      
    }
    
    if ( $element->tag === 'literal' ) {
      
      echo $element->attributes[ 'content' ];
      
      return;
      
    }
    
    if ( $element->tag === 'comment' ) {
      
      echo "\n<!--";
      
    }
    else if ( $element->tag === 'test' ) {
      
      if ( ! $element->get_attribute( 'test' ) ) {
        
        return;
        
      }
    }
    else {
    
      $this->open_tag( $element, $indent );
  
    }
    
    foreach ( $element->children as $child ) {
      
      $this->render_element( $child, $indent + 2 );
      
    }
    
    if ( $element->tag === 'comment' ) {

      echo "\n-->";
      
    }
    else if ( $element->tag === 'test' ) {
      
      // skip
      
    }
    else {
      
      $this->done_tag( $element, $indent );
      
    }
  }
  
  private function indent( $indent ) {
    
    return $this->pretty ? str_pad( '', $indent ) : '';
    
  }
  
  private function open_tag( $element, $indent ) {

    $tag = $element->tag;
    
    global $html5_spec;
    
    if ( read_d( $html5_spec, $tag, 'spec', 'space-before', true ) ) {
      
      echo "\n" . $this->indent( $indent );
      
    }
    
    echo '<' . $tag;
 
    $this->render_attributes( $element->attributes );

    echo '>';

    if ( $tag === 'script' && read_a( $element->attributes, 'src' ) ) {

      // no new line in this case...
      
    }
    else if ( read_d( $html5_spec, $tag, 'spec', 'space-after', false ) ) {
      
      echo "\n";
      
    }
    
  }
  
  private function done_tag( $element, $indent ) {

    $tag = $element->tag;
    
    global $html5_spec;

    if ( read_a( $html5_spec, $tag, 'empty' ) ) { return; }
    
    if ( $tag === 'script' && $element->get_attribute( 'src' ) ) {
      
      echo "</script>";
      
      return;
      
    }
    
    if ( count( $element->children ) === 0 ) {
      
      echo "</$tag>";
      
      return;
      
    }
        
    if ( read_d( $html5_spec, $tag, 'spec', 'space-after', true ) ) {
      
      echo "\n" . $this->indent( $indent );
      
    }

    echo "</$tag>";
    
  }
  
  private function render_attributes( &$attributes ) {

    $content = '';
    
    $result = '';
    
    foreach ( $attributes as $name => $value ) {
      
      if ( is_bool( $value ) ) {
       
        if ( $name === 'value' ) {

          $value = $value ? '1' : '0';
          
          $result .= ' ' . $name . '="' . $value . '"';
          
        }
        else {
          
          if ( $value ) { $result .= " $name"; }
          
        }
      }
      else {
      
        $result .= ' ' . $name . '="' . $value . '"';
  
      }
    }
    
    echo $result;
    
  }  
}
