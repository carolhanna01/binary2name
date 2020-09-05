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

class GrcHtmlViewX {
  
  private $pretty;
  
  private function __construct( $pretty ) {
    
    $this->pretty = $pretty;
    
  }
  
  public static function Create( $pretty = true ) {
    
    return new GrcHtmlViewX( $pretty );
    
  }
  
  public function render( $root, $output_content_type = true ) {

    if ( $output_content_type ) {

      header( 'Content-Type: application/xhtml+xml' );
      
    }
    
    if ( $root->tag === 'html' ) {

      echo '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" ' .
        '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">' . "\n";

      //echo '<?xml version="1.0" encoding="UTF-8"?' . '>' . "\n";
      
      $lang = read_a( $root->attributes, 'lang' );
      
      if ( $lang === null ) {
        
        throw Error(
          'Root HTML element must define "lang".'
        );
        
      }

      $root->xmlns = 'http://www.w3.org/1999/xhtml';
      $root->attributes[ 'xml:lang' ] = $lang;
      
    }
    
    
    $this->render_element( $root, 0 );
    
  }
  
  public function to_html( $root ) {
    
    ob_start();
    
    $this->render( $root, $output_content_type = false );
    
    return ob_get_clean();
    
  }
  
  private function render_element( $element, $indent ) {
    
    if ( $element->tag === 'text' ) {
      
      echo henc( $element->attributes[ 'content' ] );
      
      return;
      
    }
    
    if ( $element->tag === 'markup' ) {
      
      echo $element->attributes[ 'content' ];
      
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
      
      if ( ! read_a( $element->attributes, 'test' ) ) {
        
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
    $html5_class = null;
    
    global $html5_spec;
    
    if ( read_a( $html5_spec, $tag, 'version' ) === HTML5 ) {
      
      $tag = 'div';
      $html5_class = $tag;
      
    }
    
    if ( read_d( $html5_spec, $tag, 'spec', 'space-before', true ) ) {
      
      echo "\n" . $this->indent( $indent );
      
    }
    
    echo '<' . $tag;
        
    $this->render_attributes( $element->attributes, $html5_class );

    if ( read_a( $html5_spec, $tag, 'empty' ) ) { echo ' /'; }
    
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
    
    if ( read_a( $html5_spec, $tag, 'version' ) === HTML5 ) {
      
      $tag = 'div';
      
    }
    
    if ( $tag === 'script' && read_a( $element->attributes, 'src' ) ) {
      
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
  
  private function render_attributes( &$attributes, $html5_class = null ) {

    $content = '';
    
    $result = '';
    
    $done_class = false;
    
    foreach ( $attributes as $name => $value ) {

      if ( $name === 'class' && $html5_class ) {
        
        if ( strlen( $value ) ) {
          
          $value .= " $html5_class";
          
        }
        else {
          
          $value = $html5_class;
          
        }
        
        $done_class = true;
        
      }
            
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
    
    if ( ! $done_class && $html5_class ) {
      
      $result .= ' class="' . $html5_class . '"';
      
    }
    
    echo $result;
    
  }  
}
