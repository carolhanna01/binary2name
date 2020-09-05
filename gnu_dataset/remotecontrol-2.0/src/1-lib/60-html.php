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

function henc( $text, $encoding = UTF8_ENCODING ) {
  
  if ( $text === null ) { return null; }
  
  return htmlentities( strval( $text ), ENT_NOQUOTES, $encoding, true );
  
}

function aenc( $text, $encoding = UTF8_ENCODING ) {
  
  if ( $text === null ) { return null; }
  
  return htmlentities( strval( $text ), ENT_QUOTES, $encoding, true );
  
}

function html_encode( $text, $encoding = UTF8_ENCODING ) {

  if ( $text === null ) { return null; }
  
  return htmlentities( strval( $text ), ENT_QUOTES, $encoding, true );

}

function html_decode( $html ) {

  if ( $html === null ) { return null; }
  
  return html_entity_decode( strval( $html ), ENT_QUOTES, 'UTF-8' );

}

function nbsp( $text ) {
  
  if ( $text === null ) { return null; }

  if ( ! is_string( $text ) ) { $text = strval( $text ); }
  
  $parts = explode( '<', $text );
  
  $count = count( $parts );
  
  if ( $count === 1 ) {
    
    return preg_replace( '/\s/', '&nbsp;', $text );
    
  }
  
  $result = array( $parts[ 0 ] );

  for ( $i = 1; $i < $count; $i++ ) {

    $result[] = '<';
    
    $inner_parts = explode( '>', $parts[ $i ] );
    
    $result[] = $inner_parts[ 0 ];
    
    $result[] = '>';
    
    $result[] = preg_replace( '/\s/', '&nbsp;', $inner_parts[ 1 ] );
    
  }
  
  return implode( '', $result );
  
}
