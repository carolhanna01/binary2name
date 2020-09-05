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

class GrcAjax {
  
  public static function Create() { return new GrcAjax(); }
 
  public function process() {

    if ( user()->deleted ) {
      
      throw Error(
        'User "%user%" has been deleted.',
        'user', user()->username
      );
      
    }
    
    global $action, $response;

    $unsafe_action = read_r( 'action' );
    
    $action = preg_replace( '/[^a-z0-9_\\-]*/', '', $unsafe_action );

    if ( $action !== $unsafe_action ) {
      
      throw Error(
        'Unsafe AJAX action "%unsafe_action%".',
        'unsafe_action', $unsafe_action
      );
      
    }
    
    $response = array( 'action' => $action );

    $module = __DIR__ . '/module/' . $action . '.php';
        
    if ( file_exists( $module ) ) {
      
      $xsrf_token = read_r( 'xsrf' );

      $session_xsrf_token = session()->get_xsrf_token();
      
      if ( $xsrf_token !== $session_xsrf_token ) {
        
        throw Error( 'Invalid XSRF token.' );
        
      }
      
      require $module;
      
      ajax_reply( $response );
      
    }
    else {
      
      throw Error(
        'Missing AJAX module "%module%".',
        'module', $module
      );
      
    }
  }
}
