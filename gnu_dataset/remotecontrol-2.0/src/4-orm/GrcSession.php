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

class GrcSession {

  public static function Load() {
    
    if ( !headers_sent() ) {

      session_start();
      
    }
    
    return new GrcSession();
    
  }
  
  public function get_xsrf_token() {
    
    $token = read_a( $_SESSION, 'xsrf' );
    
    if ( $token === null ) {
      
      $token = new_xsrf_token();
      
      $this->set_xsrf_token( $token );
      
    }
    
    return $token;
    
  }
  
  public function set_xsrf_token( $token ) {
    
    $_SESSION[ 'xsrf' ] = $token;
    
  }

  public function get_user_id() {
    
    return read_a( $_SESSION, 'user_id' );
    
  }
  
  public function set_user_id( $user_id ) {
    
    $_SESSION[ 'user_id' ] = $user_id;
    
  }
  
  public function reset() {

    unset( $_SESSION[ 'xsrf' ] );
    unset( $_SESSION[ 'user_id' ] );
    
  }
}
