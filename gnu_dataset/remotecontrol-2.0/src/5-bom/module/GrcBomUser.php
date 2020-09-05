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

class GrcBomUser {
  
  public static function Create( $bom ) {
    
    return new GrcBomUser();
    
  }
  
  public function count() {
    
    return dal()->user->count();
    
  }
  
  public function add_admin(
    $username,
    $location,
    $max_thermostats,
    $langtag
  ) {
    
    return $this->add(
      $username,
      true, // is_admin
      true, // is_translator
      $location,
      $max_thermostats,
      $langtag
    );
    
  }
  
  public function add(
    $username,
    $is_admin,
    $is_translator,
    $location,
    $max_thermostats,
    $langtag
  ) {

    return dal()->user->add(
      $username,
      $is_admin,
      $is_translator,
      $location,
      $max_thermostats,
      $langtag
    );
    
  }
  
  public function http_add() {

    verify( is_admin() );

    $username = read_r( 'username' );
    $is_admin = read_b( 'is_admin' );
    $is_translator = read_b( 'is_translator' );
    $location = read_r( 'location' );
    $max_thermostats = read_r( 'max_thermostats' );
    $langtag = read_r( 'langtag' );
    
    return $this->add(
      $username,
      $is_admin,
      $is_translator,
      $location,
      $max_thermostats,
      $langtag
    );
    
  }
  
  public function &report() {
    
    return dal()->user->report();
    
  }  
  
  public function http_edit() {

    verify( is_admin() );

    $id = read_r( 'id' );
    $username = read_r( 'username' );
    $is_admin = boolval( read_r( 'is_admin' ) );
    $is_translator = boolval( read_r( 'is_translator' ) );
    $location_id = read_r( 'location' );
    $max_thermostats = read_r( 'max_thermostats' );
    $langtag = read_r( 'langtag' );
    
    return dal()->user->update(
      $id,
      $username,
      $is_admin,
      $is_translator,
      $location_id,
      $max_thermostats,
      $langtag
    );
    
  }

  public function http_delete() {
    
    verify( is_admin() );

    $id = read_r( 'id' );

    dal()->user->soft_delete( $id );
    
  }
  
  
  public function http_update( &$data ) {
    
    verify( true );
    
    if ( user()->id != $data[ 'id' ] ) {
      
      throw Error( 'Invalid user id.' );
      
    }

    $result = dal()->user->update_account(
      $data[ 'id' ],
      $data[ 'location_id' ],
      $data[ 'langtag' ]
    );
    
    return $result;
    
  }
}
