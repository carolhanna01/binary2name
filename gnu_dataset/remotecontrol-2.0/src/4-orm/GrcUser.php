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

class GrcUser {
  
  public $id;
  public $username;
  public $is_admin;
  public $is_translator;
  public $location_id;
  public $max_thermostats;
  public $langtag;
  public $xsrf;
  public $next;
  public $temperature_scale;
  public $deleted;
  
  public $timezone;
  
  public function __construct( &$record = null ) {

    // TODO: put temp scale in database.
    // 1 = Fahrenheit, 2 = Celsius
    $this->temperature_scale = DEFAULT_SCALE;

    $this->timezone = null;
    
    if ( $record ) {
    
      $this->id = read_a( $record, 'id' );
      $this->username = read_a( $record, 'username' );
      $this->is_admin = read_a( $record, 'is_admin' );
      $this->is_translator = read_a( $record, 'is_translator' );
      $this->location_id = read_a( $record, 'location_id' );
      $this->max_thermostats = read_a( $record, 'max_thermostats' );
      $this->langtag = read_a( $record, 'langtag' );
      $this->xsrf = read_a( $record, 'xsrf' );
      $this->next = read_a( $record, 'next' );
      $this->deleted = boolval( read_a( $record, 'deleted' ) );

    }
    else {
      
      $this->id = 0;
      $this->username = '';
      $this->is_admin = false;
      $this->is_translator = false;
      $this->location_id = 0;
      $this->max_thermostats = 0;
      $this->langtag = 'en';
      $this->xsrf = null;
      $this->next = 'view';
      $this->deleted = true;
      
    }
  }
  
  public static function Load() {

    $username = read_a( $_SERVER, 'PHP_AUTH_USER' );

    if ( $username === null ) { return new GrcUser(); }
    
    $record = dal()->user->get_by_username( $username );
    
    if ( $record === false ) {
      
      $count = dal()->user->count();
      
      if ( $count == 0 ) {
      
        $user_id = dal()->user->add(
          $username,
          true, // first user is admin
          0,    // HACK!!! (TODO: load default location)
          1234,
          'en'
        );
        
      }
      else {
        
        $user_id = dal()->user->add(
          $username,
          false, // not admin if there are other users
          0,  // HACK!!! (TODO: load default location)
          0,  // no thermostats by default
          'en'
        );
        
      }
      
      $record = dal()->user->get_by_username( $username );

    }
    
    if ( ! $record ) {
      
      throw Error(
        'Username "%username%" is invalid.',
        'username', $username
      );
      
    }
    
    return new GrcUser( $record );
    
  }
  
  public function is_null() { return $this->id === 0; }
  
  public function get_lang() { return $this->langtag; }

  public function update_xsrf() {

    $this->xsrf = new_xsrf_token();
    
    $this->save();
    
  }
  
  public function logout() {
    
    session()->reset();
    
  }
  
  public function get_timezone() {
    
    if ( $this->timezone !== null ) { return $this->timezone; }
    
    try {

      $location = dal()->location->get_by_id( $this->location_id );
      
    }
    catch ( Exception $ex ) {

      $location = array();
      
    }
    
    $timezone = new DateTimeZone(
      read_d( $location, 'timezone', DEFAULT_TIMEZONE )
    );

    $this->timezone = $timezone;

    return $timezone;
    
  }
}
