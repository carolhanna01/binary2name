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

class GrcService {
  
  private $prefix;
  private $directory;
  
  private $modules;
  
  protected function __construct( $prefix, $directory ) {
    
    $this->prefix = $prefix;
    $this->directory = $directory;
  
    $this->modules = array();
    
  }
  
  public function __get( $module ) {

    return $this->get_module( $module );
  
  }  
  
  public function get_module( $module ) {    
    
    if ( ! array_key_exists( $module, $this->modules ) ) {

      $parts = preg_split( '/[^a-z0-9]+/', $module );
      
      for ( $i = 0; $i < count( $parts ); $i++ ) {
        
        $parts[ $i ] = ucfirst( $parts[ $i ] );
        
      }

      $class = $this->prefix . implode( '', $parts );
      
      $file = $this->directory . '/module/' . $class . '.php';

      if ( ! file_exists( $file ) ) {
        
        throw Error(
          'File "%file%" for class "%class%" is missing,',
          'file', $file,
          'class', $class
        );
        
      }
      
      require_once $file;
      
      $this->modules[ $module ] = call_user_func(
        array( $class, 'Create' ),
        $this
      );      
      
    }

    return $this->modules[ $module ];
    
  }
    
}