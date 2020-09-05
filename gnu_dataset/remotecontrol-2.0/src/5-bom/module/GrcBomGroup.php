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

class GrcBomGroup {
  
  public static function Create( $bom ) {
    
    return new GrcBomGroup();
    
  }

  public function http_add() {
    
    verify( is_admin() );
    
    $name = read_r( 'name' );
    $description = read_r( 'description' );
    
    return dal()->group->add( $name, $description );
    
  }
  
  public function http_edit() {

    verify( is_admin() );

    $id = read_r( 'id' );
    $name = read_r( 'name' );
    $description = read_r( 'description' );
    
    return dal()->group->update( $id, $name, $description );
    
  }
  
  public function http_delete() {
    
    verify( is_admin() );

    $id = read_r( 'id' );

    dal()->begin();
    
    try {
      
      dal()->group->delete( $id );
      
      dal()->commit();
  
    }
    catch ( Exception $ex ) {
      
      dal()->rollback();
      
      throw $ex;
      
    }
  }
}
