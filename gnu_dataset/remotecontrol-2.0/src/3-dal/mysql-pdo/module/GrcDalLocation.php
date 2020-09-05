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

class GrcDalLocation extends GrcDalModule {
  
  public static function Create( $dal ) {
    
    return new self( $dal );
    
  }
  
  public function count_all() {
    
    return $this->dal->table_count( 'v2_location' );
    
  }
  
  public function &report(
    &$count = -1,
    $page = null,
    $sort = null
  ) {

    $sql = "
select
  *
from
  v2_location
";
    
    $start = 0;
    
    if ( $page ) {

      $count = $this->sql_count( $sql );

      $page->set_count( $count );
      
      $start = $page->start;

    }

    if ( $sort ) {

      $sql .= $sort->to_sql();

    }

    if ( $page ) {

      $sql .= $page->to_sql();

    }
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_all( $stmt, $table, $start );
    
    return $table;
    
  }

  public function &report_total() {

    $sql = "
select
  count( distinct id ) as id,
  concat( min( length( `name` ) ), ' to ', max( length( `name` ) ) ) as `name`,
  count( distinct `timezone` ) as `timezone`
from
  v2_location
";

    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_row( $stmt, $row );
    
    return $row;
    
  }  
  
  public function &get_by_id( $id ) {
    
    $sql = 'select * from v2_location where id = :id';
    
    $stmt = $this->prepare( $sql );
    
    $args = array( 'id' => $id );
    
    $this->execute( $stmt, $sql, $args );

    $this->fetch_row( $stmt, $row );
    
    return $row;
    
  }
  
  public function name_exists( $name ) {
    
    $sql = 'select count(*) as `count` from v2_location where name = :name';
    
    $stmt = $this->prepare( $sql );
    
    $args = array( 'name' => $name );
    
    $this->execute( $stmt, $sql, $args );

    $count = $this->fetch_int( $stmt, 'count' );

    return $count !== 0;
    
  }
  
  public function id_in_use( $id ) {
    
    $sql = 'select count(*) as count from v2_thermostat where location_id = :id';
    
    $stmt = $this->prepare( $sql );
    
    $args = array( 'id' => $id );
    
    $this->execute( $stmt, $sql, $args );

    $count = $this->fetch_int( $stmt, 'count' );
    
    if ( $count !== 0 ) { return true; }

    $sql = 'select count(*) as count from v2_user where location_id = :id';
    
    $stmt = $this->prepare( $sql );
    
    $args = array( 'id' => $id );
    
    $this->execute( $stmt, $sql, $args );

    $count = $this->fetch_int( $stmt, 'count' );
    
    return $count !== 0;
    
  }
  
  public function add( $name, $timezone ) {
    
    validator()->location_name( $name );
    validator()->timezone( $timezone );

    if ( $this->name_exists( $name ) ) {
      
      throw Error(
        'Location "%name%" already exists.',
        'name', $name
      );
      
    }
        
    $sql = "
insert into v2_location (
  name,
  timezone
)
values (
  :name,
  :timezone
)";

    $stmt = $this->prepare( $sql );

    $args = array(
      'name' => $name,
      'timezone' => $timezone
    );

    $this->execute( $stmt, $sql, $args );

    return $this->fetch_id( $stmt );
    
  }
  
  public function update( $id, $name, $timezone ) {

    validator()->location_name( $name );
    validator()->timezone( $timezone );
    
    $sql = "
update v2_location
set
  name = :name,
  timezone = :timezone
where
  id = :id
";

    $stmt = $this->prepare( $sql );

    $args = array(
      'id' => $id,
      'name' => $name,
      'timezone' => $timezone
    );

    $this->execute( $stmt, $sql, $args );

    $this->finish( $stmt );
    
  }
  
  public function delete( $id ) {
    
    if ( $this->id_in_use( $id ) ) {
            
      $record = $this->get_by_id( $id );
      
      $label = read_a( $record, 'name' );
      
      if ( $label === null ) {
        
        $label = $id;
        
      }
            
      throw Error(
        'Location "%label%" is in use.',
        'label', $label
      );
      
    }
    
    $sql = "
delete from
  v2_location
where
  id = :id
";

    $stmt = $this->prepare( $sql );

    $args = array(
      'id' => $id
    );

    $this->execute( $stmt, $sql, $args );

    $this->finish( $stmt );
    
  } 
  
  public function &get_map() {
    
    $sql = "
select
  id,
  name
from
  v2_location
order by
  name
";
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_map( $stmt, $result, 'id', 'name' );

    return $result;
    
  }
}
