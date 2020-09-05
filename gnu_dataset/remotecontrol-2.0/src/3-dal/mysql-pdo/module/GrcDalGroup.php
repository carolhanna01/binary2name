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

class GrcDalGroup extends GrcDalModule {
  
  public static function Create( $dal ) {
    
    return new self( $dal );
    
  }

  public function count_all() {
    
    return $this->dal->table_count( 'v2_group' );
    
  }
  
  //public function get_group_map() {
  public function &get_map() {
    
    $sql = 'select id, name from v2_group order by id asc';
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_map( $stmt, $map, 'id', 'name' );
    
    return $map;
    
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
  v2_group
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
  -- count( distinct name ) as name,
  concat( min( length( `name` ) ), ' to ', max( length( `name` ) ) ) as `name`,
  concat( min( length( `description` ) ), ' to ', max( length( `description` ) ) ) as `description`
from
  v2_group
";

    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_row( $stmt, $row );
    
    return $row;
    
  }  
  
  /*
  //public function &get_groups() {
  public function &report() {
    
    $sql = "
select
  id, name, description
from
  v2_group
";

    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_all( $stmt, $result );
    
    return $result;
    
  }
  */

  public function &get_by_id( $id ) {
    
    $sql = 'select * from v2_group where id = :id';
    
    $stmt = $this->prepare( $sql );
    
    $args = array( 'id' => $id );
    
    $this->execute( $stmt, $sql, $args );

    $this->fetch_row( $stmt, $row );
    
    return $row;
    
  }
  
  public function name_exists( $name ) {
    
    $sql = 'select count(*) as `count` from v2_group where name = :name';
    
    $stmt = $this->prepare( $sql );
    
    $args = array( 'name' => $name );
    
    $this->execute( $stmt, $sql, $args );

    $count = $this->fetch_int( $stmt, 'count' );

    return $count !== 0;
    
  }
  
  public function id_in_use( $id ) {
    
    $sql = 'select count(*) as count from v2_thermostat where group_id = :id';
    
    $stmt = $this->prepare( $sql );
    
    $args = array( 'id' => $id );
    
    $this->execute( $stmt, $sql, $args );

    $count = $this->fetch_int( $stmt, 'count' );
    
    return $count !== 0;
    
  }
  
  public function add( $name, $description ) {
    
    validator()->group_name( $name );
    validator()->group_description( $description );

    if ( $this->name_exists( $name ) ) {
      
      throw Error(
        'Group "%name%" already exists.',
        'name', $name
      );
      
    }
        
    $sql = "
insert into v2_group (
  name,
  description
)
values (
  :name,
  :description
)";

    $stmt = $this->prepare( $sql );

    $args = array(
      'name' => $name,
      'description' => $description
    );

    $this->execute( $stmt, $sql, $args );

    return $this->fetch_id( $stmt );
    
  }
  
  public function update( $id, $name, $description ) {
    
    $sql = "
update v2_group
set
  name = :name,
  description = :description
where
  id = :id
";

    $stmt = $this->prepare( $sql );

    $args = array(
      'id' => $id,
      'name' => $name,
      'description' => $description
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
        'Group "%label%" is in use.',
        'label', $label
      );
      
    }
    
    $sql = "
delete from
  v2_group
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
}
