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

class GrcDalUser extends GrcDalModule {
  
  public static function Create( $dal ) {
    
    return new self( $dal );
    
  }
  
  //public function count_users() {
  public function count() {
    
    $sql = "
select
  count(*) as `count`
from
  v2_user
where
  deleted = 0
";

    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql );

    return $this->fetch_int( $stmt, 'count' );
        
  }
  

  public function &report(
    &$count = -1,
    $page = null,
    $sort = null
  ) {

    $sql = "
select
  u.id,
  u.username,
  u.is_admin,
  u.is_translator,
  u.location_id,
  u.max_thermostats,
  u.langtag,
  l.name as location_name,
  i.english_name as language
from
  v2_user u
left join
  v2_location l
on
  u.location_id = l.id
left join
  v2_intl_language i
on
  u.langtag = i.langtag
where
  u.deleted = 0
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

  public function &get_by_id( $id ) {
    
    $sql = 'select * from v2_user where id = :id and deleted = 0';
    
    $stmt = $this->prepare( $sql );
    
    $args = array( 'id' => $id );
    
    $this->execute( $stmt, $sql, $args );

    $this->fetch_row( $stmt, $row );
    
    return $row;
    
  }
  
  public function &get_by_username( $username ) {

    validator()->string( $username, 'Username', 1, 20 );
    
    $sql = '
select
  *
from
  v2_user
where
  username = :username
';
    
    $stmt = $this->prepare( $sql );

    $args = array(
      'username' => $username
    );

    $this->execute( $stmt, $sql, $args );

    $this->try_fetch_row( $stmt, $row );
    
    return $row;
    
  }
  
  public function username_exists( $username ) {
    
    $sql = '
select
  count(*) as `count`
from
  v2_user
where
  username = :username
';
    
    $stmt = $this->prepare( $sql );
    
    $args = array( 'username' => $username );
    
    $this->execute( $stmt, $sql, $args );

    $count = $this->fetch_int( $stmt, 'count' );

    return $count !== 0;
    
  }
  
  public function add(
    $username,
    $is_admin,
    $is_translator,
    $location_id,
    $max_thermostats = 0,
    $langtag = 'en'
  ) {

    validator()->string( $username, 'Username', 1, 20 );
    validator()->dbool( $is_admin, 'Is Admin' );
    validator()->dbool( $is_translator, 'Is Translator' );
    validator()->int( $location_id, 'Location ID' );
    validator()->int( $max_thermostats, 'Max Thermostats', 0, 9999 );
    validator()->string( $langtag, 'Langtag', 1, 64 );
    
    $sql = '
insert into `v2_user` (
  username,
  is_admin,
  is_translator,
  location_id,
  max_thermostats,
  langtag,
  xsrf
)
values (
  :username,
  :is_admin,
  :is_translator,
  :location_id,
  :max_thermostats,
  :langtag,
  :xsrf
)
';
    
    $stmt = $this->prepare( $sql );

    $args = array(
      'username' => $username,
      'is_admin' => $is_admin,
      'is_translator' => $is_translator,
      'location_id' => $location_id,
      'max_thermostats' => $max_thermostats,
      'langtag' => $langtag,
      'xsrf' => new_xsrf_token()
    );

    $this->execute( $stmt, $sql, $args );

    return $this->fetch_id( $stmt );
    
  }

  public function update(
    $id,
    $username,
    $is_admin,
    $is_translator,
    $location_id,
    $max_thermostats,
    $langtag
  ) {

    validator()->int( $id, 'ID', 1 );
    validator()->string( $username, 'Username', 1, 20 );
    validator()->dbool( $is_admin, 'Is Admin' );
    validator()->dbool( $is_translator, 'Is Translator' );
    validator()->int( $location_id, 'Location ID' );
    validator()->int( $max_thermostats, 'Max Thermostats', 0, 9999 );
    validator()->string( $langtag, 'Langtag', 1, 64 );
    
    $sql = "
update
  v2_user
set
  username = :username,
  is_admin = :is_admin,
  is_translator = :is_translator,
  location_id = :location_id,
  max_thermostats = :max_thermostats,
  langtag = :langtag
where
  id = :id
";

    $stmt = $this->prepare( $sql );

    $args = array(
      'id' => $id,
      'username' => $username,
      'is_admin' => $is_admin,
      'is_translator' => $is_translator,
      'location_id' => $location_id,
      'max_thermostats' => $max_thermostats,
      'langtag' => $langtag
    );

    $this->execute( $stmt, $sql, $args );

    $this->finish( $stmt );
    
  }
  
  public function update_account(
    $id,
    $location_id,
    $langtag
  ) {

    validator()->int( $id, 'User ID' );
    validator()->int( $location_id, 'Location ID' );
    validator()->string( $langtag, 'Langtag', 1, 64 );
    
    $sql = "
update
  v2_user
set
  location_id = :location_id,
  langtag = :langtag
where
  id = :id
";

    $stmt = $this->prepare( $sql );

    $args = array(
      'id' => $id,
      'location_id' => $location_id,
      'langtag' => $langtag
    );

    $this->execute( $stmt, $sql, $args );

    $this->finish( $stmt );
    
  }
  
  public function update_xsrf(
    $id,
    $xsrf
  ) {

    validator()->int( $id, 'ID', 1 );
    validator()->sha1( $xsrf, 'XSRF' );
    
    $sql = "
update v2_user
set
  xsrf = :xsrf
where
  id = :id
";

    $stmt = $this->prepare( $sql );

    $args = array(
      'id' => $id,
      'xsrf' => $xsrf
    );

    $this->execute( $stmt, $sql, $args );

    $this->finish( $stmt );
    
  }
  
  public function delete( $id ) {
        
    $sql = "
delete from
  v2_user
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

  public function soft_delete( $id ) {
        
    $sql = "
update
  v2_user
set
  deleted = 1
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
    
    $sql = '
select
  id,
  username as name
from
  v2_user
where
  deleted = 0
order by
  2 asc
';
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_map( $stmt, $map, 'id', 'name' );
    
    return $map;
    
  }

  public function get_max_thermostats( $user_id ) {

    $sql = '
select
  max_thermostats
from
  v2_user
where
  id = :id
';
    
    $stmt = $this->prepare( $sql );
    
    $args = array( 'id' => $user_id );
    
    $this->execute( $stmt, $sql, $args );
    
    return $this->fetch_int( $stmt, 'max_thermostats' );
    
  }
  
  public function get_used_thermostats( $user_id ) {
    
    $sql = '
select
  count( id ) as `count`
from
  v2_thermostat
where
  user_id = :user_id
and
  deleted = 0
';
    
    $stmt = $this->prepare( $sql );

    $args = array( 'user_id' => $user_id );
    
    $this->execute( $stmt, $sql, $args );
    
    return $this->fetch_int( $stmt, 'count' );
    
  }
  
  public function get_stats(
    $id,
    &$max_thermostats,
    &$used_thermostats
  ) {

    $max_thermostats = $this->get_max_thermostats( $id );
    $used_thermostats = $this->get_used_thermostats( $id );
   
  }
  
}

