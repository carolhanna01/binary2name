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

class GrcDalThermostat extends GrcDalModule {

  public static function Create($dal) {

    return new self($dal);

  }

  public function insert( &$data ) {

    validator()->thermostat_name( $data[ 'name' ] );
    validator()->thermostat_description( $data[ 'description' ] );
    validator()->thermostat_host( $data[ 'host' ] );
    validator()->thermostat_port( $data[ 'port' ] );
    validator()->thermostat_user( $data[ 'user' ] );
    validator()->thermostat_pass( $data[ 'pass' ] );
    validator()->location_id( $data[ 'location_id' ] );
    validator()->group_id( $data[ 'group_id' ] );
    validator()->user_id( $data[ 'user_id' ] );

    $args = array();

    read_in( 'name', $data, $args );
    read_in( 'description', $data, $args );
    read_in( 'host', $data, $args );
    read_in( 'port', $data, $args );
    read_in( 'user', $data, $args );
    read_in( 'pass', $data, $args );
    read_in( 'location_id', $data, $args );
    read_in( 'group_id', $data, $args );
    read_in( 'user_id', $data, $args );

    $sql = "
replace into v2_thermostat (
name,
group_id,
description,
host,
port,
user,
pass,
location_id,
user_id,
deleted
)
values (
:name,
:group_id,
:description,
:host,
:port,
:user,
:pass,
:location_id,
:user_id,
0
)";

    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql, $args );

    return $this->fetch_id( $stmt );

  }

  public function update( &$data ) {

    validator()->thermostat_id( $data[ 'id' ] );
    validator()->thermostat_name( $data[ 'name' ] );
    validator()->thermostat_description( $data[ 'description' ] );
    validator()->thermostat_host( $data[ 'host' ] );
    validator()->thermostat_port( $data[ 'port' ] );
    validator()->thermostat_user( $data[ 'user' ] );
    validator()->thermostat_pass( $data[ 'pass' ] );
    validator()->location_id( $data[ 'location_id' ] );
    validator()->group_id( $data[ 'group_id' ] );
    validator()->user_id( $data[ 'user_id' ] );

    $args = array();

    read_in( 'id', $data, $args );
    read_in( 'name', $data, $args );
    read_in( 'description', $data, $args );
    read_in( 'host', $data, $args );
    read_in( 'port', $data, $args );
    read_in( 'user', $data, $args );
    read_in( 'pass', $data, $args );
    read_in( 'location_id', $data, $args );
    read_in( 'group_id', $data, $args );
    read_in( 'user_id', $data, $args );

    $sql = "
update v2_thermostat set
name = :name,
group_id = :group_id,
description = :description,
host = :host,
port = :port,
user = :user,
pass = :pass,
location_id = :location_id,
user_id = :user_id,
deleted = 0
where
id = :id
";

    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql, $args );

    return $this->fetch_row_count( $stmt );

  }

  public function undelete( $id ) {

    validator()->thermostat_id( $id );

    $args = array( 'id' => $id );

    $sql = "
update
  v2_thermostat
set
  deleted = 0
where
  id = :id
";

    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql, $args );

    return $this->fetch_row_count( $stmt );

  }

  public function delete(
    $user_id,
    $id
  ) {

    validator()->user_id( $user_id );
    validator()->thermostat_id( $id );

    $sql = "
delete from
v2_thermostat 
where
user_id = :user_id
and
id = :id
";

    $args = array(
      'user_id' => $user_id,
      'id' => $id
    );

    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql, $args );

    return $this->fetch_row_count( $stmt );

  }

  public function soft_delete(
    //$user_id,
    $id
  ) {

    //validator()->user_id( $user_id );
    validator()->thermostat_id( $id );

    $sql = "
update
  v2_thermostat
set
  deleted = 1
where
  id = :id
";

    $args = array(
      //'user_id' => $user_id,
      'id' => $id
    );

    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql, $args );

    return $this->fetch_row_count( $stmt );

  }

  public function get_max_thermostats_for_username($username) {
      validator()->string($username, 'Username', 1, 20);
      $sql = 'select max_thermostats from `v2_user` where username = :username';
      $stmt = $this->prepare($sql);
      $args = array(
          'username' => $username
      );
      $this->execute($stmt, $sql, $args);
      return $this->fetch_int($stmt, 'max_thermostats');
  }

  public function get_user_id_for_thermostat_id($thermostat_id) {
      validator()->int($thermostat_id, 'Thermostat ID', 1);
      $sql = 'select user_id from v2_thermostat where id = :id';
      $stmt = $this->prepare($sql);
      $args = array(
          'id' => $thermostat_id
      );
      $this->execute($stmt, $sql, $args);
      return $this->fetch_int($stmt, 'user_id');
  }

  public function &get_all(
    &$count = -1,
    $page = null,
    $sort = null
  ) {

    static $sort_spec = array(
      'id' => 'id',
      'location_id' => 'l.id',
      'location' => 'l.name',
      'group_id' => 'g.id',
      'group' => 'g.name',
      'name' => 't.name',
      'description' => 't.description',
      'host' => 't.host',
      'port' => 't.port',
      'username' => 'u.username'
    );

    $sql = "
select
  t.id as id,
  l.id as location_id,
  l.name as location,
  g.id as group_id,
  g.name as `group`,
  t.name,
  t.description,
  host,
  port,
  user,
  pass,
  u.username as username
from
  v2_thermostat t
left join
  v2_location l
on
  t.location_id = l.id
left join
  v2_group g
on
  t.group_id = g.id
left join
  v2_user u
on
  t.user_id = u.id
where
  t.deleted = 0
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

    $this->fetch_all( $stmt, $result, $start );

    return $result;

  }

  public function &get_for_user_id(
    $user_id,
    &$count = -1,
    $page = null,
    $sort = null
  ) {

    static $sort_spec = array(
      'id' => 'id',
      'location_id' => 'l.id',
      'location' => 'l.name',
      'group_id' => 'g.id',
      'group' => 'g.name',
      'name' => 't.name',
      'description' => 't.description',
      'host' => 't.host',
      'port' => 't.port',
      'username' => 'u.username'
    );

    validator()->int( $user_id, 'User ID', 1 );

    $sql = "
select
  t.id as id,
  l.id as location_id,
  l.name as location,
  g.id as group_id,
  g.name as `group`,
  t.name,
  t.description,
  host,
  port,
  user,
  pass,
  u.username as username
from
  v2_thermostat t
left join
  v2_location l
on
  t.location_id = l.id
left join
  v2_group g
on
  t.group_id = g.id
left join
  v2_user u
on
  t.user_id = u.id
where
  t.user_id = :user_id
and
  t.deleted = 0
";

    $args = array(
      'user_id' => $user_id
    );

    $start = 0;

    if ( $page ) {

      $count = $this->sql_count( $sql, $args );

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

    $this->execute( $stmt, $sql, $args );

    $this->fetch_all( $stmt, $result, $start );

    return $result;

  }

  public function &graveyard(
    &$count = -1,
    $page = null,
    $sort = null
  ) {

    static $sort_spec = array(
      'id' => 'id',
      'group_id' => 'g.id',
      'group' => 'g.name',
      'name' => 't.name',
      'description' => 't.description',
      'host' => 't.host',
      'port' => 't.port',
      'location_id' => 'l.id',
      'location' => 'l.name'
    );

    //validator()->int( $user_id, 'User ID', 1 );

    $sql = "
select
t.id as id,
g.id as group_id,
g.name as `group`,
t.name,
t.description,
host,
port,
user,
pass,
l.id as location_id,
l.name as location
from
v2_thermostat t
left join
v2_group g
on
t.group_id = g.id
left join
v2_location l
on
t.location_id = l.id
where
t.deleted = 1
";

    $args = array(
      //'user_id' => $user_id
    );
    

    $start = 0;

    if ( $page ) {

      $count = $this->sql_count( $sql, $args );

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

    $this->execute( $stmt, $sql, $args );

    $this->fetch_all( $stmt, $result, $start );

    return $result;

  }

  public function &get_by_id( $id ) {

    validator()->int( $id, 'Thermosat ID', 1 );

    $sql = "
select
  t.id as id,
  l.id as location_id,
  l.name as location,
  g.id as group_id,
  g.name as `group`,
  t.name,
  t.description,
  host,
  port,
  user,
  pass,
  user_id
from
  v2_thermostat t
left join
  v2_group g
on
  t.group_id = g.id
left join
  v2_location l
on
  t.location_id = l.id
where
  t.id = :id
";

    $args = array(
      'id' => $id
    );

    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql, $args );

    $this->fetch_row( $stmt, $result );

    return $result;

  }

  public function &get_by_user_and_id(
    $user_id,
    $id
  ) {

    validator()->int( $user_id, 'User ID', 1 );
    validator()->int( $id, 'Thermosat ID', 1 );

    $sql = "
select
t.id as id,
g.id as group_id,
g.name as `group`,
t.name,
t.description,
host,
port,
user,
pass,
l.id as location_id,
l.name as location
from
v2_thermostat t
left join
v2_group g
on
t.group_id = g.id
left join
v2_location l
on
t.location_id = l.id
where
t.user_id = :user_id
and
t.id = :id
";

    $args = array(
      'user_id' => $user_id,
      'id' => $id
    );

    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql, $args );

    $this->fetch_row( $stmt, $result );

    return $result;

  }

  public function count_for_user( $user_id ) {

    $sql = '
select
count(*) as count
from
v2_thermostat
where
user_id = :user_id
';

    $args = array( 'user_id' => $user_id );
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql, $args );
    
    return $this->fetch_int( $stmt, 'count' );

  }
  
  function &get_all_active() {
    
    $sql = '
select
  id 
from
  v2_thermostat
where
  deleted = 0
';

    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_all( $stmt, $table );
    
    return $table;
    
  }
  
}
