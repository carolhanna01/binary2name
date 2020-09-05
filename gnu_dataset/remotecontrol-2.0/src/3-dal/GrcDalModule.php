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

class GrcDalModule {
  
  public $dal;
  
  protected function __construct( $dal ) {
    
    $this->dal = $dal;
    
  }
    
  public function __get( $feature ) {
    
    return $this->dal->$feature;
    
  }
  
  public function set_timezone() {
    
    return $this->dal->set_timezone();
    
  }
  
  public function close() {

    return $this->dal->close();
    
  }
  
  public function begin() {

    return $this->dal->begin();
    
  }
  
  public function commit() {

    return $this->dal->commit();
    
  }
  
  public function rollback() {

    return $this->dal->rollback();
    
  }
  
  public function &query( $sql ) {

    return $this->dal->query( $sql );
    
  }
  
  public function prepare( $sql ) {

    return $this->dal->prepare( $sql );
    
  }
  
  public function execute( $stmt, $sql, &$args = null ) {

    return $this->dal->execute( $stmt, $sql, $args );
        
  }
  
  public function finish( $stmt ) {
    
    return $this->dal->finish( $stmt );
    
  }
  
  public function fetch_row_count( $stmt ) {

    return $this->dal->fetch_row_count( $stmt );
    
  }
  
  public function fetch_id( $stmt ) {

    return $this->dal->fetch_id( $stmt );
    
  }
  
  public function fetch_row( $stmt, &$row ) {

    return $this->dal->fetch_row( $stmt, $row );
    
  }
  
  public function try_fetch_row( $stmt, &$row ) {

    return $this->dal->try_fetch_row( $stmt, $row );
    
  }
  
  public function fetch_field( $stmt, $field, $default = null ) {

    return $this->dal->fetch_field( $stmt, $field, $default );
    
  }
  
  public function fetch_int( $stmt, $field, $default = 0 ) {

    return $this->dal->fetch_int( $stmt, $field, $default );
   
  }
  
  public function fetch_string( $stmt, $field, $default = '' ) {

    return $this->dal->fetch_string( $stmt, $field, $default );
    
  }
  
  public function fetch_all( $stmt, &$table, $index = 0 ) {

    return $this->dal->fetch_all( $stmt, $table, $index );
    
  }
  
  public function fetch_map(
    $stmt,
    &$map,
    $key,
    $value = null,
    $index = 0
  ) {

    return $this->dal->fetch_map(
      $stmt,
      $map,
      $key,
      $value,
      $index
    );
    
  }
  
  public function escape_string( $value ) {

    return $this->dal->escape_string( $value );

  }

  public function escape_int( $value ) {

    return $this->dal->escape_int( $value );
    
  }
  
  public function escape_float( $value ) {

    return $this->dal->escape_float( $value );
    
  }

  public function escape_bool( $value ) {

    return $this->dal->escape_bool( $value );
    
  }
  
  public function sql_count( $sql, $args = array() ) {

    return $this->dal->sql_count( $sql, $args );

  }
  
  public function table_count( $table, $equal = array(), $not_equal = array() ) {
    
    return $this->dal->table_count( $table, $equal, $not_equal );
    
  }
  
  public function apply_search(
    &$sql,
    &$search,
    $where = 'where'
  ) {

    return $this->dal->apply_search( $sql, $search, $where );
    
  }  
}
