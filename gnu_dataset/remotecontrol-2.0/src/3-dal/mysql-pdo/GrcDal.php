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

class GrcDal extends GrcService {

  public $log;
  
  private $cn;
  private $tran_count;
  
  protected function __construct( $cn ) {

    $this->log = array();
    
    parent::__construct( 'GrcDal', __DIR__ );
    
    $this->cn = $cn;
    
    $this->tran_count = 0;
    
  }
  
  public function __destruct() {
    
    $this->close();
    
  }
  
  public static function Create( $persistent = true ) {

    $persistent_string = $persistent ? '1' : '0';
      
    $cn = new PDO(
      "mysql:dbname=" . DB_NAME . ";host=" . DB_HOST . ";charset=utf8",
      DB_USER,
      DB_PASS,
      array(
        PDO::ATTR_ERRMODE             => PDO::ERRMODE_EXCEPTION,
        PDO::ATTR_ORACLE_NULLS        => PDO::NULL_NATURAL,
        PDO::ATTR_STRINGIFY_FETCHES   => false,
        PDO::ATTR_TIMEOUT             => 20,
        PDO::ATTR_AUTOCOMMIT          => true,
        PDO::ATTR_EMULATE_PREPARES    => false,
        PDO::ATTR_DEFAULT_FETCH_MODE  => PDO::FETCH_ASSOC,
        PDO::ATTR_PERSISTENT          => $persistent,
      )
    );

    $cn->exec( "SET sql_mode = 'STRICT_TRANS_TABLES'" );
    $cn->exec( 'SET foreign_key_checks = 0' );
    //$cn->exec( "SET time_zone = '+0:00'" );
    //$cn->exec( 'set innodb_lock_wait_timeout = 1' );
    
    return new GrcDal( $cn );
    
  }
  
  public function set_timezone() {

    $this->cn->exec(
      "SET time_zone = '" . get_tz_spec() . "'"
    );
    
  }
  
  public function close() {
    
    if ( $this->cn === null ) { return; }
    
    $this->rollback();
    
    $this->cn = null;
    
  }
  
  public function begin() {
    
    $this->tran_count++;
    
    $this->cn->beginTransaction();
    
  }
  
  public function commit() {
    
    $this->tran_count--;
    
    if ( $this->tran_count === 0 ) {
    
      $this->cn->commit();
    
    }
    else if ( $this->tran_count < 0 ) {
      
      $this->tran_count = 0;
      
    }
  }
  
  public function rollback() {

    if ( $this->tran_count ) {

      $this->cn->rollBack();
      
    }
    
    $this->tran_count = 0;
    
  }
  
  public function &query( $sql ) {
    
    $result = $this->cn->query( $sql );
    
    if ( is_bool( $result ) ) { return $result; }
    
    $table = $stmt->fetchAll( PDO::FETCH_ASSOC );

    $this->finish( $stmt );

    return $table;
    
  }
  
  public function prepare( $sql ) {
    
    static $options = array(
      PDO::ATTR_CURSOR => PDO::CURSOR_FWDONLY
    );
    
    if ( $stmt = $this->cn->prepare( $sql ) ) { return $stmt; }

    //var_dump( $sql );
    
    throw Error(
      'Failed to create prepared statement.'
    );
    
  }
  
  public function execute( $stmt, $sql, &$args = null ) {
    
    //$this->log[] = array( 'sql' => $sql, 'args' => $args );
    
    try {

      if ( $stmt->execute( $args ) ) { return; }
      
    }
    catch ( Exception $ex ) {

      //dump( array( 'sql' => $sql, 'args' => $args ) );

      if ( $ex->getCode() == 23000 ) {
        
        $arg_details = '';
        
        foreach ( $args as $name => $value ) {
          
          $arg_details .= ' ' . $name . ': '.
            truncate_text( $value );
          
        }

        $arg_details = truncate_text( $arg_details );
        
        throw Error(
          'Record exists: %args%',
          'args', $arg_details,
          $ex
        );
        
      }
      
      throw Error(
        'Failed to execute prepared statement for SQL "%sql%".',
        'sql', $sql,
        $ex
      );
      
    }

    throw Error(
      'Failed to execute prepared statement for SQL "%sql%".',
      'sql', $sql
    );
        
  }
  
  public function finish( $stmt ) {
    
    if ( $stmt->closeCursor() ) { return; }

    throw Error( 'Failed to close cursor.' );
    
  }
  
  public function fetch_row_count( $stmt ) {
    
    $row_count = $stmt->rowCount();
    
    $this->finish( $stmt );

    return $row_count;
    
  }
  
  public function fetch_id( $stmt ) {
    
    $id = $this->cn->lastInsertId();
    
    $this->finish( $stmt );

    return ( $id === null ) ? null : intval( $id );
    
  }
  
  public function fetch_row( $stmt, &$row ) {

    $row = null;
    
    try {
      
      $row = $stmt->fetch( PDO::FETCH_ASSOC );

    }
    catch ( Exception $ex ) {

      $this->finish( $stmt );
      
      throw $ex;
      
    }

    $this->finish( $stmt );

    if ( is_array( $row ) ) { return; }
    
    throw Error(
      'Failed to fetch row.'
    );
    
  }
  
  public function try_fetch_row( $stmt, &$row ) {

    $row = null;
    
    try {
      
      $row = $stmt->fetch( PDO::FETCH_ASSOC );

    }
    catch ( Exception $ex ) {

      $this->finish( $stmt );
      
      throw $ex;
      
    }
    
    $this->finish( $stmt );

    return $row === false ? false : true;
    
  }
  
  public function fetch_field( $stmt, $field, $default = null ) {
    
    if ( $this->try_fetch_row( $stmt, $row ) ) {

      if ( array_key_exists( $field, $row ) ) {
        
        $value = $row[ $field ];
        
        if ( $value === null ) { return $default; }
        
        return $value;
        
      }
      
      throw Error(
        'Missing field "%field%".',
        'field', $field
      );
            
    }
    
    return $default;
    
  }
  
  public function fetch_int( $stmt, $field, $default = 0 ) {
    
    return intval( $this->fetch_field( $stmt, $field, $default ) );
    
  }
  
  public function fetch_string( $stmt, $field, $default = '' ) {
    
    return trim( $this->fetch_field( $stmt, $field, $default ) );
    
  }
  
  public function fetch_all( $stmt, &$table, $index = 0 ) {

    try {
      
      $table = $stmt->fetchAll( PDO::FETCH_ASSOC );
      
    }
    catch ( Exception $ex ) {

      $this->finish( $stmt );
      
      throw $ex;
      
    }

    $this->finish( $stmt );
    
    if ( ! is_array( $table ) ) {

      throw Error( 'Failed to fetch table.' );

    }

    $len = count( $table );

    for ( $i = 0; $i < $len; $i++ ) {

      $table[ $i ][ 'index' ] = $index++;

    }      
  }
  
  public function fetch_map(
    $stmt,
    &$map,
    $key,
    $value = null,
    $index = 0
  ) {
    
    $this->fetch_all( $stmt, $table, $index );
    
    $map = array();
    
    foreach ( $table as &$row ) {
      
      if ( $value ) {
      
        $map[ $row[ $key ] ] = $row[ $value ];
        
      }
      else {

        $map[ $row[ $key ] ] = &$row;
        
      }      
    }
  }
  
  public function escape_string( $value ) {

    return $this->cn->quote( $value, PDO::PARAM_STR );

  }

  public function escape_int( $value ) {

    return strval( intval( $value ) );
    
  }
  
  public function escape_float( $value ) {
    
    return strval( floatval( $value ) );
    
  }

  public function escape_bool( $value ) {

    return $value ? 1 : 0;

  }
  
  public function sql_count( $sql, $args = array() ) {

    $stmt = $this->prepare( $sql );
      
    $this->execute( $stmt, $sql, $args );

    $count = $stmt->rowCount();

    $this->finish( $stmt );

    return intval( $count );

  }
  
  public function table_count( $table, $equal = array(), $not_equal = array() ) {

    $where = '';
    
    $args = array();
    
    foreach ( $equal as $key => $value ) {
      
      $where .= strlen( $where ) ? ' and ' : ' where ';

      $where .= "`$key` = :$key";
    
      $args[ $key ] = $value;
      
    }
    
    foreach ( $not_equal as $key => $value ) {
      
      $where .= strlen( $where ) ? ' and ' : ' where ';

      $where .= "`$key` <> :$key";

      $args[ $key ] = $value;
      
    }
    
    $sql = "select count(*) as `count` from $table $where";
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql, $args );
 
    return $this->fetch_int( $stmt, 'count' );
    
  }  
  
  public function id_exists( $table, $id ) {
    
    $sql = "
select
  count(*) as count
from
  $table
where
  id = :id
";

    $args = array( 'id' => $id );
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql, $args );
 
    return $this->fetch_int( $stmt, 'count' ) === 1;
    
  }

  public function &get_groups_old() {
        
    $sql = "
select
  GroupId,
  Name,
  Description
from
  thermostatgroups
";
    
    $sql = 'select * from v2_group';
    
    $stmt = $this->prepare( $sql );
 
    $this->execute( $stmt, $sql );

    $this->fetch_all( $stmt, $result );

    return $result;
    
  }

  public function apply_search(
    &$sql,
    &$search,
    $type = 'where'
  ) {
    
    $i = 0;

    foreach ( $search as $key => $val ) {

      $val = strval( $val );
      
      if ( $val === '' ) { continue; }
      
      $like = $this->escape_string( '%' . $val . '%' );
      
      $sql .= ( $i++ === 0 ) ? "$type " : 'and ';
      
      $sql .= "$key like " . $like;
      
    }
  }
}
