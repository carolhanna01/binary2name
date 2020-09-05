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

class GrcDalException extends GrcDalModule {
  
  public static function Create( $dal ) {
    
    return new self( $dal );
    
  }
  
  public function log( $ex, &$messages = array() ) {

    if ( $ex === null ) { return 0; }
    
    $user_id = user()->id;
    
    $previous_id = $this->log( $ex->getPrevious(), $messages );

    $type = get_class( $ex );
    
    $message = $ex->getMessage();
    
    $messages[] = $message;
    
    $code = intval( $ex->getCode() );
    $file = $ex->getFile();
    $line = intval( $ex->getLine() );
    $message_stack = implode( "\n--\n", $messages );
    $trace = $ex->getTraceAsString();
    $trace_data = '';
    
    try {
      $trace_data = json_encode( $ex->getTrace() );
    }
    catch( Exception $ex ) {
      // we tried...
    }
    //$trace_data = '';
  
    /*
    if ( $code < INT32_MIN || $code > INT32_MAX ) {
      $message = "Code: $code: $message";
      $code = -1;
    }
    
    if ( $line < 1 || $line > UINT16_MAX ) {
      $message = "Line: $line: $message";
      $line = 0;
    }
    */
    
    $sql = "
insert into v2_exception_log (
  user_id,
  previous_id,
  type,
  message,
  `code`,
  `file`,
  line,
  message_stack,
  trace,
  trace_data
)
values (
  :user_id,
  :previous_id,
  :type,
  :message,
  :code,
  :file,
  :line,
  :message_stack,
  :trace,
  :trace_data
)";

    $args = array(
      'user_id' => $user_id,
      'previous_id' => $previous_id,
      'type' => $type,
      'message' => $message,
      'code' => $code,
      'file' => $file,
      'line' => $line,
      'message_stack' => $message_stack,
      'trace' => $trace,
      'trace_data' => $trace_data,
    );
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql, $args );
    
    return $this->fetch_id( $stmt );
    
  }
  
  public function &report(
    &$count = -1,
    $page = null,
    $sort = null
  ) {

    $this->set_timezone();
    
    $sql = "
select
  e.*,
  u.username
from
  v2_exception_log e
left join
  v2_user u
on
  e.user_id = u.id
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

    $this->set_timezone();
    
    $sql = "
select
  count( distinct e.id ) as id,
  count( distinct user_id ) as user_id,
  concat( min( `timestamp` ), ' to ', max( `timestamp` ) ) as `timestamp`,
  count( distinct type ) as type,
  count( distinct previous_id ) as previous_id,
  concat( min( length( `message` ) ), ' to ', max( length( `message` ) ) ) as `message`,
  concat( min( `code` ), ' to ', max( `code` ) ) as `code`,
  concat( min( length( `file` ) ), ' to ', max( length( `file` ) ) ) as `file`,
  concat( min( `line` ), ' to ', max( `line` ) ) as `line`,
  concat( min( length( `message_stack` ) ), ' to ', max( length( `message_stack` ) ) ) as `message_stack`,
  concat( min( length( `trace` ) ), ' to ', max( length( `trace` ) ) ) as `trace`,
  concat( min( length( `trace_data` ) ), ' to ', max( length( `trace_data` ) ) ) as `trace_data`,
  count( distinct u.username ) as username
from
  v2_exception_log e
left join
  v2_user u
on
  e.user_id = u.id
";

    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_row( $stmt, $row );
    
    return $row;
    
  }
  
  public function &get_by_id( $id ) {

    $this->set_timezone();
    
    $sql = '
select
  e.*,
  u.username
from
  v2_exception_log e
left join
  v2_user u
on
  e.user_id = u.id
where
  e.id = :id
';

    $args = array( 'id' => $id );
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql, $args );
    
    $this->fetch_row( $stmt, $result );
    
    return $result;
    
  }
}
