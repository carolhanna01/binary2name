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

require_once __DIR__ . '/../src/include.php';

global $error;

verify( is_administrator() );

html( intl()->get_lang() );

render_head( 'Exception List' );

render_page();

render_foot();

html()->render();

function render_page() {

  $tail = html()->tail;

  $table = new GrcTableView(
    'msg_admin',
    array(
      'id' => array(
        'heading' => A( 'ID', CONTEXT_TABLE_HEADING ),
        'type' => INT_COLUMN,
      ),
      'timestamp' => array(
        'heading' => A( 'Timestamp', CONTEXT_TABLE_HEADING ),
        'type' => DATETIME_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          $value = $row[ $column ];
          //$date = utc_to_local( $value );
          $date = new DateTime( $value );
          $timezone = $date->getTimezone()->getName();
          $tail->
            new_link(
              '/admin-exception.php',
              format_datetime( $date ),
              aenc( get_tz_spec() ),
              null,
              array( 'id' => $row[ 'id' ] )
            );
          
          return;
          $tail->title = aenc( $timezone );
          //$tail->title = aenc( $value );
          $tail->text( format_datetime( $date ) );
        },
      ),
      'user_id' => array(
        'heading' => A( 'User', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          //$tail->add( get_user_select( read_a( $row, 'user_id' ) ) );
          return read_a( $row, 'username' );
        },
      ),
      'type' => array(
        'heading' => A( 'Type', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
      'previous_id' => array(
        'default' => false,
        'heading' => A( 'Previous', CONTEXT_TABLE_HEADING ),
        'type' => INT_COLUMN
      ),
      'message' => array(
        'heading' => A( 'Message', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'total_units' => A( ' characters', CONTEXT_UNITS ),
      ),
      'code' => array(
        'heading' => A( 'Code', CONTEXT_TABLE_HEADING ),
        'type' => ID_COLUMN,
      ),
      'file' => array(
        'heading' => A( 'File', CONTEXT_TABLE_HEADING ),
        'type' => FILE_COLUMN,
        'total_units' => A( ' characters', CONTEXT_UNITS ),
      ),
      'line' => array(
        'heading' => A( 'Line', CONTEXT_TABLE_HEADING ),
        'type' => INT_COLUMN,
      ),
      'message_stack' => array(
        'default' => false,
        'heading' => A( 'Message Stack', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'total_units' => A( ' characters', CONTEXT_UNITS ),
      ),
      'trace' => array(
        'default' => false,
        'heading' => A( 'Trace', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'total_units' => A( ' characters', CONTEXT_UNITS ),
      ),
    )
  );
  
  $data = dal()->exception->report( $count, $table->page, $table->sort );
  $total = dal()->exception->report_total();
  
  if ( count( $data ) === 0 ) {

    $tail->p()->T_H( 'No errors have been reported.' )->p_end();
    
  } 
  else {

    $table->render( $data, $tail, $total );

  }
}
