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

verify( ! user()->deleted );

html( intl()->get_lang() );

$css = <<<EOF
  .clear-button {
    border: outset 2px;
    height: 24px !important;
    display: inline-block;
    margin: 0px !important;
    padding: 0px !important;
  }
EOF;

render_head( 'Historical Reporting', $css );

render_page();

render_foot();

html()->render();

function render_page() {

  $tail = html()->tail;

  $table = new GrcTableView(
    'history',
    array(
      'id' => array(
        'sql' => 'h.id',
        'default' => false,
        'heading' => A( 'ID', CONTEXT_TABLE_HEADING ),
        'type' => INT_COLUMN,
      ),
      'timestamp' => array(
        'heading' => A( 'Timestamp', CONTEXT_TABLE_HEADING ),
        'type' => DATETIME_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
    
          $value = $row[ $column ];
          $date = new DateTime( $value );
          $timezone = $date->getTimezone()->getName();
          $tail->
            span(
              'title', aenc( get_tz_spec() )
            )->
              markup( nbsp( format_datetime( $date ) ) )->
            span_end();

          return;
          
          $value = $row[ $column ];
          $date = utc_to_local( $value );
          $timezone = $date->getTimezone()->getName();
          $tail->title = aenc( $timezone );
          //$tail->title = aenc( $value );
          $tail->text( format_datetime( $date ) );
        },
      ),
      'thermostat_id' => array(
        'default' => false,
        'heading' => A( 'Thermostat ID', CONTEXT_TABLE_HEADING ),
        'type' => INT_COLUMN
      ),
      'name' => array(
        'sql' => 't.name',
        'heading' => A( 'Thermostat', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          $value = $row[ $column ];
          $tail->
            new_link(
              '/home.php',
              nbsp( $value ),
              A( 'View this thermostat.' ),
              null,
              array( 'id[]' => read_a( $row, 'thermostat_id' ) )
            );
          
          //return format_grc_field( $field, $value );
        },
      ),
      'description' => array(
        'default' => false,
        'sql' => 't.description',
        'heading' => A( 'Description', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
      'location' => array(
        'sql' => 'l.name',
        'heading' => A( 'Location', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
      'username' => array(
        'sql' => 'u.username',
        'heading' => A( 'Username', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
      'group' => array(
        'sql' => 'g.name',
        'heading' => A( 'Group', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
      'field' => array(
        'heading' => A( 'Field', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
      'previous_value' => array(
        'heading' => A( 'Previous Value', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          $field = $row[ 'field' ];
          $value = $row[ $column ];
          return format_grc_field( $field, $value );
        },
      ),
      'current_value' => array(
        'heading' => A( 'Current Value', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          $field = $row[ 'field' ];
          $value = $row[ $column ];
          return format_grc_field( $field, $value );
        },
      ),
    )
  );
        
  $search = $table->search();
  
  $data = dal()->history->report(
    $search,
    $count,
    $table->page,
    $table->sort
  );
  
  //$total = dal()->history->report_total();
  
  $tail = $tail->form( 'method', 'GET' );
  
  $total = null;
  
  $table->render(
    $data,
    $tail,
    $total,
    null, // highlight_spec
    true, // top_nav
    true, // bottom_nav
    true, // sort_links
    true, // search_row
    'submit_clear' // clear_function
  );
  
  if ( count( $data ) === 0 ) {
    
    $tail = $tail->
      p( 'id', 'error', 'class', 'error' )->
        T_H( 'There are no records for your criteria.' )->
      p_end();
    
  }
  
  $tail = $tail->form_end();
  
}
