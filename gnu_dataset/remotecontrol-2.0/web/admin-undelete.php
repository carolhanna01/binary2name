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

global $error, $default, $input;

verify( is_administrator() );

// input is read into here:
$input = array();

//input( 'action', 'home_page_action' );


html( intl()->get_lang() );

render_head( 'Undelete Thermostats' );

render_page( $input, $error );

render_foot();

html()->render();

function render_page( &$input, &$error ) {

  $tail = html()->tail;

  $table = new GrcTableView(
    'undelete',
    array(
      /*
      'select' => array(
        'is-computed' => true,
        'no-menu' => true,
        'heading' => function() {
          return '<input id="select_all" type="checkbox" onchange="toggle_all()">';
        },
        'format' => function( &$row, $column, $schema, $tail )
          use ( $input ) {
          
          $id_list = read_a( $input, 'id' );
          
          $checked = (
            is_array( $id_list ) &&
            in_array( $row[ 'id' ], $id_list )
          );
          
          $tail->input(
            'name', 'id[]',
            'type', 'checkbox',
            'class', 'group-' . $row[ 'group_id' ],
            'value', $row[ 'id' ],
            'onchange', 'toggle_row(' . $row[ 'id' ] . ')',
            'checked', $checked
          );
        },
      ),
      */
      /*
      'load' => array(
        'is-computed' => true,
        'format' => function( &$row, $column, $schema, $tail ) {
          $tail->
            a(
              'class', 'load',
              'href', 'javascript:void(0)',
              'onclick', 'load_thermostat(' . $row[ 'id' ] . ',this)'
            )->
              T_H( 'Load' )->
            a_end();
        },
      ),
      */
      'id' => array(
        'heading' => TABLE_HEADING( 'ID' ),
        'type' => INT_COLUMN,
      ),
      'location_id' => array(
        'sql' => 'l.id',
        'default' => false,
        'heading' => TABLE_HEADING( 'Location ID' ),
        'type' => INT_COLUMN,
      ),
      'location' => array(
        'sql' => 'l.name',
        'heading' => TABLE_HEADING( 'Location' ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          return henc( read_a( $row, 'location' ) );
        },
      ),
      'group_id' => array(
        'sql' => 'g.id',
        'default' => false,
        'heading' => TABLE_HEADING( 'Group ID' ),
        'type' => INT_COLUMN,
      ),
      'group' => array(
        'sql' => 'g.name',
        'heading' => TABLE_HEADING( 'Group' ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $tail->
            span(
              'id', 'group_name_' . $id . '_span',
              'class', 'view'
            )->
              //T_T( $row[ $column ] )->
              a(
                'href', 'javascript:void(0)',
                'onclick', 'select_group(' . $row[ 'group_id' ] . ')'
              )->
                markup( $row[ $column ] )->
              a_end()->
            span_end();
        },
      ),
      'name' => array(
        'sql' => 't.name',
        'heading' => TABLE_HEADING( 'Custom Name' ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $timezone = $row[ $column ];
          $tail->
            label(
              'id', 'name_' . $id . '_label',
              'class', 'view',
              'style', 'width:100% !important;text-align:center;'
            )->
              text( read_a( $row, 'name' ) )->
            label_end();
        },
      ),
      'description' => array(
        'sql' => 't.description',
        'heading' => TABLE_HEADING( 'Description' ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $timezone = $row[ $column ];
          $tail->
            label(
              'id', 'description_' . $id . '_label',
              'class', 'view',
              'style', 'width:100% !important;text-align:center;'
            )->
              text( read_a( $row, 'description' ) )->
            label_end();
        },
      ),
      'host' => array(
        'default' => false,
        'heading' => TABLE_HEADING( 'Address' ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $tail->
            label(
              'id', 'description_' . $id . '_label',
              'class', 'view',
              'style', 'width:100% !important;text-align:center;'
            )->
              text( read_a( $row, 'host' ) )->
            label_end();
        },
      ),
      'port' => array(
        'default' => false,
        'heading' => TABLE_HEADING( 'Port' ),
        'type' => INT_COLUMN,
      ),
      'user' => array(
        'default' => false,
        'heading' => TABLE_HEADING( 'Auth User' ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          return read_a( $row, 'user' );
        },
      ),
      'action' => array(
        'is-computed' => true,
        'format' => function( &$row, $column, $schema, $tail ) {
          $tail->
            a(
              'class', 'load',
              'href', 'javascript:void(0)',
              'onclick', 'undelete_thermostat(' . $row[ 'id' ] . ',this)'
            )->
              T_H( 'Restore' )->
            a_end();
          return;
        },
      ),
    )
  );
  
  $data = dal()->thermostat->graveyard(
    $count,
    $table->page, 
    $table->sort
  );

  if ( count( $data ) === 0 ) {

    $tail->
      p()->
        T_H(
          'You don\'t have any deleted thermostats.'
        )->
      p_end();
    
  } 
  else {

    $table->render( $data, $tail );
    
  }
}
