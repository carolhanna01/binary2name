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

render_head( 'Location Administration' );

render_page();

render_foot();

html()->render();

function render_page() {

  $tail = html()->tail;
  
  $table = new GrcTableView(
    'location',
    array(
      'id' => array(
        'width' => '10em',
        'default' => false,
        'heading' => A( 'Location ID', CONTEXT_TABLE_HEADING ),
        'type' => INT_COLUMN,
      ),
      'name' => array(
        'width' => '20em',
        'heading' => A( 'Location Name', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'total_units' => A( ' characters', CONTEXT_UNITS ),
        'format' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $tail->
            span(
              'id', 'location_name_' . $id . '_span',
              'class', 'view'
            )->
              T_T( $row[ $column ] )->
            span_end()->
            input(
              'id', 'location_name_' . $id . '_input',
              'type', 'text',
              'value', $row[ $column ],
              'class', 'view',
              'autocomplete', 'off'
            );
        },
      ),
      'timezone' => array(
        'width' => '42em',
        'align' => 'left',
        'heading' => A( nbsp( 'Time Zone' ), CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $timezone = $row[ $column ];
          $tail->
            label(
              'id', 'location_timezone_' . $id . '_label',
              'class', 'view',
              'style', 'width:100% !important;text-align:center;'
            )->
              text( get_timezone_label( $timezone ) )->
            label_end()->
            
            input(
              'id', 'location_timezone_' . $id . '_input_filter',
              'placeholder', 'filter',
              'onkeyup', 'process_title_filter(this)',
              'class', 'view'
            )->
            add(
              get_timezone_select(
                'location_timezone_' . $id . '_input',
                $timezone
              )->
              attr( 'class', 'view' )
            )->
            div(
              'id', 'location_timezone_' . $id . '_input_missing',
              'style', 'display:none;width:14em;color:red;',
              'class', 'view'
            )->
              T_H( 'No match' )->
            div_end();
        },
      ),
      'offset' => array(
        'width' => '10em',
        'align' => 'right',
        'is-computed' => true,
        'heading' => A( nbsp( 'UTC Offset' ), CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          $timezone = new DateTimeZone( $row[ 'timezone' ] );
          return format_utc_offset( $timezone );
        },
      ),
      'action' => array(
        'width' => '10em',
        'is-computed' => true,
        'heading' => TH( 'Action' ),
        'format' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $tail->
            a(
              'id', 'location_edit_' . $id,
              'class', 'edit',
              'href', 'javascript:void(0);',
              'onclick', 'edit_location( ' . $id . ' )'
            )->
              T_H( 'Edit' )->
            a_end()->
            nbsp()->
            a(
              'id', 'location_delete_' . $id,
              'class', 'delete',
              'href', 'javascript:void(0);',
              'onclick', 'delete_location( ' . $id . ' )'
            )->
              T_H( 'Delete' )->
            a_end()->
            a(
              'id', 'location_update_' . $id,
              'class', 'update',
              'href', 'javascript:void(0);',
              'onclick', 'update_location( ' . $id . ' )'
            )->
              T_H( 'Update' )->
            a_end()->
            nbsp()->
            a(
              'id', 'location_cancel_edit_' . $id,
              'class', 'cancel',
              'href', 'javascript:void(0);',
              'onclick', 'cancel_edit_location( ' . $id . ' )'
            )->
              T_H( 'Cancel' )->
            a_end();
        },
      ),
    ),
    array(
      'id' => array(
        'format' => function( $column, $schema, $tail ) {
          //$tail->text( $column );
        },
      ),
      'name' => array(
        'format' => function( $column, $schema, $tail ) {
          $tail->
            input(
              'id', 'location_name_0_input',
              'type', 'text',
              'class', 'edit required'
            );
        },
      ),
      'timezone' => array(
        'format' => function( $column, $schema, $tail ) {
          $tail->
            input(
              'id', 'location_timezone_0_input_filter',
              'placeholder', 'filter',
              'onkeyup', 'process_title_filter(this)'
            )->
            add(
              get_timezone_select(
                'location_timezone_0_input',
                DEFAULT_TIMEZONE
              )
            )->
            div(
              'id', 'location_timezone_0_input_missing',
              'style', 'display:none;width:14em;color:red;'
            )->
              T_H( 'No match' )->
            div_end();
        },
      ),
      'offset' => array(
        'format' => function( $column, $schema, $tail ) {
          $tail->span()->nbsp()->span_end();
        },
      ),
      'action' => array(
        'format' => function( $column, $schema, $tail ) {
          $tail->
            a(
              'id', 'location_add',
              'class', 'add',
              'href', 'javascript:void(0);',
              'onclick', 'add_location()'
            )->
              T_H( 'Add' )->
            a_end();
        },
      ),
    ),
    'id',
    'name'
  );
  
  $data = dal()->location->report(
    $count,
    $table->page,
    $table->sort
  );
  //$total = dal()->group->report_total();
  
  if ( dal()->location->count_all() == 0 ) {

    $tail->p()->T_H( "You must add at least one location." )->p_end();

  }

  //$table->render( $data, $tail, $total );
  $tail = $table->render(
    $data,
    $tail,
    $total, // total
    null, // highlight spec
    false,
    false,
    true
  );
  
}
