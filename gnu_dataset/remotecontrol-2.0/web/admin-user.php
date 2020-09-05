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

render_head( 'User Administration' );

render_page();

render_foot();

html()->render();

function render_page() {

  $tail = html()->tail;
  
  $table = new GrcTableView(
    'user',
    array(
      'id' => array(
        'width' => '10em',
        'default' => false,
        'heading' => A( 'User ID', CONTEXT_TABLE_HEADING ),
        'type' => INT_COLUMN,
        'align' => 'left',
      ),
      'username' => array(
        'width' => '12em',
        'heading' => A( 'Username', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'total_units' => A( ' characters', CONTEXT_UNITS ),
        'align' => 'left',
        'format' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $tail->
            span(
              'id', 'user_username_' . $id . '_span',
              'class', 'view'
            )->
              text( $row[ $column ] )->
              //add( get_user_menu( $id, $row[ 'username' ] ) )->
            span_end()->
            input(
              'id', 'user_username_' . $id . '_input',
              'type', 'text',
              'value', $row[ $column ],
              'class', 'view',
              'autocomplete', 'off',
              'style', 'width:8em'
            );
        },
      ),
      'is_admin' => array(
        'width' => '7em',
        //'no-sort' => true,
        'type' => CHECKBOX_COLUMN,
        'align' => 'center',
      ),
      'is_translator' => array(
        'width' => '10em',
        //'no-sort' => true,
        'type' => CHECKBOX_COLUMN,
        'align' => 'center',
      ),
      'location_id' => array(
        'width' => '12em',
        'heading' => A( 'Location', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'align' => 'left',
        'format' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $tail->
            label(
              'id', 'user_location_' . $id . '_label',
              'class', 'view'
              //'style', 'width:100% !important;text-align:center;'
            )->
              text( read_a( $row, 'location_name' ) )->
            label_end()->
            
            add(
              get_location_select(
                'user_location_' . $id . '_input',
                $row[ 'location_id' ]
              )->
              attr( 'class', 'view' )
            );
        },        
        'gather' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $tail->
            text( read_a( $row, 'location_name' ) );
        },        
      ),
      'max_thermostats' => array(
        'width' => '9em',
        'heading' => nbsp( 'Thermostat Limit' ),
        'type' => STRING_COLUMN,
        'align' => 'left',
        'format' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $tail->
            span(
              'id', 'user_max_thermostats_' . $id . '_span',
              'class', 'view'
            )->
              text( format_int( $row[ $column ] ) )->
            span_end()->
            input(
              'id', 'user_max_thermostats_' . $id . '_input',
              'type', 'text',
              'value', $row[ $column ],
              'class', 'view',
              'autocomplete', 'off',
              'style', 'width:5em'
            );
        },
      ),
      'langtag' => array(
        'width' => '18em',
        'type' => STRING_COLUMN,
        'heading' => A( 'Language', CONTEXT_TABLE_HEADING ),
        'align' => 'left',
        'format' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $tail->
            label(
              'id', 'user_langtag_' . $id . '_label',
              'class', 'view'
              //'style', 'width:100% !important;text-align:center;'
            )->
              text( read_a( $row, 'language' ) )->
            label_end()->
            
            add(
              get_language_select(
                'user_langtag_' . $id . '_input',
                //$row[ 'location_id' ]
                $row[ 'langtag' ]
              )->
              attr( 'class', 'view' )
            );
        },        
      ),          
      'action' => array(
        'width' => '10em',
        'is-computed' => true,
        'heading' => TH( 'Action' ),
        'align' => 'center',
        'format' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $tail->
            a(
              'id', 'user_edit_' . $id,
              'class', 'edit',
              'href', 'javascript:void(0);',
              'onclick', 'edit_user( ' . $id . ' )'
            )->
              T_H( 'Edit' )->
            a_end()->
            nbsp()->
            a(
              'id', 'user_delete_' . $id,
              'class', 'delete',
              'href', 'javascript:void(0);',
              'onclick', 'delete_user( ' . $id . ' )'
            )->
              T_H( 'Delete' )->
            a_end()->            
            a(
              'id', 'user_update_' . $id,
              'class', 'update',
              'href', 'javascript:void(0);',
              'onclick', 'update_user( ' . $id . ' )'
            )->
              T_H( 'Update' )->
            a_end()->
            nbsp()->
            a(
              'id', 'user_cancel_edit_' . $id,
              'class', 'cancel',
              'href', 'javascript:void(0);',
              'onclick', 'cancel_edit_user( ' . $id . ' )'
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
      'username' => array(
        'format' => function( $column, $schema, $tail ) {
          $tail->
            input(
              'id', 'user_username_0_input',
              'type', 'text',
              'class', 'edit required',
              'style', 'width:8em'
            );
        },
      ),
      'is_admin' => array(
        'format' => function( $column, $schema, $tail ) {
          $tail->input(
            'id', 'user_is_admin_0_input',
            'type', 'checkbox',
            'checked', false
          );
        },
      ),
      'is_translator' => array(
        'format' => function( $column, $schema, $tail ) {
          $tail->input(
            'id', 'user_is_translator_0_input',
            'type', 'checkbox',
            'checked', false
          );
        },
      ),
      'location_id' => array(
        'format' => function( $column, $schema, $tail ) {
          $tail->
            /*
            input(
              'id', 'user_location_0_input_filter',
              'placeholder', 'filter',
              'onkeyup', 'process_title_filter(this)'
            )->
            */
            add(
              get_location_select(
                'user_location_0_input'
              )
            );
            /*
            div(
              'id', 'user_location_0_input_missing',
              'style', 'display:none;width:14em;color:red;'
            )->
              T_H( 'No match' )->
            div_end();
            */
        },
      ),
      'max_thermostats' => array(
        'format' => function( $column, $schema, $tail ) {
          $tail->input(
            'id', 'user_max_thermostats_0_input',
            'type', 'text',
            'autocomplete', 'off',
            'style', 'width:5em',
            'class', 'edit required'
          );
        },
      ),
      'langtag' => array(
        'format' => function( $column, $schema, $tail ) {
          $tail->
            add(
              get_language_select(
                'user_langtag_0_input',
                user()->langtag
              )
            );
        },
      ),
      'action' => array(
        'format' => function( $column, $schema, $tail ) {
          $tail->
            a(
              'id', 'user_add',
              'class', 'add',
              'href', 'javascript:void(0);',
              'onclick', 'add_user()'
            )->
              T_H( 'Add' )->
            a_end();
        },
      ),
    )
  );
  
  $data = dal()->user->report( $count, $table->page, $table->sort );
  //$total = dal()->group->report_total();
  
  
  //$table->render( $data, $tail, $total );
  $tail = $table->render(
    $data,  // data
    $tail,  // tail
    $total, // total
    null,   // highlight_spec
    false,  // top_nav
    false,  // bottom_nav
    true,   // sort_links
    false,  // search_row
    'submit_clear'  // clear_function
  );
  
}
