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

render_head( 'Group Administration' );

render_page();

render_foot();

html()->render();

function render_page() {

  $tail = html()->tail;
  
  $table = new GrcTableView(
    'group',
    array(
      'id' => array(
        'width' => '10em',
        'default' => false,
        'heading' => A( 'Group ID', CONTEXT_TABLE_HEADING ),
        'type' => INT_COLUMN,
      ),
      'name' => array(
        'width' => '20em',
        'heading' => A( 'Group Name', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'total_units' => A( ' characters', CONTEXT_UNITS ),
        'format' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $tail->
            span(
              'id', 'group_name_' . $id . '_span',
              'class', 'view'
            )->
              text( $row[ $column ] )->
            span_end()->
            input(
              'id', 'group_name_' . $id . '_input',
              'type', 'text',
              'value', $row[ $column ],
              'class', 'view',
              'autocomplete', 'off'
            );
        },
      ),
      'description' => array(
        'width' => '20em',
        'heading' => A( 'Description', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'total_units' => A( ' characters', CONTEXT_UNITS ),
        'format' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $tail->
            span(
              'id', 'group_description_' . $id . '_span',
              'class', 'view'
            )->
              text( $row[ $column ] )->
            span_end()->
            input(
              'id', 'group_description_' . $id . '_input',
              'type', 'text',
              'value', $row[ $column ],
              'class', 'view',
              'autocomplete', 'off'
            );
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
              'id', 'group_edit_' . $id,
              'class', 'edit',
              'href', 'javascript:void(0);',
              'onclick', 'edit_group( ' . $id . ' )'
            )->
              T_H( 'Edit' )->
            a_end()->
            nbsp()->
            a(
              'id', 'group_delete_' . $id,
              'class', 'delete',
              'href', 'javascript:void(0);',
              'onclick', 'delete_group( ' . $id . ' )'
            )->
              T_H( 'Delete' )->
            a_end()->
            a(
              'id', 'group_update_' . $id,
              'class', 'update',
              'href', 'javascript:void(0);',
              'onclick', 'update_group( ' . $id . ' )'
            )->
              T_H( 'Update' )->
            a_end()->
            nbsp()->
            a(
              'id', 'group_cancel_edit_' . $id,
              'class', 'cancel',
              'href', 'javascript:void(0);',
              'onclick', 'cancel_edit_group( ' . $id . ' )'
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
              'id', 'group_name_0_input',
              'type', 'text',
              'class', 'edit required'
            );
        },
      ),
      'description' => array(
        'format' => function( $column, $schema, $tail ) {
          $tail->
            input(
              'id', 'group_description_0_input',
              'type', 'text',
              'class', 'edit'
            );
        },
      ),
      'action' => array(
        'format' => function( $column, $schema, $tail ) {
          $tail->
            a(
              'id', 'group_add',
              'class', 'add',
              'href', 'javascript:void(0);',
              'onclick', 'add_group()'
            )->
              T_H( 'Add' )->
            a_end();
        },
      ),
    ),
    'id',
    'name'
  );
  
  $data = dal()->group->report(
    $count,
    $table->page,
    $table->sort
  );
  //$total = dal()->group->report_total();
  
  if ( dal()->group->count_all() == 0 ) {

    $tail->p()->T_H( "You must add at least one group." )->p_end();

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
    
  //$tail->tfoot()->tr()->td()->T( 'testing...' );
  
  return;
  
  
  
  
  
  
  
  
  //$tail->add( get_menu() );

  $users = bom()->user->report();
  $groups = bom()->get_groups();

  $user_count = format_number( count( $users ) );
  $therm_count = format_number( sum( $users, 'max_thermostats' ) );
  
  $group_count = format_number( count( $groups ) );
  
  $tail->
    div()->
      span( 'class', 'requirednote' )->
        T_H( 'Highlighted fields are required.' )->
      span_end()->
    div_end()->
    div( 'id', 'userssection', 'class', 'section' )->
      div( 'class', 'sectionheader' )->
        span( 'class', 'sectionheader' )->
          T_H( 'User Accounts' )->
        span_end()->
      div_end()->
      div( 'class', 'sectioninner' )->
        table(
          'id', 'profileadmin',
          'class', 'settings'
        )->
          tr()->
            td( 'colspan', 2 )->
              T_H(
                'Total User Accounts: '
              )->
              span(
                'id', 'totalusercount'
              )->
                text( $user_count )->
              span_end()->
            td_end()->
            td( 'colspan', 3 )->
              T_H(
                'Total Thermostats: '
              )->
              span(
                'id', 'totalthermcount'
              )->
                text( $therm_count )->
              span_end()->
            td_end()->
          tr_end()->
          tr( 'class', 'header' )->
            th()->
              markup( '&nbsp;' )->
            th_end()->
            th()->
              T_H( 'Username' )->
            th_end()->
            th()->
              T_H( 'Maximum # Thermostats' )->
            th_end()->
            th()->
              T_H( 'Language' )->
            th_end()->
          tr_end()->
          add( get_user_rows( $users ) )->
        table_end()->
      div_end()->
    div_end()->
    div( 'id', 'groupssection', 'class', 'section' )->
      div( 'class', 'sectionheader' )->
        span( 'class', 'sectionheader' )->
          T_H( 'Groups' )->
        span_end()->
      div_end()->
      div( 'class', 'sectioninner' )->
        table(
          'id', 'groupsadmin',
          'class', 'settings'
        )->
          tbody()->
            tr()->
              td( 'colspan', 4 )->
                T_H(
                  'Total Groups: '
                )->
                span(
                  'id', 'totalgroupcount'
                )->
                  text( $group_count )->
                span_end()->
              td_end()->
            tr_end()->
            tr( 'class', 'header' )->
              th()->th_end()->
              th()->T_H( 'Group Name' )->th_end()->
              th()->T_H( 'Description' )->th_end()->
              th()->th_end()->
            tr_end()->
          tbody_end()->
        table_end()->
      div_end()->
    div_end();
  
}

function get_user_rows( &$users ) {

  $result = GrcHtmlComposite::Create();
  
  foreach ( $users as &$user ) {

    $result->
      tr()->
        th()->
          text( $user[ 'username' ] )->
        th_end()->
      tr_end();
    
  } 
  
  return $result;
  
}
