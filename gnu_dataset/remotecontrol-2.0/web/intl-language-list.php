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

render_head( 'Language List' );

render_page();

render_foot();

html()->render();

function render_page() {

  $tail = html()->tail;

  //$tail->add( get_intl_menu() );
  
  $table = new GrcTableView(
    'lang_admin',
    array(
      'langtag' => array(
        'heading' => A( 'Langtag', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
      'fallback' => array(
        'heading' => A( 'Fallback', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
      'language' => array(
        'is-computed' => true,
        'heading' => A( 'Language', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          $english_name = read_a( $row, 'english_name' );
          $tail->
            markup( A( $english_name, CONTEXT_LANGUAGE ) );
        },        
      ),
      'english_name' => array(
        'default' => false,
        'heading' => A( 'English Name', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
      'local_name' => array(
        'default' => false,
        'heading' => A( 'Local Name', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
     'active' => array(
        'heading' => A( 'Active', CONTEXT_TABLE_HEADING ),
        'type' => BOOL_COLUMN
      ),
      'operations' => array(
        'is-computed' => true,
        'heading' => A( 'Operations', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          $langtag = read_a( $row, 'langtag' );
          $link = new_link( null, H( 'Edit' ) );
          $link->set_path( '/intl-language-edit.php' );
          $link->set( 'langtag', $langtag );
          if ( $row[ 'active' ] ) {
            $edit = new_link( null, H( 'Deactivate' ) );
            //$edit->clear( 'active' );
            $edit->set( 'active', 0 );
          }
          else {
            $edit = new_link( null, H( 'Activate' ) );
            $edit->set( 'active', 1 );
          }
          $edit->set( 'langtag', $langtag );
          $edit->set_path( '/intl-language-edit.php' );
          $tail->
            add( $link->to_anchor() )->
            T_H( ', ' )->
            add( $edit->to_anchor() );
          
        },
      )
    ),
    null,
    'langtag'
  );
  
  $data = dal()->intl->get_language_map( $count, $table->page, $table->sort );
  $total = dal()->intl->get_languages_total();

  $table->render( $data, $tail, $total );
  
}
