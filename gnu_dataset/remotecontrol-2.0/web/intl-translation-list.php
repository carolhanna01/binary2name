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

//verify( is_administrator() );

html( intl()->get_lang() );

render_head( 'Language Translations' );

render_page();

render_foot();

html()->render();

function render_page() {

  $tail = html()->tail;

  //$tail->add( get_intl_menu() );
  
  $table = new GrcTableView(
    'tran_list',
    array(
      'langtag' => array(
        'heading' => A( 'Langtag', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
      'type' => array(
        'heading' => A( 'Type', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
      'context' => array(
        'heading' => A( 'Context', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
      'hash' => array(
        'default' => false,
        'heading' => A( 'Hash', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
      'content' => array(
        'heading' => A( 'Content', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          $result = $row[ 'content' ];
          if ( $row[ 'type' ] === 'text' ) {
            $result = henc( $result );
          }
          $tail->markup( truncate_html( $result ) );
        },
        'total_units' => A( ' characters', CONTEXT_UNITS ),
      ),
      'text' => array(
        'heading' => A( 'Translation', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $null_html = '' ) {
          $value = $row[ 'text' ];
          if ( is_null( $value ) ) {
            $value = read_a( $schema, $column, 'null' );
          }
          if ( is_null( $value ) ) {
            return $null_html;
          }
          if ( $row[ 'type' ] === 'text' ) {
            $result = henc( $value );
          }
          else {
            $result = $value;
          }
          return truncate_html( $result );
        },
        'total_units' => A( ' characters', CONTEXT_UNITS ),
      ),
      'seq' => array(
        'default' => false,
        'heading' => A( 'Sequence', CONTEXT_TABLE_HEADING ),
        'type' => INT_COLUMN,
      ),
      'words' => array(
        'default' => false,
        'heading' => A( 'Words', CONTEXT_TABLE_HEADING ),
        'type' => INT_COLUMN,
      )
    ),
    null, // tfoot_spec
    null  // index_column
  );
  
  $data = dal()->intl->get_translations( $count, $table->page, $table->sort );
  $total = dal()->intl->get_translations_total();
  
  $table->render( $data, $tail, $total );
  
}
