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

render_head( 'Message Context List' );

render_page();

render_foot();

html()->render();

function render_page() {

  $tail = html()->tail;

  $table = new GrcTableView(
    'msg_admin',
    array(
      'context' => array(
        'heading' => A( 'Code', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
      'name' => array(
        'is-computed' => true,
        'heading' => A( 'Context', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $null_html = '' ) {
          return intl()->get_context_label( $row[ 'context' ] );
        },
      ),
     'count' => array(
        'heading' => A( 'Message Count', CONTEXT_TABLE_HEADING ),
        'type' => INT_COLUMN
      ),
      'types' => array(
        'heading' => A( 'Type', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
     'words' => array(
        'heading' => A( 'Words', CONTEXT_TABLE_HEADING ),
        'type' => INT_COLUMN
      ),
    ),
    null,
    null
  );
  
  $data = dal()->intl->get_contexts( $count, $table->page, $table->sort );
  $total = dal()->intl->get_contexts_total();
  
  $table->render( $data, $tail, $total );
  
}
