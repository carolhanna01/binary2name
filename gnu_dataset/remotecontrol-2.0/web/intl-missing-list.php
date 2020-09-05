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

render_head( 'Missing Langtag List' );

render_page();

render_foot();

html()->render();

function render_page() {

  $tail = html()->tail;

  //$tail->add( get_intl_menu() );
  
  $tail = $tail->
    p()->
      T_H( 'Missing langtags are valid langtags which have been requested ' .
        'by web browsers for which no supported language was detected.' )->
    p_end();
  
  $table = new GrcTableView(
    'missing_langtag',
    array(
      'langtag' => array(
        'heading' => A( 'Langtag', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
     'count' => array(
        'heading' => A( 'Count', CONTEXT_TABLE_HEADING ),
        'type' => INT_COLUMN
      ),
      'canonical' => array(
        'heading' => A( 'Canonical Langtag', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'is-computed' => true,
        'format' => function( &$row, $column, $schema, $null_html = '' ) {
          $value = $row[ 'langtag' ];
          if ( is_null( $value ) ) {
            $value = read_a( $schema, $column, 'null' );
          }
          if ( is_null( $value ) ) {
            return $null_html;
          }
          if ( intl()->parse_langtag( $value, $spec, $error ) ) {
            $langtag = intl()->format_langtag( $spec );
            $link = get_link( 'add' );
            $link->set_path( '/intl-language-add.php' );
            $query = $link->query;
            $query->set( 'language', $spec[ 'language' ] );
            $query->set( 'extlang', $spec[ 'extlang' ] );
            $query->set( 'script', $spec[ 'script' ] );
            $query->set( 'region', $spec[ 'region' ] );
            $query->set( 'goto', '/intl-list-missing.php' );
            $query->clear( 'variant[]' );
            foreach ( $spec[ 'variant' ] as $variant ) {
              $query->add( 'variant[]', $variant );
            }
            $link = $link->to_html();
            return henc( $langtag ) . ' (<a href="' . $link .
              '" title="Add support for this language">add</a>)';
          }
          return henc( $error );
        },        
      ),
    )
  );
  
  $data = dal()->intl->get_langtag_missing( $count, $table->page, $table->sort );
  $total = dal()->intl->get_langtag_missing_total();

  $table->render( $data, $tail, $total );
  
}
