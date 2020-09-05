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

verify( is_administrator() || is_translator() );

html( intl()->get_lang() );

render_head( 'Message List' );

render_page();

render_foot();

html()->render();

function render_page() {

  $tail = html()->tail;

  //$tail->add( get_intl_menu() );
  
  $table = new GrcTableView(
    'msg_admin',
    array(
      'content' => array(
        'heading' => A( 'Content', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          $result = $row[ 'content' ];
          if ( $row[ 'type' ] === 'text' ) {
            $result = henc( $result );
          }
          $tail->title = aenc( $row[ 'content' ] );
          $tail->
            markup( truncate_html( $result ) );
        },
        'total_units' => A( ' characters', CONTEXT_UNITS ),
      ),
      'type' => array(
        'heading' => A( 'Types', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
      'context' => array(
        'default' => false,
        'heading' => A( 'Context Codes', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
      'context_name' => array(
        'is-computed' => true,
        'heading' => A( 'Contexts', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $null_html = '' ) {
          $contexts = explode( ', ', $row[ 'context' ] );
          $context_names = array();
          foreach ( $contexts as $context ) {
            $context_names[] = intl()->get_context_label( $context );
          }
          return implode( ', ', $context_names );
        },
      ),
      'hash' => array(
        'default' => false,
        'heading' => A( 'Hash', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
     'seq' => array(
        'heading' => A( 'Sequence', CONTEXT_TABLE_HEADING ),
        'type' => INT_COLUMN
      ),
     'words' => array(
        //'sql' => 'max( m.words )',
        'heading' => A( 'Words', CONTEXT_TABLE_HEADING ),
        'type' => INT_COLUMN
      ),
      'translations' => array(
        'heading' => A( 'Translations', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN,
        'no-sort' => true,
        'is-computed' => true,
        'format' => function( &$row, $column, $schema, $tail ) {
          $langtag_spec = read_a( $row, 'translate' );
          $edit = read_a( $row, 'edit' );
          if ( $edit ) {
            $edit_list = explode( ',', $edit );
          }
          else {
            $edit_list = array();
          }          

          $langtag_list = explode( ',', $langtag_spec );
          
          foreach ( $langtag_list as $index => $langtag ) {
            if ( in_array( $langtag, $edit_list ) ) {
              unset( $langtag_list[ $index ] );
            }
          }
          
          if ( count( $langtag_list ) ) {
            $tail = $tail->
              ul( 'class', 'menu' )->
                li()->
                  span( 'class', 'underline', 'style', 'margin:0px;padding:0px;' )->
                    T_H( 'Add' )->
                  span_end()->
                  ul();
            foreach ( $langtag_list as $langtag ) {
              if ( in_array( $langtag, $edit_list ) ) { continue; }
              $link = new_link(
                null,
                H(
                  henc(
                    read_d( intl()->lang, $langtag, 'english_name', '**missing**' )
                  ),
                  CONTEXT_LANGUAGE
                )
              );
              $link->set_path( '/intl-translation-add.php' );
              $link->set( 'langtag', $langtag );
              $link->set( 'hash', $row[ 'hash' ] );
              $tail->
                li()->add( $link->to_anchor() )->li_end();
            }
            $tail = $tail->
                  ul_end()->
                li_end()->
              ul_end();
          }
          
          if ( count( $edit_list ) ) {

            if ( count( $langtag_list ) ) {
              
              $tail->
                span( 'style', 'vertical-align:top' )->
                  markup( ', ' )->
                span_end();
              
            }

            $tail = $tail->
              ul( 'class', 'menu', 'style', 'margin:0px;padding:0px;' )->
                li()->
                  span( 'class', 'underline' )->
                    T_H( 'Edit' )->
                  span_end()->
                  ul();
            foreach ( $edit_list as $langtag ) {
              $link = new_link(
                null,
                H(
                  henc( intl()->lang[ $langtag ][ 'english_name' ] ),
                  CONTEXT_LANGUAGE
                )
              );
              $link->set_path( '/intl-translation-edit.php' );
              $link->set( 'langtag', $langtag );
              $link->set( 'hash', $row[ 'hash' ] );
              $tail->
                li()->add( $link->to_anchor() )->li_end();
            }
            $tail = $tail->
                  ul_end()->
                li_end()->
              ul_end();
            
            
          }
          
        },
      ),
      'index' => array(
        'default' => false,
        'heading' => A( 'Index', CONTEXT_TABLE_HEADING ),
        'type' => INT_COLUMN
      ),
    ),
    null,
    'hash'
  );
  
  $search = $table->search();
  
  $data = dal()->intl->message_report(
    $search,
    $count,
    $table->page,
    $table->sort
  );
  
  $total = dal()->intl->message_report_total();
  
  $tail = $tail->form( 'method', 'GET' );

  $table->render(
    $data,
    $tail,
    $total,
    null, // highlight_spec
    true, // top_nav
    true, // bottom_nav
    true, // sort_links
    true  // search_row
  );

  $tail = $tail->form_end();
  
}
