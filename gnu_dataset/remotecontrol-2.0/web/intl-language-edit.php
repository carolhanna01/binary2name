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

if ( is_post() ) {
  
  try {
    
    $langtag = read_r( 'langtag' );
    $fallback = read_i( 'fallback', 'en' );
    $english_name = read_r( 'english_name' );
    $local_name = read_r( 'local_name' );
    $active = read_b( 'active' );

    /*
    throw_if(
      ! array_key_exists( $langtag, intl()->lang ),
      ERROR_WEB_INTL_EDIT_LANGUAGE_LANGTAGMISSING
    );
    
    throw_if(
      ! array_key_exists( $fallback, intl()->lang ),
      ERROR_WEB_INTL_EDIT_LANGUAGE_FALLBACKMISSING
    );
    
    throw_if(
      ! intl()->is_valid_fallback( $langtag, $fallback ),
      ERROR_WEB_INTL_EDIT_LANGUAGE_FALLBACKINVALID,
      "Fallback '%fallback%' is invalid for '%langtag%'.",
      'fallback', $fallback,
      'langtag', $langtag
    );
    */

    throw_if(
      ! array_key_exists( $langtag, intl()->lang ),
      'Langtag "%langtag%" missing.',
      'langtag', $langtag
    );
    
    throw_if(
      ! array_key_exists( $fallback, intl()->lang ),
      'Fallback "%fallback%" missing.',
      'fallback', $fallback
    );
    
    throw_if(
      ! intl()->is_valid_fallback( $langtag, $fallback ),
      'Fallback "%fallback%" is invalid for langtag "%langtag%".',
      'fallback', $fallback,
      'langtag', $langtag
    );
    
    dal()->intl->save_language(
      $langtag,
      $fallback,
      $english_name,
      $local_name,
      $active
    );
    
    redirect( '/intl-language-list.php' );

  }
  catch( Exception $ex ) {
    
    dal()->exception->log( $ex );
    
    $error = $ex->getMessage();
    
  }
}

render_head( 'Edit Language' );

render_page();

render_foot();

html()->render();

function render_page() {

  $tail = html()->tail;
  
  //$tail->add( get_intl_menu() );
  
  global $error;
  
  if ( $error ) {
    
    $tail->p( 'style', 'color:red' )->text( $error )->p_end();
    
  }
  
  render_form( $tail );

}

function render_form( $tail ) {
  
  $form = $tail = $tail->
    //form( 'id', 'lang_edit_form', 'method', 'POST' );
    form( 'id', 'lang_edit_form', 'method', 'POST' );
  
  $form->hidden( 'xsrf', session()->get_xsrf_token() );
  
  $form->add( get_global_hidden_html() );

  $langtag = read_i( 'langtag', null );
  
  $lang = read_a( intl()->lang, $langtag );

  redirect_if( $lang === null );
  
  $fallback = read_i( 'fallback', read_a( $lang, 'fallback' ) );
  $english_name = read_i( 'english_name', read_a( $lang, 'english_name' ) );
  $local_name = read_i( 'local_name', read_a( $lang, 'local_name' ) );
  $active = (bool)read_i( 'active', read_a( $lang, 'active' ) );
  
  $fallback_select = get_fallback_select( $langtag, $fallback );
  
  $tail->
    table()->
      tbody()->
        tr()->
          th()->
            T_H( 'Langtag:' )->
          th_end()->
          td()->
            input(
              'type', 'hidden',
              'name', 'langtag',
              'value', aenc( $langtag )
            )->
            text( $langtag )->
          td_end()->
        tr_end()->
        test( $langtag !== 'en' )->
          tr()->
            th()->
              T_H( 'Fallbck:' )->
            th_end()->
            td()->
              add( $fallback_select )->
            td_end()->
          tr_end()->
        test_end()->
        tr()->
          th()->
            T_H( 'English Name:' )->
          th_end()->
          td()->
            input(
              'id', 'english_name_input',
              'type', 'text',
              'name', 'english_name',
              'value', aenc( $english_name )
            )->
          td_end()->
        tr_end()->
        tr()->
          th()->
            T_H( 'Local Name:' )->
          th_end()->
          td()->
            input(
              'id', 'local_name_input',
              'type', 'text',
              'name', 'local_name',
              'value', aenc( $local_name )
            )->
          td_end()->
        tr_end()->
        tr()->
          th()->
            T_H( 'Active:' )->
          th_end()->
          td()->
            input(
              'id', 'active_input',
              'type', 'checkbox',
              'name', 'active',
              'value', '1',
              'checked', $active
            )->
            label(
              'for', 'active_input'
            )->
              T_H( 'Active' )->
            label_end()->
          td_end()->
        tr_end()->
      tbody_end()->
      tfoot()->
        tr()->
          td( 'colspan', 2 )->
            input(
              'type', 'submit',
              'value', A( 'Save' )
            )->
          td_end()->
        tr_end()->
      tfoot_end()->
    table_end();
  
  $form->form_end();
  
}

function get_fallback_select( $langtag, $selected ) {

  if ( $langtag === 'en' ) { return null; }
  
  $result = elem(
    'select',
    'id', 'fallback_select',
    'name', 'fallback'
  );
  
  foreach ( intl()->lang as $lang ) {
    
    $current_langtag = $lang[ 'langtag' ];
    
    if ( $current_langtag === $langtag ) { continue; }
    
    $result->
      option(
        'value', aenc( $current_langtag ),
        'selected', $current_langtag === $selected
      )->
        T_H( $lang[ 'english_name' ] );
    
  }
  
  return $result;
  
}
