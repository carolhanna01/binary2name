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

//verify( is_administrator() || is_translator() );
verify( false );

if ( is_post() ) {
  
  try {
        
    $langtag = read_r( 'langtag' );
    $fallback = read_i( 'fallback', 'en' );
    $english_name = read_r( 'english_name' );
    $local_name = read_r( 'local_name' );
    $active = read_b( 'active' );

    throw_if(
      ! array_key_exists( $langtag, intl()->lang ),
      ERROR_WEB_INTL_EDIT_TRANSLATION_LANGTAGMISSING
    );
    
    throw_if(
      ! array_key_exists( $fallback, intl()->lang ),
      ERROR_WEB_INTL_EDIT_TRANSLATION_FALLBACKMISSING
    );
    
    throw_if(
      ! intl()->is_valid_fallback( $langtag, $fallback ),
      ERROR_WEB_INTL_EDIT_TRANSLATION_FALLBACKINVALID,
      'Fallback "%fallback%" is invalid.',
      'fallback', $fallback
    );

    dal()->begin();
    
    dal()->intl->save_language(
      $langtag,
      $fallback,
      $english_name,
      $local_name,
      $active
    );
    
    dal()->commit();
    
    redirect( '/intl-language-list.php' );

  }
  catch( Exception $ex ) {
    
    dal()->exception->log( $ex );
    
    $error = $ex->getMessage();
    
    dal()->rollback();
    
  }
}

render_head( 'Edit Translations' );

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

  $langtag = read_i( 'langtag', null );
  $hash = read_i( 'hash', null );

  $lang = read_a( intl()->lang, $langtag );

  redirect_if( $lang === null );
  redirect_if( $hash === null );

  $untranslated = dal()->intl->untranslated_count_for_message( $langtag, $hash );
  
  if ( $untranslated !== 0 ) {

    $add_link = new_link(
      '/intl-translation-add.php',
      H( 'add them' ),
      A( 'Click to add missing translations for this message/language.' )
    );
    
    $tail = $tail->
      p()->
        markup(
          H01n(
            'There are no untranslated messages for this content. ',
            'There is one untranslated message for this content. ',
            'There are %number% untranslated messages for this content. ',
            $untranslated
          )
        )->
        T_H( 'You can ' )->
        add( $add_link->to_anchor() )->
        T_H( '.' )->
      p_end();
    
  }

  $translations = dal()->intl->get_translations_for_message( $langtag, $hash );

  foreach ( $translations as $translation ) {

    $type = read_a( $translation, 'type' );
    $context = read_a( $translation, 'context' );
    
    $type_label = H( intl()->get_type_context_label( $type ), 'context', $context );

    $tail = $tail->
      form(
        'id', "tran_{$type}_{$context}_form",
        'method', 'POST'
      )->
        hidden( 'xsrf', session()->get_xsrf_token() )->
        fieldset()->
          legend()->
            markup( $type_label )->
          legend_end()->
          add( get_global_hidden_html() )->
          input( 'type', 'submit', 'value', A( 'Update' ) )->
        fieldset_end()->
      form_end();
    
  }
}
