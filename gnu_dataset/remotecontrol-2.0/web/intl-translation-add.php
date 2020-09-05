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

if ( is_post() ) {
  
  try {
    
    $langtag = read_r( 'langtag' );
    $hash = read_r( 'hash' );
    $inherit = read_b( 'inherit' );
    $type_contexts = read_r( 'type_context' );
    $text = read_r( 'text' );
    
    if ( $inherit ) { $text = ''; }

    dal()->begin();
    
    foreach ( $type_contexts as $type_context ) {
      
      $parts = explode( '_', $type_context, 2 );
      
      $type = $parts[ 0 ];
      $context = $parts[ 1 ];
      
      dal()->intl->register_translation( $langtag, $type, $context, $hash, $text );
      
    }

    dal()->commit();

    $remaining = dal()->intl->untranslated_count_for_message( $langtag, $hash );
    
    if ( $remaining === 0 ) {

      $next = dal()->intl->next_translation( $langtag );
      
      if ( $next ) {
        
        $next_link = new_link();
        $next_link->set( 'langtag', $langtag );
        $next_link->set( 'hash', $next );

        redirect( $next_link->to_string() );
        
      }
      
      redirect( '/intl-message-list.php' );

    }
      
    $link = new_link();
    $link->set( 'langtag', $langtag );
    $link->set( 'hash', $hash );

    redirect( $link->to_string() );
      
  }
  catch ( Exception $ex ) {

    dal()->exception->log( $ex );
    
    $error = henc( $ex->getMessage() );

    dal()->rollback();
    
  }
}

render_head(
  'Add Translation',
  'table { width: 80%; } table th:first-child { width: 70%; } .hidden { display:none; }'
);

$script = <<<EOF
function toggle_inherit( checkbox ) {
  //alert( checkbox.checked );
  var textarea = document.getElementById( 'text_textarea' );
  if ( checkbox.checked ) {
    textarea.setAttribute( 'class', 'hidden' );
  }
  else {
    textarea.setAttribute( 'class', '' );
  }
}  
EOF;

render_page();

render_foot( $script );

html()->render();

function render_page() {
  
  $tail = html()->tail;

  //$tail->add( get_intl_menu() );

  global $error;
  
  if ( $error ) {
    
    $tail->p( 'style', 'color:red' )->markup( $error )->p_end();
    
  }

  $langtag = read_a( $_GET, 'langtag' );
  
  redirect_if( $langtag === null );
  
  $lang = read_a( intl()->lang, $langtag );
  
  redirect_if( $lang === null );
  
  $hash = read_a( $_GET, 'hash' );
  
  if ( $hash === null ) {
    
    $hash = dal()->intl->next_translation( $langtag );
    
  }
  
  redirect_if( ! is_valid_md5( $hash ) );
  
  $msg_list = dal()->intl->messages_for_hash( $hash );

  redirect_if( count( $msg_list ) === 0 );

  $content = $msg_list[ 0 ][ 'content' ];

  $form = read_a( $_GET, 'form' );
  
  switch( $form ) {
    
    case 'preview' : 
    
      return render_preview( $tail, $langtag, $lang, $hash, $msg_list, $content  );
    
    default : 

      return render_form( $tail, $langtag, $lang, $hash, $msg_list, $content  );

  }
}

function render_form( $tail, $langtag, &$lang, $hash, &$msg_list, &$message ) {
    
  $type_context_spec = read_a( $_GET, 'type_context' );
  
  if ( $type_context_spec === null ) {
    
    $type_context_spec = array();

    foreach ( $msg_list as &$msg ) {

      $type = $msg[ 'type' ];
      $context = $msg[ 'context' ];
      
      if ( dal()->intl->translation_exists( $langtag, $type, $context, $hash ) ) {
        
        continue;
        
      }
      
      $type_context_spec[] = $type . '_' . $context;
      
    }  
  }

  if ( count( $type_context_spec ) === 0 ) {
    
    $edit_link = new_link(
      '/intl-translation-edit.php',
      H( 'edit' ),
      A( 'Click to edit translations for this message/language.' )
    );
    
    $edit_link->set( 'langtag', $langtag );
    $edit_link->set( 'hash', $hash );
    
    $tail->
      p()->
        T_H( 'Translations already exist. ' )->
        T_H( 'You can ' )->
        add( $edit_link->to_anchor() )->
        T_H( ' them.' )->
      p_end();
    
    return;
    
  }
  
  $untranslated = dal()->intl->untranslated_count_for_language( $langtag );
  
  $tail = $tail->
    h2()->
      T_H(
        'You are translating to %language%',
        'language', H(
          read_a( intl()->lang, $langtag, 'english_name' ),
          CONTEXT_LANGUAGE
        )
      )->
    h2_end()->
    p()->
      markup(
        H01n(
          'There are no more messages to translate.',
          'There is one more message to translate.',
          'There are %number% more messages to translate.',
          $untranslated
        )
      )->
    p_end();
  
  $tail = $tail->
    form( 'id', 'tran_form', 'method', 'GET' );

  //$tail->hidden( 'xsrf', session()->get_xsrf_token() );
  
  $tail->add( get_query_hidden_html( 'type_context', 'text', 'form' ) );
  
  foreach ( $msg_list as &$msg ) {
    
    $type = $msg[ 'type' ];
    $context = $msg[ 'context' ];
    $context_label = intl()->get_context_label( $context );
    $seq = $msg[ 'seq' ];

    $type_context = $type . '_' . $context;
    
    if ( ! in_array( $type_context, $type_context_spec ) ) { continue; }
    
    $type_label = H( intl()->get_type_context_label( $type ), 'context', $context_label );
    
    $tail->
      h2( $type_label )->
      h2_end();

    $tail->
      p()->
        input(
        'id', $type_context . '_checkbox',
        'type', 'checkbox',
        'name', 'type_context[]',
        'value', aenc( $type_context ),
        'checked', in_array( $type_context, $type_context_spec )
      )->
        label( 'for', $type_context . '_checkbox' )->
          markup( $type_label )->
        label_end()->
      p_end();
    
    $msg_contexts = dal()->intl->message_context(
      $type,
      $context,
      $seq
    );

    $table = new GrcTableView(
      "ctx_$seq",
      array(
        'content' => array(
          'heading' => A( 'Content', CONTEXT_TABLE_HEADING ),
          'type' => STRING_COLUMN,
          'format' => function( &$row, $column, $schema, $tail ) {
            $result = $row[ 'content' ];
            if ( $row[ 'type' ] === 'text' ) {
              $result = henc( $result );
            }
            $tail->markup( truncate_html( $result, 90 ) );
          },
          'total_units' => A( ' characters', CONTEXT_UNITS ),
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
       'seq' => array(
          'default' => false,
          'heading' => A( 'Sequence', CONTEXT_TABLE_HEADING ),
          'type' => INT_COLUMN
        ),
       'words' => array(
          'default' => false,
          'heading' => A( 'Words', CONTEXT_TABLE_HEADING ),
          'type' => INT_COLUMN
        ),
        'index' => array(
          'default' => false,
          'heading' => A( 'Index', CONTEXT_TABLE_HEADING ),
          'type' => INT_COLUMN
        ),
      ),
      null,
      'index'
    );

    $total = null;
    
    $table->render(
      $msg_contexts,
      $tail,
      $total,
      $highlight_spec = array( 'seq' => $seq ),
      $top_nav = false,
      $bottom_nav = false,
      $sort_links = false
    );
    
  }
  
  $tail->h2()->T_H( 'English Message' )->h2_end();

  /*
  $tail->textarea(
    'style', 'width:80%;height:5em;'
  )->
    text( $message )->
  textarea_end();
  */
  
  $tail->text( $message );
  
  $fallback = read_a( intl()->lang, $langtag, 'fallback' );
  $fallback_name = H( read_a( intl()->lang, $fallback, 'english_name' ) );

  if ( $langtag !== 'en' ) {
    
    $tail->
      h2()->
        T_H(
          'Translation inherited from %language%',
          'language', $fallback_name
        )->
      h2_end();

    $tail->text( intl()->translate( $type, $message, $context, $fallback ) );

  }
  
  $tail->h2()->T_H( $lang[ 'english_name' ] . ' Translation' )->h2_end();

  $inherit = ( read_a( $_GET, 'inherit' ) === '1' );
  
  $tail->
    p()->
      input(
        'id', 'inherit_checkbox',
        'type', 'checkbox',
        'name', 'inherit',
        'value', '1',
        'checked', $inherit,
        'onchange', 'toggle_inherit(this)'
      )->
      label(
        'for', 'inherit_checkbox'
      )->
        T_H(
          'Inherit from %language% translation',
          'language',
          $fallback_name
        )->
      label_end()->
    p_end();
  
  $class = false;
  
  if ( $inherit ) { $class = 'hidden'; }
  
  $text = read_a( $_GET, 'text' );
  
  $tail->textarea(
    'id', 'text_textarea',
    'name', 'text',
    'style', 'width:80%;height:5em;',
    'class', $class
  )->
    text( $text )->
  textarea_end();
  
  $tail->
    p()->
      input( 'type', 'hidden', 'name', 'form', 'value', 'preview' )->
      input( 'type', 'submit', 'value', A( 'Preview' ) )->
    p_end();
  
  $tail->form_end();
  
}

function render_preview( $tail, $langtag, &$lang, $hash, &$msg_list, &$message ) {
  
  $type_context_spec = read_a( $_GET, 'type_context' );
  
  if( ! is_array( $type_context_spec ) ) {
    
    $retry = get_link( H( 'Try again' ) );
    $retry->set( 'form', 'add' );
    
    $tail->
      p()->
        T_H( 'You must specify at least one type/context setting. ' )->
        add( $retry->to_anchor() )->
        T_H( '.' )->
      p_end();
    
    return;
    
  }

  $form = $tail = $tail->
    form( 'id', 'tran_form', 'method', 'GET' );

  //$tail->hidden( 'xsrf', session()->get_xsrf_token() );
  
  $form->add( get_query_hidden_html() );
  
  $types = array();
  $contexts = array();
  
  foreach ( $type_context_spec as $type_context ) {
    
    $parts = explode( '_', $type_context, 2 );
    
    $type = read_a( $parts, 0 );
    $context = read_a( $parts, 1 );
    
    redirect_if( ! is_valid_intl_type( $type ) );
    redirect_if( ! is_valid_intl_context( $context ) );

    $types[ $type ] = true;
    $contexts[ $context ] = true;
    
  }
  
  $tail->h2()->T_H( 'English Message' )->h2_end();

  if ( array_key_exists( 'text', $types ) ) {
    $tail->p()->text( $message )->p_end();
  }
  else {
    $tail->p()->markup( $message )->p_end();
  }

  $tail->h2()->T_H( $lang[ 'english_name' ] . ' Translation' )->h2_end();

  $inherit = ( read_a( $_GET, 'inherit' ) === '1' );
  
  $text = $inherit ? $message : read_a( $_GET, 'text' );
  
  redirect_if( $text === null );

  $error = false;

  if ( array_key_exists( 'html', $types ) ) {
    $valid = is_valid_html( $text );
    if ( ! $valid ) { $error = true; }
    $tail->h3()->T_H( intl()->get_type_label( 'html' ) )->h3_end();
    if ( $valid ) {
      $tail->
        p()->
          markup( $text )->
        p_end();
    }
    else {
      $tail->
        p()->
          span( 'class', 'error' )->
            T_H( 'Invalid: ' )->
          span_end()->
          text( $text )->
        p_end();
    }
  }
  if ( array_key_exists( 'attr', $types ) ) {
    $valid = is_valid_attr( $text );
    if ( ! $valid ) { $error = true; }
    $tail->h3( intl()->get_type_label( 'attr' ) )->h3_end();
    if ( $valid ) {
      $tail->
        p()->
          markup( $text )->
        p_end();
    }
    else {
      $tail->
        p()->
          span( 'class', 'error' )->
            markup( $valid ? '' : H( 'Invalid: ' ) )->
          span_end()->
          text( $text )->
        p_end();
      
    }
  }
  if ( array_key_exists( 'text', $types ) ) {
    $valid = is_valid_text( $text );
    $tail->h3( intl()->get_type_label( 'text' ) )->h3_end();
    if ( ! $valid ) { $error = true; }
    if ( $valid ) {
      $tail->
        p()->
          text( $text )->
        p_end();
    }
    else {
      $tail->
        p()->
          span( 'class', 'error' )->
            markup( $valid ? '' : H( 'Invalid: ' ) )->
          span_end()->
          text( $text )->
        p_end();
      
    }
  }
  
  if ( $error ) {
    
    $tail->
      input( 'type', 'hidden', 'name', 'form', 'value', 'edit' )->
      input(
        'type', 'submit',
        'value', 'Edit'
      );

  }
  else {

    $form->method = 'POST';
    
    $form->hidden( 'xsrf', session()->get_xsrf_token() );
    
    $back = get_link( A( 'Edit' ) );
    $back->set( 'form', 'edit' );
    
    $tail->
      input( 'type', 'hidden', 'name', 'form', 'value', 'submit' )->
      input(
        'type', 'submit',
        'value', 'Add'
      )->
      markup( ' ' )->
      add( $back->to_anchor() );
    
  }
  
  $tail->form_end();
    
}

function filter_message( $key, &$msg_list, &$result ) {

  $temp = array();
  
  foreach ( $msg_list as $msg ) {
    
    $temp[ $msg[ $key ] ] = true;
    
  }
  
  $result = array_keys( $temp );
  
}

function get_message_types( &$msg_list, &$result ) {
  
  filter_message( 'type', $msg_list, $result );
  
}

function get_message_contexts( &$msg_list, &$result ) {

  filter_message( 'context', $msg_list, $result );
  
}
