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
  
    $langtag = intl()->format_langtag( $_GET );
    $langtag = intl()->canonicalize_langtag( $langtag );

    $fallback = read_r( 'fallback' );
    $english_name = read_r( 'english_name' );
    $local_name = read_r( 'local_name' );

    dal()->intl->add_language( $langtag, $fallback, $english_name, $local_name );

    $goto = read_g( 'goto', '/intl-language-list.php' );
    
    if ( strlen( $goto ) === 0 || $goto[ 0 ] !== '/' ) {
      
      $goto = '/intl-language-list.php';
      
    }
    
    redirect( $goto );
    
  }
  catch ( Exception $ex ) {

    dal()->exception->log( $ex );

    $error = henc( $ex->getMessage() );
    
  }
}

render_head( 'Add Language' );

render_page();

render_foot();

html()->render();

function render_page() {
  
  $tail = html()->tail;
  
  //$tail->add( get_intl_menu() );
  
  $file = &intl()->load_file();
  
  $langtag_comments = array(
    'language' => $file[ 'language_comments' ],
    'extlang' => $file[ 'extlang_comments' ],
    'script' => $file[ 'script_comments' ],
    'region' => $file[ 'region_comments' ],
    'variant' => $file[ 'variant_comments' ],
  );
  
  $script = "var langtag_comments = " .
    //json_encode( $langtag_comments, JSON_PRETTY_PRINT ) .
    json_encode( $langtag_comments ) .
    "\n\n";
  
  $script .= <<<EOF
    
  function handle_select_change( select ) {
   
    var id = select.id;
    
    var key = id.substr( 0, id.length - '_select'.length );
    
    var comments_id = key + '_comments';
    
    var comments_p = document.getElementById( comments_id );
    
    var comments = langtag_comments[ key ][ select.value ];
    
    if ( typeof comments === 'undefined' ) {

      comments = '';
    
    }

    comments_p.innerHTML = comments;
    
  }
    
  document.addEventListener( 'DOMContentLoaded', function() {
    var language_filter = document.getElementById( 'language_filter' );
    if ( language_filter !== null ) { process_filter( language_filter ); }
  });      
EOF;
  
  $tail->
    script( $script )->script_end();

  global $error;
  
  if ( $error ) {
    
    $tail->p( 'style', 'color:red' )->text( $error )->p_end();
    
  }
  
  render_form( $tail );

}

function render_form( $tail ) {
  
  $tail->
    form( 'id', 'langtag_form', 'method', 'GET' )->
      get( $form );
  
  $form->add( get_global_hidden_html( 'goto' ) );
  
  $prefix = '';
  
  $file = &intl()->load_file();

  $not_finished =
    render_language( $form, $file, $prefix, $language ) ||
    render_extlang( $form, $file, $prefix, $language, $extlang ) ||
    render_script( $form, $file, $prefix, $language, $extlang, $script ) ||
    render_region( $form, $file, $prefix, $language, $extlang, $script, $region ) ||
    render_variant( $form, $file, $prefix, $language, $extlang, $script, $region, $variant ) ||
    render_finish( $form, $prefix );

  if ( $not_finished ) {
    
    $form->input( 'type', 'submit', 'value', A( 'Next' ) );
    
  }
  
  $form->form_end();
  
}

function render_language( $form, &$file, &$prefix, &$language ) {
    
  return render_section(
    $form,
    $file,
    $prefix,
    $language,
    'languages',
    $list_prefix = false,
    'language',
    'Language',
    $list_na = false
  );
  
}

function render_extlang( $form, &$file, &$prefix, &$language, &$extlang ) {
  
  return render_section(
    $form,
    $file,
    $prefix,
    $extlang,
    'extlangs',
    $list_prefix = true,
    'extlang',
    'Extended Language Subtag',
    $list_na = true
  );
  
}

function render_script( $form, &$file, &$prefix, &$language, &$extlang, &$script ) {
  
  return render_section(
    $form,
    $file,
    $prefix,
    $script,
    'scripts',
    $list_prefix = false,
    'script',
    'Script',
    $list_na = true,
    read_a( $language, 'Suppress-Script' )
  );
  
}

function render_region( $form, &$file, &$prefix, &$language, &$extlang, &$script, &$region ) {
  
  return render_section(
    $form,
    $file,
    $prefix,
    $region,
    'regions',
    $list_prefix = false,
    'region',
    'Region',
    $list_na = true
  );
  
}

function render_variant(
  $form,
  &$file,
  &$prefix,
  &$language,
  &$extlang,
  &$script,
  &$region,
  &$variant
) {
  
  return render_section(
    $form,
    $file,
    $prefix,
    $variant,
    'variants',
    $list_prefix = true,
    'variant',
    'Variants',
    $list_na = true,
    $suppress_script = null,
    $index_list = true
  );
  
}

function render_finish( $form, $langtag ) {
  
  $existing = dal()->intl->get_language( $langtag );
  
  if ( $existing ) {
    
    $form->h2( 'Language Exists' )->h2_end()->
      p( 'This language is already in the database.' )->p_end();
    
    return false;
    
  }
  
  $form->method = 'POST';
  
  $form->hidden( 'xsrf', session()->get_xsrf_token() );
  
  $name = intl()->get_name( $langtag );
  
  $form->
    h2( 'Add Language' )->h2_end()->
    p()->
      T_H( 'Submit this form to add support for this language.' )->
    p_end()->
    table( 'class', 'settings' )->
      tbody()->
        tr()->
          th()->
            text( 'Langtag:' )->
          th_end()->
          td()->
            text( $langtag )->
          td_end()->
          td()->
            text( 'This is the language code that will be used to '.
              'identify this language.'
            )->
          td_end()->
        tr_end()->
        tr()->
          th()->
            label(
              'for', 'fallback_input'
            )->
              text( 'Fallback:' )->
            label_end()->
          th_end()->
          td()->
            input(
              'id', 'fallback_filter',
              'type', 'text',
              'onkeyup', 'process_filter(this)',
              'placeholder', A( ' filter' ),
              'style', 'width:6em;'
            )->
            add( get_intl_fallback_select() )->
          td_end()->
          td()->
            text( 'This is the language that will be used if translations are unavailable.' )->
          td_end()->
        tr_end()->
        tr()->
          th()->
            label(
              'for', 'english_name_input'
            )->
              text( 'English name:' )->
            label_end()->
          th_end()->
          td()->
            input(
              'id', 'english_name_input',
              'type', 'text',
              'name', 'english_name',
              'value', aenc( read_d( $_POST, 'english_name', $name ) )
            )->
          td_end()->
          td()->
            text( 'This is the name of this language in English.' )->
          td_end()->
        tr_end()->
        tr()->
          th()->
            label(
              'for', 'local_name_input'
            )->
              text( 'Local name:' )->
            label_end()->
          th_end()->
          td()->
            input(
              'id', 'local_name_input',
              'type', 'text',
              'name', 'local_name',
              'value', aenc( read_d( $_POST, 'local_name', $name ) )
            )->
            text( ' ' )->
            a(
              'href', 'meta-refresh.php?goto=' .
                rawurlencode(
                  'https://translate.google.com/#en/' . $langtag . '/' . $name
                ),
              'target', '_blank',
              'title', 'Click to translate the language name at Google Translate...'
            )->
              text( 'translate' )->
            a_end()->
          td_end()->
          td()->
            text( "This is the name of this language in it's own language." )->
          td_end()->
        tr_end()->
      tbody_end()->
      tfoot()->
        td( 'colspan', 4 )->
          input( 'type', 'submit', 'value', A( 'Create' ) )->
        td_end()->
      tfoot_end()->
    table_end();
    
  return false;
  
}

function render_section(
  $form,
  &$file,
  &$prefix,
  &$object,
  $list_name,
  $list_prefix,
  $query_term,
  $heading,
  $list_na,
  $suppress_script = null,
  $index_list = false
) {

  $object = null;
  if ( $list_prefix ) {
    $list = read_a( $file, $list_name, $prefix );
  }
  else {
    $list = read_a( $file, $list_name );
  }
  
  if ( $list === null ) {
    
    return false;
    
  }
  
  $code = read_a( $_GET, $query_term );

  if ( is_array( $code ) ) {
    foreach ( $code as $code_item ) {
      if ( $code_item !== null && $code_item !== '' ) {
        $prefix .= ( strlen( $prefix ) ? '-' : '' ) . $code_item;
      }
    }
  }
  else {
    if ( $code !== null && $code !== '' && $code !== $suppress_script ) {
      $prefix .= ( strlen( $prefix ) ? '-' : '' ) . $code;
    }    
  }
  
  $form->h2( $heading )->h2_end();
    
  if ( is_array( $code ) ) :
    foreach ( $code as $index => $code_item ) :
    
      $form->input(
        'type', 'hidden',
        'name', $query_term . '[]',
        'value', aenc( $code_item )
      );

      $object = read_a( $list, $code_item );
      $comments = read_a( $file, $query_term . '_comments', $code_item );
      $description = 'N/A';
      $descriptions = read_a( $file, $query_term . '_descriptions', $code_item );
      if ( $descriptions ) {
        $description = implode( ', ', $descriptions ) . ' (' . $code_item . ')';
      }
      
      if ( $index > 0 && $description === 'N/A' ) {
        
        // don't output last list item (e.g. for variants)
        continue;
        
      }
      
      $query = $_GET;
      $query_index = array_search( $code_item, $code );
      $query_array = array();
      for ( $i = 0; $i < $query_index; $i++ ) {
        $query_array[] = $code[ $i ];
      }
      $query[ $query_term ] = $query_array;
      $query = '?' . http_build_query( $query );

      $form->
        p()->
          markup( henc( $description ) )->
          markup( ' [' )->
          a(
            'href', aenc( $query )
          )->
            markup( 'change' )->
          a_end()->
          markup( ']' )->
        p_end()->
        p( 'class', 'langtag_comments' )->
          text( $comments )->
        p_end();
      
    endforeach;
  endif;
  
  if ( $index_list ) :
    
    $list = read_a( $file, $list_name, $prefix );
    
    if ( $list === null ) { return false; }
    
  endif;
  $tail = $form;
  if ( $code === null || $index_list ) :
    
    if ( $index_list && vector_nth_value( $code ) === '' ) { return false; }
    
    $tail = $tail->
      input(
        'id', $query_term . '_filter',
        'onkeyup', 'process_filter(this);',
        'placeholder', A( ' filter' )
      )->
      select(
        'id', $query_term . '_select',
        'name', $query_term . ( $index_list ? '[]' : '' ),
        'onchange', 'handle_select_change(this)',
        'onkeyup', 'handle_select_change(this)',
        'onkeydown', 'handle_select_change(this)'
      );
      
    if ( $list_na ) :
      
      $tail->option( 'value', '' )->markup( 'N/A' )->option_end();
    
    endif;
    foreach ( $list as &$item ) :
      foreach ( $item[ 'Descriptions' ] as $description ) :
        $item_code = $item[ 'Subtag' ];
        $tail->
          option(
            'value', aenc( $item_code ),
            'selected', ( $suppress_script === $item_code ? true : false )
          )->
            text( $description )->
            T_H( ' (' )->
            text( $item_code )->
            T_H( ')' )->
          option_end();
      endforeach;
    endforeach;
    
    $tail->
      select_end()->
      span(
        'id', $query_term . '_missing',
        'style', 'display:none'
      )->
        markup( 'No match' )->
      span_end()->
      p( 'id', $query_term . '_comments', 'class', 'langtag_comments' )->p_end();
    return true;
  endif;
  if ( is_array( $code ) ) { return true; }
  $tail->
    input(
      'type', 'hidden',
      'name', $query_term,
      'value', aenc( $code )
    );
  $object = read_a( $list, $code );  
  $comments = read_a( $file, $query_term . '_comments', $code );
  $description = 'N/A';
  $descriptions = read_a( $file, $query_term . '_descriptions', $code );
  if ( $descriptions ) {
    $description = implode( ', ', $descriptions ) . ' (' . $code . ')';
  }
  $tail = $tail->
    p( henc( $description ) );
  $query = $_GET;
  unset( $query[ $query_term ] );
  $query = '?' . http_build_query( $query );
  $tail->
      markup( ' [' )->
      a(
        'href', aenc( $query )
      )->
        markup( 'change' )->
      a_end()->
      markup( ']' )->
    p_end()->
    p( 'class', 'langtag_comments' )->
      text( $comments )->
    p_end();
  
  return false;  
}
