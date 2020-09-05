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

function get_page_styles() {
  
  $result = GrcHtmlComposite::Create();
  
  $result->
    link(
      'rel', 'stylesheet',
      'type', 'text/css',
      'href', WEB_ROOT . '/styles/menu.css'
    )->
    link(
      'rel', 'stylesheet',
      'type', 'text/css',
      'href', WEB_ROOT . '/styles/global.css'
    );
  
  return $result;
  
}

function get_page_scripts() {

  $result = GrcHtmlComposite::Create();
  
  $result->
    script( 'src', WEB_ROOT . '/scripts/lib/global.js' )->script_end();

  $parts = explode( '-', url()->get_filename() );
  $done = array();
  
  for ( $i = 0; $i < count( $parts ); $i++ ) {
    
    $done[] = $parts[ $i ];
    
    $file = implode( '-', $done ) . '.js';
    $url = WEB_ROOT . '/scripts/inc/' . $file;
    $path = __DIR__ . '/../../web/scripts/inc/' . $file;
    
    if ( ! file_exists( $path ) ) { continue; }
    
    $result->
      script(
        'src', $url
      )->
      script_end();
    
  }
  
  return $result;
  
}

function render_head( $title = 'Home', $css = null ) {

  //dump_html( 'before head' );
  
  $title = A( $title, CONTEXT_PAGE_TITLE );
  
  $node = html()->tail;
  
  $xsrf_token = session()->get_xsrf_token();
  
  $group_map = json_encode( dal()->group->get_map() );

  $temperature_scale = user()->temperature_scale;
  
  $global_script = <<<EOF
  
  document.xsrf_token = '$xsrf_token';
    
  var group_map = $group_map;

  window.user_settings = {
    'temperature_scale' : '$temperature_scale'
  };
    
EOF;

  /*
  $max_thermostats = dal()->get_max_thermostats_for_username(
    $_SERVER[ 'PHP_AUTH_USER' ]
  );
  */
  
  $max_thermostats = user()->max_thermostats;
    
  $node->
    head()->
      meta(
        'http-equiv', 'Content-Type',
        'content', 'text/html;charset=UTF-8'
      )->
      title()->markup( $title . ' - GNU Remote Control' )->title_end()->
      link(
        'rel', 'shortcut icon',
        'href', WEB_ROOT . '/images/GNURC.ico',
        'type', 'image/x-icon'
      )->
      link(
        'rel', 'icon',
        'href', WEB_ROOT . '/images/GNURC.ico',
        'type', 'image/ico'
      )->
      add( get_page_styles() )->
      style()->markup( $css )->style_end()->
      add( get_page_scripts() )->
      comment()->
      script( 'src', WEB_ROOT . '/scripts/master.js' )->script_end()->
      script( 'src', WEB_ROOT . '/scripts/ajax.js' )->script_end()->
      script( 'src', WEB_ROOT . '/scripts/form.js' )->script_end()->
      script( 'var ajax = new Ajax();' )->script_end()->
      script( 'src', WEB_ROOT . '/scripts/jj5.js' )->script_end()->
      comment_end()->
      script( $global_script )->script_end()->
    
    head_end()->
    body()->
      div(
        'id', 'main-container'
      )->
        div()->
          a(
            'href', './'
          )->
            img(
              'id', 'banner_logo',
              'alt', A( 'GNU Remote Control' ),
              'src', WEB_ROOT . '/images/logo.png',
              'style', 'border: 2px solid #BC822E; width:940px; height:112px;'
            )->
          a_end()->
          img(
            'id', 'loadimage',
            'alt', A( 'Working...' ),
            'title', A( 'Working...' ),
            'src', WEB_ROOT . '/images/ajax-loader.gif'
          )->
        div_end()->
    
        div( 'shader' )->
          div(
            'style', 'width:940px;text-align:right;'
          )->
            span( 'id', 'welcomeback' )->
              T_H(
                'Welcome back, <b class="teal">%username%</b>',
                'username', henc( user()->username )
              )->
            span_end()->
          div_end()->
          h1( 'id', 'header' )->
            markup( $title )->
          h1_end()->
          div(
            'id', 'container',
            'class', 'maximum-width'
          )->
            div( 'inner' )->
              span( 'id', 'message' )->span_end()->
              test( false )->
                div( 'thermostatscontainer' )->
                  //add( get_timezones_combo_html() )->
                  //add( get_languages_combo_html() )->
                  comment()->
                  div()->
                    span( 'id', 'subscriptioninfo' )->
                      T_H(
                        'You are allowed a maximum of <b>%max%</b> thermostats.',
                        'max', number_format( $max_thermostats )
                      )->
                    span_end()->
                  div_end()->
                  comment_end()->
      /*
                  div()->
                    span( 'id', 'transactionlog' )->
                      a(
                        'href', WEB_ROOT . '/transactionhistory.php'
                      )->
                        markup( H( 'Transaction History' ) )->
                      a_end()->
                    span_end()->
                  div_end()->
      */
                  br()->
                div_end()->
              test_end()->
              //br( 'style', 'clear:both' )->
              add( get_menu() );


  //dump_html( 'after head' );
  
}
