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

global $error, $default, $input;

verify( ! user()->deleted );

$id = read_i( 'id', user()->id );

if ( $id != user()->id ) {
  
  verify( is_admin() );
  
}

// specify default settings in here:
$default = array(
);

// input is read into here:
$input = array( 'id' => $id );

input( 'location_id' );
input( 'langtag' );

html( intl()->get_lang() );

$css = <<<EOF
  
EOF;

render_head( 'User Settings', $css );

render_page( $input, $error );

$script = <<<EOF
  
EOF;

render_foot( $script );

html()->render();

function render_page( &$input, &$error ) {

  $id = read_a( $input, 'id' );
  
  $tail = html()->tail;

  $user = dal()->user->get_by_id( $id );

  try {
  
    if ( is_post() ) {

      bom()->user->http_update( $input );

      redirect();
      
    }
  }
  catch ( Exception $ex ) {
  
    dal()->exception->log( $ex );
    
    $error[ 'MAIN' ] = $ex->getMessage();
    
  }
  
  $location_map = dal()->location->get_map();

  $tail->
    test( count( $error ) > 0 )->
      p( 'class', 'error' )->
        text( map_1st_value( $error ) )->
      p_end()->
    test_end()->
    form( 'method', 'POST' )->
      hidden( 'xsrf', session()->get_xsrf_token() )->
      hidden( 'id', read_i( 'id', user()->id ) )->
      fieldset( 'id', 'user_settings' )->
        legend()->
          T_TH( 'User Settings' )->
        legend_end()->
        table()->
          tbody()->
    
            tr()->
              th(
                'title', A( 'Username' )
              )->
                label( 'for', 'username' )->
                  T_TH( 'Username' )->
                  markup( ':' )->
                label_end()->
              th_end()->
              td(
                'title', A( 'Username' )
              )->
                span(
                  'id', 'username'
                )->
                  text( read_a( $user, 'username' ) )->
                span_end()->
              td_end()->
            tr_end()->
    
            tr()->
              th(
                'title', A( 'User ID' )
              )->
                label( 'for', 'user_id' )->
                  T_TH( 'User ID' )->
                  markup( ':' )->
                label_end()->
              th_end()->
              td(
                'title', A( 'User ID' )
              )->
                span(
                  'id', 'user_id'
                )->
                  text( read_d( $user, 'id', user()->id ) )->
                span_end()->
              td_end()->
            tr_end()->

            tr()->
              th(
                'title', A( 'Used Thermostats' )
              )->
                label( 'for', 'user_stats' )->
                  T_TH( 'Used Thermostats' )->
                  markup( ':' )->
                label_end()->
              th_end()->
              td(
                'title', A( 'Used Thermostats' )
              )->
                span(
                  'id', 'user_stats'
                )->
                  text(
                    dal()->user->get_used_thermostats( user()->id ) .
                    ' / ' .
                    user()->max_thermostats
                  )->
                span_end()->
              td_end()->
            tr_end()->
    
            decl(
              $location_filter,
              elem(
                'input',
                'id', 'location_select_filter',
                'onkeyup', 'process_filter(this);',
                'placeholder', A( ' filter' )
              )
            )->
    
            select_field(
              'Location',
              'location_select',
              'location_id',
              $location_map,
              $user,
              'Select the location of this thermostat.',
              null, // events
              null, // class
              null,
              null,
              $location_th,
              $location_td,
              $location_filter
            )->

            decl(
              $language_filter,
              elem(
                'input',
                'id', 'language_select_filter',
                'onkeyup', 'process_filter(this);',
                'placeholder', A( ' filter' )
              )
            )->
    
            select_field(
              'Language',
              'language_select',
              'langtag',
              intl()->get_lang_map(),
              $user,
              'Select the language you prefer.',
              null, // events
              null, // class
              null,
              null,
              $language_th,
              $language_td,
              $language_filter
            )->

    
            submit_field(
              'Submit',
              'submit_input',
              A( 'Submit' ),
              'Submit this thermostat.'
            )->

          tbody_end()->
        table_end()->
      fieldset_end()->
    
    
    form_end();
  
}
