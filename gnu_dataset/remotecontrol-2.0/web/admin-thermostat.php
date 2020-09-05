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

verify( is_admin() );

$id = read_i( 'id', 'new' );

html( intl()->get_lang() );

$css = <<<EOF
  
  #user_select,
  #location_select,
  #group_select { width: 10em; }
  
EOF;

if ( $id === 'new' ) {
  
  if (
    dal()->thermostat->count_for_user( user()->id ) >=
    user()->max_thermostats
  ) {
    
    throw Error(
      'You have reached your thermostat limit of %limit%.',
      'limit', user()->max_thermostats
    );
    
  }
  
  render_head( 'New Thermostat', $css );
  
}
else {
  
  render_head( 'Edit Thermostat', $css );
  
}

render_page( $id, $error );

$script = <<<EOF
  
  check_connection();
  
EOF;

render_foot( $script );

html()->render();

function render_page( $id, &$error ) {
  
  $tail = html()->tail;

  if ( $id === 'new' ) {

    $t = array(
      'user_id' => user()->id,
      'location_id' => user()->location_id,
      'next' => user()->next,
      'user' => 'admin'
    );
    
  }
  else {

    try {
    
      $t = dal()->thermostat->get_by_id( $id );

    }
    catch ( Exception $ex ) {
      
      $tail->
        p()->
          T_H( 'The thermostat you\'ve requested no longer exists.' )->
        p_end();
      
      return;
      
    }
  }

  $data = bom()->thermostat->http_read( $id, $t );
  
  try {
  
    if ( is_post() ) {

      bom()->thermostat->validate( $data );

      //var_dump( $error ); die;
      
      if ( count( $error ) === 0 ) {

        if ( $id === 'new' ) {

          bom()->thermostat->http_insert( $data );

        }
        else {

          bom()->thermostat->http_update( $data );

        }

        switch ( read_a( $data, 'next' ) ) {

        case 'edit' :

          redirect( '/admin-thermostat.php', array( 'id' => $data[ 'id' ] ) );

        case 'new' :

          redirect( '/admin-thermostat.php' );

        default :

          redirect( '/home.php' );

        }
      }
    }
  }
  catch ( Exception $ex ) {
  
    dal()->exception->log( $ex );
    
    $error[ 'MAIN' ] = $ex->getMessage();
    
  }

  $user_map = dal()->user->get_map();
  
  $location_map = dal()->location->get_map();

  if ( count( $location_map ) === 0 ) {
    
    $tail->
      p()->
        T_H( 'You haven\'t registered any locations. Please add at least ' .
          'one location before adding new thermostats.' )->
      p_end();
    
    return;
    
  }
  
  $group_map = dal()->group->get_map();
  
  if ( count( $group_map ) === 0 ) {
    
    $tail->
      p()->
        T_H( 'You haven\'t registered any groups. Please add at least ' .
          'one group before adding new thermostats.' )->
      p_end();
    
    return;
    
  }
  
  $connection_monitor = array(
    'onchange' => 'check_connection()',
    //'onblur' => 'check_connection()'
  );
  
  $tail->
    test( count( $error ) > 0 )->
      p( 'class', 'error' )->
        text( map_1st_value( $error ) )->
      p_end()->
    test_end()->
    form( 'method', 'POST' )->
      hidden( 'xsrf', session()->get_xsrf_token() )->
      hidden( 'user_id', user()->id )->
      fieldset( 'id', 'connection_options' )->
        legend()->
          T_TH( 'Connection Options' )->
        legend_end()->
        table()->
          tbody()->
    
            tr()->
              th(
                'title', A( 'Thermostat ID' )
              )->
                label( 'for', 'thermostat_id' )->
                  T_TH( 'Thermostat ID' )->
                  markup( ':' )->
                label_end()->
              th_end()->
              td(
                'title', A( 'Thermostat ID' )
              )->
                span(
                  'id', 'thermostat_id'
                )->
                  text( read_d( $data, 'id', 'new' ) )->
                span_end()->
              td_end()->
            tr_end()->
    
            input_field(
              'Address',
              'host_input',
              'host',
              $data,
              'hostname or IP address',
              'Enter the hostname or IP address used ' .
                'to connect to this thermostat.',
              'on',
              $connection_monitor,
              'required',
              '12em',
              '20em'
            )->
            input_field(
              'Port',
              'port_input',
              'port',
              $data,
              'HTTP port',
              'Enter the port number used to connect ' .
                'to this thermostat, usually port 80.',
              'on',
              $connection_monitor,
              'required'
            )->
            input_field(
              'Username',
              'user_input',
              'user',
              $data,
              'device username',
              'Enter the username of the ' .
                'administrative user, usually ' .
                '&quot;admin&quot;.',
              'on',
              $connection_monitor,
              'required',
              null,
              null,
              true // disabled
            )->
            password_field(
              'Password',
              'pass_input',
              'pass',
              $data,
              'device password',
              'Enter the password of the ' .
                'administrative user, typically ' .
                'defaults to &quot;admin&quot; but ' .
                'you should change that!',
              'off',
              $connection_monitor,
              'required'
            )->
          tbody_end()->
        table_end()->
      fieldset_end()->
      fieldset( 'id', 'database_settings' )->
        legend()->
          T_TH( 'Database Settings' )->
        legend_end()->
        table()->
          tbody()->
            select_field(
              'User',
              'user_select',
              'user_id',
              $user_map,
              $data,
              'Select a user for this thermostat.',
              null, // events
              null, // class
              null,
              null,
              $user_th,
              $user_td
            )->
            test( is_admin() )->
              decl(
                $dummy,
                $user_td->
                  markup( ' ' )->
                  new_link(
                    '/admin-user.php',
                    H( 'edit' ),
                    A( 'Click here to manage users.' )
                  )
              )->
            test_end()->
            input_field(
              'Custom Name',
              'name_input',
              'name',
              $data,
              'a name for this device',
              'Enter a friendly name to identify ' .
                'this thermostat.',
              'on',
              null, // events
              'required',
              '12em',
              '20em'
            )->
            input_field(
              'Description',
              'description_input',
              'description',
              $data,
              'a description for this device',
              'Enter a description of this thermostat.',
              'on',
              array( 'onchange' => 'check_connection()' )
            )->
            select_field(
              'Location',
              'location_select',
              'location_id',
              $location_map,
              $data,
              'Select the location of this thermostat.',
              null, // events
              null, // class
              null,
              null,
              $location_th,
              $location_td
            )->
            test( is_admin() )->
              decl(
                $dummy,
                $location_td->
                  markup( ' ' )->
                  new_link(
                    '/admin-location.php',
                    H( 'edit' ),
                    A( 'Click here to manage locations.' )
                  )
              )->
            test_end()->
            select_field(
              'Group',
              'group_select',
              'group_id',
              $group_map,
              $data,
              'Select a group for this thermostat.',
              null, // events
              null, // class
              null,
              null,
              $group_th,
              $group_td
            )->
            test( is_admin() )->
              decl(
                $dummy,
                $group_td->
                  markup( ' ' )->
                  new_link(
                    '/admin-group.php',
                    H( 'edit' ),
                    A( 'Click here to manage groups.' )
                  )
              )->
            test_end()->
          tbody_end()->
        table_end()->
      fieldset_end()->
      fieldset( 'id', 'device_settings' )->
        legend()->
          T_TH( 'Device Settings' )->
        legend_end()->
        p()->
          T_H( 'Contacting device...' )->
        p_end()->
      fieldset_end()->
      fieldset( 'id', 'sensor_settings' )->
        legend()->
          T_H( 'Sensor Settings' )->
        legend_end()->
        p()->
          T_H( 'Contacting device...' )->
        p_end()->
      fieldset_end()->
      fieldset( 'id', 'form_submission' )->
        legend()->
          T_H( 'Form Submission' )->
        legend_end()->
        table()->
          tbody()->
            decl(
              $next_map,
              array(
                'edit' => 'Edit Thermostat',
                'new' => 'Add New Thermostat',
                'home' => 'Go to Home Page'
              )
            )->
            select_field(
              'Next',
              'next_select',
              'next',
              $next_map,
              //user()->next,
              $data,
              'Where do you want to go when ' .
                'you&apos;re finished here?',
              null,
              null, // class
              '12em',
              '20em'
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
