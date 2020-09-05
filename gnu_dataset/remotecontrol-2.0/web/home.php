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

//echo "<pre>"; var_dump( user() ); die;

if ( user()->deleted ) {
  
  html( intl()->get_lang() );

  render_head( 'Invalid User Account' );

  html()->tail->
    p()->
      T_H( 'The user account you have authenticated with has been ' .
        'deleted.'
      )->
    p_end();

  render_foot();

  html()->render();
  
}

if ( dal()->location->count_all() == 0 ) {
  
  redirect( 'admin-location.php' );
  
}

if ( dal()->group->count_all() == 0 ) {
  
  redirect( 'admin-group.php' );
  
}


// specify default settings in here:
$default = array(
  'id' => array(),
  'action' => 'set-thermostats'
);

// input is read into here:
$input = array();

input( 'action', 'home_page_action' );

input( 'id', 'vector', 'thermostat_id' );
input( 'hvac_mode' );
input( 'fan_mode' );
input( 'setback_status' );
input( 'setback_heat' );
input( 'setback_cool' );

input( 'occupied_morning_hours' );
input( 'occupied_morning_minutes' );
input( 'occupied_morning_heat' );
input( 'occupied_morning_cool' );
input( 'occupied_morning_fan' );
input( 'occupied_day_hours' );
input( 'occupied_day_minutes' );
input( 'occupied_day_heat' );
input( 'occupied_day_cool' );
input( 'occupied_day_fan' );
input( 'occupied_evening_hours' );
input( 'occupied_evening_minutes' );
input( 'occupied_evening_heat' );
input( 'occupied_evening_cool' );
input( 'occupied_evening_fan' );
input( 'occupied_night_hours' );
input( 'occupied_night_minutes' );
input( 'occupied_night_heat' );
input( 'occupied_night_cool' );
input( 'occupied_night_fan' );

input( 'unoccupied_morning_hours' );
input( 'unoccupied_morning_minutes' );
input( 'unoccupied_morning_heat' );
input( 'unoccupied_morning_cool' );
input( 'unoccupied_morning_fan' );
input( 'unoccupied_day_hours' );
input( 'unoccupied_day_minutes' );
input( 'unoccupied_day_heat' );
input( 'unoccupied_day_cool' );
input( 'unoccupied_day_fan' );
input( 'unoccupied_evening_hours' );
input( 'unoccupied_evening_minutes' );
input( 'unoccupied_evening_heat' );
input( 'unoccupied_evening_cool' );
input( 'unoccupied_evening_fan' );
input( 'unoccupied_night_hours' );
input( 'unoccupied_night_minutes' );
input( 'unoccupied_night_heat' );
input( 'unoccupied_night_cool' );
input( 'unoccupied_night_fan' );

input( 'other_morning_hours' );
input( 'other_morning_minutes' );
input( 'other_morning_heat' );
input( 'other_morning_cool' );
input( 'other_morning_fan' );
input( 'other_day_hours' );
input( 'other_day_minutes' );
input( 'other_day_heat' );
input( 'other_day_cool' );
input( 'other_day_fan' );
input( 'other_evening_hours' );
input( 'other_evening_minutes' );
input( 'other_evening_heat' );
input( 'other_evening_cool' );
input( 'other_evening_fan' );
input( 'other_night_hours' );
input( 'other_night_minutes' );
input( 'other_night_heat' );
input( 'other_night_cool' );
input( 'other_night_fan' );

input( 'sun_class' );
input( 'mon_class' );
input( 'tue_class' );
input( 'wed_class' );
input( 'thu_class' );
input( 'fri_class' );
input( 'sat_class' );

handle_input( $input, $error );

html( intl()->get_lang() );

render_head( 'Home' );

render_page( $input, $error );

$foot_script = <<<EOF
  
  push_checked();
  
EOF;

render_foot( $foot_script );

html()->render();

function handle_input( &$input, &$error ) {
  
  try {

    if ( ! is_array( read_a( $input, 'id' ) ) ) {

      throw Error( 'ID list must be an array.' );

    }
    
    if ( is_post() ) {

      if ( count( $input[ 'id' ] ) <= 0 ) {
        
        throw Error( 'You must select at least one thermostat.' );
        
      }

      if ( $input[ 'action' ] === 'set-time' ) {

        foreach ( $input[ 'id' ] as $id ) {

          bom()->thermostat->write_time( $id );

        }
      }
      else {
        
        $occupied_morning_start = get_msm( 'occupied', 'morning' );
        $occupied_day_start = get_msm( 'occupied', 'day' );
        $occupied_evening_start = get_msm( 'occupied', 'evening' );
        $occupied_night_start = get_msm( 'occupied', 'night' );

        $unoccupied_morning_start = get_msm( 'unoccupied', 'morning' );
        $unoccupied_day_start = get_msm( 'unoccupied', 'day' );
        $unoccupied_evening_start = get_msm( 'unoccupied', 'evening' );
        $unoccupied_night_start = get_msm( 'unoccupied', 'night' );

        $other_morning_start = get_msm( 'other', 'morning' );
        $other_day_start = get_msm( 'other', 'day' );
        $other_evening_start = get_msm( 'other', 'evening' );
        $other_night_start = get_msm( 'other', 'night' );

        validator()->thermostat_start( 'occupied', $input );
        validator()->thermostat_start( 'unoccupied', $input );
        validator()->thermostat_start( 'other', $input );

        if (
          ( $input[ 'setback_heat' ] + 10 ) > $input[ 'setback_cool' ]
        ) {

          throw Error(
            'Setback Heat "%heat%" must be less than Setback Cool "%cool%".',
            'heat', $input[ 'setback_heat' ],
            'cool', $input[ 'setback_cool' ]
          );

        }

        validator()->thermostat_temperature( 'occupied', $input );
        validator()->thermostat_temperature( 'unoccupied', $input );
        validator()->thermostat_temperature( 'other', $input );

        foreach ( $input[ 'id' ] as $id ) {

          bom()->thermostat->write_settings( $id, $input );

        }
      }
    }
  }
  catch ( Exception $ex ) {

    dal()->exception->log( $ex );

    $error[ 'MAIN' ] = $ex->getMessage();
    
  }
}

// minutes since midnight:
function get_msm( $class, $period ) {
  
  global $input;
  
  $prefix   = $class . '_' . $period;
  
  $h_name   = $prefix . '_hours';
  $m_name   = $prefix . '_minutes';
  $msm_name = $prefix . '_start';
  
  $h = read_a( $input, $h_name );
  $m = read_a( $input, $m_name );
  
  $msm = ( $h * 60 ) + $m;
  
  $input[ $msm_name ] = $msm;
  
  return $msm;
  
}

function render_page( &$input, &$error ) {

  $tail = html()->tail;
  
  $table = new GrcTableView(
    'thermostat',
    array(
      'select' => array(
        'is-computed' => true,
        'no-menu' => true,
        'heading' => function() {
          return '<input id="select_all" type="checkbox" onchange="toggle_all()">';
        },
        'format' => function( &$row, $column, $schema, $tail )
          use ( $input ) {
          
          $id_list = read_a( $input, 'id' );
          
          $checked = (
            is_array( $id_list ) &&
            in_array( $row[ 'id' ], $id_list )
          );
          
          $tail->input(
            'name', 'id[]',
            'type', 'checkbox',
            'class', 'group-' . $row[ 'group_id' ],
            'value', $row[ 'id' ],
            'onchange', 'toggle_row(' . $row[ 'id' ] . ')',
            'checked', $checked
          );
          //)->p()->text( serialize( $input ) )->p_end();
        },
      ),
      /*
      'load' => array(
        'is-computed' => true,
        'format' => function( &$row, $column, $schema, $tail ) {
          $tail->
            a(
              'class', 'load',
              'href', 'javascript:void(0)',
              'onclick', 'load_thermostat(' . $row[ 'id' ] . ',this)'
            )->
              T_H( 'Load' )->
            a_end();
        },
      ),
      */
      'id' => array(
        'heading' => TABLE_HEADING( 'ID' ),
        'type' => INT_COLUMN,
      ),
      'device_name' => array(
        'is-computed' => true,
        'format' => function( &$row, $column, $schema, $tail ) {
          $device_name = read_a( $row, 'device_name' );
          $bad_read = read_d( $row, 'bad_read', false );
          //return serialize( $bad_read );
          if ( $bad_read !== false || $device_name === null ) {
            $tail->
              span( 'class', 'error', 'title', $bad_read )->
                text( 'Read Error' )->
              span_end();
            return;
          }
          //return serialize( $device_name );
          return $device_name;
        },
      ),
      'site_name' => array(
        'is-computed' => true,
        'format' => function( &$row, $column, $schema, $tail ) {
          return read_a( $row, 'site_name' );
        },
      ),
      'model_number' => array(
        'is-computed' => true,
        'heading' => TABLE_HEADING( 'Model #' ),
        'format' => function( &$row, $column, $schema, $tail ) {
          return read_a( $row, 'model_number' );
        },
      ),
      'location_id' => array(
        'sql' => 'l.id',
        'default' => false,
        'heading' => TABLE_HEADING( 'Location ID' ),
        'type' => INT_COLUMN,
      ),
      'location' => array(
        'sql' => 'l.name',
        'heading' => TABLE_HEADING( 'Location' ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          return henc( read_a( $row, 'location' ) );
        },
      ),
      'group_id' => array(
        'sql' => 'g.id',
        'default' => false,
        'heading' => TABLE_HEADING( 'Group ID' ),
        'type' => INT_COLUMN,
      ),
      'group' => array(
        'sql' => 'g.name',
        'heading' => TABLE_HEADING( 'Group' ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $tail->
            span(
              'id', 'group_name_' . $id . '_span',
              'class', 'view'
            )->
              //T_T( $row[ $column ] )->
              a(
                'href', 'javascript:void(0)',
                'onclick', 'select_group(' . $row[ 'group_id' ] . ')'
              )->
                markup( $row[ $column ] )->
              a_end()->
            span_end();
        },
      ),
      'name' => array(
        'sql' => 't.name',
        'heading' => TABLE_HEADING( 'Custom Name' ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $timezone = $row[ $column ];
          $tail->
            label(
              'id', 'name_' . $id . '_label',
              'class', 'view',
              'style', 'width:100% !important;text-align:center;'
            )->
              text( read_a( $row, 'name' ) )->
            label_end();
        },
      ),
      'description' => array(
        'sql' => 't.description',
        'heading' => TABLE_HEADING( 'Description' ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $timezone = $row[ $column ];
          $tail->
            label(
              'id', 'description_' . $id . '_label',
              'class', 'view',
              'style', 'width:100% !important;text-align:center;'
            )->
              text( read_a( $row, 'description' ) )->
            label_end();
        },
      ),
      'host' => array(
        'heading' => TABLE_HEADING( 'Address' ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          $id = $row[ 'id' ];
          $tail->
            label(
              'id', 'description_' . $id . '_label',
              'class', 'view',
              'style', 'width:100% !important;text-align:center;'
            )->
              text( read_a( $row, 'host' ) )->
            label_end();
        },
      ),
      'port' => array(
        'default' => false,
        'heading' => TABLE_HEADING( 'Port' ),
        'type' => INT_COLUMN,
      ),
      'user' => array(
        'default' => false,
        'heading' => TABLE_HEADING( 'Auth User' ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          return read_a( $row, 'user' );
        },
      ),
      'username' => array(
        'default' => is_admin(),
        'heading' => TABLE_HEADING( 'Username' ),
        'type' => STRING_COLUMN,
        'format' => function( &$row, $column, $schema, $tail ) {
          return read_a( $row, 'username' );
        },
      ),
      'action' => array(
        'is-computed' => true,
        'format' => function( &$row, $column, $schema, $tail ) {
          $tail->
            a(
              'class', 'load',
              'href', 'javascript:void(0)',
              'onclick', 'load_thermostat(' . $row[ 'id' ] . ',this)'
            )->
              T_H( 'Load' )->
            a_end()->
            markup( ',' )->
            nbsp()->
            new_link(
              '/admin-thermostat.php',
              A( 'Edit' ),
              A( 'Edit this thermostat.' ),
              null,
              array( 'id' => read_a( $row, 'id' ) )
            )->
            test( is_admin() )->
              markup( ',' )->
              nbsp()->
              a(
                'href', 'javascript:void(0)',
                'onclick', 'delete_thermostat(' . $row[ 'id' ] . ',this)'
              )->
                T_H( 'Delete' )->
              a_end()->
              markup( ',' )->
              nbsp()->
              a(
                'href', 'http://' . $row[ 'user' ] . ':' . $row[ 'pass' ] .
                  '@' . $row[ 'host' ] . ':' . $row[ 'port' ],
                'target', '_blank'
              )->
                T_H( 'Admin' )->
              a_end()->
            test_end();
        },
      ),
    )
  );
  
  if ( is_admin() ) {

    $data = bom()->thermostat->get_all(
      $count,
      $table->page,
      $table->sort
    );
    
  }        
  else {
  
    $data = bom()->thermostat->get_for_user(
      $count,
      $table->page,
      $table->sort
    );
    //$total = dal()->group->report_total();

  }
  
  if ( count( $data ) === 0 ) {

    $tail->
      test( is_admin() )->
        p()->
          T_H(
            'You don\'t have any thermostats. To add a new thermostat ' .
            'select %new% from %menu%.',
            'new', H( 'New Thermostat' ),
            'menu', H( 'Admin Menu' ) 
          )->
        p_end()->
      test_end()->
      test( ! is_admin() )->
        p()->
          T_H(
            'You don\'t have any thermostats. Please ask your systems ' .
            'administrator to add thermostats for you to manage.' )->
        p_end()->
      test_end();
    
  } 
  else {

    if ( count( $error ) > 0 ) {
      
      $msg = map_1st_value( $error );
      
      $tail = $tail->
        p( 'id', 'error', 'class', 'error' )->
          text( $msg )->
        p_end();
      
    }
    else if ( is_post() ) {
      
      $tail = $tail->
        p( 'id', 'success', 'class', 'success' )->
          T_H( 'Settings applied successfully.' )->
        p_end();
      
    }

    /*
    $tail = $tail->
      p()->
        T_H( 'Select thermostats to operate on using the checkboxes ' .
          'on the left. Then click "Set Time on Selected Devices" ' .
          'to set the time on the devices or click "Load" to load ' .
          'default settings for next submission.'
        )->
      p_end();
    */
    
    $tail = $tail->form( 'method', 'POST' );
    
    $tail->hidden( 'xsrf', session()->get_xsrf_token() );
    
    //$table->render( $data, $tail, $total );
    $tail = $table->render(
      $data,
      $tail,
      $total, // total
      null, // highlight spec
      false,
      false,
      true
    );

    if ( is_post() && $input[ 'action' ] === 'set-thermostats' ) {

      $tail->
        div( 'id', 'settings' )->
          p()->T_H( 'Settings submitted via HTTP.' )->p_end()->
          h2( 'id', 'hvac' )->T_H( 'HVAC Settings' )->h2_end()->
          table()->
            thead()->
              tr()->
                th()->T_TH( 'HVAC Mode' )->th_end()->
                th()->T_TH( 'Fan Mode' )->th_end()->
                th()->T_TH( 'Setback Status' )->th_end()->
                th()->T_TH( 'Setback Heat' )->th_end()->
                th()->T_TH( 'Setback Cool' )->th_end()->
              tr_end()->
            thead_end()->
            tbody()->
              tr()->
                td()->
                  select(
                    'class', 'hvac_mode_select',
                    'name', 'hvac_mode'
                  )->
                    option(
                      'value', '1',
                      'selected', $input[ 'hvac_mode' ] == '1'
                    )->
                      T_H( 'Off' )->
                    option_end()->
                    option(
                      'value', '2',
                      'selected', $input[ 'hvac_mode' ] == '2'
                    )->
                      T_H( 'Heat' )->
                    option_end()->
                    option(
                      'value', '3',
                      'selected', $input[ 'hvac_mode' ] == '3'
                    )->
                      T_H( 'Cool' )->
                    option_end()->
                    option(
                      'value', '4',
                      'selected', $input[ 'hvac_mode' ] == '4'
                    )->
                      T_H( 'Auto' )->
                    option_end()->
                  select_end()->
                td_end()->
                td()->
                  select(
                    'class', 'fan_mode_select',
                    'name', 'fan_mode'
                  )->
                    option(
                      'value', '1',
                      'selected', $input[ 'fan_mode' ] == '1'
                    )->
                      T_H( 'Auto' )->
                    option_end()->
                    option(
                      'value', '2',
                      'selected', $input[ 'fan_mode' ] == '2'
                    )->
                      T_H( 'On' )->
                    option_end()->
                    option(
                      'value', '3',
                      'selected', $input[ 'fan_mode' ] == '3'
                    )->
                      T_H( 'Schedule' )->
                    option_end()->
                  select_end()->
                td_end()->
                td()->
                  select(
                    'class', 'setback_status_select',
                    'name', 'setback_status'
                  )->
                    option(
                      'value', '1',
                      'selected', $input[ 'setback_status' ] == '1'
                    )->
                      T_H( 'Normal' )->
                    option_end()->
                    option(
                      'value', '2',
                      'selected', $input[ 'setback_status' ] == '2'
                    )->
                      T_H( 'Hold' )->
                    option_end()->
                    option(
                      'value', '3',
                      'selected', $input[ 'setback_status' ] == '3'
                    )->
                      T_H( 'Override' )->
                    option_end()->
                  select_end()->
                td_end()->
                td()->
                  add( get_temp_select( 'setback_heat', $input ) )->
                td_end()->
                td()->
                  add( get_temp_select( 'setback_cool', $input ) )->
                td_end()->
              tr_end()->
            tbody_end()->
          table_end()->
          h2( 'id', 'setback' )->T_TH( 'Setback Scheduling' )->h2_end()->
          h3( 'id', 'day' )->T_TH( 'Day Class Schedules' )->h3_end()->
          add(
            get_class_table( 'occupied', 'Occupied', $input )
          )->
          add(
            get_class_table( 'unoccupied', 'Unoccupied', $input )
          )->
          add(
            get_class_table( 'other', 'Other', $input )
          )->
          h3( 'id', 'weekly' )->
            T_TH( 'Default Weekly Schedule' )->
          h3_end()->
          add( get_weekly_schedule( $input ) )->
          br()->br()->
          input( 'type', 'submit', 'value', 'Submit' )->
        div_end()->
      form_end();
      
    }
    else {
    
      $tail->
        div( 'id', 'settings' )->
        div_end()->
      form_end();

    }
  }
}

function get_temp_select( $name, $input ) {
  
  $selected = read_a( $input, $name );

  $select = elem(
    'select',
    'class', 'temp_select',
    'name', $name,
    'style', 'width:6em'
  );

  $scale = user()->temperature_scale;

  $tab = array();
  
  if ( $scale == '1' ) {
    
    for ( $t = 990; $t >= 400; $t -= 10 ) {

      $tab[ strval( $t ) ] = strval( $t / 10 );

    }
  }
  else {
    
    $tab = array(
      "986" => '37',
      "977" => '36.5',
      "968" => '36',
      "959" => '35.5',
      "950" => '35',
      "941" => '34.5',
      "932" => '34',
      "923" => '33.5',
      "914" => '33',
      "905" => '32.5',
      "896" => '32',
      "887" => '31.5',
      "878" => '31',
      "869" => '30.5',
      "860" => '30',
      "851" => '29.5',
      "842" => '29',
      "833" => '28.5',
      "824" => '28',
      "815" => '27.5',
      "806" => '27',
      "797" => '26.5',
      "788" => '26',
      "779" => '25.5',
      "770" => '25',
      "761" => '24.5',
      "752" => '24',
      "743" => '23.5',
      "734" => '23',
      "725" => '22.5',
      "716" => '22',
      "707" => '21.5',
      "698" => '21',
      "689" => '20.5',
      "680" => '20',
      "671" => '19.5',
      "662" => '19',
      "653" => '18.5',
      "644" => '18',
      "635" => '17.5',
      "626" => '17',
      "617" => '16.5',
      "608" => '16',
      "599" => '15.5',
      "590" => '15',
      "581" => '14.5',
      "572" => '14',
      "563" => '13.5',
      "554" => '13',
      "545" => '12.5',
      "536" => '12',
      "527" => '11.5',
      "518" => '11',
      "509" => '10.5',
      "500" => '10',
      "491" => '9.5',
      "482" => '9',
      "473" => '8.5',
      "464" => '8',
      "455" => '7.5',
      "446" => '7',
      "437" => '6.5',
      "428" => '6',
      "419" => '5.5',
      "410" => '5',
      "401" => '4.5'
    );
    
  }
  
  foreach ( $tab as $f_deci => $label ) {
    
    $select->option(
      'value', $f_deci,
      'selected', $f_deci == $selected
    )->
      text( $label )->
    option_end();
    
  }
  
  return $select;
  
}

function get_class_table( $class, $title, &$input ) {
  
  $table = elem( 'table', 'id', $class );

  $table->
    thead()->
      tr()->
        th( 'style', 'background:white !important;' )->th_end()->
        th( 'colspan', '4', 'class', $class )->
          T_TH( $title )->
        th_end()->
      tr_end()->
      tr()->
        th( 'style', 'width:6em;' )->T_TH( 'Period' )->th_end()->
        th( 'style', 'width:10.6em;' )->T_TH( 'Start' )->th_end()->
        th( 'class', 'heat', 'style', 'width:8em;' )->
          T_TH( 'Heat' )->
        th_end()->
        th( 'class', 'cool', 'style', 'width:8em;' )->
          T_TH( 'Cool' )->
        th_end()->
        th( 'style', 'width:6em;' )->T_TH( 'Fan' )->th_end()->
      tr_end()->
    thead_end()->
    tbody()->
      add( get_class_period( 'morning', $class, 'Morning', $input ) )->
      add( get_class_period( 'day', $class, 'Day', $input ) )->
      add( get_class_period( 'evening', $class, 'Evening', $input ) )->
      add( get_class_period( 'night', $class, 'Night', $input ) )->
    tbody_end()->
  table_end();
  
  return $table;
  
}

function get_class_period( $period, $class, $title, $input ) {
  
  $tr = elem( 'tr' );

  $tr->
    th()->T_TH( $title )->th_end()->
    td()->add( get_start( $period, $class, $input ) )->td_end()->
    td()->add(
      get_temp_select( $class . '_' . $period . '_heat', $input )
    )->
    td_end()->
    td()->add(
      get_temp_select( $class . '_' . $period . '_cool', $input )
    )->
    td_end()->
    td()->add(
      get_fan( $period, $class, $input )
    )->
    td_end()->
  tr_end();
  
  return $tr;
  
}

function get_start( $period, $class, &$input ) {

  // minutes since midnight:
  //$t = $input[ $class . '_' . $period . '_start' ];
  //$hours = floor( $t / 60 );
  //$minutes = $t % 60;

  $hours = $input[ $class . '_' . $period . '_hours' ];
  $minutes = $input[ $class . '_' . $period . '_minutes' ];
  
  $h_select = elem(
    'select',
    'name', $class . '_' . $period . '_hours'
  );
  
  for ( $h = 0; $h <= 23; $h++ ) {
    
    $h_select->
      option( 'value', $h, 'selected', $h == $hours )->
        markup( $h )->
      option_end();
    
  }
  
  $m_select = elem(
    'select',
    'name', $class . '_' . $period . '_minutes'
  );
  
  for ( $m = 0; $m <= 55; $m += 5 ) {
    
    $m_select->
      option( 'value', $m, 'selected', $m == $minutes )->
        markup( format_dd( $m ) )->
      option_end();
    
  }

  $result = GrcHtmlComposite::Create();
  
  $result->
    add( $h_select )->
    markup( ':' )->
    add( $m_select );
  
  return $result;
  
}

function get_fan( $period, $class, &$input ) {
  
  $name = $class . '_' . $period . '_fan';
  
  $selected = read_a( $input, $name );
  
  $select = elem( 'select', 'name', $name );
  
  $select->
    option( 'value', '0', 'selected', $selected == '0' )->
      T_H( 'Off' )->
    option_end()->
    option( 'value', '60', 'selected', $selected == '60' )->
      T_H( 'On' )->
    option_end()->
    option( 'value', '15', 'selected', $selected == '15' )->
      T_H( '15' )->
    option_end()->
    option( 'value', '30', 'selected', $selected == '30' )->
      T_H( '30' )->
    option_end()->
    option( 'value', '45', 'selected', $selected == '45' )->
      T_H( '45' )->
    option_end()->
  select_end();
  
  return $select;
  
}

function get_weekly_schedule( &$input ) {
  
  $table = elem( 'table' );
  
  $table->
    thead()->
      tr()->
        th()->T_TH( 'Sunday' )->th_end()->
        th()->T_TH( 'Monday' )->th_end()->
        th()->T_TH( 'Tuesday' )->th_end()->
        th()->T_TH( 'Wednesday' )->th_end()->
        th()->T_TH( 'Thursday' )->th_end()->
        th()->T_TH( 'Friday' )->th_end()->
        th()->T_TH( 'Saturday' )->th_end()->
      tr_end()->
    thead_end()->
    tbody()->
      tr()->
        td()->add( get_day_class( 'sun', $input ) )->td_end()->
        td()->add( get_day_class( 'mon', $input ) )->td_end()->
        td()->add( get_day_class( 'tue', $input ) )->td_end()->
        td()->add( get_day_class( 'wed', $input ) )->td_end()->
        td()->add( get_day_class( 'thu', $input ) )->td_end()->
        td()->add( get_day_class( 'fri', $input ) )->td_end()->
        td()->add( get_day_class( 'sat', $input ) )->td_end()->
      tr_end()->
    tbody_end()->
  table_end();
  
  return $table;
  
}

function get_day_class( $day, &$input ) {
  
  $name = $day . '_class';
  
  $selected = read_a( $input, $name );
  
  $select_class = 'other';
  
  if ( $selected == '1' ) { $select_class = 'occupied'; }
  if ( $selected == '2' ) { $select_class = 'unoccupied'; }

  $select = elem(
    'select',
    'name', $name,
    'class', $select_class,
    'onchange', 'set_ddl_class(this)'
  );
  
  $select->
    option(
      'class', 'occupied',
      'value', '1',
      'selected', $selected == '1' 
    )->
      T_H( 'Occupied' )->
    option_end()->
    option(
      'class', 'unoccupied',
      'value', '2',
      'selected', $selected == '2' 
    )->
      T_H( 'Unoccupied' )->
    option_end()->
    option(
      'class', 'other',
      'value', '3',
      'selected', $selected == '3'
    )->
      T_H( 'Other' )->
    option_end()->
  select_end();
  
  return $select;
}