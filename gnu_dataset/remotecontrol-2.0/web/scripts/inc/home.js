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

function get_checked() {

  var result = [];
  
  var table = $( 'thermostat_table' );
  var select_all_checkbox = $( 'select_all' );
  
  var checkboxes = table.getElementsByTagName( 'input' );

  for ( var i = 0, il = checkboxes.length; i < il; i++ ) {

    var checkbox = checkboxes[ i ];

    if ( checkbox === select_all_checkbox ) { continue; }

    if ( checkbox.checked ) { result.push( checkbox ); }
    
  }
  
  return result;
  
}

function toggle_all() {
  
  var table = $( 'thermostat_table' );
  var select_all_checkbox = $( 'select_all' );
  var select_all = select_all_checkbox.checked;
  
  var checkboxes = table.getElementsByTagName( 'input' );

  for ( var i = 0, il = checkboxes.length; i < il; i++ ) {

    var checkbox = checkboxes[ i ];

    if ( checkbox === select_all_checkbox ) { continue; }
    
    checkbox.checked = select_all;
    
  }
  
  push_checked();
  
}

function toggle_row( id ) {
  
  push_checked();
  
}

function push_checked() {

  var settings = $( 'settings' );
  
  var checked = get_checked();
  
  if ( checked.length === 0 ) {
    
    settings.innerHTML = '';
    
    return;
    
  }
  
  if ( settings.innerHTML !== '' ) {
  
    return;
  
  }
  
  var anchors = checked[ 0 ].
    parentElement.
    parentElement.
    getElementsByTagName( 'a' );

  for ( var i = 0, il = anchors.length; i < il; i++ ) {
    
    var anchor = anchors[ i ];
    
    if ( anchor.className === 'load' ) {
      
      anchor.click();
      
      break;
      
    }
  }
}

function load_thermostat( id, button ) {

  var div = $( 'settings' );
  div.innerHTML = '';
  
  grc_process({
    'action' : 'thermostat-load',
    'id' : id
  },
  function( json ) {
    
    if ( typeof json !== 'object' ) {
      
      //throw "Invalid json.";
      return alert( "Invalid json." );
      
    }
    
    for ( var i in json ) {
      
      if ( json[ i ] === null ) {
        
        //throw 'Invalid json at "' + i + '".';
        return alert( 'Invalid json at "' + i + '".' );
        
      }
    }
    
    var html = '';
    html += '<p>Loaded from thermostat ' + id + '.</p>';
    html += format_hvac_settings( json );
    html += format_setback_schedule_2( json );
    
    html += '<br><br>';
    html += '<input type="submit" value="Submit">';
    
    //html += '<br><br>';
    //html += debug_json( json );
    
    div.innerHTML = html;
  },
  function( error ) {
 
    div.innerHTML = '';
    
    show_error( error );
    
  });
  
}

function format_hvac_settings( json ) {

  var html = '';
  
  html += '<h2>HVAC Settings</h2>';
  
  html += '<table><thead><tr>' +
    '<th>HVAC Mode</th>' +
    '<th>Fan Mode</th>' +
    '<th>Setback Status</th>' +
    '<th>Setback Heat</th>' +
    '<th>Setback Cool</th>' +
    '</tr></thead><tbody><tr>';
  
  //var scale = json.temperature_scale;
  var scale = user_settings.temperature_scale;
  
  html += '<td>' + format_hvac_mode( json.hvac_mode ) + '</td>';
  html += '<td>' + format_fan_mode( json.fan_mode ) + '</td>';
  html += '<td>' + format_setback_status( json.setback_status ) + '</td>';
  html += '<td>' + 
    format_setback_temp( 'heat', json.setback_heat, scale ) + '</td>';
  html += '<td>' + 
    format_setback_temp( 'cool', json.setback_cool, scale ) + '</td>';
  
  html += '</tr></tbody></table>';
  
  return html;
  
}

function format_setback_schedule_1( json ) {

  var html = '';
  
  html += '<h2>Setback Scheduling</h2>';
  html += '<h3>Day Class Schedules</h3>';
  
  html +=
    '<table style="width:110em;"><thead><tr>' +
    '<th style="background:white !important;"></th>' +
    '<th colspan="4" class="occupied">Occupied</th>' +
    '<th colspan="4" class="unoccupied">Unoccupied</th>' +
    '<th colspan="4" class="other">Other</th></tr>' +
    '<tr>' +
    '<th style="width:6em;">Period</th>' +
    '<th style="width:10.6em;">Start</th>' +
    '<th class="heat" style="width:8em;">Heat</th>' +
    '<th class="cool" style="width:8em;">Cool</th>' +
    '<th style="width:6em;">Fan</th>' +
    '<th style="width:10.6em;">Start</th>' +
    '<th class="heat" style="width:8em;">Heat</th>' +
    '<th class="cool" style="width:8em;">Cool</th>' +
    '<th style="width:6em;">Fan</th>' +
    '<th style="width:10.6em;">Start</th>' +
    '<th class="heat" style="width:8em;">Heat</th>' +
    '<th class="cool" style="width:8em;">Cool</th>' +
    '<th style="width:6em;">Fan</th>' +
    '</tr>';

  html += '</thead><tbody>';

  html += format_period_1( 'morning', 'Morning', json );
  html += format_period_1( 'day', 'Day', json );
  html += format_period_1( 'evening', 'Evening', json );
  html += format_period_1( 'night', 'Night', json );

  html += '</tbody></table>';
  
  html += '<h3>Default Weekly Schedule</h3>';

  html += format_weekly_schedule( json );

  return html;
  
}

function format_setback_schedule_2( json ) {

  var html = '';
  
  html += '<h2>Setback Scheduling</h2>';
  html += '<h3>Day Class Schedules</h3>';
  
  html +=
    '<table><thead><tr>' +
    '<th style="background:white !important;"></th>' +
    '<th colspan="4" class="occupied">Occupied</th>' +
    '<tr>' +
    '<th style="width:6em;">Period</th>' +
    '<th style="width:10.6em;">Start</th>' +
    '<th class="heat" style="width:8em;">Heat</th>' +
    '<th class="cool" style="width:8em;">Cool</th>' +
    '<th style="width:6em;">Fan</th>' +
    '</tr></thead><tbody>';

  html += format_period_2( 'morning', 'occupied', 'Morning', json );
  html += format_period_2( 'day', 'occupied', 'Day', json );
  html += format_period_2( 'evening', 'occupied', 'Evening', json );
  html += format_period_2( 'night', 'occupied', 'Night', json );

  html += '</tbody></table>';

  html +=
    '<table><thead><tr>' +
    '<th style="background:white !important;"></th>' +
    '<th colspan="4" class="unoccupied">Unoccupied</th>' +
    '<tr>' +
    '<th style="width:6em;">Period</th>' +
    '<th style="width:10.6em;">Start</th>' +
    '<th class="heat" style="width:8em;">Heat</th>' +
    '<th class="cool" style="width:8em;">Cool</th>' +
    '<th style="width:6em;">Fan</th>' +
    '</tr></thead><tbody>';

  html += format_period_2( 'morning', 'unoccupied', 'Morning', json );
  html += format_period_2( 'day', 'unoccupied', 'Day', json );
  html += format_period_2( 'evening', 'unoccupied', 'Evening', json );
  html += format_period_2( 'night', 'unoccupied', 'Night', json );

  html += '</tbody></table>';

  html +=
    '<table><thead><tr>' +
    '<th style="background:white !important;"></th>' +
    '<th colspan="4" class="other">Other</th></tr>' +
    '<tr>' +
    '<th style="width:6em;">Period</th>' +
    '<th style="width:10.6em;">Start</th>' +
    '<th class="heat" style="width:8em;">Heat</th>' +
    '<th class="cool" style="width:8em;">Cool</th>' +
    '<th style="width:6em;">Fan</th>' +
    '</tr></thead><tbody>';

  html += format_period_2( 'morning', 'other', 'Morning', json );
  html += format_period_2( 'day', 'other', 'Day', json );
  html += format_period_2( 'evening', 'other', 'Evening', json );
  html += format_period_2( 'night', 'other', 'Night', json );

  html += '</tbody></table>';

  html += '<h3>Default Weekly Schedule</h3>';

  html += format_weekly_schedule( json );

  return html;
  
}

function format_weekly_schedule( json ) {
  
  var html = '';

  html +=
    '<table><thead><tr>' +
    '<th>Sunday</th>' +
    '<th>Monday</th>' +
    '<th>Tuesday</th>' +
    '<th>Wednesday</th>' +
    '<th>Thursday</th>' +
    '<th>Friday</th>' +
    '<th>Saturday</th>' +
    '</tr></thead><tbody><tr>' +
    '<td>' + format_class( 'sun', json ) + '</td>' +
    '<td>' + format_class( 'mon', json ) + '</td>' +
    '<td>' + format_class( 'tue', json ) + '</td>' +
    '<td>' + format_class( 'wed', json ) + '</td>' +
    '<td>' + format_class( 'thu', json ) + '</td>' +
    '<td>' + format_class( 'fri', json ) + '</td>' +
    '<td>' + format_class( 'sat', json ) + '</td>' +
    '</tr></tbody></table>';
  
  return html;
  
}

function format_class( day, json ) {
  
  var selected = json[ day + '_class' ];
  
  function is( v ) { return is_selected( v, selected ); }

  var html = '';
  var select_class = 'other';

  if ( selected === '1' ) { select_class = 'occupied'; }
  if ( selected === '2' ) { select_class = 'unoccupied'; }
  
  html += '<select name="' + day + '_class" class="' +
    select_class + '" ' + 'onchange="set_ddl_class(this)">';
  
  html += '<option class="occupied" value="1"' + is( '1' ) + '>Occupied</option>';
  html += '<option class="unoccupied" value="2"' + is( '2' ) + '>Unoccupied</option>';
  html += '<option class="other" value="3"' + is( '3' ) + '>Other</option>';
  html += '</select>';
  
  return html;
  
}

function format_period_2( period, $class, title, json ) {
  
  var html = '';
  
  html +=
    '<tr>' +
    '<th>' + title + '</th>' +
    format_period_class( period, $class, json ) +
    //format_period_class( period, 'occupied', json ) +
    //format_period_class( period, 'unoccupied', json ) +
    //format_period_class( period, 'other', json ) +
    '</tr>';
    
  return html;
  
}

function format_period_1( period, title, json ) {
  
  var html = '';
  
  html +=
    '<tr>' +
    '<th>' + title + '</th>' +
    format_period_class( period, 'occupied', json ) +
    format_period_class( period, 'unoccupied', json ) +
    format_period_class( period, 'other', json ) +
    '</tr>';
    
  return html;
  
}

function format_period_class( period, $class, json ) {
  
  var html = '';

  var start = json[ $class + '_' + period + '_start' ];
  var heat  = json[ $class + '_' + period + '_heat' ];
  var cool  = json[ $class + '_' + period + '_cool' ];
  var fan   = json[ $class + '_' + period + '_fan' ];

  var heat_select = format_temperature(
    $class + '_' + period + '_heat',
    heat,
    user_settings.temperature_scale,
    html_class='heat_select',
    css='width:5em;'
  );

  var cool_select = format_temperature(
    $class + '_' + period + '_cool',
    cool,
    user_settings.temperature_scale,
    html_class='cool_select',
    css='width:5em;'
  );

  var fan_select = format_fan( period, $class, fan );
  
  html +=
    '<td>' + format_start( period, $class, start ) + '</td>' +
    '<td>' + heat_select + '</td>' +
    '<td>' + cool_select + '</td>' +
    '<td>' + fan_select + '</td>';
  
  return html;
  
}

function format_fan( period, $class, selected ) {

  function is( v ) { return is_selected( v, selected ); }

  var html = '';
  
  html += '<select name="' + $class + '_' + period + '_fan">';
  html += '<option value="0"' + is( '0' ) + '>Off</option>';
  html += '<option value="60"' + is( '60' ) + '>On</option>';
  html += '<option value="15"' + is( '15' ) + '>15</option>';
  html += '<option value="30"' + is( '30' ) + '>30</option>';
  html += '<option value="45"' + is( '45' ) + '>45</option>';
  html += '</select>';
  
  return html;
  
}

function format_start( period, $class, minutes_since_midnight ) {
  
  var t = minutes_since_midnight;
  
  var hours = Math.floor( t / 60 );
  var minutes = t % 60;

  function is_h( h ) { return is_selected( h, hours ); }
  function is_m( m ) { return is_selected( m, minutes ); }

  var h_select = '';
  var m_select = '';
  
  h_select += '<select name="' + $class + '_' + period + '_hours">';
  for ( var h = 0; h <= 23; h++ ) {
    h_select += '<option value="' + h + '"' + is_h( h ) + '>' +
      h +
      '</option>';
  }
  h_select += '</select>';

  m_select += '<select name="' + $class + '_' + period + '_minutes">';
  for ( var m = 0; m <= 55; m += 5 ) {
    m_select += '<option value="' + m + '"' + is_m( m ) + '>' +
      format_dd( m ) +
      '</option>';
  }
  m_select += '</select>';

  return h_select + ':' + m_select;
  
}

function format_start_ampm( period, $class, minutes_since_midnight ) {
  
  var t = minutes_since_midnight;
  
  var pm = ( t >= ( 12 * 60 ) );
  
  if ( pm ) { t -= ( 12 * 60 ); }
  
  var ampm = pm ? 'pm' : 'am';
  
  var hours = Math.floor( t / 60 );
  var minutes = t % 60;

  function is_h( h ) { return is_selected( h, hours ); }
  function is_m( m ) { return is_selected( m, minutes ); }
  function is_a( a ) { return is_selected( a, ampm ); }

  var h_select = '';
  var m_select = '';
  var a_select = '';
  
  h_select += '<select name="' + $class + '_' + period + '_hours">';
  for ( var h = 1; h <= 12; h++ ) {
    h_select += '<option value="' + h + '"' + is_h( h ) + '>' +
      h +
      '</option>';
  }
  h_select += '</select>';

  m_select += '<select name="' + $class + '_' + period + '_minutes">';
  for ( var m = 0; m <= 55; m += 5 ) {
    m_select += '<option value="' + m + '"' + is_m( m ) + '>' +
      format_dd( m ) +
      '</option>';
  }
  m_select += '</select>';

  a_select += '<select name="' + $class + '_' + period + '_ampm">';
  a_select += '<option value="am"' + is_a( 'am' ) + '>AM</option>';
  a_select += '<option value="pm"' + is_a( 'pm' ) + '>PM</option>';
  a_select += '</select>';

  return h_select + ':' + m_select + ' ' + a_select;
  
}

function format_dd( val ) {
  
  var result = val.toString();
  
  if ( result.length === 1 ) { result = '0' + result; }
  
  return result;
  
}

function is_selected( a, b ) {

  if ( a == b ) {

    return ' selected';

  }

  return '';

}

function format_hvac_mode( hvac_mode ) {
  
  var is = function( v ) { return is_selected( v, hvac_mode ); };
  
  var html = '';
  
  html += '<select class="hvac_mode_select" name="hvac_mode">';
  html += '<option value="1"' + is( 1 ) + '>Off</option>';
  html += '<option value="2"' + is( 2 ) + '>Heat</option>';
  html += '<option value="3"' + is( 3 ) + '>Cool</option>';
  html += '<option value="4"' + is( 4 ) + '>Auto</option>';
  html += '</select>';
  
  return html;
  
}

function format_fan_mode( fan_mode ) {
  
  var is = function( v ) { return is_selected( v, fan_mode ); };
  
  var html = '';
  
  html += '<select class="fan_mode_select" name="fan_mode">';
  html += '<option value="1"' + is( 1 ) + '>Auto</option>';
  html += '<option value="2"' + is( 2 ) + '>On</option>';
  html += '<option value="3"' + is( 3 ) + '>Schedule</option>';
  html += '</select>';
  
  return html;
  
}

function format_setback_status( setback_status ) {
  
  var is = function( v ) { return is_selected( v, setback_status ); };
  
  var html = '';
  
  html += '<select class="setback_status_select" name="setback_status">';
  html += '<option value="1"' + is( 1 ) + '>Normal</option>';
  html += '<option value="2"' + is( 2 ) + '>Hold</option>';
  html += '<option value="3"' + is( 3 ) + '>Override</option>';
  html += '</select>';
  
  return html;
  
}

function format_setback_temp( type, selected, scale ) {
  
  return format_temperature(
    'setback_' + type,
    selected,
    scale,
    html_class='temp_select',
    css='width:6em;'
  );
  
}

function format_temperature( name, selected, scale, html_class, css ) {

  //return selected;

  var html = '';
  
  html += '<select class="' + html_class + '" name="' + name + '" ' +
    'style="' + css + '">';

  var tab = [];

  if ( scale == '1' ) {
    
    for ( var t = 990; t >= 400; t -= 10 ) {
    
      tab[ t.toString() ] = ( t / 10 ).toString();

    }
  }
  else {
    
    tab = {
      "986" : '37',
      "977" : '36.5',
      "968" : '36',
      "959" : '35.5',
      "950" : '35',
      "941" : '34.5',
      "932" : '34',
      "923" : '33.5',
      "914" : '33',
      "905" : '32.5',
      "896" : '32',
      "887" : '31.5',
      "878" : '31',
      "869" : '30.5',
      "860" : '30',
      "851" : '29.5',
      "842" : '29',
      "833" : '28.5',
      "824" : '28',
      "815" : '27.5',
      "806" : '27',
      "797" : '26.5',
      "788" : '26',
      "779" : '25.5',
      "770" : '25',
      "761" : '24.5',
      "752" : '24',
      "743" : '23.5',
      "734" : '23',
      "725" : '22.5',
      "716" : '22',
      "707" : '21.5',
      "698" : '21',
      "689" : '20.5',
      "680" : '20',
      "671" : '19.5',
      "662" : '19',
      "653" : '18.5',
      "644" : '18',
      "635" : '17.5',
      "626" : '17',
      "617" : '16.5',
      "608" : '16',
      "599" : '15.5',
      "590" : '15',
      "581" : '14.5',
      "572" : '14',
      "563" : '13.5',
      "554" : '13',
      "545" : '12.5',
      "536" : '12',
      "527" : '11.5',
      "518" : '11',
      "509" : '10.5',
      "500" : '10',
      "491" : '9.5',
      "482" : '9',
      "473" : '8.5',
      "464" : '8',
      "455" : '7.5',
      "446" : '7',
      "437" : '6.5',
      "428" : '6',
      "419" : '5.5',
      "410" : '5',
      "401" : '4.5'
    };

  }
  
  var closest_distance = 999;
  var closest_f_deci = null;
  var keys = [];
  
  var unit = ( scale === '1' ) ? '&deg;F' : '&deg;C';
  
  for ( var f_deci in tab ) {
  
    keys.push( f_deci );
    
    var distance = Math.abs( parseInt( f_deci ) - selected );
    
    //alert( distance );
    
    if ( distance < closest_distance ) {
      
      closest_distance = distance;
      closest_f_deci = f_deci;
      
    }
  }

  function is( val ) {
    
    return ( val == closest_f_deci ) ? ' selected' : '';
    
  };
  
  keys.reverse();
  
  for ( var i in keys ) {
    
    var f_deci = keys[ i ];
    
    html += '<option value="' + f_deci + '"' + is( f_deci ) + '>' +
      tab[ f_deci ] + '</option>';
    
  }
  
  
  html += '</select>';
  
  return html;
  
}

function delete_thermostat( id, button ) {
  var msg = 'Are you sure you want to delete this thermostat?';
  if ( ! confirm( msg ) ) { return; }
  grc_process({
    'action' : 'thermostat-delete',
    'id' : id
  },
  function( json ) {

    //for ( var i in json ) { alert( i + ': ' + json[ i ] ); }

    location.reload();
    
  },
  show_error
  );
}

function add_thermostat() {
  
	var name = get_value( 'Name_0_Input' );
	var description = get_value( 'Description_0_Input' );
	var host = get_value( 'Host_0_Input' );
	var port = get_value( 'Port_0_Input' );
  var location_id = get_value( 'Location_0_Input' );
	var group_id = get_value( 'Group_0_Input' );

  if (
    require( name, 'Custom Name' ) &&
    //require( description, 'Description' ) &&
    require( host, 'Domain Name/IP' ) &&
    require_int( port, 'Port', 1 ) &&
    require_id( group_id, 'Group', 1 ) &&
    require_id( location_id, 'Location', 1 ) //&&
  ) {

    grc_process( {
      'action' : 'thermostat-add',
      'name' : name,
      'description' : description,
      'host' : host,
      'port' : port,
      'location_id' : location_id,
      'group_id' : group_id
    },
    function( json ) {

      //for ( var i in json ) { alert( i + ': ' + json[ i ] ); }
      
      location.reload();

    },
    function( error ) {
      show_error( error );
    });

  	//ajax.doPost('services/process_thermostat_db.php', postData, 'loadimage', handleAddResponse);
  }
}

function update_thermostat( id ) {
  
  var name = get_value( 'Name_' + id + '_Input' );
	var description = get_value( 'Description_' + id + '_Input' );
	var host = get_value( 'Host_' + id + '_Input' );
	var port = get_value( 'Port_' + id + '_Input' );
  //var timezone = get_value( 'TimeZoneOffset_' + id + '_Input' );
  var location_id = get_value( 'Location_' + id + '_Input' );
	var group_id = get_value( 'Group_' + id + '_Input' );

  if (
    require( name, 'Custom Name' ) &&
    //require( description, 'Description' ) &&
    require( host, 'Domain Name/IP' ) &&
    require( port, 'Port' ) &&
    require( location_id, 'Location' ) &&
    require( group_id, 'Group' )
  ) {
  
    grc_process( {
      'action' : 'update_thermostat',
      'thermostat_id' : id,
      'name' : name,
      'description' : description,
      'host' : host,
      'port' : port,
      'location_id' : location_id,
      'group_id' : group_id
    },
    function( json ) {

      set_view( 'Name_' + id );
      set_view( 'Description_' + id );
      set_view( 'Host_' + id );
      set_view( 'Port_' + id );
      set_view( 'Location_' + id );
      set_view( 'Group_' + id );
      
      set_display( 'Edit_' + id, 'inline' );
      set_display( 'Update_' + id, 'none' );
      set_display( 'Cancel_' + id, 'none' );

      set_value( 'Name_' + id, json.name );
      set_value( 'Description_' + id, json.description );
      set_value( 'Host_' + id, json.host );
      set_value( 'Port_' + id, json.port );
      set_value( 'Location_' + id, json.location_id );
      set_value( 'Group_' + id, json.group_id );

    },
    show_error
    );

  }
}

function set_ddl_class( ddl ) {

  switch( ddl.value ) {
    
		case '1':
      
			ddl.className = 'occupied';
      
			break;

		case '2':

			ddl.className = 'unoccupied';

			break;

		case '3':

			ddl.className = 'other';

			break;
      
	}
}

function select_group( group_id ) {
  
  var class_name = 'group-' + group_id;
  
  var input_boxes = document.getElementsByTagName( 'input' );

  for ( var i = 0, il = input_boxes.length; i < il; i++ ) {
    
    var input_box = input_boxes[ i ];
    
    if ( input_box.className === class_name ) {
      
      input_box.checked = true;
      
    }
  }
  
  push_checked();
  
}
