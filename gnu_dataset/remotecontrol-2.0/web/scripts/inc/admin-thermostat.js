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

// TODO: encode HTML strings rather than relying on them being valid...

function check_connection() {

  var host = get_value( 'host_input', true );
  var port = get_value( 'port_input', true );
  var user = get_value( 'user_input', true );
  var pass = get_value( 'pass_input', true );
  
  if ( host && port && user && pass ) {

    grc_process({
      'action' : 'thermostat-settings',
      'host' : host,
      'port' : port,
      'user' : user,
      'pass' : pass
    },
    function( json ) {
      //for ( var i in json ) { alert( i + ': ' + json[ i ] ); }
      //alert( text );
      show_device_settings( json );
      show_sensor_settings( json );
    },
    function( error ) {
      hide_thermostat_settings( 'Error reading device settings.' );
    });
  }
}

function hide_thermostat_settings( message ) {

  if ( typeof message === 'undefined' ) {
    
    message = 'Contacting device...';
    
  }

  var fieldset = $( 'device_settings' );
  
  fieldset.innerHTML = '<legend>Device Settings</legend>' +
    '<p>' + message + '</p>';
  
  fieldset = $( 'sensor_settings' );
  
  fieldset.innerHTML = '<legend>Sensor Settings</legend>' +
    '<p>' + message + '</p>';
  
}

function show_device_settings( settings ) {

  if (
    ( typeof settings.local_sensor_state === 'undefined' ) ||
    settings.device_name === null
  ) {
    
    return hide_thermostat_settings(
      'Could not connect to thermostat. Check your connection options.'
    );
    
  }

  var fieldset = $( 'device_settings' );
  
  var html = '<legend>Device Settings</legend>';

  html += '<table>';
  html += '<tbody>';
  html +=
    '<tr><th style="width:12em;">Device Name:</th>' +
    '<td style="width:20em;">' +
    '<input name="device_name" value="' + settings.device_name + '">' +
    '</td></tr>' +
    '<tr><th style="width:12em;">Site Name:</th>' +
    '<td style="width:20em;">' +
    '<input name="site_name" value="' + settings.site_name + '"></td></tr>' +
    '<tr><th style="width:12em;">Model #:</th>' +
    '<td style="width:20em;">' + settings.model_number + '</td></tr>' +
    '<tr><th style="width:12em;">Time:</th>' +
    '<td style="width:20em;">' + settings.time + '</td></tr>';

  html += '</tr></tbody>';
  html += '</table><br>';

  fieldset.innerHTML = html;

}

function show_sensor_settings( settings ) {
  
  if (
    ( typeof settings.local_sensor_state === 'undefined' ) ||
    settings.temperature_scale === null
  ) {
    
    return hide_thermostat_settings(
      'Could not connect to thermostat. Check your connection options.'
    );
    
  }
  
  var scale = window.user_settings.temperature_scale;
  
  var fieldset = $( 'sensor_settings' );
  
  var html = '<legend>Sensor Settings</legend>';

  html += '<table>';
  html += '<tbody>';
  html += '<tr><th style="width:12em;">Scale:</th>' +
    '<td style="width:20em;">';

  html +=
    get_temperature_scale_select( settings.temperature_scale );

  html += '</td></tr></tbody>';
  html += '</table><br>';
 
  html += '<table>';
  html += '<thead>';

  html += '<tr>';
  html += '<th style="width:12em;"></th>';
  html += '<th>State</th>';
  html += '<th>Temperature</th>';
  html += '<th>Averaging</th>';
  html += '<th>Correction</th>';
  html += '</tr>';

  html += '</thead>';
  html += '<tbody>';
  
  html += get_sensor_row(
    scale,
    'local_sensor_',
    'Local Sensor',
    settings.local_sensor_state,
    settings.local_sensor_temp,
    settings.local_sensor_average,
    ''
  );
  
  html += get_sensor_row(
    scale,
    'remote_sensor_1_',
    'Remote Sensor 1',
    settings.remote_sensor_1_state,
    settings.remote_sensor_1_temp,
    settings.remote_sensor_1_average,
    settings.remote_sensor_1_correction
  );
  
  html += get_sensor_row(
    scale,
    'remote_sensor_2_',
    'Remote Sensor 2',
    settings.remote_sensor_2_state,
    settings.remote_sensor_2_temp,
    settings.remote_sensor_2_average,
    settings.remote_sensor_2_correction
  );
  
  html += '</tbody>';
  html += '</table>';
  
  fieldset.innerHTML = html;
  
}

function get_temperature_scale_select( scale ) {
  
  return (
    '<select ' +
      'id="temperature_scale_select" ' +
      'name="temperature_scale">' +
      '<option value="1"' +
        ( scale === '1' ? ' selected' : '' ) +
      '>Fahrenheit</option>' +
      '<option value="2"' +
        ( scale === '2' ? ' selected' : '' ) +
      '>Celsius</option>' +
    '</select>'
  );
  
}

function get_sensor_row(
  scale,
  name_prefix,
  label,
  state,
  temp,
  average,
  correction
) {

  var html = '<tr>';

  html += '<th>' + label + ':</th>';
  
  switch ( state ) {
  
    case '0' : html += '<td>Not Present</td>'; break;
    default  :
      
      html +=
        '<td>' +
        get_sensor_state_select( name_prefix, state ) +
        '</td>';
      
      break;
  
  }

  temp = format_temp( temp, scale );

  html +=
    '<td>' + temp + '</td>';
  
  html +=
    '<td>' +
    get_sensor_average_select( name_prefix, average, state ) +
    '</td>';
  
  html +=
    '<td>' +
    get_sensor_correction_select( name_prefix, correction, state, scale ) +
    '</td>';

  html += '</tr>';

  return html;
  
}

function get_sensor_state_select( name_prefix, selected ) {

  var name = name_prefix + 'state';

  return (
    '<select name="' + name + '">' +
      '<option value="1"' +
        ( selected === '1' ? ' selected' : '' ) +
      '>Disabled</option>' +
      '<option value="2"' +
        ( selected === '2' ? ' selected' : '' ) +
      '>Enabled</option>' +
    '</select>'
  );
  
}

function get_sensor_average_select( name_prefix, selected, state ) {

  var name = name_prefix + 'average';

  if ( state === '0' ) { return ''; }

  return (
    '<select name="' + name + '">' +
      '<option value="1"' +
        ( selected === '1' ? ' selected' : '' ) +
      '>Disabled</option>' +
      '<option value="2"' +
        ( selected === '2' ? ' selected' : '' ) +
      '>Enabled</option>' +
    '</select>'
  );
  
}

function get_sensor_correction_select(
  name_prefix,
  selected,
  state,
  scale
) {

  var name = name_prefix + 'correction';
  
  if ( state === '0' ) { return ''; }

  if ( name_prefix === 'local_sensor_' ) { return ''; }

  var html = '<select class="correction_select" name="' + name + '">';

  function is( val ) {
    
    var abs = Math.abs( val - selected );

    //alert( 'abs: ' + abs + ' val: ' + val + ' selected: ' + selected );
    
    if ( abs < 2 ) {
      
      return ' selected';
      
    }
    
    return '';
    
  };

  if ( scale === '1' ) {

    html += '\
<option value="50"' + is( 50 ) + '>5</option>\
<option value="45"' + is( 45 ) + '>4.5</option>\
<option value="40"' + is( 40 ) + '>4</option>\
<option value="35"' + is( 35 ) + '>3.5</option>\
<option value="30"' + is( 30 ) + '>3</option>\
<option value="25"' + is( 25 ) + '>2.5</option>\
<option value="20"' + is( 20 ) + '>2</option>\
<option value="15"' + is( 15 ) + '>1.5</option>\
<option value="10"' + is( 10 ) + '>1</option>\
<option value="5"' + is( 5 ) + '>0.5</option>\
<option value="0"' + is( 0 ) + '>0</option>\
<option value="-5"' + is( -5 ) + '>-0.5</option>\
<option value="-10"' + is( -10 ) + '>-1</option>\
<option value="-15"' + is( -15 ) + '>-1.5</option>\
<option value="-20"' + is( -20 ) + '>-2</option>\
<option value="-25"' + is( -25 ) + '>-2.5</option>\
<option value="-30"' + is( -30 ) + '>-3</option>\
<option value="-35"' + is( -35 ) + '>-3.5</option>\
<option value="-40"' + is( -40 ) + '>-4</option>\
<option value="-45"' + is( -45 ) + '>-4.5</option>\
<option value="-50"' + is( -50 ) + '>-5</option>';
    
  }
  else {

    html += '\
<option value="45"' + is( 45 ) + '>2.5</option>\
<option value="40"' + is( 40 ) + '>2.25</option>\
<option value="36"' + is( 36 ) + '>2</option>\
<option value="31"' + is( 31 ) + '>1.75</option>\
<option value="27"' + is( 27 ) + '>1.5</option>\
<option value="22"' + is( 22 ) + '>1.25</option>\
<option value="18"' + is( 18 ) + '>1</option>\
<option value="13"' + is( 13 ) + '>0.75</option>\
<option value="9"' + is( 9 ) + '>0.5</option>\
<option value="4"' + is( 4 ) + '>0.25</option>\
<option value="0"' + is( 0 ) + '>0</option>\
<option value="-4"' + is( -4 ) + '>-0.25</option>\
<option value="-9"' + is( -9 ) + '>-0.5</option>\
<option value="-13"' + is( -13 ) + '>-0.75</option>\
<option value="-18"' + is( -18 ) + '>-1</option>\
<option value="-22"' + is( -22 ) + '>-1.25</option>\
<option value="-27"' + is( -27 ) + '>-1.5</option>\
<option value="-31"' + is( -31 ) + '>-1.75</option>\
<option value="-36"' + is( -36 ) + '>-2</option>\
<option value="-40"' + is( -40 ) + '>-2.25</option>\
<option value="-45"' + is( -45 ) + '>-2.5</option>';
    
  }

  html += '</select>';
  
  return html;
  
}

function rescale() {

  var scale = get_value( 'temperature_scale_select' );
  
  check_connection( scale );
  
}

function format_temp( temp, scale ) {
  
  temp = parseInt( temp );
  
  if ( isNaN( temp ) ) { return ''; }

  var unit = 'F';

  temp /= 10;
  
  if ( scale == '2' ) { 
    
    unit = 'C';
    
    temp = ( temp - 32 ) * ( 5 / 9 );
    
  }

  temp = Math.round( temp * 10 ) / 10;

  var parts = temp.toString().split( '.' );
  
  if ( parts.length === 1 ) {
    
    parts.push( '0' );
    
  }

  return parts[ 0 ] + '.<small>' + parts[ 1 ] + '</small> &deg; ' + unit;
  //return parts[ 0 ] + '.' + parts[ 1 ] + ' &deg; ' + unit;
  
}
