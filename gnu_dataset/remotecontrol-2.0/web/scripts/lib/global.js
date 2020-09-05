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

function $( id ) {
  
  return document.getElementById( id );
  
}

function grc_process( data, success, failure ) {
  
  var p_error = $( 'error' );
  
  if ( p_error ) { p_error.outerHTML = ''; }
  
  var p_success = $( 'success' );
  
  if ( p_success ) { p_success.outerHTML = ''; }
  
  var xhttp = get_xhttp();

  if ( xhttp === false ) {
    
    return failure( 'Cannot create XMLHttpRequest object.' );
  
  }

  var raw_data = 'xsrf=' + document.xsrf_token;
    
  for ( var name in data ) {
    
    //if ( raw_data.length ) { raw_data += '&'; }
    
    raw_data += '&' + encodeURIComponent( name ) + '=' +
      encodeURIComponent( data[ name ] );
    
  }

  var url = 'ajax.php';
  var format = 'json';
  //var format = 'text';
  
  xhttp.open( 'POST', url, /* async = */ true );

  xhttp.setRequestHeader(
    "Content-Type", "application/x-www-form-urlencoded"
  );
  xhttp.setRequestHeader( "Content-Length", raw_data.length );
  xhttp.setRequestHeader( "Connection", "close" );

  load_on();

  xhttp.onreadystatechange = function() {
        
    if ( xhttp.readyState !== 4 ) {
    
      load_on();
      
      return;
    
    }

    load_off();

    if ( xhttp.status < 200 || xhttp.status >= 300 ) {

      return failure( xhttp.responseText );

    }

    var response = null;

    switch ( format ) {

      case 'text':

        response = xhttp.responseText;

        break;

      case 'xml':

        response = xhttp.responseXML;

        break;

      case 'object':

        response = xhttp;

        break;

      default : // json

        var json = xhttp.responseText.substr( 6 );
        //var json = xhttp.responseText;

        //alert( json );
        
        response = eval( '(' + json + ')' );
        //response = { 'test' : json };

        break;

    }

    success( response );
    
  }
  
  xhttp.send( raw_data );
  
}

function get_xhttp() {
  
  var xhttp = false;
  
  try {
    
    xhttp = new XMLHttpRequest();
    
  }
  catch ( ex ) {
    
    try {
      
      xhttp = new ActiveXObject( 'MSXML2.XMLHTTP' );
      
    }
    catch ( ex ) {
      
      try {
        
        xhttp = new ActiveXObject( 'Microsoft.XMLHTTP' );
        
      }
      catch ( ex ) {
        
        // give up...
        
      }
    }
  }

  return xhttp;
  
}

function show_error( message ) {
  
  if ( message === null || message === '' ) {
    
    message = 'server process failed.';
    
  }
  
  alert( 'Error: ' + message );
  
  return false;
  
}

function get_value( id, ignore_missing ) {

  ignore_missing =
    typeof ignore_missing === 'undefined' ?
    false :
    ignore_missing;

  var element = $( id );
  
  if ( element === null ) {

    if ( ignore_missing ) { return null; }
    
    return show_error( 'missing element for "' + id + '".' );
    
  }

  if ( element.type === 'checkbox' ) { return element.checked ? 1 : 0; }

  return element.value;
  
}

function require( value, label ) {

  if ( value !== null && value !== '' ) { return true; }
    
  return show_error( label + ' is invalid.' );
  
}

function require_id( value, label, min, max ) {
  
  value = parseInt( value );

  if ( isNaN( value ) ) {
    
    return show_error( label + ' is invalid.' );
    
  }
  
  if ( typeof min !== 'undefined' && value < min ) {
    
    return show_error( label + ' is invalid.' );
    
  }
  
  if ( typeof max !== 'undefined' && value > max ) {
    
    return show_error( label + ' is invalid.' );
    
  }

  return true;
  
}

function require_int( value, label, min, max ) {
  
  if ( typeof min !== 'undefined' && value < min ) {
    
    return show_error( label + ' is too small.' );
    
  }
  
  if ( typeof max !== 'undefined' && value > max ) {
    
    return show_error( label + ' is too large.' );
    
  }

  return true;
  
}

function set_display( id, display ) {
  
	$( id ).style.display = display;
  
}

function set_view( prefix ) {
  
	$( prefix + '_Span' ).className = 'view';
	$( prefix + '_Input' ).className = 'view';
  
}

function set_value( prefix, html, value ) {
  
  if ( typeof value === 'undefined' ) { value = html; }
  
  $( prefix + '_Span' ).innerHTML = html;
	$( prefix + '_Input' ).value = value;
  
}

function process_filter(filter) {
  var id = filter.id;
  id = id.substr( 0, id.length - '_filter'.length );
  var select_id = id;
  var missing_id = id + '_missing';
  filter = filter.value.toLowerCase();
  var select = document.getElementById( select_id );
  var width = select.style.width;
  var missing = document.getElementById( missing_id );
  var first_display = -1;
  var has_one = false;
  for ( var i in select.options ) {
    if ( select.options.hasOwnProperty( i ) ) {
      var option = select.options[ i ];
      var value = option.value.toLowerCase();
      var text = option.text.toLowerCase();
      var display = 'none';
      if (
        value.indexOf( filter ) >= 0 ||
        text.indexOf( filter ) >= 0
      ) {
        if ( first_display === -1 ) { first_display = i; }
        has_one = true;
        display = 'block';
      }
      option.setAttribute( 'style', 'display:' + display );
    }
  }
  select.selectedIndex = first_display;
  if ( has_one ) {
    missing.style.display = 'none';
    select.style.display = 'inline';
    //missing.setAttribute( 'style', 'display:none' );
    //select.setAttribute( 'style', 'display:inline' );
  }
  else {
    //missing.offsetWidth = select.offsetWidth;
    missing.style.display = 'inline-block';
    select.style.display = 'none';
    //select.setAttribute( 'style', 'display:none' );
    //missing.setAttribute( 'style', 'display:inline-block' );
  }
  select.style.width = width;
}

function process_title_filter(filter) {
  var id = filter.id;
  id = id.substr( 0, id.length - '_filter'.length );
  var select_id = id;
  var missing_id = id + '_missing';
  var keywords = filter.value.toLowerCase().split( ' ' );
  var select = document.getElementById( select_id );
  var width = select.style.width;
  var missing = document.getElementById( missing_id );
  var first_display = -1;
  var has_one = false;
  for ( var i in select.options ) {
    if ( select.options.hasOwnProperty( i ) ) {
      var option = select.options[ i ];
      //var value = option.value.toLowerCase();
      var value = option.value.toLowerCase();
      var title = option.title.toLowerCase();
      var display = 'block';
      for ( var index in keywords ) {
        if ( title.indexOf( keywords[ index ] ) < 0 ) {
          display = 'none';
        }
      }
      if ( display === 'block' ) {
        if ( first_display === -1 ) { first_display = i; }
        has_one = true;
      }
      option.setAttribute( 'style', 'display:' + display );
    }
  }
  select.selectedIndex = first_display;
  if ( has_one ) {
    missing.style.display = 'none';
    select.style.display = 'inline';
    //missing.setAttribute( 'style', 'display:none' );
    //select.setAttribute( 'style', 'display:inline' );
  }
  else {
    //missing.offsetWidth = select.offsetWidth;
    missing.style.display = 'inline-block';
    select.style.display = 'none';
    //select.setAttribute( 'style', 'display:none' );
    //missing.setAttribute( 'style', 'display:inline-block' );
  }
  select.style.width = width;
}


function load_on() {
  
	$( 'loadimage' ).style.display = 'block';
  
}

function load_off() {
  
	$( 'loadimage' ).style.display = 'none';
  
}

function set_mode( mode, names, index, feature, id ) {

  var name = names[ index ];

  var element_id = feature + '_' + name + '_' + id + '_span';

  var input = $( element_id );

  if ( input !== null ) {

    input.className = mode;

  }

  var element_id = feature + '_' + name + '_' + id + '_label';

  var input = $( element_id );

  if ( input !== null ) {

    input.className = mode;

  }

  var element_id = feature + '_' + name + '_' + id + '_input';

  var input = $( element_id );

  if ( input.type !== 'checkbox' ) {

    input.className = mode;

  }
  
  var element_id = feature + '_' + name + '_' + id + '_input_filter';

  var input = $( element_id );

  if ( input !== null ) {

    input.className = mode;

  }

  var element_id = feature + '_' + name + '_' + id + '_input_missing';

  var input = $( element_id );

  if ( input !== null ) {

    input.className = mode;

  }  
}

function grc_edit( feature, id, names ) {

  for ( var index in names ) {

    set_mode( 'edit', names, index, feature, id );

  } 
  
	$( feature + '_edit_' + id ).style.display = 'none';
	$( feature + '_delete_' + id ).style.display = 'none';
	$( feature + '_update_' + id ).style.display = 'inline';
	$( feature + '_cancel_edit_' + id ).style.display = 'inline';
  
}

function grc_cancel_edit( feature, id, names ) {

  for ( var index in names ) {

    set_mode( 'view', names, index, feature, id );
    
  } 
  
	$( feature + '_edit_' + id ).style.display = 'inline';
	$( feature + '_delete_' + id ).style.display = 'inline';
	$( feature + '_update_' + id ).style.display = 'none';
	$( feature + '_cancel_edit_' + id ).style.display = 'none';
  
}

function grc_add( feature, names, success_handler ) {

  var data = {
    'action' : feature + '-add'
  };

  for ( var index in names ) {

    var name = names[ index ];
    
    var input = $( feature + '_' + name + '_0_input' );
    
    if ( input.type === 'checkbox' ) {

      if ( input.checked ) { data[ name ] = 'on'; }
      
    }
    else {
    
      data[ name ] = input.value;
  
    }
  } 
  
  grc_process(
    data,
    success_handler,
    failure_handler
  );
  
}

function grc_update( feature, id, names, success_handler ) {

  var data = {
    'action' : feature + '-edit',
    'id' : id
  };
  
  for ( var index in names ) {
    
    var name = names[ index ];
    
    data[ name ] = get_value( feature + '_' + name + '_' + id + '_input' );
    
  }
  
  grc_process(
    data,
    success_handler,
    failure_handler
  );
  //for ( var name in data ) { alert( name + ': ' + data[ name ] ); }
  
}

function grc_delete( feature, id, success_handler ) {

  var data = {
    'action' : feature + '-delete',
    'id' : id
  };

  feature = feature.replace( '_', ' ' );
  
  if ( confirm( 'Are you sure you want to delete this ' + feature + '?' ) ) {
    
    grc_process(
      data,
      success_handler,
      failure_handler
    );

  }
  
}

function grc_delete_feature( feature, id ) {
  
  grc_delete(
    feature,
    id,
    function( data ) {
      load_off();
      var id = feature + '_id_' + data.id;
      var tbody = $( id ).parentNode;
      $( id ).remove();
      var children = count_child_elements( tbody );
      if ( children === 0 ) {
        load_on();
        location.reload();
      }
    }
  );

}

function reload_on_success() {
  
  //var id = document.activeElement.id;
  //alert( location.href );
  load_on();
  
  location.reload();
  
}

function success_handler( data ) {
  
  load_off();
  
}

function failure_handler( error ) {

  load_off();
  
  return show_error( error );
  
}

function count_child_elements( element ) {
  
	var count = 0;
	var length = element.childNodes.length;
  var i = 0;
  
	for ( var i = 0; i < length; i++ ) {
		if( element.childNodes[ i ].nodeType != 3 ){
			count++;
		}
	}
  
	return count;
  
}

function debug_json( json ) {
  
  var html = '';

  html += '<h2>JSON Debug View</h2><table><tbody>';

  for ( var i in json ) {
    html += '<tr><th>' + i + '</th><td>' + json[ i ] + '</td></tr>';
  }
  
  html += '</tbody></table>';
  
  return html;
  
}
