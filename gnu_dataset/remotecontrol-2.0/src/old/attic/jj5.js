
function $( id ) {
  
  return document.getElementById( id );
  
}

function process( data, success, failure ) {
  
  var xhttp = get_xhttp();

  if ( xhttp === false ) { return failure( 'Cannot create XMLHttpRequest object.' ); }

  var raw_data = '';
  
  for ( var name in data ) {
    
    if ( raw_data.length ) { raw_data += '&'; }
    
    raw_data += encodeURIComponent( name ) + '=' + encodeURIComponent( data[ name ] );
    
  }

  var url = 'ajax.php';
  var format = 'json';
  
  xhttp.open( 'POST', url, /* async = */ true );

  xhttp.setRequestHeader( "Content-Type", "application/x-www-form-urlencoded" );
  xhttp.setRequestHeader( "Content-Length", raw_data.length );
  xhttp.setRequestHeader( "Connection", "close" );

  $( 'loadimage' ).style.display = 'block';

  xhttp.onreadystatechange = function() {
        
    if ( xhttp.readyState !== 4 ) {
    
      $( 'loadimage' ).style.display = 'block';
      
      return;
    
    }

    $( 'loadimage' ).style.display = 'none';

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

        response = eval( '(' + xhttp.responseText + ')' );

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
  
  alert( 'Error: ' + message );
  
  return false;
  
}

function get_value( id, ignore_missing ) {

  var element = $( id );
  
  if ( element === null ) {
    
    if ( ignore_missing ) { return null; }
    
    return show_error( 'missing element for "' + id + '".' );
    
  }

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

function add_thermostat() {
  
	var name = get_value( 'Name_0_Input' );
	var description = get_value( 'Description_0_Input' );
	var host = get_value( 'Host_0_Input' );
	var port = get_value( 'Port_0_Input' );
  var timezone = get_value( 'TimeZone_0_Input' );
	var group_id = get_value( 'Group_0_Input' );

  if (
    require( name, 'Custom Name' ) &&
    //require( description, 'Description' ) &&
    require( host, 'Domain Name/IP' ) &&
    require_int( port, 'Port', 1 ) &&
    require( timezone, 'TimeZone' ) &&
    require_id( group_id, 'Group', 1 )
  ) {

    process( {
      'action' : 'add_thermostat',
      'name' : name,
      'description' : description,
      'host' : host,
      'port' : port,
      'timezone' : timezone,
      'group_id' : group_id
    },
    function( json ) {

      for ( var i in json ) { alert( i + ': ' + json[ i ] ); }

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
  var timezone = get_value( 'TimeZoneOffset_' + id + '_Input' );
	var group_id = get_value( 'Group_' + id + '_Input' );

  if (
    require( name, 'Custom Name' ) &&
    //require( description, 'Description' ) &&
    require( host, 'Domain Name/IP' ) &&
    require( port, 'Port' ) &&
    require( timezone, 'TimeZone' ) &&
    require( group_id, 'Group' )
  ) {
  
    process( {
      'action' : 'update_thermostat',
      'thermostat_id' : id,
      'name' : name,
      'description' : description,
      'host' : host,
      'port' : port,
      'timezone' : timezone,
      'group_id' : group_id
    },
    function( json ) {

      set_view( 'Name_' + id );
      set_view( 'Description_' + id );
      set_view( 'Host_' + id );
      set_view( 'Port_' + id );
      set_view( 'TimeZone_' + id );
      set_view( 'Group_' + id );
      
      set_display( 'Edit_' + id, 'inline' );
      set_display( 'Update_' + id, 'none' );
      set_display( 'Cancel_' + id, 'none' );

      set_value( 'Name_' + id, json.name );
      set_value( 'Description_' + id, json.description );
      set_value( 'Host_' + id, json.host );
      set_value( 'Port_' + id, json.port );
      set_value( 'TimeZone_' + id, json.timezone );
      set_value( 'Group_' + id, json.group_id );

    },
    show_error
    );

  }
}
