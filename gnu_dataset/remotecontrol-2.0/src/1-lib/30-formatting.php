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

function to_string( $value, $strict = true ) {
  
  if ( is_array( $value ) ) {
    
    $result = array();
    
    foreach ( $value as &$inner_value ) {
      
      $result[] = to_string( $inner_value );
      
    }
    
    $value = implode( ', ', $result );
    
  }
    
  if ( $value === null ) { return ''; }

  if ( is_bool( $value ) ) { return format_bool( $value ); }

  if ( is_int( $value ) ) { return format_int( $value ); }
  
  if ( is_float( $value ) ) { return format_float( $value ); }
  
  if ( is_date( $value ) ) { return format_date( $value ); }
  
  if ( is_time( $value ) ) { return format_time( $value ); }
  
  if ( is_datetime( $value ) ) { return format_datetime( $value ); }
  
  try {

    $value = strval( $value );
    
  }
  catch ( Exception $ex ) {

    if ( is_object( $value ) ) { return get_class( $value ); }
      
    try {

      $value = serialize( $value );
      
    }
    catch ( Exception $ex ) {
      
      //$value = msg()->get( MSG__LIB__FORMAT_VALUE__INVALID );
      if ( function_exists( 'T' ) ) {
  
        $value = T( '**invalid**' );
        
      }
      else {
        
        $value = '**invalid**';
        
      }
    }    
  }

  if ( ! $strict ) {
 
    if ( preg_match( '/^[+\-]?[0-9,]+$/', $value ) ) {
    
      return format_int( $value );

    }

    if ( preg_match( '/^[+\-]?[0-9,]+\.?[0-9]*$/', $value ) ) {

      return format_float( $value );

    }
  }
  
  return $value;
  
}

function truncate_html( $html, $max_length = TRUNCATE_LENGTH ) {

  $html = to_string( $html );

  $html = strip_tags( $html );
  
  $text = html_decode( $html );
  
  $text = truncate_text( $text, $max_length );
  
  return aenc( $text );
  
}

function truncate_text( $text, $max_length = TRUNCATE_LENGTH ) {
  
  $text = to_string( $text );
  
  $text = trim( $text );

  $input_length = mb_strlen( $text );
  
  if ( $input_length <= $max_length ) {

    return $text;
    
  }
  
  $parts = preg_split( '/\b/', $text );
  $parts_count = count( $parts );
  
  $result_length = 0;
  
  $front = array();
  $back = array();

  for ( $i = 0; $i < $parts_count; $i++ ) {
    
    $index = $i;
          
    $part = $parts[ $index ];

    $new_length = $result_length + mb_strlen( $part );
        
    if ( $new_length > $max_length ) {
      
      $front[] = mb_substr( $part, 0, $max_length );
      
      break;
      
    }

    $result_length = $new_length;
    
    $front[ $index ] = $part;          

    $index = $parts_count - $i - 1;

    $part = $parts[ $index ];

    $new_length = $result_length + mb_strlen( $part );
        
    if ( $new_length > $max_length ) {
      
      break;
      
    }

    $result_length = $new_length;
    
    $back[ $index ] = $part;          
    
  }
  
  $back = array_reverse( $back );
  
  $front = implode( '', $front );
  $back = implode( '', $back );
  
  return $front . '...' . $back;
  
  exit;

  foreach ( $parts as $part ) {
    
    $result_length = mb_strlen( $result );
    
    $this_length = mb_strlen( $part );
    
    $next_length = $result_length + $this_length;
    
    if ( $next_length > $length ) {
      
      if ( $result === '' ) {
      
        return mb_substr( $text, 0, $length, UTF8_ENCODING ) . '...';
        
      }
      else {
        
        $len = mb_strlen( $result, UTF8_ENCODING );
        
        $bit = mb_substr(
          $part,
          0,
          $length - mb_strlen( $result, UTF8_ENCODING),
          UTF8_ENCODING
        );
                
        return $result . $bit . '...';
  
      }
    }
    
    if ( $result_length === 0 ) {
      
      $result = $part;
      
    }
    else {
      
      $result .= $part;
      
    }
  }
  
  return $result . '...';
  
}

// TODO: regionalize formatting functions...

function format_position( $pos ) {

  $text = strval( $pos );
  
  $len = strlen( $text );
  
  if ( $len < 1 ) { return ''; }
  
  $last_digit = substr( $text, $len - 1 );

  if ( $last_digit === '1' ) { return $text . 'st'; }
  
  if ( $last_digit === '2' ) { return $text . 'nd'; }
  
  if ( $last_digit === '3' ) { return $text . 'rd'; }

  return $text . 'th';
  
}

function format_value( $value, $units = '' ) {

  $value = to_string( $value );
  
  $parts = explode( ' to ', $value );
  
  if ( count( $parts ) === 2 ) {
    
    return T(
      '%start% to %finish%' . $units,
      'start', format_value( $parts[ 0 ] ),
      'finish', format_value( $parts[ 1 ] )
    );
    
  }
  
  if ( strlen( $units ) ) {
    
    return T( '%value%' . $units, 'value', $value );
    
  }
  
  return $value;
  
}

function format_bool( $value ) {

  if ( boolval( $value ) ) {
    
    return S( 'true' );
    
  }

  return S( 'false' );

}

function format_number( $value ) {
  
  if ( is_int( $value ) ) {
    
    return format_int( $value );
    
  }
  
  return format_float( $value );
  
}

function format_int( $value ) {
  
  if ( $value === null || $value === '' ) { return ''; }
  
  return number_format( intval( $value ) );
  
}

function format_float( $value ) {
  
  if ( $value === null || $value === '' ) { return ''; }
  
  return number_format( floatval( $value ), 2 );
  
}

function format_date( $value ) {
  
  return utc_to_local( $value, ISO_DATE );
  
}

function format_time( $value ) {
  
  return utc_to_local( $value, ISO_TIME );
  
}

function format_datetime( $value ) {
  
  return utc_to_local( $value, DB_DATETIME );
  
}

function format_seconds( $s ) {

  $d = intval( $s / 86400 );
  $s -= $d * 86400;

  $h = intval( $s / 3600 );
  $s -= $h * 3600;

  $m = intval( $s / 60 );
  $s -= $m * 60;

  $s = round( $s );
  
  $str = '';
  
  if ( $d ) $str .= $d . 'd ';
  if ( $h ) $str .= $h . 'h ';
  if ( $m ) $str .= $m . 'm ';
  if ( $s ) $str .= $s . 's';

  return trim( $str );
  
}

function yes_no( $value ) {
  
  return $value ? 'yes' : 'no';
  
}

function format_dd( $val ) {
  
  $result = strval( $val );
  
  if ( strlen( $result ) === 1 ) { $result = '0' . $result; }
  
  return $result;
  
}

// minutes since midnight to 12 hour time:
function msm212( $msm ) {

  $ampm = 'am';
  
  if ( $msm > ( 12 * 60 ) ) {
    
    $ampm = 'pm';
    
    $msm -= ( 12 * 60 );
    
  }
  
  $hours = floor( $msm / 60 );
  $minutes = format_dd( $msm % 60 );

  if ( $hours == 0 ) { $hours = 12; }
  
  return $hours . ':' . $minutes . ' ' . $ampm;
  
}

// minutes since midnight to 24 hour time:
function msm224( $msm ) {
  
  $hours = floor( $msm / 60 );
  $minutes = format_dd( $msm % 60 );

  return $hours . ':' . $minutes;
  
}

function format_1dp( $value ) {
  
  return floor( $value * 10 ) / 10;
  
}

function format_grc_field( $field, $value ) {

  if ( $value === null || strlen( $value ) === 0 ) { return ''; }

  if ( $field === 'hvac_mode' ) {
    
    static $valid_hvac_modes = array(
      '',
      'Off',
      'Heat',
      'Cool',
      'Auto'
    );

    return format_enum_string( $valid_hvac_modes, $value );
    
  }
  
  if ( $field === 'fan_mode' ) {
    
    static $valid_fan_modes = array(
      '',
      'Auto',
      'On',
      'Schedule'
    );

    return format_enum_string( $valid_fan_modes, $value );
    
  }
  
  if ( $field === 'setback_status' ) {
    
    static $valid_setback_status_modes = array(
      '',
      'Normal',
      'Hold',
      'Override'
    );

    return format_enum_string( $valid_setback_status_modes, $value );
    
  }

  if ( preg_match( '/_fan$/', $field ) ) {

    // Class/Period
    static $valid_cp_fan_modes = array(
      '0' => 'Off',
      '15' => '15',
      '30' => '30',
      '45' => '45',
      '60' => 'On'
    );
    
    return format_enum_string( $valid_cp_fan_modes, $value );
    
  } 
  
  if ( preg_match( '/(_heat)|(_cool)$/', $field ) ) {

    // rely on correct format in DB...
    
    if ( DEFAULT_SCALE === FAHRENHEIT ) {
      
      return $value . '&deg;F';
      
    }

    return $value . '&degC';
  
  }
  
  /* // handled on the way in now:
  if ( preg_match( '/_start$/', $field ) ) {
  
    return msm212( $value );
  
  }
  */
  
  if ( preg_match( '/_class$/', $field ) ) {
  
    static $valid_classes = array(
      '',
      'Occupied',
      'Unoccupied',
      'Other'
    );

    return format_enum_string( $valid_classes, $value );
    
  } 

  // TODO: other formats?
  
  return $value;
  
}

function format_enum_string( &$valid, $value ) {
  
  $label = read_a( $valid, $value );
  
  if ( $label ) {
    
    return $value . ': ' . H( $label );
    
  }
  
  return $value;
  
}

function format_utc_offset( $offset ) {
  
  if ( is_a( $offset, 'DateTimeZone' ) ) {
    
    $offset = $offset->getOffset( new DateTime() );

  }
  
  $hours = $offset / 60 / 60;
  $minutes = $offset % ( 60 * 60 );
  
  $sign = $hours >= 0 ? '+' : '';
  
  return $sign . $hours . ':' . format_dd( $minutes );
  
}
