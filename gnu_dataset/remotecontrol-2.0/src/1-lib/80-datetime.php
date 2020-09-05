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

date_default_timezone_set( DEFAULT_TIMEZONE );

const USE_OBJECT = false;
const ISO_DATE = 'Y-m-d';
const ISO_TIME = 'H:i:s';
const ISO_DATETIME = 'Y-m-d\TH:i:s';
const ISO_TIMEZONE = 'c';
const DB_DATETIME = 'Y-m-d H:i:s';

global $ONE_DAY;
$ONE_DAY = new DateInterval( 'P1D' );

global $ONE_SECOND;
$ONE_SECOND = new DateInterval( 'PT1S' );

function get_timezone_label( $timezone ) {
  
  if ( ! is_a( $timezone, 'DateTimeZone' ) ) {
    
    $timezone = new DateTimeZone( $timezone );
    
  }
  
  $label = $timezone->getName();
  
  $location = $timezone->getLocation();
  $location = read_a( $location, 'comments' );
  
  if ( $location ) {
    
    $label .= ': ' . $location;
    
  }
  
  return $label;
  
}

function utc_timezone() {
  static $timezone;
  if ( ! $timezone ) { $timezone = new DateTimeZone( 'UTC' ); }
  return $timezone;
}

function timezone( $set = null ) {
  static $timezone;
  if ( $set ) {
    $timezone = is_a( $set, 'DateTimeZone' ) ? $set : new DateTimeZone( $set );
    date_default_timezone_set( $timezone->getName() );
  }
  if ( ! $timezone ) {
    $timezone = new DateTimeZone( defined( 'TIMEZONE' ) ? TIMEZONE : 'UTC' );
    date_default_timezone_set( $timezone->getName() );
  }
  return $timezone;
}

function timezone_offset( $datetime = null ) {
  if ( $datetime === null ) { $datetime = now(); }
  $offset = timezone()->getOffset( $datetime );
  return format_timezone_offset( $offset );
}

function format_timezone_offset( $offset ) {
  $hours = round( abs( $offset ) / 3600 );
  $minutes = round( ( abs( $offset ) - $hours * 3600 ) / 60 );
  $result = ( $offset < 0 ? '-' : '+' )
    . ( $hours < 10 ? '0' : '' ) . $hours
    . ':'
    . ( $minutes < 10 ? '0' : '' ) . $minutes; 
  return $result;  
}

function now() { return new DateTime( null, timezone() ); }
function utc_now() { return new DateTime( null, utc_timezone() ); }

function utc_to_local( $date, $format = USE_OBJECT ) {  
  if ( ! is_a( $date, 'DateTime' ) ) {
    $date = strval( $date );
    if ( $date === '0000-00-00 00:00:00' ) {
      $date = date( 'Y-m-d H:i:s', 0 );
    }
    $date = new DateTime( strval( $date ), utc_timezone() );
  }
  $year = $date->format( 'Y' );
  if ( $year === '-0001' || $year === '0000' ) {
    throw Error(
      'Invalid date "%date%".',
      'date', $date
    );
  }
  $date->setTimezone( timezone() );
  if ( $format === USE_OBJECT ) {
    return $date;
  }
  return $date->format( $format );
}

function local_to_utc( $date, $format = USE_OBJECT ) {
    if ( ! is_a( $date, 'DateTime' ) ) {
    $date = new DateTime( strval( $date ), timezone() );
  }
  $year = $date->format( 'Y' );
  if ( $year === '-0001' || $year === '0000' ) {
    throw Error(
      'Invalid date "%date%".',
      'date', $date
    );
  }
  $date->setTimezone( utc_timezone() );
  if ( $format === USE_OBJECT ) {
    return $date;
  }
  return $date->format( $format );
}

function utc_range(
  $local,
  &$utc_start,
  &$utc_finish,
  $format = ISO_DATETIME,
  $between = false
) {
  
  global $ONE_DAY, $ONE_SECOND;
  
  $date_start = is_a( $local, 'DateTime' ) ?
    clone( $local ) :
    new DateTime( $local, timezone() );

  $date_finish = clone( $date_start );
  $date_finish->add( $ONE_DAY );
  
  $date_start->setTimezone( utc_timezone() );
  $date_finish->setTimezone( utc_timezone() );
  
  if ( $between ) {
    
    $date_finish->sub( $ONE_SECOND );

  }

  if ( $format === USE_OBJECT ) {
    
    $utc_start = $date_start;
    $utc_finish = $date_finish;
    
  }
  else {
    
    $utc_start = $date_start->format( $format );
    $utc_finish = $date_finish->format( $format );
    
  }
}

function date_spec_to_utc_range(
  $spec,
  &$utc_start,
  &$utc_end,
  $format = ISO_TIMEZONE,
  $between = false
) {
  
  global $ONE_SECOND;
  
  $start_local = null;
  $end_local = null;
  
  switch ( strlen( $spec ) ) {

    case 4 :
      
      $start_local = new DateTime( $spec . '-01-01', timezone() );
      $end_local = clone( $start_local );
      $end_local->add( new DateInterval( 'P1Y' ) );
      
      break;
    
    case 7 :
      
      $start_local = new DateTime( $spec . '-01', timezone() );
      $end_local = clone( $start_local );
      $end_local->add( new DateInterval( 'P1M' ) );

      break;
    
    default :
      
      $start_local = new DateTime( $spec, timezone() );
      
      utc_range( $start_local, $utc_start, $utc_end, $format, $between );

      return;
      
  }

  if ( $between ) {
    
    $end_local->sub( $ONE_SECOND );

  }
  
  $utc_start = local_to_utc( $start_local, $format );
  $utc_end = local_to_utc( $end_local, $format );
  
}

function years_from_now( $years ) {
  return str2datetime( '+' . $years . ' years' );
}

function hours_from_now( $hours ) {
  return str2datetime( '+' . $hours . ' hours' );
}

function str2datetime( $str ) {
  return new DateTime(
    gmdate( 'Y-m-d H:i:s',
      strtotime( $str )
    ),
    utc_timezone()
  );  
}

// gets the time zone spec. which is either a named time zone or
// a UTC offset
function get_tz_spec() {
  
  $tz = user()->get_timezone();

  return DB_TZ_NAME ? $tz->getName() : format_utc_offset( $tz );
  
}
