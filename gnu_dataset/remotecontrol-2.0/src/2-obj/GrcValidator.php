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

class GrcValidator {
  
  public static function Create() {
    
    return new GrcValidator();
    
  }
  
  public function sort_col( $col, &$spec ) {
    
    if ( array_key_exists( $col, $spec ) ) { return; }
      
    throw Error(
      'Invalid sort column "%column%".',
      'column', $col
    );
      
  }
  
  public function sort_dir( $dir, $field = 'Value' ) {
    
    if ( $dir === 'asc' || $dir === 'desc' ) { return; }

    $this->invalid_field( $field, $value, 'is not a valid sort direction' );
    
  }
  
  public function html( &$value, $field = 'Value' ) {
    
    $value = trim( strval( $value ) );
    
    if ( is_valid_html( $value ) ) { return; }
    
    $this->invalid_field( $field, $value, 'is not valid HTML' );
    
  }
  
  public function attr( &$value, $field = 'Value' ) {

    $value = trim( strval( $value ) );
    
    if ( is_valid_attr( $value ) ) { return; }

    $this->invalid_field( $field, $value, 'is not a valid HTML attribute' );

  }
  
  public function text( &$value, $field = 'Value' ) {

    $value = trim( strval( $value ) );
    
    if ( is_valid_text( $value ) ) { return; }

    $this->invalid_field( $field, $value, 'is not valid UTF-8' );

  }
  
  public function safe( &$value, $field = 'Value' ) {
    
    $value = trim( strval( $value ) );

    if ( is_valid_safe( $value ) ) { return; }

    $this->invalid_field( $field, $value, 'contains invalid characters' );

  }
  
  public function dbool( &$value, $field = 'Value' ) {
    
    $value = dbool( $value );
        
  }
  
  public function bool( &$value, $field = 'Value' ) {
    
    $value = boolval( $value );
    
  }
  
  public function int(
    &$value,
    $field = 'Value',
    $min = 0,
    $max = INT32_MAX
  ) {

    $value = intval( $value );
    
    if ( $value < $min ) {
      
      $this->invalid_field(
        $field,
        $value,
        'is below the minimum of ' . $min
      );
      
    }
      
    if ( $value > $max ) {
      
      $this->invalid_field(
        $field,
        $value,
        'is above the maximum of ' . $max
      );
      
    }
  }
  
  public function string(
    &$value,
    $field = 'value',
    $min = 0,
    $max = INT32_MAX
  ) {

    $value = trim( strval( $value ) );
    
    $length = mb_strlen( $value );
    
    if ( $length < $min ) {
      
      $this->invalid_field(
        $field,
        $value,
        'is below the minimum length of ' . $min
      );
        
      
    }

    if ( $length > $max ) {
      
      $this->invalid_field(
        $field,
        $value,
        'is above the maximum length of ' . $max
      );
      
    }
    
    $this->text( $value, $field );
    
  }

  public function vector( &$value, $field = 'Vector' ) {
    
    if ( is_array( $value ) ) { return; }
    
    $this->invalid_field( $field, $value, 'is not an array' );
    
  }
  
  public function safe_name(
    &$value,
    $field = 'value',
    $min = 0,
    $max = INT32_MAX
  ) {

    $this->string( $value, $field, $min, $max );
    
    if ( preg_match( '/^[a-z][a-z0-9_]*$/', $value ) ) { return; }
      
    $this->invalid_field( $field, $value, 'contains invalid characters' );
      
  }
  
  public function md5( &$value, $field = 'value' ) {

    $value = trim( strval( $value ) );
    
    if ( is_string( $value ) && is_valid_md5( $value ) ) { return; }

    $this->invalid_field( $field, $value, 'is not in MD5 format' );
          
  }

  public function langtag( &$value, $field = 'Langtag' ) {
    
    if ( array_key_exists( $value, intl()->lang ) ) { return; }
    
    $this->invalid_field( $field, $value );
    
  }
  
  public function next_view( &$value, $field = 'Next View' ) {
    
    static $values = array( 'view', 'edit', 'new', 'home' );
    
    if ( in_array( $value, $values ) ) { return; }
    
    $this->invalid_field( $field, $value );
    
  }
  
  public function group_id( &$value, $field = 'Group ID' ) {
    
    $this->id( $value, 'v2_group', $field, 1, INT8_MAX );
    
  }
  
  public function group_name( &$value, $field = 'Group Name' ) {
    
    $this->string( $value, $field, 1, 32 );
        
  }
  
  public function group_description(
    &$value,
    $field = 'Group Description'
  ) {
    
    $this->string( $value, $field, 0, 128 );
    
  }

  public function location_id( &$value, $field = 'Location ID' ) {
    
    $this->id( $value, 'v2_location', $field, 1, INT16_MAX );
    
  }
  
  public function location_name( &$value, $field = 'Location Name') {
    
    $this->string( $value, $field, 1, 32 );
        
  }

  public function user_id( &$value, $field = 'User ID' ) {
    
    $this->id( $value, 'v2_user', $field, 1, INT16_MAX );
    
  }

  public function thermostat_id( &$value, $field = 'Thermostat ID' ) {
    
    $this->id( $value, 'v2_thermostat', $field, 1, INT16_MAX );
    
  }

  public function thermostat_name( &$value, $field = 'Thermostat Name' ) {
    
    $this->string( $value, $field, 1, 32 );
    
  }
  
  public function thermostat_description(
    &$value,
    $field = 'Thermostat Description'
  ) {
    
    $this->string( $value, $field, 0, 128 );
    
  }

  public function thermostat_host( &$value, $field = 'Thermostat Address' ) {
    
    $value = strval( $value );
    
    $value = str_replace( '/', '', $value );
    $value = str_replace( 'http:', '', $value );
    $value = str_replace( 'https:', '', $value );
    
    $this->string( $value, $field, 1, 128 );
    
  }

  public function thermostat_port( &$value, $field = 'Thermostat Port' ) {
    
    $this->int( $value, $field, 1, UINT16_MAX );
    
  }

  public function thermostat_user(
    &$value,
    $field = 'Thermostat Username'
  ) {
    
    $this->string( $value, $field, 1, 64 );
    
  }
  
  public function thermostat_pass(
    &$value,
    $field = 'Thermostat Password'
  ) {
    
    $this->string( $value, $field, 1, 64 );
    
  }
  
  public function timezone( &$value, $field = 'Time Zone' ) {
    
    $this->string( $value, $field, 1, 32 );
    
    $timezones = timezone_identifiers_list();

    if ( array_search( $value, $timezones ) !== false ) { return; }
    
    $this->invalid_field( $field, $value );
    
  }

  public function hvac_mode( &$value, $field = 'HVAC Mode' ) {
   
    $this->int( $value, $field, 1, 4 );
    
  }
  
  public function fan_mode( &$value, $field = 'Fan Mode' ) {
    
    $this->int( $value, $field, 1, 3 );
    
  }
  
  public function setback_status( &$value, $field = 'Setback Status' ) {

    $this->int( $value, $field, 1, 3 );
    
  }
  
  public function setback_heat( &$value, $field = 'Setback Heat' ) {
    
    $this->temperature( $value, $field );
    
  }
  
  public function setback_cool( &$value, $field = 'Setback Cool' ) {
    
    $this->temperature( $value, $field );
    
  }

  public function occupied_morning_hours(
    &$value,
    $field = 'Occupied Morning Hours'
  ) {

    $this->hours( $value, $field );
    
  }
  
  public function occupied_morning_minutes(
    &$value,
    $field = 'Occupied Morning Minutes'
  ) {

    $this->minutes( $value, $field );
    
  }
  
  public function occupied_day_hours(
    &$value,
    $field = 'Occupied Day Hours'
  ) {

    $this->hours( $value, $field );
    
  }
  
  public function occupied_day_minutes(
    &$value,
    $field = 'Occupied Day Minutes'
  ) {

    $this->minutes( $value, $field );
    
  }
  
  public function occupied_evening_hours(
    &$value,
    $field = 'Occupied Evening Hours'
  ) {

    $this->hours( $value, $field );
    
  }
  
  public function occupied_evening_minutes(
    &$value,
    $field = 'Occupied Evening Minutes'
  ) {

    $this->minutes( $value, $field );
    
  }

  public function occupied_night_hours(
    &$value,
    $field = 'Occupied Night Hours'
  ) {

    $this->hours( $value, $field );
    
  }
  
  public function occupied_night_minutes(
    &$value,
    $field = 'Occupied Night Minutes'
  ) {

    $this->minutes( $value, $field );
    
  }

  public function unoccupied_morning_hours(
    &$value,
    $field = 'Unoccupied Morning Hours'
  ) {

    $this->hours( $value, $field );
    
  }
  
  public function unoccupied_morning_minutes(
    &$value,
    $field = 'Unoccupied Morning Minutes'
  ) {

    $this->minutes( $value, $field );
    
  }
  
  public function unoccupied_day_hours(
    &$value,
    $field = 'Unoccupied Day Hours'
  ) {

    $this->hours( $value, $field );
    
  }
  
  public function unoccupied_day_minutes(
    &$value,
    $field = 'Unoccupied Day Minutes'
  ) {

    $this->minutes( $value, $field );
    
  }
  
  public function unoccupied_evening_hours(
    &$value,
    $field = 'Unoccupied Evening Hours'
  ) {

    $this->hours( $value, $field );
    
  }
  
  public function unoccupied_evening_minutes(
    &$value,
    $field = 'Unoccupied Evening Minutes'
  ) {

    $this->minutes( $value, $field );
    
  }

  public function unoccupied_night_hours(
    &$value,
    $field = 'Unoccupied Night Hours'
  ) {

    $this->hours( $value, $field );
    
  }
  
  public function unoccupied_night_minutes(
    &$value,
    $field = 'Unoccupied Night Minutes'
  ) {

    $this->minutes( $value, $field );
    
  }

  
  
  public function other_morning_hours(
    &$value,
    $field = 'Other Morning Hours'
  ) {

    $this->hours( $value, $field );
    
  }
  
  public function other_morning_minutes(
    &$value,
    $field = 'Other Morning Minutes'
  ) {

    $this->minutes( $value, $field );
    
  }
  
  public function other_day_hours(
    &$value,
    $field = 'Other Day Hours'
  ) {

    $this->hours( $value, $field );
    
  }
  
  public function other_day_minutes(
    &$value,
    $field = 'Other Day Minutes'
  ) {

    $this->minutes( $value, $field );
    
  }
  
  public function other_evening_hours(
    &$value,
    $field = 'Other Evening Hours'
  ) {

    $this->hours( $value, $field );
    
  }
  
  public function other_evening_minutes(
    &$value,
    $field = 'Other Evening Minutes'
  ) {

    $this->minutes( $value, $field );
    
  }

  public function other_night_hours(
    &$value,
    $field = 'Other Night Hours'
  ) {

    $this->hours( $value, $field );
    
  }
  
  public function other_night_minutes(
    &$value,
    $field = 'Other Night Minutes'
  ) {

    $this->minutes( $value, $field );
    
  }
  
  public function occupied_morning_heat( &$heat ) {

    $this->temperature( $heat, 'Occupied Morning Heat' );
    
  }
  
  public function occupied_day_heat( &$heat ) {

    $this->temperature( $heat, 'Occupied Day Heat' );
    
  }
  
  public function occupied_evening_heat( &$heat ) {

    $this->temperature( $heat, 'Occupied Evening Heat' );
    
  }
  
  public function occupied_night_heat( &$heat ) {

    $this->temperature( $heat, 'Occupied Night Heat' );
    
  }
  
  public function unoccupied_morning_heat( &$heat ) {

    $this->temperature( $heat, 'Unoccupied Morning Heat' );
    
  }
  
  public function unoccupied_day_heat( &$heat ) {

    $this->temperature( $heat, 'Unoccupied Day Heat' );
    
  }
  
  public function unoccupied_evening_heat( &$heat ) {

    $this->temperature( $heat, 'Unoccupied Evening Heat' );
    
  }
  
  public function unoccupied_night_heat( &$heat ) {

    $this->temperature( $heat, 'Unoccupied Night Heat' );
    
  }
  
  public function other_morning_heat( &$heat ) {

    $this->temperature( $heat, 'Other Morning Heat' );
    
  }
  
  public function other_day_heat( &$heat ) {

    $this->temperature( $heat, 'Other Day Heat' );
    
  }
  
  public function other_evening_heat( &$heat ) {

    $this->temperature( $heat, 'Other Evening Heat' );
    
  }
  
  public function other_night_heat( &$heat ) {

    $this->temperature( $heat, 'Other Night Heat' );
    
  }

  public function occupied_morning_cool( &$cool ) {

    $this->temperature( $cool, 'Occupied Morning Cool' );
    
  }
  
  public function occupied_day_cool( &$cool ) {

    $this->temperature( $cool, 'Occupied Day Cool' );
    
  }
  
  public function occupied_evening_cool( &$cool ) {

    $this->temperature( $cool, 'Occupied Evening Cool' );
    
  }
  
  public function occupied_night_cool( &$cool ) {

    $this->temperature( $cool, 'Occupied Night Cool' );
    
  }
  
  public function unoccupied_morning_cool( &$cool ) {

    $this->temperature( $cool, 'Unoccupied Morning Cool' );
    
  }
  
  public function unoccupied_day_cool( &$cool ) {

    $this->temperature( $cool, 'Unoccupied Day Cool' );
    
  }
  
  public function unoccupied_evening_cool( &$cool ) {

    $this->temperature( $cool, 'Unoccupied Evening Cool' );
    
  }
  
  public function unoccupied_night_cool( &$cool ) {

    $this->temperature( $cool, 'Unoccupied Night Cool' );
    
  }
  
  public function other_morning_cool( &$cool ) {

    $this->temperature( $cool, 'Other Morning Cool' );
    
  }
  
  public function other_day_cool( &$cool ) {

    $this->temperature( $cool, 'Other Day Cool' );
    
  }
  
  public function other_evening_cool( &$cool ) {

    $this->temperature( $cool, 'Other Evening Cool' );
    
  }
  
  public function other_night_cool( &$cool ) {

    $this->temperature( $cool, 'Other Night Cool' );
    
  }

  public function occupied_morning_fan( &$fan ) {

    $this->fan( $fan, 'Occupied Morning Fan' );
    
  }
  
  public function occupied_day_fan( &$fan ) {

    $this->fan( $fan, 'Occupied Day Fan' );
    
  }
  
  public function occupied_evening_fan( &$fan ) {

    $this->fan( $fan, 'Occupied Evening Fan' );
    
  }
  
  public function occupied_night_fan( &$fan ) {

    $this->fan( $fan, 'Occupied Night Fan' );
    
  }
  
  public function unoccupied_morning_fan( &$fan ) {

    $this->fan( $fan, 'Unoccupied Morning Fan' );
    
  }
  
  public function unoccupied_day_fan( &$fan ) {

    $this->fan( $fan, 'Unoccupied Day Fan' );
    
  }
  
  public function unoccupied_evening_fan( &$fan ) {

    $this->fan( $fan, 'Unoccupied Evening Fan' );
    
  }
  
  public function unoccupied_night_fan( &$fan ) {

    $this->fan( $fan, 'Unoccupied Night Fan' );
    
  }
  
  public function other_morning_fan( &$fan ) {

    $this->fan( $fan, 'Other Morning Fan' );
    
  }
  
  public function other_day_fan( &$fan ) {

    $this->fan( $fan, 'Other Day Fan' );
    
  }
  
  public function other_evening_fan( &$fan ) {

    $this->fan( $fan, 'Other Evening Fan' );
    
  }
  
  public function other_night_fan( &$fan ) {

    $this->fan( $fan, 'Other Night Fan' );
    
  }

  public function hours( &$value, $field = 'Hours' ) {

    $this->int( $value, $field, 0, 23 );
    
  }
  
  public function minutes( &$value, $field = 'Minutes' ) {

    $this->int( $value, $field, 0, 55 );
    
    static $valid = array( 0, 5, 10, 15, 20, 25, 30,
      35, 40, 45, 50, 55 );
    
    if ( in_array( $value, $valid ) ) { return; }
    
    $this->invalid_field( $field, $value );
    
  }
  
  public function thermostat_temperature( $class, &$input ) {
    
    static $periods = array( 'morning', 'day', 'evening', 'night' );

    foreach ( $periods as $period ) {
      
      $heat_key = $class . '_' . $period . '_heat';
      $cool_key = $class . '_' . $period . '_cool';
      
      if ( ( $input[ $heat_key ] + 10 ) > $input[ $cool_key ] ) {

        $field = ucfirst( $class ) . ' ' . ucfirst( $period ) . ' Heat';

        $field = S( $field, CONTEXT_FIELD );

        throw Error(
          '%field% must be less than Cool.',
          'field', $field
        );
        
      }
    }
  }
  
  public function thermostat_start( $class, &$input ) {

    static $periods = array( 'morning', 'day', 'evening', 'night' );
    
    $last = -1;
    
    foreach ( $periods as $period ) {
      
      $key = $class . '_' . $period . '_start';
      
      $value = $input[ $key ];
      
      if ( $value <= $last ) {

        $field = ucfirst( $class ) . ' ' . ucfirst( $period ) . ' Start';

        $field = S( $field, CONTEXT_FIELD );

        throw Error(
          '%field% "%value%" is invalid because it %reason% of %prev%.',
          'field', $field,
          'value', msm212( $value ),
          'reason', 'is earlier or equal to previous time',
          'prev', msm212( $last )
        );
        
      }
      
      $last = $value;
      
    }
  }

  public function sun_class( &$value, $field = 'Sunday Class' ) {
    
    $this->thermostat_class( $value, $field );
    
  }
  
  public function mon_class( &$value, $field = 'Monday Class' ) {
    
    $this->thermostat_class( $value, $field );
    
  }
  
  public function tue_class( &$value, $field = 'Tuesday Class' ) {
    
    $this->thermostat_class( $value, $field );
    
  }
  
  public function wed_class( &$value, $field = 'Wednesday Class' ) {
    
    $this->thermostat_class( $value, $field );
    
  }
  
  public function thu_class( &$value, $field = 'Thursday Class' ) {
    
    $this->thermostat_class( $value, $field );
    
  }
  
  public function fri_class( &$value, $field = 'Friday Class' ) {
    
    $this->thermostat_class( $value, $field );
    
  }
  
  public function sat_class( &$value, $field = 'Saturday Class' ) {
    
    $this->thermostat_class( $value, $field );
    
  }
  
  public function thermostat_class( &$value, $field = 'Thermostat Class' ) {
    
    $this->int( $value, $field, 1, 3 );
    
  }
  
  public function temperature( &$value, $field ) {
    
    $this->int( $value, $field, 400, 990 );
    
  }

  public function fan( &$value, $field ) {

    $this->int( $value, $field, 0, 60 );
    
    static $valid = array( 0, 15, 30, 45, 60 );
    
    if ( in_array( $value, $valid ) ) { return ; }
    
    $this->invalid_field( $field, $value );
    
  }

  public function home_page_action( &$value, $field = 'Home Page Action' ) {
    
    static $valid = array( 'set-thermostats', 'set-time' );
    
    if ( in_array( $value, $valid ) ) { return; }
    
    $this->invalid_field( $field, $value );
    
  }
  
  private function invalid_field(
    $field,
    $value,
    $reason = 'is not valid'
  ) {

    $field = S( $field, CONTEXT_FIELD );

    throw Error(
      '%field% "%value%" is invalid because it %reason%.',
      'field', $field,
      'value', $value,
      'reason', $reason
    );
  }  
  
  private function id(
    &$value,
    $table,
    $field,
    $min = 0,
    $max = INT32_MAX
  ) {
    
    $this->int( $value, $field, $min, $max );

    if ( dal()->id_exists( $table, $value ) ) { return; }
    
    $this->invalid_field( $field, $value, 'does not exist' );
    
  }
}
