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

class GrcBomThermostat {

  //public $cache;

  const NOT_PRESENT = '0';
  const DISABLED = '1';
  const ENABLED = '2';
  
  public function __construct() {
    
    //$this->cache = array();
    
  }
  
  public static function Create( $bom ) {
    
    return new GrcBomThermostat();
    
  }
  
  public function &sensor_settings() {

    $data = array();
    
    $data[ 'host' ] = read_r( 'host' );
    $data[ 'port' ] = read_r( 'port' );
    $data[ 'user' ] = read_r( 'user' );
    $data[ 'pass' ] = read_r( 'pass' );
    
    //validate( 'thermostat_host', 'host', $data );
    validator()->thermostat_host( $data[ 'host' ] );
    
    validate( 'thermostat_port', 'port', $data );
    validate( 'thermostat_user', 'user', $data );
    validate( 'thermostat_pass', 'pass', $data );

    $this->read_device( $data, false );
    
    //var_dump( $data ); die;
    
    return $data;
    
  }

  public function &http_read( $id, &$default = null ) {

    verify( true );

    if ( $default === null ) { $default = array(); }
    
    $data = array();
    
    $data[ 'id' ] = $id;
    
    read_iar( $default, 'host', $data );
    read_iar( $default, 'port', $data );
    read_iar( $default, 'user', $data );
    read_iar( $default, 'pass', $data );
    read_iar( $default, 'user_id', $data );
    read_iar( $default, 'name', $data );
    read_iar( $default, 'description', $data );
    read_iar( $default, 'location_id', $data );
    read_iar( $default, 'group_id', $data );
    read_iar( $default, 'user_id', $data );
    read_iar( $default, 'next', $data );

    read_iar( $default, 'device_name', $data );
    read_iar( $default, 'site_name', $data );
    read_iar( $default, 'model_number', $data );
    read_iar( $default, 'temperature_scale', $data );
    read_iar( $default, 'local_sensor_state', $data );
    read_iar( $default, 'remote_sensor_1_state', $data );
    read_iar( $default, 'remote_sensor_2_state', $data );
    read_iar( $default, 'local_sensor_temp', $data );
    read_iar( $default, 'remote_sensor_1_temp', $data );
    read_iar( $default, 'remote_sensor_2_temp', $data );
    read_iar( $default, 'local_sensor_average', $data );
    read_iar( $default, 'remote_sensor_1_average', $data );
    read_iar( $default, 'remote_sensor_2_average', $data );
    read_iar( $default, 'remote_sensor_1_correction', $data );
    read_iar( $default, 'remote_sensor_2_correction', $data );
    
    return $data;
    
  }

  public function validate( &$data ) {

    if ( $data[ 'id' ] !== 'new' ) {
      
      validate( 'thermostat_id', 'id', $data );
      
    }
    
    validate( 'thermostat_host', 'host', $data );
    validate( 'thermostat_port', 'port', $data );
    validate( 'thermostat_user', 'user', $data );
    validate( 'thermostat_pass', 'pass', $data );
    validate( 'user_id', 'user_id', $data );
    validate( 'thermostat_name', 'name', $data );
    validate( 'thermostat_description', 'description', $data );
    validate( 'location_id', 'location_id', $data );
    validate( 'group_id', 'group_id', $data );
    validate( 'user_id', 'user_id', $data );
    validate( 'next_view', 'next', $data );
    
  }
  
  public function http_insert( &$data ) {
    
   verify( is_admin() );

   //$data[ 'user_id' ] = user()->id;
    
    $data[ 'id' ] = dal()->thermostat->insert( $data );

    $this->write_sensor_settings( $data );
    
    return $data;
    
  }
  
  public function http_update( &$data ) {
    
    verify( true );

    /*
    $user_id = read_a( $data, 'user_id' );
    
    if ( $user_id != user()->id ) {
      
      throw Error(
        'Cannot edit thermostat for user "%user%".',
        'user', $user_id
      );
      
    }
    */
    
    $result = dal()->thermostat->update( $data );

    $this->write_sensor_settings( $data );
    
    return $result;
    
  }
  
  // TODO: get rid of this...
  public function &http_add() {

    verify( is_admin() );

    $name = read_r( 'name' );
    $description = read_r( 'description' );
    $host = read_r( 'host' );
    $port = read_r( 'port' );
    $location_id = read_r( 'location_id' );
    $group_id = read_r( 'group_id' );
    //$user_id = user()->id;
    $user_id = read_r( 'user_id' );
    
    $id = dal()->thermostat->insert(
      $name,
      $description,
      $host,
      $port,
      $location_id,
      $group_id,
      $user_id
    );
    
    $result = array(
      'id' => $id,
      'name' => $name,
      'description' => $description,
      'host' => $host,
      'port' => $port,
      'location_id' => $location_id,
      'group_id' => $group_id,
      'user_id' => $user_id
    );
    
    //$this->read_device( $result );
    
    return $result;
    
  }  
  
  public function &http_delete() {

    verify( is_admin() );
    
    //$user_id = user()->id;

    $id = read_r( 'id' );
    
    $id = dal()->thermostat->soft_delete(
      //$user_id,
      $id
    );
    
    $result = array(
      'id' => $id,
    );
    
    return $result;
    
  }  
  
  public function &http_undelete() {

    verify( is_admin() );
    
    $id = read_r( 'id' );
    
    $id = dal()->thermostat->undelete( $id );
    
    $result = array(
      'id' => $id,
    );
    
    return $result;
    
  }  
  
  public function &get_all(
    &$count = -1,
    $page = null,
    $sort = null
  ) {

    $data = dal()->thermostat->get_all(
      $count,
      $page,
      $sort
    );

    $this->load_devices( $data );
    
    return $data;
    
  }
  
  public function &get_for_user(
    &$count = -1,
    $page = null,
    $sort = null
  ) {

    $user_id = user()->id;
    
    $data = dal()->thermostat->get_for_user_id(
      $user_id,
      $count,
      $page,
      $sort
    );

    $this->load_devices( $data );
    
    return $data;
    
  }
  
  public function &get_by_id() {
    
    $id = read_r( 'id' );
    
    $record = dal()->thermostat->get_by_id(
      $id
    );

    $this->load_device( $record );
      
    return $record;
    
  }

  public function &get_by_user_and_id() {
    
    $id = read_r( 'id' );
    
    $user_id = user()->id;
    
    $record = dal()->thermostat->get_by_user_and_id(
      $user_id,
      $id
    );

    $this->load_device( $record );
      
    return $record;
    
  }

  public function read_device( &$device, $use_cache = true ) {
    
    $host = $device[ 'host' ];
    $port = $device[ 'port' ];
    $user = $device[ 'user' ];
    $pass = $device[ 'pass' ];
    
    $key = "$host:$port";
    
    //if ( $use_cache && array_key_exists( $key, $this->cache ) ) {
    if ( false ) {
      
      $data = &$this->cache[ $key ];
      
    }
    else {

      $id = read_a( $device, 'id' );
      
      try {
        
        //if ( $id ) { $this->write_time( $id ); }

      }
      catch ( Exception $ex ) {

        dal()->exception->log( $ex );
        
      }
      
      $retry = false;
      
      retry:
      
      $attempt = 0;

      while ( ++$attempt < READ_ATTEMPTS ) {

        try {
      
          $ch = curl_init( "http://$host:$port/get");

          curl_setopt( $ch, CURLOPT_POST, true );
          curl_setopt( $ch, CURLOPT_TIMEOUT, NET_TIMEOUT );
          curl_setopt( $ch, CURLOPT_RETURNTRANSFER, true );
          curl_setopt( $ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC );
          curl_setopt( $ch, CURLOPT_USERPWD, "$user:$pass" );    

          $params =
            'OID1.2='           . // DeviceName
            '&OID1.10.9='       . // SiteName
            '&OID2.7.1='        . // ModelNumber
            '&OID2.5.1='        . // SecondsSinceEpoch
            '&OID4.1.21='       . // TemperatureScale
            '&OID4.3.6.1='      . // LocalSensorState
            '&OID4.3.6.2='      . // RemoteSensor1State
            '&OID4.3.6.3='      . // RemoteSensor2State
            '&OID4.3.2.1='      . // LocalSensorTemp
            '&OID4.3.2.2='      . // RemoteSensor1Temp
            '&OID4.3.2.3='      . // RemoteSensor2Temp
            '&OID4.3.8.1='      . // LocalSensorAverage
            '&OID4.3.8.2='      . // RemoteSensor1Average
            '&OID4.3.8.3='      . // RemoteSensor2Average
            '&OID4.3.4.2='      . // RemoteSensor1Correction
            '&OID4.3.4.3='      . // RemoteSensor2Correction
            '&OID4.1.1='        . // HvacMode
            '&OID4.1.3='        . // FanMode
            '&OID4.1.9='        . // SetbackStatus
            '&OID4.1.5='        . // SetbackHeat
            '&OID4.1.6='        . // SetbackCool
            '&OID4.4.1.3.1.1='  . // OccupiedMorningStart
            '&OID4.4.1.3.1.2='  . // OccupiedDayStart
            '&OID4.4.1.3.1.3='  . // OccupiedEveningStart
            '&OID4.4.1.3.1.4='  . // OccupiedNightStart
            '&OID4.4.1.3.2.1='  . // UnoccupiedMorningStart
            '&OID4.4.1.3.2.2='  . // UnoccupiedDayStart
            '&OID4.4.1.3.2.3='  . // UnoccupiedEveningStart
            '&OID4.4.1.3.2.4='  . // UnoccupiedNightStart
            '&OID4.4.1.3.3.1='  . // OtherMorningStart
            '&OID4.4.1.3.3.2='  . // OtherDayStart
            '&OID4.4.1.3.3.3='  . // OtherEveningStart
            '&OID4.4.1.3.3.4='  . // OtherNightStart
            '&OID4.4.1.4.1.1='  . // OccupiedMorningHeat
            '&OID4.4.1.4.1.2='  . // OccupiedDayHeat
            '&OID4.4.1.4.1.3='  . // OccupiedEveningHeat
            '&OID4.4.1.4.1.4='  . // OccupiedNightHeat
            '&OID4.4.1.4.2.1='  . // UnoccupiedMorningHeat
            '&OID4.4.1.4.2.2='  . // UnoccupiedDayHeat
            '&OID4.4.1.4.2.3='  . // UnoccupiedEveningHeat
            '&OID4.4.1.4.2.4='  . // UnoccupiedNightHeat
            '&OID4.4.1.4.3.1='  . // OtherMorningHeat
            '&OID4.4.1.4.3.2='  . // OtherDayHeat
            '&OID4.4.1.4.3.3='  . // OtherEveningHeat
            '&OID4.4.1.4.3.4='  . // OtherNightHeat
            '&OID4.4.1.5.1.1='  . // OccupiedMorningCool
            '&OID4.4.1.5.1.2='  . // OccupiedDayCool
            '&OID4.4.1.5.1.3='  . // OccupiedEveningCool
            '&OID4.4.1.5.1.4='  . // OccupiedNightCool
            '&OID4.4.1.5.2.1='  . // UnoccupiedMorningCool
            '&OID4.4.1.5.2.2='  . // UnoccupiedDayCool
            '&OID4.4.1.5.2.3='  . // UnoccupiedEveningCool
            '&OID4.4.1.5.2.4='  . // UnoccupiedNightCool
            '&OID4.4.1.5.3.1='  . // OtherMorningCool
            '&OID4.4.1.5.3.2='  . // OtherDayCool
            '&OID4.4.1.5.3.3='  . // OtherEveningCool
            '&OID4.4.1.5.3.4='  . // OtherNightCool
            '&OID4.4.1.6.1.1='  . // OccupiedMorningFan
            '&OID4.4.1.6.1.2='  . // OccupiedDayFan
            '&OID4.4.1.6.1.3='  . // OccupiedEveningFan
            '&OID4.4.1.6.1.4='  . // OccupiedNightFan
            '&OID4.4.1.6.2.1='  . // UnoccupiedMorningFan
            '&OID4.4.1.6.2.2='  . // UnoccupiedDayFan
            '&OID4.4.1.6.2.3='  . // UnoccupiedEveningFan
            '&OID4.4.1.6.2.4='  . // UnoccupiedNightFan
            '&OID4.4.1.6.3.1='  . // OtherMorningFan
            '&OID4.4.1.6.3.2='  . // OtherDayFan
            '&OID4.4.1.6.3.3='  . // OtherEveningFan
            '&OID4.4.1.6.3.4='  . // OtherNightFan
            '&OID4.4.3.2.1='    . // SunClass
            '&OID4.4.3.2.2='    . // MonClass
            '&OID4.4.3.2.3='    . // TueClass
            '&OID4.4.3.2.4='    . // WedClass
            '&OID4.4.3.2.5='    . // ThuClass
            '&OID4.4.3.2.6='    . // FriClass
            '&OID4.4.3.2.7='    . // SatClass
            '';

          curl_setopt( $ch, CURLOPT_POSTFIELDS, $params );

          $response = $this->parse_success( curl_exec( $ch ) );

          if ( $response === false ) {

            $error = curl_error( $ch );
            
            curl_close( $ch );
            
            throw Error(
              'Error "%error%" reading from %host%:%port%.',
              'error', $error,
              'host', $host,
              'port', $port
            );
            
          }

          curl_close( $ch );
          
          parse_str( $response, $data );

          //$this->cache[ $key ] = &$data;

          break;
          
        }
        catch ( Exception $ex ) {
          
          try { curl_close( $ch ); } catch ( Exception $ignore ) { ; }

          dal()->exception->log( $ex );
          
        }
      }

      if ( $attempt === READ_ATTEMPTS ) {

        if ( $retry === false && RECOVER_FROM_FACTORY_RESET ) {
          
          self::RecoverFromFactoryReset( $host, $port, $user, $pass );
          
          $retry = true;
          
          goto retry;
          
        }
        
        throw Error(
          'Could not read %host%:%port% after %num% attempts.',
          'host', $host,
          'port', $port,
          'num', $attempt
        );

      }
    }

    $device[ 'device_name' ]        = read_a( $data, 'OID1_2' );
    //$device[ 'device_name' ] = serialize( $data );
    $device[ 'site_name' ]          = read_a( $data, 'OID1_10_9' );
    $device[ 'model_number' ]       = read_a( $data, 'OID2_7_1' );
    
    $device[ 'time' ] = gmdate(
      'D, d M y H:i:s',
      intval( read_a( $data, 'OID2_5_1' ) )
    );
    
    $device[ 'temperature_scale' ]  = read_a( $data, 'OID4_1_21' );

    $local_sensor_state     = read_a( $data, 'OID4_3_6_1' );
    $remote_sensor_1_state  = read_a( $data, 'OID4_3_6_2' );
    $remote_sensor_2_state  = read_a( $data, 'OID4_3_6_3' );

    $this->sanitize_sensor_state( $local_sensor_state );
    $this->sanitize_sensor_state( $remote_sensor_1_state );
    $this->sanitize_sensor_state( $remote_sensor_2_state );
    
    $device[ 'local_sensor_state' ]     = $local_sensor_state;
    $device[ 'remote_sensor_1_state' ]  = $remote_sensor_1_state;
    $device[ 'remote_sensor_2_state' ]  = $remote_sensor_2_state;
    
    $local_sensor_temp     = read_a( $data, 'OID4_3_2_1' );
    $remote_sensor_1_temp  = read_a( $data, 'OID4_3_2_2' );
    $remote_sensor_2_temp  = read_a( $data, 'OID4_3_2_3' );

    $this->sanitize_sensor_temp( $local_sensor_temp );
    $this->sanitize_sensor_temp( $remote_sensor_1_temp );
    $this->sanitize_sensor_temp( $remote_sensor_2_temp );
    
    $device[ 'local_sensor_temp' ]     = $local_sensor_temp;
    $device[ 'remote_sensor_1_temp' ]  = $remote_sensor_1_temp;
    $device[ 'remote_sensor_2_temp' ]  = $remote_sensor_2_temp;
    
    $local_sensor_average     = read_a( $data, 'OID4_3_8_1' );
    $remote_sensor_1_average  = read_a( $data, 'OID4_3_8_2' );
    $remote_sensor_2_average  = read_a( $data, 'OID4_3_8_3' );

    $this->sanitize_sensor_average( $local_sensor_average );
    $this->sanitize_sensor_average( $remote_sensor_1_average );
    $this->sanitize_sensor_average( $remote_sensor_2_average );
    
    $device[ 'local_sensor_average' ]     = $local_sensor_average;
    $device[ 'remote_sensor_1_average' ]  = $remote_sensor_1_average;
    $device[ 'remote_sensor_2_average' ]  = $remote_sensor_2_average;
    
    $remote_sensor_1_correction  = read_a( $data, 'OID4_3_4_2' );
    $remote_sensor_2_correction  = read_a( $data, 'OID4_3_4_3' );

    $this->sanitize_sensor_correction( $remote_sensor_1_correction );
    $this->sanitize_sensor_correction( $remote_sensor_2_correction );
    
    $device[ 'remote_sensor_1_correction' ]  = $remote_sensor_1_correction;
    $device[ 'remote_sensor_2_correction' ]  = $remote_sensor_2_correction;

    $hvac_mode      = read_a( $data, 'OID4_1_1' );
    $fan_mode       = read_a( $data, 'OID4_1_3' );
    $setback_status = read_a( $data, 'OID4_1_9' );
    $setback_heat   = read_a( $data, 'OID4_1_5' );
    $setback_cool   = read_a( $data, 'OID4_1_6' );

    $device[ 'hvac_mode' ]      = $hvac_mode;
    $device[ 'fan_mode' ]       = $fan_mode;
    $device[ 'setback_status' ] = $setback_status;
    $device[ 'setback_heat' ]   = $setback_heat;
    $device[ 'setback_cool' ]   = $setback_cool;
    
    $occupied_morning_start   = read_a( $data, 'OID4_4_1_3_1_1' );
    $occupied_day_start       = read_a( $data, 'OID4_4_1_3_1_2' );
    $occupied_evening_start   = read_a( $data, 'OID4_4_1_3_1_3' );
    $occupied_night_start     = read_a( $data, 'OID4_4_1_3_1_4' );
    $unoccupied_morning_start = read_a( $data, 'OID4_4_1_3_2_1' );
    $unoccupied_day_start     = read_a( $data, 'OID4_4_1_3_2_2' );
    $unoccupied_evening_start = read_a( $data, 'OID4_4_1_3_2_3' );
    $unoccupied_night_start   = read_a( $data, 'OID4_4_1_3_2_4' );
    $other_morning_start      = read_a( $data, 'OID4_4_1_3_3_1' );
    $other_day_start          = read_a( $data, 'OID4_4_1_3_3_2' );
    $other_evening_start      = read_a( $data, 'OID4_4_1_3_3_3' );
    $other_night_start        = read_a( $data, 'OID4_4_1_3_3_4' );
    
    $device[ 'occupied_morning_start' ]   = $occupied_morning_start;
    $device[ 'occupied_day_start' ]       = $occupied_day_start;
    $device[ 'occupied_evening_start' ]   = $occupied_evening_start;
    $device[ 'occupied_night_start' ]     = $occupied_night_start;
    $device[ 'unoccupied_morning_start' ] = $unoccupied_morning_start;
    $device[ 'unoccupied_day_start' ]     = $unoccupied_day_start;
    $device[ 'unoccupied_evening_start' ] = $unoccupied_evening_start;
    $device[ 'unoccupied_night_start' ]   = $unoccupied_night_start;
    $device[ 'other_morning_start' ]      = $other_morning_start;
    $device[ 'other_day_start' ]          = $other_day_start;
    $device[ 'other_evening_start' ]      = $other_evening_start;
    $device[ 'other_night_start' ]        = $other_night_start;
    
    $occupied_morning_heat    = read_a( $data, 'OID4_4_1_4_1_1' );
    $occupied_day_heat        = read_a( $data, 'OID4_4_1_4_1_2' );
    $occupied_evening_heat    = read_a( $data, 'OID4_4_1_4_1_3' );
    $occupied_night_heat      = read_a( $data, 'OID4_4_1_4_1_4' );
    $unoccupied_morning_heat  = read_a( $data, 'OID4_4_1_4_2_1' );
    $unoccupied_day_heat      = read_a( $data, 'OID4_4_1_4_2_2' );
    $unoccupied_evening_heat  = read_a( $data, 'OID4_4_1_4_2_3' );
    $unoccupied_night_heat    = read_a( $data, 'OID4_4_1_4_2_4' );
    $other_morning_heat       = read_a( $data, 'OID4_4_1_4_3_1' );
    $other_day_heat           = read_a( $data, 'OID4_4_1_4_3_2' );
    $other_evening_heat       = read_a( $data, 'OID4_4_1_4_3_3' );
    $other_night_heat         = read_a( $data, 'OID4_4_1_4_3_4' );
    
    $device[ 'occupied_morning_heat' ]    = $occupied_morning_heat;
    $device[ 'occupied_day_heat' ]        = $occupied_day_heat;
    $device[ 'occupied_evening_heat' ]    = $occupied_evening_heat;
    $device[ 'occupied_night_heat' ]      = $occupied_night_heat;
    $device[ 'unoccupied_morning_heat' ]  = $unoccupied_morning_heat;
    $device[ 'unoccupied_day_heat' ]      = $unoccupied_day_heat;
    $device[ 'unoccupied_evening_heat' ]  = $unoccupied_evening_heat;
    $device[ 'unoccupied_night_heat' ]    = $unoccupied_night_heat;
    $device[ 'other_morning_heat' ]       = $other_morning_heat;
    $device[ 'other_day_heat' ]           = $other_day_heat;
    $device[ 'other_evening_heat' ]       = $other_evening_heat;
    $device[ 'other_night_heat' ]         = $other_night_heat;
    
    $occupied_morning_cool    = read_a( $data, 'OID4_4_1_5_1_1' );
    $occupied_day_cool        = read_a( $data, 'OID4_4_1_5_1_2' );
    $occupied_evening_cool    = read_a( $data, 'OID4_4_1_5_1_3' );
    $occupied_night_cool      = read_a( $data, 'OID4_4_1_5_1_4' );
    $unoccupied_morning_cool  = read_a( $data, 'OID4_4_1_5_2_1' );
    $unoccupied_day_cool      = read_a( $data, 'OID4_4_1_5_2_2' );
    $unoccupied_evening_cool  = read_a( $data, 'OID4_4_1_5_2_3' );
    $unoccupied_night_cool    = read_a( $data, 'OID4_4_1_5_2_4' );
    $other_morning_cool       = read_a( $data, 'OID4_4_1_5_3_1' );
    $other_day_cool           = read_a( $data, 'OID4_4_1_5_3_2' );
    $other_evening_cool       = read_a( $data, 'OID4_4_1_5_3_3' );
    $other_night_cool         = read_a( $data, 'OID4_4_1_5_3_4' );
    
    $device[ 'occupied_morning_cool' ]    = $occupied_morning_cool;
    $device[ 'occupied_day_cool' ]        = $occupied_day_cool;
    $device[ 'occupied_evening_cool' ]    = $occupied_evening_cool;
    $device[ 'occupied_night_cool' ]      = $occupied_night_cool;
    $device[ 'unoccupied_morning_cool' ]  = $unoccupied_morning_cool;
    $device[ 'unoccupied_day_cool' ]      = $unoccupied_day_cool;
    $device[ 'unoccupied_evening_cool' ]  = $unoccupied_evening_cool;
    $device[ 'unoccupied_night_cool' ]    = $unoccupied_night_cool;
    $device[ 'other_morning_cool' ]       = $other_morning_cool;
    $device[ 'other_day_cool' ]           = $other_day_cool;
    $device[ 'other_evening_cool' ]       = $other_evening_cool;
    $device[ 'other_night_cool' ]         = $other_night_cool;
    
    $occupied_morning_fan     = read_a( $data, 'OID4_4_1_6_1_1' );
    $occupied_day_fan         = read_a( $data, 'OID4_4_1_6_1_2' );
    $occupied_evening_fan     = read_a( $data, 'OID4_4_1_6_1_3' );
    $occupied_night_fan       = read_a( $data, 'OID4_4_1_6_1_4' );
    $unoccupied_morning_fan   = read_a( $data, 'OID4_4_1_6_2_1' );
    $unoccupied_day_fan       = read_a( $data, 'OID4_4_1_6_2_2' );
    $unoccupied_evening_fan   = read_a( $data, 'OID4_4_1_6_2_3' );
    $unoccupied_night_fan     = read_a( $data, 'OID4_4_1_6_2_4' );
    $other_morning_fan        = read_a( $data, 'OID4_4_1_6_3_1' );
    $other_day_fan            = read_a( $data, 'OID4_4_1_6_3_2' );
    $other_evening_fan        = read_a( $data, 'OID4_4_1_6_3_3' );
    $other_night_fan          = read_a( $data, 'OID4_4_1_6_3_4' );
    
    $device[ 'occupied_morning_fan' ]     = $occupied_morning_fan;
    $device[ 'occupied_day_fan' ]         = $occupied_day_fan;
    $device[ 'occupied_evening_fan' ]     = $occupied_evening_fan;
    $device[ 'occupied_night_fan' ]       = $occupied_night_fan;
    $device[ 'unoccupied_morning_fan' ]   = $unoccupied_morning_fan;
    $device[ 'unoccupied_day_fan' ]       = $unoccupied_day_fan;
    $device[ 'unoccupied_evening_fan' ]   = $unoccupied_evening_fan;
    $device[ 'unoccupied_night_fan' ]     = $unoccupied_night_fan;
    $device[ 'other_morning_fan' ]        = $other_morning_fan;
    $device[ 'other_day_fan' ]            = $other_day_fan;
    $device[ 'other_evening_fan' ]        = $other_evening_fan;
    $device[ 'other_night_fan' ]          = $other_night_fan;
   
    $sun_class = read_a( $data, 'OID4_4_3_2_1' );
    $mon_class = read_a( $data, 'OID4_4_3_2_2' );
    $tue_class = read_a( $data, 'OID4_4_3_2_3' );
    $wed_class = read_a( $data, 'OID4_4_3_2_4' );
    $thu_class = read_a( $data, 'OID4_4_3_2_5' );
    $fri_class = read_a( $data, 'OID4_4_3_2_6' );
    $sat_class = read_a( $data, 'OID4_4_3_2_7' );
    
    $device[ 'sun_class' ] = $sun_class;
    $device[ 'mon_class' ] = $mon_class;
    $device[ 'tue_class' ] = $tue_class;
    $device[ 'wed_class' ] = $wed_class;
    $device[ 'thu_class' ] = $thu_class;
    $device[ 'fri_class' ] = $fri_class;
    $device[ 'sat_class' ] = $sat_class;
    
  }

  private static function RecoverFromFactoryReset(
    $host,
    $port,
    $user,
    $pass
  ) {

    if ( $user !== 'admin' ) { return; }
    if ( $pass === 'admin' ) { return; }
    
    $ch = false;
    
    for ( $i = 0; $i < 5; $i++ ) {

      $sleep = 2 * $i;

      sleep( $sleep );

      if ( $ch ) { curl_close( $ch ); }

      $ch = curl_init( "http://$host:$port/admin.shtml" );

      curl_setopt( $ch, CURLOPT_POST, true );
      curl_setopt( $ch, CURLOPT_TIMEOUT, 10 );
      curl_setopt( $ch, CURLOPT_RETURNTRANSFER, true );
      curl_setopt( $ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC );
      curl_setopt( $ch, CURLOPT_USERPWD, 'admin:admin' );

      $pw = urlencode( $pass );
      
      $params = 'OID2.3.4=' . $pw . '&v_admin=' . $pw . '&submit=Submit';

      curl_setopt( $ch, CURLOPT_POSTFIELDS, $params );

      $curl_success = curl_exec( $ch );

      if ( strpos( $curl_success, '<!DOCTYPE' ) === 0 ) {

        // success!
        break;
        
      }
    }
    
    curl_close( $ch );
    
  }
  
  public function write_sensor_settings( &$device ) {
    
    $host = $device[ 'host' ];
    $port = $device[ 'port' ];
    $user = $device[ 'user' ];
    $pass = $device[ 'pass' ];

    $attempt = 0;

    while ( ++$attempt < WRITE_ATTEMPTS ) {
   
      try {

        $ch = curl_init( "http://$host:$port/pdp" );

        curl_setopt( $ch, CURLOPT_POST, true );
        curl_setopt( $ch, CURLOPT_TIMEOUT, NET_TIMEOUT );
        curl_setopt( $ch, CURLOPT_RETURNTRANSFER, true );
        curl_setopt( $ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC );
        curl_setopt( $ch, CURLOPT_USERPWD, "$user:$pass" );

        $params = array(
          'OID1.2' => $device[ 'device_name' ],
          'OID1.10.9' => $device[ 'site_name' ],
          'OID2.7.1' => $device[ 'model_number' ],
          'OID4.1.21' => $device[ 'temperature_scale' ],
          'OID4.3.6.1' => $device[ 'local_sensor_state' ],
          'OID4.3.6.2' => $device[ 'remote_sensor_1_state' ],
          'OID4.3.6.3' => $device[ 'remote_sensor_2_state' ],
          'OID4.3.2.1' => $device[ 'local_sensor_temp' ],
          'OID4.3.2.2' => $device[ 'remote_sensor_1_temp' ],
          'OID4.3.2.3' => $device[ 'remote_sensor_2_temp' ],
          'OID4.3.8.1' => $device[ 'local_sensor_average' ],
          'OID4.3.8.2' => $device[ 'remote_sensor_1_average' ],
          'OID4.3.8.3' => $device[ 'remote_sensor_2_average' ],
          'OID4.3.4.2' => $device[ 'remote_sensor_1_correction' ],
          'OID4.3.4.3' => $device[ 'remote_sensor_2_correction' ],
          'submit' => 'Submit'
        );

        $params = http_build_query( $params );

        curl_setopt( $ch, CURLOPT_POSTFIELDS, $params );

        $curl_success = $this->parse_success( curl_exec( $ch ) );

        if ( ! $curl_success ) {

          $error = curl_error( $ch );
          
          curl_close( $ch );
          
          throw Error(
            'Error "%error%" writing sensor settings to %host%:%port%.',
            'error', $error,
            'host', $host,
            'port', $port
          );

        }
  
        curl_close( $ch );
        
        return;
        
      }
      catch ( Exception $ex ) {

        try { curl_close( $ch ); } catch ( Exception $ignore ) { ; }
        
        dal()->exception->log( $ex );
        
      }
    } 

    $message = H01n(
      'Could not write sensor settings after zero attempts.',
      'Could not write sensor settings after one attempt.',
      'Could not write sensor settings after %number% attempts.',
      $attempt
    );
    
    throw new Exception( $message );
    
  }

  private function &get_thermostat( $id, &$user_id = null ) {
    
    if ( is_admin() ) {
    
      $thermostat = dal()->thermostat->get_by_id(
        $id
      );

      $user_id = read_a( $thermostat, 'user_id' );
      
    }
    else {
      
      $user_id = user()->id;

      $thermostat = dal()->thermostat->get_by_user_and_id(
        $user_id,
        $id
      );
      
    }
    
    return $thermostat;
    
  }
  
  public function set_times() {
    
    $therms = dal()->thermostat->get_all_active();
    
    foreach ( $therms as $therm ) {
      
      //echo "<pre>" . $therm[ 'id' ] . '</pre><br>';
      $this->write_time( $therm[ 'id' ] );
      
    }
    
  }
  
  public function write_time( $id ) {

    $thermostat = $this->get_thermostat( $id, $user_id );
    
    $host = $thermostat[ 'host' ];
    $port = $thermostat[ 'port' ];
    $user = $thermostat[ 'user' ];
    $pass = $thermostat[ 'pass' ];
    
    $location = dal()->location->get_by_id(
      $thermostat[ 'location_id' ]
    );

    $datetime = new DateTime();
    $timezone = new DateTimeZone( $location[ 'timezone' ] );
    $datetime->setTimezone( $timezone );
    
    $attempt = 0;
    
    while ( ++$attempt < WRITE_ATTEMPTS ) {
  
      try {

        $ch = curl_init( "http://$host:$port/pdp" );

        curl_setopt( $ch, CURLOPT_POST, true );
        curl_setopt( $ch, CURLOPT_TIMEOUT, NET_TIMEOUT );
        curl_setopt( $ch, CURLOPT_RETURNTRANSFER, true );
        curl_setopt( $ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC );
        curl_setopt( $ch, CURLOPT_USERPWD, "$user:$pass" );

        $seconds_since_epoch =
          $datetime->getTimestamp() +
          $datetime->getOffset();

        $params = array(
          'OID2.5.1' => $seconds_since_epoch,
          'submit' => 'Submit'
        );

        $params = http_build_query( $params );

        curl_setopt( $ch, CURLOPT_POSTFIELDS, $params );

        $curl_success = $this->parse_success( curl_exec( $ch ) );

        if ( ! $curl_success ) {
          
          $error = curl_error( $ch );

          curl_close( $ch );
          
          throw Error(
            'Error "%error%" writing time to %host%:%port%.',
            'error', $error,
            'host', $host,
            'port', $port
          );

        }

        curl_close( $ch );
        
        return;
        
      }
      catch ( Exception $ex ) {

        try { curl_close( $ch ); } catch ( Exception $ignore ) { ; }
        
        dal()->exception->log( $ex );
        
      }
    } 

    $message = H01n(
      'Could not write time after zero attempts.',
      'Could not write time after one attempt.',
      'Could not write time after %number% attempts.',
      $attempt
    );
    
    throw new Exception( $message );

  }

  public function log_historic( $id, $key, $prev, $val ) {
    
    dal()->history->log( user()->id, $id, $key, $prev, $val );
    
  }
  
  public function write_settings( $id, &$settings ) {

    static $monitor = array(
      'hvac_mode',
      'fan_mode',
      'setback_status',
      'setback_heat',
      'setback_cool',
      'occupied_morning_start',
      'occupied_day_start',
      'occupied_evening_start',
      'occupied_night_start',
      'unoccupied_morning_start',
      'unoccupied_day_start',
      'unoccupied_evening_start',
      'unoccupied_night_start',
      'other_morning_start',
      'other_day_start',
      'other_evening_start',
      'other_night_start',
      'occupied_morning_heat',
      'occupied_day_heat',
      'occupied_evening_heat',
      'occupied_night_heat',
      'unoccupied_morning_heat',
      'unoccupied_day_heat',
      'unoccupied_evening_heat',
      'unoccupied_night_heat',
      'other_morning_heat',
      'other_day_heat',
      'other_evening_heat',
      'other_night_heat',
      'occupied_morning_cool',
      'occupied_day_cool',
      'occupied_evening_cool',
      'occupied_night_cool',
      'unoccupied_morning_cool',
      'unoccupied_day_cool',
      'unoccupied_evening_cool',
      'unoccupied_night_cool',
      'other_morning_cool',
      'other_day_cool',
      'other_evening_cool',
      'other_night_cool',
      'occupied_morning_fan',
      'occupied_day_fan',
      'occupied_evening_fan',
      'occupied_night_fan',
      'unoccupied_morning_fan',
      'unoccupied_day_fan',
      'unoccupied_evening_fan',
      'unoccupied_night_fan',
      'other_morning_fan',
      'other_day_fan',
      'other_evening_fan',
      'other_night_fan',
      'sun_class',
      'mon_class',
      'tue_class',
      'wed_class',
      'thu_class',
      'fri_class',
      'sat_class'
    );

    $record = $this->get_thermostat( $id, $user_id );
    
    $host = $record[ 'host' ];
    $port = $record[ 'port' ];
    $user = $record[ 'user' ];
    $pass = $record[ 'pass' ];
    
    $this->read_device( $record );
    
    foreach ( $monitor as $key ) {
      
      $val = read_a( $settings, $key );
      $prev = read_a( $record, $key );
      
      if ( $prev === null ) {
        
        throw Error(
          'Could not load previous value for "%key%".',
          'key', $key
        );
        
      }
      
      if ( $val != $prev ) {
        
        $this->log_historic( $id, $key, $prev, $val );
        
      }
    }

    $attempt = 0;
    
    while ( ++$attempt < WRITE_ATTEMPTS ) {
   
      try {
        
        $ch = curl_init( "http://$host:$port/pdp" );

        curl_setopt( $ch, CURLOPT_POST, true );
        curl_setopt( $ch, CURLOPT_TIMEOUT, NET_TIMEOUT );
        curl_setopt( $ch, CURLOPT_RETURNTRANSFER, true );
        curl_setopt( $ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC );
        curl_setopt( $ch, CURLOPT_USERPWD, "$user:$pass" );

        $params = array(
          'OID4.1.1='        => $settings[ 'hvac_mode' ],
          'OID4.1.3='        => $settings[ 'fan_mode' ],
          'OID4.1.9='        => $settings[ 'setback_status' ], // SetbackStatus
          'OID4.1.5='        => $settings[ 'setback_heat' ], // SetbackHeat
          'OID4.1.6='        => $settings[ 'setback_cool' ], // SetbackCool
          'OID4.4.1.3.1.1='  => $settings[ 'occupied_morning_start' ], // OccupiedMorningStart
          'OID4.4.1.3.1.2='  => $settings[ 'occupied_day_start' ], // OccupiedDayStart
          'OID4.4.1.3.1.3='  => $settings[ 'occupied_evening_start' ], // OccupiedEveningStart
          'OID4.4.1.3.1.4='  => $settings[ 'occupied_night_start' ], // OccupiedNightStart
          'OID4.4.1.3.2.1='  => $settings[ 'unoccupied_morning_start' ], // UnoccupiedMorningStart
          'OID4.4.1.3.2.2='  => $settings[ 'unoccupied_day_start' ], // UnoccupiedDayStart
          'OID4.4.1.3.2.3='  => $settings[ 'unoccupied_evening_start' ], // UnoccupiedEveningStart
          'OID4.4.1.3.2.4='  => $settings[ 'unoccupied_night_start' ], // UnoccupiedNightStart
          'OID4.4.1.3.3.1='  => $settings[ 'other_morning_start' ], // OtherMorningStart
          'OID4.4.1.3.3.2='  => $settings[ 'other_day_start' ], // OtherDayStart
          'OID4.4.1.3.3.3='  => $settings[ 'other_evening_start' ], // OtherEveningStart
          'OID4.4.1.3.3.4='  => $settings[ 'other_night_start' ], // OtherNightStart
          'OID4.4.1.4.1.1='  => $settings[ 'occupied_morning_heat' ], // OccupiedMorningHeat
          'OID4.4.1.4.1.2='  => $settings[ 'occupied_day_heat' ], // OccupiedDayHeat
          'OID4.4.1.4.1.3='  => $settings[ 'occupied_evening_heat' ], // OccupiedEveningHeat
          'OID4.4.1.4.1.4='  => $settings[ 'occupied_night_heat' ], // OccupiedNightHeat
          'OID4.4.1.4.2.1='  => $settings[ 'unoccupied_morning_heat' ], // UnoccupiedMorningHeat
          'OID4.4.1.4.2.2='  => $settings[ 'unoccupied_day_heat' ], // UnoccupiedDayHeat
          'OID4.4.1.4.2.3='  => $settings[ 'unoccupied_evening_heat' ], // UnoccupiedEveningHeat
          'OID4.4.1.4.2.4='  => $settings[ 'unoccupied_night_heat' ], // UnoccupiedNightHeat
          'OID4.4.1.4.3.1='  => $settings[ 'other_morning_heat' ], // OtherMorningHeat
          'OID4.4.1.4.3.2='  => $settings[ 'other_day_heat' ], // OtherDayHeat
          'OID4.4.1.4.3.3='  => $settings[ 'other_evening_heat' ], // OtherEveningHeat
          'OID4.4.1.4.3.4='  => $settings[ 'other_night_heat' ], // OtherNightHeat
          'OID4.4.1.5.1.1='  => $settings[ 'occupied_morning_cool' ], // OccupiedMorningCool
          'OID4.4.1.5.1.2='  => $settings[ 'occupied_day_cool' ], // OccupiedDayCool
          'OID4.4.1.5.1.3='  => $settings[ 'occupied_evening_cool' ], // OccupiedEveningCool
          'OID4.4.1.5.1.4='  => $settings[ 'occupied_night_cool' ], // OccupiedNightCool
          'OID4.4.1.5.2.1='  => $settings[ 'unoccupied_morning_cool' ], // UnoccupiedMorningCool
          'OID4.4.1.5.2.2='  => $settings[ 'unoccupied_day_cool' ], // UnoccupiedDayCool
          'OID4.4.1.5.2.3='  => $settings[ 'unoccupied_evening_cool' ], // UnoccupiedEveningCool
          'OID4.4.1.5.2.4='  => $settings[ 'unoccupied_night_cool' ], // UnoccupiedNightCool
          'OID4.4.1.5.3.1='  => $settings[ 'other_morning_cool' ], // OtherMorningCool
          'OID4.4.1.5.3.2='  => $settings[ 'other_day_cool' ], // OtherDayCool
          'OID4.4.1.5.3.3='  => $settings[ 'other_evening_cool' ], // OtherEveningCool
          'OID4.4.1.5.3.4='  => $settings[ 'other_night_cool' ], // OtherNightCool
          'OID4.4.1.6.1.1='  => $settings[ 'occupied_morning_fan' ], // OccupiedMorningFan
          'OID4.4.1.6.1.2='  => $settings[ 'occupied_day_fan' ], // OccupiedDayFan
          'OID4.4.1.6.1.3='  => $settings[ 'occupied_evening_fan' ], // OccupiedEveningFan
          'OID4.4.1.6.1.4='  => $settings[ 'occupied_night_fan' ], // OccupiedNightFan
          'OID4.4.1.6.2.1='  => $settings[ 'unoccupied_morning_fan' ], // UnoccupiedMorningFan
          'OID4.4.1.6.2.2='  => $settings[ 'unoccupied_day_fan' ], // UnoccupiedDayFan
          'OID4.4.1.6.2.3='  => $settings[ 'unoccupied_evening_fan' ], // UnoccupiedEveningFan
          'OID4.4.1.6.2.4='  => $settings[ 'unoccupied_night_fan' ], // UnoccupiedNightFan
          'OID4.4.1.6.3.1='  => $settings[ 'other_morning_fan' ], // OtherMorningFan
          'OID4.4.1.6.3.2='  => $settings[ 'other_day_fan' ], // OtherDayFan
          'OID4.4.1.6.3.3='  => $settings[ 'other_evening_fan' ], // OtherEveningFan
          'OID4.4.1.6.3.4='  => $settings[ 'other_night_fan' ], // OtherNightFan
          'OID4.4.3.2.1='    => $settings[ 'sun_class' ], // SunClass
          'OID4.4.3.2.2='    => $settings[ 'mon_class' ], // MonClass
          'OID4.4.3.2.3='    => $settings[ 'tue_class' ], // TueClass
          'OID4.4.3.2.4='    => $settings[ 'wed_class' ], // WedClass
          'OID4.4.3.2.5='    => $settings[ 'thu_class' ], // ThuClass
          'OID4.4.3.2.6='    => $settings[ 'fri_class' ], // FriClass
          'OID4.4.3.2.7='    => $settings[ 'sat_class' ], // SatClass
          'submit' => 'Submit'
        );

        $params = http_build_query( $params );

        curl_setopt( $ch, CURLOPT_POSTFIELDS, $params );

        $curl_success = $this->parse_success( curl_exec( $ch ) );

        if ( ! $curl_success ) {

          $error = curl_error( $ch );
          
          curl_close( $ch );
          
          throw Error(
            'Error "%error%" writing to thermostat "%id%".',
            'error', $error,
            'id', $id
          );

        }

        curl_close( $ch );
        
        return;
        
      }
      catch ( Exception $ex ) {

        try { curl_close( $ch ); } catch ( Exception $ignore ) { ; }
        
        dal()->exception->log( $ex );
        
      }
    } 

    $message = H01n(
      'Could not write settings after zero attempts.',
      'Could not write settings after one attempt.',
      'Could not write settings after %number% attempts.',
      $attempt
    );
    
    throw new Exception( $message );
    
  }

  private function parse_success( $success ) {

    static $failures = array(
      '502 Proxy Error',
      '503 Service Temporarily Unavailable',
      '401 Authorization Required',
      'Please try again'
    );

    foreach ( $failures as $failure ) {
      
      if ( false !== strpos( $success, $failure ) ) {

        return false;
        
      }
    }
      
    return $success;
    
  }
  
  private function sanitize_sensor_state( &$state ) {

    static $valid = array( self::DISABLED, self::ENABLED );
    
    if ( in_array( $state, $valid ) ) { return; }
    
    $state = self::NOT_PRESENT;
    
  }
  
  private function sanitize_sensor_temp( &$temp ) {

    
  }
  
  private function sanitize_sensor_average( &$average ) {

    if ( $average === self::ENABLED ) { return; }
    
    $average = self::DISABLED;
    
  }

  private function sanitize_sensor_correction( &$correction ) {

   
  }
  
  private function load_devices( &$data ) {
    
    foreach ( $data as &$record ) {

      $this->load_device( $record );
      
    }
  }
  
  private function load_device( &$record ) {
    
    try {

      $this->read_device( $record );

    }
    catch ( Exception $ex ) {

      $record[ 'bad_read' ] = $ex->getMessage();

    }
  }
  
}
