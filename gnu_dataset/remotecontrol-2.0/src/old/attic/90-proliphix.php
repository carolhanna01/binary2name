<?php

abstract class NtEnum {

  private static $cache = NULL;

  private static function &GetConstants() {

    if (self::$cache == NULL) { self::$cache = array(); }
    
    $class = get_called_class();
    
    if ( ! array_key_exists( $class, self::$cache ) ) {
      
      $reflect = new ReflectionClass( $class );

      self::$cache[ $class ] = $reflect->getConstants();
      
    }
    
    return self::$cache[ $class ];
    
  }

  public static function IsValidName( $name, $strict = false ) {

    $constants = self::GetConstants();

    if ( $strict ) {
      
      return array_key_exists( $name, $constants );
      
    }

    $keys = array_map( 'strtolower', array_keys( $constants ) );
    
    return in_array( strtolower( $name ), $keys );

  }

  public static function IsValidValue( $value ) {

    $constants = self::GetConstants();
    
    $values = array_values( $constants );

    return in_array( $value, $values, $strict = true );
    
  }
}

// 2.7.1
final class NtModel extends NtEnum {
  
  const NT10 = 10;
  const NT20 = 20;
  const NT100 = 100;
  const NT120 = 120;
  const NT150 = 150;
  
  public static function Parse( $input ) {
    
    $result = intval( ltrim( $input, 'NT' ) );
    
    if ( self::IsValidValue( $result ) ) { return $result; }
    
    return -1;
    
    throw new Exception( "Invalid NtModel '$input'." );
    
  }
}

// 4.1.1
final class NtHvacMode extends NtEnum {

  const Off = 1;
  const Heat = 2;
  const Cool = 3;
  const Auto = 4;

  public static function Parse( $input ) {
    
    $result = intval( $input );
    
    if ( self::IsValidValue( $result ) ) { return $result; }
    
    return -1;
    
    throw new Exception( "Invalid NtHvacMode '$input'." );
    
  }
}

// 4.1.2
final class NtHvacState extends NtEnum {

  const Initializing = 1;
  const Off = 2;
  const Heat = 3;
  const Heat2 = 4;
  const Cool = 5;
  const Cool2 = 6;
  const Delay = 7;
  const ResetRelays = 8;

  public static function Parse( $input ) {
    
    $result = intval( $input );
    
    if ( self::IsValidValue( $result ) ) { return $result; }
    
    return -1;
    
    throw new Exception( "Invalid NtHvacState '$input'." );
    
  }
}

// 4.1.3
final class NtFanMode extends NtEnum {

  const Auto = 1;
  const On = 2;
  const Schedule = 3;

  public static function Parse( $input ) {
    
    $result = intval( $input );
    
    if ( self::IsValidValue( $result ) ) { return $result; }
    
    return -1;
    
    throw new Exception( "Invalid NtFanMode '$input'." );
    
  }
}

// 4.1.4
final class NtFanState extends NtEnum {

  const Init = 1;
  const Off = 2;
  const On = 3;

  public static function Parse( $input ) {
    
    $result = intval( $input );
    
    if ( self::IsValidValue( $result ) ) { return $result; }
    
    return -1;
    
    throw new Exception( "Invalid NtFanState '$input'." );
    
  }
}

// 4.1.8
final class NtSetbackStatus extends NtEnum {
  
  const Normal = 1;
  const Hold = 2;
  const Override = 3;
  
  public static function Parse( $input ) {
    
    $result = intval( $input );
    
    if ( self::IsValidValue( $result ) ) { return $result; }
    
    return -1;
    
    throw new Exception( "Invalid NtSetbackStatus '$input'." );
    
  }  
}

final class NtSensorState extends NtEnum {
  
  const NotPresent = 0;
  const Disabled = 1;
  const Enabled = 2;
  
  public static function Parse( $input ) {
    
    $result = intval( $input );
    
    if ( self::IsValidValue( $result ) ) { return $result; }
    
    return -1;
    
    throw new Exception( "Invalid NtSensorState '$input'." );
    
  }  
}

final class NtSensorAverage extends NtEnum {
  
  const Disabled = 1;
  const Enabled = 2;
  
  public static function Parse( $input ) {
    
    $result = intval( $input );
    
    if ( self::IsValidValue( $result ) ) { return $result; }
    
    return -1;
    
    throw new Exception( "Invalid NtSensorAverage '$input'." );
    
  }  
}

final class NtSensorType extends NtEnum {
  
  const Analog = 1;
  const Thermistor = 2;
  
  public static function Parse( $input ) {
    
    $result = intval( $input );
    
    if ( self::IsValidValue( $result ) ) { return $result; }
    
    return -1;
    
    throw new Exception( "Invalid NtSensorType '$input'." );
    
  }  
}

final class NtSetbackFan extends NtEnum {
  
  const Min0 = 0; // disable
  const Min15 = 15;
  const Min30 = 30;
  const Min45 = 45;
  const Min60 = 60;
  
  public static function Parse( $input ) {
    
    $result = intval( $input );
    
    if ( self::IsValidValue( $result ) ) { return $result; }
    
    return -1;
    
    throw new Exception( "Invalid NtSetbackFan '$input'." );
    
  }  
}

// 4.1.10
final class NtPeriod extends NtEnum {

  const Morning = 1;
  const Day = 2;
  const Evening = 3;
  const Night = 4;

  public static function Parse( $input ) {
    
    $result = intval( $input );
    
    if ( self::IsValidValue( $result ) ) { return $result; }
    
    return -1;
    
    throw new Exception( "Invalid NtPeriod '$input'." );
    
  }
}

// 4.1.11
final class NtClass extends NtEnum {

  const Occupied = 1;
  const Unoccupied = 2;
  const Other = 3;

  public static function Parse( $input ) {
    
    $result = intval( $input );
    
    if ( self::IsValidValue( $result ) ) { return $result; }
    
    return -1;
    
    throw new Exception( "Invalid NtClass '$input'." );
    
  }
}

// 4.1.12
final class NtActivePeriod extends NtEnum {

  const Morning = 1;
  const Day = 2;
  const Evening = 3;
  const Night = 4;
  const Hold = 5;
  const Override = 6;

  public static function Parse( $input ) {
    
    $result = intval( $input );
    
    if ( self::IsValidValue( $result ) ) { return $result; }
    
    return -1;
    
    throw new Exception( "Invalid NtActivePeriod '$input'." );
    
  }
}

final class NtWeekday extends NtEnum {
  
  const Sun = 1;
  const Mon = 2;
  const Tue = 3;
  const Wed = 4;
  const Thu = 5;
  const Fri = 6;
  const Sat = 7;
  
  public static function Parse( $input ) {
    
    $result = intval( $input );
    
    if ( self::IsValidValue( $result ) ) { return $result; }
    
    return -1;
    
    throw new Exception( "Invalid NtWeekday '$input'." );
    
  }
}

final class NtSensor extends NtEnum {
  
  const Local = 1;
  const Remote_1 = 2;
  const Remote_2 = 3;
  
  public static function Parse( $input ) {
    
    $result = intval( $input );
    
    if ( self::IsValidValue( $result ) ) { return $result; }
    
    return -1;
    
    throw new Exception( "Invalid NtSensor '$input'." );
    
  }
}

class NtDevice {

  /*
  public $sensor;
  public $start;
  public $heat;
  public $cool;
  public $fan;
  public $weekday;
  */
  
  // 1.2:
  public $name;
  // 1.10.9
  public $site_name;

  // 2.1.1:
  public $seconds_since_boot;
  // 2.5.1:
  public $seconds_since_epoch;
  
  // 2.7.1:
  public $model;
  
  // 4.1.1:
  public $hvac_mode;
  
  // 4.1.2:
  public $hvac_state;

  // 4.1.3:
  public $fan_mode;
  
  // 4.1.4:
  public $fan_state;

  // 4.1.5
  public $setback_heat;
  // 4.1.6
  public $setback_cool;
  // 4.1.9
  public $setback_status;
  
  // 4.1.10:
  public $current_period;

  // 4.1.11:
  public $current_class;
  
  // 4.1.12:
  public $active_period;
  
  // 4.1.13
  public $average_temp;
  
  // 4.1.14
  public $relative_humidity;
  
  // 4.3.2.1
  public $local_sensor;
  // 4.3.2.2
  public $remote_sensor_1;
  // 4.3.2.3
  public $remote_sensor_2;
  
  // 4.3.4.2
  public $remote_sensor_1_correction;
  // 4.3.4.3
  public $remote_sensor_2_correction;
  
  // 4.3.5.2
  public $remote_sensor_1_name;
  // 4.3.5.3
  public $remote_sensor_2_name;
  
  // 4.3.6.1
  public $local_sensor_state;
  // 4.3.6.2
  public $remote_sensor_1_state;
  // 4.3.6.3
  public $remote_sensor_2_state;

  // 4.3.8.1
  public $local_sensor_average;
  // 4.3.8.2
  public $remote_sensor_1_average;
  // 4.3.8.3
  public $remote_sensor_2_average;
  
  // 4.3.9.2
  public $remote_sensor_1_type;
  // 4.3.9.3
  public $remote_sensor_2_type;
  
  // 4.4.1.3.1.1-4
  public $occupied_morning_start;
  public $occupied_day_start;
  public $occupied_evening_start;
  public $occupied_night_start;
  // 4.4.1.3.2.1-4
  public $unoccupied_morning_start;
  public $unoccupied_day_start;
  public $unoccupied_evening_start;
  public $unoccupied_night_start;
  // 4.4.1.3.3.1-4
  public $other_morning_start;
  public $other_day_start;
  public $other_evening_start;
  public $other_night_start;
  
  // 4.4.1.4.1.1-4
  public $occupied_morning_heat;
  public $occupied_day_heat;
  public $occupied_evening_heat;
  public $occupied_night_heat;
  // 4.4.1.4.2.1-4
  public $unoccupied_morning_heat;
  public $unoccupied_day_heat;
  public $unoccupied_evening_heat;
  public $unoccupied_night_heat;
  // 4.4.1.4.3.1-4
  public $other_morning_heat;
  public $other_day_heat;
  public $other_evening_heat;
  public $other_night_heat;
  
  // 4.4.1.5.1.1-4
  public $occupied_morning_cool;
  public $occupied_day_cool;
  public $occupied_evening_cool;
  public $occupied_night_cool;
  // 4.4.1.5.2.1-4
  public $unoccupied_morning_cool;
  public $unoccupied_day_cool;
  public $unoccupied_evening_cool;
  public $unoccupied_night_cool;
  // 4.4.1.5.3.1-4
  public $other_morning_cool;
  public $other_day_cool;
  public $other_evening_cool;
  public $other_night_cool;
  
  // 4.4.1.6.1.1
  public $occupied_morning_fan;
  // 4.4.1.6.1.2
  public $occupied_day_fan;
  // 4.4.1.6.1.3
  public $occupied_evening_fan;
  // 4.4.1.6.1.4
  public $occupied_night_fan;
  // 4.4.1.6.2.1
  public $unoccupied_morning_fan;
  // 4.4.1.6.2.2
  public $unoccupied_day_fan;
  // 4.4.1.6.2.3
  public $unoccupied_evening_fan;
  // 4.4.1.6.2.4
  public $unoccupied_night_fan;
  // 4.4.1.6.3.1
  public $other_morning_fan;
  // 4.4.1.6.3.2
  public $other_day_fan;
  // 4.4.1.6.3.3
  public $other_evening_fan;
  // 4.4.1.6.3.4
  public $other_night_fan;
  
  // 4.4.3.2.1
  public $sun_class;
  // 4.4.3.2.2
  public $mon_class;
  // 4.4.3.2.3
  public $tue_class;
  // 4.4.3.2.4
  public $wed_class;
  // 4.4.3.2.5
  public $thu_class;
  // 4.4.3.2.6,
  public $fri_class;
  // 4.4.3.2.7
  public $sat_class;
  
  public function __construct(
    $name,
    $site_name,
    $seconds_since_boot,
    $seconds_since_epoch,
    $model,
    $hvac_mode,
    $hvac_state,
    $fan_mode,
    $fan_state,
    $setback_heat,
    $setback_cool,
    $setback_status,
    $current_period,
    $current_class,
    $active_period,
    $average_temp,
    $relative_humidity,
    $local_sensor,
    $remote_sensor_1,
    $remote_sensor_2,
    $remote_sensor_1_correction,
    $remote_sensor_2_correction,
    $remote_sensor_1_name,
    $remote_sensor_2_name,
    $local_sensor_state,
    $remote_sensor_1_state,
    $remote_sensor_2_state,
    $local_sensor_average,
    $remote_sensor_1_average,
    $remote_sensor_2_average,
    $remote_sensor_1_type,
    $remote_sensor_2_type,

    $occupied_morning_start,
    $occupied_day_start,
    $occupied_evening_start,
    $occupied_night_start,
    $unoccupied_morning_start,
    $unoccupied_day_start,
    $unoccupied_evening_start,
    $unoccupied_night_start,
    $other_morning_start,
    $other_day_start,
    $other_evening_start,
    $other_night_start,
    
    $occupied_morning_heat,
    $occupied_day_heat,
    $occupied_evening_heat,
    $occupied_night_heat,
    $unoccupied_morning_heat,
    $unoccupied_day_heat,
    $unoccupied_evening_heat,
    $unoccupied_night_heat,
    $other_morning_heat,
    $other_day_heat,
    $other_evening_heat,
    $other_night_heat,
    
    $occupied_morning_cool,
    $occupied_day_cool,
    $occupied_evening_cool,
    $occupied_night_cool,
    $unoccupied_morning_cool,
    $unoccupied_day_cool,
    $unoccupied_evening_cool,
    $unoccupied_night_cool,
    $other_morning_cool,
    $other_day_cool,
    $other_evening_cool,
    $other_night_cool,
    
    $occupied_morning_fan,
    $occupied_day_fan,
    $occupied_evening_fan,
    $occupied_night_fan,
    $unoccupied_morning_fan,
    $unoccupied_day_fan,
    $unoccupied_evening_fan,
    $unoccupied_night_fan,
    $other_morning_fan,
    $other_day_fan,
    $other_evening_fan,
    $other_night_fan,
    $sun_class,
    $mon_class,
    $tue_class,
    $wed_class,
    $thu_class,
    $fri_class,
    $sat_class
  ) {
    
    $this->name = $name;
    $this->site_name = $site_name;
    
    $this->seconds_since_boot = $seconds_since_boot;
    $this->seconds_since_epoch = $seconds_since_epoch;
    
    $this->model = $model;
    
    $this->hvac_mode = $hvac_mode;

    $this->hvac_state = $hvac_state;
    
    $this->fan_mode = $fan_mode;
    
    $this->fan_state = $fan_state;
    
    $this->setback_heat = $setback_heat;
    $this->setback_cool = $setback_cool;
    $this->setback_status = $setback_status;
    
    $this->current_period = $current_period;
    
    $this->current_class = $current_class;
    
    $this->active_period = $active_period;
    
    $this->average_temp = $average_temp;
    
    $this->relative_humidity = $relative_humidity;
    
    $this->local_sensor = $local_sensor;
    $this->remote_sensor_1 = $remote_sensor_1;
    $this->remote_sensor_2 = $remote_sensor_2;

    $this->remote_sensor_1_correction = $remote_sensor_1_correction;
    $this->remote_sensor_2_correction = $remote_sensor_2_correction;

    $this->remote_sensor_1_name = $remote_sensor_1_name;
    $this->remote_sensor_2_name = $remote_sensor_2_name;
    
    $this->local_sensor_state = $local_sensor_state;
    $this->remote_sensor_1_state = $remote_sensor_1_state;
    $this->remote_sensor_2_state = $remote_sensor_2_state;
    
    $this->local_sensor_average = $local_sensor_average;
    $this->remote_sensor_1_average = $remote_sensor_1_average;
    $this->remote_sensor_2_average = $remote_sensor_2_average;
    
    $this->remote_sensor_1_type = $remote_sensor_1_type;
    $this->remote_sensor_2_type = $remote_sensor_2_type;

    $this->occupied_morning_start = $occupied_morning_start;
    $this->occupied_day_start = $occupied_day_start;
    $this->occupied_evening_start = $occupied_evening_start;
    $this->occupied_night_start = $occupied_night_start;
    $this->unoccupied_morning_start = $unoccupied_morning_start;
    $this->unoccupied_day_start = $unoccupied_day_start;
    $this->unoccupied_evening_start = $unoccupied_evening_start;
    $this->unoccupied_night_start = $unoccupied_night_start;
    $this->other_morning_start = $other_morning_start;
    $this->other_day_start = $other_day_start;
    $this->other_evening_start = $other_evening_start;
    $this->other_night_start = $other_night_start;
    
    $this->occupied_morning_heat = $occupied_morning_heat;
    $this->occupied_day_heat = $occupied_day_heat;
    $this->occupied_evening_heat = $occupied_evening_heat;
    $this->occupied_night_heat = $occupied_night_heat;
    $this->unoccupied_morning_heat = $unoccupied_morning_heat;
    $this->unoccupied_day_heat = $unoccupied_day_heat;
    $this->unoccupied_evening_heat = $unoccupied_evening_heat;
    $this->unoccupied_night_heat = $unoccupied_night_heat;
    $this->other_morning_heat = $other_morning_heat;
    $this->other_day_heat = $other_day_heat;
    $this->other_evening_heat = $other_evening_heat;
    $this->other_night_heat = $other_night_heat;
    
    $this->occupied_morning_cool = $occupied_morning_cool;
    $this->occupied_day_cool = $occupied_day_cool;
    $this->occupied_evening_cool = $occupied_evening_cool;
    $this->occupied_night_cool = $occupied_night_cool;
    $this->unoccupied_morning_cool = $unoccupied_morning_cool;
    $this->unoccupied_day_cool = $unoccupied_day_cool;
    $this->unoccupied_evening_cool = $unoccupied_evening_cool;
    $this->unoccupied_night_cool = $unoccupied_night_cool;
    $this->other_morning_cool = $other_morning_cool;
    $this->other_day_cool = $other_day_cool;
    $this->other_evening_cool = $other_evening_cool;
    $this->other_night_cool = $other_night_cool;
    
    $this->occupied_morning_fan = $occupied_morning_fan;
    $this->occupied_day_fan = $occupied_day_fan;
    $this->occupied_evening_fan = $occupied_evening_fan;
    $this->occupied_night_fan = $occupied_night_fan;
    $this->unoccupied_morning_fan = $unoccupied_morning_fan;
    $this->unoccupied_day_fan = $unoccupied_day_fan;
    $this->unoccupied_evening_fan = $unoccupied_evening_fan;
    $this->unoccupied_night_fan = $unoccupied_night_fan;
    $this->other_morning_fan = $other_morning_fan;
    $this->other_day_fan = $other_day_fan;
    $this->other_evening_fan = $other_evening_fan;
    $this->other_night_fan = $other_night_fan;
    
    $this->sun_class = $sun_class;
    $this->mon_class = $mon_class;
    $this->tue_class = $tue_class;
    $this->wed_class = $wed_class;
    $this->thu_class = $thu_class;
    $this->fri_class = $fri_class;
    $this->sat_class = $sat_class;

    $this->parameterize();
    
  }
  
  public static function Load( $host, $port, $auth ) {

    $query_data = self::Query(
      '1.2',
      '1.10.9',
      '2.1.1',
      '2.5.1',
      '2.7.1',
      '4.1.1',
      '4.1.2',
      '4.1.3',
      '4.1.4',
      '4.1.5',
      '4.1.6',
      '4.1.9',
      '4.1.10',
      '4.1.11',
      '4.1.12',
      '4.1.13',
      '4.1.14',
      '4.3.2.1',
      '4.3.2.2',
      '4.3.2.3',
      '4.3.4.2',
      '4.3.4.3',
      '4.3.5.2',
      '4.3.5.3',
      '4.3.6.1',
      '4.3.6.2',
      '4.3.6.3',
      '4.3.8.1',
      '4.3.8.2',
      '4.3.8.3',
      '4.3.9.2',
      '4.3.9.3',
      
      '4.4.1.3.1.1',
      '4.4.1.3.1.2',
      '4.4.1.3.1.3',
      '4.4.1.3.1.4',
      '4.4.1.3.2.1',
      '4.4.1.3.2.2',
      '4.4.1.3.2.3',
      '4.4.1.3.2.4',
      '4.4.1.3.3.1',
      '4.4.1.3.3.2',
      '4.4.1.3.3.3',
      '4.4.1.3.3.4',
      
      '4.4.1.4.1.1',
      '4.4.1.4.1.2',
      '4.4.1.4.1.3',
      '4.4.1.4.1.4',
      '4.4.1.4.2.1',
      '4.4.1.4.2.2',
      '4.4.1.4.2.3',
      '4.4.1.4.2.4',
      '4.4.1.4.3.1',
      '4.4.1.4.3.2',
      '4.4.1.4.3.3',
      '4.4.1.4.3.4',
      
      '4.4.1.5.1.1',
      '4.4.1.5.1.2',
      '4.4.1.5.1.3',
      '4.4.1.5.1.4',
      '4.4.1.5.2.1',
      '4.4.1.5.2.2',
      '4.4.1.5.2.3',
      '4.4.1.5.2.4',
      '4.4.1.5.3.1',
      '4.4.1.5.3.2',
      '4.4.1.5.3.3',
      '4.4.1.5.3.4',
      
      '4.4.1.6.1.1',
      '4.4.1.6.1.2',
      '4.4.1.6.1.3',
      '4.4.1.6.1.4',
      '4.4.1.6.2.1',
      '4.4.1.6.2.2',
      '4.4.1.6.2.3',
      '4.4.1.6.2.4',
      '4.4.1.6.3.1',
      '4.4.1.6.3.2',
      '4.4.1.6.3.3',
      '4.4.1.6.3.4',
      
      '4.4.3.2.1',
      '4.4.3.2.2',
      '4.4.3.2.3',
      '4.4.3.2.4',
      '4.4.3.2.5',
      '4.4.3.2.6',
      '4.4.3.2.7'
    );
      
    $params = http_build_query( $query_data );
    
    $ch = false;
    $has_reset = false;

    for ( $i = 0; $i < 5; $i++ ) {

      $sleep = 2 * $i;

      sleep( $sleep );

      if ( $ch ) { curl_close( $ch ); }

      $ch = curl_init( "http://$host:$port/get" );
      //$ch = curl_init("http://trust.jj5.net:101/pdp");
      //$ch = curl_init("http://trust.jj5.net:101/get");

      curl_setopt($ch, CURLOPT_POST, true);
      curl_setopt($ch, CURLOPT_TIMEOUT, 10);
      curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
      curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
      curl_setopt($ch, CURLOPT_USERPWD, $auth );


      curl_setopt( $ch, CURLOPT_POSTFIELDS, $params );

      $curl_success = curl_exec( $ch );

      //var_dump( $curl_success );

      $proxy_error = ( false !== strpos( $curl_success, '502 Proxy Error' ) );
      
      $availability_error = (
        false !== strpos( $curl_success, '503 Service Temporarily Unavailable' )
      );

      if (
        $has_reset === false &&
        $curl_success === '401 Authorization Required'
      ) {
        
        $has_reset = true;
        
        self::RecoverFromFactoryReset( $host, $port, $auth );
        
        $i = 0;
        
        continue;
        
      }
      
      if (
        $curl_success === false ||
        $curl_success === 'Please try again' ||
        //$curl_success === '401 Authorization Required' ||
        $proxy_error ||
        $availability_error
      ) {

        $curl_error = curl_error( $ch );
        
        var_dump( array(
          'curl' => $curl_success,
          'curl_error' => $curl_error,
          'proxy' => $proxy_error,
          'availability' => $availability_error
        ));

        //var_dump( $curl_success );
        
        // normalize error response:
        $curl_success = false;

        continue;

      }

      break;

    }

    curl_close( $ch );
    
    if ( $curl_success === false ) {
      
      return;
      
    }
    
    parse_str( $curl_success, $s );
    
    //var_dump( $s );
    //exit;
    
    $result = new NtDevice(
      
      self::Read( $s, '1.2' ),
      
      self::Read( $s, '1.10.9'),
      
      self::Read( $s, '2.1.1' ),
      
      intval( self::Read( $s, '2.5.1' ) ),
      
      NtModel::Parse( self::Read( $s, '2.7.1' ) ),
      
      NtHvacMode::Parse( self::Read( $s, '4.1.1' ) ),
      
      NtHvacState::Parse( self::Read( $s, '4.1.2' ) ),
      
      NtFanMode::Parse( self::Read( $s, '4.1.3' ) ),
      
      NtFanState::Parse( self::Read( $s, '4.1.4' ) ),
      
      intval( self::Read( $s, '4.1.5' ) ),
      intval( self::Read( $s, '4.1.6' ) ),
      
      NtSetbackStatus::Parse( self::Read( $s, '4.1.9' ) ),
      
      NtPeriod::Parse( self::Read( $s, '4.1.10' ) ),
      
      NtClass::Parse( self::Read( $s, '4.1.11' ) ),
      
      NtActivePeriod::Parse( self::Read( $s, '4.1.12' ) ),
      
      intval( self::Read( $s, '4.1.13' ) ),
      
      self::Read( $s, '4.1.14' ),
      
      intval( self::Read( $s, '4.3.2.1' ) ),
      intval( self::Read( $s, '4.3.2.2' ) ),
      intval( self::Read( $s, '4.3.2.3' ) ),
      
      intval( self::Read( $s, '4.3.4.2' ) ),
      intval( self::Read( $s, '4.3.4.3' ) ),
      
      self::Read( $s, '4.3.5.2' ),
      self::Read( $s, '4.3.5.3' ),
      
      NtSensorState::Parse( self::Read( $s, '4.3.6.1' ) ),
      NtSensorState::Parse( self::Read( $s, '4.3.6.2' ) ),
      NtSensorState::Parse( self::Read( $s, '4.3.6.3' ) ),
      
      NtSensorAverage::Parse( self::Read( $s, '4.3.8.1' ) ),
      NtSensorAverage::Parse( self::Read( $s, '4.3.8.2' ) ),
      NtSensorAverage::Parse( self::Read( $s, '4.3.8.3' ) ),
      
      NtSensorType::Parse( self::Read( $s, '4.3.9.2' ) ),
      NtSensorType::Parse( self::Read( $s, '4.3.9.3' ) ),
      
      // start
      intval( self::Read( $s, '4.4.1.3.1.1' ) ),
      intval( self::Read( $s, '4.4.1.3.1.2' ) ),
      intval( self::Read( $s, '4.4.1.3.1.3' ) ),
      intval( self::Read( $s, '4.4.1.3.1.4' ) ),
      intval( self::Read( $s, '4.4.1.3.2.1' ) ),
      intval( self::Read( $s, '4.4.1.3.2.2' ) ),
      intval( self::Read( $s, '4.4.1.3.2.3' ) ),
      intval( self::Read( $s, '4.4.1.3.2.4' ) ),
      intval( self::Read( $s, '4.4.1.3.3.1' ) ),
      intval( self::Read( $s, '4.4.1.3.3.2' ) ),
      intval( self::Read( $s, '4.4.1.3.3.3' ) ),
      intval( self::Read( $s, '4.4.1.3.3.4' ) ),
      
      // heat
      intval( self::Read( $s, '4.4.1.4.1.1' ) ),
      intval( self::Read( $s, '4.4.1.4.1.2' ) ),
      intval( self::Read( $s, '4.4.1.4.1.3' ) ),
      intval( self::Read( $s, '4.4.1.4.1.4' ) ),
      intval( self::Read( $s, '4.4.1.4.2.1' ) ),
      intval( self::Read( $s, '4.4.1.4.2.2' ) ),
      intval( self::Read( $s, '4.4.1.4.2.3' ) ),
      intval( self::Read( $s, '4.4.1.4.2.4' ) ),
      intval( self::Read( $s, '4.4.1.4.3.1' ) ),
      intval( self::Read( $s, '4.4.1.4.3.2' ) ),
      intval( self::Read( $s, '4.4.1.4.3.3' ) ),
      intval( self::Read( $s, '4.4.1.4.3.4' ) ),
      
      // cool
      intval( self::Read( $s, '4.4.1.5.1.1' ) ),
      intval( self::Read( $s, '4.4.1.5.1.2' ) ),
      intval( self::Read( $s, '4.4.1.5.1.3' ) ),
      intval( self::Read( $s, '4.4.1.5.1.4' ) ),
      intval( self::Read( $s, '4.4.1.5.2.1' ) ),
      intval( self::Read( $s, '4.4.1.5.2.2' ) ),
      intval( self::Read( $s, '4.4.1.5.2.3' ) ),
      intval( self::Read( $s, '4.4.1.5.2.4' ) ),
      intval( self::Read( $s, '4.4.1.5.3.1' ) ),
      intval( self::Read( $s, '4.4.1.5.3.2' ) ),
      intval( self::Read( $s, '4.4.1.5.3.3' ) ),
      intval( self::Read( $s, '4.4.1.5.3.4' ) ),
      
      // fan
      NtSetbackFan::Parse( self::Read( $s, '4.4.1.6.1.1' ) ),
      NtSetbackFan::Parse( self::Read( $s, '4.4.1.6.1.2' ) ),
      NtSetbackFan::Parse( self::Read( $s, '4.4.1.6.1.3' ) ),
      NtSetbackFan::Parse( self::Read( $s, '4.4.1.6.1.4' ) ),
      NtSetbackFan::Parse( self::Read( $s, '4.4.1.6.2.1' ) ),
      NtSetbackFan::Parse( self::Read( $s, '4.4.1.6.2.2' ) ),
      NtSetbackFan::Parse( self::Read( $s, '4.4.1.6.2.3' ) ),
      NtSetbackFan::Parse( self::Read( $s, '4.4.1.6.2.4' ) ),
      NtSetbackFan::Parse( self::Read( $s, '4.4.1.6.3.1' ) ),
      NtSetbackFan::Parse( self::Read( $s, '4.4.1.6.3.2' ) ),
      NtSetbackFan::Parse( self::Read( $s, '4.4.1.6.3.3' ) ),
      NtSetbackFan::Parse( self::Read( $s, '4.4.1.6.3.4' ) ),
      
      NtClass::Parse( self::Read( $s, '4.4.3.2.1' ) ),
      NtClass::Parse( self::Read( $s, '4.4.3.2.2' ) ),
      NtClass::Parse( self::Read( $s, '4.4.3.2.3' ) ),
      NtClass::Parse( self::Read( $s, '4.4.3.2.4' ) ),
      NtClass::Parse( self::Read( $s, '4.4.3.2.5' ) ),
      NtClass::Parse( self::Read( $s, '4.4.3.2.6' ) ),
      NtClass::Parse( self::Read( $s, '4.4.3.2.7' ) )
    );
    
    return $result;
    
  }  
  
  private static function Read( $s, $oid ) {
    
    $key = 'OID' . str_replace( '.', '_', $oid );
    
    if ( array_key_exists( $key, $s ) ) { return $s[ $key ]; }

    //var_dump( $s );
    debug_print_backtrace();
    
    throw new Exception( "Could not read state for key '$key'." );
    
  }
  
  private static function Query() {
    
    $result = array();

    $args = func_get_args();
    
    foreach ( $args as $arg ) {
      
      $result[ 'OID' . $arg ] = '';
      
    }
    
    $result[ 'submit' ] = 'Submit';
    
    return $result;
    
  }
 
  private function parameterize() {

    return;
    
    //
    // sensor
    //
    
    $this->sensor = array();
    
    $this->sensor[ NtSensor::Local ][ 'temperature' ] = $this->local_sensor;
    //$this->sensor[ NtSensor::Local ][ 'correction' ] = $this->...;
    //$this->sensor[ NtSensor::Local ][ 'name' ] = $this->...;
    $this->sensor[ NtSensor::Local ][ 'state' ] = $this->local_sensor_state;
    $this->sensor[ NtSensor::Local ][ 'average' ] = $this->local_sensor_average;
    //$this->sensor[ NtSensor::Local ][ 'type' ] = $this->...;

    $this->sensor[ NtSensor::Remote_1 ][ 'temperature' ] = $this->remote_sensor_1;
    $this->sensor[ NtSensor::Remote_1 ][ 'correction' ] = $this->remote_sensor_1_correction;
    $this->sensor[ NtSensor::Remote_1 ][ 'name' ] = $this->remote_sensor_1_name;
    $this->sensor[ NtSensor::Remote_1 ][ 'state' ] = $this->remote_sensor_1_state;
    $this->sensor[ NtSensor::Remote_1 ][ 'average' ] = $this->remote_sensor_1_average;
    $this->sensor[ NtSensor::Remote_1 ][ 'type' ] = $this->remote_sensor_1_type;

    $this->sensor[ NtSensor::Remote_2 ][ 'temperature' ] = $this->remote_sensor_2;
    $this->sensor[ NtSensor::Remote_2 ][ 'correction' ] = $this->remote_sensor_2_correction;
    $this->sensor[ NtSensor::Remote_2 ][ 'name' ] = $this->remote_sensor_2_name;
    $this->sensor[ NtSensor::Remote_2 ][ 'state' ] = $this->remote_sensor_2_state;
    $this->sensor[ NtSensor::Remote_2 ][ 'average' ] = $this->remote_sensor_2_average;
    $this->sensor[ NtSensor::Remote_2 ][ 'type' ] = $this->remote_sensor_2_type;

    //
    // start
    //
    
    $this->start = array();
    
    $this->start[ NtClass::Occupied ][ NtPeriod::Morning ] = $this->occupied_morning_start;
    $this->start[ NtClass::Occupied ][ NtPeriod::Day ] = $this->occupied_day_start;
    $this->start[ NtClass::Occupied ][ NtPeriod::Evening ] = $this->occupied_evening_start;
    $this->start[ NtClass::Occupied ][ NtPeriod::Night ] = $this->occupied_night_start;
    $this->start[ NtClass::Unoccupied ][ NtPeriod::Morning ] = $this->unoccupied_morning_start;
    $this->start[ NtClass::Unoccupied ][ NtPeriod::Day ] = $this->unoccupied_day_start;
    $this->start[ NtClass::Unoccupied ][ NtPeriod::Evening ] = $this->unoccupied_evening_start;
    $this->start[ NtClass::Unoccupied ][ NtPeriod::Night ] = $this->unoccupied_night_start;
    $this->start[ NtClass::Other ][ NtPeriod::Morning ] = $this->other_morning_start;
    $this->start[ NtClass::Other ][ NtPeriod::Day ] = $this->other_day_start;
    $this->start[ NtClass::Other ][ NtPeriod::Evening ] = $this->other_evening_start;
    $this->start[ NtClass::Other ][ NtPeriod::Night ] = $this->other_night_start;
    
    //
    // heat
    //
    
    $this->heat = array();
    
    $this->heat[ NtClass::Occupied ][ NtPeriod::Morning ] = $this->occupied_morning_heat;
    $this->heat[ NtClass::Occupied ][ NtPeriod::Day ] = $this->occupied_day_heat;
    $this->heat[ NtClass::Occupied ][ NtPeriod::Evening ] = $this->occupied_evening_heat;
    $this->heat[ NtClass::Occupied ][ NtPeriod::Night ] = $this->occupied_night_heat;
    $this->heat[ NtClass::Unoccupied ][ NtPeriod::Morning ] = $this->unoccupied_morning_heat;
    $this->heat[ NtClass::Unoccupied ][ NtPeriod::Day ] = $this->unoccupied_day_heat;
    $this->heat[ NtClass::Unoccupied ][ NtPeriod::Evening ] = $this->unoccupied_evening_heat;
    $this->heat[ NtClass::Unoccupied ][ NtPeriod::Night ] = $this->unoccupied_night_heat;
    $this->heat[ NtClass::Other ][ NtPeriod::Morning ] = $this->other_morning_heat;
    $this->heat[ NtClass::Other ][ NtPeriod::Day ] = $this->other_day_heat;
    $this->heat[ NtClass::Other ][ NtPeriod::Evening ] = $this->other_evening_heat;
    $this->heat[ NtClass::Other ][ NtPeriod::Night ] = $this->other_night_heat;
    
    //
    // cool
    //
    
    $this->cool = array();
    
    $this->cool[ NtClass::Occupied ][ NtPeriod::Morning ] = $this->occupied_morning_cool;
    $this->cool[ NtClass::Occupied ][ NtPeriod::Day ] = $this->occupied_day_cool;
    $this->cool[ NtClass::Occupied ][ NtPeriod::Evening ] = $this->occupied_evening_cool;
    $this->cool[ NtClass::Occupied ][ NtPeriod::Night ] = $this->occupied_night_cool;
    $this->cool[ NtClass::Unoccupied ][ NtPeriod::Morning ] = $this->unoccupied_morning_cool;
    $this->cool[ NtClass::Unoccupied ][ NtPeriod::Day ] = $this->unoccupied_day_cool;
    $this->cool[ NtClass::Unoccupied ][ NtPeriod::Evening ] = $this->unoccupied_evening_cool;
    $this->cool[ NtClass::Unoccupied ][ NtPeriod::Night ] = $this->unoccupied_night_cool;
    $this->cool[ NtClass::Other ][ NtPeriod::Morning ] = $this->other_morning_cool;
    $this->cool[ NtClass::Other ][ NtPeriod::Day ] = $this->other_day_cool;
    $this->cool[ NtClass::Other ][ NtPeriod::Evening ] = $this->other_evening_cool;
    $this->cool[ NtClass::Other ][ NtPeriod::Night ] = $this->other_night_cool;
    
    //
    // fan
    //
    
    $this->fan = array();
    
    $this->fan[ NtClass::Occupied ][ NtPeriod::Morning ] = $this->occupied_morning_fan;
    $this->fan[ NtClass::Occupied ][ NtPeriod::Day ] = $this->occupied_day_fan;
    $this->fan[ NtClass::Occupied ][ NtPeriod::Evening ] = $this->occupied_evening_fan;
    $this->fan[ NtClass::Occupied ][ NtPeriod::Night ] = $this->occupied_night_fan;
    $this->fan[ NtClass::Unoccupied ][ NtPeriod::Morning ] = $this->unoccupied_morning_fan;
    $this->fan[ NtClass::Unoccupied ][ NtPeriod::Day ] = $this->unoccupied_day_fan;
    $this->fan[ NtClass::Unoccupied ][ NtPeriod::Evening ] = $this->unoccupied_evening_fan;
    $this->fan[ NtClass::Unoccupied ][ NtPeriod::Night ] = $this->unoccupied_night_fan;
    $this->fan[ NtClass::Other ][ NtPeriod::Morning ] = $this->other_morning_fan;
    $this->fan[ NtClass::Other ][ NtPeriod::Day ] = $this->other_day_fan;
    $this->fan[ NtClass::Other ][ NtPeriod::Evening ] = $this->other_evening_fan;
    $this->fan[ NtClass::Other ][ NtPeriod::Night ] = $this->other_night_fan;
    
    //
    // weekday
    //
    
    $this->weekday = array();
    
    $this->weekday[ NtWeekday::Sun ] = $this->sun_class;
    $this->weekday[ NtWeekday::Mon ] = $this->mon_class;
    $this->weekday[ NtWeekday::Tue ] = $this->tue_class;
    $this->weekday[ NtWeekday::Wed ] = $this->wed_class;
    $this->weekday[ NtWeekday::Thu ] = $this->thu_class;
    $this->weekday[ NtWeekday::Fri ] = $this->fri_class;
    $this->weekday[ NtWeekday::Sat ] = $this->sat_class;
    
  }
  
  private static function RecoverFromFactoryReset( $host, $port, $auth ) {

    $auth_parts = explode( ':', $auth, 2 );
    
    $username = $auth_parts[ 0 ];
    $password = $auth_parts[ 1 ];

    if ( $username !== 'admin' ) { return; }
    if ( $password === 'admin' ) { return; }
    
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

      $pw = urlencode( $password );
      
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
}
