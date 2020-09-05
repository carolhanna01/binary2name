<?php

/**
 * @ runTestsInSeparateProcesses
 * @preserveGlobalState disabled
 */
class GrcDateTimeTest extends PHPUnit_Framework_TestCase {

  public function setUp() {

    require_once __DIR__ . '/../../../src/test.php';
    
  }
  
  public function test_timezone_list() {

    $timezones = DateTimeZone::listIdentifiers();

    $len = 0;
    
    foreach ( $timezones as $timezone ) {
      
      $len = max( array( $len, strlen( $timezone ) ) );
      
    }
  }
}
