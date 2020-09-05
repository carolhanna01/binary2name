<?php

/**
 * @runTestsInSeparateProcesses
 * @preserveGlobalState disabled
 */
class GrcTypesTest extends PHPUnit_Framework_TestCase {

  public function setUp() {

    require_once __DIR__ . '/../../../src/test.php';
    
  }
  
  public function test_is_datetime_valid() {
  
    $this->datetime_valid( new DateTime() );
    //$this->datetime_valid( '2015-01-09' );
    $this->datetime_valid( '2015-01-09 14:37' );
    $this->datetime_valid( '2015-01-09 14:37:12' );
    
  }
  
  public function test_is_datetime_invalid() {

    $this->datetime_invalid( null );
    
    $this->datetime_invalid( false );
    $this->datetime_invalid( true );
    
    $this->datetime_invalid( INT32_MIN );
    $this->datetime_invalid( -1 );
    $this->datetime_invalid( 0 );
    $this->datetime_invalid( 1 );
    $this->datetime_invalid( INT32_MAX );
    
    $this->datetime_invalid( -1.0 );
    $this->datetime_invalid( 0.0 );
    $this->datetime_invalid( 1.0 );

    $this->datetime_invalid( new stdClass() );
    
    $this->datetime_invalid( '' );
    $this->datetime_invalid( 'abcd' );
    $this->datetime_invalid( 'asdf-as-as' );
    
  }
  
  private function datetime_valid( $value ) {
    
    $this->assertTrue( is_datetime( $value ), $value );
    
  }
  
  private function datetime_invalid( $value ) {
    
    $this->assertFalse( is_datetime( $value ) );
    
  }
}
