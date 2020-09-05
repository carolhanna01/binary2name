<?php

/**
 * @runTestsInSeparateProcesses
 * @preserveGlobalState disabled
 */
class GrcIntlTest extends PHPUnit_Framework_TestCase {

  public function setUp() {

    require_once __DIR__ . '/../../../src/test.php';
    
    global $grc_lang_dat_path, $grc_lang_ser_path;

    //grc_parse_lang_dat( $grc_lang_dat_path, $file );
    //grc_write_lang_ser( $grc_lang_ser_path, $file );

    if ( ! defined( 'ERROR_TEST_CLASS_INTL_ERROR' ) ) {

      define_error( 'TEST_CLASS_INTL_ERROR' );
      
    }
  }

  public function test_error() {

    $error = ERROR_TEST_CLASS_INTL_ERROR;
    
    $this->error_test(
      Error(
        $error,
        "Error with '%value%'.",
        'value', 'exception'
      ),
      $error,
      "Error with 'exception'."
    );
      
    $this->error_test(
      Error(
        $error,
        "Error with '%v1%' and '%v2%'.",
        'v1', '1',
        'v2', '2'
      ),
      $error,
      "Error with '1' and '2'."
    );

    $this->error_test(
      Error(
        $error,
        "Error with '%long-value%'.",
        'long-value', '1234567890123456789012345678901234567890'
      ),
      $error,
      "Error with '1234567890123456789012345678901234567...'."
    );
    
  }

  public function test_error_1() {

    $error = ERROR_TEST_CLASS_INTL_ERROR;
    
    try {
      
      throw Error(
        $error,
        "Error with '%value%'.",
        'value', 'exception'
      );
      
      $this->assertFalse();
      
    }
    catch ( Exception $ex ) {

      $this->assertSame( "Error with 'exception'.", $ex->getMessage() );
      $this->assertSame( $error, $ex->getCode() );
      
    }
  }
  
  public function testDump() {
   
    return;
    
    intl()->dump();
    
  }
  
  public function testParseTiming() {

    return;
    
    $a = array( $this, 'read_dat' );
    $b = array( $this, 'read_ser' );
    
    $a_time = 0;
    $b_time = 0;
    
    for ( $i = 0; $i < 9; $i++ ) {
      
      $a_time += $this->time( $a );
      $b_time += $this->time( $b );
      
    }

    $this->assertTrue( $b_time < $a_time );
        
  }
  
  public function time( $fn ) {
    
    $begin = microtime( true );
    
    $fn();
    
    $end = microtime( true );
    
    return $end - $begin;
    
  }
  
  public function read_dat() {
    
    global $grc_lang_dat_path;

    grc_parse_lang_dat( $grc_lang_dat_path, $file );
    
  }
  
  public function read_ser() {
    
    global $grc_lang_ser_path;

    grc_parse_lang_ser( $grc_lang_ser_path, $file );
    
  }
  
  private function error_test( $error, $code, $expected ) {

    try {

      throw $error;
      
      $this->assertFalse();
      
    }
    catch ( Exception $ex ) {

      $this->assertSame( $expected, $ex->getMessage() );
      $this->assertSame( $code, $ex->getCode() );
      
    }
  }
}
