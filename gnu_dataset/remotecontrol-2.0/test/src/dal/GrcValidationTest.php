<?php

require_once __DIR__ . '/test.php';

/**
 * @runTestsInSeparateProcesses
 * @preserveGlobalState disabled
 */
class GrcValidationTest extends PHPUnit_Framework_TestCase {
  
  public function setUp() {

    ensure( 'GRC_DAL', 'mysql-pdo' );
    
    require_once __DIR__ . '/../../../src/test.php';
    
  }
  
  public function test_invalid_html() {

    $html = file_get_contents( __DIR__ . '/invalid-html.html' );

    try {

      validator()->html( $html );
      
      $this->assertFalse( true );
      
    }
    catch ( Exception $ex ) {

      $this->assertSame(
        "Value '<pre class='xdebug-var-dump' dir='ltr...' is not valid HTML.",
        $ex->getMessage()
      );
      
    }
  }
  
  public function test_invalid_timezones() {

    $this->invalid_timezone( 'UTB' );
    
  }
  
  public function test_valid_timezones() {

    $this->valid_timezone( 'UTC' );
    $this->valid_timezone( 'Australia/Sydney' );
    
  }
  
  private function invalid_timezone( $timezone ) {

    try {
      
      validator()->timezone( $timezone );
      
      $this->assertFalse( true );
      
    }
    catch ( Exception $ex ) {

      $this->assertSame( "Invalid Timezone '$timezone'.", $ex->getMessage() );
      
    }
  }
  
  private function valid_timezone( $timezone ) {

    validator()->timezone( $timezone );
      
  }
}
