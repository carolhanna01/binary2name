<?php

/**
 * @runTestsInSeparateProcesses
 * @preserveGlobalState disabled
 */
class GrcDefinesTest extends PHPUnit_Framework_TestCase {

  public function setUp() {

    require_once __DIR__ . '/../../../src/test.php';
    
  }

  public function test_define_intl_context_valid() {
    
    define_intl_context( 'valid', 'Valid Label' );
    
  }
  
  public function test_define_intl_context_invalid() {
    
    $this->setExpectedException( 'Exception' );
    
    define_intl_context( 'invalid', 'Invalid Label' );
    define_intl_context( 'invalid', 'Invalid Label' );
    
  }
}
