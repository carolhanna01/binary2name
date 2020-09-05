<?php

/**
 * @runTestsInSeparateProcesses
 * @preserveGlobalState disabled
 */
class GrcArrayTest extends PHPUnit_Framework_TestCase {

  public function setUp() {

    require_once __DIR__ . '/../../../src/test.php';
    
  }

  public function test_read_r_fail() {
    
    $this->setExpectedException( 'Exception' );
    
    read_r( 'missing', false );
    
  }
}
