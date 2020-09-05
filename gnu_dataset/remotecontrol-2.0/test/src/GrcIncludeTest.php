<?php

/**
 * @ runTestsInSeparateProcesses
 * @preserveGlobalState disabled
 */
class GrcIncludeTest extends PHPUnit_Framework_TestCase {

  public function setUp() {
    
    require_once __DIR__ . '/../../src/test.php';
    
  }

  public function test_error_define() {

    /*
    err()->define(
      'ERROR__SRC__TESTING',
      'Just testing!'
    );
    */
    
  }
}
