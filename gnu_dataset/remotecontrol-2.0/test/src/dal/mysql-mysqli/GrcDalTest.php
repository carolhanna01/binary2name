<?php

require_once __DIR__ . '/../test.php';

/**
 * @runTestsInSeparateProcesses
 * @preserveGlobalState disabled
 */
class GrcDalMysqlMysqliTest extends PHPUnit_Framework_TestCase {

  use GrcDalUrlTest;
  
  public function setUp() {

    ensure( 'GRC_DAL', 'mysql-mysqli' );
    
    require_once __DIR__ . '/../../../../src/test.php';
    
  }
  
  public function test_execute_fail() {

    $this->setExpectedException( 'Exception' );
    
    $stmt = new GrcDalMysqlMysqliMockStatement();
    
    dal()->execute( $stmt, null );
    
  }
}

class GrcDalMysqlMysqliMockStatement {
  
  public function execute() { return false; }
  
}