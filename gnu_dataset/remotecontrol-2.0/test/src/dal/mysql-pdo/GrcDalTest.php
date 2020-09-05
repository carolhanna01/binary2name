<?php

require_once __DIR__ . '/../test.php';

/**
 * @runTestsInSeparateProcesses
 * @preserveGlobalState disabled
 */
class GrcDalMysqlPdoTest extends PHPUnit_Framework_TestCase {

  use GrcDalUrlTest;
  
  public function setUp() {

    ensure( 'GRC_DAL', 'mysql-pdo' );
    
    require_once __DIR__ . '/../../../../src/test.php';
    
  }
  
  public function test_execute_fail() {

    $this->setExpectedException( 'Exception' );
    
    $stmt = new GrcDalMysqlPdoMockStatement();
    
    dal()->execute( $stmt, null );
    
  }
  
  public function test_intl_get_languages_total() {
        
    $data = dal()->intl_get_languages_total();

    $this->assertSame( 5, count( $data ) );
    
  }
}

class GrcDalMysqlPdoMockStatement {
  
  public function execute() { return false; }
  
}
