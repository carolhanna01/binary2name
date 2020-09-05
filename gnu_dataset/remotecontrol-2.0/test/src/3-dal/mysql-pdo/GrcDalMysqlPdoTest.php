<?php

//require_once __DIR__ . '/../../../src/test.php';

/**
 * @ runTestsInSeparateProcesses
 * @preserveGlobalState disabled
 */
class GrcDalMysqlPdoTest extends PHPUnit_Framework_TestCase {
  
  public function setUp() {

    //ensure( 'GRC_DAL', 'mysql-pdo' );
    
    require_once __DIR__ . '/../../../../src/test.php';
    
  }

  public function test_main() {

    $ex = new Exception( 'just testing...' );
    
    $main = dal()->main;
    
    $main->log_exception( $ex );
    
  }
}
