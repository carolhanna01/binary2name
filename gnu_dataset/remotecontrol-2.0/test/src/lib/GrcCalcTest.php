<?php

/**
 * @runTestsInSeparateProcesses
 * @preserveGlobalState disabled
 */
class GrcCalcTest extends PHPUnit_Framework_TestCase {

  public function setUp() {

    require_once __DIR__ . '/../../../src/test.php';
    
  }

  public function test_count_words() {
    
    $this->assertSame( 10, count_words( '<a href="https://www.jj5.net" title="test">link</a>') );

    $this->assertSame( 1, count_words( 'one' ) );
    $this->assertSame( 3, count_words( 'one.two-three' ) );

    $this->assertSame( 1, count_words( "99%" ) );

    $this->assertSame( 2, count_words( "one '%ignore%' two" ) );
    
    $this->assertSame( 2, count_words( 'one %ignore% two' ) );

    $this->assertSame( 2, count_words( ' ... one %ignore% two %ignore% ... ' ) );
    
    
  }
}
