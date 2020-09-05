<?php

//require_once __DIR__ . '/../../../src/test.php';

/**
 * @runTestsInSeparateProcesses
 * @preserveGlobalState disabled
 */
class GrcIntlTest extends PHPUnit_Framework_TestCase {

  public function setUp() {
    
    require_once __DIR__ . '/../../../src/test.php';
    
  }
  
  public function testFormat() {

    $this->canonicalize_test( 'en-latn-au', 'en-AU' );

    $this->canonicalize_test( 'sl-Rozaj-Solba-1994', 'sl-rozaj-solba-1994' );
    
  }

  public function testFail() {

    $this->parse_fail( null, "Invalid language ''." );

    $this->parse_fail( '', "Invalid language ''." );

    $this->parse_fail( 'abcd', "Invalid language 'abcd'." );
    
    $this->parse_fail(
      'en-abc-def-ghi-jkl',
      "Invalid extlang 'jkl'."
    );
    
    
  }
  
  public function testSucceed() {

    $this->parse_succeed( 'en-abc-def-ghi', array(
      'language' => 'en',
      'extlang' => 'abc',
      'script' => '',
      'region' => '',
      'variant' => array(),
      'extension' => array(),
    ));
    
    $this->parse_succeed( 'en-LATN-AU', array(
      'language' => 'en',
      'extlang' => '',
      'script' => '',
      'region' => 'AU',
      'variant' => array(),
      'extension' => array(),
    ));
    
  }
  
  public function canonicalize_test( $langtag, $expected ) {
    
    $this->assertSame(
      $expected,
      intl()->canonicalize_langtag( $langtag ),
      "$langtag -> $expected"
    );
    
  }
  
  public function parse_succeed( $langtag, $expect ) {
    
    $this->assertTrue( intl()->parse_langtag( $langtag, $result ), $langtag );
    $this->assertSame( $expect, $result );
    
  }
  
  public function parse_fail( $langtag, $expect_error ) {
    
    $this->assertFalse( intl()->parse_langtag( $langtag, $result, $error ), $langtag );
    
    $this->assertSame( $expect_error, $error, $expect_error );
    
  }
}
