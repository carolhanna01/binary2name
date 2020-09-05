<?php

trait GrcDalUrlTest {
  
  public function test_url_redirect() {
    
    $url = 'https://www.jj5.net/';
    
    dal()->url->whitelist( $url );
    
    $this->assertTrue( dal()->url->redirect( $url ) );
    
  }
  
  public function test_url_redirect_fail() {
    
    $url = 'invalid';
    
    $this->assertFalse( dal()->url->redirect( $url ) );
    
  }
}