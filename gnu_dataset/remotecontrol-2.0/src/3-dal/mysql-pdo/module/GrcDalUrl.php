<?php
/*

Copyright (C) 2012-2015 GNU remotecontrol authors.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

class GrcDalUrl extends GrcDalModule {
  
  public static function Create( $dal ) {
    
    return new self( $dal );
    
  }

  //public function url_redirect( $url ) {
  public function redirect( $url ) {

    if ( ! is_string( $url ) ) { return false; }
    
    $hash = sha1( $url );
    
    $sql = "
select
  count(*) as `count`
from
  v2_url
where
  hash = :hash
and
  redirect = 1
";

    $args = array(
      'hash' => $hash
    );
    
    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql, $args );

    return $this->fetch_int( $stmt, 'count' ) === 1;
    
  }
  
  //public function url_whitelist( $url ) {
  public function whitelist( $url ) {
        
    validator()->string( $url, 'URL' );

    $hash = sha1( $url );
    
    $sql = "
select
  count(*) as `count`
from
  v2_url
where
  hash = :hash
and
  redirect = 1
";

    $args = array(
      'hash' => $hash
    );
    
    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql, $args );

    $count = $this->fetch_int( $stmt, 'count' );
    
    if ( $count > 0 ) { return; }
        
    $sql = "
replace into v2_url (
  `hash`,
  `url`,
  `redirect`
)
values (
  :hash,
  :url,
  1
)";
    
    $args = array(
      'hash' => $hash,
      'url' => $url
    );
    
    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql, $args );
    
    $this->finish( $stmt );
    
  }
}
