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

class GrcDalLanguage extends GrcDalModule {
  
  public static function Create( $dal ) {
    
    return new self( $dal );
    
  }
  
  public function count() {
    
    $sql = "
select
  count(*) as `count`
from
  v2_intl_language
";

    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql );

    return $this->fetch_int( $stmt, 'count' );
        
  }

  public function &report(
    &$count = -1,
    $page = null,
    $sort = null
  ) {

    $sql = "
select
  l.langtag,
  l.fallback,
  l.english_name,
  l.local_name,
  l.active
from
  v2_intl_language l
";
    
    $start = 0;
    
    if ( $page ) {

      $count = $this->sql_count( $sql );

      $page->set_count( $count );
      
      $start = $page->start;

    }

    if ( $sort ) {

      $sql .= $sort->to_sql();

    }

    if ( $page ) {

      $sql .= $page->to_sql();

    }
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_all( $stmt, $table, $start );
    
    return $table;
    
  }
}
