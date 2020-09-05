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

class GrcDalHistory extends GrcDalModule {
  
  public static function Create( $dal ) {
    
    return new self( $dal );
    
  }

  public function log(
    $user_id,
    $thermostat_id,
    $field,
    $previous_value,
    $current_value
  ) {
        
    $sql = "
insert into v2_history (
  user_id,
  thermostat_id,
  field,
  previous_value,
  current_value
)
values (
  :user_id,
  :thermostat_id,
  :field,
  :previous_value,
  :current_value
)";

    if ( preg_match( '/(_heat)|(_cool)$/', $field ) ) {
    
      if ( DEFAULT_SCALE === FAHRENHEIT ) {
        
        $previous_value = format_1dp( $previous_value / 10 );
        $current_value = format_1dp( $current_value / 10 );
        
      }
      else {
        
        $previous_value = format_1dp( f2c( $previous_value / 10 ) );
        $current_value = format_1dp( f2c( $current_value / 10 ) );
        
      }
    }
    else if ( preg_match( '/_start$/', $field ) ) {
  
      $previous_value = msm212( $previous_value );
      $current_value = msm212( $current_value );

    }

    $args = array(
      'user_id' => $user_id,
      'thermostat_id' => $thermostat_id,
      'field' => $field,
      'previous_value' => strval( $previous_value ),
      'current_value' => strval( $current_value )
    );
    
    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql, $args );
    
    $this->finish( $stmt );    
    
  }  
  
  public function &report(
    &$search,
    &$count = -1,
    $page = null,
    $sort = null
  ) {

    $sql = "
select
  h.*,
  t.name as name,
  t.description as description,
  l.name as location,
  g.name as `group`,
  u.username as username
from
  v2_history h
left join
  v2_thermostat t
on
  h.thermostat_id = t.id
left join
  v2_location l
on
  t.location_id = l.id
left join
  v2_group g
on
  t.group_id = g.id
left join
  v2_user u
on
  h.user_id = u.id
";
    
    $this->apply_search( $sql, $search );
    
    //echo "<pre>"; var_dump( $sql ); die;
    
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
