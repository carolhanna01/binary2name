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

// wait... it turns out all of the following are already implemented!
// strcmp, intcmp, boolcmp, floatcmp and 'datecmp' is the same as 'strcmp'...

if ( ! function_exists( 'intcmp' ) ) {
  function intcmp( $a, $b ) { return intval( $a ) - intval( $b ); }
}

if ( ! function_exists( 'boolcmp' ) ) {
  function boolcmp( $a, $b ) { return intcmp( intval( $a ), intval( $b ) ); }
}

if ( ! function_exists( 'floatcmp' ) ) {
  function floatcmp( $a, $b ) {
    $result = floatval( $a ) - floatval( $b );
    if ( $result < 0 ) { return -1; }
    if ( $result > 0 ) { return 1; }
    return 0;
  }
}
