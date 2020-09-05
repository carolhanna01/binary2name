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

replace into v2_location values
( 1, 'Sydney', 'Australia/Sydney' ),
( 2, 'Knoxville', 'America/New_York' );

replace into v2_group values
( 1, 'Group 1', '' ),
( 2, 'Group 2', '' ),
( 3, 'Group 3', '' );

replace into v2_user values
( 1, 'jj5', 1, 1, 1, 1234, 'en-AU', '', 0 ),
( 2, 'tstat_admin', 1, 1, 2, 1234, 'en-US', '', 0 );

replace into v2_thermostat values
( 1, 1, 1, 'Test Device 1', '', 'trust.jj5.net', 101, 'admin', 'GNUrc', 1, 0 ),
( 2, 2, 2, 'Test Device 2', '', 'trust.jj5.net', 102, 'admin', 'GNUrc', 1, 0 );
