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

var user_fields = [
  'username',
  'is_admin',
  'is_translator',
  'location',
  'max_thermostats',
  'langtag'
];

function edit_user( id ) {
  
  grc_edit( 'user', id, user_fields );

}

function cancel_edit_user( id ) {
  
  grc_cancel_edit( 'user', id, user_fields );

}

function update_user( id ) {
  
  grc_update(
    'user',
    id,
    user_fields,
    reload_on_success
  );

}

function delete_user( id ) {
  
  grc_delete_feature( 'user', id );
  
}

function add_user() {
  
  grc_add(
    'user',
    user_fields,
    reload_on_success
  );
  
}
