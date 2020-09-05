<?php

require_once __DIR__ . '/../../src/include.php';

$username = $_SERVER[ 'PHP_AUTH_USER' ];

$user_id = dal()->user->get_user_id_for_username( $username );

$action = $_POST[ 'action' ];

$thermostat_id = $_POST[ 'ThermID' ];

$thermostat_user_id = dal()->thermostat->get_user_id_for_thermostat_id(
  $thermostat_id
);

if ( $thermostat_user_id !== $user_id ) {

  die( "Unauthorised access." );

}

$oids = dal()->thermostat->get_oids();

static $ignore = array(
  'OID1.10.9',
  'OID1.2',
  'OID2.7.1',
  'OID1.13.2.1',
  'OID1.13.2.2',
  'OID1.13.2.3',
  'OID1.13.2.4'
);

$result = '';

foreach ( $oids as &$oid ) {

  if ( in_array( $oid[ 'OID' ], $ignore ) ) { continue; }

  //$result .= $oid[ 'OID' ] . '=' . $oid[ 'FieldName' ] . ';';
  $result .= $oid[ 'OID' ] . '=;';

}

$result = rtrim( $result, ';' );

print $thermostat_id . ';' . $result;
