<?php

//        # Copyright (C) 2012-2013 GNU remotecontrol authors.
//        #
//        # This program is free software: you can redistribute it and/or modify
//        # it under the terms of the GNU Affero General Public License as
//        # published by the Free Software Foundation, either version 3 of the
//        # License, or (at your option) any later version.
//        #
//        # This program is distributed in the hope that it will be useful,
//        # but WITHOUT ANY WARRANTY; without even the implied warranty of
//        # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//        # GNU Affero General Public License for more details.
//        #
//        # You should have received a copy of the GNU Affero General Public License
//        # along with this program.  If not, see <http://www.gnu.org/licenses/>.

	require_once(__DIR__.'/../lib/Global.php');

	$currentuser = $_SESSION['User'];

	$action = intval($_POST['Action']);

	@ $thermostatid = $_POST['ThermostatId'];
	@ $name = $_POST['Name'];
	@ $description = $_POST['Description'];
	@ $host = $_POST['Host'];
	@ $port = $_POST['Port'];
	@ $timeZoneOffsetID = $_POST['TimeZoneOffsetID'];
	@ $groupID = $_POST['GroupID'];

	$timeZoneOffsetObj = null;
	$groupObj = null;

	if ($action != 3)
	{
		$timeZoneOffsetObj = new TimeZoneOffset($timeZoneOffsetID, true);
		$groupObj = new IPThermostatGroup($groupID, true);
	}

	$dataAccess = new MySqlDataAccess(Common::ConnectionString);
	$dataAccess->Command->CommandType = CommandType::StoredProcedure;

	switch ($action)
	{
		case 1:

			$dataAccess->Command->CommandText = 'CALL sp_Thermostat_Insert_Update(null, ' . $groupID . ', \'' . $dataAccess->Command->Connection->real_escape_string($name) . '\', \'' . $dataAccess->Command->Connection->real_escape_string($description) . '\', \'' . $host . '\', ' . $port . ', ' . $timeZoneOffsetID . ', ' . $currentuser->ID . ')';

			$thermostatid = intval($dataAccess->Command->ExecuteScalar());

			if ($thermostatid)
			{
				printf('%s;%s;%s;%s;%s;%s;%s;%s;%s;%s', $thermostatid, $name, $description, $host, $port, $timeZoneOffsetID, $timeZoneOffsetObj->Description, $groupID, $groupObj->Name, $currentuser->MaxTherms);
			}
			else
			{
				$error = $dataAccess->Command->Connection->error;

				printf('ERR:The following error occurred while accessing the database: %s', $error);
			}

			break;

		case 2:

			$dataAccess->Command->CommandText = 'CALL sp_Thermostat_Insert_Update(' . $thermostatid . ', ' . $groupID . ', \'' . $dataAccess->Command->Connection->real_escape_string($name) . '\', \'' . $dataAccess->Command->Connection->real_escape_string($description) . '\', \'' . $host . '\', ' . $port . ', ' . $timeZoneOffsetID . ', ' . $currentuser->ID . ')';

			if ($dataAccess->Command->ExecuteNonQuery())
			{
				printf('%s;%s;%s;%s;%s;%s;%s;%s;%s', $thermostatid, $name, $description, $host, $port, $timeZoneOffsetID, $timeZoneOffsetObj->Description, $groupID, $groupObj->Name);
			}
			else
			{
				$error = $dataAccess->Command->Connection->error;

				printf('ERR:The following error occurred while accessing the database: %s', $error);
			}

			break;

		case 3:

			$dataAccess->Command->CommandText = "CALL sp_Thermostat_Delete($thermostatid)";

			if ($dataAccess->Command->ExecuteNonQuery())
			{
				print $thermostatid;
			}
			else
			{
				$error = $dataAccess->Command->Connection->error;

				printf('ERR:The following error occurred while accessing the database: %s', $error);
			}

			break;
	}

	$dataAccess->Cleanup();
?>
