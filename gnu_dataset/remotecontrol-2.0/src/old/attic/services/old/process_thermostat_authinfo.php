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

	$action = intval($_POST["Action"]);
	$thermid = intval($_POST["ThermostatId"]);
	@ $username = $_POST["Username"];
	@ $password = $_POST["Password"];

	$dataAccess = new MySqlDataAccess(Common::ConnectionString);
	$dataAccess->Command->CommandType = CommandType::StoredProcedure;

	switch($action)
	{
		case 1:

			$dataAccess->Command->CommandText = "CALL sp_Thermostat_Auth_Info_Update($thermid, '$username:$password')";

			if($dataAccess->Command->ExecuteNonQuery())
			{
				print $thermid;
			}
			else
			{
				$error = $dataAccess->Command->Connection->error;
				
				printf('ERR:The following error occurred while accessing the database: %s', $error);
			}

			break;

		case 2:

			$dataAccess->Command->CommandText = "CALL sp_Thermostat_Load_Details($thermid)";
			$dataAccess->GetData();

			if($dataAccess->ResultSet)
			{
				$row = $dataAccess->ResultSet->fetch_assoc();

				$authinfo = explode(':', $row['AuthString']);

				printf('%s;%s;%s', $thermid, $authinfo[0], $authinfo[1]);
			}
			else
			{
				$error = $dataAccess->Command->Connection->error;

				printf('ERR:The following error occurred while accessing the database: %s', $error);
			}

			break;
	}

?>
