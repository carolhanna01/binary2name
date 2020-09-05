<?php
require_once 'lib/Global.php';
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html>
	<head>
		<?php
//                    # Copyright (C) 2012-2014 GNU remotecontrol authors.
//                    #
//                    # This program is free software: you can redistribute it and/or modify
//                    # it under the terms of the GNU Affero General Public License as
//                    # published by the Free Software Foundation, either version 3 of the
//                    # License, or (at your option) any later version.
//                    #
//                    # This program is distributed in the hope that it will be useful,
//                    # but WITHOUT ANY WARRANTY; without even the implied warranty of
//                    # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//                    # GNU Affero General Public License for more details.
//                    #
//                    # You should have received a copy of the GNU Affero General Public License
//                    # along with this program.  If not, see <http://www.gnu.org/licenses/>.

                    @$currentUser = $_SESSION['User'];

                    $resourcesPageLevel = new ResourceCollection(true, 'GRCTL', false, 1, false);

                    $GRCTL = $resourcesPageLevel->FindResourceByCode('GRCTL');
                    echo '<title>'.Common::GetAppName().' - '.$GRCTL->Value.'</title>';

// sorting preserved, getting value from db
if (!isset($thermostatSort) && !is_null($currentUser->ThermostatSort)) {
    $thermostatSort = $currentUser->ThermostatSort;
}

$sortingURL = NULL;

if (isset($thermostatSort)) {
    $sortingURL = '?thermostatSort=' . $thermostatSort;
}
		?>

		<meta http-equiv="Content-Type" content="text/html;charset=utf8" />

		<link rel="shortcut icon" href="images/GNURC.ico" type="image/x-icon" />
		<link rel="icon" href="images/GNURC.ico" type="image/ico" />

		<link rel="Stylesheet" type="text/css" href="styles/master.css" />
		<link rel="Stylesheet" type="text/css" href="styles/transactionlog.css" />

                <script>
                    /*
                    @licstart  The following is the entire license notice for the
                    JavaScript code in this page.

                    Copyright (C) 2012-2013 GNU remotecontrol authors

                    The JavaScript code in this page is free software: you can
                    redistribute it and/or modify it under the terms of the GNU
                    General Public License (GNU GPL) as published by the Free Software
                    Foundation, either version 3 of the License, or (at your option)
                    any later version.  The code is distributed WITHOUT ANY WARRANTY;
                    without even the implied warranty of MERCHANTABILITY or FITNESS
                    FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.

                    As additional permission under GNU GPL version 3 section 7, you
                    may distribute non-source (e.g., minimized or compacted) forms of
                    that code without the copy of the GNU GPL normally required by
                    section 4, provided you include this license notice and a URL
                    through which recipients can access the Corresponding Source.


                    @licend  The above is the entire license notice
                    for the JavaScript code in this page.
                    */
                </script>

		<script type="text/javascript" src="scripts/transactionhistory.js"></script>

	</head>

	<body>

	  <div id="main-container">
		<div id="bannerlogocontainer">

			<img id="banner_logo" alt="GNU Remote Control" src="images/logo.png" />

			<img id="loadimage" alt="Working..." title="Working..." src="images/ajax-loader.gif" />

		</div>

		<div id="shader">

			<?php

				echo '<span id="header">' . $GRCTL->Value . '</span>';

			?>

			<div id="container" class="maximum-width">

				<div id="inner">

					<span id="message"></span>

					<!--<div id="thermostatscontainer">-->
                        <?php $GRCTL_HLK1 = $resourcesPageLevel->FindResourceByCode('GRCTL$HLK1');?>
                        <div class="display-block right maximum-width" style="margin-top:5px;margin-bottom:-2px;">
                              <a href="index.php<?php echo $sortingURL; ?>"><?php echo $GRCTL_HLK1->Value; ?></a>
                        </div>



						<div class="devices">
                                                    <table id="transactionlog" class="settings maximum-width">

							<tr class="header">

                                                            <?php

                                                                $GRCTL_CLM1 = $resourcesPageLevel->FindResourceByCode('GRCTL$CLM1');
                                                                $GRCTL_CLM2 = $resourcesPageLevel->FindResourceByCode('GRCTL$CLM2');
                                                                $GRCTL_CLM3 = $resourcesPageLevel->FindResourceByCode('GRCTL$CLM3');
                                                                $GRCTL_CLM4 = $resourcesPageLevel->FindResourceByCode('GRCTL$CLM4');
                                                                $GRCTL_CLM5 = $resourcesPageLevel->FindResourceByCode('GRCTL$CLM5');

								echo '<th>'.$GRCTL_CLM1->Value.'</th>';
								echo '<th>'.$GRCTL_CLM2->Value.'</th>';
								echo '<th>'.$GRCTL_CLM3->Value.'</th>';
								echo '<th>'.$GRCTL_CLM4->Value.'</th>';
								echo '<th>'.$GRCTL_CLM5->Value.'</th>';

                                                            ?>

							</tr>

							<tr id="filterrowheader" coslpan="5">
											<td colspan="5">

												<div>

                                                                                                    <?php

                                                                                                        $GRCTL_LBL1 = $resourcesPageLevel->FindResourceByCode('GRCTL$LBL1');
                                                                                                        $GRCTL_HLK2 = $resourcesPageLevel->FindResourceByCode('GRCTL$HLK2');

													echo '<span>'.$GRCTL_LBL1->Value.'</span><a id="clearfilterlink" href="javascript:void(0);" onclick="clearFilter();">'.$GRCTL_HLK2->Value.'</a>';

                                                                                                    ?>

												</div>

											</td>

										</tr>

										<tr class="fieldrow">

											<td><input type="text" id="thermNameFilter" onchange="filter(event);" onkeyup="filter(event);" /></td>
											<td><input type="text" id="fieldFilter" onchange="filter(event);" onkeyup="filter(event);" /></td>
											<td><input type="text" id="oldValFilter" onchange="filter(event);" onkeyup="filter(event);" /></td>
											<td><input type="text" id="newValFilter" onchange="filter(event);" onkeyup="filter(event);" /></td>
											<td><input type="text" id="timestampFilter" onchange="filter(event);" onkeyup="filter(event);" /></td>

										</tr>

							<?php

							$dataAccess = new MySqlDataAccess(Common::ConnectionString);
							$dataAccess->Command->CommandType = CommandType::StoredProcedure;
							$dataAccess->Command->CommandText = "CALL sp_Transaction_Log_Load_All($currentUser->ID)";
							$dataAccess->GetData();

							$transrecords = $dataAccess->ResultSet;

							$counter = 0;

							while($transrecord = $transrecords->fetch_assoc())
							{
								if($counter % 2 == 0)
								{
									echo '<tr class="alt">';
								}
								else
								{
									echo '<tr>';
								}

								echo '<td>'.$transrecord['Name'].'</td>';
								echo '<td>'.$transrecord['FieldName'].'</td>';
								echo '<td>'.$transrecord['OldValue'].'</td>';
								echo '<td>'.$transrecord['NewValue'].'</td>';
								echo '<td>'.$transrecord['TransactionTimeStamp'].'</td>';
								echo '</tr>';

								$counter++;
							}

							$dataAccess->Cleanup();

							?>

						</table>
						</div>

					<!--</div>-->

				</div>

			</div>

		</div>

	</body>

</html>
