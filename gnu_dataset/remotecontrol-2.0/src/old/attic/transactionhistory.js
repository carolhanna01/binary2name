/**
 *
 * @licstart  The following is the entire license notice for the 
 *  JavaScript code in this page.
 *
 * Copyright (C) 2012-2013 GNU remotecontrol authors
 *
 *
 * The JavaScript code in this page is free software: you can
 * redistribute it and/or modify it under the terms of the GNU
 * General Public License (GNU GPL) as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option)
 * any later version.  The code is distributed WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.
 *
 * As additional permission under GNU GPL version 3 section 7, you
 * may distribute non-source (e.g., minimized or compacted) forms of
 * that code without the copy of the GNU GPL normally required by
 * section 4, provided you include this license notice and a URL
 * through which recipients can access the Corresponding Source.
 *
 * @licend  The above is the entire license notice
 * for the JavaScript code in this page.
 *
 */

function filter(e)
{
	if(!e)
	{
		e = window.event;
	}

	//alert('event type = ' + e.type);
	
	if((e.keyCode != 8 &&
		(e.keyCode < 46 || e.keyCode > 90) &&
		(e.keyCode < 96 || e.keyCode > 111) &&
		e.keyCode < 186) &&
		e.button != 0 && e.button != 1)
	{
		return;
	}

	var transactionLogTable = document.getElementById('transactionlog');

	if(e.keyCode == 8 || e.keyCode == 46)
	{
		for(var i = 0; i < transactionLogTable.rows.length; i++)
		{
			transactionLogTable.rows[i].style.display = '';
		}
	}
	
	var filterFields = new Array();
	filterFields[0] = document.getElementById('thermNameFilter');
	filterFields[1] = document.getElementById('fieldFilter');
	filterFields[2] = document.getElementById('oldValFilter');
	filterFields[3] = document.getElementById('newValFilter');
	filterFields[4] = document.getElementById('timestampFilter');
	
	for(i = 0; i < filterFields.length; i++)
	{
		var filterValue = filterFields[i].value;

		for(var j = 3; j < transactionLogTable.rows.length; j++)
		{
			var targetCellIndex = 0;

			switch(filterFields[i].id)
			{
				case 'thermNameFilter':

					targetCellIndex = 0;

					break;

				case 'fieldFilter':

					targetCellIndex = 1;

					break;

				case 'oldValFilter':

					targetCellIndex = 2;

					break;

				case 'newValFilter':

					targetCellIndex = 3;

					break;

				case 'timestampFilter':

					targetCellIndex = 4;

					break;
			}

			if(transactionLogTable.rows[j].style.display != 'none')
			{
				if(transactionLogTable.rows[j].cells[targetCellIndex].innerHTML.toUpperCase().search(filterValue.toUpperCase()) == -1)
				{
					transactionLogTable.rows[j].style.display = 'none';
				}
			}	
		}
	}

	reapplyAlternatingRowStyle(transactionLogTable);
}

function reapplyAlternatingRowStyle(table)
{
	var countVisible = 0;
	var i = 2;

	for(i = 2; i < table.rows.length; i++)
	{
		if(table.rows[i].style.display != 'none')
		{
			table.rows[i].setAttribute('rowindex', countVisible.toString());

			countVisible++;
		}
	}

	for(i = 2; i < table.rows.length; i++)
	{
		if(table.rows[i].style.display != 'none')
		{
			if(parseInt(table.rows[i].attributes.getNamedItem('rowindex').value) % 2 == 0)
			{
				table.rows[i].className = 'alt';
			}
			else
			{
				table.rows[i].className = '';
			}
		}
	}
}

function clearFilter()
{
	document.getElementById('thermNameFilter').value = '';
	document.getElementById('fieldFilter').value = '';
	document.getElementById('oldValFilter').value = '';
	document.getElementById('newValFilter').value = '';
	document.getElementById('timestampFilter').value = '';

	var transactionLogTable = document.getElementById('transactionlog');

	for(var i = 0; i < transactionLogTable.rows.length; i++)
	{
		transactionLogTable.rows[i].style.display = '';
	}

	reapplyAlternatingRowStyle(transactionLogTable);

	document.getElementById('thermNameFilter').focus();
}

window.onload = function(){ document.getElementById('thermNameFilter').focus(); };
