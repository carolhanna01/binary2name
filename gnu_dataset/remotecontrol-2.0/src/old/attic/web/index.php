<?php

require_once __DIR__ . '/../src/include.php';

html(intl()->get_lang());

render_head('Home');

render_page();

render_foot();

html()->render();

function render_page() {

    $node = html()->tail;

    $thermostat_sort = get_thermostat_sort(
            $thermostat_sort_col, $thermostat_sort_dir
    );

    //$user_id = dal()->get_user_id_for_username( $_SERVER[ 'PHP_AUTH_USER' ] );
    $user_id = user()->id;

    $thermostats = dal()->thermostat->get_thermostats_for_user_id(
            $user_id, $thermostat_sort_col, $thermostat_sort_dir
    );

    $tbody = $node->
            div('class', 'devices')->
            table(
                    'id', 'thermostats', 'class', 'settings maximum-width'
            )->
            thead()->
            tr('class', 'header')->
            th('id', 'selectallthermscell')->
            input(
                    'type', 'checkbox', 'id', 'selectalltherms', 'title', A('Select All/Deselect All'), 'onclick', 'toggleSelectAllThermostats(this);'
            )->
            th_end()->
            th('colspan', '2')->markup('&nbsp;')->th_end()->
            th()->
              T_H('Device Name')->
            th_end()->
            th()->
              T_H('Site Name')->
            th_end()->
            th()->
              T_H('Model #')->
            th_end()->
            th()->
            add(
                    get_thermostat_col_html(
                            H('Group'), 'Group'
                    )
            )->
            th_end()->
            th()->
            add(
                    get_thermostat_col_html(
                            H('Custom Name'), 'Name'
                    )
            )->
            th_end()->
            th()->
            add(
                    get_thermostat_col_html(
                            H('Description'), 'Description'
                    )
            )->
            th_end()->
            th()->
            add(
                    get_thermostat_col_html(
                            H('Domain Name/IP'), 'Host'
                    )
            )->
            th_end()->
            th()->
            add(
                    get_thermostat_col_html(
                            H('Port'), 'Port'
                    )
            )->
            th_end()->
            th()->
            add(
                    get_thermostat_col_html(
                            H('Time Zone Offset'), 'TimeZoneOffset'
                    )
            )->
            th_end()->
            th()->
              T_H('Login')->
            th_end()->
            th('colspan', '2')->
            markup('&nbsp;')->
            th_end()->
            tr_end()->
            thead_end()->
            add(get_thermostats_tbody_html($thermostats))->
            add(get_thermostats_tfoot_html($thermostats, $user_id))->
            table_end()->
            input(
                    'type', 'button', 'value', A('Set Time on Selected Thermostats'), 'class', 'commandbutton', 'onclick', 'setTime();'
            )->
            markup('&nbsp;&nbsp;')->
            span('id', 'currdatetime')->
            text(T('%time% (at page load)', 'time', now()->format(DateTime::RFC822)
            ))->
            span_end()->
            div_end()->
            form(
                    'id', 'tstat', 'name', 'tstat', 'action', 'javascript:submitForm();', 'method', 'POST'
            )->
            div(
                    'id', 'hvacsettingssection', 'class', 'section half-width'
            )->
            div(
                    'class', 'sectionheader'
            )->
            span(
                    'class', 'sectionheader'
            )->
              T_H('HVAC Settings')->
            span_end()->
            div_end()->
            div(
                    'class', 'sectioninner quarter-height'
            )->
            table(
                    'class', 'settings maximum-width'
            )->
            thead()->
            tr('class', 'header')->
            th()->text(T('HVAC Mode'))->th_end()->
            th()->text(T('Fan Mode'))->th_end()->
            th()->text(T('Setback Status'))->th_end()->
            th()->text(T('Setback Heat'))->th_end()->
            th()->text(T('Setback Cool'))->th_end()->
            tr_end()->
            thead_end()->
            tbody()->
            tr()->
            td()->
            select(
                    'id', 'hvacmode', 'name', 'OID4.1.1', 'class', 'fieldvalue', 'onchange', 'setHVACModeDDLBackground(this);'
            )->
            option(
                    'value', '1'
            )->
            text(T('Off'))->
            option_end()->
            option(
                    'value', '2'
            )->
            text(T('Heat'))->
            option_end()->
            option(
                    'value', '3'
            )->
            text(T('Cool'))->
            option_end()->
            option(
                    'value', '4'
            )->
            text(T('Auto'))->
            option_end()->
            select_end()->
            td_end()->
            td()->
            select(
                    'id', 'fanmode', 'name', 'OID4.1.3', 'class', 'fieldvalue'
            )->
            option('value', '1')->
            text(T('Auto'))->
            option_end()->
            option('value', '2')->
            text(T('On'))->
            option_end()->
            option('value', '3')->
            text(T('Schedule'))->
            option_end()->
            select_end()->
            td_end()->
            td()->
            select(
                    'id', 'setbackstatus', 'name', 'OID4.1.9', 'class', 'fieldvalue'
            )->
            option('value', '1')->
            text(T('Normal'))->
            option_end()->
            option('value', '2')->
            text(T('Hold'))->
            option_end()->
            option('value', '3')->
            text(T('Override'))->
            option_end()->
            select_end()->
            td_end()->
            td()->
            add(
                    get_setback_temp_select_html('setbackheat', 'OID4.1.5')
            )->
            td_end()->
            td()->
            add(
                    get_setback_temp_select_html('setbackcool', 'OID4.1.6')
            )->
            td_end()->
            tr_end()->
            tbody_end()->
            table_end()->
            div_end()->
            div_end()->
            div(
                    'id', 'sensorsettingssection', 'class', 'section half-width'
            )->
            div(
                    'class', 'sectionheader'
            )->
            span(
                    'class', 'sectionheader'
            )->
              T_H('Sensor Settings')->
            span_end()->
            div_end()->
            div(
                    'class', 'sectioninner quarter-height'
            )->
            table(
                    'class', 'settings maximum-width'
            )->
            thead()->
            tr('class', 'header')->
            th('class', 'empty')->markup('&nbsp;')->th_end()->
            th()->markup('State')->th_end()->
            th()->markup('Averaging')->th_end()->
            th()->markup('Correction')->th_end()->
            tr_end()->
            thead_end()->
            tbody()->
            tr()->
            td('class', 'sensorlabelcell')->
              T_H('Local Sensor')->
            td_end()->
            td()->
            select(
                    'id', 'localsensorstate', 'name', 'OID4.3.6.1', 'class', 'fieldvalue', 'onchange', "setAveraging('L',this.value);"
            )->
            option('value', '1')->
              T_H('Disabled')->
            option_end()->
            option('value', '2')->
              T_H('Enabled')->
            option_end()->
            select_end()->
            td_end()->
            td()->
            select(
                    'id', 'localsensoraverage', 'name', 'OID4.3.8.1', 'class', 'fieldvalue'
            )->
            option('value', '1')->
              T_H('Disabled')->
            option_end()->
            option('value', '2')->
              T_H('Enabled')->
            option_end()->
            select_end()->
            td_end()->
            td()->markup('&nbsp;')->td_end()->
            tr_end()->
            tr()->
            td(
                    'class', 'sensorlabelcell'
            )->
            markup('Remote Sensor 1')->
            td_end()->
            td()->
            select(
                    'id', 'remotesensor1state', 'name', 'OID4.3.6.2', 'class', 'fieldvalue', 'onchange', "setAveraging('R1',this.value);"
            )->
            option('value', '1')->
              T_H('Disabled')->
            option_end()->
            option('value', '2')->
              T_H('Enabled')->
            option_end()->
            select_end()->
            td_end()->
            td()->
            select(
                    'id', 'remotesensor1average', 'name', 'OID4.3.8.2', 'class', 'fieldvalue'
            )->
            option('value', '1')->
              T_H('Disabled')->
            option_end()->
            option('value', '2')->
              T_H('Enabled')->
            option_end()->
            select_end()->
            td_end()->
            td()->
            add(get_remotesensor_correction_select_html(
                            'remotesensor1correction', 'OID4.3.4.2'
            ))->
            td_end()->
            tr_end()->
            tr()->
            td(
                    'class', 'sensorlabelcell'
            )->
            markup('Remote Sensor 2')->
            td_end()->
            td()->
            select(
                    'id', 'remotesensor2state', 'name', 'OID4.3.6.3', 'class', 'fieldvalue', 'onchange', "setAveraging('R2',this.value);"
            )->
            option('value', '1')->
              T_H('Disabled')->
            option_end()->
            option('value', '2')->
              T_H('Enabled')->
            option_end()->
            select_end()->
            td_end()->
            td()->
            select(
                    'id', 'remotesensor2average', 'name', 'OID4.3.8.3', 'class', 'fieldvalue'
            )->
            option('value', '1')->
              T_H('Disabled')->
            option_end()->
            option('value', '2')->
              T_H('Enabled')->
            option_end()->
            select_end()->
            td_end()->
            td()->
            add(get_remotesensor_correction_select_html(
                            'remotesensor2correction', 'OID4.3.4.3'
            ))->
            td_end()->
            tr_end()->
            tbody_end()->
            table_end()->
            div_end()->
            div_end()->
            div('class', 'section')->
            div('class', 'sectionheader')->
            span('class', 'subsectionheader')->
              T_H('Setback Scheduling')->
            span_end()->
            div_end()->
            div('class', 'sectioninner')->
            div('class', 'subsection')->
            span('class', 'subsectionheader')->
              T_H('Day Class Schedules')->
            span_end()->
            table(
                    'id', 'dayclasstable', 'class', 'maximum-width'
            )->
            thead()->
            tr('id', 'dayclassheader1')->
            th('class', 'empty')->markup('&nbsp;')->th_end()->
            th(
                    'colspan', '4', 'class', 'occupied'
            )->
              T_H('Occupied')->
            th_end()->
            th(
                    'colspan', '4', 'class', 'unoccupied'
            )->
              T_H('Unoccupied')->
            th_end()->
            th(
                    'colspan', '4', 'class', 'other'
            )->
              T_H('Other')->
            th_end()->
            tr_end()->
            tr(
                    'id', 'dayclassheader2', 'class', 'header'
            )->
            th()->T_H('Period')->th_end()->
            th()->T_H('Time')->th_end()->
            th('class', 'heat')->T_H('Heat')->th_end()->
            th('class', 'cool')->T_H('Cool')->th_end()->
            th()->T_H('Fan')->th_end()->
            th()->T_H('Time')->th_end()->
            th('class', 'heat')->T_H('Heat')->th_end()->
            th('class', 'cool')->T_H('Cool')->th_end()->
            th()->T_H('Fan')->th_end()->
            th()->T_H('Time')->th_end()->
            th('class', 'heat')->T_H('Heat')->th_end()->
            th('class', 'cool')->T_H('Cool')->th_end()->
            th()->T_H('Fan')->th_end()->
            tr_end()->
            thead_end()->
            tbody();

    for ($i = 1; $i <= 4; $i++) {

        $tr = $tbody->tr();

        $td = $tr->td('class', 'periodlabelcell');

        switch ($i) {

            case 1:

                $td->T_H('Morning');

                break;

            case 2:

                $td->T_H('Day');

                break;

            case 3:

                $td->T_H('Evening');

                break;

            case 4:

                $td->T_H('Night');

                break;
        }

        for ($j = 1; $j <= 3; $j++) {

            $th = $tr->th('id', 'OID4.4.1.3.' . $j . '.' . $i);

            $th->
                    select('id', 'setbackPeriodHour' . $j . '.' . $i)->
                    get($select);

            for ($k = 1; $k <= 12; $k++) {

                $select->option('value', $k)->text($k);
            }

            $th->
                    select('id', 'setbackPeriodMinute' . $j . '.' . $i)->
                    get($select);

            for ($k = 0; $k <= 55; $k += 5) {

                if ($k < 10) {

                    $select->option('value', '0' . $k)->text('0' . $k);
                } else {

                    $select->option('value', $k)->text($k);
                }
            }

            $th->
                    div()->
                    input(
                            'type', 'radio', 'id', 'setbackPeriodTimeSuffixAM' . $j . '.' . $i, 'name', 'timesuffix' . $j . '.' . $i, 'value', 'AM'
                    )->
                    span('class', 'radiobutton')->
                    T_H('AM')->
                    span_end()->
                    markup('&nbsp;&nbsp;')->
                    input(
                            'type', 'radio', 'id', 'setbackPeriodTimeSuffixPM' . $j . '.' . $i, 'name', 'timesuffix' . $j . '.' . $i, 'value', 'PM'
                    )->
                    span('class', 'radiobutton')->
                    T_H('PM')->
                    span_end()->
                    div_end()->
                    th_end()->
                    td()->
                    select(
                            'id', 'setbackHeat' . $j . '.' . $i, 'name', 'OID4.4.1.4.' . $j . '.' . $i
                    )->
                    get($select);

            for ($k = 99; $k >= 40; $k--) {

                $select->option('value', ( $k * 10))->text($k);
            }

            $select->
                    select_end()->
                    td_end()->
                    td()->
                    select(
                            'id', 'setbackCool' . $j . '.' . $i, 'name', 'OID4.4.1.5.' . $j . '.' . $i
                    )->
                    get($select);

            for ($k = 99; $k >= 40; $k--) {

                $select->option('value', ( $k * 10))->text($k);
            }

            $select->
                    select_end()->
                    td_end()->
                    td()->
                    select(
                            'id', 'setbackFan' . $j . '.' . $i, 'name', 'OID4.4.1.6.' . $j . '.' . $i
                    )->
                    option(
                            'value', '1'
                    )->
                    text(T('Off'))->
                    option_end()->
                    option(
                            'value', '2'
                    )->
                    text(T('On'))->
                    option_end()->
                    get($select);

            for ($k = 15; $k <= 45; $k += 15) {

                $select->option('value', $k)->text($k);
            }
        }
    }

    $select->
            select_end()->
            td_end()->
            tr_end()->
            tbody_end()->
            table_end()->
            div_end()->
            div('class', 'subsection')->
            span('class', 'subsectionheader')->
            text(T('Default Weekly Schedule'))->
            span_end()->
            table(
                    'id', 'defaultclasstable', 'class', 'settings maximum-width'
            )->
            thead()->
            tr('class', 'header')->
            th()->
            text(T('Sunday'))->
            th_end()->
            th()->
            text(T('Monday'))->
            th_end()->
            th()->
            text(T('Tuesday'))->
            th_end()->
            th()->
            text(T('Wednesday'))->
            th_end()->
            th()->
            text(T('Thursday'))->
            th_end()->
            th()->
            text(T('Friday'))->
            th_end()->
            th()->
            text(T('Saturday'))->
            th_end()->
            tr_end()->
            thead_end()->
            tbody()->
            tr()->
            get($tr);

    for ($i = 1; $i <= 7; $i++) {

        $tr->
                td()->
                select(
                        'name', 'OID4.4.3.2.' . $i, 'onchange', 'setDefaultClassDDLBackground(this);'
                )->
                option('value', '1', 'class', 'occupied')->
                text(T('Occupied'))->
                option_end()->
                option('value', '2', 'class', 'unoccupied')->
                text(T('Unoccupied'))->
                option_end()->
                option('value', '3', 'class', 'other')->
                text(T('Other'))->
                option_end()->
                select_end();
    }

    $tr->
            tr_end()->
            tbody_end()->
            table_end()->
            div_end()->
            div_end()->
            div_end()->
            form_end()->
            div('class', 'section')->
            div('class', 'sectionheader')->
            input(
                    'type', 'submit', 'name', 'submit', 'value', A('Submit'), 'onclick', 'scrollTo(0,0);return validateForm();', 'class', 'commandbutton'
            )->
            div_end()->
            div_end()->
            div('id', 'authinfo')->
            div('class', 'sectioninner')->
            div('style', 'display:block;')->
            label(
                    'id', 'UserNameLabel', 'for', 'UserName_ThermID_Input', 'class', 'fieldlabel'
            )->
            text(T('Username:'))->
            label_end()->
            div_end()->
            div('style', 'display:block;')->
            input(
                    'type', 'text', 'id', 'UserName_ThermID_Input', 'value', 'admin'
            )->
            div_end()->
            div('style', 'display:block;')->
            label(
                    'id', 'PasswordLabel', 'for', 'Password_ThermID_Input', 'class', 'fieldlabel'
            )->
            text(T('Password:'))->
            label_end()->
            div_end()->
            div('style', 'display:block;')->
            input(
                    'type', 'password', 'id', 'Password_ThermID_Input'
            )->
            div_end()->
            div('style', 'margin-top:10px;')->
            div('style', 'float:left;')->
            input(
                    'type', 'button', 'id', 'AuthInfoSaveButton', 'value', A('Save'), 'onclick', "updateAuthInfo('ThermID');", 'class', 'commandbutton'
            )->
            div_end()->
            div('style', 'float:left;')->
            input(
                    'type', 'button', 'id', 'AuthInfoCancelButton', 'value', A('Cancel'), 'onclick', "toggleAuthInfo(false,'ThermID');", 'class', 'commandbutton'
            )->
            div_end()->
            div_end()->
            div_end()->
            div_end()->
            div_end()->
            div_end()->
            div_end()->
            div_end();

    $firstthermid = read_a($thermostats, 0, 'ThermostatId');

    $node->
            script(
                    'type', 'text/javascript', 'defer', true
            )->
            markup("
window.onload = function() { populateForm(" . $firstthermid . ", false); };
")->
            script_end()->
            input('type', 'hidden', 'id', 'messageHidden')->
            input('type', 'hidden', 'id', 'tzoDDLItems')->
            input('type', 'hidden', 'id', 'groupDDLItems');
}
