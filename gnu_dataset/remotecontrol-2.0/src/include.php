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

error_reporting( -1 );

ini_set( 'html_errors', false );

require_once __DIR__ . '/1-lib/01-api.php';
require_once __DIR__ . '/1-lib/02-constants.php';
require_once __DIR__ . '/1-lib/10-debug.php';
require_once __DIR__ . '/1-lib/20-array.php';
require_once __DIR__ . '/1-lib/25-types.php';
require_once __DIR__ . '/1-lib/29-conversion.php';
require_once __DIR__ . '/1-lib/30-formatting.php';

if ( ! file_exists( __DIR__ . '/../config.php' ) ) {
  
  die( "You must create the config.php configuration file." );
  
}

require_once __DIR__ . '/../config.php';

require_once __DIR__ . '/1-lib/37-defaults.php';

require_once __DIR__ . '/2-obj/GrcMessage.php';
require_once __DIR__ . '/2-obj/GrcError.php';
require_once __DIR__ . '/2-obj/GrcValidator.php';
require_once __DIR__ . '/2-obj/GrcService.php';

require_once __DIR__ . '/3-dal/GrcDalModule.php';
require_once __DIR__ . '/3-dal/GrcDal.php';

require_once __DIR__ . '/4-orm/GrcSession.php';
require_once __DIR__ . '/4-orm/GrcUser.php';

require_once __DIR__ . '/5-bom/GrcBom.php';

require_once __DIR__ . '/1-lib/35-validation.php';

require_once __DIR__ . '/2-obj/GrcIntl.php';

require_once __DIR__ . '/1-lib/45-intl.php';

require_once __DIR__ . '/1-lib/55-calc.php';
require_once __DIR__ . '/1-lib/60-html.php';
require_once __DIR__ . '/1-lib/65-http.php';
require_once __DIR__ . '/1-lib/70-comparison.php';
require_once __DIR__ . '/1-lib/75-logic.php';
require_once __DIR__ . '/1-lib/80-datetime.php';
require_once __DIR__ . '/1-lib/87-ajax.php';
//require_once __DIR__ . '/1-lib/90-proliphix.php';

require_once __DIR__ . '/2-obj/GrcHtmlElement.php';
require_once __DIR__ . '/2-obj/GrcHtmlComposite.php';
require_once __DIR__ . '/2-obj/GrcHtmlView4.php';
require_once __DIR__ . '/2-obj/GrcHtmlViewX.php';
require_once __DIR__ . '/2-obj/GrcHtmlView5.php';
require_once __DIR__ . '/2-obj/GrcIntl.php';
require_once __DIR__ . '/2-obj/GrcTableView.php';
require_once __DIR__ . '/2-obj/GrcDataPage.php';
require_once __DIR__ . '/2-obj/GrcDataSort.php';
require_once __DIR__ . '/2-obj/GrcPageLinks.php';
require_once __DIR__ . '/2-obj/GrcUrl.php';
require_once __DIR__ . '/2-obj/GrcListShuffler.php';

url()->register_global( 'lang', 'html', 'pretty', 'tz' );

require_once __DIR__ . '/6-view/header.php';
require_once __DIR__ . '/6-view/footer.php';
require_once __DIR__ . '/6-view/library.php';

require_once __DIR__ . '/7-ajax/GrcAjax.php';

require_once __DIR__ . '/1-lib/98-handlers.php';
require_once __DIR__ . '/1-lib/99-screen.php';
