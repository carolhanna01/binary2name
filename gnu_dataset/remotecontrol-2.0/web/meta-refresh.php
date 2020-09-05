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

define( 'NO_LANG_CHECK', true );

require_once __DIR__ . '/../src/include.php';

$url = read_g( 'goto', null );

if ( ! dal()->url->redirect( $url ) ) {
  
  redirect( '/intl-admin.php' );
  
}

$link = aenc( $url );

?>
<!DOCTYPE html>
<html>
  <head><title>Redirecting...</title>
    <meta charset="UTF-8">
    <meta name="robots" content="noindex, nofollow">
    <meta http-equiv="refresh" content="0;url=<?= $link; ?>">
  </head>
  <body>
    
    <div id="loading" style="width:100%;height:100%;text-align:center;padding-top:17%;">
      <img
        src="data:image/gif;base64,R0lGODlhFAAUAJEAAJmZmTMzM////wAAACH/C05FVFNDQVBFMi4wAw
EAAAAh+QQFCgACACwAAAEAEwASAAACN5R/EcgtqhyKDxoAGlz14jxZzAeGEialDqWiFeuQL3d+Bl
yrekKPJY8TyEIU19AktCV1x93SUQAAIfkEBQoAAgAsAwADAA4ADgAAAhiUjxKRvd/OY1HSirPevE
cAaCCYjWF5YgUAIfkEBQoAAgAsAAAIABMABAAAAhGUAKarEYYQY86dOFd9N9NXAAAh+QQFCgACAC
wDAAMADgAOAAACGBQgqWuHvNyD6tCLs968exoEWhhmpGiKBQAh+QQFCgACACwIAAEABAASAAACDx
Rgp5fiD6OcVIYgrtYZFwAh+QQFCgACACwDAAMADgAOAAACGJSPApC9385jUdKKs968xxBoIJiNYX
liBQAh+QQFCgACACwAAAgAEwAEAAACEZQSpqsAhhBjzp04V30301cAACH5BAkKAAIALAAAAQATAB
IAAAIdlI+pyxrR1nsxTVgRzbz7D4biBwBkaXZoqrJjWAAAOw=="
        alt="Loading..."
        title="Loading..."
        style="width:20px;height:20px" />
    </div>

    <div style="text-align:center;margin:2em;">
      <p>Please wait while you are <a href="<?= $link; ?>">transferred</a>...</p>
    </div>
    
  </body>
</html>
