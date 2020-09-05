/*
  GNUzilla privacy features
  Copyright (C) 2008, 2009, 2010 Free Software
  Foundation, Inc.
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

var cookiesHost = new Object ();


var privacyFeaturesCookiesObserver =
    {
        observe: function (subject, topic, data)
        {
            if (topic == "http-on-modify-request")
            {
                var httpChannel = subject.QueryInterface (Components.interfaces.nsIHttpChannel);
                var cookie = null;

                try
                {
                    cookie = httpChannel.getRequestHeader ("Cookie");
                }
                catch(e){}

                if(httpChannel.notificationCallbacks)
                {
                    var interfaceRequestor = httpChannel.notificationCallbacks
                        .QueryInterface(Components.interfaces.nsIInterfaceRequestor);

                    var targetDoc = interfaceRequestor.getInterface(Components.interfaces.nsIDOMWindow).document;

                    var targetBrowserIndex = gBrowser.getBrowserIndexForDocument(targetDoc);

                    if(cookie != null && targetBrowserIndex > -1
                       && targetDoc.location.host != httpChannel.URI.host)
                    {
                        var stringsBundle = document.getElementById("string-bundle");
                        var messageString = stringsBundle.getString('third_party_cookie');
                        var notificationBox = gBrowser.getNotificationBox();

                        var blockButton = {
                            _outer: self,
                            label: stringsBundle.getString('block_site_cookies') + " " + httpChannel.URI.host,
                            accessKey: null,
                            callback:
                            function blockButtonCallback(aNotification, aButtonInfo) {
                                var perm = Components.classes["@mozilla.org/cookie/permission;1"]
                                    .getService(Components.interfaces.nsICookiePermission);
                                perm.setAccess(httpChannel.URI, Components.interfaces.nsICookiePermission.ACCESS_DENY);
                                return false;
                            }
                        };
                        buttons = [blockButton];

                        var now = (new Date()).getTime();
                        var time = cookiesHost[httpChannel.URI.host];


                        if(time == null || (now - time > 10000))
                        {
                            notificationBox.appendNotification(messageString,
                                                               null,
                                                               null,
                                                               notificationBox.PRIORITY_WARNING_MEDIUM,
                                                               buttons);
                        }

                        cookiesHost[httpChannel.URI.host] = now;
                    }//if
                }//httpChannel.notificationCallbacks
            }
        }
    };

var PrivacyFeatures = {
    onLoad: function(e) {
        var observerService = Components.classes["@mozilla.org/observer-service;1"]
            .getService(Components.interfaces.nsIObserverService);

        observerService.addObserver(privacyFeaturesCookiesObserver,
                                    "http-on-modify-request", false);
    },
};

window.addEventListener("load", function(e) { PrivacyFeatures.onLoad(e); }, false);


function checkLinkRewritten (ev)
{
    if (ev.attrName == "href")
    {
        var stringsBundle = document.getElementById ("string-bundle");
        var messageString = stringsBundle.getString ('link_rewritten');
        var notificationBox = gBrowser.getNotificationBox ();
        notificationBox.appendNotification (messageString,
                                            null,
                                            null,
                                            notificationBox.PRIORITY_WARNING_MEDIUM,
                                            null);
    }
}

window.addEventListener("load", function(ev)
                        {
                            var appcontent = document.getElementById("appcontent");
                            if(appcontent)
                                appcontent.addEventListener("DOMContentLoaded", function (ev)
                                                            {
                                                                var doc = ev.originalTarget;
                                                                doc.addEventListener("DOMAttrModified",
                                                                                     checkLinkRewritten, true);
                                                            }, false);
                        }, false);

function checkIfJs (prefName, message)
{
    var prefs = Components.classes["@mozilla.org/preferences-service;1"]
        .getService (Components.interfaces.nsIPrefService)
        .getBranch ("extensions.gnuzilla_privacy_features.");

    var allow = prefs.prefHasUserValue(prefName)
        && prefs.getBoolPref(prefName);

    if (! allow)
    {
        var stringsBundle = document.getElementById("string-bundle");
        var messageString = stringsBundle.getString(message);
        var notificationBox = gBrowser.getNotificationBox();
        notificationBox.appendNotification(messageString,
                                           null,
                                           null,
                                           notificationBox.PRIORITY_WARNING_MEDIUM,
                                           null);
    }
}

function checkIfJsURL (e)
{
    if (document.getElementById ("urlbar").value.indexOf ("javascript:") != 0)
        return;

    checkIfJs ('allowJsAddressBar', 'javascript_url');
}

document.getElementById ("urlbar").addEventListener ("input", checkIfJsURL, false);

function checkIfJsBookmark (url)
{
    if (url.indexOf ("javascript:") != 0)
        return;

    checkIfJs ('allowJsBookmarklets', 'javascript_bookmark');
}

var privacyFeaturesBookmarkListener = {
    onBeginUpdateBatch: function() {},
    onEndUpdateBatch: function() {},
    onItemAdded: function(aItemId, aFolder, aIndex) {
        checkIfJsBookmark (bmsvc.getBookmarkURI(aItemId).spec);
    },
    onItemRemoved: function(aItemId, aFolder, aIndex) {},
    onBeforeItemRemoved: function(aItemId, aFolder, aIndex) {},
    onItemChanged: function(aBookmarkId, aProperty, aIsAnnotationProperty, aValue) {},
    onItemVisited: function(aBookmarkId, aVisitID, time) {},
    onItemMoved: function(aItemId, aOldParent, aOldIndex, aNewParent, aNewIndex) {},
    QueryInterface: XPCOMUtils.generateQI([Components.interfaces.nsINavBookmarkObserver])
};

var bmsvc = Components.classes["@mozilla.org/browser/nav-bookmarks-service;1"]
    .getService(Components.interfaces.nsINavBookmarksService);

bmsvc.addObserver(privacyFeaturesBookmarkListener, false);

var privacyFeaturesCacheListener = {
	  cs: Components.classes["@mozilla.org/consoleservice;1"].getService (Components.interfaces.nsIConsoleService),
	  hc: Components.interfaces.nsIHttpChannel,

    initLists: function ()
    {
    },

    observe: function (subject, topic, data)
    {
        if (topic == "canvas-draw-image")
        {
            var cacheService = Components.classes["@mozilla.org/network/cache-service;1"].getService (Components.interfaces.nsICacheService)
            var writeAccess = Components.interfaces.nsICache.ACCESS_WRITE;
            var cacheSession = cacheService.createSession ("HTTP", 0, true);
            cacheSession.doomEntriesIfExpired = false;
            var file = cacheSession.openCacheEntry (subject.src, writeAccess, false);
            if (file)
            {
                file.setExpirationTime (0);
                file.close ();
            }
        }
       else if (topic == "http-on-examine-response")
        {
            var httpChannel = subject.QueryInterface (this.hc);

            var prefs = Components.classes["@mozilla.org/preferences-service;1"]
                .getService (Components.interfaces.nsIPrefService)
                .getBranch ("extensions.gnuzilla_privacy_features.");

            var seconds = 2 * 24 * 60 * 60;

            if (prefs.prefHasUserValue("max_cache_time"))
                seconds = prefs.getIntPref("max_cache_time");
        		var now = new Date();
            var expires = now.getTime () + 1000 * seconds;

            try
            {
        				var headerval = httpChannel.getResponseHeader ("Expires");
                var date = new Date (headerval).getTime ();
                if (date < expires)
                    expires = date;
            }
            catch (e) {}

            d = new Date (expires);
        		httpChannel.setResponseHeader ("Expires", d.toGMTString (), false);

            if (httpChannel.responseStatus == 404)
            {
                var browserHistory = Components.classes["@mozilla.org/browser/nav-history-service;1"]
                    .getService (Components.interfaces.nsIBrowserHistory);
                var historyService = Components.classes["@mozilla.org/browser/nav-history-service;1"]
                    .getService (Components.interfaces.nsIGlobalHistory2);

                var ioService = Components.classes["@mozilla.org/network/io-service;1"]
                    .getService (Components.interfaces.nsIIOService);

                var prevUriSpec = httpChannel.originalURI.spec.substr (0, httpChannel.originalURI.spec.length - 1);
                var prevUri = ioService.newURI (prevUriSpec,
                                                httpChannel.originalURI.originCharset,
                                                httpChannel.originalURI);

                if ((prevUri.spec.substr (prevUri.spec.length - 1, prevUri.spec.length) != "/")
                    && historyService.isVisited (prevUri))
                {
                    browserHistory.removePage (prevUri);
                }
            }
        }
    }
};

var cacheListener =
    {
        init: function()
        {
            var observerService = Components.classes["@mozilla.org/observer-service;1"].getService(Components.interfaces.nsIObserverService);
            observerService.addObserver (privacyFeaturesCacheListener, "http-on-examine-response", false);
            observerService.addObserver (privacyFeaturesCacheListener, "canvas-draw-image", false);
        }
    };

window.addEventListener ("load", cacheListener.init, false);
