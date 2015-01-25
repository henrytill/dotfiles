/*
 *     Copyright 200X Brendan Johan Lee
 * 
 *     This file is part of keysnail pinboard plugin.
 * 
 *     keysnail pinboard plugin is free software: you can redistribute
 *     it and/or modify it under the terms of the GNU General Public
 *     License as published by the Free Software Foundation, either
 *     version 3 of the License, or (at your option) any later
 *     version.
 * 
 *     keysnail pinboar plugin is distributed in the hope that it will
 *     be useful, but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE. See the GNU General Public License for more details.
 * 
 *     You should have received a copy of the GNU General Public
 *     License along with Foobar. If not, see
 *     http://www.gnu.org/licenses/.
 * 
 */
var PLUGIN_INFO =
<KeySnailPlugin>
    <name>pinboard</name>
    <description>Bookmark pages to pinboard</description>
    <version>1.0.1</version>
    <updateURL>http://github.com/deadcyclo/keysnail-plugins/raw/master/pinboard/pinboard.ks.js</updateURL>
    <iconURL>http://github.com/deadcyclo/keysnail-plugins/raw/master/pinboard/icon.png</iconURL>
    <author mail="deadcyclo@vanntett.net" homepage="http://brendan.is">brendan.is</author>
    <license>GPL v3</license>
    <minVersion>1.8.5</minVersion>
    <include>main</include>
    <detail><![CDATA[
=== About ===

This plugin allows you to interact with pinboard.in. You can bookmark the current page, store the current page as a read later bookmark and store your current tabset. In addition you get shortcuts to access your unread bookmarks, all of your bookmarks and your stored tabsets.

==== Use keysnail pinboard ====

Paste code below to your .keysnail.js file.

>|javascript|
key.setGlobalKey(['C-d', 'd'], function (ev, arg) {
    ext.exec("pinboard-save", arg, ev);
}, 'Save to pinboard', true);

key.setGlobalKey(['C-d', 'C-d'], function (ev, arg) {
    ext.exec("pinboard-save-read-later", arg, ev);
}, 'Save to pinboard as read later', true);

key.setGlobalKey(['C-d', 't'], function (ev, arg) {
    ext.exec("pinboard-save-tab-set", arg, ev);
}, 'Save current tab set to pinboard', true);

key.setGlobalKey(['C-d', 'C-t'], function (ev, arg) {
    ext.exec("pinboard-view-tab-sets", arg, ev);
}, 'View tabsets on pinboard in new tab', true);

key.setGlobalKey(['C-d', 'a'], function (ev, arg) {
    ext.exec("pinboard-view-all", arg, ev);
}, 'View own bookmarks on pinboard in new tab', true);

key.setGlobalKey(['C-d', 'r'], function (ev, arg) {
    ext.exec("pinboard-view-unread", arg, ev);
}, 'View unread bookmarks on pinboard in new tab', true);
||<

In this example you get the following keybindings:

+ C-d d   - Bookmark current page
+ C-d C-d - Bookmark current page as read later
+ C-d t   - Save current tab set to pinboard
+ C-d C-t - View your saved tab sets on pinboard in a new tab
+ C-d a   - View all your pinboard bookmarks in a new tab
+ C-d r   - View your bookmarks marked as unread on pinboard in a new tab

    ]]></detail>
</KeySnailPlugin>;

ext.add("pinboard-save", function (event, arg) {
    var q = content.document.location;
    var p = document.title;
    var d = '';
    if(content.getSelection)  {
	d=content.getSelection();
    }

    void(open(
	'https://pinboard.in/add?showtags=yes&url='
	    +encodeURIComponent(q)
	    +'&description='
	    +encodeURIComponent(d)
	    +'&title='
	    +encodeURIComponent(p),
	'Pinboard',
	'toolbar=no,width=700,height=500'));
}, "Save to pinboard");

ext.add("pinboard-save-read-later", function (evnt, arg) {
    var q = content.document.location;
    var p = document.title;

    void(open(
	'https://pinboard.in/add?later=yes&noui=yes&jump=close&url='
	    +encodeURIComponent(q)
	    +'&title='
	    +encodeURIComponent(p),
	'Pinboard',
	'toolbar=no,width=100,height=100'));
    display.echoStatusBar("Saved to pinboard");
}, "Save to pinboard as read later");

ext.add("pinboard-view-unread", function (aEvent, args) {
    var tab = gBrowser.addTab('https://pinboard.in/toread');
    gBrowser.selectedTab = tab;
}, "Show your unread bookmarks in new tab");

ext.add("pinboard-view-all", function (aEvent, args) {
    var tab = gBrowser.addTab('https://pinboard.in');
    gBrowser.selectedTab = tab;
}, "Show all your bookmarks in new tab");

ext.add("pinboard-view-tab-sets", function (aEvent, args) {
    var tab = gBrowser.addTab('https://pinboard.in/tabs');
    gBrowser.selectedTab = tab;
}, "Show your tab sets in new tab");

ext.add("pinboard-save-tab-set", function (aEvent, aarg) {
    makeTabList();
}, "Save your curret tabset to pinboard");

function makeTabList() {
    var winz = [];
    var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"].getService(Components.interfaces.nsIWindowMediator);
    var browserEnumerator = wm.getEnumerator("navigator:browser");
    var found = false;

    while (browserEnumerator.hasMoreElements()) {
        var win = browserEnumerator.getNext();
        var browser = win.gBrowser;
        var num = browser.browsers.length;
        var tabz = [];
        for (var i = 0; i < num; i++) {
            var curr = browser.getBrowserAtIndex(i);
            var url = curr.currentURI.spec;
            var title = curr.contentTitle;
            tabz.push({ title : title, url: url });
        }
        winz.push(tabz);
    }
            
    var result = { browser: "ffox",  windows: winz }; 
    var req    = new XMLHttpRequest(); 
    var json = JSON.stringify(result);
    var params = new FormData();
    
    params.append("data", json);
    req.open("POST", 'https://pinboard.in/tabs/save/', true);        
        
    req.onreadystatechange = function() {
        if (req.readyState == 4) {
	    tab = gBrowser.addTab('https://pinboard.in/tabs/show/');
	    gBrowser.selectedTab = tab;
        }
    };
    req.send(params);
}