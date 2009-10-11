/**
 * $Id: editor_plugin_src.js 677 2008-03-07 13:52:41Z spocke $
 *
 * @author Moxiecode
 * @copyright Copyright © 2004-2008, Moxiecode Systems AB, All rights reserved.
 */

function disableSelection(target){ 
    if (typeof target.onselectstart!="undefined") //IE route 
        target.onselectstart=function(){return false} ;
    else if (typeof target.style.MozUserSelect!="undefined") //Firefox route 
        target.style.MozUserSelect="none" ;
    else //All other route (ie: Opera) 
        target.onmousedown=function(){return false} 
    target.style.cursor = "default" ;
}

(function() {
	tinymce.create('tinymce.plugins.ZotonicMediaPlugin', {

        init : function(ed, url) {
                var self = this;
                
                // Register commands
                ed.addCommand('mceZotonicMedia', function() {
                        
                        var id = prompt('Enter media ID');
                        if (!id) {
                            return;
                        }


                        ed.execCommand('mceInsertContent', false, '<p>' + self._zMediaHtml(id) + '</p> ', {});
                        disableSelection(ed.dom.get(self._zMediaId(id)).parentNode);

                    });

                ed.onBeforeSetContent.add(function(ed, o) {
                        o.content = self._zMarkersToMediaHtml(o.content);
                    });

                ed.onSetContent.add(function(ed, o) {

                        tinymce.each(ed.dom.select('.' + self._zMediaClass(), o.content), function(e) {
                                disableSelection(e);
                            });
                    
                    });

                ed.onPostProcess.add(function(ed, o) {
                        o.content = self._MediaHtmlToMarkers(o.content);
                    });
                
            
                // Register buttons
                ed.addButton('zmedia', {
                    title : 'Insert a Zotonic media item.',
                            cmd : 'mceZotonicMedia',
                            class: 'mce_image'
                            });
            },
                
            getInfo : function() {
                return {
                longname : 'Zotonic Media Plugin',
                        author : 'Arjan Scherpenisse',
                        authorurl : 'http://www.zotonic.com',
                        infourl : 'http://www.zotonic.com',
                        version : tinymce.majorVersion + "." + tinymce.minorVersion
                        };
            },
                
                
            // Private methods //

            _zMediaClass: function() {
                return "z-tinymce-media";
            },
        
            _zMediaId: function(id) {
                return "z-media-" + id;
            },

            _MediaHtmlToMarkers: function (html) {
                return html.replace(new RegExp('<div class="' + this._zMediaClass() + '.*?id="z-media-([0-9]+).*?</div>', 'g'), '<!-- z-media $1 -->');
            },

            _zMediaHtml: function(id) {
                var divid = this._zMediaId(id);
                return '<div class="' + this._zMediaClass() + '"><img id="' +divid + '" src="/admin/media/preview/' + id + '" /></div>';
            },

            _zMarkersToMediaHtml: function (html) {
                var repl = this._zMediaHtml('$1');
                return html.replace(new RegExp('<!-- z-media ([0-9]+) -->', 'g'), repl);
            },

            _zMediaMarker: function(id) {
                return '<!-- z-media ' + id + ' -->';
            }

        }

        );

	// Register plugin
	tinymce.PluginManager.add('zmedia', tinymce.plugins.ZotonicMediaPlugin);
 })();
