/**
 * Set the image options or select an image to insert.
 *
 * @author Arjan Scherpenisse <arjan@scherpenisse.net>
 * @author Moxiecode
 * @copyright 2010-2012 Arjan Scherpenisse <arjan@scherpenisse.net>
 * @copyright Copyright 2004-2008, Moxiecode Systems AB, All rights reserved.
 */

(function () {
    "use strict";
    window.tinyMCEzMedia = {
        toHTML: function (id, opts) {
            var cls,
                html;
            cls = "z-tinymce-media z-tinymce-media-align-" + (opts.align || "block");
            html = '<img class="'
                + cls + '" '
                + 'data-zmedia-id="' + id + '" '
                + 'data-zmedia-opts="' + JSON.stringify(opts).replace(/\"/g, '&quot;') + '" '
                + ' src="/admin/media/preview/' + id + '" />';
            return html;
        }
    };

    tinymce.create("tinymce.plugins.ZotonicMediaPlugin", {
        init : function (ed, url) {
            var self = this,
                properties_dialog_enabled,
                insert_dialog_enabled,
                DEFAULT_ATTRIBUTES = {
                    align: "block",
                    size: "middle",
                    crop: "",
                    link: ""
                };

            // Zotonic settings
            properties_dialog_enabled = ed.getParam("z_properties_dialog_enabled", true);
            insert_dialog_enabled = ed.getParam("z_insert_dialog_enabled", true);

            // Register commands

            ed.onSetContent.add(function (ed, o) {
                if (properties_dialog_enabled) {
                    // show hover state
                    var $iframe = $(ed.getContainer()).find("iframe");
                    $iframe.contents().find("img.z-tinymce-media").addClass("z-active");
                }
            });

            ed.addCommand("mceZotonicMedia", function (ui) {
                var n = ui || ed.selection.getNode();
                if (n && self._domIsZMedia(n)) {
                    if (properties_dialog_enabled) {
                        self._openPropertiesDialog(n);
                    }
                } else if (insert_dialog_enabled) {
                    self._openInsertDialog(ed, DEFAULT_ATTRIBUTES);
                }
            });

            ed.onClick.add(function (ed, o) {
                if (o.srcElement.nodeName === "IMG") {
                    ed.execCommand("mceZotonicMedia", o.srcElement);
                }
            });

            ed.onPostProcess.add(function (ed, o) {
                o.content = self._MediaHtmlToMarkers(o.content);
            });

            ed.onLoadContent.add(function (ed, o) {
                ed.setContent(self._zMarkersToMediaHtml(o.content));
            });

            ed.onBeforeSetContent.add(function (ed, o) {
                o.content = self._zMarkersToMediaHtml(o.content);
            });

            // Register buttons
            ed.addButton("zmedia", {
                title : "Insert a Zotonic media item.",
                cmd : "mceZotonicMedia",
                "class": "mce_image"
            });
        },

        getInfo : function () {
            return {
                longname : "Zotonic Media Plugin",
                author : "Arjan Scherpenisse",
                authorurl : "http://www.zotonic.com",
                infourl : "http://www.zotonic.com",
                version : tinymce.majorVersion + "." + tinymce.minorVersion
            };
        },

        // Private methods //

        _openPropertiesDialog: function (node) {
            var id = this._zMediaIdFromDOM(node),
                opts = this._zMediaOptsFromDOM(node);

            z_dialog_open({
                title: "Media properties",
                text: '<form id="zmedia-props-form" class="form">' +
                    '  <div class="form-group row">' +
                    '      <div class="col-md-6">' +
                    '          <img style="width: 100%" src="/admin/media/preview/' + id + '" class="z-tinymce-media-left" />' +
                    '      </div>'  +
                    '      <div class="col-md-3">' +
                    '                  <div class="form-group">' +
                    '                      <label class="control-label">Alignment</label>' +
                    '                      <div class="controls">' +
                    '                          <div class="radio"><label><input type="radio" name="align" ' + (opts.align === 'block' ? 'checked="checked"' : '') + ' value="block" id="a-block"> Between text</label></div>' +
                    '                          <div class="radio"><label><input type="radio" name="align" ' + (opts.align === 'left' ? 'checked="checked"' : '') + 'value="left" id="a-left"> Aligned left</label></div>' +
                    '                          <div class="radio"><label><input type="radio" name="align" ' + (opts.align === 'right' ? 'checked="checked"' : '') + 'value="right" id="a-right"> Aligned right</label></div>' +
                    '                      </div>' +
                    '                  </div>' +
                    '                  <div class="form-group">' +
                    '                      <label class="control-label">Crop</label>' +
                    '                      <div class="controls">' +
                    '                          <div class="checkbox"><label><input type="checkbox" name="crop" ' + (opts.crop === 'crop' ? 'checked="checked"' : '') + ' value="crop" id="a-crop"> Crop image</label></div>' +
                    '                      </div>' +
                    '                  </div>' +
                    '              </div>' +
                    '              <div class="col-md-3">' +
                    '                  <div class="form-group">' +
                    '                      <label class="control-label">Size</label>' +
                    '                      <div class="controls">' +
                    '                          <div class="radio"><label><input type="radio" name="size" ' + (opts.size === 'small' ? 'checked="checked"' : '') + ' value="small" id="a-small"> Small</label></div>' +
                    '                          <div class="radio"><label><input type="radio" name="size" ' + (opts.size === 'middle' || opts.size === undefined ? 'checked="checked"' : '') + 'value="middle" id="a-middle"> Middle</label></div>' +
                    '                          <div class="radio"><label><input type="radio" name="size" ' + (opts.size === 'large' ? 'checked="checked"' : '') + 'value="large" id="a-large"> Large</label></div>' +
                    '                      </div>' +
                    '                  </div>' +
                    '                  <div class="form-group">' +
                    '                      <label class="control-label">Link</label>' +
                    '                      <div class="controls">' +
                    '                          <div class="checkbox"><label><input type="checkbox" name="link" ' + (opts.link === 'link' ? 'checked="checked"' : '') + ' value="crop" id="a-link"> Make link</label></div>' +
                    '                      </div>' +
                    '                  </div>' +
                    '              </div>' +
                    '          </div>' +
                    '          <div class="row">' +
                    '              <div class="col-md-3">' +
                    '              </div>'  +
                    '              <div class="col-md-3">' +
                    '              </div>' +
                    '          </div>' +
                    '      </div>'  +
                    '  </div>' +
                    '  <div class="modal-footer">' +
                    '      <button class="btn btn-default" id="zmedia-props-cancel">Cancel</button>' +
                    '      <button class="btn btn-primary" type="submit">Save Properties</button>' +
                    '  </div>' +
                    '</form>'
            });

            $("#zmedia-props-form").submit(function () {
                var el,
                    new_opts = {
                        "align": $("#zmedia-props-form input[name='align']:checked").val(),
                        "size": $("#zmedia-props-form input[name='size']:checked").val(),
                        "crop": $("#zmedia-props-form input[name='crop']:checked").val() ? "crop" : "",
                        "link": $("#zmedia-props-form input[name='link']:checked").val() ? "link" : ""
                    };
                el = $(window.tinyMCEzMedia.toHTML(id, new_opts));
                node.className = el.attr("class");
                $(node)
                    .attr("data-zmedia-opts", el.attr("data-zmedia-opts"))
                    .attr("data-zmedia-id", el.attr("data-zmedia-id"));
                z_dialog_close();
                return false;
            });

            $("#zmedia-props-cancel").click(function () {
                z_dialog_close();
                return false;
            });
        },

        _openInsertDialog: function (editor, attributes) {
            window.z_choose_zmedia = function (id) {
                var html;
                if (!id) {
                    return;
                }
                html = window.tinyMCEzMedia.toHTML(id, attributes);
                editor.execCommand("mceInsertContent", false, html, {});
            };
            z_event("zmedia", {
                language: window.zEditLanguage(),
                is_zmedia: 1
            });
        },

        _domIsZMedia: function (el) {
            return this._zMediaIdFromDOM(el) !== null;
        },

        _zMediaIdFromDOM: function (el) {
            return el.getAttribute("data-zmedia-id");
        },

        _zMediaOptsFromDOM: function (el) {
            return JSON.parse(el.getAttribute("data-zmedia-opts"));
        },

        _zMediaClass: function () {
            return "z-tinymce-media";
        },

        _MediaHtmlToMarkers: function (html) {
            var re = new RegExp("<img.*?/>", "g"),
                m,
                img,
                idmatch,
                id,
                optsmatch,
                opts,
                newtag;
            while (true) {
                m = re.exec(html);
                if (m === null) {
                    break;
                }
                img = m[0];
                idmatch = (new RegExp('data-zmedia-id="([0-9]+)', "g")).exec(img);
                if (!idmatch) {
                    return html;
                }
                id = idmatch[1];
                optsmatch = (new RegExp('data-zmedia-opts="(\\{.*?\\})"', "g")).exec(img);
                if (!optsmatch) {
                    return html;
                }
                opts = JSON.parse(optsmatch[1].replace(/&quot;/g, '"'));
                newtag = this._zMediaMarker(id, opts);
                html = html.substr(0, re.lastIndex - img.length) + newtag + html.substr(re.lastIndex);
                re.lastIndex = re.lastIndex - img.length + newtag.length;
            }
            return html;
        },

        _zMediaMarker: function (id, opts) {
            return "<!-- z-media " + id + " " + JSON.stringify(opts) + " -->";
        },

        _zMarkersToMediaHtml: function (html) {
            var re = new RegExp('<!-- z-media (.*?) (.*?)-->', "g"),
                m,
                id,
                opts,
                part;
            while (true) {
                m = re.exec(html);
                if (m === null) {
                    break;
                }
                id = m[1];
                opts = JSON.parse(m[2]);
                part = window.tinyMCEzMedia.toHTML(id, opts);
                html = html.substr(0, re.lastIndex - m[0].length) + part + html.substr(re.lastIndex);
                re.lastIndex = re.lastIndex - m[0].length + part.length;
            }
            return html;
        }
    });

    // Register plugin
    tinymce.PluginManager.add("zmedia", tinymce.plugins.ZotonicMediaPlugin);
}());

/* jslint globals:
tinymce z_event tinyMCE z_dialog_open $ z_dialog_close console
*/
/* jslint options: */
/*jslint browser: true, nomen: true, unparam: true */
