/**
 * Zophrenic javascript library
 *
 * (c) copyright 2009 Marc Worrell
 *
 * Based on nitrogen.js which is copyright 2008-2009 Rusty Klophaus
 */

var	zp_comet_is_running      = false;
var zp_is_in_postback        = false;
var zp_postbacks             = [];
var zp_default_form_postback = false;

// We also need to set the domain of the session cookie to 'test'
// Then we can try to make an xhr request to 0.test, 1.test etc.
//document.domain = 'test';
//var zp_xhr_domain = 'test';

/*** Growl messages ***/

function zp_growl_add( message, stay ) 
{
    if (stay)
    {
        jQuery.noticeAdd({
        	text: message,
        	stay: true
        });
    }
    else
    {
		jQuery.noticeAdd({
			text: message,
			stay: false
		});
    }
}

function zp_growl_close()
{
	jQuery. jQuery.noticeRemove($('.notice-item-wrapper'), 400);
}

/*** Postback loop ***/

function zp_postback_loop() 
{
	if (!zp_is_in_postback && zp_postbacks.length != 0) 
	{
    	// For now, allow only a single postback at a time.
    	zp_is_in_postback++;

    	var o = zp_postbacks.shift();
    	zp_do_postback(o.triggerID, o.postback, o.extraParams);

    	setTimeout("zp_postback_loop()", 1);
	}
	else
	{
    	setTimeout("zp_postback_loop()", 20);
	}
}

function zp_queue_postback(triggerID, postback, extraParams) 
{
	var o = new Object();
	o.triggerID   = triggerID;
	o.postback    = postback;
	o.extraParams = extraParams;
	zp_postbacks.push(o);
}

function zp_do_postback(triggerID, postback, extraParams) 
{
	// Get params...
	var params = 
		"postback=" + urlencode(postback) + 
		"&zp_trigger_id=" + urlencode(triggerID) +
		"&zp_pageid=" + urlencode(zp_pageid) + 
		"&" + extraParams;
	
	zp_ajax(params);
}

function zp_ajax(params) 
{
	zp_start_spinner();	

	$.ajax({ 
		url: '/postback',
		type: 'post',
		data: params,
		dataType: 'text',
		success: 
		    function(data, textStatus) 
		    {
    			zp_is_in_postback--;
    			zp_stop_spinner();
    			try {
    				//alert("SUCCESS: " + transport.responseText);
    				eval(data);
                    zp_init_postback_forms();
    			} catch (E) {
    				alert("Error evaluating ajax return value: " + data);
    				alert(E);
    			}
    		},
		error:
		    function(xmlHttpRequest, textStatus, errorThrown) 
		    {
			    zp_is_in_postback--;
    			zp_stop_spinner();
			    alert("FAIL: " + textStatus);
		    }
	});			
}


/*** Comet long poll ***/

function zp_comet_start()
{
	if (!zp_comet_is_running)
	{
		setTimeout("zp_comet();", 1000);
		zp_comet_is_running = true;
	}
}

function zp_comet() 
{
	$.ajax({ 
		url: '/comet',
		type:'post',
		data: "zp_pageid=" + urlencode(zp_pageid),
		dataType: 'text',
		success:
		    function(data, textStatus) 
		    {
    			try {
    				//alert("SUCCESS: " + data);
    				eval(data);
                    zp_init_postback_forms();
    			} catch (E) {
    				alert("Error evaluating Comet return value: " + data);
    				alert(E);
    			}
    			setTimeout("zp_comet();", 10);
    		},
		error: 
		    function(xmlHttpRequest, textStatus, errorThrown) 
		    {
			    setTimeout("zp_comet();", 1000);
		    }
	});
}

/*** Utility functions ***/

function zp_is_enter_key(event) 
{
	return (event && event.keyCode == 13);
}

/*** Spinner, showed when waiting for a postback ***/

function zp_start_spinner()
{
    if (zp_is_in_postback > 0)
    {
    	var spinner = $('#spinner');
    	if (spinner) spinner.animate({opacity: 'show'}, 200);
    }
}

function zp_stop_spinner() {
    if (zp_is_in_postback == 0)
    {
    	var spinner = $('#spinner');
    	if (spinner) spinner.animate({opacity: 'hide'}, 50);
    }
}

/*** Drag & drop interface to the postback ***/

function zp_draggable(dragObj, dragOptions, dragTag) 
{
	$(dragObj).draggable(dragOptions).data("zp_drag_tag", dragTag);	
}

function zp_droppable(dropObj, dropOptions, dropPostbackInfo) 
{
	dropOptions.drop = function(ev, ui) {
		var dragTag = $(ui.draggable[0]).data("zp_drag_tag");
		zp_queue_postback(this.id, dropPostbackInfo, "drag_item=" + urlencode(dragTag));
	}
	$(dropObj).droppable(dropOptions);
}

/*** Sorter and sortables interface to the postback ***/

function zp_sortable(sortableItem, sortTag) 
{
	$(sortableItem).data("zp_sort_tag", sortTag);
}

function zp_sorter(sortBlock, sortOptions, sortPostbackInfo) 
{
	sortOptions.update = function() {
		var sortItems = "";

		for (var i=0; i<this.childNodes.length; i++) 
		{
			var sortTag = $(this.childNodes[i]).data("zp_sort_tag");
			if (sortTag)
			{
    			if (sortItems != "") 
    			{
    			    sortItems += ",";
			    }
			    sortItems += sortTag
			}
		}
		zp_queue_postback(this.id, sortPostbackInfo, "sort_items=" + urlencode(sortItems));
	};
	$(sortBlock).sortable(sortOptions);
}

/*** Form element validations ***/

// Grab all "postback" forms, let them be handled by Ajax postback calls.
// This function can be run multiple times.
function zp_init_postback_forms()
{
	$("form[action='postback']").submit(
	        function (event)
	        {
	            var serialized  = $(this).formSerialize();
                var postback    = $(this).data("zp_submit_postback");
                var action      = $(this).data("zp_submit_action");
                var form_id     = $(this).attr('id');
                var validations = $(this).formValidationPostback();
                
                if (!postback) 
                {
                    postback = zp_default_form_postback;
                }
                zp_queue_postback(form_id, postback, serialized+'&'+validations); 
                if (action)
                {
                    setTimeout(action, 10);
                }
	            event.stopPropagation();
	            return false;
	        }
	    ).attr('action', 'postback:installed');
}


// Collect all postback validations from the form elements
$.fn.formValidationPostback = function() {
    var a = [];
    if (this.length == 0) return a;

    var form = this[0];
    var els  = form.elements;
    if (!els) return a;

    for(var i=0, max=els.length; i < max; i++) 
    {
        var el = els[i];
        var n  = el.name;
        if (n)
        {
            var v = $(el).data("zp_postback_validation");
            if (v)
            {
                a.push({name: "zp_v", value: n+":"+v})
            }
        }
    }
    return $.param(a);
}

// Initialize a validator for the element #id
function zp_init_validator(id, args)
{
    var elt = $('#'+id);
    if (elt)
    {
        if (!$(elt).data("zp_live_validation"))
        {
            $(elt).data("zp_live_validation", new LiveValidation(id, args));
        }
    }
    else
    {
        alert('Validator error: no element with id #'+id);
    }
}

// Add a validator to the input field
function zp_add_validator(id, type, args)
{
    var v = $('#'+id).data("zp_live_validation");
    if (v)
    {
        switch (type)
        {
        case 'email':       v.add(Validate.Email, args); break;
        case 'presence':    v.add(Validate.Presence, args); break;
        case 'confirmation':v.add(Validate.Confirmation, args); break;
        case 'acceptance':  v.add(Validate.Acceptance, args); break;
        case 'length':      v.add(Validate.Length, args); break;
        case 'format':      v.add(Validate.Format, args); break;
        case 'numericality':v.add(Validate.Numericality, args); break;
        default:
            alert("unknown validation: "+type);
        }
    }
}

function zp_set_validator_postback(id, postback)
{
    if (postback)
    {
        var pb = $('#'+id).data("zp_postback_validation");
        
        if (pb)
        {
            alert("Element #"+id+" had already a validation postback, add all validations as one batch.");
        }
        $('#'+id).data("zp_postback_validation", postback);
    }
}


function zp_validation_error(id, error)
{
    if (error == 'invalid')
    {
        // Generic error - handle it ourselves
        error = "please correct";
    }
    $('#'+id).addClass("form-field-error");
}

// URL encode function that is more RFC compatible.  Also encodes +, *, / and @.
function urlencode(s)
{
    s = escape(s);
    s = s.replace(/\+/g, '%2B');
    s = s.replace(/\*/g, '%2A');
    s = s.replace(/\//g, '%2F');
    s = s.replace(/@/g, '%40');
    return s;
}

// From: http://malsup.com/jquery/form/jquery.form.js

/**
 * formToArray() gathers form element data into an array of objects that can
 * be passed to any of the following ajax functions: $.get, $.post, or load.
 * Each object in the array has both a 'name' and 'value' property.  An example of
 * an array for a simple login form might be:
 *
 * [ { name: 'username', value: 'jresig' }, { name: 'password', value: 'secret' } ]
 *
 * It is this array that is passed to pre-submit callback functions provided to the
 * ajaxSubmit() and ajaxForm() methods.
 */
$.fn.formToArray = function(semantic) {
    var a = [];
    if (this.length == 0) return a;

    var form = this[0];
    var els = semantic ? form.getElementsByTagName('*') : form.elements;
    if (!els) return a;
    for(var i=0, max=els.length; i < max; i++) {
        var el = els[i];
        var n = el.name;
        if (!n) continue;

        var v = $.fieldValue(el, true);
        if (v && v.constructor == Array) {
            for(var j=0, jmax=v.length; j < jmax; j++)
                a.push({name: n, value: v[j]});
        }
        else if (v !== null && typeof v != 'undefined')
            a.push({name: n, value: v});
    }

    return a;
};

/**
 * Serializes form data into a 'submittable' string. This method will return a string
 * in the format: name1=value1&amp;name2=value2
 */
$.fn.formSerialize = function(semantic) {
    //hand off to jQuery.param for proper encoding
    return $.param(this.formToArray(semantic));
};

/**
 * Serializes all field elements in the jQuery object into a query string.
 * This method will return a string in the format: name1=value1&amp;name2=value2
 */
$.fn.fieldSerialize = function(successful) {
    var a = [];
    this.each(function() {
        var n = this.name;
        if (!n) return;
        var v = $.fieldValue(this, successful);
        if (v && v.constructor == Array) {
            for (var i=0,max=v.length; i < max; i++)
                a.push({name: n, value: v[i]});
        }
        else if (v !== null && typeof v != 'undefined')
            a.push({name: this.name, value: v});
    });
    //hand off to jQuery.param for proper encoding
    return $.param(a);
};

/**
 * Returns the value(s) of the element in the matched set.  For example, consider the following form:
 *
 *  <form><fieldset>
 *      <input name="A" type="text" />
 *      <input name="A" type="text" />
 *      <input name="B" type="checkbox" value="B1" />
 *      <input name="B" type="checkbox" value="B2"/>
 *      <input name="C" type="radio" value="C1" />
 *      <input name="C" type="radio" value="C2" />
 *  </fieldset></form>
 *
 *  var v = $(':text').fieldValue();
 *  // if no values are entered into the text inputs
 *  v == ['','']
 *  // if values entered into the text inputs are 'foo' and 'bar'
 *  v == ['foo','bar']
 *
 *  var v = $(':checkbox').fieldValue();
 *  // if neither checkbox is checked
 *  v === undefined
 *  // if both checkboxes are checked
 *  v == ['B1', 'B2']
 *
 *  var v = $(':radio').fieldValue();
 *  // if neither radio is checked
 *  v === undefined
 *  // if first radio is checked
 *  v == ['C1']
 *
 * The successful argument controls whether or not the field element must be 'successful'
 * (per http://www.w3.org/TR/html4/interact/forms.html#successful-controls).
 * The default value of the successful argument is true.  If this value is false the value(s)
 * for each element is returned.
 *
 * Note: This method *always* returns an array.  If no valid value can be determined the
 *       array will be empty, otherwise it will contain one or more values.
 */
$.fn.fieldValue = function(successful) {
    for (var val=[], i=0, max=this.length; i < max; i++) {
        var el = this[i];
        var v = $.fieldValue(el, successful);
        if (v === null || typeof v == 'undefined' || (v.constructor == Array && !v.length))
            continue;
        v.constructor == Array ? $.merge(val, v) : val.push(v);
    }
    return val;
};

/**
 * Returns the value of the field element.
 */
$.fieldValue = function(el, successful) {
    var n = el.name, t = el.type, tag = el.tagName.toLowerCase();
    if (typeof successful == 'undefined') successful = true;

    if (successful && (!n || el.disabled || t == 'reset' || t == 'button' ||
        (t == 'checkbox' || t == 'radio') && !el.checked ||
        (t == 'submit' || t == 'image') && el.form && el.form.clk != el ||
        tag == 'select' && el.selectedIndex == -1))
            return null;

    if (tag == 'select') {
        var index = el.selectedIndex;
        if (index < 0) return null;
        var a = [], ops = el.options;
        var one = (t == 'select-one');
        var max = (one ? index+1 : ops.length);
        for(var i=(one ? index : 0); i < max; i++) {
            var op = ops[i];
            if (op.selected) {
				var v = op.value;
				if (!v) // extra pain for IE...
                	v = (op.attributes && op.attributes['value'] && !(op.attributes['value'].specified)) ? op.text : op.value;
                if (one) return v;
                a.push(v);
            }
        }
        return a;
    }
    return el.value;
};

