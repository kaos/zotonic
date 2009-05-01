/**
*	zp.formreplace()
*	This function is used to unlink data that has been stored using triplets.
*	e.g. object is connected to subject with a predicate.
*	This function removes the predicate
*		
*   Copyright (c) 2009 Tim Benniks
*
*	Permission is hereby granted, free of charge, to any person obtaining a copy
*	of this software and associated documentation files (the "Software"), to deal
*	in the Software without restriction, including without limitation the rights
*	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*	copies of the Software, and to permit persons to whom the Software is
*	furnished to do so, subject to the following conditions:
*
*	The above copyright notice and this permission notice shall be included in
*	all copies or substantial portions of the Software.
*
*	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
*	THE SOFTWARE.
*	
*	@author 	Tim Benniks <tim@timbenniks.com>
* 	@copyright  2009 timbenniks.com
*	@version    $Id: zp.formreplace.js 12 2009-06-01 22:57:18Z timbenniks $
**/
$.widget("ui.fieldreplace", 
{
	_init: function() 
	{
		if(!this.element.is(':checkbox, :radio')) return false;

		var fieldWrapper, replacedField, self = this, obj = this.element;

		fieldWrapper 	= $('<span></span>').addClass('zp-field-wrapper zp-' + obj.get(0).type);
		replacedField 	= $('<span></span>').addClass(obj.is(':checked') ? 'zp-'+ obj.get(0).type +'-checked' : 'zp-'+ obj.get(0).type +'-unchecked').addClass('zp-'+ obj.get(0).type +'-replacement').bind('click', function(e) { obj.click(); $(this).toggleClass('zp-'+ obj.get(0).type +'-unchecked'); $(this).toggleClass('zp-'+ obj.get(0).type +'-checked'); });

		obj.wrap(fieldWrapper).after(replacedField).addClass('zp-'+ obj.get(0).type +'-replaced').bind('change', function(e) { console.log(e); replacedField.toggleClass('zp-'+ obj.get(0).type +'-unchecked'); replacedField.toggleClass('zp-'+ obj.get(0).type +'-checked'); });
	}
});