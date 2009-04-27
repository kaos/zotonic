/**
*	zp.autocomplete()
*	This function is used to autocomplete anything in the zophrenic system
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
*	@version    $Id: zp.autocomplete.js 12 2009-04-15 12:24:18Z timbenniks $
**/
$.widget("ui.autocomplete", 
{
	_init: function() 
	{
		var self			= this;
		var obj 			= this.element;	
		var inputWidth 		= obj.width() + parseInt(obj.css('padding-left')) + parseInt(obj.css('padding-right'));
		var ulTop 			= obj.position().top + obj.height() + parseInt(obj.css('padding-top')) + parseInt(obj.css('padding-bottom'));
		var suggestions 	= $('<ul></ul>').addClass('suggestions-list').css({width: inputWidth, position: 'absolute', top: ulTop, left: obj.position().left}).hide().appendTo(document.body);
		var input_updater 	= false;
	
		obj.bind('keyup', function()
		{
			if(input_updater)
			{
				clearTimeout(input_updater);
				input_updater = false;
			}
			
			input_updater = setTimeout(function()
			{
				if(obj.val().length >= self.options.afterChars)
				{
					obj.addClass('loading');
				
					$.post(self.options.controller, 
					{
						input: 			obj.val(),
						obj:   			obj,
						suggestions: 	suggestions
					});
				}
			}, 400);
		});
	},
	
	kill: function() 
	{
		this.destroy();
	}
});

$.ui.autocomplete.defaults = {
	controller: '/postback',
	afterChars: 3
}

var zp_autoCompleteAfterPostback = function(obj, suggestions, data)
{
	if(data)
	{
		obj.removeClass('loading');
		suggestions.animate({height: 'show', opacity: 'show'}, 200).html(data);

		$('.suggestions-result').hover(function() 
		{
			$(this).addClass('hovering')
		},
		function()
		{
			$(this).removeClass('hovering')
		})
		.click(function()
		{
			obj.val($(this).html());
			suggestions.animate({height: 'hide', opacity: 'hide'}, 200);
			$.ui.autocomplete.kill();
		});
	}
}