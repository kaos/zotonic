/**
*	jQuery.dialogAdd() and jQuery.dialogRemove()
*	These functions create and remove growl-like notices
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
*	@version    $Id: z.dialog.js 1 2009-01-24 12:24:18Z timbenniks $
**/
(function(jQuery)
{
	$.extend({
		dialogAdd: function(options)
		{	
			if(!$('.dialog').length)
			{
				// declare varaibles
				var options, dialogWrapper, dialogTop, dialogTitle, dialogTLC, dialogTRC, dialogClose, dialogContent, dialogInnerContent, dialogRightContent, dialogBottom, dialogBLC, dialogBRC, dialogSizer, leftPos, topPos;
				
				dialogTitle			= $('<h5></h5>').addClass('dialog-title').text(options.title);
				dialogTLC			= $('<span></span>').addClass('dialog-top-left');
				dialogTRC			= $('<span></span>').addClass('dialog-top-right');
				dialogClose			= $('<span></span>').addClass('dialog-close').click(function(){ $.dialogRemove(dialogWrapper); });
				dialogInnerContent	= $('<div></div>').addClass('dialog-inner-content').html(options.text).resizable({handles: 'se', alsoResize: '.dialog', maxWidth: 700, minWidth: 450, minHeight: 50});
				dialogRightContent	= $('<span></span>').addClass('dialog-content-right');
				dialogBLC			= $('<span></span>').addClass('dialog-bottom-left');
				dialogBRC			= $('<span></span>').addClass('dialog-bottom-right');
				dialogSizer			= $('<span></span>').addClass('dialog-sizer');
				leftPos				= Math.floor((parseInt($(window).width()) / 2) - (parseInt(options.width) / 2));
				topPos				= $(window).scrollTop() + 80;
			
				dialogTop			= $('<div></div>').addClass('dialog-top').append(dialogTitle, dialogTLC, dialogTRC, dialogClose);
				dialogContent		= $('<div></div>').addClass('dialog-content clearfix').append(dialogInnerContent, dialogRightContent);
				dialogBottom		= $('<div></div>').addClass('dialog-bottom').append(dialogBLC, dialogBRC, dialogSizer);
			
				dialogWrapper		= $('<div></div>')
										.addClass('dialog')
										.append(dialogTop, dialogContent, dialogBottom)
										.fadeIn(300)
										.css({left: leftPos, top: topPos, width: options.width})
										.draggable({addClasses: false, handle: dialogTop, opacity: 0.90, zIndex: 2700, iframeFix: true, scroll: true});
								
				$(document).keypress(function(e)
				{
					if($.browser.msie) 	{ var key = e.which }
					else 				{ key = e.keyCode } 
					
					if(key == $.ui.keyCode.ESCAPE)
					{
						dialogClose.click();
					}
				});
				
				$('body').append(dialogWrapper);
				$('.dialog input').eq(0).focus();
			}
		},
		
		dialogRemove: function(obj)
		{
			obj = obj || $('.dialog');
			
			obj.draggable('destroy').resizable('destroy').fadeOut(300, function()
			{
				$(this).remove();
			});
		}
	});
	
	$.widget("ui.dialog", 
	{
		_init: function() 
		{
			var title	= this.options.title;
			var text	= this.options.text;
			var width	= this.options.width;

			this.element.click(function()
			{
				$.dialogAdd(
				{
					title: title,
					text:  text,
					width: width
				})
			})
		}
	});
	
	$.ui.dialog.defaults = {
		title: 'Title',
		text: 'tekst',
		width: '450px'
	}
})(jQuery);