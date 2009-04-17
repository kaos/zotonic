/**
*	zp.tooltip()
*	This function is used to make a nice tooltip.
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
*	@version    $Id: zp.tooltip.js 12 2009-04-15 12:24:18Z timbenniks $
**/

;(function(jQuery) 
{
	$.fn.tooltip = function(options)
	{
		var defaults = {
			offsetY: 			0,
			offsetX: 			0,
			inevent: 			'mouseover',
			outevent: 			'mouseout',
			width: 				'auto',
			maxwidth: 			'330px',
		}
  		
		var options = $.extend(defaults, options);
  			
  		return this.each(function() 
		{
			obj = $(this);
		
			if(this.title == '')
			{
				obj.unbind(options.inevent, options.outevent);
				return false;
			}

			obj.bind(options.inevent, function(e) 
			{
				this.tip 		= this.title;
				var tip_content = this.title;
				this.title 		= "";
				
				tip = $('<div></div>')
						.addClass('tooltip')
						.html(tip_content)
						.css({top: e.pageY + options.offsetY, left: e.pageX + options.offsetX, width: options.width, maxWidth: options.maxwidth });
				
				$(document.body).append(tip);

				var left = $(this).position().left;
				var top  = $(this).position().top;
								
				tip.css({top: top - 30});
				
				if(left + tip.width() > $(window).width())
				{
					tip.css({left: Math.ceil(left) - Math.ceil((obj.width() / 2)) - Math.ceil((tip.width() / 2 ))});
				}
				else
				{
					tip.css({left: Math.ceil(left) + Math.ceil((obj.width() / 2)) - Math.ceil((tip.width() / 2 ))});
				}
				
				tip.stop().animate({opacity: 'show'}, 200);
			});
			
			obj.bind(options.outevent, function(e) 
			{
				tip.stop().animate({opacity: 'show'}, 200, function()
				{
					$(this).remove();
				});
				
				$.fn.tooltip.destroy();
				this.title = this.tip;
			});
			
			$.fn.tooltip.destroy = function()
			{
				obj.unbind(options.inevent, options.outevent);
				$(document).unbind('mousemove');
			}
		});
	}
})(jQuery);