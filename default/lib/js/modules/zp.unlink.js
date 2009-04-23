/**
*	zp.unlink()
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
*	@version    $Id: zp.unlink.js 12 2009-04-15 12:24:18Z timbenniks $
**/
$.widget("ui.unlink", 
{
	_init: function() 
	{
		var self			= this;
		var obj 			= this.element;
		
		$('.unlink-cross', obj).hover(function()
		{
			$(this).closest('.rsc-edge').addClass('rsc-edge-hover');
		},
		function()
		{
			$(this).closest('.rsc-edge').removeClass('rsc-edge-hover');
		});
		
		$('.unlink-cross', obj).click(function()
		{
			$.misc.log('edge removed');
			obj.parent().fadeOut(200, function()
			{
				$(this).remove();
			});

			//$.post(self.options.controller, 
			//{
			//	 object_id: self.options.object_id, 
			//	 edge_id: self.options.edge_id, 
			//	 subject_id: self.options.subject_id
			//}, 
			//function(data)
			//{
			//	$.misc.log('edge removed');
			//	self.kill();
			//});
		});
	},
	
	kill: function() 
	{
		this.destroy();
	}
});

$.ui.unlink.defaults = {
	controller: 'test.php'
}