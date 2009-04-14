/**
 * Zophrenic unlinker
 *
 * (c) copyright 2009 Tim Benniks <tim@timbenniks.com>
 */
$.widget("ui.unlink", 
{
	_init: function() 
	{
		var self			= this;
		var obj 			= this.element;
		var unlinkLeft 		= $('img', obj).position().left - 2;
		var unlinkTop  		= obj.position().top - 7;
		var orig_edge_color = obj.css('color');
		
		obj.css({cursor: 'pointer'}).click(function()
		{
			var confirm	= $('<div></div>').addClass('unlink-confirm').css({position: 'absolute', top: unlinkTop, left: unlinkLeft}).html('<span>Are you sure?</span>').append('<button class="unlink-yes">yes</button><button class="unlink-no">no</button>').fadeIn(200).appendTo(document.body);

			obj.css({color: '#f00'});
		
			$('.unlink-no', confirm).click(function()
			{
				obj.css({color: orig_edge_color});
				
				$(this).parent().fadeOut(200, function()
				{
					$(this).remove();
				});
			});
			
			$('.unlink-yes', confirm).click(function()
			{
				confirm.animate({opacity: 'hide', height: 0}, 200, function()
				{
					$(this).remove();
				})
				
				obj.parent().animate({opacity: 'hide'}, 200, function()
				{
					$(this).remove();
				});
				
				$.post(self.options.controller, 
				{
					 object_id: self.options.object_id, 
					 edge_id: self.options.edge_id, 
					 subject_id: self.options.subject_id
				}, 
				function(data)
				{
					$.misc.log('edge removed');
					self.kill();
				});
			});
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