/**
 * Zophrenic autocompleter
 *
 * (c) copyright 2009 Tim Benniks <tim@timbenniks.com>
 */
$.widget("ui.autocomplete", 
{
	_init: function() 
	{
		var self		= this;
		var obj 		= this.element;	
		var inputWidth 	= obj.width() + parseInt(obj.css('padding-left')) + parseInt(obj.css('padding-right'));
		var ulTop 		= obj.position().top + obj.height() + parseInt(obj.css('padding-top')) + parseInt(obj.css('padding-bottom'));
		var suggestions = $('<ul></ul>').addClass('suggestions-list').css({width: inputWidth, position: 'absolute', top: ulTop, left: obj.position().left}).hide().appendTo(document.body);

		obj.bind('keyup', function()
		{
			if(this.value.length >= self.options.afterChars)
			{
				obj.addClass('loading');
				
				$.get(self.options.controller, 
				{
					input: this.value
				}, 
				function(data)
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
							self.kill();
						});
					}
				});
			}
		});
	},
	
	kill: function() 
	{
		this.destroy();
	}
});

$.ui.autocomplete.defaults = {
	controller: 'test.php',
	afterChars: 3
}