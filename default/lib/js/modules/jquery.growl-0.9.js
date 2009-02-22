/**
*	jQuery.growlAdd() and jQuery.growlRemove()
*	These functions create and remove growl-like notices
*		
*	@author 	Tim Benniks <tim@timbenniks.com>
* 	@copyright  2009 timbenniks.com
*	@version    $Id: jquery.growl.js 1 2009-01-24 12:24:18Z timbenniks $
**/
(function(jQuery)
{
	var defaults = {
		inEffect: 			{opacity: 'show'},	// in effect
		inEffectDuration: 	600,				// in effect duration in miliseconds
		outEffect: 			{opacity: 'hide'},	// out effect
		outEffectDuration: 	400, 				// out effect duration in miliseconds
		stayTime: 			3000,				// time in miliseconds before the item has to disappear
		text: 				'',					// content of the item
		stay: 				'nope',				// should the growl item stay or not?
		type: 				'notice' 			// could also be error, succes
	}
	
	jQuery.extend({			
		growlAdd: function(options)
		{
			var options = jQuery.extend({}, defaults, options);

			var growlBox 	= jQuery('<div></div>')
								.hide()
								.addClass('growl-item ' + options.type)
								.css({position: 'fixed'})
								.animate(options.inEffect, options.inEffectDuration)
								.html('<p>'+options.text+'</p>');
								
			var	closeButton = jQuery('<div></div>')
								.addClass('grow-item-close')
								.html('x')
								.prependTo(growlBox)
								.click(function()
								{
									jQuery.growlRemove(growlBox, options.outEffect, options.outEffectDuration);
								});
			
			if(jQuery('.growl-item').length)
			{						
				var lastBoxBottomPoint = jQuery('.growl-item:last').position().top + jQuery('.growl-item:last').height();
				var marginBetweenGrowls = 12;
				
				growlBox.css({
					top: lastBoxBottomPoint + marginBetweenGrowls
				});	
			}
			
			$('body').append(growlBox);
			
			if(options.stay == 'nope')
			{
				function timeout_init()
				{
					jQuery.growlRemove(growlBox, options.outEffect, options.outEffectDuration);
				}
				
				setTimeout(timeout_init, options.stayTime);
			}
		},
		growlRemove: function(obj, effect, duration)
		{
			obj.animate(effect, duration, function()
			{
				$(this).remove();
			}); 
		}
	});
})(jQuery);