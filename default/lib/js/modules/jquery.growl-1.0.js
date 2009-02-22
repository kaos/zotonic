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
	jQuery.extend({			
		growlAdd: function(options)
		{	
			// declare variables
			var options, growlWrap, growlBox, closeButton;
		
			options 	= jQuery.extend({}, jQuery.growlDefaults, options);
			growlWrap 	= (!jQuery('.growl-wrap').length) ? jQuery('<div></div>').addClass('growl-wrap').appendTo('body') : jQuery('.growl-wrap');
			growlBox 	= jQuery('<div></div>').hide().addClass('growl-item ' + options.type).appendTo(growlWrap).html('<p>'+options.text+'</p>').animate(options.inEffect, options.inEffectDuration);
			closeButton = jQuery('<div></div>').addClass('grow-item-close').prependTo(growlBox).html('x').click(function() { jQuery.growlRemove(growlBox, options.outEffectDuration) });
		
			if(options.stay == 'nope')
			{
				setTimeout(function()
				{
					jQuery.growlRemove(growlBox, options.outEffectDuration);
				},
				options.stayTime);
			}
		},
		
		growlRemove: function(obj, duration)
		{
			obj.animate({opacity: '0'}, duration, function()
			{
				obj.animate({height: '0px'}, duration, function()
				{
					obj.remove();
				});
			}); 
		},
		
		growlDefaults: {
			inEffect: 			{opacity: 'show'},	// in effect
			inEffectDuration: 	600,				// in effect duration in miliseconds
			OutEffectDuration: 	600,				// out effect duration in miliseconds
			stayTime: 			3000,				// time in miliseconds before the item has to disappear
			text: 				'',					// content of the item
			stay: 				'nope',				// should the growl item stay or not?
			type: 				'notice' 			// could also be error, succes
		}
	});
})(jQuery);
			
