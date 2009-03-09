(function(jQuery)
{
	jQuery.fn.imageViewer = function()
	{
		return this.each(function() 
		{
			var obj 			= jQuery(this);
			var imageWrapper 	= jQuery('<div></div>').addClass('image-wrapper').css({width: obj.width(), height: obj.height()});
			var imageMagnifier 	= jQuery('<div></div>').addClass('image-magnifier').css({top: obj.offset().top, left: obj.offset().left});
			var bigImg			= jQuery('<img alt="'+obj.attr('alt')+'" />').hide();
					
			obj.wrap(imageWrapper).after(imageMagnifier).parent().hover(function()
			{
				imageMagnifier.slideDown(150);
			},
			function()
			{
				imageMagnifier.slideUp(150);
			});
			
			imageMagnifier.after(bigImg).click(function()
			{
				obj.loadImage();
			});
		});
	}
	
	jQuery.fn.loadImage = function()
	{
		var obj 			= $(this);
		var imageOrigSrc 	= obj.attr('src').split('.');
		var imageTempSrc 	= imageOrigSrc[0].split('/');
		var imageExt		= imageOrigSrc[imageOrigSrc.length - 1];
		var imageSrc 		= '/media/inline/' + imageTempSrc[imageTempSrc.length - 1] + '.' + imageExt;
		var bigImg 			= obj.siblings('img');
		
		var loader			= $('<span></span>').css({background: '#fff url(/lib/images/spinner.gif) 50% 50% no-repeat', opacity: .5, width: obj.width(), height: obj.height(), position: "absolute", top: obj.offset().top, left: obj.offset().left})
		
		if(!$('.loaded-bigImage', obj.parent()).length)
		{
			$(document.body).append(loader);
		}
		
		jQuery(bigImg)
			.load(function()
			{
				$(this)
					.hide()
					.addClass('loaded-bigImage')
					.unbind('load');

				if(!$('.loaded-bigImage', obj.parent()).length)
				{
					obj.after($(this));
				}
				
				loader.remove();				
				obj.setWidthHeight();
				obj.showBig();
			})
			.attr({src: imageSrc});
	}
	
	jQuery.fn.setWidthHeight = function()
	{
		var obj = $(this);

		jQuery('.loaded-bigImage', obj.parent())
			.attr({
				width: jQuery('.loaded-bigImage', obj.parent()).width(),
				height: jQuery('.loaded-bigImage', obj.parent()).height() 
			});
	}
	
	jQuery.fn.showBig = function()
	{
		var obj 			= $(this);
		var imgObj			= jQuery('.loaded-bigImage', obj.parent());
		var imgWrapper		= obj.parent();
		var zoomImgWidth 	= imgObj.attr('width');
		var zoomImgHeight 	= imgObj.attr('height');
		var fullWidth 		= zoomImgWidth;
		var fullHeight 		= zoomImgHeight;

		if(zoomImgWidth > $(window).width())
		{
			fullWidth = $(window).width() - 40;
			fullHeight = zoomImgHeight * (fullWidth / zoomImgWidth);	
		}

		leftPos = ($(window).width() / 2) - (fullWidth / 2);
		topPos 	= ($(window).height() / 2) - (fullHeight / 2);

		$(window).resize(function()
		{
			$('.image-magnifier').each(function()
			{
				$(this).css({top: $(this).parent().offset().top, left: $(this).parent().offset().left});
			});
		});
		
		if(!$('.popup-overlay').length)
		{
			$('<span</span>')
				.addClass('popup-overlay')
				.appendTo(document.body)
				.css({opacity: .8, height: $(document).height(), zIndex: 8000})
				.click(function()
				{
					destroy()
				});
		}
		
		$('.popup-overlay').show();
				
		imgObj
			.css({position: "absolute", zIndex: 9999, width: fullWidth, height: fullHeight, left: leftPos, top: topPos})
			.hide()
			.fadeIn(200)
			.click(function()
			{
				destroy()
			});
	}
	
	function destroy()
	{
		jQuery('.popup-overlay').fadeOut(100);
		jQuery('.loaded-bigImage').fadeOut(200);
	}
})(jQuery);

$(document).ready(function()
{
	$("img[rel='image-viewer']").imageViewer();
});