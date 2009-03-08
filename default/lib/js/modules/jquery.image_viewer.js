(function(jQuery)
{
	jQuery.fn.imageViewer = function()
	{
		return this.each(function() 
		{
			var obj 			= jQuery(this);
			var imageWrapper 	= jQuery('<div></div>').addClass('image-wrapper').css({width: obj.width(), height: obj.height()});
			var imageMagnifier 	= jQuery('<div></div>').addClass('image-magnifier').css({top: obj.offset().top, left: obj.offset().left});
		
			obj.wrap(imageWrapper).after(imageMagnifier).parent().hover(function()
			{
				imageMagnifier.slideDown(150);
			},
			function()
			{
				imageMagnifier.slideUp(150);
			});
			
			imageMagnifier.click(function()
			{
				jQuery.fn.imageViewer.loadImage(obj);
			});
		});
	}
	
	jQuery.fn.imageViewer.loadImage = function(obj)
	{
		var imageOrigSrc 	= obj.attr('src').split('.');
		var imageTempSrc 	= imageOrigSrc[0].split('/');
		var imageExt		= imageOrigSrc[imageOrigSrc.length - 1];
		var imageSrc 		= '/media/inline/' + imageTempSrc[imageTempSrc.length - 1] + '.' + imageExt;
		var bigImg 			= new Image();
		
		if(!$('.loaded-bigImage', obj.parent()).length)
		{
			obj.parent().addClass('loading-bigimage');
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
					obj.after($(this)).parent().removeClass('loading-bigimage');
				}
				
				jQuery.fn.imageViewer.setWidthHeight(obj);
				jQuery.fn.imageViewer.showBig(obj);
			})
			.attr({src: imageSrc, alt: obj.attr('alt')});
	}
	
	jQuery.fn.imageViewer.setWidthHeight = function(obj)
	{
		jQuery('.loaded-bigImage', obj.parent())
			.attr({
				width: jQuery('.loaded-bigImage', obj.parent()).width(),
				height: jQuery('.loaded-bigImage', obj.parent()).height() 
			});
	}
	
	jQuery.fn.imageViewer.showBig = function(obj)
	{
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
		topPos 	= $(window).scrollTop() + 10;

		$(window).resize(function()
		{
			if($(window).width() < zoomImgWidth)
			{
				imgObj.animate({ 
					width: ($(window).width() - 40),
					height: zoomImgHeight * (($(window).width() - 40) / zoomImgWidth),
					left: ($(window).width() / 2) - (($(window).width() - 40) / 2)
				}, 200)
			} 
			else
			{
				imgObj.animate({ 
					width: zoomImgWidth,
					height: zoomImgHeight,
					left: ($(window).width() / 2) - (zoomImgWidth / 2)
				}, 200)
			}			
		});
		
		$('<span</span>')
			.addClass('popup-overlay')
			.css({opacity: .8, height: $(document).height(), backgroundColor: '#fff', zIndex: 8000})
			.click(function()
			{
				jQuery.fn.imageViewer.destroy(obj)
			})
			.appendTo(document.body);
		
		imgObj
			.css({left: imgWrapper.offset().left, top: imgWrapper.offset().top, position: "absolute", zIndex: 9999})
			.animate({width: fullWidth, height: fullHeight, left: leftPos, top: topPos}, 300)
			.click(function()
			{
				jQuery.fn.imageViewer.destroy(obj)
			});
	}
	
	jQuery.fn.imageViewer.destroy = function(obj)
	{
		$('.popup-overlay').fadeOut(300, function()
		{ 
			$(this).remove() 
		});
		
		jQuery('.loaded-bigImage', obj.parent())
			.animate({
				width: obj.width(),
				height: obj.height(), 
				left: obj.parent().offset().left,
				top: obj.parent().offset().top}, 200, function()
				{ 
					$(this).remove();
				});
	}
})(jQuery);

$(document).ready(function()
{
	$("img[rel='image-viewer']").imageViewer();
});