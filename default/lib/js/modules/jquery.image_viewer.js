$.widget("ui.imageviewer", 
{
	_init: function() 
	{ 
		this.init();
		var imageWrapper, imageMagnifier, bigImg;
	},
	
	init: function()
	{
		var ui			= this;
		imageWrapper 	= jQuery('<div></div>').addClass('image-wrapper clearfix').css({width: this.element.width(), height: this.element.height()});
		imageMagnifier 	= jQuery('<div></div>').addClass('image-magnifier').css({top: this.element.offset().top, left: this.element.offset().left});
		bigImg			= jQuery('<img alt="'+ui.element.attr('alt')+'" />').hide();
		
		this.element.wrap(imageWrapper).after(imageMagnifier).parent().hover(function()
		{
			$('.image-magnifier', $(this).parent()).slideDown(150);
		},
		function()
		{
			$('.image-magnifier', $(this).parent()).slideUp(150);
		});
		
		imageMagnifier.after(bigImg).click(function()
		{
			ui.loadImage()
		});
	},
	
	loadImage: function()
	{
		var ui 				= this;
		var imageOrigSrc 	= ui.element.attr('src').split('.');
		var imageTempSrc 	= imageOrigSrc[0].split('/');
		var imageExt		= imageOrigSrc[imageOrigSrc.length - 1];
		var imageSrc 		= '/media/inline/' + imageTempSrc[imageTempSrc.length - 1] + '.' + imageExt;
		var bigImg 			= ui.element.siblings('img');
		
		var loader			= $('<span></span>').css({background: '#fff url(/lib/images/spinner.gif) 50% 50% no-repeat', opacity: .5, width: ui.element.width(), height: ui.element.height(), position: "absolute", top: ui.element.offset().top, left: ui.element.offset().left})
		
		if(!$('.loaded-bigImage', ui.element.parent()).length)
		{
			$(document.body).append(loader);
		}
		
		$(bigImg)
			.load(function()
			{
				$(this)
					.hide()
					.addClass('loaded-bigImage')
					.unbind('load');

				if(!$('.loaded-bigImage', ui.element.parent()).length)
				{
					ui.element.after($(this));
				}
				
				loader.remove();				
				ui.setWidthHeight();
				ui.showBig();
			})
			.attr({src: imageSrc});
	},
	
	setWidthHeight: function()
	{
		$('.loaded-bigImage', this.element.parent())
			.attr({
				width: jQuery('.loaded-bigImage', this.element.parent()).width(),
				height: jQuery('.loaded-bigImage', this.element.parent()).height() 
			});
	},
	
	showBig: function()
	{
		var ui 				= this;
		var imgObj			= jQuery('.loaded-bigImage', ui.element.parent());
		var imgWrapper		= ui.element.parent();
		var zoomImgWidth 	= imgObj.attr('width');
		var zoomImgHeight 	= imgObj.attr('height');
		var fullWidth 		= zoomImgWidth;
		var fullHeight 		= zoomImgHeight;

		if(zoomImgWidth > $(window).width())
		{
			fullWidth = $(window).width() - 40;
			fullHeight = zoomImgHeight * (fullWidth / zoomImgWidth);	
		}
		
		if(zoomImgHeight > $(window).height())
		{
			fullHeight = $(window).height() - 40;
			fullWidth = zoomImgWidth * (fullHeight / zoomImgHeight);	
		}

		leftPos = ($(window).width() / 2) - (fullWidth / 2);
		topPos 	= $(window).scrollTop() + ($(window).height() / 2) - (fullHeight / 2);

		$(window).resize(function()
		{
			$('.image-magnifier', ui.element.parent()).each(function()
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
					ui.kill()
				});
		}
		
		$('.popup-overlay').show();
				
		imgObj
			.css({position: "absolute", zIndex: 9999, width: (fullWidth - 80), height: (fullHeight - 80), left: (leftPos + 40), top: (topPos + 40)})
			.animate({width: fullWidth, height: fullHeight, left: leftPos, top: topPos}, 200)
			.click(function()
			{
				ui.kill()
			});
	},
	
	kill: function() 
	{
		jQuery('.popup-overlay').fadeOut(100);
		jQuery('.loaded-bigImage').fadeOut(100);
	   	this.destroy();
	}
});