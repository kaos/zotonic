/**
*	zp.imageviewer()
*	This function is used to view the big version of images in the zophrenic CMS
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
*	@version    $Id: zp.imageviewer.js 12 2009-04-15 16:14:18Z timbenniks $
**/
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
		imageWrapper 	= jQuery('<div></div>').addClass('image-wrapper');
		imageMagnifier 	= jQuery('<div></div>').addClass('image-magnifier').css({top: this.element.offset().top, left: this.element.offset().left});
		bigImg			= jQuery('<img alt="'+ui.element.attr('alt')+'" />').hide();
		
		this.element.wrap(imageWrapper).after(imageMagnifier).parent().hover(function()
		{
			$('.image-magnifier', $(this).parent()).show(150);
		},
		function()
		{
			$('.image-magnifier', $(this).parent()).hide(150);
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
		var imageTempSrc 	= imageOrigSrc[0].split('/image/');
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
				.css({display: 'none', height: $(document).height(), zIndex: 8000})
				.animate({opacity: .7}, 200)
				.click(function()
				{
					ui.kill()
				});
		}
		
		$('.popup-overlay').show();
				
		imgObj
			.css({display: 'none', position: "absolute", zIndex: 9999, width: fullWidth, height: fullHeight, left: leftPos, top: topPos})
			.fadeIn(200)
			.click(function()
			{
				ui.kill()
			});
	},
	
	kill: function() 
	{
		jQuery('.popup-overlay').fadeOut(200);
		jQuery('.loaded-bigImage').fadeOut(200);
	   	this.destroy();
	}
});