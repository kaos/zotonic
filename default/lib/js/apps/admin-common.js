$(function()
{
	$(window).bind('resize load', function()
	{
		if($(window).width() < 1200)
		{
			$('body').addClass('zp-normal').removeClass('zp-wide');
		}
		else
		{
			$('body').removeClass('zp-normal').addClass('zp-wide');	
		}
	});
});