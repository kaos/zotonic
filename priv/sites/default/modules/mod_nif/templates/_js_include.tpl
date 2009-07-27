<script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.3.2/jquery.min.js"></script>

{% lib 
	"js/apps/zotonic-1.0.js"
	"js/apps/z.widgetmanager.js"
	"js/z.cycle.js"
	"js/z.menu.js"
%}

<script type="text/javascript">
	$(function()
	{
	    $.widgetManager();
		$('h1#header').click(function()
		{
			window.location = '/';
		});
	});
</script>