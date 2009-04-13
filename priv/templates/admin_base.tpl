<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
	<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
	<title>Zophrenic admin</title>

	<meta name="author" content="Tim Benniks" />
	
	<link href="/lib/css/zp-base.css" type="text/css" media="screen" rel="stylesheet" />
	<link href="/lib/css/zp-type.css" type="text/css" media="screen" rel="stylesheet" />
	<link href="/lib/css/zp-forms.css" type="text/css" media="screen" rel="stylesheet" />
	<link href="/lib/css/zp-wysiwyg.css" type="text/css" media="screen" rel="stylesheet" />
	<link href="/lib/css/zp-admin.css" type="text/css" media="screen" rel="stylesheet" />
	
	<!--[if IE]><link href="/lib/css/zp-ie.css" type="text/css" media="screen" rel="stylesheet" /><![endif]-->

</head>
<body class="zp-wide">
	<div class="skip">
		<a href="#content" title="Go directly to page content">Go to page content</a>
	</div>

	<div class="zp-wrapper">
		<div id="header">
			<h1 class="zophrenic">Zophrenic</h1>
		</div>

		<ul id="navigation" class="zp-100">
			<li><a href="/admin/" class="current">Dashboard</a></li>
			<li><a href="/admin/overview/">Pages</a></li>
			<li><a href="#">Users</a></li>
			<li><a href="#">Products</a></li>
			<li class="right"><a href="#">Options</a></li>
		</ul>

		{% block content %}{% endblock %}
	</div>
</div>

<div id="footer" class="footer">
	<div class="footer-content"></div>
</div>

	{% include "_js_include.tpl" %}
	
	<script type="text/javascript">
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
	</script>
	{% script %}
	
</body>
</html>
