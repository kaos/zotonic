<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
	<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
	<title>Zophrenic {% block title %}Admin{% endblock %}</title>

	<meta name="author" content="Tim Benniks" />
	
	<link href="/lib/css/zp-compressed.css" type="text/css" media="screen" rel="stylesheet" />
	<link href="/lib/css/zp-admin.css" type="text/css" media="screen" rel="stylesheet" />

	<link href="/lib/css/zp-wysiwyg.css" type="text/css" media="screen" rel="stylesheet" />
	<link href="/lib/css/zp-dialog.css" type="text/css" media="screen" rel="stylesheet" />
	<link href="/lib/css/zp-formreplace.css" type="text/css" media="screen" rel="stylesheet" />
	<link href="/lib/css/zp-finder.css" type="text/css" media="screen" rel="stylesheet" />
	<link href="/lib/css/zp-growl.css" type="text/css" media="screen" rel="stylesheet" />
	
	<!--[if IE]><link href="/lib/css/zp-ie.css" type="text/css" media="screen" rel="stylesheet" /><![endif]-->
</head>
<body class="zp-wide">
	<div class="skip">
		<a href="#content" title="Go directly to page content">Go to page content</a>
	</div>
	<div class="zp-wrapper">
		<div id="header">
			<h1 class="zophrenic"><a href="/admin/">Zophrenic</a></h1>
		</div>

		<!--[if lte IE 6]>
		<div id="ie6-upgrade" class="notification notice">
			<h4>Warning</h4>
			Your version of Internet Explorer is extremely <strong>out of date</strong> and has known <strong>security issues!</strong><br />
			To have the best experience using the Zophrenic admin and to protect your computer correct this by installing <a href="http://www.opera.com">Opera</a>, <a href="http://mozilla.com">FireFox</a>, <a href="http://www.apple.com/safari/download/">Safari</a> or a higher version <a href="http://www.microsoft.com/windows/downloads/ie/getitnow.mspx">Internet Explorer</a>.
		</div>
		<![endif]-->

		{% block navigation %}
		{% wire id="zp-logoff" action={logoff} %}
		<ul id="navigation" class="zp-100">
			<li><a href="/admin/" {% if page_dashboard %}class="current"{% endif %}>Dashboard</a></li>
			<li><a href="/admin/overview/" {% if page_overview %}class="current"{% endif %}>Pages</a></li>
			<li><a href="{% url admin_media %}" {% if page_media %}class="current"{% endif %}>Media</a></li>
			{% all include "_admin_menu_module.tpl" %}
			<li class="search-box">
				<div class="quick-search-wrapper">
					<form method="get" action="{% url admin_overview_rsc %}">
						<input type="text" name="qs" value="{{ q.qs|escape }}" class="left" />
						<button type="submit">Search</button>
					</form>
				</div>
			</li>
			<li class="right"><a id="zp-logoff" href="#">Logoff</a></li>
		</ul>
		{% endblock %}

		{% block content %}{% endblock %}
	</div>

	{% include "_admin_js_include.tpl" %}
	{% block tinymce %}{% endblock %}
	{% block js_extra %}{% endblock %}
	
	{% script %}
	
</body>
</html>
