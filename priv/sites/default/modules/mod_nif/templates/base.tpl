<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
	<title>{% block title %}New Island Festival{% endblock %} &mdash; New Island Festival</title>

	<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
	<meta name="keywords" content="{{ m.rsc[id].seo_keywords }}" />
	<meta name="description" content="{{ m.rsc[id].seo_desc }}" />
	<meta name="author" content="Tim Benniks &copy; 2009" />

	<link href="/lib/css/zp-compressed.css" type="text/css" media="screen" rel="stylesheet" /> 
	<link href="/lib/css/zp-project.css" type="text/css" media="screen" rel="stylesheet" /> 

	<!--[if IE]><link href="/lib/css/zp-ie.css" type="text/css" media="screen" rel="stylesheet" /><![endif]--> 
</head>
<body class="{% block pageclass %}{% endblock %}">
	<div class="skip"><a href="#content" title="Go directly to page content">Go to page content</a></div>
	
	<div class="zp-wrapper">
		
		{% block pageheader %}{% endblock %}	
		{% block navigation %}{% endblock %}	
		{% block content %}{% endblock %}
		
		<div id="footer">
			<ul id="sponsor-logos">
				<li><a href=""></a></li>
			</ul>
		</div>
	</div>

	{% include "_js_include.tpl" %}

	{% script %}
</body>
</html>