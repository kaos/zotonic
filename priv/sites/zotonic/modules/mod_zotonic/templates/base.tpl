<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
	<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
	<title>{% block title %}Zotonic{% endblock %}</title>

	<meta name="keywords" content="Zotonic, CMS, Erlang, Pragmatic innovation, content management system" />
	<meta name="description" content="Zotonic is a content management system build in Erlang. It is build by and for professional webdevelopers who have a pragmatic way of thinking and need stuff that works." />
	<meta name="author" content="Marc Worrell, Tim Benniks" />

	{% lib
		"/css/zp-compressed.css"
		"/css/zp-project.css"
	%}
	
	<link rel="icon" href="favicon.ico" type="image/x-icon" />
	<link rel="shortcut icon" href="favicon.ico" type="image/x-icon" />
	{% include "_html_head.tpl" %}
	
	<!--[if IE]><link href=./lib/zp-ie.css" type="text/css" media="screen" rel="stylesheet" /><![endif]-->
</head>

<body class="{% block pageclass %}{% endblock %}">
<div class="skip"><a href="#content-area" title="Go directly to page content">Go to page content</a></div>

<div class="zp-wrapper">
	<div id="header">
		<a href="/" title="Zotonic home"><img src="./lib/images/zotonic.gif" alt="Zotonic" class="logo" /></a>
	
		{% block pageheader %}{% endblock %}	
		<div class="navigation-search-wrapper clearfix">
			{% block navigation %}{% menu id=id %}{% endblock %}
			{% block search %}{% include "_searchfield.tpl" %}{% endblock %}
		</div>	
	</div>
	
	<div id="content-area" class="clearfix">
		<div id="content" class="zp-65">
			<div class="padding">
				{% block content %}
				{% endblock %}
			</div>
		</div>
		<div id="sidebar" class="zp-35">
			{% block sidebar %}
			{% endblock %}
		</div>
	</div>
	
	<div class="push"><!-- push down --></div>	
</div>

<div id="footer">
	{% include "_footer.tpl" %}
</div>

{% include "_js_include.tpl" %}

{% script %}

{#
<script type="text/javascript">var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));</script>
<script type="text/javascript">try {var pageTracker = _gat._getTracker("UA-7726967-1");pageTracker._trackPageview();} catch(err) {}</script>
#}

</body>
</html>
