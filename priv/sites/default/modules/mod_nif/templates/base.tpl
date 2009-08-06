<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
	<!--
		Website build by: 
		Tim Benniks <tim@timbenniks.nl>
		Marc Worrell <marc@worrell.nl>
		
		Proudly powered by: Zotonic. <http://www.zotonic.com>
	-->
	
	<title>{% block title %}New Island Festival{% endblock %} &mdash; New Island Festival</title>

	<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
	<meta name="keywords" content="{{ m.rsc[id].seo_keywords }}" />
	<meta name="description" content="{{ m.rsc[id].seo_desc }}" />
	<meta name="author" content="Tim Benniks &copy; 2009" />

	{% lib
		"/css/zp-compressed.css"
		"/css/zp-project.css"
	%}
	
	<link rel="icon" href="/favicon.ico" type="image/x-icon" />
	<link rel="shortcut icon" href="/favicon.ico" type="image/x-icon" />
	{% include "_atom_feed_link.tpl" cat="news" %}
	
	<!-- make crappy ie8 emulate ie7 so the menu works -->
	<meta http-equiv="X-UA-Compatible" content="IE=EmulateIE7" />
	
	<!--[if IE]><link href="/lib/css/zp-ie.css" type="text/css" media="screen" rel="stylesheet" /><![endif]--> 
</head>
<body class="{% block pageclass %}{% endblock %}">
	<div class="skip"><a href="#content" title="Go directly to page content">Go to page content</a></div>
	
	<div class="zp-wrapper">
		
		{% block pageheader %}{% endblock %}	
		<div class="navigation-search-wrapper clearfix">
			{% block navigation %}{% menu id=id %}{% endblock %}
			{% block search %}{% include "_searchfield.tpl" %}{% endblock %}
		</div>	

		{% block content %}{% endblock %}
		
		<div id="footer">
			<div class="edit-this-page clearfix">
				{% include "_edit_button.tpl" %}
			</div>
			
			{% include "_footer.tpl" %}
		</div>
	</div>

	{% include "_js_include.tpl" %}

	{% script %}
	
	<script type="text/javascript">
	var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
	document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
	</script>
	<script type="text/javascript">
	try {
	var pageTracker = _gat._getTracker("UA-888876-10");
	pageTracker._trackPageview();
	} catch(err) {}</script>
</body>
</html>