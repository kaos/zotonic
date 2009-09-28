<!DOCTYPE html>
<html lang="nl">
<head>

	<!--
		Website build by: 
		Marc Worrell <marc@worrell.nl>
	 	Arjan Scherpenisse <arjan@scherpenisse.net>
		Tim Benniks <tim@timbenniks.nl>
		
		Proudly powered by: Zotonic. <http://www.zotonic.com>
	-->
	
	<title>{% block title %}{{ m.rsc[id].seo_title | default m.rsc[id].title }}{% endblock %} &mdash; Mediafonds</title>

	<meta charset="utf-8" />
	<meta name="author" content="Tim Benniks &copy; 2009" />

	{% all include "_html_head.tpl" %}

	{% lib
		"/css/zp-compressed.css"
		"/css/zp-project.css"
	%}

	<!--[if IE]>
		<link href="/lib/css/zp-ie.css" type="text/css" media="screen" rel="stylesheet" />
	<![endif]-->
	
	<!-- Make Internet Explorer understand html5 -->
	<script>(function(){if(!/*@cc_on!@*/0)return;var e = "abbr,article,aside,audio,canvas,datalist,details,dialog,eventsource,figure,footer,header,hgroup,mark,menu,meter,nav,output,progress,section,time,video".split(','),i=e.length;while(i--){document.createElement(e[i])}})()</script>
	
	<link rel="icon" href="/favicon.ico" type="image/x-icon" />
	<meta http-equiv="X-UA-Compatible" content="IE=7.5">

</head>
<body class="{% block pageclass %}{% endblock %}">

	<section class="skip">
		<a href="#content" title="Go directly to page content">Go to page content</a>
	</section>
	
	<section class="zp-wrapper">
		{% block content %}{% endblock %}
	</section>

	{% include "_js_include.tpl" %}

	{% script %}

	{% all include "_html_body.tpl" %}
	
</body>
</html>