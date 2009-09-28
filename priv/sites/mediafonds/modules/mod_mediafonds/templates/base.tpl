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

	<title>{% block title %}Homepage?{% endblock %} &mdash; Mediafonds</title>
	<meta charset="utf-8" />
	<meta name="author" content="Tim Benniks &copy; 2009" />
	{% all include "_html_head.tpl" %}
	{% lib
		"/css/zp-compressed.css"
		"/css/zp-project.css"
	%}

	<!--[if IE]><link href="/lib/css/zp-ie.css" type="text/css" media="screen" rel="stylesheet" /><![endif]-->
	
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
		
		<header>
			<nav id="subnav-top">
				<ul class="list">
					<li><a href="#" title="">Home</a></li>
					<li><a href="#" title="">Contact</a></li>
					<li><a href="#" title="">Het fonds</a></li>
					<li><a href="#" title="">English</a></li>
					<li><a href="#" title="">RSS</a></li>
					<li><a href="#" title="">FAQ</a></li>
					<li><a href="#" title="">Links</a></li>
				</ul>
			</nav>
			
			<figure>
				<a href="/" title="">
					<img src="./lib/images/logo.jpg" alt="" />
				</a>
			</figure>
			
			<section id="nav-search-wrapper" class="clearfix">
			
				<nav id="main-nav" class="left">
					<ul class="list">
						<li><a href="#" title="">Aanvragen</a></li>
						<li><a href="#" title="">Etalage</a></li>
						<li><a href="#" title="">Toegekend</a></li>
						<li><a href="#" title="">Nieuws</a></li>
					</ul>
				</nav>
			
				<section id="search" class="right">
					<form>
						<fieldset>
							<input type="text" name="q" value="" />
							<button type="submit">Zoek</button>
						</fieldset>	
					</form>	
				</section>
				
			</section>
		</header>
		
		{% block mediaviewer %}{% endblock %}
		
		{% block content %}{% endblock %}
		
		<footer>
			<div class="zp-15">Stimuleringsfonds Nederlandse Culturele Mediaproducties</div>
			<div class="zp-15">&copy; Mediafonds Herengracht 609 1017 CE Amsterdam</div>
			<div class="zp-15">020 623 39 01 www.mediafonds.nl info@mediafonds.nl</div>
			<div class="zp-15">
				<a href="">Sitemap</a>
				<a href="">disclaimer</a>
				<a href="">Colofon</a>
			</div>
			<div class="zp-40 footer-logos">
				crap plaatjes
			</div>
		</footer>
	</section>

	{% include "_js_include.tpl" %}

	{% script %}

	{% all include "_html_body.tpl" %}
	
</body>
</html>