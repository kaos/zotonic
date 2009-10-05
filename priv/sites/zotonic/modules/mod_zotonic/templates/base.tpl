<!DOCTYPE html>
<html lang="en">
<head>
	<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
	<title>{% block title %}{{ m.rsc[id].seo_title | default: m.rsc[id].title }}{% endblock %} &mdash; Zotonic</title>

	<link rel="icon" href="favicon.ico" type="image/x-icon" />
	<meta name="author" content="Tim Benniks" />

	{% all include "_html_head.tpl" %}

	{% lib
		"css/zp-compressed.css"
		"css/zp-project.css"
	%}

	<!--[if IE]>
	{% lib	"css/zp-ie.css" %}
	<![endif]-->
	
	<!-- Make ie6 understand html5 -->
	<script>
		document.createElement('header');
	   	document.createElement('footer');
	   	document.createElement('section');
	   	document.createElement('aside');
	   	document.createElement('nav');
	   	document.createElement('article');
   	</script>

	{% lib
		"/js/modules/cufon.js"
	%}

</head>

<body class="{% block page_class %}{% endblock %}">

	<section class="skip">
		<a href="#content-area" title="Go directly to page content">Go to page content</a>
	</section>

	<section class="zp-wrapper">
		<header class="clearfix">
			<figure id="logo" class="left">
				<a href="/" title="Home"><img src="/lib/images/logo.jpg" alt="Zotonic &mdash; Simple stuff that works" /></a>
			</figure>
			
			<nav class="right">
				{% menu id=id class="list" %}
			</nav>
		</header>

		{% block banner %}{% endblock %}
		
		<section id="content-area" class="clear clearfix">
			{% block content %}{% endblock %}
			{% block sidebar %}{% endblock %}
		</section>

		<div class="push"><!-- push down --></div>
	</section>
	
	<footer>
		<div id="footer-content"> 
			<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>
			<p>Latest blog posts, news items, tweets</p>
		</div>
	</footer>

	{% include "_js_include.tpl" %}

	{% script %}

	<script type="text/javascript">Cufon.now();</script>
</body>
</html>
