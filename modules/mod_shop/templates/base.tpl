<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
	<title>{% block title %}m.rsc[id].seo_title{% endblock %} &ndash; Mijn Fietsen</title>

	<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
	<meta name="keywords" content="{{ m.rsc[id].seo_keywords }}" />
	<meta name="description" content="{{ m.rsc[id].seo_desc }}" />
	<meta name="author" content="Zophrenic &copy; 2009" />
	<meta name="robots" content="index,follow" />

	<link href="/lib/css/zp-base.css" type="text/css" media="screen" rel="stylesheet" /> 
	<link href="/lib/css/zp-type.css" type="text/css" media="screen" rel="stylesheet" /> 
	<link href="/lib/css/zp-forms.css" type="text/css" media="screen" rel="stylesheet" /> 
	<link href="/lib/css/zp-project.css" type="text/css" media="screen" rel="stylesheet" /> 
	<link href="/lib/css/zp-dialog.css" type="text/css" media="screen" rel="stylesheet" />

	<link rel="icon" href="" type="image/x-icon" />

	<!--[if IE]><link href="/lib/css/zp-ie.css" type="text/css" media="screen" rel="stylesheet" /><![endif]--> 
</head>
<body>
	<div class="skip">
		<a href="#content" title="Go directly to page content">Go to page content</a>
	</div>
	
	<div class="zp-wrapper">
		<div id="header" class="zp-100 clearfix">
			
			{% include "_shopping_card_small.tpl" %}
			
			<div class="navigation-wrapper clearfix">
				<!-- Main navigation -->
				<ul id="navigation" class="zp-60">
					<li class="logo">
						<h1>
							<a href="/" title="Mijn Fietsen">
								<img class="hide_id6" src="/lib/images/shop_logo.png" alt="Mijn Fietsen" />
								<!--[if lte IE 6]>
									<img src="/lib/images/shop_logo.gif" alt="Mijn Fietsen" />
								<![endif]-->
							</a>
						</h1>
					</li>

					{# <li class="first"><a href="/compare/mtb/1" title="compare-bikes">Fietsen</a></li> #}

					{% for cat in m.category.product.tree1 %}
						<li class="{% if forloop.first %} first {% endif %} "><a href="{% url overview cat=cat.name %}" title="{{ cat.title }}">{{ cat.title }}</a></li>
					{% endfor %}

					<li class="lang" title="Nederlands"><a href="#" title="Nederlands" class="dutch"><span>Nederlands</span></a></li>
					<li class="lang" title="English"><a href="#" title="English" class="english"><span>English</span></a></li>
				</ul>

				<!-- Search bar -->
				<div id="search" class="zp-25 right">
					<form id="searchform" method="get" action="/search">
						<fieldset>
							<input id="quick-search" type="text" name="qs" value="{{ q.qs|escape }}" />
							<button class="right-side-button">Zoek &raquo;</button>
						</fieldset>
					</form>
				</div>
			</div>
		</div>
		<div id="content" class="zp-100 clearfix">
			{% block sidebar %}
				<div id="sidebar" class="zp-25">
					<div class="padding">
						{% include "_subnav.tpl" %}
					</div>
				</div>
			{% endblock %}
			{% block content %}
				<div id="content-area" class="zp-75"></div>
			{% endblock %}
		</div>
		{% include "_footer.tpl" %}
	</div>

	{% include "_js_include.tpl" %}

	{% script %}
	
</body>
</html>
