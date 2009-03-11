<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
	<title>{% block title %}{% endblock %} &ndash; Hans Struijk Fietsen</title>

	<link href="/lib/css/zp-base.css" type="text/css" media="screen" rel="stylesheet" /> 
	<link href="/lib/css/zp-type.css" type="text/css" media="screen" rel="stylesheet" /> 
	<link href="/lib/css/zp-forms.css" type="text/css" media="screen" rel="stylesheet" /> 
	<link href="/lib/css/zp-project.css" type="text/css" media="screen" rel="stylesheet" /> 
	<link href="/lib/css/zp-wysiwyg.css" type="text/css" media="screen" rel="stylesheet" /> 

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
							<a href="/" title="Hans Struijk Fietsen">
								<img src="/lib/images/hans_struijk_logo.png" alt="hans Struijk Fietsen" />
							</a>
						</h1>
					</li>
					<li class="first"><a href="/compare" title="compare-bikes">Vergelijk fietsen</a></li>
					<li><a href="#" title="#">Accessoires</a></li>
					<li><a href="#" title="#">Service</a></li>
					<li class="lang" title="Nederlands"><a href="#" title="Nederlands" class="dutch"><span>Nederlands</span></a></li>
					<li class="lang" title="English"><a href="#" title="English" class="english"><span>English</span></a></li>
				</ul>

				<!-- Search bar -->
				<div id="search" class="zp-25 right">
					<form id="searchform" method="GET" action="/search">
						<fieldset>
							<input id="quick-search" type="text" name="search" />
							<button>Zoek</button>
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
