<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
	<title>{% block title %}Home pagina{% endblock %} &ndash; Hans Struijk Fietsen</title>

	<link href="/lib/css/zp-base.css" type="text/css" media="screen" rel="stylesheet" /> 
	<link href="/lib/css/zp-type.css" type="text/css" media="screen" rel="stylesheet" /> 
	<link href="/lib/css/zp-forms.css" type="text/css" media="screen" rel="stylesheet" /> 
	<link href="/lib/css/zp-project.css" type="text/css" media="screen" rel="stylesheet" /> 

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
					<li class="logo"><h1>Hans Struijk Fietsen</h1></li>
					<li><a href="#" title="#" class="current">Home</a></li>
					<li><a href="#" title="#">Fietsen</a></li>
					<li><a href="#" title="#">Accesoires</a></li>
					<li><a href="#" title="#">Service</a></li>
					<li class="lang" title="Nederlands"><a href="#" title="Nederlands" class="dutch"><span>Nederlands</span></a></li>
					<li class="lang" title="English"><a href="#" title="English" class="english"><span>English</span></a></li>
				</ul>

				<!-- Search bar -->
				<div id="search" class="zp-25 right">
					<form id="searchform" method="post" action="postback">
						<fieldset>
							<input id="quick-search" type="text" name="search" />
							<button>Zoek</button>
						</fieldset>
					</form>
				</div>
			</div>	
		</div>
		<div id="content" class="zp-100 clearfix">
			<div id="sidebar" class="zp-25">
				{% block sidebar %}{% endblock %}
			</div>
			<div id="content-area" class="zp-75">
				{% block content %}{% endblock %}
			</div>
		</div>
		{% include "_footer.tpl" %}
	</div>
	
	<script type="text/javascript" src="/lib/js/apps/jquery-1.3.js"></script>
    <script type="text/javascript" src="/lib/js/apps/jquery-ui-all-1.6rc5.min.js"></script>
    <script type="text/javascript" src="/lib/js/apps/zophrenic-1.0.js"></script>
    <script type="text/javascript" src="/lib/js/modules/jquery.notice.js"></script>
    <script type="text/javascript" src="/lib/js/modules/livevalidation-1.3.js"></script>

	{% @script %}
	
</body>
</html>
