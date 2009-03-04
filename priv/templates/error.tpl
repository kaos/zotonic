<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
	<title>{{ error_code }} &ndash; Hans Struijk Fietsen</title>

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
					<li class="logo">
						<h1>
							<a href="/" title="Hans Struijk Fietsen">
								<img src="/lib/images/hans_struijk_logo.png" alt="hans Struijk Fietsen" />
							</a>
						</h1>
					</li>
					<li class="first"><a href="#" title="#">Fietsen</a></li>
					<li><a href="#" title="#">Accessoires</a></li>
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
			<div id="content-area" class="zp-100">
				<h2>{{ error_code }} error</h2>
				<div class="block">
					<h3>Dear website visitor. You stumbled upon one of our error pages</h3>
					<p>
						At first we would like to apologize
						for the fact that you got here. If the title of this page is 404 then you where looking for a page that does not exist
						in this system. It could have been moved or deleted. Please use our search for to search for anything you like or go the the <a href="/" title="home">homepage</a>.
					</p>
					<p>
						If the title of this page says 500, then the system had to handle 
						something it couldn't handle. A mail is now send to the system administrator.
					</p>
					
					<div class="notification error">
						<h5>{{ error_code }} error</h5>
						{% @wire id="error-trigger" action={slide_toggle speed=350 target="error-explain"} %} 
						<a href="javascript:void(0);" id="error-trigger">Click for error information.</a>
						<pre style="display: none;" id="error-explain">{{ error_dump }}</pre>
					</div>
					
				</div>
			</div>
		</div>
		{% include "_footer.tpl" %}
	</div>
	
	{% include "_js_include.tpl" %}

	{% @script %}
	
</body>
</html>
