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
				<h2>Whoops, {{ error_code }}</h2>
				<div class="block">
					<h3>The ambulance now takes you to room 404</h3>
					<p>
						Etiam mi orci, ullamcorper in, malesuada nec, tempor in, dui. Class aptent taciti sociosqu ad litora torquent per conubia nostra,
						per inceptos himenaeos. Ut adipiscing mattis lacus. Nullam tincidunt augue in eros. Donec felis nisi, ultrices vel, mollis in, 
						scelerisque a, leo. Morbi ut ipsum ut pede dapibus scelerisque. Nullam a ipsum id magna laoreet placerat. Ut a magna. Nulla facilisi. 
						Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Mauris odio ipsum, iaculis ut, vestibulum a, 
						convallis id, nulla.
					</p>
				</div>
				
				<div class="block">
					<p>
						{% @wire id="error-trigger" action={slide_toggle speed=200 target="error-explain"} %} 
						<a href="javascript:void(0);" id="error-trigger">Find out more about the error &raquo;</a>
					</p>
					<pre style="display: none;" id="error-explain">lalalalalalalalal</pre>
				</div>
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
