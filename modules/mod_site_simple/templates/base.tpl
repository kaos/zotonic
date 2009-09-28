<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
	<title>{% block title %}m.rsc[id].seo_title{% endblock %}</title>

	<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
	<meta name="keywords" content="{{ m.rsc[id].seo_keywords }}" />
	<meta name="description" content="{{ m.rsc[id].seo_desc }}" />
	<meta name="author" content="Zotonic &copy; 2009" />

	{% lib
		"/css/zp-compressed.css"
		"/css/zp-project.css"
	%}

	<style type="text/css">
	
		#header {
			margin: 18px 0;
		}
		
		#sidebar {
			font-size: 11px;
		}

		p.intro { 
			font-style: bold;
		} 

		#content .padding {
			padding: 0 70px 0 0;
		}

		.item .padding {
			padding: 0 12px 0 0;
		}

		.list-item h2 {
			margin: 0;
			line-height: 18px;
		}
	
	</style>

	<!--[if IE]><link href="/lib/css/zp-ie.css" type="text/css" media="screen" rel="stylesheet" /><![endif]--> 
</head>
<body>
	<div class="skip">
		<a href="#content" title="Go directly to page content">Go to page content</a>
	</div>
	
	<div class="zp-wrapper">
		<div id="header">
			<ul id="navigation" class="list">
				<li class="first"><a href="/" title="home">Home</a></li>
				<li><a href="/page/2" title="">Page 2</a></li>
				<li><a href="/page/3" title="">Page 3</a></li>
				<li class="last"><a href="/page1" title="">Page 1</a></li>
			</ul>
		</div>

		<div id="content-wrapper" class="clearfix">
			{% block content %}{% endblock %}
			{% block sidebar %}{% endblock %}
		</div>

	</div>

	{% include "_js_include.tpl" %}

	{% script %}
</body>
</html>