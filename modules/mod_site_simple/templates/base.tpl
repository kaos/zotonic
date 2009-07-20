<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
	<title>{% block title %}m.rsc[id].seo_title{% endblock %}</title>

	<meta http-equiv="Content-type" content="text/html; charset=utf-8" />
	<meta name="keywords" content="{{ m.rsc[id].seo_keywords }}" />
	<meta name="description" content="{{ m.rsc[id].seo_desc }}" />
	<meta name="author" content="Zotonic &copy; 2009" />
	<meta name="robots" content="index,follow" />

	<style type="text/css">
		html,body,div,span,applet,object,iframe,h1,h2,h3,h4,h5,h6,p,blockquote,pre,a,abbr,acronym,address,big,cite,code,del,dfn,em,font,img,ins,kbd,q,s,samp,small,strike,strong,sub,sup,tt,var,b,u,i,center,dl,dt,dd,ol,ul,li,fieldset,form,label,legend,table,caption,tbody,tfoot,thead,tr,th,td{border:0;outline:0;font-size:100%;vertical-align:baseline;background:transparent;margin:0;padding:0}body{line-height:1;font:12px/18px "Lucida Grande", Arial, sans-serif;color:#111}ol,ul{list-style:none}blockquote,q{quotes:none}blockquote:before,blockquote:after,q:before,q:after{content:none}:focus{outline:0}ins{text-decoration:none}del{text-decoration:line-through}table{border-collapse:collapse;border-spacing:0}hr{height:0;border:0;border-top:1px solid #e0e0e0;width:100%;margin:0 0 17px;padding:0}.zp-wrapper{width:970px;margin:0 auto}.zp-5,.zp-10,.zp-15,.zp-20,.zp-25,.zp-30,.zp-33,.zp-35,.zp-40,.zp-45,.zp-50,.zp-55,.zp-60,.zp-65,.zp-67,.zp-70,.zp-75,.zp-80,.zp-85,.zp-90,.zp-95,.zp-100{float:left;display:inline}.zp-5{width:5%}.zp-10{width:10%}.zp-15{width:15%}.zp-20{width:20%}.zp-25{width:25%}.zp-30{width:30%}.zp-33{width:33.33%}.zp-35{width:35%}.zp-40{width:40%}.zp-45{width:45%}.zp-50{width:50%}.zp-55{width:55%}.zp-60{width:60%}.zp-65{width:65%}.zp-67{width:66.67%}.zp-70{width:70%}.zp-75{width:75%}.zp-80{width:80%}.zp-85{width:85%}.zp-90{width:90%}.zp-95{width:95%}.zp-100{width:100%}.last{padding:0!important}.clear{clear:both}.left{float:left}.right{float:right}.clearfix:after,.zp-wrapper:after{content:".";display:block;clear:both;visibility:hidden;line-height:0;height:0}.clearfix,.zp-wrapper{display:inline-block}html[xmlns] .clearfix,html[xmlns] .zp-wrapper{display:block}* html .clearfix,* html .zp-wrapper{height:1%}.skip{display:block;left:-9999px;position:absolute;visibility:hidden}h1{font-size:36px;line-height:36px;font-weight:400;font-family:Georgia, "Times new roman", serif}h2{font-size:18px;line-height:36px}h3{font-size:12px;line-height:18px;font-weight:700;color:#000;margin:0}h4{font-size:12px;line-height:18px;font-weight:400;color:#666;margin:0}h5,h6{font-size:12px;line-height:18px;font-weight:400;margin:0}p{margin:0 0 18px}p img,li img{float:left;margin:4px 6px 0 0;padding:0}p img.right,li img.right{float:right;margin:4px 0 6px;padding:0}a,a:focus{color:#009;text-decoration:underline}.list{list-style:none;margin:0;padding:0}.list li{display:inline;margin:0 6px 0 0}a.button,button{display:block;float:left;border:1px solid #ccc;background:#ccc url(../images/button.png) left top repeat-x;font-family:"Lucida Grande", Tahoma, Arial, Verdana, sans-serif;font-size:11px;line-height:16px;text-decoration:none;font-weight:400;color:#333;cursor:pointer;white-space:nowrap;vertical-align:baseline;border-color:#999 #858585 #666;margin:0 3px 15px 0;padding:2px 6px}button{width:auto;overflow:visible;padding:1px 4px}a.button{line-height:14px}button[type]{line-height:16px;padding:1px 4px}a.button:hover,button:hover{background-color:#ccc;border:1px solid #000;color:#000;text-decoration:none}.notification{font-size:11px;line-height:18px;margin:0 0 17px;padding:0 4px}.notice{background:#FFF6BF;color:#514721;border-bottom:1px solid #FFD324}.error{background:#FBE3E4;color:#8a1f11;border-bottom:1px solid #FBC2C4}.success{background:#E6EFC2;color:#264409;border-bottom:1px solid #C6D880}fieldset{border:1px solid #ccc;margin:0 0 18px;padding:9px}legend{color:#333;font-size:18px;line-height:18px;padding:0}label{float:left;width:100px;display:block;text-align:left;cursor:pointer;color:#333;margin:0 12px 0 0}.form-item{margin:0 0 11px}textarea,input{border:solid #ddd;border-width:1px 1px 2px;padding:4px}textarea{font-family:"Lucida Sans",Helvetica,sans-serif;font-size:11px}textarea:focus,input:focus{background:#f9f9f9;border:solid #ddd;border-width:1px 1px 2px}input.form-field-error,textarea.form-field-error{background:#FBE3E4;color:#8A1F11;border-color:#FBC2C4 #FBC2C4 #ee9b9e;border-style:solid;border-width:1px 1px 2px}input.form-field-notice,textarea.form-field-notice{background:#FFF6BF;color:#514721;border-color:#FFD324 #FFD324 #e3bb1b;border-style:solid;border-width:1px 1px 2px}select{border:1px solid #ccc;background:#f9f9f9;color:#333}input[type=checkbox],input[type=radio]{margin:3px 4px 0 0}input[type=radio]{background-color:#fff;color:#000}option{background:#fff;color:#000}optgroup{background:#f2f2f2;color:#111}a:hover,input[type=checkbox]{color:#000}
	
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

	<link href="/lib/css/zp-project.css" type="text/css" media="screen" rel="stylesheet" /> 

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