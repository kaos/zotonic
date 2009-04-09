{% extends "base.tpl" %}

{# comment #}

{% block title %}Search{% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<!-- Area for the main content -->
		<h2 class="header-alone">Zoek resultaat</h2>
		
		<h3 class="block">Trek urban</h3>
		<div class="block clearfix">
			{% image "trek_urban.jpg" width=180 height=140 crop alt="Handgrepen" class="left do_imageviewer" %}
			<div class="zp-70">
				<h4>Description</h4>
				<p>
					Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
					Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
				</p>
				<div class="product-price clearfix">
					<h3>&euro;1950 <span>incl. btw</span></h3>
			
					<div class="clearfix button-wrapper right">
						<button class="buy-me right-side-button">Vraag een proefrit aan &raquo;</button>
					</div>
				</div>
			</div>
		</div>
		
		<h3 class="block">Handgrepen</h3>
		<div class="block clearfix">
			{% image "handgrepen.jpg" width=180 height=140 crop alt="Handgrepen" class="left do_imageviewer" %}
			<div class="zp-70">
				<h4>Description</h4>
				<p>
					Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
					Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
				</p>
				<div class="product-price clearfix">
					<h3>&euro;66 <span>incl. btw</span></h3>
					<div class="clearfix button-wrapper right">
						{% button id="product-add-basket" class="buy-me" text="leg in winkelmand" postback="show_basket_notice" action={fade_in speed=350 target="product-notice"} %}
						{% button id="product-buy-basket" class="buy-me right-side-button" text="koop direct &raquo;" postback="show_basket_notice" action={fade_in speed=350 target="product-notice"} %}
					</div>
				</div>
			</div>
		</div>
	</div>
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">
			{% include "_subnav.tpl" %}
		</div>
	</div>
{% endblock %}