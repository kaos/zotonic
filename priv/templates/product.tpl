{% extends "base.tpl" %}

{# comment #}

{% block title %} Shimano 105 ST-5600 {% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<!-- Area for the main content -->
		<h2>Shimano 105 ST-5600</h2>
		
		<div class="product-wrapper clearfix">
			<div class="block">
				<ul class="product-image clearfix">
					<li class="zp-50"><img src="/lib/images/handgrepen.jpg" /></li>
					<li class="zp-50"><img src="/lib/images/handgrepen2.jpg" /></li>
				</ul>
				
				<div class="product-price clearfix">
					<h3>&euro;49 <span>incl. btw</span></h3>
					{# An animate would be nice here #}
					{# @wire id="product-add-basket" action= #}
					{#<button id="product-add-basket">+ in winkelmand</button>#}
					{% @button id="product-add-basket" text="+ in winkelmand" postback="show_basket_notice" action={fade_in speed=300 target="product-notice"} %}
				</div>
			</div>
			
			<h3 class="block">Omschrijving van de Shimano 105 ST-5600</h3>
			<div class="block">
				<p>Ergonomic innovation is probably more critical to the higher standard of the new 10-speed S.T.I. levers than the added gearing. The narrower cradle-style perch welcomes the high-on-the-hood riding style and accommodates all sizes of hands. The shift internals borrow the robustness from our MTB technology which results in a smooth and effortless shift.</p>
				
				<h3>Beoordeling</h3>
				<p class="clearfix"><img src="/lib/images/rating.jpg" alt="rating" /></p>
			</div>
		</div>
	</div>
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">
			
			{% include "_subnav.tpl" %}
			
			<h2>Gerelateerde artikelen</h2>
			<ul class="related-articles">
				<li class="block clearfix">
					<img src="/lib/images/trapper_klein.jpg" />
					<h4><a href="#">Shimano</a></h4>
					<p>PD-6620-G Trapper</p> 
					<p><a href="#">Bestel snel &raquo;</a></p>
				</li>
				<li class="block clearfix">
					<img src="/lib/images/trapper_klein.jpg" />
					<h4><a href="#">Shimano</a></h4>
					<p>PD-6620-G Trapper</p> 
					<p><a href="#">Bestel snel &raquo;</a></p>
				</li>
				<li class="block clearfix">
					<img src="/lib/images/trapper_klein.jpg" />
					<h4><a href="#">Shimano</a></h4>
					<p>PD-6620-G Trapper</p> 
					<p><a href="#">Bestel snel &raquo;</a></p>
				</li>
			</ul>
			
			<div class="notification notice" id="product-notice">
				<h3>Winkelmand informatie</h3>
				U heeft de Shimano 105 ST-5600 <strong>2 keer</strong> in uw <a href="#">winkelmand.</a>
			</div>
		</div>
	</div>
{% endblock %}