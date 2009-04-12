{% extends "base.tpl" %}

{% block title %} Cart {% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<ul class="cart-list">
			<li class="cart-titles clearfix">
				<div class="block clearfix">
					<div class="zp-30">Product</div>
					<div class="zp-20">Eigenschappen</div>
					<div class="zp-15">Stukprijs</div>
					<div class="zp-20">Hoeveelheid</div>
					<div class="zp-15">Totaal prijs</div>
				</div>
			</li>
			<li class="cart-item">
				<div class="block clearfix">
					<div class="zp-30">
						{% image "140.jpg" width=60 height=60 crop alt="bags" class="left" %}
						<h3>Ortlieb</h3>
						<p>Rood XX</p>
					</div>
					<div class="zp-20">Rood XX</div>
					<div class="zp-15">&euro; 33</div>
					<div class="zp-20"><input type="text" value="1" class="card-item-amount" /></div>
					<div class="zp-15">&euro; 33</div>
				</div>
			</li>
			<li class="clearfix cart-item">
				<div class="block clearfix">
					<div class="zp-30">
						{% image "140.jpg" width=60 height=60 crop alt="bags" class="left" %}
						<h3>Ortlieb</h3>
						<p>Rood XX</p>
					</div>
					<div class="zp-20">Rood XX</div>
					<div class="zp-15">&euro; 33</div>
					<div class="zp-20"><input type="text" value="1" class="card-item-amount" /></div>
					<div class="zp-15">&euro; 33</div>
				</div>
			</li>
		</ul>
		<div class="block cart-product-price clearfix">
			<h3>&euro;66 <span>incl. btw</span></h3>
			{# An animate would be nice here #}
			<div class="clearfix button-wrapper right">
				{% button id="product-add-basket" class="buy-me left-side-button" text="&laquo; winkel verder" %}
				{% button id="product-buy-basket" class="buy-me right-side-button" text="reken af &raquo;" %}
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