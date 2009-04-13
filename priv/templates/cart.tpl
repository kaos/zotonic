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
			{% for id, count, price, count_price in shop_cart %}
			<li class="cart-item" id="cart-product-{{id}}">
				<div class="block clearfix">
					<div class="zp-10">
						<a href="{{ m.rsc[id].page_url }}">{% image m.rsc[id].media[1].filename width=60 height=60 crop alt="bags" class="left" %}</a>
					</div>
					<div class="zp-20">
						<h3><a href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a></h3>
						<p>Rood XX</p>
					</div>
					<div class="zp-20">Rood XX</div>
					<div class="zp-15">&euro; {{ price|format_price }}</div>
					<div class="zp-20">
						<h3><span id="count-{{id}}">{{ count }}</span> stuks</h3>
						{% button text="+" action={shop_cart_incr id=id} %}
						{% button text="-" action={shop_cart_decr id=id} %}
					</div>
					<div class="zp-15">
						<h3>&euro; <span id="cart-price-{{id}}">{{ count_price|format_price }}</h3>
						{% button text="verwijder" action={shop_cart_delete id=id} %}
					</div>
				</div>
			</li>
			{% empty %}
			<li class="clearfix cart-item">
				<div class="block clearfix">
					<p>Uw winkelmand is leeg</p>
				</div>
			</li>
			{% endfor %}
		</ul>
		<div class="block cart-product-price clearfix">
			<h3>&euro;<strong id="cart-price-total">{{ shop_cart_total|format_price }}</strong> <span>incl. btw</span></h3>
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