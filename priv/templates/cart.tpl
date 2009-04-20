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
			{% for c in shop_cart %}
			<li class="block" id="cart-product-{{c.id}}">
				<div class=" clearfix">
					<div class="zp-10">
						<a href="{{ m.rsc[c.id].page_url }}">{% image m.rsc[c.id].media[1].filename width=60 height=60 crop alt="{{ m.rsc[c.id].title }}" class="left" %}</a>
					</div>
					<div class="zp-20">
						<h3><a href="{{ m.rsc[c.id].page_url }}">{{ m.rsc[c.id].title }}</a></h3>
						<!--p>Rood XX</p-->
					</div>
					<div class="zp-20">{{ c.variant|default:"-" }}</div>
					<div class="zp-15">&euro; {{ c.price|format_price }}</div>
					<div class="zp-20">
						<h3><span id="count-{{c.id}}">{{ c.n }}</span> stuks</h3>
						{% button text="+" action={shop_cart_incr id=c.id} %}
						{% button text="-" action={shop_cart_decr id=c.id} %}
					</div>
					<div class="zp-15">
						<h3>&euro; <span id="cart-price-{{c.id}}">{{ c.nprice|format_price }}</span></h3>
						{% button text="verwijder" action={shop_cart_delete id=c.id} %}
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
			<div class="clearfix button-wrapper right">
				{% button id="product-add-basket" class="buy-me left-side-button" text="&laquo; winkel verder" action={redirect dispatch="overview"} %}
				{% button id="product-buy-basket" class="buy-me right-side-button" text="reken af &raquo;" action={redirect dispatch="shop_checkout"} %}
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