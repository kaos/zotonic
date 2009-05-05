{% extends "base.tpl" %}

{% block title %} Cart {% endblock %}

{% block content %}
	<div id="content-area">
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
				<li id="cart-product-{{c.id}}-{{c.variant|slugify}}">
					<div class="block clearfix">
					
						<div class="zp-10">
							{% if c.media_id %}
								<a href="{{ m.rsc[c.id].page_url }}">{% image m.media[c.media_id].filename width=75 height=60 crop alt="{{ m.rsc[c.id].title }}" class="left" %}</a>
							{% else %}
								{% if m.rsc[c.id].media[1].filename %}
							<a href="{{ m.rsc[c.id].page_url }}">{% image m.rsc[c.id].media[1].filename width=75 height=60 crop alt="{{ m.rsc[c.id].title }}" class="left" %}</a>
								{% else %}
								&nbsp;
								{% endif %}
							{% endif %}
						</div>
					
						<div class="zp-20">
							<h3><a href="{{ m.rsc[c.id].page_url }}">{{ m.rsc[c.id].title }}</a></h3>
							{% if not c.price_avg|is_defined %}
								<p class="notification error">Momenteel niet leverbaar.</p>
							{% endif %}
						
							<div id="cart-backorder-p-{{c.id}}-{{c.variant|slugify}}" class="notification notice clear {% if not c.backorder %}hide{% endif %}">
								Nabestelling: <span id="cart-backorder-{{c.id}}-{{c.variant|slugify}}"><strong>{{ c.backorder }}</strong></span> stuks.
							</div>
						</div>
					
						<div class="zp-20">
							{% with m.shop_product[c.id].sku[c.variant] as sku %}
								{% if sku %}
									{% if sku.title %}{{ sku.title }}{% else %}{{ sku.variant|default:"-" }}{% endif %}
								{% else %}
									{{ c.variant|default:"&nbsp;" }}
								{% endif %}
							{% endwith %}
						</div>
					
						<div class="zp-15">
							<span class="price">&euro;</span>
							<span class="price" id="cart-price-avg-{{c.id}}-{{c.variant|slugify}}">
								{{ c.price_avg|format_price }}
							</span>
							<span class="old-price" id="cart-price-old-{{c.id}}-{{c.variant|slugify}}">
								{% ifnotequal c.price_avg c.price_old %}&euro;{{ c.price_old|format_price }}{% endifnotequal %}
							</span>
						</div>
					
						<div class="zp-20">
							<h3><span id="count-{{c.id}}-{{c.variant|slugify}}">{{ c.n }}</span> stuks</h3>
							{% button text="+" action={shop_cart_incr id=c.id variant=c.variant} %}
							{% button text="-" action={shop_cart_decr id=c.id variant=c.variant} %}
						</div>
					
						<div class="zp-15">
							<h3>&euro; <span id="cart-price-{{c.id}}-{{c.variant|slugify}}">{{ c.total|format_price }}</span></h3>
							{% button text="verwijder" action={shop_cart_delete id=c.id variant=c.variant} %}
						</div>
					</div>
				</li>
			
				{% empty %}
			
				<li class="clearfix cart-item">
					<div class="block clearfix">
						<p>Uw winkelmand is leeg.</p>
					</div>
				</li>
			{% endfor %}
		</ul>
		
		<div class="block cart-product-price clearfix">
			<h3>&euro;<strong id="cart-price-total">{{ shop_cart_total|format_price }}</strong> <span>incl. btw</span></h3>
			<div class="clearfix button-wrapper right">
				{% button id="product-add-basket" class="buy-me left-side-button" text="&laquo; Winkel verder" action={redirect dispatch="overview"} %}
				{% button id="product-buy-basket" class="buy-me right-side-button" text="Reken af &raquo;" action={redirect dispatch="shop_checkout"} %}
			</div>
		</div>
		
		{% include "_cart_backorder.tpl" %}
		{% include "_cart_ordercosts.tpl" %}
	</div>

{% endblock %}

{% block sidebar %}{% endblock %}