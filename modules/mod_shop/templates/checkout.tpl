{% extends "base.tpl" %}

{% block title %}Checkout{% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<div class="padding">
			
			{% wire id="form-checkout" type="submit" postback={payment total=shop_cart_total} %}
			<form id="form-checkout" method="post" action="postback" id="checkout-form">
				<h2>Persoonlijke gegevens</h2>
				<div class="block clearfix">
					<p>Vul uw naam en adres in om de bestelling af te ronden. Heeft u al eens eerder besteld, klik dan <a href="#">hier om in te loggen</a>.</p>
			
					{# {% wire id="checkout-form" type="submit" postback="doe_iets"} %} #}
					<fieldset>
						<div class="form-item">
							<label for="client-name">Naam</label>
							<input type="text" id="client-name" name="client-name" value="" />
							{% validate id="client-name" type={presence} %}
						</div>
						<div class="form-item">
							<label for="client-phone">Telefoon</label>
							<input type="text" id="client-phone" name="client-phone" value="" />
							{% validate id="client-phone" type={presence} %}
						</div>
						<div class="form-item">
							<label for="client-email">E-mail</label>
							<input type="text" id="client-email" name="client-email" value="" />
							{% validate id="client-email" type={presence} type={email}%}
						</div>
					</fieldset>
				</div>

				<h2>Adres gegevens</h2>
				<div class="block clearfix">
					<div class="notification notice">
						U hoeft het factuuradres alleen in te vullen indien het anders is dan het afleveradres.
					</div>
					<div class="zp-50 clearfix">
						<fieldset>
							<h3>Afleveradres</h3>
							<div class="form-item">
								<label for="client-delivery-attn">T.a.v.</label>
								<input type="text" name="client-delivery-attn" id="client-delivery-attn" value="" />
							</div>
							<div class="form-item">
								<label for="client-delivery-address">Adres</label>
								<input type="text" name="client-delivery-address" id="client-delivery-address" value="" />
								{% validate id="client-delivery-address" type={presence} %}
							</div>
							<div class="form-item">
								<label for="client-delivery-city">Plaats</label>
								<input type="text" name="client-delivery-city" id="client-delivery-city" value="" />
								{% validate id="client-delivery-city" type={presence} %}
							</div>
							<div class="form-item">
								<label for="client-delivery-postal-code">Postcode</label>
								<input type="text" name="client-delivery-postcode" id="client-delivery-postcode" value="" />
								{% validate id="client-delivery-postcode" type={presence} %}
							</div>
							<div class="form-item">
								<label for="client-delivery-country">Land</label>
								<select name="client-delivery-country" id="client-delivery-country">
									{% include "_countries.tpl" %}
								</select>
								{% validate id="client-delivery-country" type={presence} %}
							</div>
						</fieldset>
					</div>
				
					<div class="zp-50 clearfix" id="billing-address-form">
						<fieldset>
							<h3>Factuuradres</h3>
							<div class="form-item">
								<label for="client-billing-attn">T.a.v.</label>
								<input type="text" name="client-billing-attn" id="client-billing-attn" value="" />
							</div>
							<div class="form-item">
								<label for="client-billing-address">Adres</label>
								<input type="text" name="client-billing-address" id="client-billing-address" value="" />
							</div>
							<div class="form-item">
								<label for="client-billing-city">Plaats</label>
								<input type="text" name="client-billing-city" id="client-billing-city" value="" />
							</div>
							<div class="form-item">
								<label for="client-billing-postal-code">Postcode</label>
								<input type="text" name="client-billing-postcode" id="client-billing-postcode" value="" />
							</div>
							<div class="form-item">
								<label for="client-billing-country">Land</label>
								<select name="client-billing-country" id="client-billing-country">
									{% include "_countries.tpl" %}
								</select>
							</div>
						</fieldset>
					</div>
				</div>
				
				<div class="block button-wrapper clearfix">
					{% button id="edit_cart" class="buy-me left-side-button" text="&laquo; Bewerk winkelmand" action={redirect dispatch="shop_cart"} %}

					{% button id="do_payment" class="buy-me right-side-button" text="Naar betalen &raquo;" %}
				</div>
				
			</form>
			
			{% include "_cart_backorder.tpl" %}
			
		</div>
	</div>
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">
			
			{% include "_subnav.tpl" %}
			
			<h3 class="block">Winkelwagen</h3>
			<ul class="mini-cart cart-list">
				{% for c in shop_cart %}
				<li class="block">
					<div class=" clearfix">
						{% if c.media_id %}
							<a href="{{ m.rsc[c.id].page_url }}">{% image m.media[c.media_id].filename width=35 height=35 crop alt="{{ m.rsc[c.id].title }}" class="left" %}</a>
						{% else %}
							{% if m.rsc[c.id].depiction.filename %}
						<a href="{{ m.rsc[c.id].page_url }}">{% image m.rsc[c.id].depiction.filename width=35 height=35 crop alt="{{ m.rsc[c.id].title }}" class="left" %}</a>
							{% else %}
							&nbsp;
							{% endif %}
						{% endif %}

						<h3><a href="{{ m.rsc[c.id].page_url }}">{{ m.rsc[c.id].title }}</a></h3>
						{% if c.variant %}
							{% with m.shop_product[c.id].sku[c.variant] as sku %}
							<span>{% if sku.title %}{{ sku.title|escape }}{% else %}{{c.variant|escape}}{% endif %}</span>
							{% endwith %}
						{% endif %}
						<p>
							{% if c.price_avg|is_defined %}
								{{ c.n }} stuks à &euro; {{ c.price_avg|format_price }}
							{% else %}
								Niet leverbaar.
							{% endif %}
						</p>
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
				<h3>&euro;<strong id="cart-price-total">{{ shop_cart_total|format_price }}</strong> <span><a href="{% url shop_cart %}" title="Shopping cart">Pas aan</a></span></h3>
			</div>

			{% include "_cart_ordercosts.tpl" %}
		
		</div>	
	</div>
{% endblock %}