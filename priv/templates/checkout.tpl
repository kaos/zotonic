{% extends "base.tpl" %}

{% block title %}Checkout{% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<div class="padding">
			<h2>Persoonlijke gegevens</h2>
			<div class="block clearfix">
				<p>Vul uw naam en adres in om de bestelling af te ronden. Heeft u al eens eerder besteld, klik dan <a href="#">hier om in te loggen</a>.</p>
			
				{# {% wire id="checkout-form" type="submit" postback="doe_iets"} %} #}
				<form method="post" action="postback" id="checkout-form">
					<fieldset>
						<legend>Persoonlijke gegevens</legend>
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
							<input type="text" id="client-email" name="cient-email" value="" />
							{% validate id="client-email" type={presence} type={email}%}
						</div>
					</fieldset>
				
					<hr />
				
					<div class="zp-50">
						<fieldset>
							<legend>Afleveradres</legend>
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
								<input type="text" name="client-delivery-postal-code" id="client-delivery-postal-code" value="" />
								{% validate id="client-delivery-postal-code" type={presence} %}
							</div>
							<div class="form-item">
								<label for="client-delivery-postal-code">Land</label>
								<input type="text" name="client-delivery-country" id="client-delivery-country" value="" />
								{% validate id="client-delivery-country" type={presence} %}
							</div>
							<div class="form-item clearfix">
								<label class="billing-address-label" for="billing-address">
									<input id="billing-address" type="checkbox" />Mijn factuuradres is anders dan mijn afleveradres.
									{% wire id="billing-address" action={toggle target="billing-address-form"} %}
								</label>
							</div>
						</fieldset>
					</div>
					
					<div class="zp-50 hide" id="billing-address-form">
						<fieldset>
							<legend>Factuuradres</legend>
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
								<input type="text" name="client-billing-postal-code" id="client-billing-postal-code" value="" />
							</div>
							<div class="form-item">
								<label for="client-billing-postal-code">Land</label>
								<input type="text" name="client-billing-country" id="client-billing-country" value="" />
							</div>
						</fieldset>
					</div>
					<div class="button-wrapper clear right">
						{% button id="edit_cart" class="buy-me left-side-button" text="&laquo; Bewerk winkelmand" action={redirect dispatch="shop_cart"} %}
						{% button id="do_payment" class="buy-me right-side-button" text="Naar betalen &raquo;" %}
					</div>
				</form>
			</div>
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
				<li class="block" id="cart-product-{{c.id}}">
					<div class=" clearfix">
						<a href="{{ m.rsc[c.id].page_url }}">{% image m.rsc[c.id].media[1].filename width=35 height=35 crop alt="m.rsc[id].title" class="left" %}</a>
						<h3><a href="{{ m.rsc[c.id].page_url }}">{{ m.rsc[c.id].title }}</a></h3>
						<span id="count-{{c.id}}">{{ c.n }}</span> stuks à &euro; {{ c.price|format_price }}
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
		</div>	
	</div>
{% endblock %}