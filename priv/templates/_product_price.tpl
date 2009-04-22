{% with m.shop_product[rsc_id].price as p %}
	{% if p.old_price %}
		<div class="notification notice">Oude prijs &euro;{{ p.old_price|format_price }} nu voor:</div>
	{% endif %}
	
	{% if p.is_variant %}
		<div class="notification notice">Dit product heeft meerdere varianten.</div>
	{% endif %}
	<div class="product-price clearfix">
		{% if p.is_variant %}
			<div class="product-choose-variant">
				{% wire id="choose-variant" type="change" action={shop_addcart id=rsc_id} %}
				<select id="choose-variant">
					<option value="">Maak uw keuze</option>
				
					{% for v in m.shop_product[rsc_id].variants %}
						<option value="{{ v.variant }}">
							{{ v.title }} - &euro;{{ v.price_actual|format_price }}
						</option>
					{% endfor %}
				</select>
			</div>
		{% endif %}
		
		<h3>{% if p.is_variant %}<span>vanaf</span> {% endif %} &euro;{{p.price|format_price}}</h3>
		
		<div class="clearfix button-wrapper right">
			{% button id="product-buy-basket" class="buy-me right-side-button" text="In winkelmand &raquo;" action={shop_buynow id=rsc_id} %}
		</div>
	</div>
{% endwith %}