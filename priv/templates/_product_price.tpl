{#
	This template is directly used and also by an event handler.
	It is reloaded in the div "product-price" when the input is selected.

	Parameters are:

		When direct: rsc_id
		Via event handler: rsc_id, variant
#}

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
				{% wire id="choose-variant" type="change" action={shop_select_variant id=rsc_id} %}
				<select id="choose-variant">
					<option value="">Maak uw keuze</option>
				
					{% for v in m.shop_product[rsc_id].variants %}
						<option value="{{ v.variant }}"{% ifequal v.variant variant %} selected="selected" {% endifequal %}>
							{{ v.title }} - &euro;{{ v.price_actual|format_price }}
						</option>
					{% endfor %}
				</select>
			</div>

			{% if variant %}
				{% for v in m.shop_product[rsc_id].variants %}
					{% ifequal v.variant variant %}
						<h3>&euro;{{v.price_actual|format_price}}</h3>
						<div class="clearfix button-wrapper right">
							{% button id="product-buy-basket" class="buy-me right-side-button" text="In winkelmand &raquo;" action={shop_buynow id=rsc_id variant=variant} %}
						</div>
					{% endifequal %}
				{% endfor %}
			{% else %}
				<h3><span>vanaf</span> &euro;{{p.price|format_price}}</h3>
			{% endif %}
		{% else %}
			<h3>&euro;{{p.price|format_price}}</h3>

			<div class="clearfix button-wrapper right">
				{% button id="product-buy-basket" class="buy-me right-side-button" text="In winkelmand &raquo;" action={shop_buynow id=rsc_id variant=""} %}
			</div>
		{% endif %}
		
	</div>
{% endwith %}
