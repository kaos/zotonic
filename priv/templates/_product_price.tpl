{#
	This template is directly used and also by an event handler.
	It is reloaded in the div "product-price" when the input is selected.

	Parameters are:

		When direct: rsc_id, variants
		Via event handler: rsc_id, variant, variants
#}

{% with m.shop_product[rsc_id].price as p %}
	{% if p.old_price %}
		<div class="notification notice">Oude prijs &euro;{{ p.old_price|format_price }} nu voor:</div>
	{% endif %}
	
	{% if not variants|length_is:1 %}
		<div class="notification notice">Dit product heeft meerdere varianten.</div>
	{% endif %}

	<div class="product-price clearfix">
		{% if not variants|length_is:1 %}
			<div class="product-choose-variant">
				{% wire id="choose-variant" type="change" action={shop_select_variant id=rsc_id} %}
				<select id="choose-variant">
					<option value="">Maak uw keuze</option>
				
					{% for v in variants %}
						<option value="{{ v.variant|escape }}"{% ifequal v.variant variant %} selected="selected" {% endifequal %}>
							{% if v.title %}{{ v.title|escape }}{% else %}{{ v.variant|escape }}{% endif %} - &euro;{{ v.price_actual|format_price }}
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
