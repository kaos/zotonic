{% extends "base.tpl" %}

{% block title %}Checkout{% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<div class="padding">
			<h2>Persoonlijke gegevens</h2>
			<div class="block clearfix">
				<p>Vul dan uw gegevens in om de bestelling te kunnen afronden. Heeft u al eens besteld, klik dan <a href="#">hier</a> om in te loggen.</p>
			
				{# {% wire id="checkout-form" type="submit" postback="doe_iets"} %} #}
				<form method="post" action="postback" id="checkout-form">
					<fieldset>
						<legend>Persoonlijk gegevens</legend>
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
							<legend>Aflever adres</legend>
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
							<legend>Factuur adres</legend>
							<div class="form-item">
								<label for="client-billing-address">Adres</label>
								<input type="text" name="client-billing-address" id="client-billing-address" value="" />
								{% validate id="client-billing-address" type={presence} %}
							</div>
							<div class="form-item">
								<label for="client-billing-city">Plaats</label>
								<input type="text" name="client-billing-city" id="client-billing-city" value="" />
								{% validate id="client-billing-city" type={presence} %}
							</div>
							<div class="form-item">
								<label for="client-billing-postal-code">Postcode</label>
								<input type="text" name="client-billing-postal-code" id="client-billing-postal-code" value="" />
								{% validate id="client-billing-postal-code" type={presence} %}
							</div>
						</fieldset>
					</div>
					<div class="button-wrapper clear">
						<button>Ga naar betalen &raquo;</button>
					</div>
				</form>
			</div>
		</div>
	</div>
{% endblock %}