{% extends "base.tpl" %}

{% block title %}Checkout{% endblock %}

{% block content %}
	<div id="register-forms" class="zp-50">
		<div class="padding">
			<h2>Geen klant? Vul dan uw gegevens in.</h2>
			<div class="block clearfix">
				<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.</p>
				
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
					
					<fieldset>
						<legend>Adres gegevens</legend>
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
								<input id="billing-address" type="checkbox" />Mijn factuuradres is anders dan mijn afeveradres.
								{% wire id="billing-address" action={fade_in target="damn"} %}
							</label>
						</div>
					</fieldset>
					
					<p id="damn" style="display: none;">lalala</p>

					<div class="button-wrapper">
						<button>Ga naar betalen &raquo;</button>
					</div>
				</form>
				
			</div>
		</div>
	</div>
	
	<div id="logon" class="zp-50">
		<div class="padding">
			<h2>Bent u al klant?</h2>
			<div class="block clearfix">
				<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.</p>
				
				{# {% wire id="logon-form" type="submit"} %} #}
				<form method="post" action="postback" id="logon-form">
					<fieldset>
						<legend>Login</legend>
						<div class="form-item">
							<label>Name</label>
							<input type="text" name="name" value="" />
						</div>
						<div class="form-item">
							<label>Password</label>
							<input type="password" name="password" value="" />
						</div>
						<div class="button-wrapper">
							<button>Login en betaal &raquo;</button>
						</div>
					</fieldset>
				</form>
			</div>
		</div>
	</div>


{% endblock %}

{% block sidebar %}{% endblock %}
