{% extends "base.tpl" %}

{% block title %} Betaling ontvangen {% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
	{% if sig_invalid %}

		<h2>Er ging iets fout</h2>
		
		<div class="block clearfix">
		<p>We konden uw betalingsstatus niet afhandelen. De verwijzing naar deze pagina was waarschijnlijk niet correct.</p>
		
		<p>Heeft u wel betaald?  Neem dan <a href="mailto:">contact met ons op</a>.
			
		<p>De foutmelding was: <span class="error">controlegetal gegevens is fout</a>.</p>
		
		<p>U kunt <a href="/">verder winkelen</a> of <a href="{% url shop_cart %}">naar uw boodschappenmand</a>.</p>

	{% else %}

		<h2>Order status</h2>
		<div class="block clearfix">
		
			<p>Bedankt voor uw bestelling bij Hans Struijk Fietsen.  Uw bestelling is met succes afgerond.
				U kunt deze pagina afdrukken voor uw administratie.</p>
			<h3>Uw opties</h3>
			<p>U heeft een aantal vervolg opties. Geen van deze opties zijn verplicht.  U kunt ook <a href="/">verder winkelen.</a></p>
			
			<div class="button-wrapper">
				<button>Onthoud mij op deze computer</button>
				<button>Registreer mij als terugkomende klant</button>
				<button>Vergeet mij</button>
			</div>

			<!-- show status van de order en de order details -->
			
		</div>

		<div class="block clearfix">
			<h3>Uw bestelling</h3>
			
			{% include "_shop_order_view.tpl" order=order %}
		</div>
	{% endif %}
	</div>	
{% endblock %}
