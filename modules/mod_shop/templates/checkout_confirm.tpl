{% extends "base.tpl" %}

{% block title %} Betaling ontvangen {% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
	{% if sig_invalid %}

		<h2>Er ging iets fout</h2>
		
		<div class="block clearfix">
		<p>
			We konden uw betalingsstatus niet afhandelen. De verwijzing naar deze pagina was waarschijnlijk niet correct.
			Heeft u wel betaald? Neem dan <a href="mailto:">contact met ons op</a>.
		</p>
			
		<p>De foutmelding was: <span class="notification error">controlegetal gegevens is fout</a>.</p>
		
		<p>U kunt <a href="/">verder winkelen</a> of <a href="{% url shop_cart %}">naar uw boodschappenmand</a>.</p>

	{% else %}
		{% if order.paid %}

		<h2>Bestelling ontvangen</h2>
		<div class="block clearfix">
		
			<p>
				Bedankt voor uw bestelling bij Hans Struijk Fietsen.  Uw bestelling is met succes afgerond.
				U kunt deze pagina afdrukken voor uw administratie.
			</p>

		{% else %}
		<h2>Betaling (nog) niet ontvangen</h2>

		<div class="block clearfix">

			{% ifequal order.status 'payment_refused' %}
				<p>Uw betaling is geweigerd door uw bank of credit card maatschappij.  U kunt <a href="{% url shop_cart %}">opnieuw proberen te betalen</p></p>
			{% else %}
				{% ifequal order.status 'payment_pending' %}
					<p>Wij hebben uw betaling nog niet ontvangen. Zodra wij uw betaling ontvangen hebben sturen wij u een e-mail en handelen we uw bestelling verder af.</p>
				{% else %}
					<p>Er ging iets mis tijdens de betaling, wij hebben uw betaling niet ontvangen. Heeft u wel betaald? Neem dan <a href="mailto:">contact met ons op</a>.
				{% endifequal %}
			{% endifequal %}

		{% endif %}

			<h3>Uw opties</h3>
			<p>U heeft een aantal vervolg opties. Geen van deze opties zijn verplicht.  U kunt ook <a href="/">verder winkelen.</a></p>
			
			<div class="button-wrapper">
				<button>Onthoud mij op deze computer</button>
				<button>Registreer mij als terugkomende klant</button>
				<button>Vergeet mij</button>
			</div>

			<!-- show status van de order en de order details -->
			
		</div>

		<h2>Uw bestelling</h2>
		<div class="block clearfix">
			{% include "_shop_order_view.tpl" order=order %}
		</div>
	{% endif %}
	</div>	
{% endblock %}
