{% extends "email_base.tpl" %}

{% block title %}Uw bestelling bij Mijn Fietsen{% endblock %}

{% block body %}
	<p>Geachte {{order.first_name}} {{order.lastname_prefix}} {{ order.lastname}},</p>

	<p>Hartelijk dank voor uw bestelling bij Mijn Fietsen.  Wij hebben uw betaling ontvangen en zullen U zo spoedig mogelijk de bestelde artikelen opsturen.</p>
	
	<p>Hieronder vind U een overzicht van uw bestelling.</p>
		
	<p>Met vriendelijke groet,<br/><br/>Mijn Fietsen</p>
	
	<hr/>

	{% include "../_shop_order_view.tpl" order=order %}
	
{% endblock %}
