{% extends "base.tpl" %}

{% block title %} Uw bestelling {% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<h2>Uw bestelling van {{ order.created|date:"Y-m-d"}}</h2>
		<div class="block clearfix">
			{% include "_shop_order_view.tpl" order=order %}
		</div>
	</div>	
{% endblock %}
