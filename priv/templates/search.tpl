{% extends "base.tpl" %}

{% block title %}Search{% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<!-- Area for the main content -->
		<h2 class="header-alone">Zoek resultaat</h2>
		
		{% pager result=result %}
		
		{% for rsc_id, rank in result %}
		<h3 class="block">{{ m.rsc[rsc_id].title }}</h3>
		<div class="block clearfix">
			<a href="{{ m.rsc[rsc_id].page_url }}">{% image m.rsc[rsc_id].media[1].filename width=180 height=140 crop alt="Handgrepen" class="left" %}</a>
			<div class="zp-70">
				<h4>Beschrijving</h4>
				<p>{{ m.rsc[rsc_id].intro }}</p>
				<div class="product-price clearfix">
					<h3>&euro;{{ m.rsc[rsc_id].price|format_price }} <!--span>incl. btw</span--></h3>

					<div class="clearfix button-wrapper right">
						{% button class="buy-me" text="meer info" action={redirect id=rsc_id} %}
						{% button class="buy-me" text="leg in winkelmand" action={shop_addcart id=rsc_id} %}
						{% button class="buy-me right-side-button" text="koop direct &raquo;" action={shop_buynow id=rsc_id} %}
					<!--
						<button class="buy-me right-side-button">Vraag een proefrit aan &raquo;</button>
					-->
					</div>
				</div>
			</div>
		</div>
		{% empty %}
		<div class="block clearfix">
			{% if q.qs %}
				<h4>Helaas niets gevonden bij uw zoekvraag voor <em>{{ q.qs|default:"niets"|escape }}</em>.</h4>
			{% else %}
				<h4>Vul uw zoekvraag in en probeer opnieuw.</em></h4>
			{% endif %}
		</div>

		<h3 class="block">Kijk ook eens naar</h3>
		<ul class="compare-list clearfix">
			{% for id in m.search[{featured cat="product"}] %}
				<li class="zp-33">
					<div class="block">
						<h3><a href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a></h3>
						<a href="{{ m.rsc[id].page_url }}">{% image m.rsc[id].media[1].filename width=225 height=130 crop %}</a>
						<p>{{ m.rsc[id].intro }}</p>
						<div class="product-price clearfix">
							<h3>&euro;{{m.rsc[id].price|format_price}} <!--span>incl. btw</span--></h3>
							{# An animate would be nice here #}
							<div class="clearfix button-wrapper right">
								{% button class="right right-side-button" text="Meer info &raquo;" action={redirect id=id} %}
							</div>
						</div>
					</div>
				</li>
			{% endfor %}
		</ul>
			
		{% endfor %}

		{% pager result=result %}
	</div>
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">
			{% include "_subnav.tpl" %}
		</div>
	</div>
{% endblock %}