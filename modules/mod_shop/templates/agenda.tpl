{% extends "base.tpl" %}

{% block title %}Agenda{% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<!-- Area for the main content -->
		<h2 class="header-alone">Aankomende evenementen</h2>

		{% with m.search.paged[{upcoming cat="event"}] as result %}
			{% pager result=result dispatch="agenda" %}

			{% for id in result %}
			<h3 class="block">{{ m.rsc[id].title }} 
				<span>
					{{ m.rsc[id].date_start|date:"d/m/Y H:i" }}
					{% ifnotequal m.rsc[id].date_start|date:"d/m/Y H:i" m.rsc[id].date_end|date:"d/m/Y H:i" %}
					 &mdash; {{ m.rsc[id].date_end|date:"d/m/Y H:i" }}
					{% endifnotequal %}
				</span>
			</h3>
			<div class="block clearfix">
				{% image m.rsc[id].media[1].filename width=180 height=140 crop alt=m.rsc[rsc_id].title class="left" %}
				<div class="zp-70">
					{% if m.rsc[id].intro %}<p class="intro">{{ m.rsc[id].intro }}</p>{% endif %}
					{{ m.rsc[id].body }}
				</div>
			</div>
			{% empty %}
			<div class="block clearfix">
				<p>Helaas zijn er in de komende periode geen evenementen.  Kom geregeld terug om te kijken of er iets nieuws is.</p>
			</div>
			{% endfor %}

			{% pager result=result dispatch="agenda" %}
		{% endwith %}
	</div>
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">
			{% include "_subnav.tpl" %}
		</div>
	</div>
{% endblock %}