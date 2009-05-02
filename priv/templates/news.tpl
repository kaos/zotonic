{% extends "base.tpl" %}

{% block title %}News overview{% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<!-- Area for the main content -->
		<h2 class="header-alone">Nieuws overzicht</h2>

		{% with m.search.paged[{latest cat="news"}] as result %}
			{% pager result=result dispatch="news" %}

			{% for id in result %}
			<h3 class="block">{{ m.rsc[id].title }} <span>{{ m.rsc[id].created|date:"d/m/Y" }}</span></h3>
			<div class="block clearfix">
				{% image m.rsc[id].media[1].filename width=180 height=140 crop alt=m.rsc[rsc_id].title class="left" %}
				<div class="zp-70">
					{% if m.rsc[id].intro %}<p class="intro">{{ m.rsc[id].intro }}</p>{% endif %}
					{{ m.rsc[id].body }}
				</div>
			</div>
			{% endfor %}

			{% pager result=result dispatch="news" %}
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