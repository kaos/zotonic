{% extends "base.tpl" %}

{% block title %}Zotonic &ndash; Pragmatic innovation. Stuff that works{% endblock %}

{% block content %}
	{% with m.rsc.home as r %}
		<h1>{{ r.title }}</h1>
		
		<p class="intro">
			{{ r.intro }}
		</p>
		
		{{ r.body }}
	{% endwith %}
{% endblock %}


{% block sidebar %}
	{% foreach id in m.search[{latest}] %}
		<h1>{% m.rsc.id.title %}</h1>
		<p>{% m.rsc.id.intro %}</p>
	{% endforeach %}
{% endblock %}
