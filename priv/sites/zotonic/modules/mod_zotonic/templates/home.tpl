{% extends "base.tpl" %}

{% block title %}Zotonic &ndash; Pragmatic innovation. Stuff that works{% endblock %}

{% block content %}
	{% with m.rsc.home as r %}
		<h1>{{ r.title }}</h1>
		
		<p class="summary">
			{{ r.summary }}
		</p>
		
		{{ r.body }}
	{% endwith %}
{% endblock %}


{% block sidebar %}
	{% for id in m.search[{latest}] %}
		<h1><a href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a></h1>
		<p>{{ m.rsc[id].summary }} <a href="{{ m.rsc[id].page_url }}">Read more...</a></p>
	{% endfor %}
{% endblock %}
