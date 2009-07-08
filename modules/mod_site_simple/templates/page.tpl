{% extends "base.tpl" %}

{% block title %}
	{{ m.rsc[id].title }}
{% endblock %}

{% block body %}
	<h1>{{ m.rsc[id].title }}</h1>
	
	{{ m.rsc[id].body }}
{% endblock %}
