{% extends "base.tpl" %}

{% block title %}{{ m.rsc[id].seo_title | default: m.rsc[id].title }}{% endblock %}

{% block page_class %}page{% endblock %}

{% block banner %}{% endblock %}

{% block content %}

	<article id="content" class="zp-67">
		<div class="padding">
				{% block chapeau %}{% endblock %}
				<h1>{{ m.rsc[id].title }}</h1>

				<p class="summary">
					{{ m.rsc[id].summary }}
				</p>

				{{ m.rsc[id].body|show_media }}
		</div>
	</article>

{% endblock %}

{% block sidebar %}
	
	<aside id="sidebar" class="zp-33">
        {% include "_keywords.tpl" %}
		<h1>Zotonic is Open Source</h1>
		<p>Zotonic is released under the Open Source <a href="#">Apache2 license</a>, which gives you the possibility to use it and modify it in every circumstance.</p>
		<p><a href="#">Read more about Zotonic &raquo;</a></p>
	</aside>


{% endblock %}
