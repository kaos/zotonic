{% extends "base.tpl" %}

{% block title %}{{ m.rsc[id].seo_title | default: m.rsc[id].title }}{% endblock %}

{% block page_class %}page{% endblock %}

{% block banner %}{% endblock %}

{% block content %}

	<article id="content" class="zp-67">
		<div class="padding">
			{% block breadcrumb %}{% endblock %}
			<h1>{{ m.rsc[id].title }}</h1>

			{% if m.rsc[id].website %}
			<p class="website"><a href="{{ m.rsc[id].website }}">{{ m.rsc[id].website }}</a></p>
			{% endif %}

			<p class="summary">{{ m.rsc[id].summary }}</p>
			{{ m.rsc[id].body|show_media }}
		</div>
	</article>

{% endblock %}

{% block sidebar %}
	
	<aside id="sidebar" class="zp-33">
        {% include "_keywords.tpl" %}

		{% with m.rsc[id].s.collection_member as collections %}
			{% if collections %}
				{% for c_id in collections %}
					<h2><a href="{{ m.rsc[c_id].page_url }}">{{ m.rsc[c_id].title }}</a></h2>
					
					<ul>
					{% for p_id in m.rsc[c_id].o.collection_member %}
						<li>
							{% ifnotequal p_id id %}
								<h3><a href="{{ m.rsc[p_id].page_url }}">{{ m.rsc[p_id].title }}</a></h3>
							{% else %}
								<h3>{{ m.rsc[p_id].title }}</h3>
							{% endifnotequal %}
							<p class="summary">{{ m.rsc[p_id].summary }}</p>
						</li>
					{% endfor %}
					</ul>
				{% endfor %}
			{% else %}
			{% endif %}
		{% endwith %}
{#
		<h1>Zotonic is Open Source</h1>
		<p>Zotonic is released under the Open Source <a href="#">Apache2 license</a>, which gives you the possibility to use it and modify it in every circumstance.</p>
		<p><a href="#">Read more about Zotonic &raquo;</a></p>
#}

	</aside>

{% endblock %}
