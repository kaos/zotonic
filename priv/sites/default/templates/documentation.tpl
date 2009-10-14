{% extends "page.tpl" %}

{% block chapeau %}
	{% with m.rsc[id].category_id as category_id %}
		<p>
		{% for cat_id in m.category[category_id].path %}
			{% ifnotequal m.rsc[cat_id].name "text" %}
				<a href="{{ m.rsc[cat_id].page_url }}">{{ m.rsc[cat_id].title }}</a> &raquo;
			{% endifnotequal %}
		{% endfor %}
			<a href="{{ m.rsc[category_id].page_url }}">{{ m.rsc[category_id].title }}</a> &raquo;
		</p>
	{% endwith %}
{% endblock %}

{% block sidebar %}
	{% with m.rsc[id].category_id as category_id %}
		<aside id="sidebar" class="zp-33">
			<h2>{{ m.rsc[category_id].title }}</h2>
			<ul>
				{% for title,c_id in m.search[{all_bytitle cat_is=category_id}] %}
					{% ifequal c_id id %}
						<li>{{ title }}</li>
					{% else %}
						<li><a href="{{ m.rsc[id].page_url }}">{{ title }}</a></li>
					{% endifequal %}
				{% endfor %}
			</ul>
		</aside>
	{% endwith %}
{% endblock %}	