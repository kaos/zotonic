{% extends "page.tpl" %}

{% block content %}
	<div id="content-wrapper" class="clearfix">
		<div id="content" class="zp-65">
			<div class="padding">
				<h1>{{ m.rsc[id].title }}</h1>

				{% if m.rsc[id].website %}
					<p class="website"><a href="{{ m.rsc[id].website }}" title="{{ m.rsc[id].title }}">Visit website</a></p>
				{% endif %}

				{% if m.rsc[id].o.about %}
					<p class="about">About:
					{% for id in m.rsc[id].o.about %}
						<a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">{{ m.rsc[id].title }}</a>{% if not forloop.last %}, {% endif %} 
					{% endfor %}
					</p>
				{% endif %}

				{% if m.rsc[id].summary %}
					<p class="intro">{{ m.rsc[id].summary }}</p>
				{% endif %}
				
				{% with m.rsc[id].medium as medium %}
					{% if medium.filename %}
						<p>Download <a href="{% url media_attachment star=medium.filename %}">{{ medium.filename }}</a> ({{ medium.mime }})</p>
					{% endif %}
				
					<p><a href="{% url media_inline star=medium.filename %}" title="View {{ medium.filename }}">{% image id width=590 %}</a></p>
				{% endwith %}

				{% if m.rsc[id].body %}
					{{ m.rsc[id].body }}
				{% endif %}
			</div>
		</div>

		<div id="sidebar" class="zp-30">
			<div class="padding">
				{% include "_sidebar_latest_news.tpl" %}
			</div>
			<p class="newletter-link"><a href="mailto:info@newislandfestival.com?subject=sign me up for mailing list of New Island Festival September 10-20, 2009" title="sign up for our mailinglist">Subscribe to our newsletter</a></p>
		</div>
	</div>
{% endblock %}