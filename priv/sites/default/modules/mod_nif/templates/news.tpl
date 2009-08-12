{% extends "base.tpl" %}

{% block title %}
	{{ m.rsc.news.title }}
{% endblock %}

{% block pageclass %}
	{{ m.rsc.news.slug }}
{% endblock %}

{% block pageheader %}
	<h1 id="header">New Island Festival. Created by Dutch artists. Governors Island september 10-20</h1>
{% endblock %}

{% block content %}
	
	<div id="content-wrapper" class="clearfix">
		<div id="content">
			<div class="padding">
				{% include "_view.tpl" %}
	
				<ul class="news-list clearfix">
					{% for id in m.search[{latest cat="news"}] %}
					<li class="clearfix">
						<h2><a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">{{ m.rsc[id].title }}</a></h2>
						{% if m.rsc[id].media %}
							<div class="item-image left">{% image  m.rsc[id].media[1] width=180 height=90 crop %}</div>
						{% endif %}
						
						<p class="intro">
							<em>{{ m.rsc[id].modified|date:"N d" }}</em> &mdash; {{ m.rsc[id].summary }}
						</p>
					</li>
					{% empty %}
					<li>
						No news to show.
					</li>
					{% endfor %}
				</ul>
							
			</div>
		</div>
	</div>
{% endblock %}

{% block sidebar %}{% endblock %}