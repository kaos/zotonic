{% extends "base.tpl" %}

{% block title %}
	{{ m.rsc[id].title }}
{% endblock %}

{% block pageclass %}
	{{ m.rsc[id].slug }}
{% endblock %}

{% block pageheader %}
	<h1 id="header">New Island Festival. Created by Dutch artists. Governors Island september 10-20</h1>
{% endblock %}

{% block content %}
	<div id="content-wrapper" class="clearfix">
		<div id="content" class="zp-65">
			<div class="padding">
				<h1>{{ m.rsc[id].title }}</h1>

				{% if m.rsc[id].summary %}
					<p class="intro">{{ m.rsc[id].summary }}</p>
				{% endif %}

				{% if m.rsc[id].body %}
					{{ m.rsc[id].body }}
				{% endif %}

				{% if m.rsc[id].media %}
					{% for media_id in m.rsc[id].media %}
						<p class="block-image clear">
							{% media media_id width=580 alt=m.rsc[media_id].title %}
							{% if media_id.summary %}
								<span class="block-image-caption">{{ media_id.summary }}</span>
							{% endif %}
						</p>
					{% endfor %}
				{% endif %}
			</div>
		</div>

		<div id="sidebar" class="zp-30">
			<div class="padding">
				<h1>Latest news</h1>
	
				<ul class="items-list">
					{% for id in m.search[{latest cat="news" pagelen="5"}] %}
					<li class="clearfix">
						<h3><a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">{{ m.rsc[id].title|striptags }}</a></h3>
						{% if m.rsc[id].media[1] %}
							<div class="item-image left">{% image m.rsc[id].media[1] width=65 height=65 crop %}</div>
						{% endif %}
						
						<p class="intro">
							<em>{{ m.rsc[id].modified|date:"d M, f A" }}</em> &mdash; 
							{{ m.rsc[id].summary|ljust:80 }}&hellip;
							<a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">Read&nbsp;more</a>
						</p>
					</li>
					{% empty %}
					<li>
						No news to show.
					</li>
					{% endfor %}
				</ul>
			</div>
			<p class="newletter-link"><a href="mailto:info@newislandfestival.com?subject=sign me up for mailing list of New Island Festival September 10-20, 2009" title="sign up for our mailinglist">Subscribe to our newsletter</a></p>
		</div>
	</div>
{% endblock %}