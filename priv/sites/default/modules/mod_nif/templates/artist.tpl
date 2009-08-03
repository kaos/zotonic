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
				{% include "_view.tpl" %}
			</div>
		</div>

		<div id="sidebar" class="zp-30">
			<div class="padding">
				<h4 class="sidebar-title">Performances:</h4>
	
				<ul class="items-list">
					{% for id in m.rsc[id].s.performer %}
					<li class="clearfix">
						<h2>
							<a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">
								{{ m.rsc[id].title }}
							</a>
						</h2>
						<h4>
							<a href="{{ m.rsc[id].o.atvenue.page_url }}" title="{{ m.rsc[id].o.atvenue.title }}">
								{{ m.rsc[id].o.atvenue.title }}
							</a>
						</h4>
						{% if m.rsc[id].media %}
							<div class="item-image left">
								<a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">
									{% image m.rsc[id].media[1] width=65 height=65 crop %}
								</a>
							</div>
						{% else %}
							<div class="item-image left">
								<a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">
									{% image m.rsc[id].o.performer.media[1] width=65 height=65 crop %}
								</a>
							</div>
						{% endif %}
						<p class="intro">
							<em>{{ m.rsc[id].date_start|date:"M d, f A" }} &mdash; {{ m.rsc[id].date_end|date:"M d, f A" }}</em>
							{{ m.rsc[id].summary|ljust:80 }}&hellip;
							<a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">Read&nbsp;more</a>
						</p>
					</li>
					{% empty %}
					<li>
						No performances to show.
					</li>
					{% endfor %}
				</ul>
			</div>
			<p class="newletter-link"><a href="#">Subscribe to our newsletter</a></p>
		</div>
	</div>
{% endblock %}