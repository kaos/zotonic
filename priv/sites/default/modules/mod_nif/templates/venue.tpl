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
				<h1>Artists playing here</h1>
	
				<ul class="items-list">
					{% for id in m.rsc[id].s.atvenue %}
					<li class="clearfix">
						<h3>
							<a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">
								{{ m.rsc[id].title }}
							</a>
						</h3>
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
							<em>{{ [m.rsc[subject_id].date_start, m.rsc[subject_id].date_end]|date_range:["N d, f A", " &mdash; ", "f A"] }}</em>
							{{ m.rsc[id].summary|ljust:80 }}&hellip;
							<a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">Read&nbsp;more</a>
						</p>
					</li>

					{% empty %}
					<li>
						No artists found.
					</li>
					{% endfor %}
				</ul>
			</div>
			<p class="newletter-link"><a href="mailto:info@newislandfestival.com?subject=sign me up for mailing list of New Island Festival September 10-20, 2009" title="sign up for our mailinglist">Subscribe to our newsletter</a></p>
		</div>
	</div>
{% endblock %}