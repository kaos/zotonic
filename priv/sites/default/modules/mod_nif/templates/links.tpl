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
				
				<ul class="links-list clearfix">
					{% for id in m.rsc[id].o.collection_member %}
					<li class="clearfix">
						{% if m.rsc[id].media %}
							<a href="{{ m.rsc[id].website }}" title="{{ m.rsc[id].title }}" target="_blank">{% image m.rsc[id].media[1] width=250 height=267 %}</a>
						{% endif %}
					</li>
					{% empty %}
					<li>
						No links to show.
					</li>
					{% endfor %}
				</ul>
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