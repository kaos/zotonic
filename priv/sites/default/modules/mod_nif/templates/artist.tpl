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
					{% for ids in m.search[{nif_artist_events id=id pagelen=200}]|group_by:"title" %}
						<li class="clearfix">
							<h2>
								<a href="{{ m.rsc[ids[1]].page_url }}" title="{{ m.rsc[ids[1]].title }}">
									{{ m.rsc[ids[1]].title }}
								</a>
							</h2>
							<h4>
								{{ m.rsc[ids[1]].o.atvenue.title }}
							</h4>
							{% if m.rsc[ids[1]].media %}
								<div class="item-image left">
									<a href="{{ m.rsc[ids[1]].page_url }}" title="{{ m.rsc[ids[1]].title }}">
										{% image m.rsc[ids[1]].media[1] width=65 height=65 crop %}
									</a>
								</div>
							{% else %}
								<div class="item-image left">
									<a href="{{ m.rsc[ids[1]].page_url }}" title="{{ m.rsc[ids[1]].title }}">
										{% image m.rsc[ids[1]].o.performer.media[1] width=65 height=65 crop %}
									</a>
								</div>
							{% endif %}
							<p class="intro">
								{{ m.rsc[ids[1]].summary|ljust:80 }}&hellip;
								<a href="{{ m.rsc[ids[1]].page_url }}" title="{{ m.rsc[ids[1]].title }}">Read&nbsp;more</a>
							</p>
							<p class="clear">
							{% for id in ids %}
								<a href="{{ m.rsc[id].page_url }}">
									{{ [m.rsc[id].date_start, m.rsc[id].date_end]|date_range:["N d, f A", " &mdash; ", "f A"] }}
								</a> {% if not forloop.last %}<br/>{% endif %}
							{% endfor %}
							</p>
						</li>
					{% empty %}
					<li>
						No performances to show.
					</li>
					{% endfor %}
				</ul>
				<p class="newletter-link"><a href="mailto:info@newislandfestival.com?subject=sign me up for mailing list of New Island Festival September 10-20, 2009" title="sign up for our mailinglist">Subscribe to our newsletter</a></p>
			</div>
		</div>
	</div>
{% endblock %}