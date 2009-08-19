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
				
				<ul class="program-list">
					{% with m.search.paged[{fulltext cat="event" cat="text" cat="artist" cat="venue" text=q.qs page=q.page}] as result %}
				
					{% for id, rank in result %}
						<li class="clearfix performance-info-wrapper {% cycle 'even' 'uneven' %}">
							<span class="media zp-20"><a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">{% image m.rsc[id].media[1] width=80 height=18 crop %}</a></span>
							<span class="artist zp-30"><a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">{{ m.rsc[id].title }}</a></span>
						
							{% if m.rsc[id].date_start %}
								{% with m.rsc[id].date_start as date_start %}
									{% with m.rsc[id].date_end as date_end %}
										<span class="time-wrapper zp-50">
											<span class="time">{{ date_start|date:"f A" }}</span>

											{% ifnotequal date_start date_end %}
												&mdash; <span class="time">{{ date_end|date:"f A" }}</span>
											{% endifnotequal %}
											
											<span class="day">on {{ date_start|date:"l" }}</span>
											<span class="date">{{ date_start|date:"N d" }}.</span>
										</span>
									{% endwith %}
								{% endwith %}
								
							{% endif %}
						</li>
					{% empty %}
					<li>We could not find what you where looking for.</li>
					{% endfor %}
				</ul>
					
				{% pager result=result dispatch="search" qargs %}
				
				{% endwith %}
			</div>
		</div>

		<div id="sidebar" class="zp-30">
			<div class="padding">
				{% include "_sidebar_latest_news.tpl" %}
				<p class="newletter-link"><a href="mailto:info@newislandfestival.com?subject=sign me up for mailing list of New Island Festival September 10-20, 2009" title="sign up for our mailinglist">Subscribe to our newsletter</a></p>
			</div>
		</div>
	</div>
{% endblock %}