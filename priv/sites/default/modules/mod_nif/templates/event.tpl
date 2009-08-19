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
				
				<div class="clearfix">
					{#<span class="artist zp-30"><a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">{{ m.rsc[id].title }}</a></span>#}
					
					{% if m.rsc[id].date_start %}
						{% with m.rsc[id].date_start as date_start %}
							{% with m.rsc[id].date_end as date_end %}
								<span class="time-wrapper zp-70">
									From 
									<span class="time">{{ date_start|date:"f A" }}</span>

									{% ifnotequal date_start date_end %}
										&mdash; <span class="time">{{ date_end|date:"f A" }}</span>
									{% endifnotequal %}

									<span class="day">on {{ date_start|date:"l" }}</span>
									<span class="date">{{ date_start|date:"N d" }}.</span>
									{% if m.rsc[id].date_remarks %}
										<span class="date-remarks">{{ m.rsc[id].date_remarks }}</span>
									{% endif %}
								</span>
							{% endwith %}
						{% endwith %}
					{% endif %}
				</div>
				<ul class="performance-genres">
					<li>Genres:</li>
					{% for genre_id in m.rsc[id].o.hasgenre %}
						<li>{{ m.rsc[genre_id].title }}{% if not forloop.last %},{% endif %}</li>
					{% empty %}
						<li>no genre</li>
					{% endfor %}
				</ul>
				
				{% if m.rsc[id].media[1] %}
					<p class="inline-image">
						{% media m.rsc[id].media[1] width=288 height=288 crop alt=m.rsc[m.rsc[id].media[1]].title %}
						{% if m.rsc[id].media[1].summary %}
							<span class="inline-image-caption">{{ m.rsc[id].media[1].summary }}</span>
						{% endif %}
					</p>
				{% endif %}
				
				{% if m.rsc[id].summary %}
					<p class="intro">{{ m.rsc[id].summary }}</p>
				{% endif %}
				
				{% if m.rsc[id].body %}
					{{ m.rsc[id].body }}
				{% endif %}

				{% if m.rsc[id].media %}
					{% for ids in m.rsc[id].media|tail|split_in:2 %}
						<div class="zp-50">
							{% for id in ids %}
								<p class="inline-image inline-event-image">
									{% if m.rsc[id].website %}
										<a href="{{ m.rsc[id].website }}" title="Go to {{m.rsc[id].title}}">{% media id width=288 alt=m.rsc[id].title %}</a>
									{% else %}
										{% media id width=288 alt=m.rsc[id].title %}
									{% endif %}
									{% if m.rsc[id].summary %}
										<span class="inline-image-caption">{{ m.rsc[id].summary }}</span>
									{% endif %}
								</p>
							{% endfor %}
						</div>
					{% endfor %}
				{% endif %}
			</div>
		</div>

		<div id="sidebar" class="zp-30">
			<div class="padding">
				<h4 class="sidebar-title">Artist:</h4>

				{% for performer_id in m.rsc[id].o.performer %}
					<h1><a href="{{ m.rsc[performer_id].page_url }}">{{ m.rsc[performer_id].title }}</a></h1>
					<p class="clearfix">
						{% with m.rsc[performer_id].depiction as depiction %}
							{% if depiction %}
								<a href="{{ m.rsc[performer_id].page_url }}" title="{{ m.rsc[performer_id].title }}">
									{% image depiction width=65 height=65 crop alt=""  %}
								</a>		
							{% endif %}
						{% endwith %}
						
						{{ m.rsc[performer_id].summary }} <a href="{{ m.rsc[performer_id].page_url }}" title="{{ m.rsc[performer_id].title }}">Read more</a>
					</p>
					
					<ul class="program-list">
						{% for subject_id in m.search[{nif_artist_events id=performer_id}] %}
							{% ifnotequal subject_id id %}
								<li class="clearfix performance-info-wrapper">
									<a href="{{ m.rsc[subject_id].page_url }}" title="{{ m.rsc[subject_id].title }}">{{ m.rsc[subject_id].title }}</a><br/>
									{{ [m.rsc[subject_id].date_start, m.rsc[subject_id].date_end]|date_range:["N d, f A", " &mdash; ", "f A"] }}
								</li>
							{% endifnotequal %}
						{% endfor %}
					</ul>	
					
				{% endfor %}	
				<p class="newletter-link"><a href="mailto:info@newislandfestival.com?subject=sign me up for mailing list of New Island Festival September 10-20, 2009" title="sign up for our mailinglist">Subscribe to our newsletter</a></p>
			</div>
		</div>
	</div>
{% endblock %}