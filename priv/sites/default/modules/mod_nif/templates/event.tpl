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
									<span class="date">{{ date_start|date:"d M" }}.</span>
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
					{% for media_id in m.rsc[id].media %}
						{% if not forloop.first %}
							<p class="block-image">
								{% media media_id width=580  alt=m.rsc[media_id].title %}
								{% if media_id.summary %}
									<span class="block-image-caption">{{ media_id.summary }}</span>
								{% endif %}
							</p>
						{% endif %}
					{% endfor %}
				{% endif %}
			</div>
		</div>

		<div id="sidebar" class="zp-30">
			<div class="padding">
				{% with m.rsc[id].o.performer as performer_id %}
					<h4 class="sidebar-title">Artist:</h4>
					<h1>{{ m.rsc[performer_id].title }}</h1>
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
						{% for subject_id in m.rsc[performer_id].s.performer %}
							{% ifnotequal subject_id id %}
								<li class="clearfix performance-info-wrapper">
									<span class="zp-40">
										<a href="{{ m.rsc[subject_id].page_url }}" title="{{ m.rsc[subject_id].title }}">{{ m.rsc[subject_id].title }}</a>
									</span>
									<span class="zp-60">{{ m.rsc[subject_id].date_start|date:"f A" }} &mdash; {{ m.rsc[subject_id].date_end|date:"f A" }}</span>
								</li>
							{% endifnotequal %}
						{% endfor %}
					</ul>	
					
				{% endwith %}	
			</div>
		</div>
	</div>
{% endblock %}