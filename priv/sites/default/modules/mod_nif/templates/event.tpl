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

{% block navigation %}
	{% menu id=id %}	
{% endblock %}

{% block content %}
	<div id="content-wrapper" class="clearfix">
		<div id="content" class="zp-65">
			<div class="padding">
				
				<h1>{{ m.rsc[id].title }}</h1>
				
				{% if m.rsc[id].summary %}
					<p class="intro">{{ m.rsc[id].summary }}</p>
				{% endif %}
				
				<div class="date-image-wrapper clearfix">
					{% if m.rsc[id].date_start %}
						{% with m.rsc[id].date_start as date_start %}
							{% with m.rsc[id].date_end as date_end %}
								<div class="date-leaf">
									<span class="day">{{ date_start|date:"d" }}</span>
									<span class="month">{{ date_start|date:"F" }}</span>
									<span class="time">{{ date_start|date:"H:i A" }}</span>
								</div>
						
								{% ifnotequal date_start date_end %}
									<div class="date-leaf">
										<span class="day">{{ date_end|date:"d" }}</span>
										<span class="month">{{ date_end|date:"F" }}</span>
										<span class="time">{{ date_end|date:"H:i A" }}</span>
									</div>
								{% endifnotequal %}
							{% endwith %}
						{% endwith %}
					{% endif %}	
							
					{% with m.rsc[id].media as media %}
						{% if media %}
							<p>{% media media[1] width=444 height=90 crop %}</p>
						{% endif %}
					{% endwith %}		
				</div>

				{{ m.rsc[id].body }}
				
				{% for media_id in m.rsc[id].media %}
					{% if not forloop.first %}
						{% media media_id width=300 height=300 crop %}
					{% endif %}
				{% endfor %}
			</div>
		</div>

		<div id="sidebar" class="zp-30">
			<div class="padding">
				{% with m.rsc[id].o.performer as id %}
					
					<h1>About {{ m.rsc[id].title }}</h1>
					<p class="clearfix">
						{% with m.rsc[id].depiction as depiction %}
							{% if depiction %}
								<a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">
									{% image depiction width=65 height=65 crop %}
								</a>		
							{% endif %}
						{% endwith %}
						
						{{ m.rsc[id].summary }} <a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">Read more</a>
					</p>

				{% endwith %}	
			</div>
		</div>
	</div>
{% endblock %}