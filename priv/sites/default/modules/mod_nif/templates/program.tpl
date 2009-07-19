{% extends "base.tpl" %}

{% block title %}
	{{ m.rsc.program.title }}
{% endblock %}

{% block pageclass %}
	{{ m.rsc.program.slug }}
{% endblock %}

{% block pageheader %}
	<h1 id="header">New Island Festival. Created by Dutch artists. Governors Island september 10-20</h1>
{% endblock %}	

{% block navigation %}
	{% menu id=m.rsc.program.id %}	
{% endblock %}

{% block content %}
	
	<div id="content-wrapper" class="clearfix">
		<div id="content">
			<div class="padding">
				<h1>{{ m.rsc.program.title }}</h1>
				<p class="intro">{{ m.rsc.program.summary }}</p>
							
				{% with m.search[{latest cat="event"}] as result %}

					<ul class="program-list clearfix">

					{% for id in result %}

						<li class="clearfix">
							<h2><a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">{{ m.rsc[id].title }}</a> <span class="genres"><a href="#">Cabaret</a>, <a href="#">for kids</a>, <a href="#">english</a></span></h2>
							<div class="item-image left">
								<a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">
									{% image  m.rsc[id].media[1] width=180 height=90 crop %}
								</a>
							</div>

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

							<p class="event-info">By <a href="#">Michael jackson</a> &mdash; <a href="#">Governors Island vanue</a>.</p>						
							<p>{{ m.rsc[id].summary|ljust:140 }}&hellip; <a href="#">Read more</a></p>
						</li>	
					{% empty %}
						<li>
							<p>Helaas zijn er in de komende periode geen evenementen.</p>
						</li>
					{% endfor %}
				
					</ul>
					
				{% endwith %}
			
			</div>
		</div>
	</div>
{% endblock %}

{% block sidebar %}{% endblock %}