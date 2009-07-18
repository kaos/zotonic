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
				
				<ul class="program-list clearfix">
					<li class="clearfix">
						<h2>Event whatever where they light fires <span class="genres"><a href="#">Cabaret</a>, <a href="#">for kids</a>, <a href="#">english</a></span></h2>
						<div class="item-image left">{% image 503 width=180 height=90 crop %}</div>
						<div class="date-leaf">
							<span class="day">21</span>
							<span class="month">September</span>
							<span class="time">10:53 AM</span>
						</div>

						<div class="date-leaf">
							<span class="day">22</span>
							<span class="month">September</span>
							<span class="time">15:53 AM</span>
						</div>

						<p class="event-info">By <a href="#">Michael jackson</a> &mdash; <a href="#">Governors Island vanue</a>.</p>						
						<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua&hellip; <a href="#">Read more</a></p>
					</li>
					
					<li class="clearfix">
						<h2>Event whatever where they light fires <span class="genres"><a href="#">Cabaret</a>, <a href="#">for kids</a>, <a href="#">english</a></span></h2>
						<div class="item-image left">{% image 503 width=180 height=90 crop %}</div>
						<div class="date-leaf">
							<span class="day">21</span>
							<span class="month">September</span>
							<span class="time">10:53 AM</span>
						</div>

						<div class="date-leaf">
							<span class="day">22</span>
							<span class="month">September</span>
							<span class="time">15:53 AM</span>
						</div>

						<p class="event-info">By <a href="#">Michael jackson</a> &mdash; <a href="#">Governors Island vanue</a>.</p>						
						<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua&hellip; <a href="#">Read more</a></p>
					</li>
					
					<li class="clearfix">
						<h2>Event whatever where they light fires <span class="genres"><a href="#">Cabaret</a>, <a href="#">for kids</a>, <a href="#">english</a></span></h2>
						<div class="item-image left">{% image 503 width=180 height=90 crop %}</div>
						<div class="date-leaf">
							<span class="day">21</span>
							<span class="month">September</span>
							<span class="time">10:53 AM</span>
						</div>

						<div class="date-leaf">
							<span class="day">22</span>
							<span class="month">September</span>
							<span class="time">15:53 AM</span>
						</div>

						<p class="event-info">By <a href="#">Michael jackson</a> &mdash; <a href="#">Governors Island vanue</a>.</p>						
						<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua&hellip; <a href="#">Read more</a></p>
					</li>
				</ul>
				
				{#
				{% with m.search[{latest cat="event"}] as result %}
				
					{% for id in result %}
						<h3>{{ m.rsc[id].title }}</h3>
						<p>
							{{ m.rsc[id].date_start|date:"d/m/Y H:i" }}
							{% ifnotequal m.rsc[id].date_start|date:"d/m/Y H:i" m.rsc[id].date_end|date:"d/m/Y H:i" %}
							 &mdash; {{ m.rsc[id].date_end|date:"d/m/Y H:i" }}
							{% endifnotequal %}
							</p>

					{% empty %}
					
						<p>Helaas zijn er in de komende periode geen evenementen.</p>
					
					{% endfor %}
				
				{% endwith %}
				#}
			</div>
		</div>
	</div>
{% endblock %}

{% block sidebar %}{% endblock %}