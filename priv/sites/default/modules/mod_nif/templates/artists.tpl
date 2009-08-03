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
							
				{% with m.search.paged[{latest cat="artist" pagelen=100}] as result %}

					<ul class="artists-list clearfix">

						{% for id in result %}
	
							<li class="clearfix">
								<h2><a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">{{ m.rsc[id].title }}</a></h2>
								{% if m.rsc[id].media[1] %}
								<div class="item-image left">
									<a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">
										{% image m.rsc[id].media[1] width=180 height=90 crop %}
									</a>
								</div>
								{% endif %}
							
								{% if m.rsc[id].summary %}
									<p>{{ m.rsc[id].summary }}</p>
								{% endif %}
							</li>	
						{% empty %}
							<li>
								<p>No artists have been found.</p>
							</li>
						{% endfor %}
				
					</ul>
					
					{% pager result=result dispatch="artists" qargs %}
					
				{% endwith %}
			
			</div>
		</div>
		
		<div id="sidebar" class="zp-30">
			<div class="padding">
				<h1>Latest news</h1>

				<ul class="items-list">
					{% for id in m.search[{latest cat="news" pagelen="5"}] %}
					<li class="clearfix">
						<h3><a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">{{ m.rsc[id].title|striptags }}</a></h3>
						{% if m.rsc[id].media[1] %}
							<div class="item-image left">{% image m.rsc[id].media[1] width=65 height=65 crop %}</div>
						{% endif %}

						<p class="intro">
							<em>{{ m.rsc[id].modified|date:"d M, f A" }}</em> &mdash; 
							{{ m.rsc[id].summary|ljust:80 }}&hellip;
							<a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">Read&nbsp;more</a>
						</p>
					</li>
					{% empty %}
					<li>
						No news to show.
					</li>
					{% endfor %}
				</ul>
			</div>
		</div>
	</div>
{% endblock %}

{% block sidebar %}{% endblock %}