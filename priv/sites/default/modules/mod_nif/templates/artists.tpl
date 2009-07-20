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
		<div id="content">
			<div class="padding">
				{% include "_view.tpl" %}
							
				{% with m.search[{latest cat="artist"}] as result %}

					<ul class="program-list clearfix">

						{% for id in result %}
	
							<li class="clearfix">
								<h2><a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">{{ m.rsc[id].title }}</a></h2>
								<div class="item-image left">
									<a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">
										{% image m.rsc[id].media[1] width=180 height=90 crop %}
									</a>
								</div>
	
								<p>{{ m.rsc[id].summary }}</p>
							</li>	
						{% empty %}
							<li>
								<p>Helaas zijn geen artiesten gevonden.</p>
							</li>
						{% endfor %}
				
					</ul>
					
				{% endwith %}
			
			</div>
		</div>
	</div>
{% endblock %}

{% block sidebar %}{% endblock %}