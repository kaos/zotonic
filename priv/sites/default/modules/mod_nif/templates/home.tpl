{% extends "base.tpl" %}

{% block title %}
	New Island Festival
{% endblock %}

{% block pageclass %}
	home
{% endblock %}

{% block pageheader %}
	<h1 id="home-header">New Island Festival. Created by Dutch artists. Governors Island september 10-20</h1>
{% endblock %}

{% block navigation %}
	{% menu id=m.rsc.home.id %}
{% endblock %}

{% block search %}{% endblock %}

{% block content %}
	<div id="home-background">
		<div id="home-content">
			<p>{{ m.rsc.home.summary }}</p>

			<div id="media-viewer" class="do_cycle {speed: 2500, timeout: 2500}">
				{% if m.rsc.media_viewer.media %}
				
					{% for media_id in m.rsc.media_viewer.media %}
						{% image media_id width=581 height=292 crop %}
					{% endfor %}
					
				{% else %}
					
					<p>No media collection found</p>
				
				{% endif %}
			</div>
			{{ m.rsc.home.body }}
		</div>
	</div>

{% endblock %}
