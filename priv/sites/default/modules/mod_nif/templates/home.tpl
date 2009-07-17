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

{% block navigation %}{% endblock %}

{% block content %}
	<div id="home-background">
		<div id="home-content">
			<p>Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.</p>

			<div id="media-viewer" class="do_cycle {speed: 1500, timeout: 2500}">
				{% if m.rsc.media_viewer.media %}
				
					{% for media_id in m.rsc.media_viewer.media %}
						{% image media_id width=581 height=292 crop %}
					{% endfor %}
					
				{% else %}
					
					<p>No media collection found</p>
				
				{% endif %}
			</div>
			<p> Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>
		</div>
	</div>

{% endblock %}
