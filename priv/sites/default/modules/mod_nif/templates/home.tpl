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

{% block search %}
	<a href="http://www.ny400.org" title="NY400"><img class="left" alt="" src="/lib/images/ny400.jpg" /></a>
{% endblock %}

{% block content %}
	<div id="home-background">
		<div id="home-content">
			<p>{{ m.rsc.home.summary }}</p>
			<p class="button-wrapper clearfix">
				<a class="button" href=" http://www.newislandfestival.com/page/564/tickets" title="For $35 All-Access Festival Passport Click Here">For $35 All-Access Festival Passport Click Here</a>
				<a class="button clear" href="http://www.newislandfestival.com/event/1288/armin-van-buuren" title="Armin van Buuren FREE Sept. 12">Armin van Buuren FREE Sept. 12</a>
			</p>
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
