<li class="clearfix">
	<h3><a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">{{ m.rsc[id].title|striptags }}</a></h3>
	{% if m.rsc[id].media[1] %}
		<div class="item-image left">{% image m.rsc[id].media[1] width=65 height=65 crop %}</div>
	{% endif %}

	<p class="intro">
		<em>{{ m.rsc[id].modified|date:"N d" }}</em> &mdash; 
		{{ m.rsc[id].summary|ljust:80 }}&hellip;
		<a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">Read&nbsp;more</a>
	</p>
</li>
