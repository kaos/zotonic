<h1>{{ m.rsc[id].title }}</h1>

{% if m.rsc[id].website %}
	<p class="website"><a href="{{ m.rsc[id].website }}" title="{{ m.rsc[id].title }}">Visit website</a></p>
{% endif %}

{% if m.rsc[id].o.about %}
	<p class="about">About:
	{% for id in m.rsc[id].o.about %}
		<a href="{{ m.rsc[id].page_url }}" title="{{ m.rsc[id].title }}">{{ m.rsc[id].title }}</a>{% if not forloop.last %}, {% endif %} 
	{% endfor %}
	</p>
{% endif %}

{% if m.rsc[id].summary %}
	<p class="intro">{{ m.rsc[id].summary }}</p>
{% endif %}

{% if m.rsc[id].media[1] %}
	<p class="inline-image">
		{% media m.rsc[id].media[1] width=288 height=288 crop alt=m.rsc[m.rsc[id].media[1]].title %}
		{% if m.rsc[id].media[1].summary %}
			<span class="inline-image-caption">{{ m.rsc[id].media[1].summary }}</span>
		{% endif %}
	</p>
{% endif %}

{% if m.rsc[id].body %}
	{{ m.rsc[id].body }}
{% endif %}

{% if m.rsc[id].media %}
	{% for ids in m.rsc[id].media|tail|split_in:2 %}
		<div class="zp-50">
			{% for id in ids %}
				<p class="inline-image inline-event-image">
					{% if m.rsc[id].website %}
						<a href="{{ m.rsc[id].website }}" title="Go to {{m.rsc[id].title}}">{% media id width=288 alt=m.rsc[id].title %}</a>
					{% else %}
						{% media id width=288 alt=m.rsc[id].title %}
					{% endif %}
					{% if m.rsc[id].summary %}
						<span class="inline-image-caption">{{ m.rsc[id].summary }}</span>
					{% endif %}
				</p>
			{% endfor %}
		</div>
	{% endfor %}
{% endif %}