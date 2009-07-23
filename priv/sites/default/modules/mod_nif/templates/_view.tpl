<h1>{{ m.rsc[id].title }}</h1>

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
	{% for media_id in m.rsc[id].media %}
		{% if not forloop.first %}
			<p class="block-image clear">
				{% media media_id width=580 alt=m.rsc[media_id].title %}
				{% if media_id.summary %}
					<span class="block-image-caption">{{ media_id.summary }}</span>
				{% endif %}
			</p>
		{% endif %}
	{% endfor %}
{% endif %}