<h1>{{ m.rsc[id].title }}</h1>

{% if m.rsc[id].summary %}
	<p class="intro">{{ m.rsc[id].summary }}</p>
{% endif %}

{% if m.rsc[id].media[1] %}				
	<p>{% image m.rsc[id].media[1] width=200 height=118 crop %}</p>
{% endif %}

{% if m.rsc[id].body %}
	{{ m.rsc[id].body }}
{% endif %}

{% if m.rsc[id].media %}				
	{% for media_id in m.rsc[id].media %}
		{% if not media_id[1] %}
			{% image media_id width=300 height=300 crop %}
		{% endif %}
	{% endfor %}
{% endif %}