{% with m.config.seo.keywords.value as keywords %}
	{% with m.config.seo.description.value as description %}
		{% if id %}
	<meta name="keywords" content="{{ m.rsc[id].seo_keywords }} {{ keywords }}" />
	<meta name="description" content="{{ m.rsc[id].seo_desc }} {{ description }}" />
		{% else %}
			{% if keywords or description %}
	<meta name="keywords" content="{{ keywords }}" />
	<meta name="description" content="{{ description }}" />
			{% endif %}
		{% endif %}
	{% endwith %}
{% endwith %}
