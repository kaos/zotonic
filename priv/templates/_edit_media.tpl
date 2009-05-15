{# Used on the resource edit page and by the media upload event #}
{% for media in m.rsc[id].media %}
	<div class="edit_media left clearfix">
		{% image media.filename width=200 height=200 crop class="do_quickview" %}
		<p>{{media.filename}} ({{ media.width }}x{{ media.height }})</p>
	</div>
{% endfor %}
