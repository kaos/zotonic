{# Used on the resource edit page and by the media upload event #}
{% for media in m.rsc[id].media %}
	<div class="edit_media left clearfix">
		<a href="{% url admin_media_edit id=media.id %}">{% image media.filename width=200 height=200 crop class="do_quickview" %}</a>
		<p>{{media.filename}} ({{ media.width }}x{{ media.height }})</p>
	</div>
{% endfor %}
