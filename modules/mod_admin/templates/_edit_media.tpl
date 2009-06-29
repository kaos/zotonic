{# Used on the resource edit page and by the medium upload event.  Show all connected media. #}
{% for media_id in m.rsc[id].media %}
	<div class="edit_media left clearfix">
		{% with m.rsc[media_id].medium as medium %}
			<a href="{% url admin_edit id=media_id %}">{% image medium.filename width=200 height=200 crop class="do_quickview" %}</a>
			<p>{{m.rsc[media_id].title}}<br/>{{medium.filename}} ({{ medium.width }}x{{ medium.height }})</p>
		{% endwith %}
	</div>
{% endfor %}
