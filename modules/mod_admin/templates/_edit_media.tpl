{# Used on the resource edit page and by the medium upload event.  Show all connected media. #}
{% for media_id in m.rsc[id].media %}
	<div id="{{ #medium }}" class="edit_media left clearfix">
		{% with m.rsc[media_id].medium as medium %}
			<a href="{% url admin_edit_rsc id=media_id %}">{% image medium.filename width=200 height=200 crop class="do_quickview" %}</a>
			{# <p>{{medium.filename}} ({{ medium.width }}x{{ medium.height }})</p> #}

			{% with m.rsc[media_id].title|striptags|default:"untitled" as title %}
			<div class="rsc-edge do_unlink">
				<span class="clearfix">
					<span id="{{ #unlink }}" class="unlink-cross do_tooltip" title="Disconnect {{title}}."></span>
					<span class="unlink-item"><a href="{% url admin_edit_rsc id=media_id %}">{{ title }}</a></span>
				</span>
			</div>
			{% endwith %}

			{% wire id=#unlink action={unlink subject_id=id predicate="depiction" object_id=media_id hide=#medium} %}

		{% endwith %}
	</div>
{% endfor %}
