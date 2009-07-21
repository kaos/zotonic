{# Used on the resource edit page and by the medium upload event.  Show all connected media. #}

	<div id="{{ #unlink_message }}"></div>
	
	{% sorter id=#media tag={object_sorter predicate="depiction" id=id} %}
	<ul id="{{ #media }}">
		{% for media_id in m.rsc[id].media %}
			{% sortable id=#medium.media_id tag=media_id %}
			<li id="{{ #medium.media_id }}" class="edit_media left clearfix">
				{% with m.rsc[media_id].medium as medium %}
					<a href="{% url admin_edit_rsc id=media_id %}">{% image medium.filename width=200 height=200 crop %}</a>

					{% with m.rsc[media_id].title|striptags|default:"untitled" as title %}
					<div class="rsc-edge do_unlink">
						<span class="clearfix">
							<span id="{{ #unlink.media_id }}" class="unlink-cross do_tooltip" title="Disconnect {{title}}."></span>
							<span class="unlink-item"><a href="{% url admin_edit_rsc id=media_id %}">{{ title }}</a></span>
						</span>
					</div>
					{% endwith %}

					{% wire id=#unlink.media_id
							action={unlink 
										subject_id=id 
										predicate="depiction" 
										object_id=media_id 
										hide=#medium.media_id
										undo_message_id=#unlink_message 
										undo_action={postback postback={reload_media rsc_id=id div_id=#media} delegate="resource_admin_edit"}} 
					%}

				{% endwith %}
			</li>
		{% endfor %}
	</ul>
