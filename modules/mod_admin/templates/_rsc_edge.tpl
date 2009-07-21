{# Show an object with an unlink option. Used in the admin_edit #}
{% with m.rsc[object_id].title as title %}
	{% sortable id=#unlink_wrapper tag=edge_id %}
	<ul id="{{ #unlink_wrapper }}" class="rsc-edge do_unlink">
		<span class="clearfix">
			<span id="{{ #unlink }}" class="unlink-cross do_tooltip" title="Disconnect {{title}}."></span>
			<span class="unlink-item"><a href="{% url admin_edit_rsc id=object_id %}">{{ title }}</a></span>
		</span>
	</ul>
{% endwith %}

{% wire id=#unlink action={unlink subject_id=subject_id predicate=predicate object_id=object_id hide=#unlink_wrapper} %}
