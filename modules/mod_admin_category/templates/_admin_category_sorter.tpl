		
		<h3 class="above-list">Category overview</h3>
		<ul class="short-list">
			{% for id, depth, nbsp, name in m.category.all_flat %}
				{% droppable id=#before.id tag="b-"|append:id %}
				{% droppable id=#cat.id tag="t-"|append:id %}
				{% draggable id=#cat.id tag="t-"|append:id axis='y' clone %}

				<li id="{{ #before.id }}" class="line depth-{{ depth }}"></li>
				<li id="{{ #cat.id }}" class="depth-{{ depth }}">
					<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
						{{ m.rsc[id].title|default:name }}

						<span class="buttons">
							{% if m.acl.is_admin %}
								{% button text="delete" action={dialog_category_delete id=id on_success={slide_fade_out target=#cat.id}} %}
							{% endif %}
							{% button text="edit" action={redirect dispatch="admin_edit_rsc" id=id}%}
						</span>
					</a>
				</li>
			{% endfor %}
			{% droppable id=#last tag="end" %}
			<li id="{{ #last }}" class="line"></li>
		</ul>
