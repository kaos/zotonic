{% with m.acl.is_admin as editable %}
		
		<h3 class="above-list">Category overview</h3>
		<ul class="short-list">
			{% for id, depth, nbsp, name in m.category.all_flat_meta %}

				{% if editable %}
					{% droppable id=#before.id tag="b-"|append:id %}
					{% droppable id=#cat.id tag="t-"|append:id %}
					{% draggable id=#cat.id tag="t-"|append:id axis='y' clone %}
				{% endif %}

				<li id="{{ #before.id }}" class="line depth-{{ depth }}"></li>
				<li id="{{ #cat.id }}" class="depth-{{ depth }}">
					<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
						{{ m.rsc[id].title|default:name }}

						<span class="buttons">
							{% button text="delete" disabled=m.rsc[id].is_protected action={dialog_category_delete id=id on_success={slide_fade_out target=#cat.id}} %}
							{% button text="edit" action={redirect dispatch="admin_edit_rsc" id=id}%}
						</span>
					</a>
				</li>
			{% endfor %}

			{% if editable %}
				{% droppable id=#last tag="end" %}
			{% endif %}

			<li id="{{ #last }}" class="line"></li>
		</ul>

{% endwith %}
