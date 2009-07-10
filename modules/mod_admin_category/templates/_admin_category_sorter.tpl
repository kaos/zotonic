		
		<style>
			.hover a, .hover {
				background: #ddf;
				color: black !important;
			}
			
			.short-list .buttons {
 				float: right;
			}
	
			.ui-draggable-dragging .buttons {
				display: none;
			}

			.short-list .line {
				height: 4px;
			}
			
			.short-list li.depth-1 { margin-left: 0px; }
			.short-list li.depth-2 { margin-left: 30px; }
			.short-list li.depth-3 { margin-left: 60px; }
			.short-list li.depth-4 { margin-left: 90px; }
			.short-list li.depth-5 { margin-left: 120px; }
			.short-list li.depth-7 { margin-left: 150px; }
			.short-list li.depth-8 { margin-left: 180px; }
			.short-list li.depth-9 { margin-left: 210px; }
			.short-list li.depth-10 { margin-left: 240px; }
			.short-list li.depth-11 { margin-left: 270px; }
		</style>
		
		<h3 class="above-list">Category overview</h3>
		<ul class="short-list">
			{% for id, depth, nbsp, name in m.category.all_flat %}
				{% if name|ne:"category" and name|ne:"predicate" and name|ne:"group" and name|ne:"meta" %}
					{% droppable id=#before.id tag="b-"|append:id %}
					{% droppable id=#cat.id tag="t-"|append:id %}
					{% draggable id=#cat.id tag="t-"|append:id axis='y' %}

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
				{% endif %}
			{% endfor %}
			{% droppable id=#last tag="end" %}
			<li id="{{ #last }}" class="line"></li>
		</ul>
