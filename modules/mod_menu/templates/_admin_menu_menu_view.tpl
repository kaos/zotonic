<ul class="short-list">
	<li id="top" class="header">
		<h3>Menu</h3>
		<p>Drag pages on top of this menu or its menu items.</p>
	</li>
	
	{% for m_id, sub in menu %}
		{% with forloop.counter as m_nr %}
			<li class="header">
				<a id="{{ #menu.m_nr }}" href="#">{{ m.rsc[m_id].title }}</a>
				{% button text="x" style="float:right" action={postback postback={delete item=[m_nr] id=#menu.m_nr}} %}
				<ul>
					{% for s_id in sub %}
						{% with forloop.counter as s_nr %}
							{% with m_nr|append:"-"|append:s_nr as m_s_nr %}

								<li id="{{ #sub.m_s_nr }}">
									<em>{{ m.rsc[s_id].title }}</em>
									{% button text="x" style="float:right" action={postback postback={delete item=[m_nr, s_nr] id=#sub.m_s_nr}} %}
								</li>

								{% droppable id=#sub.m_s_nr tag=[m_nr, s_nr] %}
								{% draggable id=#sub.m_s_nr tag=[m_nr, s_nr] %}

							{% endwith %}
						{% endwith %}
					{% endfor %}
				</ul>
			</li>
			<li id="{{ #after.m_nr }}" class="line">
				&nbsp;
			</li>

			{% droppable id=#menu.m_nr tag=[m_nr] %}
			{% draggable id=#menu.m_nr tag=[m_nr] clone axis="y" %}
			{% droppable id=#after.m_nr tag=["after", m_nr] %}

		{% endwith %}
	{% endfor %}
</ul>

{% droppable id="top" tag="top" %}
