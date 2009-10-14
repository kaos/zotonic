<h3 class="above-list">Current menu</h3>
<ul class="short-list navigation-manager">
	<li id="top" class="header">
		Drop pages here or drop them on one of the menu items.
	</li>
	
	{% for m_id, sub in menu %}
		{% with forloop.counter as m_nr %}
			<li class="header">
				
				<a id="{{ #menu.m_nr }}" href="#" class="clearfix">
					<span class="grippy"><img src="/lib/images/grippy.png" alt="Drag me" /></span>
					<span>{{ m.rsc[m_id].title }}</span>
					{% button text="x" style="float:right" action={postback postback={delete item=[m_nr] id=#menu.m_nr}} %}
				</a>
				<ul>
					{% for s_id in sub %}
						{% with forloop.counter as s_nr %}
							{% with m_nr|append:"-"|append:s_nr as m_s_nr %}

								<li id="{{ #sub.m_s_nr }}">
									<a id="{{ #menu.m_s_nr }}" href="#" class="clearfix">
										<span class="grippy"><img src="/lib/images/grippy.png" alt="Drag me" /></span>
										<span>{{ m.rsc[s_id].short_title|default:m.rsc[s_id].title }}</span>
										{% button text="x" style="float:right" action={postback postback={delete item=[m_nr, s_nr] id=#menu.m_s_nr}} %}
									</a>
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
