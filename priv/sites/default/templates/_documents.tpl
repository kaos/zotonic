{% with m.rsc[id].o.document as docs %}
	{% if docs %}
		<h2>Downloads</h2>
		
		<ul class="list_filters">
		{% for doc in docs %}
			<li>
				<a href="{% url media_inline star=m.rsc[doc].medium.filename %}">{% image doc width=310 alt=m.rsc[doc].title title=["Download",m.rsc[doc].title]|join:" " %}</a>
				<a href="{% url media_inline star=m.rsc[doc].medium.filename %}">{{ m.rsc[doc].title }}</a>
			</li>
		{% endfor %}
		</ul>
		
	{% endif %}
{% endwith %}