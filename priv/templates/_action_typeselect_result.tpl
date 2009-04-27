{% for id, rank in result %}
	<li><a style="color:white" id="{{ #connect.id }}" href="#add-connection">{{ m.rsc[id].title }} (in <span>{{ m.rsc[id].category.title }})</span></a></li>

	{% wire id=#connect.id action={link subject_id=subject_id predicate=predicate object_id=id} action={dialog_close} %}

{% empty %}
	<li>Nothing found.</li>
{% endfor %}
