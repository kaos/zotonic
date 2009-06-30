{% for id, rank in result %}
	<li class="suggestions-result">
		<a id="{{ #connect.id }}" href="#add-connection">{{ m.rsc[id].title }} (in <span>{{ m.rsc[id].category.title }})</span></a>
	</li>

	{% wire id=#connect.id action={link subject_id=subject_id predicate=predicate object_id=id action=action} action={dialog_close} %}

{% empty %}
	<li class="suggestions-result"><a href="javascript:void(0);">Nothing found.</a></li>
{% endfor %}
