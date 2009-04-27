{# Contents of the "link_add" dialog #}
<p>
	Type the title of the page you want to connect to.  Click “Create new” when the page does not yet exist.
</p>
<input id="{{#input}}" class="do_autocomplete left" type="text" value="" />

{% button text="Create new" action={growl text="create new"} %}

<div class="clear">
	<ul id="{{#suggestions}}">
		<li></li>
	</ul>
</div>

{% wire id=#input type="keyup" action={typeselect target=#suggestions subject_id=subject_id predicate=predicate} %}
