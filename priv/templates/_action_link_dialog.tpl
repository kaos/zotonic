{# Contents of the "link_add" dialog #}
<p>Type the title of the page you want to connect to.  Click “Create new” when the page does not yet exist.</p>

<div class="form-item autocomplete-wrapper">
	<input id="{{#input}}" class="autocompleter" type="text" value="" />
	<ul id="{{#suggestions}}" class="suggestions-list"></ul>
</div>

{% button text="Create new" action={growl text="create new"} %}

{% wire id=#input type="keyup" action={typeselect target=#suggestions subject_id=subject_id predicate=predicate} %}