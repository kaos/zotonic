{# Contents of the "link_add" dialog #}
<p>Type the title of the page you want to connect to.  Click “Create new” when the page does not yet exist.</p>

{% button text="Make a new connection" class="left" action={dialog_close} action={dialog_new_rsc redirect=false subject_id=subject_id predicate=predicate} %}
<p>or use the autocompleter to search in the systsem.</p>
<div class="form-item autocomplete-wrapper clear">
	<input id="{{#input}}" class="autocompleter" type="text" value="" />
	<ul id="{{#suggestions}}" class="suggestions-list"></ul>
</div>

{% wire id=#input type="keyup" action={typeselect target=#suggestions subject_id=subject_id predicate=predicate} %}