
<p>Please fill in the title and the category of the new page.  You also have to select the group you will share the page with.</p>

{% wire id=#form type="submit" postback="new_page" delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback">

	<input type="hidden" name="subject_id" value="{{ subject_id }}" />
	<input type="hidden" name="predicate" value="{{ predicate }}" />
	<input type="hidden" name="redirect" value="{{ redirect }}" />

	<div class="admin-form">

		<p>
			<label for="new_rsc_title" style="color:white">Page title</label>
			<input id="new_rsc_title" name="new_rsc_title" value="{{ title|escape }}" />
			{% validate id="new_rsc_title" type={presence} %}
		</p>
		
		<p>
			<label for="{{ #category }}" style="color:white">Category</label>
			<select id="{{ #category }}" name="category_id">
			{% for cat_id, level, indent, title in m.category.all_flat %}
				<option value="{{cat_id}}">
					{{ indent }}{{ title }}
				</option>
			{% endfor %}
			</select>
		</p>
		
		<p>
			<label for="{{ #group_id }}" style="color:white">Belongs to the group</label>
			<select id="{{ #group_id }}" name="group_id">
			{% for group_id in m.group.member %}
				<option value="{{ group_id }}">{{ m.group[group_id].title }}</option>
			{% endfor %}
			</select>
		</p>
		
		<hr/>
		
		<button type="submit">Make page</button>
		
		{% button action={dialog_close} text="Cancel" %}
	</div>
</form>

