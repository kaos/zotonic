
<p>Upload a file from your computer.  You have to specify a description of the file to make it easier to find and share.  You also have to specify with which group you want to share the uploaded file.</p>

{% wire id=#form type="submit" postback={media_upload actions=actions} delegate=delegate %}
<form id="{{ #form }}" method="POST" action="postback">

	<input type="hidden" name="rsc_id" value="{{ rsc_id }}" />

	<div class="new-media-wrapper">
		<p>
			<label for="new_media_title" style="color:white">Media title</label>
			<input type="text" id="new_media_title" name="new_media_title" value="{{ title|escape }}" />
			{% validate id="new_media_title" type={presence} %}
		</p>
		
		<p>
			<label for="{{ #group_id }}">Group</label>
			<select id="{{ #group_id }}" name="group_id">
			{% for id in m.group.member %}
				<option value="{{ group_id }}" {% ifequal group_id id %}selected="selected"{% endifequal %}>{{ m.group[id].title }}</option>
			{% endfor %}
			</select>
		</p>
		
		
		<p>
			<label for="{{ #file }}">Media file</label>
			<input type="file" id="{{ #file }}" name="file" />
		</p>
		
		<button type="submit">Upload file</button>

		{% button action={dialog_close} text="Cancel" %}
	</div>
</form>

