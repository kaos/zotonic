{% if medium.video_embed_service %}
<fieldset class="admin-form">
	{% media medium %}

	<div class="form-item clearfix">
		<label for="video_embed_service">From web site</label>
		<select id="video_embed_service" name="video_embed_service">
			{% include "_video_embed_service_options.tpl" select=medium.video_embed_service %}
		</select>
	</div>
	
	<div class="form-item clearfix">
		<label for="video_embed_code">Embed code</label>
		<textarea class="intro" id="video_embed_code" name="video_embed_code">{{ medium.video_embed_code|escape }}</textarea>
	</div>

</fieldset>
{% endif %}
