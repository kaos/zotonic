{# Let the user edit the embed code #}
{% if r.is_a.media %}
	<div class="item-wrapper">
		<h3 class="above-item">Embed from another site</h3>
		<div class="item clearfix">
			<p>Here you can enter embed code from another web site.  This code will replace any uploaded file.</p>
			
			<fieldset class="admin-form">
			{% with r.medium as medium %}
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
			{% endwith %}
			</fieldset>
		</div>
	</div>
{% endif %}
