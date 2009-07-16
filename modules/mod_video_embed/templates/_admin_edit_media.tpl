{% if medium.video_embed_service %}
<fieldset class="admin-form">
	{% media medium %}

	<p>From <strong>{{ medium.video_embed_service }}</strong></p>

</fieldset>
{% endif %}
