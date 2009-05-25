{% extends "admin_base.tpl" %}

{% block title %} Admin Media {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

			<h2>Zophrenic Media</h2>

			<p>Media encompasses all uploaded images, movies and documents. Media can be attached to pages.</p>

			<div class="clearfix">
				{% button
						text="new media item" 
						action={dialog_media_upload}
				%}
			</div>
			

			{% with m.search.paged[{media page=q.page}] as result %}

				{% pager result=result dispatch="admin_media" qargs %}
				<h3 class="above-list">Media overview</h3>

				<ul class="media-list short-list">
					<li class="headers clearfix">
						<span class="zp-10">Preview</span>
						<span class="zp-20">Title</span>
						<span class="zp-10">Type</span>
						<span class="zp-30">Filename</span>
						<span class="zp-10">Dimensions</span>
						<span class="zp-10">Uploaded</span>
						<span class="zp-10">Actions</span>
					</li>

				{% for media in result %}
					{% with media.id as id %}
						<li id="{{ #li.id }}">
							<a href="{% url admin_media_edit id=id %}" class="clearfix">
								<span class="zp-10">{% image media.filename width=80 height=60 crop %}</span>
								<span class="zp-20">{{ media.title|default:"&nbsp;" }}</span>
								<span class="zp-10">{{ media.mime|default:"&nbsp;" }}</span>
								<span class="zp-30">{{ media.filename }}</span>
								<span class="zp-10">{{ media.width }} x {{ media.height }}</span>
								<span class="zp-10">{{ media.created|date:"M d, H:i" }}</span>
								<span class="zp-10">
									{#
									{% button text="delete" action={dialog_media_delete id=id on_success={slide_fade_out target=#li.id}} %}
									#}
									{% button text="delete" 
											action={dialog_delete_media id=id on_success={slide_fade_out target=#li.id}} %}
									{% button text="edit" action={redirect dispatch="admin_media_edit" id=id} %}
								</span>
							</a>
						</li>
					{% endwith %}
				{% empty %}
					<li>
						No media found.
					</li>
				{% endfor %}

				</ul>

				{% pager result=result dispatch="admin_media" qargs %}

			{% endwith %}
		</div>
		<div class="push"></div>
	</div>
{% endblock %}