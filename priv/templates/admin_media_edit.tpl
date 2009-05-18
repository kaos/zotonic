{% extends "admin_base.tpl" %}

{% block title %} Media {{ m.media[id].title }} {% endblock %}

{% block tinymce %}
<script type="text/javascript" src="/lib/js/modules/tinymce/tiny_mce.js"></script>
<script type="text/javascript">
	tinyMCE.init(tinyInit);
</script>	
{% endblock %}

{% block content %}
{% with m.media[id] as r %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

		{% if not is_editable %}
			<h2>You are not allowed to edit <span>{{ r.title|striptags }}</span></h2>
		{% else %}
			<p class="admin-chapeau">editing:</p>
			<h2>{{ r.title|striptags }}</h2>
		{% endif %}	

			{% wire id="mediaform" type="submit" postback="mediaform" %}
			<form id="mediaform" method="post" action="postback">
				<div class="zp-67" id="poststuff">
					<div class="padding">
						<div class="item-wrapper">
							<h3 class="above-item">Description</h3>
							<div class="item">
								<fieldset class="admin-form">
									<input type="hidden" name="id" value="{{ id }}" />
									<div class="form-item clearfix">
										<label for="field-title">Title</label>
										<input type="text" id="field-title" name="title" value="{{ r.title }}" />
									</div>

									<div class="form-item clearfix">
										<label for="field-content">Description</label>
										<textarea rows="4" cols="10" id="field-content" name="descr" class="descr">{{ r.descr|escape }}</textarea>
									</div>
									<div class="form-item clearfix">
										{% button class="save-resource right" text="save media" title="test" %}
										{% button class="discard-resource right" text="cancel" action={redirect back} %}
									</div>
								</fieldset>
							</div>
						</div>

						<div class="item-wrapper">
							<h3 class="above-item">Preview</h3>
							<div class="item clearfix">
								<p>
									{{ r.mime}} 
									&mdash; {{ r.width }} x {{ r.height }} pixels
									&mdash; {{ r.filename }}
									&mdash; uploaded on {{ r.created|date:"Y-m-d H:i:s" }}
								</p>
								<div class="edit_media">
								{% if r.width|lt:725  %}
									{% image r.filename class="do_quickview" %}
								{% else %}
									{% image r.filename width=725 height=725 class="do_quickview" %}
								{% endif %}
								</div>
								<div>
									{% button text="download" action={redirect dispatch="media_attachment" star=r.filename} %}
								</div>
							</div>
						</div>
				
					</div>
				</div>
			
				<div class="zp-33" id="sidebar">
					<div class="padding" id="sort">
					
						{% sorter id="sort" handle="h3" axis="y" containment="" opacity="0.9" placeholder="sortable-placeholder" %}
						{% sortable id="sort-publish" %}
						{% sortable id="sort-name" %}
						{% sortable id="sort-access" %}
						{% sortable id="sort-referrers" %}
					
						<div class="item-wrapper" id="sort-publish">
							<h3 class="above-item clearfix do_blockminifier">
								<span class="title">Save this media</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								<div class="admin-form clearfix">
									<div class="form-item clearfix">
										{% button class="save-resource do_tooltip" text="save" title="Save descriptions and name." %}
									
										{% button class="discard-resource right" text="cancel" action={redirect back} %}
										{% button class="discard-resource right" text="delete" action={dialog_delete_media id=r.id on_success={redirect back}} %}
									</div>
								</div>
							</div>
						</div>

						<div class="item-wrapper" id="sort-access">
							<h3 class="above-item clearfix do_blockminifier">
								<span class="title">Access control</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								<div class="admin-form clearfix">
									<div class="form-item zp-50">
										<label for="visible_for">Visible for</label>
										<select id="visible_for" name="visible_for">
											<option value="0" 
												{% ifequal 0 r.visible_for %}selected="selected"
												{% else %}{% if not m.acl.is_public_publisher %}disabled="disabled"{% endif %}
												{% endifequal %}>The whole world</option>
											<option value="1"
												{% ifequal 1 r.visible_for %}selected="selected"
												{% else %}{% if not m.acl.is_community_publisher %}disabled="disabled"{% endif %}
												{% endifequal %}>Community members</option>
											<option value="2" {% ifequal 2 r.visible_for %}selected="selected"{% endifequal %}>Group members</option>
										</select>
									</div>
									
									<div class="form-item  zp-50">
										<label for="group_id">Belongs to the group</label>
										<select id="group_id" name="group_id">
											<option value="{{ r.group_id }}">{{ m.group[r.group_id].title }}</option>
										{% for group_id in m.group.member %}
											{% ifnotequal r.group_id group_id %}
											<option value="{{ group_id }}">{{ m.group[group_id].title }}</option>
											{% endifnotequal %}
										{% endfor %}
										</select>
									</div>
								</div>
							</div>
						</div>

						{% if m.acl.is_admin %}
						<div class="item-wrapper" id="sort-name">
							<h3 class="above-item clearfix do_blockminifier">
								<span class="title">Unique name</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								<div class="form-item clearfix">
									<label for="field-name">Name</label>
									<input style="width: 60%" type="text" id="field-name" name="name" value="{{ r.name }}" />
								</div>
							</div>
						</div>
						{% endif %}

						<div class="item-wrapper" id="sort-referrers">
							<h3 class="above-item clearfix do_blockminifier">
								<span class="title">In use by the pages</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								{% if referrers|length %}
									<p>
										There are {{ referrers|length }} pages using this media. 
										Only showing pages you are allowed to see.
									</p>
								
									<ul>
									{% for p in referrers %}
										{% if m.rsc[p].is_visible %}
											<li><a href="{% url admin_edit_rsc id=p %}">{{ m.rsc[p].title }}</a></li>
										{% endif %}
									{% endfor %}
									</ul>
								
								{% else %}
									<p>This media is not in use by any page.</p>
								{% endif %}
							</div>
						</div>
					
					</div>
				</div>
			</form>
		</div>
		<div class="push"></div>
{% endwith %}
{% endblock %}