{% extends "admin_base.tpl" %}

{% block title %} admin edit resource {% endblock %}

{% block tinymce %}
<script type="text/javascript" src="/lib/js/modules/tinymce/tiny_mce.js"></script>
<script type="text/javascript">
	tinyMCE.init(tinyInit);
</script>	
{% endblock %}

{% block content %}
{% with m.rsc[id] as r %}
  {% with r.is_editable as is_editable %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

			{% if not is_editable %}
				<h2>You are not allowed to edit the {{ m.rsc[r.category_id].title|lower }} “{{ r.title|striptags }}”</h2>
			{% else %}
				<p class="admin-chapeau">editing:</p>
				<h2>{{ r.title|striptags|default:"<em>untitled</em>" }} <span>{{ m.rsc[r.category_id].title|lower }} <a href="#category">change</a></span></h2>
			{% endif %}	

			{% wire id="rscform" type="submit" postback="rscform" %}
			<form id="rscform" method="post" action="postback">
				
				<div class="zp-67" id="poststuff">
					<div class="padding">

						<div class="item-wrapper">
							<h3 class="above-item">Basic content</h3>
							<div class="item">
								<fieldset class="admin-form">
									<input type="hidden" name="id" value="{{ id }}" />
									<div class="form-item clearfix">
										<label for="field-title">Title</label>
										<input type="text" id="field-title" name="title" value="{{ r.title }}" />
									</div>

									{% if m.acl.is_admin %}
										<div class="form-item clearfix">
											<label for="field-name">Unique name</label>
											<input type="text" id="field-name" name="name" value="{{ r.name }}" />
										</div>

										{% if r.is_a.meta or not r.is_authoritative %}
											<div class="form-item clearfix">
												<label for="field-name">Unique uri</label>
												<input type="text" id="field-name" name="uri" value="{{ r.uri }}" />
											</div>
										{% endif %}
									{% endif %}

									<div class="form-item clearfix">
										<label for="field-summary">Summary</label>
										<textarea rows="2" cols="10" id="field-summary" name="summary" class="intro">{{ r.summary }}</textarea>
									</div>

									{# {% include "_admin_save_buttons.tpl" %} #}
								</fieldset>
							</div>
						</div>

						{% all include "_admin_edit_content.tpl" %}

						{% if r.is_a.media %}
						<div class="item-wrapper">
							<h3 class="above-item">File/ media content</h3>
							<div class="item clearfix">
								{% with r.medium as medium %}
									<div id="media-edit-view">
										{% include "_admin_edit_media_view.tpl" id=id %}
									</div>
									
									{% button text="Replace this media item" action={dialog_media_upload id=id action={update update="media-edit-view" template="_admin_edit_media_view.tpl" id=id}} %}
								{% endwith %}
							</div>
						</div>
						{% endif %}

						<div class="item-wrapper">
							<h3 class="above-item">Body text</h3>
							<div class="item">
								<fieldset class="admin-form">
									<div class="form-item clearfix">
										<label for="field-content">Body</label>
										<textarea rows="10" cols="10" id="field-content" name="body" class="body">{{ r.body|escape }}</textarea>
									</div>

									{% include "_admin_save_buttons.tpl" %}
								</fieldset>
							</div>
						</div>

						<div class="item-wrapper">
							<h3 class="above-item">Attached media</h3>
							<div class="item clearfix">
								<div id="{{ #media }}">
									{% include "_edit_media.tpl" media=media %}
								</div>
								<div class="clear">
									{% if is_editable %}
										{% button
												text="add a new media item" 
												action={dialog_media_upload subject_id=id group_id=r.group_id stay
													action={postback postback={reload_media rsc_id=id div_id=#media} delegate="resource_admin_edit"}}
										%}

										{% button text="add existing media item" 
											action={link_dialog subject_id=id predicate="depiction"
												action={postback
															postback={reload_media rsc_id=id div_id=#media}
															delegate="resource_admin_edit"}
											} %}
									{% else %}
										&nbsp;
									{% endif %}
								</div>
							</div>
						</div>
					
						<div class="item-wrapper">
							<h3 class="above-item">Seo content</h3>
							<div class="item clearfix">
								<fieldset class="admin-form">
									<div class="form-item clearfix">
										<input id="no-google" type="checkbox" class="do_fieldreplace" name="seo_noindex" {% if r.seo_noindex %}checked="checked"{% endif %} value="1" />
										<label for="no-google">Ask google to not index this page</label>
									</div>

									<div class="form-item clearfix">
										<label for="seo_title">Page title</label>
										<input type="text" id="seo_title" name="seo_title" class="zp-100" value="{{ r.seo_title }}"/>
									</div>

									<div class="form-item clearfix">
										<label for="title">Page slug</label>
										<input type="text" id="slug" name="slug" class="zp-100" value="{{ r.slug }}"/>
									</div>

									<div class="form-item clearfix">
										<label for="seo_keywords">Page keywords</label>
										<input type="text" id="seo_keywords" name="seo_keywords" class="zp-100" value="{{ r.seo_keywords }}"/>
									</div>

									<div class="form-item clearfix">
										<label for="seo_desc">Page description</label>
										<textarea rows="2" cols="10" id="seo_desc" name="seo_desc" class="seo-desc zp-100">{{ r.seo_desc }}</textarea>
									</div>

									{% include "_admin_save_buttons.tpl" %}
								</fieldset>
							</div>
						</div>
					
					</div>
				</div>
			
				<div class="zp-33" id="sidebar">
					<div class="padding" id="sort">
					
						<div class="item-wrapper" id="sort-publish">
							<h3 class="above-item clearfix do_blockminifier">
								<span class="title">Publish this page</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								<div class="admin-form ">
									<div class="form-item clearfix">
										{% button type="submit" id="save_stay" class="save-resource do_tooltip" text="save" title="Save this page." disabled=is_editable|not %}
										{% if is_editable %}
											{% button type="submit" id="save_view" class="save-resource do_tooltip" text="save &amp; view" title="Save and view the page." %}
										{% else %}
											{% button id="save_view" class="save-resource do_tooltip" text="view" title="View this page." action={redirect id=id} %}
										{% endif %}
										
										{% button class="discard-resource right do_tooltip" text="cancel" action={redirect back} title="Go back." %}
										{% button class="discard-resource right do_tooltip" disabled=r.is_protected|ornot:is_editable id="delete-button" text="delete" action={dialog_delete_rsc id=r.id on_success={redirect back}} title="Delete this page." %}
									</div>
									
									<div class="form-item clearfix">
										<input type="checkbox" class="do_fieldreplace" id="is_published" name="is_published" value="1" {% if r.is_published %}checked="checked"{% endif %}/> 
										<label for="is_published" class="left">Published</label>
										
										<input type="checkbox" class="do_fieldreplace" id="is_featured" name="is_featured" value="1" {% if r.is_featured %}checked="checked"{% endif %}/> 
										<label for="is_featured" class="left">Featured</label>

										<input type="checkbox" class="do_fieldreplace" id="is_protected" name="is_protected" value="1" {% if r.is_protected %}checked="checked"{% endif %}/> 
										<label for="is_protected" class="left">Protect from deletion</label>
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
											<option value="{{ r.group_id }}">{{ m.rsc[r.group_id].title }}</option>
										{% for group_id in m.acl.member %}
											{% ifnotequal r.group_id group_id %}
											<option value="{{ group_id }}">{{ m.rsc[group_id].title }}</option>
											{% endifnotequal %}
										{% endfor %}
										</select>
									</div>
								</div>
							</div>
						</div>

						{% all include "_admin_edit_sidebar.tpl" %}

						{% if not r.is_a.meta %}
						<div class="item-wrapper" id="sort-date">
							<h3 class="above-item clearfix do_blockminifier">
								<span class="title">Date range</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								<div class="admin-form form-item">
									<p>
										Used for events and other periods.
										<a href="javascript:void(0)" class="do_dialog {title: 'Help about dateranges.', text: 'Every page can have a date range. For example if the page is an event or description of someone\'s life.', width: '450px'}">Need more help?</a>
									</p>
									<fieldset>
										<div class="form-item">
											<label>From</label>
											{% include "_edit_date.tpl" date=r.date_start name="date_start" is_end=0 %}
										</div>
										<div class="form-item">
											<label>Till</label>
											{% include "_edit_date.tpl" date=r.date_end name="date_end" is_end=1 %}
										</div>
									</fieldset>
								</div>
							</div>
						</div>
						{% endif %}

						<div class="item-wrapper" id="sort-connections">
							<h3 class="above-item clearfix do_blockminifier">
								<span class="title">Page connections</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								<div id="unlink-undo-message">
								<p>
									This page is able to connect to others. For example you can connect it to an actor or a brand. 
									<a href="javascript:void(0)" class="do_dialog {title: 'Help about page connections.', text: 'This page is able to connect to others. For example you can connect it to an actor or a brand.', width: '450px'}">Need more help?</a>
								</p>
								</div>
								
								{% with r.predicates_edit as pred_shown %}
									{% for name, p in m.predicate %}
										{% if p.id|member:pred_shown %}
											{% ifnotequal name "depiction" %}
												<h4>{{ p.title }}</h4>
												<div class="unlink-wrapper clearfix">
													<div id="links-{{id}}-{{name}}" class="clearfix">
													{% for o_id in r.o[name] %}
														{% include "_rsc_edge.tpl" subject_id=id predicate=name object_id=o_id %}
													{% endfor %}
													</div>
													
													{% if is_editable %}
														{% link_add subject_id=id predicate=name %}
													{% endif %}
												</div>
												<hr />
											{% endifnotequal %}
										{% endif %}
									{% endfor %}
								{% endwith %}
								
								<div class="clearfix">
									<p>{% button action={redirect dispatch="admin_referrers" id=id} text="View all referrers"%}</p>
								</div>
							</div>
						</div>
					
						{# meta categories (predicate, category and group) cannot be changed #}
						{% if not r.is_a.meta %}
						<div class="item-wrapper" id="sort-category">
							<h3 class="above-item clearfix do_blockminifier">
								<a name="category"></a>
								<span class="title">Category</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix admin-form">
								<p>The category defines what the page represents. <a href="javascript:void(0)" class="do_dialog {title: 'Help about category.', text: 'Every page is categorized in exactly one category.  The category defines what the page represents. For example an event, a product or a person.  The categories are hierarchically defined. In that way you can have a vehicles category with subcategories car and bicycle.', width: '450px'}">Need more help?</a></p>
								<p>
									{% with r.category_id as r_cat %}
										<select id="category_id" name="category_id">
										{% for cat_id, level, indent, name in m.category.all_flat %}
											<option value="{{cat_id}}" {% ifequal r_cat cat_id %}selected="selected"{% endifequal %}>
												{{ indent }}{{ m.rsc[cat_id].title|default:name }}
											</option>
										{% endfor %}
										</select>
									{% endwith %}
								</p>
								
								<div class="form-item clearfix">
									<div class="form-item clearfix">
										{% button type="submit" id="save_stay" class="save-resource do_tooltip" text="save this page" title="Save this page and change category." disabled=is_editable|not %}
										{% button class="discard-resource" text="cancel" action={redirect back} %}
									</div>
								</div>
							</div>
						</div>
						{% else %}
						<div class="item-wrapper" id="sort-category">
							<h3 class="above-item clearfix do_blockminifier">
								<span class="title">Category</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix admin-form">
								<label>This page is a</label>
								<h4>{{ m.rsc[r.category_id].title }}</h4>
								<hr/>
								<p>Predicates, groups and categories can't be changed into another category.</p>
							</div>
						</div>
						{% endif %}
					
					</div>
				</div>
			</form>
		</div>
	</div>	
  {% endwith %}
{% endwith %}
{% endblock %}