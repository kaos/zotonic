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
	<div id="content" class="zp-100">
		<div class="block clearfix">

		{% if not r.is_editable %}
			<h2>You are not allowed to edit <span>{{ r.title|striptags }}</span></h2>
		{% else %}
			<p class="admin-chapeau">editing:</p>
			<h2>{{ r.title|striptags }}</h2>
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
										<input type="text" id="field-title" name="title" value="{{ r.title|escape }}" />
									</div>

									<div class="form-item clearfix">
										<label for="field-intro">Intro</label>
										<textarea rows="2" cols="10" id="field-intro" name="intro" class="intro">{{ r.intro|escape }}</textarea>
									</div>

									<div class="form-item clearfix">
										<label for="field-content">Body</label>
										<textarea rows="10" cols="10" id="field-content" name="body" class="body">{{ r.body|escape }}</textarea>
									</div>
								</fieldset>
							</div>
						</div>
					
						<div class="item-wrapper">
							<h3 class="above-item">Seo Content</h3>
							<div class="item clearfix">
								<fieldset class="admin-form">
									<div class="form-item clearfix">
										<label for="no-google">
											<input id="no-google" type="checkbox" name="seo_noindex" {% if r.seo_noindex %}checked="checked"{% endif %} value="1" />Ask google not to index this page
										</label>
									</div>
									<div class="form-item clearfix">
										<label for="title">Page title</label>
										<input type="text" id="title" name="seo_title" class="zp-100" value="{{ r.seo_title|escape }}"/>
									</div>

									<div class="form-item clearfix">
										<label for="keywords">Page keywords</label>
										<input type="text" id="keywords" name="seo_keywords" class="zp-100" value="{{ r.seo_keywords|escape }}"/>
									</div>

									<div class="form-item clearfix">
										<label for="desc">Page description</label>
										<textarea rows="2" cols="10" id="desc" name="seo_desc" class="seo-desc zp-100">{{ r.seo_desc|escape }}</textarea>
									</div>
								</fieldset>
							</div>
						</div>
					</div>
				</div>
			
				<div class="zp-33" id="sidebar">
					<div class="padding" id="sort">
					
						{% sorter id="sort" handle="h3 .title" axis="y" containment="" opacity="0.9" placeholder="sortable-placeholder" %}
						{% sortable id="sort-publish" %}
						{% sortable id="sort-category" %}
						{% sortable id="sort-connections" %}
						{% sortable id="sort-date" %}
						{% sortable id="sort-access" %}
					
						<div class="item-wrapper" id="sort-publish">
							<h3 class="above-item clearfix">
								<span class="title">Publish this page</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								<div class="admin-form ">
									<div class="form-item clearfix">
										{% button class="save-resource save" text="save" %}
										<label for="is_published" class="left">
											<input type="checkbox" id="is_published" name="is_published" value="1" {% if r.is_published %}checked="checked"{% endif %}/> Published
										</label>
										<label for="is_featured" class="left">
											<input type="checkbox" id="is_featured" name="is_featured" value="1" {% if r.is_featured %}checked="checked"{% endif %}/> Featured
										</label>
										{% button class="discard-resource right" text="cancel" action={redirect back} %}
									</div>
									<hr />
									<div class="zp-100">
										<fieldset>
											<div class="form-item">
												<label>From</label> 
												{% include "_edit_date.tpl" date=r.publication_start name="publication_start" is_end=0 %}
											</div>
											<div class="form-item">
												<label>Till</label>
												{% include "_edit_date.tpl" date=r.publication_end name="publication_end" is_end=1 %}
											</div>
										</fieldset>
									</div>
								</div>
							</div>
						</div>

						<div class="item-wrapper" id="sort-category">
							<h3 class="above-item clearfix do_blockminifier">
								<span class="title">Category</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix admin-form">
								<p>The category defines what the page represents. <a href="javascript:void(0)" class="do_dialog {title: 'Help about category.', text: 'Every page is categorized in exactly one category.  The category defines what the page represents. For example an event, a product or a person.  The categories are hierarchically defined. In that way you can have a vehicles category with subcategories car and bicycle.', width: '450px'}">Need more help?</a></p>

								<p>
									{% with r.category_id as r_cat %}
										<select id="category_id" name="category_id">
										{% for cat_id, level, indent, title in m.category.all_flat %}
											<option value="{{cat_id}}" {% ifequal r_cat cat_id %}selected="selected"{% endifequal %}>
												{{ indent }}{{ title }}
											</option>
										{% endfor %}
										</select>
									{% endwith %}
								</p>
							</div>
						</div>
					
						<div class="item-wrapper" id="sort-connections">
							<h3 class="above-item clearfix do_blockminifier">
								<span class="title">Page connections</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								<div id="unlink-message">
								<p>
									This page is able to connect to others. For example you can connect it to an actor or a brand. 
									<a href="javascript:void(0)" class="do_dialog {title: 'Help about page connections.', text: 'This page is able to connect to others. For example you can connect it to an actor or a brand.', width: '450px'}">Need more help?</a>
								</p>
								</div>
								
								{% for name, p in m.predicate %}
									<h4>{{ p.title }}</h4>
									<div class="unlink-wrapper clearfix">
										<div id="links-{{id}}-{{name}}" class="clearfix">
										{% for o_id in r.o[name] %}
											{% include "_rsc_edge.tpl" subject_id=id predicate=name object_id=o_id %}
										{% endfor %}
										</div>
										{% link_add subject_id=id predicate=name %}
									</div>
									<hr />
								{% endfor %}
							</div>
						</div>
					
						<div class="item-wrapper" id="sort-date">
							<h3 class="above-item clearfix do_blockminifier">
								<span class="title">Date Range</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								<div class="admin-form form-item">
									<p>
										This page is able to have a date range. For example if this would have been an event or description about someone's live. 
										<a href="javascript:void(0)" class="do_dialog {title: 'Help about dateranges.', text: 'This page is able to have a date range. For example if this would have been an event or description about someone\'s live.', width: '450px'}">Need more help?</a>
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
					
						<div class="item-wrapper" id="sort-access">
							<h3 class="above-item clearfix do_blockminifier">
								<span class="title">Access management</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								<button class="do_tooltip" title="I'm am the tooptip popup, I'm am the tooptip popup">Yeah, give me a tooltip</button>
							</div>
						</div>
					</div>
				</div>
			</form>
		</div>
		<div class="push"></div>
{% endwith %}
{% endblock %}