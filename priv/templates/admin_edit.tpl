{% extends "admin_base.tpl" %}

{% block title %} admin edit resource {% endblock %}

{% block tinymce %}
<script type="text/javascript" src="/lib/js/modules/tinymce/tiny_mce.js"></script>
<script type="text/javascript">
	tinyMCE.init({
		mode: "exact",
		elements: "field-content",
		theme: "advanced",
		skin: "wp_theme", 
		theme_advanced_buttons1: "bold,italic,strikethrough,|,justifyleft,justifycenter,justifyright,|,bullist,numlist,|,outdent,indent,blockquote,|,undo,redo,|,link,unlink,|,formatselect",
		theme_advanced_buttons2: "",
		theme_advanced_buttons3: "",
		theme_advanced_buttons4: "",
		theme_advanced_toolbar_location: "top", 
		theme_advanced_toolbar_align: "left", 
		theme_advanced_statusbar_location: "bottom", 
		theme_advanced_resizing: "1", 
		theme_advanced_resize_horizontal: "", 
		dialog_type: "modal", 
		relative_urls: "", 
		remove_script_host: "", 
		convert_urls: "", 
		apply_source_formatting: "", 
		remove_linebreaks: "1", 
		paste_convert_middot_lists: "1", 
		paste_remove_spans: "1", 
		paste_remove_styles: "1", 
		gecko_spellcheck: "1", 
		entities: "38,amp,60,lt,62,gt", 
		accessibility_focus: "1", 
		tab_focus: ":prev,:next", 
		content_css: "/lib/js/modules/tinymce/zophrenic.css", 
		wpeditimage_disable_captions: "", 
		plugins: "safari"
	});
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
				
				<input type="hidden" name="id" value="{{ id }}" />
				
				<div class="zp-67" id="poststuff">
					<div class="padding">
						<div class="item-wrapper">
							<h3 class="above-item">Basic content</h3>
							<div class="item">
								<fieldset class="admin-form">
									<div class="form-item clearfix">
										<label for="field-title">Title</label>
										<input type="text" id="title" name="title" value="{{ r.title|escape }}" />
									</div>

									<div class="form-item clearfix">
										<label for="field-intro">Intro</label>
										<textarea rows="10" cols="10" id="field-intro" name="intro" class="intro">{{ r.intro|escape }}</textarea>
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
										<textarea rows="10" cols="10" id="desc" name="seo_desc" class="seo-desc zp-100">{{ r.seo_desc|escape }}</textarea>
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
						{% sortable id="sort-connections" %}
						{% sortable id="sort-date" %}
						{% sortable id="sort-access" %}
					
						<div class="item-wrapper" id="sort-publish">
							<h3 class="above-item clearfix">
								<span class="title">Publish this page</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								<div class="admin-form form-item">
									<div class="zp-33">
										{% button class="save-resource save" text="save" %}
										{% button class="discard-resource" text="cancel" action={redirect back} %}
									</div>
									<div class="zp-67">
										<label for="is_published">
											<input type="checkbox" id="is_published" name="is_published" value="1" {% if r.is_published %}checked="checked"{% endif %}/> Published
										</label>
										<label>From</label> 
										{% include "_edit_date.tpl" date=r.publication_start name="publication_start" is_end=0 %}
										<label>Till</label>
										{% include "_edit_date.tpl" date=r.publication_end name="publication_end" is_end=1 %}
									</div>
								</div>
							</div>
						</div>
					
						<div class="item-wrapper" id="sort-connections">
							<h3 class="above-item clearfix do_blockminifier">
								<span class="title">Page connections</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								<div class="unlink-wrapper clearfix">
									<h4>Brand</h4>
									<div class="rsc-edge">
										<span class="do_unlink { object_id: 2, edge_id: 33, subject_id: 4 } clearfix">
											<span class="unlink-cross"></span>
											<span class="unlink-item">Ortlieb</span>
										</span>
									</div>
									<div class="rsc-edge">
										<span class="do_unlink { object_id: 2, edge_id: 33, subject_id: 4 } clearfix">
											<span class="unlink-cross"></span>
											<span class="unlink-item">Ortlieb</span>
										</span>
									</div>
									<span class="link-add"><a href="javascript:void(0);">Add another link</a></span>
								</div>
								
								<div class="unlink-wrapper clearfix">
									<h4>Tweede link</h4>
									<div class="rsc-edge">
										<span class="do_unlink { object_id: 2, edge_id: 33, subject_id: 4 } clearfix">
											<span class="unlink-cross"></span>
											<span class="unlink-item">Ortlieb</span>
										</span>
									</div>
									<span class="link-add"><input type="text" class="do_autocomplete" /></span>
								</div>
							</div>
						</div>
					
						<div class="item-wrapper" id="sort-date">
							<h3 class="above-item clearfix do_blockminifier">
								<span class="title">Date Range</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								<div class="admin-form form-item">
									<label>From</label>
									{% include "_edit_date.tpl" date=r.date_start name="date_start" is_end=0 %}
									<label>Till</label>
									{% include "_edit_date.tpl" date=r.date_end name="date_end" is_end=1 %}
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
	</div>
{% endwith %}
{% endblock %}