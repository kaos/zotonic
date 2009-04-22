{% extends "admin_base.tpl" %}

{% block title %} admin edit resource {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

			<h2>Zophrenic Edit resource</h2>
			
			<form method="post" action="/postback">
				<div class="zp-67">
					<div class="padding">
						<div class="item-wrapper">
							<h3 class="above-item">Basic content</h3>
							<div class="item">
								<fieldset class="admin-form">
									<div class="form-item clearfix">
										<label for="field-title">Title</label>
										<input type="text" id="field-title" name="title"/>
									</div>

									<div class="form-item clearfix">
										<label for="field-intro">Intro</label>
										<textarea rows="10" cols="10" id="field-intro" name="intro" class="intro do_wysiwyg {css: '/lib/css/zp-wysiwyg-iframe.css',  controls: {italic: { visible: true }, createLink: { visible: true }}}"></textarea>
									</div>

									<div class="form-item clearfix">
										<label for="field-content">Body</label>
										<textarea rows="10" cols="10" id="field-content" name="content" class="do_wysiwyg {css: '/lib/css/zp-wysiwyg-iframe.css',  controls: $.fn.wysiwyg.defaultset} body"></textarea>
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
											<input id="no-google" type="checkbox" />Ask google not to index this page
										</label>
									</div>
									<div class="form-item clearfix">
										<label for="title">Page title</label>
										<input type="text" id="title" name="title" class="zp-100" />
									</div>

									<div class="form-item clearfix">
										<label for="keywords">Page keywords</label>
										<input type="text" id="keywords" name="keywords" class="zp-100" />
									</div>

									<div class="form-item clearfix">
										<label for="desc">Page description</label>
										<textarea rows="10" cols="10" id="desc" name="desc" class="seo-desc zp-100"></textarea>
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
							<h3 class="above-item clearfix do_blockminifier">
								<span class="title">Publish</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								{% button class="save-resource" text="save" %}
								{% button class="discard-resource" text="discard" %}
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
									<span class="link-add"><a href="javascript:void(0);">Add another link</a></span>
								</div>
							</div>
						</div>
					
						<div class="item-wrapper" id="sort-date">
							<h3 class="above-item clearfix do_blockminifier">
								<span class="title">Date management</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								<button class="do_tooltip" title="I'm am the tooptip popup, I'm am the tooptip popup">Yeah, give me a tooltip</button>
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
{% endblock %}