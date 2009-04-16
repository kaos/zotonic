{% extends "admin_base.tpl" %}

{% block title %} admin edit resource {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

			<h2>Zophrenic Edit resource</h2>

			<div class="zp-67">
				<div class="padding">
					<form method="post" target="/postback">
						<h3 class="above-list">Basic content</h3>
						<div class="item">
							<fieldset class="admin-form">
								<div class="form-item clearfix">
									<label for="title">Title</label>
									<input type="text" id="title" name="title"/>
								</div>

								<div class="form-item clearfix">
									<label for="intro">Intro</label>
									<textarea id="intro" name="intro" class="intro"></textarea>
								</div>

								<div class="form-item clearfix">
									<label for="content">Body</label>
									<textarea id="content" name="content" class="do_wysiwyg {css: '/lib/css/zp-wysiwyg-iframe.css' } body"></textarea>
								</div>
							</fieldset>
						</div>
					</form>
					
					<h3 class="above-list">Seo Content</h3>
					<div class="item clearfix">
						<fieldset class="admin-form">
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
								<textarea id="desc" name="desc" class="seo-desc zp-100"></textarea>
							</div>
						</fieldset>
					</div>
				</div>
			</div>
			
			<div class="zp-33">
				<div class="padding">
					<h3 class="above-list">Publication</h3>
					<div class="item clearfix">
						<input type="text" name="test" class="do_autocomplete" title="Try typing a word, I'll finish it." />
						<div class="rsc-edge">
							<span class="do_unlink { object_id: 2, edge_id: 33, subject_id: 4 }">
								About<img alt="" src="/lib/images/cross.png" />
							</span>
						</div>
						<div class="rsc-edge">
							<span class="do_unlink { object_id: 2, edge_id: 33, subject_id: 4 }">
								About<img alt="" src="/lib/images/cross.png" />
							</span>
						</div>
						<button class="do_tooltip" title="I'm am the tooptip popup, I'm am the tooptip popup">Yeah, give me a tooltip</button>
					</div>
					
					<h3 class="above-list">Connections to</h3>
					<div class="item clearfix">
						<input type="text" name="test" class="do_autocomplete" title="Try typing a word, I'll finish it." />
						<div class="rsc-edge">
							<span class="do_unlink { object_id: 2, edge_id: 33, subject_id: 4 }">
								About<img alt="" src="/lib/images/cross.png" />
							</span>
						</div>
						<div class="rsc-edge">
							<span class="do_unlink { object_id: 2, edge_id: 33, subject_id: 4 }">
								About<img alt="" src="/lib/images/cross.png" />
							</span>
						</div>
						<button class="do_tooltip" title="I'm am the tooptip popup, I'm am the tooptip popup">Yeah, give me a tooltip</button>
					</div>
					
					<h3 class="above-list">Date</h3>
					<div class="item clearfix">
						<input type="text" name="test" class="do_autocomplete" title="Try typing a word, I'll finish it." />
						<div class="rsc-edge">
							<span class="do_unlink { object_id: 2, edge_id: 33, subject_id: 4 }">
								About<img alt="" src="/lib/images/cross.png" />
							</span>
						</div>
						<div class="rsc-edge">
							<span class="do_unlink { object_id: 2, edge_id: 33, subject_id: 4 }">
								About<img alt="" src="/lib/images/cross.png" />
							</span>
						</div>
						<button class="do_tooltip" title="I'm am the tooptip popup, I'm am the tooptip popup">Yeah, give me a tooltip</button>
					</div>
					
					<h3 class="above-list">ACL</h3>
					<div class="item clearfix">
						<input type="text" name="test" class="do_autocomplete" title="Try typing a word, I'll finish it." />
						<div class="rsc-edge">
							<span class="do_unlink { object_id: 2, edge_id: 33, subject_id: 4 }">
								About<img alt="" src="/lib/images/cross.png" />
							</span>
						</div>
						<div class="rsc-edge">
							<span class="do_unlink { object_id: 2, edge_id: 33, subject_id: 4 }">
								About<img alt="" src="/lib/images/cross.png" />
							</span>
						</div>
						<button class="do_tooltip" title="I'm am the tooptip popup, I'm am the tooptip popup">Yeah, give me a tooltip</button>
					</div>
				</div>
			</div>
		</div>
		<div class="push"></div>
	</div>
{% endblock %}