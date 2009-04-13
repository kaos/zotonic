{% extends "admin_base.tpl" %}

{% block title %} admin edit resource {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

			<h2>Zophrenic Edit resource</h2>

			<div class="zp-75">
				<div class="padding">
					<form method="post" target="/postback">
						<fieldset class="admin-form">
							<div class="form-item">
								<label for="title">Title</label>
								<input type="text" id="title" name="title" />
							</div>

							<div class="form-item">
								<label for="shorttitle">Short title</label>
								<input type="text" id="shorttitle" name="shorttitle" />
							</div>

							<div class="form-item">
								<label for="content">Page content</label>
								<textarea id="content" name="content" class="do_wysiwyg"></textarea>
							</div>
						</fieldset>			
					</form>
				</div>
			</div>
			<div class="zp-25">
				<div class="padding">
					<h3 class="alt">Context</h3>
					<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>
				</div>
			</div>
		</div>
		<div class="push"></div>
	</div>
{% endblock %}