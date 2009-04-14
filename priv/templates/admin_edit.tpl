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
					<h3 class="alt">Auto completer</h3>
					<input type="text" name="test" class="do_autocomplete" title="Try typing a word, I'll finish it." />
					
					<p>&nbsp;</p>
					<h3 class="alt">Unlinker</h3>
					<div class="rsc-edge">
						<span class="do_unlink { object_id: 2, edge_id: 33, subject_id: 4 }">
							About	
							<img alt="" src="/lib/images/cross.png" />
						</span>
					</div>

					<div class="rsc-edge">
						<span class="do_unlink { object_id: 2, edge_id: 33, subject_id: 4 }">
							About	
							<img alt="" src="/lib/images/cross.png" />
						</span>
					</div>
				</div>
			</div>
		</div>
		<div class="push"></div>
	</div>
{% endblock %}