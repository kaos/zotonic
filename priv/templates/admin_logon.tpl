{% extends "admin_base.tpl" %}

{% block title %} admin overview {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
		<div class="clearfix">
			<div class="zp-33">&nbsp;</div>
			<div class="zp-33 block">
				<h2>Logon to Zophrenic</h2>

				<p>To administer your system you need to logon.</p>
				<form method="post">
					<fieldset>
						<div class="form-item">
							<label for="zp-username">Name</label>
							<input type="text" name="zp-username" id="zp-username" />
						</div>
						<div class="form-item">

							<label for="zp-password">Password</label>
							<input type="text" name="zp-password" id="zp-password" />
						</div>
						<div class="form-item clearfix">
							<button type="submit">Logon</button>
							<button>Cancel</button>
						</div>

					</fieldset>
				</form>
			</div>
			<div class="zp-33">&nbsp;</div>
		</div>
		<div class="push"></div>
	</div>
{% endblock %}

{% block navigation %}{% endblock %}