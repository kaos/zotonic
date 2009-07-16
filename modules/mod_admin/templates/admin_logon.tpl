{% extends "admin_base.tpl" %}

{% block title %} admin overview {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
		<div class="clearfix">
			<div class="zp-33">&nbsp;</div>
			<div class="zp-33 block">
				<h2>Logon to Zophrenic</h2>

				<p>To administer your system you need to logon.</p>

				{% include "_logon.tpl" %}

			</div>
			<div class="zp-33">&nbsp;</div>
		</div>
	</div>
{% endblock %}

{% block navigation %}{% endblock %}