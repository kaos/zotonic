{% extends "admin_base.tpl" %}

{% block title %} modules {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

		<h2>Zophrenic Modules</h2>

		<h3 class="above-list ">Modules overview</h3>
		<ul class="short-list">
			<li class="headers clearfix">
				<span class="zp-20">Title</span>
				<span class="zp-50">Description</span>
				<span class="zp-20">Author</span>
				<span class="zp-10">Activate</span>
			</li>
		{% for module, props in modules %}
			<li id="{{ #module }}">
				<a href="#" class="clearfix">
					<span class="zp-20">{{ props.mod_title|default:props.title }}</span>
					<span class="zp-50">{{ props.mod_description|default:"-" }}</span>
					<span class="zp-20">{{ props.author|escape|default:"-" }}</span>
					<span class="zp-10">
						{% if props.active %}
							{% button text="Deactivate" action={module_toggle module=module} %}
						{% else %}
							{% button text="Activate" action={module_toggle module=module} %}
						{% endif %}
					</span>
				</a>
			</li>
		{% empty %}
			<li>
				No items found
			</li>
		{% endfor %}
		</ul>

		</div>
		<div class="push"></div>
	</div>
{% endblock %}