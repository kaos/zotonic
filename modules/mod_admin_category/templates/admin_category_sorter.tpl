{% extends "admin_base.tpl" %}

{% block title %}
Category Hierarchy
{% endblock %}

{% block content %}
{% with m.acl.is_admin as editable %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

			<h2>Zotonic Categories</h2>

			<p>
				Categories are used to categorize all pages. Every page belongs to exactly one category. The categories are defined in a hierarchy. Here you can change that hierarchy.
			</p>

			{% if editable %}
				{% button text="Add a new category" action={dialog_category_add on_success={reload}} %}
			{% endif %}
		
			<div id="category-sorter" class="clear">
				{% include "_admin_category_sorter.tpl" %}
			</div>
		</div>
	</div>
{% endwith %}
{% endblock %}


