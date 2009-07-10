{% extends "admin_base.tpl" %}

{% block title %}
Category Hierarchy
{% endblock %}

{% block content %}
<div id="content" class="zp-100">
	<div class="block clearfix">

		<h2>Zotonic Categories</h2>

		<p>
			Categories are used to categorize all pages.  Every page belongs to exactly one category. <br/>
			The categories are defined in a hierarchy.  Here you can change that hierarchy.
		</p>

		<div id="category-sorter">
			{% include "_admin_category_sorter.tpl" delegate=delegate %}
		</div>
	</div>
</div>
{% endblock %}


