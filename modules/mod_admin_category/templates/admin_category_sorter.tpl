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
				Categories are used to categorize all pages.  Every page belongs to exactly one category. <br/>
				The categories are defined in a hierarchy.  Here you can change that hierarchy.
			</p>

			{% if editable %}
				<p>
					{% button text="New Category" action={dialog_category_add on_success={reload}} %}
				</p>
				<div class="clearfix">&nbsp;</div>
			{% endif %}
			
			<style>
				.hover a, .hover {
					background: #aac;
					color: black !important;
				}
			
				.short-list .buttons {
	 				float: right;
				}
	
				.ui-draggable-dragging .buttons {
					display: none;
				}

				.short-list .line {
					height: 5px;
				}
			
				.short-list li.depth-1 { margin-left: 0px; }
				.short-list li.depth-2 { margin-left: 30px; }
				.short-list li.depth-3 { margin-left: 60px; }
				.short-list li.depth-4 { margin-left: 90px; }
				.short-list li.depth-5 { margin-left: 120px; }
				.short-list li.depth-7 { margin-left: 150px; }
				.short-list li.depth-8 { margin-left: 180px; }
				.short-list li.depth-9 { margin-left: 210px; }
				.short-list li.depth-10 { margin-left: 240px; }
				.short-list li.depth-11 { margin-left: 270px; }
			</style>
		
			<div id="category-sorter">
				{% include "_admin_category_sorter.tpl" %}
			</div>
		</div>
	</div>
{% endwith %}
{% endblock %}


