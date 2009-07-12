{% extends "admin_base.tpl" %}

{% block title %}Menu{% endblock %}

{% block content %}
<div id="content" class="zp-100">
	<div class="block clearfix">

		<h2>Zotonic Menu Editor</h2>

		<p>
			Here you can change the menu of your site.  Select pages on the right hand side and drag them to the menu on the left.<br/>
			A menu is at most two levels deep.
		</p>

		<div class="zp-30">
			<style>
				.hover {
					background-color: #aac;
					color: black !important;
				}
				
				ul ul {
					margin-left: 50px;
				}
				
				.line {
					height: 5px;
				}

				.ui-draggable-dragging button {
					display: none;
				}
			</style>

			<div id="menu-editor">
				{% include "_admin_menu_menu_view.tpl" %}
			</div>
		</div>
		
		<div class="zp-20">
			&nbsp;
		</div>
		
		<div class="zp-30">
			<h3>Pages</h3>
			<p>Type your search terms to find pages.</p>

			<div class="form-item autocomplete-wrapper clear">
				<input id="{{#input}}" class="autocompleter" type="text" value="" />
				<ul id="{{#suggestions}}" class="short-list"></ul>
			</div>

			{% wire id=#input
				type="keyup" 
				action={typeselect target=#suggestions template="_admin_menu_typeselect_result.tpl"}
			%}
		</div>
	</div>
</div>
{% endblock %}
