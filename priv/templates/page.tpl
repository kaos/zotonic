{% extends "base.tpl" %}

{# comment #}

{% block title %} Page Template {% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<!-- Area for the main content -->
		<h2>Dit is een pagina dummy {{ q.id }}</h2>
		<div class="block clearfix">
			<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce ultricies nisi in lectus. Morbi et sem nec eros bibendum vestibulum. Ut vitae erat vitae dui tempor dictum. Nam sem. Sed iaculis lorem non ipsum. Donec aliquet. Fusce vel elit.ccumsan tellus orci id nisi. </p>

			<textarea class="do_wysiwyg" rows="11" cols="150"></textarea>
			
		</div>
	</div>	
{% endblock %}