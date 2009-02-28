{% extends "base.tpl" %}

{# comment #}

{% block content %}
<!-- Area for the main content -->
<h2>Dit is een pagina dummy</h2>
<div class="block clearfix">
	<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce ultricies nisi in lectus. Morbi et sem nec eros bibendum vestibulum. Ut vitae erat vitae dui tempor dictum. Nam sem. Sed iaculis lorem non ipsum. Donec aliquet. Fusce vel elit.ccumsan tellus orci id nisi. </p>

	<div class="zp-50">
		<h3>Een stukkie tekst</h3>
		<p>Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>
	</div>
	<div class="zp-50">
		<h3>Een stukkie tekst</h3>
		<p>non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>
	</div>
</div>
{% endblock %}

{% block sidebar %}
<!-- sidebar content for sub- navigation and more -->
<div class="padding">
	<ul id="sub-navigation">
		<li><a href="#">Accessoires</a></li>
		<li><a href="#">Klanten service</a></li>
		<li><a href="#">Vergelijk fietsen</a></li>
		<li><a href="#">Categorien</a></li>
		<li><a href="#">Over Hans Struijk</a></li>
	</ul>
</div>
{% endblock %}
