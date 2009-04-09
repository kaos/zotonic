{% extends "base.tpl" %}

{# comment #}

{% block title %} Category Page {% endblock %}

{% block content %}
	<div id="content-area" class="zp-75 category-overview">
		<!-- Area for the main content -->
		<h2>Accessoires category</h2>

		<ul class="zp-67 subcategory-list">
			<li class="block clearfix">
				<a href="/category/bags" title="cateogry bags">
					{% image "140.jpg" width=230 height=140 crop alt="bags" class="left" %}
				</a>
				<h3>Bags</h3>
				<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam.</p>
			</li>
			<li class="block clearfix">
				<a href="/category/bags" title="cateogry bags">
					{% image "37.jpg" width=230 height=140 crop alt="Glasses" class="left" %}
				</a>
				<h3>Glasses</h3>
				<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam.</p>
			</li>
		</ul>
		
		<div class="category-sidebar zp-33">
			<div class="block clearfix">
				<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce ultricies nisi in lectus. Morbi et sem nec eros bibendum vestibulum. Ut vitae erat vitae dui tempor dictum. Nam sem. Sed iaculis lorem non ipsum. Donec aliquet. Fusce vel elit si.</p>
			</div>
		</div>
	</div>	
{% endblock %}