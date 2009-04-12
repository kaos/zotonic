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
					{% image "140.jpg" width=200 height=70 crop alt="bags" class="left" %}
				</a>
				<h3>Bags</h3>
				<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod. <a href="#">read&nbsp;more&nbsp;&raquo;</a></p>
			</li>
			<li class="block clearfix">
				<a href="/category/bags" title="cateogry bags">
					{% image "37.jpg" width=200 height=70 crop alt="Glasses" class="left" %}
				</a>
				<h3>Glasses</h3>
				<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor. <a href="#">read&nbsp;more&nbsp;&raquo;</a></p>
			</li>
			<li class="block clearfix">
				<a href="/category/batteries" title="cateogry batteries">
					{% image "1610.jpg" width=200 height=70 crop alt="bags" class="left" %}
				</a>
				<h3>Batteries</h3>
				<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod. <a href="#">read&nbsp;more&nbsp;&raquo;</a></p>
			</li>
			<li class="block clearfix">
				<a href="/category/fietscomputers" title="cateogry bags">
					{% image "1586.jpg" width=200 height=70 crop alt="Glasses" class="left" %}
				</a>
				<h3>Fietscomputers</h3>
				<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor. <a href="#">read&nbsp;more&nbsp;&raquo;</a></p>
			</li>
			<li class="block clearfix">
				<a href="/category/montagestandaards" title="cateogry Montagestandaards">
					{% image "1636.jpg" width=200 height=70 crop alt="bags" class="left" %}
				</a>
				<h3>Montagestandaards</h3>
				<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod. <a href="#">read&nbsp;more&nbsp;&raquo;</a></p>
			</li>
		</ul>
		
		<div class="category-sidebar zp-33">
			<div class="block clearfix">
				<h3>Ldieladie</h3>
				<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce ultricies nisi in lectus. Morbi et sem nec eros bibendum vestibulum. Ut vitae erat vitae dui tempor dictum. Nam sem. Sed iaculis lorem non ipsum. Donec aliquet. Fusce vel elit si.</p>
				<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce ultricies nisi in lectus. Morbi et sem nec eros bibendum vestibulum. Ut vitae erat vitae dui tempor dictum. Nam sem. Sed iaculis lorem non ipsum. Donec aliquet. Fusce vel elit si.</p>
			</div>
		</div>
	</div>	
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">
			
			<h3 class="block">Brands</h3>
			<ul id="sub-navigation">
			    <li><a href="#">Stevens <span class="amount">(3)<span></a></li>
			    <li><a href="#">Ortliep <span class="amount">(5)<span></a></li>
			    <li><a href="#">Sky is pancaces <span class="amount">(2)<span></a></li>
			    <li><a href="#">Marc <span class="amount">(13)<span></a></li>
			</ul>
			
			<h3 class="block">Featured products</h3>
			<ul class="related-articles">
				<li class="block clearfix">
					{% image "trapper_klein.jpg" width=67 height=50 crop alt="trapper" %}
					<h4><a href="#">Shimano</a></h4>
					<p>PD-6620-G Trapper</p> 
					<p><a href="#">Bestel snel &raquo;</a></p>
				</li>
				<li class="block clearfix">
					{% image "trapper_klein.jpg" width=67 height=50 crop alt="trapper" %}
					<h4><a href="#">Shimano</a></h4>
					<p>PD-6620-G Trapper</p> 
					<p><a href="#">Bestel snel &raquo;</a></p>
				</li>
				<li class="block clearfix">
					{% image "trapper_klein.jpg" width=67 height=50 crop alt="trapper" %}
					<h4><a href="#">Shimano</a></h4>
					<p>PD-6620-G Trapper</p> 
					<p><a href="#">Bestel snel &raquo;</a></p>
				</li>
			</ul>
		</div>
	</div>
{% endblock %}
