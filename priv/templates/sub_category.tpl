{% extends "base.tpl" %}

{% block title %} Category Page sub{% endblock %}

{% block content %}
	<div id="content-area" class="zp-75 category-overview">
		<!-- Area for the main content -->
		<h2>Bags category</h2>
		<div class="block clearfix">
			<h3>Ldieladie</h3>
			<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce ultricies nisi in lectus. Morbi et sem nec eros bibendum vestibulum. Ut vitae erat vitae dui tempor dictum. Nam sem. Sed iaculis lorem non ipsum. Donec aliquet. Fusce vel elit si.</p>
		</div>
		
		<ul class="subcategory-product-list clearfix">
			<li class="zp-50 product-list-item">
				<div class="block clearfix">
					<a href="/category/bags" title="cateogry bags">
						{% image "140.jpg" width=60 height=60 crop alt="bags" class="left" %}
					</a>
					<a href="/category/bags" title="cateogry bags">
						<h3>Bags</h3>
					</a>
					<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod.</p>
					<div class="product-price clearfix clear">
						<h3>&euro;22 <span>incl. btw</span></h3>
						<div class="clearfix button-wrapper right">
							{% button id="product-view" class="buy-me" text="meer info" %}
							{% button id="product-buy-basket" class="buy-me right-side-button" text="koop direct &raquo;" %}
						</div>
					</div>
				</div>
			</li>
			<li class="zp-50 product-list-item">
				<div class="block clearfix block-last">
					<a href="/category/bags" title="cateogry bags">
						{% image "37.jpg" width=60 height=60 crop alt="bags" class="left" %}
					</a>
					<a href="/category/bags" title="cateogry bags">
						<h3>Bags</h3>
					</a>
					<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod.</p>
					<div class="product-price clearfix clear">
						<h3>&euro;22 <span>incl. btw</span></h3>
						<div class="clearfix button-wrapper right">
							{% button id="product-view" class="buy-me" text="meer info" %}
							{% button id="product-buy-basket" class="buy-me right-side-button" text="koop direct &raquo;" %}
						</div>
					</div>
				</div>
			</li>
			<li class="zp-50 product-list-item">
				<div class="block clearfix">
					<a href="/category/bags" title="cateogry bags">
						{% image "134.jpg" width=60 height=60 crop alt="bags" class="left" %}
					</a>
					<a href="/category/bags" title="cateogry bags">
						<h3>Bags</h3>
					</a>
					<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod.</p>
					<div class="product-price clearfix clear">
						<h3>&euro;22 <span>incl. btw</span></h3>
						<div class="clearfix button-wrapper right">
							{% button id="product-view" class="buy-me" text="meer info" %}
							{% button id="product-buy-basket" class="buy-me right-side-button" text="koop direct &raquo;" %}
						</div>
					</div>
				</div>
			</li>
			<li class="zp-50 product-list-item block-last">
				<div class="block clearfix">
					<a href="/category/bags" title="cateogry bags">
						{% image "1610.jpg" width=60 height=60 crop alt="bags" class="left" %}
					</a>
					<a href="/category/bags" title="cateogry bags">
						<h3>Bags</h3>
					</a>
					<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod.</p>
					<div class="product-price clearfix clear">
						<h3>&euro;22 <span>incl. btw</span></h3>
						<div class="clearfix button-wrapper right">
							{% button id="product-view" class="buy-me" text="meer info" %}
							{% button id="product-buy-basket" class="buy-me right-side-button" text="koop direct &raquo;" %}
						</div>
					</div>
				</div>
			</li>
			<li class="zp-50 product-list-item">
				<div class="block clearfix">
					<a href="/category/bags" title="cateogry bags">
						{% image "1636.jpg" width=60 height=60 crop alt="bags" class="left" %}
					</a>
					<a href="/category/bags" title="cateogry bags">
						<h3>Bags</h3>
					</a>
					<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod.</p>
					<div class="product-price clearfix clear">
						<h3>&euro;22 <span>incl. btw</span></h3>
						<div class="clearfix button-wrapper right">
							{% button id="product-view" class="buy-me" text="meer info" %}
							{% button id="product-buy-basket" class="buy-me right-side-button" text="koop direct &raquo;" %}
						</div>
					</div>
				</div>
			</li>
		</ul>
	</div>	
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">
			
			<h3 class="block">Siblings</h3>
			<ul id="sub-navigation">
			    <li><a href="#">Glasses</a></li>
			    <li><a href="#">Bags</a></li>
			    <li><a href="#">Batteries</a></li>
			    <li><a href="#">Fietscomputers</a></li>
			    <li><a href="#">Montagestandaards</a></li>
			</ul>
			
			<h3 class="block">Brands</h3>
			<ul id="sub-navigation">
			    <li><a href="#">Stevens <span class="amount">(3)<span></a></li>
			    <li><a href="#">Ortliep <span class="amount">(5)<span></a></li>
			    <li><a href="#">Sky is pancaces <span class="amount">(2)<span></a></li>
			    <li><a href="#">Marc <span class="amount">(13)<span></a></li>
			</ul>
		</div>
	</div>
{% endblock %}
