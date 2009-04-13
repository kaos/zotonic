{% extends "base.tpl" %}

{% block title %}{{ m.category[cat_id].title }}{% endblock %}

{% block content %}
	<div id="content-area" class="zp-75 category-overview">
		<!-- Area for the main content -->
		<h2>{{ m.category[cat_id].title }}</h2>
		<div class="block clearfix">
			<p class="intro">{{ m.category[cat_id].intro }}</p>
			{{ m.category[cat_id].body }}
		</div>
		
		<ul class="subcategory-product-list clearfix">
			{% for id in products %}
			<li class="zp-50 product-list-item">
				<div class="block clearfix">
					<a href="{% url product id=id slug=m.rsc[id].slug %}">
						{% image m.rsc[id].media[1].filename width=60 height=60 crop alt=m.rsc[id].title class="left" %}
					</a>
					<a href="{% url product id=id slug=m.rsc[id].slug %}">
						<h3>{{ m.rsc[id].title }}</h3>
					</a>
					<p>{{ m.rsc[id].intro }}</p>
					<div class="product-price clearfix clear">
						<h3>&euro;{{ m.rsc[id].price|format_price }} <!--span>incl. btw</span--></h3>
						<div class="clearfix button-wrapper right">
							{% button class="buy-me" text="meer info" action={redirect id=id} %}
							{% button class="buy-me right-side-button" text="koop direct &raquo;" %}
						</div>
					</div>
				</div>
			</li>
			{% empty %}
			<li class="zp-100">
				<p>Er zijn geen producten in deze categorie.</p>
			</li>
			{% endfor %}
		</ul>
	</div>	
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">
			
			<h3 class="block">{{ m.category[m.category[cat_id].parent_id].title }}</h3>
			<ul id="sub-navigation">
				{% for cat in m.category[m.category[cat_id].parent_id].tree1 %}
			    	<li><a {% ifequal cat.id cat_id %}class="current" {% endifequal %} href="{% url overview cat=m.category[cat.parent_id].name subcat=cat.name %}">{{ cat.title }}</a></li>
				{% endfor %}
			</ul>
			
			<h3 class="block">Brands</h3>
			<ul id="sub-navigation">
			    <li><a href="#">Stevens <span class="amount">(3)<span></a></li>
			    <li><a href="#">Ortliep <span class="amount">(5)<span></a></li>
			    <li><a href="#">Duracell <span class="amount">(2)<span></a></li>
			</ul>
		</div>
	</div>
{% endblock %}
