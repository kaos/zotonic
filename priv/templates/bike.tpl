{% extends "base.tpl" %}

{% block title %} bike page {% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<h2>Trek Urban Valentia</h2>
		<div class="block clearfix">
			<div class="zp-50">
				{% image "trek_urban.jpg" width=340 height=290 crop class="do_imageviewer" alt="trac_urban" %}
			</div>
		
			<div class="zp-50">
				<div class="product-price clearfix">
					<h3>&euro;1950 <span>incl. btw</span></h3>
					
					<div class="clearfix button-wrapper right">
						{% button class="right right-side-button" text="Vraag een proefrit aan &raquo;" action={slide_fade_in target="test-drive-form"} %}
					</div>
				</div>
				
				<h3>Product omschrijving</h3>
				<p>At Trek, we believe in simplicity, and the idea that complex problems can be solved in simple ways. Our Urban collection embodies those ideas, making it easy for you to park the car, and instead, go by bike.</p>

				<h3>Extra gegevens</h3>
				<p>At Trek, we believe in simplicity, and the idea that complex problems can be solved in simple ways.</p>
			</div>
		</div>
		
		<div class="notification notice clearfix" id="test-drive-form" style="display: none;">
			<form action="postbak" type="post">
				<fieldset>
					<h3>Vraag een proefrit aan</h3>
						<div class="form-item">
						<label for="client-name">Naam</label>
						<input type="text" id="client-name" name="client-name" value="" />
						{% validate id="client-name" type={presence} %}
					</div>
					<div class="form-item">
						<label for="client-phone">Telefoon</label>
						<input type="text" id="client-phone" name="client-phone" value="" />
						{% validate id="client-phone" type={presence} %}
					</div>
					<div class="form-item">
						<label for="client-email">E-mail</label>
						<input type="text" id="client-email" name="client-email" value="" />
						{% validate id="client-email" type={presence} type={email}%}
					</div>
					<div class="form-item clearfix">
						{% button id="product-review-form-trigger" text="Verstuur formulier" class="buy-me" %}
					</div>
				</fieldset>
			</form>
		</div>
	</div>
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">

			<h2>Fietsen</h2>
			<ul class="sub-navigation">
			{% for cat in m.category.bikes.tree1 %}
		    	<li><a href="{% url overview cat=m.category[cat.parent_id].name subcat=cat.name %}">{{ cat.title }}</a></li>
			{% empty %}
				<li>Er zijn geen subcategorieën.</li>
			{% endfor %}
			</ul>
			
		{% with m.rsc[rsc_id].brand[1] as brand_id %}
			<h3 class="block">Merken</h3>
			<ul class="sub-navigation">
				<li><a href="{% url overview cat=parent_cat.name subcat=cat.name %}">Alle merken <span class="amount">({{ prod_count|default:"-" }})</span></a></li>
				{% for b_id, b_name, b_count in cat_brand %}
				    <li><a {% ifequal brand_id b_id %}class="current" {% endifequal %} href="{% url overview cat=parent_cat.name subcat=cat.name brand=b_name %}">{{m.rsc[b_id].title}} <span class="amount">({{ b_count|default:"-" }})</span></a></li>
				{% endfor %}
			</ul>
		{% endwith %}
		</div>
	</div>
{% endblock %}