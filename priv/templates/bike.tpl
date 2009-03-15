{% extends "base.tpl" %}

{# comment #}

{% block title %} bike page {% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<h2>Trek Urban Valentia</h2>
		<div class="block clearfix">
			<div class="zp-50">
				{% image "trek_urban.jpg" width=340 height=290 crop class="do_imageviewer" alt="trac_urban" %}
			</div>
		
			<div class="zp-50">
				<h3>Product omschrijving</h3>
				<p>At Trek, we believe in simplicity, and the idea that complex problems can be solved in simple ways. Our Urban collection embodies those ideas, making it easy for you to park the car, and instead, go by bike.</p>

				<h3>Extra gegevens</h3>
				<p>At Trek, we believe in simplicity, and the idea that complex problems can be solved in simple ways.</p>
				
				<div class="product-price clearfix">
					<h3>&euro;1950 <span>incl. btw</span></h3>
					<button>Vraag een proefrit aan</button>
				</div>
			</div>
		</div>	
	</div>
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">
			
			{% include "_subnav.tpl" %}
			
			<h2>Gerelateerde producten</h2>
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