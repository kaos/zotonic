{% extends "base.tpl" %}

{# comment #}

{% block title %} {{rsc[2].title}} {% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<!-- Area for the main content -->
		<h2>{{rsc[2].title}}</h2>
		<div class="product-wrapper clearfix">
			<div class="block">
				{% if rsc[2].media %}
				<ul class="product-image clearfix">
					{% if rsc[2].media[1].filename %}
						<li class="zp-50">
							{% image rsc[2].media[1].filename width=300 height=200 crop alt=rsc[2].media[1].filename class="do_imageviewer" %}
						</li>
					{% endif %}
					
					{% if rsc[2].media[2].filename %}
						<li class="zp-50">
							{% image rsc[2].media[2].filename width=300 height=200 crop alt=rsc[2].media[2].filename class="do_imageviewer" %}
						</li>
					{% endif %}
				</ul>
				{% endif %}
				<div class="product-price clearfix">
					<h3>&euro;{{rsc[2].price}} <span>incl. btw</span></h3>
					{# An animate would be nice here #}
					<div class="clearfix button-wrapper right">
						{% button id="product-add-basket" class="buy-me" text="leg in winkelmand" postback="show_basket_notice" action={fade_in speed=350 target="product-notice"} %}
						{% button id="product-buy-basket" class="buy-me right-side-button" text="koop direct &raquo;" postback="show_basket_notice" action={fade_in speed=350 target="product-notice"} %}
					</div>
				</div>
			</div>
			
			{% if rsc[2].body %}
				<h3 class="block">{% _ "Product description" nl="Product omschrijving" %}</h3>
				<div class="block">
					{{rsc[2].body}}
				</div>
			{% endif %}
			
			{% print rsc[2].brand.title %}
			
			{#
			<div class="reviews-title block clearfix">
				<h3 class="in-block">Reviews</h3>
				{% button id="product-add-review-trigger" text="+ schrijf review" action={slide_fade_in speed=350 target="product-add-review-form"} %}
			</div>
			<div class="notification notice clearfix" id="product-add-review-form">
				<form action="postbak" type="post">
					<fieldset>
						<legend>Schrijf een review</legend>
						<div class="form-item">
							<label for="reviewer-name">Naam:</label>
							<input type="text" name="reviewer-name" id="reviewer-name" />
						</div>
						<div class="form-item">
							<label for="reviewer-review">Review:</label>
							<textarea id="reviewer-review" cols="20" rows="20" name="reviewer-message" /></textarea>
						</div>
						{% button id="product-review-form-trigger" text="Voeg toe" action={slide_fade_out speed=350 target="product-add-review-form"} %}
					</fieldset>
				</form>
			</div>
			
			<ul class="reviews-list">
				<li class="block">
					<div class="clearfix">
						<h4 class="left">Marc Worrell <span>5 maart 2009</span></h4>
						<img src="/lib/images/rating.jpg" alt="rating" class="right" />
					</div>
					<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam nunc. Aliquam erat volutpat. Fusce tortor ligula, dictum sit amet, aliquam non, luctus quis, justo.</p>
				</li>
				<li class="block">
					<div class="clearfix">
						<h4 class="left">Tim Benniks <span>3 maart 2009</span></h4>
						<img src="/lib/images/rating.jpg" alt="rating" class="right" />
					</div>
					<p>
						Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam nunc. Aliquam erat volutpat. Fusce tortor ligula, dictum sit amet, aliquam non, luctus quis, justo. 
						Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam nunc. Aliquam erat volutpat. Fusce tortor ligula, dictum sit amet, aliquam non, luctus quis, justo.
					</p>
				</li>
			</ul>
			#}
		</div>
	</div>
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">
			
			{% include "_subnav.tpl" %}
			
			<h2>Gerelateerde artikelen</h2>
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
			
			<div class="notification notice" id="product-notice">
				<h3>Winkelmand informatie</h3>
				U heeft de Shimano 105 ST-5600 <strong>2 keer</strong> in uw <a href="#">winkelmand.</a>
			</div>
		</div>
	</div>
{% endblock %}