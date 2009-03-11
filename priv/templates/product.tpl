{% extends "base.tpl" %}

{# comment #}

{% block title %} Shimano 105 ST-5600 {% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<!-- Area for the main content -->
		<h2>Shimano 105 ST-5600</h2>
		
		<div class="product-wrapper clearfix">
			<div class="block">
				<ul class="product-image clearfix">
					<li class="zp-50"><img src="/image/handgrepen.jpg(300x200)(crop-center)(jaap).jpg" /></li>
					<li class="zp-50"><img src="/image/handgrepen2.jpg(300x200)(crop-center)(jaap).jpg" /></li>
				</ul>
				
				<div class="product-price clearfix">
					<h3>&euro;49 <span>incl. btw</span></h3>
					{# An animate would be nice here #}
					{% button id="product-add-basket" text="+ in winkelmand" postback="show_basket_notice" action={fade_in speed=350 target="product-notice"} %}
				</div>
			</div>
			
			<h3 class="block">Product omschrijving</h3>
			<div class="block">
				<p>Ergonomic innovation is probably more critical to the higher standard of the new 10-speed S.T.I. levers than the added gearing. The narrower cradle-style perch welcomes the high-on-the-hood riding style and accommodates all sizes of hands. The shift internals borrow the robustness from our MTB technology which results in a smooth and effortless shift.</p>

				<h3>Extra gegevens</h3>
				<p>Ergonomic innovation is probably more critical to the higher standard of the new 10-speed S.T.I. levers than the added gearing.</p>
			</div>
			
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
					<img src="/image/trapper_klein.jpg(67x50)(crop)(trapper).jpg" />
					<h4><a href="#">Shimano</a></h4>
					<p>PD-6620-G Trapper</p> 
					<p><a href="#">Bestel snel &raquo;</a></p>
				</li>
				<li class="block clearfix">
					<img src="/image/trapper_klein.jpg(67x50)(crop)(trapper).jpg" />
					<h4><a href="#">Shimano</a></h4>
					<p>PD-6620-G Trapper</p> 
					<p><a href="#">Bestel snel &raquo;</a></p>
				</li>
				<li class="block clearfix">
					<img src="/image/trapper_klein.jpg(67x50)(crop)(trapper).jpg" />
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