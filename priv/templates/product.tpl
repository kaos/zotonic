{% extends "base.tpl" %}

{# comment #}

{% block title %} Shimano 105 ST-5600 {% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<!-- Area for the main content -->
		<h2>Shimano 105 ST-5600</h2>
		
		<div class="product-wrapper clearfix">
			<ul class="product-image zp-50">
				<li><img src="/lib/images/handgrepen.jpg" /></li>
				<li class="last"><img src="/lib/images/handgrepen2.jpg" /></li>
			</ul>
			<div class="product-description zp-50">
				<div class="block">
					<h3>Omschrijving van de Shimano 105 ST-5600</h3>
					<p>Ergonomic innovation is probably more critical to the higher standard of the new 10-speed S.T.I. levers than the added gearing. The narrower cradle-style perch welcomes the high-on-the-hood riding style and accommodates all sizes of hands. The shift internals borrow the robustness from our MTB technology which results in a smooth and effortless shift.</p>
				
					<h3>Extra gegevens</h3>
					<p>Ergonomic design: Refined lever hood for improved ergonomics.</p>
					
					<h3>Beoordeling</h3>
					<p class="clearfix"><img src="/lib/images/rating.jpg" alt="rating" /></p>
					
					<div class="product-price clearfix">
						<h3>&euro; 49 <span>incl. btw</span></h3>
						{% @wire id="product-add-basket" action={fade_in speed=300 target="product-notice"} %} 
						<button id="product-add-basket">+ in winkelmand</button>
					</div>
				</div>
				
				<div class="notification notice" id="product-notice">
					<h3>Winkelmand informatie</h3>
					U heeft de Shimano 105 ST-5600 <strong>2 keer</strong> in uw <a href="#">winkelmand.</a>
				</div>
				
				<h3 class="block">Gerelateerde artikelen</h3>
				<div class="block">
					<p>dd</p>
				</div>	
			</div>
		</div>
	</div>
{% endblock %}