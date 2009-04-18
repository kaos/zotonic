{% extends "base.tpl" %}

{% block title %} Home page {% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<!-- Area for the main content -->
		
		<h2>Welkom bij Hans Struijk Fietsen</h2>
		<div class="block">
			<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce ultricies nisi in lectus. Morbi et sem nec eros bibendum vestibulum. Ut vitae erat vitae dui tempor dictum. Nam sem. Sed iaculis lorem non ipsum. Donec aliquet. Fusce vel elit.ccumsan tellus orci id nisi. </p>
		</div>
		
		<h3 class="block">Featured products</h3>
		<ul class="subcategory-product-list clearfix">
			<li class="zp-50 product-list-item">
				<div class="block clearfix">
					<a href="#">
						{% image "134.jpg" width=120 height=120 crop alt=m.rsc[id].title class="left" %}
					</a>
					<a href="#">
						<h3>A nice Bag</h3>
					</a>
					<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.</p>
					<div class="product-price clearfix clear">
						<h3>&euro;33</h3>
						<div class="clearfix button-wrapper right">
							{% button class="buy-me" text="meer info" %}
							{% button class="buy-me right-side-button" text="koop direct &raquo;" %}
						</div>
					</div>
				</div>
			</li>
			<li class="zp-50 product-list-item">
				<div class="block clearfix">
					<a href="#">
						{% image "134.jpg" width=120 height=120 crop alt=m.rsc[id].title class="left" %}
					</a>
					<a href="#">
						<h3>A nice Bag</h3>
					</a>
					<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.</p>
					<div class="product-price clearfix clear">
						<h3>&euro;33</h3>
						<div class="clearfix button-wrapper right">
							{% button class="buy-me" text="meer info" %}
							{% button class="buy-me right-side-button" text="koop direct &raquo;" %}
						</div>
					</div>
				</div>
			</li>
			<li class="zp-50 product-list-item">
				<div class="block clearfix">
					<a href="#">
						{% image "134.jpg" width=120 height=120 crop alt=m.rsc[id].title class="left" %}
					</a>
					<a href="#">
						<h3>A nice Bag</h3>
					</a>
					<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.</p>
					<div class="product-price clearfix clear">
						<h3>&euro;33</h3>
						<div class="clearfix button-wrapper right">
							{% button class="buy-me" text="meer info" %}
							{% button class="buy-me right-side-button" text="koop direct &raquo;" %}
						</div>
					</div>
				</div>
			</li>
			<li class="zp-50 product-list-item">
				<div class="block clearfix">
					<a href="#">
						{% image "134.jpg" width=120 height=120 crop alt=m.rsc[id].title class="left" %}
					</a>
					<a href="#">
						<h3>A nice Bag</h3>
					</a>
					<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.</p>
					<div class="product-price clearfix clear">
						<h3>&euro;33</h3>
						<div class="clearfix button-wrapper right">
							{% button class="buy-me" text="meer info" %}
							{% button class="buy-me right-side-button" text="koop direct &raquo;" %}
						</div>
					</div>
				</div>
			</li>
		</ul>
		
		<div class="block clearfix">
			<div class="zp-33">
				<h4>Bestellen en betalen</h4>
				<ul class="misc-list">
					<li><a href="#">Betaalmogelijkheden</a></li>
					<li><a href="#">Verzendkosten</a></li>
					<li><a href="#">Retouren</a></li>
				</ul>
			</div>	

			<div class="zp-33">
				<h4>Over Hans Struijk Fietsen</h4>
				<ul class="misc-list">
					<li><a href="#">Over Hans Struijk</a></li>
					<li><a href="#">Filialen</a></li>
					<li><a href="#">Veel gestelde vragen</a></li>
				</ul>
			</div>

			<div class="zp-333">
				<h4>Overigen</h4>
				<ul class="misc-list">
					<li><a href="#">Contact</a></li>
					<li><a href="#">Privacy policy</a></li>
					<li><a href="#">Algemene voorwaarden</a></li>
				</ul>
			</div>	
		</div>
	</div>
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">
			
			{% include "_subnav.tpl" %}
			
			{#<!-- Newsletter -->
			<h3 class="block">Nieuwsbrief</h3>
			<div class="block clearfix newsletter">
				<p>Schrijf je nu in!</p>
				<form action="">
					<fieldset>
						<input type="text" name="newsletter" id="newsletter" class="input-newsletter" />
						<button>Aanmelden</button>
					</fieldset>
				</form>
			</div>#}

			<!-- Latest news -->
			<h3 class="block">Laatste nieuws</h3>
			<div class="block clearfix news">
				<ul class="news-list">
					<li><a href="#"><span>15/03/2009</span> &ndash; Website relesase is...</a></li>
					<li><a href="#"><span>22/02/2009</span> &ndash; Design fase van star...</a></li>
					<li><a href="#"><span>13/01/2009</span> &ndash; Nieuwe website in on...</a></li>
					<li><a href="#"><span>01/01/2009</span> &ndash; Eerste overleggen ge...</a></li>
				</ul>
			</div>
		</div>	
	</div>
{% endblock %}

{% block bike_logos %}
	{% include "_bikelogos.tpl" %}
{% endblock %}
