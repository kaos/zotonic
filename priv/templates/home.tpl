{% extends "base.tpl" %}

{# comment #}

{% block title %} Home page {% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<!-- Area for the main content -->
		<h2>Shimano XTR Aanbieding <span><a href="#">Lees meer &raquo;</a></span></h2>
		<div class="home-bargain clearfix">
			<img src="./lib/images/trapper.jpg" alt="Trapper" />
			<img class="bargain-last" src="./lib/images/onderdelen.jpg" alt="Trapper" />
		</div>

		<h3 class="block">Welkom bij Hans Struijk Fietsen</h3>
		<div class="block">
			<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce ultricies nisi in lectus. Morbi et sem nec eros bibendum vestibulum. Ut vitae erat vitae dui tempor dictum. Nam sem. Sed iaculis lorem non ipsum. Donec aliquet. Fusce vel elit.ccumsan tellus orci id nisi. </p>
		</div>

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
					<li><a href="#">Betaalmogelijkheden</a></li>
					<li><a href="#">Verzendkosten</a></li>
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
			
			<!-- Newsletter -->
			<h3 class="block">Nieuwsbrief</h3>
			<div class="block clearfix newsletter">
				<p>Schrijf je nu in!</p>
				<form action="">
					<fieldset>
						<input type="text" name="newsletter" id="newsletter" class="input-newsletter" />
						<button>Aanmelden</button>
					</fieldset>
				</form>
			</div>

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
