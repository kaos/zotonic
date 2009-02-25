<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
	<title>Hans Struik Fietsen</title>

	<link href="/lib/css/zp-base.css" type="text/css" media="screen" rel="stylesheet" /> 
	<link href="/lib/css/zp-type.css" type="text/css" media="screen" rel="stylesheet" /> 
	<link href="/lib/css/zp-forms.css" type="text/css" media="screen" rel="stylesheet" /> 
	<link href="/lib/css/zp-project.css" type="text/css" media="screen" rel="stylesheet" /> 
	<link href="/lib/css/zp-growl.css" type="text/css" media="screen" rel="stylesheet" /> 

	<!--[if IE]><link href="/lib/css/zp-ie.css" type="text/css" media="screen" rel="stylesheet" /><![endif]--> 
</head>
<body>
	<div class="skip">
		<a href="#content" title="Go directly to page content">Go to page content</a>
	</div>

	<div class="zp-wrapper">
		<div id="header" class="zp-100 clearfix">

			<!-- Shopping basket -->
			<div class="basket-wrapper zp-50 right">
				<div class="basket-user clearfix">
					<p class="basket-user-image"></p>
					<p class="basket-text">Welkom, Bastiaan Hagenouw <a href="#">Uitloggen &raquo;</a></p>
				</div>
				<div class="basket-basket">
					<p class="basket-basket-image"></p>
					<p class="basket-text">Uw winkelmand is leeg <a href="#">Bekijk uw winkelmand &raquo;</a></p>
				</div>
				<div class="basket-logos"><span><!-- Logo"s content -->iDeal, thuiswinkel waarborg</span></div>
			</div>
			<div class="navigation-wrapper clearfix">

				<!-- Main navigation -->
				<ul id="navigation" class="zp-60">
					<li class="logo"><h1>Hans Struijk Fietsen</h1></li>
					<li><a href="#" title="#" class="current">Home</a></li>
					<li><a href="#" title="#">Fietsen</a></li>
					<li><a href="#" title="#">Accesoires</a></li>
					<li><a href="#" title="#">Service</a></li>
					<li class="lang" title="Nederlands"><a href="#" title="Nederlands" class="dutch"><span>Nederlands</span></a></li>
					<li class="lang" title="English"><a href="#" title="English" class="english"><span>English</span></a></li>
				</ul>

				<!-- Search bar -->
				<div id="search" class="zp-25 right">
					<form id="searchform" method="post" action="postback">
						<fieldset>
							<input id="quick-search" type="text" name="search" />
							{% @button text="Zoek" postback="show_growl_search"  %}
						</fieldset>
					</form>
				</div>
			</div>	
		</div>
		<div id="content" class="zp-100 clearfix">
			<div id="sidebar" class="zp-25">
				<!-- sidebar content for sub- navigation and more -->
				<div class="padding">
					<ul id="sub-navigation">
						<li><a href="#">Accessoires</a></li>
						<li><a href="#">Klanten service</a></li>
						<li><a href="#">Vergelijk fietsen</a></li>
						<li><a href="#">Categorien</a></li>
						<li><a href="#">Over Hans Struijk</a></li>
					</ul>

					<!-- Newsletter -->
					<h3 class="block">Nieuwsbrief</h3>
					<div class="block clearfix newsletter">
						<p>Schrijf je nu in!</p>
						<form action="postback">
							<fieldset>
								<input type="text" name="newsletter" id="newsletter" class="input-newsletter" />
								{% @button text="meld aan" postback="show_growl_newsletter"  %}
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
		</div>
		<div id="footer" class="zp-100">
			<ul class="footer-bike-logos list clearfix">
				<li class="footer-logo1"><span>3T</span></li>
				<li class="footer-logo2"><span>Campagnolo</span></li>
				<li class="footer-logo3"><span>Alexrims</span></li>
				<li class="footer-logo4"><span>tektro</span></li>
				<li class="footer-logo5"><span>AHEADSET</span></li>
				<li class="footer-logo6"><span>Kore</span></li>
				<li class="footer-logo7"><span>Jagwire</span></li>
				<li class="footer-logo8"><span>Magura</span></li>
				<li class="footer-logo1"><span>3T</span></li>
				<li class="footer-logo2"><span>Campagnolo</span></li>
				<li class="footer-logo3"><span>Alexrims</span></li>
				<li class="footer-logo4"><span>tektro</span></li>
				<li class="footer-logo5"><span>AHEADSET</span></li>
				<li class="footer-logo6"><span>Kore</span></li>
				<li class="footer-logo7"><span>Jagwire</span></li>
				<li class="footer-logo8"><span>Magura</span></li>
				<li class="footer-logo1"><span>3T</span></li>
				<li class="footer-logo2"><span>Campagnolo</span></li>
			</ul>
			<div class="footer-text">
				<p><strong>Hans Struijk Fietsen &copy; 2009</strong></p>
				<p>Gebruik van deze site betekent dat u onze algemene voorwaarden accepteert alle prijzen zijn incl. 19% btw.</p>
				<p>Wijzigingen in model, prijs en/of uitvoering voorbehouden.</p>
			</div>
		</div>
	</div>
	
	<script type="text/javascript" src="/lib/js/apps/jquery-1.3.js"></script>
    <script type="text/javascript" src="/lib/js/apps/jquery-ui-all-1.6rc5.min.js"></script>
    <script type="text/javascript" src="/lib/js/apps/zophrenic-1.0.js"></script>
    <script type="text/javascript" src="/lib/js/modules/jquery.notice.js"></script>
    <script type="text/javascript" src="/lib/js/modules/livevalidation-1.3.js"></script>

	{% @script %}
	
</body>
</html>