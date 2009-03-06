{% extends "base.tpl" %}

{# comment #}

{% block title %} Compare bikes {% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<h2>Vergelijk fietsen</h2>
		<ul class="compare-list clearfix">
			<li class="zp-33 first">
				<div class="block">
					<h3>Gazelle Champion Mondial</h3>
					<img src="/image/trek_urban.jpg(225x230)(crop)(urban).jpg" alt="" />
					<p>verhaaltje</p>
					
					<div class="product-price clearfix">
						<h3>&euro;899 <span>incl. btw</span></h3>
						<button>Bekijk fiets</button>
						<button>Vraag proefrit aan</button>
					</div>
				</div>
			</li>
			<li class="zp-33">
				<div class="block">
					<h3>HEMA tank fiets</h3>
					<img src="/image/trek_urban.jpg(225x230)(crop)(urban).jpg" alt="Vallentia" />
					<p>verhaaltje</p>
					
					<div class="product-price clearfix">
						<h3>&euro;350 <span>incl. btw</span></h3>
						<button>Bekijk fiets</button>
						<button>Vraag proefrit aan</button>
					</div>
				</div>
			</li>
			<li class="zp-33">
				<div class="block">
					<h3>Bianchi road</h3>
					<img src="/image/trek_urban.jpg(225x230)(crop)(urban).jpg" alt="Vallentia" />
					<p>verhaaltje</p>
					
					<div class="product-price clearfix">
						<h3>&euro;2230 <span>incl. btw</span></h3>
						<button>Bekijk fiets</button>
						<button>Vraag proefrit aan</button>
					</div>
				</div>
			</li>
		</ul>	
	</div>
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">
			
			{% include "_subnav.tpl" %}
			
 		</div>
	</div>
{% endblock %}