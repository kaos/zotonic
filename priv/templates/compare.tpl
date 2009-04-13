{% extends "base.tpl" %}

{% block title %} Compare bikes {% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<h2>Vergelijk fietsen</h2>
		<ul class="compare-list clearfix">
			<li class="zp-33 first">
				<div class="block">
					<h3>Gazelle Champion Mondial</h3>
					<img src="{% image_url "trek_urban.jpg" width=225 height=230 crop %}" alt="fiets" class="do_imageviewer" />
					<p>verhaaltje</p>
					<div class="product-price clearfix">
						<h3>&euro;899 <span>incl. btw</span></h3>
						{% button text="Bekijk fiets" action={redirect location="/bike/9999/trek-urban"} %}
						<button>Vraag proefrit aan</button>
					</div>
				</div>
			</li>
			<li class="zp-33">
				<div class="block">
					<h3>HEMA tank fiets</h3>
					<img src="{% image_url "trek_urban.jpg" width=225 height=230 crop %}" alt="fiets" class="do_imageviewer" />
					<p>verhaaltje</p>					
					<div class="product-price clearfix">
						<h3>&euro;350 <span>incl. btw</span></h3>
						{% button text="Bekijk fiets" action={redirect location="/bike/9999/trek-urban"} %}
						<button>Vraag proefrit aan</button>
					</div>
				</div>
			</li>
			<li class="zp-33">
				<div class="block">
					<h3>Bianchi road</h3>
					<img src="{% image_url "trek_urban.jpg" width=225 height=230 crop %}" alt="fiets" class="do_imageviewer" />
					<p>verhaaltje</p>					
					<div class="product-price clearfix">
						<h3>&euro;2230 <span>incl. btw</span></h3>
						{% button text="Bekijk fiets" action={redirect location="/bike/9999/trek-urban"} %}
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
			
			<h2>Fietsen</h2>
			<ul id="sub-navigation">
			{% for cat in m.category.bikes.tree1 %}
		    	<li><a href="{% url overview cat=m.category[cat.parent_id].name subcat=cat.name %}">{{ cat.title }}</a></li>
			{% empty %}
				<li>Er zijn geen subcategorieën.</li>
			{% endfor %}
			</ul>
			
 		</div>
	</div>
{% endblock %}