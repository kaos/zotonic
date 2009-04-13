{% extends "base.tpl" %}

{% block title %} {{m.rsc[rsc_id].title}} {% endblock %}

{% block content %} 	
	<div id="content-area" class="zp-75">
		<!-- Area for the main content -->
		<h2>{{m.rsc[rsc_id].title}}</h2>
		<div class="product-wrapper clearfix block">
			{% if m.rsc[rsc_id].media %}
			
			<div class="product-images zp-50">
				<ul class="clearfix">
					{% if m.rsc[rsc_id].media[1].filename %}
						<li>
							{% image m.rsc[rsc_id].media[1].filename width=300 crop alt=m.rsc[rsc_id].media[1].filename class="do_imageviewer" %}
						</li>
					{% endif %}
				
					{% if m.rsc[rsc_id].media[2].filename %}
						<li>
							{% image m.rsc[rsc_id].media[2].filename width=300 crop alt=m.rsc[rsc_id].media[2].filename class="do_imageviewer" %}
						</li>
					{% endif %}
				</ul>
			</div>
			
			{% endif %}
			
			<div class="product-description zp-50">
				<div class="product-price clearfix">
					<h3>&euro;{{m.rsc[rsc_id].price|format_price}} <!--span>incl. btw</span--></h3>
					{# An animate would be nice here #}
					<div class="clearfix button-wrapper right">
						{% button id="product-add-basket" class="buy-me" text="leg in winkelmand" %}
						{% button id="product-buy-basket" class="buy-me right-side-button" text="koop direct &raquo;" %}
					</div>
				</div>
				
				{% if m.rsc[rsc_id].body %}
				<h3>{% _ "Description" nl="Omschrijving" %}</h3>
				
				{{m.rsc[rsc_id].body}}
				
				{% endif %}
			
				{#<div class="product-properties">
					<h3>{% _ "Properties" nl="Eigenschappen" %}</h3>
				
					{% for prop_group in m.rsc[rsc_id].prop %}
						<h4>{{prop_group.group}}</h4>
						<ul class="props-list">
						{% for test in prop_group.props %}
							<li class="clearfix">
								<span class="prop-title zp-50">{{test.title}}</span>
								<span class="prop-value zp-50">{{test.value}}</span>
							</li>
						{% endfor %}
						</ul>
					{% endfor %}
				</div>#}
			</div>
		</div>
		
		{#<div class="reviews-title block clearfix">
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
		</ul>#}
		
		<h3 class="block">Related products</h3>
		<ul class="compare-list clearfix">
			<li class="zp-33 first">
				<div class="block">
					<h3>Gazelle Champion Mondial</h3>
					<img src="{% image_url "trek_urban.jpg" width=225 height=130 crop %}" alt="fiets" class="do_imageviewer" />
					<p>verhaaltje</p>
					<div class="product-price clearfix">
						<h3>&euro;{{m.rsc[rsc_id].price|format_price}} <!--span>incl. btw</span--></h3>
						{# An animate would be nice here #}
						<div class="clearfix button-wrapper right">
							{% button class="right right-side-button" text="Meer info &raquo;" action={redirect location="/bike/9999/trek-urban"} %}
						</div>
					</div>
				</div>
			</li>
			<li class="zp-33">
				<div class="block">
					<h3>HEMA tank fiets</h3>
					<img src="{% image_url "trek_urban.jpg" width=225 height=130 crop %}" alt="fiets" class="do_imageviewer" />
					<p>verhaaltje</p>
					<div class="product-price clearfix">
						<h3>&euro;350</h3>
						{% button class="right right-side-button" text="Meer info &raquo;" action={redirect location="/bike/9999/trek-urban"} %}
					</div>
				</div>
			</li>
			<li class="zp-33">
				<div class="block">
					<h3>Bianchi road</h3>
					<img src="{% image_url "trek_urban.jpg" width=225 height=130 crop %}" alt="fiets" class="do_imageviewer" />
					<p>verhaaltje</p>
					<div class="product-price clearfix">
						<h3>&euro;2230</h3>
						{% button class="right right-side-button" text="Meer info &raquo;" action={redirect location="/bike/9999/trek-urban"} %}
					</div>
				</div>
			</li>
		</ul>
	</div>
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">

			<h3 class="block">{{ m.category[m.category[m.rsc[rsc_id].category_id].parent_id].title }}</h3>
			<ul id="sub-navigation">
				{% for cat in m.category[m.category[m.rsc[rsc_id].category_id].parent_id].tree1 %}
			    	<li><a {% ifequal cat.id m.rsc[rsc_id].category_id %}class="current" {% endifequal %} href="{% url overview cat=m.category[cat.parent_id].name subcat=cat.name %}">{{ cat.title }}</a></li>
				{% endfor %}
			</ul>
			
			<h3 class="block">Brands</h3>
			<ul id="sub-navigation">
			    <li><a href="#">Stevens <span class="amount">(3)<span></a></li>
			    <li><a href="#">Ortliep <span class="amount">(5)<span></a></li>
			    <li><a href="#">Duracell <span class="amount">(2)<span></a></li>
			</ul>
			
			<div class="notification notice" id="product-notice">
				<h3>Winkelmand informatie</h3>
				U heeft de Shimano 105 ST-5600 <strong>2 keer</strong> in uw <a href="#">winkelmand.</a>
			</div>
		</div>
	</div>
{% endblock %}