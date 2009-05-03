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
							<li>{% image m.rsc[rsc_id].media[1].filename width=300 crop alt=m.rsc[rsc_id].media[1].filename class="do_imageviewer" %}</li>
						{% endif %}
				
						{% if m.rsc[rsc_id].media[2].filename %}
							<li>{% image m.rsc[rsc_id].media[2].filename width=300 crop alt=m.rsc[rsc_id].media[2].filename class="do_imageviewer" %}</li>
						{% endif %}
					</ul>
				</div>
			{% endif %}
			
			<div class="product-description zp-50">
				<div id="product-price">
					{% include "_product_price.tpl" %}
				</div>

				{% if m.rsc[rsc_id].body %}
					<h3>{% _ "Description" nl="Omschrijving" %}</h3>
					{{m.rsc[rsc_id].body}}
				{% endif %}
			</div>
		</div>
		
		<h3 class="block">Related products</h3>
		<ul class="compare-list clearfix">
			{% for id in m.search[{featured cat="product"}] %}
				<li class="zp-33 {% ifequal forloop.counter "1" %}first{% endifequal %} {% ifequal forloop.counter "4" %}first{% endifequal %}">
					<div class="block">
						<a href="{{ m.rsc[id].page_url }}">{% image m.rsc[id].media[1].filename width=216 height=70 crop %}</a>
						<h3><a href="{{ m.rsc[id].page_url }}">{{ m.rsc[id].title }}</a></h3>
						<div class="product-price clearfix">
							<h3>&euro;{% include "_price.tpl" %}</h3>
							<div class="clearfix button-wrapper right">
								{% button class="right right-side-button" text="Meer info &raquo;" action={redirect id=id} %}
							</div>
						</div>
					</div>
				</li>
			{% endfor %}
		</ul>
		
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
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">
	
			{% with m.category[m.rsc[rsc_id].category_id] as cat %}
				{% with m.category[cat.parent_id] as parent_cat %}

					<h3 class="block">{{ parent_cat.title }}</h3>
					<ul class="sub-navigation">
						{% for sibling_cat in parent_cat.tree1 %}
					    	<li><a {% ifequal sibling_cat.id cat.id %}class="current" {% endifequal %} href="{% url overview cat=parent_cat.name subcat=sibling_cat.name %}">{{ sibling_cat.title }}</a></li>
						{% endfor %}
					</ul>
			
					{% with m.rsc[rsc_id].brand[1] as brand_id %}
						<h3 class="block">Merken</h3>
						<ul class="sub-navigation">
							<li><a href="{% url overview cat=parent_cat.name subcat=cat.name %}">Alle merken <span class="amount">({{ prod_count|default:"-" }})</span></a></li>
							{% for b_id, b_name, b_count in cat_brand %}
							    <li><a {% ifequal brand_id b_id %}class="current" {% endifequal %} href="{% url overview cat=parent_cat.name subcat=cat.name brand=b_name %}">{{m.rsc[b_id].title}} <span class="amount">({{ b_count|default:"-" }})</span></a></li>
							{% endfor %}
						</ul>
					{% endwith %}
				{% endwith %}
			{% endwith %}
			
			<div class="notification notice {% if cart_count %}viewable{% endif %} " id="product-notice">
				<h3>Winkelmand informatie</h3>
				<a title="Bekijk uw winkelmand" href="{% url shop_cart %}">U heeft de {{ m.rsc[rsc_id].title }} in uw winkelmand &raquo;</a>
			</div>
		</div>
	</div>
{% endblock %}
