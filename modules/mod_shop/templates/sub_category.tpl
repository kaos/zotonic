{% extends "base.tpl" %}

{% block title %}{{ m.rsc[brand_id].title }} {{ m.category[cat_id].title }}{% endblock %}

{% block content %}
	<div id="content-area" class="zp-75 category-overview">
		<!-- Area for the main content -->
		<h2>{{ m.rsc[brand_id].title }} {{ m.category[cat_id].title }}</h2>
		{% if m.rsc[cat_id].body %}
		<div class="block clearfix">
			{% if m.rsc[cat_id].intro %}
			<p class="intro">{{ m.category[cat_id].intro }}</p>
			{% endif %}

			{% if m.rsc[cat_id].body %}
				{{ m.category[cat_id].body }}
			{% endif %}
		</div>
		{% endif %}
		
		<ul class="subcategory-list clearfix">
			{% for id in products %}
				<li class="zp-50 {% cycle "first" "last" %}">
					<div class="block clearfix">
						<a href="{% url product id=id slug=m.rsc[id].slug %}">
							{% image m.rsc[id].depiction.filename width=140 height=80 crop alt=m.rsc[id].title class="left" %}
						</a>
						<h3>
							<a href="{% url product id=id slug=m.rsc[id].slug %}">{{ m.rsc[id].title }}</a>
						</h3>
						<p>
						{% if m.rsc[id].intro %}
							{{ m.rsc[id].intro|ljust:40 }}...
						{% else %}
							{{ m.rsc[id].body|striptags|ljust:40 }}...
						{% endif %}
						</p>
						
						{% with m.shop_product[id].price as p %}
						<div class="product-price clearfix clear">
							<h3>
								{% if p.is_variant %}<span>vanaf</span> {% endif %}
								&euro;{{ p.price|format_price }}
								{% if p.old_price %}<span class="old-price">{{ p.old_price|format_price }}</span>{% endif %}
							</h3>
							<div class="clearfix button-wrapper right">
							{% if p.is_variant %}
								{% button class="buy-me right-side-button" text="Meer info &raquo;" action={redirect id=id} %}
							{% else %}
								{% button class="buy-me" text="Meer info" action={redirect id=id} %}
								{% button class="buy-me right-side-button" text="Koop direct &raquo;" action={shop_buynow id=id} %}
							{% endif %}
							</div>
						</div>
						{% endwith %}
					</div>
				</li>
				{% empty %}
					<li class="zp-100">
						<p>Er zijn geen producten in deze categorie.</p>
					</li>
			{% endfor %}
		</ul>
	</div>	
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">
			
			<h3 class="block">{{ m.category[m.category[cat_id].parent_id].title }}</h3>
			<ul class="sub-navigation">
				{% for cat in m.category[m.category[cat_id].parent_id].tree1 %}
			    	<li><a {% ifequal cat.id cat_id %}class="current" {% endifequal %} href="{% url overview cat=m.category[cat.parent_id].name subcat=cat.name %}">{{ cat.title }}</a></li>
				{% endfor %}
			</ul>
			
			<h3 class="block">Merken</h3>
			<ul class="sub-navigation">
				<li><a href="{% url overview cat=cat_name subcat=subcat_name %}">Alle merken <span class="amount">({{ prod_count|default:"-" }})</span></a></li>
				{% for b_id, b_name, b_count in cat_brand %}
			    <li><a {% ifequal brand_id b_id %}class="current" {% endifequal %} href="{% url overview cat=cat_name subcat=subcat_name brand=b_name %}">{{m.rsc[b_id].title}} <span class="amount">({{ b_count|default:"-" }})</span></a></li>
				{% endfor %}
			</ul>
		</div>
	</div>
{% endblock %}
