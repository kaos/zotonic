{% extends "base.tpl" %}

{% block title %}{{ m.rsc[brand_id].title }}  {{cat.title}}{% endblock %}

{% block content %}
	<div id="content-area" class="zp-75 category-overview">
		<!-- Area for the main content -->
		<h2>{{ m.rsc[brand_id].title }} {{ cat.title }}</h2>
		<div class="zp-70">
			<ul class="list-cats">
			{% for sub in subcats %}
				<li class="block clearfix">
					<a href="{% ifequal cat.name "product" %}{% url overview cat=sub.name brand=brand_name %}{% else %}{% url overview cat=cat.name subcat=sub.name brand=brand_name %}{% endifequal %}">
						{% image m.category[sub.id].image width=200 height=70 crop alt="bags" class="left" %}
					</a>
					<h3>
						<a href="{% ifequal cat.name "product" %}{% url overview cat=sub.name brand=brand_name %}{% else %}{% url overview cat=cat.name subcat=sub.name brand=brand_name %}{% endifequal %}" title="{{ sub.title }}">
						{{ sub.title }}
						</a>
					</h3>
					<p>
						{{ sub.intro }}
						<a href="{% ifequal cat.name "product" %}{% url overview cat=sub.name brand=brand_name %}{% else %}{% url overview cat=cat.name subcat=sub.name brand=brand_name %}{% endifequal %}">Lees&nbsp;meer&nbsp;&raquo;</a>
					</p>
				</li>
			{% empty %}
				<li class="block clearfix">
					<p>{{cat.title}} heeft geen subcategorieën.</p>
					{% ifequal cat.name "bikes" %}
					<p>{% button action={redirect location="/compare/mtb/1"} text="Vergelijk Fietsen" %}</p>
					{% endifequal %}
				</li>
			{% endfor %}
			</ul>
		</div>
		<div class="category-sidebar zp-30">
			<div class="block clearfix">
				{% if brand_id %}
					<h3>{{ m.rsc[brand_id].title }}</h3>
					{{ m.rsc[brand_id].body }}
				{% endif %}

				<h3>{{ cat.title }}</h3>
				{{ cat.body }}
			</div>
		</div>
	</div>	
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">
			
			<h3 class="block">Merken</h3>
			<ul class="sub-navigation">
				<li><a href="{% url overview cat=cat_name subcat=subcat_name %}">Alle merken <span class="amount">({{ prod_count|default:"-" }})</span></a></li>
				{% for b_id, b_name, b_count in cat_brand %}
			    <li><a {% ifequal brand_id b_id %}class="current" {% endifequal %} href="{% url overview cat=cat_name subcat=subcat_name brand=b_name %}">{{m.rsc[b_id].title}} <span class="amount">({{ b_count|default:"-" }})</span></a></li>
				{% endfor %}
			</ul>
			
			<h3 class="block">Featured products</h3>
			<ul class="related-articles">
				{% for id in featured %}
					<li class="block clearfix">
						<a href="{% url product id=id slug=m.rsc[id].slug %}" title="{{ m.rsc[id].title }}">{% image m.rsc[id].media[1].filename width=60 height=40 crop alt="trapper" %}</a>
						<h3><a href="{% url product id=id %}">{{ m.rsc[id].title }}</a></h3>
						<span class="price">&euro; {% include "_price.tpl" %}</span>
					</li>
				{% empty %}
					<li class="block clearfix">
						<p>{{cat.title}} heeft geen producten</p>
					</li>
				{% endfor %}
			</ul>
		</div>
	</div>
{% endblock %}
