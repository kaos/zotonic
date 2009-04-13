{% extends "base.tpl" %}

{% block title %}{{cat.title}}{% endblock %}

{% block content %}
	<div id="content-area" class="zp-75 category-overview">
		<!-- Area for the main content -->
		<h2>{{ cat.title }}</h2>
		<ul class="zp-67 subcategory-list">
		{% for sub in m.category[cat_id].tree1 %}
			<li class="block clearfix">
				<a href="{% url overview cat=cat.name subcat=sub.name %}">
					{% image m.category[sub.id].image width=200 height=70 crop alt="bags" class="left" %}
				</a>
				<h3><a href="{% url overview cat=cat.name subcat=sub.name %}">{{ sub.title }}</a></h3>
				<p>
					{{ sub.intro }}
					<a href="{% url overview cat=cat.name subcat=sub.name %}">Lees&nbsp;meer&nbsp;&raquo;</a>
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
	
		<div class="category-sidebar zp-33">
			<div class="block clearfix">
				<h3>{{ cat.title }}</h3>
				{{ cat.body }}
			</div>
		</div>
	</div>	
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">
			
			<h3 class="block">Brands</h3>
			<ul id="sub-navigation">
			    <li><a href="#">Stevens <span class="amount">(3)<span></a></li>
			    <li><a href="#">Ortliep <span class="amount">(5)<span></a></li>
			    <li><a href="#">Sky is pancaces <span class="amount">(2)<span></a></li>
			    <li><a href="#">Marc <span class="amount">(13)<span></a></li>
			</ul>
			
			<h3 class="block">Featured products</h3>
			<ul class="related-articles">
				{% for id in featured %}
					<li class="block clearfix">
						<a href="{% url product id=id slug=m.rsc[id].slug %}">{% image m.rsc[id].media[1].filename width=67 height=50 crop alt="trapper" %}</a>
						<h4><a href="{% url product id=id %}"></a></h4>
						<p>{{ m.rsc[id].title }}</p> 
						<p><a href="#">Bestel snel &raquo;</a></p>
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
