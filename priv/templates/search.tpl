{% extends "base.tpl" %}

{% block title %}Search{% endblock %}

{% block content %}
	<div id="content-area" class="zp-75">
		<!-- Area for the main content -->
		<h2 class="header-alone">
			{% if q.qs %}
				U heeft gezocht op <em>{{ q.qs|escape }}</em>{% if cat_id %}, categorie <em>{{ m.category[cat_id].title }}</em>{% endif %}{% if brand_id %}, merk <em>{{ m.rsc[brand_id].title }}</em>{% endif %}
			{% else %}
				Zoek resultaat
			{% endif %}
		</h2>
		
		{% pager result=result %}
		
		{% for rsc_id, cid, bid, rank in result %}
		<h3 class="block">{{ m.rsc[rsc_id].title }}</h3>
		<div class="block clearfix">
			<a href="{{ m.rsc[rsc_id].page_url }}">{% image m.rsc[rsc_id].media[1].filename width=180 height=140 crop alt=m.rsc[rsc_id].title class="left" %}</a>
			<div class="zp-70">
				<h4>Beschrijving</h4>
				<p>{{ m.rsc[rsc_id].intro }}</p>

				{% with m.shop_product[rsc_id].price as p %}
				<div class="product-price clearfix clear">
					<h3>
						{% if p.is_variant %}<span>vanaf</span> {% endif %}
						&euro;{{ p.price|format_price }}
						{% if p.old_price %}<span class="old-price">{{ p.old_price|format_price }}</span>{% endif %}
					</h3>
					<div class="clearfix button-wrapper right">
					{% if p.is_variant %}
						{% button class="buy-me right-side-button" text="bekijk product &raquo;" action={redirect id=rsc_id} %}
					{% else %}
						{% button class="buy-me" text="bekijk product" action={redirect id=rsc_id} %}
						{% button class="buy-me right-side-button" text="koop direct &raquo;" action={shop_buynow id=rsc_id} %}
					{% endif %}
					</div>
				</div>
				{% endwith %}

{#
				<div class="product-price clearfix">
					<h3>&euro;{% include "_price.tpl" id=rsc_id %} <!--span>incl. btw</span--></h3>

					<div class="clearfix button-wrapper right">
						{% button class="buy-me right-side-button" text="Bekijk product &raquo;" action={redirect id=rsc_id} %}
					</div>
				</div>
#}
			</div>
		</div>
		{% empty %}
		<div class="block clearfix">
			{% if q.qs %}
				<h4>
					Helaas niets gevonden bij uw zoekvraag voor <em>{{ q.qs|default:"niets"|escape }}</em>
					{% if cat_id %}in de categorie <em>{{ m.category[cat_id].title }}</em>{% endif %}
					{% if brand_id %} en het merk <em>{{ m.rsc[brand_id].title }}</em>{% endif %}
					</h4>
			{% else %}
				<h4>Vul uw zoekvraag in en probeer opnieuw.</em></h4>
			{% endif %}
		</div>

		<h3 class="block">Kijk ook eens naar</h3>
		<ul class="compare-list clearfix">
			{% for id in m.search[{featured cat="product"}] %}
				<li class="zp-33 {% ifequal forloop.counter "1" %}first{% endifequal %} {% ifequal forloop.counter "4" %}first{% endifequal %}">
					<div class="block">
						<a href="{{ m.rsc[id].page_url }}">{% image m.rsc[id].media[1].filename width=216 height=130 crop %}</a>
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
			
		{% endfor %}

		{% pager result=result %}
	</div>
{% endblock %}

{% block sidebar %}
	<div id="sidebar" class="zp-25">
		<div class="padding">
			{% include "_subnav.tpl" %}

			<h3 class="block">Categorieën</h3>
			<ul class="sub-navigation">
				<li><a href="{% url search qs=q.qs qbrand=q.qbrand %}">Alle categorieën <span class="amount">({{ total|default:"-" }})</span></a></li>
				{% for c in m.category.product.tree2.children %}
					{% if cat_count[c.id] %}
						<li>
							<a {% ifequal cat_id c.id %}class="current" {% endifequal %} href="{% url search qs=q.qs qcat=c.name qbrand=q.qbrand %}">{{ c.title }} <span class="amount">({{ cat_count[c.id] }})</span></a>
							{% if c.children %}
								<ul class="sub-navigation">
								{% for cc in c.children %}
									{% if cat_count[cc.id] %}
									<li><a {% ifequal cat_id cc.id %}class="current" {% endifequal %} href="{% url search qs=q.qs qcat=cc.name qbrand=q.qbrand %}">&nbsp;&nbsp;&nbsp;&nbsp;{{ cc.title }} <span class="amount">({{ cat_count[cc.id] }})</span></a></li>
									{% endif %}
								{% endfor %}
								</ul>
							{% endif %}
						</li>
					{% endif %}
				{% endfor %}
			</ul>

			<h3 class="block">Merken</h3>
			<ul class="sub-navigation">
				<li><a href="{% url search qs=q.qs qcat=q.qcat %}">Alle merken <span class="amount">({{ total|default:"-" }})</span></a></li>
				{% for b_id, b_name, b_count in brand_count %}
			    <li><a {% ifequal brand_id b_id %}class="current" {% endifequal %} href="{% url search qs=q.qs qcat=q.qcat qbrand=b_name %}">{{m.rsc[b_id].title}} <span class="amount">({{ b_count|default:"-" }})</span></a></li>
				{% endfor %}
			</ul>

		</div>
	</div>
{% endblock %}