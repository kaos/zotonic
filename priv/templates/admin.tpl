{% extends "admin_base.tpl" %}

{% block title %} admin {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

			<h2>Zophrenic Dashboard</h2>

			<div class="zp-50">
				<div class="padding">
					<h3 class="alt">Quick navigation</h3>

					<div class="clearfix">
						<a class="button" href="{% url admin_overview_rsc %}">manage pages &raquo;</a>
						<a class="button" href="#">manage categories &raquo;</a>
						<a class="button" href="{% url admin_shop_sku %}">manage skus &raquo;</a>
						<a class="button" href="#">manage users &raquo;</a>
						
						{% button class="right" text="new page..." action={dialog_new_rsc title=""} %}
					</div>
				</div>
			</div>

			<div class="zp-50">
				<div class="padding">
					<h3 class="alt">Quick search</h3>

					<div  id="quick-search">	
						<form method="get" action="{% url admin_overview_rsc %}">
							<fieldset>
								<div class="form-element">
									<input type="text" name="qs" value="{{ q.qs|escape }}" class="left" />
									<button>Search</button>
								</div>
							</fieldset>
						</form>
					</div>	
				</div>
			</div>

			<hr class="clear" />
			
			<div class="zp-50">
				<div class="padding">
					<div id="dashboard-pages">
						<h3 class="above-list">Latest modified articles</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-35">Title</span>
								<span class="zp-25">Category</span>
								<span class="zp-25">Publish date</span>
								<span class="zp-15">Options</span>
							</li>
							{% for id in m.search[{latest cat="article"}] %}
							<li>
								<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
									<span class="zp-35">{{ m.rsc[id].title }}</span>
									<span class="zp-25">{{ m.rsc[id].category.name }}</span>
									<span class="zp-20">{{ m.rsc[id].modified|date:"F d, H:i" }}</span>
									<span class="zp-20">
										{% button text="view" action={redirect id=id} %}
										{% button text="edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
									</span>
								</a>
							</li>
							{% empty %}
							<li>
								No articles
							</li>
							{% endfor %}
						</ul>
					</div>
				
					<div id="dashboard-products">
						<h3 class="above-list">Latest modified products</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-35">Title</span>
								<span class="zp-25">Category</span>
								<span class="zp-20">Publish date</span>
								<span class="zp-20">Options</span>
							</li>
							{% for id in m.search[{latest cat="product"}] %}
							<li>
								<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
									<span class="zp-35">{{ m.rsc[id].title }}</span>
									<span class="zp-25">{{ m.rsc[id].category.name }}</span>
									<span class="zp-20">{{ m.rsc[id].modified|date:"F d, H:i" }}</span>
									<span class="zp-20">
										{% button text="view" action={redirect id=id} %}
										{% button text="edit" action={redirect dispatch="admin_edit_rsc" id=id} %}
									</span>
								</a>
							</li>
							{% empty %}
							<li>
								No products
							</li>
							{% endfor %}
						</ul>
					</div>
				</div>
			</div>

			<div class="zp-50">
				<div class="padding last">
					<div id="dashboard-selling-products">
						<h3 class="above-list">Best selling products in the last two weeks</h3>
						{% chart_pie
								data=m.search[{shop_best_selling}].result
								colors='0D8BB1' width=580 height=150 %}
					</div>

					<div id="dashboard-orders">
						<h3 class="above-list">Latest sold products</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-35">Product</span>
								<span class="zp-25">Variant</span>
								<span class="zp-20">Price</span>
								<span class="zp-20">Date</span>
							</li>
							{% for id, variant, price, date in m.search[{shop_latest_sold}] %}
								<li>
									<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
										<span class="zp-35">{{ m.rsc[id].title }}</span>
										<span class="zp-25">{{ variant|default:"-" }}</span>
										<span class="zp-20">&euro;{{ price|format_price }}</span>
										<span class="zp-20">{{ date|date:"F d, H:i" }}</span>
									</a>
								</li>
							{% empty %}
								<li>Nothing sold yet.</li>
							{% endfor %}
						</ul>
					</div>
				</div>
			</div>
		</div>
		<div class="push"></div>
{% endblock %}