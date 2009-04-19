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
						<a class="button" href="/admin/overview">manage pages &raquo;</a>
						<a class="button" href="/pages">manage categories &raquo;</a>
						<a class="button" href="/pages">manage prodcuts &raquo;</a>
						<a class="button" href="/pages">manage users &raquo;</a>
					</div>
				</div>
			</div>

			<div class="zp-50">
				<div class="padding">
					<h3 class="alt">Quick search</h3>

					<div  id="quick-search">	
						<form method="get" action="">
							<fieldset>
								<div class="form-element">
									<input type="text" name="q" value="" class="left" />
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
						<h3 class="above-list">Last modified articles</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-35">Title</span>
								<span class="zp-25">Category</span>
								<span class="zp-25">Publish date</span>
								<span class="zp-15">Options</span>
							</li>
							{% for id in m.search[{latest cat="article"}] %}
							<li>
								<a href="#" class="clearfix">
									<span class="zp-35">{{ m.rsc[id].title }}</span>
									<span class="zp-25">{{ m.rsc[id].category.name }}</span>
									<span class="zp-25">{{ m.rsc[id].modified|date:"F d, H:i" }}</span>
									<span class="zp-15">
										{% button text="view" action={redirect id=id} %}
										{% button text="edit" action={redirect dispatch="admin_edit_rsc" rsc=id} %}
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
					
					<div id="dashboard-pages">
						<h3 class="above-list">Last modified products</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-35">Title</span>
								<span class="zp-25">Category</span>
								<span class="zp-25">Publish date</span>
								<span class="zp-15">Options</span>
							</li>
							{% for id in m.search[{latest cat="product"}] %}
							<li>
								<a href="#" class="clearfix">
									<span class="zp-35">{{ m.rsc[id].title }}</span>
									<span class="zp-25">{{ m.rsc[id].category.name }}</span>
									<span class="zp-25">{{ m.rsc[id].modified|date:"F d, H:i" }}</span>
									<span class="zp-15">
										{% button text="view" action={redirect id=id} %}
										{% button text="edit" action={redirect dispatch="admin_edit_rsc" rsc=id} %}
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
					<div id="dashboard-products">
						<h3 class="above-list">Best selling products in the last two weeks</h3>
						{% chart_pie
								data=[ ['Tacx Cycle Motion Stand',10], ['Duracell AA Plus',30], ['Ortlieb Mud Racer XS',15], ['Shimano v-brake',15], [' Rudy Project Jekyll', 30]]
								colors='0D8BB1' width=581 height=150 %}
					</div>

					<div id="dashboard-orders">
						<h3 class="above-list">Latest sold products</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-33">Item(s)</span>
								<span class="zp-33">Price</span>
								<span class="zp-33">Date</span>
							</li>
							<li>
								<a href="#" class="clearfix">
									<span class="zp-33">Shimano v-brake</span>
									<span class="zp-33">&euro; 22,-</span>
									<span class="zp-33">March 15, 22:56</span>
								</a>
							</li>
							<li>
								<a href="#" class="clearfix">
									<span class="zp-33">Shimano v-brake</span>
									<span class="zp-33">&euro; 22,-</span>
									<span class="zp-33">March 15, 22:58</span>
								</a>
							</li>
							<li>
								<a href="#" class="clearfix">
									<span class="zp-33">Shimano v-brake</span>
									<span class="zp-33">&euro; 22,-</span>
									<span class="zp-33">March 15, 22:56</span>
								</a>
							</li>
							<li>
								<a href="#" class="clearfix">
									<span class="zp-33">Shimano v-brake</span>
									<span class="zp-33">&euro; 22,-</span>
									<span class="zp-33">March 15, 22:58</span>
								</a>
							</li>
						</ul>
					</div>
				</div>
			</div>
		</div>
		<div class="push"></div>
	</div>
{% endblock %}