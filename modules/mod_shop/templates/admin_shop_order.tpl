{% extends "admin_base.tpl" %}

{% block title %} Order Overview {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

		<h2>Zotonic Order Overview</h2>

		{% with m.search.paged[{shop_order_list page=q.page}] as result %}

			{% pager result=result dispatch="admin_shop_order" qargs %}
			
			<h3 class="above-list">Orders</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-5">Order Nr</span>
					<span class="zp-10">Status</span>
					<span class="zp-10">Total</span>
					<span class="zp-5">Paid</span>
					<span class="zp-20">Name</span>
					<span class="zp-15">Street</span>
					<span class="zp-15">City</span>
					<span class="zp-5">Country</span>
					<span class="zp-10">Date</span>
					<span class="zp-5">Options</span>
				</li>
			{% for order in result %}
				{% with order.id as id %}
				<li id="{{ #li.id }}">
					<a href="{% url admin_shop_order_view id=id %}" class="clearfix">
						<span class="zp-5">{{ order.id }}</span>
						<span class="zp-10">{{ order.status }}</span>
						<span class="zp-10">&euro;{{ order.total_price_incl|format_price }}</span>
						<span class="zp-5">{{ order.paid|yesno:"paid,not paid"}}</span>
						<span class="zp-20">{{ order.lastname|escape }}</span>
						<span class="zp-15">{{ order.delivery_street|escape }}</span>
						<span class="zp-15">{{ order.delivery_city|escape }}</span>
						<span class="zp-5">{{ order.delivery_country|escape }}</span>
						<span class="zp-10">{{ order.created|date:"M d, H:i" }}</span>
						<span class="zp-5">
							{% button text="view &raquo;" action={redirect dispatch="admin_shop_order_view" id=id} %}
						</span>
					</a>
				</li>
				{% endwith %}
			{% empty %}
				<li>
					No orders found.
				</li>
			{% endfor %}
			</ul>

			{% pager result=result dispatch="admin_shop_order" qargs %}

		{% endwith %}

			{#<form method="get" autocomplete="off">
				<input type="text" value="" name="q" id="q" class="do_listfilter {list: '#posts'}" />
			</form>

			<ul id="posts">
				<li><span class="title">The Well-Designed Web</span></li>
				<li><span class="title">Welcome John Nunemaker</span></li> 
				<li><span class="title">Sidebar Creative: The Next Steps</span></li> 
				<li><span class="title">The Web/Desktop Divide</span></li> 
				<li><span class="title">2007 in Review</span></li> 
				<li><span class="title">Don't Complicate the Solution</span></li> 
				<li><span class="title">Blog to Business</span></li> 
				<li><span class="title">Single Line CSS</span></li> 
				<li><span class="title">Comments Work Again</span></li> 
				<li><span class="title">The iPhone Effect</span></li> 
				<li><span class="title">Greek Blogger Camp</span></li> 
				<li><span class="title">FeedBurner FeedSmith</span></li> 
				<li><span class="title">Download Counter Update 1.3</span></li> 
				<li><span class="title">Branding Reworked</span></li>
			</ul>#}
		</div>
		<div class="push"></div>
	</div>
{% endblock %}