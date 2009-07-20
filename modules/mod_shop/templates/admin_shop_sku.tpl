{% extends "admin_base.tpl" %}

{% block title %} Skus Overview {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

		<h2>Zotonic stock keeping units overview</h2>

		{% with m.search.paged[{shop_sku_list text=q.qs page=q.page}] as result %}

			{% pager result=result dispatch="admin_shop_sku" qargs %}
			
			<h3 class="above-list">Sku's overview</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-10">Article Nr</span>
					<span class="zp-25">Description</span>
					<span class="zp-20">Variant</span>
					<span class="zp-10">Stock</span>
					<span class="zp-10">Price</span>
					<span class="zp-15">Imported on</span>
					<span class="zp-10">Options</span>
				</li>
			{% for sku in result %}
				<li id="{{ #li.id }}">
					<a href="{% url admin_shop_sku_edit id=sku.id %}" class="clearfix">
						<span class="zp-10">{{ sku.article_nr|escape }}</span>
						<span class="zp-25">{{ sku.description1|escape|default:"-" }}</span>
						<span class="zp-20">{{ sku.variant|escape|default:"-" }}</span>
						<span class="zp-10">{{ sku.stock }}</span>
						<span class="zp-10">&euro;{{ sku.price_incl|format_price }}</span>
						<span class="zp-15">{{ sku.imported|date:"d M, H:i" }}</span>
						<span class="zp-10">
							{% button text="edit &raquo;" action={redirect dispatch="admin_shop_sku_edit" id=sku.id} %}
						</span>
					</a>
				</li>
			{% empty %}
				<li>
					No skus found.
				</li>
			{% endfor %}
			</ul>

			{% pager result=result dispatch="admin_shop_sku" qargs %}

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