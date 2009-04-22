{% with m.shop_product[id].price as p %}
	{{ p.price|format_price }} {% if p.old_price %}<span class="old-price">{{ p.old_price|format_price }}</span>{% endif %}
{% endwith %}
