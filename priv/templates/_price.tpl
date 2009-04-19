{# Show a price or discounted price, parameters:  price, special_price #}
{% if special_price %}
	{{ special_price|format_price }} <span style="text-decoration: line-through">{{ price|format_price }}</span>
{% else %}
	{{ price|format_price }}
{% endif %}
