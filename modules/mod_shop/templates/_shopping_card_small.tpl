<!-- Shopping card -->
<div class="basket-wrapper zp-50 right">
	<div class="basket-user clearfix">
		<p class="basket-user-image"></p>
		<p class="basket-text">Welkom, Jan Janssen <a href="#">Uitloggen&nbsp;&raquo;</a></p>
	</div>
	<div class="basket-basket">
		<p class="basket-basket-image"></p>
		<p class="basket-text"><span id="cart-info"></span> <a href="{% url shop_cart %}">Bekijk uw winkelmand &raquo;</a></p>
		{% shop_cart_small total=shop_cart_total count=shop_cart_count %}
	</div>
	<div class="basket-logos"><span><!-- Logos content -->iDeal, thuiswinkel waarborg</span></div>
</div>