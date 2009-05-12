{% extends "admin_base.tpl" %}

{% block title %} View Order {{ id|escape }} {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
		<div class="block clearfix">
		
			<p class="admin-chapeau">showing:</p>

			{% if not order %}

				<div class="zp-80">
					<h2>There is no order with id {{ id|escape }}</h2>
				</div>	
				<div class="zp-20">
					<form method="get" action="{% url admin_shop_order_view %}">
						<fieldset>
							<div class="form-element">
								<input type="text" name="id" value="{{ q.id|escape }}" class="left" />
								<button>Show</button>
							</div>
						</fieldset>
					</form>
				</div>
			
				<p>{% button class="discard-resource" text="back" action={redirect back} %}</p>

			{% else %}

				<div class="zp-80">
					<h2>Order #{{ order.id }} by {{ order.lastname|escape|default:"-" }}</h2>
				</div>	
				<div class="zp-20">
					<form method="get" action="{% url admin_shop_order_view %}">
						<fieldset>
							<div class="form-element">
								<input type="text" name="id" value="{{ q.id|escape }}" class="left" />
								<button>Show</button>
							</div>
						</fieldset>
					</form>
				</div>
		
				<div class="zp-67" id="poststuff">
					<div class="padding">
						<div class="item-wrapper">
							<h3 class="above-item">Overview of the order</h3>
							<div class="item">
								{% include "_shop_order_view.tpl" order=order %}
							</div>
						</div>

					</div>
				</div>
			
				<div class="zp-33" id="sidebar">
					<div class="padding" id="sort">
				
						<div class="item-wrapper" id="sort-publish">
							<h3 class="above-item clearfix do_blockminifier">
								<span class="title">Close order</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								<div class="admin-form ">
									<div class="form-item clearfix">
										{# {% button class="save-resource do_tooltip" text="save" title="Save this sku." %} #}
										{% button class="discard-resource right" text="close" action={redirect back} %}
									</div>
								</div>
							</div>
						</div>
				
					</div>
				{% endif %}
			</div>
		</div>
		<div class="push"></div>
{% endblock %}