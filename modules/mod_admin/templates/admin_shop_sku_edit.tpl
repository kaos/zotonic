{% extends "admin_base.tpl" %}

{% block title %} Admin Sku {{ sku.description1|escape }} {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

			<p class="admin-chapeau">editing:</p>
			<h2>Sku “{{ sku.description1|escape }} / {{ sku.variant|escape|default:"-" }}”</h2>

			{% wire id="skuform" type="submit" postback="skuform" %}
			<form id="skuform" method="post" action="postback">
				<div class="zp-67" id="poststuff">
					<div class="padding">
						<div class="item-wrapper">
							<h3 class="above-item">All imported data</h3>
							<div class="item">
								<fieldset class="admin-form">
									<input type="hidden" name="id" value="{{ sku.id }}" />
									
									<ul class="table-list">
										<li class="clearfix item-even">
											<span class="label">Article nr.</span>
											<span>{{ sku.article_nr|escape }}</span>
										</li>
										<li class="clearfix item-uneven">
											<span class="label">Description 1</span>
											<span>{{ sku.description1|escape }}</span>
										</li>
										<li class="clearfix item-even">
											<span class="label">Description 2</span>
											<span>{{ sku.description2|escape }}</span>
										</li>
										<li class="clearfix item-uneven">
											<span class="label">Product group</span>
											<span>{{ sku.product_group|escape|default:"-" }}</span>
										</li>
										<li class="clearfix item-even">
											<span class="label">Product page</span>
											<span>
												<a href="{{ m.rsc[sku.rsc_id].page_url }}">
													{{ m.rsc[sku.rsc_id].title }}
												</a>
												{#{% button text="edit product page" action={redirect dispatch="admin_edit_rsc" id=sku.rsc_id} %}#}
											</span>
										</li>
										<li class="clearfix item-uneven">
											<span class="label">Brand</span>
											<span>{{ sku.brand|escape|default:"-" }}</span>
										</li>
										<li class="clearfix item-even">
											<span class="label">Variant</span>
											<span><input type="text" style="width: 150px" id="field-variant" name="variant" value="{{ sku.variant|escape }}" /></span>
										</li>
										<li class="clearfix item-uneven">
											<span class="label">Stock</span>
											<span>{{ sku.stock_avail }}</span>
										</li>
										<li class="clearfix item-even">
											<span class="label">Last imported</span>
											<span>{{ sku.imported|date:"M d, H:i" }}</span>
										</li>
										<li class="clearfix item-uneven">
											<span class="label">Price incl</span>
											<span>&euro;{{ sku.price_incl|format_price }}</span>
										</li>
										<li class="clearfix item-even">
											<span class="label">Price excl</span>
											<span>&euro;{{ sku.price_excl|format_price }}</span>
										</li>
										<li class="clearfix item-uneven">
											<span class="label">Special start</span>
											<span>{{ sku.special_start|date:"Y-m-d" }}</span>
										</li>
										<li class="clearfix item-even">
											<span class="label">Special end</span>
											<span>{{ sku.special_end|date:"Y-m-d" }}</span>
										</li>
										<li class="clearfix item-uneven">
											<span class="label">Special price incl</span>
											<span>&euro;{{ sku.special_price_incl|format_price }}</span>
										</li>
										<li class="clearfix item-even">
											<span class="label">Special price excl</span>
											<span>&euro;{{ sku.special_price_excl|format_price }}</span>
										</li>
									</ul>

									<div class="form-item clearfix">
										{% button class="save-resource right" text="save this sku" %}
										{% button class="discard-resource right" text="cancel" action={redirect back} %}
									</div>
								</fieldset>
							</div>
						</div>

					</div>
				</div>

				<div class="zp-33" id="sidebar">
					<div class="padding" id="sort">
					
						<div class="item-wrapper" id="sort-publish">
							<h3 class="above-item clearfix do_blockminifier">
								<span class="title">Publish this Sku</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								<div class="admin-form ">
									<div class="form-item clearfix">
										{% button class="save-resource do_tooltip" text="save" title="Save this sku." %}
										{% button class="discard-resource right" text="cancel" action={redirect back} %}
									</div>
								</div>
							</div>
						</div>
					
					</div>
				</div>
			</form>
		</div>
		<div class="push"></div>
{% endblock %}