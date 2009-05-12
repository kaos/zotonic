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
									
									<table>
										<tr>
											<td>Article#</td>
											<td>{{ sku.article_nr|escape }}</td>
										</tr>
										<tr>
											<td>Description 1</td>
											<td>{{ sku.description1|escape }}</td>
										</tr>
										<tr>
											<td>Description 2</td>
											<td>{{ sku.description2|escape }}</td>
										</tr>
										<tr>
											<td>Product group</td>
											<td>{{ sku.product_group|escape|default:"-" }}</td>
										</tr>
										<tr>
											<td>Product page</td>
											<td>
												<a href="{{ m.rsc[sku.rsc_id].page_url }}">{{ m.rsc[sku.rsc_id].title }}</a><br/>
												{% button text="edit product page" action={redirect dispatch="admin_edit_rsc" id=sku.rsc_id} %}
											</td>
										</tr>
										<tr>
											<td>Brand</td>
											<td>{{ sku.brand|escape|default:"-" }}</td>
										</tr>
										<tr>
											<td>Variant</td>
											<td>
												<div class="form-item clearfix">
													<input type="text" style="width: 300px" id="field-variant" name="variant" value="{{ sku.variant|escape }}" />
												</div>
											</td>
										</tr>
										<tr>
											<td></td>
											<td>
												<div class="edit_media left clearfix">
													{% image m.media[sku.media_id].filename width=150 height=150 crop %}
												</div>
											</td>
										</tr>
										<tr>
											<td>Stock available</td>
											<td>{{ sku.stock_avail }}</td>
										</tr>
										<tr>
											<td>Last import</td>
											<td>{{ sku.imported|date:"M d, H:i"}}</td>
										</tr>
										<tr>
											<td>Price incl</td>
											<td>&euro;{{ sku.price_incl|format_price }}</td>
										</tr>
										<tr>
											<td>Price excl</td>
											<td>&euro;{{ sku.price_excl|format_price }}</td>
										</tr>
										<tr>
											<td>Special start</td>
											<td>{{ sku.special_start|date:"Y-m-d" }}</td>
										</tr>
										<tr>
											<td>Special end</td>
											<td>{{ sku.special_end|date:"Y-m-d" }}</td>
										</tr>
										<tr>
											<td>Special price incl</td>
											<td>&euro;{{ sku.special_price_incl|format_price }}</td>
										</tr>
										<tr>
											<td>Special price excl&nbsp;&nbsp;&nbsp;&nbsp;</td>
											<td>&euro;{{ sku.special_price_excl|format_price }}</td>
										</tr>
										<!--
										<tr>
											<td>Removal tax</td>
											<td>&euro;{{ sku.removal_tax|format_price }}</td>
										</tr>
										-->
										<tr>
											<td>Extra 1</td>
											<td>{{ sku.extra1|escape }}</td>
										</tr>
										<tr>
											<td>Extra 2</td>
											<td>{{ sku.extra2|escape }}</td>
										</tr>
										<tr>
											<td>Extra 3</td>
											<td>{{ sku.extra3|escape }}</td>
										</tr>
										<tr>
											<td>Extra 4</td>
											<td>{{ sku.extra4|escape }}</td>
										</tr>
										<tr>
											<td>Extra 5</td>
											<td>{{ sku.extra5|escape }}</td>
										</tr>
										<tr>
											<td>Extra 6</td>
											<td>{{ sku.extra6|escape }}</td>
										</tr>
									</table>

									<div class="form-item clearfix">
										{% button class="save-resource right" text="save this sku" %}
										{% button class="discard-resource right" text="discard" action={redirect back} %}
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