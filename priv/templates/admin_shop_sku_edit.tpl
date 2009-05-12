{% extends "admin_base.tpl" %}

{% block title %} Admin Sku {{ sku.description1|escape }} {% endblock %}

{% block content %}
{% with m.predicate[id] as p %}
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
									{% for p,v in sku %}
										{% ifnotequal p "tsv" %}
										<tr>
											<td>
												{{ p }}
												&nbsp;
											</td>
											<td>
												{% ifequal p "variant" %}
													<div class="form-item clearfix">
														<input type="text" style="width: 300px" id="field-variant" name="variant" value="{{ v|escape }}" />
													</div>
												{% else %}
													{{ v|escape }}
												{% endifequal %}
											</td>
										</tr>
										{% endifnotequal %}
									{% endfor %}
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
{% endwith %}
{% endblock %}