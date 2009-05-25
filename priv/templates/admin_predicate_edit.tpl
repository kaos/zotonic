{% extends "admin_base.tpl" %}

{% block title %} Admin Predicate {{ m.predicate[id].name }} {% endblock %}

{% block content %}
{% with m.predicate[id] as p %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

			<p class="admin-chapeau">editing:</p>
			<h2>Predicate “{{ p.title }}”</h2>

			{% wire id="predform" type="submit" postback="predform" %}
			<form id="predform" method="post" action="postback">
				<div class="zp-67" id="poststuff">
					<div class="padding">
						<div class="item-wrapper">
							<h3 class="above-item">Title and description</h3>
							<div class="item">
								<fieldset class="admin-form">
									<input type="hidden" name="id" value="{{ id }}" />

									<div class="form-item clearfix">
										<label for="field-name">Unique name</label>
										<input type="text" id="field-name" name="name" value="{{ p.name }}" />
									</div>

									<div class="form-item clearfix">
										<label for="field-uri">Unique uri</label>
										<input type="text" id="field-uri" name="uri" value="{{ p.uri }}" />
									</div>

									<div class="form-item clearfix">
										<label for="field-title">Title</label>
										<input type="text" id="field-title" name="title" value="{{ p.title }}" />
									</div>
										
									<div class="form-item clearfix">
										<input id="field-reversed" type="checkbox" class="do_fieldreplace" name="reversed" {% if r.reversed %}checked="checked"{% endif %} value="1" />
										<label for="field-reversed">The subject/object of this predicate are reversed from the normal definition.</label>
									</div>

									<div class="form-item clearfix">
										<label for="field-descr">Description</label>
										<textarea rows="6" cols="10" id="field-descr" name="descr" class="intro">{{ p.descr }}</textarea>
									</div>

									<div class="form-item clearfix">
										{% button class="save-resource right" text="save this predicate" %}
										{% button class="discard-resource right" text="cancel" action={redirect back} %}
									</div>
								</fieldset>
							</div>
						</div>

					</div>

					<div class="padding">
						<div class="item-wrapper">
							<h3 class="above-item">Valid between</h3>
							<div class="item">
								<fieldset class="admin-form">
									<p>This predicate can be used between two pages of the following categories. <a href="javascript:void(0)" class="do_dialog {title: 'Help about predicates.', text: 'You can define for which categories the predicate is shown on the edit page.  You can also define which categories of objects will be found when searching for a page to connect to.  When you don\'t check anything then all categories are valid.', width: '450px'}">Need more help?</a></p>
								
									<div class="zp-30">
										<h4>From category</h4>
										<p>
											{% for cat_id, level, indent, title in m.category.all_flat %}
											<label for="{{ #subject.cat_id }}">
												{{ indent }}<input type="checkbox" id="{{ #subject.cat_id }}" name="subject" {% if cat_id|member:p.subject %}checked="checked" {% endif %} value="{{ cat_id }}" />{{ title }}<br/>
											</label>
											{% endfor %}
										</p>
									</div>
									
									<div class="zp-20">
										&nbsp;
									</div>
								
									<div class="zp-30">
										<h4>To category</h4>
										<p>
											{% for cat_id, level, indent, title in m.category.all_flat %}
											<label for="{{ #object.cat_id }}">
												{{ indent }}<input type="checkbox" id="{{ #object.cat_id }}" name="object"  {% if cat_id|member:p.object %}checked="checked" {% endif %} value="{{ cat_id }}" />{{ title }}<br/>
											</label>
											{% endfor %}
										</p>
									</div>

									<div class="zp-20">
										&nbsp;
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
								<span class="title">Publish this predicate</span>
								<span class="arrow">make smaller</span>
							</h3>
							<div class="item clearfix">
								<div class="admin-form ">
									<div class="form-item clearfix">
										{% button class="save-resource do_tooltip" text="save" title="Save this predicate." %}
										
										{% button class="discard-resource right" text="cancel" action={redirect back} %}
										{% button class="discard-resource right" text="delete" action={dialog_predicate_delete id=p.id on_success={redirect back}} %}
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