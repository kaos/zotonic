{% extends "admin_base.tpl" %}

{% block title %} admin {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

			<h2>Zophrenic Dashboard</h2>

			<div class="clearfix">
				{% button class="" text="Make a new page" action={dialog_new_rsc title=""} %}
				{% button class="" text="Make a new media item" action={dialog_media_upload title=""} %}
			</div>

			<hr class="clear" />
			
			<div class="zp-50">

				<div class="padding">
					<div id="dashboard-pages">
						<h3 class="above-list">Latest modified texts</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-35">Title</span>
								<span class="zp-25">Category</span>
								<span class="zp-20">Publish date</span>
								<span class="zp-20">Options</span>
							</li>
							
							{% for id in m.search[{latest cat="text"}] %}
							<li>
								<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
									<span class="zp-35">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
									<span class="zp-25">{{ m.rsc[id].category.name }}</span>
									<span class="zp-20">{{ m.rsc[id].modified|date:"F d, H:i" }}</span>
									<span class="zp-20">
										{% button text="view" action={redirect id=id} %}
										{% button text="edit &raquo;" action={redirect dispatch="admin_edit_rsc" id=id} %}
									</span>
								</a>
							</li>
							{% empty %}
							<li>
								No articles
							</li>
							{% endfor %}
						</ul>
					</div>
				</div>

				<div class="padding">
					<div id="dashboard-pages">
						<h3 class="above-list">Latest modified persons</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-35">Title</span>
								<span class="zp-25">Category</span>
								<span class="zp-20">Publish date</span>
								<span class="zp-20">Options</span>
							</li>
							
							{% for id in m.search[{latest cat="person"}] %}
							<li>
								<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
									<span class="zp-35">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
									<span class="zp-25">{{ m.rsc[id].category.name }}</span>
									<span class="zp-20">{{ m.rsc[id].modified|date:"F d, H:i" }}</span>
									<span class="zp-20">
										{% button text="view" action={redirect id=id} %}
										{% button text="edit &raquo;" action={redirect dispatch="admin_edit_rsc" id=id} %}
									</span>
								</a>
							</li>
							{% empty %}
							<li>
								No articles
							</li>
							{% endfor %}
						</ul>
					</div>
				</div>

			</div>

			<div class="zp-50">

				<div class="padding last">
					<div id="dashboard-media">
						<h3 class="above-list">Latest modified media items</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-10">Preview</span>
								<span class="zp-25">Title</span>
								<span class="zp-25">Category</span>
								<span class="zp-20">Publish date</span>
								<span class="zp-20">Options</span>
							</li>
							
							{% for id in m.search[{latest cat="media"}] %}
							<li>
								<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
									<span class="zp-10">{% image id width=40 height=18 crop %}&nbsp;</span>
									
									<span class="zp-25">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
									<span class="zp-25">{{ m.rsc[id].category.name }}</span>
									<span class="zp-20">{{ m.rsc[id].modified|date:"F d, H:i" }}</span>
									<span class="zp-20">
										{% button text="view" action={redirect id=id} %}
										{% button text="edit &raquo;" action={redirect dispatch="admin_edit_rsc" id=id} %}
									</span>
								</a>
							</li>
							{% empty %}
							<li>
								No media found.
							</li>
							{% endfor %}
						</ul>
					</div>
				</div>

				<div class="padding">
					<div id="dashboard-pages">
						<h3 class="above-list">Latest modified locations</h3>
						<ul class="short-list">
							<li class="headers clearfix">
								<span class="zp-35">Title</span>
								<span class="zp-25">Category</span>
								<span class="zp-20">Publish date</span>
								<span class="zp-20">Options</span>
							</li>
							
							{% for id in m.search[{latest cat="location"}] %}
							<li>
								<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
									<span class="zp-35">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
									<span class="zp-25">{{ m.rsc[id].category.name }}</span>
									<span class="zp-20">{{ m.rsc[id].modified|date:"F d, H:i" }}</span>
									<span class="zp-20">
										{% button text="view" action={redirect id=id} %}
										{% button text="edit &raquo;" action={redirect dispatch="admin_edit_rsc" id=id} %}
									</span>
								</a>
							</li>
							{% empty %}
							<li>
								No articles
							</li>
							{% endfor %}
						</ul>
					</div>
				</div>

			</div>

		</div>
		<div class="push"></div>
	</div>
{% endblock %}