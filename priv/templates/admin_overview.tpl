{% extends "admin_base.tpl" %}

{% block title %} admin overview {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

			<h2>Zophrenic Overview</h2>

			<h3 class="above-list">Pagelist</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-30">Title</span>
					<span class="zp-15">Category</span>
					<span class="zp-15">Publish date</span>
					<span class="zp-15">Create date</span>
					<span class="zp-15">Created by</span>
					<span class="zp-10">Options</span>
				</li>
				<li>
					<a href="#" class="clearfix">
						<span class="zp-30">Home</span>
						<span class="zp-15">Struijk</span>
						<span class="zp-15">March 10, 19:33</span>
						<span class="zp-15">March 8, 18:43</span>
						<span class="zp-15">Tim Benniks</span>
						<span class="zp-10">
							{% button text="delete" %}
							{% button text="edit &raquo;" action={redirect location="/admin/edit/2"} %}
						</span>
					</a>
				</li>
				<li>
					<a href="#" class="clearfix">
						<span class="zp-30">Home</span>
						<span class="zp-15">Struijk</span>
						<span class="zp-15">March 10, 19:33</span>
						<span class="zp-15">March 8, 18:43</span>
						<span class="zp-15">Tim Benniks</span>
						<span class="zp-10">
							{% button text="delete" %}
							{% button text="edit &raquo;" action={redirect location="/admin/edit/2"} %}
						</span>
					</a>
				</li>
				<li>
					<a href="#" class="clearfix">
						<span class="zp-30">Home</span>
						<span class="zp-15">Struijk</span>
						<span class="zp-15">March 10, 19:33</span>
						<span class="zp-15">March 8, 18:43</span>
						<span class="zp-15">Tim Benniks</span>
						<span class="zp-10">
							{% button text="delete" %}
							{% button text="edit &raquo;" action={redirect location="/admin/edit/2"} %}
						</span>
					</a>
				</li>
				<li>
					<a href="#" class="clearfix">
						<span class="zp-30">Home</span>
						<span class="zp-15">Struijk</span>
						<span class="zp-15">March 10, 19:33</span>
						<span class="zp-15">March 8, 18:43</span>
						<span class="zp-15">Tim Benniks</span>
						<span class="zp-10">
							{% button text="delete" %}
							{% button text="edit &raquo;" action={redirect location="/admin/edit/2"} %}
						</span>	
					</a>
				</li>
			</ul>
		</div>
		<div class="push"></div>
	</div>
{% endblock %}