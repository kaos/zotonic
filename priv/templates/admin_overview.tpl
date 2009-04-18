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
			
			<form method="get" autocomplete="off">
				<input type="text" value="" name="q" id="q" class="do_listfilter {list: '#posts'}" />
			</form>

			<ul id="posts">
				<li><span class="title">The Well-Designed Web</span></li>
				<li><span class="title">Welcome John Nunemaker</span></li> 
				<li><span class="title">Sidebar Creative: The Next Steps</span></li> 
				<li><span class="title">The Web/Desktop Divide</span></li> 
				<li><span class="title">2007 in Review</span></li> 
				<li><span class="title">Don't Complicate the Solution</span></li> 
				<li><span class="title">Blog to Business</span></li> 
				<li><span class="title">Single Line CSS</span></li> 
				<li><span class="title">Comments Work Again</span></li> 
				<li><span class="title">The iPhone Effect</span></li> 
				<li><span class="title">Greek Blogger Camp</span></li> 
				<li><span class="title">FeedBurner FeedSmith</span></li> 
				<li><span class="title">Download Counter Update 1.3</span></li> 
				<li><span class="title">Branding Reworked</span></li>
			</ul>
		</div>
		<div class="push"></div>
	</div>
{% endblock %}