{% extends "admin_base.tpl" %}

{% block title %} users overview {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

		<h2>Zophrenic User Overview</h2>
		
		<p>Every page/person can be made into an user on the edit page.  The difference between an user and a normal page is only
			that the former has logon credentials attached to its page record.</p>
	
		<div class="clearfix">
			{% button class="" text="Make a new page" action={dialog_new_rsc title=""} %}

			<div class="quick-search-wrapper right">
				<form method="get" action="{% url admin_user %}">
					<input type="text" name="qs" value="{{ q.qs|escape }}" class="left" />
					<button type="submit">Search user</button>
				</form>
			</div>
		</div>

		{% with m.search.paged[{users text=q.qs page=q.page}] as result %}

			{% pager result=result dispatch="admin_overview_rsc" qargs %}
			
			<h3 class="above-list ">
				Users overview{% if q.qs %}, 
					matching “{{ q.qs|escape }}”
					{% button text="show all" action={redirect dispatch="admin_user"} %}
				{% endif %}
			</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					<span class="zp-20">Name</span>
					<span class="zp-15">Username</span>
					<span class="zp-10">Modified on</span>
					<span class="zp-10">Created on</span>
					<span class="zp-30">Options</span>
				</li>
			{% for id, rank in result %}
				<li id="{{ #li.id }}">
					<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
						<span class="zp-20">{{ m.rsc[id].title|striptags }}</span>
						<span class="zp-15">{{ m.identity[id].username }}</span>
						<span class="zp-10">{{ m.rsc[id].modified|date:"F d, H:i" }}</span>
						<span class="zp-10">{{ m.rsc[id].created|date:"F d, H:i" }}</span>
						<span class="zp-30">
							{% button action={dialog_set_username_password id=id} text="set username/ password" on_delete={slide_fade_out target=#li.id} %}
							{% if id|ne:1 %}
								{% button text="delete username" action={dialog_delete_username id=id on_success={slide_fade_out target=#li.id}} %}
							{% endif %}
							{% button text="edit &raquo;" action={redirect dispatch="admin_edit_rsc" id=id} %}
						</span>
					</a>
				</li>
			{% empty %}
				<li>
					No users found.
				</li>
			{% endfor %}
			</ul>

			{% pager result=result dispatch="admin_overview_rsc" qargs %}

		{% endwith %}

			{#<form method="get" autocomplete="off">
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
			</ul>#}
		</div>
		<div class="push"></div>
	</div>
{% endblock %}