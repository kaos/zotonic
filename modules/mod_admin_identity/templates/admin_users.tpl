{% extends "admin_base.tpl" %}

{% block title %} users overview {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
		<div class="block clearfix">

		<h2>Zophrenic User Overview</h2>
		
		<p>Every page/person can be made into an user on the edit page.  The difference between an user and a normal page is only
			that the former has logon credentials attached to its page record.</p>
	
		<div class="clearfix">
			{% button text="New User" action={dialog_user_add on_success={reload}} %}

			<div class="quick-search-wrapper right">
				<form method="get" action="{% url admin_user %}">
					<input type="text" name="qs" value="{{ q.qs|escape }}" class="left" />
					<button type="submit">Search user</button>
				</form>
			</div>
		</div>
	
	{% with m.acl.user as me %}

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
						<span class="zp-15">{{ m.identity[id].username|escape }}{% if id|eq:me %}  <strong>(that's you)</strong>{% endif %}</span>
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

	{% endwith %}

		</div>
		<div class="push"></div>
	</div>
{% endblock %}