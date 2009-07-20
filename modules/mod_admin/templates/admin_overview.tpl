{% extends "admin_base.tpl" %}

{% block title %} admin overview {% endblock %}

{% block content %}

{% with q.qcat|eq:"event" as is_event %}

	<div id="content" class="zp-100">
		<div class="block clearfix">

		<h2>Zophrenic Page Overview</h2>
		<div class="clearfix">
			{% button class="" text="Make a new page" action={dialog_new_rsc title=""} %}
		</div>

		{% with m.search.paged[{fulltext cat=q.qcat text=q.qs page=q.page}] as result %}

			{% pager result=result dispatch="admin_overview_rsc" qargs %}
			
			<h3 class="above-list ">
				Pages overview{% if q.qs %}, 
				matching “{{ q.qs|escape }}”
				{% button text="show all" action={redirect dispatch="admin_overview_rsc"} %}
			{% endif %}</h3>
			<ul class="short-list">
				<li class="headers clearfix">
					{% if is_event %}
						<span class="zp-20">Title</span>
						<span class="zp-15">Performer</span>
						<span class="zp-15">Start date</span>
						<span class="zp-10">Category</span>
						<span class="zp-15">Modified on</span>
						<span class="zp-15">Modified by</span>
					{% else %}
						<span class="zp-30">Title</span>
						<span class="zp-15">Category</span>
						<span class="zp-15">Modified on</span>
						<span class="zp-15">Modified by</span>
						<span class="zp-15">Created on</span>
					{% endif %}
					<span class="zp-10">Options</span>
				</li>
			{% for id, rank in result %}
				<li id="{{ #li.id }}">
					<a href="{% url admin_edit_rsc id=id %}" class="clearfix">
						{% if is_event %}
							<span class="zp-20">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
							<span class="zp-15">{{ m.rsc[id].o.performer.title|default:"-" }}</span>
							<span class="zp-15">{{ m.rsc[id].date_start|date:"d M Y, H:i"|default:"-" }}</span>
							<span class="zp-10">{{ m.rsc[id].category.name }}</span>
							<span class="zp-15">{{ m.rsc[id].modified|date:"d M Y, H:i" }}</span>
							<span class="zp-15">{{ m.rsc[m.rsc[id].modifier_id].title|default:"-" }}</span>
						{% else %}
							<span class="zp-30">{{ m.rsc[id].title|striptags|default:"<em>untitled</em>" }}</span>
							<span class="zp-15">{{ m.rsc[id].category.name }}</span>
							<span class="zp-15">{{ m.rsc[id].modified|date:"d M Y, H:i" }}</span>
							<span class="zp-15">{{ m.rsc[id].created|date:"d M Y, H:i" }}</span>
							<span class="zp-15">{{ m.rsc[m.rsc[id].modifier_id].title|default:"-" }}</span>
						{% endif %}
						<span class="zp-10">
							{% button text="delete" disabled=m.rsc[id].is_protected action={dialog_delete_rsc id=id on_success={slide_fade_out target=#li.id}} %}

							{% button text="edit &raquo;" action={redirect dispatch="admin_edit_rsc" id=id} %}
						</span>
					</a>
				</li>
			{% empty %}
				<li>
					No pages found.
				</li>
			{% endfor %}
			</ul>

			{% pager result=result dispatch="admin_overview_rsc" qargs %}

		{% endwith %}
		
		</div>
	</div>

{% endwith %}

{% endblock %}